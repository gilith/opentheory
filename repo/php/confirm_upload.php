<?php

///////////////////////////////////////////////////////////////////////////////
//
// UPLOAD CONFIRMATION TABLE
//
// Copyright (c) 2011 Joe Hurd, distributed under the GNU GPL version 2
//
///////////////////////////////////////////////////////////////////////////////

require_once 'global.php';
require_once 'error.php';
require_once 'date.php';
require_once 'input.php';
require_once 'database.php';
require_once 'author.php';
require_once 'package.php';
require_once 'upload.php';

///////////////////////////////////////////////////////////////////////////////
// Confirm upload type.
///////////////////////////////////////////////////////////////////////////////

define('AUTHOR_CONFIRM_UPLOAD_TYPE','author');
define('OBSOLETE_CONFIRM_UPLOAD_TYPE','obsolete');

$all_confirm_upload_type =
  array(AUTHOR_CONFIRM_UPLOAD_TYPE,
        OBSOLETE_CONFIRM_UPLOAD_TYPE);

function is_confirm_upload_type($type) {
  global $all_confirm_upload_type;

  return is_string($type) && in_array($type,$all_confirm_upload_type);
}

function equal_confirm_upload_type($type1,$type2) {
  is_confirm_upload_type($type1) or trigger_error('bad type1');
  is_confirm_upload_type($type2) or trigger_error('bad type2');

  return (strcmp($type1,$type2) == 0);
}

function is_author_confirm_upload_type($type) {
  is_confirm_upload_type($type) or trigger_error('bad type');

  return equal_confirm_upload_type($type,AUTHOR_CONFIRM_UPLOAD_TYPE);
}

function is_obsolete_confirm_upload_type($type) {
  is_confirm_upload_type($type) or trigger_error('bad type');

  return equal_confirm_upload_type($type,OBSOLETE_CONFIRM_UPLOAD_TYPE);
}

///////////////////////////////////////////////////////////////////////////////
// A class to store upload confirmation information.
///////////////////////////////////////////////////////////////////////////////

class ConfirmUpload {
  var $_id;
  var $_type;
  var $_sent;
  var $_upload;

  function id() { return $this->_id; }

  function type() { return $this->_type; }

  function sent() { return $this->_sent; }

  function upload() { return $this->_upload; }

  function is_author() {
    $type = $this->type();

    return is_author_confirm_upload_type($type);
  }

  function is_obsolete() {
    $type = $this->type();

    return is_obsolete_confirm_upload_type($type);
  }

  function since_sent() {
    $now = server_datetime();
    $sent = $this->sent();

    return $now->subtract($sent);
  }

  function upload_id() {
    $upload = $this->upload();

    if (isset($upload)) {
      return $upload->id();
    }
    else {
      return null;
    }
  }

  function to_string() { return $this->id(); }

  function url() {
    $path = array();

    $args = array('confirm' => $this->to_string());

    return external_site_url($path,$args);
  }

  function ConfirmUpload($id,$type,$sent,$upload) {
    is_string($id) or trigger_error('bad id');
    is_confirm_upload_type($type) or trigger_error('bad type');
    isset($sent) or trigger_error('bad sent');

    $this->_id = $id;
    $this->_type = $type;
    $this->_sent = $sent;
    $this->_upload = $upload;
  }
}

function from_row_confirm_upload($row) {
  is_array($row) or trigger_error('bad row');

  $id = $row['id'];
  $type = $row['type'];
  $sent_datetime = $row['sent'];
  $upload_id = $row['upload'];

  $sent = new TimePoint();
  $sent->from_database_datetime($sent_datetime);

  $upload = find_upload($upload_id);

  return new ConfirmUpload($id,$type,$sent,$upload);
}

///////////////////////////////////////////////////////////////////////////////
// The upload confirmation database table.
///////////////////////////////////////////////////////////////////////////////

define('CONFIRM_UPLOAD_ID_CHARS',HASH_CHARS);

class ConfirmUploadTable extends DatabaseTable {
  function find_confirm_upload_where($where_condition) {
    is_string($where_condition) or trigger_error('bad where_condition');

    $row = $this->find_row($where_condition);

    if (!isset($row)) { return null; }

    return from_row_confirm_upload($row);
  }

  function find_confirm_upload($id) {
    is_string($id) or trigger_error('bad id');

    return $this->find_confirm_upload_where('id = ' . database_value($id));
  }

  function insert_confirm_upload($confirm) {
    isset($confirm) or trigger_error('bad confirm');

    $id = $confirm->id();

    $type = $confirm->type();

    $sent = $confirm->sent();
    $sent_datetime = $sent->to_database_datetime();

    $upload = $confirm->upload_id();

    database_query('
      INSERT INTO ' . $this->table() . '
      SET id = ' . database_value($id) . ',
          type = ' . database_value($type) . ',
          sent = ' . database_value($sent_datetime) . ',
          upload = ' . database_value($upload) . ';');
  }

  function delete_confirm_upload($confirm) {
    isset($confirm) or trigger_error('bad confirm');

    $id = $confirm->id();

    database_query('
      DELETE FROM ' . $this->table() . '
      WHERE id = ' . database_value($id) . ';');
  }

  function ConfirmUploadTable($table) {
    global $all_confirm_upload_type;

    $fields =
      array('id' => 'char(' . CONFIRM_UPLOAD_ID_CHARS . ') NOT NULL',
            'type' =>
              array_to_database_enum($all_confirm_upload_type) . ' NOT NULL',
            'sent' => 'datetime NOT NULL',
            'upload' => 'char(' . UPLOAD_ID_CHARS . ') NOT NULL');

    $indexes =
      array('PRIMARY KEY (id)');

    parent::DatabaseTable($table,$fields,$indexes);
  }
}

$global_confirm_upload_table = null;

function confirm_upload_table() {
  global $global_confirm_upload_table;

  if (!isset($global_confirm_upload_table)) {
    $global_confirm_upload_table = new ConfirmUploadTable('confirm_upload');
  }

  return $global_confirm_upload_table;
}

///////////////////////////////////////////////////////////////////////////////
// Look up an upload confirmation.
///////////////////////////////////////////////////////////////////////////////

function find_confirm_upload($confirm_id) {
  is_string($confirm_id) or trigger_error('bad confirm_id');

  $confirm_upload_table = confirm_upload_table();

  return $confirm_upload_table->find_confirm_upload($confirm_id);
}

///////////////////////////////////////////////////////////////////////////////
// Creating new upload tokens.
///////////////////////////////////////////////////////////////////////////////

define('NEW_CONFIRM_UPLOAD_SALT',SITE_NAME);

function create_new_confirm_upload($type,$upload) {
  is_confirm_upload_type($type) or trigger_error('bad type');
  isset($upload) or trigger_error('bad upload');

  $confirm_upload_table = confirm_upload_table();

  $sent = server_datetime();

  $id = md5(NEW_CONFIRM_UPLOAD_SALT . ' ' .
            $type . ' ' .
            $sent->to_database_datetime() . ' ' .
            $upload->id() . ' ' .
            rand());

  $confirm = new ConfirmUpload($id,$type,$sent,$upload);

  $confirm_upload_table->insert_confirm_upload($confirm);

  return $confirm;
}

///////////////////////////////////////////////////////////////////////////////
// Delete a package upload.
///////////////////////////////////////////////////////////////////////////////

function delete_confirm_upload($confirm) {
  isset($confirm) or trigger_error('bad confirm');

  $confirm_upload_table = confirm_upload_table();

  $confirm_upload_table->delete_confirm_upload($confirm);
}

?>
