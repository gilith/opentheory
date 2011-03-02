<?php

///////////////////////////////////////////////////////////////////////////////
//
// PACKAGE UPLOAD TABLE
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

///////////////////////////////////////////////////////////////////////////////
// Package upload status.
///////////////////////////////////////////////////////////////////////////////

define('INITIAL_UPLOAD_STATUS','initial');
define('ADD_PACKAGE_UPLOAD_STATUS','package');
define('CONFIRM_AUTHOR_UPLOAD_STATUS','author');
define('CONFIRM_OBSOLETE_UPLOAD_STATUS','obsolete');
define('TIMED_OUT_UPLOAD_STATUS','timeout');
define('WITHDRAWN_UPLOAD_STATUS','withdrawn');
define('REJECTED_UPLOAD_STATUS','rejected');
define('ACCEPTED_UPLOAD_STATUS','accepted');

$all_upload_status =
  array(INITIAL_UPLOAD_STATUS,
        ADD_PACKAGE_UPLOAD_STATUS,
        CONFIRM_AUTHOR_UPLOAD_STATUS,
        CONFIRM_OBSOLETE_UPLOAD_STATUS,
        TIMED_OUT_UPLOAD_STATUS,
        WITHDRAWN_UPLOAD_STATUS,
        REJECTED_UPLOAD_STATUS,
        ACCEPTED_UPLOAD_STATUS);

function is_upload_status($status) {
  global $all_upload_status;

  return is_string($status) && in_array($status,$all_upload_status);
}

function equal_upload_status($status1,$status2) {
  is_upload_status($status1) or trigger_error('bad status1');
  is_upload_status($status2) or trigger_error('bad status2');

  return (strcmp($status1,$status2) == 0);
}

function add_packagable_upload_status($status) {
  is_upload_status($status) or trigger_error('bad status');

  return (equal_upload_status($status,INITIAL_UPLOAD_STATUS) ||
          equal_upload_status($status,ADD_PACKAGE_UPLOAD_STATUS));
}

///////////////////////////////////////////////////////////////////////////////
// A class to store package upload information.
///////////////////////////////////////////////////////////////////////////////

class Upload {
  var $_id;
  var $_initiated;
  var $_status;
  var $_author;
  var $_obsolete;

  function id() { return $this->_id; }

  function initiated() { return $this->_initiated; }

  function status() { return $this->_status; }

  function author() { return $this->_author; }

  function obsolete() { return $this->_obsolete; }

  function since_initiated() {
    $now = server_datetime();
    $initiated = $this->initiated();

    return $now->subtract($initiated);
  }

  function add_packagable() {
    $status = $this->status();

    return add_packagable_upload_status($status);
  }

  function author_id() {
    $author = $this->author();

    if (isset($author)) {
      return $author->id();
    }
    else {
      return null;
    }
  }

  function author_name() {
    $author = $this->author();

    if (isset($author)) {
      return $author->name();
    }
    else {
      return null;
    }
  }

  function author_email() {
    $author = $this->author();

    if (isset($author)) {
      return $author->email();
    }
    else {
      return null;
    }
  }

  function obsolete_id() {
    $obsolete = $this->obsolete();

    if (isset($obsolete)) {
      return $obsolete->id();
    }
    else {
      return null;
    }
  }

  function obsolete_name() {
    $obsolete = $this->obsolete();

    if (isset($obsolete)) {
      return $obsolete->name();
    }
    else {
      return null;
    }
  }

  function obsolete_email() {
    $obsolete = $this->obsolete();

    if (isset($obsolete)) {
      return $obsolete->email();
    }
    else {
      return null;
    }
  }

  function to_string() { return $this->id(); }

  function link($text) {
    is_string($text) or trigger_error('bad text');

    $path = array();

    $args = array('upload' => $this->to_string());

    $atts = array('class' => 'upload');

    return site_link($path,$text,$args,$atts);
  }

  function jump() {
    $path = array();

    $args = array('upload' => $this->to_string());

    jump_path($path,$args);

    trigger_error('post-jump');
  }

  function set_status($status) {
    is_upload_status($status) or trigger_error('bad status');

    $this->_status = $status;
  }

  function Upload($id,$initiated,$status,$author,$obsolete) {
    is_string($id) or trigger_error('bad id');
    isset($initiated) or trigger_error('bad initiated');
    is_string($status) or trigger_error('bad status');

    $this->_id = $id;
    $this->_initiated = $initiated;
    $this->_status = $status;
    $this->_author = $author;
    $this->_obsolete = $obsolete;
  }
}

function from_row_upload($row) {
  is_array($row) or trigger_error('bad row');

  $id = $row['id'];
  $initiated_datetime = $row['initiated'];
  $status = $row['status'];
  $author = $row['author'];
  $obsolete = $row['obsolete'];

  $initiated = new TimePoint();
  $initiated->from_database_datetime($initiated_datetime);

  if (isset($author)) {
    $author = (integer)$author;
    $author = get_package_author($author);
  }

  if (isset($obsolete)) {
    $obsolete = (integer)$obsolete;
    $obsolete = get_package_author($obsolete);
  }

  return new Upload($id,$initiated,$status,$author,$obsolete);
}

///////////////////////////////////////////////////////////////////////////////
// The upload database table.
///////////////////////////////////////////////////////////////////////////////

define('UPLOAD_ID_CHARS',HASH_CHARS);

class UploadTable extends DatabaseTable {
  function find_upload_where($where_condition) {
    is_string($where_condition) or trigger_error('bad where_condition');

    $row = $this->find_row($where_condition);

    if (!isset($row)) { return null; }

    return from_row_upload($row);
  }

  function find_upload($upload) {
    is_string($upload) or trigger_error('bad upload');
    return $this->find_upload_where('id = ' . database_value($upload));
  }

  function list_uploads($where,$order_by,$limit) {
    !isset($where) or is_string($where) or trigger_error('bad where');
    is_string($order_by) or trigger_error('bad order_by');
    !isset($limit) or is_int($limit) or is_string($limit) or
      trigger_error('bad limit');

    $result = database_query('
      SELECT *
      FROM ' . $this->table() . (isset($where) ? ('
      WHERE ' . $where) : '') . '
      ORDER BY ' . $order_by . (isset($limit) ? ('
      LIMIT ' . $limit) : '') . ';');

    $upls = array();

    while ($row = mysql_fetch_array($result)) {
      $upls[] = from_row_upload($row);
    }

    return $upls;
  }

  function list_recent_uploads($limit) {
    is_int($limit) or trigger_error('bad limit');

    $where = null;

    $order_by = 'initiated DESC';

    return $this->list_uploads($where,$order_by,$limit);
  }

  function insert_upload($upload) {
    $id = $upload->id();
    $initiated = $upload->initiated();
    $initiated_datetime = $initiated->to_database_datetime();
    $status = $upload->status();
    $author_id = $upload->author_id();
    $obsolete_id = $upload->obsolete_id();

    database_query('
      INSERT INTO ' . $this->table() . '
      SET id = ' . database_value($id) . ',
          initiated = ' . database_value($initiated_datetime) . ',
          status = ' . database_value($status) . ',
          author = ' . database_value($author_id) . ',
          obsolete = ' . database_value($obsolete_id) . ';');
  }

  function update_upload($upload) {
    isset($upload) or trigger_error('bad upload');

    $id = $upload->id();
    $initiated = $upload->initiated();
    $initiated_datetime = $initiated->to_database_datetime();
    $status = $upload->status();
    $author_id = $upload->author_id();
    $obsolete_id = $upload->obsolete_id();

    database_query('
      UPDATE ' . $this->table() . '
      SET initiated = ' . database_value($initiated_datetime) . ',
          status = ' . database_value($status) . ',
          author = ' . database_value($author_id) . ',
          obsolete = ' . database_value($obsolete_id) . '
      WHERE id = ' . database_value($id) . ';');
  }

  function delete_upload($upload) {
    $id = $upload->id();

    database_query('
      DELETE FROM ' . $this->table() . '
      WHERE id = ' . database_value($id) . ';');
  }

  function set_status($upload,$status) {
    isset($upload) or trigger_error('bad upload');
    is_upload_status($status) or trigger_error('bad status');

    if (!equal_upload_status($upload->status(),$status)) {
      $upload->set_status($status);

      $id = $upload->id();

      database_query('
        UPDATE ' . $this->table() . '
        SET status = ' . database_value($status) . '
        WHERE id = ' . database_value($id) . ';');
    }
  }

  function UploadTable($table) {
    global $all_upload_status;

    $fields =
      array('id' => 'char(' . UPLOAD_ID_CHARS . ') NOT NULL',
            'initiated' => 'datetime NOT NULL',
            'status' =>
              array_to_database_enum($all_upload_status) . ' NOT NULL',
            'author' => 'int(' . PACKAGE_AUTHOR_ID_DIGITS . ')',
            'obsolete' => 'int(' . PACKAGE_AUTHOR_ID_DIGITS . ')');

    $indexes =
      array('PRIMARY KEY (id)',
            'INDEX (initiated)');

    parent::DatabaseTable($table,$fields,$indexes);
  }
}

$global_upload_table = null;

function upload_table() {
  global $global_upload_table;

  if (!isset($global_upload_table)) {
    $global_upload_table = new UploadTable('upload');
  }

  return $global_upload_table;
}

///////////////////////////////////////////////////////////////////////////////
// Look up an upload.
///////////////////////////////////////////////////////////////////////////////

function find_upload($upload_id) {
  is_string($upload_id) or trigger_error('bad upload_id');

  $upload_table = upload_table();

  return $upload_table->find_upload($upload_id);
}

///////////////////////////////////////////////////////////////////////////////
// Creating new upload tokens.
///////////////////////////////////////////////////////////////////////////////

define('NEW_UPLOAD_SALT',SITE_NAME);

function create_new_upload() {
  $upload_table = upload_table();

  $initiated = server_datetime();

  $id = md5(NEW_UPLOAD_SALT . ' ' .
            $initiated->to_database_datetime() . ' ' .
            rand());

  $status = INITIAL_UPLOAD_STATUS;

  $author = null;

  $obsolete = null;

  $upload = new Upload($id,$initiated,$status,$author,$obsolete);

  $upload_table->insert_upload($upload);

  return $upload;
}

///////////////////////////////////////////////////////////////////////////////
// Set an upload status.
///////////////////////////////////////////////////////////////////////////////

function set_upload_status($upload,$status) {
  isset($upload) or trigger_error('bad upload');
  is_upload_status($status) or trigger_error('bad status');

  $upload_table = upload_table();

  $upload_table->set_status($upload,$status);
}

?>
