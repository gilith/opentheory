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

///////////////////////////////////////////////////////////////////////////////
// A class to store package upload information.
///////////////////////////////////////////////////////////////////////////////

class Upload {
  var $_id;
  var $_initiated;
  var $_status;
  var $_author;

  function id() { return $this->_id; }

  function initiated() { return $this->_initiated; }

  function status() { return $this->_status; }

  function author() { return $this->_author; }

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

  function to_string() { return $this->id(); }

  function Upload($id,$initiated,$status,$author) {
    is_string($id) or trigger_error('bad id');
    isset($initiated) or trigger_error('bad initiated');
    is_string($status) or trigger_error('bad status');

    $this->_id = $id;
    $this->_initiated = $initiated;
    $this->_status = $status;
    $this->_author = $author;
  }
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

    $upload = $row['id'];
    $initiated_datetime = $row['initiated'];
    $status = $row['status'];
    $author_name = $row['author_name'];
    $author_email = $row['author_email'];

    $initiated = new TimePoint();
    $initiated->from_database_datetime($initiated_datetime);

    if (isset($author_name) && isset($author_email)) {
      $author = new PackageAuthor($author_name,$author_email);
    }
    else {
      $author = null;
    }

    return new Upload($upload,$initiated,$status,$author);
  }

  function find_upload($upload) {
    is_string($upload) or trigger_error('bad upload');
    return $this->find_upload_where('id = ' . database_value($upload));
  }

  function insert_upload($upload) {
    $id = $upload->id();
    $initiated = $upload->initiated();
    $initiated_datetime = $initiated->to_database_datetime();
    $status = $upload->status();
    $author_name = $upload->author_name();
    $author_email = $upload->author_email();

    database_query('
      INSERT INTO ' . $this->table() . '
      SET id = ' . database_value($id) . ',
          initiated = ' . database_value($initiated_datetime) . ',
          status = ' . database_value($status) . ',
          author_name = ' . database_value($author_name) . ',
          author_email = ' . database_value($author_email) . ';');
  }

  function update_upload($upload) {
    isset($upload) or trigger_error('bad upload');

    $id = $upload->id();
    $initiated = $upload->initiated();
    $initiated_datetime = $initiated->to_database_datetime();
    $status = $upload->status();
    $author_name = $upload->author_name();
    $author_email = $upload->author_email();

    database_query('
      UPDATE ' . $this->table() . '
      SET initiated = ' . database_value($initiated_datetime) . ',
          status = ' . database_value($status) . ',
          author_name = ' . database_value($author_name) . ',
          author_email = ' . database_value($author_email) . '
      WHERE id = ' . database_value($id) . ';');
  }

  function delete_upload($upload) {
    $id = $upload->id();

    database_query('
      DELETE FROM ' . $this->table() . '
      WHERE id = ' . database_value($id) . ';');
  }

  function UploadTable($table) {
    global $all_upload_status;

    $fields =
      array('id' => 'char(' . UPLOAD_ID_CHARS . ') NOT NULL',
            'initiated' => 'datetime NOT NULL',
            'status' =>
              array_to_database_enum($all_upload_status) . ' NOT NULL',
            'author_name' => 'varchar(' . PACKAGE_AUTHOR_NAME_CHARS . ')',
            'author_email' => 'varchar(' . PACKAGE_AUTHOR_EMAIL_CHARS . ')');

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

  $upload = new Upload($id,$initiated,$status,$author);

  $upload_table->insert_upload($upload);

  return $upload;
}

?>
