<?php

///////////////////////////////////////////////////////////////////////////////
//
// PACKAGE AUTHORS
//
// Copyright (c) 2010 Joe Leslie-Hurd, distributed under the MIT license
//
///////////////////////////////////////////////////////////////////////////////

require_once 'global.php';
require_once 'error.php';

///////////////////////////////////////////////////////////////////////////////
// Package author regular expressions.
///////////////////////////////////////////////////////////////////////////////

define('PACKAGE_AUTHOR_NAME_REGEXP',
       '[-a-zA-Z0-9\' ]+');

define('PACKAGE_AUTHOR_EMAIL_REGEXP',
       '[a-zA-Z0-9_.@-]+');

define('PACKAGE_AUTHOR_REGEXP',
       '(' . PACKAGE_AUTHOR_NAME_REGEXP . ') <' .
       '(' . PACKAGE_AUTHOR_EMAIL_REGEXP . ')>');

///////////////////////////////////////////////////////////////////////////////
// A class to store package authors.
///////////////////////////////////////////////////////////////////////////////

class PackageAuthor {
  var $_name;
  var $_email;

  function id() { return $this->_id; }

  function name() { return $this->_name; }

  function email() { return $this->_email; }

  function equal($author) {
    isset($author) or trigger_error('bad author');

    return $this->id() == $author->id();
  }

  function to_string() {
    return ($this->name() . ' <' . $this->email() . '>');
  }

  function PackageAuthor($id,$name,$email) {
    is_int($id) or trigger_error('bad id');
    is_string($name) or trigger_error('bad name');
    is_string($email) or trigger_error('bad email');

    $this->_id = $id;
    $this->_name = $name;
    $this->_email = $email;
  }
}

///////////////////////////////////////////////////////////////////////////////
// Package author functions.
///////////////////////////////////////////////////////////////////////////////

function from_row_package_author($row) {
  is_array($row) or trigger_error('bad row');

  $id = (integer)$row['id'];
  $name = $row['name'];
  $email = $row['email'];

  return new PackageAuthor($id,$name,$email);
}

///////////////////////////////////////////////////////////////////////////////
// The package author database table.
///////////////////////////////////////////////////////////////////////////////

define('PACKAGE_AUTHOR_ID_DIGITS',7);
define('PACKAGE_AUTHOR_NAME_CHARS',100);
define('PACKAGE_AUTHOR_EMAIL_CHARS',100);

class PackageAuthorTable extends DatabaseTable {
  function find_package_author_where($where_condition) {
    is_string($where_condition) or trigger_error('bad where_condition');

    $row = $this->find_row($where_condition);

    if (!isset($row)) { return null; }

    return from_row_package_author($row);
  }

  function find_package_author($id) {
    is_int($id) or trigger_error('bad id');

    return $this->find_package_author_where('id = ' . database_value($id));
  }

  function find_package_author_by_name_email($name,$email) {
    is_string($name) or trigger_error('bad name');
    is_string($email) or trigger_error('bad email');

    $where =
      'name = ' . database_value($name) . ' AND ' .
      'email = ' . database_value($email);

    return $this->find_package_author_where($where);
  }

  function count_package_authors() {
    $where = null;

    return $this->count_rows($where);
  }

  function insert_package_author($author) {
    $id = $author->id();

    $name = $author->name();

    $email = $author->email();

    database_query('
      INSERT INTO ' . $this->table() . '
      SET id = ' . database_value($id) . ',
          name = ' . database_value($name) . ',
          email = ' . database_value($email) . ';');
  }

  function create_package_author($name,$email) {
    is_string($name) or trigger_error('bad name');
    is_string($email) or trigger_error('bad email');

    $id = $this->max_rows('id') + 1;

    $author = new PackageAuthor($id,$name,$email);

    $this->insert_package_author($author);

    return $author;
  }

  function PackageAuthorTable($table) {
    $fields =
      array('id' => 'int(' . PACKAGE_AUTHOR_ID_DIGITS . ') NOT NULL',
            'name' => 'varchar(' . PACKAGE_AUTHOR_NAME_CHARS . ') NOT NULL',
            'email' => 'varchar(' . PACKAGE_AUTHOR_EMAIL_CHARS . ') NOT NULL');

    $indexes =
      array('PRIMARY KEY (id)',
            'INDEX (name,email)');

    parent::DatabaseTable($table,$fields,$indexes);
  }
}

$global_package_author_table = null;

function package_author_table() {
  global $global_package_author_table;

  if (!isset($global_package_author_table)) {
    $global_package_author_table = new PackageAuthorTable('package_author');
  }

  return $global_package_author_table;
}

///////////////////////////////////////////////////////////////////////////////
// Count packages.
///////////////////////////////////////////////////////////////////////////////

function count_package_authors() {
  $package_author_table = package_author_table();
  return $package_author_table->count_package_authors();
}

///////////////////////////////////////////////////////////////////////////////
// Look up a package.
///////////////////////////////////////////////////////////////////////////////

function find_package_author($author_id) {
  is_int($author_id) or trigger_error('bad author_id');

  $package_author_table = package_author_table();

  return $package_author_table->find_package_author($author_id);
}

function get_package_author($author_id) {
  is_int($author_id) or trigger_error('bad author_id');

  $author = find_package_author($author_id);

  if (!isset($author)) {
    trigger_error('unknown author');
  }

  return $author;
}

function find_or_create_package_author($name,$email) {
  is_string($name) or trigger_error('bad name');
  is_string($email) or trigger_error('bad email');

  $package_author_table = package_author_table();

  $author =
    $package_author_table->find_package_author_by_name_email($name,$email);

  if (!isset($author)) {
    $author = $package_author_table->create_package_author($name,$email);
  }

  return $author;
}

function from_string_package_author($name_email) {
  is_string($name_email) or trigger_error('bad name_email');

  $re = '/^' . PACKAGE_AUTHOR_REGEXP . '$/';

  if (preg_match($re,$name_email,$arr)) {
    $name = $arr[1];
    $email = $arr[2];

    return find_or_create_package_author($name,$email);
  }
  else {
    return null;
  }
}

?>
