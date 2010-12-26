<?php

///////////////////////////////////////////////////////////////////////////////
//
// PACKAGE TABLE
//
// Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2
//
///////////////////////////////////////////////////////////////////////////////

require_once 'global.php';
require_once 'error.php';
require_once 'date.php';
require_once 'database.php';

///////////////////////////////////////////////////////////////////////////////
// A class to store package information.
///////////////////////////////////////////////////////////////////////////////

class Package {
  var $_id;
  var $_name;
  var $_description;
  var $_uploaded;

  function id() { return $this->_id; }

  function name() { return $this->_name; }

  function description() { return $this->_description; }

  function uploaded() { return $this->_uploaded; }

  function Package($id,$name,$description,$uploaded) {
    is_int($id) or trigger_error('bad id');
    is_string($name) or trigger_error('bad name');
    !isset($description) or is_string($description) or
      trigger_error('bad description');
    isset($uploaded) or trigger_error('bad uploaded');

    $this->_id = $id;
    $this->_name = $name;
    $this->_description = $description;
    $this->_uploaded = $uploaded;
  }
}

///////////////////////////////////////////////////////////////////////////////
// The package database table.
///////////////////////////////////////////////////////////////////////////////

define('PACKAGE_ID_DIGITS',7);
define('PACKAGE_NAME_CHARS',50);
define('PACKAGE_DESCRIPTION_CHARS',200);

class PackageTable extends DatabaseTable {
  function find_package_where($where_condition) {
    is_string($where_condition) or trigger_error('bad where_condition');

    $row = $this->find_row($where_condition);

    if (!isset($row)) { return null; }

    $id = (integer)$row['id'];
    $name = $row['name'];
    $description = $row['description'];
    $uploaded_datetime = $row['uploaded'];

    $uploaded = new TimePoint();
    $uploaded->from_database_datetime($uploaded_datetime);

    return new Package($id,$name,$description,$uploaded);
  }

  function find_package($id) {
    is_int($id) or trigger_error('bad package');
    return $this->find_package_where('id = ' . database_value($id));
  }

  function find_package_by_name($name) {
    is_string($name) or trigger_error('bad name');
    return $this->find_package_where('name = ' . database_value($name));
  }

  function insert_package($package) {
    $id = $package->id();

    $name = $package->name();

    $description = $package->description();

    $uploaded = $package->uploaded();
    $uploaded_datetime = $uploaded->to_database_datetime();

    database_query('
      INSERT INTO ' . $this->table() . '
      SET id = ' . database_value($id) . ',
          name = ' . database_value($name) . ',
          description = ' . database_value($description) . ',
          uploaded = ' . database_value($uploaded_datetime) . ';');
  }

  function create_package($name,$description) {
    is_string($name) or trigger_error('bad name');
    !isset($description) or is_string($description) or
      trigger_error('bad description');

    $id = $this->max_rows('id') + 1;

    $uploaded = server_datetime();

    $package = new Package($id,$name,$description,$uploaded);

    $this->insert_package($package);

    return $package;
  }

  function PackageTable($table) {
    $fields =
      array('id' => 'int(' . PACKAGE_ID_DIGITS . ') NOT NULL',
            'name' => 'varchar(' . PACKAGE_NAME_CHARS . ') NOT NULL',
            'description' => 'char(' . PACKAGE_DESCRIPTION_CHARS . ')',
            'uploaded' => 'datetime NOT NULL');

    $indexes =
      array('PRIMARY KEY (id)',
            'INDEX (name)',
            'INDEX (uploaded)');

    parent::DatabaseTable($table,$fields,$indexes);
  }
}

$global_package_table = null;

function package_table() {
  global $global_package_table;

  if (!isset($global_package_table)) {
    $global_package_table = new PackageTable('package');
  }

  return $global_package_table;
}

?>
