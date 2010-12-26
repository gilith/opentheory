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
// A class to store package names.
///////////////////////////////////////////////////////////////////////////////

class PackageName {
  var $_base;
  var $_version;

  function base() { return $this->_base; }

  function version() { return $this->_version; }

  function name() { return ($this->base() . '-' . $this->version()); }

  function tarball() { return ($this->name() . '.tgz'); }

  function document() { return ($this->name() . '.html'); }

  function PackageName($base,$version) {
    is_string($base) or trigger_error('bad base');
    is_string($version) or trigger_error('bad version');

    $this->_base = $base;
    $this->_version = $version;
  }
}

///////////////////////////////////////////////////////////////////////////////
// Package name functions.
///////////////////////////////////////////////////////////////////////////////

define('PACKAGE_BASE_REGEXP',
       '[a-z][a-z0-9]*(-[a-z][a-z0-9]*)*');

define('PACKAGE_VERSION_REGEXP',
       '[0-9]+([.][0-9]+)*');

define('PACKAGE_NAME_REGEXP',
       '(' . PACKAGE_BASE_REGEXP . ')-(' . PACKAGE_VERSION_REGEXP . ')');

define('PACKAGE_TARBALL_REGEXP',
       '(' . PACKAGE_NAME_REGEXP . ')[.]tgz');

function from_name_package_name($name) {
  is_string($name) or trigger_error('bad name');

  $re = '^' . PACKAGE_NAME_REGEXP . '$';

  if (ereg($re,$name,$arr)) {
    $base = $arr[1];
    $version = $arr[2];

    return new PackageName($base,$version);
  }
  else {
    return null;
  }
}

function from_tarball_package_name($tarball) {
  is_string($tarball) or trigger_error('bad tarball');

  $re = '^' . PACKAGE_TARBALL_REGEXP . '$';

  if (ereg($re,$tarball,$arr)) {
    $name = $arr[1];

    return from_name_package_name($name);
  }
  else {
    return null;
  }
}

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
    isset($name) or trigger_error('bad name');
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
define('PACKAGE_BASE_CHARS',50);
define('PACKAGE_VERSION_CHARS',50);
define('PACKAGE_DESCRIPTION_CHARS',200);

class PackageTable extends DatabaseTable {
  function find_package_where($where_condition) {
    is_string($where_condition) or trigger_error('bad where_condition');

    $row = $this->find_row($where_condition);

    if (!isset($row)) { return null; }

    $id = (integer)$row['id'];
    $base = $row['base'];
    $version = $row['version'];
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
    isset($name) or trigger_error('bad name');

    $where =
      'base = ' . database_value($name->base()) . ' AND ' .
      'version = ' . database_value($name->version());

    return $this->find_package_where($where);
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
          base = ' . database_value($name->base()) . ',
          version = ' . database_value($name->version()) . ',
          description = ' . database_value($description) . ',
          uploaded = ' . database_value($uploaded_datetime) . ';');
  }

  function create_package($name,$description) {
    isset($name) or trigger_error('bad name');
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
            'base' => 'varchar(' . PACKAGE_BASE_CHARS . ') NOT NULL',
            'version' => 'varchar(' . PACKAGE_VERSION_CHARS . ') NOT NULL',
            'description' => 'varchar(' . PACKAGE_DESCRIPTION_CHARS . ')',
            'uploaded' => 'datetime NOT NULL');

    $indexes =
      array('PRIMARY KEY (id)',
            'INDEX (base,version)',
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
