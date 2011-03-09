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
require_once 'links.php';
require_once 'database.php';
require_once 'name_version.php';
require_once 'author.php';

///////////////////////////////////////////////////////////////////////////////
// Package status.
///////////////////////////////////////////////////////////////////////////////

define('INSTALLED_PACKAGE_STATUS','installed');
define('STAGED_PACKAGE_STATUS','staged');
define('UPLOADED_PACKAGE_STATUS','uploaded');

$all_package_status =
  array(INSTALLED_PACKAGE_STATUS,
        STAGED_PACKAGE_STATUS,
        UPLOADED_PACKAGE_STATUS);

function is_package_status($status) {
  global $all_package_status;

  return is_string($status) && in_array($status,$all_package_status);
}

function equal_package_status($status1,$status2) {
  is_package_status($status1) or trigger_error('bad status1');
  is_package_status($status2) or trigger_error('bad status2');

  return (strcmp($status1,$status2) == 0);
}

function staged_package_status($status) {
  is_package_status($status) or trigger_error('bad status');

  return equal_package_status($status,STAGED_PACKAGE_STATUS);
}

function pretty_package_status($status) {
  is_package_status($status) or trigger_error('bad status');

  if (equal_package_status($status,INSTALLED_PACKAGE_STATUS)) {
    return 'Waiting for a package to be added';
  }
  elseif (equal_package_status($status,ADD_PACKAGE_PACKAGE_STATUS)) {
    return 'Waiting for more packages to be added';
  }
  elseif (equal_package_status($status,CONFIRM_AUTHOR_PACKAGE_STATUS)) {
    return 'Waiting for the package author to confirm their email address';
  }
  elseif (equal_package_status($status,CONFIRM_OBSOLETE_PACKAGE_STATUS)) {
    return 'Waiting for the author of the obsoleted packages to sign off';
  }
  else {
    trigger_error('default case');
  }
}

///////////////////////////////////////////////////////////////////////////////
// A class to store *uploaded* package information.
///////////////////////////////////////////////////////////////////////////////

class Package {
  var $_id;
  var $_name_version;
  var $_description;
  var $_author;
  var $_license;
  var $_registered;
  var $_status;
  var $_auxiliary;
  var $_obsolete;

  function id() { return $this->_id; }

  function name_version() { return $this->_name_version; }

  function description() { return $this->_description; }

  function author() { return $this->_author; }

  function license() { return $this->_license; }

  function registered() { return $this->_registered; }

  function status() { return $this->_status; }

  function auxiliary() { return $this->_auxiliary; }

  function obsolete() { return $this->_obsolete; }

  function name() {
    $namever = $this->name_version();

    return $namever->name();
  }

  function version() {
    $namever = $this->name_version();

    return $namever->version();
  }

  function author_id() {
    $author = $this->author();

    return $author->id();
  }

  function author_name() {
    $author = $this->author();

    return $author->name();
  }

  function author_email() {
    $author = $this->author();

    return $author->email();
  }

  function since_registered() {
    $now = server_datetime();

    $registered = $this->registered();

    return $now->subtract($registered);
  }

  function staged_status() {
    $status = $this->status();

    return staged_package_status($status);
  }

  function is_auxiliary_parent($parent) {
    isset($parent) or trigger_error('bad parent');

    $name1 = $this->name();
    $name2 = $parent->name();

    return is_prefix_package_name($name1,$name2);
  }

  function to_string() {
    $namever = $this->name_version();

    return $namever->to_string();
  }

  function link($text) {
    is_string($text) or trigger_error('bad text');

    $namever = $this->name_version();

    return $namever->package_link($text);
  }

  function summary_file_name() {
    $namever = $this->name_version();

    return $namever->summary_file_name();
  }

  function summary_file_link($text) {
    is_string($text) or trigger_error('bad text');

    $namever = $this->name_version();

    if ($this->staged_status()) {
      return $namever->staged_summary_file_link($text);
    }
    else {
      return $namever->summary_file_link($text);
    }
  }

  function tarball_name() {
    $namever = $this->name_version();

    return $namever->tarball_name();
  }

  function tarball_link($text) {
    is_string($text) or trigger_error('bad text');

    $namever = $this->name_version();

    if ($this->staged_status()) {
      return $namever->staged_tarball_link($text);
    }
    else {
      return $namever->tarball_link($text);
    }
  }

  function theory_file_name() {
    $namever = $this->name_version();

    return $namever->theory_file_name();
  }

  function theory_file_link($text) {
    is_string($text) or trigger_error('bad text');

    $namever = $this->name_version();

    if ($this->staged_status()) {
      return $namever->staged_theory_file_link($text);
    }
    else {
      return $namever->theory_file_link($text);
    }
  }

  function mark_uploaded() { $this->_status = UPLOADED_PACKAGE_STATUS; }

  function mark_auxiliary() { $this->_auxiliary = true; }

  function mark_obsolete() { $this->_obsolete = true; }

  function Package($id,$name_version,$description,$author,$license,
                   $registered,$status,$auxiliary,$obsolete)
  {
    is_int($id) or trigger_error('bad id');
    isset($name_version) or trigger_error('bad name_version');
    is_string($description) or trigger_error('bad description');
    isset($author) or trigger_error('bad author');
    is_string($license) or trigger_error('bad license');
    isset($registered) or trigger_error('bad registered');
    is_package_status($status) or trigger_error('bad status');
    is_bool($auxiliary) or trigger_error('bad auxiliary');
    is_bool($obsolete) or trigger_error('bad obsolete');

    $this->_id = $id;
    $this->_name_version = $name_version;
    $this->_description = $description;
    $this->_author = $author;
    $this->_license = $license;
    $this->_registered = $registered;
    $this->_status = $status;
    $this->_auxiliary = $auxiliary;
    $this->_obsolete = $obsolete;
  }
}

function from_row_package($row) {
  is_array($row) or trigger_error('bad row');

  $id = (integer)$row['id'];
  $name = $row['name'];
  $version = $row['version'];
  $description = $row['description'];
  $author_id = (integer)$row['author'];
  $license = $row['license'];
  $registered_datetime = $row['registered'];
  $status = $row['staged'];
  $auxiliary_database = $row['auxiliary'];
  $obsolete_database = $row['obsolete'];

  $name_version = new PackageNameVersion($name,$version);

  $author = get_package_author($author_id);

  $registered = new TimePoint();
  $registered->from_database_datetime($registered_datetime);

  $auxiliary = bool_from_database_bool($auxiliary_database);

  $obsolete = bool_from_database_bool($obsolete_database);

  return new Package($id,$name_version,$description,$author,$license,
                     $registered,$status,$auxiliary,$obsolete);
}

///////////////////////////////////////////////////////////////////////////////
// The package database table.
///////////////////////////////////////////////////////////////////////////////

define('PACKAGE_ID_DIGITS',7);
define('PACKAGE_NAME_CHARS',100);
define('PACKAGE_VERSION_CHARS',50);
define('PACKAGE_DESCRIPTION_CHARS',200);
define('PACKAGE_LICENSE_CHARS',50);

class PackageTable extends DatabaseTable {
  function find_package_where($where_condition) {
    is_string($where_condition) or trigger_error('bad where_condition');

    $row = $this->find_row($where_condition);

    if (!isset($row)) { return null; }

    return from_row_package($row);
  }

  function find_package($id) {
    is_int($id) or trigger_error('bad id');

    return $this->find_package_where('id = ' . database_value($id));
  }

  function find_package_by_name_version($name_version) {
    isset($name_version) or trigger_error('bad name_version');

    $name = $name_version->name();

    $version = $name_version->version();

    $where =
      'name = ' . database_value($name) . ' AND ' .
      'version = ' . database_value($version);

    return $this->find_package_where($where);
  }

  function list_packages($where,$order_by,$limit) {
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

    $pkgs = array();

    while ($row = mysql_fetch_array($result)) {
      $pkgs[] = from_row_package($row);
    }

    return $pkgs;
  }

  function list_active_packages() {
    $where =
      'status <=> ' . database_value(UPLOADED_PACKAGE_STATUS) .
      ' AND auxiliary <=> ' . database_value(DATABASE_FALSE) .
      ' AND obsolete <=> ' . database_value(DATABASE_FALSE);

    $order_by = 'name';

    $limit = null;

    return $this->list_packages($where,$order_by,$limit);
  }

  function count_active_packages() {
    $where =
      'status <=> ' . database_value(UPLOADED_PACKAGE_STATUS) .
      ' AND auxiliary <=> ' . database_value(DATABASE_FALSE) .
      ' AND obsolete <=> ' . database_value(DATABASE_FALSE);

    return $this->count_rows($where);
  }

  function list_recent_packages($limit) {
    is_int($limit) or trigger_error('bad limit');

    $where =
      'status <=> ' . database_value(UPLOADED_PACKAGE_STATUS) .
      ' AND auxiliary <=> ' . database_value(DATABASE_FALSE);

    $order_by = 'registered DESC';

    return $this->list_packages($where,$order_by,$limit);
  }

  function insert_package($package) {
    $id = $package->id();

    $name = $package->name();

    $version = $package->version();

    $description = $package->description();

    $author_id = $package->author_id();

    $license = $package->license();

    $registered = $package->registered();
    $registered_datetime = $registered->to_database_datetime();

    $status = $package->status();

    $auxiliary = $package->auxiliary();
    $auxiliary_database = bool_to_database_bool($auxiliary);

    $obsolete = $package->obsolete();
    $obsolete_database = bool_to_database_bool($obsolete);

    database_query('
      INSERT INTO ' . $this->table() . '
      SET id = ' . database_value($id) . ',
          name = ' . database_value($name) . ',
          version = ' . database_value($version) . ',
          description = ' . database_value($description) . ',
          author = ' . database_value($author_id) . ',
          license = ' . database_value($license) . ',
          registered = ' . database_value($registered_datetime) . ',
          status = ' . database_value($status) . ',
          auxiliary = ' . database_value($auxiliary_database) . ',
          obsolete = ' . database_value($obsolete_database) . ';');
  }

  function mark_uploaded($pkg) {
    isset($pkg) or trigger_error('bad pkg');

    $pkg->mark_uploaded();

    $id = $pkg->id();

    database_query('
      UPDATE ' . $this->table() . '
      SET status = ' . database_value(UPLOADED_PACKAGE_STATUS) . '
      WHERE id = ' . database_value($id) . ';');
  }

  function mark_auxiliary($pkg) {
    isset($pkg) or trigger_error('bad pkg');

    $pkg->mark_auxiliary();

    $id = $pkg->id();

    database_query('
      UPDATE ' . $this->table() . '
      SET auxiliary = ' . database_value(DATABASE_TRUE) . '
      WHERE id = ' . database_value($id) . ';');
  }

  function mark_obsolete($pkg) {
    isset($pkg) or trigger_error('bad pkg');

    $pkg->mark_obsolete();

    $id = $pkg->id();

    database_query('
      UPDATE ' . $this->table() . '
      SET obsolete = ' . database_value(DATABASE_TRUE) . '
      WHERE id = ' . database_value($id) . ';');
  }

  function create_package($name_version,$description,$author,$license,$status) {
    isset($name_version) or trigger_error('bad name_version');
    is_string($description) or trigger_error('bad description');
    isset($author) or trigger_error('bad author');
    is_string($license) or trigger_error('bad license');
    is_package_status($status) or trigger_error('bad status');

    $id = $this->max_rows('id') + 1;

    $registered = server_datetime();

    $auxiliary = false;

    $obsolete = false;

    $pkg = new Package($id,$name_version,$description,$author,$license,
                       $registered,$status,$auxiliary,$obsolete);

    $this->insert_package($pkg);

    return $pkg;
  }

  function PackageTable($table) {
    global $all_package_status;

    $fields =
      array('id' => 'int(' . PACKAGE_ID_DIGITS . ') NOT NULL',
            'name' => 'varchar(' . PACKAGE_NAME_CHARS . ') NOT NULL',
            'version' => 'varchar(' . PACKAGE_VERSION_CHARS . ') NOT NULL',
            'description' => 'varchar(' . PACKAGE_DESCRIPTION_CHARS . ') NOT NULL',
            'author' => 'int (' . PACKAGE_AUTHOR_ID_DIGITS . ') NOT NULL',
            'license' => 'varchar(' . PACKAGE_LICENSE_CHARS . ') NOT NULL',
            'registered' => 'datetime NOT NULL',
            'status' =>
              array_to_database_enum($all_package_status) . ' NOT NULL',
            'auxiliary' => database_bool_type() . ' NOT NULL',
            'obsolete' => database_bool_type() . ' NOT NULL');

    $indexes =
      array('PRIMARY KEY (id)',
            'INDEX (name,version)',
            'INDEX (status,auxiliary,obsolete,name)',
            'INDEX (status,auxiliary,registered)');

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

///////////////////////////////////////////////////////////////////////////////
// Create a package.
///////////////////////////////////////////////////////////////////////////////

function create_package($name_version,$description,$author,$license) {
  $package_table = package_table();

  return $package_table->create_package($name_version,$description,
                                        $author,$license);
}

///////////////////////////////////////////////////////////////////////////////
// Count packages.
///////////////////////////////////////////////////////////////////////////////

function count_active_packages() {
  $package_table = package_table();

  return $package_table->count_active_packages();
}

///////////////////////////////////////////////////////////////////////////////
// Look up a package.
///////////////////////////////////////////////////////////////////////////////

function find_package($package_id) {
  is_int($package_id) or trigger_error('bad package_id');

  $package_table = package_table();

  return $package_table->find_package($package_id);
}

function find_package_by_name_version($name_version) {
  isset($name_version) or trigger_error('bad name_version');

  $package_table = package_table();

  return $package_table->find_package_by_name_version($name_version);
}

function exists_package_called_name_version($name_version) {
  isset($name_version) or trigger_error('bad name_version');

  $pkg = find_package_by_name_version($name_version);

  return isset($pkg);
}

?>
