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
// A class to store package information.
///////////////////////////////////////////////////////////////////////////////

class Package {
  var $_id;
  var $_name_version;
  var $_description;
  var $_author;
  var $_license;
  var $_uploaded;
  var $_auxiliary;

  function id() { return $this->_id; }

  function name_version() { return $this->_name_version; }

  function description() { return $this->_description; }

  function author() { return $this->_author; }

  function license() { return $this->_license; }

  function uploaded() { return $this->_uploaded; }

  function auxiliary() { return $this->_auxiliary; }

  function name() {
    $namever = $this->name_version();
    return $namever->name();
  }

  function version() {
    $namever = $this->name_version();
    return $namever->version();
  }

  function author_name() {
    $author = $this->author();
    return $author->name();
  }

  function author_email() {
    $author = $this->author();
    return $author->email();
  }

  function since_uploaded() {
    $now = server_datetime();
    $uploaded = $this->uploaded();

    return $now->subtract($uploaded);
  }

  function is_auxiliary_child($child) {
    isset($child) or trigger_error('bad child');

    $name1 = $this->name();
    $name2 = $child->name();

    return is_prefix_package_name($name1,$name2);
  }

  function to_string() {
    $namever = $this->name_version();
    return $namever->to_string();
  }

  function site_link($text) {
    is_string($text) or trigger_error('bad text');

    $path = array('packages');

    $args = array('pkg' => $this->to_string());

    $atts = array('class' => 'package');

    return site_link($path,$text,$args,$atts);
  }

  function Package($id,$name_version,$description,$author,
                   $license,$uploaded,$auxiliary)
  {
    is_int($id) or trigger_error('bad id');
    isset($name_version) or trigger_error('bad name_version');
    is_string($description) or trigger_error('bad description');
    isset($author) or trigger_error('bad author');
    is_string($license) or trigger_error('bad license');
    isset($uploaded) or trigger_error('bad uploaded');
    is_bool($auxiliary) or trigger_error('bad auxiliary');

    $this->_id = $id;
    $this->_name_version = $name_version;
    $this->_description = $description;
    $this->_author = $author;
    $this->_license = $license;
    $this->_uploaded = $uploaded;
    $this->_auxiliary = $auxiliary;
  }
}

function from_row_package($row) {
  is_array($row) or trigger_error('bad row');

  $id = (integer)$row['id'];
  $name = $row['name'];
  $version = $row['version'];
  $description = $row['description'];
  $author_name = $row['author_name'];
  $author_email = $row['author_email'];
  $license = $row['license'];
  $uploaded_datetime = $row['uploaded'];
  $auxiliary_database = $row['auxiliary'];

  $name_version = new PackageNameVersion($name,$version);

  $author = new PackageAuthor($author_name,$author_email);

  $uploaded = new TimePoint();
  $uploaded->from_database_datetime($uploaded_datetime);

  $auxiliary = bool_from_database_bool($auxiliary_database);

  return new Package($id,$name_version,$description,
                     $author,$license,$uploaded,$auxiliary);
}

///////////////////////////////////////////////////////////////////////////////
// The package database table.
///////////////////////////////////////////////////////////////////////////////

define('PACKAGE_ID_DIGITS',7);
define('PACKAGE_NAME_CHARS',100);
define('PACKAGE_VERSION_CHARS',50);
define('PACKAGE_DESCRIPTION_CHARS',200);
define('PACKAGE_AUTHOR_NAME_CHARS',100);
define('PACKAGE_AUTHOR_EMAIL_CHARS',100);
define('PACKAGE_LICENSE_CHARS',50);

class PackageTable extends DatabaseTable {
  function find_package_where($where_condition) {
    is_string($where_condition) or trigger_error('bad where_condition');

    $row = $this->find_row($where_condition);

    if (!isset($row)) { return null; }

    return from_row_package($row);
  }

  function find_package($id) {
    is_int($id) or trigger_error('bad package');

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
    $where = 'auxiliary <=> ' . database_value(DATABASE_FALSE);

    $order_by = 'name';

    $limit = null;

    return $this->list_packages($where,$order_by,$limit);
  }

  function list_recent_packages($limit) {
    is_int($limit) or trigger_error('bad limit');

    $where = 'auxiliary <=> ' . database_value(DATABASE_FALSE);

    $order_by = 'uploaded';

    return $this->list_packages($where,$order_by,$limit);
  }

  function insert_package($package) {
    $id = $package->id();

    $name = $package->name();

    $version = $package->version();

    $description = $package->description();

    $author_name = $package->author_name();

    $author_email = $package->author_email();

    $license = $package->license();

    $uploaded = $package->uploaded();
    $uploaded_datetime = $uploaded->to_database_datetime();

    $auxiliary = $package->auxiliary();
    $auxiliary_database = bool_to_database_bool($auxiliary);

    database_query('
      INSERT INTO ' . $this->table() . '
      SET id = ' . database_value($id) . ',
          name = ' . database_value($name) . ',
          version = ' . database_value($version) . ',
          description = ' . database_value($description) . ',
          author_name = ' . database_value($author_name) . ',
          author_email = ' . database_value($author_email) . ',
          license = ' . database_value($license) . ',
          uploaded = ' . database_value($uploaded_datetime) . ',
          auxiliary = ' . database_value($auxiliary_database) . ';');
  }

  function mark_auxiliary($pkg) {
    isset($pkg) or trigger_error('bad pkg');

    $id = $pkg->id();

    database_query('
      UPDATE ' . $this->table() . '
      SET auxiliary = ' . database_value(DATABASE_TRUE) . '
      WHERE id = ' . database_value($id) . ';');
  }

  function create_package($name_version,$description,$author,$license) {
    isset($name_version) or trigger_error('bad name_version');
    is_string($description) or trigger_error('bad description');
    isset($author) or trigger_error('bad author');
    is_string($license) or trigger_error('bad license');

    $id = $this->max_rows('id') + 1;

    $uploaded = server_datetime();

    $auxiliary = false;

    $package = new Package($id,$name_version,$description,
                           $author,$license,$uploaded,$auxiliary);

    $this->insert_package($package);

    return $package;
  }

  function PackageTable($table) {
    $fields =
      array('id' => 'int(' . PACKAGE_ID_DIGITS . ') NOT NULL',
            'name' => 'varchar(' . PACKAGE_NAME_CHARS . ') NOT NULL',
            'version' => 'varchar(' . PACKAGE_VERSION_CHARS . ') NOT NULL',
            'description' => 'varchar(' . PACKAGE_DESCRIPTION_CHARS . ') NOT NULL',
            'author_name' => 'varchar(' . PACKAGE_AUTHOR_NAME_CHARS . ') NOT NULL',
            'author_email' => 'varchar(' . PACKAGE_AUTHOR_EMAIL_CHARS . ') NOT NULL',
            'license' => 'varchar(' . PACKAGE_LICENSE_CHARS . ') NOT NULL',
            'uploaded' => 'datetime NOT NULL',
            'auxiliary' => database_bool_type() . ' NOT NULL');

    $indexes =
      array('PRIMARY KEY (id)',
            'INDEX (name,version)',
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

?>
