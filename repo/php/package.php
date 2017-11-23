<?php

///////////////////////////////////////////////////////////////////////////////
//
// PACKAGE TABLE
//
// Copyright (c) 2010 Joe Leslie-Hurd, distributed under the MIT license
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
// Constants.
///////////////////////////////////////////////////////////////////////////////

define('EARLIER_VERSION_LIMIT',2);

define('LATER_VERSION_LIMIT',2);

///////////////////////////////////////////////////////////////////////////////
// Package status.
///////////////////////////////////////////////////////////////////////////////

define('INSTALLED_PACKAGE_STATUS','installed');
define('STAGED_PACKAGE_STATUS','staged');

$all_package_status =
  array(INSTALLED_PACKAGE_STATUS,
        STAGED_PACKAGE_STATUS);

function is_package_status($status) {
  global $all_package_status;

  return is_string($status) && in_array($status,$all_package_status);
}

function equal_package_status($status1,$status2) {
  is_package_status($status1) or trigger_error('bad status1');
  is_package_status($status2) or trigger_error('bad status2');

  return (strcmp($status1,$status2) == 0);
}

function installed_package_status($status) {
  is_package_status($status) or trigger_error('bad status');

  return equal_package_status($status,INSTALLED_PACKAGE_STATUS);
}

function staged_package_status($status) {
  is_package_status($status) or trigger_error('bad status');

  return equal_package_status($status,STAGED_PACKAGE_STATUS);
}

///////////////////////////////////////////////////////////////////////////////
// A class to store package information.
///////////////////////////////////////////////////////////////////////////////

class Package {
  var $_id;
  var $_name_version;
  var $_description;
  var $_author;
  var $_license;
  var $_registered;
  var $_status;
  var $_empty_theory;
  var $_subtheory;
  var $_obsolete;

  function id() { return $this->_id; }

  function name_version() { return $this->_name_version; }

  function description() { return $this->_description; }

  function author() { return $this->_author; }

  function license() { return $this->_license; }

  function registered() { return $this->_registered; }

  function status() { return $this->_status; }

  function empty_theory() { return $this->_empty_theory; }

  function subtheory() { return $this->_subtheory; }

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

  function is_installed() {
    $status = $this->status();

    return installed_package_status($status);
  }

  function is_staged() {
    $status = $this->status();

    return staged_package_status($status);
  }

  function to_string() {
    $namever = $this->name_version();

    return $namever->to_string();
  }

  function external_url() {
    $namever = $this->name_version();

    return $namever->external_url();
  }

  function link($text) {
    is_string($text) or trigger_error('bad text');

    $namever = $this->name_version();

    return $namever->package_link($text);
  }

  function document_file_name() {
    $namever = $this->name_version();

    return $namever->document_file_name();
  }

  function document_file_link($text) {
    is_string($text) or trigger_error('bad text');

    $namever = $this->name_version();

    if ($this->is_staged()) {
      return $namever->staged_document_file_link($text);
    }
    else {
      return $namever->document_file_link($text);
    }
  }

  function tarball_name() {
    $namever = $this->name_version();

    return $namever->tarball_name();
  }

  function tarball_link($text) {
    is_string($text) or trigger_error('bad text');

    $namever = $this->name_version();

    if ($this->is_staged()) {
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

    if ($this->is_staged()) {
      return $namever->staged_theory_file_link($text);
    }
    else {
      return $namever->theory_file_link($text);
    }
  }

  function mark_installed() { $this->_status = INSTALLED_PACKAGE_STATUS; }

  function mark_subtheory() { $this->_subtheory = true; }

  function mark_obsolete() { $this->_obsolete = true; }

  function Package($id,$name_version,$description,$author,$license,
                   $registered,$status,$empty_theory,$subtheory,$obsolete)
  {
    is_int($id) or trigger_error('bad id');
    isset($name_version) or trigger_error('bad name_version');
    is_string($description) or trigger_error('bad description');
    isset($author) or trigger_error('bad author');
    is_string($license) or trigger_error('bad license');
    isset($registered) or trigger_error('bad registered');
    is_package_status($status) or trigger_error('bad status');
    is_bool($empty_theory) or trigger_error('bad empty_theory');
    is_bool($subtheory) or trigger_error('bad subtheory');
    is_bool($obsolete) or trigger_error('bad obsolete');

    $this->_id = $id;
    $this->_name_version = $name_version;
    $this->_description = $description;
    $this->_author = $author;
    $this->_license = $license;
    $this->_registered = $registered;
    $this->_status = $status;
    $this->_empty_theory = $empty_theory;
    $this->_subtheory = $subtheory;
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
  $status = $row['status'];
  $empty_theory_database = $row['empty_theory'];
  $subtheory_database = $row['subtheory'];
  $obsolete_database = $row['obsolete'];

  $name_version = new PackageNameVersion($name,$version);

  $author = get_package_author($author_id);

  $registered = new TimePoint();
  $registered->from_database_datetime($registered_datetime);

  $empty_theory = bool_from_database_bool($empty_theory_database);

  $subtheory = bool_from_database_bool($subtheory_database);

  $obsolete = bool_from_database_bool($obsolete_database);

  return new Package($id,$name_version,$description,$author,$license,
                     $registered,$status,$empty_theory,$subtheory,$obsolete);
}

///////////////////////////////////////////////////////////////////////////////
// A total comparison function on package names.
///////////////////////////////////////////////////////////////////////////////

function compare_package($pkg1,$pkg2) {
  isset($pkg1) or trigger_error('bad pkg1');
  isset($pkg2) or trigger_error('bad pkg2');

  $namever1 = $pkg1->name_version();
  $namever2 = $pkg2->name_version();

  return compare_name_version($namever1,$namever2);
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
      'name <=> ' . database_value($name) .
      ' AND version <=> ' . database_value($version);

    return $this->find_package_where($where);
  }

  function list_packages($where,$order_by,$limit) {
    !isset($where) or is_string($where) or
      trigger_error('bad where');
    !isset($order_by) or is_string($order_by) or
      trigger_error('bad order_by');
    !isset($limit) or is_int($limit) or is_string($limit) or
      trigger_error('bad limit');

    $result = database_query('
      SELECT *
      FROM ' . $this->table() . (isset($where) ? ('
      WHERE ' . $where) : '') . (isset($order_by) ? ('
      ORDER BY ' . $order_by) : '') . (isset($limit) ? ('
      LIMIT ' . $limit) : '') . ';');

    $pkgs = array();

    while ($row = mysqli_fetch_assoc($result)) {
      $pkgs[] = from_row_package($row);
    }

    return $pkgs;
  }

  function list_active_packages() {
    $where =
      'status <=> ' . database_value(INSTALLED_PACKAGE_STATUS) .
      ' AND subtheory <=> ' . database_value(DATABASE_FALSE) .
      ' AND obsolete <=> ' . database_value(DATABASE_FALSE) .
      ' AND empty_theory <=> ' . database_value(DATABASE_FALSE);

    $order_by = 'name';

    $limit = null;

    return $this->list_packages($where,$order_by,$limit);
  }

  function count_active_packages() {
    $where =
      'status <=> ' . database_value(INSTALLED_PACKAGE_STATUS) .
      ' AND subtheory <=> ' . database_value(DATABASE_FALSE) .
      ' AND obsolete <=> ' . database_value(DATABASE_FALSE) .
      ' AND empty_theory <=> ' . database_value(DATABASE_FALSE);

    return $this->count_rows($where);
  }

  function count_packages_with_status($status) {
    is_package_status($status) or trigger_error('bad status');

    $where =
      'status <=> ' . database_value($status);

    return $this->count_rows($where);
  }

  function list_recent_packages($limit) {
    is_int($limit) or trigger_error('bad limit');

    $where =
      'status <=> ' . database_value(INSTALLED_PACKAGE_STATUS) .
      ' AND subtheory <=> ' . database_value(DATABASE_FALSE);

    $order_by = 'registered DESC';

    return $this->list_packages($where,$order_by,$limit);
  }

  function list_package_versions($name) {
    is_string($name) or trigger_error('bad name');

    $where =
      'name <=> ' . database_value($name) .
      ' AND status <=> ' . database_value(INSTALLED_PACKAGE_STATUS);

    $order_by = null;

    $limit = null;

    $pkgs = $this->list_packages($where,$order_by,$limit);

    usort($pkgs,"compare_package") or trigger_error('usort failed');

    return $pkgs;
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

    $empty_theory = $package->empty_theory();
    $empty_theory_database = bool_to_database_bool($empty_theory);

    $subtheory = $package->subtheory();
    $subtheory_database = bool_to_database_bool($subtheory);

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
          empty_theory = ' . database_value($empty_theory_database) . ',
          subtheory = ' . database_value($subtheory_database) . ',
          obsolete = ' . database_value($obsolete_database) . ';');
  }

  function mark_installed($pkg) {
    isset($pkg) or trigger_error('bad pkg');

    $pkg->mark_installed();

    $id = $pkg->id();

    database_query('
      UPDATE ' . $this->table() . '
      SET status = ' . database_value(INSTALLED_PACKAGE_STATUS) . '
      WHERE id = ' . database_value($id) . ';');
  }

  function mark_subtheory($pkg) {
    isset($pkg) or trigger_error('bad pkg');

    $pkg->mark_subtheory();

    $id = $pkg->id();

    database_query('
      UPDATE ' . $this->table() . '
      SET subtheory = ' . database_value(DATABASE_TRUE) . '
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

  function create_package($name_version,$description,$author,$license,
                          $registered,$status,$empty_theory) {
    isset($name_version) or trigger_error('bad name_version');
    is_string($description) or trigger_error('bad description');
    isset($author) or trigger_error('bad author');
    is_string($license) or trigger_error('bad license');
    isset($registered) or trigger_error('bad registered');
    is_package_status($status) or trigger_error('bad status');
    is_bool($empty_theory) or trigger_error('bad empty_theory');

    $id = $this->max_rows('id') + 1;

    $subtheory = false;

    $obsolete = false;

    $pkg = new Package($id,$name_version,$description,$author,$license,
                       $registered,$status,$empty_theory,$subtheory,$obsolete);

    $this->insert_package($pkg);

    return $pkg;
  }

  function delete_package($pkg) {
    isset($pkg) or trigger_error('bad pkg');

    $id = $pkg->id();

    database_query('
      DELETE FROM ' . $this->table() . '
      WHERE id = ' . database_value($id) . ';');
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
            'empty_theory' => database_bool_type() . ' NOT NULL',
            'subtheory' => database_bool_type() . ' NOT NULL',
            'obsolete' => database_bool_type() . ' NOT NULL');

    $indexes =
      array('PRIMARY KEY (id)',
            'INDEX (name,version)',
            'INDEX (status,subtheory,obsolete,empty_theory,name)',
            'INDEX (status,subtheory,registered)');

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

function create_package($name_version,$description,$author,$license,
                        $registered,$empty_theory) {
  $package_table = package_table();

  $status = INSTALLED_PACKAGE_STATUS;

  return $package_table->create_package($name_version,$description,$author,
                                        $license,$registered,$status,
                                        $empty_theory);
}

function create_staged_package($name_version,$description,$author,$license,
                               $registered,$empty_theory) {
  $package_table = package_table();

  $status = STAGED_PACKAGE_STATUS;

  return $package_table->create_package($name_version,$description,$author,
                                        $license,$registered,$status,
                                        $empty_theory);
}

///////////////////////////////////////////////////////////////////////////////
// Count packages.
///////////////////////////////////////////////////////////////////////////////

function count_active_packages() {
  $package_table = package_table();

  return $package_table->count_active_packages();
}

function count_packages_with_status($status) {
  is_package_status($status) or trigger_error('bad status');

  $package_table = package_table();

  return $package_table->count_packages_with_status($status);
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

///////////////////////////////////////////////////////////////////////////////
// Find all versions of a package.
///////////////////////////////////////////////////////////////////////////////

function list_package_versions($name) {
  is_string($name) or trigger_error('bad name');

  $package_table = package_table();

  return $package_table->list_package_versions($name);
}

function latest_package_version($name) {
  is_string($name) or trigger_error('bad name');

  $pkgs = list_package_versions($name);

  $n = count($pkgs);

  if ($n > 0) { return $pkgs[$n - 1]; }
  else { return null; }
}

function previous_package_version($namever) {
  isset($namever) or trigger_error('bad namever');

  $pkgs = list_package_versions($namever->name());

  $prev = null;

  foreach ($pkgs as $pkg) {
    $nv = $pkg->name_version();

    if (compare_name_version($nv,$namever) < 0) {
      $prev = $pkg;
    }
    else {
      return $prev;
    }
  }

  return $prev;
}

function next_package_version($namever) {
  isset($namever) or trigger_error('bad namever');

  $pkgs = array_reverse(list_package_versions($namever->name()));

  $next = null;

  foreach ($pkgs as $pkg) {
    $nv = $pkg->name_version();

    if (compare_name_version($nv,$namever) > 0) {
      $next = $pkg;
    }
    else {
      return $next;
    }
  }

  return $next;
}

function is_latest_package_version($namever) {
  isset($namever) or trigger_error('bad namever');

  $next = next_package_version($namever);

  return !isset($next);
}

function pretty_list_package_versions($namever) {
  isset($namever) or trigger_error('bad namever');

  $pkgs = list_package_versions($namever->name());

  $thisver = $namever->version();

  $earlier_versions = array();
  $later_versions = array();

  foreach ($pkgs as $pkg) {
    $nv = $pkg->name_version();

    $cmp = compare_name_version($nv,$namever);

    if ($cmp < 0) {
      $earlier_versions[] = $nv;
    }
    elseif ($cmp > 0) {
      $later_versions[] = $nv;
    }
  }

  $versions = array();

  $n = count($earlier_versions);
  for ($i = 0; $i < $n; ++$i) {
    if ($i == 0 || $n < $i + EARLIER_VERSION_LIMIT + 1) {
      $nv = $earlier_versions[$i];
      $versions[] =
        '<small>' . $nv->package_link($nv->version()) . '</small>';
    }
    elseif ($n == $i + EARLIER_VERSION_LIMIT + 1) {
      $versions[] = '<small>&nbsp;&middot;&middot;&middot;&nbsp;</small>';
    }
  }

  $versions[] =
    ((count($earlier_versions) == 0) ? '' : '&nbsp;') .
    $thisver .
    ((count($later_versions) == 0) ? '' : '&nbsp;');

  $n = count($later_versions);
  for ($i = 0; $i < $n; ++$i) {
    if ($i + 1 == $n) {
      $nv = $later_versions[$i];
      $versions[] = '&nbsp;' . $nv->package_link($nv->version());
    }
    elseif ($i < LATER_VERSION_LIMIT) {
      $nv = $later_versions[$i];
      $versions[] =
        '<small>' . $nv->package_link($nv->version()) . '</small>';
    }
    elseif ($i == LATER_VERSION_LIMIT) {
      $versions[] = '<small>&nbsp;&middot;&middot;&middot;&nbsp;</small>';
    }
  }

  return implode('<small>&rarr;</small>', $versions);
}

?>
