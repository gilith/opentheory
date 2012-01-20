<?php

///////////////////////////////////////////////////////////////////////////////
//
// PACKAGE DEPENDENCY TABLES
//
// Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2
//
///////////////////////////////////////////////////////////////////////////////

require_once 'global.php';
require_once 'error.php';
require_once 'date.php';
require_once 'database.php';
require_once 'package.php';

///////////////////////////////////////////////////////////////////////////////
// Tracking package dependencies using a database table.
///////////////////////////////////////////////////////////////////////////////

class PackageGraphTable extends DatabaseTable {
  function insert_edge($parent,$child) {
    isset($parent) or trigger_error('bad parent');
    isset($child) or trigger_error('bad child');

    $parent_id = $parent->id();
    $child_id = $child->id();

    database_query('
      INSERT INTO ' . $this->table() . '
      SET parent = ' . database_value($parent_id) . ',
          child = ' . database_value($child_id) . ';');
  }

  function child_count($parent) {
    isset($parent) or trigger_error('bad parent');

    $parent_id = $parent->id();
    is_int($parent_id) or trigger_error('bad parent_id');

    return $this->count_rows('parent = ' . database_value($parent_id));
  }

  function child_ids($parent) {
    isset($parent) or trigger_error('bad parent');

    $parent_id = $parent->id();
    is_int($parent_id) or trigger_error('bad parent_id');

    $result = database_query('
      SELECT child
      FROM ' . $this->table() . '
      WHERE parent = ' . database_value($parent_id) . '
      ORDER BY child;');

    $child_ids = array();

    while ($row = mysql_fetch_array($result)) {
      $child_ids[] = (integer)$row['child'];
    }

    return $child_ids;
  }

  function parent_count($child) {
    isset($child) or trigger_error('bad child');

    $child_id = $child->id();
    is_int($child_id) or trigger_error('bad child_id');

    return $this->count_rows('child = ' . database_value($child_id));
  }

  function parent_ids($child) {
    isset($child) or trigger_error('bad child');

    $child_id = $child->id();
    is_int($child_id) or trigger_error('bad child_id');

    $result = database_query('
      SELECT parent
      FROM ' . $this->table() . '
      WHERE child = ' . database_value($child_id) . '
      ORDER BY parent;');

    $parent_ids = array();

    while ($row = mysql_fetch_array($result)) {
      $parent_ids[] = (integer)$row['parent'];
    }

    return $parent_ids;
  }

  function delete_package($pkg) {
    isset($pkg) or trigger_error('bad pkg');

    $pkg_id = $pkg->id();

    database_query('
      DELETE FROM ' . $this->table() . '
      WHERE parent = ' . database_value($pkg_id) . ';');

    database_query('
      DELETE FROM ' . $this->table() . '
      WHERE child = ' . database_value($pkg_id) . ';');
  }

  function PackageGraphTable($table) {
    $fields =
      array('parent' => 'int(' . PACKAGE_ID_DIGITS . ') NOT NULL',
            'child' => 'int(' . PACKAGE_ID_DIGITS . ') NOT NULL');

    $indexes =
      array('PRIMARY KEY (parent,child)',
            'INDEX (child,parent)');

    parent::DatabaseTable($table,$fields,$indexes);
  }
}

///////////////////////////////////////////////////////////////////////////////
// The package includes table.
///////////////////////////////////////////////////////////////////////////////

$global_include_table = null;

function include_table() {
  global $global_include_table;

  if (!isset($global_include_table)) {
    $global_include_table = new PackageGraphTable('include');
  }

  return $global_include_table;
}

///////////////////////////////////////////////////////////////////////////////
// Look up package includes.
///////////////////////////////////////////////////////////////////////////////

function count_package_includes($pkg) {
  isset($pkg) or trigger_error('bad pkg');

  $include_table = include_table();

  return $include_table->parent_count($pkg);
}

function package_includes($pkg) {
  isset($pkg) or trigger_error('bad pkg');

  $include_table = include_table();

  $includes_ids = $include_table->parent_ids($pkg);

  $includes = array();

  foreach ($includes_ids as $pkg_id) {
    $pkg = find_package($pkg_id);
    isset($pkg) or trigger_error('bad includes');

    $includes[] = $pkg;
  }

  return $includes;
}

///////////////////////////////////////////////////////////////////////////////
// Look up package included by.
///////////////////////////////////////////////////////////////////////////////

function count_package_included_by($pkg) {
  isset($pkg) or trigger_error('bad pkg');

  $include_table = include_table();

  return $include_table->child_count($pkg);
}

function package_included_by($pkg) {
  isset($pkg) or trigger_error('bad pkg');

  $include_table = include_table();

  $included_by_ids = $include_table->child_ids($pkg);

  $included_by = array();

  foreach ($included_by_ids as $pkg_id) {
    $pkg = find_package($pkg_id);
    isset($pkg) or trigger_error('bad included by');

    $included_by[] = $pkg;
  }

  return $included_by;
}

///////////////////////////////////////////////////////////////////////////////
// Add package include.
///////////////////////////////////////////////////////////////////////////////

function add_package_include($included,$includer) {
  isset($included) or trigger_error('bad included');
  isset($includer) or trigger_error('bad includer');

  $include_table = include_table();

  $include_table->insert_edge($included,$includer);
}

?>
