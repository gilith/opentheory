<?php

///////////////////////////////////////////////////////////////////////////////
//
// PACKAGE DEPENDENCY TABLE
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
// The package dependency database table.
///////////////////////////////////////////////////////////////////////////////

class DependencyTable extends DatabaseTable {
  function insert_dependency($parent,$child) {
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

  function DependencyTable($table) {
    $fields =
      array('parent' => 'int(' . PACKAGE_ID_DIGITS . ') NOT NULL',
            'child' => 'int(' . PACKAGE_ID_DIGITS . ') NOT NULL');

    $indexes =
      array('PRIMARY KEY (parent,child)',
            'INDEX (child,parent)');

    parent::DatabaseTable($table,$fields,$indexes);
  }
}

$global_dependency_table = null;

function dependency_table() {
  global $global_dependency_table;

  if (!isset($global_dependency_table)) {
    $global_dependency_table = new DependencyTable('dependency');
  }

  return $global_dependency_table;
}

///////////////////////////////////////////////////////////////////////////////
// Package children.
///////////////////////////////////////////////////////////////////////////////

function package_children($pkg) {
  isset($pkg) or trigger_error('bad pkg');

  $dependency_table = dependency_table();

  $child_ids = $dependency_table->child_ids($pkg);

  $children = array();

  foreach ($child_ids as $child_id) {
    $child = find_package($child_id);
    isset($child) or trigger_error('bad child');

    $children[] = $child;
  }

  return $children;
}

?>
