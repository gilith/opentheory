<?php

///////////////////////////////////////////////////////////////////////////////
//
// DATABASE
//
// Copyright (c) 2006 Joe Hurd, distributed under the GNU GPL version 2
//
///////////////////////////////////////////////////////////////////////////////

require_once 'global.php';
require_once 'error.php';
require_once 'functions.php';

///////////////////////////////////////////////////////////////////////////////
// Connecting to the MySQL database
///////////////////////////////////////////////////////////////////////////////

define('DATABASE_CONNECT_TRIES',2);

$global_database_connection = null;

function database_connection() {
  global $global_database_connection;

  if (!isset($global_database_connection)) {
    $tries = DATABASE_CONNECT_TRIES;

    // This uses persistent connections: mysql_pconnect
    // To use simple connections, replace with: mysql_connect

    while (!($global_database_connection =
               mysql_pconnect(DATABASE_HOST,DATABASE_USER,DATABASE_PASSWORD))) {
      if (--$tries <= 0) {
        trigger_error('could not connect to MySQL server');
      }
    }

    if (!(mysql_select_db(DATABASE_NAME,$global_database_connection))) {
      trigger_error('could not select MySQL database: ' .
                    'error #' . mysql_errno() . ' = ' . mysql_error());
    }
  }

  return $global_database_connection;
}

///////////////////////////////////////////////////////////////////////////////
// Profiling database queries.
///////////////////////////////////////////////////////////////////////////////

$global_total_database_queries = 0;

function total_database_queries() {
  global $global_total_database_queries;
  return $global_total_database_queries;
}

function increment_total_database_queries() {
  global $global_total_database_queries;
  ++$global_total_database_queries;
}

///////////////////////////////////////////////////////////////////////////////
// Querying the database.
///////////////////////////////////////////////////////////////////////////////

function database_query($query) {
  $connection = database_connection();

  increment_total_database_queries();

#  var_dump($query);

  if (!($result = mysql_query($query,$connection))) {
    trigger_error('MySQL Error ' . mysql_errno() . ': ' . mysql_error()
                  . "\n\n" . $query);
  }

  return $result;
}

///////////////////////////////////////////////////////////////////////////////
// Escaping quotes in database values.
///////////////////////////////////////////////////////////////////////////////

function database_value($v) {
  if (!isset($v)) { return 'NULL'; }

  $v = addslashes($v);

  return ('\'' . $v . '\'');
}

///////////////////////////////////////////////////////////////////////////////
// Generic database tables.
///////////////////////////////////////////////////////////////////////////////

class DatabaseTable {
  var $_table;
  var $_fields;
  var $_indexes;

  function table() { return $this->_table; }

  function fields() { return $this->_fields; }

  function indexes() { return $this->_indexes; }

  function select_unique($select_expression, $where_condition = null) {
    is_string($select_expression) or trigger_error('bad select_expression');

    $result = database_query('
      SELECT ' . $select_expression . '
      FROM ' . $this->table() . (isset($where_condition) ? ('
      WHERE ' . $where_condition) : '') . ';');

    if ($row = mysql_fetch_array($result)) {
      if (mysql_fetch_array($result)) {
        trigger_error('multiple rows');
      }
      else {
        return $row;
      }
    }
    else {
      return null;
    }
  }

  function count_rows($where_condition = null) {
    $select_expression = 'COUNT(*)';

    $row = $this->select_unique($select_expression, $where_condition);
    isset($row) or trigger_error('empty result');

    return int_from_string($row[$select_expression]);
  }

  function max_rows($max_expression, $where_condition = null) {
    $select_expression = 'MAX(' . $max_expression . ')';

    $row = $this->select_unique($select_expression, $where_condition);
    isset($row) or trigger_error('empty result');

    return int_from_string($row[$select_expression]);
  }

  function find_row($where_condition) {
    is_string($where_condition) or trigger_error('bad where_condition');

    $result = database_query('
      SELECT *
      FROM ' . $this->table() . '
      WHERE ' . $where_condition . ';');

    if ($row = mysql_fetch_array($result)) {
      if (mysql_fetch_array($result)) {
        trigger_error('multiple rows');
      }
      else {
        return $row;
      }
    }
    else {
      return null;
    }
  }

  function is_field($field) {
    return array_key_exists($field, $this->_fields);
  }

  function field_type($field) {
    $type = $this->_fields[$field];

    is_string($type) or trigger_error('bad field');

    return $type;
  }

  function field_names() {
    $names = array();

    foreach ($this->_fields as $name => $type) {
      $names[] = $name;
    }

    return $names;
  }

  function reset_no_indexes() {
    database_query('DROP TABLE IF EXISTS ' . $this->table() . ';');

    $query = 'CREATE TABLE ' . $this->table() . ' (';

    $first = true;

    foreach ($this->_fields as $field => $field_type) {
      if ($first) { $first = false; } else { $query .= ','; }
      $query .= "\n" . '  ' . $field . ' ' . $field_type;
    }

    foreach ($this->_indexes as $index) {
      if (ereg('^PRIMARY KEY',$index)) {
        if ($first) { $first = false; } else { $query .= ','; }
        $query .= "\n" . '  ' . $index;
      }
    }

    $query .= "\n" . ') type=MyISAM;';

    database_query($query);
  }

  function reset_add_indexes() {
    $query = 'ALTER TABLE ' . $this->table();

    $first = true;

    foreach ($this->_indexes as $index) {
      if (!ereg('^PRIMARY KEY',$index)) {
        if ($first) { $first = false; } else { $query .= ','; }
        $query .= "\n" . '  ADD ' . $index;
      }
    }

    $query .= ';';

    database_query($query);
  }

  function reset() {
    $this->reset_no_indexes();
    $this->reset_add_indexes();
  }

  function import_from($table) {
    $fields = array();
    foreach ($this->field_names() as $field) {
      if (ereg('^cached_',$field)) {
        $fields[] = database_value(null);
      }
      else {
        $fields[] = $field;
      }
    }

    database_query('
      INSERT INTO ' . $this->table() . '
      SELECT ' . implode(',',$fields) . '
      FROM ' . $table . ';');
  }

  function copy_from($table) {
    $this->reset_no_indexes();
    $this->import_from($table);
    $this->reset_add_indexes();
  }

  function DatabaseTable($table,$fields,$indexes) {
    is_string($table) or trigger_error('bad table name');
    is_array($fields) or trigger_error('bad fields');
    is_array($indexes) or trigger_error('bad indexes');

    $this->_table = $table;
    $this->_fields = $fields;
    $this->_indexes = $indexes;
  }
}

///////////////////////////////////////////////////////////////////////////////
// Enumerated types.
///////////////////////////////////////////////////////////////////////////////

function array_to_database_enum($values) {
  isset($values) or trigger_error('bad values');

  $x = '';
  foreach ($values as $value) {
    isset($value) or trigger_error('bad value');
    if (strcmp($x,'') != 0) { $x .= ','; }
    $x .= database_value($value);
  }
  return 'enum(' . $x . ')';
}

///////////////////////////////////////////////////////////////////////////////
// Booleans.
///////////////////////////////////////////////////////////////////////////////

define('DATABASE_TRUE','T');
define('DATABASE_FALSE','F');

$all_database_bools = array(DATABASE_TRUE,DATABASE_FALSE);

function database_bool_type() {
  global $all_database_bools;
  return array_to_database_enum($all_database_bools);
}

function bool_to_database_bool($bool) {
  if (!isset($bool)) { return null; }
  else { return $bool ? DATABASE_TRUE : DATABASE_FALSE; }
}

function bool_from_database_bool($value) {
  if (!isset($value)) { return null; }
  elseif (strcmp($value,DATABASE_TRUE) == 0) { return true; }
  elseif (strcmp($value,DATABASE_FALSE) == 0) { return false; }
  else { trigger_error('bad value'); }
}

?>
