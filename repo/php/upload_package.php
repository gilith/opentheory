<?php

///////////////////////////////////////////////////////////////////////////////
//
// UPLOADED PACKAGES TABLE
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
require_once 'upload.php';

///////////////////////////////////////////////////////////////////////////////
// The uploaded packages database table.
///////////////////////////////////////////////////////////////////////////////

define('UPLOAD_PACKAGE_ID_DIGITS',4);

class UploadPackageTable extends DatabaseTable {
  function insert_upload_package($upload,$pkg) {
    isset($upload) or trigger_error('bad upload');
    isset($pkg) or trigger_error('bad pkg');

    $upload_id = $upload->id();
    $pkg_id = $pkg->id();

    $where_condition = 'upload = ' . database_value($upload_id);

    $sequence = $this->max_rows('sequence',$where_condition) + 1;

    database_query('
      INSERT INTO ' . $this->table() . '
      SET upload = ' . database_value($upload_id) . ',
          sequence = ' . database_value($sequence) . ',
          package = ' . database_value($pkg_id) . ';');
  }

  function sequence_package_upload($upload,$pkg) {
    isset($upload) or trigger_error('bad upload');
    isset($pkg) or trigger_error('bad pkg');

    $upload_id = $upload->id();
    $pkg_id = $pkg->id();

    $where_condition =
      'upload = ' . database_value($upload_id) . ' AND ' .
      'package = ' . database_value($pkg_id);

    $row = $this->find_row($where_condition);

    if (!isset($row)) { return null; }

    return (integer)$row['sequence'];
  }

  function package_count($upload) {
    isset($upload) or trigger_error('bad upload');

    $upload_id = $upload->id();
    is_int($upload_id) or trigger_error('bad upload_id');

    return $this->count_rows('upload = ' . database_value($upload_id));
  }

  function package_ids($upload) {
    isset($upload) or trigger_error('bad upload');

    $upload_id = $upload->id();
    is_string($upload_id) or trigger_error('bad upload_id');

    $result = database_query('
      SELECT package
      FROM ' . $this->table() . '
      WHERE upload = ' . database_value($upload_id) . '
      ORDER BY sequence;');

    $pkg_ids = array();

    while ($row = mysql_fetch_array($result)) {
      $pkg_ids[] = (integer)$row['package'];
    }

    return $pkg_ids;
  }

  function UploadPackageTable($table) {
    $fields =
      array('upload' => 'char(' . UPLOAD_ID_CHARS . ') NOT NULL',
            'sequence' => 'int(' . UPLOAD_PACKAGE_ID_DIGITS . ') NOT NULL',
            'package' => 'int(' . PACKAGE_ID_DIGITS . ') NOT NULL');

    $indexes =
      array('PRIMARY KEY (upload,sequence)');

    parent::DatabaseTable($table,$fields,$indexes);
  }
}

$global_upload_package_table = null;

function upload_package_table() {
  global $global_upload_package_table;

  if (!isset($global_upload_package_table)) {
    $global_upload_package_table = new UploadPackageTable('upload_package');
  }

  return $global_upload_package_table;
}

///////////////////////////////////////////////////////////////////////////////
// Add a package to an upload set.
///////////////////////////////////////////////////////////////////////////////

function add_package_upload($upload,$pkg) {
  isset($upload) or trigger_error('bad upload');
  isset($pkg) or trigger_error('bad pkg');

  $upload_package_table = upload_package_table();

  $upload_package_table->insert_upload_package($upload,$pkg);
}

///////////////////////////////////////////////////////////////////////////////
// Look up the uploaded packages.
///////////////////////////////////////////////////////////////////////////////

function package_count_upload($upload) {
  isset($upload) or trigger_error('bad upload');

  $upload_package_table = upload_package_table();

  return $upload_package_table->package_count($upload);
}

function member_package_upload($pkg,$upload) {
  isset($pkg) or trigger_error('bad pkg');
  isset($upload) or trigger_error('bad upload');

  $upload_package_table = upload_package_table();

  $seq = $upload_package_table->sequence_package_upload($upload,$pkg);

  return (isset($seq));
}

function packages_upload($upload) {
  isset($upload) or trigger_error('bad upload');

  $upload_package_table = upload_package_table();

  $pkg_ids = $upload_package_table->package_ids($upload);

  $pkgs = array();

  foreach ($pkg_ids as $pkg_id) {
    $pkg = find_package($pkg_id);
    isset($pkg) or trigger_error('bad pkg');

    $pkgs[] = $pkg;
  }

  return $pkgs;
}

?>
