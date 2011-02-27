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

class UploadPackageTable extends DatabaseTable {
  function insert_upload_package($upload,$pkg) {
    isset($upload) or trigger_error('bad upload');
    isset($pkg) or trigger_error('bad pkg');

    $upload_id = $upload->id();
    $pkg_id = $pkg->id();

    database_query('
      INSERT INTO ' . $this->table() . '
      SET upload = ' . database_value($upload_id) . ',
          package = ' . database_value($pkg_id) . ';');
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
    is_int($upload_id) or trigger_error('bad upload_id');

    $result = database_query('
      SELECT package
      FROM ' . $this->table() . '
      WHERE upload = ' . database_value($upload_id) . '
      ORDER BY package;');

    $pkg_ids = array();

    while ($row = mysql_fetch_array($result)) {
      $pkg_ids[] = (integer)$row['package'];
    }

    return $pkg_ids;
  }

  function UploadPackageTable($table) {
    $fields =
      array('upload' => 'char(' . UPLOAD_ID_CHARS . ') NOT NULL',
            'package' => 'int(' . PACKAGE_ID_DIGITS . ') NOT NULL');

    $indexes =
      array('PRIMARY KEY (upload,package)');

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
// Look up the uploaded packages.
///////////////////////////////////////////////////////////////////////////////

function package_count_upload($upload) {
  isset($upload) or trigger_error('bad upload');

  $upload_package_table = upload_package_table();

  return $upload_package_table->package_count($upload);
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
