<?php

///////////////////////////////////////////////////////////////////////////////
//
// UPLOADED PACKAGES TABLE
//
// Copyright (c) 2011 Joe Leslie-Hurd, distributed under the MIT license
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

    while ($row = mysqli_fetch_assoc($result)) {
      $pkg_ids[] = (integer)$row['package'];
    }

    return $pkg_ids;
  }

  function delete_package($pkg) {
    isset($pkg) or trigger_error('bad pkg');

    $pkg_id = $pkg->id();

    database_query('
      DELETE FROM ' . $this->table() . '
      WHERE package = ' . database_value($pkg_id) . ';');
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

///////////////////////////////////////////////////////////////////////////////
// Delete a staged package.
///////////////////////////////////////////////////////////////////////////////

function delete_staged_package($pkg) {
  isset($pkg) or trigger_error('bad pkg');
  $pkg->is_staged() or trigger_error('not staged');

  opentheory_cleanup($pkg->name_version());

  $upload_package_table = upload_package_table();
  $upload_package_table->delete_package($pkg);

  $include_table = include_table();
  $include_table->delete_package($pkg);

  $package_table = package_table();
  $package_table->delete_package($pkg);
}

///////////////////////////////////////////////////////////////////////////////
// Compute the set of packages that are obsoleted by an upload.
///////////////////////////////////////////////////////////////////////////////

function obsolete_packages_upload($upload) {
  isset($upload) or trigger_error('bad upload');

  $pkgs = packages_upload($upload);

  $obs_map = array();

  foreach ($pkgs as $pkg) {
    $namever = $pkg->name_version();

    $obs_pkg = previous_package_version($namever);

    if (isset($obs_pkg)) {
      $obs_namever = $obs_pkg->name_version();

      $obs_map[$obs_namever->to_string()] = $obs_pkg;
    }
  }

  $obs_pkgs = array();

  foreach ($obs_map as $pkg) {
    $obs_pkgs[] = $pkg;
  }

  return $obs_pkgs;
}

///////////////////////////////////////////////////////////////////////////////
// Compute the other author (if any) of packages obsoleted by an upload.
///////////////////////////////////////////////////////////////////////////////

function obsolete_author_upload($upload) {
  isset($upload) or trigger_error('bad upload');

  $author = $upload->author();
  isset($author) or trigger_error('bad author');

  $obsolete = null;

  $pkgs = obsolete_packages_upload($upload);

  foreach ($pkgs as $pkg) {
    $pkg_author = $pkg->author();

    if (!$pkg_author->equal($author)) {
      if (isset($obsolete)) {
        if (!$obsolete->equal($pkg_author)) {
          trigger_error('upload obsoletes packages by multiple other authors');
        }
      }
      else {
        $obsolete = $pkg_author;
      }
    }
  }

  return $obsolete;
}

///////////////////////////////////////////////////////////////////////////////
// Complete a package upload.
///////////////////////////////////////////////////////////////////////////////

function complete_upload($upload) {
  isset($upload) or trigger_error('bad upload');

  $author = $upload->author();
  isset($author) or trigger_error('bad author');

  $pkgs = packages_upload($upload);

  $package_table = package_table();

  foreach ($pkgs as $pkg) {
    $namever = $pkg->name_version();

    opentheory_complete($namever);

    $package_table->mark_installed($pkg);

    // Mark subtheories of uploaded packages

    $subtheories = opentheory_subtheories($namever);

    foreach ($subtheories as $subtheory_namever) {
      $subtheory = find_package_by_name_version($subtheory_namever);
      if (!isset($subtheory)) { trigger_error('no subtheory package entry'); }

      if (!$subtheory->subtheory()) {
        $package_table->mark_subtheory($subtheory);
      }
    }
  }

  // Mark packages obsoleted by this upload

  $obsoletes = obsolete_packages_upload($upload);

  foreach ($obsoletes as $pkg) {
    if (!$pkg->obsolete()) {
      $package_table->mark_obsolete($pkg);
    }
  }

  // Refresh the uploaded package list from the package table to ensure we
  // have the correct subtheory and obsolete markings

  $pkgs = packages_upload($upload);

  // Mark uploaded packages that are obsolete on arrival

  foreach ($pkgs as $pkg) {
    if (!$pkg->obsolete()) {
      $namever = $pkg->name_version();

      if (!is_latest_package_version($namever)) {
        $package_table->mark_obsolete($pkg);
      }
    }
  }

  // Delete the upload

  $upload_package_table = upload_table();
  $upload_package_table->delete_upload($upload);

  $upload_table = upload_table();
  $upload_table->delete_upload($upload);

  // Tweet about non-subtheory packages

  foreach ($pkgs as $pkg) {
    $namever = $pkg->name_version();

    if (!$pkg->subtheory()) {
      $status =
        $namever->to_string() . ' uploaded by ' .
        $author->name() . ' ' . $pkg->external_url();

      opentheory_tweet($status);
    }
  }
}

///////////////////////////////////////////////////////////////////////////////
// Delete a package upload.
///////////////////////////////////////////////////////////////////////////////

function delete_upload($upload) {
  isset($upload) or trigger_error('bad upload');

  $pkgs = array_reverse(packages_upload($upload));

  foreach ($pkgs as $pkg) {
    delete_staged_package($pkg);
  }

  $upload_table = upload_table();
  $upload_table->delete_upload($upload);
}

?>
