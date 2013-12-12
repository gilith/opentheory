<?php

///////////////////////////////////////////////////////////////////////////////
//
// REPO OPERATIONS
//
// Copyright (c) 2010 Joe Leslie-Hurd, distributed under the MIT license
//
///////////////////////////////////////////////////////////////////////////////

require_once 'global.php';
require_once 'error.php';
require_once 'name_version.php';
require_once 'author.php';
require_once 'opentheory.php';
require_once 'package.php';
require_once 'tag.php';
require_once 'dependency.php';
require_once 'upload.php';
require_once 'upload_package.php';

///////////////////////////////////////////////////////////////////////////////
// Check a staged package with the repo.
///////////////////////////////////////////////////////////////////////////////

function repo_check_staged($upload,$name_version,$tags,$includes) {
  isset($upload) or trigger_error('bad upload');
  isset($name_version) or trigger_error('bad name_version');
  is_array($tags) or trigger_error('bad tags');
  is_array($includes) or trigger_error('bad includes');

  // Check that we are not already registered

  if (exists_package_called_name_version($name_version)) {
    trigger_error('package already registered');
  }

  // Check uploaded included packages are part of this upload

  foreach ($includes as $inc_namever) {
    $inc = find_package_by_name_version($inc_namever);

    if ($inc->is_staged() && !member_package_upload($inc,$upload)) {
      $error =
        'included package ' . $inc->to_string() .
        ' is not part of this upload set';

      return $error;
    }
  }

  return null;
}

///////////////////////////////////////////////////////////////////////////////
// Register a staged package with the repo.
///////////////////////////////////////////////////////////////////////////////

function repo_register_staged($upload,$name_version,$tags,$registered,
                              $empty_theory,$includes) {
  isset($upload) or trigger_error('bad upload');
  isset($name_version) or trigger_error('bad name_version');
  is_array($tags) or trigger_error('bad tags');
  isset($registered) or trigger_error('bad registered');
  is_bool($empty_theory) or trigger_error('bad empty_theory');
  is_array($includes) or trigger_error('bad includes');

  // Create a new entry in the package table

  $description = description_from_tags($tags);

  $author = author_from_tags($tags);

  $license = license_from_tags($tags);

  $pkg = create_staged_package($name_version,$description,$author,$license,
                               $registered,$empty_theory);

  // Add the package to the upload set

  add_package_upload($upload,$pkg);

  // Record the package includes in the include table

  foreach ($includes as $inc_namever) {
    $inc = find_package_by_name_version($inc_namever);
    if (!isset($inc)) { trigger_error('no included package entry'); }

    if ($inc->is_staged() && !member_package_upload($inc,$upload)) {
      trigger_error('included package not installed or part of this upload set');
    }

    add_package_include($inc,$pkg);
  }

  return $pkg;
}

///////////////////////////////////////////////////////////////////////////////
// Register an installed package with the repo.
///////////////////////////////////////////////////////////////////////////////

function repo_register($name_version) {
  isset($name_version) or trigger_error('bad name_version');

  // Create a new entry in the package table

  $tags = opentheory_tags($name_version);

  $description = description_from_tags($tags);

  $author = author_from_tags($tags);

  $license = license_from_tags($tags);

  $registered = opentheory_timestamp($name_version);

  $empty_theory = opentheory_empty_theory($name_version);

  $pkg = create_package($name_version,$description,$author,$license,
                        $registered,$empty_theory);

  // Record the package includes in the include table

  $includes = opentheory_includes($name_version);

  foreach ($includes as $inc_namever) {
    $inc = find_package_by_name_version($inc_namever);
    if (!isset($inc)) { trigger_error('no included package entry'); }

    add_package_include($inc,$pkg);
  }
}

///////////////////////////////////////////////////////////////////////////////
// Register all installed packages with the repo.
///////////////////////////////////////////////////////////////////////////////

function repo_register_all() {
  $package_table = package_table();

  opentheory_update();

  $name_versions = opentheory_list('All');

  foreach ($name_versions as $name_version) {
    repo_register($name_version);
  }

  // Mark subtheory packages

  $subtheories = opentheory_list('Subtheories All');

  foreach ($subtheories as $subtheory_namever) {
    $subtheory = find_package_by_name_version($subtheory_namever);
    isset($subtheory) or trigger_error('no entry for subtheory package');

    $package_table->mark_subtheory($subtheory);
  }

  // Mark obsolete packages

  $obsoletes = opentheory_list('(Identity - Latest) All');

  foreach ($obsoletes as $obsolete_namever) {
    $obsolete = find_package_by_name_version($obsolete_namever);
    isset($obsolete) or trigger_error('no entry for obsolete package');

    $package_table->mark_obsolete($obsolete);
  }
}

///////////////////////////////////////////////////////////////////////////////
// Reset the repo and register all installed packages.
///////////////////////////////////////////////////////////////////////////////

function repo_reset() {
  // Delete all staged packages
  opentheory_cleanup_all();

  $package_author_table = package_author_table();
  $package_author_table->reset();

  $package_table = package_table();
  $package_table->reset();

  $include_table = include_table();
  $include_table->reset();

  $upload_table = upload_table();
  $upload_table->reset();

  $upload_package_table = upload_package_table();
  $upload_package_table->reset();

  $confirm_upload_table = confirm_upload_table();
  $confirm_upload_table->reset();

  repo_register_all();
}

///////////////////////////////////////////////////////////////////////////////
// Pretty-print recently uploaded packages.
///////////////////////////////////////////////////////////////////////////////

function pretty_recent_packages($limit) {
  is_int($limit) or trigger_error('bad limit');

  $package_table = package_table();

  $pkgs = $package_table->list_recent_packages($limit);

  if (count($pkgs) == 0) {
    $ret = '<p>No theory packages have been uploaded to this repo.</p>';
  }
  else {
    $ret = '';

    foreach ($pkgs as $pkg) {
      $description = $pkg->description();

      $since_registered = $pkg->since_registered();

      $author_name = $pkg->author_name();

      $ret .=
'<p class="recent-package">' .
$pkg->link($pkg->to_string()) .
' &mdash; ' .
string_to_html($description) .
'<br /><small>' .
'Uploaded ' . $since_registered->to_string() . ' ago by ' . $author_name .
'</small></p>';
    }
  }

  return $ret;
}

?>
