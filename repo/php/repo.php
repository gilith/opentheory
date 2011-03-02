<?php

///////////////////////////////////////////////////////////////////////////////
//
// REPO OPERATIONS
//
// Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2
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
// Register a staged package with the repo.
///////////////////////////////////////////////////////////////////////////////

function repo_register_staged($upload,$name_version) {
  isset($upload) or trigger_error('bad upload');
  isset($name_version) or trigger_error('bad name_version');

  // Check whether we are already registered

  $pkg = find_package_by_name_version($name_version);

  if (isset($pkg)) { trigger_error('already registered'); }

  // Create a new entry in the package table

  $tags = opentheory_staged_tags($name_version);

  $description = description_from_tags($tags);

  $author = author_from_tags($tags);

  $license = license_from_tags($tags);

  $pkg = create_package($name_version,$description,$author,$license);

  // Add the package to the upload set

  add_package_upload($upload,$pkg);

  // Record the children in the dependency table

  $children = opentheory_staged_children($name_version);

  foreach ($children as $child_name_version) {
    $child = find_package_by_name_version($child_name_version);

    if (!isset($child)) { trigger_error('no child package entry'); }

    if (!($child->installed() || member_package_upload($child,$upload))) {
      trigger_error('child not installed or part of this upload set');
    }

    insert_package_dependency($pkg,$child);
  }

  return $pkg;
}

///////////////////////////////////////////////////////////////////////////////
// Register an installed package with the repo.
///////////////////////////////////////////////////////////////////////////////

function repo_register($name_version) {
  isset($name_version) or trigger_error('bad name_version');

  // Check whether we are already registered

  $package_table = package_table();

  $pkg = $package_table->find_package_by_name_version($name_version);

  if (isset($pkg)) { return $pkg; }

  // Create a new entry in the package table

  $tags = opentheory_tags($name_version);

  $description = description_from_tags($tags);

  $author = author_from_tags($tags);

  $license = license_from_tags($tags);

  $pkg = $package_table->create_package($name_version,$description,$author,
                                        $license);

  $package_table->mark_installed($pkg);

  // Record the children in the dependency table

  $dependency_table = dependency_table();

  $children = opentheory_children($name_version);

  foreach ($children as $child_name_version) {
    $child = repo_register($child_name_version);

    $dependency_table->insert_dependency($pkg,$child);

    if (!$child->auxiliary() && $pkg->is_auxiliary_child($child)) {
      $package_table->mark_auxiliary($child);
    }
  }

  return $pkg;
}

///////////////////////////////////////////////////////////////////////////////
// Register all installed packages with the repo.
///////////////////////////////////////////////////////////////////////////////

function repo_register_all() {
  $name_versions = opentheory_list();

  foreach ($name_versions as $name_version) {
    repo_register($name_version);
  }
}

///////////////////////////////////////////////////////////////////////////////
// Reset the repo and register all installed packages.
///////////////////////////////////////////////////////////////////////////////

function repo_reset() {
  $package_author_table = package_author_table();
  $package_author_table->reset();

  $package_table = package_table();
  $package_table->reset();

  $dependency_table = dependency_table();
  $dependency_table->reset();

  $upload_table = upload_table();
  $upload_table->reset();

  $upload_package_table = upload_package_table();
  $upload_package_table->reset();

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

      $since_uploaded = $pkg->since_uploaded();

      $author_name = $pkg->author_name();

      $ret .=
'<p class="recent-package">' .
$pkg->link($pkg->to_string()) .
' &mdash; ' .
string_to_html($description) .
'<br /><small>' .
'Uploaded ' . $since_uploaded->to_string() . ' ago by ' . $author_name .
'</small></p>';
    }
  }

  return $ret;
}

?>
