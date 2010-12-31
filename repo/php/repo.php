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
require_once 'package.php';
require_once 'tag.php';
require_once 'dependency.php';
require_once 'opentheory.php';

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

  $package_table = package_table();

  $pkg = $package_table->create_package($name_version,$description,$author,
                                        $license);

  // Record the children in the dependency table

  $dependency_table = dependency_table();

  $children = opentheory_children($name_version);

  foreach ($children as $child_name_version) {
    $child = $package_table->find_package_by_name_version($child_name_version);

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
  $package_table = package_table();
  $package_table->reset();

  $dependency_table = dependency_table();
  $dependency_table->reset();

  repo_register_all();
}

?>
