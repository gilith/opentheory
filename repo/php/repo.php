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
require_once 'opentheory.php';

///////////////////////////////////////////////////////////////////////////////
// Register an installed package with the repo.
///////////////////////////////////////////////////////////////////////////////

function repo_register($name_version) {
  isset($name_version) or trigger_error('bad name_version');

  $tags = opentheory_tags($name_version);

  $description = description_from_tags($tags);

  $author = author_from_tags($tags);

  $license = license_from_tags($tags);

  $package_table = package_table();

  $pkg = $package_table->create_package($name_version,$description,$author,
                                        $license);

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

  repo_register_all();
}

?>
