<?php

///////////////////////////////////////////////////////////////////////////////
//
// OPENTHEORY TOOL INTERFACE
//
// Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2
//
///////////////////////////////////////////////////////////////////////////////

require_once 'functions.php';
require_once 'date.php';
require_once 'input.php';
require_once 'links.php';

///////////////////////////////////////////////////////////////////////////////
// Paths.
///////////////////////////////////////////////////////////////////////////////

define('PACKAGES_DIR','packages');

define('REPO_PATH', SITE_PATH . '/' . REPO_DIR);

define('LOG_PATH', SITE_PATH . '/' . REPO_LOG);

///////////////////////////////////////////////////////////////////////////////
// Link to a package.
///////////////////////////////////////////////////////////////////////////////

function package_directory_path($name) {
  is_string($name) or trigger_error('bad name');

  return array(REPO_DIR,PACKAGES_DIR,$name);
}

function package_path($name) {
  is_string($name) or trigger_error('bad name');

  $doc = mk_package_document($name);

  return array(REPO_DIR,PACKAGES_DIR,$name,$doc);
}

///////////////////////////////////////////////////////////////////////////////
// Invoke the opentheory program.
///////////////////////////////////////////////////////////////////////////////

function opentheory_action($action,$args) {
  is_string($action) or trigger_error('bad action');

  $cmd =
OPENTHEORY_BIN .
' -d ' . REPO_PATH . ' ' .
$action . $args .
' 2>&1 >> ' . LOG_PATH;

  return shell_exec($cmd);
}

///////////////////////////////////////////////////////////////////////////////
// Initialize the opentheory repo.
///////////////////////////////////////////////////////////////////////////////

function opentheory_init() {
  $args = '';

  $output = opentheory_action('init',$args);

  if (isset($output)) {
    trigger_error('couldn\'t initialize directory: ' . $output);
  }
}

///////////////////////////////////////////////////////////////////////////////
// Install a package from a tarball.
///////////////////////////////////////////////////////////////////////////////

function opentheory_install($tarball,$name,$checksum) {
  is_string($tarball) or trigger_error('bad tarball');
  isset($name) or trigger_error('bad name');
  !isset($checksum) or is_string($checksum) or trigger_error('bad checksum');

  $args = ' --minimal';

  $args .= ' --name ' . $name->name();

  if (isset($checksum)) { $args .= ' --checksum ' . $checksum; }

  $args .= ' tarball:' . $tarball;

  $output = opentheory_action('install',$args);

  return $output;
}

///////////////////////////////////////////////////////////////////////////////
// Reset the opentheory repo.
///////////////////////////////////////////////////////////////////////////////

function opentheory_reset() {
  $cmd = 'rm -r ' . REPO_PATH;

  shell_exec($cmd);

  opentheory_init();
}

///////////////////////////////////////////////////////////////////////////////
// Check the opentheory repo exists.
///////////////////////////////////////////////////////////////////////////////

if (effective_privilege_is_admin() && !file_exists(REPO_PATH)) {
  opentheory_init();
}

?>
