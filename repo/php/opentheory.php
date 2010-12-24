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
// Package name functions.
///////////////////////////////////////////////////////////////////////////////

define('PACKAGE_BASE_REGEXP',
       '[a-z][a-z0-9]*(-[a-z][a-z0-9]*)*');

define('PACKAGE_VERSION_REGEXP',
       '[0-9]+([.][0-9]+)*');

define('PACKAGE_NAME_REGEXP',
       '(' . PACKAGE_BASE_REGEXP . ')-(' . PACKAGE_VERSION_REGEXP . ')');

define('PACKAGE_TARBALL_REGEXP',
       '(' . PACKAGE_NAME_REGEXP . ')[.]tgz');

function dest_package_tarball($s) {
  is_string($s) or trigger_error('bad s');

  $re = '^' . PACKAGE_TARBALL_REGEXP . '$';

  if (ereg($re,$s,$arr)) {
    return $arr[1];
  }
  else {
    return null;
  }
}

function mk_package_document($name) {
  is_string($name) or trigger_error('bad name');

  return $name . '.html';
}

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

function opentheory($action,$args) {
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

  $output = opentheory('init',$args);

  if (isset($output)) {
    trigger_error('couldn\'t initialize directory: ' . $output);
  }

  opentheory_list();
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

if (!file_exists(REPO_PATH)) {
  opentheory_init();
}

?>
