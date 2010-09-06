<?php

///////////////////////////////////////////////////////////////////////////////
//
// OPENTHEORY DIRECTORY
//
// Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2
//
///////////////////////////////////////////////////////////////////////////////

require_once 'functions.php';
require_once 'date.php';
require_once 'input.php';
require_once 'links.php';

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

  return array(DIRECTORY_DIR,'packages',$name);
}

function package_path($name) {
  is_string($name) or trigger_error('bad name');

  $doc = mk_package_document($name);

  return array(DIRECTORY_DIR,'packages',$name,$doc);
}

///////////////////////////////////////////////////////////////////////////////
// Invoke the opentheory program.
///////////////////////////////////////////////////////////////////////////////

function opentheory($action,$args) {
  $cmd =
OPENTHEORY_BIN .
' -d ' . DIRECTORY_PATH . ' ' .
$action . $args .
' 2>&1 >> ' . LOG_PATH;

  return shell_exec($cmd);
}

?>
