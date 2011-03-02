<?php

///////////////////////////////////////////////////////////////////////////////
//
// PACKAGE NAME/VERSIONS
//
// Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2
//
///////////////////////////////////////////////////////////////////////////////

require_once 'global.php';
require_once 'error.php';
require_once 'links.php';

///////////////////////////////////////////////////////////////////////////////
// Paths.
///////////////////////////////////////////////////////////////////////////////

define('REPO_PATH', SITE_PATH . '/' . REPO_DIR);

define('REPO_LOG_PATH', SITE_PATH . '/' . REPO_LOG);

define('REPO_PACKAGES_DIR','packages');

define('REPO_STAGING_DIR','staging');

///////////////////////////////////////////////////////////////////////////////
// Package name/version regular expressions.
///////////////////////////////////////////////////////////////////////////////

define('PACKAGE_NAME_REGEXP',
       '[a-z][a-z0-9]*(?:-[a-z][a-z0-9]*)*');

define('PACKAGE_VERSION_REGEXP',
       '[0-9]+([.][0-9]+)*');

define('PACKAGE_NAME_VERSION_REGEXP',
       '(' . PACKAGE_NAME_REGEXP . ')-' .
       '(' . PACKAGE_VERSION_REGEXP . ')');

define('PACKAGE_TARBALL_REGEXP',
       '(' . PACKAGE_NAME_VERSION_REGEXP . ')[.]tgz');

///////////////////////////////////////////////////////////////////////////////
// A class to store package name/versions.
///////////////////////////////////////////////////////////////////////////////

class PackageNameVersion {
  var $_name;
  var $_version;

  function name() { return $this->_name; }

  function version() { return $this->_version; }

  function to_string() { return ($this->name() . '-' . $this->version()); }

  function staged_to_string() {
    return ('staged:' . $this->name() . '-' . $this->version());
  }

  function package_link($text) {
    is_string($text) or trigger_error('bad text');

    $path = array();

    $args = array('pkg' => $this->to_string());

    $atts = array('class' => 'package');

    return site_link($path,$text,$args,$atts);
  }

  function directory_name() {
    return $this->to_string();
  }

  function directory_path() {
    $dir = $this->directory_name();

    return array(REPO_DIR,REPO_PACKAGES_DIR,$dir);
  }

  function staged_directory_path() {
    $dir = $this->directory_name();

    return array(REPO_DIR,REPO_STAGING_DIR,$dir);
  }

  function summary_file_name() {
    return ($this->to_string() . '.html');
  }

  function summary_file_path() {
    $dir = $this->directory_name();

    $file = $this->summary_file_name();

    return array(REPO_DIR,REPO_PACKAGES_DIR,$dir,$file);
  }

  function summary_file_link($text) {
    is_string($text) or trigger_error('bad text');

    $path = $this->summary_file_path();

    return site_link($path,$text);
  }

  function tarball_name() {
    return ($this->to_string() . '.tgz');
  }

  function tarball_path() {
    $dir = $this->directory_name();

    $file = $this->tarball_name();

    return array(REPO_DIR,REPO_PACKAGES_DIR,$dir,$file);
  }

  function tarball_link($text) {
    is_string($text) or trigger_error('bad text');

    $path = $this->tarball_path();

    return site_link($path,$text);
  }

  function theory_file_name() {
    return ($this->name() . '.thy');
  }

  function theory_file_path() {
    $dir = $this->directory_name();

    $file = $this->theory_file_name();

    return array(REPO_DIR,REPO_PACKAGES_DIR,$dir,$file);
  }

  function staged_theory_file_path() {
    $dir = $this->directory_name();

    $file = $this->theory_file_name();

    return array(REPO_DIR,REPO_STAGING_DIR,$dir,$file);
  }

  function theory_file_link($text) {
    is_string($text) or trigger_error('bad text');

    $path = $this->theory_file_path();

    return site_link($path,$text);
  }

  function PackageNameVersion($name,$version) {
    is_string($name) or trigger_error('bad name');
    is_string($version) or trigger_error('bad version');

    $this->_name = $name;
    $this->_version = $version;
  }
}

///////////////////////////////////////////////////////////////////////////////
// Package name/version functions.
///////////////////////////////////////////////////////////////////////////////

function is_prefix_package_name($name1,$name2) {
  is_string($name1) or trigger_error('bad name1');
  is_string($name2) or trigger_error('bad name2');

  $s1 = $name1 . '-';
  $s2 = $name2 . '-';

  return is_prefix_string($s1,$s2);
}

function from_string_package_name_version($namever) {
  is_string($namever) or trigger_error('bad namever');

  $re = '/^' . PACKAGE_NAME_VERSION_REGEXP . '$/';

  if (preg_match($re,$namever,$arr)) {
    $name = $arr[1];
    $version = $arr[2];

    return new PackageNameVersion($name,$version);
  }
  else {
    return null;
  }
}

function from_tarball_package_name_version($tarball) {
  is_string($tarball) or trigger_error('bad tarball');

  $re = '/^' . PACKAGE_TARBALL_REGEXP . '$/';

  if (preg_match($re,$tarball,$arr)) {
    $name = $arr[1];

    return from_string_package_name_version($name);
  }
  else {
    return null;
  }
}

?>
