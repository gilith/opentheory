<?php

///////////////////////////////////////////////////////////////////////////////
//
// PACKAGE NAME/VERSIONS
//
// Copyright (c) 2010 Joe Leslie-Hurd, distributed under the MIT license
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

  function external_url() {
    $path = array();

    $args = array('pkg' => $this->to_string());

    return external_site_url($path,$args);
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

  function document_file_name() {
    return ($this->to_string() . '.html');
  }

  function document_file_path() {
    $dir = $this->directory_name();

    $file = $this->document_file_name();

    return array(REPO_DIR,REPO_PACKAGES_DIR,$dir,$file);
  }

  function staged_document_file_path() {
    $dir = $this->directory_name();

    $file = $this->document_file_name();

    return array(REPO_DIR,REPO_STAGING_DIR,$dir,$file);
  }

  function document_file_link($text) {
    is_string($text) or trigger_error('bad text');

    $path = $this->document_file_path();

    return site_link($path,$text);
  }

  function staged_document_file_link($text) {
    is_string($text) or trigger_error('bad text');

    $path = $this->staged_document_file_path();

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

  function staged_tarball_path() {
    $dir = $this->directory_name();

    $file = $this->tarball_name();

    return array(REPO_DIR,REPO_STAGING_DIR,$dir,$file);
  }

  function tarball_link($text) {
    is_string($text) or trigger_error('bad text');

    $path = $this->tarball_path();

    return site_link($path,$text);
  }

  function staged_tarball_link($text) {
    is_string($text) or trigger_error('bad text');

    $path = $this->staged_tarball_path();

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

  function staged_theory_file_link($text) {
    is_string($text) or trigger_error('bad text');

    $path = $this->staged_theory_file_path();

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

function is_valid_package_name($name) {
  is_string($name) or trigger_error('bad name');

  $re = '/^' . PACKAGE_NAME_REGEXP . '$/';

  return (preg_match($re,$name) != 0);
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

///////////////////////////////////////////////////////////////////////////////
// A total comparison function.
///////////////////////////////////////////////////////////////////////////////

function compare_version($ver1,$ver2) {
  is_string($ver1) or trigger_error('bad ver1');
  is_string($ver2) or trigger_error('bad ver2');

  $cs1 = explode('.',$ver1);
  $cs2 = explode('.',$ver2);

  $n1 = count($cs1);
  $n2 = count($cs2);

  $n = ($n1 < $n2 ? $n1 : $n2);

  for ($i = 0; $i < $n; ++$i) {
    $c1 = (integer)$cs1[$i];
    $c2 = (integer)$cs2[$i];

    $cmp = int_cmp($c1,$c2);

    if ($cmp != 0) { return $cmp; }
  }

  return int_cmp($n1,$n2);
}

function compare_name_version($namever1,$namever2) {
  isset($namever1) or trigger_error('bad namever1');
  isset($namever2) or trigger_error('bad namever2');

  $cmp = strcmp($namever1->name(), $namever2->name());

  if ($cmp != 0) { return $cmp; }

  $cmp = compare_version($namever1->version(), $namever2->version());

  return $cmp;
}

?>
