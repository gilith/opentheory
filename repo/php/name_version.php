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

  function tarball() { return ($this->to_string() . '.tgz'); }

  function document() { return ($this->to_string() . '.html'); }

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
