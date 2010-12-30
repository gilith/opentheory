<?php

///////////////////////////////////////////////////////////////////////////////
//
// PACKAGE TAGS
//
// Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2
//
///////////////////////////////////////////////////////////////////////////////

require_once 'global.php';
require_once 'error.php';
require_once 'name_version.php';
require_once 'author.php';
require_once 'package.php';

///////////////////////////////////////////////////////////////////////////////
// Package tag constants.
///////////////////////////////////////////////////////////////////////////////

define('PACKAGE_NAME_TAG','name');
define('PACKAGE_VERSION_TAG','version');
define('PACKAGE_DESCRIPTION_TAG','description');
define('PACKAGE_AUTHOR_TAG','author');
define('PACKAGE_LICENSE_TAG','license');

///////////////////////////////////////////////////////////////////////////////
// Package tag regular expressions.
///////////////////////////////////////////////////////////////////////////////

define('PACKAGE_TAG_REGEXP',
       ' *(' . PACKAGE_NAME_REGEXP . ') *: *(.*)');

///////////////////////////////////////////////////////////////////////////////
// A class to store package tags.
///////////////////////////////////////////////////////////////////////////////

class PackageTag {
  var $_name;
  var $_value;

  function name() { return $this->_name; }

  function value() { return $this->_value; }

  function to_string() { return ($this->name() . ': ' . $this->value()); }

  function PackageTag($name,$value) {
    is_string($name) or trigger_error('bad name');
    is_string($value) or trigger_error('bad value');

    $this->_name = $name;
    $this->_value = $value;
  }
}

///////////////////////////////////////////////////////////////////////////////
// Package tag functions.
///////////////////////////////////////////////////////////////////////////////

function from_string_package_tag($line) {
  is_string($line) or trigger_error('bad line');

  $re = '/^' . PACKAGE_TAG_REGEXP . '$/';

  if (preg_match($re,$line,$arr)) {
    $name = $arr[1];
    $value = $arr[2];

    return new PackageTag($name,$value);
  }
  else {
    return null;
  }
}

function find_name_tags($name,$tags) {
  is_string($name) or trigger_error('bad name');
  is_array($tags) or trigger_error('bad tags');

  foreach ($tags as $tag) {
    if (strcmp($tag->name(),$name) == 0) { return $tag->value(); }
  }

  return null;
}

function name_version_from_tags($tags) {
  is_array($tags) or trigger_error('bad tags');

  $name = find_name_tags(PACKAGE_NAME_TAG,$tags);
  is_string($name) or trigger_error('bad name');

  $version = find_name_tags(PACKAGE_VERSION_TAG,$tags);
  is_string($version) or trigger_error('bad version');

  return new PackageNameVersion($name,$version);
}

function description_from_tags($tags) {
  is_array($tags) or trigger_error('bad tags');

  $description = find_name_tags(PACKAGE_DESCRIPTION_TAG,$tags);
  is_string($description) or trigger_error('bad description');

  return $description;
}

function author_from_tags($tags) {
  is_array($tags) or trigger_error('bad tags');

  $author = find_name_tags(PACKAGE_AUTHOR_TAG,$tags);
  is_string($author) or trigger_error('bad author');

  return from_string_package_author($author);
}

function license_from_tags($tags) {
  is_array($tags) or trigger_error('bad tags');

  $license = find_name_tags(PACKAGE_LICENSE_TAG,$tags);
  is_string($license) or trigger_error('bad license');

  return $license;
}

?>
