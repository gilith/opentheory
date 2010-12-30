<?php

///////////////////////////////////////////////////////////////////////////////
//
// PACKAGE AUTHORS
//
// Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2
//
///////////////////////////////////////////////////////////////////////////////

require_once 'global.php';
require_once 'error.php';

///////////////////////////////////////////////////////////////////////////////
// Package author regular expressions.
///////////////////////////////////////////////////////////////////////////////

define('PACKAGE_AUTHOR_NAME_REGEXP',
       '[a-zA-Z0-9 ]+');

define('PACKAGE_AUTHOR_EMAIL_REGEXP',
       '[a-zA-Z0-9.@-]+');

define('PACKAGE_AUTHOR_REGEXP',
       '(' . PACKAGE_AUTHOR_NAME_REGEXP . ') <' .
       '(' . PACKAGE_AUTHOR_EMAIL_REGEXP . ')>');

///////////////////////////////////////////////////////////////////////////////
// A class to store package authors.
///////////////////////////////////////////////////////////////////////////////

class PackageAuthor {
  var $_name;
  var $_email;

  function name() { return $this->_name; }

  function email() { return $this->_email; }

  function to_string() {
    return ($this->name() . ' <' . $this->email() . '>');
  }

  function PackageAuthor($name,$email) {
    is_string($name) or trigger_error('bad name');
    is_string($email) or trigger_error('bad email');

    $this->_name = $name;
    $this->_email = $email;
  }
}

///////////////////////////////////////////////////////////////////////////////
// Package author functions.
///////////////////////////////////////////////////////////////////////////////

function from_string_package_author($author) {
  is_string($author) or trigger_error('bad author');

  $re = '^' . PACKAGE_AUTHOR_REGEXP . '$';

  if (ereg($re,$author,$arr)) {
    $name = $arr[1];
    $email = $arr[2];

    return new PackageAuthor($name,$email);
  }
  else {
    return null;
  }
}

?>
