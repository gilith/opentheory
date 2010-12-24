<?php

///////////////////////////////////////////////////////////////////////////////
//
// USER PRIVILEGES
//
// Copyright (c) 2008 Joe Hurd, distributed under the GNU GPL version 2
//
///////////////////////////////////////////////////////////////////////////////

require_once 'global.php';
require_once 'error.php';
require_once 'form.php';

///////////////////////////////////////////////////////////////////////////////
// User privilege levels.
///////////////////////////////////////////////////////////////////////////////

define('PUBLIC_PRIVILEGE','public');  // Represented by 'null'
define('USER_PRIVILEGE','user');
define('ADMIN_PRIVILEGE','admin');

$all_privileges = array(USER_PRIVILEGE,ADMIN_PRIVILEGE);

function is_privilege($privilege) {
  global $all_privileges;
  return is_string($privilege) && in_array($privilege,$all_privileges);
}

function privilege_to_int($privilege) {
  if (!isset($privilege)) {
    return 0;
  }
  else {
    is_string($privilege) or trigger_error('bad privilege');
    if (strcmp($privilege,USER_PRIVILEGE) == 0) {
      return 1;
    }
    elseif (strcmp($privilege,ADMIN_PRIVILEGE) == 0) {
      return 2;
    }
    else {
      trigger_error('unknown privilege');
    }
  }
}

function privilege_cmp($p1,$p2) {
  return int_cmp(privilege_to_int($p1), privilege_to_int($p2));
}

function privilege_equal($p1,$p2) {
  return (privilege_cmp($p1,$p2) == 0);
}

function privilege_less($p1,$p2) {
  return (privilege_cmp($p1,$p2) < 0);
}

function pretty_privilege($p) {
  if (isset($p)) {
    is_privilege($p) or trigger_error('bad privilege');

    return $p;
  }
  else {
    return PUBLIC_PRIVILEGE;
  }
}

///////////////////////////////////////////////////////////////////////////////
// A class to collect privileges.
///////////////////////////////////////////////////////////////////////////////

class SelectPrivilege extends SelectList {
  function error_message_missing() {
    return 'Please select a privilege';
  }

  function null_option_text() {
    return PUBLIC_PRIVILEGE;
  }

  function SelectPrivilege($field,$required) {
    global $all_privileges;

    $options = array();
    foreach ($all_privileges as $priv) {
      $options[$priv] = $priv;
    }

    parent::SelectList($field,$options,$required);
  }
}

?>
