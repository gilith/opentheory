<?php

///////////////////////////////////////////////////////////////////////////////
//
// INPUT PARAMETERS
//
// Copyright (c) 2009 Joe Leslie-Hurd, distributed under the MIT license
//
///////////////////////////////////////////////////////////////////////////////

require_once 'global.php';
require_once 'error.php';
require_once 'functions.php';

///////////////////////////////////////////////////////////////////////////////
// Undo 'helpful' backslashing of user data
///////////////////////////////////////////////////////////////////////////////

function stripslashes_deep($value) {
  $value =
    is_array($value)
    ? array_map('stripslashes_deep', $value)
    : stripslashes($value);

  return $value;
}

function normalize_user_data() {
  if (get_magic_quotes_gpc()) {
    $_POST = array_map('stripslashes_deep', $_POST);
    $_GET = array_map('stripslashes_deep', $_GET);
    $_COOKIE = array_map('stripslashes_deep', $_COOKIE);
  }
}

///////////////////////////////////////////////////////////////////////////////
// Collecting the input parameters.
///////////////////////////////////////////////////////////////////////////////

$global_all_inputs = null;

function all_inputs() {
  global $global_all_inputs;

  if (!isset($global_all_inputs)) {
    normalize_user_data();

    $global_all_inputs = array();

    foreach ($_GET as $key => $value) {
      $global_all_inputs[$key] = deep_copy($value);
    }

    foreach ($_POST as $key => $value) {
      if (!array_key_exists($key,$global_all_inputs)) {
        $global_all_inputs[$key] = deep_copy($value);
      }
    }

    foreach ($_COOKIE as $key => $value) {
      if (!array_key_exists($key,$global_all_inputs)) {
        $global_all_inputs[$key] = deep_copy($value);
      }
    }
  }

  return $global_all_inputs;
}

///////////////////////////////////////////////////////////////////////////////
// Querying the input parameters.
///////////////////////////////////////////////////////////////////////////////

function input($parameter) {
  is_string($parameter) or trigger_error('bad parameter');

  $all_inputs = all_inputs();

  if (array_key_exists($parameter,$all_inputs)) {
    return $all_inputs[$parameter];
  }
  else {
    return null;
  }
}

function input_exists($parameter) {
  is_string($parameter) or trigger_error('bad parameter');
  $value = input($parameter);
  return isset($value);
}

///////////////////////////////////////////////////////////////////////////////
// The browser.
///////////////////////////////////////////////////////////////////////////////

$global_browser = null;

function browser() {
  global $global_browser;

  if (!isset($global_browser)) {
    if (is_array($_SERVER) &&
        array_key_exists('HTTP_USER_AGENT',$_SERVER) &&
        is_string($_SERVER['HTTP_USER_AGENT'])) {
      $global_browser = $_SERVER['HTTP_USER_AGENT'];
    }
    else {
      $global_browser = 'Unknown';
    }
  }

  return $global_browser;
}

function is_mobile() {
  $b = browser();

  return ereg('iPhone',$b);
}

function is_script() {
  $b = browser();

  return (strcmp($b,SCRIPT_NAME) == 0);
}

?>
