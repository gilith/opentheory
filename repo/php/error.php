<?php

///////////////////////////////////////////////////////////////////////////////
//
// ERROR HANDLING
//
// Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2
//
///////////////////////////////////////////////////////////////////////////////

require_once 'global.php';

///////////////////////////////////////////////////////////////////////////////
// Whether to email error reports or display them onscreen.
///////////////////////////////////////////////////////////////////////////////

define('ERROR_OUTPUT_EMAIL',0);
define('ERROR_OUTPUT_SCRIPT',1);
define('ERROR_OUTPUT_DEBUG',2);

$error_output = ERROR_OUTPUT_LEVEL;

///////////////////////////////////////////////////////////////////////////////
// Define a custom error handler.
///////////////////////////////////////////////////////////////////////////////

function error_handler($errno, $errstr, $errfile, $errline, $errcontext) {
  global $error_output;

  $fatal = true;
  $err = null;

  switch ($errno) {
  case E_WARNING: if (!isset($err)) { $err = 'E_WARNING'; }
  case E_NOTICE: if (!isset($err)) { $err = 'E_NOTICE'; }
  case E_CORE_WARNING: if (!isset($err)) { $err = 'E_CORE_WARNING'; }
  case E_COMPILE_WARNING: if (!isset($err)) { $err = 'E_COMPILE_WARNING'; }
    $fatal = false;

  // If the error condition is E_USER_ERROR or above then abort

  case E_USER_WARNING: if (!isset($err)) { $err = 'E_USER_WARNING'; }
  case E_USER_NOTICE: if (!isset($err)) { $err = 'E_USER_NOTICE'; }
  case E_USER_ERROR: if (!isset($err)) { $err = 'E_USER_ERROR'; }
  case E_ERROR: if (!isset($err)) { $err = 'E_ERROR'; }
  case E_PARSE: if (!isset($err)) { $err = 'E_PARSE'; }
  case E_CORE_ERROR: if (!isset($err)) { $err = 'E_CORE_ERROR'; }
  case E_COMPILE_ERROR: if (!isset($err)) { $err = 'E_COMPILE_ERROR'; }

    $error_report =
($fatal ? 'ERROR' : 'WARNING') . ' #' . $errno . ' (' . $err . '): ' .
$errstr . ' at line ' . $errline . ' of file ' . $errfile;

    $error_report .=
((is_array($_SERVER) && array_key_exists('REQUEST_URI',$_SERVER))
 ? ('

REQUEST_URI: ' . print_r($_SERVER['REQUEST_URI'],true))
 : '') .
((is_array($_SERVER) && array_key_exists('HTTP_REFERER',$_SERVER))
 ? ('

HTTP_REFERER: ' . print_r($_SERVER['HTTP_REFERER'],true))
 : '') .
((is_array($_SERVER) && array_key_exists('REMOTE_ADDR',$_SERVER))
 ? ('

REMOTE_ADDR: ' . print_r($_SERVER['REMOTE_ADDR'],true))
 : '') .
((is_array($_SERVER) && array_key_exists('HTTP_USER_AGENT',$_SERVER))
 ? ('

HTTP_USER_AGENT: ' . print_r($_SERVER['HTTP_USER_AGENT'],true))
 : '');

    $error_report .= "\n\nCALL STACK:\n";

    $first = true;
    foreach (debug_backtrace() as $call) {
      if ($first) {
        $first = false;
      }
      else {
        $f = $call['function'];
        if (isset($call['type'])) {
          $f = $call['class'] . $call['type'] . $f;
        }

        $a = $call['args'];
        if (is_array($a)) {
          $b = array();
          foreach ($a as $v) {
            if (!isset($v)) { $b[] = 'null'; }
            elseif (is_string($v)) { $b[] = '"' . $v . '"'; }
            elseif (is_int($v)) { $b[] = $v; }
            elseif (is_float($v)) { $b[] = $v; }
            elseif (is_array($v)) { $b[] = 'Array'; }
            else { $b[] = '???'; }
          }
          $a = implode(', ',$b);
        }
        elseif (!isset($a)) {
          $a = '';
        }
        else {
          $a = '?non-array-call-args?';
        }

        $error_report .= '  ' .  $f . ' (' . $a . ')' . "\n";
      }
    }

    if ($error_output == ERROR_OUTPUT_DEBUG) {
      echo ('<pre>' . $error_report . '</pre>');
    }
    elseif ($error_output == ERROR_OUTPUT_SCRIPT) {
      echo $error_report;
    }
    else {
      if ($error_output != ERROR_OUTPUT_EMAIL) {
        $error_report .= '
BAD VALUE: $error_output = ' . $error_output;
      }

      $subject = '[' . SITE_NAME . '] Error';

      mail(ADMIN_EMAIL, $subject, $error_report);

      if ($fatal) {
        $fatal_report =
'<html>
<head><title>' . SITE_NAME . ' Error</title></head>
<body>
<h1>' . SITE_NAME . ' Error</h1>

<p>Sorry for the inconvenience, but an error on the ' . SITE_NAME . '
repo has prevented this web page from being displayed.</p>

<p>Please click the back button to continue browsing.</p>
</body>
</html>';

        echo $fatal_report;
      }
    }

    if ($fatal) {
      exit;
    }

    break;

  default:
    break;
  }
}

///////////////////////////////////////////////////////////////////////////////
// Activate the error handler.
///////////////////////////////////////////////////////////////////////////////

set_error_handler('error_handler');

?>
