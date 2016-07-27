<?php

///////////////////////////////////////////////////////////////////////////////
//
// OPENTHEORY TOOL INTERFACE
//
// Copyright (c) 2010 Joe Leslie-Hurd, distributed under the MIT license
//
///////////////////////////////////////////////////////////////////////////////

require_once 'global.php';
require_once 'error.php';
require_once 'functions.php';
require_once 'date.php';
require_once 'input.php';
require_once 'links.php';
require_once 'tag.php';

///////////////////////////////////////////////////////////////////////////////
// Write to the opentheory repo log.
///////////////////////////////////////////////////////////////////////////////

function opentheory_log($line) {
  is_string($line) or trigger_error('bad line');

  $cmd = 'echo "' . $line . '" >> ' . REPO_LOG_PATH;

  shell_exec($cmd);
}

///////////////////////////////////////////////////////////////////////////////
// Invoke the opentheory program.
///////////////////////////////////////////////////////////////////////////////

function opentheory_exec($args,&$output) {
  is_string($args) or trigger_error('bad args');

  $cmd = REPO_BIN . ' -d ' . REPO_PATH . ' ' . $args;

  exec($cmd, $output, $status);

  is_array($output) or trigger_error('bad output');
  is_int($status) or trigger_error('bad status');

  return ($status == 0);
}

///////////////////////////////////////////////////////////////////////////////
// Invoke the opentheory program to carry out an action.
///////////////////////////////////////////////////////////////////////////////

function opentheory_action($action,$args,&$error) {
  is_string($action) or trigger_error('bad action');
  is_string($args) or trigger_error('bad args');

  $action_args = $action . $args . ' 2>&1 >> ' . REPO_LOG_PATH;

  $success = opentheory_exec($action_args,$output);

  $error = implode("\n",$output);

  return $success;
}

///////////////////////////////////////////////////////////////////////////////
// Invoke the opentheory program to query information.
///////////////////////////////////////////////////////////////////////////////

function opentheory_query($action,$args) {
  is_string($action) or trigger_error('bad action');
  is_string($args) or trigger_error('bad args');

  $action_args = $action . $args . ' 2>&1';

  if (!opentheory_exec($action_args,$output)) {
    trigger_error('query failed');
  }

  return $output;
}

///////////////////////////////////////////////////////////////////////////////
// Get the version of the opentheory program
///////////////////////////////////////////////////////////////////////////////

function opentheory_version() {
  $args = '--version';

  $output = implode("\n", opentheory_query('',$args));

  return $output;
}

///////////////////////////////////////////////////////////////////////////////
// Initialize the opentheory repo.
///////////////////////////////////////////////////////////////////////////////

function opentheory_init() {
  $args = ' --repo';

  if (!opentheory_action('init',$args,$error)) {
    trigger_error('couldn\'t initialize directory: ' . $error);
  }
}

///////////////////////////////////////////////////////////////////////////////
// Clean up a staged package.
///////////////////////////////////////////////////////////////////////////////

function opentheory_cleanup($name_version) {
  isset($name_version) or trigger_error('bad name_version');

  $args = ' ' . $name_version->staged_to_string();

  if (!opentheory_action('cleanup',$args,$error)) {
    trigger_error('cleanup failed: ' . $error);
  }
}

function opentheory_cleanup_all() {
  if (!opentheory_action('cleanup','',$error)) {
    trigger_error('cleanup failed: ' . $error);
  }
}

///////////////////////////////////////////////////////////////////////////////
// Update the upstream repos.
///////////////////////////////////////////////////////////////////////////////

function opentheory_update() {
  $args = '';

  if (!opentheory_action('update',$args,$error)) {
    trigger_error('couldn\'t update upstream repos: ' . $error);
  }
}

///////////////////////////////////////////////////////////////////////////////
// Stage a package (in tarball form) for installation.
///////////////////////////////////////////////////////////////////////////////

function opentheory_stage($tarball,$name_version,$checksum) {
  is_string($tarball) or trigger_error('bad tarball');
  isset($name_version) or trigger_error('bad name_version');
  !isset($checksum) or is_string($checksum) or trigger_error('bad checksum');

  $args = ' --stage --manual --name ' . $name_version->to_string();

  if (isset($checksum)) { $args .= ' --checksum ' . $checksum; }

  $args .= ' tarball:' . $tarball;

  if (opentheory_action('install',$args,$error)) {
    if (strcmp($error,'') == 0) {
      $error = null;
    }
    else {
      // Uploading packages with warnings is not allowed
      opentheory_cleanup($name_version);
    }
  }
  else {
    isset($error) or trigger_error('silent failure is bad');
  }

  return $error;
}

///////////////////////////////////////////////////////////////////////////////
// Complete the installation of a staged package.
///////////////////////////////////////////////////////////////////////////////

function opentheory_complete($name_version) {
  isset($name_version) or trigger_error('bad name_version');

  $args = ' ' . $name_version->staged_to_string();

  if (!opentheory_action('install',$args,$error)) {
    trigger_error('complete failed: ' . $error);
  }
}

///////////////////////////////////////////////////////////////////////////////
// Install a package with a specified checksum.
///////////////////////////////////////////////////////////////////////////////

function opentheory_install($name_version,$checksum) {
  isset($name_version) or trigger_error('bad name_version');
  is_string($checksum) or trigger_error('bad checksum');

  $args = ' --manual --checksum ' . $checksum;

  $args .= ' ' . $name_version->to_string();

  if (opentheory_action('install',$args,$error)) {
    if (strcmp($error,'') != 0) {
      // Installing packages with warnings is allowed
      $log =
'installing package ' . $name_version->to_string() .
" generated warnings:\n" . $error;
      opentheory_log($log);
    }
    $error = null;
  }
  else {
    isset($error) or trigger_error('silent failure is bad');
  }

  return $error;
}

///////////////////////////////////////////////////////////////////////////////
// Query package information.
///////////////////////////////////////////////////////////////////////////////

function opentheory_query_tags($target) {
  is_string($target) or trigger_error('bad target');

  $args = ' --information ' . $target;

  $lines = opentheory_query('info',$args);

  $tags = array();

  foreach ($lines as $line) {
    $tag = from_string_package_tag($line);

    if (!isset($tag)) { trigger_error('bad tag'); }

    $tags[] = $tag;
  }

  return $tags;
}

function opentheory_tags($name_version) {
  isset($name_version) or trigger_error('bad name_version');

  $target = $name_version->to_string();

  return opentheory_query_tags($target);
}

function opentheory_staged_tags($name_version) {
  isset($name_version) or trigger_error('bad name_version');

  $target = $name_version->staged_to_string();

  return opentheory_query_tags($target);
}

///////////////////////////////////////////////////////////////////////////////
// Query installed packages.
///////////////////////////////////////////////////////////////////////////////

function opentheory_query_name_versions($action,$args) {
  is_string($action) or trigger_error('bad action');
  is_string($args) or trigger_error('bad args');

  $lines = opentheory_query($action,$args);

  $name_versions = array();

  foreach ($lines as $line) {
    $name_version = from_string_package_name_version($line);

    if (!isset($name_version)) {
      trigger_error('bad NAME-VERSION format in ' . $line);
    }

    $name_versions[] = $name_version;
  }

  return $name_versions;
}

function opentheory_list($query) {
  is_string($query) or trigger_error('bad query');

  $args = " --include-order '" . $query . "'";

  $name_versions = opentheory_query_name_versions('list',$args);

  return $name_versions;
}

function opentheory_list_staged() {
  $dir = site_path(array(REPO_DIR,REPO_STAGING_DIR));

  $files = scandir($dir);

  $name_versions = array();

  foreach ($files as $file) {
    if (strcmp($file,".") != 0 && strcmp($file,"..") != 0) {
      $name_version = from_string_package_name_version($file);

      if (!isset($name_version)) {
        trigger_error('bad NAME-VERSION format in ' . $file);
      }

      $name_versions[] = $name_version;
    }
  }

  return $name_versions;
}

///////////////////////////////////////////////////////////////////////////////
// Query package dependencies.
///////////////////////////////////////////////////////////////////////////////

function opentheory_includes($name_version) {
  isset($name_version) or trigger_error('bad name_version');

  $args = ' --includes ' . $name_version->to_string();

  $name_versions = opentheory_query_name_versions('info',$args);

  return $name_versions;
}

function opentheory_staged_includes($name_version) {
  isset($name_version) or trigger_error('bad name_version');

  $args = ' --includes ' . $name_version->staged_to_string();

  $name_versions = opentheory_query_name_versions('info',$args);

  return $name_versions;
}

function opentheory_subtheories($name_version) {
  isset($name_version) or trigger_error('bad name_version');

  $query = 'Subtheories ' . $name_version->to_string();

  return opentheory_list($query);
}

///////////////////////////////////////////////////////////////////////////////
// Query package timestamp.
///////////////////////////////////////////////////////////////////////////////

function opentheory_timestamp($name_version) {
  isset($name_version) or trigger_error('bad name_version');

  $file = site_path($name_version->tarball_path());

  $mod_time = filemtime($file);

  $timestamp = new TimePoint();

  $timestamp->from_datetime($mod_time);

  return $timestamp;
}

function opentheory_staged_timestamp($name_version) {
  isset($name_version) or trigger_error('bad name_version');

  $file = site_path($name_version->staged_tarball_path());

  $mod_time = filemtime($file);

  $timestamp = new TimePoint();

  $timestamp->from_datetime($mod_time);

  return $timestamp;
}

///////////////////////////////////////////////////////////////////////////////
// Query whether a package is empty.
///////////////////////////////////////////////////////////////////////////////

function opentheory_query_empty_theory($target) {
  is_string($target) or trigger_error('bad target');

  $args = ' --format EMPTY ' . $target;

  $lines = opentheory_query('info',$args);

  $output = implode("\n",$lines);

  if (strcmp($output,"T") == 0) {
    $empty_theory = true;
  }
  elseif (strcmp($output,"F") == 0) {
    $empty_theory = false;
  }
  else {
    trigger_error('bad output string');
  }

  return $empty_theory;
}

function opentheory_empty_theory($name_version) {
  isset($name_version) or trigger_error('bad name_version');

  return opentheory_query_empty_theory($name_version->to_string());
}

function opentheory_staged_empty_theory($name_version) {
  isset($name_version) or trigger_error('bad name_version');

  return opentheory_query_empty_theory($name_version->staged_to_string());
}

///////////////////////////////////////////////////////////////////////////////
// Reset the opentheory repo.
///////////////////////////////////////////////////////////////////////////////

function opentheory_reset() {
  $cmd = 'rm -rf ' . REPO_PATH . ' 2>&1';

  $error = shell_exec($cmd);

  if (isset($error)) { trigger_error('deleting error: ' . $error); }

  opentheory_log('deleted package directory');

  opentheory_init();
}

///////////////////////////////////////////////////////////////////////////////
// Tweet about opentheory stuff.
///////////////////////////////////////////////////////////////////////////////

function opentheory_tweet($status) {
  is_string($status) or trigger_error('bad status');

  $cmd = TWITTER_BIN;

  if (isset($cmd)) {
    $cmd .=
' -silent' .
' -status="' . $status . '"' .
' 2>&1';

    $error = shell_exec($cmd);

    if (isset($error)) { trigger_error('tweeting error: ' . $error); }

    opentheory_log('tweeted: "' . $status . '"');
  }
}

?>
