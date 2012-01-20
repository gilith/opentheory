<?php

///////////////////////////////////////////////////////////////////////////////
//
// OPENTHEORY TOOL INTERFACE
//
// Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2
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
// Invoke the opentheory program to carry out an action.
///////////////////////////////////////////////////////////////////////////////

function opentheory_action($action,$args) {
  is_string($action) or trigger_error('bad action');
  is_string($args) or trigger_error('bad args');

  $cmd =
REPO_BIN .
' -d ' . REPO_PATH . ' ' .
$action . $args .
' 2>&1 >> ' . REPO_LOG_PATH;

  $output = shell_exec($cmd);

  if (isset($output)) { $output = rtrim($output); }

  return $output;
}

///////////////////////////////////////////////////////////////////////////////
// Invoke the opentheory program to query information.
///////////////////////////////////////////////////////////////////////////////

function opentheory_query($action,$args) {
  is_string($action) or trigger_error('bad action');
  is_string($args) or trigger_error('bad args');

  $cmd =
REPO_BIN .
' -d ' . REPO_PATH . ' ' .
$action . $args .
' 2>&1';

  $output = shell_exec($cmd);

  if (isset($output)) { $output = rtrim($output); }
  else { $output = ''; }

  return $output;
}

///////////////////////////////////////////////////////////////////////////////
// Initialize the opentheory repo.
///////////////////////////////////////////////////////////////////////////////

function opentheory_init() {
  $args = ' --repo';

  $output = opentheory_action('init',$args);

  if (isset($output)) {
    trigger_error('couldn\'t initialize directory: ' . $output);
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

  $output = opentheory_action('install',$args);

  return $output;
}

///////////////////////////////////////////////////////////////////////////////
// Complete the installation of a staged package.
///////////////////////////////////////////////////////////////////////////////

function opentheory_complete($name_version) {
  isset($name_version) or trigger_error('bad name_version');

  $args = ' ' . $name_version->staged_to_string();

  $error = opentheory_action('install',$args);

  if (isset($error)) { trigger_error('complete failed: ' . $error); }
}

///////////////////////////////////////////////////////////////////////////////
// Clean up a staged package.
///////////////////////////////////////////////////////////////////////////////

function opentheory_cleanup($name_version) {
  isset($name_version) or trigger_error('bad name_version');

  $args = ' ' . $name_version->to_string();

  $error = opentheory_action('cleanup',$args);

  if (isset($error)) { trigger_error('cleanup failed: ' . $error); }
}

///////////////////////////////////////////////////////////////////////////////
// Install a package with a specified checksum.
///////////////////////////////////////////////////////////////////////////////

function opentheory_install($name_version,$checksum) {
  isset($name_version) or trigger_error('bad name_version');
  is_string($checksum) or trigger_error('bad checksum');

  $args = ' --manual --checksum ' . $checksum;

  $args .= ' ' . $name_version->to_string();

  $output = opentheory_action('install',$args);

  return $output;
}

///////////////////////////////////////////////////////////////////////////////
// Query package information.
///////////////////////////////////////////////////////////////////////////////

function opentheory_parse_tags($target) {
  is_string($target) or trigger_error('bad target');

  $args = ' --information ' . $target;

  $output = opentheory_query('info',$args);

  $tags = array();

  if (strcmp($output,'') != 0) {
    $lines = explode("\n", $output);

    foreach ($lines as $line) {
      $tag = from_string_package_tag($line);

      if (!isset($tag)) { trigger_error('bad tag'); }

      $tags[] = $tag;
    }
  }

  return $tags;
}

function opentheory_tags($name_version) {
  isset($name_version) or trigger_error('bad name_version');

  $target = $name_version->to_string();

  return opentheory_parse_tags($target);
}

function opentheory_staged_tags($name_version) {
  isset($name_version) or trigger_error('bad name_version');

  $target = $name_version->staged_to_string();

  return opentheory_parse_tags($target);
}

///////////////////////////////////////////////////////////////////////////////
// Query package parents.
///////////////////////////////////////////////////////////////////////////////

function opentheory_parse_parents($target) {
  is_string($target) or trigger_error('bad target');

  $args = " 'Includes " . $target . "'";

  $output = opentheory_query('list',$args);

  $parents = array();

  if (strcmp($output,'') != 0) {
    $lines = explode("\n", $output);

    foreach ($lines as $line) {
      $parent = from_string_package_name_version($line);

      if (!isset($parent)) { trigger_error('bad parent'); }

      $parents[] = $parent;
    }
  }

  return $parents;
}

function opentheory_parents($name_version) {
  isset($name_version) or trigger_error('bad name_version');

  $target = $name_version->to_string();

  return opentheory_parse_parents($target);
}

function opentheory_staged_parents($name_version) {
  isset($name_version) or trigger_error('bad name_version');

  $target = $name_version->staged_to_string();

  return opentheory_parse_parents($target);
}

///////////////////////////////////////////////////////////////////////////////
// Query package timestamp.
///////////////////////////////////////////////////////////////////////////////

function opentheory_timestamp($name_version) {
  isset($name_version) or trigger_error('bad name_version');

  $file = site_path($name_version->theory_file_path());

  $mod_time = filemtime($file);

  $timestamp = new TimePoint();

  $timestamp->from_datetime($mod_time);

  return $timestamp;
}

///////////////////////////////////////////////////////////////////////////////
// Query package list.
///////////////////////////////////////////////////////////////////////////////

function opentheory_list() {
  $args = ' --include-order All';

  $output = opentheory_query('list',$args);

  $name_versions = array();

  if (strcmp($output,'') != 0) {
    $lines = explode("\n", $output);

    foreach ($lines as $line) {
      $name_version = from_string_package_name_version($line);

      if (!isset($name_version)) {
        trigger_error('bad name_version in ' . $line);
      }

      $name_versions[] = $name_version;
    }
  }

  return $name_versions;
}

///////////////////////////////////////////////////////////////////////////////
// Reset the opentheory repo.
///////////////////////////////////////////////////////////////////////////////

function opentheory_reset() {
  $cmd = 'rm -rf ' . REPO_PATH;

  shell_exec($cmd);

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
