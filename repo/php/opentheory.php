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
  $args = '';

  $output = opentheory_action('init',$args);

  if (isset($output)) {
    trigger_error('couldn\'t initialize directory: ' . $output);
  }
}

///////////////////////////////////////////////////////////////////////////////
// Install a package from a tarball.
///////////////////////////////////////////////////////////////////////////////

function opentheory_install($tarball,$name_version,$checksum) {
  is_string($tarball) or trigger_error('bad tarball');
  isset($name_version) or trigger_error('bad name_version');
  !isset($checksum) or is_string($checksum) or trigger_error('bad checksum');

  $args = ' --minimal';

  $args .= ' --name ' . $name_version->to_string();

  if (isset($checksum)) { $args .= ' --checksum ' . $checksum; }

  $args .= ' tarball:' . $tarball;

  $output = opentheory_action('install',$args);

  return $output;
}

///////////////////////////////////////////////////////////////////////////////
// Query package information.
///////////////////////////////////////////////////////////////////////////////

function opentheory_tags($name_version) {
  isset($name_version) or trigger_error('bad name_version');

  $args = ' --information ' . $name_version->to_string();

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

///////////////////////////////////////////////////////////////////////////////
// Query package children.
///////////////////////////////////////////////////////////////////////////////

function opentheory_children($name_version) {
  isset($name_version) or trigger_error('bad name_version');

  $args = ' --dependencies ' . $name_version->to_string();

  $output = opentheory_query('info',$args);

  $children = array();

  if (strcmp($output,'') != 0) {
    $lines = explode("\n", $output);

    foreach ($lines as $line) {
      $child = from_string_package_name_version($line);

      if (!isset($child)) { trigger_error('bad child'); }

      $children[] = $child;
    }
  }

  return $children;
}

///////////////////////////////////////////////////////////////////////////////
// Query package list.
///////////////////////////////////////////////////////////////////////////////

function opentheory_list() {
  $args = ' --name';

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

  opentheory_init();
}

?>
