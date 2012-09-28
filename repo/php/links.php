<?php

///////////////////////////////////////////////////////////////////////////////
//
// GENERATING LINKS
//
// Copyright (c) 2009 Joe Leslie-Hurd, distributed under the MIT license
//
///////////////////////////////////////////////////////////////////////////////

require_once 'global.php';
require_once 'error.php';
require_once 'functions.php';

///////////////////////////////////////////////////////////////////////////////
// Site email.
///////////////////////////////////////////////////////////////////////////////

function site_email($address, $subject, $body) {
  is_string($address) or trigger_error('bad address');
  is_string($subject) or trigger_error('bad subject');
  is_string($body) or trigger_error('bad body');

  $headers = 'From: ' . ADMIN_NAME . ' <' . ADMIN_EMAIL . '>';

  $subject = '[' . SITE_NAME . '] ' . $subject;

  mail($address,$subject,$body,$headers) or
    trigger_error('couldn\'t send email');
}

///////////////////////////////////////////////////////////////////////////////
// Bread crumbs.
///////////////////////////////////////////////////////////////////////////////

$global_bread_crumbs = null;

function bread_crumbs() {
  global $global_bread_crumbs;

  if (!isset($global_bread_crumbs)) {
    $path = $_SERVER['PHP_SELF'];

    $path = ereg_replace('/[^/]+$','/',$path);
    $path = ereg_replace('^/','',$path);
    $path = ereg_replace('/$','',$path);

    if (strcmp($path,'') == 0) {
      $global_bread_crumbs = array();
    }
    else {
      $global_bread_crumbs = explode('/', $path);
    }
  }

  return $global_bread_crumbs;
}

///////////////////////////////////////////////////////////////////////////////
// The bread crumbs extension handles script arguments.
///////////////////////////////////////////////////////////////////////////////

$global_bread_crumbs_extension = null;

function bread_crumbs_extension() {
  global $global_bread_crumbs_extension;

  return $global_bread_crumbs_extension;
}

function set_bread_crumbs_extension($e) {
  global $global_bread_crumbs_extension;

  !isset($e) or is_array($e) or trigger_error('bad e');

  $global_bread_crumbs_extension = $e;
}

///////////////////////////////////////////////////////////////////////////////
// URL arguments.
///////////////////////////////////////////////////////////////////////////////

function url_arguments($args) {
  is_array($args) or trigger_error('bad args');

  $l = array();

  $j = null;

  foreach ($args as $arg => $val) {
    is_string($arg) or trigger_error('bad arg');

    if (isset($val)) {
      if (strcmp($arg,'') == 0) {
        $j = $val;
      }
      elseif (is_array($val)) {
        foreach ($val as $v) {
          $l[] = string_to_url($arg . '[]') . '=' . string_to_url($v);
        }
      }
      else {
        $l[] = string_to_url($arg) . '=' . string_to_url($val);
      }
    }
  }

  return
    ((count($l) == 0) ? '' : ('?' . implode('&',$l))) .
    (isset($j) ? ('#' . string_to_url($j)) : '');
}

///////////////////////////////////////////////////////////////////////////////
// Site URLs.
///////////////////////////////////////////////////////////////////////////////

function external_site_url($path, $args = null) {
  is_array($path) or trigger_error('bad path');
  if (!isset($args)) { $args = array(); }
  is_array($args) or trigger_error('bad args');

  $url = SITE_URL;

  foreach ($path as $dir) {
    $url .= $dir . '/';
  }

  $url .= url_arguments($args);

  return $url;
}

///////////////////////////////////////////////////////////////////////////////
// Site paths.
///////////////////////////////////////////////////////////////////////////////

function site_path($target_path) {
  is_array($target_path) or trigger_error('bad target_path');

  $target_depth = count($target_path);

  $bread_crumbs = bread_crumbs();
  $current_depth = count($bread_crumbs);

  $common_depth = 0;
  $finished = false;
  while (!$finished) {
    if ($common_depth < $current_depth &&
        $common_depth < $target_depth &&
        strcmp($bread_crumbs[$common_depth],$target_path[$common_depth]) == 0) {
      ++$common_depth;
    }
    else {
      $finished = true;
    }
  }

  $ret = array();

  for ($i = $common_depth; $i < $current_depth; ++$i) {
    $ret[] = '..';
  }

  for ($i = $common_depth; $i < $target_depth; ++$i) {
    $ret[] = $target_path[$i];
  }

  if (count($ret) == 0) {
    return '.';
  }
  else {
    return implode('/',$ret);
  }
}

///////////////////////////////////////////////////////////////////////////////
// Site URLs.
///////////////////////////////////////////////////////////////////////////////

function site_url($path, $args = null) {
  is_array($path) or trigger_error('bad path');
  if (!isset($args)) { $args = array(); }
  is_array($args) or trigger_error('bad args');

  $path_text = site_path($path);

  $args_text = url_arguments($args);

  if (strcmp($path_text,'.') == 0) {
    if (strcmp($args_text,'') == 0) {
      return '.';
    }
    else {
      return $args_text;
    }
  }
  else {
    if (strcmp($args_text,'') == 0) {
      return $path_text;
    }
    else {
      return $path_text . '/' . $args_text;
    }
  }
}

///////////////////////////////////////////////////////////////////////////////
// Site images.
///////////////////////////////////////////////////////////////////////////////

function site_image_path($image) {
  is_string($image) or trigger_error('bad image');

  return array(IMAGE_DIR,$image);
}

function site_image($image, $alt, $atts = null) {
  is_string($image) or trigger_error('bad image');
  is_string($alt) or trigger_error('bad alt');

  $url = site_url(array(IMAGE_DIR,$image),null);

  $atts_text = attributes_to_html($atts);

  return
    '<img src="' . string_to_html($url) . '"' .
    ' alt="' . string_to_html($alt) . '"' .
    $atts_text . ' />';
}

///////////////////////////////////////////////////////////////////////////////
// Site links.
///////////////////////////////////////////////////////////////////////////////

function site_link($path, $text, $args = null, $atts = null) {
  is_array($path) or trigger_error('bad path');
  is_string($text) or trigger_error('bad text');

  $url = site_url($path,$args);

  $atts_text = attributes_to_html($atts);

  return
    '<a href="' . string_to_html($url) . '"' . $atts_text . '>' .
    $text . '</a>';
}

///////////////////////////////////////////////////////////////////////////////
// Jumping.
///////////////////////////////////////////////////////////////////////////////

function jump_url($url) {
  is_string($url) or trigger_error('bad url');
  header('Location: ' . $url);
  exit;
}

function jump_path($path, $args = null) {
  is_array($path) or trigger_error('bad path');
  $url = site_url($path,$args);
  jump_url($url);
}

?>
