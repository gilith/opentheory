<?php

///////////////////////////////////////////////////////////////////////////////
//
// OPENTHEORY OUTPUT
//
// Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2
//
///////////////////////////////////////////////////////////////////////////////

require_once 'global.php';
require_once 'error.php';
require_once 'links.php';

///////////////////////////////////////////////////////////////////////////////
// Repo name.
///////////////////////////////////////////////////////////////////////////////

function repo_name() {
  $name = REPO_NAME . ' OpenTheory Repo';

  return $name;
}

///////////////////////////////////////////////////////////////////////////////
// Site map.
///////////////////////////////////////////////////////////////////////////////

$global_site_map = null;

function site_map() {
  global $global_site_map;

  if (!isset($global_site_map)) {
    $bread_crumbs = bread_crumbs();

    $global_site_map = array();
    $global_site_map['packages'] = null;
    $global_site_map['recent'] = null;
    $global_site_map['upload'] = null;
  }

  return $global_site_map;
}

///////////////////////////////////////////////////////////////////////////////
// Navigation.
///////////////////////////////////////////////////////////////////////////////

function navigation_text($path,$key,$bold,$link) {
  is_array($path) or trigger_error('bad path');
  is_string($key) or trigger_error('bad key');
  is_bool($bold) or trigger_error('bad bold');
  is_bool($link) or trigger_error('bad link');

  $text = $key;

  if ($bold) { $text = '<b>' . $text . '</b>'; }

  if ($link) {
    $path_key = array_merge($path,array($key));
    $text = site_link($path_key,$text);
  }

  return $text;
}

function navigation_item($next,$text) {
  is_bool($next) or trigger_error('bad next');
  is_string($text) or trigger_error('bad text');

  if ($next) { $item = '</p><p>'; }
  else { $item = ' &nbsp; &bull; &nbsp; '; }

  $item .= $text;

  return $item;
}

function navigation() {
  $site_map = site_map();
  $bread_crumbs = bread_crumbs();
  $extension = bread_crumbs_extension();

  $num_bread_crumbs = count($bread_crumbs);

  $extended_bread_crumbs =
    (isset($extension)
     ? array_merge($bread_crumbs,$extension)
     : $bread_crumbs);

  $extended_bread_crumbs[] = null;

  $nav = '<p>';

  $map = $site_map;
  $path = array();
  $depth = 0;

  foreach ($extended_bread_crumbs as $bread_crumb) {
    $next = true;
    $found = null;

    if (isset($map)) {
      foreach ($map as $key => $submap) {
        if ($depth < $num_bread_crumbs && strcmp($bread_crumb,$key) == 0) {
          $found = (isset($submap) ? $submap : array());
          $link = ($depth != $num_bread_crumbs - 1 || isset($extension));
          $text = navigation_text($path,$key,true,$link);
          $nav .= navigation_item($next,$text);
          $next = false;
        }
        else {
          $text = navigation_text($path,$key,false,true);
          $nav .= navigation_item($next,$text);
          $next = false;
        }
      }
    }

    if (isset($bread_crumb) && !isset($found)) {
      if ($depth < $num_bread_crumbs) {
        $link = ($depth != $num_bread_crumbs - 1 || isset($extension));
        $text = navigation_text($path,$bread_crumb,true,$link);
      }
      else {
        $text = $bread_crumb;
      }
      $nav .= navigation_item($next,$text);
      $next = false;
    }

    $map = $found;
    if ($depth < $num_bread_crumbs) { $path[] = $bread_crumb; }
    ++$depth;
  }

  $nav .= '</p>';

  $nav = ereg_replace('^<p></p>','',$nav);

  $text = repo_name();
  if ($num_bread_crumbs > 0 || isset($extension)) {
    $text = site_link(array(), $text);
  }
  $nav = '<h1>' . $text . '</h1>' . $nav;

  return $nav;
}

///////////////////////////////////////////////////////////////////////////////
// Repo admin.
///////////////////////////////////////////////////////////////////////////////

function repo_admin() {
  $admin = REPO_ADMIN;

  $admin_url = REPO_ADMIN_URL;
  if (isset($admin_url)) {
    $admin = '<a href="' . string_to_html($admin_url) . '">' . $admin . '</a>';
  }

  return $admin;
}

///////////////////////////////////////////////////////////////////////////////
// The complete page.
///////////////////////////////////////////////////////////////////////////////

function output($head, $main, $image) {
  $mobile = is_mobile();

  if (!isset($head)) { $head = array(); }

  if (array_key_exists('title',$head)) {
    $title = repo_name() . ' - ' . $head['title'];
  }
  else {
    $title = repo_name();
  }

  if (array_key_exists('favicon',$head)) { $favicon = $head['favicon']; }
  else { $favicon = site_path(array('favicon.ico')); }

  $page =
'<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
<head>
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
<title>' . $title . '</title>' .
($mobile
 ? ''
 : ('<link rel="stylesheet" type="text/css" href="' .
    site_path(array('opentheory.css')) . '" />' .
    '<script type="text/javascript" src="' .
    site_path(array('opentheory.js')) . '"></script>')) .
'<link rel="shortcut icon" type="image/x-icon" href="' . $favicon . '" />
</head>
<body>
<div id="document">
<div id="header">' .
navigation() .
'</div>
<div id="main">' .
(isset($main)
 ? ('<div id="content">' .
    ((isset($image) && !$mobile)
     ? ('<div id="image-wrapper"><div id="image">' . $image . '</div></div>')
     : '') .
    $main . '</div>')
 : '') .
'<div id="main-clearer"></div>' .
'</div>' .
($mobile
 ? ''
 : ('<div id="footer">' .
    '<div id="footer-validator">' .
    '<a href="http://validator.w3.org/check?uri=referer">' .
    site_image('valid.png','Valid XHTML 1.0') .
    '</a>' .
    '</div>' .
    site_image('favicon.png','OpenTheory') .
    ' &nbsp; ' . repo_name() . ', maintained by ' .
    repo_admin() .
    '.</div>')) .
'</div>
</body>
</html>';

  print $page;

  exit;
}

?>