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
// Site map.
///////////////////////////////////////////////////////////////////////////////

$global_site_map = null;

function site_map() {
  global $global_site_map;

  if (!isset($global_site_map)) {
    $bread_crumbs = bread_crumbs();

    $global_site_map = array();
    $global_site_map['software'] = null;
    $global_site_map['research'] =
      array('opentheory' => null,
            'papers' => null,
            'talks' => null);
    $global_site_map['chess'] =
      array('endgames' => null,
            'coaching' => null,
            'diagrams' => null);
    $global_site_map['go'] = null;
    $global_site_map['about'] = null;
  }

  return $global_site_map;
}

///////////////////////////////////////////////////////////////////////////////
// The navigation bar.
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

function navigation_item($home,$next,$text) {
  is_bool($home) or trigger_error('bad home');
  is_bool($next) or trigger_error('bad next');
  is_string($text) or trigger_error('bad text');

  $item = '<li';

  if ($home) { $item .= ' id="home"'; }
  elseif ($next) { $item .= ' class="next"'; }

  $item .= '>' . $text . '</li>';

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

  $nav = '<ul>';

  $map = $site_map;
  $path = array();
  $depth = 0;

  $text = site_link($path,'<b>opentheory</b>');
  $nav .= navigation_item(true,false,$text);

  foreach ($extended_bread_crumbs as $bread_crumb) {
    $next = true;
    $found = null;

    if (isset($map)) {
      foreach ($map as $key => $submap) {
        if ($depth < $num_bread_crumbs && strcmp($bread_crumb,$key) == 0) {
          $found = (isset($submap) ? $submap : array());
          $link = ($depth != $num_bread_crumbs - 1 || isset($extension));
          $text = navigation_text($path,$key,true,$link);
          $nav .= navigation_item(false,$next,$text);
          $next = false;
        }
        else {
          $text = navigation_text($path,$key,false,true);
          $nav .= navigation_item(false,$next,$text);
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
      $nav .= navigation_item(false,$next,$text);
      $next = false;
    }

    $map = $found;
    if ($depth < $num_bread_crumbs) { $path[] = $bread_crumb; }
    ++$depth;
  }

  $nav .= '</ul>';

  return $nav;
}

///////////////////////////////////////////////////////////////////////////////
// The complete page.
///////////////////////////////////////////////////////////////////////////////

function output($head, $header, $main, $image, $footer) {
  $mobile = is_mobile();

  if (!isset($head)) { $head = array(); }

  if (array_key_exists('title',$head)) { $title = $head['title']; }
  else { $title = REPO_NAME . ' OpenTheory Repo'; }

  if (array_key_exists('favicon',$head)) { $favicon = $head['favicon']; }
  else { $favicon = site_path(array('favicon.ico')); }

  if (!isset($footer)) {
    $footer = 'Copyright &copy; 2006&ndash;' . date('Y') . ' Joe Hurd';
  }

  $page =
'<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
<head>
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
<title>' . $title . '</title>
<link rel="stylesheet" type="text/css" href="' .
($mobile
 ? site_path(array('opentheory-mobile.css'))
 : site_path(array('opentheory.css'))) .
'" />
<script type="text/javascript" src="' .
site_path(array('opentheory.js')) . '"></script>
<link rel="shortcut icon" type="image/x-icon" href="' . $favicon . '" />
</head>
<body>
<div id="document">' .
(isset($header)
 ? ('<div id="header">' . $header . '</div>')
 : '') .
'<div id="main">' .
'<div id="navigation-wrapper"><div id="navigation">' . navigation() .
'</div></div>' .
(isset($main)
 ? ('<div id="content">' .
    ((isset($image) && !$mobile)
     ? ('<div id="image-wrapper"><div id="image">' . $image . '</div></div>')
     : '') .
    $main . '</div>')
 : '') .
'<div id="main-clearer"></div>' .
'</div>' .
'<div id="footer">' .
($mobile
 ? ''
 : ('<div id="footer-validator">' .
    '<a href="http://validator.w3.org/check?uri=referer">' .
    site_image('valid.png','Valid XHTML 1.0') .
    '</a>' .
    '</div>')) .
site_link(array('account'), site_image('favicon.png','OpenTheory')) .
' &nbsp; ' . $footer . '</div>' .
'</div>' .
'</body>
</html>';

  print $page;

  exit;
}

?>
