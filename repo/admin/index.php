<?php

require_once '../opentheory.php';

///////////////////////////////////////////////////////////////////////////////
// Check for admin privilege.
///////////////////////////////////////////////////////////////////////////////

if (!effective_privilege_is_admin()) { jump_path(array()); }

///////////////////////////////////////////////////////////////////////////////
// Constants.
///////////////////////////////////////////////////////////////////////////////

define('SHORT_RECENT_UPLOADS',3);

define('SHORT_REPO_LOG_LINES',5);

///////////////////////////////////////////////////////////////////////////////
// Pretty-print package profile.
///////////////////////////////////////////////////////////////////////////////

function profile_repo_packages() {
  $installed = count_packages_with_status(INSTALLED_PACKAGE_STATUS);
  $staged = count_packages_with_status(STAGED_PACKAGE_STATUS);

  if ($installed == 0) {
    $text = 'The repo doesn\'t contain any theory packages';

    if ($staged > 0) {
      $text .=
', but there ' .
(($staged == 1) ? 'is' : 'are') .
' ' .
pretty_number($staged) .
' staged';
    }

    $text .= '.';
  }
  else {
    $text =
'The repo contains ' . pretty_number($installed) . ' theory package' .
(($installed == 1) ? '' : 's');

    if ($staged > 0) {
      $text .= ', plus ' . pretty_number($staged) . ' more staged';
    }

    $text .= '.';
  }

  return '<p>' . $text . '</p>';
}

///////////////////////////////////////////////////////////////////////////////
// Pretty-print recent package uploads.
///////////////////////////////////////////////////////////////////////////////

function pretty_recent_uploads($limit) {
  is_int($limit) or trigger_error('bad limit');

  $upload_table = upload_table();

  $upls = $upload_table->list_recent_uploads($limit);

  if (count($upls) == 0) {
    $ret = '<p>No package uploads.</p>';
  }
  else {
    $ret = '<ul>';

    foreach ($upls as $upl) {
      $author = $upl->author_name();

      $author_info =
        isset($author) ? string_to_html($author) : 'Starting';

      $since_initiated = $upl->since_initiated();

      $initiated_info = $since_initiated->to_string() . ' ago';

      $ret .=
'<li>' .
$upl->link($author_info) .
' &mdash; ' .
string_to_html($initiated_info) .
'</li>';
    }

    $ret .= '</ul>';
  }

  return $ret;
}

///////////////////////////////////////////////////////////////////////////////
// Reading the log file.
///////////////////////////////////////////////////////////////////////////////

function read_log($num) {
  is_int($num) or trigger_error('bad num');

  $cmd_num = $num + 1;

  $cmd = 'tail -n ' . $cmd_num . ' ' . REPO_LOG_PATH . ' 2>&1';

  $output = shell_exec($cmd);

  if (isset($output)) { $output = rtrim($output); }
  else { $output = ''; }

  if (strcmp($output,'') == 0) {
    $ret = 'The repo log file is empty.';
  }
  else {
    $lines = explode("\n", $output);

    $munge = (count($lines) == $cmd_num);

    $first = true;

    $ret = '';

    foreach ($lines as $line) {
      if ($first) { $first = false; }
      else { $ret .= '<br />'; }

      if ($munge) { $munge = false; $s = '[...]'; }
      else { $s = $line; }

      $ret .= '<tt>' . string_to_html($s) . '</tt>';
    }
  }

  $ret = '<p>' . $ret . '</p>';

  return $ret;
}

///////////////////////////////////////////////////////////////////////////////
// Main page.
///////////////////////////////////////////////////////////////////////////////

$title = 'Admin';

$main =
'<h2>' . ucfirst(REPO_NAME) . ' Repo Administration</h2>' .

profile_repo_packages() .

'<h3>Package Uploads</h3>' .
pretty_recent_uploads(SHORT_RECENT_UPLOADS) .

'<h3>Repo Log</h3>' .
read_log(SHORT_REPO_LOG_LINES) .

'<h3>Tool version</h3>' .
opentheory_version();

$image = site_image('cedar-point.jpg','Cedar Point');

output(array('title' => $title), $main, $image);

?>
