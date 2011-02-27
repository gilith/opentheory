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
      $status_info = $upl->status();

      $since_initiated = $upl->since_initiated();

      $initiated_info = $since_initiated->to_string() . ' ago';

      $ret .=
'<li>' .
$upl->link($status_info) .
' &mdash; ' .
string_to_html($initiated_info) .
'</li>';
    }

    $ret .= '</ul>';
  }

  return $ret;
}

///////////////////////////////////////////////////////////////////////////////
// Main page.
///////////////////////////////////////////////////////////////////////////////

$title = 'Admin';

$main =
'<h2>Repo Administration<h2>' .

'<h3>Package Uploads</h3>' .
pretty_recent_uploads(SHORT_RECENT_UPLOADS) .

'<h3>Repo Log</h3>' .
read_log(SHORT_REPO_LOG_LINES);

$image = site_image('cedar-point.jpg','Cedar Point');

output(array('title' => $title), $main, $image);

?>
