<?php

require_once '../opentheory.php';

///////////////////////////////////////////////////////////////////////////////
// Check for admin privilege.
///////////////////////////////////////////////////////////////////////////////

if (!effective_privilege_is_admin()) { jump_path(array()); }

///////////////////////////////////////////////////////////////////////////////
// Reading the log file.
///////////////////////////////////////////////////////////////////////////////

define('REPO_LOG_LINES',10);

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
'<h2>Repo Administration<h2>' .

'<h3>Repo Log</h3>' .
read_log(REPO_LOG_LINES);

$image = site_image('katoomba.jpg','Katoomba Scenic Railway');

output(array('title' => $title), $main, $image);

?>
