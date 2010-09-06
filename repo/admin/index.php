<?php

require_once '../opentheory.php';

///////////////////////////////////////////////////////////////////////////////
// Main page.
///////////////////////////////////////////////////////////////////////////////

$cmd = from_string(input('x'));
if (isset($cmd) && strcmp($cmd,'reset') == 0) {
  $cmd = 'rm -r ' . DIRECTORY_PATH;
  shell_exec($cmd);

  $cmd = 'echo "reset package directory" >> ' . LOG_PATH;
  shell_exec($cmd);

  $cmd = 'rm ' . site_path(array('packages','all.pkg'));
  shell_exec($cmd);

  jump_path(bread_crumbs(), null);
}

$title = 'Admin';

$main =
'<ol>
<li>' .

site_link(bread_crumbs(),
          'Reset the package database.',
          array('x' => 'reset')) .
'</li>
</ol>';

$image = site_image('katoomba.jpg','Katoomba Scenic Railway');

output(array('title' => $title), $main, $image);

?>
