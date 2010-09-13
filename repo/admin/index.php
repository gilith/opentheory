<?php

require_once '../opentheory.php';

///////////////////////////////////////////////////////////////////////////////
// Main page.
///////////////////////////////////////////////////////////////////////////////

$cmd = from_string(input('x'));
if (isset($cmd) && strcmp($cmd,'reset') == 0) {
  $cmd = 'rm -r ' . DIRECTORY_PATH;
  shell_exec($cmd);

  opentheory_init();

  $cmd = 'echo "reset package directory" >> ' . LOG_PATH;
  shell_exec($cmd);

  if (is_script()) {
    output_script('successfully reset the package directory');
  }
  else {
    jump_path(bread_crumbs(), null);
  }
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
