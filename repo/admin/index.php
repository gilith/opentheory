<?php

require_once '../opentheory.php';

///////////////////////////////////////////////////////////////////////////////
// Main page.
///////////////////////////////////////////////////////////////////////////////

$cmd = from_string(input('x'));

if (isset($cmd) && strcmp($cmd,'reset') == 0) {
  opentheory_reset();

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

$main = '<p>Administrative interface.</p>';

$image = site_image('katoomba.jpg','Katoomba Scenic Railway');

output(array('title' => $title), $main, $image);

?>
