<?php

require_once '../opentheory.php';

///////////////////////////////////////////////////////////////////////////////
// Helper functions.
///////////////////////////////////////////////////////////////////////////////

function render_package($line) {
  if (ereg('^([^ ]+) (.*)$',$line,$arr)) {
    $name = $arr[1];
    $description = $arr[2];

    return
'<tr><td>' .
site_link(package_path($name),$name) .
'</td><td>' .
$description .
'</td></tr>';
  }
  else {
    trigger_error('bad line in all.pkg: ' . $line);
  }
}

///////////////////////////////////////////////////////////////////////////////
// Main page.
///////////////////////////////////////////////////////////////////////////////

$title = 'Packages';

$main =
'<h2>All Packages</h2>';

$data = file_get_contents(site_path(array('packages','all.pkg')));

$lines = explode("\n",$data);

$pkgs = array();

foreach ($lines as $line) {
  if (strcmp($line,'') != 0) {
    $pkgs[] = render_package($line);
  }
}

if (count($pkgs) == 0) {
  $main .= '<p>No theory packages uploaded to this repo.</p>';
}
else {
  $main .=
'<table>' .
implode($pkgs) .
'</table>';
}

$image = site_image('tree.jpg','Sunset Tree');

output(array('title' => $title), $main, $image);

?>
