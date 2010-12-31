<?php

require_once '../opentheory.php';

///////////////////////////////////////////////////////////////////////////////
// Main page.
///////////////////////////////////////////////////////////////////////////////

$pkg = from_string(input('pkg'));
if (isset($pkg)) { $pkg = from_string_package_name_version(input('pkg')); }
if (isset($pkg)) { $pkg = find_package_by_name_version($pkg); }

if (isset($pkg)) {
  $title = 'Package ' . $pkg->to_string();

  $main =
'<p>Thank you for uploading ' . $pkg . '</p>';

  $image = site_image('tree.jpg','Sunset Tree');

  output(array('title' => $title), $main, $image);
}

$title = 'Packages';

$main =
'<h2>All Packages</h2>';

$package_table = package_table();

$pkgs = $package_table->list_active_packages();

if (count($pkgs) == 0) {
  $main .= '<p>No theory packages have been uploaded to this repo.</p>';
}
else {
  $main .= '<ul>';

  foreach ($pkgs as $pkg) {
    $description = $pkg->description();

    $main .=
'<li>' .
$pkg->site_link($pkg->name()) .
' &mdash; ' .
string_to_html($description) .
'</li>';
  }

  $main .= '</ul>';
}

$image = site_image('tree.jpg','Sunset Tree');

output(array('title' => $title), $main, $image);

?>
