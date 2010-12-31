<?php

require_once '../opentheory.php';

///////////////////////////////////////////////////////////////////////////////
// Main page.
///////////////////////////////////////////////////////////////////////////////

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
    $name_version = $pkg->name_version();

    $description = $pkg->description();

    $main .=
'<li>' .
site_link(package_document_path($name_version), $name_version->name()) .
' &mdash; ' .
string_to_html($description) .
'</li>';
  }

  $main .= '</ul>';
}

$image = site_image('tree.jpg','Sunset Tree');

output(array('title' => $title), $main, $image);

?>
