<?php

require_once '../opentheory.php';

///////////////////////////////////////////////////////////////////////////////
// Main page.
///////////////////////////////////////////////////////////////////////////////

$title = 'Packages';

$package_table = package_table();

$pkgs = $package_table->list_active_packages();

$num_pkgs = count($pkgs);

$main =
'<h2>All ' .
(($num_pkgs >= 10)
 ? (pretty_number($num_pkgs) . ' ')
 : '') .
'Packages</h2>';

if ($num_pkgs == 0) {
  $main .= '<p>No theory packages have been uploaded to this repo.</p>';
}
else {
  $main .= '<ul>';

  foreach ($pkgs as $pkg) {
    $description = $pkg->description();

    $main .=
'<li>' .
$pkg->link($pkg->name()) .
' &mdash; ' .
string_to_html($description) .
'</li>';
  }

  $main .= '</ul>';
}

$image = site_image('frozen-forest.jpg','Frozen Forest');

output(array('title' => $title), $main, $image);

?>
