<?php

require_once '../opentheory.php';

///////////////////////////////////////////////////////////////////////////////
// Constants.
///////////////////////////////////////////////////////////////////////////////

define('RECENT_PACKAGE_LIMIT',20);

///////////////////////////////////////////////////////////////////////////////
// Main page.
///////////////////////////////////////////////////////////////////////////////

$title = 'Recent Uploads';

$main =
'<h2>Recently Uploaded Packages</h2>';

$package_table = package_table();

$pkgs = $package_table->list_recent_packages(RECENT_PACKAGE_LIMIT);

if (count($pkgs) == 0) {
  $main .= '<p>No theory packages have been uploaded to this repo.</p>';
}
else {
  foreach ($pkgs as $pkg) {
    $description = $pkg->description();

    $since_uploaded = $pkg->since_uploaded();

    $author_name = $pkg->author_name();

    $main .=
'<p class="recent">' .
$pkg->link($pkg->to_string()) .
' &mdash; ' .
string_to_html($description) .
'<br /><small>' .
'Uploaded ' . $since_uploaded->to_string() . ' ago by ' . $author_name .
'</small></p>';
  }
}

$image = site_image('katoomba.jpg','Katoomba Scenic Railway');

output(array('title' => $title), $main, $image);

?>
