<?php

require_once '../opentheory.php';

///////////////////////////////////////////////////////////////////////////////
// Main page.
///////////////////////////////////////////////////////////////////////////////

$pkg = from_string(input('pkg'));
if (isset($pkg)) { $pkg = from_string_package_name_version(input('pkg')); }
if (isset($pkg)) { $pkg = find_package_by_name_version($pkg); }

if (isset($pkg)) {
  $version_info = $pkg->version();

  $author = $pkg->author();

  $author_info = $author->to_string();

  $license_info = $pkg->license();

  $uploaded = $pkg->uploaded();

  $uploaded_info =
    $uploaded->to_string_time() . ' on ' .
    $uploaded->to_verbose_string_date();

  $main =
'<h2>Package ' . $pkg->name() . '</h2>' .
'<p>' . string_to_html($pkg->description()) . '</p>';

  $main .=
'<h3>Information</h3>' .
'<table class="package">' .
'<tr><td>version</td><td>' . string_to_html($version_info) . '</td></tr>' .
'<tr><td>author</td><td>' . string_to_html($author_info) . '</td></tr>' .
'<tr><td>license</td><td>' . string_to_html($license_info) . '</td></tr>' .
'<tr><td>uploaded</td><td>' . string_to_html($uploaded_info) . '</td></tr>' .
'</table>';

  $main .=
'<h3>Dependencies</h3>';

  $main .=
'<h3>Files</h3>' .
'<ul>' .
'<li>Package summary ' .
$pkg->summary_file_link($pkg->summary_file_name()) .
'</li>' .
'<li>Package tarball ' .
$pkg->tarball_link($pkg->tarball_name()) .
'</li>' .
'<li>Theory file ' .
$pkg->theory_file_link($pkg->theory_file_name()) .
' (included in the package tarball)</li>' .
'</ul>';

  $title = 'Package ' . $pkg->to_string();

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
$pkg->link($pkg->name()) .
' &mdash; ' .
string_to_html($description) .
'</li>';
  }

  $main .= '</ul>';
}

$image = site_image('tree.jpg','Sunset Tree');

output(array('title' => $title), $main, $image);

?>
