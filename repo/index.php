<?php

require_once 'opentheory.php';

///////////////////////////////////////////////////////////////////////////////
// Constants.
///////////////////////////////////////////////////////////////////////////////

define('SHORT_RECENT_PACKAGE_LIMIT',3);

///////////////////////////////////////////////////////////////////////////////
// Package page.
///////////////////////////////////////////////////////////////////////////////

$pkg = from_string(input('pkg'));
if (isset($pkg)) { $pkg = from_string_package_name_version($pkg); }
if (isset($pkg)) { $pkg = find_package_by_name_version($pkg); }

if (isset($pkg)) {
  set_bread_crumbs_extension(array());

  $author = $pkg->author();

  $uploaded = $pkg->uploaded();

  $children = package_children($pkg);

  $version_info = $pkg->version();

  $author_info = $author->to_string();

  $license_info = $pkg->license();

  $uploaded_info =
    $uploaded->to_string_time() . ' on ' .
    $uploaded->to_verbose_string_date();

  $main =
'<h2>Package ' . $pkg->name() . '</h2>' .
'<p>' . string_to_html($pkg->description()) . '</p>';

  $main .=
'<h3>Information</h3>' .
'<table class="information">' .
'<tr><td>version</td><td>' . string_to_html($version_info) . '</td></tr>' .
'<tr><td>author</td><td>' . string_to_html($author_info) . '</td></tr>' .
'<tr><td>license</td><td>' . string_to_html($license_info) . '</td></tr>' .
'<tr><td>uploaded</td><td>' . string_to_html($uploaded_info) . '</td></tr>' .
'</table>';

  if (count($children) > 0) {
    $main .=
'<h3>Dependencies</h3>' .
'<ul>';

    foreach ($children as $child) {
      $main .=
'<li>' .
$child->link($child->to_string()) .
' &mdash; ' .
string_to_html($child->description()) .
'</li>';
    }

    $main .=
'</ul>';
  }

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

  $image = site_image('sunset-tree.jpg','Sunset Tree');

  output(array('title' => $title), $main, $image);
}

///////////////////////////////////////////////////////////////////////////////
// Upload page.
///////////////////////////////////////////////////////////////////////////////

$upload = from_string(input('upload'));
if (isset($upload)) { $upload = find_upload($upload); }

if (isset($upload)) {
  set_bread_crumbs_extension(array());

  $initiated = $upload->initiated();

  $since_initiated = $upload->since_initiated();

  $status = $upload->status();

  $author = $upload->author();

  $pkgs = packages_upload($upload);

  $initiated_info = $since_initiated->to_string() . ' ago';

  $status_info = pretty_upload_status($status);

  $main =
'<h2>Package Upload</h2>' .
'<h3>Information</h3>' .
'<table class="information">' .
'<tr><td>status</td><td>' . string_to_html($status_info) . '</td></tr>' .
'<tr><td>initiated</td><td>' . string_to_html($initiated_info) . '</td></tr>';

  if (isset($author)) {
    $author_info = $author->to_string();

    $main .=
'<tr><td>author</td><td>' . string_to_html($author_info) . '</td></tr>';
  }

  $main .=
'</table>';

  if (count($pkgs) > 0) {
    $main .=
'<h3>Packages</h3>' .
'<ul>';

    foreach ($pkgs as $pkg) {
      $main .=
'<li>' .
$pkg->link($pkg->to_string()) .
' &mdash; ' .
string_to_html($pkg->description()) .
'</li>';
    }

    $main .=
'</ul>';
  }

  $title = 'Package Upload';

  $image = site_image('elephant-and-castle.jpg','Elephant and Castle');

  output(array('title' => $title), $main, $image);
}

///////////////////////////////////////////////////////////////////////////////
// Main page.
///////////////////////////////////////////////////////////////////////////////

$num_pkgs = count_active_packages();

$main =
'<p>Welcome to the ' . ucfirst(REPO_NAME) . ' OpenTheory repo, which
is currently storing ' .

pretty_number($num_pkgs) . ' theory package' . (($num_pkgs == 1) ? '' : 's') .

'. Each theory package contains a collection of theorems together with
their proofs. The proofs have been broken down into the primitive
inferences of higher order logic, allowing them to be checked by
computer.</p>' .

'<p>This web interface is provided to help browse through the ' .

site_link(array('packages'),'available packages') .

', but the recommended way of downloading and processing theory
packages is to use the

<a href="http://www.gilith.com/software/opentheory/">opentheory</a>

package management tool. For more information on OpenTheory please
refer to the

<a href="http://www.gilith.com/research/opentheory/">project homepage</a>.</p>' .

'<h2>Recently Uploaded Packages <span class="more">[' .

site_link(array('recent'),'more') .

']</span></h2>' .

pretty_recent_packages(SHORT_RECENT_PACKAGE_LIMIT);

$image =
  array(site_image('silver-falls.jpg','Silver Falls'),
        ('<div id="twitter-wrapper"><div id="twitter"><p><a href="http://twitter.com/OpenTheory">OpenTheory twitter feed:</a></p><ul id="twitter_update_list"><li>Loading...</li></ul></div></div>' .
         '<script src="http://twitter.com/javascripts/blogger.js" type="text/javascript"></script><script src="http://twitter.com/statuses/user_timeline/OpenTheory.json?callback=twitterCallback2&amp;count=4" type="text/javascript"></script>'));

output(array(), $main, $image);

?>
