<?php

require_once 'opentheory.php';

///////////////////////////////////////////////////////////////////////////////
// Constants.
///////////////////////////////////////////////////////////////////////////////

define('SHORT_RECENT_PACKAGE_LIMIT',3);

define('CHILD_PACKAGE_LIMIT',5);

define('TWEET_LIMIT',3);

///////////////////////////////////////////////////////////////////////////////
// Pretty package information.
///////////////////////////////////////////////////////////////////////////////

function pretty_package_information($pkg) {
  isset($pkg) or trigger_error('bad pkg');

  $author = $pkg->author();

  $registered = $pkg->registered();

  $includes = package_includes($pkg);

  $num_included_by = count_package_included_by($pkg);

  $version_info = pretty_list_package_versions($pkg->name_version());

  $description_info = string_to_html($pkg->description());

  $author_info = string_to_html($author->to_string());

  $license_info = string_to_html($pkg->license());

  $registered_key = ($pkg->is_installed() ? 'installed' : 'uploaded');

  $registered_info =
    $registered->to_string_time() . ' on ' .
    $registered->to_verbose_string_date();

  $registered_info = string_to_html($registered_info);

  $main =
'<h3>Information</h3>' .
'<table class="information">' .
'<tr><td>versions</td><td>' . $version_info . '</td></tr>' .
'<tr><td>description</td><td>' . $description_info . '</td></tr>' .
'<tr><td>author</td><td>' . $author_info . '</td></tr>' .
'<tr><td>license</td><td>' . $license_info . '</td></tr>' .
'<tr><td>' . string_to_html($registered_key) . '</td><td>' .
$registered_info . '</td></tr>' .
'<tr><td>theory file</td><td>' .
$pkg->summary_file_link($pkg->summary_file_name()) .
'</td></tr>' .
'<tr><td>tarball</td><td>' .
$pkg->tarball_link($pkg->tarball_name()) .
'</td></tr>' .
'<tr><td>theory source file</td><td>' .
$pkg->theory_file_link($pkg->theory_file_name()) .
' (included in the tarball)</li>' .
'</td></tr>' .
'</table>';

  if (count($includes) > 0) {
    $main .=
'<h3>Includes</h3>' .
'<ul>';

    foreach ($includes as $inc) {
      $main .=
'<li>' .
$inc->link($inc->to_string()) .
' &mdash; ' .
string_to_html($inc->description()) .
'</li>';
    }

    $main .=
'</ul>';
  }

  if ($num_included_by > 0) {
    $main .=
'<h3>Included By</h3>';

    if ($num_included_by <= CHILD_PACKAGE_LIMIT) {
      $included_by = package_included_by($pkg);

      $main .=
'<ul>';

      foreach ($included_by as $incby) {
        $main .=
'<li>' .
$incby->link($incby->to_string()) .
' &mdash; ' .
string_to_html($incby->description()) .
'</li>';
      }

      $main .=
'</ul>';
    }
    else {
      $main .= 'Included by ' . $num_included_by . ' packages';
    }
  }

  return $main;
}

///////////////////////////////////////////////////////////////////////////////
// Pretty upload information.
///////////////////////////////////////////////////////////////////////////////

function pretty_upload_information($upload) {
  isset($upload) or trigger_error('bad upload');

  $initiated = $upload->initiated();

  $since_initiated = $upload->since_initiated();

  $status = $upload->status();

  $author = $upload->author();

  $pkgs = packages_upload($upload);

  $obsolete_pkgs = obsolete_packages_upload($upload);

  $initiated_info = $since_initiated->to_string() . ' ago';

  $status_info = pretty_upload_status($status);

  $main =
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

  if (count($obsolete_pkgs) > 0) {
    $main .=
'<h3>Obsoleted Packages</h3>' .
'<ul>';

    foreach ($obsolete_pkgs as $pkg) {
      $pkg_author = $pkg->author();

      $main .=
'<li>' .
$pkg->link($pkg->to_string()) .
' &mdash; by ';

      if (isset($author) && $author->equal($pkg_author)) {
        $main .= 'the same author';
      }
      else {
        $main .= string_to_html($pkg_author->name());
      }

      $main .=
'</li>';
    }

    $main .=
'</ul>';
  }

  return $main;
}

///////////////////////////////////////////////////////////////////////////////
// Package page.
///////////////////////////////////////////////////////////////////////////////

$pkg = from_string(input('pkg'));
if (isset($pkg)) {
  set_bread_crumbs_extension(array());

  $name = from_string_package_name_version($pkg);

  if (isset($name)) {
    $pkg = find_package_by_name_version($name);

    $name = $name->to_string();
  }
  elseif (is_valid_package_name($pkg)) {
    $name = $pkg;

    $pkg = latest_package_version($name);

    if (isset($pkg)) { $name = $pkg->to_string(); }
  }
  else {
    trigger_error('bad package name');
  }

  $main =
'<h2>Package ' . $name . '</h2>';

  if (isset($pkg)) {
    $main .= pretty_package_information($pkg);
  }
  else {
    $main .=
'<p>Sorry for the inconvenience, but the package ' .
$name .
' is not on the ' .
repo_name() .
'.</p>';
  }

  $title = 'Package ' . $name;

  $image = site_image('sunset-tree.jpg','Sunset Tree');

  output(array('title' => $title), $main, $image);
}

///////////////////////////////////////////////////////////////////////////////
// Upload page.
///////////////////////////////////////////////////////////////////////////////

$upload = from_string(input('upload'));
if (isset($upload)) {
  set_bread_crumbs_extension(array());

  $upload = find_upload($upload);

  $main =
'<h2>Package Upload</h2>';

  if (isset($upload)) {
    $main .=
pretty_upload_information($upload) .
'<h3>Actions</h3>' .
'<ul>';

    if ($upload->add_packagable()) {
      $main .=
'<li>' .
site_link(array('upload'),
          'Add a package to this upload.',
          array('u' => $upload->to_string())) .
'</li>';
    }

    if ($upload->finishable()) {
      $main .=
'<li>' .
site_link(array('upload','finish'),
          'Finish adding packages and confirm package author.',
          array('u' => $upload->to_string())) .
'</li>';
    }

    $main .=
'<li>' .
site_link(array('upload','delete'),
          'Withdraw this package upload.',
          array('u' => $upload->to_string())) .
'</li>' .
'</ul>';
  }
  else {
    $main .=
'<p>Sorry for the inconvenience, but this package upload has ' .
'either completed or been withdrawn.</p>';
  }

  $title = 'Package Upload';

  $image = site_image('elephant-and-castle.jpg','Elephant and Castle');

  output(array('title' => $title), $main, $image);
}

///////////////////////////////////////////////////////////////////////////////
// Upload confirmation page.
///////////////////////////////////////////////////////////////////////////////

$confirm = from_string(input('confirm'));
if (isset($confirm)) {
  set_bread_crumbs_extension(array());

  $confirm = find_confirm_upload($confirm);

  $main =
'<h2>Package Upload Confirmation</h2>';

  if (isset($confirm)) {
    $upload = $confirm->upload();

    if (isset($upload)) {
      $action = from_string(input('x'));

      if (isset($action) && strcmp($action,'confirm') == 0) {
        if ($confirm->is_author()) {
          $obsolete_author = $upload->obsolete();

          if (isset($obsolete_author)) {
            delete_confirm_upload($confirm);

            set_confirm_obsolete_upload_status($upload);

            $confirm =
              create_new_confirm_upload(OBSOLETE_CONFIRM_UPLOAD_TYPE,$upload);

            $author = $upload->author();

            $address = $obsolete_author->to_string();

            $subject = 'Sign off on upload that obsoletes your package';

            $body =
'Hi ' . $obsolete_author->name() . ',

This is an automatic email sent on behalf of the maintainer of the
' . repo_name() . '.

The reason for this email is that I just took delivery of a theory
package upload that obsoletes one or more of your packages, made by

' . $author->to_string() . '

Hopefully they\'ve followed the recommended practice and contacted you
to discuss taking over the maintenance of some of your packages, and
if so this notification won\'t be a surprise to you.

Please visit

' . $confirm->url() . '

to view the details of the package upload, particularly the section
called "Obsoleted Packages" to see which of your packages are
affected. After examining the upload, please click one of the links in
the "Actions" section at the bottom to either sign off on the upload
or send a report to me that this was not previously agreed.

Thank you for your contributions to the repo,

' . ADMIN_NAME;

            site_email($address,$subject,$body);

            $upload->jump();
          }
          else {
            delete_confirm_upload($confirm);

            complete_upload($upload);

            jump_path(array('recent'));
          }
        }
        elseif ($confirm->is_obsolete()) {
          delete_confirm_upload($confirm);

          $obsolete_author = $upload->obsolete();

          $author = $upload->author();

          $address = $author->to_string();

          $subject = 'Package upload complete';

          $body =
'Hi ' . $author->name() . ',

This is another automatic email from the maintainer of the
' . repo_name() . '.

I am happy to let you know that your recent upload is now complete and
the packages are now available. The last step was getting a sign-off
from the author of the packages that your upload obsoleted, and this
was just performed by ' . $obsolete_author->name() . '.

Thank you for your contribution,

' . ADMIN_NAME;

          site_email($address,$subject,$body);

          complete_upload($upload);

          $main .=
'<p>Thank you for signing off on the package upload!</p>';
        }
        else {
          trigger_error('default case');
        }
      }
      elseif (isset($action) && strcmp($action,'report') == 0) {
        if ($confirm->is_author()) {
          $main .=
'<p>Thank you for reporting to the repo maintainer that ' .
'you are not the author of this package upload, and sorry ' .
'for any inconvenience caused.</p>';

          $since_sent = $confirm->since_sent();

          $author = $upload->author();

          $subject = 'Package upload denied by author';

          $body =
$since_sent->to_string() .
' ago someone finished a theory package upload with author

' . $author->to_string() . '

but the owner of this email address reported this to be incorrect.';

          site_email(ADMIN_EMAIL,$subject,$body);
        }
        elseif ($confirm->is_obsolete()) {
          $main .=
'<p>Thank you for reporting to the repo maintainer that ' .
'you do not agree with this package upload obsoleting ' .
'your package, and sorry for any inconvenience caused.</p>';

          $author = $upload->author();

          $obsolete = $upload->obsolete();
          if (!isset($obsolete)) { trigger_error('bad obsolete'); }

          $address = $author->to_string();

          $subject = 'Package upload denied by obsoleted author';

          $body =
'Hi ' . $author->name() . ',

This is another automatic email from the maintainer of the
' . repo_name() . '.

Your recent package upload obsoleted one or more packages that were
authored by

' . $obsolete->to_string() . '

In this situation the repo policy is that they have to agree to you
taking over as maintainer of these packages, and so an email was sent
asking them to sign off on the upload. In this instance the author of
the obsoleted packages did not agree, and so your package upload was
deleted.

To prevent this happening in the future, I recommend getting the
permission of package authors to take over as maintainer before
uploading obsoleting packages.

Sorry for the inconvenience,

' . ADMIN_NAME;

          site_email($address,$subject,$body);
          site_email(ADMIN_EMAIL,$subject,$body);
        }
        else {
          trigger_error('default case');
        }

        delete_confirm_upload($confirm);
        delete_upload($upload);
      }
      else {
        $main .=
pretty_upload_information($upload) .
'<h3>Actions</h3>' .
'<ul>';

        if ($confirm->is_author()) {
          $main .=
'<li>' .
site_link(array(),
          'Confirm that I am the author of this package upload.',
          array('confirm' => $confirm->to_string(),
                'x' => 'confirm')) .
'</li>' .
'<li>' .
site_link(array(),
          'Report to the repo maintainer that I am not the author.',
          array('confirm' => $confirm->to_string(),
                'x' => 'report')) .
'</li>';
        }
        elseif ($confirm->is_obsolete()) {
          $main .=
'<li>' .
site_link(array(),
          'Sign off on this package upload.',
          array('confirm' => $confirm->to_string(),
                'x' => 'confirm')) .
'</li>' .
'<li>' .
site_link(array(),
          'Report to the repo maintainer that I do not agree with this upload.',
          array('confirm' => $confirm->to_string(),
                'x' => 'report')) .
'</li>';
        }
        else {
          trigger_error('default case');
        }

        $main .=
'</ul>';
      }
    }
    else {
      $since_sent = $confirm->since_sent();

      $main .=
'<p>Sorry for the inconvenience, but since the confirmation ' .
'email was sent out to you ' .
$since_sent->to_string() .
' ago, this package upload has been withdrawn.</p>';

      delete_confirm_upload($confirm);
    }
  }
  else {
    $main .=
'<p>Sorry for the inconvenience, but this package upload confirmation ' .
'has expired.</p>';
  }

  $title = 'Package Upload Confirmation';

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
         '<script type="text/javascript" src="http://twitter.com/javascripts/blogger.js"></script><script type="text/javascript" src="http://api.twitter.com/1/statuses/user_timeline.json?screen_name=OpenTheory&amp;include_rts=true&amp;count=' . TWEET_LIMIT . '&amp;callback=twitterCallback2"></script>'));

output(array(), $main, $image);

?>
