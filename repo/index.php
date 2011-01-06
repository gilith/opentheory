<?php

require_once 'opentheory.php';

///////////////////////////////////////////////////////////////////////////////
// Constants.
///////////////////////////////////////////////////////////////////////////////

define('SHORT_RECENT_PACKAGE_LIMIT',3);

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
