<?php

require_once 'opentheory.php';

$main =
'<p>Hello and welcome</p>';

$image =
  array(site_image('silver-falls.jpg','Silver Falls'),
        ('<div id="twitter-wrapper"><div id="twitter"><p><a href="http://twitter.com/OpenTheory">OpenTheory twitter feed:</a></p><ul id="twitter_update_list"><li>Loading...</li></ul></div></div>' .
         '<script src="http://twitter.com/javascripts/blogger.js" type="text/javascript"></script><script src="http://twitter.com/statuses/user_timeline/OpenTheory.json?callback=twitterCallback2&amp;count=4" type="text/javascript"></script>'));

output(array(), $main, $image);

?>
