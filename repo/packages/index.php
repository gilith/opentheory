<?php

require_once '../opentheory.php';

$title = 'Packages';

$main =
'<p>List of packages</p>';

$image = site_image('tree.jpg','Sunset Tree');

output(array('title' => $title), $main, $image);

?>
