<?php

require_once '../opentheory.php';

$title = 'Packages';

$main =
'<p>List of packages</p>';

$image = site_image('katoomba.jpg','Katoomba Scenic Railway');

output(array('title' => $title), $main, $image);

?>
