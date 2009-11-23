<?php

require_once '../opentheory.php';

$title = 'Upload Package';

$main =
'<p>Form to upload packages</p>';

$image = site_image('katoomba.jpg','Katoomba Scenic Railway');

output(array('title' => $title), $main, $image);

?>
