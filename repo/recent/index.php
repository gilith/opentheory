<?php

require_once '../opentheory.php';

$title = 'Recent Uploads';

$main =
'<p>List of recently uploaded packages</p>';

$image = site_image('katoomba.jpg','Katoomba Scenic Railway');

output(array('title' => $title), $main, $image);

?>
