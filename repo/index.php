<?php

require_once 'opentheory.php';

$title = REPO_NAME . ' OpenTheory Repo';

$main =
'<p>Hello and welcome</p>';

$image = site_image('katoomba.jpg','Katoomba Scenic Railway');

output(array('title' => $title), $main, $image);

?>
