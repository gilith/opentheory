<?php

require_once 'opentheory.php';

$title = REPO_NAME . ' OpenTheory Repo';

$header = '<h1>' . REPO_NAME . ' OpenTheory Repo</h1>';

$main =
'<p>Hello and welcome</p>';

$image =
'<p class="top">' .
site_image('sky.jpg','Gilith is a Sindarin word meaning outer space') .
'</p>';

output(array('title' => $title), $header, $main, $image, null);

?>
