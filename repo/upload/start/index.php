<?php

require_once '../../opentheory.php';

if (is_script()) {
  $upload = create_new_upload();

  output_script('new upload = ' . $upload->id());
}
else {
  jump_path(array('upload'));
}

?>
