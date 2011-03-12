<?php

require_once '../../opentheory.php';

///////////////////////////////////////////////////////////////////////////////
// Delete the package upload.
///////////////////////////////////////////////////////////////////////////////

$upload = from_string(input('u'));
if (isset($upload)) { $upload = find_upload($upload); }
if (!isset($upload)) { trigger_error('bad upload'); }

delete_upload($upload);

if (is_script()) {
  output_script('successfully deleted package upload');
}
else {
  jump_path(array());
}

?>
