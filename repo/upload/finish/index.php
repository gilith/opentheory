<?php

require_once '../../opentheory.php';

///////////////////////////////////////////////////////////////////////////////
// Finish the package upload.
///////////////////////////////////////////////////////////////////////////////

$upload = from_string(input('u'));
if (isset($upload)) { $upload = find_upload($upload); }
if (!isset($upload)) { trigger_error('bad upload'); }
if (!$upload->finishable()) { trigger_error('cannot finish'); }

$obsolete = obsolete_author_upload($upload);

$author = $upload->author();

$confirm = create_new_confirm_upload(AUTHOR_CONFIRM_UPLOAD_TYPE,$upload);

$address = $author->to_string();

$subject = 'Confirm package upload';

$body = 'Hi, I\'m the maintainer of the ' . repo_name() . '.

Someone just made a theory package upload with your email address
as the package author, and this is an automatic email to make sure
that you are indeed the package author.

Please visit

' . $confirm->url() . '

to view the package upload, and click one of the links in the
"Actions" section at the bottom to either confirm that you are
the package author or send me a report that you are not.

Thank you for your time,

' . ADMIN_NAME;

site_email($address,$subject,$body);

set_confirm_author_upload_status($upload,$obsolete);

if (is_script()) {
  output_script('successfully finished package upload');
}
else {
  $upload->jump();
}

?>
