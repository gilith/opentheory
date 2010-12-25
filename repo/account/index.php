<?php

require_once '../opentheory.php';

$user = effective_user();

if (isset($user)) {
  $session = effective_session();
  $session_expires = $session->expires();

  $title = 'Account \'' . $user->name() . '\'';

  $select_session = session_form('s');

  $main =
'<h2>' . ucfirst(pretty_privilege($user->privilege())) .
' Account \'<kbd>' . string_to_html($user->name()) . '</kbd>\'</h2>

<p>' . field_text('Password') . '&nbsp; &nbsp;' .
'<em>To request a password change, please email

<a href="mailto:' . ADMIN_EMAIL . '">' . ADMIN_EMAIL . '</a></em></p>

<hr />

<h2>Current Session</h2>

<p>' . field_text('Expires') . '&nbsp; &nbsp;' .
$session_expires->to_string_date() . '</p>' .

site_form(array('account'),
          $select_session->select());

  $image = site_image('the-castle.jpg','The Castle');

  output(array('title' => $title), $main, $image);
}
else {
  $title = 'Login';

  $select = login_form();

  $main =
site_form(array('account'),
          $select->select(),
          null,
          'POST');

  $image = site_image('easter-island-head.jpg','Easter Island Head');

  output(array('title' => $title), $main, $image);
}

?>
