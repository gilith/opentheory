<?php

require_once '../opentheory.php';

///////////////////////////////////////////////////////////////////////////////
// A class to collect package data on a form.
///////////////////////////////////////////////////////////////////////////////

class SelectUploadData extends SelectValue {
  var $_select_tarball;
  var $_select_submit;

  function error() {
    $e = $this->_error;
    if (!isset($e)) { $e = $this->_select_tarball->handled_error(); }
    return $e;
  }

  function validate() {
    parent::validate();

    $this->_select_tarball->validate();

    if (!$this->is_error()) {
      $file = $this->_select_tarball->value();

      $tarname = $file['client'];
      $tarname = ereg_replace('^.*/','',$tarname);

      $name = dest_package_tarball($tarname);

      if (isset($name)) {
        $tarball = $file['server'];

        $value =
           array('name' => $name,
                 'tarball' => $tarball);

        parent::set_value($value);
      }
      else {
        $this->set_error('tarball filename is not a valid package name');
      }
    }
  }

  function select() {
    return
'<p>' .
$this->form_error() .
$this->_select_tarball->form_error() .
$this->_select_submit->form_error() .
field_text('Tarball') .
' &nbsp; ' .
$this->_select_tarball->select() .
'</p>' .
'<p>' .
$this->_select_submit->select() .
'</p>';
  }

  function submitted() {
    return $this->_select_submit->is_value();
  }

  function set_value($value) {
    trigger_error('can\'t set a package upload value');
  }

  function SelectUploadData($field) {
    parent::SelectValue($field);

    $this->_select_tarball = new SelectFile($this->field() . 't', true);
    $this->_select_submit =
      new SelectSubmit($this->field() . 's', 'upload package');

    if ($this->submitted()) { $this->validate(); }
  }
}

///////////////////////////////////////////////////////////////////////////////
// Main page.
///////////////////////////////////////////////////////////////////////////////

$pkg = from_string(input('p'));
if (isset($pkg)) {
  $title = 'Upload Package';

  $main =
'<p>Thank you for uploading ' . $pkg . '</p>';

  $image = site_image('katoomba.jpg','Katoomba Scenic Railway');

  output(array('title' => $title), $main, $image);
}

$select = new SelectUploadData('');

$main = null;

if ($select->is_value()) {
  $value = $select->value();

  $name = $value['name'];

  $tarball = $value['tarball'];

  $output = opentheory('install',' tarball:' . $tarball);

  if (isset($output)) {
    $main =
'<p>Failed to install package tarball:</p>' .
'<pre>' . $output . '</pre>';
  }
  else {
    jump_path(bread_crumbs(), array('p' => $name));
  }
}

$title = 'Upload Package';

if (isset($main)) {
  set_bread_crumbs_extension(array());
}
else {
  $main =
'<p>Form to upload packages</p>' .

site_form(bread_crumbs(),
          $select->select(),
          null,
          'post');
}

$image = site_image('katoomba.jpg','Katoomba Scenic Railway');

output(array('title' => $title), $main, $image);

?>
