<?php

require_once '../opentheory.php';

///////////////////////////////////////////////////////////////////////////////
// A class to collect package data on a form.
///////////////////////////////////////////////////////////////////////////////

class SelectUploadData extends SelectValue {
  var $_select_tarball;
  var $_select_checksum;
  var $_select_submit;

  function error() {
    $e = $this->_error;
    if (!isset($e)) { $e = $this->_select_tarball->handled_error(); }
    if (!isset($e)) { $e = $this->_select_checksum->handled_error(); }
    return $e;
  }

  function validate() {
    parent::validate();

    $this->_select_tarball->validate();

    $this->_select_checksum->validate();

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

        $checksum = $this->_select_checksum->value();

        if (isset($checksum)) {
          $value['checksum'] = $checksum;
        }

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
field_text('Tarball') . required_mark() .
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
    $this->_select_checksum = new SelectChecksum($this->field() . 'c', false);
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

  $args = ' --minimal';

  if (array_key_exists('checksum',$value)) {
    $args .= ' --checksum ' . $value['checksum'];
  }

  $args .= ' tarball:' . $tarball;

  $output = opentheory('install',$args);

  if (isset($output)) {
    $main =
'<p>Failed to install package tarball:</p>' .
'<pre>' . $output . '</pre>';
  }
  else {
    opentheory_list();

    if (is_script()) {
      output_script('successfully uploaded package ' . $name);
    }
    else {
      jump_path(bread_crumbs(), array('p' => $name));
    }
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

$image = site_image('easter-island-head.jpg','Easter Island Head');

output(array('title' => $title), $main, $image);

?>
