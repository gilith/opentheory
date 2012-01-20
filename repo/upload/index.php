<?php

require_once '../opentheory.php';

///////////////////////////////////////////////////////////////////////////////
// A class to collect package data on a form.
///////////////////////////////////////////////////////////////////////////////

class SelectUploadData extends SelectValue {
  var $_select_upload;
  var $_select_tarball;
  var $_select_checksum;
  var $_select_submit;

  function error() {
    $e = $this->_error;
    if (!isset($e)) { $e = $this->_select_upload->handled_error(); }
    if (!isset($e)) { $e = $this->_select_tarball->handled_error(); }
    if (!isset($e)) { $e = $this->_select_checksum->handled_error(); }
    return $e;
  }

  function validate() {
    parent::validate();

    $this->_select_upload->validate();

    $this->_select_tarball->validate();

    $this->_select_checksum->validate();

    if (!$this->is_error()) {
      $file = $this->_select_tarball->value();

      $tarball_name = $file['client'];
      $tarball_name = ereg_replace('^.*/','',$tarball_name);

      $name_version = from_tarball_package_name_version($tarball_name);

      if (!isset($name_version)) {
        $this->set_error('tarball filename is not a valid package name');
      }

      if (!$this->is_error()) {
        $upload = $this->_select_upload->value();

        if (isset($upload)) {
          $upload = find_upload($upload);

          if (!isset($upload)) {
            $this->set_error('upload key is not known');
          }
        }

        if (!$this->is_error()) {
          $value = array();

          if (isset($upload)) {
            $value['upload'] = $upload;
          }

          $tarball = $file['server'];

          $value['tarball'] = $tarball;
          $value['name_version'] = $name_version;

          $checksum = $this->_select_checksum->value();

          if (isset($checksum)) {
            $value['checksum'] = $checksum;
          }

          parent::set_value($value);
        }
      }
    }
  }

  function select() {
    return
'<p>' .
$this->form_error() .
$this->_select_upload->form_error() .
$this->_select_upload->hidden_select() .
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

    $this->_select_upload = new SelectChecksum($this->field() . 'u', false);
    $this->_select_tarball = new SelectFile($this->field() . 't', true);
    $this->_select_checksum = new SelectChecksum($this->field() . 'c', false);
    $this->_select_submit =
      new SelectSubmit($this->field() . 'x', 'upload package');

    if ($this->submitted()) { $this->validate(); }
  }
}

///////////////////////////////////////////////////////////////////////////////
// Main page.
///////////////////////////////////////////////////////////////////////////////

$select = new SelectUploadData('');

$main = null;

if ($select->is_value()) {
  $value = $select->value();

  if (array_key_exists('upload',$value)) {
    $upload = $value['upload'];
  }
  else {
    $upload = null;
  }

  $tarball = $value['tarball'];

  $name_version = $value['name_version'];

  if (array_key_exists('checksum',$value)) {
    $checksum = $value['checksum'];
  }
  else {
    $checksum = null;
  }

  if (isset($upload) && !$upload->add_packagable()) {
    $error = 'this upload set is closed';
  }
  else {
    $error = opentheory_stage($tarball,$name_version,$checksum);
  }

  if (!isset($error)) {
    if (!isset($upload)) { $upload = create_new_upload(); }

    $tags = opentheory_staged_tags($name_version);

    $registered = opentheory_staged_timestamp($name_version);

    $includes = opentheory_staged_includes($name_version);

    $error = repo_check_staged($upload,$name_version,$tags,$includes);

    if (isset($error)) { opentheory_cleanup($name_version); }
  }

  if (isset($error)) {
    if (is_script()) {
      $report =
        'failed to upload package ' . $name_version->to_string() . ':' . "\n" .
        $error;

      output_script($report);
    }

    $main =
'<p>Failed to upload package ' . $name_version->to_string() . ':</p>' .
'<pre>' . $error . '</pre>';
  }
  else {
    $pkg = repo_register_staged($upload,$name_version,$tags,$registered,
                                $includes);

    $status = $upload->status();

    if (equal_upload_status($status,INITIAL_UPLOAD_STATUS)) {
      $author = $pkg->author();

      set_add_package_upload_status($upload,$author);
    }

    if (is_script()) {
      $report = 'successfully uploaded package ' . $name_version->to_string();
      output_script($report);
    }
    else {
      $upload->jump();
    }
  }
}

$title = 'Upload Package';

if (isset($main)) {
  set_bread_crumbs_extension(array());
}
else {
  $main =
'<h2>Upload a Package</h2>

<p>The recommended way to upload packages is to use the

<a href="http://www.gilith.com/software/opentheory/">opentheory</a>

package management tool, but it is also possible to <i>manually</i>
upload package tarballs using the following form:</p>' .

site_form(bread_crumbs(),
          $select->select(),
          null,
          'post');
}

$image = site_image('elephant-and-castle.jpg','Elephant and Castle');

output(array('title' => $title), $main, $image);

?>
