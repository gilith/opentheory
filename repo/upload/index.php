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

      $tarball_name = $file['client'];
      $tarball_name = ereg_replace('^.*/','',$tarball_name);

      $name_version = from_tarball_package_name_version($tarball_name);

      if (isset($name_version)) {
        $tarball = $file['server'];

        $value =
          array('tarball' => $tarball,
                'name_version' => $name_version);

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

$pkg = from_string(input('pkg'));
if (isset($pkg)) { $pkg = from_string_package_name_version(input('pkg')); }
if (isset($pkg)) { $pkg = find_package_by_name_version($pkg); }

if (isset($pkg)) {
  $title = 'Upload Package';

  $main =
'<p>Thank you for uploading ' . $pkg->to_string() . '</p>';

  $image = site_image('katoomba.jpg','Katoomba Scenic Railway');

  output(array('title' => $title), $main, $image);
}

$select = new SelectUploadData('');

$main = null;

if ($select->is_value()) {
  $value = $select->value();

  $tarball = $value['tarball'];

  $name_version = $value['name_version'];

  if (array_key_exists('checksum',$value)) {
    $checksum = $value['checksum'];
  }
  else {
    $checksum = null;
  }

  $output = opentheory_install($tarball,$name_version,$checksum);

  if (isset($output)) {
    if (is_script()) {
      $report =
        'failed to upload package ' . $name_version->to_string() . ':' . "\n" .
        $output;

      output_script($report);
    }

    $main =
'<p>Failed to upload package ' . $name_version->to_string() . ':</p>' .
'<pre>' . $output . '</pre>';
  }
  else {
    $pkg = repo_register($name_version);

    if (is_script()) {
      $report = 'successfully uploaded package ' . $name_version->to_string();
      output_script($report);
    }
    else {
      jump_path(bread_crumbs(), array('pkg' => $name_version->to_string()));
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
