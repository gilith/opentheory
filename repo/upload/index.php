<?php

require_once '../opentheory.php';

///////////////////////////////////////////////////////////////////////////////
// A class to collect package data on a form.
///////////////////////////////////////////////////////////////////////////////

class SelectUploadData extends SelectValue {
  var $_select_url;
  var $_select_submit;

  function error() {
    $e = $this->_error;
    if (!isset($e)) { $e = $this->_select_url->handled_error(); }
    return $e;
  }

  function validate() {
    parent::validate();

    $this->_select_url->validate();

    if (!$this->is_error()) {
      $data = $this->_select_url->value();
      parent::set_value($data);
    }
  }

  function select() {
    return
'<p>' .
$this->form_error() .
$this->_select_url->form_error() .
$this->_select_submit->form_error() .
field_text('URL') .
' &nbsp; ' .
$this->_select_url->select() .
' &nbsp; ' .
$this->_select_submit->select() .
'</p>';
  }

  function submitted() {
    return $this->_select_submit->is_value();
  }

  function set_value($data) {
    is_string($data) or trigger_error('bad data');

    $this->_select_url->set_value($data);

    parent::set_value($data);
  }

  function SelectURLData($field) {
    parent::SelectValue($field);

    $this->_select_url = new SelectURL($this->field() . 'u', true, null);
    $this->_select_submit = new SelectSubmit($this->field() . 's', 'shorten');

    if ($this->submitted()) { $this->validate(); }
  }
}

///////////////////////////////////////////////////////////////////////////////
// Main page.
///////////////////////////////////////////////////////////////////////////////

$title = 'Upload Package';

$main =
'<p>Form to upload packages</p>';

$image = site_image('katoomba.jpg','Katoomba Scenic Railway');

output(array('title' => $title), $main, $image);

?>
