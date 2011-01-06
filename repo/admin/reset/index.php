<?php

require_once '../../opentheory.php';

///////////////////////////////////////////////////////////////////////////////
// Check for admin privilege.
///////////////////////////////////////////////////////////////////////////////

if (!effective_privilege_is_admin()) { jump_path(array()); }

///////////////////////////////////////////////////////////////////////////////
// A class to collect reset data on a form.
///////////////////////////////////////////////////////////////////////////////

class SelectResetData extends SelectValue {
  var $_select_delete;
  var $_select_submit;

  function error() {
    $e = $this->_error;
    if (!isset($e)) { $e = $this->_select_delete->handled_error(); }
    return $e;
  }

  function validate() {
    parent::validate();

    $this->_select_delete->validate();

    if (!$this->is_error()) {
      $value = $this->_select_delete->value();

      parent::set_value($value);
    }
  }

  function select() {
    return
'<p>' .
$this->form_error() .
$this->_select_delete->form_error() .
$this->_select_submit->form_error() .
field_text('Delete all repo packages') .
' &nbsp; ' .
$this->_select_delete->select() .
'</p>' .
'<p>' .
$this->_select_submit->select() .
'</p>';
  }

  function submitted() {
    return $this->_select_submit->is_value();
  }

  function set_value($value) {
    $select_delete->set_value($value);
  }

  function SelectResetData($field) {
    parent::SelectValue($field);

    $this->_select_delete =
      new SelectCheckbox($this->field() . 'd');

    $this->_select_submit =
      new SelectSubmit($this->field() . 'x', 'reset repo');

    if ($this->submitted()) { $this->validate(); }
  }
}

///////////////////////////////////////////////////////////////////////////////
// Main page.
///////////////////////////////////////////////////////////////////////////////

$select = new SelectResetData('');

if ($select->is_value()) {
  $delete = $select->value();

  if ($delete) {
    opentheory_reset();
  }

  repo_reset();

  if (is_script()) {
    output_script('successfully reset the package directory');
  }
  else {
    jump_path(array('admin'));
  }
}

$title = 'Admin Reset';

$main =
'<h2>Reset the ' . ucfirst(REPO_NAME) . ' OpenTheory Repo</h2>' .

site_form(bread_crumbs(),
          $select->select());

$image = site_image('flooded-fenland.jpg','Flooded Fenland');

output(array('title' => $title), $main, $image);

?>
