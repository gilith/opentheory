<?php

require_once '../../opentheory.php';

///////////////////////////////////////////////////////////////////////////////
// Check for admin privilege.
///////////////////////////////////////////////////////////////////////////////

if (!effective_privilege_is_admin()) { jump_path(array()); }

///////////////////////////////////////////////////////////////////////////////
// A class to collect config data on a form.
///////////////////////////////////////////////////////////////////////////////

define('CONFIG_FILE', SITE_PATH . '/' . REPO_DIR . '/config');

define('CONFIG_COLUMNS',80);

define('CONFIG_ROWS',20);

class SelectConfigData extends SelectValue {
  var $_select_config;
  var $_select_submit;

  function error() {
    $e = $this->_error;
    if (!isset($e)) { $e = $this->_select_config->handled_error(); }
    return $e;
  }

  function validate() {
    parent::validate();

    $this->_select_config->validate();

    if (!$this->is_error()) {
      $value = $this->_select_config->value();

      parent::set_value($value);
    }
  }

  function select() {
    return
'<p>' .
$this->form_error() .
$this->_select_config->form_error() .
$this->_select_submit->form_error() .
$this->_select_config->select() .
'</p>' .
'<p>' .
$this->_select_submit->select() .
'</p>';
  }

  function submitted() {
    return $this->_select_submit->is_value();
  }

  function set_value($value) {
    $this->_select_config->set_value($value);
  }

  function SelectConfigData($field) {
    parent::SelectValue($field);

    $this->_select_config =
      new SelectTextArea($this->field() . 'c', true,
                         CONFIG_COLUMNS, CONFIG_ROWS);

    $this->_select_submit =
      new SelectSubmit($this->field() . 'x', 'update configuration');

    if ($this->submitted()) { $this->validate(); }
  }
}

///////////////////////////////////////////////////////////////////////////////
// Main page.
///////////////////////////////////////////////////////////////////////////////

$select = new SelectConfigData('');

if ($select->is_value()) {
  $config = $select->value();

  $value = file_put_contents(CONFIG_FILE,$config);

  jump_path(bread_crumbs());
}
else {
  $config = file_get_contents(CONFIG_FILE);

  if (!is_string($config)) { trigger_error("couldn't open config file"); }

  $select->set_value($config);
}

$title = 'Admin Config';

$main =
'<h2>Update the ' . ucfirst(REPO_NAME) . ' Repo Configuration</h2>' .

site_form(bread_crumbs(),
          $select->select());

$image = site_image('cedar-point.jpg','Cedar Point');

output(array('title' => $title), $main, $image);

?>
