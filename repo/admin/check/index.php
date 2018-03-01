<?php

require_once '../../opentheory.php';

///////////////////////////////////////////////////////////////////////////////
// Check for admin privilege.
///////////////////////////////////////////////////////////////////////////////

if (!effective_privilege_is_admin()) { jump_path(array()); }

///////////////////////////////////////////////////////////////////////////////
// Constants.
///////////////////////////////////////////////////////////////////////////////

define('SHORT_UNREGISTERED_STAGED_PACKAGES',5);

define('SHORT_PACKAGES_OUT_OF_SYNC',5);

///////////////////////////////////////////////////////////////////////////////
// A class to collect unregistered staged package data on a form.
///////////////////////////////////////////////////////////////////////////////

class SelectStagedData extends SelectValue {
  var $_select_packages;
  var $_select_submit;

  function error() {
    $e = $this->_error;
    return $e;
  }

  function validate() {
    parent::validate();

    if (!$this->is_error()) {
      $value = $this->_select_packages->value();

      parent::set_value($value);
    }
  }

  function select() {
    return
'<p>' .
$this->form_error() .
$this->_select_packages->form_error() .
$this->_select_packages->select() .
$this->_select_submit->form_error() .
$this->_select_submit->select() .
'</p>';
  }

  function submitted() {
    return $this->_select_submit->is_value();
  }

  function set_value($value) {
    is_array($value) or trigger_error('bad value');

    $this->_select_packages->set_value($value);
  }

  function SelectStagedData($field) {
    parent::SelectValue($field);

    $this->_select_packages =
      new SelectHidden($this->field() . 'p');

    $this->_select_submit =
      new SelectSubmit($this->field() . 'x', 'cleanup packages');

    if ($this->submitted()) { $this->validate(); }
  }
}

///////////////////////////////////////////////////////////////////////////////
// A class to collect timestamp synchronization data on a form.
///////////////////////////////////////////////////////////////////////////////

class SelectSyncData extends SelectValue {
  var $_select_submit;

  function error() {
    $e = $this->_error;
    return $e;
  }

  function validate() {
    parent::validate();

    if (!$this->is_error()) {
      $value = $this->_select_submit->value();

      parent::set_value($value);
    }
  }

  function select() {
    return
'<p>' .
$this->form_error() .
$this->_select_submit->form_error() .
$this->_select_submit->select() .
'</p>';
  }

  function submitted() {
    return $this->_select_submit->is_value();
  }

  function SelectSyncData($field) {
    parent::SelectValue($field);

    $this->_select_submit =
      new SelectSubmit($this->field() . 'x',
                       'change package timestamps to match database records');

    if ($this->submitted()) { $this->validate(); }
  }
}

///////////////////////////////////////////////////////////////////////////////
// A class to collect repo checking configuration data on a form.
///////////////////////////////////////////////////////////////////////////////

class SelectCheckData extends SelectValue {
  var $_select_staged;
  var $_select_sync;
  var $_select_submit;

  function error() {
    $e = $this->_error;
    if (!isset($e)) { $e = $this->_select_staged->handled_error(); }
    if (!isset($e)) { $e = $this->_select_sync->handled_error(); }
    return $e;
  }

  function validate() {
    parent::validate();

    $this->_select_staged->validate();
    $this->_select_sync->validate();

    if (!$this->is_error()) {
      $value = array();
      $value['staged'] = $this->_select_staged->value();
      $value['sync'] = $this->_select_sync->value();

      parent::set_value($value);
    }
  }

  function select() {
    return
'<p>' .
$this->form_error() .
$this->_select_staged->form_error() .
field_text('Check for unregistered staged packages') .
' &nbsp; ' .
$this->_select_staged->select() .
'</p>' .
'<p>' .
$this->_select_sync->form_error() .
field_text('Check for timestamps out of sync') .
' &nbsp; ' .
$this->_select_sync->select() .
'</p>' .
'<p>' .
$this->_select_submit->form_error() .
$this->_select_submit->select() .
'</p>';
  }

  function submitted() {
    return $this->_select_submit->is_value();
  }

  function set_value($value) {
    is_array($value) or trigger_error('bad value');

    $this->_select_staged->set_value($value['staged']);
    $this->_select_sync->set_value($value['sync']);
  }

  function SelectCheckData($field) {
    parent::SelectValue($field);

    $this->_select_staged =
      new SelectCheckbox($this->field() . 'u');

    $this->_select_sync =
      new SelectCheckbox($this->field() . 's');

    $this->_select_submit =
      new SelectSubmit($this->field() . 'x', 'check repo');

    if ($this->submitted()) { $this->validate(); }
  }
}

///////////////////////////////////////////////////////////////////////////////
// Finding unregistered staged packages.
///////////////////////////////////////////////////////////////////////////////

function unregistered_staged_packages() {
  $result = array();

  $namevers = opentheory_list_staged();

  foreach ($namevers as $namever) {
    $pkg = find_package_by_name_version($namever);

    if (!isset($pkg)) {
      $result[] = $namever;
    }
  }

  return $result;
}

///////////////////////////////////////////////////////////////////////////////
// Finding packages with timestamps out of sync.
///////////////////////////////////////////////////////////////////////////////

function packages_out_of_sync_compare($a, $b) {
  $a1 = $a[1];
  $b1 = $b[1];

  return $a1 - $b1;
}

function packages_out_of_sync() {
  $result = array();

  $namevers = opentheory_list('All');

  foreach ($namevers as $namever) {
    $pkg = find_package_by_name_version($namever);

    if (isset($pkg)) {
      $timestamp = opentheory_timestamp($namever);

      $registered = $pkg->registered();

      if (!$timestamp->equal($registered)) {
        $diff = $timestamp->to_datetime() - $registered->to_datetime();

        $result[] = array($namever,$diff);
      }
    }
  }

  usort($result, "packages_out_of_sync_compare");

  return $result;
}

///////////////////////////////////////////////////////////////////////////////
// Main page.
///////////////////////////////////////////////////////////////////////////////

$select_staged = new SelectStagedData('u');
$select_sync = new SelectSyncData('s');
$select = new SelectCheckData('c');

if ($select_staged->submitted()) {
  $staged = $select_staged->value();

  foreach($staged as $nv) {
    $name_version = from_string_package_name_version($nv);

    opentheory_cleanup($name_version);
  }

  if (is_script()) {
    output_script('successfully cleaned up unregistered staged packages');
  }
  else {
    jump_path(bread_crumbs());
  }
}

if ($select_sync->submitted()) {
  $out_of_sync = packages_out_of_sync();

  $n = count($out_of_sync);

  for ($i = 0; $i < $n; ++$i) {
    $data = $out_of_sync[$i];
    $namever = $data[0];

    $pkg = find_package_by_name_version($namever);
    isset($pkg) or trigger_error('package disappeared from database');
    $registered = $pkg->registered();

    opentheory_set_timestamp($namever,$registered);
  }

  if (is_script()) {
    output_script('successfully synchronized package timestamps');
  }
  else {
    jump_path(bread_crumbs());
  }
}

if ($select->submitted()) {
  set_bread_crumbs_extension(array());

  $config = $select->value();

  $issues = array();

  if ($config['staged']) {
    $unregistered_staged = unregistered_staged_packages();

    if (count($unregistered_staged) != 0) {
      $n = count($unregistered_staged);

      $unregistered_staged_names = array();

      $issue = '<ul>';

      for ($i = 0; $i < $n; ++$i) {
        $namever = $unregistered_staged[$i];

        $unregistered_staged_names[] = $namever->to_string();

        if ($n <= 2 * SHORT_UNREGISTERED_STAGED_PACKAGES + 1 ||
            $i < SHORT_UNREGISTERED_STAGED_PACKAGES ||
            $i >= $n - SHORT_UNREGISTERED_STAGED_PACKAGES) {
          $issue .=
'<li>' . $namever->staged_document_file_link($namever->to_string()) . '</li>';
        }
        elseif ($i == SHORT_UNREGISTERED_STAGED_PACKAGES) {
          $k = $n - 2 * SHORT_UNREGISTERED_STAGED_PACKAGES;

          $issue .=
'</ul><p><em>...' . pretty_number($k) .
' unregistered staged packages omitted...</em></p><ul>';
        }
      }

      $issue .= '</ul>';

      $select_staged->set_value($unregistered_staged_names);

      $issues[] =
'<h2>Issue: ' . pretty_number($n) . ' Unregistered Staged Package' .
(($n == 1) ? '' : 's') .
'</h2>' .
$issue .
site_form(bread_crumbs(),
          $select_staged->select());
    }
  }

  if ($config['sync']) {
    $out_of_sync = packages_out_of_sync();

    if (count($out_of_sync) != 0) {
      $n = count($out_of_sync);

      $issue = '<ul>';

      for ($i = 0; $i < $n; ++$i) {
        if ($n <= 2 * SHORT_PACKAGES_OUT_OF_SYNC + 1 ||
            $i < SHORT_PACKAGES_OUT_OF_SYNC ||
            $i >= $n - SHORT_PACKAGES_OUT_OF_SYNC) {
          $data = $out_of_sync[$i];
          $namever = $data[0];
          $diff = $data[1];

          if ($diff < 0) {
            $adj = 'earlier';

            $interval = new TimeInterval(-$diff);
          }
          else {
            $adj = 'later';

            $interval = new TimeInterval($diff);
          }

          $issue .=
'<li>' . $namever->package_link($namever->to_string()) . ' timestamp is ' .
'<span style="color: red">' . $interval->to_string() .
'</span> ' . $adj . ' than recorded</li>';
        }
        elseif ($i == SHORT_PACKAGES_OUT_OF_SYNC) {
          $k = $n - 2 * SHORT_PACKAGES_OUT_OF_SYNC;

          $issue .=
'</ul><p><em>...' . pretty_number($k) .
' unsynchronized packages omitted...</em></p><ul>';
        }
      }

      $issue .= '</ul>';

      $issues[] =
'<h2>Issue: ' . pretty_number($n) . ' Package Timestamp' .
(($n == 1) ? '' : 's') .
' Out of Sync</h2>' .
$issue .
site_form(bread_crumbs(),
          $select_sync->select());
    }
  }

  $title = 'Admin Check';

  if (count($issues) == 0) {
    $main = '<p>Everything looks good right now, but thanks for checking.</p>';
  }
  else {
    $main = implode('',$issues);
  }

  $image = site_image('flooded-fenland.jpg','Flooded Fenland');

  output(array('title' => $title), $main, $image);
}

$select->set_value(array('staged' => true, 'sync' => true));

$title = 'Admin Check';

$main =
'<h2>Check the ' . ucfirst(REPO_NAME) . ' OpenTheory Repo</h2>' .

site_form(bread_crumbs(),
          $select->select());

$image = site_image('flooded-fenland.jpg','Flooded Fenland');

output(array('title' => $title), $main, $image);

?>
