<?php

require_once '../../opentheory.php';

///////////////////////////////////////////////////////////////////////////////
// Check for admin privilege.
///////////////////////////////////////////////////////////////////////////////

if (!effective_privilege_is_admin()) { jump_path(array()); }

///////////////////////////////////////////////////////////////////////////////
// Constants.
///////////////////////////////////////////////////////////////////////////////

define('SHORT_PACKAGES_OUT_OF_SYNC',5);

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
      new SelectSubmit($this->field() . 'x', 'sync timestamps');

    if ($this->submitted()) { $this->validate(); }
  }
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

$select_sync = new SelectSyncData('s');

if ($select_sync->is_value()) {
  trigger_error('synchronizing timestamps is not implemented');

  if (is_script()) {
    output_script('successfully synchronized package timestamps');
  }
  else {
    jump_path(bread_crumbs());
  }
}

$issues = array();

$out_of_sync = packages_out_of_sync();

if (count($out_of_sync) != 0) {
  $n = count($out_of_sync);

  $issue = '<ul>';

  $i = 0;
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
'<li><em>...' . $k . ' unsynchronized packages omitted...</em></li>';
    }
  }

  $issue .= '</ul>';

  $issues[] =
'<h2>Issue: ' . $n . ' Package Timestamp' .
(($n == 1) ? '' : 's') .
' Out of Sync</h2>' .
$issue .
site_form(bread_crumbs(),
          $select_sync->select());
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

?>
