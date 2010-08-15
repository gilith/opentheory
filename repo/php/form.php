<?php

///////////////////////////////////////////////////////////////////////////////
//
// OPENTHEORY FORMS
//
// Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2
//
///////////////////////////////////////////////////////////////////////////////

require_once 'functions.php';
require_once 'date.php';
require_once 'input.php';
require_once 'links.php';

///////////////////////////////////////////////////////////////////////////////
// Constants.
///////////////////////////////////////////////////////////////////////////////

define('MAX_TEXT_INPUT_CHARS',60);
define('MAX_PASSWORD_INPUT_CHARS',MAX_TEXT_INPUT_CHARS);

///////////////////////////////////////////////////////////////////////////////
// Form widgets.
///////////////////////////////////////////////////////////////////////////////

function hidden_input($name,$value) {
  is_string($name) or trigger_error('bad name');

  if (!isset($value)) { return ''; }

  $ret = '';

  if (is_array($value)) {
    foreach ($value as $v) {
      $ret .=
        '<input type="hidden" name="' . string_to_html($name . '[]') .
        '" value="' . string_to_html($v) . '" />';
    }
  }
  else {
    $ret .=
      '<input type="hidden" name="' . string_to_html($name) .
      '" value="' . string_to_html($value) . '" />';
  }

  return $ret;
}

function select_option($value,$selected,$text) {
  is_bool($selected) or trigger_error('bad selected');
  isset($text) or trigger_error('bad text');

  return
'<option' . (isset($value) ? (' value="' . $value . '"') : '') .
($selected ? ' selected="selected"' : '') . '>' .
string_to_html($text) . '</option>';
}

function select_input($name,$multiple,$options) {
  is_string($name) or trigger_error('bad name');
  is_bool($multiple) or trigger_error('bad multiple');
  is_array($options) or trigger_error('bad options');

  return
'<select name="' . $name .
($multiple ? '[]" multiple="multiple"' : '"') .
' size="' . ($multiple ? '5' : '1') . '">' .
implode('',$options) . '</select>';
}

function radio_input($name,$value,$selected) {
  is_string($name) or trigger_error('bad name');
  is_bool($selected) or trigger_error('bad selected');

  return
'<input type="radio" name="' . $name .
'" value="' . (isset($value) ? $value : '') . '"' .
($selected ? ' checked="checked"' : '') . ' />';
}

function checkbox_input($name,$value,$checked) {
  is_string($name) or trigger_error('bad name');
  is_bool($checked) or trigger_error('bad checked');

  return
'<input type="checkbox" name="' . $name .
'" value="' . (isset($value) ? $value : '') . '"' .
($checked ? ' checked="checked"' : '') . ' />';
}

function text_input($name, $value, $size = null) {
  is_string($name) or trigger_error('bad name');
  if (!isset($size)) { $size = MAX_TEXT_INPUT_CHARS; }
  is_int($size) or trigger_error('bad size');

  return
'<input type="text" name="' . $name . '" size="' .
min($size,MAX_TEXT_INPUT_CHARS) .
'" value="' . (isset($value) ? $value : '') . '" />';
}

function password_input($name, $value, $size = null) {
  is_string($name) or trigger_error('bad name');
  if (!isset($size)) { $size = MAX_PASSWORD_INPUT_CHARS; }
  is_int($size) or trigger_error('bad size');

  return
'<input type="password" name="' . $name . '" size="' .
min($size,MAX_PASSWORD_INPUT_CHARS) . '" value="' .
(isset($value) ? $value : '') . '" />';
}

function button_input($name, $value) {
  is_string($value) or trigger_error('bad value');

  return
'<input type="submit"' .
(isset($name) ? (' name="' . $name . '"') : '') .
' value="' . $value . '" />';
}

function image_button_input($image, $name, $value = null) {
  is_string($image) or trigger_error('bad image');

  return
'<input type="image" src="' . site_image_path($image) . '"' .
(isset($name)
 ? (' name="' . $name . '" value="' . (isset($value) ? $value : '1') . '"')
 : '') .
' alt="" />';
}

function file_input($name) {
  is_string($name) or trigger_error('bad name');

  return '<input type="file" name="' . $name . '" />';
}

function hidden_button($name, $value = null) {
  return image_button_input('transparent-1x1.gif',$name,$value);
}

///////////////////////////////////////////////////////////////////////////////
// The abstract SelectData class.
///////////////////////////////////////////////////////////////////////////////

class SelectData {
  var $_field;

  function field() { return $this->_field; }

  function value() { trigger_error('abstract function'); }

  function set_value() { trigger_error('abstract function'); }

  function error() { trigger_error('abstract function'); }

  function set_error() { trigger_error('abstract function'); }

  function select() { trigger_error('abstract function'); }

  function is_value() {
    $value = $this->value();
    return isset($value);
  }

  function is_error() {
    $error = $this->error();
    return isset($error);
  }

  function handled_error() {
    $error = $this->error();
    return isset($error) ? '' : null;
  }

  function validate() {}

  function focus() { return null; }

  function submit() { return null; }

  function form_error() {
    $error = $this->error();

    if (isset($error) && strcmp($error,'') != 0) {
      return form_error_text($error) . '<br />';
    }
    else {
      return '';
    }
  }

  function input($name) {
    is_string($name) or trigger_error('bad name');
    return input($this->field() . $name);
  }

  function hidden_input($name,$value) {
    is_string($name) or trigger_error('bad name');
    return hidden_input($this->field() . $name, $value);
  }

  function select_option($value,$selected,$text) {
    return select_option($value,$selected,$text);
  }

  function select_input($name,$multiple,$options) {
    is_string($name) or trigger_error('bad name');
    return select_input($this->field() . $name, $multiple, $options);
  }

  function radio_input($name,$value,$selected) {
    is_string($name) or trigger_error('bad name');
    return radio_input($this->field() . $name, $value, $selected);
  }

  function checkbox_input($name,$value,$checked) {
    is_string($name) or trigger_error('bad name');
    return checkbox_input($this->field() . $name, $value, $checked);
  }

  function text_input($name, $value, $size = null) {
    is_string($name) or trigger_error('bad name');
    return text_input($this->field() . $name, $value, $size);
  }

  function password_input($name, $value, $size = null) {
    is_string($name) or trigger_error('bad name');
    return password_input($this->field() . $name, $value, $size);
  }

  function button_input($name,$value) {
    $button_name = (isset($name) ? ($this->field() . $name) : null);
    return button_input($button_name,$value);
  }

  function file_input($name) {
    is_string($name) or trigger_error('bad name');
    return file_input($this->field() . $name);
  }

  function SelectData($field) {
    is_string($field) or trigger_error('bad field');
    $this->_field = $field;
  }
}

///////////////////////////////////////////////////////////////////////////////
// The slightly less abstract SelectValue class.
///////////////////////////////////////////////////////////////////////////////

class SelectValue extends SelectData {
  var $_value;
  var $_error;

  function value() { return $this->_value; }

  function error() { return $this->_error; }

  function set_value($value) {
    $this->_value = $value;
    $this->_error = null;
  }

  function set_error($error) {
    $this->_error = $error;
  }

  function SelectValue($field) {
    parent::SelectData($field);
  }
}

///////////////////////////////////////////////////////////////////////////////
// Submission buttons.
///////////////////////////////////////////////////////////////////////////////

class SelectSubmit extends SelectValue {
  var $_button;

  function select() {
    return $this->button_input('',$this->_button);
  }

  function SelectSubmit($field,$button) {
    parent::SelectValue($field);

    $this->_button = $button;

    $this->set_value(from_string($this->input('')));
  }
}

///////////////////////////////////////////////////////////////////////////////
// Selecting checkboxes.
///////////////////////////////////////////////////////////////////////////////

class SelectCheckbox extends SelectValue {
  function select() {
    return $this->checkbox_input('','1',$this->value());
  }

  function SelectCheckbox($field) {
    parent::SelectValue($field);
    $this->set_value(bool_from_string($this->input('')));
  }
}

///////////////////////////////////////////////////////////////////////////////
// Selecting from drop-down lists.
///////////////////////////////////////////////////////////////////////////////

class SelectList extends SelectValue {
  var $_options;
  var $_required;

  function options() { return $this->_options; }

  function required() { return $this->_required; }

  function error_message_missing() {
    return 'Please select an option';
  }

  function null_option_text() {
    return '---';
  }

  function validate() {
    parent::validate();

    if (!$this->is_error() && $this->_required && !$this->is_value()) {
      $this->set_error($this->error_message_missing());
    }
  }

  function select() {
    $options = $this->options();
    $value = $this->value();

    $select_options = array();
    $select_options[] =
      $this->select_option('', !isset($value), $this->null_option_text());

    foreach ($options as $v => $t) {
      $selected = (isset($value) && (strcmp($value,$v) == 0));
      $select_options[] =
        $this->select_option($v,$selected,$t);
    }

    return $this->select_input('',false,$select_options);
  }

  function SelectList($field,$options,$required) {
    parent::SelectValue($field);

    $this->_options = $options;
    $this->_required = $required;
    $this->set_value(from_string($this->input('')));
  }
}

///////////////////////////////////////////////////////////////////////////////
// Selecting from radio lists.
///////////////////////////////////////////////////////////////////////////////

class SelectRadio extends SelectValue {
  var $_options;

  function options() { return $this->_options; }

  function validate() {
    parent::validate();
  }

  function select() {
    $options = $this->options();
    $value = $this->value();

    $select_options = array();

    foreach ($options as $v => $t) {
      $selected =
        (isset($value)
        ? (strcmp($value,$v) == 0)
        : (strcmp($v,'') == 0));

      $select_options[] = $this->radio_input('',$v,$selected) . $t;
    }

    return implode('&nbsp; &nbsp;', $select_options);
  }

  function SelectRadio($field,$options) {
    parent::SelectValue($field);

    $this->_options = $options;
    $this->set_value(from_string($this->input('')));
  }
}

///////////////////////////////////////////////////////////////////////////////
// Selecting text.
///////////////////////////////////////////////////////////////////////////////

class SelectText extends SelectValue {
  var $_required;
  var $_min_length;
  var $_max_length;

  function required() { return $this->_required; }

  function min_length() { return $this->_min_length; }

  function max_length() { return $this->_max_length; }

  function focus() { return $this->is_value() ? null : $this->field(); }

  function error_message_missing() { return 'Please enter some text'; }

  function error_message_too_short() {
    return ('Too short: please enter at least ' . $this->_min_length .
            ' characters');
  }

  function error_message_too_long() {
    return ('Too long: please cut it down to ' . $this->_max_length .
            ' characters');
  }

  function validate() {
    parent::validate();

    if (!$this->is_error() && $this->_required && !$this->is_value()) {
      $this->set_error($this->error_message_missing());
    }

    if (!$this->is_error() && $this->is_value() && isset($this->_min_length) &&
        strlen($this->value()) < $this->_min_length) {
      $this->set_error($this->error_message_too_short());
    }

    if (!$this->is_error() && $this->is_value() && isset($this->_max_length) &&
        strlen($this->value()) > $this->_max_length) {
      $this->set_error($this->error_message_too_long());
    }
  }

  function select() {
    $max = (isset($this->_max_length) ? ($this->_max_length + 1) : null);
    return $this->text_input('',$this->value(),$max);
  }

  function SelectText($field,$required,$min,$max) {
    parent::SelectValue($field);

    $this->_required = $required;
    $this->_min_length = $min;
    $this->_max_length = $max;
    $this->set_value(from_string($this->input('')));
  }
}

///////////////////////////////////////////////////////////////////////////////
// Selecting numbers.
///////////////////////////////////////////////////////////////////////////////

class SelectNumber extends SelectText {
  function error_message_missing() { return 'Please enter a number'; }

  function error_message_too_long() {
    return ('Too long: please cut it down to ' . $this->max_length() .
            ' digits');
  }

  function error_message_not_number() {
    return ('Please use only digits [0-9]');
  }

  function validate() {
    if (!$this->is_error()) {
      $v = parent::value();
      if (isset($v) && !ereg('[-~]?[0-9]+',$v)) {
        $this->set_error($this->error_message_not_number());
      }
    }

    parent::validate();
  }

  function value() {
    $v = parent::value();
    if (isset($v)) { return (integer)$v; }
    else { return null; }
  }

  function SelectNumber($field,$required,$max) {
    parent::SelectText($field, $required, null, $max + 1);
  }
}

///////////////////////////////////////////////////////////////////////////////
// Selecting dates.
///////////////////////////////////////////////////////////////////////////////

class SelectDate extends SelectValue {
  var $_select_day;
  var $_select_month;
  var $_select_year;
  var $_required;

  function required() { return $this->_required; }

  function error_message_missing() { return 'Please enter a date'; }

  function error() {
    $e = $this->_error;
    if (!isset($e)) { $e = $this->_select_day->error(); }
    if (!isset($e)) { $e = $this->_select_month->error(); }
    if (!isset($e)) { $e = $this->_select_year->error(); }
    return $e;
  }

  function validate() {
    parent::validate();

    $this->_select_day->validate();
    $this->_select_month->validate();
    $this->_select_year->validate();

    if (!$this->is_error()) {
      $day = $this->_select_day->value();
      $month = $this->_select_month->value();
      $year = $this->_select_year->value();

      if ($this->required() || isset($day) || isset($month) || isset($year)) {
        if (!isset($day)) {
          $this->set_error('Please select a day');
        }
        elseif (!isset($month)) {
          $this->set_error('Please select a month');
        }
        elseif (!isset($year)) {
          $this->set_error('Please enter a year');
        }
        elseif ($year < 100) {
          $this->set_error('Please enter a four digit year');
        }
        else {
          $day = (integer)$day;
          $month = (integer)$month;

          if (!checkdate($month,$day,$year)) {
            $this->set_error('Please enter a valid date');
          }
          else {
            $value = new TimePoint();
            $value->from_array_date(array($year,$month,$day));
            parent::set_value($value);
          }
        }
      }
      else {
        parent::set_value(null);
      }
    }
  }

  function select() {
    return
      $this->_select_day->select() . ' &nbsp; ' .
      $this->_select_month->select() . ' &nbsp; ' .
      $this->_select_year->select();
  }

  function set_value($date) {
    if (isset($date) && $date->is_date_valid()) {
      $year = $date->year();
      $month = (string)($date->month());
      $day = (string)($date->day());
    }
    else {
      $year = null;
      $month = null;
      $day = null;
    }

    $this->_select_year->set_value($year);
    $this->_select_month->set_value($month);
    $this->_select_day->set_value($day);

    parent::set_value($date);
  }

  function SelectDate($field,$required) {
    global $all_months;

    parent::SelectValue($field);

    $days = array();
    for ($d = 1; $d <= 31; ++$d) {
      $days[$d] = $d;
    }

    $this->_required = $required;
    $this->_select_day =
      new SelectList($this->field() . 'd', $days, $required);
    $this->_select_month =
      new SelectList($this->field() . 'm', $all_months, $required);
    $this->_select_year =
      new SelectNumber($this->field() . 'y', $required, 4);
  }
}

///////////////////////////////////////////////////////////////////////////////
// Selecting times.
///////////////////////////////////////////////////////////////////////////////

class SelectTime extends SelectValue {
  var $_select_hour;
  var $_select_minute;
  var $_select_second;
  var $_required;

  function required() { return $this->_required; }

  function error_message_missing() { return 'Please enter a time'; }

  function error() {
    $e = $this->_error;
    if (!isset($e)) { $e = $this->_select_hour->error(); }
    if (!isset($e)) { $e = $this->_select_minute->error(); }
    if (!isset($e)) { $e = $this->_select_second->error(); }
    return $e;
  }

  function validate() {
    parent::validate();

    $this->_select_hour->validate();
    $this->_select_minute->validate();
    $this->_select_second->validate();

    if (!$this->is_error()) {
      $hour = $this->_select_hour->value();
      $minute = $this->_select_minute->value();
      $second = $this->_select_second->value();

      if ($this->required() ||
          isset($hour) || isset($minute) || isset($second)) {
        if (!isset($hour)) {
          $this->set_error('Please enter an hour');
        }
        elseif (!isset($minute)) {
          $this->set_error('Please enter a minute');
        }
        elseif (!isset($second)) {
          $this->set_error('Please enter a second');
        }
        elseif ($hour < 0 || $hour >= 24) {
          $this->set_error('Please enter an hour between 0 and 23');
        }
        elseif ($minute < 0 || $minute >= 60) {
          $this->set_error('Please enter a minute between 0 and 59');
        }
        elseif ($second < 0 || $second >= 60) {
          $this->set_error('Please enter a second between 0 and 59');
        }
        else {
          $value = new TimePoint();
          $value->from_array_time(array($hour,$minute,$second));
          parent::set_value($value);
        }
      }
      else {
        parent::set_value(null);
      }
    }
  }

  function select() {
    return
      $this->_select_hour->select() . ' : ' .
      $this->_select_minute->select() . ' : ' .
      $this->_select_second->select();
  }

  function set_value($time) {
    if (isset($time) && $time->is_time_valid()) {
      $hour = $time->hour();
      $minute = $time->minute();
      $second = $time->second();
    }
    else {
      $hour = null;
      $minute = null;
      $second = null;
    }

    $this->_select_hour->set_value($hour);
    $this->_select_minute->set_value($minute);
    $this->_select_second->set_value($second);

    parent::set_value($time);
  }

  function SelectTime($field,$required) {
    parent::SelectValue($field);

    $this->_required = $required;
    $this->_select_hour =
      new SelectNumber($this->field() . 'h', $required, 2);
    $this->_select_minute =
      new SelectNumber($this->field() . 'm', $required, 2);
    $this->_select_second =
      new SelectNumber($this->field() . 's', $required, 2);
  }
}

///////////////////////////////////////////////////////////////////////////////
// Selecting URLs.
///////////////////////////////////////////////////////////////////////////////

class SelectURL extends SelectText {
  function error_message_missing() { return 'Please enter a URL'; }

  function error_message_too_long() {
    return ('Too long: please cut it down to ' . $this->max_length() .
            ' characters');
  }

  function error_message_not_url() {
    return ('Please begin the URL with <em>http://</em>');
  }

  function validate() {
    if (!$this->is_error()) {
      $v = parent::value();
      if (isset($v) && !ereg('^http://',$v)) {
        $this->set_error($this->error_message_not_url());
      }
    }

    parent::validate();
  }

  function SelectURL($field,$required,$max) {
    parent::SelectText($field, $required, null, $max);
  }
}

///////////////////////////////////////////////////////////////////////////////
// Selecting files.
///////////////////////////////////////////////////////////////////////////////

class SelectFile extends SelectValue {
  var $_required;

  function required() { return $this->_required; }

  function focus() { return $this->is_value() ? null : $this->field(); }

  function error_message_missing() { return 'Please enter a filename'; }

  function validate() {
    parent::validate();

    if (!$this->is_error() && $this->_required && !$this->is_value()) {
      $this->set_error($this->error_message_missing());
    }
  }

  function set_value($value) {
    trigger_error('can\'t set a file value');
  }

  function select() {
    return $this->file_input('');
  }

  function SelectFile($field,$required) {
    parent::SelectValue($field);

    $this->_required = $required;

    $value = null;

    if (array_key_exists($field,$_FILES)) {
      $value =
        array('client' => $_FILES[$field]['name'],
              'server' => $_FILES[$field]['tmp_name']);
    }

    parent::set_value($value);
  }
}

///////////////////////////////////////////////////////////////////////////////
// Site forms.
///////////////////////////////////////////////////////////////////////////////

function site_form($path, $text, $args = null, $method = 'get', $atts = null) {
  is_array($path) or trigger_error('bad path');
  is_string($text) or trigger_error('bad text');
  if (!isset($args)) { $args = array(); }
  is_array($args) or trigger_error('bad args');

  $url = site_url($path,null);

  if (strcmp(strtolower($method),'get') == 0) {
    $enctype = 'application/x-www-form-urlencoded';
  }
  elseif (strcmp(strtolower($method),'post') == 0) {
    $enctype = 'multipart/form-data';
  }
  else {
    trigger_error('bad method');
  }

  $atts_text = attributes_to_html($atts);

  $form_text = '';

  foreach ($args as $arg => $val) {
    is_string($arg) or trigger_error('bad arg');
    $form_text .= hidden_input($arg,$val);
  }

  $form_text .= $text;

  return
    '<form action="' . string_to_html($url) . '" method="' . $method .
    '" enctype="' . $enctype . '"' . $atts_text . '>' .
    $form_text . '</form>';
}

?>
