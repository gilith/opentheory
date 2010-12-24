<?php

///////////////////////////////////////////////////////////////////////////////
//
// DATES
//
// Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2
//
///////////////////////////////////////////////////////////////////////////////

require_once 'global.php';
require_once 'error.php';
require_once 'functions.php';

///////////////////////////////////////////////////////////////////////////////
// Date functions.
///////////////////////////////////////////////////////////////////////////////

$all_days_of_the_week =
  array('Monday','Tuesday','Wednesday','Thursday','Friday',
        'Saturday','Sunday');

$all_months =
  array(1 => 'January', 2 => 'February', 3 => 'March', 4 => 'April',
        5 => 'May', 6 => 'June', 7 => 'July', 8 => 'August',
        9 => 'September', 10 => 'October', 11 => 'November', 12 => 'December');

$all_short_months = array();
foreach ($all_months as $id => $month) {
  $all_short_months[$id] = substr($month,0,3);
}

function days_in_month($month,$year) {
  is_int($month) or trigger_error('bad month');
  is_int($year) or trigger_error('bad year');

  if ($month == 1) { return 31; }
  elseif ($month == 2) {
    $leap = (($year % 4 == 0) && (($year % 100 != 0) || ($year % 400 == 0)));
    return $leap ? 29 : 28;
  }
  elseif ($month == 3) { return 31; }
  elseif ($month == 4) { return 30; }
  elseif ($month == 5) { return 31; }
  elseif ($month == 6) { return 30; }
  elseif ($month == 7) { return 31; }
  elseif ($month == 8) { return 31; }
  elseif ($month == 9) { return 30; }
  elseif ($month == 10) { return 31; }
  elseif ($month == 11) { return 30; }
  elseif ($month == 12) { return 31; }
  else { trigger_error('invalid month'); }
}

///////////////////////////////////////////////////////////////////////////////
// Date class.
///////////////////////////////////////////////////////////////////////////////

class TimePoint {
  var $_year;  // any integer
  var $_month;  // 1..12
  var $_day;  // 1..31
  var $_hour;  // 0..23
  var $_minute;  // 0..59
  var $_second;  // 0..60

  function is_date_valid() { return isset($this->_year); }

  function check_date_valid() {
    if (!$this->is_date_valid()) { trigger_error('invalid date'); }
  }

  function is_time_valid() { return isset($this->_hour); }

  function check_time_valid() {
    if (!$this->is_time_valid()) { trigger_error('invalid time'); }
  }

  function is_valid() {
    return $this->is_date_valid() && $this->is_time_valid();
  }

  function check_valid() {
    $this->check_date_valid();
    $this->check_time_valid();
  }

  function year() {
    $this->check_date_valid();
    return $this->_year;
  }

  function month() {
    $this->check_date_valid();
    return $this->_month;
  }

  function day() {
    $this->check_date_valid();
    return $this->_day;
  }

  function hour() {
    $this->check_time_valid();
    return $this->_hour;
  }

  function minute() {
    $this->check_time_valid();
    return $this->_minute;
  }

  function second() {
    $this->check_time_valid();
    return $this->_second;
  }

  // Returns a number in the range {1, ..., 366}

  function day_of_year() {
    $this->check_date_valid();
    $year = $this->year();
    $month = $this->month();
    $day = $this->day();

    $d = $day;

    while ($month > 1) {
      --$month;
      $d += days_in_month($month,$year);
    }

    return $d;
  }

  // Returns the number of days since 1 January 1970

  function days_since_epoch() {
    $this->check_date_valid();

    // The number of days between 31 Dec 0000 and 1 Jan 1970
    $d = -719163;

    // Normalize to the year (i.e., 6 May 1986 -> 31 Dec 1985)
    $d += $this->day_of_year();
    $year = $this->year() - 1;

    // Normalize to the century (i.e., 31 Dec 1985 -> 31 Dec 1900)
    $yoc = $year % 100;
    $d += ($yoc % 4) * 365 + ((integer)($yoc / 4)) * 1461;

    // Normalize to the quad-century (i.e., 31 Dec 1900 -> 31 Dec 1600)
    $century = (integer)($year / 100);
    $d += ($century % 4) * 36524;

    // Normalize to 31 Dec 0000
    $quad_centuries = (integer)($century / 4);
    $d += $quad_centuries * 146097;

    return $d;
  }

  // Returns a number in the range {0, ..., 6}

  function day_of_the_week() {
    // The 3 below is because 1 January 1970 was a Thursday
    $d = 3 + $this->days_since_epoch();
    if ($d < 0) { $d = (-$d) % 7; if ($d != 0) { $d = 7 - $d; } }
    else { $d = $d % 7; }
    return $d;
  }

  function compare($date, $null_at_beginning = true) {
    $d1 = $this->is_date_valid();
    $d2 = $date->is_date_valid();

    if ($d1) {
      if ($d2) {
        $c = int_cmp($this->year(),$date->year());
        if ($c != 0) { return $c; }

        $c = int_cmp($this->month(),$date->month());
        if ($c != 0) { return $c; }

        $c = int_cmp($this->day(),$date->day());
        if ($c != 0) { return $c; }
      }
      else {
        return ($null_at_beginning ? 1 : -1);
      }
    }
    else {
      if ($d2) {
        return ($null_at_beginning ? -1 : 1);
      }
    }

    $t1 = $this->is_time_valid();
    $t2 = $date->is_time_valid();

    if ($t1) {
      if ($t2) {
        $c = int_cmp($this->hour(),$date->hour());
        if ($c != 0) { return $c; }

        $c = int_cmp($this->minute(),$date->minute());
        if ($c != 0) { return $c; }

        $c = int_cmp($this->second(),$date->second());
        if ($c != 0) { return $c; }
      }
      else {
        return ($null_at_beginning ? 1 : -1);
      }
    }
    else {
      if ($t2) {
        return ($null_at_beginning ? -1 : 1);
      }
    }

    return 0;
  }

  function is_before($date, $null_at_beginning = true) {
    return ($this->compare($date, $null_at_beginning) < 0);
  }

  function equal($date, $null_at_beginning = true) {
    return ($this->compare($date, $null_at_beginning) == 0);
  }

  function is_after($date, $null_at_beginning = true) {
    return ($this->compare($date, $null_at_beginning) > 0);
  }

  function is_past($null_at_beginning = true) {
    return $this->is_before(server_datetime(), $null_at_beginning);
  }

  function is_now($null_at_beginning = true) {
    return $this->equal(server_datetime(), $null_at_beginning);
  }

  function is_future($null_at_beginning = true) {
    return $this->is_after(server_datetime(), $null_at_beginning);
  }

  function set_previous_year($num = 1) {
    is_int($num) or trigger_error('bad num');
    $this->check_date_valid();

    if ($num < 0) {
      $this->set_next_year(-$num);
    }
    elseif ($num > 0) {
      $this->_year -= $num;

      if ($this->_day > 28) {
        $days = days_in_month($this->_month,$this->_year);
        if ($this->_day > $days) { $this->_day = $days; }
      }
    }
  }

  function set_next_year($num = 1) {
    is_int($num) or trigger_error('bad num');
    $this->check_date_valid();

    if ($num < 0) {
      $this->set_previous_year(-$num);
    }
    elseif ($num > 0) {
      $this->_year += $num;

      if ($this->_day > 28) {
        $days = days_in_month($this->_month,$this->_year);
        if ($this->_day > $days) { $this->_day = $days; }
      }
    }
  }

  function set_previous_month($num = 1) {
    is_int($num) or trigger_error('bad num');
    $this->check_date_valid();

    if ($num < 0) {
      $this->set_next_month(-$num);
    }
    elseif ($num > 0) {
      $this->_month -= $num;
      while ($this->_month < 1) {
        $this->_month += 12;
        --$this->_year;
      }

      if ($this->_day > 28) {
        $days = days_in_month($this->_month,$this->_year);
        if ($this->_day > $days) { $this->_day = $days; }
      }
    }
  }

  function set_next_month($num = 1) {
    is_int($num) or trigger_error('bad num');
    $this->check_date_valid();

    if ($num < 0) {
      $this->set_previous_month(-$num);
    }
    elseif ($num > 0) {
      $this->_month += $num;
      while ($this->_month > 12) {
        $this->_month -= 12;
        ++$this->_year;
      }

      if ($this->_day > 28) {
        $days = days_in_month($this->_month,$this->_year);
        if ($this->_day > $days) { $this->_day = $days; }
      }
    }
  }

  function set_previous_day($num = 1) {
    is_int($num) or trigger_error('bad num');
    $this->check_date_valid();

    if ($num < 0) {
      $this->set_next_day(-$num);
    }
    elseif ($num > 0) {
      $this->_day -= $num;
      while ($this->_day < 1) {
        --$this->_month;
        if ($this->_month < 1) { $this->_month = 12; --$this->_year; }
        $this->_day += days_in_month($this->_month,$this->_year);
      }
    }
  }

  function set_next_day($num = 1) {
    is_int($num) or trigger_error('bad num');
    $this->check_date_valid();

    if ($num < 0) {
      $this->set_previous_day(-$num);
    }
    elseif ($num > 0) {
      $this->_day += $num;
      $finished = false;
      while (!$finished && 28 < $this->_day) {
        $days = days_in_month($this->_month,$this->_year);
        if ($this->_day <= $days) {
          $finished = true;
        }
        else {
          $this->_day -= $days;
          ++$this->_month;
          if ($this->_month > 12) { $this->_month = 1; ++$this->_year; }
        }
      }
    }
  }

  function set_previous_week($num = 1) {
    is_int($num) or trigger_error('bad num');
    $this->check_date_valid();
    return $this->set_previous_day($num * 7);
  }

  function set_next_week($num = 1) {
    is_int($num) or trigger_error('bad num');
    $this->check_date_valid();
    return $this->set_next_day($num * 7);
  }

  function set_previous_hour($num = 1) {
    is_int($num) or trigger_error('bad num');
    $this->check_time_valid();

    if ($num < 0) {
      $this->set_next_hour(-$num);
    }
    elseif ($num > 0) {
      $this->_hour -= $num;

      $days = 0;
      while ($this->_hour < 0) {
        $this->_hour += 24;
        ++$days;
      }

      if ($days > 0 && $this->is_date_valid()) {
        $this->set_previous_day($days);
      }
    }
  }

  function set_next_hour($num = 1) {
    is_int($num) or trigger_error('bad num');
    $this->check_time_valid();

    if ($num < 0) {
      $this->set_previous_hour(-$num);
    }
    elseif ($num > 0) {
      $this->_hour += $num;

      $days = 0;
      while ($this->_hour > 23) {
        $this->_hour -= 24;
        ++$days;
      }

      if ($days > 0 && $this->is_date_valid()) {
        $this->set_next_day($days);
      }
    }
  }

  function set_previous_minute($num = 1) {
    is_int($num) or trigger_error('bad num');
    $this->check_time_valid();

    if ($num < 0) {
      $this->set_next_minute(-$num);
    }
    elseif ($num > 0) {
      $this->_minute -= ($num % 60);

      $num = (integer)($num / 60);

      if ($this->_minute < 0) {
        $this->_minute += 60;
        $num++;
      }

      $this->set_previous_hour($num);
    }
  }

  function set_next_minute($num = 1) {
    is_int($num) or trigger_error('bad num');
    $this->check_time_valid();

    if ($num < 0) {
      $this->set_previous_minute(-$num);
    }
    elseif ($num > 0) {
      $this->_minute += ($num % 60);

      $num = (integer)($num / 60);

      if ($this->_minute >= 60) {
        $this->_minute -= 60;
        $num++;
      }

      $this->set_next_hour($num);
    }
  }

  function set_previous_second($num = 1) {
    is_int($num) or trigger_error('bad num');
    $this->check_time_valid();

    if ($num < 0) {
      $this->set_next_second(-$num);
    }
    elseif ($num > 0) {
      $this->_second -= ($num % 60);

      $num = (integer)($num / 60);

      if ($this->_second < 0) {
        $this->_second += 60;
        $num++;
      }

      $this->set_previous_minute($num);
    }
  }

  function set_next_second($num = 1) {
    is_int($num) or trigger_error('bad num');
    $this->check_time_valid();

    if ($num < 0) {
      $this->set_previous_second(-$num);
    }
    elseif ($num > 0) {
      $this->_second += ($num % 60);

      $num = (integer)($num / 60);

      if ($this->_second >= 60) {
        $this->_second -= 60;
        $num++;
      }

      $this->set_next_minute($num);
    }
  }

  function previous_year($num = 1) {
    is_int($num) or trigger_error('bad num');
    $this->check_date_valid();

    $ret = $this->copy();
    $ret->set_previous_year($num);

    return $ret;
  }

  function next_year($num = 1) {
    is_int($num) or trigger_error('bad num');
    $this->check_date_valid();

    $ret = $this->copy();
    $ret->set_next_year($num);

    return $ret;
  }

  function previous_month($num = 1) {
    is_int($num) or trigger_error('bad num');
    $this->check_date_valid();

    $ret = $this->copy();
    $ret->set_previous_month($num);

    return $ret;
  }

  function next_month($num = 1) {
    is_int($num) or trigger_error('bad num');
    $this->check_date_valid();

    $ret = $this->copy();
    $ret->set_next_month($num);

    return $ret;
  }

  function previous_week($num = 1) {
    is_int($num) or trigger_error('bad num');
    $this->check_date_valid();

    $ret = $this->copy();
    $ret->set_previous_week($num);

    return $ret;
  }

  function next_week($num = 1) {
    is_int($num) or trigger_error('bad num');
    $this->check_date_valid();

    $ret = $this->copy();
    $ret->set_next_week($num);

    return $ret;
  }

  function previous_day($num = 1) {
    is_int($num) or trigger_error('bad num');
    $this->check_date_valid();

    $ret = $this->copy();
    $ret->set_previous_day($num);

    return $ret;
  }

  function next_day($num = 1) {
    is_int($num) or trigger_error('bad num');
    $this->check_date_valid();

    $ret = $this->copy();
    $ret->set_next_day($num);

    return $ret;
  }

  function yesterday() {
    $this->check_date_valid();
    return $this->previous_day();
  }

  function tomorrow() {
    $this->check_date_valid();
    return $this->next_day();
  }

  function previous_hour($num = 1) {
    is_int($num) or trigger_error('bad num');
    $this->check_time_valid();

    $ret = $this->copy();
    $ret->set_previous_hour($num);

    return $ret;
  }

  function next_hour($num = 1) {
    is_int($num) or trigger_error('bad num');
    $this->check_time_valid();

    $ret = $this->copy();

    $ret->set_next_hour($num);

    return $ret;
  }

  function previous_minute($num = 1) {
    is_int($num) or trigger_error('bad num');
    $this->check_time_valid();

    $ret = $this->copy();
    $ret->set_previous_minute($num);

    return $ret;
  }

  function next_minute($num = 1) {
    is_int($num) or trigger_error('bad num');
    $this->check_time_valid();

    $ret = $this->copy();

    $ret->set_next_minute($num);

    return $ret;
  }

  function previous_second($num = 1) {
    is_int($num) or trigger_error('bad num');
    $this->check_time_valid();

    $ret = $this->copy();
    $ret->set_previous_second($num);

    return $ret;
  }

  function next_second($num = 1) {
    is_int($num) or trigger_error('bad num');
    $this->check_time_valid();

    $ret = $this->copy();

    $ret->set_next_second($num);

    return $ret;
  }

  function from_array_date($date, $preserve_time = false) {
    is_array($date) or trigger_error('bad date');

    $this->_year = $date[0];
    $this->_month = $date[1];
    $this->_day = $date[2];

    if (!$preserve_time) {
      $this->_hour = null;
      $this->_minute = null;
      $this->_second = null;
    }
  }

  function from_array_time($time, $preserve_date = false) {
    is_array($time) or trigger_error('bad time');

    $this->_hour = $time[0];
    $this->_minute = $time[1];
    $this->_second = $time[2];

    if (!$preserve_date) {
      $this->_year = null;
      $this->_month = null;
      $this->_day = null;
    }
  }

  function to_string_date() {
    global $all_short_months;
    $this->check_date_valid();
    return $this->_day . ' ' . $all_short_months[$this->_month] . ' ' .
           $this->_year;
  }

  function to_string_time() {
    $this->check_time_valid();
    return sprintf('%02d:%02d:%02d',
                   $this->hour(), $this->minute(), $this->second());
  }

  function from_string_date($date, $preserve_time = false) {
    global $all_short_months;

    is_string($date) or trigger_error('bad date');

    $date_regexp =
      '^([0-9][0-9]?)' . ' *' .
      '(' . implode('|', $all_short_months) . ')' . ' *' .
      '([12][0-9]{3})$';

    if (!eregi($date_regexp, $date, $arr)) {
      trigger_error('invalid date format');
    }

    $this->_day = (integer)$arr[1];
    $month = $arr[2];
    $this->_year = (integer)$arr[3];

    $this->_month = array_search($month,$all_short_months);
    if ($this->_month === false) { trigger_error('month bug'); }

    if (!$preserve_time) {
      $this->_hour = null;
      $this->_minute = null;
      $this->_second = null;
    }
  }

  function to_verbose_string_date() {
    global $all_months;
    global $all_days_of_the_week;

    $this->check_date_valid();

    $d = $this->day_of_the_week();

    return
      $all_days_of_the_week[$d] . ' ' . $this->_day . ' ' .
      $all_months[$this->_month] . ' ' . $this->_year;
  }

  function to_string_date_range($end, $html = true) {
    global $all_short_months;

    isset($end) or trigger_error('bad end');
    is_bool($html) or trigger_error('bad html');

    $this->check_date_valid();
    $end->check_date_valid();

    $ret = $end->to_string_date();

    if ($this->year() != $end->year()) {
      $ret = $this->to_string_date() . ' to ' . $ret;
    }
    elseif ($this->month() != $end->month()) {
      $ret =
        $this->day() . ' ' . $all_short_months[$this->month()] . ' to ' . $ret;
    }
    elseif ($this->day() != $end->day()) {
      $ret = $this->day() . ($html ? '&ndash;' : '-') . $ret;
    }

    return $ret;
  }

  function to_database_date() {
    $this->check_date_valid();
    return sprintf('%04d-%02d-%02d',
                   $this->year(), $this->month(), $this->day());
  }

  function from_database_date($date, $preserve_time = false) {
    is_string($date) or trigger_error('bad date');

    if (!ereg('^([0-9]{4})-([0-9]{2})-([0-9]{2})$', $date, $arr)) {
      trigger_error('invalid date format');
    }

    $this->_year = (integer)$arr[1];
    $this->_month = (integer)$arr[2];
    $this->_day = (integer)$arr[3];

    if (!$preserve_time) {
      $this->_hour = null;
      $this->_minute = null;
      $this->_second = null;
    }
  }

  function to_database_time() {
    $this->check_time_valid();
    return sprintf('%02d:%02d:%02d',
                   $this->hour(), $this->minute(), $this->second());
  }

  function from_database_time($time, $preserve_date = false) {
    is_string($time) or trigger_error('bad time');

    if (!ereg('^([0-9]{2}):([0-9]{2}):([0-9]{2})$', $time, $arr)) {
      trigger_error('invalid time format');
    }

    $this->_hour = (integer)$arr[1];
    $this->_minute = (integer)$arr[2];
    $this->_second = (integer)$arr[3];

    if (!$preserve_date) {
      $this->_year = null;
      $this->_month = null;
      $this->_day = null;
    }
  }

  function to_database_datetime() {
    $this->check_valid();
    return $this->to_database_date() . ' ' . $this->to_database_time();
  }

  function from_database_datetime($datetime) {
    is_string($datetime) or trigger_error('bad datetime');

    if (!ereg('^([0-9]{4}-[0-9]{2}-[0-9]{2}) ' .
              '([0-9]{2}:[0-9]{2}:[0-9]{2})$', $datetime, $arr)) {
      trigger_error('invalid datetime format');
    }

    $this->from_database_date($arr[1]);
    $this->from_database_time($arr[2], true);
  }

  function from_photo_datetime($datetime) {
    is_string($datetime) or trigger_error('bad datetime');

    if (!ereg('^([0-9]{4}):([0-9]{2}):([0-9]{2}) ' .
              '([0-9]{2}):([0-9]{2}):([0-9]{2})$', $datetime, $arr)) {
      trigger_error('invalid datetime format');
    }

    $this->_year = (integer)$arr[1];
    $this->_month = (integer)$arr[2];
    $this->_day = (integer)$arr[3];
    $this->_hour = (integer)$arr[4];
    $this->_minute = (integer)$arr[5];
    $this->_second = (integer)$arr[6];
  }

  function to_datetime() {
    $this->check_date_valid();

    if ($this->is_time_valid()) {
      return strtotime($this->to_database_datetime());
    }
    else {
      return strtotime($this->to_database_date());
    }
  }

  function from_datetime($time) {
    is_int($time) or trigger_error('bad time');

    $s = date('Y-m-d H:i:s', $time);

    $this->from_database_datetime($s);
  }

  function set_date($src, $preserve_time = false) {
    $this->_year = $src->_year;
    $this->_month = $src->_month;
    $this->_day = $src->_day;

    if (!$preserve_time) {
      $this->_hour = null;
      $this->_minute = null;
      $this->_second = null;
    }
  }

  function set_time($src, $preserve_date = false) {
    if (!$preserve_date) {
      $this->_year = null;
      $this->_month = null;
      $this->_day = null;
    }

    $this->_hour = $src->_hour;
    $this->_minute = $src->_minute;
    $this->_second = $src->_second;
  }

  function set($src) {
    $this->set_date($src);
    $this->set_time($src,true);
  }

  function decrease_to($datetime, $null_at_beginning = true) {
    isset($datetime) or trigger_error('bad datetime');

    if ($this->is_after($datetime, $null_at_beginning)) {
      $this->set($datetime);
    }
  }

  function increase_to($datetime, $null_at_beginning = true) {
    isset($datetime) or trigger_error('bad datetime');

    if ($this->is_before($datetime, $null_at_beginning)) {
      $this->set($datetime);
    }
  }

  function copy() {
    $ret = new TimePoint();
    $ret->set($this);
    return $ret;
  }

  function TimePoint() {
    $this->_year = null;
    $this->_month = null;
    $this->_day = null;
    $this->_hour = null;
    $this->_minute = null;
    $this->_second = null;
  }
}

///////////////////////////////////////////////////////////////////////////////
// Server time.
///////////////////////////////////////////////////////////////////////////////

$global_server_datetime = null;

function server_datetime() {
  global $global_server_datetime;

  if (!isset($global_server_datetime)) {
    $global_server_datetime = new TimePoint();
    $global_server_datetime->from_datetime(time());
  }

  return $global_server_datetime;
}

///////////////////////////////////////////////////////////////////////////////
// Parsing.
///////////////////////////////////////////////////////////////////////////////

function date_from_string($s) {
  $s = from_string($s);
  if (!isset($s)) { return null; }
  $date = new TimePoint();
  $date->from_string($s);
  return $date;
}

///////////////////////////////////////////////////////////////////////////////
// Comparing dates.
///////////////////////////////////////////////////////////////////////////////

function datetime_cmp($date1,$date2) {
  return $date1->compare($date2);
}

///////////////////////////////////////////////////////////////////////////////
// Time intervals.
///////////////////////////////////////////////////////////////////////////////

class TimeInterval {
  var $_seconds;

  function seconds() { return $_seconds; }

  function negate() {
    return new TimeInterval(-$this->seconds());
  }

  function to_string($abbrev = false) {
    $t = $this->seconds();

    if ($t < 0) {
      $n = new TimeInterval(-$t);
      return '-' . $n->to_string();
    }

    if ($t < 0.1) {
      $n = (integer)($t * 1000.0);
      return $n . ($abbrev ? 'ms' : (' millisecond' . (($n == 1) ? '' : 's')));
    }

    if ($t < 1.0) {
      $s = sprintf('%.2f',$t);
      $s = ereg_replace('0$','',$s);
      $s = ereg_replace('.0$','',$s);
      $s .= ($abbrev ? 's' : (' second' . ((strcmp($s,'1') == 0) ? '' : 's')));
      return $s;
    }

    if ($t < 10.0) {
      $s = sprintf('%.1f',$t);
      $s = ereg_replace('.0$','',$s);
      $s .= ($abbrev ? 's' : (' second' . ((strcmp($s,'1') == 0) ? '' : 's')));
      return $s;
    }

    if ($t < 120.0) {
      $n = (integer)$t;
      return $n . ($abbrev ? 's' : (' second' . (($n == 1) ? '' : 's')));
    }

    $t = $t / 60.0;  // Minutes

    if ($t < 120.0) {
      $n = (integer)$t;
      return $n . ($abbrev ? 'm' : (' minute' . (($n == 1) ? '' : 's')));
    }

    $t = $t / 60.0;  // Hours

    if ($t < 48.0) {
      $n = (integer)$t;
      return $n . ($abbrev ? 'h' : (' hour' . (($n == 1) ? '' : 's')));
    }

    $t = $t / 24.0;  // Days

    if ($t < 14.0) {
      $n = (integer)$t;
      return $n . ' day' . (($n == 1) ? '' : 's');
    }

    $days = $t;

    $t = $days / 7.0;  // Weeks

    if ($t < 9.0) {
      $n = (integer)$t;
      return $n . ' week' . (($n == 1) ? '' : 's');
    }

    $t = $days / (365.25 / 12.0);  // Months(ish)

    if ($t < 24.0) {
      $n = (integer)$t;
      return $n . ' month' . (($n == 1) ? '' : 's');
    }

    $t = $days / 365.25;  // Years

    $n = (integer)$t;
    return $n . ' year' . (($n == 1) ? '' : 's');
  }

  function TimeInterval($t) {
    is_int($t) or is_float($t) or trigger_error('bad t');

    $this->_seconds = (float)$t;
  }
}

?>
