<?php

///////////////////////////////////////////////////////////////////////////////
//
// UTILITY FUNCTIONS
//
// Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2
//
///////////////////////////////////////////////////////////////////////////////

require_once 'global.php';
require_once 'error.php';

///////////////////////////////////////////////////////////////////////////////
// Basic functions
///////////////////////////////////////////////////////////////////////////////

function bool_cmp($b1,$b2) {
  if (!$b1 && $b2) { return -1; }
  elseif ($b1 && !$b2) { return 1; }
  else { return 0; }
}

function int_cmp($n1,$n2) {
  if ($n1 < $n2) { return -1; }
  elseif ($n1 > $n2) { return 1; }
  else { return 0; }
}

function deep_copy($value) {
  if (is_array($value)) {
    $result = array();
    foreach ($value as $key => $val) {
      $result[$key] = deep_copy($val);
    }
  }
  else {
    $result = $value;
  }

  return $result;
}

function round_to_percent($x) { return (integer)($x * 100.0 + 0.5) / 100.0; }

function is_prefix_string($s1,$s2) {
  is_string($s1) or trigger_error('bad s1');
  is_string($s2) or trigger_error('bad s2');

  $l1 = strlen($s1);
  $l2 = strlen($s2);

  if ($l1 > $l2) { return false; }

  if ($l1 == $l2) {
    $s = $s2;
  }
  else {
    $s = substr($s2,0,$l1);
  }

  return (strcmp($s,$s1) == 0);
}

///////////////////////////////////////////////////////////////////////////////
// Pretty printing.
///////////////////////////////////////////////////////////////////////////////

function pretty_number($n) {
  isset($n) or trigger_error('pretty_number: null input');

  if ($n < 0) {
    return '-' . pretty_number(-$n);
  }
  elseif ($n < 1000) {
    return $n;
  }
  else {
    return
      pretty_number((int)($n / 1000)) . ','
      . str_pad($n % 1000, 3, '0', STR_PAD_LEFT);
  }
}
function number_to_words($n) {
  isset($n) or trigger_error('number_to_words: null input');

  if ($n == 0) { $s = 'zero'; }
  elseif ($n == 1) { $s = 'one'; }
  elseif ($n == 2) { $s = 'two'; }
  elseif ($n == 3) { $s = 'three'; }
  elseif ($n == 4) { $s = 'four'; }
  elseif ($n == 5) { $s = 'five'; }
  elseif ($n == 6) { $s = 'six'; }
  elseif ($n == 7) { $s = 'seven'; }
  elseif ($n == 8) { $s = 'eight'; }
  elseif ($n == 9) { $s = 'nine'; }
  elseif ($n == 10) { $s = 'ten'; }
  elseif ($n == 11) { $s = 'eleven'; }
  elseif ($n == 12) { $s = 'twelve'; }
  elseif ($n == 13) { $s = 'thirteen'; }
  elseif ($n == 14) { $s = 'fourteen'; }
  elseif ($n == 15) { $s = 'fifteen'; }
  elseif ($n == 16) { $s = 'sixteen'; }
  elseif ($n == 17) { $s = 'seventeen'; }
  elseif ($n == 18) { $s = 'eighteen'; }
  elseif ($n == 19) { $s = 'nineteen'; }
  elseif ($n == 20) { $s = 'twenty'; }
  else { trigger_error('number_to_words: too large'); }

  return $s;
}

function pretty_ordinal($n) {
  isset($n) or trigger_error('pretty_ordinal: null input');

  if (($n % 100) == 11) { $s = 'th'; }
  elseif (($n % 100) == 12) { $s = 'th'; }
  elseif (($n % 100) == 13) { $s = 'th'; }
  elseif (($n % 10) == 1) { $s = 'st'; }
  elseif (($n % 10) == 2) { $s = 'nd'; }
  elseif (($n % 10) == 3) { $s = 'rd'; }
  else { $s = 'th'; }

  return (pretty_number($n) . $s);
}

function pretty_times($n) {
  is_int($n) or trigger_error('bad n');

  if ($n == 1) {
    return 'once';
  }
  elseif ($n == 2) {
    return 'twice';
  }
  else {
    return pretty_number($n) . ' times';
  }
}

function pretty_percentage($x) {
  isset($x) or trigger_error('pretty_percentage: null');
  $n = (integer)($x * 100.0 + 0.5);
  return $n . '%';
}

function pretty_prob($prob) {
  isset($prob) or trigger_error('null prob');
  return pretty_percentage(round_to_percent($prob));
}

function pretty_list($list) {
  isset($list) or trigger_error('pretty_list: null');
  is_array($list) or trigger_error('pretty_list: not a list');
  (0 < count($list)) or trigger_error('pretty_list: zero length');

  $first_item = true;
  foreach ($list as $item) {
    if ($first_item) {
      $optimist = $item;
      $pessimist = $item;
      $first_item = false;
    }
    else {
      $optimist = $pessimist . ' and ' . $item;
      $pessimist .= ', ' . $item;
    }
  }

  return $optimist;
}

///////////////////////////////////////////////////////////////////////////////
// Parsing.
///////////////////////////////////////////////////////////////////////////////

function from_string($s) {
  if (!isset($s)) { return null; }
  $s = trim($s);
  if (strcmp($s,'') == 0) { return null; }
  $s = ereg_replace('  +',' ',$s);
  return $s;
}

function bool_from_string($s) {
  return is_string($s) && strcmp($s,'1') == 0;
}

function int_from_string($s) {
  $s = from_string($s);
  if (!isset($s)) { return null; }
  else { return (integer)$s; }
}

function float_from_string($s) {
  $s = from_string($s);
  if (!isset($s)) { return null; }
  else { return (float)$s; }
}

function percentage_from_string($s) {
  $s = float_from_string($s);
  if (!isset($s)) { return null; }
  else { return $s / 100.0; }
}

///////////////////////////////////////////////////////////////////////////////
// HTML functions.
///////////////////////////////////////////////////////////////////////////////

function string_to_url($s) {
  $s = ereg_replace('%','%25',$s);  // Must go first in the list

  $s = ereg_replace('#','%23',$s);
  $s = ereg_replace('\$','%24',$s);
  $s = ereg_replace('&','%26',$s);
  $s = ereg_replace('\'','%27',$s);
  $s = ereg_replace('\+','%2B',$s);
  $s = ereg_replace('/','%2F',$s);
  $s = ereg_replace(':','%3A',$s);
  $s = ereg_replace(';','%3B',$s);
  $s = ereg_replace('<','%3C',$s);
  $s = ereg_replace('=','%3D',$s);
  $s = ereg_replace('>','%3E',$s);
  $s = ereg_replace('\?','%3F',$s);
  $s = ereg_replace('@','%40',$s);
  $s = ereg_replace('\[','%5B',$s);
  $s = ereg_replace('\\\\','%5C',$s);
  $s = ereg_replace('\]','%5D',$s);
  $s = ereg_replace('\^','%5E',$s);
  $s = ereg_replace('`','%60',$s);
  $s = ereg_replace('\{','%7B',$s);
  $s = ereg_replace('\|','%7C',$s);
  $s = ereg_replace('\}','%7D',$s);
  $s = ereg_replace('~','%7E',$s);

  $s = ereg_replace(' ','+',$s);  // Must go last in the list

  return $s;
}

function string_to_html($s) {
  $s = ereg_replace('&','&amp;',$s);
  $s = ereg_replace('<','&lt;',$s);
  $s = ereg_replace('>','&gt;',$s);
  return $s;
}

function attributes_to_html($atts = null) {
  if (!isset($atts)) { $atts = array(); }
  is_array($atts) or trigger_error('bad atts');

  $atts_text = '';
  foreach ($atts as $att => $value) {
    $atts_text .=
      ' ' . string_to_html($att) . '="' . string_to_html($value) . '"';
  }

  return $atts_text;
}

function string_to_html_with_linebreaks($s,$max) {
  $len = strlen($s);

  if (!isset($max) || $len <= $max) {
    $s = string_to_html($s);
    $s = ereg_replace(' ','&nbsp;',$s);
    return $s;
  }

  $chunks = explode(' ', $s);
  $r = $len;
  $l = 0;
  $s = '';
  foreach ($chunks as $chunk) {
    $c = strlen($chunk);
    if (strcmp($s,'') != 0) {
      --$r;
      if ($l + 1 + $c > $max || ($l + 1 + $r > $max && $l + 1 + $c > $r)) {
        $s .= ' ';
        $l = 0;
      }
      else {
        $s .= '&nbsp;';
        ++$l;
      }
    }
    $s .= string_to_html($chunk);
    $l += $c;
    $r -= $c;
  }

  return $s;
}

function span_text($c,$s) {
  is_string($c) or trigger_error('bad c');
  is_string($s) or trigger_error('bad s');
  return '<span class="' . $c . '">' . $s . '</span>';
}

function error_text($s) {
  is_string($s) or trigger_error('bad s');
  return span_text('error',$s);
}

function form_error_text($s) {
  is_string($s) or trigger_error('bad s');
  return span_text('form_error',$s);
}

function field_text($s) {
  is_string($s) or trigger_error('bad s');
  return span_text('field',$s);
}

function definition_text($s) {
  is_string($s) or trigger_error('bad s');
  return span_text('definition',$s);
}

function required_mark() {
  return span_text('form_required','*');
}

?>
