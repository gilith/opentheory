<?php

///////////////////////////////////////////////////////////////////////////////
//
// SESSION TABLE
//
// Copyright (c) 2008 Joe Hurd, distributed under the MIT license
//
///////////////////////////////////////////////////////////////////////////////

require_once 'global.php';
require_once 'error.php';
require_once 'date.php';
require_once 'input.php';
require_once 'database.php';
require_once 'user.php';

///////////////////////////////////////////////////////////////////////////////
// A class to store session information.
///////////////////////////////////////////////////////////////////////////////

class Session {
  var $_id;
  var $_user;
  var $_expires;
  var $_mobile;

  function id() { return $this->_id; }

  function user() { return $this->_user; }

  function expires() { return $this->_expires; }

  function mobile() { return $this->_mobile; }

  function is_expired() { return $this->_expires->is_past(); }

  function is_valid() { return !$this->is_expired(); }

  function set_mobile($mobile) {
    !isset($mobile) or is_bool($mobile) or trigger_error('bad mobile');
    $this->_mobile = $mobile;
  }

  function to_string() { return $this->id(); }

  function Session($id,$user,$expires,$mobile) {
    is_string($id) or trigger_error('bad id');
    isset($user) or trigger_error('bad user');
    isset($expires) or trigger_error('bad expires');
    !isset($mobile) or is_bool($mobile) or trigger_error('bad mobile');

    $this->_id = $id;
    $this->_user = $user;
    $this->_expires = $expires;
    $this->_mobile = $mobile;
  }
}

///////////////////////////////////////////////////////////////////////////////
// Creating new sessions.
///////////////////////////////////////////////////////////////////////////////

define('NEW_SESSION_SALT',SITE_NAME);

function create_new_session($user) {
  $server_datetime = server_datetime();
  $expires = $server_datetime->next_day(SESSION_TIMEOUT_DAYS);
  $mobile = null;

  $id = md5(NEW_SESSION_SALT . ' ' .
            $user->encrypted_password() . ' ' .
            $expires->to_database_datetime() . ' ' .
            rand());

  return new Session($id,$user,$expires,$mobile);
}

///////////////////////////////////////////////////////////////////////////////
// The session database table.
///////////////////////////////////////////////////////////////////////////////

define('SESSION_ID_CHARS',HASH_CHARS);

class SessionTable extends DatabaseTable {
  function find_session_where($where_condition) {
    is_string($where_condition) or trigger_error('bad where_condition');

    $row = $this->find_row($where_condition);

    if (!isset($row)) { return null; }

    $session = $row['id'];
    $user_id = (integer)$row['user'];
    $expires_datetime = $row['expires'];
    $mobile_bool = $row['mobile'];

    $user_table = user_table();
    $user = $user_table->find_user($user_id);

    $expires = new TimePoint();
    $expires->from_database_datetime($expires_datetime);

    $mobile = bool_from_database_bool($mobile_bool);

    return new Session($session,$user,$expires,$mobile);
  }

  function find_session($session) {
    is_string($session) or trigger_error('bad session');
    return $this->find_session_where('id = ' . database_value($session));
  }

  function delete_expired_sessions() {
    $now = server_datetime();
    $now = $now->to_database_datetime();

    database_query('
      DELETE FROM ' . $this->table() . '
      WHERE expires < ' . database_value($now) . ';');
  }

  function insert_session($session) {
    $this->delete_expired_sessions();

    $id = $session->id();
    $user = $session->user();
    $user_id = $user->id();
    $expires = $session->expires();
    $expires_datetime = $expires->to_database_datetime();
    $mobile = $session->mobile();
    $mobile_bool = bool_to_database_bool($mobile);

    database_query('
      INSERT INTO ' . $this->table() . '
      SET id = ' . database_value($id) . ',
          user = ' . database_value($user_id) . ',
          expires = ' . database_value($expires_datetime) . ',
          mobile = ' . database_value($mobile_bool) . ';');
  }

  function update_session($session) {
    isset($session) or trigger_error('bad session');

    $id = $session->id();
    $user = $session->user();
    $user_id = $user->id();
    $expires = $session->expires();
    $expires_datetime = $expires->to_database_datetime();
    $mobile = $session->mobile();
    $mobile_bool = bool_to_database_bool($mobile);

    database_query('
      UPDATE ' . $this->table() . '
      SET user = ' . database_value($user_id) . ',
          expires = ' . database_value($expires_datetime) . ',
          mobile = ' . database_value($mobile_bool) . '
      WHERE id = ' . database_value($id) . ';');
  }

  function delete_session($session) {
    $id = $session->id();

    database_query('
      DELETE FROM ' . $this->table() . '
      WHERE id = ' . database_value($id) . ';');
  }

  function SessionTable($table) {
    $fields =
      array('id' => 'char(' . SESSION_ID_CHARS . ') NOT NULL',
            'user' => 'int(' . USER_ID_DIGITS . ') NOT NULL',
            'expires' => 'datetime NOT NULL',
            'mobile' => database_bool_type());

    $indexes =
      array('PRIMARY KEY (id)',
            'INDEX (user)',
            'INDEX (expires)');

    parent::DatabaseTable($table,$fields,$indexes);
  }
}

$global_session_table = null;

function session_table() {
  global $global_session_table;

  if (!isset($global_session_table)) {
    $global_session_table = new SessionTable('session');
  }

  return $global_session_table;
}

///////////////////////////////////////////////////////////////////////////////
// Effective session.
///////////////////////////////////////////////////////////////////////////////

$global_effective_session_active = null;
$global_effective_session = null;

function effective_session() {
  global $global_effective_session_active;
  global $global_effective_session;

  if (!isset($global_effective_session_active)) {
    $session_id = from_string(input('session'));

    if (isset($session_id)) {
      $session_table = session_table();
      $session = $session_table->find_session($session_id);

      if (isset($session)) {
        if ($session->is_valid()) {
          $global_effective_session = $session;
        }
        else {
          $session_table->delete_session($session_id);
        }
      }
    }

    $global_effective_session_active = true;
  }

  return $global_effective_session;
}

function effective_user() {
  $session = effective_session();
  if (isset($session)) {
    return $session->user();
  }
  else {
    return null;
  }
}

function effective_privilege() {
  $user = effective_user();
  if (isset($user)) {
    return $user->privilege();
  }
  else {
    return null;
  }
}

function effective_privilege_is_user() {
  $privilege = effective_privilege();
  return isset($privilege);
}

function effective_privilege_is_admin() {
  $privilege = effective_privilege();
  return isset($privilege) && strcmp($privilege,ADMIN_PRIVILEGE) == 0;
}

function effective_privilege_less($priv) {
  $privilege = effective_privilege();
  return privilege_less($privilege,$priv);
}

function effective_mobile() {
  $mobile = null;

  $session = effective_session();
  if (isset($session)) { $mobile = $session->mobile(); }

  if (!isset($mobile)) { $mobile = is_mobile(); }

  return $mobile;
}

///////////////////////////////////////////////////////////////////////////////
// Logging in.
///////////////////////////////////////////////////////////////////////////////

function login($user) {
  isset($user) or trigger_error('bad user');

  $session_table = session_table();

  $session = create_new_session($user);

  $expires = $session->expires();

  $session_table->insert_session($session);

  setcookie('session', $session->id(), $expires->to_datetime(), '/');
}

function login_form($field = '') {
  is_string($field) or trigger_error('bad field');

  $select = new SelectLoginForm($field);

  $user = $select->value();

  if (isset($user)) {
    login($user);

    jump_path(array('account'));
  }

  return $select;
}

///////////////////////////////////////////////////////////////////////////////
// Logging out.
///////////////////////////////////////////////////////////////////////////////

function logout() {
  $session = effective_session();

  if (isset($session)) {
    $session_table = session_table();

    $session_table->delete_session($session);
  }
}

///////////////////////////////////////////////////////////////////////////////
// A class to collect site selection.
///////////////////////////////////////////////////////////////////////////////

$site_options = array(
  'f' => 'Desktop',
  'm' => 'Mobile',
  'd' => 'Auto Detect');

class SelectMobile extends SelectRadio {
  function SelectMobile($field) {
    global $site_options;

    parent::SelectRadio($field,$site_options);
  }
}

///////////////////////////////////////////////////////////////////////////////
// A class to collect session details.
///////////////////////////////////////////////////////////////////////////////

class SelectSessionData extends SelectValue {
  var $_select_mobile;
  var $_select_submit;

  function error() {
    $e = $this->_error;
    if (!isset($e)) { $e = $this->_select_mobile->handled_error(); }
    return $e;
  }

  function validate() {
    parent::validate();

    $this->_select_mobile->validate();

    if (!$this->is_error()) {
      $data = array();

      $mobile_option = $this->_select_mobile->value();
      if (strcmp($mobile_option,'f') == 0) { $mobile = false; }
      elseif (strcmp($mobile_option,'m') == 0) { $mobile = true; }
      elseif (strcmp($mobile_option,'d') == 0) { $mobile = null; }
      else { trigger_error('bad mobile_option'); }

      $data['mobile'] = $mobile;

      parent::set_value($data);
    }
  }

  function select() {
    return
'<p>' .
$this->form_error() .
$this->_select_mobile->form_error() .
field_text('Browser') . '&nbsp; &nbsp;' .
$this->_select_mobile->select() .
'</p>' .
'<p>' .
$this->_select_submit->form_error() .
$this->_select_submit->select() .
'</p>';
  }

  function submitted() {
    return $this->_select_submit->is_value();
  }

  function set_value($data) {
    is_array($data) or trigger_error('bad data');

    $mobile = $data['mobile'];

    if (!isset($mobile)) { $mobile_option = 'd'; }
    elseif ($mobile) { $mobile_option = 'm'; }
    else { $mobile_option = 'f'; }

    $this->_select_mobile->set_value($mobile_option);

    parent::set_value($data);
  }

  function SelectSessionData($field) {
    parent::SelectValue($field);

    $this->_select_mobile = new SelectMobile($this->field() . 'm');
    $this->_select_submit = new SelectSubmit($this->field() . 's', 'update');

    if ($this->_select_submit->is_value()) { $this->validate(); }
  }
}

function session_form($field) {
  is_string($field) or trigger_error('bad field');

  $select = new SelectSessionData($field);

  if ($select->submitted()) {
    $data = $select->value();

    if (isset($data)) {
      $session = effective_session();
      if (isset($session)) {
        $session->set_mobile($data['mobile']);
        $session_table = session_table();
        $session_table->update_session($session);
        jump_path(array('account'));
      }
    }
  }
  else {
    $session = effective_session();

    if (isset($session)) {
      $data = array();
      $data['mobile'] = $session->mobile();

      $select->set_value($data);
    }
  }

  return $select;
}

?>
