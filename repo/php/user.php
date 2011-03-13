<?php

///////////////////////////////////////////////////////////////////////////////
//
// USER TABLE
//
// Copyright (c) 2008 Joe Hurd, distributed under the GNU GPL version 2
//
///////////////////////////////////////////////////////////////////////////////

require_once 'global.php';
require_once 'error.php';
require_once 'form.php';
require_once 'privilege.php';
require_once 'database.php';

///////////////////////////////////////////////////////////////////////////////
// Encrypting passwords.
///////////////////////////////////////////////////////////////////////////////

define('PASSWORD_SALT', SITE_NAME . '.' . REPO_NAME . '.password');

function encrypt_password($user,$password) {
  is_int($user) or trigger_error('bad user');
  is_string($password) or trigger_error('bad password');

  return md5(PASSWORD_SALT . ' ' . $user . ' ' . strtolower($password));
}

///////////////////////////////////////////////////////////////////////////////
// A class to store user information.
///////////////////////////////////////////////////////////////////////////////

class User {
  var $_id;
  var $_name;
  var $_encrypted_password;
  var $_privilege;

  function id() { return $this->_id; }

  function name() { return $this->_name; }

  function encrypted_password() { return $this->_encrypted_password; }

  function privilege() { return $this->_privilege; }

  function is_password($password) {
    is_string($password) or trigger_error('bad password');
    $encrypted = encrypt_password($this->_id,$password);
    //var_dump($encrypted);
    return (strcmp($encrypted,$this->_encrypted_password) == 0);
  }

  function User($id,$name,$encrypted_password,$privilege) {
    is_int($id) or trigger_error('bad id');
    is_string($name) or trigger_error('bad name');
    is_string($encrypted_password) or trigger_error('bad encrypted_password');
    is_privilege($privilege) or trigger_error('bad privilege');

    $this->_id = $id;
    $this->_name = $name;
    $this->_encrypted_password = $encrypted_password;
    $this->_privilege = $privilege;
  }
}

///////////////////////////////////////////////////////////////////////////////
// The user database table.
///////////////////////////////////////////////////////////////////////////////

define('USER_ID_DIGITS',6);
define('USER_NAME_CHARS',20);
define('USER_ENCRYPTED_PASSWORD_CHARS',HASH_CHARS);

class UserTable extends DatabaseTable {
  function find_user_where($where_condition) {
    is_string($where_condition) or trigger_error('bad where_condition');

    $row = $this->find_row($where_condition);

    if (!isset($row)) { return null; }

    $id = (integer)$row['id'];
    $name = $row['name'];
    $encrypted_password = $row['encrypted_password'];
    $privilege = $row['privilege'];

    return new User($id,$name,$encrypted_password,$privilege);
  }

  function find_user($id) {
    is_int($id) or trigger_error('bad user');
    return $this->find_user_where('id = ' . database_value($id));
  }

  function find_user_by_name($name) {
    is_string($name) or trigger_error('bad name');
    return $this->find_user_where('name = ' . database_value($name));
  }

  function insert_user($user) {
    $id = $user->id();
    $name = $user->name();
    $encrypted_password = $user->encrypted_password();
    $privilege = $user->privilege();

    database_query('
      INSERT INTO ' . $this->table() . '
      SET id = ' . database_value($id) . ',
          name = ' . database_value($name) . ',
          encrypted_password = ' . database_value($encrypted_password) . ',
          privilege = ' . database_value($privilege) . ';');
  }

  function create_user($name,$password,$privilege) {
    is_string($name) or trigger_error('bad name');
    is_string($password) or trigger_error('bad password');
    is_privilege($privilege) or trigger_error('bad privilege');

    $id = $this->max_rows('id') + 1;
    $encrypted_password = encrypt_password($id,$password);
    $user = new User($id,$name,$encrypted_password,$privilege);
    $this->insert_user($user);

    return $user;
  }

  function UserTable($table) {
    global $all_privileges;

    $fields =
      array('id' => 'int(' . USER_ID_DIGITS . ') NOT NULL',
            'name' => 'varchar(' . USER_NAME_CHARS . ') NOT NULL',
            'encrypted_password' =>
              'char(' . USER_ENCRYPTED_PASSWORD_CHARS . ') NOT NULL',
            'privilege' =>
              array_to_database_enum($all_privileges) . ' NOT NULL');

    $indexes =
      array('PRIMARY KEY (id)',
            'INDEX (name)');

    parent::DatabaseTable($table,$fields,$indexes);
  }
}

$global_user_table = null;

function user_table() {
  global $global_user_table;

  if (!isset($global_user_table)) {
    $global_user_table = new UserTable('user');
  }

  return $global_user_table;
}

///////////////////////////////////////////////////////////////////////////////
// A class to collect user names.
///////////////////////////////////////////////////////////////////////////////

class SelectUserName extends SelectText {
  function error_message_missing() {
    return 'Please enter your login name';
  }

  function select() {
    return $this->text_input('',$this->value(),USER_NAME_CHARS);
  }

  function SelectUserName($field,$required) {
    parent::SelectText($field,$required,null,USER_NAME_CHARS);
  }
}

///////////////////////////////////////////////////////////////////////////////
// A class to collect user passwords.
///////////////////////////////////////////////////////////////////////////////

class SelectUserPassword extends SelectText {
  function error_message_missing() {
    return 'Please enter your password';
  }

  function select() {
    return $this->password_input('',$this->value(),USER_NAME_CHARS);
  }

  function SelectUserPassword($field,$required) {
    parent::SelectText($field,$required,null,null);
  }
}

///////////////////////////////////////////////////////////////////////////////
// A class to collect login information.
///////////////////////////////////////////////////////////////////////////////

class SelectLoginForm extends SelectValue {
  var $_select_login;
  var $_select_password;
  var $_select_submit;

  function error() {
    $e = $this->_error;
    if (!isset($e)) { $e = $this->_select_login->handled_error(); }
    if (!isset($e)) { $e = $this->_select_password->handled_error(); }
    return $e;
  }

  function validate() {
    $this->_select_login->validate();
    $this->_select_password->validate();

    if (!$this->is_error()) {
      $name = $this->_select_login->value();
      $password = $this->_select_password->value();
      $user_table = user_table();
      $user = $user_table->find_user_by_name($name);
      if (isset($user)) {
        if ($user->is_password($password)) {
          $this->set_value($user);
        }
        else {
          $this->set_error('Wrong password');
        }
      }
      else {
        $this->set_error('Unknown login name');
      }
    }
  }

  function select() {
    return
'<table cellpadding="0" cellspacing="0">' .
($this->is_error()
 ? ('<tr>' .
    '<td colspan="3">' .
    error_text($this->error()) .
    '</td>' .
    '</tr>' .
    '<tr><td height="20"></td></tr>')
 : '') .
'<tr>' .
'<td>' .
field_text('Login name') .
'</td>' .
'<td width="20"></td>' .
'<td>' .
$this->_select_login->form_error() .
$this->_select_login->select() .
'</td>' .
'</tr>' .
'<tr><td height="10"></td></tr>' .
'<tr>' .
'<td>' .
field_text('Password') .
'</td>' .
'<td width="20"></td>' .
'<td>' .
$this->_select_password->form_error() .
$this->_select_password->select() .
'</td>' .
'</tr>' .
'<tr><td height="20"></td></tr>' .
'<tr>' .
'<td colspan="3">' .
$this->_select_submit->form_error() .
$this->_select_submit->select() .
'</td>' .
'</tr>' .
'</table>';
  }

  function SelectLoginForm($field) {
    parent::SelectValue($field);

    $this->_select_login = new SelectUserName($this->field() . 'u', true);
    $this->_select_password = new SelectUserPassword($this->field() . 'p', true);
    $this->_select_submit = new SelectSubmit($this->field() . 's', 'login');
    $this->_error = null;
    $this->_validated_user = null;

    if ($this->_select_submit->is_value()) {
      $this->validate();
    }
  }
}

?>
