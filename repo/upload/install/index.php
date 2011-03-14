<?php

require_once '../../opentheory.php';

///////////////////////////////////////////////////////////////////////////////
// Delete the package upload.
///////////////////////////////////////////////////////////////////////////////

$upload = from_string(input('u'));
if (isset($upload)) { $upload = find_upload($upload); }
if (!isset($upload)) { trigger_error('bad upload'); }

$pkg = from_string(input('p'));
if (isset($pkg)) { $pkg = from_string_package_name_version($pkg); }
if (!isset($pkg)) { trigger_error('bad pkg'); }

$chk = checksum_from_string(input('c'));
if (!isset($chk)) { trigger_error('bad chk'); }

$action = from_string(input('x'));
if (!isset($action)) { trigger_error('bad action'); }

if (!$upload->add_packagable()) {
  $error = 'this upload set is closed';
}
else {
  $error = opentheory_install($pkg,$chk);
}

if (isset($error)) {
  $report =
'failed to install package ' . $pkg->to_string() . ':' . "\n" .
$error;

  output_script($report);
}

repo_register($pkg);

output_script('successfully installed package ' . $pkg->to_string());

?>
