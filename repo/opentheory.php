<?php

///////////////////////////////////////////////////////////////////////////////
//
// OPENTHEORY REPO
//
// Copyright (c) 2009 Joe Leslie-Hurd, distributed under the MIT license
//
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
// Generic functionality.
///////////////////////////////////////////////////////////////////////////////

require_once 'php/global.php';  // SITE-SPECIFIC configuration
require_once 'php/error.php';  // Error handling
require_once 'php/functions.php';  // Utility functions
require_once 'php/date.php';  // Date functions
require_once 'php/input.php';  // Input parameters
require_once 'php/links.php';  // Generating links
require_once 'php/form.php';  // Stateful forms
require_once 'php/privilege.php';  // User privileges
require_once 'php/database.php';  // Database interface
require_once 'php/user.php';  // User table
require_once 'php/session.php';  // Session table
require_once 'php/output.php';  // SITE-SPECIFIC page output

///////////////////////////////////////////////////////////////////////////////
// OpenTheory functionality.
///////////////////////////////////////////////////////////////////////////////

require_once 'php/name_version.php';  // Package name/versions
require_once 'php/author.php';  // Package authors
require_once 'php/tag.php';  // Package tags
require_once 'php/opentheory.php';  // Opentheory tool interface
require_once 'php/package.php';  // Package table
require_once 'php/dependency.php';  // Package dependency table
require_once 'php/upload.php';  // Package upload table
require_once 'php/upload_package.php';  // Uploaded packages table
require_once 'php/confirm_upload.php';  // Upload confirmations table
require_once 'php/repo.php';  // Repo operations

///////////////////////////////////////////////////////////////////////////////
// Initialization.
///////////////////////////////////////////////////////////////////////////////

//require_once 'php/command.php';  // Manually execute commands

?>
