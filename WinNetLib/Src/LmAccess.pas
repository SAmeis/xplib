{******************************************************************}
{                                                                  }
{ Borland Delphi Runtime Library                                   }
{ LanManager share functions for Windows NT interface unit         }
{                                                                  }
{ Portions created by Microsoft are                                }
{ Copyright (C) 1995-1999 Microsoft Corporation.                   }
{ All Rights Reserved.                                             }
{                                                                  }
{ The original file is: lmaccess.h, released 8 Feb 1999.           }
{ The original Pascal code is: LmAccess.pas, released 11 Jan 2000. }
{ The initial developer of the Pascal code is Petr Vones           }
{ (petr.v@mujmail.cz).                                             }
{                                                                  }
{ Portions created by Petr Vones are                               }
{ Copyright (C) 2000 Petr Vones                                    }
{                                                                  }
{ Obtained through:                                                }
{                                                                  }
{ Joint Endeavour of Delphi Innovators (Project JEDI)              }
{                                                                  }
{ You may retrieve the latest version of this file at the Project  }
{ JEDI home page, located at http://delphi-jedi.org                }
{                                                                  }
{ The contents of this file are used with permission, subject to   }
{ the Mozilla Public License Version 1.1 (the "License"); you may  }
{ not use this file except in compliance with the License. You may }
{ obtain a copy of the License at                                  }
{ http://www.mozilla.org/MPL/MPL-1.1.html                          }
{                                                                  }
{ Software distributed under the License is distributed on an      }
{ "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or   }
{ implied. See the License for the specific language governing     }
{ rights and limitations under the License.                        }
{                                                                  }
{******************************************************************}

unit LmAccess;

{$I LANMAN.INC}

{$ALIGN ON}
{$MINENUMSIZE 4}
{$WEAKPACKAGEUNIT}

interface

uses
  Windows, LmCons;

(*$HPPEMIT '#include <netcons.h>'*)
(*$HPPEMIT '#include <lmcons.h>'*)
(*$HPPEMIT '#include <lmaccess.h>'*)

// User Class

// Function Prototypes - User

{ TODO -oroger -clib : Incluir diretivas de compilação a todas as units e ao proprio pacote }

{$EXTERNALSYM NetUserAdd}
function NetUserAdd(servername: LPCWSTR; level: DWORD; buf: Pointer;
  parm_err: PDWORD): NET_API_STATUS; stdcall;

{$EXTERNALSYM NetUserEnum}
function NetUserEnum(servername: LPCWSTR; level: DWORD; filter: DWORD;
  var bufptr: Pointer; prefmaxlen: DWORD; var entriesread: DWORD;
  var totalentries: DWORD; resume_handle: PDWORD): NET_API_STATUS; stdcall;

{$EXTERNALSYM NetUserGetInfo}
function NetUserGetInfo(servername: LPCWSTR; username: LPCWSTR; level: DWORD;
  var bufptr: Pointer): NET_API_STATUS; stdcall;

{$EXTERNALSYM NetUserSetInfo}
function NetUserSetInfo(servername: LPCWSTR; username: LPCWSTR; level: DWORD;
  buf: Pointer; parm_err: PDWORD): NET_API_STATUS; stdcall;

{$EXTERNALSYM NetUserDel}
function NetUserDel(servername: LPCWSTR; username: LPCWSTR): NET_API_STATUS; stdcall;

{$EXTERNALSYM NetUserGetGroups}
function NetUserGetGroups(servername: LPCWSTR; username: LPCWSTR; level: DWORD;
  var bufptr: Pointer; prefmaxlen: DWORD; var entriesread: DWORD;
  var totalentries: DWORD): NET_API_STATUS; stdcall;

{$EXTERNALSYM NetUserSetGroups}
function NetUserSetGroups(servername: LPCWSTR; username: LPCWSTR; level: DWORD;
  buf: Pointer; num_entries: DWORD): NET_API_STATUS; stdcall;

{$EXTERNALSYM NetUserGetLocalGroups}
function NetUserGetLocalGroups(servername: LPCWSTR; username: LPCWSTR;
  level: DWORD; flags: DWORD; var bufptr: Pointer; prefmaxlen: DWORD;
  var entriesread: DWORD; var totalentries: DWORD): NET_API_STATUS; stdcall;

{$EXTERNALSYM NetUserModalsGet}
function NetUserModalsGet(servername: LPCWSTR; level: DWORD;
  var bufptr: Pointer): NET_API_STATUS; stdcall;

{$EXTERNALSYM NetUserModalsSet}
function NetUserModalsSet(servername: LPCWSTR; level: DWORD; buf: Pointer;
  parm_err: PDWORD): NET_API_STATUS; stdcall;

{$EXTERNALSYM NetUserChangePassword}
function NetUserChangePassword(domainname, username, oldpassword,
  newpassword: LPCWSTR): NET_API_STATUS; stdcall;

//  Data Structures - User

type
  PUserInfo0 = ^TUserInfo0;
  {$EXTERNALSYM _USER_INFO_0}
  _USER_INFO_0  = record
    usri0_name: LPWSTR;
  end;
  TUserInfo0 = _USER_INFO_0;
  {$EXTERNALSYM USER_INFO_0}
  USER_INFO_0 = _USER_INFO_0;

  PUserInfo1 = ^TUserInfo1;
  {$EXTERNALSYM _USER_INFO_1}
  _USER_INFO_1 = record
    usri1_name: LPWSTR;
    usri1_password: LPWSTR;
    usri1_password_age: DWORD;
    usri1_priv: DWORD;
    usri1_home_dir: LPWSTR;
    usri1_comment: LPWSTR;
    usri1_flags: DWORD;
    usri1_script_path: LPWSTR;
  end;
  TUserInfo1 = _USER_INFO_1;
  {$EXTERNALSYM USER_INFO_1}
  USER_INFO_1 = _USER_INFO_1;

  PUserInfo2 = ^TUserInfo2;
  {$EXTERNALSYM _USER_INFO_2}
  _USER_INFO_2 = record
    usri2_name: LPWSTR;
    usri2_password: LPWSTR;
    usri2_password_age: DWORD;
    usri2_priv: DWORD;
    usri2_home_dir: LPWSTR;
    usri2_comment: LPWSTR;
    usri2_flags: DWORD;
    usri2_script_path: LPWSTR;
    usri2_auth_flags: DWORD;
    usri2_full_name: LPWSTR;
    usri2_usr_comment: LPWSTR;
    usri2_parms: LPWSTR;
    usri2_workstations: LPWSTR;
    usri2_last_logon: DWORD;
    usri2_last_logoff: DWORD;
    usri2_acct_expires: DWORD;
    usri2_max_storage: DWORD;
    usri2_units_per_week: DWORD;
    usri2_logon_hours: PBYTE;
    usri2_bad_pw_count: DWORD;
    usri2_num_logons: DWORD;
    usri2_logon_server: LPWSTR;
    usri2_country_code: DWORD;
    usri2_code_page: DWORD;
  end;
  TUserInfo2 = _USER_INFO_2;
  {$EXTERNALSYM USER_INFO_2}
  USER_INFO_2 = _USER_INFO_2;

  PUserInfo3 = ^TUserInfo3;
  {$EXTERNALSYM _USER_INFO_3}
  _USER_INFO_3 = record
    usri3_name: LPWSTR;
    usri3_password: LPWSTR;
    usri3_password_age: DWORD;
    usri3_priv: DWORD;
    usri3_home_dir: LPWSTR;
    usri3_comment: LPWSTR;
    usri3_flags: DWORD;
    usri3_script_path: LPWSTR;
    usri3_auth_flags: DWORD;
    usri3_full_name: LPWSTR;
    usri3_usr_comment: LPWSTR;
    usri3_parms: LPWSTR;
    usri3_workstations: LPWSTR;
    usri3_last_logon: DWORD;
    usri3_last_logoff: DWORD;
    usri3_acct_expires: DWORD;
    usri3_max_storage: DWORD;
    usri3_units_per_week: DWORD;
    usri3_logon_hours: PBYTE;
    usri3_bad_pw_count: DWORD;
    usri3_num_logons: DWORD;
    usri3_logon_server: LPWSTR;
    usri3_country_code: DWORD;
    usri3_code_page: DWORD;
    usri3_user_id: DWORD;
    usri3_primary_group_id: DWORD;
    usri3_profile: LPWSTR;
    usri3_home_dir_drive: LPWSTR;
    usri3_password_expired: DWORD;
  end;
  TUserInfo3 = _USER_INFO_3;
  {$EXTERNALSYM USER_INFO_3}
  USER_INFO_3 = _USER_INFO_3;

  PUserInfo10 = ^TUserInfo10;
  {$EXTERNALSYM _USER_INFO_10}
  _USER_INFO_10 = record
    usri10_name: LPWSTR;
    usri10_comment: LPWSTR;
    usri10_usr_comment: LPWSTR;
    usri10_full_name: LPWSTR;
  end;
  TUserInfo10 = _USER_INFO_10;
  {$EXTERNALSYM USER_INFO_10}
  USER_INFO_10 = _USER_INFO_10;

  PUserInfo11 = ^TUserInfo11;
  {$EXTERNALSYM _USER_INFO_11}
  _USER_INFO_11 = record
    usri11_name: LPWSTR;
    usri11_comment: LPWSTR;
    usri11_usr_comment: LPWSTR;
    usri11_full_name: LPWSTR;
    usri11_priv: DWORD;
    usri11_auth_flags: DWORD;
    usri11_password_age: DWORD;
    usri11_home_dir: LPWSTR;
    usri11_parms: LPWSTR;
    usri11_last_logon: DWORD;
    usri11_last_logoff: DWORD;
    usri11_bad_pw_count: DWORD;
    usri11_num_logons: DWORD;
    usri11_logon_server: LPWSTR;
    usri11_country_code: DWORD;
    usri11_workstations: LPWSTR;
    usri11_max_storage: DWORD;
    usri11_units_per_week: DWORD;
    usri11_logon_hours: PBYTE;
    usri11_code_page: DWORD;
  end;
  TUserInfo11 = _USER_INFO_11;
  {$EXTERNALSYM USER_INFO_11}
  USER_INFO_11 = _USER_INFO_11;

  PUserInfo20 = ^TUserInfo20;
  {$EXTERNALSYM _USER_INFO_20}
  _USER_INFO_20 = record
    usri20_name: LPWSTR;
    usri20_full_name: LPWSTR;
    usri20_comment: LPWSTR;
    usri20_flags: DWORD;
    usri20_user_id: DWORD;
  end;
  TUserInfo20 = _USER_INFO_20;
  {$EXTERNALSYM USER_INFO_20}
  USER_INFO_20 = _USER_INFO_20;

  PUserInfo21 = ^TUserInfo21;
  {$EXTERNALSYM _USER_INFO_21}
  _USER_INFO_21 = record
    usri21_password: array[0..ENCRYPTED_PWLEN-1] of Byte;
  end;
  TUserInfo21 = _USER_INFO_21;
  {$EXTERNALSYM USER_INFO_21}
  USER_INFO_21 = _USER_INFO_21;

  PUserInfo22 = ^TUserInfo22;
  {$EXTERNALSYM _USER_INFO_22}
  _USER_INFO_22 = record
    usri22_name: LPWSTR;
    usri22_password: array[0..ENCRYPTED_PWLEN-1] of Byte;
    usri22_password_age: DWORD;
    usri22_priv: DWORD;
    usri22_home_dir: LPWSTR;
    usri22_comment: LPWSTR;
    usri22_flags: DWORD;
    usri22_script_path: LPWSTR;
    usri22_auth_flags: DWORD;
    usri22_full_name: LPWSTR;
    usri22_usr_comment: LPWSTR;
    usri22_parms: LPWSTR;
    usri22_workstations: LPWSTR;
    usri22_last_logon: DWORD;
    usri22_last_logoff: DWORD;
    usri22_acct_expires: DWORD;
    usri22_max_storage: DWORD;
    usri22_units_per_week: DWORD;
    usri22_logon_hours: PBYTE;
    usri22_bad_pw_count: DWORD;
    usri22_num_logons: DWORD;
    usri22_logon_server: LPWSTR;
    usri22_country_code: DWORD;
    usri22_code_page: DWORD;
  end;
  TUserInfo22 = _USER_INFO_22;
  {$EXTERNALSYM USER_INFO_22}
  USER_INFO_22 = _USER_INFO_22;

  PUserInfo1003 = ^TUserInfo1003;
  {$EXTERNALSYM _USER_INFO_1003}
  _USER_INFO_1003 = record
    usri1003_password: LPWSTR;
  end;
  TUserInfo1003 = _USER_INFO_1003;
  {$EXTERNALSYM USER_INFO_1003}
  USER_INFO_1003 = _USER_INFO_1003;

  PUserInfo1005 = ^TUserInfo1005;
  {$EXTERNALSYM _USER_INFO_1005}
  _USER_INFO_1005 = record
    usri1005_priv: DWORD;
  end;
  TUserInfo1005 = _USER_INFO_1005;
  {$EXTERNALSYM USER_INFO_1005}
  USER_INFO_1005 = _USER_INFO_1005;

  PUserInfo1006 = ^TUserInfo1006;
  {$EXTERNALSYM _USER_INFO_1006}
  _USER_INFO_1006 = record
    usri1006_home_dir: LPWSTR;
  end;
  TUserInfo1006 = _USER_INFO_1006;
  {$EXTERNALSYM USER_INFO_1006}
  USER_INFO_1006 = _USER_INFO_1006;

  PUserInfo1007 = ^TUserInfo1007;
  {$EXTERNALSYM _USER_INFO_1007}
  _USER_INFO_1007 = record
    usri1007_comment: LPWSTR;
  end;
  TUserInfo1007 = _USER_INFO_1007;
  {$EXTERNALSYM USER_INFO_1007}
  USER_INFO_1007 = _USER_INFO_1007;

  PUserInfo1008 = ^TUserInfo1008;
  {$EXTERNALSYM _USER_INFO_1008}
  _USER_INFO_1008 = record
    usri1008_flags: DWORD;
  end;
  TUserInfo1008 = _USER_INFO_1008;
  {$EXTERNALSYM USER_INFO_1008}
  USER_INFO_1008 = _USER_INFO_1008;

  PUserInfo1009 = ^TUserInfo1009;
  {$EXTERNALSYM _USER_INFO_1009}
  _USER_INFO_1009 = record
    usri1009_script_path: LPWSTR;
  end;
  TUserInfo1009 = _USER_INFO_1009;
  {$EXTERNALSYM USER_INFO_1009}
  USER_INFO_1009 = _USER_INFO_1009;

  PUserInfo1010 = ^TUserInfo1010;
  {$EXTERNALSYM _USER_INFO_1010}
  _USER_INFO_1010 = record
    usri1010_auth_flags: DWORD;
  end;
  TUserInfo1010 = _USER_INFO_1010;
  {$EXTERNALSYM USER_INFO_1010}
  USER_INFO_1010 = _USER_INFO_1010;

  PUserInfo1011 = ^TUserInfo1011;
  {$EXTERNALSYM _USER_INFO_1011}
  _USER_INFO_1011 = record
    usri1011_full_name: LPWSTR;
  end;
  TUserInfo1011 = _USER_INFO_1011;
  {$EXTERNALSYM USER_INFO_1011}
  USER_INFO_1011 = _USER_INFO_1011;

  PUserInfo1012 = ^TUserInfo1012;
  {$EXTERNALSYM _USER_INFO_1012}
  _USER_INFO_1012 = record
    usri1012_usr_comment: LPWSTR;
  end;
  TUserInfo1012 = _USER_INFO_1012;
  {$EXTERNALSYM USER_INFO_1012}
  USER_INFO_1012 = _USER_INFO_1012;

  PUserInfo1013 = ^TUserInfo1013;
  {$EXTERNALSYM _USER_INFO_1013}
  _USER_INFO_1013 = record
    usri1013_parms: LPWSTR;
  end;
  TUserInfo1013 = _USER_INFO_1013;
  {$EXTERNALSYM USER_INFO_1013}
  USER_INFO_1013 = _USER_INFO_1013;

  PUserInfo1014 = ^TUserInfo1014;
  {$EXTERNALSYM _USER_INFO_1014}
  _USER_INFO_1014 = record
    usri1014_workstations: LPWSTR;
  end;
  TUserInfo1014 = _USER_INFO_1014;
  {$EXTERNALSYM USER_INFO_1014}
  USER_INFO_1014 = _USER_INFO_1014;

  PUserInfo1017 = ^TUserInfo1017;
  {$EXTERNALSYM _USER_INFO_1017}
  _USER_INFO_1017 = record
    usri1017_acct_expires: DWORD;
  end;
  TUserInfo1017 = _USER_INFO_1017;
  {$EXTERNALSYM USER_INFO_1017}
  USER_INFO_1017 = _USER_INFO_1017;

  PUserInfo1018 = ^TUserInfo1018;
  {$EXTERNALSYM _USER_INFO_1018}
  _USER_INFO_1018 = record
    usri1018_max_storage: DWORD;
  end;
  TUserInfo1018 = _USER_INFO_1018;
  {$EXTERNALSYM USER_INFO_1018}
  USER_INFO_1018 = _USER_INFO_1018;

  PUserInfo1020 = ^TUserInfo1020;
  {$EXTERNALSYM _USER_INFO_1020}
  _USER_INFO_1020 = record
    usri1020_units_per_week: DWORD;
    usri1020_logon_hours: Pointer;
  end;
  TUserInfo1020 = _USER_INFO_1020;
  {$EXTERNALSYM USER_INFO_1020}
  USER_INFO_1020 = _USER_INFO_1020;

  PUserInfo1023 = ^TUserInfo1023;
  {$EXTERNALSYM _USER_INFO_1023}
  _USER_INFO_1023 = record
    usri1023_logon_server: LPWSTR;
  end;
  TUserInfo1023 = _USER_INFO_1023;
  {$EXTERNALSYM USER_INFO_1023}
  USER_INFO_1023 = _USER_INFO_1023;

  PUserInfo1024 = ^TUserInfo1024;
  {$EXTERNALSYM _USER_INFO_1024}
  _USER_INFO_1024 = record
    usri1024_country_code: DWORD;
  end;
  TUserInfo1024 = _USER_INFO_1024;
  {$EXTERNALSYM USER_INFO_1024}
  USER_INFO_1024 = _USER_INFO_1024;

  PUserInfo1025 = ^TUserInfo1025;
  {$EXTERNALSYM _USER_INFO_1025}
  _USER_INFO_1025 = record
    usri1025_code_page: DWORD;
  end;
  TUserInfo1025 = _USER_INFO_1025;
  {$EXTERNALSYM USER_INFO_1025}
  USER_INFO_1025 = _USER_INFO_1025;

  PUserInfo1051 = ^TUserInfo1051;
  {$EXTERNALSYM _USER_INFO_1051}
  _USER_INFO_1051 = record
    usri1051_primary_group_id: DWORD;
  end;
  TUserInfo1051 = _USER_INFO_1051;
  {$EXTERNALSYM USER_INFO_1051}
  USER_INFO_1051 = _USER_INFO_1051;

  PUserInfo1052 = ^TUserInfo1052;
  {$EXTERNALSYM _USER_INFO_1052}
  _USER_INFO_1052 = record
    usri1052_profile: LPWSTR;
  end;
  TUserInfo1052 = _USER_INFO_1052;
  {$EXTERNALSYM USER_INFO_1052}
  USER_INFO_1052 = _USER_INFO_1052;

  PUserInfo1053 = ^TUserInfo1053;
  {$EXTERNALSYM _USER_INFO_1053}
  _USER_INFO_1053 = record
    usri1053_home_dir_drive: LPWSTR;
  end;
  TUserInfo1053 = _USER_INFO_1053;
  {$EXTERNALSYM USER_INFO_1053}
  USER_INFO_1053 = _USER_INFO_1053;

//  Data Structures - User Modals

  PUserModalsInfo0 = ^TUserModalsInfo0;
  {$EXTERNALSYM _USER_MODALS_INFO_0}
  _USER_MODALS_INFO_0 = record
    usrmod0_min_passwd_len: DWORD;
    usrmod0_max_passwd_age: DWORD;
    usrmod0_min_passwd_age: DWORD;
    usrmod0_force_logoff: DWORD;
    usrmod0_password_hist_len: DWORD;
  end;
  TUserModalsInfo0 = _USER_MODALS_INFO_0;
  {$EXTERNALSYM USER_MODALS_INFO_0}
  USER_MODALS_INFO_0 = _USER_MODALS_INFO_0;

  PUserModalsInfo1 = ^TUserModalsInfo1;
  {$EXTERNALSYM _USER_MODALS_INFO_1}
  _USER_MODALS_INFO_1 = record
    usrmod1_role: DWORD;
    usrmod1_primary: LPWSTR;
  end;
  TUserModalsInfo1 = _USER_MODALS_INFO_1;
  {$EXTERNALSYM USER_MODALS_INFO_1}
  USER_MODALS_INFO_1 = _USER_MODALS_INFO_1;

  PUserModalsInfo2 = ^TUserModalsInfo2;
  {$EXTERNALSYM _USER_MODALS_INFO_2}
  _USER_MODALS_INFO_2 = record
    usrmod2_domain_name: LPWSTR;
    usrmod2_domain_id: PSID;
  end;
  TUserModalsInfo2 = _USER_MODALS_INFO_2;
  {$EXTERNALSYM USER_MODALS_INFO_2}
  USER_MODALS_INFO_2 = _USER_MODALS_INFO_2;

  PUserModalsInfo3 = ^TUserModalsInfo3;
  {$EXTERNALSYM _USER_MODALS_INFO_3}
  _USER_MODALS_INFO_3 = record
    usrmod3_lockout_duration: DWORD;
    usrmod3_lockout_observation_window: DWORD;
    usrmod3_lockout_threshold: DWORD;
  end;
  TUserModalsInfo3 = _USER_MODALS_INFO_3;
  {$EXTERNALSYM USER_MODALS_INFO_3}
  USER_MODALS_INFO_3 = _USER_MODALS_INFO_3;

  PUserModalsInfo1001 = ^TUserModalsInfo1001;
  {$EXTERNALSYM _USER_MODALS_INFO_1001}
  _USER_MODALS_INFO_1001 = record
    usrmod1001_min_passwd_len: DWORD;
  end;
  TUserModalsInfo1001 = _USER_MODALS_INFO_1001;
  {$EXTERNALSYM USER_MODALS_INFO_1001}
  USER_MODALS_INFO_1001 = _USER_MODALS_INFO_1001;

  PUserModalsInfo1002 = ^TUserModalsInfo1002;
  {$EXTERNALSYM _USER_MODALS_INFO_1002}
  _USER_MODALS_INFO_1002 = record
    usrmod1002_max_passwd_age: DWORD;
  end;
  TUserModalsInfo1002 = _USER_MODALS_INFO_1002;
  {$EXTERNALSYM USER_MODALS_INFO_1002}
  USER_MODALS_INFO_1002 = _USER_MODALS_INFO_1002;

  PUserModalsInfo1003 = ^TUserModalsInfo1003;
  {$EXTERNALSYM _USER_MODALS_INFO_1003}
  _USER_MODALS_INFO_1003 = record
    usrmod1003_min_passwd_age: DWORD;
  end;
  TUserModalsInfo1003 = _USER_MODALS_INFO_1003;
  {$EXTERNALSYM USER_MODALS_INFO_1003}
  USER_MODALS_INFO_1003 = _USER_MODALS_INFO_1003;

  PUserModalsInfo1004 = ^TUserModalsInfo1004;
  {$EXTERNALSYM _USER_MODALS_INFO_1004}
  _USER_MODALS_INFO_1004 = record
    usrmod1004_force_logoff: DWORD;
  end;
  TUserModalsInfo1004 = _USER_MODALS_INFO_1004;
  {$EXTERNALSYM USER_MODALS_INFO_1004}
  USER_MODALS_INFO_1004 = _USER_MODALS_INFO_1004;

  PUserModalsInfo1005 = ^TUserModalsInfo1005;
  {$EXTERNALSYM _USER_MODALS_INFO_1005}
  _USER_MODALS_INFO_1005 = record
    usrmod1005_password_hist_len: DWORD;
  end;
  TUserModalsInfo1005 = _USER_MODALS_INFO_1005;
  {$EXTERNALSYM USER_MODALS_INFO_1005}
  USER_MODALS_INFO_1005 = _USER_MODALS_INFO_1005;

  PUserModalsInfo1006 = ^TUserModalsInfo1006;
  {$EXTERNALSYM _USER_MODALS_INFO_1006}
  _USER_MODALS_INFO_1006 = record
    usrmod1006_role: DWORD;
  end;
  TUserModalsInfo1006 = _USER_MODALS_INFO_1006;
  {$EXTERNALSYM USER_MODALS_INFO_1006}
  USER_MODALS_INFO_1006 = _USER_MODALS_INFO_1006;

  PUserModalsInfo1007 = ^TUserModalsInfo1007;
  {$EXTERNALSYM _USER_MODALS_INFO_1007}
  _USER_MODALS_INFO_1007 = record
    usrmod1007_primary: LPWSTR;
  end;
  TUserModalsInfo1007 = _USER_MODALS_INFO_1007;
  {$EXTERNALSYM USER_MODALS_INFO_1007}
  USER_MODALS_INFO_1007 = _USER_MODALS_INFO_1007;

// Special Values and Constants - User

//  Bit masks for field usriX_flags of USER_INFO_X (X = 0/1).

const
  {$EXTERNALSYM UF_SCRIPT}
  UF_SCRIPT                          = $0001;
  {$EXTERNALSYM UF_ACCOUNTDISABLE}
  UF_ACCOUNTDISABLE                  = $0002;
  {$EXTERNALSYM UF_HOMEDIR_REQUIRED}
  UF_HOMEDIR_REQUIRED                = $0008;
  {$EXTERNALSYM UF_LOCKOUT}
  UF_LOCKOUT                         = $0010;
  {$EXTERNALSYM UF_PASSWD_NOTREQD}
  UF_PASSWD_NOTREQD                  = $0020;
  {$EXTERNALSYM UF_PASSWD_CANT_CHANGE}
  UF_PASSWD_CANT_CHANGE              = $0040;
  {$EXTERNALSYM UF_ENCRYPTED_TEXT_PASSWORD_ALLOWED}
  UF_ENCRYPTED_TEXT_PASSWORD_ALLOWED = $0080;

// Account type bits as part of usri_flags.

  {$EXTERNALSYM UF_TEMP_DUPLICATE_ACCOUNT}
  UF_TEMP_DUPLICATE_ACCOUNT       = $0100;
  {$EXTERNALSYM UF_NORMAL_ACCOUNT}
  UF_NORMAL_ACCOUNT               = $0200;
  {$EXTERNALSYM UF_INTERDOMAIN_TRUST_ACCOUNT}
  UF_INTERDOMAIN_TRUST_ACCOUNT    = $0800;
  {$EXTERNALSYM UF_WORKSTATION_TRUST_ACCOUNT}
  UF_WORKSTATION_TRUST_ACCOUNT    = $1000;
  {$EXTERNALSYM UF_SERVER_TRUST_ACCOUNT}
  UF_SERVER_TRUST_ACCOUNT         = $2000;

  {$EXTERNALSYM UF_MACHINE_ACCOUNT_MASK}
  UF_MACHINE_ACCOUNT_MASK = UF_INTERDOMAIN_TRUST_ACCOUNT or
    UF_WORKSTATION_TRUST_ACCOUNT or UF_SERVER_TRUST_ACCOUNT;

  {$EXTERNALSYM UF_ACCOUNT_TYPE_MASK}
  UF_ACCOUNT_TYPE_MASK = UF_TEMP_DUPLICATE_ACCOUNT or UF_NORMAL_ACCOUNT or
    UF_INTERDOMAIN_TRUST_ACCOUNT or UF_WORKSTATION_TRUST_ACCOUNT or
    UF_SERVER_TRUST_ACCOUNT;

  {$EXTERNALSYM UF_DONT_EXPIRE_PASSWD}
  UF_DONT_EXPIRE_PASSWD           = $10000;
  {$EXTERNALSYM UF_MNS_LOGON_ACCOUNT}
  UF_MNS_LOGON_ACCOUNT            = $20000;
  {$EXTERNALSYM UF_SMARTCARD_REQUIRED}
  UF_SMARTCARD_REQUIRED           = $40000;
  {$EXTERNALSYM UF_TRUSTED_FOR_DELEGATION}
  UF_TRUSTED_FOR_DELEGATION       = $80000;
  {$EXTERNALSYM UF_NOT_DELEGATED}
  UF_NOT_DELEGATED               = $100000;
  {$EXTERNALSYM UF_USE_DES_KEY_ONLY}
  UF_USE_DES_KEY_ONLY            = $200000;
  {$EXTERNALSYM UF_DONT_REQUIRE_PREAUTH}
  UF_DONT_REQUIRE_PREAUTH        = $400000;

  {$EXTERNALSYM UF_SETTABLE_BITS}
  UF_SETTABLE_BITS = UF_SCRIPT or UF_ACCOUNTDISABLE or UF_LOCKOUT or
    UF_HOMEDIR_REQUIRED or UF_PASSWD_NOTREQD or UF_PASSWD_CANT_CHANGE or
    UF_ACCOUNT_TYPE_MASK or UF_DONT_EXPIRE_PASSWD or UF_MNS_LOGON_ACCOUNT or
    UF_ENCRYPTED_TEXT_PASSWORD_ALLOWED or UF_SMARTCARD_REQUIRED or
    UF_TRUSTED_FOR_DELEGATION or UF_NOT_DELEGATED or UF_USE_DES_KEY_ONLY or
    UF_DONT_REQUIRE_PREAUTH;

// bit masks for the NetUserEnum filter parameter.

  {$EXTERNALSYM FILTER_TEMP_DUPLICATE_ACCOUNT}
  FILTER_TEMP_DUPLICATE_ACCOUNT       = $0001;
  {$EXTERNALSYM FILTER_NORMAL_ACCOUNT}
  FILTER_NORMAL_ACCOUNT               = $0002;
  {$EXTERNALSYM FILTER_PROXY_ACCOUNT}
  FILTER_PROXY_ACCOUNT                = $0004;
  {$EXTERNALSYM FILTER_INTERDOMAIN_TRUST_ACCOUNT}
  FILTER_INTERDOMAIN_TRUST_ACCOUNT    = $0008;
  {$EXTERNALSYM FILTER_WORKSTATION_TRUST_ACCOUNT}
  FILTER_WORKSTATION_TRUST_ACCOUNT    = $0010;
  {$EXTERNALSYM FILTER_SERVER_TRUST_ACCOUNT}
  FILTER_SERVER_TRUST_ACCOUNT         = $0020;

// bit masks for the NetUserGetLocalGroups flags

  {$EXTERNALSYM LG_INCLUDE_INDIRECT}
  LG_INCLUDE_INDIRECT         = $0001;

//  Bit masks for field usri2_auth_flags of USER_INFO_2.

  {$EXTERNALSYM AF_OP_PRINT}
  AF_OP_PRINT             = $1;
  {$EXTERNALSYM AF_OP_COMM}
  AF_OP_COMM              = $2;
  {$EXTERNALSYM AF_OP_SERVER}
  AF_OP_SERVER            = $4;
  {$EXTERNALSYM AF_OP_ACCOUNTS}
  AF_OP_ACCOUNTS          = $8;
  {$EXTERNALSYM AF_SETTABLE_BITS}
  AF_SETTABLE_BITS = AF_OP_PRINT or AF_OP_COMM or AF_OP_SERVER or AF_OP_ACCOUNTS;

//  UAS role manifests under NETLOGON

  {$EXTERNALSYM UAS_ROLE_STANDALONE}
  UAS_ROLE_STANDALONE     = 0;
  {$EXTERNALSYM UAS_ROLE_MEMBER}
  UAS_ROLE_MEMBER         = 1;
  {$EXTERNALSYM UAS_ROLE_BACKUP}
  UAS_ROLE_BACKUP         = 2;
  {$EXTERNALSYM UAS_ROLE_PRIMARY}
  UAS_ROLE_PRIMARY        = 3;

//  Values for ParmError for NetUserSetInfo.

  {$EXTERNALSYM USER_NAME_PARMNUM}
  USER_NAME_PARMNUM               = 1;
  {$EXTERNALSYM USER_PASSWORD_PARMNUM}
  USER_PASSWORD_PARMNUM           = 3;
  {$EXTERNALSYM USER_PASSWORD_AGE_PARMNUM}
  USER_PASSWORD_AGE_PARMNUM       = 4;
  {$EXTERNALSYM USER_PRIV_PARMNUM}
  USER_PRIV_PARMNUM               = 5;
  {$EXTERNALSYM USER_HOME_DIR_PARMNUM}
  USER_HOME_DIR_PARMNUM           = 6;
  {$EXTERNALSYM USER_COMMENT_PARMNUM}
  USER_COMMENT_PARMNUM            = 7;
  {$EXTERNALSYM USER_FLAGS_PARMNUM}
  USER_FLAGS_PARMNUM              = 8;
  {$EXTERNALSYM USER_SCRIPT_PATH_PARMNUM}
  USER_SCRIPT_PATH_PARMNUM        = 9;
  {$EXTERNALSYM USER_AUTH_FLAGS_PARMNUM}
  USER_AUTH_FLAGS_PARMNUM         = 10;
  {$EXTERNALSYM USER_FULL_NAME_PARMNUM}
  USER_FULL_NAME_PARMNUM          = 11;
  {$EXTERNALSYM USER_USR_COMMENT_PARMNUM}
  USER_USR_COMMENT_PARMNUM        = 12;
  {$EXTERNALSYM USER_PARMS_PARMNUM}
  USER_PARMS_PARMNUM              = 13;
  {$EXTERNALSYM USER_WORKSTATIONS_PARMNUM}
  USER_WORKSTATIONS_PARMNUM       = 14;
  {$EXTERNALSYM USER_LAST_LOGON_PARMNUM}
  USER_LAST_LOGON_PARMNUM         = 15;
  {$EXTERNALSYM USER_LAST_LOGOFF_PARMNUM}
  USER_LAST_LOGOFF_PARMNUM        = 16;
  {$EXTERNALSYM USER_ACCT_EXPIRES_PARMNUM}
  USER_ACCT_EXPIRES_PARMNUM       = 17;
  {$EXTERNALSYM USER_MAX_STORAGE_PARMNUM}
  USER_MAX_STORAGE_PARMNUM        = 18;
  {$EXTERNALSYM USER_UNITS_PER_WEEK_PARMNUM}
  USER_UNITS_PER_WEEK_PARMNUM     = 19;
  {$EXTERNALSYM USER_LOGON_HOURS_PARMNUM}
  USER_LOGON_HOURS_PARMNUM        = 20;
  {$EXTERNALSYM USER_PAD_PW_COUNT_PARMNUM}
  USER_PAD_PW_COUNT_PARMNUM       = 21;
  {$EXTERNALSYM USER_NUM_LOGONS_PARMNUM}
  USER_NUM_LOGONS_PARMNUM         = 22;
  {$EXTERNALSYM USER_LOGON_SERVER_PARMNUM}
  USER_LOGON_SERVER_PARMNUM       = 23;
  {$EXTERNALSYM USER_COUNTRY_CODE_PARMNUM}
  USER_COUNTRY_CODE_PARMNUM       = 24;
  {$EXTERNALSYM USER_CODE_PAGE_PARMNUM}
  USER_CODE_PAGE_PARMNUM          = 25;
  {$EXTERNALSYM USER_PRIMARY_GROUP_PARMNUM}
  USER_PRIMARY_GROUP_PARMNUM      = 51;
  {$EXTERNALSYM USER_PROFILE}
  USER_PROFILE                    = 52; // ?? Delete when convenient
  {$EXTERNALSYM USER_PROFILE_PARMNUM}
  USER_PROFILE_PARMNUM            = 52;
  {$EXTERNALSYM USER_HOME_DIR_DRIVE_PARMNUM}
  USER_HOME_DIR_DRIVE_PARMNUM     = 53;

// the new infolevel counterparts of the old info level + parmnum

  {$EXTERNALSYM USER_NAME_INFOLEVEL}
  USER_NAME_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + USER_NAME_PARMNUM);
  {$EXTERNALSYM USER_PASSWORD_INFOLEVEL}
  USER_PASSWORD_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + USER_PASSWORD_PARMNUM);
  {$EXTERNALSYM USER_PASSWORD_AGE_INFOLEVEL}
  USER_PASSWORD_AGE_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + USER_PASSWORD_AGE_PARMNUM);
  {$EXTERNALSYM USER_PRIV_INFOLEVEL}
  USER_PRIV_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + USER_PRIV_PARMNUM);
  {$EXTERNALSYM USER_HOME_DIR_INFOLEVEL}
  USER_HOME_DIR_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + USER_HOME_DIR_PARMNUM);
  {$EXTERNALSYM USER_COMMENT_INFOLEVEL}
  USER_COMMENT_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + USER_COMMENT_PARMNUM);
  {$EXTERNALSYM USER_FLAGS_INFOLEVEL}
  USER_FLAGS_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + USER_FLAGS_PARMNUM);
  {$EXTERNALSYM USER_SCRIPT_PATH_INFOLEVEL}
  USER_SCRIPT_PATH_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + USER_SCRIPT_PATH_PARMNUM);
  {$EXTERNALSYM USER_AUTH_FLAGS_INFOLEVEL}
  USER_AUTH_FLAGS_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + USER_AUTH_FLAGS_PARMNUM);
  {$EXTERNALSYM USER_FULL_NAME_INFOLEVEL}
  USER_FULL_NAME_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + USER_FULL_NAME_PARMNUM);
  {$EXTERNALSYM USER_USR_COMMENT_INFOLEVEL}
  USER_USR_COMMENT_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + USER_USR_COMMENT_PARMNUM);
  {$EXTERNALSYM USER_PARMS_INFOLEVEL}
  USER_PARMS_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + USER_PARMS_PARMNUM);
  {$EXTERNALSYM USER_WORKSTATIONS_INFOLEVEL}
  USER_WORKSTATIONS_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + USER_WORKSTATIONS_PARMNUM);
  {$EXTERNALSYM USER_LAST_LOGON_INFOLEVEL}
  USER_LAST_LOGON_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + USER_LAST_LOGON_PARMNUM);
  {$EXTERNALSYM USER_LAST_LOGOFF_INFOLEVEL}
  USER_LAST_LOGOFF_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + USER_LAST_LOGOFF_PARMNUM);
  {$EXTERNALSYM USER_ACCT_EXPIRES_INFOLEVEL}
  USER_ACCT_EXPIRES_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + USER_ACCT_EXPIRES_PARMNUM);
  {$EXTERNALSYM USER_MAX_STORAGE_INFOLEVEL}
  USER_MAX_STORAGE_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + USER_MAX_STORAGE_PARMNUM);
  {$EXTERNALSYM USER_UNITS_PER_WEEK_INFOLEVEL}
  USER_UNITS_PER_WEEK_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + USER_UNITS_PER_WEEK_PARMNUM);
  {$EXTERNALSYM USER_LOGON_HOURS_INFOLEVEL}
  USER_LOGON_HOURS_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + USER_LOGON_HOURS_PARMNUM);
  {$EXTERNALSYM USER_PAD_PW_COUNT_INFOLEVEL}
  USER_PAD_PW_COUNT_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + USER_PAD_PW_COUNT_PARMNUM);
  {$EXTERNALSYM USER_NUM_LOGONS_INFOLEVEL}
  USER_NUM_LOGONS_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + USER_NUM_LOGONS_PARMNUM);
  {$EXTERNALSYM USER_LOGON_SERVER_INFOLEVEL}
  USER_LOGON_SERVER_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + USER_LOGON_SERVER_PARMNUM);
  {$EXTERNALSYM USER_COUNTRY_CODE_INFOLEVEL}
  USER_COUNTRY_CODE_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + USER_COUNTRY_CODE_PARMNUM);
  {$EXTERNALSYM USER_CODE_PAGE_INFOLEVEL}
  USER_CODE_PAGE_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + USER_CODE_PAGE_PARMNUM);
  {$EXTERNALSYM USER_PRIMARY_GROUP_INFOLEVEL}
  USER_PRIMARY_GROUP_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + USER_PRIMARY_GROUP_PARMNUM);
//  {$EXTERNALSYM USER_POSIX_ID_INFOLEVEL}
//  USER_POSIX_ID_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + USER_POSIX_ID_PARMNUM);
  {$EXTERNALSYM USER_HOME_DIR_DRIVE_INFOLEVEL}
  USER_HOME_DIR_DRIVE_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + USER_HOME_DIR_DRIVE_PARMNUM);

//  For SetInfo call (parmnum 0) when password change not required

  {$EXTERNALSYM NULL_USERSETINFO_PASSWD}
  NULL_USERSETINFO_PASSWD =    '              ';

  {$EXTERNALSYM TIMEQ_FOREVER}
  TIMEQ_FOREVER               = DWORD(-1);
  {$EXTERNALSYM USER_MAXSTORAGE_UNLIMITED}
  USER_MAXSTORAGE_UNLIMITED   = DWORD(-1);
  {$EXTERNALSYM USER_NO_LOGOFF}
  USER_NO_LOGOFF              = DWORD(-1);
  {$EXTERNALSYM UNITS_PER_DAY}
  UNITS_PER_DAY               = 24;
  {$EXTERNALSYM UNITS_PER_WEEK}
  UNITS_PER_WEEK              = UNITS_PER_DAY * 7;

// Privilege levels (USER_INFO_X field usriX_priv (X = 0/1)).

  {$EXTERNALSYM USER_PRIV_MASK}
  USER_PRIV_MASK      = $3;
  {$EXTERNALSYM USER_PRIV_GUEST}
  USER_PRIV_GUEST     = 0;
  {$EXTERNALSYM USER_PRIV_USER}
  USER_PRIV_USER      = 1;
  {$EXTERNALSYM USER_PRIV_ADMIN}
  USER_PRIV_ADMIN     = 2;

// user modals related defaults

  {$EXTERNALSYM MAX_PASSWD_LEN}
  MAX_PASSWD_LEN      = PWLEN;
  {$EXTERNALSYM DEF_MIN_PWLEN}
  DEF_MIN_PWLEN       = 6;
  {$EXTERNALSYM DEF_PWUNIQUENESS}
  DEF_PWUNIQUENESS    = 5;
  {$EXTERNALSYM DEF_MAX_PWHIST}
  DEF_MAX_PWHIST      = 8;

  {$EXTERNALSYM DEF_MAX_PWAGE}
  DEF_MAX_PWAGE       = TIMEQ_FOREVER; // forever
  {$EXTERNALSYM DEF_MIN_PWAGE}
  DEF_MIN_PWAGE       = 0;             // 0 days
  {$EXTERNALSYM DEF_FORCE_LOGOFF}
  DEF_FORCE_LOGOFF    = MAXDWORD;      // never
  {$EXTERNALSYM DEF_MAX_BADPW}
  DEF_MAX_BADPW       = 0;             // no limit
  {$EXTERNALSYM ONE_DAY}
  ONE_DAY             = 01*24*3600;    // 01 day

// User Logon Validation (codes returned)

  {$EXTERNALSYM VALIDATED_LOGON}
  VALIDATED_LOGON         = 0;
  {$EXTERNALSYM PASSWORD_EXPIRED}
  PASSWORD_EXPIRED        = 2;
  {$EXTERNALSYM NON_VALIDATED_LOGON}
  NON_VALIDATED_LOGON     = 3;

  {$EXTERNALSYM VALID_LOGOFF}
  VALID_LOGOFF            = 1;

// parmnum manifests for user modals

  {$EXTERNALSYM MODALS_MIN_PASSWD_LEN_PARMNUM}
  MODALS_MIN_PASSWD_LEN_PARMNUM       = 1;
  {$EXTERNALSYM MODALS_MAX_PASSWD_AGE_PARMNUM}
  MODALS_MAX_PASSWD_AGE_PARMNUM       = 2;
  {$EXTERNALSYM MODALS_MIN_PASSWD_AGE_PARMNUM}
  MODALS_MIN_PASSWD_AGE_PARMNUM       = 3;
  {$EXTERNALSYM MODALS_FORCE_LOGOFF_PARMNUM}
  MODALS_FORCE_LOGOFF_PARMNUM         = 4;
  {$EXTERNALSYM MODALS_PASSWD_HIST_LEN_PARMNUM}
  MODALS_PASSWD_HIST_LEN_PARMNUM      = 5;
  {$EXTERNALSYM MODALS_ROLE_PARMNUM}
  MODALS_ROLE_PARMNUM                 = 6;
  {$EXTERNALSYM MODALS_PRIMARY_PARMNUM}
  MODALS_PRIMARY_PARMNUM              = 7;
  {$EXTERNALSYM MODALS_DOMAIN_NAME_PARMNUM}
  MODALS_DOMAIN_NAME_PARMNUM          = 8;
  {$EXTERNALSYM MODALS_DOMAIN_ID_PARMNUM}
  MODALS_DOMAIN_ID_PARMNUM            = 9;
  {$EXTERNALSYM MODALS_LOCKOUT_DURATION_PARMNUM}
  MODALS_LOCKOUT_DURATION_PARMNUM     = 10;
  {$EXTERNALSYM MODALS_LOCKOUT_OBSERVATION_WINDOW_PARMNUM}
  MODALS_LOCKOUT_OBSERVATION_WINDOW_PARMNUM = 11;
  {$EXTERNALSYM MODALS_LOCKOUT_THRESHOLD_PARMNUM}
  MODALS_LOCKOUT_THRESHOLD_PARMNUM    = 12;

// the new infolevel counterparts of the old info level + parmnum

  {$EXTERNALSYM MODALS_MIN_PASSWD_LEN_INFOLEVEL}
  MODALS_MIN_PASSWD_LEN_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + MODALS_MIN_PASSWD_LEN_PARMNUM);
  {$EXTERNALSYM MODALS_MAX_PASSWD_AGE_INFOLEVEL}
  MODALS_MAX_PASSWD_AGE_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + MODALS_MAX_PASSWD_AGE_PARMNUM);
  {$EXTERNALSYM MODALS_MIN_PASSWD_AGE_INFOLEVEL}
  MODALS_MIN_PASSWD_AGE_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + MODALS_MIN_PASSWD_AGE_PARMNUM);
  {$EXTERNALSYM MODALS_FORCE_LOGOFF_INFOLEVEL}
  MODALS_FORCE_LOGOFF_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + MODALS_FORCE_LOGOFF_PARMNUM);
  {$EXTERNALSYM MODALS_PASSWD_HIST_LEN_INFOLEVEL}
  MODALS_PASSWD_HIST_LEN_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + MODALS_PASSWD_HIST_LEN_PARMNUM);
  {$EXTERNALSYM MODALS_ROLE_INFOLEVEL}
  MODALS_ROLE_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + MODALS_ROLE_PARMNUM);
  {$EXTERNALSYM MODALS_PRIMARY_INFOLEVEL}
  MODALS_PRIMARY_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + MODALS_PRIMARY_PARMNUM);
  {$EXTERNALSYM MODALS_DOMAIN_NAME_INFOLEVEL}
  MODALS_DOMAIN_NAME_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + MODALS_DOMAIN_NAME_PARMNUM);
  {$EXTERNALSYM MODALS_DOMAIN_ID_INFOLEVEL}
  MODALS_DOMAIN_ID_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + MODALS_DOMAIN_ID_PARMNUM);


// Group Class

// Function Prototypes

{$EXTERNALSYM NetGroupAdd}
function NetGroupAdd(servername: LPCWSTR; level: DWORD; buf: Pointer;
  parm_err: PDWORD): NET_API_STATUS; stdcall;

{$EXTERNALSYM NetGroupAddUser}
function NetGroupAddUser(servername: LPCWSTR; GroupName: LPCWSTR;
  username: LPCWSTR): NET_API_STATUS; stdcall;

{$EXTERNALSYM NetGroupEnum}
function NetGroupEnum(servername: LPCWSTR; level: DWORD; var bufptr: Pointer;
  prefmaxlen: DWORD; var entriesread: DWORD; var totalentries: DWORD;
  resume_handle: PDWORD): NET_API_STATUS; stdcall;

{$EXTERNALSYM NetGroupGetInfo}
function NetGroupGetInfo(servername: LPCWSTR; groupname: LPCWSTR; level: DWORD;
  var bufptr: Pointer): NET_API_STATUS; stdcall;

{$EXTERNALSYM NetGroupSetInfo}
function NetGroupSetInfo(servername: LPCWSTR; groupname: LPCWSTR; level: DWORD;
  buf: Pointer; parm_err: PDWORD): NET_API_STATUS; stdcall;

{$EXTERNALSYM NetGroupDel}
function NetGroupDel(servername, groupname: LPCWSTR): NET_API_STATUS; stdcall;

{$EXTERNALSYM NetGroupDelUser}
function NetGroupDelUser(servername: LPCWSTR; GroupName: LPCWSTR;
  Username: LPCWSTR): NET_API_STATUS; stdcall;

{$EXTERNALSYM NetGroupGetUsers}
function NetGroupGetUsers(servername: LPCWSTR; groupname: LPCWSTR; level: DWORD;
  var bufptr: Pointer; prefmaxlen: DWORD; var entriesread: DWORD;
  var totalentries: DWORD; ResumeHandle: PDWORD): NET_API_STATUS; stdcall;

{$EXTERNALSYM NetGroupSetUsers}
function NetGroupSetUsers(servername: LPCWSTR; groupname: LPCWSTR; level: DWORD;
  buf: Pointer; totalentries: DWORD): NET_API_STATUS; stdcall;

//  Data Structures - Group

type
  PGroupInfo0 = ^TGroupInfo0;
  {$EXTERNALSYM _GROUP_INFO_0}
  _GROUP_INFO_0 = record
    grpi0_name: LPWSTR;
  end;
  TGroupInfo0 = _GROUP_INFO_0;
  {$EXTERNALSYM GROUP_INFO_0}
  GROUP_INFO_0 = _GROUP_INFO_0;

  PGroupInfo1 = ^TGroupInfo1;
  {$EXTERNALSYM _GROUP_INFO_1}
  _GROUP_INFO_1 = record
    grpi1_name: LPWSTR;
    grpi1_comment: LPWSTR;
  end;
  TGroupInfo1 = _GROUP_INFO_1;
  {$EXTERNALSYM GROUP_INFO_1}
  GROUP_INFO_1 = _GROUP_INFO_1;

  PGroupInfo2 = ^TGroupInfo2;
  {$EXTERNALSYM _GROUP_INFO_2}
  _GROUP_INFO_2 = record
    grpi2_name: LPWSTR;
    grpi2_comment: LPWSTR;
    grpi2_group_id: DWORD;
    grpi2_attributes: DWORD;
  end;
  TGroupInfo2 = _GROUP_INFO_2;
  {$EXTERNALSYM GROUP_INFO_2}
  GROUP_INFO_2 = _GROUP_INFO_2;

  PGroupInfo1002 = ^TGroupInfo1002;
  {$EXTERNALSYM _GROUP_INFO_1002}
  _GROUP_INFO_1002 = record
    grpi1002_comment: LPWSTR;
  end;
  TGroupInfo1002 = _GROUP_INFO_1002;
  {$EXTERNALSYM GROUP_INFO_1002}
  GROUP_INFO_1002 = _GROUP_INFO_1002;

  PGroupInfo1005 = ^TGroupInfo1005;
  {$EXTERNALSYM _GROUP_INFO_1005}
  _GROUP_INFO_1005 = record
    grpi1005_attributes: DWORD;
  end;
  TGroupInfo1005 = _GROUP_INFO_1005;
  {$EXTERNALSYM GROUP_INFO_1005}
  GROUP_INFO_1005 = _GROUP_INFO_1005;

  PGroupUsersInfo0 = ^TGroupUsersInfo0;
  {$EXTERNALSYM _GROUP_USERS_INFO_0}
  _GROUP_USERS_INFO_0 = record
    grui0_name: LPWSTR;
  end;
  TGroupUsersInfo0 = _GROUP_USERS_INFO_0;
  {$EXTERNALSYM GROUP_USERS_INFO_0}
  GROUP_USERS_INFO_0 = _GROUP_USERS_INFO_0;

  PGroupUsersInfo1 = ^TGroupUsersInfo1;
  {$EXTERNALSYM _GROUP_USERS_INFO_1}
  _GROUP_USERS_INFO_1 = record
    grui1_name: LPWSTR;
    grui1_attributes: DWORD;
  end;
  TGroupUsersInfo1 = _GROUP_USERS_INFO_1;
  {$EXTERNALSYM GROUP_USERS_INFO_1}
  GROUP_USERS_INFO_1 = _GROUP_USERS_INFO_1;

//
// Special Values and Constants - Group
//

const
  {$EXTERNALSYM GROUPIDMASK}
  GROUPIDMASK                 = $8000;      // MSB set if uid refers;
                                            // to a group

// Predefined group for all normal users, administrators and guests
// LOCAL is a special group for pinball local security.

  {$EXTERNALSYM GROUP_SPECIALGRP_USERS}
  GROUP_SPECIALGRP_USERS = 'USERS';
  {$EXTERNALSYM GROUP_SPECIALGRP_ADMINS}
  GROUP_SPECIALGRP_ADMINS = 'ADMINS';
  {$EXTERNALSYM GROUP_SPECIALGRP_GUESTS}
  GROUP_SPECIALGRP_GUESTS = 'GUESTS';
  {$EXTERNALSYM GROUP_SPECIALGRP_LOCAL}
  GROUP_SPECIALGRP_LOCAL = 'LOCAL';

// parmnum manifests for SetInfo calls (only comment is settable)

  {$EXTERNALSYM GROUP_ALL_PARMNUM}
  GROUP_ALL_PARMNUM           = 0;
  {$EXTERNALSYM GROUP_NAME_PARMNUM}
  GROUP_NAME_PARMNUM          = 1;
  {$EXTERNALSYM GROUP_COMMENT_PARMNUM}
  GROUP_COMMENT_PARMNUM       = 2;
  {$EXTERNALSYM GROUP_ATTRIBUTES_PARMNUM}
  GROUP_ATTRIBUTES_PARMNUM    = 3;

// the new infolevel counterparts of the old info level + parmnum

  {$EXTERNALSYM GROUP_ALL_INFOLEVEL}
  GROUP_ALL_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + GROUP_ALL_PARMNUM);
  {$EXTERNALSYM GROUP_NAME_INFOLEVEL}
  GROUP_NAME_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + GROUP_NAME_PARMNUM);
  {$EXTERNALSYM GROUP_COMMENT_INFOLEVEL}
  GROUP_COMMENT_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + GROUP_COMMENT_PARMNUM);
  {$EXTERNALSYM GROUP_ATTRIBUTES_INFOLEVEL}
  GROUP_ATTRIBUTES_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + GROUP_ATTRIBUTES_PARMNUM);
//  {$EXTERNALSYM GROUP_POSIX_ID_INFOLEVEL}
//  GROUP_POSIX_ID_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + GROUP_POSIX_ID_PARMNUM);

// LocalGroup Class

// Function Prototypes

{$EXTERNALSYM NetLocalGroupAdd}
function NetLocalGroupAdd(servername: LPCWSTR; level: DWORD; buf: Pointer;
  parm_err: PDWORD): NET_API_STATUS; stdcall;

{$EXTERNALSYM NetLocalGroupAddMember}
function NetLocalGroupAddMember(servername: LPCWSTR; groupname: LPCWSTR;
  membersid: PSID): NET_API_STATUS; stdcall;

{$EXTERNALSYM NetLocalGroupEnum}
function NetLocalGroupEnum(servername: LPCWSTR; level: DWORD; var bufptr: Pointer;
  prefmaxlen: DWORD; var entriesread: DWORD; var totalentries: DWORD;
  resumehandle: PDWORD): NET_API_STATUS; stdcall;

{$EXTERNALSYM NetLocalGroupGetInfo}
function NetLocalGroupGetInfo(servername: LPCWSTR; groupname: LPCWSTR;
  level: DWORD; var bufptr: Pointer): NET_API_STATUS; stdcall;

{$EXTERNALSYM NetLocalGroupSetInfo}
function NetLocalGroupSetInfo(servername: LPCWSTR; groupname: LPCWSTR;
  level: DWORD; buf: Pointer; parm_err: PDWORD): NET_API_STATUS; stdcall;

{$EXTERNALSYM NetLocalGroupDel}
function NetLocalGroupDel(servername, groupname: LPCWSTR): NET_API_STATUS; stdcall;

{$EXTERNALSYM NetLocalGroupDelMember}
function NetLocalGroupDelMember(servername: LPCWSTR; groupname: LPCWSTR;
  membersid: PSID): NET_API_STATUS; stdcall;

{$EXTERNALSYM NetLocalGroupGetMembers}
function NetLocalGroupGetMembers(servername: LPCWSTR; localgroupname: LPCWSTR;
  level: DWORD; var bufptr: Pointer; prefmaxlen: DWORD; var entriesread: DWORD;
  var totalentries: DWORD; resumehandle: PDWORD): NET_API_STATUS; stdcall;

{$EXTERNALSYM NetLocalGroupSetMembers}
function NetLocalGroupSetMembers(servername: LPCWSTR; groupname: LPCWSTR;
  level: DWORD; buf: Pointer; totalentries: DWORD): NET_API_STATUS; stdcall;

{$EXTERNALSYM NetLocalGroupAddMembers}
function NetLocalGroupAddMembers(servername: LPCWSTR; groupname: LPCWSTR;
  level: DWORD; buf: Pointer; totalentries: DWORD): NET_API_STATUS; stdcall;

{$EXTERNALSYM NetLocalGroupDelMembers}
function NetLocalGroupDelMembers(servername: LPCWSTR; groupname: LPCWSTR;
  level: DWORD; buf: Pointer; totalentries: DWORD): NET_API_STATUS; stdcall;

//  Data Structures - LocalGroup

type
  PLocalGroupInfo0 = ^TLocalGroupInfo0;
  {$EXTERNALSYM _LOCALGROUP_INFO_0}
  _LOCALGROUP_INFO_0 = record
    lgrpi0_name: LPWSTR;
  end;
  TLocalGroupInfo0 = _LOCALGROUP_INFO_0;
  {$EXTERNALSYM LOCALGROUP_INFO_0}
  LOCALGROUP_INFO_0 = _LOCALGROUP_INFO_0;

  PLocalGroupInfo1 = ^TLocalGroupInfo1;
  {$EXTERNALSYM _LOCALGROUP_INFO_1}
  _LOCALGROUP_INFO_1 = record
    lgrpi1_name: LPWSTR;
    lgrpi1_comment: LPWSTR;
  end;
  TLocalGroupInfo1 = _LOCALGROUP_INFO_1;
  {$EXTERNALSYM LOCALGROUP_INFO_1}
  LOCALGROUP_INFO_1 = _LOCALGROUP_INFO_1;

  PLocalGroupInfo1002 = ^TLocalGroupInfo1002;
  {$EXTERNALSYM _LOCALGROUP_INFO_1002}
  _LOCALGROUP_INFO_1002 = record
    lgrpi1002_comment: LPWSTR;
  end;
  TLocalGroupInfo1002 = _LOCALGROUP_INFO_1002;
  {$EXTERNALSYM LOCALGROUP_INFO_1002}
  LOCALGROUP_INFO_1002 = _LOCALGROUP_INFO_1002;

  PLocalGroupMembersInfo0 = ^TLocalGroupMembersInfo0;
  {$EXTERNALSYM _LOCALGROUP_MEMBERS_INFO_0}
  _LOCALGROUP_MEMBERS_INFO_0 = record
    lgrmi0_sid: PSID;
  end;
  TLocalGroupMembersInfo0 = _LOCALGROUP_MEMBERS_INFO_0;
  {$EXTERNALSYM LOCALGROUP_MEMBERS_INFO_0}
  LOCALGROUP_MEMBERS_INFO_0 = _LOCALGROUP_MEMBERS_INFO_0;

  PLocalGroupMembersInfo1 = ^TLocalGroupMembersInfo1;
  {$EXTERNALSYM _LOCALGROUP_MEMBERS_INFO_1}
  _LOCALGROUP_MEMBERS_INFO_1 = record
    lgrmi1_sid: PSID;
    lgrmi1_sidusage: SID_NAME_USE;
    lgrmi1_name: LPWSTR;
  end;
  TLocalGroupMembersInfo1 = _LOCALGROUP_MEMBERS_INFO_1;
  {$EXTERNALSYM LOCALGROUP_MEMBERS_INFO_1}
  LOCALGROUP_MEMBERS_INFO_1 = _LOCALGROUP_MEMBERS_INFO_1;

  PLocalGroupMembersInfo2 = ^TLocalGroupMembersInfo2;
  {$EXTERNALSYM _LOCALGROUP_MEMBERS_INFO_2}
  _LOCALGROUP_MEMBERS_INFO_2 = record
    lgrmi2_sid: PSID;
    lgrmi2_sidusage: SID_NAME_USE;
    lgrmi2_domainandname: LPWSTR;
  end;
  TLocalGroupMembersInfo2 = _LOCALGROUP_MEMBERS_INFO_2;
  {$EXTERNALSYM LOCALGROUP_MEMBERS_INFO_2}
  LOCALGROUP_MEMBERS_INFO_2 = _LOCALGROUP_MEMBERS_INFO_2;

  PLocalGroupMembersInfo3 = ^TLocalGroupMembersInfo3;
  {$EXTERNALSYM _LOCALGROUP_MEMBERS_INFO_3}
  _LOCALGROUP_MEMBERS_INFO_3 = record
    lgrmi3_domainandname: LPWSTR;
  end;
  TLocalGroupMembersInfo3 = _LOCALGROUP_MEMBERS_INFO_3;
  {$EXTERNALSYM LOCALGROUP_MEMBERS_INFO_3}
  LOCALGROUP_MEMBERS_INFO_3 = _LOCALGROUP_MEMBERS_INFO_3;

  PLocalGroupUserInfo0 = ^TLocalGroupUserInfo0;
  {$EXTERNALSYM _LOCALGROUP_USERS_INFO_0}
  _LOCALGROUP_USERS_INFO_0 = record
    lgrui0_name: LPWSTR;
  end;
  TLocalGroupUserInfo0 = _LOCALGROUP_USERS_INFO_0;
  {$EXTERNALSYM LOCALGROUP_USERS_INFO_0}
  LOCALGROUP_USERS_INFO_0 = _LOCALGROUP_USERS_INFO_0;

const
  {$EXTERNALSYM LOCALGROUP_NAME_PARMNUM}
  LOCALGROUP_NAME_PARMNUM = 1;
  {$EXTERNALSYM LOCALGROUP_COMMENT_PARMNUM}
  LOCALGROUP_COMMENT_PARMNUM = 2;

// Display Information APIs

{$EXTERNALSYM NetQueryDisplayInformation}
function NetQueryDisplayInformation(ServerName: LPCWSTR; Level: DWORD;
  Index: DWORD; EntriesRequested: DWORD; PreferredMaximumLength: DWORD;
  var ReturnedEntryCount: DWORD; var SortedBuffer: Pointer): NET_API_STATUS; stdcall;

{$EXTERNALSYM NetGetDisplayInformationIndex}
function NetGetDisplayInformationIndex(ServerName: LPCWSTR; Level: DWORD;
  Prefix: LPCWSTR; var Index: DWORD): NET_API_STATUS; stdcall;

// QueryDisplayInformation levels

type
  PNetDisplayUser = ^TNetDisplayUser;
  {$EXTERNALSYM _NET_DISPLAY_USER}
  _NET_DISPLAY_USER = record
    usri1_name: LPWSTR;
    usri1_comment: LPWSTR;
    usri1_flags: DWORD;
    usri1_full_name: LPWSTR;
    usri1_user_id: DWORD;
    usri1_next_index: DWORD;
  end;
  TNetDisplayUser = _NET_DISPLAY_USER;
  {$EXTERNALSYM NET_DISPLAY_USER}
  NET_DISPLAY_USER = _NET_DISPLAY_USER;

  PNetDisplayMachine = ^TNetDisplayMachine;
  {$EXTERNALSYM _NET_DISPLAY_MACHINE}
  _NET_DISPLAY_MACHINE = record
    usri2_name: LPWSTR;
    usri2_comment: LPWSTR;
    usri2_flags: DWORD;
    usri2_user_id: DWORD;
    usri2_next_index: DWORD;
  end;
  TNetDisplayMachine = _NET_DISPLAY_MACHINE;
  {$EXTERNALSYM NET_DISPLAY_MACHINE}
  NET_DISPLAY_MACHINE = _NET_DISPLAY_MACHINE;

  PNetDisplayGroup = ^TNetDisplayGroup;
  {$EXTERNALSYM _NET_DISPLAY_GROUP}
  _NET_DISPLAY_GROUP = record
    grpi3_name: LPWSTR;
    grpi3_comment: LPWSTR;
    grpi3_group_id: DWORD;
    grpi3_attributes: DWORD;
    grpi3_next_index: DWORD;
  end;
  TNetDisplayGroup = _NET_DISPLAY_GROUP;
  {$EXTERNALSYM NET_DISPLAY_GROUP}
  NET_DISPLAY_GROUP = _NET_DISPLAY_GROUP;

// Access Class

// Function Prototypes - Access

// The NetAccess APIs are only available to downlevel

//#define NetAccessAdd RxNetAccessAdd

{$EXTERNALSYM NetAccessAdd}
function NetAccessAdd(servername: LPCWSTR; level: DWORD; buf: Pointer;
  parm_err: PDWORD): NET_API_STATUS; stdcall;

//#define NetAccessEnum RxNetAccessEnum

{$EXTERNALSYM NetAccessEnum}
function NetAccessEnum(servername: LPCWSTR; BasePath: LPCWSTR; Recursive: DWORD;
  level: DWORD; bufptr: Pointer; prefmaxlen: DWORD; var entriesread: DWORD;
  var totalentries: DWORD; resume_handle: PDWORD): NET_API_STATUS; stdcall;

//#define NetAccessGetInfo RxNetAccessGetInfo

{$EXTERNALSYM NetAccessGetInfo}
function NetAccessGetInfo(servername: LPCWSTR; resource: LPCWSTR; level: DWORD;
  bufptr: Pointer): NET_API_STATUS; stdcall;

//#define NetAccessSetInfo RxNetAccessSetInfo

{$EXTERNALSYM NetAccessSetInfo}
function NetAccessSetInfo(servername: LPCWSTR; resource: LPCWSTR; level: DWORD;
  buf: Pointer; parm_err: PDWORD): NET_API_STATUS; stdcall;

//#define NetAccessDel RxNetAccessDel

{$EXTERNALSYM NetAccessDel}
function NetAccessDel(servername, resource: LPCWSTR): NET_API_STATUS; stdcall;

//#define NetAccessGetUserPerms RxNetAccessGetUserPerms

{$EXTERNALSYM NetAccessGetUserPerms}
function NetAccessGetUserPerms(servername: LPCWSTR; UGname: LPCWSTR;
  resource: LPCWSTR; var Perms: DWORD): NET_API_STATUS; stdcall;

//
// Data Structures - Access
//
type
  PAccessInfo0 = ^TAccessInfo0;
  {EXTERNALSYM _ACCESS_INFO_0}
  _ACCESS_INFO_0 = record
    acc0_resource_name: LPWSTR;
  end;
  TAccessInfo0 = _ACCESS_INFO_0;
  {$EXTERNALSYM ACCESS_INFO_0}
  ACCESS_INFO_0 = _ACCESS_INFO_0;

  PAccessInfo1 = ^TAccessInfo1;
  {EXTERNALSYM _ACCESS_INFO_1}
  _ACCESS_INFO_1 = record
    acc1_resource_name: LPWSTR;
    acc1_attr: DWORD;
    acc1_count: DWORD;
  end;
  TAccessInfo1 = _ACCESS_INFO_1;
  {$EXTERNALSYM ACCESS_INFO_1}
  ACCESS_INFO_1 = _ACCESS_INFO_1;

  PAccessInfo1002 = ^TAccessInfo1002;
  {EXTERNALSYM _ACCESS_INFO_1002}
  _ACCESS_INFO_1002 = record
    acc1002_attr: DWORD;
  end;
  TAccessInfo1002 = _ACCESS_INFO_1002;
  {$EXTERNALSYM ACCESS_INFO_1002}
  ACCESS_INFO_1002 = _ACCESS_INFO_1002;

  PAccessList = ^TAccessList;
  {EXTERNALSYM _ACCESS_LIST}
  _ACCESS_LIST = record
    acl_ugname: LPWSTR;
    acl_access: DWORD;
  end;
  TAccessList = _ACCESS_LIST;
  {$EXTERNALSYM ACCESS_LIST}
  ACCESS_LIST = _ACCESS_LIST;

// Special Values and Constants - Access

// Maximum number of permission entries for each resource.

const
  {$EXTERNALSYM MAXPERMENTRIES}
  MAXPERMENTRIES = 64;

//  Bit values for the access permissions.  ACCESS_ALL is a handy
//  way to specify maximum permissions.  These are used in
//  acl_access field of access_list structures.

  {$EXTERNALSYM ACCESS_NONE}
  ACCESS_NONE = 0;

  {$EXTERNALSYM ACCESS_READ}
  ACCESS_READ         = $01;
  {$EXTERNALSYM ACCESS_WRITE}
  ACCESS_WRITE        = $02;
  {$EXTERNALSYM ACCESS_CREATE}
  ACCESS_CREATE       = $04;
  {$EXTERNALSYM ACCESS_EXEC}
  ACCESS_EXEC         = $08;
  {$EXTERNALSYM ACCESS_DELETE}
  ACCESS_DELETE       = $10;
  {$EXTERNALSYM ACCESS_ATRIB}
  ACCESS_ATRIB        = $20;
  {$EXTERNALSYM ACCESS_PERM}
  ACCESS_PERM         = $40;

  {$EXTERNALSYM ACCESS_GROUP}
  ACCESS_GROUP        = $8000;

  {$EXTERNALSYM ACCESS_ALL}
  ACCESS_ALL = ACCESS_READ or ACCESS_WRITE or ACCESS_CREATE or ACCESS_EXEC or
    ACCESS_DELETE or ACCESS_ATRIB or ACCESS_PERM;

// Bit values for the acc1_attr field of the ACCESS_INFO_1 structure.

  {$EXTERNALSYM ACCESS_AUDIT}
  ACCESS_AUDIT        = $1;

  {$EXTERNALSYM ACCESS_SUCCESS_OPEN}
  ACCESS_SUCCESS_OPEN         = $10;
  {$EXTERNALSYM ACCESS_SUCCESS_WRITE}
  ACCESS_SUCCESS_WRITE        = $20;
  {$EXTERNALSYM ACCESS_SUCCESS_DELETE}
  ACCESS_SUCCESS_DELETE       = $40;
  {$EXTERNALSYM ACCESS_SUCCESS_ACL}
  ACCESS_SUCCESS_ACL          = $80;
  {$EXTERNALSYM ACCESS_SUCCESS_MASK}
  ACCESS_SUCCESS_MASK         = $F0;

  {$EXTERNALSYM ACCESS_FAIL_OPEN}
  ACCESS_FAIL_OPEN            = $100;
  {$EXTERNALSYM ACCESS_FAIL_WRITE}
  ACCESS_FAIL_WRITE           = $200;
  {$EXTERNALSYM ACCESS_FAIL_DELETE}
  ACCESS_FAIL_DELETE          = $400;
  {$EXTERNALSYM ACCESS_FAIL_ACL}
  ACCESS_FAIL_ACL             = $800;
  {$EXTERNALSYM ACCESS_FAIL_MASK}
  ACCESS_FAIL_MASK            = $F00;

  {$EXTERNALSYM ACCESS_FAIL_SHIFT}
  ACCESS_FAIL_SHIFT           = 4;

// Parmnum value for NetAccessSetInfo.

  {$EXTERNALSYM ACCESS_RESOURCE_NAME_PARMNUM}
  ACCESS_RESOURCE_NAME_PARMNUM    = 1;
  {$EXTERNALSYM ACCESS_ATTR_PARMNUM}
  ACCESS_ATTR_PARMNUM             = 2;
  {$EXTERNALSYM ACCESS_COUNT_PARMNUM}
  ACCESS_COUNT_PARMNUM            = 3;
  {$EXTERNALSYM ACCESS_ACCESS_LIST_PARMNUM}
  ACCESS_ACCESS_LIST_PARMNUM      = 4;

// the new infolevel counterparts of the old info level + parmnum

  {$EXTERNALSYM ACCESS_RESOURCE_NAME_INFOLEVEL}
  ACCESS_RESOURCE_NAME_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + ACCESS_RESOURCE_NAME_PARMNUM);
  {$EXTERNALSYM ACCESS_ATTR_INFOLEVEL}
  ACCESS_ATTR_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + ACCESS_ATTR_PARMNUM);
  {$EXTERNALSYM ACCESS_COUNT_INFOLEVEL}
  ACCESS_COUNT_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + ACCESS_COUNT_PARMNUM);
  {$EXTERNALSYM ACCESS_ACCESS_LIST_INFOLEVEL}
  ACCESS_ACCESS_LIST_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + ACCESS_ACCESS_LIST_PARMNUM);

// ACCESS_LETTERS defines a letter for each bit position in
// the acl_access field of struct access_list.  Note that some
// bits have a corresponding letter of ' ' (space).

  {$EXTERNALSYM ACCESS_LETTERS}
  ACCESS_LETTERS = 'RWCXDAP         ';


// Domain Class

// Function Prototypes - Domain

{$EXTERNALSYM NetGetDCName}
function NetGetDCName(servername: LPCWSTR; domainname: LPCWSTR;
  bufptr: Pointer): NET_API_STATUS; stdcall;

{$EXTERNALSYM NetGetAnyDCName}
function NetGetAnyDCName(servername: LPCWSTR; domainname: LPCWSTR;
  bufptr: Pointer): NET_API_STATUS; stdcall;

{$EXTERNALSYM I_NetLogonControl}
function I_NetLogonControl(ServerName: LPCWSTR; FunctionCode: DWORD;
  QueryLevel: DWORD; Buffer: Pointer): NET_API_STATUS; stdcall;

{$EXTERNALSYM I_NetLogonControl2}
function I_NetLogonControl2(ServerName: LPCWSTR; FunctionCode: DWORD;
  QueryLevel: DWORD; Data: Pointer; Buffer: Pointer): NET_API_STATUS; stdcall;

type
  PNtStatus = ^TNtStatus;
  NTSTATUS = LongInt;
  {$EXTERNALSYM NTSTATUS}
  TNtStatus = NTSTATUS;

{$EXTERNALSYM NetEnumerateTrustedDomains}
function NetEnumerateTrustedDomains(ServerName: LPWSTR; DomainNames: LPWSTR): NET_API_STATUS; stdcall;

// Special Values and Constants - Domain

// FunctionCode values for I_NetLogonControl.
// NOTE : if you change the following NETLOGON_CONTROL_* values,
// change them in net\svcdlls\logonsrv\logon.idl file also.

const
  {$EXTERNALSYM NETLOGON_CONTROL_QUERY}
  NETLOGON_CONTROL_QUERY         = 1;    // No-op: just query
  {$EXTERNALSYM NETLOGON_CONTROL_REPLICATE}
  NETLOGON_CONTROL_REPLICATE     = 2;    // Force replicate on BDC
  {$EXTERNALSYM NETLOGON_CONTROL_SYNCHRONIZE}
  NETLOGON_CONTROL_SYNCHRONIZE   = 3;    // Force synchronize on BDC
  {$EXTERNALSYM NETLOGON_CONTROL_PDC_REPLICATE}
  NETLOGON_CONTROL_PDC_REPLICATE = 4;    // Force PDC to broadcast change
  {$EXTERNALSYM NETLOGON_CONTROL_REDISCOVER}
  NETLOGON_CONTROL_REDISCOVER    = 5;    // Force to re-discover trusted domain DCs
  {$EXTERNALSYM NETLOGON_CONTROL_TC_QUERY}
  NETLOGON_CONTROL_TC_QUERY      = 6;    // Query status of specified trusted channel status
  {$EXTERNALSYM NETLOGON_CONTROL_TRANSPORT_NOTIFY}
  NETLOGON_CONTROL_TRANSPORT_NOTIFY = 7; // Notify netlogon that a new transport has come online
  {$EXTERNALSYM NETLOGON_CONTROL_FIND_USER}
  NETLOGON_CONTROL_FIND_USER     = 8;    // Find named user in a trusted domain
  {$EXTERNALSYM NETLOGON_CONTROL_CHANGE_PASSWORD}
  NETLOGON_CONTROL_CHANGE_PASSWORD  = 9;  // Change machine password on a secure channel to a trusted domain


// Debug function codes

  {$EXTERNALSYM NETLOGON_CONTROL_UNLOAD_NETLOGON_DLL}
  NETLOGON_CONTROL_UNLOAD_NETLOGON_DLL = $FFFB;
  {$EXTERNALSYM NETLOGON_CONTROL_BACKUP_CHANGE_LOG}
  NETLOGON_CONTROL_BACKUP_CHANGE_LOG   = $FFFC;
  {$EXTERNALSYM NETLOGON_CONTROL_TRUNCATE_LOG}
  NETLOGON_CONTROL_TRUNCATE_LOG        = $FFFD;
  {$EXTERNALSYM NETLOGON_CONTROL_SET_DBFLAG}
  NETLOGON_CONTROL_SET_DBFLAG          = $FFFE;
  {$EXTERNALSYM NETLOGON_CONTROL_BREAKPOINT}
  NETLOGON_CONTROL_BREAKPOINT          = $FFFF;

// Query level 1 for I_NetLogonControl

type
  PNetLogonInfo1 = ^TNetLogonInfo1;
  {$EXTERNALSYM _NETLOGON_INFO_1}
  _NETLOGON_INFO_1 = record
    netlog1_flags: DWORD;
    netlog1_pdc_connection_status: NET_API_STATUS;
  end;
  TNetLogonInfo1 = _NETLOGON_INFO_1;
  {$EXTERNALSYM NETLOGON_INFO_1}
  NETLOGON_INFO_1 = _NETLOGON_INFO_1;

  PNetLogonInfo2 = ^TNetLogonInfo2;
  {$EXTERNALSYM _NETLOGON_INFO_2}
  _NETLOGON_INFO_2 = record
    netlog2_flags: DWORD;
    netlog2_pdc_connection_status: NET_API_STATUS;
    netlog2_trusted_dc_name: LPWSTR;
    netlog2_tc_connection_status: NET_API_STATUS;
  end;
  TNetLogonInfo2 = _NETLOGON_INFO_2;
  {$EXTERNALSYM NETLOGON_INFO_2}
  NETLOGON_INFO_2 = _NETLOGON_INFO_2;

  PNetLogonInfo3 = ^TNetLogonInfo3;
  {$EXTERNALSYM _NETLOGON_INFO_3}
  _NETLOGON_INFO_3 = record
    netlog3_flags: DWORD;
    netlog3_logon_attempts: DWORD;
    netlog3_reserved1: DWORD;
    netlog3_reserved2: DWORD;
    netlog3_reserved3: DWORD;
    netlog3_reserved4: DWORD;
    netlog3_reserved5: DWORD;
  end;
  TNetLogonInfo3 = _NETLOGON_INFO_3;
  {$EXTERNALSYM NETLOGON_INFO_3}
  NETLOGON_INFO_3 = _NETLOGON_INFO_3;

  PNetLogonInfo4 = ^TNetLogonInfo4;
  {$EXTERNALSYM _NETLOGON_INFO_4}
  _NETLOGON_INFO_4 = record
    netlog4_trusted_dc_name: LPWSTR;
    netlog4_trusted_domain_name: LPWSTR;
  end;
  TNetLogonInfo4 = _NETLOGON_INFO_4;
  {$EXTERNALSYM NETLOGON_INFO_4}
  NETLOGON_INFO_4 = _NETLOGON_INFO_4;

// Values of netlog1_flags

const
  {$EXTERNALSYM NETLOGON_REPLICATION_NEEDED}
  NETLOGON_REPLICATION_NEEDED       = $01;  // Database is out of date
  {$EXTERNALSYM NETLOGON_REPLICATION_IN_PROGRESS}
  NETLOGON_REPLICATION_IN_PROGRESS  = $02;  // Replication is happening now
  {$EXTERNALSYM NETLOGON_FULL_SYNC_REPLICATION}
  NETLOGON_FULL_SYNC_REPLICATION    = $04;  // full sync replication required/progress
  {$EXTERNALSYM NETLOGON_REDO_NEEDED}
  NETLOGON_REDO_NEEDED              = $08;  // Redo of previous replication needed
  {$EXTERNALSYM NETLOGON_HAS_IP}
  NETLOGON_HAS_IP                   = $10;  // The trusted domain DC has an IP address
  {$EXTERNALSYM NETLOGON_HAS_TIMESERV}
  NETLOGON_HAS_TIMESERV             = $20;  // The trusted domain DC runs the Windows Time Service

implementation

function NetUserAdd; external netapi32lib name 'NetUserAdd';
function NetUserEnum; external netapi32lib name 'NetUserEnum';
function NetUserGetInfo; external netapi32lib name 'NetUserGetInfo';
function NetUserSetInfo; external netapi32lib name 'NetUserSetInfo';
function NetUserDel; external netapi32lib name 'NetUserDel';
function NetUserGetGroups; external netapi32lib name 'NetUserGetGroups';
function NetUserSetGroups; external netapi32lib name 'NetUserSetGroups';
function NetUserGetLocalGroups; external netapi32lib name 'NetUserGetLocalGroups';
function NetUserModalsGet; external netapi32lib name 'NetUserModalsGet';
function NetUserModalsSet; external netapi32lib name 'NetUserModalsSet';
function NetUserChangePassword; external netapi32lib name 'NetUserChangePassword';
function NetGroupAdd; external netapi32lib name 'NetGroupAdd';
function NetGroupAddUser; external netapi32lib name 'NetGroupAddUser';
function NetGroupEnum; external netapi32lib name 'NetGroupEnum';
function NetGroupGetInfo; external netapi32lib name 'NetGroupGetInfo';
function NetGroupSetInfo; external netapi32lib name 'NetGroupSetInfo';
function NetGroupDel; external netapi32lib name 'NetGroupDel';
function NetGroupDelUser; external netapi32lib name 'NetGroupDelUser';
function NetGroupGetUsers; external netapi32lib name 'NetGroupGetUsers';
function NetGroupSetUsers; external netapi32lib name 'NetGroupSetUsers';
function NetLocalGroupAdd; external netapi32lib name 'NetLocalGroupAdd';
function NetLocalGroupAddMember; external netapi32lib name 'NetLocalGroupAddMember';
function NetLocalGroupEnum; external netapi32lib name 'NetLocalGroupEnum';
function NetLocalGroupGetInfo; external netapi32lib name 'NetLocalGroupGetInfo';
function NetLocalGroupSetInfo; external netapi32lib name 'NetLocalGroupSetInfo';
function NetLocalGroupDel; external netapi32lib name 'NetLocalGroupDel';
function NetLocalGroupDelMember; external netapi32lib name 'NetLocalGroupDelMember';
function NetLocalGroupGetMembers; external netapi32lib name 'NetLocalGroupGetMembers';
function NetLocalGroupSetMembers; external netapi32lib name 'NetLocalGroupSetMembers';
function NetLocalGroupAddMembers; external netapi32lib name 'NetLocalGroupAddMembers';
function NetLocalGroupDelMembers; external netapi32lib name 'NetLocalGroupDelMembers';
function NetQueryDisplayInformation; external netapi32lib name 'NetQueryDisplayInformation';
function NetGetDisplayInformationIndex; external netapi32lib name 'NetGetDisplayInformationIndex';
function NetAccessAdd; external netapi32lib name 'NetAccessAdd';
function NetAccessEnum; external netapi32lib name 'NetAccessEnum';
function NetAccessGetInfo; external netapi32lib name 'NetAccessGetInfo';
function NetAccessSetInfo; external netapi32lib name 'NetAccessSetInfo';
function NetAccessDel; external netapi32lib name 'NetAccessDel';
function NetAccessGetUserPerms; external netapi32lib name 'NetAccessGetUserPerms';
function NetGetDCName; external netapi32lib name 'NetGetDCName';
function NetGetAnyDCName; external netapi32lib name 'NetGetAnyDCName';
function I_NetLogonControl; external netapi32lib name 'I_NetLogonControl';
function I_NetLogonControl2; external netapi32lib name 'I_NetLogonControl2';
function NetEnumerateTrustedDomains; external netapi32lib name 'NetEnumerateTrustedDomains';

end.
