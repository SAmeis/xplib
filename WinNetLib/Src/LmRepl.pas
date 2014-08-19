{******************************************************************}
{                                                                  }
{ Borland Delphi Runtime Library                                   }
{ LanManager share functions for Windows NT interface unit         }
{                                                                  }
{ Portions created by Microsoft are                                }
{ Copyright (C) 1995-1999 Microsoft Corporation.                   }
{ All Rights Reserved.                                             }
{                                                                  }
{ The original file is: lmrepl.h, released 24 Nov 1998.            }
{ The original Pascal code is: LmRepl.pas, released 13 Jan 2000.   }
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

unit LmRepl;

{$I LANMAN.INC}

{$ALIGN ON}
{$MINENUMSIZE 4}
{$WEAKPACKAGEUNIT}

interface

uses
  Windows, LmCons;

(*$HPPEMIT '#include <lmcons.h>'*)
(*$HPPEMIT '#include <lmrepl.h>'*)

// Replicator Configuration APIs

const
  REPL_ROLE_EXPORT        = 1;
  {$EXTERNALSYM REPL_ROLE_EXPORT}
  REPL_ROLE_IMPORT        = 2;
  {$EXTERNALSYM REPL_ROLE_IMPORT}
  REPL_ROLE_BOTH          = 3;
  {$EXTERNALSYM REPL_ROLE_BOTH}


  REPL_INTERVAL_INFOLEVEL         = (PARMNUM_BASE_INFOLEVEL + 0);
  {$EXTERNALSYM REPL_INTERVAL_INFOLEVEL}
  REPL_PULSE_INFOLEVEL            = (PARMNUM_BASE_INFOLEVEL + 1);
  {$EXTERNALSYM REPL_PULSE_INFOLEVEL}
  REPL_GUARDTIME_INFOLEVEL        = (PARMNUM_BASE_INFOLEVEL + 2);
  {$EXTERNALSYM REPL_GUARDTIME_INFOLEVEL}
  REPL_RANDOM_INFOLEVEL           = (PARMNUM_BASE_INFOLEVEL + 3);
  {$EXTERNALSYM REPL_RANDOM_INFOLEVEL}

type
  PReplInfo0 = ^TReplInfo0;
  _REPL_INFO_0 = record
    rp0_role: DWORD;
    rp0_exportpath: LPWSTR;
    rp0_exportlist: LPWSTR;
    rp0_importpath: LPWSTR;
    rp0_importlist: LPWSTR;
    rp0_logonusername: LPWSTR;
    rp0_interval: DWORD;
    rp0_pulse: DWORD;
    rp0_guardtime: DWORD;
    rp0_random: DWORD;
  end;
  {$EXTERNALSYM _REPL_INFO_0}
  TReplInfo0 = _REPL_INFO_0;
  REPL_INFO_0 = _REPL_INFO_0;
  {$EXTERNALSYM REPL_INFO_0}

  PReplInfo1000 = ^TReplInfo1000;
  _REPL_INFO_1000 = record
    rp1000_interval: DWORD;
  end;
  {$EXTERNALSYM _REPL_INFO_1000}
  TReplInfo1000 = _REPL_INFO_1000;
  REPL_INFO_1000 = _REPL_INFO_1000;
  {$EXTERNALSYM REPL_INFO_1000}

  PReplInfo1001 = ^TReplInfo1001;
  _REPL_INFO_1001 = record
    rp1001_pulse: DWORD;
  end;
  {$EXTERNALSYM _REPL_INFO_1001}
  TReplInfo1001 = _REPL_INFO_1001;
  REPL_INFO_1001 = _REPL_INFO_1001;
  {$EXTERNALSYM REPL_INFO_1001}

  PReplInfo1002 = ^TReplInfo1002;
  _REPL_INFO_1002 = record
    rp1002_guardtime: DWORD;
  end;
  {$EXTERNALSYM _REPL_INFO_1002}
  TReplInfo1002 = _REPL_INFO_1002;
  REPL_INFO_1002 = _REPL_INFO_1002;
  {$EXTERNALSYM REPL_INFO_1002}

  PReplInfo1003 = ^TReplInfo1003;
  _REPL_INFO_1003 = record
    rp1003_random: DWORD;
  end;
  {$EXTERNALSYM _REPL_INFO_1003}
  TReplInfo1003 = _REPL_INFO_1003;
  REPL_INFO_1003 = _REPL_INFO_1003;
  {$EXTERNALSYM REPL_INFO_1003}

function NetReplGetInfo(servername: LPCWSTR; level: DWORD; bufptr: Pointer): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetReplGetInfo}

function NetReplSetInfo(servername: LPCWSTR; level: DWORD; const buf: Pointer;
  parm_err: PDWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetReplSetInfo}

// Replicator Export Directory APIs

const
  REPL_INTEGRITY_FILE     = 1;
  {$EXTERNALSYM REPL_INTEGRITY_FILE}
  REPL_INTEGRITY_TREE     = 2;
  {$EXTERNALSYM REPL_INTEGRITY_TREE}

  REPL_EXTENT_FILE        = 1;
  {$EXTERNALSYM REPL_EXTENT_FILE}
  REPL_EXTENT_TREE        = 2;
  {$EXTERNALSYM REPL_EXTENT_TREE}

  REPL_EXPORT_INTEGRITY_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + 0);
  {$EXTERNALSYM REPL_EXPORT_INTEGRITY_INFOLEVEL}
  REPL_EXPORT_EXTENT_INFOLEVEL    = (PARMNUM_BASE_INFOLEVEL + 1);
  {$EXTERNALSYM REPL_EXPORT_EXTENT_INFOLEVEL}

type
  PReplEdirInfo0 = ^TReplEdirInfo0;
  _REPL_EDIR_INFO_0 = record
    rped0_dirname: LPWSTR;
  end;
  {$EXTERNALSYM _REPL_EDIR_INFO_0}
  TReplEdirInfo0 = _REPL_EDIR_INFO_0;
  REPL_EDIR_INFO_0 = _REPL_EDIR_INFO_0;
  {$EXTERNALSYM REPL_EDIR_INFO_0}

  PReplEdirInfo1 = ^TReplEdirInfo1;
  _REPL_EDIR_INFO_1 = record
    rped1_dirname: LPWSTR;
    rped1_integrity: DWORD;
    rped1_extent: DWORD;
  end;
  {$EXTERNALSYM _REPL_EDIR_INFO_1}
  TReplEdirInfo1 = _REPL_EDIR_INFO_1;
  REPL_EDIR_INFO_1 = _REPL_EDIR_INFO_1;
  {$EXTERNALSYM REPL_EDIR_INFO_1}

  PReplEdirInfo2 = ^TReplEdirInfo2;
  _REPL_EDIR_INFO_2 = record
    rped2_dirname: LPWSTR;
    rped2_integrity: DWORD;
    rped2_extent: DWORD;
    rped2_lockcount: DWORD;
    rped2_locktime: DWORD;
  end;
  {$EXTERNALSYM _REPL_EDIR_INFO_2}
  TReplEdirInfo2 = _REPL_EDIR_INFO_2;
  REPL_EDIR_INFO_2 = _REPL_EDIR_INFO_2;
  {$EXTERNALSYM REPL_EDIR_INFO_2}

  PReplEdirInfo1000 = ^TReplEdirInfo1000;
  _REPL_EDIR_INFO_1000 = record
    rped1000_integrity: DWORD;
  end;
  {$EXTERNALSYM _REPL_EDIR_INFO_1000}
  TReplEdirInfo1000 = _REPL_EDIR_INFO_1000;
  REPL_EDIR_INFO_1000 = _REPL_EDIR_INFO_1000;
  {$EXTERNALSYM REPL_EDIR_INFO_1000}

  PReplEdirInfo1001 = ^TReplEdirInfo1001;
  _REPL_EDIR_INFO_1001 = record
    rped1001_extent: DWORD;
  end;
  {$EXTERNALSYM _REPL_EDIR_INFO_1001}
  TReplEdirInfo1001 = _REPL_EDIR_INFO_1001;
  REPL_EDIR_INFO_1001 = _REPL_EDIR_INFO_1001;
  {$EXTERNALSYM REPL_EDIR_INFO_1001}


function NetReplExportDirAdd(servername: LPCWSTR; level: DWORD; buf: Pointer;
  parm_err: PDWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetReplExportDirAdd}

function NetReplExportDirDel(servername, dirname: LPCWSTR): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetReplExportDirDel}

function NetReplExportDirEnum(servername: LPCWSTR; level: DWORD; bufptr: Pointer;
  prefmaxlen: DWORD; var entriesread: DWORD; var totalentries: DWORD;
  resumehandle: PDWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetReplExportDirEnum}

function NetReplExportDirGetInfo(servername: LPCWSTR; dirname: LPCWSTR;
  level: DWORD; bufptr: Pointer): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetReplExportDirGetInfo}

function NetReplExportDirSetInfo(servername: LPCWSTR; dirname: LPCWSTR;
  level: DWORD; const buf: Pointer; parm_err: PDWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetReplExportDirSetInfo}

function NetReplExportDirLock(servername, dirname: LPCWSTR): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetReplExportDirLock}

function NetReplExportDirUnlock(servername: LPCWSTR; dirname: LPCWSTR;
  unlockforce: DWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetReplExportDirUnlock}

const
  REPL_UNLOCK_NOFORCE     = 0;
  {$EXTERNALSYM REPL_UNLOCK_NOFORCE}
  REPL_UNLOCK_FORCE       = 1;
  {$EXTERNALSYM REPL_UNLOCK_FORCE}

// Replicator Import Directory APIs

type
  PReplIdirInfo0 = ^TReplIdirInfo0;
  _REPL_IDIR_INFO_0 = record
    rpid0_dirname: LPWSTR;
  end;
  {$EXTERNALSYM _REPL_IDIR_INFO_0}
  TReplIdirInfo0 = _REPL_IDIR_INFO_0;
  REPL_IDIR_INFO_0 = _REPL_IDIR_INFO_0;
  {$EXTERNALSYM REPL_IDIR_INFO_0}

  PReplIdirInfo1 = ^TReplIdirInfo1;
  _REPL_IDIR_INFO_1 = record
    rpid1_dirname: LPWSTR;
    rpid1_state: DWORD;
    rpid1_mastername: LPWSTR;
    rpid1_last_update_time: DWORD;
    rpid1_lockcount: DWORD;
    rpid1_locktime: DWORD;
  end;
  {$EXTERNALSYM _REPL_IDIR_INFO_1}
  TReplIdirInfo1 = _REPL_IDIR_INFO_1;
  REPL_IDIR_INFO_1 = _REPL_IDIR_INFO_1;
  {$EXTERNALSYM REPL_IDIR_INFO_1}

function NetReplImportDirAdd(servername: LPCWSTR; level: DWORD; buf: Pointer;
  parm_err: PDWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetReplImportDirAdd}

function NetReplImportDirDel(servername, dirname: LPCWSTR): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetReplImportDirDel}

function NetReplImportDirEnum(servername: LPCWSTR; level: DWORD; bufptr: Pointer;
  prefmaxlen: DWORD; var entriesread: DWORD; var totalentries: DWORD;
  resumehandle: PDWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetReplImportDirEnum}

function NetReplImportDirGetInfo(servername, dirname: LPCWSTR; level: DWORD;
  bufptr: Pointer): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetReplImportDirGetInfo}

function NetReplImportDirLock(servername, dirname: LPCWSTR): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetReplImportDirLock}

function NetReplImportDirUnlock(servername: LPCWSTR; dirname: LPCWSTR;
  unlockforce: DWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetReplImportDirUnlock}

const
  REPL_STATE_OK                   = 0;
  {$EXTERNALSYM REPL_STATE_OK}
  REPL_STATE_NO_MASTER            = 1;
  {$EXTERNALSYM REPL_STATE_NO_MASTER}
  REPL_STATE_NO_SYNC              = 2;
  {$EXTERNALSYM REPL_STATE_NO_SYNC}
  REPL_STATE_NEVER_REPLICATED     = 3;
  {$EXTERNALSYM REPL_STATE_NEVER_REPLICATED}

implementation

function NetReplGetInfo; external netapi32lib name 'NetReplGetInfo';
function NetReplSetInfo; external netapi32lib name 'NetReplSetInfo';
function NetReplExportDirAdd; external netapi32lib name 'NetReplExportDirAdd';
function NetReplExportDirDel; external netapi32lib name 'NetReplExportDirDel';
function NetReplExportDirEnum; external netapi32lib name 'NetReplExportDirEnum';
function NetReplExportDirGetInfo; external netapi32lib name 'NetReplExportDirGetInfo';
function NetReplExportDirSetInfo; external netapi32lib name 'NetReplExportDirSetInfo';
function NetReplExportDirLock; external netapi32lib name 'NetReplExportDirLock';
function NetReplExportDirUnlock; external netapi32lib name 'NetReplExportDirUnlock';
function NetReplImportDirAdd; external netapi32lib name 'NetReplImportDirAdd';
function NetReplImportDirDel; external netapi32lib name 'NetReplImportDirDel';
function NetReplImportDirEnum; external netapi32lib name 'NetReplImportDirEnum';
function NetReplImportDirGetInfo; external netapi32lib name 'NetReplImportDirGetInfo';
function NetReplImportDirLock; external netapi32lib name 'NetReplImportDirLock';
function NetReplImportDirUnlock; external netapi32lib name 'NetReplImportDirUnlock';

end.

