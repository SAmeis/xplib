{******************************************************************}
{                                                                  }
{ Borland Delphi Runtime Library                                   }
{ LanManager share functions for Windows NT interface unit         }
{                                                                  }
{ Portions created by Microsoft are                                }
{ Copyright (C) 1995-1999 Microsoft Corporation.                   }
{ All Rights Reserved.                                             }
{                                                                  }
{ The original file is: lmshare.h, released 14 Nov 1998.           }
{ The original Pascal code is: LmShare.pas, released 29 Dec 1999.  }
{ The initial developer of the Pascal code is Petr Vones           }
{ (petr.v@mujmail.cz).                                             }
{                                                                  }
{ Portions created by Petr Vones are                               }
{ Copyright (C) 1999 Petr Vones                                    }
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

unit LmShare;

interface

{$I LANMAN.INC}

{$ALIGN ON}
{$MINENUMSIZE 4}
{$IFNDEF LANMAN_DYNAMIC_LINK}
{$WEAKPACKAGEUNIT}
{$ENDIF}

uses
  Windows, LmCons;

(*$HPPEMIT '#include <lmcons.h>'*)
(*$HPPEMIT '#include <windef.h>'*)
(*$HPPEMIT '#include <lmshare.h>'*)

// Function Prototypes - Share

{$IFDEF LANMAN_DYNAMIC_LINK}
type
  TNetShareAdd = function (servername: LPWSTR; level: DWORD; const buf: Pointer;
    parm_err: PDWORD): NET_API_STATUS; stdcall;

  TNetShareEnum = function (servername: LPWSTR; level: DWORD; var butptr: Pointer;
    prefmaxlen: DWORD; var entriesread: DWORD; var totalentries: DWORD;
    resume_handle: PDWORD): NET_API_STATUS; stdcall;

  TNetShareEnumSticky = function (servername: LPWSTR; level: DWORD; var butptr: Pointer;
    prefmaxlen: DWORD; var entriesread: DWORD; var totalentries: DWORD;
    resume_handle: PDWORD): NET_API_STATUS; stdcall;

  TNetShareGetInfo = function (servername: LPWSTR; netname: LPWSTR; level: DWORD;
    var butptr: Pointer): NET_API_STATUS; stdcall;

  TNetShareSetInfo = function (servername: LPWSTR; netname: LPWSTR; leve: DWORD;
   const buf: Pointer; parm_err: PDWORD): NET_API_STATUS; stdcall;

  TNetShareDel = function (servername: LPWSTR; netname: LPWSTR;
    reserved: DWORD): NET_API_STATUS; stdcall;

  TNetShareDelSticky = function (servername: LPWSTR; netname: LPWSTR;
    reserved: DWORD): NET_API_STATUS; stdcall;

  TNetShareCheck = function (servername: LPWSTR; device: LPWSTR;
    var _type: DWORD): NET_API_STATUS; stdcall;
{$ELSE}
function NetShareAdd(servername: LPWSTR; level: DWORD; const buf: Pointer;
  parm_err: PDWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetShareAdd}

function NetShareEnum(servername: LPWSTR; level: DWORD; var butptr: Pointer;
  prefmaxlen: DWORD; var entriesread: DWORD; var totalentries: DWORD;
  resume_handle: PDWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetShareEnum}

function NetShareEnumSticky(servername: LPWSTR; level: DWORD; var butptr: Pointer;
  prefmaxlen: DWORD; var entriesread: DWORD; var totalentries: DWORD;
  resume_handle: PDWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetShareEnumSticky}

function NetShareGetInfo(servername: LPWSTR; netname: LPWSTR; level: DWORD;
  var butptr: Pointer): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetShareGetInfo}

function NetShareSetInfo(servername: LPWSTR; netname: LPWSTR; leve: DWORD;
  const buf: Pointer; parm_err: PDWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetShareSetInfo}

function NetShareDel(servername: LPWSTR; netname: LPWSTR;
  reserved: DWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetShareDel}

function NetShareDelSticky(servername: LPWSTR; netname: LPWSTR;
  reserved: DWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetShareDelSticky}

function NetShareCheck(servername: LPWSTR; device: LPWSTR;
  var _type: DWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetShareCheck}
{$ENDIF}

// Data Structures - Share

type
  PShareInfo0 = ^TShareInfo0;
  {$EXTERNALSYM _SHARE_INFO_0}
  _SHARE_INFO_0 = record
    shi0_netname: LPWSTR;
  end;
  TShareInfo0 = _SHARE_INFO_0;
  {$EXTERNALSYM SHARE_INFO_0}
  SHARE_INFO_0 = _SHARE_INFO_0;

  PShareInfo1 = ^TShareInfo1;
  {$EXTERNALSYM _SHARE_INFO_1}
  _SHARE_INFO_1 = record
    shi1_netname: LPWSTR;
    shi1_type: DWORD;
    shi1_remark: LPWSTR;
  end;
  TShareInfo1 = _SHARE_INFO_1;
  {$EXTERNALSYM SHARE_INFO_1}
  SHARE_INFO_1 = _SHARE_INFO_1;

  PShareInfo2 = ^TShareInfo2;
  {$EXTERNALSYM _SHARE_INFO_2}
  _SHARE_INFO_2 = record
    shi2_netname: LPWSTR;
    shi2_type: DWORD;
    shi2_remark: LPWSTR;
    shi2_permissions: DWORD;
    shi2_max_uses: DWORD;
    shi2_current_uses: DWORD;
    shi2_path: LPWSTR;
    shi2_passwd: LPWSTR;
  end;
  TShareInfo2 = _SHARE_INFO_2;
  {$EXTERNALSYM SHARE_INFO_2}
  SHARE_INFO_2 = _SHARE_INFO_2;

  PShareInfo501 = ^TShareInfo501;
  {$EXTERNALSYM _SHARE_INFO_501}
  _SHARE_INFO_501 = record
    shi501_netname: LPWSTR;
    shi501_type: DWORD;
    shi501_remark: LPWSTR;
    shi501_flags: DWORD;
  end;
  TShareInfo501 = _SHARE_INFO_501;
  {$EXTERNALSYM SHARE_INFO_501}
  SHARE_INFO_501 = _SHARE_INFO_501;

  PShareInfo502 = ^TShareInfo502;
  {$EXTERNALSYM _SHARE_INFO_502}
  _SHARE_INFO_502 = record
    shi502_netname: LPWSTR;
    shi502_type: DWORD;
    shi502_remark: LPWSTR;
    shi502_permissions: DWORD;
    shi502_max_uses: DWORD;
    shi502_current_uses: DWORD;
    shi502_path: LPWSTR;
    shi502_passwd: LPWSTR;
    shi502_reserved: DWORD;
    shi502_security_descriptor: PSECURITY_DESCRIPTOR;
  end;
  TShareInfo502 = _SHARE_INFO_502;
  {$EXTERNALSYM SHARE_INFO_502}
  SHARE_INFO_502 = _SHARE_INFO_502;

  PShareInfo1004 = ^TShareInfo1004;
  {$EXTERNALSYM _SHARE_INFO_1004}
  _SHARE_INFO_1004 = record
    shi1004_remark: LPWSTR;
  end;
  TShareInfo1004 = _SHARE_INFO_1004;
  {$EXTERNALSYM SHARE_INFO_1004}
  SHARE_INFO_1004 = _SHARE_INFO_1004;

  PShareInfo1005 = ^TShareInfo1005;
  {$EXTERNALSYM _SHARE_INFO_1005}
  _SHARE_INFO_1005 = record
    shi1005_flags: DWORD;
  end;
  TShareInfo1005 = _SHARE_INFO_1005;
  {$EXTERNALSYM SHARE_INFO_1005}
  SHARE_INFO_1005 = _SHARE_INFO_1005;

  PShareInfo1006 = ^TShareInfo1006;
  {$EXTERNALSYM _SHARE_INFO_1006}
  _SHARE_INFO_1006 = record
    shi1006_max_uses: DWORD;
  end;
  TShareInfo1006 = _SHARE_INFO_1006;
  {$EXTERNALSYM SHARE_INFO_1006}
  SHARE_INFO_1006 = _SHARE_INFO_1006;

  PShareInfo1501 = ^TShareInfo1501;
  {$EXTERNALSYM _SHARE_INFO_1501}
  _SHARE_INFO_1501 = record
    shi1501_reserved: DWORD;
    shi1501_security_descriptor: PSECURITY_DESCRIPTOR;
  end;
  TShareInfo1501 = _SHARE_INFO_1501;
  {$EXTERNALSYM SHARE_INFO_1501}
  SHARE_INFO_1501 = _SHARE_INFO_1501;

// Values for parm_err parameter.

const
  {$EXTERNALSYM SHARE_NETNAME_PARMNUM}
  SHARE_NETNAME_PARMNUM = 1;
  {$EXTERNALSYM SHARE_TYPE_PARMNUM}
  SHARE_TYPE_PARMNUM = 3;
  {$EXTERNALSYM SHARE_REMARK_PARMNUM}
  SHARE_REMARK_PARMNUM = 4;
  {$EXTERNALSYM SHARE_PERMISSIONS_PARMNUM}
  SHARE_PERMISSIONS_PARMNUM = 5;
  {$EXTERNALSYM SHARE_MAX_USES_PARMNUM}
  SHARE_MAX_USES_PARMNUM = 6;
  {$EXTERNALSYM SHARE_CURRENT_USES_PARMNUM}
  SHARE_CURRENT_USES_PARMNUM = 7;
  {$EXTERNALSYM SHARE_PATH_PARMNUM}
  SHARE_PATH_PARMNUM = 8;
  {$EXTERNALSYM SHARE_PASSWD_PARMNUM}
  SHARE_PASSWD_PARMNUM = 9;
  {$EXTERNALSYM SHARE_FILE_SD_PARMNUM}
  SHARE_FILE_SD_PARMNUM = 501;

// Single-field infolevels for NetShareSetInfo.

  {$EXTERNALSYM SHARE_REMARK_INFOLEVEL}
  SHARE_REMARK_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SHARE_REMARK_PARMNUM);
  {$EXTERNALSYM SHARE_MAX_USES_INFOLEVEL}
  SHARE_MAX_USES_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SHARE_MAX_USES_PARMNUM);
  {$EXTERNALSYM SHARE_FILE_SD_INFOLEVEL}
  SHARE_FILE_SD_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SHARE_FILE_SD_PARMNUM);

  {$EXTERNALSYM SHI1_NUM_ELEMENTS}
  SHI1_NUM_ELEMENTS = 4;
  {$EXTERNALSYM SHI2_NUM_ELEMENTS}
  SHI2_NUM_ELEMENTS = 10;

// Share types (shi1_type and shi2_type fields).

  {$EXTERNALSYM STYPE_DISKTREE}
  STYPE_DISKTREE = 0;
  {$EXTERNALSYM STYPE_PRINTQ}
  STYPE_PRINTQ = 1;
  {$EXTERNALSYM STYPE_DEVICE}
  STYPE_DEVICE = 2;
  {$EXTERNALSYM STYPE_IPC}
  STYPE_IPC = 3;

  {$EXTERNALSYM STYPE_SPECIAL}
  STYPE_SPECIAL = $80000000;

  {$EXTERNALSYM SHI_USES_UNLIMITED}
  SHI_USES_UNLIMITED = DWORD(-1);

// Flags values for the 501 and 1005 levels

  {$EXTERNALSYM SHI1005_FLAGS_DFS}
  SHI1005_FLAGS_DFS      = $01;        // Share is in the DFS
  {$EXTERNALSYM SHI1005_FLAGS_DFS_ROOT}
  SHI1005_FLAGS_DFS_ROOT = $02;        // Share is root of DFS

  {$EXTERNALSYM CSC_MASK}
  CSC_MASK               = $30;    // Used to mask off the following states

  {$EXTERNALSYM CSC_CACHE_MANUAL_REINT}
  CSC_CACHE_MANUAL_REINT = $00;    // No automatic file by file reintegration
  {$EXTERNALSYM CSC_CACHE_AUTO_REINT}
  CSC_CACHE_AUTO_REINT   = $10;    // File by file reintegration is OK
  {$EXTERNALSYM CSC_CACHE_VDO}
  CSC_CACHE_VDO          = $20;    // no need to flow opens
  {$EXTERNALSYM CSC_CACHE_NONE}
  CSC_CACHE_NONE         = $30;    // no CSC for this share

// The subset of 1005 infolevel flags that can be set via the API

  {$EXTERNALSYM SHI1005_VALID_FLAGS_SET}
  SHI1005_VALID_FLAGS_SET = CSC_MASK;

// SESSION API

// Function Prototypes Session

{$IFDEF LANMAN_DYNAMIC_LINK}
type
  TNetSessionEnum = function (servername: LPWSTR; UncClientName: LPWSTR;
    username: LPWSTR; level: DWORD; var bufptr: Pointer; prefmaxlen: DWORD;
    var entriesread: DWORD; var totalentries: DWORD;
    resume_handle: PDWORD): NET_API_STATUS; stdcall;

  TNetSessionDel = function (servername: LPWSTR; UncClientName: LPWSTR;
    username: LPWSTR): NET_API_STATUS; stdcall;

  TNetSessionGetInfo = function (servername: LPWSTR; UncClientName: LPWSTR;
    username: LPWSTR; level: DWORD; var bufptr: Pointer): NET_API_STATUS; stdcall;
{$ELSE}
function NetSessionEnum(servername: LPWSTR; UncClientName: LPWSTR;
  username: LPWSTR; level: DWORD; var bufptr: Pointer; prefmaxlen: DWORD;
  var entriesread: DWORD; var totalentries: DWORD;
  resume_handle: PDWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetSessionEnum}

function NetSessionDel(servername: LPWSTR; UncClientName: LPWSTR;
  username: LPWSTR): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetSessionDel}

function NetSessionGetInfo(servername: LPWSTR; UncClientName: LPWSTR;
  username: LPWSTR; level: DWORD; var bufptr: Pointer): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetSessionGetInfo}
{$ENDIF}

// Data Structures - Session

type
  PSessionInfo0 = ^TSessionInfo0;
  {$EXTERNALSYM _SESSION_INFO_0}
  _SESSION_INFO_0 = record
    sesi0_cname: LPWSTR;                // client name (no backslashes)
  end;
  TSessionInfo0 = _SESSION_INFO_0;
  {$EXTERNALSYM SESSION_INFO_0}
  SESSION_INFO_0 = _SESSION_INFO_0;

  PSessionInfo1 = ^TSessionInfo1;
  {$EXTERNALSYM _SESSION_INFO_1}
  _SESSION_INFO_1 = record
    sesi1_cname: LPWSTR;                // client name (no backslashes)
    sesi1_username: LPWSTR;
    sesi1_num_opens: DWORD;
    sesi1_time: DWORD;
    sesi1_idle_time: DWORD;
    sesi1_user_flags: DWORD;
  end;
  TSessionInfo1 = _SESSION_INFO_1;
  {$EXTERNALSYM SESSION_INFO_1}
  SESSION_INFO_1 = _SESSION_INFO_1;

  PSessionInfo2 = ^TSessionInfo2;
  {$EXTERNALSYM _SESSION_INFO_2}
  _SESSION_INFO_2 = record
    sesi2_cname: LPWSTR;                // client name (no backslashes)
    sesi2_username: LPWSTR;
    sesi2_num_opens: DWORD;
    sesi2_time: DWORD;
    sesi2_idle_time: DWORD;
    sesi2_user_flags: DWORD;
    sesi2_cltype_name: LPWSTR;
  end;
  TSessionInfo2 = _SESSION_INFO_2;
  {$EXTERNALSYM SESSION_INFO_2}
  SESSION_INFO_2 = _SESSION_INFO_2;

  PSessionInfo10 = ^TSessionInfo10;
  {$EXTERNALSYM _SESSION_INFO_10}
  _SESSION_INFO_10 = record
    sesi10_cname: LPWSTR;               // client name (no backslashes)
    sesi10_username: LPWSTR;
    sesi10_time: DWORD;
    sesi10_idle_time: DWORD;
  end;
  TSessionInfo10 = _SESSION_INFO_10;
  {$EXTERNALSYM SESSION_INFO_10}
  SESSION_INFO_10 = _SESSION_INFO_10;

  PSessionInfo502 = ^TSessionInfo502;
  {$EXTERNALSYM _SESSION_INFO_502}
  _SESSION_INFO_502 = record
    sesi502_cname: LPWSTR;              // client name (no backslashes)
    sesi502_username: LPWSTR;
    sesi502_num_opens: DWORD;
    sesi502_time: DWORD;
    sesi502_idle_time: DWORD;
    sesi502_user_flags: DWORD;
    sesi502_cltype_name: LPWSTR;
    sesi502_transport: LPWSTR;
  end;
  TSessionInfo502 = _SESSION_INFO_502;
  {$EXTERNALSYM SESSION_INFO_502}
  SESSION_INFO_502 = _SESSION_INFO_502;

// Bits defined in sesi1_user_flags.

const
  {$EXTERNALSYM SESS_GUEST}
  SESS_GUEST        = $00000001;  // session is logged on as a guest
  {$EXTERNALSYM SESS_NOENCRYPTION}
  SESS_NOENCRYPTION = $00000002;  // session is not using encryption

  {$EXTERNALSYM SESI1_NUM_ELEMENTS}
  SESI1_NUM_ELEMENTS = 8;
  {$EXTERNALSYM SESI2_NUM_ELEMENTS}
  SESI2_NUM_ELEMENTS = 9;

// CONNECTION API

// Function Prototypes - CONNECTION

{$IFDEF LANMAN_DYNAMIC_LINK}
type
  TNetConnectionEnum = function (servername: LPWSTR; qualifier: LPWSTR;
    level: DWORD; var bufptr: Pointer; prefmaxlen: DWORD; var entriesread: DWORD;
    var totalentries: DWORD; resume_handle: PDWORD): NET_API_STATUS; stdcall;
{$ELSE}
function NetConnectionEnum(servername: LPWSTR; qualifier: LPWSTR;
  level: DWORD; var bufptr: Pointer; prefmaxlen: DWORD; var entriesread: DWORD;
  var totalentries: DWORD; resume_handle: PDWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetConnectionEnum}
{$ENDIF}

// Data Structures - CONNECTION

type
  PConnectionInfo0 = ^TConnectionInfo0;
  {$EXTERNALSYM _CONNECTION_INFO_0}
  _CONNECTION_INFO_0 = record
    coni0_id: DWORD;
  end;
  TConnectionInfo0 = _CONNECTION_INFO_0;
  {$EXTERNALSYM CONNECTION_INFO_0}
  CONNECTION_INFO_0 = _CONNECTION_INFO_0;

  PConnectionInfo1 = ^TConnectionInfo1;
  {$EXTERNALSYM _CONNECTION_INFO_1}
  _CONNECTION_INFO_1 = record
    coni1_id: DWORD;
    coni1_type: DWORD;
    coni1_num_opens: DWORD;
    coni1_num_users: DWORD;
    coni1_time: DWORD;
    coni1_username: LPWSTR;
    coni1_netname: LPWSTR;
  end;
  TConnectionInfo1 = _CONNECTION_INFO_1;
  {$EXTERNALSYM CONNECTION_INFO_1}
  CONNECTION_INFO_1 = _CONNECTION_INFO_1;

// FILE API

// Function Prototypes - FILE

{$IFDEF LANMAN_DYNAMIC_LINK}
type
  TNetFileClose = function (servername: LPWSTR; fileid: DWORD): NET_API_STATUS; stdcall;

  TNetFileEnum = function (servername: LPWSTR; basepath: LPWSTR; username: LPWSTR;
    level: DWORD; var bufptr: Pointer; prefmaxlen: DWORD; var entriesread: DWORD;
    var totalentries: DWORD; resume_handle: PDWORD): NET_API_STATUS; stdcall;

  TNetFileGetInfo = function (servername: LPWSTR; fileid: DWORD; level: DWORD;
    bufptr: Pointer): NET_API_STATUS; stdcall;
{$ELSE}
function NetFileClose(servername: LPWSTR; fileid: DWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetFileClose}

function NetFileEnum(servername: LPWSTR; basepath: LPWSTR; username: LPWSTR;
  level: DWORD; var bufptr: Pointer; prefmaxlen: DWORD; var entriesread: DWORD;
  var totalentries: DWORD; resume_handle: PDWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetFileEnum}

function NetFileGetInfo(servername: LPWSTR; fileid: DWORD; level: DWORD;
  bufptr: Pointer): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetFileGetInfo}
{$ENDIF}

// Data Structures - File

// File APIs are available at information levels 2 & 3 only. Levels 0 &
// 1 are not supported.

type
  PFileInfo2 = ^TFileInfo2;
  {$EXTERNALSYM _FILE_INFO_2}
  _FILE_INFO_2 = record
    fi2_id: DWORD;
  end;
  TFileInfo2 = _FILE_INFO_2;
  {$EXTERNALSYM FILE_INFO_2}
  FILE_INFO_2 = _FILE_INFO_2;

  PFileInfo3 = ^TFileInfo3;
  {$EXTERNALSYM _FILE_INFO_3}
  _FILE_INFO_3 = record
    fi3_id: DWORD;
    fi3_permissions: DWORD;
    fi3_num_locks: DWORD;
    fi3_pathname: LPWSTR;
    fi3_username: LPWSTR;
  end;
  TFileInfo3 = _FILE_INFO_3;
  {$EXTERNALSYM FILE_INFO_3}
  FILE_INFO_3 = _FILE_INFO_3;

// bit values for permissions

const
  {$EXTERNALSYM PERM_FILE_READ}
  PERM_FILE_READ   = $01; // user has read access
  {$EXTERNALSYM PERM_FILE_WRITE}
  PERM_FILE_WRITE  = $02; // user has write access
  {$EXTERNALSYM PERM_FILE_CREATE}
  PERM_FILE_CREATE = $04; // user has create access

{$IFDEF LANMAN_DYNAMIC_LINK}
var
  {$EXTERNALSYM NetShareAdd}
  NetShareAdd: TNetShareAdd = nil;
  {$EXTERNALSYM NetShareEnum}
  NetShareEnum: TNetShareEnum = nil;
  {$EXTERNALSYM NetShareEnumSticky}
  NetShareEnumSticky: TNetShareEnumSticky = nil;
  {$EXTERNALSYM NetShareGetInfo}
  NetShareGetInfo: TNetShareGetInfo = nil;
  {$EXTERNALSYM NetShareSetInfo}
  NetShareSetInfo: TNetShareSetInfo = nil;
  {$EXTERNALSYM NetShareDel}
  NetShareDel: TNetShareDel = nil;
  {$EXTERNALSYM NetShareDelSticky}
  NetShareDelSticky: TNetShareDelSticky = nil;
  {$EXTERNALSYM NetShareCheck}
  NetShareCheck: TNetShareCheck = nil;
  {$EXTERNALSYM NetSessionEnum}
  NetSessionEnum: TNetSessionEnum = nil;
  {$EXTERNALSYM NetSessionDel}
  NetSessionDel: TNetSessionDel = nil;
  {$EXTERNALSYM NetSessionGetInfo}
  NetSessionGetInfo: TNetSessionGetInfo = nil;
  {$EXTERNALSYM NetConnectionEnum}
  NetConnectionEnum: TNetConnectionEnum = nil;
  {$EXTERNALSYM NetFileClose}
  NetFileClose: TNetFileClose = nil;
  {$EXTERNALSYM NetFileEnum}
  NetFileEnum: TNetFileEnum = nil;
  {$EXTERNALSYM NetFileGetInfo}
  NetFileGetInfo: TNetFileGetInfo = nil;
{$ENDIF}

{$IFDEF LANMAN_DYNAMIC_LINK}
function LmShareLoaded: Boolean;
{$IFDEF LANMAN_DYNAMIC_LINK_EXPLICIT}
function LoadLmShare: Boolean;
function UnloadLmShare: Boolean;
{$ENDIF}
{$ENDIF}

implementation

{$IFDEF LANMAN_DYNAMIC_LINK}
var
  LibHandle: THandle = 0;

function LmShareLoaded: Boolean;
begin
  Result := (LibHandle <> 0);
end;

function LoadLmShare: Boolean;
//----------------------------------------------------------------------------------------------------------------------------------
begin
	Result := LmShareLoaded;
	if Result then begin
		Exit;
	end;
	LibHandle := LoadLibrary(netapi32lib);
	Result := LmShareLoaded;
	if Result then begin
		@NetShareAdd := GetProcAddress(LibHandle, 'NetShareAdd');
		@NetShareEnum := GetProcAddress(LibHandle, 'NetShareEnum');
		@NetShareEnumSticky := GetProcAddress(LibHandle, 'NetShareEnumSticky');
		@NetShareGetInfo := GetProcAddress(LibHandle, 'NetShareGetInfo');
		@NetShareSetInfo := GetProcAddress(LibHandle, 'NetShareSetInfo');
		@NetShareDel := GetProcAddress(LibHandle, 'NetShareDel');
		@NetShareDelSticky := GetProcAddress(LibHandle, 'NetShareDelSticky');
		@NetShareCheck := GetProcAddress(LibHandle, 'NetShareCheck');
		@NetSessionEnum := GetProcAddress(LibHandle, 'NetSessionEnum');
		@NetSessionDel := GetProcAddress(LibHandle, 'NetSessionDel');
		@NetSessionGetInfo := GetProcAddress(LibHandle, 'NetSessionGetInfo');
		@NetConnectionEnum := GetProcAddress(LibHandle, 'NetConnectionEnum');
		@NetFileClose := GetProcAddress(LibHandle, 'NetFileClose');
		@NetFileEnum := GetProcAddress(LibHandle, 'NetFileEnum');
		@NetFileGetInfo := GetProcAddress(LibHandle, 'NetFileGetInfo');
	end;
end;

function UnloadLmShare: Boolean;
begin
  Result := True;
  if LmShareLoaded then
  begin
    Result := FreeLibrary(LibHandle);
    LibHandle := 0;
    @NetShareAdd := nil;
    @NetShareEnum := nil;
    @NetShareEnumSticky := nil;
    @NetShareGetInfo := nil;
    @NetShareSetInfo := nil;
    @NetShareDel := nil;
    @NetShareDelSticky := nil;
    @NetShareCheck := nil;
    @NetSessionEnum := nil;
    @NetSessionDel := nil;
    @NetSessionGetInfo := nil;
    @NetConnectionEnum := nil;
    @NetFileClose := nil;
    @NetFileEnum := nil;
    @NetFileGetInfo := nil;
  end;
end;

{$ELSE}
function NetShareAdd; external netapi32lib name 'NetShareAdd';
function NetShareEnum; external netapi32lib name 'NetShareEnum';
function NetShareEnumSticky; external netapi32lib name 'NetShareEnumSticky';
function NetShareGetInfo; external netapi32lib name 'NetShareGetInfo';
function NetShareSetInfo; external netapi32lib name 'NetShareSetInfo';
function NetShareDel; external netapi32lib name 'NetShareDel';
function NetShareDelSticky; external netapi32lib name 'NetShareDelSticky';
function NetShareCheck; external netapi32lib name 'NetShareCheck';
function NetSessionEnum; external netapi32lib name 'NetSessionEnum';
function NetSessionDel; external netapi32lib name 'NetSessionDel';
function NetSessionGetInfo; external netapi32lib name 'NetSessionGetInfo';
function NetConnectionEnum; external netapi32lib name 'NetConnectionEnum';
function NetFileClose; external netapi32lib name 'NetFileClose';
function NetFileEnum; external netapi32lib name 'NetFileEnum';
function NetFileGetInfo; external netapi32lib name 'NetFileGetInfo';
{$ENDIF}

{$IFDEF LANMAN_DYNAMIC_LINK}
initialization
{$IFNDEF LANMAN_DYNAMIC_LINK_EXPLICIT}
  LoadLmShare;
{$ENDIF}
finalization
  UnloadLmShare;
{$ENDIF}
end.
