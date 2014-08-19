{******************************************************************}
{                                                                  }
{ Borland Delphi Runtime Library                                   }
{ LanManager share functions for Windows NT interface unit         }
{                                                                  }
{ Portions created by Microsoft are                                }
{ Copyright (C) 1995-1999 Microsoft Corporation.                   }
{ All Rights Reserved.                                             }
{                                                                  }
{ The original file is: lmuse.h, released 24 Nov 1998.             }
{ The original Pascal code is: LmUse.pas, released 14 Jan 2000.    }
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

unit LmUse;

{$I LANMAN.INC}

{$ALIGN ON}
{$MINENUMSIZE 4}
{$WEAKPACKAGEUNIT}

interface

uses
  Windows, LmCons, LmUseflg;

(*$HPPEMIT '#include <netcons.h>'*)
(*$HPPEMIT '#include <lmcons.h>'*)
(*$HPPEMIT '#include <lmuseflg.h>'*)
(*$HPPEMIT '#include <lmuse.h>'*)

function NetUseAdd(UncServerName: LPWSTR; Level: DWORD; Buf: Pointer;
  ParmError: PDWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetUseAdd}

function NetUseDel(UncServerName, UseName: LPWSTR; ForceCond: DWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetUseDel}

function NetUseEnum(UncServerName: LPWSTR; Level: DWORD; BufPtr: Pointer;
  PreferedMaximumSize: DWORD; var EntriesRead: DWORD; var TotalEntries: DWORD;
  ResumeHandle: PDWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetUseEnum}

function NetUseGetInfo(UncServerName: LPWSTR; UseName: LPWSTR; Level: DWORD;
  BufPtr: Pointer): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetUseGetInfo}

type
  PUseInfo0 = ^TUseInfo0;
  _USE_INFO_0 = record
    ui0_local: LPWSTR;
    ui0_remote: LPWSTR;
  end;
  {$EXTERNALSYM _USE_INFO_0}
  TUseInfo0 = _USE_INFO_0;
  USE_INFO_0 = _USE_INFO_0;
  {$EXTERNALSYM USE_INFO_0}


  PUseInfo1 = ^TUseInfo1;
  _USE_INFO_1 = record
    ui1_local: LPWSTR;
    ui1_remote: LPWSTR;
    ui1_password: LPWSTR;
    ui1_status: DWORD;
    ui1_asg_type: DWORD;
    ui1_refcount: DWORD;
    ui1_usecount: DWORD;
  end;
  {$EXTERNALSYM _USE_INFO_1}
  TUseInfo1 = _USE_INFO_1;
  USE_INFO_1 = _USE_INFO_1;
  {$EXTERNALSYM USE_INFO_1}


  PUseInfo2 = ^TUseInfo2;
  _USE_INFO_2 = record
    ui2_local: LPWSTR;
    ui2_remote: LPWSTR;
    ui2_password: LPWSTR;
    ui2_status: DWORD;
    ui2_asg_type: DWORD;
    ui2_refcount: DWORD;
    ui2_usecount: DWORD;
    ui2_username: LPWSTR;
    ui2_domainname: LPWSTR;
  end;
  {$EXTERNALSYM _USE_INFO_2}
  TUseInfo2 = _USE_INFO_2;
  USE_INFO_2 = _USE_INFO_2;
  {$EXTERNALSYM USE_INFO_2}


  PUseInfo3 = ^TUseInfo3;
  _USE_INFO_3 = record
    ui3_ui2: USE_INFO_2;
    ui3_flags: ULONG;
  end;
  {$EXTERNALSYM _USE_INFO_3}
  TUseInfo3 = _USE_INFO_3;
  USE_INFO_3 = _USE_INFO_3;
  {$EXTERNALSYM USE_INFO_3}

// One of these values indicates the parameter within an information
// structure that is invalid when ERROR_INVALID_PARAMETER is returned by
// NetUseAdd.

const
  USE_LOCAL_PARMNUM       = 1;
  {$EXTERNALSYM USE_LOCAL_PARMNUM}
  USE_REMOTE_PARMNUM      = 2;
  {$EXTERNALSYM USE_REMOTE_PARMNUM}
  USE_PASSWORD_PARMNUM    = 3;
  {$EXTERNALSYM USE_PASSWORD_PARMNUM}
  USE_ASGTYPE_PARMNUM     = 4;
  {$EXTERNALSYM USE_ASGTYPE_PARMNUM}
  USE_USERNAME_PARMNUM    = 5;
  {$EXTERNALSYM USE_USERNAME_PARMNUM}
  USE_DOMAINNAME_PARMNUM  = 6;
  {$EXTERNALSYM USE_DOMAINNAME_PARMNUM}

// Values appearing in the ui1_status field of use_info_1 structure.
// Note that USE_SESSLOST and USE_DISCONN are synonyms.

  USE_OK                  = 0;
  {$EXTERNALSYM USE_OK}
  USE_PAUSED              = 1;
  {$EXTERNALSYM USE_PAUSED}
  USE_SESSLOST            = 2;
  {$EXTERNALSYM USE_SESSLOST}
  USE_DISCONN             = 2;
  {$EXTERNALSYM USE_DISCONN}
  USE_NETERR              = 3;
  {$EXTERNALSYM USE_NETERR}
  USE_CONN                = 4;
  {$EXTERNALSYM USE_CONN}
  USE_RECONN              = 5;
  {$EXTERNALSYM USE_RECONN}

// Values of the ui1_asg_type field of use_info_1 structure

  USE_WILDCARD            = DWORD(-1);
  {$EXTERNALSYM USE_WILDCARD}
  USE_DISKDEV             = 0;
  {$EXTERNALSYM USE_DISKDEV}
  USE_SPOOLDEV            = 1;
  {$EXTERNALSYM USE_SPOOLDEV}
  USE_CHARDEV             = 2;
  {$EXTERNALSYM USE_CHARDEV}
  USE_IPC                 = 3;
  {$EXTERNALSYM USE_IPC}

// Flags defined in the use_info_3 structure

  CREATE_NO_CONNECT = $1;        // creation flags
  {$EXTERNALSYM CREATE_NO_CONNECT}
  CREATE_BYPASS_CSC = $2;        // force connection to server, bypassing CSC
                                 //  all ops on this connection go to the server,
                                 //  never to the cache
  {$EXTERNALSYM CREATE_BYPASS_CSC}

implementation

function NetUseAdd; external netapi32lib name 'NetUseAdd';
function NetUseDel; external netapi32lib name 'NetUseDel';
function NetUseEnum; external netapi32lib name 'NetUseEnum';
function NetUseGetInfo; external netapi32lib name 'NetUseGetInfo';

end.
