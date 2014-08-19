{******************************************************************}
{                                                                  }
{ Borland Delphi Runtime Library                                   }
{ LanManager share functions for Windows NT interface unit         }
{                                                                  }
{ Portions created by Microsoft are                                }
{ Copyright (C) 1995-1999 Microsoft Corporation.                   }
{ All Rights Reserved.                                             }
{                                                                  }
{ The original file is: lmchdev.h, released 27 Jan 1999.           }
{ The original Pascal code is: LmChdev.pas, released 17 Jan 2000.  }
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

unit LmChdev;

{$I LANMAN.INC}

{$ALIGN ON}
{$MINENUMSIZE 4}
{$WEAKPACKAGEUNIT}

interface

uses
  Windows, LmCons;

(*$HPPEMIT '#include <netcons.h>'*)

// CharDev Class

// Function Prototypes - CharDev

function NetCharDevEnum(servername: LPCWSTR; level: DWORD; bufptr: Pointer;
  prefmaxlen: DWORD; var entriesread: DWORD; var totalentries: DWORD;
  resume_handle: DWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetCharDevEnum}

function NetCharDevGetInfo(servername: LPCWSTR; devname: LPCWSTR;
  level: DWORD; bufptr: Pointer): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetCharDevGetInfo}

function NetCharDevControl(servername: LPCWSTR; devname: LPCWSTR;
  opcode: DWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetCharDevControl}

// Data Structures - CharDev

type
  PChardevInfo0 = ^TChardevInfo0;
  _CHARDEV_INFO_0 = record
    ch0_dev: LPWSTR;
  end;
  {$EXTERNALSYM _CHARDEV_INFO_0}
  TChardevInfo0 = _CHARDEV_INFO_0;
  CHARDEV_INFO_0 = _CHARDEV_INFO_0;
  {$EXTERNALSYM CHARDEV_INFO_0}

  PChardevInfo1 = ^TChardevInfo1;
  _CHARDEV_INFO_1 = record
    ch1_dev: LPWSTR;
    ch1_status: DWORD;
    ch1_username: LPWSTR;
    ch1_time: DWORD;
  end;
  {$EXTERNALSYM _CHARDEV_INFO_1}
  TChardevInfo1 = _CHARDEV_INFO_1;
  CHARDEV_INFO_1 = _CHARDEV_INFO_1;
  {$EXTERNALSYM CHARDEV_INFO_1}

// CharDevQ Class

// Function Prototypes - CharDevQ

function NetCharDevQEnum(servername: LPCWSTR; username: LPCWSTR; level: DWORD;
  bufptr: Pointer; prefmaxlen: DWORD; var entriesread: DWORD;
  var totalentries: DWORD; resume_handle: PDWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetCharDevQEnum}

function NetCharDevQGetInfo(servername, queuename, username: LPCWSTR;
  level: DWORD; bufptr: Pointer): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetCharDevQGetInfo}

function NetCharDevQSetInfo(servername, queuename: LPCWSTR; level: DWORD;
  buf: Pointer; parm_err: PDWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetCharDevQSetInfo}

function NetCharDevQPurge(servername, queuename: LPCWSTR): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetCharDevQPurge}

function NetCharDevQPurgeSelf(servername, queuename, computername: LPCWSTR): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetCharDevQPurgeSelf}

// Data Structures - CharDevQ

type
  PChardevqInfo0 = ^TChardevqInfo0;
  _CHARDEVQ_INFO_0 = record
    cq0_dev: LPWSTR;
  end;
  {$EXTERNALSYM _CHARDEVQ_INFO_0}
  TChardevqInfo0 = _CHARDEVQ_INFO_0;
  CHARDEVQ_INFO_0 = _CHARDEVQ_INFO_0;
  {$EXTERNALSYM CHARDEVQ_INFO_0}

  PChardevqInfo1 = ^TChardevqInfo1;
  _CHARDEVQ_INFO_1 = record
    cq1_dev: LPWSTR;
    cq1_priority: DWORD;
    cq1_devs: LPWSTR;
    cq1_numusers: DWORD;
    cq1_numahead: DWORD;
  end;
  {$EXTERNALSYM _CHARDEVQ_INFO_1}
  TChardevqInfo1 = _CHARDEVQ_INFO_1;
  CHARDEVQ_INFO_1 = _CHARDEVQ_INFO_1;
  {$EXTERNALSYM CHARDEVQ_INFO_1}

  PChardevqInfo1002 = ^TChardevqInfo1002;
  _CHARDEVQ_INFO_1002 = record
    cq1002_priority: DWORD;
  end;
  {$EXTERNALSYM _CHARDEVQ_INFO_1002}
  TChardevqInfo1002 = _CHARDEVQ_INFO_1002;
  CHARDEVQ_INFO_1002 = _CHARDEVQ_INFO_1002;
  {$EXTERNALSYM CHARDEVQ_INFO_1002}

  PChardevqInfo1003 = ^TChardevqInfo1003;
  _CHARDEVQ_INFO_1003 = record
    cq1003_devs: LPWSTR;
  end;
  {$EXTERNALSYM _CHARDEVQ_INFO_1003}
  TChardevqInfo1003 = _CHARDEVQ_INFO_1003;
  CHARDEVQ_INFO_1003 = _CHARDEVQ_INFO_1003;
  {$EXTERNALSYM CHARDEVQ_INFO_1003}


// Bits for chardev_info_1 field ch1_status.

const
  CHARDEV_STAT_OPENED             = $02;
  {$EXTERNALSYM CHARDEV_STAT_OPENED}
  CHARDEV_STAT_ERROR              = $04;
  {$EXTERNALSYM CHARDEV_STAT_ERROR}

// Opcodes for NetCharDevControl

  CHARDEV_CLOSE                   = 0;
  {$EXTERNALSYM CHARDEV_CLOSE}

// Values for parm_err parameter.

  CHARDEVQ_DEV_PARMNUM        = 1;
  {$EXTERNALSYM CHARDEVQ_DEV_PARMNUM}
  CHARDEVQ_PRIORITY_PARMNUM   = 2;
  {$EXTERNALSYM CHARDEVQ_PRIORITY_PARMNUM}
  CHARDEVQ_DEVS_PARMNUM       = 3;
  {$EXTERNALSYM CHARDEVQ_DEVS_PARMNUM}
  CHARDEVQ_NUMUSERS_PARMNUM   = 4;
  {$EXTERNALSYM CHARDEVQ_NUMUSERS_PARMNUM}
  CHARDEVQ_NUMAHEAD_PARMNUM   = 5;
  {$EXTERNALSYM CHARDEVQ_NUMAHEAD_PARMNUM}

// Single-field infolevels for NetCharDevQSetInfo.

  CHARDEVQ_PRIORITY_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + CHARDEVQ_PRIORITY_PARMNUM);
  {$EXTERNALSYM CHARDEVQ_PRIORITY_INFOLEVEL}
  CHARDEVQ_DEVS_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + CHARDEVQ_DEVS_PARMNUM);
  {$EXTERNALSYM CHARDEVQ_DEVS_INFOLEVEL}

// Minimum, maximum, and recommended default for priority.

  CHARDEVQ_MAX_PRIORITY           = 1;
  {$EXTERNALSYM CHARDEVQ_MAX_PRIORITY}
  CHARDEVQ_MIN_PRIORITY           = 9;
  {$EXTERNALSYM CHARDEVQ_MIN_PRIORITY}
  CHARDEVQ_DEF_PRIORITY           = 5;
  {$EXTERNALSYM CHARDEVQ_DEF_PRIORITY}

// Value indicating no requests in the queue.

  CHARDEVQ_NO_REQUESTS            = -1;
  {$EXTERNALSYM CHARDEVQ_NO_REQUESTS}

// Handle Class

// Function Prototypes

function NetHandleGetInfo(handle: THandle; level: DWORD; bufptr: Pointer): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetHandleGetInfo}

function NetHandleSetInfo(handle: THandle; level: DWORD; buf: Pointer;
  parmnum: DWORD; parmerr: PDWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetHandleSetInfo}

//
//  Data Structures
//

type
  PHandleInfo1 = ^THandleInfo1;
  _HANDLE_INFO_1 = record
    hdli1_chartime: DWORD;
    hdli1_charcount: DWORD;
  end;
  {$EXTERNALSYM _HANDLE_INFO_1}
  THandleInfo1 = _HANDLE_INFO_1;
  HANDLE_INFO_1 = _HANDLE_INFO_1;
  {$EXTERNALSYM HANDLE_INFO_1}

// Handle Get Info Levels

const
  HANDLE_INFO_LEVEL_1                 = 1;
  {$EXTERNALSYM HANDLE_INFO_LEVEL_1}

// Handle Set Info parm numbers

  HANDLE_CHARTIME_PARMNUM     = 1;
  {$EXTERNALSYM HANDLE_CHARTIME_PARMNUM}
  HANDLE_CHARCOUNT_PARMNUM    = 2;
  {$EXTERNALSYM HANDLE_CHARCOUNT_PARMNUM}

implementation

function NetCharDevEnum; external netapi32lib name 'NetCharDevEnum';
function NetCharDevGetInfo; external netapi32lib name 'NetCharDevGetInfo';
function NetCharDevControl; external netapi32lib name 'NetCharDevControl';
function NetCharDevQEnum; external netapi32lib name 'NetCharDevQEnum';
function NetCharDevQGetInfo; external netapi32lib name 'NetCharDevQGetInfo';
function NetCharDevQSetInfo; external netapi32lib name 'NetCharDevQSetInfo';
function NetCharDevQPurge; external netapi32lib name 'NetCharDevQPurge';
function NetCharDevQPurgeSelf; external netapi32lib name 'NetCharDevQPurgeSelf';
function NetHandleGetInfo; external netapi32lib name 'NetHandleGetInfo';
function NetHandleSetInfo; external netapi32lib name 'NetHandleSetInfo';

end.
