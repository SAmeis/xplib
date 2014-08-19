{******************************************************************}
{                                                                  }
{ Borland Delphi Runtime Library                                   }
{ LanManager share functions for Windows NT interface unit         }
{                                                                  }
{ Portions created by Microsoft are                                }
{ Copyright (C) 1995-1999 Microsoft Corporation.                   }
{ All Rights Reserved.                                             }
{                                                                  }
{ The original file is: lmat.h, released 14 Nov 1998.              }
{ The original Pascal code is: LmAt.pas, released 11 Jan 2000.     }
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

unit LmAt;

{$I LANMAN.INC}

{$ALIGN ON}
{$MINENUMSIZE 4}
{$WEAKPACKAGEUNIT}

interface

uses
  Windows, LmCons;

(*$HPPEMIT '#include <netcons.h>'*)
(*$HPPEMIT '#include <lmat.h>'*)

//  The following bits are used with Flags field in structures below.

//  Do we exec programs for this job periodically (/EVERY switch)
//  or one time (/NEXT switch).

const
  {$EXTERNALSYM JOB_RUN_PERIODICALLY}
  JOB_RUN_PERIODICALLY            = $01;    //  set if EVERY

//  Was there an error last time we tried to exec a program on behalf of
//  this job.
//  This flag is meaningfull on output only!

  {$EXTERNALSYM JOB_EXEC_ERROR}
  JOB_EXEC_ERROR                  = $02;    //  set if error

//  Will this job run today or tomorrow.
//  This flag is meaningfull on output only!

  {$EXTERNALSYM JOB_RUNS_TODAY}
  JOB_RUNS_TODAY                  = $04;    //  set if today

//  Add current day of the month to DaysOfMonth input.
//  This flag is meaningfull on input only!

  {$EXTERNALSYM JOB_ADD_CURRENT_DATE}
  JOB_ADD_CURRENT_DATE            = $08;    // set if to add current date


//  Will this job be run interactively or not.  Windows NT 3.1 do not
//  know about this bit, i.e. they submit interactive jobs only.

  {$EXTERNALSYM JOB_NONINTERACTIVE}
  JOB_NONINTERACTIVE              = $10;    // set for noninteractive

  {$EXTERNALSYM JOB_INPUT_FLAGS}
  JOB_INPUT_FLAGS = JOB_RUN_PERIODICALLY or JOB_ADD_CURRENT_DATE or
    JOB_NONINTERACTIVE;

  {$EXTERNALSYM JOB_OUTPUT_FLAGS}
  JOB_OUTPUT_FLAGS = JOB_RUN_PERIODICALLY or JOB_EXEC_ERROR or JOB_RUNS_TODAY or
    JOB_NONINTERACTIVE;


type
  PAtInfo = ^TAtInfo;
  {$EXTERNALSYM _AT_INFO}
  _AT_INFO = record
    JobTime: DWORD;
    DaysOfMonth: DWORD;
    DaysOfWeek: UCHAR;
    Flags: UCHAR;
   Command: LPWSTR;
  end;
  TAtInfo = _AT_INFO;
  {$EXTERNALSYM AT_INFO}
  AT_INFO = _AT_INFO;

  PAtEnum = ^TAtEnum;
  {$EXTERNALSYM _AT_ENUM}
  _AT_ENUM = record
    JobId: DWORD;
    JobTime: DWORD;
    DaysOfMonth: DWORD;
    DaysOfWeek: UCHAR;
    Flags: UCHAR;
    Command: LPWSTR;
  end;
  TAtEnum = _AT_ENUM;
  {$EXTERNALSYM AT_ENUM}
  AT_ENUM = _AT_ENUM;
  
{$EXTERNALSYM NetScheduleJobAdd}
function NetScheduleJobAdd(Servername: LPCWSTR; Buffer: Pointer;
  var JobId: DWORD): NET_API_STATUS; stdcall;

{$EXTERNALSYM NetScheduleJobDel}
function NetScheduleJobDel(Servername: LPCWSTR; MinJobId: DWORD;
  MaxJobId: DWORD): NET_API_STATUS; stdcall;

{$EXTERNALSYM NetScheduleJobEnum}
function NetScheduleJobEnum( Servername: LPCWSTR; PointerToBuffer: Pointer;
  PrefferedMaximumLength: DWORD; var EntriesRead: DWORD; var TotalEntries: DWORD;
  ResumeHandle: PDWORD): NET_API_STATUS; stdcall;

{$EXTERNALSYM NetScheduleJobGetInfo}
function NetScheduleJobGetInfo(Servername: LPCWSTR; JobId: DWORD;
  PointerToBuffer: Pointer): NET_API_STATUS; stdcall;

implementation

function NetScheduleJobAdd; external netapi32lib name 'NetScheduleJobAdd';
function NetScheduleJobDel; external netapi32lib name 'NetScheduleJobDel';
function NetScheduleJobEnum; external netapi32lib name 'NetScheduleJobEnum';
function NetScheduleJobGetInfo; external netapi32lib name 'NetScheduleJobGetInfo';

end.
