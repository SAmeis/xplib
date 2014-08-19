{******************************************************************}
{                                                                  }
{ Borland Delphi Runtime Library                                   }
{ LanManager share functions for Windows NT interface unit         }
{                                                                  }
{ Portions created by Microsoft are                                }
{ Copyright (C) 1995-1999 Microsoft Corporation.                   }
{ All Rights Reserved.                                             }
{                                                                  }
{ The original file is: lmalert.h, released 14 Nov 1998.           }
{ The original Pascal code is: LmAlert.pas, released 11 Jan 2000.  }
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

unit LmAlert;

{$I LANMAN.INC}

{$ALIGN ON}
{$WEAKPACKAGEUNIT}
{$MINENUMSIZE 4}

interface

uses
  Windows, LmCons;

(*$HPPEMIT '#include <lmcons.h>'*)
(*$HPPEMIT '#include <lmalert.h>'*)

// Function Prototypes

{$EXTERNALSYM NetAlertRaise}
function NetAlertRaise(AlertEventName: LPCWSTR; Buffer: Pointer;
  BufferSize: DWORD): NET_API_STATUS; stdcall;

{$EXTERNALSYM NetAlertRaiseEx}
function NetAlertRaiseEx(AlertEventName: LPCWSTR; VariableInfo: Pointer;
  VariableInfoSize: DWORD; ServiceName: LPCWSTR): NET_API_STATUS; stdcall;

//  Data Structures

type
  PStdAlert = ^TStdAlert;
  {$EXTERNALSYM _STD_ALERT}
  _STD_ALERT = record
    alrt_timestamp: DWORD;
    alrt_eventname: packed array[0..EVLEN] of WCHAR;
    alrt_servicename: packed array[0..SNLEN] of WCHAR;
  end;
  TStdAlert = _STD_ALERT;
  STD_ALERT = _STD_ALERT;
  {$EXTERNALSYM STD_ALERT}

  PAdminOtherInfo = ^TAdminOtherInfo;
  {$EXTERNALSYM _ADMIN_OTHER_INFO}
  _ADMIN_OTHER_INFO = record
    alrtad_errcode: DWORD;
    alrtad_numstrings: DWORD;
  end;
  TAdminOtherInfo = _ADMIN_OTHER_INFO;
  ADMIN_OTHER_INFO = _ADMIN_OTHER_INFO;
  {$EXTERNALSYM ADMIN_OTHER_INFO}

  PErrorOtherInfo = ^TErrorOtherInfo;
  {$EXTERNALSYM _ERRLOG_OTHER_INFO}
  _ERRLOG_OTHER_INFO = record
    alrter_errcode: DWORD;
    alrter_offset: DWORD;
  end;
  TErrorOtherInfo = _ERRLOG_OTHER_INFO;
  ERRLOG_OTHER_INFO = _ERRLOG_OTHER_INFO;
  {$EXTERNALSYM ERRLOG_OTHER_INFO}

  PPrintOtherInfo = ^TPrintOtherInfo;
  {$EXTERNALSYM _PRINT_OTHER_INFO}
  _PRINT_OTHER_INFO = record
    alrtpr_jobid: DWORD;
    alrtpr_status: DWORD;
    alrtpr_submitted: DWORD;
    alrtpr_size: DWORD;
  end;
  TPrintOtherInfo = _PRINT_OTHER_INFO;
  PRINT_OTHER_INFO = _PRINT_OTHER_INFO;
  {$EXTERNALSYM PRINT_OTHER_INFO}

  PUserOtherInfo = ^TUserOtherInfo;
  {$EXTERNALSYM _USER_OTHER_INFO}
  _USER_OTHER_INFO = record
    alrtus_errcode: DWORD;
    alrtus_numstrings: DWORD;
  end;
  TUserOtherInfo = _USER_OTHER_INFO;
  USER_OTHER_INFO = _USER_OTHER_INFO;
  {$EXTERNALSYM USER_OTHER_INFO}

// Special Values and Constants

// Name of mailslot to send alert notifications

const
  {$EXTERNALSYM ALERTER_MAILSLOT}
  ALERTER_MAILSLOT = '\\.\MAILSLOT\Alerter';

// The following macro gives a pointer to the other_info data.
// It takes an alert structure and returns a pointer to structure
// beyond the standard portion.


  function ALERT_OTHER_INFO(x: Pointer): Pointer;    //((LPBYTE)(x) + sizeof(STD_ALERT))

// The following macro gives a pointer to the variable-length data.
// It takes a pointer to one of the other-info structs and returns a
// pointer to the variable data portion.

  function ALERT_VAR_DATA(const p): Pointer;      //((LPBYTE)(p) + sizeof(*p))

//      Names of standard Microsoft-defined alert events.

const

  {$EXTERNALSYM ALERT_PRINT_EVENT}
  ALERT_PRINT_EVENT           = 'PRINTING';
  {$EXTERNALSYM ALERT_MESSAGE_EVENT}
  ALERT_MESSAGE_EVENT         = 'MESSAGE';
  {$EXTERNALSYM ALERT_ERRORLOG_EVENT}
  ALERT_ERRORLOG_EVENT        = 'ERRORLOG';
  {$EXTERNALSYM ALERT_ADMIN_EVENT}
  ALERT_ADMIN_EVENT           = 'ADMIN';
  {$EXTERNALSYM ALERT_USER_EVENT}
  ALERT_USER_EVENT            = 'USER';

//      Bitmap masks for prjob_status field of PRINTJOB.

// 2-7 bits also used in device status

  {$EXTERNALSYM PRJOB_QSTATUS}
  PRJOB_QSTATUS       = $3;         // Bits 0,1
  {$EXTERNALSYM PRJOB_DEVSTATUS}
  PRJOB_DEVSTATUS     = $1fc;       // 2-8 bits
  {$EXTERNALSYM PRJOB_COMPLETE}
  PRJOB_COMPLETE      = $4;         // Bit 2
  {$EXTERNALSYM PRJOB_INTERV}
  PRJOB_INTERV        = $8;         // Bit 3
  {$EXTERNALSYM PRJOB_ERROR}
  PRJOB_ERROR         = $10;        // Bit 4
  {$EXTERNALSYM PRJOB_DESTOFFLINE}
  PRJOB_DESTOFFLINE   = $20;        // Bit 5
  {$EXTERNALSYM PRJOB_DESTPAUSED}
  PRJOB_DESTPAUSED    = $40;        // Bit 6
  {$EXTERNALSYM PRJOB_NOTIFY}
  PRJOB_NOTIFY        = $80;        // BIT 7
  {$EXTERNALSYM PRJOB_DESTNOPAPER}
  PRJOB_DESTNOPAPER   = $100;       // BIT 8
  {$EXTERNALSYM PRJOB_DELETED}
  PRJOB_DELETED       = $8000;      // BIT 15

//      Values of PRJOB_QSTATUS bits in prjob_status field of PRINTJOB.

  {$EXTERNALSYM PRJOB_QS_QUEUED}
  PRJOB_QS_QUEUED                 = 0;
  {$EXTERNALSYM PRJOB_QS_PAUSED}
  PRJOB_QS_PAUSED                 = 1;
  {$EXTERNALSYM PRJOB_QS_SPOOLING}
  PRJOB_QS_SPOOLING               = 2;
  {$EXTERNALSYM PRJOB_QS_PRINTING}
  PRJOB_QS_PRINTING               = 3;


implementation

function ALERT_OTHER_INFO(x: Pointer): Pointer;
begin
  Result := PChar(x) + Sizeof(STD_ALERT);
end;

function ALERT_VAR_DATA(const p): Pointer;
begin
  Result := PChar(p) + Sizeof(p);
end;

function NetAlertRaise; external netapi32lib name 'NetAlertRaise';

function NetAlertRaiseEx; external netapi32lib name 'NetAlertRaiseEx';

end.
