{******************************************************************}
{                                                                  }
{ Borland Delphi Runtime Library                                   }
{ LanManager share functions for Windows NT interface unit         }
{                                                                  }
{ Portions created by Microsoft are                                }
{ Copyright (C) 1995-1999 Microsoft Corporation.                   }
{ All Rights Reserved.                                             }
{                                                                  }
{ The original file is: lmstats.h, released 14 Nov 1998.           }
{ The original Pascal code is: LmStats.pas, released 13 Jan 2000.  }
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

unit LmStats;

{$I LANMAN.INC}

{$ALIGN ON}
{$MINENUMSIZE 4}
{$WEAKPACKAGEUNIT}

interface

uses
  Windows, LmCons;

(*$HPPEMIT '#include <netcons.h>'*)
(*$HPPEMIT '#include <lmcons.h>'*)
(*$HPPEMIT '#include <lmstats.h>'*)

// Function Prototypes - Statistics

function NetStatisticsGet(server: LPWSTR; service: LPWSTR; level: DWORD;
  options: DWORD; bufptr: Pointer): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetStatisticsGet}

// Data Structures - Statistics

type
{$IFDEF LM20_WORKSTATION_STATISTICS}
  PStatWorkstation0 = ^TStatWorkstation0;
  _STAT_WORKSTATION_0 = record
    stw0_start: DWORD;
    stw0_numNCB_r: DWORD;
    stw0_numNCB_s: DWORD;
    stw0_numNCB_a: DWORD;
    stw0_fiNCB_r: DWORD;
    stw0_fiNCB_s: DWORD;
    stw0_fiNCB_a: DWORD;
    stw0_fcNCB_r: DWORD;
    stw0_fcNCB_s: DWORD;
    stw0_fcNCB_a: DWORD;
    stw0_sesstart: DWORD;
    stw0_sessfailcon: DWORD;
    stw0_sessbroke: DWORD;
    stw0_uses: DWORD;
    stw0_usefail: DWORD;
    stw0_autorec: DWORD;
    stw0_bytessent_r_lo: DWORD;
    stw0_bytessent_r_hi: DWORD;
    stw0_bytesrcvd_r_lo: DWORD;
    stw0_bytesrcvd_r_hi: DWORD;
    stw0_bytessent_s_lo: DWORD;
    stw0_bytessent_s_hi: DWORD;
    stw0_bytesrcvd_s_lo: DWORD;
    stw0_bytesrcvd_s_hi: DWORD;
    stw0_bytessent_a_lo: DWORD;
    stw0_bytessent_a_hi: DWORD;
    stw0_bytesrcvd_a_lo: DWORD;
    stw0_bytesrcvd_a_hi: DWORD;
    stw0_reqbufneed: DWORD;
    stw0_bigbufneed: DWORD;
  end;
  {$EXTERNALSYM _STAT_WORKSTATION_0}
  TStatWorkstation0 = _STAT_WORKSTATION_0;
  STAT_WORKSTATION_0 = _STAT_WORKSTATION_0;
  {$EXTERNALSYM STAT_WORKSTATION_0}

{$ELSE}

// NB: The following structure is REDIR_STATISTICS in sdk\inc\ntddnfs.h. If you
// change the structure, change it in both places

  PStatWorkstation0 = ^TStatWorkstation0;
  _STAT_WORKSTATION_0 = record
    StatisticsStartTime: LARGE_INTEGER;

    BytesReceived: LARGE_INTEGER;
    SmbsReceived: LARGE_INTEGER;
    PagingReadBytesRequested: LARGE_INTEGER;
    NonPagingReadBytesRequested: LARGE_INTEGER;
    CacheReadBytesRequested: LARGE_INTEGER;
    NetworkReadBytesRequested: LARGE_INTEGER;

    BytesTransmitted: LARGE_INTEGER;
    SmbsTransmitted: LARGE_INTEGER;
    PagingWriteBytesRequested: LARGE_INTEGER;
    NonPagingWriteBytesRequested: LARGE_INTEGER;
    CacheWriteBytesRequested: LARGE_INTEGER;
    NetworkWriteBytesRequested: LARGE_INTEGER;

    InitiallyFailedOperations: DWORD;
    FailedCompletionOperations: DWORD;

    ReadOperations: DWORD;
    RandomReadOperations: DWORD;
    ReadSmbs: DWORD;
    LargeReadSmbs: DWORD;
    SmallReadSmbs: DWORD;

    WriteOperations: DWORD;
    RandomWriteOperations: DWORD;
    WriteSmbs: DWORD;
    LargeWriteSmbs: DWORD;
    SmallWriteSmbs: DWORD;

    RawReadsDenied: DWORD;
    RawWritesDenied: DWORD;

    NetworkErrors: DWORD;

    //  Connection/Session counts
    Sessions: DWORD;
    FailedSessions: DWORD;
    Reconnects: DWORD;
    CoreConnects: DWORD;
    Lanman20Connects: DWORD;
    Lanman21Connects: DWORD;
    LanmanNtConnects: DWORD;
    ServerDisconnects: DWORD;
    HungSessions: DWORD;
    UseCount: DWORD;
    FailedUseCount: DWORD;

    //  Queue Lengths (updates protected by RdrMpxTableSpinLock NOT
    //  RdrStatisticsSpinlock)
    CurrentCommands: DWORD;
  end;
  {$EXTERNALSYM _STAT_WORKSTATION_0}
  TStatWorkstation0 = _STAT_WORKSTATION_0;
  STAT_WORKSTATION_0 = _STAT_WORKSTATION_0;
  {$EXTERNALSYM STAT_WORKSTATION_0}

{$ENDIF}

  PStatServer0 = ^TStatServer0;
  _STAT_SERVER_0 = record
    sts0_start: DWORD;
    sts0_fopens: DWORD;
    sts0_devopens: DWORD;
    sts0_jobsqueued: DWORD;
    sts0_sopens: DWORD;
    sts0_stimedout: DWORD;
    sts0_serrorout: DWORD;
    sts0_pwerrors: DWORD;
    sts0_permerrors: DWORD;
    sts0_syserrors: DWORD;
    sts0_bytessent_low: DWORD;
    sts0_bytessent_high: DWORD;
    sts0_bytesrcvd_low: DWORD;
    sts0_bytesrcvd_high: DWORD;
    sts0_avresponse: DWORD;
    sts0_reqbufneed: DWORD;
    sts0_bigbufneed: DWORD;
  end;
  {$EXTERNALSYM _STAT_SERVER_0}
  TStatServer0 = _STAT_SERVER_0;
  STAT_SERVER_0 = _STAT_SERVER_0;
  {$EXTERNALSYM STAT_SERVER_0}

// Special Values and Constants

const
  STATSOPT_CLR    = 1;
{$EXTERNALSYM STATSOPT_CLR}
  STATS_NO_VALUE  = DWORD(-1);
{$EXTERNALSYM STATS_NO_VALUE}
  STATS_OVERFLOW  = DWORD(-2);
{$EXTERNALSYM STATS_OVERFLOW}

implementation

function NetStatisticsGet; external netapi32lib name 'NetStatisticsGet';

end.
