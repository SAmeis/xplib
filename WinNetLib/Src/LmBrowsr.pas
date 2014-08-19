{******************************************************************}
{                                                                  }
{ Borland Delphi Runtime Library                                   }
{ LanManager share functions for Windows NT interface unit         }
{                                                                  }
{ Portions created by Microsoft are                                }
{ Copyright (C) 1995-1999 Microsoft Corporation.                   }
{ All Rights Reserved.                                             }
{                                                                  }
{ The original file is: lmbrowsr.h, released 27 Jan 1999.          }
{ The original Pascal code is: LmBrowsr.pas, released 17 Jan 2000. }
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

unit LmBrowsr;

{$I LANMAN.INC}

{$ALIGN ON}
{$MINENUMSIZE 4}
{$WEAKPACKAGEUNIT}

interface

uses
  Windows, LmCons;

(*$HPPEMIT '#include <netcons.h>'*)
(*$HPPEMIT '#include <lmbrowsr.h>'*)

type
  PBrowserStatistics = ^TBrowserStatistics;
  _BROWSER_STATISTICS = record
    StatisticsStartTime: LARGE_INTEGER;
    NumberOfServerAnnouncements: LARGE_INTEGER;
    NumberOfDomainAnnouncements: LARGE_INTEGER;
    NumberOfElectionPackets: ULONG;
    NumberOfMailslotWrites: ULONG;
    NumberOfGetBrowserServerListRequests: ULONG;
    NumberOfServerEnumerations: ULONG;
    NumberOfDomainEnumerations: ULONG;
    NumberOfOtherEnumerations: ULONG;
    NumberOfMissedServerAnnouncements: ULONG;
    NumberOfMissedMailslotDatagrams: ULONG;
    NumberOfMissedGetBrowserServerListRequests: ULONG;
    NumberOfFailedServerAnnounceAllocations: ULONG;
    NumberOfFailedMailslotAllocations: ULONG;
    NumberOfFailedMailslotReceives: ULONG;
    NumberOfFailedMailslotWrites: ULONG;
    NumberOfFailedMailslotOpens: ULONG;
    NumberOfDuplicateMasterAnnouncements: ULONG;
    NumberOfIllegalDatagrams: LARGE_INTEGER;
  end;
  {$EXTERNALSYM _BROWSER_STATISTICS}
  TBrowserStatistics = _BROWSER_STATISTICS;
  BROWSER_STATISTICS = _BROWSER_STATISTICS;
  {$EXTERNALSYM BROWSER_STATISTICS}

  PBrowserStatistics100 = ^TBrowserStatistics100;
  _BROWSER_STATISTICS_100 = record
    StartTime: LARGE_INTEGER;
    NumberOfServerAnnouncements: LARGE_INTEGER;
    NumberOfDomainAnnouncements: LARGE_INTEGER;
    NumberOfElectionPackets: ULONG;
    NumberOfMailslotWrites: ULONG;
    NumberOfGetBrowserServerListRequests: ULONG;
    NumberOfIllegalDatagrams: LARGE_INTEGER;
  end;
  {$EXTERNALSYM _BROWSER_STATISTICS_100}
  TBrowserStatistics100 = _BROWSER_STATISTICS_100;
  BROWSER_STATISTICS_100 = _BROWSER_STATISTICS_100;
  {$EXTERNALSYM BROWSER_STATISTICS_100}

  PBrowserStatistics101 = ^TBrowserStatistics101;
  _BROWSER_STATISTICS_101 = record
    StartTime: LARGE_INTEGER;
    NumberOfServerAnnouncements: LARGE_INTEGER;
    NumberOfDomainAnnouncements: LARGE_INTEGER;
    NumberOfElectionPackets: ULONG;
    NumberOfMailslotWrites: ULONG;
    NumberOfGetBrowserServerListRequests: ULONG;
    NumberOfIllegalDatagrams: LARGE_INTEGER;
    NumberOfMissedServerAnnouncements: ULONG;
    NumberOfMissedMailslotDatagrams: ULONG;
    NumberOfMissedGetBrowserServerListRequests: ULONG;
    NumberOfFailedServerAnnounceAllocations: ULONG;
    NumberOfFailedMailslotAllocations: ULONG;
    NumberOfFailedMailslotReceives: ULONG;
    NumberOfFailedMailslotWrites: ULONG;
    NumberOfFailedMailslotOpens: ULONG;
    NumberOfDuplicateMasterAnnouncements: ULONG;
  end;
  {$EXTERNALSYM _BROWSER_STATISTICS_101}
  TBrowserStatistics101 = _BROWSER_STATISTICS_101;
  BROWSER_STATISTICS_101 = _BROWSER_STATISTICS_101;
  {$EXTERNALSYM BROWSER_STATISTICS_101}

  PBrowserEmulatedDomain = ^TBrowserEmulatedDomain;
  _BROWSER_EMULATED_DOMAIN = record
    DomainName: LPWSTR;
    EmulatedServerName: LPWSTR;
    Role: DWORD;
  end;
  {$EXTERNALSYM _BROWSER_EMULATED_DOMAIN}
  TBrowserEmulatedDomain = _BROWSER_EMULATED_DOMAIN;
  BROWSER_EMULATED_DOMAIN = _BROWSER_EMULATED_DOMAIN;
  {$EXTERNALSYM BROWSER_EMULATED_DOMAIN}

// Function Prototypes - BROWSER

function I_BrowserServerEnum(servername, transport, clientname: LPCWSTR;
  level: DWORD; bufptr: Pointer; prefmaxlen: DWORD; var entriesread: DWORD;
  var totalentries: DWORD; servertype: DWORD; domain: LPCWSTR;
  resume_handle: PDWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM I_BrowserServerEnum}

function I_BrowserServerEnumEx(servername, transport, clientname: LPCWSTR;
  level: DWORD; bufptr: Pointer; prefmaxlen: DWORD; var entriesread: DWORD;
  var totalentries: DWORD; servertype: DWORD; domain: LPCWSTR;
  FirstNameToReturn: LPCWSTR): NET_API_STATUS; stdcall;
{$EXTERNALSYM I_BrowserServerEnumEx}

function I_BrowserQueryOtherDomains(servername: LPCWSTR; bufptr: Pointer;
  var entriesread: DWORD; var totalentries: DWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM I_BrowserQueryOtherDomains}

function I_BrowserResetNetlogonState(servername: LPCWSTR): NET_API_STATUS; stdcall;
{$EXTERNALSYM I_BrowserResetNetlogonState}

function I_BrowserSetNetlogonState(ServerName, DomainName, EmulatedServerName: LPWSTR;
  Role: DWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM I_BrowserSetNetlogonState}

const
  BROWSER_ROLE_PDC = $1;
  {$EXTERNALSYM BROWSER_ROLE_PDC}
  BROWSER_ROLE_BDC = $2;
  {$EXTERNALSYM BROWSER_ROLE_BDC}

function I_BrowserQueryEmulatedDomains(ServerName: LPWSTR;
  var EmulatedDomains: TBrowserEmulatedDomain;
  var EntriesRead: DWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM I_BrowserQueryEmulatedDomains}

function I_BrowserQueryStatistics(servername: LPCWSTR;
  var statistics: TBrowserStatistics): NET_API_STATUS; stdcall;
{$EXTERNALSYM I_BrowserQueryStatistics}

function I_BrowserResetStatistics(servername: LPCWSTR): NET_API_STATUS; stdcall;
{$EXTERNALSYM I_BrowserResetStatistics}

function I_BrowserServerEnumForXactsrv(TransportName, ClientName: LPCWSTR;
  NtLevel: ULONG; ClientLevel: Word; Buffer: Pointer; BufferLength: WORD;
  PreferedMaximumLength: DWORD; var EntriesRead: DWORD; var TotalEntries: DWORD;
  ServerType: DWORD; Domain: LPCWSTR; FirstNameToReturn: LPCWSTR;
  Converter: PWord): Word; stdcall;
{$EXTERNALSYM I_BrowserServerEnumForXactsrv}

function I_BrowserDebugTrace(Server: PWideChar; Buffer: PChar): NET_API_STATUS; stdcall;
{$EXTERNALSYM I_BrowserDebugTrace}

implementation

function I_BrowserServerEnum; external netapi32lib name 'I_BrowserServerEnum';
function I_BrowserServerEnumEx; external netapi32lib name 'I_BrowserServerEnumEx';
function I_BrowserQueryOtherDomains; external netapi32lib name 'I_BrowserQueryOtherDomains';
function I_BrowserResetNetlogonState; external netapi32lib name 'I_BrowserResetNetlogonState';
function I_BrowserSetNetlogonState; external netapi32lib name 'I_BrowserSetNetlogonState';
function I_BrowserQueryEmulatedDomains; external netapi32lib name 'I_BrowserQueryEmulatedDomains';
function I_BrowserQueryStatistics; external netapi32lib name 'I_BrowserQueryStatistics';
function I_BrowserResetStatistics; external netapi32lib name 'I_BrowserResetStatistics';
function I_BrowserServerEnumForXactsrv; external netapi32lib name 'I_BrowserServerEnumForXactsrv';
function I_BrowserDebugTrace; external netapi32lib name 'I_BrowserDebugTrace';

end.
