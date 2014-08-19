{******************************************************************}
{                                                                  }
{ Borland Delphi Runtime Library                                   }
{ LanManager share functions for Windows NT interface unit         }
{                                                                  }
{ Portions created by Microsoft are                                }
{ Copyright (C) 1995-1999 Microsoft Corporation.                   }
{ All Rights Reserved.                                             }
{                                                                  }
{ The original file is: lmconfig.h, released 14 Nov 1998.          }
{ The original Pascal code is: LmConfig.pas, released 11 Jan 2000. }
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

unit LmConfig;

{$I LANMAN.INC}

{$ALIGN ON}
{$MINENUMSIZE 4}
{$WEAKPACKAGEUNIT}

interface

uses
  Windows, LmCons;

(*$HPPEMIT '#include <netcons.h>'*)
(*$HPPEMIT '#include <lmconfig.h>'*)

function NetConfigGet(server: LPCWSTR; component: LPCWSTR; parameter: LPCWSTR;
  bufptr: Pointer; var totalavailable: DWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetConfigGet}

function NetConfigGetAll(server: LPCWSTR; component: LPCWSTR; bufptr: Pointer;
  var totalavailable: DWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetConfigGetAll}

function NetConfigSet(server: LPCWSTR; reserved1: LPCWSTR; component: LPCWSTR;
  level: DWORD; reserved2: DWORD; buf: Pointer; reserved3: DWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetConfigSet}

function NetRegisterDomainNameChangeNotification(NotificationEventHandle: PHandle): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetRegisterDomainNameChangeNotification}

function NetUnregisterDomainNameChangeNotification(NotificationEventHandle: THandle): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetUnregisterDomainNameChangeNotification}

// Data Structures - Config

type
  PConfigInfo0 = ^TConfigInfo0;
  _CONFIG_INFO_0 = record
    cfgi0_key: LPWSTR;
    cfgi0_data: LPWSTR;
  end;
  {$EXTERNALSYM _CONFIG_INFO_0}
  TConfigInfo0 = _CONFIG_INFO_0;
  CONFIG_INFO_0 = _CONFIG_INFO_0;
  {$EXTERNALSYM CONFIG_INFO_0}

implementation

function NetConfigGet; external netapi32lib name 'NetConfigGet';
function NetConfigGetAll; external netapi32lib name 'NetConfigGetAll';
function NetConfigSet; external netapi32lib name 'NetConfigSet';
function NetRegisterDomainNameChangeNotification; external netapi32lib name 'NetRegisterDomainNameChangeNotification';
function NetUnregisterDomainNameChangeNotification; external netapi32lib name 'NetUnregisterDomainNameChangeNotification';

end.
