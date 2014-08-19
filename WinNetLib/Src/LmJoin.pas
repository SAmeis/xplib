{******************************************************************}
{                                                                  }
{ Borland Delphi Runtime Library                                   }
{ LanManager share functions for Windows NT interface unit         }
{                                                                  }
{ Portions created by Microsoft are                                }
{ Copyright (C) 1995-1999 Microsoft Corporation.                   }
{ All Rights Reserved.                                             }
{                                                                  }
{ The original file is: lmjoin.h, released 14 Nov 1998.            }
{ The original Pascal code is: LmJoin.pas, released 13 Jan 2000.   }
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

unit LmJoin;

{$I LANMAN.INC}

{$ALIGN ON}
{$MINENUMSIZE 4}
{$WEAKPACKAGEUNIT}

interface

uses
  Windows, LmCons;

(*$HPPEMIT '#include <netsetup.h>'*)

// Types of name that can be validated

type
  PNetSetupNameType = ^TNetSetupNameType;
  _NETSETUP_NAME_TYPE = DWORD;
  {$EXTERNALSYM _NETSETUP_NAME_TYPE}
  TNetSetupNameType = _NETSETUP_NAME_TYPE;
  NETSETUP_NAME_TYPE = _NETSETUP_NAME_TYPE;
  {$EXTERNALSYM NETSETUP_NAME_TYPE}

const
  NetSetupUnknown = 0;
  NetSetupMachine = 1;
  NetSetupWorkgroup = 2;
  NetSetupDomain = 3;
  NetSetupNonExistentDomain = 4;
  NetSetupDnsMachine = 5;

// Status of a workstation

type
  PNetSetupJoinStatus = ^TNetSetupJoinStatus;
  _NETSETUP_JOIN_STATUS = DWORD;
  {$EXTERNALSYM _NETSETUP_JOIN_STATUS}
  TNetSetupJoinStatus = _NETSETUP_JOIN_STATUS;
  NETSETUP_JOIN_STATUS = _NETSETUP_JOIN_STATUS;
  {$EXTERNALSYM NETSETUP_JOIN_STATUS}

const
  NetSetupUnknownStatus = 0;
  NetSetupUnjoined = 1;
  NetSetupWorkgroupName = 2;
  NetSetupDomainName = 3;

// Flags to determine the behavior of the join/unjoin APIs

  NETSETUP_JOIN_DOMAIN    = $00000001;      // If not present, workgroup is joined
  {$EXTERNALSYM NETSETUP_JOIN_DOMAIN}
  NETSETUP_ACCT_CREATE    = $00000002;      // Do the server side account creation/rename
  {$EXTERNALSYM NETSETUP_ACCT_CREATE}
  NETSETUP_ACCT_DELETE    = $00000004;      // Delete the account when a domain is left
  {$EXTERNALSYM NETSETUP_ACCT_DELETE}
  NETSETUP_WIN9X_UPGRADE  = $00000010;      // Invoked during upgrade of Windows 9x to
                                            // Windows NT
  {$EXTERNALSYM NETSETUP_WIN9X_UPGRADE}
  NETSETUP_DOMAIN_JOIN_IF_JOINED  = $00000020;  // Allow the client to join a new domain
                                                // even if it is already joined to a domain
  {$EXTERNALSYM NETSETUP_DOMAIN_JOIN_IF_JOINED}
  NETSETUP_JOIN_UNSECURE  = $00000040;      // Performs an unsecure join
  {$EXTERNALSYM NETSETUP_JOIN_UNSECURE}

  NETSETUP_INSTALL_INVOCATION = $00040000;  // The APIs were invoked during install
  {$EXTERNALSYM NETSETUP_INSTALL_INVOCATION}

// 0x80000000 is reserved for internal use only

// Joins a machine to the domain.

function NetJoinDomain(lpServer, lpDomain, lpAccountOU, lpAccount,
  lpPassword: LPCWSTR; fJoinOptions: DWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetJoinDomain}

function NetUnjoinDomain(lpServer, lpAccount, lpPassword: LPCWSTR;
  fUnjoinOptions: DWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetUnjoinDomain}

function NetRenameMachineInDomain(lpServer, lpNewMachineName, lpAccount,
  lpPassword: LPCWSTR; fRenameOptions: DWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetRenameMachineInDomain}

// Determine the validity of a name

function NetValidateName(lpServer, lpName, lpAccount, lpPassword: LPCWSTR;
  NameType: TNetSetupNameType): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetValidateName}

// Determines whether a workstation is joined to a domain or not

function NetGetJoinInformation(lpServer: LPCWSTR; lpNameBuffer: LPWSTR;
  var BufferType: TNetSetupNameType): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetGetJoinInformation}

// Determines the list of OUs that the client can create a machine account in

function NetGetJoinableOUs(lpServer, lpDomain, lpAccount, lpPassword: LPCWSTR;
  var OUCount: DWORD; OUs: Pointer): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetGetJoinableOUs}

implementation

function NetJoinDomain; external netapi32lib name 'NetJoinDomain';
function NetUnjoinDomain; external netapi32lib name 'NetUnjoinDomain';
function NetRenameMachineInDomain; external netapi32lib name 'NetRenameMachineInDomain';
function NetValidateName; external netapi32lib name 'NetValidateName';
function NetGetJoinInformation; external netapi32lib name 'NetGetJoinInformation';
function NetGetJoinableOUs; external netapi32lib name 'NetGetJoinableOUs';

end.
