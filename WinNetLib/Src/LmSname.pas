{******************************************************************}
{                                                                  }
{ Borland Delphi Runtime Library                                   }
{ LanManager share functions for Windows NT interface unit         }
{                                                                  }
{ Portions created by Microsoft are                                }
{ Copyright (C) 1995-1999 Microsoft Corporation.                   }
{ All Rights Reserved.                                             }
{                                                                  }
{ The original file is: lmsname.h, released 14 Nov 1998.           }
{ The original Pascal code is: LmSname.pas, released 13 Jan 2000.  }
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

unit LmSname;

{$I LANMAN.INC}

{$MINENUMSIZE 4}
{$WEAKPACKAGEUNIT}

interface

uses
  Windows;

(*$HPPEMIT '#include <lmsname.h>'*)

//  Standard LAN Manager service names.

const
  SERVICE_WORKSTATION       = 'LanmanWorkstation';
  {$EXTERNALSYM SERVICE_WORKSTATION}
  SERVICE_LM20_WORKSTATION  = 'WORKSTATION';
  {$EXTERNALSYM SERVICE_LM20_WORKSTATION}
  WORKSTATION_DISPLAY_NAME  = 'Workstation';
  {$EXTERNALSYM WORKSTATION_DISPLAY_NAME}

  SERVICE_SERVER            = 'LanmanServer';
  {$EXTERNALSYM SERVICE_SERVER}
  SERVICE_LM20_SERVER       = 'SERVER';
  {$EXTERNALSYM SERVICE_LM20_SERVER}
  SERVER_DISPLAY_NAME       = 'Server';
  {$EXTERNALSYM SERVER_DISPLAY_NAME}

  SERVICE_BROWSER           = 'BROWSER';
  {$EXTERNALSYM SERVICE_BROWSER}
  SERVICE_LM20_BROWSER      = SERVICE_BROWSER;
  {$EXTERNALSYM SERVICE_LM20_BROWSER}

  SERVICE_MESSENGER         = 'MESSENGER';
  {$EXTERNALSYM SERVICE_MESSENGER}
  SERVICE_LM20_MESSENGER    = SERVICE_MESSENGER;
  {$EXTERNALSYM SERVICE_LM20_MESSENGER}

  SERVICE_NETRUN            = 'NETRUN';
  {$EXTERNALSYM SERVICE_NETRUN}
  SERVICE_LM20_NETRUN       = SERVICE_NETRUN;
  {$EXTERNALSYM SERVICE_LM20_NETRUN}

  SERVICE_SPOOLER           = 'SPOOLER';
  {$EXTERNALSYM SERVICE_SPOOLER}
  SERVICE_LM20_SPOOLER      = SERVICE_SPOOLER;
  {$EXTERNALSYM SERVICE_LM20_SPOOLER}

  SERVICE_ALERTER           = 'ALERTER';
  {$EXTERNALSYM SERVICE_ALERTER}
  SERVICE_LM20_ALERTER      = SERVICE_ALERTER;
  {$EXTERNALSYM SERVICE_LM20_ALERTER}

  SERVICE_NETLOGON          = 'NETLOGON';
  {$EXTERNALSYM SERVICE_NETLOGON}
  SERVICE_LM20_NETLOGON     = SERVICE_NETLOGON;
  {$EXTERNALSYM SERVICE_LM20_NETLOGON}

  SERVICE_NETPOPUP          = 'NETPOPUP';
  {$EXTERNALSYM SERVICE_NETPOPUP}
  SERVICE_LM20_NETPOPUP     = SERVICE_NETPOPUP;
  {$EXTERNALSYM SERVICE_LM20_NETPOPUP}

  SERVICE_SQLSERVER         = 'SQLSERVER';
  {$EXTERNALSYM SERVICE_SQLSERVER}
  SERVICE_LM20_SQLSERVER    = SERVICE_SQLSERVER;
  {$EXTERNALSYM SERVICE_LM20_SQLSERVER}

  SERVICE_REPL              = 'REPLICATOR';
  {$EXTERNALSYM SERVICE_REPL}
  SERVICE_LM20_REPL         = SERVICE_REPL;
  {$EXTERNALSYM SERVICE_LM20_REPL}

  SERVICE_RIPL              = 'REMOTEBOOT';
  {$EXTERNALSYM SERVICE_RIPL}
  SERVICE_LM20_RIPL         = SERVICE_RIPL;
  {$EXTERNALSYM SERVICE_LM20_RIPL}

  SERVICE_TIMESOURCE        = 'TIMESOURCE';
  {$EXTERNALSYM SERVICE_TIMESOURCE}
  SERVICE_LM20_TIMESOURCE   = SERVICE_TIMESOURCE;
  {$EXTERNALSYM SERVICE_LM20_TIMESOURCE}

  SERVICE_AFP               = 'AFP';
  {$EXTERNALSYM SERVICE_AFP}
  SERVICE_LM20_AFP          = SERVICE_AFP;
  {$EXTERNALSYM SERVICE_LM20_AFP}

  SERVICE_UPS               = 'UPS';
  {$EXTERNALSYM SERVICE_UPS}
  SERVICE_LM20_UPS          = SERVICE_UPS;
  {$EXTERNALSYM SERVICE_LM20_UPS}

  SERVICE_XACTSRV           = 'XACTSRV';
  {$EXTERNALSYM SERVICE_XACTSRV}
  SERVICE_LM20_XACTSRV      = SERVICE_XACTSRV;
  {$EXTERNALSYM SERVICE_LM20_XACTSRV}

  SERVICE_TCPIP             = 'TCPIP';
  {$EXTERNALSYM SERVICE_TCPIP}
  SERVICE_LM20_TCPIP        = SERVICE_TCPIP;
  {$EXTERNALSYM SERVICE_LM20_TCPIP}

  SERVICE_NBT               = 'NBT';
  {$EXTERNALSYM SERVICE_NBT}
  SERVICE_LM20_NBT          = SERVICE_NBT;
  {$EXTERNALSYM SERVICE_LM20_NBT}

  SERVICE_LMHOSTS           = 'LMHOSTS';
  {$EXTERNALSYM SERVICE_LMHOSTS}
  SERVICE_LM20_LMHOSTS      = SERVICE_LMHOSTS;
  {$EXTERNALSYM SERVICE_LM20_LMHOSTS}

  SERVICE_TELNET            = 'Telnet';
  {$EXTERNALSYM SERVICE_TELNET}
  SERVICE_LM20_TELNET       = SERVICE_TELNET;
  {$EXTERNALSYM SERVICE_LM20_TELNET}

  SERVICE_SCHEDULE          = 'Schedule';
  {$EXTERNALSYM SERVICE_SCHEDULE}
  SERVICE_LM20_SCHEDULE     = SERVICE_SCHEDULE;
  {$EXTERNALSYM SERVICE_LM20_SCHEDULE}

  SERVICE_NTLMSSP           = 'NtLmSsp';
  {$EXTERNALSYM SERVICE_NTLMSSP}

  SERVICE_DHCP              = 'DHCP';
  {$EXTERNALSYM SERVICE_DHCP}
  SERVICE_LM20_DHCP         = SERVICE_DHCP;
  {$EXTERNALSYM SERVICE_LM20_DHCP}

  SERVICE_NWSAP             = 'NwSapAgent';
  {$EXTERNALSYM SERVICE_NWSAP}
  SERVICE_LM20_NWSAP        = SERVICE_NWSAP;
  {$EXTERNALSYM SERVICE_LM20_NWSAP}
  NWSAP_DISPLAY_NAME        = 'NW Sap Agent';
  {$EXTERNALSYM NWSAP_DISPLAY_NAME}

  SERVICE_NWCS              = 'NWCWorkstation';
  {$EXTERNALSYM SERVICE_NWCS}
  SERVICE_DNS_CACHE         = 'DnsCache';
  {$EXTERNALSYM SERVICE_DNS_CACHE}

  SERVICE_W32TIME           = 'w32time';
  {$EXTERNALSYM SERVICE_W32TIME}
  SERVCE_LM20_W32TIME       = SERVICE_W32TIME;
  {$EXTERNALSYM SERVCE_LM20_W32TIME}

  SERVICE_KDC               = 'kdc';
  {$EXTERNALSYM SERVICE_KDC}
  SERVICE_LM20_KDC          = SERVICE_KDC;
  {$EXTERNALSYM SERVICE_LM20_KDC}

  SERVICE_RPCLOCATOR        = 'RPCLOCATOR';
  {$EXTERNALSYM SERVICE_RPCLOCATOR}
  SERVICE_LM20_RPCLOCATOR   = SERVICE_RPCLOCATOR;
  {$EXTERNALSYM SERVICE_LM20_RPCLOCATOR}

  SERVICE_TRKSVR            = 'TrkSvr';
  {$EXTERNALSYM SERVICE_TRKSVR}
  SERVICE_LM20_TRKSVR       = SERVICE_TRKSVR;
  {$EXTERNALSYM SERVICE_LM20_TRKSVR}

  SERVICE_TRKWKS            = 'TrkWks';
  {$EXTERNALSYM SERVICE_TRKWKS}
  SERVICE_LM20_TRKWKS       = SERVICE_TRKWKS;
  {$EXTERNALSYM SERVICE_LM20_TRKWKS}

  SERVICE_NTFRS             = 'NtFrs';
  {$EXTERNALSYM SERVICE_NTFRS}
  SERVICE_LM20_NTFRS        = SERVICE_NTFRS;
  {$EXTERNALSYM SERVICE_LM20_NTFRS}

  SERVICE_ISMSERV           = 'IsmServ';
  {$EXTERNALSYM SERVICE_ISMSERV}
  SERVICE_LM20_ISMSERV      = SERVICE_ISMSERV;
  {$EXTERNALSYM SERVICE_LM20_ISMSERV}

implementation

end.
