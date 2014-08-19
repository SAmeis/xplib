{******************************************************************}
{                                                                  }
{ Borland Delphi Runtime Library                                   }
{ LanManager share functions for Windows NT interface unit         }
{                                                                  }
{ Portions created by Microsoft are                                }
{ Copyright (C) 1995-1999 Microsoft Corporation.                   }
{ All Rights Reserved.                                             }
{                                                                  }
{ The original file is: lmserver.h, released 24 Nov 1999.          }
{ The original Pascal code is: LmServer.pas, released 14 Jan 2000. }
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

unit LmServer;

{$I LANMAN.INC}

{$ALIGN ON}
{$MINENUMSIZE 4}
{$WEAKPACKAGEUNIT}

interface

uses
  Windows, LmCons, WinSvc;

(*$HPPEMIT '#include <netcons.h>'*)
(*$HPPEMIT '#include <lmcons.h>'*)
(*$HPPEMIT '#include <winsvc.h>'*)
(*$HPPEMIT '#include <lmserver.h>'*)

// Function Prototypes - SERVER

function NetServerEnum(servername: LPCWSTR; level: DWORD; var bufptr: Pointer;
  prefmaxlen: DWORD; var entriesread: DWORD; var totalentries: DWORD;
  servertype: DWORD; domain: LPCWSTR; resume_handle: PDWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetServerEnum}

function NetServerEnumEx(ServerName: LPCWSTR; Level: DWORD; var Bufptr: Pointer;
  PrefMaxlen: DWORD; var EntriesRead: DWORD; var totalentries: DWORD;
  servertype: DWORD; domain, FirstNameToReturn: LPCWSTR): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetServerEnumEx}

function NetServerGetInfo(servername: LPWSTR; level: DWORD;
  var bufptr: Pointer): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetServerGetInfo}

function NetServerSetInfo(servername: LPWSTR; level: DWORD; buf: Pointer;
  ParmError: PDWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetServerSetInfo}

// Temporary hack function.

function NetServerSetInfoCommandLine(argc: Word; argv: LPWSTR): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetServerSetInfoCommandLine}

function NetServerDiskEnum(servername: LPWSTR; level: DWORD; bufptr: Pointer;
  prefmaxlen: DWORD; var entriesread: DWORD; var totalentries: DWORD;
  resume_handle: PDWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetServerDiskEnum}

function NetServerComputerNameAdd(ServerName: LPWSTR; EmulatedDomainName: LPWSTR;
  EmulatedServerName: LPWSTR): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetServerComputerNameAdd}

function NetServerComputerNameDel(ServerName, EmulatedServerName: LPWSTR): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetServerComputerNameDel}

function NetServerTransportAdd(servername: LPWSTR; level: DWORD;
  bufptr: Pointer): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetServerTransportAdd}

function NetServerTransportAddEx(servername: LPWSTR; level: DWORD;
  bufptr: Pointer): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetServerTransportAddEx}

function NetServerTransportDel(servername: LPWSTR; level: DWORD;
  bufptr: Pointer): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetServerTransportDel}

function NetServerTransportEnum(servername: LPWSTR; level: DWORD; bufptr: Pointer;
  prefmaxlen: DWORD; var entriesread: DWORD; var totalentries: DWORD;
  resumehandle: PDWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetServerTransportEnum}

// The following function can be called by Win NT services to register
// their service type.  This function is exported from advapi32.dll.
// Therefore, if this is the only function called by that service, then
// it is not necessary to link to netapi32.lib.

function SetServiceBits(hServiceStatus: SERVICE_STATUS_HANDLE;
  dwServiceBits: DWORD; bSetBitsOn: BOOL; bUpdateImmediately: BOOL): BOOL; stdcall;
{$EXTERNALSYM SetServiceBits}

// Data Structures - SERVER

type
  PServerInfo100 = ^TServerInfo100;
  _SERVER_INFO_100 = record
    sv100_platform_id: DWORD;
    sv100_name: LPWSTR;
  end;
  {$EXTERNALSYM _SERVER_INFO_100}
  TServerInfo100 = _SERVER_INFO_100;
  SERVER_INFO_100 = _SERVER_INFO_100;
  {$EXTERNALSYM SERVER_INFO_100}

  PServerInfo101 = ^TServerInfo101;
  _SERVER_INFO_101 = record
    sv101_platform_id: DWORD;
    sv101_name: LPWSTR;
    sv101_version_major: DWORD;
    sv101_version_minor: DWORD;
    sv101_type: DWORD;
    sv101_comment: LPWSTR;
  end;
  {$EXTERNALSYM _SERVER_INFO_101}
  TServerInfo101 = _SERVER_INFO_101;
  SERVER_INFO_101 = _SERVER_INFO_101;
  {$EXTERNALSYM SERVER_INFO_101}

  PServerInfo102 = ^TServerInfo102;
  _SERVER_INFO_102 = record
     sv102_platform_id: DWORD;
     sv102_name: LPWSTR;
     sv102_version_major: DWORD;
     sv102_version_minor: DWORD;
     sv102_type: DWORD;
     sv102_comment: LPWSTR;
     sv102_users: DWORD;
     sv102_disc: LongInt;
     sv102_hidden: BOOL;
     sv102_announce: DWORD;
     sv102_anndelta: DWORD;
     sv102_licenses: DWORD;
     sv102_userpath: LPWSTR;
  end;
  {$EXTERNALSYM _SERVER_INFO_102}
  TServerInfo102 = _SERVER_INFO_102;
  SERVER_INFO_102 = _SERVER_INFO_102;
  {$EXTERNALSYM SERVER_INFO_102}

  PServerInfo402 = ^TServerInfo402;
  _SERVER_INFO_402 = record
     sv402_ulist_mtime: DWORD;
     sv402_glist_mtime: DWORD;
     sv402_alist_mtime: DWORD;
     sv402_alerts: LPWSTR;
     sv402_security: DWORD;
     sv402_numadmin: DWORD;
     sv402_lanmask: DWORD;
     sv402_guestacct: LPWSTR;
     sv402_chdevs: DWORD;
     sv402_chdevq: DWORD;
     sv402_chdevjobs: DWORD;
     sv402_connections: DWORD;
     sv402_shares: DWORD;
     sv402_openfiles: DWORD;
     sv402_sessopens: DWORD;
     sv402_sessvcs: DWORD;
     sv402_sessreqs: DWORD;
     sv402_opensearch: DWORD;
     sv402_activelocks: DWORD;
     sv402_numreqbuf: DWORD;
     sv402_sizreqbuf: DWORD;
     sv402_numbigbuf: DWORD;
     sv402_numfiletasks: DWORD;
     sv402_alertsched: DWORD;
     sv402_erroralert: DWORD;
     sv402_logonalert: DWORD;
     sv402_accessalert: DWORD;
     sv402_diskalert: DWORD;
     sv402_netioalert: DWORD;
     sv402_maxauditsz: DWORD;
     sv402_srvheuristics: LPWSTR;
  end;
  {$EXTERNALSYM _SERVER_INFO_402}
  TServerInfo402 = _SERVER_INFO_402;
  SERVER_INFO_402 = _SERVER_INFO_402;
  {$EXTERNALSYM SERVER_INFO_402}

  PServerInfo403 = ^TServerInfo403;
  _SERVER_INFO_403 = record
     sv403_ulist_mtime: DWORD;
     sv403_glist_mtime: DWORD;
     sv403_alist_mtime: DWORD;
     sv403_alerts: LPWSTR;
     sv403_security: DWORD;
     sv403_numadmin: DWORD;
     sv403_lanmask: DWORD;
     sv403_guestacct: LPWSTR;
     sv403_chdevs: DWORD;
     sv403_chdevq: DWORD;
     sv403_chdevjobs: DWORD;
     sv403_connections: DWORD;
     sv403_shares: DWORD;
     sv403_openfiles: DWORD;
     sv403_sessopens: DWORD;
     sv403_sessvcs: DWORD;
     sv403_sessreqs: DWORD;
     sv403_opensearch: DWORD;
     sv403_activelocks: DWORD;
     sv403_numreqbuf: DWORD;
     sv403_sizreqbuf: DWORD;
     sv403_numbigbuf: DWORD;
     sv403_numfiletasks: DWORD;
     sv403_alertsched: DWORD;
     sv403_erroralert: DWORD;
     sv403_logonalert: DWORD;
     sv403_accessalert: DWORD;
     sv403_diskalert: DWORD;
     sv403_netioalert: DWORD;
     sv403_maxauditsz: DWORD;
     sv403_srvheuristics: LPWSTR;
     sv403_auditedevents: DWORD;
     sv403_autoprofile: DWORD;
     sv403_autopath: LPWSTR;
  end;
  {$EXTERNALSYM _SERVER_INFO_403}
  TServerInfo403 = _SERVER_INFO_403;
  SERVER_INFO_403 = _SERVER_INFO_403;
  {$EXTERNALSYM SERVER_INFO_403}

  PServerInfo502 = ^TServerInfo502;
  _SERVER_INFO_502 = record
    sv502_sessopens: DWORD;
    sv502_sessvcs: DWORD;
    sv502_opensearch: DWORD;
    sv502_sizreqbuf: DWORD;
    sv502_initworkitems: DWORD;
    sv502_maxworkitems: DWORD;
    sv502_rawworkitems: DWORD;
    sv502_irpstacksize: DWORD;
    sv502_maxrawbuflen: DWORD;
    sv502_sessusers: DWORD;
    sv502_sessconns: DWORD;
    sv502_maxpagedmemoryusage: DWORD;
    sv502_maxnonpagedmemoryusage: DWORD;
    sv502_enablesoftcompat: BOOL;
    sv502_enableforcedlogoff: BOOL;
    sv502_timesource: BOOL;
    sv502_acceptdownlevelapis: BOOL;
    sv502_lmannounce: BOOL;
  end;
  {$EXTERNALSYM _SERVER_INFO_502}
  TServerInfo502 = _SERVER_INFO_502;
  SERVER_INFO_502 = _SERVER_INFO_502;
  {$EXTERNALSYM SERVER_INFO_502}

  PServerInfo503 = ^TServerInfo503;
  _SERVER_INFO_503 = record
    sv503_sessopens: DWORD;
    sv503_sessvcs: DWORD;
    sv503_opensearch: DWORD;
    sv503_sizreqbuf: DWORD;
    sv503_initworkitems: DWORD;
    sv503_maxworkitems: DWORD;
    sv503_rawworkitems: DWORD;
    sv503_irpstacksize: DWORD;
    sv503_maxrawbuflen: DWORD;
    sv503_sessusers: DWORD;
    sv503_sessconns: DWORD;
    sv503_maxpagedmemoryusage: DWORD;
    sv503_maxnonpagedmemoryusage: DWORD;
    sv503_enablesoftcompat: BOOL;
    sv503_enableforcedlogoff: BOOL;
    sv503_timesource: BOOL;
    sv503_acceptdownlevelapis: BOOL;
    sv503_lmannounce: BOOL;
    sv503_domain: LPWSTR;
    sv503_maxcopyreadlen: DWORD;
    sv503_maxcopywritelen: DWORD;
    sv503_minkeepsearch: DWORD;
    sv503_maxkeepsearch: DWORD;
    sv503_minkeepcomplsearch: DWORD;
    sv503_maxkeepcomplsearch: DWORD;
    sv503_threadcountadd: DWORD;
    sv503_numblockthreads: DWORD;
    sv503_scavtimeout: DWORD;
    sv503_minrcvqueue: DWORD;
    sv503_minfreeworkitems: DWORD;
    sv503_xactmemsize: DWORD;
    sv503_threadpriority: DWORD;
    sv503_maxmpxct: DWORD;
    sv503_oplockbreakwait: DWORD;
    sv503_oplockbreakresponsewait: DWORD;
    sv503_enableoplocks: BOOL;
    sv503_enableoplockforceclose: BOOL;
    sv503_enablefcbopens: BOOL;
    sv503_enableraw: BOOL;
    sv503_enablesharednetdrives: BOOL;
    sv503_minfreeconnections: DWORD;
    sv503_maxfreeconnections: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_503}
  TServerInfo503 = _SERVER_INFO_503;
  SERVER_INFO_503 = _SERVER_INFO_503;
  {$EXTERNALSYM SERVER_INFO_503}

  PServerInfo599 = ^TServerInfo599;
  _SERVER_INFO_599 = record
    sv599_sessopens: DWORD;
    sv599_sessvcs: DWORD;
    sv599_opensearch: DWORD;
    sv599_sizreqbuf: DWORD;
    sv599_initworkitems: DWORD;
    sv599_maxworkitems: DWORD;
    sv599_rawworkitems: DWORD;
    sv599_irpstacksize: DWORD;
    sv599_maxrawbuflen: DWORD;
    sv599_sessusers: DWORD;
    sv599_sessconns: DWORD;
    sv599_maxpagedmemoryusage: DWORD;
    sv599_maxnonpagedmemoryusage: DWORD;
    sv599_enablesoftcompat: BOOL;
    sv599_enableforcedlogoff: BOOL;
    sv599_timesource: BOOL;
    sv599_acceptdownlevelapis: BOOL;
    sv599_lmannounce: BOOL;
    sv599_domain: LPWSTR;
    sv599_maxcopyreadlen: DWORD;
    sv599_maxcopywritelen: DWORD;
    sv599_minkeepsearch: DWORD;
    sv599_maxkeepsearch: DWORD;
    sv599_minkeepcomplsearch: DWORD;
    sv599_maxkeepcomplsearch: DWORD;
    sv599_threadcountadd: DWORD;
    sv599_numblockthreads: DWORD;
    sv599_scavtimeout: DWORD;
    sv599_minrcvqueue: DWORD;
    sv599_minfreeworkitems: DWORD;
    sv599_xactmemsize: DWORD;
    sv599_threadpriority: DWORD;
    sv599_maxmpxct: DWORD;
    sv599_oplockbreakwait: DWORD;
    sv599_oplockbreakresponsewait: DWORD;
    sv599_enableoplocks: BOOL;
    sv599_enableoplockforceclose: BOOL;
    sv599_enablefcbopens: BOOL;
    sv599_enableraw: BOOL;
    sv599_enablesharednetdrives: BOOL;
    sv599_minfreeconnections: DWORD;
    sv599_maxfreeconnections: DWORD;
    sv599_initsesstable: DWORD;
    sv599_initconntable: DWORD;
    sv599_initfiletable: DWORD;
    sv599_initsearchtable: DWORD;
    sv599_alertschedule: DWORD;
    sv599_errorthreshold: DWORD;
    sv599_networkerrorthreshold: DWORD;
    sv599_diskspacethreshold: DWORD;
    sv599_reserved: DWORD;
    sv599_maxlinkdelay: DWORD;
    sv599_minlinkthroughput: DWORD;
    sv599_linkinfovalidtime: DWORD;
    sv599_scavqosinfoupdatetime: DWORD;
    sv599_maxworkitemidletime: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_599}
  TServerInfo599 = _SERVER_INFO_599;
  SERVER_INFO_599 = _SERVER_INFO_599;
  {$EXTERNALSYM SERVER_INFO_599}

  PServerInfo598 = ^TServerInfo598;
  _SERVER_INFO_598 = record
    sv598_maxrawworkitems: DWORD;
    sv598_maxthreadsperqueue: DWORD;
    sv598_producttype: DWORD;
    sv598_serversize: DWORD;
    sv598_connectionlessautodisc: DWORD;
    sv598_sharingviolationretries: DWORD;
    sv598_sharingviolationdelay: DWORD;
    sv598_maxglobalopensearch: DWORD;
    sv598_removeduplicatesearches: DWORD;
    sv598_lockviolationoffset: DWORD;
    sv598_lockviolationdelay: DWORD;
    sv598_mdlreadswitchover: DWORD;
    sv598_cachedopenlimit: DWORD;
    sv598_otherqueueaffinity: DWORD;
    sv598_restrictnullsessaccess: BOOL;
    sv598_enablewfw311directipx: BOOL;
    sv598_queuesamplesecs: DWORD;
    sv598_balancecount: DWORD;
    sv598_preferredaffinity: DWORD;
    sv598_maxfreerfcbs: DWORD;
    sv598_maxfreemfcbs: DWORD;
    sv598_maxfreelfcbs: DWORD;
    sv598_maxfreepagedpoolchunks: DWORD;
    sv598_minpagedpoolchunksize: DWORD;
    sv598_maxpagedpoolchunksize: DWORD;
    sv598_sendsfrompreferredprocessor: BOOL;
    sv598_cacheddirectorylimit: DWORD;
    sv598_maxcopylength: DWORD;
    sv598_enablecompression: BOOL;
    sv598_autosharewks: BOOL;
    sv598_autoshareserver: BOOL;
    sv598_enablesecuritysignature: BOOL;
    sv598_requiresecuritysignature: BOOL;
    sv598_minclientbuffersize: DWORD;
    sv598_serverguid: TGUID;
    sv598_ConnectionNoSessionsTimeout: DWORD;
    sv598_IdleThreadTimeOut: DWORD;
    sv598_enableW9xsecuritysignature: BOOL;
  end;
  {$EXTERNALSYM _SERVER_INFO_598}
  TServerInfo598 = _SERVER_INFO_598;
  SERVER_INFO_598 = _SERVER_INFO_598;
  {$EXTERNALSYM SERVER_INFO_598}

  PServerInfo1005 = ^TServerInfo1005;
  _SERVER_INFO_1005 = record
    sv1005_comment: LPWSTR;
  end;
  {$EXTERNALSYM _SERVER_INFO_1005}
  TServerInfo1005 = _SERVER_INFO_1005;
  SERVER_INFO_1005 = _SERVER_INFO_1005;
  {$EXTERNALSYM SERVER_INFO_1005}

  PServerInfo1107 = ^TServerInfo1107;
  _SERVER_INFO_1107 = record
    sv1107_users: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1107}
  TServerInfo1107 = _SERVER_INFO_1107;
  SERVER_INFO_1107 = _SERVER_INFO_1107;
  {$EXTERNALSYM SERVER_INFO_1107}

  PServerInfo1010 = ^TServerInfo1010;
  _SERVER_INFO_1010 = record
    sv1010_disc: LongInt;
  end;
  {$EXTERNALSYM _SERVER_INFO_1010}
  TServerInfo1010 = _SERVER_INFO_1010;
  SERVER_INFO_1010 = _SERVER_INFO_1010;
  {$EXTERNALSYM SERVER_INFO_1010}

  PServerInfo1016 = ^TServerInfo1016;
  _SERVER_INFO_1016 = record
    sv1016_hidden: BOOL;
  end;
  {$EXTERNALSYM _SERVER_INFO_1016}
  TServerInfo1016 = _SERVER_INFO_1016;
  SERVER_INFO_1016 = _SERVER_INFO_1016;
  {$EXTERNALSYM SERVER_INFO_1016}

  PServerInfo1017 = ^TServerInfo1017;
  _SERVER_INFO_1017 = record
    sv1017_announce: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1017}
  TServerInfo1017 = _SERVER_INFO_1017;
  SERVER_INFO_1017 = _SERVER_INFO_1017;
  {$EXTERNALSYM SERVER_INFO_1017}

  PServerInfo1018 = ^TServerInfo1018;
  _SERVER_INFO_1018 = record
    sv1018_anndelta: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1018}
  TServerInfo1018 = _SERVER_INFO_1018;
  SERVER_INFO_1018 = _SERVER_INFO_1018;
  {$EXTERNALSYM SERVER_INFO_1018}

  PServerInfo1501 = ^TServerInfo1501;
  _SERVER_INFO_1501 = record
    sv1501_sessopens: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1501}
  TServerInfo1501 = _SERVER_INFO_1501;
  SERVER_INFO_1501 = _SERVER_INFO_1501;
  {$EXTERNALSYM SERVER_INFO_1501}

  PServerInfo1502 = ^TServerInfo1502;
  _SERVER_INFO_1502 = record
    sv1502_sessvcs: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1502}
  TServerInfo1502 = _SERVER_INFO_1502;
  SERVER_INFO_1502 = _SERVER_INFO_1502;
  {$EXTERNALSYM SERVER_INFO_1502}

  PServerInfo1503 = ^TServerInfo1503;
  _SERVER_INFO_1503 = record
    sv1503_opensearch: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1503}
  TServerInfo1503 = _SERVER_INFO_1503;
  SERVER_INFO_1503 = _SERVER_INFO_1503;
  {$EXTERNALSYM SERVER_INFO_1503}

  PServerInfo1506 = ^TServerInfo1506;
  _SERVER_INFO_1506 = record
    sv1506_maxworkitems: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1506}
  TServerInfo1506 = _SERVER_INFO_1506;
  SERVER_INFO_1506 = _SERVER_INFO_1506;
  {$EXTERNALSYM SERVER_INFO_1506}

  PServerInfo1509 = ^TServerInfo1509;
  _SERVER_INFO_1509 = record
    sv1509_maxrawbuflen: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1509}
  TServerInfo1509 = _SERVER_INFO_1509;
  SERVER_INFO_1509 = _SERVER_INFO_1509;
  {$EXTERNALSYM SERVER_INFO_1509}

  PServerInfo1510 = ^TServerInfo1510;
  _SERVER_INFO_1510 = record
    sv1510_sessusers: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1510}
  TServerInfo1510 = _SERVER_INFO_1510;
  SERVER_INFO_1510 = _SERVER_INFO_1510;
  {$EXTERNALSYM SERVER_INFO_1510}

  PServerInfo1511 = ^TServerInfo1511;
  _SERVER_INFO_1511 = record
    sv1511_sessconns: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1511}
  TServerInfo1511 = _SERVER_INFO_1511;
  SERVER_INFO_1511 = _SERVER_INFO_1511;
  {$EXTERNALSYM SERVER_INFO_1511}

  PServerInfo1512 = ^TServerInfo1512;
  _SERVER_INFO_1512 = record
    sv1512_maxnonpagedmemoryusage: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1512}
  TServerInfo1512 = _SERVER_INFO_1512;
  SERVER_INFO_1512 = _SERVER_INFO_1512;
  {$EXTERNALSYM SERVER_INFO_1512}

  PServerInfo1513 = ^TServerInfo1513;
  _SERVER_INFO_1513 = record
    sv1513_maxpagedmemoryusage: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1513}
  TServerInfo1513 = _SERVER_INFO_1513;
  SERVER_INFO_1513 = _SERVER_INFO_1513;
  {$EXTERNALSYM SERVER_INFO_1513}

  PServerInfo1514 = ^TServerInfo1514;
  _SERVER_INFO_1514 = record
    sv1514_enablesoftcompat: BOOL;
  end;
  {$EXTERNALSYM _SERVER_INFO_1514}
  TServerInfo1514 = _SERVER_INFO_1514;
  SERVER_INFO_1514 = _SERVER_INFO_1514;
  {$EXTERNALSYM SERVER_INFO_1514}

  PServerInfo1515 = ^TServerInfo1515;
  _SERVER_INFO_1515 = record
    sv1515_enableforcedlogoff: BOOL;
  end;
  {$EXTERNALSYM _SERVER_INFO_1515}
  TServerInfo1515 = _SERVER_INFO_1515;
  SERVER_INFO_1515 = _SERVER_INFO_1515;
  {$EXTERNALSYM SERVER_INFO_1515}

  PServerInfo1516 = ^TServerInfo1516;
  _SERVER_INFO_1516 = record
    sv1516_timesource: BOOL;
  end;
  {$EXTERNALSYM _SERVER_INFO_1516}
  TServerInfo1516 = _SERVER_INFO_1516;
  SERVER_INFO_1516 = _SERVER_INFO_1516;
  {$EXTERNALSYM SERVER_INFO_1516}

  PServerInfo1518 = ^TServerInfo1518;
  _SERVER_INFO_1518 = record
    sv1518_lmannounce: BOOL;
  end;
  {$EXTERNALSYM _SERVER_INFO_1518}
  TServerInfo1518 = _SERVER_INFO_1518;
  SERVER_INFO_1518 = _SERVER_INFO_1518;
  {$EXTERNALSYM SERVER_INFO_1518}

  PServerInfo1520 = ^TServerInfo1520;
  _SERVER_INFO_1520 = record
    sv1520_maxcopyreadlen: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1520}
  TServerInfo1520 = _SERVER_INFO_1520;
  SERVER_INFO_1520 = _SERVER_INFO_1520;
  {$EXTERNALSYM SERVER_INFO_1520}

  PServerInfo1521 = ^TServerInfo1521;
  _SERVER_INFO_1521 = record
    sv1521_maxcopywritelen: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1521}
  TServerInfo1521 = _SERVER_INFO_1521;
  SERVER_INFO_1521 = _SERVER_INFO_1521;
  {$EXTERNALSYM SERVER_INFO_1521}

  PServerInfo1522 = ^TServerInfo1522;
  _SERVER_INFO_1522 = record
    sv1522_minkeepsearch: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1522}
  TServerInfo1522 = _SERVER_INFO_1522;
  SERVER_INFO_1522 = _SERVER_INFO_1522;
  {$EXTERNALSYM SERVER_INFO_1522}

  PServerInfo1523 = ^TServerInfo1523;
  _SERVER_INFO_1523 = record
    sv1523_maxkeepsearch: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1523}
  TServerInfo1523 = _SERVER_INFO_1523;
  SERVER_INFO_1523 = _SERVER_INFO_1523;
  {$EXTERNALSYM SERVER_INFO_1523}

  PServerInfo1524 = ^TServerInfo1524;
  _SERVER_INFO_1524 = record
    sv1524_minkeepcomplsearch: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1524}
  TServerInfo1524 = _SERVER_INFO_1524;
  SERVER_INFO_1524 = _SERVER_INFO_1524;
  {$EXTERNALSYM SERVER_INFO_1524}

  PServerInfo1525 = ^TServerInfo1525;
  _SERVER_INFO_1525 = record
    sv1525_maxkeepcomplsearch: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1525}
  TServerInfo1525 = _SERVER_INFO_1525;
  SERVER_INFO_1525 = _SERVER_INFO_1525;
  {$EXTERNALSYM SERVER_INFO_1525}

  PServerInfo1528 = ^TServerInfo1528;
  _SERVER_INFO_1528 = record
    sv1528_scavtimeout: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1528}
  TServerInfo1528 = _SERVER_INFO_1528;
  SERVER_INFO_1528 = _SERVER_INFO_1528;
  {$EXTERNALSYM SERVER_INFO_1528}

  PServerInfo1529 = ^TServerInfo1529;
  _SERVER_INFO_1529 = record
    sv1529_minrcvqueue: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1529}
  TServerInfo1529 = _SERVER_INFO_1529;
  SERVER_INFO_1529 = _SERVER_INFO_1529;
  {$EXTERNALSYM SERVER_INFO_1529}

  PServerInfo1530 = ^TServerInfo1530;
  _SERVER_INFO_1530 = record
    sv1530_minfreeworkitems: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1530}
  TServerInfo1530 = _SERVER_INFO_1530;
  SERVER_INFO_1530 = _SERVER_INFO_1530;
  {$EXTERNALSYM SERVER_INFO_1530}

  PServerInfo1533 = ^TServerInfo1533;
  _SERVER_INFO_1533 = record
    sv1533_maxmpxct: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1533}
  TServerInfo1533 = _SERVER_INFO_1533;
  SERVER_INFO_1533 = _SERVER_INFO_1533;
  {$EXTERNALSYM SERVER_INFO_1533}

  PServerInfo1534 = ^TServerInfo1534;
  _SERVER_INFO_1534 = record
    sv1534_oplockbreakwait: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1534}
  TServerInfo1534 = _SERVER_INFO_1534;
  SERVER_INFO_1534 = _SERVER_INFO_1534;
  {$EXTERNALSYM SERVER_INFO_1534}

  PServerInfo1535 = ^TServerInfo1535;
  _SERVER_INFO_1535 = record
    sv1535_oplockbreakresponsewait: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1535}
  TServerInfo1535 = _SERVER_INFO_1535;
  SERVER_INFO_1535 = _SERVER_INFO_1535;
  {$EXTERNALSYM SERVER_INFO_1535}

  PServerInfo1536 = ^TServerInfo1536;
  _SERVER_INFO_1536 = record
    sv1536_enableoplocks: BOOL;
  end;
  {$EXTERNALSYM _SERVER_INFO_1536}
  TServerInfo1536 = _SERVER_INFO_1536;
  SERVER_INFO_1536 = _SERVER_INFO_1536;
  {$EXTERNALSYM SERVER_INFO_1536}

  PServerInfo1537 = ^TServerInfo1537;
  _SERVER_INFO_1537 = record
    sv1537_enableoplockforceclose: BOOL;
  end;
  {$EXTERNALSYM _SERVER_INFO_1537}
  TServerInfo1537 = _SERVER_INFO_1537;
  SERVER_INFO_1537 = _SERVER_INFO_1537;
  {$EXTERNALSYM SERVER_INFO_1537}

  PServerInfo1538 = ^TServerInfo1538;
  _SERVER_INFO_1538 = record
    sv1538_enablefcbopens: BOOL;
  end;
  {$EXTERNALSYM _SERVER_INFO_1538}
  TServerInfo1538 = _SERVER_INFO_1538;
  SERVER_INFO_1538 = _SERVER_INFO_1538;
  {$EXTERNALSYM SERVER_INFO_1538}

  PServerInfo1539 = ^TServerInfo1539;
  _SERVER_INFO_1539 = record
    sv1539_enableraw: BOOL;
  end;
  {$EXTERNALSYM _SERVER_INFO_1539}
  TServerInfo1539 = _SERVER_INFO_1539;
  SERVER_INFO_1539 = _SERVER_INFO_1539;
  {$EXTERNALSYM SERVER_INFO_1539}

  PServerInfo1540 = ^TServerInfo1540;
  _SERVER_INFO_1540 = record
    sv1540_enablesharednetdrives: BOOL;
  end;
  {$EXTERNALSYM _SERVER_INFO_1540}
  TServerInfo1540 = _SERVER_INFO_1540;
  SERVER_INFO_1540 = _SERVER_INFO_1540;
  {$EXTERNALSYM SERVER_INFO_1540}

  PServerInfo1541 = ^TServerInfo1541;
  _SERVER_INFO_1541 = record
    sv1541_minfreeconnections: BOOL;
  end;
  {$EXTERNALSYM _SERVER_INFO_1541}
  TServerInfo1541 = _SERVER_INFO_1541;
  SERVER_INFO_1541 = _SERVER_INFO_1541;
  {$EXTERNALSYM SERVER_INFO_1541}

  PServerInfo1542 = ^TServerInfo1542;
  _SERVER_INFO_1542 = record
    sv1542_maxfreeconnections: BOOL;
  end;
  {$EXTERNALSYM _SERVER_INFO_1542}
  TServerInfo1542 = _SERVER_INFO_1542;
  SERVER_INFO_1542 = _SERVER_INFO_1542;
  {$EXTERNALSYM SERVER_INFO_1542}

  PServerInfo1543 = ^TServerInfo1543;
  _SERVER_INFO_1543 = record
    sv1543_initsesstable: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1543}
  TServerInfo1543 = _SERVER_INFO_1543;
  SERVER_INFO_1543 = _SERVER_INFO_1543;
  {$EXTERNALSYM SERVER_INFO_1543}

  PServerInfo1544 = ^TServerInfo1544;
  _SERVER_INFO_1544 = record
    sv1544_initconntable: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1544}
  TServerInfo1544 = _SERVER_INFO_1544;
  SERVER_INFO_1544 = _SERVER_INFO_1544;
  {$EXTERNALSYM SERVER_INFO_1544}

  PServerInfo1545 = ^TServerInfo1545;
  _SERVER_INFO_1545 = record
    sv1545_initfiletable: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1545}
  TServerInfo1545 = _SERVER_INFO_1545;
  SERVER_INFO_1545 = _SERVER_INFO_1545;
  {$EXTERNALSYM SERVER_INFO_1545}

  PServerInfo1546 = ^TServerInfo1546;
  _SERVER_INFO_1546 = record
    sv1546_initsearchtable: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1546}
  TServerInfo1546 = _SERVER_INFO_1546;
  SERVER_INFO_1546 = _SERVER_INFO_1546;
  {$EXTERNALSYM SERVER_INFO_1546}

  PServerInfo1547 = ^TServerInfo1547;
  _SERVER_INFO_1547 = record
    sv1547_alertschedule: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1547}
  TServerInfo1547 = _SERVER_INFO_1547;
  SERVER_INFO_1547 = _SERVER_INFO_1547;
  {$EXTERNALSYM SERVER_INFO_1547}

  PServerInfo1548 = ^TServerInfo1548;
  _SERVER_INFO_1548 = record
    sv1548_errorthreshold: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1548}
  TServerInfo1548 = _SERVER_INFO_1548;
  SERVER_INFO_1548 = _SERVER_INFO_1548;
  {$EXTERNALSYM SERVER_INFO_1548}

  PServerInfo1549 = ^TServerInfo1549;
  _SERVER_INFO_1549 = record
    sv1549_networkerrorthreshold: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1549}
  TServerInfo1549 = _SERVER_INFO_1549;
  SERVER_INFO_1549 = _SERVER_INFO_1549;
  {$EXTERNALSYM SERVER_INFO_1549}

  PServerInfo1550 = ^TServerInfo1550;
  _SERVER_INFO_1550 = record
    sv1550_diskspacethreshold: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1550}
  TServerInfo1550 = _SERVER_INFO_1550;
  SERVER_INFO_1550 = _SERVER_INFO_1550;
  {$EXTERNALSYM SERVER_INFO_1550}

  PServerInfo1552 = ^TServerInfo1552;
  _SERVER_INFO_1552 = record
    sv1552_maxlinkdelay: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1552}
  TServerInfo1552 = _SERVER_INFO_1552;
  SERVER_INFO_1552 = _SERVER_INFO_1552;
  {$EXTERNALSYM SERVER_INFO_1552}

  PServerInfo1553 = ^TServerInfo1553;
  _SERVER_INFO_1553 = record
    sv1553_minlinkthroughput: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1553}
  TServerInfo1553 = _SERVER_INFO_1553;
  SERVER_INFO_1553 = _SERVER_INFO_1553;
  {$EXTERNALSYM SERVER_INFO_1553}

  PServerInfo1554 = ^TServerInfo1554;
  _SERVER_INFO_1554 = record
    sv1554_linkinfovalidtime: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1554}
  TServerInfo1554 = _SERVER_INFO_1554;
  SERVER_INFO_1554 = _SERVER_INFO_1554;
  {$EXTERNALSYM SERVER_INFO_1554}

  PServerInfo1555 = ^TServerInfo1555;
  _SERVER_INFO_1555 = record
    sv1555_scavqosinfoupdatetime: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1555}
  TServerInfo1555 = _SERVER_INFO_1555;
  SERVER_INFO_1555 = _SERVER_INFO_1555;
  {$EXTERNALSYM SERVER_INFO_1555}

  PServerInfo1556 = ^TServerInfo1556;
  _SERVER_INFO_1556 = record
    sv1556_maxworkitemidletime: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1556}
  TServerInfo1556 = _SERVER_INFO_1556;
  SERVER_INFO_1556 = _SERVER_INFO_1556;
  {$EXTERNALSYM SERVER_INFO_1556}

  PServerInfo1557 = ^TServerInfo1557;
  _SERVER_INFO_1557 = record
    sv1557_maxrawworkitems: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1557}
  TServerInfo1557 = _SERVER_INFO_1557;
  SERVER_INFO_1557 = _SERVER_INFO_1557;
  {$EXTERNALSYM SERVER_INFO_1557}

  PServerInfo1560 = ^TServerInfo1560;
  _SERVER_INFO_1560 = record
    sv1560_producttype: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1560}
  TServerInfo1560 = _SERVER_INFO_1560;
  SERVER_INFO_1560 = _SERVER_INFO_1560;
  {$EXTERNALSYM SERVER_INFO_1560}

  PServerInfo1561 = ^TServerInfo1561;
  _SERVER_INFO_1561 = record
    sv1561_serversize: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1561}
  TServerInfo1561 = _SERVER_INFO_1561;
  SERVER_INFO_1561 = _SERVER_INFO_1561;
  {$EXTERNALSYM SERVER_INFO_1561}

  PServerInfo1562 = ^TServerInfo1562;
  _SERVER_INFO_1562 = record
    sv1562_connectionlessautodisc: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1562}
  TServerInfo1562 = _SERVER_INFO_1562;
  SERVER_INFO_1562 = _SERVER_INFO_1562;
  {$EXTERNALSYM SERVER_INFO_1562}

  PServerInfo1563 = ^TServerInfo1563;
  _SERVER_INFO_1563 = record
    sv1563_sharingviolationretries: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1563}
  TServerInfo1563 = _SERVER_INFO_1563;
  SERVER_INFO_1563 = _SERVER_INFO_1563;
  {$EXTERNALSYM SERVER_INFO_1563}

  PServerInfo1564 = ^TServerInfo1564;
  _SERVER_INFO_1564 = record
    sv1564_sharingviolationdelay: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1564}
  TServerInfo1564 = _SERVER_INFO_1564;
  SERVER_INFO_1564 = _SERVER_INFO_1564;
  {$EXTERNALSYM SERVER_INFO_1564}

  PServerInfo1565 = ^TServerInfo1565;
  _SERVER_INFO_1565 = record
    sv1565_maxglobalopensearch: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1565}
  TServerInfo1565 = _SERVER_INFO_1565;
  SERVER_INFO_1565 = _SERVER_INFO_1565;
  {$EXTERNALSYM SERVER_INFO_1565}

  PServerInfo1566 = ^TServerInfo1566;
  _SERVER_INFO_1566 = record
    sv1566_removeduplicatesearches: BOOL;
  end;
  {$EXTERNALSYM _SERVER_INFO_1566}
  TServerInfo1566 = _SERVER_INFO_1566;
  SERVER_INFO_1566 = _SERVER_INFO_1566;
  {$EXTERNALSYM SERVER_INFO_1566}

  PServerInfo1567 = ^TServerInfo1567;
  _SERVER_INFO_1567 = record
    sv1567_lockviolationretries: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1567}
  TServerInfo1567 = _SERVER_INFO_1567;
  SERVER_INFO_1567 = _SERVER_INFO_1567;
  {$EXTERNALSYM SERVER_INFO_1567}

  PServerInfo1568 = ^TServerInfo1568;
  _SERVER_INFO_1568 = record
    sv1568_lockviolationoffset: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1568}
  TServerInfo1568 = _SERVER_INFO_1568;
  SERVER_INFO_1568 = _SERVER_INFO_1568;
  {$EXTERNALSYM SERVER_INFO_1568}

  PServerInfo1569 = ^TServerInfo1569;
  _SERVER_INFO_1569 = record
    sv1569_lockviolationdelay: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1569}
  TServerInfo1569 = _SERVER_INFO_1569;
  SERVER_INFO_1569 = _SERVER_INFO_1569;
  {$EXTERNALSYM SERVER_INFO_1569}

  PServerInfo1570 = ^TServerInfo1570;
  _SERVER_INFO_1570 = record
    sv1570_mdlreadswitchover: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1570}
  TServerInfo1570 = _SERVER_INFO_1570;
  SERVER_INFO_1570 = _SERVER_INFO_1570;
  {$EXTERNALSYM SERVER_INFO_1570}

  PServerInfo1571 = ^TServerInfo1571;
  _SERVER_INFO_1571 = record
    sv1571_cachedopenlimit: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1571}
  TServerInfo1571 = _SERVER_INFO_1571;
  SERVER_INFO_1571 = _SERVER_INFO_1571;
  {$EXTERNALSYM SERVER_INFO_1571}

  PServerInfo1572 = ^TServerInfo1572;
  _SERVER_INFO_1572 = record
    sv1572_criticalthreads: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1572}
  TServerInfo1572 = _SERVER_INFO_1572;
  SERVER_INFO_1572 = _SERVER_INFO_1572;
  {$EXTERNALSYM SERVER_INFO_1572}

  PServerInfo1573 = ^TServerInfo1573;
  _SERVER_INFO_1573 = record
    sv1573_restrictnullsessaccess: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1573}
  TServerInfo1573 = _SERVER_INFO_1573;
  SERVER_INFO_1573 = _SERVER_INFO_1573;
  {$EXTERNALSYM SERVER_INFO_1573}

  PServerInfo1574 = ^TServerInfo1574;
  _SERVER_INFO_1574 = record
    sv1574_enablewfw311directipx: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1574}
  TServerInfo1574 = _SERVER_INFO_1574;
  SERVER_INFO_1574 = _SERVER_INFO_1574;
  {$EXTERNALSYM SERVER_INFO_1574}

  PServerInfo1575 = ^TServerInfo1575;
  _SERVER_INFO_1575 = record
    sv1575_otherqueueaffinity: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1575}
  TServerInfo1575 = _SERVER_INFO_1575;
  SERVER_INFO_1575 = _SERVER_INFO_1575;
  {$EXTERNALSYM SERVER_INFO_1575}

  PServerInfo1576 = ^TServerInfo1576;
  _SERVER_INFO_1576 = record
    sv1576_queuesamplesecs: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1576}
  TServerInfo1576 = _SERVER_INFO_1576;
  SERVER_INFO_1576 = _SERVER_INFO_1576;
  {$EXTERNALSYM SERVER_INFO_1576}

  PServerInfo1577 = ^TServerInfo1577;
  _SERVER_INFO_1577 = record
    sv1577_balancecount: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1577}
  TServerInfo1577 = _SERVER_INFO_1577;
  SERVER_INFO_1577 = _SERVER_INFO_1577;
  {$EXTERNALSYM SERVER_INFO_1577}

  PServerInfo1578 = ^TServerInfo1578;
  _SERVER_INFO_1578 = record
    sv1578_preferredaffinity: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1578}
  TServerInfo1578 = _SERVER_INFO_1578;
  SERVER_INFO_1578 = _SERVER_INFO_1578;
  {$EXTERNALSYM SERVER_INFO_1578}

  PServerInfo1579 = ^TServerInfo1579;
  _SERVER_INFO_1579 = record
    sv1579_maxfreerfcbs: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1579}
  TServerInfo1579 = _SERVER_INFO_1579;
  SERVER_INFO_1579 = _SERVER_INFO_1579;
  {$EXTERNALSYM SERVER_INFO_1579}

  PServerInfo1580 = ^TServerInfo1580;
  _SERVER_INFO_1580 = record
    sv1580_maxfreemfcbs: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1580}
  TServerInfo1580 = _SERVER_INFO_1580;
  SERVER_INFO_1580 = _SERVER_INFO_1580;
  {$EXTERNALSYM SERVER_INFO_1580}

  PServerInfo1581 = ^TServerInfo1581;
  _SERVER_INFO_1581 = record
    sv1581_maxfreemlcbs: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1581}
  TServerInfo1581 = _SERVER_INFO_1581;
  SERVER_INFO_1581 = _SERVER_INFO_1581;
  {$EXTERNALSYM SERVER_INFO_1581}

  PServerInfo1582 = ^TServerInfo1582;
  _SERVER_INFO_1582 = record
    sv1582_maxfreepagedpoolchunks: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1582}
  TServerInfo1582 = _SERVER_INFO_1582;
  SERVER_INFO_1582 = _SERVER_INFO_1582;
  {$EXTERNALSYM SERVER_INFO_1582}

  PServerInfo1583 = ^TServerInfo1583;
  _SERVER_INFO_1583 = record
    sv1583_minpagedpoolchunksize: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1583}
  TServerInfo1583 = _SERVER_INFO_1583;
  SERVER_INFO_1583 = _SERVER_INFO_1583;
  {$EXTERNALSYM SERVER_INFO_1583}

  PServerInfo1584 = ^TServerInfo1584;
  _SERVER_INFO_1584 = record
    sv1584_maxpagedpoolchunksize: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1584}
  TServerInfo1584 = _SERVER_INFO_1584;
  SERVER_INFO_1584 = _SERVER_INFO_1584;
  {$EXTERNALSYM SERVER_INFO_1584}

  PServerInfo1585 = ^TServerInfo1585;
  _SERVER_INFO_1585 = record
    sv1585_sendsfrompreferredprocessor: BOOL;
  end;
  {$EXTERNALSYM _SERVER_INFO_1585}
  TServerInfo1585 = _SERVER_INFO_1585;
  SERVER_INFO_1585 = _SERVER_INFO_1585;
  {$EXTERNALSYM SERVER_INFO_1585}

  PServerInfo1586 = ^TServerInfo1586;
  _SERVER_INFO_1586 = record
    sv1586_maxthreadsperqueue: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1586}
  TServerInfo1586 = _SERVER_INFO_1586;
  SERVER_INFO_1586 = _SERVER_INFO_1586;
  {$EXTERNALSYM SERVER_INFO_1586}

  PServerInfo1587 = ^TServerInfo1587;
  _SERVER_INFO_1587 = record
    sv1587_cacheddirectorylimit: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1587}
  TServerInfo1587 = _SERVER_INFO_1587;
  SERVER_INFO_1587 = _SERVER_INFO_1587;
  {$EXTERNALSYM SERVER_INFO_1587}

  PServerInfo1588 = ^TServerInfo1588;
  _SERVER_INFO_1588 = record
    sv1588_maxcopylength: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1588}
  TServerInfo1588 = _SERVER_INFO_1588;
  SERVER_INFO_1588 = _SERVER_INFO_1588;
  {$EXTERNALSYM SERVER_INFO_1588}

  PServerInfo1590 = ^TServerInfo1590;
  _SERVER_INFO_1590 = record
    sv1590_enablecompression: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1590}
  TServerInfo1590 = _SERVER_INFO_1590;
  SERVER_INFO_1590 = _SERVER_INFO_1590;
  {$EXTERNALSYM SERVER_INFO_1590}

  PServerInfo1591 = ^TServerInfo1591;
  _SERVER_INFO_1591 = record
    sv1591_autosharewks: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1591}
  TServerInfo1591 = _SERVER_INFO_1591;
  SERVER_INFO_1591 = _SERVER_INFO_1591;
  {$EXTERNALSYM SERVER_INFO_1591}

  PServerInfo1592 = ^TServerInfo1592;
  _SERVER_INFO_1592 = record
    sv1592_autosharewks: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1592}
  TServerInfo1592 = _SERVER_INFO_1592;
  SERVER_INFO_1592 = _SERVER_INFO_1592;
  {$EXTERNALSYM SERVER_INFO_1592}

  PServerInfo1593 = ^TServerInfo1593;
  _SERVER_INFO_1593 = record
    sv1593_enablesecuritysignature: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1593}
  TServerInfo1593 = _SERVER_INFO_1593;
  SERVER_INFO_1593 = _SERVER_INFO_1593;
  {$EXTERNALSYM SERVER_INFO_1593}

  PServerInfo1594 = ^TServerInfo1594;
  _SERVER_INFO_1594 = record
    sv1594_requiresecuritysignature: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1594}
  TServerInfo1594 = _SERVER_INFO_1594;
  SERVER_INFO_1594 = _SERVER_INFO_1594;
  {$EXTERNALSYM SERVER_INFO_1594}

  PServerInfo1595 = ^TServerInfo1595;
  _SERVER_INFO_1595 = record
    sv1595_minclientbuffersize: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1595}
  TServerInfo1595 = _SERVER_INFO_1595;
  SERVER_INFO_1595 = _SERVER_INFO_1595;
  {$EXTERNALSYM SERVER_INFO_1595}

  PServerInfo1596 = ^TServerInfo1596;
  _SERVER_INFO_1596 = record
    sv1596_ConnectionNoSessionsTimeout: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1596}
  TServerInfo1596 = _SERVER_INFO_1596;
  SERVER_INFO_1596 = _SERVER_INFO_1596;
  {$EXTERNALSYM SERVER_INFO_1596}

  PServerInfo1597 = ^TServerInfo1597;
  _SERVER_INFO_1597 = record
    sv1597_IdleThreadTimeOut: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1597}
  TServerInfo1597 = _SERVER_INFO_1597;
  SERVER_INFO_1597 = _SERVER_INFO_1597;
  {$EXTERNALSYM SERVER_INFO_1597}

  PServerInfo1598 = ^TServerInfo1598;
  _SERVER_INFO_1598 = record
    sv1598_enableW9xsecuritysignature: DWORD;
  end;
  {$EXTERNALSYM _SERVER_INFO_1598}
  TServerInfo1598 = _SERVER_INFO_1598;
  SERVER_INFO_1598 = _SERVER_INFO_1598;
  {$EXTERNALSYM SERVER_INFO_1598}

// A special structure definition is required in order for this
// structure to work with RPC.  The problem is that having addresslength
// indicate the number of bytes in address means that RPC must know the
// link between the two.

  PServerTransportInfo0 = ^TServerTransportInfo0;
  _SERVER_TRANSPORT_INFO_0 = record
    svti0_numberofvcs: DWORD;
    svti0_transportname: LPWSTR;
    svti0_transportaddress: Pointer;
    svti0_transportaddresslength: DWORD;
    svti0_networkaddress: LPWSTR;
  end;
  {$EXTERNALSYM _SERVER_TRANSPORT_INFO_0}
  TServerTransportInfo0 = _SERVER_TRANSPORT_INFO_0;
  SERVER_TRANSPORT_INFO_0 = _SERVER_TRANSPORT_INFO_0;
  {$EXTERNALSYM SERVER_TRANSPORT_INFO_0}

  PServerTransportInfo1 = ^TServerTransportInfo1;
  _SERVER_TRANSPORT_INFO_1 = record
    svti1_numberofvcs: DWORD;
    svti1_transportname: LPWSTR;
    svti1_transportaddress: Pointer;
    svti1_transportaddresslength: DWORD;
    svti1_networkaddress: LPWSTR;
    svti1_domain: LPWSTR;
  end;
  {$EXTERNALSYM _SERVER_TRANSPORT_INFO_1}
  TServerTransportInfo1 = _SERVER_TRANSPORT_INFO_1;
  SERVER_TRANSPORT_INFO_1 = _SERVER_TRANSPORT_INFO_1;
  {$EXTERNALSYM SERVER_TRANSPORT_INFO_1}

  PServerTransportInfo2 = ^TServerTransportInfo2;
  _SERVER_TRANSPORT_INFO_2 = record
    svti2_numberofvcs: DWORD;
    svti2_transportname: LPWSTR;
    svti2_transportaddress: Pointer;
    svti2_transportaddresslength: DWORD;
    svti2_networkaddress: LPWSTR;
    svti2_domain: LPWSTR;
    svti2_flags: ULONG;
  end;
  {$EXTERNALSYM _SERVER_TRANSPORT_INFO_2}
  TServerTransportInfo2 = _SERVER_TRANSPORT_INFO_2;
  SERVER_TRANSPORT_INFO_2 = _SERVER_TRANSPORT_INFO_2;
  {$EXTERNALSYM SERVER_TRANSPORT_INFO_2}

  PServerTransportInfo3 = ^TServerTransportInfo3;
  _SERVER_TRANSPORT_INFO_3 = record
    svti3_numberofvcs: DWORD;
    svti3_transportname: LPWSTR;
    svti3_transportaddress: Pointer;
    svti3_transportaddresslength: DWORD;
    svti3_networkaddress: LPWSTR;
    svti3_domain: LPWSTR;
    svti3_flags: ULONG;
    svti3_passwordlength: DWORD;
    svti3_password: packed array[0..255] of Byte;
  end;
  {$EXTERNALSYM _SERVER_TRANSPORT_INFO_3}
  TServerTransportInfo3 = _SERVER_TRANSPORT_INFO_3;
  SERVER_TRANSPORT_INFO_3 = _SERVER_TRANSPORT_INFO_3;
  {$EXTERNALSYM SERVER_TRANSPORT_INFO_3}

// Defines - SERVER

// The platform ID indicates the levels to use for platform-specific
// information.

const
  SV_PLATFORM_ID_OS2 = 400;
  {$EXTERNALSYM SV_PLATFORM_ID_OS2}
  SV_PLATFORM_ID_NT  = 500;
  {$EXTERNALSYM SV_PLATFORM_ID_NT}

// Mask to be applied to svX_version_major in order to obtain
// the major version number.

  MAJOR_VERSION_MASK  = $0F;
  {$EXTERNALSYM MAJOR_VERSION_MASK}

// Bit-mapped values for svX_type fields. X = 1, 2 or 3.

  SV_TYPE_WORKSTATION         = $00000001;
  {$EXTERNALSYM SV_TYPE_WORKSTATION}
  SV_TYPE_SERVER              = $00000002;
  {$EXTERNALSYM SV_TYPE_SERVER}
  SV_TYPE_SQLSERVER           = $00000004;
  {$EXTERNALSYM SV_TYPE_SQLSERVER}
  SV_TYPE_DOMAIN_CTRL         = $00000008;
  {$EXTERNALSYM SV_TYPE_DOMAIN_CTRL}
  SV_TYPE_DOMAIN_BAKCTRL      = $00000010;
  {$EXTERNALSYM SV_TYPE_DOMAIN_BAKCTRL}
  SV_TYPE_TIME_SOURCE         = $00000020;
  {$EXTERNALSYM SV_TYPE_TIME_SOURCE}
  SV_TYPE_AFP                 = $00000040;
  {$EXTERNALSYM SV_TYPE_AFP}
  SV_TYPE_NOVELL              = $00000080;
  {$EXTERNALSYM SV_TYPE_NOVELL}
  SV_TYPE_DOMAIN_MEMBER       = $00000100;
  {$EXTERNALSYM SV_TYPE_DOMAIN_MEMBER}
  SV_TYPE_PRINTQ_SERVER       = $00000200;
  {$EXTERNALSYM SV_TYPE_PRINTQ_SERVER}
  SV_TYPE_DIALIN_SERVER       = $00000400;
  {$EXTERNALSYM SV_TYPE_DIALIN_SERVER}
  SV_TYPE_XENIX_SERVER        = $00000800;
  {$EXTERNALSYM SV_TYPE_XENIX_SERVER}
  SV_TYPE_SERVER_UNIX         = SV_TYPE_XENIX_SERVER;
  {$EXTERNALSYM SV_TYPE_SERVER_UNIX}
  SV_TYPE_NT                  = $00001000;
  {$EXTERNALSYM SV_TYPE_NT}
  SV_TYPE_WFW                 = $00002000;
  {$EXTERNALSYM SV_TYPE_WFW}
  SV_TYPE_SERVER_MFPN         = $00004000;
  {$EXTERNALSYM SV_TYPE_SERVER_MFPN}
  SV_TYPE_SERVER_NT           = $00008000;
  {$EXTERNALSYM SV_TYPE_SERVER_NT}
  SV_TYPE_POTENTIAL_BROWSER   = $00010000;
  {$EXTERNALSYM SV_TYPE_POTENTIAL_BROWSER}
  SV_TYPE_BACKUP_BROWSER      = $00020000;
  {$EXTERNALSYM SV_TYPE_BACKUP_BROWSER}
  SV_TYPE_MASTER_BROWSER      = $00040000;
  {$EXTERNALSYM SV_TYPE_MASTER_BROWSER}
  SV_TYPE_DOMAIN_MASTER       = $00080000;
  {$EXTERNALSYM SV_TYPE_DOMAIN_MASTER}
  SV_TYPE_SERVER_OSF          = $00100000;
  {$EXTERNALSYM SV_TYPE_SERVER_OSF}
  SV_TYPE_SERVER_VMS          = $00200000;
  {$EXTERNALSYM SV_TYPE_SERVER_VMS}
  SV_TYPE_WINDOWS             = $00400000;  //* Windows95 and above */
  {$EXTERNALSYM SV_TYPE_WINDOWS}
  SV_TYPE_DFS                 = $00800000;  //* Root of a DFS tree */
  {$EXTERNALSYM SV_TYPE_DFS}
  SV_TYPE_CLUSTER_NT          = $01000000;  //* NT Cluster */
  {$EXTERNALSYM SV_TYPE_CLUSTER_NT}
  SV_TYPE_TERMINALSERVER      = $02000000;  //* Terminal Server(Hydra) */
  {$EXTERNALSYM SV_TYPE_TERMINALSERVER}
  SV_TYPE_DCE                 = $10000000;  //* IBM DSS (Directory and Security Services) or equivalent */
  {$EXTERNALSYM SV_TYPE_DCE}
  SV_TYPE_ALTERNATE_XPORT     = $20000000;  //* return list for alternate transport */
  {$EXTERNALSYM SV_TYPE_ALTERNATE_XPORT}
  SV_TYPE_LOCAL_LIST_ONLY     = $40000000;  //* Return local list only */
  {$EXTERNALSYM SV_TYPE_LOCAL_LIST_ONLY}
  SV_TYPE_DOMAIN_ENUM         = $80000000;
  {$EXTERNALSYM SV_TYPE_DOMAIN_ENUM}
  SV_TYPE_ALL                 = $FFFFFFFF;  //* handy for NetServerEnum2 */
  {$EXTERNALSYM SV_TYPE_ALL}

// Special value for sv102_disc that specifies infinite disconnect
// time.

  SV_NODISC           = -1;  //* No autodisconnect timeout enforced */
  {$EXTERNALSYM SV_NODISC}

// Values of svX_security field. X = 2 or 3.

  SV_USERSECURITY     = 1;
  {$EXTERNALSYM SV_USERSECURITY}
  SV_SHARESECURITY    = 0;
  {$EXTERNALSYM SV_SHARESECURITY}

// Values of svX_hidden field. X = 2 or 3.

  SV_HIDDEN       = 1;
  {$EXTERNALSYM SV_HIDDEN}
  SV_VISIBLE      = 0;
  {$EXTERNALSYM SV_VISIBLE}

// Values for ParmError parameter to NetServerSetInfo.

  SV_PLATFORM_ID_PARMNUM          = 101;
  {$EXTERNALSYM SV_PLATFORM_ID_PARMNUM}
  SV_NAME_PARMNUM                 = 102;
  {$EXTERNALSYM SV_NAME_PARMNUM}
  SV_VERSION_MAJOR_PARMNUM        = 103;
  {$EXTERNALSYM SV_VERSION_MAJOR_PARMNUM}
  SV_VERSION_MINOR_PARMNUM        = 104;
  {$EXTERNALSYM SV_VERSION_MINOR_PARMNUM}
  SV_TYPE_PARMNUM                 = 105;
  {$EXTERNALSYM SV_TYPE_PARMNUM}
  SV_COMMENT_PARMNUM              = 5;
  {$EXTERNALSYM SV_COMMENT_PARMNUM}
  SV_USERS_PARMNUM                = 107;
  {$EXTERNALSYM SV_USERS_PARMNUM}
  SV_DISC_PARMNUM                 = 10;
  {$EXTERNALSYM SV_DISC_PARMNUM}
  SV_HIDDEN_PARMNUM               = 16;
  {$EXTERNALSYM SV_HIDDEN_PARMNUM}
  SV_ANNOUNCE_PARMNUM             = 17;
  {$EXTERNALSYM SV_ANNOUNCE_PARMNUM}
  SV_ANNDELTA_PARMNUM             = 18;
  {$EXTERNALSYM SV_ANNDELTA_PARMNUM}
  SV_USERPATH_PARMNUM             = 112;
  {$EXTERNALSYM SV_USERPATH_PARMNUM}

  SV_ULIST_MTIME_PARMNUM          = 401;
  {$EXTERNALSYM SV_ULIST_MTIME_PARMNUM}
  SV_GLIST_MTIME_PARMNUM          = 402;
  {$EXTERNALSYM SV_GLIST_MTIME_PARMNUM}
  SV_ALIST_MTIME_PARMNUM          = 403;
  {$EXTERNALSYM SV_ALIST_MTIME_PARMNUM}
  SV_ALERTS_PARMNUM               = 11;
  {$EXTERNALSYM SV_ALERTS_PARMNUM}
  SV_SECURITY_PARMNUM             = 405;
  {$EXTERNALSYM SV_SECURITY_PARMNUM}
  SV_NUMADMIN_PARMNUM             = 406;
  {$EXTERNALSYM SV_NUMADMIN_PARMNUM}
  SV_LANMASK_PARMNUM              = 407;
  {$EXTERNALSYM SV_LANMASK_PARMNUM}
  SV_GUESTACC_PARMNUM             = 408;
  {$EXTERNALSYM SV_GUESTACC_PARMNUM}
  SV_CHDEVQ_PARMNUM               = 410;
  {$EXTERNALSYM SV_CHDEVQ_PARMNUM}
  SV_CHDEVJOBS_PARMNUM            = 411;
  {$EXTERNALSYM SV_CHDEVJOBS_PARMNUM}
  SV_CONNECTIONS_PARMNUM          = 412;
  {$EXTERNALSYM SV_CONNECTIONS_PARMNUM}
  SV_SHARES_PARMNUM               = 413;
  {$EXTERNALSYM SV_SHARES_PARMNUM}
  SV_OPENFILES_PARMNUM            = 414;
  {$EXTERNALSYM SV_OPENFILES_PARMNUM}
  SV_SESSREQS_PARMNUM             = 417;
  {$EXTERNALSYM SV_SESSREQS_PARMNUM}
  SV_ACTIVELOCKS_PARMNUM          = 419;
  {$EXTERNALSYM SV_ACTIVELOCKS_PARMNUM}
  SV_NUMREQBUF_PARMNUM            = 420;
  {$EXTERNALSYM SV_NUMREQBUF_PARMNUM}
  SV_NUMBIGBUF_PARMNUM            = 422;
  {$EXTERNALSYM SV_NUMBIGBUF_PARMNUM}
  SV_NUMFILETASKS_PARMNUM         = 423;
  {$EXTERNALSYM SV_NUMFILETASKS_PARMNUM}
  SV_ALERTSCHED_PARMNUM           = 37;
  {$EXTERNALSYM SV_ALERTSCHED_PARMNUM}
  SV_ERRORALERT_PARMNUM           = 38;
  {$EXTERNALSYM SV_ERRORALERT_PARMNUM}
  SV_LOGONALERT_PARMNUM           = 39;
  {$EXTERNALSYM SV_LOGONALERT_PARMNUM}
  SV_ACCESSALERT_PARMNUM          = 40;
  {$EXTERNALSYM SV_ACCESSALERT_PARMNUM}
  SV_DISKALERT_PARMNUM            = 41;
  {$EXTERNALSYM SV_DISKALERT_PARMNUM}
  SV_NETIOALERT_PARMNUM           = 42;
  {$EXTERNALSYM SV_NETIOALERT_PARMNUM}
  SV_MAXAUDITSZ_PARMNUM           = 43;
  {$EXTERNALSYM SV_MAXAUDITSZ_PARMNUM}
  SV_SRVHEURISTICS_PARMNUM        = 431;
  {$EXTERNALSYM SV_SRVHEURISTICS_PARMNUM}

  SV_SESSOPENS_PARMNUM                = 501;
  {$EXTERNALSYM SV_SESSOPENS_PARMNUM}
  SV_SESSVCS_PARMNUM                  = 502;
  {$EXTERNALSYM SV_SESSVCS_PARMNUM}
  SV_OPENSEARCH_PARMNUM               = 503;
  {$EXTERNALSYM SV_OPENSEARCH_PARMNUM}
  SV_SIZREQBUF_PARMNUM                = 504;
  {$EXTERNALSYM SV_SIZREQBUF_PARMNUM}
  SV_INITWORKITEMS_PARMNUM            = 505;
  {$EXTERNALSYM SV_INITWORKITEMS_PARMNUM}
  SV_MAXWORKITEMS_PARMNUM             = 506;
  {$EXTERNALSYM SV_MAXWORKITEMS_PARMNUM}
  SV_RAWWORKITEMS_PARMNUM             = 507;
  {$EXTERNALSYM SV_RAWWORKITEMS_PARMNUM}
  SV_IRPSTACKSIZE_PARMNUM             = 508;
  {$EXTERNALSYM SV_IRPSTACKSIZE_PARMNUM}
  SV_MAXRAWBUFLEN_PARMNUM             = 509;
  {$EXTERNALSYM SV_MAXRAWBUFLEN_PARMNUM}
  SV_SESSUSERS_PARMNUM                = 510;
  {$EXTERNALSYM SV_SESSUSERS_PARMNUM}
  SV_SESSCONNS_PARMNUM                = 511;
  {$EXTERNALSYM SV_SESSCONNS_PARMNUM}
  SV_MAXNONPAGEDMEMORYUSAGE_PARMNUM   = 512;
  {$EXTERNALSYM SV_MAXNONPAGEDMEMORYUSAGE_PARMNUM}
  SV_MAXPAGEDMEMORYUSAGE_PARMNUM      = 513;
  {$EXTERNALSYM SV_MAXPAGEDMEMORYUSAGE_PARMNUM}
  SV_ENABLESOFTCOMPAT_PARMNUM         = 514;
  {$EXTERNALSYM SV_ENABLESOFTCOMPAT_PARMNUM}
  SV_ENABLEFORCEDLOGOFF_PARMNUM       = 515;
  {$EXTERNALSYM SV_ENABLEFORCEDLOGOFF_PARMNUM}
  SV_TIMESOURCE_PARMNUM               = 516;
  {$EXTERNALSYM SV_TIMESOURCE_PARMNUM}
  SV_ACCEPTDOWNLEVELAPIS_PARMNUM      = 517;
  {$EXTERNALSYM SV_ACCEPTDOWNLEVELAPIS_PARMNUM}
  SV_LMANNOUNCE_PARMNUM               = 518;
  {$EXTERNALSYM SV_LMANNOUNCE_PARMNUM}
  SV_DOMAIN_PARMNUM                   = 519;
  {$EXTERNALSYM SV_DOMAIN_PARMNUM}
  SV_MAXCOPYREADLEN_PARMNUM           = 520;
  {$EXTERNALSYM SV_MAXCOPYREADLEN_PARMNUM}
  SV_MAXCOPYWRITELEN_PARMNUM          = 521;
  {$EXTERNALSYM SV_MAXCOPYWRITELEN_PARMNUM}
  SV_MINKEEPSEARCH_PARMNUM            = 522;
  {$EXTERNALSYM SV_MINKEEPSEARCH_PARMNUM}
  SV_MAXKEEPSEARCH_PARMNUM            = 523;
  {$EXTERNALSYM SV_MAXKEEPSEARCH_PARMNUM}
  SV_MINKEEPCOMPLSEARCH_PARMNUM       = 524;
  {$EXTERNALSYM SV_MINKEEPCOMPLSEARCH_PARMNUM}
  SV_MAXKEEPCOMPLSEARCH_PARMNUM       = 525;
  {$EXTERNALSYM SV_MAXKEEPCOMPLSEARCH_PARMNUM}
  SV_THREADCOUNTADD_PARMNUM           = 526;
  {$EXTERNALSYM SV_THREADCOUNTADD_PARMNUM}
  SV_NUMBLOCKTHREADS_PARMNUM          = 527;
  {$EXTERNALSYM SV_NUMBLOCKTHREADS_PARMNUM}
  SV_SCAVTIMEOUT_PARMNUM              = 528;
  {$EXTERNALSYM SV_SCAVTIMEOUT_PARMNUM}
  SV_MINRCVQUEUE_PARMNUM              = 529;
  {$EXTERNALSYM SV_MINRCVQUEUE_PARMNUM}
  SV_MINFREEWORKITEMS_PARMNUM         = 530;
  {$EXTERNALSYM SV_MINFREEWORKITEMS_PARMNUM}
  SV_XACTMEMSIZE_PARMNUM              = 531;
  {$EXTERNALSYM SV_XACTMEMSIZE_PARMNUM}
  SV_THREADPRIORITY_PARMNUM           = 532;
  {$EXTERNALSYM SV_THREADPRIORITY_PARMNUM}
  SV_MAXMPXCT_PARMNUM                 = 533;
  {$EXTERNALSYM SV_MAXMPXCT_PARMNUM}
  SV_OPLOCKBREAKWAIT_PARMNUM          = 534;
  {$EXTERNALSYM SV_OPLOCKBREAKWAIT_PARMNUM}
  SV_OPLOCKBREAKRESPONSEWAIT_PARMNUM  = 535;
  {$EXTERNALSYM SV_OPLOCKBREAKRESPONSEWAIT_PARMNUM}
  SV_ENABLEOPLOCKS_PARMNUM            = 536;
  {$EXTERNALSYM SV_ENABLEOPLOCKS_PARMNUM}
  SV_ENABLEOPLOCKFORCECLOSE_PARMNUM   = 537;
  {$EXTERNALSYM SV_ENABLEOPLOCKFORCECLOSE_PARMNUM}
  SV_ENABLEFCBOPENS_PARMNUM           = 538;
  {$EXTERNALSYM SV_ENABLEFCBOPENS_PARMNUM}
  SV_ENABLERAW_PARMNUM                = 539;
  {$EXTERNALSYM SV_ENABLERAW_PARMNUM}
  SV_ENABLESHAREDNETDRIVES_PARMNUM    = 540;
  {$EXTERNALSYM SV_ENABLESHAREDNETDRIVES_PARMNUM}
  SV_MINFREECONNECTIONS_PARMNUM       = 541;
  {$EXTERNALSYM SV_MINFREECONNECTIONS_PARMNUM}
  SV_MAXFREECONNECTIONS_PARMNUM       = 542;
  {$EXTERNALSYM SV_MAXFREECONNECTIONS_PARMNUM}
  SV_INITSESSTABLE_PARMNUM            = 543;
  {$EXTERNALSYM SV_INITSESSTABLE_PARMNUM}
  SV_INITCONNTABLE_PARMNUM            = 544;
  {$EXTERNALSYM SV_INITCONNTABLE_PARMNUM}
  SV_INITFILETABLE_PARMNUM            = 545;
  {$EXTERNALSYM SV_INITFILETABLE_PARMNUM}
  SV_INITSEARCHTABLE_PARMNUM          = 546;
  {$EXTERNALSYM SV_INITSEARCHTABLE_PARMNUM}
  SV_ALERTSCHEDULE_PARMNUM            = 547;
  {$EXTERNALSYM SV_ALERTSCHEDULE_PARMNUM}
  SV_ERRORTHRESHOLD_PARMNUM           = 548;
  {$EXTERNALSYM SV_ERRORTHRESHOLD_PARMNUM}
  SV_NETWORKERRORTHRESHOLD_PARMNUM    = 549;
  {$EXTERNALSYM SV_NETWORKERRORTHRESHOLD_PARMNUM}
  SV_DISKSPACETHRESHOLD_PARMNUM       = 550;
  {$EXTERNALSYM SV_DISKSPACETHRESHOLD_PARMNUM}
  SV_MAXLINKDELAY_PARMNUM             = 552;
  {$EXTERNALSYM SV_MAXLINKDELAY_PARMNUM}
  SV_MINLINKTHROUGHPUT_PARMNUM        = 553;
  {$EXTERNALSYM SV_MINLINKTHROUGHPUT_PARMNUM}
  SV_LINKINFOVALIDTIME_PARMNUM        = 554;
  {$EXTERNALSYM SV_LINKINFOVALIDTIME_PARMNUM}
  SV_SCAVQOSINFOUPDATETIME_PARMNUM    = 555;
  {$EXTERNALSYM SV_SCAVQOSINFOUPDATETIME_PARMNUM}
  SV_MAXWORKITEMIDLETIME_PARMNUM      = 556;
  {$EXTERNALSYM SV_MAXWORKITEMIDLETIME_PARMNUM}
  SV_MAXRAWWORKITEMS_PARMNUM          = 557;
  {$EXTERNALSYM SV_MAXRAWWORKITEMS_PARMNUM}
  SV_PRODUCTTYPE_PARMNUM              = 560;
  {$EXTERNALSYM SV_PRODUCTTYPE_PARMNUM}
  SV_SERVERSIZE_PARMNUM               = 561;
  {$EXTERNALSYM SV_SERVERSIZE_PARMNUM}
  SV_CONNECTIONLESSAUTODISC_PARMNUM   = 562;
  {$EXTERNALSYM SV_CONNECTIONLESSAUTODISC_PARMNUM}
  SV_SHARINGVIOLATIONRETRIES_PARMNUM  = 563;
  {$EXTERNALSYM SV_SHARINGVIOLATIONRETRIES_PARMNUM}
  SV_SHARINGVIOLATIONDELAY_PARMNUM    = 564;
  {$EXTERNALSYM SV_SHARINGVIOLATIONDELAY_PARMNUM}
  SV_MAXGLOBALOPENSEARCH_PARMNUM      = 565;
  {$EXTERNALSYM SV_MAXGLOBALOPENSEARCH_PARMNUM}
  SV_REMOVEDUPLICATESEARCHES_PARMNUM  = 566;
  {$EXTERNALSYM SV_REMOVEDUPLICATESEARCHES_PARMNUM}
  SV_LOCKVIOLATIONRETRIES_PARMNUM     = 567;
  {$EXTERNALSYM SV_LOCKVIOLATIONRETRIES_PARMNUM}
  SV_LOCKVIOLATIONOFFSET_PARMNUM      = 568;
  {$EXTERNALSYM SV_LOCKVIOLATIONOFFSET_PARMNUM}
  SV_LOCKVIOLATIONDELAY_PARMNUM       = 569;
  {$EXTERNALSYM SV_LOCKVIOLATIONDELAY_PARMNUM}
  SV_MDLREADSWITCHOVER_PARMNUM        = 570;
  {$EXTERNALSYM SV_MDLREADSWITCHOVER_PARMNUM}
  SV_CACHEDOPENLIMIT_PARMNUM          = 571;
  {$EXTERNALSYM SV_CACHEDOPENLIMIT_PARMNUM}
  SV_CRITICALTHREADS_PARMNUM          = 572;
  {$EXTERNALSYM SV_CRITICALTHREADS_PARMNUM}
  SV_RESTRICTNULLSESSACCESS_PARMNUM   = 573;
  {$EXTERNALSYM SV_RESTRICTNULLSESSACCESS_PARMNUM}
  SV_ENABLEWFW311DIRECTIPX_PARMNUM    = 574;
  {$EXTERNALSYM SV_ENABLEWFW311DIRECTIPX_PARMNUM}
  SV_OTHERQUEUEAFFINITY_PARMNUM       = 575;
  {$EXTERNALSYM SV_OTHERQUEUEAFFINITY_PARMNUM}
  SV_QUEUESAMPLESECS_PARMNUM          = 576;
  {$EXTERNALSYM SV_QUEUESAMPLESECS_PARMNUM}
  SV_BALANCECOUNT_PARMNUM             = 577;
  {$EXTERNALSYM SV_BALANCECOUNT_PARMNUM}
  SV_PREFERREDAFFINITY_PARMNUM        = 578;
  {$EXTERNALSYM SV_PREFERREDAFFINITY_PARMNUM}
  SV_MAXFREERFCBS_PARMNUM             = 579;
  {$EXTERNALSYM SV_MAXFREERFCBS_PARMNUM}
  SV_MAXFREEMFCBS_PARMNUM             = 580;
  {$EXTERNALSYM SV_MAXFREEMFCBS_PARMNUM}
  SV_MAXFREELFCBS_PARMNUM             = 581;
  {$EXTERNALSYM SV_MAXFREELFCBS_PARMNUM}
  SV_MAXFREEPAGEDPOOLCHUNKS_PARMNUM   = 582;
  {$EXTERNALSYM SV_MAXFREEPAGEDPOOLCHUNKS_PARMNUM}
  SV_MINPAGEDPOOLCHUNKSIZE_PARMNUM    = 583;
  {$EXTERNALSYM SV_MINPAGEDPOOLCHUNKSIZE_PARMNUM}
  SV_MAXPAGEDPOOLCHUNKSIZE_PARMNUM    = 584;
  {$EXTERNALSYM SV_MAXPAGEDPOOLCHUNKSIZE_PARMNUM}
  SV_SENDSFROMPREFERREDPROCESSOR_PARMNUM    = 585;
  {$EXTERNALSYM SV_SENDSFROMPREFERREDPROCESSOR_PARMNUM}
  SV_MAXTHREADSPERQUEUE_PARMNUM       = 586;
  {$EXTERNALSYM SV_MAXTHREADSPERQUEUE_PARMNUM}
  SV_CACHEDDIRECTORYLIMIT_PARMNUM     = 587;
  {$EXTERNALSYM SV_CACHEDDIRECTORYLIMIT_PARMNUM}
  SV_MAXCOPYLENGTH_PARMNUM            = 588;
  {$EXTERNALSYM SV_MAXCOPYLENGTH_PARMNUM}
  SV_ENABLECOMPRESSION_PARMNUM        = 590;
  {$EXTERNALSYM SV_ENABLECOMPRESSION_PARMNUM}
  SV_AUTOSHAREWKS_PARMNUM             = 591;
  {$EXTERNALSYM SV_AUTOSHAREWKS_PARMNUM}
  SV_AUTOSHARESERVER_PARMNUM          = 592;
  {$EXTERNALSYM SV_AUTOSHARESERVER_PARMNUM}
  SV_ENABLESECURITYSIGNATURE_PARMNUM  = 593;
  {$EXTERNALSYM SV_ENABLESECURITYSIGNATURE_PARMNUM}
  SV_REQUIRESECURITYSIGNATURE_PARMNUM = 594;
  {$EXTERNALSYM SV_REQUIRESECURITYSIGNATURE_PARMNUM}
  SV_MINCLIENTBUFFERSIZE_PARMNUM      = 595;
  {$EXTERNALSYM SV_MINCLIENTBUFFERSIZE_PARMNUM}
  SV_CONNECTIONNOSESSIONSTIMEOUT_PARMNUM = 596;
  {$EXTERNALSYM SV_CONNECTIONNOSESSIONSTIMEOUT_PARMNUM}
  SV_IDLETHREADTIMEOUT_PARMNUM        = 597;
  {$EXTERNALSYM SV_IDLETHREADTIMEOUT_PARMNUM}
  SV_ENABLEW9XSECURITYSIGNATURE_PARMNUM        = 598;
  {$EXTERNALSYM SV_ENABLEW9XSECURITYSIGNATURE_PARMNUM}

// Single-field infolevels for NetServerSetInfo.

  SV_COMMENT_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_COMMENT_PARMNUM);
  {$EXTERNALSYM SV_COMMENT_INFOLEVEL}
  SV_USERS_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_USERS_PARMNUM);
  {$EXTERNALSYM SV_USERS_INFOLEVEL}
  SV_DISC_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_DISC_PARMNUM);
  {$EXTERNALSYM SV_DISC_INFOLEVEL}
  SV_HIDDEN_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_HIDDEN_PARMNUM);
  {$EXTERNALSYM SV_HIDDEN_INFOLEVEL}
  SV_ANNOUNCE_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_ANNOUNCE_PARMNUM);
  {$EXTERNALSYM SV_ANNOUNCE_INFOLEVEL}
  SV_ANNDELTA_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_ANNDELTA_PARMNUM);
  {$EXTERNALSYM SV_ANNDELTA_INFOLEVEL}
  SV_SESSOPENS_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_SESSOPENS_PARMNUM);
  {$EXTERNALSYM SV_SESSOPENS_INFOLEVEL}
  SV_SESSVCS_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_SESSVCS_PARMNUM);
  {$EXTERNALSYM SV_SESSVCS_INFOLEVEL}
  SV_OPENSEARCH_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_OPENSEARCH_PARMNUM);
  {$EXTERNALSYM SV_OPENSEARCH_INFOLEVEL}
  SV_MAXWORKITEMS_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_MAXWORKITEMS_PARMNUM);
  {$EXTERNALSYM SV_MAXWORKITEMS_INFOLEVEL}
  SV_MAXRAWBUFLEN_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_MAXRAWBUFLEN_PARMNUM);
  {$EXTERNALSYM SV_MAXRAWBUFLEN_INFOLEVEL}
  SV_SESSUSERS_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_SESSUSERS_PARMNUM);
  {$EXTERNALSYM SV_SESSUSERS_INFOLEVEL}
  SV_SESSCONNS_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_SESSCONNS_PARMNUM);
  {$EXTERNALSYM SV_SESSCONNS_INFOLEVEL}
  SV_MAXNONPAGEDMEMORYUSAGE_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_MAXNONPAGEDMEMORYUSAGE_PARMNUM);
  {$EXTERNALSYM SV_MAXNONPAGEDMEMORYUSAGE_INFOLEVEL}
  SV_MAXPAGEDMEMORYUSAGE_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_MAXPAGEDMEMORYUSAGE_PARMNUM);
  {$EXTERNALSYM SV_MAXPAGEDMEMORYUSAGE_INFOLEVEL}
  SV_ENABLESOFTCOMPAT_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_ENABLESOFTCOMPAT_PARMNUM);
  {$EXTERNALSYM SV_ENABLESOFTCOMPAT_INFOLEVEL}
  SV_ENABLEFORCEDLOGOFF_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_ENABLEFORCEDLOGOFF_PARMNUM);
  {$EXTERNALSYM SV_ENABLEFORCEDLOGOFF_INFOLEVEL}
  SV_TIMESOURCE_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_TIMESOURCE_PARMNUM);
  {$EXTERNALSYM SV_TIMESOURCE_INFOLEVEL}
  SV_LMANNOUNCE_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_LMANNOUNCE_PARMNUM);
  {$EXTERNALSYM SV_LMANNOUNCE_INFOLEVEL}
  SV_MAXCOPYREADLEN_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_MAXCOPYREADLEN_PARMNUM);
  {$EXTERNALSYM SV_MAXCOPYREADLEN_INFOLEVEL}
  SV_MAXCOPYWRITELEN_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_MAXCOPYWRITELEN_PARMNUM);
  {$EXTERNALSYM SV_MAXCOPYWRITELEN_INFOLEVEL}
  SV_MINKEEPSEARCH_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_MINKEEPSEARCH_PARMNUM);
  {$EXTERNALSYM SV_MINKEEPSEARCH_INFOLEVEL}
  SV_MAXKEEPSEARCH_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_MAXKEEPSEARCH_PARMNUM);
  {$EXTERNALSYM SV_MAXKEEPSEARCH_INFOLEVEL}
  SV_MINKEEPCOMPLSEARCH_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_MINKEEPCOMPLSEARCH_PARMNUM);
  {$EXTERNALSYM SV_MINKEEPCOMPLSEARCH_INFOLEVEL}
  SV_MAXKEEPCOMPLSEARCH_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_MAXKEEPCOMPLSEARCH_PARMNUM);
  {$EXTERNALSYM SV_MAXKEEPCOMPLSEARCH_INFOLEVEL}
  SV_SCAVTIMEOUT_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_SCAVTIMEOUT_PARMNUM);
  {$EXTERNALSYM SV_SCAVTIMEOUT_INFOLEVEL}
  SV_MINRCVQUEUE_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_MINRCVQUEUE_PARMNUM);
  {$EXTERNALSYM SV_MINRCVQUEUE_INFOLEVEL}
  SV_MINFREEWORKITEMS_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_MINFREEWORKITEMS_PARMNUM);
  {$EXTERNALSYM SV_MINFREEWORKITEMS_INFOLEVEL}
  SV_MAXMPXCT_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_MAXMPXCT_PARMNUM);
  {$EXTERNALSYM SV_MAXMPXCT_INFOLEVEL}
  SV_OPLOCKBREAKWAIT_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_OPLOCKBREAKWAIT_PARMNUM);
  {$EXTERNALSYM SV_OPLOCKBREAKWAIT_INFOLEVEL}
  SV_OPLOCKBREAKRESPONSEWAIT_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_OPLOCKBREAKRESPONSEWAIT_PARMNUM);
  {$EXTERNALSYM SV_OPLOCKBREAKRESPONSEWAIT_INFOLEVEL}
  SV_ENABLEOPLOCKS_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_ENABLEOPLOCKS_PARMNUM);
  {$EXTERNALSYM SV_ENABLEOPLOCKS_INFOLEVEL}
  SV_ENABLEOPLOCKFORCECLOSE_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_ENABLEOPLOCKFORCECLOSE_PARMNUM);
  {$EXTERNALSYM SV_ENABLEOPLOCKFORCECLOSE_INFOLEVEL}
  SV_ENABLEFCBOPENS_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_ENABLEFCBOPENS_PARMNUM);
  {$EXTERNALSYM SV_ENABLEFCBOPENS_INFOLEVEL}
  SV_ENABLERAW_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_ENABLERAW_PARMNUM);
  {$EXTERNALSYM SV_ENABLERAW_INFOLEVEL}
  SV_ENABLESHAREDNETDRIVES_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_ENABLESHAREDNETDRIVES_PARMNUM);
  {$EXTERNALSYM SV_ENABLESHAREDNETDRIVES_INFOLEVEL}
  SV_MINFREECONNECTIONS_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_MINFREECONNECTIONS_PARMNUM);
  {$EXTERNALSYM SV_MINFREECONNECTIONS_INFOLEVEL}
  SV_MAXFREECONNECTIONS_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_MAXFREECONNECTIONS_PARMNUM);
  {$EXTERNALSYM SV_MAXFREECONNECTIONS_INFOLEVEL}
  SV_INITSESSTABLE_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_INITSESSTABLE_PARMNUM);
  {$EXTERNALSYM SV_INITSESSTABLE_INFOLEVEL}
  SV_INITCONNTABLE_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_INITCONNTABLE_PARMNUM);
  {$EXTERNALSYM SV_INITCONNTABLE_INFOLEVEL}
  SV_INITFILETABLE_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_INITFILETABLE_PARMNUM);
  {$EXTERNALSYM SV_INITFILETABLE_INFOLEVEL}
  SV_INITSEARCHTABLE_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_INITSEARCHTABLE_PARMNUM);
  {$EXTERNALSYM SV_INITSEARCHTABLE_INFOLEVEL}
  SV_ALERTSCHEDULE_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_ALERTSCHEDULE_PARMNUM);
  {$EXTERNALSYM SV_ALERTSCHEDULE_INFOLEVEL}
  SV_ERRORTHRESHOLD_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_ERRORTHRESHOLD_PARMNUM);
  {$EXTERNALSYM SV_ERRORTHRESHOLD_INFOLEVEL}
  SV_NETWORKERRORTHRESHOLD_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_NETWORKERRORTHRESHOLD_PARMNUM);
  {$EXTERNALSYM SV_NETWORKERRORTHRESHOLD_INFOLEVEL}
  SV_DISKSPACETHRESHOLD_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_DISKSPACETHRESHOLD_PARMNUM);
  {$EXTERNALSYM SV_DISKSPACETHRESHOLD_INFOLEVEL}
  SV_MAXLINKDELAY_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_MAXLINKDELAY_PARMNUM);
  {$EXTERNALSYM SV_MAXLINKDELAY_INFOLEVEL}
  SV_MINLINKTHROUGHPUT_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_MINLINKTHROUGHPUT_PARMNUM);
  {$EXTERNALSYM SV_MINLINKTHROUGHPUT_INFOLEVEL}
  SV_LINKINFOVALIDTIME_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_LINKINFOVALIDTIME_PARMNUM);
  {$EXTERNALSYM SV_LINKINFOVALIDTIME_INFOLEVEL}
  SV_SCAVQOSINFOUPDATETIME_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_SCAVQOSINFOUPDATETIME_PARMNUM);
  {$EXTERNALSYM SV_SCAVQOSINFOUPDATETIME_INFOLEVEL}
  SV_MAXWORKITEMIDLETIME_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_MAXWORKITEMIDLETIME_PARMNUM);
  {$EXTERNALSYM SV_MAXWORKITEMIDLETIME_INFOLEVEL}
  SV_MAXRAWWORKITEMS_INFOLOEVEL = (PARMNUM_BASE_INFOLEVEL + SV_MAXRAWWORKITEMS_PARMNUM);
  {$EXTERNALSYM SV_MAXRAWWORKITEMS_INFOLOEVEL}
  SV_PRODUCTTYPE_INFOLOEVEL = (PARMNUM_BASE_INFOLEVEL + SV_PRODUCTTYPE_PARMNUM);
  {$EXTERNALSYM SV_PRODUCTTYPE_INFOLOEVEL}
  SV_SERVERSIZE_INFOLOEVEL = (PARMNUM_BASE_INFOLEVEL + SV_SERVERSIZE_PARMNUM);
  {$EXTERNALSYM SV_SERVERSIZE_INFOLOEVEL}
  SV_CONNECTIONLESSAUTODISC_INFOLOEVEL = (PARMNUM_BASE_INFOLEVEL + SV_CONNECTIONLESSAUTODISC_PARMNUM);
  {$EXTERNALSYM SV_CONNECTIONLESSAUTODISC_INFOLOEVEL}
  SV_SHARINGVIOLATIONRETRIES_INFOLOEVEL = (PARMNUM_BASE_INFOLEVEL + SV_SHARINGVIOLATIONRETRIES_PARMNUM);
  {$EXTERNALSYM SV_SHARINGVIOLATIONRETRIES_INFOLOEVEL}
  SV_SHARINGVIOLATIONDELAY_INFOLOEVEL = (PARMNUM_BASE_INFOLEVEL + SV_SHARINGVIOLATIONDELAY_PARMNUM);
  {$EXTERNALSYM SV_SHARINGVIOLATIONDELAY_INFOLOEVEL}
  SV_MAXGLOBALOPENSEARCH_INFOLOEVEL = (PARMNUM_BASE_INFOLEVEL + SV_MAXGLOBALOPENSEARCH_PARMNUM);
  {$EXTERNALSYM SV_MAXGLOBALOPENSEARCH_INFOLOEVEL}
  SV_REMOVEDUPLICATESEARCHES_INFOLOEVEL = (PARMNUM_BASE_INFOLEVEL + SV_REMOVEDUPLICATESEARCHES_PARMNUM);
  {$EXTERNALSYM SV_REMOVEDUPLICATESEARCHES_INFOLOEVEL}
  SV_LOCKVIOLATIONRETRIES_INFOLOEVEL = (PARMNUM_BASE_INFOLEVEL + SV_LOCKVIOLATIONRETRIES_PARMNUM);
  {$EXTERNALSYM SV_LOCKVIOLATIONRETRIES_INFOLOEVEL}
  SV_LOCKVIOLATIONOFFSET_INFOLOEVEL = (PARMNUM_BASE_INFOLEVEL + SV_LOCKVIOLATIONOFFSET_PARMNUM);
  {$EXTERNALSYM SV_LOCKVIOLATIONOFFSET_INFOLOEVEL}
  SV_LOCKVIOLATIONDELAY_INFOLOEVEL = (PARMNUM_BASE_INFOLEVEL + SV_LOCKVIOLATIONDELAY_PARMNUM);
  {$EXTERNALSYM SV_LOCKVIOLATIONDELAY_INFOLOEVEL}
  SV_MDLREADSWITCHOVER_INFOLOEVEL = (PARMNUM_BASE_INFOLEVEL + SV_MDLREADSWITCHOVER_PARMNUM);
  {$EXTERNALSYM SV_MDLREADSWITCHOVER_INFOLOEVEL}
  SV_CACHEDOPENLIMIT_INFOLOEVEL = (PARMNUM_BASE_INFOLEVEL + SV_CACHEDOPENLIMIT_PARMNUM);
  {$EXTERNALSYM SV_CACHEDOPENLIMIT_INFOLOEVEL}
  SV_CRITICALTHREADS_INFOLOEVEL = (PARMNUM_BASE_INFOLEVEL + SV_CRITICALTHREADS_PARMNUM);
  {$EXTERNALSYM SV_CRITICALTHREADS_INFOLOEVEL}
  SV_RESTRICTNULLSESSACCESS_INFOLOEVEL = (PARMNUM_BASE_INFOLEVEL + SV_RESTRICTNULLSESSACCESS_PARMNUM);
  {$EXTERNALSYM SV_RESTRICTNULLSESSACCESS_INFOLOEVEL}
  SV_ENABLEWFW311DIRECTIPX_INFOLOEVEL = (PARMNUM_BASE_INFOLEVEL + SV_ENABLEWFW311DIRECTIPX_PARMNUM);
  {$EXTERNALSYM SV_ENABLEWFW311DIRECTIPX_INFOLOEVEL}
  SV_OTHERQUEUEAFFINITY_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_OTHERQUEUEAFFINITY_PARMNUM);
  {$EXTERNALSYM SV_OTHERQUEUEAFFINITY_INFOLEVEL}
  SV_QUEUESAMPLESECS_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_QUEUESAMPLESECS_PARMNUM);
  {$EXTERNALSYM SV_QUEUESAMPLESECS_INFOLEVEL}
  SV_BALANCECOUNT_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_BALANCECOUNT_PARMNUM);
  {$EXTERNALSYM SV_BALANCECOUNT_INFOLEVEL}
  SV_PREFERREDAFFINITY_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_PREFERREDAFFINITY_PARMNUM);
  {$EXTERNALSYM SV_PREFERREDAFFINITY_INFOLEVEL}
  SV_MAXFREERFCBS_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_MAXFREERFCBS_PARMNUM);
  {$EXTERNALSYM SV_MAXFREERFCBS_INFOLEVEL}
  SV_MAXFREEMFCBS_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_MAXFREEMFCBS_PARMNUM);
  {$EXTERNALSYM SV_MAXFREEMFCBS_INFOLEVEL}
  SV_MAXFREELFCBS_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_MAXFREELFCBS_PARMNUM);
  {$EXTERNALSYM SV_MAXFREELFCBS_INFOLEVEL}
  SV_MAXFREEPAGEDPOOLCHUNKS_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_MAXFREEPAGEDPOOLCHUNKS_PARMNUM);
  {$EXTERNALSYM SV_MAXFREEPAGEDPOOLCHUNKS_INFOLEVEL}
  SV_MINPAGEDPOOLCHUNKSIZE_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_MINPAGEDPOOLCHUNKSIZE_PARMNUM);
  {$EXTERNALSYM SV_MINPAGEDPOOLCHUNKSIZE_INFOLEVEL}
  SV_MAXPAGEDPOOLCHUNKSIZE_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_MAXPAGEDPOOLCHUNKSIZE_PARMNUM);
  {$EXTERNALSYM SV_MAXPAGEDPOOLCHUNKSIZE_INFOLEVEL}
  SV_SENDSFROMPREFERREDPROCESSOR_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_SENDSFROMPREFERREDPROCESSOR_PARMNUM);
  {$EXTERNALSYM SV_SENDSFROMPREFERREDPROCESSOR_INFOLEVEL}
  SV_MAXTHREADSPERQUEUE_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_MAXTHREADSPERQUEUE_PARMNUM);
  {$EXTERNALSYM SV_MAXTHREADSPERQUEUE_INFOLEVEL}
  SV_CACHEDDIRECTORYLIMIT_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_CACHEDDIRECTORYLIMIT_PARMNUM);
  {$EXTERNALSYM SV_CACHEDDIRECTORYLIMIT_INFOLEVEL}
  SV_MAXCOPYLENGTH_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_MAXCOPYLENGTH_PARMNUM);
  {$EXTERNALSYM SV_MAXCOPYLENGTH_INFOLEVEL}
  SV_ENABLECOMPRESSION_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_ENABLECOMPRESSION_PARMNUM);
  {$EXTERNALSYM SV_ENABLECOMPRESSION_INFOLEVEL}
  SV_AUTOSHAREWKS_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_AUTOSHAREWKS_PARMNUM);
  {$EXTERNALSYM SV_AUTOSHAREWKS_INFOLEVEL}
  SV_AUTOSHARESERVER_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_AUTOSHARESERVER_PARMNUM);
  {$EXTERNALSYM SV_AUTOSHARESERVER_INFOLEVEL}
  SV_ENABLESECURITYSIGNATURE_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_ENABLESECURITYSIGNATURE_PARMNUM);
  {$EXTERNALSYM SV_ENABLESECURITYSIGNATURE_INFOLEVEL}
  SV_REQUIRESECURITYSIGNATURE_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_REQUIRESECURITYSIGNATURE_PARMNUM);
  {$EXTERNALSYM SV_REQUIRESECURITYSIGNATURE_INFOLEVEL}
  SV_MINCLIENTBUFFERSIZE_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_MINCLIENTBUFFERSIZE_PARMNUM);
  {$EXTERNALSYM SV_MINCLIENTBUFFERSIZE_INFOLEVEL}
  SV_CONNECTIONNOSESSIONSTIMEOUT_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_CONNECTIONNOSESSIONSTIMEOUT_PARMNUM);
  {$EXTERNALSYM SV_CONNECTIONNOSESSIONSTIMEOUT_INFOLEVEL}
  SV_IDLETHREADTIMEOUT_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_IDLETHREADTIMEOUT_PARMNUM);
  {$EXTERNALSYM SV_IDLETHREADTIMEOUT_INFOLEVEL}
  SV_ENABLEW9XSECURITYSIGNATURE_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_ENABLEW9XSECURITYSIGNATURE_PARMNUM);
  {$EXTERNALSYM SV_ENABLEW9XSECURITYSIGNATURE_INFOLEVEL}

  SVI1_NUM_ELEMENTS       = 5;
  {$EXTERNALSYM SVI1_NUM_ELEMENTS}
  SVI2_NUM_ELEMENTS       = 40;
  {$EXTERNALSYM SVI2_NUM_ELEMENTS}
  SVI3_NUM_ELEMENTS       = 44;
  {$EXTERNALSYM SVI3_NUM_ELEMENTS}

// Maxmimum length for command string to NetServerAdminCommand.

  SV_MAX_CMD_LEN          = PATHLEN;
  {$EXTERNALSYM SV_MAX_CMD_LEN}

// Masks describing AUTOPROFILE parameters

  SW_AUTOPROF_LOAD_MASK   = $1;
  {$EXTERNALSYM SW_AUTOPROF_LOAD_MASK}
  SW_AUTOPROF_SAVE_MASK   = $2;
  {$EXTERNALSYM SW_AUTOPROF_SAVE_MASK}

// Max size of svX_srvheuristics.

  SV_MAX_SRV_HEUR_LEN     = 32;      // Max heuristics info string length.
  {$EXTERNALSYM SV_MAX_SRV_HEUR_LEN}

// Equate for use with sv102_licenses.

  SV_USERS_PER_LICENSE    = 5;
  {$EXTERNALSYM SV_USERS_PER_LICENSE}

// Equate for use with svti2_flags in NetServerTransportAddEx.

  SVTI2_REMAP_PIPE_NAMES  = $2;
  {$EXTERNALSYM SVTI2_REMAP_PIPE_NAMES}

implementation

function NetServerEnum; external netapi32lib name 'NetServerEnum';
function NetServerEnumEx; external netapi32lib name 'NetServerEnumEx';
function NetServerGetInfo; external netapi32lib name 'NetServerGetInfo';
function NetServerSetInfo; external netapi32lib name 'NetServerSetInfo';
function NetServerSetInfoCommandLine; external netapi32lib name 'NetServerSetInfoCommandLine';
function NetServerDiskEnum; external netapi32lib name 'NetServerDiskEnum';
function NetServerComputerNameAdd; external netapi32lib name 'NetServerComputerNameAdd';
function NetServerComputerNameDel; external netapi32lib name 'NetServerComputerNameDel';
function NetServerTransportAdd; external netapi32lib name 'NetServerTransportAdd';
function NetServerTransportAddEx; external netapi32lib name 'NetServerTransportAddEx';
function NetServerTransportDel; external netapi32lib name 'NetServerTransportDel';
function NetServerTransportEnum; external netapi32lib name 'NetServerTransportEnum';
function SetServiceBits; external netapi32lib name 'SetServiceBits';

end.
