{******************************************************************}
{                                                                  }
{ Borland Delphi Runtime Library                                   }
{ LanManager share functions for Windows NT interface unit         }
{                                                                  }
{ Portions created by Microsoft are                                }
{ Copyright (C) 1995-1999 Microsoft Corporation.                   }
{ All Rights Reserved.                                             }
{                                                                  }
{ The original file is: lmsvc.h, released 14 Nov 1998.             }
{ The original Pascal code is: LmSvc.pas, released 14 Jan 2000.    }
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

unit LmSvc;

{$I LANMAN.INC}

{$ALIGN ON}
{$MINENUMSIZE 4}
{$WEAKPACKAGEUNIT}

interface

uses
  Windows, LmCons, LmSname;

(*$HPPEMIT '#include <netcons.h>'*)
(*$HPPEMIT '#include <lmsname.h>'*)
(*$HPPEMIT '#include <lmsvc.h>'*)

type
  PServiceInfo0 = ^TServiceInfo0;
  _SERVICE_INFO_0 = record
    svci0_name: LPWSTR;
  end;
  {$EXTERNALSYM _SERVICE_INFO_0}
  TServiceInfo0 = _SERVICE_INFO_0;
  SERVICE_INFO_0 = _SERVICE_INFO_0;
  {$EXTERNALSYM SERVICE_INFO_0}

  PServiceInfo1 = ^TServiceInfo1;
  _SERVICE_INFO_1 = record
    svci1_name: LPWSTR;
    svci1_status: DWORD;
    svci1_code: DWORD;
    svci1_pid: DWORD;
  end;
  {$EXTERNALSYM _SERVICE_INFO_1}
  TServiceInfo1 = _SERVICE_INFO_1;
  SERVICE_INFO_1 = _SERVICE_INFO_1;
  {$EXTERNALSYM SERVICE_INFO_1}

  PServiceInfo2 = ^TServiceInfo2;
  _SERVICE_INFO_2 = record
    svci2_name: LPWSTR;
    svci2_status: DWORD;
    svci2_code: DWORD;
    svci2_pid: DWORD;
    svci2_text: LPWSTR;
    svci2_specific_error: DWORD;
    svci2_display_name: LPWSTR;
  end;
  {$EXTERNALSYM _SERVICE_INFO_2}
  TServiceInfo2 = _SERVICE_INFO_2;
  SERVICE_INFO_2 = _SERVICE_INFO_2;
  {$EXTERNALSYM SERVICE_INFO_2}


function NetServiceControl(servername: LPCWSTR; service: LPCWSTR; opcode: DWORD;
  arg: DWORD; bufptr: Pointer): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetServiceControl}

function NetServiceEnum(servername: LPCWSTR; level: DWORD; bufptr: Pointer;
  prefmaxlen: DWORD; var entriesread: DWORD; var totalentries: DWORD;
  resume_handle: PDWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetServiceEnum}

function NetServiceGetInfo(servername: LPCWSTR; service: LPCWSTR; level: DWORD;
  bufptr: Pointer): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetServiceGetInfo}

function NetServiceInstall(servername: LPCWSTR; service: LPCWSTR; argc: DWORD;
  argv: LPCWSTR; bufptr: Pointer): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetServiceInstall}

//  Bitmask and bit values for svci1_status, and svci2_status
//  fields.  For each "subfield", there is a mask defined,
//  and a number of constants representing the value
//  obtained by doing (status & mask).

const
// Bits 0,1 -- general status

  SERVICE_INSTALL_STATE       = $03;
  {$EXTERNALSYM SERVICE_INSTALL_STATE}
  SERVICE_UNINSTALLED         = $00;
  {$EXTERNALSYM SERVICE_UNINSTALLED}
  SERVICE_INSTALL_PENDING     = $01;
  {$EXTERNALSYM SERVICE_INSTALL_PENDING}
  SERVICE_UNINSTALL_PENDING   = $02;
  {$EXTERNALSYM SERVICE_UNINSTALL_PENDING}
  SERVICE_INSTALLED           = $03;
  {$EXTERNALSYM SERVICE_INSTALLED}

// Bits 2,3 -- paused/active status

  SERVICE_PAUSE_STATE              = $0C;
  {$EXTERNALSYM SERVICE_PAUSE_STATE}
  LM20_SERVICE_ACTIVE              = $00;
  {$EXTERNALSYM LM20_SERVICE_ACTIVE}
  LM20_SERVICE_CONTINUE_PENDING    = $04;
  {$EXTERNALSYM LM20_SERVICE_CONTINUE_PENDING}
  LM20_SERVICE_PAUSE_PENDING       = $08;
  {$EXTERNALSYM LM20_SERVICE_PAUSE_PENDING}
  LM20_SERVICE_PAUSED              = $0C;
  {$EXTERNALSYM LM20_SERVICE_PAUSED}

// Bit 4 -- uninstallable indication

  SERVICE_NOT_UNINSTALLABLE   = $00;
  {$EXTERNALSYM SERVICE_NOT_UNINSTALLABLE}
  SERVICE_UNINSTALLABLE       = $10;
  {$EXTERNALSYM SERVICE_UNINSTALLABLE}

// Bit 5 -- pausable indication

  SERVICE_NOT_PAUSABLE        = $00;
  {$EXTERNALSYM SERVICE_NOT_PAUSABLE}
  SERVICE_PAUSABLE            = $20;
  {$EXTERNALSYM SERVICE_PAUSABLE}

// Workstation service only:
// Bits 8,9,10 -- redirection paused/active

  SERVICE_REDIR_PAUSED        = $700;
  {$EXTERNALSYM SERVICE_REDIR_PAUSED}
  SERVICE_REDIR_DISK_PAUSED   = $100;
  {$EXTERNALSYM SERVICE_REDIR_DISK_PAUSED}
  SERVICE_REDIR_PRINT_PAUSED  = $200;
  {$EXTERNALSYM SERVICE_REDIR_PRINT_PAUSED}
  SERVICE_REDIR_COMM_PAUSED   = $400;
  {$EXTERNALSYM SERVICE_REDIR_COMM_PAUSED}

// Additional standard LAN Manager for MS-DOS services

  SERVICE_DOS_ENCRYPTION  = 'ENCRYPT';
  {$EXTERNALSYM SERVICE_DOS_ENCRYPTION}

// NetServiceControl opcodes.

  SERVICE_CTRL_INTERROGATE    = 0;
  {$EXTERNALSYM SERVICE_CTRL_INTERROGATE}
  SERVICE_CTRL_PAUSE          = 1;
  {$EXTERNALSYM SERVICE_CTRL_PAUSE}
  SERVICE_CTRL_CONTINUE       = 2;
  {$EXTERNALSYM SERVICE_CTRL_CONTINUE}
  SERVICE_CTRL_UNINSTALL      = 3;
  {$EXTERNALSYM SERVICE_CTRL_UNINSTALL}

// Workstation service only:  Bits used in the "arg" parameter
// to NetServiceControl in conjunction with the opcode
// SERVICE_CTRL_PAUSE or SERVICE_CTRL_CONTINUE, to pause or
// continue redirection.

  SERVICE_CTRL_REDIR_DISK     = $1;
  {$EXTERNALSYM SERVICE_CTRL_REDIR_DISK}
  SERVICE_CTRL_REDIR_PRINT    = $2;
  {$EXTERNALSYM SERVICE_CTRL_REDIR_PRINT}
  SERVICE_CTRL_REDIR_COMM     = $4;
  {$EXTERNALSYM SERVICE_CTRL_REDIR_COMM}

// Values for svci1_code, and svci2_code when status
// of the service is SERVICE_INSTALL_PENDING or
// SERVICE_UNINSTALL_PENDING.
// A service can optionally provide a hint to the installer
// that the install is proceeding and how long to wait
// (in 0.1 second increments) before querying status again.

  SERVICE_IP_NO_HINT          = $0;
  {$EXTERNALSYM SERVICE_IP_NO_HINT}
  SERVICE_CCP_NO_HINT         = $0;
  {$EXTERNALSYM SERVICE_CCP_NO_HINT}

  SERVICE_IP_QUERY_HINT       = $10000;
  {$EXTERNALSYM SERVICE_IP_QUERY_HINT}
  SERVICE_CCP_QUERY_HINT      = $10000;
  {$EXTERNALSYM SERVICE_CCP_QUERY_HINT}

// Mask for install proceeding checkpoint number

  SERVICE_IP_CHKPT_NUM        = $0FF;
  {$EXTERNALSYM SERVICE_IP_CHKPT_NUM}
  SERVICE_CCP_CHKPT_NUM       = $0FF;
  {$EXTERNALSYM SERVICE_CCP_CHKPT_NUM}

// Mask for wait time hint before querying again

  SERVICE_IP_WAIT_TIME        = $0FF00;
  {$EXTERNALSYM SERVICE_IP_WAIT_TIME}
  SERVICE_CCP_WAIT_TIME       = $0FF00;
  {$EXTERNALSYM SERVICE_CCP_WAIT_TIME}

// Shift count for building wait time _code values

  SERVICE_IP_WAITTIME_SHIFT   = 8;
  {$EXTERNALSYM SERVICE_IP_WAITTIME_SHIFT}
  SERVICE_NTIP_WAITTIME_SHIFT = 12;
  {$EXTERNALSYM SERVICE_NTIP_WAITTIME_SHIFT}

// Mask used for upper and lower portions of wait hint time.

  UPPER_HINT_MASK     = $0000FF00;
  {$EXTERNALSYM UPPER_HINT_MASK}
  LOWER_HINT_MASK     = $000000FF;
  {$EXTERNALSYM LOWER_HINT_MASK}
  UPPER_GET_HINT_MASK = $0FF00000;
  {$EXTERNALSYM UPPER_GET_HINT_MASK}
  LOWER_GET_HINT_MASK = $0000FF00;
  {$EXTERNALSYM LOWER_GET_HINT_MASK}
  SERVICE_NT_MAXTIME  = $0000FFFF;
  {$EXTERNALSYM SERVICE_NT_MAXTIME}
  SERVICE_RESRV_MASK  = $0001FFFF;
  {$EXTERNALSYM SERVICE_RESRV_MASK}
  SERVICE_MAXTIME     = $000000FF;
  {$EXTERNALSYM SERVICE_MAXTIME}

// SERVICE_BASE is the base of service error codes,
// chosen to avoid conflict with OS, redirector,
// netapi, and errlog codes.
//
// Don't change the comments following the manifest constants without
// understanding how mapmsg works.

  SERVICE_BASE                = 3050;
  {$EXTERNALSYM SERVICE_BASE}
  SERVICE_UIC_NORMAL          = 0;
  {$EXTERNALSYM SERVICE_UIC_NORMAL}
  // Uninstall codes, to be used in high byte of 'code' on final NetStatus,
  // which sets the status to UNINSTALLED.

  SERVICE_UIC_BADPARMVAL         = (SERVICE_BASE + 1);
  {$EXTERNALSYM SERVICE_UIC_BADPARMVAL}
  // The Registry or the information you just typed includes an illegal
  // value for "%1".

  SERVICE_UIC_MISSPARM           = (SERVICE_BASE + 2);
  {$EXTERNALSYM SERVICE_UIC_MISSPARM}
  // The required parameter was not provided on the command
  // line or in the configuration file.

  SERVICE_UIC_UNKPARM            = (SERVICE_BASE + 3);
  {$EXTERNALSYM SERVICE_UIC_UNKPARM}
  // LAN Manager does not recognize "%1" as a valid option.

  SERVICE_UIC_RESOURCE           = (SERVICE_BASE + 4);
  {$EXTERNALSYM SERVICE_UIC_RESOURCE}
  // A request for resource could not be satisfied.

  SERVICE_UIC_CONFIG             = (SERVICE_BASE + 5);
  {$EXTERNALSYM SERVICE_UIC_CONFIG}
  // A problem exists with the system configuration.

  SERVICE_UIC_SYSTEM             = (SERVICE_BASE + 6);
  {$EXTERNALSYM SERVICE_UIC_SYSTEM}
  // A system error has occurred.

  SERVICE_UIC_INTERNAL           = (SERVICE_BASE + 7);
  {$EXTERNALSYM SERVICE_UIC_INTERNAL}
  // An internal consistency error has occurred.

  SERVICE_UIC_AMBIGPARM          = (SERVICE_BASE + 8);
  {$EXTERNALSYM SERVICE_UIC_AMBIGPARM}
  // The configuration file or the command line has an ambiguous option.

  SERVICE_UIC_DUPPARM            = (SERVICE_BASE + 9);
  {$EXTERNALSYM SERVICE_UIC_DUPPARM}
  // The configuration file or the command line has a duplicate parameter.

  SERVICE_UIC_KILL               = (SERVICE_BASE + 10);
  {$EXTERNALSYM SERVICE_UIC_KILL}
  // The service did not respond to control and was stopped with
  // the DosKillProc function.

  SERVICE_UIC_EXEC               = (SERVICE_BASE + 11);
  {$EXTERNALSYM SERVICE_UIC_EXEC}
  // An error occurred when attempting to run the service program.

  SERVICE_UIC_SUBSERV            = (SERVICE_BASE + 12);
  {$EXTERNALSYM SERVICE_UIC_SUBSERV}
  // The sub-service failed to start.

  SERVICE_UIC_CONFLPARM          = (SERVICE_BASE + 13);
  {$EXTERNALSYM SERVICE_UIC_CONFLPARM}
  // There is a conflict in the value or use of these options: %1.

  SERVICE_UIC_FILE               = (SERVICE_BASE + 14);
  {$EXTERNALSYM SERVICE_UIC_FILE}
  // There is a problem with the file.

// The modifiers

// General:

  SERVICE_UIC_M_NULL   = 0;
  {$EXTERNALSYM SERVICE_UIC_M_NULL}

// RESOURCE:

  SERVICE_UIC_M_MEMORY    = (SERVICE_BASE + 20);     //* memory */
  {$EXTERNALSYM SERVICE_UIC_M_MEMORY}
  SERVICE_UIC_M_DISK      = (SERVICE_BASE + 21);     //* disk space */
  {$EXTERNALSYM SERVICE_UIC_M_DISK}
  SERVICE_UIC_M_THREADS   = (SERVICE_BASE + 22);     //* thread */
  {$EXTERNALSYM SERVICE_UIC_M_THREADS}
  SERVICE_UIC_M_PROCESSES = (SERVICE_BASE + 23);     //* process */
  {$EXTERNALSYM SERVICE_UIC_M_PROCESSES}

// CONFIG:

// Security failure

  SERVICE_UIC_M_SECURITY          = (SERVICE_BASE + 24);
  {$EXTERNALSYM SERVICE_UIC_M_SECURITY}
  // Security Failure.

  SERVICE_UIC_M_LANROOT           = (SERVICE_BASE + 25);
  {$EXTERNALSYM SERVICE_UIC_M_LANROOT}
  // Bad or missing LAN Manager root directory.

  SERVICE_UIC_M_REDIR             = (SERVICE_BASE + 26);
  {$EXTERNALSYM SERVICE_UIC_M_REDIR}
  // The network software is not installed.

  SERVICE_UIC_M_SERVER            = (SERVICE_BASE + 27);
  {$EXTERNALSYM SERVICE_UIC_M_SERVER}
  // The server is not started.

  SERVICE_UIC_M_SEC_FILE_ERR      = (SERVICE_BASE + 28);
  {$EXTERNALSYM SERVICE_UIC_M_SEC_FILE_ERR}
  // The server cannot access the user accounts database (NET.ACC).

  SERVICE_UIC_M_FILES             = (SERVICE_BASE + 29);
  {$EXTERNALSYM SERVICE_UIC_M_FILES}
  // Incompatible files are installed in the LANMAN tree.

  SERVICE_UIC_M_LOGS              = (SERVICE_BASE + 30);
  {$EXTERNALSYM SERVICE_UIC_M_LOGS}
  // The LANMAN\LOGS directory is invalid.

  SERVICE_UIC_M_LANGROUP          = (SERVICE_BASE + 31);
  {$EXTERNALSYM SERVICE_UIC_M_LANGROUP}
  // The domain specified could not be used.

  SERVICE_UIC_M_MSGNAME           = (SERVICE_BASE + 32);
  {$EXTERNALSYM SERVICE_UIC_M_MSGNAME}
  // The computer name is being used as a message alias on another computer.

  SERVICE_UIC_M_ANNOUNCE          = (SERVICE_BASE + 33);
  {$EXTERNALSYM SERVICE_UIC_M_ANNOUNCE}
  // The announcement of the server name failed.

  SERVICE_UIC_M_UAS               = (SERVICE_BASE + 34);
  {$EXTERNALSYM SERVICE_UIC_M_UAS}
  // The user accounts database is not configured correctly.

  SERVICE_UIC_M_SERVER_SEC_ERR    = (SERVICE_BASE + 35);
  {$EXTERNALSYM SERVICE_UIC_M_SERVER_SEC_ERR}
  // The server is not running with user-level security.

  SERVICE_UIC_M_WKSTA             = (SERVICE_BASE + 37);
  {$EXTERNALSYM SERVICE_UIC_M_WKSTA}
  // The workstation is not configured properly.

  SERVICE_UIC_M_ERRLOG            = (SERVICE_BASE + 38);
  {$EXTERNALSYM SERVICE_UIC_M_ERRLOG}
  // View your error log for details.

  SERVICE_UIC_M_FILE_UW           = (SERVICE_BASE + 39);
  {$EXTERNALSYM SERVICE_UIC_M_FILE_UW}
  // Unable to write to this file.

  SERVICE_UIC_M_ADDPAK            = (SERVICE_BASE + 40);
  {$EXTERNALSYM SERVICE_UIC_M_ADDPAK}
  // ADDPAK file is corrupted.  Delete LANMAN\NETPROG\ADDPAK.SER
  // and reapply all ADDPAKs.

  SERVICE_UIC_M_LAZY              = (SERVICE_BASE + 41);
  {$EXTERNALSYM SERVICE_UIC_M_LAZY}
  // The LM386 server cannot be started because CACHE.EXE is not running.

  SERVICE_UIC_M_UAS_MACHINE_ACCT  = (SERVICE_BASE + 42);
  {$EXTERNALSYM SERVICE_UIC_M_UAS_MACHINE_ACCT}
  // There is no account for this computer in the security database.

  SERVICE_UIC_M_UAS_SERVERS_NMEMB = (SERVICE_BASE + 43);
  {$EXTERNALSYM SERVICE_UIC_M_UAS_SERVERS_NMEMB}
  // This computer is not a member of the group SERVERS.

  SERVICE_UIC_M_UAS_SERVERS_NOGRP = (SERVICE_BASE + 44);
  {$EXTERNALSYM SERVICE_UIC_M_UAS_SERVERS_NOGRP}
  // The group SERVERS is not present in the local security database.

  SERVICE_UIC_M_UAS_INVALID_ROLE  = (SERVICE_BASE + 45);
  {$EXTERNALSYM SERVICE_UIC_M_UAS_INVALID_ROLE}
  // This Windows NT computer is configured as a member of a workgroup, not as
  // a member of a domain. The Netlogon service does not need to run in this
  // configuration.

  SERVICE_UIC_M_NETLOGON_NO_DC    = (SERVICE_BASE + 46);
  {$EXTERNALSYM SERVICE_UIC_M_NETLOGON_NO_DC}
  // The Windows NT domain controller for this domain could not be located.

  SERVICE_UIC_M_NETLOGON_DC_CFLCT = (SERVICE_BASE + 47);
  {$EXTERNALSYM SERVICE_UIC_M_NETLOGON_DC_CFLCT}
  // This computer is configured to be the primary domain controller of its domain.
  // However, the computer %1 is currently claiming to be the primary domain controller
  // of the domain.

  SERVICE_UIC_M_NETLOGON_AUTH     = (SERVICE_BASE + 48);
  {$EXTERNALSYM SERVICE_UIC_M_NETLOGON_AUTH}
  // The service failed to authenticate with the primary domain controller.

  SERVICE_UIC_M_UAS_PROLOG        = (SERVICE_BASE + 49);
  {$EXTERNALSYM SERVICE_UIC_M_UAS_PROLOG}
  // There is a problem with the security database creation date or serial number.

  SERVICE2_BASE    = 5600;
  {$EXTERNALSYM SERVICE2_BASE}
  // new SEVICE_UIC messages go here

  SERVICE_UIC_M_NETLOGON_MPATH    = (SERVICE2_BASE + 0);
  {$EXTERNALSYM SERVICE_UIC_M_NETLOGON_MPATH}
  // Could not share the User or Script path.

  SERVICE_UIC_M_LSA_MACHINE_ACCT  = (SERVICE2_BASE + 1);
  {$EXTERNALSYM SERVICE_UIC_M_LSA_MACHINE_ACCT}
  // The password for this computer is not found in the local security
  // database.

  SERVICE_UIC_M_DATABASE_ERROR    = (SERVICE2_BASE + 2);
  {$EXTERNALSYM SERVICE_UIC_M_DATABASE_ERROR}
  // An internal error occurred while accessing the computer's
  // local or network security database.

// End modifiers

// Commonly used Macros:

function SERVICE_IP_CODE(tt, nn: LongInt): LongInt;
{$EXTERNALSYM SERVICE_IP_CODE}

function SERVICE_CCP_CODE(tt, nn: LongInt): LongInt;
{$EXTERNALSYM SERVICE_CCP_CODE}

function SERVICE_UIC_CODE(cc, mm: LongInt): LongInt;
{$EXTERNALSYM SERVICE_UIC_CODE}

// This macro takes a wait hint (tt) which can have a maximum value of
// 0xFFFF and puts it into the service status code field.
// 0x0FF1FFnn  (where nn is the checkpoint information).

function SERVICE_NT_CCP_CODE(tt, nn: LongInt): LongInt;
{$EXTERNALSYM SERVICE_NT_CCP_CODE}

// This macro takes a status code field, and strips out the wait hint
// from the upper and lower sections.
// 0x0FF1FFnn results in 0x0000FFFF.

function SERVICE_NT_WAIT_GET(code: DWORD): DWORD;
{$EXTERNALSYM SERVICE_NT_WAIT_GET}

implementation

function SERVICE_IP_CODE(tt, nn: LongInt): LongInt;
begin
  Result := SERVICE_IP_QUERY_HINT or (nn or tt shl SERVICE_IP_WAITTIME_SHIFT);
end;

function SERVICE_CCP_CODE(tt, nn: LongInt): LongInt;
begin
  Result := SERVICE_CCP_QUERY_HINT or (nn or tt shl SERVICE_IP_WAITTIME_SHIFT);
end;

function SERVICE_UIC_CODE(cc, mm: LongInt): LongInt;
begin
  Result := (cc shl 16) or Word(mm);
end;

function SERVICE_NT_CCP_CODE(tt, nn: LongInt): LongInt;
begin
  Result := nn or ((tt and LOWER_HINT_MASK) shl SERVICE_IP_WAITTIME_SHIFT) or
    ((tt and UPPER_HINT_MASK) shl SERVICE_NTIP_WAITTIME_SHIFT);
end;

function SERVICE_NT_WAIT_GET(code: DWORD): DWORD;
begin
  Result := ((code and UPPER_GET_HINT_MASK) shr SERVICE_NTIP_WAITTIME_SHIFT) or
    ((code and LOWER_GET_HINT_MASK) shr SERVICE_IP_WAITTIME_SHIFT);
end;

function NetServiceControl; external netapi32lib name 'NetServiceControl';
function NetServiceEnum; external netapi32lib name 'NetServiceEnum';
function NetServiceGetInfo; external netapi32lib name 'NetServiceGetInfo';
function NetServiceInstall; external netapi32lib name 'NetServiceInstall';

end.
