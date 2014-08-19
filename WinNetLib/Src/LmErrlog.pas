{******************************************************************}
{                                                                  }
{ Borland Delphi Runtime Library                                   }
{ LanManager share functions for Windows NT interface unit         }
{                                                                  }
{ Portions created by Microsoft are                                }
{ Copyright (C) 1995-1999 Microsoft Corporation.                   }
{ All Rights Reserved.                                             }
{                                                                  }
{ The original file is: lmerrlog.h, released 14 Nov 1998.          }
{ The original Pascal code is: LmErrlog.pas, released 17 Jan 2000. }
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

unit LmErrlog;

{$I LANMAN.INC}

{$ALIGN ON}
{$MINENUMSIZE 4}
{$WEAKPACKAGEUNIT}

interface

uses
  Windows, LmCons, LmAudit;

(*$HPPEMIT '#include <netcons.h>'*)
(*$HPPEMIT '#include <lmerrlog.h>'*)

// Data Structures - Config

type
  PErrorLog = ^TErrorLog;
  _ERROR_LOG = record
    el_len: DWORD;
    el_reserved: DWORD;
    el_time: DWORD;
    el_error: DWORD;
    el_name: LPWSTR;                    // pointer to service name
    el_text: LPWSTR;                    // pointer to string array
    el_data: Pointer;                   // pointer to BYTE array
    el_data_size: DWORD;                // byte count of el_data area
    el_nstrings: DWORD;                 // number of strings in el_text.
  end;
  {$EXTERNALSYM _ERROR_LOG}
  TErrorLog = _ERROR_LOG;
  ERROR_LOG = _ERROR_LOG;
  {$EXTERNALSYM ERROR_LOG}

// Function Prototypes - ErrorLog

function NetErrorLogClear(server: LPCWSTR; backupfile: LPCWSTR;
  reserved: Pointer): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetErrorLogClear}

function NetErrorLogRead(server: LPCWSTR; reserved1: LPWSTR; errloghandle: PHLog;
  offset: DWORD;  reserved2: PDWORD; reserved3: DWORD; offsetflag: DWORD;
  bufptr: Pointer; prefmaxlen: DWORD; var bytesread: DWORD;
  var totalbytes: DWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetErrorLogRead}

function NetErrorLogWrite(reserved1: Pointer; code: DWORD; component: LPCWSTR;
  buffer: Pointer; numbytes: DWORD; msgbuf: Pointer; strcount: DWORD;
  reserved2: Pointer): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetErrorLogWrite}

// Special Values and Constants

//  Generic (could be used by more than one service)
//  error log messages from 0 to 25
//
// Do not change the comments following the manifest constants without
// understanding how mapmsg works.

const
  ERRLOG_BASE = 3100;        //* NELOG errors start here */
  {$EXTERNALSYM ERRLOG_BASE}


  NELOG_Internal_Error        = (ERRLOG_BASE + 0);
  {$EXTERNALSYM NELOG_Internal_Error}
  // The operation failed because a network software error occurred.

  NELOG_Resource_Shortage     = (ERRLOG_BASE + 1);
  {$EXTERNALSYM NELOG_Resource_Shortage}
  // The system ran out of a resource controlled by the %1 option.

  NELOG_Unable_To_Lock_Segment    = (ERRLOG_BASE + 2);
  {$EXTERNALSYM NELOG_Unable_To_Lock_Segment}
  // The service failed to obtain a long-term lock on the
  // segment for network control blocks (NCBs). The error code is the data.

  NELOG_Unable_To_Unlock_Segment  = (ERRLOG_BASE + 3);
  {$EXTERNALSYM NELOG_Unable_To_Unlock_Segment}
  // The service failed to release the long-term lock on the
  // segment for network control blocks (NCBs). The error code is the data.

  NELOG_Uninstall_Service     = (ERRLOG_BASE + 4);
  {$EXTERNALSYM NELOG_Uninstall_Service}
  // There was an error stopping service %1.
  // The error code from NetServiceControl is the data.

  NELOG_Init_Exec_Fail        = (ERRLOG_BASE + 5);
  {$EXTERNALSYM NELOG_Init_Exec_Fail}
  // Initialization failed because of a system execution failure on
  // path %1. The system error code is the data.

  NELOG_Ncb_Error         = (ERRLOG_BASE + 6);
  {$EXTERNALSYM NELOG_Ncb_Error}
  // An unexpected network control block (NCB) was received. The NCB is the data.

  NELOG_Net_Not_Started       = (ERRLOG_BASE + 7);
  {$EXTERNALSYM NELOG_Net_Not_Started}
  // The network is not started.

  NELOG_Ioctl_Error       = (ERRLOG_BASE + 8);
  {$EXTERNALSYM NELOG_Ioctl_Error}
  // A DosDevIoctl or DosFsCtl to NETWKSTA.SYS failed.
  // The data shown is in this format:
  //     DWORD  approx CS:IP of call to ioctl or fsctl
  //     WORD   error code
  //     WORD   ioctl or fsctl number

  NELOG_System_Semaphore      = (ERRLOG_BASE + 9);
  {$EXTERNALSYM NELOG_System_Semaphore}
  // Unable to create or open system semaphore %1.
  // The error code is the data.

  NELOG_Init_OpenCreate_Err   = (ERRLOG_BASE + 10);
  {$EXTERNALSYM NELOG_Init_OpenCreate_Err}
  // Initialization failed because of an open/create error on the
  // file %1. The system error code is the data.

  NELOG_NetBios           = (ERRLOG_BASE + 11);
  {$EXTERNALSYM NELOG_NetBios}
  // An unexpected NetBIOS error occurred.
  // The error code is the data.

  NELOG_SMB_Illegal       = (ERRLOG_BASE + 12);
  {$EXTERNALSYM NELOG_SMB_Illegal}
  // An illegal server message block (SMB) was received.
  // The SMB is the data.

  NELOG_Service_Fail      = (ERRLOG_BASE + 13);
  {$EXTERNALSYM NELOG_Service_Fail}
  // Initialization failed because the requested service %1
  // could not be started.

  NELOG_Entries_Lost      = (ERRLOG_BASE + 14);
  {$EXTERNALSYM NELOG_Entries_Lost}
  // Some entries in the error log were lost because of a buffer
  // overflow.

//  Server specific error log messages from 20 to 40

  NELOG_Init_Seg_Overflow     = (ERRLOG_BASE + 20);
  {$EXTERNALSYM NELOG_Init_Seg_Overflow}
  // Initialization parameters controlling resource usage other
  // than net buffers are sized so that too much memory is needed.

  NELOG_Srv_No_Mem_Grow       = (ERRLOG_BASE + 21);
  {$EXTERNALSYM NELOG_Srv_No_Mem_Grow}
  // The server cannot increase the size of a memory segment.

  NELOG_Access_File_Bad       = (ERRLOG_BASE + 22);
  {$EXTERNALSYM NELOG_Access_File_Bad}
  // Initialization failed because account file %1 is either incorrect
  // or not present.

  NELOG_Srvnet_Not_Started    = (ERRLOG_BASE + 23);
  {$EXTERNALSYM NELOG_Srvnet_Not_Started}
  // Initialization failed because network %1 was not started.

  NELOG_Init_Chardev_Err      = (ERRLOG_BASE + 24);
  {$EXTERNALSYM NELOG_Init_Chardev_Err}
  // The server failed to start. Either all three chdev
  //  parameters must be zero or all three must be nonzero.

  NELOG_Remote_API        = (ERRLOG_BASE + 25);
  {$EXTERNALSYM NELOG_Remote_API}
  // A remote API request was halted due to the following
  // invalid description string: %1.

  NELOG_Ncb_TooManyErr        = (ERRLOG_BASE + 26);
  {$EXTERNALSYM NELOG_Ncb_TooManyErr}
  // The network %1 ran out of network control blocks (NCBs).  You may need to increase NCBs
  // for this network.  The following information includes the
  // number of NCBs submitted by the server when this error occurred:

  NELOG_Mailslot_err      = (ERRLOG_BASE + 27);
  {$EXTERNALSYM NELOG_Mailslot_err}
  // The server cannot create the %1 mailslot needed to send
  // the ReleaseMemory alert message.  The error received is:

  NELOG_ReleaseMem_Alert      = (ERRLOG_BASE + 28);
  {$EXTERNALSYM NELOG_ReleaseMem_Alert}
  // The server failed to register for the ReleaseMemory alert,
  // with recipient %1. The error code from
  // NetAlertStart is the data.

  NELOG_AT_cannot_write       = (ERRLOG_BASE + 29);
  {$EXTERNALSYM NELOG_AT_cannot_write}
  // The server cannot update the AT schedule file. The file
  // is corrupted.

  NELOG_Cant_Make_Msg_File    = (ERRLOG_BASE + 30);
  {$EXTERNALSYM NELOG_Cant_Make_Msg_File}
  // The server encountered an error when calling
  // NetIMakeLMFileName. The error code is the data.

  NELOG_Exec_Netservr_NoMem   = (ERRLOG_BASE + 31);
  {$EXTERNALSYM NELOG_Exec_Netservr_NoMem}
  // Initialization failed because of a system execution failure on
  // path %1. There is not enough memory to start the process.
  // The system error code is the data.

  NELOG_Server_Lock_Failure   = (ERRLOG_BASE + 32);
  {$EXTERNALSYM NELOG_Server_Lock_Failure}
  // Longterm lock of the server buffers failed.
  // Check swap disk's free space and restart the system to start the server.

//  Message service and POPUP specific error log messages from 40 to 55

  NELOG_Msg_Shutdown      = (ERRLOG_BASE + 40);
  {$EXTERNALSYM NELOG_Msg_Shutdown}
  // The service has stopped due to repeated consecutive
  //  occurrences of a network control block (NCB) error.  The last bad NCB follows
  //  in raw data.

  NELOG_Msg_Sem_Shutdown      = (ERRLOG_BASE + 41);
  {$EXTERNALSYM NELOG_Msg_Sem_Shutdown}
  // The Message server has stopped due to a lock on the
  //  Message server shared data segment.

  NELOG_Msg_Log_Err       = (ERRLOG_BASE + 50);
  {$EXTERNALSYM NELOG_Msg_Log_Err}
  // A file system error occurred while opening or writing to the
  //  system message log file %1. Message logging has been
  //  switched off due to the error. The error code is the data.

  NELOG_VIO_POPUP_ERR     = (ERRLOG_BASE + 51);
  {$EXTERNALSYM NELOG_VIO_POPUP_ERR}
  // Unable to display message POPUP due to system VIO call error.
  //  The error code is the data.

  NELOG_Msg_Unexpected_SMB_Type   = (ERRLOG_BASE + 52);
  {$EXTERNALSYM NELOG_Msg_Unexpected_SMB_Type}
  // An illegal server message block (SMB) was received.  The SMB is the data.

//  Workstation specific error log messages from 60 to 75

  NELOG_Wksta_Infoseg     = (ERRLOG_BASE + 60);
  {$EXTERNALSYM NELOG_Wksta_Infoseg}
  // The workstation information segment is bigger than 64K.
  // The size follows, in DWORD format:

  NELOG_Wksta_Compname        = (ERRLOG_BASE + 61);
  {$EXTERNALSYM NELOG_Wksta_Compname}
  // The workstation was unable to get the name-number of the computer.

  NELOG_Wksta_BiosThreadFailure   = (ERRLOG_BASE + 62);
  {$EXTERNALSYM NELOG_Wksta_BiosThreadFailure}
  // The workstation could not initialize the Async NetBIOS Thread.
  // The error code is the data.

  NELOG_Wksta_IniSeg      = (ERRLOG_BASE + 63);
  {$EXTERNALSYM NELOG_Wksta_IniSeg}
  // The workstation could not open the initial shared segment.
  // The error code is the data.

  NELOG_Wksta_HostTab_Full    = (ERRLOG_BASE + 64);
  {$EXTERNALSYM NELOG_Wksta_HostTab_Full}
  // The workstation host table is full.

  NELOG_Wksta_Bad_Mailslot_SMB    = (ERRLOG_BASE + 65);
  {$EXTERNALSYM NELOG_Wksta_Bad_Mailslot_SMB}
  // A bad mailslot server message block (SMB) was received.  The SMB is the data.

  NELOG_Wksta_UASInit     = (ERRLOG_BASE + 66);
  {$EXTERNALSYM NELOG_Wksta_UASInit}
  // The workstation encountered an error while trying to start the user accounts database.
  //  The error code is the data.

  NELOG_Wksta_SSIRelogon      = (ERRLOG_BASE + 67);
  {$EXTERNALSYM NELOG_Wksta_SSIRelogon}
  // The workstation encountered an error while responding to an SSI revalidation request.
  //  The function code and the error codes are the data.

//  Alerter service specific error log messages from 70 to 79

  NELOG_Build_Name        = (ERRLOG_BASE + 70);
  {$EXTERNALSYM NELOG_Build_Name}
  // The Alerter service had a problem creating the list of
  // alert recipients.  The error code is %1.

  NELOG_Name_Expansion        = (ERRLOG_BASE + 71);
  {$EXTERNALSYM NELOG_Name_Expansion}
  // There was an error expanding %1 as a group name. Try
  // splitting the group into two or more smaller groups.

  NELOG_Message_Send      = (ERRLOG_BASE + 72);
  {$EXTERNALSYM NELOG_Message_Send}
  // There was an error sending %2 the alert message -
  //  (
  //  %3 )
  //  The error code is %1.

  NELOG_Mail_Slt_Err      = (ERRLOG_BASE + 73);
  {$EXTERNALSYM NELOG_Mail_Slt_Err}
  // There was an error in creating or reading the alerter mailslot.
  //  The error code is %1.

  NELOG_AT_cannot_read        = (ERRLOG_BASE + 74);
  {$EXTERNALSYM NELOG_AT_cannot_read}
  // The server could not read the AT schedule file.

  NELOG_AT_sched_err      = (ERRLOG_BASE + 75);
  {$EXTERNALSYM NELOG_AT_sched_err}
  // The server found an invalid AT schedule record.

  NELOG_AT_schedule_file_created  = (ERRLOG_BASE + 76);
  {$EXTERNALSYM NELOG_AT_schedule_file_created}
  // The server could not find an AT schedule file so it created one.

  NELOG_Srvnet_NB_Open        = (ERRLOG_BASE + 77);
  {$EXTERNALSYM NELOG_Srvnet_NB_Open}
  // The server could not access the %1 network with NetBiosOpen.

  NELOG_AT_Exec_Err       = (ERRLOG_BASE + 78);
  {$EXTERNALSYM NELOG_AT_Exec_Err}
  // The AT command processor could not run %1.

// Cache Lazy Write and HPFS386 specific error log messages from 80 to 89

  NELOG_Lazy_Write_Err            = (ERRLOG_BASE + 80);
  {$EXTERNALSYM NELOG_Lazy_Write_Err}
  // * WARNING:  Because of a lazy-write error, drive %1 now
  // *  contains some corrupted data.  The cache is stopped.

  NELOG_HotFix            = (ERRLOG_BASE + 81);
  {$EXTERNALSYM NELOG_HotFix}
  // A defective sector on drive %1 has been replaced (hotfixed).
  // No data was lost.  You should run CHKDSK soon to restore full
  // performance and replenish the volume's spare sector pool.
  //
  // The hotfix occurred while processing a remote request.

  NELOG_HardErr_From_Server   = (ERRLOG_BASE + 82);
  {$EXTERNALSYM NELOG_HardErr_From_Server}
  // A disk error occurred on the HPFS volume in drive %1.
  // The error occurred while processing a remote request.

  NELOG_LocalSecFail1 = (ERRLOG_BASE + 83);
  {$EXTERNALSYM NELOG_LocalSecFail1}
  // The user accounts database (NET.ACC) is corrupted.  The local security
  // system is replacing the corrupted NET.ACC with the backup
  // made at %1.
  // Any updates made to the database after this time are lost.

  NELOG_LocalSecFail2 = (ERRLOG_BASE + 84);
  {$EXTERNALSYM NELOG_LocalSecFail2}
  // The user accounts database (NET.ACC) is missing.  The local
  // security system is restoring the backup database
  // made at %1.
  // Any updates made to the database made after this time are lost.

  NELOG_LocalSecFail3 = (ERRLOG_BASE + 85);
  {$EXTERNALSYM NELOG_LocalSecFail3}
  // Local security could not be started because the user accounts database
  // (NET.ACC) was missing or corrupted, and no usable backup
  // database was present.
  //
  // THE SYSTEM IS NOT SECURE.

  NELOG_LocalSecGeneralFail   = (ERRLOG_BASE + 86);
  {$EXTERNALSYM NELOG_LocalSecGeneralFail}
  // Local security could not be started because an error
  // occurred during initialization. The error code returned is %1.
  //
  // THE SYSTEM IS NOT SECURE.

// NETWKSTA.SYS specific error log messages from 90 to 99

  NELOG_NetWkSta_Internal_Error   = (ERRLOG_BASE + 90);
  {$EXTERNALSYM NELOG_NetWkSta_Internal_Error}
  // A NetWksta internal error has occurred: %1

  NELOG_NetWkSta_No_Resource  = (ERRLOG_BASE + 91);
  {$EXTERNALSYM NELOG_NetWkSta_No_Resource}
  // The redirector is out of a resource: %1.

  NELOG_NetWkSta_SMB_Err      = (ERRLOG_BASE + 92);
  {$EXTERNALSYM NELOG_NetWkSta_SMB_Err}
  // A server message block (SMB) error occurred on the connection to %1.
  // The SMB header is the data.

  NELOG_NetWkSta_VC_Err       = (ERRLOG_BASE + 93);
  {$EXTERNALSYM NELOG_NetWkSta_VC_Err}
  // A virtual circuit error occurred on the session to %1.
  //  The network control block (NCB) command and return code is the data.

  NELOG_NetWkSta_Stuck_VC_Err = (ERRLOG_BASE + 94);
  {$EXTERNALSYM NELOG_NetWkSta_Stuck_VC_Err}
  // Hanging up a stuck session to %1.

  NELOG_NetWkSta_NCB_Err      = (ERRLOG_BASE + 95);
  {$EXTERNALSYM NELOG_NetWkSta_NCB_Err}
  // A network control block (NCB) error occurred (%1).
  // The NCB is the data.

  NELOG_NetWkSta_Write_Behind_Err = (ERRLOG_BASE + 96);
  {$EXTERNALSYM NELOG_NetWkSta_Write_Behind_Err}
  // A write operation to %1 failed.
  // Data may have been lost.

  NELOG_NetWkSta_Reset_Err    = (ERRLOG_BASE + 97);
  {$EXTERNALSYM NELOG_NetWkSta_Reset_Err}
  // Reset of driver %1 failed to complete the network control block (NCB).
  //  The NCB is the data.

  NELOG_NetWkSta_Too_Many     = (ERRLOG_BASE + 98);
  {$EXTERNALSYM NELOG_NetWkSta_Too_Many}
  // The amount of resource %1 requested was more
  // than the maximum. The maximum amount was allocated.

// Spooler specific error log messages from 100 to 103

  NELOG_Srv_Thread_Failure        = (ERRLOG_BASE + 104);
  {$EXTERNALSYM NELOG_Srv_Thread_Failure}
  // The server could not create a thread.
  // The THREADS parameter in the CONFIG.SYS file should be increased.

  NELOG_Srv_Close_Failure         = (ERRLOG_BASE + 105);
  {$EXTERNALSYM NELOG_Srv_Close_Failure}
  // The server could not close %1.
  // The file is probably corrupted.

  NELOG_ReplUserCurDir               = (ERRLOG_BASE + 106);
  {$EXTERNALSYM NELOG_ReplUserCurDir}
  // The replicator cannot update directory %1. It has tree integrity
  // and is the current directory for some process.

  NELOG_ReplCannotMasterDir       = (ERRLOG_BASE + 107);
  {$EXTERNALSYM NELOG_ReplCannotMasterDir}
  // The server cannot export directory %1 to client %2.
  // It is exported from another server.

  NELOG_ReplUpdateError           = (ERRLOG_BASE + 108);
  {$EXTERNALSYM NELOG_ReplUpdateError}
  // The replication server could not update directory %2 from the source
  // on %3 due to error %1.

  NELOG_ReplLostMaster            = (ERRLOG_BASE + 109);
  {$EXTERNALSYM NELOG_ReplLostMaster}
  // Master %1 did not send an update notice for directory %2 at the expected
  // time.

  NELOG_NetlogonAuthDCFail        = (ERRLOG_BASE + 110);
  {$EXTERNALSYM NELOG_NetlogonAuthDCFail}
  // Failed to authenticate with %2, a Windows NT domain controller for domain %1.

  NELOG_ReplLogonFailed           = (ERRLOG_BASE + 111);
  {$EXTERNALSYM NELOG_ReplLogonFailed}
  // The replicator attempted to log on at %2 as %1 and failed.

  NELOG_ReplNetErr            = (ERRLOG_BASE + 112);
  {$EXTERNALSYM NELOG_ReplNetErr}
  // Network error %1 occurred.

  NELOG_ReplMaxFiles            = (ERRLOG_BASE + 113);
  {$EXTERNALSYM NELOG_ReplMaxFiles}
  // Replicator limit for files in a directory has been exceeded.

  NELOG_ReplMaxTreeDepth            = (ERRLOG_BASE + 114);
  {$EXTERNALSYM NELOG_ReplMaxTreeDepth}
  // Replicator limit for tree depth has been exceeded.

  NELOG_ReplBadMsg             = (ERRLOG_BASE + 115);
  {$EXTERNALSYM NELOG_ReplBadMsg}
  // Unrecognized message received in mailslot.

  NELOG_ReplSysErr            = (ERRLOG_BASE + 116);
  {$EXTERNALSYM NELOG_ReplSysErr}
  // System error %1 occurred.

  NELOG_ReplUserLoged          = (ERRLOG_BASE + 117);
  {$EXTERNALSYM NELOG_ReplUserLoged}
  // Cannot log on. User is currently logged on and argument TRYUSER
  // is set to NO.

  NELOG_ReplBadImport           = (ERRLOG_BASE + 118);
  {$EXTERNALSYM NELOG_ReplBadImport}
  // IMPORT path %1 cannot be found.

  NELOG_ReplBadExport           = (ERRLOG_BASE + 119);
  {$EXTERNALSYM NELOG_ReplBadExport}
  // EXPORT path %1 cannot be found.

  NELOG_ReplSignalFileErr           = (ERRLOG_BASE + 120);
  {$EXTERNALSYM NELOG_ReplSignalFileErr}
  // Replicator failed to update signal file in directory %2 due to
  // %1 system error.

  NELOG_DiskFT                = (ERRLOG_BASE+121);
  {$EXTERNALSYM NELOG_DiskFT}
  // Disk Fault Tolerance Error
  //
  // %1

  NELOG_ReplAccessDenied           = (ERRLOG_BASE + 122);
  {$EXTERNALSYM NELOG_ReplAccessDenied}
  // Replicator could not access %2
  // on %3 due to system error %1.

  NELOG_NetlogonFailedPrimary      = (ERRLOG_BASE + 123);
  {$EXTERNALSYM NELOG_NetlogonFailedPrimary}
   {*
    *The primary domain controller for domain %1 has apparently failed.
    *}

  NELOG_NetlogonPasswdSetFailed = (ERRLOG_BASE + 124);
  {$EXTERNALSYM NELOG_NetlogonPasswdSetFailed}
  // Changing machine account password for account %1 failed with
  // the following error: %n%2

  NELOG_NetlogonTrackingError      = (ERRLOG_BASE + 125);
  {$EXTERNALSYM NELOG_NetlogonTrackingError}
  // An error occurred while updating the logon or logoff information for %1.

  NELOG_NetlogonSyncError          = (ERRLOG_BASE + 126);
  {$EXTERNALSYM NELOG_NetlogonSyncError}
  // An error occurred while synchronizing with primary domain controller %1

  NELOG_NetlogonRequireSignOrSealError = (ERRLOG_BASE + 127);
  {$EXTERNALSYM NELOG_NetlogonRequireSignOrSealError}
  // The session setup to the Windows NT Domain Controller %1 for the domain %2
  // failed because %1 does not support signing or sealing the Netlogon
  // session.
  //
  // Either upgrade the Domain controller or set the RequireSignOrSeal
  // registry entry on this machine to 0.

//  UPS service specific error log messages from 130 to 135

  NELOG_UPS_PowerOut      = (ERRLOG_BASE + 130);
  {$EXTERNALSYM NELOG_UPS_PowerOut}
  // A power failure was detected at the server.

  NELOG_UPS_Shutdown      = (ERRLOG_BASE + 131);
  {$EXTERNALSYM NELOG_UPS_Shutdown}
  // The UPS service performed server shut down.

  NELOG_UPS_CmdFileError      = (ERRLOG_BASE + 132);
  {$EXTERNALSYM NELOG_UPS_CmdFileError}
  // The UPS service did not complete execution of the
  // user specified shut down command file.

  NELOG_UPS_CannotOpenDriver  = (ERRLOG_BASE+133);
  {$EXTERNALSYM NELOG_UPS_CannotOpenDriver}
  // The UPS driver could not be opened.  The error code is
  // the data.

  NELOG_UPS_PowerBack     = (ERRLOG_BASE + 134);
  {$EXTERNALSYM NELOG_UPS_PowerBack}
  // Power has been restored.

  NELOG_UPS_CmdFileConfig     = (ERRLOG_BASE + 135);
  {$EXTERNALSYM NELOG_UPS_CmdFileConfig}
  // There is a problem with a configuration of user specified
  // shut down command file.

  NELOG_UPS_CmdFileExec       = (ERRLOG_BASE + 136);
  {$EXTERNALSYM NELOG_UPS_CmdFileExec}
  // The UPS service failed to execute a user specified shutdown
  // command file %1.  The error code is the data.

// Remoteboot server specific error log messages are from 150 to 157

  NELOG_Missing_Parameter     = (ERRLOG_BASE + 150);
  {$EXTERNALSYM NELOG_Missing_Parameter}
  // Initialization failed because of an invalid or missing
  // parameter in the configuration file %1.

  NELOG_Invalid_Config_Line   = (ERRLOG_BASE + 151);
  {$EXTERNALSYM NELOG_Invalid_Config_Line}
  // Initialization failed because of an invalid line in the
  // configuration file %1. The invalid line is the data.

  NELOG_Invalid_Config_File   = (ERRLOG_BASE + 152);
  {$EXTERNALSYM NELOG_Invalid_Config_File}
  // Initialization failed because of an error in the configuration
  // file %1.

  NELOG_File_Changed      = (ERRLOG_BASE + 153);
  {$EXTERNALSYM NELOG_File_Changed}
  // The file %1 has been changed after initialization.
  // The boot-block loading was temporarily terminated.

  NELOG_Files_Dont_Fit        = (ERRLOG_BASE + 154);
  {$EXTERNALSYM NELOG_Files_Dont_Fit}
  // The files do not fit to the boot-block configuration
  // file %1. Change the BASE and ORG definitions or the order
  // of the files.

  NELOG_Wrong_DLL_Version     = (ERRLOG_BASE + 155);
  {$EXTERNALSYM NELOG_Wrong_DLL_Version}
  // Initialization failed because the dynamic-link
  // library %1 returned an incorrect version number.

  NELOG_Error_in_DLL      = (ERRLOG_BASE + 156);
  {$EXTERNALSYM NELOG_Error_in_DLL}
  // There was an unrecoverable error in the dynamic- link library of the
  // service.

  NELOG_System_Error      = (ERRLOG_BASE + 157);
  {$EXTERNALSYM NELOG_System_Error}
  // The system returned an unexpected error code.
  // The error code is the data.

  NELOG_FT_ErrLog_Too_Large = (ERRLOG_BASE + 158);
  {$EXTERNALSYM NELOG_FT_ErrLog_Too_Large}
  // The fault-tolerance error log file, LANROOT\LOGS\FT.LOG, is more than 64K.

  NELOG_FT_Update_In_Progress = (ERRLOG_BASE + 159);
  {$EXTERNALSYM NELOG_FT_Update_In_Progress}
  // The fault-tolerance error-log file, LANROOT\LOGS\FT.LOG, had the
  // update in progress bit set upon opening, which means that the
  // system crashed while working on the error log.


// Microsoft has created a generic error log entry for OEMs to use to
// log errors from OEM value added services.  The code, which is the
// 2nd arg to NetErrorLogWrite, is 3299.  This value is manifest in
// NET/H/ERRLOG.H as NELOG_OEM_Code.  The text for error log entry
// NELOG_OEM_Code is:  "%1 %2 %3 %4 %5 %6 %7 %8 %9.".
//
// Microsoft suggests that OEMs use the insertion strings as follows:
// %1:  OEM System Name (e.g. 3+Open)
// %2:  OEM Service Name (e.g. 3+Mail)
// %3:  Severity level (e.g.  error, warning, etc.)
// %4:  OEM error log entry sub-identifier  (e.g. error code #)
// %5 - % 9:  Text.
//
// The call to NetErrorWrite must set nstrings = 9, and provide 9
// ASCIIZ strings.  If the caller does not have 9 insertion strings,
// provide null strings for the empty insertion strings.

  NELOG_OEM_Code              = (ERRLOG_BASE + 199);
  {$EXTERNALSYM NELOG_OEM_Code}
  // %1 %2 %3 %4 %5 %6 %7 %8 %9.

// another error log range defined for NT Lanman.

  ERRLOG2_BASE=  5700;       // New NT NELOG errors start here
  {$EXTERNALSYM ERRLOG2_BASE}

  NELOG_NetlogonSSIInitError              = (ERRLOG2_BASE + 0);
  {$EXTERNALSYM NELOG_NetlogonSSIInitError}
  // The Netlogon service could not initialize the replication data
  // structures successfully. The service was terminated.  The following
  // error occurred: %n%1

  NELOG_NetlogonFailedToUpdateTrustList   = (ERRLOG2_BASE + 1);
  {$EXTERNALSYM NELOG_NetlogonFailedToUpdateTrustList}
  // The Netlogon service failed to update the domain trust list.  The
  // following error occurred: %n%1

  NELOG_NetlogonFailedToAddRpcInterface   = (ERRLOG2_BASE + 2);
  {$EXTERNALSYM NELOG_NetlogonFailedToAddRpcInterface}
  // The Netlogon service could not add the RPC interface.  The
  // service was terminated. The following error occurred: %n%1

  NELOG_NetlogonFailedToReadMailslot      = (ERRLOG2_BASE + 3);
  {$EXTERNALSYM NELOG_NetlogonFailedToReadMailslot}
  // The Netlogon service could not read a mailslot message from %1 due
  // to the following error: %n%2

  NELOG_NetlogonFailedToRegisterSC        = (ERRLOG2_BASE + 4);
  {$EXTERNALSYM NELOG_NetlogonFailedToRegisterSC}
  // The Netlogon service failed to register the service with the
  // service controller. The service was terminated. The following
  // error occurred: %n%1

  NELOG_NetlogonChangeLogCorrupt          = (ERRLOG2_BASE + 5);
  {$EXTERNALSYM NELOG_NetlogonChangeLogCorrupt}
  // The change log cache maintained by the Netlogon service for
  // database changes is corrupted. The Netlogon service is resetting
  // the change log.

  NELOG_NetlogonFailedToCreateShare       = (ERRLOG2_BASE + 6);
  {$EXTERNALSYM NELOG_NetlogonFailedToCreateShare}
  // The Netlogon service could not create server share %1.  The following
  // error occurred: %n%2

  NELOG_NetlogonDownLevelLogonFailed      = (ERRLOG2_BASE + 7);
  {$EXTERNALSYM NELOG_NetlogonDownLevelLogonFailed}
  // The down-level logon request for the user %1 from %2 failed.

  NELOG_NetlogonDownLevelLogoffFailed     = (ERRLOG2_BASE + 8);
  {$EXTERNALSYM NELOG_NetlogonDownLevelLogoffFailed}
  // The down-level logoff request for the user %1 from %2 failed.

  NELOG_NetlogonNTLogonFailed             = (ERRLOG2_BASE + 9);
  {$EXTERNALSYM NELOG_NetlogonNTLogonFailed}
  // The Windows NT %1 logon request for the user %2\%3 from %4 (via %5)
  // failed.

  NELOG_NetlogonNTLogoffFailed            = (ERRLOG2_BASE + 10);
  {$EXTERNALSYM NELOG_NetlogonNTLogoffFailed}
  // The Windows NT %1 logoff request for the user %2\%3 from %4
  // failed.

  NELOG_NetlogonPartialSyncCallSuccess    = (ERRLOG2_BASE + 11);
  {$EXTERNALSYM NELOG_NetlogonPartialSyncCallSuccess}
  // The partial synchronization request from the server %1 completed
  // successfully. %2 changes(s) has(have) been returned to the caller.

  NELOG_NetlogonPartialSyncCallFailed     = (ERRLOG2_BASE + 12);
  {$EXTERNALSYM NELOG_NetlogonPartialSyncCallFailed}
  // The partial synchronization request from the server %1 failed with
  // the following error: %n%2

  NELOG_NetlogonFullSyncCallSuccess       = (ERRLOG2_BASE + 13);
  {$EXTERNALSYM NELOG_NetlogonFullSyncCallSuccess}
  // The full synchronization request from the server %1 completed
  // successfully. %2 object(s) has(have) been returned to the caller.

  NELOG_NetlogonFullSyncCallFailed        = (ERRLOG2_BASE + 14);
  {$EXTERNALSYM NELOG_NetlogonFullSyncCallFailed}
  // The full synchronization request from the server %1 failed with
  // the following error: %n%2

  NELOG_NetlogonPartialSyncSuccess        = (ERRLOG2_BASE + 15);
  {$EXTERNALSYM NELOG_NetlogonPartialSyncSuccess}
  // The partial synchronization replication of the %1 database from the
  // primary domain controller %2 completed successfully. %3 change(s) is(are)
  // applied to the database.

  NELOG_NetlogonPartialSyncFailed         = (ERRLOG2_BASE + 16);
  {$EXTERNALSYM NELOG_NetlogonPartialSyncFailed}
  // The partial synchronization replication of the %1 database from the
  // primary domain controller %2 failed with the following error: %n%3

  NELOG_NetlogonFullSyncSuccess           = (ERRLOG2_BASE + 17);
  {$EXTERNALSYM NELOG_NetlogonFullSyncSuccess}
  // The full synchronization replication of the %1 database from the
  // primary domain controller %2 completed successfully.

  NELOG_NetlogonFullSyncFailed            = (ERRLOG2_BASE + 18);
  {$EXTERNALSYM NELOG_NetlogonFullSyncFailed}
  // The full synchronization replication of the %1 database from the
  // primary domain controller %2 failed with the following error: %n%3

  NELOG_NetlogonAuthNoDomainController    = (ERRLOG2_BASE + 19);
  {$EXTERNALSYM NELOG_NetlogonAuthNoDomainController}
  // No Windows NT Domain Controller is available for domain %1.
  // (This event is expected and can be ignored when booting with
  // the 'No Net' Hardware Profile.)  The following error occurred:%n%2

  NELOG_NetlogonAuthNoTrustLsaSecret      = (ERRLOG2_BASE + 20);
  {$EXTERNALSYM NELOG_NetlogonAuthNoTrustLsaSecret}
  // The session setup to the Windows NT Domain Controller %1 for the domain %2
  // failed because the computer %3 does not have a local security database account.

  NELOG_NetlogonAuthNoTrustSamAccount     = (ERRLOG2_BASE + 21);
  {$EXTERNALSYM NELOG_NetlogonAuthNoTrustSamAccount}
  // The session setup to the Windows NT Domain Controller %1 for the domain %2
  // failed because the Windows NT Domain Controller does not have an account
  // for the computer %3.

  NELOG_NetlogonServerAuthFailed          = (ERRLOG2_BASE + 22);
  {$EXTERNALSYM NELOG_NetlogonServerAuthFailed}
  // The session setup from the computer %1 failed to authenticate.
  // The name of the account referenced in the security database is
  // %2.  The following error occurred: %n%3

  NELOG_NetlogonServerAuthNoTrustSamAccount = (ERRLOG2_BASE + 23);
  {$EXTERNALSYM NELOG_NetlogonServerAuthNoTrustSamAccount}
  // The session setup from the computer %1 failed because there is
  // no trust account in the security database for this computer. The name of
  // the account referenced in the security database is %2.

// General log messages for NT services.

  NELOG_FailedToRegisterSC                  = (ERRLOG2_BASE + 24);
  {$EXTERNALSYM NELOG_FailedToRegisterSC}
  // Could not register control handler with service controller %1.

  NELOG_FailedToSetServiceStatus            = (ERRLOG2_BASE + 25);
  {$EXTERNALSYM NELOG_FailedToSetServiceStatus}
  // Could not set service status with service controller %1.

  NELOG_FailedToGetComputerName             = (ERRLOG2_BASE + 26);
  {$EXTERNALSYM NELOG_FailedToGetComputerName}
  // Could not find the computer name %1.

  NELOG_DriverNotLoaded                     = (ERRLOG2_BASE + 27);
  {$EXTERNALSYM NELOG_DriverNotLoaded}
  // Could not load %1 device driver.

  NELOG_NoTranportLoaded                    = (ERRLOG2_BASE + 28);
  {$EXTERNALSYM NELOG_NoTranportLoaded}
  // Could not load any transport.

// More Netlogon service events

  NELOG_NetlogonFailedDomainDelta           = (ERRLOG2_BASE + 29);
  {$EXTERNALSYM NELOG_NetlogonFailedDomainDelta}
  // Replication of the %1 Domain Object "%2" from primary domain controller
  // %3 failed with the following error: %n%4

  NELOG_NetlogonFailedGlobalGroupDelta      = (ERRLOG2_BASE + 30);
  {$EXTERNALSYM NELOG_NetlogonFailedGlobalGroupDelta}
  // Replication of the %1 Global Group "%2" from primary domain controller
  // %3 failed with the following error: %n%4

  NELOG_NetlogonFailedLocalGroupDelta       = (ERRLOG2_BASE + 31);
  {$EXTERNALSYM NELOG_NetlogonFailedLocalGroupDelta}
  // Replication of the %1 Local Group "%2" from primary domain controller
  // %3 failed with the following error: %n%4

  NELOG_NetlogonFailedUserDelta             = (ERRLOG2_BASE + 32);
  {$EXTERNALSYM NELOG_NetlogonFailedUserDelta}
  // Replication of the %1 User "%2" from primary domain controller
  // %3 failed with the following error: %n%4

  NELOG_NetlogonFailedPolicyDelta           = (ERRLOG2_BASE + 33);
  {$EXTERNALSYM NELOG_NetlogonFailedPolicyDelta}
  // Replication of the %1 Policy Object "%2" from primary domain controller
  // %3 failed with the following error: %n%4

  NELOG_NetlogonFailedTrustedDomainDelta    = (ERRLOG2_BASE + 34);
  {$EXTERNALSYM NELOG_NetlogonFailedTrustedDomainDelta}
  // Replication of the %1 Trusted Domain Object "%2" from primary domain controller
  // %3 failed with the following error: %n%4

  NELOG_NetlogonFailedAccountDelta          = (ERRLOG2_BASE + 35);
  {$EXTERNALSYM NELOG_NetlogonFailedAccountDelta}
  // Replication of the %1 Account Object "%2" from primary domain controller
  // %3 failed with the following error: %n%4

  NELOG_NetlogonFailedSecretDelta           = (ERRLOG2_BASE + 36);
  {$EXTERNALSYM NELOG_NetlogonFailedSecretDelta}
  // Replication of the %1 Secret "%2" from primary domain controller
  // %3 failed with the following error: %n%4

  NELOG_NetlogonSystemError                 = (ERRLOG2_BASE + 37);
  {$EXTERNALSYM NELOG_NetlogonSystemError}
  // The system returned the following unexpected error code: %n%1

  NELOG_NetlogonDuplicateMachineAccounts    = (ERRLOG2_BASE + 38);
  {$EXTERNALSYM NELOG_NetlogonDuplicateMachineAccounts}
  // Netlogon has detected two machine accounts for server "%1".
  // The server can be either a Windows NT Server that is a member of the
  // domain or the server can be a LAN Manager server with an account in the
  // SERVERS global group.  It cannot be both.

  NELOG_NetlogonTooManyGlobalGroups         = (ERRLOG2_BASE + 39);
  {$EXTERNALSYM NELOG_NetlogonTooManyGlobalGroups}
  // This domain has more global groups than can be replicated to a LanMan
  // BDC.  Either delete some of your global groups or remove the LanMan
  // BDCs from the domain.

  NELOG_NetlogonBrowserDriver               = (ERRLOG2_BASE + 40);
  {$EXTERNALSYM NELOG_NetlogonBrowserDriver}
  // The Browser driver returned the following error to Netlogon: %n%1

  NELOG_NetlogonAddNameFailure              = (ERRLOG2_BASE + 41);
  {$EXTERNALSYM NELOG_NetlogonAddNameFailure}
  // Netlogon could not register the %1<1B> name for the following reason: %n%2

//  More Remoteboot service events.

  NELOG_RplMessages                         = (ERRLOG2_BASE + 42);
  {$EXTERNALSYM NELOG_RplMessages}
  // Service failed to retrieve messages needed to boot remote boot clients.

  NELOG_RplXnsBoot                          = (ERRLOG2_BASE + 43);
  {$EXTERNALSYM NELOG_RplXnsBoot}
  // Service experienced a severe error and can no longer provide remote boot
  // for 3Com 3Start remote boot clients.

  NELOG_RplSystem                           = (ERRLOG2_BASE + 44);
  {$EXTERNALSYM NELOG_RplSystem}
  // Service experienced a severe system error and will shut itself down.

  NELOG_RplWkstaTimeout                     = (ERRLOG2_BASE + 45);
  {$EXTERNALSYM NELOG_RplWkstaTimeout}
  // Client with computer name %1 failed to acknowledge receipt of the
  // boot data.  Remote boot of this client was not completed.

  NELOG_RplWkstaFileOpen                    = (ERRLOG2_BASE + 46);
  {$EXTERNALSYM NELOG_RplWkstaFileOpen}
  // Client with computer name %1 was not booted due to an error in opening
  // file %2.

  NELOG_RplWkstaFileRead                    = (ERRLOG2_BASE + 47);
  {$EXTERNALSYM NELOG_RplWkstaFileRead}
  // Client with computer name %1 was not booted due to an error in reading
  // file %2.

  NELOG_RplWkstaMemory                      = (ERRLOG2_BASE + 48);
  {$EXTERNALSYM NELOG_RplWkstaMemory}
  // Client with computer name %1 was not booted due to insufficent memory
  // at the remote boot server.

  NELOG_RplWkstaFileChecksum                = (ERRLOG2_BASE + 49);
  {$EXTERNALSYM NELOG_RplWkstaFileChecksum}
  // Client with computer name %1 will be booted without using checksums
  // because checksum for file %2 could not be calculated.

  NELOG_RplWkstaFileLineCount               = (ERRLOG2_BASE + 50);
  {$EXTERNALSYM NELOG_RplWkstaFileLineCount}
  // Client with computer name %1 was not booted due to too many lines in
  // file %2.

  NELOG_RplWkstaBbcFile                     = (ERRLOG2_BASE + 51);
  {$EXTERNALSYM NELOG_RplWkstaBbcFile}
  // Client with computer name %1 was not booted because the boot block
  // configuration file %2 for this client does not contain boot block
  // line and/or loader line.

  NELOG_RplWkstaFileSize                    = (ERRLOG2_BASE + 52);
  {$EXTERNALSYM NELOG_RplWkstaFileSize}
  // Client with computer name %1 was not booted due to a bad size of
  // file %2.

  NELOG_RplWkstaInternal                    = (ERRLOG2_BASE + 53);
  {$EXTERNALSYM NELOG_RplWkstaInternal}
  // Client with computer name %1 was not booted due to remote boot
  // service internal error.

  NELOG_RplWkstaWrongVersion                = (ERRLOG2_BASE + 54);
  {$EXTERNALSYM NELOG_RplWkstaWrongVersion}
  // Client with computer name %1 was not booted because file %2 has an
  // invalid boot header.

  NELOG_RplWkstaNetwork                     = (ERRLOG2_BASE + 55);
  {$EXTERNALSYM NELOG_RplWkstaNetwork}
  // Client with computer name %1 was not booted due to network error.

  NELOG_RplAdapterResource                  = (ERRLOG2_BASE + 56);
  {$EXTERNALSYM NELOG_RplAdapterResource}
  // Client with adapter id %1 was not booted due to lack of resources.

  NELOG_RplFileCopy                         = (ERRLOG2_BASE + 57);
  {$EXTERNALSYM NELOG_RplFileCopy}
  // Service experienced error copying file or directory %1.

  NELOG_RplFileDelete                       = (ERRLOG2_BASE + 58);
  {$EXTERNALSYM NELOG_RplFileDelete}
  // Service experienced error deleting file or directory %1.

  NELOG_RplFilePerms                        = (ERRLOG2_BASE + 59);
  {$EXTERNALSYM NELOG_RplFilePerms}
  // Service experienced error setting permissions on file or directory %1.

  NELOG_RplCheckConfigs                     = (ERRLOG2_BASE + 60);
  {$EXTERNALSYM NELOG_RplCheckConfigs}
  // Service experienced error evaluating RPL configurations.

  NELOG_RplCreateProfiles                   = (ERRLOG2_BASE + 61);
  {$EXTERNALSYM NELOG_RplCreateProfiles}
  // Service experienced error creating RPL profiles for all configurations.

  NELOG_RplRegistry                         = (ERRLOG2_BASE + 62);
  {$EXTERNALSYM NELOG_RplRegistry}
  // Service experienced error accessing registry.

  NELOG_RplReplaceRPLDISK                   = (ERRLOG2_BASE + 63);
  {$EXTERNALSYM NELOG_RplReplaceRPLDISK}
  // Service experienced error replacing possibly outdated RPLDISK.SYS.

  NELOG_RplCheckSecurity                    = (ERRLOG2_BASE + 64);
  {$EXTERNALSYM NELOG_RplCheckSecurity}
  // Service experienced error adding security accounts or setting
  // file permissions.  These accounts are the RPLUSER local group
  // and the user accounts for the individual RPL workstations.

  NELOG_RplBackupDatabase                   = (ERRLOG2_BASE + 65);
  {$EXTERNALSYM NELOG_RplBackupDatabase}
  // Service failed to back up its database.

  NELOG_RplInitDatabase                     = (ERRLOG2_BASE + 66);
  {$EXTERNALSYM NELOG_RplInitDatabase}
  // Service failed to initialize from its database.  The database may be
  // missing or corrupted.  Service will attempt restoring the database
  // from the backup.

  NELOG_RplRestoreDatabaseFailure           = (ERRLOG2_BASE + 67);
  {$EXTERNALSYM NELOG_RplRestoreDatabaseFailure}
  // Service failed to restore its database from the backup.  Service
  // will not start.

  NELOG_RplRestoreDatabaseSuccess           = (ERRLOG2_BASE + 68);
  {$EXTERNALSYM NELOG_RplRestoreDatabaseSuccess}
  // Service sucessfully restored its database from the backup.

  NELOG_RplInitRestoredDatabase             = (ERRLOG2_BASE + 69);
  {$EXTERNALSYM NELOG_RplInitRestoredDatabase}
  // Service failed to initialize from its restored database.  Service
  // will not start.

// More Netlogon and RPL service events

  NELOG_NetlogonSessionTypeWrong            = (ERRLOG2_BASE + 70);
  {$EXTERNALSYM NELOG_NetlogonSessionTypeWrong}
  // The session setup to the Windows NT Domain Controller %1 from computer
  // %2 using account %4 failed.  %2 is declared to be a BDC in domain %3.
  // However, %2 tried to connect as either a DC in a trusted domain,
  // a member workstation in domain %3, or as a server in domain %3.
  // Use the Server Manager to remove the BDC account for %2.

  NELOG_RplUpgradeDBTo40                    = (ERRLOG2_BASE + 71);
  {$EXTERNALSYM NELOG_RplUpgradeDBTo40}
  // The Remoteboot database was in NT 3.5 / NT 3.51 format and NT is
  // attempting to convert it to NT 4.0 format. The JETCONV converter
  // will write to the Application event log when it is finished.

  NELOG_NetlogonLanmanBdcsNotAllowed        = (ERRLOG2_BASE + 72);
  {$EXTERNALSYM NELOG_NetlogonLanmanBdcsNotAllowed}
  // Global group SERVERS exists in domain %1 and has members.
  // This group defines Lan Manager BDCs in the domain.
  // Lan Manager BDCs are not permitted in NT domains.

  NELOG_NetlogonNoDynamicDns                = (ERRLOG2_BASE + 73);
  {$EXTERNALSYM NELOG_NetlogonNoDynamicDns}
  // The DNS server for this DC does not support dynamic DNS.
  // Add the DNS records from the file '%SystemRoot%\System32\Config\netlogon.dns'
  // to the DNS server serving the domain referenced in that file.

  NELOG_NetlogonDynamicDnsRegisterFailure   = (ERRLOG2_BASE + 74);
  {$EXTERNALSYM NELOG_NetlogonDynamicDnsRegisterFailure}
  // Registration of the DNS record '%1' failed with the following error: %n%2

  NELOG_NetlogonDynamicDnsDeregisterFailure = (ERRLOG2_BASE + 75);
  {$EXTERNALSYM NELOG_NetlogonDynamicDnsDeregisterFailure}
  // Deregistration of the DNS record '%1' failed with the following error: %n%2

  NELOG_NetlogonFailedFileCreate            = (ERRLOG2_BASE + 76);
  {$EXTERNALSYM NELOG_NetlogonFailedFileCreate}
  // Failed to create/open file %1 with the following error: %n%2

  NELOG_NetlogonGetSubnetToSite             = (ERRLOG2_BASE + 77);
  {$EXTERNALSYM NELOG_NetlogonGetSubnetToSite}
  // Netlogon got the following error while trying to get the subnet to site
  // mapping information from the DS: %n%1

  NELOG_NetlogonNoSiteForClient              = (ERRLOG2_BASE + 78);
  {$EXTERNALSYM NELOG_NetlogonNoSiteForClient}
  // '%1' tried to determine its site by looking up its IP address ('%2')
  // in the Configuration\Sites\Subnets container in the DS.  No subnet matched
  // the IP address.  Consider adding a subnet object for this IP address.

  NELOG_NetlogonBadSiteName                  = (ERRLOG2_BASE + 79);
  {$EXTERNALSYM NELOG_NetlogonBadSiteName}
  // The site name for this computer is '%1'.  That site name is not a valid
  // site name.  A site name must be a valid DNS label.
  // Rename the site to be a valid name.

  NELOG_NetlogonBadSubnetName                = (ERRLOG2_BASE + 80);
  {$EXTERNALSYM NELOG_NetlogonBadSubnetName}
  // The subnet object '%1' appears in the Configuration\Sites\Subnets
  // container in the DS.  The name is not syntactically valid.  The valid
  // syntax is xx.xx.xx.xx/yy where xx.xx.xx.xx is a valid IP subnet number
  // and yy is the number of bits in the subnet mask.
  //
  // Correct the name of the subnet object.

  NELOG_NetlogonDynamicDnsServerFailure      = (ERRLOG2_BASE + 81);
  {$EXTERNALSYM NELOG_NetlogonDynamicDnsServerFailure}
  // Dynamic registration or deregistration of one or more DNS records failed because no DNS servers are available.

  NELOG_NetlogonDynamicDnsFailure            = (ERRLOG2_BASE + 82);
  {$EXTERNALSYM NELOG_NetlogonDynamicDnsFailure}
  // Dynamic registration or deregistration of one or more DNS records failed with the following error: %n%1

  NELOG_NetlogonRpcCallCancelled             = (ERRLOG2_BASE + 83);
  {$EXTERNALSYM NELOG_NetlogonRpcCallCancelled}
  // The session setup to the Windows NT Domain Controller %1 for the domain %2
  // is not responsive.  The current RPC call from Netlogon on \\%3 to %1 has been cancelled.

  NELOG_NetlogonDcSiteCovered                = (ERRLOG2_BASE + 84);
  {$EXTERNALSYM NELOG_NetlogonDcSiteCovered}
  // Site '%2' does not have any Domain Controllers for domain '%3'.
  // Domain Controllers in site '%1' have been automatically
  // selected to cover site '%2' based on configured Directory Server
  // replication costs.

  NELOG_NetlogonDcSiteNotCovered             = (ERRLOG2_BASE + 85);
  {$EXTERNALSYM NELOG_NetlogonDcSiteNotCovered}
  // This Domain Controller no longer automatically covers site '%1'.

  NELOG_NetlogonGcSiteCovered                = (ERRLOG2_BASE + 86);
  {$EXTERNALSYM NELOG_NetlogonGcSiteCovered}
  // Site '%2' does not have any Global Catalog servers.
  // Global Catalog servers in site '%1' have been automatically
  // selected to cover site '%2' based on configured Directory Server
  // replication costs.

  NELOG_NetlogonGcSiteNotCovered             = (ERRLOG2_BASE + 87);
  {$EXTERNALSYM NELOG_NetlogonGcSiteNotCovered}
  // This Global Catalog server no longer automatically covers site '%1'.

  NELOG_NetlogonFailedSpnUpdate              = (ERRLOG2_BASE + 88);
  {$EXTERNALSYM NELOG_NetlogonFailedSpnUpdate}
  // Attemp to update Service Principal Name (SPN) of the computer object
  // in Active Directory failed.  The following error occurred: %n%1

  NELOG_NetlogonFailedDnsHostNameUpdate      = (ERRLOG2_BASE + 89);
  {$EXTERNALSYM NELOG_NetlogonFailedDnsHostNameUpdate}
  // Attemp to update DNS Host Name of the computer object
  // in Active Directory failed.  The following error occurred: %n%1


implementation

function NetErrorLogClear; external netapi32lib name 'NetErrorLogClear';
function NetErrorLogRead; external netapi32lib name 'NetErrorLogRead';
function NetErrorLogWrite; external netapi32lib name 'NetErrorLogWrite';

end.
