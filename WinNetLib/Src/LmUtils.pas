{******************************************************************}
{                                                                  }
{ Borland Delphi Runtime Library                                   }
{ LanManager error messages and return code checking function      }
{                                                                  }
{ Portions created by Microsoft are                                }
{ Copyright (C) 1995-1999 Microsoft Corporation.                   }
{ All Rights Reserved.                                             }
{                                                                  }
{ The original Pascal code is: LmErrText.pas, released 29 Dec 1999 }
{ The initial developer of the Pascal code is Petr Vones           }
{ (petr.v@mujmail.cz).                                             }
{                                                                  }
{ Portions created by Petr Vones are                               }
{ Copyright (C) 1999 Petr Vones                                    }
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

unit LmUtils;

interface

uses
  Windows, Classes, SysUtils, LmErr;

function NetErrorMessage(ErrorCode: Integer): String;

function SysAndNetErrorMessage(ErrorCode: Integer): String;

function NetCheck(RetCode: DWORD): DWORD;

implementation

resourcestring
  sNetError = 'Net Error code: %d.'#10'"%s"';

{$IFDEF LANMAN_INCLUDE_ERRTEXTS}
  sNERR_NetNotStarted = 'The workstation driver is not installed.';
  sNERR_UnknownServer = 'The server could not be located.';
  sNERR_ShareMem = 'An internal error occurred.  The network cannot access a shared memory segment.';
  sNERR_NoNetworkResource = 'A network resource shortage occurred .';
  sNERR_RemoteOnly = 'This operation is not supported on workstations.';
  sNERR_DevNotRedirected = 'The device is not connected.';
  sNERR_ServerNotStarted = 'The Server service is not started.';
  sNERR_ItemNotFound = 'The queue is empty.';
  sNERR_UnknownDevDir = 'The device or directory does not exist.';
  sNERR_RedirectedPath = 'The operation is invalid on a redirected resource.';
  sNERR_DuplicateShare = 'The name has already been shared.';
  sNERR_NoRoom = 'The server is currently out of the requested resource.';
  sNERR_TooManyItems = 'Requested addition of items exceeds the maximum allowed.';
  sNERR_InvalidMaxUsers = 'The Peer service supports only two simultaneous users.';
  sNERR_BufTooSmall = 'The API return buffer is too small.';
  sNERR_RemoteErr = 'A remote API error occurred.';
  sNERR_LanmanIniError = 'An error occurred when opening or reading the configuration file.';
  sNERR_NetworkError = 'A general network error occurred.';
  sNERR_WkstaInconsistentState = 'The Workstation service is in an inconsistent state. Restart the computer before restarting the Workstation service.';
  sNERR_WkstaNotStarted = 'The Workstation service has not been started.';
  sNERR_BrowserNotStarted = 'The requested information is not available.';
  sNERR_InternalError = 'An internal Windows NT error occurred.';
  sNERR_BadTransactConfig = 'The server is not configured for transactions.';
  sNERR_InvalidAPI = 'The requested API is not supported on the remote server.';
  sNERR_BadEventName = 'The event name is invalid.';
  sNERR_DupNameReboot = 'The computer name already exists on the network. Change it and restart the computer.';
  sNERR_CfgCompNotFound = 'The specified component could not be found in the configuration information.';
  sNERR_CfgParamNotFound = 'The specified parameter could not be found in the configuration information.';
  sNERR_LineTooLong = 'A line in the configuration file is too long.';
  sNERR_QNotFound = 'The printer does not exist.';
  sNERR_JobNotFound = 'The print job does not exist.';
  sNERR_DestNotFound = 'The printer destination cannot be found.';
  sNERR_DestExists = 'The printer destination already exists.';
  sNERR_QExists = 'The printer queue already exists.';
  sNERR_QNoRoom = 'No more printers can be added.';
  sNERR_JobNoRoom = 'No more print jobs can be added.';
  sNERR_DestNoRoom = 'No more printer destinations can be added.';
  sNERR_DestIdle = 'This printer destination is idle and cannot accept control operations.';
  sNERR_DestInvalidOp = 'This printer destination request contains an invalid control function.';
  sNERR_ProcNoRespond = 'The print processor is not responding.';
  sNERR_SpoolerNotLoaded = 'The spooler is not running.';
  sNERR_DestInvalidState = 'This operation cannot be performed on the print destination in its current state.';
  sNERR_QInvalidState = 'This operation cannot be performed on the printer queue in its current state.';
  sNERR_JobInvalidState = 'This operation cannot be performed on the print job in its current state.';
  sNERR_SpoolNoMemory = 'A spooler memory allocation failure occurred.';
  sNERR_DriverNotFound = 'The device driver does not exist.';
  sNERR_DataTypeInvalid = 'The data type is not supported by the print processor.';
  sNERR_ProcNotFound = 'The print processor is not installed.';
  sNERR_ServiceTableLocked = 'The service database is locked.';
  sNERR_ServiceTableFull = 'The service table is full.';
  sNERR_ServiceInstalled = 'The requested service has already been started.';
  sNERR_ServiceEntryLocked = 'The service does not respond to control actions.';
  sNERR_ServiceNotInstalled = 'The service has not been started.';
  sNERR_BadServiceName = 'The service name is invalid.';
  sNERR_ServiceCtlTimeout = 'The service is not responding to the control function.';
  sNERR_ServiceCtlBusy = 'The service control is busy.';
  sNERR_BadServiceProgName = 'The configuration file contains an invalid service program name.';
  sNERR_ServiceNotCtrl = 'The service could not be controlled in its present state.';
  sNERR_ServiceKillProc = 'The service ended abnormally.';
  sNERR_ServiceCtlNotValid = 'The requested pause or stop is not valid for this service.';
  sNERR_NotInDispatchTbl = 'The service control dispatcher could not find the service name in the dispatch table.';
  sNERR_BadControlRecv = 'The service control dispatcher pipe read failed.';
  sNERR_ServiceNotStarting = 'A thread for the new service could not be created.';
  sNERR_AlreadyLoggedOn = 'This workstation is already logged on to the local-area network.';
  sNERR_NotLoggedOn = 'The workstation is not logged on to the local-area network.';
  sNERR_BadUsername = 'The user name or group name parameter is invalid.';
  sNERR_BadPassword = 'The password parameter is invalid.';
  sNERR_UnableToAddName_W = '@W The logon processor did not add the message alias.';
  sNERR_UnableToAddName_F = 'The logon processor did not add the message alias.';
  sNERR_UnableToDelName_W = '@W The logoff processor did not delete the message alias.';
  sNERR_UnableToDelName_F = 'The logoff processor did not delete the message alias.';
  sNERR_LogonsPaused = 'Network logons are paused.';
  sNERR_LogonServerConflict = 'A centralized logon-server conflict occurred.';
  sNERR_LogonNoUserPath = 'The server is configured without a valid user path.';
  sNERR_LogonScriptError = 'An error occurred while loading or running the logon script.';
  sNERR_StandaloneLogon = 'The logon server was not specified.  Your computer will be logged on as STANDALONE.';
  sNERR_LogonServerNotFound = 'The logon server could not be found.';
  sNERR_LogonDomainExists = 'There is already a logon domain for this computer.';
  sNERR_NonValidatedLogon = 'The logon server could not validate the logon.';
  sNERR_ACFNotFound = 'The security database could not be found.';
  sNERR_GroupNotFound = 'The group name could not be found.';
  sNERR_UserNotFound = 'The user name could not be found.';
  sNERR_ResourceNotFound = 'The resource name could not be found.';
  sNERR_GroupExists = 'The group already exists.';
  sNERR_UserExists = 'The user account already exists.';
  sNERR_ResourceExists = 'The resource permission list already exists.';
  sNERR_NotPrimary = 'This operation is only allowed on the primary domain controller of the domain.';
  sNERR_ACFNotLoaded = 'The security database has not been started.';
  sNERR_ACFNoRoom = 'There are too many names in the user accounts database.';
  sNERR_ACFFileIOFail = 'A disk I/O failure occurred.';
  sNERR_ACFTooManyLists = 'The limit of 64 entries per resource was exceeded.';
  sNERR_UserLogon = 'Deleting a user with a session is not allowed.';
  sNERR_ACFNoParent = 'The parent directory could not be located.';
  sNERR_CanNotGrowSegment = 'Unable to add to the security database session cache segment.';
  sNERR_SpeGroupOp = 'This operation is not allowed on this special group.';
  sNERR_NotInCache = 'This user is not cached in user accounts database session cache.';
  sNERR_UserInGroup = 'The user already belongs to this group.';
  sNERR_UserNotInGroup = 'The user does not belong to this group.';
  sNERR_AccountUndefined = 'This user account is undefined.';
  sNERR_AccountExpired = 'This user account has expired.';
  sNERR_InvalidWorkstation = 'The user is not allowed to log on from this workstation.';
  sNERR_InvalidLogonHours = 'The user is not allowed to log on at this time.';
  sNERR_PasswordExpired = 'The password of this user has expired.';
  sNERR_PasswordCantChange = 'The password of this user cannot change.';
  sNERR_PasswordHistConflict = 'This password cannot be used now.';
  sNERR_PasswordTooShort = 'The password is shorter than required.';
  sNERR_PasswordTooRecent = 'The password of this user is too recent to change.';
  sNERR_InvalidDatabase = 'The security database is corrupted.';
  sNERR_DatabaseUpToDate = 'No updates are necessary to this replicant network/local security database.';
  sNERR_SyncRequired = 'This replicant database is outdated; synchronization is required.';
  sNERR_UseNotFound = 'The network connection could not be found.';
  sNERR_BadAsgType = 'This asg_type is invalid.';
  sNERR_DeviceIsShared = 'This device is currently being shared.';
  sNERR_NoComputerName = 'The computer name could not be added as a message alias.  The name may already exist on the network.';
  sNERR_MsgAlreadyStarted = 'The Messenger service is already started.';
  sNERR_MsgInitFailed = 'The Messenger service failed to start.';
  sNERR_NameNotFound = 'The message alias could not be found on the network.';
  sNERR_AlreadyForwarded = 'This message alias has already been forwarded.';
  sNERR_AddForwarded = 'This message alias has been added but is still forwarded.';
  sNERR_AlreadyExists = 'This message alias already exists locally.';
  sNERR_TooManyNames = 'The maximum number of added message aliases has been exceeded.';
  sNERR_DelComputerName = 'The computer name could not be deleted.';
  sNERR_LocalForward = 'Messages cannot be forwarded back to the same workstation.';
  sNERR_GrpMsgProcessor = 'An error occurred in the domain message processor.';
  sNERR_PausedRemote = 'The message was sent, but the recipient has paused the Messenger service.';
  sNERR_BadReceive = 'The message was sent but not received.';
  sNERR_NameInUse = 'The message alias is currently in use. Try again later.';
  sNERR_MsgNotStarted = 'The Messenger service has not been started.';
  sNERR_NotLocalName = 'The name is not on the local computer.';
  sNERR_NoForwardName = 'The forwarded message alias could not be found on the network.';
  sNERR_RemoteFull = 'The message alias table on the remote station is full.';
  sNERR_NameNotForwarded = 'Messages for this alias are not currently being forwarded.';
  sNERR_TruncatedBroadcast = 'The broadcast message was truncated.';
  sNERR_InvalidDevice = 'This is an invalid device name.';
  sNERR_WriteFault = 'A write fault occurred.';
  sNERR_DuplicateName = 'A duplicate message alias exists on the network.';
  sNERR_DeleteLater = '@W This message alias will be deleted later.';
  sNERR_IncompleteDel = 'The message alias was not successfully deleted from all networks.';
  sNERR_MultipleNets = 'This operation is not supported on computers with multiple networks.';
  sNERR_NetNameNotFound = 'This shared resource does not exist.';
  sNERR_DeviceNotShared = 'This device is not shared.';
  sNERR_ClientNameNotFound = 'A session does not exist with that computer name.';
  sNERR_FileIdNotFound = 'There is not an open file with that identification number.';
  sNERR_ExecFailure = 'A failure occurred when executing a remote administration command.';
  sNERR_TmpFile = 'A failure occurred when opening a remote temporary file.';
  sNERR_TooMuchData = 'The data returned from a remote administration command has been truncated to 64K.';
  sNERR_DeviceShareConflict = 'This device cannot be shared as both a spooled and a non-spooled resource.';
  sNERR_BrowserTableIncomplete = 'The information in the list of servers may be incorrect.';
  sNERR_NotLocalDomain = 'The computer is not active in this domain.';
  sNERR_IsDfsShare = 'The share must be removed from the Distributed File System before it can be deleted.';
  sNERR_DevInvalidOpCode = 'The operation is invalid for this device.';
  sNERR_DevNotFound = 'This device cannot be shared.';
  sNERR_DevNotOpen = 'This device was not open.';
  sNERR_BadQueueDevString = 'This device name list is invalid.';
  sNERR_BadQueuePriority = 'The queue priority is invalid.';
  sNERR_NoCommDevs = 'There are no shared communication devices.';
  sNERR_QueueNotFound = 'The queue you specified does not exist.';
  sNERR_BadDevString = 'This list of devices is invalid.';
  sNERR_BadDev = 'The requested device is invalid.';
  sNERR_InUseBySpooler = 'This device is already in use by the spooler.';
  sNERR_CommDevInUse = 'This device is already in use as a communication device.';
  sNERR_InvalidComputer = 'This computer name is invalid.';
  sNERR_MaxLenExceeded = 'The string and prefix specified are too long.';
  sNERR_BadComponent = 'This path component is invalid.';
  sNERR_CantType = 'Could not determine the type of input.';
  sNERR_TooManyEntries = 'The buffer for types is not big enough.';
  sNERR_ProfileFileTooBig = 'Profile files cannot exceed 64K.';
  sNERR_ProfileOffset = 'The start offset is out of range.';
  sNERR_ProfileCleanup = 'The system cannot delete current connections to network resources.';
  sNERR_ProfileUnknownCmd = 'The system was unable to parse the command line in this file.';
  sNERR_ProfileLoadErr = 'An error occurred while loading the profile file.';
  sNERR_ProfileSaveErr = '@W Errors occurred while saving the profile file.  The profile was partially saved.';
  sNERR_LogOverflow = 'Log file %1 is full.';
  sNERR_LogFileChanged = 'This log file has changed between reads.';
  sNERR_LogFileCorrupt = 'Log file %1 is corrupt.';
  sNERR_SourceIsDir = 'The source path cannot be a directory.';
  sNERR_BadSource = 'The source path is illegal.';
  sNERR_BadDest = 'The destination path is illegal.';
  sNERR_DifferentServers = 'The source and destination paths are on different servers.';
  sNERR_RunSrvPaused = 'The Run server you requested is paused.';
  sNERR_ErrCommRunSrv = 'An error occurred when communicating with a Run server.';
  sNERR_ErrorExecingGhost = 'An error occurred when starting a background process.';
  sNERR_ShareNotFound = 'The shared resource you are connected to could not be found.';
  sNERR_InvalidLana = 'The LAN adapter number is invalid.';
  sNERR_OpenFiles = 'There are open files on the connection.';
  sNERR_ActiveConns = 'Active connections still exist.';
  sNERR_BadPasswordCore = 'This share name or password is invalid.';
  sNERR_DevInUse = 'The device is being accessed by an active process.';
  sNERR_LocalDrive = 'The drive letter is in use locally.';
  sNERR_AlertExists = 'The specified client is already registered for the specified event.';
  sNERR_TooManyAlerts = 'The alert table is full.';
  sNERR_NoSuchAlert = 'An invalid or nonexistent alert name was raised.';
  sNERR_BadRecipient = 'The alert recipient is invalid.';
  sNERR_AcctLimitExceeded = 'A user''s session with this server has been deleted because the user''s logon hours are no longer valid.';
  sNERR_InvalidLogSeek = 'The log file does not contain the requested record number.';
  sNERR_BadUasConfig = 'The user accounts database is not configured correctly.';
  sNERR_InvalidUASOp = 'This operation is not permitted when the Netlogon service is running.';
  sNERR_LastAdmin = 'This operation is not allowed on the last administrative account.';
  sNERR_DCNotFound = 'Could not find domain controller for this domain.';
  sNERR_LogonTrackingError = 'Could not set logon information for this user.';
  sNERR_NetlogonNotStarted = 'The Netlogon service has not been started.';
  sNERR_CanNotGrowUASFile = 'Unable to add to the user accounts database.';
  sNERR_TimeDiffAtDC = 'This server''s clock is not synchronized with the primary domain controller''s clock.';
  sNERR_PasswordMismatch = 'A password mismatch has been detected.';
  sNERR_NoSuchServer = 'The server identification does not specify a valid server.';
  sNERR_NoSuchSession = 'The session identification does not specify a valid session.';
  sNERR_NoSuchConnection = 'The connection identification does not specify a valid connection.';
  sNERR_TooManyServers = 'There is no space for another entry in the table of available servers.';
  sNERR_TooManySessions = 'The server has reached the maximum number of sessions it supports.';
  sNERR_TooManyConnections = 'The server has reached the maximum number of connections it supports.';
  sNERR_TooManyFiles = 'The server cannot open more files because it has reached its maximum number.';
  sNERR_NoAlternateServers = 'There are no alternate servers registered on this server.';
  sNERR_TryDownLevel = 'Try down-level (remote admin protocol); version of API instead.';
  sNERR_UPSDriverNotStarted = 'The UPS driver could not be accessed by the UPS service.';
  sNERR_UPSInvalidConfig = 'The UPS service is not configured correctly.';
  sNERR_UPSInvalidCommPort = 'The UPS service could not access the specified Comm Port.';
  sNERR_UPSSignalAsserted = 'The UPS indicated a line fail or low battery situation. Service not started.';
  sNERR_UPSShutdownFailed = 'The UPS service failed to perform a system shut down.';
  sNERR_BadDosRetCode = 'The program below returned an MS-DOS error code:';
  sNERR_ProgNeedsExtraMem = 'The program below needs more memory:';
  sNERR_BadDosFunction = 'The program below called an unsupported MS-DOS function:';
  sNERR_RemoteBootFailed = 'The workstation failed to boot.';
  sNERR_BadFileCheckSum = 'The file below is corrupt.';
  sNERR_NoRplBootSystem = 'No loader is specified in the boot-block definition file.';
  sNERR_RplLoadrNetBiosErr = 'NetBIOS returned an error: The NCB and SMB are dumped above.';
  sNERR_RplLoadrDiskErr = 'A disk I/O error occurred.';
  sNERR_ImageParamErr = 'Image parameter substitution failed.';
  sNERR_TooManyImageParams = 'Too many image parameters cross disk sector boundaries.';
  sNERR_NonDosFloppyUsed = 'The image was not generated from an MS-DOS diskette formatted with /S.';
  sNERR_RplBootRestart = 'Remote boot will be restarted later.';
  sNERR_RplSrvrCallFailed = 'The call to the Remoteboot server failed.';
  sNERR_CantConnectRplSrvr = 'Cannot connect to the Remoteboot server.';
  sNERR_CantOpenImageFile = 'Cannot open image file on the Remoteboot server.';
  sNERR_CallingRplSrvr = 'Connecting to the Remoteboot server...';
  sNERR_StartingRplBoot = 'Connecting to the Remoteboot server...';
  sNERR_RplBootServiceTerm = 'Remote boot service was stopped; check the error log for the cause of the problem.';
  sNERR_RplBootStartFailed = 'Remote boot startup failed; check the error log for the cause of the problem.';
  sNERR_RPL_CONNECTED = 'A second connection to a Remoteboot resource is not allowed.';
  sNERR_BrowserConfiguredToNotRun = 'The browser service was configured with MaintainServerList=No.';
  sNERR_RplNoAdaptersStarted = 'Service failed to start since none of the network adapters started with this service.';
  sNERR_RplBadRegistry = 'Service failed to start due to bad startup information in the registry.';
  sNERR_RplBadDatabase = 'Service failed to start because its database is absent or corrupt.';
  sNERR_RplRplfilesShare = 'Service failed to start because RPLFILES share is absent.';
  sNERR_RplNotRplServer = 'Service failed to start because RPLUSER group is absent.';
  sNERR_RplCannotEnum = 'Cannot enumerate service records.';
  sNERR_RplWkstaInfoCorrupted = 'Workstation record information has been corrupted.';
  sNERR_RplWkstaNotFound = 'Workstation record was not found.';
  sNERR_RplWkstaNameUnavailable = 'Workstation name is in use by some other workstation.';
  sNERR_RplProfileInfoCorrupted = 'Profile record information has been corrupted.';
  sNERR_RplProfileNotFound = 'Profile record was not found.';
  sNERR_RplProfileNameUnavailable = 'Profile name is in use by some other profile.';
  sNERR_RplProfileNotEmpty = 'There are workstations using this profile.';
  sNERR_RplConfigInfoCorrupted = 'Configuration record information has been corrupted.';
  sNERR_RplConfigNotFound = 'Configuration record was not found.';
  sNERR_RplAdapterInfoCorrupted = 'Adapter id record information has been corrupted.';
  sNERR_RplInternal = 'An internal service error has occurred.';
  sNERR_RplVendorInfoCorrupted = 'Vendor id record information has been corrupted.';
  sNERR_RplBootInfoCorrupted = 'Boot block record information has been corrupted.';
  sNERR_RplWkstaNeedsUserAcct = 'The user account for this workstation record is missing.';
  sNERR_RplNeedsRPLUSERAcct = 'The RPLUSER local group could not be found.';
  sNERR_RplBootNotFound = 'Boot block record was not found.';
  sNERR_RplIncompatibleProfile = 'Chosen profile is incompatible with this workstation.';
  sNERR_RplAdapterNameUnavailable = 'Chosen network adapter id is in use by some other workstation.';
  sNERR_RplConfigNotEmpty = 'There are profiles using this configuration.';
  sNERR_RplBootInUse = 'There are workstations, profiles or configurations using this boot block.';
  sNERR_RplBackupDatabase = 'Service failed to backup Remoteboot database.';
  sNERR_RplAdapterNotFound = 'Adapter record was not found.';
  sNERR_RplVendorNotFound = 'Vendor record was not found.';
  sNERR_RplVendorNameUnavailable = 'Vendor name is in use by some other vendor record.';
  sNERR_RplBootNameUnavailable = '(boot name, vendor id); is in use by some other boot block record.';
  sNERR_RplConfigNameUnavailable = 'Configuration name is in use by some other configuration.';
  sNERR_DfsInternalCorruption = 'The internal database maintained by the Dfs service is corrupt';
  sNERR_DfsVolumeDataCorrupt = 'One of the records in the internal Dfs database is corrupt';
  sNERR_DfsNoSuchVolume = 'There is no volume whose entry path matches the input Entry Path';
  sNERR_DfsVolumeAlreadyExists = 'A volume with the given name already exists';
  sNERR_DfsAlreadyShared = 'The server share specified is already shared in the Dfs';
  sNERR_DfsNoSuchShare = 'The indicated server share does not support the indicated Dfs volume';
  sNERR_DfsNotALeafVolume = 'The operation is not valid on a non-leaf volume';
  sNERR_DfsLeafVolume = 'The operation is not valid on a leaf volume';
  sNERR_DfsVolumeHasMultipleServers = 'The operation is ambiguous because the volume has multiple servers';
  sNERR_DfsCantCreateJunctionPoint = 'Unable to create a junction point';
  sNERR_DfsServerNotDfsAware = 'The server is not Dfs Aware';
  sNERR_DfsBadRenamePath = 'The specified rename target path is invalid';
  sNERR_DfsVolumeIsOffline = 'The specified Dfs volume is offline';
  sNERR_DfsNoSuchServer = 'The specified server is not a server for this volume';
  sNERR_DfsCyclicalName = 'A cycle in the Dfs name was detected';
  sNERR_DfsNotSupportedInServerDfs = 'The operation is not supported on a server-based Dfs';
  sNERR_DfsDuplicateService = 'This volume is already supported by the specified server-share';
  sNERR_DfsCantRemoveLastServerShare = 'Can''t remove the last server-share supporting this volume';
  sNERR_DfsVolumeIsInterDfs = 'The operation is not supported for an Inter-Dfs volume';
  sNERR_DfsInconsistent = 'The internal state of the Dfs Service has become inconsistent';
  sNERR_DfsServerUpgraded = 'The Dfs Service has been installed on the specified server';
  sNERR_DfsDataIsIdentical = 'The Dfs data being reconciled is identical';
  sNERR_DfsCantRemoveDfsRoot = 'The Dfs root volume cannot be deleted - Uninstall Dfs if required';
  sNERR_DfsChildOrParentInDfs = 'A child or parent directory of the share is already in a Dfs';
  sNERR_DfsInternalError = 'Dfs internal error';
  sNERR_SetupAlreadyJoined = 'This machine is already joined to a domain.';
  sNERR_SetupNotJoined = 'This machine is not currently joined to a domain.';
  sNERR_SetupDomainController = 'This machine is a domain controller and cannot be unjoined from a domain.';
  sNERR_DefaultJoinRequired = 'The destination domain controller does not support creating machine accounts in OUs';
  sNERR_InvalidWorkgroupName = 'The specified workgroup name is invalid';
  sNERR_NameUsesIncompatibleCodePage = 'The specified computer name is incompatible with the default language used on the domain controller';
  sNERR_ComputerAccountNotFound = 'The specified computer account could not be found';

function NetErrorMessage(ErrorCode: Integer): String;
begin
  case ErrorCode of
    NERR_NetNotStarted: Result := sNERR_NetNotStarted;
    NERR_UnknownServer: Result := sNERR_UnknownServer;
    NERR_ShareMem: Result := sNERR_ShareMem;
    NERR_NoNetworkResource: Result := sNERR_NoNetworkResource;
    NERR_RemoteOnly: Result := sNERR_RemoteOnly;
    NERR_DevNotRedirected: Result := sNERR_DevNotRedirected;
    NERR_ServerNotStarted: Result := sNERR_ServerNotStarted;
    NERR_ItemNotFound: Result := sNERR_ItemNotFound;
    NERR_UnknownDevDir: Result := sNERR_UnknownDevDir;
    NERR_RedirectedPath: Result := sNERR_RedirectedPath;
    NERR_DuplicateShare: Result := sNERR_DuplicateShare;
    NERR_NoRoom: Result := sNERR_NoRoom;
    NERR_TooManyItems: Result := sNERR_TooManyItems;
    NERR_InvalidMaxUsers: Result := sNERR_InvalidMaxUsers;
    NERR_BufTooSmall: Result := sNERR_BufTooSmall;
    NERR_RemoteErr: Result := sNERR_RemoteErr;
    NERR_LanmanIniError: Result := sNERR_LanmanIniError;
    NERR_NetworkError: Result := sNERR_NetworkError;
    NERR_WkstaInconsistentState: Result := sNERR_WkstaInconsistentState;
    NERR_WkstaNotStarted: Result := sNERR_WkstaNotStarted;
    NERR_BrowserNotStarted: Result := sNERR_BrowserNotStarted;
    NERR_InternalError: Result := sNERR_InternalError;
    NERR_BadTransactConfig: Result := sNERR_BadTransactConfig;
    NERR_InvalidAPI: Result := sNERR_InvalidAPI;
    NERR_BadEventName: Result := sNERR_BadEventName;
    NERR_DupNameReboot: Result := sNERR_DupNameReboot;
    NERR_CfgCompNotFound: Result := sNERR_CfgCompNotFound;
    NERR_CfgParamNotFound: Result := sNERR_CfgParamNotFound;
    NERR_LineTooLong: Result := sNERR_LineTooLong;
    NERR_QNotFound: Result := sNERR_QNotFound;
    NERR_JobNotFound: Result := sNERR_JobNotFound;
    NERR_DestNotFound: Result := sNERR_DestNotFound;
    NERR_DestExists: Result := sNERR_DestExists;
    NERR_QExists: Result := sNERR_QExists;
    NERR_QNoRoom: Result := sNERR_QNoRoom;
    NERR_JobNoRoom: Result := sNERR_JobNoRoom;
    NERR_DestNoRoom: Result := sNERR_DestNoRoom;
    NERR_DestIdle: Result := sNERR_DestIdle;
    NERR_DestInvalidOp: Result := sNERR_DestInvalidOp;
    NERR_ProcNoRespond: Result := sNERR_ProcNoRespond;
    NERR_SpoolerNotLoaded: Result := sNERR_SpoolerNotLoaded;
    NERR_DestInvalidState: Result := sNERR_DestInvalidState;
    NERR_QInvalidState: Result := sNERR_QInvalidState;
    NERR_JobInvalidState: Result := sNERR_JobInvalidState;
    NERR_SpoolNoMemory: Result := sNERR_SpoolNoMemory;
    NERR_DriverNotFound: Result := sNERR_DriverNotFound;
    NERR_DataTypeInvalid: Result := sNERR_DataTypeInvalid;
    NERR_ProcNotFound: Result := sNERR_ProcNotFound;
    NERR_ServiceTableLocked: Result := sNERR_ServiceTableLocked;
    NERR_ServiceTableFull: Result := sNERR_ServiceTableFull;
    NERR_ServiceInstalled: Result := sNERR_ServiceInstalled;
    NERR_ServiceEntryLocked: Result := sNERR_ServiceEntryLocked;
    NERR_ServiceNotInstalled: Result := sNERR_ServiceNotInstalled;
    NERR_BadServiceName: Result := sNERR_BadServiceName;
    NERR_ServiceCtlTimeout: Result := sNERR_ServiceCtlTimeout;
    NERR_ServiceCtlBusy: Result := sNERR_ServiceCtlBusy;
    NERR_BadServiceProgName: Result := sNERR_BadServiceProgName;
    NERR_ServiceNotCtrl: Result := sNERR_ServiceNotCtrl;
    NERR_ServiceKillProc: Result := sNERR_ServiceKillProc;
    NERR_ServiceCtlNotValid: Result := sNERR_ServiceCtlNotValid;
    NERR_NotInDispatchTbl: Result := sNERR_NotInDispatchTbl;
    NERR_BadControlRecv: Result := sNERR_BadControlRecv;
    NERR_ServiceNotStarting: Result := sNERR_ServiceNotStarting;
    NERR_AlreadyLoggedOn: Result := sNERR_AlreadyLoggedOn;
    NERR_NotLoggedOn: Result := sNERR_NotLoggedOn;
    NERR_BadUsername: Result := sNERR_BadUsername;
    NERR_BadPassword: Result := sNERR_BadPassword;
    NERR_UnableToAddName_W: Result := sNERR_UnableToAddName_W;
    NERR_UnableToAddName_F: Result := sNERR_UnableToAddName_F;
    NERR_UnableToDelName_W: Result := sNERR_UnableToDelName_W;
    NERR_UnableToDelName_F: Result := sNERR_UnableToDelName_F;
    NERR_LogonsPaused: Result := sNERR_LogonsPaused;
    NERR_LogonServerConflict: Result := sNERR_LogonServerConflict;
    NERR_LogonNoUserPath: Result := sNERR_LogonNoUserPath;
    NERR_LogonScriptError: Result := sNERR_LogonScriptError;
    NERR_StandaloneLogon: Result := sNERR_StandaloneLogon;
    NERR_LogonServerNotFound: Result := sNERR_LogonServerNotFound;
    NERR_LogonDomainExists: Result := sNERR_LogonDomainExists;
    NERR_NonValidatedLogon: Result := sNERR_NonValidatedLogon;
    NERR_ACFNotFound: Result := sNERR_ACFNotFound;
    NERR_GroupNotFound: Result := sNERR_GroupNotFound;
    NERR_UserNotFound: Result := sNERR_UserNotFound;
    NERR_ResourceNotFound: Result := sNERR_ResourceNotFound;
    NERR_GroupExists: Result := sNERR_GroupExists;
    NERR_UserExists: Result := sNERR_UserExists;
    NERR_ResourceExists: Result := sNERR_ResourceExists;
    NERR_NotPrimary: Result := sNERR_NotPrimary;
    NERR_ACFNotLoaded: Result := sNERR_ACFNotLoaded;
    NERR_ACFNoRoom: Result := sNERR_ACFNoRoom;
    NERR_ACFFileIOFail: Result := sNERR_ACFFileIOFail;
    NERR_ACFTooManyLists: Result := sNERR_ACFTooManyLists;
    NERR_UserLogon: Result := sNERR_UserLogon;
    NERR_ACFNoParent: Result := sNERR_ACFNoParent;
    NERR_CanNotGrowSegment: Result := sNERR_CanNotGrowSegment;
    NERR_SpeGroupOp: Result := sNERR_SpeGroupOp;
    NERR_NotInCache: Result := sNERR_NotInCache;
    NERR_UserInGroup: Result := sNERR_UserInGroup;
    NERR_UserNotInGroup: Result := sNERR_UserNotInGroup;
    NERR_AccountUndefined: Result := sNERR_AccountUndefined;
    NERR_AccountExpired: Result := sNERR_AccountExpired;
    NERR_InvalidWorkstation: Result := sNERR_InvalidWorkstation;
    NERR_InvalidLogonHours: Result := sNERR_InvalidLogonHours;
    NERR_PasswordExpired: Result := sNERR_PasswordExpired;
    NERR_PasswordCantChange: Result := sNERR_PasswordCantChange;
    NERR_PasswordHistConflict: Result := sNERR_PasswordHistConflict;
    NERR_PasswordTooShort: Result := sNERR_PasswordTooShort;
    NERR_PasswordTooRecent: Result := sNERR_PasswordTooRecent;
    NERR_InvalidDatabase: Result := sNERR_InvalidDatabase;
    NERR_DatabaseUpToDate: Result := sNERR_DatabaseUpToDate;
    NERR_SyncRequired: Result := sNERR_SyncRequired;
    NERR_UseNotFound: Result := sNERR_UseNotFound;
    NERR_BadAsgType: Result := sNERR_BadAsgType;
    NERR_DeviceIsShared: Result := sNERR_DeviceIsShared;
    NERR_NoComputerName: Result := sNERR_NoComputerName;
    NERR_MsgAlreadyStarted: Result := sNERR_MsgAlreadyStarted;
    NERR_MsgInitFailed: Result := sNERR_MsgInitFailed;
    NERR_NameNotFound: Result := sNERR_NameNotFound;
    NERR_AlreadyForwarded: Result := sNERR_AlreadyForwarded;
    NERR_AddForwarded: Result := sNERR_AddForwarded;
    NERR_AlreadyExists: Result := sNERR_AlreadyExists;
    NERR_TooManyNames: Result := sNERR_TooManyNames;
    NERR_DelComputerName: Result := sNERR_DelComputerName;
    NERR_LocalForward: Result := sNERR_LocalForward;
    NERR_GrpMsgProcessor: Result := sNERR_GrpMsgProcessor;
    NERR_PausedRemote: Result := sNERR_PausedRemote;
    NERR_BadReceive: Result := sNERR_BadReceive;
    NERR_NameInUse: Result := sNERR_NameInUse;
    NERR_MsgNotStarted: Result := sNERR_MsgNotStarted;
    NERR_NotLocalName: Result := sNERR_NotLocalName;
    NERR_NoForwardName: Result := sNERR_NoForwardName;
    NERR_RemoteFull: Result := sNERR_RemoteFull;
    NERR_NameNotForwarded: Result := sNERR_NameNotForwarded;
    NERR_TruncatedBroadcast: Result := sNERR_TruncatedBroadcast;
    NERR_InvalidDevice: Result := sNERR_InvalidDevice;
    NERR_WriteFault: Result := sNERR_WriteFault;
    NERR_DuplicateName: Result := sNERR_DuplicateName;
    NERR_DeleteLater: Result := sNERR_DeleteLater;
    NERR_IncompleteDel: Result := sNERR_IncompleteDel;
    NERR_MultipleNets: Result := sNERR_MultipleNets;
    NERR_NetNameNotFound: Result := sNERR_NetNameNotFound;
    NERR_DeviceNotShared: Result := sNERR_DeviceNotShared;
    NERR_ClientNameNotFound: Result := sNERR_ClientNameNotFound;
    NERR_FileIdNotFound: Result := sNERR_FileIdNotFound;
    NERR_ExecFailure: Result := sNERR_ExecFailure;
    NERR_TmpFile: Result := sNERR_TmpFile;
    NERR_TooMuchData: Result := sNERR_TooMuchData;
    NERR_DeviceShareConflict: Result := sNERR_DeviceShareConflict;
    NERR_BrowserTableIncomplete: Result := sNERR_BrowserTableIncomplete;
    NERR_NotLocalDomain: Result := sNERR_NotLocalDomain;
    NERR_IsDfsShare: Result := sNERR_IsDfsShare;
    NERR_DevInvalidOpCode: Result := sNERR_DevInvalidOpCode;
    NERR_DevNotFound: Result := sNERR_DevNotFound;
    NERR_DevNotOpen: Result := sNERR_DevNotOpen;
    NERR_BadQueueDevString: Result := sNERR_BadQueueDevString;
    NERR_BadQueuePriority: Result := sNERR_BadQueuePriority;
    NERR_NoCommDevs: Result := sNERR_NoCommDevs;
    NERR_QueueNotFound: Result := sNERR_QueueNotFound;
    NERR_BadDevString: Result := sNERR_BadDevString;
    NERR_BadDev: Result := sNERR_BadDev;
    NERR_InUseBySpooler: Result := sNERR_InUseBySpooler;
    NERR_CommDevInUse: Result := sNERR_CommDevInUse;
    NERR_InvalidComputer: Result := sNERR_InvalidComputer;
    NERR_MaxLenExceeded: Result := sNERR_MaxLenExceeded;
    NERR_BadComponent: Result := sNERR_BadComponent;
    NERR_CantType: Result := sNERR_CantType;
    NERR_TooManyEntries: Result := sNERR_TooManyEntries;
    NERR_ProfileFileTooBig: Result := sNERR_ProfileFileTooBig;
    NERR_ProfileOffset: Result := sNERR_ProfileOffset;
    NERR_ProfileCleanup: Result := sNERR_ProfileCleanup;
    NERR_ProfileUnknownCmd: Result := sNERR_ProfileUnknownCmd;
    NERR_ProfileLoadErr: Result := sNERR_ProfileLoadErr;
    NERR_ProfileSaveErr: Result := sNERR_ProfileSaveErr;
    NERR_LogOverflow: Result := sNERR_LogOverflow;
    NERR_LogFileChanged: Result := sNERR_LogFileChanged;
    NERR_LogFileCorrupt: Result := sNERR_LogFileCorrupt;
    NERR_SourceIsDir: Result := sNERR_SourceIsDir;
    NERR_BadSource: Result := sNERR_BadSource;
    NERR_BadDest: Result := sNERR_BadDest;
    NERR_DifferentServers: Result := sNERR_DifferentServers;
    NERR_RunSrvPaused: Result := sNERR_RunSrvPaused;
    NERR_ErrCommRunSrv: Result := sNERR_ErrCommRunSrv;
    NERR_ErrorExecingGhost: Result := sNERR_ErrorExecingGhost;
    NERR_ShareNotFound: Result := sNERR_ShareNotFound;
    NERR_InvalidLana: Result := sNERR_InvalidLana;
    NERR_OpenFiles: Result := sNERR_OpenFiles;
    NERR_ActiveConns: Result := sNERR_ActiveConns;
    NERR_BadPasswordCore: Result := sNERR_BadPasswordCore;
    NERR_DevInUse: Result := sNERR_DevInUse;
    NERR_LocalDrive: Result := sNERR_LocalDrive;
    NERR_AlertExists: Result := sNERR_AlertExists;
    NERR_TooManyAlerts: Result := sNERR_TooManyAlerts;
    NERR_NoSuchAlert: Result := sNERR_NoSuchAlert;
    NERR_BadRecipient: Result := sNERR_BadRecipient;
    NERR_AcctLimitExceeded: Result := sNERR_AcctLimitExceeded;
    NERR_InvalidLogSeek: Result := sNERR_InvalidLogSeek;
    NERR_BadUasConfig: Result := sNERR_BadUasConfig;
    NERR_InvalidUASOp: Result := sNERR_InvalidUASOp;
    NERR_LastAdmin: Result := sNERR_LastAdmin;
    NERR_DCNotFound: Result := sNERR_DCNotFound;
    NERR_LogonTrackingError: Result := sNERR_LogonTrackingError;
    NERR_NetlogonNotStarted: Result := sNERR_NetlogonNotStarted;
    NERR_CanNotGrowUASFile: Result := sNERR_CanNotGrowUASFile;
    NERR_TimeDiffAtDC: Result := sNERR_TimeDiffAtDC;
    NERR_PasswordMismatch: Result := sNERR_PasswordMismatch;
    NERR_NoSuchServer: Result := sNERR_NoSuchServer;
    NERR_NoSuchSession: Result := sNERR_NoSuchSession;
    NERR_NoSuchConnection: Result := sNERR_NoSuchConnection;
    NERR_TooManyServers: Result := sNERR_TooManyServers;
    NERR_TooManySessions: Result := sNERR_TooManySessions;
    NERR_TooManyConnections: Result := sNERR_TooManyConnections;
    NERR_TooManyFiles: Result := sNERR_TooManyFiles;
    NERR_NoAlternateServers: Result := sNERR_NoAlternateServers;
    NERR_TryDownLevel: Result := sNERR_TryDownLevel;
    NERR_UPSDriverNotStarted: Result := sNERR_UPSDriverNotStarted;
    NERR_UPSInvalidConfig: Result := sNERR_UPSInvalidConfig;
    NERR_UPSInvalidCommPort: Result := sNERR_UPSInvalidCommPort;
    NERR_UPSSignalAsserted: Result := sNERR_UPSSignalAsserted;
    NERR_UPSShutdownFailed: Result := sNERR_UPSShutdownFailed;
    NERR_BadDosRetCode: Result := sNERR_BadDosRetCode;
    NERR_ProgNeedsExtraMem: Result := sNERR_ProgNeedsExtraMem;
    NERR_BadDosFunction: Result := sNERR_BadDosFunction;
    NERR_RemoteBootFailed: Result := sNERR_RemoteBootFailed;
    NERR_BadFileCheckSum: Result := sNERR_BadFileCheckSum;
    NERR_NoRplBootSystem: Result := sNERR_NoRplBootSystem;
    NERR_RplLoadrNetBiosErr: Result := sNERR_RplLoadrNetBiosErr;
    NERR_RplLoadrDiskErr: Result := sNERR_RplLoadrDiskErr;
    NERR_ImageParamErr: Result := sNERR_ImageParamErr;
    NERR_TooManyImageParams: Result := sNERR_TooManyImageParams;
    NERR_NonDosFloppyUsed: Result := sNERR_NonDosFloppyUsed;
    NERR_RplBootRestart: Result := sNERR_RplBootRestart;
    NERR_RplSrvrCallFailed: Result := sNERR_RplSrvrCallFailed;
    NERR_CantConnectRplSrvr: Result := sNERR_CantConnectRplSrvr;
    NERR_CantOpenImageFile: Result := sNERR_CantOpenImageFile;
    NERR_CallingRplSrvr: Result := sNERR_CallingRplSrvr;
    NERR_StartingRplBoot: Result := sNERR_StartingRplBoot;
    NERR_RplBootServiceTerm: Result := sNERR_RplBootServiceTerm;
    NERR_RplBootStartFailed: Result := sNERR_RplBootStartFailed;
    NERR_RPL_CONNECTED: Result := sNERR_RPL_CONNECTED;
    NERR_BrowserConfiguredToNotRun: Result := sNERR_BrowserConfiguredToNotRun;
    NERR_RplNoAdaptersStarted: Result := sNERR_RplNoAdaptersStarted;
    NERR_RplBadRegistry: Result := sNERR_RplBadRegistry;
    NERR_RplBadDatabase: Result := sNERR_RplBadDatabase;
    NERR_RplRplfilesShare: Result := sNERR_RplRplfilesShare;
    NERR_RplNotRplServer: Result := sNERR_RplNotRplServer;
    NERR_RplCannotEnum: Result := sNERR_RplCannotEnum;
    NERR_RplWkstaInfoCorrupted: Result := sNERR_RplWkstaInfoCorrupted;
    NERR_RplWkstaNotFound: Result := sNERR_RplWkstaNotFound;
    NERR_RplWkstaNameUnavailable: Result := sNERR_RplWkstaNameUnavailable;
    NERR_RplProfileInfoCorrupted: Result := sNERR_RplProfileInfoCorrupted;
    NERR_RplProfileNotFound: Result := sNERR_RplProfileNotFound;
    NERR_RplProfileNameUnavailable: Result := sNERR_RplProfileNameUnavailable;
    NERR_RplProfileNotEmpty: Result := sNERR_RplProfileNotEmpty;
    NERR_RplConfigInfoCorrupted: Result := sNERR_RplConfigInfoCorrupted;
    NERR_RplConfigNotFound: Result := sNERR_RplConfigNotFound;
    NERR_RplAdapterInfoCorrupted: Result := sNERR_RplAdapterInfoCorrupted;
    NERR_RplInternal: Result := sNERR_RplInternal;
    NERR_RplVendorInfoCorrupted: Result := sNERR_RplVendorInfoCorrupted;
    NERR_RplBootInfoCorrupted: Result := sNERR_RplBootInfoCorrupted;
    NERR_RplWkstaNeedsUserAcct: Result := sNERR_RplWkstaNeedsUserAcct;
    NERR_RplNeedsRPLUSERAcct: Result := sNERR_RplNeedsRPLUSERAcct;
    NERR_RplBootNotFound: Result := sNERR_RplBootNotFound;
    NERR_RplIncompatibleProfile: Result := sNERR_RplIncompatibleProfile;
    NERR_RplAdapterNameUnavailable: Result := sNERR_RplAdapterNameUnavailable;
    NERR_RplConfigNotEmpty: Result := sNERR_RplConfigNotEmpty;
    NERR_RplBootInUse: Result := sNERR_RplBootInUse;
    NERR_RplBackupDatabase: Result := sNERR_RplBackupDatabase;
    NERR_RplAdapterNotFound: Result := sNERR_RplAdapterNotFound;
    NERR_RplVendorNotFound: Result := sNERR_RplVendorNotFound;
    NERR_RplVendorNameUnavailable: Result := sNERR_RplVendorNameUnavailable;
    NERR_RplBootNameUnavailable: Result := sNERR_RplBootNameUnavailable;
    NERR_RplConfigNameUnavailable: Result := sNERR_RplConfigNameUnavailable;
    NERR_DfsInternalCorruption: Result := sNERR_DfsInternalCorruption;
    NERR_DfsVolumeDataCorrupt: Result := sNERR_DfsVolumeDataCorrupt;
    NERR_DfsNoSuchVolume: Result := sNERR_DfsNoSuchVolume;
    NERR_DfsVolumeAlreadyExists: Result := sNERR_DfsVolumeAlreadyExists;
    NERR_DfsAlreadyShared: Result := sNERR_DfsAlreadyShared;
    NERR_DfsNoSuchShare: Result := sNERR_DfsNoSuchShare;
    NERR_DfsNotALeafVolume: Result := sNERR_DfsNotALeafVolume;
    NERR_DfsLeafVolume: Result := sNERR_DfsLeafVolume;
    NERR_DfsVolumeHasMultipleServers: Result := sNERR_DfsVolumeHasMultipleServers;
    NERR_DfsCantCreateJunctionPoint: Result := sNERR_DfsCantCreateJunctionPoint;
    NERR_DfsServerNotDfsAware: Result := sNERR_DfsServerNotDfsAware;
    NERR_DfsBadRenamePath: Result := sNERR_DfsBadRenamePath;
    NERR_DfsVolumeIsOffline: Result := sNERR_DfsVolumeIsOffline;
    NERR_DfsNoSuchServer: Result := sNERR_DfsNoSuchServer;
    NERR_DfsCyclicalName: Result := sNERR_DfsCyclicalName;
    NERR_DfsNotSupportedInServerDfs: Result := sNERR_DfsNotSupportedInServerDfs;
    NERR_DfsDuplicateService: Result := sNERR_DfsDuplicateService;
    NERR_DfsCantRemoveLastServerShare: Result := sNERR_DfsCantRemoveLastServerShare;
    NERR_DfsVolumeIsInterDfs: Result := sNERR_DfsVolumeIsInterDfs;
    NERR_DfsInconsistent: Result := sNERR_DfsInconsistent;
    NERR_DfsServerUpgraded: Result := sNERR_DfsServerUpgraded;
    NERR_DfsDataIsIdentical: Result := sNERR_DfsDataIsIdentical;
    NERR_DfsCantRemoveDfsRoot: Result := sNERR_DfsCantRemoveDfsRoot;
    NERR_DfsChildOrParentInDfs: Result := sNERR_DfsChildOrParentInDfs;
    NERR_DfsInternalError: Result := sNERR_DfsInternalError;
    NERR_SetupAlreadyJoined: Result := sNERR_SetupAlreadyJoined;
    NERR_SetupNotJoined: Result := sNERR_SetupNotJoined;
    NERR_SetupDomainController: Result := sNERR_SetupDomainController;
    NERR_DefaultJoinRequired: Result := sNERR_DefaultJoinRequired;
    NERR_InvalidWorkgroupName: Result := sNERR_InvalidWorkgroupName;
    NERR_NameUsesIncompatibleCodePage: Result := sNERR_NameUsesIncompatibleCodePage;
    NERR_ComputerAccountNotFound: Result := sNERR_ComputerAccountNotFound;
  else
    Result := '';
  end;
end;

{$ELSE}

function NetErrorMessage(ErrorCode: Integer): String;
const
  ResourceLibName: array[Boolean] of PChar = ('netmsg95.dll', 'netmsg.dll');
var
  LibHandle: THandle;
  Buffer: array[0..1023] of Char;
begin
{$IFDEF DEBUG}
	LibHandle := LoadLibraryEx(ResourceLibName[True], 0, LOAD_LIBRARY_AS_DATAFILE);
{$ELSE}
	LibHandle := LoadLibraryEx(ResourceLibName[Win32Platform = VER_PLATFORM_WIN32_NT], 0, LOAD_LIBRARY_AS_DATAFILE);
{$ENDIF}
	if LibHandle <> 0 then begin
		SetString(Result, Buffer, LoadString(LibHandle, ErrorCode, Buffer, Sizeof(Buffer)));
		FreeLibrary(LibHandle);
	end else begin
		Result := '';
	end;
end;

{$ENDIF}

function SysAndNetErrorMessage(ErrorCode: Integer): String;
begin
  if (ErrorCode >= NERR_BASE) and (ErrorCode <= MAX_NERR) then
    Result := NetErrorMessage(ErrorCode)
  else
    Result := SysErrorMessage(ErrorCode);
end;

function NetCheck(RetCode: DWORD): DWORD;
var
  Error: EOSError;
begin
  if RetCode <> NERR_Success then
  begin
    Error := EOSError.CreateFmt(sNetError, [RetCode, SysAndNetErrorMessage(RetCode)]);
    Error.ErrorCode := RetCode;
    raise Error;
  end;
  Result := RetCode;
end;

end.
