{******************************************************************}
{                                                                  }
{ Borland Delphi Runtime Library                                   }
{ LanManager share functions for Windows NT interface unit         }
{                                                                  }
{ Portions created by Microsoft are                                }
{ Copyright (C) 1995-1999 Microsoft Corporation.                   }
{ All Rights Reserved.                                             }
{                                                                  }
{ The original file is: lmdfs.h, released 5 Jan 1999.              }
{ The original Pascal code is: LmDfs.pas, released 17 Jan 2000.    }
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

unit LmDfs;

{$I LANMAN.INC}

{$ALIGN ON}
{$MINENUMSIZE 4}
{$WEAKPACKAGEUNIT}

interface

uses
  Windows, LmCons;

(*$HPPEMIT '#include <lmcons.h>'*)
(*$HPPEMIT '#include <windef.h>'*)
(*$HPPEMIT '#include <lmdfs.h>'*)

// DFS Volume state

const
  DFS_VOLUME_STATE_OK            = 1;
  {$EXTERNALSYM DFS_VOLUME_STATE_OK}
  DFS_VOLUME_STATE_INCONSISTENT  = 2;
  {$EXTERNALSYM DFS_VOLUME_STATE_INCONSISTENT}
  DFS_VOLUME_STATE_OFFLINE       = 3;
  {$EXTERNALSYM DFS_VOLUME_STATE_OFFLINE}
  DFS_VOLUME_STATE_ONLINE        = 4;
  {$EXTERNALSYM DFS_VOLUME_STATE_ONLINE}

// DFS Storage State

  DFS_STORAGE_STATE_OFFLINE      = 1;
  {$EXTERNALSYM DFS_STORAGE_STATE_OFFLINE}
  DFS_STORAGE_STATE_ONLINE       = 2;
  {$EXTERNALSYM DFS_STORAGE_STATE_ONLINE}
  DFS_STORAGE_STATE_ACTIVE       = 4;
  {$EXTERNALSYM DFS_STORAGE_STATE_ACTIVE}

// Level 1:

type
  PDfsInfo1 = ^TDfsInfo1;
  _DFS_INFO_1 = packed record
    EntryPath: LPWSTR;                // Dfs name for the top of this piece of storage
  end;
  {$EXTERNALSYM _DFS_INFO_1}
  TDfsInfo1 = _DFS_INFO_1;
  DFS_INFO_1 = _DFS_INFO_1;
  {$EXTERNALSYM DFS_INFO_1}

// Level 2:

  PDfsInfo2 = ^TDfsInfo2;
  _DFS_INFO_2 = packed record
    EntryPath: LPWSTR;                // Dfs name for the top of this volume
    Comment: LPWSTR;                  // Comment for this volume
    State: DWORD;                     // State of this volume, one of DFS_VOLUME_STATE_*
    NumberOfStorages: DWORD;          // Number of storages for this volume
  end;
  {$EXTERNALSYM _DFS_INFO_2}
  TDfsInfo2 = _DFS_INFO_2;
  DFS_INFO_2 = _DFS_INFO_2;
  {$EXTERNALSYM DFS_INFO_2}


  PDfsStorageInfo = ^TDfsStorageInfo;
  _DFS_STORAGE_INFO = packed record
    State: ULONG;                     // State of this storage, one of DFS_STORAGE_STATE_*
                                      // possibly OR'd with DFS_STORAGE_STATE_ACTIVE
    ServerName: LPWSTR;               // Name of server hosting this storage
    ShareName: LPWSTR;                // Name of share hosting this storage
  end;
  {$EXTERNALSYM _DFS_STORAGE_INFO}
  TDfsStorageInfo = _DFS_STORAGE_INFO;
  DFS_STORAGE_INFO = _DFS_STORAGE_INFO;
  {$EXTERNALSYM DFS_STORAGE_INFO}

// Level 3:

  PDfsInfo3 = ^TDfsInfo3;
  _DFS_INFO_3 = packed record
    EntryPath: LPWSTR;                // Dfs name for the top of this volume
    Comment: LPWSTR;                  // Comment for this volume
    State: DWORD;                     // State of this volume, one of DFS_VOLUME_STATE_*
    NumberOfStorages: DWORD;          // Number of storage servers for this volume
    Storage: PDfsStorageInfo;         // An array (of NumberOfStorages elements) of storage-specific information.
  end;
  {$EXTERNALSYM _DFS_INFO_3}
  TDfsInfo3 = _DFS_INFO_3;
  DFS_INFO_3 = _DFS_INFO_3;
  {$EXTERNALSYM DFS_INFO_3}

// Level 4:

  PDfsInfo4 = ^TDfsInfo4;
  _DFS_INFO_4 = packed record
    EntryPath: LPWSTR;                // Dfs name for the top of this volume
    Comment: LPWSTR;                  // Comment for this volume
    State: DWORD;                     // State of this volume, one of DFS_VOLUME_STATE_*
    Timeout: ULONG;                   // Timeout, in seconds, of this junction point
    Guid: TGUID;                      // Guid of this junction point
    NumberOfStorages: DWORD;          // Number of storage servers for this volume
    Storage: PDfsStorageInfo;         // An array (of NumberOfStorages elements) of storage-specific information.
  end;
  {$EXTERNALSYM _DFS_INFO_4}
  TDfsInfo4 = _DFS_INFO_4;
  DFS_INFO_4 = _DFS_INFO_4;
  {$EXTERNALSYM DFS_INFO_4}

// Level 100:

  PDfsInfo100 = ^TDfsInfo100;
  _DFS_INFO_100 = packed record
    Comment: LPWSTR;                  // Comment for this volume or storage
  end;
  {$EXTERNALSYM _DFS_INFO_100}
  TDfsInfo100 = _DFS_INFO_100;
  DFS_INFO_100 = _DFS_INFO_100;
  {$EXTERNALSYM DFS_INFO_100}

// Level 101:

  PDfsInfo101 = ^TDfsInfo101;
  _DFS_INFO_101 = packed record
    State: DWORD;                     // State of this storage, one of DFS_STORAGE_STATE_*
                                      // possibly OR'd with DFS_STORAGE_STATE_ACTIVE
  end;
  {$EXTERNALSYM _DFS_INFO_101}
  TDfsInfo101 = _DFS_INFO_101;
  DFS_INFO_101 = _DFS_INFO_101;
  {$EXTERNALSYM DFS_INFO_101}

// Level 102:

  PDfsInfo102 = ^TDfsInfo102;
  _DFS_INFO_102 = packed record
    Timeout: ULONG;                   // Timeout, in seconds, of the junction
  end;
  {$EXTERNALSYM _DFS_INFO_102}
  TDfsInfo102 = _DFS_INFO_102;
  DFS_INFO_102 = _DFS_INFO_102;
  {$EXTERNALSYM DFS_INFO_102}

// Level 200:

  PDfsInfo200 = ^TDfsInfo200;
  _DFS_INFO_200 = packed record
    FtDfsName: LPWSTR;                // FtDfs name
  end;
  {$EXTERNALSYM _DFS_INFO_200}
  TDfsInfo200 = _DFS_INFO_200;
  DFS_INFO_200 = _DFS_INFO_200;
  {$EXTERNALSYM DFS_INFO_200}


// Add a new volume or additional storage for an existing volume at
// DfsEntryPath.

function NetDfsAdd(DfsEntryPath, ServerName, ShareName, Comment: LPWSTR;
  Flags: DWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetDfsAdd}

// Flags:

const
  DFS_ADD_VOLUME          = 1;   // Add a new volume to the DFS if not already there
  {$EXTERNALSYM DFS_ADD_VOLUME}
  DFS_RESTORE_VOLUME      = 2;   // Volume/Replica is being restored - do not verify share etc.
  {$EXTERNALSYM DFS_RESTORE_VOLUME}

// Setup/teardown API's for standard and FtDfs roots.

function NetDfsAddStdRoot(ServerName, RootShare, Comment: LPWSTR;
  Flags: DWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetDfsAddStdRoot}

function NetDfsRemoveStdRoot(ServerName: LPWSTR; RootShare: LPWSTR;
  Flags: DWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetDfsRemoveStdRoot}

function NetDfsAddFtRoot(ServerName, RootShare, FtDfsName, Comment: LPWSTR;
  Flags: DWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetDfsAddFtRoot}

function NetDfsRemoveFtRoot(ServerName, RootShare, FtDfsName: LPWSTR;
  Flags: DWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetDfsRemoveFtRoot}

function NetDfsRemoveFtRootForced(DomainName, ServerName, RootShare, FtDfsName: LPWSTR;
  Flags: DWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetDfsRemoveFtRootForced}

// Call to reinitialize the dfsmanager on a machine

function NetDfsManagerInitialize(ServerName: LPWSTR; Flags: DWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetDfsManagerInitialize}

function NetDfsAddStdRootForced(ServerName, RootShare, Comment, Store: LPWSTR): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetDfsAddStdRootForced}

function NetDfsGetDcAddress(ServerName: LPWSTR; DcIpAddress: LPWSTR;
  var IsRoot: Boolean; var Timeout: ULONG): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetDfsGetDcAddress}

function NetDfsSetDcAddress(ServerName: LPWSTR; DcIpAddress: LPWSTR;
  Timeout: ULONG; Flags: DWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetDfsSetDcAddress}

// Flags for NetDfsSetDcAddress()

const
  NET_DFS_SETDC_FLAGS                 = $00000000;
  {$EXTERNALSYM NET_DFS_SETDC_FLAGS}
  NET_DFS_SETDC_TIMEOUT               = $00000001;
  {$EXTERNALSYM NET_DFS_SETDC_TIMEOUT}
  NET_DFS_SETDC_INITPKT               = $00000002;
  {$EXTERNALSYM NET_DFS_SETDC_INITPKT}

// Structures used for site reporting

type
  PDfsSitenameInfo = ^TDfsSitenameInfo;
  DFS_SITENAME_INFO = packed record
    SiteFlags: ULONG;     // Below
    SiteName: LPWSTR;
  end;
  {$EXTERNALSYM DFS_SITENAME_INFO}
  TDfsSitenameInfo = DFS_SITENAME_INFO;


const
  DFS_SITE_PRIMARY    = $1;     // This site returned by DsGetSiteName()
  {$EXTERNALSYM DFS_SITE_PRIMARY}

type
  PDfsSitelistInfo = ^TDfsSitelistInfo;
  DFS_SITELIST_INFO = packed record
    cSites: ULONG;
    Site: TDfsSitenameInfo;
  end;
  {$EXTERNALSYM DFS_SITELIST_INFO}
  TDfsSitelistInfo = DFS_SITELIST_INFO;

// Remove a volume or additional storage for volume from the Dfs at
// DfsEntryPath. When applied to the last storage in a volume, removes
// the volume from the DFS.

function NetDfsRemove(DfsEntryPath, ServerName, ShareName: LPWSTR): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetDfsRemove}

// Get information about all of the volumes in the Dfs. DfsName is
// the "server" part of the UNC name used to refer to this particular Dfs.
//
// Valid levels are 1-4, 200

function NetDfsEnum(DfsName: LPWSTR; Level: DWORD; PrefMaxLen: DWORD;
  Buffer: Pointer; var EntriesRead: DWORD; ResumeHandle: PDWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetDfsEnum}

// Get information about the volume or storage.
// If ServerName and ShareName are specified, the information returned
// is specific to that server and share, else the information is specific
// to the volume as a whole.
//
// Valid levels are 1-4, 100

function NetDfsGetInfo(DfsEntryPath, ServerName, ShareName: LPWSTR;
  Level: DWORD; Buffer: Pointer): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetDfsGetInfo}

// Set info about the volume or storage.
// If ServerName and ShareName are specified, the information set is
// specific to that server and share, else the information is specific
// to the volume as a whole.
//
// Valid levels are 100, 101 and 102

function NetDfsSetInfo(DfsEntryPath, ServerName, ShareName: LPWSTR;
  Level: DWORD; Buffer: Pointer): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetDfsSetInfo}

// Get client's cached information about the volume or storage.
// If ServerName and ShareName are specified, the information returned
// is specific to that server and share, else the information is specific
// to the volume as a whole.
//
// Valid levels are 1-4

function NetDfsGetClientInfo(DfsEntryPath, ServerName, ShareName: LPWSTR;
  Level: DWORD; Buffer: Pointer): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetDfsGetClientInfo}

// Set client's cached info about the volume or storage.
// If ServerName and ShareName are specified, the information set is
// specific to that server and share, else the information is specific
// to the volume as a whole.
//
// Valid levels are 101 and 102.

function NetDfsSetClientInfo(DfsEntryPath, ServerName, ShareName: LPWSTR;
  Level: DWORD; Buffer: Pointer): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetDfsSetClientInfo}

// Move a DFS volume and all subordinate volumes from one place in the
// DFS to another place in the DFS.

function NetDfsMove(DfsEntryPath, DfsNewEntryPath: LPWSTR): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetDfsMove}

function NetDfsRename(Path, NewPath: LPWSTR): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetDfsRename}

implementation

function NetDfsAdd; external netapi32lib name 'NetDfsAdd';
function NetDfsAddStdRoot; external netapi32lib name 'NetDfsAddStdRoot';
function NetDfsRemoveStdRoot; external netapi32lib name 'NetDfsRemoveStdRoot';
function NetDfsAddFtRoot; external netapi32lib name 'NetDfsAddFtRoot';
function NetDfsRemoveFtRoot; external netapi32lib name 'NetDfsRemoveFtRoot';
function NetDfsRemoveFtRootForced; external netapi32lib name 'NetDfsRemoveFtRootForced';
function NetDfsManagerInitialize; external netapi32lib name 'NetDfsManagerInitialize';
function NetDfsAddStdRootForced; external netapi32lib name 'NetDfsAddStdRootForced';
function NetDfsGetDcAddress; external netapi32lib name 'NetDfsGetDcAddress';
function NetDfsSetDcAddress; external netapi32lib name 'NetDfsSetDcAddress';
function NetDfsRemove; external netapi32lib name 'NetDfsRemove';
function NetDfsEnum; external netapi32lib name 'NetDfsEnum';
function NetDfsGetInfo; external netapi32lib name 'NetDfsGetInfo';
function NetDfsSetInfo; external netapi32lib name 'NetDfsSetInfo';
function NetDfsGetClientInfo; external netapi32lib name 'NetDfsGetClientInfo';
function NetDfsSetClientInfo; external netapi32lib name 'NetDfsSetClientInfo';
function NetDfsMove; external netapi32lib name 'NetDfsMove';
function NetDfsRename; external netapi32lib name 'NetDfsRename';

end.
