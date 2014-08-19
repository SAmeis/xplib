{******************************************************************}
{                                                                  }
{ Borland Delphi Runtime Library                                   }
{ Svrapi library for Windows 9x interface unit                     }
{                                                                  }
{ Portions created by Microsoft are                                }
{ Copyright (C) 1995-1999 Microsoft Corporation.                   }
{ All Rights Reserved.                                             }
{                                                                  }
{ The original file is: svrapi.h, released 9 Feb 1998.             }
{ The original Pascal code is: Svrapi.pas, released 29 Dec 1999.   }
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

unit Svrapi;

{$I LANMAN.INC}

{$MINENUMSIZE 4}
{$IFNDEF LANMAN_DYNAMIC_LINK}
{$WEAKPACKAGEUNIT}
{$ENDIF}

interface

uses
  Windows, LmCons, LmErr;

(*$HPPEMIT '#include <lmcons.h>'*)
(*$HPPEMIT '#include <lmerr.h>'*)
(*$HPPEMIT '#include <svrapi.h>'*)


// ********************************************************************
// *                                                                  *
// *  About this file ...  SVRAPI.H                                   *
// *                                                                  *
// *  This file contains information about the NetAccess,             *
// *  NetConnection, NetFile, NetServer, NetSession, NetShare and     *
// *  NetSecurity APIs.                                               *
// *  There is a section for each set of APIs.                        *
// *  Each section contains:                                          *
// *                                                                  *
// *      Function prototypes.                                        *
// *                                                                  *
// *      Data structure templates.                                   *
// *                                                                  *
// *      Definition of special values.                               *
// *                                                                  *
// *      Description of level of Win95 peer server support           *
// *
// *  For background information refer to the Lan Manager Programmer's
// *  Reference.
// *
// *  WARNING:
// *      The APIs documented herein are not guaranteed to be supported
// * in future versions of Windows. Their primary purpose is to       *
// * administer Win95 peer servers.                                   *
// *                                                                  *
// ********************************************************************


//      NOTE:  Lengths of ASCIIZ strings are given as the maximum
//      strlen() value.  This does not include space for the
//      terminating 0-byte.  When allocating space for such an item,
//      use the form:
//
//              char username[LM20_UNLEN+1];
//
//      An exception to this is the PATHLEN manifest, which does
//      include space for the terminating 0-byte.
//
//      User names, computer names and share names should be
//      upper-cased by the caller and drawn from the ANSI
//      character set.


// ****************************************************************
// *                                                              *
// *                 Access Class                                 *
// *                                                              *
// ****************************************************************


// ****************************************************************
// *                                                              *
// *                  Function prototypes - ACCESS                *
// *
// * 	Requires User level security to be enabled
// *                                                              *
// *	Peer Server Support:
// *      Remote support of these APIs on NWSERVER is limited as
// *      described below:
// *
// *		NetAccessAdd -
// *				local and remote VSERVER - level 2
// *              remote NWSERVER -          level 2
// *	    NetAccessCheck - local only
// *      NetAccessDel -
// *              local, remote NWSERVER and remote VSERVER
// *      NetAccessEnum -
// *              sLevel 0 on remote NWSERVER (fRecursive = 1),
// *              slevel 0, 1, 2 on local and remote VSERVER
// *		NetAccessGetInfo -
// *               all sLevels on local and remote VSERVER,
// *      		 sLevel 0, 12 on remote NWSERVER
// *      NetAccessSetInfo -
// *              sLevel 1, 12 on local and remote VSERVER,
// *              sLevel 12 on remote NWSERVER
// *              parmnum = PARMNUM_ALL only
// *      NetAccessGetUserPerms - local and remote VSERVER only
// ****************************************************************

{$IFDEF LANMAN_DYNAMIC_LINK}
type
  TNetAccessAdd = function (const pszServer: LPSTR; sLevel: SmallInt;
    pbBuffer: Pointer; cbBuffer: Word): NET_API_STATUS; stdcall;

  TNetAccessCheck = function (pszReserved: LPSTR; pszUserName: LPSTR;
    pszResource: LPSTR; usOperation: Word; var pusResult: Word): NET_API_STATUS; stdcall;

  TNetAccessDel = function (const pszServer: LPSTR; pszResource: LPSTR): NET_API_STATUS; stdcall;

  TNetAccessEnum = function (const pszServer: LPSTR; pszBasePath: LPSTR;
    fsRecursive: SmallInt; sLevel: SmallInt; pbBuffer: Pointer; cbBuffer: Word;
    var pcEntriesRead: Word; var pcTotalAvail: Word): NET_API_STATUS; stdcall;

  TNetAccessGetInfo = function (const pszServer: LPSTR; pszResource: LPSTR;
    sLevel: SmallInt; pbBuffer: Pointer; cbBuffer: Word;
    var pcTotalAvail: Word): NET_API_STATUS; stdcall;

  TNetAccessSetInfo = function (const pszServer: LPSTR; pszResource: LPSTR;
    sLevel: SmallInt; pbBuffer: Pointer; cbBuffer: Word;
    sParmNum: SmallInt): NET_API_STATUS; stdcall;

  TNetAccessGetUserPerms = function (const pszServer: LPSTR; pszUgName: LPSTR;
    pszResource: LPSTR; var pusPerms: SmallInt): NET_API_STATUS; stdcall;
{$ELSE}
function NetAccessAdd(const pszServer: LPSTR; sLevel: SmallInt;
  pbBuffer: Pointer; cbBuffer: Word): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetAccessAdd}

function NetAccessCheck(pszReserved: LPSTR; pszUserName: LPSTR;
  pszResource: LPSTR; usOperation: Word; var pusResult: Word): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetAccessCheck}

function NetAccessDel(const pszServer: LPSTR; pszResource: LPSTR): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetAccessDel}

function NetAccessEnum(const pszServer: LPSTR; pszBasePath: LPSTR;
  fsRecursive: SmallInt; sLevel: SmallInt; pbBuffer: Pointer; cbBuffer: Word;
  var pcEntriesRead: Word; var pcTotalAvail: Word): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetAccessEnum}

function NetAccessGetInfo(const pszServer: LPSTR; pszResource: LPSTR;
  sLevel: SmallInt; pbBuffer: Pointer; cbBuffer: Word;
  var pcTotalAvail: Word): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetAccessGetInfo}

function NetAccessSetInfo(const pszServer: LPSTR; pszResource: LPSTR;
  sLevel: SmallInt; pbBuffer: Pointer; cbBuffer: Word;
  sParmNum: SmallInt): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetAccessSetInfo}

function NetAccessGetUserPerms(const pszServer: LPSTR; pszUgName: LPSTR;
  pszResource: LPSTR; var pusPerms: SmallInt): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetAccessGetUserPerms}
{$ENDIF}

// ****************************************************************
// *                                                              *
// *              Data structure templates - ACCESS               *
// *                                                              *
// ****************************************************************

type

  PAccessList = ^TAccessList;
  {$EXTERNALSYM access_list}
  access_list = packed record
    acl_ugname: array[0..LM20_UNLEN] of Char;
    acl_ugname_pad_1: Char;
    acl_access: SmallInt;
  end;
  TAccessList = access_list;

  PAccessList2 = ^TAccessList2;
  {$EXTERNALSYM access_list_2}
  access_list_2 = packed record
    acl2_ugname: PChar;
    acl2_access: Word;
  end;
  TAccessList2 = access_list_2;

  PAccessList12 = ^TAccessList12;
  {$EXTERNALSYM access_list_12}
  access_list_12 = packed record
    acl12_ugname: PChar;
    acl12_access: Word;
  end;
  TAccessList12 = access_list_12;

  PAccessInfo0 = ^TAccessInfo0;
  {$EXTERNALSYM access_info_0}
  access_info_0 = packed record
    acc0_resource_name: PChar;
  end;
  TAccessInfo0 = access_info_0;

  PAccessInfo1 = ^TAccessInfo1;
  {$EXTERNALSYM access_info_1}
  access_info_1 = packed record
    acc1_resource_name: PChar;
    acc1_attr: SmallInt;      // See values below
    acc1_count: SmallInt;
  end;
  TAccessInfo1 = access_info_1;

  PAccessInfo2 = ^TAccessInfo2;
  {$EXTERNALSYM access_info_2}
  access_info_2 = packed record
    acc1_resource_name: PChar;
    acc1_attr: SmallInt;
    acc1_count: SmallInt;
  end;
  TAccessInfo2 = access_info_2;

  PAccessInfo10 = ^TAccessInfo10;
  {$EXTERNALSYM access_info_10}
  access_info_10 = packed record
    acc10_resource_name: PChar;
  end;
  TAccessInfo10 = access_info_10;

  PAccessInfo12 = ^TAccessInfo12;
  {$EXTERNALSYM access_info_12}
  access_info_12 = packed record
    acc12_resource_name: PChar;
    acc12_attr: SmallInt;
    acc12_count: SmallInt;
  end;
  TAccessInfo12 = access_info_12;


// ****************************************************************
// *                                                              *
// *              Special values and constants - ACCESS           *
// *                                                              *
// ****************************************************************}

// Maximum number of permission entries for each resource.

const
  {$EXTERNALSYM MAXPERMENTRIES}
  MAXPERMENTRIES = 64;


// Bit values for the access permissions.  ACCESS_ALL is a handy
// way to specify maximum permissions.  These are used in
// acl_access field of access_list structures.

  {$EXTERNALSYM ACCESS_READ}
  ACCESS_READ      = $01;
  {$EXTERNALSYM ACCESS_WRITE}
  ACCESS_WRITE     = $02;
  {$EXTERNALSYM ACCESS_CREATE}
  ACCESS_CREATE    = $04;
  {$EXTERNALSYM ACCESS_EXEC}
  ACCESS_EXEC      = $08;
  {$EXTERNALSYM ACCESS_DELETE}
  ACCESS_DELETE    = $10;
  {$EXTERNALSYM ACCESS_ATRIB}
  ACCESS_ATRIB     = $20;
  {$EXTERNALSYM ACCESS_PERM}
  ACCESS_PERM      = $40;
  {$EXTERNALSYM ACCESS_FINDFIRST}
  ACCESS_FINDFIRST = $80;

  {$EXTERNALSYM ACCESS_GROUP}
  ACCESS_GROUP     = $8000;

  {$EXTERNALSYM ACCESS_NONE}
  ACCESS_NONE = 0;
  {$EXTERNALSYM ACCESS_ALL}
  ACCESS_ALL = ACCESS_READ or ACCESS_WRITE or ACCESS_CREATE or ACCESS_EXEC or
               ACCESS_DELETE or ACCESS_ATRIB or ACCESS_PERM or ACCESS_FINDFIRST;


// Bit values for the acc1_attr field of the access_info_1 structure.
// Only one bit is currently defined.

  {$EXTERNALSYM ACCESS_AUDIT}
  ACCESS_AUDIT = $01;

// Parmnum value for NetAccessSetInfo.

  {$EXTERNALSYM ACCESS_ATTR_PARMNUM}
  ACCESS_ATTR_PARMNUM = 2;

// ACCESS_LETTERS defines a letter for each bit position in
// the acl_access field of struct access_list.  Note that some
// bits have a corresponding letter of ' ' (space).

  {$EXTERNALSYM ACCESS_LETTERS}
  ACCESS_LETTERS = 'RWCXDAP         ';



// **************************************************************
// *								*
// *	  	Share Class			                *
// *								*
// **************************************************************

// ****************************************************************
// *                                                              *
// *              Function prototypes - SHARE                     *
// *                                                              *
// *	Peer Server Support
// * 		NetShareAdd() - sLevel 50 on VSERVER and NWSERVER
// * 		NetShareDel() - VSERVER and NWSERVER
// *		NetShareEnum() - sLevel 1,50 on VSERVER; 50 on NWSERVER
// *      NetShareGetInfo() - sLevel 50 on VSERVER, NWSERVER
// * 		NetShareSetInfo() - sLevel 50, sParmNum PARMNUM_ALL
// *						 on VSERVER, NWSERVER
// ****************************************************************

// ***	NetShareAdd - add a new share to the server tables
// *
// *	NetShareAdd( servername, level, buf, buflen )
// *
// *	ENTRY:	servername - asciz string containing name of server
// *                       or NULL if local
// *		level- Must be 50 for Win95 peer servers.
// *		buf - far ptr to struct share_info
// *		buflen - unsigned int length of buffer
// *
// *	EXIT:	0 = success
// *		ERROR_INVALID_LEVEL
// *      ERROR_BAD_NETPATH
// *		ERROR_INVALID_PARAMETER
// *		NERR_UnknownDevDir
// *		NERR_ShareExists
// *		NERR_UnknownServer
// *		NERR_ServerNotStarted
// *		NERR_RedirectedPath
// *		NERR_DuplicateShare
// *		NERR_BufTooSmall
// *		ERROR_NOT_ENOUGH_MEMORY
// *

{$IFDEF LANMAN_DYNAMIC_LINK}
type
  TNetShareAdd = function (const pszServer: PChar; sLevel: SmallInt; pbBuffer: Pointer;
    cbBuffer: Word): NET_API_STATUS; stdcall;
{$ELSE}
function NetShareAdd(const pszServer: PChar; sLevel: SmallInt; pbBuffer: Pointer;
  cbBuffer: Word): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetShareAdd}
{$ENDIF}

// ***	NetShareDel (Admin only)
// *
// *	API_FUNCTION NetShareDel( servername, netname, reserved )
// *
// *	ENTRY
// *
// *	char FAR *  servername;     asciz remote srv name, NULL if local
// *	char FAR *  netname;        asciz network name of share being deleted
// *	unsigned short reserved;    MBZ
// *
// *	EXIT
// *
// *	0 = success
// *	NERR_NetNotStarted
// *  ERROR_BAD_NETPATH
// *	NERR_ServerNotStarted
// *	NERR_NetNameNotFound
// *	ERROR_INVALID_PARAMETER
// *
// *
// *	Note:  Deleting a share will also delete any existing connections
// *		to the shared resource, and close open files within the
// *		connections.

{$IFDEF LANMAN_DYNAMIC_LINK}
type
  TNetShareDel = function (const pszServer: PChar; const pszNetName: PChar;
    usReserved: Word): NET_API_STATUS; stdcall;
{$ELSE}
function NetShareDel(const pszServer: PChar; const pszNetName: PChar;
  usReserved: Word): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetShareDel}
{$ENDIF}

// * 2.1  NetShareEnum
// *
// * API_FUNCTION
// * NetShareEnum( servername, level, buf, buflen, entriesread, totalentries )
// * char FAR *          servername;     asciz remote server name or NULL if local
// * short               sLevel;         level of detail requested; 1 or 50
// * char FAR *          pbBuffer;       buffer to return entries in
// * unsigned short      cbBuffer;       size of buffer on call
// * unsigned short FAR *pcEntriesRead;  # of entries supplied on return
// * unsigned short FAR *pcTotalAvail ;  total # of entries available
// *
// * Supply information about existing shares at specified level.
// *
// * Buffer contents on response (format for a single entry):
// *     Level 1 contains a "struct share_info_1".
// *     Level 50 contains a "struct share_info_50".
// *
// * Returns 0 if successful.  Possible error returns:
// *  ERROR_INVALID_LEVEL
// *  ERROR_BAD_NETPATH
// *  NERR_NetNotStarted
// *  NERR_ServerNotStarted
// *  ERROR_MORE_DATA

{$IFDEF LANMAN_DYNAMIC_LINK}
type
  TNetShareEnum = function (const pszServer: PChar; sLevel: SmallInt;
    pbBuffer: Pointer; cbBuffer: Word; var pcEntriesRead: Word;
    var pcTotalAvail: Word): NET_API_STATUS; stdcall;
{$ELSE}
function NetShareEnum(const pszServer: PChar; sLevel: SmallInt;
  pbBuffer: Pointer; cbBuffer: Word; var pcEntriesRead: Word;
  var pcTotalAvail: Word): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetShareEnum}
{$ENDIF}

// * 2.2  NetShareGetInfo
// *
// * Purpose: Read complete information about a single outstanding share.
// *
// * API_FUNCTION
// * NetShareGetInfo( servername, netname, level, buf, buflen, totalavail )
// * char FAR *          servername;     asciz remote server name or NULL if local
// * char FAR *          netname;        asciz network name of share being queried
// * short               level;          level of info requested (50 for Win95 peer servers)
// * char FAR *          buf;            for returned entry
// * unsigned short      buflen;         size of buffer
// * unsigned short FAR *totalavail;     total size needed for buffer
// *
// * Buffer contents on response:
// *     Level 50 contains a "struct share_info_50".
// *
// * Returns 0 if successful.  Possible error returns:
// *  ERROR_INVALID_LEVEL
// *  ERROR_INVALID_PARAMETER
// *  ERROR_BAD_NETPATH
// *  NERR_NetNotStarted
// *  NERR_ServerNotStarted
// *  NERR_NetNameNotFound
// *  NERR_MoreData
// *  NERR_BufTooSmall

{$IFDEF LANMAN_DYNAMIC_LINK}
type
  TNetShareGetInfo = function (const pszServer: PChar; const pszNetName: PChar;
    sLevel: SmallInt; pbBuffer: Pointer; cbBuffer: Word;
    var pcTotalAvail: Word): NET_API_STATUS; stdcall;
{$ELSE}
function NetShareGetInfo(const pszServer: PChar; const pszNetName: PChar;
  sLevel: SmallInt; pbBuffer: Pointer; cbBuffer: Word;
  var pcTotalAvail: Word): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetShareGetInfo}
{$ENDIF}

// ***	NetShareSetInfo (Admin only)
// *
// *	API_FUNCTION NetShareSetInfo( servername,
// *					netname,
// *					level,
// *					buf,
// *					buflen,
// *					parmnum )
// *
// *	ENTRY
// *
// *	servername;     asciz remote srv name, NULL if local
// *	netname;        asciz network name of share being set
// *	level;		level of info provided (50 for Win95 peer servers)
// *	buf;            contents described below
// *	buflen;         size of buffer
// *	parmnum;        must be PARMNUM_ALL for Win95 peer servers
// *
// *	Buffer contents on call if parmnum is zero:
// *   	    Level 50 contains a "struct share_info_50".
// *
// *	Settable fields are:
// *          shi_remark
// *          shi_passwd
// *
// *	EXIT
// *
// *	0 = success
// *	NERR_NetNotStarted
// *	NERR_ServerNotStarted
// *	NERR_NetNameNotFound
// *	ERROR_INVALID_LEVEL
// * 	NERR_BufTooSmall
// *	NERR_RemoteErr
// *	ERROR_MORE_DATA
// *	ERROR_INVALID_PARAMETER

{$IFDEF LANMAN_DYNAMIC_LINK}
type
  TNetShareSetInfo = function (const pszServer: PChar; const pszNetName: PChar;
    sLevel: SmallInt; pbBuffer: Pointer; cbBuffer: Word;
    sParmNum: Integer): NET_API_STATUS; stdcall;
{$ELSE}
function NetShareSetInfo(const pszServer: PChar; const pszNetName: PChar;
  sLevel: SmallInt; pbBuffer: Pointer; cbBuffer: Word;
  sParmNum: Integer): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetShareSetInfo}
{$ENDIF}

// **************************************************************
// *								*
// *	  	Data structure templates - SHARE		*
// *                                                            *
// **************************************************************

type
  PShareInfo0 = ^TShareInfo0;
  {$EXTERNALSYM share_info_0}
  share_info_0 = packed record
    shi0_netname: array[0..LM20_NNLEN] of Char;
  end;
  TShareInfo0 = share_info_0;

  PShareInfo1 = ^TShareInfo1;
  {$EXTERNALSYM share_info_1}
  share_info_1 = packed record
    shi1_netname: array[0..LM20_NNLEN] of Char;
    shi1_pad1: Char;
    shi1_type: Word;
    shi1_remark: PChar;
  end;
  TShareInfo1 = share_info_1;

  PShareInfo2 = ^TShareInfo2;
  {$EXTERNALSYM share_info_2}
  share_info_2 = packed record
    shi2_netname: array[0..LM20_NNLEN] of Char;
    shi2_pad1: Char;
    shi2_type: Word;
    shi2_remark: PChar;
    shi2_permissions: Word;
    shi2_max_uses: Word;
    shi2_current_uses: Word;
    shi2_path: PChar;
    shi2_passwd: array[0..SHPWLEN] of Char;
    shi2_pad2: Char;
  end;
  TShareInfo2 = share_info_2;

  PShareInfo50 = ^TShareInfo50;
  {$EXTERNALSYM share_info_50}
  share_info_50 = packed record
    shi50_netname: array[0..LM20_NNLEN] of Char;     // share name
    shi50_type: Byte;                                // see below
    shi50_flags: Word;                               // see below
    shi50_remark: PChar;                             // ANSI comment string
    shi50_path: PChar;                               // shared resource
    shi50_rw_password: array[0..SHPWLEN] of Char;    // read-write password (share-level security)
    shi50_ro_password: array[0..SHPWLEN] of Char;    // read-only password (share-level security)
  end;
  TShareInfo50 = share_info_50;

// **************************************************************
// *								*
// *	  	Special values and constants - SHARE		*
// *								*
// **************************************************************

// Field values for shi50_flags

// These flags are relevant for share-level security on VSERVER
// When operating with user-level security, use SHI50F_FULL - the actual
// access rights are determined by the NetAccess APIs.

const
  {$EXTERNALSYM SHI50F_RDONLY}
  SHI50F_RDONLY         = $0001;
  {$EXTERNALSYM SHI50F_FULL}
  SHI50F_FULL           = $0002;
  {$EXTERNALSYM SHI50F_DEPENDSON}
  SHI50F_DEPENDSON      = SHI50F_RDONLY or SHI50F_FULL;
  {$EXTERNALSYM SHI50F_ACCESSMASK}
  SHI50F_ACCESSMASK     = SHI50F_RDONLY or SHI50F_FULL;

// The share is restored on system startup

  {$EXTERNALSYM SHI50F_PERSIST}
  SHI50F_PERSIST        = $0100;

// The share is not normally visible

  {$EXTERNALSYM SHI50F_SYSTEM}
  SHI50F_SYSTEM	        = $0200;

// Values for parmnum parameter to NetShareSetInfo.

  {$EXTERNALSYM PARMNUM_ALL}
  PARMNUM_ALL = 0;

  {$EXTERNALSYM SHI_REMARK_PARMNUM}
  SHI_REMARK_PARMNUM = 4;
  {$EXTERNALSYM SHI_PERMISSIONS_PARMNUM}
  SHI_PERMISSIONS_PARMNUM = 5;
  {$EXTERNALSYM SHI_MAX_USES_PARMNUM}
  SHI_MAX_USES_PARMNUM = 6;
  {$EXTERNALSYM SHI_PASSWD_PARMNUM}
  SHI_PASSWD_PARMNUM = 9;

  {$EXTERNALSYM SHI1_NUM_ELEMENTS}
  SHI1_NUM_ELEMENTS = 4;
  {$EXTERNALSYM SHI2_NUM_ELEMENTS}
  SHI2_NUM_ELEMENTS = 10;


// Share types
// STYPE_DISKTREE and STYPE_PRINTQ are recognized on peer servers

  {$EXTERNALSYM STYPE_DISKTREE}
  STYPE_DISKTREE = 0;       // disk share
  {$EXTERNALSYM STYPE_PRINTQ}
  STYPE_PRINTQ = 1;         // printer share
  {$EXTERNALSYM STYPE_DEVICE}
  STYPE_DEVICE = 2;
  {$EXTERNALSYM STYPE_IPC}
  STYPE_IPC = 3;

  {$EXTERNALSYM SHI_USES_UNLIMITED}
  SHI_USES_UNLIMITED = DWORD(-1);


// **************************************************************
// *								*
// *	  	Session Class			                *
// *								*
// **************************************************************

// ****************************************************************
// *                                                              *
// *              Function prototypes - SESSION                   *
// *
// *	Peer Server Support                                       *
// *  	NetSessionDel() - NWSERVER and VSERVER
// *	    NetSessionEnum() - sLevel 50 on NWSERVER and VSERVER  *
// *		NetSessionGetInfo() - not supported on peer servers
// ****************************************************************}

// ***	NetSessionDel (Admin only)
// *
// *
// *	API_FUNCTION NetSessionDel( servername, clientname, reserved )
// *
// *	ENTRY
// *
// * 	servername;     asciz remote srv name, NULL if local
// *	clientname;     asciz remote computer name (returned by NetSessionEnum)
// *                               	of session being deleted
// *                  In the case of a Win95 NWSERVER, the clientname should be the
// *                  ascii connection number
// *	reserved;       session key returned by NetSessionEnum
// *
// * 	EXIT
// *
// *	0 = success
// *	NERR_NetNotStarted
// *  ERROR_BAD_NETPATH
// *	NERR_ServerNotStarted
// *	ERROR_INVALID_LEVEL
// *	NERR_RemoteErr
// *	NERR_RemoteOnly
// * 	ERROR_ACCESS_DENIED
// *	NERR_BufTooSmall
// *	NERR_ClientNameNotFound
// *

{$IFDEF LANMAN_DYNAMIC_LINK}
type
  TNetSessionDel = function (const pszServer: PChar; const pszClientName: PChar;
    sReserved: SmallInt): NET_API_STATUS; stdcall;
{$ELSE}
function NetSessionDel(const pszServer: PChar; const pszClientName: PChar;
  sReserved: SmallInt): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetSessionDel}
{$ENDIF}

// ***	NetSessionEnum
// *
// *	API_FUNCTION NetSessionEnum( servername,
// *				       level,
// *				       buf,
// *				       buflen,
// *				       entriesread,
// *				       totalentries )
// *	ENTRY
// *
// *	servername;     asciz remote srv name, NULL if local
// * 	level;          level of detail requested; (50 for Win95 peer servers)
// *	buf;            for returned entries
// *	buflen;         size of buffer on call;
// *	entriesread;    # of entries supplied on return
// *	totalentries;   total # of entries available
// *
// * 	EXIT
// *
// *	0 = success
// *	NERR_NetNotStarted
// *	NERR_ServerNotStarted
// *  ERROR_BAD_NETPATH
// *	ERROR_INVALID_LEVEL
// *	NERR_RemoteErr
// *	ERROR_MORE_DATA
// * 	ERROR_ACCESS_DENIED
// *
// *	Buffer contains an array of session_info structures.

{$IFDEF LANMAN_DYNAMIC_LINK}
type
  TNetSessionEnum = function (const pszServer: PChar; sLevel: SmallInt;
    pbBuffer: Pointer; cbBuffer: Word; var pcEntriesRead: Word;
    var pcTotalAvail: Word): NET_API_STATUS; stdcall;

  TNetSessionGetInfo = function (const pszServer: PChar; const pszClientName: PChar;
    sLevel: SmallInt; pbBuffer: Pointer; cbBuffer: Word;
    var pcTotalAvail: Word): NET_API_STATUS; stdcall;
{$ELSE}
function NetSessionEnum(const pszServer: PChar; sLevel: SmallInt;
  pbBuffer: Pointer; cbBuffer: Word; var pcEntriesRead: Word;
  var pcTotalAvail: Word): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetSessionEnum}

function NetSessionGetInfo(const pszServer: PChar; const pszClientName: PChar;
  sLevel: SmallInt; pbBuffer: Pointer; cbBuffer: Word;
  var pcTotalAvail: Word): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetSessionGetInfo}
{$ENDIF}

// **************************************************************
// *								*
// *		Data structure templates - SESSION		*
// *								*
// **************************************************************

type
  PSessionInfo0 = ^TSessionInfo0;
  {$EXTERNALSYM session_info_0}
  session_info_0 = packed record
    sesi0_cname: PChar;
  end;
  TSessionInfo0 = session_info_0;

  PSessionInfo1 = ^TSessionInfo1;
  {$EXTERNALSYM session_info_1}
  session_info_1 = packed record
    sesi1_cname: PChar;
    sesi1_username: PChar;
    sesi1_num_conns: Word;
    sesi1_num_opens: Word;
    sesi1_num_users: Word;
    sesi1_time: DWORD;
    sesi1_idle_time: DWORD;
    sesi1_user_flags: DWORD;
  end;
  TSessionInfo1 = session_info_1;

  PSession_info2 = ^TSessionInfo2;
  {$EXTERNALSYM session_info_2}
  session_info_2 = packed record
    sesi2_cname: PChar;
    sesi2_username: PChar;
    sesi2_num_conns: Word;
    sesi2_num_opens: Word;
    sesi2_num_users: Word;
    sesi2_time: DWORD;
    sesi2_idle_time: DWORD;
    sesi2_user_flags: DWORD;
    sesi2_cltype_name: PChar;
  end;
  TSessionInfo2 = session_info_2;

  PSessionInfo10 = ^TSessionInfo10;
  {$EXTERNALSYM session_info_10}
  session_info_10 = packed record
    sesi10_cname: PChar;
    sesi10_username: PChar;
    sesi10_time: PChar;
    sesi10_idle_time: PChar;
   end;
  TSessionInfo10 = session_info_10;

  PSessionInfo50 = ^TSessionInfo50;
  {$EXTERNALSYM session_info_50}
  session_info_50 = packed record
    sesi50_cname: PChar;                   //remote computer name (connection id in Netware)
    sesi50_username: PChar;
    sesi50_key: DWORD;                     // used to delete session (not used in Netware)
    sesi50_num_conns: Word;
    sesi50_num_opens: Word;                //not available in Netware
    sesi50_time: DWORD;
    sesi50_idle_time: DWORD;               //not available in Netware
    sesi50_protocol: Char;
    padl: Char;
  end;
  TSessionInfo50 = session_info_50;

// **************************************************************
// *								*
// *	  	Special values and constants - SESSION		*
// *								*
// **************************************************************

// Bits defined in sesi1_user_flags.

const
  {$EXTERNALSYM SESS_GUEST}
  SESS_GUEST = 1;               // session is logged on as a guest
  {$EXTERNALSYM SESS_NOENCRYPTION}
  SESS_NOENCRYPTION = 2;        // session is not using encryption 

  {$EXTERNALSYM SESI1_NUM_ELEMENTS}
  SESI1_NUM_ELEMENTS = 8;
  {$EXTERNALSYM SESI2_NUM_ELEMENTS}
  SESI2_NUM_ELEMENTS = 9;


// **************************************************************
// *								*
// *	  	Connection Class			        *
// *								*
// **************************************************************

// ****************************************************************
// *                                                              *
// *              Function prototypes - CONNECTION                *
// *                                                              *
// *  Peer Server Support
// * 		NetConnectionEnum -
// *               sLevel 50 on VSERVER and NWSERVER              *
// *               On NWSERVER, this API doesnt provide more      *
// *               information than NetSessionEnum
// ****************************************************************

// ***	NetConnectionEnum (Admin only)
// *
// *	API_FUNCTION NetConnectionEnum( servername,
// *					  qualifier,
// *					  level,
// *					  buf,
// *					  buflen,
// *					  totalavail )
// *
// *	ENTRY
// *
// *	servername;     asciz remote srv name, NULL if local
// *	qualifier;      netname or client computer name.
// *                  computer name should be prefaced by '\\'.
// *	level;	    	level of info requested
// *	buf;            for returned entry
// *	buflen;         size of buffer
// *	totalavail;     total size needed for buffer
// *
// *	EXIT
// *
// *	0 = success
// *	NERR_NetNotStarted
// *	NERR_ServerNotStarted
// *	ERROR_INVALID_LEVEL
// *	NERR_RemoteErr
// *	NERR_RemoteOnly		(DOS)
// *	ERROR_MORE_DATA
// * 	ERROR_ACCESS_DENIED
// *	NERR_ClientNameNotFound
// *	NERR_NetNameNotFound
// *
// *	Buffer contents on response (format for a single entry):
// *   	    Level 50 contains a "struct connection_info_50".

{$IFDEF LANMAN_DYNAMIC_LINK}
type
  TNetConnectionEnum = function (const pszServer: PChar; const pszQualifier: PChar;
    sLevel: SmallInt; pbBuffer: Pointer; cbBuffer: Word; var pcEntriesRead: Word;
    var pcTotalAvail: Word): NET_API_STATUS; stdcall;
{$ELSE}
function NetConnectionEnum(const pszServer: PChar; const pszQualifier: PChar;
  sLevel: SmallInt; pbBuffer: Pointer; cbBuffer: Word; var pcEntriesRead: Word;
  var pcTotalAvail: Word): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetConnectionEnum}
{$ENDIF}

// **************************************************************
// *								*
// *	  	Data structure templates - CONNECTION		*
// *								*
// **************************************************************

type
  PConnectionInfo0 = ^TConnectionInfo0;
  {$EXTERNALSYM connection_info_0}
  connection_info_0 = packed record
    coni0_id: Word;
  end;
  TConnectionInfo0 = connection_info_0;

  PConnectionInfo1 = ^TConnectionInfo1;
  {$EXTERNALSYM connection_info_1}
  connection_info_1 = packed record
    coni1_id: Word;
    coni1_type: Word;
    coni1_num_opens: Word;
    coni1_num_users: Word;
    coni1_time: DWORD;
    coni1_username: PChar;
    coni1_netname: PChar;
  end;
  TConnectionInfo1 = connection_info_1;

  PConnectionInfo50 = ^TConnectionInfo50;
  {$EXTERNALSYM connection_info_50}
  connection_info_50 = packed record
    coni50_type: Word;                    // share type
    coni50_num_opens: Word;               //not used in Netware
    coni50_time: DWORD;
    coni50_netname: PChar;                // share name
    coni50_username: PChar;               // user connected to share
  end;
  TConnectionInfo50 = connection_info_50;

// **************************************************************
// *								*
// *	  	File Class			                *
// *								*
// **************************************************************


// ****************************************************************
// *                                                              *
// *              Function prototypes - FILE                      *
// *                                                              *
// *  Peer Server Support
// *	NetFileEnum - sLevel 50 on VSERVER and NWSERVER        *
// *      NetFileClose2 - VSERVER only
// ****************************************************************

// ***	NetFileClose2
// *
// *	int FAR PASCAL	NetFileClose2( servername, fileid )
// *
// *	ENTRY
// *
// *	servername;     asciz remote srv name, NULL if local
// *	fileid;     	file id supplied by NetFileEnum
// *
// *	EXIT
// *
// *	0 = success
// *	NERR_NetNotStarted
// *	NERR_ServerNotStarted
// *	NERR_RemoteErr
// * 	ERROR_ACCESS_DENIED
// *	NERR_FileIdNotFound

{$IFDEF LANMAN_DYNAMIC_LINK}
type
  TNetFileClose2 = function (const pszServer: PChar; ulFileId: DWORD): NET_API_STATUS; stdcall;
{$ELSE}
function NetFileClose2(const pszServer: PChar; ulFileId: DWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetFileClose2}
{$ENDIF}

// ***	NetFileEnum (Admin Only)
// *
// *	int FAR PASCAL NetFileEnum( servername,
// *				    level,
// *				    buf,
// *				    buflen,
// *				    entriesread,
// *				    totalentries )
// *
// *	ENTRY
// *
// *	servername;     asciz remote srv name, NULL if local
// *	basepath;	path qualifier for file matching
// *              (not used for Win95 NWSERVER)
// *	level;          level of detail requested; (50 for Win95 peer servers)
// *	buf;            for returned entries
// *	buflen;         size of buffer on call;
// *	entriesread;    # of entries supplied on return
// *	totalentries;   total # of entries available
// *
// * 	EXIT
// *
// *	0 = success
// *	NERR_RemoteOnly
// *	NERR_NetNotStarted
// *	NERR_ServerNotStarted
// *	ERROR_INVALID_LEVEL
// *	NERR_RemoteErr
// *	ERROR_MORE_DATA
// * 	ERROR_ACCESS_DENIED
// *
// *
// *	Buffer contents on response (format for a single entry):
// *   	    Level 0 contains a "struct file_info_0".
// *   	    Level 50 contains a "struct file_info_50".

{$IFDEF LANMAN_DYNAMIC_LINK}
type
  TNetFileEnum = function (const pszServer: PChar; const pszBasePath: PChar;
    sLevel: SmallInt; pbBuffer: Pointer; cbBuffer: Word; var pcEntriesRead: Word;
    var pcTotalAvail: Word): NET_API_STATUS; stdcall;
{$ELSE}
function NetFileEnum(const pszServer: PChar; const pszBasePath: PChar;
  sLevel: SmallInt; pbBuffer: Pointer; cbBuffer: Word; var pcEntriesRead: Word;
  var pcTotalAvail: Word): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetFileEnum}
{$ENDIF}

// **************************************************************
// *								*
// *	  	Data structure templates - FILE			*
// *								*
// **************************************************************

type
  PFileInfo0 = ^TFileInfo0;
  {$EXTERNALSYM file_info_0}
  file_info_0 = packed record
    fi0_id: Word;
  end;
  TFileInfo0 = file_info_0;

  PFileInfo1 = ^TFileInfo1;
  {$EXTERNALSYM file_info_1}
  file_info_1 = packed record
    fi1_id: Word;
    fi1_permissions: Word;
    fi1_num_locks: Word;
    fi50_pathname: PChar;
    fi50_username: PChar;
  end;
  TFileInfo1 = file_info_1;

  PFileInfo2 = ^TFileInfo2;
  {$EXTERNALSYM file_info_2}
  file_info_2 = packed record
    fi2_id: DWORD;
  end;
  TFileInfo2 = file_info_2;

  PFileInfo3 = ^TFileInfo3;
  {$EXTERNALSYM file_info_3}
  file_info_3 = packed record
    fi3_id: DWORD;
    fi3_permissions: Word;
    fi3_num_locks: Word;
    fi3_pathname: PChar;
    fi3_username: PChar;
  end;
  TFileInfo3 = file_info_3;

  PFileInfo50 = ^TFileInfo50;
  {$EXTERNALSYM file_info_50}
  file_info_50 = packed record
    fi50_id: DWORD;                      // not used on NWSERVER
    fi50_permissions: Word;              // not available on NWSERVER
    fi50_num_locks: Word;                // not available on NWSERVER
    fi50_pathname: PChar;
    fi50_username: PChar;
    fi50_sharename: PChar;
  end;
  TFileInfo50 = file_info_50;

  PResFileEnum2 = ^TResFileEnum2;
  {$EXTERNALSYM res_file_enum_2}
  res_file_enum_2 = packed record
    res_pad: Word;                       // not used now
    res_fs: Word;                        // server type
    res_pro: DWORD;                      // progressive
  end;
  TResFileEnum2 = res_file_enum_2;

// **************************************************************
// *								*
// *		Special values and constants - FILE		*
// *								*
// **************************************************************

const					 // bit values for permissions
  {$EXTERNALSYM PERM_FILE_READ}
  PERM_FILE_READ   = $01;                // user has read access
  {$EXTERNALSYM PERM_FILE_WRITE}
  PERM_FILE_WRITE  = $02;                // user has write access
  {$EXTERNALSYM PERM_FILE_CREATE}
  PERM_FILE_CREATE = $04;                // user has create access


type
  PFRK = ^TFRK;
  {$EXTERNALSYM TFRK}
  TFRK = TResFileEnum2;

procedure FRK_INIT(var F: TFRK);

// **************************************************************
// *								*
// *	  	Server Class			                *
// *								*
// **************************************************************


// ****************************************************************
// *                                                              *
// *              Function prototypes - SERVER                    *
// *                                                              *
// * Peer Server Support
// * 	NetServerGetInfo - sLevel 1,50 on NWSERVER, VSERVER
// ****************************************************************

// * 6.2  NetServerGetInfo
// *
// * Purpose: Read the current configuration parameters of the server.
// *
// * int FAR PASCAL
// * NetServerGetInfo( servername, level, buf, buflen, totalavail )
// * char FAR *          servername;   asciz remote server name or NULL if local
// * short               level;          level of information to be returned
// * char FAR *          buf;            for returned data
// * unsigned short      buflen;         size of buffer
// * unsigned short FAR *totalavail;     total size needed for buffer
// *
// * Buffer contents on response (format for a single entry):
// *     Level 1 contains a "struct server_info_1".
// *     Level 50 contains a "struct server_info_50".
// *
// * If the buflen is not large enough for all of the information, the call
// * will return as much as will fit in the buffer.
// *
// * Returns 0 if successful. Error return information:
// *
// *     - ERROR_INVALID_LEVEL       - Level parameter specified is invalid
// *     - ERROR_INVALID_PARAMETER   - An invalid input parameter was detected.
// *     - NERR_NetNotStarted        - Network not installed on local machine
// *     - NERR_ServerNotStarted     - Server is not started
// *     - NERR_BufTooSmall          - The buffer supplied was to small to
// *                                   return the fixed length structure
// *				     requested.
// *     - NERR_MoreData             - The buffer supplied was too small to
// *				     return all the information available
// *				     for this server.

{$IFDEF LANMAN_DYNAMIC_LINK}
type
  TNetServerGetInfo = function (const pszServer: PChar; sLevel: SmallInt;
    pbBuffer: Pointer; cbBuffer: Word; var pcTotalAvail: Word): NET_API_STATUS; stdcall;
{$ELSE}
function NetServerGetInfo(const pszServer: PChar; sLevel: SmallInt;
  pbBuffer: Pointer; cbBuffer: Word; var pcTotalAvail: Word): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetServerGetInfo}
{$ENDIF}

// **************************************************************
// *								*
// *	  	Data structure templates - SERVER		*
// *								*
// **************************************************************

type
  PServerInfo0 = ^TServerInfo0;
  {$EXTERNALSYM server_info_0}
  server_info_0 = packed record
    sv0_name: array[0..CNLEN] of Char;
  end;
  TServerInfo0 = server_info_0;

  PServerInfo1 = ^TServerInfo1;
  {$EXTERNALSYM server_info_1}
  server_info_1 = packed record
    sv1_name: array[0..CNLEN] of Char;
    sv1_version_major: Byte;                   // Major version # of net
    sv1_version_minor: Byte;                   // Minor version # of net
    sv1_type: DWORD;                           // Server type
    sv1_comment: PChar;                        // Exported server comment
  end;
  TServerInfo1 = server_info_1;


// NOTE struct prefix must equal server_info_1 like below!

  PServerInfo50 = ^TServerInfo50;
  {$EXTERNALSYM server_info_50}
  server_info_50 = packed record
    sv50_name: array[0..CNLEN] of Char;
    sv50_version_major: Byte;                  // Major version # of net
    sv50_version_minor: Byte;                  // Minor version # of net
    sv50_type: DWORD;                          // Server type
    sv50_comment: PChar;                       // Exported server comment
    sv50_security: Word;                       // SV_SECURITY_* (see below)
    sv50_auditing: Word;                       // 0 = no auditing; nonzero = auditing
    sv50_container: PChar;                     // Security server/domain
    sv50_ab_server: PChar;                     // Address book server
    sv50_ab_dll: PChar;                        // Address book provider DLL 
  end;
  TServerInfo50 = server_info_50;

  PServerInfo2 = ^TServerInfo2;
  {$EXTERNALSYM server_info_2}
  server_info_2 = packed record
    sv2_name: array[0..CNLEN] of Char;
    sv2_version_major: Byte;
    sv2_version_minor: Byte;
    sv2_type: DWORD;
    sv2_comment: PChar;
    sv2_ulist_mtime: DWORD;          // User list, last modification time    
    sv2_glist_mtime: DWORD;          // Group list, last modification time   
    sv2_alist_mtime: DWORD;          // Access list, last modification time  
    sv2_users: Word;                 // max number of users allowed          
    sv2_disc: Word;                  // auto-disconnect timeout(in minutes)  
    sv2_alerts: PChar;               // alert names (semicolon separated)    
    sv2_security: Word;              // SV_USERSECURITY or SV_SHARESECURITY  
    sv2_auditing: Word;              // 0 = no auditing; nonzero = auditing  
    sv2_numadmin: Word;              // max number of administrators allowed 
    sv2_lanmask: Word;               // bit mask representing the srv'd nets 
    sv2_hidden: Word;                // 0 = visible; nonzero = hidden        
    sv2_announce: Word;              // visible server announce rate (sec)   
    sv2_anndelta: Word;              // announce randomize interval (sec)    
                                     // name of guest account                
    sv2_guestacct: array[0..LM20_UNLEN] of Char;
    sv2_pad1: Byte;                  // Word alignment pad byte              
    sv2_userpath: PChar;             // ASCIIZ path to user directories      
    sv2_chdevs: Word;                // max # shared character devices       
    sv2_chdevq: Word;                // max # character device queues        
    sv2_chdevjobs: Word;             // max # character device jobs          
    sv2_connections: Word;           // max # of connections                 
    sv2_shares: Word;	             // max # of shares                      
    sv2_openfiles: Word;             // max # of open files                  
    sv2_sessopens: Word;             // max # of open files per session      
    sv2_sessvcs: Word;               // max # of virtual circuits per client 
    sv2_sessreqs: Word;              // max # of simul. reqs. from a client  
    sv2_opensearch: Word;            // max # of open searches               
    sv2_activelocks: Word;           // max # of active file locks           
    sv2_numreqbuf: Word;             // number of server (standard) buffers  
    sv2_sizreqbuf: Word;             // size of svr (standard) bufs (bytes)  
    sv2_numbigbuf: Word;             // number of big (64K) buffers          
    sv2_numfiletasks: Word;          // number of file worker processes      
    sv2_alertsched: Word;            // alert counting interval (minutes)    
    sv2_erroralert: Word;            // error log alerting threshold         
    sv2_logonalert: Word;            // logon violation alerting threshold   
    sv2_accessalert: Word;           // access violation alerting threshold  
    sv2_diskalert: Word;             // low disk space alert threshold (KB)  
    sv2_netioalert: Word;            // net I/O error ratio alert threshold  
                                     //  (tenths of a percent)               
    sv2_maxauditsz: Word;            // Maximum audit file size (KB)         
    sv2_srvheuristics: PChar;        // performance related server switches  
  end;
  TServerInfo2 = server_info_2;

  PServerInfo3 = ^TServerInfo3;
  {$EXTERNALSYM server_info_3}
  server_info_3 = packed record
    sv3_name: array[0..CNLEN] of Char;
    sv3_version_major: Byte;
    sv3_version_minor: Byte;
    sv3_type: DWORD;
    sv3_comment: PChar;
    sv3_ulist_mtime: DWORD;          // User list, last modification time    
    sv3_glist_mtime: DWORD;          // Group list, last modification time   
    sv3_alist_mtime: DWORD;          // Access list, last modification time  
    sv3_users: Word;                 // max number of users allowed          
    sv3_disc: Word;                  // auto-disconnect timeout(in minutes)  
    sv3_alerts: PChar;               // alert names (semicolon separated)    
    sv3_security: Word;              // SV_USERSECURITY or SV_SHARESECURITY  
    sv3_auditing: Word;              // 0 = no auditing; nonzero = auditing  
    sv3_numadmin: Word;              // max number of administrators allowed 
    sv3_lanmask: Word;               // bit mask representing the srv'd nets 
    sv3_hidden: Word;                // 0 = visible; nonzero = hidden        
    sv3_announce: Word;              // visible server announce rate (sec)   
    sv3_anndelta: Word;              // announce randomize interval (sec)    
                                     // name of guest account                
    sv3_guestacct: array[0..LM20_UNLEN] of Char;
    sv3_pad1: Byte;                  // Word alignment pad byte              
    sv3_userpath: PChar;             // ASCIIZ path to user directories      
    sv3_chdevs: Word;                // max # shared character devices       
    sv3_chdevq: Word;                // max # character device queues        
    sv3_chdevjobs: Word;             // max # character device jobs          
    sv3_connections: Word;           // max # of connections                 
    sv3_shares: Word;	             // max # of shares                      
    sv3_openfiles: Word;             // max # of open files                  
    sv3_sessopens: Word;             // max # of open files per session      
    sv3_sessvcs: Word;               // max # of virtual circuits per client 
    sv3_sessreqs: Word;              // max # of simul. reqs. from a client  
    sv3_opensearch: Word;            // max # of open searches               
    sv3_activelocks: Word;           // max # of active file locks           
    sv3_numreqbuf: Word;             // number of server (standard) buffers  
    sv3_sizreqbuf: Word;             // size of svr (standard) bufs (bytes)  
    sv3_numbigbuf: Word;             // number of big (64K) buffers          
    sv3_numfiletasks: Word;          // number of file worker processes      
    sv3_alertsched: Word;            // alert counting interval (minutes)    
    sv3_erroralert: Word;            // error log alerting threshold         
    sv3_logonalert: Word;            // logon violation alerting threshold   
    sv3_accessalert: Word;           // access violation alerting threshold  
    sv3_diskalert: Word;             // low disk space alert threshold (KB)  
    sv3_netioalert: Word;            // net I/O error ratio alert threshold  
                                     //  (tenths of a percent)               
    sv3_maxauditsz: Word;            // Maximum audit file size (KB)         
    sv3_srvheuristics: PChar;        // performance related server switches  


    sv3_auditedevents: DWORD;        // Audit event control mask             
    sv3_autoprofile: Word;           // (0,1,2,3) = (NONE,LOAD,SAVE,or BOTH) 
    sv3_autopath: PChar;             // file pathname (where to load & save) 
  end;
  TServerInfo3 = server_info_3;


// **************************************************************
// *								*
// *	  	Special values and constants - SERVER		*
// *								*
// **************************************************************

// Mask to be applied to svX_version_major in order to obtain
// the major version number.

const
  {$EXTERNALSYM MAJOR_VERSION_MASK}
  MAJOR_VERSION_MASK = $0F;

// Bit-mapped values for svX_type fields. X = 1, 2 or 3.

  {$EXTERNALSYM SV_TYPE_WORKSTATION}
  SV_TYPE_WORKSTATION	 = $00000001;
  {$EXTERNALSYM SV_TYPE_SERVER}
  SV_TYPE_SERVER         = $00000002;
  {$EXTERNALSYM SV_TYPE_SQLSERVER}
  SV_TYPE_SQLSERVER      = $00000004;
  {$EXTERNALSYM SV_TYPE_DOMAIN_CTRL}
  SV_TYPE_DOMAIN_CTRL    = $00000008;
  {$EXTERNALSYM SV_TYPE_DOMAIN_BAKCTRL}
  SV_TYPE_DOMAIN_BAKCTRL = $00000010;
  {$EXTERNALSYM SV_TYPE_TIME_SOURCE}
  SV_TYPE_TIME_SOURCE	 = $00000020;
  {$EXTERNALSYM SV_TYPE_AFP}
  SV_TYPE_AFP		 = $00000040;
  {$EXTERNALSYM SV_TYPE_NOVELL}
  SV_TYPE_NOVELL         = $00000080; // This flag is also set by Win95 NWSERVER 
  {$EXTERNALSYM SV_TYPE_DOMAIN_MEMBER}
  SV_TYPE_DOMAIN_MEMBER  = $00000100;
  {$EXTERNALSYM SV_TYPE_PRINTQ_SERVER}
  SV_TYPE_PRINTQ_SERVER  = $00000200;
  {$EXTERNALSYM SV_TYPE_DIALIN_SERVER}
  SV_TYPE_DIALIN_SERVER	 = $00000400;
  {$EXTERNALSYM SV_TYPE_ALL}
  SV_TYPE_ALL            = $FFFFFFFF; // handy for NetServerEnum2 

// Special value for svX_disc that specifies infinite disconnect
// time. X = 2 or 3.

  {$EXTERNALSYM SV_NODISC}
  SV_NODISC = $FFFF;                  // No autodisconnect timeout enforced

// Values of svX_security field. X = 2 or 3.

  {$EXTERNALSYM SV_USERSECURITY}
  SV_USERSECURITY = 1;
  {$EXTERNALSYM SV_SHARESECURITY}
  SV_SHARESECURITY = 0;

// Values of svX_security field. X = 50.
// For Win95 NWSERVER, the only possible returned value is SV_SECURITY_NETWARE.

  {$EXTERNALSYM SV_SECURITY_SHARE}
  SV_SECURITY_SHARE = 0;   // Share-level
  {$EXTERNALSYM SV_SECURITY_WINNT}
  SV_SECURITY_WINNT = 1;   // User-level - Windows NT workst'n
  {$EXTERNALSYM SV_SECURITY_WINNTAS}
  SV_SECURITY_WINNTAS = 2; // User-level - Windows NT domain
  {$EXTERNALSYM SV_SECURITY_NETWARE}
  SV_SECURITY_NETWARE = 3; // User-level - NetWare 3.x bindery

// Values of svX_hidden field. X = 2 or 3.

  {$EXTERNALSYM SV_HIDDEN}
  SV_HIDDEN = 1;
  {$EXTERNALSYM SV_VISIBLE}
  SV_VISIBLE = 0;

  {$EXTERNALSYM SVI1_NUM_ELEMENTS}
  SVI1_NUM_ELEMENTS = 5;
  {$EXTERNALSYM SVI2_NUM_ELEMENTS}
  SVI2_NUM_ELEMENTS = 44;
  {$EXTERNALSYM SVI3_NUM_ELEMENTS}
  SVI3_NUM_ELEMENTS = 45;

// Masks describing AUTOPROFILE parameters

  {$EXTERNALSYM SW_AUTOPROF_LOAD_MASK}
  SW_AUTOPROF_LOAD_MASK	= $01;
  {$EXTERNALSYM SW_AUTOPROF_SAVE_MASK}
  SW_AUTOPROF_SAVE_MASK	= $02;


// ****************************************************************
// *                                                              *
// *                 Security Class                               *
// *                                                              *
// ****************************************************************

// ****************************************************************
// *                                                              *
// *                  Function prototypes - SECURITY              *
// *                                                              *
// ****************************************************************

{$IFDEF LANMAN_DYNAMIC_LINK}
type
  TNetSecurityGetInfo = function (const pszServer: PChar; sLevel: SmallInt;
    pbBuffer: Pointer; cbBuffer: Word; var pcTotalAvail: Word): NET_API_STATUS; stdcall;
{$ELSE}
function NetSecurityGetInfo(const pszServer: PChar; sLevel: SmallInt;
  pbBuffer: Pointer; cbBuffer: Word; var pcTotalAvail: Word): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetSecurityGetInfo}
{$ENDIF}

// **************************************************************
// *								*
// *	  	Data structure templates - SECURITY		*
// *								*
// **************************************************************

type
  PSecurityInfo1 = ^TSecurityInfo1;
  {$EXTERNALSYM security_info_1}
  security_info_1 = packed record
   sec1_security: DWORD;               // SEC_SECURITY_* (see below)
   sec1_container: PChar;              // Security server/domain
   sec1_ab_server: PChar;              // Address book server
   sec1_ab_dll: PChar;                 // Address book provider DLL
  end;
  TSecurityInfo1 = security_info_1;

// **************************************************************
// *								*
// *	  	Special values and constants - SECURITY		*
// *								*
// **************************************************************

// Values of secX_security field. X = 1.

const
  {$EXTERNALSYM SEC_SECURITY_SHARE}
  SEC_SECURITY_SHARE    = SV_SECURITY_SHARE;
  {$EXTERNALSYM SEC_SECURITY_WINNT}
  SEC_SECURITY_WINNT	= SV_SECURITY_WINNT;
  {$EXTERNALSYM SEC_SECURITY_WINNTAS}
  SEC_SECURITY_WINNTAS	= SV_SECURITY_WINNTAS;
  {$EXTERNALSYM SEC_SECURITY_NETWARE}
  SEC_SECURITY_NETWARE	= SV_SECURITY_NETWARE;

{$IFDEF LANMAN_DYNAMIC_LINK}
var
  {$EXTERNALSYM NetAccessAdd}
  NetAccessAdd: TNetAccessAdd = nil;
  {$EXTERNALSYM NetAccessCheck}
  NetAccessCheck: TNetAccessCheck = nil;
  {$EXTERNALSYM NetAccessDel}
  NetAccessDel: TNetAccessDel = nil;
  {$EXTERNALSYM NetAccessEnum}
  NetAccessEnum: TNetAccessEnum = nil;
  {$EXTERNALSYM NetAccessGetInfo}
  NetAccessGetInfo: TNetAccessGetInfo = nil;
  {$EXTERNALSYM NetAccessSetInfo}
  NetAccessSetInfo: TNetAccessSetInfo = nil;
  {$EXTERNALSYM NetAccessGetUserPerms}
  NetAccessGetUserPerms: TNetAccessGetUserPerms = nil;
  {$EXTERNALSYM NetShareAdd}
  NetShareAdd: TNetShareAdd = nil;
  {$EXTERNALSYM NetShareDel}
  NetShareDel: TNetShareDel = nil;
  {$EXTERNALSYM NetShareEnum}
  NetShareEnum: TNetShareEnum = nil;
  {$EXTERNALSYM NetShareGetInfo}
  NetShareGetInfo: TNetShareGetInfo = nil;
  {$EXTERNALSYM NetShareSetInfo}
  NetShareSetInfo: TNetShareSetInfo = nil;
  {$EXTERNALSYM NetSessionDel}
  NetSessionDel: TNetSessionDel = nil;
  {$EXTERNALSYM NetSessionEnum}
  NetSessionEnum: TNetSessionEnum = nil;
  {$EXTERNALSYM NetSessionGetInfo}
  NetSessionGetInfo: TNetSessionGetInfo = nil;
  {$EXTERNALSYM NetConnectionEnum}
  NetConnectionEnum: TNetConnectionEnum = nil;
  {$EXTERNALSYM NetFileClose2}
  NetFileClose2: TNetFileClose2 = nil;
  {$EXTERNALSYM NetFileEnum}
  NetFileEnum: TNetFileEnum = nil;
  {$EXTERNALSYM NetServerGetInfo}
  NetServerGetInfo: TNetServerGetInfo = nil;
  {$EXTERNALSYM NetSecurityGetInfo}
  NetSecurityGetInfo: TNetSecurityGetInfo = nil;
{$ENDIF}

{$IFDEF LANMAN_DYNAMIC_LINK}
function SvrApiLoaded: Boolean;
{$IFDEF LANMAN_DYNAMIC_LINK_EXPLICIT}
function LoadSvrApi: Boolean;
function UnloadSvrApi: Boolean;
{$ENDIF}
{$ENDIF}

implementation

const
  svrapilib = 'svrapi.dll';

procedure FRK_INIT(var F: TFRK);
begin
  ZeroMemory(@F, Sizeof(F));
end;

{$IFDEF LANMAN_DYNAMIC_LINK}
var
  LibHandle: THandle = 0;

function SvrApiLoaded: Boolean;
begin
  Result := (LibHandle <> 0);
end;

function LoadSvrApi: Boolean;
//----------------------------------------------------------------------------------------------------------------------------------
begin
	Result := SvrApiLoaded;
	if Result then begin
		Exit;
	end;
	LibHandle := LoadLibrary(svrapilib);
	Result := SvrApiLoaded;
	if Result then begin
		@NetAccessAdd := GetProcAddress(LibHandle, 'NetAccessAdd');
		@NetAccessCheck := GetProcAddress(LibHandle, 'NetAccessCheck');
		@NetAccessDel := GetProcAddress(LibHandle, 'NetAccessDel');
		@NetAccessEnum := GetProcAddress(LibHandle, 'NetAccessEnum');
		@NetAccessGetInfo := GetProcAddress(LibHandle, 'NetAccessGetInfo');
		@NetAccessSetInfo := GetProcAddress(LibHandle, 'NetAccessSetInfo');
		@NetAccessGetUserPerms := GetProcAddress(LibHandle, 'NetAccessGetUserPerms');
		@NetShareAdd := GetProcAddress(LibHandle, 'NetShareAdd');
		@NetShareDel := GetProcAddress(LibHandle, 'NetShareDel');
		@NetShareEnum := GetProcAddress(LibHandle, 'NetShareEnum');
		@NetShareGetInfo := GetProcAddress(LibHandle, 'NetShareGetInfo');
		@NetShareSetInfo := GetProcAddress(LibHandle, 'NetShareSetInfo');
		@NetSessionDel := GetProcAddress(LibHandle, 'NetSessionDel');
		@NetSessionEnum := GetProcAddress(LibHandle, 'NetSessionEnum');
		@NetSessionGetInfo := GetProcAddress(LibHandle, 'NetSessionGetInfo');
		@NetConnectionEnum := GetProcAddress(LibHandle, 'NetConnectionEnum');
		@NetFileClose2 := GetProcAddress(LibHandle, 'NetFileClose2');
		@NetFileEnum := GetProcAddress(LibHandle, 'NetFileEnum');
		@NetServerGetInfo := GetProcAddress(LibHandle, 'NetServerGetInfo');
		@NetSecurityGetInfo := GetProcAddress(LibHandle, 'NetSecurityGetInfo');
	end;
end;

function UnloadSvrApi: Boolean;
begin
  Result := True;
  if SvrApiLoaded then
  begin
    Result := FreeLibrary(LibHandle);
    @NetAccessAdd := nil;
    @NetAccessCheck := nil;
    @NetAccessDel := nil;
    @NetAccessEnum := nil;
    @NetAccessGetInfo := nil;
    @NetAccessSetInfo := nil;
    @NetAccessGetUserPerms := nil;
    @NetShareAdd := nil;
    @NetShareDel := nil;
    @NetShareEnum := nil;
    @NetShareGetInfo := nil;
    @NetShareSetInfo := nil;
    @NetSessionDel := nil;
    @NetSessionEnum := nil;
    @NetSessionGetInfo := nil;
    @NetConnectionEnum := nil;
    @NetFileClose2 := nil;
    @NetFileEnum := nil;
    @NetServerGetInfo := nil;
    @NetSecurityGetInfo := nil;
    LibHandle := 0;
  end;
end;

{$ELSE}
function NetAccessAdd; external svrapilib name 'NetAccessAdd';
function NetAccessCheck; external svrapilib name 'NetAccessCheck';
function NetAccessDel; external svrapilib name 'NetAccessDel';
function NetAccessEnum; external svrapilib name 'NetAccessEnum';
function NetAccessGetInfo; external svrapilib name 'NetAccessGetInfo';
function NetAccessSetInfo; external svrapilib name 'NetAccessSetInfo';
function NetAccessGetUserPerms; external svrapilib name 'NetAccessGetUserPerms';
function NetShareAdd; external svrapilib name 'NetShareAdd';
function NetShareDel; external svrapilib name 'NetShareDel';
function NetShareEnum; external svrapilib name 'NetShareEnum';
function NetShareGetInfo; external svrapilib name 'NetShareGetInfo';
function NetShareSetInfo; external svrapilib name 'NetShareSetInfo';
function NetSessionDel; external svrapilib name 'NetSessionDel';
function NetSessionEnum; external svrapilib name 'NetSessionEnum';
function NetSessionGetInfo; external svrapilib name 'NetSessionGetInfo';
function NetConnectionEnum; external svrapilib name 'NetConnectionEnum';
function NetFileClose2; external svrapilib name 'NetFileClose2';
function NetFileEnum; external svrapilib name 'NetFileEnum';
function NetServerGetInfo; external svrapilib name 'NetServerGetInfo';
function NetSecurityGetInfo; external svrapilib name 'NetSecurityGetInfo';
{$ENDIF}

{$IFDEF LANMAN_DYNAMIC_LINK}
initialization
{$IFNDEF LANMAN_DYNAMIC_LINK_EXPLICIT}
  LoadSvrApi;
{$ENDIF}
finalization
  UnloadSvrApi;
{$ENDIF}
end.


