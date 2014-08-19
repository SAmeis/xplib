{******************************************************************}
{                                                                  }
{ Borland Delphi Runtime Library                                   }
{ LanManager constants interface unit                              }
{                                                                  }
{ Portions created by Microsoft are                                }
{ Copyright (C) 1995-1999 Microsoft Corporation.                   }
{ All Rights Reserved.                                             }
{                                                                  }
{ The original file is: lmcons.h, released 15 Mar 1999.            }
{ The original Pascal code is: LmCons.pas, released 29 Dec 1999.   }
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

unit Lmcons;

{$I LANMAN.INC}

{$ALIGN ON}
{$MINENUMSIZE 4}
{$WEAKPACKAGEUNIT}

interface

uses
  Windows, Lmerr;

(*$HPPEMIT '<lmcons.h>'*)

// String Lengths for various LanMan names

const
  {$EXTERNALSYM CNLEN}
  CNLEN = 15;                           // Computer name length
  {$EXTERNALSYM LM20_CNLEN}
  LM20_CNLEN = 15;                      // LM 2.0 Computer name length
  {$EXTERNALSYM DNLEN}
  DNLEN = CNLEN;                        // Maximum domain name length
  {$EXTERNALSYM LM20_DNLEN}
  LM20_DNLEN = LM20_CNLEN;              // LM 2.0 Maximum domain name length

  {$EXTERNALSYM UNCLEN}
  UNCLEN = (CNLEN+2);                   // UNC computer name length
  {$EXTERNALSYM LM20_UNCLEN}
  LM20_UNCLEN = (LM20_CNLEN+2);         // LM 2.0 UNC computer name length

  {$EXTERNALSYM NNLEN}
  NNLEN = 80;                           // Net name length (share name)
  {$EXTERNALSYM LM20_NNLEN}
  LM20_NNLEN = 12;                      // LM 2.0 Net name length

  {$EXTERNALSYM RMLEN}
  RMLEN = (UNCLEN+1+NNLEN);             // Max remote name length
  {$EXTERNALSYM LM20_RMLEN}
  LM20_RMLEN  = (LM20_UNCLEN+1+LM20_NNLEN); // LM 2.0 Max remote name length

  {$EXTERNALSYM SNLEN}
  SNLEN = 80;                           // Service name length
  {$EXTERNALSYM LM20_SNLEN}
  LM20_SNLEN = 15;                      // LM 2.0 Service name length
  {$EXTERNALSYM STXTLEN}
  STXTLEN = 256;                        // Service text length
  {$EXTERNALSYM LM20_STXTLEN}
  LM20_STXTLEN = 63;                    // LM 2.0 Service text length

  {$EXTERNALSYM PATHLEN}
  PATHLEN = 256;                        // Max. path (not including drive name)
  {$EXTERNALSYM LM20_PATHLEN}
  LM20_PATHLEN = 256;                   // LM 2.0 Max. path

  {$EXTERNALSYM DEVLEN}
  DEVLEN = 80;                          // Device name length
  {$EXTERNALSYM LM20_DEVLEN}
  LM20_DEVLEN = 8;                      // LM 2.0 Device name length

  {$EXTERNALSYM EVLEN}
  EVLEN = 16;                           // Event name length

// User, Group and Password lengths

  {$EXTERNALSYM UNLEN}
  UNLEN = 256;                          // Maximum user name length
  {$EXTERNALSYM LM20_UNLEN}
  LM20_UNLEN = 20;                      // LM 2.0 Maximum user name length

  {$EXTERNALSYM GNLEN}
  GNLEN = UNLEN;                        // Group name
  {$EXTERNALSYM LM20_GNLEN}
  LM20_GNLEN = LM20_UNLEN;              // LM 2.0 Group name

  {$EXTERNALSYM PWLEN}
  PWLEN = 256;                          // Maximum password length
  {$EXTERNALSYM LM20_PWLEN}
  LM20_PWLEN = 14;                      // LM 2.0 Maximum password length

  {$EXTERNALSYM SHPWLEN}
  SHPWLEN = 8;                          // Share password length (bytes)

  {$EXTERNALSYM CLTYPE_LEN}
  CLTYPE_LEN = 12;                      // Length of client type string

  {$EXTERNALSYM MAXCOMMENTSZ}
  MAXCOMMENTSZ = 256;                   // Multipurpose comment length
  {$EXTERNALSYM LM20_MAXCOMMENTSZ}
  LM20_MAXCOMMENTSZ = 48;               // LM 2.0 Multipurpose comment length

  {$EXTERNALSYM QNLEN}
  QNLEN = NNLEN;                        // Queue name maximum length
  {$EXTERNALSYM LM20_QNLEN}
  LM20_QNLEN = LM20_NNLEN;              // LM 2.0 Queue name maximum length

// The ALERTSZ and MAXDEVENTRIES defines have not yet been NT'ized.
// Whoever ports these components should change these values appropriately.

  {$EXTERNALSYM ALERTSZ}
  ALERTSZ = 128;                        // size of alert string in server
  {$EXTERNALSYM MAXDEVENTRIES}
  MAXDEVENTRIES = (Sizeof(Integer)*8);  // Max number of device entries

                                        //
                                        // We use int bitmap to represent
                                        //

  {$EXTERNALSYM NETBIOS_NAME_LEN}
  NETBIOS_NAME_LEN = 16;                // NetBIOS net name (bytes)

// Value to be used with APIs which have a "preferred maximum length"
// parameter.  This value indicates that the API should just allocate
// "as much as it takes."

  {$EXTERNALSYM MAX_PREFERRED_LENGTH}
  MAX_PREFERRED_LENGTH = DWORD(-1);

//        Constants used with encryption

  {$EXTERNALSYM CRYPT_KEY_LEN}
  CRYPT_KEY_LEN = 7;
  {$EXTERNALSYM CRYPT_TXT_LEN}
  CRYPT_TXT_LEN = 8;
  {$EXTERNALSYM ENCRYPTED_PWLEN}
  ENCRYPTED_PWLEN = 16;
  {$EXTERNALSYM SESSION_PWLEN}
  SESSION_PWLEN = 24;
  {$EXTERNALSYM SESSION_CRYPT_KLEN}
  SESSION_CRYPT_KLEN = 21;

//  Value to be used with SetInfo calls to allow setting of all
//  settable parameters (parmnum zero option)

  {$EXTERNALSYM PARMNUM_ALL}
  PARMNUM_ALL = 0;

  {$EXTERNALSYM PARM_ERROR_UNKNOWN}
  PARM_ERROR_UNKNOWN = DWORD(-1);
  {$EXTERNALSYM PARM_ERROR_NONE}
  PARM_ERROR_NONE = 0;
  {$EXTERNALSYM PARMNUM_BASE_INFOLEVEL}
  PARMNUM_BASE_INFOLEVEL = 1000;

//        Message File Names

  {$EXTERNALSYM MESSAGE_FILENAME}
  MESSAGE_FILENAME = 'NETMSG';
  {$EXTERNALSYM OS2MSG_FILENAME}
  OS2MSG_FILENAME = 'BASE';
  {$EXTERNALSYM HELP_MSG_FILENAME}
  HELP_MSG_FILENAME = 'NETH';

//**INTERNAL_ONLY**/

// The backup message file named here is a duplicate of net.msg. It
// is not shipped with the product, but is used at buildtime to
// msgbind certain messages to netapi.dll and some of the services.
// This allows for OEMs to modify the message text in net.msg and
// have those changes show up.        Only in case there is an error in
// retrieving the messages from net.msg do we then get the bound
// messages out of bak.msg (really out of the message segment).

  {$EXTERNALSYM BACKUP_MSG_FILENAME}
  BACKUP_MSG_FILENAME = 'BAK.MSG';

//**END_INTERNAL**/

// Keywords used in Function Prototypes

type
  {$EXTERNALSYM NET_API_STATUS}
  NET_API_STATUS = DWORD;
  {$EXTERNALSYM API_RET_TYPE}
  API_RET_TYPE = NET_API_STATUS;        // Old value: do not use

// The platform ID indicates the levels to use for platform-specific
// information.

const
  {$EXTERNALSYM PLATFORM_ID_DOS}
  PLATFORM_ID_DOS = 300;
  {$EXTERNALSYM PLATFORM_ID_OS2}
  PLATFORM_ID_OS2 = 400;
  {$EXTERNALSYM PLATFORM_ID_NT}
  PLATFORM_ID_NT = 500;
  {$EXTERNALSYM PLATFORM_ID_OSF}
  PLATFORM_ID_OSF = 600;
  {$EXTERNALSYM PLATFORM_ID_VMS}
  PLATFORM_ID_VMS = 700;

//      There message numbers assigned to different LANMAN components
//      are as defined below.
//
//      lmerr.h:        2100 - 2999     NERR_BASE
//      alertmsg.h:     3000 - 3049     ALERT_BASE
//      lmsvc.h:        3050 - 3099     SERVICE_BASE
//      lmerrlog.h:     3100 - 3299     ERRLOG_BASE
//      msgtext.h:      3300 - 3499     MTXT_BASE
//      apperr.h:       3500 - 3999     APPERR_BASE
//      apperrfs.h:     4000 - 4299     APPERRFS_BASE
//      apperr2.h:      4300 - 5299     APPERR2_BASE
//      ncberr.h:       5300 - 5499     NRCERR_BASE
//      alertmsg.h:     5500 - 5599     ALERT2_BASE
//      lmsvc.h:        5600 - 5699     SERVICE2_BASE
//      lmerrlog.h      5700 - 5799     ERRLOG2_BASE

  {$EXTERNALSYM MIN_LANMAN_MESSAGE_ID}
  MIN_LANMAN_MESSAGE_ID = NERR_BASE;
  {$EXTERNALSYM MAX_LANMAN_MESSAGE_ID}
  MAX_LANMAN_MESSAGE_ID = 5799;

const
  netapi32lib = 'netapi32.dll';
  {$NODEFINE netapi32lib}

implementation

end.
