{******************************************************************}
{                                                                  }
{ Borland Delphi Runtime Library                                   }
{ LanManager interface unit                                        }
{                                                                  }
{ Portions created by Microsoft are                                }
{ Copyright (C) 1995-1999 Microsoft Corporation.                   }
{ All Rights Reserved.                                             }
{                                                                  }
{ The original file is: lmuseflg.h, released 14 Nov 1998.          }
{ The original Pascal code is: LmUseflg.pas, released 30 Dec 1999. }
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

unit LmUseflg;

{$I LANMAN.INC}

{$ALIGN ON}
{$MINENUMSIZE 4}
{$WEAKPACKAGEUNIT}

interface

uses
  Windows;

(*$HPPEMIT '#include <lmuseflg.h>'*)

// Definition for NetWkstaTransportDel and NetUseDel deletion force levels

const
  {$EXTERNALSYM USE_NOFORCE}
  USE_NOFORCE = 0;
  {$EXTERNALSYM USE_FORCE}
  USE_FORCE = 1;
  {$EXTERNALSYM USE_LOTS_OF_FORCE}
  USE_LOTS_OF_FORCE = 2;

implementation

end.
