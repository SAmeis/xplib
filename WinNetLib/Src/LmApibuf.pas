{******************************************************************}
{                                                                  }
{ Borland Delphi Runtime Library                                   }
{ LanManager memory allocationg functions for NT interface unit    }
{                                                                  }
{ Portions created by Microsoft are                                }
{ Copyright (C) 1995-1999 Microsoft Corporation.                   }
{ All Rights Reserved.                                             }
{                                                                  }
{ The original file is: lmapibuf.h, released 14 Nov 1998.          }
{ The original Pascal code is: LmApibuf.pas, released 29 Dec 1999. }
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

unit LmApibuf;

{$I LANMAN.INC}

{$ALIGN ON}
{$MINENUMSIZE 4}
{$IFNDEF LANMAN_DYNAMIC_LINK}
{$WEAKPACKAGEUNIT}
{$ENDIF}

interface

uses
  Windows, LmCons;

(*$HPPEMIT '#include <lmcons.h>'*)
(*$HPPEMIT '#include <lmapibuf.h>'*)

{$IFDEF LANMAN_DYNAMIC_LINK}
type
  TNetApiBufferAllocate = function (ByteCount: DWORD; var Buffer: Pointer): NET_API_STATUS; stdcall;

  TNetApiBufferFree = function (Buffer: Pointer): NET_API_STATUS; stdcall;

  TNetApiBufferReallocate = function (OldBuffer: Pointer; NewByteCount: DWORD;
    var NewBuffer: Pointer): NET_API_STATUS; stdcall;

  TNetApiBufferSize = function (Buffer: Pointer; var ByteCount: DWORD): NET_API_STATUS; stdcall;
{$ELSE}
function NetApiBufferAllocate(ByteCount: DWORD; var Buffer: Pointer): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetApiBufferAllocate}

function NetApiBufferFree(Buffer: Pointer): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetApiBufferFree}

function NetApiBufferReallocate(OldBuffer: Pointer; NewByteCount: DWORD;
  var NewBuffer: Pointer): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetApiBufferReallocate}

function NetApiBufferSize(Buffer: Pointer; var ByteCount: DWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetApiBufferSize}
{$ENDIF}

// The following private function will go away eventually.
// Call NetApiBufferAllocate instead.
// Internal Function

{$IFDEF LANMAN_DYNAMIC_LINK}
  TNetapipBufferAllocate = function (ByteCount: DWORD; var Buffer: Pointer): NET_API_STATUS; stdcall;
{$ELSE}
function NetapipBufferAllocate(ByteCount: DWORD; var Buffer: Pointer): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetapipBufferAllocate}
{$ENDIF}

{$IFDEF LANMAN_DYNAMIC_LINK}
var
  NetApiBufferAllocate: TNetApiBufferAllocate = nil;
  {$EXTERNALSYM NetApiBufferAllocate}
  NetApiBufferFree: TNetApiBufferFree = nil;
  {$EXTERNALSYM NetApiBufferFree}
  NetApiBufferReallocate: TNetApiBufferReallocate = nil;
  {$EXTERNALSYM NetApiBufferReallocate}
  NetApiBufferSize: TNetApiBufferSize = nil;
  {$EXTERNALSYM NetApiBufferSize}
  NetapipBufferAllocate: TNetapipBufferAllocate = nil;
  {$EXTERNALSYM NetapipBufferAllocate}
{$ENDIF}

{$IFDEF LANMAN_DYNAMIC_LINK}
function LmApibufLoaded: Boolean;
{$IFDEF LANMAN_DYNAMIC_LINK_EXPLICIT}
function LoadLmApibuf: Boolean;
function UnloadLmApibuf: Boolean;
{$ENDIF}
{$ENDIF}

implementation

{$IFDEF LANMAN_DYNAMIC_LINK}
var
  LibHandle: THandle = 0;

function LmApibufLoaded: Boolean;
begin
  Result := (LibHandle <> 0);
end;

function LoadLmApibuf: Boolean;
begin
  Result := LmApibufLoaded;
  if Result then Exit;
  LibHandle := LoadLibrary(netapi32lib);
  Result := LmApibufLoaded;
  if Result then
  begin
    @NetApiBufferAllocate := GetProcAddress(LibHandle, 'NetApiBufferAllocate');
    @NetApiBufferFree := GetProcAddress(LibHandle, 'NetApiBufferFree');
    @NetApiBufferReallocate := GetProcAddress(LibHandle, 'NetApiBufferReallocate');
    @NetApiBufferSize := GetProcAddress(LibHandle, 'NetApiBufferSize');
    @NetapipBufferAllocate := GetProcAddress(LibHandle, 'NetapipBufferAllocate');
  end;
end;

function UnloadLmApibuf: Boolean;
begin
  Result := True;
  if LmApibufLoaded then
  begin
    Result := FreeLibrary(LibHandle);
    LibHandle := 0;
    @NetApiBufferAllocate := nil;
    @NetApiBufferFree := nil;
    @NetApiBufferReallocate := nil;
    @NetApiBufferSize := nil;
    @NetapipBufferAllocate := nil;
  end;
end;

{$ELSE}
function NetApiBufferAllocate; external netapi32lib name 'NetApiBufferAllocate';
function NetApiBufferFree; external netapi32lib name 'NetApiBufferFree';
function NetApiBufferReallocate; external netapi32lib name 'NetApiBufferReallocate';
function NetApiBufferSize; external netapi32lib name 'NetApiBufferSize';
function NetapipBufferAllocate; external netapi32lib name 'NetapipBufferAllocate';
{$ENDIF}

{$IFDEF LANMAN_DYNAMIC_LINK}
initialization
{$IFNDEF LANMAN_DYNAMIC_LINK_EXPLICIT}
  LoadLmApibuf;
{$ENDIF}
finalization
  UnloadLmApibuf;
{$ENDIF}


end.
