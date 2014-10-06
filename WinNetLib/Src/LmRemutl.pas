{$IFDEF LmRemUtl}
{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I WinNetLib.inc}
{ ****************************************************************** }
{ }
{ Borland Delphi Runtime Library }
{ LanManager share functions for Windows NT interface unit }
{ }
{ Portions created by Microsoft are }
{ Copyright (C) 1995-1999 Microsoft Corporation. }
{ All Rights Reserved. }
{ }
{ The original file is: lmremutl.h, released 14 Nov 1998. }
{ The original Pascal code is: LmRemUtl.pas, released 13 Jan 2000. }
{ The initial developer of the Pascal code is Petr Vones }
{ (petr.v@mujmail.cz). }
{ }
{ Portions created by Petr Vones are }
{ Copyright (C) 2000 Petr Vones }
{ }
{ Obtained through: }
{ }
{ Joint Endeavour of Delphi Innovators (Project JEDI) }
{ }
{ You may retrieve the latest version of this file at the Project }
{ JEDI home page, located at http://delphi-jedi.org }
{ }
{ The contents of this file are used with permission, subject to }
{ the Mozilla Public License Version 1.1 (the "License"); you may }
{ not use this file except in compliance with the License. You may }
{ obtain a copy of the License at }
{ http://www.mozilla.org/MPL/MPL-1.1.html }
{ }
{ Software distributed under the License is distributed on an }
{ "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or }
{ implied. See the License for the specific language governing }
{ rights and limitations under the License. }
{ }
{ ****************************************************************** }

unit LmRemUtl;

{$I LANMAN.INC}
{$ALIGN ON}
{$MINENUMSIZE 4}
{ $ WEAKPACKAGEUNIT }

interface

uses
	Windows, LmCons;

(*$HPPEMIT '#include <lmremutl.h>'*)
{$IFDEF LANMAN_DYNAMIC_LINK}

type
	TNetRemoteTOD              = function(UncServerName: LPCWSTR; BufferPtr: Pointer): NET_API_STATUS; stdcall;
	TNetRemoteComputerSupports = function(UncServerName: LPCWSTR; OptionsWanted: DWORD; var OptionsSupported: DWORD)
		: NET_API_STATUS; stdcall;

{$ELSE}
function NetRemoteTOD(UncServerName: LPCWSTR; BufferPtr: Pointer): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetRemoteTOD}
function NetRemoteComputerSupports(UncServerName: LPCWSTR; OptionsWanted: DWORD; var OptionsSupported: DWORD)
	: NET_API_STATUS; stdcall;
{$EXTERNALSYM NetRemoteComputerSupports}
{$ENDIF}

type
	PTimeOfDayInfo = ^TTimeOfDayInfo;

	_TIME_OF_DAY_INFO = record
		tod_elapsedt: DWORD;
		tod_msecs: DWORD;
		tod_hours: DWORD;
		tod_mins: DWORD;
		tod_secs: DWORD;
		tod_hunds: DWORD;
		tod_timezone: LongInt;
		tod_tinterval: DWORD;
		tod_day: DWORD;
		tod_month: DWORD;
		tod_year: DWORD;
		tod_weekday: DWORD;
	end;
	{$EXTERNALSYM _TIME_OF_DAY_INFO}

	TTimeOfDayInfo   = _TIME_OF_DAY_INFO;
	TIME_OF_DAY_INFO = _TIME_OF_DAY_INFO;
	{$EXTERNALSYM TIME_OF_DAY_INFO}
	//Mask bits for use with NetRemoteComputerSupports:

const
	SUPPORTS_REMOTE_ADMIN_PROTOCOL = $00000002;
	{ EXTERNALSYM SUPPORTS_REMOTE_ADMIN_PROTOCOL }
	SUPPORTS_RPC = $00000004;
	{ EXTERNALSYM SUPPORTS_RPC }
	SUPPORTS_SAM_PROTOCOL = $00000008;
	{ EXTERNALSYM SUPPORTS_SAM_PROTOCOL }
	SUPPORTS_UNICODE = $00000010;
	{ EXTERNALSYM SUPPORTS_UNICODE }
	SUPPORTS_LOCAL = $00000020;
	{ EXTERNALSYM SUPPORTS_LOCAL }
	SUPPORTS_ANY = $FFFFFFFF;
	{ EXTERNALSYM SUPPORTS_ANY }

	//Flag bits for RxRemoteApi:

	NO_PERMISSION_REQUIRED = $00000001; //set if use NULL session;
	{ EXTERNALSYM NO_PERMISSION_REQUIRED }
	ALLOCATE_RESPONSE = $00000002; //set if RxRemoteApi allocates response buffer;
	{ EXTERNALSYM ALLOCATE_RESPONSE }
	USE_SPECIFIC_TRANSPORT = $80000000;
	{ EXTERNALSYM USE_SPECIFIC_TRANSPORT }

	{$IFDEF LANMAN_DYNAMIC_LINK}

var
	NetRemoteTOD             : TNetRemoteTOD              = nil;
	NetRemoteComputerSupports: TNetRemoteComputerSupports = nil;
	{$ENDIF}
	{$IFDEF LANMAN_DYNAMIC_LINK}
function LmRemUtlLoaded: Boolean;
{$IFDEF LANMAN_DYNAMIC_LINK_EXPLICIT}
function LoadLmRemUtl: Boolean;
function UnloadLmRemUtl: Boolean;
{$ENDIF}
{$ENDIF}

implementation

{$IFDEF LANMAN_DYNAMIC_LINK}

var
	LibHandle: THandle = 0;

function LmRemUtlLoaded: Boolean;
//----------------------------------------------------------------------------------------------------------------------
begin
	Result :=(LibHandle <> 0);
end;
{$ENDIF}
{$IFDEF LANMAN_DYNAMIC_LINK_EXPLICIT}

function LoadLmRemUtl: Boolean;
//----------------------------------------------------------------------------------------------------------------------
begin
	Result := LmRemUtlLoaded;
	if Result then begin
		Exit;
	end;
	LibHandle := LoadLibrary(netapi32lib);
	Result    := LmRemUtlLoaded;
	if Result then begin
		@NetRemoteTOD              := GetProcAddress(LibHandle, 'NetRemoteTOD');
		@NetRemoteComputerSupports := GetProcAddress(LibHandle, 'NetRemoteComputerSupports');
	end;
	//Teste dosenderecos
	{$TYPEDADDRESS OFF}
	Result :=(@NetRemoteTOD <> nil) and (@NetRemoteComputerSupports <> nil);
	{$TYPEDADDRESS ON}
end;

function UnloadLmRemUtl(): Boolean;
//----------------------------------------------------------------------------------------------------------------------
begin
	Result := True;
	if LmRemUtlLoaded then begin
		Result                     := FreeLibrary(LibHandle);
		LibHandle                  := 0;
		@NetRemoteTOD              := nil;
		@NetRemoteComputerSupports := nil;
	end;
end;
{$ENDIF}
{$IFNDEF LANMAN_DYNAMIC_LINK}
function NetRemoteTOD; external netapi32lib name 'NetRemoteTOD';
function NetRemoteComputerSupports; external netapi32lib name 'NetRemoteComputerSupports';
{$ENDIF}
{$IFDEF LANMAN_DYNAMIC_LINK}

initialization

{$IFNDEF LANMAN_DYNAMIC_LINK_EXPLICIT}
begin
	LoadLmRemUtl;
end;
{$ENDIF}

finalization

UnloadLmRemUtl;
{$ENDIF}

end.
