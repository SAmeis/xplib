{******************************************************************}
{                                                                  }
{ Borland Delphi Runtime Library                                   }
{ LanManager helper classes                                        }
{                                                                  }
{ Portions created by Microsoft are                                }
{ Copyright (C) 1995-1999 Microsoft Corporation.                   }
{ All Rights Reserved.                                             }
{                                                                  }
{ The original Pascal code is: LmClasses.pas, released 12 Apr 2000 }
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

unit LmClasses;

{$I LANMAN.INC}

{$IFNDEF LANMAN_DYNAMIC_LINK}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!'You MUST define LANMAN_DYNAMIC_LINK conditional in LANMAN.INC when use this '!
!'unit. LANMAN.INC is placed in \Pas directory'                                !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
{$ENDIF}
{$IFNDEF LANMAN_DYNAMIC_LINK_EXPLICIT}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!'You MUST define LANMAN_DYNAMIC_LINK_EXPLICIT conditional in LANMAN.INC when '!
!'use this unit. LANMAN.INC is placed in \Pas directory'                       !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
{$ENDIF}

interface

uses
  Windows, Classes, SysUtils, Contnrs, LmCons, LmErr;

type
  TNetBaseList = class;

  TNetBaseItem = class(TObject)
  private
    FBaseList: TNetBaseList;
  public
    constructor Create(ABaseList: TNetBaseList);
  end;

  TNetBaseList = class(TObjectList)
  private
    FLastError: DWORD;
    FServerNameA: string;
    FServerNameW: WideString;
    FNoExceptions: Boolean;
    function GetServerName: string;
    procedure SetServerName(const Value: string);
    function GetServerNameA: PAnsiChar;
    function GetServerNameW: PWideChar;
  protected
    procedure BuildListNT; dynamic; abstract;
    procedure BuildListW9x; dynamic; abstract;
    function InternalCheck(RetCode: DWORD): Boolean;
    property ServerNameA: PAnsiChar read GetServerNameA;
	 property ServerNameW: PWideChar read GetServerNameW;
  public
    constructor Create(ANoExceptions: Boolean = False);
    procedure Refresh;
    property LastError: DWORD read FLastError;
    property ServerName: string read GetServerName write SetServerName;
  end;

  TNetSessionItem = class(TNetBaseItem)
  private
    FComputerName: string;
    FConnections: Integer;
    FIdleTime: DWORD;
    FKey: Word;
    FTime: DWORD;
    FOpens: Integer;
    FUserName: string;
    function GetIdleTimeStr: string;
    function GetTimeStr: string;
  public
    property ComputerName: string read FComputerName;
    property Connections: Integer read FConnections;
    property IdleTime: DWORD read FIdleTime;
    property IdleTimeStr: string read GetIdleTimeStr;
    property Key: Word read FKey; // Svrapi only
    property Opens: Integer read FOpens;
    property Time: DWORD read FTime;
    property TimeStr: string read GetTimeStr;
    property UserName: string read FUserName;
  end;

  TNetSessionList = class(TNetBaseList)
  private
    function GetItems(Index: Integer): TNetSessionItem;
  protected
	 procedure BuildListNT; override;
	 procedure BuildListW9x; override;
  public
    property Items[Index: Integer]: TNetSessionItem read GetItems; default;
  end;

  TNetShareItem = class(TNetBaseItem)
  private
    FPath: string;
    FPasswordRO: string;
    FPasswordRW: string;
    FRemark: string;
    FShareFlags: DWORD;
    FShareType: DWORD;
    FShareName: string;
    FUpdateCount: Integer;
    procedure SetPasswordRO(const Value: string);
    procedure SetPasswordRW(const Value: string);
    procedure SetPath(const Value: string);
    procedure SetRemark(const Value: string);
    procedure SetShareFlags(const Value: DWORD);
    procedure SetShareName(const Value: string);
    procedure SetShareType(const Value: DWORD);
  protected
    function UpdateNT: Boolean;
    function UpdateW9x: Boolean;
  public
    procedure BeginUpdate;
    function DeleteShare: Boolean;
    procedure EndUpdate;
    function ShareNameSame(const AShareName: string): Boolean;
    procedure Update;
    property PasswordRO: string read FPasswordRO write SetPasswordRO;
    property PasswordRW: string read FPasswordRW write SetPasswordRW;
    property Path: string read FPath write SetPath;
    property Remark: string read FRemark write SetRemark;
    property ShareFlags: DWORD read FShareFlags write SetShareFlags;
    property ShareName: string read FShareName write SetShareName;
    property ShareType: DWORD read FShareType write SetShareType;
  end;

  TNetShareList = class(TNetBaseList)
  private
	 function GetItems(Index: Integer): TNetShareItem;
  protected
	 procedure BuildListNT; override;
    procedure BuildListW9x; override;
  public
    property Items[Index: Integer]: TNetShareItem read GetItems; default;
  end;

  TNetConnectionItem = class(TNetBaseItem)
  private
    FConnectionType: DWORD;
    FOpens: DWORD;
    FShareName: string;
    FTime: DWORD;
    FUserName: string;
    function GetTimeStr: string;
  public
    function ShareNameSame(const AShareName: string): Boolean;
    function UserNameSame(const AUserName: string): Boolean;
    property ConnectionType: DWORD read FConnectionType;
    property Opens: DWORD read FOpens;
    property Time: DWORD read FTime;
    property TimeStr: string read GetTimeStr;
    property ShareName: string read FShareName;
    property UserName: string read FUserName;
  end;

  TNetConnetionList = class(TNetBaseList)
  private
    FQualifier: string;
    function GetComputerName: string;
    function GetItems(Index: Integer): TNetConnectionItem;
    function GetShareName: string;
    procedure SetComputerName(const Value: string);
    procedure SetShareName(const Value: string);
  protected
    procedure BuildListNT; override;
    procedure BuildListW9x; override;
  public
    property ComputerName: string read GetComputerName write SetComputerName;
    property Items[Index: Integer]: TNetConnectionItem read GetItems; default;
    property ShareName: string read GetShareName write SetShareName;
    constructor Create(ANoExceptions: Boolean = False);
  end;

  TNetFileItem = class(TNetBaseItem)
  private
    FID: DWORD;
    FNumLocks: DWORD;
    FPathName: string;
    FPermissions: DWORD;
    FShareName: string;
    FUserName: string;
  public
    function CloseFile: Boolean;
    function ShareNameSame(const AShareName: string): Boolean;
    function UserNameSame(const AUserName: string): Boolean;
    property ID: DWORD read FID;
    property NumLocks: DWORD read FNumLocks;
    property PathName: string read FPathName;
    property Permissions: DWORD read FPermissions;
    property ShareName: string read FShareName;
    property UserName: string read FUserName;
  end;

  TNetFileList = class(TNetBaseList)
  private
	 function GetItems(Index: Integer): TNetFileItem;
  protected
    procedure BuildListNT; override;
    procedure BuildListW9x; override;
  public
    property Items[Index: Integer]: TNetFileItem read GetItems; default;
  end;

function LmClassesLoaded: Boolean;

function AddDoubleSlash(const S: string): string;
function CutDoubleSlash(const S: string): string;
function IsDoubleSlashed(const S: string): Boolean;

implementation

uses
  SvrApi, LmShare, LmApiBuf, LmUtils;

const
  MaxNetArrayItems = 512;

function AddDoubleSlash(const S: string): string;
//----------------------------------------------------------------------------------------------------------------------------------
begin
	if Copy(S, 1, 2) = '\\' then begin
		Result := S;
	end else begin
		Result := '\\' + S;
	end;
end;

function CutDoubleSlash(const S: string): string;
//----------------------------------------------------------------------------------------------------------------------------------
begin
	if Copy(S, 1, 2) = '\\' then begin
		Result := Copy(S, 3, Length(S))
	end else begin
		Result := S;
	end;
end;

function IsDoubleSlashed(const S: string): Boolean;
//----------------------------------------------------------------------------------------------------------------------------------
begin
	Result := (Copy(S, 1, 2) = '\\');
end;

function SecondsToStr(Sec: DWORD): string;
var
  TS: TTimeStamp;
begin
  TS.Time := Sec * 1000;
  TS.Date := 1;
  Result := FormatDateTime('tt', TimeStampToDateTime(TS));
end;

var
  LmClassesLoadedFlag: Boolean = False;

function LmClassesLoaded: Boolean;
begin
  Result := LmClassesLoadedFlag;
end;

procedure LoadLmClasses;
//----------------------------------------------------------------------------------------------------------------------------------
//Carrega DLL com a API de rede de acordo com o sistema operacional presente
begin
{$IFDEF FORCE_NT}
	LmClassesLoadedFlag := LoadLmApiBuf and LoadLmShare;
{$ELSE}
	if Win32Platform = VER_PLATFORM_WIN32_WINDOWS then begin
		LmClassesLoadedFlag := LoadSvrApi;
	end else begin
		LmClassesLoadedFlag := LoadLmApiBuf and LoadLmShare;
	end;
{$ENDIF}
end;

procedure UnloadLmClasses;
//----------------------------------------------------------------------------------------------------------------------------------
//Descarrega DLL com a API de rede de acordo com o sistema operacional presente
begin
	if LmClassesLoadedFlag then begin
		UnloadLmShare;
		UnloadLmApibuf;
		UnloadSvrApi;
		LmClassesLoadedFlag := False;
	end;
end;

{ TNetBaseItem }

constructor TNetBaseItem.Create(ABaseList: TNetBaseList);
//----------------------------------------------------------------------------------------------------------------------------------
begin
	inherited Create;
	FBaseList := ABaseList;
end;

{ TNetBaseList }

constructor TNetBaseList.Create(ANoExceptions: Boolean);
//----------------------------------------------------------------------------------------------------------------------
begin
   inherited Create(True);
   FNoExceptions := ANoExceptions;
	Self.FLastError:=NERR_Success; //Seta LastError para 0
end;

function TNetBaseList.GetServerName: string;
//----------------------------------------------------------------------------------------------------------------------------------
begin
	Result := CutDoubleSlash(FServerNameA);
end;

function TNetBaseList.GetServerNameA: PAnsiChar;
//----------------------------------------------------------------------------------------------------------------------------------
begin
	Result := PAnsiChar(FServerNameA);
end;

function TNetBaseList.GetServerNameW: PWideChar;
//----------------------------------------------------------------------------------------------------------------------------------
begin
	Result := PWideChar(FServerNameW);
end;

function TNetBaseList.InternalCheck(RetCode: DWORD): Boolean;
//----------------------------------------------------------------------------------------------------------------------------------
begin
	FLastError := RetCode;
	Result := (RetCode = NERR_Success);
	if not Result and not FNoExceptions then begin
		NetCheck(RetCode);
	end;
end;

procedure TNetBaseList.Refresh;
//----------------------------------------------------------------------------------------------------------------------------------
begin
	Clear;  //Limpa e destroi elementos anteriores
	FLastError := 0;
	{$IFDEF FORCE_NT}
	BuildListNT;
	{$ELSE}
	if Win32Platform = VER_PLATFORM_WIN32_WINDOWS then begin
		BuildListW9x;
	end else begin
		BuildListNT;
	end;
	{$ENDIF}
end;

procedure TNetBaseList.SetServerName(const Value: string);
//----------------------------------------------------------------------------------------------------------------------------------
begin
	if FServerNameA <> AddDoubleSlash(Value) then begin
		FServerNameA := AddDoubleSlash(Value);
		FServerNameW := FServerNameA;
		Refresh;
  	end;
end;

{ TNetSessionItem }

function TNetSessionItem.GetIdleTimeStr: string;
//----------------------------------------------------------------------------------------------------------------------------------
begin
	Result := SecondsToStr(FIdleTime);
end;

function TNetSessionItem.GetTimeStr: string;
//----------------------------------------------------------------------------------------------------------------------------------
begin
	Result := SecondsToStr(FTime);
end;

{ TNetSessionList }

function TNetSessionList.GetItems(Index: Integer): TNetSessionItem;
//----------------------------------------------------------------------------------------------------------------------------------
begin
	Result := TNetSessionItem(inherited Items[Index]);
end;

procedure TNetSessionList.BuildListNT;
//----------------------------------------------------------------------------------------------------------------------------------
var
	EntriesRead, TotalAvail: DWORD;
	Buf: Pointer;
	TempBuf: LmShare.PSessionInfo2;
	I: Integer;
	Item: TNetSessionItem;
begin
	if InternalCheck( LmShare.NetSessionEnum(PWideChar(ServerNameW), nil, nil, 2, Buf, MAX_PREFERRED_LENGTH, EntriesRead,
					  TotalAvail, nil)) then
	begin
		TempBuf := Buf;
		for I := 1 to EntriesRead do begin
			with TempBuf^ do begin
				Item := TNetSessionItem.Create(Self);
				Item.FComputerName := sesi2_cname;
				Item.FConnections := -1;
				Item.FIdleTime := sesi2_idle_time;
				Item.FKey := 0;
				Item.FTime := sesi2_time;
				Item.FOpens := sesi2_num_opens;
				Item.FUserName := sesi2_username;
				Add(Item);
			end;
			Inc(TempBuf);
		end;
		NetApiBufferFree(Buf);
	end;
end;

procedure TNetSessionList.BuildListW9x;
//----------------------------------------------------------------------------------------------------------------------------------
var
	SessionInfo: array[0..MaxNetArrayItems] of SvrApi.TSessionInfo50;
	EntriesRead, TotalAvail : Word;
	I : Integer;
	Item : TNetSessionItem;
begin
	if InternalCheck(SvrApi.NetSessionEnum(PChar(ServerNameA), 50, @SessionInfo, Sizeof(SessionInfo), EntriesRead, TotalAvail)) then begin
		for I := 0 to EntriesRead - 1 do begin
			Item := TNetSessionItem.Create(Self);
			Item.FComputerName := SessionInfo[I].sesi50_cname;
			Item.FConnections := SessionInfo[I].sesi50_num_conns;
			Item.FIdleTime := SessionInfo[I].sesi50_idle_time;
			Item.FKey := SessionInfo[I].sesi50_key;
			Item.FTime := SessionInfo[I].sesi50_time;
			Item.FOpens := SessionInfo[I].sesi50_num_opens;
			Item.FUserName := SessionInfo[I].sesi50_username;
			Add(Item);
		end;
	end;
end;

{ TNetShareItem }

procedure TNetShareItem.BeginUpdate;
//----------------------------------------------------------------------------------------------------------------------------------
begin
	Inc(FUpdateCount);
end;

function TNetShareItem.DeleteShare: Boolean;
//----------------------------------------------------------------------------------------------------------------------------------
var
  ShareNameW: WideString;
begin
{$IFDEF FORCE_NT}
	with FBaseList do begin
		ShareNameW := FShareName;
		Result := InternalCheck(LmShare.NetShareDel(ServerNameW, PWideChar(ShareNameW), 0));
	end;
{$ELSE}
	with FBaseList do begin
		if Win32Platform = VER_PLATFORM_WIN32_WINDOWS then begin
			Result := InternalCheck(SvrApi.NetShareDel(ServerNameA, PChar(FShareName), 0))
		end else begin
			ShareNameW := FShareName;
			Result := InternalCheck(LmShare.NetShareDel(ServerNameW, PWideChar(ShareNameW), 0));
		end;
	end;
{$ENDIF}
end;

procedure TNetShareItem.EndUpdate;
//----------------------------------------------------------------------------------------------------------------------------------
begin
	if FUpdateCount > 0 then begin
		Dec(FUpdateCount);
		if FUpdateCount = 0 then begin
			Update;
		end;
	end;
end;

procedure TNetShareItem.SetPasswordRO(const Value: string);
//----------------------------------------------------------------------------------------------------------------------------------
begin
	if FPasswordRO <> Value then begin
		FPasswordRO := Value;
		Update;
	end;
end;

procedure TNetShareItem.SetPasswordRW(const Value: string);
//----------------------------------------------------------------------------------------------------------------------------------
begin
	if FPasswordRW <> Value then begin
		FPasswordRW := Value;
	  	Update;
	end;
end;

procedure TNetShareItem.SetPath(const Value: string);
//----------------------------------------------------------------------------------------------------------------------------------
begin
	if FPath <> Value then begin
		FPath := Value;
	  	Update;
	end;
end;

procedure TNetShareItem.SetRemark(const Value: string);
//----------------------------------------------------------------------------------------------------------------------------------
begin
	if FRemark <> Value then begin
	  	FRemark := Value;
		Update;
	end;
end;

procedure TNetShareItem.SetShareFlags(const Value: DWORD);
//----------------------------------------------------------------------------------------------------------------------------------
begin
	if FShareFlags <> Value then begin
	  	FShareFlags := Value;
		Update;
	end;
end;

procedure TNetShareItem.SetShareName(const Value: string);
//----------------------------------------------------------------------------------------------------------------------------------
begin
	if FShareName <> Value then begin
		FShareName := Value;
	  	Update;
	end;
end;

procedure TNetShareItem.SetShareType(const Value: DWORD);
//----------------------------------------------------------------------------------------------------------------------------------
begin
	if FShareType <> Value then begin
		FShareType := Value;
	  	Update;
	end;
end;

function TNetShareItem.ShareNameSame(const AShareName: string): Boolean;
//----------------------------------------------------------------------------------------------------------------------------------
begin
	Result := AnsiCompareText(AShareName, ShareName) = 0;
end;

procedure TNetShareItem.Update;
//----------------------------------------------------------------------------------------------------------------------------------
begin
	if FUpdateCount <> 0 then begin
		Exit;
	end;
	{$IFDEF FORCE_NT}
	UpdateNT;
	{$ELSE}
	if Win32Platform = VER_PLATFORM_WIN32_WINDOWS then begin
		UpdateW9x
	end else begin
		UpdateNT;
	end;
	{$ENDIF}
end;

function TNetShareItem.UpdateNT: Boolean;
//----------------------------------------------------------------------------------------------------------------------------------
var
	ShareInfo: LmShare.TShareInfo2;
begin
	ZeroMemory(@ShareInfo, Sizeof(ShareInfo));
	Result:=True;
end;

function TNetShareItem.UpdateW9x: Boolean;
//----------------------------------------------------------------------------------------------------------------------------------
var
	ShareInfo: SvrApi.TShareInfo50;
begin
	ZeroMemory(@ShareInfo, Sizeof(ShareInfo));
	StrPCopy(ShareInfo.shi50_netname, ShareName);
	ShareInfo.shi50_type := ShareType;
	ShareInfo.shi50_flags := ShareFlags;
	StrPCopy(ShareInfo.shi50_remark, Remark);
	StrPCopy(ShareInfo.shi50_path, Path);
	StrPCopy(ShareInfo.shi50_rw_password, PasswordRW);
	StrPCopy(ShareInfo.shi50_ro_password, PasswordRO);
	with FBaseList do begin
	  Result := InternalCheck(SvrApi.NetShareSetInfo(ServerNameA, PChar(ShareName), 50, @ShareInfo, Sizeof(ShareInfo), PARMNUM_ALL));
	end;
end;

{ TNetShareList }

procedure TNetShareList.BuildListNT;
//----------------------------------------------------------------------------------------------------------------------------------
var
	Buf : Pointer;
	TempBuf : LmShare.PShareInfo2;
	EntriesRead, TotalAvail : DWORD;
	I : Integer;
	Item : TNetShareItem;
begin
	if InternalCheck(LmShare.NetShareEnum(ServerNameW, 2, Buf, MAX_PREFERRED_LENGTH, EntriesRead, TotalAvail, nil)) then begin
		TempBuf := Buf;
		for I := 1 to EntriesRead do begin
			Item := TNetShareItem.Create(Self);
			Item.FPath := TempBuf^.shi2_netname ;
			Item.FPasswordRO := TempBuf^.shi2_passwd;
			Item.FPasswordRW := TempBuf^.shi2_passwd;
			Item.FRemark := TempBuf^.shi2_remark;
			Item.FShareFlags := 0;
			Item.FShareType := TempBuf^.shi2_type;
			Item.FShareName := TempBuf^.shi2_netname;
			Add( Item );
			Inc(TempBuf);
		end;
		NetApiBufferFree(Buf);
	end;
end;

procedure TNetShareList.BuildListW9x;
//----------------------------------------------------------------------------------------------------------------------------------
var
	ShareInfo : array[0..MaxNetArrayItems] of SvrApi.TShareInfo50;
	EntriesRead, TotalAvail : Word;
	I : Integer;
	Item : TNetShareItem;
begin
	if InternalCheck(SvrApi.NetShareEnum(ServerNameA, 50, @ShareInfo, Sizeof(ShareInfo), EntriesRead, TotalAvail)) then begin
		for I := 0 to EntriesRead - 1 do begin
			Item := TNetShareItem.Create(Self);
			Item.FPath := ShareInfo[I].shi50_path;
			Item.FPasswordRO := ShareInfo[I].shi50_ro_password;
			Item.FPasswordRW := ShareInfo[I].shi50_rw_password;
			Item.FRemark := ShareInfo[I].shi50_remark;
			Item.FShareFlags := ShareInfo[I].shi50_flags;
			Item.FShareType := ShareInfo[I].shi50_type;
			Item.FShareName := ShareInfo[I].shi50_netname;
			Add(Item);
		end;
	end;
end;

function TNetShareList.GetItems(Index: Integer): TNetShareItem;
//----------------------------------------------------------------------------------------------------------------------------------
begin
	Result := TNetShareItem(inherited Items[Index]);
end;

{ TNetConnectionItem }

function TNetConnectionItem.GetTimeStr: string;
//----------------------------------------------------------------------------------------------------------------------------------
begin
	Result := SecondsToStr(FTime);
end;

function TNetConnectionItem.ShareNameSame(const AShareName: string): Boolean;
//----------------------------------------------------------------------------------------------------------------------------------
begin
	Result := AnsiCompareText(AShareName, ShareName) = 0;
end;

function TNetConnectionItem.UserNameSame(const AUserName: string): Boolean;
//----------------------------------------------------------------------------------------------------------------------------------
begin
	Result := AnsiCompareText(AUserName, UserName) = 0;
end;

{ TNetConnetionList }

procedure TNetConnetionList.BuildListNT;
//----------------------------------------------------------------------------------------------------------------------------------
var
	Buf : Pointer;
	TempBuf : LmShare.PConnectionInfo1;
	EntriesRead, TotalAvail : DWORD;
	I : Integer;
	Item : TNetConnectionItem;
	QualifierW : WideString;
begin
	QualifierW := FQualifier;
	if InternalCheck( LmShare.NetConnectionEnum(ServerNameW, PWideChar(QualifierW), 1, Buf, MAX_PREFERRED_LENGTH, EntriesRead,
					  TotalAvail, nil)) then
	begin
		TempBuf := Buf;
		for I := 1 to EntriesRead do begin
			Item := TNetConnectionItem.Create(Self);
			Item.FConnectionType := TempBuf^.coni1_type;
			Item.FOpens := TempBuf^.coni1_num_opens;
			Item.FShareName := TempBuf^.coni1_netname;
			Item.FTime := TempBuf^.coni1_time;
			Item.FUserName := TempBuf^.coni1_username;
			Inc(TempBuf);
			Add( Item );
		end;
		NetApiBufferFree(Buf);
	end;
end;

procedure TNetConnetionList.BuildListW9x;
//----------------------------------------------------------------------------------------------------------------------------------
var
	ConnectionInfo : array[0..MaxNetArrayItems] of SvrApi.TConnectionInfo50;
	EntriesRead, TotalAvail : Word;
	I : Integer;
	Item : TNetConnectionItem;
begin
	if Length(CutDoubleSlash(FQualifier)) = 0 then begin
		Exit;
	end;
	if InternalCheck( SvrApi.NetConnectionEnum(ServerNameA, PChar(FQualifier), 50, @ConnectionInfo, Sizeof(ConnectionInfo),
					  EntriesRead, TotalAvail)) then
	begin
		for I := 0 to EntriesRead - 1 do begin
			Item := TNetConnectionItem.Create(Self);
			Item.FConnectionType := ConnectionInfo[I].coni50_type;
			Item.FOpens := ConnectionInfo[I].coni50_num_opens;
			Item.FShareName := ConnectionInfo[I].coni50_netname;
			Item.FTime := ConnectionInfo[I].coni50_time;
			Item.FUserName := ConnectionInfo[I].coni50_username;
			Add(Item);
		end;
	end;
end;

constructor TNetConnetionList.Create(ANoExceptions: Boolean);
//----------------------------------------------------------------------------------------------------------------------
begin
   inherited;
   Self.FQualifier:='\';
end;

function TNetConnetionList.GetComputerName: string;
//----------------------------------------------------------------------------------------------------------------------------------
begin
	if IsDoubleSlashed( FQualifier ) then begin
		Result := CutDoubleSlash(FQualifier)
	end else begin
		Result := EmptyStr;
	end;
end;

function TNetConnetionList.GetItems(Index: Integer): TNetConnectionItem;
//----------------------------------------------------------------------------------------------------------------------------------
begin
	Result := TNetConnectionItem(inherited Items[Index]);
end;

function TNetConnetionList.GetShareName: string;
//----------------------------------------------------------------------------------------------------------------------------------
begin
	if not IsDoubleSlashed(FQualifier) then begin
		Result := FQualifier
	end else begin
		Result := EmptyStr;
	end;
end;

procedure TNetConnetionList.SetComputerName(const Value: string);
//----------------------------------------------------------------------------------------------------------------------------------
begin
	FQualifier := AddDoubleSlash(Value);
	Refresh();
end;

procedure TNetConnetionList.SetShareName(const Value: string);
//----------------------------------------------------------------------------------------------------------------------------------
begin
	FQualifier := CutDoubleSlash(Value);
	Refresh();
end;

{ TNetFileItem }

function TNetFileItem.CloseFile: Boolean;
//----------------------------------------------------------------------------------------------------------------------------------
begin
	with FBaseList do begin
		if Win32Platform = VER_PLATFORM_WIN32_WINDOWS then begin
			Result := InternalCheck(SvrApi.NetFileClose2(ServerNameA, ID))
		end else begin
			Result := InternalCheck(LmShare.NetFileClose(ServerNameW, ID));
		end;
	end;
end;

function TNetFileItem.ShareNameSame(const AShareName: string): Boolean;
//----------------------------------------------------------------------------------------------------------------------------------
begin
	Result := AnsiCompareText(AShareName, ShareName) = 0;
end;

function TNetFileItem.UserNameSame(const AUserName: string): Boolean;
//----------------------------------------------------------------------------------------------------------------------------------
begin
	Result := AnsiCompareText(AUserName, UserName) = 0;
end;

{ TNetFileList }

procedure TNetFileList.BuildListNT;
//----------------------------------------------------------------------------------------------------------------------------------
var
	Buf: Pointer;
	TempBuf : LmShare.PFileInfo3;
	EntriesRead, TotalAvail : DWORD;
	I : Integer;
	Item : TNetFileItem;
begin
	//*** Usado 3 no lugar de 1 como level
	if InternalCheck(LmShare.NetFileEnum(ServerNameW, nil, nil, 3, Buf, MAX_PREFERRED_LENGTH, EntriesRead, TotalAvail, nil)) then
	begin
		TempBuf := Buf;
		for I := 1 to EntriesRead do begin
			Item := TNetFileItem.Create(Self);
			Item.FID := TempBuf^.fi3_id;
			Item.FNumLocks := TempBuf^.fi3_num_locks;
			Item.FPathName := TempBuf^.fi3_pathname;
			Item.FPermissions := TempBuf^.fi3_permissions;
			Item.FShareName := EmptyStr;
			Item.FUserName := TempBuf^.fi3_username;
			Add(Item);
			Inc(TempBuf);
		end;
		NetApiBufferFree(Buf);
	end;
end;

procedure TNetFileList.BuildListW9x;
//----------------------------------------------------------------------------------------------------------------------------------
var
	FileInfo : array[0..MaxNetArrayItems] of SvrApi.TFileInfo50;
	EntriesRead, TotalAvail : Word;
	I : Integer;
	Item : TNetFileItem;
begin
  if InternalCheck(SvrApi.NetFileEnum(ServerNameA, nil, 50, @FileInfo, Sizeof(FileInfo), EntriesRead, TotalAvail)) then begin
	   for I := 0 to EntriesRead - 1 do begin
		   Item := TNetFileItem.Create(Self);
		   Item.FID := FileInfo[I].fi50_id;
		   Item.FNumLocks := FileInfo[I].fi50_num_locks;
		   Item.FPathName := FileInfo[I].fi50_pathname;
		   Item.FPermissions := FileInfo[I].fi50_permissions;
		   Item.FShareName := FileInfo[I].fi50_sharename;
		   Item.FUserName := FileInfo[I].fi50_username;
		   Add(Item);
	   end;
  end;
end;

function TNetFileList.GetItems(Index: Integer): TNetFileItem;
//----------------------------------------------------------------------------------------------------------------------------------
begin
	Result := TNetFileItem(inherited Items[Index]);
end;

initialization
  LoadLmClasses;
finalization
  UnloadLmClasses;
end.
