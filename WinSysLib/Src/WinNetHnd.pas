{$IFDEF WinNetHnd}
{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I WinSysLib.inc}
unit WinNetHnd;

interface

uses
	Types, Windows, SysUtils;

//Toma o nome do computador local
function GetComputerName(): string;

//Toma o nome do usuario primario logado para este processo
function GetUserName: string;

//Toma o nome do usuario logado na maquina pelo valor da env_var USERNAME
function GetUserNameLogged(): string;

//Indica se existe rede no computador em execucao
function NetworkIsPresent(): boolean;

implementation

function GetComputerName(): string;
//----------------------------------------------------------------------------------------------------------------------
var
	Buf: array[0 .. MAX_COMPUTERNAME_LENGTH + 1] of char;
	Len: cardinal;
begin
	Len := high(Buf);
	if Windows.GetComputerName(Buf, Len) then begin
		Result := string(Buf);
	end else begin
		Result := EmptyStr;
	end;
end;

function GetUserName: string;
//----------------------------------------------------------------------------------------------------------------------
var
	Buf: array[0 .. 2 * MAX_COMPUTERNAME_LENGTH + 1] of char;
	Len: cardinal;
begin
	Len := high(Buf);
	if Windows.GetUserName(Buf, Len) then begin
		Result := string(Buf);
	end else begin
		Result := EmptyStr;
	end;
end;

function GetUserNameLogged(): string;
//----------------------------------------------------------------------------------------------------------------------------------
const
	VAR_NAME = '%username%';
var
	Buf: array[0 .. 255] of char;
begin
	if ExpandEnvironmentStrings('%username%', Buf, 254) > 0 then begin
		Result := Buf;
		if SameText(Result, VAR_NAME) then begin //Ninguem logado na maquina
			Result := EmptyStr;
		end;
	end else begin
		RaiseLastOSError;
	end;
end;

function NetworkIsPresent(): boolean;
//----------------------------------------------------------------------------------------------------------------------------------
//Indica se existe rede no computador em execucao
begin
	Result := GetSystemMetrics(SM_NETWORK) and $01 = $01;
end;

end.
