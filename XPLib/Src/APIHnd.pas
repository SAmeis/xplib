{$IFDEF APIHnd}
{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I XPLib.inc}
unit APIHnd;

interface

uses
	Windows, SysUtils, SysConst;

const
	///<summary>
	///Erro base para todos os usados no modelo de chamada da API clássica windows derivados
	///</summary>
	ERROR_XP_BASE         = 80000;
	ERROR_XP_ALREADY_DONE = ERROR_XP_BASE + 1;

	{$IFNDEF VER270} //alterar para futura migração para versão posterior
	{$MESSAGE 'Validar as constantes e explicitar em local apropriado futuramente' }
	{$ENDIF}
	NERR_BASE = 2100;
	NERR_MAX  = NERR_BASE + 899;

	///<summary>
	///Monta excessao de acordo com o codigo de erro informado pelo sistema operacional/API.
	///Esta rotina espera por implementacao de codigos extendidos desta biblioteca
	///
	///Depreciada em favor para o mesmo metodo de classe em TAPIHnd.
	///</summary>
function CheckAPI(LastError: Integer): boolean; deprecated;

///<summary>
///Retorna a string com o valor da variavel e ambiente
///Depreciada em favor para o mesmo metodo de classe em TAPIHnd.
///</summary>
function GetEnvironmentVar(const EnvVar: string): string; deprecated;

type
	///<author>roger</author>
	///<since>20100626</since>
	///<version>1.0</version>
	TAPIHnd = class(TObject)
	private
		class function NetErrorMessage(ErrorCode: Cardinal): string;
	public
		///<author>roger</author>
		///<input>API error code</input>
		///<summary>
		///Monta excessao de acordo com o codigo de erro informado pelo sistema operacional/API.
		///Esta rotina espera por implementacao de codigos extendidos desta biblioteca
		///</summary>
		class function CheckAPI(LastError: Integer): boolean;
		///<summary>
		///Retorna a string com o valor da variavel de ambiente
		///</summary>
		class function GetEnvironmentVar(const EnvVar: string): string;
		///<summary>
		///Traduz todas as mensagens de erro extendidos ou não da Win API
		///</summary>
		class function SysErrorMessageEx(LastError: Integer): string;
	end;

implementation

//
function CheckAPI(LastError: Integer): boolean;
var
	Error: EOSError;
begin
	if LastError <> 0 then begin
		{$WARN UNSAFE_CODE OFF}
		Error := SysUtils.EOSError.CreateResFmt(PResStringRec(@SysConst.SOSError),
			[LastError, SysUtils.SysErrorMessage(LastError)]);
		{$WARN UNSAFE_CODE ON}
		Error.ErrorCode := LastError;
		raise Error;
	end else begin
		Result := True;
	end;
end;

function GetEnvironmentVar(const EnvVar: string): string; deprecated;
var
	Match, pEnv, Pt: PChar;
begin
	{$IFDEF WIN32}
	pEnv := GetEnvironmentStrings;
	{$ELSE}
	pEnv := GetDosEnvironment;
	{$ENDIF}
	Match := StrAlloc(128);
	try
		StrCopy(Match, PChar(EnvVar));
		StrCat(Match, '=');
		Match := StrUpper(Match);
		repeat
			if StrLen(pEnv) = 0 then begin
				Result := EmptyStr;
				Exit;
			end;
			Pt := StrPos(StrUpper(pEnv), Match); //Localiza cadeia "var="
			if Pt = nil then begin
				pEnv := StrEnd(pEnv);
				Inc(pEnv);
			end;
		until Pt <> nil;
		Pt := StrPos(StrUpper(pEnv), Match);
		Inc(Pt, StrLen(Match));
		Result := StrPas(Pt);
	finally
		StrDispose(Match);
	end;
end;

class function TAPIHnd.CheckAPI(LastError: Integer): boolean;
{ {
  Revision - 20141022 - roger - Adequado ao XE6+

  Revision: 18/7/2005 - Criada
}
var
	Error: EOSError;
	Msg  : string;
begin
	if LastError <> 0 then begin
		if (LastError >= NERR_BASE) and (LastError <= NERR_MAX) then begin
			//Captura os erros da API de rede
			Msg := NetErrorMessage(LastError);
		end else begin
			Msg := SysUtils.SysErrorMessage(LastError);
		end;
		{$WARN UNSAFE_CODE OFF}
		//No XE6 cadeia possui parametro a mais agora
		Error := SysUtils.EOSError.CreateResFmt(PResStringRec(@SysConst.SOSError), [LastError, Msg, '']);
		{$WARN UNSAFE_CODE ON}
		Error.ErrorCode := LastError;
		raise Error;
	end else begin
		Result := True;
	end;
end;

class function TAPIHnd.GetEnvironmentVar(const EnvVar: string): string;
{ {
  Revision: 18/7/2005
}
var
	Match, pEnv, Pt: PChar;
begin
	{$IFDEF WIN32}
	pEnv := GetEnvironmentStrings;
	{$ELSE}
	pEnv := GetDosEnvironment;
	{$ENDIF}
	Match := StrAlloc(128);
	try
		StrCopy(Match, PChar(EnvVar));
		StrCat(Match, '=');
		Match := StrUpper(Match);
		repeat
			if StrLen(pEnv) = 0 then begin
				Result := EmptyStr;
				Exit;
			end;
			Pt := StrPos(StrUpper(pEnv), Match); //Localiza cadeia "var="
			if Pt = nil then begin
				pEnv := StrEnd(pEnv);
				Inc(pEnv);
			end;
		until Pt <> nil;
		Pt := StrPos(StrUpper(pEnv), Match);
		Inc(Pt, StrLen(Match));
		Result := StrPas(Pt);
	finally
		StrDispose(Match);
	end;
end;

class function TAPIHnd.NetErrorMessage(ErrorCode: Cardinal): string;
const
	ResourceLibName: array [boolean] of PChar = ('netmsg95.dll', 'netmsg.dll');
var
	LibHandle: THandle;
	Buffer   : array [0 .. 1023] of char;
begin
	LibHandle := LoadLibraryEx(ResourceLibName[Win32Platform = VER_PLATFORM_WIN32_NT], 0, LOAD_LIBRARY_AS_DATAFILE);
	if LibHandle <> 0 then begin
		SetString(Result, Buffer, LoadString(LibHandle, ErrorCode, Buffer, Length(Buffer)));
		FreeLibrary(LibHandle);
	end else begin
		Result := Format('Erro extendido de rede desconhecido(%d)', [ErrorCode]);;
	end;
end;

class function TAPIHnd.SysErrorMessageEx(LastError: Integer): string;
begin
	if (LastError >= NERR_BASE) and (LastError <= NERR_MAX) then begin
		//Captura os erros da API de rede
		Result := NetErrorMessage(LastError);
	end else begin
		Result := SysUtils.SysErrorMessage(LastError);
	end;
end;

end.
