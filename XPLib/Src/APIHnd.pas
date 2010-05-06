{$IFDEF APIHnd}
	{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I XPLib.inc}

unit APIHnd;

interface

uses
	Windows, SysUtils, SysConst;

function CheckAPI( LastError : integer ) : boolean; deprecated;
{{
Monta excessao de acordo com o codigo de erro informado pelo sistema operacional/API.
Esta rotina espera por implementacao de codigos extendidos desta biblioteca

Depreciada em favor para o mesmo metodo de classe em TAPIHnd.

Revision: 18/7/2005
}

function GetEnvironmentVar( const EnvVar : string ) : string; deprecated;
{{
Retorna a string com o valor da variavel e ambiente
Depreciada em favor para o mesmo metodo de classe em TAPIHnd.

Revision: 18/7/2005
}

type
    TAPIHnd = class(TObject)
    public
        class function CheckAPI( LastError : integer ) : boolean;
        class function GetEnvironmentVar( const EnvVar : string ) : string;
    end;


implementation

function CheckAPI( LastError : integer ) : boolean;
//----------------------------------------------------------------------------------------------------------------------
//Monta excessao de acordo com o codigo de erro informado pelo sistema operacional/API.
//Esta rotina espera por implementacao de codigos extendidos desta biblioteca
var
	Error: EOSError;
begin
	if LastError <> 0 then begin
		Error := SysUtils.EOSError.CreateResFmt(	PResStringRec(@SysConst.SOSError),
													[LastError, SysUtils.SysErrorMessage(LastError)] );
		Error.ErrorCode := LastError;
		raise Error;
	end else begin
		Result:=True;
	end;
end;

function GetEnvironmentVar( const EnvVar : string ) : string; deprecated;
{{
Retorna a string com o valor da variavel e ambiente
Depreciada em favor para o mesmo metodo de classe em TAPIHnd.

Revision: 18/7/2005
}
var
	Match, pEnv, Pt : PChar;
begin
	{$IFDEF WIN32}
	pEnv:=GetEnvironmentStrings;
	{$ELSE}
	pEnv:=GetDosEnvironment;
	{$ENDIF}
	Match:=StrAlloc(128);
	try
		StrCopy(Match, PChar( EnvVar ));
		StrCat(Match,'=');
		Match:=StrUpper(Match);
		repeat
			if StrLen(pEnv) = 0 then begin
				Result:=EmptyStr;
				Exit;
			end;
			Pt:=StrPos(StrUpper(pEnv), Match); //Localiza cadeia "var="
			if Pt = nil then begin
				pEnv:=StrEnd(pEnv);
				Inc(pEnv);
			end;
		until Pt <> nil;
		Pt:=StrPos(StrUpper(pEnv), Match);
		Inc(Pt,StrLen(Match));
		Result:=StrPas(Pt);
	finally
		StrDispose(Match);
	end;
end;

class function TAPIHnd.CheckAPI(LastError: integer): boolean;
{{
Monta excessao de acordo com o codigo de erro informado pelo sistema operacional/API.
Esta rotina espera por implementacao de codigos extendidos desta biblioteca

Revision: 18/7/2005
}
var
	Error: EOSError;
begin
	if LastError <> 0 then begin
		Error := SysUtils.EOSError.CreateResFmt(	PResStringRec(@SysConst.SOSError),
													[LastError, SysUtils.SysErrorMessage(LastError)] );
		Error.ErrorCode := LastError;
		raise Error;
	end else begin
		Result:=True;
	end;
end;

class function TAPIHnd.GetEnvironmentVar(const EnvVar: string): string;
{{
Retorna a string com o valor da variavel de ambiente

Revision: 18/7/2005
}
var
	Match, pEnv, Pt : PChar;
begin
	{$IFDEF WIN32}
	pEnv:=GetEnvironmentStrings;
	{$ELSE}
	pEnv:=GetDosEnvironment;
	{$ENDIF}
	Match:=StrAlloc(128);
	try
		StrCopy(Match, PChar( EnvVar ));
		StrCat(Match,'=');
		Match:=StrUpper(Match);
		repeat
			if StrLen(pEnv) = 0 then begin
				Result:=EmptyStr;
				Exit;
			end;
			Pt:=StrPos(StrUpper(pEnv), Match); //Localiza cadeia "var="
			if Pt = nil then begin
				pEnv:=StrEnd(pEnv);
				Inc(pEnv);
			end;
		until Pt <> nil;
		Pt:=StrPos(StrUpper(pEnv), Match);
		Inc(Pt,StrLen(Match));
		Result:=StrPas(Pt);
	finally
		StrDispose(Match);
	end;
end;

end.
