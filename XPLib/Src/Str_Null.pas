{$IFDEF Str_Null}
    {$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I XPLib.inc}

unit Str_Null;

{1  }
{{
Unit de manipulação de string em armazenamento PChar
}
interface

uses
    SysUtils, Windows, StrHnd;

function StrAllocString(Str : string) : PChar; deprecated;
function StrAllocAnsiString(Str : AnsiString) : PAnsiChar;

//Converte o mapa de caracteres para o tipo especificado pelo sistema operacional
function StrConvertToOEM(Str : PChar) : PChar;

implementation

function StrAllocString(Str : string) : PChar; deprecated;
    //----------------------------------------------------------------------------------------------------------------------
begin
    Result := StrAlloc(Length(Str) + 1);
    StrPCopy(Result, Str);
end;

function StrAllocAnsiString(Str : AnsiString) : PAnsiChar;
begin
   {$IF CompilerVersion >= 21.00}
   Result := AnsiStrAlloc(Length(Str) + 1);
   {$ELSE}
   Result := StrHnd.AnsiStrAlloc(Length(Str) + 1);
   {$IFEND}
    StrPCopy(Result, Str);
end;
function StrConvertToOEM(Str : PChar) : PChar;
{{
Aloca e converte o mapa de caracteres para o tipo especificado pelo sistema operacional
}
begin
    Result := StrNew(Str);
    {$IF CompilerVersion >= 21.00}
    CharToOEMA(PAnsiChar(Result), PAnsiChar(Result));
    {$ELSE}
    CharToOEM(Result, Result);
	 {$IFEND}
end;

end.


