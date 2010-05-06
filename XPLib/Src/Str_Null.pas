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
    SysUtils, Windows;

function StrAllocString(Str : string) : PChar;

//Converte o mapa de caracteres para o tipo especificado pelo sistema operacional
function StrConvertToOEM(Str : PChar) : PChar;

implementation

function StrAllocString(Str : string) : PChar;
    //----------------------------------------------------------------------------------------------------------------------
begin
    Result := StrAlloc(Length(Str) + 1);
    StrPCopy(Result, Str);
end;

function StrConvertToOEM(Str : PChar) : PChar;
{{
Aloca e converte o mapa de caracteres para o tipo especificado pelo sistema operacional
}
begin
    Result := StrNew(Str);
    CharToOEM(Result, Result);
end;

end.


