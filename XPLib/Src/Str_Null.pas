{$IFDEF Str_Null}
{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I XPLib.inc}
unit Str_Null;

{ 1 }
{ {
  Unit de manipulação de string em armazenamento PChar
}
interface

uses
  SysUtils, Windows, StrHnd;

function StrAllocString(Str: string): PChar;
	deprecated 'use System.AnsiStrings.AnsiStrAlloc() or SysUtils.StrAlloc() with StrPCopy';
function StrAllocAnsiString(Str: AnsiString): PAnsiChar;

// Converte o mapa de caracteres para o tipo especificado pelo sistema operacional
function StrConvertToOEM(Str: PChar): PChar;

implementation

uses
  AnsiStrings;

function StrAllocString(Str: string): PChar; deprecated;
// ----------------------------------------------------------------------------------------------------------------------
begin
  Result := StrAlloc(Length(Str) + 1);
  StrPCopy(Result, Str);
end;

function StrAllocAnsiString(Str: AnsiString): PAnsiChar;
begin
  Result := AnsiStrAlloc(Length(Str) + 1);
  AnsiStrings.StrPCopy(Result, Str);
end;

function StrConvertToOEM(Str: PChar): PChar;
{ {
  Aloca e converte o mapa de caracteres para o tipo especificado pelo sistema operacional
}
begin
  Result := StrNew(Str);
  CharToOEMA(PAnsiChar(Result), PAnsiChar(Result));
end;

end.
