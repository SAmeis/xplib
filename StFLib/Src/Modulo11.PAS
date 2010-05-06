{$IFDEF Modulo11}
    {$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I StFLib.inc}

unit Modulo11;

interface

uses
    Windows, Messages, SysUtils, Classes;

function Mod11(sValor : string) : Integer;

implementation

function Mod11(sValor : string) : Integer;
    //----------------------------------------------------------------------------------------------------------------------------------
var
    n, iLen, nAcumulado, nResto : integer;
begin
    nAcumulado := 0;
    sValor := Trim(sValor);
    iLen := Length(sValor) + 1;
    for n := iLen downto 2 do begin
        nAcumulado := nAcumulado + (n * StrToInt(Copy(sValor, 1, 1)));
        Delete(sValor, 1, 1);
    end;
    nResto := nAcumulado mod 11;
    if nResto < 2 then begin
        Result := 0;
    end else begin
        Result := Trunc(11 - nResto);
    end;
end;

end.


