{$IFDEF StreamHnd}
    {$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I StFLib.inc}

unit StreamHnd;

interface

uses
    Classes, SysUtils;

type
    TStreamHnd = Class
    public
        Class function ReadStream(SrcStream, DestStream : TStream; HowMany : int64) : int64;
    end;

implementation

Class function TStreamHnd.ReadStream(SrcStream, DestStream : TStream; HowMany : int64) : int64;
{{
HowMany (-1) -> Todo o restante do stream de origem sera repassado para o de destino
}
var
    Buffer : Pointer;
    BufSize : int64;
begin
    if (HowMany >= 0) then begin
        BufSize := HowMany;
    end else begin
        BufSize := SrcStream.Size - SrcStream.Position;
    end;
    GetMem(Buffer, BufSize);
    try
        Result := SrcStream.Read(Buffer^, BufSize);
        DestStream.Write(Buffer^, BufSize);
    finally
        FreeMem(Buffer);
    end;
end;

end.


