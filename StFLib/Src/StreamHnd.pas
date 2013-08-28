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

	 THashHnd = class
	 public
		class function MD5(const fileName : string) : string; overload;
		class function MD5(const strm : TStream) : string; overload;
	 end;


implementation

uses IdHashMessageDigest, idHash;

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

{ TRedundancyCheck }

class function THashHnd.MD5(const fileName : string) : string;
var
	 fs : TFileStream;
begin
	 fs := TFileStream.Create(fileName, fmOpenRead or fmShareDenyWrite);
	 try
		 Result := MD5(fs);
	 finally
		 fs.Free;
	 end;
end;

class function THashHnd.MD5(const strm : TStream) : string;
var
	 idmd5 : TIdHashMessageDigest5;
begin
	 strm.Seek( 0, soBeginning );
	 idmd5 := TIdHashMessageDigest5.Create;
	 try
		 Result := idmd5.HashStreamAsHex(strm);
	 finally
		 idmd5.Free;
	 end;
end;


end.


