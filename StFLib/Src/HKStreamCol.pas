{$IFDEF HKStreamCol}
{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I StFLib.inc}
{ TODO -oroger -cfuture : Adequação para UNICODE pendente }

unit HKStreamCol;

{
  ----------------------------------------------------------------
  THKStreams v1.7 by Harry Kakoulidis 01/2002
  prog@xarka.com
  http://www.xarka.com/prog/

  This is Freeware. Please copy HKStrm17.zip unchanged.
  If you find bugs, have options etc. Please send at my e-mail.

  The use of this component is at your own risk.
  I do not take any responsibility for any damages.

  ----------------------------------------------------------------
  Update v1.7

  * Bug in RemoveStream procedure fixed by
  David Quinn (david@eternia.net)

  * Compiles with D6

  Update v1.6

  * Compability problem with Delphi 3
  pointed out by Glenn (buddyboy@idcomm.com)

  * Wrong password event idea by Lai xiaolong (laixl@mei29.scgb.com)

  * Bug with empty streams pointed
  out by Simon Horup, Crystal Art Software (info@casdk.com)

  * Bug with corrupted compressed files pointed out by
  Tsahi Chitin (TUtils@poboxes.com)

  ----------------------------------------------------------------
}

interface

uses
  HKStreamRoutines, Windows, Messages, SysUtils, Classes, Graphics, Controls;

type
  TGoodbytes   = array [1 .. 8] of byte;
  ECorruptFile = class(Exception);

const
  EncryptedByte: array [FALSE .. TRUE] of byte  = (ord(' '), ord('*'));
  CompressedByte: array [FALSE .. TRUE] of byte = (ord(' '), ord('&'));
  Goodbytes: TGoodbytes                         = (1, 2, 3, 4, 5, 6, 7, 8);

type
  TOnAskForKey = function(sender: TObject): string of object;

  THKStreams = class(TComponent)
  private
	FCompressed, FEncrypted: Boolean;
	FKey: string;
	FOnAskForKey: TOnAskForKey;
	FOnCorrupt: TNotifyEvent;
	procedure WriteStr(S: string; Stream: TStream);
	function ReadStr(Stream: TStream): string;
	procedure LoadFromStreamNor(ms: TStream);
	procedure SaveToStreamNor(ms: TStream);
	function CheckGood(ms: TStream): Boolean;
	procedure FoundCorrupt;
  public
	StreamList: TStringList;
	constructor Create(AOWner: TComponent); override;
	destructor Destroy; override;
	procedure LoadFromFile(const Filename: string);
	procedure SaveToFile(const Filename: string);
	procedure AddStream(const ID: string; Source: TStream);
	procedure RemoveStream(const ID: string);
	procedure LoadFromStream(ms: TStream);
	procedure SaveToStream(ms: TStream);
	procedure GetStream(const ID: string; Dest: TStream);
	procedure ClearStreams;
  published
	property Compressed: Boolean read FCompressed write FCompressed;
	property Encrypted: Boolean read FEncrypted write FEncrypted;
	property Key: string read FKey write FKey;
	property OnAskForKey: TOnAskForKey read FOnAskForKey write FOnAskForKey;
	property OnCorrupt: TNotifyEvent read FOnCorrupt write FOnCorrupt;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Super', [THKStreams]);
end;

{ THKStreams }

procedure THKStreams.AddStream(const ID: string; Source: TStream);
var
  ms: TMemoryStream;
begin
  if (length(ID) > 0) and (assigned(Source)) then begin
	ms := TMemoryStream.Create;
	ms.CopyFrom(Source, 0);
	StreamList.AddObject(ID, ms);
  end;
end;

procedure THKStreams.ClearStreams;
var
  a: integer;
begin
  with StreamList do begin
	for a := 0 to count - 1 do begin
	  TMemoryStream(objects[a]).free;
	end;
	clear;
  end;
end;

constructor THKStreams.Create(AOWner: TComponent);
begin
  inherited Create(AOWner);
  FCompressed := TRUE;
  FEncrypted := FALSE;
  StreamList := TStringList.Create;
  StreamList.Sorted := TRUE;
end;

destructor THKStreams.Destroy;
begin
  ClearStreams;
  StreamList.free;
  inherited Destroy;
end;

procedure THKStreams.GetStream(const ID: string; Dest: TStream);
var
  i: integer;
begin
  if (length(ID) > 0) then begin
	i := StreamList.IndexOf(ID);
	if i >= 0 then begin
	  Dest.CopyFrom(TMemoryStream(StreamList.objects[i]), 0);
	end;
	Dest.Position := 0;
  end;
end;

procedure THKStreams.LoadFromFile(const Filename: string);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(Filename, fmOpenRead);
  try
	LoadFromStream(fs);
  finally
	fs.free;
  end;
end;

procedure THKStreams.FoundCorrupt;
begin
  if assigned(FOnCorrupt) then begin
	FOnCorrupt(Self);
  end;
  raise ECorruptFile.Create('File is corrupt.');
end;

function THKStreams.CheckGood(ms: TStream): Boolean;
var
  GoodTest: TGoodbytes;
  a: integer;
begin
  ms.Position := 0;
  ms.read(GoodTest, sizeof(TGoodbytes));
  Result := TRUE;
  for a := 1 to sizeof(TGoodbytes) do begin
	if Goodbytes[a] <> GoodTest[a] then begin
	  FoundCorrupt;
	  Result := FALSE;
	  exit;
	end;
  end;
end;

procedure THKStreams.LoadFromStream(ms: TStream);
var
  CMem, mem: TMemoryStream;
  e, c: byte;
  AKey: string;
begin
  AKey := FKey;
  CMem := TMemoryStream.Create;
  mem := TMemoryStream.Create;
  try
	ms.Position := 0;
	ms.read(c, sizeof(c));
	ms.read(e, sizeof(e));
	CMem.CopyFrom(ms, ms.size - 2);
	CMem.Position := 0;
	if (e = EncryptedByte[TRUE]) then begin
	  if not assigned(FOnAskForKey) then begin
		AKey := FKey;
	  end else begin
		AKey := FOnAskForKey(Self);
	  end;
	  try
		DecryptStream(CMem, AKey);
	  except
		on Exception do begin
		  FoundCorrupt;
		end;
	  end;
	end;
	if not(c = CompressedByte[TRUE]) then begin
	  if not CheckGood(CMem) then begin
		exit;
	  end;
	end;
	CMem.Position := 0;
	if (c = CompressedByte[TRUE]) then begin
	  try
		LHAExpand(CMem, mem)
	  except
		on Exception do begin
		  FoundCorrupt;
		end;
	  end;
	end else begin
	  mem.CopyFrom(CMem, 0);
	end;
	LoadFromStreamNor(mem);
  finally
	CMem.free;
	mem.free;
  end;
  FKey := AKey;
  FCompressed := (c = CompressedByte[TRUE]);
  FEncrypted := (e = EncryptedByte[TRUE]);
end;

procedure THKStreams.LoadFromStreamNor(ms: TStream);
var
  mem: TMemoryStream;
  count, size, a: integer;
  ID: string;
begin
  if not CheckGood(ms) then begin
	exit;
  end;
  ClearStreams;
  ms.Position := sizeof(TGoodbytes);
  ms.read(count, sizeof(count));
  for a := 0 to count - 1 do begin
	mem := TMemoryStream.Create;
	ID := ReadStr(ms);
	ms.read(size, sizeof(size));
	if size <> 0 then begin
	  mem.CopyFrom(ms, size);
	end;
	StreamList.AddObject(ID, mem);
  end;
end;

function THKStreams.ReadStr(Stream: TStream): string;
var
  i: word;
  S: string;
begin
  {$WARN UNSAFE_CODE OFF}
  Stream.read(i, sizeof(i));
  setlength(S, i);
  Stream.read(PChar(S)^, i);
  Result := S;
  {$WARN UNSAFE_CODE ON}
end;

procedure THKStreams.RemoveStream(const ID: string);
var
  i: integer;
begin
  if (length(ID) > 0) then begin
	i := StreamList.IndexOf(ID);
	if i >= 0 then begin { DTQ }
	  TMemoryStream(StreamList.objects[i]).free;
	  StreamList.Delete(i);
	end;
  end;
end;

procedure THKStreams.SaveToFile(const Filename: string);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(Filename, fmCreate);
  try
	SaveToStream(fs);
  finally
	fs.free;
  end;
end;

procedure THKStreams.SaveToStream(ms: TStream);
var
  mem, CMem: TMemoryStream;
  e, c: byte;
begin
  CMem := TMemoryStream.Create;
  mem := TMemoryStream.Create;
  try
	SaveToStreamNor(mem);
	mem.Position := 0;
	c := CompressedByte[FCompressed];
	e := EncryptedByte[FEncrypted and (FKey <> '')];
	ms.Write(c, sizeof(c));
	ms.Write(e, sizeof(e));
	if FCompressed then begin
	  LHACompress(mem, CMem);
	end else begin
	  CMem.CopyFrom(mem, 0);
	end;
	if (FEncrypted) and (FKey <> '') then begin
	  EncryptStream(CMem, FKey);
	end;
	ms.CopyFrom(CMem, 0);
  finally
	mem.free;
	CMem.free;
  end;
end;

procedure THKStreams.SaveToStreamNor(ms: TStream);
var
  count, size, a: integer;
begin
  ms.Write(Goodbytes, sizeof(TGoodbytes));
  count := StreamList.count;
  ms.Write(count, sizeof(count));
  for a := 0 to count - 1 do begin
	WriteStr(StreamList.strings[a], ms);
	size := TMemoryStream(StreamList.objects[a]).size;
	ms.Write(size, sizeof(size));
	ms.CopyFrom(TMemoryStream(StreamList.objects[a]), 0);
  end;
end;

procedure THKStreams.WriteStr(S: string; Stream: TStream);
var
  i: word;
begin
  i := length(S);
  Stream.Write(i, sizeof(i));
  {$WARN UNSAFE_CODE OFF}
  Stream.Write(PChar(S)^, i);
  {$WARN UNSAFE_CODE ON}
end;

end.
