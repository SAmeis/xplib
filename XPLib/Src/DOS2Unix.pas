{$IFDEF DOS2UNIX}
{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I XPLib.inc}

{$WARN UNSAFE_CODE OFF} {$WARN UNSAFE_TYPE OFF} {$WARN IMMUTABLE_STRINGS OFF}

{$WARN SYMBOL_DEPRECATED OFF} {$WARN UNIT_DEPRECATED OFF}
unit DOS2UNIX deprecated 'Sem utilidade prática agora';
{$WARN SYMBOL_DEPRECATED ON} {$WARN UNIT_DEPRECATED ON}

interface

uses Windows, Classes, SysUtils;

type
  TDos2Unix = class(TComponent)
  private
	FFileIn: string;
	FFileOut: string;
	FDirect: boolean;
	SameName: boolean;
	procedure SetFileIn(Value: string);
	procedure SetFileOut(Value: string);
  public
	constructor Create(AOwner: TComponent); override;
	function Execute: boolean; platform;
  published
	property FileIn: string read FFileIn write SetFileIn;
	property FileOut: string read FFileOut write SetFileOut;
	property Direct: boolean read FDirect write FDirect;
  end platform;

procedure Register; platform;

implementation

uses FileHnd;

type
  TBufSingle = array [1 .. 4096] of byte;
  TBufDouble = array [1 .. 8192] of byte;
  TBufIn     = ^TBufSingle;
  TBufOut    = ^TBufDouble;

const
  _CR = $0D;
  _LF = $0A;

constructor TDos2Unix.Create(AOwner: TComponent);
// ----------------------------------------------------------------------------------------------------------------------------------
begin
  inherited Create(AOwner);
  FFileIn := '';
  FFileOut := '';
  SameName := True;
end;

procedure TDos2Unix.SetFileIn(Value: string);
// ----------------------------------------------------------------------------------------------------------------------------------
begin
  if Value = FFileOut then begin
	SameName := True;
  end else begin
	SameName := False;
  end;
  FFileIn := Value;
end;

procedure TDos2Unix.SetFileOut(Value: string);
// ----------------------------------------------------------------------------------------------------------------------------------
begin
  if (Value = FFileIn) or (Value = EmptyStr) then begin
	SameName := True;
  end else begin
	SameName := False;
  end;
  FFileOut := Value;
end;

function TDos2Unix.Execute: boolean; platform;
// ----------------------------------------------------------------------------------------------------------------------------------
var
  TmpName, TmpPath: Pchar;
  TmpFile: string;
  BufIn: TBufIn;
  BufOut: TBufOut;
  AfterConv, BytesRead, BytesWrite, Count: integer;
  InF, OutF: file;
  OldMode: byte;
begin
  BufIn := nil;
  BufOut := nil;
  Result := False;
  TmpName := StrAlloc(255); // Arquivo temporario para conversao
  try
	if (FileIn = '') or (not(__FileExists(FileIn))) then begin
	  Exit;
	end;
	GetMem(BufIn, 4096); // Buffer de leitura
	GetMem(BufOut, 8132); // Pior caso apenas linhas vazias = *2
	TmpPath := StrAlloc(255);
	GetTempPath(255, TmpPath);
	GetTempFileName(TmpPath, 'DOSUNIX', 0, TmpName);
	StrDispose(TmpPath);
	TmpFile := StrPas(TmpName);
	AssignFile(InF, FFileIn);
	AssignFile(OutF, TmpFile);
	OldMode := FileMode;
	FileMode := OF_SHARE_EXCLUSIVE;
	Reset(InF, word(1));
	ReWrite(OutF, word(1));
	FileMode := OldMode;
	repeat
	begin
	  BlockRead(InF, BufIn^, 4096, BytesRead);
	  Count := 0; AfterConv := 0;
	  if FDirect then begin // Coversao DOS->UNIX
		repeat
		begin
		  Inc(Count);
		  if BufIn^[Count] <> _CR then begin
			Inc(AfterConv);
			BufOut^[AfterConv] := BufIn^[Count];
		  end;
		end;
		until Count >= BytesRead;
	  end else begin // Convesao UNIX->DOS
		repeat
		begin
		  Inc(Count);
		  if BufIn^[Count] = _LF then begin
			if BufIn^[Count + 1] <> _CR then begin
			  Inc(AfterConv);
			  BufOut^[AfterConv] := _CR;
			end;
		  end;
		  Inc(AfterConv);
		  BufOut^[AfterConv] := BufIn^[Count];
		end;
		until Count >= BytesRead;
	  end;
	  if (BytesRead > 0) then begin
		BlockWrite(OutF, BufOut^, AfterConv, BytesWrite);
	  end;
	end;
	until BytesRead = 0;
	CloseFile(InF);
	CloseFile(OutF);
	if SameName then begin
	  FileCopy(TmpFile, FileIn, True);
	end else begin
	  FileCopy(TmpFile, FileOut, True);
	end;
	Result := True;
  except
	if BufIn <> nil then begin
	  FreeMem(BufIn, 4096);
	end;
	if BufOut <> nil then begin
	  FreeMem(BufOut, 8132);
	end;
	if __FileExists(TmpFile) then begin
	  DeleteFile(TmpFile);
	end;
	StrDispose(TmpName);
  end
end;

procedure Register;
// ----------------------------------------------------------------------------------------------------------------------------------
begin
  RegisterComponents('Super', [TDos2Unix]);
end;

end.
