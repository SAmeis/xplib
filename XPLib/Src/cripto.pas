{$IFDEF cripto}
{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I XPLib.inc}
unit cripto;

interface

// Decodifica uma string usando apenas ASCII reduzido
function ASCIIDec(const Str: string): string;
// Codifica uma string usando apenas ASCII reduzido
function ASCIIEnc(const Str: string): string;
// Criptografa/Decriptografa um arquivo usando matriz de off-sets
function CriptoFileByOffSet(const FileIn, FileOut: string; cripto: boolean): boolean;
// Decodifica uma string usando apenas sequencias de numeros espacados por brancos
function CriptoSpaceDecode(Str: string): string;
// Codifica uma string usando apenas sequencias de numeros espacados c/ brancos
function CriptoSpaceEncode(const Str: string): string;

implementation

uses SysUtils, Str_Pas, Classes;

function CriptoSpaceEncode(const Str: string): string;
// ----------------------------------------------------------------------------------------------------------------------------------
// Codifica uma string usando apenas sequencias de numeros espacados c/ brancos
var
  l, i: integer;
  Key, car: Byte;
begin
  Result := EmptyStr;
  l := Length(Str);
  Key := Random(255);
  Result := Result + IntToStr(Byte(Key));
  for i := 1 to l do begin
	car := Byte(Str[i]);
	{$WARN UNSAFE_CODE OFF}
	asm
	  mov al,key
	  mov ah,car
	  xor al,ah
	  mov key,al
	end;
	{$WARN UNSAFE_CODE ON}
	Result := Result + ' ' + IntToStr(Byte(Key));
  end;
end;

function CriptoSpaceDecode(Str: string): string;
// ----------------------------------------------------------------------------------------------------------------------------------
// Decodifica uma string usando apenas sequencias de numeros espacados por brancos
const
  _SEP_ = ' '; // espaco em branco
var
  p: integer;
  Key, Prev: Byte;
  Item: string;
begin
  Result := EmptyStr;
  p := Pos(_SEP_, Str);
  if p = 0 then begin
	Exit;
  end;
  Item := GetDelimitedSubStr(_SEP_, Str, 0);
  try
	Key := StrToInt(Item);
  except
	on Exception do begin
	  Item := EmptyStr; // Assinala retorno da funcao
	  Key := 0;
	end;
  end;
  if Item = EmptyStr then begin
	Exit;
  end;
  Prev := Key;
  Delete(Str, 1, p);
  repeat
	Item := GetDelimitedSubStr(_SEP_, Str, 0);
	Delete(Str, 1, Length(Item) + 1);
	try
	  Key := StrToInt(Item);
	except
	  on Exception do begin
		Item := EmptyStr;
	  end;
	end;
	if Item <> EmptyStr then begin
	  Result := Result + Chr(Byte(Byte(Key) xor Byte(Prev)));
	end;
	Prev := Key;
  until Str = EmptyStr;
end;

{ ------------------------------------------------------------------------------ }
function ASCIIDec(const Str: string): string;
var
  CPos: integer;
  CMask, C: Byte;
begin
  Result := Str;
  {$WARN UNSAFE_CODE OFF} {$WARN IMMUTABLE_STRINGS OFF}
  CMask := Lo(Length(Str));
  for CPos := 1 to Length(Str) do begin
	C := Byte(Str[CPos]);
	C := (C - 32) mod CMask;
	Result[CPos] := Char(C);
	CMask := C;
  end;
  {$WARN UNSAFE_CODE ON} {$WARN IMMUTABLE_STRINGS ON}
end;

{ ------------------------------------------------------------------------------ }
function ASCIIEnc(const Str: string): string;
var
  CPos: integer;
  CMask, C: Byte;
begin
  Result := Str;
  {$WARN UNSAFE_CODE OFF} {$WARN IMMUTABLE_STRINGS OFF}
  CMask := Lo(Length(Str));
  for CPos := 1 to Length(Str) do begin
	C := Byte(Str[CPos]);
	C := (C mod CMask) + 32;
	Result[CPos] := Char(C);
	CMask := C;
  end;
  {$WARN UNSAFE_CODE ON} {$WARN IMMUTABLE_STRINGS ON}
end;

function CriptoFileByOffSet(const FileIn, FileOut: string; cripto: boolean): boolean;
{ ------------------------------------------------------------------------------ }
/// <summary>
/// Criptografa/Decriptografa um arquivo usando matriz de off-sets
/// </summary>
/// Modified by roger 24/09/2014 18:41:35 - Removido o modo arcaico de acesso(file-BlockRead) para o uso de TFileStream
const
  LOW_BUFFER                                              = 0;
  HIGH_BUFFER                                             = 79;
  AValChave: array [LOW_BUFFER .. HIGH_BUFFER] of integer = (7, 5, 3, 3, 5, 3, 7, 9, 5, 23, 11, 34, 3, 16, 15, 16, 13, 58, 19, 30,
	  21, 24, 33, 24, 22, 21, 77, 28, 79, 30, 91, 32, 43, 35, 65, 36, 77, 38, 35, 44, 41, 52, 13, 15, 16, 16, 37, 18, 32, 12, 11,
	  22, 13, 15, 18, 56, 17, 58, 16, 60, 21, 62, 23, 24, 35, 24, 37, 22, 39, 33, 41, 35, 63, 64, 37, 34, 47, 32, 29, 30);
  // ................................................................................................................................
var
  fDestino, fOrigem: TFileStream;
  iBytesLidos: integer;
  aCripto, aBuffer: TBytes;

  procedure Criptografa;
  var
	i: integer;
  begin
	if cripto then begin
	  for i := low(aCripto) to high(aCripto) do begin
		aCripto[i] := Byte(AnsiChar(Ord(aBuffer[i]) + AValChave[i]));
	  end;
	end else begin
	  for i := low(aCripto) to high(aCripto) do begin
		aCripto[i] := Byte(AnsiChar(Ord(aBuffer[i]) - AValChave[i]));
	  end;
	end;
  end;

// ................................................................................................................................
begin
  try
	SetLength(aBuffer, HIGH_BUFFER);
	SetLength(aCripto, HIGH_BUFFER);
	fOrigem := TFileStream.Create(FileIn, fmOpenRead); // abre origem
	try
	  if FileExists(FileOut) then begin // abre destino
		fDestino := TFileStream.Create(FileOut, fmOpenReadWrite + fmExclusive);
	  end else begin
		fDestino := TFileStream.Create(FileOut, fmCreate + fmExclusive);
	  end;
	  try
		iBytesLidos := fOrigem.ReadData(aBuffer, SizeOf(aBuffer));
		while ( iBytesLidos > 0 ) do begin
		  Criptografa;
		  fDestino.WriteData(aCripto, iBytesLidos);
		  iBytesLidos := fOrigem.ReadData(aBuffer, SizeOf(aBuffer));
		end;
		Result := True;
	  finally
		fDestino.Free;
	  end;
	finally
	  fOrigem.Free;
	end;
  except
	on E: Exception do begin
	  Result := False;
	end;
  end;
end;

end.
