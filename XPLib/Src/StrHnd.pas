{$IFDEF StrHnd }
{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I XPLib.inc}


unit StrHnd;

/// <summary>
///
/// </summary>
///  Modified by roger 28/09/2014 18:04:42 - removidos os simbolos abaixo por serem usados apenas para compiladores antigos
///  function AnsiStrAlloc(Size: cardinal): PAnsiChar;
///  function CharInSet(C: AnsiChar; const CharSet: TSysCharSet): boolean;
///
///

interface

uses
  Classes, SysUtils, XPTypes;

const
  DOS_EOL  = #13#10;
  UNIX_EOL = #13;

const
  DEFAULT_WORD_DELIMITERS_LIST: TSysCharSet = [#0, ' ', #13, #10, #9]; // Caracteres que podem preceder/anteceder um palavra;
  { TODO -oRoger -cLIB : procurar referencias a VISUAL_CHARS_LIST e detectar problemas, pois pelo nome VISUAL_CHARS_LIST deveria ser [ #32..#126] e mais alguma coisa }
  VISUAL_CHARS_LIST: TSysCharSet = ['\', '$', '%', '&', '@', '*', '+', '/', 'A' .. 'Z', 'a' .. 'z', '{', '}', '(', ')', '>', '<',
	  '?', '0' .. '9', '[', ']'];
  INTEGER_CHARS_LIST: TSysCharSet              = ['0' .. '9', '-', '+'];
  FLOAT_CHARS_LIST: TSysCharSet                = ['0' .. '9', '-', '+'] + [',', '.', 'E', 'e'];
  NOT_ALPHA_LOWER_CASE_CHARS_LIST: TSysCharSet = [#0 .. #255] - [#9, #32, 'a' .. 'z', 'á', 'é', 'í', 'ó', 'ú', 'à', 'ã', 'õ', 'â',
	  'ê', 'ô', 'ç', 'ü'];

type
  PSysCharSet = ^TSysCharSet;

type
  TStrHnd = class(TObject)
  public
	class function ASCIISearchString(const NormalSrcString: string): string;
	class function CopyAfterLast(const SubStr, Str: string): string;
	class function CopyAfterFirst(const SubStr, Str: string): string;
	class function endsWith(const fullStr: string; endChar: char): boolean; overload;
	{ {
	  Indica se a string termina com o dado caracter
	}
	class function endsWith(const fullStr, endStr: string): boolean; overload;
	class function EnsurePrefix(const Prefix, Str: string): string;
	class function FilterDigits(const Str: string): string;
	class function GetInteger(const Str: string; Index, ErrorValue: Integer): Integer;
	class function GetAlphaText(const Str: string; Index: Integer; ErrorValue: string): string;
	class function IsRangeValue(Value: string; MinValue, MaxValue: Extended): boolean;
	class procedure ResetLength(var S: WideString); overload;
	class procedure ResetLength(var S: AnsiString); overload;
	class procedure ResetLength(var S: UnicodeString); overload;
	class function startsWith(const Str, Prefix: string): boolean;
	class function StrPosChar(const S: string; const CharSet: TSysCharSet; StartPos: cardinal = 0): Integer;
	class function StrToPChar(Str: string): PChar;
	class function IsPertinent(const Value: string; const Values: array of string; CaseSensitive: boolean): boolean;
	class function Contains(const SubStr, SuperStr: string): boolean;
  end;

  TXPStringList = class(TStringList)
  public
	function FindPos(const SubStr: string; var Line, Col: Integer): boolean;
	function FindPosIgnoreCase(const SubStr: string; var Line, Col: Integer): boolean;
  end;

  TStringConnector = class(TObject)
	{ {
	  Classe usada para realizar concatenações entre elementos diversos no formato de strings, por exemplo na geração de listas do tipo:
	  "( elemento1, elemento2, ..., elementon )"
	}
  private
	FAppend: string;
	FFinalConnector: string;
	FInitialConnector: string;
	FIntermediateConnector: string;
	FPrepend: string;
  public
	constructor Create; overload; virtual;
	constructor Create(const AInitialConnector, AIntermediateConnector, AFinalConnector: string); overload; virtual;
	constructor Create(const AInitialConnector, AIntermediateConnector, AFinalConnector, APrepend, AAppend: string);
		overload; virtual;
	function Connect(List: TStrings): string;
	class function MakeSetString(Lst: TStrings): string;
	class function MakeSetStringOfNames(Lst: TStrings): string;
	class function MakeSetStringOfValues(Lst: TStrings): string;
	property Append: string read FAppend write FAppend;
	{ {
	  Valor a ser adcionado ao final da cadeia de retorno
	}
	property FinalConnector: string read FFinalConnector write FFinalConnector;
	{ {
	  Valor a ser adcionado ao entre o último e penúltimo elementos da cadeia de retorno
	}
	property InitialConnector: string read FInitialConnector write FInitialConnector;
	{ {
	  Valor a ser adcionado ao entre o primeiro e o segundo e elementos da cadeia de retorno
	}
	property IntermediateConnector: string read FIntermediateConnector write FIntermediateConnector;
	{ {
	  Valor a ser adcionado ao entre o segundo e os enesimos elementos da cadeia de retorno
	}
	property Prepend: string read FPrepend write FPrepend;
	{ {
	  Valor a ser adcionado ao inicio da cadeia de retorno
	}
  end;

  TBufferedStringStream = class(TObject)
  private
	FBuffer: PAnsiChar;
	FBufferSize: Integer;
	FEol: AnsiString;
	FMaxBufferSize: Integer;
	FOffSet: Integer;
	FStream: TStream;
	FWordDelimiters: PSysCharSet;
	function GetEoS: boolean;
	function GetPosition: int64;
	function GetSize: int64;
  protected
	function FillBuffer(): Integer;
  public
	constructor Create(AStream: TStream; BufferSize: Integer = 1024); virtual;
	destructor Destroy; override;
	function ReadChar: AnsiChar;
	function ReadLine: AnsiString;
	function ReadString(Len: Word): AnsiString;
	function ReadStringWord: AnsiString;
	procedure Reset;
	procedure Seek(pos: int64);
	function Search(const Str: AnsiString): boolean; overload;
	function Search(const Str: AnsiString; SkipStr: boolean): boolean; overload;
	procedure SetWordDelimiters(WordDelimiters: PSysCharSet);
	property Eol: AnsiString read FEol write FEol;
	property EoS: boolean read GetEoS;
	property Position: int64 read GetPosition;
	property Size: int64 read GetSize;
  end;

implementation

uses
  Math, Str_Pas, StrUtils, AnsiStrings;

{ -**********************************************************************
  ************************************************************************
  ******************
  ******************  Class:    TStrHnd
  ******************  Category: No category
  ******************
  ************************************************************************
  ************************************************************************ }
{ -------------------------------------------------------------------------------------------------------------------------------- }
class function TStrHnd.ASCIISearchString(const NormalSrcString: string): string;
{ {
  Transforma um text com recursos de acentuacao em um texto totalmente maiusculo e sem acentos
}
var
  i: Integer;
  ret: TStringBuilder;
begin
  {$MESSAGE 'Remover este alerta após sucesso de teste das alterações para uso do StringBuilder' }
  ret := TStringBuilder.Create;
  try
	ret.Append(AnsiUpperCase(NormalSrcString));
	for i := 1 to ret.Length do begin
	  if Ord(ret.Chars[i]) > 127 then begin // Sera necessario corrigir
		case ret.Chars[i] of
		  'Ç': begin
			  ret.Chars[i] := 'C';
			end;
		  'Á', 'À', 'Ã', 'Â', 'Ä': begin
			  ret.Chars[i] := 'A';
			end;
		  'É', 'È', 'Ê', 'Ë': begin
			  ret.Chars[i] := 'E';
			end;
		  'Í', 'Ì', 'Î', 'Ï': begin
			  ret.Chars[i] := 'I';
			end;
		  'Ó', 'Ò', 'Õ', 'Ô', 'Ö': begin
			  ret.Chars[i] := 'O';
			end;
		  'Ú', 'Ù', 'Û', 'Ü': begin
			  ret.Chars[i] := 'U';
			end;
		  'º': begin
			  ret.Chars[i] := 'O';
			end;
		  'ª': begin
			  ret.Chars[i] := 'A';
			end;
		else begin
			ret.Chars[i] := ' ';
		  end;
		end;
	  end;
	end;
  finally
	Result := ret.ToString;
	ret.Free;
  end;
end;

{ -------------------------------------------------------------------------------------------------------------------------------- }
class function TStrHnd.endsWith(const fullStr: string; endChar: char): boolean;
{ 1 Indica se a string termina com o dado caracter }
{ {
  Indica se a string termina com o dado caracter;

  fullStr : String completa.

  endChar : Caracter a ser localizado no final da cadeia.

  Returns : Verdadeiro se a fullStr termina com o dado caracter.
}
var
  p: PChar;
begin
  p := StrRScan(PChar(fullStr), endChar);
  if (p <> nil) then begin
	Result := (StrLen(p) = 1);
  end else begin
	Result := False;
  end;
end;

/// <summary>
/// Retorna valor lógico indicando se SubStr está contida em SuperStr
/// </summary>
/// <param name="SubStr">Cadeia a ser procurada</param>
/// <param name="SuperStr">Cadeia onde se procura</param>
/// <returns></returns>
class function TStrHnd.Contains(const SubStr, SuperStr: string): boolean;
begin
  Result := (pos(SubStr, SuperStr) <> 0);
end;

class function TStrHnd.CopyAfterFirst(const SubStr, Str: string): string;
/// <summary>
/// This functions scans s for SubStr from the left and returns the portion after SubStr(Case Sensitive).
/// Example: AfterRev('.','c:\my.file.txt') > 'file.txt'
/// Example: AfterRev('T','c:\my.file.txt') > ''
/// </summary>
var
  p: Integer;
begin
  p := pos(SubStr, Str);
  if (p > 0) then begin
	Result := Copy(Str, Length(SubStr) + 1, Length(Str));
  end else begin
	Result := EmptyStr;
  end;
end;

class function TStrHnd.CopyAfterLast(const SubStr, Str: string): string;
/// <summary>
/// This functions scans s for SubStr from the right and returns the portion after SubStr.
/// Example: AfterRev('.','c:\my.file.txt') > 'txt'
/// </summary>
begin
  Result := StrRScanStr(PChar(SubStr), PChar(Str));
  if (Result <> EmptyStr) then begin
	Result := System.Copy(Result, Length(SubStr) + 1, Length(Result));
  end;
end;

class function TStrHnd.endsWith(const fullStr, endStr: string): boolean;
{ {
  Indica se a string termina com string passada;

  fullStr : String completa.

  endStr : Cadeia a ser localizada no final da string fullStr.

  Returns : Verdadeiro se a fullStr termina com a dada cadeia.
}
var
  p: PChar;
begin
  p := StrRScanStr(PChar(endStr), PChar(fullStr));
  if (p <> nil) then begin
	Result := (StrLen(p) = StrLen(PChar(endStr)));
  end else begin
	Result := False;
  end;
end;

{ -------------------------------------------------------------------------------------------------------------------------------- }
class function TStrHnd.EnsurePrefix(const Prefix, Str: string): string;
{ {
  retorna string onde o inicio é garantido como prefixado por Prefix.

  Revision: 10/8/2005 - Roger
}
begin
  if (pos(Prefix, Str) = 1) then begin // deve estar no inicio da cadeia
	Result := Str;
  end else begin
	Result := Prefix + Str;
  end;
end;

{ -------------------------------------------------------------------------------------------------------------------------------- }
class function TStrHnd.FilterDigits(const Str: string): string;
{ {
  remove todos os caracteres fora do conjunto de digitos decimais( 0..9 )

  Str : String de origem

  returns: Cadeia com apenas os digitos filtrados.
}
var
  i: Integer;
begin
  SetLength(Result, Length(Str));
  Result := EmptyStr;
  for i := 1 to Length(Str) do begin
	if CharInSet(Str[i], ['0' .. '9']) then begin
	  Result := Result + Str[i];
	end;
  end;
end;

class function TStrHnd.GetAlphaText(const Str: string; Index: Integer; ErrorValue: string): string;
// Retorna cadeia a partir da posição Index até o final da Cadeia original ou até encontrar caracter não alfabético
var
  x, ML: Integer;
begin
  Result := EmptyStr;
  ML := Length(Str);
  x := index;
  while ((x <= ML) and ((CharInSet(Str[x], ['a' .. 'z']) or CharInSet(Str[x], ['A' .. 'Z'])))) do begin
	Result := Result + Str[x];
	Inc(x);
  end;
end;

class function TStrHnd.GetInteger(const Str: string; Index, ErrorValue: Integer): Integer;
var
  x, ML: Integer;
  chain: string;
begin
  ML := Length(Str);
  x := index;
  while ((x <= ML) and CharInSet(Str[x], ['0' .. '9'])) do begin
	chain := chain + Str[x];
	Inc(x);
  end;
  Result := StrToInt(chain);
end;

{ -------------------------------------------------------------------------------------------------------------------------------- }
class function TStrHnd.IsPertinent(const Value: string; const Values: array of string; CaseSensitive: boolean): boolean;
{ {
  Verifica se Value pertence a lista de valores passada, considerando ou não a caixa.

  Revision: 29/11/2006 - Roger
}
var
  CmpValue: string;
  i: Integer;
begin
  Result := False;
  if (CaseSensitive) then begin
	CmpValue := Value;
	for i := low(Values) to high(Values) do begin
	  if (CmpValue = Values[i]) then begin
		Result := True;
		Exit;
	  end;
	end;
  end else begin
	CmpValue := UpperCase(Value);
	for i := low(Values) to high(Values) do begin
	  if (CmpValue = UpperCase(Values[i])) then begin
		Result := True;
		Exit;
	  end;
	end;
  end;
end;

class function TStrHnd.IsRangeValue(Value: string; MinValue, MaxValue: Extended): boolean;
{ {
  Verifica se o valor passado como string se encontra dentro do intervalo dado.

  Value : string contendo um valor de ponto flutuante valido.

  MinValue : Inicio do intervalo

  MaxValue : Inicio do intervalo

  returns: Resultado da comparacao.

  Notas: se MaxValue < MinValue sempre teremos resultado falso.
}
var
  ConvertValue: Extended;
begin
  if (TryStrToFloat(Value, ConvertValue)) then begin
	Result := (ConvertValue >= MinValue) and (ConvertValue <= MaxValue);
  end else begin
	Result := False;
  end;
end;

{ -------------------------------------------------------------------------------------------------------------------------------- }
class function TStrHnd.startsWith(const Str, Prefix: string): boolean;
{ {
  str : String a ser analisada

  prefix : String que se deseja sabe se a outra  é iniciada por ela.

  return : Boolean indicando a condicao de que a string é iniciada com o prefix passado

  NOTES: Todos os elementos são case-sensitive.
}
begin
  Result := pos(Prefix, Str) = 1;
end;

{ -------------------------------------------------------------------------------------------------------------------------------- }
class function TStrHnd.StrPosChar(const S: string; const CharSet: TSysCharSet; StartPos: cardinal = 0): Integer;
{ {
  Localiza primeira ocorrencia de um dos caracteres presentes na lista
  returns the position of the first character found in CharSet.
  Note that StartPos 1 and 0 are functionally the same.
  Example:
  Pos(['a'..'z'],'the man there') > 1
  Pos(['a'..'z'],'THE man there') > 5

  Revision: 27/7/2005 - Roger
}
var
  i: Integer;
begin
  Result := 0;
  if StartPos = 0 then begin
	StartPos := 1;
  end;
  for i := StartPos to Length(S) do begin
	if CharInSet(S[i], CharSet) then begin
	  Result := i;
	  Break;
	end;
  end;
end;

class procedure TStrHnd.ResetLength(var S: WideString) overload;
var
  i: Integer;
begin
  for i := 0 to Length(S) - 1 do begin
	if S[i + 1] = #0 then begin
	  SetLength(S, i);
	  Exit;
	end;
  end;
end;

class procedure TStrHnd.ResetLength(var S: AnsiString) overload;
var
  i: Integer;
begin
  for i := 0 to Length(S) - 1 do begin
	if S[i + 1] = #0 then begin
	  SetLength(S, i);
	  Exit;
	end;
  end;
end;

class procedure TStrHnd.ResetLength(var S: UnicodeString) overload;
var
  i: Integer;
begin
  for i := 0 to Length(S) - 1 do begin
	if S[i + 1] = #0 then begin
	  SetLength(S, i);
	  Exit;
	end;
  end;
end;

{ -------------------------------------------------------------------------------------------------------------------------------- }
class function TStrHnd.StrToPChar(Str: string): PChar;
{ {
  Aloca e copia a string para um PChar
}
var
  Size: Integer;
begin
  Size := Length(Str) + SizeOf(cardinal) + 1;
  Result := StrAlloc(Size);
  StrPCopy(Result, Str);
end;

{ -**********************************************************************
  ************************************************************************
  ******************
  ******************  Class:    TBufferedStringStream
  ******************  Category: No category
  ******************
  ************************************************************************
  ************************************************************************ }
{ -------------------------------------------------------------------------------------------------------------------------------- }
function TBufferedStringStream.GetEoS: boolean;
{ {
  Verdadeiro se nada mais a ser lido do stream e do buffer
}
begin
  Result := (Self.FOffSet >= Self.FBufferSize) and (Self.FStream.Position >= Self.FStream.Size);
end;

{ -------------------------------------------------------------------------------------------------------------------------------- }
function TBufferedStringStream.GetPosition: int64;
{ {
  Retorna a posicao corrente em relacao ao todo
}
begin
  Result := Self.FStream.Position - Self.FBufferSize + Self.FOffSet;
end;

{ -------------------------------------------------------------------------------------------------------------------------------- }
function TBufferedStringStream.GetSize: int64;
{ {
  Retorna o tamanho total do stream( dados )
}
begin
  Result := Self.FStream.Size;
end;

{ -------------------------------------------------------------------------------------------------------------------------------- }
function TBufferedStringStream.FillBuffer: Integer;
{ {
  Preenche o buffer totalmente com o conteudo lido do stream
}
var
  dataSize, freeSize: Integer;
  PUnreadData: PAnsiChar;
begin
  // Deslocamento dos dados naum utilizados
  if (Self.FOffSet > 0) then begin // necessita deslocamento
	PUnreadData := (Self.FBuffer + Self.FOffSet);
	dataSize := (Self.FBufferSize - Self.FOffSet);
	Move(PUnreadData[0], Self.FBuffer[0], dataSize);
	Self.FOffSet := 0;
	Self.FBufferSize := dataSize;
  end;

  // Carrega o complemento do buffer
  if (Self.FBufferSize < Self.FMaxBufferSize) then begin
	freeSize := (Self.FMaxBufferSize - Self.FBufferSize);
	PUnreadData := (Self.FBuffer + Self.FBufferSize);
	Result := Self.FStream.Read(PUnreadData[0], freeSize);
	Inc(Self.FBufferSize, Result);
	Inc(PUnreadData, Result); // Sempre delimita o fim dos dados
	{$WARN UNSAFE_CODE OFF}
	PUnreadData^ := #0;
	{$WARN UNSAFE_CODE ON}
  end else begin
	Result := 0;
  end;
end;

{ -------------------------------------------------------------------------------------------------------------------------------- }
constructor TBufferedStringStream.Create(AStream: TStream; BufferSize: Integer = 1024);
{ {
  Reserva o Stream passado para leitura posterior e aloca o buffer com o tamanho passado
}
begin
  inherited Create;
  {$WARN UNSAFE_CODE OFF}
  Self.FWordDelimiters := @DEFAULT_WORD_DELIMITERS_LIST;
  {$WARN UNSAFE_CODE ON}
  FEol := DOS_EOL;
  // Vincula Stream para leituras posteriores
  Self.FStream := AStream;
  // Reserva um buffer com o tamanho maximo passado
  Self.FMaxBufferSize := BufferSize;
  Self.FBufferSize := 0;
  Self.FOffSet := 0;
  Self.FBuffer := GetMemory(BufferSize + 1);
  AnsiStrings.StrPCopy(Self.FBuffer, '');
end;

{ -------------------------------------------------------------------------------------------------------------------------------- }
destructor TBufferedStringStream.Destroy;
{ {
  Destroi a instancia e libera o buffer interno
}
begin
  FreeMemory(Self.FBuffer);
  inherited;
end;

{ -------------------------------------------------------------------------------------------------------------------------------- }
function TBufferedStringStream.ReadChar: AnsiChar;
{ {
  Retorna o caracter da posicao corrente e incrementa o cursor
}
begin
  {$WARN UNSAFE_CODE OFF}
  if ((Self.FOffSet >= Self.FBufferSize) and (not Self.EoS)) then begin
	Self.FillBuffer;
  end;
  Result := (Self.FBuffer + Self.FOffSet)^;
  Inc(Self.FOffSet);
  {$WARN UNSAFE_CODE ON}
end;

{ -------------------------------------------------------------------------------------------------------------------------------- }
function TBufferedStringStream.ReadStringWord: AnsiString;
{ {
  Retorna a palavra imediatamente seguinte ao ponteiro interno.

  AINDA A SER IMPLEMENTADO!!!
}
var
  C: AnsiChar;
begin
  {$WARN UNSAFE_CODE OFF} {$WARN EXPLICIT_STRING_CAST_LOSS OFF}
  Result := AnsiString(EmptyStr);
  repeat
	C := ReadChar;
	Result := Result + C;
  until C in Self.FWordDelimiters^;
  Delete(Result, Length(Result), 1);
  {$WARN UNSAFE_CODE ON} {$WARN EXPLICIT_STRING_CAST_LOSS ON}
end;

{ -------------------------------------------------------------------------------------------------------------------------------- }
procedure TBufferedStringStream.Reset;
{ {
  Posiciona o Streamer no seu inicio e recarrega o buffer interno. Deve ser chamado antes de qualquer operacao de leitura.
  Ou opcionalmente um seek para alguma posicao desejada do streamer.
}
begin
  Self.Seek(0);
end;

{ -------------------------------------------------------------------------------------------------------------------------------- }
procedure TBufferedStringStream.Seek(pos: int64);
{ {
  Posiciona o cursor do stream interno na posicao desejada e recarrega todo o buffer interno.
}
begin
  Self.FStream.Position := pos;
  Self.FOffSet := 0;
  Self.FBufferSize := 0;
  Self.FillBuffer();
end;

{ -------------------------------------------------------------------------------------------------------------------------------- }
procedure TBufferedStringStream.SetWordDelimiters(WordDelimiters: PSysCharSet);
{ {
  Ajusta a lista de caracteres validos como separadores de palavras.

  Por padrão o construtor seta a constante DEFAULT_WORD_DELIMITERS_LIST
}
begin
  Self.FWordDelimiters := WordDelimiters;
end;

{ TStringConnector }

{ -**********************************************************************
  ************************************************************************
  ******************
  ******************  Class:    TStringConnector
  ******************  Category: No category
  ******************
  ************************************************************************
  ************************************************************************ }
{ -------------------------------------------------------------------------------------------------------------------------------- }
function TStringConnector.Connect(List: TStrings): string;
{ {
  Une os elementos de uma string list usando os conectores ajustados e adcionando as cadeias dadas por Prepend e Append.
}
var
  C: Integer;
begin
  Result := EmptyStr;
  C := List.Count;
  case C of
	0: begin
		// nada a retornar
	  end;
	1: begin
		Result := List.Strings[0];
	  end;
	2: begin
		Result := List.Strings[0] + Self.FFinalConnector + List.Strings[1];
	  end;
  else begin // 3 ou mais
	  Result := List.Strings[0] + Self.FInitialConnector;
	  for C := 1 to List.Count - 2 do begin
		Result := Result + List.Strings[C] + Self.FIntermediateConnector;
	  end;
	  Result := Copy(Result, 1, Length(Result) - Length(Self.FIntermediateConnector)) + Self.FFinalConnector +
		  List.Strings[List.Count - 1];
	end;
  end;
  Result := Self.FPrepend + Result + Self.FAppend;
end;

{ -------------------------------------------------------------------------------------------------------------------------------- }
constructor TStringConnector.Create;
{ {
  Inicia o conector de string com o conectores padrao para um frase na lingua portuguesa
  gerando algo do tipo "n1, n2, n3, ..., nn-1 e nn".

  Equivale a chamar Create(', ', ', ', ' e ');

  Revision: 13/10/2005 - Roger
}
begin
  Create(', ', ', ', ' e ');
end;

{ -------------------------------------------------------------------------------------------------------------------------------- }
constructor TStringConnector.Create(const AInitialConnector, AIntermediateConnector, AFinalConnector: string);
{ {
  Instancia passando os valores a serem usados como conector inicial, final e intermediário.
}
begin
  Create(AInitialConnector, AIntermediateConnector, AFinalConnector, EmptyStr, EmptyStr);
end;

{ -------------------------------------------------------------------------------------------------------------------------------- }
constructor TStringConnector.Create(const AInitialConnector, AIntermediateConnector, AFinalConnector, APrepend, AAppend: string);
{ {
  Instancia passando os valores a serem usados como conector inicial, final e intermediário.
  bem como os valores a serem adcionados no inicio e no final da cadeia gerada.
}
begin
  Self.FInitialConnector := AInitialConnector;
  Self.FIntermediateConnector := AIntermediateConnector;
  Self.FFinalConnector := AFinalConnector;
  Self.FPrepend := APrepend;
  Self.FAppend := AAppend;
end;

{ -------------------------------------------------------------------------------------------------------------------------------- }
class function TStringConnector.MakeSetString(Lst: TStrings): string;
{ {
  Monta string na forma de conjunto "( e1, e2, ..., en )", onde os "e's" são as entradas em Lst.
  Exemplo:
  Para gerar uma lista com aspas entre os elementos use a seguinte forma:
  con:=TStringConnector.Create( '", "', '", "', '", "', '( "', '")' );
  result:=con.Connect( Lst ); //result = "( "1", "2", ...., "n" )

  Revision: 13/10/2005 - Roger
}
var
  con: TStringConnector;
begin
  con := TStringConnector.Create(',', ',', ',', '(', ')');
  try
	Result := con.Connect(Lst);
  finally
	con.Free;
  end;
end;

{ -------------------------------------------------------------------------------------------------------------------------------- }
class function TStringConnector.MakeSetStringOfNames(Lst: TStrings): string;
{ {
  Idem a MakeSetString usando os Nomes das entradas em Lst;

  Revision: 13/10/2005 - Roger
}
var
  Names: TStringList;
  x: Integer;
begin
  Names := TStringList.Create;
  try
	for x := 0 to Lst.Count - 1 do begin
	  Names.Add(Lst.Names[x]);
	end;
	Result := TStringConnector.MakeSetString(Names);
  finally
	Names.Free;
  end;
end;

{ -------------------------------------------------------------------------------------------------------------------------------- }
class function TStringConnector.MakeSetStringOfValues(Lst: TStrings): string;
{ {
  Idem a MakeSetString usando os valores em Lst;

  Revision: 13/10/2005 - Roger
}
var
  Names: TStringList;
  x: Integer;
begin
  Names := TStringList.Create;
  try
	for x := 0 to Lst.Count - 1 do begin
	  Names.Add(Lst.ValueFromIndex[x]);
	end;
	Result := TStringConnector.MakeSetString(Names);
  finally
	Names.Free;
  end;
end;

function TBufferedStringStream.ReadString(Len: Word): AnsiString;
{ {
  Faz a leitura de uma cadeia aleatoria de Len caracteres.

  Revision: 18/12/2006 - Roger
}
var
  i: Word;
begin
  for i := 1 to Len do begin
	Result := Result + Self.ReadChar;
  end;
end;

function TBufferedStringStream.Search(const Str: AnsiString): boolean;
{ {
  Busca pela cadeia passada.
  Se encontrada posiciona o cursor no seu inicio e retorna true.

  Revision: 18/12/2006 - Roger
}
var
  TokenSize, TokenIndex: Integer;
  TokenPos: PAnsiChar;
  CurrChar: AnsiChar;
label
  FirstSearch;
begin
  Result := False;
  TokenSize := Length(Str);
  if (TokenSize > 0) then begin
  FirstSearch:;
	while (not Self.EoS) do begin
	  // Modified by roger 28/09/2014 17:35:26 - Passou a chamar a rotina em AnsiStrings para mover manipulação de AnsiString
	  TokenPos := AnsiStrings.SearchBuf(Self.FBuffer, Self.FBufferSize, Self.FOffSet, 0, Str[1], [soMatchCase, soDown]);
	  if (TokenPos <> nil) then begin // pagina contem 1o char da sequencia -> varrer atras do resto
		Self.FOffSet := (TokenPos - Self.FBuffer) + 1; // Offset salta ate a localizacao do 1o char
		for TokenIndex := 2 to TokenSize do begin
		  CurrChar := Self.ReadChar;
		  if (CurrChar <> Str[TokenIndex]) then begin
			// quebra da sequencia -> repasse do inicio da cadeia de busca ate este ponto
			goto FirstSearch; // -tentar 1o char novamente
		  end;
		end;
		// Chegando aqui a cadeia foi completamente localizada
		Result := True;
		Break;
	  end else begin // 1o Char da sequencia not exists -> repassa todo o resto para o retorno e carrega a nova pagina
		Self.FOffSet := Self.FBufferSize;
		Self.FillBuffer;
	  end;
	end;
  end;
end;

function TBufferedStringStream.Search(const Str: AnsiString; SkipStr: boolean): boolean;
{ {
  Realiza a busca de Str e caso encontre posiciona o cursor após a cadeia.
  Caso não encontre Str retorna falso e o cursor será posicionado no final do streamer/buffer.

  Revision: 18/12/2006 - Roger
}
var
  PrevPos: int64;
begin
  PrevPos := Self.Position;
  Result := Self.Search(Str);
  if (not SkipStr) then begin // retrocede ao inicio da cadeia
	Self.Seek(PrevPos - Length(Str));
  end;
end;

function TBufferedStringStream.ReadLine: AnsiString;
{ {
  Retorna a cadeia que finaliza com a quebra de linha a partir do ponto corrente, avançando o cursor.

  Revision: 19/12/2006 - Roger
}
var
  TokenSize, TokenIndex: Integer;
  TokenPos: PAnsiChar;
  PrevChar, CurrChar: AnsiChar;
label
  FirstSearch;

begin
  {$WARN UNSAFE_CODE OFF}
  TokenSize := Length(Self.FEol);
  if (TokenSize <= 0) then begin
	raise Exception.Create('Quebra de linha nula');
  end;
  {$WARN EXPLICIT_STRING_CAST_LOSS OFF}
  Result := AnsiString(EmptyStr);
  {$WARN EXPLICIT_STRING_CAST_LOSS ON}
FirstSearch:;
  while (not Self.EoS) do begin
	TokenPos := AnsiStrings.SearchBuf(Self.FBuffer, Self.FBufferSize, Self.FOffSet, 0, Self.FEol[1], [soMatchCase, soDown]);
	if (TokenPos <> nil) then begin // pagina contem 1o char da sequencia -> varrer atras do resto
	  // Repassa a parte anterior a encontada
	  PrevChar := TokenPos^;
	  TokenPos^ := #0;
	  Result := Result + PAnsiChar(Self.FBuffer + Self.FOffSet);
	  TokenPos^ := PrevChar;
	  Self.FOffSet := (TokenPos - Self.FBuffer) + 1; // Offset salta ate a localizacao do 1o char
	  for TokenIndex := 2 to TokenSize do begin
		CurrChar := Self.ReadChar;
		if (CurrChar <> Self.FEol[TokenIndex]) then begin
		  // quebra da sequencia -> repasse do inicio da cadeia de busca ate este ponto
		  Result := Result + Copy(Self.FEol, 1, TokenIndex);
		  goto FirstSearch; // -tentar 1o char novamente
		end;
	  end;
	  // Chegando aqui a cadeia foi completamente localizada
	  Break;
	end else begin // 1o Char da sequencia not exists -> repassa todo o resto para o retorno e carrega a nova pagina
	  Result := Result + PAnsiChar(Self.FBuffer + Self.FOffSet);
	  Self.FOffSet := Self.FBufferSize;
	  Self.FillBuffer;
	end;
  end;
  {$WARN UNSAFE_CODE ON}
end;

{ TXPStringList }

function TXPStringList.FindPos(const SubStr: string; var Line, Col: Integer): boolean;
/// <summary>
/// Retorna a linha e coluna de onde se achou SubStr. Retornando true neste caso apenas
/// Line e Col representam o ponto inicial da busca
/// <bold>IMPORTANTE</bold> ambos, Line e Col, baseados em 1
/// </summary>
/// <remarks>
///
/// </remarks>
var
  x, y, ofs: Integer;
begin
  Result := False;
  if (Line > Self.Count) then begin
	Exit;
  end;
  ofs := Max(1, Col); // Primeira passagem a partir do offset passado(ajuste para a base 1 no minimo)
  for y := Line - 1 to Self.Count - 1 do begin
	x := PosEx(SubStr, Self.Strings[y], ofs);
	if (x > 0) then begin
	  Line := y + 1; // ajuste retorno da base 1
	  Col := x;
	  Result := True;
	  Exit;
	end;
	ofs := 1; // zera offset apos primeira passagem
  end;
end;

function TXPStringList.FindPosIgnoreCase(const SubStr: string; var Line, Col: Integer): boolean;
/// <summary>
/// Retorna a linha e coluna de onde se achou SubStr. Retornando true neste caso apenas
/// Line e Col representam o ponto inicial da busca
/// <bold>IMPORTANTE</bold> ambos, Line e Col, baseados em 1
/// </summary>
/// <remarks>
///
/// </remarks>
var
  x, y, ofs: Integer;
  upSubStr: string;
begin
  Result := False;
  if (Line > Self.Count) then begin
	Exit;
  end;
  upSubStr := UpperCase(SubStr);
  ofs := Max(1, Col); // Primeira passagem a partir do offset passado(ajuste para a base 1 no minimo)
  for y := Line - 1 to Self.Count - 1 do begin
	x := PosEx(upSubStr, UpperCase(Self.Strings[y]), ofs);
	if (x > 0) then begin
	  Line := y + 1; // ajuste retorno da base 1
	  Col := x;
	  Result := True;
	  Exit;
	end;
	ofs := 1; // zera offset apos primeira passagem
  end;
end;

end.
