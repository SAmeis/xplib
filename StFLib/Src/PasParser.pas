{$IFDEF PasParser}
{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I StFLib.inc}
{$R-}
unit PasParser;

{
  Parser for Pascal source files.

  Written by Keith Wood, 31 July 1998.
}

interface

uses
  SysUtils, Windows, Classes;

const
  { TParser special tokens }
  toEOF        = AnsiChar(0);
  toIdentifier = AnsiChar(1);
  toReserved   = AnsiChar(2);
  toString     = AnsiChar(3);
  toComment    = AnsiChar(4);
  toNumber     = AnsiChar(5);

type
  { TPascalParser }
  TPascalParser = class(TObject)
  private
	FStream: TStream;
	FOrigin: Longint;
	FBuffer: PAnsiChar;
	FBufPtr: PAnsiChar;
	FBufEnd: PAnsiChar;
	FSourcePtr: PAnsiChar;
	FSourceEnd: PAnsiChar;
	FTokenPtr: PAnsiChar;
	FStringPtr: PAnsiChar;
	FSourceLine: Integer;
	FSaveChar: AnsiChar;
	FToken: AnsiChar;
	FWantAsSource: Boolean;
	procedure ReadBuffer;
	procedure SkipBlanks;
  public
	constructor Create(stmStream: TStream; bWantAsSource: Boolean);
	destructor Destroy; override;
	procedure CheckToken(cToken: AnsiChar);
	procedure CheckTokenSymbol(const sSymbol: string);
	procedure ErrorStr(const sMessage: string);
	function NextToken: AnsiChar;
	function SourcePos: Longint;
	function TokenFloat: Extended;
	function TokenInt: Longint;
	function TokenString: string;
	function TokenSymbolIs(const sSymbol: string): Boolean;
	property SourceLine: Integer read FSourceLine;
	property Token: AnsiChar read FToken;
	property WantAsSource: Boolean read FWantAsSource write FWantAsSource;
  end;

implementation

uses
  Consts, TypInfo;

{ TPascalParser }

const
  ParseBufSize        = 4096;
  sIdentifierExpected = 'Identifier expected';
  sReservedExpected   = 'Reserved word expected';
  sStringExpected     = 'String expected';
  sCommentExpected    = 'Comment expected';
  sNumberExpected     = 'Number expected';
  sCharacterExpected  = 'Character (%s) expected';
  sWordExpected       = 'Identifier (%s) expected';
  sParserError        = 'Error in parser on sourceline %d: %s';
  sInvalidString      = 'Invalid string';
  sInvalidComment     = 'Invalid comment';
  sLineTooLong        = 'Input line is too long';

var
  slsReserved: TStringList;

constructor TPascalParser.Create(stmStream: TStream; bWantAsSource: Boolean);
begin
  Self.FStream := stmStream;
  Self.FWantAsSource := bWantAsSource;
  Self.FBuffer := GetMemory(ParseBufSize);
  Self.FBuffer[0] := #0;
  Self.FBufPtr := Self.FBuffer;
  Self.FBufEnd := Self.FBuffer + ParseBufSize;
  Self.FSourcePtr := Self.FBuffer;
  Self.FSourceEnd := Self.FBuffer;
  Self.FTokenPtr := Self.FBuffer;
  Self.FSourceLine := 1;
  NextToken;
end;

destructor TPascalParser.Destroy;
begin
  if FBuffer <> nil then begin
	FStream.Seek(Longint(FTokenPtr) - Longint(FBufPtr), 1);
	FreeMemory(Self.FBuffer);
  end;
end;

procedure TPascalParser.CheckToken(cToken: AnsiChar);
begin
  if Token <> cToken then begin
	case cToken of
	  PasParser.toIdentifier: begin
		  ErrorStr(sIdentifierExpected);
		end;
	  PasParser.toReserved: begin
		  ErrorStr(sReservedExpected);
		end;
	  PasParser.toString: begin
		  ErrorStr(sStringExpected);
		end;
	  PasParser.toComment: begin
		  ErrorStr(sCommentExpected);
		end;
	  PasParser.toNumber: begin
		  ErrorStr(sNumberExpected);
		end;
	else begin
		ErrorStr(Format(sCharacterExpected, [cToken]));
	  end;
	end;
  end;
end;

procedure TPascalParser.CheckTokenSymbol(const sSymbol: string);
begin
  if not TokenSymbolIs(sSymbol) then begin
	ErrorStr(Format(sWordExpected, [sSymbol]));
  end;
end;

procedure TPascalParser.ErrorStr(const sMessage: string);
begin
  raise EParserError.Create(Format(sParserError, [FSourceLine, sMessage]));
end;

function TPascalParser.NextToken: AnsiChar;
var
  i: Integer;
  p, s: PAnsiChar;
begin
  {$WARN UNSAFE_CODE OFF}
  SkipBlanks;
  p := FSourcePtr;
  FTokenPtr := p;
  case p^ of
	'A' .. 'Z', 'a' .. 'z', '_': begin { Identifier or reserved word }
		Inc(p);
		while p^ in ['A' .. 'Z', 'a' .. 'z', '0' .. '9', '_'] do begin
		  Inc(p);
		end;
		Result := toIdentifier;
	  end;
	'#', '''': begin { String literal }
		s := p;
		while TRUE do begin
		  case p^ of
			'#': begin
				Inc(p);
				if WantAsSource then begin
				  Inc(s);
				end;
				i := 0;
				while p^ in ['0' .. '9'] do begin
				  i := i * 10 + (Ord(p^) - Ord('0'));
				  Inc(p);
				  if WantAsSource then begin
					Inc(s);
				  end;
				end;
				if not WantAsSource then begin
				  s^ := AnsiChar(i);
				  Inc(s);
				end;
			  end;
			'''': begin
				Inc(p);
				if WantAsSource then begin
				  Inc(s);
				end;
				while TRUE do begin
				  case p^ of
					#0, #10, #13: begin
						ErrorStr(sInvalidString);
					  end;
					'''': begin
						Inc(p);
						if WantAsSource then begin
						  Inc(s);
						end;
						if p^ <> '''' then begin
						  Break;
						end;
					  end;
				  end;
				  if not WantAsSource then begin
					s^ := p^;
				  end;
				  Inc(p);
				  Inc(s);
				end;
			  end;
		  else begin
			  Break;
			end;
		  end;
		end;
		FStringPtr := s;
		Result := PasParser.toString;
	  end;
	'{': { Comment } begin
		s := p;
		Inc(p);
		if WantAsSource then begin
		  Inc(s);
		end;
		while TRUE do begin
		  case p^ of
			#0: begin
				ErrorStr(sInvalidComment);
			  end;
			#10: begin
				Inc(FSourceLine);
			  end;
			'}': begin
				Inc(p);
				if WantAsSource then begin
				  Inc(s);
				end;
				Break;
			  end;
		  end;
		  s^ := p^;
		  Inc(p);
		  Inc(s);
		end;
		FStringPtr := s;
		Result := toComment;
	  end;
	'/': { Comment - possibly } begin
		Result := p^;
		s := p;
		Inc(p);
		if WantAsSource then begin
		  Inc(s);
		end;
		if p^ = '/' then { Comment definitely } begin
		  Inc(p);
		  if WantAsSource then begin
			Inc(s);
		  end;
		  while TRUE do begin
			case p^ of
			  #0: begin
				  ErrorStr(sInvalidComment);
				end;
			  #13, #10: begin
				  Break;
				end;
			end;
			s^ := p^;
			Inc(p);
			Inc(s);
		  end;
		  FStringPtr := s;
		  Result := toComment;
		end;
	  end;
	'(': { Comment - possibly } begin
		Result := p^;
		s := p;
		Inc(p);
		if WantAsSource then begin
		  Inc(s);
		end;
		if p^ = '*' then { Comment definitely } begin
		  Inc(p);
		  if WantAsSource then begin
			Inc(s);
		  end;
		  while TRUE do begin
			case p^ of
			  #0: begin
				  ErrorStr(sInvalidComment);
				end;
			  #10: begin
				  Inc(FSourceLine);
				end;
			  '*': begin
				  s^ := p^;
				  Inc(p);
				  if p^ = ')' then begin
					if WantAsSource then begin
					  Inc(s);
					end;
					s^ := p^;
					Inc(p);
					if WantAsSource then begin
					  Inc(s);
					end;
					Break;
				  end else begin
					Inc(s);
				  end;
				end;
			end;
			s^ := p^;
			Inc(p);
			Inc(s);
		  end;
		  FStringPtr := s;
		  Result := toComment;
		end;
	  end;
	'$': { Hexadecimal literal } begin
		Inc(p);
		while p^ in ['0' .. '9', 'A' .. 'F', 'a' .. 'f'] do begin
		  Inc(p);
		end;
		Result := toNumber;
	  end;
	'-', '0' .. '9': { Numeric literal } begin
		Inc(p);
		while p^ in ['0' .. '9', '.', 'e', 'E', '+', '-'] do begin
		  Inc(p);
		end;
		Result := toNumber;
	  end;
  else { Single AnsiCharacter } begin
	  Result := p^;
	end;
	if Result <> toEOF then begin
	  Inc(p);
	end;
  end;
  FSourcePtr := p;
  if (Result = toIdentifier) and (slsReserved.IndexOf(TokenString) > -1) then begin
	Result := toReserved;
  end;
  FToken := Result;
  {$WARN UNSAFE_CODE ON}
end;

procedure TPascalParser.ReadBuffer;
var
  Count: Integer;
begin
  Inc(FOrigin, FSourcePtr - FBuffer);
  FSourceEnd[0] := FSaveChar;
  Count := FBufPtr - FSourcePtr;
  if Count <> 0 then begin
	Move(FSourcePtr[0], FBuffer[0], Count);
  end;
  FBufPtr := FBuffer + Count;
  Inc(FBufPtr, FStream.Read(FBufPtr[0], FBufEnd - FBufPtr));
  FSourcePtr := FBuffer;
  FSourceEnd := FBufPtr;
  if FSourceEnd = FBufEnd then begin
	FSourceEnd := LineStart(FBuffer, FSourceEnd - 1);
	if FSourceEnd = FBuffer then begin
	  ErrorStr(sLineTooLong);
	end;
  end;
  FSaveChar := FSourceEnd[0];
  FSourceEnd[0] := #0;
end;

procedure TPascalParser.SkipBlanks;
begin
  {$WARN UNSAFE_CODE OFF}
  while TRUE do begin
	case FSourcePtr^ of
	  #0: begin
		  ReadBuffer;
		  if FSourcePtr^ = #0 then begin
			Exit;
		  end;
		  Continue;
		end;
	  #10: begin
		  Inc(FSourceLine);
		end;
	  #33 .. #255: begin
		  Exit;
		end;
	else begin
		if WantAsSource then begin
		  Exit;
		end;
	  end;
	end;
	Inc(FSourcePtr);
  end;
  {$WARN UNSAFE_CODE ON}
end;

function TPascalParser.SourcePos: Longint;
begin
  Result := FOrigin + (FTokenPtr - FBuffer);
end;

function TPascalParser.TokenFloat: Extended;
begin
  Result := StrToFloat(TokenString);
end;

function TPascalParser.TokenInt: Longint;
begin
  Result := StrToInt(TokenString);
end;

function TPascalParser.TokenString: string;
var
  i: Integer;
begin
  if FToken in [PasParser.toString, PasParser.toComment] then begin
	i := FStringPtr - FTokenPtr;
  end else begin
	i := FSourcePtr - FTokenPtr;
  end;
  SetString(Result, FTokenPtr, i);
end;

function TPascalParser.TokenSymbolIs(const sSymbol: string): Boolean;
begin
  Result := (Token in [toIdentifier, toReserved]) and (CompareText(sSymbol, TokenString) = 0);
end;

initialization

begin
  slsReserved := TStringList.Create;
  with slsReserved do begin
	{ Reserved words }
	Add('and');
	Add('array');
	Add('as');
	Add('asm');
	Add('begin');
	Add('case');
	Add('class');
	Add('constructor');
	Add('destructor');
	Add('dispinterface');
	Add('div');
	Add('do');
	Add('downto');
	Add('else');
	Add('end');
	Add('except');
	Add('exports');
	Add('file');
	Add('finalization');
	Add('finally');
	Add('for');
	Add('function');
	Add('goto');
	Add('if');
	Add('implementation');
	Add('in');
	Add('inherited');
	Add('initialization');
	Add('inline');
	Add('interface');
	Add('is');
	Add('label');
	Add('library');
	Add('mod');
	Add('nil');
	Add('not');
	Add('object');
	Add('of');
	Add('or');
	Add('out');
	Add('packed');
	Add('procedure');
	Add('program');
	Add('property');
	Add('raise');
	Add('record');
	Add('repeat');
	Add('resourcestring');
	Add('set');
	Add('shl');
	Add('shr');
	Add('string');
	Add('stringresource');
	Add('then');
	Add('threadvar');
	Add('to');
	Add('try');
	Add('type');
	Add('unit');
	Add('until');
	Add('uses');
	Add('var');
	Add('while');
	Add('with');
	Add('xor');
	{ Standard directives }
	Add('absolute');
	Add('abstract');
	Add('assembler');
	Add('automated');
	Add('cdecl');
	Add('default');
	Add('dispid');
	Add('dynamic');
	Add('export');
	Add('external');
	Add('far');
	Add('forward');
	Add('index');
	Add('message');
	Add('name');
	Add('near');
	Add('nodefault');
	Add('override');
	Add('pascal');
	Add('private');
	Add('protected');
	Add('public');
	Add('published');
	Add('read');
	Add('readonly');
	Add('register');
	Add('resident');
	Add('safecall');
	Add('stdcall');
	Add('stored');
	Add('virtual');
	Add('write');
	Add('writeonly');
	Sorted := TRUE;
  end;
end;

finalization

begin
  slsReserved.Free;
end;

end.
