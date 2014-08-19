{$IFDEF Str_pas}
	{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I XPLib.inc}

unit Str_pas;

interface

uses SysUtils, Classes, Windows;

const
	DEFAULT_WORD_DELIMITERS_LIST = [#0, ' ', #13, #10, #9]; //Caracteres que podem preceder/anteceder um palavra;
	{ TODO -oRoger -cLIB : procurar referencias a VISUAL_CHARS_LIST e detectar problemas, pois pelo nome VISUAL_CHARS_LIST deveria ser [ #32..#126] }
	VISUAL_CHARS_LIST = ['\', '$', '%', '&', '@', '*', '+', '/', 'A'..'Z', 'a'..'z', '{', '}', '(', ')', '>', '<', '?', '0'..'9', '[', ']'];
    INTEGER_CHARS_LIST = ['0'..'9', '-', '+'];
	FLOAT_CHARS_LIST  = INTEGER_CHARS_LIST + [',', '.', 'E', 'e'];
    NOT_ALPHA_LOWER_CASE_CHARS_LIST = [#0..#255] -
        [#9, #32, 'a'..'z', 'á', 'é', 'í', 'ó', 'ú', 'à', 'ã', 'õ', 'â', 'ê', 'ô', 'ç', 'ü'];

type
    TTrimPos = (tpTrimLeft, tpTrimRight, tpTrimAll);

function AllTrim(Str : string) : string;
//Transforma um text com recursos de acentuacao em um texto totalmente maiusculo e sem acentos
function ASCIISearchString(const NormalSrcString : string) : string; deprecated;
//Obtém os dígitos de uma string( existe bug )
function Digits(const S : string) : string; deprecated;
function DoubleQuoteStr(const Str : string) : string;
function EmptyString(Str : string) : Boolean;
function FormatMemSize(Size : int64) : string;
function GetDelimitedLevelSubStr(const StartDelimiter, EndDelimiter, Str : string; Level : integer) : string;
function GetDelimitedSubStr(const Delimiter, Src : string; const Index : integer) : string;
function GetDelimitedSubStr2(const StartDelimiter, EndDelimiter, Src : string; const Index : integer) : string;
//Retorna caracter na posicao dada por Index;
function GetIChar(const Str : string; Index : integer) : char;
//Retorna a indexesima palavra de Str
function GetIndexedWordString(Str : PChar; Index : integer; Delimiters : TSysCharSet = DEFAULT_WORD_DELIMITERS_LIST) : string;
//Retorna uma substring na forma de macro
function GetMacro(Str, Delimiter : string; Index : integer) : string;
//Retorna string alinhada a esquerda para o tamanho dado
function PadL(const Str : string; Len : word; C : Char = ' ') : string;
//Retorna string alinhada a direita para o tamanho dado
function PadR(const Str : string; Len : word; C : Char = ' ') : string;
function PadRBlank(const Str : string; const Size : cardinal) : string;
function PadStrPas(Str : string; Caracter : Char; Size, Position : integer) : string;
function RemovDupBlanks(Str : string) : string;
//Retira todos os caracteres "C" duplicados no inicio e/ou no meio e/ou no fim da string
function RemoveDupChar(const Str : string; const C : Char) : string;
procedure RemoveDupStrList(List : TStrings; IgnoreCase : Boolean);
//  remove as aspas duplas 
function RemoveQuoteChar(const Str : String; const quote : Char = '"'): String;
//Remove todas as ocorrencias de Caracter em Str
function RemovStrChar(const Str : string; Caracter : Char) : string;
function RepChar(Caracter : Char; Many : cardinal) : string; deprecated;
function ReplaceSubString(const Str : string; OldStr, NewStr : string) : string;
//retorna string da composicao dos valores da lista com o conector no meio
function StrConnectSubStrings(const Connector : string; SubStrings : array of string) : string; overload;
function StrConnectSubStrings(const Connector : string; SubStrings : TStrings) : string; overload;
function StrCopyAfter(const SubStr, Str : string; Position : Cardinal = 0) : string;
function StrCopyAfterLast(const SubStr, Str : string) : string; deprecated;
function StrCopyAfterNth(const SubStr, Str : string; Index : word = 0) : string;
function StrCopyBefore(const SubStr, Str : string; Position : Cardinal = 0) : string;
function StrCopyBeforeLast(const SubStr, Str : string) : string;
function StrCopyBeforeNth(const SubStr, Str : string; Nth : integer = 0) : string;
function StrCountMacros(const Sep, Str : string) : integer;
//Contas o numero de vezes que a palavra se repete na string
function StrCountCharRepet(C : Char; const Str : string) : integer;
//Contas o numero de vezes que a palavra se repete na string
function StrCountRepet(const Sub : string; Str : string; Delimiters : TSysCharSet = DEFAULT_WORD_DELIMITERS_LIST) : integer;
//Contas o numero palavras que o texto comtem
function StrCountWords(const Str : string; Delimiters : TSysCharSet = DEFAULT_WORD_DELIMITERS_LIST) : integer;
//Retorna a 1a palavra da Str passada
function StrFirstWord(const Str : string; Delims : TSysCharSet = DEFAULT_WORD_DELIMITERS_LIST) : string;
//Filtra/elimina todos os caracteres nao contidos no conjunto passado
function StrFilterChars(const Str : string; FilterSet : TSysCharSet) : string;
//posicao da ultima ocorrenia de SubStr em Str
function StrLastPos(const SubStr, Str : string) : integer;
//Retorna a ultima palara de Str
function StrLastWord(const Str : string; Delims : TSysCharSet = DEFAULT_WORD_DELIMITERS_LIST) : string;
//Returns true if a string contains lowercase characters.
function StrHasLowCaseChar(const S : string) : boolean;
//Returns true if a string contains Uppercase characters.
function StrHasUpCaseChar(const S : string) : boolean;
function StrInNumRange(Str : string; MinVal, MaxVal : double) : boolean; deprecated;
//Retorna a posicao da enesima ocorrencia de SubStr em Str
function StrNPos(const SubStr : string; Str : string; Many : integer) : integer;
//Retorna string centralizada no tamanaho passado complementando com o caracter indicado
function StrPadC(const Str : string; Size : integer; C : Char = ' ') : string;
//Localiza primeira ocorrencia de um dos caracteres presentes na lista
function StrPosChar(const s : string; const CharSet : TSysCharSet; StartPos : cardinal = 0) : integer;
//Retorna posicao de SubStr em Str iniciando a pesquisa em StartPos e podendo ignorar caixa
function StrPosEx(const SubStr, Str : string; StartPos : Cardinal = 0; IgnoreCase : boolean = FALSE) : integer;
//Remove todos os caracteres de Str que existam no conjunto passado
function StrPurge(const Str : string; aSet : TSysCharSet) : string;
//Troca OldStr por NewStr em SrcStr uma unica vez atribuindo StartAt com o indice dessa troca
function StrReplaceFirst(OldStr, NewStr, SrcStr : string; var StartAt : integer; Wholeword, CaseSensitive : boolean;
    Delims : TSysCharSet = DEFAULT_WORD_DELIMITERS_LIST) : string;
//Retorna um PChar para o inicio da ultima ocorrencia de SubStr em Str
function StrRScanStr(const SubStr, Str : PChar) : PChar;
//Transforma um CharSet em um lista visivel no formato string
function StrSysCharSetToString(const ASet : TSysCharSet; DoRange : boolean = TRUE) : string;
//Acrescenta tab no inicio de cada linha das strings passadas
procedure StrTabifyStrings(Str : TStrings);
//Converte um valor para um logico
function StrToBool(const Str : string) : boolean;
//Converte string para currency
function StrToCurrency(Str : string) : Currency;
//Converte string em inteiro tentando removendo caracteres improvaveis
function StrToIntFilter(const Str : string) : integer;
//Converte string para float
function StrToFloat2(Str : string) : Extended;
//Remoe caracteres invalidos antes dee converter para um float
function StrToFloatFilter(const Str : string) : double;
// Aloca e copia a string para um PChar ( Usar TStrHnd )
function StrToPChar(Str : string) : PChar; deprecated;
//Converte string em coordenadas 2D
function StrToPoint(Str : string; Delimiter : char = ',') : TPoint;
function StrToSysCharSet(Str : string) : TSysCharSet;
function StrTrimChar(Str : string; Caracter : Char; Position : TTrimPos) : string;
function StrTrimTabs(Str : string; Position : TTrimPos) : string;
//Retorna indice onde se encontra a palavra dada por SubStr em Str
function StrWordPos(WordStr : string; Str : string; Delims : TSysCharSet = [' ']; StartPos : Cardinal = 0) : cardinal;
//Remove espacos e tabulacoes iniciais e finais de uma string;
function StrUnindent(Str : string) : string;
//Trunca nome de arquivo/url de modo a reduzir seu comprimento
function TruncMidleFileName(const FileName : string) : string;
//Retorna o conjunto de caracteres usados no texto que pertencam ao conjunto dado
function UsedCharSet(const CharSet, Text : string) : string;

implementation


uses
    Math, StrHnd;

function AllTrim(Str : string) : string;
{
 Returns a string whithout trailing and leading spaces, by calling StrTrimChar() with tpTrimAll flag.
}
begin
	AllTrim := StrTrimChar(Str, ' ', tpTrimAll);
end;

function ASCIISearchString(const NormalSrcString : string) : string;
{
Deprecated Ver TStrHnd.ASCIISearchString().
}
var
	i : integer;
begin
	Result := AnsiUpperCase(NormalSrcString);
	for i := 1 to Length(Result) do begin
		if Ord(Result[i]) > 127 then begin  //Sera necessario corrigir
			case Result[i] of
				'Ç'    : begin
					Result[i] := 'C';
				end;
				'Á', 'À', 'Ã', 'Â', 'Ä'    : begin
					Result[i] := 'A';
				end;
				'É', 'È', 'Ê', 'Ë'    : begin
					Result[i] := 'E';
				end;
				'Í', 'Ì', 'Î', 'Ï'    : begin
					Result[i] := 'I';
				end;
				'Ó', 'Ò', 'Õ', 'Ô', 'Ö'    : begin
					Result[i] := 'O';
				end;
				'Ú', 'Ù', 'Û', 'Ü'    : begin
					Result[i] := 'U';
				end;
				'º'    : begin
					Result[i] := 'O';
				end;
				'ª'    : begin
					Result[i] := 'A';
				end;
				else begin
					Result[i] := ' ';
				end;
			end;
		end;
	end;
end;

function Digits(const S : string) : string;
{
Obtém os dígitos de uma string, removendo aqueles caracteres fora do range 0..9 ( made by aruah )
}
var
	i : integer;
begin
	SetLength(Result, Length(S));
	for i := 1 to Length(S) do begin
		if CharInSet( S[i], ['0'..'9'] ) then begin
			Result := Result + S[i];
		end;
	end;
end;

function DoubleQuoteStr(const Str : string) : string;
{
Returns a string with a double quotes (") at both edges.
}
begin
	Result := '"' + Str + '"';
end;

function EmptyString(Str : string) : Boolean;
{
Tests if a string has any significant character.


WARNING: This routine needs optmization and corrections.
}
var
    i : integer;
begin
    Result := TRUE;
    for i := 1 to Length(Str) do begin
        if Str[i] <> ' ' then begin
            Result := FALSE;
            Exit;
        end;
    end;
end;

function FormatMemSize(Size : int64) : string;
{
Returns the value given by Size formatted at bytes units.
Used when a size of file or memory is outputed.
The output limit is Giga bytes.

Ex.: 123546 -> 123,5 MB
WARNING: Need transpose to a correct Formmatter class in Class method form.
}
const
    MULT = 1024;
begin
    //Size:=Size div 1024;
    if (Size < MULT) then begin //Bytes
        Result := Format('%d Bytes', [Size]);
    end else begin
        if Size < IntPower(MULT, 2) then begin //KBytes
            Result := Format('%5.2f KB', [Size / MULT]);
        end else begin
            if Size < IntPower(MULT, 3) then begin //MBytes
                Result := Format('%5.2f MB', [Size / (IntPower(MULT, 2))]);
            end else begin
                if Size < IntPower(MULT, 4) then begin //GBytes
                    Result := Format('%5.2f GB', [Size / (IntPower(MULT, 3))]);
                {end else begin
                    Result:=Format( '%5.2f TB', [ Size / (1e+9*MULT) ] );}
                end;
            end;
        end;
    end;
end;

function GetDelimitedLevelSubStr(const StartDelimiter, EndDelimiter, Str : string; Level : integer) : string;
{
Isola string cercada pelos tokens especificado por StartDelimiter e EndDelimiter.
Level representa a profundidade desejada. Para Level > 1 o resultado se assemelha a chamar a rotina com o resultado anterior.
}
var
	i, SPos : integer;
	PEndStr : PChar;
begin
	if Str = EmptyStr then begin
		Result := EmptyStr;
	end else begin
		Result := Str;
		for i := 0 to Level do begin
			SPos := Pos(StartDelimiter, Result);
            if SPos = 0 then begin
                Result := EmptyStr;
            end else begin
                Delete(Result, 1, SPos + Length(StartDelimiter) - 1);
                PEndStr := StrRScanStr(PChar(EndDelimiter), PChar(Result));
                if PEndStr <> NIL then begin
                    Result := Copy(Result, 1, Length(Result) - Length(PEndStr));
                end else begin
                    Result := EmptyStr;
                end;
            end;
        end;
    end;
end;

function GetDelimitedSubStr(const Delimiter, Src : string; const Index : integer) : string;
{
Returns a string at the cell surrounded by Index times the Delimiter.

Ex.:
Ret:=GetDelimitedSubStr('*', 'init*medium*final', 0 ); //Ret = 'init';
Ret:=GetDelimitedSubStr('*', 'init*medium*final', 1 ); //Ret = 'medium';
Ret:=GetDelimitedSubStr('*', 'init*medium*final', 99 ); //Ret = EmptyStr;

Revision 18/5/2005
}
var
	i, Loc : integer;
begin
	Result := Src;
    for i := 0 to Index - 1 do begin
        Loc := Pos(Delimiter, Result);
        if Loc = 0 then begin
			Result := EmptyStr;
			Exit;
		end;
		Result := Copy(Result, Loc + Length(Delimiter), Length(Result));
	end;
	Loc := Pos(Delimiter, Result);
	if Loc <> 0 then begin
		Result := Copy(Result, 1, Loc - 1);
    end;
end;

function GetDelimitedSubStr2(const StartDelimiter, EndDelimiter, Src : string; const Index : integer) : string;
{
Retorna string circundada por delimitadores
}
var
	BPos, EPos : integer;
	S : string;
	i : integer;
begin
	S := Src;
	for i := 0 to Index do begin
		BPos := Pos(StartDelimiter, S);
		Delete(S, 1, BPos + Length(StartDelimiter) - 1);
		EPos := Pos(EndDelimiter, S);
		if EPos = 0 then begin
			Result := EmptyStr;
			Exit;
		end else begin
			Result := Copy(S, 1, EPos - 1); //Atualiza resultado
			S := Copy(S, EPos + Length(EndDelimiter), Length(S));
		end;
	end;
end;

function GetIChar(const Str : string; Index : integer) : char;
{
Retorna caracter na posicao dada por Index;
}
begin
	if (Length(Str) < Index) or (Index <= 0) then begin
		Result := #0;
	end else begin
		Result := Str[Index];
	end;
end;

function GetIndexedWordString(Str : PChar; Index : integer; Delimiters : TSysCharSet = DEFAULT_WORD_DELIMITERS_LIST) : string;
{
Retorna a indexesima palavra de Str
}
var
	CharCount : integer;
	P, PEnd : PChar;
begin
	Include(Delimiters, #0); //Deve-se garantir que #0 pertence ao conjunto passado
	P := Str;
	//Varre ate encontra a primeira palavra valida
	while CharInSet( P^ ,Delimiters ) do begin
		Inc(P);
	end;
	while Index > 0 do begin
		//Range checking
		if P^ = #0 then begin
			Result := EmptyStr;
			Exit;
		end;
		//Posiciona no fim da primeira palavra para este indice
		while not CharInSet(P^ ,Delimiters) do begin
			Inc(P);
		end;
		//Ultima palvra atingida precocemente
		if P^ = #0 then begin
			Break;
		end;
		//Varre ate encontra a primeira palavra valida
		while CharInSet( P^ ,Delimiters )do begin
			Inc(P);
		end;
		Dec(Index);
	end;
	PEnd := P;
	while not CharInSet(PEnd^ ,Delimiters) do begin
		Inc(PEnd);
	end;
	CharCount := (PEnd - P);
	//   SetLength( Result, CharCount + 1 );
	SetLength(Result, CharCount); //Com 1 byte a menos evita lixo do fim
	StrLCopy(PChar(Result), P, CharCount);
end;

function GetMacro(Str, Delimiter : string; Index : integer) : string;
{
 Returns the result of GetDelimitedSubStr() enclosure by the Delimiter.

 Ret:=GetMacro('MyPath=@path@;c:\windows', 1); //Ret =@path@

 WARNING: Routine has a bug for Index = 0
 }
begin
	Result := GetDelimitedSubStr(Delimiter, Str, Index);
	Result := Delimiter + Result + Delimiter;
end;

function PadL(const Str : string; Len : word; C : Char = ' ') : string;
    //----------------------------------------------------------------------------------------------------------------------
    //Retorna string alinhada a esquerda para o tamanho dado
var
    Dif : integer;
begin
    Result := Str;
    SetLength(Result, Len);
    Dif := Len - Length(Str);
    if Dif > 0 then begin
        Result := StringOfChar(C, Dif) + Str;
    end;
end;

function PadR(const Str : string; Len : word; C : Char = ' ') : string;
    //----------------------------------------------------------------------------------------------------------------------
    //Retorna string alinhada a direita para o tamanho dado
var
    Dif : integer;
begin
    Result := Str;
    SetLength(Result, Len);
    Dif := Len - Length(Str);
    if Dif > 0 then begin
        Result := Str + StringOfChar(C, Dif);
    end;
end;

function PadRBlank(const Str : string; const Size : cardinal) : string;
    //----------------------------------------------------------------------------------------------------------------------
var
    Total : cardinal;
begin
    Result := Str;
    Total  := Length(Str);
    if Total > Size then begin
        SetLength(Result, Size);
    end else begin
		Result := Result + StringOfChar(' ', Size - Total);
    end;
end;

function PadStrPas(Str : string; Caracter : Char; Size, Position : integer) : string;
    //----------------------------------------------------------------------------------------------------------------------
var
    Total : integer;
    SubStr : string;
begin
    Total := Length(Str);
    if Total > Size then begin
        Exit;
    end;
    SubStr := StringOfChar(Caracter, Size - Total);
    if (Position < 0) or (Position > Length(Str)) then begin
        Str := Str + Substr;
    end else begin
        Insert(Substr, Str, Position);
    end;
    PadStrPas := Str;
end;

function RemovDupBlanks(Str : string) : string;
    //----------------------------------------------------------------------------------------------------------------------
var
    DupPos : byte;
begin
    Str := StrTrimChar(Str, ' ', tpTrimAll);
    repeat
        DupPos := Pos('  ', Str);
        if DupPos <> 0 then begin
            Delete(Str, DupPos, 1);
        end;
    until DupPos = 0;
    RemovDupBlanks := Str;
end;

function RemoveDupChar(const Str : string; const C : Char) : string;
    //----------------------------------------------------------------------------------------------------------------------
    //Retira todos os caracteres C duplicados no início e/ou no meio e/ou no fim da string
begin
    Result := Str;
    while (Pos(C + C, Result) > 0) do begin
        Delete(Result, Pos(C + C, Result), 1);
    end;
    //    if( Result > EmptyStr ) and (Result[1]                = C) then Delete(Result,    1,                     1);
    //    if( Result > EmptyStr ) and (Result[Length(Result)] = C) then Delete(Result,    Length( Result ),    1);
end;

procedure RemoveDupStrList(List : TStrings; IgnoreCase : boolean);
//----------------------------------------------------------------------------------------------------------------------
var
    i : integer;
    e : integer;
begin
    for e := 0 to List.count - 1 do begin
        for i := (List.count - 1) downto (e + 1) do begin
            if IgnoreCase then begin
                if UpperCase(List.Strings[e]) = UpperCase(List.Strings[i]) then begin
                    List.Delete(i);
                end;
            end else begin
                if List.Strings[e] = List.Strings[i] then begin
                    List.Delete(i);
                end;
            end;
        end;
    end;
end;

function RemoveQuoteChar(const Str : String; const quote : Char = '"'): String;
begin
   if (str[1]=quote) and (str[length(str)]=quote) then begin
       Result := copy(Str,2,length(str)-2);
   end;                                    
end;

function RemovStrChar(const Str : string; Caracter : Char) : string;
    //----------------------------------------------------------------------------------------------------------------------
    //Remove todas as ocorrencias de Caracter em Str
var
    CPos : byte;
begin
    Result := Str;
    repeat
        CPos := Pos(Caracter, Result);
        if CPos <> 0 then begin
            Delete(Result, CPos, 1);
        end;
    until CPos = 0;
end;

function RepChar(Caracter : Char; Many : cardinal) : string;
{
Usar StringOfChar()
}
begin
    SetLength(Result, Many);
    if Many > 0 then begin
        FillChar(Result[1], Byte(Many), Caracter);
    end;
end;

function ReplaceSubString(const Str : string; OldStr, NewStr : string) : string;
    //----------------------------------------------------------------------------------------------------------------------
var
    p : integer;
begin
    Result := Str;
    p := Pos(OldStr, Result);
    while p <> 0 do begin
        Delete(Result, p, Length(OldStr));
        Insert(NewStr, Result, P);
        p := Pos(OldStr, Result);
    end;
end;

function StrConnectSubStrings(const Connector : string; SubStrings : array of string) : string;
    //----------------------------------------------------------------------------------------------------------------------------------
    //Retorna string da composicao dos valores da lista com o conector no meio
var
    i : integer;
begin
    Result := EmptyStr;
    for i := Low(SubStrings) to High(SubStrings) do begin
        Result := Result + Connector + SubStrings[i];
    end;
    Result := Copy(Result, Length(Connector) + 1, Length(Result));
end;

function StrConnectSubStrings(const Connector : string; SubStrings : TStrings) : string;
    //----------------------------------------------------------------------------------------------------------------------------------
var
    i : integer;
begin
    Result := EmptyStr;
    if SubStrings.Count >= 1 then begin
        Result := SubStrings.Strings[0];
        for i := 1 to SubStrings.Count - 1 do begin
            Result := Result + Connector + SubStrings.Strings[i];
        end;
    end;
end;


function StrCopyAfter(const SubStr, Str : string; Position : Cardinal = 0) : string;
    //----------------------------------------------------------------------------------------------------------------------
    // Scans for SubStr in s - if found the characters after SubStr is returned else
    // an empty string is returned.
    // Example: After('Funny','Those Funny People') > ' People';
    // Example: After('land','Your land is my land ok',1) > ' is my land ok'
    // Example: After('land','Your land is my land ok',7) > ' ok'
    // Example: After('not there','Your land is my land ok') > ''
    // See also Before,Parse,AfterIC,AfterRev and Set overloaded versions.
var
    p : integer;
begin
    p := StrPosEx(SubStr, Str, Position);
    if (p = 0) then begin
        Result := EmptyStr;
    end else begin
        Result := System.Copy(Str, P + Length(SubStr), Length(Str));
    end;
end;

function StrCopyAfterLast(const SubStr, Str : string) : string; deprecated;
{{
URGENTE: Devido a bug básico rotina depreciada usar TStrHnd.CopyAfterLast

This functions scans s for SubStr from the right and returns the portion after SubStr.
Example: AfterRev('.','c:\my.file.txt') > '.txt'
}
begin
    Result := StrRScanStr(PChar(SubStr), PChar(Str));
    if (Result <> EmptyStr) then begin
        Result := System.Copy(Str, Length(Result) + Length(SubStr), Length(Str));
    end;
end;

function StrCopyAfterNth(const SubStr : string; const Str : string; Index : word = 0) : string;
    //----------------------------------------------------------------------------------------------------------------------
    //AfterNth - Returns the string after the <Nth> appearance of SubStr.
    // Example: AfterNth('tree','Green tree, blue tree everywhere',1) > ', blue tree everywhere'
    // Example: AfterNth('tree','Green tree, blue tree everywhere',2) > ' everywhere'
    //See also After,BeforeNth
var
    i : integer;
begin
    if Index <= 1 then begin
        Result := StrCopyAfter(SubStr, Str);
    end else begin
        Result := Str;
        for i := 1 to Index do begin
            Result := StrCopyAfter(SubStr, Result);
        end;
    end;
end;

function StrCopyBefore(const SubStr, Str : string; Position : Cardinal = 0) : string;
    //----------------------------------------------------------------------------------------------------------------------
var
    p : integer;
begin
    p := StrPosEx(SubStr, Str, Position);
    if (p = 0) then begin
        Result := Str;
    end else begin
        Result := System.Copy(Str, 1, p - 1);
    end;
end;

function StrCopyBeforeLast(const SubStr, Str : string) : string;
    //----------------------------------------------------------------------------------------------------------------------
var
    p : integer;
begin
    p := StrLastPos(SubStr, Str);
    if (p = 0) then begin
        Result := Str;
    end else begin
        Result := System.Copy(Str, 1, p);
    end;
end;


function StrCopyBeforeNth(const SubStr, Str : string; Nth : integer = 0) : string;
    //----------------------------------------------------------------------------------------------------------------------
var
    i, p : integer;
begin
    if Nth <= 1 then begin
        Result := StrCopyBefore(SubStr, Str);
    end else begin
        p := 1;
        for i := 1 to Nth do begin
            p := StrPosEx(SubStr, Str, p + 1);
        end;
        if (p = 0) then begin
            Result := Str;
        end else begin
            Result := System.Copy(Str, 1, p - 1);
        end;
    end;
end;

function StrCountMacros(const Sep, Str : string) : integer;
    //----------------------------------------------------------------------------------------------------------------------
begin
    Result := (StrCountRepet(Sep, Str) div 2);
end;

function StrCountCharRepet(C : Char; const Str : string) : integer;
    //----------------------------------------------------------------------------------------------------------------------------------
    //Conta o numero de vezes que o caracter se repete na string
var
    P : PChar;
begin
    Result := 0;
    P := PChar(Str);
    while (P^ <> #0) do begin
        if P^ = C then begin
            Inc(Result);
        end;
        Inc(P);
    end;
end;

function StrCountRepet(const Sub : string; Str : string; Delimiters : TSysCharSet = DEFAULT_WORD_DELIMITERS_LIST) : integer;
    //----------------------------------------------------------------------------------------------------------------------
    //Contas o numero de vezes que a palavra se repete na string
    //Esta rotina foi alterada em 2001/01/11 para incluir delimitadores para as palavras
var
    Loc : integer;
begin
    Result := -1;
    repeat
        Loc := StrWordPos(Sub, Str, Delimiters);
        Str := Copy(Str, Loc + Length(Sub), Length(Str));
        Inc(Result);
    until Loc = 0;
end;

function StrCountWords(const Str : string; Delimiters : TSysCharSet = DEFAULT_WORD_DELIMITERS_LIST) : integer;
    //----------------------------------------------------------------------------------------------------------------------------------
    //Contas o numero palavras que o texto comtem
var
    P : Pchar;
begin
    P := PChar(Str);
    //Travar no inicio da 1a palavra
    while CharInSet( P^ ,Delimiters) and (P^ <> #0) do begin
        Inc(P);
    end;
    Result := 0;
    if P^ <> #0 then begin
        Result := 0;
        while P^ <> #0 do begin

            //Pula a palavra corrente
            while not CharInSet(P^, Delimiters) and (P^ <> #0) do begin
                Inc(P);
            end;
            Inc(Result);
            if P^ = #0 then begin
                Exit;
            end;

            //Pula conjunto de delimitadores seguintes
            while CharInSet(P^ ,Delimiters) and (P^ <> #0) do begin
                Inc(P);
            end;
            if P^ = #0 then begin
                Exit;
            end;

        end;
    end;
end;

function StrFirstWord(const Str : string; Delims : TSysCharSet = DEFAULT_WORD_DELIMITERS_LIST) : string;
    //----------------------------------------------------------------------------------------------------------------------
    //
    // Example: FirstWord('Delphi programming is fun') > 'Delphi'
    // Example: FirstWord('Delphi, programming is fun') >'Delphi,'
    // Example: FirstWord('Delphi, programming is fun',[' ','.',',']) >'Delphi'
    // Example: FirstWord('Delphiprogramming') > 'Delphiprogramming'
    //See also LastWord
var
    i : integer;
begin
    for i := 1 to Length(Str) do begin
        if CharInSet( Str[i], Delims ) then begin
            Result := Copy(Str, 1, i - 1);
            Exit;
        end;
    end;
    Result := Str;
end;


function StrFilterChars(const Str : string; FilterSet : TSysCharSet) : string;
    //----------------------------------------------------------------------------------------------------------------------
    //Filtra/elimina todos os caracteres nao contidos no conjunto passado
    //*StrFilterChars - Returns characters as specified by aSet. (Purges characters not in set)
    // Example: StrFilterChars('123Hallo',['a'..'z']) > 'allo'
    // Example: StrFilterChars('123Hallo',['0'..'9']) > '123'
    //See also CountChars, Purge
var
    i : integer;
begin
    Result := EmptyStr;
    for i := 1 to Length(Str) do begin
        if CharInSet( Str[i], FilterSet ) then begin
            Result := Result + Str[i];
        end;
    end;
end;


function StrLastPos(const SubStr, Str : string) : integer;
    //----------------------------------------------------------------------------------------------------------------------
    //posicao da ultima ocorrenia de SubStr em Str
var
    P, LPos, Start : PChar;
begin
    LPos := NIL;
    Start := PChar(Str);
    P := Start;
    while Assigned(P) do begin
        P := StrPos(Start, PChar(SubStr));
        if Assigned(P) then begin
            LPos := P;
        end;
        Start := P + Length(SubStr);
    end;
    if Assigned(LPos) then begin
        Result := (Integer(LPos) - Integer(PChar(Str)));
    end else begin
        Result := 0;
    end;
end;

function StrLastWord(const Str : string; Delims : TSysCharSet = DEFAULT_WORD_DELIMITERS_LIST) : string;
    //----------------------------------------------------------------------------------------------------------------------
    //Retorna a ultima palara de Str
    //Example: LastWord('Delphi programming is fun') > 'fun'
    //Example: LastWord('Delphiprogramming') > ''
    //See also FirstWord
var
    i : integer;
begin
    for i := Length(Str) downto 1 do begin
        if CharInSet( Str[i] , Delims ) then begin
            Result := Copy(Str, i + 1, Length(Str));
            Exit;
        end;
    end;
    Result := Str;
end;

function StrHasLowCaseChar(const S : string) : boolean;
    //----------------------------------------------------------------------------------------------------------------------
    //HasLoCase - Returns true if a string contains lowercase characters.
    // Example: HasLoCase('Hello') > true
    // ANSI compatible
var
    i : integer;
begin
    Result := TRUE;
    for i := 1 to Length(S) do begin
        if S[i] <> UpCase(S[i]) then begin
            Exit; //Evita atribuicao para false
        end;
    end;
    Result := FALSE;
end;

function StrHasUpCaseChar(const S : string) : boolean;
    //----------------------------------------------------------------------------------------------------------------------
    //HasLoCase - Returns true if a string contains uppercase characters.
    // Example: HasLoCase('Hello') > true
    // ANSI compatible
var
    i : integer;
begin
    Result := TRUE;
    for i := 1 to Length(S) do begin
        if S[i] = UpCase(S[i]) then begin
            Exit; //Evita atribuicao para false
        end;
    end;
    Result := FALSE;
end;

function StrInNumRange(Str : string; MinVal, MaxVal : double) : boolean;
{
Usar TStrHnd.IsRangeValue
}
var
    Val : extended;
begin
    try
        Val := StrToFloat(Str);
        if (Val >= MinVal) and (Val <= MaxVal) then begin
            Result := TRUE;
        end else begin
            Result := FALSE;
        end;
    except
        Result := FALSE;
    end;
end;

function StrNPos(const SubStr : string; Str : string; Many : integer) : integer;
    //Retorna a posicao da enesima ocorrencia de SubStr em Str
    {-------------------------------------------------------------------------------------------------------------}
var
    n : integer;
begin
    Result := 0;
    repeat
        n := Pos(SubStr, Str);
        Delete(Str, 1, n);
        Inc(Result, n);
        Dec(Many);
    until (n = 0) or (Many = 0);
end;

function StrPadC(const Str : string; Size : integer; C : Char = ' ') : string;
    //----------------------------------------------------------------------------------------------------------------------
    //Retorna string centralizada no tamanaho passado complementando com o caracter indicado
var
    LS, LSide : integer;
begin
    SetLength(Result, Size);
    LS := Length(Str);
    LSide := Abs(LS - Size) div 2;
    if LS > Size then begin //Truncamento pelo centro de Str
        Result := Copy(Str, LSide, LS - LSide - 1);
    end else begin //Complementacao normal + resto a direita se necessario
        Result := StringOfChar(C, LSide) + Str + StringOfChar(C, LSide) + StringOfChar(C, Abs(LS - Size) mod 2);
    end;
end;

function StrPosChar(const s : string; const CharSet : TSysCharSet; StartPos : cardinal = 0) : integer;
    //----------------------------------------------------------------------------------------------------------------------
    //Localiza primeira ocorrencia de um dos caracteres presentes na lista
    //CharSet version - returns the position of the first character found in aSet.
    //Note that StartPos 1 and 0 are functionally the same.
    //  Example:
    //  Pos(['a'..'z'],'the man there') > 1
    //  Pos(['a'..'z'],'THE man there') > 5
var
    i : integer;
begin
    Result := 0;
    if StartPos = 0 then begin
        StartPos := 1;
    end;
    for i := StartPos to Length(S) do begin
        if CharInSet( S[i] , CharSet ) then begin
            Result := i;
            Break;
        end;
    end;
end;

function StrPosEx(const SubStr, Str : string; StartPos : Cardinal = 0; IgnoreCase : boolean = FALSE) : integer;
    //----------------------------------------------------------------------------------------------------------------------
    //Retorna posicao de SubStr em Str iniciando a pesquisa em StartPos e podendo ignorar caixa
begin
    if StartPos = 0 then begin
        if IgnoreCase then begin
            Result := System.Pos(AnsiUpperCase(SubStr), AnsiUpperCase(Str));
        end else begin
            Result := System.Pos(SubStr, Str);
        end;
    end else begin
        if IgnoreCase then begin
            Result := System.Pos(AnsiUpperCase(SubStr), AnsiUpperCase(System.Copy(Str, StartPos, Length(Str))));
        end else begin
            Result := System.Pos(SubStr, System.Copy(Str, StartPos, Length(Str)));
        end;
        if Result > 0 then begin
            Result := (Result + Integer(StartPos)) - 1;
        end;
    end;
end;

function StrPurge(const Str : string; ASet : TSysCharSet) : string;
    //----------------------------------------------------------------------------------------------------------------------
    //Remove todos os caracteres de Str que existam no conjunto passado
    //*Purge - Removes all characters from s that are contained in aSet.
    // Example: Purge('helloWORLD123Hello',['a'..'z']) --> 'WORLD123H'
    // Example: Purge('helloWORLD123Hello',['A'..'z']) --> '123'
    //See also overloaded char version, GetChars
var
    i : integer;
begin
    Result := EmptyStr;
    for i := 1 to Length(Str) do begin
        if not CharInSet(Str[i] , ASet) then begin
            Result := Result + Str[i];
        end;
    end;
end;

function StrReplaceFirst(OldStr, NewStr, SrcStr : string; var StartAt : integer; Wholeword, CaseSensitive : boolean;
    Delims : TSysCharSet = DEFAULT_WORD_DELIMITERS_LIST) : string;
    //----------------------------------------------------------------------------------------------------------------------
    // The function starts at position StartAt and searches for the first occurence of OldStr in SrcStr and replaces it
    // with NewStr.
    // If the FindStr was found StartAt will be advanced else it will be 0.
    // Example:
    // p := 1;
    // s := ReplaceFirst('dog','cat','The dog sleeps',P, true, false, [' ']) will GetChars 'The cat sleeps';
    //................................................................................................................................
    function Whole : boolean;
    var
        x : integer;
    begin
        Result := TRUE;
        if WholeWord = FALSE then begin
            Exit;
        end;
        if StartAt > 1 then begin
            if not (CharInSet(SrcStr[StartAt - 1] , Delims) or (Delims = [])) then begin
                Result := FALSE;
                Exit;
            end;
        end;
        x := StartAt + Length(OldStr);
        if (x <= Length(SrcStr)) then begin
            if not (CharInSet( SrcStr[x] , Delims) or (Delims = [])) then begin
                Result := FALSE;
                Exit;
            end;
        end;
    end;
    //................................................................................................................................
var
    DoIt : boolean;
begin
    if StartAt = 0 then begin
        StartAt := 1;
    end;
    repeat
        doit := FALSE;
        StartAt := StrPosEx(OldStr, SrcStr, StartAt, CaseSensitive);
        //StartAt := StrPosEx( AnsiUpperCase(FindStr),AnsiUpperCase(SrcStr),StartAt, CaseSensitive);
        if StartAt > 0 then begin
            DoIt := Whole();
            if not DoIt then begin
                Inc(StartAt);
            end;
        end;
    until (StartAt = 0) or (DoIt);
    if (DoIt) and (StartAt > 0) then begin
        Result := System.Copy(SrcStr, 1, StartAt - 1) + NewStr + System.Copy(SrcStr, StartAt + Length(OldStr), $FFF0);
        StartAt := StartAt + Length(NewStr);
    end else begin
        Result := SrcStr;
        StartAt := 0;
    end;
end;

function StrRScanStr(const SubStr, Str : PChar) : PChar;
    //----------------------------------------------------------------------------------------------------------------------
    //Retorna um PChar para o inicio da ultima ocorrencia de SubStr em Str
    //Faz de conta que nao fui eu quem escreveu isto
var
    Tmp : PChar;
begin
    Result := NIL;
    Tmp := Str;
    repeat
        Tmp := StrPos(Tmp, SubStr);
        if (Tmp <> NIL) then begin
            Result := Tmp;
            if StrLen(Tmp) > 1 then begin
                Inc(Tmp);
            end else begin
                Tmp := NIL;
            end;
        end;
    until Tmp = NIL;
end;

function StrToIntFilter(const Str : string) : integer;
    //----------------------------------------------------------------------------------------------------------------------
var
    EPos : integer;
begin
    System.Val(StrFilterChars(Str, INTEGER_CHARS_LIST), Result, EPos);
    if EPos <> 0 then begin
        raise EConvertError.CreateFmt('%s não pode ser convertido para um inteiro válido', [Str]);
    end;
end;

function StrSysCharSetToString(const ASet : TSysCharSet; DoRange : boolean = TRUE) : string;
    //----------------------------------------------------------------------------------------------------------------------
    //StrSysCharSetToString - Converts a CharSet to a string in a visual presentation form.
    //
    //NOTE that only chars in gcCharSetVisualRange will be represented as literal values.
    //Because special characters like control characters, space and other extended
    //characters are not easy to identify in a string, or depends visually on the
    //system locale they are presended in their byte value with a hash prefix.
    //Therefore the escape character will be #27.
    //
    // The result is similar to inspecting a set of chars in the integrated debugger.
    // Example: StrSysCharSetToString(['A'..'Z',#13,#12,#11,#9],true) > '#9,#11..#13,A..Z'
    // Example: StrSysCharSetToString(['A'..'D',false) > 'A,B,C,D'
    // See also StrToSysCharSet
var
    i : Char;
    f, l : SmallInt;
    //................................................................................................................................
    function LSRGetS(b : byte) : string;
    begin
        if CharInSet( Char(b) , VISUAL_CHARS_LIST ) then        begin
            Result := Char(b);
        end else begin
            Result := '#' + inttostr(b);
        end;
    end;
    //................................................................................................................................
    procedure LSRAdd();
    begin
        if F = -1 then        begin
            Exit;
        end;
        if F = L then begin
            if Result = EmptyStr then begin
                Result := LSRGets(F);
            end else begin
                Result := Result + ',' + LSRGets(F);
            end;
        end else begin
            if Result = EmptyStr then begin
                Result := LSRGets(f) + '..' + LSRGetS(l);
            end else begin
                Result := Result + ',' + LSRGets(f) + '..' + LSRGetS(l);
            end;
        end;
    end;
    //................................................................................................................................
begin
    Result := EmptyStr;
    F := -1;
    for i := #0 to #255 do begin
        if DoRange then begin
            if CharInSet( i , ASet ) then begin
                if F = -1 then begin
                    F := Byte(i);
                end;
                L := Byte(i);
            end else begin
                LSRAdd();
                F := -1;
            end;
        end else begin
            if CharInSet( i , aSet ) then begin
                F := Byte(i);
                L := f;
                LSRAdd();
                F := -1;
            end;
        end;
    end;
    LSRAdd();
end;

procedure StrTabifyStrings(Str : TStrings);
//----------------------------------------------------------------------------------------------------------------------------------
var
    i : integer;
begin
    for i := 0 to Str.Count - 1 do begin
        Str[i] := #9 + Str[i];
    end;
end;

function StrToBool(const Str : string) : boolean;
    //----------------------------------------------------------------------------------------------------------------------
    //Converte um valor para um logico
var
    UC : string;
begin
    UC := UpperCase(Str);
    Result := (UC = 'SIM') or (UC = 'S') or (UC = 'YES') or (UC = 'ON');
end;

function StrToCurrency(Str : string) : Currency;
    //----------------------------------------------------------------------------------------------------------------------
    //Converte string para currency
var
    DotSep, SemiCSep : PChar;
    OldDecimalSeparator : char;
begin
    DotSep := StrRScan(PChar(Str), '.');
    SemiCSep := StrRScan(PChar(Str), ',');
    if ((DWORD(DotSep) or DWORD(SemiCSep)) <> $0) then begin //Existe separador
        OldDecimalSeparator := DecimalSeparator; //***Caso outro thread alterar esse valor UM ABRACO !!!!!!!
        if LongInt(DotSep) > LongInt(SemiCSep) then begin //Assume que o separador sera ponto
            try
                DecimalSeparator := '.';
                StringReplace(Str, ',', EmptyStr, [rfReplaceAll]);
                Result := StrToCurr(Str);
            finally
                DecimalSeparator := OldDecimalSeparator;
            end;
        end else begin //Separador sera virgula
            try
                DecimalSeparator := ',';
                StringReplace(Str, '.', EmptyStr, [rfReplaceAll]);
                Result := StrToCurr(Str);
            finally
                DecimalSeparator := OldDecimalSeparator;
            end;
        end;
    end else begin //Converte direto
        Result := StrToCurr(Str);
    end;
end;

function StrToFloat2(Str : string) : Extended;
    //----------------------------------------------------------------------------------------------------------------------
    //Converte string para float
var
    DotSep, SemiCSep : PChar;
    OldDecimalSeparator : char;
begin
    DotSep := StrRScan(PChar(Str), '.');
    SemiCSep := StrRScan(PChar(Str), ',');
    if ((DWORD(DotSep) or DWORD(SemiCSep)) <> $0) then begin //Existe separador
        OldDecimalSeparator := DecimalSeparator; //***Caso outro thread alterar esse valor UM ABRACO !!!!!!!
        if LongInt(DotSep) > LongInt(SemiCSep) then begin //Assume que o separador sera ponto
            try
                DecimalSeparator := '.';
                StringReplace(Str, ',', EmptyStr, [rfReplaceAll]);
                Result := StrToFloat(Str);
            finally
                DecimalSeparator := OldDecimalSeparator;
            end;
        end else begin //Separador sera virgula
            try
                DecimalSeparator := ',';
                StringReplace(Str, '.', EmptyStr, [rfReplaceAll]);
                Result := StrToFloat(Str);
            finally
                DecimalSeparator := OldDecimalSeparator;
            end;
        end;
    end else begin //Converte direto
        Result := StrToFloat(Str);
    end;
end;

function StrToFloatFilter(const Str : string) : double;
{
Remove caracteres invalidos antes dee converter para um float
}
begin
	Result := StrToFloat2(StrFilterChars(Str, FLOAT_CHARS_LIST));
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function StrToPChar(Str : string) : PChar;
{
Aloca e copia a string para um PChar ( Usar TStrHnd )
}
var
	Size : integer;
begin
{$IFDEF WIN32}
	Size := Length(Str) + SizeOf(Cardinal) + 1;
	//    GetMem(Result, Size);
	//    Cardinal(Pointer(Result)^) := Size;
	//    Inc(Result, SizeOf(Cardinal));
	Result := StrAlloc(Size);
	StrPCopy(Result, Str);
{$ELSE}
	Size := Length(Str) + SizeOf(Integer) + 1; {buffer + tamanho + /0}
	GetMem(Result, Size);
	Integer(Pointer(Result)^) := Size;
	Inc(Result, 2);
	StrPCopy(Result, Str);
{$ENDIF}
end;

function StrToPoint(Str : string; Delimiter : char = ',') : TPoint;
    //----------------------------------------------------------------------------------------------------------------------
    //Converte string em coordenadas 2D
const
    _MSG_INVALID_VALUE_ = '"%s" não é um valor válido';
var
    Part : string;
    ECode, DelimPos : integer;
begin
    Result.x := 0;
    Result.y := Result.x;
    if Length(Str) < 3 then begin
        raise Exception.CreateFmt(_MSG_INVALID_VALUE_, [Str]);
    end;
    //Remove simbolo construtor
    if CharInSet( Str[1] ,  [')', ']'] ) then begin
        Delete(Str, 1, 1);
    end;
    if CharInSet( Str[Length(Str)] , [')', ']'] )then begin
        Delete(Str, Length(Str) - 1, 1);
    end;
    //Separa as partes e numeriza
    DelimPos := Pos(Delimiter, Str);
    if DelimPos = 0 then begin
        Val(Str, Result.x, ECode);
        if ECode <> 0 then begin
            raise Exception.CreateFmt(_MSG_INVALID_VALUE_, [Str]);
        end;
        Exit;
    end;
    Part := Copy(Str, 1, DelimPos - 1);
    Val(Part, Result.x, ECode);
    if ECode <> 0 then begin
        raise Exception.CreateFmt(_MSG_INVALID_VALUE_, [Str]);
    end;
    Part := Copy(Str, DelimPos + 1, Length(Str));
    Val(Part, Result.y, ECode);
    if ECode <> 0 then begin
        raise Exception.CreateFmt(_MSG_INVALID_VALUE_, [Str]);
    end;
end;

function StrToSysCharSet(Str : string) : TSysCharSet;
    //----------------------------------------------------------------------------------------------------------------------
var
    t1, t2 : string;
    //................................................................................................................................
    function LSRGetc(t : string) : char;
    begin
        Result := #0;
        if t <> EmptyStr then begin
            if t[1] = '#' then begin
                Result := Char(StrToIntFilter(copy(t, 2, length(t))));
            end else begin
                Result := t[1];
            end;
        end;
    end;
    //................................................................................................................................
    procedure LSRDoIt();
    var
        a, b : char;
        x : char;
    begin
        a := LSRGetc(t1);
        b := LSRGetc(t2);
        if b = #0 then begin
            Result := Result + [a];
        end else begin
            for x := a to b do begin
                Result := Result + [x];
            end;
        end;
    end;
    //................................................................................................................................
begin
    Result := [];
    while Str <> EmptyStr do begin
        t1 := AllTrim(RemoveDupChar(StrCopyBefore(',', Str), ' '));
        if t1 <> EmptyStr then begin
            t2 := AllTrim(RemoveDupChar(StrCopyAfter('..', t1), ' '));
            t1 := AllTrim(RemoveDupChar(StrCopyBefore('..', t1), ' '));
            LSRDoIt;
        end;
        Str := StrCopyAfter(',', Str);
    end;
end;

function StrTrimChar(Str : string; Caracter : Char; Position : TTrimPos) : string;
    //----------------------------------------------------------------------------------------------------------------------
var
    Chars, i : integer;
begin
    if (Position = tpTrimRight) or (Position = tpTrimAll) then begin
        Chars := 0;
        for i := Length(Str) downto 1 do begin
            if Str[i] <> Caracter then begin
                break;
            end;
            Inc(Chars);
        end;
        Delete(Str, Length(Str) - Chars + 1, Chars);
    end;
    if (Position = tpTrimLeft) or (Position = tpTrimAll) then begin
        Chars := 0;
        for i := 1 to Length(Str) do begin
            if Str[i] <> Caracter then begin
                break;
            end;
            Inc(Chars);
        end;
        Delete(Str, 1, Chars);
    end;
    StrTrimChar := Str;
end;


function StrTrimTabs(Str : string; Position : TTrimPos) : string;
    {-------------------------------------------------------------------------------------------------------------}
begin
    Result := StrTrimChar(Str, #9, Position);
end;

function StrWordPos(WordStr : string; Str : string; Delims : TSysCharSet = [' ']; StartPos : Cardinal = 0) : cardinal;
    //----------------------------------------------------------------------------------------------------------------------
    //Retorna indice onde se encontra a palavra dada por SubStr em Str
var
    SLen, WLen, EndPos : cardinal;
begin
    Result := StartPos; //Faz papel de posicao inicial
    WLen := Length(WordStr);
    SLen := Length(Str);
    repeat
        Inc(Result);
        Result := StrPosEx(WordStr, Str, Result); //Localiza inicio da palavra a partir de StartPos
        EndPos := Result + WLen;
        if (EndPos > SLen) or (Result = 0) then begin //Palavra nao encontrada ou no fim da string
            Exit;
        end;
        if (Result = 1) and CharInSet( Str[EndPos] , Delims) then begin //Palavra no inicio da string
            Exit;
        end;
        //Posicao seguinte/anterior a palavra localizada pertence aos delimitadores
    until CharInSet( Str[EndPos] , Delims) and CharInSet( Str[Result - 1] , Delims);
end;


function StrUnindent(Str : string) : string;
    {-------------------------------------------------------------------------------------------------------------}
begin
    Result := Str;
    repeat
        Str := Result;
        Result := StrTrimTabs(Result, tpTrimAll);
        Result := StrTrimChar(Result, ' ', tpTrimAll);
        Result := StrTrimChar(Result, #255, tpTrimAll);
    until (Result = Str);
end;



function TruncMidleFileName(const FileName : string) : string;
    {-------------------------------------------------------------------------------------------------------------}
var
    PrePoint : integer;
    Radix, FileId, Meta : string;
begin
    if Pos('\\', FileName) <> 0 then begin //Indica uma UNC/URL
        Radix := Copy(FileName, 1, StrNPos('\', FileName, 3) - 1);
    end else begin
        Radix := Copy(FileName, 1, 3);
    end;
    FileId := ExtractFileName(FileName);
    PrePoint := Length(Radix);
    Meta := Copy(FileName, PrePoint + 1, Length(FileName) - PrePoint - Length(FileId));
    if Meta <> EmptyStr then begin //Nome reduzido ao maximo no estado normal
        Meta := ReplaceSubString(Meta, '\...\', EmptyStr); //Elimina resultado de chamadas anteriores a esta funcao
        //Meta:=ReplaceSubString( Meta, '..', EmptyStr ); pode-se pensar em reduzir para 2 pontos apenas nesta funcao
        if (Meta <> EmptyStr) and (Meta <> '\') then begin
            PrePoint := StrCountRepet('\', Meta);
            if PrePoint > 1 then begin
                PrePoint := PrePoint div 2;
                Result := GetDelimitedSubStr('\', Meta, PrePoint);
                Meta := ReplaceSubString(Meta, '\' + Result + '\', '\...\');
            end else begin
                Meta := '...\';
            end;
        end else begin
            if Meta <> '\' then begin
                Meta := '\...\';
            end;
        end;
    end else begin
        Meta := EmptyStr;
    end;
    Result := Radix + Meta + FileId;
end;

function UsedCharSet(const CharSet, Text : string) : string;
    //----------------------------------------------------------------------------------------------------------------------
    //Retorna o conjunto de caracteres usados no texto que pertencam ao conjunto dado
var
    i, L, P : integer;
begin
    L := Length(CharSet);
    Result := EmptyStr;
    for i := 1 to L do begin
        P := Pos(CharSet[i], Text);
        if P <> 0 then begin
            Result := Result + CharSet[i];
        end;
    end;
end;

{----------------------------------------------------------------------------}

{ TODO -oRoger -cLIB : Verficar uso destas rotinas }
(*
//==================================================
//All code herein is copyrighted by
//Peter Morris
//-----
//No copying, alteration, or use is permitted without
//prior permission from myself.
//------
//Do not alter / remove this copyright notice
//Email me at : MrPMorris@hotmail.com
//==================================================
unit FastStrings;

interface

uses
   Windows, SysUtils;

function FastPos(const aSourceString, aFindString : String; const aSourceLen, aFindLen, StartPos : integer) : integer;
function FastPosNoCase(const aSourceString, aFindString : String; const aSourceLen, aFindLen, StartPos : integer) : integer;
function FastReplace(var aSourceString : String; const aFindString, aReplaceString : String; CaseSensitive : Boolean = False) : String;
implementation

//This TYPE declaration will become apparent later
Type
  TFastPosProc = function(const aSourceString, aFindString : String; const aSourceLen, aFindLen, StartPos : integer) : integer;


//The first thing to note here is that I am passing the SourceLength and FindLength
//As neither Source or Find will alter at any point during FastReplace there is
//no need to call the LENGTH subroutine each time !
function FastPos(const aSourceString, aFindString : String; const aSourceLen, aFindLen, StartPos : integer) : integer;
var
  SourceLen : integer;
begin
  //Next, we determine how many bytes we need to scan to find the "start" of
  //aFindString
  SourceLen := aSourceLen;
  SourceLen := SourceLen - aFindLen;
  if (StartPos-1) > SourceLen then begin
    Result := 0;
    Exit;
  end;
  SourceLen := SourceLen - StartPos;
  SourceLen := SourceLen +2;

  //Here starts the ASM
  asm
          // Delphi uses ESI, EDI and EBX a lot, so we must preserve them
          push ESI
          push EDI
          push EBX

          //Get the address of sourceString[1] and Add (StartPos-1)
          //we do this for the purpose of finding the NEXT occurence
          //rather than always the first !
          mov EDI, aSourceString
          add EDI, StartPos
          Dec EDI

          //Get the address of FindString
          mov ESI, aFindString

          //Note how many bytes we need to look through in aSourceString
          //to find aFindString
          mov ECX, SourceLen

          //Get the first char of aFindString, note how it is done outside
          //of the main loop, as it never changes !
          Mov  Al, [ESI]

          //Now the FindFirstCharacter loop !
    @ScaSB:
          //Get the value of the current character in aSourceString
          //This is equal to ah := EDI^, that is what the [] are around [EDI]
          Mov  Ah, [EDI]

          //compare this character with aDestString[1]
          cmp  Ah,Al
          //If they are not equal we dont compare the strings
           jne  @NextChar

          //If they are equal, obviously we do !
    @CompareStrings:
          //Put the length of aFindLen in EBX
          mov  EBX, aFindLen

          //We DEC EBX to point to the end of the string, ie, we dont
          //want to add 1 if aFindString is 1 in length !
          dec  EBX

          //here is another optimization tip !
          //People at this point usually PUSH ESI etc and then POP ESI etc
          //at the end, instead I opted not to change ESI etc at all.
          //This saves lots of pushing and popping !
    @CompareNext:

          //Get aFindString character + aFindStringLength (the last char)
          mov  Al, [ESI+EBX]

          //Get aSourceString character (current position + aFindStringLength)
          mov  Ah, [EDI+EBX]

          //Compare them
          cmp  Al, Ah
          Jz   @Matches

          //If they don't match, we put the first char of aFindString into
          //Al again to continue looking for the first character
          Mov  Al, [ESI]
          Jmp  @NextChar

    @Matches:
          //If they match we DEC EBX (point to previous character to compare)
          Dec  EBX
          //If EBX <> 0 ("J"ump "N"ot "Z"ero) we continue comparing strings
           Jnz  @CompareNext

          //If EBX is zero then we have successfully compared each character,
          //ie A MATCH !!!

          //Move the address of the *current* character in EDI
          //note, we have not altered EDI since the first char was found
          mov  EAX, EDI
          //This is an address, so subtract the address of aSourceString[1]
          //to get an actual character position
          sub  EAX, aSourceString
          //Inc EAX to make it 1 based, rather than 0 based
          inc  EAX
          //Put it into result
          mov  Result, EAX
          //Finish this routine !
          jmp  @TheEnd
    @NextChar:

          //This is where we jump to when we want to continue searching for the
          //first character of aFindString in aSearchString

          //Point EDI (aFindString[X]) to the next character
          Inc  EDI
          //Dec ECX tells us we have checked another character, and that we are
          //fast running out of string to check !
          dec  ECX
          //If EBX <> 0 then continue scanning for the first character
          jnz  @ScaSB

          //If EBX = 0 then move 0 into RESULT
          mov  Result,0

          //Restore EBX, EDI, ESI for delphi to work correctly,
          //note, they are POPped in the opposite order they were PUSHed
    @TheEnd:
           pop  EBX
          pop  EDI
          pop  ESI
  end;
end;

//This routine is an identical copy of FastPOS *except* where commented !
//The idea is that when grabbing bytes, it ANDs them with $df
//effectively making them lowercase before comparing.
//Maybe this would be quicker if aFindString was made lowercase in 1 foul
//swoop at the beginning of the function, saving an AND instruction each time
//(I think I will try when I have finished this article)
function FastPosNoCase(const aSourceString, aFindString : String; const aSourceLen, aFindLen, StartPos : integer) : integer;
var
  SourceLen : integer;
begin
  SourceLen := aSourceLen;
  SourceLen := SourceLen - aFindLen;
  if (StartPos-1) > SourceLen then begin
    Result := 0;
    Exit;
  end;
  SourceLen := SourceLen - StartPos;
  SourceLen := SourceLen +2;
  asm
          push ESI
          push EDI
          push EBX

          mov EDI, aSourceString
          add EDI, StartPos
          Dec EDI
          mov ESI, aFindString
          mov ECX, SourceLen

          Mov  Al, [ESI]

          //Make Al lowercase
          and  Al, $df
    @ScaSB:
          Mov  Ah, [EDI]
          //Make Ah lowercase
          and  Ah, $df
          cmp  Ah,Al
          jne  @NextChar

    @CompareStrings:
          mov  EBX, aFindLen
          dec  EBX
    @CompareNext:
          mov  Al, [ESI+EBX]
          mov  Ah, [EDI+EBX]
          //Make Al, and Ah lowercase
          and  Al, $df
          and  Ah, $df
          cmp  Al, Ah
          Jz   @Matches
          Mov  Al, [ESI]
          //Make Al lowercase
          and  Al, $df
          Jmp  @NextChar
    @Matches:
          Dec  EBX
          Jnz  @CompareNext
          mov  EAX, EDI
          sub  EAX, aSourceString
          inc  EAX
          mov  Result, EAX
          jmp  @TheEnd
    @NextChar:
          Inc  EDI
          dec  ECX
           jnz  @ScaSB
          mov  Result,0

    @TheEnd:
          pop  EBX
          pop  EDI
          pop  ESI
  end;
end;

//My move is not as fast as MOVE when source and destination are both
//DWord aligned, but certainly faster when they are not.
//As we are moving characters in a string, it is not very likely at all that
//both source and destination are DWord aligned, so moving bytes avoids the
//cycle penality of reading/writing DWords across physical boundaries
procedure MyMove(const Source; var Dest; Count : Integer);
asm
//Note:  When this function is called, delphi passes the parameters as follows
//ECX = Count
//EAX = Const Source
//EDX = Var Dest

        //If no bytes to copy, just quit altogether, no point pushing registers
        cmp   ECX,0
        Je    @JustQuit

        //Preserve the critical delphi registers
        push  ESI
        push  EDI

        //move Source into ESI  (generally the SOURCE register)
        //move Dest into EDI (generally the DEST register for string commands)
        //This may not actually be neccessary, as I am not using MOVsb etc
        //I may be able just to use EAX and EDX, there may be a penalty for
        //not using ESI, EDI but I doubt it, this is another thing worth trying !
        mov   ESI, EAX
         mov   EDI, EDX

        //The following loop is the same as repNZ MovSB, but oddly quicker !
    @Loop:
        //Get the source byte
        Mov   AL, [ESI]
        //Point to next byte
        Inc   ESI
        //Put it into the Dest
        mov   [EDI], AL
        //Point dest to next position
        Inc   EDI
        //Dec ECX to note how many we have left to copy
        Dec   ECX
        //If ECX <> 0 then loop
        Jnz   @Loop

        //Another optimization note.
        //Many people like to do this

        //Mov AL, [ESI]
        //Mov [EDI], Al
        //Inc ESI
        //Inc ESI

        //There is a hidden problem here, I wont go into too much detail, but
        //the pentium can continue processing instructions while it is still
        //working out the desult of INC ESI or INC EDI
        //(almost like a multithreaded CPU)
        //if, however, you go to use them while they are still being calculated
        //the processor will stop until they are calculated (a penalty)
        //Therefore I alter ESI and EDI as far in advance as possible of using them

        //Pop the critical Delphi registers that we have altered
        pop   EDI
        pop   ESI
     @JustQuit:
end;

//Point 1
//I pass VAR aSourceString rather than just aSourceString
//This is because I will just be passed a pointer to the data
//rather than a 10mb copy of the data itself, much quicker !
function FastReplace(var aSourceString : String; const aFindString, aReplaceString : String; CaseSensitive : Boolean = False) : String;
var
  ActualResultLen,      //Size already passed to SetLength, the REAL size of RESULT
  CurrentPos,           //Position of aFindString is aSourceString
  LastPos,              //Last position the aFindString was found at
  BytesToCopy,          //Bytes to copy (ie, lastpos to this pos)
  ResultLen,            //The "running" result length, not the actual one
  FindLen,              //Length of aFindString, to save calling LENGTH repetitively
  ReplaceLen,           //Length of aReplaceString, for the same reason
  SourceLen         : Integer;

  //This is where I explain the TYPE TFastPosProc from ealier !
  FastPosProc       : TFastPosProc;
begin
  //As this function has the option of being case-insensitive we would need
  //to call either FastPOS or FastPOSNoCase
  //The problem is that we would have to do this within a loop
  //This is a bad idea as the result never changes throughout the whole
  //opertaion
  //In which case we can determine it in advance, like so
  if CaseSensitive then
    FastPosProc := FastPOS
  else
    FastPOSProc := FastPOSNoCase;

  //I dont think I actually need this, but, I dont really mind !
  Result := '';

  //Get the lengths of the strings
  FindLen := Length(aFindString);
  ReplaceLen := Length(aReplaceString);
  SourceLen := Length(aSourceString);


  //If we already have room for the replacments, then set the length of the
  //result to the length of the SourceString
  if ReplaceLen <= FindLen then
    ActualResultLen := SourceLen
  else
    //If not, we need to calculate the worst case scenario
    //Ie, the Source consists ONLY of aFindString and we are going to replace
    //every one of them !
    ActualResultLen := SourceLen + (SourceLen * ReplaceLen div FindLen) + ReplaceLen;

  //Set the length of Result, this will assign the memory etc
  SetLength(Result,ActualResultLen);

  CurrentPos := 1;
  ResultLen := 0;
  LastPos := 1;

  //Again I am eliminating an IF statement in a loop by repeating code,
  //this approach reaults in *very* slightly larger code, but if ever you can
  //trade some memory in exchange for speed, go for it !
  if ReplaceLen > 0 then begin
    repeat
      //Get the position of the first (or next) aFindString in aSourceString
      //Note there is no If CaseSensitive, I just call FastPOSProc which is
      //pointing to the correct pre-determined routine
      CurrentPos := FastPosProc(aSourceString, aFindString, SourceLen, FindLen, CurrentPos);

      //If 0 then we are finished
      if CurrentPos = 0 then break;

      //Number of bytes to copy from the source string is
       //CurrentPos - LastPos, ie " cat " in "the cat the"
      BytesToCopy := CurrentPos-LastPos;


      //Copy chars from aSourceString to the end of Result
      MyMove(aSourceString[LastPos],Result[ResultLen+1], BytesToCopy);
      //Copy chars from aReplaceString to the end of Result
      MyMove(aReplaceString[1], Result[ResultLen+1+BytesToCopy], ReplaceLen);
      //Remember, using COPY would copy all of the data over and over again.
      //Never fall into this trap (like a certain software company did)


      //Set the running length to
      ResultLen := ResultLen + BytesToCopy + ReplaceLen;

      //Set the position in aSourceString to where we want to continue searching
      //from
      CurrentPos := CurrentPos + FindLen;
      LastPos := CurrentPos;
    until false;
  end else begin
    //You may have noticed If ReplaceLen > 0
    //well, if ReplaceLen = 0 then we are deleting the substrings, rather than
    //replacing them, so we dont need the extra MyMove from aReplaceString
    repeat
      CurrentPos := FastPos(aSourceString, aFindString, SourceLen, FindLen, CurrentPos);
      if CurrentPos = 0 then break;

      BytesToCopy := CurrentPos-LastPos;

      MyMove(aSourceString[LastPos],Result[ResultLen+1], BytesToCopy);
      ResultLen := ResultLen + BytesToCopy + ReplaceLen;

      CurrentPos := CurrentPos + FindLen;
      LastPos := CurrentPos;
    until false;
  end;

  //Now we have finished doing all the replaces, we just need to adjust the
  //length of the final result.
  Dec(LastPOS);

  //Set the length to the Length + the bit of string left
  //ie " mat" when replacing "the" in "sat on the mat"
  SetLength(Result, ResultLen + (SourceLen-LastPos));

  //If there is a bit of string dangling, then add it to the end of our string
  if LastPOS+1 <= SourceLen then MyMove(aSourceString[LastPos+1],Result[ResultLen+1],SourceLen-LastPos);
end;
end.
*)
end.


