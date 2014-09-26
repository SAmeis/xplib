{$IFDEF MathHnd}
{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I XPLib.inc}
unit MathHnd;

interface

uses
  SysUtils, Math, Super;

function BaseConvert(const Number: string; SrcBase, DestBase: Int64): string;
function BaseNumToStr(num, len, base: Int64; neg: Boolean; fill: char): string;
function BaseStrToNum(const s: string; base: Integer; neg: Boolean; max: Int64 = MaxInt64): Int64;
function SafeDiv(Dividendo, Divisor: extended): extended;
function TruncPrecision(Value: extended; Precision: word): extended;

var
  { {
	Valor a ser comparado como sendo desprezivel ( equivalente a ZERO ) para efeito de minizar erros de ponto flutuante
  }
  XPEpsilon: extended = 1E-23;

implementation

const
  MinBase = 2;
  MaxBase = 36;

function BaseConvert(const Number: string; SrcBase, DestBase: Int64): string;
// ----------------------------------------------------------------------------------------------------------------------------------
var
  i: Int64;
begin
  i := BaseStrToNum(Number, SrcBase, FALSE, Int64($FFFFFFFFFFFFFFF));
  Result := BaseNumToStr(i, 0, DestBase, FALSE, ' ');
end;

function BaseNumToStr(num, len, base: Int64; neg: Boolean; fill: char): string;
// ----------------------------------------------------------------------------------------------------------------------
// num = the number to convert
// len = minimum length of the resulting string
// base = numeric base 2 = bin, 8 = octal, 10 = dec, 16 = hex
// neg = if true num is treated as negative number
// fill = character that ist used as fill in to get a string
// of the length len
//
// Example:
// NumToStr (45, 8, 2, false, '0') > '00101101'
// NumToStr (45, 4, 8, false, '0') > '0055'
// NumToStr (45, 4, 10, false, ' ') > ' 45'
// NumToStr (45, 4, 16, false, '0') > '002D'
// NumToStr (45, 0, 36, false, ' ') > '19'
//
var
  s: string;
  digit: Integer;
begin
  num := ABS(num);
  if ((base >= MinBase) and (base <= MaxBase)) then begin
	s := EmptyStr;
	repeat
	  digit := num mod base;
	  if digit < 10 then begin
		Insert(CHR(digit + 48), s, 1);
	  end else begin
		Insert(CHR(digit + 55), s, 1);
	  end;
	  num := num div base;
	until (num = 0);
	if neg then begin
	  Insert('-', s, 1);
	end;
	while Length(s) < len do begin
	  Insert(fill, s, 1);
	end;
  end;
  Result := s;
end;

function BaseStrToNum(const s: string; base: Integer; neg: Boolean; max: Int64 = MaxInt64): Int64;
// ----------------------------------------------------------------------------------------------------------------------
// s = the string containing the number
// base = numeric base that is expected
// neg = string maybe contains ''-'' to show if its < 0
// max = maximum number that can be containd (normally MaxInt)
//
// Example:
// i:= StrToNum (''00101101'', 2, false, MaxInt);
// i:= StrToNum (''002D'', 16, false, MaxInt);
// i:= StrToNum (''-45'', 10, true, MaxInt);
// i:= StrToNum (''ZZ'', 36, true, MaxInt);
//
var
  negate, done: Boolean;
  i, len, digit: Integer;
  c: char;
  mmb, mdb, res: Int64;
begin
  res := 0;
  i := 1;
  digit := 0;
  if (base >= MinBase) and (base <= MaxBase) then begin
	mmb := max mod base;
	mdb := max div base;
	len := Length(s);
	negate := FALSE;
	while (i <= len) and (s[i] = ' ') do begin
	  Inc(i);
	end;
	if neg then begin
	  case s[i] of
		'+': begin
			Inc(i);
		  end;
		'-': begin
			Inc(i);
			negate := TRUE;
		  end;
	  end; (* CASE *)
	end; (* IF neg *)
	done := len >= i;
	while (i <= len) and done do begin
	  c := Upcase(s[i]);
	  case c of
		'0' .. '9': begin
			digit := ORD(c) - 48;
		  end;
		'A' .. 'Z': begin
			digit := ORD(c) - 55;
		  end;
	  else begin
		  done := FALSE;
		end
	  end; (* CASE *)
	  done := done and (digit < base);
	  if done then begin
		done := (res < mdb) or ((res = mdb) and (digit <= mmb));
		if done then begin
		  res := res * base + digit;
		  Inc(i);
		end; (* IF done *)
	  end; (* IF done *)
	end; (* WHILE *)
	if negate then begin
	  res := -res;
	end;
  end; (* IF done *)
  Result := res;
end;

function SafeDiv(Dividendo, Divisor: extended): extended;
// ----------------------------------------------------------------------------------------------------------------------------------
begin
  if Divisor = 0 then begin
	Result := 0;
  end else begin
	Result := (Dividendo / Divisor);
  end;
end;

function TruncPrecision(Value: extended; Precision: word): extended;
{ {
  Ver Tambem rotina em Math.RoundTo() e corrigir necessidade da exclusão do Range checking

  Revision: 5/7/2005
}
begin
  {$R+}
  Result := IntPower(10, Precision) * Value;
  Result := Round(Result);
  Result := (Result) / IntPower(10, Precision);
  {$R-}
end;

end.
