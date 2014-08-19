unit rjString;
{
File:           rjString.pas
Version:        0.1 / Initial Public Release
Last Modified:  1. April 2000
Decription:     Various string handling routines. See source for details.
                Some comments are in German. Translation upon request in next
                version.
Author:         Ralf Junker
E-Mail:         ralfjunker@gmx.de
Legal:          This unit is provided "as is" and without warranty of any kind.
                At the moment, it is freeware for private use only.
                I might, however, change this in the future. If you intend
                to use it commercially, please contact me via e-mail.
Bug Report:     If you encounter any bugs or want to suggest improvements,
                just get in touch using my e-mail above. When writing, please
                include the name and version of the unit you are referring to.

Copyright (c) 2000 Ralf Junker
}

interface

{$I rj.inc}

uses
{$IFDEF RangeChecking}
 SysUtils,
{$ENDIF}
 Windows,
 rjBase;

function StrLen (tStr: PChar): Integer;

function PCompStr (const p1, p2: PChar): Integer;
function PCompText (const p1, p2: PChar): Integer;

function CompStr (const s1, s2: AnsiString): Integer;
function CompText (const s1, s2: AnsiString): Integer;

function GetLastErrorString (const MessageID: DWord): AnsiString;

function LoadStringFromResource (const Instance: THandle; const ResourceName: AnsiString): AnsiString;

procedure AppendChar (var s: AnsiString; const c: AnsiChar);
{ Adds a character to a string. }

function CapitalizeWords (const s: AnsiString): AnsiString;

function IsEmptyString (const s: AnsiString): Boolean;

procedure SkipSet (var p: PChar; const Search: TCharSet);
procedure SkipSetBackwards (var p: PChar; const Search: TCharSet);

procedure SkipNotSet (var p: PChar; const Search: TCharSet);
procedure SkipNotSetBackwards (var p: PChar; const Search: TCharSet);

procedure SkipChar (var p: PChar; const Search: Char);
procedure SkipCharBackwards (var p: PChar; const Search: Char);
procedure SkipNotChar (var p: PChar; const Search: Char);

procedure LowerCaseBuffer (Run: PChar);
procedure LowerCase (var s: AnsiString); overload;
function LowerCaseFunc (const s: AnsiString): AnsiString;
procedure UpperCaseBuffer (Run: PChar);
procedure UpperCase (var s: AnsiString);
function UpperCaseFunc (const s: AnsiString): AnsiString;

procedure ReplaceStr (var Str: string; const sub1, sub2: string);

function PScanChar (const p: PChar; const c: Char): Integer;

function ScanS (Src: string; Search: TCharSet; const Start: Integer): Integer;
{ ScanS sucht das erste Zeichen, das im Set ist.
  R¸ckgabewerte: Position bei Erfolg, 0 wenn nicht gefunden oder Start zu groﬂ / klein.}

function ScanBNS (Src: string; Search: TCharSet; const Start: Integer): Integer;
{ ScanBS: Wie ScanNS, sucht jedoch r¸ckw‰rts}

function ScanBS (const Src: AnsiString; Search: TCharSet; const Start: Integer): Integer;
{ ScanBS: Wie ScanS, sucht jedoch r¸ckw‰rts}

//function ScanNS (Src: PChar; Search: TCharSet; const Start: Integer): Integer; overload;
function ScanNS (const Src: AnsiString; Search: TCharSet; const Start: Integer): Integer;
{ ScannNS sucht das erste Zeichen, das NICHT im Set ist.
  R¸ckgabewerte: Position bei Erfolg, 0 wenn nicht gefunden oder Start zu groﬂ / klein.}

function LongMonthName (const i: Integer): AnsiString;
{ Funktioniert auch mit LongMonthNames [i], in SysUtils definiert.
  Diese Funktionen nutzen also nur dann, wenn SysUtils nicht gebraucht wird. }
function ShortMonthName (const i: Integer): AnsiString;
function LongDayName (const i: Integer): AnsiString;
function ShortDayName (const i: Integer): AnsiString;

const
 
 UpperCharArray               : array[#0..#255] of Cardinal = (
  0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
  10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
  20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
  30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
  40, 41, 42, 43, 44, 45, 46, 47, 48, 49,
  50, 51, 52, 53, 54, 55, 56, 57, 58, 59,
  60, 61, 62, 63, 64, 65, 66, 67, 68, 69,
  70, 71, 72, 73, 74, 75, 76, 77, 78, 79,
  80, 81, 82, 83, 84, 85, 86, 87, 88, 89,
  90, 91, 92, 93, 94, 95, 96, 65, 66, 67,
  68, 69, 70, 71, 72, 73, 74, 75, 76, 77,
  78, 79, 80, 81, 82, 83, 84, 85, 86, 87,
  88, 89, 90, 123, 124, 125, 126, 127, 128, 129,
  130, 131, 132, 133, 134, 135, 136, 137, 138, 139,
  140, 141, 142, 143, 144, 145, 146, 147, 148, 149,
  150, 151, 152, 153, 138, 155, 140, 157, 142, 159,
  160, 161, 162, 163, 164, 165, 166, 167, 168, 169,
  170, 171, 172, 173, 174, 175, 176, 177, 178, 179,
  180, 181, 182, 183, 184, 185, 186, 187, 188, 189,
  190, 191, 192, 193, 194, 195, 196, 197, 198, 199,
  200, 201, 202, 203, 204, 205, 206, 207, 208, 209,
  210, 211, 212, 213, 214, 215, 216, 217, 218, 219,
  220, 221, 222, 223, 192, 193, 194, 195, 196, 197,
  198, 199, 200, 201, 202, 203, 204, 205, 206, 207,
  208, 209, 210, 211, 212, 213, 214, 247, 216, 217,
  218, 219, 220, 221, 254, 159);
{$Z4}
 AsciiUpperChars              : array[#0..#255] of Char = (
  #0, #1, #2, #3, #4, #5, #6, #7, #8, #9,
  #10, #11, #12, #13, #14, #15, #16, #17, #18, #19,
  #20, #21, #22, #23, #24, #25, #26, #27, #28, #29, #30, #31,
  ' ', '!', '"', '#', '$', '%', '&', '''', '(', ')',
  '*', '+', ',', '-', '.', '/', '0', '1', '2', '3',
  '4', '5', '6', '7', '8', '9', ':', ';', '<', '=',
  '>', '?', '@', 'A', 'B', 'C', 'D', 'E', 'F', 'G',
  'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q',
  'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', '[',
  '\', ']', '^', '_', '`', 'A', 'B', 'C', 'D', 'E',
  'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O',
  'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y',
  'Z', '{', '|', '}', '~', '', 'Ä', 'Å', 'Ç', 'É',
  'Ñ', 'Ö', 'Ü', 'á', 'à', 'â', 'ä', 'ã', 'å', 'ç',
  'é', 'è', 'ê', 'ë', 'í', 'ì', 'î', 'ï', 'ñ', 'ó',
  'ò', 'ô', 'ä', 'õ', 'å', 'ù', 'é', 'ü', '†', '°',
  '¢', '£', '§', '•', '¶', 'ß', '®', '©', '™', '´',
  '¨', '≠', 'Æ', 'Ø', '∞', '±', '≤', '≥', '¥', 'µ',
  '∂', '∑', '∏', 'π', '∫', 'ª', 'º', 'Ω', 'æ', 'ø',
  '¿', '¡', '¬', '√', 'ƒ', '≈', '∆', '«', '»', '…',
  ' ', 'À', 'Ã', 'Õ', 'Œ', 'œ', '–', '—', '“', '”',
  '‘', '’', '÷', '◊', 'ÿ', 'Ÿ', '⁄', '€', '‹', '›',
  'ﬁ', 'ﬂ', '¿', '¡', '¬', '√', 'ƒ', '≈', '∆', '«',
  '»', '…', ' ', 'À', 'Ã', 'Õ', 'Œ', 'œ', '–', '—',
  '“', '”', '‘', '’', '÷', '˜', 'ÿ', 'Ÿ', '⁄', '€',
  '‹', '›', '˛', 'ü');
 
 AsciiLowerChars              : array[#0..#255] of Char = (
  #0, #1, #2, #3, #4, #5, #6, #7, #8, #9,
  #10, #11, #12, #13, #14, #15, #16, #17, #18, #19,
  #20, #21, #22, #23, #24, #25, #26, #27, #28, #29, #30, #31,
  ' ', '!', '"', '#', '$', '%', '&', '''', '(', ')',
  '*', '+', ',', '-', '.', '/', '0', '1', '2', '3',
  '4', '5', '6', '7', '8', '9', ':', ';', '<', '=',
  '>', '?', '@', 'a', 'b', 'c', 'd', 'e', 'f', 'g',
  'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q',
  'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', '[',
  '\', ']', '^', '_', '`', 'a', 'b', 'c', 'd', 'e',
  'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o',
  'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y',
  'z', '{', '|', '}', '~', '', 'Ä', 'Å', 'Ç', 'É',
  'Ñ', 'Ö', 'Ü', 'á', 'à', 'â', 'ö', 'ã', 'ú', 'ç',
  'û', 'è', 'ê', 'ë', 'í', 'ì', 'î', 'ï', 'ñ', 'ó',
  'ò', 'ô', 'ö', 'õ', 'ú', 'ù', 'û', 'ˇ', '†', '°',
  '¢', '£', '§', '•', '¶', 'ß', '®', '©', '™', '´',
  '¨', '≠', 'Æ', 'Ø', '∞', '±', '≤', '≥', '¥', 'µ',
  '∂', '∑', '∏', 'π', '∫', 'ª', 'º', 'Ω', 'æ', 'ø',
  '‡', '·', '‚', '„', '‰', 'Â', 'Ê', 'Á', 'Ë', 'È',
  'Í', 'Î', 'Ï', 'Ì', 'Ó', 'Ô', '', 'Ò', 'Ú', 'Û',
  'Ù', 'ı', 'ˆ', '◊', '¯', '˘', '˙', '˚', '¸', '˝',
  '˛', 'ﬂ', '‡', '·', '‚', '„', '‰', 'Â', 'Ê', 'Á',
  'Ë', 'È', 'Í', 'Î', 'Ï', 'Ì', 'Ó', 'Ô', '', 'Ò',
  'Ú', 'Û', 'Ù', 'ı', 'ˆ', '˜', '¯', '˘', '˙', '˚',
  '¸', '˝', 'ﬁ', 'ˇ');
 
implementation

function StrLen (tStr: PChar): Integer;
{ StrLen replacement. Faster than the Delphi original. }
begin
 Result := 0;
 while tStr[Result] <> #0 do
  if tStr[Result + 1] <> #0 then
   if tStr[Result + 2] <> #0 then
    if tStr[Result + 3] <> #0 then
     Inc (Result, 4)
    else
    begin
     Inc (Result, 3);
     Exit;
    end
   else
   begin
    Inc (Result, 2);
    Exit;
   end
  else
  begin
   Inc (Result);
   Exit;
  end;
end;

{******************************************************************************}
{ Comparing Strings }
{******************************************************************************}

function PCompStr (const p1, p2: PChar): Integer;
var
 Run                          : Integer;
begin
{$IFDEF RangeChecking}
 if (p1 = nil) or (p2 = nil) then raise Exception.Create ('Must not pass nil pointers to PCompStr');
{$ENDIF}
 if p1^ <> p2^ then
 begin
  Result := Byte (p1^) - Byte (p2^);
  Exit;
 end;
 
 Run := 1;
 while (p1[Run] <> #0) and (p1[Run] = p2[Run]) do
  if (p1[Run + 1] <> #0) and (p1[Run + 1] = p2[Run + 1]) then
   if (p1[Run + 2] <> #0) and (p1[Run + 2] = p2[Run + 2]) then
    if (p1[Run + 3] <> #0) and (p1[Run + 3] = p2[Run + 3]) then
     Inc (Run, 4)
    else
    begin
     Inc (Run, 3);
     Break;
    end
   else
   begin
    Inc (Run, 2);
    Break;
   end
  else
  begin
   Inc (Run, 1);
   Break;
  end;
 Result := Byte (p1[Run]) - Byte (p2[Run]);
end;

{ ********** }

function PCompText (const p1, p2: PChar): Integer;
var
 Run                          : Integer;
begin
{$IFDEF RangeChecking}
 if (p1 = nil) or (p2 = nil) then raise Exception.Create ('Must not pass nil pointers to PCompText');
{$ENDIF}
 Run := 0;
 while p1[Run] <> #0 do
 begin
  if (p1[Run] = p2[Run]) or (AsciiUpperChars[p1[Run]] = AsciiUpperChars[p2[Run]]) then
   Inc (Run)
  else
   Break;
 end;
 Result := Byte (p1[Run]) - Byte (p2[Run]);
end;

{ ********** }

function CompStr (const s1, s2: AnsiString): Integer;
var
 p1, p2                       : PChar;
 Run                          : Integer;
begin
 p1 := Pointer (s1);
 p2 := Pointer (s2);
 
 if p1^ <> p2^ then
 begin
  Result := Byte (p1^) - Byte (p2^);
  Exit;
 end;
 
 Run := 1;
 while (p1[Run] <> #0) and (p1[Run] = p2[Run]) do
  if (p1[Run + 1] <> #0) and (p1[Run + 1] = p2[Run + 1]) then
   if (p1[Run + 2] <> #0) and (p1[Run + 2] = p2[Run + 2]) then
    if (p1[Run + 3] <> #0) and (p1[Run + 3] = p2[Run + 3]) then
     Inc (Run, 4)
    else
    begin
     Inc (Run, 3);
     Break;
    end
   else
   begin
    Inc (Run, 2);
    Break;
   end
  else
  begin
   Inc (Run, 1);
   Break;
  end;
 Result := Byte (p1[Run]) - Byte (p2[Run]);
end;

{ ********** }

function CompText (const s1, s2: AnsiString): Integer;
var
 p1, p2                       : PChar;
 Run                          : Integer;
begin
 p1 := Pointer (s1);
 p2 := Pointer (s2);
 Run := 0;
 while p1[Run] <> #0 do
 begin
  if (p1[Run] = p2[Run]) or (AsciiUpperChars[p1[Run]] = AsciiUpperChars[p2[Run]]) then
   Inc (Run)
  else
   Break;
 end;
 Result := Byte (p1[Run]) - Byte (p2[Run]);
end;

{******************************************************************************}
{ Changing Case }
{******************************************************************************}

procedure UpperCaseBuffer (Run: PChar);
{ Do not call this function unless you are absolutely sure you are
  passing a reference to a unique string which resides entirely in memory. }
begin
{$IFDEF RangeChecking}
 if (Run = nil) then raise Exception.Create ('Funktion "UpperCaseBuffer": Null-Pointer ¸bergeben');
{$ENDIF}
 while Run^ <> #0 do
 begin
  Run^ := AsciiUpperChars[Run^];
  if Run[1] <> #0 then
   Run[1] := AsciiUpperChars[Run[1]]
  else
   Break;
  if Run[2] <> #0 then
   Run[2] := AsciiUpperChars[Run[2]]
  else
   Break;
  if Run[3] <> #0 then
   Run[3] := AsciiUpperChars[Run[3]]
  else
   Break;
  if Run[4] <> #0 then
   Run[4] := AsciiUpperChars[Run[4]]
  else
   Break;
  if Run[5] <> #0 then
   Run[5] := AsciiUpperChars[Run[5]]
  else
   Break;
  if Run[6] <> #0 then
   Run[6] := AsciiUpperChars[Run[6]]
  else
   Break;
  if Run[7] <> #0 then
   Run[7] := AsciiUpperChars[Run[7]]
  else
   Break;
  Inc (Run, 8);
 end;
end;

{ ********** }

procedure LowerCaseBuffer (Run: PChar);
{ Do not call this function unless you are absolutely sure you are
  passing a reference to a unique string which resides entirely in memory. }
begin
{$IFDEF RangeChecking}
 if (Run = nil) then raise Exception.Create ('Funktion "LowerCaseBuffer": Null-Pointer ¸bergeben');
{$ENDIF}
 while Run^ <> #0 do
 begin
  Run^ := AsciiLowerChars[Run^];
  if Run[1] <> #0 then
   Run[1] := AsciiLowerChars[Run[1]]
  else
   Break;
  if Run[2] <> #0 then
   Run[2] := AsciiLowerChars[Run[2]]
  else
   Break;
  if Run[3] <> #0 then
   Run[3] := AsciiLowerChars[Run[3]]
  else
   Break;
  if Run[4] <> #0 then
   Run[4] := AsciiLowerChars[Run[4]]
  else
   Break;
  if Run[5] <> #0 then
   Run[5] := AsciiLowerChars[Run[5]]
  else
   Break;
  if Run[6] <> #0 then
   Run[6] := AsciiLowerChars[Run[6]]
  else
   Break;
  if Run[7] <> #0 then
   Run[7] := AsciiLowerChars[Run[7]]
  else
   Break;
  Inc (Run, 8);
 end;
end;

{ ********** }

procedure UpperCase (var s: AnsiString);
var
 p                            : Pointer;
begin
 p := Pointer (s); // There is a speed advantage in using a variable at this point.
 if p <> nil then
 begin
  UniqueString (s);
  p := Pointer (s);
  UpperCaseBuffer (p);
 end;
end;

{ ********** }

function UpperCaseFunc (const s: AnsiString): AnsiString;
var
 p                            : Pointer;
begin
 p := Pointer (s); // Hier die Variable zu nutzen bringt Geschwindigkeit.
 if p <> nil then
 begin
  SetString (Result, PChar (s), Length (s));
  p := Pointer (Result);
  UpperCaseBuffer (p);
 end;
end;

{ ********** }

procedure LowerCase (var s: AnsiString);
var
 p                            : Pointer;
begin
 p := Pointer (s); // Hier die Variable zu nutzen bringt Geschwindigkeit.
 if p <> nil then
 begin
  UniqueString (s);
  p := Pointer (s);
  LowerCaseBuffer (p);
 end;
end;

function LowerCaseFunc (const s: AnsiString): AnsiString;
var
 p                            : Pointer;
begin
 p := Pointer (s); // Hier die Variable zu nutzen bringt Geschwindigkeit.
 if p <> nil then
 begin
  SetString (Result, PChar (s), Length (s));
  p := Pointer (Result);
  LowerCaseBuffer (p);
 end;
end;

{ ********** }

procedure AppendChar (var s: AnsiString; const c: AnsiChar);
{ Adds a character to a string. }
begin
 SetLength (s, Succ (Length (s)));
 s[Length (s)] := c;
end;

{ **************************************************************************** }

function LoadStringFromResource (const Instance: THandle; const ResourceName: AnsiString): AnsiString;
var
 HResInfo                     : HRSRC;
 p                            : PChar;
begin
 HResInfo := FindResource (Instance, PChar (ResourceName), RT_RCDATA);
 p := LockResource (LoadResource (Instance, HResInfo));
 SetString (Result, p, SizeOfResource (Instance, HResInfo));
end;

{ **************************************************************************** }
{ Routines for skipping sets and characters in buffers and strings.
{ **************************************************************************** }

procedure SkipSet (var p: PChar; const Search: TCharSet);
var
 Run                          : PChar;
begin
 Run := p;
 while Run^ in Search do
  if Run[1] in Search then
   if Run[2] in Search then
    if Run[3] in Search then
    begin
     Inc (Run, 4);
    end
    else
    begin
     Inc (Run, 3);
     Break;
    end
   else
   begin
    Inc (Run, 2);
    Break;
   end
  else
   Inc (Run, 1);
 p := Run;
end;

{ ********** }

procedure SkipSetBackwards (var p: PChar; const Search: TCharSet);
var
 Run                          : PChar;
begin
 Run := p;
 while Run^ in Search do
  if Run[-1] in Search then
   if Run[-2] in Search then
    if Run[-3] in Search then
    begin
     Dec (Run, 4);
    end
    else
    begin
     Dec (Run, 3);
     Break;
    end
   else
   begin
    Dec (Run, 2);
    Break;
   end
  else
   Dec (Run, 1);
 p := Run;
end;

{ ********** }

procedure SkipNotSet (var p: PChar; const Search: TCharSet);
var
 Run                          : PChar;
begin
 Run := p;
 while not (Run^ in Search) do
  if not (Run[1] in Search) then
   if not (Run[2] in Search) then
    if not (Run[3] in Search) then
    begin
     Inc (Run, 4);
    end
    else
    begin
     Inc (Run, 3);
     Break;
    end
   else
   begin
    Inc (Run, 2);
    Break;
   end
  else
   Inc (Run, 1);
 p := Run;
end;

{ ********** }

procedure SkipNotSetBackwards (var p: PChar; const Search: TCharSet);
var
 Run                          : PChar;
begin
 Run := p;
 while not (Run^ in Search) do
  if not (Run[-1] in Search) then
   if not (Run[-2] in Search) then
    if not (Run[-3] in Search) then
    begin
     Dec (Run, 4);
    end
    else
    begin
     Dec (Run, 3);
     Break;
    end
   else
   begin
    Dec (Run, 1);
    Break;
   end
  else
   Dec (Run, 1);
 p := Run;
end;

{ Skipping characters ********** }

procedure SkipChar (var p: PChar; const Search: Char);
var
 Run                          : PChar;
begin
 Run := p;
 while Run^ = Search do
 begin
  if not (Run[1] = Search) then
  begin
   Inc (Run, 1);
   Break;
  end;
  if not (Run[2] = Search) then
  begin
   Inc (Run, 2);
   Break;
  end;
  if not (Run[3] = Search) then
  begin
   Inc (Run, 3);
   Break;
  end;
  Inc (Run, 4);
 end;
 p := Run;
end;

{ ********** }

procedure SkipCharBackwards (var p: PChar; const Search: Char);
var
 Run                          : PChar;
begin
 Run := p;
 while Run^ = Search do
 begin
  if not (Run[-1] = Search) then
  begin
   Dec (Run, 1);
   Break;
  end;
  if not (Run[-2] = Search) then
  begin
   Dec (Run, 2);
   Break;
  end;
  if not (Run[-3] = Search) then
  begin
   Dec (Run, 3);
   Break;
  end;
  Dec (Run, 4);
 end;
 p := Run;
end;

{ ********** }

procedure SkipNotChar (var p: PChar; const Search: Char);
var
 Run                          : PChar;
begin
 Run := p;
 while not (Run^ = Search) do
 begin
  if Run[1] = Search then
  begin
   Inc (Run, 1);
   Break;
  end;
  if Run[2] = Search then
  begin
   Inc (Run, 2);
   Break;
  end;
  if Run[3] = Search then
  begin
   Inc (Run, 3);
   Break;
  end;
  Inc (Run, 4);
 end;
 p := Run;
end;

{ **************************************************************************** }

function IsEmptyString (const s: AnsiString): Boolean;
var
 p                            : PChar;
begin
 p := Pointer (s); // Wir kˆnnen auch PChar (s) nehmen.
 if p = nil then // Dann br‰uchten wir nicht auf nil pr¸fen.
  Result := True // Dauert aber trotzdem l‰nger, vor allem wenn s = ''.
 else
 begin
  SkipSet (p, csWhiteSpace);
  Result := p^ = #0;
 end;
end; {Ende Function IsEmptyString}

{ **************************************************************************** }

function PScanChar (const p: PChar; const c: Char): Integer;
var
 Run                          : PChar;
begin
 Run := p;
 while (Run^ <> #0) and (Run^ <> c) do
  if (Run[1] <> #0) and (Run[1] <> c) then
   if (Run[2] <> #0) and (Run[2] <> c) then
    if (Run[3] <> #0) and (Run[3] <> c) then
     Inc (Run, 4)
    else
    begin
     Inc (Run, 3);
     Break;
    end
   else
   begin
    Inc (Run, 2);
    Break;
   end
  else
  begin
   Inc (Run, 1);
   Break;
  end;
 if Run^ = c then
  Result := Run - p
 else
  Result := -1;
end;

{ **************************************************************************** }
{ Looking for characters of a set in a string.
{ **************************************************************************** }

function ScanS (Src: string; Search: TCharSet; const Start: Integer): Integer;
var
 p, s                         : PChar;
begin
 if (Start < 1) or
  (Start > Length (Src)) then
 begin
  Result := 0;
  Exit;
 end;
 Include (Search, #0);
 s := Pointer (Src);
 p := s + Start - 1;
 
 SkipNotSet (p, Search);
 
 if p^ = #0 then
  Result := 0
 else
  Result := p - s + 1;
end; { Ende Function ScanS }

{ ********** }

function ScanBS (const Src: AnsiString; Search: TCharSet; const Start: Integer): Integer;
var
 Before, Run                  : PChar;
begin
 if (Start < 1) or
  (Start > Length (Src)) then
 begin
  Result := 0;
  Exit;
 end;
 
 Before := Pointer (Src);
 Dec (Before); // Zeigt jetzt auf die Speicherzelle vor dem String, also das letzte Byte der String-L‰nge
 
 Include (Search, Before^);
 
 Run := Before + Start; // Run zeigt jetzt auf die Startposition
 
 SkipNotSetBackwards (Run, Search);
 
 if Run = Before then
  Result := 0
 else
  Result := Run - Before;
end; {Ende Function ScanBS}

{ ********** }

function ScanNS (const Src: AnsiString; Search: TCharSet; const Start: Integer): Integer;
var
 p, Run                       : PChar;
begin
 if (Start < 1) or (Start > Length (Src)) then
 begin
  Result := 0;
  Exit;
 end;
 
 p := Pointer (Src);
 Run := p + Start - 1;
 
 Exclude (Search, #0);
 
 SkipSet (Run, Search);
 
 if Run^ = #0 then
  Result := 0
 else
  Result := Run - p + 1;
end; {Ende Function ScanNS}

{ ********** }

function ScanBNS (Src: string; Search: TCharSet; const Start: Integer): Integer;
var
 Before, Run                  : PChar;
begin
 if (Start < 1) or
  (Start > Length (Src)) then
 begin
  Result := 0;
  Exit;
 end;
 
 Before := Pointer (Src);
 Dec (Before); // Zeigt jetzt auf die Speicherzelle vor dem String, also das letzte Byte der String-L‰nge
 
 Exclude (Search, Before^);
 
 Run := Before + Start; // Run zeigt jetzt auf die Startposition
 
 SkipSetBackwards (Run, Search);
 
 if Run = Before then
  Result := 0
 else
  Result := Run - Before;
end; { Ende Function ScanBNS }

{ **************************************************************************** }

procedure ReplaceStr (var Str: string; const sub1, sub2: string);
var
 iStr, jstr, StrEnd, pSub1, Sub2End, pTemp: PChar;
 k, l, lenS1, lenS2           : Integer;
 newStr                       : string;
 c                            : Char;
begin
 pSub1 := PChar (sub1);
 lenS1 := Length (sub1);
 lenS2 := Length (sub2);
 StrEnd := @Str[Length (Str) + 1];
 Sub2End := @sub2[lenS2 + 1];
 Assert (lenS1 > 0, 'Sub1 - must not be empty');
 // make sure newStr is long enough
 if lenS2 > lenS1 then
  SetLength (newStr, ((Length (Str) div lenS1) + 1) * lenS2)
 else
  SetLength (newStr, Length (Str));
 iStr := @Str[1];
 jstr := @newStr[1];
 c := sub1[1];
 StrEnd[0] := c; // set to stop loop (could be a problem if ref count >1 )
 while True do
 begin
  k := 0;
  while (iStr[k] <> c) do // copy until possible match
  begin
   jstr[k] := iStr[k];
   Inc (k);
  end;
  Inc (jstr, k);
  Inc (iStr, k);
  if iStr = StrEnd then // check end
   Break; // bail out
  l := StrEnd - iStr;
  k := 1; // First char is already matched.
  if l > lenS1 then
  begin // don't need to check l since the zero at end of Sub1 will stop loop
   l := lenS1;
   pTemp := pSub1; // increases "priority" of psub1
   while iStr[k] = pTemp[k] do
    Inc (k);
  end
  else // here we have to check
   while ((k < l) and (iStr[k] = pSub1[k])) do
    Inc (k);
  if (k = l) then // match
  begin // copy
   Inc (iStr, lenS1);
   k := -lenS2;
   Inc (jstr, lenS2);
   while k < 0 do
   begin
    jstr[k] := Sub2End[k];
    Inc (k);
   end;
  end
  else // no match
  begin
   jstr^ := iStr^;
   Inc (jstr);
   Inc (iStr);
  end;
 end;
 StrEnd[0] := #0; // return to original state
 SetLength (newStr, jstr - PChar (newStr));
 Str := newStr;
end;

{ **************************************************************************** }

function CapitalizeWords (const s: AnsiString): AnsiString;
var
 p                            : PChar;
 LastWasAlphaNum              : Boolean;
begin
 LastWasAlphaNum := False;
 SetString (Result, PChar (s), Length (s));
 p := Pointer (Result);
 while p^ <> #0 do
 begin
  if p^ in csAlphaNumeric + [''''] then
  begin
   { We have a letter or number, is it the first or is the character
     in front not a letter or number? }
   if LastWasAlphaNum then
    p^ := AsciiLowerChars[p^] // Lowercase the character.
   else
   begin
    p^ := AsciiUpperChars[p^]; // Uppercase the character.
    LastWasAlphaNum := True;
   end;
  end
  else
   LastWasAlphaNum := False;
  Inc (p);
 end; { while }
end; { Ende Function CapitalizeWords }

{ **************************************************************************** }

function GetLastErrorString (const MessageID: DWord): AnsiString;
var
 Len                          : Integer;
 Buffer                       : array[0..255] of Char;
begin
 Len := FormatMessage (FORMAT_MESSAGE_FROM_SYSTEM, nil, MessageID, 0, Buffer, SizeOf (Buffer), nil);
 
 while (Len > 0) and (Buffer[Len - 1] in [#0..#32]) do
  Dec (Len);
 
 Buffer[Len] := #0;
 Result := Buffer;
end;

{ **************************************************************************** }
{ Funktioniert auch mit LongMonthNames [i] etc., in SysUtils definiert.
  Diese Funktionen nutzen also nur dann,
  wenn SysUtils im Programm nicht gebraucht wird.
  Bei Bedarf kann die Sprache mit ThreadLocale angepaﬂt werden.
{ **************************************************************************** }

function LongMonthName (const i: Integer): AnsiString;
var
 l                            : Integer;
 Buffer                       : array[0..255] of Char;
begin
 l := GetLocaleInfo (GetThreadLocale, LOCALE_SMONTHNAME1 + i - 1, Buffer, SizeOf (Buffer));
 if l > 0 then
  SetString (Result, Buffer, l - 1)
 else
  Result := '';
end; {Ende Function LongMonthName}

{ ********** }

function ShortMonthName (const i: Integer): AnsiString;
var
 l                            : Integer;
 Buffer                       : array[0..255] of Char;
begin
 l := GetLocaleInfo (GetThreadLocale, LOCALE_SABBREVMONTHNAME1 + i - 1, Buffer, SizeOf (Buffer));
 if l > 0 then
  SetString (Result, Buffer, l - 1)
 else
  Result := '';
end; {Ende Function ShortMonthName}

{ ********** }

function LongDayName (const i: Integer): AnsiString;
var
 l                            : Integer;
 Buffer                       : array[0..255] of Char;
begin
 l := GetLocaleInfo (GetThreadLocale, LOCALE_SDAYNAME1 + i - 1, Buffer, SizeOf (Buffer));
 if l > 0 then
  SetString (Result, Buffer, l - 1)
 else
  Result := '';
end; {Ende Function LongDayName}

{ ********** }

function ShortDayName (const i: Integer): AnsiString;
var
 l                            : Integer;
 Buffer                       : array[0..255] of Char;
begin
 l := GetLocaleInfo (GetThreadLocale, LOCALE_SABBREVDAYNAME1 + i - 1, Buffer, SizeOf (Buffer));
 if l > 0 then
  SetString (Result, Buffer, l - 1)
 else
  Result := '';
end; {Ende Function ShortDayName}

end.

