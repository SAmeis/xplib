unit rjSearch;
{
File:           rjSearch.pas
Version:        0.1 / Initial Public Release
Last Modified:  1. April 2000
Decription:     TSearch is a very fast implementation of the
                Boyer-Moore-Horspool pattern match algorithm for
                object pascal.

                It is inspired by the work of Raymond Gardner.
                The original is from "High Performance Delphi" at
                <http://econos.com/optimize/>.

                Major changes and new features:
                * Completely new design as class.
                * Restrictions to pattern length and string length
                  are virtually removed. Both can now be up to MaxInt
                  characters long.
                * Searching is now also possible without case sensitivity.

Author:         Ralf Junker
E-Mail:         ralfjunker@gmx.de
Legal:          This unit is provided "as is" and without warranty of any kind.
                At the moment, it is freeware for private use only.
                I might, however, change this in the future. If you intend
                to use it commercially, please contact me via e-mail.
Bug Report:     If you encounter any bugs or want to suggest improvements,
                just get in touch using my e-mail above. When writing, please
                include the name and version of the unit you are referring to.
Thanks to:      * Raymond Gardner <?@?.?> for his tricky implementation.
                * Robert Lee <rhlee@nwu.edu> for his "High Performance Delphi"
                  page <http://econos.com/optimize/> full of tips & tricks.

Copyright (c) 2000 Ralf Junker.

Original Copyright Notice:

  +++Date last modified: 05-Jul-1997 */
  Case-sensitive Boyer-Moore-Horspool pattern match
  public domain by Raymond Gardner 7/92
  limitation: pattern length + string length must be less than 32767
  10/21/93 rdg  Fixed bug found by Jeff Dunlop
}

interface

{$I rj.inc}

uses
{$IFDEF RangeChecking}
 SysUtils,
{$ENDIF}

 rjString;

{$IFDEF RangeChecking}
resourcestring
 sPatternEmpty                = 'Pattern is empty.';
 sBufferNil                   = 'Passed nil Pointer to SearchBuffer.';
{$ENDIF}

const
 Large                        = MaxInt;

type
{$IFDEF RangeChecking}
 ESearch = class (Exception);
{$ENDIF}
 
 PShift = ^TShift;
 TShift = array[#0..#255] of Integer;

 TSearchBufferFunc = function (const Buffer: Pointer; const BufferLength: Integer): Pointer of object;
 
 TSearch = class
 private
  FLookAt: Integer;
  FShift: TShift;
  FPattern: AnsiString;
  FPatternLength: Integer;
  FCaseSensitive: Boolean;
  FSearchBufferFunc: TSearchBufferFunc;
  function SearchBufferCaseInSensitive (const Buffer: Pointer; const BufferLength: Integer): Pointer;
  function SearchBufferCaseSensitive (const Buffer: Pointer; const BufferLength: Integer): Pointer;
 protected
 public
  constructor Create (const Pattern: AnsiString; const CaseSensitive: Boolean = True);
  procedure SetPattern (const Value: AnsiString; const CaseSensitive: Boolean = True);
  function SearchBuffer (const Buffer: Pointer; const BufferLength: Integer): Pointer;
  function SearchString (const s: AnsiString): Integer;
  function SearchStringPos (const s: AnsiString; const Start: Integer): Integer;
 end;
 
implementation

{ TSearch }

constructor TSearch.Create (const Pattern: AnsiString; const CaseSensitive: Boolean = True);
begin
 SetPattern (Pattern, CaseSensitive);
end;

{ ********** }

function TSearch.SearchBuffer (const Buffer: Pointer; const BufferLength: Integer): Pointer;
begin
{$IFDEF RangeChecking}
 if Buffer = nil then raise ESearch.Create (sBufferNil);
{$ENDIF}
 Result := FSearchBufferFunc (Buffer, BufferLength);
end;

{ ********** }

function TSearch.SearchString (const s: AnsiString): Integer;
var
 p, r                         : Pointer;
begin
 p := PChar (s);
 r := FSearchBufferFunc (p, Length (s));
 if r = nil then
  Result := 0
 else
  Result := Integer (r) - Integer (p) + 1;
end;

{ ********** }

function TSearch.SearchStringPos (const s: AnsiString; const Start: Integer): Integer;
var
 p, r                         : Pointer;
 l                            : Integer;
begin
 l := Length (s);
 if Start > l then
 begin
  Result := 0;
  Exit;
 end;
 p := PChar (s);
 Dec (l, Start);
 r := FSearchBufferFunc (Pointer (Integer (p) + Start), l);
 if r = nil then
  Result := 0
 else
  Result := Integer (r) - Integer (p) + 1;
end;

{ ********** }

function TSearch.SearchBufferCaseSensitive (const Buffer: Pointer; const BufferLength: Integer): Pointer;
var
 SearchEnd, PatternEnd        : PChar;
 i, j, PatternLength          : Integer;
begin
 Integer (SearchEnd) := Integer (Buffer) + BufferLength; // PEnd zeigt auf erstes Zeichen nach dem Puffer.
 
 PatternLength := FPatternLength;
 PatternEnd := PChar (FPattern) + PatternLength;
 
 i := PatternLength - BufferLength;
 
 while i < 0 do // Case Sensitive Loop
 begin
  repeat
   Inc (i, FShift[SearchEnd[i]]);
  until i >= 0;
  if i < BufferLength then Break;

  Dec (i, Large); // i steht jetzt auf dem Treffer (letzter Buchstabe in Pattern).

  Inc (SearchEnd, i); // Since i is negative we need to increment SearchEnd to actually decrement it.
  j := -PatternLength;
  while (j < 0) and (SearchEnd[j] = PatternEnd[j]) do
   Inc (j);
  if j = 0 then
  begin
   Result := SearchEnd - PatternLength;
   Exit;
  end;
  Dec (SearchEnd, i); // Set SearchEnd back to the first Char after the buffer.
  Inc (i, FLookAt);
 end;
 Result := nil;
end;

{ ********** }

function TSearch.SearchBufferCaseInSensitive (const Buffer: Pointer; const BufferLength: Integer): Pointer;
var
 SearchEnd, PatternEnd        : PChar;
 i, j, PatternLength          : Integer;
begin
 Integer (SearchEnd) := Integer (Buffer) + BufferLength; // PEnd zeigt auf erstes Zeichen nach dem Puffer.
 
 PatternLength := FPatternLength;
 PatternEnd := PChar (FPattern) + PatternLength;
 
 i := PatternLength - BufferLength;
 
 while i < 0 do // Non Case Sensitive Loop
 begin
  repeat
   Inc (i, FShift[SearchEnd[i]]);
  until i >= 0;
  if i < BufferLength then Break;

  Dec (i, Large); // SearchEnd [i] steht jetzt auf dem Treffer (letzter Buchstabe in Pattern).

  Inc (SearchEnd, i); // Since i is negative we need to increment SearchEnd to actually decrement it.
  j := -PatternLength;
  while (j < 0) and (AsciiLowerChars[SearchEnd[j]] = PatternEnd[j]) do
   Inc (j);
  if j = 0 then
  begin
   Result := SearchEnd - PatternLength;
   Exit;
  end;
  Dec (SearchEnd, i); // Set SearchEnd back to the first Char after the buffer.
  Inc (i, FLookAt);
 end;
 Result := nil;
end;

{ ********** }

procedure TSearch.SetPattern (const Value: AnsiString; const CaseSensitive: Boolean = True);
var
 c                            : Char;
 i, PatternLength             : Integer;
 p                            : PChar;
 Shift                        : PShift;
begin
 PatternLength := Length (Value);
{$IFDEF RangeChecking}
 if PatternLength = 0 then raise ESearch.Create (sPatternEmpty);
{$ENDIF}
 FPattern := Value;
 FCaseSensitive := CaseSensitive;
 
 Shift := @FShift;
 for i := 0 to 255 do
  Shift[Char (i)] := PatternLength;
 
 Dec (PatternLength);
 FPatternLength := PatternLength;
 
 if CaseSensitive then
 begin
  p := PChar (FPattern);

  for i := 1 to PatternLength do
   Shift[p[i]] := PatternLength - i;

  c := p[PatternLength];

  Shift[c] := Large; // Indicates "hit" in the search function.

  FSearchBufferFunc := SearchBufferCaseSensitive
 end

 else
 begin
  LowerCase (FPattern);
  p := PChar (FPattern);

  for i := 1 to PatternLength do
  begin
   Shift[p[i]] := PatternLength - i;
   Shift[AsciiUpperChars[p[i]]] := PatternLength - i;
  end;

  c := p[PatternLength];

  Shift[c] := Large; // Indicates "hit" in the search function.
  Shift[AsciiUpperChars[c]] := Large; // Indicates "hit" in the search function.

  FSearchBufferFunc := SearchBufferCaseInSensitive;
 end;
 
 i := PatternLength - 1; // Local variable for FLookAt. Stored to FLookAt below.
 while (i >= 0) and (p[i] <> c) do
  Dec (i);
 
 FLookAt := PatternLength - i;
end;

end.

