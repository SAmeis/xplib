unit rjHtmlTag;
{
File:           rjHtmlTag.pas
Version:        0.1 / Initial Public Release
Last Modified:  1. April 2000
Decription:     THtmlTag holds a HTML Tag and its attributes.
                Tags are stored as a type of TTags. Positive values represent
                StartTags, negative values EndTags.
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
 Classes, Consts,
{$ENDIF}
 rjString,
 rjHtmlAttribs, rjHtmlTags;

type
 TTags = -tagMax..tagMax;
 TStartTags = 1..tagMax;
 TEndTags = -tagMax.. - 1;
 
 TTagSet = set of TStartTags;
 
 TTagAttrib = record
  Tag: TTags;
  Attrib: TAttribs;
 end;
 
 PAttribItem = ^TAttribItem;
 TAttribItem = record
  Attrib: TAttribs;
  Value: AnsiString;
 end;
 
 PAttribItemList = ^TAttribItemList;
 TAttribItemList = array[0..MaxInt div 16] of TAttribItem;
 
 { Class THtmlTag }
 THtmlTag = class
 private
  { Tag }
  FTag: TTags;
  { Attribs }
  FAttribList: PAttribItemList;
  FAttribCount: Integer;

  function GetAttribValue (const Attrib: TAttribs): AnsiString;
  procedure SetAttribValue (const Attrib: TAttribs; const Value: AnsiString);

  function GetAttrib (const Index: Integer): TAttribs;
  procedure SetAttrib (const Index: Integer; const Attrib: TAttribs);

  function GetValue (const Index: Integer): AnsiString;
  procedure SetValue (const Index: Integer; const Value: AnsiString);

  function GetHtmlString: AnsiString;

 protected
{$IFDEF RangeChecking}
  procedure Error (const Msg: AnsiString; Data: Integer);
{$ENDIF}
 public
  constructor Create; virtual;
  constructor CreateCopy (const Source: THtmlTag);
  destructor Destroy; override;
  procedure Clear;
  procedure AddAttrib (const Attrib: TAttribs; const Value: AnsiString);
  property AttribCount: Integer read FAttribCount;
  procedure ExchangeAttribs (const Index1, Index2: Integer);
  function IndexOfAttrib (const Attrib: TAttribs): Integer;
  procedure InsertAttrib (const Index: Integer; const Attrib: TAttribs; const Value: AnsiString);
  property Attribs[const Index: Integer]: TAttribs read GetAttrib write SetAttrib; default;
  property Values[const Index: Integer]: AnsiString read GetValue write SetValue;
  procedure Parse (var p: PChar);
  procedure SkipTag (var p: PChar);
  procedure Assign (const Source: THtmlTag); virtual;
  procedure ClearAttribs;
  procedure DeleteAttrib (const Index: Integer);
  procedure FontSizeToRelative;
  procedure FontSizeToAbsolute;
  function HasAttrib (const Attrib: TAttribs): Boolean;
  function HasAttribValue (const Attrib: TAttribs; const Value: AnsiString): Boolean;
  function HasAttribs: Boolean;
  property Html: AnsiString read GetHtmlString;
  property Tag: TTags read FTag write FTag;
 end;
 
implementation

{ Class THtmlTag }

constructor THtmlTag.Create;
begin
 inherited Create;
 FTag := Cardinal (tagUndefined);
 FAttribCount := 0;
end;

{ ********** }

constructor THtmlTag.CreateCopy (const Source: THtmlTag);
begin
 Create;
 Assign (Source);
end;

{ ********** }

destructor THtmlTag.Destroy;
begin
 Clear;
 inherited Destroy;
end;

{ ********** }

{$IFDEF RangeChecking}

procedure THtmlTag.Error (const Msg: AnsiString; Data: Integer);

 function ReturnAddr: Pointer;
 asm
          MOV     EAX,[EBP+4]
 end;
begin
 raise EListError.CreateFmt (Msg, [Data])At ReturnAddr;
end;
{$ENDIF}

{ ********** }

procedure THtmlTag.ExchangeAttribs (const Index1, Index2: Integer);
var
 Temp                         : TAttribItem;
begin
 Temp := FAttribList^[Index1];
 FAttribList^[Index1] := FAttribList^[Index2];
 FAttribList^[Index2] := Temp;
end;

{ ********** }

procedure THtmlTag.InsertAttrib (const Index: Integer; const Attrib: TAttribs; const Value: AnsiString);
begin
{$IFDEF RangeChecking}
 if (Index < 0) or (Index > FAttribCount) then Error (SListIndexError, Index);
{$ENDIF}
 if (FAttribCount and 1) = 0 then
  ReAllocMem (FAttribList, ((FAttribCount + 2) and $FFFFFFFE) * SizeOf (TAttribItem));
 
 if Index < FAttribCount then
  System.Move (FAttribList^[Index], FAttribList^[Index + 1],
   (FAttribCount - Index) * SizeOf (TAttribItem));
 
 FAttribList^[Index].Attrib := Attrib;
 
 Pointer (FAttribList^[Index].Value) := nil;
 FAttribList^[Index].Value := Value;
 
 Inc (FAttribCount);
end;

function THtmlTag.HasAttrib (const Attrib: TAttribs): Boolean;
begin
 Result := IndexOfAttrib (Attrib) <> -1;
end;

{ ********** }

function THtmlTag.HasAttribValue (const Attrib: TAttribs; const Value: AnsiString): Boolean;
var
 i                            : Integer;
begin
 i := IndexOfAttrib (Attrib);
 if i <> -1 then
  Result := FAttribList[i].Value = Value
 else
  Result := False;
end;

{ ********** }

function THtmlTag.IndexOfAttrib (const Attrib: TAttribs): Integer;
begin
 Result := 0;
 while Result < FAttribCount do
 begin
  if FAttribList^[Result].Attrib = Attrib then Exit;
  Inc (Result);
 end;
 Result := -1;
end;

{ ********** }

function THtmlTag.GetAttrib (const Index: Integer): TAttribs;
begin
{$IFDEF RangeChecking}
 if (Index < 0) or (Index > FAttribCount) then Error (SListIndexError, Index);
{$ENDIF}
 Result := FAttribList[Index].Attrib;
end;

{ ********** }

procedure THtmlTag.SetAttrib (const Index: Integer; const Attrib: TAttribs);
begin
{$IFDEF RangeChecking}
 if (Index < 0) or (Index > FAttribCount) then Error (SListIndexError, Index);
{$ENDIF}
 FAttribList[Index].Attrib := Attrib;
end;

{ ********** }

function THtmlTag.GetValue (const Index: Integer): AnsiString;
begin
{$IFDEF RangeChecking}
 if (Index < 0) or (Index >= FAttribCount) then Error (SListIndexError, Index);
{$ENDIF}
 Result := FAttribList^[Index].Value;
end;

{ ********** }

procedure THtmlTag.SetValue (const Index: Integer; const Value: AnsiString);
begin
{$IFDEF RangeChecking}
 if (Index < 0) or (Index >= FAttribCount) then Error (SListIndexError, Index);
{$ENDIF}
 FAttribList^[Index].Value := Value;
end;

{ ********** }

function THtmlTag.GetAttribValue (const Attrib: TAttribs): AnsiString;
var
 i                            : Integer;
begin
 i := IndexOfAttrib (Attrib);
 if i < 0 then
  Result := ''
 else
  Result := FAttribList[i].Value;
end;

{ ********** }

procedure THtmlTag.SetAttribValue (const Attrib: TAttribs; const Value: AnsiString);
var
 i                            : Integer;
begin
 i := IndexOfAttrib (Attrib);
 if i >= 0 then
  FAttribList[i].Value := Value;
end;

{ ********** }

procedure THtmlTag.Parse (var p: PChar);
begin
 Clear;
end;

{ ********** }

procedure THtmlTag.SkipTag (var p: PChar); // Html-Tag überspringen ----------------------------------------------
var
 Quote                        : Char;
begin
 repeat
  SkipNotSet (p, ['>', '"', '''', #0]);

  if (p^ = '"') or (p^ = '''') then
  begin
   Quote := p^;
   SkipNotSet (p, ['>', Quote, #0]);
   if p^ = Quote then Inc (p);
  end;
 until p^ in [#0, '>']; // Pointer steht jetzt auf '>' oder #0
 
 if p^ = '>' then Inc (p);
end;

{ ********** }

function THtmlTag.HasAttribs: Boolean;
begin
 Result := FAttribCount > 0;
end;

{ ********** }

procedure THtmlTag.Assign (const Source: THtmlTag);
var
 i                            : Integer;
begin
 Clear;
 FTag := Source.FTag;
 
 i := 0;
 while i < Source.AttribCount do
 begin
  AddAttrib (Source.Attribs[i], Source.Values[i]);
  Inc (i);
 end;
end;

{ ********** }

procedure THtmlTag.Clear;
begin
 FTag := Cardinal (tagUndefined);
 ClearAttribs;
end;

{ ********** }

procedure THtmlTag.ClearAttribs;
begin
 if FAttribCount <> 0 then
 begin
  Finalize (FAttribList^[0], FAttribCount);
  FAttribCount := 0;
  ReAllocMem (FAttribList, FAttribCount);
 end;
end;

{ ********** }

procedure THtmlTag.AddAttrib (const Attrib: TAttribs; const Value: AnsiString);
begin
 if IndexOfAttrib (Attrib) >= 0 then Exit;
 
 if (FAttribCount and 1) = 0 then
  ReAllocMem (FAttribList, ((FAttribCount + 2) and $FFFFFFFE) * SizeOf (TAttribItem));
 
 FAttribList^[FAttribCount].Attrib := Attrib;
 
 Pointer (FAttribList^[FAttribCount].Value) := nil;
 FAttribList^[FAttribCount].Value := Value;
 
 Inc (FAttribCount);
end;

{ ********** }

procedure THtmlTag.DeleteAttrib (const Index: Integer);
begin
{$IFDEF RangeChecking}
 if (Index < 0) or (Index >= FAttribCount) then Error (SListIndexError, Index);
{$ENDIF}
 Finalize (FAttribList^[Index]);
 Dec (FAttribCount);
 if Index < FAttribCount then
  System.Move (FAttribList^[Index + 1], FAttribList^[Index],
   (FAttribCount - Index) * SizeOf (TAttribItem));
end;

{ ********** }

function THtmlTag.GetHtmlString: AnsiString;
{ Convert THtmlTag for output as string. }
var
 i                            : Integer;
 s                            : AnsiString;
begin
 if FTag < 0 then
  Result := '</' + TagNames[Abs (FTag)]
 else
  Result := '<' + TagNames[FTag];
 
 i := 0;
 while i < FAttribCount do
 begin
  Result := Result + ' ' + AttribNames[FAttribList[i].Attrib];
  s := FAttribList[i].Value;
  if not IsEmptyString (s) then
   Result := Result + '="' + s + '"';
  Inc (i);
 end;
 Result := Result + '>';
end;

{ ********** }

procedure THtmlTag.FontSizeToAbsolute;
{ Changes Font Sizes from relative to absolute Values. }
var
 i                            : Integer;
 s                            : AnsiString;
begin
 if FTag <> tagFONT then Exit;
 
 i := IndexOfAttrib (attribSize);
 if i < 0 then Exit;
 
 s := FAttribList^[i].Value;
 if s = '' then Exit;
 
 if s = '+4' then
  FAttribList^[i].Value := '7'
 else
  if s = '+3' then
   FAttribList^[i].Value := '6'
  else
   if s = '+2' then
    FAttribList^[i].Value := '5'
   else
    if s = '+1' then
     FAttribList^[i].Value := '4'
    else
     if s = '3' then // Size="3" is standard size. We can saftely delete it.
      DeleteAttrib (i)
     else
      if s = '-1' then
       FAttribList^[i].Value := '2'
      else
       if s = '-2' then
        FAttribList^[i].Value := '1';
end;

{ ********** }

procedure THtmlTag.FontSizeToRelative;
{ Changes Font Sizes from absolute to relative Values. }
var
 i                            : Integer;
 s                            : AnsiString;
begin
 if FTag <> tagFONT then Exit;
 
 i := IndexOfAttrib (attribSize);
 if i < 0 then Exit;
 
 s := FAttribList^[i].Value;
 if s = '' then Exit;
 
 if s = '7' then
  FAttribList^[i].Value := '+4'
 else
  if s = '6' then
   FAttribList^[i].Value := '+3'
  else
   if s = '5' then
    FAttribList^[i].Value := '+2'
   else
    if s = '4' then
     FAttribList^[i].Value := '+1'
    else
     if s = '3' then // Size="3" is standard size. We can saftely delete it.
      DeleteAttrib (i)
     else
      if s = '2' then
       FAttribList^[i].Value := '-1'
      else
       if s = '1' then
        FAttribList^[i].Value := '-2';
end;

end.

