unit rjBase;
{
File:           rjBase.pas
Version:        0.1 / Initial Public Release
Last Modified:  1. April 2000
Decription:     Types, constants and routines repeatedly used by my units
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

uses Classes, Controls, ShlObj, SysUtils;

type
 { Events }
 TProcEvent = procedure of object;
 TBooleanFunc = function: Boolean of object;
 
 { Exceptions }
 EFileOperation = class (Exception);
 EFileCreate = class (EFileOperation);
 EFileOpen = class (EFileOperation);
 EFileWrite = class (EFileOperation);
 EFileRead = class (EFileOperation);
 
 EFileMapping = class (EFileOperation);
 EMapViewOfFile = class (EFileMapping);
 
 ECreateDir = class (EFileOperation);
 
 PObjectArray = ^TObjectArray;
 // TObjectArray = array [0.. (MaxListSize - 1) div SizeOf (TObject)] of TObject;
 TObjectArray = array[0..MaxInt div SizeOf (TObject) - 1] of TObject;
 
 PInteger = ^Integer;
 PIntegerArray = ^TIntegerArray;
 // TIntegerArray = array [0.. (MaxListSize - 1) div SizeOf (Integer)] of Integer;
 TIntegerArray = array[0..MaxInt div SizeOf (Integer) - 1] of Integer;
 
 PCardinal = ^Cardinal;
 PCardinalArray = ^TCardinalArray;
 // TCardinalArray = array [0.. (MaxListSize - 1) div SizeOf (Cardinal)] of Cardinal;
 TCardinalArray = array[0..MaxInt div SizeOf (Cardinal) - 1] of Cardinal;
 
 PDualStringItem = ^TDualStringItem;
 TDualStringItem = record
  Key: AnsiString;
  Value: AnsiString;
 end;
 
 PDualStringItemList = ^TDualStringItemList;
 TDualStringItemList = array[0..MaxListSize] of TDualStringItem;
 
 TCharSet = set of Char;
 
 TControlCallBack = procedure (Control: TControl);
 
 TSpecialFolder = (sfCustom, sfDesktop, sfFonts, sfPersonal, sfPrograms,
  sfRecent, sfSendTo, sfStartMenu, sfStartUp);
 
const
 SpecialFolders               : array[TSpecialFolder] of Integer = (-1,
  CSIDL_DESKTOP, CSIDL_FONTS, CSIDL_PERSONAL, CSIDL_PROGRAMS,
  CSIDL_RECENT, CSIDL_SENDTO, CSIDL_STARTMENU, CSIDL_STARTUP);
 
 CRLF                         = #13#10;
 
 csDigits                     = ['0'..'9'];
 csLowAlpha                   = ['a'..'z', 'ä', 'ö', 'ü', 'ß'];
 csHighAlpha                  = ['A'..'Z', 'Ä', 'Ö', 'Ü'];
 csAlpha                      = csLowAlpha + csHighAlpha;
 
 csAlphaNumeric               = csAlpha + csDigits;
 
 csUpperChars                 = [#1..'`', '{'..'™', '›', 'Ÿ', '£'..'´', '¶'..'Ý'];
 
 csWhiteSpace                 = [#1..#32];
 
 TicksPerSecond               = 1000;
 
resourcestring
 SCannotCreateDir             = 'Kann Verzeichnis "%s" nicht erstellen.';
 sErrorWritingFile            = 'Fehler beim Schreiben der Datei "%s". %s';
 sErrorReadingFile            = 'Fehler beim Lesen der Datei "%s". %s';
 sErrorCreatingFile           = 'Fehler beim Erstellen der Datei "%s". %s';
 sErrorOpeningFile            = 'Fehler beim Öffnen der Datei "%s". %s';
 
 sErrorMappingFile            = 'Fehler beim Erstellen eines Mappings der Datei %s. %s';
 
function IntToStr (const i: Integer): AnsiString;
{ Faster than the Delphi original IntToStr function. }

procedure ForEachControl (Control: TWinControl; ControlClass: TControlClass; CallBack: TControlCallBack);

procedure FreeAndNil (var Obj);

procedure ZeroMem (const pBuff: Pointer; const Count: Integer);
{ Initializes block of memory with zeros }

function Min (const a, b: LongInt): LongInt;
{ Returns the smaller value of 2 integers }

function Max (const a, b: LongInt): LongInt;
{ Returns the bigger value of 2 integers }

implementation

{ **************************************************************************** }

function IntToStr (const i: Integer): AnsiString;
{ Surprisingly faster than the Delphi original. }
begin
 Str (i, Result);
end;

{ ********** }

procedure ForEachControl (Control: TWinControl; ControlClass: TControlClass; CallBack: TControlCallBack);
var
 i                            : Integer;
 TmpControl                   : TControl;
begin
 with Control do
 begin
  for i := 0 to ControlCount - 1 do
  begin
   TmpControl := Controls[i];
   if (TmpControl is ControlClass) then
    CallBack (TmpControl);
   if TmpControl is TWinControl then
    ForEachControl (TWinControl (TmpControl), ControlClass, CallBack);
  end;
 end;
end;

{ ********** }

procedure FreeAndNil (var Obj);
var
 o                            : TObject;
begin
 o := TObject (Obj);
 TObject (Obj) := nil;
 o.Free;
end;

{ ********** }

procedure ZeroMem (const pBuff: Pointer; const Count: Integer);
asm
        MOV     ECX,EDX
        SAR     ECX,2
        JS      @@exit
        PUSH    EDI
        MOV     EDI,EAX { Point EDI to destination      }
        XOR     EAX,EAX
        REP     STOSD   { Fill count DIV 4 dwords       }
        MOV     ECX,EDX
        AND     ECX,3
        REP     STOSB   { Fill count MOD 4 bytes        }
        POP     EDI
@@exit:
end;

{ **************************************************************************** }
{ Integer minimum and maximum functions.
{ **************************************************************************** }

function Min (const a, b: LongInt): LongInt;
begin
 if a < b then
  Result := a
 else
  Result := b;
end; {End Function Minimum }

{ ********** }

function Max (const a, b: LongInt): LongInt;
begin
 if a > b then
  Result := a
 else
  Result := b;
end; {End Function Minimum }

end.

