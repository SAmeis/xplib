{$IFDEF Win_DOS}
{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I WinSysLib.inc}
{ Diretivas originais A+,B-,C-,D-,E-,F-,G+,H+,I-,J+,K-,L-,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y-,Z1 }

unit Win_DOS;

interface

uses Windows, SysUtils;

//Pega o endereco da porta paralela pela bios
function GetDOSLPTPortAddress(PortNo: integer): word; assembler; stdcall;
//Retorna a string com o valor da variavel e ambiente( Usar APIHnd.GetEnvironmentVar() )
function GetDOSEnvironmentVariable(EnvVar, Buffer: PChar; MaxLength: word): string; deprecated;

implementation

function GetDOSLPTPortAddress(PortNo: integer): word; assembler; stdcall;
//Pega o endereco da porta paralela pela bios
begin
	{$WARN UNSAFE_CODE OFF}  {$WARN NO_RETVAL OFF}
	asm
		push es
		push ebx
		mov ebx, PortNo
		shl ebx,1
		mov ax,40h //DOS segment adress
		mov es,ax
		mov ax,ES:[ebx+6] //get port adress in 16Bit way :)
		pop ebx
		pop es
	end;
	{$WARN UNSAFE_CODE ON}
end;{$WARN NO_RETVAL ON}

function GetDOSEnvironmentVariable(EnvVar, Buffer: PChar; MaxLength: word): string;
//Retorna a string com o valor da variavel e ambiente( Usar APIHnd.GetEnvironmentVar() )
{ ------------------------------------------------------------------------------------------------------------- }
var
	Match, pEnv, Pt: PChar;
begin
	{$IFDEF WIN32}
	pEnv := GetEnvironmentStrings;
	{$ELSE}
	pEnv := GetDosEnvironment;
	{$ENDIF}
	Match := StrAlloc(128);
	StrCopy(Match, EnvVar);
	StrCat(Match, '=');
	Match := StrUpper(Match);
	repeat
		if StrLen(pEnv) = 0 then begin
			Result := '';
			StrDispose(Match);
			Exit;
		end;
		Pt := StrPos(StrUpper(pEnv), Match);
		if Pt = nil then begin
			pEnv := StrEnd(pEnv);
			Inc(pEnv);
		end;
	until Pt <> nil;
	Pt := StrPos(StrUpper(pEnv), Match);
	Inc(Pt, StrLen(Match));
	StrDispose(Match);
	StrLCopy(Buffer, Pt, MaxLength);
	Result := StrPas(Pt);
end;

(*
///
///  how can i retrieve the computer's BIOS date with Delphi Pascal (16 bits version)?
///
///  I tried memory location Mem[$FFFF:$0005] but this gives a general protection error.
///
///  I made a DOS-program in Turbo Pascal 4.0 about 6 year's ago which retrieves 8 bytes starting at memory
///  location $FFFF:$0005. This 8 bytes contain the BIOS date of the computer. The program worked well with
///  the three generations of computers i owned (286, 486 Pentium 166).
///
///  But in Delphi i get a general protection failure at run time in the line marked with {*} in the function below.
///
///  function
///  BiosDate: string;
///  var
///  Offset: Word;
///  begin
///  Result := '';
///  for Offset := $0005 to $000C do
///  Result := Result + Char(Byte(Ptr($FFFF, Offset)^));  {*}
///  end;
///
///
///
///  Accepted Answer
///
///  From: peter_vc
///  Date: Monday, August 18 1997 - 10:09AM PDT
///
///
///  Text below...
///
///  Question History
///
///  Comment
///
///  From: peter_vc
///  Date: Friday, August 15 1997 - 01:26PM PDT
///
///  I think I have an easy solution, but I need to know if $FFFF:$0005 is really the correct location.  The
///  Undocumented PC and Undocumented DOS don't seem to have that info.
///
///
///
///
///
///  Comment
///
///  From: rj
///  Date: Saturday, August 16 1997 - 03:17AM PDT
///
///  Edited text of question
///
///
///
///  Comment
///
///  From: rj
///  Date: Saturday, August 16 1997 - 03:20AM PDT
///
///  i expanded my question about biosdate with some more info.
///
///
///
///  Accepted Answer
///
///  From: peter_vc
///  Date: Monday, August 18 1997 - 10:09AM PDT
///
///  Sorry this isn't as clean as it could be, but it works.
///
///  Be aware that you may have to call FreeSelector to deallocate the selector created with AllocSelector.
///
///  var
///  d : Byte;
///  dp : ^Byte;
///  c : LongInt;
///  I : integer;
///  date : array[0..7] of char;
///  begin
///      c := 1048565;  { $ffff:$0005 }
///      dp := CreateFarPointer(c,8);
///      for I:= 0 to 7 do begin
///          d := Byte(dp^);
///          Inc(dp);
///          date[I] := Chr(d);
///      end;
///  end;
///
///
///  function CreateFarPointer(phys:LongInt; length:Cardinal) : Pointer;
///  var
///      limit : Cardinal;
///      sel : Cardinal;
///  begin
///      limit := length - 1;
///
///      asm
///        MOV sel,DS
///      end;
///
///      sel := AllocSelector(sel);
///      SetSelectorBase(sel, phys);
///      SetSelectorLimit(sel, limit);
///      Result:=Ptr(sel,0);
///  end;
///
///
///
///
///  Comment
///
///  From: rj
///  Date: Tuesday, August 19 1997 - 01:30PM PDT
///
///  Thanks for your answer. You're a real expert.
///  I added your source to my project and it worked!
///
///  I have just one question: what is the meaning of the assembler line in your source (asm MOV sel,DS end;)?
///  I tried to run without this line and this gave also the same result.
///
///  bye, robbert-jan
///
///  Comment
///
///  From: peter_vc
///  Date: Tuesday, August 19 1997 - 07:07PM PDT
///
///  It stores the data segment register into the variable sel.  It just ensures that its using the proper
///  parameters to allocate the selector.  You should leave it in.
///
///  As you are testing it, put a call to it in a loop a few thousand times and let me know what happens.  I'm
///  wondering if a call to FreeSelector is needed.  I think you'll need to free it once your done with the
///  pointer.  I really should have done this, but ...
///
///  Credit for this actually goes to Andrew Shulman, author of Undocumented Windows and many other titles.
///
///
///  Comment
///
///  From: rj
///  Date: Wednesday, August 20 1997 - 11:42AM PDT
///
///  Hi,
///  I rewrited your code according to my programming standards. (see below). I tested the code with and
///  without the FreeSelector command, calling the routine 30000 times. Without FreeSelector the program is
///  crashing leaving Windows in an instable state. With the FreeSelector command there is no problem. So I
///  think your advise on using FreeSelector is a right one.
///
///  I have rated your solution as excellent! I also had dropped my question in the Pascal topic area, sou you
///  can get another 200 pionts by locking this question in that area.
///
///  If you have more comments, please contact me at RJM@XS4ALL.NL
///
///  { begin of code }
///
///  uses
///  WinProcs;
///
///  function fBiosDate: string;
///  {this routine returns the BIOS-date of the computer}
///  var
///      Selector: Word;
///      CharacterIndex: Integer;
///  begin
///      Result := '';
///
///      asm {this assembler code ensures a proper allocation of the Selector}
///        MOV Selector, DS;
///      end;
///
///      Selector := AllocSelector(Selector);
///      try
///          SetSelectorBase(Selector, 16*$FFFF + $0005);
///          SetSelectorLimit(Selector, 8 - 1);
///          for CharacterIndex := 0 to 7 do
///            Result := Result + Chr(Byte(Ptr(Selector, CharacterIndex)^));
///      finally
///          FreeSelector(Selector);
///      end;
///  end;
///  //end of code
*)
end.
