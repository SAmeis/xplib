unit rjRegExpr;
{
File:           rjRegExp.pas
Version:        0.1 / Initial Public Release
Last Modified:  1. April 2000
Decription:     TRegExpr searches a Buffer for a Regular Expression.

                This is a slightly modified and simplified version
                of the free mkRegEx.pas file written by
                Martin C. van der Kooij <mlwkooij@hetnet.nl>.

                His original package with full help and
                regular expression syntax explanation can be
                found at <http://jump.to/mkgal>.

Author:         Ralf Junker
E-Mail:         ralfjunker@gmx.de
Legal:          This unit is provided "as is" and without warranty of any kind.
                At the moment, it is freeware for private use only.
                I might, however, change this in the future. If you intend
                to use it commercially, please contact me via e-mail.
Bug Report:     If you encounter any bugs or want to suggest improvements,
                just get in touch using my e-mail above. When writing, please
                include the name and version of the unit you are referring to.
Thanks to:      * Martijn van der Kooij <mlwkooij@hetnet.nl>.

Copyright (c) 2000 Ralf Junker

Original Copyright Notice:

  Author: M.C. van der Kooij (MLWKooij@hetnet.nl)
  Translated to Delphi in may 1998

  Last modification: 20 - march - 2000

  Original regexpr.c

  Author: Tatu Ylonen <ylo@ngs.fi>

  Copyright (c) 1991 Tatu Ylonen, Espoo, Finland

  Permission to use, copy, modify, distribute, and sell this software \
  and its documentation for any purpose is hereby granted without \
  fee, provided that the above copyright notice appear in all copies. \
  This software is provided "as is" without express or implied \
  warranty.

  Created: Thu Sep 26 17:14:05 1991 ylo
  Last modified: Mon Nov  4 17:06:48 1991 ylo
  Ported to Think C: 19 Jan 1992 guido@cwi.nl

  This code draws many ideas from the regular expression packages by \
  Henry Spencer of the University of Toronto and Richard Stallman of \
  the Free Software Foundation.

  Emacs-specific code and syntax table code is almost directly borrowed \
  from GNU regexp.

  Bugs fixed and lots of reorganization by Jeffrey C. Ollie, April \
  1997 Thanks for bug reports and ideas from Andrew Kuchling, Tim \
  Peters, Guido van Rossum, Ka-Ping Yee, Sjoerd Mullender, and \
  probably one or two others that I'm forgetting.
}

interface

{$I rj.inc}

uses Classes, SysUtils;

resourcestring
 SreAbnormal                  = 'Abnormal error, contact author!!';
 SreAssertion                 = 'Assertion: ';
 SreBadMregN                  = 'Bad match register number.';
 SreBadlyPPar                 = 'Badly placed parenthesis.';
 SreBadlyPSpe                 = 'Badly placed special character.';
 SreEndPrem                   = 'Regular expression ends prematurel!';
 SreSyntax                    = 'Regular expression syntax error.';
 SreToComplex                 = 'Regular expression too complex.';
 SreOptimize                  = 'Optimization error.';
 SreUnknowRE                  = 'Unknown regex opcode: memory corrupted?';
 
const
 Re_NREGS                     = 100;
 
 //* bit definitions for syntax */
 Re_NO_BK_PARENS              = 1; //* no quoting for parentheses */
 Re_NO_BK_VBAR                = 2; //* no quoting for vertical bar */
 Re_BK_PLUS_QM                = 4; //* quoting needed for + and ? */
 Re_TIGHT_VBAR                = 8; //* | binds tighter than ^ and $ */
 Re_NEWLINE_OR                = 16; //* treat newline (in expression) as or */
 Re_CONTEXT_INDEP_OPS         = 32; //* ^$?*+ are special in all contexts */
 Re_ANSI_HEX                  = 64; //* ansi sequences (\n etc) and \xhh */
 Re_NO_GNU_EXTENSIONS         = 128; //* no gnu extensions */
 
 //* definitions for some common regexp styles */
 Re_SYNTAX_AWK                = (Re_NO_BK_PARENS or Re_NO_BK_VBAR or Re_CONTEXT_INDEP_OPS);
 Re_SYNTAX_EGREP              = (Re_SYNTAX_AWK or Re_NEWLINE_OR);
 Re_SYNTAX_GREP               = (Re_BK_PLUS_QM or Re_NEWLINE_OR);
 Re_SYNTAX_EMACS              = 0;
 
 Sword                        = 1;
 Swhitespace                  = 2;
 Sdigit                       = 4;
 Soctaldigit                  = 8;
 Shexdigit                    = 16;
 
type
 
 TmkreSyntaxStyle = (mkRe_No_Bk_Parens, mkRe_No_Bk_Vbar, mkRe_Bk_Plus_Qm,
  mkRe_Tight_Vbar, mkRe_Newline_Or, mkRe_Context_Indep_Ops,
  mkRe_Ansi_Hex, mkRe_No_Gnu_Extensions);
 TmkreSyntaxStyles = set of TmkreSyntaxStyle;
 
 RegExp_syntax_op = { syntax codes for plain and quoted characters }
 (
  Rend, // special code for end of regexp */
  Rnormal, // normal character */
  Ranychar, //* any character except newline */
  Rquote, //* the quote character */
  Rbol, //* match beginning of line */
  Reol, //* match end of line */
  Roptional, //* match preceding expression optionally */
  Rstar, //* match preceding expr zero or more times */
  Rplus, //* match preceding expr one or more times */
  Ror, //* match either of alternatives */
  Ropenpar, //* opening parenthesis */
  Rclosepar, //* closing parenthesis */
  Rmemory, //* match memory register */
  Rextended_memory, //* \vnn to match registers 10-99 */
  Ropenset, //* open set.  Internal syntax hard-coded below. */
  //* the following are gnu extensions to "normal" regexp syntax */
  Rbegbuf, //* beginning of buffer */
  Rendbuf, //* end of buffer */
  RDigitChar, //* digit character */ RJ
  RNotDigitChar, //* not digit character */ RJ
  Rwordchar, //* word character */
  Rnotwordchar, //* not word character */
  Rwordbeg, //* beginning of word */
  Rwordend, //* end of word */
  Rwordbound, //* word bound */
  Rnotwordbound, //* not word bound */
  Rnum_ops
  );
 
 RegExp_Compiled_Ops = //* opcodes for compiled regexp */
 (
  CEnd, //* end of pattern reached */
  CBol, //* beginning of line */
  CEol, //* end of line */
  CSet, //* character set.  Followed by 32 bytes of set. */
  CExact, //* followed by a byte to match */
  CAnyChar, //* matches any character except newline */
  CStart_Memory, //* set register start addr (followed by reg number) */
  CEnd_Memory, //* set register end addr (followed by reg number) */
  Cmatch_memory, //* match a duplicate of reg contents (regnum follows)*/
  Cjump, //* followed by two bytes (lsb,msb) of displacement. */
  CStar_Jump, //* will change to jump/update_failure_jump at runtime */
  CFailuRe_Jump, //* jump to addr on failure */
  CUpdate_FailuRe_Jump, //* update topmost failure point and jump */
  CDummy_FailuRe_Jump, //* push a dummy failure point and jump */
  CBegBuf, //* match at beginning of buffer */
  CEndbuf, //* match at end of buffer */
  Cwordbeg, //* match at beginning of word */
  Cwordend, //* match at end of word */
  Cwordbound, //* match if at word boundary */
  Cnotwordbound, //* match if not at word boundary */
  CSyntaxSpec, //* matches syntax code (1 byte follows) */
  Cnotsyntaxspec, //* matches if syntax code does not match (1 byte follows) */
  Crepeat1
  );
 
type
 //CE_Desc_Begin(ERegularExpression)
 {
 This Exception is used in <%LINK TRegExpr%>}
 //CE_Desc_End
 ERegularExpression = class (Exception);
 
 PRegExp_t = ^TRegExp_t;
 TRegExp_t = record
  Buffer: string; {compiled pattern}
  FastMap: string; {fastmap[ch] is true if ch can start pattern}
  Translate: string; {translation to apply during compilation/matching}
  FastMap_Accurate: Boolean; {true if fastmap is valid}
  can_be_null: Char; {true if can match empty string}
  Uses_Registers: Boolean; {registers are used and need to be initialized}
  Num_Registers: Integer; {number of registers used}
  Anchor: Byte; {anchor: 0=none 1=begline 2=begbuf}
 end;
 
 PRegExpRegisters = ^TRegExpRegisters;
 TRegExpRegisters = record
  _Start: array[0..Re_NREGS - 1] of Integer;
  _End: array[0..Re_NREGS - 1] of Integer;
 end;
 
 TRegExpr = class
 private
  FStyle: Integer;
  FActive,
   FUseFastmap,
   FCanBeEmpty: Boolean;
  FNoChange,
   FStyleChange: Boolean;
  FSyntaxStyles: TmkreSyntaxStyles;
  FPattern: string; // uncompiled pattern
  RegExp_T: TRegExp_t;
  Re_Syntax_Table: array[0..255] of Char;
  Re_Compile_Initialized: Boolean;
  RegExp_plain_ops,
   RegExp_quoted_ops: array[0..255] of RegExp_syntax_op;
  RegExp_precedences: array[0..Ord (Rnum_ops)] of Char;
  RegExp_context_indep_ops: Boolean;
  RegExp_ansi_sequences: Boolean;

  procedure CheckRegExp;
  procedure SetUseFastmap (fstm: Boolean);
  procedure SetCanBeEmpty (BeEm: Boolean);
  procedure SetSyntaxStyles (NewStyles: TmkreSyntaxStyles);
  procedure SetPattern (const pat: AnsiString);

  procedure Inser_Jump (Pos: Integer; opcode_type: RegExp_Compiled_Ops;
   addr: Integer; var pattern_offset: Integer; var Pattern: string);
  function Ansi_Translate (ch: Char; Size: Integer; var Pos: Integer;
   RegEx, Translate: string): Char;
  function Hex_Char_To_Decimal (ch: Char): Char;
  function Re_Optimize: Boolean;
  function re_optimize_star_jump (var Code: PChar): Boolean;
  function Re_Do_Compile_Fastmap (bufferstr: string; Pos: Integer;
   var can_be_null: Char;
   FastMap: PChar): Boolean;
  procedure Re_Compile_Fastmap_Aux (var Code: PChar; Pos: Integer;
   Visited: PChar; var can_be_null: Char;
   FastMap: PChar);
  procedure Re_Compile_FastMap;
  procedure Re_Compile_Initialize;
  function Re_Compile_Pattern: string;
  function Re_Match (const ABufferStart, ABufferPos, ABufferEnd: PChar; const Old_Regs: PRegExpRegisters): Boolean;

 public
  constructor Create;
  function MatchBuffer (const BufferStart: PChar; const BufferLength: Integer; const Registers: PRegExpRegisters): Boolean;

  property Translate: string read RegExp_T.Translate write RegExp_T.Translate;
  property RegExp: TRegExp_t read RegExp_T write RegExp_T; // we can copy this thing
 published
  property Active: Boolean read FActive write FActive;
  property UseFastmap: Boolean read FUseFastmap write SetUseFastmap;
  property CanBeEmpty: Boolean read FCanBeEmpty write SetCanBeEmpty;
  property Pattern: string read FPattern write SetPattern;
  property SyntaxStyles: TmkreSyntaxStyles read FSyntaxStyles write SetSyntaxStyles;
 end;
 
implementation

const
 STACK_PAGE_SIZE              = 256;
 Num_Registers                = 256;
 NUM_LEVELS                   = 5;
 MAX_NESTING                  = 100;
 
 {  The stack implementation is taken from an idea by Andrew Kuchling.
  * It's a doubly linked list of arrays. The advantages of this over a
  * simple linked list are that the number of mallocs required are
  * reduced. It also makes it possible to statically allocate enough
  * space so that small patterns don't ever need to call malloc.
  *
  * The advantages over a single array is that is periodically
  * realloced when more space is needed is that we avoid ever copying
  * the stack. }
 
 {  item_t is the basic stack element.  Defined as a union of
  * structures so that both registers, failure points, and counters can
  * be pushed/popped from the stack.  There's nothing built into the
  * item to keep track of whether a certain stack item is a register, a
  * failure point, or a counter. }
 
type
 Pitem_t = ^Titem_t;
 Titem_t = record
  Reg_Num: Integer;
  Reg_Level: Integer;
  Reg_Start: PChar;
  Reg_End: PChar;
  fail_count: Integer;
  fail_level: Integer;
  fail_phantom: Integer;
  fail_code: PChar;
  fail_text: PChar;
 end;
 
 { A 'page' of stack items.}
 PItem_Page_T = ^TItem_Page_T;
 TItem_Page_T = record
  Items: array[0..Num_Registers] of Titem_t;
  Prev: PItem_Page_T;
  Next: PItem_Page_T;
 end;
 
 { Structure to encapsulate the stack.}
 TStack = record
  Index: Integer;
  Current: PItem_Page_T; // Pointer to the current page.
  First: TItem_Page_T; // First page is statically allocated.
 end;
 
 TMatch_State = record
  {   The number of registers that have been pushed onto the stack
      since the last failure point. }
  Count: Integer;
  {   The number of failure points on the stack.}
  Level: Integer;
  {   The number of failure points on the stack.}
  Point: Integer;
  {   Storage for the registers.  Each register consists of two
      pointers to characters.  So register N is represented as
      start[N] and end[N].  The pointers must be converted to
      offsets from the beginning of the string before returning the
      registers to the calling program. }
  _Start: array[0..Num_Registers] of PChar;
  _End: array[0..Num_Registers] of PChar;
  {   Keeps track of whether a register has changed recently.}
  Changed: array[0..Num_Registers] of Integer;
  {   index into the curent page.  If index == 0 and you need
      to pop an item, move to the previous page and set index
      = STACK_PAGE_SIZE - 1.  Otherwise decrement index to
      push a page. If index == STACK_PAGE_SIZE and you need
      to push a page move to the next page and set index =
      0. If there is no new next page, allocate a new page
      and link it in. Otherwise, increment index to push a
      page.}
  Stack: TStack;
 end;
 
constructor TRegExpr.Create;
begin
 FActive := False;
 FNoChange := False;
 FStyleChange := True;
 SetSyntaxStyles ([mkRe_No_Bk_Parens, mkRe_No_Bk_Vbar, mkRe_Context_Indep_Ops, mkRe_Newline_Or]);
 SetLength (RegExp_T.FastMap, 256);
 RegExp_T.Translate := '';
 RegExp_T.FastMap_Accurate := False;
 RegExp_T.can_be_null := #0;
 RegExp_T.Uses_Registers := True;
 RegExp_T.Anchor := 0;
 FUseFastmap := True;
end;

procedure TRegExpr.CheckRegExp;
begin
 if RegExp_T.Buffer = '' then
  raise Exception.Create ('No compiled pattern available.');
 if RegExp_T.Translate <> '' then
  if Length (RegExp_T.Translate) <> 256 then
   raise Exception.Create ('Translate table length error.');
 if RegExp_T.FastMap <> '' then
  if Length (RegExp_T.FastMap) <> 256 then
   raise Exception.Create ('Fastmap table length error.');
end;

procedure TRegExpr.SetUseFastmap (fstm: Boolean);
begin
 if fstm <> FUseFastmap then
 begin
  if fstm then
   SetLength (RegExp_T.FastMap, 256)
  else
   SetLength (RegExp_T.FastMap, 0);
  FStyleChange := True;
  FUseFastmap := fstm;
 end;
end;

procedure TRegExpr.SetCanBeEmpty (BeEm: Boolean);
begin
 if BeEm <> FCanBeEmpty then
 begin
  FStyleChange := True;
  FCanBeEmpty := BeEm;
 end;
end;

procedure TRegExpr.SetSyntaxStyles (NewStyles: TmkreSyntaxStyles);
const
 Syntax                       : array[Low (TmkreSyntaxStyle)..High (TmkreSyntaxStyle)] of Integer = (
  Re_NO_BK_PARENS, //* no quoting for parentheses */
  Re_NO_BK_VBAR, //* no quoting for vertical bar */
  Re_BK_PLUS_QM, //* quoting needed for + and ? */
  Re_TIGHT_VBAR, //* | binds tighter than ^ and $ */
  Re_NEWLINE_OR, //* treat newline as or */
  Re_CONTEXT_INDEP_OPS, //* ^$?*+ are special in all contexts */
  Re_ANSI_HEX, //* ansi sequences (\n etc) and \xhh */
  Re_NO_GNU_EXTENSIONS); //* no gnu extensions */
var
 i                            : TmkreSyntaxStyle;
begin
 if NewStyles <> FSyntaxStyles then
 begin
  FStyle := 0;
  for i := Low (TmkreSyntaxStyle) to High (TmkreSyntaxStyle) do
   if i in NewStyles then FStyle := FStyle + Syntax[i];
  FSyntaxStyles := NewStyles;
  FStyleChange := True;
  FNoChange := False;
 end;
end;

procedure New_State (var State: TMatch_State; NRegs: Integer);
var
 i                            : Integer;
begin
 for i := 0 to NRegs - 1 do
 begin
  State._Start[i] := nil;
  State._End[i] := nil;
  State.Changed[i] := 0;
 end;
 State.Stack.Current := @State.Stack.First;
 State.Stack.First.Prev := nil;
 State.Stack.First.Next := nil;
 State.Stack.Index := 0;
 State.Level := 0;
 State.Count := 0;
 State.Point := 0;
end;

// Free any memory that might have been malloc'd

procedure Free_state (var State: TMatch_State);
begin
 while (State.Stack.First.Next <> nil) do
 begin
  State.Stack.Current := State.Stack.First.Next;
  State.Stack.First.Next := State.Stack.Current.Next;
  Dispose (State.Stack.Current);
  State.Stack.Current := nil;
 end;
end;

function short (a: Word): Integer;
begin
 if (a > $7FFF) then
  Result := a - $10000
 else
  Result := a;
end;

procedure TRegExpr.Re_Compile_Fastmap_Aux (var Code: PChar; Pos: Integer;
 Visited: PChar; var can_be_null: Char;
 FastMap: PChar);
var
 a, b                         : Integer;
 syntaxcode                   : Char;
begin
 if Visited[Pos] <> #0 then Exit;
 Visited[Pos] := #1;
 while True do
 begin
  case RegExp_Compiled_Ops (Ord (Code[Pos])) of // > ord
   CEnd:
    begin
     can_be_null := #1;
     Exit;
    end;
   CBol,
    CBegBuf,
    CEndbuf,
    Cwordbeg,
    Cwordend,
    Cwordbound,
    Cnotwordbound:
    begin
     Inc (Pos);
     for a := 0 to 255 do
      FastMap[a] := #1;
    end;
   CSyntaxSpec:
    begin
     Inc (Pos);
     syntaxcode := Code[Pos];
     for a := 0 to 255 do
      if (Ord (Re_Syntax_Table[a]) and Ord (syntaxcode)) > 0 then // integer > ord
       FastMap[a] := #1;
     Exit;
    end;
   Cnotsyntaxspec:
    begin
     Inc (Pos);
     syntaxcode := Code[Pos];
     for a := 0 to 255 do
      if not ((Ord (Re_Syntax_Table[a]) and Ord (syntaxcode)) > 0) then // integer > ord
       FastMap[a] := #1;
     Exit;
    end;
   CEol:
    begin
     FastMap[10] := #1; // was 13
     //can match null, but only at end of buffer
     if can_be_null = #0 then can_be_null := #2;
     Exit;
    end;
   CSet:
    begin
     Inc (Pos);
     for a := 0 to 31 do
      if Code[Pos + a] <> #0 then
       for b := 0 to 7 do
        if (Ord (Code[Pos + a]) and (1 shl b)) > 0 then // integer > ord
         FastMap[(a shl 3) + b] := #1;
     Exit;
    end;
   CExact:
    begin
     Inc (Pos);
     FastMap[Ord (Code[Pos])] := #1; // integer > ord
     Exit;
    end;
   CAnyChar:
    begin
     for a := 0 to 255 do
      if a <> 10 then // was 13
       FastMap[a] := #1;
     Exit;
    end;
   CStart_Memory,
    CEnd_Memory: Inc (Pos, 2);
   Cmatch_memory:
    begin
     for a := 0 to 255 do
      FastMap[a] := #1;
     can_be_null := #1;
     Exit;
    end;
   Cjump,
    CDummy_FailuRe_Jump,
    CUpdate_FailuRe_Jump,
    CStar_Jump:
    begin
     Inc (Pos);
     a := Ord (Code[Pos]) + Ord (Code[Pos + 1]) shl 8; // integer > ord
     Inc (Pos, 2); // check a for sign!
     Pos := Pos + short (a);
     if Visited[Pos] <> #0 then
      {/* argh... the regexp contains empty loops.  This is not
          good, as this may cause a failure stack overflow when
          matching.  Oh well. */
       /* this path leads nowhere; pursue other paths. */}
      Exit;
     Visited[Pos] := #1;
    end;
   CFailuRe_Jump:
    begin
     Inc (Pos);
     a := Ord (Code[Pos]) + Ord (Code[Pos + 1]) shl 8; // integer > ord
     Inc (Pos, 2); // check a for sign!
     a := Pos + short (a);
     Re_Compile_Fastmap_Aux (Code, a, Visited, can_be_null, FastMap);
    end;
   Crepeat1: Inc (Pos, 3);
  else
   begin
    raise ERegularExpression.Create (SreUnknowRE);
   end;
  end;
 end;
end;

function TRegExpr.Re_Do_Compile_Fastmap (bufferstr: string; Pos: Integer;
 var can_be_null: Char;
 FastMap: PChar): Boolean;
var
 Small_Visited                : array[0..511] of Char;
 Ext_visited                  : string;
 Visited, Buffer              : PChar;
begin
 if Length (bufferstr) <= SizeOf (Small_Visited) then
  Visited := Small_Visited
 else
 begin
  SetLength (Ext_visited, Length (bufferstr));
  Visited := @Ext_visited[1];
 end;
 can_be_null := #0;
 FillChar (FastMap^, 256, 0);
 FillChar (Visited^, Length (bufferstr), 0);
 Buffer := @bufferstr[1];
 Re_Compile_Fastmap_Aux (Buffer, Pos, Visited, can_be_null, FastMap);
 Result := True;
end;

//CE_Desc_Begin(TRegExpr.re_compile_fastmap)
{This computes the fastmap for the regexp.  For this to have any effect, \
the calling program must have initialized the fastmap field to point \
to an array of 256 characters.}
//CE_Desc_End

procedure TRegExpr.Re_Compile_FastMap;
begin
 if (RegExp_T.FastMap = '') or (RegExp_T.FastMap_Accurate) then Exit;
 if not (Re_Do_Compile_Fastmap (RegExp_T.Buffer, 0, RegExp_T.can_be_null, PChar (RegExp_T.FastMap))) then
  Exit;
 if RegExp_T.Buffer[1] = Char (CBol) then
  RegExp_T.Anchor := 1 //begline
 else
  if RegExp_T.Buffer[1] = Char (CBegBuf) then
   RegExp_T.Anchor := 2 //begbuf
  else
   RegExp_T.Anchor := 0; //none
 RegExp_T.FastMap_Accurate := True;
end;

//CE_Desc_Begin(TRegExpr.re_optimize_star_jump)
{star is coded as:
1: failure_jump 2
   ... code for operand of star
   star_jump 1
2: ... code after star

We change the star_jump to update_failure_jump if we can determine \
that it is safe to do so; otherwise we change it to an ordinary \
jump.

plus is coded as

    jump 2
1: failure_jump 3
2: ... code for operand of plus
   star_jump 1
3: ... code after plus

For star_jump considerations this is processed identically to star.
*
}
//CE_Desc_End

function TRegExpr.re_optimize_star_jump (var Code: PChar): Boolean;
label
 make_normal_jump, loop_p1;
var
 map                          : array[0..255] of Char;
 can_be_null                  : Char;
 p1, p2                       : PChar;
 ch                           : Char;
 a, b                         : Integer;
 num_instructions             : Integer;
begin
 Result := False;
 num_instructions := 0;
 
 a := short (Byte (Code[0]) + Byte (Code[1]) * 256);
 
 Inc (Code, 2);
 p1 := Code + a + 3; //skip the failure_jump
 //Check that the jump is within the pattern
 if (p1 < @RegExp_T.Buffer[1]) or
  ((Integer (@RegExp_T.Buffer[1]) + Length (RegExp_T.Buffer)) < Integer (p1)) then
  Exit;
 Assert ((p1[-3] = Char (CFailuRe_Jump)), 'No Cfailure_jump');
 p2 := Code;
 //p1 points inside loop, p2 points to after loop
 if not Re_Do_Compile_Fastmap (RegExp_T.Buffer, Integer (p2) - Integer (@RegExp_T.Buffer[1]), can_be_null, map) then
  goto make_normal_jump;
 {/* If we might introduce a new update point inside the
   * loop, we can't optimize because then update_jump would
   * update a wrong failure point.  Thus we have to be
   * quite careful here.
   */}
 
   //loop until we find something that consumes a character
 loop_p1:
 Inc (num_instructions);
 case RegExp_Compiled_Ops (Ord (p1[0])) of
  CBol,
   CEol,
   CBegBuf,
   CEndbuf,
   Cwordbeg,
   Cwordend,
   Cwordbound,
   Cnotwordbound:
   begin
    Inc (p1);
    goto loop_p1;
   end;
  CStart_Memory,
   CEnd_Memory:
   begin
    Inc (p1, 2);
    goto loop_p1;
   end;
  CExact:
   begin
    Inc (p1);
    ch := p1[0];
    Inc (p1);
    if (map[Ord (ch)]) <> #0 then
     goto make_normal_jump;
   end;
  CAnyChar:
   begin
    Inc (p1);
    for b := 0 to 255 do
     if (b <> 10) and (map[b] <> #0) then // was 13
      goto make_normal_jump;
   end;
  CSet:
   begin
    Inc (p1);
    for b := 0 to 255 do
     // Check This!!!!
     if ((Ord (p1[b div 8]) and (1 shl (b and 7))) <> 0) and // integer > ord
     (map[b] <> #0) then
      goto make_normal_jump;
    Inc (p1, 32);
   end;
 else
  goto make_normal_jump;
 end;
 
 //now we know that we can't backtrack.
 while p1 <> (p2 - 3) do
 begin
  Inc (num_instructions);
  case RegExp_Compiled_Ops (Ord (p1[0])) of
   CEnd: Exit;
   CBol,
    CEol,
    CAnyChar,
    CBegBuf,
    CEndbuf,
    Cwordbeg,
    Cwordend,
    Cwordbound,
    Cnotwordbound: Inc (p1);
   CSet: Inc (p1, 33);
   CExact,
    CStart_Memory,
    CEnd_Memory,
    Cmatch_memory,
    CSyntaxSpec,
    Cnotsyntaxspec: Inc (p1, 2);
   Cjump,
    CStar_Jump,
    CFailuRe_Jump,
    CUpdate_FailuRe_Jump,
    CDummy_FailuRe_Jump: goto make_normal_jump;
  else
   Exit;
  end;
 end;
 //make_update_jump:
 Dec (Code, 3);
 Inc (a, 3);
 Code[0] := Char (CUpdate_FailuRe_Jump);
 Code[1] := Char (a and 255);
 Code[2] := Char (a shr 8);
 if num_instructions > 1 then
 begin
  Result := True;
  Exit;
 end;
 Assert (num_instructions = 1, 'No instructions found!');
 {/* if the only instruction matches a single character, we can do
 * better */}
 p1 := Code + 3 + a; //start of sole instruction
 if (p1[0] = Char (CSet)) or (p1[0] = Char (CExact)) or (p1[0] = Char (CAnyChar)) or
  (p1[0] = Char (CSyntaxSpec)) or (p1[0] = Char (Cnotsyntaxspec)) then
  Code[0] := Char (Crepeat1);
 Result := True;
 Exit;
 
 make_normal_jump:
 Dec (Code, 3);
 Code[0] := Char (Cjump);
 Result := True;
end;

function TRegExpr.Re_Optimize: Boolean;
var
 Code                         : PChar;
begin
 Result := False;
 Code := @RegExp_T.Buffer[1];
 while True do
 begin
  case RegExp_Compiled_Ops (Ord (Code[0])) of
   CEnd:
    begin
     Result := True;
     Exit;
    end;
   CAnyChar,
    CBol,
    CEol,
    CBegBuf,
    CEndbuf,
    Cwordbeg,
    Cwordend,
    Cwordbound,
    Cnotwordbound: Inc (Code);
   CSet: Inc (Code, 33);
   CExact,
    CStart_Memory,
    CEnd_Memory,
    Cmatch_memory,
    CSyntaxSpec,
    Cnotsyntaxspec: Inc (Code, 2);
   CStar_Jump:
    begin
     Inc (Code);
     if not re_optimize_star_jump (Code) then Exit;
    end;
   CUpdate_FailuRe_Jump,
    Cjump,
    CDummy_FailuRe_Jump,
    CFailuRe_Jump,
    Crepeat1: Inc (Code, 3);
  else
   Exit;
  end;
 end;
end;

function TRegExpr.Hex_Char_To_Decimal (ch: Char): Char;
begin
 Result := #16; // error
 if (ch >= '0') and (ch <= '9') then
  Result := Char (Ord (ch) - Ord ('0'));
 if (ch >= 'a') and (ch <= 'f') then
  Result := Char (Ord (ch) - Ord ('a') + 10);
 if (ch >= 'A') and (ch <= 'F') then
  Result := Char (Ord (ch) - Ord ('A') + 10);
end;

function TRegExpr.Ansi_Translate (ch: Char; Size: Integer; var Pos: Integer;
 RegEx, Translate: string): Char;
var
 GetHex_Ch, GetHex_Value      : Char;
begin
 Result := #0;
 case ch of
  'a': Result := #7; // audible bell
  'b': Result := #8; // backspace
  'f': Result := #12; // form feed
  'n': Result := #10; // line feed
  'r': Result := #13; // carriage return
  't': Result := #9; // tab
  'v': Result := #11; // vertical tab
  'x':
   begin // hex code
    if Pos > Size then
     raise ERegularExpression.Create ('Regular expression ends prematurely');
    GetHex_Ch := RegEx[Pos];
    Inc (Pos);
    GetHex_Value := Hex_Char_To_Decimal (GetHex_Ch);
    if (GetHex_Value = #16) then
     raise ERegularExpression.Create ('');
    if Pos > Size then
     raise ERegularExpression.Create ('Regular expression ends prematurely');
    GetHex_Ch := RegEx[Pos];
    Inc (Pos);
    GetHex_Ch := Hex_Char_To_Decimal (GetHex_Ch);
    if (GetHex_Value = #16) then
     raise ERegularExpression.Create ('');
    Result := Char (Ord (GetHex_Value) * 16 + Ord (GetHex_Ch));
   end;
 else
  if Translate <> '' then
   Result := Translate[Ord (ch)];
 end;
end;

procedure TRegExpr.Re_Compile_Initialize;
var
 a, i                         : Integer;
begin
 for a := Ord ('a') to Ord ('z') do
  Re_Syntax_Table[a] := Char (Sword);
 for a := Ord ('A') to Ord ('Z') do
  Re_Syntax_Table[a] := Char (Sword);
 for a := Ord ('0') to Ord ('9') do
  Re_Syntax_Table[a] := Char (Sword or Sdigit or Shexdigit);
 for a := Ord ('0') to Ord ('7') do
  Re_Syntax_Table[a] := Char (Ord (Re_Syntax_Table[a]) + Soctaldigit); // integer > ord
 for a := Ord ('a') to Ord ('f') do
  Re_Syntax_Table[a] := Char (Ord (Re_Syntax_Table[a]) + Shexdigit); // integer > ord
 for a := Ord ('A') to Ord ('F') do
  Re_Syntax_Table[a] := Char (Ord (Re_Syntax_Table[a]) + Shexdigit); // integer > ord
 Re_Syntax_Table[Ord ('_')] := Char (Sword);
 for a := 9 to 13 do
  Re_Syntax_Table[a] := Char (Swhitespace);
 Re_Syntax_Table[Ord (' ')] := Char (Swhitespace);
 
 for i := 0 to 255 do
 begin
  RegExp_plain_ops[i] := Rnormal;
  RegExp_quoted_ops[i] := Rnormal;
 end;
 for a := Ord ('0') to Ord ('9') do
  RegExp_quoted_ops[a] := Rmemory;
 RegExp_plain_ops[Ord ('\')] := Rquote;
 if (FStyle and Re_NO_BK_PARENS) = Re_NO_BK_PARENS then
 begin
  RegExp_plain_ops[Ord ('(')] := Ropenpar;
  RegExp_plain_ops[Ord (')')] := Rclosepar;
 end
 else
 begin
  RegExp_quoted_ops[Ord ('(')] := Ropenpar;
  RegExp_quoted_ops[Ord (')')] := Rclosepar;
 end;
 
 if (FStyle and Re_NO_BK_VBAR) = Re_NO_BK_VBAR then
  RegExp_plain_ops[Ord ('|')] := Ror
 else
  RegExp_quoted_ops[Ord ('|')] := Ror;
 RegExp_plain_ops[Ord ('*')] := Rstar;
 if (FStyle and Re_BK_PLUS_QM) = Re_BK_PLUS_QM then
 begin
  RegExp_quoted_ops[Ord ('+')] := Rplus;
  RegExp_quoted_ops[Ord ('?')] := Roptional;
 end
 else
 begin
  RegExp_plain_ops[Ord ('+')] := Rplus;
  RegExp_plain_ops[Ord ('?')] := Roptional;
 end;
 
 if (FStyle and Re_NEWLINE_OR) = Re_NEWLINE_OR then
  RegExp_plain_ops[10] := Ror; // was 13
 RegExp_plain_ops[Ord ('[')] := Ropenset;
 RegExp_plain_ops[Ord ('^')] := Rbol;
 RegExp_plain_ops[Ord ('$')] := Reol;
 RegExp_plain_ops[Ord ('.')] := Ranychar;
 if not ((FStyle and Re_NO_GNU_EXTENSIONS) = Re_NO_GNU_EXTENSIONS) then
 begin
  RegExp_quoted_ops[Ord ('d')] := RDigitChar; // RJ
  RegExp_quoted_ops[Ord ('D')] := RNotDigitChar; // RJ
  RegExp_quoted_ops[Ord ('w')] := Rwordchar;
  RegExp_quoted_ops[Ord ('W')] := Rnotwordchar;
  RegExp_quoted_ops[Ord ('<')] := Rwordbeg;
  RegExp_quoted_ops[Ord ('>')] := Rwordend;
  RegExp_quoted_ops[Ord ('b')] := Rwordbound;
  RegExp_quoted_ops[Ord ('B')] := Rnotwordbound;
  RegExp_quoted_ops[Ord ('`')] := Rbegbuf;
  RegExp_quoted_ops[44] := Rendbuf; // '
 end;
 if (FStyle and Re_ANSI_HEX) = Re_ANSI_HEX then
  RegExp_quoted_ops[Ord ('v')] := Rextended_memory;
 
 for a := 0 to Ord (Rnum_ops) - 1 do
  RegExp_precedences[a] := #4;
 if (FStyle and Re_TIGHT_VBAR) > 0 then
 begin
  RegExp_precedences[Ord (Ror)] := #3;
  RegExp_precedences[Ord (Rbol)] := #2;
  RegExp_precedences[Ord (Reol)] := #2;
 end
 else
 begin
  RegExp_precedences[Ord (Ror)] := #2;
  RegExp_precedences[Ord (Rbol)] := #3;
  RegExp_precedences[Ord (Reol)] := #3;
 end;
 RegExp_precedences[Ord (Rclosepar)] := #1;
 RegExp_precedences[Ord (Rend)] := #0;
 RegExp_context_indep_ops := (FStyle and Re_CONTEXT_INDEP_OPS) > 0;
 RegExp_ansi_sequences := (FStyle and Re_ANSI_HEX) > 0;
 
 Re_Compile_Initialized := True;
end;

procedure TRegExpr.Inser_Jump (Pos: Integer; opcode_type: RegExp_Compiled_Ops;
 addr: Integer; var pattern_offset: Integer; var Pattern: string);
var
 a, disp                      : Integer;
begin
 for a := pattern_offset - 1 downto Pos do
  Pattern[a + 3] := Pattern[a];
 Pattern[Pos] := Char (opcode_type);
 //PUT_ADDR(offset,addr)
 disp := addr - (Pos + 1) - 2;
 Pattern[Pos + 1] := Char (disp and 255);
 Pattern[Pos + 2] := Char ((disp shr 8) and 255);
 Inc (pattern_offset, 3);
end;

//CE_Desc_Begin(TRegExpr.re_compile_pattern)
{This compiles the regexp (given in regex and length in regex_size).
This empty string if the regexp compiled successfully, and an error \
message if an error was encountered.
The translate field must be set to point to a valid translation table, \
or empty if it is not used.}
//CE_Desc_End

function TRegExpr.Re_Compile_Pattern: string;
label
 normal_char, stoRe_opcode_and_arg, stoRe_opcode;
var
 i, Pos, current_level, Level : Integer;
 op                           : RegExp_syntax_op;
 opcode                       : RegExp_Compiled_Ops;
 pattern_offset               : Integer;
 starts                       : array[0..NUM_LEVELS * MAX_NESTING] of Integer;
 starts_base                  : Integer;
 futuRe_jumps                 : array[0..MAX_NESTING] of Integer;
 num_jumps                    : Integer;
 a, ch                        : Char;
 Pattern                      : string;
 Translate                    : string;
 next_register                : Integer;
 paren_depth                  : Integer;
 num_open_registers           : Integer;
 open_registers               : array[0..Re_NREGS] of Integer;
 beginning_context            : Boolean;
 
 Size, disp                   : Integer;
 
 complement, firstchar, Range : Boolean;
 Prev, OffSet                 : Integer;
begin
 pattern_offset := 0;
 ch := #0;
 if not Re_Compile_Initialized then
  Re_Compile_Initialize;
 RegExp_T.FastMap_Accurate := False;
 RegExp_T.Uses_Registers := True;
 RegExp_T.Num_Registers := 1;
 Translate := RegExp_T.Translate;
 Pattern := '';
 pattern_offset := 1;
 try
  starts_base := 0;
  num_jumps := 0;
  current_level := 0;
  starts[starts_base + current_level] := pattern_offset; {SET_LEVEL_START}
  num_open_registers := 0;
  next_register := 1;
  paren_depth := 0;
  beginning_context := True;
  op := Rnum_ops; // maybe wrong, just give it a try
  {we use Rend dummy to ensure that pending jumps are updated
   (due to low priority of Rend) before exiting the loop.}
  Size := Length (FPattern);
  Pos := 1;
  while op <> Rend do
  begin
   if Pos > Size then
    op := Rend
   else
   begin
    if Pos > Size then
     raise ERegularExpression.Create (SreEndPrem);
    ch := FPattern[Pos];
    Inc (Pos);
    if Translate <> '' then ch := Translate[Ord (ch)];
    op := RegExp_plain_ops[Ord (ch)];
    if op = Rquote then
    begin
     if Pos > Size then
      raise ERegularExpression.Create (SreEndPrem);
     ch := FPattern[Pos];
     Inc (Pos);
     op := RegExp_quoted_ops[Ord (ch)];
     if (op = Rnormal) and RegExp_ansi_sequences then
      ch := Ansi_Translate (ch, Size, Pos, FPattern, Translate);
    end;
   end;
   Level := Ord (RegExp_precedences[Ord (op)]); // integer > ord
   if (Level > current_level) then
   begin
    Inc (current_level); // before or after the while??
    while current_level < Level do
    begin
     starts[starts_base + current_level] := pattern_offset;
     Inc (current_level);
    end;
    starts[starts_base + current_level] := pattern_offset;
   end
   else
    if Level < current_level then
    begin
     current_level := Level;
     while (num_jumps > 0) and
      (futuRe_jumps[num_jumps - 1] >= starts[starts_base + current_level]) do
     begin
      //PUT_ADDR(offset,addr)
      //         offset = future_jumps[num_jumps-1]
      //         addr   = pattern_offset
      disp := pattern_offset - futuRe_jumps[num_jumps - 1] - 2;
      Pattern[futuRe_jumps[num_jumps - 1]] := Char (disp and 255);
      Pattern[futuRe_jumps[num_jumps - 1] + 1] := Char ((disp shr 8) and 255);
      Dec (num_jumps);
     end;
    end;

   case op of
    Rend: ;
    Rnormal:
     begin
      normal_char:
      opcode := CExact;
      stoRe_opcode_and_arg: //* opcode & ch must be set */
      starts[starts_base + current_level] := pattern_offset;
      Pattern := Pattern + Char (opcode) + ch;
      Inc (pattern_offset, 2);
     end;
    Ranychar:
     begin
      opcode := CAnyChar;
      stoRe_opcode:
      starts[starts_base + current_level] := pattern_offset;
      Pattern := Pattern + Char (opcode);
      Inc (pattern_offset);
     end;
    Rquote:
     raise ERegularExpression.Create (SreAbnormal);
    Rbol:
     begin
      if not beginning_context then
       if RegExp_context_indep_ops then
        raise ERegularExpression.Create (SreBadlyPSpe)
       else
        goto normal_char;
      opcode := CBol;
      goto stoRe_opcode;
     end;
    Reol:
     begin
      if not ((Pos > Size) or
       (((FStyle and Re_NO_BK_VBAR) = Re_NO_BK_VBAR) and (FPattern[Pos] = #124)) or // oct 174
       (((FStyle and Re_NO_BK_VBAR) <> Re_NO_BK_VBAR) and (((Pos + 1) < Size) and
       (FPattern[Pos] = #92) and (FPattern[Pos + 1] = #124))) or // oct 92 / 174
       (((FStyle and Re_NO_BK_PARENS) = Re_NO_BK_PARENS) and (FPattern[Pos] = ')')) or
       (((FStyle and Re_NO_BK_PARENS) <> Re_NO_BK_PARENS) and (((Pos + 1) < Size) and
       (FPattern[Pos] = #92) and (FPattern[Pos + 1] = ')'))) // oct 92
       ) then
       if RegExp_context_indep_ops then
        raise ERegularExpression.Create (SreBadlyPSpe)
       else
        goto normal_char;
      opcode := CEol;
      goto stoRe_opcode;
     end;
    Roptional:
     begin
      if beginning_context then
       if RegExp_context_indep_ops then
        raise ERegularExpression.Create (SreBadlyPSpe)
       else
        goto normal_char;
      if starts[starts_base + current_level] <> pattern_offset then
      begin
       Pattern := Pattern + #0#0#0;
       Inser_Jump (starts[starts_base + current_level], CFailuRe_Jump,
        pattern_offset + 3, pattern_offset, Pattern);
      end;
     end;
    Rstar,
     Rplus:
     begin
      if beginning_context then
       if RegExp_context_indep_ops then
        raise ERegularExpression.Create (SreBadlyPSpe)
       else
        goto normal_char;

      if starts[starts_base + current_level] <> pattern_offset then
      begin //* ignore empty patterns for + and * */
       Pattern := Pattern + #0#0#0#0#0#0; //#0#0#0; //ALLOC(9);
       Inser_Jump (starts[starts_base + current_level], CFailuRe_Jump,
        pattern_offset + 6, pattern_offset, Pattern);
       Inser_Jump (pattern_offset, CStar_Jump,
        starts[starts_base + current_level], pattern_offset, Pattern);
       if op = Rplus then
       begin //* jump over initial failure_jump */
        Pattern := Pattern + #0#0#0;
        Inser_Jump (starts[starts_base + current_level], CDummy_FailuRe_Jump,
         starts[starts_base + current_level] + 6, pattern_offset, Pattern);
       end {
        else
          SetLength(pattern, Length(pattern) - 3)}; // weer verwijderen van drie codes
      end;
     end;
    Ror:
     begin
      Pattern := Pattern + #0#0#0#0#0#0; //ALLOC(6);
      Inser_Jump (starts[starts_base + current_level], CFailuRe_Jump,
       pattern_offset + 6, pattern_offset, Pattern);
      if num_jumps >= MAX_NESTING then
       raise ERegularExpression.Create (SreToComplex);
      Pattern[pattern_offset] := Char (Cjump);
      Inc (pattern_offset);
      futuRe_jumps[num_jumps] := pattern_offset;
      Inc (num_jumps);
      //              pattern := pattern + #0#0;
      Inc (pattern_offset, 2);
      starts[starts_base + current_level] := pattern_offset;
     end;
    Ropenpar:
     begin
      starts[starts_base + current_level] := pattern_offset;
      if next_register < Re_NREGS then
      begin
       RegExp_T.Uses_Registers := True;
       Pattern := Pattern + Char (CStart_Memory) + Char (next_register);
       Inc (pattern_offset, 2);
       open_registers[num_open_registers] := next_register;
       Inc (num_open_registers);
       RegExp_T.Num_Registers := RegExp_T.Num_Registers + 1;
       Inc (next_register);
      end;
      Inc (paren_depth);
      //PUSH_LEVEL_STARTS;
      if starts_base < ((MAX_NESTING - 1) * NUM_LEVELS) then
       starts_base := starts_base + NUM_LEVELS
      else
       raise ERegularExpression.Create (SreToComplex);

      current_level := 0;
      starts[starts_base + current_level] := pattern_offset;
     end;
    Rclosepar:
     begin
      if paren_depth <= 0 then
       raise ERegularExpression.Create (SreBadlyPPar);
      Dec (starts_base, NUM_LEVELS);
      current_level := Ord (RegExp_precedences[Ord (Ropenpar)]); // integer > ord
      Dec (paren_depth);
      if paren_depth < num_open_registers then
      begin
       RegExp_T.Uses_Registers := True;
       Dec (num_open_registers);
       Pattern := Pattern + Char (CEnd_Memory) + Char (open_registers[num_open_registers]);
       Inc (pattern_offset, 2);
      end;
     end;
    Rmemory:
     begin
      if ch = '0' then
       raise ERegularExpression.Create (SreBadMregN);
      RegExp_T.Uses_Registers := True;
      opcode := Cmatch_memory;
      ch := Char (Ord (ch) - Ord ('0'));
      goto stoRe_opcode_and_arg;
     end;
    Rextended_memory:
     begin
      if Pos > Size then
       raise ERegularExpression.Create (SreEndPrem);
      ch := FPattern[Pos];
      Inc (Pos);
      if (ch < '0') or (ch > '9') then
       raise ERegularExpression.Create (SreBadMregN);
      if Pos > Size then
       raise ERegularExpression.Create (SreEndPrem);
      a := FPattern[Pos];
      Inc (Pos);
      if (a < '0') or (a > '9') then
       raise ERegularExpression.Create (SreBadMregN);
      ch := Char (10 * (Ord (a) - Ord ('0')) + Ord (ch) - Ord ('0'));
      if (ch <= '0') or (ch >= Char (Re_NREGS)) then
       raise ERegularExpression.Create (SreBadMregN);
      RegExp_T.Uses_Registers := True;
      opcode := Cmatch_memory;
      goto stoRe_opcode_and_arg;
     end;
    Ropenset:
     begin
      starts[starts_base + current_level] := pattern_offset; //   SET_LEVEL_START;
      //   ALLOC(1+256/8);
      Pattern := Pattern + Char (CSet);
      Inc (pattern_offset);
      OffSet := pattern_offset;
      Pattern := Pattern + #0#0#0#0#0#0#0#0 + #0#0#0#0#0#0#0#0 +
       #0#0#0#0#0#0#0#0 + #0#0#0#0#0#0#0#0;
      Inc (pattern_offset, 32);
      if Pos > Size then
       raise ERegularExpression.Create (SreEndPrem);
      ch := FPattern[Pos];
      Inc (Pos);
      if Translate <> '' then ch := Translate[Ord (ch)];
      if ch = '^' then
      begin
       complement := True;
       if Pos > Size then
        raise ERegularExpression.Create (SreEndPrem);
       ch := FPattern[Pos];
       Inc (Pos);
       if Translate <> '' then ch := Translate[Ord (ch)];
      end
      else
       complement := False;
      Prev := -1;
      Range := False;
      firstchar := True;
      while (ch <> #93) or firstchar do
      begin //was oct
       firstchar := False;
       if (RegExp_ansi_sequences and (ch = #92)) then
       begin // was oct
        if Pos > Size then
         raise ERegularExpression.Create (SreEndPrem);
        ch := FPattern[Pos];
        Inc (Pos);
        Ansi_Translate (ch, Size, Pos, FPattern, Translate);
       end;
       if Range then
       begin
        for i := Prev to Ord (ch) do
         Pattern[OffSet + (i div 8)] := Char (Ord (Pattern[OffSet + (i div 8)]) or (1 shl (i and 7))); // integer > ord
        Prev := -1;
        Range := False;
       end
       else
        if (Prev <> -1) and (ch = '-') then
         Range := True
        else
        begin
         Pattern[OffSet + (Ord (ch) div 8)] := Char (Ord (Pattern[OffSet + (Ord (ch) div 8)]) or (1 shl (Ord (ch) and 7))); // integer > ord
         Prev := Ord (ch);
        end;
       if Pos > Size then
        raise ERegularExpression.Create (SreEndPrem);
       ch := FPattern[Pos];
       Inc (Pos);
       Ansi_Translate (ch, Size, Pos, FPattern, Translate);
      end;
      if Range then
       Pattern[OffSet + (Ord ('-') div 8)] := Char (Ord (Pattern[OffSet + (Ord ('-') div 8)]) or (1 shl (Ord ('-') and 7))); // integer > ord
      if (complement) then
      begin
       for i := 0 to 256 div 8 do
        Pattern[OffSet + i] := Char (Ord (Pattern[OffSet + i]) xor 255); // integer > ord
      end;
     end;
    Rbegbuf:
     begin
      opcode := CBegBuf;
      goto stoRe_opcode;
     end;
    Rendbuf:
     begin
      opcode := CEndbuf;
      goto stoRe_opcode;
     end;
    RDigitChar:
     begin // RJ // RJ
      opcode := CSyntaxSpec; // RJ
      ch := Char (Sdigit); // RJ
      goto stoRe_opcode_and_arg; // RJ
     end;
    RNotDigitChar:
     begin // RJ // RJ
      opcode := Cnotsyntaxspec; // RJ
      ch := Char (Sdigit); // RJ
      goto stoRe_opcode_and_arg; // RJ
     end; // RJ
    Rwordchar:
     begin
      opcode := CSyntaxSpec;
      ch := Char (Sword);
      goto stoRe_opcode_and_arg;
     end;
    Rnotwordchar:
     begin
      opcode := Cnotsyntaxspec;
      ch := Char (Sword);
      goto stoRe_opcode_and_arg;
     end;
    Rwordbeg:
     begin
      opcode := Cwordbeg;
      goto stoRe_opcode;
     end;
    Rwordend:
     begin
      opcode := Cwordend;
      goto stoRe_opcode;
     end;
    Rwordbound:
     begin
      opcode := Cwordbound;
      goto stoRe_opcode;
     end;
    Rnotwordbound:
     begin
      opcode := Cnotwordbound;
      goto stoRe_opcode;
     end;
   else
    raise ERegularExpression.Create (SreSyntax);
   end;
   beginning_context := (op = Ropenpar) or (op = Ror);
  end;
  if starts_base <> 0 then
   raise ERegularExpression.Create (SreBadlyPPar);
  Pattern := Pattern + Char (CEnd);
  Inc (pattern_offset);
 finally
  RegExp_T.Buffer := Pattern;
 end;
 if not Re_Optimize then
  raise ERegularExpression.Create (SreOptimize);
end;

//CE_Desc_Begin(TRegExpr.re_match)
{This tries to match the regexp against the string. This returns the \
length of the matched portion, or -1 if the pattern could not be \
matched and -2 if an error (such as failure stack overflow) is \
encountered.}
//CE_Desc_End

function TRegExpr.MatchBuffer (const BufferStart: PChar; const BufferLength: Integer; const Registers: PRegExpRegisters): Boolean;
var
 FastMap                      : PChar;
 Translate                    : PChar;
 BufferPos, BufferEnd         : PChar;
 Anchor                       : Byte;
begin
 BufferPos := BufferStart;
 BufferEnd := BufferPos + BufferLength;
 
 if RegExp_T.FastMap <> '' then // FastMap
  FastMap := @RegExp_T.FastMap[1]
 else
  FastMap := nil;
 
 if RegExp_T.Translate <> '' then
 begin // Translate
  Translate := @RegExp_T.Translate[1];
  Dec (Translate);
 end
 else
  Translate := nil;
 
 if (FastMap <> nil) and (not RegExp_T.FastMap_Accurate) then Re_Compile_FastMap;
 
 Anchor := RegExp_T.Anchor;
 
 if RegExp_T.can_be_null = #1 then FastMap := nil; //can_be_null == 2: can match null at eob
 
 while BufferPos < BufferEnd do
 begin
  if FastMap <> nil then
  begin
   if Translate <> nil then
   begin
    while (BufferPos <> BufferEnd) and not (FastMap[Ord (Translate[Ord (BufferPos[0])])] > #0) do
     Inc (BufferPos);
   end
   else
    while (BufferPos <> BufferEnd) and not (FastMap[Ord (BufferPos[0])] > #0) do
     Inc (BufferPos);

   if (BufferPos = BufferEnd) and (RegExp_T.can_be_null = #0) then
   begin
    Result := False;
    Exit;
   end;
  end;

  if Anchor = 1 then
   if (Integer (BufferPos) - Integer (BufferStart) > 2) and (BufferPos[-1] <> #10) then
   begin // was #13
    Inc (BufferPos);
    Continue;
   end;

  if Re_Match (BufferStart, BufferPos, BufferEnd, Registers) then
  begin
   Result := True;
   Exit;
  end;

  Inc (BufferPos)
 end;
 Result := False;
end;

procedure TRegExpr.SetPattern (const pat: AnsiString);
begin
 if (pat <> FPattern) or FStyleChange then
 begin
  FNoChange := False;
  if FStyleChange then
   Re_Compile_Initialize;
  FPattern := pat;
  Re_Compile_Pattern;
  FStyleChange := False;
 end;
end;

function TRegExpr.Re_Match (const ABufferStart, ABufferPos, ABufferEnd: PChar; const Old_Regs: PRegExpRegisters): Boolean;
label
 Continue_Matching, fail, done_matching, Error;
var
 Code, Translate, Text, TextStart, TextEnd: PChar;
 a, b, Reg, Match_End         : Integer;
 ch                           : Char;
 RegStart, RegEnd             : PChar;
 RegSize                      : Integer;
 State                        : TMatch_State;
 Item                         : Pitem_t;
 
 FailureDest, PInst           : PChar;
 Item_T                       : Pitem_t;
 Item_T2                      : Pitem_t;
 Index                        : Integer;
 Current                      : PItem_Page_T;
begin
 // Assert ((Pos > 0) and (Length (FStr) >= 0), 'Nothing to do');
 // Assert ((Pos <= Length (FStr)), 'Position not valid');
 
 Text := ABufferPos;
 TextStart := ABufferStart;
 TextEnd := ABufferEnd;
 
 Code := @RegExp_T.Buffer[1];
 if RegExp_T.Translate <> '' then
 begin
  Translate := @RegExp_T.Translate[1];
  Dec (Translate);
 end
 else
  Translate := nil;
 
 New_State (State, RegExp_T.Num_Registers);
 
 Continue_Matching:
 case RegExp_Compiled_Ops (Ord (Code[0])) of
  CEnd:
   begin
    Match_End := Text - ABufferPos;
    if Old_Regs <> nil then
    begin
     Old_Regs._Start[0] := ABufferPos - ABufferStart; // RJ
     Old_Regs._End[0] := Match_End;
     if not RegExp_T.Uses_Registers then
     begin
      for a := 1 to Re_NREGS - 1 do
      begin
       Old_Regs._Start[a] := -1;
       Old_Regs._End[a] := -1;
      end;
     end
     else
     begin
      a := 1;
      while a < RegExp_T.Num_Registers do
      begin
       if ((State._Start[a]) = nil) or (State._End[a] = nil) then
       begin
        Old_Regs._Start[a] := -1;
        Old_Regs._End[a] := -1;
        Inc (a);
        Continue;
       end;
       Old_Regs._Start[a] := State._Start[a] - TextStart;
       Old_Regs._End[a] := State._End[a] - State._Start[a];
       Inc (a);
      end;
      while a < Re_NREGS do
      begin
       Old_Regs._Start[a] := -1;
       Old_Regs._End[a] := -1;
       Inc (a);
      end;
     end;
    end;
    Free_state (State);
    // RJ Result := Match_End - Pos;
    Result := True; // RJ
    Exit;
   end;
  CBol:
   begin
    Inc (Code);
    if (Text = TextStart) or (Text[-1] = #10) then // was #13
     goto Continue_Matching;
    goto fail;
   end;
  CEol:
   begin
    Inc (Code);
    if (Text = TextEnd) or (Text[0] = #10) then // was #13
     goto Continue_Matching;
    goto fail;
   end;
  CSet:
   begin
    Inc (Code);
    //NEXTCHAR
    if (Text = TextEnd) then goto fail;
    ch := Text[0];
    Inc (Text);
    if (Translate <> nil) then ch := Translate[Ord (ch)];
    if (Ord (Code[Ord (ch) div 8]) and
     Integer (1 shl (Ord (ch) and 7)) > 0) then
    begin
     Inc (Code, 32);
     goto Continue_Matching;
    end;
    goto fail;
   end;
  CExact:
   begin
    Inc (Code);
    //NEXTCHAR
    if (Text = TextEnd) then goto fail;
    ch := Text[0];
    Inc (Text);
    if (Translate <> nil) then ch := Translate[Ord (ch)];
    Inc (Code);
    if ch <> Code[-1] then goto fail;
    goto Continue_Matching;
   end;
  CAnyChar:
   begin
    Inc (Code);
    //NEXTCHAR
    if (Text = TextEnd) then goto fail;
    ch := Text[0];
    Inc (Text);
    if (Translate <> nil) then ch := Translate[Ord (ch)];
    if ch = #10 then goto fail; // was #13
    goto Continue_Matching;
   end;
  CStart_Memory:
   begin
    Inc (Code);
    Reg := Ord (Code[0]);
    Inc (Code);
    //Set_Reg_Start(state, reg, text, 'Cstart_memory');
    if State.Changed[Reg] < State.Level then
    begin
     //Stack_Next(state.stack, item_t, msg);
     if State.Stack.Index = STACK_PAGE_SIZE then
     begin
      if State.Stack.Current.Next = nil then
      begin
       New (State.Stack.Current.Next);
       if State.Stack.Current.Next = nil then
        //Raise Exception.Create(msg);
        goto Error;
       State.Stack.Current.Next.Prev := State.Stack.Current;
       State.Stack.Current.Next.Next := nil;
      end;
      State.Stack.Current := State.Stack.Current.Next;
      State.Stack.Index := 0;
     end;
     Item_T := @State.Stack.Current.Items[State.Stack.Index];
     Inc (State.Stack.Index);
     //Stack_Next
     Item_T.Reg_Num := Reg;
     Item_T.Reg_Start := State._Start[Reg];
     Item_T.Reg_End := State._End[Reg];
     Item_T.Reg_Level := State.Changed[Reg];
     State.Changed[Reg] := State.Level;
     Inc (State.Count);
    end;
    State._Start[Reg] := Text;
    //Set_Reg_Start
    goto Continue_Matching;
   end;
  CEnd_Memory:
   begin
    Inc (Code);
    Reg := Ord (Code[0]);
    Inc (Code);
    //Set_Reg_End(state, reg, text, 'Cstart_memory');
    if State.Changed[Reg] < State.Level then
    begin
     //Stack_Next(state.stack, item_t, msg);
     if State.Stack.Index = STACK_PAGE_SIZE then
     begin
      if State.Stack.Current.Next = nil then
      begin
       New (State.Stack.Current.Next);
       if State.Stack.Current.Next = nil then
        //Raise Exception.Create(msg);
        goto Error;
       State.Stack.Current.Next.Prev := State.Stack.Current;
       State.Stack.Current.Next.Next := nil;
      end;
      State.Stack.Current := State.Stack.Current.Next;
      State.Stack.Index := 0;
     end;
     Item_T := @State.Stack.Current.Items[State.Stack.Index];
     Inc (State.Stack.Index);
     //Stack_Next
     Item_T.Reg_Num := Reg;
     Item_T.Reg_Start := State._Start[Reg];
     Item_T.Reg_End := State._End[Reg];
     Item_T.Reg_Level := State.Changed[Reg];
     State.Changed[Reg] := State.Level;
     Inc (State.Count);
    end;
    State._End[Reg] := Text;
    //Set_Reg_End
    goto Continue_Matching;
   end;
  Cmatch_memory:
   begin
    Inc (Code);
    Reg := Ord (Code[0]);
    Inc (Code);
    RegStart := State._Start[Reg];
    RegEnd := State._End[Reg];
    if (RegStart = nil) or (RegEnd = nil) then goto fail; // or should we just match nothing?
    RegSize := RegEnd - RegStart;

    if (RegSize > (TextEnd - Text)) then goto fail;
    if Translate <> nil then
    begin
     while RegStart < RegEnd do
     begin
      if Translate[Ord (RegStart[0])] <> Translate[Ord (Text[0])] then
       goto fail;
      Inc (RegStart);
      Inc (Text);
     end;
    end
    else
     while RegStart < RegEnd do
     begin
      if RegStart[0] <> Text[0] then
       goto fail;
      Inc (RegStart);
      Inc (Text);
     end;
    goto Continue_Matching;
   end;
  CUpdate_FailuRe_Jump,
   CStar_Jump,
   Cjump:
   begin
    Inc (Code);
    if RegExp_Compiled_Ops (Ord (Code[-1])) = CUpdate_FailuRe_Jump then
    begin //Update_Failure(state, text, 'Cupdate_failure_jump');
     //Stack_Back(state.stack, item_t, state.count + 1, msg);
     Current := State.Stack.Current;
     Index := State.Stack.Index - (State.Count + 1);
     while Index < 0 do
     begin
      if Current.Prev = nil then
       //Raise Exception.Create(msg);
       goto Error;
      Current := Current.Prev;
      Index := Index + STACK_PAGE_SIZE;
     end;
     Item_T := @Current.Items[Index];
     //Stack_Back
     if Item_T.fail_phantom = 0 then
     begin
      //Stack_Next(state.stack, item_t2, msg);
      if State.Stack.Index = STACK_PAGE_SIZE then
      begin
       if State.Stack.Current.Next = nil then
       begin
        New (State.Stack.Current.Next);
        if State.Stack.Current.Next = nil then
         //Raise Exception.Create(msg);
         goto Error;
        State.Stack.Current.Next.Prev := State.Stack.Current;
        State.Stack.Current.Next.Next := nil;
       end;
       State.Stack.Current := State.Stack.Current.Next;
       State.Stack.Index := 0;
      end;
      Item_T2 := @State.Stack.Current.Items[State.Stack.Index];
      Inc (State.Stack.Index);
      //Stack_Next
      Item_T2.fail_code := Item_T.fail_code;
      Item_T2.fail_text := Text;
      Item_T2.fail_count := State.Count;
      Item_T2.fail_level := State.Level;
      Item_T2.fail_phantom := 1;
      State.Count := 0;
      Inc (State.Level);
      Inc (State.Point);
     end
     else
     begin
      //Stack_Discard(state.stack, state.count, msg); // on error
      State.Stack.Index := State.Stack.Index - State.Count;
      while State.Stack.Index < 0 do
      begin
       if State.Stack.Current.Prev = nil then
        //Raise Exception.Create(msg);
        goto Error;
       State.Stack.Current := State.Stack.Current.Prev;
       State.Stack.Index := State.Stack.Index + STACK_PAGE_SIZE;
      end;
      //-Stack_Discard
      //Stack_Top(state.stack, item_t, msg);
      if State.Stack.Index = 0 then
      begin
       if State.Stack.Current.Prev = nil then
        //Raise Exception.Create(msg);
        goto Error;
       Item_T := @State.Stack.Current.Prev.Items[STACK_PAGE_SIZE - 1];
      end
      else
       Item_T := @State.Stack.Current.Items[State.Stack.Index - 1];
      //Stack_Top
      Item_T.fail_text := Text;
      State.Count := 0;
      Inc (State.Level);
     end;
    end;
    //Update_Failure
    a := short (Ord (Code[0]) + Ord (Code[1]) * 256);
    Inc (Code, 2);
    Inc (Code, a);
    if (Code < PChar (RegExp_T.Buffer)) or
     (Integer (@RegExp_T.Buffer[1]) + Length (RegExp_T.Buffer) < Integer (Code)) then
    begin
     Free_state (State);
     // RJ Result := -2;
     Result := False; // RJ
     Exit;
    end;
    goto Continue_Matching;
   end;
  CDummy_FailuRe_Jump:
   begin
    Inc (Code);
    a := short (Ord (Code[0]) + Ord (Code[1]) * 256);
    Inc (Code, 2);
    Assert (Code[0] = Char (CFailuRe_Jump), 'No Cfailure_Jump');
    b := short (Ord (Code[1]) + Ord (Code[2]) * 256);
    FailureDest := Code + b + 3;
    if (FailureDest < PChar (RegExp_T.Buffer)) or
     (Integer (@RegExp_T.Buffer[1]) + Length (RegExp_T.Buffer) < Integer (FailureDest)) then
    begin
     Free_state (State);
     // RJ Result := -2;
     Result := False; // RJ
     Exit;
    end;
    //Push_Failure(state, failuredest, nil, 'Cdummy_failure_jump');
    //Stack_Next(state.stack, item_t, msg);
    if State.Stack.Index = STACK_PAGE_SIZE then
    begin
     if State.Stack.Current.Next = nil then
     begin
      New (State.Stack.Current.Next);
      if State.Stack.Current.Next = nil then
       //Raise Exception.Create(msg);
       goto Error;
      State.Stack.Current.Next.Prev := State.Stack.Current;
      State.Stack.Current.Next.Next := nil;
     end;
     State.Stack.Current := State.Stack.Current.Next;
     State.Stack.Index := 0;
    end;
    Item_T := @State.Stack.Current.Items[State.Stack.Index];
    Inc (State.Stack.Index);
    //Stack_Next
    Item_T.fail_code := FailureDest;
    Item_T.fail_text := nil;
    Item_T.fail_count := State.Count;
    Item_T.fail_level := State.Level;
    Item_T.fail_phantom := 0;
    State.Count := 0;
    Inc (State.Level);
    Inc (State.Point);
    //Push_Failure
    Inc (Code, a);
    if (Code < PChar (RegExp_T.Buffer)) or
     (Integer (@RegExp_T.Buffer[1]) + Length (RegExp_T.Buffer) < Integer (Code)) then
    begin
     Free_state (State);
     // RJ Result := -2;
     Result := False; // RJ
     Exit;
    end;
    goto Continue_Matching;
   end;
  CFailuRe_Jump:
   begin
    Inc (Code);
    a := short (Ord (Code[0]) + Ord (Code[1]) * 256);
    Inc (Code, 2);
    if ((Code + a) < PChar (RegExp_T.Buffer)) or
     (Integer (@RegExp_T.Buffer[1]) + Length (RegExp_T.Buffer) < Integer (Code + a)) then
    begin
     Free_state (State);
     // RJ Result := -2;
     Result := False; // RJ
     Exit;
    end;
    //Push_Failure(state, code + a, text, 'Cdummy_failure_jump');
    //Stack_Next(state.stack, item_t, msg);
    if State.Stack.Index = STACK_PAGE_SIZE then
    begin
     if State.Stack.Current.Next = nil then
     begin
      New (State.Stack.Current.Next);
      if State.Stack.Current.Next = nil then
       //Raise Exception.Create(msg);
       goto Error;
      State.Stack.Current.Next.Prev := State.Stack.Current;
      State.Stack.Current.Next.Next := nil;
     end;
     State.Stack.Current := State.Stack.Current.Next;
     State.Stack.Index := 0;
    end;
    Item_T := @State.Stack.Current.Items[State.Stack.Index];
    Inc (State.Stack.Index);
    //Stack_Next
    Item_T.fail_code := Code + a;
    Item_T.fail_text := Text;
    Item_T.fail_count := State.Count;
    Item_T.fail_level := State.Level;
    Item_T.fail_phantom := 0;
    State.Count := 0;
    Inc (State.Level);
    Inc (State.Point);
    //Push_Failure
    goto Continue_Matching;
   end;
  Crepeat1:
   begin
    Inc (Code);
    a := short (Ord (Code[0]) + Ord (Code[1]) * 256);
    Inc (Code, 2);
    PInst := Code + a;
    if (PInst < PChar (RegExp_T.Buffer)) or
     (Integer (@RegExp_T.Buffer[1]) + Length (RegExp_T.Buffer) < Integer (PInst)) then
    begin
     Free_state (State);
     // RJ Result := -2;
     Result := False; // RJ
     Exit;
    end;
    // pinst is sole instruction in loop, and it matches a
    //* single character.  Since Crepeat1 was originally a
    //* Cupdate_failure_jump, we also know that backtracking
    //* is useless: so long as the single-character
    //* expression matches, it must be used.  Also, in the
    //* case of +, we've already matched one character, so +
    //* can't fail: nothing here can cause a failure.
    case RegExp_Compiled_Ops (Ord (PInst[0])) of
     CSet:
      begin
       Inc (PInst);
       if (Translate <> nil) then
       begin
        while (Text < TextEnd) do
        begin
         ch := Translate[Ord (Text[0])];
         if (Ord (PInst[Ord (ch) div 8]) and
          Integer (1 shl (Ord (ch) and 7)) > 0) then
          Inc (Text)
         else
          Break;
        end;
       end
       else
        while (Text < TextEnd) do
        begin
         ch := Text[0];
         if (Ord (PInst[Ord (ch) div 8]) and
          Integer (1 shl (Ord (ch) and 7)) > 0) then
          Inc (Text)
         else
          Break;
        end;
      end;
     CExact:
      begin
       Inc (PInst);
       ch := PInst[0];
       if (Translate <> nil) then
       begin
        while (Text < TextEnd) and
         (Translate[Ord (Text[0])] = ch) do
         Inc (Text);
       end
       else
        while (Text < TextEnd) and
         (Text[0] = ch) do
         Inc (Text);
      end;
     CAnyChar:
      begin
       while (Text < TextEnd) and (Text[0] <> #10) do
        Inc (Text); // was #13
       //break;
      end;
     CSyntaxSpec:
      begin
       Inc (PInst);
       a := Ord (PInst[0]);
       if (Translate <> nil) then
       begin
        while (Text < TextEnd) and
         ((Ord (Re_Syntax_Table[Ord (Translate[Ord (Text[0])])]) and a) > 0) do
         Inc (Text);
       end
       else
       begin
        while (Text < TextEnd) and
         ((Ord (Re_Syntax_Table[Ord (Text[0])]) and a) > 0) do
         Inc (Text);
       end;
      end;
     Cnotsyntaxspec:
      begin
       Inc (PInst);
       a := Ord (PInst[0]);
       if (Translate <> nil) then
       begin
        while (Text < TextEnd) and
         not ((Ord (Re_Syntax_Table[Ord (Translate[Ord (Text[0])])]) and a) > 0) do
         Inc (Text);
       end
       else
       begin
        while (Text < TextEnd) and
         not ((Ord (Re_Syntax_Table[Ord (Text[0])]) and a) > 0) do
         Inc (Text);
       end;
      end;
    else
     begin
      Free_state (State);
      raise ERegularExpression.Create (SreUnknowRE);
     end;
    end;
    // due to the funky way + and * are compiled, the top
    //* failure- stack entry at this point is actually a
    //* success entry -- update it & pop it
    //Update_Failure(state, text, '');
    begin
     //Stack_Back(state.stack, item_t, state.count + 1, msg);
     Current := State.Stack.Current;
     Index := State.Stack.Index - (State.Count + 1);
     while Index < 0 do
     begin
      if Current.Prev = nil then
       //Raise Exception.Create(msg);
       goto Error;
      Current := Current.Prev;
      Index := Index + STACK_PAGE_SIZE;
     end;
     Item_T := @Current.Items[Index];
     //Stack_Back
     if Item_T.fail_phantom = 0 then
     begin
      //Stack_Next(state.stack, item_t2, msg);
      if State.Stack.Index = STACK_PAGE_SIZE then
      begin
       if State.Stack.Current.Next = nil then
       begin
        New (State.Stack.Current.Next);
        if State.Stack.Current.Next = nil then
         //Raise Exception.Create(msg);
         goto Error;
        State.Stack.Current.Next.Prev := State.Stack.Current;
        State.Stack.Current.Next.Next := nil;
       end;
       State.Stack.Current := State.Stack.Current.Next;
       State.Stack.Index := 0;
      end;
      Item_T2 := @State.Stack.Current.Items[State.Stack.Index];
      Inc (State.Stack.Index);
      //Stack_Next
      Item_T2.fail_code := Item_T.fail_code;
      Item_T2.fail_text := Text;
      Item_T2.fail_count := State.Count;
      Item_T2.fail_level := State.Level;
      Item_T2.fail_phantom := 1;
      State.Count := 0;
      Inc (State.Level);
      Inc (State.Point);
     end
     else
     begin
      //Stack_Discard(state.stack, state.count, msg); // on error
      State.Stack.Index := State.Stack.Index - State.Count;
      while State.Stack.Index < 0 do
      begin
       if State.Stack.Current.Prev = nil then
        //Raise Exception.Create(msg);
        goto Error;
       State.Stack.Current := State.Stack.Current.Prev;
       State.Stack.Index := State.Stack.Index + STACK_PAGE_SIZE;
      end;
      //-Stack_Discard
      //Stack_Top(state.stack, item_t, msg);
      if State.Stack.Index = 0 then
      begin
       if State.Stack.Current.Prev = nil then
        //Raise Exception.Create(msg);
        goto Error;
       Item_T := @State.Stack.Current.Prev.Items[STACK_PAGE_SIZE - 1];
      end
      else
       Item_T := @State.Stack.Current.Items[State.Stack.Index - 1];
      //Stack_Top
      Item_T.fail_text := Text;
      State.Count := 0;
      Inc (State.Level);
     end;
    end;
    //Update_Failure
    goto fail;
   end;
  CBegBuf:
   begin
    Inc (Code);
    if Text = TextStart then goto Continue_Matching;
    goto fail;
   end;
  CEndbuf:
   begin
    Inc (Code);
    if Text = TextEnd then goto Continue_Matching;
    goto fail;
   end;
  Cwordbeg:
   begin
    Inc (Code);
    if Text = TextEnd then goto fail;
    if (not (Ord (Re_Syntax_Table[Ord (Text[0])])) and Sword) > 0 then goto fail;
    if Text = TextStart then goto Continue_Matching;
    if (not (Ord (Re_Syntax_Table[Ord (Text[-1])])) and Sword) > 0 then goto Continue_Matching;
    goto fail;
   end;
  Cwordend:
   begin
    Inc (Code);
    if Text = TextStart then goto fail;
    if (not (Ord (Re_Syntax_Table[Ord (Text[-1])])) and Sword) > 0 then goto fail;
    if Text = TextEnd then goto Continue_Matching;
    if (not (Ord (Re_Syntax_Table[Ord (Text[0])])) and Sword) > 0 then goto Continue_Matching;
    goto fail;
   end;
  Cwordbound:
   begin
    Inc (Code);
    { Note: as in gnu regexp, this also matches at the
    * beginning and end of buffer.}
    if (Text = TextStart) or (Text = TextEnd) then
     goto Continue_Matching;
    if ((Ord (Re_Syntax_Table[Ord (Text[-1])]) and Sword) xor
     (Ord (Re_Syntax_Table[Ord (Text[0])]) and Sword)) > 0 then
     goto Continue_Matching;
    goto fail;
   end;
  Cnotwordbound:
   begin
    Inc (Code);
    { Note: as in gnu regexp, this never matches at the
    * beginning and end of buffer.}
    if (Text = TextStart) or (Text = TextEnd) then
     goto fail;
    if ((Ord (Re_Syntax_Table[Ord (Text[-1])]) and Sword) xor
     (Ord (Re_Syntax_Table[Ord (Text[0])]) and Sword)) > 0 then
     goto fail;
    goto Continue_Matching;
   end;
  CSyntaxSpec:
   begin
    Inc (Code);
    if (Text = TextEnd) then goto fail;
    ch := Text[0];
    Inc (Text);
    if (Translate <> nil) then ch := Translate[Ord (ch)];
    Inc (Code);
    if ((not (Ord (Re_Syntax_Table[Ord (ch)])) and Ord (Code[-1])) > 0) then goto fail;
    goto Continue_Matching;
   end;
  Cnotsyntaxspec:
   begin
    Inc (Code);
    if (Text = TextEnd) then goto fail;
    ch := Text[0];
    Inc (Text);
    if (Translate <> nil) then ch := Translate[Ord (ch)];
    Inc (Code);
    if ((Ord (Re_Syntax_Table[Ord (ch)]) and Ord (Code[-1])) > 0) then goto fail;
    goto Continue_Matching;
   end;
 end;
 fail: // POP_FAILURE(state, code, text, goto done_matching, goto error)
 repeat
  while State.Count > 0 do
  begin
   //Stack_Prev(state.stack, item, 'POP FAILURE - error');
   if State.Stack.Index = 0 then
   begin
    if State.Stack.Current.Prev = nil then
     //Raise Exception.Create(msg);
     goto Error;
    State.Stack.Current := State.Stack.Current.Prev;
    State.Stack.Index := STACK_PAGE_SIZE - 1;
   end
   else
    Dec (State.Stack.Index);
   Item := @State.Stack.Current.Items[State.Stack.Index];
   //Stack_Prev
   State._Start[Item.Reg_Num] := Item.Reg_Start;
   State._End[Item.Reg_Num] := Item.Reg_End;
   State.Changed[Item.Reg_Num] := Item.Reg_Level;
   Dec (State.Count);
  end;
  //Stack_Prev(state.stack, item, 'POP FAILURE - Empty');
  if State.Stack.Index = 0 then
  begin
   if State.Stack.Current.Prev = nil then
    //Raise Exception.Create(msg);
    goto done_matching;
   ;
   State.Stack.Current := State.Stack.Current.Prev;
   State.Stack.Index := STACK_PAGE_SIZE - 1;
  end
  else
   Dec (State.Stack.Index);
  Item := @State.Stack.Current.Items[State.Stack.Index];
  //Stack_Prev
  Code := Item.fail_code;
  Text := Item.fail_text;
  State.Count := Item.fail_count;
  State.Level := Item.fail_level;
  Dec (State.Point);
 until (Item.fail_text <> nil);
 goto Continue_Matching;
 
 done_matching:
 // RJ Result := -1;
 Result := False; // RJ
 
 Free_state (State);
 Exit;
 
 Error:
 // RJ Result := -2;
 Result := False; // RJ
 Free_state (State);
 Exit;
 
end;

end.

