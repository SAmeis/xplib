unit rjHtmlTags;
{
File:           rjHtmlTags.pas
Version:        0.1 / Initial Public Release
Last Modified:  1. April 2000
Decription:     Various types, constants, and helper functions
                for the identification of HTML tags.
                Used by the rjHtmlParser unit.
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

{ Tag-Definitionen }
const
 tagUndefined                 = 0;
 tagA                         = 1;
 tagABBR                      = 2;
 tagAcronym                   = 3;
 tagAddress                   = 4;
 tagAPPLET                    = 5;
 tagAREA                      = 6;
 tagArticle                   = 7;
 tagB                         = 8;
 tagBase                      = 9;
 tagBASEFONT                  = 10;
 tagBDO                       = 11;
 tagBIG                       = 12;
 tagBlockquote                = 13;
 tagBODY                      = 14;
 tagBR                        = 15;
 tagBUTTON                    = 16;
 tagCaption                   = 17;
 tagCenter                    = 18;
 tagCite                      = 19;
 tagCODE                      = 20;
 tagCOL                       = 21;
 tagCOLGROUP                  = 22;
 tagDD                        = 23;
 tagDEL                       = 24;
 tagDFN                       = 25;
 tagDIR                       = 26;
 tagDiv                       = 27;
 tagDl                        = 28;
 tagDT                        = 29;
 tagEM                        = 30;
 tagFIELDSET                  = 31;
 tagFONT                      = 32;
 tagFORM                      = 33;
 tagFRAME                     = 34;
 tagFrameSet                  = 35;
 tagH1                        = 36;
 tagH2                        = 37;
 tagH3                        = 38;
 tagH4                        = 39;
 tagH5                        = 40;
 tagH6                        = 41;
 taghead                      = 42;
 tagHr                        = 43;
 tagHtml                      = 44;
 tagI                         = 45;
 tagIFRAME                    = 46;
 tagIMG                       = 47;
 tagINPUT                     = 48;
 tagIns                       = 49;
 tagISINDEX                   = 50;
 tagKBD                       = 51;
 tagLabel                     = 52;
 tagLegend                    = 53;
 tagLI                        = 54;
 tagLINK                      = 55;
 tagMAP                       = 56;
 tagMenu                      = 57;
 tagMeta                      = 58;
 tagNoFrames                  = 59;
 tagNoscript                  = 60;
 tagObject                    = 61;
 tagOL                        = 62;
 tagOptgroup                  = 63;
 tagOption                    = 64;
 tagP                         = 65;
 tagParam                     = 66;
 tagPre                       = 67;
 tagQ                         = 68;
 Tags                         = 69;
 tagSAMP                      = 70;
 tagSCRIPT                    = 71;
 tagSelect                    = 72;
 tagSmall                     = 73;
 tagSPAN                      = 74;
 tagStrike                    = 75;
 tagSTRONG                    = 76;
 tagSTYLE                     = 77;
 tagSUB                       = 78;
 tagSUP                       = 79;
 TagTABLE                     = 80;
 tagTBODY                     = 81;
 tagTD                        = 82;
 tagTEXTAREA                  = 83;
 tagTFoot                     = 84;
 tagTH                        = 85;
 tagTHead                     = 86;
 tagTITLE                     = 87;
 tagTr                        = 88;
 tagTT                        = 89;
 tagU                         = 90;
 tagUL                        = 91;
 tagVar                       = 92;
 
 tagMax                       = tagVar;
 
 { Tag HashSize }
 
 TAG_HASHSIZE                 = 141;
 
 { Tag-Names -- Keep in order with those above! }
 
 TagNames                     : array[tagUndefined..tagVar] of AnsiString = (
  '',
  'A',
  'ABBR',
  'ACRONYM',
  'ADDRESS',
  'APPLET',
  'AREA',
  'ARTICLE',
  'B',
  'BASE',
  'BASEFONT',
  'BDO',
  'BIG',
  'BLOCKQUOTE',
  'BODY',
  'BR',
  'BUTTON',
  'CAPTION',
  'CENTER',
  'CITE',
  'CODE',
  'COL',
  'COLGROUP',
  'DD',
  'DEL',
  'DFN',
  'DIR',
  'DIV',
  'DL',
  'DT',
  'EM',
  'FIELDSET',
  'FONT',
  'FORM',
  'FRAME',
  'FRAMESET',
  'H1',
  'H2',
  'H3',
  'H4',
  'H5',
  'H6',
  'HEAD',
  'HR',
  'HTML',
  'I',
  'IFRAME',
  'IMG',
  'INPUT',
  'INS',
  'ISINDEX',
  'KBD',
  'LABEL',
  'LEGEND',
  'LI',
  'LINK',
  'MAP',
  'MENU',
  'META',
  'NOFRAMES',
  'NOSCRIPT',
  'OBJECT',
  'OL',
  'OPTGROUP',
  'OPTION',
  'P',
  'PARAM',
  'PRE',
  'Q',
  'S',
  'SAMP',
  'SCRIPT',
  'SELECT',
  'SMALL',
  'SPAN',
  'STRIKE',
  'STRONG',
  'STYLE',
  'SUB',
  'SUP',
  'TABLE',
  'TBODY',
  'TD',
  'TEXTAREA',
  'TFOOT',
  'TH',
  'THEAD',
  'TITLE',
  'TR',
  'TT',
  'U',
  'UL',
  'VAR'
  );
 
type
 
 TTagFunc = function (const p: PChar; const l: Integer): Integer;
 
 { Tag Functions }
 
function TagFuncUndefined (const p: PChar; const l: Integer): Integer;
function TagFunc0 (const p: PChar; const l: Integer): Integer;
function TagFunc3 (const p: PChar; const l: Integer): Integer;
function TagFunc4 (const p: PChar; const l: Integer): Integer;
function TagFunc6 (const p: PChar; const l: Integer): Integer;
function TagFunc9 (const p: PChar; const l: Integer): Integer;
function TagFunc10 (const p: PChar; const l: Integer): Integer;
function TagFunc12 (const p: PChar; const l: Integer): Integer;
function TagFunc13 (const p: PChar; const l: Integer): Integer;
function TagFunc14 (const p: PChar; const l: Integer): Integer;
function TagFunc19 (const p: PChar; const l: Integer): Integer;
function TagFunc20 (const p: PChar; const l: Integer): Integer;
function TagFunc21 (const p: PChar; const l: Integer): Integer;
function TagFunc26 (const p: PChar; const l: Integer): Integer;
function TagFunc27 (const p: PChar; const l: Integer): Integer;
function TagFunc29 (const p: PChar; const l: Integer): Integer;
function TagFunc30 (const p: PChar; const l: Integer): Integer;
function TagFunc31 (const p: PChar; const l: Integer): Integer;
function TagFunc37 (const p: PChar; const l: Integer): Integer;
function TagFunc40 (const p: PChar; const l: Integer): Integer;
function TagFunc45 (const p: PChar; const l: Integer): Integer;
function TagFunc48 (const p: PChar; const l: Integer): Integer;
function TagFunc49 (const p: PChar; const l: Integer): Integer;
function TagFunc50 (const p: PChar; const l: Integer): Integer;
function TagFunc52 (const p: PChar; const l: Integer): Integer;
function TagFunc53 (const p: PChar; const l: Integer): Integer;
function TagFunc56 (const p: PChar; const l: Integer): Integer;
function TagFunc57 (const p: PChar; const l: Integer): Integer;
function TagFunc58 (const p: PChar; const l: Integer): Integer;
function TagFunc62 (const p: PChar; const l: Integer): Integer;
function TagFunc64 (const p: PChar; const l: Integer): Integer;
function TagFunc65 (const p: PChar; const l: Integer): Integer;
function TagFunc66 (const p: PChar; const l: Integer): Integer;
function TagFunc67 (const p: PChar; const l: Integer): Integer;
function TagFunc68 (const p: PChar; const l: Integer): Integer;
function TagFunc69 (const p: PChar; const l: Integer): Integer;
function TagFunc72 (const p: PChar; const l: Integer): Integer;
function TagFunc73 (const p: PChar; const l: Integer): Integer;
function TagFunc74 (const p: PChar; const l: Integer): Integer;
function TagFunc76 (const p: PChar; const l: Integer): Integer;
function TagFunc77 (const p: PChar; const l: Integer): Integer;
function TagFunc79 (const p: PChar; const l: Integer): Integer;
function TagFunc80 (const p: PChar; const l: Integer): Integer;
function TagFunc81 (const p: PChar; const l: Integer): Integer;
function TagFunc83 (const p: PChar; const l: Integer): Integer;
function TagFunc85 (const p: PChar; const l: Integer): Integer;
function TagFunc88 (const p: PChar; const l: Integer): Integer;
function TagFunc91 (const p: PChar; const l: Integer): Integer;
function TagFunc92 (const p: PChar; const l: Integer): Integer;
function TagFunc93 (const p: PChar; const l: Integer): Integer;
function TagFunc94 (const p: PChar; const l: Integer): Integer;
function TagFunc95 (const p: PChar; const l: Integer): Integer;
function TagFunc97 (const p: PChar; const l: Integer): Integer;
function TagFunc98 (const p: PChar; const l: Integer): Integer;
function TagFunc99 (const p: PChar; const l: Integer): Integer;
function TagFunc100 (const p: PChar; const l: Integer): Integer;
function TagFunc101 (const p: PChar; const l: Integer): Integer;
function TagFunc102 (const p: PChar; const l: Integer): Integer;
function TagFunc103 (const p: PChar; const l: Integer): Integer;
function TagFunc104 (const p: PChar; const l: Integer): Integer;
function TagFunc106 (const p: PChar; const l: Integer): Integer;
function TagFunc108 (const p: PChar; const l: Integer): Integer;
function TagFunc114 (const p: PChar; const l: Integer): Integer;
function TagFunc117 (const p: PChar; const l: Integer): Integer;
function TagFunc120 (const p: PChar; const l: Integer): Integer;
function TagFunc124 (const p: PChar; const l: Integer): Integer;
function TagFunc125 (const p: PChar; const l: Integer): Integer;
function TagFunc127 (const p: PChar; const l: Integer): Integer;
function TagFunc129 (const p: PChar; const l: Integer): Integer;
function TagFunc130 (const p: PChar; const l: Integer): Integer;
function TagFunc132 (const p: PChar; const l: Integer): Integer;
function TagFunc133 (const p: PChar; const l: Integer): Integer;
function TagFunc135 (const p: PChar; const l: Integer): Integer;
function TagFunc137 (const p: PChar; const l: Integer): Integer;
function TagFunc139 (const p: PChar; const l: Integer): Integer;
function TagFunc140 (const p: PChar; const l: Integer): Integer;

const
 
 { Tag Functions Table }
 
 TagFuncTable                 : array[0..TAG_HASHSIZE - 1] of TTagFunc = (
  TagFunc0, TagFuncUndefined, TagFuncUndefined,
  TagFunc3,
  TagFunc4, TagFuncUndefined,
  TagFunc6, TagFuncUndefined, TagFuncUndefined,
  TagFunc9,
  TagFunc10, TagFuncUndefined,
  TagFunc12,
  TagFunc13,
  TagFunc14, TagFuncUndefined, TagFuncUndefined, TagFuncUndefined, TagFuncUndefined,
  TagFunc19,
  TagFunc20,
  TagFunc21, TagFuncUndefined, TagFuncUndefined, TagFuncUndefined, TagFuncUndefined,
  TagFunc26,
  TagFunc27, TagFuncUndefined,
  TagFunc29,
  TagFunc30,
  TagFunc31, TagFuncUndefined, TagFuncUndefined, TagFuncUndefined, TagFuncUndefined, TagFuncUndefined,
  TagFunc37, TagFuncUndefined, TagFuncUndefined,
  TagFunc40, TagFuncUndefined, TagFuncUndefined, TagFuncUndefined, TagFuncUndefined,
  TagFunc45, TagFuncUndefined, TagFuncUndefined,
  TagFunc48,
  TagFunc49,
  TagFunc50, TagFuncUndefined,
  TagFunc52,
  TagFunc53, TagFuncUndefined, TagFuncUndefined,
  TagFunc56,
  TagFunc57,
  TagFunc58, TagFuncUndefined, TagFuncUndefined, TagFuncUndefined,
  TagFunc62, TagFuncUndefined,
  TagFunc64,
  TagFunc65,
  TagFunc66,
  TagFunc67,
  TagFunc68,
  TagFunc69, TagFuncUndefined, TagFuncUndefined,
  TagFunc72,
  TagFunc73,
  TagFunc74, TagFuncUndefined,
  TagFunc76,
  TagFunc77, TagFuncUndefined,
  TagFunc79,
  TagFunc80,
  TagFunc81, TagFuncUndefined,
  TagFunc83, TagFuncUndefined,
  TagFunc85, TagFuncUndefined, TagFuncUndefined,
  TagFunc88, TagFuncUndefined, TagFuncUndefined,
  TagFunc91,
  TagFunc92,
  TagFunc93,
  TagFunc94,
  TagFunc95, TagFuncUndefined,
  TagFunc97,
  TagFunc98,
  TagFunc99,
  TagFunc100,
  TagFunc101,
  TagFunc102,
  TagFunc103,
  TagFunc104, TagFuncUndefined,
  TagFunc106, TagFuncUndefined,
  TagFunc108, TagFuncUndefined, TagFuncUndefined, TagFuncUndefined, TagFuncUndefined, TagFuncUndefined,
  TagFunc114, TagFuncUndefined, TagFuncUndefined,
  TagFunc117, TagFuncUndefined, TagFuncUndefined,
  TagFunc120, TagFuncUndefined, TagFuncUndefined, TagFuncUndefined,
  TagFunc124,
  TagFunc125, TagFuncUndefined,
  TagFunc127, TagFuncUndefined,
  TagFunc129,
  TagFunc130, TagFuncUndefined,
  TagFunc132,
  TagFunc133, TagFuncUndefined,
  TagFunc135, TagFuncUndefined,
  TagFunc137, TagFuncUndefined,
  TagFunc139,
  TagFunc140
  );
 
implementation

uses rjHtmlParser;

{ Tag Functions }

function TagFuncUndefined (const p: PChar; const l: Integer): Integer;
begin
 Result := tagUndefined
end;

function TagFunc0 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'BASEFONT', l) then
  Result := tagBASEFONT
 else
  Result := tagUndefined;
end;

function TagFunc3 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'DIV', l) then
  Result := tagDiv
 else
  Result := tagUndefined;
end;

function TagFunc4 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'DT', l) then
  Result := tagDT
 else
  if CompareTA (p, 'SELECT', l) then
   Result := tagSelect
  else
   Result := tagUndefined;
end;

function TagFunc6 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'DEL', l) then
  Result := tagDEL
 else
  if CompareTA (p, 'INPUT', l) then
   Result := tagINPUT
  else
   Result := tagUndefined;
end;

function TagFunc9 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'COLGROUP', l) then
  Result := tagCOLGROUP
 else
  Result := tagUndefined;
end;

function TagFunc10 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'FRAMESET', l) then
  Result := tagFrameSet
 else
  Result := tagUndefined;
end;

function TagFunc12 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'MENU', l) then
  Result := tagMenu
 else
  if CompareTA (p, 'TFOOT', l) then
   Result := tagTFoot
  else
   Result := tagUndefined;
end;

function TagFunc13 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'COL', l) then
  Result := tagCOL
 else
  Result := tagUndefined;
end;

function TagFunc14 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'BASE', l) then
  Result := tagBase
 else
  Result := tagUndefined;
end;

function TagFunc19 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'IMG', l) then
  Result := tagIMG
 else
  Result := tagUndefined;
end;

function TagFunc20 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'FORM', l) then
  Result := tagFORM
 else
  Result := tagUndefined;
end;

function TagFunc21 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'NOFRAMES', l) then
  Result := tagNoFrames
 else
  Result := tagUndefined;
end;

function TagFunc26 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'LABEL', l) then
  Result := tagLabel
 else
  if CompareTA (p, 'PRE', l) then
   Result := tagPre
  else
   if CompareTA (p, 'KBD', l) then
    Result := tagKBD
   else
    Result := tagUndefined;
end;

function TagFunc27 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'OPTGROUP', l) then
  Result := tagOptgroup
 else
  Result := tagUndefined;
end;

function TagFunc29 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'EM', l) then
  Result := tagEM
 else
  Result := tagUndefined;
end;

function TagFunc30 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'META', l) then
  Result := tagMeta
 else
  Result := tagUndefined;
end;

function TagFunc31 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'STRIKE', l) then
  Result := tagStrike
 else
  if CompareTA (p, 'ADDRESS', l) then
   Result := tagAddress
  else
   Result := tagUndefined;
end;

function TagFunc37 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'BDO', l) then
  Result := tagBDO
 else
  Result := tagUndefined;
end;

function TagFunc40 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'FONT', l) then
  Result := tagFONT
 else
  Result := tagUndefined;
end;

function TagFunc45 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'PARAM', l) then
  Result := tagParam
 else
  Result := tagUndefined;
end;

function TagFunc48 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'ISINDEX', l) then
  Result := tagISINDEX
 else
  if CompareTA (p, 'BIG', l) then
   Result := tagBIG
  else
   Result := tagUndefined;
end;

function TagFunc49 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'TITLE', l) then
  Result := tagTITLE
 else
  Result := tagUndefined;
end;

function TagFunc50 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'BUTTON', l) then
  Result := tagBUTTON
 else
  Result := tagUndefined;
end;

function TagFunc52 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'BODY', l) then
  Result := tagBODY
 else
  Result := tagUndefined;
end;

function TagFunc53 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'DFN', l) then
  Result := tagDFN
 else
  Result := tagUndefined;
end;

function TagFunc56 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'NOSCRIPT', l) then
  Result := tagNoscript
 else
  if CompareTA (p, 'CENTER', l) then
   Result := tagCenter
  else
   if CompareTA (p, 'TABLE', l) then
    Result := TagTABLE
   else
    Result := tagUndefined;
end;

function TagFunc57 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'ARTICLE', l) then
  Result := tagArticle
 else
  if CompareTA (p, 'AREA', l) then
   Result := tagAREA
  else
   Result := tagUndefined;
end;

function TagFunc58 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'ACRONYM', l) then
  Result := tagAcronym
 else
  Result := tagUndefined;
end;

function TagFunc62 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'STRONG', l) then
  Result := tagSTRONG
 else
  Result := tagUndefined;
end;

function TagFunc64 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'SMALL', l) then
  Result := tagSmall
 else
  Result := tagUndefined;
end;

function TagFunc65 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'A', l) then
  Result := tagA
 else
  Result := tagUndefined;
end;

function TagFunc66 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'B', l) then
  Result := tagB
 else
  Result := tagUndefined;
end;

function TagFunc67 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'SCRIPT', l) then
  Result := tagSCRIPT
 else
  if CompareTA (p, 'SAMP', l) then
   Result := tagSAMP
  else
   Result := tagUndefined;
end;

function TagFunc68 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'STYLE', l) then
  Result := tagSTYLE
 else
  Result := tagUndefined;
end;

function TagFunc69 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'SUB', l) then
  Result := tagSUB
 else
  Result := tagUndefined;
end;

function TagFunc72 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'FIELDSET', l) then
  Result := tagFIELDSET
 else
  if CompareTA (p, 'THEAD', l) then
   Result := tagTHead
  else
   Result := tagUndefined;
end;

function TagFunc73 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'I', l) then
  Result := tagI
 else
  Result := tagUndefined;
end;

function TagFunc74 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'MAP', l) then
  Result := tagMAP
 else
  Result := tagUndefined;
end;

function TagFunc76 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'INS', l) then
  Result := tagIns
 else
  Result := tagUndefined;
end;

function TagFunc77 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'TD', l) then
  Result := tagTD
 else
  if CompareTA (p, 'IFRAME', l) then
   Result := tagIFRAME
  else
   Result := tagUndefined;
end;

function TagFunc79 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'OL', l) then
  Result := tagOL
 else
  Result := tagUndefined;
end;

function TagFunc80 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'P', l) then
  Result := tagP
 else
  Result := tagUndefined;
end;

function TagFunc81 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'TH', l) then
  Result := tagTH
 else
  if CompareTA (p, 'Q', l) then
   Result := tagQ
  else
   Result := tagUndefined;
end;

function TagFunc83 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'SUP', l) then
  Result := tagSUP
 else
  if CompareTA (p, 'S', l) then
   Result := Tags
  else
   Result := tagUndefined;
end;

function TagFunc85 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'U', l) then
  Result := tagU
 else
  Result := tagUndefined;
end;

function TagFunc88 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'CODE', l) then
  Result := tagCODE
 else
  Result := tagUndefined;
end;

function TagFunc91 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'TR', l) then
  Result := tagTr
 else
  Result := tagUndefined;
end;

function TagFunc92 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'BLOCKQUOTE', l) then
  Result := tagBlockquote
 else
  if CompareTA (p, 'BR', l) then
   Result := tagBR
  else
   if CompareTA (p, 'HTML', l) then
    Result := tagHtml
   else
    Result := tagUndefined;
end;

function TagFunc93 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'TT', l) then
  Result := tagTT
 else
  Result := tagUndefined;
end;

function TagFunc94 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'LINK', l) then
  Result := tagLINK
 else
  Result := tagUndefined;
end;

function TagFunc95 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'SPAN', l) then
  Result := tagSPAN
 else
  Result := tagUndefined;
end;

function TagFunc97 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'H1', l) then
  Result := tagH1
 else
  Result := tagUndefined;
end;

function TagFunc98 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'H2', l) then
  Result := tagH2
 else
  Result := tagUndefined;
end;

function TagFunc99 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'H3', l) then
  Result := tagH3
 else
  Result := tagUndefined;
end;

function TagFunc100 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'H4', l) then
  Result := tagH4
 else
  Result := tagUndefined;
end;

function TagFunc101 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'H5', l) then
  Result := tagH5
 else
  Result := tagUndefined;
end;

function TagFunc102 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'H6', l) then
  Result := tagH6
 else
  Result := tagUndefined;
end;

function TagFunc103 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'CITE', l) then
  Result := tagCite
 else
  Result := tagUndefined;
end;

function TagFunc104 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'ABBR', l) then
  Result := tagABBR
 else
  Result := tagUndefined;
end;

function TagFunc106 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'LEGEND', l) then
  Result := tagLegend
 else
  Result := tagUndefined;
end;

function TagFunc108 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'LI', l) then
  Result := tagLI
 else
  Result := tagUndefined;
end;

function TagFunc114 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'FRAME', l) then
  Result := tagFRAME
 else
  Result := tagUndefined;
end;

function TagFunc117 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'UL', l) then
  Result := tagUL
 else
  Result := tagUndefined;
end;

function TagFunc120 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'VAR', l) then
  Result := tagVar
 else
  Result := tagUndefined;
end;

function TagFunc124 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'CAPTION', l) then
  Result := tagCaption
 else
  Result := tagUndefined;
end;

function TagFunc125 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'APPLET', l) then
  Result := tagAPPLET
 else
  Result := tagUndefined;
end;

function TagFunc127 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'OBJECT', l) then
  Result := tagObject
 else
  Result := tagUndefined;
end;

function TagFunc129 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'DD', l) then
  Result := tagDD
 else
  Result := tagUndefined;
end;

function TagFunc130 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'HR', l) then
  Result := tagHr
 else
  Result := tagUndefined;
end;

function TagFunc132 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'HEAD', l) then
  Result := taghead
 else
  Result := tagUndefined;
end;

function TagFunc133 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'TBODY', l) then
  Result := tagTBODY
 else
  Result := tagUndefined;
end;

function TagFunc135 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'OPTION', l) then
  Result := tagOption
 else
  Result := tagUndefined;
end;

function TagFunc137 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'DL', l) then
  Result := tagDl
 else
  Result := tagUndefined;
end;

function TagFunc139 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'TEXTAREA', l) then
  Result := tagTEXTAREA
 else
  Result := tagUndefined;
end;

function TagFunc140 (const p: PChar; const l: Integer): Integer;
begin
 if CompareTA (p, 'DIR', l) then
  Result := tagDIR
 else
  Result := tagUndefined;
end;

end.

