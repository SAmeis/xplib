unit rjHtmlAttribs;
{
File:           rjHtmlAttribs.pas
Version:        0.1 / Initial Public Release
Last Modified:  1. April 2000
Decription:     Various types, constants, and helper functions
                for the identification of HTML attributes.
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

type
 
 { Attrib Definitions }
 
 TAttribs = (
  attribUndefined,
  attribAbbr,
  attribAccept,
  attribAcceptcharset,
  attribAccesskey,
  attribAction,
  attribAlign,
  attribAlink,
  attribAlt,
  attribArchive,
  attribAxis,
  attribBACKGROUND,
  attribBgcolor,
  attribBorder,
  attribCellpadding,
  attribCellspacing,
  attribChar,
  attribCharoff,
  attribCharset,
  attribChecked,
  attribCite,
  attribClass,
  attribClassid,
  attribClear,
  attribCODE,
  attribCodebase,
  attribCodetype,
  attribColor,
  attribCols,
  attribColspan,
  attribCompact,
  attribContent,
  attribCoords,
  attribData,
  attribDatetime,
  attribDeclare,
  attribDefer,
  attribDir,
  attribDisabled,
  attribEnctype,
  attribFace,
  attribFor,
  attribFrame,
  attribFrameborder,
  attribHeaders,
  attribHeight,
  attribHref,
  attribHreflang,
  attribHspace,
  attribHttpequiv,
  attribId,
  attribIsmap,
  attribLabel,
  attribLang,
  attribLanguage,
  attribLINK,
  attribLongdesc,
  attribLowsrc,
  attribMarginheight,
  attribMarginwidth,
  attribMaxlength,
  attribMedia,
  attribMethod,
  attribMultiple,
  attribName,
  attribNohref,
  attribNoresize,
  attribNoshade,
  attribNowrap,
  attribObject,
  attribOnblur,
  attribOnchange,
  attribOnclick,
  attribOndblclick,
  attribOnfocus,
  attribOnkeydown,
  attribOnkeypress,
  attribOnkeyup,
  attribOnload,
  attribOnmousedown,
  attribOnmousemove,
  attribOnmouseout,
  attribOnmouseover,
  attribOnmouseup,
  attribOnreset,
  attribOnselect,
  attribOnsubmit,
  attribOnunload,
  attribProfile,
  attribPrompt,
  attribReadonly,
  attribRel,
  attribRev,
  attribRows,
  attribRowspan,
  attribRules,
  attribScheme,
  attribScope,
  attribScrolling,
  attribSelected,
  attribShape,
  attribSize,
  attribSpan,
  attribSRC,
  attribStandby,
  attribStart,
  attribStyle,
  attribSummary,
  attribTabindex,
  attribTarget,
  attribText,
  attribTitle,
  attribType,
  attribUsemap,
  attribValign,
  attribValue,
  attribValuetype,
  attribVersion,
  attribVlink,
  attribVspace,
  attribWidth
  );
 
 TAttribFunc = function (const p: PChar; const l: Integer): TAttribs;
 
 { Attrib Strings }
 
const
 ATTRIB_HASHSIZE              = 137;
 
 AttribNames                  : array[attribUndefined..attribWidth] of AnsiString = (
  '',
  'abbr',
  'accept',
  'accept-charset',
  'accesskey',
  'action',
  'align',
  'alink',
  'alt',
  'archive',
  'axis',
  'background',
  'bgcolor',
  'border',
  'cellpadding',
  'cellspacing',
  'char',
  'charoff',
  'charset',
  'checked',
  'cite',
  'class',
  'classid',
  'clear',
  'code',
  'codebase',
  'codetype',
  'color',
  'cols',
  'colspan',
  'compact',
  'content',
  'coords',
  'data',
  'datetime',
  'declare',
  'defer',
  'dir',
  'disabled',
  'enctype',
  'face',
  'for',
  'frame',
  'frameborder',
  'headers',
  'height',
  'href',
  'hreflang',
  'hspace',
  'http-equiv',
  'id',
  'ismap',
  'label',
  'lang',
  'language',
  'link',
  'longdesc',
  'lowsrc',
  'marginheight',
  'marginwidth',
  'maxlength',
  'media',
  'method',
  'multiple',
  'name',
  'nohref',
  'noresize',
  'noshade',
  'nowrap',
  'object',
  'onblur',
  'onchange',
  'onclick',
  'ondblclick',
  'onfocus',
  'onkeydown',
  'onkeypress',
  'onkeyup',
  'onload',
  'onmousedown',
  'onmousemove',
  'onmouseout',
  'onmouseover',
  'onmouseup',
  'onreset',
  'onselect',
  'onsubmit',
  'onunload',
  'profile',
  'prompt',
  'readonly',
  'rel',
  'rev',
  'rows',
  'rowspan',
  'rules',
  'scheme',
  'scope',
  'scrolling',
  'selected',
  'shape',
  'size',
  'span',
  'src',
  'standby',
  'start',
  'style',
  'summary',
  'tabindex',
  'target',
  'text',
  'title',
  'type',
  'usemap',
  'valign',
  'value',
  'valuetype',
  'version',
  'vlink',
  'vspace',
  'width'
  );
 
 { Function Definitions }
 
function AttribFuncUndefined (const p: PChar; const l: Integer): TAttribs;
function AttribFunc0 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc2 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc3 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc5 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc7 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc10 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc13 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc14 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc15 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc16 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc17 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc18 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc21 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc22 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc23 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc24 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc26 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc28 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc29 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc30 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc31 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc32 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc34 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc35 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc37 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc38 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc39 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc40 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc42 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc43 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc44 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc46 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc47 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc49 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc50 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc53 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc55 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc57 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc58 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc59 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc60 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc61 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc62 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc63 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc64 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc65 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc66 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc70 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc74 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc75 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc78 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc80 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc82 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc84 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc85 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc88 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc89 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc91 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc93 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc96 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc97 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc101 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc102 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc103 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc104 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc105 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc106 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc108 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc111 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc113 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc114 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc116 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc117 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc120 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc121 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc122 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc123 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc124 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc125 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc126 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc128 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc129 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc131 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc132 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc135 (const p: PChar; const l: Integer): TAttribs;
function AttribFunc136 (const p: PChar; const l: Integer): TAttribs;

{ Attrib Function Table }

const
 AttribFuncTable              : array[0..ATTRIB_HASHSIZE - 1] of TAttribFunc = (
  AttribFunc0, AttribFuncUndefined,
  AttribFunc2,
  AttribFunc3, AttribFuncUndefined,
  AttribFunc5, AttribFuncUndefined,
  AttribFunc7, AttribFuncUndefined, AttribFuncUndefined,
  AttribFunc10, AttribFuncUndefined, AttribFuncUndefined,
  AttribFunc13,
  AttribFunc14,
  AttribFunc15,
  AttribFunc16,
  AttribFunc17,
  AttribFunc18, AttribFuncUndefined, AttribFuncUndefined,
  AttribFunc21,
  AttribFunc22,
  AttribFunc23,
  AttribFunc24, AttribFuncUndefined,
  AttribFunc26, AttribFuncUndefined,
  AttribFunc28,
  AttribFunc29,
  AttribFunc30,
  AttribFunc31,
  AttribFunc32, AttribFuncUndefined,
  AttribFunc34,
  AttribFunc35, AttribFuncUndefined,
  AttribFunc37,
  AttribFunc38,
  AttribFunc39,
  AttribFunc40, AttribFuncUndefined,
  AttribFunc42,
  AttribFunc43,
  AttribFunc44, AttribFuncUndefined,
  AttribFunc46,
  AttribFunc47, AttribFuncUndefined,
  AttribFunc49,
  AttribFunc50, AttribFuncUndefined, AttribFuncUndefined,
  AttribFunc53, AttribFuncUndefined,
  AttribFunc55, AttribFuncUndefined,
  AttribFunc57,
  AttribFunc58,
  AttribFunc59,
  AttribFunc60,
  AttribFunc61,
  AttribFunc62,
  AttribFunc63,
  AttribFunc64,
  AttribFunc65,
  AttribFunc66, AttribFuncUndefined, AttribFuncUndefined, AttribFuncUndefined,
  AttribFunc70, AttribFuncUndefined, AttribFuncUndefined, AttribFuncUndefined,
  AttribFunc74,
  AttribFunc75, AttribFuncUndefined, AttribFuncUndefined,
  AttribFunc78, AttribFuncUndefined,
  AttribFunc80, AttribFuncUndefined,
  AttribFunc82, AttribFuncUndefined,
  AttribFunc84,
  AttribFunc85, AttribFuncUndefined, AttribFuncUndefined,
  AttribFunc88,
  AttribFunc89, AttribFuncUndefined,
  AttribFunc91, AttribFuncUndefined,
  AttribFunc93, AttribFuncUndefined, AttribFuncUndefined,
  AttribFunc96,
  AttribFunc97, AttribFuncUndefined, AttribFuncUndefined, AttribFuncUndefined,
  AttribFunc101,
  AttribFunc102,
  AttribFunc103,
  AttribFunc104,
  AttribFunc105,
  AttribFunc106, AttribFuncUndefined,
  AttribFunc108, AttribFuncUndefined, AttribFuncUndefined,
  AttribFunc111, AttribFuncUndefined,
  AttribFunc113,
  AttribFunc114, AttribFuncUndefined,
  AttribFunc116,
  AttribFunc117, AttribFuncUndefined, AttribFuncUndefined,
  AttribFunc120,
  AttribFunc121,
  AttribFunc122,
  AttribFunc123,
  AttribFunc124,
  AttribFunc125,
  AttribFunc126, AttribFuncUndefined,
  AttribFunc128,
  AttribFunc129, AttribFuncUndefined,
  AttribFunc131,
  AttribFunc132, AttribFuncUndefined, AttribFuncUndefined,
  AttribFunc135,
  AttribFunc136
  );
 
implementation

uses rjHtmlParser;

{ Attrib Functions }

function AttribFuncUndefined (const p: PChar; const l: Integer): TAttribs;
begin
 Result := attribUndefined
end;

function AttribFunc0 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'CODETYPE', l) then
  Result := attribCodetype
 else
  if CompareTA (p, 'CONTENT', l) then
   Result := attribContent
  else
   Result := attribUndefined;
end;

function AttribFunc2 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'MEDIA', l) then
  Result := attribMedia
 else
  if CompareTA (p, 'VERSION', l) then
   Result := attribVersion
  else
   Result := attribUndefined;
end;

function AttribFunc3 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'CLASSID', l) then
  Result := attribClassid
 else
  Result := attribUndefined;
end;

function AttribFunc5 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'ENCTYPE', l) then
  Result := attribEnctype
 else
  Result := attribUndefined;
end;

function AttribFunc7 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'ONMOUSEMOVE', l) then
  Result := attribOnmousemove
 else
  Result := attribUndefined;
end;

function AttribFunc10 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'COLSPAN', l) then
  Result := attribColspan
 else
  Result := attribUndefined;
end;

function AttribFunc13 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'LONGDESC', l) then
  Result := attribLongdesc
 else
  Result := attribUndefined;
end;

function AttribFunc14 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'FACE', l) then
  Result := attribFace
 else
  Result := attribUndefined;
end;

function AttribFunc15 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'VALUE', l) then
  Result := attribValue
 else
  Result := attribUndefined;
end;

function AttribFunc16 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'MAXLENGTH', l) then
  Result := attribMaxlength
 else
  Result := attribUndefined;
end;

function AttribFunc17 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'SPAN', l) then
  Result := attribSpan
 else
  if CompareTA (p, 'STYLE', l) then
   Result := attribStyle
  else
   Result := attribUndefined;
end;

function AttribFunc18 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'LANGUAGE', l) then
  Result := attribLanguage
 else
  Result := attribUndefined;
end;

function AttribFunc21 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'NORESIZE', l) then
  Result := attribNoresize
 else
  Result := attribUndefined;
end;

function AttribFunc22 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'STANDBY', l) then
  Result := attribStandby
 else
  if CompareTA (p, 'PROFILE', l) then
   Result := attribProfile
  else
   Result := attribUndefined;
end;

function AttribFunc23 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'VLINK', l) then
  Result := attribVlink
 else
  Result := attribUndefined;
end;

function AttribFunc24 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'ARCHIVE', l) then
  Result := attribArchive
 else
  Result := attribUndefined;
end;

function AttribFunc26 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'SRC', l) then
  Result := attribSRC
 else
  if CompareTA (p, 'ACCESSKEY', l) then
   Result := attribAccesskey
  else
   Result := attribUndefined;
end;

function AttribFunc28 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'ALT', l) then
  Result := attribAlt
 else
  Result := attribUndefined;
end;

function AttribFunc29 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'BGCOLOR', l) then
  Result := attribBgcolor
 else
  Result := attribUndefined;
end;

function AttribFunc30 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'TABINDEX', l) then
  Result := attribTabindex
 else
  Result := attribUndefined;
end;

function AttribFunc31 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'SCROLLING', l) then
  Result := attribScrolling
 else
  Result := attribUndefined;
end;

function AttribFunc32 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'TEXT', l) then
  Result := attribText
 else
  Result := attribUndefined;
end;

function AttribFunc34 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'MARGINWIDTH', l) then
  Result := attribMarginwidth
 else
  Result := attribUndefined;
end;

function AttribFunc35 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'CITE', l) then
  Result := attribCite
 else
  Result := attribUndefined;
end;

function AttribFunc37 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'CLEAR', l) then
  Result := attribClear
 else
  if CompareTA (p, 'AXIS', l) then
   Result := attribAxis
  else
   Result := attribUndefined;
end;

function AttribFunc38 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'NOSHADE', l) then
  Result := attribNoshade
 else
  if CompareTA (p, 'BACKGROUND', l) then
   Result := attribBACKGROUND
  else
   Result := attribUndefined;
end;

function AttribFunc39 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'CHECKED', l) then
  Result := attribChecked
 else
  if CompareTA (p, 'ALIGN', l) then
   Result := attribAlign
  else
   Result := attribUndefined;
end;

function AttribFunc40 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'SCHEME', l) then
  Result := attribScheme
 else
  Result := attribUndefined;
end;

function AttribFunc42 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'FRAMEBORDER', l) then
  Result := attribFrameborder
 else
  Result := attribUndefined;
end;

function AttribFunc43 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'START', l) then
  Result := attribStart
 else
  Result := attribUndefined;
end;

function AttribFunc44 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'COMPACT', l) then
  Result := attribCompact
 else
  Result := attribUndefined;
end;

function AttribFunc46 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'LINK', l) then
  Result := attribLINK
 else
  Result := attribUndefined;
end;

function AttribFunc47 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'CHARSET', l) then
  Result := attribCharset
 else
  if CompareTA (p, 'DATA', l) then
   Result := attribData
  else
   Result := attribUndefined;
end;

function AttribFunc49 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'ROWSPAN', l) then
  Result := attribRowspan
 else
  if CompareTA (p, 'ONRESET', l) then
   Result := attribOnreset
  else
   if CompareTA (p, 'ABBR', l) then
    Result := attribAbbr
   else
    Result := attribUndefined;
end;

function AttribFunc50 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'CODEBASE', l) then
  Result := attribCodebase
 else
  Result := attribUndefined;
end;

function AttribFunc53 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'ONDBLCLICK', l) then
  Result := attribOndblclick
 else
  Result := attribUndefined;
end;

function AttribFunc55 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'WIDTH', l) then
  Result := attribWidth
 else
  if CompareTA (p, 'ACCEPT', l) then
   Result := attribAccept
  else
   if CompareTA (p, 'COORDS', l) then
    Result := attribCoords
   else
    Result := attribUndefined;
end;

function AttribFunc57 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'USEMAP', l) then
  Result := attribUsemap
 else
  Result := attribUndefined;
end;

function AttribFunc58 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'ONMOUSEDOWN', l) then
  Result := attribOnmousedown
 else
  Result := attribUndefined;
end;

function AttribFunc59 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'FOR', l) then
  Result := attribFor
 else
  Result := attribUndefined;
end;

function AttribFunc60 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'CHAR', l) then
  Result := attribChar
 else
  if CompareTA (p, 'COLS', l) then
   Result := attribCols
  else
   Result := attribUndefined;
end;

function AttribFunc61 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'CELLSPACING', l) then
  Result := attribCellspacing
 else
  Result := attribUndefined;
end;

function AttribFunc62 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'ONUNLOAD', l) then
  Result := attribOnunload
 else
  Result := attribUndefined;
end;

function AttribFunc63 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'ONMOUSEUP', l) then
  Result := attribOnmouseup
 else
  Result := attribUndefined;
end;

function AttribFunc64 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'CODE', l) then
  Result := attribCODE
 else
  if CompareTA (p, 'NOHREF', l) then
   Result := attribNohref
  else
   Result := attribUndefined;
end;

function AttribFunc65 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'ONFOCUS', l) then
  Result := attribOnfocus
 else
  if CompareTA (p, 'HEIGHT', l) then
   Result := attribHeight
  else
   Result := attribUndefined;
end;

function AttribFunc66 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'SHAPE', l) then
  Result := attribShape
 else
  if CompareTA (p, 'OBJECT', l) then
   Result := attribObject
  else
   Result := attribUndefined;
end;

function AttribFunc70 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'HREFLANG', l) then
  Result := attribHreflang
 else
  if CompareTA (p, 'LANG', l) then
   Result := attribLang
  else
   if CompareTA (p, 'HTTP-EQUIV', l) then
    Result := attribHttpequiv
   else
    Result := attribUndefined;
end;

function AttribFunc74 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'ONBLUR', l) then
  Result := attribOnblur
 else
  Result := attribUndefined;
end;

function AttribFunc75 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'ID', l) then
  Result := attribId
 else
  Result := attribUndefined;
end;

function AttribFunc78 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'BORDER', l) then
  Result := attribBorder
 else
  Result := attribUndefined;
end;

function AttribFunc80 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'SCOPE', l) then
  Result := attribScope
 else
  Result := attribUndefined;
end;

function AttribFunc82 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'VSPACE', l) then
  Result := attribVspace
 else
  Result := attribUndefined;
end;

function AttribFunc84 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'NOWRAP', l) then
  Result := attribNowrap
 else
  Result := attribUndefined;
end;

function AttribFunc85 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'READONLY', l) then
  Result := attribReadonly
 else
  Result := attribUndefined;
end;

function AttribFunc88 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'SIZE', l) then
  Result := attribSize
 else
  Result := attribUndefined;
end;

function AttribFunc89 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'CLASS', l) then
  Result := attribClass
 else
  Result := attribUndefined;
end;

function AttribFunc91 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'TARGET', l) then
  Result := attribTarget
 else
  Result := attribUndefined;
end;

function AttribFunc93 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'REL', l) then
  Result := attribRel
 else
  if CompareTA (p, 'ONKEYDOWN', l) then
   Result := attribOnkeydown
  else
   Result := attribUndefined;
end;

function AttribFunc96 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'DEFER', l) then
  Result := attribDefer
 else
  Result := attribUndefined;
end;

function AttribFunc97 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'MULTIPLE', l) then
  Result := attribMultiple
 else
  if CompareTA (p, 'ACCEPT-CHARSET', l) then
   Result := attribAcceptcharset
  else
   Result := attribUndefined;
end;

function AttribFunc101 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'ONCHANGE', l) then
  Result := attribOnchange
 else
  Result := attribUndefined;
end;

function AttribFunc102 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'METHOD', l) then
  Result := attribMethod
 else
  if CompareTA (p, 'COLOR', l) then
   Result := attribColor
  else
   if CompareTA (p, 'TYPE', l) then
    Result := attribType
   else
    if CompareTA (p, 'LABEL', l) then
     Result := attribLabel
    else
     Result := attribUndefined;
end;

function AttribFunc103 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'LOWSRC', l) then
  Result := attribLowsrc
 else
  if CompareTA (p, 'REV', l) then
   Result := attribRev
  else
   Result := attribUndefined;
end;

function AttribFunc104 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'MARGINHEIGHT', l) then
  Result := attribMarginheight
 else
  Result := attribUndefined;
end;

function AttribFunc105 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'RULES', l) then
  Result := attribRules
 else
  Result := attribUndefined;
end;

function AttribFunc106 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'HREF', l) then
  Result := attribHref
 else
  if CompareTA (p, 'SELECTED', l) then
   Result := attribSelected
  else
   Result := attribUndefined;
end;

function AttribFunc108 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'DISABLED', l) then
  Result := attribDisabled
 else
  Result := attribUndefined;
end;

function AttribFunc111 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'ROWS', l) then
  Result := attribRows
 else
  Result := attribUndefined;
end;

function AttribFunc113 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'ONSELECT', l) then
  Result := attribOnselect
 else
  Result := attribUndefined;
end;

function AttribFunc114 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'HEADERS', l) then
  Result := attribHeaders
 else
  if CompareTA (p, 'NAME', l) then
   Result := attribName
  else
   Result := attribUndefined;
end;

function AttribFunc116 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'ONLOAD', l) then
  Result := attribOnload
 else
  Result := attribUndefined;
end;

function AttribFunc117 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'DECLARE', l) then
  Result := attribDeclare
 else
  Result := attribUndefined;
end;

function AttribFunc120 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'CHAROFF', l) then
  Result := attribCharoff
 else
  Result := attribUndefined;
end;

function AttribFunc121 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'CELLPADDING', l) then
  Result := attribCellpadding
 else
  Result := attribUndefined;
end;

function AttribFunc122 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'VALUETYPE', l) then
  Result := attribValuetype
 else
  if CompareTA (p, 'ONMOUSEOVER', l) then
   Result := attribOnmouseover
  else
   Result := attribUndefined;
end;

function AttribFunc123 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'ONSUBMIT', l) then
  Result := attribOnsubmit
 else
  if CompareTA (p, 'ALINK', l) then
   Result := attribAlink
  else
   if CompareTA (p, 'PROMPT', l) then
    Result := attribPrompt
   else
    Result := attribUndefined;
end;

function AttribFunc124 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'DATETIME', l) then
  Result := attribDatetime
 else
  Result := attribUndefined;
end;

function AttribFunc125 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'DIR', l) then
  Result := attribDir
 else
  if CompareTA (p, 'VALIGN', l) then
   Result := attribValign
  else
   Result := attribUndefined;
end;

function AttribFunc126 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'ONKEYPRESS', l) then
  Result := attribOnkeypress
 else
  if CompareTA (p, 'ACTION', l) then
   Result := attribAction
  else
   if CompareTA (p, 'ONCLICK', l) then
    Result := attribOnclick
   else
    if CompareTA (p, 'ONMOUSEOUT', l) then
     Result := attribOnmouseout
    else
     Result := attribUndefined;
end;

function AttribFunc128 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'ONKEYUP', l) then
  Result := attribOnkeyup
 else
  Result := attribUndefined;
end;

function AttribFunc129 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'TITLE', l) then
  Result := attribTitle
 else
  Result := attribUndefined;
end;

function AttribFunc131 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'SUMMARY', l) then
  Result := attribSummary
 else
  Result := attribUndefined;
end;

function AttribFunc132 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'FRAME', l) then
  Result := attribFrame
 else
  Result := attribUndefined;
end;

function AttribFunc135 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'HSPACE', l) then
  Result := attribHspace
 else
  Result := attribUndefined;
end;

function AttribFunc136 (const p: PChar; const l: Integer): TAttribs;
begin
 if CompareTA (p, 'ISMAP', l) then
  Result := attribIsmap
 else
  Result := attribUndefined;
end;

end.

