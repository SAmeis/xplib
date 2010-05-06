unit rjHtmlEntities;
{
File:           rjHtmlEntities.pas
Version:        0.1 / Initial Public Release
Last Modified:  1. April 2000
Decription:     Various types, constants, and helper functions
                for the identification of HTML entities.
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

 TEntityFunc = function (const p: PChar; const l: Integer): Char;

 { Entity Function Definitions }
 
function EntityFuncUndefined (const p: PChar; const l: Integer): Char;
function EntityFunc0 (const p: PChar; const l: Integer): Char;
function EntityFunc1 (const p: PChar; const l: Integer): Char;
function EntityFunc4 (const p: PChar; const l: Integer): Char;
function EntityFunc5 (const p: PChar; const l: Integer): Char;
function EntityFunc6 (const p: PChar; const l: Integer): Char;
function EntityFunc8 (const p: PChar; const l: Integer): Char;
function EntityFunc9 (const p: PChar; const l: Integer): Char;
function EntityFunc11 (const p: PChar; const l: Integer): Char;
function EntityFunc14 (const p: PChar; const l: Integer): Char;
function EntityFunc15 (const p: PChar; const l: Integer): Char;
function EntityFunc17 (const p: PChar; const l: Integer): Char;
function EntityFunc19 (const p: PChar; const l: Integer): Char;
function EntityFunc20 (const p: PChar; const l: Integer): Char;
function EntityFunc21 (const p: PChar; const l: Integer): Char;
function EntityFunc22 (const p: PChar; const l: Integer): Char;
function EntityFunc26 (const p: PChar; const l: Integer): Char;
function EntityFunc27 (const p: PChar; const l: Integer): Char;
function EntityFunc28 (const p: PChar; const l: Integer): Char;
function EntityFunc31 (const p: PChar; const l: Integer): Char;
function EntityFunc33 (const p: PChar; const l: Integer): Char;
function EntityFunc34 (const p: PChar; const l: Integer): Char;
function EntityFunc35 (const p: PChar; const l: Integer): Char;
function EntityFunc38 (const p: PChar; const l: Integer): Char;
function EntityFunc42 (const p: PChar; const l: Integer): Char;
function EntityFunc44 (const p: PChar; const l: Integer): Char;
function EntityFunc45 (const p: PChar; const l: Integer): Char;
function EntityFunc47 (const p: PChar; const l: Integer): Char;
function EntityFunc48 (const p: PChar; const l: Integer): Char;
function EntityFunc49 (const p: PChar; const l: Integer): Char;
function EntityFunc51 (const p: PChar; const l: Integer): Char;
function EntityFunc57 (const p: PChar; const l: Integer): Char;
function EntityFunc60 (const p: PChar; const l: Integer): Char;
function EntityFunc62 (const p: PChar; const l: Integer): Char;
function EntityFunc65 (const p: PChar; const l: Integer): Char;
function EntityFunc66 (const p: PChar; const l: Integer): Char;
function EntityFunc67 (const p: PChar; const l: Integer): Char;
function EntityFunc69 (const p: PChar; const l: Integer): Char;
function EntityFunc70 (const p: PChar; const l: Integer): Char;
function EntityFunc72 (const p: PChar; const l: Integer): Char;
function EntityFunc74 (const p: PChar; const l: Integer): Char;
function EntityFunc75 (const p: PChar; const l: Integer): Char;
function EntityFunc76 (const p: PChar; const l: Integer): Char;
function EntityFunc77 (const p: PChar; const l: Integer): Char;
function EntityFunc79 (const p: PChar; const l: Integer): Char;
function EntityFunc80 (const p: PChar; const l: Integer): Char;
function EntityFunc82 (const p: PChar; const l: Integer): Char;
function EntityFunc83 (const p: PChar; const l: Integer): Char;
function EntityFunc85 (const p: PChar; const l: Integer): Char;
function EntityFunc87 (const p: PChar; const l: Integer): Char;
function EntityFunc93 (const p: PChar; const l: Integer): Char;
function EntityFunc94 (const p: PChar; const l: Integer): Char;
function EntityFunc96 (const p: PChar; const l: Integer): Char;
function EntityFunc97 (const p: PChar; const l: Integer): Char;
function EntityFunc99 (const p: PChar; const l: Integer): Char;
function EntityFunc101 (const p: PChar; const l: Integer): Char;
function EntityFunc103 (const p: PChar; const l: Integer): Char;
function EntityFunc104 (const p: PChar; const l: Integer): Char;
function EntityFunc107 (const p: PChar; const l: Integer): Char;
function EntityFunc108 (const p: PChar; const l: Integer): Char;
function EntityFunc110 (const p: PChar; const l: Integer): Char;
function EntityFunc111 (const p: PChar; const l: Integer): Char;
function EntityFunc112 (const p: PChar; const l: Integer): Char;
function EntityFunc113 (const p: PChar; const l: Integer): Char;
function EntityFunc114 (const p: PChar; const l: Integer): Char;
function EntityFunc118 (const p: PChar; const l: Integer): Char;
function EntityFunc119 (const p: PChar; const l: Integer): Char;
function EntityFunc123 (const p: PChar; const l: Integer): Char;
function EntityFunc124 (const p: PChar; const l: Integer): Char;
function EntityFunc127 (const p: PChar; const l: Integer): Char;
function EntityFunc128 (const p: PChar; const l: Integer): Char;
function EntityFunc129 (const p: PChar; const l: Integer): Char;
function EntityFunc131 (const p: PChar; const l: Integer): Char;
function EntityFunc134 (const p: PChar; const l: Integer): Char;
function EntityFunc135 (const p: PChar; const l: Integer): Char;
function EntityFunc138 (const p: PChar; const l: Integer): Char;
function EntityFunc139 (const p: PChar; const l: Integer): Char;

const
 
 ENTITY_HASHSIZE              = 141;
 
 { Entity Function Table }
 
 EntityFuncTable              : array[0..ENTITY_HASHSIZE - 1] of TEntityFunc = (
  EntityFunc0,
  EntityFunc1, EntityFuncUndefined, EntityFuncUndefined,
  EntityFunc4,
  EntityFunc5,
  EntityFunc6, EntityFuncUndefined,
  EntityFunc8,
  EntityFunc9, EntityFuncUndefined,
  EntityFunc11, EntityFuncUndefined, EntityFuncUndefined,
  EntityFunc14,
  EntityFunc15, EntityFuncUndefined,
  EntityFunc17, EntityFuncUndefined,
  EntityFunc19,
  EntityFunc20,
  EntityFunc21,
  EntityFunc22, EntityFuncUndefined, EntityFuncUndefined, EntityFuncUndefined,
  EntityFunc26,
  EntityFunc27,
  EntityFunc28, EntityFuncUndefined, EntityFuncUndefined,
  EntityFunc31, EntityFuncUndefined,
  EntityFunc33,
  EntityFunc34,
  EntityFunc35, EntityFuncUndefined, EntityFuncUndefined,
  EntityFunc38, EntityFuncUndefined, EntityFuncUndefined, EntityFuncUndefined,
  EntityFunc42, EntityFuncUndefined,
  EntityFunc44,
  EntityFunc45, EntityFuncUndefined,
  EntityFunc47,
  EntityFunc48,
  EntityFunc49, EntityFuncUndefined,
  EntityFunc51, EntityFuncUndefined, EntityFuncUndefined, EntityFuncUndefined, EntityFuncUndefined, EntityFuncUndefined,
  EntityFunc57, EntityFuncUndefined, EntityFuncUndefined,
  EntityFunc60, EntityFuncUndefined,
  EntityFunc62, EntityFuncUndefined, EntityFuncUndefined,
  EntityFunc65,
  EntityFunc66,
  EntityFunc67, EntityFuncUndefined,
  EntityFunc69,
  EntityFunc70, EntityFuncUndefined,
  EntityFunc72, EntityFuncUndefined,
  EntityFunc74,
  EntityFunc75,
  EntityFunc76,
  EntityFunc77, EntityFuncUndefined,
  EntityFunc79,
  EntityFunc80, EntityFuncUndefined,
  EntityFunc82,
  EntityFunc83, EntityFuncUndefined,
  EntityFunc85, EntityFuncUndefined,
  EntityFunc87, EntityFuncUndefined, EntityFuncUndefined, EntityFuncUndefined, EntityFuncUndefined, EntityFuncUndefined,
  EntityFunc93,
  EntityFunc94, EntityFuncUndefined,
  EntityFunc96,
  EntityFunc97, EntityFuncUndefined,
  EntityFunc99, EntityFuncUndefined,
  EntityFunc101, EntityFuncUndefined,
  EntityFunc103,
  EntityFunc104, EntityFuncUndefined, EntityFuncUndefined,
  EntityFunc107,
  EntityFunc108, EntityFuncUndefined,
  EntityFunc110,
  EntityFunc111,
  EntityFunc112,
  EntityFunc113,
  EntityFunc114, EntityFuncUndefined, EntityFuncUndefined, EntityFuncUndefined,
  EntityFunc118,
  EntityFunc119, EntityFuncUndefined, EntityFuncUndefined, EntityFuncUndefined,
  EntityFunc123,
  EntityFunc124, EntityFuncUndefined, EntityFuncUndefined,
  EntityFunc127,
  EntityFunc128,
  EntityFunc129, EntityFuncUndefined,
  EntityFunc131, EntityFuncUndefined, EntityFuncUndefined,
  EntityFunc134,
  EntityFunc135, EntityFuncUndefined, EntityFuncUndefined,
  EntityFunc138,
  EntityFunc139, EntityFuncUndefined
  );
 
implementation

uses rjHtmlParser;

{ Entity Functions }

function EntityFuncUndefined (const p: PChar; const l: Integer): Char;
begin
 Result := #0
end;

function EntityFunc0 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'Yacute', l) then
  Result := 'Ý'
 else
  if CompareE (p, 'curren', l) then
   Result := '¤'
  else
   Result := #0;
end;

function EntityFunc1 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'acirc', l) then
  Result := 'â'
 else
  if CompareE (p, 'amp', l) then
   Result := '&'
  else
   Result := #0;
end;

function EntityFunc4 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'agrave', l) then
  Result := 'à'
 else
  if CompareE (p, 'middot', l) then
   Result := '·'
  else
   Result := #0;
end;

function EntityFunc5 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'ordm', l) then
  Result := 'º'
 else
  Result := #0;
end;

function EntityFunc6 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'eth', l) then
  Result := 'ð'
 else
  Result := #0;
end;

function EntityFunc8 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'Ccedil', l) then
  Result := 'Ç'
 else
  Result := #0;
end;

function EntityFunc9 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'shy', l) then
  Result := #173 // Soft Hyphen
 else
  if CompareE (p, 'para', l) then
   Result := '¶'
  else
   if CompareE (p, 'reg', l) then
    Result := '®'
   else
    Result := #0;
end;

function EntityFunc11 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'pound', l) then
  Result := '£'
 else
  Result := #0;
end;

function EntityFunc14 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'Eacute', l) then
  Result := 'É'
 else
  if CompareE (p, 'ocirc', l) then
   Result := 'ô'
  else
   Result := #0;
end;

function EntityFunc15 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'THORN', l) then
  Result := 'Þ'
 else
  Result := #0;
end;

function EntityFunc17 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'iacute', l) then
  Result := 'í'
 else
  Result := #0;
end;

function EntityFunc19 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'iuml', l) then
  Result := 'ï'
 else
  Result := #0;
end;

function EntityFunc20 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'aelig', l) then
  Result := 'æ'
 else
  Result := #0;
end;

function EntityFunc21 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'Ecirc', l) then
  Result := 'Ê'
 else
  Result := #0;
end;

function EntityFunc22 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'AElig', l) then
  Result := 'Æ'
 else
  Result := #0;
end;

function EntityFunc26 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'Aring', l) then
  Result := 'Å'
 else
  Result := #0;
end;

function EntityFunc27 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'Uuml', l) then
  Result := 'Ü'
 else
  if CompareE (p, 'ucirc', l) then
   Result := 'û'
  else
   Result := #0;
end;

function EntityFunc28 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'sect', l) then
  Result := '§'
 else
  Result := #0;
end;

function EntityFunc31 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'Uacute', l) then
  Result := 'Ú'
 else
  if CompareE (p, 'Ouml', l) then
   Result := 'Ö'
  else
   Result := #0;
end;

function EntityFunc33 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'sup1', l) then
  Result := '¹'
 else
  Result := #0;
end;

function EntityFunc34 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'yacute', l) then
  Result := 'ý'
 else
  if CompareE (p, 'sup2', l) then
   Result := '²'
  else
   Result := #0;
end;

function EntityFunc35 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'Auml', l) then
  Result := 'Ä'
 else
  if CompareE (p, 'euro', l) then
   Result := '€'
  else
   if CompareE (p, 'sup3', l) then
    Result := '³'
   else
    Result := #0;
end;

function EntityFunc38 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'divide', l) then
  Result := '÷'
 else
  if CompareE (p, 'Oacute', l) then
   Result := 'Ó'
  else
   Result := #0;
end;

function EntityFunc42 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'nbsp', l) then
  Result := #160 // Non-Breaking Space
 else
  if CompareE (p, 'ccedil', l) then
   Result := 'ç'
  else
   Result := #0;
end;

function EntityFunc44 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'Acirc', l) then
  Result := 'Â'
 else
  Result := #0;
end;

function EntityFunc45 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'Aacute', l) then
  Result := 'Á'
 else
  Result := #0;
end;

function EntityFunc47 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'lt', l) then
  Result := '<'
 else
  if CompareE (p, 'frac12', l) then
   Result := '½'
  else
   if CompareE (p, 'die', l) then
    Result := '¨'
   else
    Result := #0;
end;

function EntityFunc48 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'Oslash', l) then
  Result := 'Ø'
 else
  if CompareE (p, 'eacute', l) then
   Result := 'é'
  else
   if CompareE (p, 'laquo', l) then
    Result := '«'
   else
    if CompareE (p, 'macr', l) then
     Result := '¯'
    else
     Result := #0;
end;

function EntityFunc49 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'Igrave', l) then
  Result := 'Ì'
 else
  if CompareE (p, 'frac14', l) then
   Result := '¼'
  else
   Result := #0;
end;

function EntityFunc51 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'Ntilde', l) then
  Result := 'Ñ'
 else
  Result := #0;
end;

function EntityFunc57 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'Ocirc', l) then
  Result := 'Ô'
 else
  Result := #0;
end;

function EntityFunc60 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'Iuml', l) then
  Result := 'Ï'
 else
  Result := #0;
end;

function EntityFunc62 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'deg', l) then
  Result := '°'
 else
  Result := #0;
end;

function EntityFunc65 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'uacute', l) then
  Result := 'ú'
 else
  Result := #0;
end;

function EntityFunc66 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'not', l) then
  Result := '¬'
 else
  Result := #0;
end;

function EntityFunc67 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'yen', l) then
  Result := '¥'
 else
  Result := #0;
end;

function EntityFunc69 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'yuml', l) then
  Result := 'ÿ'
 else
  if CompareE (p, 'COPY', l) then
   Result := '©'
  else
   Result := #0;
end;

function EntityFunc70 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'Ucirc', l) then
  Result := 'Û'
 else
  Result := #0;
end;

function EntityFunc72 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'oacute', l) then
  Result := 'ó'
 else
  Result := #0;
end;

function EntityFunc74 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'iquest', l) then
  Result := '¿'
 else
  Result := #0;
end;

function EntityFunc75 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'acute', l) then
  Result := '´'
 else
  Result := #0;
end;

function EntityFunc76 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'times', l) then
  Result := '×'
 else
  Result := #0;
end;

function EntityFunc77 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'euml', l) then
  Result := 'ë'
 else
  Result := #0;
end;

function EntityFunc79 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'aacute', l) then
  Result := 'á'
 else
  Result := #0;
end;

function EntityFunc80 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'Egrave', l) then
  Result := 'È'
 else
  Result := #0;
end;

function EntityFunc82 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'oslash', l) then
  Result := 'ø'
 else
  Result := #0;
end;

function EntityFunc83 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'igrave', l) then
  Result := 'ì'
 else
  Result := #0;
end;

function EntityFunc85 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'ntilde', l) then
  Result := 'ñ'
 else
  Result := #0;
end;

function EntityFunc87 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'szlig', l) then
  Result := 'ß'
 else
  Result := #0;
end;

function EntityFunc93 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'brvbar', l) then
  Result := '¦'
 else
  if CompareE (p, 'cedil', l) then
   Result := '¸'
  else
   Result := #0;
end;

function EntityFunc94 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'Otilde', l) then
  Result := 'Õ'
 else
  Result := #0;
end;

function EntityFunc96 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'ETH', l) then
  Result := 'Ð'
 else
  if CompareE (p, 'icirc', l) then
   Result := 'î'
  else
   Result := #0;
end;

function EntityFunc97 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'Ugrave', l) then
  Result := 'Ù'
 else
  Result := #0;
end;

function EntityFunc99 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'hibar', l) then
  Result := '¯'
 else
  Result := #0;
end;

function EntityFunc101 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'Atilde', l) then
  Result := 'Ã'
 else
  Result := #0;
end;

function EntityFunc103 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'iexcl', l) then
  Result := '¡'
 else
  Result := #0;
end;

function EntityFunc104 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'Ograve', l) then
  Result := 'Ò'
 else
  Result := #0;
end;

function EntityFunc107 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'raquo', l) then
  Result := '»'
 else
  Result := #0;
end;

function EntityFunc108 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'copy', l) then
  Result := '©'
 else
  Result := #0;
end;

function EntityFunc110 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'plusmn', l) then
  Result := '±'
 else
  Result := #0;
end;

function EntityFunc111 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'Agrave', l) then
  Result := 'À'
 else
  Result := #0;
end;

function EntityFunc112 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'cent', l) then
  Result := '¢'
 else
  Result := #0;
end;

function EntityFunc113 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'frac34', l) then
  Result := '¾'
 else
  Result := #0;
end;

function EntityFunc114 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'egrave', l) then
  Result := 'è'
 else
  Result := #0;
end;

function EntityFunc118 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'gt', l) then
  Result := '>'
 else
  if CompareE (p, 'Euml', l) then
   Result := 'Ë'
  else
   Result := #0;
end;

function EntityFunc119 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'ecirc', l) then
  Result := 'ê'
 else
  Result := #0;
end;

function EntityFunc123 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'um', l) then
  Result := '¨'
 else
  if CompareE (p, 'thorn', l) then
   Result := 'þ'
  else
   Result := #0;
end;

function EntityFunc124 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'aring', l) then
  Result := 'å'
 else
  if CompareE (p, 'Iacute', l) then
   Result := 'Í'
  else
   Result := #0;
end;

function EntityFunc127 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'uuml', l) then
  Result := 'ü'
 else
  Result := #0;
end;

function EntityFunc128 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'otilde', l) then
  Result := 'õ'
 else
  Result := #0;
end;

function EntityFunc129 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'quot', l) then
  Result := '"'
 else
  Result := #0;
end;

function EntityFunc131 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'ugrave', l) then
  Result := 'ù'
 else
  if CompareE (p, 'ouml', l) then
   Result := 'ö'
  else
   Result := #0;
end;

function EntityFunc134 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'micro', l) then
  Result := 'µ'
 else
  Result := #0;
end;

function EntityFunc135 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'auml', l) then
  Result := 'ä'
 else
  if CompareE (p, 'atilde', l) then
   Result := 'ã'
  else
   Result := #0;
end;

function EntityFunc138 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'ograve', l) then
  Result := 'ò'
 else
  Result := #0;
end;

function EntityFunc139 (const p: PChar; const l: Integer): Char;
begin
 if CompareE (p, 'ordf', l) then
  Result := 'ª'
 else
  if CompareE (p, 'Icirc', l) then
   Result := 'Î'
  else
   Result := #0;
end;

end.

