unit IHTMLPas;

{
  THTMLPascal - generate a Pascal source file as HTML.

  Written by Keith Wood, 30 July 1998.
}

interface

uses
  Windows, Classes, SysUtils, Graphics, IHTML4;

type
  THTMLPascal = class(THTMLBase)
  private
    FFileName: String;
  public
    constructor Create(sFileName: String);
    function AsHTML: String; override; stdcall;
    property FileName: String read FFileName write FFileName;
  end;

var
  ReservedFormat: String;
  StringFormat: String;
  CommentFormat: String;
  NumberFormat: String;
  PlainFormat: String;

function PascalStyleSheet: THTMLStyleSheet;

implementation

uses
  Registry, PasParser;

procedure LoadFormats;
//Get formatting for Pascal files
var
  reg: TRegistry;
//.....................................................................................................................
   function ReadFormat(sKey, sDefault: String): String;
   const
     clrColours: array [0..15] of TColor = ( clBlack, clMaroon, clGreen, clOlive, clNavy, clPurple, clAqua, clSilver,
                                             clGray, clRed, clLime, clYellow, clBlue, clFuchsia, clTeal, clWhite);
   var
     clr: TColor;
   //Read and decode a single format entry - in Delphi 4 each item has a separate entry
   //......................................................................................................
     function ReadBoolean(sName: String): Boolean;
     { Translate string into Boolean value }
     var
       sBool: String;
     begin
       try
         sBool := reg.ReadString(sName);
         if sBool = '' then
           Abort;
         Result := (sBool[1] in ['-', 'T']);  { "-1" or "True" }
       except
         Result := False;
       end;
     end;
//.....................................................................................................................
  begin
    try
      Result := '';
	   {$IFNDEF VER150}
	   --Origem das cores Ver abaixo.
	   {$ENDIF}
	   if not reg.OpenKey('\Software\Borland\Delphi\7.0\Editor\Highlight\' +
          sKey, False) then
        Exit;

      { Foreground colour }
      try
        clr := clrColours[reg.ReadInteger('Foreground Color')];
        { Use specified foreground colour ? }
        if not ReadBoolean('Default Foreground') then
          Result := Result + '; color: rgb(' + IntToStr(GetRValue(clr)) + ',' +
            IntToStr(GetGValue(clr)) + ',' + IntToStr(GetBValue(clr)) + ')';
      except  { Ignore }
      end;
      { Background colour }
      try
        clr := clrColours[reg.ReadInteger('Background Color')];
        { Use specified background colour ? }
        if not ReadBoolean('Default Background') then
          Result := Result + '; background-color: rgb(' + IntToStr(GetRValue(clr)) + ',' +
            IntToStr(GetGValue(clr)) + ',' + IntToStr(GetBValue(clr)) + ')';
      except  { Ignore }
      end;
      { Bold, italic, underline }
      if ReadBoolean('Bold') then
        Result := Result + '; font-weight: bold';
      if ReadBoolean('Italic') then
        Result := Result + '; font-style: italic';
      if ReadBoolean('Underline') then
        Result := Result + '; text-decoration: underline';
    finally
      if Result = '' then
        Result := sDefault
      else
        Result := Copy(Result, 3, Length(Result) - 2);
    end;
  end;

begin
  reg := TRegistry.Create;
  with reg do
    try
      {$IFNDEF VER150}
	   --Origem das cores Ver abaixo.
      {$ENDIF}
      { Read Pascal formatting selections }
      if OpenKey('\Software\Borland\Delphi\6.0\Editor\Highlight', False) then
      begin
        ReservedFormat := ReadFormat('Reserved word', 'font-weight: bold');
        StringFormat := ReadFormat('String', '');
        CommentFormat := ReadFormat('Comment', 'font-style: italic; color: blue');
        NumberFormat := ReadFormat('Number', '');
        PlainFormat := ReadFormat('Identifier', '');
      end;
    finally
      Free;
    end;
end;

{ Return a Pascal source style sheet }
function PascalStyleSheet: THTMLStyleSheet;
begin
  { Add formatting for Pascal elements }
  Result := THTMLStyleSheet.Create;
  try
    with Result do
    begin
      if ReservedFormat <> '' then
        AddStyle('span.reserved', ReservedFormat);
      if StringFormat <> '' then
        AddStyle('span.string', StringFormat);
      if CommentFormat <> '' then
        AddStyle('span.comment', CommentFormat);
      if NumberFormat <> '' then
        AddStyle('span.number', NumberFormat);
      if PlainFormat <> '' then
        AddStyle('pre', PlainFormat);
    end;
  except
    Result.Free;
    raise;
  end;
end;

{ THTMLPascal -----------------------------------------------------------------}

{ Initialisation }
constructor THTMLPascal.Create(sFileName: String);
begin
  inherited Create;
  FFileName := sFileName;
end;

{ Return the Pascal source file formatted as HTML }
function THTMLPascal.AsHTML: String;
var
  htx: THTMLText;
  hdv: THTMLDivision;
  stm: TFileStream;
  psr: TPascalParser;
  sClass: String;
begin
   { Preformat the rest of the source file }
   htx := THTMLText.Create(tsPreformat, '');
   try
       { Open specified file }
       stm := TFileStream.Create(FileName, fmOpenRead);
       try
           { And prepare to parse it }
           psr := TPascalParser.Create(stm, True);
           try
             while psr.Token <> toEOF do begin
               { Set class of formatting from token type }
               case psr.Token of
                 toReserved: sClass := 'reserved';
                 toString:   sClass := 'string';
                 toComment:  sClass := 'comment';
                 toNumber:   sClass := 'number';
                 else        sClass := '';
               end;
               { Add (formatted) text to document }
               if sClass <> '' then begin
                 hdv := THTMLDivision.Create(False, '', EscapeText(psr.TokenString));
                 hdv.TagClass := sClass;
                 htx.Add(hdv);
               end else
                 htx.Add(THTMLText.Create(tsNormal, EscapeText(psr.TokenString)));
               psr.NextToken;
             end;
           finally
             { Free resources }
             psr.Free;
           end;
           Result := htx.AsHTML;
       finally
           stm.Free;
       end;
   finally
     htx.Free;
   end;
end;

initialization
  LoadFormats;
end.
