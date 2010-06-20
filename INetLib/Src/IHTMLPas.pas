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
        FFileName : string;
    public
        constructor Create(sFileName : string);
        function AsHTML : string; override; stdcall;
        property FileName : string read FFileName write FFileName;
    end;

var
    ReservedFormat : string;
    StringFormat :   string;
    CommentFormat :  string;
    NumberFormat :   string;
    PlainFormat :    string;

function PascalStyleSheet : THTMLStyleSheet;

implementation

uses
    Registry, PasParser;

procedure LoadFormats;
// Get formatting for Pascal files
var
    reg : TRegistry;
    // .....................................................................................................................
    function ReadFormat(sKey, sDefault : string) : string;
    const
        clrColours: array [0 .. 15] of TColor = (clBlack, clMaroon, clGreen, clOlive, clNavy, clPurple, clAqua, clSilver, clGray,
            clRed, clLime, clYellow, clBlue, clFuchsia, clTeal, clWhite);
    var
        clr : TColor;
        // Read and decode a single format entry - in Delphi 4 each item has a separate entry
        // ......................................................................................................
        function ReadBoolean(sName : string) : boolean;
            { Translate string into Boolean value }
        var
            sBool : string;
        begin
            try
                sBool := reg.ReadString(sName);
                if sBool = '' then begin
                    Abort;
                end;
                Result := (sBool[1] in ['-', 'T']); { "-1" or "True" }
            except
                Result := False;
            end;
        end;

        // .....................................................................................................................
    begin
        try
            Result := '';
            // Decide de onde pegar as cores de acordo com o compilador usado(RTLVersion)
            if (RTLVersion > 15.00) then begin
                // Delphi 7
                if not reg.OpenKey('\Software\Borland\Delphi\7.0\Editor\Highlight\' + sKey, False) then begin
                    Exit;
                end;
            end else begin
                if (RTLVersion = 21.00) then begin
                    // Delphi 2010
                    if not reg.OpenKey('\Software\CodeGear\BDS\7.0\Editor\Highlight\' + sKey, False) then    begin
                        Exit;
                    end;
                end else begin
                    raise Exception.Create('Compilador usado não encontrado neste computador para leitura de padrão de cores');
                end;
            end;

            { Foreground colour }
            try
                clr := clrColours[reg.ReadInteger('Foreground Color')];
                { Use specified foreground colour ? }
                if not ReadBoolean('Default Foreground') then begin
                    Result := Result + '; color: rgb(' + IntToStr(GetRValue(clr)) + ',' + IntToStr(GetGValue(clr)) + ',' + IntToStr
                        (GetBValue(clr)) + ')';
                end;
            except { Ignore }
            end;
            { Background colour }
            try
                clr := clrColours[reg.ReadInteger('Background Color')];
                { Use specified background colour ? }
                if not ReadBoolean('Default Background') then begin
                    Result := Result + '; background-color: rgb(' + IntToStr(GetRValue(clr)) + ',' + IntToStr(GetGValue(clr))
                        + ',' + IntToStr(GetBValue(clr)) + ')';
                end;
            except { Ignore }
            end;
            { Bold, italic, underline }
            if ReadBoolean('Bold') then begin
                Result := Result + '; font-weight: bold';
            end;
            if ReadBoolean('Italic') then begin
                Result := Result + '; font-style: italic';
            end;
            if ReadBoolean('Underline') then begin
                Result := Result + '; text-decoration: underline';
            end;
        finally
            if Result = '' then begin
                Result := sDefault;
            end else begin
                Result := Copy(Result, 3, Length(Result) - 2);
            end;
        end;
    end;

begin
    reg := TRegistry.Create;
    with reg do begin
        try
            if (RTLVersion = 15.00) then begin
                { Read Pascal formatting selections }
                if OpenKey('\Software\Borland\Delphi\7.0\Editor\Highlight', False) then begin
                    ReservedFormat := ReadFormat('Reserved word', 'font-weight: bold');
                    StringFormat   := ReadFormat('String', '');
                    CommentFormat  := ReadFormat('Comment', 'font-style: italic; color: blue');
                    NumberFormat   := ReadFormat('Number', '');
                    PlainFormat    := ReadFormat('Identifier', '');
                end;
            end else begin
                if (RTLVersion = 21.00) then begin
                    if OpenKey('\Software\CodeGear\BDS\7.0\Editor\Highlight', False) then begin
                        ReservedFormat := ReadFormat('Reserved word', 'font-weight: bold');
                        StringFormat   := ReadFormat('String', '');
                        CommentFormat  := ReadFormat('Comment', 'font-style: italic; color: blue');
                        NumberFormat   := ReadFormat('Number', '');
                        PlainFormat    := ReadFormat('Identifier', '');
                    end;
                end;
            end;
        finally
            Free;
        end;
    end;
end;

{ Return a Pascal source style sheet }
function PascalStyleSheet : THTMLStyleSheet;
begin
    { Add formatting for Pascal elements }
    Result := THTMLStyleSheet.Create;
    try
        with Result do begin
            if ReservedFormat <> '' then begin
                AddStyle('span.reserved', ReservedFormat);
            end;
            if StringFormat <> '' then begin
                AddStyle('span.string', StringFormat);
            end;
            if CommentFormat <> '' then begin
                AddStyle('span.comment', CommentFormat);
            end;
            if NumberFormat <> '' then begin
                AddStyle('span.number', NumberFormat);
            end;
            if PlainFormat <> '' then begin
                AddStyle('pre', PlainFormat);
            end;
        end;
    except
        Result.Free;
        raise;
    end;
end;

{ THTMLPascal ----------------------------------------------------------------- }

{ Initialisation }
constructor THTMLPascal.Create(sFileName : string);
begin
    inherited Create;
    FFileName := sFileName;
end;

{ Return the Pascal source file formatted as HTML }
function THTMLPascal.AsHTML : string;
var
    htx :    THTMLText;
    hdv :    THTMLDivision;
    stm :    TFileStream;
    psr :    TPascalParser;
    sClass : string;
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
                       PasParser.toReserved : begin
                            sClass := 'reserved';
                        end;
                        PasParser.toString : begin
                            sClass := 'string';
                        end;
                        PasParser.toComment : begin
                            sClass := 'comment';
                        end;
                        PasParser.toNumber : begin
                            sClass := 'number';
                        end;
                        else
                        begin
                            sClass := '';
                        end;
                    end;
                    { Add (formatted) text to document }
                    if sClass <> '' then begin
                        hdv := THTMLDivision.Create(False, '', EscapeText(psr.TokenString));
                        hdv.TagClass := sClass;
                        htx.Add(hdv);
                    end else begin
                        htx.Add(THTMLText.Create(tsNormal, EscapeText(psr.TokenString)));
                    end;
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
