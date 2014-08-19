{$IFDEF ClassHnd }
	{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I OPLib.inc}

unit ClassHnd;

interface


uses
	Classes;


function ComponentToString(Component : TComponent) : string;
function LoadComponentFromFile(const FileName : string) : TComponent;
procedure SaveComponentToFile(Instance : TComponent; const FileName : string);
function StringToComponent(Value : string) : TComponent;

implementation

uses
    SysUtils;

function ComponentToString(Component : TComponent) : string;
    {------------------------------------------------------------------------------------------------------------}
var
    BinStream : TMemoryStream;
    StrStream : TStringStream;
    s : string;
begin
    s := '';
    BinStream := TMemoryStream.Create;
    try
        StrStream := TStringStream.Create(s);
        try
            BinStream.WriteComponent(Component);
            BinStream.Seek(0, soFromBeginning);
            ObjectBinaryToText(BinStream, StrStream);
            StrStream.Seek(0, soFromBeginning);
            Result := StrStream.DataString;
        finally
            StrStream.Free;
        end;
    finally
        BinStream.Free
    end;
end;

function LoadComponentFromFile(const FileName : string) : TComponent;
    //----------------------------------------------------------------------------------------------------------------------
var
    PList : TStringList;
    CompStr : string;
begin
    PList := TStringList.Create;
    try
        try
            PList.LoadFromFile(FileName);
            CompStr := PList.Text;
            PList.Free;
        except
            PList.Free;
            raise;
        end;
        Result := StringToComponent(CompStr);
    except //Criacao original
        Result := NIL;
    end;
end;

procedure SaveComponentToFile(Instance : TComponent; const FileName : string);
//----------------------------------------------------------------------------------------------------------------------
var
    SList : TStringList;
begin
    SList := TStringList.Create;
    try
        SList.Text := ComponentToString(Instance);
        try
            SList.SaveToFile(FileName);
        except
            raise Exception.CreateFmt('Componente %s não pode ser salvo em %s', [Instance.Name, FileName]);
        end;
    finally
        SList.Free;
    end;
end;

function StringToComponent(Value : string) : TComponent;
    {------------------------------------------------------------------------------------------------------------}
var
    StrStream : TStringStream;
    BinStream : TMemoryStream;
begin
    StrStream := TStringStream.Create(Value);
    try
        BinStream := TMemoryStream.Create;
        try
            ObjectTextToBinary(StrStream, BinStream);
            BinStream.Seek(0, soFromBeginning);
            Result := BinStream.ReadComponent(NIL);
        finally
            BinStream.Free;
        end;
    finally
        StrStream.Free;
    end;
end;


end.


