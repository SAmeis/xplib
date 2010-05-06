{$IFDEF GDBHnd}
	{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I GDBLib.inc}

unit GDBHnd;

interface

uses
    DB, Variants, SysUtils, Classes;

type
    TGDBHnd = class
    public
        class procedure RefreshTables(DataSet : TDataSet; Recursive : boolean);
        class function DataSetLocateThrough(DataSet : TDataSet; const KeyFields : string; const KeyValues : variant;
            Options : TLocateOptions) : boolean;
        class procedure AssignRecord(Source, Dest : TDataSet; ByName : boolean);
        class procedure DBError(const Msg : string);
    end;

type
    TDBFieldPair = record
        Field1:  TField;
        Field2:  TField;
        Enabled: boolean;
    end;
    PDBFieldPair = ^TDBFieldPair;

    TDataSetFieldMapping = class
    {{
    Classe utilitaria para a copia de valores entre dataset de forma controlada.

    Exemplo de uso:
        Cloner := TDataSetFieldMapping.Create(Self.SrcTbl, Self.TblResult, NIL); //Todos os campos da origem encontrados no destino
        try
            c := 0;
            Self.SrcTbl.First;
            while (not Self.SrcTbl.Eof) and (c < 100) do begin
                Inc(c);
                Cloner.Append();
                Self.SrcTbl.Next;
            end;
        finally
            Cloner.Free;
        end;

    Revision: 8/9/2005 - Roger  
    }
    private
        function GetCount : Integer;
        function GetPair(Index : Integer) : TDBFieldPair;
    protected
        Associations : TList;
        Source :      TDataSet;
        Destination : TDataSet;
        procedure ClearAssociations;
    public
        constructor Create(ASource, ADestination : TDataSet; Mapping : TStringList); overload;
        constructor Create(ASource, ADestination : TDataSet; Mapping : string); overload;
        destructor Destroy; override;
        function CheckReferences : boolean;
        procedure Copy(CallPost : boolean = True);
        procedure Append(CallPost : boolean = True);
        property Pair[Index : Integer] : TDBFieldPair read GetPair;
        property Count : Integer read GetCount;
    end;

//Copia conjunto de campos de um registro  da table fonte para a tabela destino
procedure CloneFieldsValues(SrcDataSet, DestDataSet : TDataSet; PostRecord : boolean = False;
    NewRecord : boolean = False; IgnoreMissing : boolean = True);
//Busca em DataSet pelos valores passados no array de variantes
function DataSetLocate(DataSet : TDataSet; const FieldsStrList : string; const FieldValues : array of variant;
    LocateOptions : TLocateOptions = [loCaseInsensitive, loPartialKey]) : boolean;

implementation

uses
    DBConsts, Math;

procedure CloneFieldsValues(SrcDataSet, DestDataSet : TDataSet; PostRecord : boolean = False;
    NewRecord : boolean = False; IgnoreMissing : boolean = True);
//----------------------------------------------------------------------------------------------------------------------
var
    i : Integer;
    FldName : string;
    DestFld : TField;
begin
    try
        if NewRecord then begin
            DestDataSet.Append();
        end else begin
            DestDataSet.Edit();
        end;
        for i := 0 to SrcDataSet.FieldCount - 1 do begin
            FldName := SrcDataSet.Fields[i].FieldName;
            DestFld := DestDataSet.FindField(FldName);
            if not (Assigned(DestFld) or IgnoreMissing) then begin
                raise Exception.CreateFmt('Tabela %s não é um sub-conjunto de %s (Ausência do campo %s)',
                    [SrcDataSet.Name, DestDataSet.Name, FldName]);
            end else begin
                if not Assigned(DestFld) then begin
                    System.Continue;
                end;
            end;
            try
                DestFld.Value := SrcDataSet.Fields[i].Value; //Usa valor do campo achado acima
            except
                on E : Exception do begin
                    raise Exception.CreateFmt('Os campos de nome "%s" são incompatíveis.'#13'%s', [FldName, E.Message]);
                end;
            end;
        end;
        if PostRecord then begin
            DestDataSet.Post();
        end;
    except
        DestDataSet.Cancel();
        raise;
    end;
end;

function DataSetLocate(DataSet : TDataSet; const FieldsStrList : string; const FieldValues : array of variant;
    LocateOptions : TLocateOptions = [loCaseInsensitive, loPartialKey]) : boolean;
{{
Busca em DataSet pelos valores passados no array de variantes

Revision: 8/9/2005 - Roger
}
var
    arr : variant;
    i :   Integer;
begin
    try
        if High(FieldValues) >= 1 then begin  //Previne bug do tipo do variant no locate
            arr := VarArrayCreate([Low(FieldValues), High(FieldValues)], varOleStr);
            for i := Low(FieldValues) to High(FieldValues) do begin
                arr[i] := FieldValues[i];
            end;
        end else begin
            arr := FieldValues[High(FieldValues)];
        end;
        Result := DataSet.Locate(FieldsStrList, arr, LocateOptions);
    except
        Result := False;
    end;
end;

procedure TDataSetFieldMapping.Append(CallPost : boolean = True);
{{
Insere novo registro com os valores do mapeamento

Revision: 8/9/2005 - Roger
}
begin
    Self.Destination.Append;
    Self.Copy(CallPost);
end;

function TDataSetFieldMapping.CheckReferences : boolean;
{{
Varre lista de pares para saber se as references ao dataset e ao proprio campo ainda sao validas

Revision: 8/9/2005 - Roger
}
var
    i :   Integer;
    PFP : PDBFieldPair;
begin
    Result := True;
    for i := 0 to Self.Associations.Count - 1 do begin
        PFP := Self.Associations.Items[i];
        //Checar integridade deste par
        if not (Assigned(PFP.Field1.Dataset)) or not (Assigned(PFP.Field2.Dataset)) then begin
            Result := False;
            Exit;
        end;
    end;
end;

procedure TDataSetFieldMapping.ClearAssociations;
//----------------------------------------------------------------------------------------------------------------------------------
var
    i : Integer;
begin
    for i := 0 to Self.Associations.Count - 1 do begin
        Dispose(PDBFieldPair(Self.Associations.Items[i]));
    end;
    Self.Associations.Clear;
end;

procedure TDataSetFieldMapping.Copy(CallPost : boolean = True);
{{
Copia  valores dos mapeamentos dos campos

Revision: 8/9/2005 - Roger
}
var
    i :   Integer;
    PFP : PDBFieldPair;
begin
    Self.Destination.Edit;
    for i := 0 to Self.Associations.Count - 1 do begin
        PFP := Self.Associations.Items[i];
        if PFP^.Enabled then begin
            PFP^.Field2.Value := PFP^.Field1.Value;
        end;
    end;
    if CallPost then begin
        Self.Destination.Post;
    end;
end;

constructor TDataSetFieldMapping.Create(ASource, ADestination : TDataSet; Mapping : TStringList);
{{
ASource - Dataset de origem dos dados.

ADestination - Dataset de destino dos dados.

Mapping - Stringlist com os mapeamentos na forma campoOrigem = campoDestino.

Caso Mapping = nil ajusta-se apenas para os campos que existem em ambos DataSets lidos a partir da origem

Revision: 8/9/2005 - Roger
}
    procedure LSRGetField(var Fld : TField; DS : TDataSet; const FName : string; MustExists : boolean);
    {{
    Atribui a Fld a referencia ao campo do dataset DS( destino ) com o nome pedido.
    Caso MustExist e o campo não for encontrado EDatabaseError será gerada.

    Revision: 8/9/2005 - Roger
    }
    begin
        Fld := DS.FindField(FName);
        if (not Assigned(Fld)) and MustExists then begin
            raise EDatabaseError.CreateFmt('Campo "%s" não encontrado na tabela "%s"', [FName, DS.Name]);
        end;
    end;
    //..................................................................................................................................
var
    i :   Integer;
    Fld1, Fld2 : TField;
    PFP : PDBFieldPair;
begin
    if not (ASource.Active and ADestination.Active) then begin
        raise Exception.Create('Datasets devem estar abertos para efetuar o mapeamento.');
    end;
    inherited Create;
    Self.Source      := ASource;
    Self.Destination := ADestination;
    Self.Associations := TList.Create;
    try
        //Inicia campos internos e monta o mapeamento
        if Assigned(Mapping) then begin
            for i := 0 to Mapping.Count - 1 do begin
                LSRGetField(Fld1, ASource, Mapping.Names[i], True);
                LSRGetField(Fld2, ADestination, Mapping.Values[Mapping.Names[i]], True);
                New(PFP);
                PFP^.Field1  := Fld1;
                PFP^.Field2  := Fld2;
                PFP^.Enabled := True;
                Self.Associations.Add(PFP);
            end;
        end else begin
            for i := 0 to Self.Source.FieldCount - 1 do begin
                Fld1 := Source.Fields.Fields[i];
                LSRGetField(Fld2, Destination, Fld1.FieldName, False);
                if Assigned(Fld2) then begin
                    New(PFP);
                    PFP^.Field1  := Fld1;
                    PFP^.Field2  := Fld2;
                    PFP^.Enabled := True;
                    Self.Associations.Add(PFP);
                end;
            end;
        end;
    except
        Self.ClearAssociations;
        FreeAndNil(Self.Associations);
        raise;
    end;
end;

constructor TDataSetFieldMapping.Create(ASource, ADestination : TDataSet; Mapping : string);
{{
ASource - Dataset de origem dos dados.

ADestination - Dataset de destino dos dados.

Mapping - Stringlist com os mapeamentos na forma campoOrigem = campoDestino separados por ";".

Caso Mapping = "" ajusta-se apenas para os campos que existem em ambos DataSets lidos a partir da origem

Revision: 8/9/2005 - Roger
}
var
    Lines : TStringList;
begin
    Self.Associations := nil;
    if Mapping <> EmptyStr then begin
        Lines := TStringList.Create;
        try
            Lines.Text := StringReplace(Mapping, ';', #13, [rfReplaceAll]);
            Create(ASource, ADestination, Lines);
        finally
            Lines.Free;
        end;
    end else begin
        Create(ASource, ADestination, nil);
    end;
end;

class procedure TGDBHnd.AssignRecord(Source, Dest : TDataSet; ByName : boolean);
{{
Rotina importada da JEDI 3.0 que copia os valores de um dataset para outro, ou baseado no indice do campo o em seu nome.

O Dataset de destino deve estar no modo de edicao para permitir tal operacao.

Revision: 25/7/2005
}
var
    I : Integer;
    F, FSrc : TField;
begin
    if not (Dest.State in dsEditModes) then begin
        TGDBHnd.DBError(SNotEditing);
    end;
    if ByName then begin
        for I := 0 to Source.FieldCount - 1 do begin
            F := Dest.FindField(Source.Fields[I].FieldName);
            if (F <> nil) and (F.DataType <> ftAutoInc) then begin
                F.Value := Source.Fields[I].Value;
            end;
        end;
    end else begin
        for I := 0 to Min(Source.FieldDefs.Count - 1, Dest.FieldDefs.Count - 1) do begin
            F    := Dest.FindField(Dest.FieldDefs[I].Name);
            FSrc := Source.FindField(Source.FieldDefs[I].Name);
            if (F <> nil) and (FSrc <> nil) and (F.DataType <> ftAutoInc) then begin
                F.Value := FSrc.Value;
            end;
        end;
    end;
end;

class function TGDBHnd.DataSetLocateThrough(DataSet : TDataSet; const KeyFields : string; const KeyValues : variant;
    Options : TLocateOptions) : boolean;
{{
Rotina importada da JEDI 3.0 usada para localizar em um DataSet um registro especificado pelos valores de determinados registros.

Revision: 25/7/2005
}
var
    FieldCount : Integer;
    Fields :     TList;
    Bookmark :   TBookmarkStr;

    function LSRCompareField(Field : TField; Value : variant) : boolean;
    var
        S : string;
    begin
        if Field.DataType = ftString then begin
            if Value = Null then begin
                Result := Field.IsNull;
            end else begin
                S := Field.AsString;
                if loPartialKey in Options then begin
                    Delete(S, Length(Value) + 1, MaxInt);
                end;
                if loCaseInsensitive in Options then begin
                    Result := AnsiSameText(S, Value);
                end else begin
                    Result := AnsiSameStr(S, Value);
                end;
            end;
        end else begin
            Result := (Field.Value = Value);
        end;
    end;

    function LSRCompareRecord : boolean;
    var
        I : Integer;
    begin
        if FieldCount = 1 then begin
            Result := LSRCompareField(TField(Fields.First), KeyValues);
        end else begin
            Result := True;
            for I := 0 to FieldCount - 1 do begin
                Result := Result and LSRCompareField(TField(Fields[I]), KeyValues[I]);
            end;
        end;
    end;

begin
    Result := False;
    with DataSet do begin
        CheckBrowseMode;
        if Bof and EOF then begin
            Exit;
        end;
    end;
    Fields := TList.Create;
    try
        DataSet.GetFieldList(Fields, KeyFields);
        FieldCount := Fields.Count;
        Result     := LSRCompareRecord;
        if Result then begin
            Exit;
        end;
        DataSet.DisableControls;
        try
            Bookmark := DataSet.Bookmark;
            try
                with DataSet do begin
                    First;
                    while not EOF do begin
                        Result := LSRCompareRecord;
                        if Result then begin
                            Break;
                        end;
                        Next;
                    end;
                end;
            finally
                if not Result and DataSet.BookmarkValid(PChar(Bookmark)) then begin
                    DataSet.Bookmark := Bookmark;
                end;
            end;
        finally
            DataSet.EnableControls;
        end;
    finally
        Fields.Free;
    end;
end;

destructor TDataSetFieldMapping.Destroy;
    //----------------------------------------------------------------------------------------------------------------------------------
    //Limpa mapeamentos e desaloca recursos
begin
    if Assigned(Self.Associations) then begin
        Self.ClearAssociations;
        Self.Associations.Free;
    end;
    inherited;
end;

function TDataSetFieldMapping.GetCount : Integer;
    //----------------------------------------------------------------------------------------------------------------------------------
begin
    Result := Self.Associations.Count;
end;

function TDataSetFieldMapping.GetPair(Index : Integer) : TDBFieldPair;
{{
Retorna valor do par

Revision: 8/9/2005 - Roger
}
begin
    Result := PDBFieldPair(Self.Associations.Items[Index])^;
end;

class procedure TGDBHnd.RefreshTables(DataSet : TDataSet; Recursive : boolean);
//----------------------------------------------------------------------------------------------------------------------
var
    List : TList;
    i :    Integer;
begin
    DataSet.Refresh;
    List := TList.Create;
    try
        DataSet.GetDetailDataSets(List);
        for i := 0 to List.Count - 1 do begin
            if Recursive then begin
                RefreshTables(TDataSet(List[i]), True);
            end else begin
                TDataSet(List[i]).Refresh;
            end;
        end;
    finally
        List.Free;
    end;
end;

class procedure TGDBHnd.DBError(const Msg : string);
{{
Rotina importada da JEDI 3.0 que gera um erro do tipo EDatabaseError.

Revision: 25/7/2005
}
begin
    DatabaseError(Msg);
end;

end.
