{$IFDEF IOObj }
    {$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I OPLib.inc}

unit IOObj;

interface

uses
    Classes, TypInfo;

type
    TIOObj = class(TObject)
    private
        FList :      PPropList;
        FCount :     Integer;
        FSize :      Integer;
        SBuffer :    TStringList;
        RootObject : TObject;
        function Get(Index : Integer) : PPropInfo;
        procedure SetAsText(const Value : string);
        function GetPropType(PInfo : PPropInfo) : PTypeInfo;
        procedure EnumChildren(Child : TComponent);
    protected
        FObject :  TObject;
        CastName : string; //Qdo a classe eh na verdade uma descendente od tipo da propriedade
        function ChildrenComponentsAsText : string;
        function GetAsText : string;
        function SubPropAsText(PInfo : PPropInfo) : string;
        function ValPropClass(PInfo : PPropInfo) : string;
        function ValPropEnum(PInfo : PPropInfo) : string;
        function ValPropSet(PInfo : PPropInfo) : string;
        function ValPropFloat(PInfo : PPropInfo) : string;
        function ValPropInteger(PInfo : PPropInfo) : string;
        function ValPropInt64(PInfo : PPropInfo) : string;
        function ValPropString(PInfo : PPropInfo) : string;
    public
        constructor Create(AObject, ARootObject : TObject; Filter : TTypeKinds); virtual;
        destructor Destroy; override;
        function Contains(P : PPropInfo) : boolean;
        function Find(const AName : string) : PPropInfo;
        procedure Delete(Index : Integer);
        procedure Intersect(List : TIOObj);
        property AsText : string read GetAsText write SetAsText;
        property Count : Integer read FCount;
        property Items[Index : Integer] : PPropInfo read Get; default;
    end;


function ExportObjText(Obj : TPersistent) : string;
procedure ImportObjText(Obj : TPersistent; const Str : string);

type
    THackWriter = class(TWriter)
    protected
        procedure WriteProperties(Instance : TPersistent);
    end;

    THackReader = class(TReader);

    THackPersistent = class(TPersistent)
    protected
        //procedure DefineProperties(Filer: TFiler);
    end;

implementation

uses
    SysUtils, AnsiStrings, Controls;

const
    _ENDENT_ = #9; //#9;

type

    TComponentProtected = class(TComponent)
    public
        procedure GetChildrenProctected(Proc : TGetChildProc; Root : TComponent);
    end;



function ExportObjText(Obj : TPersistent) : string;
    ///  <summary>
    ///     Exporta objeto de forma simples para texto
    ///  </summary>
    ///  <remarks>
    ///    Rotina usada no passado para serialização, não se aplica mais agora
    /// Revision - 20101229 - Roger
    /// Ajustada para suporte unicode
    ///  </remarks>
    //----------------------------------------------------------------------------------------------------------------------
var
    W :      THackWriter;
    BinMS :  TMemoryStream;
    TextMS : TStringStream;
    S :      string;
    j, i :   Integer;
    SL, SubSL : TStringList;
begin
    BinMS := TMemoryStream.Create;
    W     := THackWriter.Create(BinMS, 4096);
    try
        W.WriteSignature;
        W.WritePrefix([], 0);
        //-- Como foi escrito um prefixo o duplo WritelistEnd no fim - comparar novamente a forma e ver q este modo deve ser chamado para TComponent. Testar usando um TPersitent puro
        W.WriteStr(ansistring(Obj.ClassName));
        if Obj is TComponent then begin
            W.WriteStr(ansistring(TComponent(Obj).Name));
        end else begin
            W.WriteStr(ansistring(Obj.ClassName));
        end;
        W.WriteProperties(Obj);
        W.WriteListEnd;
        W.WriteListEnd;
        W.FlushBuffer;
        TextMS := TStringStream.Create(s);
        try
            BinMS.Seek(0, soFromBeginning);
            ObjectBinaryToText(BinMS, TextMS);
            TextMS.Seek(0, soFromBeginning);
            if Obj is TComponent then begin
                SL := TStringList.Create;
                try
                    SL.Text := TextMS.DataString;
                    SL.Delete(SL.Count - 1); //Remove end final
                    SubSL := TStringList.Create;
                    try
                        for i := 0 to TComponent(Obj).ComponentCount - 1 do begin
                            SubSL.Text := ExportObjText(TComponent(Obj).Components[i]);
                            for j := 0 to SubSL.Count - 1 do begin
                                SubSL.Strings[j] := '  ' + SubSL.Strings[j];
                            end;
                            SL.AddStrings(SubSL);
                        end;
                    finally
                        SubSL.Free;
                    end;
                    SL.Add('end'); //Reisere o end removido
                    Result := SL.Text;
                finally
                    SL.Free;
                end;
            end else begin
                Result := TextMS.DataString;
            end;
        finally
            TextMS.Free;
        end;
    finally
        W.Free;
    end;
end;

procedure ImportObjText(Obj : TPersistent; const Str : string);
//----------------------------------------------------------------------------------------------------------------------
var
    R :      THackReader;
    BinMS :  TMemoryStream;
    TextMS : TStringStream;
    ChildPos : Integer;
    Flags :  TFilerFlags;
begin
    { TODO -oRoger : Rotina para setar as properiedaddes atraver de um TReader }
    BinMS := TMemoryStream.Create;
    try
        TextMS := TStringStream.Create(Str);
        try
            ObjectTextToBinary(TextMS, BinMS);
        finally
            TextMS.Free;
        end;
        BinMS.Seek(0, soFromBeginning);
        R := THackReader.Create(BinMS, 4096);
        try
            if Obj is TComponent then begin
                R.Root := TComponent(Obj);
                R.ReadSignature;           //ffInline nos flags????
                R.BeginReferences;
                try
                    R.ReadComponent(TComponent(Obj));
                finally
                    R.EndReferences;
                end;
            end else begin
                R.ReadSignature;
                R.ReadPrefix(Flags, ChildPos);
                R.ReadStr; //ClassName
                //R.ReadStr; Nao ler Component Name
                while not R.EndOfList do begin
                    R.ReadProperty(Obj);
                end;
            end;
        finally
            R.Free;
        end;
    finally
        BinMS.Free;
    end;
end;


constructor TIOObj.Create(AObject, ARootObject : TObject; Filter : TTypeKinds);
    //----------------------------------------------------------------------------------------------------------------------
begin
    inherited Create;
    Self.RootObject := ARootObject;
    if AObject <> nil then begin
        Self.FObject := AObject;
        FCount := GetPropList(AObject.ClassInfo, Filter, nil);
        FSize  := FCount * SizeOf(Pointer);
        GetMem(FList, FSize);
        GetPropList(AObject.ClassInfo, Filter, FList);
    end else begin
        FCount := 0;
        FList  := nil;
    end;
end;

destructor TIOObj.Destroy;
    //----------------------------------------------------------------------------------------------------------------------
begin
    if FList <> nil then begin
        FreeMem(FList, FSize); //Alocado com GetMem em GetPropList
    end;
end;

function TIOObj.Contains(P : PPropInfo) : boolean;
    //----------------------------------------------------------------------------------------------------------------------
    ///  <summary>
    ///    Retorna true para o caso do objeto possuir uma propriedade de tipo e nome igual a informada
    ///  </summary>
    /// Revision - 20110103 - Roger
    /// para suporte a unicode inserida unit especifica
var
    I : Integer;
begin
    for I := 0 to FCount - 1 do begin
        if (Self.FList^[i]^.PropType = P^.PropType) and (AnsiStrings.CompareText(Self.FList^[i]^.Name, P^.Name) = 0) then begin
            Result := True;
            Exit;
        end;
    end;
    Result := False;
end;

function TIOObj.Find(const AName : string) : PPropInfo;
    //----------------------------------------------------------------------------------------------------------------------
var
    I : Integer;
begin
    for I := 0 to FCount - 1 do begin
        with FList^[I]^ do begin
            if (CompareText(Name, AName) = 0) then begin
                Result := FList^[I];
                Exit;
            end;
        end;
    end;
    Result := nil;
end;

procedure TIOObj.Delete(Index : Integer);
//----------------------------------------------------------------------------------------------------------------------
begin
    Dec(FCount);
    if Index < FCount then begin
        Move(FList^[Index + 1], FList^[Index], (FCount - Index) * SizeOf(Pointer));
    end;
end;

function TIOObj.Get(Index : Integer) : PPropInfo;
    //----------------------------------------------------------------------------------------------------------------------
begin
    Result := FList^[Index];
end;

procedure TIOObj.Intersect(List : TIOObj);
//----------------------------------------------------------------------------------------------------------------------
var
    I : Integer;
begin
    for I := FCount - 1 downto 0 do begin
        if not List.Contains(FList^[I]) then begin
            Delete(I);
        end;
    end;
end;


function TIOObj.GetAsText : string;
    //----------------------------------------------------------------------------------------------------------------------
var
    SubSL, SL : TStringList;
    j, i :      Integer;
    PInfo :     PPropInfo;
    SubPropText, PropClassName : string;
begin
    if not Assigned(Self.FObject) then begin
        Exit;
    end;
    SL := TStringList.Create;
    try
        if Self.CastName <> EmptyStr then begin
            PropClassName := Self.CastName;
        end else begin
            PropClassName := FObject.ClassName;
        end;
        if Self.FObject is TComponent then begin
            SL.Add('object ' + TComponent(FObject).Name + ': ' + PropClassName);
        end else begin
            SubPropText := FObject.ClassName;
            SL.Add('object ' + FObject.ClassName + ': ' + PropClassName);
        end;
        //Add internal Fields/properties
        SubSL := TStringList.Create;
        try
            for i := 0 to Self.Count - 1 do begin
                PInfo := Self.Items[i];
                SubPropText := Self.SubPropAsText(PInfo);
                if (SubPropText <> EmptyStr) then begin
                    SubSL.Text := SubPropText;
                    for j := 0 to SubSL.Count - 1 do begin
                        SubSL.Strings[j] := _ENDENT_ + SubSL.Strings[j];
                    end;
                    SL.AddStrings(SubSL);
                    SubPropText := EmptyStr;
                end;
            end;
            //Add contained components
            if Self.FObject is TComponent then begin
                SubPropText := Self.ChildrenComponentsAsText;
                if (SubPropText <> EmptyStr) then begin
                    SubSL.Text := SubPropText;
                    for j := 0 to SubSL.Count - 1 do begin
                        SubSL.Strings[j] := _ENDENT_ + SubSL.Strings[j];
                    end;
                    SL.AddStrings(SubSL);
                    SubPropText := EmptyStr;
                end;
            end;
        finally
            SubSL.Free;
        end;

        SL.Add('end');
        Result := SL.Text;
    finally
        SL.Free;
    end;
end;

procedure TIOObj.SetAsText(const Value : string);
//----------------------------------------------------------------------------------------------------------------------
begin

end;

function TIOObj.ValPropInteger(PInfo : PPropInfo) : string;
    //----------------------------------------------------------------------------------------------------------------------
var
    Val : longint; //maior tipo
begin
    try
        Val := GetOrdProp(Self.FObject, PInfo);
        if Val <> 0 then begin
            Result := IntToStr(Val);
        end;
    except
        Result := EmptyStr;
    end;
end;

function TIOObj.ValPropString(PInfo : PPropInfo) : string;
    //----------------------------------------------------------------------------------------------------------------------
begin
    Result := GetStrProp(Self.FObject, PInfo);
end;

function TIOObj.SubPropAsText(PInfo : PPropInfo) : string;
    //----------------------------------------------------------------------------------------------------------------------
begin
    case PInfo.PropType^.Kind of
        tkUnknown : begin

        end;
        tkInteger : begin
            Result := Self.ValPropInteger(PInfo);
        end;
        tkChar : begin
            Result := Self.ValPropString(PInfo);
        end;
        tkEnumeration : begin
            Result := Self.ValPropEnum(PInfo);
        end;
        tkFloat : begin
            Result := Self.ValPropFloat(PInfo);
        end;
        tkString : begin
            Result := Self.ValPropString(PInfo);
        end;
        tkSet : begin
            Result := Self.ValPropSet(PInfo);
        end;
        tkClass : begin
            Result := Self.ValPropClass(PInfo);
         {
            SubPropInstance:=TIOObj.Create( GetObjectProp( Self.FObject, PInfo ), Self.RootObject, tkAny );
            try
                SL:=TStringList.Create;
                try
                    SL.Text:=SubPropInstance.AsText;
                    for i:=1 to SL.Count-1 do begin //Nao endenta 1a linha, que recebe endentacao no chamador
                        SL.Strings[i]:=_ENDENT_ + SL.Strings[i];
                    end;
                    Result:=SL.Text;
                    //System.Delete( Result, Length( Result ) - 2, 2 ); //Remove a linha em branco do final;
                finally
                    SL.Free;
                end;
            finally
                SubPropInstance.Free;
            end;
            Exit; //Nao agregar o PropName=Value do final
         }
        end;
        tkMethod : begin

        end;
        tkWChar : begin
            Result := Self.ValPropString(PInfo);
        end;
        tkLString : begin
            Result := Self.ValPropString(PInfo);
        end;
        tkWString : begin
            Result := Self.ValPropString(PInfo);
        end;
        tkVariant : begin
            Result := Self.ValPropString(PInfo);
        end;
        tkArray : begin

        end;
        tkRecord : begin

        end;
        tkInterface : begin

        end;
        tkInt64 : begin
            Result := Self.ValPropInt64(PInfo);
        end;
        tkDynArray : begin

        end;
    end;
    if (Result <> EmptyStr) and (PInfo.PropType^.Kind <> tkClass) then begin
        Result := PInfo.Name + ' = ' + Result;
    end;
end;

function TIOObj.ValPropInt64(PInfo : PPropInfo) : string;
    //----------------------------------------------------------------------------------------------------------------------
var
    Val : int64; //maior tipo
begin
    try
        Val := GetInt64Prop(Self.FObject, PInfo);
        if Val <> PInfo.Default then begin
            Result := IntToStr(Val);
        end;
    except
        Result := EmptyStr;
    end;
end;

function TIOObj.ValPropEnum(PInfo : PPropInfo) : string;
    //----------------------------------------------------------------------------------------------------------------------
var
    OrdVal : Integer;
begin
    OrdVal := GetOrdProp(FObject, PInfo);
    if OrdVal <> PInfo^.Default then begin
        Result := GetEnumName(GetPropType(PInfo), OrdVal);
    end;
end;

function TIOObj.GetPropType(PInfo : PPropInfo) : PTypeInfo;
    //----------------------------------------------------------------------------------------------------------------------
begin
    Result := PInfo.PropType^;
end;

function TIOObj.ValPropFloat(PInfo : PPropInfo) : string;
    //----------------------------------------------------------------------------------------------------------------------
const
    Precisions: array[TFloatType] of Integer = (7, 15, 18, 18, 19); //digitos
var
    Val :  Extended;
    Prec : TFloatType;
begin
    Val    := GetFloatProp(FObject, PInfo);
    Prec   := GetTypeData(GetPropType(PInfo)).FloatType;
    Result := FloatToStrF(Val, ffGeneral, Precisions[Prec], 0);
end;

function TIOObj.ValPropSet(PInfo : PPropInfo) : string;
    //----------------------------------------------------------------------------------------------------------------------
begin
    Result := GetSetProp(Self.FObject, PInfo);
    if Result <> EmptyStr then begin
        Result := '[' + Result + ']';
    end;
end;

function TIOObj.ChildrenComponentsAsText : string;
    //----------------------------------------------------------------------------------------------------------------------
var
    lixo : string;
    RootOwner, OLevel : TComponent;
begin
    { TODO -oRoger -cDSG : Enumerar sub-componentes e verificar a remocao da linha adcional para eles tb }
    if not Assigned(Self.SBuffer) then begin
        SBuffer := TStringList.Create;
    end;
    try
        //Na enumeracao faz uso de SBuffer
        if Self.FObject is TComponent then begin
            OLevel := TComponent(Self.FObject);
            if Self.RootObject <> nil then begin
                RootOwner := TComponent(Self.RootObject).Owner;
            end else begin
                RootOwner := nil;
            end;
            while (OLevel <> nil) and (OLevel <> RootOwner) do begin //Ate nao ter dono ou componente raiz
                Lixo := OLevel.ClassName;
                TComponentProtected(Self.FObject).GetChildrenProctected(Self.EnumChildren, OLevel);
                OLevel := TComponent(OLevel).Owner;
            end;
        end;
        Result := SBuffer.Text;
    finally
        if Assigned(SBuffer) then begin
            FreeAndNil(SBuffer);
        end;
    end;
end;

procedure TIOObj.EnumChildren(Child : TComponent);
//----------------------------------------------------------------------------------------------------------------------
var
    OWrapper : TIOObj;
begin
    { TODO -oRoger -cDSG : Usar  SBuffer para manter os valores }
    OWrapper := TIOObj.Create(Child, Self.RootObject, tkAny {=[Low(TTypeKind)..High(TTypeKind)]}); //Todos os tipos ?
    try
        Self.SBuffer.Text := Self.SBuffer.Text + OWrapper.AsText;
    finally
        OWrapper.Free;
    end;
end;

{ TComponentProtected }

procedure TComponentProtected.GetChildrenProctected(Proc : TGetChildProc; Root : TComponent);
//----------------------------------------------------------------------------------------------------------------------
begin
    Self.GetChildren(Proc, Root);
end;

function TIOObj.ValPropClass(PInfo : PPropInfo) : string;
    //----------------------------------------------------------------------------------------------------------------------
var
    Writer : TWriter;
    BinMS :  TMemoryStream;
    TextMS : TStringStream;
    s :      string;
    Instance : TObject;
    SubObj : TIOObj;
    W :      THackWriter;
begin
    Instance := TypInfo.GetObjectProp(Self.FObject, PInfo);
    if Instance <> nil then begin
        if (Instance is TComponent) and ((Self.RootObject is TComponent) or (Self.RootObject = nil)) then begin
            BinMS  := TMemoryStream.Create;
            Writer := TWriter.Create(BinMS, 4096);
            try
                Writer.WriteDescendent(TComponent(Instance), TComponent(Self.RootObject));
            finally
                Writer.Free;
            end;
            TextMS := TStringStream.Create(s);
            try
                BinMS.Seek(0, soFromBeginning);
                ObjectBinaryToText(BinMS, TextMS);
                TextMS.Seek(0, soFromBeginning);
                Result := TextMS.DataString;
            finally
                TextMS.Free;
            end;
            {
             DefinProperty e DefineBinaryProperty sao usados pra ler valore a aparti de TReader e TWriter
             Apenas componentes sao anihandos e propriedades apenas as descendentes de TPersistent
            }
        end else begin
            //As propriedades sao enumeradas normalmente
            SubObj := TIOObj.Create(Instance, Self, tkAny);
            SubObj.CastName := PInfo.PropType^.Name; //Corrige tipo esperado pela prop sobre o instanciado
            try
                if SubObj.CastName = 'TStrings' then begin
                    BinMS := TMemoryStream.Create;
                    W     := THackWriter.Create(BinMS, 4096);
                    try
                        W.WriteSignature;
                        W.WritePrefix([], 0);
                        //-- Como foi escrito um prefixo o duplo WritelistEnd no fim - comparar novamente a forma e ver q este modo deve ser chamado para TComponent. Testar usando um TPersitent puro
                        W.WriteStr(SubObj.CastName);
                        W.WriteStr(PInfo^.Name);
                        W.WriteProperty(TPersistent(Self.FObject), PInfo);
                        W.WriteListEnd;
                        W.WriteListEnd;
                        W.FlushBuffer;
                        //Writer.WriteDescendent( TComponent(Instance), TComponent( Self.RootObject ) );
                    finally
                        //Writer.Free;
                    end;
                    TextMS := TStringStream.Create(s);
                    try
                        BinMS.Seek(0, soFromBeginning);
                        ObjectBinaryToText(BinMS, TextMS);
                        TextMS.Seek(0, soFromBeginning);
                        Result := TextMS.DataString;
                        //Result:=string(BinMS.Memory);
                    finally
                        TextMS.Free;
                    end;
                end else begin
                    Result := SubObj.AsText;
                end;
            finally
                SubObj.Free;
            end;
        end;
    end;
end;

{ THackWriter }

procedure THackWriter.WriteProperties(Instance : TPersistent);
//----------------------------------------------------------------------------------------------------------------------
var
    I, Count : Integer;
    PropInfo : PPropInfo;
    PropList : PPropList;
begin
    Count := GetTypeData(Instance.ClassInfo)^.PropCount;
    if Count > 0 then begin
        GetMem(PropList, Count * SizeOf(Pointer));
        try
            GetPropInfos(Instance.ClassInfo, PropList);
            for I := 0 to Count - 1 do begin
                PropInfo := PropList^[I];
                if (PropInfo = nil) then begin
                    break;
                end;
                if IsStoredProp(Instance, PropInfo) and (PropInfo^.PropType^.Kind <> tkMethod) then begin
                    WriteProperty(Instance, PropInfo);
                end;
            end;
        finally
            FreeMem(PropList, Count * SizeOf(Pointer));
        end;
    end;
    THackPersistent(Instance).DefineProperties(Self);
end;

{ THackPersistent }

{
procedure THackPersistent.DefineProperties(Filer: TFiler);
begin
   //Nada;
end;
}

end.
