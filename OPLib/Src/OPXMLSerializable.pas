unit OPXMLSerializable;

interface

uses
    Classes, SysUtils, TypInfo, XMLIntf, Contnrs;


type
    IXMLSerializable = interface(IUnknown)
        ['{ADBDEA7E-AE56-437A-ADBB-0759C738501A}']
        procedure SaveTo(Node : IXMLNode);
        procedure LoadFrom(Node : IXMLNode; CreateDefault : boolean);
    end;

    {TODO -oroger -clib : implementar classe para a lista  }
    TXMLSerializableList = class(TObjectList, IXMLSerializable)
    private
        FRefCount : Integer;
    protected
        function QueryInterface(const IID : TGUID; out Obj) : HResult; stdcall;
        function _AddRef : Integer; stdcall;
        function _Release : Integer; stdcall;
    public
        procedure SaveTo(Node : IXMLNode);
        procedure LoadFrom(Node : IXMLNode; CreateDefault : boolean);
    end;

    TXMLSerializable = class(TPersistent, IXMLSerializable)
    private
        FRefCount : Integer;
        procedure SaveAttribute(PInfo : PPropInfo; Node : IXMLNode);
        procedure SaveSubInstance(PInfo : PPropInfo; ParentNode : IXMLNode);
        function GetPropType(PInfo : PPropInfo) : PTypeInfo;
        function ValPropInteger(PInfo : PPropInfo) : string;
        function ValPropString(PInfo : PPropInfo) : string;
        function ValPropEnum(PInfo : PPropInfo) : string;
        function ValPropFloat(PInfo : PPropInfo) : string;
        function ValPropSet(PInfo : PPropInfo) : string;
        function ValPropInt64(PInfo : PPropInfo) : string;
    protected
        function QueryInterface(const IID : TGUID; out Obj) : HResult; stdcall;
        function _AddRef : Integer; stdcall;
        function _Release : Integer; stdcall;
    public
        procedure SaveTo(Node : IXMLNode);
        procedure LoadFrom(Node : IXMLNode; CreateDefault : boolean);
    end;


implementation

uses
    Windows;

{ TXLMSerializable }

function TXMLSerializable.GetPropType(PInfo : PPropInfo) : PTypeInfo;
begin
    Result := PInfo.PropType^;
end;

procedure TXMLSerializable.LoadFrom(Node : IXMLNode; CreateDefault : boolean);
begin

end;

function TXMLSerializable.QueryInterface(const IID : TGUID; out Obj) : HResult;
begin
    if GetInterface(iid, Obj) then begin
        Result := S_OK;
    end else begin
        Result := E_NOINTERFACE;
    end;
end;

procedure TXMLSerializable.SaveTo(Node : IXMLNode);
var
    propList : PPropList;
    x, propCount : Integer;
    Filter :   TTypeKinds;
    propInfo : PPropInfo;
begin
    //Ajusta nome da classe registrada para a recuperação
    Node.Attributes['class'] := Self.ClassName;

    //Fitra atributos pertinentes a serialização. Remove atributos não suportados
    Filter := [ {tkUnknown,} tkInteger, tkChar, tkEnumeration, tkFloat,
        tkString, tkSet, tkClass, {tkMethod,} tkWChar, tkLString, tkWString,
        tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
        {, tkPointer, tkProcedure}];
    {$IFDEF UNICODE}
	Filter := Filter + [ tkClassRef, tkUString ];  //adiciona tipo especifico para Delphgi 2010+
	{$ENDIF}

    //Carrega os atributos para a lista
    propCount := GetPropList(Self.ClassInfo, Filter, nil);
    GetMem(propList, propCount * SizeOf(Pointer));
    GetPropList(Self.ClassInfo, Filter, propList);

    for x := 0 to propCount - 1 do begin
        Self.SaveAttribute(propList^[x], Node);
    end;
end;

function TXMLSerializable.ValPropEnum(PInfo : PPropInfo) : string;
var
    OrdVal : Integer;
begin
    OrdVal := GetOrdProp(Self, PInfo);
    if OrdVal <> PInfo^.Default then begin
        Result := GetEnumName(GetPropType(PInfo), OrdVal);
    end;
end;

function TXMLSerializable.ValPropFloat(PInfo : PPropInfo) : string;
const
    Precisions: array[TFloatType] of Integer = (7, 15, 18, 18, 19); //digitos
var
    Val :  Extended;
    Prec : TFloatType;
begin
    Val    := GetFloatProp(Self, PInfo);
    Prec   := GetTypeData(GetPropType(PInfo)).FloatType;
    Result := FloatToStrF(Val, ffGeneral, Precisions[Prec], 0);
end;

function TXMLSerializable.ValPropInt64(PInfo : PPropInfo) : string;
var
    Val : int64; //maior tipo
begin
    try
        Val := GetInt64Prop(Self, PInfo);
        if Val <> PInfo.Default then begin
            Result := IntToStr(Val);
        end;
    except
        Result := EmptyStr;
    end;
end;

function TXMLSerializable.ValPropInteger(PInfo : PPropInfo) : string;
var
    Val : longint; //maior tipo
begin
    try
        Val := GetOrdProp(Self, PInfo);
        if Val <> 0 then begin
            Result := IntToStr(Val);
        end;
    except
        Result := EmptyStr;
    end;
end;

function TXMLSerializable.ValPropSet(PInfo : PPropInfo) : string;
begin
    Result := GetSetProp(Self, PInfo);
    if Result <> EmptyStr then begin
        Result := '[' + Result + ']';
    end;
end;

function TXMLSerializable.ValPropString(PInfo : PPropInfo) : string;
begin
    Result := GetStrProp(Self, PInfo);
end;

function TXMLSerializable._AddRef : Integer;
begin
    Result := InterlockedIncrement(Self.FRefCount);
end;

function TXMLSerializable._Release : Integer;
begin
    Result := InterlockedDecrement(FRefCount);
    if Result = 0 then begin
        Destroy;
    end;
end;

procedure TXMLSerializable.SaveAttribute(PInfo : PPropInfo; Node : IXMLNode);
var
    SName, StrValue : string;
begin
    {TODO -oroger -cdsg : Pegar demais metodos de IOObj}
    case PInfo.PropType^.Kind of
        tkUnknown : begin
            raise Exception.Create('Tipo desconhecido de atributo encontrado');
        end;
        tkInteger : begin
            StrValue := Self.ValPropInteger(PInfo);
        end;
        tkChar : begin
            StrValue := Self.ValPropString(PInfo);
        end;
        tkEnumeration : begin
            StrValue := Self.ValPropEnum(PInfo);
        end;
        tkFloat : begin
            StrValue := Self.ValPropFloat(PInfo);
        end;
        tkString : begin
            StrValue := Self.ValPropString(PInfo);
        end;
        {$IFDEF UNICODE}
		 tkUString    : begin
			 StrValue := Self.ValPropString(PInfo);
		 end;
        {$ENDIF}
        tkSet : begin
            StrValue := Self.ValPropSet(PInfo);
        end;
        tkClass : begin
            //StrValue := Self.ValPropClass(PInfo);
            Self.SaveSubInstance(PInfo, Node);
        end;
        tkMethod : begin

        end;
        tkWChar : begin
            StrValue := Self.ValPropString(PInfo);
        end;
        tkLString : begin
            StrValue := Self.ValPropString(PInfo);
        end;
        tkWString : begin
            StrValue := Self.ValPropString(PInfo);
        end;
        tkVariant : begin
            StrValue := Self.ValPropString(PInfo);
        end;
        tkArray : begin

        end;
        tkRecord : begin

        end;
        tkInterface : begin

        end;
        tkInt64 : begin
            StrValue := Self.ValPropInt64(PInfo);
        end;
        tkDynArray : begin

        end;
    end;
    if (StrValue <> EmptyStr) and (PInfo.PropType^.Kind <> tkClass) then begin
		 //StrValue := PInfo.Name + ' = ' + StrValue;
		 Node.Attributes[ PInfo.Name ] := StrValue;
    end;
end;

procedure TXMLSerializable.SaveSubInstance(PInfo : PPropInfo; ParentNode : IXMLNode);
var
    Instance : TObject;
    subNode :  IXMLNode;
    subProp :  IXMLSerializable;
begin
    Instance := TypInfo.GetObjectProp(Self, PInfo);
    if (Instance <> nil) then begin
        if SysUtils.Supports(Instance, IXMLSerializable, subProp) then begin
            subNode := ParentNode.ChildNodes.FindNode(PInfo.Name);
            if (subNode = nil) then begin
                subNode := ParentNode.AddChild(PInfo.Name);
                subNode.Attributes['class'] := PInfo.PropType^.Name;
            end;
            subProp.SaveTo(subNode);
        end else begin
            {TODO -oroger -cdsglib : Como proceder}
        end;
    end;
end;

{ TXMLSerializableList }

procedure TXMLSerializableList.LoadFrom(Node : IXMLNode; CreateDefault : boolean);
begin

end;

function TXMLSerializableList.QueryInterface(const IID : TGUID; out Obj) : HResult;
begin
    if GetInterface(iid, Obj) then begin
        Result := S_OK;
    end else begin
        Result := E_NOINTERFACE;
    end;
end;

procedure TXMLSerializableList.SaveTo(Node : IXMLNode);
var
    sn, itemNode : IXMLNode;
    x : Integer;
    subInstance : TObject;
begin
    Node.Attributes['class'] := Self.ClassName;
    sn := Node.FindNamespaceDecl('Items');
    if not Assigned(sn) then begin
        sn := Node.AddChild('Items');
    end;
    sn.AttributeNodes.Clear;
    for x := 0 to Self.Count - 1 do begin
        subInstance := Self.Items[x];
        if (subInstance is TXMLSerializable) then begin
            //itemNode := sn.AttributeNodes.Nodes
        end else begin

        end;
    end;
end;

function TXMLSerializableList._AddRef : Integer;
begin
    Result := InterlockedIncrement(Self.FRefCount);
end;

function TXMLSerializableList._Release : Integer;
begin
    Result := InterlockedDecrement(Self.FRefCount);
    if Result < 0 then begin
        Destroy;
    end;
end;

end.
