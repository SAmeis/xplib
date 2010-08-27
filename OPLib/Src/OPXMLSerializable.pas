unit OPXMLSerializable;

interface

uses
	Classes, SysUtils, TypInfo, XMLIntf;

type
	TXMLSerializable = class(TPersistent)
	private
		procedure SaveProp(PInfo : PPropInfo; Node : IXMLNode);
		procedure SaveSubInstance(PInfo : PPropInfo; ParentNode : IXMLNode );
		function GetPropType(PInfo : PPropInfo) : PTypeInfo;
		function ValPropInteger(PInfo : PPropInfo) : string;
		function ValPropString(PInfo : PPropInfo) : string;
		function ValPropEnum(PInfo : PPropInfo) : string;
		function ValPropFloat(PInfo : PPropInfo) : string;
		function ValPropSet(PInfo : PPropInfo) : string;
		function ValPropInt64(PInfo : PPropInfo) : string;
	public
		procedure SaveTo( Node : IXMLNode );
		procedure LoadFrom( Node : IXMLNode; CreateDefault : Boolean );
	end;


implementation

{ TXLMSerializable }

function TXMLSerializable.GetPropType(PInfo: PPropInfo): PTypeInfo;
begin
	 Result := PInfo.PropType^;
end;

procedure TXMLSerializable.LoadFrom(Node: IXMLNode; CreateDefault: Boolean);
begin

end;

procedure TXMLSerializable.SaveTo(Node: IXMLNode);
var
	propList : PPropList;
	x, propCount : Integer;
	Filter : TTypeKinds;
	propInfo : PPropInfo;
begin
   //Ajusta nome da classe registrada para a recuperação
	Node.Attributes['class']:=Self.ClassName;

	//Fitra atributos pertinentes a serialização
	Filter := [ {tkUnknown,} tkInteger, tkChar, tkEnumeration, tkFloat,
	tkString, tkSet, tkClass, {tkMethod,} tkWChar, tkLString, tkWString,
	tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray,
	tkUString, tkClassRef {, tkPointer, tkProcedure} ];

	//Carrega os atributos para a lista
	propCount:= GetPropList(Self.ClassInfo, Filter, nil);
	GetMem(propList, propCount * SizeOf(Pointer));
	GetPropList(Self.ClassInfo, Filter, propList);

	for x := 0 to propCount - 1 do begin
		Self.SaveProp( propList^[x], Node );
	end;
end;

function TXMLSerializable.ValPropEnum(PInfo: PPropInfo): string;
var
	 OrdVal : integer;
begin
	 OrdVal := GetOrdProp(Self, PInfo);
	 if OrdVal <> PInfo^.Default then begin
		 Result := GetEnumName(GetPropType(PInfo), OrdVal);
	 end;
end;

function TXMLSerializable.ValPropFloat(PInfo: PPropInfo): string;
const
	 Precisions: array[TFloatType] of Integer = (7, 15, 18, 18, 19); //digitos
var
	 Val : Extended;
	 Prec : TFloatType;
begin
	 Val := GetFloatProp(Self, PInfo);
	 Prec := GetTypeData(GetPropType(PInfo)).FloatType;
	 Result := FloatToStrF(Val, ffGeneral, Precisions[Prec], 0);
end;

function TXMLSerializable.ValPropInt64(PInfo: PPropInfo): string;
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

function TXMLSerializable.ValPropInteger(PInfo: PPropInfo): string;
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

function TXMLSerializable.ValPropSet(PInfo: PPropInfo): string;
begin
	 Result := GetSetProp(Self, PInfo);
	 if Result <> EmptyStr then begin
		 Result := '[' + Result + ']';
	 end;
end;

function TXMLSerializable.ValPropString(PInfo: PPropInfo): string;
begin
	 Result := GetStrProp(Self, PInfo);
end;

procedure TXMLSerializable.SaveProp(PInfo : PPropInfo; Node : IXMLNode);
var
	SName, StrValue : string;
begin
	{TODO -oroger -cdsg : Pegar demais metodos de IOObj}
	 case PInfo.PropType^.Kind of
		 tkUnknown    : begin
		 	raise Exception.Create('Tipo desconhecido de atributo encontrado');
		 end;
		 tkInteger    : begin
			 StrValue := Self.ValPropInteger(PInfo);
		 end;
		 tkChar    : begin
			 StrValue := Self.ValPropString(PInfo);
		 end;
		 tkEnumeration    : begin
			 StrValue := Self.ValPropEnum(PInfo);
		 end;
		 tkFloat    : begin
			 StrValue := Self.ValPropFloat(PInfo);
		 end;
		 tkString    : begin
			 StrValue := Self.ValPropString(PInfo);
		 end;
		 tkSet    : begin
			 StrValue := Self.ValPropSet(PInfo);
		 end;
		 tkClass    : begin
			//StrValue := Self.ValPropClass(PInfo);
			 Self.SaveSubInstance( PInfo, Node );
		 end;
		 tkMethod    : begin

		 end;
		 tkWChar    : begin
			 StrValue := Self.ValPropString(PInfo);
		 end;
		 tkLString    : begin
			 StrValue := Self.ValPropString(PInfo);
		 end;
		 tkWString    : begin
			 StrValue := Self.ValPropString(PInfo);
		 end;
		 tkVariant    : begin
			 StrValue := Self.ValPropString(PInfo);
		 end;
		 tkArray    : begin

		 end;
		 tkRecord    : begin

		 end;
		 tkInterface    : begin

		 end;
		 tkInt64    : begin
			 StrValue := Self.ValPropInt64(PInfo);
		 end;
		 tkDynArray    : begin

		 end;
	 end;
	 if (StrValue <> EmptyStr) and (PInfo.PropType^.Kind <> tkClass) then begin
		 StrValue := PInfo.Name + ' = ' + StrValue;
	 end;
end;

procedure TXMLSerializable.SaveSubInstance(PInfo : PPropInfo; ParentNode : IXMLNode );
var
	 Instance : TObject;
	 subNode : IXMLNode;
begin
	 Instance := TypInfo.GetObjectProp(Self, PInfo);
	 if ( Instance <> nil )then begin
		 if (Instance is TXMLSerializable) then begin
			subNode:=ParentNode.ChildNodes.FindNode(PInfo.Name);
			if ( subNode = nil ) then begin
				subNode:=ParentNode.AddChild( PInfo.Name );
				subNode.Attributes['class' ]:=PInfo.PropType^.Name;
			end;
			TXMLSerializable( Instance ).SaveTo( subNode );
		 end else begin

		 end;
    end;
end;

end.
