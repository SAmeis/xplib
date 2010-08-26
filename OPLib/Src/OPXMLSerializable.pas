unit OPXMLSerializable;

interface

uses
	Classes, SysUtils, TypInfo, XMLIntf;

type
	TXLMSerializable = class
	private
		procedure SaveProp(PInfo : PPropInfo; Node : IXMLNode);
	public
		procedure SaveTo( Node : IXMLNode );
		procedure LoadFrom( Node : IXMLNode; CreateDefault : Boolean );
	end;


implementation

{ TXLMSerializable }

procedure TXLMSerializable.LoadFrom(Node: IXMLNode; CreateDefault: Boolean);
begin

end;

procedure TXLMSerializable.SaveTo(Node: IXMLNode);
var
	propList : PPropList;
	x, propCount : Integer;
	Filter : TTypeKinds;
	propName : string;
	propInfo : PPropInfo;
begin
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


procedure TXLMSerializable.SaveProp(PInfo : PPropInfo; Node : IXMLNode);
var
	StrValue : string;
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
			 StrValue := Self.ValPropClass(PInfo);
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

end.
