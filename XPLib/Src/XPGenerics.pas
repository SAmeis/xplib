{$IFDEF XPGenerics}
	 {$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I XPLib.inc}

//Baseado na solução encontrada em
//http://stackoverflow.com/questions/924659/how-can-i-cast-an-object-to-a-generic

unit XPGenerics;

interface

type
	TDynTypeCast = class
	public
		//ReinterpretCast does a hard type cast
		class function ReinterpretCast<ReturnT>(const Value): ReturnT;
		//StaticCast does a hard type cast but requires an input type
		class function StaticCast<T, ReturnT>(const Value: T): ReturnT;
		//DynamicCast is like the as-operator. It checks if the object can be typecasted
		class function DynamicCast<T, ReturnT>(const Value: T): ReturnT;
	end;

implementation

uses
	Classes, SysUtils, System.Rtti, System.TypInfo;


class function TDynTypeCast.ReinterpretCast<ReturnT>(const Value): ReturnT;
begin
	Result := ReturnT(Value);
end;

class function TDynTypeCast.StaticCast<T, ReturnT>(const Value: T): ReturnT;
begin
	Result := ReinterpretCast<ReturnT>(Value);
end;

class function TDynTypeCast.DynamicCast<T, ReturnT>(const Value: T): ReturnT;
var
	TypeT, TypeReturnT          : PTypeInfo;
	Obj                         : TObject;
	LClass                      : TClass;
	ClassNameReturnT, ClassNameT: string;
	FoundReturnT, FoundT        : Boolean;
begin
	TypeT       := TypeInfo(T);
	TypeReturnT := TypeInfo(ReturnT);
	if (TypeT = nil) or (TypeReturnT = nil) then
		raise Exception.Create('Missing Typeinformation');
	if TypeT.Kind <> tkClass then
		raise Exception.Create('Source type is not a class');
	if TypeReturnT.Kind <> tkClass then
		raise Exception.Create('Destination type is not a class');

	{$WARN UNSAFE_CODE OFF} {$WARN UNSAFE_TYPE OFF}
	Obj := TObject(Pointer(@Value)^);
	{$WARN UNSAFE_CODE ON} {$WARN UNSAFE_TYPE ON}
	if Obj = nil then
		Result := default (ReturnT)
	else begin
		ClassNameReturnT := UTF8ToString(TypeReturnT.Name);
		ClassNameT       := UTF8ToString(TypeT.Name);
		LClass           := Obj.ClassType;
		FoundReturnT     := False;
		FoundT           := False;
		while (LClass <> nil) and not(FoundT and FoundReturnT) do begin
			if not FoundReturnT and (LClass.ClassName = ClassNameReturnT) then
				FoundReturnT := True;
			if not FoundT and (LClass.ClassName = ClassNameT) then
				FoundT := True;
			LClass     := LClass.ClassParent;
		end;
		//if LClass <> nil then << TObject doesn't work with this line
		if FoundT and FoundReturnT then
			Result := ReinterpretCast<ReturnT>(Obj)
		else if not FoundReturnT then
			raise Exception.CreateFmt('Cannot cast class %s to %s', [Obj.ClassName, ClassNameReturnT])
		else
			raise Exception.CreateFmt('Object (%s) is not of class %s', [Obj.ClassName, ClassNameT]);
	end;
end;

end.
