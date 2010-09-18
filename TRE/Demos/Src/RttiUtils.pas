{$IFDEF RttiUtils}
	 {$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I DemoTRELib.inc}

{
reference
http://robstechcorner.blogspot.com/2009/09/so-what-is-rtti-rtti-is-acronym-for-run.html
}


unit RttiUtils;
// MIT License

// Copyright (c) 2009 - Robert Love

 // Permission is hereby granted, free of charge, to any person obtaining a copy
 // of this software and associated documentation files (the "Software"), to deal
 // in the Software without restriction, including without limitation the rights
 // to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 // copies of the Software, and to permit persons to whom the Software is
 // furnished to do so, subject to the following conditions:

 // The above copyright notice and this permission notice shall be included in
 // all copies or substantial portions of the Software.

 // THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 // IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 // FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 // AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 // LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 // OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 // THE SOFTWARE


interface

uses
    Generics.Collections,
    SysUtils,
    Classes,
    Rtti,
    TypInfo;

type
    ERttiMemberHelperException = class(Exception);
    // Make things a bit easier.
    TRttiMemberHelper = class helper for TRttiMember
    private
        function GetType : TRttiType;
    published
    public
        function GetValue(Instance : Pointer) : TValue; overload;
        function GetValue(const Instance : TValue) : TValue; overload;
        procedure SetValue(Instance : Pointer; const AValue : TValue); overload;
        procedure SetValue(const Instance : TValue; const AValue : TValue); overload;
        property MemberType : TRttiType read GetType;
    end;

    TCustomAttributeClass = class of TCustomAttribute;

    TAttrUtils = class(TObject)
    public
        class function HasAttribute(aType : pTypeinfo; aClass : TCustomAttributeClass; var Attr : TCustomAttribute) : boolean; overload;
        class function HasAttribute(aContext : TRttiContext; aType : TRttiObject; aClass : TCustomAttributeClass;
            var Attr : TCustomAttribute) : boolean; overload;
        class function HasAttributes(aType : pTypeinfo; aClass : TCustomAttributeClass;
            var Attrs : TArray<TCustomAttribute>) : boolean; overload;
        class function HasAttributes(aContext : TRttiContext; aType : TRttiObject; aClass : TCustomAttributeClass;
            var Attrs : TArray<TCustomAttribute>) : boolean; overload;

        class function GetAttribute(aType : pTypeinfo; aClass : TCustomAttributeClass) : TCustomAttribute; overload;
        class function GetAttribute(aContext : TRttiContext; aType : TRttiObject; aClass : TCustomAttributeClass) : TCustomAttribute;
            overload;

        class function GetAttributes(aType : pTypeinfo; aClass : TCustomAttributeClass) : TArray<TCustomAttribute>; overload;
        class function GetAttributes(aContext : TRttiContext; aType : TRttiObject;
            aClass : TCustomAttributeClass) : TArray<TCustomAttribute>; overload;
    end;

    TRttiEnumerator = class(TEnumerator<TValue>) // Adapater allowing a common interface
    protected
        FContext :      TRttiContext;
        FCurrentValue : TRttiProperty;
        FMoveNext :     TRttiMethod;
        FInstance :     TObject;
        function DoGetCurrent : TValue; override;
        function DoMoveNext : boolean; override;
    public
        constructor Create(aEnumerator : TObject; aContext : TRttiContext; aCurrent : TRttiProperty; aMoveNext : TRttiMethod);
    end;

    TArrayEnumerator = class(TEnumerator<TValue>)// Adapater allowing a common interface
    protected
        FArray : TValue;
        FIndex : Integer;
        function DoGetCurrent : TValue; override;
        function DoMoveNext : boolean; override;
    public
        constructor Create(aArray : TValue);
    end;

    EEnumerableFactoryException = class(Exception);
    // Factory to create the correct adapter
    TEnumerableFactory = class(TEnumerable<TValue>)
    protected
        FValue : TValue;
        function CreateRttiEnum(aValue : TValue) : TRttiEnumerator;
        function DoGetEnumerator : TEnumerator<TValue>; override;
    public
        constructor Create(aValue : TValue);
        class function IsTypeSupported(aType : TRttiType) : boolean;
    end;

    EElementAddException = class(Exception);
    ERttiElementAddException = class(EElementAddException);
    EArrayElementAddException = class(EElementAddException);


    TElementAdd = class abstract(TObject) // Adapater base class
    protected
        FList : TValue;
    public
        procedure Add(aAddElement : TValue); virtual; abstract;
        procedure AddFinalize; virtual;  // Can't depend on the data to be added to FList until this called.
        property List : TValue read FList;
        constructor Create(aList : TValue); virtual;
        class function TypeSupported(aListType : pTypeInfo) : boolean; virtual; abstract;
        class function GetAddType(aListType : pTypeInfo) : pTypeInfo; virtual; abstract;
    end;

    TRttiElementAdd = class(TElementAdd) // Adapater
    protected
        FContext :   TRttiContext;
        FAddMethod : TRttiMethod;
    public
        procedure Add(aAddElement : TValue); override;
        constructor Create(aList : TValue); override;
        class function TypeSupported(aListType : pTypeInfo) : boolean; override;
        class function GetAddType(aListType : pTypeInfo) : pTypeInfo; override;
    end;

    TArrayElementAdd = class(TElementAdd) // Adapater
    protected
        FTempList : TList<TValue>;
    public
        procedure Add(aAddElement : TValue); override;
        procedure AddFinalize; override;
        constructor Create(aList : TValue); override;
        destructor Destroy; override;
        class function TypeSupported(aListType : pTypeInfo) : boolean; override;
        class function GetAddType(aListType : pTypeInfo) : pTypeInfo; override;
    end;

    TElementAddFactory = class(TObject)
        class function CreateElementAdd(Value : TValue) : TElementAdd;
        class function TypeSupported(Value : pTypeInfo) : boolean;
        class function GetAddType(aListType : pTypeInfo) : pTypeInfo;
    end;



implementation

{ TRttiMemberHelper }

function TRttiMemberHelper.GetType : TRttiType;
begin
    Assert(Assigned(Self)); // For those who forget to check first.
    if Self is TRttiProperty then    begin
        Result := TRttiProperty(Self).PropertyType;
    end else
    if Self is TRttiField then    begin
        Result := TRttiField(Self).FieldType;
    end else //if Self is TRttiMethod then
             //     result := TRttiMethod(self).  hmmm Don't know how to get to the  TRttiMethodType and I don't need it
    begin
        Result := nil;
    end;
end;

function TRttiMemberHelper.GetValue(Instance : Pointer) : TValue;
begin
    Assert(Assigned(Self)); // For those who forget to check first.
    if InheritsFrom(TRttiProperty) then    begin
        Result := TRttiProperty(Self).GetValue(Instance);
    end else
    if InheritsFrom(TRttiField) then    begin
        Result := TRttiField(Self).GetValue(Instance);
    end else begin
        raise ERttiMemberHelperException.CreateFmt('Expecting Property or Field, found: %s', [ClassName]);
    end;

end;

procedure TRttiMemberHelper.SetValue(Instance : Pointer; const AValue : TValue);
begin
    Assert(Assigned(Self)); // For those who forget to check first.
    if InheritsFrom(TRttiProperty) then    begin
        TRttiProperty(Self).SetValue(Instance, aValue);
    end else
    if InheritsFrom(TRttiField) then    begin
        TRttiField(Self).SetValue(Instance, aValue);
    end else begin
        raise ERttiMemberHelperException.Create('Expecting Property or Field');
    end;
end;

function TRttiMemberHelper.GetValue(const Instance : TValue) : TValue;
begin
    if Instance.isObject then  begin
        Result := GetValue(Instance.AsObject);
    end else begin
        Result := GetValue(Instance.GetReferenceToRawData);
    end;
end;

procedure TRttiMemberHelper.SetValue(const Instance : TValue; const AValue : TValue);
begin
    if Instance.isObject then  begin
        SetValue(Instance.AsObject, AValue);
    end else begin
        SetValue(Instance.GetReferenceToRawData, aValue);
    end;
end;

class function TAttrUtils.GetAttribute(aType : pTypeinfo;
    aClass : TCustomAttributeClass) : TCustomAttribute;
var
    c : TRttiContext;
begin
    c := TRttiContext.Create;
    try
        Result := GetAttribute(c, c.GetType(aType), aClass);
    finally
        c.Free;
    end;
end;

class function TAttrUtils.GetAttribute(aContext : TRttiContext; aType : TRttiObject;
    aClass : TCustomAttributeClass) : TCustomAttribute;
var
    lAttr : TCustomAttribute;
begin
    Assert(Assigned(aType));
    for lAttr in aType.GetAttributes do begin
        if lAttr is aClass then    begin
            exit(lAttr);
        end;
    end;
    Result := nil;
end;

class function TAttrUtils.GetAttributes(aContext : TRttiContext;
    aType : TRttiObject; aClass : TCustomAttributeClass) : TArray<TCustomAttribute>;
var
    Attrs :   TArray<TCustomAttribute>;
    lp, idx : Integer;
begin
    Assert(Assigned(aType));
    Attrs := aType.GetAttributes;
    SetLength(Result, Length(Attrs));
    idx := 0;
    for lp := 0 to Length(Attrs) - 1 do begin
        if Attrs[lp] is aClass then    begin
            Result[idx] := Attrs[lp];
            Inc(idx);
        end;
    end;
    SetLength(Result, idx);
end;

class function TAttrUtils.GetAttributes(aType : pTypeinfo;
    aClass : TCustomAttributeClass) : TArray<TCustomAttribute>;
var
    c : TRttiContext;
begin
    c := TRttiContext.Create;
    try
        Result := GetAttributes(c, c.GetType(aType), aClass);
    finally
        c.Free;
    end;
end;

class function TAttrUtils.HasAttribute(aType : pTypeinfo;
    aClass : TCustomAttributeClass; var Attr : TCustomAttribute) : boolean;
var
    c : TRttiContext;
begin
    c := TRttiContext.Create;
    try
        Result := HasAttribute(c, c.GetType(aType), aClass, Attr);
    finally
        c.Free;
    end;
end;

class function TAttrUtils.HasAttribute(aContext : TRttiContext; aType : TRttiObject;
    aClass : TCustomAttributeClass; var Attr : TCustomAttribute) : boolean;
begin
    Attr   := GetAttribute(aContext, aType, aClass);
    Result := Assigned(Attr);
end;

class function TAttrUtils.HasAttributes(aContext : TRttiContext;
    aType : TRttiObject; aClass : TCustomAttributeClass;
    var Attrs : TArray<TCustomAttribute>) : boolean;
begin
    Attrs  := GetAttributes(aContext, aType, aClass);
    Result := Length(Attrs) > 0;
end;

class function TAttrUtils.HasAttributes(aType : pTypeinfo;
    aClass : TCustomAttributeClass; var Attrs : TArray<TCustomAttribute>) : boolean;
var
    c : TRttiContext;
begin
    c := TRttiContext.Create;
    try
        Result := HasAttributes(c, c.GetType(aType), aClass, Attrs);
    finally
        c.Free;
    end;
end;

{ TRttiEnumerator }

constructor TRttiEnumerator.Create(aEnumerator : TObject; aContext : TRttiContext; aCurrent : TRttiProperty; aMoveNext : TRttiMethod);
begin
    FCurrentValue := aCurrent;
    FMoveNext     := aMoveNext;
    FInstance     := aEnumerator;
    // Only need to keep a reference around to keep Method's from being freed.
    FContext      := aContext;
end;

function TRttiEnumerator.DoGetCurrent : TValue;
begin
    Result := FCurrentValue.GetValue(FInstance);
end;

function TRttiEnumerator.DoMoveNext : boolean;
begin
    Result := FMoveNext.Invoke(FInstance, []).AsBoolean;
end;

{ TArrayEnumerator }

constructor TArrayEnumerator.Create(aArray : TValue);
begin
    FArray := aArray;
    FIndex := -1;
end;

function TArrayEnumerator.DoGetCurrent : TValue;
begin
    Result := FArray.GetArrayElement(FIndex);
end;

function TArrayEnumerator.DoMoveNext : boolean;
begin
    Inc(FIndex);
    Result := (FIndex > FArray.GetArrayLength);
end;

{ TEnumerableFactory }

constructor TEnumerableFactory.Create(aValue : TValue);
begin
    FValue := aValue;
end;

function TEnumerableFactory.CreateRttiEnum(aValue : TValue) : TRttiEnumerator;
var
    lContext :    TRttiContext;
    lGetEnum :    TRttiMethod;
    lEnumerator : TValue;
    lMoveNext :   TRttiMethod;
    lCurrent :    TRttiProperty;
begin
    lContext := TRttiContext.Create;
    lGetEnum := lContext.GetType(aValue.TypeInfo).GetMethod('GetEnumerator');
    if not Assigned(lGetEnum) then    begin
        raise EEnumerableFactoryException.CreateFmt('No Enumerator Adapter avalable for Value Specified: %s', [FValue.TypeInfo.Name]);
    end;
    lEnumerator := lGetEnum.Invoke(aValue, []);

    lMoveNext := lContext.GetType(lEnumerator.TypeInfo).GetMethod('MoveNext');
    lCurrent  := lContext.GetType(lEnumerator.TypeInfo).GetProperty('Current');

    if not Assigned(lMoveNext) then    begin
        raise EEnumerableFactoryException.CreateFmt('GetEnumerator did not return a method named MoveNext', [FValue.TypeInfo.Name]);
    end;

    if not Assigned(lCurrent) then    begin
        raise EEnumerableFactoryException.CreateFmt('GetEnumerator did not return a property named Current', [FValue.TypeInfo.Name]);
    end;

    Result := TRttiEnumerator.Create(lEnumerator.AsObject, lContext, lCurrent, lMoveNext);
end;

function TEnumerableFactory.DoGetEnumerator : TEnumerator<TValue>;
begin
    if FValue.IsEmpty then    begin
        raise EEnumerableFactoryException.Create('Value Specified is Empty, DoGetEnumerator requires an assigned TValue');
    end;

    if FValue.IsArray then  begin
        Result := TArrayEnumerator.Create(FValue);
    end else
    if FValue.IsObject then    begin
        Result := CreateRttiEnum(FValue);
    end else begin
        raise EEnumerableFactoryException.CreateFmt('No Enumerator Adapter avalable for Value Type Specified: %s', [FValue.TypeInfo.Name]);
    end;
end;

class function TEnumerableFactory.IsTypeSupported(aType : TRttiType) : boolean;
var
    lContext :    TRttiContext;
    lGetEnum :    TRttiMethod;
    lEnumerator : TValue;
begin
    // Dynamic Arrays Supported
    Result := (aType.TypeKind = tkDynArray);

    // if TObject then check for Enumerator
    if aType.IsInstance then begin
        Result   := True;
        lContext := TRttiContext.Create;
        lGetEnum := aType.GetMethod('GetEnumerator');
        if not Assigned(lGetEnum) then    begin
            exit(False);
        end;
        if not Assigned(lGetEnum.ReturnType) then    begin
            exit(False);
        end;
        if not Assigned(lGetEnum.ReturnType.GetMethod('MoveNext')) then    begin
            exit(False);
        end;
        if not Assigned(lGetEnum.ReturnType.GetProperty('Current')) then    begin
            exit(False);
        end;
    end;
end;

{ TElementAdd }

procedure TElementAdd.AddFinalize;
begin
    // Do Nothing by default
end;

constructor TElementAdd.Create(aList : TValue);
begin
    FList := aList;
    if aList.IsEmpty then    begin
        raise EElementAddException.Create('Empty TValue passed to TElementAdd.Create');
    end;
end;

{ TRttiElementAdd }

procedure TRttiElementAdd.Add(aAddElement : TValue);
begin
    FAddMethod.Invoke(FList, [aAddElement]);
end;

constructor TRttiElementAdd.Create(aList : TValue);
begin
    inherited;
    FContext   := TRttiContext.Create;
    FAddMethod := FContext.GetType(aList.TypeInfo).GetMethod('Add');
    if not Assigned(FAddMethod) then    begin
        raise ERttiElementAddException.Create('Expected Add Method not found');
    end;
    if Length(FAddMethod.GetParameters) <> 1 then    begin
        raise ERttiElementAddException.Create('Add Method with only one Parameter expected');
    end;
end;

class function TRttiElementAdd.GetAddType(aListType : pTypeInfo) : pTypeInfo;
var
    lContext :   TRttiContext;
    lAddMethod : TRttiMethod;
begin
    lContext   := TRttiContext.Create;
    lAddMethod := lContext.GetType(aListType).GetMethod('Add');
    if not Assigned(lAddMethod) then    begin
        raise ERttiElementAddException.Create('Expected Add Method not found');
    end;
    if Length(lAddMethod.GetParameters) <> 1 then    begin
        raise ERttiElementAddException.Create('Add Method with only one Parameter expected');
    end;
    Result := lAddMethod.GetParameters[0].ParamType.Handle;
end;

class function TRttiElementAdd.TypeSupported(aListType : pTypeInfo) : boolean;
var
    lContext :   TRttiContext;
    lAddMethod : TRttiMethod;
begin
    if aListType.Kind <> tkClass then    begin
        exit(False);
    end;
    lContext   := TRttiContext.Create;
    lAddMethod := lContext.GetType(aListType).GetMethod('Add');
    if not Assigned(lAddMethod) then    begin
        exit(False);
    end;
    if Length(lAddMethod.GetParameters) <> 1 then    begin
        exit(False);
    end;
    Result := True;
end;

{ TArrayElementAdd }

procedure TArrayElementAdd.Add(aAddElement : TValue);
begin
    FTempList.Add(aAddElement);
end;

procedure TArrayElementAdd.AddFinalize;
// Copy the FTempList to the End of the FList which will be an Array
var
    lNewArray : TValue;
    lNewArrayPtr : Pointer;
    Len : longint;
    I :   Integer;
    lExistingArrayLen : Integer;
begin
    // Create New array
    TValue.Make(nil, FList.TypeInfo, lNewArray);
    // Set it's size and have to resort to lower levels as we don't have something that will work with SetLength()
    lNewArrayPtr := lNewArray.GetReferenceToRawData;
    lExistingArrayLen := FList.GetArrayLength;
    Len := lExistingArrayLen + FTempList.Count;
    DynArraySetLength(lNewArrayPtr, FList.TypeInfo, 1, @Len);
    // Copy Existing Values to New Array
    for I := 0 to lExistingArrayLen - 1 do begin
        lNewArray.SetArrayElement(I, FList.GetArrayElement(I));
    end;
    // Copy Added Values to New Array
    for I := 0 to FTempList.Count - 1 do begin
        lNewArray.SetArrayElement(I + lExistingArrayLen, FTempList.Items[I]);
    end;
    // Finally Replace old Array with New Array
    FList := lNewArray;
end;

constructor TArrayElementAdd.Create(aList : TValue);
begin
    inherited;
    if not (aList.Kind = tkDynArray) then    begin
        raise EArrayElementAddException.Create('Expected an Dynamic array Type');
    end;
    FTempList := TList<TValue>.Create;
end;

destructor TArrayElementAdd.Destroy;
begin
    FTempList.Free;
    inherited;
end;

class function TArrayElementAdd.GetAddType(aListType : pTypeInfo) : pTypeInfo;
var
	 C : TRttiContext;
	 rt : TRttiType;
begin
	(*Rotina original
	 C := TRttiContext.Create;
	 Result := (C.GetType(aListType) as TRttiDynamicArrayType).ElementType.Handle;
	*)
	 C      := TRttiContext.Create;
	 //Originalmente havia cast que sempre dava erro
	 rt:=C.GetType(aListType);
	 if rt is TRttiDynamicArrayType then begin
	 	Result := (rt as TRttiDynamicArrayType).ElementType.Handle;
	 end else begin
		Result:=rt.Handle;
	 end;
end;

class function TArrayElementAdd.TypeSupported(aListType : pTypeInfo) : boolean;
begin
    Result := aListType.Kind = tkDynArray;
end;

{ TElementAddFactory }

class function TElementAddFactory.CreateElementAdd(Value : TValue) : TElementAdd;
begin
    if TArrayElementAdd.TypeSupported(Value.TypeInfo) then    begin
        Result := TArrayElementAdd.Create(Value);
    end else
    if TRttiElementAdd.TypeSupported(Value.TypeInfo) then    begin
        Result := TRttiElementAdd.Create(Value);
    end else begin
        raise EElementAddException.CreateFmt('Unsupported TValue type: %s', [Value.TypeInfo.Name]);
    end;
end;


class function TElementAddFactory.GetAddType(aListType : pTypeInfo) : pTypeInfo;
begin
    if TArrayElementAdd.TypeSupported(aListType) then    begin
        Result := TArrayElementAdd.GetAddType(aListType);
    end else
    if TRttiElementAdd.TypeSupported(aListType) then    begin
        Result := TRttiElementAdd.GetAddType(aListType);
    end else begin
        Result := nil;
    end;
end;

class function TElementAddFactory.TypeSupported(Value : pTypeInfo) : boolean;
begin
    Result := TRttiElementAdd.TypeSupported(Value) or TArrayElementAdd.TypeSupported(Value);
end;

end.
