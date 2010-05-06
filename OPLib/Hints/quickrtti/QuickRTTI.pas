unit QuickRTTI;

interface

uses
   Classes, Typinfo, Sysutils, LowX;

type
{
This file is released under an MIT style license as detailed at opensource.org.
Just don't preteend you wrote it, and leave this comment in the text, and
I'll be happy.  Consider it my resume :)

April 17,2000 Michael Johnson
 father@bigattichouse.com
 www.bigattichouse.com

Quick RTTI gives you simple string based access to RTTI data in any
RTTI capable component.  WIth the addition of my lowx and strutils you
can also read and write basic XML structures based off of your component.
SUPER easy.

Just set RTTIObject:= some TPersistent descendant (any TComponent) and you will have
nice string based access to all its fields by Name... cool huh.

I am testing an "Object Shell" that will allow using QuickRTTI and
Toadbase to use/store RTTI Objects thru memory mapped files, ie..
web based persistence.. I know, why not use CORBA..

 hell, in the
words of Disney via Mr W.T.Pooh "...I wouldn't climb this tree / if a bear
flew like a bee / but I wouldn't be a bear then / so I guess I wouldn't
care then..."

Perhaps having the simple ability to convert TComponents to and from XML
will open up a new world for delphi... now to work on RPC !..
}
   TQuickRTTI = class (TPersistent)
   private
       FObj : TPersistent;
       FVal : string;
       PList : PPropList;
       Props : TStringList;
   protected
       function OutputXML : string;
       procedure InputXML ( sXML : string);
       procedure SetValue ( FieldName, Value : string);
       function GetValue (FieldName : string):string;
       procedure SetObject ( ObjInst : TPersistent );
   public
       function PropertyCount : integer;
       function IndexOf( Name : string) : Integer;
       function PropertyNames(Index : integer) : string;
       function PropertyVarTypes( Index : integer ) : string;
       property Value[ FieldName : string ] : string read GetValue write SetValue;
   published
       property RTTIObject : TPersistent read FObj write SetObject;
       property XML : string read OutputXML write InputXML;
   end;


implementation

procedure TQuickRTTI.SetObject( ObjInst : TPersistent );
//----------------------------------------------------------------------------------------------------------------------
var
   PTI : PTypeInfo;
   List : TPropList;
   TInfo : TPropInfo;
   i : integer;
   vin : variant;
begin
   if not(Assigned(Props)) then begin
       Props:=TStringList.Create;
   end;
   Props.Clear;
   FObj:=ObjInst;
   PTI:=ObjInst.ClassInfo ;
   PList:=@List;
   i:= GetPropList(PTI,[ tkClass,tkInteger, tkChar, tkEnumeration, tkFloat, tkstring, tkSet, tkClass, tkMethod, tkWChar,
                         tkLstring, tkWstring, tkVariant, tkArray, tkRecord, tkInterface, tkInt64],
                   PList);
   {getting the list... but I'm pretty much trying to ignore
   method calls for this version}
   for i:= 0 to 255 do
   if Assigned(Plist[i]) then begin
      TInfo:= Plist[i]^;
      vin:=GetPropValue( FObj, TInfo.Name, True);
      Props.AddObject(Uppercase(TInfo.Name), Pointer(PList[i]) );
   end;
end;

function TQuickRTTI.PropertyCount : integer;
//----------------------------------------------------------------------------------------------------------------------
begin
   Result:=-1;
   if Assigned(Props) then begin
       Result:=Props.Count;
   end;
end;


function TQuickRTTI.PropertyNames( Index : integer ) : string;
//----------------------------------------------------------------------------------------------------------------------
begin
   Result:=EmptyStr;
   if Assigned(Props) then begin
       Result:=Props[Index];
   end;
end;

function TQuickRTTI.IndexOf( Name : string ) : Integer;
//----------------------------------------------------------------------------------------------------------------------
begin
   if Assigned(Props) then begin
       Result:=Props.IndexOf (Uppercase(Name));
   end;
end;

function TQuickRTTI.PropertyVarTypes( Index : integer ) : string;
//----------------------------------------------------------------------------------------------------------------------
var
   TInfo : TPropInfo;
begin
   Result:=EmptyStr;
   if Assigned(Props) then begin
       TInfo:=TPropInfo( Pointer(Props.Objects[Index])^ );
       Result:=TInfo.PropType^.Name;
   end;
end;

procedure TQuickRTTI.SetValue( FieldName, Value : string );
//----------------------------------------------------------------------------------------------------------------------
var
   vin : Variant;
   FName : ShortString;
begin
   FName:=FieldName;
   Vin:=Value;
   SetPropValue( FObj, FName, Vin );
end;

function TQuickRTTI.GetValue( FieldName : string ) : string;
//----------------------------------------------------------------------------------------------------------------------
var
   v, vin : Variant;
   FName : ShortString;
begin
   FName:=FieldName;
   vin:=GetPropValue( FObj, FName, True );
   VarCast( v, vin, varstring);
   Result:=vin;
end;

function TQuickRTTI.OutputXML : string;
//----------------------------------------------------------------------------------------------------------------------
var
   i, j  : integer;
   IntObj : TQuickRTTI;
   Parcial : TStringList;
   PProp : ^TPersistent;
   SubProp : TPersistent;
begin
   IntObj:=TQuickRTTI.Create;
   Parcial:=TStringList.Create;
   try
       Result:= '<' + FObj.ClassName + '>'#13#10;
       for i:= 0 to Props.Count-1 do begin
           PProp:=Self.RTTIObject.FieldAddress( Props[i] );
           if PProp <> nil then begin //Sub-Instancia
               SubProp:=PProp^;
               if SubProp is TPersistent then begin
                   IntObj.RTTIObject:=SubProp;
                   Parcial.Text:=IntObj.XML;
                   for j:=0 to Parcial.Count-1 do begin
                       Parcial.Strings[ j ]:= #9 + Parcial.Strings[ j ];
                   end;
                   Result:=Result + Parcial.Text;
               end;
           end else begin
               Result:=Result + ' <' + Props[i] + '>' + GetValue(PropertyNames(i)) + '</' + Props[i] + '>'#13#10;
           end;
       end;
       Result:=Result+'</'+FObj.ClassName+'>'#13#10;
   finally
       IntObj.Free;
       Parcial.Free;
   end;
end;

procedure TQuickRTTI.InputXML( sXML : string );
//----------------------------------------------------------------------------------------------------------------------
var
   ThisClass, ThisObj : string;
   i : integer;
begin
   {OK.. I could get a whole list of Objects.. but since I'm delaing with em one at a time..}
   ThisClass:= FObj.ClassName;
   ThisObj:=GetTagString( sXML, ThisClass );
   {Now let RTTI discover itself in the XML!}
   for i:= 0 to Props.Count-1 do begin
       SetValue(Props[i],GetTagData(thisobj,Props[i]));
   end;
end;

end.
