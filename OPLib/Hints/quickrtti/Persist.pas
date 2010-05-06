unit Persist;

interface

uses
   Classes, Typinfo, Sysutils, LowX;



{

type
   TTextPersistent = class (TPersistent)
   private
       FObj : TPersistent;
       FVal : string;
       PList : PPropList;
       Props : TStringList;
   protected
       function OutputDFMText : string;
       procedure InputDFMText ( StrDFMText : string);
       procedure SetValue ( FieldName, Value : string);
       function GetValue (FieldName : string):string;
       procedure SetObject ( ObjInst : TPersistent );
   public
       property Value[ FieldName : string ] : string read GetValue write SetValue;
       function CreateNew : TPersistent;
       function IndexOf( Name : string) : Integer;
       function PropertyCount : integer;
       function PropertyNames(Index : integer) : string;
       function PropertyVarTypes( Index : integer ) : string;
   published
       property ParsedObject : TPersistent read FParsedObject write SetParsedObject; //Instancia a ser usada como entrada
       property DFMText : string read OutputDFMText write InputDFMText;
   end;
}

function SaveComponentToText( IComp : TComponent ) : string;
function LoadComponentFromText( StrDFM : string; Parent : TComponent ) : TComponent;

implementation



function LoadPropertyTable( ObjInst : TPersistent; Props : TStringList ) : integer;
//----------------------------------------------------------------------------------------------------------------------
var
   PTI : PTypeInfo;
   List : TPropList;
   TInfo : TPropInfo;
   PList : PPropList;
   ManyProps, i : integer;
   vin : variant;
begin
   if not(Assigned(Props)) then begin
       Props:=TStringList.Create;
   end;
   Props.Clear;
   PTI:=ObjInst.ClassInfo ;
   PList:=@List;
   ManyProps:= GetPropList(PTI,[ tkClass,tkInteger, tkChar, tkEnumeration, tkFloat, tkstring, tkSet, tkClass, tkMethod,
                                 tkWChar, tkLstring, tkWstring, tkVariant, tkArray, tkRecord, tkInterface, tkInt64],
                           PList);
   {getting the list... but I'm pretty much trying to ignore
   method calls for this version}
   for i:= 0 to ManyProps do
   if Assigned(Plist[i]) then begin
      TInfo:= Plist[i]^;
      //vin:=GetPropValue( FObj, TInfo.Name, True);
      Props.AddObject(Uppercase(TInfo.Name), Pointer(PList[i]) );
   end;
end;

function SaveComponentToText( IComp : TComponent ) : string;
//----------------------------------------------------------------------------------------------------------------------
var
   SubProps : TStringList;
begin
   //Le as informacoes da instancia
   SubProps:=TStringList.Create;
   LoadPropertyTable( IComp, SubProps );
end;


function LoadComponentFromText( StrDFM : string; Parent : TComponent ) : TComponent;
begin

end;


end.
 