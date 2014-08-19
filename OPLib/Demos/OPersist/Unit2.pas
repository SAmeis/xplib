unit Unit2;

interface

uses
   Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls;

type
  TForm2 = class(TForm)
    Memo1: TMemo;
    Button2: TButton;
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  THSComponent = class(TComponent)
  private
    ComponentProperty: Pointer;
    procedure   LoadComponent(Reader: TReader);
    procedure   StoreComponent(Writer: TWriter);
  protected
    procedure   GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function    GetChildOwner: TComponent; override;
    procedure   SetOwner( aComponent: TComponent );
  end;

  TCity = class;
  THouse = class;
  TRoom = class;

  TFurniture = class( THSComponent )
  private
    _FurnitureName: String;
  public
    Constructor Create( aRoom: TRoom ); reintroduce;
  published 
    Property    FurnitureName: String read _FurnitureName write _FurnitureName; 
  end; 

  TChair = class( TFurniture ) 
  private 
    _ChairType: String; 
  published 
    Property    ChairType: String read _ChairType write _ChairType; 
  end; 

  TRoom = class( THSComponent )
  private 
    _RoomName: String; 
    function    getFurniture( index: Integer ): TFurniture;
  public
    Constructor Create( aHouse: THouse ); reintroduce;
    Property    Furniture[index:integer]: TFurniture read getFurniture; default; 
  published 
    Property    RoomName: String read _RoomName write _RoomName; 
  end; 

  TRoomSpecial = class( TRoom ) 
  private 
    _Extra:     TTimer; 
    _Extra2:     TTimer; 
  protected 
  public
    Constructor Create( aHouse: THouse );
    Destructor  Destroy; override;
    procedure   DefineProperties(Filer: TFiler); override;
    Property    Extra: TTimer read _Extra write _Extra; 
    Property    Extra2: TTimer read _Extra2 write _Extra2; 
  end; 

  THouse = class( THSComponent ) 
  private
    _HouseName: String; 
    _Rooms:     TRoom;
    _Timers:    TTimer;
    function    getRoom( index: Integer ): TRoom;
  protected
    procedure   DefineProperties(Filer: TFiler); override;
  public
    Constructor Create( aCity: TCity ); reintroduce;
    Property    Room[index:integer]: TRoom read getRoom; default;
  published 
    Property    HouseName: String read _HouseName write _HouseName; 
  end; 

  TCity = Class( THSComponent ) 
  private 
    _CityName: String; 
    function getHouse( index: integer ): THouse; 
  public 
    Property House[index:integer]: THouse read getHouse; default; 
  published 
    Property CityName: String read _CityName write _CityName; 
  end; 


var
  Form2: TForm2;

implementation 

{$R *.DFM} 

uses
   ClassHnd;

{ THSComponent }

procedure THSComponent.SetOwner(aComponent: TComponent);
begin
  if aComponent.Owner<>nil then
    aComponent.Owner.RemoveComponent( aComponent );
  InsertComponent( aComponent );
end;

function THSComponent.GetChildOwner: TComponent;
begin 
  Result := self; 
end; 

procedure THSComponent.GetChildren(Proc: TGetChildProc; Root: TComponent); 
var 
  i : Integer; 
begin 
  inherited; 
  for i := 0 to ComponentCount-1 do 
    Proc( Components[i] ); 
end; 

procedure THSComponent.LoadComponent(Reader: TReader); 
begin 
  if Reader.ReadBoolean then begin 
    Reader.Owner := Nil; 
    TComponent(ComponentProperty^) := Reader.ReadComponent(nil); 
    Reader.Owner := self.Owner; 
  end; 
end; 

procedure THSComponent.StoreComponent(Writer: TWriter); 
begin 
  Writer.WriteBoolean(TComponent(ComponentProperty^)<>nil); 
  if TComponent(ComponentProperty^)<>nil then 
    Writer.WriteComponent( TComponent(ComponentProperty^) ); 
end; 

{ TCity } 

function TCity.getHouse(index: integer): THouse; 
begin 
  Result := THouse( Components[index] ); 
end; 

{ THouse } 

constructor THouse.Create(aCity: TCity); 
begin 
  inherited Create( aCity ); 
end; 

procedure THouse.DefineProperties(Filer: TFiler); 
begin
  inherited;
  ComponentProperty := @_Rooms;
  Filer.DefineProperty('Rooms', LoadComponent, StoreComponent, True);
  ComponentProperty := @_Timers;
  Filer.DefineProperty('Timers', LoadComponent, StoreComponent, True);
end;

function THouse.getRoom(index: Integer): TRoom;
begin
  Result := TRoom( Components[index] ); 
end; 

{ TRoom } 

constructor TRoom.Create(aHouse: THouse); 
begin 
  inherited Create( aHouse ); 
end; 

function TRoom.getFurniture(index: Integer): TFurniture; 
begin 
  Result := TFurniture( Components[index] ); 
end; 

{ TRoomSpecial } 

constructor TRoomSpecial.Create(aHouse: THouse); 
begin 
  inherited; 
  _Extra := TTimer.Create( nil ); 
  _Extra2 := TTimer.Create( nil ); 
end; 

procedure TRoomSpecial.DefineProperties(Filer: TFiler); 
begin 
  inherited; 
  ComponentProperty := @_Extra;
  Filer.DefineProperty('Extra', LoadComponent, StoreComponent, True); 
  ComponentProperty := @_Extra2; 
  Filer.DefineProperty('Extra2', LoadComponent, StoreComponent, True); 
end; 

destructor TRoomSpecial.Destroy; 
begin 
  _Extra.Free; 
  _Extra2.Free; 
  inherited; 
end; 

{ TFurniture } 

constructor TFurniture.Create(aRoom: TRoom); 
begin 
  inherited Create( aRoom ); 
end; 


procedure TForm2.Button2Click(Sender: TObject);
var
  City: TCity; 
  House: THouse; 
  RoomSpecial: TRoomSpecial; 
  Room: TRoom; 
  Furniture: TFurniture; 
  Chair: TChair; 
  fs : TFileStream; 
  x,y,z: Integer;
  fsText : TStringList;
begin
  City := TCity.Create( self );
  City.CityName := 'TestCity';

  House := THouse.Create( City ); 
  House.HouseName := 'HouseA'; 
    Room := TRoom.Create( House ); 
    Room.RoomName := 'Room1'; 
      Furniture := TFurniture.Create( Room ); 
      Furniture.FurnitureName := 'Furniture1'; 
      Furniture := TFurniture.Create( Room ); 
      Furniture.FurnitureName := 'Furniture2'; 
    Room := TRoom.Create( House ); 
    Room.RoomName := 'Room2';
      Furniture := TFurniture.Create( Room ); 
      Furniture.FurnitureName := 'Furniture3'; 
      Furniture := TFurniture.Create( Room ); 
      Furniture.FurnitureName := 'Furniture4'; 

  House := THouse.Create( City ); 
  House.HouseName := 'HouseB'; 
    RoomSpecial := TRoomSpecial.Create( House ); 
    RoomSpecial.RoomName := 'Room3'; 
    RoomSpecial.Extra.Interval := 2000; 
    RoomSpecial.Extra2.Interval := 4000; 
      Furniture := TFurniture.Create( Room ); 
      Furniture.FurnitureName := 'Furniture1'; 
      Furniture := TFurniture.Create( Room ); 
      Furniture.FurnitureName := 'Furniture2'; 
    Room := TRoom.Create( House ); 
    Room.RoomName := 'Room4'; 
      Chair := TChair.Create( Room ); 
      Chair.ChairType := 'Type1'; 
      Chair.FurnitureName := 'Chair1'; 
      Chair := TChair.Create( Room ); 
      Chair.ChairType := 'Type2';
      Chair.FurnitureName := 'Chair2'; 

  // Move from HouseB Room4 to HouseA Room2 
  City[0][1].SetOwner( Chair ); 

  fs := TFileStream.Create('c:\test.txt', fmCreate ); 
  fs.WriteComponent( City ); 
  fs.Free; 

  City.Free; 

  fs := TFileStream.Create('c:\test.txt', fmOpenRead ); 
  City := TCity( fs.ReadComponent( nil ) );
  fs.Free;

  //Texto direto
  fsText:=TStringList.Create;
  fsText.Text:=ComponentToString( City );
  fsText.SaveToFile( 'c:\text.txt' );
  fsText.Free;


  Memo1.Lines.Add( City.CityName ); 
  for x := 0 to City.ComponentCount-1 do begin 
    House := City[x]; 
    Memo1.Lines.Add( '  '+House.HouseName ); 
    for y := 0 to House.ComponentCount-1 do begin 
      Room := House[y]; 
      Memo1.Lines.Add( '    '+Room.RoomName );
      if Room.ClassType = TRoomSpecial then with Room as TRoomSpecial do begin 
        Memo1.Lines.Add( '    '+IntToStr(Extra.Interval) ); 
        Memo1.Lines.Add( '    '+IntToStr(Extra2.Interval) ); 
      end; 
      for z := 0 to Room.ComponentCount-1 do begin 
        Furniture := Room[z]; 
        Memo1.Lines.Add( '      '+Furniture.FurnitureName ); 
        if Furniture.ClassType = TChair then with Furniture as TChair do begin 
          Memo1.Lines.Add( '      '+ChairType ); 
        end; 
      end; 
    end; 
  end;
end;

initialization
begin
   RegisterClasses([TCity, THouse, TRoom, TRoomSpecial, TTimer, TFurniture, TChair]);
end;

end.


{
To solve your Q smth. like this:

  TTimers = class( THSComponent )
  private
    function    getTimer( index: Integer ): TTimer;
  public
    Constructor Create( aRooms: TRooms );
    Property    Timer[index:integer]: TTimer read getTimer; default;
  end;

  TRoom = class( THSComponent )
  private
    _RoomName: String;
  published
    Property    RoomName: String read _RoomName write _RoomName;
  end;

  TRooms = class( THSComponent )
  private
  protected
    function    getRoom( index: Integer ): TRoom;
  public
    Constructor Create( aHouse: THouse );
    Destructor  Destroy; override;
    Property    Room[index:integer]: TRoom read getRoom; default;
  end;

  THouse = class( THSComponent )
  private
    _HouseName: String;
    _Rooms:     TRooms;
    _Timers:    TTimers;
  protected
    procedure   DefineProperties(Filer: TFiler); override;
  public
    Constructor Create( aCity: TCity );
    property    Rooms: TRooms read _Rooms write _Rooms;
    property    Timers: TTimers read _Timers write _Timers;
  published
    Property    HouseName: String read _HouseName write _HouseName;
  end;

  TCity = Class( THSComponent ) 
  private
    _CityName: String;
    function getHouse( index: integer ): THouse;
  public
    Property House[index:integer]: THouse read getHouse; default;
  published
    Property CityName: String read _CityName write _CityName;
  end;

procedure THouse.DefineProperties(Filer: TFiler);
begin
  inherited;
  ComponentProperty := @_Rooms;
  Filer.DefineProperty('Rooms', LoadComponent, StoreComponent, True);
  ComponentProperty := @_Timers;
  Filer.DefineProperty('Timers', LoadComponent, StoreComponent, True);
end;
}
