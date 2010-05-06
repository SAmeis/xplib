unit XMLMenu;
{
  The following procedure allows you to build a menu from an XML file.
  Special feature: You only need to specify the Name of the procedure which then
  will be attached to a OnClick handler.
  Note that the procedure must be declared as public.
}

{
  Mit folgender Prozedur kann man aus einem XML-File ein Menu
  erstellen lassen (einfach im OnCreate aufrufen).
  Besonderes Feature: Im XML-File gebt ihr nur den Namen der Prozedur an, 
  die dem OnClick-Ereignis zugewiesen werden soll. 
  Die einzige Einschränkung besteht darin, dass diese Prozedur 
  published sein muss. 
  Bindet einfach diese Prozedur in euer Hauptformular ein: 
} 


procedure TMainForm.CreateMenuFromXMLFile; 

  function Get_Int(S: string): Integer; 
  begin 
    Result := 0; 
    try 
      Result := StrToInt(S); 
    except 
    end; 
  end; 

  procedure AddRecursive(Parent: TMenuItem; Item: IXMLNode); 
  var 
    I: Integer; 
    Node: TMenuItem; 
    Child: IXMLNode; 
    Address: TMethod; 
  begin 
    Node := TMenuItem.Create(Parent); 
    if (Uppercase(Item.Attributes['CAPTION']) <> 'SEPERATOR') then 
    begin 
      Node.Caption := Item.Attributes['CAPTION']; 
      if (Uppercase(Item.Attributes['ID']) <> 'NONE') then 
      begin 
        Address.Code := MethodAddress(Item.Attributes['ID']); 
        Address.Data := Self; 
        if (Item.ChildNodes.Count - 1 < 0) then 
          Node.OnClick := TNotifyEvent(Address); 
      end; 
      if (Uppercase(Item.Attributes['SHORTCUT']) <> 'NONE') then 
        Node.ShortCut := TextToShortCut(Item.Attributes['SHORTCUT']); 
      Node.Checked := (Item.Attributes['CHECKED'] = '1'); 
    end 
    else 
      Node.Caption := '-'; 
    Node.Visible := (Item.Attributes['VISIBLE'] = '1'); 

    if Parent <> nil then 
      Parent.Add(Node) 
    else 
      MainMenu.Items.Add(Node); 

    for I := 0 to Item.ChildNodes.Count - 1 do 
    begin 
      Child := item.ChildNodes[i]; 
      if (Child.NodeName = 'ENTRY') then 
        AddRecursive(Node, Child); 
    end; 
  end; 
var 
  Root: IXMLMENUType; 
  Parent: TMenuItem; 
  I: Integer; 
  Child: IXMLNode; 
begin 
  XMLDocument.FileName := ExtractFilePath(Application.ExeName) + XMLFile; 
  if not FileExists(XMLDocument.FileName) then 
  begin 
    MessageDlg('Menu-XML-Document nicht gefunden!', mtError, [mbOK], 0); 
    Halt; 
  end; 
  XMLDocument.Active := True; 

  Screen.Cursor := crHourglass; 
  try 
    Root := GetXMLMenu(XMLDocument); 
    Parent := nil; 

    for I := 0 to Root.ChildNodes.Count - 1 do 
    begin 
      Child := Root.ChildNodes[i]; 
      if (Child.NodeName = 'ENTRY') then 
        AddRecursive(Parent, Child); 
    end; 
  finally 
    Screen.Cursor := crDefault; 
  end; 
end; 

{---------------------------------------------------------- 
  You also need the encapsulation of the XML-File. 
  ( Save it as unit and add it to your program. 
   Created with Delphi6 -> New -> XML Data Binding Wizard ) 
-----------------------------------------------------------} 

{***************************************************} 
{                                                   } 
{              Delphi XML-Datenbindung              } 
{                                                   } 
{         Erzeugt am: 27.06.2002 13:25:01           } 
{                                                   } 
{***************************************************} 

unit XMLMenuTranslation; 

interface 

uses xmldom, XMLDoc, XMLIntf; 

type 

  { Forward-Deklarationen } 

  IXMLMENUType  = interface; 
  IXMLENTRYType = interface; 

  { IXMLMENUType } 

  IXMLMENUType = interface(IXMLNode) 
    ['{8F36F5E2-834F-41D9-918F-9B1A441C9074}'] 
    { Zugriff auf Eigenschaften } 
    function Get_ENTRY: IXMLENTRYType; 
    { Methoden & Eigenschaften } 
    property ENTRY: IXMLENTRYType read Get_ENTRY; 
  end; 

  { IXMLENTRYType } 

  IXMLENTRYType = interface(IXMLNode) 
    ['{AD85CD05-725E-40F8-A8D7-D6EC05FD4360}'] 
    { Zugriff auf Eigenschaften } 
    function Get_CAPTION: WideString; 
    function Get_VISIBLE: Integer; 
    function Get_ID: Integer; 
    function Get_ENTRY: IXMLENTRYType; 
    procedure Set_CAPTION(Value: WideString); 
    procedure Set_VISIBLE(Value: Integer); 
    procedure Set_ID(Value: Integer); 
    { Methoden & Eigenschaften } 
    property Caption: WideString read Get_CAPTION write Set_CAPTION; 
    property Visible: Integer read Get_VISIBLE write Set_VISIBLE; 
    property ID: Integer read Get_ID write Set_ID; 
    property ENTRY: IXMLENTRYType read Get_ENTRY; 
  end; 

  { Forward-Deklarationen } 

  TXMLMENUType  = class; 
  TXMLENTRYType = class; 

  { TXMLMENUType } 

  TXMLMENUType = class(TXMLNode, IXMLMENUType) 
  protected 
    { IXMLMENUType } 
    function Get_ENTRY: IXMLENTRYType; 
  public 
    procedure AfterConstruction; override; 
  end; 

  { TXMLENTRYType } 

  TXMLENTRYType = class(TXMLNode, IXMLENTRYType) 
  protected 
    { IXMLENTRYType } 
    function Get_CAPTION: WideString; 
    function Get_VISIBLE: Integer; 
    function Get_ID: Integer; 
    function Get_ENTRY: IXMLENTRYType; 
    procedure Set_CAPTION(Value: WideString); 
    procedure Set_VISIBLE(Value: Integer); 
    procedure Set_ID(Value: Integer); 
  public 
    procedure AfterConstruction; override; 
  end; 

  { Globale Funktionen } 

function GetXMLMENU(Doc: IXMLDocument): IXMLMENUType; 
function LoadMENU(const FileName: WideString): IXMLMENUType; 
function NewMENU: IXMLMENUType; 

implementation 

{ Globale Funktionen } 

function GetXMLMENU(Doc: IXMLDocument): IXMLMENUType; 
begin 
  Result := Doc.GetDocBinding('MENU', TXMLMENUType) as IXMLMENUType; 
end; 

function LoadMENU(const FileName: WideString): IXMLMENUType; 
begin 
  Result := LoadXMLDocument(FileName).GetDocBinding('MENU', TXMLMENUType) as IXMLMENUType; 
end; 

function NewMENU: IXMLMENUType; 
begin 
  Result := NewXMLDocument.GetDocBinding('MENU', TXMLMENUType) as IXMLMENUType; 
end; 

{ TXMLMENUType } 

procedure TXMLMENUType.AfterConstruction; 
begin 
  RegisterChildNode('ENTRY', TXMLENTRYType); 
  inherited; 
end; 

function TXMLMENUType.Get_ENTRY: IXMLENTRYType; 
begin 
  Result := ChildNodes['ENTRY'] as IXMLENTRYType; 
end; 

{ TXMLENTRYType } 

procedure TXMLENTRYType.AfterConstruction; 
begin 
  RegisterChildNode('ENTRY', TXMLENTRYType); 
  inherited; 
end; 

function TXMLENTRYType.Get_CAPTION: WideString; 
begin 
  Result := ChildNodes['CAPTION'].Text; 
end; 

procedure TXMLENTRYType.Set_CAPTION(Value: WideString); 
begin 
  ChildNodes['CAPTION'].NodeValue := Value; 
end; 

function TXMLENTRYType.Get_VISIBLE: Integer; 
begin 
  Result := ChildNodes['VISIBLE'].NodeValue; 
end; 

procedure TXMLENTRYType.Set_VISIBLE(Value: Integer); 
begin 
  ChildNodes['VISIBLE'].NodeValue := Value; 
end; 

function TXMLENTRYType.Get_ID: Integer; 
begin 
  Result := ChildNodes['ID'].NodeValue; 
end; 

procedure TXMLENTRYType.Set_ID(Value: Integer); 
begin 
  ChildNodes['ID'].NodeValue := Value; 
end; 

function TXMLENTRYType.Get_ENTRY: IXMLENTRYType; 
begin 
  Result := ChildNodes['ENTRY'] as IXMLENTRYType; 
end; 

end.
