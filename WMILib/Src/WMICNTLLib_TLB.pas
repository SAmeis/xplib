unit WMICNTLLib_TLB;

// ************************************************************************ //
// WARNING                                                                    
// -------                                                                    
// The types declared in this file were generated from data read from a       
// Type Library. If this type library is explicitly or indirectly (via        
// another type library referring to this type library) re-imported, or the   
// 'Refresh' command of the Type Library Editor activated while editing the   
// Type Library, the contents of this file will be regenerated and all        
// manual modifications will be lost.                                         
// ************************************************************************ //

// $Rev: 17244 $
// File generated on 31/3/2011 18:29:53 from Type Library described below.

// ************************************************************************  //
// Type Lib: C:\WINDOWS\system32\wbem\wbemcntl.dll (1)
// LIBID: {5C65924B-E236-11D2-8899-00104B2AFB46}
// LCID: 0
// Helpfile: 
// HelpString: WMICntl 1.0 Type Library
// DepndLst: 
//   (1) v2.0 stdole, (C:\WINDOWS\system32\stdole2.tlb)
// ************************************************************************ //
// *************************************************************************//
// NOTE:                                                                      
// Items guarded by $IFDEF_LIVE_SERVER_AT_DESIGN_TIME are used by properties  
// which return objects that may need to be explicitly created via a function 
// call prior to any access via the property. These items have been disabled  
// in order to prevent accidental use from within the object inspector. You   
// may enable them by defining LIVE_SERVER_AT_DESIGN_TIME or by selectively   
// removing them from the $IFDEF blocks. However, such items must still be    
// programmatically created via a method of the appropriate CoClass before    
// they can be used.                                                          
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
{$ALIGN 4}
interface

uses Windows, ActiveX, Classes, Graphics, OleServer, StdVCL, Variants;
  

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  WMICNTLLibMajorVersion = 1;
  WMICNTLLibMinorVersion = 0;

  LIBID_WMICNTLLib: TGUID = '{5C65924B-E236-11D2-8899-00104B2AFB46}';

  CLASS_WMISnapin: TGUID = '{5C659257-E236-11D2-8899-00104B2AFB46}';
  CLASS_WMISnapinAbout: TGUID = '{5C659258-E236-11D2-8899-00104B2AFB46}';
type

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  WMISnapin = IUnknown;
  WMISnapinAbout = IUnknown;


// *********************************************************************//
// The Class CoWMISnapin provides a Create and CreateRemote method to          
// create instances of the default interface IUnknown exposed by              
// the CoClass WMISnapin. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoWMISnapin = class
    class function Create: IUnknown;
    class function CreateRemote(const MachineName: string): IUnknown;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TWMISnapin
// Help String      : WMISnapin Class
// Default Interface: IUnknown
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TWMISnapinProperties= class;
{$ENDIF}
  TWMISnapin = class(TOleServer)
  private
    FIntf: IUnknown;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TWMISnapinProperties;
    function GetServerProperties: TWMISnapinProperties;
{$ENDIF}
    function GetDefaultInterface: IUnknown;
  protected
    procedure InitServerData; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IUnknown);
    procedure Disconnect; override;
    property DefaultInterface: IUnknown read GetDefaultInterface;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TWMISnapinProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TWMISnapin
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TWMISnapinProperties = class(TPersistent)
  private
    FServer:    TWMISnapin;
    function    GetDefaultInterface: IUnknown;
    constructor Create(AServer: TWMISnapin);
  protected
  public
    property DefaultInterface: IUnknown read GetDefaultInterface;
  published
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoWMISnapinAbout provides a Create and CreateRemote method to          
// create instances of the default interface IUnknown exposed by              
// the CoClass WMISnapinAbout. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoWMISnapinAbout = class
    class function Create: IUnknown;
    class function CreateRemote(const MachineName: string): IUnknown;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TWMISnapinAbout
// Help String      : WMISnapin Class About
// Default Interface: IUnknown
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TWMISnapinAboutProperties= class;
{$ENDIF}
  TWMISnapinAbout = class(TOleServer)
  private
    FIntf: IUnknown;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TWMISnapinAboutProperties;
    function GetServerProperties: TWMISnapinAboutProperties;
{$ENDIF}
    function GetDefaultInterface: IUnknown;
  protected
    procedure InitServerData; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IUnknown);
    procedure Disconnect; override;
    property DefaultInterface: IUnknown read GetDefaultInterface;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TWMISnapinAboutProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TWMISnapinAbout
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TWMISnapinAboutProperties = class(TPersistent)
  private
    FServer:    TWMISnapinAbout;
    function    GetDefaultInterface: IUnknown;
    constructor Create(AServer: TWMISnapinAbout);
  protected
  public
    property DefaultInterface: IUnknown read GetDefaultInterface;
  published
  end;
{$ENDIF}


procedure Register;

resourcestring
  dtlServerPage = 'WMI';

  dtlOcxPage = 'WMI';

implementation

uses ComObj;

class function CoWMISnapin.Create: IUnknown;
begin
  Result := CreateComObject(CLASS_WMISnapin) as IUnknown;
end;

class function CoWMISnapin.CreateRemote(const MachineName: string): IUnknown;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_WMISnapin) as IUnknown;
end;

procedure TWMISnapin.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{5C659257-E236-11D2-8899-00104B2AFB46}';
    IntfIID:   '{00000000-0000-0000-C000-000000000046}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TWMISnapin.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IUnknown;
  end;
end;

procedure TWMISnapin.ConnectTo(svrIntf: IUnknown);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TWMISnapin.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TWMISnapin.GetDefaultInterface: IUnknown;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TWMISnapin.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TWMISnapinProperties.Create(Self);
{$ENDIF}
end;

destructor TWMISnapin.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TWMISnapin.GetServerProperties: TWMISnapinProperties;
begin
  Result := FProps;
end;
{$ENDIF}

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TWMISnapinProperties.Create(AServer: TWMISnapin);
begin
  inherited Create;
  FServer := AServer;
end;

function TWMISnapinProperties.GetDefaultInterface: IUnknown;
begin
  Result := FServer.DefaultInterface;
end;

{$ENDIF}

class function CoWMISnapinAbout.Create: IUnknown;
begin
  Result := CreateComObject(CLASS_WMISnapinAbout) as IUnknown;
end;

class function CoWMISnapinAbout.CreateRemote(const MachineName: string): IUnknown;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_WMISnapinAbout) as IUnknown;
end;

procedure TWMISnapinAbout.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{5C659258-E236-11D2-8899-00104B2AFB46}';
    IntfIID:   '{00000000-0000-0000-C000-000000000046}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TWMISnapinAbout.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IUnknown;
  end;
end;

procedure TWMISnapinAbout.ConnectTo(svrIntf: IUnknown);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TWMISnapinAbout.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TWMISnapinAbout.GetDefaultInterface: IUnknown;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TWMISnapinAbout.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TWMISnapinAboutProperties.Create(Self);
{$ENDIF}
end;

destructor TWMISnapinAbout.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TWMISnapinAbout.GetServerProperties: TWMISnapinAboutProperties;
begin
  Result := FProps;
end;
{$ENDIF}

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TWMISnapinAboutProperties.Create(AServer: TWMISnapinAbout);
begin
  inherited Create;
  FServer := AServer;
end;

function TWMISnapinAboutProperties.GetDefaultInterface: IUnknown;
begin
  Result := FServer.DefaultInterface;
end;

{$ENDIF}

procedure Register;
begin
  RegisterComponents(dtlServerPage, [TWMISnapin, TWMISnapinAbout]);
end;

end.
