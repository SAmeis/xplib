{$IFDEF WNetSever}
	 {$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I WinNetLib.inc}

unit WNetSever;

interface

uses
	SysUtils, Classes, LmUtils, LmErr, AppLog, Contnrs;


type
	ELanManager = class( ELoggedException );

type
	TLanManagerServer = class;
	TServerResource = class( TObject )
	private
		FName : string;
		FParent: TLanManagerServer;
	public
		property Name : string read FName;
		property Parent : TLanManagerServer read FParent;
		constructor Create( const AName : string; AParent : TLanManagerServer ); virtual;
	end;

	TLanManagerServer = class( TComponent )
	private
		FConnected: boolean;
		FServerName: string;
		procedure SetConnected(const Value: boolean);
		procedure SetServerName(const Value: string);
		procedure SetUserName(const Value: string);
		function GetUserName: string;
		function GetResourceCount: integer;
		function GetResource(Index: integer): TServerResource;
	private
		InternalUserName : string;
		ResourceList : TObjectList;
	protected
		procedure LoadServerResources();
		procedure ResourceListNeeded();
	public
		property Resource[ Index : integer ] : TServerResource read GetResource;
		property ResourceCount : integer read GetResourceCount;
		constructor Create( AOwner : TComponent ); override;
		destructor  Destroy; override;
		procedure Refresh();
	published
		property Connected : boolean read FConnected write SetConnected;
		property ServerName : string read FServerName write SetServerName;
		property UserName : string read GetUserName write SetUserName;
	end;

implementation

uses
	WinNetHnd;

procedure RaiseNotConnected;
//----------------------------------------------------------------------------------------------------------------------------------
begin
	raise ELanManager.Create( 'Serviço não pode ser realizado sem acesso ao Servidor' );
end;


{ TLanManagerServer }

constructor TLanManagerServer.Create(AOwner: TComponent);
//----------------------------------------------------------------------------------------------------------------------------------
begin
	inherited;
	Self.ResourceList:=nil;
	Self.FConnected:=False;
	Self.FServerName:=EmptyStr;
	Self.InternalUserName:=EmptyStr;
end;

destructor TLanManagerServer.Destroy;
//----------------------------------------------------------------------------------------------------------------------------------
begin
	Self.ResourceList.Free;
	Self.Connected:=False;
	inherited;
end;

function TLanManagerServer.GetResource(Index: integer): TServerResource;
//----------------------------------------------------------------------------------------------------------------------------------
begin
   Self.ResourceListNeeded();
	Result:=TServerResource( Self.ResourceList.Items[ Index ] );
end;

function TLanManagerServer.GetResourceCount: integer;
//----------------------------------------------------------------------------------------------------------------------------------
begin
   Self.ResourceListNeeded;
	Result:=Self.ResourceList.Count;
end;

function TLanManagerServer.GetUserName: string;
//----------------------------------------------------------------------------------------------------------------------------------
begin
	if Self.InternalUserName = EmptyStr then begin
		Result:=GetUserName();
	end else begin
		Result:=Self.InternalUserName;
	end;
end;

procedure TLanManagerServer.LoadServerResources;
//----------------------------------------------------------------------------------------------------------------------------------
begin
	{ TODO -oRoger -cLIB : Carrega lista dos recursos do servidor }
	if not Self.Connected then begin
		RaiseNotConnected;
	end;
end;

procedure TLanManagerServer.Refresh;
//----------------------------------------------------------------------------------------------------------------------------------
begin
	//Renova lista de recursos
	if Assigned( Self.ResourceList ) then begin
		FreeAndNil( Self.ResourceList );
		Self.ResourceListNeeded();
	end;
end;

procedure TLanManagerServer.ResourceListNeeded;
//----------------------------------------------------------------------------------------------------------------------------------
begin
	if not Assigned( Self.ResourceList ) then begin
		Self.ResourceList:=TObjectList.Create( True ); //Destroi filhos na remocao
		Self.LoadServerResources;
	end;
end;

procedure TLanManagerServer.SetConnected(const Value: boolean);
//----------------------------------------------------------------------------------------------------------------------------------
begin
	FConnected := Value;
	{ TODO -oRoger -cLIB : Abre ou fecha conexao com o servidor em questao }
	if not Value then begin
		{ TODO -oRoger -cLIB : Anular lista de recursos }
		FreeAndNil( Self.ResourceList );
	end else begin
		{ TODO -oRoger -cLIB : Passos ainda indefinidos para a abertura da conexao}
	end;
end;

procedure TLanManagerServer.SetServerName(const Value: string);
//----------------------------------------------------------------------------------------------------------------------------------
var
	OldConnState : boolean;
begin
	if not SameText( Value, Self.FServerName ) then begin
		OldConnState:=Self.FConnected;
		Self.Connected:=False;  //Fecha conexao anterior
		FServerName := Value;
		Self.Connected:=OldConnState;
	end; //Ignora se valor igual
end;

procedure TLanManagerServer.SetUserName(const Value: string);
//----------------------------------------------------------------------------------------------------------------------------------
var
	OldConnState : boolean;
	UserValue : string;
begin
	if Value = EmptyStr then begin
		UserValue:=GetUserName();
	end;
	if not SameText( Self.UserName, UserValue ) then begin
		OldConnState:=Self.FConnected;
		Self.Connected:=False;
		Self.InternalUserName := Value;
		Self.Connected:=OldConnState;
	end;
end;

{ TServerResource }

constructor TServerResource.Create(const AName : string; AParent : TLanManagerServer ); 
//----------------------------------------------------------------------------------------------------------------------------------
begin
	inherited Create;
	Self.FParent:=AParent;
	Self.FName:=AName;
end;

end.
