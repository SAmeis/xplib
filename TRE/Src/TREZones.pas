{$IFDEF TREZones}
	 {$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I TRELib.inc}

unit TREZones;

interface

uses
	 SysUtils, contnrs, Classes, TREConsts, TREUtils, XMLIntf, Generics.Collections;


type
	TTRESerializable = class( TPersistent );  // TXMLSerializable
	TTRESerializableList  = class( TList ); //   TXMLSerializableList
	TTREXMLPersist = class(TTRESerializable);
	TTREUnits = class(TList<TTREXMLPersist>); //Ancestral de zonas e de central

	 TTRELocalNet = class(TTRESerializable)
	 private
		 FPrimaryZone : Integer;
		 FUnits :       TTRESerializableList;
		 function GetSubNet : string;
	 public
		 constructor Create(APrimaryZoneId : Integer); virtual;
		 property SubNet : string read GetSubNet;
	 published
		 property PrimaryZoneId : Integer read FPrimaryZone write FPrimaryZone;
		 property Units : TTRESerializableList read FUnits write FUnits;
	 end;

	 TTRERegional = class(TTREUnits)
	 private
		 FLocalNetwork : TTRESerializableList;
		 FDescription :  string;
		 function GetNetworks(index : Integer) : TTRELocalNet;
	 public
		 constructor Create; virtual;
		 function AddNetwork(ANet : TTRELocalNet) : Integer;
		 property Networks[index : Integer] : TTRELocalNet read GetNetworks;
	 published
		 property LocalNetwork : TTRESerializableList read FLocalNetwork write FLocalNetwork;
		 property Description : string read FDescription write FDescription;
	 end;


	 //Estação de trabalho
	 TTREStation = class(TTREXMLPersist)
    private
        function GetIsPrimary : boolean;
        procedure SetIsPrimary(const Value : boolean);
    public
		 constructor Create(AUnit : TTREUnits); virtual;
		 property isPrimary : boolean read GetIsPrimary write SetIsPrimary;
	 published
	 end;

	 TTRECentral = class;

	 TTREZone = class(TTREUnits)
	 private
		 FId :      Integer;
		 FCentral : TTRECentral;
		 function GetCentral : TTRECentral;
		 function GetCentralIndex : Integer;
		 procedure SetCentral(const Value : TTRECentral);
	 protected
	 public
		 constructor Create(AZoneId : Integer); virtual;
		 procedure BeforeDestruction; override;
		 property Id : Integer read FId;
		 /// <summary> Retorna ordem da zona dentro da central, caso haja uma definida para a mesma </summary>
		 /// <remarks>
		 /// Caso não haja central pai para esta zona -1 será retornado
		 /// </remarks>
		 property CentralIndex : Integer read GetCentralIndex;
		 property Central : TTRECentral read GetCentral write SetCentral;
	 end;

	 TTRECentral = class(TTREUnits)
	 private
		 FId :       Integer;
		 FZoneList : TObjectList;
		 /// <summary>
		 /// Retorna a zona indexada pelo valor de index(lista interna)
		 /// </summary>
		 /// <param name="index"></param>
		 /// <returns>Zona eleitoral da central</returns>
		 function GetZones(index : Integer) : TTREZone;
		 function GetPrimaryZone : TTREZone;
		 ///  <summary> Ajusta a zona dada como sendo a zona primária </summary>
		 procedure SetPrimaryZone(const Value : TTREZone);
		 function GetCount : Integer;
	 public
		 constructor Create(ACentralId : Integer); virtual;
		 property Id : Integer read FId;
		 property Count : Integer read GetCount;
		 property Zones[index : Integer] : TTREZone read GetZones;
		 property PrimaryZone : TTREZone read GetPrimaryZone write SetPrimaryZone;
        ///  <summary> Adiciona uma zona a lista/central </summary>
        ///  <remarks> Impede a repetição das zonas por instância </remarks>
        function Add(Zone : TTREZone) : Integer;
        ///  <summary>Indica se a zona passada pertence ao conjunto(central)</summary>
        ///  <remarks>
		 ///  Indica se a zona passada pertence ao conjunto(central)
        ///  Informa-se o identificar da zona
        ///  </remarks>
        function isPertinent(ZoneId : Integer) : boolean;
        function GetZoneById(ZoneId : Integer) : TTREZone;
    end;

	 TTRECentralMapping = class(TTRESerializable)
    private
		 FCentralList : TTRESerializableList;
        function GetCentrals(index : Integer) : TTRECentral;
        procedure SetCentrals(index : Integer; const Value : TTRECentral);
    public
        constructor Create; virtual;
        destructor Destroy; override;
        procedure LoadHardCoded;
        procedure LoadXMLConfig(CentralMapping : IXMLNode);
        procedure SaveXMLConfig(CentralMapping : IXMLNode);
        function GetZoneById(ZoneId : Integer) : TTREZone;
        property Centrals[index : Integer] : TTRECentral read GetCentrals;
    published

	 end;

implementation

procedure TTREZone.BeforeDestruction;
begin
	 if Assigned(Self.FCentral) then begin
		 Self.FCentral.FZoneList.Remove(Self);
        Self.FCentral := nil;
    end;
    inherited;
end;

constructor TTREZone.Create(AZoneId : Integer);
begin
    inherited Create;
    Self.FId := AZoneId;
end;

function TTRECentral.Add(Zone : TTREZone) : Integer;
begin
    Result := Self.FZoneList.IndexOf(Zone);
    if Result < 0 then begin
        Result := Self.FZoneList.Add(Zone);
        Zone.FCentral := Self;
    end;
end;

constructor TTRECentral.Create(ACentralId : Integer);
begin
    inherited Create;
    Self.FId := ACentralId;
    Self.FZoneList := TObjectList.Create;
    Self.FZoneList.OwnsObjects := False;
end;

function TTRECentral.GetCount : Integer;
begin
    Result := Self.FZoneList.Count;
end;

function TTRECentral.GetPrimaryZone : TTREZone;
begin
    if (Self.FZoneList.Count > 0) then begin
        Result := TTREZone(Self.FZoneList.Items[0]);
    end else begin
        Result := nil;
    end;
end;

function TTRECentral.GetZoneById(ZoneId : Integer) : TTREZone;
var
    x : Integer;
begin
    Result := nil;
    for x := 0 to Self.FZoneList.Count - 1 do begin
        if TTREZone(Self.FZoneList.Items[x]).Id = ZoneId then begin
            Result := TTREZone(Self.FZoneList.Items[x]);
            Exit;
        end;
    end;
end;

function TTRECentral.GetZones(index : Integer) : TTREZone;
begin
    Result := TTREZone(Self.FZoneList.Items[index]);
end;

function TTRECentral.isPertinent(ZoneId : Integer) : boolean;
var
    x : Integer;
begin
    Result := False;
    for x := 0 to Self.FZoneList.Count - 1 do begin
        if (TTREZone(Self.FZoneList.Items[x]).FId = ZoneId) then begin
            Result := True;
            Exit;
        end;
    end;
end;

procedure TTRECentral.SetPrimaryZone(const Value : TTREZone);
var
    current : TTREZone;
begin
    Self.Add(Value);
    current := Self.GetPrimaryZone;
    if (current <> Value) then begin //Fazer permuta, deve-se fazer lock neste momento no futuro
        Self.FZoneList.Exchange(current.CentralIndex, Value.CentralIndex);
    end;
end;

function TTREZone.GetCentral : TTRECentral;
begin
    Result := Self.FCentral;
end;

function TTREZone.GetCentralIndex : Integer;
begin
    if Assigned(Self.FCentral) then begin
        Result := Self.FCentral.FZoneList.IndexOf(Self);
    end else begin
        Result := -1;
    end;
end;

procedure TTREZone.SetCentral(const Value : TTRECentral);
begin
    if Assigned(Self.FCentral) then begin     //Remove da anterior se for o caso
        Self.FCentral.FZoneList.Remove(Self);
    end;
    if Assigned(Value) then begin
        Value.Add(Self);
    end else begin
        Self.FCentral := nil;
	 end;
end;

constructor TTRECentralMapping.Create;
begin
    Self.FCentralList := TTRESerializableList.Create;
end;

destructor TTRECentralMapping.Destroy;
begin
    Self.FCentralList.Free;
    inherited;
end;

function TTRECentralMapping.GetCentrals(index : Integer) : TTRECentral;
begin
    Result := TTRECentral(Self.FCentralList.Items[index]);
end;

function TTRECentralMapping.GetZoneById(ZoneId : Integer) : TTREZone;
var
    c : Integer;
	 central : TTRECentral;
begin
    Result := nil;
    for c := 0 to Self.FCentralList.Count - 1 do begin
        central := TTRECentral(Self.FCentralList.Items[c]);
        Result  := central.GetZoneById(ZoneId);
        if Assigned(Result) then begin
            Exit;
        end;
    end;
end;

procedure TTRECentralMapping.LoadHardCoded;
var
    c : TTRECentral;
    z : TTREZone;
begin
    //CENTRAL ( 1 ) - capital joão pessoa
    c := TTRECentral.Create(1);
    Self.FCentralList.Add(c);
    z := TTREZone.Create(1);
    z.Central := c;
	 z := TTREZone.Create(64);
    z.Central := c;
    z := TTREZone.Create(70);
    z.Central := c;
    z := TTREZone.Create(76);
    z.Central := c;
    z := TTREZone.Create(77);
    z.Central := c;

    //CENTRAL ( 2 ) - Campina Grande
    c := TTRECentral.Create(2);
    Self.FCentralList.Add(c);
    z := TTREZone.Create(16);
    z.Central := c;
    z := TTREZone.Create(17);
    z.Central := c;
    z := TTREZone.Create(71);
    z.Central := c;
    z := TTREZone.Create(72);
    z.Central := c;

    //CENTRAL ( 3 ) - Patos
	 c := TTRECentral.Create(3);
    Self.FCentralList.Add(c);
    z := TTREZone.Create(28);
    z.Central := c;
    z := TTREZone.Create(65);
    z.Central := c;

	 //CENTRAL ( 5 ) - Sousa
    c := TTRECentral.Create(4);
    Self.FCentralList.Add(c);
    z := TTREZone.Create(35);
    z.Central := c;
    z := TTREZone.Create(63);
    z.Central := c;

	 //CENTRAL ( 4 ) - Cajazeiras
    c := TTRECentral.Create(5);
    Self.FCentralList.Add(c);
    z := TTREZone.Create(42);
    z.Central := c;
    z := TTREZone.Create(68);
    z.Central := c;

    //CENTRAL ( 6 ) - Piancó
    c := TTRECentral.Create(6);
    Self.FCentralList.Add(c);
    z := TTREZone.Create(32);
    z.Central := c;
    z := TTREZone.Create(66);
    z.Central := c;
end;

procedure TTRECentralMapping.LoadXMLConfig(CentralMapping : IXMLNode);
begin

end;

procedure TTRECentralMapping.SaveXMLConfig(CentralMapping : IXMLNode);
begin

end;

procedure TTRECentralMapping.SetCentrals(index : Integer; const Value : TTRECentral);
begin

end;

{ TTREStation }

constructor TTREStation.Create(AUnit : TTREUnit);
begin

end;

function TTREStation.GetIsPrimary : boolean;
begin

end;

procedure TTREStation.SetIsPrimary(const Value : boolean);
begin

end;

{ TTRERegional }

function TTRERegional.AddNetwork(ANet : TTRELocalNet) : Integer;
begin
    Result := Self.FLocalNetwork.Add(ANet);
end;

constructor TTRERegional.Create;
begin
    inherited;
    Self.FLocalNetwork := TTRESerializableList.Create;
end;

function TTRERegional.GetNetworks(index : Integer) : TTRELocalNet;
begin
    Result := TTRELocalNet(Self.FLocalNetwork.Items[index]);
end;

{ TTRELocalNet }

constructor TTRELocalNet.Create(APrimaryZoneId : Integer);
begin
	 inherited Create;
	 Self.FUnits:=TTRESerializableList.Create;
	 Self.FPrimaryZone := APrimaryZoneId;
end;

function TTRELocalNet.GetSubNet : string;
begin
    Result := '10.183.' + Format('%3.3d', [Self.FPrimaryZone]) + '.*';
end;

end.
