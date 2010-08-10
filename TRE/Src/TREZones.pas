unit TREZones;

interface

uses
    contnrs, TREConsts, TREUtils;

type
    TTREZone = class
    private
        FId : Integer;
    public
        constructor Create(AZoneId : Integer); virtual;
        property Id : Integer read FId;
    end;

    TTRECentral = class
    private
        FId :       Integer;
        FZoneList : TObjectList;
        function GetZones(index : Integer) : TTREZone;
	 function GetPrimaryZone: TTREZone;
    procedure SetPrimaryZone(const Value: TTREZone);
	 public
		 constructor Create(ACentralId : Integer); virtual;
		 property Id : Integer read FId;
		 property Zones[index : Integer] : TTREZone read GetZones;
		 property PrimaryZone : TTREZone read GetPrimaryZone write SetPrimaryZone;
		 function Add(Zone : TTREZone) : Integer;
		 function isPertinent(ZoneId : integer) : boolean;
    end;

implementation

constructor TTREZone.Create(AZoneId : Integer);
begin
    inherited Create;
    Self.FId := AZoneId;
end;

function TTRECentral.Add(Zone : TTREZone) : Integer;
begin
	 Result := Self.FZoneList.Add(Zone);
end;

constructor TTRECentral.Create(ACentralId : Integer);
begin
	 inherited Create;
	 Self.FId := ACentralId;
end;

function TTRECentral.GetPrimaryZone: TTREZone;
begin

end;

function TTRECentral.GetZones(index : Integer) : TTREZone;
begin
    Result := TTREZone(Self.FZoneList.Items[index]);
end;

function TTRECentral.isPertinent(ZoneId: integer): boolean;
begin

end;

procedure TTRECentral.SetPrimaryZone(const Value: TTREZone);
var
	pivot : TTREZone;
	pivotIndex : integer;
begin

end;

end.
