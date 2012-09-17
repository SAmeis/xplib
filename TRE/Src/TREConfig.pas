{$IFDEF TREConfig}
     {$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I TRELib.inc}

unit TREConfig;

interface

uses
    Classes, SysUtils, XMLDoc, XMLIntf, AppSettings, TREZones, OPXMLSerializable;

const
    TRE_DV_CONFIG_FILENAME = 'BaseConfig.xml';

type
    TTREBaseConfig = class(TXMLSerializable)
    private
        _FCentralMapping : TTRECentralMapping;
        FAnchor : TComponent;
        XMLDoc : TXMLDocument;
        FCfg : TBaseSettings;
        function GetCentralMapping : TTRECentralMapping;
        function GetDebugLevel : Integer;
        procedure SetDebugLevel(const Value : Integer);
    public
        constructor Create(const XMLFilename : string); reintroduce;
        destructor Destroy; override;
        property DebugLevel : Integer read GetDebugLevel write SetDebugLevel;
    published
        property CentralMapping : TTRECentralMapping read GetCentralMapping;
    end;

implementation



{ TTREBaseConfig }

constructor TTREBaseConfig.Create(const XMLFilename : string);
var
    node : IXMLNode;
begin
    if not (FileExists(XMLFilename)) then begin
        raise Exception.CreateFmt('Arquivo "%s" para leitura de confgurações não encontrado', [XMLFilename]);
    end;
    Self.FAnchor := TComponent.Create(nil);
    xmlDoc := TXMLDocument.Create(Self.FAnchor);
    XMLDoc.Options := XMLDoc.Options + [doNodeAutoCreate];
    try
        xmlDoc.LoadFromFile(XMLFilename);
    except
        on E : Exception do begin
            raise Exception.CreateFmt('Arquivo de configurações não pode ser lido:'#13'%s', [E.Message]);
        end;
    end;
    node      := xmlDoc.Node;
    Self.FCfg := TXMLBasedSettings.Create(node);
end;

destructor TTREBaseConfig.Destroy;
begin
    Self.FCfg.Free;
    Self._FCentralMapping.Free;
    Self.XMLDoc.Free;
    Self.FAnchor.Free;
    inherited;
end;

function TTREBaseConfig.GetCentralMapping : TTRECentralMapping;
begin
    if not (Assigned(Self._FCentralMapping)) then begin
        Self._FCentralMapping := TTRECentralMapping.Create;
        {TODO -oroger -cdsg_urgente : Realizar a carga das instancias das centrais e zonas pela instancia de configuracao}
    end;
    Result := Self._FCentralMapping;
end;

function TTREBaseConfig.GetDebugLevel : Integer;
begin
    { TODO -oroger -curgente : Leitura falha deste atributo, valor padrao = zero }
    Result := 10;
end;

procedure TTREBaseConfig.SetDebugLevel(const Value : Integer);
begin
    { TODO -oroger -curgente : Constante para entrada }
end;

initialization
    begin
        RegisterClass(TTREBaseConfig);
    end;

end.
