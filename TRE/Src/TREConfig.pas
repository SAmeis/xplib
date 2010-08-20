{$IFDEF TREConfig}
	 {$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I TRELib.inc}

unit TREConfig;

interface

uses
    Classes, SysUtils, XMLDoc, XMLIntf, AppSettings, TREZones;

const
	TRE_DV_CONFIG_FILENAME = 'BaseConfig.xml';

type
    TTREBaseConfig = class(TComponent)
    private
        _FCentralMapping : TTRECentralMapping;
        FCfg : TBaseSettings;
        function GetCentralMapping : TTRECentralMapping;
    public
        constructor Create(const XMLFilename : string); reintroduce;
        destructor Destroy; override;
        property CentralMapping : TTRECentralMapping read GetCentralMapping;
    end;

implementation

{ TTREBaseConfig }

constructor TTREBaseConfig.Create(const XMLFilename : string);
var
    xmlDoc : TXMLDocument;
    node :   IXMLNode;
begin
	 if not(FileExists( XMLFilename )) then begin
	  raise Exception.CreateFmt('Arquivo "%s" para leitura de confgurações não encontrado', [ XMLFilename ]);
	 end;
	 xmlDoc := TXMLDocument.Create(Self);
	 try
		xmlDoc.LoadFromFile(XMLFilename);
	 except
		on E : Exception do begin
			raise Exception.CreateFmt('Arquivo de configurações não pode ser lido:'#13'%s', [ E.Message ]);
		end;
    end;
    node      := xmlDoc.Node;
    Self.FCfg := TXMLBasedSettings.Create(node);
end;

destructor TTREBaseConfig.Destroy;
begin
	 Self.FCfg.Free;
    Self._FCentralMapping.Free;
    inherited;
end;

function TTREBaseConfig.GetCentralMapping : TTRECentralMapping;
begin
    if not (Assigned(Self._FCentralMapping)) then begin
        Self._FCentralMapping := TTRECentralMapping.Create;
        {TODO -oroger -cdsg : Realizar a carga das instancias das centrais e zonas pela instancia de configuracao}
    end;
    Result := Self._FCentralMapping;
end;

end.
