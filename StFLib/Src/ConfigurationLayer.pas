{$IFDEF ConfigurationLayer}
{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I StFLib.inc}
unit ConfigurationLayer;

{ {
  Unit que implementa classe de acesso as configuracoes de um aplicativo.
}
interface

uses
  SysUtils, Windows, Registry, IniFiles, WinReg32, AppSettings;

type

  TBaseConfiguration = class(TObject)
  private
	FOwnedData: Boolean;
	procedure SetUserPrefs(const Value: TBaseUserPreferences);
  protected
	FGlobalSettings: TBaseGlobalSettings;
	FLocalSettings: TBaseLocalSettings;
	FStartupSettings: TBaseStartSettings;
	FUserPrefs: TBaseUserPreferences;
	FUserSettings: TBaseUserSettings;
  public
	constructor Create(CreateOmitted: Boolean; AGlobalSettings: TBaseGlobalSettings; ALocalSettings: TBaseLocalSettings;
		AStartupSettings: TBaseStartSettings; AUserPrefs: TBaseUserPreferences; AUserSettings: TBaseUserSettings);
	destructor Destroy; override;
	property GlobalSettings: TBaseGlobalSettings read FGlobalSettings;
	property LocalSettings: TBaseLocalSettings read FLocalSettings;
	property OwnedData: Boolean read FOwnedData write FOwnedData;
	property StartupSettings: TBaseStartSettings read FStartupSettings;
	property UserPrefs: TBaseUserPreferences read FUserPrefs write SetUserPrefs;
	property UserSettings: TBaseUserSettings read FUserSettings;
  end;

implementation

{ -**********************************************************************
  ************************************************************************
  ******************
  ******************  Class:    TBaseConfiguration
  ******************  Category: Undefined
  ******************
  ************************************************************************
  ************************************************************************ }
{ {
  Classe ancestral de todas a serem usadas para implementar acesso as configuracoes
}
{ ---------------------------------------------------------------------------------------------------------------------------------- }
constructor TBaseConfiguration.Create(CreateOmitted: Boolean; AGlobalSettings: TBaseGlobalSettings;
	ALocalSettings: TBaseLocalSettings; AStartupSettings: TBaseStartSettings; AUserPrefs: TBaseUserPreferences;
	AUserSettings: TBaseUserSettings);
{ {
  CreateOmitted : Caso algum dos parametros seja omitido um padrao sera criado para ele.

  AGlobalSettings : Instancia de TBaseGlobalSettings.

  ALocalSettings : Instancia de TBaseLocalSettings

  AStartupSettings : Instancia de TBaseStartupSettings

  AUserPrefs : Instancia de TBaseUserPreferences.

  AUserSettings : Instancia de TBaseUserSettings.
}
begin
  FOwnedData := False;
  if CreateOmitted then begin
	if AGlobalSettings = nil then begin
	  AGlobalSettings := TBaseGlobalSettings.Create;
	end;
	if ALocalSettings = nil then begin
	  ALocalSettings := TBaseLocalSettings.Create;
	end;
	if AStartupSettings = nil then begin
	  AStartupSettings := TBaseStartSettings.Create; // Ini em memoria
	end;
	if AUserPrefs = nil then begin
	  AUserPrefs := TBaseUserPreferences.Create;
	end;
	if AUserSettings = nil then begin
	  AUserSettings := TBaseUserSettings.Create;
	end;
  end;
  FGlobalSettings := AGlobalSettings;
  FLocalSettings := ALocalSettings;
  FStartupSettings := AStartupSettings;
  FUserPrefs := AUserPrefs;
  FUserSettings := AUserSettings;
end;

{ ---------------------------------------------------------------------------------------------------------------------------------- }
destructor TBaseConfiguration.Destroy;
{ {
  Este destrutor checa OwnedData, para o caso de verdadeiro as instancias de
  GlobalSettings, LocalSettings, StartupSettings, UserPrefs e UserSettings serao destruidas junto.
}
begin
  if (Self.FOwnedData) then begin
	FGlobalSettings.Free;
	FLocalSettings.Free;
	FStartupSettings.Free;
	FUserPrefs.Free;
	FUserSettings.Free;
  end;
  inherited;
end;

{ ---------------------------------------------------------------------------------------------------------------------------------- }
procedure TBaseConfiguration.SetUserPrefs(const Value: TBaseUserPreferences);
{ {
  Troca instancia de acesso as preferencias do usuário pela nova.
  Caso Self.OwnedData verdadeiro a instancia antiga sera destruida.
}
begin
  if (Value <> Self.FUserPrefs) then begin
	if (Self.FOwnedData) then begin
	  FreeAndNil(Self.FUserPrefs);
	end;
	FUserPrefs := Value;
  end;
end;

end.
