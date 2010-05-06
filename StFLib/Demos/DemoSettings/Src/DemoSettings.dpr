program DemoSettings;

uses
  Forms,
  DemoSettingsMainForm in 'DemoSettingsMainForm.pas' {Form1},
  ConfigurationLayer in 'ConfigurationLayer.pas',
  AppSettings in 'AppSettings.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
