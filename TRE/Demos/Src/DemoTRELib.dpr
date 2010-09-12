program DemoTRELib;

uses
  Forms,
  MainDemoForm in 'MainDemoForm.pas' {Form1},
  JvAppStorage in '..\..\..\Jedi\jvcl\run\JvAppStorage.pas',
  JvAppXMLStorage in '..\..\..\Jedi\jvcl\run\JvAppXMLStorage.pas',
  TREZones in '..\..\Src\TREZones.pas',
  OPXMLSerializable in '..\..\..\OPLib\Src\OPXMLSerializable.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
