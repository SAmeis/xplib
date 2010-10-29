program DemoTRELib;

uses
  Forms,
  MainDemoForm in 'MainDemoForm.pas' {Form1},
  TREZones in '..\..\Src\TREZones.pas',
  RttiUtils in 'RttiUtils.pas',
  MainDemoUtils in 'MainDemoUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
