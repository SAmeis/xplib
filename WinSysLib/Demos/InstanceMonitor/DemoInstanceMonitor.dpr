program DemoInstanceMonitor;

uses
  Forms,
  InstanceMonitorMainForm in 'InstanceMonitorMainForm.pas' {Form1},
  InstanceMonitor in '..\..\Src\InstanceMonitor.pas',
  BomeOneInstance in '..\..\Doc\InstanceMonitor\Demos\BomeOneInstance.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
