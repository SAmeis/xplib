program XPLibDemo;

uses
  Forms,
  MasterDemoForm in 'MasterDemoForm.pas' {MasterDemoFrm},
  TestCtrlsHnd in 'CtrlsHnd\TestCtrlsHnd.pas' {CtrlsHndDemoFrm},
  CtrlsHnd in '..\..\Src\CtrlsHnd.pas',
  AppLogDemoForm in 'AppLog\AppLogDemoForm.pas' {AppLogDemoFrm},
  AppLog in '..\..\src\AppLog.pas',
  TestFileHnd in 'FileHnd\TestFileHnd.pas' {FileHndDemoForm},
  FileHnd in '..\..\Src\FileHnd.pas',
  WinRegDemo in 'WinRegistry\WinRegDemo.pas' {FormWinRegDemo},
  WinReg32 in '..\..\Src\WinReg32.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMasterDemoFrm, MasterDemoFrm);
  Application.Run;
end.
