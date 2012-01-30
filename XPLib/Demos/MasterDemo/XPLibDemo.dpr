program XPLibDemo;

uses
  Forms,
  MasterDemoForm in 'MasterDemoForm.pas' {MasterDemoFrm},
  CtrlsHnd in '..\..\Src\CtrlsHnd.pas',
  AppLog in '..\..\src\AppLog.pas',
  TestFileHnd in 'FileHnd\TestFileHnd.pas' {FileHndDemoForm},
  FileHnd in '..\..\Src\FileHnd.pas',
  WinRegDemo in 'WinRegistry\WinRegDemo.pas' {FormWinRegDemo},
  WinReg32 in '..\..\Src\WinReg32.pas',
  TestCtrlsHnd in 'CtrlsHnd\TestCtrlsHnd.pas' {CtrlsHndDemoFrm},
  AppLogDemoForm in 'AppLog\AppLogDemoForm.pas' {AppLogDemoFrm},
  XPTypes in '..\..\Src\XPTypes.pas',
  Registry in 'c:\program files\embarcadero\rad studio\7.0\source\Win32\rtl\common\Registry.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMasterDemoFrm, MasterDemoFrm);
  Application.CreateForm(TCtrlsHndDemoFrm, CtrlsHndDemoFrm);
  Application.CreateForm(TAppLogDemoFrm, AppLogDemoFrm);
  Application.Run;
end.
