program TestHotkeyManager;

uses
  Forms,
  Main in 'Main.pas' {MainForm},
  WinHotKey in '..\..\..\..\..\..\Pcks\WinSysLib\Src\WinHotKey.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'HotKeyManager test';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
