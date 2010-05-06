program ShellDialogTest;

uses
  Forms,
  ShellDialogTestForm in 'ShellDialogTestForm.pas' {fmShellDialogTest},
  DBHnd in '..\..\DBLib\Src\DBHnd.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Shell Dialogs Test';
  Application.CreateForm(TfmShellDialogTest, fmShellDialogTest);
  Application.Run;
end.
