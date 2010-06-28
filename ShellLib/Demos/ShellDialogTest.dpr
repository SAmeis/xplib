program ShellDialogTest;

uses
  Forms,
  ShellDialogTestForm in 'ShellDialogTestForm.pas' {fmShellDialogTest},
  ShellShlDlg in '..\Src\ShellShlDlg.pas',
  ShellPIDL in '..\Src\ShellPIDL.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Shell Dialogs Test';
  Application.CreateForm(TfmShellDialogTest, fmShellDialogTest);
  Application.Run;
end.
