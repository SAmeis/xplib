program DlgLibMasterDemo;

uses
  Forms,
  DlgDemoMainForm in 'DlgDemoMainForm.pas' {DlgDemoMainFrm},
  Passdlg in '..\..\..\Src\Passdlg.pas' {PasswordDlg},
  Logndlg in '..\..\..\Src\Logndlg.pas' {LoginDlgs},
  Chgpwd in '..\..\..\Src\Chgpwd.pas' {ChgPwdDlg};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TDlgDemoMainFrm, DlgDemoMainFrm);
  Application.Run;
end.
