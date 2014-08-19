program NetWatch95;

{%File '..\..\..\Src\LanMan.inc'}

uses
  Forms,
  NetWatchMain in 'NetWatchMain.pas' {MainForm},
  SelectServer in 'SelectServer.pas' {SelectServerDlg},
  About in 'About.pas' {AboutDlg},
  ShareAdd in 'ShareAdd.pas' {ShareAddDlg},
  WNetSever in '..\..\..\Src\WNetSever.pas',
  Svrapi in '..\..\..\Src\SvrApi.pas',
  LmWksta in '..\..\..\Src\LmWksta.pas',
  LmUtils in '..\..\..\Src\LmUtils.pas',
  LmUseflg in '..\..\..\Src\LmUseflg.pas',
  LmUse in '..\..\..\Src\LmUse.pas',
  LmSvc in '..\..\..\Src\LmSvc.pas',
  LmStats in '..\..\..\Src\LmStats.pas',
  LmSname in '..\..\..\Src\LmSname.pas',
  LmShare in '..\..\..\Src\LmShare.pas',
  LmServer in '..\..\..\Src\LmServer.pas',
  LmRepl in '..\..\..\Src\LmRepl.pas',
  LmRemUtl in '..\..\..\Src\LmRemutl.pas',
  LmMsg in '..\..\..\Src\LmMsg.pas',
  LmJoin in '..\..\..\Src\LmJoin.pas',
  LmErrlog in '..\..\..\Src\LmErrlog.pas',
  LmErr in '..\..\..\Src\LmErr.pas',
  LmDfs in '..\..\..\Src\LmDfs.pas',
  Lmcons in '..\..\..\Src\LmCons.pas',
  LmConfig in '..\..\..\Src\LmConfig.pas',
  LmClasses in '..\..\..\Src\LmClasses.pas',
  LmChdev in '..\..\..\Src\LmChdev.pas',
  LmBrowsr in '..\..\..\Src\LmBrowsr.pas',
  LmAudit in '..\..\..\Src\LmAudit.pas',
  LmAt in '..\..\..\Src\LmAt.pas',
  LmApibuf in '..\..\..\Src\LmApibuf.pas',
  LmAlert in '..\..\..\Src\LmAlert.pas',
  LmAccess in '..\..\..\Src\LmAccess.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'NetWatch95';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
