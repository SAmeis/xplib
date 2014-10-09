{$I DlgLib.inc}
unit DlgLibDsgnRegister;

interface

uses
	Classes,
	Aboutdlg,
	Logndlg,
	Chgpwd,
	Passdlg,
	PathEdit,
	RadioDlg,
	FileCDlg,
	SimpleProgress,
	ThreadProgressForm,
	LognDBdlg;

procedure Register;

implementation

procedure Register;
//----------------------------------------------------------------------------------------------------------------------------------
begin
	RegisterComponents('Dialogs', [TLoginDlg, TAboutBoxDlg, TChgPwd, TPassDlg, TMultPathEditor, TRadioDlg, TSimpleFileCopy,
		TSimpleProgress, TDBLoginDlg]);
end;

end.
