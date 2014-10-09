unit ShellDialogTestForm;

interface

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	StdCtrls, Buttons, shlobj, ShellShlDlg, ShellPIDL;

type
	TfmShellDialogTest = class(TForm)
    btnShellFileProps: TBitBtn;
    flpndlgSelFile: TFileOpenDialog;
    procedure btnShellFilePropsClick(Sender: TObject);
	public
		dlgShellAbout      : TkbShellAboutDialog;
		dlgBrowseFolders   : TkbBrowseForFolderDialog;
		dlgPickIcon        : TkbPickIconDialog;
		dlgRestartWindows  : TkbRestartWindowsDialog;
		dlgObjectProperties: TkbObjectPropertiesDialog;
		dlgFindFiles       : TkbFindFilesDialog;
		dlgRunFile         : TkbRunFileDialog;
		dlgFormatDrive     : TkbFormatDriveDialog;
	published
		btnShellAbout      : TBitBtn;
		btnBrowseFolders   : TBitBtn;
		btnPickIcon        : TBitBtn;
		btnFindFiles       : TBitBtn;
		btnExitWindows     : TBitBtn;
		btnFormatDrive     : TBitBtn;
		btnRestart         : TBitBtn;
		btnObjectProperties: TBitBtn;
		btnRunFile         : TBitBtn;
		btnFindComputer    : TBitBtn;
		btnShellMessageBox : TBitBtn;
		btnHandleDiskFill  : TBitBtn;
		btnNetConnection   : TBitBtn;
		btnOutOfMemory     : TBitBtn;
		procedure btnShellAboutClick(Sender: TObject);
		procedure btnBrowseFoldersClick(Sender: TObject);
		procedure dlgBrowseFoldersChange(Sender: TObject; var Path, StatusText: string; var OKState: TkbOKState);
		procedure btnPickIconClick(Sender: TObject);
		procedure btnFindFilesClick(Sender: TObject);
		procedure btnExitWindowsClick(Sender: TObject);
		procedure btnFormatDriveClick(Sender: TObject);
		procedure btnRestartClick(Sender: TObject);
		procedure btnObjectPropertiesClick(Sender: TObject);
		procedure btnRunFileClick(Sender: TObject);
		procedure dlgRunFileValidate(Sender: TObject; TheFile, TheWorkPath: string; Visible: boolean; var Action: TkbRunFileAction);
		procedure btnFindComputerClick(Sender: TObject);
		procedure btnShellMessageBoxClick(Sender: TObject);
		procedure btnHandleDiskFillClick(Sender: TObject);
		procedure btnNetConnectionClick(Sender: TObject);
		procedure btnOutOfMemoryClick(Sender: TObject);
		procedure FormCreate(Sender: TObject);
	end;

var
	fmShellDialogTest: TfmShellDialogTest;

implementation

uses
  ShellFilesHnd, TypInfo;

{$R *.DFM}

procedure TfmShellDialogTest.btnShellAboutClick(Sender: TObject);
begin
	Self.dlgShellAbout.Execute;
end;

procedure TfmShellDialogTest.btnShellFilePropsClick(Sender: TObject);
var
	exeType : TExecutableType;
	enumValue : string;
begin
	if ( Self.flpndlgSelFile.Execute ) then begin
		exeType := ShellFilesHnd.GetExeFileType(  Self.flpndlgSelFile.FileName );
		enumValue := GetEnumName( TypeInfo( TExecutableType ), integer(exeType) );
		MessageDlg( Self.flpndlgSelFile.FileName + '= ' + enumValue,  mtInformation, [mbOK], 0);
	end;
end;

procedure TfmShellDialogTest.btnBrowseFoldersClick(Sender: TObject);
begin
	Self.dlgBrowseFolders.RootPath:='C:\';
	if ( Self.dlgBrowseFolders.Execute ) then begin
		MessageDlg( Self.dlgBrowseFolders.RootPath,  mtInformation, [mbOK], 0);
	end else begin
		MessageDlg('Cancelado?' + SysErrorMessage( GetLastError() ),  mtInformation, [mbOK], 0);
	end;
end;

procedure TfmShellDialogTest.dlgBrowseFoldersChange(Sender: TObject; var Path, StatusText: string; var OKState: TkbOKState);
begin
	StatusText := Path;
end;

procedure TfmShellDialogTest.btnPickIconClick(Sender: TObject);
begin
	dlgPickIcon.Execute;
	MessageDlg('FileName: ' + dlgPickIcon.FileName + #13#10 + ' Icon Index: ' + IntToStr(dlgPickIcon.IconIndex), mtInformation,
		[mbOK], 0);
end;

procedure TfmShellDialogTest.btnFindFilesClick(Sender: TObject);
begin
	dlgFindFiles.Execute;
end;

procedure TfmShellDialogTest.btnExitWindowsClick(Sender: TObject);
begin
	ShowExitWindowsDialog;
end;

procedure TfmShellDialogTest.btnFormatDriveClick(Sender: TObject);
begin
	dlgFormatDrive.Execute;
end;

procedure TfmShellDialogTest.btnRestartClick(Sender: TObject);
begin
	dlgRestartWindows.Execute;
end;

procedure TfmShellDialogTest.btnObjectPropertiesClick(Sender: TObject);
begin
	Self.dlgObjectProperties.Execute;
end;

procedure TfmShellDialogTest.btnRunFileClick(Sender: TObject);
begin
	dlgRunFile.Execute;
end;

procedure TfmShellDialogTest.dlgRunFileValidate(Sender: TObject; TheFile, TheWorkPath: string; Visible: boolean;
	var Action: TkbRunFileAction);
begin
	case (MessageDlg('Run file ' + TheFile + '?', mtConfirmation, [mbYes, mbCancel, mbRetry], 0)) of
		mrYes: begin
				Action := kbsdRun;
			end;
		mrRetry: begin
				Action := kbsdRetry;
			end;
	else begin
			Action := kbsdCancel;
		end;
	end; { case }
end;

procedure TfmShellDialogTest.FormCreate(Sender: TObject);
begin
	Self.dlgShellAbout                     := TkbShellAboutDialog.Create(Self);
	Self.dlgBrowseFolders                  := TkbBrowseForFolderDialog.Create(Self);
	Self.dlgBrowseFolders.CanExpandDomains := True;
	Self.dlgBrowseFolders.Caption          := 'Test Caption';
	Self.dlgBrowseFolders.InstructionText  := 'These are some instructions';
	Self.dlgBrowseFolders.RootFolder       := kbsdPath;
	Self.dlgBrowseFolders.RootPath         := 'C:\';
	Self.dlgBrowseFolders.ShowStatusText   := True;
	Self.dlgBrowseFolders.StatusText       := 'Test Status Text';
	Self.dlgBrowseFolders.OnChange         := dlgBrowseFoldersChange;
	Self.dlgPickIcon                       := TkbPickIconDialog.Create(Self);
	Self.dlgFindFiles                      := TkbFindFilesDialog.Create(Self);
	Self.dlgFindFiles.SearchFileName       := 'poo.*';
	Self.dlgFindFiles.RootFolder           := kbsdPath;
	Self.dlgFindFiles.RootPath             := 'E:\';

	Self.dlgFormatDrive                    := TkbFormatDriveDialog.Create(Self);
	Self.dlgFormatDrive.DefaultQuickFormat := True;
	Self.dlgRestartWindows                 := TkbRestartWindowsDialog.Create(Self);
	Self.dlgRestartWindows.Reason          := 'Something changed!';
	Self.dlgObjectProperties               := TkbObjectPropertiesDialog.Create(Self);
	Self.dlgObjectProperties.ObjectName    := 'C:\';
	Self.dlgRunFile                        := TkbRunFileDialog.Create(Self);
end;

procedure TfmShellDialogTest.btnFindComputerClick(Sender: TObject);
begin
	ShowFindComputerDialog;
end;

procedure TfmShellDialogTest.btnShellMessageBoxClick(Sender: TObject);
var
	Arg1: string;
	Arg2: string;
begin
	Arg1 := 'This is the first inclusion string.';
	Arg2 := 'This is the second inclusion string.';
	ShowShellMessageBox('Sample Caption', 'Argument 1: %1' + #13#10 + 'Argument 2: %2' + #13#10 + 'Argument 3: %3',
		MB_ICONINFORMATION or MB_OK, [PWideChar(Arg1), PWideChar(Arg2), PWideChar('This is the third inclusion string.')]);
end;

procedure TfmShellDialogTest.btnHandleDiskFillClick(Sender: TObject);
begin
	ShowHandleDiskFullDialog(kbsdDriveC);
end;

procedure TfmShellDialogTest.btnNetConnectionClick(Sender: TObject);
begin
	ShowNetConnectionDialog(EmptyStr, kbsdDiskResource);
	ShowNetConnectionDialog(EmptyStr, kbsdPrintResource);
end;

procedure TfmShellDialogTest.btnOutOfMemoryClick(Sender: TObject);
begin
	ShowOutOfMemoryDialog;
end;

end.
