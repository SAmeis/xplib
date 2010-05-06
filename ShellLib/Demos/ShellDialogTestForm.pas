//Teste de revisao2
//E agora ?
//Esta deveria ser uma versao copiada do Master Copy *****
//Cade as alteracoes????
unit ShellDialogTestForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, shlobj, ShellShlDlg;

type
  TfmShellDialogTest = class(TForm)
    btnShellAbout: TBitBtn;
    dlgShellAbout: TkbShellAboutDialog;
    dlgBrowseFolders: TkbBrowseForFolderDialog;
    btnBrowseFolders: TBitBtn;
    dlgPickIcon: TkbPickIconDialog;
    btnPickIcon: TBitBtn;
    btnFindFiles: TBitBtn;
    dlgFindFiles: TkbFindFilesDialog;
    btnExitWindows: TBitBtn;
    btnFormatDrive: TBitBtn;
    dlgFormatDrive: TkbFormatDriveDialog;
    btnRestart: TBitBtn;
    dlgRestartWindows: TkbRestartWindowsDialog;
    dlgObjectProperties: TkbObjectPropertiesDialog;
    btnObjectProperties: TBitBtn;
    dlgRunFile: TkbRunFileDialog;
    btnRunFile: TBitBtn;
    btnFindComputer: TBitBtn;
    btnShellMessageBox: TBitBtn;
    btnHandleDiskFill: TBitBtn;
    btnNetConnection: TBitBtn;
    btnOutOfMemory: TBitBtn;
    procedure btnShellAboutClick(Sender: TObject);
    procedure btnBrowseFoldersClick(Sender: TObject);
    procedure dlgBrowseFoldersChange(Sender: TObject; var Path,
      StatusText: String; var OKState: TkbOKState);
    procedure btnPickIconClick(Sender: TObject);
    procedure btnFindFilesClick(Sender: TObject);
    procedure btnExitWindowsClick(Sender: TObject);
    procedure btnFormatDriveClick(Sender: TObject);
    procedure btnRestartClick(Sender: TObject);
    procedure btnObjectPropertiesClick(Sender: TObject);
    procedure btnRunFileClick(Sender: TObject);
    procedure dlgRunFileValidate(Sender: TObject; TheFile,
      TheWorkPath: String; Visible: Boolean; var Action: TkbRunFileAction);
    procedure btnFindComputerClick(Sender: TObject);
    procedure btnShellMessageBoxClick(Sender: TObject);
    procedure btnHandleDiskFillClick(Sender: TObject);
    procedure btnNetConnectionClick(Sender: TObject);
    procedure btnOutOfMemoryClick(Sender: TObject);
  end;

var
  fmShellDialogTest: TfmShellDialogTest;

implementation

{$R *.DFM}

procedure TfmShellDialogTest.btnShellAboutClick(Sender: TObject);
begin
  Self.dlgShellAbout.Execute;
end;

procedure TfmShellDialogTest.btnBrowseFoldersClick(Sender: TObject);
begin
  Self.dlgBrowseFolders.Execute;
end;

procedure TfmShellDialogTest.dlgBrowseFoldersChange(Sender: TObject;
  var Path, StatusText: String; var OKState: TkbOKState);
begin
  StatusText := Path;
end;

procedure TfmShellDialogTest.btnPickIconClick(Sender: TObject);
begin
  dlgPickIcon.Execute;
  MessageDlg('FileName: ' + dlgPickIcon.FileName + #13#10 + ' Icon Index: ' + IntToStr(dlgPickIcon.IconIndex),
             mtInformation, [mbOK], 0);
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

procedure TfmShellDialogTest.dlgRunFileValidate(Sender: TObject; TheFile,
  TheWorkPath: String; Visible: Boolean; var Action: TkbRunFileAction);
begin
  case (MessageDlg('Run file ' + TheFile + '?', mtConfirmation, [mbYes, mbCancel, mbRetry], 0)) of
    mrYes:   Action := kbsdRun;
    mrRetry: Action := kbsdRetry;
    else     Action := kbsdCancel;
  end; {case}
end;

procedure TfmShellDialogTest.btnFindComputerClick(Sender: TObject);
begin
  ShowFindComputerDialog;
end;

procedure TfmShellDialogTest.btnShellMessageBoxClick(Sender: TObject);
var
  Arg1: String;
  Arg2: String;
begin
  Arg1 := 'This is the first inclusion string.';
  Arg2 := 'This is the second inclusion string.';
  ShowShellMessageBox('Sample Caption', 'Argument 1: %1' + #13#10 + 'Argument 2: %2' + #13#10 +'Argument 3: %3',
                      MB_ICONINFORMATION or MB_OK, [PChar(Arg1), PChar(Arg2), PChar('This is the third inclusion string.')]);
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

