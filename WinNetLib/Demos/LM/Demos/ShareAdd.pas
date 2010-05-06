unit ShareAdd;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, LmClasses;

type
  TShareAddDlg = class(TForm)
    OkBtn: TButton;
    CancelBtn: TButton;
    Bevel1: TBevel;
    NetNameEdit: TEdit;
    PathEdit: TEdit;
    RemarkEdit: TEdit;
    ROpasswordEdit: TEdit;
    RWpasswordEdit: TEdit;
    VerifyROpasswordEdit: TEdit;
    VerifyRWpasswordEdit: TEdit;
    ShareTypeRadioGroup: TRadioGroup;
    Button1: TButton;
    ROpassLabel: TLabel;
    RWpassLabel: TLabel;
    GroupBox1: TGroupBox;
    SHI50F_RDONLYBox: TCheckBox;
    SHI50F_FULLBox: TCheckBox;
    SHI50F_PERSISTBox: TCheckBox;
    SHI50F_SYSTEMBox: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    ROverifyLabel: TLabel;
    RWverifyLabel: TLabel;
    ClearBtn: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure NetNameEditChange(Sender: TObject);
    procedure ClearBtnClick(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
  private
    FShareItem: TNetShareItem;
    procedure SetShareItem(const Value: TNetShareItem);
    procedure SetControlsFromInfo(ShareItem: TNetShareItem);
  public
//    procedure SetControlsFromInfo(const NetName: String);
    property ShareItem: TNetShareItem read FShareItem write SetShareItem;
  end;

var
  ShareAddDlg: TShareAddDlg;

implementation

{$R *.DFM}

uses
  SvrApi, LmErr, LmCons, LmUtils, NetWatchMain, FileCtrl;

const
  ShareTypes: array[0..3] of DWORD =
    (STYPE_DISKTREE, STYPE_PRINTQ, STYPE_DEVICE, STYPE_IPC);

procedure TShareAddDlg.FormCreate(Sender: TObject);
begin
  NetNameEdit.MaxLength := LM20_NNLEN;
  PathEdit.MaxLength := MAX_PATH;
  ROpasswordEdit.MaxLength := SHPWLEN;
  RWpasswordEdit.MaxLength := SHPWLEN;
  VerifyROpasswordEdit.MaxLength := SHPWLEN;
  VerifyRWpasswordEdit.MaxLength := SHPWLEN;
  NetNameEditChange(nil);
end;

procedure TShareAddDlg.Button1Click(Sender: TObject);
var
  S: String;
begin
  if SelectDirectory('', '', S) then
    PathEdit.Text := S;
end;

procedure TShareAddDlg.NetNameEditChange(Sender: TObject);
var
  IsOK: Boolean;
begin
  IsOK := (Length(Trim(NetNameEdit.Text)) > 0) and (Length(PathEdit.Text) > 0);
  if ROpasswordEdit.Text <> VerifyROpasswordEdit.Text then
  begin
    IsOK := False;
    ROpassLabel.Font.Color := clRed;
  end else
    ROpassLabel.Font.Color := clWindowText;
  ROverifyLabel.Font.Color := ROpassLabel.Font.Color;
  if RWpasswordEdit.Text <> VerifyRWpasswordEdit.Text then
  begin
    IsOK := False;
    RWpassLabel.Font.Color := clRed;
  end else
    RWpassLabel.Font.Color := clWindowText;
  RWverifyLabel.Font.Color := RWpassLabel.Font.Color;
  OKBtn.Enabled := IsOk;
end;

{procedure TShareAddDlg.SetControlsFromInfo(const NetName: String);
var
  ShareInfo: PShareInfo50;
  BufferSize, TotalAvail: Word;
  Res: NET_API_STATUS;
  I: Integer;
begin
  ZeroMemory(@ShareInfo, Sizeof(ShareInfo));
  Res := NetShareGetInfo(PChar(MainForm.ServerName), PChar(NetName), 50, ShareInfo, 0, TotalAvail);
  if Res <> NERR_BufTooSmall then Exit;
  BufferSize := TotalAvail;
  GetMem(ShareInfo, BufferSize);
  try
    NetCheck(NetShareGetInfo(PChar(MainForm.ServerName), PChar(NetName), 50, ShareInfo, BufferSize, TotalAvail));
    with ShareInfo^ do
    begin
      NetNameEdit.Text := shi50_netname;
      PathEdit.Text := shi50_path;
      RemarkEdit.Text := shi50_remark;
      for I := Low(ShareTypes) to High(ShareTypes) do
        if ShareTypes[I] = shi50_type then
        begin
          ShareTypeRadioGroup.ItemIndex := I;
          Break;
        end;
      SHI50F_RDONLYBox.Checked := shi50_flags and SHI50F_RDONLY <> 0;
      SHI50F_FULLBox.Checked := shi50_flags and SHI50F_FULL <> 0;
      SHI50F_PERSISTBox.Checked := shi50_flags and SHI50F_PERSIST <> 0;
      SHI50F_SYSTEMBox.Checked := shi50_flags and SHI50F_SYSTEM <> 0;
    end;
  finally
    FreeMem(ShareInfo);
  end;
end;}

procedure TShareAddDlg.ClearBtnClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to ControlCount - 1 do
    if Controls[I] is TEdit then TEdit(Controls[I]).Text := '';
  ShareTypeRadioGroup.ItemIndex := 0;
  SHI50F_RDONLYBox.Checked := False;
  SHI50F_FULLBox.Checked := True;
  SHI50F_PERSISTBox.Checked := True;
  SHI50F_SYSTEMBox.Checked := False;
end;

procedure TShareAddDlg.SetControlsFromInfo(ShareItem: TNetShareItem);
var
  I: Integer;
begin
  with ShareItem do
  begin
    NetNameEdit.Text := ShareName;
    PathEdit.Text := Path;
    RemarkEdit.Text := Remark;
    for I := Low(ShareTypes) to High(ShareTypes) do
      if ShareTypes[I] = ShareType then
      begin
        ShareTypeRadioGroup.ItemIndex := I;
        Break;
      end;
    SHI50F_RDONLYBox.Checked := ShareFlags and SHI50F_RDONLY <> 0;
    SHI50F_FULLBox.Checked := ShareFlags and SHI50F_FULL <> 0;
    SHI50F_PERSISTBox.Checked := ShareFlags and SHI50F_PERSIST <> 0;
    SHI50F_SYSTEMBox.Checked := ShareFlags and SHI50F_SYSTEM <> 0;
  end;
end;

procedure TShareAddDlg.SetShareItem(const Value: TNetShareItem);
begin
  FShareItem := Value;
  SetControlsFromInfo(FShareItem);
end;

procedure TShareAddDlg.OkBtnClick(Sender: TObject);
{var
  ShareInfo: TShareInfo50;
  Path, Remark: String;
begin
  ZeroMemory(@ShareInfo, Sizeof(ShareInfo));
  Path := Trim(PathEdit.Text);
  Remark := RemarkEdit.Text;
  with ShareInfo do
  begin
    StrPCopy(shi50_netname, Trim(NetNameEdit.Text));
    shi50_type := ShareTypes[ShareTypeRadioGroup.ItemIndex];
    if SHI50F_RDONLYBox.Checked then shi50_flags := shi50_flags or SHI50F_RDONLY;
    if SHI50F_FULLBox.Checked then shi50_flags := shi50_flags or SHI50F_FULL;
    if SHI50F_PERSISTBox.Checked then shi50_flags := shi50_flags or SHI50F_PERSIST;
    if SHI50F_SYSTEMBox.Checked then shi50_flags := shi50_flags or SHI50F_SYSTEM;
    shi50_remark := PChar(Remark);
    shi50_path := PChar(Path);
    StrPCopy(shi50_rw_password, RWpasswordEdit.Text);
    StrPCopy(shi50_ro_password, ROpasswordEdit.Text);
  end;
  NetShareDel(PChar(MainForm.ServerName), ShareInfo.shi50_netname, 0); // needed for edit
  NetCheck(NetShareAdd(PChar(MainForm.ServerName), 50, @ShareInfo, Sizeof(ShareInfo)));
  ModalResult := mrOk;}
begin
  with ShareItem do
  begin
    BeginUpdate;
    ShareName := NetNameEdit.Text;
    ShareType := ShareTypes[ShareTypeRadioGroup.ItemIndex];
    if SHI50F_RDONLYBox.Checked then ShareFlags := ShareFlags or SHI50F_RDONLY;
    if SHI50F_FULLBox.Checked then ShareFlags := ShareFlags or SHI50F_FULL;
    if SHI50F_PERSISTBox.Checked then ShareFlags := ShareFlags or SHI50F_PERSIST;
    if SHI50F_SYSTEMBox.Checked then ShareFlags := ShareFlags or SHI50F_SYSTEM;
    Remark := RemarkEdit.Text;
    Path := PathEdit.Text;
    PasswordRW := RWpasswordEdit.Text;
    PasswordRO := ROpasswordEdit.Text;
    EndUpdate;
  end;
end;

end.
