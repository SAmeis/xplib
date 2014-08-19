unit DlgDemoMainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Passdlg, Chgpwd, Logndlg, ExtCtrls;

type
  TDlgDemoMainFrm = class(TForm)
    ChgPwd: TChgPwd;
    PassDlg: TPassDlg;
    GroupBox1: TGroupBox;
    PwdCapBtn: TButton;
    CorrectPassword: TLabeledEdit;
    CaseSensitiveCheckBox: TCheckBox;
    GroupBox2: TGroupBox;
    LoginCapBtn: TButton;
    LoginUserNameEdit: TLabeledEdit;
    LoginDlg: TLoginDlg;
    LoginCorrectPassword: TLabeledEdit;
    GroupBox3: TGroupBox;
    ChgPwdBtn: TButton;
    ChgPwdOldPwdEdit: TLabeledEdit;
    ChgPwdNewPwdEdit: TLabeledEdit;
    procedure PassDlgCheckPassword(var UserPwd, CorrectPwd: string ; var Accepted: Boolean);
    procedure PwdCapBtnClick(Sender: TObject);
    procedure LoginCapBtnClick(Sender: TObject);
    procedure LoginDlgCheckLogin(Sender: TLoginDlg; var UserName,
      Password: String; var Accepted: Boolean);
    procedure ChgPwdBtnClick(Sender: TObject);
    procedure ChgPwdCheckPwdChange(Sender: TChgPwd; var Accepted: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DlgDemoMainFrm: TDlgDemoMainFrm;

implementation

uses Math;

{$R *.dfm}

procedure TDlgDemoMainFrm.PassDlgCheckPassword(var UserPwd, CorrectPwd: string ; var Accepted: Boolean);
//----------------------------------------------------------------------------------------------------------------------
begin
   MessageDlg( UserPwd, mtInformation, [ mbOK ], 0 );
   MessageDlg( CorrectPwd, mtInformation, [ mbOK ], 0 );
   Accepted:=not Accepted;
end;

procedure TDlgDemoMainFrm.PwdCapBtnClick(Sender: TObject);
//----------------------------------------------------------------------------------------------------------------------
begin
   Self.PassDlg.CorrectPassword:=Self.CorrectPassword.Text;
   Self.PassDlg.CaseSensitive:=Self.CaseSensitiveCheckBox.Checked;
   if ( Self.PassDlg.Execute ) then begin
     MessageDlg( 'OK', mtInformation, [ mbOK ], 0 );
   end;
end;

procedure TDlgDemoMainFrm.LoginCapBtnClick(Sender: TObject);
//----------------------------------------------------------------------------------------------------------------------
begin
   Self.LoginDlg.UserName:=Self.LoginUserNameEdit.Text;
   Self.LoginDlg.PassWord:=Self.LoginCorrectPassword.Text;
   Self.LoginDlg.InitUserName:=Self.LoginUserNameEdit.Text;
   if ( Self.LoginDlg.Execute ) then begin
     MessageDlg( 'OK', mtInformation, [ mbOK ], 0 );
   end;
end;

procedure TDlgDemoMainFrm.LoginDlgCheckLogin(Sender: TLoginDlg; var UserName, Password: String; var Accepted: Boolean);
//----------------------------------------------------------------------------------------------------------------------
begin
   if ( SameText( UserName, Self.LoginUserNameEdit.Text) and ( SameText( Password, Self.LoginCorrectPassword.Text ))  ) then begin
       Accepted:=True;
   end else begin
       Accepted:=False;
   end;
end;

procedure TDlgDemoMainFrm.ChgPwdBtnClick(Sender: TObject);
//----------------------------------------------------------------------------------------------------------------------
begin
   Self.ChgPwd.OldPass:=ChgPwdNewPwdEdit.Text;
   if ( Self.ChgPwd.Execute ) then begin
     MessageDlg( 'OK', mtInformation, [ mbOK ], 0 );
   end else begin
       MessageDlg( 'ERRRO', mtError, [ mbOK ], 0 );
   end;
end;

procedure TDlgDemoMainFrm.ChgPwdCheckPwdChange(Sender: TChgPwd; var Accepted: Boolean);
//----------------------------------------------------------------------------------------------------------------------
begin
   if ( Sender.NewPassword = EmptyStr ) then begin
       Sender.Abort();
   end else begin
       if ( Sender.NewPassword = Self.ChgPwdNewPwdEdit.Text ) and ( Sender.OldPass = Self.ChgPwdOldPwdEdit.Text )  then begin
         Accepted:=True;
       end else begin
         Accepted:=False;
       end;
   end;
end;

end.
