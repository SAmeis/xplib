{$IFDEF Chgpwd}
    {$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I DlgLib.inc}

unit Chgpwd;

interface


uses
	WinTypes, WinProcs, Classes, Graphics, Forms, Controls, Buttons, StdCtrls;

type
	TChgPwdDlg = Class (TForm)
		OKBtn : TBitBtn;
		CancelBtn : TBitBtn;
		HelpBtn : TBitBtn;
		Label1 : TLabel;
		Label2 : TLabel;
		Label3 : TLabel;
		OldPassEdit : TEdit;
		NewPassEdit : TEdit;
		ConfPassEdit : TEdit;
	private
		{ Private declarations }
	public
		{ Public declarations }
	end;

type
	TChgPwd = Class ;
	TOnCheckChangePwdEvent = procedure(Sender : TChgPwd; var Accepted : boolean) of object;

	TChgPwd = Class (TComponent)
	private
		FOldPass : string;
		FNewPassword : string;
		FNewPass1, FNewPass2 : string;
		FCharsFilled : integer;
		FMaxLength : integer;
		FMaxRetries : integer;
		FCaseSensitive : boolean;
		FAllowBlank : boolean;
		FHelpContext : THelpContext;
		FOnCheckPwdChange : TOnCheckChangePwdEvent;
		FEnabled : boolean;
		FRetries : integer;
		procedure SetMaxRetries(Value : integer);
		function GetIdentical : boolean;
	public
		constructor Create(AOwner : TComponent); override;
		procedure Abort;
		function Execute : boolean;
		property Enabled : boolean read FEnabled write FEnabled;
		property NewPass1 : string read FNewPass1 write FNewPass1;
		property NewPass2 : string read FNewPass2 write FNewPass2;
		property Identical : boolean read GetIdentical;
		property OldPass : string read FOldPass write FOldPass;
		property NewPassword : string read FNewPassword write FNewPassword;
		property Retries : integer read FRetries write FRetries;
	published
		property AllowBlank : boolean read FAllowBlank write FAllowBlank default TRUE;
		property CharsFilled : integer read FCharsFilled write FCharsFilled default 0;
		property MaxLength : integer read FMaxLength write FMaxLength default 0;
		property OnCheckPwdChange : TOnCheckChangePwdEvent read FOnCheckPwdChange write FOnCheckPwdChange;
		property HelpContext : THelpContext read FHelpContext write FHelpContext default 0;
		property CaseSensitive : boolean read FCaseSensitive write FCaseSensitive default TRUE;
		property MaxRetries : integer read FMaxRetries write SetMaxRetries default 0;
	end;

{var
  {Variavel global de acesso ao FORM foi removida para acesso exclusivo pelo componente
  BtnBottomDlg: TBtnBottomDlg;
  }

implementation

uses
	SysUtils, Str_Pas, Dialogs;

{$R *.DFM}

const
	_MSG_ILEGAL_PASSWORD_ = 'As novas senhas informadas são diferentes.'#13'Tente novamente';
	_MSG_BLANK_PASSWORD_  = 'Senha nula inválida';
	_MSG_INVALID_OLDPASSWORD_ = 'Senha anterior incorreta';

procedure TChgPwd.SetMaxRetries(Value : integer);
//----------------------------------------------------------------------------------------------------------------------
begin
	if Value < 0 then begin
		SysUtils.Abort;
	end;
	FMaxRetries := Value;
end;

constructor TChgPwd.Create(AOwner : TComponent);
	//----------------------------------------------------------------------------------------------------------------------
begin
	inherited Create(AOWner);
	FEnabled := TRUE;
	FCharsFilled := 0;
	FMaxLength := 0;
	FAllowBlank := TRUE;
	FCaseSensitive := TRUE;
end;

function TChgPwd.GetIdentical : boolean;
	//----------------------------------------------------------------------------------------------------------------------
begin
	Result := (FNewPass1 = FNewPass2);
end;

function TChgPwd.Execute : boolean;
	//----------------------------------------------------------------------------------------------------------------------
var
	Dlg : TChgPwdDlg;
begin
	(*Procurar forma com testes condicionais para a situacao de case sensitive*)
	{Inicializacao do dialogo}
	Application.CreateForm(TChgPwdDlg, Dlg);
	try
		Dlg.HelpContext := Self.HelpContext;
		Dlg.HelpBtn.Visible := (Self.HelpContext <> 0);
		if FOldPass <> EmptyStr then begin
			Dlg.ActiveControl := Dlg.OldPassEdit;
		end else begin
			Dlg.OldPassEdit.Color := clGrayText;
			Dlg.ActiveControl := Dlg.NewPassEdit;
			Dlg.OldPassEdit.Enabled := FALSE;
		end;
		if CharsFilled <> 0 then begin
			Dlg.OldPassEdit.Text := StringOfChar('*', CharsFilled);
		end;
		if MaxLength > 0 then begin
			Dlg.NewPassEdit.MaxLength := MaxLength;
			Dlg.ConfPassEdit.MaxLength := MaxLength;
		end;
		Self.FRetries := 0; {Tentativas usadas}
		{Fim de inicializacao do dialogo }

		Self.FEnabled := TRUE;
		while (Self.FEnabled) do begin
			Result := (Dlg.ShowModal = mrOK); //Executa p/ posterior checagem
			if not (Result) then begin //dialog Cancelado
				Self.Abort();
				Break;
			end;
			//Repassa valores para o componente
			Self.FNewPass1 := Dlg.NewPassEdit.Text;
			Self.FNewPass2 := Dlg.ConfPassEdit.Text;
			Self.FOldPass  := Dlg.OldPassEdit.Text;

			//Teste de senha em branco
			if (not (AllowBlank)) and (Dlg.NewPassEdit.Text = EmptyStr) then begin
				MessageBeep(MB_OK);
				MessageDlg(_MSG_BLANK_PASSWORD_, mtError, [mbOK], FHelpContext);
				Dlg.ActiveControl := Dlg.NewPassEdit;
				System.Continue;
			end;

			if GetIdentical then begin //Senha confirmada na repeticao -> OK
				if FCaseSensitive then begin
					Self.NewPassword := FNewPass1;
				end else begin
					Self.NewPassword := UpperCase(FNewPass1);
				end;
			end else begin //Repeticao diferente
				MessageBeep(MB_OK);
				MessageDlg(_MSG_ILEGAL_PASSWORD_, mtError, [mbOK], FHelpContext);
				Dlg.ActiveControl := Dlg.NewPassEdit;
				System.Continue;
			end;

			if (Assigned(Self.FOnCheckPwdChange)) then begin //Chamar evento
				Self.FOnCheckPwdChange(Self, Result);
				Dlg.NewPassEdit.Text := Self.FNewPass1;
				Dlg.ConfPassEdit.Text := Self.FNewPass2;
				Dlg.OldPassEdit.Text := Self.FOldPass;
				Result := Self.FEnabled;  //Repassa este valor, se Abortado pelo callback -> Result = False;
			end else begin //Sempre assume falha de alteracao -> sem validacao
				Result := FALSE;
				Exit;
			end;

			//Passando aqui -> validacao ocorreu!
			if FMaxRetries <> 0 then begin
				Inc(Self.FRetries);
				if Self.FRetries > FMaxRetries then begin
					//Condicao de erro por excesso de tentativas
					Self.Abort();
				end;
			end;
		end;
	finally //Termina dialogo
		Dlg.Free;
	end;
end;

procedure TChgPwd.Abort;
//----------------------------------------------------------------------------------------------------------------------
begin
	Self.FEnabled := FALSE;
end;

end.


