{$IFDEF Logndlg}
{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I DlgLib.inc}
unit Logndlg;

interface

uses
	WinTypes, WinProcs, Classes, Graphics, Forms, Controls, StdCtrls, Buttons, SysUtils;

type
	TPropText = TCaption;

	TLoginDlgs = class(TForm)
		Label1: TLabel;
		PasswordEdit: TEdit;
		OKBtn: TBitBtn;
		CancelBtn: TBitBtn;
		UserNameEdit: TEdit;
		Label2: TLabel;
		procedure FormShow(Sender: TObject);
	private
		{ Private declarations }
	public
		{ Public declarations }
	end;

	TLoginDlg          = class;
	TOnCheckLoginEvent = procedure(Sender: TLoginDlg; var UserName, Password: string; var Accepted: boolean) of object;

	TLoginDlg = class(TComponent)
	private
		FTitle           : TPropText;
		FInitUserName    : TPropText;
		FUserName        : TPropText;
		FPass            : TPropText;
		Dlg              : TLoginDlgs;
		FUserNameCharCase: TEditCharCase;
		FPasswordCharCase: TEditCharCase;
		FOnCheckLogin    : TOnCheckLoginEvent;
		FMaxRetries      : integer;
		FRetries         : integer;
		FInitPwd         : TPropText;
		FCancelled       : boolean;
		procedure SetUserName(const Value: TPropText);
		procedure SetInitUserName(const Value: TPropText);
		procedure SetInitPwd(const Value: TPropText);
		function GetActive: boolean;
	public
		constructor Create(AOwner: TComponent); override;
		procedure Cancel;
		function Execute: boolean;
		property UserName: TPropText read FUserName write SetUserName;
		{ {
		  Nome do usuário em processo de login e validado
		}
		property Password: TPropText read FPass write FPass;
		property Retries: integer read FRetries;
	published
		property Cancelled: boolean read FCancelled;
		{ {
		  Indica que a falha de login foi gerada pelo cancelamento do usuário
		}
		property Active: boolean read GetActive;
		{ {
		  Flag indicando que o dialogo está ativo
		}
		property MaxRetries: integer read FMaxRetries write FMaxRetries;
		{ {
		  Quantidade maxima de tentativas permitidas, o valor padrão é 3. Quando Retries >= MaxRetries o resultado do login e dado como falso
		}
		property UserNameCharCase: TEditCharCase read FUserNameCharCase write FUserNameCharCase;
		{ {
		  Qual CharCase será usado para edição do nome do usuário.
		}
		property OnCheckLogin: TOnCheckLoginEvent read FOnCheckLogin write FOnCheckLogin;
		{ {
		  Evento de validação do login.
		  procedure(Sender : TLoginDlg; var UserName, Password : string; var Accepted : boolean);
		  Ajuste Accepted para true para validar positivamente o login.
		}
		property PasswordCharCase: TEditCharCase read FPasswordCharCase write FPasswordCharCase;
		{ {
		  Charcase da senha.
		}
		property Title: TPropText read FTitle write FTitle;
		{ {
		  Titulo a ser exibido no dialogo de captura da senha e nome do usuário.
		}
		property InitPwd: TPropText read FInitPwd write SetInitPwd;
		{ {
		  Valor da senha inicialmente preenchida no dialogo.
		}
		property InitUserName: TPropText read FInitUserName write SetInitUserName;
		{ {
		  Nome do usuario inicialmente preenchido.
		}
	end;

implementation

{$R *.DFM}

procedure TLoginDlg.Cancel;
{ {
  Cancela a execução do dialogo de captura do par senha/usuario e ajusta Cancelled para true

  Revision: 19/7/2005
}
begin
	Self.FCancelled := True;
	if (Assigned(Self.Dlg)) then begin
		Self.Dlg.Close;
	end;
end;

constructor TLoginDlg.Create(AOwner: TComponent);
{ {
  Construtor padrão de um componente e inicialização de atributos padrão.

  Revision: 19/7/2005
}
begin
	inherited Create(AOwner);
	Self.FMaxRetries   := 3;
	Self.FInitUserName := EmptyStr;
end;

function TLoginDlg.Execute: boolean;
{ {
  Executa o dialogo de captura do par usuário/senha de acordo com os parametros ajustados

  Revision: 19/7/2005

  Ajustados os valores de Self.Password e Self.Username para o conteudo final do dialogo de modo a compatibilizar com os programas
  antigos que não usavam o evento OnValidate deste componente.

  Revision: 18/11/2005 - Roger
}
var
	UName, Pwd: string;
begin
	Self.FCancelled := False;
	Dlg             := TLoginDlgs.Create(Self.Owner);
	try
		Dlg.Caption               := FTitle;
		Dlg.UserNameEdit.CharCase := Self.FUserNameCharCase;
		Dlg.PasswordEdit.CharCase := Self.FPasswordCharCase;
		if FInitUserName <> EmptyStr then begin
			Dlg.UserNameEdit.Text := FInitUserName; //Caso o Charcase <> nornal converso sera realizada
			if Dlg.UserNameEdit.Text = EmptyStr then begin
				Dlg.ActiveControl := Dlg.UserNameEdit;
			end else begin
				Dlg.ActiveControl := Dlg.PasswordEdit;
			end;
		end;
		Application.Restore;
		Application.BringToFront;
		FRetries := 0;
		while (Self.Active) do begin
			if (Dlg.ShowModal = mrOk) then begin
				UName := Dlg.UserNameEdit.Text;
				Pwd   := Dlg.PasswordEdit.Text;
				if (Assigned(Self.FOnCheckLogin)) then begin
					Self.FOnCheckLogin(Self, UName, Pwd, Result);
					if (Result) then begin //Tudo OK -> Sair da execucao
						Dlg.UserNameEdit.Text := UName;
						Dlg.PasswordEdit.Text := Pwd;
						//Repassa valores dos controles aos atributos da instancia que podem ter sido modificados no evento de validação
						Self.UserName := UName;
						Self.Password := Pwd;
						Break;
					end else begin //Tentar novamente se permitido
						Inc(Self.FRetries);
						if (Self.Retries >= Self.FMaxRetries) then begin //Tentativas permitidas falharam
							Break;
						end;
					end;
				end else begin //Não existe validacao -> sempre sera aceito como OK
					Self.UserName := UName;
					Self.Password := Pwd;
					Result        := True;
					Break;
				end;
				//Repassa valores dos controles aos atributos da instancia que podem ter sido modificados no evento de validação
				Self.UserName := UName;
				Self.Password := Pwd;
			end else begin
				Self.FCancelled := True;
				Break;
			end;
		end;
	finally
		FreeAndNil(Dlg);
	end;
end;

procedure TLoginDlgs.FormShow(Sender: TObject);
{ {
  Evento disparado no momento da exibição do dialogo de captura do par senha/usuário

  Revision: 19/7/2005
}
begin
	Application.BringToFront;
	Application.ProcessMessages;
	SetForeGroundWindow(Self.Handle);
end;

function TLoginDlg.GetActive: boolean;
{ {
  Flag que indica se o dialogo está em execução
}
begin
	Result := Assigned(Self.Dlg);
end;

procedure TLoginDlg.SetInitPwd(const Value: TPropText);
{ {
  Ajusta o valor incial da senha do dialogo.

  Revision: 19/7/2005
}
begin
	FInitPwd := Value;
	if (Self.Active) then begin
		Self.Dlg.PasswordEdit.Text := Value;
	end;
end;

procedure TLoginDlg.SetInitUserName(const Value: TPropText);
{ {
  Ajusta o valor do nome do usuário em processo de login

  Revision: 19/7/2005
}
begin
	Self.FInitUserName := Value;
	if (Assigned(Self.Dlg)) then begin
		Self.Dlg.UserNameEdit.Text := Value;
	end;
end;

procedure TLoginDlg.SetUserName(const Value: TPropText);
{ {
  Ajusta o nome do usuário em processo de login, bem como o valor inicial exibido no dialgo.

  Revision: 19/7/2005
}
begin
	Self.FUserName    := Value;
	Self.InitUserName := Value; //Mais coerente para uma chamada posterior a Execute deste componente
end;

end.
