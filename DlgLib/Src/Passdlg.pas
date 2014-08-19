{$IFDEF Passdlg}
    {$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I DlgLib.inc}

unit Passdlg;

interface

uses WinTypes, WinProcs, Forms, Controls, StdCtrls, Buttons, Classes, Str_Pas,
	SysUtils;

type
	TOnCheckPasswordEvent = procedure(var UserPwd, CorrectPwd : string; var Accepted : boolean) of object;

	TPassDlg = Class (TComponent)
	private
		FTitle : string;
		FMsgLabel : string;
		FCorrectPassword : string;
		FPassword : string;
		FInitialChars : byte;
		FPassWordChar : char;
		FMaxLength : integer;
		FCaseSensitive : Boolean;
		FMaxRetries : integer;
		FOnCheckPassword : TOnCheckPasswordEvent;
	public
		constructor Create(AOwner : TComponent); override;
		destructor Destroy; override;
		function Execute : boolean;
		property PassWord : string read FPassWord;
	published
		property Title : string read FTitle write FTitle;
		property MsgLabel : string read FMsgLabel write FMsgLabel;
		property CorrectPassword : string read FCorrectPassWord write FCorrectPassword;
		property PassWordChar : char read FPassWordChar write FPassWordChar default '#';
		property InitialChars : byte read FInitialChars write FInitialChars default 10;
		property MaxLength : integer read FMaxLength write FMaxLength default 0;
		property CaseSensitive : Boolean read FCaseSensitive write FCaseSensitive default TRUE;
		property MaxRetries : integer read FMaxRetries write FMaxRetries default 0;
		property OnCheckPassword : TOnCheckPasswordEvent read FOnCheckPassword write FOnCheckPassword;
	end;

type
	TPasswordDlg = Class (TForm)
		PassMsg : TLabel;
		Pass : TEdit;
		OKBtn : TBitBtn;
		CancelBtn : TBitBtn;
	private
		{ Private declarations }
	public
		{ Public declarations }
	end;

implementation

{$R *.DFM}

var
	PasswordDlg : TPasswordDlg;


constructor TPassDlg.Create(AOwner : TComponent);
	//----------------------------------------------------------------------------------------------------------------------
begin
	inherited Create(AOwner);
	FTitle := 'Entrada de Senha';
	FMsgLabel := 'Digite sua senha:';
	FCorrectPassword := EmptyStr;
	FInitialChars := 3;
	FPassWordChar := '#';
	FMaxLength := 0;
	FCaseSensitive := FALSE;
	FMaxRetries := 0;
end;

destructor TPassDlg.Destroy;
	//----------------------------------------------------------------------------------------------------------------------
begin
	inherited Destroy;
end;

function TPassDlg.Execute : boolean;
	//----------------------------------------------------------------------------------------------------------------------
var
	Retries : integer;
begin
	Application.CreateForm(TPassWordDlg, PassWordDlg);
	with PassWordDlg do begin
		Caption := FTitle;
		Pass.MaxLength := FMaxLength;
		Pass.PassWordChar := FPassWordChar;
		Pass.Text := StringOfChar(FPassWordChar, FInitialChars);
		PassMsg.Caption := FMsgLabel;
		if (PassMsg.Left + PassMsg.Width) > Width then begin
			Width := (PassMsg.Left + PassMsg.Width) + 15;
			Pass.Width := PassMsg.Width;
		end;
	end;
	//Ajusta tamanho de acordo com comprimento da mensagem de Label
	Result := FALSE;
	Retries := 0;
	repeat
		Inc(Retries);
		if PassWordDlg.ShowModal = mrOK then begin
			FPassWord := PassWordDlg.Pass.Text;
			if FCorrectPassWord <> '' then begin
				if FCaseSensitive then begin
					Result := (FPassWord = FCorrectPassWord);
				end else begin
					Result := (UpperCase(FPassWord) = UpperCase(FCorrectPassWord));
				end;
			end else begin
				Result := TRUE;
			end;
			//Chama evento se este existir
			if (Assigned(FOnCheckPassword)) then begin
				Self.FOnCheckPassword(FPassword, FCorrectPassword, Result);
			end;
		end else begin
			FPassWord := EmptyStr;
			Break;
		end;
	until (Result) or ((Retries >= FMaxRetries) and (FMaxRetries <> 0));
	PassWordDlg.Free;
end;

end.


