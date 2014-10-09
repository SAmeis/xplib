{$IFDEF Logndlg}
{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I DlgLib.inc}
unit LognDBdlg;

{ {
}
interface

uses
	Classes, SysUtils, Windows, Forms, Controls, StdCtrls, Buttons;

type

	TLoginDBDlgForm = class(TForm)
		CancelBtn: TBitBtn;
		Label1: TLabel;
		Label2: TLabel;
		Label3: TLabel;
		OKBtn: TBitBtn;
		PasswordEdit: TEdit;
		UnitName: TComboBox;
		UserNameEdit: TEdit;
		procedure FormShow(Sender: TObject);
	end;

	TDBLoginDlg          = class;
	TOnCheckDBLoginEvent = procedure(Sender: TDBLoginDlg; var UnitID: integer; var UserName, Password: string;
		var Accepted: boolean) of object;
	TOnGetDBLoginUnitNames = procedure(Sender: TDBLoginDlg; List: TStrings; var DefaultUnitId: integer) of object;

	{ {
	  A lista das unidades de negócio é carregada com o nome longo para seleção do operador.
	  IMPORTANTE: Cada entrada DEVE ser inserida via AddObject() com o valor da unidade sendo o seu Id.
	}
	{ 1 Carrega a lista de nomes longos das unidades de negócio }
	TDBLoginDlg = class(TComponent)
	private
		Dlg              : TLoginDBDlgForm;
		FActive          : boolean;
		FInitUnitID      : integer;
		FInitUserName    : string;
		FMaxRetries      : integer;
		FOnCheckLogin    : TOnCheckDBLoginEvent;
		FOnGetUnitNames  : TOnGetDBLoginUnitNames;
		FPass            : string;
		FPasswordCharCase: TEditCharCase;
		FRetries         : integer;
		FTitle           : string;
		FUnitID          : integer;
		FUserName        : string;
		FUserNameCharCase: TEditCharCase;
		procedure SetUnitID(const Value: integer);
		procedure SetUserName(const Value: string);
	public
		constructor Create(AOwner: TComponent); override;
		procedure Cancel;
		function Execute: boolean;
		property Password: string read FPass write FPass;
		property Retries: integer read FRetries;
		property UnitID: integer read FUnitID write SetUnitID;
		property UserName: string read FUserName write SetUserName;
	published
		property Active        : boolean read FActive;
		property InitUnitID    : integer read FInitUnitID write FInitUnitID;
		property InitUserName  : string read FInitUserName write FInitUserName;
		property MaxRetries    : integer read FMaxRetries write FMaxRetries;
		property OnCheckLogin  : TOnCheckDBLoginEvent read FOnCheckLogin write FOnCheckLogin;
		property OnGetUnitNames: TOnGetDBLoginUnitNames read FOnGetUnitNames write FOnGetUnitNames;
		{ {
		  Evento para a carga dos nomes das unidades de negócio nas quais o operador pode realizar seu login, bem como o seu valor inicial(
		  em termos do id da unidade ).
		}
		property PasswordCharCase: TEditCharCase read FPasswordCharCase write FPasswordCharCase;
		property Title           : string read FTitle write FTitle;
		property UserNameCharCase: TEditCharCase read FUserNameCharCase write FUserNameCharCase;
	end;

implementation

{$R *.DFM}

uses
	Super, Dialogs;

{ -**********************************************************************
  ************************************************************************
  ******************
  ******************  Class:    TDBLoginDlg
  ******************  Category: No category
  ******************
  ************************************************************************
  ************************************************************************ }
{ -------------------------------------------------------------------------------------------------------------------------------- }
procedure TDBLoginDlg.SetUnitID(const Value: integer);
begin
	Self.FUnitID    := Value;
	Self.InitUnitID := Value; //Mais coerente para uma chamada posterior a Execute deste componente
end;

{ -------------------------------------------------------------------------------------------------------------------------------- }
procedure TDBLoginDlg.SetUserName(const Value: string);

//----------------------------------------------------------------------------------------------------------------------

begin
	Self.FUserName    := Value;
	Self.InitUserName := Value; //Mais coerente para uma chamada posterior a Execute deste componente
end;

{ -------------------------------------------------------------------------------------------------------------------------------- }
procedure TDBLoginDlg.Cancel;

//----------------------------------------------------------------------------------------------------------------------------------

begin
	Self.Dlg.Close;
	Self.FActive := FALSE;
end;

{ -------------------------------------------------------------------------------------------------------------------------------- }
constructor TDBLoginDlg.Create(AOwner: TComponent);

//----------------------------------------------------------------------------------------------------------------------------------

begin
	inherited Create(AOwner);
	FMaxRetries   := 3;
	FInitUserName := EmptyStr;
	FActive       := FALSE;
end;

{ -------------------------------------------------------------------------------------------------------------------------------- }
function TDBLoginDlg.Execute: boolean;
var
	UName, Pwd: string;
	defUnitID : integer;

	//----------------------------------------------------------------------------------------------------------------------------------

begin
	Dlg       := TLoginDBDlgForm.Create(Self.Owner);
	defUnitID := -1;
	if (Assigned(Self.FOnGetUnitNames)) then begin
		Self.FOnGetUnitNames(Self, Dlg.UnitName.Items, Self.FInitUnitID);
	end;
	if (Self.FInitUnitID >= 0) then begin
		defUnitID := Dlg.UnitName.Items.IndexOfObject(Pointer(Self.FInitUnitID));
	end else begin
		defUnitID := -1;
	end;
	Dlg.UnitName.ItemIndex    := defUnitID;
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
	try
		FRetries     := 1;
		Self.FActive := TRUE;
		while (Self.FActive) do begin
			if (Dlg.ShowModal = mrOK) then begin
				if (Dlg.UnitName.ItemIndex < 0) then begin
					MessageDlg('Selecione a unidade de negócio desejada!', mtError, [mbOK], 0);
					System.Continue;
				end;
				{$WARN UNSAFE_CAST OFF}
				defUnitID := integer(Dlg.UnitName.Items.Objects[Dlg.UnitName.ItemIndex]); //Busca pelo valor de Objects
				{$WARN UNSAFE_CAST ON}
				if (Assigned(Self.FOnCheckLogin)) then begin
					UName := Dlg.UserNameEdit.Text;
					Pwd   := Dlg.PasswordEdit.Text;
					Self.FOnCheckLogin(Self, defUnitID, UName, Pwd, Result);
					if (Result) then begin
						Dlg.UserNameEdit.Text := UName;
						Dlg.PasswordEdit.Text := Pwd;
						Self.FActive          := FALSE;
					end else begin
						Inc(Self.FRetries);
						Self.FActive := Self.FMaxRetries >= Self.Retries;
					end;
				end else begin
					Self.FActive := FALSE;
					Result       := TRUE;
				end;
				Self.UserName := UName;
				Self.Password := Pwd;
				Self.UnitID   := defUnitID;
			end else begin
				Self.FActive := FALSE;
				Result       := FALSE;
			end;
		end;
	finally
		Self.FActive := FALSE;
	end;
	Dlg.Free;
end;

{ -**********************************************************************
  ************************************************************************
  ******************
  ******************  Class:    TLoginDBDlgForm
  ******************  Category: No category
  ******************
  ************************************************************************
  ************************************************************************ }
{ -------------------------------------------------------------------------------------------------------------------------------- }
procedure TLoginDBDlgForm.FormShow(Sender: TObject);

//----------------------------------------------------------------------------------------------------------------------------------

begin
	Application.BringToFront;
	Application.ProcessMessages;
	SetForeGroundWindow(Self.Handle);
end;

end.
