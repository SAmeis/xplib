{$IFDEF WinStealth}
{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I WinSysLib.inc}
//NOTA:
// ****** Nao habilitar o Range Checking ***********
// ****************** NOTA IMPORTANTE : Devemos setar Application.ShowMainForm:=False; antes de criar o MainForm

unit WinStealth;

interface

uses
	Windows, Classes, Forms, SysUtils, Controls, Messages, Dialogs;

type
	EDuplicateComponent = class(Exception);
	EFormNotOwner       = class(Exception);

	TStealth = class(TComponent)
	private
		FHideForm:  Boolean;
		FHideApp:   Boolean;
		OldWndProc: TFarProc;
		NewWndProc: Pointer;
		procedure SetHideForm(Value: Boolean);
		procedure SetHideApp(Value: Boolean);
		procedure HookParent;
		procedure UnhookParent;
		procedure HookWndProc(var Message: TMessage);
	protected
		{ Protected declarations }
		{$IFNDEF WINNT}
		procedure HideApplication;
		procedure ShowApplication;
		{$ENDIF}
	public
		{ Public declarations }
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
		procedure Loaded; override;
		procedure ProcessEnabled(LParam: LParam);
	published
		{ Published declarations }
		property HideForm: Boolean read FHideForm write SetHideForm stored TRUE default TRUE;
		property HideApp:  Boolean read FHideApp write SetHideApp;
	end;

procedure Register;

implementation

uses
	WinHnd;

procedure Register;
//----------------------------------------------------------------------------------------------------------------------
begin
	RegisterComponents('Super', [TStealth]);
end;

constructor TStealth.Create(AOwner: TComponent);
//----------------------------------------------------------------------------------------------------------------------
var
	i:         Word;
	CompCount: Byte;
begin
	// ********** NOTA IMPORTANTE ********: Devemos setar Application.ShowMainForm:=False; antes de criar o MainForm
	inherited Create(AOwner);
	FHideForm := TRUE;
	NewWndProc := nil;
	OldWndProc := nil;
	CompCount := 0;
	if (csDesigning in ComponentState) then begin
		if AOwner.InheritsFrom(TForm) then begin //TForm como ancestral
			with (AOwner as TForm) do begin
				for i := 0 to ComponentCount - 1 do begin
					if Components[i] is TStealth then begin
						Inc(CompCount);
					end;
				end;
				if CompCount > 1 then begin
					raise EDuplicateComponent.Create('Existe uma instância anterior deste componente');
				end;
			end;
		end else begin
			raise EFormNotOwner.Create('Este componente só pode ser colocado em TForm');
		end;
	end else begin
		HookParent;
	end;
end;

destructor TStealth.Destroy;
begin
	{$IFNDEF WINNT}
	ShowApplication;
	{$ENDIF}
	UnhookParent;
	inherited Destroy;
end;

procedure TStealth.SetHideApp(Value: Boolean);
//------------------------------------------------------------------------------------------------------------
begin
	FHideApp := Value;
	{$IFNDEF WINNT}
	if Value then begin
		HideApplication;
	end else begin
		ShowApplication;
	end;
	{$ENDIF}
end;

{$IFNDEF WINNT}

procedure TStealth.HideApplication;
{ ------------------------------------------------------------------------------------------------------------ }
begin
	if not(csDesigning in ComponentState) then begin
		//RegisterServiceProcess(GetCurrentProcessID, 1);
		RegisterServiceProcess(0, 1);
	end;
end;

procedure TStealth.ShowApplication;
{ ------------------------------------------------------------------------------------------------------------ }
begin
	if not(csDesigning in ComponentState) then begin
		//RegisterServiceProcess(GetCurrentProcessID, 0);
		RegisterServiceProcess(0, 0);
	end;
end;

{$ENDIF}

procedure TStealth.Loaded;
{ ------------------------------------------------------------------------------------------------------------ }
begin
	inherited Loaded; { Always call inherited Loaded method }
	if not(csDesigning in ComponentState) then begin
		ProcessEnabled(0);
	end;
end;

procedure TStealth.ProcessEnabled(LParam: LParam);
{ ------------------------------------------------------------------------------------------------------------ }
begin
	if LParam <> SW_PARENTOPENING then begin //Inserido este parametro par aevitar rechamada
		if not(csDesigning in ComponentState) then begin
			if FHideForm then begin
				TForm(Owner).Hide;
				//ShowWindow( TForm(Owner).Handle, SW_HIDE)
			end else begin
				TForm(Owner).Show;
				//ShowWindow(TForm(Owner).Handle, SW_RESTORE);
			end;
		end;
	end;
end;

{ ------------------------------------------------------------------------------------------------------------ }
procedure TStealth.SetHideForm(Value: Boolean);
begin
	FHideForm := Value;
	ProcessEnabled(0);
end;

{ ------------------------------------------------------------------------------------------------------------- }
procedure TStealth.HookParent;
begin
	if (Owner = nil) then begin
		Exit;
	end;
	OldWndProc := TFarProc(GetWindowLong((Owner as TForm).Handle, GWL_WNDPROC));
	NewWndProc := Classes.MakeObjectInstance(HookWndProc);
	SetWindowLong((Owner as TForm).Handle, GWL_WNDPROC, LongInt(NewWndProc));
end;

procedure TStealth.UnhookParent;
{ ------------------------------------------------------------------------------------------------------------ }
begin
	if (Owner <> nil) and Assigned(OldWndProc) then begin
		SetWindowLong((Owner as TForm).Handle, GWL_WNDPROC, LongInt(OldWndProc));
	end;
	if Assigned(NewWndProc) then begin
		Classes.FreeObjectInstance(NewWndProc);
	end;
	NewWndProc := nil;
	OldWndProc := nil;
end;

{ ------------------------------------------------------------------------------------------------------------- }
procedure TStealth.HookWndProc(var Message: TMessage);
begin
	if Owner = nil then begin
		Exit;
	end;
	if NewWndProc <> TFarProc(GetWindowLong((Owner as TForm).Handle, GWL_WNDPROC)) then begin
		MessageDlg('Falha ao registrar Hook para classe de janela( TStealth )', mtError, [mbOK], 0);
	end;
	if (message.Msg = WM_SHOWWINDOW) then begin
		if (message.wParam <> 0) then begin
			ProcessEnabled(SW_PARENTOPENING);
		end;
	end;
	TForm(Owner).Dispatch(message);
	//Chamada original do fonte do componente abaixo
	//Message.Result := CallWindowProc(OldWndProc, TForm(Owner).Handle, Message.Msg, Message.wParam, Message.lParam);
end;

end.
