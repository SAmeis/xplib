{$IFDEF DosMove }
	{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I ECLib.inc}



{{
 TDosMove ver 1.02

 Last updated at: 09/10/1998

 Component that allows you to move thourgh the controls in your app with UP/DOWN arrows or ENTER key insted of using the old boring TAB.

 Code by: Liran Shahar Israel simpletech@ibm.net
}

unit DosMove;

interface

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	StdCtrls;

type
	TMoveOptions = set of (moEnter, moUpDn);

	TDosMove = Class (TComponent)
	private
		FActive : boolean;
		FOptions : TMoveOptions;
		FEditNoBeep : boolean;
		FOwnerKeyDown : TKeyEvent;
		FOwnerKeyPress : TKeyPressEvent;
		FLastWasEdit : boolean;
	protected
		procedure NewKeyDown(Sender : TObject; var Key : word; Shift : TShiftState);
		procedure NewKeyPress(Sender : Tobject; var Key : char);
		procedure ValidateContainer(AComponent: TComponent); override;
	public
		constructor Create(AOwner : TComponent); override;
	published
		property Active : boolean read FActive write FActive default FALSE;
		property Options : TMoveOptions read FOptions write FOptions default [moEnter, moUpDn];
		property EditNoBeep : boolean read FEditNoBeep write FEditNoBeep default TRUE;
	end;


implementation

//-----------------------------------------------------------------------------
constructor TDosMove.Create(AOwner : TComponent);
var
	Loop : integer;
begin
	// First check to see no other TDosMove exists on the form
	if (csDesigning in Self.ComponentState) then begin
		for Loop := 0 to AOwner.ComponentCount - 1 do begin
			if AOwner.Components[Loop] is TDosMove then begin
				raise EInvalidOperation.Create('TDosMove - pode haver apenas uma instância por Form');
			end;
		end;
	end;

	// Create component and set default properties
	inherited Create(AOwner);
	FActive := FALSE;
	FOptions := [moEnter, moUpDn];
	FEditNoBeep := TRUE;

	// Intercept with OnKeyDown event and OnKeyPress event of 'Owner'
	TForm(AOwner).KeyPreview := TRUE;
	FOwnerKeyDown := TForm(AOwner).OnKeyDown;
	TForm(AOwner).OnKeyDown := NewKeyDown;
	FOwnerKeyPress := TForm(AOwner).OnKeyPress;
	TForm(AOwner).OnKeyPress := NewKeyPress;

end;

procedure TDosMove.NewKeyDown(Sender : TObject; var Key : word; Shift : TShiftState);
{{
Novo evento
}
begin
	{ TODO -oRoger -cLIB : Filtrar qdo o controle for descendentes de Grid }
	if FActive then begin	// true if last active control is TCustomEdit and above
		FLastWasEdit := (Owner as TForm).ActiveControl is TCustomEdit;

		if (FOptions <> []) then begin	// Handle the specials keys
			if ((Key = VK_DOWN) and (moUpDn in FOptions)) or ((Key = VK_RETURN) and (moEnter in FOptions)) then begin
				TForm(Owner).Perform(WM_NEXTDLGCTL, 0, 0);
			end else begin
				if (Key = VK_UP) and (moUpDn in FOptions) then begin
					TForm(Owner).Perform(WM_NEXTDLGCTL, 1, 0);
				end;
			end;
		end;
	end;
	// Call owner OnKeyDown if it's assigned
	if assigned(FOwnerKeyDown) then begin
		FOwnerKeyDown(Sender, Key, Shift);
	end;
end;

procedure TDosMove.NewKeyPress(Sender : TObject; var Key : char);
{--------------------------------------------------------------------------------------------------------------------------------}
begin
	if FActive then begin	// Handle 'Enter' key that makes Edits beep
		if FEditNoBeep and FLastWasEdit and (Key = #13) then begin
			Key := #0;
		end;
	end;
	// Call owner OnKeyPress if it's assigned
	if assigned(FOwnerKeyPress) then begin
		FOwnerKeyPress(Sender, Key);
	end;
end;

procedure TDosMove.ValidateContainer(AComponent: TComponent);
begin
	inherited;
	if ( not( AComponent is TForm )) then begin
		Abort;
	end;
end;

end.


