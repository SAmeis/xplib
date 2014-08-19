{$IFDEF MenuHnd}
	{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I XPLib.inc}

unit MenuHnd;


//* A classe TXPPopupMenu nasceu da necessida de termos popups dinamicos. O original pode manter o items/acellerators ativos
//e isso pode gerar gpf´s


interface

uses
  SysUtils, Classes, Windows, Menus, Messages, Dialogs;


type
	TMenuCloseEvent = procedure(Sender: TMenu; SelectedItem : TMenuItem ) of object;

type
  TXPPopupMenu = class(TPopupMenu)
  private
	 { Private declarations }
	 FOnCloseMenu: TMenuCloseEvent;
  protected
	 { Protected declarations }
  public
	 { Public declarations }
	 procedure Popup(X, Y: Integer); override;
  published
	 { Published declarations }
	 property OnCloseMenu: TMenuCloseEvent read FOnCloseMenu write FOnCloseMenu;
  end;

procedure Register;

implementation

procedure Register;
//----------------------------------------------------------------------------------------------------------------------
begin
  RegisterComponents('Standard', [TXPPopupMenu]);
end;

{ TXPPopupMenu }

procedure TXPPopupMenu.Popup(X, Y: Integer);
//----------------------------------------------------------------------------------------------------------------------
var
	Msg : TMsg;
	i : integer;
	SelectedItem : TMenuItem;
begin
	inherited;
	{ TODO -oRoger -cLIB : Para melhor controle podemos ler a pilha de mensagens e descobrir que dos items foi selecionado e criar novo evento informando-o }
	if Assigned( Self.OnCloseMenu ) then begin  //Identifica qual foi selecionado
       SelectedItem:=nil;
		if PeekMessage( Msg, 0 {??? * }, 0, 0, PM_NOREMOVE ) then begin  //***Nao consegui identificar que P... de janela era a certa -> pegar todas
			if Msg.message = WM_COMMAND then begin
				for i:=0 to Self.Items.Count-1 do begin
					if Self.Items.Items[i].Command = Msg.WParam then begin //Detectado o elemento
						SelectedItem:=Self.Items.Items[i];
						Break;
					end;
				end;
			end;
		end else begin
			//Assume que nada foi selecionado
		end;
		Self.FOnCloseMenu( Self, SelectedItem );
	end;
end;

end.
