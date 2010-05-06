{$IFDEF WinTrayIcon}
	{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I WinSysLib.inc}

{ Diretivas originais A+,B-,C-,D-,E-,F-,G+,H+,I+,J+,K-,L-,M-,N+,O+,P+,Q+,R+,S-,T-,U-,V+,W-,X+,Y-,Z1}


{ TTrayIcon VCL. Version 1.3

  Requires:  Delphi 2.0 32 bit.

  One strange anomaly that I worked around but don't know why it happens -
  if a ToolTip is not specified, then at run-time the icon shows up as
  blank.  If a ToolTip is specified, everything works fine.  To fix this,
  I set up another windows message that set the tool tip if it was blank -
  this ensures proper operation at all times, but I don't know why this
  is necessary.  If you can figure it out, send me some mail and let me
  know! (4/17/96 note - still no solution for this!)

  Pete Ness
  Compuserve ID: 102347,710
  Internet: 102347.710@compuserve.com
  http:\\ourworld.compuserve.com\homepages\peteness

  4/17/96 - Version 1.3
     Added a PopupMenu property to automatically handle right clicking on
     the tray icon.
     Fixed bug that would not allow you to instantiate a TTrayIcon instance
     at run-time.
     Added an example program to show how to do some of the things I've
      gotten the most questions on.
      This version is available from my super lame web page - see above for
      the address.

  }



unit WinTrayIcon;

interface

uses
	SysUtils, Windows, Messages, Classes, Graphics, Controls, ShellAPI, Forms, menus, Super;


const
	TRAY_WINDOW_CLASS_NAME = 'Shell_TrayWnd';


type

	TTrayIcon = Class (TComponent)
	private
		IconData : TNOTIFYICONDATA;
		FIcon : TIcon;
		FToolTip : String;
		FWindowHandle : HWND;
		FActive : boolean;
		FShowDesigning : Boolean;
		FOnClick : TNotifyEvent;
		FOnDblClick : TNotifyEvent;
		FOnRightClick : TMouseEvent;
		FPopupMenu : TPopupMenu;
		function AddIcon : boolean;
		function DeleteIcon : boolean;
		procedure SetActive(Value : boolean);
		procedure SetShowDesigning(Value : boolean);
		procedure SetIcon(Value : TIcon);
		procedure SetToolTip(Value : String);
		procedure WndProc(var msg : TMessage);
		procedure FillDataStructure;
		procedure DoRightClick(Sender : TObject);
	protected
		FTrayWindowHandle : THandle;
	public
		property Handle : HWnd read FWindowHandle;
		constructor Create(aOwner : TComponent); override;
		destructor Destroy; override;
		function ModifyIcon : boolean;
	published
		property Active : Boolean read FActive write SetActive;
		property ShowDesigning : Boolean read FShowDesigning write SetShowDesigning;
		property Icon : TIcon read FIcon write SetIcon;
		property ToolTip : string read FTooltip write SetToolTip;
		property OnClick : TNotifyEvent read FOnClick write FOnClick;
		property OnDblClick : TNotifyEvent read FOnDblClick write FOnDblClick;
		property OnRightClick : TMouseEvent read FOnRightClick write FonRightClick;
		property PopupMenu : TPopupMenu read FPopupMenu write FPopupMenu;
	end;


function FindTrayWindowHandle() : HWND;

procedure Register;


implementation

uses
	WinHnd;

procedure TTrayIcon.SetActive(Value : boolean);
//----------------------------------------------------------------------------------------------------------------------------------
var
	CurSystray : THandle;
begin
	if not (csdesigning in ComponentState) then begin
		if Value then begin //Deseja-se lancar o tray
			CurSystray := GetDeskTopWindow();
			if (CurSystray <> 0) then begin
				CurSysTray := FindWindowChildByClass(CurSystray, TRAY_WINDOW_CLASS_NAME);
				if CurSysTray <> 0 then begin
					if CurSysTray <> Self.FTrayWindowHandle then begin
						Self.Active := FALSE;
					end;
					FActive := AddIcon;
					if FActive then begin
						Self.FTrayWindowHandle := CurSysTray;
					end;
				end else begin
					Self.Active := FALSE;
				end;
			end else begin //Nao existe possibilidade do tray ser lancado
				Self.Active := FALSE;
			end;
		end else begin  //Deseja-se remover o tray
			if FActive then begin
				FActive := not (DeleteIcon);
			end;
		end;
	end else begin
		FActive := Value;
	end;

	{  //Versao original
    if Value <> FActive then begin
        if not (csdesigning in ComponentState) then begin
            if Value then begin
                FActive := AddIcon;
            end else begin
                FActive := not ( DeleteIcon ); //if DeleteIcon=>FActive=False
            end;
        end else begin
            FActive :=Value;
        end;
    end;
    }
end;

{---------------------------------------------------------------------------------------------}
procedure TTrayIcon.SetShowDesigning(Value : boolean);
begin
	if csdesigning in ComponentState then begin
		if value <> fShowDesigning then begin
			fShowDesigning := Value;
			if Value then begin
				AddIcon;
			end else begin
				DeleteIcon;
			end;
		end;
	end;
end;

{---------------------------------------------------------------------------------------------}
procedure TTrayIcon.SetIcon(Value : Ticon);
begin
	if Value <> fIcon then begin
		FIcon.Assign(value);
		ModifyIcon;
	end;
end;

{---------------------------------------------------------------------------------------------}
procedure TTrayIcon.SetToolTip(Value : string);
begin

	// This routine ALWAYS re-sets the field value and re-loads the
	// icon.  This is so the ToolTip can be set blank when the component
	// is first loaded.  If this is changed, the icon will be blank on
	// the tray when no ToolTip is specified.

	if length(Value) > 62 then	   begin
		Value := copy(Value, 1, 62);
	end;
	FToolTip := value;
	ModifyIcon;

end;

constructor TTrayIcon.Create(aOwner : Tcomponent);
	//----------------------------------------------------------------------------------------------------------------------------------
begin
	inherited create(aOwner);
	FTrayWindowHandle := 0; //Identificador da area de notificacao do momento da criacao do trayicon
	FWindowHandle := Classes.AllocateHWnd(WndProc);
	FIcon := TIcon.Create;
	Self.FActive := FALSE;
end;

{---------------------------------------------------------------------------------------------}
destructor TTrayIcon.Destroy;
begin
	if (not (csDesigning in ComponentState) and fActive) or ((csDesigning in ComponentState) and fShowDesigning) then	begin
		DeleteIcon;
	end;
	FIcon.Free;
	Classes.DeAllocateHWnd(FWindowHandle);
	inherited Destroy;
end;

{---------------------------------------------------------------------------------------------}
procedure TTrayIcon.FillDataStructure;
begin
	with IconData do begin
		cbSize := sizeof(TNOTIFYICONDATA);
		wnd := FWindowHandle;
		uID := 0; // is not passed in with message so make it 0
		uFlags := NIF_MESSAGE + NIF_ICON + NIF_TIP;
		hIcon := FIcon.Handle;
		StrPCopy(szTip, fToolTip);
		uCallbackMessage := UM_TOOLTRAYICON;
	end;
end;

{---------------------------------------------------------------------------------------------}
function TTrayIcon.AddIcon : boolean;
begin
	FillDataStructure;
	Result := Shell_NotifyIcon(NIM_ADD, @IconData);
	// For some reason, if there is no tool tip set up, then the icon
	// doesn't display.  This fixes that.
	if FToolTip = '' then begin
		PostMessage(fWindowHandle, UM_TRAYRESETTOOLTIP, 0, 0);
	end;

end;

function TTrayIcon.ModifyIcon : boolean;
	{-------------------------------------------------------------------------------------------------------------}
begin
	FillDataStructure;
	if fActive then begin
		Result := Shell_NotifyIcon(NIM_MODIFY, @IconData);
	end else begin
		Result := TRUE;
	end;
end;

procedure TTrayIcon.DoRightClick(Sender : TObject);
{-------------------------------------------------------------------------------------------------------------}
var
	MouseCo : Tpoint;
begin

	GetCursorPos(MouseCo);

	if Assigned(FPopupMenu) then begin
		SetForegroundWindow(Application.Handle);
		Application.ProcessMessages;
		fPopupmenu.Popup(Mouseco.X, Mouseco.Y);
	end;

	if Assigned(FOnRightClick) then begin
		FOnRightClick(Self, mbRight, [], MouseCo.x, MouseCo.y);
	end;
end;

function TTrayIcon.DeleteIcon : boolean;
	//----------------------------------------------------------------------------------------------------------------------------------
begin
	Result := Shell_NotifyIcon(NIM_DELETE, @IconData);
end;

{---------------------------------------------------------------------------------------------}
procedure TTrayIcon.WndProc(var msg : TMessage);
begin
	case Msg.Msg of
		WM_USERCHANGED, WM_ENDSESSION	: begin
			Self.Active := FALSE;
		end;
		else begin
			with msg do begin
				if (msg = UM_TRAYRESETTOOLTIP) then begin
					SetToolTip(fToolTip);
				end else begin
					if (msg = UM_TOOLTRAYICON) then begin
						case lParam of
							WM_LBUTTONDBLCLK	: begin
								if assigned(FOnDblClick) then begin
									FOnDblClick(Self);
								end;
							end;
							WM_LBUTTONUP	: begin
								if assigned(FOnClick) then begin
									FOnClick(Self);
								end;
							end;
							WM_RBUTTONUP	: begin
								DoRightClick(Self);
							end;
						end;
					end else begin // Handle all messages with the default handler
						Result := DefWindowProc(FWindowHandle, Msg, wParam, lParam);
					end;
				end;
			end;
		end;
	end;
end;


function FindTrayWindowHandle() : HWND;
	//----------------------------------------------------------------------------------------------------------------------
	//Localiza o Handle da janela para a area de notificacao do Windows
begin
	Result := GetDeskTopWindow();
	if (Result <> 0) then begin
		Result := FindWindowChildByClass(Result, TRAY_WINDOW_CLASS_NAME);
	end;
end;

procedure Register;
//----------------------------------------------------------------------------------------------------------------------
begin
	RegisterComponents('Super', [TTrayIcon]);
end;

end.


