{$IFDEF XPSpinControls }
  {$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I ECLib.inc}

unit XPSpinControls;

{{
Baseado no trabalho de Igor Popov atualizado em 2002.07.24 version: 1.2


Description: Created for all who needs Value of TSpinEdit to be a float type. I did it Currency type to provide
negative(-) numbers.

http://igp.hut.ru
E-mail: support<at>igp.hut.ru
Site: http://igp.hut.ru
}

interface

uses Windows, Classes, StdCtrls, ExtCtrls, Controls, Messages, SysUtils, Forms, Graphics, Menus, Buttons;

const
	InitRepeatPause = 400;  { pause before repeat timer (ms) }
	RepeatPause = 100;  { pause before hint window displays (ms)}

type

	TNumGlyphs = Buttons.TNumGlyphs;

	TXPTimerSpeedButton = Class ;


	TXPSpinButton = Class (TWinControl)
	private
		FDownButton : TXPTimerSpeedButton;
		FFocusControl : TWinControl;
		FFocusedButton : TXPTimerSpeedButton;
		FOnDownClick : TNotifyEvent;
		FOnUpClick : TNotifyEvent;
		FUpButton : TXPTimerSpeedButton;
		procedure AdjustSize(var W, H : Integer); reintroduce;
		procedure BtnClick(Sender : TObject);
		procedure BtnMouseDown(Sender : TObject; Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
		function CreateButton : TXPTimerSpeedButton;
		procedure SetFocusBtn(Btn : TXPTimerSpeedButton);
		function GetDownGlyph : TBitmap;
		procedure SetDownGlyph(Value : TBitmap);
		function GetDownNumGlyphs : TNumGlyphs;
		procedure SetDownNumGlyphs(Value : TNumGlyphs);
		function GetUpGlyph : TBitmap;
		procedure SetUpGlyph(Value : TBitmap);
		function GetUpNumGlyphs : TNumGlyphs;
		procedure SetUpNumGlyphs(Value : TNumGlyphs);
		procedure WMGetDlgCode(var Message : TWMGetDlgCode); message WM_GETDLGCODE;
		procedure WMKillFocus(var Message : TWMKillFocus); message WM_KILLFOCUS;
		procedure WMSetFocus(var Message : TWMSetFocus); message WM_SETFOCUS;
		procedure WMSize(var Message : TWMSize); message WM_SIZE;
	protected
		procedure KeyDown(var Key : Word; Shift : TShiftState); override;
		procedure Loaded; override;
		procedure Notification(AComponent : TComponent; Operation : TOperation); override;
	public
		constructor Create(AOwner : TComponent); override;
		procedure SetBounds(ALeft, ATop, AWidth, AHeight : Integer); override;
	published
		property Align;
		property Anchors;
		property Constraints;
		property Ctl3D;
		property DownGlyph : TBitmap read GetDownGlyph write SetDownGlyph;
		property DownNumGlyphs : TNumGlyphs read GetDownNumGlyphs write SetDownNumGlyphs default 1;
		property DragCursor;
		property DragKind;
		property DragMode;
		property Enabled;
		property FocusControl : TWinControl read FFocusControl write FFocusControl;
		property OnDownClick : TNotifyEvent read FOnDownClick write FOnDownClick;
		property OnDragDrop;
		property OnDragOver;
		property OnEndDock;
		property OnEndDrag;
		property OnEnter;
		property OnExit;
		property OnStartDock;
		property OnStartDrag;
		property OnUpClick : TNotifyEvent read FOnUpClick write FOnUpClick;
		property ParentCtl3D;
		property ParentShowHint;
		property PopupMenu;
		property ShowHint;
		property TabOrder;
		property TabStop;
		property UpGlyph : TBitmap read GetUpGlyph write SetUpGlyph;
		property UpNumGlyphs : TNumGlyphs read GetUpNumGlyphs write SetUpNumGlyphs default 1;
		property Visible;
	end;

	TXPSpinEdit = Class (TCustomEdit)
	private
		FButton : TXPSpinButton;
		FEditorEnabled : Boolean;
		FIncrement : Currency;
		FMaxValue : Currency;
		FMinValue : Currency;
		function CheckValue(NewValue : Currency) : Currency;
		function GetMinHeight : Integer;
		procedure SetEditRect;
		function GetValue : Currency;
		procedure SetValue(NewValue : Currency);
		procedure CMEnter(var Message : TCMGotFocus); message CM_ENTER;
		procedure CMExit(var Message : TCMExit); message CM_EXIT;
		procedure WMCut(var Message : TWMCut); message WM_CUT;
		procedure WMPaste(var Message : TWMPaste); message WM_PASTE;
		procedure WMSize(var Message : TWMSize); message WM_SIZE;
	protected
		procedure CreateParams(var Params : TCreateParams); override;
		procedure CreateWnd; override;
		procedure DownClick(Sender : TObject); virtual;
		function IsValidChar(Key : Char) : Boolean; virtual;
		procedure KeyDown(var Key : Word; Shift : TShiftState); override;
		procedure KeyPress(var Key : Char); override;
		procedure UpClick(Sender : TObject); virtual;
	public
		constructor Create(AOwner : TComponent); override;
		destructor Destroy; override;
       procedure GetChildren(Proc : TGetChildProc; Root : TComponent); override;
		property Button : TXPSpinButton read FButton;
	published
		property Anchors;
		property AutoSelect;
		property AutoSize;
		property Color;
		property Constraints;
		property Ctl3D;
		property DragCursor;
		property DragMode;
		property EditorEnabled : Boolean read FEditorEnabled write FEditorEnabled default TRUE;
		property Enabled;
		property Font;
		property Increment : Currency read FIncrement write FIncrement;
		property MaxLength;
		property MaxValue : Currency read FMaxValue write FMaxValue;
		property MinValue : Currency read FMinValue write FMinValue;
		property OnChange;
		property OnClick;
		property OnDblClick;
		property OnDragDrop;
		property OnDragOver;
		property OnEndDrag;
		property OnEnter;
		property OnExit;
		property OnKeyDown;
		property OnKeyPress;
		property OnKeyUp;
		property OnMouseDown;
		property OnMouseMove;
		property OnMouseUp;
		property OnStartDrag;
		property ParentColor;
		property ParentCtl3D;
		property ParentFont;
		property ParentShowHint;
		property PopupMenu;
		property ReadOnly;
		property ShowHint;
		property TabOrder;
		property TabStop;
		property Value : Currency read GetValue write SetValue;
		property Visible;
	end;

	TTimeBtnState = set of (tbFocusRect, tbAllowTimer);

	TXPTimerSpeedButton = Class (TSpeedButton)
	private
		FRepeatTimer : TTimer;
		FTimeBtnState : TTimeBtnState;
		procedure TimerExpired(Sender : TObject);
	protected
		procedure MouseDown(Button : TMouseButton; Shift : TShiftState; X, Y : Integer); override;
		procedure MouseUp(Button : TMouseButton; Shift : TShiftState; X, Y : Integer); override;
		procedure Paint; override;
	public
		destructor Destroy; override;
		property TimeBtnState : TTimeBtnState read FTimeBtnState write FTimeBtnState;
	end;


implementation

//Imagens dos botoes dos constroles incluidas neste recurso
{$R XPSpinControls.res }

const
	IMG_RESOURCE_UP_BUTTON = 'XPButtonSpinUp';
	IMG_RESOURCE_DOWN_BUTTON = 'XPButtonSpinDown';

{ TSpinButton }

{-**********************************************************************
************************************************************************
******************
******************  Class:    TXPSpinButton
******************  Category: No category
******************
************************************************************************
************************************************************************}
{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPSpinButton.AdjustSize(var W, H : Integer);
begin
	if (FUpButton = NIL) or (csLoading in ComponentState) then begin
		Exit;
	end;
	if W < 15 then begin
		W := 15;
	end;
	FUpButton.SetBounds(0, 0, W, H div 2);
	FDownButton.SetBounds(0, FUpButton.Height - 1, W, H - FUpButton.Height + 1);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPSpinButton.BtnClick(Sender : TObject);
begin
	if Sender = FUpButton then begin
		if Assigned(FOnUpClick) then begin
			FOnUpClick(Self);
		end;
	end else if Assigned(FOnDownClick) then begin
		FOnDownClick(Self);
	end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPSpinButton.BtnMouseDown(Sender : TObject; Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
begin
	if Button = mbLeft then begin
		SetFocusBtn(TXPTimerSpeedButton(Sender));
		if (FFocusControl <> NIL) and FFocusControl.TabStop and
			FFocusControl.CanFocus and (GetFocus <> FFocusControl.Handle) then begin
			FFocusControl.SetFocus;
		end else if TabStop and (GetFocus <> Handle) and CanFocus then begin
			SetFocus;
		end;
	end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TXPSpinButton.CreateButton : TXPTimerSpeedButton;
begin
	Result := TXPTimerSpeedButton.Create(Self);
	Result.OnClick := BtnClick;
	Result.OnMouseDown := BtnMouseDown;
	Result.Visible := TRUE;
	Result.Enabled := TRUE;
	Result.TimeBtnState := [tbAllowTimer];
	Result.Parent := Self;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPSpinButton.SetFocusBtn(Btn : TXPTimerSpeedButton);
begin
	if TabStop and CanFocus and (Btn <> FFocusedButton) then begin
		FFocusedButton.TimeBtnState := FFocusedButton.TimeBtnState - [tbFocusRect];
		FFocusedButton := Btn;
		if (GetFocus = Handle) then begin
			FFocusedButton.TimeBtnState := FFocusedButton.TimeBtnState + [tbFocusRect];
			Invalidate;
		end;
	end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TXPSpinButton.GetDownGlyph : TBitmap;
begin
	Result := FDownButton.Glyph;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPSpinButton.SetDownGlyph(Value : TBitmap);
begin
	if Value <> NIL then begin
		FDownButton.Glyph := Value;
	end else begin
		FDownButton.Glyph.Handle := LoadBitmap(HInstance, IMG_RESOURCE_DOWN_BUTTON);
		FUpButton.NumGlyphs := 1;
		FDownButton.Invalidate;
	end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TXPSpinButton.GetDownNumGlyphs : TNumGlyphs;
begin
	Result := FDownButton.NumGlyphs;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPSpinButton.SetDownNumGlyphs(Value : TNumGlyphs);
begin
	FDownButton.NumGlyphs := Value;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TXPSpinButton.GetUpGlyph : TBitmap;
begin
	Result := FUpButton.Glyph;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPSpinButton.SetUpGlyph(Value : TBitmap);
begin
	if Value <> NIL then begin
		FUpButton.Glyph := Value;
	end else begin
		FUpButton.Glyph.Handle := LoadBitmap(HInstance, IMG_RESOURCE_UP_BUTTON);
		FUpButton.NumGlyphs := 1;
		FUpButton.Invalidate;
	end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TXPSpinButton.GetUpNumGlyphs : TNumGlyphs;
begin
	Result := FUpButton.NumGlyphs;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPSpinButton.SetUpNumGlyphs(Value : TNumGlyphs);
begin
	FUpButton.NumGlyphs := Value;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPSpinButton.WMGetDlgCode(var Message : TWMGetDlgCode);
begin
	Message.Result := DLGC_WANTARROWS;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPSpinButton.WMKillFocus(var Message : TWMKillFocus);
begin
	FFocusedButton.TimeBtnState := FFocusedButton.TimeBtnState - [tbFocusRect];
	FFocusedButton.Invalidate;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPSpinButton.WMSetFocus(var Message : TWMSetFocus);
begin
	FFocusedButton.TimeBtnState := FFocusedButton.TimeBtnState + [tbFocusRect];
	FFocusedButton.Invalidate;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPSpinButton.WMSize(var Message : TWMSize);
var
	W, H : Integer;
begin
	inherited;

	{ check for minimum size }
	W := Width;
	H := Height;
	AdjustSize(W, H);
	if (W <> Width) or (H <> Height) then begin
		inherited SetBounds(Left, Top, W, H);
	end;
	Message.Result := 0;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPSpinButton.KeyDown(var Key : Word; Shift : TShiftState);
begin
	case Key of
		VK_UP	: begin
			SetFocusBtn(FUpButton);
			FUpButton.Click;
		end;
		VK_DOWN	: begin
			SetFocusBtn(FDownButton);
			FDownButton.Click;
		end;
		VK_SPACE	: begin
			FFocusedButton.Click;
		end;
	end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPSpinButton.Loaded;
var
	W, H : Integer;
begin
	inherited Loaded;
	W := Width;
	H := Height;
	AdjustSize(W, H);
	if (W <> Width) or (H <> Height) then begin
		inherited SetBounds(Left, Top, W, H);
	end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPSpinButton.Notification(AComponent : TComponent; Operation : TOperation);
begin
	inherited Notification(AComponent, Operation);
	if (Operation = opRemove) and (AComponent = FFocusControl) then begin
		FFocusControl := NIL;
	end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
constructor TXPSpinButton.Create(AOwner : TComponent);
begin
	inherited Create(AOwner);
	ControlStyle := ControlStyle - [csAcceptsControls, csSetCaption] + [csFramed, csOpaque];

	FUpButton := CreateButton;
	FDownButton := CreateButton;
	UpGlyph := NIL;
	DownGlyph := NIL;

	Width := 20;
	Height := 25;
	FFocusedButton := FUpButton;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPSpinButton.SetBounds(ALeft, ATop, AWidth, AHeight : Integer);
var
	W, H : Integer;
begin
	W := AWidth;
	H := AHeight;
	AdjustSize(W, H);
	inherited SetBounds(ALeft, ATop, W, H);
end;

{-**********************************************************************
************************************************************************
******************
******************  Class:    TXPSpinEdit
******************  Category: No category
******************
************************************************************************
************************************************************************}
{--------------------------------------------------------------------------------------------------------------------------------}
function TXPSpinEdit.CheckValue(NewValue : Currency) : Currency;
begin
	Result := NewValue;
	if (FMaxValue <> FMinValue) then begin
		if NewValue < FMinValue then begin
			Result := FMinValue;
		end else if NewValue > FMaxValue then begin
			Result := FMaxValue;
		end;
	end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TXPSpinEdit.GetMinHeight : Integer;
var
	DC : HDC;
	SaveFont : HFont;
	I :  Integer;
	SysMetrics, Metrics : TTextMetric;
begin
	DC := GetDC(0);
	GetTextMetrics(DC, SysMetrics);
	SaveFont := SelectObject(DC, Font.Handle);
	GetTextMetrics(DC, Metrics);
	SelectObject(DC, SaveFont);
	ReleaseDC(0, DC);
	I := SysMetrics.tmHeight;
	if I > Metrics.tmHeight then begin
		I := Metrics.tmHeight;
	end;
	Result := Metrics.tmHeight + I div 4 + GetSystemMetrics(SM_CYBORDER) * 4 + 2;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPSpinEdit.SetEditRect;
var
	Loc : TRect;
begin
	SendMessage(Handle, EM_GETRECT, 0, LongInt(@Loc));
	Loc.Bottom := ClientHeight + 1;  {+1 is workaround for windows paint bug}
	Loc.Right := ClientWidth - FButton.Width - 2;
	Loc.Top := 0;
	Loc.Left := 0;
	SendMessage(Handle, EM_SETRECTNP, 0, LongInt(@Loc));
	SendMessage(Handle, EM_GETRECT, 0, LongInt(@Loc));  {debug}
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TXPSpinEdit.GetValue : Currency;
begin
	if ( not TryStrToCurr( Text, Result ) ) then begin
		Result := FMinValue;
	end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPSpinEdit.SetValue(NewValue : Currency);
begin
	Text := CurrToStr(CheckValue(NewValue));
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPSpinEdit.CMEnter(var Message : TCMGotFocus);
begin
	if AutoSelect and not (csLButtonDown in ControlState) then begin
		SelectAll;
	end;
	inherited;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPSpinEdit.CMExit(var Message : TCMExit);
begin
	inherited;
	if CheckValue(Value) <> Value then begin
		SetValue(Value);
	end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPSpinEdit.WMCut(var Message : TWMCut);
begin
	if not FEditorEnabled or ReadOnly then begin
		Exit;
	end;
	inherited;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPSpinEdit.WMPaste(var Message : TWMPaste);
begin
	if not FEditorEnabled or ReadOnly then begin
		Exit;
	end;
	inherited;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPSpinEdit.WMSize(var Message : TWMSize);
var
	MinHeight : Integer;
begin
	inherited;
	MinHeight := GetMinHeight;
	{ text edit bug: if size to less than minheight, then edit ctrl does not display the text }
	if Height < MinHeight then begin
		Height := MinHeight;
	end else if FButton <> NIL then begin
		if NewStyleControls and Ctl3D then begin
			FButton.SetBounds(Width - FButton.Width - 5, 0, FButton.Width, Height - 5);
		end else begin
			FButton.SetBounds(Width - FButton.Width, 1, FButton.Width, Height - 3);
		end;
		SetEditRect;
	end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPSpinEdit.CreateParams(var Params : TCreateParams);
begin
	inherited CreateParams(Params);
	{  Params.Style := Params.Style and not WS_BORDER;  }
	Params.Style := Params.Style or ES_MULTILINE or WS_CLIPCHILDREN;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPSpinEdit.CreateWnd;
begin
	inherited CreateWnd;
	SetEditRect;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPSpinEdit.DownClick(Sender : TObject);
begin
	if ReadOnly then begin
		MessageBeep(0);
	end else begin
		Value := Value - FIncrement;
	end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPSpinEdit.GetChildren(Proc : TGetChildProc; Root : TComponent);
begin
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TXPSpinEdit.IsValidChar(Key : Char) : Boolean;
begin
	Result := SysUtils.CharInSet( Key , [DecimalSeparator, '+', '-', '0'..'9', ',', '.']) or ((Key < #32) and (Key <> Chr(VK_RETURN)));
	if not FEditorEnabled and Result and ((Key >= #32) or (Key = Char(VK_BACK)) or (Key = Char(VK_DELETE))) then begin
       Result := FALSE;
	end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPSpinEdit.KeyDown(var Key : Word; Shift : TShiftState);
begin
	if Key = VK_UP then begin
		UpClick(Self);
	end else if Key = VK_DOWN then begin
		DownClick(Self);
	end;
	inherited KeyDown(Key, Shift);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPSpinEdit.KeyPress(var Key : Char);
begin
	if not IsValidChar(Key) then begin
		Key := #0;
		MessageBeep(0);
	end;
	if Key <> #0 then begin
		inherited KeyPress(Key);
	end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPSpinEdit.UpClick(Sender : TObject);
begin
	if ReadOnly then begin
		MessageBeep(0);
	end else begin
		Value := Value + FIncrement;
	end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
constructor TXPSpinEdit.Create(AOwner : TComponent);
begin
	inherited Create(AOwner);
	FButton := TXPSpinButton.Create(Self);
	FButton.Width := 15;
	FButton.Height := 17;
	FButton.Visible := TRUE;
	FButton.Parent := Self;
	FButton.FocusControl := Self;
	FButton.OnUpClick := UpClick;
	FButton.OnDownClick := DownClick;
	Text := '0';
	ControlStyle := ControlStyle - [csSetCaption];
	FIncrement := 1;
	FEditorEnabled := TRUE;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
destructor TXPSpinEdit.Destroy;
begin
	FButton := NIL;
	inherited Destroy;
end;

{-**********************************************************************
************************************************************************
******************
******************  Class:    TXPTimerSpeedButton
******************  Category: No category
******************
************************************************************************
************************************************************************}
{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPTimerSpeedButton.TimerExpired(Sender : TObject);
begin
	FRepeatTimer.Interval := RepeatPause;
	if (FState = bsDown) and MouseCapture then begin
		try
			Click;
		except
			FRepeatTimer.Enabled := FALSE;
			raise;
		end;
	end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPTimerSpeedButton.MouseDown(Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
begin
	inherited MouseDown(Button, Shift, X, Y);
	if tbAllowTimer in FTimeBtnState then begin
		if FRepeatTimer = NIL then begin
			FRepeatTimer := TTimer.Create(Self);
		end;

		FRepeatTimer.OnTimer := TimerExpired;
		FRepeatTimer.Interval := InitRepeatPause;
		FRepeatTimer.Enabled := TRUE;
	end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPTimerSpeedButton.MouseUp(Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
begin
	inherited MouseUp(Button, Shift, X, Y);
	if FRepeatTimer <> NIL then begin
		FRepeatTimer.Enabled := FALSE;
	end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPTimerSpeedButton.Paint;
var
	R : TRect;
begin
	inherited Paint;
	if tbFocusRect in FTimeBtnState then begin
		R := Bounds(0, 0, Width, Height);
		InflateRect(R, -3, -3);
		if FState = bsDown then begin
			OffsetRect(R, 1, 1);
		end;
		DrawFocusRect(Canvas.Handle, R);
	end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
destructor TXPTimerSpeedButton.Destroy;
begin
	if FRepeatTimer <> NIL then begin
		FRepeatTimer.Free;
	end;
	inherited Destroy;
end;

end.


