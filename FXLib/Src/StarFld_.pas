{$IFDEF StarFld_}
{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I FXLib.inc}
unit StarFld_;

interface

uses
  Windows, Classes, Graphics, Controls, Forms, Extctrls;

{ TStarFieldBasic component - version 1.00

  - can be published or supplied on CD-ROM (only if not amended)

  Other components/applications I have created and released:

  TMJWstar          - Panel with moving stars as a background.
  (MJWSTAR.ZIP)
  www.delphi32.com
  Compuserve Delphi32 Forum.

  Orginal credits to:
  - TMJWstar by Michael Wilcox Email:    mwilcox@economat.demon.co.uk  michael@economatics.co.uk
  Adtiopnal credits to - Matthias Laschat (STARFLD.PAS)



  Features:
  - Inherited Panel component with moving stars as a background.
  - Warps during design time.
  - Forward and Reverse Warps. (Reverse speed eg: -20)
  - Option of raised/lowered Bevels.

}

type
  TStarTiplePoint = record
	x, y, z: single;
  end;
  TStarData = array [1 .. 1000] of TStarTiplePoint;

  TStarFieldBasic = class(TCustomPanel)
  private
	{ Private }
	FNumberOfStars: word;
	FZoom, FSpeed: Integer;
	TStarData: TStarData;
	FWrapStars: Boolean;
	awidth, bwidth: Integer;
	FInterval: Integer;
	FWarp: Boolean;
	Timer: TTimer;
	FWarp10: Boolean;
	procedure GenerateStars;
	procedure MoveBackgroundStars(mx, my: Integer);
	procedure WrapStars;
	procedure SetSpeed(i: Integer);
	procedure SetZoomFactor(i: Integer);
	procedure SetNumberOfStars(i: word);
	procedure SetInterval(Value: Integer);
	procedure SetWarp(Onn: Boolean);
	procedure TimeHit(Sender: TObject);
  protected
	{ Protected }
  public
	{ Public }
	constructor Create(Aowner: Tcomponent); override;
	destructor Destroy; override;
	procedure MoveStars(mx, my, mz: Integer);
	procedure PaintStars;
	procedure Paint; override;
	procedure Redraw; virtual;
  published
	{ Published }
	property Width;
	property Height;
	property NumberOfStars: word read FNumberOfStars write SetNumberOfStars;
	property ZoomFactor: Integer read FZoom write SetZoomFactor;
	property Speed: Integer read FSpeed write SetSpeed;
	property WarpStart: Boolean read FWarp write SetWarp;
	property WarpInterval: Integer read FInterval write SetInterval;
	property Warp10: Boolean read FWarp10 write FWarp10;

	property Align;
	property BevelOuter;
	property BevelWidth;
	property BorderStyle;
	property DragCursor;
	property DragMode;
	property Ctl3D;
	property Locked;
	property ParentShowHint;
	property PopupMenu;
	property ShowHint;
	property TabOrder;
	property TabStop;
	property Visible;
	property OnClick;
	property OnDblClick;
	property OnDragDrop;
	property OnDragOver;
	property OnEndDrag;
	property OnEnter;
	property OnExit;
	property OnMouseDown;
	property OnMouseMove;
	property OnMouseUp;
	property OnResize;
  end;

procedure register;

implementation

const

  a: longint                        = 200;
  grays: array [0 .. 15] of longint = ($FFFFFF, $FFFFFF, $FFFFFF, $FFFFFF, $EEEEEE, $DDDDDD, $CCCCCC, $BBBBBB, $AAAAAA, $999999,
	  $888888, $777777, $555555, $333333, $111111, $000000);

procedure register;
begin
  RegisterComponents('FX Controls', [TStarFieldBasic]);
end;

{ Create Method }
constructor TStarFieldBasic.Create(Aowner: Tcomponent);
// ----------------------------------------------------------------------------------------------------------------------
begin
  inherited Create(Aowner);
  Width := 300;
  Height := 200;
  FNumberOfStars := 200;
  FZoom := 100;
  FSpeed := 20;
  color := clblack;
  if screen.Width > 2000 then begin
	awidth := screen.Width * 2
  end else begin
	awidth := 2000;
  end;
  bwidth := awidth div 2;
  GenerateStars;
  FInterval := 1;
  FWarp := false;
  FWarp10 := false;
end;

{ Destroy Method }
destructor TStarFieldBasic.Destroy;
// ----------------------------------------------------------------------------------------------------------------------
begin
  inherited Destroy;
end;

{ Generate Star Data }
procedure TStarFieldBasic.GenerateStars;
// ----------------------------------------------------------------------------------------------------------------------
var
  i: Integer;
begin
  for i := 1 to FNumberOfStars do begin
	with TStarData[i] do begin
	  x := Integer(random(awidth)) - 1000;
	  y := Integer(random(awidth)) - bwidth;
	  z := Integer(random(awidth));
	end;
  end;
end;

{ Wrap Stars }
procedure TStarFieldBasic.WrapStars;
// ----------------------------------------------------------------------------------------------------------------------
var
  i: Integer;
begin
  for i := 1 to FNumberOfStars do begin
	with TStarData[i] do begin
	  while x < -bwidth do
		x := x + awidth;
	  while x > bwidth do
		x := x - awidth;
	  while y < -bwidth do
		y := y + awidth;
	  while y > bwidth do
		y := y - awidth;
	  while z <= 0 do
		z := z + awidth;
	  while z > awidth do
		z := z - awidth;
	end;
  end;
  FWrapStars := false;
end;

{ Move Stars }
procedure TStarFieldBasic.MoveStars(mx, my, mz: Integer);
// ----------------------------------------------------------------------------------------------------------------------
var
  i: Integer;
begin
  for i := 1 to FNumberOfStars do begin
	with TStarData[i] do begin
	  x := x + mx;
	  y := y + my;
	  z := z + mz;
	end;
  end;
  FWrapStars := true;
end;

{ Set Speed }
procedure TStarFieldBasic.SetSpeed(i: Integer);
// ----------------------------------------------------------------------------------------------------------------------
begin
  FSpeed := i;
  Redraw;
end;

{ Set Zoom Factor }
procedure TStarFieldBasic.SetZoomFactor(i: Integer);
// ----------------------------------------------------------------------------------------------------------------------
begin
  FZoom := i;
  Redraw;
end;

{ Set Number of Stars }
procedure TStarFieldBasic.SetNumberOfStars(i: word);
// ----------------------------------------------------------------------------------------------------------------------
begin
  if (i > 1000) then begin
	i := 1000;
  end;
  if (i < 5) then begin
	i := 5;
  end;
  FNumberOfStars := i;
  GenerateStars;
  Redraw;
end;

{ Timer Interval }
procedure TStarFieldBasic.SetInterval(Value: Integer);
// ----------------------------------------------------------------------------------------------------------------------
begin
  if Value <> FInterval then begin
	Timer.Free;
	Timer := nil;
	if FWarp and (Value > 0) then begin
	  Timer := TTimer.Create(Self);
	  Timer.Interval := Value;
	  Timer.OnTimer := TimeHit;
	end;
	FInterval := Value;
  end;
end;

{ Star timer to move stars }
procedure TStarFieldBasic.SetWarp(Onn: Boolean);
// ----------------------------------------------------------------------------------------------------------------------
begin
  if Onn <> FWarp then begin
	FWarp := Onn;
	if not Onn then begin
	  Timer.Free;
	  Timer := nil;
	end else begin
	  if FInterval > 0 then begin
		Timer := TTimer.Create(Self);
		Timer.Interval := FInterval;
		Timer.OnTimer := TimeHit;
	  end;
	end;
  end;
end;

{ Paint Stars }
procedure TStarFieldBasic.PaintStars;
// ----------------------------------------------------------------------------------------------------------------------
var
  i: Integer;
  rx, ry: Integer;
  xmid, ymid: Integer;
  azoom: single;
  Rect: TRect;
  TopColor, BottomColor, clr: TColor;
begin
  if (csDesigning in ComponentState) and (FWarp = false) then begin
	canvas.brush.color := clblack;
	canvas.rectangle(0, 0, Width, Height);
  end;

  if FWrapStars then begin
	WrapStars;
  end;
  azoom := FZoom / 100;

  xmid := Width div 2;
  ymid := Height div 2;

  { Draw Background Stars }
  for i := 1 to (FNumberOfStars div 2) do begin
	with TStarData[i] do begin
	  rx := round(xmid + (a * x / 300) * azoom);
	  ry := round(ymid + (a * y / 500) * azoom);
	  if (ry > (ClientRect.top + BevelWidth) + 1) and (ry < (ClientRect.Bottom - BevelWidth) - 1) and
		  (rx > (ClientRect.Left + BevelWidth) + 1) and (rx < (ClientRect.Right - BevelWidth) - 1) then begin
		canvas.pixels[rx, ry] := clWhite;
	  end;
	end;
  end;

  for i := (FNumberOfStars div 2) + 1 to FNumberOfStars do begin
	with TStarData[i] do begin
	  if z > 0 then begin
		if FWarp10 = true then begin
		  clr := grays[random(15)]
		end else begin
		  clr := color;
		end;
		{ Remove Small Star }
		rx := round(xmid + (a * x / z) * azoom);
		ry := round(ymid + (a * y / z) * azoom);
		if (ry > (ClientRect.top + BevelWidth) + 1) and (ry < (ClientRect.Bottom - BevelWidth) - 1) and
			(rx > (ClientRect.Left + BevelWidth) + 1) and (rx < (ClientRect.Right - BevelWidth) - 1) then begin
		  canvas.pixels[rx, ry] := clr;
		end;
		if round(z * 15 / awidth) < 7 then begin { Remove Large Star }
		  if (ry > (ClientRect.top + BevelWidth) + 1) and (ry < (ClientRect.Bottom - BevelWidth) - 1) and
			  (rx > (ClientRect.Left + BevelWidth) + 1) and (rx < (ClientRect.Right - BevelWidth) - 1) then begin
			canvas.pixels[rx, ry + 1] := clr;
			canvas.pixels[rx, ry - 1] := clr;
			canvas.pixels[rx + 1, ry] := clr;
			canvas.pixels[rx - 1, ry] := clr;
		  end;
		end;
	  end;

	  x := x + 0;
	  y := y + 0;
	  z := z + (-FSpeed);
	  FWrapStars := true;

	  if z > 0 then begin
		{ Draw Small Star }
		rx := round(xmid + (a * x / z) * azoom);
		ry := round(ymid + (a * y / z) * azoom);
		if (ry > (ClientRect.top + BevelWidth) + 1) and (ry < (ClientRect.Bottom - BevelWidth) - 1) and
			(rx > (ClientRect.Left + BevelWidth) + 1) and (rx < (ClientRect.Right - BevelWidth) - 1) then begin
		  canvas.pixels[rx, ry] := grays[round(z * 15 / awidth)];
		end;
		if round(z * 15 / awidth) < 7 then begin
		  { Draw Large Star }
		  if (ry > (ClientRect.top + BevelWidth) + 1) and (ry < (ClientRect.Bottom - BevelWidth) - 1) and
			  (rx > (ClientRect.Left + BevelWidth) + 1) and (rx < (ClientRect.Right - BevelWidth) - 1) then begin
			canvas.pixels[rx, ry + 1] := grays[round(z * 15 / awidth)];
			canvas.pixels[rx, ry - 1] := grays[round(z * 15 / awidth)];
			canvas.pixels[rx + 1, ry] := grays[round(z * 15 / awidth)];
			canvas.pixels[rx - 1, ry] := grays[round(z * 15 / awidth)];
		  end;
		end;
	  end;
	end;
  end;
  { Display Bevel }
  Rect := GetClientRect;
  if BevelOuter <> bvNone then begin
	TopColor := clBtnHighlight;
	if BevelOuter = bvLowered then begin
	  TopColor := clBtnShadow;
	end;
	BottomColor := clBtnShadow;
	if BevelOuter = bvLowered then begin
	  BottomColor := clBtnHighlight;
	end;
	Frame3D(canvas, Rect, TopColor, BottomColor, BevelWidth);
  end;
end;

{ paint }
procedure TStarFieldBasic.Paint;
// ----------------------------------------------------------------------------------------------------------------------
begin
  canvas.brush.color := clblack;
  canvas.rectangle(0, 0, Width, Height);
  PaintStars;
end;

{ Redraw }
procedure TStarFieldBasic.Redraw;
// ----------------------------------------------------------------------------------------------------------------------
begin
  Paint;
end;

{ Respond to timer by calling Paint method }
procedure TStarFieldBasic.TimeHit(Sender: TObject);
// ----------------------------------------------------------------------------------------------------------------------
begin
  if FWarp then begin
	PaintStars;
  end else begin
	Timer.Free;
	Timer := nil;
  end;
  // Self.MoveStars( Random( 25 ),Random( 25 ), Random( 25 ) );
  Self.MoveBackgroundStars(random(25), random(25));
end;

procedure TStarFieldBasic.MoveBackgroundStars(mx, my: Integer);
// ----------------------------------------------------------------------------------------------------------------------
var
  i: Integer;
begin
  for i := 1 to FNumberOfStars do begin
	with TStarData[i] do begin
	  if (z < 5) then begin
		x := x + mx;
		y := y + my;
	  end;
	end;
  end;
  FWrapStars := true;
end;

end.
