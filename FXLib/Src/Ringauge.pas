{$IFDEF RinGauge}
{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I FXLib.inc}
unit RinGauge;

{ Written By Tom Lee,Taiwan Ver 0.94 Beta }
{ Tomm.bbs@[140.113.17.154] }
interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, ExtCtrls;

type
  TRingGauge = class(TPanel)
  private
	FColor1: TColor;
	FColor2: TColor;
	FColor3: TColor;
	FGaugeHeight: Integer;
	FIndicationPoint1: LongInt;
	FIndicationPoint2: LongInt;
	FInnerRadius: Integer;
	FGaugeLeft: Integer;
	FMax: LongInt;
	FMin: LongInt;
	FNeedleRadius: Integer;
	FNeedleDegree: Integer;
	FNeedleWidth: Integer;
	FOuterRadius: Integer;
	FProgress: LongInt;
	FSectorDegree: Integer;
	FGaugeTop: Integer;
	FGaugeWidth: Integer;
	procedure DrawCanvas;
	procedure DrawGauge(x1, y1, x2, y2, OutR, InR, PieDeg, P1, P2: Integer);
	procedure DrawNeedle(Deg, x1, y1, x2, y2, R: Integer);
	procedure SetColor1(Value: TColor);
	procedure SetColor2(Value: TColor);
	procedure SetColor3(Value: TColor);
	procedure SetIndicationPoint1(Value: LongInt);
	procedure SetIndicationPoint2(Value: LongInt);
	procedure SetInnerRadius(Value: Integer);
	procedure SetMaxValue(Value: LongInt);
	procedure SetMinValue(Value: LongInt);
	procedure SetNeedleRadius(Value: Integer);
	procedure SetNeedleWidth(Value: Integer);
	procedure SetOuterRadius(Value: Integer);
	procedure SetPosition(Value: LongInt);
	procedure SetSectorDegree(Value: Integer);
	function DegToRad(inputDeg: Double): Double;
	function GetArcPointX(Deg: Double; x1, x2, d: Integer): Integer;
	function GetArcPointY(Deg: Double; y1, y2, d: Integer): Integer;
	function GetNeedleDegree(Min, Max, Progress: LongInt): Integer;
	function GetRx(x1, x2: Integer): Integer;
	function GetRy(y1, y2: Integer): Integer;
	function GetR(x1, x2: Integer): Integer;
  protected
	procedure Paint; override;
	procedure WMSize(var Message: TWMSize); message WM_SIZE;
	property Canvas;
  public
	constructor Create(AOwner: TComponent); override;
  published
	property Align;
	property Alignment;
	property BevelInner;
	property BevelOuter;
	property BevelWidth;
	property BorderStyle;
	property BorderWidth;
	property Caption;
	property Color;
	property Ctl3D;
	property Cursor;
	property DragCursor;
	property DragMode;
	property Enabled;
	property FirstIndicationPoint: LongInt read FIndicationPoint1 write SetIndicationPoint1 default 60;
	property FirstPartColor: TColor read FColor1 write SetColor1 default clBlue;
	property Font;
	property Height;
	property HelpContext;
	property Hint;
	property InnerRadius: Integer read FInnerRadius write SetInnerRadius default 60;
	property Left;
	property Locked;
	property name;
	property Max: LongInt read FMax write SetMaxValue default 100;
	property Min: LongInt read FMin write SetMinValue default 0;
	property NeedleRadius: Integer read FNeedleRadius write SetNeedleRadius default 70;
	property NeedleWidth: Integer read FNeedleWidth write SetNeedleWidth default 1;
	property OnClick;
	property OnDragDrop;
	property OnDblClick;
	property OnDragOver;
	property OnEndDrag;
	property OnEnter;
	property OnExit;
	property OnMouseDown;
	property OnMouseMove;
	property OnMouseUp;
	property OnResize;
	property OuterRadius: Integer read FOuterRadius write SetOuterRadius default 110;
	property ParentColor;
	property ParentCtl3D;
	property ParentFont;
	property ParentShowHint;
	property Progress: LongInt read FProgress write SetPosition default 0;
	property SecondIndicationPoint: LongInt read FIndicationPoint2 write SetIndicationPoint2 default 90;
	property SecondPartColor: TColor read FColor2 write SetColor2 default clYellow;
	property SectorDegree: Integer read FSectorDegree write SetSectorDegree default 60;
	property ShowHint;
	property TabOrder;
	property TabStop;
	property Tag;
	property ThirdPartColor: TColor read FColor3 write SetColor3 default ClRed;
	property Top;
	property Visible;
	property Width;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('FX Controls', [TRingGauge]);
end;

constructor TRingGauge.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColor1 := clBlue;
  FColor2 := clYellow;
  FColor3 := ClRed;
  Height := 300;
  FIndicationPoint1 := 60;
  FIndicationPoint2 := 90;
  FInnerRadius := 60;
  FMax := 100;
  FMin := 0;
  FNeedleRadius := 70;
  FNeedleWidth := 1;
  FOuterRadius := 110;
  FProgress := 0;
  FSectorDegree := 60;
  Width := 300;
  FNeedleDegree := GetNeedleDegree(FMin, FMax, FProgress);
  Refresh;
end;

procedure TRingGauge.WMSize(var Message: TWMSize);
begin
  inherited;
  if Width < 130 then
	Width := 130;
  if Height < 130 then
	Height := 130;
  if Width < Height then begin
	FGaugeTop := 10;
	FGaugeLeft := 5;
	FGaugeWidth := Width - 10;
	FGaugeHeight := Width - 10;
	FOuterRadius := (FGaugeWidth div 2) - 15;
	FInnerRadius := (FGaugeWidth div 2) - 30;
	FNeedleRadius := (FGaugeWidth div 2) - 25;
  end else begin
	FGaugeTop := 10;
	FGaugeLeft := 5;
	FGaugeWidth := Height - 10;
	FGaugeHeight := Height - 10;
	FOuterRadius := (FGaugeWidth div 2) - 15;
	FInnerRadius := (FGaugeWidth div 2) - 30;
	FNeedleRadius := (FGaugeWidth div 2) - 25;
  end;
  Refresh;
end;

procedure TRingGauge.Paint;
begin
  inherited Paint;
  DrawCanvas;
end;

procedure TRingGauge.DrawCanvas;
begin
  DrawGauge(FGaugeLeft, FGaugeTop, FGaugeLeft + FGaugeWidth, FGaugeTop + FGaugeHeight, FOuterRadius, FInnerRadius, FSectorDegree,
	  FIndicationPoint1, FIndicationPoint2);
  FNeedleDegree := GetNeedleDegree(FMin, FMax, FProgress);
  DrawNeedle(FNeedleDegree, FGaugeLeft, FGaugeTop, FGaugeLeft + FGaugeWidth, FGaugeTop + FGaugeHeight, FNeedleRadius);
end;

procedure TRingGauge.SetNeedleWidth(Value: Integer);
begin
  if Value <> FNeedleWidth then begin
	if Value < 1 then
	  Value := 1;
	FNeedleWidth := Value;
	DrawCanvas;
	Refresh;
  end;
end;

procedure TRingGauge.SetColor1(Value: TColor);
begin
  if Value <> FColor1 then begin
	FColor1 := Value;
	DrawCanvas;
	Refresh;
  end;
end;

procedure TRingGauge.SetColor2(Value: TColor);
begin
  if Value <> FColor2 then begin
	FColor2 := Value;
	DrawCanvas;
	Refresh;
  end;
end;

procedure TRingGauge.SetColor3(Value: TColor);
begin
  if Value <> FColor3 then begin
	FColor3 := Value;
	DrawCanvas;
	Refresh;
  end;
end;

procedure TRingGauge.SetSectorDegree(Value: Integer);
begin
  if FSectorDegree <> Value then begin
	if Value > 300 then
	  Value := 300;
	if Value < 6 then
	  Value := 6;
	FSectorDegree := Value;
	DrawCanvas;
	Refresh;
  end;
end;

procedure TRingGauge.SetNeedleRadius(Value: Integer);
begin
  if FNeedleRadius <> Value then begin
	FNeedleRadius := Value;
	DrawCanvas;
	Refresh;
  end;
end;

procedure TRingGauge.SetOuterRadius(Value: Integer);
begin
  if FOuterRadius <> Value then begin
	if Value > (FGaugeWidth div 2) then
	  Value := (FGaugeWidth div 2) - 5;
	FOuterRadius := Value;
	DrawCanvas;
	Refresh;
  end;
end;

procedure TRingGauge.SetInnerRadius(Value: Integer);
begin
  if FInnerRadius <> Value then begin
	if Value > FOuterRadius - 10 then
	  Value := FOuterRadius - 10;
	if Value < 0 then
	  Value := 0;
	FInnerRadius := Value;
	DrawCanvas;
	Refresh;
  end;
end;

procedure TRingGauge.SetIndicationPoint1(Value: LongInt);
begin
  if FIndicationPoint1 <> Value then begin
	if Value >= (FIndicationPoint2 - (FMax - FMin) * 5 div 100) then
	  Value := FIndicationPoint2 - (FMax - FMin) * 5 div 100;
	if Value < FMin + ((FMax - FMin) * 5 div 100) then
	  Value := FMin + ((FMax - FMin) * 5 div 100);
	FIndicationPoint1 := Value;
	DrawCanvas;
	Refresh;
  end;
end;

procedure TRingGauge.SetIndicationPoint2(Value: LongInt);
begin
  if FIndicationPoint2 <> Value then begin
	if Value <= (FIndicationPoint1 + (FMax - FMin) * 5 div 100) then
	  Value := (FIndicationPoint1 + (FMax - FMin) * 5 div 100);
	if Value > FMax - ((FMax - FMin) * 5 div 100) then
	  Value := FMax - ((FMax - FMin) * 5 div 100);
	FIndicationPoint2 := Value;
	DrawCanvas;
	Refresh;
  end;
end;

procedure TRingGauge.SetPosition(Value: LongInt);
begin
  if FProgress <> Value then begin
	if Value > FMax then
	  Value := FMax;
	if Value < FMin then
	  Value := FMin;
	DrawNeedle(FNeedleDegree, FGaugeLeft, FGaugeTop, FGaugeLeft + FGaugeWidth, FGaugeTop + FGaugeHeight, FNeedleRadius);
	FProgress := Value;
	FNeedleDegree := GetNeedleDegree(FMin, FMax, FProgress);
	DrawCanvas;
  end;
end;

procedure TRingGauge.SetMinValue(Value: LongInt);
begin
  if Value <> FMin then begin
	if Value >= FMax - 50 then
	  Value := FMax - 50;
	FIndicationPoint1 := (FMax - Value) * (FIndicationPoint1 - FMin) div (FMax - FMin) + Value;
	FIndicationPoint2 := (FMax - Value) * (FIndicationPoint2 - FMin) div (FMax - FMin) + Value;
	FProgress := (FMax - Value) * (FProgress - FMin) div (FMax - FMin) + Value;
	FMin := Value;
	FNeedleDegree := GetNeedleDegree(FMin, FMax, FProgress);
	DrawCanvas;
	Refresh;
  end;
end;

procedure TRingGauge.SetMaxValue(Value: LongInt);
begin
  if Value <> FMax then begin
	if Value <= FMin + 50 then
	  Value := FMin + 50;
	FIndicationPoint1 := (Value - FMin) * (FIndicationPoint1 - FMin) div (FMax - FMin) + FMin;
	FIndicationPoint2 := (Value - FMin) * (FIndicationPoint2 - FMin) div (FMax - FMin) + FMin;
	FProgress := (FMax - Value) * (FProgress - FMin) div (FMax - FMin) + FMin;
	FMax := Value;
	FNeedleDegree := GetNeedleDegree(FMin, FMax, FProgress);
	DrawCanvas;
	Refresh;
  end;
end;

function TRingGauge.DegToRad(inputDeg: Double): Double;
begin
  Result := inputDeg * pi / 180;
end;

procedure TRingGauge.DrawGauge(x1, y1, x2, y2, OutR, InR, PieDeg, P1, P2: Integer);
var
  Sx1, Sy1, Ex1, Ey1: Integer;
  Sx2, Sy2, Ex2, Ey2: Integer;
  Sx3, Sy3, Ex3, Ey3: Integer;
  Sx4, Sy4, Ex4, Ey4: Integer;
  P1x1, P1y1, P1x2, P1y2: Integer;
  P2x1, P2y1, P2x2, P2y2: Integer;
  Pnx1, Pny1, Pnx2, Pny2: Integer;
  D1, D2, R, Idx: Integer;
  P1Deg, P2Deg, PnDeg: Integer;
  SimePieDeg: Integer;

begin
  with Canvas do begin
	{ Initialize Pen }
	Pen.Color := clBlack;
	Pen.Mode := pmCopy;
	Pen.Width := 1;
	{ Draw Arc }
	R := GetR(x1, x2);
	D1 := R - OutR;
	D2 := R - InR;
	SimePieDeg := PieDeg div 2;
	Sx1 := GetArcPointX(-SimePieDeg, x1, x2, OutR + 2);
	Sy1 := GetArcPointY(-SimePieDeg, y1, y2, OutR + 2);
	Ex1 := GetArcPointX(SimePieDeg, x1, x2, OutR + 2);
	Ey1 := GetArcPointY(SimePieDeg, y1, y2, OutR + 2);
	Sx2 := GetArcPointX(-SimePieDeg, x1, x2, InR - 4);
	Sy2 := GetArcPointY(-SimePieDeg, y1, y2, InR - 4);
	Ex2 := GetArcPointX(SimePieDeg, x1, x2, InR - 4);
	Ey2 := GetArcPointY(SimePieDeg, y1, y2, InR - 4);
	Sx3 := GetArcPointX(-SimePieDeg + 2, x1, x2, OutR);
	Sy3 := GetArcPointY(-SimePieDeg + 2, y1, y2, OutR);
	Ex3 := GetArcPointX(SimePieDeg - 2, x1, x2, OutR);
	Ey3 := GetArcPointY(SimePieDeg - 2, y1, y2, OutR);
	Sx4 := GetArcPointX(-SimePieDeg + 2, x1, x2, InR);
	Sy4 := GetArcPointY(-SimePieDeg + 2, y1, y2, InR);
	Ex4 := GetArcPointX(SimePieDeg - 2, x1, x2, InR);
	Ey4 := GetArcPointY(SimePieDeg - 2, y1, y2, InR);
	Arc(x1 + D1, y1 + D1, x2 - D1, y2 - D1, Sx3, Sy3, Ex3, Ey3); { Draw Outer Arc }
	Arc(x1 + D2, y1 + D2, x2 - D2, y2 - D2, Sx4, Sy4, Ex4, Ey4); { Draw Inner Arc }
	MoveTo(Sx1, Sy1);
	LineTo(Sx2, Sy2);
	MoveTo(Ex1, Ey1);
	LineTo(Ex2, Ey2);
	P1Deg := SimePieDeg + (P1 - FMin) * (360 - PieDeg) div (FMax - FMin);
	P2Deg := SimePieDeg + (P2 - FMin) * (360 - PieDeg) div (FMax - FMin);
	P1x1 := GetArcPointX(P1Deg, x1, x2, OutR);
	P1y1 := GetArcPointY(P1Deg, y1, y2, OutR);
	P1x2 := GetArcPointX(P1Deg, x1, x2, InR - 4);
	P1y2 := GetArcPointY(P1Deg, y1, y2, InR - 4);
	P2x1 := GetArcPointX(P2Deg, x1, x2, OutR);
	P2y1 := GetArcPointY(P2Deg, y1, y2, OutR);
	P2x2 := GetArcPointX(P2Deg, x1, x2, InR - 4);
	P2y2 := GetArcPointY(P2Deg, y1, y2, InR - 4);
	MoveTo(P1x1, P1y1);
	LineTo(P1x2, P1y2);
	MoveTo(P2x1, P2y1);
	LineTo(P2x2, P2y2);
	Brush.Color := FColor1;
	FloodFill(GetArcPointX(P1Deg - 2, x1, x2, OutR - 3), GetArcPointY(P1Deg - 2, y1, y2, OutR - 3), clBlack, fsBorder);
	Brush.Color := FColor2;
	FloodFill(GetArcPointX(P1Deg + 2, x1, x2, OutR - 3), GetArcPointY(P1Deg + 2, y1, y2, OutR - 3), clBlack, fsBorder);
	Brush.Color := FColor3;
	FloodFill(GetArcPointX(P2Deg + 2, x1, x2, OutR - 3), GetArcPointY(P2Deg + 2, y1, y2, OutR - 3), clBlack, fsBorder);
	{ Draw Arc Shadow }
	Pen.Color := clWhite;
	Pen.Width := 1;
	Arc(x1 + D1 - 1, y1 + D1 - 1, x2 - D1 + 1, y2 - D1 + 1, Sx1, Sy1, Ex1, Ey1);
	{ Draw Indicator }
	Pen.Color := clBlack;
	Pen.Width := 1;
	Arc(x1 + D2 + 3, y1 + D2 + 3, x2 - D2 - 3, y2 - D2 - 3, Sx2, Sy2, Ex2, Ey2);
	for Idx := 0 to 19 do begin
	  PnDeg := SimePieDeg + Idx * 5 * (360 - PieDeg) div 100;
	  Pnx1 := GetArcPointX(PnDeg, x1, x2, InR - 3);
	  Pny1 := GetArcPointY(PnDeg, y1, y2, InR - 3);
	  if Idx mod 2 = 0 then begin
		Pnx2 := GetArcPointX(PnDeg, x1, x2, InR - 12);
		Pny2 := GetArcPointY(PnDeg, y1, y2, InR - 12);
	  end else begin
		Pnx2 := GetArcPointX(PnDeg, x1, x2, InR - 8);
		Pny2 := GetArcPointY(PnDeg, y1, y2, InR - 8);
	  end;
	  MoveTo(Pnx1, Pny1);
	  LineTo(Pnx2, Pny2);
	end;
	PnDeg := SimePieDeg + (360 - PieDeg);
	Pnx1 := GetArcPointX(PnDeg, x1, x2, InR - 3);
	Pny1 := GetArcPointY(PnDeg, y1, y2, InR - 3);
	Pnx2 := GetArcPointX(PnDeg, x1, x2, InR - 12);
	Pny2 := GetArcPointY(PnDeg, y1, y2, InR - 12);
	MoveTo(Pnx1, Pny1);
	LineTo(Pnx2, Pny2);
  end;
end;

procedure TRingGauge.DrawNeedle(Deg, x1, y1, x2, y2, R: Integer);
var
  rx, ry: Integer;
  x, y: Integer;
begin
  with Canvas do begin
	rx := GetRx(x1, x2);
	ry := GetRy(y1, y2);
	x := GetArcPointX(Deg, x1, x2, R);
	y := GetArcPointY(Deg, y1, y2, R);
	Pen.Color := clBlack;
	Pen.Mode := pmNot;
	Pen.Width := FNeedleWidth;
	MoveTo(rx, ry);
	LineTo(x, y);
	Pen.Mode := pmCopy;
  end;
end;

function TRingGauge.GetR(x1, x2: Integer): Integer;
begin
  Result := Abs(x2 - x1) div 2;
end;

function TRingGauge.GetArcPointX(Deg: Double; x1, x2, d: Integer): Integer;
var
  tmpint: Integer;
begin
  tmpint := Round(d * Sin(DegToRad(Deg)));
  Result := GetRx(x1, x2) - tmpint;
end;

function TRingGauge.GetArcPointY(Deg: Double; y1, y2, d: Integer): Integer;
var
  tmpint: Integer;
begin
  tmpint := Integer(Round(d * cos(DegToRad(Deg))));
  Result := GetRy(y1, y2) + tmpint;
end;

function TRingGauge.GetRx(x1, x2: Integer): Integer;
begin
  Result := x1 + (x2 - x1) div 2;
end;

function TRingGauge.GetRy(y1, y2: Integer): Integer;
begin
  Result := y1 + (y2 - y1) div 2;
end;

function TRingGauge.GetNeedleDegree(Min, Max, Progress: LongInt): Integer;
begin
  Result := (((360 - SectorDegree) * (Progress - Min)) div (Max - Min)) + (SectorDegree div 2)
end;

end.
