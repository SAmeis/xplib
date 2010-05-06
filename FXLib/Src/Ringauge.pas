unit RinGauge;
{                      Written By Tom Lee,Taiwan Ver 0.94 Beta}
{                                   Tomm.bbs@[140.113.17.154] }
interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, ExtCtrls;

type
  TRingGauge = class(TPanel)
  private
    { Private declarations }
    FColor1:TColor;
    FColor2:TColor;
    FColor3:TColor;
    FGaugeHeight:Integer;
    FIndicationPoint1:LongInt;
    FIndicationPoint2:LongInt;
    FInnerRadius:Integer;
    FGaugeLeft:Integer;
    FMax:Longint;
    FMin:Longint;
    FNeedleRadius:Integer;
    FNeedleDegree:Integer;
    FNeedleWidth:Integer;
    FOuterRadius:Integer;
    FProgress:LongInt;
    FSectorDegree:Integer;
    FGaugeTop:Integer;
    FGaugeWidth:Integer;
    Procedure DrawCanvas;
    Procedure DrawGauge(x1,y1,x2,y2,OutR,InR,PieDeg,P1,P2:Integer);
    Procedure DrawNeedle(Deg,x1,y1,x2,y2,R:Integer);
    Procedure SetColor1(Value:TColor);
    Procedure SetColor2(Value:TColor);
    Procedure SetColor3(Value:TColor);
    Procedure SetIndicationPoint1(Value:LongInt);
    Procedure SetIndicationPoint2(Value:LongInt);
    Procedure SetInnerRadius(Value:Integer);
    Procedure SetMaxValue(Value:LongInt);
    Procedure SetMinValue(Value:LongInt);
    Procedure SetNeedleRadius(Value:Integer);
    Procedure SetNeedleWidth(Value:Integer);
    Procedure SetOuterRadius(value:Integer);
    Procedure SetPosition(Value:LongInt);
    Procedure SetSectorDegree(Value:Integer);
    Function  DegToRad(inputDeg:Double):Double;
    Function  GetArcPointX(Deg:Double;x1,x2,d:integer):Integer;
    Function  GetArcPointY(Deg:Double;y1,y2,d:integer):Integer;
    Function  GetNeedleDegree(Min,Max,Progress:LongInt):Integer;
    Function  GetRx(x1,x2:Integer):Integer;
    Function  GetRy(y1,y2:Integer):Integer;
    Function  GetR(x1,x2:Integer):Integer;
  Protected
    { Protected declarations }
    Procedure Paint; override;
    Procedure WMSize(var Message: TWMSize); message WM_SIZE;
    Property  Canvas;
  Public
    { Public declarations }
    Constructor Create(AOwner: TComponent); override;
  Published
    { Published declarations }
    Property Align;
    Property Alignment;
    Property BevelInner;
    Property BevelOuter;
    Property BevelWidth;
    Property BorderStyle;
    Property BorderWidth;
    Property Caption;
    Property Color;
    Property Ctl3D;
    Property Cursor;
    Property DragCursor;
    Property DragMode;
    Property Enabled;
    Property FirstIndicationPoint:Longint Read FIndicationPoint1 Write SetIndicationPoint1 Default 60;
    Property FirstPartColor:TColor Read FColor1 Write SetColor1 Default clBlue;
    Property Font;
    Property Height;
    Property HelpContext;
    Property Hint;
    Property InnerRadius:Integer Read FInnerRadius Write SetInnerRadius Default 60;
    Property Left;
    Property Locked;
    Property Name;
    Property Max:Longint Read FMax Write SetMaxValue Default 100;
    Property Min:Longint Read FMin Write SetMinValue Default 0;
    Property NeedleRadius:Integer Read FNeedleRadius Write SetNeedleRadius Default 70;
    Property NeedleWidth:Integer Read FNeedleWidth Write SetNeedleWidth Default 1;
    Property OnClick;
    Property OnDragDrop;
    Property OnDblClick;
    Property OnDragOver;
    Property OnEndDrag;
    Property OnEnter;
    Property OnExit;
    Property OnMouseDown;
    Property OnMouseMove;
    Property OnMouseUp;
    Property OnResize;
    Property OuterRadius:Integer Read FOuterRadius Write SetOuterRadius Default 110;
    Property ParentColor;
    Property ParentCtl3D;
    Property ParentFont;
    Property ParentShowHint;
    Property Progress: Longint Read FProgress Write SetPosition Default 0;
    Property SecondIndicationPoint:Longint Read FIndicationPoint2 Write SetIndicationPoint2 Default 90;
    Property SecondPartColor:TColor Read FColor2 Write SetColor2 Default clYellow;
    Property SectorDegree:Integer Read FSectorDegree Write SetSectorDegree Default 60;
    Property ShowHint;
    Property TabOrder;
    Property TabStop;
    Property Tag;
    Property ThirdPartColor:TColor Read FColor3 Write SetColor3 Default ClRed;
    Property Top;
    Property Visible;
    Property Width;
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
     FColor1:=clBlue;
     FColor2:=clYellow;
     FColor3:=clRed;
     Height:=300;
     FIndicationPoint1:=60;
     FIndicationPoint2:=90;
     FInnerRadius:=60;
     FMax:=100;
     FMin:=0;
     FNeedleRadius:=70;
     FNeedleWidth:=1;
     FOuterRadius:=110;
     FProgress:=0;
     FSectorDegree:=60;
     Width:=300;
     FNeedleDegree:=GetNeedleDegree(FMin,FMax,FProgress);
     Refresh;
end;

procedure TRingGauge.WMSize(var Message: TWMSize);
begin
	inherited;
		 if Width<130 Then Width:=130;
        if Height<130 Then Height:=130;
        if Width<Height Then begin
           FGaugeTop:=10;
           FGaugeLeft:=5;
    	   FGaugeWidth:=Width-10;
    	   FGaugeHeight:=Width-10;
           FOuterRadius:=(FGaugeWidth Div 2)-15;
           FInnerRadius:=(FGaugeWidth Div 2)-30;
           FNeedleRadius:=(FGaugeWidth Div 2)-25;
        end
        else begin
           FGaugeTop:=10;
           FGaugeLeft:=5;
	   FGaugeWidth:=Height-10;
	   FGaugeHeight:=Height-10;
           FOuterRadius:=(FGaugeWidth Div 2)-15;
           FInnerRadius:=(FGaugeWidth Div 2)-30;
           FNeedleRadius:=(FGaugeWidth Div 2)-25;
        end;
        Refresh;
end;

Procedure TRingGauge.Paint;
begin
     inherited Paint;
	  DrawCanvas;
end;

Procedure TRingGauge.DrawCanvas;
begin
     DrawGauge(FGaugeLeft,FGaugeTop,FGaugeLeft+FGaugeWidth,FGaugeTop+FGaugeHeight,
     FOuterRadius,FInnerRadius,FSectorDegree,FIndicationPoint1,FIndicationPoint2);
     FNeedleDegree:=GetNeedleDegree(FMin,FMax,FProgress);
     DrawNeedle(FNeedleDegree,FGaugeLeft,FGaugeTop,FGaugeLeft+FGaugeWidth,
     FGaugeTop+FGaugeHeight,FNeedleRadius);
end;

Procedure TRingGauge.SetNeedleWidth(Value:Integer);
begin
   if Value<>FNeedleWidth Then begin
        If Value<1 Then Value:=1;
        FNeedleWidth:=Value;
        DrawCanvas;
        Refresh;
   end;
end;

Procedure TRingGauge.SetColor1(Value:TColor);
begin
     IF Value<>FColor1 Then begin
        FColor1:=Value;
		 DrawCanvas;
		Refresh;
     end;
end;

Procedure TRingGauge.SetColor2(Value:TColor);
begin
     IF Value<>FColor2 Then begin
        FColor2:=Value;
        DrawCanvas;
        Refresh;
     end;
end;

Procedure TRingGauge.SetColor3(Value:TColor);
begin
     IF Value<>FColor3 Then begin
        FColor3:=Value;
        DrawCanvas;
        Refresh;
     end;
end;

Procedure TRingGauge.SetSectorDegree(Value:Integer);
begin
     if FSectorDegree<>Value Then Begin
		 IF Value>300 Then Value:=300;
        IF Value<6   Then Value:=6;
        FSectorDegree:=Value;
        DrawCanvas;
        Refresh;
     end;
end;

Procedure TRingGauge.SetNeedleRadius(Value:Integer);
begin
     if FNeedleRadius<>Value Then Begin
        FNeedleRadius:=Value;
        DrawCanvas;
        Refresh;
     end;
end;

Procedure TRingGauge.SetOuterRadius(Value:Integer);
begin
    if FOuterRadius<>Value Then Begin
       If  Value>(FGaugeWidth Div 2) Then Value:=(FGaugeWidth Div 2)-5;
       FOuterRadius:=Value;
       DrawCanvas;
       Refresh;
    end;
end;

Procedure TRingGauge.SetInnerRadius(Value:Integer);
begin
    if FInnerRadius<>Value Then Begin
       If  Value>FOuterRadius-10 Then Value:=FOuterRadius-10;
	   IF  Value<0 Then Value:=0;	
       FInnerRadius:=Value;
       DrawCanvas;
       Refresh;
    end;
end;

Procedure TRingGauge.SetIndicationPoint1(Value:LongInt);
begin
     if FIndicationPoint1<>Value Then Begin
        if Value >= (FIndicationPoint2-(FMax-FMin)*5 Div 100) then Value:=FIndicationPoint2-(FMax-FMin)*5 Div 100;
        if Value < FMin+((FMax-FMin)*5 Div 100) then Value:=FMin+((FMax-FMin)*5 Div 100);
        FIndicationPoint1:=Value;
        DrawCanvas;
        Refresh;
     end;
end;

Procedure TRingGauge.SetIndicationPoint2(Value:LongInt);
begin
     if FIndicationPoint2<>Value Then Begin
		 if Value <= (FIndicationPoint1+(FMax-FMin)*5 Div 100) then Value:=(FIndicationPoint1+(FMax-FMin)*5 Div 100);
        if Value > FMax-((FMax-FMin)*5 Div 100)  then Value:=FMax-((FMax-FMin)*5 Div 100) ;
        FIndicationPoint2:=Value;
        DrawCanvas;
        Refresh;
     end;
end;

Procedure TRingGauge.SetPosition(Value:LongInt);
begin
    if FProgress<>Value Then Begin
       IF Value>FMax Then Value:=FMax;
       IF Value<FMin Then Value:=FMin;
       DrawNeedle(FNeedleDegree,FGaugeLeft,FGaugeTop,FGaugeLeft+FGaugeWidth,
       FGaugeTop+FGaugeHeight,FNeedleRadius);
       FProgress:=Value;
       FNeedleDegree:=GetNeedleDegree(FMin,FMax,FProgress);
       DrawCanvas;
    end;
end;

Procedure TRingGauge.SetMinValue(Value:LongInt);
begin
     IF Value<>FMin Then Begin
        IF Value>=FMax-50 Then Value:=FMax-50;
        FIndicationPoint1:=(FMax-Value)*(FIndicationPoint1-FMin) div (FMax-FMin) + Value ;
		 FIndicationPoint2:=(FMax-Value)*(FIndicationPoint2-FMin) div (FMax-FMin) + Value ;
        FProgress:=(FMax-Value)*(FProgress-FMin) div (FMax-FMin) + Value ;
        FMin:=Value;
        FNeedleDegree:=GetNeedleDegree(FMin,FMax,FProgress);
        DrawCanvas;
        Refresh;
     End;
end;

Procedure TRingGauge.SetMaxValue(Value:LongInt);
begin
     IF Value<>FMax Then Begin
        IF Value<=FMin+50 Then Value:=FMin+50;
        FIndicationPoint1:=(Value-FMin)*(FIndicationPoint1-FMin) div (FMax-FMin)  + FMin ;
        FIndicationPoint2:=(Value-FMin)*(FIndicationPoint2-FMin) div (FMax-FMin)  + FMin ;
        FProgress:=(FMax-Value)*(FProgress-FMin) div (FMax-FMin) + FMin ;
        FMax:=Value;
        FNeedleDegree:=GetNeedleDegree(FMin,FMax,FProgress);
        DrawCanvas;
        Refresh;
     End;
end;

Function TRingGauge.DegToRad(inputDeg:Double):Double;
begin
     Result:=inputDeg*pi/180;
end;

Procedure TRingGauge.DrawGauge (x1,y1,x2,y2,OutR,InR,PieDeg,P1,P2:integer);
var
  Sx1,Sy1,Ex1,Ey1:integer;
  Sx2,Sy2,Ex2,Ey2:integer;
  Sx3,Sy3,Ex3,Ey3:integer;
  Sx4,Sy4,Ex4,Ey4:integer;
  P1x1,P1y1,P1x2,P1y2:integer;
  P2x1,P2y1,P2x2,P2y2:integer;
  Pnx1,Pny1,Pnx2,Pny2:integer;
  D1,D2,R,Idx:integer;
  P1Deg,P2Deg,PnDeg:integer;
  SimePieDeg:integer;

begin
  with Canvas Do begin
      {Initialize Pen}
      Pen.Color:=clBlack;
      Pen.Mode:=pmCopy;
      Pen.Width:=1;
      {Draw Arc}
      R:=GetR(x1,x2);
      D1:=R-OutR;
      D2:=R-InR;
      SimePieDeg:=PieDeg Div 2;
	   Sx1:=GetArcPointX(-SimePieDeg,x1,x2,OutR+2);
      Sy1:=GetArcPointY(-SimePieDeg,y1,y2,OutR+2);
      Ex1:=GetArcPointX(SimePieDeg,x1,x2,OutR+2);
      Ey1:=GetArcPointY(SimePieDeg,y1,y2,OutR+2);
      Sx2:=GetArcPointX(-SimePieDeg,x1,x2,InR-4);
      Sy2:=GetArcPointY(-SimePieDeg,y1,y2,InR-4);
      Ex2:=GetArcPointX(SimePieDeg,x1,x2,InR-4);
      Ey2:=GetArcPointY(SimePieDeg,y1,y2,InR-4);
      Sx3:=GetArcPointX(-SimePieDeg+2,x1,x2,OutR);
      Sy3:=GetArcPointY(-SimePieDeg+2,y1,y2,OutR);
      Ex3:=GetArcPointX(SimePieDeg-2,x1,x2,OutR);
      Ey3:=GetArcPointY(SimePieDeg-2,y1,y2,OutR);
      Sx4:=GetArcPointX(-SimePieDeg+2,x1,x2,InR);
      Sy4:=GetArcPointY(-SimePieDeg+2,y1,y2,InR);
      Ex4:=GetArcPointX(SimePieDeg-2,x1,x2,InR);
      Ey4:=GetArcPointY(SimePieDeg-2,y1,y2,InR);
      Arc(x1+D1,y1+D1,x2-D1,Y2-D1,Sx3,Sy3,Ex3,Ey3); {Draw Outer Arc}
      Arc(x1+D2,y1+D2,x2-D2,Y2-D2,Sx4,Sy4,Ex4,Ey4); {Draw Inner Arc}
      MoveTo(Sx1,Sy1);
      LineTo(Sx2,Sy2);
      MoveTo(Ex1,Ey1);
      LineTo(Ex2,Ey2);
      P1Deg:=SimePieDeg+(P1-FMin)*(360-PieDeg) Div (FMax-FMin);
      P2Deg:=SimePieDeg+(P2-FMin)*(360-PieDeg) Div (FMax-FMin);
      P1x1:=GetArcPointX(P1Deg,x1,x2,OutR);
      P1y1:=GetArcPointY(P1Deg,y1,y2,OutR);
	   P1x2:=GetArcPointX(P1Deg,x1,x2,InR-4);
      P1y2:=GetArcPointY(P1Deg,y1,y2,InR-4);
      P2x1:=GetArcPointX(P2Deg,x1,x2,OutR);
      P2y1:=GetArcPointY(P2Deg,y1,y2,OutR);
      P2x2:=GetArcPointX(P2Deg,x1,x2,InR-4);
      P2y2:=GetArcPointY(P2Deg,y1,y2,InR-4);
      MoveTo(P1x1,P1y1);
      LineTo(P1x2,P1y2);
      MoveTo(P2x1,P2y1);
      LineTo(p2x2,P2y2);
      Brush.Color:=FColor1;
      FloodFill(GetArcPointX(P1Deg-2,x1,x2,OutR-3),GetArcPointY(P1Deg-2,y1,y2,OutR-3),clBlack, fsBorder);
      Brush.Color:=FColor2;
      FloodFill(GetArcPointX(P1Deg+2,x1,x2,OutR-3),GetArcPointY(P1Deg+2,y1,y2,OutR-3),clBlack, fsBorder);
      Brush.Color:=FColor3;
      FloodFill(GetArcPointX(P2Deg+2,x1,x2,OutR-3),GetArcPointY(P2Deg+2,y1,y2,OutR-3),clBlack, fsBorder);
      {Draw Arc Shadow}
      Pen.Color:=clWhite;
      Pen.Width:=1;
      Arc(x1+D1-1,y1+D1-1,x2-D1+1,Y2-D1+1,Sx1,Sy1,Ex1,Ey1);
      {Draw Indicator}
      Pen.Color:=clBlack;
      Pen.Width:=1;
      Arc(x1+D2+3,y1+D2+3,x2-D2-3,Y2-D2-3,Sx2,Sy2,Ex2,Ey2);
      For Idx:=0 To 19 do begin
            PnDeg:=SimePieDeg+Idx*5*(360-PieDeg) Div 100;
			 Pnx1:=GetArcPointX(PnDeg,x1,x2,InR-3);
            Pny1:=GetArcPointY(PnDeg,y1,y2,InR-3);
            if Idx Mod 2 = 0 then begin
               Pnx2:=GetArcPointX(PnDeg,x1,x2,InR-12);
               Pny2:=GetArcPointY(PnDeg,y1,y2,InR-12);
            end
            else begin
               Pnx2:=GetArcPointX(PnDeg,x1,x2,InR-8);
               Pny2:=GetArcPointY(PnDeg,y1,y2,InR-8);
            end;
            MoveTo(Pnx1,Pny1);
            LineTo(Pnx2,Pny2);
      end;
      PnDeg:=SimePieDeg+(360-PieDeg);
      Pnx1:=GetArcPointX(PnDeg,x1,x2,InR-3);
      Pny1:=GetArcPointY(PnDeg,y1,y2,InR-3);
      Pnx2:=GetArcPointX(PnDeg,x1,x2,InR-12);
      Pny2:=GetArcPointY(PnDeg,y1,y2,InR-12);
      MoveTo(Pnx1,Pny1);
      LineTo(Pnx2,Pny2);
  end;
end;

Procedure TRingGauge.DrawNeedle(Deg,x1,y1,x2,y2,R:integer);
var
   rx,ry:integer;
	x,y:integer;
begin
  with Canvas Do begin
      rx:=GetRx(x1,x2);
      ry:=GetRy(y1,y2);
      x:=GetArcPointX(Deg,x1,x2,R);
      y:=GetArcPointY(Deg,y1,y2,R);
      Pen.Color:=clBlack;
      Pen.Mode:=pmNot;
      Pen.Width:=FNeedleWidth;
      MoveTo(rx,ry);
      LineTo(x,y);
      Pen.Mode:=pmCopy;
  end;
end;

Function TRingGauge.GetR(x1,x2:integer):integer;
begin
     Result:=Abs(x2-x1) div 2;
end;

Function TRingGauge.GetArcPointX(Deg:double;x1,x2,d:integer):integer;
var
   tmpint:integer;
begin
     tmpint:=Round(d*Sin(DegToRad(Deg)));
	  Result:=GetRx(x1,x2)-tmpint;
end;

Function TRingGauge.GetArcPointY(Deg:double;y1,y2,d:integer):integer;
var
   tmpint:integer;
begin
      tmpint:=integer(Round(d*cos(DegToRad(Deg))));
      Result:=GetRy(y1,y2)+tmpint;
end;

Function TRingGauge.GetRx(x1,x2:integer):Integer;
begin
     Result:=x1+(x2-x1) div 2;
end;

Function TRingGauge.GetRy(y1,y2:integer):Integer;
begin
     Result:=y1+(y2-y1) div 2;
end;

Function  TRingGauge.GetNeedleDegree(Min,Max,Progress:LongInt):Integer;
begin
     Result:= (((360-SectorDegree)*(Progress-Min)) div (Max-Min))+(SectorDegree Div 2)
end;

end.
