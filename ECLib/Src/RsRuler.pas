{$IFDEF RsRuler }
	{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I ECLib.inc}

unit RsRuler;

{{
  Delphi 2-5 Ruler component, version 1.0, 22 nov 2000

  (c) 2000 Hans Roos, Roos Software, The Netherlands
  Website: www.RoosSoftware.nl
  Email: mail@roossoftware.nl

  Features:
  4 layouts rdTop, rdLeft, rdRight and rdBottom with
    automatic scale adjustments for each layout
  Scale: from 1-1000
  Units: Inches, Centimetres, Millimetres
  Automatic calculation of scalenumbers (no overlapping)
  Sideways text for vertical layouts
  Flat or 3D appearance
  TRsRulerCorner: extra component for joining up to 4
    rulers, can show the unit ('cm', 'mm' or 'in')

  See demo project for usage
  Licence: Freeware! Use in non-commercial or commercial apps
  Feel free to modify the source for your own needs, but don't remove
  my name from this file, please.
  If you find this component useful, please let me know.
  Don't send money, just be grateful ;)

  Known issues: None
  Future expansions: Hairline property indicating cursor position
  Better scale divisions when Inches are used
  (is it customary to divide inches in 4ths, 8ths, 16ths etc?)
  Use custom colors/fonts
  Anything YOU can think of; please let me know!! (mail@roossoftware.nl)
}

interface

uses
	Windows, SysUtils, Messages, Classes, Graphics, Controls, Forms;

const
	Centi: String = 'cm';
	Milli: String = 'mm';
	Inch: String  = 'in';
	None: String  = '';

type
	TRulerDir = (rdTop, rdLeft, rdRight, rdBottom);
	TRulerUnit = (ruCenti, ruMilli, ruInch, ruNone);
	TCornerPos = (cpLeftTop, cpRightTop, cpLeftBottom, cpRightBottom);

	TRsRuler = Class (TGraphicControl)
	private
		fDirection : TRulerDir;
		fUnits : TRulerUnit;
		fScale : Integer;
		fScaleFactor : Double;
		fAdvance : Double;
		fFlat :  Boolean;
		procedure SetDirection(const Value : TRulerDir);
		procedure SetScale(const Value : Integer);
		procedure SetUnit(const Value : TRulerUnit);
		procedure SetFlat(const Value : Boolean);
	protected
		LeftSideLF, RightSideLF, NormLF : TLogFont;
		NormFont, LeftSideFont, RightSideFont : HFont;
		FirstTime : Boolean;
		procedure CalcAdvance;
		procedure PaintScaleTics;
		procedure PaintScaleLabels;
		procedure Paint; override;
	public
		constructor Create(AOwner : TComponent); override;
		destructor Destroy; override;
	published
		property Align;
		property Direction : TRulerDir read fDirection write SetDirection;
		property Units : TRulerUnit read fUnits write SetUnit;
		property Scale : Integer read fScale write SetScale;
		property Flat : Boolean read fFlat write SetFlat;
		property Height;
		property Width;
		property Visible;
		property Hint;
		property ShowHint;
		property Tag;
		property OnMouseDown;
		property OnMouseMove;
		property OnMouseUp;
		property OnClick;
		property OnResize;
	end;

	TRsRulerCorner = Class (TGraphicControl)
	private
		fPosition : TCornerPos;
		fFlat : Boolean;
		fUnits : TRulerUnit;
		procedure SetPosition(const Value : TCornerPos);
		procedure SetFlat(const Value : Boolean);
		procedure SetUnits(const Value : TRulerUnit);
	protected
		fUStr : String;
		procedure Paint; override;
	public
		constructor Create(AOwner : TComponent); override;
	published
		property Align;
		property Position : TCornerPos read fPosition write SetPosition;
		property Flat : Boolean read fFlat write SetFlat;
		property Units : TRulerUnit read fUnits write SetUnits;
		property Height;
		property Width;
		property Visible;
		property Hint;
		property ShowHint;
		property Tag;
		property OnMouseDown;
		property OnMouseMove;
		property OnMouseUp;
		property OnClick;
		property OnResize;
	end;


implementation


constructor TRsRuler.Create(AOwner : TComponent);
begin
	inherited;
	fDirection := rdTop;
	fUnits := ruCenti;
	fScale := 100;
	Color  := clBtnFace;
	Height := 33;
	Width  := 200;
	fScaleFactor := 1;
	fAdvance := 1;
	with LeftSideLF do begin
		FillChar(LeftSideLF, SizeOf(LeftSideLF), 0);
		lfHeight := 11;
		lfEscapement := 900;
		lfOrientation := 900;
		StrPCopy(lfFaceName, 'Tahoma');
	end;
	with RightSideLF do begin
		FillChar(RightSideLF, SizeOf(RightSideLF), 0);
		lfHeight := 11;
		lfEscapement := 2700;
		lfOrientation := 2700;
		StrPCopy(lfFaceName, 'Tahoma');
	end;
	with NormLF do begin
		FillChar(NormLF, SizeOf(NormLF), 0);
		lfHeight := 11;
		StrPCopy(lfFaceName, 'Tahoma');
	end;
	FirstTime := TRUE;
	fFlat := FALSE;
end;

destructor TRsRuler.Destroy;
begin
	DeleteObject(NormFont);
	DeleteObject(LeftSideFont);
	DeleteObject(RightSideFont);
	inherited;
end;

procedure TRsRuler.CalcAdvance;
begin
	fAdvance := Screen.PixelsPerInch / 10 * Scale / 100;
	if fUnits <> ruInch then begin
		fAdvance := fAdvance / 2.54;
	end;
	case Scale of
		1	: begin
			fScaleFactor := 100;
		end;
		2	: begin
			fScaleFactor := 50;
		end;
		3..5	: begin
			fScaleFactor := 25;
		end;
		6..8	: begin
			fScaleFactor := 20;
		end;
		9..12	: begin
			fScaleFactor := 10;
		end;
		13..25	: begin
			fScaleFactor := 5;
		end;
		26..35	: begin
			fScaleFactor := 4;
		end;
		36..50	: begin
			fScaleFactor := 2;
		end;
		51..125	: begin
			fScaleFactor := 1;
		end;
		126..300	: begin
			fScaleFactor := 0.5;
		end;
		301..400	: begin
			fScaleFactor := 0.25;
		end;
		401..500	: begin
			fScaleFactor := 0.2;
		end;
		501..1000	: begin
			fScaleFactor := 0.1;
		end;
	end;
	fAdvance := fAdvance * fScaleFactor;
end;

procedure TRsRuler.PaintScaleTics;
var
	Pos : Double;
	N, Last, LongTick : Integer;
begin
	if (fDirection = rdTop) or (fDirection = rdBottom) then begin
		Last := Width;
	end else begin
		Last := Height;
	end;
	Pos := 0;
	N := 0;
	while Pos < Last do begin
		with Canvas do begin
			LongTick := 2 * (3 + Integer(N mod 5 = 0));
			if (fDirection = rdTop) or (fDirection = rdBottom) then begin
				if fDirection = rdTop then begin
					MoveTo(Trunc(Pos), Height - 1);
					LineTo(Trunc(Pos), Height - LongTick);
				end;
				if fDirection = rdBottom then begin
					MoveTo(Trunc(Pos), 1);
					LineTo(Trunc(Pos), LongTick);
				end;
			end else begin
				if fDirection = rdLeft then begin
					MoveTo(Width - 1, Trunc(Pos));
					LineTo(Width - LongTick, Trunc(Pos));
				end;
				if fDirection = rdRight then	  begin
					MoveTo(1, Trunc(Pos));
					LineTo(LongTick, Trunc(Pos));
				end;
			end;
			Inc(N);
			Pos := Pos + 2 * fAdvance; // always advance two units to next ticmark
		end;
	end;
end;

procedure TRsRuler.PaintScaleLabels;
var
	Pos, Number : Double;
	N, Last, Wi, He : Integer;
	S : String;
begin
	if (fDirection = rdTop) or (fDirection = rdBottom) then begin
		Last := Width;
	end else begin
		Last := Height;
	end;
	Pos := 0;
	N := 0;
	while Pos < Last do begin
		with Canvas do begin
			Number := fScaleFactor * N / 10;
			if Units = ruMilli then begin
				Number := 10 * Number;
			end;
			S := FloatToStr(Number);
			Wi := TextWidth(S);
			He := TextHeight(S);
			if (fDirection = rdTop) or (fDirection = rdBottom) then	begin
				MoveTo(Trunc(Pos), 1);  // only Pos is important
				if fDirection = rdTop then	  begin
					if (N > 0) and (N mod 10 = 0) then begin
						TextOut(PenPos.X - Wi div 2, Height - He - 8, S);
					end else if (N > 0) and (N mod 5 = 0) then		begin
						MoveTo(Trunc(Pos), Height - 12);
						LineTo(Trunc(Pos), Height - 16);
					end;
				end;
				if fDirection = rdBottom then	  begin
					if (N > 0) and (N mod 10 = 0) then begin
						TextOut(PenPos.X - Wi div 2, 8, S);
					end else if (N > 0) and (N mod 5 = 0) then		begin
						MoveTo(Trunc(Pos), 12);
						LineTo(Trunc(Pos), 16);
					end;
				end;
			end else begin
				MoveTo(1, Trunc(Pos));
				if fDirection = rdLeft then	  begin
					if (N > 0) and (N mod 10 = 0) then begin
						TextOut(Width - He - 8, PenPos.Y + Wi div 2, S);
					end else if (N > 0) and (N mod 5 = 0) then		begin
						MoveTo(Width - 12, Trunc(Pos));
						LineTo(Width - 16, Trunc(Pos));
					end;
				end;
				if fDirection = rdRight then	  begin
					if (N > 0) and (N mod 10 = 0) then begin
						TextOut(He + 8, PenPos.Y - Wi div 2, S);
					end else if (N > 0) and (N mod 5 = 0) then		begin
						MoveTo(12, Trunc(Pos));
						LineTo(16, Trunc(Pos));
					end;
				end;
			end;
			Inc(N);
			Pos := Pos + fAdvance;
		end;
	end;
end;

procedure TRsRuler.Paint;
var
	Rect : TRect;
	He : Integer;
begin
	inherited;
	if FirstTime then  begin
		FirstTime := FALSE;
		LeftSideFont := CreateFontIndirect(LeftSideLF);
		RightSideFont := CreateFontIndirect(RightSideLF);
		NormFont  := CreateFontIndirect(NormLF);
	end;
	Rect := ClientRect;
	if not Flat then begin
		DrawEdge(Canvas.Handle, Rect, EDGE_RAISED, BF_RECT);
	end;
	He := Canvas.TextHeight('0') + 6;
	if (fDirection = rdTop) or (fDirection = rdBottom) then   begin
		if (fDirection = rdTop) then begin
			SetRect(Rect, 2, Height - He, Width - 2, Height - 8);
		end;
		if (fDirection = rdBottom) then begin
			SetRect(Rect, 2, 8, Width - 2, He);
		end;
		SelectObject(Canvas.Handle, NormFont);
	end else begin
		if fDirection = rdLeft then	begin
			SetRect(Rect, Width - He, 2, Width - 8, Height - 2);
			SelectObject(Canvas.Handle, LeftSideFont);
		end;
		if fDirection = rdRight then	begin
			SetRect(Rect, He, 2, 8, Height - 2);
			SelectObject(Canvas.Handle, RightSideFont);
		end;
	end;
	Canvas.Brush.Color := clWindow;
	Canvas.FillRect(Rect);
	CalcAdvance;
	SetBKMode(Canvas.Handle, TRANSPARENT);
	PaintScaleTics;
	PaintScaleLabels;
	SetBKMode(Canvas.Handle, OPAQUE);
end;

procedure TRsRuler.SetDirection(const Value : TRulerDir);
var
	Dim : TPoint;
	OldDir : TRulerDir;
begin
	OldDir := fDirection;
	if Value <> fDirection then  begin
		if ((OldDir = rdTop) or (OldDir = rdBottom)) and ((Value = rdLeft) or (Value = rdRight))
			or ((OldDir = rdLeft) or (OldDir = rdRight)) and ((Value = rdTop) or (Value = rdBottom)) then	begin
			Dim := Point(Width, Height);
			Width := Dim.Y;
			Height := Dim.X;
		end;
		fDirection := Value;
		Invalidate;
	end;
end;

procedure TRsRuler.SetScale(const Value : Integer);
begin
	if (Value <> fScale) and (Value > 0) then  begin
		fScale := Value;
		Invalidate;
	end;
end;

procedure TRsRuler.SetUnit(const Value : TRulerUnit);
begin
	if Value <> fUnits then  begin
		fUnits := Value;
		Invalidate;
	end;
end;

procedure TRsRuler.SetFlat(const Value : Boolean);
begin
	if Value <> fFlat then  begin
		fFlat := Value;
		Invalidate;
	end;
end;

{ TRsRulerCorner }

constructor TRsRulerCorner.Create(AOwner : TComponent);
begin
	inherited;
	fPosition := cpLeftTop;
	fFlat := FALSE;
	fUnits := ruCenti;
	fUStr := Centi;
	Width := 24;
	Height := 24;
end;

procedure TRsRulerCorner.Paint;
var
	OrgH, Wi, He : Integer;
	R : TRect;
begin
	inherited;
	R := ClientRect;
	with Canvas do begin
		if not Flat then begin
			DrawEdge(Handle, R, EDGE_RAISED, BF_RECT);
		end;
		Brush.Color := clWindow;
		He := TextHeight('0') + 6;
		Font.Name := 'Tahoma';
		OrgH := Font.Height;
		Font.Height := 11;
		SetBKMode(Handle, TRANSPARENT);
		Font.Color := clBtnShadow;
		Wi := TextWidth(fUStr);
		if fPosition = cpLeftTop then	begin
			FillRect(Rect(Width - He, Height - He, Width - 2, Height - 8));
			FillRect(Rect(Width - He, Height - He, Width - 8, Height - 2));
			TextOut(Width - He + 1 + (He - 2 - Wi) div 2, Height - He, fUStr);
		end;
		if fPosition = cpRightTop then	begin
			FillRect(Rect(2, Height - He, He, Height - 8));
			FillRect(Rect(8, Height - He, He, Height - 2));
			TextOut(2 + (He - Wi) div 2, Height - He, fUStr);
		end;
		if fPosition = cpLeftBottom then	begin
			FillRect(Rect(Width - He, 8, Width - 2, He));
			FillRect(Rect(Width - He, 2, Width - 8, He));
			TextOut(Width - He + 1 + (He - 2 - Wi) div 2, 8, fUStr);
		end;
		if fPosition = cpRightBottom then	begin
			FillRect(Rect(2, 8, He, He));
			FillRect(Rect(8, 2, He, He));
			TextOut(2 + (He - Wi) div 2, 8, fUStr);
		end;
	end;
	Canvas.Font.Height := OrgH;
	SetBKMode(Canvas.Handle, OPAQUE);
end;

procedure TRsRulerCorner.SetFlat(const Value : Boolean);
begin
	if Value <> fFlat then  begin
		fFlat := Value;
		Invalidate;
	end;
end;

procedure TRsRulerCorner.SetPosition(const Value : TCornerPos);
begin
	if Value <> fPosition then  begin
		fPosition := Value;
		Invalidate;
	end;
end;

procedure TRsRulerCorner.SetUnits(const Value : TRulerUnit);
begin
	if Value <> fUnits then  begin
		fUnits := Value;
		if fUnits = ruCenti then begin
			fUStr := Centi;
		end;
		if fUnits = ruMilli then begin
			fUStr := Milli;
		end;
		if fUnits = ruInch then begin
			fUStr := Inch;
		end;
		if fUnits = ruNone then begin
			fUStr := None;
		end;
		Invalidate;
	end;
end;

end.


