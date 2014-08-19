unit Credits;

{*******************************************************************************
*  TScrollingCredits version 1.0                                               *
********************************************************************************
* Author: Raoul Snyman                                                         *
* ---------------------------------------------------------------------------- *
* E-Mail: components@saturnlaboratories.gq.nu                                  *
* ---------------------------------------------------------------------------- *
* Copyright: ©2000 Saturn Laboratories, All rights reserved.                   *
* ---------------------------------------------------------------------------- *
* Description: TScrollingCredits is a component which displays scrolling       *
*              credits like at the end of movies, videos, etc.                 *
********************************************************************************
* This component is FREEWARE.                                                  *
* ---------------------------------------------------------------------------- *
* Please let me know if you find it useful!!                                   *
* ---------------------------------------------------------------------------- *
* It may be used for commercial purposes on the condition that you give me     *
* credit (i.e. place it in your credits list).                                 *
* ---------------------------------------------------------------------------- *
* If used in freeware, it's not necessary to give me credit, although it would *
* be appreciated.                                                              *
* ---------------------------------------------------------------------------- *
* If you modify this code, please send me an e-mail with a copy of the code    *
* attached, letting me know what it was that you changed/modified/added.       *
*******************************************************************************}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls;

type
  TScrollingCredits = class(TGraphicControl)
  private
    { Private declarations }
    FCredits: TStringList;
    FFont: TFont;
    FBackgroundColor: TColor;
    FBorderColor: TColor;
	 FAnimate: Boolean;
    FInterval: Cardinal;
    FTimer: TTimer;
    YPos: integer;
    TPos: integer;
    FBitmap: TBitmap;
  protected
    { Protected declarations }
    procedure SetCredits(Value: TStringList);
    procedure SetFont(Value: TFont);
    procedure SetBackgroundColor(Value: TColor);
    procedure SetBorderColor(Value: TColor);
    procedure SetAnimate(Value: Boolean);
    procedure SetInterval(Value: Cardinal);
    procedure Animation;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure TimerFired(Sender: TObject);
    procedure Reset;
  published
    { Published declarations }
    property Credits: TStringList read FCredits write SetCredits;
    property CreditsFont: TFont read FFont write SetFont;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor;
    property BorderColor: TColor read FBorderColor write SetBorderColor;
    property Animate: Boolean read FAnimate write SetAnimate;
	 property Interval: Cardinal read FInterval write SetInterval;
	 //Eventos
	 property OnMouseDown;
  end;

procedure Register;

implementation

constructor TScrollingCredits.Create(AOwner: TComponent);
//----------------------------------------------------------------------------------------------------------------------
begin
	inherited Create(AOwner);
	Width := 305;
	Height := 201;
	FCredits := TStringList.Create;
	FFont := TFont.Create;
	FTimer := TTimer.Create(Self);
	FBitmap := TBitmap.Create;
	FCredits.Add('TScrollingCredits');
	FCredits.Add('¯¯¯¯¯¯¯¯¯¯¯¯¯¯');
	FCredits.Add('Copyright ©2000 Saturn Laboratories');
	FCredits.Add('');
	FCredits.Add('Please let me know if you find');
	FCredits.Add('this component useful');
	FCredits.Add('components@saturnlaboratories.gq.nu');
	FFont.Color := clWhite;
	FFont.Name := 'Tahoma';
	FTimer.Interval := 50;
	FTimer.Enabled := False;
	FTimer.OnTimer := TimerFired;
	FBitmap.Width := Width;
	FBitmap.Height := Height;
	FBackgroundColor := clBlack;
	FBorderColor := clWhite;
	FAnimate := False;
	FInterval := 50;
	YPos := 4;
	TPos := 0;
end;

destructor TScrollingCredits.Destroy;
//----------------------------------------------------------------------------------------------------------------------
begin
	FBitmap.Free;
	FTimer.Free;
	FFont.Free;
	FCredits.Free;
	inherited Destroy;
end;

procedure TScrollingCredits.Paint;
//----------------------------------------------------------------------------------------------------------------------
var
	I, X, Y: integer;
begin
	inherited Paint;
	FBitmap.Width := Width;
	FBitmap.Height := Height;
	with FBitmap do begin
		Canvas.Font := FFont;
		Canvas.Brush.Style := bsSolid;
		Canvas.Brush.Color := FBackgroundColor;
		Canvas.FillRect(Rect(0, 0, Width, Height));
		Canvas.Brush.Style := bsClear;
		Canvas.Pen.Color := FBorderColor;
		Canvas.Rectangle(0, 0, Width, Height);
		if FAnimate then begin
			Animation
		end else begin
			Y := 0;
			//Fit := Height div Canvas.TextHeight('A');
			for I := 0 to FCredits.Count-1 do begin
				X := (Width div 2) - (Canvas.TextWidth(FCredits.Strings[I]) div 2);
				Canvas.TextOut(X, YPos+Y, FCredits.Strings[I]);
				Y := Y + Canvas.TextHeight('A');
			end;
		 end;
		Canvas.Rectangle(0, 0, Width, Height);
	end;
	Self.Canvas.Draw(0, 0, FBitmap);
end;

procedure TScrollingCredits.SetCredits(Value: TStringList);
//----------------------------------------------------------------------------------------------------------------------
begin
	FCredits.Assign(Value);
	Paint;
end;

procedure TScrollingCredits.SetFont(Value: TFont);
//----------------------------------------------------------------------------------------------------------------------
begin
	FFont.Assign(Value);
	Paint;
end;

procedure TScrollingCredits.SetBackgroundColor(Value: TColor);
//----------------------------------------------------------------------------------------------------------------------
begin
	FBackgroundColor := Value;
	Paint;
end;

procedure TScrollingCredits.SetBorderColor(Value: TColor);
//----------------------------------------------------------------------------------------------------------------------
begin
	FBorderColor := Value;
	Paint;
end;

procedure TScrollingCredits.SetAnimate(Value: Boolean);
//----------------------------------------------------------------------------------------------------------------------
begin
	TPos := Height + Canvas.TextHeight('A');
	FAnimate := Value;
	FTimer.Enabled := Value;
	Paint;
end;

procedure TScrollingCredits.Animation;
//----------------------------------------------------------------------------------------------------------------------
var
	I, X, Y: integer;
begin
	FBitmap.Width := Width;
	FBitmap.Height := Height;
	with FBitmap do begin
		Canvas.Pen.Color := FBorderColor;
		Canvas.Brush.Style := bsSolid;
		Canvas.Brush.Color := FBackgroundColor;
		Canvas.Rectangle(0, 0, Width, Height);
		Canvas.Brush.Style := bsClear;
		Canvas.Font := FFont;
		Y := TPos;
		for I := 0 to FCredits.Count-1 do begin
			if I >= 0 then begin
				X := (Width div 2) - (Canvas.TextWidth(FCredits.Strings[I]) div 2);
				Canvas.TextOut(X, Y, FCredits.Strings[I]);
				Y := Y + Canvas.TextHeight('A');
			end else begin
				Y := Y + Canvas.TextHeight('A');
			end;
		end;
		Canvas.Rectangle(0, 0, Width, Height);
	end;
	if not FBitmap.Empty then begin
		Self.Canvas.Draw(0, 0, FBitmap);
	end;
end;

procedure TScrollingCredits.TimerFired(Sender: TObject);
//----------------------------------------------------------------------------------------------------------------------
begin
	Canvas.Font := FFont;
	if TPos < (0 - (FCredits.Count * Canvas.TextHeight('A'))) then begin
		TPos := Height + Canvas.TextHeight('A')
	end else begin
		TPos := TPos - 1;
	end;
	Animation;
end;

procedure TScrollingCredits.SetInterval(Value: Cardinal);
//----------------------------------------------------------------------------------------------------------------------
begin
	FInterval := Value;
	FTimer.Interval := Value;
end;

procedure TScrollingCredits.Reset;
//----------------------------------------------------------------------------------------------------------------------
begin
	Canvas.Font := FFont;
	TPos := Height + Canvas.TextHeight('A');
end;

procedure Register;
//----------------------------------------------------------------------------------------------------------------------
begin
	RegisterComponents('FX Controls', [TScrollingCredits]);
end;

end.
 