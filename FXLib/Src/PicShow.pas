{$IFDEF DEBUG_DCU}
	{$DEFINE PicShow}
{$ENDIF}
{$IFDEF DateOper}
{$A+,B-,C-,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O-,P+,Q-,R+,S-,T-,U-,V+,W-,X+,Y+,Z4}
{$DEBUGINFO ON}
{$ELSE}
{$A+,B-,C-,D-,E-,F-,G+,H-,I-,J-,K-,L-,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y-,Z1}
{$F+}
{$ENDIF}

unit PicShow;

{

 TPicShow v2.0
 by Kambiz R. Khojasteh

 email: khojasteh@www.dci.co.ir
 web: http://www.geocities.com/k_khojasteh/

 This component is freeware and may be used in any software
 product (free or commercial) under the condition that I'm
 given proper credit (title, name and e-mail address in the
 documentation or the About box of the product this component
 is used in).

 Thanks to M. R. Zamani for adding 8 effects.
 email: M_R_Zamani@yahoo.com

 Special thanks to:
   k3nx@hotmail.com
   Douglass Tietjen (support@delhipages.com)
   Jerry McLain (jkmclain@cyberstation.net)

}

{$IFNDEF VER80} { Delphi 1.0 }
  {$IFNDEF VER90} { Delphi 2.0 }
    {$IFNDEF VER100} { Delphi 3.0 }
       {$DEFINE PS_HiD4}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Menus, DesignIntf;

type

  {$IFNDEF PS_HiD4}
  HRgn = THandle;
  {$ENDIF}

  TShowStyle = 0..117;
  TPrecent = 0..100;
  TBackgroundMode = (bmNone, bmTiled, bmStretched, bmCentered);

  TCustomDrawEvent = procedure(Sender: TObject; Picture, Screen: TBitmap;
    var UpdatedRgn: HRgn) of object;

  TAbout = class(TObject);

{ TPicShow }

  TPicShow = class(TCustomControl)
  private
    { Private declarations }
    fAbout: TAbout;
    fPicture: TPicture;
    fBgPicture: TPicture;
    fBgMode: TBackgroundMode;
    fAutoSize: Boolean;
    fCenter: Boolean;
    fStretch: Boolean;
    fStretchFine: Boolean;
    fThreaded: Boolean;
    fThreadPriority: TThreadPriority;
    fManual: Boolean;
    fStyle: TShowStyle;
    fStep: Word;
    fDelay: Word;
    fOnChange: TNotifyEvent;
    fOnProgress: TNotifyEvent;
	 fOnComplete: TNotifyEvent;
    fOnCustomDraw: TCustomDrawEvent;
    fBusy: Boolean;
    fProgress: TPrecent;
    Media: TBitmap;
    PicRect: TRect;
    Thread: TThread;
    Drawing: Boolean;
    OffScreen: TBitmap;
    Stopping: Boolean;
    OldPic: TBitmap;
    Pic: TBitmap;
	 procedure SetPicture(Value: TPicture);
    procedure SetBgPicture(Value: TPicture);
    procedure SetBgMode(Value: TBackgroundMode);
    procedure SetCenter(Value: Boolean);
    procedure SetStretch(Value: Boolean);
    procedure SetStretchFine(Value: Boolean);
    procedure SetStep(Value: Word);
    procedure SetProgress(Value: TPrecent);
    procedure SetManual(Value: Boolean);
    function GetEmpty: Boolean;
    procedure AnimationComplete(Sender: TObject);
    procedure PictureChange(Sender: TObject);
    procedure BgPictureChange(Sender: TObject);
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
	 procedure AdjustClientSize;
    procedure CalculatePicRect;
    procedure InvalidateArea(Area: TRect);
    procedure Prepare;
    procedure Animate;
    procedure UpdateDisplay;
  protected
    { Protected declarations }
	 procedure Paint; override;
	 procedure SetAutoSize(Value: Boolean); override;
  public
	 { Public declarations }
	 constructor Create(AOwner: TComponent); override;
	 destructor Destroy; override;
	 procedure Execute;
	 procedure Stop;
	 procedure Clear;
	 property Busy: Boolean read fBusy;
	 property Empty: Boolean read GetEmpty;
	 property Progress: TPrecent read fProgress write SetProgress;
  published
    { Published declarations }
    property About: TAbout read fAbout write fAbout stored False;
    property Align;
    {$IFDEF PS_HiD4}
    property Anchors;
    {$ENDIF}
    property AutoSize: Boolean read fAutoSize write SetAutoSize default True;
    property BgMode: TBackgroundMode read fBgMode write SetBgMode default bmTiled;
    property BgPicture: TPicture read fBgPicture write SetBgPicture;
	 property Center: Boolean read fCenter write SetCenter default False;
    property Color;
    property Delay: Word read fDelay write fDelay default 20;
    property DragCursor;
    {$IFDEF PS_HiD4}
    property DragKind;
    {$ENDIF}
    property DragMode;
    property Enabled;
    property Height default 100;
    property Manual: Boolean read fManual write SetManual default False;
    property ParentColor;
    property ParentShowHint;
    property Picture: TPicture read fPicture write SetPicture;
    property PopupMenu;
    property ShowHint;
    property Stretch: Boolean read fStretch write SetStretch default False;
    property StretchFine: Boolean read fStretchFine write SetStretchFine default False;
    property Step: Word read fStep write SetStep default 4;
    property Style: TShowStyle read fStyle write fStyle default 51;
    property Threaded: Boolean read fThreaded write fThreaded default True;
    property ThreadPriority: TThreadPriority read fThreadPriority write fThreadPriority default tpNormal;
    property Visible;
    property Width default 100;
    property OnClick;
    property OnCustomDraw: TCustomDrawEvent read fOnCustomDraw write fOnCustomDraw;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
	 property OnEndDrag;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
    property OnComplete: TNotifyEvent read fOnComplete write fOnComplete;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnProgress: TNotifyEvent read fOnProgress write fOnProgress;
  end;

function CreateTriangleRgn(x1, y1, x2, y2, x3, y3: Integer): HRgn;
function ScaleImageToRect(IR, R: TRect): TRect;
procedure DrawTiledImage(Canvas: TCanvas; Rect: TRect; G: TGraphic);
procedure MirrorCopyRect(Canvas: TCanvas; dstRect: TRect; Bitmap: TBitmap;
  srcRect: TRect; Horz, Vert: Boolean);

procedure Register;

implementation

uses
	DesignEditors;
	{ If you are using using Delphi 5.0, you will find this unit in the Source\Toolsapi directory of Delphi. Copy it to Lib
	directory. Thanks to k3nx@hotmail.com for this comment. }

type

{ TAnimateThread }

  TAnimateThread = class(TThread)
  private
	 PicShow: TPicShow;
    procedure Update;
  public
	 constructor Create(APicShow: TPicShow);
	 procedure Execute; override;
  end;

{$WARNINGS OFF}

{ TPicShowComponentEditor }

  TPicShowComponentEditor = class(TDefaultEditor)
	 procedure ExecuteVerb(Index: Integer); override;
	 function GetVerbCount: Integer; override;
	 function GetVerb(Index: Integer): string;
  end;

{ TAboutPropertyEditor }

  TAboutPropertyEditor = class(TStringProperty)
  public
	 procedure Edit; override;
	 function GetValue: string;
	 function GetAttributes: TPropertyAttributes; override;
  end;

{$WARNINGS ON}

{ Miscellaneous routines }

function CreateBarRgn(X, Y, W, H, S: Integer; XMode, YMode: Byte): HRgn;
//----------------------------------------------------------------------------------------------------------------------
var
	X1, Y1: Integer;
	Rgn, tRgn: HRgn;
begin
	Result := 0;
	Rgn := 0;
	if X <= W then begin
		Y1 := 0
	end else begin
		Y1 := 5;
	end;
	while Y1 < H do begin
		if X > W then begin
		  if XMode in [1, 4] then begin
			  Rgn := CreateRectRgn(2 * W - X, Y1, W, Y1 + 5)
		  end else begin
			  if XMode in [2, 5] then begin
				  Rgn := CreateRectRgn(0, Y1, X - W, Y1 + 5);
			  end;
		  end;
		end else begin
		  if (X + S) > W then begin
			  X := W;
		  end;
		  if XMode in [1, 5] then begin
			  Rgn := CreateRectRgn(W - X, Y1, W, Y1 + 5)
		  end else begin
			  if XMode in [2, 4] then begin
				  Rgn := CreateRectRgn(0, Y1, X, Y1 + 5)
			  end else begin
				  if XMode = 3 then begin
					  tRgn := CreateRectRgn(W - X, Y1, W, Y1 + 5);
					  Rgn := CreateRectRgn(0, Y1 + 5, X, Y1 + 10);
					  CombineRgn(Rgn, Rgn, tRgn, RGN_OR);
					  DeleteObject(tRgn);
				  end;
			  end;
		  end;
		end;
		if Result <> 0 then begin
			  CombineRgn(Result, Result, Rgn, RGN_OR);
			  DeleteObject(Rgn);
		end else begin
			  Result := Rgn;
		end;
		Inc(Y1, 10);
	end;
	if Y <= H then begin
		X1 := 0
	end else begin
		X1 := 5;
	end;
	while X1 < W do begin
		if Y > H then begin
		  if YMode in [1, 4] then begin
			  Rgn := CreateRectRgn(X1, 2 * H - Y, X1 + 5, H);
		  end else begin
			  if YMode in [2, 5] then begin
				  Rgn := CreateRectRgn(X1, 0, X1 + 5, Y - H);
			  end;
		  end;
		end else begin
		  if (Y + S) > H then begin
			  Y := H;
		  end;
		  if YMode in [1, 5] then begin
			  Rgn := CreateRectRgn(X1, H - Y, X1 + 5, H)
		  end else begin
			  if YMode in [2, 4] then begin
				  Rgn := CreateRectRgn(X1, 0, X1 + 5, Y)
			  end else begin
				   if YMode = 3 then begin
					   tRgn := CreateRectRgn(X1, H - Y, X1 + 5, H);
					   Rgn := CreateRectRgn(X1 + 5, 0, X1 + 10, Y);
					   CombineRgn(Rgn, Rgn, tRgn, RGN_OR);
					   DeleteObject(tRgn);
				   end;
			  end;
		  end;
		end;
		if Result <> 0 then begin
		   CombineRgn(Result, Result, Rgn, RGN_OR);
		   DeleteObject(Rgn);
		end else begin
		   Result := Rgn;
		end;
		Inc(X1, 10)
	end;
end;

function CreateSplashRgn(X, Y, W, H, XMode, YMode: Integer): HRgn;
//----------------------------------------------------------------------------------------------------------------------
var
	X1, Y1, N: Integer;
	Rgn, tRgn: HRgn;
begin
	Result := 0;
	if XMode <> 0 then
	begin
		if X < W then begin
			N := W div 7
		end else begin
			N := 0;
		end;
		Y1 := 0;
		while Y1 < H do begin
			if XMode = 1 then begin
				Rgn := CreateRectRgn(W - X + Random(N) - Random(N), Y1, W, Y1 + 5 + H mod 5);
			end else begin
				if XMode = 2 then begin
					Rgn := CreateRectRgn(0, Y1, X + Random(N) - Random(N), Y1 + 5 + H mod 5)
				end else begin
					if XMode = 3 then begin
						Rgn := CreateRectRgn((W - X + Random(N) - Random(N)) div 2, Y1, W div 2, Y1 + 5 + H mod 5);
						tRgn := CreateRectRgn(W div 2, Y1, (W + X + Random(N) - Random(N)) div 2, Y1 + 5 + H mod 5);
						CombineRgn(Rgn, Rgn, tRgn, RGN_OR);
						DeleteObject(tRgn);
					end else begin
						Rgn := CreateRectRgn(W - (X + Random(N) - Random(N)) div 2, Y1, W, Y1 + 5 + H mod 5);
						tRgn := CreateRectRgn(0, Y1, (X + Random(N) - Random(N)) div 2, Y1 + 5 + H mod 5);
						CombineRgn(Rgn, Rgn, tRgn, RGN_OR);
						DeleteObject(tRgn);
					end;
				end;
			end;
			if Result <> 0 then begin
				CombineRgn(Result, Result, Rgn, RGN_OR);
				DeleteObject(Rgn);
			end else begin
				Result := Rgn;
			end;
			Inc(Y1, 5);
		end;
	end;
	if YMode <> 0 then
	begin
		if Y < H then begin
			N := H div 7
		end else begin
			N := 0;
		end;
		X1 := 0;
		while X1 < W do begin
			if YMode = 1 then
				Rgn := CreateRectRgn(X1, H - Y + Random(N) - Random(N), X1 + 5 + W mod 5, H)
			else begin
				if YMode = 2 then
					Rgn := CreateRectRgn(X1, 0, X1 + 5 + W mod 5, Y + Random(N) - Random(N))
				else begin
					if YMode = 3 then begin
						Rgn := CreateRectRgn(X1, (H - Y + Random(N) - Random(N)) div 2, X1 + 5 + W mod 5, H div 2);
						tRgn := CreateRectRgn(X1, H div 2, X1 + 5 + W mod 5, (H + Y + Random(N) - Random(N)) div 2);
						CombineRgn(Rgn, Rgn, tRgn, RGN_OR);
						DeleteObject(tRgn);
					end else begin
						Rgn := CreateRectRgn(X1, H - (Y + Random(N) - Random(N)) div 2, X1 + 5 + W mod 5, H);
						tRgn := CreateRectRgn(X1, 0, X1 + 5 + W mod 5, (Y + Random(N) - Random(N)) div 2);
						CombineRgn(Rgn, Rgn, tRgn, RGN_OR);
						DeleteObject(tRgn);
					end;
				end;
			end;
			if Result <> 0 then begin
				CombineRgn(Result, Result, Rgn, RGN_OR);
				DeleteObject(Rgn);
			end else begin
				Result := Rgn;
			end;
			Inc(X1, 5);
		end;
	end;
end;

function CreateTriangleRgn(x1, y1, x2, y2, x3, y3: Integer): HRgn;
//----------------------------------------------------------------------------------------------------------------------
var
	ptArray : array[1..4] of TPoint;
begin
	ptArray[1].x := x1;
	ptArray[1].y := y1;
	ptArray[2].x := x2;
	ptArray[2].y := y2;
	ptArray[3].x := x3;
	ptArray[3].y := y3;
	ptArray[4].x := x1;
	ptArray[4].y := y1;
	Result := CreatePolygonRgn(ptArray, 4, WINDING);
end;

function ScaleImageToRect(IR, R: TRect): TRect;
//----------------------------------------------------------------------------------------------------------------------
var
	iW, iH: Integer;
	rW, rH: Integer;
begin
	iW := IR.Right - IR.Left;
	iH := IR.Bottom - IR.Top;
	rW := R.Right - R.Left;
	rH := R.Bottom - R.Top;
	if (rW / iW) < (rH / iH) then begin
		iH := MulDiv(iH, rW, iW);
		iW := MulDiv(iW, rW, iW);
	end else begin
		iW := MulDiv(iW, rH, iH);
		iH := MulDiv(iH, rH, iH);
	end;
	SetRect(Result, 0, 0, iW, iH);
	OffsetRect(Result, R.Left + (rW - iW) div 2, R.Top + (rH - iH) div 2);
end;

procedure DrawTiledImage(Canvas: TCanvas; Rect: TRect; G: TGraphic);
//----------------------------------------------------------------------------------------------------------------------
var
	R, Rows, C, Cols: Integer;
begin
	if (G <> nil) and (not G.Empty) then begin
		Rows := ((Rect.Bottom - Rect.Top) div G.Height) + 1;
		Cols := ((Rect.Right - Rect.Left) div G.Width) + 1;
		for R := 1 to Rows do begin
			for C := 1 to Cols do begin
				Canvas.Draw(Rect.Left + (C-1) * G.Width, Rect.Top + (R-1) * G.Height, G)
			end;
		end;
	end;
end;

procedure MirrorCopyRect(Canvas: TCanvas; dstRect: TRect; Bitmap: TBitmap; srcRect: TRect; Horz, Vert: Boolean);
//----------------------------------------------------------------------------------------------------------------------
type
	PDWordArray = ^TDWordArray;
	TDWordArray = array[0..16383] of DWord;
var
	MirrorBitmap: TBitmap;
	P1, P2: PDWordArray;
	X, Y: Integer;
	P: DWord;
	R: TRect;
begin
	if SrcRect.Left < 0 then begin
		SrcRect.Left := 0;
	end;
	if SrcRect.Right > Bitmap.Width then begin
		SrcRect.Right := Bitmap.Width;
	end;
	if SrcRect.Top < 0 then begin
		SrcRect.Top := 0;
	end;
	if SrcRect.Bottom > Bitmap.Height then begin
		SrcRect.Bottom := Bitmap.Height;
	end;
	SetRect(R, 0, 0, srcRect.Right - srcRect.Left, srcRect.Bottom - srcRect.Top);
	if IsRectEmpty(R) then begin
		Exit;
	end;
	MirrorBitmap := TBitmap.Create;
	try
		if Horz then begin
			MirrorBitmap.Width := 2 * R.Right;
		end else begin
			MirrorBitmap.Width := R.Right;
		end;
		MirrorBitmap.Height := R.Bottom;
		if Horz then begin
			MirrorBitmap.Canvas.CopyRect(R, Bitmap.Canvas, srcRect);
			OffsetRect(R, R.Right, 0);
			MirrorBitmap.Canvas.CopyRect(R, Bitmap.Canvas, srcRect);
			OffsetRect(R, -R.Left, 0);
		end else begin
			MirrorBitmap.Canvas.CopyRect(R, Bitmap.Canvas, srcRect);
		end;
		MirrorBitmap.HandleType := bmDIB;
		MirrorBitmap.PixelFormat := pf32bit;
		if Horz then begin
			for Y := 0 to MirrorBitmap.Height-1 do begin
				P1 := MirrorBitmap.ScanLine[Y];
				for X := 0 to MirrorBitmap.Width-1 do begin
					P := P1[MirrorBitmap.Width-X-1];
					P1[MirrorBitmap.Width-X-1] := P;
					P1[X] := P;
				end;
			end;
		end;
		if Vert then begin
			for Y := 0 to MirrorBitmap.Height div 2 do begin
				P1 := MirrorBitmap.ScanLine[Y];
				P2 := MirrorBitmap.ScanLine[MirrorBitmap.Height-Y-1];
				for X := 0 to MirrorBitmap.Width-1 do begin
					P := P2[X];
					P2[X] := P1[X];
					P1[X] := P;
				end;
			end;
		end;
		Canvas.CopyRect(dstRect, MirrorBitmap.Canvas, R);
	finally
		MirrorBitmap.Free;
	end;
end;

{ TPicShowComponentEditor }

procedure TPicShowComponentEditor.ExecuteVerb(Index: Integer);
//----------------------------------------------------------------------------------------------------------------------
begin
	case Index of
		0 : begin
			TPicShow(Component).Execute;
		end;
		1 : begin
			TPicShow(Component).Clear;
		end;
	else
		inherited ExecuteVerb(Index);
	end;
end;

function TPicShowComponentEditor.GetVerb(Index: Integer): string;
//----------------------------------------------------------------------------------------------------------------------
begin
	case Index of
	  0 : begin
		Result := 'Show picture';
	  end;
	  1 : begin
		Result := 'Hide Picture';
	  end;
	else
	  Result := EmptyStr;
	end;
end;

function TPicShowComponentEditor.GetVerbCount: Integer;
//----------------------------------------------------------------------------------------------------------------------
begin
	Result := 2;
end;

{ TAboutPropertyEditor }

function TAboutPropertyEditor.GetAttributes: TPropertyAttributes;
//----------------------------------------------------------------------------------------------------------------------
begin
	Result := [paDialog, paReadOnly, paMultiSelect];
end;

function TAboutPropertyEditor.GetValue: string;
//----------------------------------------------------------------------------------------------------------------------
begin
	Result := '(About)'
end;

procedure TAboutPropertyEditor.Edit;
//----------------------------------------------------------------------------------------------------------------------
const
	AboutStr = '%s'#10#13 +
			   'by Kambiz R. Khojasteh'#10#13#10#13 +
			   'This componenet is freeware.'#10#13#10#13 +
			   'email: khojasteh@www.dci.co.ir'#10#13 +
			   'web: http://www.geocities.com/k_khojasteh/';
begin
	MessageDlg(Format(AboutStr, [GetComponent(0).ClassName + ' v2.0']), mtInformation, [mbOK], 0);
end;

{ TAnimateThread }

constructor TAnimateThread.Create(APicShow: TPicShow);
//----------------------------------------------------------------------------------------------------------------------
begin
	PicShow := APicShow;
	Priority := PicShow.ThreadPriority;
	OnTerminate := PicShow.AnimationComplete;
	FreeOnTerminate := True;
	inherited Create(False);
end;

procedure TAnimateThread.Execute;
//----------------------------------------------------------------------------------------------------------------------
var
	Elapsed: DWord;
begin
	while not (Terminated or PicShow.Manual) do begin
		Elapsed := GetTickCount;
		Synchronize(Update);
		Elapsed := GetTickCount - Elapsed;
		if PicShow.Progress = High(TPrecent) then begin
			Terminate
		end else begin
			if PicShow.Delay > Elapsed then begin
				Sleep(PicShow.Delay - Elapsed);
			end;
		end;
	end;
end;

procedure TAnimateThread.Update;
//----------------------------------------------------------------------------------------------------------------------
begin
	PicShow.Progress := PicShow.Progress + PicShow.Step;
end;

{ TPicShow }

constructor TPicShow.Create(AOwner: TComponent);
//----------------------------------------------------------------------------------------------------------------------
begin
	inherited Create(AOwner);
	Media := TBitmap.Create;
	fStep := 4;
	fDelay := 20;
	fStyle := 51;
	fCenter := False;
	fStretch := False;
	fStretchFine := False;
	fAutoSize := True;
	fThreaded := True;
	fThreadPriority := tpNormal;
	fManual := False;
	fProgress := 0;
	fBusy := False;
	fPicture := TPicture.Create;
	fPicture.OnChange := PictureChange;
	fBgPicture := TPicture.Create;
	fBgPicture.OnChange := BgPictureChange;
	fBgMode := bmTiled;
	OffScreen := TBitmap.Create;
	Width := 100;
	Height := 100;
	Thread := nil;
	Stopping := False;
	Drawing := False;
end;

destructor TPicShow.Destroy;
//----------------------------------------------------------------------------------------------------------------------
begin
	Drawing := True;
	Stop;
	Media.Free;
	fPicture.Free;
	OffScreen.Free;
	inherited Destroy;
end;

procedure TPicShow.SetPicture(Value: TPicture);
//----------------------------------------------------------------------------------------------------------------------
begin
	if Assigned(Value) then begin
		fPicture.Assign(Value)
	end else begin
		fPicture.Graphic := nil;
	end;
end;

procedure TPicShow.SetBgPicture(Value: TPicture);
//----------------------------------------------------------------------------------------------------------------------
begin
	if Assigned(Value) then begin
		fBgPicture.Assign(Value)
	end else begin
		fBgPicture.Graphic := nil;
	end;
end;

procedure TPicShow.SetBgMode(Value: TBackgroundMode);
//----------------------------------------------------------------------------------------------------------------------
begin
	if fBgMode <> Value then begin
		fBgMode := Value;
		if Assigned(fBgPicture.Graphic) and not Drawing then begin
			Invalidate;
		end;
	end;
end;

procedure TPicShow.SetCenter(Value: Boolean);
//----------------------------------------------------------------------------------------------------------------------
begin
	if fCenter <> Value then begin
		fCenter := Value;
		if Assigned(fPicture.Graphic) then begin
			CalculatePicRect;
			if not (Media.Empty or Drawing) then begin
				Invalidate;
			end;
		end;
	end;
end;

procedure TPicShow.SetStretch(Value: Boolean);
//----------------------------------------------------------------------------------------------------------------------
begin
	if fStretch <> Value then begin
		fStretch := Value;
		if not (Media.Empty or Drawing) then begin
			Invalidate;
		end;
	end;
end;

procedure TPicShow.SetStretchFine(Value: Boolean);
//----------------------------------------------------------------------------------------------------------------------
begin
	if fStretchFine <> Value then begin
		fStretchFine := Value;
		if not (Media.Empty or Drawing) then begin
			Invalidate;
		end;
	end;
end;

procedure TPicShow.SetStep(Value: Word);
//----------------------------------------------------------------------------------------------------------------------
begin
	if Value = 0 then begin
		Value := 1;
	end;
	if Value > High(TPrecent) then begin
		Value := High(TPrecent);
	end;
	fStep := Value;
end;

function TPicShow.GetEmpty: Boolean;
//----------------------------------------------------------------------------------------------------------------------
begin
	Result := not Assigned(fPicture.Graphic) or fPicture.Graphic.Empty;
end;

procedure TPicShow.PictureChange(Sender: TObject);
//----------------------------------------------------------------------------------------------------------------------
begin
	if Assigned(fPicture.Graphic) and fAutoSize then begin
		AdjustClientSize;
	end;
	if Assigned(fOnChange) and not (csDestroying in ComponentState) then begin
		fOnProgress(Self);
	end;
end;

procedure TPicShow.BgPictureChange(Sender: TObject);
//----------------------------------------------------------------------------------------------------------------------
begin
	if (fBgMode <> bmNone) and not Drawing then begin
		Invalidate;
	end;
end;

procedure TPicShow.SetProgress(Value: TPrecent);
//----------------------------------------------------------------------------------------------------------------------
begin
	if Value < Low(TPrecent) then begin
		Value := Low(TPrecent);
	end;
	if Value > High(TPrecent) then begin
		Value := High(TPrecent);
	end;
	if fBusy and (fProgress <> Value) then begin
		if (fProgress > Value) and not Drawing then begin
			InvalidateArea(Rect(0, 0, Media.Width, Media.Height));
		end;
		fProgress := Value;
		UpdateDisplay;
		if Assigned(fOnProgress) and not (csDestroying in ComponentState) then begin
			fOnProgress(Self);
		end;
	end;
end;

procedure TPicShow.SetManual(Value: Boolean);
//----------------------------------------------------------------------------------------------------------------------
begin
	if fManual <> Value then begin
		fManual := Value;
		if not fBusy then begin
			fProgress := 0
		end else begin
			if not fManual then begin
				Animate;
			end;
		end;
	end;
end;

procedure TPicShow.AnimationComplete(Sender: TObject);
//----------------------------------------------------------------------------------------------------------------------
begin
	Thread := nil;
	if Stopping or not fManual then begin
		fBusy := False;
		if Assigned(Pic) then begin
			Pic.Free;
		end;
		if Assigned(OldPic) then begin
			OldPic.Free;
		end;
		Pic := nil;
		OldPic := nil;
		if Assigned(FOnComplete) and not (csDestroying in ComponentState) and (Progress = High(TPrecent)) then begin
			FOnComplete(Self);
		end;
	end;
end;

procedure TPicShow.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
//----------------------------------------------------------------------------------------------------------------------
begin
	Msg.Result := 1;
end;

procedure TPicShow.WMPaint(var Msg: TWMPaint);
//----------------------------------------------------------------------------------------------------------------------
begin
	if not Drawing and (GetCurrentThreadID = MainThreadID) then begin
		Drawing := True;
		try
			inherited;
		finally
			Drawing := False;
		end;
	end;
end;

procedure TPicShow.SetAutoSize(Value: Boolean);
//----------------------------------------------------------------------------------------------------------------------
begin
	if fAutoSize <> Value then begin
		fAutoSize := Value;
		if fAutoSize then begin
			AdjustClientSize;
		end;
	end;
end;

procedure TPicShow.AdjustClientSize;
//----------------------------------------------------------------------------------------------------------------------
begin
	if Assigned(fPicture.Graphic) and (Align = alNone) then begin
		ClientWidth := fPicture.Width;
		ClientHeight := fPicture.Height;
	end;
end;

procedure TPicShow.WMSize(var Msg: TWMSize);
//----------------------------------------------------------------------------------------------------------------------
begin
	inherited;
	if Assigned(fPicture.Graphic) then begin
		CalculatePicRect;
		if not (Media.Empty or Drawing) then begin
			Invalidate;
		end;
	end;
end;

procedure TPicShow.Paint;
//----------------------------------------------------------------------------------------------------------------------
var
	R: TRect;
	C: TCanvas;
begin
	OffScreen.Width := ClientWidth;
	OffScreen.Height := ClientHeight;
	C := OffScreen.Canvas;
	C.Lock;
	try
		R := ClientRect;
		C.Brush.Color := Color;
		C.FillRect(R);
		if not fStretch and Assigned(fBgPicture.Graphic) then begin
			case fBgMode of
				bmTiled : begin
					DrawTiledImage(C, R, fBgPicture.Graphic);
				end;
				bmStretched : begin
					C.StretchDraw(R, fBgPicture.Graphic);
				end;
				bmCentered : begin
					C.Draw( (R.Right - R.Left - fBgPicture.Width) div 2, (R.Bottom - R.Top - fBgPicture.Height) div 2,
						    fBgPicture.Graphic);
				end;
			end;
		end;
		if not Media.Empty then begin
			if fStretch then begin
				if fStretchFine then begin
					C.StretchDraw(ScaleImageToRect(PicRect, R), Media)
				end else begin
					C.StretchDraw(R, Media)
				end;
			end else begin
			  C.Draw(PicRect.Left, PicRect.Top, Media);
			end;
		end;
	finally
		C.Unlock;
	end;
	Canvas.Lock;
	try
		Canvas.Draw(0, 0, OffScreen);
	finally
		Canvas.Unlock;
	end;
end;

procedure TPicShow.CalculatePicRect;
//----------------------------------------------------------------------------------------------------------------------
begin
	if not Media.Empty then begin
		SetRect(PicRect, 0, 0, Media.Width, Media.Height);
		if fCenter then begin
			OffsetRect(PicRect, (ClientWidth - Media.Width) div 2, (ClientHeight - Media.Height) div 2);
		end;
	end;
end;

procedure TPicShow.InvalidateArea(Area: TRect);
//----------------------------------------------------------------------------------------------------------------------
var
	R: TRect;
begin
	if fStretch then begin
		if fStretchFine then begin
			R := ScaleImageToRect(PicRect, ClientRect)
		end else begin
			R := ClientRect;
		end;
		Area.Left := R.Left + MulDiv(Area.Left, R.Right - R.Left, PicRect.Right - PicRect.Left);
		Area.Right := R.Left + MulDiv(Area.Right, R.Right - R.Left, PicRect.Right - PicRect.Left);
		Area.Top := R.Top + MulDiv(Area.Top, R.Bottom - R.Top, PicRect.Bottom - PicRect.Top);
		Area.Bottom := R.Top + MulDiv(Area.Bottom, R.Bottom - R.Top, PicRect.Bottom - PicRect.Top);
	end else begin
		if fCenter then begin
			OffsetRect(Area, PicRect.Left, PicRect.Top);
		end;
		if Area.Left < PicRect.Left then begin
			Area.Left := PicRect.Left;
		end;
		if Area.Right > PicRect.Right then begin
			Area.Right := PicRect.Right;
		end;
		if Area.Top < PicRect.Top then begin
			Area.Top := PicRect.Top;
		end;
		if Area.Bottom > PicRect.Bottom then begin
			Area.Bottom := PicRect.Bottom;
		end;
	end;
	if not (csDestroying in ComponentState) then begin
		InvalidateRect(Handle, @Area, False);
	end;
end;

Procedure TPicShow.Clear;
//----------------------------------------------------------------------------------------------------------------------
begin
	if not (fBusy or Media.Empty) then begin
		if Media.Canvas.TryLock then begin
			Media.Canvas.Unlock;
			Media.Free;
			Media := TBitmap.Create;
			Invalidate;
		end;
	end;
end;

procedure TPicShow.Stop;
//----------------------------------------------------------------------------------------------------------------------
begin
	if fBusy and not Stopping then begin
		Stopping := True;
		if Assigned(Thread) then begin
			Thread.Terminate;
			while fBusy do begin
				Thread.WaitFor;
				Application.ProcessMessages;
			end;
		end else begin
			AnimationComplete(Self);
		end;
		Stopping := False;
	end;
end;

procedure TPicShow.Execute;
//----------------------------------------------------------------------------------------------------------------------
begin
	if not fBusy and Assigned(Picture.Graphic) then begin
		fBusy := True;
		try
			Prepare;
			if not fManual then begin
				Animate;
			end;
		except
			if Assigned(Pic) then begin
				Pic.Free;
			end;
			if Assigned(OldPic) then begin
				OldPic.Free;
			end;
			fBusy := False;
			raise;
		end;
	end;
end;

procedure TPicShow.Animate;
//----------------------------------------------------------------------------------------------------------------------
var
	StartTime: DWord;
begin
	if fThreaded and (fStyle <> 0) then begin
		Thread := TAnimateThread.Create(Self)
	end else begin
		repeat
			StartTime := GetTickCount;
			Progress := Progress + Step;
			if Progress <> High(TPrecent) then begin
				repeat
					Application.ProcessMessages;
				until ((GetTickCount - StartTime) > Delay) or not fBusy;
			end;
		until (Progress = High(TPrecent)) or not fBusy or fManual;
		if fBusy and not fManual then begin
			AnimationComplete(Self);
		end;
	end;
end;

procedure TPicShow.Prepare;
//----------------------------------------------------------------------------------------------------------------------
var
	R: TRect;
begin
	Media.Canvas.Brush.Color := Color;
	Media.Width := fPicture.Width;
	Media.Height := fPicture.Height;
	CalculatePicRect;
	OldPic := TBitmap.Create;
	OldPic.Width := Media.Width;
	OldPic.Height := Media.Height;
	if fStretch then begin
		if fStretchFine then begin
			R := ScaleImageToRect(PicRect, ClientRect)
		end else begin
			R := ClientRect
		end;
	end else begin
		R := PicRect;
	end;
	OldPic.Canvas.CopyRect(Rect(0, 0, OldPic.Width, OldPic.Height), OffScreen.Canvas, R);
	Pic := TBitmap.Create;
	Pic.Width := Media.Width;
	Pic.Height := Media.Height;
	Pic.Canvas.Draw(0, 0, fPicture.Graphic);
	Progress := 0;
end;

procedure TPicShow.UpdateDisplay;
var
	X, Y, W, H: Integer;
	R, Rgn, PicRgn: HRgn;
	R1, R2: TRect;
	I, J, S: Integer;
begin
	Media.Canvas.Draw(0, 0, OldPic);
	if (fStyle in [1..57]) or not fStretch then begin
		W := Pic.Width;
		H := Pic.Height;
		SetRect(R1, 0, 0, W, H);
		SetRect(R2, 0, 0, W, H);
	end else begin
		R1 := ClientRect;
		if fStretchFine then begin
			R1 := ScaleImageToRect(PicRect, R1);
		end;
		W := R1.Right - R1.Left;
		H := R1.Bottom - R1.Top;
	end;
	Rgn := 0;
	if W >= H then begin
		X := MulDiv(W, fProgress, 100);
		Y := MulDiv(X, H, W);
		S := 2 * MulDiv(W, fStep, 100);
	end else begin
		Y := MulDiv(H, fProgress, 100);
		X := MulDiv(Y, W, H);
		S := 2 * MulDiv(H, fStep, 100);
	end;
	case fStyle of
		0 : begin
			if Assigned(fOnCustomDraw) then begin
				fOnCustomDraw(Self, Pic, Media, Rgn)
			end else begin
				Media.Canvas.Draw(0, 0, Pic);
				Rgn := CreateRectRgn(0, 0, W, H);
			end;
		end;
		1 : begin
			R1.Left := W - X;
		end;
		2: begin
			R1.Right := X;
		end;
		3: begin
			R1.Left := W - X;
			R1.Right := (2 * W) - X;
		end;
		4: begin
			R1.Left := X - W;
			R1.Right := X;
		end;
		5: begin
			R1.Right := X;
			R2.Right := X;
		end;
		6: begin
			R1.Left := W - X;
			R2.Left := W - X;
		end;
		7: begin
			R1.Right := (2 * W) - X;
			R2.Right := X;
		end;
		8: begin
			R1.Left := X - W;
			R2.Left := W - X;
		end;
		9: begin
			R1.Left := X - W;
			R1.Right := (2 * W) - X;
			R2.Left := (W - X) div 2;
			R2.Right := (W + X) div 2;
		end;
		10: begin
			R1.Left := (W - X) div 2;
			R1.Right := (W + X) div 2;
		end;
		11: begin
			R1.Left := (W - X) div 2;
			R1.Right := (W + X) div 2;
			R2.Left := (W - X) div 2;
			R2.Right := (W + X) div 2;
		end;
		12: begin
			R1.Left := 0;
			R1.Right := (X div 2) + 1;
			R2.Left := 0;
			R2.Right := (X div 2) + 1;
			Media.Canvas.CopyRect(R1, Pic.Canvas, R2);
			InvalidateArea(R1);
			R1.Left := W - (X div 2) - 1;
			R1.Right := W;
			R2.Left := W - (X div 2) - 1;
			R2.Right := W;
		end;
		13: begin
			R1.Left := 0;
			R1.Right := (X div 2) + 1;
			R2.Left := 0;
			R2.Right := (W div 2) + 1;
			Media.Canvas.CopyRect(R1, Pic.Canvas, R2);
			InvalidateArea(R1);
			R1.Left := W - (X div 2) - 1;
			R1.Right := W;
			R2.Left := W div 2;
			R2.Right := W;
		end;
		14: begin
			R1.Left := X;
			if (R1.Left + W div 10) > W then begin
				R1.Right := R1.Left + (W - X) div 2;
			end else begin
				R1.Right := R1.Left + W div 10;
			end;
			R2.Left := R1.Right;
			R2.Right := R2.Left + R1.Right - R1.Left;
			MirrorCopyRect(Media.Canvas, R1, Pic, R2, True, False);
			InvalidateArea(R1);
			R1.Left := 0;
			R1.Right := X;
			R2.Left := 0;
			R2.Right := X;
		end;
		15: begin
			R1.Right := W - X;
			if (R1.Right - W div 10) < 0 then begin
				R1.Left := R1.Right - (W - X) div 2
			end else begin
				R1.Left := R1.Right - W div 10;
			end;
			R2.Right := R1.Left;
			R2.Left := R2.Right - R1.Right + R1.Left;
			MirrorCopyRect(Media.Canvas, R1, Pic, R2, True, False);
			InvalidateArea(R1);
			R1.Left := W - X;
			R1.Right := W;
			R2.Left := W - X;
			R2.Right := W;
		end;
		16: begin
			R1.Left := 0;
			R1.Right := X;
			R2.Left := 0;
			R2.Right := X;
			Media.Canvas.CopyRect(R1, Pic.Canvas, R2);
			InvalidateArea(R1);
			R1.Left := X;
			R1.Right := W;
			R2.Left := X;
			R2.Right := X + W div 20;
		end;
		17: begin
			R1.Left := W - X;
			R1.Right := W;
			R2.Left := W - X;
			R2.Right := W;
			Media.Canvas.CopyRect(R1, Pic.Canvas, R2);
			InvalidateArea(R1);
			R1.Left := 0;
			R1.Right := W - X;
			R2.Left := (W - X) - W div 20;
			R2.Right := W - X;
		end;
		18: begin
			R1.Top := H - Y;
		end;
		19: begin
			R1.Bottom := Y;
		end;
		20: begin
			R1.Top := H - Y;
			R1.Bottom := (2 * H) - Y;
		end;
		21: begin
			R1.Top := Y - H;
			R1.Bottom := Y;
		end;
		22: begin
			R1.Bottom := Y;
			R2.Bottom := Y;
		end;
		23: begin
			R1.Top := H - Y;
			R2.Top := H - Y;
		end;
		24: begin
			R1.Bottom := (2 * H) - Y;
			R2.Bottom := Y;
		end;
		25: begin
			R1.Top := Y - H;
			R2.Top := H - Y;
		end;
		26: begin
			R1.Top := Y - H;
			R1.Bottom := (2 * H) - Y;
			R2.Top := (H - Y) div 2;
			R2.Bottom := (H + Y) div 2;
		end;
		27: begin
			R1.Top := (H - Y) div 2;
			R1.Bottom := (H + Y) div 2;
		end;
		28: begin
			R1.Top := (H - Y) div 2;
			R1.Bottom := (H + Y) div 2;
			R2.Top := (H - Y) div 2;
			R2.Bottom := (H + Y) div 2;
		end;
		29: begin
			R1.Top := 0;
			R1.Bottom := (Y div 2) + 1;
			R2.Top := 0;
			R2.Bottom := (Y div 2) + 1;
			Media.Canvas.CopyRect(R1, Pic.Canvas, R2);
			InvalidateArea(R1);
			R1.Top := H - (Y div 2) - 1;
			R1.Bottom := H;
			R2.Top := H - (Y div 2) - 1;
			R2.Bottom := H;
		end;
		30: begin
			R1.Top := 0;
			R1.Bottom := (Y div 2) + 1;
			R2.Top := 0;
			R2.Bottom := (H div 2) + 1;
			Media.Canvas.CopyRect(R1, Pic.Canvas, R2);
			InvalidateArea(R1);
			R1.Top := H - (Y div 2) - 1;
			R1.Bottom := H;
			R2.Top := H div 2;
			R2.Bottom := H;
		end;
		31: begin
			R1.Top := Y;
			if (R1.Top + H div 10) > H then begin
				R1.Bottom := R1.Top + (H - Y) div 2
			end else begin
				R1.Bottom := R1.Top + H div 10;
			end;
			R2.Top := R1.Bottom;
			R2.Bottom := R2.Top + R1.Bottom - R1.Top;
			MirrorCopyRect(Media.Canvas, R1, Pic, R2, False, True);
			InvalidateArea(R1);
			R1.Top := 0;
			R1.Bottom := Y;
			R2.Top := 0;
			R2.Bottom := Y;
		end;
		32: begin
			R1.Bottom := H - Y;
			if (R1.Bottom - H div 10) < 0 then begin
				R1.Top := R1.Bottom - (H - Y) div 2
			end else begin
				R1.Top := R1.Bottom - H div 10;
			end;
			R2.Bottom := R1.Top;
			R2.Top := R2.Bottom - R1.Bottom + R1.Top;
			MirrorCopyRect(Media.Canvas, R1, Pic, R2, False, True);
			InvalidateArea(R1);
			R1.Top := H - Y;
			R1.Bottom := H;
			R2.Top := H - Y;
			R2.Bottom := H;
		end;
		33: begin
			R1.Top := 0;
			R1.Bottom := Y;
			R2.Top := 0;
			R2.Bottom := Y;
			Media.Canvas.CopyRect(R1, Pic.Canvas, R2);
			InvalidateArea(R1);
			R1.Top := Y;
			R1.Bottom := H;
			R2.Top := Y;
			R2.Bottom := Y + H div 20;
		end;
		34: begin
			R1.Top := H - Y;
			R1.Bottom := H;
			R2.Top := H - Y;
			R2.Bottom := H;
			Media.Canvas.CopyRect(R1, Pic.Canvas, R2);
			InvalidateArea(R1);
			R1.Top := 0;
			R1.Bottom := H - Y;
			R2.Top := (H - Y) - H div 20;
			R2.Bottom := H - Y;
		end;
		35: begin
			R1.Left := W - X;
			R1.Top := H - Y;
		end;
		36: begin
			R1.Left := W - X;
			R1.Bottom := Y;
		end;
		37: begin
			R1.Right := X;
			R1.Bottom := Y;
		end;
		38: begin
			R1.Right := X;
			R1.Top := H - Y;
		end;
		39: begin
			R1.Left := W - X;
			R1.Top := H - Y;
			R1.Right := (2 * W) - X;
			R1.Bottom := (2 * H) - Y;
		end;
		40: begin
			R1.Left := W - X;
			R1.Top := Y - H;
			R1.Right := (2 * W) - X;
			R1.Bottom := Y;
		end;
		41: begin
			R1.Left := X - W;
			R1.Top := Y - H;
			R1.Right := X;
			R1.Bottom := Y;
		end;
		42: begin
			R1.Left := X - W;
			R1.Top := H - Y;
			R1.Right := X;
			R1.Bottom := (2 * H) - Y;
		end;
		43: begin
			R1.Right := X;
			R1.Bottom := Y;
			R2.Right := X;
			R2.Bottom := Y;
		end;
		44: begin
			R1.Right := X;
			R1.Top := H - Y;
			R2.Right := X;
			R2.Top := H - Y;
		end;
		45: begin
			R1.Left := W - X;
			R1.Top := H - Y;
			R2.Left := W - X;
			R2.Top := H - Y;
		end;
		46: begin
			R1.Left := W - X;
			R1.Bottom := Y;
			R2.Left := W - X;
			R2.Bottom := Y;
		end;
		47: begin
			R1.Right := (2 * W) - X;
			R1.Bottom := (2 * H) - Y;
			R2.Right := X;
			R2.Bottom := Y;
		end;
		48: begin
			R1.Right := (2 * W) - X;
			R1.Top := Y - H;
			R2.Right := X;
			R2.Top := H - Y;
		end;
		49: begin
			R1.Left := X - W;
			R1.Top := Y - H;
			R2.Left := W - X;
			R2.Top := H - Y;
		end;
		50: begin
			R1.Left := X - W;
			R1.Bottom := (2 * H) - Y;
			R2.Left := W - X;
			R2.Bottom := Y;
		end;
		51: begin
			R1.Left := X - W;
			R1.Top := Y - H;
			R1.Right := (2 * W) - X;
			R1.Bottom := (2 * H) - Y;
			R2.Left := (W - X) div 2;
			R2.Top := (H - Y) div 2;
			R2.Right := (W + X) div 2;
			R2.Bottom := (H + Y) div 2;
		end;
		52: begin
			R1.Left := (W - X) div 2;
			R1.Top := (H - Y) div 2;
			R1.Right := (W + X) div 2;
			R1.Bottom := (H + Y) div 2;
		end;
	   	53: begin
			R1.Left := (W - X) div 2;
			R1.Top := (H - Y) div 2;
			R1.Right := (W + X) div 2;
			R1.Bottom := (H + Y) div 2;
			R2.Left := (W - X) div 2;
			R2.Top := (H - Y) div 2;
			R2.Right := (W + X) div 2;
			R2.Bottom := (H + Y) div 2;
		end;
		54: begin
			R1.Left := 0;
			R1.Right := W;
			R1.Top := 0;
			R1.Bottom := Y div 2;
			R2.Left := 0;
			R2.Right := W;
			R2.Top := 0;
			R2.Bottom := Y div 2;
			Media.Canvas.CopyRect(R1, Pic.Canvas, R2);
			InvalidateArea(R1);
			R1.Left := 0;
			R1.Right := W;
			R1.Top := H - (Y div 2);
			R1.Bottom := H;
			R2.Left := 0;
			R2.Right := W;
			R2.Top := H - (Y div 2);
			R2.Bottom := H;
			Media.Canvas.CopyRect(R1, Pic.Canvas, R2);
			InvalidateArea(R1);
			R1.Left := 0;
			R1.Right := X div 2;
			R1.Top := 0;
			R1.Bottom := H;
			R2.Left := 0;
			R2.Right := X div 2;
			R2.Top := 0;
			R2.Bottom := H;
			Media.Canvas.CopyRect(R1, Pic.Canvas, R2);
			InvalidateArea(R1);
			R1.Left := W - (X div 2);
			R1.Right := W;
			R1.Top := 0;
			R1.Bottom := H;
			R2.Left := W - (X div 2);
			R2.Right := W;
			R2.Top := 0;
			R2.Bottom := H;
		end;
		55: begin
			R1.Left := 0;
			R1.Top := 0;
			R1.Right := (X div 2) + 1;
			R1.Bottom := (Y div 2) + 1;
			R2.Left := 0;
			R2.Top := 0;
			R2.Right := (X div 2) + 1;
			R2.Bottom := (Y div 2) + 1;
			Media.Canvas.CopyRect(R1, Pic.Canvas, R2);
			InvalidateArea(R1);
			R1.Left := 0;
			R1.Top := H - (Y div 2) - 1;
			R1.Right := (X div 2) + 1;
			R1.Bottom := H;
			R2.Left := 0;
			R2.Top := H - (Y div 2) - 1;
			R2.Right := (X div 2) + 1;
			R2.Bottom := H;
			Media.Canvas.CopyRect(R1, Pic.Canvas, R2);
			InvalidateArea(R1);
			R1.Left := W - (X div 2) - 1;
			R1.Top := H - (Y div 2) - 1;
			R1.Right := W;
			R1.Bottom := H;
			R2.Left := W - (X div 2) - 1;
			R2.Top := H - (Y div 2) - 1;
			R2.Right := W;
			R2.Bottom := H;
			Media.Canvas.CopyRect(R1, Pic.Canvas, R2);
			InvalidateArea(R1);
			R1.Left := W - (X div 2) - 1;
			R1.Top := 0;
			R1.Right := W;
			R1.Bottom := (Y div 2) + 1;
			R2.Left := W - (X div 2) - 1;
			R2.Top := 0;
			R2.Right := W;
			R2.Bottom := (Y div 2) + 1;
	   end;
	   56: begin
			R1.Left := 0;
			R1.Top := 0;
			R1.Right := (X div 2) + 1;
			R1.Bottom := (Y div 2) + 1;
			R2.Left := 0;
			R2.Top := 0;
			R2.Right := (W div 2) + 1;
			R2.Bottom := (H div 2) + 1;
			Media.Canvas.CopyRect(R1, Pic.Canvas, R2);
			InvalidateArea(R1);
			R1.Left := 0;
			R1.Top := H - (Y div 2);
			R1.Right := (X div 2) + 1;
			R1.Bottom := H;
			R2.Left := 0;
			R2.Top := (H div 2) + 1;
			R2.Right := (W div 2) + 1;
			R2.Bottom := H;
			Media.Canvas.CopyRect(R1, Pic.Canvas, R2);
			InvalidateArea(R1);
			R1.Left := W - (X div 2);
			R1.Top := H - (Y div 2);
			R1.Right := W;
			R1.Bottom := H;
			R2.Left := (W div 2) + 1;
			R2.Top := (H div 2) + 1;
			R2.Right := W;
			R2.Bottom := H;
			Media.Canvas.CopyRect(R1, Pic.Canvas, R2);
			InvalidateArea(R1);
			R1.Left := W - (X div 2);
			R1.Top := 0;
			R1.Right := W;
			R1.Bottom := (Y div 2) + 1;
			R2.Left := (W div 2) + 1;
			R2.Top := 0;
			R2.Right := W;
			R2.Bottom := (H div 2) + 1;
	   end;
	   57: begin
			R1.Left := (X - W) div 2;
			R1.Right := (X div 2) + 1;
			R1.Top := 0;
			R1.Bottom := (H div 2) + 1;
			R2.Left := 0;
			R2.Right := (W div 2) + 1;
			R2.Top := 0;
			R2.Bottom := (H div 2) + 1;
			Media.Canvas.CopyRect(R1, Pic.Canvas, R2);
			InvalidateArea(R1);
			R1.Left := (W div 2) - 1;
			R1.Right := W;
			R1.Top := (Y - H) div 2;
			R1.Bottom := (Y div 2) + 1;
			R2.Left := (W div 2) - 1;
			R2.Right := W;
			R2.Top := 0;
			R2.Bottom := (H div 2) + 1;
			Media.Canvas.CopyRect(R1, Pic.Canvas, R2);
			InvalidateArea(R1);
			R1.Left := W - X div 2;
			R1.Right := W + (W - X) div 2;
			R1.Top := (H div 2) - 1;
			R1.Bottom := H;
			R2.Left := (W div 2) + 1;
			R2.Right := W;
			R2.Top := (H div 2) - 1;
			R2.Bottom := H;
			Media.Canvas.CopyRect(R1, Pic.Canvas, R2);
			InvalidateArea(R1);
			R1.Left := 0;
			R1.Right := (W div 2) + 1;
			R1.Top := H - Y div 2;
			R1.Bottom := H + (H - Y) div 2;
			R2.Left := 0;
			R2.Right := (W div 2) + 1;
			R2.Top := (H div 2) + 1;
			R2.Bottom := H;
	   end;
	   58: begin
			Rgn := CreateRoundRectRgn(-(2 * W), -5, 2 * X, H + 5, 2 * W, 2 * W);
	   end;
	   59: begin
			Rgn := CreateRoundRectRgn(W - 2 * X, -5, W + (2 * W), H + 5, 2 * W, 2 * W);
	   end;
	   60: begin
			Rgn := CreateBarRgn(2 * X, 0, W, H, S, 1, 0);
	   end;
	   61: begin
			Rgn := CreateBarRgn(2 * X, 0, W, H, S, 2, 0);
	   end;
	   62: begin
			Rgn := CreateBarRgn(2 * X, 0, W, H, S, 4, 0);
	   end;
	   63: begin
			Rgn := CreateBarRgn(2 * X, 0, W, H, S, 5, 0);
	   end;
	   64: begin
			Rgn := CreateBarRgn(X, 0, W, H, 0, 3, 0);
	   end;
	   65: begin
			Rgn := CreateSplashRgn(X, 0, W, H, 1, 0);
	   end;
	   66: begin
			Rgn := CreateSplashRgn(X, 0, W, H, 2, 0);
	   end;
	   67: begin
			Rgn := CreateSplashRgn(X, 0, W, H, 3, 0);
	   end;
	   68: begin
			Rgn := CreateSplashRgn(X, 0, W, H, 4, 0);
	   end;
	   69: begin
			Rgn := CreateRoundRectRgn(-5, -(2 * H), W + 5, 2 * Y, 2 * H, 2 * H);
	   end;
	   70: begin
			Rgn := CreateRoundRectRgn(-5, H - 2 * Y, W + 5, H + (2 * H), 2 * H, 2 * H);
	   end;
	   71: begin
			Rgn := CreateBarRgn(0, 2 * Y, W, H, S, 0, 1);
	   end;
	   72: begin
			Rgn := CreateBarRgn(0, 2 * Y, W, H, S, 0, 2);
	   end;
	   73: begin
			Rgn := CreateBarRgn(0, 2 * Y, W, H, S, 0, 4);
	   end;
	   74: begin
			Rgn := CreateBarRgn(0, 2 * Y, W, H, S, 0, 5);
	   end;
	   75: begin
			Rgn := CreateBarRgn(0, Y, W, H, 0, 0, 3);
	   end;
	   76: begin
			Rgn := CreateSplashRgn(0, Y, W, H, 0, 1);
	   end;
	   77: begin
			Rgn := CreateSplashRgn(0, Y, W, H, 0, 2);
	   end;
	   78: begin
			Rgn := CreateSplashRgn(0, Y, W, H, 0, 3);
	   end;
	   79: begin
			Rgn := CreateSplashRgn(0, Y, W, H, 0, 4);
	   end;
	   80: begin
			Rgn := CreateRoundRectRgn(-(2 * W), -(2 * H), 2 * X, 2 * Y, 2 * W, 2 * H);
	   end;
	   81: begin
			Rgn := CreateRoundRectRgn(W - 2 * X, -(2 * H), W + (2 * W), 2 * Y, 2 * W, 2 * H);
	   end;
	   82: begin
			Rgn := CreateRoundRectRgn(-(2 * W), H - 2 * Y, 2 * X, H + (2 * H), 2 * W, 2 * H);
	   end;
	   83: begin
			Rgn := CreateRoundRectRgn(W - 2 * X, H - 2 * Y, W + (2 * W), H + (2 * H), 2 * H, 2 * H);
	   end;
	   84: begin
			Rgn := CreateRoundRectRgn(W div 2 - X, H div 2 - Y, W div 2 + X, H div 2 + Y, 9 * X div 5, 9 * Y div 5);
	   end;
	   85: begin
			R := CreateRectRgn(0, 0, W, H);
			Rgn := CreateRoundRectRgn( X - W div 2, Y - H div 2, 3 * W div 2 - X, 3 * H div 2 - Y,
									   9 * (W - X) div 5, 9 * (H - Y) div 5);
			CombineRgn(Rgn, Rgn, R, RGN_XOR);
			DeleteObject(R);
	   end;
	   86: begin
			Rgn := CreateBarRgn(2 * X, 2 * Y, W, H, S, 1, 1);
	   end;
	   87: begin
			Rgn := CreateBarRgn(2 * X, 2 * Y, W, H, S, 1, 2);
	   end;
	   88: begin
			Rgn := CreateBarRgn(2 * X, 2 * Y, W, H, S, 2, 1);
	   end;
	   89: begin
			Rgn := CreateBarRgn(2 * X, 2 * Y, W, H, S, 2, 2);
	   end;
	   90: begin
			Rgn := CreateBarRgn(2 * X, 2 * Y, W, H, S, 4, 4);
	   end;
	   91: begin
			Rgn := CreateBarRgn(2 * X, 2 * Y, W, H, S, 4, 5);
	   end;
	   92: begin
			Rgn := CreateBarRgn(2 * X, 2 * Y, W, H, S, 5, 4);
	   end;
	   93: begin
			Rgn := CreateBarRgn(2 * X, 2 * Y, W, H, S, 5, 5);
	   end;
	   94: begin
			Rgn := CreateBarRgn(2 * X, 2 * Y, W, H, S, 1, 3);
	   end;
	   95: begin
			Rgn := CreateBarRgn(2 * X, 2 * Y, W, H, S, 2, 3);
	   end;
	   96: begin
			Rgn := CreateBarRgn(2 * X, 2 * Y, W, H, S, 3, 1);
	   end;
	   97: begin
			Rgn := CreateBarRgn(2 * X, 2 * Y, W, H, S, 3, 2);
	   end;
	   98: begin
			Rgn := CreateBarRgn(X, Y, W, H, 0, 3, 3);
	   end;
	   99: begin
			R := CreateBarRgn(2 * X, 2 * Y, W, H, S, 1, 1);
			Rgn := CreateBarRgn(2 * X, 2 * Y, W, H, S, 2, 2);
			CombineRgn(Rgn, Rgn, R, RGN_AND);
			DeleteObject(R);
	   end;
	   100: begin
			Rgn := CreateSplashRgn(X, Y, W, H, 1, 1);
	   end;
	   101: begin
			Rgn := CreateSplashRgn(X, Y, W, H, 1, 2);
	   end;
	   102: begin
			Rgn := CreateSplashRgn(X, Y, W, H, 2, 1);
	   end;
	   103: begin
			Rgn := CreateSplashRgn(X, Y, W, H, 2, 2);
	   end;
	   104: begin
			Rgn := CreateSplashRgn(X, Y, W, H, 1, 3);
	   end;
	   105: begin
			Rgn := CreateSplashRgn(X, Y, W, H, 2, 3);
	   end;
	   106: begin
			Rgn := CreateSplashRgn(X, Y, W, H, 3, 1);
	   end;
	   107: begin
			Rgn := CreateSplashRgn(X, Y, W, H, 3, 2);
	   end;
	   108: begin
			Rgn := CreateSplashRgn(X, Y, W, H, 3, 3);
	   end;
	   109: begin
			Rgn := CreateSplashRgn(X, Y, W, H, 4, 4);
	   end;
	   // Thanks to M. R. Zamani for these effects
	   110: begin
			Rgn := CreateTriangleRgn(0, 0, 2 * X, 0, 0, 2 * Y);
	   end;
	   111: begin
			Rgn := CreateTriangleRgn(W, 0, W - 2 * X, 0, W, 2 * Y);
	   end;
	   112: begin
			Rgn := CreateTriangleRgn(0, H, 2 * X, H, 0, H - 2 * Y);
	   end;
	   113: begin
			Rgn := CreateTriangleRgn(W, H, W - 2 * X, H, W, H - 2 * Y);
	   end;
	   114: begin
			R := CreateTriangleRgn(0, H, 0, 0, X, H);
			Rgn := CreateTriangleRgn(W, H, W, 0, W - X, 0);
			CombineRgn(Rgn, Rgn, R, RGN_OR);
			DeleteObject(R);
	   end;
	   115: begin
			R := CreateTriangleRgn(W, 0, 0, 0, W, Y);
			Rgn := CreateTriangleRgn(W, H, 0, H, 0, H - Y);
			CombineRgn(Rgn, Rgn, R, RGN_OR);
			DeleteObject(R);
	   end;
	   116: begin
			Rgn := CreateTriangleRgn(W div 2, H div 2, 0, H, 0, H - Y);
			R := CreateTriangleRgn(0, 0, X, 0, W div 2, H div 2);
			CombineRgn(Rgn, Rgn, R, RGN_OR);
			DeleteObject(R);
			R := CreateTriangleRgn(W - X, H, W div 2, H div 2, W, H);
			CombineRgn(Rgn, Rgn, R, RGN_OR);
			DeleteObject(R);
			R := CreateTriangleRgn(W div 2, H div 2, W, 0, W, Y);
			CombineRgn(Rgn, Rgn, R, RGN_OR);
			DeleteObject(R);
	   end;
	   117: begin
			X := X div 5;
			Y := MulDiv(X, H, W);
			for J := 0 to 9 do begin
				for I := 0 to 9 do begin
					R := CreateTriangleRgn(I * W div 10, J * H div 10, I * W div 10 + X, J * H div 10, I * W div 10, J * H div 10 + Y);
					if Rgn <> 0 then begin
						CombineRgn(Rgn, Rgn, R, RGN_OR);
						DeleteObject(R);
					end else begin
						Rgn := R;
					end;
				end;
			end;
		  end;
	else
		Exit;
	end; // end of case
	if fStyle in [1..57] then begin
		Media.Canvas.CopyRect(R1, Pic.Canvas, R2);
		InvalidateArea(R1);
	end else begin
		if (fProgress <> 0) and (fStyle <> 0) then
			Media.Canvas.Draw(0, 0, Pic);
		if not fStretch then begin
			OffsetRgn(Rgn, PicRect.Left, PicRect.Top);
			PicRgn := CreateRectRgnIndirect(PicRect);
			CombineRgn(Rgn, Rgn, PicRgn, RGN_AND);
			DeleteObject(PicRgn);
		end else begin
			OffsetRgn(Rgn, R1.Left, R1.Top);
			PicRgn := CreateRectRgnIndirect(R1);
			CombineRgn(Rgn, Rgn, PicRgn, RGN_AND);
			DeleteObject(PicRgn);
		end;
		if not Drawing then InvalidateRgn(Handle, Rgn, False);
		DeleteObject(Rgn);
	end;
	if not Drawing then Update;
end;


procedure Register;
//----------------------------------------------------------------------------------------------------------------------
begin
	RegisterComponents('FX Controls', [TPicShow]);
	RegisterComponentEditor(TPicShow, TPicShowComponentEditor);
	RegisterPropertyEditor(TypeInfo(TAbout), TPicShow, 'About', TAboutPropertyEditor);
end;

end.
