//NOTA : as diretivas de compilacao devem ser as mesmas para os dois programas
{$IFDEF QrAngLbl}
{$A+,B-,C-,D+,E-,F+,G+,H+,I+,J+,K-,L+,M-,N+,O-,P+,Q-,R+,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$DEBUGINFO ON}
{$ELSE}
{$A+,B-,C-,D-,E-,F+,G+,H+,I+,J+,K-,L-,M-,N+,O+,P+,Q+,R+,S-,T-,U-,V+,W-,X+,Y-,Z4}
{$ENDIF}


///
//History: 20111101 - Roger
//1 - Formatação de codigo parcialmente realizada
//2 - Removidas queixas menores do compilador
///



(********************************************************************************)
(* QuickReport Angled Labels                                                    *)
(*                                                                              *)
(* Those components are text labels with rotate capabilities, to use with       *)
(* QuickReport versions 1.x and 2.x under Delphi 1, Delphi 2 (not tested) or    *)
(* Delphi 3                                                                     *)
(*                                                                              *)
(* See README.TXT for more details                                              *)
(*                                                                              *)
(* NOTE: This component was tested with:                                        *)
(*           QuickReport 1.1a under Delphi 1,                                   *)
(*           QuickReport 1.1b under Delphi 3, and                               *)
(*           QuickReport 2.0i under Delphi 1 and Delphi 3                       *)
(*                                                                              *)
(* Copyright © 1997 by Francisco Maia Goncalves Neto                            *)
(* e-mail: fneto_br@hotmail.com                                                 *)
(*                                                                              *)
(* Thanks to Keith Wood for his LabelEffect component                           *)
(* Thanks to Olivier Exbrayat and Michael Snell for your help                   *)
(*==============================================================================*)
(* This component is freeware and can be freely used and distributed in         *)
(* commercial and private environments.                                         *)
(* The source code may be freely distributed and used.                          *)
(* The author assumes no liability for damages, direct or consequential, which  *)
(* may result from the use of this component.                                   *)
(* Any comments or suggestions email-me please.                                 *)
(*==============================================================================*)
(* History:                                                                     *)
(* October, 20, 1997 - Version 1.1a                                             *)
(*   . Bug fix! Anchor Style don't work correctly in QRAngledDBText with QR1    *)
(*   . Bug fix! .EXE Preview and Print don't work with some versions of QR2     *)
(*   . Bug fix! The first QRAngledLabel placed directly in the QuickRep surface *)
(*              aren't printed rotated in QR2                                   *)
(* September, 25, 1997 - Version 1.1                                            *)
(*   . New base class TQRAngledCustom. Totaly rewriten to works better with QR2 *)
(*     Several print problems solved                                            *)
(*   . New component! QRAngledDBText                                            *)
(*   . New property! AnchorStyle                                                *)
(*   . New property! TrueTypeAlert                                              *)
(* August, 27, 1997 - Version 1.0c                                              *)
(*   . Bug fix! Cause a GPF when not placed in a TQuickRep in QR2               *)
(*   . Bug fix! Preview and Print don't work in QR2 under D1                    *)
(* August, 17, 1997 - Version 1.0b                                              *)
(*   . Minor adjustments                                                        *)
(* August, 03, 1997 - Version 1.0a                                              *)
(*   . Bug fix! Several print bugs fixed under QR1 and QR2                      *)
(*   . Bug fix! Zoom of ParentReport considered in paint routine of QR2         *)
(* July, 26, 1997 - Version 1.0                                                 *)
(*   . Initial version                                                          *)
(*==============================================================================*)
(* Know problems:                                                               *)
(*                                                                              *)
(* QR2. The DataField property of QRAngledDBText must be assigned manually      *)
(********************************************************************************)

unit QrAngLbl;

{$I QRANGLBL.INC}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, DB, DBTables, QrAConst,
  {$IFDEF QR1X}
    {$IFDEF VER100}
      DBCtrls ,
    {$ENDIF} {DEF VER100}
    QuickRep
  {$ELSE}
    QuickRpt, QrCtrls
  {$ENDIF} {DEF QR1X} ;

type
  TAnchorStyle = (asNone, asTextLeft, asTextCenter, asTextRight);
  TTrueTypeAlert = (ttaNone, ttaMessage, ttaAbort, ttaAbortMessage);

  TAngledValues = record
    fntWidth,
    fntHeight,
    txtWidth,
    txtHeight,
    gapTxtWidth,
    gapTxtHeight,
    totWidth,
    totHeight,
    posLeft,
    posTop,
    posX,
    posY: Integer
  end;

{$IFDEF QR1X}
  TQRAngledCustom = class(TQRCustomControl)
{$ELSE}
  TQRAngledCustom = class(TQRPrintable)
{$ENDIF} {DEF QR1X}
  private
    FAnchorStyle: TAnchorStyle;
    FAngle: Integer;
    FAutoSize: Boolean;
    FCaption: TCaption;
    FOnPrintEvent: TQRLabelOnPrintEvent;
    FTTFonts: TStringList;
    FTrueTypeAlert: TTrueTypeAlert;
    FValues: TAngledValues;
    procedure AdjustSizes;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
{$IFDEF QR1X}
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
{$ENDIF} {DEF QR1X}
  protected
    procedure BuildTTFontsList;
    procedure CalculateAngledValues(pCanvas: TCanvas; pCaption: TCaption; pZoom: Integer);
    function GetCaption: TCaption;
    function IsTrueTypeFont(pFont: TFont): Boolean;
    procedure Loaded; override;
    procedure Paint; override;
    procedure Print(X, Y: Integer); override;
    procedure SetAnchorStyle(pValue: TAnchorStyle);
    procedure SetAngle(pValue: Integer);
    procedure SetAutoSize(pValue: Boolean); override;  //### Rotina passou a categoria de OVERRIDED
    procedure SetCaption(pValue: TCaption);
    procedure SetName(const pValue: TComponentName); override;
    procedure SetTrueTypeAlert(pValue: TTrueTypeAlert);
{$IFDEF QR2X}
    procedure CreateWnd; override;
{$ENDIF} {DEF QR2X}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property AnchorStyle: TAnchorStyle read FAnchorStyle write SetAnchorStyle;
    property Angle: Integer read FAngle write SetAngle default 45;
    property AutoSize: Boolean read  FAutoSize write SetAutoSize default True;
    property Caption: TCaption read GetCaption write SetCaption stored True;
    property OnPrint: TQRLabelOnPrintEvent read FOnPrintEvent write FOnPrintEvent;
    property TrueTypeAlert: TTrueTypeAlert read FTrueTypeAlert write SetTrueTypeAlert;
  end;

  TQRAngledLabel = class(TQRAngledCustom)
  published
    { Inherited properties republished }
    property AnchorStyle;
    property Angle;
    property AutoSize;
    property Caption;
    property Color;
    property Font;
    property OnPrint;
    property ParentFont;
    property Transparent;
    property TrueTypeAlert;
  end;

  TQRAngledDBText = class(TQRAngledCustom)
  private
{$IFDEF QR2X}
    FDataSourceName: string[30];
    FDataSet: TDataSet;
    FDataField: string;
    FField: TField;
    FFieldNumber: Integer;
    FFieldOK: Boolean;
    FMask: string;
    procedure SetDataField(pValue: string);
    procedure SetDataSet(pValue: TDataSet);
    procedure SetMask(pValue: string);
{$ENDIF} {DEF QR2X}
  protected
{$IFDEF QR1X}
    FDataLink: TFieldDataLink;
    procedure DataChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    procedure SetDataField(const pValue: string);
    procedure SetDataSource(pValue: TDataSource);
{$ELSE}
    procedure DefineProperties(pFiler: TFiler); override;
    procedure Loaded; override;
    procedure Prepare; override;
    procedure ReadValues(pReader: TReader); virtual;
    procedure UnPrepare; override;
    procedure WriteValues(pWriter: TWriter); virtual;
{$ENDIF} {DEF QR1X}
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Print(X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
{$IFDEF QR1X}
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
{$ELSE}
    property DataField: string read FDataField write SetDataField;
    property DataSet: TDataSet read FDataSet write SetDataSet;
    property Mask: string read FMask write SetMask;
{$ENDIF} {DEF QR1X}
    { Inherited properties republished }
    property AnchorStyle;
    property Angle;
    property AutoSize;
    property Color;
    property Font;
    property OnPrint;
    property ParentFont;
    property Transparent;
    property TrueTypeAlert;
  end;

procedure Register;

implementation

{$IFDEF WIN32}				//****** RECURSOS ALOCADOS COMO PADRAO NO dcr DO PACOTE
  { $ R QRANGLBL.D32}
{$ELSE} {DEF WIN32}
  { $ R QRANGLBL.D16}
{$ENDIF} {DEF WIN32}

procedure Register;
//----------------------------------------------------------------------------------------------------------------------
begin
  RegisterComponents( 'General Imports', [TQRAngledLabel, TQRAngledDBText]);
end;

{ Common routines ********************************************************************** }

function DegToRad(pDegrees: Real): Real;
begin
  Result := (pDegrees * PI / 180);
end;

function EnumTTFontsProc(var pLogFont: TLogFont; var pTextMetric: TTextMetric;
 pFontType: Integer; pData: Pointer): Integer; export;
 {$IFDEF WIN32} StdCall; {$ENDIF}
begin
  if (pFontType and TRUETYPE_FONTTYPE = TRUETYPE_FONTTYPE) then
    TStringList(pData^).Add(StrPas(pLogFont.lfFaceName))
  ;
  Result := 1;
end;

procedure CreateAngledFont(pCanvas: TCanvas; pAngle: Integer);
{ Create angled font. Procedure writen by Keith Wood }
var
  FntLogRec: TLogFont { Storage area for font information } ;
begin
  { Get the current font information. We only want to modify the angle }
  GetObject(pCanvas.Font.Handle, SizeOf(FntLogRec), Addr(FntLogRec));

  { Modify the angle. "The angle, in tenths of a degrees, between the base
    line of a character and the x-axis." (Windows API Help file.) }
  FntLogRec.lfEscapement := (pAngle * 10);
  FntLogRec.lfOutPrecision := OUT_TT_ONLY_PRECIS;  { Request TrueType precision }

  { Delphi will handle the deallocation of the old font handle }
  pCanvas.Font.Handle := CreateFontIndirect(FntLogRec);
end;

{ End of Common routines *************************************************************** }

{ TQRAngledCustom ********************************************************************** }

constructor TQRAngledCustom.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAutoSize := True;
  Transparent := False;
  FTTFonts := TStringList.Create;
{$IFDEF QR1X}
  BuildTTFontsList;
{$ENDIF} {DEF QR1X}
  SetTrueTypeAlert(ttaAbort);
  FAnchorStyle := asNone;
  SetAngle(45) { Default angle } ;
end;

destructor TQRAngledCustom.Destroy;
begin
  FTTFonts.Free;
  inherited Destroy;
end;

procedure TQRAngledCustom.AdjustSizes;
begin
{$IFDEF QR2X}
  if HandleAllocated then
{$ELSE}
  if (Parent <> nil) and (Parent.HandleAllocated) then
{$ENDIF} {DEF QR2X}
  begin
    CalculateAngledValues(Canvas, Caption, {$IFDEF QR2X} Zoom {$ELSE} 100 {$ENDIF});
    if AutoSize then
      SetBounds(FValues.posLeft, FValues.posTop, FValues.totWidth, FValues.totHeight)
    ;
  end;
end;

procedure TQRAngledCustom.BuildTTFontsList;
var
  DC: HDC;
  EnumProc: TFarProc;
begin
{$IFDEF QR2X}
  if not HandleAllocated then
{$ELSE}
  if (Parent <> nil) and (not Parent.HandleAllocated) then
{$ENDIF} {DEF QR2X}
    Exit
  ;

  DC := GetDC(0);
  FTTFonts.Clear;
  try
    EnumProc := MakeProcInstance(@EnumTTFontsProc, HInstance);
    try
      EnumFonts(DC, nil, EnumProc, @FTTFonts);
    finally
      FreeProcInstance(EnumProc);
    end;
  finally
    ReleaseDC(0, DC);
  end;
end;

procedure TQRAngledCustom.CalculateAngledValues(pCanvas: TCanvas; pCaption: TCaption;
 pZoom: Integer);
var
  angB: Real;
  nCenterX, nCenterY: Integer;
begin
  pCanvas.Font.Assign(Font);
  if pCanvas.Font.Size <> Round(Font.Size * pZoom / 100) then
    pCanvas.Font.Size := Round(Font.Size * pZoom / 100)
  ;

  CreateAngledFont(pCanvas, FAngle);

  { Calculate intermediate values }
  FValues.fntWidth := pCanvas.TextWidth(pCaption);
  FValues.fntHeight := pCanvas.TextHeight(pCaption);
  case FAngle of
    0..89   : angB := DegToRad(90 - FAngle);
    90..179 : angB := DegToRad(FAngle - 90);
    180..269: angB := DegToRad(270 - FAngle);
  else { 270..359 }
    angB := DegToRad(FAngle - 270)
  end;
  FValues.txtWidth := Round(sin(angB) * FValues.fntWidth);
  FValues.gapTxtWidth := Round(cos(angB) * FValues.fntHeight);
  FValues.txtHeight := Round(cos(angB) * FValues.fntWidth);
  FValues.gapTxtHeight := Round(sin(angB) * FValues.fntHeight);

  { Calculate new sizes of component }
  FValues.totWidth := (FValues.txtWidth + FValues.gapTxtWidth);
  FValues.totHeight := (FValues.txtHeight + FValues.gapTxtHeight);

  { Calculate Anchor positon of component }
  if FAnchorStyle in [asNone] then
  begin
    FValues.posLeft := Left;
    FValues.posTop := Top;
  end
  else
    if FAnchorStyle in [asTextLeft] then
    begin
      { Calculate Left position }
      case FAngle of 0..89, 270..359:
        FValues.posLeft := Left
      else { 90..179, 180..269 }
        FValues.posLeft := (Left + Width - FValues.totWidth)
      end;
      { Calculate Top position }
      case FAngle of 180..269, 270..359:
        FValues.posTop := Top
      else { 0..89, 90..179 }
        FValues.posTop := (Top + Height - FValues.totHeight)
      end;
    end
    else
      if FAnchorStyle in [asTextRight] then
      begin
        { Calculate Left position }
        case FAngle of 90..179, 180..269:
          FValues.posLeft := Left
        else { 0..89, 270..359 }
          FValues.posLeft := (Left + Width - FValues.totWidth)
        end;
        { Calculate Top position }
        case FAngle of 0..89, 90..179:
          FValues.posTop := Top
        else { 180..269, 270..359 }
          FValues.posTop := (Top + Height - FValues.totHeight)
        end;
      end
      else { asTextCenter }
      begin
        FValues.posLeft := (Left + Round((Width - FValues.totWidth) / 2));
        FValues.posTop := (Top + Round((Height - FValues.totHeight) / 2));
      end
  ;

  { Calculate draw position of text }
  case FAngle of
    0..89:
    begin
      FValues.posX := 0;
      FValues.posY := FValues.txtHeight
    end;
    90..179:
    begin
      FValues.posX := FValues.txtWidth;
      FValues.posY := FValues.totHeight
    end;
    180..269:
    begin
      FValues.posX := FValues.totWidth;
      FValues.posY := FValues.gapTxtHeight
    end;
  else { 270..359 }
    begin
      FValues.posX := FValues.gapTxtWidth;
      FValues.posY := 0
    end;
  end;

  { Calculate draw position of text inside area of component }
  if (FAnchorStyle in [asTextLeft, asTextRight, asTextCenter]) and not AutoSize then
    if FAnchorStyle in [asTextLeft] then
      case FAngle of
        0..89:
        begin
          FValues.posX := 0;
          FValues.posY := (Height - FValues.gapTxtHeight);
        end;
        90..179:
        begin
          FValues.posX := (Width - FValues.gapTxtWidth);
          FValues.posY := Height;
        end;
        180..279:
        begin
          FValues.posX := Width;
          FValues.posY := FValues.gapTxtHeight;
        end;
      else { 280..359 }
        begin
          FValues.posX := FValues.gapTxtWidth;
          FValues.posY := 0;
        end;
      end
    else
      if FAnchorStyle in [asTextRight] then
        case FAngle of
          0..89:
          begin
            FValues.posX := (Width - FValues.txtWidth - FValues.gapTxtWidth);
            FValues.posY := FValues.txtHeight;
          end;
          90..179:
          begin
            FValues.posX := FValues.txtWidth;
            FValues.posY := (FValues.txtHeight + FValues.gapTxtHeight);
          end;
          180..279:
          begin
            FValues.posX := (FValues.txtWidth + FValues.gapTxtWidth);
            FValues.posY := (Height - FValues.txtHeight);
          end;
        else { 280..359 }
          begin
            FValues.posX := (Width - FValues.txtWidth);
            FValues.posY := (Height - FValues.txtHeight - FValues.gapTxtHeight);
          end;
        end
      else { asTextCenter }
        begin
          nCenterX := Round((Width - FValues.txtWidth - FValues.gapTxtHeight) / 2);
          nCenterY := Round((Height - FValues.txtHeight - FValues.gapTxtHeight) / 2);
          case FAngle of
            0..89:
            begin
              FValues.posX := nCenterX;
              FValues.posY := (nCenterY + FValues.txtHeight);
            end;
            90..179:
            begin
              FValues.posX := (nCenterX + FValues.txtWidth);
              FValues.posY := (nCenterY + FValues.txtHeight + FValues.gapTxtHeight);
            end;
            180..279:
            begin
              FValues.posX := (nCenterX + FValues.txtWidth + FValues.gapTxtWidth);
              FValues.posY := (nCenterY + FValues.gapTxtHeight);
            end;
          else { 280..359 }
            begin
              FValues.posX := (nCenterX + FValues.gapTxtWidth);
              FValues.posY := nCenterY;
            end;
          end
        end
  ;
end;

procedure TQRAngledCustom.CMFontChanged(var Message: TMessage);
begin
  inherited;

  { Prevent non TrueType Fonts }
  if not (FTrueTypeAlert in [ttaNone]) then
    if (not IsTrueTypeFont(Font)) then
      if (FTrueTypeAlert in [ttaMessage]) then
        ShowMessage(LoadStr(STTAlertMessage))
      else
      begin
        { TControl class haven't Font property published!!! }
{$IFDEF QR2X}
        if ((Parent is TQRCustomBand) and IsTrueTypeFont((Parent as TQRCustomBand).Font)) or
         ((Parent is TQuickRep) and IsTrueTypeFont((Parent as TQuickRep).Font)) then
{$ELSE}
        if ((Parent is TQRBand) and IsTrueTypeFont((Parent as TQRBand).Font)) then
{$ENDIF} {DEF QR2X}
          ParentFont := True { Assign parent font }
        else
        begin
(*
          ParentFont := False;
*)
          Font.Name := 'Arial';
        end;

        if (FTrueTypeAlert in [ttaAbortMessage]) then
          ShowMessage(FmtLoadStr(STTAbortMessage, [Font.Name]))
        ;
      end
  ;

  AdjustSizes;
end;

procedure TQRAngledCustom.CMTextChanged(var Message: TMessage);
begin
  AdjustSizes;
  Invalidate;
end;

{$IFDEF QR2X}
procedure TQRAngledCustom.CreateWnd;
begin
  inherited CreateWnd;
  BuildTTFontsList;
  AdjustSizes;
end;
{$ENDIF} {DEF QR2X}

function TQRAngledCustom.GetCaption: TCaption;
begin
  Result := FCaption;
end;

function TQRAngledCustom.IsTrueTypeFont(pFont: TFont): Boolean;
begin
  Result := (FTTFonts.IndexOf(pFont.Name) <> (-1));
end;

procedure TQRAngledCustom.Loaded;
begin
  inherited Loaded;
{$IFDEF QR2X}
  if not HandleAllocated then
    HandleNeeded
  ;
{$ELSE}
  if (Parent <> nil) and (not Parent.HandleAllocated) then
    Parent.HandleNeeded
  ;
{$ENDIF} {DEF QR2X}
end;

procedure TQRAngledCustom.Paint;
var
  aRect: TRect;
begin
  CalculateAngledValues(Canvas, Caption, {$IFDEF QR2X} Zoom {$ELSE} 100 {$ENDIF});
{$IFDEF QR2X}
  inherited Paint;
{$ENDIF}

  with Canvas do
  begin
    aRect := Rect(0, 0, Width, Height);

    if not Transparent then
    begin
      Brush.Color := Color;
      Brush.Style := bsSolid;
      FillRect(aRect);
    end;

    ExtTextOut(Handle, FValues.posX, FValues.posY, ETO_CLIPPED, @aRect,
     @FCaption[1], Length(FCaption), nil);
  end;
end;

procedure TQRAngledCustom.Print(X, Y: Integer);
var
  aCanvas: TCanvas;
  OldCaption, NewCaption: string;
  aRect: TRect;
{$IFDEF QR2X}
  PixFactor: Extended;
{$ENDIF} {DEF QR2X}
  recLf, recTp, recRt, recBt, txtXP, txtYP: Integer;
  Str : PChar;
begin
  OldCaption := Caption;
  NewCaption := Caption;
  if Assigned(FOnPrintEvent) and not (csDesigning in ComponentState) then
  begin
    FOnPrintEvent(Self, NewCaption);
    if NewCaption <> FCaption then
    begin
      OldCaption := Caption;
      SetCaption(NewCaption);
    end;
  end;

{$IFDEF QR2X}
  PixFactor := (Height / Size.Height);
{$ENDIF} {DEF QR2X}
  aCanvas := QRPrinter.Canvas;
  aCanvas.Font.Assign(Font);
{$IFDEF QR1X}
  aCanvas.Font.Size := Trunc(Abs(ParentReport.XPos(Font.Size)));
{$ENDIF} {DEF QR1X}
  CreateAngledFont(aCanvas, Angle);

  with aCanvas do
  begin
{$IFDEF QR2X}
    with ParentReport.QRPrinter do
    begin
      recLf := XPos(X + Size.Left);
      recTp := YPos(Y + Size.Top);
      recRt := XPos(X + Size.Left + Size.Width);
      recBt := YPos(Y + Size.Top + Size.Height);
      txtXP := XPos(X + Size.Left + Round(FValues.posX / PixFactor));
      txtYP := YPos(Y + Size.Top + Round(FValues.posY / PixFactor));
    end;
{$ELSE}
    with ParentReport do
    begin
      recLf := XPos(X + Left);
      recTp := YPos(Y + Top);
      recRt := XPos(X + Left + Width);
      recBt := YPos(Y + Top + Height);
      txtXP := XPos(X + Left + FValues.posX);
      txtYP := YPos(Y + Top + FValues.posY);
    end;
{$ENDIF} {DEF QR2X}

    aRect := Rect(recLf, recTp, recRt, recBt);

    if not Transparent then
    begin
      Brush.Color := Color;
      Brush.Style := bsSolid;
      FillRect(aRect);
    end;
    Brush.Style := bsClear;

    SetTextAlign(Handle, TA_LEFT + TA_TOP + TA_NOUPDATECP);
    Str:=PChar( NewCaption );
    ExtTextOut(Handle, txtXP, txtYP, ETO_CLIPPED, @aRect, Str, Length(NewCaption), nil);
   //***NOTAS: Criada Str para suportar caso onde NewCaption = '' usado na linha abaixo
   //ExtTextOut(Handle, txtXP, txtYP, ETO_CLIPPED, @aRect, @NewCaption[1], Length(NewCaption), nil);
  end;

{$IFDEF QR2X}
  inherited Print(X, Y);
{$ENDIF}

  if OldCaption <> NewCaption then
    SetCaption(OldCaption)
  ;
end;

procedure TQRAngledCustom.SetAnchorStyle(pValue: TAnchorStyle);
begin
  if FAnchorStyle <> pValue then
  begin
    FAnchorStyle := pValue;
    AdjustSizes;
    Invalidate { Redraw } ;
  end;
end;

procedure TQRAngledCustom.SetAngle(pValue: Integer);
begin
  if FAngle <> (pValue mod 360) then
  begin
    FAngle := (pValue mod 360);
    AdjustSizes;
    Invalidate { Redraw } ;
  end;
end;

procedure TQRAngledCustom.SetAutoSize(pValue: Boolean);
begin
  if FAutoSize <> pValue then
    FAutoSize := pValue
  ;
  if FAutoSize then
    AdjustSizes
  ;
end;

procedure TQRAngledCustom.SetCaption(pValue: TCaption);
begin
  if FCaption <> pValue then
  begin
    FCaption := pValue;
    Perform(CM_TEXTCHANGED, 0, 0);
  end;
end;

procedure TQRAngledCustom.SetName(const pValue: TComponentName);
var
  OldName: TComponentName;
begin
  if Name <> pValue then
  begin
    OldName := Name;
    inherited SetName(pValue);
    if ((Caption = '') or (Caption = OldName)) then
      SetCaption(pValue)
    ;
  end;
end;

procedure TQRAngledCustom.SetTrueTypeAlert(pValue: TTrueTypeAlert);
var
  Message: TMessage;
begin
  if FTrueTypeAlert <> pValue then
  begin
    FTrueTypeAlert := pValue;
    CMFontChanged(Message);
  end;
end;

{$IFDEF QR1X}
procedure TQRAngledCustom.WMPaint(var Message: TWMPaint);
begin
  inherited;
  if AutoSize then
    AdjustSizes
  ;
end;
{$ENDIF} {DEF QR1X}

{ End of TQRAngledCustom *************************************************************** }

{ TQRAngledDBText ********************************************************************** }

constructor TQRAngledDBText.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF QR1X}
  FDataLink := TFieldDataLink.Create;
  FDataLink.OnDataChange := DataChange;
{$ELSE}
  FDataSourceName := '';
{$ENDIF} {DEF QR1X}
end;

destructor TQRAngledDBText.Destroy;
begin
{$IFDEF QR1X}
  FDataLink.Free;
  FDataLink := nil;
{$ENDIF} {DEF QR1X}
  inherited Destroy;
end;

{$IFDEF QR1X}

{ Start of QuickReport 1.x DB Routines ------------------------------------------------- }

procedure TQRAngledDBText.DataChange(Sender: TObject);
begin
  if FDataLink.Field <> nil then
    Caption := FDataLink.Field.DisplayText
  else
    if csDesigning in ComponentState then
      Caption := Name
    else
      Caption := ''
  ;
end;

function TQRAngledDBText.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

function TQRAngledDBText.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TQRAngledDBText.Print(X, Y: Integer);
begin
  CalculateAngledValues(Canvas, Caption, 100);
  inherited Print(X, Y);
end;

procedure TQRAngledDBText.SetDataField(const pValue: string);
begin
  FDataLink.FieldName := pValue;
end;

procedure TQRAngledDBText.SetDataSource(pValue: TDataSource);
begin
  FDataLink.DataSource := pValue;
end;

{ End of QuickReport 1.x DB Routines --------------------------------------------------- }

{$ELSE} {DEF QR1X}

{ Start of QuickReport 2.x DB Routines ------------------------------------------------- }

procedure TQRAngledDBText.DefineProperties(pFiler: TFiler);
begin
  pFiler.DefineProperty('DataSource', ReadValues, WriteValues, False);
  inherited DefineProperties(pFiler);
end;

procedure TQRAngledDBText.Loaded;
var
  aComponent: TComponent;
begin
  inherited Loaded;
  if FDataSourceName <> '' then   begin
    aComponent := Owner.FindComponent(FDataSourceName);
    if (aComponent <> nil) and (aComponent is TDataSource) then
      DataSet:=TDataSource(aComponent).DataSet;
  end;
end;

procedure TQRAngledDBText.Prepare;
begin
  inherited Prepare;
  if Assigned(FDataSet) then begin
    FField := FDataSet.FindField(FDataField);
    if FField <> nil then  begin
      FFieldNumber := FField.Index;
      FFieldOK := True;
    end;
  end  else   begin
    FField := nil;
    FFieldOK := False;
  end;
end;

procedure TQRAngledDBText.Print(X, Y: Integer);
begin
  if FFieldOK then
  begin
    if FDataSet.DefaultFields then
      FField := FDataSet.Fields[FFieldNumber]
    ;
  end
  else
    FField := nil
  ;

  if Assigned(FField) then
    try
      if (Mask = '') or (FField is TStringField) then
        Caption := FField.DisplayText
      else
        if (FField is TNumericField) then
           Caption := FormatFloat(Mask, (FField as TNumericField).AsFloat)
        else
          if (FField is TDateTimeField) then
            Caption := FormatDateTime(Mask, (FField as TDateTimeField).AsDateTime)
      ;
    except
      Caption := '';
    end
  else
    Caption := ''
  ;

  inherited Print(X, Y);
end;

procedure TQRAngledDBText.ReadValues(pReader: TReader);
begin
  FDataSourceName := pReader.ReadIdent;
end;

procedure TQRAngledDBText.SetDataField(pValue: string);
begin
  FDataField := pValue;
  Caption := pValue;
end;

procedure TQRAngledDBText.SetDataSet(pValue: TDataSet);
begin
  FDataSet := pValue;
{$IFDEF WIN32}
  if pValue <> nil then
    pValue.FreeNotification(Self)
  ;
{$ENDIF} {DEF WIN32}
end;

procedure TQRAngledDBText.SetMask(pValue: string);
begin
  if FMask <> pValue then
    FMask := pValue
  ;
end;

procedure TQRAngledDBText.Unprepare;
begin
  FField := nil;
  inherited Unprepare;
  if FDataField <> '' then
    SetDataField(FDataField) { Reset caption }
  else
    SetDataField(Name)
  ;
end;

procedure TQRAngledDBText.WriteValues(pWriter: TWriter);
begin
{ nothing }
end;

{ End of QuickReport 2.x DB Routines --------------------------------------------------- }

{$ENDIF} {DEF QR1X}

procedure TQRAngledDBText.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
{$IFDEF QR1X}
  if (Operation = opRemove) and (FDataLink <> nil) and (AComponent = DataSource) then
    DataSource := nil
  ;
{$ELSE}
  if Operation = opRemove then
    if AComponent = FDataSet then
      FDataSet := nil
  ;
{$ENDIF} {DEF QR1X}
end;

end.
