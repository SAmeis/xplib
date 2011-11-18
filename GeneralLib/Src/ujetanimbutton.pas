///<history>
///Revision - 20111108 - Roger
/// Alterações da versão original para permite a inserção em pacote de design e runtime separados
/// Inseridas as diretivas de compilação do pacote
///</history>

{$IFDEF ujetanimbutton}
{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I GeneralLib.inc}


{TODO -oroger -clib : Inserir diretivas de compilaçao padrao}

//{$A+,B-,D+,F-,G+,I+,K+,L+,N+,P+,Q-,R-,S+,T-,V-,W-,X+,Y+}

unit ujetanimbutton;

interface

uses
    SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
    Forms, Buttons, DesignConst, DesignIntf, ExtCtrls;
    {TODO -oroger -cdoc : Inserir alteração de pacote de design para esta unit}

type
    TJetAnimButtonAnimStyle = (asRepeat, asPingPong, asPingPongRepeat);


    TJetAnimButton = class(TGraphicControl)
    private
        FTimer :          TTimer;
        FStateChanged :   boolean;
        FCounterUp :      Integer;
        FCounterOver :    Integer;
        FCounterDown :    Integer;
        FInterval :       Integer;
        FAnimated :       boolean;
        FAutoSize :       boolean;
        FBitmapUp :       TBitmap;
        FBitmapDown :     TBitmap;
        FBitmapOver :     TBitmap;
        FBitmapDisabled : TBitmap;
        FImageListUp :    TImageList;
        FImageListOver :  TImageList;
        FImageListDown :  TImageList;
        TempBitmap :      TBitmap;
        MouseOver :       boolean;
        FTransparentColor : TColor;
        FAnimStyle :      TJetAnimButtonAnimStyle;
        AnimDir :         Integer;
        procedure AdjustBounds;
        procedure BitmapUpChanged(Sender : TObject);
        procedure BitmapDownChanged(Sender : TObject);
        procedure BitmapOverChanged(Sender : TObject);
        procedure BitmapDisabledChanged(Sender : TObject);
        procedure SetBitmapDown(Value : TBitmap);
        procedure SetBitmapUp(Value : TBitmap);
        procedure SetBitmapOver(Value : TBitmap);
        procedure SetBitmapDisabled(Value : TBitmap);
        procedure SetImageListUp(Value : TImageList);
        procedure SetImageListOver(Value : TImageList);
        procedure SetImageListDown(Value : TImageList);
        procedure SetInterval(Value : Integer);
        procedure SetAnimated(Value : boolean);
        procedure SetAnimStyle(Value : TJetAnimButtonAnimStyle);
        procedure CMDialogChar(var Message : TCMDialogChar); message CM_DIALOGCHAR;
        procedure CMFontChanged(var Message : TMessage); message CM_FONTCHANGED;
        procedure CMTextChanged(var Message : TMessage); message CM_TEXTCHANGED;
        procedure CMSysColorChange(var Message : TMessage); message CM_SYSCOLORCHANGE;
        procedure CMMouseEnter(var Message : TMessage); message CM_MOUSEENTER;
        procedure CMMouseLeave(var Message : TMessage); message CM_MOUSELEAVE;
        procedure TimerEvent(Sender : TObject);
        function PtInMask(const X, Y : Integer) : boolean;
    protected
        FState :   TButtonState;
        FEnabled : boolean;
        function GetPalette : HPALETTE; override;
        procedure SetEnabled(Value : boolean); override;
        procedure DrawButtonText(Canvas : TCanvas; const Caption : string; TextBounds : TRect; State : TButtonState);
        procedure MouseDown(Button : TMouseButton; Shift : TShiftState; X, Y : Integer); override;
        procedure MouseMove(Shift : TShiftState; X, Y : Integer); override;
        procedure MouseUp(Button : TMouseButton; Shift : TShiftState; X, Y : Integer); override;
        procedure Paint; override;
        procedure Loaded; override;
        procedure Click; override;
    public
        constructor Create(AOwner : TComponent); override;
        destructor Destroy; override;
        procedure Invalidate; override;
        procedure SetBounds(ALeft, ATop, AWidth, AHeight : Integer); override;
    published
        property Animated : boolean read FAnimated write SetAnimated;
        property TransparentColor : TColor read FTransparentColor write FTransparentColor;
        //    property BitmapUp: TBitmap read FBitmapUp write SetBitmapUp;
        property BitmapDisabled : TBitmap read FBitmapDisabled write SetBitmapDisabled;
        property ImageListUp : TImageList read FImageListUp write SetImageListUp;
        property ImageListDown : TImageList read FImageListDown write SetImageListDown;
        property ImageListOver : TImageList read FImageListOver write SetImageListOver;
        property Interval : Integer read FInterval write SetInterval;
        property Enabled : boolean read FEnabled write SetEnabled;
        property AnimStyle : TJetAnimButtonAnimStyle read FAnimStyle write SetAnimStyle;
        property Caption;
        property Font;
        property ShowHint;
        property Visible;
        property OnClick;
        property OnMouseDown;
        property OnMouseMove;
        property OnMouseUp;
    end;

procedure Register;

implementation

uses
    DesignEditors;

type
    TAboutJetAnimButton = class(TPropertyEditor)
    public
        procedure Edit; override;
        function GetAttributes : TPropertyAttributes; override;
        function GetValue : string; override;
    end;
function MakeMask(const ColorBmp : TBitmap; TransparentColor : TColor) : TBitmap;
var
    Temp :      TRect;
    OldBkColor : TColorRef;
    TmpBitmap : Tbitmap;
begin
    Makemask  := nil;
    TmpBitmap := TBitmap.Create;
    try
        TmpBitmap.Monochrome := True;
        TmpBitmap.Width := ColorBmp.Width;
        TmpBitmap.Height := ColorBmp.Height;
        OldBkColor := SetBkColor(ColorBmp.Canvas.Handle, ColorToRGB(TransparentColor));
        Temp := Rect(0, 0, ColorBmp.Width, ColorBmp.Height);
        TmpBitmap.Canvas.CopyMode := cmSrcCopy;
        TmpBitmap.Canvas.CopyRect(Temp, ColorBmp.Canvas, Temp);
        SetBkColor(ColorBmp.Canvas.Handle, OldBkColor);
        MakeMask := TmpBitmap;
    except
        TmpBitmap.Free;
    end;
end;
////////////////////////////////////////////////////////////////////////////////////////
procedure TAboutJetAnimButton.Edit;
begin
    Application.MessageBox('TJetAnimButton v1.00 for Delphi32. (C) 1999 Jimmy Theo Weku.' + #10#13 +
        'for more information about how to use this component please read README.TXT that included with this component',
        'About TJetAnimButton Component', MB_OK + MB_ICONINFORMATION);
end;

function TAboutJetAnimButton.GetAttributes : TPropertyAttributes;
begin
    Result := [paMultiSelect, paDialog, paReadOnly];
end;

function TAboutJetAnimButton.GetValue : string;
begin
    Result := '(About)';
end;

////////////////////////////////////////////////////////////////////////////////////////
constructor TJetAnimButton.Create(AOwner : TComponent);
begin
    inherited Create(AOwner);
    SetBounds(0, 0, 50, 50);
    ControlStyle := [csCaptureMouse, csOpaque];
    FAnimated   := False;
    FInterval   := 100;
    FCounterUp  := 0;
    FCounterOver := 0;
    FCounterDown := 0;
    FTimer      := TTimer.Create(Self);
    FTimer.Enabled := FAnimated;
    FTimer.OnTimer := TimerEvent;
    FTimer.Interval := FInterval;
    FImageListUp := nil;
    FImageListDown := nil;
    FImageListOver := nil;
    FAutoSize   := True;
    FBitmapUp   := TBitmap.Create;
    FBitmapUp.OnChange := BitmapUpChanged;
    FBitmapDown := TBitmap.Create;
    FBitmapDown.OnChange := BitmapDownChanged;
    FBitmapOver := TBitmap.Create;
    FBitmapOver.OnChange := BitmapOverChanged;
    FBitmapDisabled := TBitmap.Create;
    FBitmapDisabled.OnChange := BitmapDisabledChanged;
    FTransparentColor := clWhite;
    TempBitmap  := nil;
    ParentFont  := True;
    FEnabled    := True;
    MouseOver   := False;
    FState      := bsUp;
    FAnimStyle  := asRepeat;
    FStateChanged := False;
end;

destructor TJetAnimButton.Destroy;
begin
    FBitmapUp.Free;
    FBitmapOver.Free;
    FBitmapDown.Free;
    FBitmapDisabled.Free;
    TempBitmap.Free;
    FTimer.Free;
    inherited Destroy;
end;

procedure TJetAnimButton.SetAnimStyle(Value : TJetAnimButtonAnimStyle);
begin
    if Value <> FAnimStyle then begin
        FAnimStyle    := Value;
        FStateChanged := True;
    end;
end;

procedure TJetAnimButton.SetBitmapUp(Value : TBitmap);
begin
    FBitmapUp.Assign(Value);
end;

procedure TJetAnimButton.SetBitmapDown(Value : TBitmap);
begin
    FBitmapDown.Assign(Value);
end;

procedure TJetAnimButton.SetBitmapOver(Value : TBitmap);
begin
    FBitmapOver.Assign(Value);
end;

procedure TJetAnimButton.SetBitmapDisabled(Value : TBitmap);
begin
    FBitmapDisabled.Assign(Value);
end;

procedure TJetAnimButton.BitmapUpChanged(Sender : TObject);
var
    Maskbmp : TBitmap;
    R : TRect;
begin
    AdjustBounds;
    MaskBmp := TBitmap.Create;
    MaskBmp.Width := FBitmapUp.Width;
    MaskBmp.Height := FBitmapUp.Height;
    R := Rect(0, 0, FBitmapUp.Width, FBitmapUp.Height);
    MaskBmp.Canvas.CopyRect(R, FBitmapUp.Canvas, R);
    FTransparentColor := FBitmapUp.TransparentColor;
    TempBitmap.Free;
    TempBitmap := MakeMask(MaskBmp, FTransparentColor);
    MaskBmp.Free;
    Invalidate;
end;

procedure TJetAnimButton.BitmapDownChanged(Sender : TObject);
begin
    Invalidate;
end;

procedure TJetAnimButton.BitmapOverChanged(Sender : TObject);
var
    Maskbmp : TBitmap;
    R : TRect;
begin
    if FBitmapUp.Empty then begin
        SetBounds(Left, Top, FBitmapOver.Width, FBitmapOver.Height);
        MaskBmp := TBitmap.Create;
        MaskBmp.Width := FBitmapOver.Width;
        MaskBmp.Height := FBitmapOver.Height;
        R := Rect(0, 0, FBitmapOver.Width, FBitmapOver.Height);
        FTransparentColor := FBitmapOver.TransparentColor;
        MaskBmp.Canvas.CopyRect(R, FBitmapOver.Canvas, R);
        TempBitmap.Free;
        TempBitmap := MakeMask(MaskBmp, FTransparentColor);
        MaskBmp.Free;
    end;
    Invalidate;
end;

procedure TJetAnimButton.BitmapDisabledChanged(Sender : TObject);
begin
    Invalidate;
end;

procedure TJetAnimButton.SetImageListUp(Value : TImageList);
begin
    FImageListUp  := Value;
    FstateChanged := True;
    if (csDesigning in componentstate) then begin
        if (FImageListUp <> nil) then begin
            FImageListUp.getbitmap(0, FBitmapUp);
        end else begin
            FBitmapUp.Free;
            FBitmapUp := TBitmap.Create;
            FBitmapUp.OnChange := BitmapUpChanged;
        end;
        Invalidate;
    end;
end;

procedure TJetAnimButton.SetImageListOver(Value : TImageList);
begin
    FImageListOver := Value;
    FstateChanged  := True;
end;

procedure TJetAnimButton.SetImageListDown(Value : TImageList);
begin
    FImageListDown := Value;
    FstateChanged  := True;
end;

procedure TJetAnimButton.SetAnimated(Value : boolean);
begin
    if FAnimated <> Value then begin
        FAnimated      := Value;
        FstateChanged  := True;
        FTimer.Enabled := FAnimated;
    end;
end;

procedure TJetAnimButton.SetInterval(Value : Integer);
begin
    if FInterval <> Value then begin
        FInterval := Value;
        if Animated then begin
            FTimer.Enabled  := False;
            FTimer.Interval := Value;
            FTimer.Enabled  := True;
        end else begin
            FTimer.Interval := Value;
        end;
    end;
end;

procedure TJetAnimButton.TimerEvent(Sender : TObject);
begin
    if FEnabled and MouseOver and not ((csDesigning in ComponentState) or (FState = bsDown)) then begin
        if FStateChanged then begin
            FCounterOver := 0;
            AnimDir      := 1;
            FStateChanged := False;
        end;
        if FCounterOver < 0 then begin
            FCounterOver := 0;
        end;
        if (FImageListOver <> nil) then begin
            FImageListOver.GetBitmap(FCounterOver, FBitmapOver);
        end;
        Repaint;
        FCounterOver := FCounterOver + AnimDir;
        if (FImageListOver <> nil) and ((FCounterOver >= FImageListOver.Count - 1) or (FCounterOver <= 0)) then begin
            case FAnimStyle of
                asPingPong : begin
                    AnimDir := AnimDir * -1;
                end;
            end;
        end;
        if (FImageListOver <> nil) and ((FCounterOver >= FImageListOver.Count) or (FCounterOver < 0)) then begin
            case FAnimStyle of
                asPingPongRepeat : begin
                    AnimDir := AnimDir * -1;
                end;
                asRepeat : begin
                    AnimDir      := 1;
                    FCounterOver := 0;
                end;
            end;
        end;
    end else
    if FEnabled and (FState = bsDown) and not (csDesigning in ComponentState) then begin
        if FStateChanged then begin
            FCounterDown := 0;
            AnimDir      := 1;
            FStateChanged := False;
        end;
        if FCounterDown < 0 then begin
            FCounterDown := 0;
        end;
        if (FImageListDown <> nil) then begin
            FImageListDown.GetBitmap(FCounterDown, FBitmapDown);
        end;
        Repaint;
        FCounterDown := FCounterDown + AnimDir;
        if (FImageListDown <> nil) and ((FCounterDown >= FImageListDown.Count - 1) or (FCounterDown <= 0)) then begin
            case FAnimStyle of
                asPingPong : begin
                    AnimDir := AnimDir * -1;
                end;
            end;
        end;
        if (FImageListDown <> nil) and ((FCounterDown >= FImageListDown.Count) or (FCounterDown < 0)) then begin
            case FAnimStyle of
                asPingPongRepeat : begin
                    AnimDir := AnimDir * -1;
                end;
                asRepeat : begin
                    AnimDir      := 1;
                    FCounterDown := 0;
                end;
            end;
        end;
    end else
    if FEnabled and (FState = bsUp) and not (csDesigning in ComponentState) then begin
        if FStateChanged then begin
            FCounterUp := 0;
            AnimDir    := 1;
            FStateChanged := False;
        end;
        if FCounterUp < 0 then begin
            FCounterUp := 0;
        end;
        if (FImageListUp <> nil) then begin
            FImageListUp.GetBitmap(FCounterUp, FBitmapUp);
        end;
        Repaint;
        FCounterUp := FCounterUp + AnimDir;
        if (FImageListUp <> nil) and ((FCounterUp >= FImageListUp.Count - 1) or (FCounterUp <= 0)) then begin
            case FAnimStyle of
                asPingPong : begin
                    AnimDir := AnimDir * -1;
                end;
            end;
        end;
        if (FImageListUp <> nil) and ((FCounterUp >= FImageListUp.Count) or (FCounterUp < 0)) then begin
            case FAnimStyle of
                asPingPongRepeat : begin
                    AnimDir := AnimDir * -1;
                end;
                asRepeat : begin
                    AnimDir    := 1;
                    FCounterUp := 0;
                end;
            end;
        end;
    end;
end;

procedure TJetAnimButton.Paint;
var
    W, H :    Integer;
    Composite, Mask, Overlay, CurrentBmp : TBitmap;
    R, NewR : TRect;
begin
    if csDesigning in ComponentState then begin
        with Canvas do begin
            Pen.Style   := psSolid;
            Brush.Style := bsClear;
            Rectangle(0, 0, Width, Height);
        end;
    end;

    if (FState in [bsDisabled, bsExclusive]) then begin
        if not FBitmapDisabled.Empty then begin
            CurrentBmp := FBitmapDisabled;
        end else begin
            CurrentBmp := FBitmapUp;
        end;
    end else
    if (FState = bsUp) and (not MouseOver) then begin
        CurrentBmp := FBitmapUp;
    end else
    if (FState = bsUp) and MouseOver then begin
        if not FBitmapOver.Empty then begin
            CurrentBmp := FBitmapOver;
        end else begin
            CurrentBmp := FBitmapUp;
        end;
    end else begin
        if not FBitmapDown.Empty then begin
            CurrentBmp := FBitmapDown;
        end else begin
            CurrentBmp := FBitmapUp;
        end;
    end;

    if not CurrentBmp.Empty then begin
        W    := Width;
        H    := Height;
        R    := ClientRect;
        NewR := R;

        Composite := TBitmap.Create;
        Overlay   := TBitmap.Create;

        try
            with Composite do begin
                Width  := W;
                Height := H;
                Canvas.CopyMode := cmSrcCopy;
                Canvas.CopyRect(R, Self.Canvas, R);
            end;

            with Overlay do begin
                Width  := W;
                Height := H;
                Canvas.CopyMode := cmSrcCopy;
                Canvas.Brush.Color := CurrentBmp.TransparentColor;
                Canvas.FillRect(R);
                Canvas.CopyRect(NewR, CurrentBmp.Canvas, R);
            end;

            Mask := MakeMask(Overlay, CurrentBmp.TransparentColor);
            try
                Composite.Canvas.CopyMode := cmSrcAnd;
                Composite.Canvas.CopyRect(R, Mask.Canvas, R);

                Overlay.Canvas.CopyMode := $00220326;
                Overlay.Canvas.CopyRect(R, Mask.Canvas, R);

                Composite.Canvas.CopyMode := cmSrcPaint;
                Composite.Canvas.CopyRect(R, Overlay.Canvas, R);

                Canvas.CopyMode := cmSrcCopy;
                Canvas.CopyRect(R, Composite.Canvas, R);

            finally
                Mask.Free;
            end;

        finally
            Composite.Free;
            Overlay.Free;
        end;

    end;

    if Length(Caption) > 0 then begin
        Canvas.Font := Self.Font;
        R := CLIENTRECT;
        DrawButtonText(Canvas, Caption, R, FState);
    end;

end;

function TJetAnimButton.PtInMask(const X, Y : Integer) : boolean;
begin
    Result := True;
    if TempBitmap <> nil then begin
        Result := (TempBitmap.Canvas.Pixels[X, Y] = clBlack);
    end;
end;

procedure TJetAnimButton.MouseMove(Shift : TShiftState; X, Y : Integer);
var
    Last : boolean;
begin
    inherited MouseMove(Shift, X, Y);
    Last      := MouseOver;
    MouseOver := PtInMask(X, Y);
    if (Last <> MouseOver) and (FState <> bsDown) then begin
        FStateChanged := True;
        if (not mouseover) and animated then //skip redraw on mouse over an animated
        begin
            if FBitmapUp.Empty and Enabled then begin
                Invalidate;
            end;
        end else
        if (not mouseover) and (not animated) then begin
            if FBitmapUp.Empty and Enabled then begin
                Invalidate;
            end else begin
                repaint;
            end;
        end else
        if mouseover and (not animated) then begin
            repaint;
        end;
    end;
end;

procedure TJetAnimButton.MouseDown(Button : TMouseButton; Shift : TShiftState;
    X, Y : Integer);
var
    Clicked : boolean;
begin
    inherited MouseDown(Button, Shift, X, Y);
    if (Button = mbLeft) and Enabled then begin
        Clicked := PtInMask(X, Y);
        if Clicked then begin
            FStateChanged := True;
            FState := bsDown;
            if FBitmapDown.Empty and Enabled then begin
                Invalidate;
            end else
            if (not FBitmapDown.Empty) and (not Animated) and Enabled then begin
                Repaint;
            end;
        end;
    end;
end;

procedure TJetAnimButton.MouseUp(Button : TMouseButton; Shift : TShiftState;
    X, Y : Integer);
var
    DoClick : boolean;
begin
    inherited MouseUp(Button, Shift, X, Y);
    DoClick := PtInMask(X, Y);
    if (FState = bsDown) then begin
        FStateChanged := True;
        FState := bsUp;
        if FBitmapUp.Empty and Enabled then begin
            Invalidate;
        end else
        if (not FBitmapUp.Empty) and (not Animated) and Enabled then begin
            Repaint;
        end else
        if Doclick and (not Animated) then begin
            repaint;
        end;
    end;
    if DoClick then begin
        MouseOver := True;
        Click;
    end;
end;

procedure TJetAnimButton.Click;
begin
    inherited Click;
end;

function TJetAnimButton.GetPalette : HPALETTE;
begin
    Result := FBitmapUp.Palette;
end;

procedure TJetAnimButton.CMDialogChar(var Message : TCMDialogChar);
begin
    with Message do begin
        if IsAccel(CharCode, Caption) and Enabled then begin
            Click;
            Result := 1;
        end else begin
            inherited;
        end;
    end;
end;

procedure TJetAnimButton.CMFontChanged(var Message : TMessage);
begin
    Invalidate;
end;

procedure TJetAnimButton.CMTextChanged(var Message : TMessage);
begin
    Invalidate;
end;

procedure TJetAnimButton.CMSysColorChange(var Message : TMessage);
begin
    BitmapDisabledChanged(Self);
end;

procedure TJetAnimButton.CMMouseEnter(var Message : TMessage);
begin
    inherited;
end;

procedure TJetAnimButton.CMMouseLeave(var Message : TMessage);
begin
    inherited;
    MouseOver := False;
    if (Fstate = bsDown) then begin
        FStateChanged := False;
    end else begin
        FStateChanged := True;
    end;
    if FBitmapUp.Empty and (not (Fstate = bsDown)) and Enabled then begin
        Invalidate;
    end else
    if (not FBitmapUp.Empty) and (not Animated) and Enabled then begin
        Repaint;
    end;
end;

procedure TJetAnimButton.DrawButtonText(Canvas : TCanvas; const Caption : string;
    TextBounds : TRect; State : TButtonState);
var
    CString : array[0..255] of char;
begin
    StrPCopy(CString, Caption);
    Canvas.Brush.Style := bsClear;
    if State = bsDown then begin
        OffsetRect(TextBounds, 1, 1);
    end;
    DrawText(Canvas.Handle, CString, -1, TextBounds,
        DT_CENTER or DT_VCENTER or DT_SINGLELINE);
end;

procedure TJetAnimButton.Loaded;
begin
    inherited Loaded;
    if FImageListUp <> nil then begin
        FImageListUp.GetBitmap(0, FBitmapUp);
    end;
    if (FBitmapUp <> nil) and (FBitmapUp.Width > 0) and (FBitmapUp.Height > 0) then begin
        BitmapUpChanged(Self);
        if FImageListDown <> nil then begin
            FImageListDown.GetBitmap(0, FBitmapDown);
        end;
        if FImageListOver <> nil then begin
            FImageListOver.GetBitmap(0, FBitmapOver);
        end;
    end else begin
        if FImageListDown <> nil then begin
            FImageListDown.GetBitmap(0, FBitmapDown);
        end;
        if FImageListOver <> nil then begin
            FImageListOver.GetBitmap(0, FBitmapOver);
        end;
        if (FBitmapOver <> nil) and (FBitmapOver.Width > 0) and (FBitmapOver.Height > 0) then begin
            BitmapOverChanged(Self);
        end;
    end;
end;

procedure TJetAnimButton.AdjustBounds;
begin
    SetBounds(Left, Top, Width, Height);
end;

procedure TJetAnimButton.SetBounds(ALeft, ATop, AWidth, AHeight : Integer);
var
    W, H : Integer;
begin
    W := AWidth;
    H := AHeight;
    if not (csReading in ComponentState) and FAutoSize and not FBitmapUp.Empty then begin
        W := FBitmapUp.Width;
        H := FBitmapUp.Height;
    end;
    inherited SetBounds(ALeft, ATop, W, H);
end;

procedure TJetAnimButton.Invalidate;
var
    R : TRect;
begin
    if (Visible or (csDesigning in ComponentState)) and
        (Parent <> nil) and Parent.HandleAllocated then begin
        R := BoundsRect;
        InvalidateRect(Parent.Handle, @R, True);
    end;
end;

procedure TJetAnimButton.SetEnabled(Value : boolean);
///  <summary>
/// Rotina passada da ser override para calar compilador
///  </summary>
///  <remarks>
///
///  </remarks>
begin
    if Value <> FEnabled then begin
        FEnabled := Value;
        if FEnabled = False then begin
            FState := bsDisabled;
        end else begin
            FState := bsUp;
        end;
        Invalidate;
    end;
end;

procedure Register;
begin
    RegisterComponents('Jet', [TJetAnimButton]);
    RegisterPropertyEditor(TypeInfo(TAboutJetAnimButton), TJetAnimButton, 'ABOUT', TAboutJetAnimButton);
end;

end.
