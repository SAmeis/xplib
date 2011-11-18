
{TODO -oroger -clib : Inserir diretivas de compilaçao padrao}


//{$A+,B-,D+,F-,G+,I+,K+,L+,N+,P+,Q-,R-,S+,T-,V-,W-,X+,Y+}
unit UjetButton;

interface

uses
    SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
    Forms, Buttons;
    {TODO -oroger -cdoc : Inserir alteração de pacote de design para esta unit}

type

    TJetButton = class(TGraphicControl)
    private
        FAutoSize :         boolean;
        FBitmapUp :         TBitmap;
        FBitmapDown :       TBitmap;
        FBitmapOver :       TBitmap;
        FBitmapDisabled :   TBitmap;
        TempBitmap :        TBitmap;
        MouseOver :         boolean;
        FTransparentColor : TColor;
        procedure AdjustBounds;
        procedure BitmapUpChanged(Sender : TObject);
        procedure BitmapDownChanged(Sender : TObject);
        procedure BitmapOverChanged(Sender : TObject);
        procedure BitmapDisabledChanged(Sender : TObject);
        procedure SetBitmapDown(Value : TBitmap);
        procedure SetBitmapUp(Value : TBitmap);
        procedure SetBitmapOver(Value : TBitmap);
        procedure SetBitmapDisabled(Value : TBitmap);
        procedure CMDialogChar(var Message : TCMDialogChar); message CM_DIALOGCHAR;
        procedure CMFontChanged(var Message : TMessage); message CM_FONTCHANGED;
        procedure CMTextChanged(var Message : TMessage); message CM_TEXTCHANGED;
        procedure CMSysColorChange(var Message : TMessage); message CM_SYSCOLORCHANGE;
        procedure CMMouseEnter(var Message : TMessage); message CM_MOUSEENTER;
        procedure CMMouseLeave(var Message : TMessage); message CM_MOUSELEAVE;
        function PtInMask(const X, Y : Integer) : boolean;
    protected
        FState :   TButtonState;
        FEnabled : boolean;
        function GetPalette : HPALETTE; override;
        procedure SetEnabled(Value : boolean);  override;
        procedure DrawButtonText(Canvas : TCanvas; const Caption : string; TextBounds : TRect; State : TButtonState);
        procedure MouseDown(Button : TMouseButton; Shift : TShiftState; X, Y : Integer); override;
        procedure MouseMove(Shift : TShiftState; X, Y : Integer); override;
        procedure MouseUp(Button : TMouseButton; Shift : TShiftState; X, Y : Integer); override;
        procedure Paint; override;
        procedure Click; override;
        procedure Loaded; override;
    public
        procedure Invalidate; override;
        constructor Create(AOwner : TComponent); override;
        destructor Destroy; override;
        procedure SetBounds(ALeft, ATop, AWidth, AHeight : Integer); override;
    published
        property TransparentColor : TColor read FTransparentColor write FTransparentColor;
        property BitmapUp : TBitmap read FBitmapUp write SetBitmapUp;
        property BitmapDown : TBitmap read FBitmapDown write SetBitmapDown;
        property BitmapOver : TBitmap read FBitmapOver write SetBitmapOver;
        property BitmapDisabled : TBitmap read FBitmapDisabled write SetBitmapDisabled;
        property Caption;
        property Enabled : boolean read FEnabled write SetEnabled;
        property Font;
        property ShowHint;
        property Visible;
        property OnClick;
        property OnMouseDown;
        property OnMouseMove;
        property OnMouseUp;
    end;

implementation

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
constructor TJetButton.Create(AOwner : TComponent);
begin
    inherited Create(AOwner);
    SetBounds(0, 0, 50, 50);
    ControlStyle := [csCaptureMouse, csOpaque];
    FAutoSize := True;
    FBitmapUp := TBitmap.Create;
    FBitmapUp.OnChange := BitmapUpChanged;
    FBitmapDown := TBitmap.Create;
    FBitmapDown.OnChange := BitmapDownChanged;
    FBitmapOver := TBitmap.Create;
    FBitmapOver.OnChange := BitmapOverChanged;
    FBitmapDisabled := TBitmap.Create;
    FBitmapDisabled.OnChange := BitmapDisabledChanged;
    FTransparentColor := clWhite;
    TempBitmap := nil;
    ParentFont := True;
    FEnabled := True;
    MouseOver := False;
    FState := bsUp;
end;

destructor TJetButton.Destroy;
begin
    FBitmapUp.Free;
    FBitmapDown.Free;
    FBitmapOver.Free;
    FBitmapDisabled.Free;
    TempBitmap.Free;
    inherited Destroy;
end;

procedure TJetButton.Paint;
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

function TJetButton.PtInMask(const X, Y : Integer) : boolean;
begin
    Result := True;
    if TempBitmap <> nil then begin
        Result := (TempBitmap.Canvas.Pixels[X, Y] = clBlack);
    end;
end;

procedure TJetButton.MouseMove(Shift : TShiftState; X, Y : Integer);
var
    Last : boolean;
begin
    inherited MouseMove(Shift, X, Y);
    Last      := MouseOver;
    MouseOver := PtInMask(X, Y);
    if (Last <> MouseOver) and (not (Fstate = bsDown)) then begin
        if FBitmapUp.Empty and Enabled then begin
            Invalidate;
        end else begin
            Repaint;
        end;
    end;
end;

procedure TJetButton.MouseDown(Button : TMouseButton; Shift : TShiftState;
    X, Y : Integer);
var
    Clicked : boolean;
begin
    inherited MouseDown(Button, Shift, X, Y);
    if (Button = mbLeft) and Enabled then begin
        Clicked := PtInMask(X, Y);
        if Clicked then begin
            FState := bsDown;
            //      MouseOver := true;
            Repaint;
        end;
    end;
end;

procedure TJetButton.MouseUp(Button : TMouseButton; Shift : TShiftState;
    X, Y : Integer);
var
    DoClick : boolean;
begin
    inherited MouseUp(Button, Shift, X, Y);
    DoClick := PtInMask(X, Y);
    if (FState = bsDown) then begin
        FState := bsUp;
        Repaint;
        if FBitmapUp.Empty and Enabled then begin
            Invalidate;
        end else begin
            Repaint;
        end;
    end;
    if DoClick then begin
        MouseOver := True;
        Click;
    end;
end;

procedure TJetButton.Click;
begin
    inherited Click;
end;

function TJetButton.GetPalette : HPALETTE;
begin
    Result := FBitmapUp.Palette;
end;

procedure TJetButton.SetBitmapUp(Value : TBitmap);
begin
    FBitmapUp.Assign(Value);
end;

procedure TJetButton.SetBitmapDown(Value : TBitmap);
begin
    FBitmapDown.Assign(Value);
end;

procedure TJetButton.SetBitmapOver(Value : TBitmap);
begin
    FBitmapOver.Assign(Value);
end;

procedure TJetButton.SetBitmapDisabled(Value : TBitmap);
begin
    FBitmapDisabled.Assign(Value);
end;

procedure TJetButton.BitmapUpChanged(Sender : TObject);
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

procedure TJetButton.BitmapDownChanged(Sender : TObject);
begin
    Invalidate;
end;

procedure TJetButton.BitmapOverChanged(Sender : TObject);
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

procedure TJetButton.BitmapDisabledChanged(Sender : TObject);
begin
    Invalidate;
end;

procedure TJetButton.CMDialogChar(var Message : TCMDialogChar);
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

procedure TJetButton.CMFontChanged(var Message : TMessage);
begin
    Invalidate;
end;

procedure TJetButton.CMTextChanged(var Message : TMessage);
begin
    Invalidate;
end;

procedure TJetButton.CMSysColorChange(var Message : TMessage);
begin
    BitmapUpChanged(Self);
    BitmapDownChanged(Self);
    BitmapOverChanged(Self);
    BitmapDisabledChanged(Self);
end;

procedure TJetButton.CMMouseEnter(var Message : TMessage);
begin
    inherited;
end;

procedure TJetButton.CMMouseLeave(var Message : TMessage);
begin
    inherited;
    MouseOver := False;
    if (Fstate = bsDown) then begin
        exit;
    end;
    if FBitmapUp.Empty and Enabled then begin
        Invalidate;
    end else begin
        Repaint;
    end;
end;

procedure TJetButton.DrawButtonText(Canvas : TCanvas; const Caption : string;
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

procedure TJetButton.Loaded;
begin
    inherited Loaded;
    if (FBitmapUp <> nil) and (FBitmapUp.Width > 0) and (FBitmapUp.Height > 0) then begin
        BitmapUpChanged(Self);
    end else
    if (FBitmapOver <> nil) and (FBitmapOver.Width > 0) and (FBitmapOver.Height > 0) then begin
        BitmapOverChanged(Self);
    end;
end;

procedure TJetButton.AdjustBounds;
begin
    SetBounds(Left, Top, Width, Height);
end;

procedure TJetButton.SetBounds(ALeft, ATop, AWidth, AHeight : Integer);
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

procedure TJetButton.Invalidate;
var
    R : TRect;
begin
    if (Visible or (csDesigning in ComponentState)) and
        (Parent <> nil) and Parent.HandleAllocated then begin
        R := BoundsRect;
        InvalidateRect(Parent.Handle, @R, True);
    end;
end;

procedure TJetButton.SetEnabled(Value : boolean);
///  <summary>
///    Metodo passado para override para calar compilador
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

end.
