{$IFDEF Grf_Hnd}
	{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I XPLib.inc}

unit Grf_Hnd;

interface

uses
    Windows, Classes, Dialogs, Graphics, Forms, Controls, Super, SysUtils, XPGraph;

function ExtentCharSizeAverage(Canvas : TCanvas) : TPoint;

{Retorna um TBitmap com a tela presente no momento da chamada. Nao esquecer de fazer um ASSIGN seguido de um Free para o resultado desta funcao}
procedure CaptureBmpScreen(Bmp : TBitMap);

//Converte valor em mm para pixels segundo o canvas dado
function MM2PixelHorizontal(DC : HDC; const mmValue : double) : integer;
//Converte valor em mm para pixels segundo o canvas dado
function MM2PixelVertical(DC : HDC; const mmValue : double) : integer;

//Informa se ponto esta contido no rect( descarta fronteira )
function PointInsideRect(const P : TPoint; const Rect : TRect) : boolean; deprecated;

//Centraliza um TRect sobre outro
function RectCenterOverRect(const FixedRect, MovRect : TRect) : TRect;

//Centraliza um TRect sobre outro
function RectCenterInsideRect(const FixedRect, MovRect : TRect) : TRect;

//Retorna o centro de um TRect usar metodos em TXPRect
function RectCenterPoint(const ARect : TRect) : TPoint; deprecated;

//Gera Rect a partir de seus limites
function RectFromPoints(const P1, P2 : TPoint) : TRect;

//Aumenta dimensoes do retangulo preservando seu centro
function RectGrow(const SourceRect : TRect; Delta : integer) : TRect;

//Retorna o retangulo gerado pela interseccao de outros 2
function RectIntersectRect(const R1, R2 : TRect) : TRect;

//Informa se OutterRect cirncunda InnerRect
function RectIsInsideRect(const OutterRect, InnerRect : TRect) : boolean;

//Indica se os retangulos se interceptam
function RectIsIntersection(const R1, R2 : TRect) : boolean; deprecated;

//Indica se o retangulo se deformou para um ponto
function RectIsNull(const R : TRect) : boolean; deprecated;

//Indica se a geometria do retangulo eh consistente
function RectIsValid(const R : TRect) : boolean;

//Seta valores por metodo para retangulo
function RectMake(Left, Top, Right, Bottom : integer) : TRect;

//Altera a posicao de um rect pelos valores passados
function RectMoveRelative(const SourceRect : TRect; DeltaX, DeltaY : integer) : TRect;

//Posiciona retangulo na posicao absoluta
function RectMoveTo(const SourceRect : TRect; PX, PY : integer) : TRect;

//Posiciona centro do retangulo na posicao absoluta
function RectMoveCenterTo(const SourceRect : TRect; CX, CY : integer) : TRect;

//Rescala rect preservando sua origem
function RectRescale(Rect : TRect; ScaleX, ScaleY : double; FixedCenter : boolean = True) : TRect;

//Altera a altura do rect
function RectSetHeight(Rect : TRect; NewHeight : word; FixedCenter : boolean = True) : TRect;

//Altera a largura do rect
function RectSetWidth(Rect : TRect; NewWidth : word; FixedCenter : boolean = True) : TRect;

{ standalone function: Fills the background of the canvas with wallpaper specified in user's Windows 95 settings }
procedure Wallpaper(Canvas : TCanvas);

implementation

uses
    Math, Types;


function ExtentCharSizeAverage(Canvas : TCanvas) : TPoint;
    //----------------------------------------------------------------------------------------------------------------------
    //Retorna extensao media do conjunto de caracteres conforme os atributos do canvas passado
var
    i :      integer;
    Buffer : array[0..51] of char;
begin
    for i := 0 to 25 do begin
        Buffer[I] := Chr(I + Ord('A'));
    end;
    for i := 0 to 25 do begin
        Buffer[I + 26] := Chr(I + Ord('a'));
    end;
    GetTextExtentPoint(Canvas.Handle, Buffer, 52, TSize(Result));
    Result.X := Result.X div 52;
end;

procedure CaptureBmpScreen(Bmp : TBitMap);
//----------------------------------------------------------------------------------------------------------------------------------
var
    hdcCompatible, hdcScreen : Hdc;
    hbmScreen : HBitMap;
    OldBmp :    THandle;
begin
    hdcScreen := CreateDC('DISPLAY', nil, nil, nil);
    try
        hdcCompatible := CreateCompatibleDC(hdcScreen);
        try
            hbmScreen := CreateCompatibleBitmap(hdcScreen, GetDeviceCaps(hdcScreen, HORZRES), GetDeviceCaps(hdcScreen, VERTRES));
            if (hbmScreen = 0) then begin
                DeleteDC(hdcCompatible);
                Exit;
            end;
            OldBmp := SelectObject(hdcCompatible, hbmScreen);
            BitBlt(hdcCompatible, 0, 0, GetDeviceCaps(hdcScreen, HORZRES), GetDeviceCaps(hdcScreen, VERTRES), hdcScreen, 0, 0, SRCCOPY);
            SelectObject(hdcCompatible, OldBmp);
            {DeleteObject(Image1.Picture.BitMap.Handle);}
            DeleteObject(Bmp.Handle);
            Bmp.Handle := hbmScreen;
        finally
            DeleteDC(hdcCompatible);
        end;
    finally
        DeleteDC(hdcScreen);
    end;
end;

function MM2PixelHorizontal(DC : HDC; const mmValue : double) : integer;
    //----------------------------------------------------------------------------------------------------------------------
    //Converte valor em mm para pixels segundo o canvas dado
begin
    Result := Trunc((mmValue * GetDeviceCaps(DC, LOGPIXELSX)) / 25.4);
end;

function MM2PixelVertical(DC : HDC; const mmValue : double) : integer;
    //----------------------------------------------------------------------------------------------------------------------
    //Converte valor em mm para pixels segundo o canvas dado
begin
    Result := Trunc((mmValue * GetDeviceCaps(DC, LOGPIXELSY)) / 25.4);
end;

function PointInsideRect(const P : TPoint; const Rect : TRect) : boolean;
    //----------------------------------------------------------------------------------------------------------------------
{{
 Informa se ponto esta contido no rect( descarta fronteira )

 Ver
 function TXPGraph.Inside(const Point: TPoint; const Rect: TRect): boolean;
 }
begin
    Result := (P.x > Rect.left) and (P.x < Rect.right) and (P.y > Rect.top) and (P.y < Rect.bottom);
end;

function RectCenterInsideRect(const FixedRect, MovRect : TRect) : TRect;
    //----------------------------------------------------------------------------------------------------------------------------------
    //Centraliza um TRect dentro de outro, rescalonando se necessario
var
    RatioH, RatioW, HInt, HExt, WInt, WExt : double;
begin
    WInt := (MovRect.Right - MovRect.Left);
    HInt := (MovRect.Bottom - MovRect.Top);
    WExt := (FixedRect.Right - FixedRect.Left);
    HExt := (FixedRect.Bottom - FixedRect.Top);

    //Calcula escalonamento
    if (WInt > WExt) or (HInt > HExt) then begin
        if (WInt <> 0) and (HInt <> 0) then begin //Usa a razao para escalonamento
            RatioH := HExt / HInt;
            RatioW := WExt / WInt;
            if RatioH > RatioW then begin //Ajusta pela maior distorcao -> W
                Result := RectResCale(MovRect, RatioW, RatioW);
            end else begin //Ajusta pela maior distorcao -> H
                Result := RectResCale(MovRect, RatioH, RatioH);
            end;
        end else begin //Razao nao pode ser calculada
            Result := MovRect;
            if HInt = 0 then begin  //Ancora -> linha horizontal
                RectSetHeight(Result, 0);
                RectSetWidth(Result, Trunc(Min(WExt, WInt)));
            end else begin  //Ancora -> linha vertical
                RectSetWidth(Result, 0);
                RectSetHeight(Result, Trunc(Min(HExt, HInt)));
            end;
        end;
        Result := RectCenterOverRect(FixedRect, Result); //Desloca para centralizar o retangulo rescalonado
    end else begin
        Result := RectCenterOverRect(FixedRect, MovRect); //Desloca para centralizar normalmente
    end;
end;

function RectCenterOverRect(const FixedRect, MovRect : TRect) : TRect;
    //----------------------------------------------------------------------------------------------------------------------
    //Centraliza um TRect sobre outro
var
    Dw, Dh : integer;
begin
    Dw := MovRect.Right - MovRect.Left;
    Dh := MovRect.Bottom - MovRect.Top;
    Result.Left := FixedRect.Left + (((FixedRect.Right - FixedRect.Left) - Dw) div 2);
    Result.Top := FixedRect.Top + (((FixedRect.Bottom - FixedRect.Top) - Dh) div 2);
    Result.Right := Result.Left + Dw;
    Result.Bottom := Result.Top + Dh;
end;

function RectCenterPoint(const ARect : TRect) : TPoint;
    //----------------------------------------------------------------------------------------------------------------------------------
    //Retorna o centro de um TRect
    //Usar metodos em TXPRect
begin
    Result.Y := ARect.Top + ((ARect.Bottom - ARect.Top) div 2);
    Result.X := ARect.Left + ((ARect.Right - ARect.Left) div 2);
end;

function RectFromPoints(const P1, P2 : TPoint) : TRect;
    //----------------------------------------------------------------------------------------------------------------------
    //Gera Rect a partir de seus limites
begin
    Result.Left   := Min(P1.x, P2.x);
    Result.Top    := Min(P1.y, P2.y);
    Result.Bottom := Max(P1.y, P2.y);
    Result.Right  := Max(P1.x, P2.x);
end;

function RectGrow(const SourceRect : TRect; Delta : integer) : TRect;
    //----------------------------------------------------------------------------------------------------------------------
    //Aumenta dimensoes do retangulo preservando seu centro
begin
    Result := SourceRect;
    with Result do begin
        Dec(Left, Delta);
        Dec(Top, Delta);
        Inc(Right, Delta);
        Inc(Bottom, Delta);
    end;
end;

function RectIntersectRect(const R1, R2 : TRect) : TRect;
    //----------------------------------------------------------------------------------------------------------------------
    //Retorna o retangulo gerado pela interseccao de outros 2
begin
    with Result do begin
        Left   := Max(R1.Left, R2.Left);
        Top    := Max(R1.Top, R2.Top);
        Right  := Min(R1.Right, R2.Right);
        Bottom := Min(R1.Bottom, R2.Bottom);
    end;
    if not RectIsValid(Result) then begin
        Result := RectMake(0, 0, 0, 0);
    end;
end;

function RectIsInsideRect(const OutterRect, InnerRect : TRect) : boolean;
    //----------------------------------------------------------------------------------------------------------------------
    //Informa se OutterRect cirncunda InnerRect
begin
    Result := (InnerRect.Left >= OutterRect.Left) and (InnerRect.Top >= OutterRect.Top) and
        (InnerRect.Right <= OutterRect.Right) and (InnerRect.Bottom <= OutterRect.Bottom);
end;

function RectIsIntersection(const R1, R2 : TRect) : boolean;
    //----------------------------------------------------------------------------------------------------------------------
{{
Indica se os retangulos se interceptam

Ver

XPGraph
TXPGraph.HasIntersection
 }
begin
    Result := not RectIsNull(RectIntersectRect(R1, R2));
end;

function RectIsNull(const R : TRect) : boolean;
    //----------------------------------------------------------------------------------------------------------------------
{{
Indica se o retangulo se deformou para um ponto

Depreciada por haver discordancia com a documentacao original
Ver :

TXPGraph.RectIsNull
}
begin
    Result := (R.Left = 0) and (R.Right = 0) and (R.Top = 0) and (R.Bottom = 0);
end;

function RectIsValid(const R : TRect) : boolean;
    //----------------------------------------------------------------------------------------------------------------------
    //Indica se a geometria do retangulo eh consistente
begin
    Result := (R.Left <= R.Right) and (R.Top <= R.Bottom);
end;

function RectMake(Left, Top, Right, Bottom : integer) : TRect;
    //----------------------------------------------------------------------------------------------------------------------
    //Seta valores por metodo para retangulo
begin
    Result.Left   := Left;
    Result.Top    := Top;
    Result.Right  := Right;
    Result.Bottom := Bottom;
end;

function RectMoveRelative(const SourceRect : TRect; DeltaX, DeltaY : integer) : TRect;
    //----------------------------------------------------------------------------------------------------------------------
    //Altera a posicao de um rect pelos valroes passados
begin
    Result.Left   := SourceRect.Left + DeltaX;
    Result.Right  := SourceRect.Right + DeltaX;
    Result.Top    := SourceRect.Top + DeltaY;
    Result.Bottom := SourceRect.Bottom + DeltaY;
end;

function RectMoveTo(const SourceRect : TRect; PX, PY : integer) : TRect;
    //----------------------------------------------------------------------------------------------------------------------
    //Posiciona retangulo na posicao absoluta
begin
    Result := SourceRect;
    with Result do begin
        Right  := PX + Right - Left;
        Bottom := PY + Bottom - Top;
        Left   := PX;
        Top    := PY;
    end;
end;

function RectMoveCenterTo(const SourceRect : TRect; CX, CY : integer) : TRect;
    //----------------------------------------------------------------------------------------------------------------------------------
    //Posiciona centro do retangulo na posicao absoluta
var
    DeltaX, DeltaY : integer;
    P : TPoint;
begin
    P      := TXPRect.CenterPoint(SourceRect);
    DeltaX := P.X - CX;
    DeltaY := P.Y - CY;
    Result := RectMake(SourceRect.Left + DeltaX, SourceRect.Top + DeltaY, SourceRect.Right + DeltaX, SourceRect.Bottom + DeltaY);
end;

function RectRescale(Rect : TRect; ScaleX, ScaleY : double; FixedCenter : boolean = True) : TRect;
    //----------------------------------------------------------------------------------------------------------------------
    //Rescala rect preservando sua origem
    //Dica: para descendentes de TWinControl usar ScaleBy
var
    H, W : integer;
    C :    TPoint;
begin
    { TODO -oRoger -cLIB : Adaptar para responder ao centro fixo }
    H := Trunc((Rect.Bottom - Rect.Top) * ScaleY);
    W := Trunc((Rect.Right - Rect.Left) * ScaleX);
    if FixedCenter then begin
        H := (H div 2);
        W := (W div 2);
        C := TXPRect.CenterPoint(Rect);
        Result.Top := C.Y - H;
        Result.Left := C.X - W;
        Result.Bottom := C.Y + H;
        Result.Right := C.X + W;
    end else begin
        Result.Top    := Rect.Top;
        Result.Left   := Rect.Left;
        Result.Bottom := Rect.Top + H;
        Result.Right  := Rect.Left + W;
    end;
end;

function RectSetHeight(Rect : TRect; NewHeight : word; FixedCenter : boolean = True) : TRect;
    //----------------------------------------------------------------------------------------------------------------------------------
    //Altera a altura do rect
var
    DeltaY : integer;
begin
    DeltaY := NewHeight - (Rect.Bottom - Rect.Top);
    if FixedCenter then begin
        DeltaY := DeltaY div 2;
        Result := RectMake(Rect.Left, Rect.Top - DeltaY, Rect.Right, Rect.Bottom + DeltaY);
    end else begin
        Result := RectMake(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom + DeltaY);
    end;
end;

function RectSetWidth(Rect : TRect; NewWidth : word; FixedCenter : boolean = True) : TRect;
    //----------------------------------------------------------------------------------------------------------------------------------
    //Altera a largura do rect
var
    DeltaX : integer;
begin
    DeltaX := NewWidth - (Rect.Right - Rect.Left);
    if FixedCenter then begin
        DeltaX := DeltaX div 2;
        Result := RectMake(Rect.Left - DeltaX, Rect.Top, Rect.Right + DeltaX, Rect.Bottom);
    end else begin
        Result := RectMake(Rect.Left, Rect.Top, Rect.Right + DeltaX, Rect.Bottom);
    end;
end;

procedure Wallpaper(Canvas : TCanvas);
//----------------------------------------------------------------------------------------------------------------------
begin
    PaintDesktop(Canvas.Handle);
end;




end.
