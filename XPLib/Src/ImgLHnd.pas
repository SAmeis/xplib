{$IFDEF ImgLHnd}
	{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I XPLib.inc}

unit ImgLHnd;

interface

uses
   SysUtils, Windows, Classes, Graphics, controls;

procedure ReadFlatBmpFromImageList( ImageList : TImageList; Bmp : TBitmap );

implementation

uses
	Commctrl;


procedure ReadFlatBmpFromImageList( ImageList : TImageList; Bmp : TBitmap );
//----------------------------------------------------------------------------------------------------------------------
//Salva bmp com conteudo do imagelist em unica imagem
var
  I: Integer;
  R: TRect;
begin
	Bmp.Width:=ImageList.Width * ImageList.Count;
	Bmp.Height:=ImageList.Height;
	R:=Rect(0,0,ImageList.Width,ImageList.Height);
	Bmp.Canvas.Brush.Color:=clBtnFace;
	Bmp.Canvas.FillRect(R);
	for I := 0 to ImageList.Count-1 do begin
		ImageList_DrawEx( ImageList.Handle, I, Bmp.Canvas.Handle, I * ImageList.Width, 0, 0, 0, ImageList.BkColor,
						  ImageList.BlendColor, ILD_TRANSPARENT);
	end;
end;
                                                               
end.
 