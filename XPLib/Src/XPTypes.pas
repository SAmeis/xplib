{$IFDEF XPTypes}
  {$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I XPLib.inc}

unit XPTypes;

{{
Unit que declara varios tipos utilitarios, esta unit complementa as units System, SysUtils, Classes e Types. Caso algum tipo de
interesse esteja presente nestas units ele tem a preferência de uso

PLongWordArray = ^TLongWordArray;
TLongWordArray = array[0..8192] of LongWord; //Totaliza 32KB

PXPRGBTripleArray = ^TRGBTripleArray;
TXPRGBTripleArray = array[0..XPPixelMaxCount- 1] of TRGBTriple;
}

interface


uses
	Windows;

const
	XPPixelMaxCount = 32768;

type
	{$IFNDEF UNICODE}
	//Tentativa de compatibilidade entre cmpiladores
	 UnicodeString = WideString;
	{$ENDIF}



	PLongWordArray = ^TLongWordArray;
	TLongWordArray = array[0..8192] of LongWord; //Totaliza 32KB

	PXPRGBTripleArray = ^TXPRGBTripleArray;
  {{
    Totaliza quase 100K, valor muito alto para uso com a pilha. Este tipo deve ser preferencialmente usado no heap.

    TXPRGBTripleArray e´ usado principalmente para acesso a bitmaps de 24 bits, ou informacoes triplas RGB.
  }
	TXPRGBTripleArray = array[0..XPPixelMaxCount - 1] of TRGBTriple;


	TPoint2D = record
 {{
 Record to store a point 2D at decimal precision
 }
		X: Double;
		{1 X Value }
		Y: Double;
		{1 Y Value }
	end;

	TPoint2DDynArray = array of TPoint2D;


	TPoint3D = record
 {{
 Record to store a point 3D at decimal precision
 }
		X: Double;
		{1 X Value }
		Y: Double;
		{1 Y Value }
		Z: Double;
		{1 Z Value }
	end;

	TPoint3DDynArray = array of TPoint3D;

implementation

end.


