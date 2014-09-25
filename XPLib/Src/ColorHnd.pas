{$IFDEF ColorHnd}
{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I XPLib.inc}
unit ColorHnd;

{ Usar as rotinas abaixo de Classes.pas
  procedure RegisterIntegerConsts(IntegerType: Pointer; IdentToInt: TIdentToInt; IntToIdent: TIntToIdent);
  function IdentToInt(const Ident: string; var Int: Longint; const Map: array of TIdentMapEntry): Boolean;
  function IntToIdent(Int: Longint; var Ident: string; const Map: array of TIdentMapEntry): Boolean;
  function FindIntToIdent(AIntegerType: Pointer): TIntToIdent;
  function FindIdentToInt(AIntegerType: Pointer): TIdentToInt;
  para manipular constantes deste tipo
}

interface

uses
  Classes, Graphics;

const
  DESCRIPTIVE_COLORS_BR: array [0 .. 41] of TIdentMapEntry = ((Value: clBlack; name: 'preto'), (Value: clMaroon; name: 'marrom'),
	  (Value: clGreen; name: 'verde'), (Value: clOlive; name: 'oliva'), (Value: clNavy; name: 'marinho'), (Value: clPurple;
	  name: 'purpura'), (Value: clTeal; name: 'musgo'), (Value: clGray; name: 'cinza'), (Value: clSilver; name: 'prateado'),
	  (Value: clRed; name: 'vermelho'), (Value: clLime; name: 'limAo'), (Value: clYellow; name: 'amarelo'), (Value: clBlue;
	  name: 'azul'), (Value: clFuchsia; name: 'lilas'), (Value: clAqua; name: 'piscina'), (Value: clWhite; name: 'branco'),
	  (Value: clScrollBar; name: 'BarraRolagemWindows'), (Value: clBackground; name: 'BackgroundWindows'), (Value: clActiveCaption;
	  name: 'TituloAtivoWindows'), (Value: clInactiveCaption; name: 'TituloInativoWindows'), (Value: clMenu; name: 'MenuWindows'),
	  (Value: clWindow; name: 'JanelaWindows'), (Value: clWindowFrame; name: 'FrameWindows'), (Value: clMenuText;
	  name: 'TextoMenuWindows'), (Value: clWindowText; name: 'TextoJanelaWindows'), (Value: clCaptionText;
	  name: 'TextoRotuloWindow'), (Value: clActiveBorder; name: 'BordaAtivaWindows'), (Value: clInactiveBorder;
	  name: 'BordaInativaWindows'), (Value: clAppWorkSpace; name: 'AreaAplicativoWindows'), (Value: clHighlight;
	  name: 'HighlightWindows'), (Value: clHighlightText; name: 'TextoHighlightWindows'), (Value: clBtnFace;
	  name: 'FaceBotaoWindows'), (Value: clBtnShadow; name: 'SombraBotaoWindows'), (Value: clGrayText; name: 'TextoCinzaWindows'),
	  (Value: clBtnText; name: 'TextBotaoWindows'), (Value: clInactiveCaptionText; name: 'TextoRotuloInativoWindows'),
	  (Value: clBtnHighlight; name: 'BotaoHighlightWindows'), (Value: cl3DDkShadow; name: '3DSombraEscuraWindows'),
	  (Value: cl3DLight; name: '3DLightWindows'), (Value: clInfoText; name: 'TextInformativoWindows'), (Value: clInfoBk;
	  name: 'FundoInfoWindows'), (Value: clNone; name: 'nenhuma'));

  DESCRIPTIVE_COLORS_RESTRICT_BR: array [0 .. 16] of TIdentMapEntry = ((Value: clBlack; name: 'preto'), (Value: clMaroon;
	  name: 'marrom'), (Value: clGreen; name: 'verde'), (Value: clOlive; name: 'oliva'), (Value: clNavy; name: 'marinho'),
	  (Value: clPurple; name: 'purpura'), (Value: clTeal; name: 'musgo'), (Value: clGray; name: 'cinza'), (Value: clSilver;
	  name: 'prateado'), (Value: clRed; name: 'vermelho'), (Value: clLime; name: 'limao'), (Value: clYellow; name: 'amarelo'),
	  (Value: clBlue; name: 'azul'), (Value: clFuchsia; name: 'lilas'), (Value: clAqua; name: 'piscina'), (Value: clWhite;
	  name: 'branco'), (Value: clNone; name: 'nenhuma'));

// Gera um padrao de cor em constraste com o originalmente fornecido;
function GetConstrastColor(OriginalColor: TColor): TColor;
// Gera padrao de cor na reta para o tom azul
function NormalizeBlue(Delta, DeltaValue: integer): byte;
// Gera padrao de cor na reta para o tom verde
function NormalizeGreen(Delta, DeltaValue: integer): byte;
// Gera padrao de cor na reta para o tom vermelha
function NormalizeRed(Delta, DeltaValue: integer): byte;
// Gera padrao de cor na reta para o espectro visivel Azul>>Vermelho
function NormalizeSpectrum(Delta, DeltaValue: integer): TColor;

implementation

uses
  Windows;

function GetConstrastColor(OriginalColor: TColor): TColor;
// ----------------------------------------------------------------------------------------------------------------------
begin
  Result := OriginalColor xor $00FFFFFF;
end;

function NormalizeBlue(Delta, DeltaValue: integer): byte;
// ----------------------------------------------------------------------------------------------------------------------
var
  Ret: Double;
begin
  if Delta > 0 then begin
	Ret := 255 * ((Delta - DeltaValue) / Delta);
  end else begin
	Ret := 255;
  end;
  Result := Trunc(Ret)
end;

function NormalizeGreen(Delta, DeltaValue: integer): byte;
// ----------------------------------------------------------------------------------------------------------------------
var
  Half: integer;
  Ret: Double;
begin
  Half := Delta div 2;
  if DeltaValue <= (Half) then begin // Curva ascendente
	Ret := (500 / Delta) * DeltaValue;
  end else begin
	Ret := 500 * ((1 - (DeltaValue / Delta)));
  end;
  Result := Trunc(Ret)
end;

function NormalizeRed(Delta, DeltaValue: integer): byte;
// ----------------------------------------------------------------------------------------------------------------------
var
  Ret: Double;
begin
  if Delta > 0 then begin
	Ret := 255 * (DeltaValue / Delta);
  end else begin
	Ret := 0;
  end;
  Result := Trunc(Ret);
end;

function NormalizeSpectrum(Delta, DeltaValue: integer): TColor;
// ----------------------------------------------------------------------------------------------------------------------
begin
  Result := TColor(RGB(NormalizeRed(Delta, DeltaValue), NormalizeGreen(Delta, DeltaValue), NormalizeBlue(Delta, DeltaValue)));
end;

end.
