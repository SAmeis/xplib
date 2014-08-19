{$IFDEF ColorHnd}
	{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I XPLib.inc}

unit ColorHnd;


{  Usar as rotinas abaixo de Classes.pas
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
  DESCRIPTIVE_COLORS_BR : array[0..41] of TIdentMapEntry = (
    (Value: clBlack; Name: 'preto'),
    (Value: clMaroon; Name: 'marrom'),
    (Value: clGreen; Name: 'verde'),
    (Value: clOlive; Name: 'oliva'),
    (Value: clNavy; Name: 'marinho'),
    (Value: clPurple; Name: 'purpura'),
    (Value: clTeal; Name: 'musgo'),
    (Value: clGray; Name: 'cinza'),
    (Value: clSilver; Name: 'prateado'),
    (Value: clRed; Name: 'vermelho'),
    (Value: clLime; Name: 'limAo'),
    (Value: clYellow; Name: 'amarelo'),
    (Value: clBlue; Name: 'azul'),
    (Value: clFuchsia; Name: 'lilas'),
    (Value: clAqua; Name: 'piscina'),
    (Value: clWhite; Name: 'branco'),
    (Value: clScrollBar; Name: 'BarraRolagemWindows'),
    (Value: clBackground; Name: 'BackgroundWindows'),
    (Value: clActiveCaption; Name: 'TituloAtivoWindows'),
    (Value: clInactiveCaption; Name: 'TituloInativoWindows'),
    (Value: clMenu; Name: 'MenuWindows'),
    (Value: clWindow; Name: 'JanelaWindows'),
    (Value: clWindowFrame; Name: 'FrameWindows'),
	 (Value: clMenuText; Name: 'TextoMenuWindows'),
    (Value: clWindowText; Name: 'TextoJanelaWindows'),
    (Value: clCaptionText; Name: 'TextoRotuloWindow'),
    (Value: clActiveBorder; Name: 'BordaAtivaWindows'),
    (Value: clInactiveBorder; Name: 'BordaInativaWindows'),
    (Value: clAppWorkSpace; Name: 'AreaAplicativoWindows'),
    (Value: clHighlight; Name: 'HighlightWindows'),
    (Value: clHighlightText; Name: 'TextoHighlightWindows'),
    (Value: clBtnFace; Name: 'FaceBotaoWindows'),
    (Value: clBtnShadow; Name: 'SombraBotaoWindows'),
    (Value: clGrayText; Name: 'TextoCinzaWindows'),
    (Value: clBtnText; Name: 'TextBotaoWindows'),
    (Value: clInactiveCaptionText; Name: 'TextoRotuloInativoWindows'),
    (Value: clBtnHighlight; Name: 'BotaoHighlightWindows'),
    (Value: cl3DDkShadow; Name: '3DSombraEscuraWindows'),
    (Value: cl3DLight; Name: '3DLightWindows'),
    (Value: clInfoText; Name: 'TextInformativoWindows'),
    (Value: clInfoBk; Name: 'FundoInfoWindows'),
    (Value: clNone; Name: 'nenhuma'));

  DESCRIPTIVE_COLORS_RESTRICT_BR : array[0..16] of TIdentMapEntry = (
    (Value: clBlack; Name: 'preto'),
    (Value: clMaroon; Name: 'marrom'),
    (Value: clGreen; Name: 'verde'),
    (Value: clOlive; Name: 'oliva'),
    (Value: clNavy; Name: 'marinho'),
    (Value: clPurple; Name: 'purpura'),
    (Value: clTeal; Name: 'musgo'),
    (Value: clGray; Name: 'cinza'),
    (Value: clSilver; Name: 'prateado'),
    (Value: clRed; Name: 'vermelho'),
    (Value: clLime; Name: 'limao'),
    (Value: clYellow; Name: 'amarelo'),
    (Value: clBlue; Name: 'azul'),
    (Value: clFuchsia; Name: 'lilas'),
    (Value: clAqua; Name: 'piscina'),
    (Value: clWhite; Name: 'branco'),
	 (Value: clNone; Name: 'nenhuma'));


	 //Gera um padrao de cor em constraste com o originalmente fornecido;
	 function GetConstrastColor( OriginalColor : TColor ) : TColor;
	 //Gera padrao de cor na reta para o tom azul
	 function NormalizeBlue( Delta, DeltaValue : integer ) : byte;
	 //Gera padrao de cor na reta para o tom verde
	 function NormalizeGreen( Delta, DeltaValue : integer ) : byte;
	 //Gera padrao de cor na reta para o tom vermelha
	 function NormalizeRed( Delta, DeltaValue : integer ) : byte;
	 //Gera padrao de cor na reta para o espectro visivel Azul>>Vermelho
	 function NormalizeSpectrum( Delta, DeltaValue : integer ) : TColor;



implementation

uses
	Windows;

	
function GetConstrastColor( OriginalColor : TColor ) : TColor;
//----------------------------------------------------------------------------------------------------------------------
begin
	Result:=OriginalColor xor $00FFFFFF;
end;


function NormalizeBlue( Delta, DeltaValue : integer ) : byte;
//----------------------------------------------------------------------------------------------------------------------
var
	Ret : Double;
begin
	if Delta > 0 then begin
		Ret:=255*( ( Delta - DeltaValue)/Delta );
	end else begin
		Ret:=255;
	end;
	Result:=Trunc( Ret )
end;


function NormalizeGreen( Delta, DeltaValue : integer ) : byte;
//----------------------------------------------------------------------------------------------------------------------
var
	Half : integer;
	Ret : Double;
begin
	Half:=Delta Div 2;
	if DeltaValue <= ( Half ) then begin //Curva ascendente
		Ret:=(500/Delta)*DeltaValue;
	end else begin
		Ret:=500*((1-(DeltaValue/Delta)));
	end;
	Result:=Trunc(Ret)
end;

function NormalizeRed( Delta, DeltaValue : integer ) : byte;
//----------------------------------------------------------------------------------------------------------------------
var
	Ret : Double;
begin
	if Delta > 0 then begin
		Ret:=255*( DeltaValue/Delta );
	end else begin
		Ret:=0;
	end;
	Result:=Trunc( Ret );
end;

function NormalizeSpectrum( Delta, DeltaValue : integer ) : TColor;
//----------------------------------------------------------------------------------------------------------------------
begin
	Result:=TColor(
					RGB( NormalizeRed( Delta, DeltaValue ),
						 NormalizeGreen( Delta, DeltaValue ),
						 NormalizeBlue( Delta, DeltaValue ) )
					);
end;


end.
