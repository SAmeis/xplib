{$IFDEF XPGraph }
  {$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I XPLib.inc}

unit XPGraph;

{{
XPLib based unit for basic graphical manipulation.
}
interface

uses
    SysUtils, Classes, Windows, Math, Graphics, XPTypes;

type
    {{
    Representação das direções de movimentação possívéis em uma matriz de pontos.

    NOTA: Esta ordem não pode ser modificada devido a optmização de alguns algoritmos.
    
    Revision: 22/5/2006 - Roger  
    }
    TCardinalDirection  = (cdNone, cdEast, cdSouth, cdSouthEast, cdNorthEast, cdNorth, cdNorthWest,
        cdSouthWest, cdWest, cdIrregular);
    {{
    Valores possíveis par aum deslocamento plano.

    Revision: 29/5/2006 - Roger
    }
    TCardinalDirections = set of TCardinalDirection;

const
    {{
    Incrementos na coordenada X para um deslocamento unitário no vetor da direção indexada via valores de TCardinalDirection.

    Revision: 26/5/2006 - Roger
    }
    DeltaXCardinalSteps: array[cdNone..cdIrregular] of integer = (0, 1, 0, 1, 1, 0, -1, -1, -1, 0);
    {{
    Incrementos na coordenada Y para um deslocamento unitário no vetor da direção indexada via valores de TCardinalDirection.

    Revision: 26/5/2006 - Roger
    }
    DeltaYCardinalSteps: array[cdNone..cdIrregular] of integer = (0, 0, 1, 1, -1, -1, -1, 1, 0, 0);


type
    TPixelIterator = class(TObject)
 {{
 Realiza a iteração "corrida" pelos pixels de um bitmap de forma optmizada.
 
 Seu implica em fazer uma sub-classe que sobre-escreva ProcessRGBPixel e chamar o método Run(), para que o mesmo seja chamado.
 A modelagem foi feita desta forma para evitar o uso de ponteiros para metodos( eventos ) que sobrecarregam com testes para
 leitura/escrita e chamadas a callbacks.
 
 Desta forma pode-se ter acesso ao estado completo da instancia e alterar o que for necessario.
 
 IMPORTANTE:
 Esta classe ainda não está finalizada, pois trabalha nativamente apenas para bitmaps de 24 bits de cor. Necessita implementação
 eficiente para trabalhar nativamente com outras profundidades de cor.
 }
    private
        FBmp :     TBitMap;
        FCollumn : integer;
        FLine :    integer;
        FPixelFormat : TPixelFormat;
        FPRGB :    PRGBQuad;
        FRow15Bits : PWordArray;
        FRow16Bits : PWordArray;
        FRow24Bits : PXPRGBTripleArray;
    protected
        procedure Finalize;
        procedure Initialize;
        procedure Run15pfBits;
        procedure Run16pfBits;
        procedure Run24pfBitsReadOnly;
    public
        constructor Create(ABmp : TBitmap); virtual;
        destructor Destroy; override;
        procedure ProcessRGBPixel; virtual; abstract;
        procedure Run;
        procedure Stop;
        procedure WritePixel(RGB : TRGBTriple);
        property Collumn : integer read FCollumn;
  {{
  Coluna corrente da corrida.
  }
        property Line : integer read FLine;
  {{
  Linha corrente da corrida.
  }
        property PRGB : PRGBQuad read FPRGB;
  {{
  Valor RGB indicado por [ Self.Line, Self.Collumn ].
  }
    end;

    TShapePlane = class;

    TXPShape = class(TObject)
 {{
 Classe base para os elementos graficos mais simples como pontos retângulos quadrados etc.
 }
    private
        FPoint :      TPoint;
        FShapePlane : TShapePlane;
        procedure SetShapePlane(const Value : TShapePlane);
    public
        constructor Create(APlane : TShapePlane; const AOrigin : TPoint);
        procedure BeforeDestruction; override;
        property Origin : TPoint read FPoint;
        property ShapePlane : TShapePlane read FShapePlane write SetShapePlane;
    end;

    TShapePlane = class(TObject)
 {{
 Plano que contem os elementos graficos baseados em TXPShape.
 }
    private
        FAngle :     double;
        FOrigin :    TPoint;
        FShapeList : TList;
        function GetShapes(index : integer) : TXPShape;
    public
        constructor Create(const AOrigin : TPoint; AAngle : double);
        destructor Destroy; override;
        property Angle : double read FAngle;
        property Origin : TPoint read FOrigin;
        property Shapes[index : integer] : TXPShape read GetShapes;
    end;

    TXPGraph = class(TObject)
    {{
    Classe com vários métodos para manipulação de elementos gráficos simples.

    NOTAS: TODAS as operações são baseadas nas coordenadas de tela padrão.
    
    }
    public
        class function AngularCoefficient(const BasePoint, TargetPoint : TPoint) : extended;
        class function Distance(const P1, P2 : TPoint) : integer;
        class function HasIntersection(const R1, R2 : TRect) : boolean;
        class function Inside(const Point : TPoint; const Rect : TRect) : boolean;
        class function PixelFormatEx(ABitmap : Graphics.TBitmap) : TPixelFormat;
        class function RectIsNull(const R : TRect) : boolean;
        class function PointToDirection(const InitialPoint, FinalPoint : TPoint) : TCardinalDirection;
    end;

    TXPRect = class
    {{
    Operações com Retângulos.

    NOTAS:
    TODAS as operações são baseadas nas coordenadas de tela padrão.

    Revision: 6/6/2006 - Roger
    }
    public
        class function CenterPoint(const ARect : TRect) : TPoint;
    end;

implementation

uses
    MathHnd, APIHnd;

{ TXPGraph }

{-**********************************************************************
************************************************************************
******************
******************  Class:    TXPGraph
******************  Category: No category
******************
************************************************************************
************************************************************************}
{--------------------------------------------------------------------------------------------------------------------------------}
class function TXPGraph.AngularCoefficient(const BasePoint, TargetPoint : TPoint) : extended;
{{
Calcula o angulo formado entre dois pontos e o eixo horizontal, seguindo a direcao do ponto base para o ponto alvo. Este angulo e
o visto pelo observador da tela.


NOTA: O sinal do eixo Y é invertido devido a orientacao da tela "aumentar" para baixo.
Exemplos:
f((0,1),(0,2)) = Pi
f((0,2),(0,1)) = -Pi
f((1,1),(2,2)) = -Pi/4
}
var
    Delta : longint;
begin
    Delta := (TargetPoint.X - BasePoint.X);
    if (Delta <> 0) then begin
        // Delta sempre sera um valor afastado de Zero numerico assim o teste (Abs(Delta) > XPEpsilon) nao precisa ser feito
        Result := Math.ArcTan2((BasePoint.Y - TargetPoint.Y), Delta);
        //NOTA: Sinal invertido devido a orientacao da tela "aumentar" para baixo
    end else begin
        if (BasePoint.Y > TargetPoint.Y) then begin    //Sentido do eixo Y indica o angulo agora
            Result := (Pi / 2);
        end else begin
            Result := -(Pi / 2);
        end;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
class function TXPGraph.Distance(const P1, P2 : TPoint) : integer;
{{
Calcula a distance em pontos entre P1 e P2, usando o velho pitagoras

Revision: 5/7/2005
}
begin
    Result := Round(Sqrt(Sqr(P1.X - P2.X) + Sqr(P1.Y - P2.Y)));
end;

class function TXPGraph.HasIntersection(const R1, R2 : TRect) : boolean;
{{
Indica se existe interseccao entre os retangulos passados

returns : true se existir interseccao
}
var
    RTop, LBottom : TPoint;
begin
    Result := TXPGraph.Inside(R1.TopLeft, R2) or TXPGraph.Inside(R1.BottomRight, R2);
    if (not Result) then begin    //Tenta extremos restantes
        RTop.X    := R1.Right;
        RTop.Y    := R1.Top;
        LBottom.X := R1.Left;
        LBottom.Y := R1.Bottom;
        Result    := TXPGraph.Inside(RTop, R2) or TXPGraph.Inside(LBottom, R2);
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
class function TXPGraph.Inside(const Point : TPoint; const Rect : TRect) : boolean;
{{
Indica se o ponto pertence ao interior do retangulo passado

returns : True se ponto no interior do retangulo
}
begin
    Result := (Point.X > Rect.left) and (Point.X < Rect.right) and (Point.Y > Rect.top) and (Point.Y < Rect.bottom);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
class function TXPGraph.PixelFormatEx(ABitmap : Graphics.TBitmap) : TPixelFormat;
{{
Analisa a instancia do bitmap passado para complementar algumas omissões da VCL
}
var
    DIB : TDIBSection;
    Ret : integer;
begin
    Result := ABitmap.PixelFormat;
    if Result = pfCustom then begin
        Ret := GetObject(ABitmap.Handle, SizeOf(DIB), @DIB);
        if (Ret = 0) then begin
            APIHnd.CheckAPI(GetLastError());
        end;
        with DIB, dsbmih do begin
            if biBitCount = 16 then begin
                if biCompression = BI_BITFIELDS then begin
                    if dsBitFields[1] = $3E0 then begin
                        Result := pf15bit;
                    end;
                end;
            end;
        end;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
class function TXPGraph.PointToDirection(const InitialPoint, FinalPoint : TPoint) : TCardinalDirection;
{{
Calcula a direcao de deslocamento necessaria para se realizar tal deslocamento. Caso este deslocamento não possa ser realizado
em nenhum dos valores válidos cdIrregular será retornado.

Nota: O sistema de coordenadas usado é o padrão de tela.

Revision: 1/6/2006 - Roger
}
var
    DeltaX, DeltaY : integer;
begin
    Result := cdIrregular;
    DeltaX := FinalPoint.X - InitialPoint.X;
    DeltaY := FinalPoint.Y - InitialPoint.Y;
    case DeltaX of
        0 : begin
            //DeltaX = 0 -> cima ou abaixo
            case (DeltaY) of
                0 : begin
                    Result := cdNone;
                end;
                1..MaxInt : begin
                    Result := cdSouth;
                end;
                -MaxInt.. -1 : begin
                    Result := cdNorth;
                end;
            end;
        end;
        1..MaxInt : begin
            //DeltaX positivo -> lado direito
            case (DeltaY) of
                0 : begin
                    Result := cdEast;
                end;
                1..MaxInt : begin
                    if (Abs(DeltaX) = Abs(DeltaY)) then begin
                        Result := cdSouthEast;
                    end else begin
                        Result := cdIrregular;
                    end;
                end;
                -MaxInt.. -1 : begin
                    if (Abs(DeltaX) = Abs(DeltaY)) then begin
                        Result := cdNorthEast;
                    end else begin
                        Result := cdIrregular;
                    end;
                end;
            end;
        end;
        -MaxInt.. -1 : begin
            //DeltaX negativo -> lado esquerdo
            case (DeltaY) of
                0 : begin
                    Result := cdWest;
                end;
                1..MaxInt : begin
                    if (Abs(DeltaX) = Abs(DeltaY)) then begin
                        Result := cdNorthWest;
                    end else begin
                        Result := cdIrregular;
                    end;
                end;
                -MaxInt.. -1 : begin
                    if (Abs(DeltaX) = Abs(DeltaY)) then begin
                        Result := cdSouthWest;
                    end else begin
                        Result := cdIrregular;
                    end;
                end;
            end;
        end;
    end;
end;

class function TXPGraph.RectIsNull(const R : TRect) : boolean;

    //----------------------------------------------------------------------------------------------------------------------
 {{
 Indica se o retangulo se deformou para um ponto
 }

begin
    Result := (R.Left = R.Right) and (R.Top = R.Bottom);
end;


{ TPixelIterator }

{-**********************************************************************
************************************************************************
******************
******************  Class:    TPixelIterator
******************  Category: No category
******************
************************************************************************
************************************************************************}
{--------------------------------------------------------------------------------------------------------------------------------}
procedure TPixelIterator.Finalize;
{{
Garante que o estado do objeto serah dado como fora da "corrida" e uma nova pode ser iniciada sem falha.
}
begin
    Self.FCollumn   := 0;
    Self.FLine      := 0;
    Self.FRow16Bits := nil;
    Self.FRow15Bits := nil;
    Self.FRow24Bits := nil;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TPixelIterator.Initialize;
{{
prepara estado do objeto para iniciar uma "corrida" pelo pixels
}
begin
    Self.FCollumn := 0;
    Self.FLine    := 0;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TPixelIterator.Run15pfBits;
{{
Corrida para bmp de 15bpp

IMPORTANTE:
Rotina ainda em desenvolvimento.
}

const
    //5 bits ligados no final a serem rotacionados de 5 para cada cor e o primeiro descartado
    R15BITMASK: word = $7C00; //0111110000000000
    G15BITMASK: word = $3E0; //0000001111100000
    B15BITMASK: word = $1F; //0000000000011111
var
    RGB : word;
    LineLimit, ColLimit : integer;

begin
    LineLimit  := Self.FBmp.Height - 1;
    ColLimit   := Self.FBmp.Width - 1;
    Self.FLine := 0;
    while (Self.FLine <= LineLimit) do begin
        Self.FRow15Bits := Self.FBmp.Scanline[Self.FLine];
        Self.FCollumn   := 0;
        while (Self.FCollumn <= ColLimit) do begin
            RGB := Self.FRow15Bits[Self.FCollumn];
            Self.FPRGB^.rgbBlue := RGB and B15BITMASK;
            Self.FPRGB^.rgbGreen := RGB and G15BITMASK;
            Self.FPRGB^.rgbRed := RGB and R15BITMASK;
            Self.ProcessRGBPixel();
            Inc(Self.FCollumn);
        end;
        Inc(Self.FLine);
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TPixelIterator.Run16pfBits;
{{
Corrida para bmp de 16bpp

IMPORTANTE:
Rotina ainda em desenvolvimento.
}

const
    //5 bits para red, 6 para green e 5 para blue na ordem abaixo
    R15BITMASK: word = $F800;//1111100000000000
    G15BITMASK: word = $7E0; //0000011111100000 *** Cor verde com 1 bit a mais( humanos veem melhor o verde )
    B15BITMASK: word = $1F;  //0000000000011111
var
    RGB : word;
    LineLimit, ColLimit : integer;

begin
    LineLimit  := Self.FBmp.Height - 1;
    ColLimit   := Self.FBmp.Width - 1;
    Self.FLine := 0;
    while (Self.FLine <= LineLimit) do begin
        Self.FRow16Bits := Self.FBmp.Scanline[Self.FLine];
        Self.FCollumn   := 0;
        while (Self.FCollumn <= ColLimit) do begin
            RGB := Self.FRow16Bits[Self.FCollumn];
            Self.FPRGB^.rgbBlue := RGB and B15BITMASK;
            Self.FPRGB^.rgbGreen := RGB and G15BITMASK;
            Self.FPRGB^.rgbRed := RGB and R15BITMASK;
            Self.ProcessRGBPixel();
            Inc(Self.FCollumn);
        end;
        Inc(Self.FLine);
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TPixelIterator.Run24pfBitsReadOnly;
{{
Usa Bmp.ScanLine para pegar um vetor com TRGBTriple( 24 bits ), e chama ProcessRGBPixel para cada um deles.
}
var
    RGB : TRGBTriple;
    LineLimit, ColLimit : integer;
begin
    LineLimit  := Self.FBmp.Height - 1;
    ColLimit   := Self.FBmp.Width - 1;
    Self.FLine := 0;
    while (Self.FLine <= LineLimit) do begin
        Self.FRow24Bits := Self.FBmp.Scanline[Self.FLine];
        Self.FCollumn   := 0;
        while (Self.FCollumn <= ColLimit) do begin
            RGB := Self.FRow24Bits[Self.FCollumn];
            Self.FPRGB^.rgbBlue := RGB.rgbtBlue;
            Self.FPRGB^.rgbGreen := RGB.rgbtGreen;
            Self.FPRGB^.rgbRed := RGB.rgbtRed;
            Self.ProcessRGBPixel();
            Inc(Self.FCollumn);
        end;
        Inc(Self.FLine);
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
constructor TPixelIterator.Create(ABmp : TBitmap);
{{
Usara o bmp passado para realizar iteracoes com seus pixels.

IMPORTANTE:
Pelo momento apenas bmps de 24 bits podem ser processados com seguranca. Assim eles serão convertidos para essa profundidade de cor
antes de prosseguir.
}
begin
    Self.FBmp := ABmp;
    Self.FPixelFormat := TXPGraph.PixelFormatEx(ABmp);
    if (TXPGraph.PixelFormatEx(Self.FBmp) <> pf24bit) then begin
        Self.FBmp.PixelFormat := pf24bit;
        Self.FPixelFormat     := TXPGraph.PixelFormatEx(Self.FBmp);
    end;
    New(Self.FPRGB);
    Self.Initialize;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
destructor TPixelIterator.Destroy;
{{
Libera espaço alocado
}
begin
    Dispose(Self.FPRGB);
    inherited;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
{{
ProcessRGBPixel é o método chave a ser sobre-escrito.
O funcionamento da classe foi modelado desta forma para aumentar a performance evitando o uso de eventos e sucessivos testes disso
e daquilo.

Dentro deste método podemos cancelar a iteração chamando Self.Stop(), alterar o valor de um pixel chamando Self.WritePixel(), tudo
isso baseado no valor de Self.PRGB que trata-se de um ponteiro para o valor corrente em RGB.

Rev. 6/5/2005
}
{--------------------------------------------------------------------------------------------------------------------------------}
procedure TPixelIterator.Run;
{{
Para cada Pixel do bmp chama os eventos atribuidos. Caso deseje-se parar a corrida chama-se Stop().

IMPORTANTE : Devido a falta de informacoes o Bmp DEVE ter 24 bits de cor. Assim o mesmo será convertido para isso antes do
proacessamento
}
begin
    Self.Initialize;
    try
        try
            case Self.FPixelFormat of
                pf15bit : begin
                    Self.Run15pfBits;
                end;
                pf16bit : begin
                    Self.Run16pfBits;
                end;
                pf24bit : begin
                    Self.Run24pfBitsReadOnly();
                end;
                else begin
                    raise Exception.Create('Profundidade e/ou compressão de cores não suportada.');
                end;
            end;
        except
            on E : EAbort do begin
                //Nada a fazer para esse caso
            end else begin
                raise; //relanca erro para qq outro caso
            end;
        end;
    finally
        Self.Finalize;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TPixelIterator.Stop;
{{
Pode ser chamado de dentro de ProcessRGBPixel
Chama Abort() para suspender a corrida. Caso desejado Abort() pode ser chamado no evento.
}
begin
    SysUtils.Abort();
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TPixelIterator.WritePixel(RGB : TRGBTriple);
{{
Usa o pixelformat corrente para escrever no ponteiro correto capturado por ScanLine();
Lembrar que esta rotina so pode ser chamada de dentro de uma chamada a ProcessRGBPixel()

IMPORTANTE:
Metodo seguro apenas para 24 bits. Falta finalizar esta classe.
}
begin
    case Self.FPixelFormat of
        pf15Bit : begin
            if (Self.FRow15Bits <> nil) then begin
                Self.FRow15Bits[Self.FCollumn] := RGB.rgbtBlue + RGB.rgbtGreen + RGB.rgbtRed;
            end;
        end;
        pf16Bit : begin
            if (Self.FRow16Bits <> nil) then begin
                Self.FRow16Bits[Self.FCollumn] := RGB.rgbtBlue + RGB.rgbtGreen + RGB.rgbtRed;
            end;
        end;
        pf24bit : begin
            if (Self.FRow24Bits <> nil) then begin
                Self.FRow24Bits[Self.FCollumn] := RGB;
            end;
        end;
        else begin
            raise Exception.Create('Profundidade de cor não suportada.');
        end;
    end;
end;

{-**********************************************************************
************************************************************************
******************
******************  Class:    TShapePlane
******************  Category: No category
******************
************************************************************************
************************************************************************}
{--------------------------------------------------------------------------------------------------------------------------------}
function TShapePlane.GetShapes(index : integer) : TXPShape;
{{
Retorna a figura indicada pelo index.

Revision:2/6/2005
}
begin
    Result := TXPShape(Self.FShapeList.Items[index]);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
constructor TShapePlane.Create(const AOrigin : TPoint; AAngle : double);
{{
AOrigin - Ponto de origem do plano.

AAngle - Angulo de rotacao do plano ao plano absoluto, geralmente a tela/monitor.


Revision:2/6/2005
}
begin
    Self.FOrigin    := AOrigin;
    Self.FAngle     := AAngle;
    Self.FShapeList := TList.Create;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
destructor TShapePlane.Destroy;
{{
Destroi-se e a TODAS as figuras que ele contem.


Revision:2/6/2005
}
var
    i : integer;
begin
    //Destroi os elementos contidos
    for i := 0 to Self.FShapeList.Count - 1 do begin
        TXPShape(Self.FShapeList.Items[i]).Free;
    end;
    Self.FShapeList.Free;
    inherited;
end;

{-**********************************************************************
************************************************************************
******************
******************  Class:    TXPShape
******************  Category: No category
******************
************************************************************************
************************************************************************}
{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPShape.SetShapePlane(const Value : TShapePlane);
{{
Altera o plano pai da figura.

Revision:2/6/2005
}
begin
    if (Self.FShapePlane <> Value) then begin
        Self.FShapePlane.FShapeList.Extract(Self);
        Self.FShapePlane := Value;
        Value.FShapeList.Add(Self);
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPShape.BeforeDestruction;
{{
Remover-se do plano a que pertence

Revision:2/6/2005
}
begin
    inherited;
    if (Self.FShapePlane <> nil) then begin
        Self.FShapePlane.FShapeList.Extract(Self);
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
constructor TXPShape.Create(APlane : TShapePlane; const AOrigin : TPoint);
{{
APlane : Plano ao que esta figura pertence.

Ponto de origem desta figura em relacao a origem do plano.

Revision:2/6/2005
}
begin
    inherited Create;
    Self.FPoint      := AOrigin;
    Self.FShapePlane := APlane;
    APlane.FShapeList.Add(Self);
end;

class function TXPRect.CenterPoint(const ARect : TRect) : TPoint;
{{
Retorna o centro de um TRect

Revision: 6/6/2006 - Roger
}
begin
    Result.Y := ARect.Top  + ((ARect.Bottom - ARect.Top) div 2);
    Result.X := ARect.Left + ((ARect.Right  - ARect.Left) div 2);
end;

end.
