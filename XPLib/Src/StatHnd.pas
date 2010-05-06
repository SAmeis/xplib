{$IFDEF StatHnd}
    {$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I XPLib.inc}


unit StatHnd;

interface

uses
    SysUtils, Forms, Classes;

function CombinationListInc(IndexList : TList; Base : integer; CarryOn : integer = 0) : boolean;
function CombinationPosition(InputList, OutPutList : TList; Elements : integer; Position : longint) : boolean;
function FactLn(N : Integer) : Single;
function GammaLn(X : Extended) : Extended;
procedure Initialize;
function Combinations(Number, NumberChosen : Integer) : Extended;

var
    FactorialTablesInitialized : boolean = FALSE;

implementation


const
    MFactA = 33;
    MFactLnA = 65;
    sqrt2pi = 2.5066282746310005; //sqrt(2*pi)

var
    FactLnATblLookup : array[2..MFactLna] of Extended; {lookup table of FactLn values}
    FactATblLookup : array[2..MFactA] of Extended; {lookup table of factorial values}

function CombinationListInc(IndexList : TList; Base : integer; CarryOn : integer = 0) : boolean;
    //---------------------------------------------------------------------------------------------------------------------
    //Esta rotina reolve o processo de desdobramento numerico, onde usamos uma lista montada com os indices da lista original
    // Assim [ 3, 5, 7, 9, 12 ] sao 3º, 5º, .. e decimo segundo elementos.
    //Base eh o valor do espaco de possibilidades da lista original ( InputList.Count ).
    //O primeiro valor do desdrobamento seria [ 1, 2, 3, 4(, ...)] e o segundo seria [ 1, 2, 3, 5(, ...)]
var
    Top, Val : integer;
begin
    if CarryOn > (IndexList.Count - 1) then begin
        Result := TRUE;
    end else begin
        Result := FALSE;
        Top := Base - 1;
        Val := Integer(IndexList.Items[CarryOn]);
        if Val >= Top then begin
            Inc(CarryOn);
            if CombinationListInc(IndexList, Top, CarryOn) then begin
                Result := TRUE;
                Exit;
            end;
            Val := Integer(IndexList.Items[CarryOn]) + 1;
            IndexList.Items[CarryOn - 1] := Pointer(Val);
        end else begin
            IndexList.Items[CarryOn] := Pointer(Val + 1);
        end;
    end;
end;

function CombinationPosition(InputList, OutPutList : TList; Elements : integer; Position : longint) : boolean;
    //----------------------------------------------------------------------------------------------------------------------
    //Pega a conbinacao do desdriobramento numerico de indice position
var
    IndexList : TList;
    i : longint;
begin
    Result := FALSE;
    IndexList := TList.Create;
    try
        IndexList.Count := Elements;
        for i := 0 to Elements - 1 do begin //inicializa a lista de indices com o 0th elemento
            IndexList.Items[i] := Pointer(i);
        end;
        //Calcula a Position(th) combination
        for i := 0 to Position - 1 do begin
            Result := CombinationListInc(IndexList, IndexList.Count, 0);
            if Result then begin //"Overflow" do indice
                Exit;
            end;
        end;
        //Preenche a lista de retorno
        OutputList.Clear;
        OutPutList.Count := Elements;
        for i := 0 to Elements - 1 do begin
            OutputList.Items[i] := InputList.Items[Integer(IndexList.Items[i])];
        end;
    finally
        IndexList.Free;
    end;
end;

function FactLn(N : Integer) : Single;
    //----------------------------------------------------------------------------------------------------------------------
    //Returns ln(N!) for N >= 0
begin
    if (N <= 1) then begin
        Result := 0.0;
    end else begin
        if (N <= MFactLnA) then begin
            //use lookup table
            Result := FactLnATblLookup[N];
        end else begin
            //compute each time
            Result := GammaLn(N + 1.0);
        end;
    end;
end;

function Combinations(Number, NumberChosen : Integer) : Extended;
    //----------------------------------------------------------------------------------------------------------------------
begin
    if (Number < 0) or (NumberChosen < 0) or (Number < NumberChosen) then begin
        raise Exception.Create('Paramêtro inválido');
    end;
    {the 0.5 and Int function clean up roundoff error for smaller N and K}
    Result := Int(0.5 + exp(FactLn(Number) - FactLn(NumberChosen) - FactLn(Number - NumberChosen)));
end;


function GammaLn(X : Extended) : Extended;
    //----------------------------------------------------------------------------------------------------------------------
    //Returns ln(Gamma(X)) where X > 0
const
    cof: array[0..5] of Double = (76.18009172947146, -86.50532032941677, 24.01409824083091, -1.231739572450155,
        0.1208650973866179e-2, -0.5395239384953e-5);
var
    y, tmp, ser : Double;
    j : Integer;
begin
    if (X <= 0) then begin
        raise Exception.Create('Paramêtro inválido');
    end;
    y := X;
    tmp := X + 5.5;
    tmp := tmp - (X + 0.5) * ln(tmp);
    ser := 1.000000000190015;
    for j := low(cof) to high(cof) do begin
        y := y + 1.0;
        ser := ser + cof[j] / y;
    end;
    Result := -tmp + ln(sqrt2pi * ser / X);
end;



procedure Initialize;
//----------------------------------------------------------------------------------------------------------------------
//Fully initializes factorial lookup tables
var
    i : Integer;
begin
    FactATblLookup[2] := 2.0;
    for i := 3 to MFactA do begin
        FactATblLookup[i] := i * FactATblLookup[i - 1];
    end;
    for i := 2 to MFactLna do begin
        FactLnATblLookup[i] := GammaLn(i + 1.0);
    end;
    FactorialTablesInitialized := TRUE;
end;

end.


