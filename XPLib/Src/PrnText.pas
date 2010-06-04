{$IFDEF PrnText}
    {$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I XPLib.inc}

unit PrnText;

interface

{ TODO -oLIB -cLIB : Deslocar para SuperLib no formato de classe }
uses
    Windows, Messages, Classes, WinSpool, SysUtils;

type

    TPrnDestiny = (pdSpool, pdFile, pdBoth);

    TPrnText = class
    private
        Fph :      THandle;
        FDevMode : TDeviceModeA;
        FPrJob :   dword;
        FPrnFile : TStringList;
        FPrnFilePath : string;
        FPrnDestiny : TPrnDestiny;
        FPrnName : AnsiString;
        FDataType : string;

        function GetOutFilePath : string;
        procedure SetOutFilePath(const Value : string);
        function GetPrintDestiny : TPrnDestiny;
        procedure SetPrintDestiny(const Value : TPrnDestiny);
    protected
        class function WinString(const str : string) : string;
    public
        constructor Create(const pPrnName : string = ''; pDataType : string = 'RAW'); virtual;
        class procedure EnumPrt(pSt : TStrings; var pDef : Integer);
        procedure StartPrint(const pNameDoc : string; pCopies : Integer);
        procedure CancelPrint;
        procedure EndPrint;
        class function DOSString(const AnsiStr : string) : string;
        function ToPrn(const pText : string) : boolean;
        function ToPrnEmphasis(const pText : string) : boolean;
        function ToPrnBold(const pText : string) : boolean;
        function ToPrnLn(const pText : string) : boolean;
        function ToPrnFrm(const pFrmStr : string; const pArgs : array of const) : boolean;
        function ToPrnFrmC(const pFrmStr : string; const pArgs : array of const) : boolean;
        property OutFilePath : string read GetOutFilePath write SetOutFilePath;
      {{
       Caminho do arquivo para impressão.
      }
        property PrinterName : AnsiString read FPrnName;
      {{
       Nome da impressora.
      }
        property PrintDestiny : TPrnDestiny read GetPrintDestiny write SetPrintDestiny;
      {{
       Destino da impressão, podendo ser os seguintes valores:

           pdSpool : Para o Spool da Impressora.

           pdFile : Para o Arquivo setado.

           pdBoth : Para ambos acima.
      }
    end;

    TPrnEpson = class(TPrnText)
    public
        function SetPrnCharSetBR() : boolean;
        function SetPrnLineHeigth(Index : Integer) : boolean;
        function SetPrnToCondensed() : boolean;
    end;




implementation


 //----------------------------------------------------------------------------------------------------------------------
 //                            ***** ROTINAS AUXILIARES  *******
 //----------------------------------------------------------------------------------------------------------------------
function ReplaceStr(const S, Srch, Replace : string) : string;
var
    I :      Integer;
    Source : string;
begin
    Source := S;
    Result := EmptyStr;
    repeat
        I := Pos(Srch, Source);
        if I > 0 then begin
            Result := Result + Copy(Source, 1, I - 1) + Replace;
            Source := Copy(Source, I + Length(Srch), MaxInt);
        end else begin
            Result := Result + Source;
        end;
    until (I <= 0);
end;

 //----------------------------------------------------------------------------------------------------------------------
 //                            ***** ROTINAS AUXILIARES  *******
 //----------------------------------------------------------------------------------------------------------------------

procedure TPrnText.EndPrint();
//----------------------------------------------------------------------------------------------------------------------
{{
Finaliza o job de impressao

Revision: 1/8/2005
}
begin
    ToPrn(#7); //Emissao de beep
    if Assigned(FPrnFile) then begin
        FPrnFile.SaveToFile('C:\PrnResult.Txt');
        FPrnFile.Clear();
    end else begin
        if not (EndDocPrinter(Fph)) then begin
            RaiseLastOSError;
        end;
        //  Win32Check(EndDoc(ph)>0); //!!! linha comentada sem explicacao equivale a RaiseLastOSError;
    end;
end;

function TPrnEpson.SetPrnCharSetBR() : boolean;
    //----------------------------------------------------------------------------------------------------------------------
{{
Setar para a impressora epson caracteres do tipo BRASCII.

Revision: 1/8/2005
}
begin
    try
        Result := Self.ToPrn(#27'(t'#3#0#1#25#0); //BRASCII
    except
        Result := False;
    end;
end;

function TPrnEpson.SetPrnLineHeigth(Index : Integer) : boolean;
    //----------------------------------------------------------------------------------------------------------------------
{{
Setar para a impressora epson a altura da linha.

Revision: 1/8/2005
}
begin
    try
        case Index of
            0 : begin
                Result := Self.ToPrn(#27#48);  //1/8 polegada   ->  Normal
            end;
            1 : begin
                Result := Self.ToPrn(#27#49);  //7/72 polegada  -> Simples
            end;
            2 : begin
                Result := Self.ToPrn(#27#50);  //1/6 polegada  -> Duplo
            end;
            else begin
                Result := Self.ToPrn(#27#48);
            end;
        end;
    except
        Result := False;
    end;
end;

function TPrnEpson.SetPrnToCondensed() : boolean;
    //----------------------------------------------------------------------------------------------------------------------
{{
Setar para a impressora epson caracteres o tipo de impressão condensada.

Revision: 1/8/2005
}
begin
    try
        Result := Self.ToPrn(#27#120#0);
        Result := Result and Self.ToPrn(#27#33#4);
    except
        Result := False;
    end;
end;

function TPrnText.ToPrn(const pText : string) : boolean;
    //----------------------------------------------------------------------------------------------------------------------
{{
Alimenta o spool com a string passada

Revision: 1/8/2005
}

var
    cp : dword;
begin
    if (FPrnDestiny = pdBoth) then begin
        FPrnFile.Add(pText);
        if not (WritePrinter(Fph, PChar(pText), length(pText), cp)) then begin
            RaiseLastOSError;
        end;
    end else begin
        if (FPrnDestiny = pdSpool) then begin
            if not (WritePrinter(Fph, PChar(pText), length(pText), cp)) then begin
                RaiseLastOSError;
            end;
        end else begin
            FPrnFile.Add(pText);
        end;
    end;
    Result := True;
end;

function TPrnText.ToPrnBold(const pText : string) : boolean;
    //----------------------------------------------------------------------------------------------------------------------
{{
   Alimenta o spool com a string passada, no formato Negrito.

Revision: 1/8/2005
}
begin
    Result := ToPrn(#27#71);
    if Result then begin
        try
            Result := ToPrn(pText);
        finally
            ToPrn(#27#72);
        end;
    end;
end;


function TPrnText.ToPrnLn(const pText : string) : boolean;
    //----------------------------------------------------------------------------------------------------------------------
{{
Alimenta o spool com a string passada, adcionando um linha ao final

Revision: 1/8/2005
}
begin
    Result := Self.ToPrn(pText + #13#10);
end;

function TPrnText.ToPrnFrm(const pFrmStr : string; const pArgs : array of const) : boolean;
    //----------------------------------------------------------------------------------------------------------------------
{{
    Imprimi string para a impressora direcionada.

Revision: 1/8/2005
}
begin
    Result := Self.ToPrnLn(Format(pFrmStr, pArgs));
end;

function TPrnText.ToPrnFrmC(const pFrmStr : string; const pArgs : array of const) : boolean;
    //----------------------------------------------------------------------------------------------------------------------
{{
Imprime string para a impressora direcionada formatando antes seu conteudo.

Revision: 1/8/2005
}
begin
    Result := Self.ToPrnLn(DosString(SysUtils.Format(pFrmStr, pArgs)));
end;

{ TPrnText }

procedure TPrnText.CancelPrint;
{{
Cancela a impressão.

Revision: 1/8/2005
}
begin
    if (Assigned(FPrnFile)) then begin
        FPrnFile.Clear();
    end;
    if not SetJob(Fph, FPrJob, 0, nil, JOB_CONTROL_CANCEL) then begin
        RaiseLastOSError;
    end;
end;

constructor TPrnText.Create(const pPrnName : string = ''; pDataType : string = 'RAW');
var
    lPrinters : TStringList;
    lIndex, lIndex2 : Integer;
begin
    lIndex    := -1;
    lIndex2   := -1;
    lPrinters := TStringList.Create;
    TPrnText.EnumPrt(lPrinters, lIndex);    // Retorna a lista de impressoras.
    if ((lIndex > -1) or (lPrinters.Find(pPrnName, lIndex2))) then begin
        // Verifica se existe alguma impressora ou a que foi solicitada
        if (lIndex2 > -1) then begin // Verifica se a impressora solicitada foi encontrada.
            FPrnName := pPrnName;
        end else begin
            FPrnName := lPrinters.Strings[lIndex]; // Atribui o nome da impressora padrão.
        end;
    end else begin
        raise Exception.CreateFmt('A impressora %s não existe! ', [pPrnName]);
    end;
    FPrnDestiny := pdSpool;
    if (Trim(pDataType) <> EmptyStr) then begin
        FDataType := Trim(pDataType);
    end else begin
        FDataType := 'RAW';
    end;
    FPrnFile     := nil;
    FPrnFilePath := EmptyStr;
end;

procedure TPrnText.StartPrint(const pNameDoc : string; pCopies : Integer);
//----------------------------------------------------------------------------------------------------------------------
{{
   Inicia o spool da impressora para receber os caracteres
Revision: 29/7/2005
}
var
    pdi : PDocInfo1A;
    pd :  TPrinterDefaultsA;
begin
    { TODO -oRoger -cFUTURE : Verificar o modo de tracao da impressora Epson }

    if (FPrnDestiny = pdFile) then begin  //Se instanciada StringList para arquivo nao inicia spool(ver diretivas de compilacao)
        Exit;
    end;

    FDevMode.dmCopies := pCopies; // Especifica o Número de Copias para serem Impressas.
    FDevMode.dmFields := DM_COPIES;

    pd.pDatatype     := PAnsiChar(FDataType);
    // Especifica o tipo de dados da impressora. Ex. 'RAW', 'EMF'. Esses Tipos podem serem consultados
    // na Propiedade da Impressora->Avançados->Processador de Impressão.
    pd.pDevMode      := @FDevMode;
    pd.DesiredAccess := PRINTER_ACCESS_USE;
    if OpenPrinterA(PAnsiChar(FPrnName), Fph, @pd) then begin
        new(pdi);  { TODO -oRoger -cLIB : Tentar usar var local da pilha para alimentar este parametro da API do Spool do Windows }
        with pdi^ do begin
            pDocName := PAnsiChar(pNameDoc);
            if (FPrnFilePath = EmptyStr) then begin
                pOutputFile := nil;
            end else begin
                pOutputFile := PAnsiChar(FPrnFilePath);
            end;
            pDatatype := PAnsiChar(FDataType);
        end;
        FPrJob := StartDocPrinter(Fph, 1, pdi);
        if (FPrJob = 0) then begin
            RaiseLastOSError;
        end;
    end else begin
        RaiseLastOSError;
    end;

end;

function TPrnText.ToPrnEmphasis(const pText : string) : boolean;
{{
Alimenta o spool com a string passada, formatado os caracteres para seu tamanho máximo.

Revision: 1/8/2005
}

begin
    Result := ToPrn(#27#69);
    if Result then begin
        try
            Result := ToPrn(pText);
        finally
            ToPrn(#27#70);
        end;
    end;
end;

class procedure TPrnText.EnumPrt(pSt : TStrings; var pDef : Integer);
{{
Enumera a lista de impressoras com os seguintes parâmetros:

pSt : Onde irá ser adicionado os nomes das impressoras.

pDef : Indice da impressora padrão em pSt. Default -1.

Revision: 1/8/2005
}
    function LSRGetDefaultPrinter : string;
       {{
       Retorna o nome da impressora padrão.

       Revision: 1/8/2005
       }
    var
        ResStr : array[0..255] of char;
    begin
        GetProfileString('Windows', 'device', '', ResStr, 255);
        Result := Copy(StrPas(ResStr), 1, pos(',', StrPas(ResStr)) - 1);
        ;
    end;

type
    PPrInfoArr = ^TPrInfoArr;
    TPrInfoArr = array [0..0] of TPRINTERINFO2;
var
    i, Indx, Level : Integer;
    buf : Pointer;
    Need, Returned : dword;
    PrInfoArr : PPrInfoArr;
begin
    pSt.Clear;
    pDef  := -1; // Nenhuma Impressora Encontrada.
    Level := 2;
    EnumPrinters(PRINTER_ENUM_LOCAL or PRINTER_ENUM_CONNECTIONS, nil, Level, nil, 0, Need, Returned);
    GetMem(buf, Need);
    try
        EnumPrinters(PRINTER_ENUM_LOCAL or PRINTER_ENUM_CONNECTIONS, nil, Level, PByte(buf), Need, Need, Returned);
        PrInfoArr := buf;
       {$RANGECHECKS OFF}
        for i := 0 to Returned - 1 do begin
            Indx := pSt.Add(PrInfoArr[i].pPrinterName);
            if (PrInfoArr[i].Attributes and PRINTER_ATTRIBUTE_DEFAULT) > 0 then begin
                pDef := Indx;
            end;
        end;
       {$RANGECHECKS ON}
        if (pDef < 0) then begin
            pDef := pSt.indexOf(LSRGetDefaultPrinter());
        end;
    finally
        FreeMem(buf);
    end;
end;

function TPrnText.GetOutFilePath : string;
begin
    Result := FPrnFilePath;
end;

function TPrnText.GetPrintDestiny : TPrnDestiny;
begin
    Result := FPrnDestiny;
end;

procedure TPrnText.SetOutFilePath(const Value : string);
begin
    if (Trim(Value) <> EmptyStr) then begin
        FPrnFilePath := Trim(Value);
    end else begin
        raise Exception.CreateFmt('Caminho do Arquivo Inválido: "%s".', [Value]);
    end;
end;

procedure TPrnText.SetPrintDestiny(const Value : TPrnDestiny);
begin
    if (Value = pdFile) or (Value = pdBoth) then begin
        if (FPrnFilePath <> EmptyStr) then begin
            if (not Assigned(FPrnFile)) then begin
                FPrnFile := TStringList.Create;
            end;
            FPrnDestiny := Value;
        end else begin
            raise Exception.CreateFmt('Caminho do Arquivo Inválido: "%s".', [FPrnFilePath]);
        end;
    end else begin
        FPrnDestiny := Value;
        FreeAndNil(FPrnFile);
    end;
end;

class function TPrnText.DOSString(const AnsiStr : string) : string;
{{
Converte a cadeia para o mapeamento de caracteres local.

Revision: 17/8/2006 - Roger  
}
begin
    SetLength(Result, Length(AnsiStr));
    if Length(Result) > 0 then begin
        AnsiToOem(PAnsiChar(AnsiStr), PAnsiChar(Result));
    end;
    //comantario original do autor russo
    //OemToAnsi ïðåîáðàçóåò 15->253, à AnsiToOem 253->15 íå äåëàåò, ïîýòîìó òåðÿåòñÿ êîä ñæàòèÿ (çàìå÷åíî íà ðóññêîé Windows 95)
    Result := ReplaceStr(Result, chr(253), chr(15));
end;

class function TPrnText.WinString(const str : string) : string;
begin
    if (str <> EmptyStr) then begin
        OemToAnsi(PAnsiChar(str), PAnsiChar(str));
        Result := str;
    end else begin
        Result := EmptyStr;
    end;
end;

initialization
//----------------------------------------------------------------------------------------------------------------------
begin
{$IFDEF PRINT_TO_FILE}
    PrnFile := TStringList.Create();
{$ENDIF}
end;

finalization
//----------------------------------------------------------------------------------------------------------------------
begin
   {$IFDEF PRINT_TO_FILE}
   if Assigned(PrnFile) then begin
       PrnFile.Free();
   end;
   {$ENDIF}
end;

end.
