{$IFDEF TREUtils}
	 {$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I TRELib.inc}

unit TREUtils;

interface

uses
    SysUtils, Windows, Classes, BRRules, TREConsts;


type
    ETREException = class(Exception);

type
    TTREUtils = class
    public
        class procedure CheckComputername(const Name : string); overload;
        class procedure CheckComputername(const Name : string; Local : Integer); overload;
        class procedure CheckComputername(const Name : string; ComputerType : TTREComputerType); overload;
        class procedure CheckComputername(const Name : string; Local : Integer; ComputerType : TTREComputerType); overload;
        class procedure CheckComputername(const Name : string; Local, CompId : Integer; ComputerType : TTREComputerType); overload;
        class function GetComputerTypeByName(const Name : string) : TTREComputerType;
        class function GetComputerZone(const Computername : string) : Integer;
        class function GetComputerId(const Computername : string) : Integer;
        class function GetNetworkType(const ComputerName : string) : TTRENetType;
        class function GetZonePrimaryComputer(const StationName : string) : string;
    end;

implementation

uses
    StrHnd, Str_Pas;

{ TTREUtils }

class procedure TTREUtils.CheckComputername(const Name : string; Local : Integer);
begin
    CheckComputername(Name, Local, ctAny);
end;

class procedure TTREUtils.CheckComputername(const Name : string);
begin
    CheckComputername(Name, CMPNAME_LOCAL_ALL, ctAny);
end;

class procedure TTREUtils.CheckComputername(const Name : string; Local, CompId : Integer; ComputerType : TTREComputerType);
var
    prefixFound, prefixExpected : char;
    prefixLen, localInt, IdInt, idBias : Integer;
    localStr, IdStr : string;
    typeFound : TTREComputerType;
begin
    //Checa o tipo do computador
    prefixFound := Str_Pas.GetIChar(UpperCase(Name), 1);
    case ComputerType of
        ctCentralPDC, ctCentralWKS : begin
            prefixExpected := CMPNAME_LOCAL_CENTRAL;
        end;
        ctZonePDC, ctZoneWKS : begin
            prefixExpected := CMPNAME_LOCAL_ZONE;
        end;
        ctTREWKS : begin
            prefixExpected := CMPNAME_LOCAL_REGIONAL;
        end;
        ctVirtual : begin
            prefixExpected := CMPNAME_LOCAL_REGIONAL;
        end;
        ctAny : begin
            prefixExpected := prefixFound;
        end;
        else begin
            raise ETREException.Create('Tipo do computador desconhecido');
        end;
    end;
    if (prefixFound <> prefixExpected) then begin
        raise ETREException.CreateFmt('Tipo de computador encontrado "%s" diverge do desejado "%s".', [prefixFound, prefixExpected
            ]);
    end;

    //Calcula o comprimento do prefixo encontrado
    case prefixFound of
        CMPNAME_LOCAL_ZONE : begin
            prefixLen := 3;
            idBias    := PrefixLen + CMPNAME_LOCAL_LENGHT + CMPNAME_TYPE_LENGHT;
            if (Pos(CMPNAME_TYPE_DOMAIN_CONTROLLER, Name) > 0) then begin
                typeFound := ctZonePDC;
            end else begin
                typeFound := ctZoneWKS;
            end;
        end;
        CMPNAME_LOCAL_CENTRAL : begin
            prefixLen := 3;
            idBias    := PrefixLen + CMPNAME_LOCAL_LENGHT + CMPNAME_TYPE_LENGHT;
            if (Pos(CMPNAME_TYPE_DOMAIN_CONTROLLER, Name) > 0) then begin
                typeFound := ctCentralPDC;
            end else begin
                typeFound := ctCentralWKS;
            end;
        end;
        CMPNAME_LOCAL_REGIONAL : begin
            prefixLen := 4;
            idBias    := PrefixLen + CMPNAME_LOCAL_LENGHT + CMPNAME_TYPE_LENGHT;
            typeFound := ctTREWKS;
            { TODO -oroger -cdsg : Para identificar se a máquina é virtual não existe modo seguro }
        end;
        else begin
            raise ETREException.CreateFmt('Impossível validar nome de computador para prefixo/tipo "%s"', [prefixFound]);
        end;
    end;

    //Checa o local do computador
    if ((Local <> CMPNAME_LOCAL_ANY) and (Local <> CMPNAME_LOCAL_ALL)) then begin
        localStr := Copy(Name, prefixLen, CMPNAME_LOCAL_LENGHT);
        try
            localInt := StrToInt(localStr);
            if (localInt <> Local) then begin
                raise ETREException.Create('Local/Zona do computador diverge do esperado.');
            end;
        except
            on E : Exception do begin
                raise ETREException.CreateFmt('Local/Zona do computador "%s" inválido.'#13'%s', [localStr, E.Message]);
            end;
        end;
    end;

    //checa o id do computador
    IdStr := System.Copy(Name, idBias + 1, Length(Name));
    try
        IdInt := StrToInt(IdStr);
        if ((CompId <> CMPNAME_ID_ALL) and (CompId <> CMPNAME_ID_ANY) and (CompId <> IdInt)) then begin
            raise ETREException.CreateFmt('O identificador da estação esperado "%d" diverge do encontrado "%d".', [CompId, IdInt]
                );
        end;
    except
        on E : Exception do begin
            raise ETREException.CreateFmt('A string "%s" não representa um identificador de estação válido.'#13'%s', [IdStr,
                E.Message]);
        end;
    end;

    //tipo do computador
    if ((ComputerType <> ctAny) and (ComputerType <> typeFound)) then begin
        raise ETREException.Create('Tipo do computador encontrado diverge do esperado');
    end;

end;

class procedure TTREUtils.CheckComputername(const Name : string; ComputerType : TTREComputerType);
begin
    CheckComputername(Name, CMPNAME_LOCAL_ALL, ComputerType);
end;

class procedure TTREUtils.CheckComputername(const Name : string; Local : Integer; ComputerType : TTREComputerType);
begin
    CheckComputername(Name, Local, CMPNAME_ID_ANY, ComputerType);
end;

class function TTREUtils.GetComputerId(const Computername : string) : Integer;
{{
Leitura do identificador do computador na rede local.
Para zonas representa 'R|Z|C'<UF>[SO(1)]<zzz><NetType(3)><nnn>
}
var
    part : string;
    x :    Integer;
begin
    {$MESSAGE 'ALERTA: Implementação meia-boca'}
    {TODO -oroger -clib : A análise dos componentes do nome do computador deve passar por um paser devido a sua complexidade.
    Para esta implementação(fator tempo sempre) vamos pegar os digitos finais}
    x := Pos(CMPNAME_TYPE_WORKGROUP, Computername);
    if x <= 0 then begin //tipo std
        x := Pos(CMPNAME_TYPE_DOMAIN_WORKSTATION, Computername);
        if x <= 0 then begin
            x := Pos(CMPNAME_TYPE_DOMAIN_CONTROLLER, Computername);
        end;
    end;
    if x > 0 then begin
        part   := Copy(Computername, x + 3); //Posição mais comprimento da cadeia
        Result := StrToInt(part);
    end else begin
        raise ETREException.CreateFmt('"%s" não é um nome de computador válido', [Computername]);
    end;
end;

class function TTREUtils.GetComputerTypeByName(const Name : string) : TTREComputerType;
    ///
    /// Identifica o tipo de computador pelo seu nome de acordo com os padrões da Módulo(SiS)
    /// O padrão original é o seguinte:
    /// <L(1)><UF(2)>[OS(1)]<lll><WKS|STD><sss>
    /// L(1) - Local(Z - Zona, C - Central, R - Regional )
    /// UF(2) - Sigla de unidade federativa
    /// OS(1) - Tipo do SO( W - Windows )
    /// lll - Número do local com 3 dígitos, caso máquina <> de secretária, neste caso o id tem inúmeros dígitos
    /// WKS|STD - Tipo do domínio
    /// sss - Número da estação com 3 dígitos
    ///
var
    PrefixLocal : char;
    PrefixUF :    string[2];
    PrefixOS :    char;
    ZoneID, StationID : Integer;
    NormName :    string;
    NumberChain : string;
    NetType :     string;
begin
    Result := ctUnknow;
    if (Length(Name) < 6) then begin //não obedece nehum padrão válido
        Exit;
    end;

    NormName    := UpperCase(Name); //Normatiza o nome
    PrefixLocal := NormName[1];
    //Filtra classe do computador
    case PrefixLocal of   //Diferencia o local do computador Zona/Central/Secretaria/outras
        CMPNAME_LOCAL_ZONE, CMPNAME_LOCAL_CENTRAL, CMPNAME_LOCAL_REGIONAL : begin
            PrefixUF := Copy(NormName, 2, 2);
            if (BRRules.TBRRules.ProvinceNameByCode(PrefixUF) = EmptyStr) then begin //nome de UF invalida
                Result := ctUnknow;
            end else begin
                //Identifica se pc virtual
                if (NormName[4] = 'W') then begin
                    StationID   := TStrHnd.GetInteger(NormName, 5, 0);
                    NumberChain := Copy(NormName, 4, Length(NormName));
                    Result      := ctTREWKS;
                end;
                ZoneID  := TStrHnd.GetInteger(NormName, 4, 0);
                NetType := TStrHnd.GetAlphaText(NormName, 7, EmptyStr);
                case ZoneId of
                    1..80, 226..999 : begin  {TODO -oroger -cdsg : Passar constantes para biblioteca ???}
                        if SameText(NetType, CMPNAME_TYPE_DOMAIN_WORKSTATION) then begin
                            Result := ctZoneWKS;
                        end else begin
                            if (SameText(NetType, CMPNAME_TYPE_WORKGROUP)) then begin
                                Result := ctZoneSTD;
                            end else begin
                                if (SameText(NetType, CMPNAME_TYPE_DOMAIN_CONTROLLER)) then begin
                                    Result := ctZonePDC;
                                end else begin
                                    Result := ctUnknow;
                                end;
                            end;
                        end;
                    end;
                    200..205 : begin
                        Result := ctNATU;
                    end;
                    220..225 : begin
                        Result := ctNATT;
                    end;
                    210..215 : begin
                        Result := ctDFE;
                    end;
                    else begin
                        //Mesmo sabendo que primeiro case pega quase tudo fica a regra abaixo
                        Result := ctUnknow;
                    end;
                end;
            end;
        end;
        else begin
            //Computador sem padrão conhecido
            Result := ctUnknow;
        end;
    end;
end;

class function TTREUtils.GetComputerZone(const Computername : string) : Integer;
{{
Leitura da zona do computador baseado em seu nome

PRE: Computername no formato de nome de computador de zona eleitoral

Revision: 1/6/2008 - roger
}
var
    zone : string;
begin
    zone   := Copy(Computername, CMPNAME_LOCAL_POS, CMPNAME_LOCAL_LENGHT);
    Result := StrToInt(zone);
end;

class function TTREUtils.GetNetworkType(const ComputerName : string) : TTRENetType;
begin
    if (Pos(CMPNAME_TYPE_WORKGROUP, ComputerName) > 0) then begin   //Rede workgroup
        Result := ntWorkGroup;
    end else begin
        if ((Pos(CMPNAME_TYPE_DOMAIN_WORKSTATION, ComputerName) > 0) or (Pos(CMPNAME_TYPE_DOMAIN_CONTROLLER, ComputerName) > 0))
        then begin
            Result := ntDomain;
        end else begin
            raise ETREException.CreateFmt('"%s" não representa um nome de computador válido para uma zona eleitoral', [
                ComputerName]);
        end;
    end;
end;

class function TTREUtils.GetZonePrimaryComputer(const StationName : string) : string;
    /// <summary>
    /// Calcula o nome do computador primário da zona. O nome da estação deve obedecer o padrão ZPB<zzz><NetType><nn>
    /// IMPORTANTE: Esta rotina desconsidera a existência das centrais de atendimento!!!!!
    /// </summary>
    /// Revision - 2014-08-26 19:13:46 - Roger
    /// Este ano o controlador de domínio passou a ser centralizado para todas as zonas, centrais, etc. Assim a figura do pc-primario passa a ser
    /// a máquina de ID 1, tanto para zne, como para central
var
    ZoneId :  Integer;
    EnvChar : char;
    SZonId :  string;
    SPCType : string;
begin
    Result := EmptyStr;
    if StationName <> EmptyStr then begin
        EnvChar := StationName[1];
        if CharInSet(EnvChar, [CMPNAME_LOCAL_ZONE, CMPNAME_LOCAL_CENTRAL, CMPNAME_LOCAL_REGIONAL]) then
        begin //Tipo permitido de estação
            SZonId := Copy(StationName, 4, 3); //Id da zona
            TryStrToInt(SZonId, ZoneId);
            SPCType := Copy(StationName, 7, 3);
            if ( not TStrHnd.IsPertinent(SPCType, [CMPNAME_TYPE_WORKGROUP, CMPNAME_TYPE_DOMAIN_WORKSTATION], True)) then
			 begin //estacao normal - primario = wks/std(01)
				 if (SameText(SPCType, CMPNAME_TYPE_DOMAIN_CONTROLLER)) then begin
                    Result := StationName;
                    Exit;
                end;
            end;
            Result := Copy(StationName, 1, 3) + Format('%3.3d', [ZoneId]) + SPCType + '01';
        end else begin
            raise Exception.CreateFmt('O nome do computador "%s" não suportado para este serviço', [StationName]);
        end;
    end;
end;

end.
