{$IFDEF TREConsts}
	 {$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I TRELib.inc}

unit TREConsts;

interface

const
    //Informações inferidas pelo nome do computador 
    CMPNAME_LOCAL_ZONE     = 'Z';
    CMPNAME_LOCAL_CENTRAL  = 'C';
    CMPNAME_LOCAL_REGIONAL = 'R';
    CMPNAME_LOCAL_POS      = 4;  //Inicio da localização do computador, podendo ser numero da zona ou central
    CMPNAME_TYPE_LENGHT    = 3;  //Refere-se aos tipos abaixo
    CMPNAME_TYPE_WORKGROUP = 'STD';
    CMPNAME_TYPE_DOMAIN    = 'WKS';
    CMPNAME_TYPE_DOMAIN_CONTROLLER = 'PDC';



    CMPNAME_LOCAL_LENGHT: Integer    = 3;  //Comprimento do identificador da zona no nome do computador
    CMPNAME_LOCAL_ANY: Integer       = 0;
    CMPNAME_LOCAL_ALL: Integer       = -1;
    CMPNAME_LOCAL_MIN_VALUE: Integer = 1;
    CMPNAME_LOCAL_MAX_VALUE: Integer = 77;

    CMPNAME_ID_ANY: Integer = 0;
    CMPNAME_ID_ALL: Integer = -1;
    CMPNAME_ID_MIN_VALUE    = 1;
    CMPNAME_ID_MAX_VALUE    = 999999; //Lembrar das virtuais do TRE relacionadas com o patrimonio



type
    TTREComputerType = (
        ctUnknow,        //Desconhecido
        ctCentralPDC,    //Controlador de central/dominio
        ctCentralWKS,    //Estação de central
        ctZonePDC,       //Controlador de dominio zona
		 ctZoneWKS,       //Estação de trabalho zona em domínio
		 ctZoneSTD,       //Estação de trabalho zona em grupo de trabalho
        ctTREWKS,        //Estação de trabalho TRE
        ctNATT,          //Estação de NATT
        ctNATU,          //Estação de NATT
        ctDFE,           //Estação de Diretoria de fórum eleitoral
		 ctVirtual,       //Estação de máquina virtual (prefixo RPBW)
		 ctAny            //todos os tipos anteriores
		 );

    TTRENetType      = (ntUnknow, ntWorkGroup, ntDomain);
    TTREAccessMedium = (treamNone, treamVSAT, treamDialled, treamFrameRelay, treamAny);


implementation

end.
