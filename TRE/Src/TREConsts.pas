{$IFDEF TREConsts}
	 {$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I TRELib.inc}

unit TREConsts;

interface

const
   //Informações inferidas pelo nome do computador 
   CMPNAME_LOCAL_ZONE = 'Z';
   CMPNAME_LOCAL_CENTRAL = 'C';
	CMPNAME_LOCAL_REGIONAL = 'R';
	CMPNAME_LOCAL_POS = 4; //Inicio da localização do computador, podendo ser numero da zona ou central
	CMPNAME_TYPE_LENGHT = 3;  //Refere-se aos tipos abaixo
	CMPNAME_TYPE_WORKGROUP = 'STD';
   CMPNAME_TYPE_DOMAIN = 'WKS';
   CMPNAME_TYPE_DOMAIN_CONTROLLER = 'PDC';



   CMPNAME_LOCAL_LENGHT : integer = 3;  //Comprimento do identificador da zona no nome do computador
   CMPNAME_LOCAL_ANY : integer = 0;
   CMPNAME_LOCAL_ALL : integer = -1;
   CMPNAME_LOCAL_MIN_VALUE : integer = 1;
   CMPNAME_LOCAL_MAX_VALUE :  integer = 77;

   CMPNAME_ID_ANY : integer = 0;
   CMPNAME_ID_ALL : integer = -1;
   CMPNAME_ID_MIN_VALUE = 1;
   CMPNAME_ID_MAX_VALUE = 999999; //Lembrar das virtuais do TRE relacionadas com o patrimonio



type
   TTREComputerType = ( ctCentralPDC, ctCentralWKS, ctZonePDC, ctZoneWKS, ctTREWKS, ctVirtual, ctAny );
   TTRENetType = ( ntUnknow, ntWorkGroup, ntDomain );
   TTREAccessMedium = ( treamNone, treamVSAT, treamDialled, treamFrameRelay, treamAny );


implementation

end.
