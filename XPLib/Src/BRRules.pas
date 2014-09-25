{$IFDEF BRRules}
{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I XPLib.inc}
unit BRRules;

{ {
  Implementa rotinas, classes e constantes para a validação, formatação e captura de parametros vinculados as regras brasileiras.
}
{ 1 Implementa rotinas, classes e constantes para a validação, formatação e captura de parametros vinculados as regras brasileiras. }

interface

uses
  XPTypes, SysUtils;

const
  BR_FORMAT_SETTINGS_DECIMAL_SEPARATOR = ',';
  BR_FORMAT_SETTINGS_SHORTDATEFORMAT   = 'dd/mm/yyyy';
  BR_FORMAT_SETTINGS_CURRENCYSTRING    = 'R$'; // colocado como vazio originalmente
  BR_FORMAT_SETTINGS_CURRENCYFORMAT    = 0;
  BR_FORMAT_SETTINGS_CURRENCYDECIMALS  = 2;

  BR_PROV_NAME_ACRE                = 'Acre';
  BR_PROV_CODE_ACRE                = 'AC';
  BR_PROV_NAME_ALAGOAS             = 'Alagoas';
  BR_PROV_CODE_ALAGOAS             = 'AL';
  BR_PROV_NAME_AMAPA               = 'Amapá';
  BR_PROV_CODE_AMAPA               = 'AP';
  BR_PROV_NAME_AMAZONAS            = 'Amazonas';
  BR_PROV_CODE_AMAZONAS            = 'AM';
  BR_PROV_NAME_BAHIA               = 'Bahia';
  BR_PROV_CODE_BAHIA               = 'BA';
  BR_PROV_NAME_CEARA               = 'Ceará';
  BR_PROV_CODE_CEARA               = 'CE';
  BR_PROV_NAME_DISTRITO_FEDERAL    = 'Distrito Federal';
  BR_PROV_CODE_DISTRITO_FEDERAL    = 'DF';
  BR_PROV_NAME_ESPIRITO_SANTO      = 'Espírito Santo';
  BR_PROV_CODE_ESPIRITO_SANTO      = 'ES';
  BR_PROV_NAME_GOIAS               = 'Goiás';
  BR_PROV_CODE_GOIAS               = 'GO';
  BR_PROV_NAME_MARANHAO            = 'Maranhão';
  BR_PROV_CODE_MARANHAO            = 'MA';
  BR_PROV_NAME_MATO_GROSSO         = 'Mato Grosso';
  BR_PROV_CODE_MATO_GROSSO         = 'MT';
  BR_PROV_NAME_MATO_GROSSO_DO_SUL  = 'Mato Grosso do Sul';
  BR_PROV_CODE_MATO_GROSSO_DO_SUL  = 'MS';
  BR_PROV_NAME_MINAS_GERAIS        = 'Minas Gerais';
  BR_PROV_CODE_MINAS_GERAIS        = 'MG';
  BR_PROV_NAME_PARA                = 'Pará';
  BR_PROV_CODE_PARA                = 'PA';
  BR_PROV_NAME_PARAIBA             = 'Paraíba';
  BR_PROV_CODE_PARAIBA             = 'PB';
  BR_PROV_NAME_PARANA              = 'Paraná';
  BR_PROV_CODE_PARANA              = 'PR';
  BR_PROV_NAME_PERNAMBUCO          = 'Pernambuco';
  BR_PROV_CODE_PERNAMBUCO          = 'PE';
  BR_PROV_NAME_PIAUI               = 'Piauí';
  BR_PROV_CODE_PIAUI               = 'PI';
  BR_PROV_NAME_RIO_DE_JANEIRO      = 'Rio de Janeiro';
  BR_PROV_CODE_RIO_DE_JANEIRO      = 'RJ';
  BR_PROV_NAME_RIO_GRANDE_DO_NORTE = 'Rio Grande do Norte';
  BR_PROV_CODE_RIO_GRANDE_DO_NORTE = 'RN';
  BR_PROV_NAME_RIO_GRANDE_DO_SUL   = 'Rio Grande do Sul';
  BR_PROV_CODE_RIO_GRANDE_DO_SUL   = 'RS';
  BR_PROV_NAME_RONDONIA            = 'Rondônia';
  BR_PROV_CODE_RONDONIA            = 'RO';
  BR_PROV_NAME_RORAIMA             = 'Rorâima';
  BR_PROV_CODE_RORAIMA             = 'RR';
  BR_PROV_NAME_SANTA_CATARINA      = 'Santa Catarina';
  BR_PROV_CODE_SANTA_CATARINA      = 'SC';
  BR_PROV_NAME_SAO_PAULO           = 'São Paulo';
  BR_PROV_CODE_SAO_PAULO           = 'SP';
  BR_PROV_NAME_SERGIPE             = 'Sergipe';
  BR_PROV_CODE_SERGIPE             = 'SE';
  BR_PROV_NAME_TOCANTINS           = 'Tocantins';
  BR_PROV_CODE_TOCANTINS           = 'TO';

  BR_PROVINCES_NAME_CODE: array [0 .. 26] of array [0 .. 1] of string = ((BR_PROV_NAME_ACRE, BR_PROV_CODE_ACRE),
	  (BR_PROV_NAME_ALAGOAS, BR_PROV_CODE_ALAGOAS), (BR_PROV_NAME_AMAPA, BR_PROV_CODE_AMAPA),
	  (BR_PROV_NAME_AMAZONAS, BR_PROV_CODE_AMAZONAS), (BR_PROV_NAME_BAHIA, BR_PROV_CODE_BAHIA),
	  (BR_PROV_NAME_CEARA, BR_PROV_CODE_CEARA), (BR_PROV_NAME_DISTRITO_FEDERAL, BR_PROV_CODE_DISTRITO_FEDERAL),
	  (BR_PROV_NAME_ESPIRITO_SANTO, BR_PROV_CODE_ESPIRITO_SANTO), (BR_PROV_NAME_GOIAS, BR_PROV_CODE_GOIAS),
	  (BR_PROV_NAME_MARANHAO, BR_PROV_CODE_MARANHAO), (BR_PROV_NAME_MATO_GROSSO, BR_PROV_CODE_MATO_GROSSO),
	  (BR_PROV_NAME_MATO_GROSSO_DO_SUL, BR_PROV_CODE_MATO_GROSSO_DO_SUL), (BR_PROV_NAME_MINAS_GERAIS, BR_PROV_CODE_MINAS_GERAIS),
	  (BR_PROV_NAME_PARA, BR_PROV_CODE_PARA), (BR_PROV_NAME_PARAIBA, BR_PROV_CODE_PARAIBA),
	  (BR_PROV_NAME_PARANA, BR_PROV_CODE_PARANA), (BR_PROV_NAME_PERNAMBUCO, BR_PROV_CODE_PERNAMBUCO),
	  (BR_PROV_NAME_PIAUI, BR_PROV_CODE_PIAUI), (BR_PROV_NAME_RIO_DE_JANEIRO, BR_PROV_CODE_RIO_DE_JANEIRO),
	  (BR_PROV_NAME_RIO_GRANDE_DO_NORTE, BR_PROV_CODE_RIO_GRANDE_DO_NORTE),
	  (BR_PROV_NAME_RIO_GRANDE_DO_SUL, BR_PROV_CODE_RIO_GRANDE_DO_SUL), (BR_PROV_NAME_RONDONIA, BR_PROV_CODE_RONDONIA),
	  (BR_PROV_NAME_RORAIMA, BR_PROV_CODE_RORAIMA), (BR_PROV_NAME_SANTA_CATARINA, BR_PROV_CODE_SANTA_CATARINA),
	  (BR_PROV_NAME_SAO_PAULO, BR_PROV_CODE_SAO_PAULO), (BR_PROV_NAME_SERGIPE, BR_PROV_CODE_SERGIPE),
	  (BR_PROV_NAME_TOCANTINS, BR_PROV_CODE_TOCANTINS));

  BR_PROVINCES_CODE_NAME: array [0 .. 26] of array [0 .. 1] of string = ((BR_PROV_NAME_ACRE, BR_PROV_CODE_ACRE), // AC
	  (BR_PROV_NAME_ALAGOAS, BR_PROV_CODE_ALAGOAS), // AL
	  (BR_PROV_NAME_AMAZONAS, BR_PROV_CODE_AMAZONAS), // AM
	  (BR_PROV_NAME_AMAPA, BR_PROV_CODE_AMAPA), // AP
	  (BR_PROV_NAME_BAHIA, BR_PROV_CODE_BAHIA), // BA
	  (BR_PROV_NAME_CEARA, BR_PROV_CODE_CEARA), // CE
	  (BR_PROV_NAME_DISTRITO_FEDERAL, BR_PROV_CODE_DISTRITO_FEDERAL), // DF
	  (BR_PROV_NAME_ESPIRITO_SANTO, BR_PROV_CODE_ESPIRITO_SANTO), // ES
	  (BR_PROV_NAME_GOIAS, BR_PROV_CODE_GOIAS), // GO
	  (BR_PROV_NAME_MARANHAO, BR_PROV_CODE_MARANHAO), // MA
	  (BR_PROV_NAME_MINAS_GERAIS, BR_PROV_CODE_MINAS_GERAIS), // MG
	  (BR_PROV_NAME_MATO_GROSSO_DO_SUL, BR_PROV_CODE_MATO_GROSSO_DO_SUL), // MS
	  (BR_PROV_NAME_MATO_GROSSO, BR_PROV_CODE_MATO_GROSSO), // MT
	  (BR_PROV_NAME_PARA, BR_PROV_CODE_PARA), // PA
	  (BR_PROV_NAME_PARAIBA, BR_PROV_CODE_PARAIBA), // PB
	  (BR_PROV_NAME_PERNAMBUCO, BR_PROV_CODE_PERNAMBUCO), // PE
	  (BR_PROV_NAME_PIAUI, BR_PROV_CODE_PIAUI), // PI
	  (BR_PROV_NAME_PARANA, BR_PROV_CODE_PARANA), // PR
	  (BR_PROV_NAME_RIO_DE_JANEIRO, BR_PROV_CODE_RIO_DE_JANEIRO), // RJ
	  (BR_PROV_NAME_RIO_GRANDE_DO_NORTE, BR_PROV_CODE_RIO_GRANDE_DO_NORTE), // RN
	  (BR_PROV_NAME_RONDONIA, BR_PROV_CODE_RONDONIA), // RO
	  (BR_PROV_NAME_RORAIMA, BR_PROV_CODE_RORAIMA), // RR
	  (BR_PROV_NAME_RIO_GRANDE_DO_SUL, BR_PROV_CODE_RIO_GRANDE_DO_SUL), // RS
	  (BR_PROV_NAME_SANTA_CATARINA, BR_PROV_CODE_SANTA_CATARINA), // SC
	  (BR_PROV_NAME_SERGIPE, BR_PROV_CODE_SERGIPE), // SE
	  (BR_PROV_NAME_SAO_PAULO, BR_PROV_CODE_SAO_PAULO), // SP
	  (BR_PROV_NAME_TOCANTINS, BR_PROV_CODE_TOCANTINS) // TO
	  );

function CPFVerifierDigit(CPF: Double): Integer; overload;
function CPFVerifierDigit(CPF: string): string; overload;
function CGCIsValid(mCGC: string): Boolean;
function CPFIsValid(S: string): Boolean;

var
  EDIT_MASK_DATE: string         = '99/99/9999;1;_';
  EDIT_MASK_CNPJ: string         = '99.999.999/9999-99;0;_';
  EDIT_MASK_CPF: string          = '999.999.999-99;0;_';
  EDIT_MASK_PHONE_NUMBER: string = '999.999.999-99;0;_';

type
  TBRRules = class(TObject)
	{ {
	  Classe utilitária para implementação das regras brasileiras.
	}
  private
	class var BRFormatSettings: TFormatSettings;
	class function GetPFormatSettingsRef: PFormatSettings; static;
  public
	class function CheckVerifierDigit(check: longint): Byte;
	class function ProvinceCodeByName(const ProvinceName: string): string;
	class function ProvinceNameByCode(const ProvinceCode: string): string;
	class procedure SetBRDefaultFormats;
	class property FormatSettingsRef: PFormatSettings read GetPFormatSettingsRef;
  end;

implementation

uses
  StrHnd, Str_Pas;

function CPFVerifierDigit(CPF: Double): Integer;
// ----------------------------------------------------------------------------------------------------------------------
var
  S: string;
  Soma: Integer;
  I, CalcDig1, CalcDig2: Byte;
begin
  Result := 0;

  { Obtém somente os dígitos }
  S := FloatToStr(CPF);

  { Se não tiver 9 dígitos... }
  if Length(S) <> 9 then begin
	Exit;
  end;

  { Cálculo do 1º dígito }
  Soma := 0;
  for I := 1 to 9 do begin
	Soma := Soma + StrToInt(S[I]) * (11 - I);
  end;

  CalcDig1 := 11 - (Soma mod 11);
  if CalcDig1 in [10, 11] then begin
	CalcDig1 := 0;
  end;

  S := S + IntToStr(CalcDig1);

  { Cálculo do 2º dígito }
  Soma := 0;
  for I := 1 to 10 do begin
	Soma := Soma + StrToInt(S[I]) * (12 - I);
  end;

  CalcDig2 := 11 - (Soma mod 11);
  if CalcDig2 in [10, 11] then begin
	CalcDig2 := 0;
  end;

  Result := StrToInt(IntToStr(CalcDig1) + IntToStr(CalcDig2));
end;

function CPFVerifierDigit(CPF: string): string;
// ----------------------------------------------------------------------------------------------------------------------
var
  S: string;
  Soma: Integer;
  I, CalcDig1, CalcDig2: Byte;
begin
  Result := '';

  { Obtém somente os dígitos }
  S := TStrHnd.FilterDigits(CPF);

  { Se não tiver 9 dígitos... }
  if Length(S) <> 9 then begin
	Exit;
  end;

  { Cálculo do 1º dígito }
  Soma := 0;
  for I := 1 to 9 do begin
	Soma := Soma + StrToInt(S[I]) * (11 - I);
  end;

  CalcDig1 := 11 - (Soma mod 11);
  if CalcDig1 in [10, 11] then begin
	CalcDig1 := 0;
  end;

  S := S + IntToStr(CalcDig1);

  { Cálculo do 2º dígito }
  Soma := 0;
  for I := 1 to 10 do begin
	Soma := Soma + StrToInt(S[I]) * (12 - I);
  end;

  CalcDig2 := 11 - (Soma mod 11);
  if CalcDig2 in [10, 11] then begin
	CalcDig2 := 0;
  end;

  Result := IntToStr(CalcDig1) + IntToStr(CalcDig2);
end;

function CPFIsValid(S: string): Boolean;
// ----------------------------------------------------------------------------------------------------------------------
var
  Soma: Integer;
  I, CalcDig1, CalcDig2: Byte;
begin
  Result := FALSE;

  { Obtém somente os dígitos }
  S := TStrHnd.FilterDigits(S);

  { Se não tiver 11 dígitos... }
  if Length(S) <> 11 then begin
	Exit;
  end;

  { Cálculo do 1º dígito }
  Soma := 0;
  for I := 1 to 9 do begin
	Soma := Soma + StrToInt(S[I]) * (11 - I);
  end;

  CalcDig1 := 11 - (Soma mod 11);
  if CalcDig1 in [10, 11] then begin
	CalcDig1 := 0;
  end;

  { Cálculo do 2º dígito }
  Soma := 0;
  for I := 1 to 10 do begin
	Soma := Soma + StrToInt(S[I]) * (12 - I);
  end;

  CalcDig2 := 11 - (Soma mod 11);
  if CalcDig2 in [10, 11] then begin
	CalcDig2 := 0;
  end;

  Result := (CalcDig1 = StrToInt(S[10])) and (CalcDig2 = StrToInt(S[11]));
end;

function CGCIsValid(mCGC: string): Boolean;
// ----------------------------------------------------------------------------------------------------------------------
var
  Tot, mNum: Integer;
  mDigito1, mDigito2: string;
begin
  mCGC := TStrHnd.FilterDigits(mCGC);
  if (mCGC = EmptyStr) then begin
	Result := FALSE;
  end else begin
	mCGC := PadL(mCGC, 14, '0'); // Completa com 0 a esquerda
	Tot := ((StrToInt(mCGC[1]) + StrToInt(mCGC[9])) * 5) + ((StrToInt(mCGC[2]) + StrToInt(mCGC[10])) * 4) +
		((StrToInt(mCGC[3]) + StrToInt(mCGC[11])) * 3) + ((StrToInt(mCGC[4]) + StrToInt(mCGC[12])) * 2) +
		((StrToInt(mCGC[5]) * 9) + (StrToInt(mCGC[6]) * 8)) + ((StrToInt(mCGC[7]) * 7) + (StrToInt(mCGC[8]) * 6));

	mNum := 11 - (Tot mod 11);
	if mNum > 9 then begin
	  mNum := 0;
	end;
	mDigito1 := IntToStr(mNum);
	Tot := ((StrToInt(mCGC[1]) + StrToInt(mCGC[9])) * 6) + ((StrToInt(mCGC[2]) + StrToInt(mCGC[10])) * 5) +
		((StrToInt(mCGC[3]) + StrToInt(mCGC[11])) * 4) + ((StrToInt(mCGC[4]) + StrToInt(mCGC[12])) * 3) +
		((StrToInt(mCGC[5]) + StrToInt(mDigito1)) * 2) + ((StrToInt(mCGC[6]) * 9) + (StrToInt(mCGC[7]) * 8)) +
		((StrToInt(mCGC[8]) * 7));

	mNum := 11 - (Tot mod 11);
	if mNum > 9 then begin
	  mNum := 0;
	end;
	mDigito2 := IntToStr(mNum);
	if mCGC[13] + mCGC[14] = mDigito1 + mDigito2 then begin
	  Result := TRUE;
	end else begin
	  Result := FALSE;
	end;
  end;
end;

{$WARNINGS OFF}
// Linha Acima para realcar as finais

{ -**********************************************************************
  ************************************************************************
  ******************
  ******************  Class:    TBRRules
  ******************  Category: No category
  ******************
  ************************************************************************
  ************************************************************************ }
{ -------------------------------------------------------------------------------------------------------------------------------- }
class function TBRRules.CheckVerifierDigit(check: longint): Byte;
{ {
  Calculo do campo C3 de um cheque.

  check : Numero do cheque

  returns: valor do campo C3.
}
{ 1 Calculo do campo C3 de um cheque. }
var
  multiplicador, tamanho, resto, total, y: Integer;
  aux: Char;
  S: string;
begin
  S := IntToStr(check);
  total := 0;
  tamanho := Length(S);
  multiplicador := 9;
  for y := (tamanho) downto 1 do begin
	aux := S[y];
	total := total + ((ord(aux) - ord('0')) * multiplicador);
	Dec(multiplicador);
	if (multiplicador = 1) then begin
	  multiplicador := 9;
	end;
  end;
  resto := total mod 11;
  if (resto = 10) then begin
	Result := 0;
  end else begin
	Result := resto;
  end;
end;

{ -------------------------------------------------------------------------------------------------------------------------------- }
class function TBRRules.GetPFormatSettingsRef: PFormatSettings;
begin
  Result := @TBRRules.BRFormatSettings;
end;

class function TBRRules.ProvinceCodeByName(const ProvinceName: string): string;
{ {
  Retorna a sigla de um estado brasileiro passado o seu nome.

  ProvinceName : Nome do estado brasileiro.

  Result
  Sigla do estado.
}
{ 1 Retorna a sigla de um estado brasileiro passado o seu nome. }

const
  _TOKEN_          = '@';
  _PROVINCE_NAMES_ = _TOKEN_ + BR_PROV_NAME_ACRE + _TOKEN_ + BR_PROV_NAME_ALAGOAS + _TOKEN_ + BR_PROV_NAME_AMAPA + _TOKEN_ +
	  BR_PROV_NAME_AMAZONAS + _TOKEN_ + BR_PROV_NAME_BAHIA + _TOKEN_ + BR_PROV_NAME_CEARA + _TOKEN_ + BR_PROV_NAME_DISTRITO_FEDERAL
	  + _TOKEN_ + BR_PROV_NAME_ESPIRITO_SANTO + _TOKEN_ + BR_PROV_NAME_GOIAS + _TOKEN_ + BR_PROV_NAME_MARANHAO + _TOKEN_ +
	  BR_PROV_NAME_MATO_GROSSO + _TOKEN_ + BR_PROV_NAME_MATO_GROSSO_DO_SUL + _TOKEN_ + BR_PROV_NAME_MINAS_GERAIS + _TOKEN_ +
	  BR_PROV_NAME_PARA + _TOKEN_ + BR_PROV_NAME_PARAIBA + _TOKEN_ + BR_PROV_NAME_PARANA + _TOKEN_ + BR_PROV_NAME_PERNAMBUCO +
	  _TOKEN_ + BR_PROV_NAME_PIAUI + _TOKEN_ + BR_PROV_NAME_RIO_DE_JANEIRO + _TOKEN_ + BR_PROV_NAME_RIO_GRANDE_DO_NORTE + _TOKEN_ +
	  BR_PROV_NAME_RIO_GRANDE_DO_SUL + _TOKEN_ + BR_PROV_NAME_RONDONIA + _TOKEN_ + BR_PROV_NAME_RORAIMA + _TOKEN_ +
	  BR_PROV_NAME_SANTA_CATARINA + _TOKEN_ + BR_PROV_NAME_SAO_PAULO + _TOKEN_ + BR_PROV_NAME_SERGIPE + _TOKEN_ +
	  BR_PROV_NAME_TOCANTINS;
var
  SearchString, Pattern: string;
  p: Integer;

begin
  Pattern := AnsiUpperCase(_PROVINCE_NAMES_);
  SearchString := _TOKEN_ + AnsiUpperCase(ProvinceName);
  p := Pos(SearchString, Pattern);
  if (p > 0) then begin
	Delete(Pattern, p, System.MaxInt);
	p := StrCountCharRepet(_TOKEN_, Pattern);
	Result := BR_PROVINCES_NAME_CODE[p, 1];
  end else begin
	Result := EmptyStr;
  end;
end;

{ -------------------------------------------------------------------------------------------------------------------------------- }
class function TBRRules.ProvinceNameByCode(const ProvinceCode: string): string;
{ {
  Retorna o nome de um estado brasileiro dado a sua sigla.

  ProvinceCode: Sigla do estado brasileiro.

  Result
  Nome do estado brasileiro.
}
{ 1 Retorna o nome de um estado brasileiro dado a sua sigla. }

const
  _TOKEN_           = '@';
  _PROVINCES_CODES_ = _TOKEN_ + BR_PROV_CODE_ACRE + _TOKEN_ + BR_PROV_CODE_ALAGOAS + _TOKEN_ + BR_PROV_CODE_AMAZONAS + _TOKEN_ +
	  BR_PROV_CODE_AMAPA + _TOKEN_ + BR_PROV_CODE_BAHIA + _TOKEN_ + BR_PROV_CODE_CEARA + _TOKEN_ + BR_PROV_CODE_DISTRITO_FEDERAL +
	  _TOKEN_ + BR_PROV_CODE_ESPIRITO_SANTO + _TOKEN_ + BR_PROV_CODE_GOIAS + _TOKEN_ + BR_PROV_CODE_MARANHAO + _TOKEN_ +
	  BR_PROV_CODE_MINAS_GERAIS + _TOKEN_ + BR_PROV_CODE_MATO_GROSSO_DO_SUL + _TOKEN_ + BR_PROV_CODE_MATO_GROSSO + _TOKEN_ +
	  BR_PROV_CODE_PARA + _TOKEN_ + BR_PROV_CODE_PARAIBA + _TOKEN_ + BR_PROV_CODE_PERNAMBUCO + _TOKEN_ + BR_PROV_CODE_PIAUI +
	  _TOKEN_ + BR_PROV_CODE_PARANA + _TOKEN_ + BR_PROV_CODE_RIO_DE_JANEIRO + _TOKEN_ + BR_PROV_CODE_RIO_GRANDE_DO_NORTE + _TOKEN_ +
	  BR_PROV_CODE_RONDONIA + _TOKEN_ + BR_PROV_CODE_RORAIMA + _TOKEN_ + BR_PROV_CODE_RIO_GRANDE_DO_SUL + _TOKEN_ +
	  BR_PROV_CODE_SANTA_CATARINA + _TOKEN_ + BR_PROV_CODE_SERGIPE + _TOKEN_ + BR_PROV_CODE_SAO_PAULO + _TOKEN_ +
	  BR_PROV_CODE_TOCANTINS;
var
  SearchString, Pattern: string;
  p: Integer;

begin
  Pattern := AnsiUpperCase(_PROVINCES_CODES_);
  SearchString := _TOKEN_ + AnsiUpperCase(ProvinceCode);
  p := Pos(SearchString, Pattern);
  if (p > 0) then begin
	Delete(Pattern, p, System.MaxInt);
	p := StrCountCharRepet(_TOKEN_, Pattern);
	Result := BR_PROVINCES_CODE_NAME[p, 0];
  end else begin
	Result := EmptyStr;
  end;
end;

{ -------------------------------------------------------------------------------------------------------------------------------- }
class procedure TBRRules.SetBRDefaultFormats;
{ {
  Os valores setados são:
  DecimalSeparator := ',';
  ShortDateFormat := 'dd/mm/yyyy';
  CurrencyString := '';
  CurrencyFormat := 0;
  CurrencyDecimals := 2;
}
{ 1 Ajusta os parametros de formatação globais do aplicativo para PT-BR( valores comuns ) }
var
  PFS: PFormatSettings;
begin
  PFS := TBRRules.FormatSettingsRef;
  { SETA BRASIL }
  PFS^.DecimalSeparator := BR_FORMAT_SETTINGS_DECIMAL_SEPARATOR;
  PFS^.ShortDateFormat := BR_FORMAT_SETTINGS_SHORTDATEFORMAT;
  PFS^.CurrencyString := BR_FORMAT_SETTINGS_CURRENCYSTRING;
  PFS^.CurrencyFormat := BR_FORMAT_SETTINGS_CURRENCYFORMAT;
  PFS^.CurrencyDecimals := BR_FORMAT_SETTINGS_CURRENCYDECIMALS;
end;

end.
