{$IFDEF CryptIni}
{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I StFLib.inc}
unit CryptIni;

{
  ///function TCypher.Encode(Txt : string) : string;
  ///var
  ///	buffer : array of char;
  ///	block :  string;
  ///	i, z, qtd, startPos, endPos, Len : integer;
  ///begin
  ///	Result := EmptyStr;
  ///	Len    := Length(Self.FKey);
  ///	if (Len <= 3) or (Len > 7) then begin
  ///		raise Exception.Create('Chave deve ter mais que 3 e menos de 8 caracteres.');
  ///	end;
  ///	SetLength(buffer, Len);
  ///	if (length(txt) mod len) = 0 then begin
  ///		qtd := (length(txt) div len);
  ///	end else begin
  ///		qtd := (length(txt) div len) + 1;
  ///	end;
  ///	EndPos := Len;
  ///	for z := 1 to qtd do begin
  ///		startPos := (z * len) - (len - 1);
  ///        block    := copy(txt, StartPos, endPos);
  ///        if length(block) < len then begin
  ///            for i := 1 to len - length(block) do begin
  ///                block := block + #0;
  ///            end;
  ///        end;
  ///        for i := 1 to Len do begin
  ///			buffer[i - 1] := char(Ord(Self.FKey[i]) xor Ord(block[i]));
  ///        end;
  ///        block := EmptyStr; //zera e le a partir de buffer
  ///        for i := 0 to len - 1 do begin
  ///            block := Block + binhnd.Byte2Hexa(Ord(buffer[i]));
  ///        end;
  ///        block  := Mathhnd.BaseConvert(block, 16, 36);
  ///		Result := Result + block + ' ';
  ///	end;
  ///end;
  ///
  ///function TCypher.Decode(const pCode : string) : string;
  ///var
  ///    buffer : array of char;    // variavel que conterá os caracteres codificados
  ///    block, par, code : string; //segmento de txt em paginas de Len caracteres
  ///    i, z, qtd, startPos, endPos, Len : integer;
  ///begin
  ///    Result := EmptyStr;
  ///    code   := trim(pCode);
  ///    if (code <> EmptyStr) then begin
  ///        qtd := 1 + Str_Pas.StrCountCharRepet(' ', code);
  ///        // conta quantos espaços(' ') existem na string code. isso determina em quantos blocos ela sera dividida
  ///        len := length(Self.FKey);    // Tamanho da chave
  ///        SetLength(buffer, 0);         // zera o buffer
  ///        if Len <= 3 then begin
  ///            raise Exception.Create('Chave deve ter mais que 3 caracteres.');
  ///        end;
  ///        startPos := 1; // define 1 como posicao inicial para montar um bloco
  ///        //for i := 0 to qtd do begin
  ///        i := 0;
  ///        repeat
  ///            endPos := pos(' ', code) - 1;
  ///            block  := EmptyStr;    // zera block
  ///            if (endPos > 0) then begin
  ///				block := copy(code, startPos, endPos); // monta um bloco baseado em StartPos e EndPos
  ///                Delete(code, 1, endpos + 1);
  ///            end else begin
  ///                block := code;
  ///            end;
  ///            block := Mathhnd.BaseConvert(block, 36, 16); // converte um bloco para hexadecimal
  ///            block := Str_Pas.PadR(block, (2 * len), '0');
  ///            // completa com a quantidade necessaria de zeros a esquerda, para que fique com o dobro do tamanho da chave, uma vez que cada par hexadecimal ira ser comparado com um caracter da chave
  ///			for z := 0 to length(block) do begin
  ///				if z mod 2 = 0 then begin
  ///					continue; // se o valor de Z for par ele pula para o proximo loop
  ///				end;
  ///				par := copy(block, z, 2);
  ///				// obtem um par hexadecimal a partir do bloco, utilizando z como posicao inicial, uma vez que toda vez que o loop chegar a esse ponto, Z tera assumido um valor impar. isso se faz necessario para que seja obtido um par a partir do primeiro caracter do bloco
  ///				SetLength(buffer, length(buffer) + 1); // incrementa buffer de uma posicao
  ///				buffer[high(buffer)] := char(binhnd.StrHex2Byte(par));
  ///				// insere na ultima posicao de buffer um caracter originado da conversao de um par hexadecimal para char.
  ///
  ///			end;
  ///			block := EmptyStr; // zera o bloco
  ///			for z := 0 to len - 1 do begin // monta agora um bloco com dessa vez de caracteres
  ///				block := block + char(Ord(Self.fkey[z + 1]) xor Ord(buffer[z]));
  ///				// incrementa o bloco com um caracter originado de um XOR entre um caracter de buffer e o seu caracter correspondente na chave
  ///			end;
  ///			SetLength(buffer, 0);
  ///			Result := Result + block; // incrementa o result com um bloco decodificado.
  ///			Inc(i);
  ///		until (i = qtd);
  ///	end;
  ///end;

  Revision: 14/10/2005 - Roger

}

interface

uses
  Classes, SysUtils, IniFiles,
  HKStreamRoutines; //Rotinas para criptografia de streams;

type
  TCryptedIni = class(TCustomIniFile)
  private
	FSections: TStringList;
	procedure SetEncrypted(const Value: boolean);
  protected
	FEncrypted: boolean; //Permite que descentes comecem com qualquer tipo de dado por padrao
	function AddSection(const Section: string): TStrings;
	procedure LoadValues; virtual; //Override este metodo e o UpdateFile para os descendentes
	procedure ReadFromFile(const IniFileName: string; List: TStrings); virtual;
  public
	property Encrypted: boolean read FEncrypted write SetEncrypted;
	constructor Create(const FileName: string); virtual;
	destructor Destroy; override;
	procedure Clear;
	procedure DeleteKey(const Section, Ident: string); override;
	procedure EraseSection(const Section: string); override;
	procedure GetStrings(List: TStrings);
	procedure ReadSection(const Section: string; Strings: TStrings); override;
	procedure ReadSections(Strings: TStrings); override;
	procedure ReadSectionValues(const Section: string; Strings: TStrings); override;
	function ReadString(const Section, Ident, Default: string): string; override;
	procedure SetStrings(List: TStrings);
	procedure UpdateFile; override; //Deve salvar de acordo com Encrypted
	procedure WriteString(const Section, Ident, Value: string); override;
  end;

  TWinSignatureIni = class(TCryptedIni)
  private
	FChecked:      boolean;
	Updated:       boolean;
	FLazyWrite:    boolean;
	FOpenReadOnly: boolean;
	SignatureRead: string;
	VersionRead:   string;
	function GetSignatureString: string;
	procedure ReadOnlyError();
	function GetChecked: boolean;
	procedure UncheckedError();
	procedure SetOpenReadOnly(const Value: boolean);
	function GetVersion: string;
  protected
	procedure ReadFromFile(const IniFileName: string; List: TStrings); override;
  public
	property Checked:         boolean read GetChecked;
	property FileVersion:     string read VersionRead;
	property LazyWrite:       boolean read FLazyWrite write FLazyWrite;
	property OpenReadOnly:    boolean read FOpenReadOnly write SetOpenReadOnly;
	property SignatureString: string read GetSignatureString;
	property Version:         string read GetVersion;
	constructor Create(const FileName: string); override;
	destructor Destroy; override;
	procedure UpdateFile(); override;
	procedure WriteString(const Section, Ident, Value: string); override;
	procedure EraseSection(const Section: string); override;
	procedure DeleteKey(const Section, Ident: string); override;
	procedure ReadSection(const Section: string; Strings: TStrings); override;
	procedure ReadSections(Strings: TStrings); override;
	procedure ReadSectionValues(const Section: string; Strings: TStrings); override;
	function ReadString(const Section, Ident, Default: string): string; override;
	procedure SetStrings(List: TStrings);
  end;

  EWinSignatureIni = class(Exception);

type
  TCypher = class
  private
	FKey: string;
  public
	constructor Create(const AKey: string);
	function Encode(Txt: string): string;
	function Decode(const pCode: string): string;
	function BinDecode(const pCode: string): string;
	property Key: string read FKey write FKey;
  end;

implementation

uses
  Cripto, Math, Str_Pas, MathHnd, Binhnd, StrHnd;

const
  VALIDATOR_CURRENT_VERSION = '01'; //Prefixo da versao da validacao
  _CRYPTO_KEY_              = 'radler';
  TOKEN_SIGNATURE           = ';ASSINATURA=';
  NULL_SIGNATURE            = VALIDATOR_CURRENT_VERSION + '-0000-0000-0000-0000';

  { TCriptoIni }

function TCryptedIni.AddSection(const Section: string): TStrings;
//----------------------------------------------------------------------------------------------------------------------
begin
  Result := TStringList.Create;
  try
	FSections.AddObject(Section, Result);
  except
	Result.Free;
  end;
end;

procedure TCryptedIni.Clear;
//----------------------------------------------------------------------------------------------------------------------
var
  i: integer;
begin
  for i := 0 to FSections.Count - 1 do begin
	TStrings(FSections.Objects[i]).Free;
  end;
  FSections.Clear;
end;

constructor TCryptedIni.Create(const FileName: string);
//----------------------------------------------------------------------------------------------------------------------
//NOTAS : Valor de FEncrypted DEVE SER DEFINIDO pelos descendentes qdo construcao antes da chamada a inherited Create
//Ex.:
//Self.FEncrypted:=true;
//inherited;
//Comeca com False para ser compativel com a maioria dos ancrestrais/usos
begin
  inherited;
  Self.FEncrypted := False;
  FSections := TStringList.Create;
  LoadValues;
end;

procedure TCryptedIni.DeleteKey(const Section, Ident: string);
//----------------------------------------------------------------------------------------------------------------------
var
  i, j:    integer;
  Strings: TStrings;
begin
  i := FSections.IndexOf(Section);
  if i >= 0 then begin
	Strings := TStrings(FSections.Objects[i]);
	j := Strings.IndexOfName(Ident);
	if j >= 0 then begin
	  Strings.Delete(j);
	end;
  end;
end;

destructor TCryptedIni.Destroy;
//----------------------------------------------------------------------------------------------------------------------
begin
  if FSections <> nil then begin
	Clear;
  end;
  FSections.Free;
  inherited;
end;

procedure TCryptedIni.EraseSection(const Section: string);
//----------------------------------------------------------------------------------------------------------------------
var
  i: integer;
begin
  i := FSections.IndexOf(Section);
  if i >= 0 then begin
	TStrings(FSections.Objects[i]).Free;
	FSections.Delete(i);
  end;
end;

procedure TCryptedIni.GetStrings(List: TStrings);
//----------------------------------------------------------------------------------------------------------------------
var
  i, j:    integer;
  Strings: TStrings;
begin
  List.BeginUpdate;
  try
	for i := 0 to FSections.Count - 1 do begin
	  List.Add('[' + FSections[i] + ']');
	  Strings := TStrings(FSections.Objects[i]);
	  for j := 0 to Strings.Count - 1 do begin
		List.Add(Strings[j]);
	  end;
	  List.Add(EmptyStr);
	end;
  finally
	List.EndUpdate;
  end;
end;

procedure TCryptedIni.LoadValues;
//----------------------------------------------------------------------------------------------------------------------
var
  List: TStringList;
begin
  if (FileName <> EmptyStr) and FileExists(FileName) then begin
	List := TStringList.Create;
	try
	  Self.ReadFromFile(FileName, List);
	  SetStrings(List);
	finally
	  List.Free;
	end;
  end else begin
	Clear;
  end;
end;

procedure TCryptedIni.ReadFromFile(const IniFileName: string; List: TStrings);
//----------------------------------------------------------------------------------------------------------------------------------
var
  MS: TMemoryStream;
begin
  if Self.FEncrypted then begin
	MS := TMemoryStream.Create;
	try
	  MS.LoadFromFile(Self.FileName);
	  DecryptStream(MS, _CRYPTO_KEY_);
	  MS.Seek(soFromBeginning, 0);
	  List.LoadFromStream(MS);
	finally
	  MS.Free;
	end;
  end else begin
	List.LoadFromFile(FileName);
  end;
end;

procedure TCryptedIni.ReadSection(const Section: string; Strings: TStrings);
//----------------------------------------------------------------------------------------------------------------------
var
  i, j:           integer;
  SectionStrings: TStrings;
begin
  Strings.BeginUpdate;
  try
	Strings.Clear;
	i := FSections.IndexOf(Section);
	if i >= 0 then begin
	  SectionStrings := TStrings(FSections.Objects[i]);
	  for j := 0 to SectionStrings.Count - 1 do begin
		Strings.Add(SectionStrings.Names[j]);
	  end;
	end;
  finally
	Strings.EndUpdate;
  end;
end;

procedure TCryptedIni.ReadSections(Strings: TStrings);
//----------------------------------------------------------------------------------------------------------------------
begin
  Strings.Assign(FSections);
end;

procedure TCryptedIni.ReadSectionValues(const Section: string; Strings: TStrings);
//----------------------------------------------------------------------------------------------------------------------
var
  i: integer;
begin
  Strings.BeginUpdate;
  try
	Strings.Clear;
	i := FSections.IndexOf(Section);
	if i >= 0 then begin
	  Strings.Assign(TStrings(FSections.Objects[i]));
	end;
  finally
	Strings.EndUpdate;
  end;
end;

function TCryptedIni.ReadString(const Section, Ident, Default: string): string;
//----------------------------------------------------------------------------------------------------------------------
var
  i:       integer;
  Strings: TStrings;
begin
  i := FSections.IndexOf(Section);
  if i >= 0 then begin
	Strings := TStrings(FSections.Objects[i]);
	i := Strings.IndexOfName(Ident);
	if i >= 0 then begin
	  Result := Copy(Strings[i], Length(Ident) + 2, Maxint);
	  Exit;
	end;
  end;
  Result := default;
end;

procedure TCryptedIni.SetEncrypted(const Value: boolean);
//----------------------------------------------------------------------------------------------------------------------
//Alterar o conteudo do arquivo apenas
begin
  if Self.FEncrypted <> Value then begin
	FEncrypted := Value; //Altera estado interno e atualiza arquivo
	Self.UpdateFile;
  end;
end;

procedure TCryptedIni.SetStrings(List: TStrings);
//----------------------------------------------------------------------------------------------------------------------
//Conteudo de List esta de acordo com Encrypted. Este metodo sera chamado por LoadValues
var
  i:       integer;
  s:       string;
  Strings: TStrings;
begin
  //Parte onde tudo acontece como em TMenIniFile
  Clear;
  Strings := nil;
  for i := 0 to List.Count - 1 do begin
	s := List[i];
	if (s <> EmptyStr) and (s[1] <> ';') then begin //Comentario
	  if (s[1] = '[') and (s[Length(s)] = ']') then begin
		Strings := AddSection(Copy(s, 2, Length(s) - 2));
	  end else begin
		if Strings <> nil then begin
		  Strings.Add(s);
		end;
	  end;
	end;
  end;
end;

procedure TCryptedIni.UpdateFile;
//----------------------------------------------------------------------------------------------------------------------
//Deve salvar de acordo com Encrypted
var
  List: TStringList;
  MS:   TMemoryStream;
begin
  List := TStringList.Create;
  try
	Self.GetStrings(List);
	if Self.FEncrypted then begin
	  MS := TMemoryStream.Create;
	  try
		List.SaveToStream(MS);
		EncryptStream(MS, _CRYPTO_KEY_);
		MS.SaveToFile(Self.FileName);
	  finally
		MS.Free;
	  end;
	end else begin //Clear Text
	  List.SaveToFile(Self.FileName);
	end;
  finally
	List.Free;
  end;
end;

procedure TCryptedIni.WriteString(const Section, Ident, Value: string);
//----------------------------------------------------------------------------------------------------------------------
var
  i:       integer;
  s:       string;
  Strings: TStrings;
begin
  i := FSections.IndexOf(Section);
  if i >= 0 then begin
	Strings := TStrings(FSections.Objects[i]);
  end else begin
	Strings := AddSection(Section);
  end;
  s := Ident + '=' + Value;
  i := Strings.IndexOfName(Ident);
  if i >= 0 then begin
	Strings[i] := s;
  end else begin
	Strings.Add(s);
  end;
end;

{ TWinSignatureIni }

constructor TWinSignatureIni.Create(const FileName: string);
//----------------------------------------------------------------------------------------------------------------------------------
begin
  Self.FEncrypted := False; //Para este tipo de arquivo nao ha criptografia
  inherited;
  if not FileExists(FileName) then begin
	Self.SignatureRead := NULL_SIGNATURE;
  end;
  Self.FChecked := (Self.SignatureString = Self.SignatureRead);
  Self.VersionRead := EmptyStr;
  Self.Updated := True; //Inicialmente nada escrito
  Self.FLazyWrite := False;
  Self.FOpenReadOnly := True;
end;

procedure TWinSignatureIni.DeleteKey(const Section, Ident: string);
//----------------------------------------------------------------------------------------------------------------------------------
begin
  if not Self.FOpenReadOnly then begin
	if not Checked then begin
	  UncheckedError;
	end;
	inherited;
	if Self.FLazyWrite then begin
	  Self.Updated := False;
	end else begin
	  Self.UpdateFile;
	end;
  end else begin
	ReadOnlyError();
  end;
end;

destructor TWinSignatureIni.Destroy;
//----------------------------------------------------------------------------------------------------------------------------------
begin
  if not Self.FOpenReadOnly then begin
	if not Self.Updated then begin
	  try
		Self.UpdateFile();
	  except
		on E: Exception do begin
		  if not(E is EWinSignatureIni) then begin //Ignora este tipo de erro (EWinSignatureIni)
			raise;
		  end;
		end;
	  end;
	end;
  end;
  inherited;
end;

procedure TWinSignatureIni.EraseSection(const Section: string);
//----------------------------------------------------------------------------------------------------------------------------------
begin
  if Self.FOpenReadOnly then begin
	ReadOnlyError;
  end else begin
	if not Checked then begin
	  UncheckedError;
	end;
	inherited;
	if FLazyWrite then begin
	  Updated := False;
	end else begin
	  UpdateFile;
	end;
  end;
end;

function TWinSignatureIni.GetChecked: boolean;
//----------------------------------------------------------------------------------------------------------------------------------
begin
  Result := Self.FChecked;
  if not Result then begin
	Result := (Self.SignatureRead = Self.SignatureString);
	Self.FChecked := Result;
  end;
end;

function TWinSignatureIni.GetSignatureString: string;
//----------------------------------------------------------------------------------------------------------------------------------
//Calcula magicamente o valor a ser escrito no final do arquivo

  function LSRCalcStringListHeigth(List: TStrings; MaxSize: integer): int64;
  //...........................................................................................................................
  var
	i, L:       integer;
	StrR, StrL: string;
  begin
	Result := 0;
	if List.Count >= 2 then begin
	  for i := 0 to List.Count - 2 do begin
		StrL := PadL(List.Strings[i], MaxSize, #170);
		StrR := PadR(List.Strings[i + 1], MaxSize, #170);
		for L := 1 to MaxSize do begin
		  Inc(Result, Ord(StrL[L]) + Ord(StrR[L]) + (Ord(StrL[L]) * Ord(StrR[L])) + (Ord(StrL[L]) xor Ord(StrR[L])));
		end;
	  end;
	end;
  end;

var
  //..................................................................................................................................
  PartSections, PartEntries, PartEntry_S, PartModule, CrStr: string;
  i, j, MaxSize: integer;
  Acc:                         int64;
  SectionItems:                TStrings;
  GroupSections, GroupEntries: TStringList;
begin
  GroupSections := TStringList.Create;
  try
	MaxSize := 0;
	//Calculo para secoes
	if Self.FSections.Count > 0 then begin
	  for i := 0 to Self.FSections.Count - 1 do begin
		CrStr := Self.FSections.Strings[i];
		GroupSections.Add(CrStr);
		MaxSize := Max(Length(CrStr), MaxSize);
	  end;
	  GroupSections.Add(GroupSections.Strings[0]); //Fecha ciclo novamente com a 1a secao
	  Acc := LSRCalcStringListHeigth(GroupSections, MaxSize);
	  PartSections := BaseNumToStr(Acc, 4, 36, False, '0');
	end else begin
	  Result := NULL_SIGNATURE;
	  Exit;
	end;

	//Calculo para Entradas e valores
	Acc := 0;
	GroupEntries := TStringList.Create;
	try
	  //Calculo para entradas+valores
	  for i := 0 to Self.FSections.Count - 1 do begin
		SectionItems := TStrings(Self.FSections.Objects[i]);
		if SectionItems.Count > 0 then begin
		  GroupEntries.Clear;
		  MaxSize := 0;
		  for j := 0 to SectionItems.Count - 1 do begin
			CrStr := SectionItems.Strings[j];
			GroupEntries.Add(CrStr);
			MaxSize := Max(Length(CrStr), MaxSize);
		  end;
		  GroupEntries.Add(SectionItems.Strings[0]); //Fecha ciclo com 1a entrada
		  Inc(Acc, LSRCalcStringListHeigth(GroupEntries, MaxSize));
		end;
	  end;
	  PartEntries := BaseNumToStr(Acc, 8, 36, False, '0');

	  //Calculo para valores
	  Acc := 0;
	  for i := 0 to Self.FSections.Count - 1 do begin
		SectionItems := TStrings(Self.FSections.Objects[i]);
		if SectionItems.Count > 0 then begin
		  GroupEntries.Clear;
		  MaxSize := 0;
		  for j := 0 to SectionItems.Count - 1 do begin
			CrStr := SectionItems.Values[SectionItems.Names[j]];
			GroupEntries.Add(CrStr);
			MaxSize := Max(Length(CrStr), MaxSize);
		  end;
		  GroupEntries.Add(SectionItems.Values[SectionItems.Names[0]]); //Fecha ciclo com 1o valor
		  Inc(Acc, LSRCalcStringListHeigth(GroupEntries, MaxSize));
		end;
	  end;
	  PartEntry_S := BaseNumToStr(Acc, 8, 36, False, '0');

	  //PartModule
	  GroupEntries.Clear;
	  GroupEntries.Add(PadL(PartSections, 255, #170));
	  GroupEntries.Add(PadR(PartEntries, 255, #170));
	  GroupEntries.Add(PadL(PartEntry_S, 255, #170));
	  Acc := LSRCalcStringListHeigth(GroupEntries, 255);
	  PartModule := BaseNumToStr(Acc, 4, 36, False, '0');
	finally
	  GroupEntries.Free;
	end;
	//Composicao final
	Result := StrConnectSubStrings('-', [VALIDATOR_CURRENT_VERSION, PartSections, PartEntries, PartEntry_S, PartModule]);
  finally
	GroupSections.Free;
  end;
end;

function TWinSignatureIni.GetVersion: string;
//----------------------------------------------------------------------------------------------------------------------------------
begin
  Result := VALIDATOR_CURRENT_VERSION;
end;

procedure TWinSignatureIni.ReadFromFile(const IniFileName: string; List: TStrings);
//----------------------------------------------------------------------------------------------------------------------------------
var
  MS:       TMemoryStream;
  LastLine: string;
begin
  SignatureRead := EmptyStr;
  VersionRead := '00'; //Invalida!!!
  if Self.FEncrypted then begin
	MS := TMemoryStream.Create;
	try
	  MS.LoadFromFile(Self.FileName);
	  DecryptStream(MS, _CRYPTO_KEY_);
	  MS.Seek(soFromBeginning, 0);
	  List.LoadFromStream(MS);
	finally
	  MS.Free;
	end;
  end else begin
	List.LoadFromFile(FileName);
  end;
  //Carregar valor da assinatura
  if List.Text <> EmptyStr then begin
	LastLine := List.Strings[List.Count - 1];
	if Pos(TOKEN_SIGNATURE, Trim(LastLine)) = 1 then begin
	  SignatureRead := Trim(Copy(LastLine, Pos('=', LastLine) + 1, Length(LastLine)));
	  VersionRead := Copy(SignatureRead, 1, 2);
	end;
  end else begin
	SignatureRead := EmptyStr;
  end;
end;

procedure TWinSignatureIni.ReadOnlyError();
//----------------------------------------------------------------------------------------------------------------------------------
begin
  raise EWinSignatureIni.CreateFmt('Arquivo "%s" aberto no modo somente leitura.', [Self.FileName]);
end;

procedure TWinSignatureIni.ReadSection(const Section: string; Strings: TStrings);
//----------------------------------------------------------------------------------------------------------------------------------
begin
  if not Checked then begin
	UncheckedError;
  end;
  inherited;
end;

procedure TWinSignatureIni.ReadSections(Strings: TStrings);
//----------------------------------------------------------------------------------------------------------------------------------
begin
  if not Checked then begin
	UncheckedError;
  end;
  inherited;
end;

procedure TWinSignatureIni.ReadSectionValues(const Section: string; Strings: TStrings);
//----------------------------------------------------------------------------------------------------------------------------------
begin
  if not Checked then begin
	UncheckedError;
  end;
  inherited;
end;

function TWinSignatureIni.ReadString(const Section, Ident, Default: string): string;
//----------------------------------------------------------------------------------------------------------------------------------
begin
  if not Checked then begin
	UncheckedError;
  end;
  Result := inherited ReadString(Section, Ident, default);
end;

procedure TWinSignatureIni.SetOpenReadOnly(const Value: boolean);
//----------------------------------------------------------------------------------------------------------------------------------
begin
  if (not Value) and FOpenReadOnly and (not Self.Updated) then begin //Garante a atualizacao antes
	Self.UpdateFile;
  end;
  FOpenReadOnly := Value;
end;

procedure TWinSignatureIni.SetStrings(List: TStrings);
//----------------------------------------------------------------------------------------------------------------------------------
begin
  if Self.OpenReadOnly then begin
	Self.ReadOnlyError();
  end;
  inherited;
  if not Self.LazyWrite then begin
	Self.SignatureRead := Self.SignatureString; //Atualiza para ser coerente no futuro
	Self.UpdateFile;
  end else begin
	Updated := False;
  end;
end;

procedure TWinSignatureIni.UncheckedError;
//----------------------------------------------------------------------------------------------------------------------------------
begin
  raise EWinSignatureIni.CreateFmt('Assinatura para o arquivo "%s" não é válida.', [Self.FileName]);
end;

procedure TWinSignatureIni.UpdateFile();
//----------------------------------------------------------------------------------------------------------------------------------
var
  FHnd:     Text;
  LastLine: string;
begin
  if not(Self.Updated) and Self.FOpenReadOnly then begin
	ReadOnlyError;
  end;
  if not Self.Checked then begin
	UncheckedError();
  end;
  inherited; //Salva todas as partes significantes do arquivo

  AssignFile(FHnd, Self.FileName);
  if FileExists(Self.FileName) then begin
	Append(FHnd);
  end else begin
	Rewrite(FHnd);
  end;
  try
	LastLine := TOKEN_SIGNATURE + Self.SignatureString;
	WriteLn(FHnd);
	WriteLn(FHnd, LastLine);
  finally
	Flush(FHnd);
	CloseFile(FHnd);
  end;
end;

procedure TWinSignatureIni.WriteString(const Section, Ident, Value: string);
//----------------------------------------------------------------------------------------------------------------------------------
begin
  if Self.FOpenReadOnly then begin
	Self.ReadOnlyError;
  end else begin
	if not Checked then begin
	  UncheckedError();
	end;
	inherited;
	if Self.FLazyWrite then begin
	  Self.Updated := False;
	end else begin
	  Self.UpdateFile;
	end;
  end;
end;

constructor TCypher.Create(const AKey: string);
{ {
  AKey - Chave deve ter mais que 3 e menos de 8 caracteres.
}
begin
  Self.FKey := AKey;
end;

function TCypher.Encode(Txt: string): string;
{ {
  Codifica a string Txt.

  Procedimento:
  divide a string a ser codificada em n blocos de caractees, onde n é um multiplo do
  tamanho da chave, em caso de falta completa-se com caracteres nulos (#0).
  para cada bloco  aplica-se XOR byte a byte com a chave, resultando uma nova string.
  converte-se o bloco para a base 36.
  O resultado será uma string codificada, onde ao fim de cada bloco conterá um espaço em
  branco.
}
var
  buffer:                                array of char;
  block:                                 string;
  i, startPos, endPos, KeyLen, InputLen: integer;
begin
  KeyLen := Length(Self.FKey);
  if (KeyLen <= 3) or (KeyLen > 7) then begin
	raise Exception.Create('Chave deve ter mais que 3 e menos de 8 caracteres.');
  end;
  SetLength(buffer, KeyLen);
  InputLen := Length(Txt);
  Result := EmptyStr;
  startPos := 1;
  endPos := KeyLen;
  while (startPos <= InputLen) do begin
	block := Copy(Txt, startPos, endPos);
	block := PadR(block, KeyLen, #0);
	for i := 1 to KeyLen do begin
	  buffer[i - 1] := char(Ord(Self.FKey[i]) xor Ord(block[i]));
	end;
	block := EmptyStr; //zera e le a partir de buffer
	for i := 0 to KeyLen - 1 do begin
	  block := block + Binhnd.Byte2Hexa(Ord(buffer[i]));
	end;
	block := MathHnd.BaseConvert(block, 16, 36);
	Result := Result + block + ' ';
	Inc(startPos, KeyLen);
	Inc(endPos, KeyLen);
  end;
end;

function TCypher.BinDecode(const pCode: string): string;
{ {
  Decodifica a string code de maneira binaria

  Revision: 3/2/2006 - Roger
}
var
  buffer:                array of char; //variavel que conterá os caracteres codificados
  block, par:            string; //segmento de txt em paginas de Len caracteres
  blockCount, z, KeyLen: integer;
begin
  if (pCode = EmptyStr) then begin
	raise Exception.Create('Valor a ser decodificado nulo.');
  end;
  KeyLen := Length(Self.FKey); //Tamanho da chave
  if (KeyLen <= 3) or (KeyLen > 7) then begin
	raise Exception.Create('Chave deve ter mais que 3 caracteres e menos de 8.');
	//levanta excecao caso a chave possua menos de 3 caracteres
  end;
  Result := EmptyStr;
  blockCount := 0;
  repeat
	block := Str_Pas.GetDelimitedSubStr(' ', pCode, blockCount);
	if (block <> EmptyStr) then begin
	  block := MathHnd.BaseConvert(block, 36, 16); //converte um bloco para hexadecimal
	  { completa com a quantidade necessaria de zeros a esquerda, para que fique com o dobro do tamanho da chave,
		uma vez que cada par hexadecimal ira ser comparado com um caracter da chave }
	  block := Str_Pas.PadL(block, (2 * KeyLen), '0');
	  for z := 0 to ((Length(block) div 2) - 1) do begin
		par := Copy(block, 2 * z + 1, 2);
		//obtem um par hexadecimal a partir do bloco, utilizando z como posicao inicial, uma vez que toda vez que o loop chegar a esse ponto, Z tera assumido um valor impar. isso se faz necessario para que seja obtido um par a partir do primeiro caracter do bloco
		SetLength(buffer, Length(buffer) + 1); //incrementa buffer de uma posicao
		buffer[high(buffer)] := char(Binhnd.StrHex2Byte(par));
		//insere na ultima posicao de buffer um caracter originado da conversao de um par hexadecimal para char.
	  end;
	  block := EmptyStr; //zera o bloco
	  for z := 0 to KeyLen - 1 do begin //monta agora um bloco com dessa vez de caracteres
		//incrementa o bloco com um caracter originado de um XOR entre um caracter de buffer e o seu caracter correspondente na chave
		block := block + char(Ord(Self.FKey[z + 1]) xor Ord(buffer[z]));
	  end;
	  SetLength(buffer, 0);
	  Result := Result + block; //incrementa o result com um bloco decodificado.
	end;
	Inc(blockCount);
  until (block = EmptyStr);
end;

function TCypher.Decode(const pCode: string): string;
{ {
  Decodifica a string e retorna até o limite do primeiro nulo ou o tamanho total do resultado.

  Revision: 3/2/2006 - Roger
}
var
  ret: string;
begin
  ret := Self.BinDecode(pCode);
  Result := PChar(ret); //Elimina os nulos finais
end;

end.
