{$IFDEF StreamHnd}
{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I StFLib.inc}
unit StreamHnd;

interface

uses
	Classes, SysUtils, Windows, FileHnd;

type
	TFileCompareAttributes = set of (fcaAccessDate, fcaCreateDate, fcaModifyDate, fcaSize, fcaHash);

const
	FCA_ALL_ATTRIBUTES = [fcaAccessDate, fcaCreateDate, fcaModifyDate, fcaSize, fcaHash];

type
	TStreamHnd = class
	public
		class function ReadStream(SrcStream, DestStream: TStream; HowMany: int64): int64;
	end;

	THashHnd = class
	public
		class function CompareFiles(const Filename1, Filename2: string; Attributes: TFileCompareAttributes; ShortEval: boolean)
			: TFileCompareAttributes; overload;
		class function CompareFiles(const Filename1, Filename2: string): TFileCompareAttributes; overload;
		class function MD5(const fileName: string): string; overload;
		class function MD5(const strm: TStream): string; overload;
		class function MD5(const strm: TStream; Length: int64): string; overload;
	end;

implementation

uses IdHashMessageDigest, idHash, APIHnd;

class function TStreamHnd.ReadStream(SrcStream, DestStream: TStream; HowMany: int64): int64;
{ {
  HowMany (-1) -> Todo o restante do stream de origem sera repassado para o de destino
}
var
	Buffer : Pointer;
	BufSize: int64;
begin
	if (HowMany >= 0) then begin
		BufSize := HowMany;
	end else begin
		BufSize := SrcStream.Size - SrcStream.Position;
	end;
	Buffer := GetMemory(BufSize);
	try
		{$WARN UNSAFE_CODE OFF}
		Result := SrcStream.Read(Buffer^, BufSize);
		DestStream.Write(Buffer^, BufSize);
		{$WARN UNSAFE_CODE ON}
	finally
		FreeMemory(Buffer);
	end;
end;

{ TRedundancyCheck }

class function THashHnd.MD5(const fileName: string): string;
///<summary>
///Calcula o MD5 do arquivo passado
///</summary>
///<remarks>
///Rotina usa lib da indy
///</remarks>
var
	fs: TFileStream;
begin
	fs := TFileStream.Create(fileName, fmOpenRead or fmShareDenyWrite);
	try
		Result := MD5(fs);
	finally
		fs.Free;
	end;
end;

class function THashHnd.CompareFiles(const Filename1, Filename2: string; Attributes: TFileCompareAttributes; ShortEval: boolean)
	: TFileCompareAttributes;
///<summary>
///Compara os arquivos passados, considerando apenas os atributos informados em Attributes.
///ShortEval indica que assim que um atributo divergente seja encontrado o método retorna
///<returns>
///</returns>
///Conjunto de atributos divergentes encontrados. Esta lista pode ser incompleta, caso ShortEval seja true
///</summary>
///<remarks>
///Hash calculado por MD5
///</remarks>
var
	createDate1, acessDate1, modDate1: TDateTime;
	createDate2, acessDate2, modDate2: TDateTime;
	h1, h2                           : string;
	ret                              : Integer;
begin
	Result := []; //nenhum atributo divergente

	if (Attributes * [fcaAccessDate, fcaCreateDate, fcaModifyDate] <> []) then begin //datas envolvidas

		try
			//Datas arquivo 1
			ret := TFileHnd.FileTimeProperties(Filename1, createDate1, acessDate1, modDate1);
			TAPIHnd.CheckAPI(ret);
			//Datas arquivo 2
			ret := TFileHnd.FileTimeProperties(Filename2, createDate2, acessDate2, modDate2);
			TAPIHnd.CheckAPI(ret);
		except
			raise Exception.CreateFmt('Falha coletando atributos dos arquivos(%s, %s)', [Filename1, Filename2]);
		end;

		//Teste de data de acesso
		if ((fcaAccessDate in Attributes) and (acessDate1 <> acessDate2)) then begin
			Result := Result + [fcaAccessDate];
		end;
		//Teste de data de criação
		if ((fcaCreateDate in Attributes) and (createDate1 <> createDate2)) then begin
			Result := Result + [fcaCreateDate];
		end;
		//Teste de data de modificação
		if ((fcaModifyDate in Attributes) and (modDate1 <> modDate2)) then begin
			Result := Result + [fcaModifyDate];
		end;
		//encurta a avaliação dos demais atributos se ShortEval
		if (ShortEval and (Result <> [])) then begin
			Exit;
		end;
	end;

	//teste de tamanho
	if (fcaSize in Attributes) then begin
		if (TFileHnd.GetFileSizeEx(Filename1) <> TFileHnd.GetFileSizeEx(Filename2)) then begin
			Result := Result + [fcaSize, fcaHash]; //Hash incluso pela propria condição de tamanhos diferentes
			if (ShortEval) then begin
				Exit;
			end;
		end;
	end;

	//teste de hash o mais extenso de ser realizado
	if (fcaHash in Attributes) then begin
		h1 := THashHnd.MD5(Filename1);
		h2 := THashHnd.MD5(Filename2);
		if (h1 <> h2) then begin
			Result := Result + [fcaHash];
			if (ShortEval) then begin
				begin
					Exit;
				end;
			end;
		end;
	end;
end;

class function THashHnd.CompareFiles(const Filename1, Filename2: string): TFileCompareAttributes;
///<summary>
///Retorna os atributos divergentes dos arquivos passados, ignorando fcaAccessDate e fcaCreateDate
///</summary>
///<remarks>
///
///</remarks>
begin
	Result := THashHnd.CompareFiles(Filename1, Filename2, FCA_ALL_ATTRIBUTES - [fcaAccessDate, fcaCreateDate], False);
end;

class function THashHnd.MD5(const strm: TStream; Length: int64): string;
///<summary>
///Calcula o MD5 do stream passado até a quantidade de bytes dado por Length
///</summary>
///<remarks>
///Rotina usa lib da indy
///</remarks>
var
	idmd5: TIdHashMessageDigest5;
begin
	{$IF CompilerVersion = 21.00}
	if ((Length mod 2048) <> 0) then begin
		//Para a Indy presente no RAD 21(D2010) um bug impede qq tamanho de bloco
		raise Exception.Create
			('Implementação de hash DEVE ser calculada em multiplos de 2048 para esta versão(ver notas de codificação)');
	end;
	{$ELSE}
	{$MESSAGE 'REVER BLOCO DE CÓDIGO PRÓXIMO'}
	{$ENDIF}
	idmd5 := TIdHashMessageDigest5.Create;
	try
		Result := idmd5.HashStreamAsHex(strm, strm.Position, Length);
	finally
		idmd5.Free;
	end;
end;

class function THashHnd.MD5(const strm: TStream): string;
///<summary>
///Calcula o MD5 do stream passado, de seu início ao seu fim
///</summary>
///<remarks>
///Rotina usa lib da indy
///</remarks>
var
	idmd5: TIdHashMessageDigest5;
begin
	strm.Seek(0, soBeginning);
	idmd5 := TIdHashMessageDigest5.Create;
	try
		Result := idmd5.HashStreamAsHex(strm);
	finally
		idmd5.Free;
	end;
end;

end.
