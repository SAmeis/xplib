{$IFDEF FileHnd}
{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I XPLib.inc}
unit FileHnd;

{ {
  Implementa classes e rotinas para tratamento de arquivos.

  Revision: 5/6/2008 - roger
  1 - Trocadas as refer�ncias a '\" pela vari�vel SysUtils.PathDelim
  2 - Atribui��o correta de retorno para rotina FileCopy

  Revision: 10/8/2006 - Roger
  Rotina FileTimeToDateTime foi deslocada para TFileHnd.FileTimeToDateTime.
  Rotinas(overloaded) FileTimeProperties foram deslocadas para TFileHnd.FileTimeToDateTime.

}

interface

{ TODO -oroger -clib : Avaliar uso das classes em System.IOUtils }
uses Windows, Classes, SysUtils;

const
	///<summary> Conjunto de todos os atributos dos arquivos. Lembrar que FILE_ATTRIBUTE_NORMAL mutuamente exclusivo -> nenhum atributo existente
	///</summary>
	faAllFiles = FILE_ATTRIBUTE_ARCHIVE or FILE_ATTRIBUTE_COMPRESSED or FILE_ATTRIBUTE_DIRECTORY or FILE_ATTRIBUTE_HIDDEN or
		FILE_ATTRIBUTE_NORMAL or FILE_ATTRIBUTE_OFFLINE or FILE_ATTRIBUTE_READONLY or FILE_ATTRIBUTE_SYSTEM or
		FILE_ATTRIBUTE_TEMPORARY;
	//faAllFiles =   faReadOnly or  faHidden or faSysFile or faVolumeID or faDirectory or faArchive;

	//Uso para validacao de nomes de arquivos, usar as constantes colocadas abaixo
	INVALID_FILE_NAME_CHARS1_STR = '*?"\/<>	:{}[]'; //tab inside
	{ 1 constante deprecated usar familia INVALID_FILENAME_CHARS_????_CRITIC }
	INVALID_FILE_NAME_CHARS2_STR = INVALID_FILE_NAME_CHARS1_STR + '+&�������������������'; //acentos
	{ 1 constante deprecated usar familia INVALID_FILENAME_CHARS_????_CRITIC }
	INVALID_FILE_NAME_CHARS3_STR = INVALID_FILE_NAME_CHARS2_STR + '- &'; //unix
	{ 1 constante deprecated usar familia INVALID_FILENAME_CHARS_????_CRITIC }

	//Uso para valida��o de nomes de arquivos, ***SUPLANTA*** as constantes acima.
	INVALID_FILENAME_CANONIC_CHARS: TSysCharSet = ['*', '?', '"', '\', '/', '<', '>', #9 { tab } , ':', '{', '}', '[', ']', '�'];
	{ 1 Caracteres usados como tokens nos sistemas operacionais em geral }
	INVALID_FILENAME_LINUX_EXT: TSysCharSet = ['-', ' ' { espaco } , '&'];
	{ 1 Caracteres usados como tokens no linux que excedem os encontrados em  INVALID_FILENAME_CANONIC_CHARS }
	INVALID_FILENAME_CHARS_HIGH_CRITIC: TSysCharSet = [#0 .. #32] + [#128 .. #255] + ['*', '?', '"', '\', '/', '<', '>',
		#9 { tab } , ':', '{', '}', '[', ']', '�'] { INVALID_FILENAME_CANONIC_CHARS } + ['-', ' ' { espaco } , '&']
	{ INVALID_FILENAME_LINUX_EXT };
	{ 1 Conjunto de caracteres que podem ser usados nos sistemas operacionais alvos de nossos aplicativos }
	INVALID_FILENAME_CHARS_MEDIUM_CRITIC: TSysCharSet = [#0 .. #32] + [#128 .. #255] + ['*', '?', '"', '\', '/', '<', '>',
		#9 { tab } , ':', '{', '}', '[', ']', '�'] { INVALID_FILENAME_CANONIC_CHARS };
	{ 1 Conjunto de caracteres que podem ser usados sem muito risco no Windows usando apenas o ASCII abaixo de #127 }
	INVALID_FILENAME_CHARS_LOW_CRITIC: TSysCharSet = [#0 .. #32] + ['*', '?', '"', '\', '/', '<', '>', #9 { tab } , ':', '{', '}',
		'[', ']', '�'] { INVALID_FILENAME_CANONIC_CHARS };
	{ 1 Conjunto de caracteres que podem ser usados com algum risco no Windows usando too o ASCII }

	IDEAL_FILE_BLOCK_SIZE = 4096; //blocos de 4K parecem ser hoje o melhor valor

	EOL_DOS = #13#10;

type
	///<summary>
	///Class for repass to Enumeration callBacks, instances of TEnumFileParam contains information about the current file.
	///</summary>
	TEnumFileParam = class(TObject)
	private
		FErrorMsg: PChar;
		FFileName: PChar;
		procedure SetErrorMsg(const Value: PChar);
		procedure SetFileName(const Value: PChar);
	public
		Data     : Pointer;
		ErrorCode: Integer;
		SR       : ^TSearchRec;
		constructor Create;
		destructor Destroy; override;
		property ErrorMsg: PChar read FErrorMsg write SetErrorMsg;
		property FileName: PChar read FFileName write SetFileName;
	end;

	TEnumFileProc = function(const FileName: string; FileParam: TEnumFileParam): Integer;

	//Apaga o arquivo independente de seus atributos
function __DeleteFile(const FileName: string): boolean; platform; deprecated;
//Verifica se arquivo existe, extendido para sub-diretorios
function __FileExists(const Name: string): boolean; platform;
///<summary>
///Abre arquivo com valor para auto-incremento
///para ler erro ver LasErrorCodeAbre arquivo com valor para auto-incremento para ler erro ver LasErrorCode
///</summary>
function AddKeyGenFile(const FileName: string; TimeOut: Integer): Integer;
///<summary>
///Junta as parte de um caminho ver TFileHnd
///</summary>
function ConcatPath(Paths: array of string): string; deprecated;
//Conta a quantidade de arquivos que obedecem uma dada mascara
function CountMaskFilesMatch(const PathMask: string): Integer; platform;
//Cria um nome de arquivo temporario
function CreateTempFileName(const Prefix: PChar; const Number: byte): string;
//Apaga arquivos com uma dada mascara
function DeleteMaskedFiles(const MaskFiles: string): boolean; platform;
//Apaga a pasta e seu conteudo
function Deltree(const Path: string; IncludeSelf: boolean = True; IgnoreErrors: boolean = True; IgnoreReadOnly: boolean = True;
	IgnoreSystem: boolean = True): Integer;
//Preenche Lista com os arquivos do path passado, omite sub-dirs
function DirFileList(List: TStrings; const Dir: string; Masks: array of string): Integer;
//Retorna uma lista com strings relativas ao diretorio passado
function DirectoryList(const DirMask: string; FList: TStringList): TStringList; platform;

//Chama um callback para todos os arquivos abaixo do diretorio dado enquanto o retorno for 0
function EnumFiles(const Directory, Mask: string; FileParam: TEnumFileParam; FileProc: TEnumFileProc): Integer; overload;
//Chama um callback para todos os arquivos abaixo do diretorio dado enquanto o retorno for 0
function EnumFiles(const Directory: string; MaskList: TList; FileParam: TEnumFileParam; FileProc: TEnumFileProc): Integer; overload;
//Chama um callback para todos os arquivos abaixo do diretorio dado enquanto o retorno for 0
function EnumFiles(const Directory: string; MaskList: TList; IncludeAttrs, ExcludeAttrs: Integer; FileParam: TEnumFileParam;
	FileProc: TEnumFileProc): Integer; overload;
//Chama um callback para todos os arquivos abaixo do diretorio dado enquanto o retorno for 0
function EnumFiles(const Directory, Mask: string; IncludeAttrs, ExcludeAttrs: Integer; FileParam: TEnumFileParam;
	FileProc: TEnumFileProc): Integer; platform; overload;

//Chama um callback para todos os arquivos do diretorio dado enquanto o retorno for 0
function EnumFilesDir(const Directory, InitialMask: string; FileParam: TEnumFileParam; FileProc: TEnumFileProc): Integer;
//Avalia um caminho de modo a substituir %ENV_VARS% pelos seus valores e caminhos relativos "." e ".."( inclusive UNC's )
function EvalPathName(const Path: string): string;
//Extrai a unidade do caminho do arquivo
function ExtractFileDrive(const FileName: string): string;
//Extrai a extensao sem o ponto final de um arquivo
function ExtractFileExtension(const FileName: string): string; deprecated;
//extrai a parte relativa ao recurso de um unc
function ExtractUNCResource(const UNC: string): string;
//Extrai a raiz do recurso especificada
function ExtractRootResource(const ResourceName: string): string;
//Copia um arquivo para outro lugar //******************** TIPO DE RETORNO ALTERADO EM 08/01/1999 ****************************
function FileCopy(const Source, Dest: string; Over: boolean): Integer; platform;
//Copia um arquivo para outro lugar criando os respectivos diretorios ate o destino} //******************** TIPO DE RETORNO ALTERADO EM 08/01/1999 ***************************
function FileCopyEx(const Source, Dest: string; Over: boolean): Integer; platform;
//Copia arquivos de uma dada mascara para um dado diretorio
function FileCopyMaskDir(const MaskSourceDir, DestDir: string; Over, IncHiddens: boolean): boolean; platform;
//Move arquivos de uma dada mascara para um dado diretorio
function FileMoveMaskDir(const MaskSourceDir, DestDir: string; Over, IncHiddens, IncSystem: boolean): Integer; platform;
//Retorna string com todos os caractares invalidos do nome do arquivo, nao funciona para path
function FileNameInvalidChars(Strictness: Integer; const FileName: string): string;
//Seta os atributos para os arquivos que obedecem a mascara passada
function FileMaskSetAttribs(const FileMask: string; Attr: Integer): Integer; platform;
//Retorna o tamanho de um arquivo
function FileSize_(const FileName: string): longint;
//Retorna o nome curto aceito para aquele arquivo incluindo o caminho
function FileShortName(LongFileName: string): string;
//Conta quantas linha o arquivo possui
function FileTextCountLines(const FileName: string): longint;
//Conta quantas vezes a substr aparece dentro do arquivo
function FileTextCountSubStr(const FileName, SubStr: string): longint;
//Copia todos os arquivos da arvore fonte para a arvore destino
function FileXCopy(const Source, Dest: string; Over, IncHiddens: boolean): boolean; platform;
//Retorna o primeiro diretorio filho do diretorio dado
function FindFirstChildDir(const DirName: string): string;
//Retorna o primeiro arquivo filho de um subdiretorio
function FindFirstChildFile(const DirName: string): string;
//Retorna lista de drives removiveis
function GetFloppiesList: string;
//Retorna o tamanho do arquivo
function GetFileSize(const FileName: string): int64; platform; deprecated;
//Le em arquivo de usos exclusivo valor limitado ao periodo de espera
function GetKeyGenFileValue(const FileName: string; var Value, TimeOut: Integer): Integer;
//Retorna o caminho do diretorio de arquivos temporarios
function GetTempDir: string;
//Verifica se string dada pode ser um UNC valido
function IsUNCName(const UNC: string): boolean;
//Determina se o caminho dado e raiz de algum recurso
function IsRootDir(const Name: string): boolean;
//Lista todos os arquivos de uma pasta
procedure ListDirFilesNames(Dir, Mask: string; Attr: Integer; IncSubDirs: boolean; List: TStrings); platform;
//Retorna nome do arquivo compativel com o formato 8.3
function LongToShortFileName(const LongName: string): string; platform;
//Copia um arquivo para outro em uma rede peba
function NetFileCopy(const Source, Dest: string; Over: boolean): boolean; platform;
//Cria arvore dada se esta nao existir ainda
function MkDirEx(Dir: string): boolean; deprecated;
//Retorna o diretorio pai do caminho dado
function ParentDir(const Dir: string): string; deprecated;
//Retorna nome do Path no formato longo
function PathNameLong(const ShortPathName: string): string;
//Remove a extensao do nome do arquivo
function RemoveFileExtension(const FileName: string): string;
//RmDirEX : apaga um diretorio com seus subdiretoris junto
function RmDirEx(const DirName: string): boolean; platform; deprecated;
//Ajusta as datas de criacao/acesso/escrita de um dado arquivo
function SetFileTimeProperties(const FileName: string; var CreateDate, LastAccessDate, LastWriteDate: TDateTime): Integer;
	overload; platform;
//Ajusta as datas de criacao/acesso/escrita de um dado arquivo Notas : O arquivo deve ter a flag GENERIC_READ Exemplo :
function SetFileTimeProperties(FileHandle: THandle; var CreateDate, LastAccessDate, LastWriteDate: TDateTime): Integer; overload;
//Salvar em arquivo de usos exclusivo valor limitado ao periodo de espera
function SetKeyGenFileValue(const FileName: string; Value, TimeOut: Integer): Integer;
//Retorna o nome longo de um arquivo passado como nome curto
function ShortToLongFileName(const ShortName: string): string;
//Remove barra do fim do caminho
function SlashRem(const Path: string; PreserveRoot: boolean = False): string; deprecated;
//Formata corretamente o fullpath de um arquivo
function SlashSep(const Path, FileName: string): string;
//Retorna o espaco em bytes livres para determinado caminho
function SpaceFree(const PathName: string): int64;

type
	///<summary>
	///Class for file related manipulation.
	///</summary>
	TFileHnd = class(TObject)
	public
		///<summary>
		///Repassa as componentes da vers�o do arquivo dado.
		///</summary>
		///<param name="FileName">Nome de arquivo de interesse. Caso string vazia seja passa o caminho de ParamStr(0) sera usado.</param>
		///<param name="V1">1 componente da vers�o</param> <param name="V2">2 componente da vers�o</param>
		///<param name="V3">3 componente da vers�o</param> <param name="V4">4 componente da vers�o</param>
		///<remarks>
		///Para o caso do arquivo passado n�o possuir informa��o de vers�o todos os elementos ser�o retornados como 0 (zero).
		///Revision: 30/10/2006 - Roger
		///</remarks>
		class procedure BuildVersionInfo(const FileName: string; var V1, V2, V3, V4: Word);
		///<summary>
		///Retorna c caminho completo para o nome de arquivo baseado no nome original.
		///Ex.: ChangeFileName( 'c:\teste.pqp', '\windows\temp\novo.txt' ) -> c:\windows\temp\novo.txt
		///</summary>
		///<param name="OriginalFullPath"></param>
		///<param name="FileName"></param>
		///<returns></returns>
		class function ChangeFileName(const OriginalFullPath, FileName: string): string;
		///<summary>
		///Junta as parte da esquerda para a direita de um caminho. Esta rotina � �til para evitar as duplas \\ na composicao do nome do
		///arquivo.
		///</summary>
		///<param name="Paths"> array com os elementos que compoem o nome do arquivo.</param>
		///<returns></returns>
		class function ConcatPath(Paths: array of string): string;
		///<summary>
		///Copia um diret�rio recursivamente para outro.
		///</summary>
		///<param name="SourceDir">Fonte</param>
		///<param name="DestDir">Destino</param>
		///<returns></returns>
		class function CopyDir(const SourceDir, DestDir: TFilename): Integer;
		///<summary>
		///Copia os arquivos.
		///</summary>
		///<param name="SourceFile">Nome do arquivo fonte</param>
		///<param name="DestFile">Nome do arquivo destino</param>
		///<param name="Overwrite">Flag de sobrescrita. Caso falso e destino exista, ocorre erro</param>
		///<param name="ResetProtections">Remove atributos de prote��o do destino que impe�am a sobrescrita</param>
		///<returns>sucesso = ERRROR_SUCCESS, para falhar o error code correlato</returns>
		class function CopyFile(const SourceFile, DestFile: TFilename; Overwrite, ResetProtections: boolean): Integer;
		///<summary>
		///Gera nome de arquivo temporario pela Win32 API.
		///</summary>
		///<param name="Prefix">Parte inicial do nome do arquivo.</param>
		///<param name="Number">Indice preferencial ver Win32 API GetTempFileName().</param>
		///<param name="CreateFile">Indica se este arquivo sera automaticamente criado vazio.</param>
		///<returns>Nome do arquivo temporario para uso.</returns>
		class function CreateTempFileName(const Prefix: PChar; const Number: byte; CreateFile: boolean): string;
		///<summary>
		///Retorna caminho completo ap�s expans�o relativa de BasePath em Path
		///</summary>
		///<param name="BasePath">Caminho base para o calculo do caminho expandido.</param>
		///<param name="Path">Caminho a ser expandido.</param>
		///<returns>Caminho expandido do path passado</returns>
		class function ExpandFilename(const BasePath, Path: string): string;
		///<summary>
		///Returns the size of file, or -1 if fail.
		///Call GetLastError() to see extended error information.
		///WARNING : Its a platform method.
		///</summary>
		///<param name="FileName"></param>
		///<returns>Size em bytes</returns>
		///<remarks>
		///Para o caso de se calcular o tamanho usado por um diret�rio usar GetFileSizeEx()
		///</remarks>
		class function FileSize(const FileName: string): int64;
		///<summary>
		///Retorna caminho completo de arquivo obedecendo os criterios de path e mask, ou vazio se nada ocorrer
		///</summary>
		///<param name="Path">Caminho para a busca</param>
		///<param name="Mask">M�scara da busca</param>
		///<returns>Nome completo do arquivo encontrado que atenda ao pedido</returns>
		class function FirstOccurrence(const Path, Mask: string): string;
		///<summary>
		///Garante que o nome do arquivo finalizar� com a extens�o desejada
		///</summary>
		///<param name="OriginalName">nome informado(geralmente pelo usu�rio)</param>
		///<param name="DesiredExtension">extens�o desejada</param>
		///<returns>Nome completo com a extens�o desejada</returns>
		///<remarks>
		///Ver tamb�m:
		///TFileHnd.MakeDefaultFileExtension
		///SysUtils.ChangeFileExt()
		///</remarks>
		class function ForceFileExtension(const OriginalName, DesiredExtension: string): string;
		///<summary>
		///Garante a existencia de um arquivo com o nome especificado.
		///Para o caso do arquivo n�o exisitir ele se�ra criado vazio, bem como a �rvore de diretorios necessarios.
		///</summary>
		///<param name="FileName">nome do arquivo desejado.</param>
		class procedure ForceFilename(const FileName: string);
		///<summary>
		///Retorna o caminho mais profundo v�lido. Caso n�o exista nem mesmo o raiz do caminho passado o home_dir � retornado.
		///</summary>
		///<param name="Path">M�ximo caminho desejado</param>
		///<returns>M�ximo caminho criado, geralmente o mesmo passado, a menos que n�o se possa cria-lo</returns>
		class function DeepExistingPath(const Path: string): string;
		///<summary>
		///Extrai apenas o nome do arquivo desconsiderando inclusive a extens�o do mesmo
		///</summary>
		///<param name="FileName">Caminho, tanto faz se completo ou parial</param>
		///<returns>Nome do arquivo sem todos os acess�rios</returns>
		class function ExtractFilenamePure(const FileName: string): string;
		///<summary>
		///Retorna o caminho do Homedir do usu�rio do processo.
		///</summary>
		///<remarks>
		///Este valor � lido inicialmente iniciamente pela variavel de embiente USERPROFILE,
		///Alternativamente por HOMEDRIVE + HOMEPATH
		///caso n�o exista usa-se o caminho apontado pelo shell
		///</remarks>
		class function GetUserHomeDir(): string;
		///<summary>
		///Retorna o caminho para a pasta "Meu documentos" do usu�rio chamador
		///</summary>
		///<remarks>
		///Caso falhe retorna EmptytStr
		///</remarks>
		class function GetUserMyDocuments(): string;
		///<summary>
		///Calcula o tamanho do arquivo, caso o arquivo seja um diret�rio o c�lculo ser� recursivo
		///</summary>
		///<param name="FileName">Objeto a ter seu tamanho calculado, pode ser diret�rio</param>
		///<returns>Tamanho total de bytes usados pelo nome passado</returns>
		class function GetFileSizeEx(const FileName: string): int64;
		///<summary>
		///Retorna nome de arquivo com a extensao default se esta n�o tiver sido especificada no proprio nome do arquivo
		///Nenhuma critica � feita com o nome resultante.
		///</summary>
		///<param name="OriginalFileName">Nome original</param>
		///<param name="DefaultExtension">Extens�o desejada</param>
		///<returns>Nome de arquivo baseado em OriginalFilename com a extens�o desejada</returns>
		///<remarks>
		///Ver tamb�m:
		///TFileHnd.ForceFileExtension
		///</remarks>
		class function MakeDefaultFileExtension(const OriginalFileName, DefaultExtension: string): string;
		///<summary>
		///Retorna o caminho do diretorio pai do arquivo sem a incoveniente "\" final.
		///</summary>
		///<param name="FileName">Nome do arquivo desejado.</param>
		///<returns>Diret�rio pai sem barra final</returns>
		class function ParentDir(const FileName: string): string; overload;
		///<summary>
		///Retorna o diretorio pai de um arquivo passado.
		///Caso "Existing" o arquivo deve ser v�lido. Caso contr�rio o novo pai � calculado at� que um v�lido seja encontrado, ou EmptyStr
		///ser� retornada.
		///</summary>
		///<param name="FileName">Caminho completo do arquivo</param>
		///<param name="Existing">Flag de pr�via exit�ncia</param>
		///<returns>Caminho de acordo com a flag informada</returns>
		class function ParentDir(const FileName: string; Existing: boolean): string; overload;
		///<summary>
		///Garante nome de caminho sem a barra final, exceto raiz com a flag ligada
		///</summary>
		///<param name="Path">Caminho original.</param>
		///<param name="PreserveRoot">Se true mantem a barra final para caso de caminho raiz. Vale para unidade l�gica e UNC's</param>
		///<returns>Caminho passado sem barra caso poss�vel</returns>
		///<remarks>
		///Valor do separador de caminho normalizado para a constante localizada em "SysUtils.PathDelim".
		///</remarks>
		class function SlashRem(const Path: string; PreserveRoot: boolean = False): string;
		///<summary>
		///Ensure a PathDelim at final of Path
		///</summary>
		///<param name="Path">Caminho de entrada</param>
		///<returns>Caminho finalizado com PathDelim ao final</returns>
		class function SlashAdd(const Path: string): string;
		///<summary>
		///Monta string com a vers�o do arquivo passado, para qq erro encontrado '0.0.0.0' ser� retornado
		///</summary>
		///<param name="FileName">Nome de arquivo de interesse. Caso string vazia seja passa o caminho de ParamStr(0) sera usado.</param>
		///<returns>Vers�o do arquivo</returns>
		class function VersionInfo(const FileName: string): string;
		///<summary>
		///Valida se o conjunto de caracteres que compoe o nome do arquivo pode ser gerado, isto � n�o viola o conjunto de
		///caracteres invalidos representados por ContrainLevel.
		///</summary>
		///<param name="FileName">Arquivo a ser validado</param>
		///<param name="ConstrainLevel">N�vel de exig�ncia</param>
		///<returns>Nome do arquivo, com corre��es necess�rias para atender as restri��es</returns>
		///<remarks>
		///ConstrainLevel = 1, indica baixa critica, aceita espaco, acentos e ascii extendido
		///ConstrainLevel = 2, indica media critica, aceita espaco e o ascii baixo ConstrainLevel diferente
		///dos valores acima sera usado alta critica, invalidando todos os acima adcionados aos invalidos no linux
		///</remarks>
		class function IsValidFilename(const FileName: string; ConstrainLevel: Integer): boolean;
		///<summary>
		///Identifica se o arquivo passado pode ser escrito pelo thread em contexto
		///</summary>
		///<param name="FileName">Nome do objeto</param>
		///<returns>True para o caso poss�vel escrita</returns>
		class function IsWritable(const FileName: string): boolean;
		///<summary>
		///Apaga o caminho passado e seus arquivos e sub-diretorios bem como.
		///</summary>
		///<param name="Path"></param>
		///<returns>Valor do codigo de erro do sistema operacional ou 0 se sucesso.</returns>
		///<remarks>
		///Verifique se o diretorio corrente do aplicativo n�o pertence ao conjunto a ser apagado.
		///</remarks>
		class function RmDir(const Path: string): Integer;
		///<summary>
		///Calcula o nome do arquivo seguinte para a sequ�ncia na pasta destino dada por BaseFilename.
		///Caso seja passsado BaseFilename j� contendo um identificador de sequencia (n) este ser� desconsiderado, assim o nome
		///gerado n�o ser� sequ�ncia(n+1).
		///</summary>
		///<param name="BaseFilename">Nome base</param>
		///<returns>Nome de objeto �nico baseado no sequencial dado</returns>
		class function NextFamilyFilename(const BaseFilename: string): string;
		///<summary>
		///Converte data/hora do windows para tipo TDateTime
		///</summary>
		///<param name="FTime">data hora formato de arquivo</param>
		///<returns>data hora padr�o </returns>
		class function FileTimeToDateTime(FTime: TFileTime): TDateTime;
		///<summary>
		///Calcula as datas de criacao/acesso/escrita de um dado arquivo
		///</summary>
		///<param name="FileHandle">Handle do arquivo para coleta das datas</param>
		///<param name="CreateDate"></param>
		///<param name="LastAccessDate"></param>
		///<param name="LastWriteDate"></param>
		///<returns>C�digo de erro da opera��o</returns>
		///<remarks>
		///Notas : O arquivo deve ter a flag GENERIC_READ
		///<BR>Exemplo:</BR>
		///hf := CreateFile (PChar(FFilename), GENERIC_READ , 0, nil, OPEN_EXISTING,
		///0, //FILE_ATTRIBUTE_NORMAL or FILE_FLAG_SEQUENTIAL_SCAN 0);
		///</remarks>
		class function FileTimeProperties(FileHandle: THandle; var CreateDate, LastAccessDate, LastWriteDate: TDateTime)
			: Integer; overload;
		///<summary>
		///Calcula as datas de criacao/acesso/escrita de um dado arquivo
		///</summary>
		///<param name="FileName"></param>
		///<param name="CreateDate"></param>
		///<param name="LastAccessDate"></param>
		///<param name="LastWriteDate"></param>
		///<returns>C�digo de erro da opera��o</returns>
		class function FileTimeProperties(const FileName: string; var CreateDate, LastAccessDate, LastWriteDate: TDateTime)
			: Integer; overload;
		///<summary>
		///Retorna data de modifica��o de um dado arquivo
		///</summary>
		///<param name="FileName"></param>
		///<returns>Data de modifica��o, -1 indica falha</returns>
		class function FileTimeChangeTime(const FileName: string): TDateTime;

	end;

implementation

uses
	Str_Pas, Super, Masks, DateOper, Contnrs, APIHnd, StrHnd, ShlObj;

function ConvSearchMaskArg(Mask: string): string;
var
	P: Integer;
begin
	Result := EmptyStr;
	repeat
		P := Pos(';', Mask);
		if P <> 0 then begin
			Result := Result + '[' + Copy(Mask, 1, P) + ']';
			Delete(Mask, 1, P);
		end else begin
			if Mask <> EmptyStr then begin
				Result := Result + '[' + Mask + ']';
			end;
		end;
	until P = 0;
end;

function __DeleteFile(const FileName: string): boolean; deprecated;
{ {
  Apaga o arquivo independente de seus atributos
}
begin
	Result := False;
	if __FileExists(FileName) then begin
		if (FileSetAttr(FileName, faArchive) = 0) then begin
			Result := DeleteFile(FileName);
		end;
	end else begin
		Result := True; //inexistencia <> erro
	end;
end;

function __FileExists(const Name: string): boolean;
{ {
  Verifica se arquivo existe, extendido para sub-diretorios
}
{$IFDEF MSWINDOWS}
var
	Entry: TSearchRec;
	{$ENDIF}
begin
	{$IFDEF MSWINDOWS}
	Result := (FindFirst(name, faAnyFile - faDirectory, Entry) = 0);
	if Result then begin
		FindClose(Entry);
	end;
	{$ELSE}
	raise Exception.Create('M�todo n�o implementado.');
	{$ENDIF}
end;

function AddKeyGenFile(const FileName: string; TimeOut: Integer): Integer;
var
	Val: Integer;
begin
	if FileExists(FileName) then begin
		Result := GetKeyGenFileValue(FileName, Val, TimeOut);
		if Result = 0 then begin
			Inc(Val);
			Result := SetKeyGenFileValue(FileName, Val, TimeOut);
			if Result = 0 then begin
				Result := Val;
			end else begin
				Result := -1;
			end;
		end;
	end else begin
		Result := SetKeyGenFileValue(FileName, 0, TimeOut);
		if Result = 0 then begin
			Result := 1;
		end else begin
			Result := -1;
		end;
	end;
end;

function ConcatPath(Paths: array of string): string;
{ {
  Junta as parte de um caminho
}
var
	RPart: string;
	i    : Integer;
begin
	if high(Paths) < 1 then begin
		Exit;
	end;
	Result := Paths[0];
	for i  := 1 to high(Paths) do begin
		if Result <> EmptyStr then begin
			while Result[Length(Result)] = PathDelim do begin
				Delete(Result, Length(Result), 1);
			end;
		end;
		RPart := Paths[i];
		if RPart <> EmptyStr then begin
			while RPart[1] = PathDelim do begin
				Delete(RPart, 1, 1);
			end;
			Result := Result + PathDelim + RPart;
		end;
	end;
end;

function CountMaskFilesMatch(const PathMask: string): Integer;
{ {
  Conta a quantidade de arquivos que obedecem uma dada mascara

  Revision - 20100728 - Roger

  Removido faVolumeID da mascara de atributos
}
var
	SRec  : TSearchRec;
	Status: Integer;
begin
	Result := 0;
	Status := FindFirst(PathMask, faDirectory, SRec);
	if Status = 0 then begin
		while Status = 0 do begin
			if (SRec.Attr and (faAnyFile - faDirectory) <> 0) then begin
				Inc(Result);
			end;
			Status := FindNext(SRec);
		end;
		FindClose(SRec);
	end;
end;

function CreateTempFileName(const Prefix: PChar; const Number: byte): string;
{ {
  Cria um nome de arquivo temporario
}
var
	Path, Name: PChar;
	{$IFNDEF WIN32}
	Drive: char;
	{$ENDIF}
begin
	Result := '';
	Path   := StrAlloc(256);
	name   := StrAlloc(512);
	{$IFDEF WIN32}
	if GetTempPath(255, Path) <> 0 then begin
		if GetTempFileName(Path, Prefix, Number, name) <> 0 then begin
			Result := StrPas(name);
		end;
	end;
	{$ELSE}
	GetTempDrive(Drive);
	if GetTempFileName(Drive, Prefix, Number, name) <> 0 then begin
		Result := StrPas(name);
	end;
	{$ENDIF}
	StrDispose(Path);
	StrDispose(name);
end;

{ {
  Class for repass to Enumeration callBacks, instances of TEnumFileParam contains information about the current file.
}
{ -**********************************************************************
  ************************************************************************
  ******************
  ******************  Class:    TEnumFileParam
  ******************  Category: No category
  ******************
  ************************************************************************
  ************************************************************************ }
{ -------------------------------------------------------------------------------------------------------------------------------- }
procedure TEnumFileParam.SetErrorMsg(const Value: PChar);
{ {
  Sets the Error message for this Enumerator file parameter
}
begin
	if FErrorMsg <> nil then begin
		StrDispose(FErrorMsg);
	end;
	FErrorMsg := StrAlloc(Length(Value) + 1);
	StrCopy(FErrorMsg, Value);
end;

{ -------------------------------------------------------------------------------------------------------------------------------- }
procedure TEnumFileParam.SetFileName(const Value: PChar);
{ {
  Sets the filename for this Enumerator file parameter
}
begin
	if FFileName <> nil then begin
		StrDispose(FFileName);
	end;
	FFileName := StrAlloc(Length(Value) + 1);
	StrCopy(FFileName, Value);
end;

{ -------------------------------------------------------------------------------------------------------------------------------- }
constructor TEnumFileParam.Create;
{ {
  Enumerator file parameter constructor
}
begin
	inherited Create;
	Self.FFileName := nil;
	Self.ErrorCode := 0;
	Self.FErrorMsg := nil;
	Self.Data      := nil;
	Self.SR        := nil;
end;

{ -------------------------------------------------------------------------------------------------------------------------------- }
destructor TEnumFileParam.Destroy;
{ {
  Enumerator file parameter destructor
}
begin
	if Self.FFileName <> nil then begin
		StrDispose(Self.FileName);
	end;
	if Self.FErrorMsg <> nil then begin
		StrDispose(Self.ErrorMsg);
	end;
	inherited Destroy;
end;

function DeleteMaskedFiles(const MaskFiles: string): boolean;
{ {
  Apaga arquivos com uma dada mascara
}
var
	FileList : TStringList;
	FinalMask: string;
begin
	FileList := TStringList.Create();
	try
		//FinalMask := ExtractFileName(MaskFiles);
		//Result := (DirFileList(FileList, ExtractFilePath(MaskFiles), [FinalMask]) = 0);
		//Altera��o de arlindo
		Result := (FileHnd.DirectoryList(MaskFiles, FileList) <> nil);
		if Result then begin
			while (FileList.Count > 0) and Result do begin
				FinalMask := FileList.Strings[FileList.Count - 1];
				Result    := DeleteFile(FinalMask);
				if not Result then begin
					Result := SetFileAttributes(PChar(FinalMask), FILE_ATTRIBUTE_ARCHIVE);
					if Result then begin
						Result := DeleteFile(FinalMask);
					end;
				end;
				FileList.Delete(FileList.Count - 1); //Remove ultimo elemento da lista
			end;
		end;
	finally
		FileList.Free;
	end;
end;

function Deltree(const Path: string; IncludeSelf: boolean = True; IgnoreErrors: boolean = True; IgnoreReadOnly: boolean = True;
	IgnoreSystem: boolean = True): Integer;
{ {
  Apaga a pasta e seu conteudo recursivamente
}
{ TODO -oRoger -cLIB : 1Tentativa de filtrar os mais variados warnings abaixo }
{$WARN SYMBOL_PLATFORM OFF }
var
	ChildItem                : string;
	OldAttribs, RemoveAttribs: DWORD;
	Lst                      : TStringList;
	i                        : Integer;
begin
	//Limpeza das sub-pastas
	Result := ERROR_SUCCESS;
	SetLastError(Result);
	Lst := TStringList.Create;
	try
		ListDirFilesNames(Path, '*.*', faAnyFile, False, Lst);
		for i := Lst.Count - 1 downto 0 do begin
			if DirectoryExists(Lst.Strings[i]) then begin
				Result := Deltree(Lst.Strings[i], True, IgnoreErrors, IgnoreReadOnly, IgnoreSystem);
				if (not IgnoreErrors) and (Result <> ERROR_SUCCESS) then begin //Reporta o erro e sai
					Exit;
				end;
			end else begin //Trata-se de arquivo
				ChildItem := Lst.Strings[i];
				if not DeleteFile(ChildItem) then begin
					Result     := GetLastError();                      //Reserva retorno se nada for ignorado
					OldAttribs := GetFileAttributes(PChar(ChildItem)); //Reserva atributo corrente do arquivo
					if OldAttribs = DWORD(-1) then begin               //Falha em pegar atributos correntes
						if IgnoreErrors then begin
							System.Continue;
						end else begin
							Exit;
						end;
					end;

					//Ajusta os atributos do arquivo para a nova tentativa
					if IgnoreReadOnly then begin
						RemoveAttribs := faHidden;
					end else begin
						RemoveAttribs := 0; //Mascara nenhum remocao
					end;
					if IgnoreSystem then begin
						RemoveAttribs := RemoveAttribs + faSysFile;
					end;
					RemoveAttribs := OldAttribs and (not(RemoveAttribs));
					if not SetFileAttributes(PChar(ChildItem), RemoveAttribs) then begin
						//Nada mais a fazer ate setting dos atributos falhou
						if not IgnoreErrors then begin
							Break;
						end else begin
							System.Continue; //Tenta apagar arquivos restantes
						end;
					end;
					if not DeleteFile(ChildItem) then begin
						SetFileAttributes(PChar(ChildItem), OldAttribs); //Ridiculamente retorna atributos originais
						if not IgnoreErrors then begin
							Break;
						end;
					end;
				end;
			end;
		end;
		//�ltimo passo a elimina��o do diretorio caso nenhum error anterior
		if ((Result = ERROR_SUCCESS) and IncludeSelf) then begin
			if (not RemoveDir(Path)) then begin
				Result := GetLastError();
			end;
		end;
	finally
		Lst.Free;
	end;
	{$WARN SYMBOL_PLATFORM ON}
end;

function DirFileList(List: TStrings; const Dir: string; Masks: array of string): Integer;
{ {
  Preenche Lista com os arquivos do path passado, omite sub-dirs
}
var
	SRec    : TSearchRec;
	i       : Integer;
	MaskList: TObjectList;
begin
	List.Clear;
	List.BeginUpdate;
	try
		Result := FindFirst(TFileHnd.ConcatPath([Dir, '*.*']), faAnyFile, SRec);
		try
			if (Result = 0) then begin
				MaskList := TObjectList.Create;
				try
					for i := low(Masks) to high(Masks) do begin
						MaskList.Add(TMask.Create(Masks[i]));
					end;
					while (Result = 0) do begin
						if SRec.Attr and faDirectory = 0 then begin //Exclui sub-dirs
							//Loop de comparacao com as mascaras fornecidas
							for i := 0 to MaskList.Count - 1 do begin
								if TMask(MaskList.Items[i]).Matches(SRec.Name) then begin
									List.Add(TFileHnd.ConcatPath([Dir, SRec.Name]));
									Break;
								end;
							end;
						end;
						Result := FindNext(SRec);
					end;
				finally
					MaskList.Free;
				end;
				Result := 0;
			end;
		finally
			FindClose(SRec);
		end;
	finally
		List.EndUpdate;
	end;
end;

function DirectoryList(const DirMask: string; FList: TStringList): TStringList;
{ {
  Retorna uma lista com os nomes dos arquivos ( Omite sub-pastas ) apenas em strings relativas ao diretorio passado

  Revision - 20100728 - Roger
  faVolumeID removido da mascara de atributos
}
var
	SRec   : TSearchRec;
	Status : Integer;
	DirName: string;
begin
	Status := FindFirst(DirMask, faDirectory, SRec);
	if Status = 0 then begin
		try
			Result := FList;
			Result.Clear;
			DirName := ExtractFilePath(DirMask);
			while Status = 0 do begin
				if (SRec.Attr and (faAnyFile - faDirectory)) <> 0 then begin
					Result.Add(DirName + SRec.Name);
				end;
				Status := FindNext(SRec);
			end;
		finally
			FindClose(SRec);
		end;
	end else begin
		Result := nil;
	end;
end;

function EnumFiles(const Directory: string; MaskList: TList; IncludeAttrs, ExcludeAttrs: Integer; FileParam: TEnumFileParam;
	FileProc: TEnumFileProc): Integer; platform; overload;
{ {
  For each file existing at Directory that obeys the rules given by MaskList, IncludeAttrs, ExcludeAttrs the FileProc will be called. FileParam will be filled with the file information and repassed to FileProc.

  Revision - 20100728 - Roger
  faVolumeID removido da mascara de atributos
}
var
	SR: TSearchRec;
	i : Integer;
begin
	{$WARN UNSAFE_CODE OFF} {$WARN UNSAFE_CAST OFF}
	Result := ERROR_SUCCESS;
	SetLastError(Result);
	try
		if FindFirst(Directory + PathDelim + '*.*', faAnyFile, SR) = 0 then begin //Busca qualquer arquivo
			try
				repeat
					if ((SR.Attr and faDirectory) <> 0) then begin //Tratamento diferente para diretorios
						if (SR.Name = '.') then begin              //Chama callback para repassar o diretorio corrente
							if (((IncludeAttrs and faDirectory) <> 0) and ((ExcludeAttrs and faDirectory) = 0)) then begin
								//Chamada repassa diretorio corrente
								FileParam.SR := @SR;
								Result       := FileProc(Directory, FileParam); //FileParam preenchido por FileProc!!!
								if Result <> 0 then begin
									Break;
								end;
							end;
						end else begin
							if (SR.Name = '..') then begin
								System.Continue;
							end else begin //Chamada recursiva para o sub-diretorio
								Result := EnumFiles(Directory + PathDelim + SR.Name, MaskList, FileParam, FileProc);
							end;
						end;
					end else begin //Trata-se de arquivo
						if (((SR.Attr and IncludeAttrs) <> 0) and ((SR.Attr and ExcludeAttrs) = 0)) then begin //Teste dos atributos
							if (MaskList <> nil) then begin //Checa se mascara eh valida
								for i := 0 to MaskList.Count - 1 do begin
									if (TMask(MaskList.Items[i]).Matches(SR.Name)) then begin
										FileParam.SR := @SR;
										Result       := FileProc(Directory + PathDelim + SR.Name, FileParam);
										Break; //pelo menos 1 mascara entrou para este arquivo
									end;
								end;
							end else begin //Acceita qualquer arquivo
								FileParam.SR := @SR;
								Result       := FileProc(Directory, FileParam);
							end;
						end;
					end;
				until ((Result <> ERROR_SUCCESS) or (FindNext(SR) <> 0));
			finally
				FindClose(SR);
			end;
		end else begin
			Result := GetLastError();
		end;
	finally
		if (Result <> ERROR_SUCCESS) then begin
			FileParam.FileName  := TStrHnd.StrToPChar(Directory);
			FileParam.ErrorCode := Result;
			FileParam.ErrorMsg  := TStrHnd.StrToPChar(SysErrorMessage(Result));
		end;
	end;
	{$WARN UNSAFE_CODE ON}  {$WARN UNSAFE_CAST ON}
end;

function EnumFiles(const Directory, Mask: string; IncludeAttrs, ExcludeAttrs: Integer; FileParam: TEnumFileParam;
	FileProc: TEnumFileProc): Integer; platform; overload;
{ {
  For each file existing at Directory that obeys the rules given by Mask, IncludeAttrs and ExcludeAttrs the FileProc will be called. FileParam will be filled with the file information and repassed to FileProc.
}
var
	MaskList   : TObjectList;
	index      : Integer;
	s          : string;
	MaskChecker: TMask;
begin
	if (Mask <> EmptyStr) then begin
		MaskList := TObjectList.Create();
		try
			//Monta a lista de mascaras
			index := 0;
			repeat
				s := GetDelimitedSubStr(';', Mask, index);
				Inc(index);
				if (s <> EmptyStr) then begin
					MaskChecker := TMask.Create(s);
					MaskList.Add(MaskChecker);
				end;
			until (s = EmptyStr);

			//Usa a lista para repassar para a nova chamada para montar o resultado
			Result := EnumFiles(Directory, MaskList, IncludeAttrs, ExcludeAttrs, FileParam, FileProc);
		finally
			MaskList.Free();
		end;
	end else begin
		Result := EnumFiles(Directory, nil, IncludeAttrs, ExcludeAttrs, FileParam, FileProc);
	end;
end;

function EnumFiles(const Directory: string; MaskList: TList; FileParam: TEnumFileParam; FileProc: TEnumFileProc): Integer;
{ {
  Chama um callback para todos os arquivos abaixo do diretorio dado enquanto o retorno for 0
}
begin
	{$WARN SYMBOL_PLATFORM OFF}
	Result := EnumFiles(Directory, MaskList, faAllFiles { inclui todos } , 0 { Exclui nenhum } , FileParam, FileProc);
	{$WARN SYMBOL_PLATFORM ON}
end;

function EnumFiles(const Directory, Mask: string; FileParam: TEnumFileParam; FileProc: TEnumFileProc): Integer;
{ {
  Chama um callback para todos os arquivos abaixo do diretorio dado enquanto o retorno for 0

  (1) Corre��o de bug para o caso de �nica m�scara passada ser *.*

  Revision: 3/6/2008 - roger
}
var
	MaskChecker: TMask;
	MaskList   : TObjectList;
	s          : string;
	index      : Integer;
begin
	if (Mask = EmptyStr) then begin { 1 }
		//Usa a lista para repassar para a nova chamada para montar o resultado
		Result := EnumFiles(Directory, nil, FileParam, FileProc);
	end else begin
		MaskList := TObjectList.Create();
		try
			//Monta a lista de mascaras
			index := 0;
			repeat
				s := GetDelimitedSubStr(';', Mask, index);
				Inc(index);
				if (s <> EmptyStr) then begin
					MaskChecker := TMask.Create(s);
					MaskList.Add(MaskChecker);
				end;
			until (s = EmptyStr);

			//Usa a lista para repassar para a nova chamada para montar o resultado
			Result := EnumFiles(Directory, MaskList, FileParam, FileProc);
		finally
			MaskList.Free();
		end;
	end;
end;

function EnumFilesDir(const Directory, InitialMask: string; FileParam: TEnumFileParam; FileProc: TEnumFileProc): Integer;
{ {
  Chama um callback para todos os arquivos do diretorio dado enquanto o retorno for 0.
  InitialMask apenas fornece uma m�scara para o primeiro arquivo de interesse. N�o devendo ser considerador como filtro para os
  nomes de arquivos desejados.

  Ver:
  TEnumFileParam e TEnumFileProc

  Revision: 25/11/2005 - Roger
}
var
	ActualDir: string;
	SR       : TSearchRec;
begin
	if not DirectoryExists(Directory) then begin
		Result              := ERROR_PATH_NOT_FOUND;
		FileParam.FileName  := TStrHnd.StrToPChar(Directory);
		FileParam.ErrorCode := Result;
		FileParam.ErrorMsg  := TStrHnd.StrToPChar(SysErrorMessage(Result));
		Exit;
	end;
	//Manipula os arquivos
	Result := 0;

	ActualDir := Directory;

	if FindFirst(TFileHnd.ConcatPath([ActualDir, InitialMask]), faAnyFile, SR) = 0 then begin
		try
			repeat
				if (SR.Name = '.') or (SR.Name = '..') then begin //ignora diretorios especiais
					System.Continue;
				end;
				{$WARN UNSAFE_CODE OFF}
				FileParam.SR := @SR;
				{$WARN UNSAFE_CODE ON}
				Result := FileProc(TFileHnd.ConcatPath([ActualDir, SR.Name]), FileParam); //FileParam preenchido por FileProc!!!
				if Result <> 0 then begin
					Break;
				end;
			until FindNext(SR) <> 0;
		finally
			FindClose(SR);
		end;
	end;
end;

function EvalPathName(const Path: string): string;
{ {
  Avalia um caminho de modo a substituir %ENV_VARS% pelos seus valores e caminhos relativos "." e ".."( inclusive UNC's )
}
const
	DELIM = '%';
var
	Macro, Value: string;
begin
	Result := Path;
	Macro  := GetDelimitedLevelSubStr(DELIM, DELIM, Result, 0);
	while Macro <> EmptyStr do begin
		Value  := TAPIHnd.GetEnvironmentVar(Macro);
		Result := StringReplace(Result, DELIM + Macro + DELIM, Value, [rfReplaceAll, rfIgnoreCase]);
		Macro  := GetDelimitedLevelSubStr(DELIM, DELIM, Result, 0);
	end;
	if IsUNCName(Result) then begin
		Result := ExpandUNCFileName(Result);
	end else begin
		Result := ExpandFilename(Result);
	end;
end;

function ExtractFileDrive(const FileName: string): string;
{ {
  Extrai a unidade do caminho do arquivo
}
var
	Loc: Integer;
begin
	Result := FileName;
	Loc    := Pos(':', Result);
	if Loc = 0 then begin
		GetDir(0, Result);
		Loc := Pos(':', Result);
	end;
	Result := Copy(Result, 1, Loc);
end;

function ExtractFileExtension(const FileName: string): string;
{ {
  Usar SysUtils.ExtractFileExt
  Depreciada em 2005.04.29
  Extrai a extensao sem o ponto final de um arquivo
}
var
	{$IFNDEF WIN32}
	Tmp: array [0 .. 255] of char;
	{$ELSE}
	Tmp: PChar;
	{$ENDIF}
begin
	{$IFNDEF WIN32}
	Result := AllTrim(Copy(List.Strings[i], Pos('.', List.Strings[i])));
	{$ELSE}
	Tmp    := TStrHnd.StrToPChar(FileName);
	Result := AllTrim(Copy(StrPas(StrRScan(Tmp, '.')), 2, StrLen(Tmp)));
	StrDispose(Tmp);
	{$ENDIF}
end;

function ExtractRootResource(const ResourceName: string): string;
{ {
  Extrai a raiz do recurso especificado
}
begin
	if IsUNCName(ResourceName) then begin
		Result := ExtractUNCResource(ResourceName);
	end else begin
		Result := ExtractFileDrive(ResourceName);
	end;
end;

function ExtractUNCResource(const UNC: string): string;
{ {
  Extrai a parte relativa ao recurso de um unc
}
var
	Part: string;
begin
	Result := EmptyStr;
	if not IsUNCName(UNC) then begin
		Exit;
	end;
	Part := GetDelimitedSubStr(PathDelim, UNC, 2);
	if Part <> EmptyStr then begin
		Result := '\\' + Part + PathDelim;
	end else begin
		Exit;
	end;
	Part := GetDelimitedSubStr(PathDelim, UNC, 3);
	if Part <> EmptyStr then begin
		Result := Result + Part;
	end else begin
		Result := EmptyStr;
	end;
end;

function FileCopy(const Source, Dest: string; Over: boolean): Integer;
{ {
  Copia um arquivo para outro lugar

  Revision - 20120529 - roger
  Removida chamada interna para identificar se arquivo existe por chamada a SysUtils.FileExists
}
var
	Buffer: TMemoryStream;
	Attr  : Integer;
	Age   : longint;
	Hnd   : Integer;
begin
	if (SysUtils.FileExists(Source)) then begin              //source exists
		if DirectoryExists(ExtractFilePath(Dest)) then begin //dest exists
			if (Over or (not(__FileExists(Dest)))) then begin
				if (TFileHnd.FileSize(Source) > SpaceFree(ExtractFilePath(Dest))) then begin //falta espaco
					Result := ERROR_DISK_FULL;
					Exit;
				end;
				if SysUtils.FileExists(Dest) and (FileSetAttr(Dest, SysUtils.faArchive) <> 0) then begin
					//atributos de protecao nao foram resetados
					Result := ERROR_OPEN_FAILED;
					Exit;
				end else begin
					DeleteFile(Dest);
				end;
				Buffer := TMemoryStream.Create;
				try
					Buffer.LoadFromFile(Source);
					Buffer.SaveToFile(Dest);
				except
					on Exception do begin
						Buffer.Free;
						Buffer := nil;
					end;
				end;
				if Buffer <> nil then begin
					Buffer.Free;
				end;
				//Sleep(200); //essa merda se justifica pelo sistema de I/O do NT
				Hnd := FileOpen(Source, fmOpenRead);
				Age := FileGetDate(Hnd);
				FileClose(Hnd);
				Hnd := FileOpen(Dest, fmOpenWrite or fmShareDenyNone);
				FileSetDate(Hnd, Age);
				FileClose(Hnd);
				Attr   := FileGetAttr(Source);
				Result := FileSetAttr(Dest, Attr);
			end else begin
				Result := ERROR_ALREADY_EXISTS;
			end;
		end else begin
			Result := ERROR_CANNOT_MAKE;
		end;
	end else begin
		Result := ERROR_FILE_NOT_FOUND;
	end;
end;

function FileCopyEx(const Source, Dest: string; Over: boolean): Integer;
{ {
  Copia um arquivo para outro lugar criando os respectivos diretorios
  //******************** TIPO DE RETORNO ALTERADO EM 08/01/1999 ****************************
}
var
	SubDir: string;
begin
	SubDir := TFileHnd.ParentDir(Dest);
	if not(DirectoryExists(SubDir)) then begin
		if (not ForceDirectories(SubDir)) then begin
			Result := GetLastError();
			Exit;
		end;
	end;
	Result := FileCopy(Source, Dest, Over);
end;

function FileMoveMaskDir(const MaskSourceDir, DestDir: string; Over, IncHiddens, IncSystem: boolean): Integer; platform;
{ {
  Move arquivos de uma dada mascara para um dado diretorio
}
var
	SR           : TSearchRec;
	Path         : string;
	Found        : boolean;
	Attrib, Flags: Integer;
begin
	if Over then begin
		Flags := MOVEFILE_WRITE_THROUGH or MOVEFILE_REPLACE_EXISTING or MOVEFILE_COPY_ALLOWED;
	end else begin
		Flags := MOVEFILE_WRITE_THROUGH or MOVEFILE_COPY_ALLOWED;
	end;
	if not ForceDirectories(DestDir) then begin
		Result := ERROR_PATH_NOT_FOUND;
		Exit;
	end;
	Path   := TFileHnd.SlashRem(ExtractFilePath(MaskSourceDir), True);
	Attrib := faAnyFile - faDirectory;
	if not IncHiddens then begin //Isola Hidden
		Attrib := Attrib - faHidden;
	end;
	if not IncSystem then begin //Isola SystemFiles
		Attrib := Attrib - faSysFile;
	end;
	Result := ERROR_SUCCESS;
	repeat
		Found := (FindFirst(MaskSourceDir, Attrib, SR) = 0);
		try
			if Found then begin
				if not MoveFileEx(PChar(TFileHnd.ConcatPath([Path, SR.Name])), PChar(TFileHnd.ConcatPath([DestDir, SR.Name])), Flags)
				then begin
					Result := GetLastError();
				end;
			end;
		finally
			if Found then begin
				FindClose(SR);
			end;
		end;
	until (Result <> ERROR_SUCCESS) or (not Found);
end;

function FileNameInvalidChars(Strictness: Integer; const FileName: string): string;
{ {
  Retorna string com todos os caractares invalidos do nome do arquivo, nao funciona para path
}
begin
	case Strictness of
		0: begin //Complacente
				Result := Str_Pas.UsedCharSet(INVALID_FILE_NAME_CHARS1_STR, FileName);
			end;
		1: begin //Mediano
				Result := Str_Pas.UsedCharSet(INVALID_FILE_NAME_CHARS2_STR, FileName);
			end;
	else begin //Rigoroso
			Result := Str_Pas.UsedCharSet(INVALID_FILE_NAME_CHARS3_STR, FileName);
		end;
	end;
end;

function FileCopyMaskDir(const MaskSourceDir, DestDir: string; Over, IncHiddens: boolean): boolean;
{ {
  Copia arquivos de uma dada mascara para um dado diretorio

  Revision - 20100728 - Roger
  faVolumeID removido da mascara de atributos

}
var
	Rec    : TSearchRec;
	Path   : string;
	Attrib : Word;
	EndList: boolean;
begin
	Path := ExtractFilePath(MaskSourceDir);
	if IncHiddens then begin
		Attrib := faAnyFile - faDirectory;
	end else begin
		Attrib := faAnyFile - faHidden - faDirectory;
	end;
	Result := False;
	if FindFirst(MaskSourceDir, Attrib, Rec) <> 0 then begin { Nenhum arquivo }
		Result := True;                                      { A inexistencia da origem nao implica em falha }
		Exit;
	end;
	EndList := False;
	repeat
	begin
		if (Rec.Attr or faDirectory) = Rec.Attr then begin
			System.Continue; { Pula diretorios }
		end;
		if ((Rec.Attr or faHidden) = Rec.Attr) and not(IncHiddens) then begin
			System.Continue; { Pula ocul }
		end;
		if __FileExists(DestDir + PathDelim + Rec.Name) and (not Over) then begin
			FindClose(Rec);
			Exit;
		end;
		if FileCopy(Path + Rec.Name, DestDir + PathDelim + Rec.Name, True) <> 0 then begin
			{ falha copia }
			Break;
		end;
		EndList := (FindNext(Rec) <> 0); { Toma proximo }
	end
	until EndList;
	Result := True;
	FindClose(Rec);
end;

function FileMaskSetAttribs(const FileMask: string; Attr: Integer): Integer;
{ {
  Seta os atributos para os arquivos que obedecem a mascara passada

  Revision - 20100728 - Roger
  faVolumeID removido da mascara de atributos
}
var
	Rec    : TSearchRec;
	Path   : string;
	EndList: boolean;
begin
	Path := ExtractFilePath(FileMask);
	if FindFirst(FileMask, faAnyFile - faDirectory, Rec) <> 0 then begin { Nenhum arquivo }
		Result := ERROR_FILE_NOT_FOUND;                                  { A inexistencia da origem implica em falha }
		Exit;
	end;
	EndList := False;
	Result  := 0;
	repeat
	begin
		if Rec.Name = '..' then begin { Pula diretorio pai }
			System.Continue;
		end;
		Result := FileSetAttr(Path + Rec.Name, Attr);
		if Result <> 0 then begin
			Break;
		end;
		EndList := (FindNext(Rec) <> 0); { Toma proximo }
	end
	until EndList;
	FindClose(Rec);
end;

function FileShortName(LongFileName: string): string; platform;
{ {
  Retorna o nome curto aceito para aquele arquivo incluindo o caminho
}
var
	LastSlash  : PChar;
	TempPathPtr: array [0 .. MAX_PATH] of char;
begin
	{$WARN UNSAFE_CODE OFF}
	Result := EmptyStr;
	StrPCopy(TempPathPtr, LongFileName);
	LastSlash := StrRScan(TempPathPtr, PathDelim);
	while LastSlash <> nil do begin
		Result := PathDelim + LongToShortFileName(TempPathPtr) + Result;
		if LastSlash <> nil then begin
			LastSlash^ := char(0);
			LastSlash  := StrRScan(TempPathPtr, PathDelim);
		end;
	end;
	Result := TempPathPtr + Result;
	{$WARN UNSAFE_CODE ON}
end;

function FileTextCountLines(const FileName: string): longint;
{ {
  Conta quantas linha o arquivo possui
}
begin
	Result := FileTextCountSubStr(FileName, EOL_DOS);
end;

function FileTextCountSubStr(const FileName, SubStr: string): Integer;
{ {
  Conta quantas vezes a substr aparece dentro do arquivo
}
var
	FS       : TFileStream;
	Buf      : array [0 .. IDEAL_FILE_BLOCK_SIZE] of byte;
	PS       : PChar;
	Acc, Step: longint;
	OffSet   : Integer;
begin
	{$TYPEDADDRESS OFF} {$WARN UNSAFE_CODE OFF}
	//NOTAS : O tamanho de SubStr nao pode exceder IDEAL_FILE_BLOCK_SIZE !!!!
	SetLastError(0);
	try
		FS := TFileStream.Create(FileName, fmOpenRead);
		try
			OffSet := Length(SubStr);
			Acc    := 0;
			Result := 0;
			while FS.Size > Acc do begin
				Step          := FS.Read(Buf, IDEAL_FILE_BLOCK_SIZE - 1);
				Buf[Step + 1] := byte(#0); //Marca fim de pesquisa para evitar ir alem do fim do arquivo
				Inc(Acc, Step);
				PS := @Buf[0]; //Inicio da pesquisa
				while PS <> nil do begin
					PS := StrPos(PS, PChar(SubStr));
					if PS <> nil then begin
						Inc(PS, OffSet); //Avanca substr
						Inc(Result);
					end;
				end;
			end;
		finally
			FS.Free;
		end;
	except
		Result := -1; //Valor incorreto acesse GetLastError
		if GetLastError = 0 then begin
			SetLastError(ERROR_INVALID_HANDLE);
		end;
	end;
	{$TYPEDADDRESS ON} {$WARN UNSAFE_CODE ON}
end;

function FileXCopy(const Source, Dest: string; Over, IncHiddens: boolean): boolean;
{ {
  Copia todos os arquivos da arvore fonte para a arvore destino
}
var
	ActualDir: string;
	SR       : TSearchRec;
begin
	Result := False;
	if not DirectoryExists(Source) then begin
		Exit;
	end;
	ForceDirectories(Dest);
	if not DirectoryExists(Dest) then begin
		Exit;
	end;
	if FindFirst(Source + PathDelim + '*.*', faDirectory + faSysFile + IIfInt(IncHiddens, faHidden, 0), SR) = 0 then begin
		repeat
			if (SR.Name = '.') or (SR.Name = '..') or ((SR.Attr and faDirectory) = 0) then begin
				System.Continue;
			end;
			ActualDir := Source + PathDelim + SR.Name;
			if not FileXCopy(ActualDir, Dest + PathDelim + ExtractFileName(ActualDir), Over, IncHiddens) then begin
				FindClose(SR);
				Exit;
			end;
		until FindNext(SR) <> 0;
		FindClose(SR);
	end;
	FileSetAttr(Dest, FileGetAttr(Source));
	Result := FileCopyMaskDir(Source + PathDelim + '*.*', Dest, Over, IncHiddens);
end;

function FileSize_(const FileName: string): longint;
{ {
  Retorna o tamanho de um arquivo
}
var
	SR: TSearchRec;
begin
	if FindFirst(FileName, faAnyFile, SR) = 0 then begin
		Result := SR.Size;
		FindClose(SR);
	end else begin
		Result := -1;
	end;
end;

function FindFirstChildDir(const DirName: string): string;
{ {
  Retorna o primeiro diretorio filho do diretorio dado
}
var
	SR: TSearchRec;
begin
	{$WARN SYMBOL_PLATFORM OFF}
	Result := EmptyStr;
	//Localiza qq coisa inicialmente
	if FindFirst(DirName + PathDelim + '*.*', faDirectory + faHidden + faReadOnly + faSysFile, SR) = 0 then begin //encontrado alvo
		try
			repeat
				if (SR.Name = '.') or (SR.Name = '..') then begin //Filtra dir corrente e pai
					System.Continue;
				end;
				if ((SR.Attr or faDirectory) = SR.Attr) then begin //trata-se de um sub-diretorio
					Result := DirName + PathDelim + SR.Name;
					Break;
				end;
			until (FindNext(SR) <> 0);
		finally
			FindClose(SR);
		end;
	end;
	{$WARN SYMBOL_PLATFORM ON}
end;

function FindFirstChildFile(const DirName: string): string;
{ {
  Returns the first child file at specified directory
}
var
	SR: TSearchRec;
begin
	if FindFirst(DirName + PathDelim + '*.*', faAnyFile - faDirectory, SR) = 0 then begin
		Result := DirName + PathDelim + SR.Name;
		FindClose(SR);
	end else begin
		Result := EmptyStr;
	end;
end;

function NetFileCopy(const Source, Dest: string; Over: boolean): boolean;
{ {
  Copia um arquivo para outro em uma rede peba
}
var
	Buffer: TMemoryStream;
	Attr  : Integer;
	Info  : TSearchRec;
	Age   : longint;
	Hnd   : Integer;
begin
	Result := False;
	if (FindFirst(Source, faAnyFile, Info) = 0) then begin
		FindClose(Info);
		if DirectoryExists(ExtractFilePath(Dest)) then begin
			if (Over or (not(FileExists(Dest)))) then begin
				Buffer := TMemoryStream.Create;
				try
					Buffer.LoadFromFile(Source);
					Buffer.SaveToFile(Dest);
				except
					on Exception do begin
						Buffer.Free;
						Buffer := nil;
					end;
				end;
				if Buffer <> nil then begin
					Buffer.Free;
				end;
				Attr   := FileGetAttr(Source);
				Result := (FileSetAttr(Dest, Attr) = 0);
				if Result then begin
					Hnd := FileOpen(Source, fmOpenRead);
					Age := FileGetDate(Hnd);
					FileClose(Hnd);
					Hnd := FileOpen(Dest, fmOpenRead);
					FileSetDate(Hnd, Age);
					FileClose(Hnd);
				end;
			end;
		end;
	end;
end;

function GetFileSize(const FileName: string): int64;
{ {
  Retorna o tamanho do arquivo

  Depreciada, use TFileHnd.FileSize();
}
var
	{ TODO -oRoger -cLIB : 2Tentativa de filtrar os mais variados warnings abaixo }
	{$WARN SYMBOL_PLATFORM OFF}
	SR: TSearchRec;
begin
	{$IFDEF WIN32}
	if FindFirst(FileName, faAnyFile, SR) = 0 then begin
		Result := (SR.FindData.nFileSizeHigh shl 32) + SR.FindData.nFileSizeLow;
		FindClose(SR);
	end else begin
		Result := -1;
	end;
	{$WARN SYMBOL_PLATFORM ON}
	{$ELSE}
	raise Exception.Create('Sem solu��o dispon�vel para Linux.');
	{$ENDIF}
end;

function GetFloppiesList: string;
{ {
  Retorna lista de drives removiveis
}
var
	i  : byte;
	Reg: Word;
begin
	{$WARN UNSAFE_CODE OFF}
	asm
		INT     $11
		MOV     Reg,AX
	end;
	{ intr($11,regs); }
	Result     := '';
	for i      := 1 to succ((Reg and 192) shr 6) do begin
		Result := Result + chr(64 + i);
	end;
	{$WARN UNSAFE_CODE ON}
end;

function GetKeyGenFileValue(const FileName: string; var Value, TimeOut: Integer): Integer;
{ {
  Le em arquivo de usos exclusivo valor limitado ao periodo de espera
}
var
	FHnd  : TextFile;
	FTime : TDateTime;
	Sucess: boolean;
	Line  : string;
begin
	FTime := IncTime(Now, 0, 0, 0, TimeOut);
	AssignFile(FHnd, FileName);
	Value  := 0;
	Sucess := False;
	Result := -1;
	try
		repeat
		begin
			{$IOCHECKS OFF}
			Reset(FHnd);
			{$IOCHECKS ON}
			if IOResult = 0 then begin
				Sucess := True;
			end
		end;
		until ((Sucess) or (Now > FTime));

		if Sucess then begin
			ReadLn(FHnd, Line);
			try
				Value := StrToInt(Line);
			except
				Value  := 0;
				Result := ERROR_INVALID_PARAMETER;
			end;
		end else begin
			if IOResult <> 0 then begin
				Result := IOResult;
			end else begin
				Result := ERROR_TIMEOUT;
			end;
		end;
	finally
		if Sucess then begin
			CloseFile(FHnd);
			Result := NO_ERROR;
		end;
	end;
end;

function GetTempDir: string;
{ {
  Retorna o caminho do diretorio de arquivos temporarios
}
var
	Path: array [0 .. MAX_PATH] of char;
begin
	SetLastError(ERROR_SUCCESS);
	GetTempPath(MAX_PATH, Path);
	if GetLastError() = ERROR_SUCCESS then begin
		Result := Path;
	end else begin
		Result := EmptyStr;
	end;
end;

function IsUNCName(const UNC: string): boolean;
{ {
  Tests if the argument is a UNC compliant.
}
begin
	Result := (Pos('\\', UNC) = 1);
end;

procedure ListDirFilesNames(Dir, Mask: string; Attr: Integer; IncSubDirs: boolean; List: TStrings);
{ {
  Lista todos os arquivos de uma pasta que obede�am a m�scara dada e aos atributos especificados.

  Revision: 25/11/2005 - Roger

  Corrigido bug que falha na enumera��o das mascaras que continham "?', pois comparava o caminho completo do arquivo com a m�scara.
  em LocalListFiles()
  PParam^.Mask.Matches(Filename) -> PParam^.Mask.Matches(ExtractFileName(Filename))

  Revision: 2/5/2006 - Roger
}
type
	PLocalDataParam = ^TLocalDataParam;

	TLocalDataParam = record
		FList: TStrings;
		MatchAttribut: Integer;
		Mask: TMask;
	end;
var
	param    : TEnumFileParam;
	DataParam: TLocalDataParam;
	//.........................................................................................................
	function LocalListFiles(const FileName: string; FileParam: TEnumFileParam): Integer;
	var
		TargetAttr: Integer;
		PParam    : PLocalDataParam;
	begin
		{$WARN UNSAFE_CODE OFF}
		try
			PParam     := FileParam.Data;
			TargetAttr := PParam^.MatchAttribut;
			if (((TargetAttr = faAnyFile) or ((TargetAttr and FileParam.SR^.Attr) = TargetAttr)) and
				PParam^.Mask.Matches(ExtractFileName(FileName))) then begin
				PParam^.FList.Add(FileName);
			end;
		except
			Result := GetLastError;
			Exit;
		end;
		Result := 0;
		{$WARN UNSAFE_CODE ON}
	end;

//.........................................................................................................
begin
	{$TYPEDADDRESS OFF} {$WARN UNSAFE_CODE OFF}
	Dir := TFileHnd.SlashRem(Dir);
	if GetIChar(Dir, Length(Dir)) = ':' then begin //Checa por raiz de unidade
		Dir := Dir + PathDelim;
	end;
	param := TEnumFileParam.Create;
	List.Clear;
	DataParam.FList         := List;
	DataParam.MatchAttribut := Attr;
	DataParam.Mask          := TMask.Create(Mask);
	param.Data              := @DataParam;
	try
		if IncSubDirs then begin
			EnumFiles(Dir, Mask, param, @LocalListFiles);
		end else begin
			EnumFilesDir(Dir, Mask, param, @LocalListFiles);
		end;
	finally
		DataParam.Mask.Free; //Libera mascara vinculada acima
		param.Free;
	end;
	{$TYPEDADDRESS ON} {$WARN UNSAFE_CODE ON}
end;

function LongToShortFileName(const LongName: string): string;
{ {
  Retorna nome do arquivo compativel com o formato 8.3 estilo DOS
}
var
	SR: TSearchRec;
begin
	if SysUtils.FindFirst(LongName, faAnyFile, SR) = 0 then begin
		Result := string(SR.FindData.cAlternateFileName);
		if Result = EmptyStr then begin
			Result := string(SR.Name);
		end;
		SysUtils.FindClose(SR);
	end else begin
		Result := EmptyStr;
	end;
end;

function IsRootDir(const Name: string): boolean;
{ {
  Tests if the argument is a root of any disk volume
}
var
	Rep: Integer;
begin
	Result := False;
	if IsUNCName(name) then begin
		Rep := StrCountCharRepet(PathDelim, name);
		if Rep <= 4 then begin
			if Rep = 4 then begin
				Result := (name[Length(name)] = PathDelim); //o ultimo nao vale
			end else begin
				Result := True;
			end;
		end;
	end else begin
		Rep := Length(name);
		case (Rep) of
			2: begin
					Result := CharInSet(UpCase(name[1]), ['A' .. 'Z']) and (name[2] = ':'); //Exemplo (L:\) or (C:)
				end;
			3: begin
					Result := CharInSet(UpCase(name[1]), ['A' .. 'Z']) and (name[2] = ':') and (name[3] = PathDelim);
					//Exemplo (L:\) or (C:)
				end;
		else begin
				Result := False;
			end;
		end;
	end;
end;

function MkDirEx(Dir: string): boolean; deprecated;
{ {
  Revision - 20120524 - roger
  Depreciado - Usar ForceDirectories

  Cria arvore dada se esta nao existir ainda
}
var
	_Sub: boolean;
begin
	if (DirectoryExists(Dir)) then begin
		Result := True;
	end else begin
		Result := False;
		if DirectoryExists(TFileHnd.ParentDir(Dir)) then begin
			MkDir(Dir);
			Result := (IOResult = 0);
		end else begin
			{ Temos bronca quando o n�vel � no raiz devido ao dir pai retornado }
			_Sub := MkDirEx(TFileHnd.ParentDir(Dir));
			if _Sub then begin
				Result := MkDirEx(Dir);
			end;
		end;
	end;
end;

function ParentDir(const Dir: string): string;
{ {
  Retorna o diretorio pai do caminho dado

  Deprecated at 19/5/2005 use TFileHnd.ParentDir
}
var
	Res: string;
begin
	if IsUNCName(Dir) then begin { UNC }
		Res    := ExtractUNCResource(Dir);
		Result := ReplaceSubString(Dir, Res, EmptyStr);
		Result := ParentDir(Result);
		if (Result = PathDelim) or (Result = EmptyStr) then begin
			Result := Res;
		end;
	end else begin { driver logico }
		{ Precisa da funcao ExtractFilePath do SysUtils do Delphi }
		Result := ExtractFilePath(Dir);
		Result := Copy(Result, 1, Length(Result) - 1);
		if Result = '' then begin
			Result := PathDelim;
		end else begin
			if Result[Length(Result)] = ':' then begin
				Result := Result + PathDelim;
			end;
		end;
	end;
end;

function PathNameLong(const ShortPathName: string): string;
{ {
  Converte nome do caminho para o formato longo
}
var
	LastSlash, TempPathPtr: PChar;
	PathBuffer            : array [0 .. MAX_PATH] of char;
begin
	Result := EmptyStr;
	StrPCopy(PathBuffer, ShortPathName); //Salva copia em buffer para alocacao dos marcadores #0�s
	TempPathPtr := PathBuffer;
	LastSlash   := StrRScan(TempPathPtr, PathDelim);
	while LastSlash <> nil do begin
		Result := PathDelim + ShortToLongFileName(TempPathPtr) + Result;
		if LastSlash <> nil then begin
			{$WARN UNSAFE_CODE OFF}
			LastSlash^ := char(0);
			{$WARN UNSAFE_CODE ON}
			LastSlash := StrRScan(TempPathPtr, PathDelim);
		end;
	end;
	Result := TempPathPtr + Result;
end;

function RemoveFileExtension(const FileName: string): string;
{ {
  Remove a extensao do nome do arquivo
}
var
	Pi, Pf: PChar;
begin
	{$WARN UNSAFE_CODE OFF}
	Pi := PChar(FileName);
	Pf := StrRScan(Pi, '.');
	if Pf <> nil then begin
		Pf^    := #0;
		Result := string(Pi);
		Pf^    := '.';
	end else begin
		Result := FileName;
	end;
	{$WARN UNSAFE_CODE ON}
end;

function RmDirEx(const DirName: string): boolean;
{ {
  Rotina depreciada - Usar TFileHnd.RmDir()

  Apaga um diretorio com seus subdiretoris junto
}
var
	SR         : TSearchRec;
	NextProcDir: string;
begin
	Result := False;
	if not(DirectoryExists(DirName)) then begin
		Result := True;
		Exit;
	end;
	GetDir(0, NextProcDir);
	if Pos(UpperCase(DirName), UpperCase(NextProcDir)) <> 0 then begin
		{ Nao pode apagar caminho em uso = Diretorio atual }
		Exit;
	end;
	repeat
		NextProcDir := FindFirstChildDir(DirName);
		if NextProcDir <> EmptyStr then begin
			if not RmDirEx(NextProcDir) then begin
				Exit;
			end;
		end;
	until NextProcDir = EmptyStr;
	while FindFirst(DirName + PathDelim + '*.*', faAnyFile - faVolumeID - faDirectory, SR) = 0 do begin
		if FileSetAttr(DirName + PathDelim + SR.Name, faArchive) = 0 then begin
			if not DeleteFile(DirName + PathDelim + SR.Name) then begin
				FindClose(SR);
				Exit;
			end;
		end;
		FindClose(SR);
	end;
	if TFileHnd.ParentDir(DirName) <> DirName then begin { Eliminando raiz de recurso }
		try
			FileSetAttr(DirName, faDirectory);
			if not RemoveDir(DirName) then begin
				Result := False;
				Exit;
			end;
		except
			on Exception do begin
				Exit;
			end;
		end;
	end;
	Result := True;
end;

function SetFileTimeProperties(const FileName: string; var CreateDate, LastAccessDate, LastWriteDate: TDateTime): Integer;
	overload; platform;
{ {
  Ajusta as datas de criacao/acesso/escrita de um dado arquivo
}
var
	Hnd: THandle;
begin
	{ TODO -oroger -clib : Avaliar das chamadas abaixo qual a menos restritiva }
	{
	  Hnd := CreateFile(PChar(Filename), GENERIC_WRITE, 0, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL or
	  FILE_FLAG_SEQUENTIAL_SCAN, 0);
	}
	Hnd := CreateFile(PChar(FileName), GENERIC_WRITE, FILE_SHARE_WRITE, nil, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, 0);
	try
		if Hnd <> INVALID_HANDLE_VALUE then begin
			Result := SetFileTimeProperties(Hnd, CreateDate, LastAccessDate, LastWriteDate);
		end else begin
			Result := GetLastError();
		end;
	finally
		CloseHandle(Hnd);
	end;
end;

function SetFileTimeProperties(FileHandle: THandle; var CreateDate, LastAccessDate, LastWriteDate: TDateTime): Integer; overload;
{ {
  Ajusta as datas de criacao/acesso/escrita de um dado arquivo

  Notas : O arquivo deve ter acesso GENERIC_WRITE


  Exemplo:
  hf := CreateFile ( PChar(FFilename), GENERIC_WRITE, 0, nil, OPEN_EXISTING, 0, //FILE_ATTRIBUTE_NORMAL or FILE_FLAG_SEQUENTIAL_SCAN , 0);
}
var
	DCreate, DAccess, DWrite: TFileTime;
	ST                      : TSystemTime;
begin
	Result := ERROR_SUCCESS;
	SetLastError(Result);
	DateTimeToSystemTime(CreateDate, ST);
	SystemTimeToFileTime(ST, DCreate);
	LocalFileTimeToFileTime(DCreate, DCreate);
	DateTimeToSystemTime(LastAccessDate, ST);
	SystemTimeToFileTime(ST, DAccess);
	LocalFileTimeToFileTime(DAccess, DAccess);
	DateTimeToSystemTime(LastWriteDate, ST);
	SystemTimeToFileTime(ST, DWrite);
	LocalFileTimeToFileTime(DWrite, DWrite);
	{$WARN UNSAFE_CODE OFF}
	if not SetFileTime(FileHandle, @DCreate, @DAccess, @DWrite) then begin
		Result := GetLastError();
	end;
	{$WARN UNSAFE_CODE ON}
end;

function SetKeyGenFileValue(const FileName: string; Value, TimeOut: Integer): Integer;
{ {
  Salva em arquivo de usos exclusivo valor limitado ao periodo de espera
}
var
	FHnd  : TextFile;
	FTime : TDateTime;
	Sucess: boolean;
begin
	FTime := IncTime(Now, 0, 0, 0, TimeOut);
	AssignFile(FHnd, FileName);
	Sucess := False;
	Result := ERROR_TIMEOUT;
	try
		{$IOCHECKS OFF}
		repeat
		begin
			ReWrite(FHnd);
			if IOResult = 0 then begin
				Sucess := True;
			end
		end;
		until (Sucess) or (Now > FTime);
		{$IOCHECKS OFF}
		if Sucess then begin
			WriteLn(FHnd, IntToStr(Value));
		end else begin
			if IOResult <> 0 then begin
				Result := IOResult;
			end else begin
				Result := ERROR_TIMEOUT;
			end;
		end;
	finally
		if Sucess then begin
			CloseFile(FHnd);
			Result := NO_ERROR;
		end;
	end;
end;

function ShortToLongFileName(const ShortName: string): string;
{ {
  Retorna o nome longo de um arquivo passado como nome curto
}
var
	Temp        : TWin32FindData;
	SearchHandle: THandle;
begin
	SearchHandle := FindFirstFile(PChar(ShortName), Temp);
	if SearchHandle <> INVALID_HANDLE_VALUE then begin
		Result := string(Temp.cFileName);
	end else begin
		Result := EmptyStr;
	end;
	Windows.FindClose(SearchHandle);
end;

function SlashRem(const Path: string; PreserveRoot: boolean = False): string; deprecated;
{ {
  Remove barra do fim do caminho

  Deprecated : Use TFileHnd.SlashRem()
}
begin
	{$WARN UNSAFE_CODE OFF}
	if AnsiLastChar(Path)^ <> PathDelim then begin
		Result := Path;
	end else begin
		Result := Copy(Path, 1, Length(Path) - 1);
		if PreserveRoot then begin
			if IsRootDir(Path) then begin
				Result := Result + PathDelim;
			end;
		end;
	end;
	{$WARN UNSAFE_CODE ON}
end;

function SlashSep(const Path, FileName: string): string;
{ {
  Returns a path tha forces PathDelim at your final
}
begin
	{$WARN UNSAFE_CODE OFF}
	if AnsiLastChar(Path)^ <> PathDelim then begin
		Result := Path + PathDelim + FileName;
	end else begin
		Result := Path + FileName;
	end;
	{$WARN UNSAFE_CODE ON}
end;

function SpaceFree(const PathName: string): int64;
{ {
  Retorna o espaco em bytes livres para determinado caminho
}
var
	Drive                                                         : byte;
	os                                                            : OSVERSIONINFO;
	TotalFree, TotalExisting                                      : int64;
	SectorsPerCluster, BytesPerSector, FreeClusters, TotalClusters: cardinal;
begin
	if Pos(':', PathName) <> 0 then begin //Unidade de disco
		Drive := Ord((ExtractFileDrive(UpperCase(PathName))[1]));
		Drive := Drive - Ord('A') + 1;
		{$IFDEF WIN32}
		Result := DiskFree(Drive);
		{$ELSE}
		Result := _DiskFree(Drive);
		{$ENDIF}
	end else begin //Recurso via UNC
		os.dwOSVersionInfoSize := SizeOf(OSVERSIONINFO);
		GetVersionEx(os);
		if (os.dwMajorVersion = 4) and (os.dwBuildNumber < 1000) and (os.dwPlatformId = 1) then begin //Usar modo OSR1
			if GetDiskFreeSpace(PChar(PathName), SectorsPerCluster, BytesPerSector, FreeClusters, TotalClusters) then begin
				Result := (SectorsPerCluster * BytesPerSector * FreeClusters);
			end else begin
				Result := -1;
			end;
		end else begin //Usar os servicos do NT ou Win95B ou superior
			{$WARN UNSAFE_CODE OFF}
			if not GetDiskFreeSpaceEx(PChar(PathName), Result, TotalExisting, @TotalFree) then begin
				Result := -1;
			end;
			{$WARN UNSAFE_CODE ON}
		end;
	end;
end;

class procedure TFileHnd.BuildVersionInfo(const FileName: string; var V1, V2, V3, V4: Word);
var
	VerInfoSize, VerValueSize, Dummy: DWORD;
	VerInfo                         : Pointer;
	VerValue                        : PVSFixedFileInfo;
	Path                            : string;
begin
	try
		if (FileName = EmptyStr) then begin
			Path := ParamStr(0);
		end else begin
			Path := FileName;
		end;
		VerInfoSize := GetFileVersionInfoSize(PChar(Path), Dummy);
		if (VerInfoSize > 0) then begin
			VerInfo := GetMemory(VerInfoSize);
			try
				GetFileVersionInfo(PChar(Path), 0, VerInfoSize, VerInfo);
				VerQueryValue(VerInfo, '\' { dont localize } , Pointer(VerValue), VerValueSize);
				{$WARN UNSAFE_CODE OFF}
				V1 := VerValue^.dwFileVersionMS shr 16;
				V2 := VerValue^.dwFileVersionMS and $FFFF;
				V3 := VerValue^.dwFileVersionLS shr 16;
				V4 := VerValue^.dwFileVersionLS and $FFFF;
				{$WARN UNSAFE_CODE ON}
			finally
				FreeMemory(VerInfo);
			end;
		end else begin //informa��o de vers�o n�o encontrada neste arquivo
			V1 := 0;
			V2 := 0;
			V3 := 0;
			V4 := 0;
		end;
	except
		V1 := 0;
		V2 := 0;
		V3 := 0;
		V4 := 0;
	end;
end;

class function TFileHnd.ChangeFileName(const OriginalFullPath, FileName: string): string;
begin
	Result := ExtractFilePath(OriginalFullPath);
	Result := TFileHnd.ConcatPath([Result, FileName]);
end;

class function TFileHnd.ConcatPath(Paths: array of string): string;
var
	RPart: string;
	i    : Integer;
begin
	if high(Paths) < 1 then begin
		Exit;
	end;
	Result := Paths[0];
	for i  := 1 to high(Paths) do begin
		if Result <> EmptyStr then begin
			while Result[Length(Result)] = PathDelim do begin
				Delete(Result, Length(Result), 1);
			end;
		end;
		RPart := Paths[i];
		if RPart <> EmptyStr then begin
			while RPart[1] = PathDelim do begin
				Delete(RPart, 1, 1);
			end;
			Result := Result + PathDelim + RPart;
		end;
	end;
end;

{ -------------------------------------------------------------------------------------------------------------------------------- }
class function TFileHnd.CopyDir(const SourceDir, DestDir: TFilename): Integer;
{ {
  Revision: 5/6/2008 - roger
}
var
	SearchRec     : TSearchRec;
	DosError      : Integer;
	Path, DestPath: TFilename;
begin
	if not ForceDirectories(DestDir) then begin
		Result := GetLastError();
	end else begin
		Result   := ERROR_SUCCESS;
		Path     := SourceDir;
		DestPath := TFileHnd.SlashAdd(DestDir);
		Path     := TFileHnd.SlashAdd(Path);
		DosError := FindFirst(Path + '*.*', faAnyFile, SearchRec);
		try
			while DosError = 0 do begin
				if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then begin
					if (SearchRec.Attr and faDirectory) = faDirectory then begin
						Result := CopyDir(Path + SearchRec.Name, TFileHnd.SlashAdd(DestDir) + SearchRec.Name);
					end else if (not Windows.CopyFile(PChar(Path + SearchRec.Name), PChar(DestPath + SearchRec.Name), True)) then
					begin
						Result := GetLastError();
						Exit;
					end;
				end;
				DosError := FindNext(SearchRec);
			end;
			FindClose(SearchRec);
		except
			FindClose(SearchRec); //usado apenas para n�o omitir chamada
			raise;
		end;
	end;
end;

class function TFileHnd.CopyFile(const SourceFile, DestFile: TFilename; Overwrite, ResetProtections: boolean): Integer;
var
	Attr: Integer;
begin
	//Verifica se destino existe
	if (FileExists(DestFile)) then begin
		if (not Overwrite) then begin
			Result := ERROR_FILE_EXISTS;
			Exit;
		end else begin
			//Como se sobrescreve devemos testar os atributos de prote��o
			if (ResetProtections) then begin
				Attr := GetFileAttributes(PWideChar(DestFile));
				Attr := (Attr) and not(FILE_ATTRIBUTE_HIDDEN or FILE_ATTRIBUTE_SYSTEM or FILE_ATTRIBUTE_READONLY);
				if (not SetFileAttributes(PWideChar(DestFile), Attr)) then begin
					Result := GetLastError();
					Exit;
				end;
			end;
		end;
	end;
	//Chegando aqui tentamos gravar no destino
	if (Windows.CopyFile(PWideChar(SourceFile), PWideChar(DestFile), True)) then begin
		Result := ERROR_SUCCESS;
	end else begin
		Result := GetLastError();
	end;
end;

class function TFileHnd.CreateTempFileName(const Prefix: PChar; const Number: byte; CreateFile: boolean): string;
var
	Path, Name: PChar;
	f         : file;
begin
	Result := '';
	Path   := StrAlloc(256);
	name   := StrAlloc(512);
	if GetTempPath(255, Path) <> 0 then begin
		if GetTempFileName(Path, Prefix, Number, name) <> 0 then begin
			Result := StrPas(name);
		end;
	end;
	StrDispose(Path);
	StrDispose(name);
	if (CreateFile) then begin //Se eh para criar
		AssignFile(f, Result);
		ReWrite(f); //Arquivo zerado
		CloseFile(f);
	end;
end;

class function TFileHnd.DeepExistingPath(const Path: string): string;
begin
	Result := Path;
	while ((not DirectoryExists(Result) and (not IsRootDir(Result)))) do begin
		Result := ParentDir(Result);
	end;
	if (IsRootDir(Result)) then begin
		Result := GetUserMyDocuments();
		if Result = EmptyStr then begin
			Result := GetUserHomeDir();
		end;
	end;
end;

class function TFileHnd.ExpandFilename(const BasePath, Path: string): string;
const
	PARENT_DIR_TOKEN = '..' + SysUtils.PathDelim;

	procedure LSRRemapPaths(var localBase, localPath: string);
	begin
		while (TStrHnd.startsWith(localPath, PARENT_DIR_TOKEN)) do begin
			Delete(localPath, 1, Length(PARENT_DIR_TOKEN));
			localBase := TFileHnd.SlashRem(ExtractFilePath(SlashRem(localBase)));
		end;
	end;

var
	c                  : char;
	rightPart, leftPart: string;
	posPDir            : Integer;

begin
	if (Path <> EmptyStr) then begin
		c := GetIChar(Path, 1); //Pega 1o caracter para testar se caminho base serah diretamente agregado
		if ((c = SysUtils.PathDelim) or (c = '.')) then begin //Agrega ao final de BasePath diretamente
			Result := TFileHnd.ConcatPath([BasePath, Path]);
		end else begin //Verifica se path inicia-se com ".."
			rightPart := Path;
			leftPart  := BasePath;
			if (TStrHnd.startsWith(rightPart, PARENT_DIR_TOKEN)) then begin
				//Retorna aos diretorios pais ate finalizar as recorrencias
				LSRRemapPaths(leftPart, rightPart);
				Result := TFileHnd.ConcatPath([leftPart, rightPart]);
			end else begin
				//Checa se prefixo bate e remota caminhos
				posPDir := Pos(PARENT_DIR_TOKEN, rightPart);
				if (posPDir > 0) then begin
					if (Copy(leftPart, 1, posPDir - 1) = Copy(rightPart, 1, posPDir - 1)) then begin
						//Iniciam com o mesmo valor -> chamada a remapeamento
						leftPart := Copy(leftPart, 1, posPDir - 1);
						Delete(rightPart, 1, posPDir - 1);
						LSRRemapPaths(leftPart, rightPart);
						Result := TFileHnd.ConcatPath([leftPart, rightPart]);
					end;
				end;
			end;
		end;
	end else begin
		Result := SlashRem(BasePath);
	end;
end;

class function TFileHnd.ExtractFilenamePure(const FileName: string): string;
var
	i: Integer;
begin
	Result := ExtractFileName(FileName);
	i      := LastDelimiter('.', Result);
	if (i > 0) then begin
		Result := Copy(Result, 1, i - 1);
	end;
end;

class function TFileHnd.FileSize(const FileName: string): int64;
{ {
  Revision: 3/6/2008 - roger
}
var
	SR: TSearchRec;
begin
	{$WARN SYMBOL_PLATFORM OFF}
	if FindFirst(FileName, faAnyFile, SR) = 0 then begin
		Result := (SR.FindData.nFileSizeHigh shl 32) + SR.FindData.nFileSizeLow;
		FindClose(SR);
	end else begin
		Result := -1;
	end;
	{$WARN SYMBOL_PLATFORM OFF}
end;

{ -------------------------------------------------------------------------------------------------------------------------------- }
class function TFileHnd.FileTimeProperties(FileHandle: THandle; var CreateDate, LastAccessDate, LastWriteDate: TDateTime): Integer;
var
	DCreate, DAccess, DWrite: TFileTime;
begin
	{$WARN UNSAFE_CODE OFF}
	if GetFileTime(FileHandle, @DCreate, @DAccess, @DWrite) then begin
		Result := 0;
		FileTimeToLocalFileTime(DCreate, DCreate); //Criacao
		CreateDate := TFileHnd.FileTimeToDateTime(DCreate);
		FileTimeToLocalFileTime(DAccess, DAccess); //Acesso
		LastAccessDate := TFileHnd.FileTimeToDateTime(DAccess);
		FileTimeToLocalFileTime(DWrite, DWrite); //Escrita
		LastWriteDate := TFileHnd.FileTimeToDateTime(DWrite);
	end else begin
		Result := GetLastError();
	end;
	{$WARN UNSAFE_CODE ON}
end;

class function TFileHnd.FileTimeChangeTime(const FileName: string): TDateTime;
var
	Dummy: TDateTime;
begin
	if (FileTimeProperties(FileName, Dummy, Dummy, Result) <> ERROR_SUCCESS) then
		Result := -1;
end;

class function TFileHnd.FileTimeProperties(const FileName: string;
	var CreateDate, LastAccessDate, LastWriteDate: TDateTime): Integer;
var
	SR                      : TSearchRec;
	DCreate, DAccess, DWrite: TFileTime;
	ret                     : boolean;
begin
	Result := FindFirst(FileName, faAnyFile, SR);
	if (Result = ERROR_SUCCESS) then begin
		try
			//Criacao
			DCreate := SR.FindData.ftCreationTime;
			ret     := FileTimeToLocalFileTime(DCreate, DCreate);
			if not ret then begin
				Result := GetLastError();
				Exit;
			end;
			CreateDate := TFileHnd.FileTimeToDateTime(DCreate);

			//Acesso
			DAccess := SR.FindData.ftLastAccessTime;
			ret     := FileTimeToLocalFileTime(DAccess, DAccess);
			if not ret then begin
				Result := GetLastError();
				Exit;
			end;
			LastAccessDate := TFileHnd.FileTimeToDateTime(DAccess);

			//escrita
			DWrite := SR.FindData.ftLastWriteTime;
			FileTimeToLocalFileTime(DWrite, DWrite);
			if not ret then begin
				Result := GetLastError();
				Exit;
			end;
			LastWriteDate := TFileHnd.FileTimeToDateTime(DWrite);
		finally
			FindClose(SR);
		end;
	end;
end;

class function TFileHnd.FileTimeToDateTime(FTime: TFileTime): TDateTime;
{ {
  Revision: 10/8/2006 - Roger
}
var
	ST: TSystemTime;
begin
	FileTimeToSystemTime(FTime, ST);
	Result := EncodeDate(ST.wYear, ST.wMonth, ST.wDay) + EncodeTime(ST.wHour, ST.wMinute, ST.wSecond, ST.wMilliSeconds);
end;

class function TFileHnd.FirstOccurrence(const Path, Mask: string): string;
var
	SR: TSearchRec;
begin
	try
		if (FindFirst(TFileHnd.ConcatPath([Path, Mask]), faAllFiles, SR) = ERROR_SUCCESS) then begin
			Result := TFileHnd.ConcatPath([Path, SR.FindData.cFileName]);
		end else begin
			Result := EmptyStr;
		end;
	finally
		FindClose(SR);
	end;
end;

class function TFileHnd.ForceFileExtension(const OriginalName, DesiredExtension: string): string;
var
	originalExt, ForceExt: string;
begin
	//Localiza qual a extensao e se for diferente da desejada adciona-a
	if DesiredExtension = EmptyStr then begin
		Result := OriginalName;
	end else begin
		if DesiredExtension[1] = '.' then begin
			ForceExt := DesiredExtension;
		end else begin
			ForceExt := '.' + DesiredExtension;
		end;
		originalExt := ExtractFileExt(OriginalName);  //Vem com o "."
		if SameText(originalExt, ForceExt) then begin //Adiciona "." a comparacao
			Result := OriginalName;
		end else begin
			Result := OriginalName + ForceExt;
		end;
	end;
end;

{ -------------------------------------------------------------------------------------------------------------------------------- }
class procedure TFileHnd.ForceFilename(const FileName: string);
var
	ST: TFileStream;
begin
	if (not FileExists(FileName)) then begin
		if (not DirectoryExists(ParentDir(FileName))) then begin
			if (not ForceDirectories(ParentDir(FileName))) then begin
				TAPIHnd.CheckAPI(GetLastError());
			end;
		end;
		ST := TFileStream.Create(FileName, fmCreate);
		ST.Free;
	end;
end;

{ -------------------------------------------------------------------------------------------------------------------------------- }
class function TFileHnd.GetFileSizeEx(const FileName: string): int64;
{ {
  Revision: 3/6/2008 - roger
}
var
	SR  : TSearchRec;
	List: TStringList;
	x   : Integer;
begin
	{$WARN SYMBOL_PLATFORM OFF}
	if FindFirst(FileName, faAnyFile, SR) = 0 then begin
		try
			if ((SR.Attr and faDirectory) = faDirectory) then begin //Necessita varrer os subdirs
				List := TStringList.Create;
				try
					FileHnd.ListDirFilesNames(FileName, '*.*', faAnyFile, True, List);
					Result := 0;
					for x  := 0 to List.Count - 1 do begin
						Inc(Result, TFileHnd.FileSize(List.Strings[x]));
					end;
				finally
					List.Free;
				end;
			end else begin
				Result := (SR.FindData.nFileSizeHigh shl 32) + SR.FindData.nFileSizeLow;
			end;
		finally
			SysUtils.FindClose(SR);
		end;
	end else begin
		Result := -1;
	end;
	{$WARN SYMBOL_PLATFORM ON}
end;

class function TFileHnd.GetUserHomeDir: string;
var
	envValue: string;
	Path    : array [0 .. MAX_PATH] of char;
begin
	envValue := TAPIHnd.GetEnvironmentVar('USERPROFILE');
	if (envValue = EmptyStr) then begin
		envValue := TFileHnd.ConcatPath([TAPIHnd.GetEnvironmentVar('HOMEDRIVE'), TAPIHnd.GetEnvironmentVar('HOMEPATH')]);
		if DirectoryExists(envValue) then begin
			Result := envValue;
		end else begin
			if SHGetSpecialFolderPathW(0, Path, CSIDL_PROFILE, False) then begin
				Result := Path;
			end else begin
				Result := EmptyStr;
			end;
		end;
	end;
end;

class function TFileHnd.GetUserMyDocuments: string;
var
	Path: array [0 .. MAX_PATH] of char;
begin
	if SHGetSpecialFolderPathW(0, Path, CSIDL_MYDOCUMENTS, False) then begin
		Result := Path;
	end else begin
		Result := EmptyStr;
	end;
end;

class function TFileHnd.IsValidFilename(const FileName: string; ConstrainLevel: Integer): boolean;
{ {
  Revision: 27/7/2005 - Roger
}
var
	CSet : TSysCharSet;
	LPart: string;
begin
	if (FileName = EmptyStr) then begin
		Result := False;
		Exit;
	end;
	case (ConstrainLevel) of
		1: begin
				CSet := INVALID_FILENAME_CHARS_LOW_CRITIC;
			end;
		2: begin
				CSet := INVALID_FILENAME_CHARS_MEDIUM_CRITIC;
			end;
	else begin
			CSet := INVALID_FILENAME_CHARS_HIGH_CRITIC;
		end;
	end;
	LPart := ExtractFilePath(FileName);
	if (LPart = FileName) then begin //raiz ou nome final da sequencia
		Result := (Length(LPart) = 3) and CharInSet(GetIChar(LPart, 2), ['a' .. 'Z']) and (GetIChar(LPart, 3) = PathSep);
		//algo do tipo c:\
		Result := Result or (TStrHnd.StrPosChar(ExtractFileName(FileName), CSet) = 0);
	end else begin
		if (LPart = EmptyStr) then begin
			Result := (TStrHnd.StrPosChar(ExtractFileName(FileName), CSet) = 0);
		end else begin
			Result := TFileHnd.IsValidFilename(LPart, ConstrainLevel);
			Result := Result and (TStrHnd.StrPosChar(ExtractFileName(FileName), CSet) = 0);
		end;
	end;
end;

class function TFileHnd.IsWritable(const FileName: string): boolean;
var
	FS: TFileStream;
begin
	Result := False;
	if FileExists(FileName) then begin
		try
			FS := TFileStream.Create(FileName, fmOpenWrite);
			FS.Free;
			Result := True;
		except
			Exit; //Apenas retorna false
		end;
	end else begin
		try
			FS := TFileStream.Create(FileName, fmCreate);
			FS.Free;
			Result := True;
			DeleteFile(FileName); //ignora erros de dele��o(objetivo apenas escrever)
		except
			Exit; //Apenas retorna false
		end;
	end;
end;

class function TFileHnd.MakeDefaultFileExtension(const OriginalFileName, DefaultExtension: string): string;
var
	Ext: string;
begin
	Ext := SysUtils.ExtractFileExt(OriginalFileName);
	if (Ext = EmptyStr) then begin
		Result := OriginalFileName + DefaultExtension;
	end else begin
		Result := OriginalFileName;
	end;
end;

{ -------------------------------------------------------------------------------------------------------------------------------- }
class function TFileHnd.NextFamilyFilename(const BaseFilename: string): string;
{ {
  Revision - 11/3/2010 - roger Passou a funcionar para diretorios da mesma forma que para arquivos

  Revision: 5/12/2005 - Roger - original
}
var
	TargetExt, Prefix: string;
	TargetCount      : Integer;
begin
	TargetCount := 0;
	TargetExt   := ExtractFileExt(BaseFilename);
	Prefix      := ChangeFileExt(BaseFilename, EmptyStr);
	repeat
		Inc(TargetCount);
		Result := Prefix + '(' + IntToStr(TargetCount) + ')' + TargetExt;
	until ((not FileExists(Result)) and (not DirectoryExists(Result)));
end;

class function TFileHnd.ParentDir(const FileName: string): string;
begin
	Result := SlashRem(ExtractFilePath(FileName));
end;

class function TFileHnd.ParentDir(const FileName: string; Existing: boolean): string;
{ {
  Revision: 11/7/2006 - Roger
}
begin
	Result := TFileHnd.ParentDir(FileName);
	if (Existing) then begin
		if (not DirectoryExists(Result)) then begin
			if (IsRootDir(Result)) then begin //tudo o possivel tentado
				Result := EmptyStr;
			end else begin
				if (Result <> EmptyStr) then begin
					Result := TFileHnd.ParentDir(Result, True);
				end;
			end;
		end;
	end;
end;

class function TFileHnd.RmDir(const Path: string): Integer;
{ {
  Revision: 26/9/2005 - Roger
}
	function LSRDeleteFile(const DirName: string; FileParam: TEnumFileParam): Integer;
	{ {
	  Apaga o arquivo dado se o mesmo for um sub-diretorio chama recursivamente a TFileHnd.RmDir()
	  Revision: 26/9/2005 - Roger
	}
	var
		RemFilename: string;
	begin
		Result := ERROR_SUCCESS;
		if (FileParam.SR.Name <> '..') and (FileParam.SR.Name <> '.') then begin
			RemFilename := DirName + PathDelim + FileParam.SR.Name;
			if (not DeleteFile(RemFilename)) then begin //tentar remover atributos impeditivos do arquivo
				if (not SetFileAttributes(PChar(RemFilename), FILE_ATTRIBUTE_NORMAL)) then begin
					Result := GetLastError();
				end else begin
					if (not DeleteFile(RemFilename)) then begin //outro motivo de falha
						Result := GetLastError();
					end;
				end;
			end;
		end;
	end;

var
	ChildDir : string;
	FileParam: TEnumFileParam;
begin
	Result := ERROR_SUCCESS;
	if (DirectoryExists(Path)) then begin
		ChildDir := FindFirstChildDir(Path);
		while ((Result = ERROR_SUCCESS) and (ChildDir <> EmptyStr)) do begin
			Result   := TFileHnd.RmDir(ChildDir);
			ChildDir := FindFirstChildDir(Path);
		end;
		if (Result = ERROR_SUCCESS) then begin
			FileParam := TEnumFileParam.Create;
			try
				Result := EnumFiles(Path, TList(nil), FileParam, TEnumFileProc(@LSRDeleteFile));
				if (Result = ERROR_SUCCESS) then begin //Conseguiu limpar seus arquivos
					if (not RemoveDirectory(PChar(Path))) then begin
						Result := GetLastError();
					end;
				end;
			finally
				FileParam.Free;
			end;
		end;
	end;
end;

class function TFileHnd.SlashAdd(const Path: string): string;
{ {
  Revision: 5/6/2008 - roger
}
begin
	{$WARN UNSAFE_CODE OFF}
	if AnsiLastChar(Path)^ <> PathDelim then begin
		Result := Path + PathDelim;
	end else begin
		Result := Path;
	end;
	{$WARN UNSAFE_CODE ON}
end;

class function TFileHnd.SlashRem(const Path: string; PreserveRoot: boolean = False): string;
{ {
  Revision: 30/10/2006 - Roger
}
var
	P: PChar;
begin
	{$WARN UNSAFE_CODE OFF}
	P := AnsiLastChar(Path);
	if (P = nil) or (P^ <> SysUtils.PathDelim) then begin //SysUtils.PathDelim agora definido como constante
		Result := Path;
	end else begin
		Result := Copy(Path, 1, Length(Path) - 1);
		if PreserveRoot then begin
			if IsRootDir(Path) then begin
				Result := Result + SysUtils.PathDelim;
			end;
		end;
	end;
	{$WARN UNSAFE_CODE ON}
end;

{ -------------------------------------------------------------------------------------------------------------------------------- }
class function TFileHnd.VersionInfo(const FileName: string): string;
const
	STR_DOT = '.';
var
	V1, V2, V3, V4: Word;
begin
	BuildVersionInfo(FileName, V1, V2, V3, V4);
	Result := IntToStr(V1) + STR_DOT + IntToStr(V2) + STR_DOT + IntToStr(V3) + STR_DOT + IntToStr(V4);
end;

end.
