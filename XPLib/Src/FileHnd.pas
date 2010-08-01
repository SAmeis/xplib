{$IFDEF FileHnd}
	{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I XPLib.inc}

unit FileHnd;

{{
Implementa classes e rotinas para tratamento de arquivos.

Revision: 5/6/2008 - roger
1 - Trocadas as referências a '\" pela variável SysUtils.PathDelim
2 - Atribuição correta de retorno para rotina FileCopy

Revision: 10/8/2006 - Roger
    Rotina FileTimeToDateTime foi deslocada para TFileHnd.FileTimeToDateTime.
    Rotinas(overloaded) FileTimeProperties foram deslocadas para TFileHnd.FileTimeToDateTime.

}
interface

uses Windows, Classes, SysUtils;

const
    //Conjunto de todos os atributos dos arquivos. Lembrar que FILE_ATTRIBUTE_NORMAL mutuamente exclusivo -> nenhum atributo existente
    faAllFiles = FILE_ATTRIBUTE_ARCHIVE or FILE_ATTRIBUTE_COMPRESSED or FILE_ATTRIBUTE_DIRECTORY or FILE_ATTRIBUTE_HIDDEN or
        FILE_ATTRIBUTE_NORMAL or FILE_ATTRIBUTE_OFFLINE or FILE_ATTRIBUTE_READONLY or FILE_ATTRIBUTE_SYSTEM or
        FILE_ATTRIBUTE_TEMPORARY;
    //    faAllFiles =   faReadOnly or  faHidden or faSysFile or faVolumeID or faDirectory or faArchive;

    //Uso para validacao de nomes de arquivos, usar as constantes colocadas abaixo
    INVALID_FILE_NAME_CHARS1_STR = '*?"\/<>	:{}[]';//tab inside
    {1 constante deprecated usar familia INVALID_FILENAME_CHARS_????_CRITIC }
    INVALID_FILE_NAME_CHARS2_STR = INVALID_FILE_NAME_CHARS1_STR + '+&Ççáéíóúäëïöüàèìòùãõ'; //acentos
    {1 constante deprecated usar familia INVALID_FILENAME_CHARS_????_CRITIC }
    INVALID_FILE_NAME_CHARS3_STR = INVALID_FILE_NAME_CHARS2_STR + '- &'; //unix
    {1 constante deprecated usar familia INVALID_FILENAME_CHARS_????_CRITIC }


    //Uso para validação de nomes de arquivos, ***SUPLANTA*** as constantes acima.
    INVALID_FILENAME_CANONIC_CHARS: TSysCharSet       = ['*', '?', '"', '\', '/', '<', '>', #9{tab}, ':', '{', '}', '[', ']', '¦'];
    {1 Caracteres usados como tokens nos sistemas operacionais em geral }
    INVALID_FILENAME_LINUX_EXT: TSysCharSet           = ['-', ' '{espaco}, '&'];
    {1 Caracteres usados como tokens no linux que excedem os encontrados em  INVALID_FILENAME_CANONIC_CHARS }
    INVALID_FILENAME_CHARS_HIGH_CRITIC: TSysCharSet   = [#0..#32] + [#128..#255] +
        ['*', '?', '"', '\', '/', '<', '>', #9{tab}, ':', '{', '}', '[', ']', '¦'] { INVALID_FILENAME_CANONIC_CHARS } +
        ['-', ' '{espaco}, '&'] {INVALID_FILENAME_LINUX_EXT};
    {1 Conjunto de caracteres que podem ser usados nos sistemas operacionais alvos de nossos aplicativos }
    INVALID_FILENAME_CHARS_MEDIUM_CRITIC: TSysCharSet = [#0..#32] + [#128..#255] +
        ['*', '?', '"', '\', '/', '<', '>', #9{tab}, ':', '{', '}', '[', ']', '¦'] {INVALID_FILENAME_CANONIC_CHARS};
    {1 Conjunto de caracteres que podem ser usados sem muito risco no Windows usando apenas o ASCII abaixo de #127 }
    INVALID_FILENAME_CHARS_LOW_CRITIC: TSysCharSet    = [#0..#32] +
        ['*', '?', '"', '\', '/', '<', '>', #9{tab}, ':', '{', '}', '[', ']', '¦'] {INVALID_FILENAME_CANONIC_CHARS};
    {1 Conjunto de caracteres que podem ser usados com algum risco no Windows usando too o ASCII }

    IDEAL_FILE_BLOCK_SIZE = 4096; //blocos de 4K parecem ser hoje o melhor valor

    EOL_DOS = #13#10;

type
    TEnumFileParam = class(TObject)
 {{
 Class for repass to Enumeration callBacks, instances of TEnumFileParam contains information about the current file.
 }
    private
        FErrorMsg : PChar;
        FFileName : PChar;
        procedure SetErrorMsg(const Value : PChar);
        procedure SetFileName(const Value : PChar);
    public
        Data :      Pointer;
        ErrorCode : Integer;
        SR :        ^TSearchRec;
        constructor Create;
        destructor Destroy; override;
        property ErrorMsg : PChar read FErrorMsg write SetErrorMsg;
        property FileName : PChar read FFileName write SetFileName;
    end;

    TEnumFileProc = function(const FileName : string; FileParam : TEnumFileParam) : Integer;

//Apaga o arquivo independente de seus atributos
function __DeleteFile(const FileName : string) : boolean; platform; deprecated;
//Verifica se arquivo existe, extendido para sub-diretorios
function __FileExists(const Name : string) : boolean; platform;
//Abre arquivo com valor para auto-incremento para ler erro ver LasErrorCode
function AddKeyGenFile(const FileName : string; TimeOut : Integer) : Integer;
//Junta as parte de um caminho
function ConcatPath(Paths : array of string) : string; deprecated;
//Conta a quantidade de arquivos que obedecem uma dada mascara
function CountMaskFilesMatch(const PathMask : string) : Integer; platform;
//Cria um nome de arquivo temporario
function CreateTempFileName(const Prefix : PChar; const Number : byte) : string;
//Apaga arquivos com uma dada mascara
function DeleteMaskedFiles(const MaskFiles : string) : boolean; platform;
//Apaga a pasta e seu conteudo
function Deltree(const Path : string; IncludeSelf : boolean = True; IgnoreErrors : boolean = True;
    IgnoreReadOnly : boolean = True; IgnoreSystem : boolean = True) : Integer;
//Preenche Lista com os arquivos do path passado, omite sub-dirs
function DirFileList(List : TStrings; const Dir : string; Masks : array of string) : Integer;
//Retorna uma lista com strings relativas ao diretorio passado
function DirectoryList(const DirMask : string; FList : TStringList) : TStringList; platform;


//Chama um callback para todos os arquivos abaixo do diretorio dado enquanto o retorno for 0
function EnumFiles(const Directory, Mask : string; FileParam : TEnumFileParam; FileProc : TEnumFileProc) : Integer;
    overload;
//Chama um callback para todos os arquivos abaixo do diretorio dado enquanto o retorno for 0
function EnumFiles(const Directory : string; MaskList : TList; FileParam : TEnumFileParam; FileProc : TEnumFileProc) : Integer;
    overload;
//Chama um callback para todos os arquivos abaixo do diretorio dado enquanto o retorno for 0
function EnumFiles(const Directory : string; MaskList : TList; IncludeAttrs, ExcludeAttrs : Integer;
    FileParam : TEnumFileParam; FileProc : TEnumFileProc) : Integer; overload;
//Chama um callback para todos os arquivos abaixo do diretorio dado enquanto o retorno for 0
function EnumFiles(const Directory, Mask : string; IncludeAttrs, ExcludeAttrs : Integer;
    FileParam : TEnumFileParam; FileProc : TEnumFileProc) : Integer; platform; overload;



//Chama um callback para todos os arquivos do diretorio dado enquanto o retorno for 0
function EnumFilesDir(const Directory, InitialMask : string; FileParam : TEnumFileParam; FileProc : TEnumFileProc) : Integer;
//Avalia um caminho de modo a substituir %ENV_VARS% pelos seus valores e caminhos relativos "." e ".."( inclusive UNC's )
function EvalPathName(const Path : string) : string;
//Extrai a unidade do caminho do arquivo
function ExtractFileDrive(const FileName : string) : string;
//Extrai a extensao sem o ponto final de um arquivo
function ExtractFileExtension(const FileName : string) : string; deprecated;
//extrai a parte relativa ao recurso de um unc
function ExtractUNCResource(const UNC : string) : string;
//Extrai a raiz do recurso especificada
function ExtractRootResource(const ResourceName : string) : string;
//Copia um arquivo para outro lugar //******************** TIPO DE RETORNO ALTERADO EM 08/01/1999 ****************************
function FileCopy(const Source, Dest : string; Over : boolean) : Integer; platform;
//Copia um arquivo para outro lugar criando os respectivos diretorios ate o destino} //******************** TIPO DE RETORNO ALTERADO EM 08/01/1999 ***************************
function FileCopyEx(const Source, Dest : string; Over : boolean) : Integer; platform;
//Copia arquivos de uma dada mascara para um dado diretorio
function FileCopyMaskDir(const MaskSourceDir, DestDir : string; Over, IncHiddens : boolean) : boolean; platform;
//Move arquivos de uma dada mascara para um dado diretorio
function FileMoveMaskDir(const MaskSourceDir, DestDir : string; Over, IncHiddens, IncSystem : boolean) : Integer; platform;
//Retorna string com todos os caractares invalidos do nome do arquivo, nao funciona para path
function FileNameInvalidChars(Strictness : Integer; const FileName : string) : string;
//Seta os atributos para os arquivos que obedecem a mascara passada
function FileMaskSetAttribs(const FileMask : string; Attr : Integer) : Integer; platform;
//Retorna o tamanho de um arquivo
function FileSize_(const FileName : string) : longint;
//Retorna o nome curto aceito para aquele arquivo incluindo o caminho
function FileShortName(LongFileName : string) : string;
//Conta quantas linha o arquivo possui
function FileTextCountLines(const FileName : string) : longint;
//Conta quantas vezes a substr aparece dentro do arquivo
function FileTextCountSubStr(const FileName, SubStr : string) : longint;
//Copia todos os arquivos da arvore fonte para a arvore destino
function FileXCopy(const Source, Dest : string; Over, IncHiddens : boolean) : boolean; platform;
//Retorna o primeiro diretorio filho do diretorio dado
function FindFirstChildDir(const DirName : string) : string;
//Retorna o primeiro arquivo filho de um subdiretorio
function FindFirstChildFile(const DirName : string) : string;
//Retorna lista de drives removiveis
function GetFloppiesList : string;
//Retorna o tamanho do arquivo
function GetFileSize(const FileName : string) : int64; platform; deprecated;
//Le em arquivo de usos exclusivo valor limitado ao periodo de espera
function GetKeyGenFileValue(const FileName : string; var Value, TimeOut : Integer) : Integer;
//Retorna o caminho do diretorio de arquivos temporarios
function GetTempDir : string;
//Verifica se string dada pode ser um UNC valido
function IsUNCName(const UNC : string) : boolean;
//Determina se o caminho dado e raiz de algum recurso
function IsRootDir(const Name : string) : boolean;
//Lista todos os arquivos de uma pasta
procedure ListDirFilesNames(Dir, Mask : string; Attr : Integer; IncSubDirs : boolean; List : TStrings); platform;
//Retorna nome do arquivo compativel com o formato 8.3
function LongToShortFileName(const LongName : string) : string; platform;
//Copia um arquivo para outro em uma rede peba
function NetFileCopy(const Source, Dest : string; Over : boolean) : boolean; platform;
//Cria arvore dada se esta nao existir ainda
function MkDirEx(Dir : string) : boolean;
//Retorna o diretorio pai do caminho dado
function ParentDir(const Dir : string) : string; deprecated;
//Retorna nome do Path no formato longo
function PathNameLong(const ShortPathName : string) : string;
//Remove a extensao do nome do arquivo
function RemoveFileExtension(const FileName : string) : string;
//RmDirEX : apaga um diretorio com seus subdiretoris junto
function RmDirEx(const DirName : string) : boolean; platform; deprecated;
//Ajusta as datas de criacao/acesso/escrita de um dado arquivo
function SetFileTimeProperties(const FileName : string; var CreateDate, LastAccessDate, LastWriteDate : TDateTime) : Integer;
    overload; platform;
//Ajusta as datas de criacao/acesso/escrita de um dado arquivo Notas : O arquivo deve ter a flag GENERIC_READ Exemplo :
function SetFileTimeProperties(FileHandle : THandle; var CreateDate, LastAccessDate, LastWriteDate : TDateTime) : Integer;
    overload;
//Salvar em arquivo de usos exclusivo valor limitado ao periodo de espera
function SetKeyGenFileValue(const FileName : string; Value, TimeOut : Integer) : Integer;
//Retorna o nome longo de um arquivo passado como nome curto
function ShortToLongFileName(const ShortName : string) : string;
//Remove barra do fim do caminho
function SlashRem(const Path : string; PreserveRoot : boolean = False) : string; deprecated;
//Formata corretamente o fullpath de um arquivo
function SlashSep(const Path, FileName : string) : string;
//Retorna o espaco em bytes livres para determinado caminho
function SpaceFree(const PathName : string) : int64;

type

    TFileHnd = class(TObject)
 {{
 Class for file related manipulation.
 }
    public
        class procedure BuildVersionInfo(const Filename : string; var V1, V2, V3, V4 : Word);
        class function ConcatPath(Paths : array of string) : string;
        class function CopyDir(const SourceDir, DestDir : TFilename) : Integer;
        class function CreateTempFileName(const Prefix : PChar; const Number : byte; CreateFile : boolean) : string;
        class function ExpandFilename(const BasePath, path : string) : string;
        class function FileSize(const FileName : string) : int64;
        class function ForceFileExtension(const OriginalName, DesiredExtension : string) : string;
        class procedure ForceFilename(const Filename : string);
        class function GetFileSizeEx(const Filename : string) : int64;
        class function MakeDefaultFileExtension(const OriginalFileName, DefaultExtension : string) : string;
        class function ParentDir(const filename : string) : string; overload;
        class function ParentDir(const filename : string; Existing : boolean) : string; overload;
        class function SlashRem(const Path : string; PreserveRoot : boolean = False) : string;
        class function SlashAdd(const Path : string) : string;
        class function VersionInfo(const Filename : string) : string;
        class function IsValidFilename(const Filename : string; ConstrainLevel : Integer) : boolean;
        class function RmDir(const Path : string) : Integer;
        class function NextFamilyFilename(const BaseFilename : string) : string;
        class function FileTimeToDateTime(FTime : TFileTime) : TDateTime;
        class function FileTimeProperties(FileHandle : THandle;
            var CreateDate, LastAccessDate, LastWriteDate : TDateTime) : Integer; overload;
        class function FileTimeProperties(const FileName : string;
            var CreateDate, LastAccessDate, LastWriteDate : TDateTime) : Integer; overload; platform;
    end;

implementation

uses
    Str_Pas, Super, Masks, DateOper, Contnrs, APIHnd, StrHnd;

function ConvSearchMaskArg(Mask : string) : string;
var
    P : Integer;
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

function __DeleteFile(const FileName : string) : boolean; deprecated;
{{
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

function __FileExists(const Name : string) : boolean;
{{
Verifica se arquivo existe, extendido para sub-diretorios
}
{$IFDEF MSWINDOWS}
var
    Entry : TSearchRec;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
    Result := (FindFirst(Name, faAnyFile - faDirectory, Entry) = 0);
    if Result then begin
        FindClose(Entry);
    end;
{$ELSE}
	raise Exception.Create('Método não implementado.');
{$ENDIF}
end;

function AddKeyGenFile(const FileName : string; TimeOut : Integer) : Integer;
{{
Abre arquivo com valor para auto-incremento para ler erro ver LasErrorCode
}
var
    Val : Integer;
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

function ConcatPath(Paths : array of string) : string;
{{
Junta as parte de um caminho
}
var
    RPart : string;
    i :     Integer;
begin
    if High(Paths) < 1 then begin
        Exit;
    end;
    Result := Paths[0];
    for i := 1 to High(Paths) do begin
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

function CountMaskFilesMatch(const PathMask : string) : Integer;
{{
Conta a quantidade de arquivos que obedecem uma dada mascara

Revision - 20100728 - Roger

Removido faVolumeID da mascara de atributos
}
var
    SRec :   TSearchRec;
    Status : Integer;
begin
    Result := 0;
    Status := FindFirst(PathMask, faDirectory, SRec);
    if Status = 0 then begin
        while Status = 0 do begin
            if (SRec.Attr and (faAnyFile - faDirectory ) <> 0) then begin
                Inc(Result);
            end;
            Status := FindNext(SRec);
        end;
        FindClose(SRec);
    end;
end;

function CreateTempFileName(const Prefix : PChar; const Number : byte) : string;
{{
Cria um nome de arquivo temporario
}
var
    Path, Name : PChar;
  {$IFNDEF WIN32}
    Drive :      char;
  {$ENDIF}
begin
    Result := '';
    Path   := StrAlloc(256);
    Name   := StrAlloc(512);
    {$IFDEF WIN32}
    if GetTempPath(255, Path) <> 0 then begin
        if GetTempFileName(Path, Prefix, Number, Name) <> 0 then begin
            Result := StrPas(Name);
        end;
    end;
    {$ELSE}
    GetTempDrive(Drive);
    if GetTempFileName(Drive, Prefix, Number, Name) <> 0 then begin
        Result := StrPas(Name);
    end;
    {$ENDIF}
    StrDispose(Path);
    StrDispose(Name);
end;

{{
Class for repass to Enumeration callBacks, instances of TEnumFileParam contains information about the current file.
}
{-**********************************************************************
************************************************************************
******************
******************  Class:    TEnumFileParam
******************  Category: No category
******************
************************************************************************
************************************************************************}
{--------------------------------------------------------------------------------------------------------------------------------}
procedure TEnumFileParam.SetErrorMsg(const Value : PChar);
{{
Sets the Error message for this Enumerator file parameter
}
begin
    if FErrorMsg <> nil then begin
        StrDispose(FErrorMsg);
    end;
    FErrorMsg := StrAlloc(Length(Value) + 1);
    StrCopy(FErrorMsg, Value);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TEnumFileParam.SetFileName(const Value : PChar);
{{
Sets the filename for this Enumerator file parameter
}
begin
    if FFileName <> nil then begin
        StrDispose(FFileName);
    end;
    FFileName := StrAlloc(Length(Value) + 1);
    StrCopy(FFileName, Value);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
constructor TEnumFileParam.Create;
{{
Enumerator file parameter constructor
}
begin
    inherited Create;
    Self.FFileName := nil;
    Self.ErrorCode := 0;
    Self.FErrorMsg := nil;
    Self.Data := nil;
    Self.SR := nil;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
destructor TEnumFileParam.Destroy;
{{
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


function DeleteMaskedFiles(const MaskFiles : string) : boolean;
{{
Apaga arquivos com uma dada mascara
}
var
    FileList :  TStringList;
    FinalMask : string;
begin
    FileList := TStringList.Create();
    try
        //FinalMask := ExtractFileName(MaskFiles);
        //Result := (DirFileList(FileList, ExtractFilePath(MaskFiles), [FinalMask]) = 0);
        //Alteração de arlindo
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

function Deltree(const Path : string; IncludeSelf : boolean = True; IgnoreErrors : boolean = True;
    IgnoreReadOnly : boolean = True; IgnoreSystem : boolean = True) : Integer;
{{
Apaga a pasta e seu conteudo recursivamente
}
    { TODO -oRoger -cLIB : 1Tentativa de filtrar os mais variados warnings abaixo }
{$WARN SYMBOL_PLATFORM OFF}
var
    ChildItem : string;
    OldAttribs, RemoveAttribs : DWORD;
    Lst : TStringList;
    i :   Integer;
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
                if (not IgnoreErrors) and (Result <> ERROR_SUCCESS) then begin  //Reporta o erro e sai
                    Exit;
                end;
            end else begin //Trata-se de arquivo
                ChildItem := Lst.Strings[i];
                if not DeleteFile(ChildItem) then begin
                    Result     := GetLastError(); //Reserva retorno se nada for ignorado
                    OldAttribs := GetFileAttributes(PChar(ChildItem)); //Reserva atributo corrente do arquivo
                    if OldAttribs = DWORD(-1) then begin //Falha em pegar atributos correntes
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
                    RemoveAttribs := OldAttribs and (not (RemoveAttribs));
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
        //Último passo a eliminação do diretorio caso nenhum error anterior
        if ( (Result = ERROR_SUCCESS ) and IncludeSelf ) then begin
           if ( not RemoveDir(Path) ) then begin
              Result:=GetLastError();
           end;
        end;
    finally
        Lst.Free;
    end;
{$WARN SYMBOL_PLATFORM ON}
end;

function DirFileList(List : TStrings; const Dir : string; Masks : array of string) : Integer;
{{
Preenche Lista com os arquivos do path passado, omite sub-dirs
}
var
    SRec : TSearchRec;
    i :    Integer;
    MaskList : TObjectList;
begin
    List.Clear;
    List.BeginUpdate;
    try
        Result := FindFirst(TFileHnd.ConcatPath([Dir, '*.*']), faAnyFile, SRec);
        try
            if (Result = 0) then begin
                MaskList := TObjectList.Create;
                try
                    for i := Low(Masks) to High(Masks) do begin
                        MaskList.Add(TMask.Create(Masks[i]));
                    end;
                    while (Result = 0) do begin
                        if SRec.Attr and faDirectory = 0 then begin  //Exclui sub-dirs
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

function DirectoryList(const DirMask : string; FList : TStringList) : TStringList;
{{
Retorna uma lista com os nomes dos arquivos ( Omite sub-pastas ) apenas em strings relativas ao diretorio passado

Revision - 20100728 - Roger
faVolumeID removido da mascara de atributos
}
var
    SRec :    TSearchRec;
    Status :  Integer;
    DirName : string;
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

function EnumFiles(const Directory : string; MaskList : TList; IncludeAttrs, ExcludeAttrs : Integer;
    FileParam : TEnumFileParam; FileProc : TEnumFileProc) : Integer; platform; overload;
{{
For each file existing at Directory that obeys the rules given by MaskList, IncludeAttrs, ExcludeAttrs the FileProc will be called. FileParam will be filled with the file information and repassed to FileProc.

Revision - 20100728 - Roger
faVolumeID removido da mascara de atributos
}
var
    SR : TSearchRec;
    i :  Integer;
begin
    Result := ERROR_SUCCESS;
    SetLastError(Result);
    try
        if FindFirst(Directory + PathDelim + '*.*', faAnyFile, SR) = 0 then begin //Busca qualquer arquivo
            try
                repeat
                    if ((SR.Attr and faDirectory) <> 0) then begin // Tratamento diferente para diretorios
                        if (SR.Name = '.') then begin //Chama callback para repassar o diretorio corrente
                            if (
                                ((IncludeAttrs and faDirectory) <> 0) and
                                ((ExcludeAttrs and faDirectory) = 0)) then begin //Chamada repassa diretorio corrente
                                FileParam.SR := @SR;
                                Result := FileProc(Directory, FileParam); //FileParam preenchido por FileProc!!!
                                if Result <> 0 then begin
                                    Break;
                                end;
                            end;
                        end else begin
                            if (SR.Name = '..') then begin
                                System.Continue;
                            end else begin  //Chamada recursiva para o sub-diretorio
                                Result := EnumFiles(Directory + PathDelim + SR.Name, MaskList, FileParam, FileProc);
                            end;
                        end;
                    end else begin //Trata-se de arquivo
                        if (((SR.Attr and IncludeAttrs) <> 0) and
                            ((SR.Attr and ExcludeAttrs) = 0)) then begin //Teste dos atributos
                            if (MaskList <> nil) then begin //Checa se mascara eh valida
                                for i := 0 to MaskList.Count - 1 do begin
                                    if (TMask(MaskList.Items[i]).Matches(SR.Name)) then begin
                                        FileParam.SR := @SR;
                                        Result := FileProc(Directory + PathDelim + SR.Name, FileParam);
                                        Break; //pelo menos 1 mascara entrou para este arquivo
                                    end;
                                end;
                            end else begin //Acceita qualquer arquivo
                                FileParam.SR := @SR;
                                Result := FileProc(Directory, FileParam);
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
end;


function EnumFiles(const Directory, Mask : string; IncludeAttrs, ExcludeAttrs : Integer; FileParam : TEnumFileParam;
    FileProc : TEnumFileProc) : Integer; platform; overload;
{{
For each file existing at Directory that obeys the rules given by Mask, IncludeAttrs and ExcludeAttrs the FileProc will be called. FileParam will be filled with the file information and repassed to FileProc.
}
var
    MaskList : TObjectList;
    index : Integer;
    s : string;
    MaskChecker : TMask;
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


function EnumFiles(const Directory : string; MaskList : TList; FileParam : TEnumFileParam; FileProc : TEnumFileProc) : Integer;
{{
Chama um callback para todos os arquivos abaixo do diretorio dado enquanto o retorno for 0
}
begin
     {$WARN SYMBOL_PLATFORM OFF}
    Result := EnumFiles(Directory, MaskList, faAllFiles { inclui todos }, 0 { Exclui nenhum }, FileParam, FileProc);
    {$WARN SYMBOL_PLATFORM ON}
end;

function EnumFiles(const Directory, Mask : string; FileParam : TEnumFileParam; FileProc : TEnumFileProc) : Integer;
{{
Chama um callback para todos os arquivos abaixo do diretorio dado enquanto o retorno for 0

(1) Correção de bug para o caso de única máscara passada ser *.*

Revision: 3/6/2008 - roger
}
var
    MaskChecker : TMask;
    MaskList : TObjectList;
    s :     string;
    index : Integer;
begin
    if (Mask = EmptyStr) then begin{1}
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


function EnumFilesDir(const Directory, InitialMask : string; FileParam : TEnumFileParam; FileProc : TEnumFileProc) : Integer;
{{
Chama um callback para todos os arquivos do diretorio dado enquanto o retorno for 0.
InitialMask apenas fornece uma máscara para o primeiro arquivo de interesse. Não devendo ser considerador como filtro para os
nomes de arquivos desejados.

Ver:
    TEnumFileParam e TEnumFileProc

Revision: 25/11/2005 - Roger
}
var
    ActualDir : string;
    SR : TSearchRec;
begin
    if not DirectoryExists(Directory) then begin
        Result := ERROR_PATH_NOT_FOUND;
        FileParam.FileName := TStrHnd.StrToPChar(Directory);
        FileParam.ErrorCode := Result;
        FileParam.ErrorMsg := TStrHnd.StrToPChar(SysErrorMessage(Result));
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
                FileParam.SR := @SR;
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

function EvalPathName(const Path : string) : string;
{{
Avalia um caminho de modo a substituir %ENV_VARS% pelos seus valores e caminhos relativos "." e ".."( inclusive UNC's )
}
const
    DELIM = '%';
var
    Macro, Value : string;
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
        Result := ExpandFileName(Result);
    end;
end;

function ExtractFileDrive(const FileName : string) : string;
{{
Extrai a unidade do caminho do arquivo
}
var
    Loc : Integer;
begin
    Result := FileName;
    Loc    := Pos(':', Result);
    if Loc = 0 then begin
        GetDir(0, Result);
        Loc := Pos(':', Result);
    end;
    Result := Copy(Result, 1, Loc);
end;

function ExtractFileExtension(const FileName : string) : string;
{{
Usar SysUtils.ExtractFileExt
Depreciada em 2005.04.29
Extrai a extensao sem o ponto final de um arquivo
}
var
{$IFNDEF WIN32}
    Tmp : array[0..255] of char;
{$ELSE}
	Tmp : PChar;
{$ENDIF}
begin
{$IFNDEF WIN32}
    Result := AllTrim(Copy(List.Strings[i], Pos('.', List.Strings[i])));
{$ELSE}
	Tmp := TStrHnd.StrToPChar(FileName);
	Result := AllTrim(Copy(StrPas(StrRScan(Tmp, '.')), 2, StrLen(Tmp)));
	StrDispose(Tmp);
{$ENDIF}
end;


function ExtractRootResource(const ResourceName : string) : string;
{{
Extrai a raiz do recurso especificado
}
begin
    if IsUNCName(ResourceName) then begin
        Result := ExtractUNCResource(ResourceName);
    end else begin
        Result := ExtractFileDrive(ResourceName);
    end;
end;


function ExtractUNCResource(const UNC : string) : string;
{{
Extrai a parte relativa ao recurso de um unc
}
var
    Part : string;
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

function FileCopy(const Source, Dest : string; Over : boolean) : Integer;
{{
Copia um arquivo para outro lugar
}
var
    Buffer : TMemoryStream;
    Attr :   Integer;
    Info :   TSearchRec;
    Age :    longint;
    Hnd :    Integer;
begin
    if (FindFirst(Source, faAnyFile, Info) = 0) then begin //source exists
        FindClose(Info); //Fecha handle de pesquisa desnecessario
        if DirectoryExists(ExtractFilePath(Dest)) then begin //dest exists
            if (Over or (not (__FileExists(Dest)))) then begin
                if (TFileHnd.FileSize(Source) > SpaceFree(ExtractFilePath(Dest))) then begin //falta espaco
                    Result := ERROR_DISK_FULL;
                    Exit;
                end;
                if __FileExists(Dest) and (FileSetAttr(Dest, faArchive) <> 0) then begin
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

function FileCopyEx(const Source, Dest : string; Over : boolean) : Integer;
{{
Copia um arquivo para outro lugar criando os respectivos diretorios
//******************** TIPO DE RETORNO ALTERADO EM 08/01/1999 ****************************
}
var
    SubDir : string;
begin
    SubDir := TFileHnd.ParentDir(Dest);
    if not (DirectoryExists(SubDir)) then begin
  {$I-}
        MkDirEx(SubDir);
  {$I+}
        if IoResult <> 0 then begin
            Result := GetLastError;
            Exit;
        end;
    end;
    Result := FileCopy(Source, Dest, Over);
end;

function FileMoveMaskDir(const MaskSourceDir, DestDir : string; Over, IncHiddens, IncSystem : boolean) : Integer; platform;
{{
Move arquivos de uma dada mascara para um dado diretorio
}
var
    SR :    TSearchRec;
    Path :  string;
    Found : boolean;
    Attrib, Flags : Integer;
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
    if not IncHiddens then begin     //Isola Hidden
        Attrib := Attrib - faHidden;
    end;
    if not IncSystem then begin        //Isola SystemFiles
        Attrib := Attrib - faSysFile;
    end;
    Result := ERROR_SUCCESS;
    repeat
        Found := (FindFirst(MaskSourceDir, Attrib, SR) = 0);
        try
            if Found then begin
                if not MoveFileEx(PChar(TFileHnd.ConcatPath([Path, SR.Name])), PChar(TFileHnd.ConcatPath([DestDir, SR.Name])),
                    Flags) then begin
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

function FileNameInvalidChars(Strictness : Integer; const FileName : string) : string;
{{
Retorna string com todos os caractares invalidos do nome do arquivo, nao funciona para path
}
begin
    case Strictness of
        0 : begin //Complacente
            Result := Str_Pas.UsedCharSet(INVALID_FILE_NAME_CHARS1_STR, FileName);
        end;
        1 : begin //Mediano
            Result := Str_Pas.UsedCharSet(INVALID_FILE_NAME_CHARS2_STR, FileName);
        end;
        else begin //Rigoroso
            Result := Str_Pas.UsedCharSet(INVALID_FILE_NAME_CHARS3_STR, FileName);
        end;
    end;
end;

function FileCopyMaskDir(const MaskSourceDir, DestDir : string; Over, IncHiddens : boolean) : boolean;
{{
Copia arquivos de uma dada mascara para um dado diretorio

Revision - 20100728 - Roger
faVolumeID removido da mascara de atributos

}
var
    Rec :     TSearchRec;
    Path :    string;
    Attrib :  Word;
    EndList : boolean;
begin
    Path := ExtractFilePath(MaskSourceDir);
    if IncHiddens then begin
        Attrib := faAnyFile - faDirectory;
    end else begin
        Attrib := faAnyFile - faHidden - faDirectory;
    end;
    Result := False;
    if FindFirst(MaskSourceDir, Attrib, Rec) <> 0 then begin {Nenhum arquivo}
        Result := True; { A inexistencia da origem nao implica em falha}
        Exit;
    end;
    EndList := False;
    repeat
        begin
            if (Rec.Attr or faDirectory) = Rec.Attr then begin
                System.Continue; {Pula diretorios}
            end;
            if ((Rec.Attr or faHidden) = Rec.Attr) and not (IncHiddens) then begin
                System.Continue; {Pula ocul}
            end;
            if __FileExists(DestDir + PathDelim + Rec.Name) and (not Over) then begin
                FindClose(Rec);
                Exit;
            end;
            if FileCopy(Path + Rec.Name, DestDir + PathDelim + Rec.Name, True) <> 0 then begin
                {falha copia}
                Break;
            end;
            EndList := (FindNext(Rec) <> 0); {Toma proximo}
        end
    until EndList;
    Result := True;
    FindClose(Rec);
end;


function FileMaskSetAttribs(const FileMask : string; Attr : Integer) : Integer;
{{
Seta os atributos para os arquivos que obedecem a mascara passada

Revision - 20100728 - Roger
faVolumeID removido da mascara de atributos
}
var
    Rec :     TSearchRec;
    Path :    string;
    EndList : boolean;
begin
    Path := ExtractFilePath(FileMask);
    if FindFirst(FileMask, faAnyFile - faDirectory, Rec) <> 0 then begin {Nenhum arquivo}
        Result := ERROR_FILE_NOT_FOUND; { A inexistencia da origem implica em falha}
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
            EndList := (FindNext(Rec) <> 0); {Toma proximo}
        end
    until EndList;
    FindClose(Rec);
end;

function FileShortName(LongFileName : string) : string; platform;
{{
Retorna o nome curto aceito para aquele arquivo incluindo o caminho
}
var
    LastSlash :   PChar;
    TempPathPtr : array[0..MAX_PATH] of char;
begin
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
end;


function FileTextCountLines(const FileName : string) : longint;
{{
Conta quantas linha o arquivo possui
}
begin
    Result := FileTextCountSubStr(FileName, EOL_DOS);
end;

function FileTextCountSubStr(const FileName, SubStr : string) : Integer;
{{
Conta quantas vezes a substr aparece dentro do arquivo
}
var
    FS :     TFileStream;
    Buf :    array[0..IDEAL_FILE_BLOCK_SIZE] of byte;
    PS :     PChar;
    Acc, Step : longint;
    OffSet : Integer;
begin
  {$TYPEDADDRESS OFF}
    //NOTAS : O tamanho de SubStr nao pode exceder IDEAL_FILE_BLOCK_SIZE !!!!
    SetLastError(0);
    try
        FS := TFileStream.Create(FileName, fmOpenRead);
        try
            OffSet := Length(SubStr);
            Acc    := 0;
            Result := 0;
            while FS.Size > Acc do begin
                Step := FS.Read(buf, IDEAL_FILE_BLOCK_SIZE - 1);
                Buf[Step + 1] := byte(#0); //Marca fim de pesquisa para evitar ir alem do fim do arquivo
                Inc(Acc, Step);
                PS := @Buf[0];//Inicio da pesquisa
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
  {$TYPEDADDRESS ON}
end;

function FileXCopy(const Source, Dest : string; Over, IncHiddens : boolean) : boolean;
{{
Copia todos os arquivos da arvore fonte para a arvore destino
}
var
    ActualDir : string;
    SR : TSearchRec;
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

function FileSize_(const FileName : string) : longint;
{{
Retorna o tamanho de um arquivo
}
var
    SR : TSearchRec;
begin
    if FindFirst(FileName, faAnyFile, SR) = 0 then begin
        Result := SR.Size;
        FindClose(SR);
    end else begin
        Result := -1;
    end;
end;

function FindFirstChildDir(const DirName : string) : string;
{{
Retorna o primeiro diretorio filho do diretorio dado
}
var
    SR : TSearchRec;
begin
     {$WARN SYMBOL_PLATFORM OFF}
    Result := EmptyStr;
    //Localiza qq coisa inicialmente
    if FindFirst(DirName + PathDelim + '*.*', faDirectory + faHidden + faReadOnly + faSysFile, SR) = 0 then
    begin  //encontrado alvo
        try
            repeat
                if (SR.Name = '.') or (SR.Name = '..') then begin   //Filtra dir corrente e pai
                    System.Continue;
                end;
                if ((SR.Attr or faDirectory) = SR.Attr) then begin  //trata-se de um sub-diretorio
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

function FindFirstChildFile(const DirName : string) : string;
{{
Returns the first child file at specified directory
}
var
    SR : TSearchRec;
begin
    if FindFirst(DirName + PathDelim + '*.*', faAnyFile - faDirectory, SR) = 0 then begin
        Result := DirName + PathDelim + SR.Name;
        FindClose(SR);
    end else begin
        Result := EmptyStr;
    end;
end;

function NetFileCopy(const Source, Dest : string; Over : boolean) : boolean;
{{
Copia um arquivo para outro em uma rede peba
}
var
    Buffer : TMemoryStream;
    Attr :   Integer;
    Info :   TSearchRec;
    Age :    longint;
    Hnd :    Integer;
begin
    Result := False;
    if (FindFirst(Source, faAnyFile, Info) = 0) then begin
        FindClose(Info);
        if DirectoryExists(ExtractFilePath(Dest)) then begin
            if (Over or (not (FileExists(Dest)))) then begin
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

function GetFileSize(const FileName : string) : int64;
{{
Retorna o tamanho do arquivo

Depreciada, use TFileHnd.FileSize();
}
var
    { TODO -oRoger -cLIB : 2Tentativa de filtrar os mais variados warnings abaixo }
  {$WARN SYMBOL_PLATFORM OFF}
    SR : TSearchRec;
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
    raise Exception.Create('Sem solução disponível para Linux.');
{$ENDIF}
end;

function GetFloppiesList : string;
{{
Retorna lista de drives removiveis
}
var
    i :   byte;
    Reg : Word;
begin
    asm
        INT $11
        MOV Reg,AX
    end;
    {intr($11,regs);}
    Result := '';
    for i := 1 to succ((Reg and 192) shr 6) do begin
        Result := Result + chr(64 + i);
    end;
end;


function GetKeyGenFileValue(const FileName : string; var Value, TimeOut : Integer) : Integer;
{{
Le em arquivo de usos exclusivo valor limitado ao periodo de espera
}
var
    FHnd :   TextFile;
    FTime :  TDateTime;
    Sucess : boolean;
    Line :   string;
begin
    FTime := IncTime(Now, 0, 0, 0, TimeOut);
    AssignFile(FHnd, FileName);
    Value  := 0;
    Sucess := False;
    Result := -1;
    try
  {$I-}
        repeat
            Reset(FHnd);
            if IOResult = 0 then begin
                Sucess := True;
            end
        until (Sucess) or (Now > FTime);
  {$I+}
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

function GetTempDir : string;
{{
Retorna o caminho do diretorio de arquivos temporarios
}
var
    Path : array[0..MAX_PATH] of char;
begin
    SetLastError(ERROR_SUCCESS);
    GetTempPath(MAX_PATH, Path);
    if GetLastError() = ERROR_SUCCESS then begin
        Result := Path;
    end else begin
        Result := EmptyStr;
    end;
end;

function IsUNCName(const UNC : string) : boolean;
{{
Tests if the argument is a UNC compliant.
}
begin
    Result := (Pos('\\', UNC) = 1);
end;


procedure ListDirFilesNames(Dir, Mask : string; Attr : Integer; IncSubDirs : boolean; List : TStrings);
{{
Lista todos os arquivos de uma pasta que obedeçam a máscara dada e aos atributos especificados.

Revision: 25/11/2005 - Roger

Corrigido bug que falha na enumeração das mascaras que continham "?', pois comparava o caminho completo do arquivo com a máscara.
em LocalListFiles()
    PParam^.Mask.Matches(Filename) -> PParam^.Mask.Matches(ExtractFileName(Filename))

Revision: 2/5/2006 - Roger
}
type
    PLocalDataParam = ^TLocalDataParam;

    TLocalDataParam = record
        FList: TStrings;
        MatchAttribut: Integer;
        Mask:  TMask;
    end;
var
    Param :     TEnumFileParam;
    DataParam : TLocalDataParam;
    //.........................................................................................................
    function LocalListFiles(const FileName : string; FileParam : TEnumFileParam) : Integer;
    var
        TargetAttr : Integer;
        PParam :     PLocalDataParam;
    begin
        try
            PParam     := FileParam.Data;
            TargetAttr := PParam^.MatchAttribut;
            if (((TargetAttr = faAnyFile) or ((TargetAttr and FileParam.SR^.Attr) = TargetAttr))
                and
                PParam^.Mask.Matches(ExtractFileName(Filename))
                ) then begin
                PParam^.FList.Add(FileName);
            end;
        except
            Result := GetLastError;
            Exit;
        end;
        Result := 0;
    end;
    //.........................................................................................................
begin
  {$TYPEDADDRESS OFF}
    Dir := TFileHnd.SlashRem(Dir);
    if GetIChar(Dir, Length(Dir)) = ':' then begin //Checa por raiz de unidade
        Dir := Dir + PathDelim;
    end;
    Param := TEnumFileParam.Create;
    List.Clear;
    DataParam.FList := List;
    DataParam.MatchAttribut := Attr;
    DataParam.Mask  := TMask.Create(Mask);
    Param.Data      := @DataParam;
    try
        if IncSubDirs then begin
            EnumFiles(Dir, Mask, Param, @LocalListFiles);
        end else begin
            EnumFilesDir(Dir, Mask, Param, @LocalListFiles);
        end;
    finally
        DataParam.Mask.Free; //Libera mascara vinculada acima
        Param.Free;
    end;
  {$TYPEDADDRESS ON}
end;

function LongToShortFileName(const LongName : string) : string;
{{
Retorna nome do arquivo compativel com o formato 8.3 estilo DOS
}
var
    SR : TSearchRec;
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


function IsRootDir(const Name : string) : boolean;
{{
Tests if the argument is a root of any disk volume
}
var
    Rep : Integer;
begin
    Result := False;
    if IsUNCName(Name) then begin
        Rep := StrCountCharRepet(PathDelim, Name);
        if Rep <= 4 then begin
            if Rep = 4 then begin
                Result := (Name[Length(Name)] = PathDelim); //o ultimo nao vale
            end else begin
                Result := True;
            end;
        end;
    end else begin
        Rep := Length(Name);
        case (Rep) of
            2 : begin
                Result := CharInSet(UpCase(Name[1]) , ['A'..'Z']) and (Name[2] = ':'); //Exemplo (L:\) or (C:)
            end;
            3 : begin
                Result := CharInSet(UpCase(Name[1]) , ['A'..'Z']) and (Name[2] = ':') and (Name[3] = PathDelim); //Exemplo (L:\) or (C:)
            end;
            else begin
                Result := False;
            end;
        end;
    end;
end;

function MkDirEx(Dir : string) : boolean;
{{
Cria arvore dada se esta nao existir ainda
}
var
    _Sub : boolean;
begin
    Result := False;
    if DirectoryExists(TFileHnd.ParentDir(Dir)) then begin
        MkDir(Dir);
        Result := (IoResult = 0);
    end else begin
        {Temos bronca quando o nível é no raiz devido ao dir pai retornado}
        _Sub := MkDirEx(TFileHnd.ParentDir(Dir));
        if _Sub then begin
            Result := MkDirEx(Dir);
        end;
    end;
end;

function ParentDir(const Dir : string) : string;
{{
Retorna o diretorio pai do caminho dado

Deprecated at 19/5/2005 use TFileHnd.ParentDir
}
var
    Res : string;
begin
    if IsUNCName(Dir) then begin {UNC}
        Res    := ExtractUNCResource(Dir);
        Result := ReplaceSubString(Dir, Res, EmptyStr);
        Result := ParentDir(Result);
        if (Result = PathDelim) or (Result = EmptyStr) then begin
            Result := Res;
        end;
    end else begin {driver logico}
        {Precisa da funcao ExtractFilePath do SysUtils do Delphi}
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

function PathNameLong(const ShortPathName : string) : string;
{{
Converte nome do caminho para o formato longo
}
var
    LastSlash, TempPathPtr : PChar;
    PathBuffer : array[0..MAX_PATH] of char;
begin
    Result := EmptyStr;
    StrPCopy(PathBuffer, ShortPathName); //Salva copia em buffer para alocacao dos marcadores #0´s
    TempPathPtr := PathBuffer;
    LastSlash   := StrRScan(TempPathPtr, PathDelim);
    while LastSlash <> nil do begin
        Result := PathDelim + ShortToLongFileName(TempPathPtr) + Result;
        if LastSlash <> nil then begin
            LastSlash^ := char(0);
            LastSlash  := StrRScan(TempPathPtr, PathDelim);
        end;
    end;
    Result := TempPathPtr + Result;
end;

function RemoveFileExtension(const FileName : string) : string;
{{
Remove a extensao do nome do arquivo
}
var
    Pi, Pf : PChar;
begin
    Pi := PChar(FileName);
    Pf := StrRScan(Pi, '.');
    if Pf <> nil then begin
        Pf^    := #0;
        Result := string(Pi);
        Pf^    := '.';
    end else begin
        Result := FileName;
    end;
end;

function RmDirEx(const DirName : string) : boolean;
{{
Rotina depreciada - Usar TFileHnd.RmDir()

Apaga um diretorio com seus subdiretoris junto
}
var
    SR : TSearchRec;
    NextProcDir : string;
begin
    Result := False;
    if not (DirectoryExists(DirName)) then begin
        Result := True;
        Exit;
    end;
    GetDir(0, NextProcDir);
    if Pos(UpperCase(DirName), UpperCase(NextProcDir)) <> 0 then begin
        {Nao pode apagar caminho em uso = Diretorio atual}
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
    if TFileHnd.ParentDir(DirName) <> DirName then begin {Eliminando raiz de recurso}
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

function SetFileTimeProperties(const FileName : string; var CreateDate, LastAccessDate, LastWriteDate : TDateTime) : Integer;
    overload; platform;
{{
Ajusta as datas de criacao/acesso/escrita de um dado arquivo
}
var
    Hnd : THandle;
begin
    Hnd := CreateFile(PChar(Filename), GENERIC_WRITE, 0, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL or
        FILE_FLAG_SEQUENTIAL_SCAN, 0);
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

function SetFileTimeProperties(FileHandle : THandle; var CreateDate, LastAccessDate, LastWriteDate : TDateTime) : Integer;
    overload;
{{
Ajusta as datas de criacao/acesso/escrita de um dado arquivo

Notas : O arquivo deve ter acesso GENERIC_WRITE


Exemplo:
hf := CreateFile ( PChar(FFilename), GENERIC_WRITE, 0, nil, OPEN_EXISTING, 0, //FILE_ATTRIBUTE_NORMAL or FILE_FLAG_SEQUENTIAL_SCAN , 0);
}
var
    DCreate, DAccess, DWrite : TFileTime;
    ST : TSystemTime;
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
    if not SetFileTime(FileHandle, @DCreate, @DAccess, @DWrite) then begin
        Result := GetLastError();
    end;
end;

function SetKeyGenFileValue(const FileName : string; Value, TimeOut : Integer) : Integer;
{{
Salva em arquivo de usos exclusivo valor limitado ao periodo de espera
}
var
    FHnd :   TextFile;
    FTime :  TDateTime;
    Sucess : boolean;
begin
    FTime := IncTime(Now, 0, 0, 0, TimeOut);
    AssignFile(FHnd, FileName);
    Sucess := False;
    Result := ERROR_TIMEOUT;
    try
  {$I-}
        repeat
            ReWrite(FHnd);
            if IOResult = 0 then begin
                Sucess := True;
            end
        until (Sucess) or (Now > FTime);
  {$I+}
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

function ShortToLongFileName(const ShortName : string) : string;
{{
Retorna o nome longo de um arquivo passado como nome curto
}
var
    Temp : TWin32FindData;
    SearchHandle : THandle;
begin
    SearchHandle := FindFirstFile(PChar(ShortName), Temp);
    if SearchHandle <> INVALID_HANDLE_VALUE then begin
        Result := string(Temp.cFileName);
    end else begin
        Result := EmptyStr;
    end;
    Windows.FindClose(SearchHandle);
end;

function SlashRem(const Path : string; PreserveRoot : boolean = False) : string; deprecated;
{{
Remove barra do fim do caminho

Deprecated : Use TFileHnd.SlashRem()
}
begin
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
end;

function SlashSep(const Path, FileName : string) : string;
{{
Returns a path tha forces PathDelim at your final
}
begin
    if AnsiLastChar(Path)^ <> PathDelim then begin
        Result := Path + PathDelim + FileName;
    end else begin
        Result := Path + FileName;
    end;
end;


function SpaceFree(const PathName : string) : int64;
{{
Retorna o espaco em bytes livres para determinado caminho
}
var
    Drive : byte;
    OS :    OSVERSIONINFO;
    TotalFree, TotalExisting : int64;
    SectorsPerCluster, BytesPerSector, FreeClusters, TotalClusters : cardinal;
begin
    if Pos(':', PathName) <> 0 then begin //Unidade de disco
        Drive  := Ord((ExtractFileDrive(UpperCase(PathName))[1]));
        Drive  := Drive - Ord('A') + 1;
  {$IFDEF WIN32}
		Result := DiskFree(Drive);
		{$ELSE}
        Result := _DiskFree(Drive);
  {$ENDIF}
    end else begin //Recurso via UNC
        OS.dwOSVersionInfoSize := SizeOf(OSVERSIONINFO);
        GetVersionEx(OS);
        if (OS.dwMajorVersion = 4) and (OS.dwBuildNumber < 1000) and (OS.dwPlatformId = 1) then begin //Usar modo OSR1
            if GetDiskFreeSpace(PChar(PathName), SectorsPerCluster, BytesPerSector, FreeClusters, TotalClusters) then begin
                Result := (SectorsPerCluster * BytesPerSector * FreeClusters);
            end else begin
                Result := -1;
            end;
        end else begin //Usar os servicos do NT ou Win95B ou superior
            if not GetDiskFreeSpaceEx(PChar(PathName), Result, TotalExisting, @TotalFree) then begin
                Result := -1;
            end;
        end;
    end;
end;

{-**********************************************************************
************************************************************************
******************
******************  Class:    TFileHnd
******************  Category: No category
******************
************************************************************************
************************************************************************}
{--------------------------------------------------------------------------------------------------------------------------------}
class procedure TFileHnd.BuildVersionInfo(const Filename : string; var V1, V2, V3, V4 : Word);
{{
Repassa as componentes da versão do arquivo dado.

Filename - Nome de arquivo de interesse. Caso string vazia seja passa o caminho de ParamStr(0) sera usado.

V1 - 1 componente da versão

V2 - 2 componente da versão

V3 - 3 componente da versão

V4 - 4 componente da versão

Revision: 30/10/2006 - Roger

Para o caso do arquivo passado não possuir informação de versão todos os elementos serão retornados como 0 (zero).

}
var
    VerInfoSize, VerValueSize, Dummy : DWord;
    VerInfo :  Pointer;
    VerValue : PVSFixedFileInfo;
    Path :     string;
begin
    try
        if (Filename = EmptyStr) then begin
            path := ParamStr(0);
        end else begin
            path := Filename;
        end;
        VerInfoSize := GetFileVersionInfoSize(PChar(Path), Dummy);
        if (VerInfoSize > 0) then begin
            GetMem(VerInfo, VerInfoSize);
            try
                GetFileVersionInfo(PChar(Path), 0, VerInfoSize, VerInfo);
                VerQueryValue(VerInfo, '\' {dont localize}, Pointer(VerValue), VerValueSize);
                V1 := VerValue^.dwFileVersionMS shr 16;
                V2 := VerValue^.dwFileVersionMS and $FFFF;
                V3 := VerValue^.dwFileVersionLS shr 16;
                V4 := VerValue^.dwFileVersionLS and $FFFF;
            finally
                FreeMem(VerInfo, VerInfoSize);
            end;
        end else begin  //informação de versão não encontrada neste arquivo
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

{--------------------------------------------------------------------------------------------------------------------------------}
class function TFileHnd.ConcatPath(Paths : array of string) : string;
{{
Junta as parte da esquerda para a direita de um caminho. Esta rotina é útil para evitar as duplas \\ na composicao do nome do
arquivo.

Paths : array com os elementos que compoem o nome do arquivo.
}
var
    RPart : string;
    i :     Integer;
begin
    if High(Paths) < 1 then begin
        Exit;
    end;
    Result := Paths[0];
    for i := 1 to High(Paths) do begin
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

{--------------------------------------------------------------------------------------------------------------------------------}
class function TFileHnd.CopyDir(const SourceDir, DestDir : TFilename) : Integer;
{{
Copia um diretório recursivamente para outro.

Revision: 5/6/2008 - roger
}
var
    SearchRec : TSearchRec;
    DosError :  Integer;
    Path, DestPath : TFileName;
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
                    end else
                    if (not CopyFile(PChar(Path + SearchRec.Name), PChar(DestPath + SearchRec.Name), True)) then begin
                        Result := GetLastError();
                        Exit;
                    end;
                end;
                DosError := FindNext(SearchRec);
            end;
            FindClose(SearchRec);
        except
            FindClose(SearchRec); //usado apenas para não omitir chamada
            raise;
        end;
    end;
end;

class function TFileHnd.CreateTempFileName(const Prefix : PChar; const Number : byte; CreateFile : boolean) : string;
{{
Gera nome de arquivo temporario pela Win32 API.

Prefix :
Parte inicial do nome do arquivo.

Number :
Indice preferencial ver Win32 API GetTempFileName().

CreateFile :
Indica se este arquivo sera automaticamente criado vazio.

Returns:

Nome do arquivo temporario para uso.
}
var
    Path, Name : PChar;
    f : file;
begin
    Result := '';
    Path   := StrAlloc(256);
    Name   := StrAlloc(512);
    if GetTempPath(255, Path) <> 0 then begin
        if GetTempFileName(Path, Prefix, Number, Name) <> 0 then begin
            Result := StrPas(Name);
        end;
    end;
    StrDispose(Path);
    StrDispose(Name);
    if (CreateFile) then begin    //Se eh para criar
        AssignFile(f, Result);
        Rewrite(f); //Arquivo zerado
        CloseFile(f);
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
class function TFileHnd.ExpandFilename(const BasePath, path : string) : string;
{{
BasePath : Caminho base para o calculo do caminho expandido.

path  : Caminho a ser expandido.

return : Caminho expandido do path passado.
}

const
    PARENT_DIR_TOKEN = '..' + SysUtils.PathDelim;

    procedure LSRRemapPaths(var localBase, localPath : string);
    begin
        while (TStrHnd.startsWith(localPath, PARENT_DIR_TOKEN)) do begin
            Delete(localPath, 1, Length(PARENT_DIR_TOKEN));
            localBase := TFileHnd.SlashRem(ExtractFilePath(SlashRem(localBase)));
        end;
    end;

var
    c : char;
    rightPart, leftPart : string;
    posPDir : Integer;

begin
    if (path <> EmptyStr) then begin
        c := GetIChar(path, 1); //Pega 1o caracter para testar se caminho base serah diretamente agregado
        if ((c = SysUtils.PathDelim) or (c = '.')) then begin    //Agrega ao final de BasePath diretamente
            Result := TFileHnd.ConcatPath([BasePath, path]);
        end else begin    //Verifica se path inicia-se com ".."
            rightPart := path;
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

{--------------------------------------------------------------------------------------------------------------------------------}
class function TFileHnd.FileSize(const FileName : string) : int64;
{{
Returns the size of file, or -1 if fail.
Call GetLastError() to see extended error information.

WARNING : Its a platform method.

Para o caso de se calcular o tamanho usado por um diretório usar GetFileSizeEx()

Revision: 3/6/2008 - roger
}
var
    SR : TSearchRec;
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

{--------------------------------------------------------------------------------------------------------------------------------}
class function TFileHnd.FileTimeProperties(FileHandle : THandle;
    var CreateDate, LastAccessDate, LastWriteDate : TDateTime) : Integer;
{{
Calcula as datas de criacao/acesso/escrita de um dado arquivo Notas : O arquivo deve ter a flag GENERIC_READ
Exemplo :
hf := CreateFile (PChar(FFilename), GENERIC_READ , 0, nil, OPEN_EXISTING, 0, //FILE_ATTRIBUTE_NORMAL or FILE_FLAG_SEQUENTIAL_SCAN 0);
}
var
    DCreate, DAccess, DWrite : TFileTime;
begin
    if GetFileTime(FileHandle, @DCreate, @DAccess, @DWrite) then begin
        Result := 0;
        FileTimeToLocalFileTime(DCreate, DCreate);      //Criacao
        CreateDate := TFileHnd.FileTimeToDateTime(DCreate);
        FileTimeToLocalFileTime(DAccess, DAccess);      //Acesso
        LastAccessDate := TFileHnd.FileTimeToDateTime(DAccess);
        FileTimeToLocalFileTime(DWrite, DWrite);        //Escrita
        LastWriteDate := TFileHnd.FileTimeToDateTime(DWrite);
    end else begin
        Result := GetLastError();
    end;
end;

class function TFileHnd.FileTimeProperties(const FileName : string;
    var CreateDate, LastAccessDate, LastWriteDate : TDateTime) : Integer;
{{
Calcula as datas de criacao/acesso/escrita de um dado arquivo
}
var
    SR : TSearchRec;
    DCreate, DAccess, DWrite : TFileTime;
begin
    Result := FindFirst(FileName, faAnyFile, SR);
    if Result = 0 then begin
        //Criacao
        DCreate := SR.FindData.ftCreationTime;
        FileTimeToLocalFileTime(DCreate, DCreate);
        CreateDate := TFileHnd.FileTimeToDateTime(DCreate);
        //Acesso
        DAccess    := SR.FindData.ftLastAccessTime;
        FileTimeToLocalFileTime(DAccess, DAccess);
        LastAccessDate := TFileHnd.FileTimeToDateTime(DAccess);
        //escrita
        DWrite := SR.FindData.ftLastWriteTime;
        FileTimeToLocalFileTime(DWrite, DWrite);
        LastWriteDate := TFileHnd.FileTimeToDateTime(DWrite);
        FindClose(SR);
    end;
end;

class function TFileHnd.FileTimeToDateTime(FTime : TFileTime) : TDateTime;
{{
Converte data/hora do windows para tipo TDateTime

Revision: 10/8/2006 - Roger
}
var
    ST : TSystemTime;
begin
    FileTimeToSystemTime(FTime, ST);
    Result := EncodeDate(ST.wYear, ST.wMonth, ST.wDay) + EncodeTime(ST.wHour, ST.wMinute, ST.wSecond, ST.wMilliSeconds);
end;

class function TFileHnd.ForceFileExtension(const OriginalName, DesiredExtension : string) : string;
{{
OriginalName : Nome original do arquivo.

DesiredExtension : Extensao desejada

Ver também:
TFileHnd.MakeDefaultFileExtension
}
var
    originalExt, ForceExt : string;
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
        if SameText(originalExt, ForceExt) then begin    //Adiciona "." a comparacao
            Result := OriginalName;
        end else begin
            Result := OriginalName + ForceExt;
        end;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
class procedure TFileHnd.ForceFilename(const filename : string);
{{
Garante a existencia de um arquivo com o nome especificado.
Para o caso do arquivo não exisitir ele se´ra criado vazio, bem como a árvore de diretorios necessarios.

filename : nome do arquivo desejado.
}
var
    st : TFileStream;
begin
    if (not FileExists(filename)) then begin
        if (not DirectoryExists(ParentDir(filename))) then begin
            if (not ForceDirectories(ParentDir(filename))) then begin
                TAPIHnd.CheckAPI(GetLastError());
            end;
        end;
        st := TFileStream.Create(Filename, fmCreate);
        st.Free;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
class function TFileHnd.GetFileSizeEx(const Filename : string) : int64;
{{
Calcula o tamanho do arquivo, caso o arquivo seja um diretório o cálculo será recursivo

Revision: 3/6/2008 - roger
}
var
    SR :   TSearchRec;
    List : TStringList;
    x :    Integer;
begin
 {$WARN SYMBOL_PLATFORM OFF}
    if FindFirst(FileName, faAnyFile, SR) = 0 then begin
        try
            if ((SR.Attr and faDirectory) = faDirectory) then begin   //Necessita varrer os subdirs
                List := TStringList.Create;
                try
                    FileHnd.ListDirFilesNames(Filename, '*.*', faAnyFile, True, List);
                    Result := 0;
                    for x := 0 to List.Count - 1 do begin
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

class function TFileHnd.IsValidFilename(const Filename : string; ConstrainLevel : Integer) : boolean;
{{
Valida se o conjunto de caracteres que compoe o nome do arquivo pode ser gerado, isto é não viola o conjunto de caracteres invalidos
representados por ContrainLevel.

ConstrainLevel = 1^, indica baixa critica, aceita espaco, acentos e ascii extendido
ConstrainLevel = 2 , indica media critica, aceita espaco e o ascii baixo
ConstrainLevel <> dos valores acima sera usado alta critica, invalidando todos os acima adcionados aos invalidos no linux

Revision: 27/7/2005 - Roger  
}
var
    CSet :  TSysCharSet;
    LPart : string;
begin
    if (FileName = EmptyStr) then begin
        Result := False;
        Exit;
    end;
    case (ConstrainLevel) of
        1 : begin
            CSet := INVALID_FILENAME_CHARS_LOW_CRITIC;
        end;
        2 : begin
            CSet := INVALID_FILENAME_CHARS_MEDIUM_CRITIC;
        end;
        else begin
            CSet := INVALID_FILENAME_CHARS_HIGH_CRITIC;
        end;
    end;
    LPart := ExtractFilePath(Filename);
    if (LPart = Filename) then begin  //raiz ou nome final da sequencia
        Result := (Length(LPart) = 3) and CharInSet(GetIChar(LPart, 2) , ['a'..'Z']) and (GetIChar(LPart, 3) = PathSep); //algo do tipo c:\
        Result := Result or (TStrHnd.StrPosChar(ExtractFileName(Filename), CSet) = 0);
    end else begin
        if (LPart = EmptyStr) then begin
            Result := (TStrHnd.StrPosChar(ExtractFileName(Filename), CSet) = 0);
        end else begin
            Result := TFileHnd.IsValidFilename(LPart, ConstrainLevel);
            Result := Result and (TStrHnd.StrPosChar(ExtractFileName(Filename), CSet) = 0);
        end;
    end;
end;

class function TFileHnd.MakeDefaultFileExtension(const OriginalFileName, DefaultExtension : string) : string;
{{
Retorna nome de arquivo com a extensao default se esta não tiver sido especificada no proprio nome do arquivo
Nenhuma critica é feita com o nome resultante.

Ver também:
TFileHnd.ForceFileExtension
}
var
    Ext : string;
begin
    Ext := SysUtils.ExtractFileExt(OriginalFileName);
    if (Ext = EmptyStr) then begin
        Result := OriginalFileName + DefaultExtension;
    end else begin
        Result := OriginalFileName;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
class function TFileHnd.NextFamilyFilename(const BaseFilename : string) : string;
{{
Calcula o nome do arquivo seguinte para a sequência na pasta destino dada por BaseFilename.
Caso seja passsado BaseFilename já contendo um identificador de sequencia (n) este será desconsiderado, assim o nome
gerado não será sequência(n+1).

Revision: 5/12/2005 - Roger
}
var
    TargetExt, Prefix : string;
    TargetCount : Integer;
begin
    TargetCount := 0;
    TargetExt   := ExtractFileExt(BaseFilename);
    Prefix      := ChangeFileExt(BaseFilename, EmptyStr);
    repeat
        Inc(TargetCount);
        Result := Prefix + '(' + IntToStr(TargetCount) + ')' + TargetExt;
    until (not FileExists(Result));
end;

class function TFileHnd.ParentDir(const filename : string) : string;
{{
Retorna o caminho do diretorio pai do arquivo sem a incoveniente "\" final.

filename : Nome do arquivo desejado.
}
begin
    Result := SlashRem(ExtractFilePath(filename));
end;

class function TFileHnd.ParentDir(const filename : string; Existing : boolean) : string;
{{
Retorna o diretorio pai de um arquivo passado.
Caso "Existing" o arquivo deve ser válido. Caso contrário o novo pai é calculado até que um válido seja encontrado, ou EmptyStr
será retornada.

Revision: 11/7/2006 - Roger
}
begin
    Result := TFileHnd.ParentDir(filename);
    if (Existing) then begin
        if (not DirectoryExists(Result)) then begin
            if (IsRootDir(Result)) then begin  //tudo o possivel tentado
                Result := EmptyStr;
            end else begin
                if (Result <> EmptyStr) then begin
                    Result := TFileHnd.ParentDir(Result, True);
                end;
            end;
        end;
    end;
end;

class function TFileHnd.RmDir(const Path : string) : Integer;
{{
Apaga o caminho passado e seus arquivos e sub-diretorios bem como.

returns: Valor do codigo de erro do sistema operacional ou 0 se sucesso.

Nota: Verifique se o diretorio corrente do aplicativo não pertence ao conjunto a ser apagado.

Revision: 26/9/2005 - Roger
}
    function LSRDeleteFile(const DirName : string; FileParam : TEnumFileParam) : Integer;
    {{
    Apaga o arquivo dado se o mesmo for um sub-diretorio chama recursivamente a TFileHnd.RmDir()
    Revision: 26/9/2005 - Roger
    }
    var
        RemFilename : string;
    begin
        Result := ERROR_SUCCESS;
        if (FileParam.SR.Name <> '..') and (FileParam.SR.Name <> '.') then begin
            RemFilename := DirName + PathDelim + FileParam.SR.Name;
            if (not DeleteFile(RemFilename)) then begin    //tentar remover atributos impeditivos do arquivo
                if (not SetFileAttributes(PChar(RemFilename), FILE_ATTRIBUTE_NORMAL)) then begin
                    Result := GetLastError();
                end else begin
                    if (not DeleteFile(RemFilename)) then begin    //outro motivo de falha
                        Result := GetLastError();
                    end;
                end;
            end;
        end;
    end;

var
    ChildDir :  string;
    FileParam : TEnumFileParam;
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
                if (Result = ERROR_SUCCESS) then begin    //Conseguiu limpar seus arquivos
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

class function TFileHnd.SlashAdd(const Path : string) : string;
{{
Ensure a PathDelim at final of Path

Revision: 5/6/2008 - roger
}
begin
    if AnsiLastChar(Path)^ <> PathDelim then begin
        Result := Path + PathDelim;
    end else begin
        Result := Path;
    end;
end;

class function TFileHnd.SlashRem(const Path : string; PreserveRoot : boolean = False) : string;
{{
Path : Caminho original.

PreserveRootPath : Se true mantem a barra final para caso de caminho raiz. Vale para unidade lógica e UNC's

Revision: 30/10/2006 - Roger

Valor do separador de caminho normalizado para a constante localizada em "SysUtils.PathDelim".

}
var
    p : PChar;
begin
    p := AnsiLastChar(Path);
    if (p = nil) or (p^ <> SysUtils.PathDelim) then begin
        Result := Path;
    end else begin
        Result := Copy(Path, 1, Length(Path) - 1);
        if PreserveRoot then begin
            if IsRootDir(Path) then begin
                Result := Result + SysUtils.PathDelim;
            end;
        end;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
class function TFileHnd.VersionInfo(const Filename : string) : string;
{{
Monta string com a versão do arquivo passado, para qq erro encontrado '0.0.0.0' será retornado

Filename - Nome de arquivo de interesse. Caso string vazia seja passa o caminho de ParamStr(0) sera usado.

Returns: Versão do arquivo
}
const
    STR_DOT = '.';
var
    V1, V2, V3, V4 : Word;
begin
    BuildVersionInfo(Filename, V1, V2, V3, V4);
    Result := IntToStr(V1) + STR_DOT + IntToStr(V2) + STR_DOT + IntToStr(V3) + STR_DOT + IntToStr(V4);
end;



end.
