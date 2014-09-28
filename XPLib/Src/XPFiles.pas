{$IFDEF XPFiles}
{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I XPLib.inc}
unit XPFiles;

{ {
}

interface

uses
  SysUtils, Classes, Windows, Contnrs, Masks, FileHnd;

type
  EXPIOException = class(EInOutError);

  EFileException = class(EXPIOException)
	{ {
	  Classe base para erros com arquivos.


	  Revision: 10/8/2006 - Roger
	}
  public
	constructor Create(AErrorCode: Integer);
  end;

  TXPFileIterator = class
	{ {
	  Classe base para a construção de iteradores de arquivos/diretorios.

	  Todos os descendentes devem sobreescrever o método FileIterator de modo a realizar as operações desejadas.

	  FileIterator() deve retornar false se a iteração deve ser interrompida por qualquer motivo.

	  Revision: 16/8/2006 - Roger
	}
  public
	function FileIterator(var Filename: string; var FileRecord: TSearchRec): boolean; virtual; abstract;
  end;

  TFileCollector = class(TXPFileIterator)
	{ {
	  Classe descendente de TXPFileIterator com a habilidade de coletar informações nome e tamanho de arquivo via seu atributo List.

	  List.Strings[n] = Nome do arquivo/diretorio
	  List.Objects[n] = Tamanho do arquivo associado.

	  Revision: 16/8/2006 - Roger
	}
  private
	FSize: int64;
	FList: TStrings;
	procedure SetList(const Value: TStrings);
  public
	constructor Create();
	destructor Destroy; override;
	procedure Reset;
	function FileIterator(var Filename: string; var FileRecord: TSearchRec): boolean; override;
	property List: TStrings read FList write SetList;
	property Size: int64 read FSize;
  end;

  TXPFile = class(TObject)
	{ {
	  Classe representando um arquivo do sub-sistema de arquivos de uma máquina.

	  Revision: 16/8/2006 - Roger
	}
  private
	FPathname: string;
	function GetCanonicalPathname: string;
	function GetCanRead: boolean;
	function GetCanWrite: boolean;
	function GetExists: boolean;
	function GetIsAbsolute: boolean;
	function GetIsDirectory: boolean;
	function GetIsFile: boolean;
	function GetIsHidden: boolean;
	procedure SetIsHidden(const Value: boolean);
	function GetLastModified: TDateTime;
	procedure SetLastModified(const Value: TDateTime);
  protected
	function GetSize: int64; virtual;
  public
	constructor Create(const APathname: string); overload; virtual;
	constructor Create(const Parent, Child: string); overload; virtual;
	constructor Create(Parent: TXPFile; const Child: string); overload; virtual;
	function CompareTo(const APathname: string): Integer;
	function Delete: Integer; virtual;
	property CanonicalPathname: string read GetCanonicalPathname;
	property CanRead: boolean read GetCanRead;
	property CanWrite: boolean read GetCanWrite;
	property Exists: boolean read GetExists;
	property IsAbsolute: boolean read GetIsAbsolute;
	property IsDirectory: boolean read GetIsDirectory;
	property IsFile: boolean read GetIsFile;
	property IsHidden: boolean read GetIsHidden write SetIsHidden;
	property LastModified: TDateTime read GetLastModified write SetLastModified;
	property Pathname: string read FPathname;
	property Size: int64 read GetSize;
  end;

  TXPDirectory = class(TXPFile)
	{ {
	  Classe representando um diretorio do sistema de arquivos.

	  Revision: 16/8/2006 - Roger
	}
  private
	FIsRecursive: boolean;
	FExcludeVirtualDirs: boolean;
	function EnumFiles(Iterator: TXPFileIterator; const DirName: string): boolean; overload;
  protected
	function GetSize: int64; override;
	procedure DoFileEnumeration(Iterator: TXPFileIterator; var Filename: string; var FileRecord: TSearchRec); virtual;
	function FilterFile(const FileRecord: TSearchRec): boolean; virtual;
	procedure CreateNew(var Dir: TXPDirectory); virtual;
  public
	constructor Create(const APathname: string); override;
	function Delete: Integer; override;
	procedure List(List: TStrings);
	function EnumFiles(Iterator: TXPFileIterator): boolean; overload;
	property IsRecursive: boolean read FIsRecursive write FIsRecursive;
	property ExcludeVirtualDirs: boolean read FExcludeVirtualDirs write FExcludeVirtualDirs;
  end;

  TXPFilteredDirectory = class(TXPDirectory)
	{ {
	  Classe descendente de TXPDirectory que pode filtrar um sub-conjunto dos arquivos contidos
	  com os seguintes atributos:

	  IncludedAttrs : Conjunto de atributos que o arquivo/diretorio deve possuir pelo menos um deles
	  ExcludedAttrs : Conjunto de atributos que o arquivo/diretorio não pode possuir nenhum deles
	  IncludedMasks : Conjunto de máscaras que o arquivo/diretorio deve possuir pelo menos um deles
	  ExcludedMasks : Conjunto de máscaras que o arquivo/diretorio não pode possuir nenhum deles

	  Revision: 16/8/2006 - Roger
	}
  private
	FIncludedAttrs: Integer;
	FExcludedAttrs: Integer;
	FIncludedMasks: TStrings;
	FExcludedMasks: TStrings;
	FIncludedMaskList: TObjectList;
	FExcludedMaskList: TObjectList;
	procedure UpdateIncludeMasks(Sender: TObject);
	procedure UpdateExcludeMasks(Sender: TObject);
  protected
	function GetSize: int64; override;
	function FilterFile(const FileRecord: TSearchRec): boolean; override;
	procedure CreateNew(var Dir: TXPDirectory); override;
  public
	constructor Create(const APathname: string); override;
	destructor Destroy; override;
	property IncludedAttrs: Integer read FIncludedAttrs write FIncludedAttrs;
	property ExcludedAttrs: Integer read FExcludedAttrs write FExcludedAttrs;
	property IncludedMasks: TStrings read FIncludedMasks;
	property ExcludedMasks: TStrings read FExcludedMasks;
  end;

implementation

{ -**********************************************************************
  ************************************************************************
  ******************
  ******************  Class:    TXPFile
  ******************  Category: No category
  ******************
  ************************************************************************
  ************************************************************************ }
{ -------------------------------------------------------------------------------------------------------------------------------- }
function TXPFile.GetCanonicalPathname: string;
{ {
  Returns the canonical pathname string of this abstract pathname.

  A canonical pathname is both absolute and unique. The precise definition of canonical form is system-dependent. This method
  first converts this pathname to absolute form if necessary, as if by invoking the getAbsolutePath() method, and
  then maps it to its unique form in a system-dependent way. This typically involves removing redundant
  names such as "." and ".." from the pathname, resolving symbolic links (on UNIX platforms), and converting
  drive letters to a standard case (on Win32 platforms).

  Every pathname that denotes an existing file or directory has a unique canonical form. Every pathname that denotes a nonexistent
  file or directory also has a unique canonical form. The canonical form of the pathname of a nonexistent file or directory may be
  different from the canonical form of the same pathname after the file or directory is created. Similarly, the canonical form of the
  pathname of an existing file or directory may be different from the canonical form of the same pathname after the file or directory
  is deleted.

  Returns:

  The canonical pathname string denoting the same file or directory as this abstract pathname

  Revision: 9/8/2006 - Roger
}
begin
  Result := ExpandFileName(Self.FPathname);
end;

{ -------------------------------------------------------------------------------------------------------------------------------- }
function TXPFile.GetCanRead: boolean;
{ {
  Tests whether the application can read the file denoted by this abstract pathname.

  Returns:
  true if and only if the file specified by this abstract pathname exists and can be read by the application; false otherwiseRevision:

  Revision: 9/8/2006 - Roger
}
var
  Hnd: Integer;
begin
  Hnd := SysUtils.FileOpen(Self.CanonicalPathname, fmOpenRead);
  Result := (Hnd <> 0);
  if (Result) then begin
	FileClose(Hnd);
  end;
end;

{ -------------------------------------------------------------------------------------------------------------------------------- }
function TXPFile.GetCanWrite: boolean;
{ {
  Tests whether the application can modify to the file denoted by this abstract pathname.

  Returns:
  true if and only if the file system actually contains a file denoted by this abstract pathname and the application is allowed to
  write to the file; false otherwise

  Revision: 9/8/2006 - Roger
}
var
  Hnd: Integer;
begin
  Hnd := SysUtils.FileOpen(Self.CanonicalPathname, fmOpenWrite);
  Result := (Hnd <> 0);
  if (Result) then begin
	FileClose(Hnd);
  end;
end;

{ -------------------------------------------------------------------------------------------------------------------------------- }
function TXPFile.GetExists: boolean;
{ {
  Tests whether the file denoted by this abstract pathname exists.

  Returns:
  true if and only if the file denoted by this abstract pathname exists; false otherwise

  Revision: 9/8/2006 - Roger
}
begin
  Result := SysUtils.FileExists(Self.CanonicalPathname);
end;

{ -------------------------------------------------------------------------------------------------------------------------------- }
function TXPFile.GetIsAbsolute: boolean;
{ {
  Tests whether this abstract pathname is absolute. The definition of absolute pathname is system dependent. On UNIX
  systems, a pathname is absolute if its prefix is "/". On Win32 systems, a pathname is absolute if its prefix is a drive
  specifier followed by "\\", or if its prefix is "\\".

  Returns:
  true if this abstract pathname is absolute, false otherwise

  Revision: 9/8/2006 - Roger
}
begin
  Result := FileHnd.IsRootDir(Self.CanonicalPathname);
end;

{ -------------------------------------------------------------------------------------------------------------------------------- }
function TXPFile.GetIsDirectory: boolean;
{ {
  Tests whether the file denoted by this abstract pathname is a directory.

  Returns:
  true if and only if the file denoted by this abstract pathname exists and is a directory; false otherwise

  Revision: 9/8/2006 - Roger
}
begin
  Result := SysUtils.DirectoryExists(Self.CanonicalPathname);
end;

{ -------------------------------------------------------------------------------------------------------------------------------- }
function TXPFile.GetIsFile: boolean;
{ {
  Tests whether the file denoted by this abstract pathname is a normal file. A file is normal if it is not a directory.

  Returns:
  true if and only if the file denoted by this abstract pathname exists and is a normal file; false otherwise

  Revision: 9/8/2006 - Roger
}
begin
  Result := not Self.IsDirectory;
end;

{ -------------------------------------------------------------------------------------------------------------------------------- }
function TXPFile.GetIsHidden: boolean;
{ {
  Tests whether the file named by this abstract pathname is a hidden file. The exact definition of hidden is system-dependent. On UNIX
  systems, a file is considered to be hidden if its name begins with a period character ('.'). On Win32 systems, a file
  is considered to be hidden if it has been marked as such in the filesystem.

  Returns:
  true if and only if the file denoted by this abstract pathname is hidden according to the conventions of the underlying platform

  Revision: 9/8/2006 - Roger
}
var
  Rec: TSearchRec;
begin
  {$WARN SYMBOL_PLATFORM OFF}
  if (FindFirst(Self.CanonicalPathname, faHidden, Rec) = 0) then begin
	Result := (Rec.Attr and faHidden) <> 0;
	SysUtils.FindClose(Rec);
  end else begin
	Result := False;
  end;
  {$WARN SYMBOL_PLATFORM ON}
end;

{ -------------------------------------------------------------------------------------------------------------------------------- }
procedure TXPFile.SetIsHidden(const Value: boolean);
{ {
  Changes the Hidden flag attribute for the file.

  Revision: 9/8/2006 - Roger
}
var
  PrevAttrs, NewAttrs: Integer;
begin
  {$WARN SYMBOL_PLATFORM OFF}
  PrevAttrs := SysUtils.FileGetAttr(Self.CanonicalPathname);
  if (Value) then begin
	NewAttrs := PrevAttrs or faHidden;
  end else begin
	NewAttrs := PrevAttrs and (not faHidden);
  end;
  if (NewAttrs <> PrevAttrs) then begin
	SysUtils.FileSetAttr(Self.CanonicalPathname, NewAttrs);
  end;
  {$WARN SYMBOL_PLATFORM ON}
end;

{ -------------------------------------------------------------------------------------------------------------------------------- }
function TXPFile.GetLastModified: TDateTime;
{ {
  Returns the time that the file denoted by this abstract pathname was last modified.

  Returns:
  A value representing the time the file was last modified, or 0 if the file does not exist or if an I/O error occurs.

  Revision: 9/8/2006 - Roger
}
var
  SR: TSearchRec;
  DWrite: TFileTime;
begin
  if (SysUtils.FindFirst(Self.CanonicalPathname, faAnyFile, SR) = 0) then begin
	try
	  // ultima atualização
	  {$WARN SYMBOL_PLATFORM OFF}
	  DWrite := SR.FindData.ftLastWriteTime;
	  {$WARN SYMBOL_PLATFORM ON}
	  if (FileTimeToLocalFileTime(DWrite, DWrite)) then begin
		Result := TFileHnd.FileTimeToDateTime(DWrite);
	  end else begin
		Result := 0;
	  end;
	finally
	  SysUtils.FindClose(SR);
	end;
  end else begin
	Result := 0;
  end;
end;

{ -------------------------------------------------------------------------------------------------------------------------------- }
procedure TXPFile.SetLastModified(const Value: TDateTime);
{ {
  Change the time that the file denoted by this abstract pathname was last modified.

  Revision: 9/8/2006 - Roger
}
var
  DWrite: TFileTime;
  ST: TSystemTime;
  Hnd: THandle;
begin
  SetLastError(ERROR_SUCCESS);
  Hnd := CreateFile(PChar(Self.CanonicalPathname), GENERIC_WRITE, 0, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL or
	  FILE_FLAG_SEQUENTIAL_SCAN, 0);
  try
	if (Hnd <> INVALID_HANDLE_VALUE) then begin
	  DateTimeToSystemTime(Value, ST);
	  SystemTimeToFileTime(ST, DWrite);
	  LocalFileTimeToFileTime(DWrite, DWrite);
	  {$WARN UNSAFE_CODE OFF}
	  if not SetFileTime(Hnd, nil, nil, @DWrite) then begin
		raise EFileException.Create(GetLastError());
	  end;
	  {$WARN UNSAFE_CODE ON}
	end else begin
	  raise EFileException.Create(GetLastError());
	end;
  finally
	CloseHandle(Hnd);
  end;
end;

{ -------------------------------------------------------------------------------------------------------------------------------- }
function TXPFile.GetSize: int64;
{ {
  Returns the length of the file denoted by this abstract pathname.

  Returns:
  The length, in bytes, of the file denoted by this abstract pathname, or -1 if the file does not exist

  Revision: 9/8/2006 - Roger
}
begin
  Result := TFileHnd.FileSize(Self.CanonicalPathname);
end;

{ -------------------------------------------------------------------------------------------------------------------------------- }
function TXPFile.CompareTo(const APathname: string): Integer;
{ {
  Compares two abstract pathnames lexicographically. The ordering defined by this method depends upon the underlying
  system. On UNIX systems, alphabetic case is significant in comparing pathnames; on Win32 systems it is not.

  Parameters:
  pathname - The abstract pathname to be compared to this abstract pathname

  Returns:
  Zero if the argument is equal to this abstract pathname, a value less than zero if this abstract pathname is
  lexicographically less than the argument, or a value greater than zero if this abstract pathname is lexicographically
  greater than the argument

  Revision: 9/8/2006 - Roger
}
var
  Cmp: string;
begin
  Cmp := AnsiUpperCase(TFileHnd.SlashRem(APathname));
  Result := AnsiCompareText(Self.FPathname, Cmp);
end;

{ -------------------------------------------------------------------------------------------------------------------------------- }
constructor TXPFile.Create(const APathname: string);
{ {
  Creates a new File instance by converting the given pathname string into an abstract pathname.

  Revision: 9/8/2006 - Roger
}
begin
  Self.FPathname := TFileHnd.SlashRem(APathname);
end;

{ -------------------------------------------------------------------------------------------------------------------------------- }
constructor TXPFile.Create(const Parent, Child: string);
{ {
  Creates a new File instance from a parent pathname string and a child pathname string.

  Revision: 9/8/2006 - Roger
}
begin
  Create(TFileHnd.ConcatPath([Parent, Child]));
end;

{ -------------------------------------------------------------------------------------------------------------------------------- }
constructor TXPFile.Create(Parent: TXPFile; const Child: string);
{ {
  Creates a new File instance from a parent abstract pathname and a child pathname string.

  Revision: 9/8/2006 - Roger
}
begin
  Create(TFileHnd.ConcatPath([Parent.Pathname, Child]));
end;

{ -------------------------------------------------------------------------------------------------------------------------------- }
function TXPFile.Delete: Integer;
{ {
  Deletes the file or directory denoted by this abstract pathname. If this pathname denotes a directory, then the directory must be
  empty in order to be deleted or if the IsRecursive is set true the operation will be recursive too.

  Returns:
  ERROR_SUCCESS if ok, or The O.S Error code otherwise.

  Revision: 9/8/2006 - Roger
}
begin
  // apagar arquivo ordinario
  if (SysUtils.DeleteFile(Self.CanonicalPathname)) then begin
	Result := ERROR_SUCCESS;
  end else begin
	Result := GetLastError();
  end;
end;

{ -**********************************************************************
  ************************************************************************
  ******************
  ******************  Class:    EFileException
  ******************  Category: No category
  ******************
  ************************************************************************
  ************************************************************************ }
{ -------------------------------------------------------------------------------------------------------------------------------- }
constructor EFileException.Create(AErrorCode: Integer);
begin
  inherited Create(SysErrorMessage(AErrorCode));
  Self.ErrorCode := AErrorCode;
end;

{ -**********************************************************************
  ************************************************************************
  ******************
  ******************  Class:    TXPDirectory
  ******************  Category: No category
  ******************
  ************************************************************************
  ************************************************************************ }
{ -------------------------------------------------------------------------------------------------------------------------------- }
function TXPDirectory.GetSize: int64;
{ {
  Calcula o tamanho do conjunto de arquivos.

  Revision: 16/8/2006 - Roger
}
var
  Iterator: TFileCollector;
  Dir: TXPDirectory;
begin
  Dir := TXPDirectory.Create(Self.CanonicalPathname);
  try
	Iterator := TFileCollector.Create;
	try
	  Dir.EnumFiles(Iterator);
	  Result := Iterator.Size;
	finally
	  Iterator.Free;
	end;
  finally
	Dir.Free;
  end;
end;

{ -------------------------------------------------------------------------------------------------------------------------------- }
constructor TXPDirectory.Create(const APathname: string);
{ {
  Cria acesso ao diretorio dado por APathname.

  Revision: 16/8/2006 - Roger
}
begin
  inherited;
  Self.FIsRecursive := True;
  Self.FExcludeVirtualDirs := True;
  if (not Self.IsDirectory) then begin
	raise EFileException.Create(ERROR_NOT_SUPPORTED);
  end;
end;

{ -------------------------------------------------------------------------------------------------------------------------------- }
function TXPDirectory.Delete: Integer;
begin
  if (Self.IsDirectory) then begin
	if (Self.FIsRecursive) then begin
	  Result := TFileHnd.RmDir(Self.CanonicalPathname);
	end else begin
	  if (SysUtils.RemoveDir(Self.CanonicalPathname)) then begin
		Result := ERROR_SUCCESS;
	  end else begin
		Result := GetLastError();
	  end;
	end;
  end else begin
	// apagar arquivo ordinario
	if (SysUtils.DeleteFile(Self.CanonicalPathname)) then begin
	  Result := ERROR_SUCCESS;
	end else begin
	  Result := GetLastError();
	end;
  end;
end;

{ -------------------------------------------------------------------------------------------------------------------------------- }
procedure TXPDirectory.List(List: TStrings);
{ {
  Para todos os arquivos contidos no diretorio da instancia, bem como para os sub-diretorios se Self.IsRecursive verdadeiro
  os nomes dos arquivos serão adcionados a List, e o tamanho do mesmo pode ser acessado via List.Objects[n].

  Revision: 16/8/2006 - Roger
}
var
  Sum: TXPDirectory;
  Iterator: TFileCollector;
begin
  Self.CreateNew(Sum); // cria copia desta instancia
  try
	Iterator := TFileCollector.Create;
	try
	  Sum.EnumFiles(Iterator);
	  List.Assign(Iterator.List);
	finally
	  Iterator.Free;
	end;
  finally
	Sum.Free;
  end;
end;

constructor TXPFilteredDirectory.Create(const APathname: string);
begin
  inherited;
  Self.FIncludedMaskList := TObjectList.Create;
  Self.FExcludedMaskList := TObjectList.Create;
  Self.FIncludedMasks := TStringList.Create;
  TStringList(Self.FIncludedMasks).OnChange := Self.UpdateIncludeMasks;
  Self.FExcludedMasks := TStringList.Create;
  TStringList(Self.FExcludedMasks).OnChange := Self.UpdateExcludeMasks;
  Self.FIncludedMasks.Text := '*.*';
  Self.FIncludedAttrs := faAnyFile;
  Self.FExcludedAttrs := 0;
end;

procedure TXPFilteredDirectory.CreateNew(var Dir: TXPDirectory);
{ {
  Cria copia desta instancia, repassando todos os seus atributos.

  Revision: 16/8/2006 - Roger
}
begin
  Dir := TXPFilteredDirectory.Create(Self.CanonicalPathname);
  Dir.FIsRecursive := Self.FIsRecursive;
  Dir.FExcludeVirtualDirs := Self.FExcludeVirtualDirs;
  TXPFilteredDirectory(Dir).FIncludedAttrs := Self.FIncludedAttrs;
  TXPFilteredDirectory(Dir).FExcludedAttrs := Self.FExcludedAttrs;
  TXPFilteredDirectory(Dir).FIncludedMasks.Text := Self.FIncludedMasks.Text;
  TXPFilteredDirectory(Dir).FExcludedMasks.Text := Self.FExcludedMasks.Text;
end;

destructor TXPFilteredDirectory.Destroy;
{ {
  Libera as mascaras no formato string e mascaras.

  Revision: 16/8/2006 - Roger
}
begin
  Self.FIncludedMasks.Free;
  Self.FExcludedMasks.Free;
  Self.FIncludedMaskList.Free;
  Self.FExcludedMaskList.Free;
  inherited;
end;

function TXPFilteredDirectory.FilterFile(const FileRecord: TSearchRec): boolean;
{ {
  Baseado nos critérios da instancia sinaliza se o arquivo dado por FileRecord pertence ao conjunto de arquivos desejados.

  Se os critérios forem dúbios, prevalece a exclusão.

  Revision: 14/8/2006 - Roger
}
var
  MaskIndex: Integer;
begin
  Result := inherited FilterFile(FileRecord);
  if (Result) then begin
	Exit;
  end;
  // testa se algum atributo de exclusão pertence ao arquivo.
  Result := ((Self.FExcludedAttrs = faAnyFile) or ((FileRecord.Attr and Self.FExcludedAttrs) <> 0));
  if (not Result) then begin
	// testa a lista de mascaras de exclusao
	for MaskIndex := 0 to Self.FExcludedMasks.Count - 1 do begin
	  if (TMask(Self.FExcludedMaskList[MaskIndex]).Matches(FileRecord.Name)) then begin
		Result := True;
		Exit;
	  end;
	end;

	// Testa se algum dos atributos desejados esta presente
	Result := not((Self.FIncludedAttrs = faAnyFile) or ((Self.FIncludedAttrs and FileRecord.Attr) <> 0));
	if (not Result) then begin
	  Result := True;
	  // Pelo menos uma das mascaras deve ser atendida
	  for MaskIndex := 0 to Self.FIncludedMasks.Count - 1 do begin
		if (TMask(Self.FIncludedMaskList[MaskIndex]).Matches(FileRecord.Name)) then begin
		  Result := False;
		  Exit;
		end;
	  end;
	end;
  end;
end;

function TXPFilteredDirectory.GetSize: int64;
{ {
  Retorna o espaço ocupado pelos arquivos contidos no diretorio dado por Self.CanonicalPathname que atendem aos filtros e podem ou
  não pertencer aos sub-diretorios dependendo do valor Self.IsRecursive.

  Revision: 16/8/2006 - Roger
}
var
  Dir: TXPFilteredDirectory;
  Collector: TFileCollector;
begin
  Self.CreateNew(TXPDirectory(Dir));
  try
	Collector := TFileCollector.Create;
	try
	  Dir.EnumFiles(Collector);
	  Result := Collector.Size;
	finally
	  Collector.Free;
	end;
  finally
	Dir.Free;
  end;
end;

procedure TXPDirectory.DoFileEnumeration(Iterator: TXPFileIterator; var Filename: string; var FileRecord: TSearchRec);
{ {
  Repassa os dados do arquivo encontrado ao iterador passado.

  Revision: 16/8/2006 - Roger
}
begin
  Iterator.FileIterator(Filename, FileRecord);
end;

function TXPDirectory.EnumFiles(Iterator: TXPFileIterator): boolean;
{ {
  Para cada arquivo encontrado neste diretorio bem como os sub-diretorios se Self.IsRecursive ligado será chamado o metodo
  Iterator.FileIterator, se este metodo retornar false o processo de iteração será interrompido.

  Revision: 11/8/2006 - Roger
}
begin
  Result := Self.EnumFiles(Iterator, Self.CanonicalPathname);
end;

function TXPDirectory.FilterFile(const FileRecord: TSearchRec): boolean;
{ {
  Baseado nos critérios da instancia sinaliza se o arquivo dado por FileRecord pertence ao conjunto de arquivos desejados.

  Se os critérios forem dúbios, prevalece a exclusão.

  Revision: 14/8/2006 - Roger
}
begin
  Result := Self.FExcludeVirtualDirs and ((FileRecord.Name = '.') or (FileRecord.Name = '..'));
  // todos os arquivos são pertencem ao conjunto valido
end;

procedure TXPFilteredDirectory.UpdateExcludeMasks(Sender: TObject);
{ {
  Limpa a lista de mascaras de exclusão e insere todos os items isoladamente.

  Revision: 14/8/2006 - Roger
}
var
  i: Integer;
  ItMask: TMask;
begin
  Self.FExcludedMaskList.Clear;
  for i := 0 to Self.FExcludedMasks.Count - 1 do begin
	ItMask := TMask.Create(Self.FExcludedMasks.Strings[i]);
	Self.FExcludedMaskList.Add(ItMask);
  end;
end;

procedure TXPFilteredDirectory.UpdateIncludeMasks(Sender: TObject);
{ {
  Limpa a lista de mascaras de inclusão e insere todos os items isoladamente.

  Revision: 14/8/2006 - Roger
}
var
  i: Integer;
  ItMask: TMask;
begin
  Self.FIncludedMaskList.Clear;
  for i := 0 to Self.FIncludedMasks.Count - 1 do begin
	ItMask := TMask.Create(Self.FIncludedMasks.Strings[i]);
	Self.FIncludedMaskList.Add(ItMask);
  end;
end;

function TXPDirectory.EnumFiles(Iterator: TXPFileIterator; const DirName: string): boolean;
{ {
  Enumera os arquivos contidos no caminho dado por DirName.

  Pode ser chamado recursivamente caso o valor de Self.IsRecursive seja verdadeiro.

  PRE_C: Iterator ser uma instancia valida.

  Revision: 16/8/2006 - Roger
}
var
  SR: TSearchRec;
  Name: string;
begin
  Result := True;
  if (FindFirst(TFileHnd.ConcatPath([DirName, '*.*']), faAnyFile, SR) = ERROR_SUCCESS) then begin
	try
	  repeat
		if (not Self.FilterFile(SR)) then begin
		  name := TFileHnd.ConcatPath([DirName, SR.Name]);
		  Result := Iterator.FileIterator(name, SR);
		  if (not Result) then begin
			Break; // suspende a iteracao
		  end;
		end;
		if (Self.IsRecursive and ((SR.Attr and faDirectory) = faDirectory) and (SR.Name <> '.') and (SR.Name <> '..')) then begin
		  // recursivamente vai para a sub-pasta
		  Result := Self.EnumFiles(Iterator, TFileHnd.ConcatPath([DirName, SR.Name]));
		  if (not Result) then begin
			Break;
		  end;
		end;
	  until (FindNext(SR) <> ERROR_SUCCESS);
	finally
	  SysUtils.FindClose(SR);
	end;
  end;
end;

constructor TFileCollector.Create;
{ {
  Inicia a instancia com a lista interna usada na enumeração de coleta de dados.

  Revision: 16/8/2006 - Roger
}
begin
  Self.FList := TStringList.Create;
end;

destructor TFileCollector.Destroy;
{ {
  Libera a lista interna de coleta de dados.

  Revision: 16/8/2006 - Roger
}
begin
  Self.FList.Free;
  inherited;
end;

function TFileCollector.FileIterator(var Filename: string; var FileRecord: TSearchRec): boolean;
{ {
  Para esta classe a chamada de iteração adciona a lista interna o caminho completo do arquivo e incrementa o valor do acumulado total
  do espaço usado.

  Como este iterador não cancela a operação o seu retorno sempre sera true.

  Revision: 16/8/2006 - Roger
}
begin
  Self.FList.AddObject(Filename, TObject(FileRecord.Size));
  Inc(Self.FSize, FileRecord.Size);
  Result := True;
end;

procedure TFileCollector.Reset;
{ {
  Limpa a lista interna e o acumulador de espaço usado.

  Revision: 16/8/2006 - Roger
}
begin
  Self.FSize := 0;
  Self.FList.Clear;
end;

procedure TFileCollector.SetList(const Value: TStrings);
{ {
  Atribui um valor a lista interna de arquivos.

  NOTA: O valor do acumulador não será alterado nesta versão.

  Revision: 16/8/2006 - Roger
}
begin
  Self.FList.Assign(Value);
end;

procedure TXPDirectory.CreateNew(var Dir: TXPDirectory);
{ {
  Cria copia desta instancia.

  Revision: 16/8/2006 - Roger
}
begin
  Dir := TXPDirectory.Create(Self.CanonicalPathname);
  Dir.FIsRecursive := Self.FIsRecursive;
  Dir.FExcludeVirtualDirs := Self.FExcludeVirtualDirs;
end;

end.
