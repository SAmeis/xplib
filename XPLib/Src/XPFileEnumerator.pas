{$IFDEF XPFileEnumerator}
{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I XPLib.inc}
// Removidos todos os alertas de plataforma para esta unit
{$WARN UNIT_PLATFORM OFF} {$WARN SYMBOL_PLATFORM OFF}
// -/Exemplo basico de enumeração de arquivos
// -/var
// -/	 IFiles :  IEnumerable<TFileSystemEntry>;
// -/	 f :      TFileSystemEntry;
// -/begin
// -/	IFiles := TDirectory.FileSystemEntries( Path, FileMaks, RecursiveFlag );
// -/	for f in IFiles do begin
// -/		WriteLn( System.Output, f.FullName );
// -/	end;
// -/end;

unit XPFileEnumerator;

interface

uses
  SysUtils, Masks, Generics.Collections;

type
  { Override System's IEnumerator }
  IEnumerator<T> = interface
	function MoveNext: Boolean;
	function GetCurrent: T;
	property Current: T read GetCurrent;
  end;

  { Override System's IEnumerable }
  IEnumerable<T> = interface
	function GetEnumerator: IEnumerator<T>;
  end;

  { Used for full enumerator }
  TFileSystemEntry = record
  private
	FName: string;
	FFullPath: string;
	FSize: Int64;
	FAttr: Integer;

	{ Attribute reader }
	function GetAttribute(const Attribute: Integer): Boolean;

  public
	{ Public accessors }
	property name: string read FName;
	property FullName: string read FFullPath;
	property Size: Int64 read FSize;
	property Attributtes: Integer read FAttr;

	{ Attribute readings }
	property IsDirectory: Boolean index faDirectory read GetAttribute;
	property IsReadOnly: Boolean index faReadOnly read GetAttribute;
	property IsHidden: Boolean index faHidden read GetAttribute;
	property IsSystem: Boolean index faSysFile read GetAttribute;
	property IsArchive: Boolean index faArchive read GetAttribute;
	property IsSymLink: Boolean index faSymLink read GetAttribute;
	property IsNormal: Boolean index faNormal read GetAttribute;
	property IsTemporary: Boolean index faTemporary read GetAttribute;
  end;

  { Contains only statics }
  TDirectory = record
  strict private
  type
	TDirEnumType = (etAll, etDirectories, etFiles);

	{ The FS enumerator object }
	TStructEnumerator = class(TInterfacedObject, IEnumerator<TFileSystemEntry>)
	private
	  { General }
	  FPath: string;
	  FPattern: string;
	  FRecursive: Boolean;
	  FReturnFullPath: Boolean;
	  FType: TDirEnumType;
	  FMask: TMask;

	  { __State__based__ }
	  FBegin: Boolean;
	  FPathSoFar: string;
	  FCurrent: TFileSystemEntry;
	  FCurrentRec: TSearchRec;
	  FStoredRecs: TStack<TSearchRec>;

	  { Used internally to perform the actual iterations }
	  function InternalMoveNext(): Boolean;
	public
	  constructor Create(const APath, APattern: string; const ARecursive: Boolean);
	  destructor Destroy; override;

	  { IEnumerator }
	  function MoveNext: Boolean;

	  { IEnumerator<T> }
	  function GetCurrent: TFileSystemEntry;
	  property Current: TFileSystemEntry read GetCurrent;
	end;

	{ The FS enumerator object }
	TNameEnumerator = class(TInterfacedObject, IEnumerator<string>)
	private
	  { The underlying enumerator object }
	  FEnumerator: TStructEnumerator;
	  FType: TDirEnumType;
	  FReturnFullPath: Boolean;
	  FCurrent: TFileSystemEntry;

	public
	  constructor Create(const APath, APattern: string; const ARecursive, AReturnFullPath: Boolean; const AEnumType: TDirEnumType);
	  destructor Destroy; override;

	  { IEnumerator }
	  function MoveNext: Boolean;

	  { IEnumerator<T> }
	  function GetCurrent: string;
	  property Current: string read GetCurrent;
	end;

	{ The FS/Name enumerable object }
	TNameEnumerable = class(TInterfacedObject, IEnumerable<string>)
	private
	  FPath: string;
	  FPattern: string;
	  FRecursive: Boolean;
	  FReturnFullPath: Boolean;
	  FType: TDirEnumType;

	public
	  constructor Create(const APath, APattern: string; const ARecursive, AReturnFullPath: Boolean; const AEnumType: TDirEnumType);

	  { IEnumerable<T> }
	  function GetEnumerator: IEnumerator<string>; overload;
	end;

	{ The FS/Data enumerable object }
	TStructEnumerable = class(TInterfacedObject, IEnumerable<TFileSystemEntry>)
	private
	  FPath: string;
	  FPattern: string;
	  FRecursive: Boolean;

	public
	  constructor Create(const APath, APattern: string; const ARecursive: Boolean);

	  { IEnumerable<T> }
	  function GetEnumerator: IEnumerator<TFileSystemEntry>; overload;
	end;

  const
	SAllEntries = '*.*';

  public
	{ Used to enumerate Directories, Files or Entries in a given path }
	class function Directories(const APath: string; const ARecursive: Boolean = false; const AReturnFullPath: Boolean = false)
		: IEnumerable<string>; overload; static;

	class function Directories(const APath, APattern: string; const ARecursive: Boolean = false;
		const AReturnFullPath: Boolean = false): IEnumerable<string>; overload; static;

	class function Files(const APath: string; const ARecursive: Boolean = false; const AReturnFullPath: Boolean = false)
		: IEnumerable<string>; overload; static;

	class function Files(const APath, APattern: string; const ARecursive: Boolean = false; const AReturnFullPath: Boolean = false)
		: IEnumerable<string>; overload; static;

	class function Entries(const APath: string; const ARecursive: Boolean = false; const AReturnFullPath: Boolean = false)
		: IEnumerable<string>; overload; static;

	class function Entries(const APath, APattern: string; const ARecursive: Boolean = false; const AReturnFullPath: Boolean = false)
		: IEnumerable<string>; overload; static;

	{ Full file information }
	class function FileSystemEntries(const APath: string; const ARecursive: Boolean = false): IEnumerable<TFileSystemEntry>;
		overload; static;

	class function FileSystemEntries(const APath, APattern: string; const ARecursive: Boolean = false)
		: IEnumerable<TFileSystemEntry>; overload; static;
  end;

implementation

{ TDirectory.TStructEnumerator }

constructor TDirectory.TNameEnumerable.Create(const APath, APattern: string; const ARecursive, AReturnFullPath: Boolean;
	const AEnumType: TDirEnumType);
begin
  FPath := APath;
  FRecursive := ARecursive;
  FReturnFullPath := AReturnFullPath;
  FType := AEnumType;
  FPattern := APattern;
end;

function TDirectory.TNameEnumerable.GetEnumerator: IEnumerator<string>;
begin
  { Create an enumerator object }
  Result := TNameEnumerator.Create(FPath, FPattern, FRecursive, FReturnFullPath, FType);
end;

{ TDirectory.TStructEnumerator }

constructor TDirectory.TStructEnumerator.Create(const APath, APattern: string; const ARecursive: Boolean);
begin
  FPath := IncludeTrailingPathDelimiter(APath);
  FRecursive := ARecursive;
  FPattern := APattern;

  { .. }
  FBegin := false;
  FPathSoFar := '';

  { Only initilize the stack for recursive enumerators }
  if ARecursive then begin
	FStoredRecs := TStack<TSearchRec>.Create();

	{ When recursive allow level 1 masks, and introduce level 2 }
	FMask := TMask.Create(FPattern);
	FPattern := SAllEntries;
  end;
end;

destructor TDirectory.TStructEnumerator.Destroy;
begin
  { Close current rec ... in any case }
  FindClose(FCurrentRec);

  while (FRecursive) and (FStoredRecs.Count > 0) do begin
	{ Close all stored serach recs }
	FCurrentRec := FStoredRecs.Pop();
	FindClose(FCurrentRec);
  end;

  { Kill the stack }
  FStoredRecs.Free;
  FMask.Free;
  inherited;
end;

function TDirectory.TStructEnumerator.GetCurrent: TFileSystemEntry;
begin
  { Return the current file }
  Result := FCurrent;
end;

function TDirectory.TStructEnumerator.InternalMoveNext: Boolean;
var
  LCanRecurse: Boolean;
begin
  Result := false;
  LCanRecurse := true;

  while true do begin
	{ The search is just starting }
	if not FBegin then begin
	  { Initiate search }
	  Result := FindFirst(FPath + FPathSoFar + FPattern, faAnyFile, FCurrentRec) = 0;

	  { Mark we've been here already }
	  FBegin := true;
	end else begin
	  { Here we decide whether the previous entry was a directory and whether we should recurse into it
	  }
	  if LCanRecurse and ((FCurrentRec.Attr and faDirectory) = faDirectory) and (FRecursive) and (FCurrentRec.Name <> '.') and
		  (FCurrentRec.Name <> '..') then begin
		{ ... it sure looks like it }
		FStoredRecs.Push(FCurrentRec);
		FBegin := false;
		FPathSoFar := FPathSoFar + FCurrentRec.Name + PathDelim;

		{ And restart the internal loop }
		continue;
	  end;

	  { Otherwise continue enumerating locally }
	  // LCanRecurse := true;
	  Result := (FindNext(FCurrentRec) = 0);
	end;

	{ If we've got to the bottom of the directory }
	if not Result then begin
	  { Close this search record }
	  FindClose(FCurrentRec);

	  if (FRecursive) and (FStoredRecs.Count > 0) then begin
		{ We've reched the bottom in this directory, return one up }
		FCurrentRec := FStoredRecs.Pop();

		{ YOU SHALL NOT RECURSE BACK! }
		LCanRecurse := false;

		{ Revert the path a position back }
		FPathSoFar := ExtractFilePath(ExcludeTrailingPathDelimiter(FPathSoFar));

		{ Continue the loop to get the next entry }
		continue;
	  end;
	end;

	{ Leave function here }
	Exit;
  end;
end;

function TDirectory.TStructEnumerator.MoveNext: Boolean;
begin
  { Cycle until something mathing the criteria is found and can be returned. }
  while InternalMoveNext() do begin
	{ Verify level 2 mask if required }
	if FRecursive and not FMask.Matches(FCurrentRec.Name) then begin
	  continue;
	end;

	{ Built the exit structure }
	FCurrent.FName := FPathSoFar + FCurrentRec.Name;
	FCurrent.FFullPath := FPath + FPathSoFar + FCurrentRec.Name;
	FCurrent.FSize := FCurrentRec.Size;
	FCurrent.FAttr := FCurrentRec.Attr;

	Exit(true);
  end;

  { Nothing, we've reached the bottom }
  Result := false;
end;

{ TDirectory }

class function TDirectory.Directories(const APath: string; const ARecursive: Boolean; const AReturnFullPath: Boolean)
	: IEnumerable<string>;
begin
  { Create an enumerable for Directories only }
  Result := TNameEnumerable.Create(APath, SAllEntries, ARecursive, AReturnFullPath, etDirectories);
end;

class function TDirectory.Entries(const APath: string; const ARecursive: Boolean; const AReturnFullPath: Boolean)
	: IEnumerable<string>;
begin
  { Create an enumerable for All entries }
  Result := TNameEnumerable.Create(APath, SAllEntries, ARecursive, AReturnFullPath, etAll);
end;

class function TDirectory.Files(const APath: string; const ARecursive: Boolean; const AReturnFullPath: Boolean)
	: IEnumerable<string>;
begin
  { Create an enumerable for Files only }
  Result := TNameEnumerable.Create(APath, SAllEntries, ARecursive, AReturnFullPath, etFiles);
end;

class function TDirectory.Directories(const APath, APattern: string; const ARecursive: Boolean; const AReturnFullPath: Boolean)
	: IEnumerable<string>;
begin
  { Create an enumerable for Directories only }
  Result := TNameEnumerable.Create(APath, APattern, ARecursive, AReturnFullPath, etDirectories);
end;

class function TDirectory.Entries(const APath, APattern: string; const ARecursive: Boolean; const AReturnFullPath: Boolean)
	: IEnumerable<string>;
begin
  { Create an enumerable for All entries }
  Result := TNameEnumerable.Create(APath, APattern, ARecursive, AReturnFullPath, etAll);
end;

class function TDirectory.Files(const APath, APattern: string; const ARecursive: Boolean; const AReturnFullPath: Boolean)
	: IEnumerable<string>;
begin
  { Create an enumerable for Files only }
  Result := TNameEnumerable.Create(APath, APattern, ARecursive, AReturnFullPath, etFiles);
end;

class function TDirectory.FileSystemEntries(const APath: string; const ARecursive: Boolean): IEnumerable<TFileSystemEntry>;
begin
  { Create an enumerable for Files only }
  Result := TStructEnumerable.Create(APath, SAllEntries, ARecursive);
end;

class function TDirectory.FileSystemEntries(const APath, APattern: string; const ARecursive: Boolean)
	: IEnumerable<TFileSystemEntry>;
begin
  { Create an enumerable for Files only }
  Result := TStructEnumerable.Create(APath, APattern, ARecursive);
end;

{ TDirectory.TStructEnumerable }

constructor TDirectory.TStructEnumerable.Create(const APath, APattern: string; const ARecursive: Boolean);
begin
  FPath := APath;
  FPattern := APattern;
  FRecursive := ARecursive;
end;

function TDirectory.TStructEnumerable.GetEnumerator: IEnumerator<TFileSystemEntry>;
begin
  Result := TStructEnumerator.Create(FPath, FPattern, FRecursive);
end;

{ TDirectory.TNameEnumerator }

constructor TDirectory.TNameEnumerator.Create(const APath, APattern: string; const ARecursive, AReturnFullPath: Boolean;
	const AEnumType: TDirEnumType);
begin
  { Create a normal used enumerator }
  FEnumerator := TStructEnumerator.Create(APath, APattern, ARecursive);

  { Store locals }
  FType := AEnumType;
  FReturnFullPath := AReturnFullPath;
end;

destructor TDirectory.TNameEnumerator.Destroy;
begin
  { Kill enumerator }
  FEnumerator.Free;

  inherited;
end;

function TDirectory.TNameEnumerator.GetCurrent: string;
begin
  if FReturnFullPath then
	Result := FCurrent.FFullPath
  else
	Result := FCurrent.FName;
end;

function TDirectory.TNameEnumerator.MoveNext: Boolean;
begin
  { Cycle until something mathing the criteria is found
	and can be returned. }
  while FEnumerator.MoveNext() do begin
	{ Get the current selected struct }
	FCurrent := FEnumerator.Current;

	{ Do not show the . and .. directories }
	if (FCurrent.FName = '.') or (FCurrent.FName = '..') then
	  continue;

	{ Prepare the exit stuff }
	Result := true;

	if FType = etDirectories then begin
	  { Only return the result for a directory }
	  if (FCurrent.FAttr and faDirectory) <> 0 then
		Exit;
	end else if FType = etFiles then begin
	  { Only return the result for a file }
	  if (FCurrent.FAttr and faDirectory) = 0 then
		Exit;
	end
	else
	  Exit; { Always return the result }
  end;

  { Nothing, we've reached the bottom }
  Result := false;
end;

{ TFileSystemEntry }

function TFileSystemEntry.GetAttribute(const Attribute: Integer): Boolean;
begin
  Result := (FAttr and Attribute) <> 0;
end;

end.
