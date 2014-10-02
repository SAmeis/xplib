{$IFDEF FileLogs}
{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I StFLib.inc}
{ NOTAS :
  Descricao : Implementa rotinas e classes para "logar" propriedades de arquivos/pastas/disco/rede, etc...
}

unit FileLogs;

interface

uses
  Windows, Classes, SysUtils, FileHnd, Masks, contnrs;

type
  TDirLogBasic = class
  private
	ByPassStringList: TStringList;
	IncludeMaskList: TObjectList;
	ExcludeMaskList: TObjectList;
	FIncSubDirs: boolean;
	FDirName: string;
  protected

  public
	property DirName: string read FDirName write FDirName;
	property IncSubDirs: boolean read FIncSubDirs write FIncSubDirs;
	constructor Create(const ADirName: string; const IncludeFileMasks, ExcludeFileMasks: array of string; bIncSubDirs: boolean);
	destructor Destroy; override;
	procedure LogFileSizes(FileNames: TStringList);
  end;

implementation

constructor TDirLogBasic.Create(const ADirName: string; const IncludeFileMasks, ExcludeFileMasks: array of string;
	bIncSubDirs: boolean);
var
  i: integer;
  Mask: TMask;
begin
  inherited Create;
  Self.IncludeMaskList := TObjectList.Create;
  Self.ExcludeMaskList := TObjectList.Create;
  Self.DirName := ADirName;
  Self.IncSubDirs := bIncSubDirs;
  // Ajusta mascara de filtro para arquivos a serem incluidos
  for i := low(IncludeFileMasks) to high(IncludeFileMasks) do begin
	Mask := TMask.Create(IncludeFileMasks[i]);
	Self.IncludeMaskList.Add(Mask);
  end;
  // Ajusta mascara de filtro para arquivos a serem excluidos
  for i := low(ExcludeFileMasks) to high(ExcludeFileMasks) do begin
	Mask := TMask.Create(ExcludeFileMasks[i]);
	Self.ExcludeMaskList.Add(Mask);
  end;
end;

destructor TDirLogBasic.Destroy;
// ----------------------------------------------------------------------------------------------------------------------
begin
  Self.IncludeMaskList.Free;
  Self.ExcludeMaskList.Free;
  inherited;
end;

procedure TDirLogBasic.LogFileSizes(FileNames: TStringList);
{ {
  Enumera arquivos setando o objectes do stringlist com o tamanho do mesmo
}
var
  FileParam: TEnumFileParam;
  // ..............................................................
  function LocalFileHandler(const FileName: string; FileParam: TEnumFileParam): integer;
  { {
	Adciona arquivo e tamanho atual a lista
  }
  var
	i: integer;
	DLog: TDirLogBasic;
	FFName: string;
  begin
	{$WARN UNSAFE_CODE OFF}
	Result := 0;
	if (FileParam.SR^.Attr and faDirectory) = 0 then begin
	  DLog := FileParam.Data;
	  // Busca por excluidos
	  FFName := ExtractFileName(FileName);
	  for i := 0 to DLog.ExcludeMaskList.Count - 1 do begin
		if TMask(DLog.ExcludeMaskList.Items[i]).Matches(FFName) then begin
		  Exit; // Nao insere este arquivo
		end;
	  end;
	  // Busca por incluidos
	  for i := 0 to DLog.IncludeMaskList.Count - 1 do begin
		if TMask(DLog.IncludeMaskList.Items[i]).Matches(FFName) then begin
		  DLog.ByPassStringList.AddObject(FileName, TObject(FileParam.SR^.Size));
		  Break;
		end;
	  end;
	end;
	{$WARN UNSAFE_CODE ON}
  end;

// ................................................................................................................................
begin
  Self.ByPassStringList := FileNames;
  FileParam := TEnumFileParam.Create;
  try
	FileParam.Data := Self;
	if Self.FIncSubDirs then begin
	  FileHnd.EnumFiles(Self.FDirName, '*.*', FileParam, TEnumFileProc(@LocalFileHandler));
	end else begin
	  FileHnd.EnumFilesDir(Self.FDirName, '*.*', FileParam, TEnumFileProc(@LocalFileHandler));
	end;
  finally
	FileParam.Free;
  end;
end;

end.
