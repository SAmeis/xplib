{$IFDEF WinFilesHnd}
	{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I WinSysLib.inc}

unit WinFilesHnd deprecated;

interface

uses
	Windows, SysUtils, Classes, WinDisks, FileHnd;

//{Verifica se existe aquele diretorio no disco
function __DirectoryExists(const Name : string) : boolean;

//Retorna caminmho para a imagem de runtime corrente
function CurrentModuleFileName() : string;

//Carrega os nomes dos metodos dinamicamente carregaveis do arquivo passado
procedure ListLoadableLibraryFunctions( ImageFileName : string; List: TStrings );


implementation

uses
	ImageHlp; // routines to access debug information

function __DirectoryExists(const Name : string) : boolean;
//----------------------------------------------------------------------------------------------------------------------------------
//{Verifica se existe aquele diretorio no disco
var
	Entry : TSearchRec;
	Dir, ActualDir : string;
	Size : integer;
begin
	if not DirectoryExists(Name) then begin
		Size:=Length(Name);
		Dir:=Name;
		if Dir[Size] = '\' then begin
			Dir:=Copy(Dir, 1, Size -1);
			Size:=Length(Dir);
			if Dir[Size] = ':' then begin
				Result:=(SpaceFree(Dir + '\*') >=0);
				Exit;
			end;
		end;
		if (Dir[1] = '\') and (Dir[2] <> '\') then begin //raiz da uindade atual
			Dir:=Chr(GetDefaultDrive) + Dir;
		end;
		if Dir[1]+Dir[2] = '..' then begin //a aparicao de diretorios pai deve ser melhorada
			GetDir(0, ActualDir);
			Dir:=TFileHnd.ParentDir(ActualDir)+Copy(Dir,3,Length(Dir));
		end;
		Size:=FindFirst(Dir,faAnyFile ,Entry);
		Result:=(Size = 0);
		if Result then begin
			FindClose(Entry);
		end;
		if (not(Result)) and (Length(Dir) = 3) and (Dir[3] = '\') then begin
			Result:=DriveReady(Ord(UpCase(Dir[1]))-Ord('@'));
		end;
	end else begin
		Result:=True;
	end;
end;

function CurrentModuleFileName() : string;
//----------------------------------------------------------------------------------------------------------------------------------
//Retorna caminmho para a imagem de runtime corrente
var
	FileName: array[0..255] of Char;
begin
	if IsLibrary then begin
		GetModuleFileName(HInstance, FileName, SizeOf(FileName) - 1);
		Result := StrPas(FileName);
	end else begin
		Result := ParamStr(0);
	end;
end;


procedure ListLoadableLibraryFunctions( ImageFileName : string; List: TStrings );
//----------------------------------------------------------------------------------------------------------------------------------
//Carrega os nomes dos metodos dinamicamente carregaveis do arquivo passado
// by Dmitry Streblechenko
//----------------------------------------------------------------------------------------------------------------------------------
type
  chararr = array [0..$FFFFFF] of Char;
//..................................................................................................................................
var
	H: THandle;
	I, fc: integer;
	st: string;
	arr: Pointer;
	ImageDebugInformation: PImageDebugInformation;
begin
	List.Clear;
	ImageFileName := ExpandFileName(ImageFileName);
	if FileExists(ImageFileName) then begin
		H := CreateFile(PChar(ImageFileName), GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
		if H <> INVALID_HANDLE_VALUE then begin
			try
			  ImageDebugInformation := MapDebugInformation(H, PAnsiChar(ImageFileName), nil, 0);
			  if ( ImageDebugInformation <> nil ) then begin
				  try
					  arr := ImageDebugInformation^.ExportedNames;
					  fc := 0;
					  for I := 0 to ImageDebugInformation^.ExportedNamesSize - 1 do begin
						  if chararr(arr^)[I]=#0 then begin
							  st := PChar(@chararr(arr^)[fc]);
							  if Length(st)>0 then begin
								  List.Add(st);
							  end;
							  if (I>0) and (chararr(arr^)[I-1]=#0) then begin
								  Break;
							  end;
							  fc := I + 1
						  end;
					  end;
				  finally
					  UnmapDebugInformation(ImageDebugInformation)
				  end;
			  end;
			finally
			  CloseHandle(H)
			end;
		end;
	end;
end;


end.
