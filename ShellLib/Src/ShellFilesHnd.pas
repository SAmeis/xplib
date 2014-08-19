unit ShellFilesHnd;

interface

uses
	 Windows, SysUtils, ShellAPI, Types, Str_Pas, FileHnd, Graphics, Classes, Controls;

type
	 TExecutableType = (etDOS, etWIN16, etWIN32, etWINCONSOLE, etBATCH, etPIF, etLINK, etERROR);

	 TShellHnd = class
	 public
		 class function GetAllUsersDesktop() : string;
		 class procedure CreateShellShortCut(const TargetName, LinkFileName, IconFilename : WideString; IconIndex : Integer = 0);
	 end;

const
    FOF_DEFAULT_IDEAL    = FOF_MULTIDESTFILES + FOF_RENAMEONCOLLISION + FOF_NOCONFIRMATION + FOF_ALLOWUNDO +
        FOF_FILESONLY + FOF_NOCONFIRMMKDIR + FOF_NOERRORUI + FOF_SIMPLEPROGRESS;
    FOF_DEFAULT_DELTREE  = FOF_NOCONFIRMATION + FOF_ALLOWUNDO + FOF_NOERRORUI;
    FOF_DEFAULT_COPY     = FOF_NOCONFIRMATION + FOF_ALLOWUNDO + FOF_NOCONFIRMMKDIR + FOF_NOERRORUI + FOF_MULTIDESTFILES;
    FOF_DEFAULT_DELFILES = FOF_DEFAULT_DELTREE;

//{Retorna o tipo/plataforma do requerida pelo executavel/atalho
function GetExeFileType(FileName : string) : TExecutableType;
//Copia todos os arquivos de uma pasta para outra
function ShellCopyDir(hWnd : THandle; const Source, Dest : string; Flags : FILEOP_FLAGS; WinTitle : PChar) : Integer;
//Apaga os arquivos da mascara usando a animacao do windows
function ShellDeleteFiles(hWnd : THandle; const DirName : string; Flags : FILEOP_FLAGS; WinTitle : PChar) : Integer;
{Apaga arquivos/Diretorios atraves do shell do windows}
function ShellDelTree(hWnd : THandle; const Name : string; Flags : FILEOP_FLAGS; WinTitle : PChar) : boolean;
//Move pasta pelo shell do Windows
function ShellMoveDir(hWnd : THandle; const Source, Dest : string; Flags : FILEOP_FLAGS; WinTitle : PChar) : Integer;
//Seleciona um diretorio pelo uso do shell *****NOTA: Incompativel com o NT4 segundo a documentacao******
function ShellSelectDir(hWnd : THandle; UserMessage : PChar) : string;
//Pega o ícone associado à extensão passada como parametro
function GetIconFromExtension(const Ext : string; var Icon : TIcon) : boolean;

implementation

uses
    ShlObj, ActiveX, ComObj;

function GetIconFromExtension(const Ext : string; var Icon : TIcon) : boolean;
    //----------------------------------------------------------------------------------------------------------------------
const
    _TEMP_FILE_ = 'GetIExt';
var
    AHandle :  DWORD;
    TempFile : TFileStream;
    FileInfo : TSHFileInfo;
    Icons :    TImageList;
    FileName : string;
begin
    // Cria a lista de imagens e o arquivo temporario
    Icons    := TImageList.CreateSize(32, 32);
    FileName := _TEMP_FILE_ + ExtractFileExt(Ext);
    TempFile := TFileStream.Create(FileName, fmCreate);
    try
        // Fecha o arquivo
        FreeAndNil(TempFile);
        Icons.ShareImages := True;

        // Pega handle dos icones do windows
        { get Icons handle from windows }
        AHandle := SHGetFileInfo(PChar(FileName),
            0,
            FileInfo,
            sizeof(TSHFileInfo),
            SHGFI_ICON or SHGFI_SYSICONINDEX);
        if AHandle <> 0 then begin
            // Atribui o icone à variavel passada
            Icons.Handle := AHandle;
            Icons.GetIcon(FileInfo.IIcon, Icon);
            Result := True;
        end else begin
            Result := False;
        end;

    finally
        // Apaga o arquivo temporario
        DeleteFile(FileName);
        Icons.Free;
        if (TempFile <> nil) then begin
            TempFile.Free;
        end;
    end;
end;


function GetExeFileType(FileName : string) : TExecutableType;
    //----------------------------------------------------------------------------------------------------------------------------------
    //{Retorna o tipo/plataforma do requerida pelo executavel/atalho
var
    Ret :      DWORD;
    FileInfo : TSHFileInfo;
    PFile :    array[0..1024] of char;
    PPos :     PChar;
begin

    //######### NOTA ##########  Alterar para uso desta funcao com GetBinaryType
    //Esta funcao pode ser melhor implementada com o uso de GetBinaryType
    Result := etError;
    StrPCopy(PFile, UpperCase(FileName));
    //Arquivo de lote/Batch
    PPos := StrPos(PFile, '.BAT');
    if (Abs(Integer(PPos) - Integer(StrEnd(PFile))) = 4) then begin
        Result := etBATCH;
        Exit;
    end;
    //Arquivo de lote/Batch
    PPos := StrPos(PFile, '.PIF');
    if (Abs(Integer(PPos) - Integer(StrEnd(PFile))) = 4) then begin
        Result := etPIF;
        Exit;
    end;
    //Arquivo de lote/Batch
    PPos := StrPos(PFile, '.LNK');
    if (Abs(Integer(PPos) - Integer(StrEnd(PFile))) = 4) then begin
        Result := etLINK;
        Exit;
    end;
    //Demais casos
    FillChar(FileInfo, SizeOf(TSHFileInfo), 0);
    Ret := SHGetFileInfo(PFile, 0, FileInfo, SizeOf(TSHFileInfo), SHGFI_EXETYPE);
    if Ret = 0 then begin //Erro ou tipo indeterminado
        Result := etError;
        Exit;
    end;
    if HiWord(Ret) = 0 then begin //Aplicativo de modo texto/DOS/Console
        if LoWord(Ret) = IMAGE_DOS_SIGNATURE then begin
            Result := etDOS;
        end else begin
            if LoWord(Ret) = IMAGE_NT_SIGNATURE then begin
                Result := etWINCONSOLE;
            end;
        end;
    end else begin //Aplicativo GUI/Windows
        if (HiWord(Ret) >= $300) then begin //Checa se dentro do limite
            if (HiWord(Ret) >= $300) and (HiWord(Ret) < $350) then begin //WIN16
                Result := etWIN16;
            end else begin
                if (HiWord(Ret) >= $350) and (HiWord(Ret) < $400) then begin //NT3?????
                    Result := etWIN32; //Fazer o que? eh 32bits
                end else begin
                    if (HiWord(Ret) >= $400) then begin
                        Result := etWIN32;
                    end;
                end;
            end;
        end else begin
            Result := etError;
        end;
    end;
end;


function ShellCopyDir(hWnd : THandle; const Source, Dest : string; Flags : FILEOP_FLAGS; WinTitle : PChar) : Integer;
    {---------------------------------------------------------------------------------------------}
    {Copia todos os arquivos de uma pasta para outra}
    //Notas: Ver comentario sobre o uso de duplo #0 nos parametros de Origem e destino
var
    FileOpShell : TSHFileOpStruct;
    Src, Dst :    array[0..1024] of char;
    OldDir :      string;
begin
    if WinTitle <> nil then begin
        Flags := Flags + FOF_SIMPLEPROGRESS;
    end;
    if not DirectoryExists(Dest) then begin
        ForceDirectories(Dest);
        if not DirectoryExists(Dest) then begin
            Result := ERROR_CANNOT_MAKE;
            Exit;
        end;
    end;
    StrPCopy(Src, Source + '\*.*');
    StrPCopy(Dst, Dest);
    with FileOpShell do begin
        wFunc  := FO_COPY;
        pFrom  := Src;
        pTo    := Dst;
        fFlags := Flags;
        lpszProgressTitle := WinTitle;
        Wnd    := hWnd;
        hNameMappings := nil;
        fAnyOperationsAborted := False;
    end;
    OldDir := GetCurrentDir;
    ChDir(Dest);
    Result := ShFileOperation(FileOpShell);
    if FileOpShell.fAnyOperationsAborted then begin
        Result := ERROR_CANCELLED;
    end;
    ChDir(OldDir);
end;

function ShellDeleteFiles(hWnd : THandle; const DirName : string; Flags : FILEOP_FLAGS; WinTitle : PChar) : Integer;
    {---------------------------------------------------------------------------------------------}
    {Apaga arquivos/Diretorios atraves do shell do windows}
    //Notas: Ver comentario sobre o uso de duplo #0 nos parametros de Origem e destino
var
    FileOpShell : TSHFileOpStruct;
    Oper : array[0..1024] of char;
begin
    if WinTitle <> nil then begin
        Flags := Flags + FOF_SIMPLEPROGRESS;
    end;
    with FileOpShell do begin
        wFunc  := FO_DELETE;
        pFrom  := Oper;
        pTo    := Oper; //pra garantir a rapadura!
        fFlags := Flags;
        lpszProgressTitle := WinTitle;
        Wnd    := hWnd;
        hNameMappings := nil;
        fAnyOperationsAborted := False;
    end;
    StrPCopy(Oper, DirName);
    StrCat(Oper, PChar(ExtractFileName(FindFirstChildFile(DirName))));
    Result := 0;
    try
        while Oper <> EmptyStr do begin
            Result := ShFileOperation(FileOpShell);
            if FileOpShell.fAnyOperationsAborted then begin
                Result := ERROR_REQUEST_ABORTED;
                break;
            end else begin
                if Result <> 0 then begin
                    Break;
                end;
            end;
            StrPCopy(Oper, FindFirstChildFile(DirName));
        end;
    except
        Result := ERROR_EXCEPTION_IN_SERVICE;
    end;
end;

function ShellDelTree(hWnd : THandle; const Name : string; Flags : FILEOP_FLAGS; WinTitle : PChar) : boolean;
    {---------------------------------------------------------------------------------------------}
    {Apaga arquivos/Diretorios atraves do shell do windows}
    //Notas: Ver comentario sobre o uso de duplo #0 nos parametros de Origem e destino
var
    FileOpShell : TSHFileOpStruct;
    Oper : array[0..1024] of char;
begin
    if WinTitle <> nil then begin
        Flags := Flags + FOF_SIMPLEPROGRESS;
    end;
    StrPCopy(Oper, Name + '\*'); //Sem esta mascara sempre da pau!!!!!!!!
    with FileOpShell do begin
        wFunc  := FO_DELETE;
        pFrom  := Oper;
        pTo    := Oper; //pra garantir a rapadura!
        fFlags := Flags;
        lpszProgressTitle := WinTitle;
        Wnd    := hWnd;
        hNameMappings := nil;
    end;
    ShFileOperation(FileOpShell);
    if (StrCountRepet('\', Name) >= 4) then begin
        //Raiz de unidade de rede(Ver caso netware}
        Result := RemoveDir(Name);
    end else begin
        if ISUNCName(Name) then begin
            Result := True;
        end else begin
            if (StrCountRepet('\', Name) = 1) and (System.Pos('.', Name) <> 0) then begin
                Result := True;
            end else begin
                Result := RemoveDir(Name);
            end;
        end;
    end;
end;

function ShellMoveDir(hWnd : THandle; const Source, Dest : string; Flags : FILEOP_FLAGS; WinTitle : PChar) : Integer;
    {---------------------------------------------------------------------------------------------}
    {Copia todos os arquivos de uma pasta para outra}
    //Notas: Ver comentario sobre o uso de duplo #0 nos parametros de Origem e destino
var
    FileOpShell : TSHFileOpStruct;
    Src, Dst :    array[0..1024] of char;
    OldDir :      string;
begin
    if WinTitle <> nil then begin
        Flags := Flags + FOF_SIMPLEPROGRESS;
    end;
    if not DirectoryExists(Dest) then begin
        ForceDirectories(Dest);
        if not DirectoryExists(Dest) then begin
            Result := ERROR_CANNOT_MAKE;
            Exit;
        end;
    end;
    StrPCopy(Src, Source + '\');
    StrPCopy(Dst, Dest);
    with FileOpShell do begin
        wFunc  := FO_MOVE;
        pFrom  := Src;
        pTo    := Dst;
        fFlags := Flags;
        lpszProgressTitle := WinTitle;
        Wnd    := hWnd;
        hNameMappings := nil;
        fAnyOperationsAborted := False;
    end;
    OldDir := GetCurrentDir;
    ChDir(Dest);
    Result := ShFileOperation(FileOpShell);
    if FileOpShell.fAnyOperationsAborted then begin
        Result := ERROR_CANCELLED;
    end;
    ChDir(OldDir);
end;

function ShellSelectDir(hWnd : THandle; UserMessage : PChar) : string;
    //Seleciona um diretorio pelo uso do shell
    //NOTA: Incompativel com o NT4 segundo a documentacao
    {-------------------------------------------------------------------------------------------------------------}
var
    lpItemID :    PItemIDList;
    BrowseInfo :  TBrowseInfo;
    DisplayName : array[0..MAX_PATH] of char;
    TempPath :    array[0..MAX_PATH] of char;
begin
    //Para maiores detalhes ver a funcao SHGetPathFromIDList e TBrowseInfo
    if hWnd = 0 then begin
        hWnd := GetDeskTopWindow;
    end;
    FillChar(BrowseInfo, sizeof(TBrowseInfo), #0);
    BrowseInfo.hwndOwner      := hWnd;
    BrowseInfo.pszDisplayName := @DisplayName;
    if UserMessage = nil then begin
        BrowseInfo.lpszTitle := PChar('Favor selecione um diretório');
    end else begin
        BrowseInfo.lpszTitle := UserMessage;
    end;
    BrowseInfo.ulFlags := BIF_RETURNONLYFSDIRS;
    lpItemID := SHBrowseForFolder(BrowseInfo);
    if lpItemId <> nil then begin
        SHGetPathFromIDList(lpItemID, TempPath);
        Result := TempPath;
        GlobalFreePtr(lpItemID);
    end else begin
        Result := EmptyStr;
    end;
end;


{ TShellHnd }

class procedure TShellHnd.CreateShellShortCut(const TargetName, LinkFileName, IconFilename: WideString; IconIndex: Integer);
var
    IObject : IUnknown;
    ISLink :  IShellLink;
    IPFile :  IPersistFile;
begin

    //Apaga link anterior com o mesmo nome
    DeleteFile(PWChar(LinkFileName));

    IObject := CreateComObject(CLSID_ShellLink);
    ISLink  := IObject as IShellLink;
    IPFile  := IObject as IPersistFile;

    ISLink.SetPath(PChar(TargetName));
    ISLink.SetWorkingDirectory(PChar(ExtractFilePath(TargetName)));
    if (IconFilename = EmptyStr) then begin
        ISLink.SetIconLocation(PWideChar(TargetName), IconIndex);
	 end else begin
		 ISLink.SetIconLocation(PWideChar(IconFilename), IconIndex);
	 end;
	 IPFile.Save(PWChar(LinkFileName), False);
end;

class function TShellHnd.GetAllUsersDesktop : string;
var
    PIDL :     PItemIDList;
    InFolder : array[0..MAX_PATH] of char;
begin
	 //Localização da pasta do desktop de all users
	 Result:=EmptyStr;
	 SHGetFolderLocation(0, CSIDL_COMMON_DESKTOPDIRECTORY, 0, 0, PIDL);
	 try
		 SHGetPathFromIDList(PIDL, InFolder);
		 Result := InFolder;
	 finally
		 ILFree(PIDL);
	 end;
end;

end.
