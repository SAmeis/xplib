{$IFDEF FileCDlg}
	{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I DlgLib.inc}

unit FileCDlg;

interface

uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons, ComCtrls, Masks, FileHnd,
    FileLogs, TrcCtrls, Contnrs;

type
    TFileOperationEvent = procedure(const PathName : string; var Retry, Abort : boolean) of object;
    TFileCopyEvent = procedure(var SourceFileName, DestFileName : string) of object;

    TFCSimpleGaugeForm = class(TForm)
        PrgBar :    TProgressBar;
        Label1 :    TLabel;
        CancelBtn : TBitBtn;
        OperationLabel : TLabel;
        FileNameLabel : TTruncFileLabel;
        procedure CancelBtnClick(Sender : TObject);
        procedure FormShow(Sender : TObject);
        procedure FormClose(Sender : TObject; var Action : TCloseAction);
    private
        { Private declarations }
    protected
        procedure CreateParams(var Params : TCreateParams); override;
    public
        { Public declarations }
        InProgress : boolean;
        CopyThread : TThread;
        constructor Create(AOnwer : TComponent); override;
    end;

    TSimpleFileCopy = class(TComponent)
    private
        FPrepared : boolean;
        FolderList : TObjectList;   //Lista de entrada para pastas de origem a serem logadas
        TotalBytes : int64;         //Total de bytes logados
        CopiedBytes : int64;        //Total de bytes copiados no momento
        Dlg :      TFCSimpleGaugeForm;
        FRootDestDir : string;
        FOnDestFolderFail : TFileOperationEvent;
        FOnSourceOpenFail : TFileOperationEvent;
        FOnDestExists : TFileOperationEvent;
        FOnDestCreateFail : TFileOperationEvent;
        FCaption : string;
        FErrorLog : TStringList;
        FOnBeforeCopy : TFileCopyEvent;
        FOnAfterCopy : TFileCopyEvent;
        FCanCancel : boolean;
        FRetryTimeout: integer;
        FRetriesCount: integer;
    protected
        LogSList : TStringList;         //Lista de origem logadas
        LogDList : TStringList;         //Lista de destino logadas
        procedure ProcessFileN(FileIndex : integer);
        procedure LogFiles();
        procedure CopyFiles();
        procedure Prepare();
    public
        property ErrorLog : TStringList read FErrorLog;
        property Prepared : boolean read FPrepared;
        constructor Create(AOwner : TComponent); override;
        destructor Destroy; override;
        function AddDir(const SourceDir : string; const IncludeFileMasks, ExcludeFileMasks : array of string;
            IncSubDir : boolean) : integer;
        function Execute : integer;
        procedure Reset;  //Limpa listas internas para nova operacao
        procedure UpdateGauge;
        procedure SumarizeFiles;
    published
        property CanCancel : boolean read FCanCancel write FCanCancel stored True;
        property Caption : string read FCaption write FCaption;
        property OnAfterCopy : TFileCopyEvent read FOnAfterCopy write FOnAfterCopy;
        property OnBeforeCopy : TFileCopyEvent read FOnBeforeCopy write FOnBeforeCopy;
        property OnDestExists : TFileOperationEvent read FOnDestExists write FOnDestExists;
        property OnDestCreateFail : TFileOperationEvent read FOnDestCreateFail write FOnDestCreateFail;
        property OnDestFolderFail : TFileOperationEvent read FOnDestFolderFail write FOnDestFolderFail;
        property OnSourceOpenFail : TFileOperationEvent read FOnSourceOpenFail write FOnSourceOpenFail;
        property RetriesCount : integer read FRetriesCount write FRetriesCount default 1;
        property RetryTimeout : integer read FRetryTimeout write FRetryTimeout default 250;
        property RootDestDir : string read FRootDestDir write FRootDestDir;
    end;

implementation

{$R *.DFM}

uses
    Math;

const
    _BLOCK_SIZE_ = 4096;


type
    //Thread que controla processo time lengthy
    TExecuteThread = class(TThread)
    private
    protected
        DlgForm :      TFCSimpleGaugeForm;
        FileCopyComp : TSimpleFileCopy;
        procedure Execute; override;
    public
        constructor Create(CreateSuspended : boolean);
    end;

constructor TFCSimpleGaugeForm.Create(AOnwer : TComponent);
    //----------------------------------------------------------------------------------------------------------------------
begin
    inherited;
    InProgress := False;
end;

procedure TFCSimpleGaugeForm.CancelBtnClick(Sender : TObject);
//----------------------------------------------------------------------------------------------------------------------
begin
    Self.InProgress := False;
    Self.CopyThread.Terminate();
    Self.Close;
    Self.Refresh();
    SetLastError(ERROR_CANCELLED);
end;

procedure TSimpleFileCopy.LogFiles();
//----------------------------------------------------------------------------------------------------------------------
var
    j, i, SrcPrefixLen : integer;
    FLst :   TStringList;
    DirLog : TDirLogBasic;
begin
    FLst := TStringList.Create;
    try
        //Varre lista de entradas para montar listas  de origem/destino
        for i := 0 to Self.FolderList.Count - 1 do begin
            DirLog := TDirLogBasic(Self.FolderList.Items[i]);
            DirLog.LogFileSizes(FLst);
            SrcPrefixLen := Length(DirLog.DirName);
            for j := 0 to FLst.Count - 1 do begin
                Self.LogSList.AddObject(FLst.Strings[j], FLst.Objects[j]);
                Self.LogDList.Add(
                    TFileHnd.ConcatPath(
                    [Self.RootDestDir, System.Copy(FLst.Strings[j], SrcPrefixLen + 1, Length(FLst.Strings[j]))])
                    );
            end;
        end;
    finally
        FLst.Free;
    end;
    Self.FPrepared := True;
end;

procedure TSimpleFileCopy.ProcessFileN(FileIndex : integer);
//----------------------------------------------------------------------------------------------------------------------

function LSRCreateFileStream( const Filename : string; OpenMode : Word ) : TFileStream;
{{
Tenta abrir o FileStream no modo desejado, caso  algum erro ocorra tentar de acordo com o especificado por RetriesCount e
RetryTimeout desta instancia

Revision: 13/1/2006 - Roger
}
var
    RetCount : integer;
begin
    Result:=nil;
    RetCount:=Max( 0, Self.FRetriesCount );
    while ( ( Result = nil )  ) do begin
        try
            Result:=TFileStream.Create( Filename, OpenMode );
        except
            on E : Exception do begin
                Result:=nil;
                if (  RetCount > 0  ) then begin
                    Dec( RetCount );
                    if ( Self.FRetryTimeout > 0 ) then begin
                         Sleep( Self.FRetryTimeout );
                    end;
                end else begin
                    raise;
                end;
            end;
        end;
    end;
end;


const
    _ABORT_MSG_ = 'Operação cancelada pelo usuário';
var
    ReadStep, PrePos : int64;
    InStrm, OutStrm :  TFileStream;
    SFName, DFName :   string;
    RetryFile, RetryDir, AbortDir, AbortFile, DestinationExists : boolean;
    OldSize, NewSize, SourceDate, SourceAttribs : integer;
begin
{$WARN SYMBOL_PLATFORM OFF}
    //Sossega a bacurinha do compilador com bobagens
    SourceDate    := 0;
    SourceAttribs := 0;

    OutStrm := nil;
    InStrm  := nil;
    PrePos  := Self.CopiedBytes;
    try
        SFName := Self.LogSList.Strings[FileIndex];  //Source File
        DFName := Self.LogDList.Strings[FileIndex];  //Dest File
        if Assigned(Self.FOnBeforeCopy) then begin
            OldSize := DWORD(Self.LogSList.Objects[FileIndex]); //Salva tamanho anterior do arquivo
            Self.FOnBeforeCopy(SFName, DFName);
            NewSize := TFileHnd.FileSize(SFName);    //Toma tamanho corrente da origem
            if NewSize <> OldSize then begin   //Atualiza progresso para o novo valor
                Self.TotalBytes := Self.TotalBytes + (NewSize - OldSize);
                Self.LogSList.Objects[FileIndex] := TObject(NewSize);
            end;
        end;

        //Tenta Abrir origem
        RetryFile := True;
        while RetryFile do begin
            RetryFile := False;
            if FileExists(SFName) then begin
                try
                    InStrm     := LSRCreateFileStream(SFName, fmShareDenyWrite);
                    SourceDate := FileGetDate(InStrm.Handle);
                    SourceAttribs := FileGetAttr(SFName);
                except
                    on E : Exception do begin
                        if Assigned(Self.FOnSourceOpenFail) then begin
                            Self.FOnSourceOpenFail(SFName, RetryFile, AbortFile);
                            if AbortFile then begin
                                SetLastError(ERROR_CANCELLED);
                                raise Exception.Create(_ABORT_MSG_);
                            end;
                        end else begin
                            Self.ErrorLog.Add(Format('Arquivo %s não pode ser acessado corretamente.'#13'%s',
                                [SFName, E.Message]));
                            Exit;
                        end;
                    end;
                end;
            end else begin
                //Alimentar ErrorLog
                Self.ErrorLog.Add(Format('Arquivo %s não encontrado', [SFName]));
                Exit;
            end;
        end;
        if not Assigned(InStrm) then begin //Passando daqui -> Erro nao tratado que sera ignorado
            Exit;
            Self.ErrorLog.Add(Format('Impossível abrir arquivo %s'#13'%s', [SFName, SysErrorMessage(GetLastError)]));
        end;

        //Checa caminho de destino
        RetryDir := True;
        while RetryDir do begin
            RetryDir := False;
            if not ForceDirectories(ExtractFilePath(DFName)) then begin //Forcar o caminho
                if Assigned(Self.FOnDestFolderFail) then begin
                    Self.FOnDestFolderFail(ExtractFilePath(DFName), RetryDir, AbortDir);
                    if AbortDir then begin
                        SetLastError(ERROR_CANCELLED);
                        raise Exception.Create(_ABORT_MSG_);
                    end;
                end else begin
                    Self.ErrorLog.Add(Format('Impossível criar caminho %s'#13'%s', [ExtractFilePath(DFName),
                        SysErrorMessage(GetLastError)]));
                end;
            end;
        end;

        //Checa choque nome arquivo destino -> Padrao sera sobreescrevr de toda a forma
        RetryFile := True;
        DestinationExists := False;
        while RetryFile do begin  //Choque no nome de destino
            RetryFile := False;
            if FileExists(DFName) then begin
                DestinationExists := True;
                if (Assigned(Self.FOnDestExists)) then begin
                    Self.FOnDestExists(DFName, RetryFile, AbortFile); //Dispara evento e pega resultado de retorno
                    if AbortFile then begin
                        SetLastError(ERROR_CANCELLED);
                        raise Exception.Create(_ABORT_MSG_);
                    end;
                end;
            end;
        end;

        //Cria arquivo de destino
        RetryFile := True;
        while RetryFile do begin
            RetryFile := False;
            try
                if (DestinationExists) then begin
                    OutStrm := LSRCreateFileStream(DFName, fmOpenWrite);
                    OutStrm.Position := 0;
                end else begin
                    OutStrm := LSRCreateFileStream(DFName, fmCreate);
                end;
            except
                on E : Exception do begin
                    if Assigned(Self.FOnDestCreateFail) then begin
                        Self.FOnDestCreateFail(DFName, RetryFile, AbortFile);
                        if AbortFile then begin
                            SetLastError(ERROR_CANCELLED);
                            raise Exception.Create(_ABORT_MSG_);
                        end;
                    end else begin
                        Self.ErrorLog.Add(Format('Arquivo de destino %s não pode ser acessado corretamente.'#13'%s',
                            [DFName, E.Message]));
                        Exit;
                    end;
                end;
            end;
        end;
        if not Assigned(OutStrm) then begin
            Self.ErrorLog.Add(Format('Impossível gravar destino: %s'#13'%s', [DFName, SysErrorMessage(GetLastError)]));
            Exit;
        end;

        //Copiar arquivos em blocos, atualizar gauge
        Self.Dlg.FileNameLabel.Caption := DFName;
        try
            try
                while (InStrm.Position < InStrm.Size) and (Self.Dlg.InProgress) do begin
                    Application.ProcessMessages();
                    ReadStep := OutStrm.CopyFrom(InStrm, Min(_BLOCK_SIZE_, InStrm.Size - InStrm.Position));
                    Inc(Self.CopiedBytes, ReadStep);
                    if not Self.Dlg.InProgress then begin
                        SysUtils.Abort();
                    end;
                    Self.UpdateGauge();
                end;
                //Seta data do arquivo de destino
                FileSetDate(OutStrm.Handle, SourceDate);
            finally
                InStrm.Free;
                OutStrm.Free;
            end;
            //Seta atributos do arquivo de destino
            FileSetAttr(DFName, SourceAttribs);
        except
            on E : Exception do begin
                if FileExists(DFName) then begin //Elimina restos de arquivo incompleto
                    Self.FErrorLog.Add('Removendo arquivo falho: ' + DFName);
                    DeleteFile(DFName);
                end;
                if E is EAbort then begin
                    SetLastError(ERROR_CANCELLED);
                end;
            end;
        end;

        { TODO -oRoger -cLIB : Repor atributos de segurança Criar propriedades para configurar isso }

        //Chama evento pos-copia
        if Assigned(Self.FOnAfterCopy) then begin
            Self.FOnAfterCopy(SFName, DFName);
        end;
    finally
        //Garantir que o valor final sera dado pelo valor logado
        Self.CopiedBytes := PrePos + DWORD(Self.LogSList.Objects[FileIndex]);
        Self.Dlg.FileNameLabel.Caption := EmptyStr;
        Self.UpdateGauge();
        Application.ProcessMessages();
    end;
{$WARN SYMBOL_PLATFORM OFF}
end;

procedure TSimpleFileCopy.Prepare;
//----------------------------------------------------------------------------------------------------------------------
begin
    Self.Dlg.OperationLabel.Caption := 'Lendo arquivos. Favor, aguarde.....';
    Self.Dlg.OperationLabel.Refresh;
    Self.LogFiles();
    SetLastError(0);
    Self.Dlg.OperationLabel.Caption := 'Copiando arquivos...';
end;

{ TExecuteThread }

constructor TExecuteThread.Create(CreateSuspended : boolean);
    //----------------------------------------------------------------------------------------------------------------------
begin
    inherited Create(CreateSuspended);
    Self.DlgForm := nil;
end;

procedure TExecuteThread.Execute;
//----------------------------------------------------------------------------------------------------------------------
begin
    try
        Synchronize(Self.FileCopyComp.Prepare);           //Loga as pastas
        Synchronize(Self.FileCopyComp.SumarizeFiles);     //Calcula total dos arquivos
        Synchronize(Self.FileCopyComp.CopyFiles);         //Inicia a copia real
    finally
        PostMessage(Self.DlgForm.Handle, WM_CLOSE, 0, 0); //Envia mensagem para fechar dialogo
    end;
end;

{ TSimpleFileCopy }

constructor TSimpleFileCopy.Create(AOwner : TComponent);
    //----------------------------------------------------------------------------------------------------------------------
begin
    inherited;
    Self.CanCancel  := True;
    Self.FPrepared  := False;
    Self.FErrorLog  := TStringList.Create;
    Self.LogDList   := TStringList.Create;
    Self.LogSList   := TStringList.Create;
    Self.FolderList := TObjectList.Create;
end;

destructor TSimpleFileCopy.Destroy;
    //----------------------------------------------------------------------------------------------------------------------
begin
    if Assigned(Self.Dlg) then begin
        Self.Dlg.Free;
    end;
    Self.FErrorLog.Free;
    Self.LogSList.Free;
    Self.LogDList.Free;
    Self.FolderList.Free; //Limpa lista da pastas para copia
    inherited;
end;

procedure TFCSimpleGaugeForm.FormShow(Sender : TObject);
//----------------------------------------------------------------------------------------------------------------------
begin
    Self.InProgress := True; //Libera Thread Execution
    Self.CopyThread.Resume();
end;

procedure TSimpleFileCopy.CopyFiles;
//----------------------------------------------------------------------------------------------------------------------
var
    i : integer;
begin
    for i := 0 to Self.LogSList.Count - 1 do begin
        Self.ProcessFileN(i);
        if not Self.Dlg.InProgress then begin
            Exit;
        end;
    end;
end;

procedure TFCSimpleGaugeForm.FormClose(Sender : TObject; var Action : TCloseAction);
//----------------------------------------------------------------------------------------------------------------------
begin
    if Assigned(Self.CopyThread) then begin
        Self.CopyThread.Terminate();
    end;
end;

function TSimpleFileCopy.Execute : integer;
    //----------------------------------------------------------------------------------------------------------------------
var
    ParalellThread : TExecuteThread;
begin
    //Cria dialogo
    if not Assigned(Self.Dlg) then begin
        Self.Dlg := TFCSimpleGaugeForm.Create(Self);
    end;

    Self.Dlg.Caption := Self.FCaption;
    Self.Dlg.CancelBtn.Visible := Self.CanCancel;
    if Self.CanCancel then begin
        Self.Dlg.BorderIcons := Self.Dlg.BorderIcons - [biSystemMenu];
    end else begin
        Self.Dlg.BorderIcons := Self.Dlg.BorderIcons + [biSystemMenu];
    end;

    ParalellThread := TExecuteThread.Create(True);
    try
        ParalellThread.DlgForm   := Self.Dlg;
        ParalellThread.FileCopyComp := Self;
        //ParalellThread.OnTerminate:=Self.Dlg.TerminateDlg;
        Self.Dlg.CopyThread      := ParalellThread;
        Self.Dlg.PrgBar.Position := 0;
        Self.Dlg.OperationLabel.Caption := EmptyStr;
        Self.Dlg.FileNameLabel.Caption := EmptyStr;
        Self.Dlg.ShowModal();
        Result := GetLastError();
    finally
        ParalellThread.Terminate;
        ParalellThread.Free;
        FreeAndNil(Self.Dlg);
    end;
end;

procedure TSimpleFileCopy.Reset;
begin
    Self.TotalBytes  := 0;
    Self.CopiedBytes := 0;
    Self.FolderList.Clear();
    Self.LogSList.Clear();
    Self.LogDList.Clear();
    if Assigned(Self.Dlg) then begin
        FreeAndNil(Self.Dlg);
    end;
end;

procedure TSimpleFileCopy.SumarizeFiles;
var
    i : integer;
begin
    Self.TotalBytes := 0;
    for i := 0 to Self.LogSList.Count - 1 do begin
        Inc(Self.TotalBytes, DWORD(Self.LogSList.Objects[i]));
    end;
end;

procedure TSimpleFileCopy.UpdateGauge;
//----------------------------------------------------------------------------------------------------------------------
{{
Calculo da posicao do gauge
}
var
    R : extended;
begin
    R := (Self.CopiedBytes / (Self.TotalBytes + 1)) * 100;
    if Assigned(Self.Dlg) then begin
        Self.Dlg.PrgBar.Position := Trunc(R);
    end;
end;

procedure TFCSimpleGaugeForm.CreateParams(var Params : TCreateParams);
//----------------------------------------------------------------------------------------------------------------------
var
    ParentForm : TForm;
begin
    ParentForm := Screen.ActiveForm;
    if Assigned(ParentForm) then begin
        Params.WndParent := ParentForm.Handle;
    end;
    inherited;
end;

function TSimpleFileCopy.AddDir(const SourceDir : string; const IncludeFileMasks, ExcludeFileMasks : array of string;
    IncSubDir : boolean) : integer;
{{
armazena nova entrada na lista de pastas
}
var
    LogDir : TDirLogBasic;
begin
    LogDir := TDirLogBasic.Create(SourceDir, IncludeFileMasks, ExcludeFileMasks, IncSubDir);
    Result := Self.FolderList.Add(LogDir);
end;

end.
