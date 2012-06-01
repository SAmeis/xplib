{$IFDEF AppLog}
    {$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I XPLib.inc}

unit AppLog;

{{
Implementa os tipos básicos para log de aplicativos.
}
{1 Implementa os tipos básicos para log de aplicativos. }

interface

uses
    SysUtils, Classes, Types;

const
    DBGLEVEL_ULTIMATE   = 10;
    DBGLEVEL_DETAILED   = 5;
    DBGLEVEL_ALERT_ONLY = 1;
    DBGLEVEL_NONE       = 0;

type
    TLogMessageType = (lmtError, lmtInformation, lmtWarning, lmtDebug);

type
    TErrorLogMode      = (elmFormatted, elmUnFormatted);
    TFormatMessageProc = function(const Msg : string; LogMessageType : TLogMessageType = lmtError) : string;

type
    IFormatter = interface(IInterface)
        ['{D8AAABBB-6EE6-4AD9-B921-7138692CFB36}']
 {{
 Interface a ser implementada pelos formatadores de log
 }
        function FormatLogMsg(const LogMsg : string; LogMessageType : TLogMessageType = lmtError) : string;
    end;

    TLogFormatter = class(TInterfacedObject, IFormatter)
 {{
 Formatador padrão inciado com a carga do pacote, para o caso de uso de outro carregá-lo mediante alteracao da propriedade
 Formatter da
 instância do TLogFile em vigor.
 Ver TLogFile.Formatter
 
 Nota: Este formatador particulamente desagrada e esta livre para modificações, assim sua documentação está em aberto
 }
    public
        function FormatLogMsg(const LogMsg : string; LogMessageType : TLogMessageType = lmtError) : string; virtual;
    end;

    TLogSingleLineFormatter = class(TLogFormatter)
    {{
    Formatador de única linha.
    }
    public
        function FormatLogMsg(const LogMsg : string; LogMessageType : TLogMessageType = lmtError) : string; override;
    end;

type
    TLogFile = class(TObject)
     {{
     Classe usada como base para registro de logs de aplicativos.
     }
    private
        FFileName :   string;
        FFormatter :  IFormatter;
        FLocked :     boolean;
        FOpenMode :   Word;
        FDebugLevel : Integer;
        function GetBuffered : boolean;
        procedure SetBuffered(const Value : boolean);
        function GetBufferText : string;
        procedure SetFileName(const Value : string);
        function GetSize : int64;
    protected
        FLogBuffer : TStringList;
        FStream :    TStream;
		 procedure StreamDispose; virtual;
		 procedure StreamNeeded; virtual;
		 procedure WriteTo(const Txt : string);
	 public
        constructor Create(const AFileName : string; Lock : boolean); virtual;
        destructor Destroy; override;
        procedure Commit;
        function CopyToStream(InitPos, Count : int64; DestStream : TStream) : int64;
        procedure Discard;
        class function GetDefaultLogFile : TLogFile;
        class procedure Initialize; virtual;
        class procedure Log(const Msg : string; LogMessageType : TLogMessageType = lmtError);
        class procedure LogDebug(const Msg : string; MinDebugLevel : Integer);
        class procedure SetDefaultLogFile(LogFile : TLogFile);
        procedure WriteLog(const Msg : string; LogMessageType : TLogMessageType = lmtError); virtual;
        property Buffered : boolean read GetBuffered write SetBuffered;
        {{
        Flag que sinaliza se as entradas registradas por este logger serão ou não descarregadas diretamente para o streamer de saida
        }
        {1 Flag que sinaliza se as entradas registradas por este logger serão ou não descarregadas diretamente para o streamer de saida }
        property BufferText : string read GetBufferText;
        {{
        Somente leitura: Conteudo armazenado no buffer deste logger
        }
        {1 Somente leitura: Conteudo armazenado no buffer deste logger }
        property FileName : string read FFileName write SetFileName;
        {{
        Nome do arquivo usado para se obter o streamer de saída do log.
        }
        {1 Nome do arquivo usado para se obter o streamer de saída do log. }
        property Formatter : IFormatter read FFormatter write FFormatter;
        {1 Referência ao formatador desta instância, se nulo a rotina padrao será usada ( esta sempre válida ) }
        property Locked : boolean read FLocked;
        {{
        Flag de travamento do streamer. Com ela ativa outros processos não podem ter acesso ao destino do log.
        }
        {1 Flag de travamento do streamer. Com ela ativa outros processos não podem ter acesso ao destino do log. }
        property OpenMode : Word read FOpenMode write FOpenMode;
        {{
        Modo de abertura. Valem os valores para o construtor de TFileStream.
        }
        {1 Modo de abertura. Valem os valores para o construtor de TFileStream. }
        property Size : int64 read GetSize;
         {{
         Tamanho corrente do Streamer do log.
         }
        {1 Tamanho corrente do Streamer do log. }
        property DebugLevel : Integer read FDebugLevel write FDebugLevel;
         {{
         Nível de detalhamento para as mensagens de depuração. Usado em conjunção com o método
         TLogFile.LogDebug(Msg : string; CurrentDbgLevel : integer).
         }
        {1 Tamanho corrente do Streamer do log. }
    end;

type
    ELoggedException = class(Exception)
    {{
    Ancestral comum a todas as excessões logadas automaticamente.

    Todas as excessões descendentes serão logadas no seu construtor seguindo as regras globais vingentes em AppLog.
    }
    private
        procedure WriteLog(const Msg : string);
    protected
        function FormatLogMessage(const ErrorMsg : string) : string; virtual;
    public
        constructor Create(const Msg : string); virtual;
        constructor CreateFmt(const Msg : string; const Args : array of const);
        constructor CreateFmtHelp(const Msg : string; const Args : array of const; AHelpContext : Integer);
        constructor CreateHelp(const Msg : string; AHelpContext : Integer);
        constructor CreateRes(Ident : Integer); overload;
        constructor CreateRes(ResStringRec : PResStringRec); overload;
        constructor CreateResFmt(Ident : Integer; const Args : array of const); overload;
        constructor CreateResFmt(ResStringRec : PResStringRec; const Args : array of const); overload;
        constructor CreateResFmtHelp(Ident : Integer; const Args : array of const; AHelpContext : Integer); overload;
        constructor CreateResFmtHelp(ResStringRec : PResStringRec; const Args : array of const; AHelpContext : Integer); overload;
        constructor CreateResHelp(Ident : Integer; AHelpContext : Integer); overload;
        constructor CreateResHelp(ResStringRec : PResStringRec; AHelpContext : Integer); overload;
    end;


procedure AppFatalError(const Msg : string; ExitCode : Integer = 1; InteractiveApp : boolean = True);
procedure CloseErrorLogBuffer(); deprecated;
function FormatErrorLogMsg(const ErrorMsg : string; LogMessageType : TLogMessageType = lmtError) : string;
procedure FlushErrorLogBuffer(); deprecated;
procedure OpenErrorLogBuffer(); deprecated;
procedure RaiseWin32Error(ErrorCode : DWORD); deprecated;
procedure ReadErrorLogBuffer(BufferCopy : TStringList); deprecated;
function WriteErrorLogMsg(const ErrorMsg : string) : boolean; deprecated;
function WriteErrorLogMsgFmt(const ErrorMsg : string; Params : array of const) : boolean; deprecated;
function WriteLogMsg(const LogMsg : string; LogMessageType : TLogMessageType = lmtError) : boolean; deprecated;
function FormatLogMsg(const LogMsg : string; LogMessageType : TLogMessageType = lmtError) : string;
procedure SetLogFileName(const LogFileName : string); deprecated;
function WriteSingleLineLogMsg(const LogMsg : string; LogMessageType : TLogMessageType = lmtError) : boolean; deprecated;
function FormatSimpleLineLogMsg(const LogMsg : string; LogMessageType : TLogMessageType = lmtError) : string;



var
    ErrorLogMode :      TErrorLogMode = elmFormatted;
    FormatMessageProc : TFormatMessageProc = FormatLogMsg;
    LogUserName :       string;
    LogBuffered :       boolean = False;

implementation

uses
    FileHnd, Windows, SysConst;

const
    DEFAULT_APP_LOG_EXTENSION = '.log';

var
    GlobalLogFileName : string = ''; //Na inicializacao assume variacao do nome do aplicativo,
    LogBuffer : TStringList = nil;
    LogCriticalSection : TRtlCriticalSection;
    GlobalDefaultLogFile : TLogFile = nil;


procedure SetLogFileName(const LogFileName : string);
{{
Deprecated : Use TLogFile.GetDefaultLogFile.FileName:=LogFileName;

Rev. 19/5/2005
}
begin
    TLogFile.GetDefaultLogFile.FileName := LogFileName;
end;


procedure AppFatalError(const Msg : string; ExitCode : Integer = 1; InteractiveApp : boolean = True);
{{
Termina o aplicativo setando o ExitCode dado, e repassando a mensagem ao usuário inicialmente. Registra esta mesma mensagem no log do aplicativo.

Notas: CUIDADO não existe tratamento para aplicações consoloe e/ou servicos. Este método chama MessageDlg() no momento se, tratar os requisitos citados

Rev. 19/5/2005
}
begin
    TLogFile.Log('ERRO FATAL:'#13 + Msg, lmtError);
    //Alerta e impede processamento posterior se indicado que aplicativo roda no modo interativo
    if (InteractiveApp) then begin
        Windows.MessageBox(GetActiveWindow(), PChar(Msg), 'Erro fatal', MB_ICONERROR + MB_OK + MB_TASKMODAL);
    end;
    Halt(ExitCode);
end;

procedure CloseErrorLogBuffer();
{{
Fecha o buffer de erros do aplicativo. Depois desta chamada os erros registrados ( inclusive pendentes ) serão diretamente repassados ao arquivo de registro.

Comportamento deste método deve ser acessado pelos atributos coletados em GetDefaultLogFile

Rev. 19/5/2005
}
begin
    EnterCriticalSection(LogCriticalSection);
    try
        TLogFile.GetDefaultLogFile.Commit;
        TLogFile.GetDefaultLogFile.Buffered := False;
    finally
        LeaveCriticalSection(LogCriticalSection);
    end;
end;

function FormatErrorLogMsg(const ErrorMsg : string; LogMessageType : TLogMessageType = lmtError) : string;
{{
Rotina padrao de formatacao de messagens de erro para arquivo de LOG

Rev. 19/5/2005
}
var
    i :  Integer;
    SL : TStringList;
begin
    SL := TStringList.Create;
    try
        //Identa o texto
        SL.Text := ErrorMsg;
        for i := 0 to SL.Count - 1 do begin
            SL.Strings[i] := #9 + SL.Strings[i];
        end;
        SL.Insert(0, Format('-Registro de erro: (%s) ', [LogUserName]) +
            FormatDateTime('dddd, mmmm d, yyyy, " às " hh:mm:ss', Now()));
        Result := SL.Text; //Usa var para adcionar Separadores
    finally
        SL.Free;
    end;
end;

function FormatLogMsg(const LogMsg : string; LogMessageType : TLogMessageType = lmtError) : string;
{{
Rotina padrao de formatacao de messagens de erro. Recebe  LogMessageType indicando o tipo de erro

Rev. 19/5/2005
}
var
    i :  Integer;
    SL : TStringList;
begin
    SL := TStringList.Create;
    try
        //Identa o texto
        SL.Text := LogMsg;
        for i := 0 to SL.Count - 1 do begin
            SL.Strings[i] := #9 + SL.Strings[i];
        end;
        if LogMessageType = lmtError then begin
            SL.Insert(0, Format('-Registro de erro: (%s) ', [LogUserName]) +
                FormatDateTime('dddd, mmmm d, yyyy, " às " hh:mm:ss', Now()));
        end else begin
            if LogMessageType = lmtInformation then begin
                SL.Insert(0, Format('-Registro de Informação: (%s) ', [LogUserName]) +
                    FormatDateTime('dddd, mmmm d, yyyy, " às " hh:mm:ss', Now()));
            end else begin
                if LogMessageType = lmtWarning then begin
                    SL.Insert(0, Format('-Registro de Alerta: (%s) ', [LogUserName]) +
                        FormatDateTime('dddd, mmmm d, yyyy, " às " hh:mm:ss', Now()));
                end else begin
                    if LogMessageType = lmtDebug then begin
                        SL.Insert(0, Format('-Registro de Debug: (%s) ', [LogUserName]) +
                            FormatDateTime('dddd, mmmm d, yyyy, " às " hh:mm:ss', Now()));
                    end;
                end;
            end;
        end;
        Result := SL.Text; //Usa var para adcionar Separadores
    finally
        SL.Free;
    end;
end;

function WriteLogMsg(const LogMsg : string; LogMessageType : TLogMessageType = lmtError) : boolean;
{{
Salva LogMsg no arquivo de log padrao do aplicativo

Deprecated : Use TLogFile.Log( LogMsg, LogMessageType );

Rev. 19/5/2005
}
begin
    Result := True;
    try
        TLogFile.Log(LogMsg, LogMessageType);
    except
        Result := False;
    end;
end;


function WriteSingleLineLogMsg(const LogMsg : string; LogMessageType : TLogMessageType = lmtError) : boolean;
{{
Salva LogMsg no arquivo de log padrao do aplicativo, em uma linha

Deprecated Usar para instancia do logger algo do tipo:

TLogFile.GetDefaultLogFile.Formatter:=TLogSingleLineFormatter.Create();

Rev. 19/5/2005
{
var
    F : TFileStream;
    LocalLogMsg : string;
    SB : TStringList;
    FileName : string;
}
var
    OldFormatProc : TFormatMessageProc;
begin
    OldFormatProc     := FormatMessageProc;
    FormatMessageProc := FormatSimpleLineLogMsg;
    try
        TLogFile.Log(LogMsg, LogMessageType);
    finally
        FormatMessageProc := OldFormatProc;
    end;
    Result := True;
{
    Result := FALSE;
    try
        FileName := GlobalLogFileName;
        EnterCriticalSection(LogCriticalSection);
        try
            if not FileExists(FileName) then begin
                F := TFileStream.Create(FileName, fmCreate);
            end else begin
                F := TFileStream.Create(FileName, fmOpenReadWrite + fmShareDenyNone);
                F.Seek(0, soFromEnd); //Fim do streamer
            end;
            try
                LocalLogMsg := FormatSimpleLineLogMsg(LogMsg, LogMessageType);
                if LogBuffered then begin
                    SB := TStringList.Create();
                    try
                        SB.Text := LocalLogMsg;
                        LogBuffer.AddStrings(SB);
                    finally
                        SB.Free;
                    end;
                end else begin
                    F.WriteBuffer(PChar(LocalLogMsg)^, Length(LocalLogMsg));
                end;
                Result := TRUE;
            finally
                F.Free;
            end;
        finally
            LeaveCriticalSection(LogCriticalSection);
        end;
    except
        on E    : EConvertError do begin
            //Ignora registro de log
        end else begin
            if ExceptObject is Exception then begin
                //Podemos ter varias causas inclusive a negacao de escrita no arquivo em questao
                //Ignora do mesmo jeito, estudar esta forma para melhorar formatacao
            end;
        end;
    end;
}
end;

function FormatSimpleLineLogMsg(const LogMsg : string; LogMessageType : TLogMessageType = lmtError) : string;
{{
Rotina padrao de formatacao de messagens de erro em uma linha. Recebe  LogMessageType indicando o tipo de erro

Rev. 19/5/2005
}
begin
    if LogMessageType = lmtError then begin
        Result := Format('Erro: (%s) ', [LogUserName]) + FormatDateTime('dd/mm/yyyy hh:nn:ss', Now()) + ' - ' + LogMsg + #13#10;
    end else begin
        if LogMessageType = lmtInformation then begin
            Result := Format('Informação: (%s) ', [LogUserName]) + FormatDateTime('dd/mm/yyyy hh:nn:ss', Now()) +
                ' - ' + LogMsg + #13#10;
        end else begin
            if LogMessageType = lmtWarning then begin
                Result := Format('Alerta: (%s) ', [LogUserName]) + FormatDateTime('dd/mm/yyyy hh:nn:ss', Now()) +
                    ' - ' + LogMsg + #13#10;
            end else begin
                if LogMessageType = lmtDebug then begin
                    Result := Format('Debug: (%s) ', [LogUserName]) + FormatDateTime('dd/mm/yyyy hh:nn:ss', Now()) +
                        ' - ' + LogMsg + #13#10;
                end;
            end;
        end;
    end;
end;


procedure FlushErrorLogBuffer();
{{
Descarrega conteudo do buffer de erro do aplicativo, mas o mantem aberto

Deprecated  :Use TLogFile.GetDefaultLogFile.Commit();

Rev. 19/5/2005
}
begin
    TLogFile.GetDefaultLogFile.Commit();
end;

function GetWindowsUserName : string;
{{
Rotina sem sentido nesta unit, serve apenas para suporte a outras da mesma forma deprecated use a mesma em WinNetHnd
Retorna o nome do usuário logado no computador, existe duvida se na realidade isso se aplica ao thread corrente

Rev. 19/5/2005
}
var
    Buf : array[0..2 * MAX_COMPUTERNAME_LENGTH + 1] of char;
    Len : cardinal;
begin
    Len := High(Buf);
    if Windows.GetUserName(Buf, Len) then begin
        Result := string(Buf);
    end else begin
        Result := EmptyStr;
    end;
end;


procedure OpenErrorLogBuffer();
{{
Abre o Buffer de erro do aplicativo.

Deprecated : Use TLogFile.GetDefaultLogFile.Buffered:=True;

Rev. 19/5/2005
}
begin
    TLogFile.GetDefaultLogFile.Buffered := True;
end;

procedure RaiseWin32Error(ErrorCode : DWORD);
{{
Eleva uma EOSError, a menos que ErrorCode = ERROR_SUCCESS. Por exemplo ErrorCode = 3 -> Acesso negado

Deprecated : Use APIHnd.CheckApi()

Rev. 19/5/2005
}
var
    Error : EOSError;
begin
    {$TYPEDADDRESS OFF}
    if ErrorCode <> ERROR_SUCCESS then begin
        Error := EOSError.CreateResFmt(@SOSError, [ErrorCode, SysErrorMessage(ErrorCode)]);
        Error.ErrorCode := ErrorCode;
    end else begin
        Error := EOSError.CreateRes(@SUnkOSError);
    end;
    raise Error;
    {$TYPEDADDRESS ON}
end;

procedure ReadErrorLogBuffer(BufferCopy : TStringList);
{{
Carrega para BufferCopy o conteudo do buffer de erro do aplicativo

Deprecated : Use BufferCopy.Text:=TLogFile.GetDefaultLogFile.BufferText;

Rev. 19/5/2005
}
begin
    BufferCopy.Text := TLogFile.GetDefaultLogFile.BufferText;
end;

function WriteErrorLogMsg(const ErrorMsg : string) : boolean;
{{
Salva ErrorMsg log do aplicativo como do tipo erro

Deprecated : Use TLogFile.Log( ErrorMsg, lmtError );

Rev. 19/5/2005
}
begin
    Result := True;
    try
        TLogFile.Log(ErrorMsg, lmtError);
    except
        Result := False;
    end;
end;

function WriteErrorLogMsgFmt(const ErrorMsg : string; Params : array of const) : boolean;
{{
Idem WriteErrorLogMsg

Formata a string antes de chamar WriteErrorLogMsg()

Rev. 19/5/2005
}
begin
    Result := WriteErrorLogMsg(Format(ErrorMsg, Params));
end;

{-**********************************************************************
************************************************************************
******************
******************  Class:    ELoggedException
******************  Category: No category
******************
************************************************************************
************************************************************************}
{--------------------------------------------------------------------------------------------------------------------------------}
constructor ELoggedException.Create(const Msg : string);
{{
Contrutor básico de ELoggedException.

Msg : String com a mensagem a ser logada e repassada para o sub-sistema de excessões.

Construtor básico de ELoggedException

Rev. 19/5/2005
}
begin
    //Devido a babaquice da Borland nenhum constructor aqui e virtual!!!!
    inherited;
    Self.WriteLog(Self.Message);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
constructor ELoggedException.CreateFmt(const Msg : string; const Args : array of const);
{{
Construtor básico de ELoggedException.

Msg : String com a mensagem a ser logada e repassada para o sub-sistema de excessões.

Args: Vetor com os valores para a formatação da cadeia.

Rev. 19/5/2005
}
begin
    inherited;
    Self.WriteLog(Self.Message);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
constructor ELoggedException.CreateFmtHelp(const Msg : string; const Args : array of const; AHelpContext : Integer);
{{
Construtor básico de ELoggedException.

Msg : String com a mensagem a ser logada e repassada para o sub-sistema de excessões.

Args: Vetor com os valores para a formatação da cadeia.

Rev. 19/5/2005
}
begin
    inherited;
    Self.WriteLog(Self.Message);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
constructor ELoggedException.CreateHelp(const Msg : string; AHelpContext : Integer);
{{
Construtor de ELoggedException com suporte a Ajuda de contexto.

Msg : String com a mensagem a ser logada e repassada para o sub-sistema de excessões.

Rev. 19/5/2005
}
begin
    inherited;
    Self.WriteLog(Self.Message);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
constructor ELoggedException.CreateRes(Ident : Integer);
{{
Construtor de ELoggedException que realiza a leitura de identificador de recurso.

Msg : String com a mensagem a ser logada e repassada para o sub-sistema de excessões.

Rev. 19/5/2005
}
begin
    inherited;
    Self.WriteLog(Self.Message);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
constructor ELoggedException.CreateRes(ResStringRec : PResStringRec);
{{
Construtor de ELoggedException que faz a leitura da mensagem de um PResStringRec.

ResStringRef : PResStringRec com a mensagem a ser logada e repassada para o sub-sistema de excessões.

Rev. 19/5/2005
}
begin
    inherited;
    Self.WriteLog(Self.Message);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
constructor ELoggedException.CreateResFmt(Ident : Integer; const Args : array of const);
{{
Construtor de ELoggedException que faz a leitura da mensagem de um recurso a ser formatado posteriormente.

Ident : Identifcador do recurso com a mensagem a ser logada e repassada para o sub-sistema de excessões.

Args: Vetor com os valores para a formatação da cadeia.

Rev. 19/5/2005
}
begin
    inherited;
    Self.WriteLog(Self.Message);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
constructor ELoggedException.CreateResFmt(ResStringRec : PResStringRec; const Args : array of const);
{{
Construtor de ELoggedException que faz a leitura da mensagem de um PResStringRec a ser formatado posteriormente.

ResStringRec : String com a mensagem a ser logada e repassada para o sub-sistema de excessões.

Args: Vetor com os valores para a formatação da cadeia.

Rev. 19/5/2005
}
begin
    inherited;
    Self.WriteLog(Self.Message);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
constructor ELoggedException.CreateResFmtHelp(Ident : Integer; const Args : array of const; AHelpContext : Integer);
{{
Construtor de ELoggedException que faz a leitura da mensagem de um identificador de recurso a ser formatado posteriormente.

Ident : Identificador do recurso com a mensagem a ser logada e repassada para o sub-sistema de excessões.

Args: Vetor com os valores para a formatação da cadeia.

Rev. 19/5/2005
}
begin
    inherited;
    Self.WriteLog(Self.Message);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
constructor ELoggedException.CreateResFmtHelp(ResStringRec : PResStringRec; const Args : array of const; AHelpContext : Integer);
{{
Construtor de ELoggedException que faz a leitura da mensagem de um PResStringRec a ser formatado posteriormente.

Msg : PResStringRec com a mensagem a ser logada e repassada para o sub-sistema de excessões.


Rev. 19/5/2005
}
begin
    inherited;
    Self.WriteLog(Self.Message);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
constructor ELoggedException.CreateResHelp(Ident : Integer; AHelpContext : Integer);
{{
Construtor de ELoggedException que faz a leitura da mensagem de um identificador de recurso a ser formatado posteriormente.

Ident : Identificador do recurso do com a mensagem a ser logada e repassada para o sub-sistema de excessões.

Rev. 19/5/2005
}
begin
    inherited;
    Self.WriteLog(Self.Message);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
constructor ELoggedException.CreateResHelp(ResStringRec : PResStringRec; AHelpContext : Integer);
{{
Construtor de ELoggedException que faz a leitura da mensagem de um PResStringRec e com suporte a ajuda de contexto.

ResStringRec : PResStringRec com a mensagem a ser logada e repassada para o sub-sistema de excessões.

AHelpContexto : Id do contexto de ajuda.

Rev. 19/5/2005
}
begin
    inherited;
    Self.WriteLog(Self.Message);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function ELoggedException.FormatLogMessage(const ErrorMsg : string) : string;
{{
Formata a mensagem recebida pelo construtor da excessão para a string a ser salva no streamer de log

Rev. 19/5/2005
}
begin
    if Assigned(FormatMessageProc) and (ErrorLogMode = elmFormatted) then begin
        FormatMessageProc(ErrorMsg);
    end else begin
        Result := ErrorMsg;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure ELoggedException.WriteLog(const Msg : string);
{{
Salva mensagem da excessão no arquivo de log padrão com a formatação deste

Rev. 19/5/2005
}
begin
    TLogFile.Log(Msg, lmtError);
end;

{-**********************************************************************
************************************************************************
******************
******************  Class:    TLogFile
******************  Category: No category
******************
************************************************************************
************************************************************************}
{--------------------------------------------------------------------------------------------------------------------------------}
constructor TLogFile.Create(const AFileName : string; Lock : boolean);
{{
Construtor que inicializa o streamer de saída.

AFileName : caminho completo para um arquivo/identificador do streamer de saída

Lock : Flag para deixar este streamer no modo exclusivo. Assim este arquivo de log será acessivel apenas para um processo por vez.
E também não será liberado entre as chamadas de StreamNeeded/StreamDispose.

Rev. 19/5/2005

Revision - 20120522 - roger
Removido a ForceFile para os casos onde não se exite a necessidade de Lock, assim o arquivo de log pode ser inicializado em outro
local fora da mesma pasta do runtime

}
begin
    inherited Create;
    Self.FDebugLevel := DBGLEVEL_ULTIMATE; //inicia com nivel mais alto possivel
    Self.FFileName   := AFileName;
	 //TFileHnd.ForceFilename(Self.FFileName);
	 Self.FLocked   := Lock;
	 Self.FOpenMode := fmOpenReadWrite + fmShareDenyWrite; //Padrao para acesso exclusivo RW
	 if Self.FLocked then begin
	 	 TFileHnd.ForceFilename(Self.FFileName);
        Self.FStream := TFileStream.Create(AFileName, Self.OpenMode);
    end else begin
        Self.FStream := nil;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
destructor TLogFile.Destroy;
{{
Destrutor que libera o streamer de saída do log e a referencia do formatador.

Rev. 19/5/2005
}
begin
    Self.FFormatter := nil; //libera referencia
    Self.FLocked    := False;
    Self.StreamDispose;
    inherited;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TLogFile.Commit;
{{
Salva o conteudo o buffer interno do LogFile para o disco.

Notas: Esta chamada não altera o estado Buffered para false. Para o caso de não haver nada no Buffer nada será feito

Rev. 19/5/2005
}
begin
    if ((Self.FLogBuffer <> nil) and (Self.FLogBuffer.Count > 0)) then begin
        EnterCriticalSection(LogCriticalSection);
        try
            Self.WriteTo(Self.FLogBuffer.Text);
            Self.FLogBuffer.Clear;
        finally
            LeaveCriticalSection(LogCriticalSection);
        end;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TLogFile.CopyToStream(InitPos, Count : int64; DestStream : TStream) : int64;
{{
Copia o conteudo do Stream do log para um passado.

InitPos : Posição inicial do streamer de log.

Count : Quantidade de bytes a ser copiados

DestStream : Stream de destino da operação.

Rev. 19/5/2005
}
var
    OldPos : int64;
begin
    Result := 0;
    Self.StreamNeeded();
    try
        OldPos := Self.FStream.Position;
        try
            Self.FStream.Position := InitPos;
            Self.FStream.CopyFrom(Self.FStream, Count);
        finally
            Self.FStream.Position := OldPos;
        end;
    finally
        Self.StreamNeeded();
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TLogFile.Discard;
{{
Limpa o buffer ( se este existir )
}
{1 Limpa o buffer ( se este existir ) }
begin
    EnterCriticalSection(LogCriticalSection);
    try
        if (Self.FLogBuffer <> nil) then begin
            Self.FLogBuffer.Clear;
        end;
    finally
        LeaveCriticalSection(LogCriticalSection);
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TLogFile.GetBuffered : boolean;
{{
Retorna se a instancia do Logger esta buferizada ou não.

Rev. 19/5/2005
}
begin
    Result := (Self.FLogBuffer <> nil);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TLogFile.GetBufferText : string;
{{
Retorna o texto bufferizado do log

Rev. 19/5/2005
}
begin
    EnterCriticalSection(LogCriticalSection);
    try
        if (Self.FLogBuffer <> nil) then begin
            Result := Self.FLogBuffer.Text;
        end else begin
            Result := EmptyStr;
        end;
    finally
        LeaveCriticalSection(LogCriticalSection);
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
class function TLogFile.GetDefaultLogFile : TLogFile;
{{
Retorna instância do Logger padrao do aplicativo.

Rev. 19/5/2005
}
begin
    Result := GlobalDefaultLogFile;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TLogFile.GetSize : int64;
{{
Tamanho do Streamer corrente do log.

Rev. 19/5/2005
}
begin
    Self.StreamNeeded;
    try
        Result := Self.FStream.Size;
    finally
        Self.StreamDispose;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
class procedure TLogFile.Initialize;
{{
Método de classe que garante a existencia de uma instancia valida em GlobalDefaultLogFile e inicializa os atributos globais do modo
de compatibilidade.

A inicialização desta unit chama este metodo.

Método de classe que garante a existencia de uma instancia valida em GlobalDefaultLogFile

Rev. 19/5/2005
}
begin
    GlobalLogFileName := ChangeFileExt(ParamStr(0), DEFAULT_APP_LOG_EXTENSION);
    LogUserName := GetWindowsUserName();
    if LogUserName = EmptyStr then begin
        LogUserName := '"Usuário desconhecido"';
    end;
    if (GlobalDefaultLogFile = nil) then begin
        GlobalDefaultLogFile := TLogFile.Create(GlobalLogFileName, False);//Streamer de saida não locked para outros
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
class procedure TLogFile.Log(const Msg : string; LogMessageType : TLogMessageType = lmtError);
{{
Rotina chave onde o conteudo em Msg sera salvo pela instancia atribuida a GlobalDefaultLogFile que é uma descendente de TLogFile.

Rev. 19/5/2005
}
begin
    GlobalDefaultLogFile.WriteLog(Msg, LogMessageType);
end;

class procedure TLogFile.LogDebug(const Msg : string; MinDebugLevel : Integer);
begin
	 if (GlobalDefaultLogFile.DebugLevel >= MinDebugLevel) then begin
		 GlobalDefaultLogFile.WriteLog(Msg, lmtDebug);
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TLogFile.SetBuffered(const Value : boolean);
{{
Ajusta o LogFile para salva em memoria todo o conteudo registrado. Este conteudo ficara armazenado ate a chamada a Commit ou
descartado com a chamada a Discard();

Rev. 19/5/2005
}
begin
    EnterCriticalSection(LogCriticalSection);
    try
        if (Value) then begin
            if (Self.FLogBuffer = nil) then begin
                Self.FLogBuffer := TStringList.Create;
            end;
        end else begin
            Self.Commit();
            if (Self.FLogBuffer <> nil) then begin
                FreeAndNil(Self.FLogBuffer);
            end;
        end;
    finally
        LeaveCriticalSection(LogCriticalSection);
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
class procedure TLogFile.SetDefaultLogFile(LogFile : TLogFile);
{{
Alterna a referencia de GetDefaultLogFile para o valor passado.

Nota: A instância anterior não será destruida. Assim se desejavel recupere-a antes para fazer sua liberacao posterior.

Ex.:
OldRef := TLogFile.GetDefaultLogFile;
TLogFile.SetDefaultLogFile( NewLogger );
if (OldRef <> nil) then begin
OldRef.Free;
end;

Rev. 19/5/2005
}
var
    OldRef : TLogFile;
begin
    OldRef := GlobalDefaultLogFile;
    if (OldRef <> LogFile) then begin
        GlobalDefaultLogFile := LogFile;
        OldRef.Free;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TLogFile.SetFileName(const Value : string);
{{
Ajusta o nome do arquivo de saida para o log.

Rev. 19/5/2005
}
begin
    if (not SameText(Self.FFileName, Value)) then begin
        EnterCriticalSection(LogCriticalSection);
        try
            FFileName := Value;
            //Libera o streamer corrente
            if (Self.FStream <> nil) then begin
                Self.FStream.Free;
            end;
            //Remonta com o novo nome, se locked ele persiste ao final
            Self.StreamNeeded;
            Self.StreamDispose;
        finally
            LeaveCriticalSection(LogCriticalSection);
        end;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TLogFile.StreamDispose;
{{
Método chamado em conjunção com StreamNeeded para liberar/alocar um streamer para acesso ao arquivo de saída.
Assim para o caso de sobreescrever o modo como o streamer de saída é gerado, ambos deve ser tratados.
}
{1 Libera se não estiver locked o streamer de saida }
begin
	 if (not Self.Locked) and Assigned(Self.FStream) then begin //Libera apenas se naum exclusivo
        FreeAndNil(Self.FStream);
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TLogFile.StreamNeeded;
{{
Método chamado em conjunção com StreamDispose para liberar/alocar um streamer para acesso ao arquivo de saída.
Assim para o caso de sobreescrever o modo como o streamer de saída é gerado, ambos deve ser tratados.
}
{1 Recupera/cria um streamer de saida. }
begin
	 if not Assigned(Self.FStream) then begin
        if not FileExists(Self.FFileName) then begin
            TFileHnd.ForceFilename(Self.FFileName);
        end;
        Self.FStream := TFileStream.Create(Self.FFileName, Self.OpenMode);
        Self.FStream.Seek(0, soFromEnd); //Fim do streamer
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TLogFile.WriteLog(const Msg : string; LogMessageType : TLogMessageType = lmtError);
{{
Formata e salva a mensagem passada. Usa( se houver ) a referencia ao IFormatter da instancia, não existindo usa o ponteiro
atribuido a FormatMessageProc( este assumido como sempre válido )

Msg : Mensagem a ser salva.

LogMessageType : Tipo da mensage a ser salva.
}
{1 Formata e salva para o buffer ou streamer de saida a mensagem passada }
var
    Txt : string;
begin
    EnterCriticalSection(LogCriticalSection);
    try
        if (Assigned(Self.FFormatter)) then begin
            Txt := Self.FFormatter.FormatLogMsg(Msg, LogMessageType);
        end else begin
            Txt := FormatMessageProc(Msg, LogMessageType); //Asumido que sempre existe ponteiro para rotina atribuido
        end;
        if (Self.FLogBuffer <> nil) then begin
            Self.FLogBuffer.Add(Txt);
        end else begin
            Self.WriteTo(Txt);
        end;
    finally
        LeaveCriticalSection(LogCriticalSection);
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TLogFile.WriteTo(const Txt : string);
{{
Rotina chave onde o texto capturado sera salvo no streamer de saida.
Sempre salvará arquivo no modo AnsiString.

Revision - 20100820 - roger
Sempre salvará arquivo no modo AnsiString
}
{1 Rotina chave onde o texto capturado sera salvo no streamer de saida }
var
    ConvertStr : ansistring;
begin
    EnterCriticalSection(LogCriticalSection);
    try
        Self.StreamNeeded();
        try
            ConvertStr := ansistring(Txt);
            Self.FStream.WriteBuffer(PAnsiChar(ConvertStr)^, Length(Txt));
        finally
            Self.StreamDispose;
        end;
    finally
        LeaveCriticalSection(LogCriticalSection);
    end;
end;

{-**********************************************************************
************************************************************************
******************
******************  Class:    TLogFormatter
******************  Category: No category
******************
************************************************************************
************************************************************************}
{--------------------------------------------------------------------------------------------------------------------------------}
function TLogFormatter.FormatLogMsg(const LogMsg : string; LogMessageType : TLogMessageType = lmtError) : string;
{{
Rotina padrao de formatacao de messagens de log. Recebe  LogMessageType indicando o tipo de log.
}
    {1 Rotina padrao de formatacao de messagens de log. Recebe  LogMessageType indicando o tipo de log. }
var
    i :  Integer;
    SL : TStringList;
begin
    SL := TStringList.Create;
    try
        //Identa o texto
        SL.Text := LogMsg;
        for i := 0 to SL.Count - 1 do begin
            SL.Strings[i] := #9 + SL.Strings[i];
        end;
        if LogMessageType = lmtError then begin
            SL.Insert(0, Format('-Registro de erro: (%s) ', [LogUserName]) +
                FormatDateTime('dddd, mmmm d, yyyy, " às " hh:mm:ss AM/PM', Now()));
        end else begin
            if LogMessageType = lmtInformation then begin
                SL.Insert(0, Format('-Registro de Informação: (%s) ', [LogUserName]) +
                    FormatDateTime('dddd, mmmm d, yyyy, " às " hh:mm:ss AM/PM', Now()));
            end else begin
                if LogMessageType = lmtWarning then begin
                    SL.Insert(0, Format('-Registro de Alerta: (%s) ', [LogUserName]) +
                        FormatDateTime('dddd, mmmm d, yyyy, " às " hh:mm:ss AM/PM', Now()));
                end else begin
                    if LogMessageType = lmtDebug then begin
                        SL.Insert(0, Format('-Registro de Debug: (%s) ', [LogUserName]) +
                            FormatDateTime('dddd, mmmm d, yyyy, " às " hh:mm:ss AM/PM', Now()));
                    end;
                end;
            end;
        end;
        Result := SL.Text; //Usa var para adcionar Separadores
    finally
        SL.Free;
    end;
end;

{-**********************************************************************
************************************************************************
******************
******************  Class:    TLogSingleLineFormatter
******************  Category: No category
******************
************************************************************************
************************************************************************}
{--------------------------------------------------------------------------------------------------------------------------------}
function TLogSingleLineFormatter.FormatLogMsg(const LogMsg : string; LogMessageType : TLogMessageType = lmtError) : string;
{{
Rotina padrao de formatacao de messagens de log para única linha. Recebe  LogMessageType indicando o tipo de log.
}
    {1 Rotina padrao de formatacao de messagens de log para única linha. Recebe  LogMessageType indicando o tipo de log. }
begin
    if LogMessageType = lmtError then begin
        Result := Format('Erro: (%s) ', [LogUserName]) + FormatDateTime('dd/mm/yyyy hh:nn:ss', Now()) + ' - ' + LogMsg + #13#10;
    end else begin
        if LogMessageType = lmtInformation then begin
            Result := Format('Informação: (%s) ', [LogUserName]) + FormatDateTime('dd/mm/yyyy hh:nn:ss', Now()) +
                ' - ' + LogMsg + #13#10;
        end else begin
            if LogMessageType = lmtWarning then begin
                Result := Format('Alerta: (%s) ', [LogUserName]) + FormatDateTime('dd/mm/yyyy hh:nn:ss', Now()) +
                    ' - ' + LogMsg + #13#10;
            end else begin
                if LogMessageType = lmtDebug then begin
                    Result := Format('Debug: (%s) ', [LogUserName]) + FormatDateTime('dd/mm/yyyy hh:nn:ss', Now()) +
                        ' - ' + LogMsg + #13#10;
                end;
            end;
        end;
    end;
end;

initialization
    begin
        InitializeCriticalSection(LogCriticalSection);
        EnterCriticalSection(LogCriticalSection);
        try
            TLogFile.Initialize();
        finally
            LeaveCriticalSection(LogCriticalSection);
        end;
    end;

finalization
    begin
        EnterCriticalSection(LogCriticalSection);
        try
            if (Assigned(GlobalDefaultLogFile)) then begin
                GlobalDefaultLogFile.Free;
            end;
        finally
            LeaveCriticalSection(LogCriticalSection);
        end;
        DeleteCriticalSection(LogCriticalSection);
    end;
end.
