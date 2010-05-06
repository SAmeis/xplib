{$IFDEF ThreadProgressForm}
    {$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I DlgLib.inc}

unit ThreadProgressForm;

{{
}

interface

uses
    Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls, Buttons, ExtCtrls, Gauges, Super, Messages, XPThreads;

type
    TProgressThread = class;

    TProgressUpdateEvent = procedure(Sender : TProgressThread) of object;
 {{
 Evento de atualização do progresso do thread
 }
    {1 Evento de atualização do progresso do thread }
    TThreadedSimpleProgressForm = class(TForm)
 {{
 Descendente de TForm ajustado de acordo com os atributos do TProgressThread de modo a exibir o progresso do mesmo.
 }
        CancelBtn : TBitBtn;
        DescriptionLabel : TLabel;
        Gauge :     TGauge;
        ProgressTimer : TTimer;
        procedure CancelBtnClick(Sender : TObject);
        procedure FormShow(Sender : TObject);
        procedure ProgressTimerTimer(Sender : TObject);
    private
        ErrorCode :    Integer;
        ErrorMessage : string;
        FExecutingThread : TProgressThread;
    public
        constructor Create(AOwner : TComponent); override;
        procedure BeforeDestruction; override;
    end;

    TCustomProgressThread = class(TXPNamedThread)
 {{
 Descendente de TXPNamedThread capaz de gerar acompanhamento de progresso da operação.
 
 Como todo descendente de TThread será necessário sobre-escrever Execute e prover os 3 métodos de acompanhamento de progresso
 básicos. Sendo eles:
 
 - GetMinValue
 
 - GetMaxValue
 
 - GetCurrentValue
 
 NOTA 1 : Sempre criar este thread como suspenso e Garantir que o mesmo irá atender a flag "Terminated" para finalizar seu
 processamento e retornar do execute.
 
 NOTA 2 : Sempre sair do método execute com a flag Terminated = True.
 }
    private
        FJobDescription : string;
        FJobTile : string;
        FOnUpdateProgress : TProgressUpdateEvent;
    protected
        function GetCurrentValue : int64; virtual; abstract;
        {1 Método para leitura do valor corrente do progresso do thread. }
        function GetMaxValue : int64; virtual; abstract;
        {1 Método para leitura do valor máximo do progresso do thread. }
        function GetMinValue : int64; virtual; abstract;
        {1 Método para leitura do valor mínimo do progresso do thread. }
        procedure SetJobDescription(const Value : string); virtual;
        {1 Método para escrita da descrição do processo executado pelo thread. }
        procedure SetJobTile(const Value : string); virtual;
        {1 Método para escrita do titulo do processo executado pelo thread. }
    public
        property JobDescription : string read FJobDescription write SetJobDescription;
        property JobTile : string read FJobTile write SetJobTile;
        property OnUpdateProgress : TProgressUpdateEvent read FOnUpdateProgress write FOnUpdateProgress;
        {{
        Evento disparado quando a VCL for consultar o estado do progresso do thread em execução.
        }
        {1 Evento disparado quando a VCL for consultar o estado do progresso do thread em execução. }
    end;

    TProgressThread = class(TCustomProgressThread)
 {{
 Descendente de TThread capaz de gerar dialogo de acompanhamento de progresso.
 
 Como todo descendente de TThread será necessário sobre-escrever Execute e prover os 3 métodos de acompanhamento de progresso
 básicos. Sendo eles:
 
 - GetMinValue
 
 - GetMaxValue
 
 - GetCurrentValue
 
 NOTA 1 : Sempre criar este thread como suspenso e Garantir que o mesmo irá atender a flag "Terminated" para finalizar seu
 processamento e retornar do execute.
 
 NOTA 2 : Sempre sair do método execute com a flag Terminated = True.
 }
    private
        FCS : TRTLCriticalSection;
        FModalDialog : boolean;
        FModalResult : TModalResult;
        FProgressForm : TThreadedSimpleProgressForm;
        FUpdateInterval : Integer;
        procedure ReleaseForm;
        procedure ReStart;
        {1 Descendente de TThread capaz de gerar dialogo de acompanhamento de progresso }
        function GetCanCancel : boolean;
        procedure SetCanCancel(const Value : boolean);
        procedure SetUpdateInterval(const Value : Integer);
    protected
        procedure SetJobDescription(const Value : string); override;
        {1 Método para leitura do valor máximo do progresso do thread. }
        procedure SetJobTile(const Value : string); override;
        {1 Método para leitura do valor corrente do progresso do thread. }
    public
        constructor Create(const ThreadName : string); virtual;
        {1 Construtor de TProgressThread }
        destructor Destroy; override;
        {1 Destrutor de TProgressThread }
        procedure BeforeDestruction; override;
        {1 Remove referências a si próprio }
        procedure Lock;
        {1 Trava a instância para acesso aos atributos ThreadSafe }
        procedure Resume;
        procedure UnLock;
        {1 Destrava a instância para acesso aos atributos ThreadSafe }
        property CanCancel : boolean read GetCanCancel write SetCanCancel;
  {{
  Flag indicando se o thread pode ser cancelado pelo diálogo de progresso.
  }
        {1 Flag indicando se o thread pode ser cancelado pelo diálogo de progresso. }
        property ModalDialog : boolean read FModalDialog write FModalDialog;
  {{
  Flag indicando se o diálogo será modal ou não, o padrão é verdadeiro.
  }
        {1 Flag indicando se o diálogo será modal ou não, o padrão é verdadeiro. }
        property ModalResult : TModalResult read FModalResult;
  {{
  ModalResult, fará sentido apenas se thread for cancelado.
  }
        {1 ModalResult, fará sentido apenas se thread for cancelado. }
        property UpdateInterval : Integer read FUpdateInterval write SetUpdateInterval;
    end;


implementation

uses
    Math, Dialogs;


{$R *.DFM}


{-**********************************************************************
************************************************************************
******************
******************  Class:    TThreadedSimpleProgressForm
******************  Category: No category
******************
************************************************************************
************************************************************************}
{--------------------------------------------------------------------------------------------------------------------------------}
procedure TThreadedSimpleProgressForm.CancelBtnClick(Sender : TObject);
{{
Para o caso de habilitado o cancelamento o botao cancelar dispara este metodo.
Uma confirmacao eh pedida apos o thread ser suspenso, se nao confirmado o thread eh liberado para executar novamente.

NOTA: Esta rotina DEVE rodar no Tread primario do aplicativo.
}
var
    ret : Integer;
begin
    if (Self.FExecutingThread.Terminated) then begin
        Self.Close();
    end else begin
        while (not Self.FExecutingThread.Suspended) do begin
            Self.FExecutingThread.Suspend;
        end;
        ret := MessageDlg('Deseja cancelar a operação?', mtConfirmation, [mbYes, mbNo], 0);
        if (ret = mrYes) then begin
            Self.FExecutingThread.Terminate();
        end else begin
            //reativa o thread
            while (Self.FExecutingThread.Suspended) do begin
                Self.FExecutingThread.Resume();
            end;
        end;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TThreadedSimpleProgressForm.FormShow(Sender : TObject);
begin
    Application.ProcessMessages;
    Self.ProgressTimer.Enabled := True; //form visible -> atualizar status
    Self.FExecutingThread.ReStart;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TThreadedSimpleProgressForm.ProgressTimerTimer(Sender : TObject);
{{
Atualiza controles GUI com atributos do thread
}
begin
    if (Self.FExecutingThread = nil) then begin
        Exit;
    end;
    Self.FExecutingThread.Lock; //Impede thrash pointers nesta area
    try
        if (not Self.FExecutingThread.IsAlive) or (Self.FExecutingThread.FatalException <> nil) then begin
            Self.Close();
            Self.ProgressTimer.Enabled := False; //form fechado -> sem atualização do status
        end else begin
            Self.ProgressTimer.Enabled := False;  //Impede sobre-posicao de chamadas
            try
                if Assigned(Self.FExecutingThread.OnUpdateProgress) then begin
                    try
                        Self.FExecutingThread.OnUpdateProgress(Self.FExecutingThread);
                    except
                        on E : Exception do begin
                            Self.ProgressTimer.Enabled := False;
                            Self.Close;
                            Self.ErrorMessage := E.Message;
                            Self.ErrorCode    := ERROR_CONTINUE; //Nao pode executar operacao, Chamador que se vire
                            Exit;
                        end;
                    end;
                end else begin
                    Self.Gauge.MinValue := Min(Self.FExecutingThread.GetMinValue, Self.Gauge.MaxValue);
                    Self.Gauge.MaxValue := Self.FExecutingThread.GetMaxValue;
                    Self.Gauge.Progress := Self.FExecutingThread.GetCurrentValue;
                end;
            finally
                Self.ProgressTimer.Enabled := True;  //Restaura novos incrementos de progresso
            end;
        end;
    finally
        Self.FExecutingThread.UnLock;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TThreadedSimpleProgressForm.BeforeDestruction;
{{
Antes de se destruir o thread libera a referencia a si mesmo do Dialogo de progresso.

Revision: 4/9/2006 - Roger  
}
begin
    if ((Self.FExecutingThread <> nil) and (Self.FExecutingThread.FProgressForm = Self)) then begin
        //Elimina refencia que se torna agora invalida invalida
        Self.FExecutingThread.FProgressForm := nil;
    end;
    inherited;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
constructor TThreadedSimpleProgressForm.Create(AOwner : TComponent);
begin
    inherited;
    ErrorMessage := EmptyStr;
end;

{{
Método para leitura do valor corrente do progresso do thread.
}
{1 Método para leitura do valor corrente do progresso do thread. }
{{
Método para leitura do valor máximo do progresso do thread.
}
{1 Método para leitura do valor máximo do progresso do thread. }
{{
Método para leitura do valor mínimo do progresso do thread.
}
{1 Método para leitura do valor mínimo do progresso do thread. }
{{
Método para leitura do valor corrente do progresso do thread.
}
{1 Método para leitura do valor corrente do progresso do thread. }
{{
Método para leitura do valor máximo do progresso do thread.
}
{1 Método para leitura do valor máximo do progresso do thread. }
{-**********************************************************************
************************************************************************
******************
******************  Class:    TCustomProgressThread
******************  Category: No category
******************
************************************************************************
************************************************************************}
{--------------------------------------------------------------------------------------------------------------------------------}
{{
Método para leitura do valor corrente do progresso do thread.
}
 {1 Método para leitura do valor corrente do progresso do thread. }
 {--------------------------------------------------------------------------------------------------------------------------------}
{{
Método para leitura do valor máximo do progresso do thread.
}
 {1 Método para leitura do valor máximo do progresso do thread. }
 {--------------------------------------------------------------------------------------------------------------------------------}
{{
Método para leitura do valor mínimo do progresso do thread.
}
 {1 Método para leitura do valor mínimo do progresso do thread. }
 {--------------------------------------------------------------------------------------------------------------------------------}
procedure TCustomProgressThread.SetJobDescription(const Value : string);
{{
Método para escrita da descrição do passo executado pelo thread.
}
{1 Método para escrita da descrição do passo executado pelo thread. }
begin
    FJobDescription := Value;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TCustomProgressThread.SetJobTile(const Value : string);
{{
Método para leitura do valor mínimo do progresso do thread.
}
{1 Método para leitura do valor mínimo do progresso do thread. }
begin
    FJobTile := Value;
end;

{{
Método para leitura do valor corrente do progresso do thread.
}
{1 Método para leitura do valor corrente do progresso do thread. }
{{
Método para leitura do valor máximo do progresso do thread.
}
{1 Método para leitura do valor máximo do progresso do thread. }
{{
Método para leitura do valor mínimo do progresso do thread.
}
{1 Método para leitura do valor mínimo do progresso do thread. }
{{
Método para leitura do valor corrente do progresso do thread.
}
{1 Método para leitura do valor corrente do progresso do thread. }
{{
Método para leitura do valor máximo do progresso do thread.
}
{1 Método para leitura do valor máximo do progresso do thread. }
{{
Método para leitura do valor mínimo do progresso do thread.
}
{1 Método para leitura do valor mínimo do progresso do thread. }
{{
Método para leitura do valor corrente do progresso do thread.
}
{1 Método para leitura do valor corrente do progresso do thread. }
{{
Método para leitura do valor máximo do progresso do thread.
}
{1 Método para leitura do valor máximo do progresso do thread. }
{{
Método para leitura do valor mínimo do progresso do thread.
}
{1 Método para leitura do valor mínimo do progresso do thread. }
{-**********************************************************************
************************************************************************
******************
******************  Class:    TProgressThread
******************  Category: No category
******************
************************************************************************
************************************************************************}
{--------------------------------------------------------------------------------------------------------------------------------}
procedure TProgressThread.ReleaseForm;
{{
Rotina responsavel pela liberacao do form de progresso, chamada no destrutor do thread.

NOTA: Rotina deve ser chamada pelo thread primario(VCL).

Revision: 6/9/2006 - Roger  
}
begin
    Self.FProgressForm.Free;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TProgressThread.ReStart;
{{
Descendente de TThread capaz de gerar dialogo de acompanhamento de progresso.

Como todo descendente de TThread será necessário sobre-escrever Execute e prover os 3 métodos de acompanhamento de progresso
básicos. Sendo eles:

- GetMinValue

- GetMaxValue

- GetCurrentValue

NOTA 1 : Sempre criar este thread como suspenso e Garantir que o mesmo irá atender a flag "Terminated" para finalizar seu
processamento e retornar do execute.

NOTA 2 : Sempre sair do método execute com a flag Terminated = True.
}
{1 Descendente de TThread capaz de gerar dialogo de acompanhamento de progresso }
begin
    Self.Resume();
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TProgressThread.GetCanCancel : boolean;
{{
Retorna boolean indicando se a operação em execução pelo thread pode ser cancelada.

Revision: 4/9/2006 - Roger  
}
begin
    Result := Assigned(Self.FProgressForm) and Self.FProgressForm.CancelBtn.Enabled;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TProgressThread.SetCanCancel(const Value : boolean);
begin
    if (Assigned(Self.FProgressForm)) then begin
        Self.FProgressForm.CancelBtn.Enabled := Value;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TProgressThread.SetJobDescription(const Value : string);
{{
Método para leitura do valor máximo do progresso do thread.
}
{1 Método para leitura do valor máximo do progresso do thread. }
begin
    inherited;
    if (Self.FProgressForm <> nil) then begin
        Self.FProgressForm.DescriptionLabel.Caption := Value;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TProgressThread.SetJobTile(const Value : string);
{{
Método para leitura do valor corrente do progresso do thread.
}
{1 Método para leitura do valor corrente do progresso do thread. }
begin
    inherited;
    if (Self.FProgressForm <> nil) then begin
        Self.FProgressForm.Caption := Value;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TProgressThread.BeforeDestruction;
{{
Remove referências a si próprio no dialogo de progresso, pois form é do tipo auto-destroy.
}
{1 Remove referências a si próprio }
begin
    Self.Lock();
    try
        if ((Self.FProgressForm <> nil) and (Self.FProgressForm.FExecutingThread = Self)) then begin    //remove trash pointer
            Self.FProgressForm.FExecutingThread := nil;
        end;
        inherited;
    finally
        Self.UnLock();
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
constructor TProgressThread.Create(const ThreadName : string);
{{
Construtor de TProgressThread, recebe o nome do thread para identificação no depurador apenas.
}
    {1 Construtor de TProgressThread }
begin
    inherited Create(True, ThreadName);
    InitializeCriticalSection(Self.FCS);
    Self.FreeOnTerminate := False; //sempre falso -> ser destruido pelo form
    Self.FModalDialog    := True;
    Self.FProgressForm   := TThreadedSimpleProgressForm.Create(nil);
    Self.FProgressForm.FExecutingThread := Self;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
destructor TProgressThread.Destroy;
{{
Destrutor de TProgressThread.
Libera a RTLCriticalSection responsável pelo acesso concorrente aos atributos da instância, bem como a janela de acompanhamento do
processo.

}
    {1 Destrutor de TProgressThread }
begin
    if (GetCurrentThreadId = Self.ThreadID) then begin
        Synchronize(Self.ReleaseForm);
    end else begin
        Self.ReleaseForm;
    end;
    DeleteCriticalSection(FCS);
    inherited;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TProgressThread.Lock;
{{
Trava a instância para acesso aos atributos ThreadSafe
}
{1 Trava a instância para acesso aos atributos ThreadSafe }
begin
    EnterCriticalSection(Self.FCS);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TProgressThread.Resume;
{{
Esta chamada quando feita pelo Thread criador( Assumido como sendo o thread da VCL) irá criar o form de exibição do progresso.
Sendo chamado pelo próprio thread chama o ancestral.
}
begin
    if ((Self.FProgressForm <> nil) and
        (not Self.FProgressForm.Visible) and (GetCurrentThreadId() <> Self.ThreadID)) then begin
        Self.FProgressForm.ProgressTimer.Interval := Self.FUpdateInterval;
        //Apenas o Thread VCL cria/exibe o dialogo
        if (Self.FModalDialog) then begin
            Self.FModalResult := Self.FProgressForm.ShowModal;
        end else begin
            Self.FProgressForm.Show;
        end;
    end else begin
        inherited Resume();
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TProgressThread.UnLock;
{{
Destrava a instância para acesso aos atributos ThreadSafe
}
{1 Destrava a instância para acesso aos atributos ThreadSafe }
begin
    LeaveCriticalSection(Self.FCS);
end;

procedure TProgressThread.SetUpdateInterval(const Value : Integer);
begin
    FUpdateInterval := Value;
    if (Assigned(Self.FProgressForm)) then begin
        Self.FProgressForm.ProgressTimer.Interval := Value;
    end;
end;

{{
Método para leitura do valor corrente do progresso do thread.
}
{1 Método para leitura do valor corrente do progresso do thread. }
{{
Método para leitura do valor máximo do progresso do thread.
}
{1 Método para leitura do valor máximo do progresso do thread. }
{{
Método para leitura do valor mínimo do progresso do thread.
}
{1 Método para leitura do valor mínimo do progresso do thread. }

end.
