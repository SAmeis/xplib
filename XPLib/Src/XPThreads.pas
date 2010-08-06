{$IFDEF XPThreads}
  {$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I XPLib.inc}

unit XPThreads;

{{
Classes and routines for thread and synchronization.


Revision 19/5/2005
}
interface

uses
    Windows, SysUtils, Classes;

type
    TThreadNameInfo = record
 {{
 Record for access to internal thread info, by Windows.RaiseException()
 }
        FType:     longword;
  {{
  Type for TThreadNameInfo.
  }
        {1 Type for TThreadNameInfo. }
        FName:     PChar;
  {{
  Name for TThreadNameInfo.
  }
        {1 Name for TThreadNameInfo. }
        FThreadID: longword;
  {{
  ThreadID for TThreadNameInfo.
  }
        {1 ThreadID for TThreadNameInfo. }
        FFlags:    longword;
  {{
  Flags for TThreadNameInfo.
  }
        {1 Flags for TThreadNameInfo. }
    end;


type
    EXPThread = Exception;

type
    TThreadHnd = class(TObject)
 {{
 Related methods for thread manipulation
 }
    public
        class procedure GlobalLibLock;
        class procedure GlobalLibUnlock;
        class function AllocCurrentThreadHandle() : THandle;
    end;

type
	 TXPBaseThread = class(TThread)
	 private
		 FMaxTerminateTime : cardinal;
		 FIsAlive : boolean;
	 protected
		 procedure DoTerminate(); override;
	 public
		 procedure Terminate;
		 procedure Start; virtual;
		 procedure Resume; virtual; deprecated;
        function WaitFor(MaxTime : cardinal) : longword; overload;
        property MaxTerminateTime : cardinal read FMaxTerminateTime write FMaxTerminateTime;
        {{
        Caso o valor de MaxTerminateTime seja Windows.INFINITE o thread sera aguardado ate o seu final.
        }
        property IsAlive : boolean read FIsAlive;
    end;

    TXPNamedThread = class(TXPBaseThread)
 {{
 Sub-class of TXPBaseThread wich has a name diferenciated at debug display, used for debug only.
 }
    private
        FName : string;
        FNameInitialized : boolean;
    protected
        procedure Initialize;
        procedure SetName(const Value : string);
    public
        constructor Create(CreateSuspended : boolean); overload; virtual;
        constructor Create(CreateSuspended : boolean; const ThreadName : string); overload; virtual;
        procedure Execute; override;
        property Name : string read FName write SetName;
  {{
  The name displayed at Debug control
  }
        {1 The name displayed at Debug control }
        class procedure SetCurrentThreadName(const ThreadName : string);
    end;



    TXPNamedThreadSample = class(TXPNamedThread)
 {{
 Sub-class of TXPNamedThread wich has a name diferenciated at debug display, used for debug only.
 }
    public
        procedure Execute; override;
    end;


implementation

uses
    Math, SyncObjs;

var
    GlobalLockInstance : TCriticalSection;

{-**********************************************************************
************************************************************************
******************
******************  Class:    TXPNamedThread
******************  Category: No category
******************
************************************************************************
************************************************************************}
{{
Sub-class of TThread wich has a name diferenciated at debug display, used for debug only.
}
{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPNamedThread.Initialize;
{{
Adjuste calling SetName for this thread. This method should be called by Execute(), this is made by Execute on this class. So
sub-classes calling inherited ensure the correct way.
}
begin
    if Self.FName <> EmptyStr then begin
        Self.SetName(Self.FName);
    end else begin
        Self.SetName(Self.ClassName);
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPNamedThread.SetName(const Value : string);
{{
Changes the name for this thread.

WARNING :  this method should be executed by the self thread, not by another.
}
begin
    if not Self.FNameInitialized then begin
        if (GetCurrentThreadId() = Self.ThreadID) then begin
            FName := Value;
            SetCurrentThreadName(FName);
            Self.FNameInitialized := True;
        end else begin
            //Setavel apenas pelo proprio thread
        end;
    end else begin
        if Value <> Self.FName then begin
            raise EXPThread.CreateFmt('Nome do thread já foi inicializado para "%s"', [Self.FName]);
        end;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
constructor TXPNamedThread.Create(CreateSuspended : boolean);
{{
Constructor for TXPNamedThread to provide the name functionality.
}
begin
    Create(CreateSuspended, Self.ClassName);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
constructor TXPNamedThread.Create(CreateSuspended : boolean; const ThreadName : string);
{{
Constructor for TXPNamedThread to provide the name functionality.
}
begin
    Self.FNameInitialized := False;
    Self.FName := ThreadName;
    inherited Create(CreateSuspended);
    if not CreateSuspended then begin
        Self.SetName(ThreadName);
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPNamedThread.Execute;
{{
Foi visto na VCL que a flag Terminated ( Indicando o final da execucao ) não é automaticamente setada qdo o metodo Execute()
termina.
Assim devemos Chamar Self.Terminate no final do thread.execute para sinalizar externamente que esta thread foi finalizada.

Example:
try
  inherited;
  //Do some work thread;
finally
  Self.Terminate();
end;

***NOTA: Os descendentes SEMPRE devem chamar os ancestrais
// TODO -oRoger -cLIB : resolver essa limitacao 
}
begin
    Self.Initialize();
    inherited;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
class procedure TXPNamedThread.SetCurrentThreadName(const ThreadName : string);
{{
Changes the name for the execution thread using ThreadName.
}
var
    ThreadNameInfo : TThreadNameInfo;
begin
 {$TYPEDADDRESS OFF}
    ThreadNameInfo.FType     := $1000;
    ThreadNameInfo.FName     := PChar(ThreadName);
    ThreadNameInfo.FThreadID := GetCurrentThreadId();
    ThreadNameInfo.FFlags    := 0;
    try
        RaiseException($406D1388, 0, sizeof(ThreadNameInfo) div sizeof(longword), @ThreadNameInfo);
    except
        //Nada a ser feito segundo a documentacao da Borland
    end;
 {$TYPEDADDRESS ON}
end;

{-**********************************************************************
************************************************************************
******************
******************  Class:    TXPNamedThreadSample
******************  Category: No category
******************
************************************************************************
************************************************************************}
{{
Sub-class of TXPNamedThread wich has a name diferenciated at debug display, used for debug only.
}
{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPNamedThreadSample.Execute;
{{
This classe is a sample for use of TXPNamedThread.
}
begin
    inherited;  //***NOTA: SEMPRE deve ser chamado( VCL nao coloca outra forma de implementar isso )
    while not Self.Terminated do begin
        MessageBeep(cardinal(-1));
        Sleep(250);
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
class function TThreadHnd.AllocCurrentThreadHandle : THandle;
{{
Retorna um manipulador Handle para o thread em executor da rotina. Com este handle as rotinas de manipulação de threads que
necesitam de um handle podem usar.

NOTA: Este handle DEVE ser liberado após seu uso via CloseHandle().

Revision: 13/9/2006 - Roger  
}
begin
    if not (
        DuplicateHandle(GetCurrentProcess(), GetCurrentThread(), GetCurrentProcess(), @Result, 0, False, DUPLICATE_SAME_ACCESS)
        )
    then begin
        Result := 0;
    end;
end;

class procedure TThreadHnd.GlobalLibLock;
{{
Locks the XPLib global CriticalSession. Always take care to call this method and always call GlobalLibUnlock after make the job.
}
begin
    GlobalLockInstance.Acquire;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
class procedure TThreadHnd.GlobalLibUnlock;
{{
Unlocks the XPLib global CriticalSession. Always should be called after a call to GlobalLibLock().
}
begin
    GlobalLockInstance.Release;
end;


procedure TXPBaseThread.DoTerminate;
{{
 Indica que o thread saiu do metodo Execute normalmente( com ou sem excessao )
 }
begin
    Self.FIsAlive := False;
    inherited;
end;

procedure TXPBaseThread.Start;
begin
	 Self.FIsAlive := True;
	{$IF CompilerVersion >= 21.00}
	inherited Start;
	{$ELSE}
	inherited Resume;
	{$IFEND}
end;

procedure TXPBaseThread.Resume; deprecated;
{{
 Indica que o thread esta "vivo" a partir de agora

 Revision - 20100806 - roger
 Método deprecated - Usar Start apenas, idem para Suspend

 Revision - 20/5/2009 - roger
 Método transformado para virtual de modo que se possa preparar ambiente para a execução de classe especializada.
}
begin
	Self.Start;
end;

procedure TXPBaseThread.Terminate;
{{
Caso exista a exigencia de saida suave do thread( MaxTerminateTime > 0 ), o thread terah este tempo para finalizar-se.
A finalizacao do thread eh dado pela flag FinishedJob que sera setada apos a chamada de Self.DoTerminate.

Lembramos que o thread que chamar Terminate para finalzar outro irá ter sua responsividade comprometida pela espera do final do outro.
}
begin
    inherited; //Atualmente pela VCL apenas seta Self.Terminated para True, indicando ao thread que ele deve ser finalizado
    if ((Self.FMaxTerminateTime > 0) and (GetCurrentThreadID() <> Self.ThreadID)) then begin

		 while (Self.Suspended) do begin
		 	{$IF CompilerVersion >= 21.00}
			Self.Start;
			{$ELSE}
			 Self.Resume;
			{$IFEND}
        end;

        if (Self.FMaxTerminateTime = Windows.INFINITE) then begin
            Self.WaitFor();
        end else begin
            if (Self.FIsAlive) then begin    //DeadLocks evitados pela propria VCL
                Self.WaitFor(Self.FMaxTerminateTime);
            end;
        end;
    end else begin
        Self.WaitFor(Max(250, Self.MaxTerminateTime));
        if (Self.FIsAlive) then begin //forcar literalmente a barra
            Windows.TerminateThread(Self.Handle, WAIT_TIMEOUT);
        end;
    end;
end;

function TXPBaseThread.WaitFor(MaxTime : cardinal) : longword;
{{
Espera no maximo MaxTime milisegundos ou o fim do thread para retornar

Ver tb:
Inherited WaitFor
}
var
    H :   array[0..1] of THandle;
    WaitResult, StepTime, ElapsedTime : cardinal;
    Msg : TMsg;
begin
    StepTime := (MaxTime div 10);
    if (StepTime > 1000) then begin
        StepTime := 1000;
    end;
    H[0] := Self.Handle;
    if (GetCurrentThreadID() = MainThreadID) then begin    //Thread primario deve ser protegido em esperas segmentadas
        WaitResult := 0;
        H[1] := SyncEvent;
        ElapsedTime := 0;
        repeat
            { This prevents a potential deadlock if the background thread does a SendMessage to the foreground thread }
            if (WaitResult = WAIT_OBJECT_0 + 2) then begin
                PeekMessage(Msg, 0, 0, 0, PM_NOREMOVE);
            end;
            WaitResult := MsgWaitForMultipleObjects(2, H, False, StepTime, QS_SENDMESSAGE);
            CheckThreadError(WaitResult <> WAIT_FAILED);
            if (WaitResult = WAIT_OBJECT_0 + 1) then begin
                CheckSynchronize;
            end;
            Inc(ElapsedTime, StepTime);
        until ((WaitResult = WAIT_OBJECT_0) or (ElapsedTime >= MaxTime));
    end else begin
        WaitForSingleObject(H[0], MaxTime);//Espera o tempo simplesmente
    end;
    CheckThreadError(GetExitCodeThread(H[0], Result));
end;

initialization
    begin
        GlobalLockInstance := TCriticalSection.Create();
    end;


finalization
    begin
        GlobalLockInstance.Free;
    end;

end.
