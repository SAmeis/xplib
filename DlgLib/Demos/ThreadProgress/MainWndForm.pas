unit MainWndForm;

interface

uses
    Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, StdCtrls, Dialogs, ThreadProgressForm;

type
    TForm1 = class(TForm)
        Button1 : TButton;
    MemoResult: TMemo;
        procedure Button1Click(Sender : TObject);
    private
        { Private declarations }
        procedure ThreadCollectResult( Sender : TObject );
    public
        { Public declarations }
    end;

    TTestThread = class(TProgressThread)
    private
        FCurrentValue : int64;
        FMaxValue : int64;
        FMinValue : int64;
        FStep : Integer;
    protected
        function GetCurrentValue : int64; override;
        {1 Método para leitura do valor corrente do progresso do thread. }
        function GetMaxValue : int64; override;
        {1 Método para leitura do valor máximo do progresso do thread. }
        function GetMinValue : int64; override;
        {1 Método para leitura do valor mínimo do progresso do thread. }
    public
        procedure Execute; override;
    end;


var
    Form1 : TForm1;

implementation

uses Math;

{$R *.dfm}

var
    ThreadCounter : Integer = 0;

procedure TForm1.Button1Click(Sender : TObject);
var
    TestThread : TTestThread;
begin
    Inc(ThreadCounter);
    TestThread := TTestThread.Create('teste' + IntToStr(ThreadCounter));
    TestThread.ModalDialog := False;
    TestThread.FreeOnTerminate := True;
    TestThread.OnTerminate:=Self.ThreadCollectResult;
    System.Randomize();
    TestThread.UpdateInterval:=RandomRange(1, 10);
    TestThread.Resume;
end;


procedure TTestThread.Execute;
{{
Seta os valores de fronteira e entra em loop operação tartaruga.

Revision: 6/9/2006 - Roger
}
var
    LoopCount : integer;
begin
    LoopCount:=0;
    inherited;
    Self.FCurrentValue := 0;
    System.Randomize();
    Self.FMaxValue := RandomRange(100, 10000);
    Self.FMinValue := 10;
    Self.FStep := RandomRange(1, 10);
    while ( (Self.FCurrentValue < Self.FMaxValue) and ( not Self.Terminated ) ) do begin
        Inc(Self.FCurrentValue, Self.FStep);
        Sleep( 100 );
        SwitchToThread();
        if ( LoopCount > 300 ) then begin
             raise Exception.Create( 'estouro de passos.');
        end else begin
          Inc( LoopCount );
        end;
    end;
end;

function TTestThread.GetCurrentValue : int64;
begin
    Result := Self.FCurrentValue;
end;

function TTestThread.GetMaxValue : int64;
begin
    Result := Self.FMaxValue;
end;

function TTestThread.GetMinValue : int64;
begin
    Result := Self.FMinValue;
end;

procedure TForm1.ThreadCollectResult(Sender: TObject);
{{
Evento do tipo producer-consumer para a coleta dos produtos do thread.

Revision: 6/9/2006 - Roger  
}
var
    Producer : TTestThread;
begin
    Producer:=TTestThread( Sender );
    Self.MemoResult.Lines.Add( Producer.Name + '=' );
    if( Producer.FatalException <> nil )then begin
        Self.MemoResult.Lines.Add( Exception(Producer.FatalException).Message );
    end else begin
        Self.MemoResult.Lines.Add( 'OK' );
    end;
end;

end.
