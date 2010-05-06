program DemoThreadProgress;

uses
  Forms,
  MainWndForm in 'MainWndForm.pas' {Form1},
  XPThreads in '..\..\..\XPLib\Src\XPThreads.pas',
  ThreadProgressForm in '..\..\src\ThreadProgressForm.pas' {ThreadedSimpleProgressForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
