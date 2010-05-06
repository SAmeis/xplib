program NamedThread;

uses
  Forms,
  MainTestForm in 'MainTestForm.pas' {Form1},
  XPThreads in '..\..\..\Pcks.7\XPLib\Src\XPThreads.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
