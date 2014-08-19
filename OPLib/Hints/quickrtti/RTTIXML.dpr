program RTTIXML;

uses
  Forms,
  quicktest in 'quicktest.pas' {Form1},
  lixo in 'lixo.pas' {DataModule1: TDataModule},
  Persist in 'Persist.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TDataModule1, DataModule1);
  Application.Run;
end.
