program DemoDlgs;

uses
  Forms,
  MainForm in 'MainForm.pas' {Form1},
  MsgDlgs in '..\..\Src\MsgDlgs.pas',
  Str_pas in '..\..\Src\Str_Pas.pas',
  FileInfo in '..\..\..\IMPORTS\Super.Lib\SOURCE\2Rodada\FileInfo.pas',
  mmedia in '..\..\..\IMPORTS\Super.Lib\SOURCE\2Rodada\MMEDIA.PAS';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
