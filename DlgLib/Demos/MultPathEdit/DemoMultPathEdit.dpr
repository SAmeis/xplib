program DemoMultPathEdit;

uses
  Forms,
  MainFrm in 'MainFrm.pas' {Form1},
  PathEdit in '..\..\Src\PathEdit.pas' {PathEditorDlg},
  FileHnd in '..\..\..\XPLib\Src\FileHnd.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
