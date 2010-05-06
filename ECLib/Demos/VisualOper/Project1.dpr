program Project1;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  EnhGrids in '..\Src\EnhGrids.pas',
  Operatrs in '..\Src\Operatrs.pas',
  AVLtrees in '..\..\OPLib\Src\AVLtrees.pas',
  TreeHnd in '..\..\OPLib\Src\TreeHnd.pas',
  ImgLHnd in '..\..\GraphLib\Src\ImgLHnd.pas',
  VCLHnd in '..\..\Super.lib\source\VCLHnd.pas',
  PasParser in '..\..\INetLib\Src\PasParser.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
