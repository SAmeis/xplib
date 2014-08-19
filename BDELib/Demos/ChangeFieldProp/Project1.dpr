program Project1;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  DB_Files in 'S:\Src\DELPHI\Pcks\Super.Lib\SOURCE\DB_Files.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
