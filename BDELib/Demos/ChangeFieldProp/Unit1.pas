unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, DBTables, StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Table1: TTable;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses DB_Files;

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
//----------------------------------------------------------------------------------------------------------------------
var
   FieldInfo : TFieldRecInfo;
   F : TField;
begin
   Table1.Open();
   F:=Table1.FieldByName('MATRICULA');
   Fillchar(FieldInfo, SizeOf(FieldInfo), #0);
   FieldInfo.iLength:=12;
//   FieldInfo.szName:='MATRICULA';
   BDELocalChangeFieldProps( Table1, F, FieldInfo );
end;

end.
