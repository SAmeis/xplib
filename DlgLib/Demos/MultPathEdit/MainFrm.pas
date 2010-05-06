unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, PathEdit, ShellFileOper;

type
  TForm1 = class(TForm)
    MultPathEditor1: TMultPathEditor;
    Button1: TButton;
    Edit1: TEdit;
    Button2: TButton;
    Button3: TButton;
    ShellFileOperator1: TShellFileOperator;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Edit1DblClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses FileHnd;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
	Self.MultPathEditor1.Execute
end;

procedure TForm1.Button2Click(Sender: TObject);
//----------------------------------------------------------------------------------------------------------------------
begin
	Self.MultPathEditor1.Path:=Self.Edit1.Text;
end;

procedure TForm1.Button3Click(Sender: TObject);
//----------------------------------------------------------------------------------------------------------------------
begin
	Self.Edit1.Text:=Self.MultPathEditor1.Path;
end;

procedure TForm1.Edit1DblClick(Sender: TObject);
var
	VarDateTime : TDateTime;
begin
	FileTimeProperties( '', VarDateTime, VarDateTime, VarDateTime );
end;


end.
