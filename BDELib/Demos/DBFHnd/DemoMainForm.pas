unit DemoMainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Mask, BDEHnd, DB, Grids, DBGrids, DBTables,
  JvToolEdit;

type
  TForm1 = class(TForm)
    MDXDetachBtn: TButton;
    OpenDBFBtn: TButton;
    DBFTable: TTable;
    DBGrid1: TDBGrid;
    DataSource: TDataSource;
    PackTableBtn: TButton;
    Button1: TButton;
    IndexExpEdit: TEdit;
    FieldNamesEdit: TEdit;
    FilenameEdit: TJvFilenameEdit;
    Label1: TLabel;
    Label2: TLabel;
	 procedure MDXDetachBtnClick(Sender: TObject);
	 procedure OpenDBFBtnClick(Sender: TObject);
    procedure PackTableBtnClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
	 { Private declarations }
	 procedure ActiveSelectFile( OpenTable : boolean );
  public
	 { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.ActiveSelectFile(OpenTable: boolean);
//----------------------------------------------------------------------------------------------------------------------------------
begin
	if not SameText( Self.DBFTable.TableName, Self.FilenameEdit.FileName ) then begin
		Self.DBFTable.Active:=False;
		Self.DBFTable.TableName:=Self.FilenameEdit.FileName;
	end;
	Self.DBFTable.Active:=OpenTable;
end;

procedure TForm1.MDXDetachBtnClick(Sender: TObject);
//----------------------------------------------------------------------------------------------------------------------------------
var
	OldState : boolean;
	Ret : integer;
begin
	OldState:=Self.DBFTable.Active;
	Self.DBFTable.Active:=False;
	if FileExists( FilenameEdit.FileName ) then begin
		Ret:=BDEDBFDetachMDXIndexFile( FilenameEdit.FileName );
		MessageDlg( SysErrorMessage( Ret ), mtInformation, [ mbOK ], 0 );
	end else begin
		MessageDlg( Format( 'Arquivo "%s" não encontrado.', [ FilenameEdit.FileName ] ), mtError, [ mbOK ], 0 );
	end;
	Self.ActiveSelectFile( OldState );
end;

procedure TForm1.OpenDBFBtnClick(Sender: TObject);
//----------------------------------------------------------------------------------------------------------------------------------
begin
	Self.ActiveSelectFile( True );
end;


procedure TForm1.PackTableBtnClick(Sender: TObject);
//----------------------------------------------------------------------------------------------------------------------------------
begin
	Self.ActiveSelectFile( False );
	BDEPackTable( Self.DBFTable );
	Self.ActiveSelectFile( True );
end;

procedure TForm1.Button1Click(Sender: TObject);
//----------------------------------------------------------------------------------------------------------------------------------
begin
	Self.DBFTable.Close;
	Self.DBFTable.Exclusive:=True;
	Self.ActiveSelectFile( True );
//	BDEDbaseExpressionCreateIndex( Self.DBFTable, 'TESTE', PChar( Self.IndexExpEdit.Text ) );
	if SameText( ExtractFileExt( Self.FilenameEdit.FileName ), '.DBF' ) then begin
		BDEDbaseFieldsCreateIndex( Self.DBFTable, 'CAMPOS', FieldNamesEdit.Text );
	end else begin
		BDEParadoxFieldsCreateIndex( Self.DBFTable, 'CAMPOS', FieldNamesEdit.Text );
	end;
	Self.DBFTable.IndexName:='TESTE';
end;

end.

