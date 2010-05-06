unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ImgList, ComCtrls, ExtCtrls, dbgrids;

type
  TForm1 = class(TForm)
    ObjLabel: TLabel;
    ExportBtn: TButton;
    Button1: TButton;
    RXExportBtn: TButton;
    GroupBox1: TGroupBox;
    ListBox1: TListBox;
    Memo: TMemo;
    TreeView1: TTreeView;
    ImageList1: TImageList;
    WriterBtn: TButton;
    Button2: TButton;
    procedure ExportBtnClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure RXExportBtnClick(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure WriterBtnClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TItemContainer   =   class;
  TItemElement = class( TCollectionItem )
  private
       FData: string;
  public
       constructor Create(Container : TItemContainer ; AData : string ); reintroduce;
  published
       property Data : string read FData write FData;
  end;

   TItemContainer  =   class( TCollection )
   public
       function Add( ItemData : string ) : TCollectionItem;
   end;

   TFormEsp =   class( TForm1 )
   private
       FMyPersist: TItemContainer;
   published
       property MyPersist : TItemContainer read FMyPersist write FMyPersist;
   end;


var
  Form1: TFormEsp;

implementation

{$R *.DFM}

uses
   ClassHnd, RXProps, TypInfo, IOObj;

procedure TForm1.ExportBtnClick(Sender: TObject);
//----------------------------------------------------------------------------------------------------------------------
begin
   Self.Memo.Lines.Text:=ClassHnd.ComponentToString( Self );
end;

procedure TForm1.Button1Click(Sender: TObject);
//----------------------------------------------------------------------------------------------------------------------
var
  Writer: TWriter;
  BinMS : TMemoryStream;
  TextMS : TStringStream;
  s : string;
begin
	BinMS:=TMemoryStream.Create;
	Writer := TWriter.Create(BinMS, 4096);
	try
		Writer.WriteDescendent( Self, nil );
	finally
		Writer.Free;
	end;
	TextMS:=TStringStream.Create( s );
	try
		BinMS.Seek( 0, soFromBeginning);
		ObjectBinaryToText( BinMS, TextMS );
		TextMS.Seek( 0, soFromBeginning );
		Self.Memo.Lines.Text:=TextMS.DataString;
	finally
		TextMS.Free;
	end;
	{
	 DefinProperty e DefineBinaryProperty sao usados pra ler valore a apartir de TReader e TWriter
	 Apenas componentes sao anihandos e propriedades apenas as descendentes de TPersistent
	}
end;


procedure TForm1.RXExportBtnClick(Sender: TObject);
//----------------------------------------------------------------------------------------------------------------------
var
   PS : TPropsStorage;
   SL : TStringList;
   PList : TPropInfoList;
   i : integer;
begin
   PS:=TPropsStorage.Create;
   try
       SL:=TStringList.Create;
       try
           PList:=TPropInfoList.Create( Self.ListBox1, [ tkUnknown .. tkDynArray] ); //Todos os tipos
           try
               for i:=0 to PList.Count-1 do begin
                   SL.Add( PList.Items[ i ].Name );
               end;
           finally
               PList.Free;
           end;
           PS.AObject:=Self.ListBox1;
           PS.StoreProperties( SL );
           Self.Memo.Lines.Text:=SL.Text;
       finally
           SL.Free;
       end;
   finally
       PS.Free;
   end;
end;

procedure TForm1.ListBox1DblClick(Sender: TObject);
//----------------------------------------------------------------------------------------------------------------------
begin
   MessageDlg( 'teste', mtInformation, [ mbOK ], 0 )
end;

procedure TForm1.FormCreate(Sender: TObject);
//----------------------------------------------------------------------------------------------------------------------
var
   ColItem : TItemElement;
begin
   TFormEsp(Self).MyPersist:=TItemContainer.Create( TItemElement );
   ColItem:=TItemElement( TFormEsp(Self).MyPersist.Add( 'Parametro de criacao 1' ) );
   ColItem.Data:=ColItem.Data + 'Appended - LEGAL1';
   ColItem:=TItemElement( TFormEsp(Self).MyPersist.Add( 'Parametro de criacao 2' ) );
   ColItem.Data:=ColItem.Data + 'Appended - LEGAL2';
end;

{ TItemContainer }

function TItemContainer.Add( ItemData : string ) : TCollectionItem;
//----------------------------------------------------------------------------------------------------------------------
begin
   Result:=Self.ItemClass.Create( Self );
   TItemElement( Result ).FData:=ItemData;
end;


{ TItemElement }

constructor TItemElement.Create( Container : TItemContainer ; AData : string );
//----------------------------------------------------------------------------------------------------------------------
begin
   inherited Create( Container );
   Self.FData:=AData;
end;

procedure TForm1.WriterBtnClick(Sender: TObject);
//----------------------------------------------------------------------------------------------------------------------
begin
   Self.Memo.Lines.Clear;
   Self.Memo.Lines.Text:=IOObj.ExportObjText( Self );
end;

procedure TForm1.Button2Click(Sender: TObject);
//----------------------------------------------------------------------------------------------------------------------
begin
   IOObj.ImportObjText( Self, Self.Memo.Lines.Text );
end;

initialization
//----------------------------------------------------------------------------------------------------------------------
begin
   RegisterClass( TListBox );
   RegisterClass( TLabel );
end;

finalization
//----------------------------------------------------------------------------------------------------------------------
begin
   UnRegisterClass( TListBox );
   UnRegisterClass( TLabel );
end;


end.
