unit DemoSettingsMainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ConfigurationLayer, StdCtrls, XMLDoc;

type
	TDemoConfig = class( TBaseConfiguration )
	public
		function GlobalReadString( const Node : string ) : string;
	end;

type
  TForm1 = class(TForm)
	 KeyNameEdit: TEdit;
	 Button1: TButton;
    ListBox: TListBox;
	 procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
	 { Private declarations }
	 Cfg : TDemoConfig;
  public
	 { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses AppSettings;


{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
//------------------------------------------------------------------------------
var
	GlobalSetting : TBaseGlobalSettings;
	XMLDoc : TXMLDocument;
	filename : string;
begin
	filename:=ExtractFilePath( Application.ExeName ) + 'SWAgent.xml';
	XMLDoc := TXMLDocument.Create( Self );
	XMLDoc.LoadFromFile( filename );
	GlobalSetting := TBaseGlobalSettings.Create( XMLDoc );
	Self.Cfg := TDemoConfig.Create( False, GlobalSetting, nil, nil, nil, nil );
end;

{ TDemoConfig }

function TDemoConfig.GlobalReadString(const Node: string): string;
//------------------------------------------------------------------------------
begin
	Result:=Self.GlobalSettings.ReadString( Node );
end;

procedure TForm1.Button1Click(Sender: TObject);
//------------------------------------------------------------------------------
var
	Lst : TStringList;
begin
	Lst:=TStringList.Create;
	try
		Self.Cfg.GlobalSettings.ListValuesNames( Self.KeyNameEdit.Text, Lst );
		ListBox.Items.AddStrings( Lst );
	finally
		Lst.Free;
	end;
end;

end.
