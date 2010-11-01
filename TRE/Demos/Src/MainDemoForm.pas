{$IFDEF MainDemoForm}
	 {$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I DemoTRELib.inc}

unit MainDemoForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, TREZones,
  Dialogs, StdCtrls, Buttons, TREConsts, TREUtils, TREConfig, ComCtrls, MainDemoUtils,
  xmldom, XMLIntf, msxmldom, XMLDoc, JvgXMLSerializer, JvComponentBase, JvAppStorage, JvAppXMLStorage;

type
	TTRERegionalTest = class


   end;
  TForm1 = class(TForm)
    btnLoadConfig: TBitBtn;
    btnSaveConfig: TBitBtn;
    tvDemo: TTreeView;
	 xmldocSamples: TXMLDocument;
	 btnTestSerial: TBitBtn;
	 btnDEHSerializer: TBitBtn;
    btnDeHLUnserialize: TBitBtn;
	 procedure btnSaveConfigClick(Sender: TObject);
	 procedure btnTestSerialClick(Sender: TObject);
	 procedure btnDEHSerializerClick(Sender: TObject);
    procedure btnDeHLUnserializeClick(Sender: TObject);
  private
    { Private declarations }
	 FRegional : TTRERegional;
  public
    { Public declarations }
    constructor Create( AOwner : TComponent ); override;
  end;

var
  Form1: TForm1;

implementation

uses
  DeHL.Serialization.XML, Applog;

{$R *.dfm}

procedure TForm1.btnDeHLUnserializeClick(Sender: TObject);
var
	serial : TXMLSerializer<TTRERegional>;
	dx : TXMLDocument;
begin
	if Assigned( Self.FRegional ) then
		FreeAndNil( Self.FRegional );
	dx:=TXMLDocument.Create( Self );
	dx.FileName:='c:\lixo.xml';
	dx.Options:=dx.Options + [doAutoSave, doNodeAutoIndent ];
	dx.Active:=True;
	Self.xmldocSamples.Options:=Self.xmldocSamples.Options + [doAutoSave, doNodeAutoIndent ];

	serial:=TXMLSerializer<TTRERegional>.Create;
	{ Force fields to elements by default }
	//serial.DefaultFieldsToTags := true;
	{ Serialize the structure }
	//serial.Serialize(Self.FRegional, Self.xmldocSamples.Node );
	try
		serial.Deserialize(Self.FRegional, dx.Node );
		//Self.FRegional.Networks[0].SubNet
		MessageDlg( 'Sub-rede 0 = ' + Self.FRegional.Networks[0].SubNet, mtInformation, [ mbOK ], 0 );
		MessageDlg( 'Sub-rede 1 = ' + Self.FRegional.Networks[1].SubNet, mtInformation, [ mbOK ], 0 );
	except
		on E : Exception do begin
			TLogFile.Log( E.Message );
			raise;
		end;
	end;
end;

procedure TForm1.btnDEHSerializerClick(Sender: TObject);
var
	serial : TXMLSerializer<TTRERegional>;
	net : TTRELocalNet;
	zone : TTREZone;
	dx : TXMLDocument;
begin
	dx:=TXMLDocument.Create( Self );
	dx.FileName:='c:\lixo.xml';
	dx.Options:=dx.Options + [doAutoSave, doNodeAutoIndent ];
	dx.Active:=True;
	dx.Node.ChildNodes.Clear;
	Self.xmldocSamples.Options:=Self.xmldocSamples.Options + [doAutoSave, doNodeAutoIndent ];
	//Regional
	Self.FRegional.Description:='TRE-PB';
	//Rede de uma zona
	net:=TTRELocalNet.Create( 80 );
	Self.FRegional.AddNetwork( net );

	net:=TTRELocalNet.Create(33);
	Self.FRegional.AddNetwork( net );

	//Zona da rede 33
	zone:=TTREZone.Create( 33 );
	net.Units.Add( zone );

	serial:=TXMLSerializer<TTRERegional>.Create;
	{ Force fields to elements by default }
	//serial.DefaultFieldsToTags := true;
	{ Serialize the structure }
	//serial.Serialize(Self.FRegional, Self.xmldocSamples.Node );
	try
		serial.Serialize(Self.FRegional, dx.Node );
	except
		on E : Exception do begin
			TLogFile.Log( E.Message );
			raise;
		end;
	end;

	Self.FRegional.SaveTo( Self.xmldocSamples.DocumentElement );
end;

procedure TForm1.btnSaveConfigClick(Sender: TObject);
var
   xmlStream : TFileStream;
	ret : AnsiString;
	net : TTRELocalNet;
	zone : TTREZone;
begin
	Self.xmldocSamples.Options:=Self.xmldocSamples.Options + [doAutoSave, doNodeAutoIndent ];
	//Regional
	Self.FRegional.Description:='TRE-PB';
	//Rede de uma zona
	net:=TTRELocalNet.Create( 80 );
	Self.FRegional.AddNetwork( net );

	net:=TTRELocalNet.Create(33);
	Self.FRegional.AddNetwork( net );

	//Zona da rede 33
	zone:=TTREZone.Create( 33 );
	net.Units.Add( zone );

	Self.FRegional.SaveTo( Self.xmldocSamples.DocumentElement );
end;

procedure TForm1.btnTestSerialClick(Sender: TObject);
var
	//s : XMLSerial.TXmlSerializer<TTRERegional>;
	net : TTRELocalNet;
	zone : TTREZone;
begin
{
	//Regional
	Self.FRegional.Description:='TRE-PB';
	//Rede de uma zona
	net:=TTRELocalNet.Create( 80 );
	Self.FRegional.AddNetwork( net );

	net:=TTRELocalNet.Create(33);
	Self.FRegional.AddNetwork( net );

	//Zona da rede 33
	zone:=TTREZone.Create( 33 );
	net.Units.Add( zone );


	//s:=TXmlSerializer<TTRERegional>.Create();
	Self.xmldocSamples.XML.Clear;
	s:=TXmlTypeSerializer.Create( TypeInfo( TTRERegional ));
	try
		s.Serialize( Self.xmldocSamples, Self.FRegional );
	finally
		s.Free;
	end;
	Self.xmldocSamples.Xml.SaveToFile( '..\Data\BasicConfig.xml' );

}
end;

constructor TForm1.Create(AOwner: TComponent);
begin
   inherited;
   Self.xmldocSamples.Active:=True;
   Self.FREgional := TTRERegional.Create();
end;

end.
