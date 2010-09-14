{$IFDEF MainDemoForm}
	 {$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I DemoTRELib.inc}

unit MainDemoForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, TREZones,
  Dialogs, StdCtrls, Buttons, TREConsts, TREUtils, TREConfig, ComCtrls,
  xmldom, XMLIntf, msxmldom, XMLDoc, JvgXMLSerializer, JvComponentBase, JvAppStorage, JvAppXMLStorage;

type
  TForm1 = class(TForm)
    btnLoadConfig: TBitBtn;
    btnSaveConfig: TBitBtn;
    tvDemo: TTreeView;
    xmlflstrgApp: TJvAppXMLFileStorage;
    xmlsrlzrApp: TJvgXMLSerializer;
    xmldocSamples: TXMLDocument;
    btnTestSerial: TBitBtn;
    procedure btnSaveConfigClick(Sender: TObject);
	 procedure btnTestSerialClick(Sender: TObject);
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
  XmlSerial;


{$R *.dfm}

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



   {
   Self.xmlflstrgApp.WritePersistent('', Self );

   Self.xmlflstrgApp.Xml.SaveToFile( '.\jvxmlfile.xml' );


   //Self.xmlflstrgApp.WritePersistent('\Roger\teste', Self.tvDemo, True, nil );
   //xmlStream:=TFileStream.Create( '.\TestOut.xml', fmCreate );
   //Self.xmlsrlzrApp.Serialize( Self.tvDemo.Items, xmlStream  );
   //ret:=SerilializeObjectToXML( Self, [scTStrings, scTCollection, scTBitmap], [soIncludeObjectLinks, soSortProperties, soStoreParentInfo, soResetDefaultValues ] );
   //xmlStream.WriteBuffer(ret[1], Length(ret));
   }
end;

procedure TForm1.btnTestSerialClick(Sender: TObject);
var
	s : XMLSerial.TXmlSerializer<TTRERegional>;
begin
	s:=TXmlSerializer<TTRERegional>.Create();
	try
		s.Serialize( Self.xmldocSamples, Self.FRegional );
	finally
	 	s.Free;
	end;
end;

constructor TForm1.Create(AOwner: TComponent);
begin
   inherited;
   Self.xmldocSamples.Active:=True;
   Self.FREgional := TTRERegional.Create();
end;

end.
