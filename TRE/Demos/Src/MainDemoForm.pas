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
    xmlflstrgApp: TJvAppXMLFileStorage;
	 xmlsrlzrApp: TJvgXMLSerializer;
    xmldocSamples: TXMLDocument;
    btnTestSerial: TBitBtn;
    btnJVXMLSerializer: TBitBtn;
    procedure btnSaveConfigClick(Sender: TObject);
	 procedure btnTestSerialClick(Sender: TObject);
    procedure btnJVXMLSerializerClick(Sender: TObject);
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

procedure TForm1.btnJVXMLSerializerClick(Sender: TObject);
var
	fs : TFileStream;
begin
	if FileExists( 'C:\Lixo.xml' ) then begin
		fs:=TFileStream.Create('C:\Lixo.xml', fmOpenReadWrite );
	end	else begin
		fs:=TFileStream.Create('C:\Lixo.xml', fmCreate );
   end;
	try
	Self.xmlsrlzrApp.Serialize( Self.tvDemo, fs );
   finally
    fs.Free();
   end;
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
	//s : XMLSerial.TXmlSerializer<TTRERegional>;
	s : TXmlTypeSerializer;
	net : TTRELocalNet;
	zone : TTREZone;
begin
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
end;

constructor TForm1.Create(AOwner: TComponent);
begin
   inherited;
   Self.xmldocSamples.Active:=True;
   Self.FREgional := TTRERegional.Create();
end;

end.
