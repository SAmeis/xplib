unit MainDemoForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, TREZones,
  Dialogs, StdCtrls, Buttons, TREConsts, TREUtils, TREConfig, ComCtrls, JvComponentBase, JvAppStorage, JvAppXMLStorage,
  JvgXMLSerializer, xmldom, XMLIntf, msxmldom, XMLDoc;

type
  TForm1 = class(TForm)
    btnLoadConfig: TBitBtn;
    btnSaveConfig: TBitBtn;
    tvDemo: TTreeView;
    xmlflstrgApp: TJvAppXMLFileStorage;
    xmlsrlzrApp: TJvgXMLSerializer;
    xmldocSamples: TXMLDocument;
    procedure btnSaveConfigClick(Sender: TObject);
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
  XMLSerializer;


{$R *.dfm}

procedure TForm1.btnSaveConfigClick(Sender: TObject);
var
   xmlStream : TFileStream;
   ret : AnsiString;
begin
   Self.xmldocSamples.Options:=Self.xmldocSamples.Options + [doAutoSave];
   Self.FRegional.Description:='TRE-PB';
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

constructor TForm1.Create(AOwner: TComponent);
begin
   inherited;
   Self.xmldocSamples.Active:=True;
   Self.FREgional := TTRERegional.Create();
end;

end.
