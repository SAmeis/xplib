unit quicktest;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,QuickRTTI, Buttons, ExtCtrls;

type

TContact = class  (TPersistent)
 private
  fname,faddress,fcity,fstate,fzip:String;
 published
  property Name:String read fname write fname;
  property Address:String read faddress write faddress;
  property City:String read fcity write fcity;
  property State:String read fstate write fstate;
  property Zip:String read fzip write fzip;
 end;

  TForm1 = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    Button2: TButton;
    Panel1: TPanel;
    BitBtn1: TBitBtn;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    fquick:TQuickRTTI;
    c:TContact;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
begin
{ask RTTI to build the XML on the fly!}
memo1.lines.text:=fquick.XML ;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
fquick:=TQuickRTTI.create;

{Here is my object}
c:=TContact.Create ;
c.name:='Ann Johnson';
c.Address :='424 S. Division Street'    ;
c.city:='Chenoa';
c.state:='IL';
c.zip:='61726';

{Tell QuickRTTI where to find the object}
fquick.RTTIObject :=c;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
{Ask RTTI to read the XML and populate our object!}
fquick.XML:= memo1.lines.text;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
fquick.free;
end;

procedure TForm1.BitBtn1Click(Sender: TObject);
begin
   fquick.RTTIObject:=Self.BitBtn1;   
end;

procedure TForm1.Button3Click(Sender: TObject);
var
   Writer: TWriter;
   AsciiStream, BinStream : TMemoryStream;
   OrgFormat : TStreamOriginalFormat;
begin
   AsciiStream:=TMemoryStream.Create;
   AsciiStream.SetSize( 6144 );
   try
       BinStream:=TMemoryStream.Create;
       BinStream.SetSize( 6144 );
       try
           Writer := TWriter.Create(BinStream, 2048);
           try
               Writer.WriteComponent( Self.BitBtn1 );
               BinStream.Seek(0, soFromBeginning);
               OrgFormat:=sofBinary;
               ObjectBinaryToText(BinStream, AsciiStream, OrgFormat);
           finally
               Writer.Free;
           end;
       finally
           BinStream.Seek(0,0);
           BinStream.SaveToFile( 'C:\Teste2.txt' );
           BinStream.Free;
       end;
   finally
       AsciiStream.Seek( 0,0 );
       AsciiStream.SaveToFile( 'C:\Teste.txt ');
       AsciiStream.Free;
   end;
end;

end.
