unit UDemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons, UPlasmaForm;

type
  TForm1 = class(TForm)
    PlasmaForm1: TPlasmaForm;
    BitBtn1: TBitBtn;
    procedure BitBtn1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses UPlasmaEd;

{$R *.DFM}

procedure TForm1.BitBtn1Click(Sender: TObject);
begin
close;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
var gap:integer;
begin
gap:=1;
while Top>-height do
      begin
      Top:=Top-gap;
      gap:=gap+1;
      Application.ProcessMessages;
      end;
end;

end.
