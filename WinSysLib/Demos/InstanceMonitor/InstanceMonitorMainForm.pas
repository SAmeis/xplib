unit InstanceMonitorMainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Mask, JvMaskEdit, JvSpin, InstanceMonitor;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    JvSpinEdit1: TJvSpinEdit;
    Label2: TLabel;
  private
	 { Private declarations }
	 instMonitor : TInstanceMonitor;
  public
	 { Public declarations }
	 constructor Create( AOwner : TComponent ); override;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{ TForm1 }

constructor TForm1.Create(AOwner: TComponent);
//----------------------------------------------------------------------------------------------------------------------
var
	ret : integer;
begin
	inherited;
	Self.instMonitor := TInstanceMonitor.Create( self );
   Self.instMonitor.Signature:='apolo';
	ret := Self.instMonitor.Scan();
	MessageDlg( IntToStr( ret ), mtInformation, [ mbOK ], 0 );
end;

end.
