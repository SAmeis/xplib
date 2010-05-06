unit MainTestForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, XPThreads;

type
  TForm1 = class(TForm)
	 Button1: TButton;
	 Timer1: TTimer;
	 procedure Button1Click(Sender: TObject);
	 procedure Timer1Timer(Sender: TObject);
  private
	 { Private declarations }
	 AuxThread : TXPNamedThreadSample;
	 procedure CleanThread( Sender : TObject );
  public
	 { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
//----------------------------------------------------------------------------------------------------------------------
begin
	if not Assigned( AuxThread ) then begin
		Self.AuxThread:=TXPNamedThreadSample.Create( True );
		AuxThread.OnTerminate:=Self.CleanThread;
		AuxThread.FreeOnTerminate:=True;
		AuxThread.Resume();
		Self.Timer1.Enabled:=True;
	end else begin
		MessageDlg( 'Thread ainda ocupado', mtInformation, [ mbOK ], 0 );
	end;
end;

procedure TForm1.CleanThread(Sender: TObject);
//----------------------------------------------------------------------------------------------------------------------
begin
	Self.AuxThread:=nil;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
//----------------------------------------------------------------------------------------------------------------------
begin
	Self.Timer1.Enabled:=False;
	Self.AuxThread.Terminate();
end;

end.
