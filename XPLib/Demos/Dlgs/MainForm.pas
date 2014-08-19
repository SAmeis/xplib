unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, HKStreamCol, HKStreamRoutines, Str_Pas, ExtCtrls;

type
  TForm1 = class(TForm)
	 Button1: TButton;
	 HKStreamButton: TButton;
    Button2: TButton;
    ListBox1: TListBox;
    Panel1: TPanel;
	 procedure Button1Click(Sender: TObject);
	 procedure FormCreate(Sender: TObject);
  private
	 { Private declarations }
	 HK : THKStreams;
  public
	 { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
	MsgDlgs;

{$R *.dfm}


procedure EnumTypeAsStrings(anEnum: PTypeInfo; theStrings: TStrings);
  var i: integer;
begin
  if(anEnum.Kind = tkEnumeration)then
  begin
    theStrings.Clear;
    for i:= GetTypeData(anEnum).MinValue to GetTypeData(anEnum).MaxValue do
    begin
      theStrings.AddObject( GetEnumName(anEnum,i), TObject(anEnum));
    end;{}
  end;{}
end;{}


procedure TForm1.Button1Click(Sender: TObject);
//----------------------------------------------------------------------------------------------------------------------
var
	Ret : TModalResult;
begin
	Ret:=MsgDlg( nil, 'teste de caption' , mtWarning,   mbYesAllNoAllCancel, mbNoToAll );
	case Ret of
		mrYes : begin
		MessageDlg('mrYes', mtInformation, [mbOK], 0);
		end;
		mrNo  : begin
			MessageDlg( 'mrNo', mtInformation, [ mbOK ], 0 );
		end;
		mrOk : begin
			MessageDlg( 'mrOk', mtInformation, [ mbOK ], 0 );
		end;
		mrCancel : begin
			MessageDlg( 'mrCancel', mtInformation, [ mbOK ], 0 );
		end;
		mrAbort : begin
			MessageDlg( 'mrAbort', mtInformation, [ mbOK ], 0 );
		end;
		mrRetry : begin
			MessageDlg( 'mrRetry', mtInformation, [ mbOK ], 0 );
		end;
		mrIgnore : begin
			MessageDlg( 'mrIgnore', mtInformation, [ mbOK ], 0 );
		end;
		mrAll : begin
			MessageDlg( 'mrAll', mtInformation, [ mbOK ], 0 );
		end;
		mrNoToAll : begin
			MessageDlg( 'mrNoToAll', mtInformation, [ mbOK ], 0 );
		end;
		else begin
			MessageDlg( 'Ignorado!!!!!!!!!', mtInformation, [ mbOK ], 0 );
		end;
	end;
end;

procedure TForm1.FormCreate(Sender: TObject);
//----------------------------------------------------------------------------------------------------------------------
begin
	HK:=THKStreams.Create( Self );
end;

end.
