unit MainWnd;

interface

uses
    Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
    Dialogs, SimpleProgress, StdCtrls;

type
    TForm1 = class(TForm)
        SimpleProgress1 : TSimpleProgress;
        Button1 : TButton;
        procedure Button1Click(Sender : TObject);
    procedure SimpleProgress1UpdateProgress(Sender: TSimpleProgress);
    private
        { Private declarations }
    public
        { Public declarations }
        Step : Integer;
    end;

var
    Form1 : TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender : TObject);
//----------------------------------------------------------------------------------------------------------------------------------
var
    Ret : Integer;
begin
    Ret := Self.SimpleProgress1.Execute;
    MessageDlg(IntToStr(Ret), mtInformation, [mbOK], 0);
end;

procedure TForm1.SimpleProgress1UpdateProgress(Sender: TSimpleProgress);
begin
    Inc(Self.Step);
    Sender.Progress := Self.Step;
    Sender.Finished  := (Step >= Self.SimpleProgress1.MaxValue);
end;

end.
