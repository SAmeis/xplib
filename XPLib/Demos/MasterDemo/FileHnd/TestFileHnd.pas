unit TestFileHnd;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls;

type
  TFileHndDemoForm = class(TForm)
    EditRmDir: TLabeledEdit;
    BtnRmDir: TBitBtn;
    procedure BtnRmDirClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    class procedure Execute();
  end;

var
  FileHndDemoForm: TFileHndDemoForm;

implementation

{$R *.dfm}

uses
    FileHnd;


procedure TFileHndDemoForm.BtnRmDirClick(Sender: TObject);
var
    ret : integer;
begin
    ret:=TFileHnd.RmDir( Self.EditRmDir.Text );
    MessageDlg(SysErrorMessage(ret), mtInformation, [mbOK], 0);
end;

class procedure TFileHndDemoForm.Execute;
var
    Dlg : TFileHndDemoForm;
begin
    Application.CreateForm( TFileHndDemoForm, Dlg );
    try
        Dlg.ShowModal;
    finally
        Dlg.Free;
    end;
end;

end.
