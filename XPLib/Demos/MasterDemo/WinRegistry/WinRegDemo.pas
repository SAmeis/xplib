unit WinRegDemo;

interface

uses
    Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
    Dialogs, StdCtrls, ExtCtrls;

type
    TFormWinRegDemo = class(TForm)
        EditArgSource1 : TEdit;
        Label1 : TLabel;
        RadioGroupArgumentSelector : TRadioGroup;
        EditArgSource2 : TEdit;
        Label2 : TLabel;
        BtnKeyExists : TButton;
        procedure BtnKeyExistsClick(Sender : TObject);
    private
        { Private declarations }
        function GetPrimaryArgument() : string;
    public
        { Public declarations }
        class procedure RunIt;
        property PrimaryArgument : string Read GetPrimaryArgument;
    end;

var
    FormWinRegDemo : TFormWinRegDemo;

implementation

{$R *.dfm}

uses
    WinReg32;


function TFormWinRegDemo.GetPrimaryArgument : string;
begin
    case Self.RadioGroupArgumentSelector.ItemIndex of
        0 : begin
            Result := EditArgSource1.Text;
        end;
        1 : begin
            Result := EditArgSource2.Text;
        end;
        else begin
            raise Exception.Create('Fonte do argumento não especificada.');
        end;
    end;
end;

procedure TFormWinRegDemo.BtnKeyExistsClick(Sender : TObject);
var
    Reg : TRegistryNT;
begin
    Reg := TRegistryNT.Create();
    try
        if (Reg.FullKeyExists(Self.PrimaryArgument)) then begin
            MessageDlg('Positivo', mtInformation, [mbOK], 0);
        end else begin
            MessageDlg('Negativo', mtError, [mbOK], 0);
        end;
    finally
        Reg.Free;
    end;
end;

class procedure TFormWinRegDemo.RunIt;
var
    Dlg : TFormWinRegDemo;
begin
    Application.CreateForm(TFormWinRegDemo, Dlg);
    try
        Dlg.ShowModal;
    finally
        Dlg.Free;
    end;
end;

end.
