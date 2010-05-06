unit Unit1;

interface

uses
    Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
    Dialogs, StdCtrls, ExtCtrls;

type
    TForm1 = class(TForm)
        Button1 : TButton;
        LabeledEdit1 : TLabeledEdit;
        Memo1 :  TMemo;
        Memo2 :  TMemo;
        Label1 : TLabel;
        RadioGroup1 : TRadioGroup;
        CBAlternateMode : TCheckBox;
        procedure Button1Click(Sender : TObject);
    private
        { Private declarations }
    public
        { Public declarations }
    end;

var
    Form1 : TForm1;

implementation

uses CryptIni;

{$R *.dfm}

procedure TForm1.Button1Click(Sender : TObject);
var
    Teste : TCypher;
    str :   string;
begin
    Teste := TCypher.Create(LabeledEdit1.Text);
    try
        if RadioGroup1.ItemIndex = 0 then begin
            if (CBAlternateMode.Checked) then begin
                str := Teste.Encode(Memo1.Text);
            end else begin
                str := Teste.Encode(Memo1.Text);
            end;
        end else begin
            if (CBAlternateMode.Checked) then begin
                str := Teste.Decode(Memo1.Text);
            end else begin
                str := Teste.Decode(Memo1.Text);
            end;
        end;
        Memo2.Text := str;
    finally
        Teste.Free;
    end;
end;

end.
