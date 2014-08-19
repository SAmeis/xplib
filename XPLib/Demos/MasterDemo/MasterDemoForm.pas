unit MasterDemoForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TMasterDemoFrm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    BtnFileHnd: TButton;
    BtnWinReg: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure BtnFileHndClick(Sender: TObject);
    procedure BtnWinRegClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MasterDemoFrm: TMasterDemoFrm;

implementation

uses TestCtrlsHnd, AppLogDemoForm, TestFileHnd, WinRegDemo;

{$R *.dfm}

procedure TMasterDemoFrm.Button1Click(Sender: TObject);
//----------------------------------------------------------------------------------------------------------------------
begin
   TCtrlsHndDemoFrm.RunIt();
end;

procedure TMasterDemoFrm.Button2Click(Sender: TObject);
begin
	TAppLogDemoFrm.Execute();
end;

procedure TMasterDemoFrm.BtnFileHndClick(Sender: TObject);
begin
    TestFileHnd.TFileHndDemoForm.Execute;
end;

procedure TMasterDemoFrm.BtnWinRegClick(Sender: TObject);
begin
    TFormWinRegDemo.RunIt();
end;

end.
