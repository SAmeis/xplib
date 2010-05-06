unit TestCtrlsHnd;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CtrlsHnd, ExtCtrls, Buttons;

type
  TCtrlsHndDemoFrm = class(TForm)
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Panel1: TPanel;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    BitBtn1: TBitBtn;
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
  public
    { Public declarations }
    class procedure RunIt();
  end;

var
  CtrlsHndDemoFrm: TCtrlsHndDemoFrm;

implementation

{$R *.dfm}

procedure TCtrlsHndDemoFrm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
//----------------------------------------------------------------------------------------------------------------------
begin

{  //Forma 1
   if Key = VK_TAB then Exit;

   Key:=0;
   PostMessage(Self.Handle, WM_KEYDOWN, VK_TAB, 0);
   PostMessage(Self.Handle, WM_KEYUP, VK_TAB, 0);
}

   //Forma 2
   case Key of
       VK_UP : begin
           Key:=0;
           TCtrlsHnd.GotoNextControl( Self.ActiveControl, False, True );
       end;
       VK_DOWN : begin
           Key:=0;
           TCtrlsHnd.GotoNextControl( Self.ActiveControl, True, True );
       end;
       VK_RETURN : begin
           Key:=0;
           TCtrlsHnd.GotoNextControl( Self.ActiveControl, True, True );
       end;
   end;

end;

procedure TCtrlsHndDemoFrm.FormKeyPress(Sender: TObject; var Key: Char);
//----------------------------------------------------------------------------------------------------------------------
begin
   //Forma3
{
   if key = #13 then begin
      key:=#0;
      PostMessage(Self.Handle, WM_KEYDOWN, VK_TAB, 0);
   end;
   }
end;

class procedure TCtrlsHndDemoFrm.RunIt;
var
    Dlg : TCtrlsHndDemoFrm;
begin
    Application.CreateForm(TCtrlsHndDemoFrm, Dlg);
    try
        Dlg.ShowModal;
    finally
        Dlg.Free;
    end;
end;

end.

