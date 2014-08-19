{$A+,B-,C-,D-,E-,F-,G+,H+,I-,J+,K-,L-,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y-,Z1}
unit Cpdlgfrm;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, Gauges, Super;

type
  TCopyStatusFrm = class(TForm)
	 SourceLbl: TLabel;
	 DestLbl: TLabel;
	 Gauge: TGauge;
	 CancelBtn: TBitBtn;
	 procedure CancelBtnClick(Sender: TObject);
  private
	 { Private declarations }
  public
	 { Public declarations }
  end;

var
  CopyStatusFrm: TCopyStatusFrm;

implementation

{$R *.DFM}

procedure TCopyStatusFrm.CancelBtnClick(Sender: TObject);
begin
	SendMessage(Handle, UM_COPY_ERROR,0,0);
   Self.Close;
end;

end.
