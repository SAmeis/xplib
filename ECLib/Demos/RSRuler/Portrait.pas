unit Portrait;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, Grids, RsRuler;

type
  TForm1 = class(TForm)
    PreviewGroupBox: TPanel;
    Panel1: TPanel;
    RsRulerCorner2: TRsRulerCorner;
    RsRulerCorner4: TRsRulerCorner;
    RsRuler2: TRsRuler;
    Panel3: TPanel;
    RsRulerCorner1: TRsRulerCorner;
    RsRulerCorner3: TRsRulerCorner;
    RsRuler1: TRsRuler;
    RsRuler4: TRsRuler;
    LabelGrid: TDrawGrid;
    LabelSim: TPanel;
    BorderShape: TShape;
    RsRuler3: TRsRuler;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

end.
