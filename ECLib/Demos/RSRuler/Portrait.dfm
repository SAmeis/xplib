object Form1: TForm1
  Left = 263
  Top = 107
  Width = 870
  Height = 640
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object PreviewGroupBox: TPanel
    Left = 0
    Top = 0
    Width = 862
    Height = 613
    Align = alClient
    TabOrder = 0
    object RsRuler4: TRsRuler
      Left = 836
      Top = 25
      Width = 25
      Height = 563
      Hint = 'TRsRuler: Direction = rdRight (Text sideways)'
      Align = alRight
      Direction = rdRight
      Units = ruCenti
      Scale = 100
      Flat = False
      ShowHint = True
    end
    object RsRuler3: TRsRuler
      Left = 1
      Top = 25
      Width = 24
      Height = 563
      Hint = 'TRsRuler: Direction = rdLeft (Text sideways)'
      Align = alLeft
      Direction = rdLeft
      Units = ruCenti
      Scale = 100
      Flat = False
      ShowHint = True
    end
    object Panel1: TPanel
      Left = 1
      Top = 1
      Width = 860
      Height = 24
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object RsRulerCorner2: TRsRulerCorner
        Left = 0
        Top = 0
        Width = 24
        Height = 24
        Hint = 'TRsRulerCorner, Position = cpLeftTop'
        Align = alLeft
        Position = cpLeftTop
        Flat = False
        Units = ruCenti
        ShowHint = True
      end
      object RsRulerCorner4: TRsRulerCorner
        Left = 836
        Top = 0
        Width = 24
        Height = 24
        Hint = 'TRsRulerCorner, Position = cpRightTop'
        Align = alRight
        Position = cpRightTop
        Flat = False
        Units = ruNone
        ShowHint = True
      end
      object RsRuler2: TRsRuler
        Left = 24
        Top = 0
        Width = 812
        Height = 24
        Hint = 'TRsRuler: Direction = rdTop'
        Align = alClient
        Direction = rdTop
        Units = ruCenti
        Scale = 100
        Flat = False
        ShowHint = True
      end
    end
    object Panel3: TPanel
      Left = 1
      Top = 588
      Width = 860
      Height = 24
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      object RsRulerCorner1: TRsRulerCorner
        Left = 0
        Top = 0
        Width = 24
        Height = 24
        Hint = 'TRsRulerCorner, Position = cpLeftBottom'
        Align = alLeft
        Position = cpLeftBottom
        Flat = False
        Units = ruNone
        ShowHint = True
      end
      object RsRulerCorner3: TRsRulerCorner
        Left = 836
        Top = 0
        Width = 24
        Height = 24
        Hint = 'TRsRulerCorner, Position = cpRightBottom'
        Align = alRight
        Position = cpRightBottom
        Flat = False
        Units = ruNone
        ShowHint = True
      end
      object RsRuler1: TRsRuler
        Left = 24
        Top = 0
        Width = 812
        Height = 24
        Hint = 'TRsRuler: Direction = rdBottom'
        Align = alClient
        Direction = rdBottom
        Units = ruCenti
        Scale = 100
        Flat = False
        ShowHint = True
      end
    end
    object LabelGrid: TDrawGrid
      Left = 25
      Top = 25
      Width = 811
      Height = 563
      Align = alClient
      Color = clBtnFace
      FixedCols = 0
      FixedRows = 0
      Options = []
      TabOrder = 2
    end
    object LabelSim: TPanel
      Left = 265
      Top = 249
      Width = 251
      Height = 114
      Color = clWhite
      TabOrder = 3
      object BorderShape: TShape
        Left = 1
        Top = 1
        Width = 249
        Height = 112
        Align = alClient
        Pen.Width = 2
        Visible = False
      end
    end
  end
end
