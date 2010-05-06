object Form1: TForm1
  Left = 312
  Top = 188
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
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 24
    Top = 32
    Width = 113
    Height = 25
    Caption = 'MsgDlgButton'
    TabOrder = 0
    OnClick = Button1Click
  end
  object HKStreamButton: TButton
    Left = 25
    Top = 78
    Width = 113
    Height = 25
    Caption = 'HKStreams'
    TabOrder = 1
  end
  object Button2: TButton
    Left = 360
    Top = 48
    Width = 75
    Height = 25
    Caption = 'Lixo'
    TabOrder = 2
  end
  object ListBox1: TListBox
    Left = 376
    Top = 200
    Width = 121
    Height = 97
    ItemHeight = 13
    Items.Strings = (
      'Color'
      'DragMode')
    TabOrder = 3
  end
  object Panel1: TPanel
    Left = 216
    Top = 288
    Width = 321
    Height = 193
    Caption = 'Panel1'
    TabOrder = 4
  end
end
