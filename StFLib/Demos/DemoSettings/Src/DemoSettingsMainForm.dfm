object Form1: TForm1
  Left = 234
  Top = 190
  Width = 696
  Height = 480
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
  object KeyNameEdit: TEdit
    Left = 36
    Top = 44
    Width = 173
    Height = 21
    TabOrder = 0
    Text = '\NLM\VPID\Roger'
  end
  object Button1: TButton
    Left = 244
    Top = 44
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 1
    OnClick = Button1Click
  end
  object ListBox: TListBox
    Left = 36
    Top = 88
    Width = 325
    Height = 225
    ItemHeight = 13
    TabOrder = 2
  end
end
