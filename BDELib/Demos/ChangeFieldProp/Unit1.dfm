object Form1: TForm1
  Left = 226
  Top = 106
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
  object Button1: TButton
    Left = 344
    Top = 208
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Edit1: TEdit
    Left = 144
    Top = 112
    Width = 457
    Height = 21
    TabOrder = 1
    Text = 'S:\Utils\Internos\EB\CorrecaoBV_1&2\TESTE.dbf'
  end
  object Table1: TTable
    Exclusive = True
    TableName = 'S:\Utils\Internos\EB\CorrecaoBV_1&2\TESTE.dbf'
    Left = 112
    Top = 192
  end
end
