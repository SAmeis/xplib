object Form1: TForm1
  Left = 216
  Top = 107
  Width = 495
  Height = 268
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
    Left = 304
    Top = 88
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Edit1: TEdit
    Left = 120
    Top = 152
    Width = 249
    Height = 21
    TabOrder = 1
    Text = 'C:\Windows;C:\Files;c:\Droga;'
    OnDblClick = Edit1DblClick
  end
  object Button2: TButton
    Left = 368
    Top = 152
    Width = 75
    Height = 25
    Caption = 'Setar'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 232
    Top = 176
    Width = 75
    Height = 25
    Caption = 'Recuperar'
    TabOrder = 3
    OnClick = Button3Click
  end
  object MultPathEditor1: TMultPathEditor
    Caption = 'Sele'#231#227'o de Caminhos'
    EntriesLimit = 4
    LabelString = 'Label dos Caminhos '
    HelpContext = 0
    Path = 'C:\Windows;c:\Files;'
    PathString.Strings = (
      'C:\Windows'
      'c:\Files')
    TextLimit = 0
    TokenChar = ';'
    Left = 248
    Top = 88
  end
  object ShellFileOperator1: TShellFileOperator
    Left = 144
    Top = 56
  end
end
