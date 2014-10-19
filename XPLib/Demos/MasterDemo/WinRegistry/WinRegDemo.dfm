object FormWinRegDemo: TFormWinRegDemo
  Left = 212
  Top = 134
  BorderStyle = bsDialog
  Caption = 'WinReg Demo Form'
  ClientHeight = 416
  ClientWidth = 649
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 160
    Top = 26
    Width = 144
    Height = 13
    Caption = '(1) - Chave/Entrada Completa:'
  end
  object Label2: TLabel
    Left = 160
    Top = 78
    Width = 144
    Height = 13
    Caption = '(2) - Chave/Entrada Completa:'
  end
  object EditArgSource1: TEdit
    Left = 160
    Top = 42
    Width = 329
    Height = 21
    TabOrder = 0
    Text = 'HKEY_CURRENT_USER\Software\AAA\Roger\PAV\Windows'
  end
  object RadioGroupArgumentSelector: TRadioGroup
    Left = 32
    Top = 24
    Width = 113
    Height = 105
    Caption = 'Fonte de Valores'
    ItemIndex = 0
    Items.Strings = (
      '1 - Valor(1)'
      '2 - Valor(2)')
    TabOrder = 1
  end
  object EditArgSource2: TEdit
    Left = 160
    Top = 94
    Width = 329
    Height = 21
    TabOrder = 2
    Text = 'HKEY_LOCAL_MACHINE\Software\AAA\Roger\PAV\Windows'
  end
  object BtnKeyExists: TButton
    Left = 32
    Top = 160
    Width = 82
    Height = 21
    Caption = 'Chave Existe?'
    TabOrder = 3
    OnClick = BtnKeyExistsClick
  end
end
