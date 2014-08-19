object Form1: TForm1
  Left = 288
  Top = 193
  Width = 677
  Height = 371
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefault
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 352
    Top = 144
    Width = 48
    Height = 13
    Caption = 'Resultado'
  end
  object Button1: TButton
    Left = 288
    Top = 272
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 0
    OnClick = Button1Click
  end
  object LabeledEdit1: TLabeledEdit
    Left = 264
    Top = 40
    Width = 185
    Height = 21
    EditLabel.Width = 36
    EditLabel.Height = 13
    EditLabel.Caption = 'CHAVE'
    TabOrder = 1
    Text = 'MEDUSA'
  end
  object Memo1: TMemo
    Left = 40
    Top = 160
    Width = 281
    Height = 89
    Lines.Strings = (
      'Memo1')
    TabOrder = 2
  end
  object Memo2: TMemo
    Left = 352
    Top = 160
    Width = 281
    Height = 89
    Lines.Strings = (
      'Memo2')
    ReadOnly = True
    TabOrder = 3
  end
  object RadioGroup1: TRadioGroup
    Left = 40
    Top = 24
    Width = 185
    Height = 105
    Caption = 'Queres o QUe ?'
    ItemIndex = 0
    Items.Strings = (
      'Codificar'
      'Decodificar')
    TabOrder = 4
  end
  object CBAlternateMode: TCheckBox
    Left = 264
    Top = 88
    Width = 145
    Height = 17
    Caption = 'Modo Alternativo'
    Checked = True
    State = cbChecked
    TabOrder = 5
  end
end
