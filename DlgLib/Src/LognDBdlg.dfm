object LoginDBDlgForm: TLoginDBDlgForm
  Left = 359
  Top = 148
  ActiveControl = UserNameEdit
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Login'
  ClientHeight = 185
  ClientWidth = 306
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = [fsBold]
  OldCreateOrder = True
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 19
    Top = 90
    Width = 41
    Height = 13
    Caption = 'Senha:'
  end
  object Label2: TLabel
    Left = 19
    Top = 48
    Width = 48
    Height = 13
    Caption = 'Usu'#225'rio:'
  end
  object Label3: TLabel
    Left = 19
    Top = 6
    Width = 121
    Height = 13
    Caption = 'Unidade de Neg'#243'cio:'
  end
  object PasswordEdit: TEdit
    Left = 19
    Top = 108
    Width = 220
    Height = 21
    MaxLength = 40
    PasswordChar = '*'
    TabOrder = 2
  end
  object OKBtn: TBitBtn
    Left = 132
    Top = 143
    Width = 77
    Height = 27
    TabOrder = 3
    Kind = bkOK
    Margin = 2
    Spacing = -1
  end
  object CancelBtn: TBitBtn
    Left = 218
    Top = 143
    Width = 77
    Height = 27
    TabOrder = 4
    Kind = bkCancel
    Margin = 2
    Spacing = -1
  end
  object UserNameEdit: TEdit
    Left = 19
    Top = 66
    Width = 220
    Height = 21
    MaxLength = 40
    TabOrder = 1
  end
  object UnitName: TComboBox
    Left = 19
    Top = 24
    Width = 220
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    MaxLength = 40
    TabOrder = 0
  end
end
