object LoginDlgs: TLoginDlgs
  Left = 344
  Top = 296
  ActiveControl = UserNameEdit
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Login'
  ClientHeight = 151
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
    Top = 55
    Width = 41
    Height = 13
    Caption = 'Senha:'
  end
  object Label2: TLabel
    Left = 19
    Top = 10
    Width = 48
    Height = 13
    Caption = 'Usu'#225'rio:'
  end
  object PasswordEdit: TEdit
    Left = 19
    Top = 73
    Width = 220
    Height = 21
    MaxLength = 40
    PasswordChar = '*'
    TabOrder = 1
  end
  object OKBtn: TBitBtn
    Left = 132
    Top = 109
    Width = 77
    Height = 27
    TabOrder = 2
    Kind = bkOK
    Margin = 2
    Spacing = -1
  end
  object CancelBtn: TBitBtn
    Left = 218
    Top = 109
    Width = 77
    Height = 27
    TabOrder = 3
    Kind = bkCancel
    Margin = 2
    Spacing = -1
  end
  object UserNameEdit: TEdit
    Left = 19
    Top = 31
    Width = 220
    Height = 21
    MaxLength = 40
    TabOrder = 0
  end
end
