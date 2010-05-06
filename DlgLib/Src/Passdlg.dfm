object PasswordDlg: TPasswordDlg
  Left = 416
  Top = 280
  ActiveControl = Pass
  BorderStyle = bsDialog
  Caption = 'Entrada de Senha'
  ClientHeight = 93
  ClientWidth = 234
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = [fsBold]
  OldCreateOrder = True
  Position = poScreenCenter
  DesignSize = (
    234
    93)
  PixelsPerInch = 96
  TextHeight = 13
  object PassMsg: TLabel
    Left = 8
    Top = 9
    Width = 96
    Height = 13
    Caption = 'Digite sua senha'
  end
  object Pass: TEdit
    Left = 8
    Top = 27
    Width = 220
    Height = 21
    PasswordChar = '*'
    TabOrder = 0
  end
  object OKBtn: TBitBtn
    Left = 66
    Top = 59
    Width = 77
    Height = 27
    Anchors = [akRight, akBottom]
    TabOrder = 1
    Kind = bkOK
    Margin = 2
    Spacing = -1
  end
  object CancelBtn: TBitBtn
    Left = 152
    Top = 59
    Width = 77
    Height = 27
    Anchors = [akRight, akBottom]
    TabOrder = 2
    Kind = bkCancel
    Margin = 2
    Spacing = -1
  end
end
