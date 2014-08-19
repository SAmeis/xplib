object ChgPwdDlg: TChgPwdDlg
  Left = 486
  Top = 354
  ActiveControl = OKBtn
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Altera'#231#227'o de Senha'
  ClientHeight = 127
  ClientWidth = 354
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = [fsBold]
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 12
    Top = 14
    Width = 88
    Height = 16
    Caption = 'Senha Atual:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 12
    Top = 49
    Width = 90
    Height = 16
    Caption = 'Nova Senha:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label3: TLabel
    Left = 12
    Top = 86
    Width = 92
    Height = 16
    Caption = 'Confirma'#231#227'o:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object OKBtn: TBitBtn
    Left = 270
    Top = 7
    Width = 77
    Height = 27
    Caption = '&OK'
    TabOrder = 0
    Kind = bkOK
    Margin = 2
    Spacing = -1
    IsControl = True
  end
  object CancelBtn: TBitBtn
    Left = 270
    Top = 43
    Width = 77
    Height = 27
    Caption = '&Cancel'
    TabOrder = 1
    Kind = bkCancel
    Margin = 2
    Spacing = -1
    IsControl = True
  end
  object HelpBtn: TBitBtn
    Left = 270
    Top = 79
    Width = 77
    Height = 27
    TabOrder = 2
    Kind = bkHelp
    Margin = 2
    Spacing = -1
    IsControl = True
  end
  object OldPassEdit: TEdit
    Left = 114
    Top = 10
    Width = 151
    Height = 21
    PasswordChar = '*'
    TabOrder = 3
    Text = 'rtyrty r'
  end
  object NewPassEdit: TEdit
    Left = 114
    Top = 45
    Width = 151
    Height = 21
    PasswordChar = '*'
    TabOrder = 4
  end
  object ConfPassEdit: TEdit
    Left = 114
    Top = 82
    Width = 151
    Height = 21
    PasswordChar = '*'
    TabOrder = 5
  end
end
