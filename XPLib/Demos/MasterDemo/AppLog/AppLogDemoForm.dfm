object AppLogDemoFrm: TAppLogDemoFrm
  Left = 204
  Top = 339
  Caption = 'AppLog Demo'
  ClientHeight = 357
  ClientWidth = 516
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object LogMemo: TMemo
    Left = 24
    Top = 136
    Width = 233
    Height = 105
    Lines.Strings = (
      'Texto a ser logado.'
      'T++')
    TabOrder = 0
  end
  object LogBtn: TButton
    Left = 264
    Top = 136
    Width = 75
    Height = 25
    Caption = 'Log'
    TabOrder = 1
    OnClick = LogBtnClick
  end
  object CommitBtn: TButton
    Left = 264
    Top = 168
    Width = 75
    Height = 25
    Caption = 'Commit'
    Enabled = False
    TabOrder = 2
    OnClick = CommitBtnClick
  end
  object LogMessageTypeRadio: TRadioGroup
    Left = 24
    Top = 16
    Width = 121
    Height = 105
    Caption = 'Tipo'
    ItemIndex = 0
    Items.Strings = (
      'lmtError'
      'lmtInformation'
      'lmtWarning'
      'lmtDebug')
    TabOrder = 3
  end
  object BufferizeCB: TCheckBox
    Left = 161
    Top = 20
    Width = 97
    Height = 17
    Caption = 'Bufferizado'
    TabOrder = 4
    OnClick = BufferizeCBClick
  end
  object btnMultiSz: TButton
    Left = 263
    Top = 199
    Width = 75
    Height = 25
    Caption = 'btnMultiSz'
    TabOrder = 5
    OnClick = btnMultiSzClick
  end
end
