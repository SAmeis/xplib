object FileHndDemoForm: TFileHndDemoForm
  Left = 257
  Top = 151
  Width = 870
  Height = 640
  Caption = 'FileHndDemoForm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object EditRmDir: TLabeledEdit
    Left = 24
    Top = 64
    Width = 249
    Height = 21
    EditLabel.Width = 161
    EditLabel.Height = 13
    EditLabel.Caption = 'Caminha a ser apagado via RmDir'
    TabOrder = 0
    Text = 'C:\Temp\UpSIAF'
  end
  object BtnRmDir: TBitBtn
    Left = 288
    Top = 64
    Width = 75
    Height = 25
    Caption = 'BtnRmDir'
    TabOrder = 1
    OnClick = BtnRmDirClick
  end
end
