object SimpleProgressForm: TSimpleProgressForm
  Left = 353
  Top = 194
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Progresso'
  ClientHeight = 172
  ClientWidth = 386
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Gauge: TGauge
    Left = 20
    Top = 63
    Width = 345
    Height = 36
    ForeColor = clHotLight
    Progress = 50
  end
  object DescriptionLabel: TLabel
    Left = 16
    Top = 8
    Width = 345
    Height = 49
    AutoSize = False
    Caption = 'Descri'#231#227'o'
    Layout = tlBottom
  end
  object CancelBtn: TBitBtn
    Left = 155
    Top = 125
    Width = 75
    Height = 25
    Caption = '&Cancelar'
    TabOrder = 0
    OnClick = CancelBtnClick
    Kind = bkCancel
  end
  object ProgressTimer: TTimer
    Enabled = False
    OnTimer = ProgressTimerTimer
    Left = 312
    Top = 120
  end
end
