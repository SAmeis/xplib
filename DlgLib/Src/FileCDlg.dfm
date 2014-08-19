object FCSimpleGaugeForm: TFCSimpleGaugeForm
  Left = 427
  Top = 264
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Executando backup imediato'
  ClientHeight = 120
  ClientWidth = 318
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 24
    Width = 47
    Height = 13
    Caption = 'Progresso'
  end
  object OperationLabel: TLabel
    Left = 8
    Top = 8
    Width = 297
    Height = 13
    AutoSize = False
  end
  object FileNameLabel: TTruncFileLabel
    Left = 9
    Top = 64
    Width = 304
    Height = 13
    ShowAccelChar = False
    TruncPosition = hpRigth
  end
  object PrgBar: TProgressBar
    Left = 10
    Top = 40
    Width = 297
    Height = 16
    TabOrder = 0
  end
  object CancelBtn: TBitBtn
    Left = 122
    Top = 80
    Width = 75
    Height = 25
    Caption = 'Cancelar'
    DoubleBuffered = True
    Kind = bkCancel
    ParentDoubleBuffered = False
    TabOrder = 1
    OnClick = CancelBtnClick
  end
end
