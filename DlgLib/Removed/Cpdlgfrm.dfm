object CopyStatusFrm: TCopyStatusFrm
  Left = -1
  Top = 137
  ActiveControl = CancelBtn
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Copiando ou movendo...'
  ClientHeight = 148
  ClientWidth = 418
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'System'
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 16
  object SourceLbl: TLabel
    Left = 12
    Top = 18
    Width = 385
    Height = 16
    AutoSize = False
    Caption = 'De:'
  end
  object DestLbl: TLabel
    Left = 12
    Top = 66
    Width = 34
    Height = 16
    Caption = 'Para:'
  end
  object Gauge: TGauge
    Left = 12
    Top = 96
    Width = 265
    Height = 22
    BackColor = clSilver
    Color = clWhite
    ForeColor = clBlue
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'System'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Progress = 0
  end
  object CancelBtn: TBitBtn
    Left = 306
    Top = 91
    Width = 89
    Height = 33
    Caption = '&Cancelar'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'System'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnClick = CancelBtnClick
    Kind = bkCancel
  end
end
