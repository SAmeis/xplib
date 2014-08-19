object CtrlsHndDemoFrm: TCtrlsHndDemoFrm
  Left = 224
  Top = 199
  BorderStyle = bsDialog
  Caption = 'CtrlsHnd Demo'
  ClientHeight = 338
  ClientWidth = 608
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnKeyPress = FormKeyPress
  OnKeyUp = FormKeyUp
  PixelsPerInch = 96
  TextHeight = 13
  object Edit4: TEdit
    Left = 64
    Top = 136
    Width = 121
    Height = 21
    TabOrder = 0
    Text = 'Edit4'
  end
  object Edit5: TEdit
    Left = 224
    Top = 136
    Width = 121
    Height = 21
    TabOrder = 1
    Text = 'Edit5'
  end
  object Edit6: TEdit
    Left = 376
    Top = 136
    Width = 121
    Height = 21
    TabOrder = 2
    Text = 'Edit6'
  end
  object Panel1: TPanel
    Left = 56
    Top = 40
    Width = 457
    Height = 81
    Caption = 'Panel1'
    TabOrder = 3
    TabStop = True
    object Edit1: TEdit
      Left = 24
      Top = 28
      Width = 121
      Height = 21
      TabOrder = 0
      Text = 'Edit1'
    end
    object Edit2: TEdit
      Left = 160
      Top = 28
      Width = 121
      Height = 21
      TabOrder = 1
      Text = 'Edit2'
    end
    object Edit3: TEdit
      Left = 296
      Top = 28
      Width = 121
      Height = 21
      TabOrder = 2
      Text = 'Edit3'
    end
  end
  object BitBtn1: TBitBtn
    Left = 248
    Top = 200
    Width = 75
    Height = 25
    TabOrder = 4
    Kind = bkClose
  end
end
