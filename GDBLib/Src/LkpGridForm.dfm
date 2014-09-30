object LookupGridForm: TLookupGridForm
  Left = 289
  Top = 203
  ActiveControl = LkpEdit
  BorderIcons = [biSystemMenu]
  Caption = 'Busca por registro'
  ClientHeight = 266
  ClientWidth = 462
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poScreenCenter
  DesignSize = (
    462
    266)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 13
    Width = 69
    Height = 13
    Caption = '&Entrada direta'
    FocusControl = LkpEdit
  end
  object DBGrid: TDBGrid
    Left = 13
    Top = 56
    Width = 445
    Height = 157
    Anchors = [akLeft, akTop, akRight, akBottom]
    DataSource = GridDataSource
    Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgRowSelect, dgConfirmDelete, dgCancelOnExit]
    ReadOnly = True
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
    OnDblClick = DBGridDblClick
  end
  object LkpEdit: TEdit
    Left = 15
    Top = 27
    Width = 266
    Height = 21
    TabOrder = 1
    OnChange = LkpEditChangeByUser
    OnClick = LkpEditChangeByUser
  end
  object Panel1: TPanel
    Left = 0
    Top = 208
    Width = 462
    Height = 58
    Align = alBottom
    Anchors = [akBottom]
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      462
      58)
    object OKBtn: TButton
      Left = 134
      Top = 26
      Width = 74
      Height = 25
      Anchors = [akBottom]
      Caption = '&OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object CancelBtn: TButton
      Left = 261
      Top = 26
      Width = 74
      Height = 25
      Anchors = [akTop, akBottom]
      Cancel = True
      Caption = '&Cancelar'
      ModalResult = 2
      TabOrder = 1
    end
    object AutoSeekCheckBox: TCheckBox
      Left = 11
      Top = 6
      Width = 142
      Height = 17
      Caption = 'Busca &Automatica'
      Checked = True
      State = cbChecked
      TabOrder = 2
      OnClick = AutoSeekCheckBoxClick
    end
    object IgnoreCaseCheckBox: TCheckBox
      Left = 195
      Top = 6
      Width = 142
      Height = 17
      Caption = '&Ignorar caixa'
      Checked = True
      State = cbChecked
      TabOrder = 3
      OnClick = AutoSeekCheckBoxClick
    end
  end
  object SeekButton: TButton
    Left = 289
    Top = 24
    Width = 75
    Height = 25
    Caption = '&Buscar'
    Enabled = False
    TabOrder = 3
    OnClick = SeekButtonClick
  end
  object GridDataSource: TDataSource
    OnDataChange = GridDataSourceDataChangeUpdate
    Left = 232
    Top = 104
  end
end
