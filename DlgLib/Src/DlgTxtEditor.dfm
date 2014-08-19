object XPTxtEditForm: TXPTxtEditForm
  Left = 335
  Top = 190
  Width = 435
  Height = 308
  Caption = 'Sem t'#237'tulo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = True
  PopupMenu = PopupMenu
  Position = poDefault
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Editor: TRichEdit
    Left = 0
    Top = 0
    Width = 427
    Height = 254
    Align = alClient
    BorderStyle = bsNone
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object MainMenu: TMainMenu
    Left = 264
    Top = 64
    object MenuFile: TMenuItem
      Caption = '&Arquivo'
      object MenuNew: TMenuItem
        Caption = '&Novo'
        OnClick = MenuNewClick
      end
      object MenuOpen: TMenuItem
        Caption = '&Abrir...'
        OnClick = MenuOpenClick
      end
      object MenuClose: TMenuItem
        Caption = '&Fechar'
        OnClick = MenuCloseClick
      end
      object MenuSave: TMenuItem
        Caption = '&Salvar'
        OnClick = MenuSaveClick
      end
      object MenuSaveAs: TMenuItem
        Caption = 'Salvar &Como...'
        OnClick = MenuSaveAsClick
      end
      object MenuPrint: TMenuItem
        Caption = '&Imprimir'
        OnClick = MenuPrintClick
      end
      object MenuPrinterSetup: TMenuItem
        Caption = 'Con&figurar Impressora...'
        OnClick = MenuPrinterSetupClick
      end
      object MenuSeparator2: TMenuItem
        Caption = '-'
      end
      object MenuExit: TMenuItem
        Caption = '&Sair'
        OnClick = MenuExitClick
      end
    end
    object MenuEdit: TMenuItem
      Caption = '&Editar'
      GroupIndex = 1
      OnClick = MenuEditClick
      object MenuCut: TMenuItem
        Caption = 'Recor&tar'
        ShortCut = 16472
        OnClick = MenuCutClick
      end
      object MenuCopy: TMenuItem
        Caption = '&Copiar'
        ShortCut = 16451
        OnClick = MenuCopyClick
      end
      object MenuPaste: TMenuItem
        Caption = 'Co&lar'
        ShortCut = 16470
        OnClick = MenuPasteClick
      end
      object MenuDeleteText: TMenuItem
        Caption = '&Apagar'
        ShortCut = 16452
        OnClick = MenuDeleteTextClick
      end
      object MenuSeparator3: TMenuItem
        Caption = '-'
      end
      object MenuSelectall: TMenuItem
        Caption = '&Selecionar tudo'
        ShortCut = 16449
        OnClick = MenuSelectallClick
      end
    end
    object MenuCharacter: TMenuItem
      Caption = '&Caracteres'
      GroupIndex = 1
      object MenuAlignLeft: TMenuItem
        Caption = '&Esquerda'
        Checked = True
        OnClick = AlignClick
      end
      object MenuAlignRight: TMenuItem
        Caption = '&Direita'
        OnClick = AlignClick
      end
      object MenuAlignCenter: TMenuItem
        Caption = '&Centro'
        OnClick = AlignClick
      end
      object MenuSeparator4: TMenuItem
        Caption = '-'
      end
      object MenuWordWrap: TMenuItem
        Caption = '&Quebra de linha'
        Checked = True
        OnClick = MenuWordWrapClick
      end
      object MenuSeparator5: TMenuItem
        Caption = '-'
      end
      object MenuChangeFont: TMenuItem
        Caption = '&Fonte...'
        OnClick = MenuChangeFontClick
      end
    end
  end
  object PopupMenu: TPopupMenu
    OnPopup = MenuEditClick
    Left = 96
    Top = 64
    object PopMenuCut: TMenuItem
      Caption = 'Cu&t'
      OnClick = MenuCutClick
    end
    object PopMenuCopy: TMenuItem
      Caption = '&Copy'
      OnClick = MenuCopyClick
    end
    object PopMenuPaste: TMenuItem
      Caption = '&Paste'
      OnClick = MenuPasteClick
    end
  end
  object SaveFileDialog: TSaveDialog
    Filter = 
      'Rich text files (*.rtf)|*.rtf|Plain text files (*.txt)|*.txt|All' +
      ' files|*.*'
    Options = [ofHideReadOnly, ofFileMustExist, ofNoReadOnlyReturn]
    Left = 56
    Top = 64
  end
  object FontDialog: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Left = 136
    Top = 64
  end
  object PrinterSetupDialog: TPrinterSetupDialog
    Left = 176
    Top = 64
  end
  object PrintDialog: TPrintDialog
    Left = 216
    Top = 64
  end
end
