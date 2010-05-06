object Form1: TForm1
  Left = 111
  Top = 38
  Width = 951
  Height = 740
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 7
    Top = 33
    Width = 866
    Height = 600
    TabOrder = 0
  end
  object Button1: TButton
    Left = 7
    Top = 7
    Width = 65
    Height = 20
    Caption = 'Obj to XML>'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 111
    Top = 7
    Width = 91
    Height = 20
    Caption = '< XML to Obj'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Panel1: TPanel
    Left = 432
    Top = 648
    Width = 145
    Height = 57
    Caption = 'Panel1'
    TabOrder = 3
    object BitBtn1: TBitBtn
      Left = 38
      Top = 16
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
      Glyph.Data = {
        DE010000424DDE01000000000000760000002800000024000000120000000100
        0400000000006801000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        3333333333333333333333330000333333333333333333333333F33333333333
        00003333344333333333333333388F3333333333000033334224333333333333
        338338F3333333330000333422224333333333333833338F3333333300003342
        222224333333333383333338F3333333000034222A22224333333338F338F333
        8F33333300003222A3A2224333333338F3838F338F33333300003A2A333A2224
        33333338F83338F338F33333000033A33333A222433333338333338F338F3333
        0000333333333A222433333333333338F338F33300003333333333A222433333
        333333338F338F33000033333333333A222433333333333338F338F300003333
        33333333A222433333333333338F338F00003333333333333A22433333333333
        3338F38F000033333333333333A223333333333333338F830000333333333333
        333A333333333333333338330000333333333333333333333333333333333333
        0000}
      NumGlyphs = 2
    end
  end
  object Button3: TButton
    Left = 152
    Top = 656
    Width = 75
    Height = 25
    Caption = 'WriteButton'
    TabOrder = 4
    OnClick = Button3Click
  end
end
