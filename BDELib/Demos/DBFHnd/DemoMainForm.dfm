object Form1: TForm1
  Left = 327
  Top = 170
  Width = 656
  Height = 472
  Caption = 'DBFDetachIndexForm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 12
    Top = 97
    Width = 81
    Height = 13
    Caption = 'Indice Express'#227'o'
  end
  object Label2: TLabel
    Left = 279
    Top = 97
    Width = 146
    Height = 13
    Caption = 'Indice Composicao de Campos'
  end
  object MDXDetachBtn: TButton
    Left = 8
    Top = 56
    Width = 129
    Height = 25
    Caption = 'Detach MDX Index'
    TabOrder = 0
    OnClick = MDXDetachBtnClick
  end
  object OpenDBFBtn: TButton
    Left = 163
    Top = 56
    Width = 129
    Height = 25
    Caption = 'Open File'
    TabOrder = 1
    OnClick = OpenDBFBtnClick
  end
  object DBGrid1: TDBGrid
    Left = 24
    Top = 176
    Width = 577
    Height = 233
    DataSource = DataSource
    TabOrder = 2
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
  end
  object PackTableBtn: TButton
    Left = 319
    Top = 56
    Width = 129
    Height = 25
    Caption = 'Pack Table'
    TabOrder = 3
    OnClick = PackTableBtnClick
  end
  object Button1: TButton
    Left = 475
    Top = 56
    Width = 129
    Height = 25
    Caption = 'Recreate Index'
    TabOrder = 4
    OnClick = Button1Click
  end
  object IndexExpEdit: TEdit
    Left = 8
    Top = 112
    Width = 233
    Height = 21
    TabOrder = 6
    Text = 'DTOS(DIA) + TIPO + TIPO_TRAB + CIDADE'
  end
  object FieldNamesEdit: TEdit
    Left = 280
    Top = 112
    Width = 233
    Height = 21
    TabOrder = 5
    Text = 'MIN_VALUE;MAX_VALUE'
  end
  object FilenameEdit: TJvFilenameEdit
    Left = 16
    Top = 24
    Width = 433
    Height = 21
    ButtonFlat = False
    NumGlyphs = 1
    TabOrder = 7
    Text = 'Arquivo DBF'
  end
  object DBFTable: TTable
    Left = 32
    Top = 224
  end
  object DataSource: TDataSource
    DataSet = DBFTable
    Left = 104
    Top = 224
  end
end
