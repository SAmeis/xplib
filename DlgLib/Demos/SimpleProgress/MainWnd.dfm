object Form1: TForm1
  Left = 313
  Top = 203
  Caption = 'TESTE DE COMPONENTE'
  ClientHeight = 191
  ClientWidth = 430
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 184
    Top = 120
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 0
    OnClick = Button1Click
  end
  object SimpleProgress1: TSimpleProgress
    Caption = 'Teste do Componente'
    Description = 
      'Descri'#231#227'o do teste, Ih esqueci das multriplas linhas para esta p' +
      'ropriedade'
    MaxValue = 120
    MinValue = 0
    OnUpdateProgress = SimpleProgress1UpdateProgress
    Progress = 0
    Left = 96
    Top = 32
  end
end
