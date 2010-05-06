object AboutBox: TAboutBox
  Left = 384
  Top = 192
  BorderStyle = bsDialog
  Caption = 'About'
  ClientHeight = 277
  ClientWidth = 298
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'System'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 16
  object Label1: TLabel
    Left = 12
    Top = 136
    Width = 102
    Height = 13
    Caption = 'Windows Version:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    WordWrap = True
    IsControl = True
  end
  object Label2: TLabel
    Left = 12
    Top = 152
    Width = 77
    Height = 13
    Caption = 'DOS Version:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    WordWrap = True
    IsControl = True
  end
  object WinVersion: TLabel
    Left = 128
    Top = 136
    Width = 161
    Height = 13
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    WordWrap = True
    IsControl = True
  end
  object DosVersion: TLabel
    Left = 128
    Top = 152
    Width = 161
    Height = 13
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    WordWrap = True
    IsControl = True
  end
  object Label4: TLabel
    Left = 12
    Top = 168
    Width = 30
    Height = 13
    Caption = 'CPU:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    WordWrap = True
    IsControl = True
  end
  object CPU: TLabel
    Left = 128
    Top = 168
    Width = 162
    Height = 13
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    WordWrap = True
    IsControl = True
  end
  object Label5: TLabel
    Left = 12
    Top = 189
    Width = 77
    Height = 13
    Caption = 'Free Memory:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    WordWrap = True
    IsControl = True
  end
  object FreeMemory: TLabel
    Left = 128
    Top = 189
    Width = 162
    Height = 13
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    WordWrap = True
    IsControl = True
  end
  object Label6: TLabel
    Left = 12
    Top = 205
    Width = 94
    Height = 13
    Caption = 'Free Resources:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    WordWrap = True
    IsControl = True
  end
  object FreeResources: TLabel
    Left = 128
    Top = 205
    Width = 163
    Height = 13
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    WordWrap = True
    IsControl = True
  end
  object Label8: TLabel
    Left = 12
    Top = 105
    Width = 31
    Height = 13
    Caption = 'User:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    Visible = False
    WordWrap = True
    IsControl = True
  end
  object UserName: TLabel
    Left = 128
    Top = 105
    Width = 163
    Height = 13
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    Visible = False
    WordWrap = True
    IsControl = True
  end
  object Label10: TLabel
    Left = 12
    Top = 121
    Width = 56
    Height = 13
    Caption = 'Company:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    Visible = False
    WordWrap = True
    IsControl = True
  end
  object CompanyName: TLabel
    Left = 128
    Top = 121
    Width = 164
    Height = 13
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    Visible = False
    WordWrap = True
    IsControl = True
  end
  object Panel1: TPanel
    Left = 8
    Top = 8
    Width = 281
    Height = 93
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 0
    object ProgramIcon: TImage
      Left = 8
      Top = 8
      Width = 32
      Height = 32
      Picture.Data = {
        07544269746D617076020000424D760200000000000076000000280000002000
        0000200000000100040000000000000200000000000000000000100000000000
        000000000000000080000080000000808000800000008000800080800000C0C0
        C000808080000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
        FF00000000000000000000000000000000000EE8787878EEEEEEE03F30878EEE
        EEE00EE8787878EEEEEEE03F30878EEEEEE00EE8787878EEEEEEE03F30878EEE
        EEE00EE8787878EEEEEEE03F30878EEEEEE00887787877788888803F3088787E
        EEE00788787878878887803F3088887EEEE00788887888878887803F3088887E
        EEE00877888887788888703F308887EEEEE00888777778888888037883088888
        8EE007777777777777703787883087777EE00888888888888803787FF8830888
        888008888888888880378777778830888880077777777788037873F3F3F87808
        88E00888888888803787FFFFFFFF8830EEE00887777778800001111111111100
        EEE00888888888888899B999B99999EEEEE00888888888888899B9B99BB9B9EE
        EEE0088888888888899BB9BB99BB99EEEEE0078888888888899B999B999999EE
        EEE0087788888778899B9B9BB9BB99EEEEE00888778778888E9B9B9BB9999EEE
        EEE0088888788888EE9B99B9BB9BEEEEEEE00EE8888888EEEEE999B9999EEEEE
        EEE00EEEE888EEEEEEEE99BB999EEEEEEEE00EEEEE8EEEEEEEEEE999B9EEEEEE
        EEE00EEEEE8EEEEEEEEEEEE999EEEEEEEEE00EEEEE8EEEEEEEEEEEEE99EEEEEE
        EEE00EEEEE8EEEEEEEEEEEEE9EEEEEEEEEE00EEEEE8EEEEEEEEEEEEEEEEEEEEE
        EEE00EEEEEEEEEEEEEEEEEEEEEEEEEEEEEE00000000000000000000000000000
        0000}
      Stretch = True
      IsControl = True
    end
    object ProductName: TLabel
      Left = 56
      Top = 8
      Width = 189
      Height = 13
      AutoSize = False
      Caption = 'Product Name'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      IsControl = True
    end
    object Version: TLabel
      Left = 56
      Top = 28
      Width = 217
      Height = 13
      AutoSize = False
      Caption = 'Version'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      IsControl = True
    end
    object Copyright: TLabel
      Left = 8
      Top = 52
      Width = 265
      Height = 13
      AutoSize = False
      Caption = 'Copyright'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      IsControl = True
    end
    object Comments: TLabel
      Left = 8
      Top = 72
      Width = 265
      Height = 13
      AutoSize = False
      Caption = 'Comments'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      WordWrap = True
      IsControl = True
    end
  end
  object Button1: TButton
    Left = 118
    Top = 243
    Width = 62
    Height = 26
    Caption = 'OK'
    Default = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'System'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    OnClick = Button1Click
  end
end
