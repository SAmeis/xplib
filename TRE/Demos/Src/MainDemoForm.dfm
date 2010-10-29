object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 467
  ClientWidth = 636
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btnLoadConfig: TBitBtn
    Left = 504
    Top = 32
    Width = 97
    Height = 41
    Caption = 'btnLoadConfig'
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 0
  end
  object btnSaveConfig: TBitBtn
    Left = 504
    Top = 104
    Width = 97
    Height = 41
    Caption = 'btnSaveConfig'
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 1
    OnClick = btnSaveConfigClick
  end
  object tvDemo: TTreeView
    Left = 24
    Top = 32
    Width = 369
    Height = 409
    AutoExpand = True
    Indent = 19
    TabOrder = 2
    Items.NodeData = {
      03010000003E0000000000000000000000FFFFFFFFFFFFFFFF00000000000000
      0002000000011052006500670069006F006E0061006C0028005400520045002D
      00500042002900260000000000000000000000FFFFFFFFFFFFFFFF0000000000
      00000003000000010452006500640065002E0000000000000000000000FFFFFF
      FFFFFFFFFF00000000000000000200000001085A006F006E0061005B005A0031
      005D00320000000000000000000000FFFFFFFFFFFFFFFF000000000000000000
      000000010A4500730074006100E700E3006F005B0031005D0032000000000000
      0000000000FFFFFFFFFFFFFFFF000000000000000000000000010A4500730074
      006100E700E3006F005B006E005D002E0000000000000000000000FFFFFFFFFF
      FFFFFF00000000000000000200000001085A006F006E0061005B005A006E005D
      00320000000000000000000000FFFFFFFFFFFFFFFF0000000000000000000000
      00010A4500730074006100E700E3006F005B0031005D00320000000000000000
      000000FFFFFFFFFFFFFFFF000000000000000000000000010A45007300740061
      00E700E3006F005B006E005D00320000000000000000000000FFFFFFFFFFFFFF
      FF000000000000000002000000010A430065006E007400720061006C005B0063
      005D00320000000000000000000000FFFFFFFFFFFFFFFF000000000000000000
      000000010A4500730074006100E700E3006F005B0031005D0032000000000000
      0000000000FFFFFFFFFFFFFFFF000000000000000000000000010A4500730074
      006100E700E3006F005B006E005D002E0000000000000000000000FFFFFFFFFF
      FFFFFF0000000000000000050000000108430065006E00740072006100690073
      00220000000000000000000000FFFFFFFFFFFFFFFF0000000000000000000000
      00010243003100220000000000000000000000FFFFFFFFFFFFFFFF0000000000
      00000000000000010243003200220000000000000000000000FFFFFFFFFFFFFF
      FF000000000000000000000000010243003300220000000000000000000000FF
      FFFFFFFFFFFFFF00000000000000000000000001024300350022000000000000
      0000000000FFFFFFFFFFFFFFFF000000000000000000000000010243003600}
  end
  object btnTestSerial: TBitBtn
    Left = 504
    Top = 168
    Width = 97
    Height = 41
    Caption = '&Test Serializer'
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 3
    OnClick = btnTestSerialClick
  end
  object btnJVXMLSerializer: TBitBtn
    Left = 504
    Top = 232
    Width = 97
    Height = 41
    Caption = '&Test JVSerializer'
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 4
    OnClick = btnJVXMLSerializerClick
  end
  object btnDEHSerializer: TBitBtn
    Left = 504
    Top = 304
    Width = 97
    Height = 41
    Caption = '&Test DEH Serializer'
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 5
    OnClick = btnDEHSerializerClick
  end
  object btnDeHLUnserialize: TBitBtn
    Left = 504
    Top = 368
    Width = 97
    Height = 41
    Caption = '&Test DEH UNSerializer'
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 6
    OnClick = btnDeHLUnserializeClick
  end
  object xmlflstrgApp: TJvAppXMLFileStorage
    StorageOptions.BooleanStringTrueValues = 'TRUE, YES, Y'
    StorageOptions.BooleanStringFalseValues = 'FALSE, NO, N'
    StorageOptions.InvalidCharReplacement = '_'
    AutoFlush = True
    FileName = 'C:\Sw\WorkDir\Pcks\TRE\Demos\Data\BaseConfig.xml'
    RootNodeName = 'Configuration'
    SubStorages = <>
    Left = 432
    Top = 176
  end
  object xmlsrlzrApp: TJvgXMLSerializer
    ExcludeEmptyValues = True
    ExcludeDefaultValues = True
    ReplaceReservedSymbols = False
    IgnoreUnknownTags = False
    WrapCollections = False
    Left = 432
    Top = 112
  end
  object xmldocSamples: TXMLDocument
    Active = True
    FileName = 'C:\Sw\WorkDir\Pcks\TRE\Demos\Data\BaseConfig.xml'
    Options = [doNodeAutoCreate, doNodeAutoIndent, doAttrNull, doAutoPrefix, doNamespaceDecl, doAutoSave]
    Left = 432
    Top = 56
    DOMVendorDesc = 'MSXML'
  end
end
