object DataModule1: TDataModule1
  OldCreateOrder = False
  Height = 0
  Width = 0
  object IBDatabase1: TIBDatabase
    DefaultTransaction = IBTransaction1
    IdleTimer = 0
    SQLDialect = 1
    TraceFlags = []
    Left = 312
    Top = 128
  end
  object IBQuery1: TIBQuery
    Database = IBDatabase1
    Transaction = IBTransaction1
    CachedUpdates = False
    Left = 56
    Top = 120
  end
  object IBTransaction1: TIBTransaction
    Active = False
    DefaultDatabase = IBDatabase1
    Left = 176
    Top = 120
  end
end
