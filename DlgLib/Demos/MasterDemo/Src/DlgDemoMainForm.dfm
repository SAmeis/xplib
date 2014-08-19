object DlgDemoMainFrm: TDlgDemoMainFrm
  Left = 325
  Top = 153
  Width = 870
  Height = 640
  Caption = 'Demonstra'#231#227'o do Pacote DlgLib'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 16
    Top = 16
    Width = 249
    Height = 121
    Caption = 'Password Capture'
    TabOrder = 0
    object PwdCapBtn: TButton
      Left = 16
      Top = 86
      Width = 121
      Height = 25
      Caption = 'Password Capture'
      TabOrder = 0
      OnClick = PwdCapBtnClick
    end
    object CorrectPassword: TLabeledEdit
      Left = 16
      Top = 48
      Width = 121
      Height = 21
      EditLabel.Width = 80
      EditLabel.Height = 13
      EditLabel.Caption = 'CorrectPassword'
      TabOrder = 1
      Text = 'senha'
    end
    object CaseSensitiveCheckBox: TCheckBox
      Left = 144
      Top = 52
      Width = 97
      Height = 17
      Caption = 'Case Sensitive'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
  end
  object GroupBox2: TGroupBox
    Left = 279
    Top = 16
    Width = 250
    Height = 121
    Caption = 'Login Capture'
    TabOrder = 1
    object LoginCapBtn: TButton
      Left = 144
      Top = 70
      Width = 94
      Height = 25
      Caption = 'Login Capture'
      TabOrder = 0
      OnClick = LoginCapBtnClick
    end
    object LoginUserNameEdit: TLabeledEdit
      Left = 8
      Top = 31
      Width = 121
      Height = 21
      EditLabel.Width = 50
      EditLabel.Height = 13
      EditLabel.Caption = 'UserName'
      TabOrder = 1
      Text = 'user'
    end
    object LoginCorrectPassword: TLabeledEdit
      Left = 8
      Top = 72
      Width = 121
      Height = 21
      EditLabel.Width = 80
      EditLabel.Height = 13
      EditLabel.Caption = 'CorrectPassword'
      TabOrder = 2
      Text = 'senha'
    end
  end
  object GroupBox3: TGroupBox
    Left = 15
    Top = 144
    Width = 250
    Height = 121
    Caption = 'Change Password'
    TabOrder = 2
    object ChgPwdBtn: TButton
      Left = 128
      Top = 68
      Width = 94
      Height = 25
      Caption = 'Chg Pwd Capture'
      TabOrder = 0
      OnClick = ChgPwdBtnClick
    end
    object ChgPwdOldPwdEdit: TLabeledEdit
      Left = 8
      Top = 32
      Width = 93
      Height = 21
      EditLabel.Width = 84
      EditLabel.Height = 13
      EditLabel.Caption = 'Original Password'
      TabOrder = 1
      Text = 'senha1'
    end
    object ChgPwdNewPwdEdit: TLabeledEdit
      Left = 8
      Top = 72
      Width = 93
      Height = 21
      EditLabel.Width = 71
      EditLabel.Height = 13
      EditLabel.Caption = 'New Password'
      TabOrder = 2
      Text = 'senha2'
    end
  end
  object ChgPwd: TChgPwd
    OnCheckPwdChange = ChgPwdCheckPwdChange
    Left = 232
    Top = 160
  end
  object PassDlg: TPassDlg
    Title = 'Entrada de Senha'
    MsgLabel = 'Digite sua senha:'
    CorrectPassword = 'teste'
    InitialChars = 3
    CaseSensitive = False
    OnCheckPassword = PassDlgCheckPassword
    Left = 24
    Top = 72
  end
  object LoginDlg: TLoginDlg
    UserNameCharCase = ecNormal
    OnCheckLogin = LoginDlgCheckLogin
    PasswordCharCase = ecNormal
    Title = 'Teste de login'
    Left = 455
    Top = 40
  end
end
