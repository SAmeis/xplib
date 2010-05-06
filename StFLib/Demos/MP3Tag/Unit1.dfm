object Form1: TForm1
  Left = 248
  Top = 147
  Width = 386
  Height = 420
  Caption = 'Demo mp3Tag'
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
  object Button1: TButton
    Left = 24
    Top = 24
    Width = 97
    Height = 25
    Caption = 'Load music'
    TabOrder = 0
    OnClick = Button1Click
  end
  object MediaPlayer1: TMediaPlayer
    Left = 152
    Top = 29
    Width = 85
    Height = 20
    VisibleButtons = [btPlay, btPause, btStop]
    TabOrder = 1
  end
  object GroupBox1: TGroupBox
    Left = 24
    Top = 56
    Width = 329
    Height = 313
    Caption = ' Info: '
    TabOrder = 2
    object Label1: TLabel
      Left = 17
      Top = 19
      Width = 23
      Height = 13
      Caption = 'Title:'
    end
    object Label2: TLabel
      Left = 17
      Top = 59
      Width = 26
      Height = 13
      Caption = 'Artist:'
    end
    object Label3: TLabel
      Left = 17
      Top = 99
      Width = 32
      Height = 13
      Caption = 'Album:'
    end
    object Label4: TLabel
      Left = 17
      Top = 139
      Width = 31
      Height = 13
      Caption = 'Track:'
    end
    object Label5: TLabel
      Left = 17
      Top = 179
      Width = 25
      Height = 13
      Caption = 'Year:'
    end
    object Label6: TLabel
      Left = 17
      Top = 259
      Width = 47
      Height = 13
      Caption = 'Comment:'
    end
    object Label7: TLabel
      Left = 17
      Top = 219
      Width = 32
      Height = 13
      Caption = 'Genre:'
    end
    object Edit1: TEdit
      Left = 16
      Top = 32
      Width = 193
      Height = 21
      TabOrder = 0
    end
    object Edit2: TEdit
      Left = 16
      Top = 72
      Width = 193
      Height = 21
      TabOrder = 1
    end
    object Edit3: TEdit
      Left = 16
      Top = 112
      Width = 193
      Height = 21
      TabOrder = 2
    end
    object Edit5: TEdit
      Left = 16
      Top = 192
      Width = 193
      Height = 21
      TabOrder = 3
    end
    object Edit6: TEdit
      Left = 16
      Top = 232
      Width = 193
      Height = 21
      TabOrder = 4
    end
    object Edit4: TEdit
      Left = 16
      Top = 152
      Width = 193
      Height = 21
      TabOrder = 5
    end
    object Edit7: TEdit
      Left = 16
      Top = 272
      Width = 193
      Height = 21
      TabOrder = 6
    end
    object Button2: TButton
      Left = 224
      Top = 268
      Width = 89
      Height = 25
      Caption = 'Change tag'
      TabOrder = 7
      OnClick = Button2Click
    end
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'mp3'
    FileName = '*.mp3'
    Filter = 'mp3-files|*.mp3|All files|*.*'
    Title = 'Load mp3-file'
    Left = 312
    Top = 8
  end
  object mp3Tag1: Tmp3Tag
    TagPresent = False
    v1Tag = False
    Track = 0
    GenreID = 0
    Genre = 'Blues'
    Genres.Strings = (
      'Blues'
      'Classic Rock'
      'Country'
      'Dance'
      'Disco'
      'Funk'
      'Grunge'
      'Hip-Hop'
      'Jazz'
      'Metal'
      'New Age'
      'Oldies'
      'Other'
      'Pop'
      'R&B'
      'Rap'
      'Reggae'
      'Rock'
      'Techno'
      'Industrial'
      'Alternative'
      'Ska'
      'Death Metal'
      'Pranks'
      'Soundtrack'
      'Euro-Techno'
      'Ambient'
      'Trip-Hop'
      'Vocal'
      'Jazz+Funk'
      'Fusion'
      'Trance'
      'Classical'
      'Instrumental'
      'Acid'
      'House'
      'Game'
      'Sound Clip'
      'Gospel'
      'Noise'
      'AlternRock'
      'Bass'
      'Soul'
      'Punk'
      'Space'
      'Meditative'
      'Instrumental Pop'
      'Instrumental Rock'
      'Ethnic'
      'Gothic'
      'Darkwave'
      'Techno-Industrial'
      'Electronic'
      'Pop-Folk'
      'Eurodance'
      'Dream'
      'Southern Rock'
      'Comedy'
      'Cult'
      'Gangsta'
      'Top 40'
      'Christian Rap'
      'Pop/Funk'
      'Jungle'
      'Native American'
      'Cabaret'
      'New Wave'
      'Psychedelic'
      'Rave'
      'Showtunes'
      'Trailer'
      'Lo-Fi'
      'Tribal'
      'Acid Punk'
      'Acid Jazz'
      'Polka'
      'Retro'
      'Musical'
      'Rock & Roll'
      'Hard Rock'
      'Folk'
      'Folk/Rock'
      'National Folk'
      'Swing'
      'Fast Fusion'
      'Bebob'
      'Latin'
      'Revival'
      'Celtic'
      'Bluegrass'
      'Avantgarde'
      'Gothic Rock'
      'Progressive Rock'
      'Psychedelic Rock'
      'Symphonic Rock'
      'Slow Rock'
      'Big Band'
      'Chorus'
      'Easy Listening'
      'Acoustic'
      'Humour'
      'Speech'
      'Chanson'
      'Opera'
      'Chamber Music'
      'Sonata'
      'Symphony'
      'Booty Bass'
      'Primus'
      'Porn Groove'
      'Satire'
      'Slow Jam'
      'Club'
      'Tango'
      'Samba'
      'Folklore'
      'Ballad'
      'Power Ballad'
      'Rhythmic Soul'
      'Freestyle'
      'Duet'
      'Punk Rock'
      'Drum Solo'
      'Acapella'
      'Euro-House'
      'Dance Hall'
      'Goa'
      'Drum & Bass'
      'Club-House'
      'Hardcore'
      'Terror'
      'Indie'
      'BritPop'
      'Negerpunk'
      'Polsk Punk'
      'Beat'
      'Christian Gangs'
      'Heavy Metal'
      'Black Metal'
      'Crossover'
      'Contemporary Ch?'
      'Cristian Rock'
      'Merengue'
      'Salsa'
      'Thrash Metal'
      'Anime'
      'JPop'
      'Synthpop'
      '')
    About = 'Version 1.0, 2001'#174' Andrey V.Sorokin & Mats Asplund'
    Left = 344
    Top = 8
  end
end
