unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, mp3Tag, MPlayer;

type
  TForm1 = class(TForm)
    Button1: TButton;
    OpenDialog1: TOpenDialog;
    MediaPlayer1: TMediaPlayer;
    GroupBox1: TGroupBox;
    mp3Tag1: Tmp3Tag;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit4: TEdit;
    Edit7: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
//----------------------------------------------------------------------------------------------------------------------------------
begin
	if OpenDialog1.Execute then begin
		mp3Tag1.LoadTagFromFile(OpenDialog1.FileName);
		Edit1.Text:= mp3Tag1.Title;
		Edit2.Text:= mp3Tag1.Artist;
		Edit3.Text:= mp3Tag1.Album;
		Edit4.Text:= IntToStr(mp3Tag1.Track);
		Edit5.Text:= mp3Tag1.Year;
		Edit6.Text:= mp3Tag1.Genre;
		Edit7.Text:= mp3Tag1.Comment;
		MediaPlayer1.FileName:= OpenDialog1.FileName;
		MediaPlayer1.Open;
	end;
end;

procedure TForm1.Button2Click(Sender: TObject);
//----------------------------------------------------------------------------------------------------------------------------------
begin
	if MediaPlayer1.FileName = '' then begin
		MessageDlg('There''s no mp3-file loaded !', mtWarning, [mbOK], 0);
		Exit;
	end;
	mp3Tag1.Title:= Edit1.Text;
	mp3Tag1.Artist:= Edit2.Text;
	mp3Tag1.Album:= Edit3.Text;
	mp3Tag1.Track:= StrToInt(Edit4.Text);
	mp3Tag1.Year:= Edit5.Text;
	mp3Tag1.Genre:= Edit6.Text;
	mp3Tag1.Comment:= Edit7.Text;
	MediaPlayer1.Close;
	mp3Tag1.SaveTagToFile(OpenDialog1.FileName);
	MediaPlayer1.FileName:= OpenDialog1.FileName;
	MediaPlayer1.Open;
end;

end.

