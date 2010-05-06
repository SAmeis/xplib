unit Unit1;

interface

uses
 Classes, ComCtrls, Controls, Dialogs, ExtCtrls,
 Forms, Graphics, Messages, StdCtrls, SysUtils,
 
 rjHtmlParser, rjBase;

type
 TFormMain = class (TForm)
  RichEdit: TRichEdit;
  Panel1: TPanel;
  btn_Select: TButton;
  HtmlReporter: THtmlReporter;
  GroupBox1: TGroupBox;
  chk_ASPs: TCheckBox;
  chk_Comments: TCheckBox;
  chk_Scripts: TCheckBox;
  chk_Styles: TCheckBox;
  chk_SSIs: TCheckBox;
  chk_Tags: TCheckBox;
  chk_Text: TCheckBox;
  chk_DTDs: TCheckBox;
  btn_Report: TButton;
  lbl_FileName: TLabel;
  procedure btn_SelectClick (Sender: TObject);
  function HtmlReporterReadChar: Char;
  procedure FormCreate (Sender: TObject);
  procedure chk_ASPsClick (Sender: TObject);
  procedure chk_CommentsClick (Sender: TObject);
  procedure chk_DTDsClick (Sender: TObject);
  procedure chk_ScriptsClick (Sender: TObject);
  procedure chk_SSIsClick (Sender: TObject);
  procedure chk_StylesClick (Sender: TObject);
  procedure chk_TagsClick (Sender: TObject);
  procedure chk_TextClick (Sender: TObject);
  procedure btn_ReportClick (Sender: TObject);
  procedure FormDestroy (Sender: TObject);
 private
  procedure AddColoredText (const Text: string; const Color: TColor; const FontStyle: TFontStyles);
  { Private declarations }
 public
  FFileStream: TFileStream;
  { Public declarations }
 end;
 
var
 FormMain                     : TFormMain;
 
implementation

{$R *.DFM}

function TFormMain.HtmlReporterReadChar: Char;
begin
 if FFileStream.Read (Result, 1) = 0 then Result := #0;
end;

procedure TFormMain.AddColoredText (const Text: string; const Color: TColor; const FontStyle: TFontStyles);
begin
 RichEdit.SelAttributes.Color := Color;
 RichEdit.SelAttributes.style := FontStyle;
 RichEdit.Lines.Add (Text);
end;

procedure TFormMain.FormCreate (Sender: TObject);
begin
 Application.Title := Self.Caption;
 
 chk_ASPs.Checked := HtmlReporter.ReportASPs;
 chk_Comments.Checked := HtmlReporter.ReportComments;
 chk_DTDs.Checked := HtmlReporter.ReportDTDs;
 chk_Scripts.Checked := HtmlReporter.ReportScripts;
 chk_SSIs.Checked := HtmlReporter.ReportSSIs;
 chk_Styles.Checked := HtmlReporter.ReportStyles;
 chk_Tags.Checked := HtmlReporter.ReportTags;
 chk_Text.Checked := HtmlReporter.ReportText;
end;

{ ********** }

procedure TFormMain.FormDestroy (Sender: TObject);
begin
 FreeAndNil (FFileStream);
end;

{ **************************************************************************** }

procedure TFormMain.btn_SelectClick (Sender: TObject);
var
 od                           : TOpenDialog;
begin
 od := TOpenDialog.Create (Self);
 try
  od.Filter := 'HTML-files (*.htm, *.html)|*.HTM;*.HTML';
  od.Options := [ofFileMustExist, ofHideReadOnly];
  od.Title := 'Select a HTML-file to display';
  if od.Execute then
   begin
    if Assigned (FFileStream) then FreeAndNil (FFileStream);

    try
     FFileStream := TFileStream.Create (od.FileName, fmOpenRead);
     lbl_FileName.Caption := od.FileName;
    except
     lbl_FileName.Caption := 'Error opening file.';
     FFileStream.Free;
    end;

   end;
 finally
  od.Free;
 end;
end;

{ ********** }

procedure TFormMain.btn_ReportClick (Sender: TObject);
begin
 if not Assigned (FFileStream) then Exit;
 
 RichEdit.HideScrollBars := True;
 RichEdit.HideSelection := True;
 RichEdit.Clear;
 
 FFileStream.Seek (0, soFromBeginning);
 HtmlReporter.InitOnReadCharFunc;
 
 while HtmlReporter.Next do
  begin
   case HtmlReporter.ElementType of
    eASP:
     AddColoredText ('|' + HtmlReporter.CDataString + '|', cldkGray, []);
    eTag:
     AddColoredText ('|' + HtmlReporter.Tag.Html + '|', clRed, []);
    eScript:
     begin
      AddColoredText ('|' + HtmlReporter.Tag.Html + '|', clGreen, []);
      AddColoredText ('|' + HtmlReporter.CDataString + '|', clFuchsia, []);
     end;
    eStyle:
     begin
      AddColoredText ('|' + HtmlReporter.Tag.Html + '|', clNavy, []);
      AddColoredText ('|' + HtmlReporter.CDataString + '|', clMaroon, []);
     end;
    eComment:
     AddColoredText ('|' + HtmlReporter.CDataString + '|', clNavy, [fsitalic]);
    eDTD:
     AddColoredText ('|' + HtmlReporter.CDataString + '|', clOlive, []);
    eSSI:
     AddColoredText ('|' + HtmlReporter.CDataString + '|', clMaroon, [fsbold]);
   else
    AddColoredText ('|' + HtmlReporter.CDataString + '|', clBlack, []);
   end;
  end;
 
 RichEdit.HideScrollBars := False;
 RichEdit.HideSelection := False;
end;

{ **************************************************************************** }

procedure TFormMain.chk_ASPsClick (Sender: TObject);
begin
 HtmlReporter.ReportASPs := TCheckBox (Sender).Checked;
end;

procedure TFormMain.chk_CommentsClick (Sender: TObject);
begin
 HtmlReporter.ReportComments := TCheckBox (Sender).Checked;
end;

procedure TFormMain.chk_DTDsClick (Sender: TObject);
begin
 HtmlReporter.ReportDTDs := TCheckBox (Sender).Checked;
end;

procedure TFormMain.chk_ScriptsClick (Sender: TObject);
begin
 HtmlReporter.ReportScripts := TCheckBox (Sender).Checked;
end;

procedure TFormMain.chk_SSIsClick (Sender: TObject);
begin
 HtmlReporter.ReportSSIs := TCheckBox (Sender).Checked;
end;

procedure TFormMain.chk_StylesClick (Sender: TObject);
begin
 HtmlReporter.ReportStyles := TCheckBox (Sender).Checked;
end;

procedure TFormMain.chk_TagsClick (Sender: TObject);
begin
 HtmlReporter.ReportTags := TCheckBox (Sender).Checked;
end;

procedure TFormMain.chk_TextClick (Sender: TObject);
begin
 HtmlReporter.ReportText := TCheckBox (Sender).Checked;
end;

end.

