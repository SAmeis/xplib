unit Unit1;

interface

uses
 Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
 StdCtrls, ExtCtrls,
 
 rjHtmlParser, rjHtmlTag, rjHtmlTags, rjHtmlAttribs;

type
 TFormMain = class (TForm)
  HtmlReporter: THtmlReporter;
  Panel2: TPanel;
  Memo_Source: TMemo;
  Panel3: TPanel;
  btn_SelectFile: TButton;
  lbl_FileName: TLabel;
  Panel4: TPanel;
  Memo_Links: TMemo;
  Panel1: TPanel;
  GroupBox1: TGroupBox;
  chk_Th: TCheckBox;
  chk_IFrame: TCheckBox;
  chk_Frame: TCheckBox;
  chk_Td: TCheckBox;
  chk_Body: TCheckBox;
  chk_Table: TCheckBox;
  chk_Area: TCheckBox;
  chk_Link: TCheckBox;
  chk_Applet: TCheckBox;
  chk_Img: TCheckBox;
  chk_A: TCheckBox;
  chk_Input: TCheckBox;
  Splitter1: TSplitter;
  btn_Parse: TButton;
  procedure btn_SelectFileClick (Sender: TObject);
  procedure btn_ParseClick (Sender: TObject);
  procedure FormCreate (Sender: TObject);
  procedure chk_TagsClick (Sender: TObject);
 private
  { Private declarations }
 public
  { Public declarations }
 end;
 
var
 FormMain                     : TFormMain;
 
implementation

{$R *.DFM}

procedure TFormMain.btn_SelectFileClick (Sender: TObject);
begin
 with TOpenDialog.Create (Self) do
 try
  Filter := 'HTML-files (*.htm, *.html)|*.HTM;*.HTML';
  Options := [ofFileMustExist, ofHideReadOnly];
  Title := 'Select a HTML-file';
  if Execute then
   begin
    lbl_FileName.Caption := FileName;
    Memo_Source.Lines.LoadFromFile (FileName);
   end;
 finally
  Free;
 end;
end;

procedure TFormMain.btn_ParseClick (Sender: TObject);
const
 TagLinkAttribList            : array[0..12] of TTagAttrib = (
  (Tag: tagA; Attrib: attribHREF),
  (Tag: tagAPPLET; Attrib: attribCODE),
  (Tag: tagAREA; Attrib: attribHREF),
  (Tag: tagBODY; Attrib: attribBACKGROUND),
  (Tag: tagFRAME; Attrib: attribSRC),
  (Tag: tagIFRAME; Attrib: attribSRC),
  (Tag: tagINPUT; Attrib: attribSRC),
  (Tag: tagIMG; Attrib: attribSRC),
  (Tag: tagIMG; Attrib: attribLowSrc),
  (Tag: tagLINK; Attrib: attribHREF),
  (Tag: tagTABLE; Attrib: attribBACKGROUND),
  (Tag: tagTD; Attrib: attribBACKGROUND),
  (Tag: tagTH; Attrib: attribBACKGROUND)
  );
var
 a, i                         : Integer;
begin
 Memo_Links.Clear;
 Memo_Links.Perform (WM_SETREDRAW, 0, 0);
 
 HtmlReporter.SourceString := Memo_Source.Text;
 
 while HtmlReporter.Next do
  begin
   i := Low (TagLinkAttribList);
   while i <= High (TagLinkAttribList) do
    begin
     if HtmlReporter.Tag.Tag = TagLinkAttribList[i].Tag then
      begin
       repeat
        a := HtmlReporter.Tag.IndexOfAttrib (TagLinkAttribList[i].Attrib);
        if a >= 0 then Memo_Links.Lines.Add (HtmlReporter.Tag.Values[a]);
        Inc (i);
       until (i > High (TagLinkAttribList)) or
        (HtmlReporter.Tag.Tag <> TagLinkAttribList[i].Tag);
       Continue;
      end
     else
      Inc (i);
    end;
  end;
 
 Memo_Links.Perform (WM_SETREDRAW, 1, 0);
end;

procedure TFormMain.FormCreate (Sender: TObject);
begin
 Application.Title := Self.Caption;
 
 HtmlReporter.ReportTagsFilter ([tagA, tagAPPLET, tagAREA, tagBODY, tagFRAME, tagIFRAME, tagINPUT, tagIMG, tagLINK, tagTABLE, tagTD, tagTH], True);
 
 chk_A.Checked := HtmlReporter.ReportTagFilter[tagA];
 chk_Applet.Checked := HtmlReporter.ReportTagFilter[tagAPPLET];
 chk_Area.Checked := HtmlReporter.ReportTagFilter[tagAREA];
 chk_Body.Checked := HtmlReporter.ReportTagFilter[tagBODY];
 chk_Frame.Checked := HtmlReporter.ReportTagFilter[tagFRAME];
 chk_IFrame.Checked := HtmlReporter.ReportTagFilter[tagIFRAME];
 chk_Input.Checked := HtmlReporter.ReportTagFilter[tagINPUT];
 chk_Img.Checked := HtmlReporter.ReportTagFilter[tagIMG];
 chk_Link.Checked := HtmlReporter.ReportTagFilter[tagLINK];
 chk_Table.Checked := HtmlReporter.ReportTagFilter[tagTABLE];
 chk_Td.Checked := HtmlReporter.ReportTagFilter[tagTD];
 chk_Th.Checked := HtmlReporter.ReportTagFilter[tagTH];
 
 chk_A.Tag := tagA;
 chk_Applet.Tag := tagAPPLET;
 chk_Area.Tag := tagAREA;
 chk_Body.Tag := tagBODY;
 chk_Frame.Tag := tagFRAME;
 chk_IFrame.Tag := tagIFRAME;
 chk_Input.Tag := tagINPUT;
 chk_Img.Tag := tagIMG;
 chk_Link.Tag := tagLINK;
 chk_Table.Tag := tagTABLE;
 chk_Td.Tag := tagTD;
 chk_Th.Tag := tagTH;
end;

procedure TFormMain.chk_TagsClick (Sender: TObject);
begin
 HtmlReporter.ReportTagFilter[TCheckBox (Sender).Tag] := TCheckBox (Sender).Checked;
end;

end.

