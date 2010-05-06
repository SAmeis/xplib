unit HTMLDDe1;

{
  This unit demonstrates a component that allows the contents of
  database tables to be generated into HTML tables.
  Written by Keith Wood - 22 Jun 1996.
}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Spin, ExtCtrls, DB, DBTables,
  HTMLWrtr, HTMLData, Grids, DBGrids, TabNotBk;

type
  TfmDemo = class(TForm)
    HTMLWriter: THTMLWriter;
    HTMLDataSource: THTMLDataSource;
    tblParts: TTable;
    tblPartsPartNo: TFloatField;
    tblPartsVendorNo: TFloatField;
    tblPartsDescription: TStringField;
    tblPartsOnHand: TFloatField;
    tblPartsOnOrder: TFloatField;
    tblPartsCost: TCurrencyField;
    tblPartsListPrice: TCurrencyField;
    qryParts: TQuery;
    qryPartsPartNo: TFloatField;
    qryPartsVendorNo: TFloatField;
    qryPartsDescription: TStringField;
    qryPartsOnHand: TFloatField;
    qryPartsOnOrder: TFloatField;
    qryPartsCost: TCurrencyField;
    qryPartsListPrice: TCurrencyField;
    dlgColour: TColorDialog;
    TabbedNotebook1: TTabbedNotebook;
    GroupBox1: TGroupBox;
    pnlTable: TPanel;
    lblIndex: TLabel;
    cmbIndex: TComboBox;
    cbxCost: TCheckBox;
    rbTable: TRadioButton;
    pnlSQL: TPanel;
    lblDescr: TLabel;
    edDescr: TEdit;
    rbSQL: TRadioButton;
    GroupBox2: TGroupBox;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label1: TLabel;
    spnBorder: TSpinEdit;
    spnWidth: TSpinEdit;
    cbxPercent: TCheckBox;
    cbxHeaders: TCheckBox;
    spnSpacing: TSpinEdit;
    spnPadding: TSpinEdit;
    pnlBackgroundColour: TPanel;
    btnBackgroundColour: TButton;
    cbxBackgroundDefault: TCheckBox;
    pnlHeaderColour: TPanel;
    btnHeaderColour: TButton;
    cbxHeaderDefault: TCheckBox;
    edCaption: TEdit;
    btnGenerate1: TButton;
    btnGenerate2: TButton;
    tblWeb: TTable;
    tblWebWebSite: TStringField;
    tblWebURL: TStringField;
    tblWebLastVisited: TDateField;
    dsWeb: TDataSource;
    DBGrid1: TDBGrid;
    procedure FormCreate(Sender: TObject);
    procedure rbSourceClick(Sender: TObject);
    procedure btnGenerate1Click(Sender: TObject);
    procedure HTMLDataSourceRowShow(Sender: TObject;
      var ahAlignHoriz: THTMLAlignHoriz; var avAlignVert: THTMLAlignVert;
      var clrBackground, clrBorder, clrBorderLight, clrBorderDark: TColor);
    procedure HTMLDataSourceCellShow(Sender: TObject; fField: TField;
      var ahAlignHoriz: THTMLAlignHoriz; var avAlignVert: THTMLAlignVert;
      var clrBackground, clrBorder, clrBorderLight, clrBorderDark: TColor);
    procedure btnBackgroundColourClick(Sender: TObject);
    procedure btnHeaderColourClick(Sender: TObject);
    procedure btnGenerate2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmDemo: TfmDemo;

implementation

{$R *.DFM}

{ Initialise the list of indexes }
procedure TfmDemo.FormCreate(Sender: TObject);
begin
  tblParts.GetIndexNames(cmbIndex.Items);
  cmbIndex.Items.Insert(0, 'Primary key');
  cmbIndex.ItemIndex := 0;
end;

{ Dis/enable approriate options for data source }
procedure TfmDemo.rbSourceClick(Sender: TObject);
begin
  lblIndex.Enabled := rbTable.Checked;
  cmbIndex.Enabled := rbTable.Checked;
  cbxCost.Enabled := rbTable.Checked;
  lblDescr.Enabled := rbSQL.Checked;
  edDescr.Enabled := rbSQL.Checked;
  if rbTable.Checked then
    cmbIndex.SetFocus
  else
    edDescr.SetFocus;
end;

{ Set table background colour }
procedure TfmDemo.btnBackgroundColourClick(Sender: TObject);
begin
  dlgColour.Color := pnlBackgroundColour.Color;
  if dlgColour.Execute then
  begin
    pnlBackgroundColour.Color := dlgColour.Color;
    cbxBackgroundDefault.Checked := False;
  end;
end;

{ Set header background colour }
procedure TfmDemo.btnHeaderColourClick(Sender: TObject);
begin
  dlgColour.Color := pnlHeaderColour.Color;
  if dlgColour.Execute then
  begin
    pnlHeaderColour.Color := dlgColour.Color;
    cbxHeaderDefault.Checked := False;
  end;
end;

{ Generate HTML to display the contents of the PARTS table }
procedure TfmDemo.btnGenerate1Click(Sender: TObject);
const
  sInclExcl: array [Boolean] of String[10] = ('excluding', 'including');
var
  i: Integer;
begin
  try
    with HTMLWriter, HTMLDataSource do
    begin
      { Set source of data from GUI options }
      if rbTable.Checked then
      begin
        if cmbIndex.ItemIndex = 0 then  { Set table index when selected }
          tblParts.IndexName := ''
        else
          tblParts.IndexName := cmbIndex.Items[cmbIndex.ItemIndex];
        tblPartsCost.Visible := cbxCost.Checked;  { Set cost field to in/visible }
        DataSet := tblParts;
      end
      else
      begin
        with qryParts do  { Set description to search for in SQL }
        begin
          Active := False;
          ParamByName('descr').AsString := '%' + edDescr.Text + '%';
          Active := True;
        end;
        DataSet := qryParts;
      end;

      { Set HTMLDataSource display properties from GUI }
      Border := spnBorder.Value;
      if cbxPercent.Checked then
        Width := Percent(spnWidth.Value)
      else
        Width := spnWidth.Value;
      CellSpacing := spnSpacing.Value;
      CellPadding := spnPadding.Value;
      Headers := cbxHeaders.Checked;
      if cbxBackgroundDefault.Checked then
        ColourBackground := clDefault
      else
        ColourBackground := pnlBackgroundColour.Color;
      if cbxHeaderDefault.Checked then
        HeaderBackground := clDefault
      else
        HeaderBackground := pnlHeaderColour.Color;
      Caption := edCaption.Text;
      OnRowShow := HTMLDataSourceRowShow;
      OnCellShow := HTMLDataSourceCellShow;
      LinkField := nil;
      LinkTarget := nil;

      { Set up page to display table }
      Initialise;
      Title('THTMLDataSource Demonstration');
      Body;
      Heading(1, 'THTMLDataSource Demonstration', ahDefault);
      HorizRule;
      if rbTable.Checked then
        Paragraph('Below are the contents of the PARTS.DB table, ordered by index ' +
          cmbIndex.Items[cmbIndex.ItemIndex] + ' and ' + sInclExcl[cbxCost.Checked] +
          ' the ' + FormatTextEffect(efItalic, 'cost') + ' field.', ahDefault)
      else
        Paragraph('Below are the results of an SQL query on the PARTS.DB database' +
          ' for records with a description containing "' + edDescr.Text + '".',
          ahDefault);
      { Display contents of the nominated table }
      GenerateHTML;

      ParagraphStart(ahDefault);
      Text('If you have a browser that displays colours in tables, then you will' +
        ' note that the cost column (if displayed) is coloured yellow,' +
        ' and that records with less than 20 items on hand are highlighted in red.');
      Text(' This is achieved through the ' + FormatTextEffect(efItalic, 'OnRowShow') +
        ' and ' + FormatTextEffect(efItalic, 'OnCellShow') + ' events of the component.');
      ParagraphEnd;
      { Close off the HTML file }
      Finalise;
    end;

    MessageDlg('Selected data from the database has been generated as' + #13#10 +
      'HTML in ' + HTMLWriter.Filename + '. Please open this in your viewer.',
      mtInformation, [mbOK], 0);
  except
    on e: EHTMLDataSource do
      MessageDlg(e.Message, mtError, [mbOK], 0);
  end;
end;

{ Highlight rows with less than 20 items on hand }
procedure TfmDemo.HTMLDataSourceRowShow(Sender: TObject;
  var ahAlignHoriz: THTMLAlignHoriz; var avAlignVert: THTMLAlignVert;
  var clrBackground, clrBorder, clrBorderLight, clrBorderDark: TColor);
begin
  if HTMLDataSource.Dataset.FieldByName('OnHand').AsInteger < 20 then
    clrBackground := clRed;
end;

{ Highlight the cost column }
procedure TfmDemo.HTMLDataSourceCellShow(Sender: TObject; fField: TField;
  var ahAlignHoriz: THTMLAlignHoriz; var avAlignVert: THTMLAlignVert;
  var clrBackground, clrBorder, clrBorderLight, clrBorderDark: TColor);
begin
  if fField.FieldName = 'Cost' then
    clrBackground := clYellow;
end;

{ Generate HTML to display the contents of the WEBSITE table }
procedure TfmDemo.btnGenerate2Click(Sender: TObject);
begin
  try
    with HTMLWriter, HTMLDataSource do
    begin
      { Set source of data }
      DataSet := tblWeb;

      { Set HTMLDataSource display properties }
      Border := 2;
      Width := Percent(100);
      CellSpacing := 0;
      CellPadding := 0;
      Headers := True;
      ColourBackground := clDefault;
      HeaderBackground := clDefault;
      Caption := '';
      OnRowShow := nil;
      OnCellShow := nil;
      LinkField := tblWebWebSite;
      LinkTarget := tblWebURL;

      { Set up page to display table }
      Initialise;
      Title('THTMLDataSource Demonstration');
      Body;
      Heading(1, 'THTMLDataSource Demonstration', ahDefault);
      HorizRule;
      Paragraph('Below are the contents of the WEBSITE.DB table, ' +
          'showing fields being used as link hot spots and targets.', ahDefault);
      { Display contents of the nominated table }
      GenerateHTML;
      { Close off the HTML file }
      Finalise;
    end;

    MessageDlg('Selected data from the database has been generated as' + #13#10 +
      'HTML in ' + HTMLWriter.Filename + '. Please open this in your viewer.',
      mtInformation, [mbOK], 0);
  except
    on e: EHTMLDataSource do
      MessageDlg(e.Message, mtError, [mbOK], 0);
  end;
end;

end.
