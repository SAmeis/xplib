unit HTMLWrtr;

{
  This unit implements a component that allows HTML to be written
  without knowing the details of that language.
  Written by Keith Wood - 24 Feb 1996.

  The component provides functions to format text for the various HTML tags,
  and corresponding procedures to write these tags to the specified file.

  Errors and warnings are implemented as exceptions, with the user being
  able to control which types will be raised. Note that when a warning is
  raised the intended action will always be done.
}

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, Forms, Dialogs;

const
  { Unusual colour to use as no value specified - some sort of purple }
  clDefault = $00FE09F1;

  { Default filename for output }
  DefaultFilename = 'htmlwrtr.htm';

type
  { Heading levels available }
  THTMLHeadingLevel = 1..6;

  { Horizontal alignment values }
  THTMLAlignHoriz = (ahDefault, ahLeft, ahCentre, ahRight);

  { Vertical alignment values }
  THTMLAlignVert = (avDefault, avTop, avMiddle, avBottom);

  { Image alignment values }
  THTMLAlignImage = (aiDefault, aiTop, aiMiddle, aiBottom, aiLeft, aiRight);

  { Shapes for inline maps }
  THTMLShapes = (shDefault, shRect, shCircle, shPolygon);

  { Clear options for line breaks }
  THTMLClear = (crDefault, crLeft, crRight, crAll);

  { List types }
  THTMLListType = (ltUnordered, ltOrdered, ltMenu, ltDirectory, ltGlossary);

  { List items }
  THTMLListItem = (liNormal, liTerm, liDefinition);

  { List number schema }
  THTMLNumberScheme = (nsDefault, nsLargeLetters, nsSmallLetters,
    nsLargeRoman, nsSmallRoman, nsNumbers);

  { List processing }
  THTMLListCheck = record
    ListType: THTMLListType;
    Elements: Boolean;
  end;

  { Text effects }
  THTMLTextEffect = (efBold, efItalic, efUnderlined, efFixedPitch,
    efEmphasised, efStrong, efCode, efSample, efKeyboard, efCitation,
    efDefinition, efVariable, efBlockQuote, efBlink, efNonBreaking,
    efSuperscript, efSubscript, efInserted, efDeleted, efSmall,
    efBig, efAddress, efPreformat, efCentre);

  { Special characters }
  THTMLSpecialChar = (scLessThan, scGreaterThan, scAmpersand, scQuote,
    scNBSpace, scCopyright, scRegistered, scCent, scPound, scYen, scHalf,
    scQuarter, scThreeQuarter, scAELig, scAAcute, scACirc, scAGrave, scARing,
    scATilde, scAUml, scCCedil, scEth, scEAcute, scECirc, scEGrave, scEUml,
    scIAcute, scICirc, scIGrave, scIUml, scNTilde, scOAcute, scOCirc,
    scOGrave, scOSlash, scOTilde, scOUml, scThorn, scUAcute, scUCirc, scUGrave,
    scUUml, scYAcute, scaeLigL, scaAcuteL, scaCircL, scaGraveL, scaRingL,
    scaTildeL, scaUmlL, sccCedilL, scethL, sceAcuteL, sceCircL, sceGraveL,
    sceUmlL, sciAcuteL, sciCircL, sciGraveL, sciUmlL, scnTildeL, scoAcuteL,
    scoCircL, scoGraveL, scoSlashL, scoTildeL, scoUmlL, scthornL, scszLigL,
    scuAcuteL, scuCircL, scuGraveL, scuUmlL, scyAcuteL, scyUmlL);

  { Font sizes }
  THTMLFontSize = 1..7;

  { Font variations }
  THTMLFontChange = (fcAbsolute, fcSmaller, fcBigger);

  { Marquee behaviour }
  THTMLMarqueeBehave = (mbScroll, mbSlide, mbAlternate);

  { Marquee direction }
  THTMLMarqueeDirection = (mdLeft, mdRight);

  { Form methods }
  THTMLFormMethod = (fmGet, fmPost);

  { Input field types }
  THTMLInputField = (ifText, ifPassword, ifCheckbox, ifRadio, ifSubmit,
    ifReset, ifImage, ifHidden);

  { Error/warning levels }
  THTMLErrors = (erErrors, erWarnings, erNetscape, erIExplorer, erHTML3);
  THTMLErrorSet = set of THTMLErrors;

  { Tag categories - for exceptions }
  THTMLTagCategory = (tcContent, tcHead, tcTitle, tcIsIndex, tcBase, tcMeta,
    tcBody, tcHeading, tcParagraph, tcImage, tcMap, tcList, tcBreak, tcEffect,
    tcLink, tcForm, tcTable, tcFile);

const
  { Descriptions of the tag categories }
  TagCategory : array [THTMLTagCategory] of String[9] =
    ('Content', 'Head', 'Title', 'IsIndex', 'Base', 'Meta', 'Body', 'Heading',
    'Paragraph', 'Image', 'Map', 'List', 'Break', 'Effect', 'Link', 'Form',
    'Table', 'File');

type
  { Field value for dynamic insertion of values into HTML templates }
  THTMLFieldValue = class(TObject)
  private
    { Private declarations }
    FFieldValue: String;
  public
    { Public declarations }
    constructor Create(sValue: String);
    property FieldValue: String read FFieldValue write FFieldValue;
  end;

  { Field/value pairs for dynamic insertion of values into HTML templates }
  THTMLDictionary = class(TStringList)
  public
    { Public declarations }
    procedure AddFieldAndValue(sField, sValue: String);
    function GetValue(sField: String): String;
  end;

  { The HTML writer class that allows HTML to be generated }
  THTMLWriter = class(TComponent)
  private
    { Private declarations }
    FFilename: String;
    FErrors: THTMLErrorSet;
    FIncludeMIMEType: Boolean;
    FVersion: String;
    fOutput: TextFile;
    bWrittenContent, bWrittenHead, bInHead,
    bWrittenTitle, bWrittenIsIndex, bWrittenBase,
    bInBody, bInForm, bInSelect, bSelectOption, bInTextArea, bSubmit,
    bInLink, bInMap, bWrittenArea, bBulkInsert: Boolean;
    iCurHeadingLevel, iLastHeadingLevel,
    iTableLevels, iCurList: Byte;
    recListCheck: array [1..20] of THTMLListCheck;
    sTags: TStringList;

    procedure SetFilename(sFilename: String);
    procedure CheckNesting( tcTag: THTMLTagCategory; bAllowedInHead, bAllowedInBody, bAllowedInMap, bAllowedInTable,
                            bAllowedInForm: Boolean);
    procedure CheckClosing(tcTag: THTMLTagCategory; bNotThere: Boolean; sTag, sText: String);
    function FormatBorderColours(clrBackground, clrBorder, clrBorderLight, clrBorderDark: TColor): String;
    procedure CheckBorderColours(clrBackground, clrBorder, clrBorderLight, clrBorderDark: TColor; sResult: String);
    function FormatInputField( ifField: THTMLInputField; sName, sValue: String; bChecked: Boolean; iSize, iMaxLength: Byte;
                               sImage: String; ahAlign: THTMLAlignHoriz; iBorder: Byte): String;
    procedure CheckField;
    function CheckIfPercentage(sText: String; iValue: Integer): String;
    function ConvertColour(clrColour: TColor): String;

  protected
    { Protected declarations }

  public
    { Public declarations }
    property Version: String read Fversion;	{ Read only }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Initialise;

    { Functions to format text with appropriate HTML tags, returned as strings }

    { HTML header functions }
    function FormatContent(sContent: String): String;
    function FormatHead: String;
    function FormatTitle(sTitle: String): String;
    function FormatIsIndex: String;
    function FormatBase(sUrl: String): String;
    function FormatMeta(sName, sHttpEquiv, sContent: String): String;
    function FormatComment(sText: String): String;

    { HTML body functions }
    function FormatBodyParams(sImage: string; clrBackground, clrText, clrLinks, clrVisited, clrActive: TColor; bFixed: Boolean): string;
    function FormatBody: String;
    function FormatSound(sUrl: String; iLoop: Byte): String;

    { HTML basic formatting functions }
    function FormatHeadingStart(iLevel: THTMLHeadingLevel; ahAlign: THTMLAlignHoriz): String;
    function FormatHeadingEnd(iLevel: THTMLHeadingLevel): String;
    function FormatHeading(iLevel: THTMLHeadingLevel; sHeading: String; ahAlign: THTMLAlignHoriz): String;
    function FormatParagraphStart(ahAlign: THTMLAlignHoriz): String;
    function FormatParagraphEnd: String;
    function FormatParagraph(sText: String; ahAlign: THTMLAlignHoriz): String;

    { HTML image processing functions }
    function FormatImageParams( sImage, sAlt, sMap: String; aiAlign: THTMLAlignImage; iHeight, iWidth: Integer; iHSpace, iVSpace,
                                iBorder: Byte; bIsMap: Boolean): String;
    function FormatImage(sImage, sAlt: String; aiAlign: THTMLAlignImage): String;
    function FormatMapStart(sName: String): String;
    function FormatMapEnd: String;
    function FormatMapArea( shShape: THTMLShapes; iCoords: array of Integer; sUrl, sAlt: String): String;

    { HTML list processing functions }
    function FormatListStartParams(ltList: THTMLListType; nsNum: THTMLNumberScheme; iStart: Byte): String;
    function FormatListStart(ltList: THTMLListType): String;
    function FormatListEnd(ltList: THTMLListType): String;
    function FormatListItemParams(liItem: THTMLListItem; sText: String; nsNum: THTMLNumberScheme; iValue: Byte): String;
    function FormatListItem(liItem: THTMLListItem; sText: String): String;

    { HTML line break functions }
    function FormatHorizRuleParams(iSize: Byte; iWidth: Integer; ahAlign: THTMLAlignHoriz; bNoShade: Boolean; crClear: THTMLClear): String;
    function FormatHorizRule: String;
    function FormatLineBreak(crClear: THTMLClear): String;
    function FormatWordBreak: String;

    { HTML character formatting functions }
    function FormatTextEffectStart(efEffect: THTMLTextEffect): String;
    function FormatTextEffectEnd(efEffect: THTMLTextEffect): String;
    function FormatTextEffect(efEffect: THTMLTextEffect; sText: String): String;
    function FormatFontStart(iSize: THTMLFontSize; fcChange: THTMLFontChange; sFace: String; clrColour: TColor): String;
    function FormatFontEnd: String;
    function FormatFont(sText: String; iSize: THTMLFontSize; fcChange: THTMLFontChange; sFace: String; clrColour: TColor): String;
    function FormatBaseFont(iSize: THTMLFontSize): String;
    function FormatSpecialChar(scSpecial: THTMLSpecialChar): String;
    function FormatSpecialCharValue(iValue: Byte): String;
    function FormatMarqueeStart( avAlign: THTMLAlignVert; iHeight, iWidth: Integer; iHSpace, iVSpace, iScrollAmount: Byte;
                                 iScrollDelay: Integer; mbBehave: THTMLMarqueeBehave; mdDir: THTMLMarqueeDirection; iLoop: Byte;
                                 clrColour: TColor): String;
    function FormatMarqueeEnd: String;
    function FormatMarquee( sText: String; avAlign: THTMLAlignVert; iHeight, iWidth: Integer; iHSpace, iVSpace, iScrollAmount: Byte;
                            iScrollDelay: Integer; mbBehave: THTMLMarqueeBehave; mdDir: THTMLMarqueeDirection; iLoop: Byte;
                            clrColour: TColor): String;

    { HTML link functions }
    function FormatLinkStart(sUrl, sName: String): String;
    function FormatLinkEnd: String;
    function FormatLink(sUrl, sName, sText: String): String;

    { HTML table functions }
    function FormatTableStartParams( iBorder: Byte; iWidth: Integer; iCellSpacing, iCellPadding: Byte; clrBackground, clrBorder,
                                     clrBorderLight, clrBorderDark: TColor; sCaption: String; ahCaptionHAlign: THTMLAlignHoriz;
                                     avCaptionVAlign: THTMLAlignVert): String;
    function FormatTableStart(iBorder: Byte; iWidth: Integer): String;
    function FormatTableEnd: String;
    function FormatTableRowStartParams( ahAlign: THTMLAlignHoriz; avAlign: THTMLAlignVert; clrBackground, clrBorder, clrBorderLight,
                                        clrBorderDark: TColor): String;
    function FormatTableRowStart: String;
    function FormatTableRowEnd: String;
    function FormatTableHeadingStartParams( iRowSpan, iColSpan: Byte; ahAlign: THTMLAlignHoriz; avAlign: THTMLAlignVert;
                                            clrBackground, clrBorder, clrBorderLight, clrBorderDark: TColor): String;
    function FormatTableHeadingStart: String;
    function FormatTableHeadingEnd: String;
    function FormatTableHeadingParams(sHeading: String; iRowSpan, iColSpan: Byte;
      ahAlign: THTMLAlignHoriz; avAlign: THTMLAlignVert;
      clrBackground, clrBorder, clrBorderLight, clrBorderDark: TColor): String;
    function FormatTableHeading(sHeading: String): String;
    function FormatTableCellStartParams(iRowSpan, iColSpan: Byte; iWidth: Integer;
      ahAlign: THTMLAlignHoriz; avAlign: THTMLAlignVert;
      clrBackground, clrBorder, clrBorderLight, clrBorderDark: TColor): String;
    function FormatTableCellStart: String;
    function FormatTableCellEnd: String;
    function FormatTableCellParams(sText: String; iRowSpan, iColSpan: Byte;
      iWidth: Integer; ahAlign: THTMLAlignHoriz; avAlign: THTMLAlignVert;
      clrBackground, clrBorder, clrBorderLight, clrBorderDark: TColor): String;
    function FormatTableCell(sText: String): String;

    { HTML form functions }
    function FormatFormStart(sUrl: String; fmMethod: THTMLFormMethod): String;
    function FormatFormEnd: String;
    function FormatTextField(sName, sDefault: String;
      iSize, iMaxLength: Byte): String;
    function FormatPasswordField(sName, sDefault: String;
      iSize, iMaxLength: Byte): String;
    function FormatCheckboxField(sName, sValue: String;
      bChecked: Boolean): String;
    function FormatRadioField(sName, sValue: String; bChecked: Boolean): String;
    function FormatSubmitField(sName, sLabel: String): String;
    function FormatResetField(sLabel: String): String;
    function FormatImageField(sName, sImage: String; ahAlign: THTMLAlignHoriz;
      iBorder: Byte): String;
    function FormatHiddenField(sName, sValue: String): String;
    function FormatSelectStart(sName: String; iSize: Byte;
      bMultiple: Boolean): String;
    function FormatSelectEnd: String;
    function FormatSelectOption(sText, sValue: String;
      bSelected: Boolean): String;
    function FormatTextAreaStart(sName: String; iRows, iCols: Byte): String;
    function FormatTextAreaEnd: String;
    function FormatTextArea(sName, sDefault: String; iRows, iCols: Byte): String;

    { HTML bulk insertion functions }
    function FormatEscapeText(sText: String): String;

    { Procedures to write text to Destination with appropriate HTML tags }

    { HTML header procedures }
    procedure Content(sContent: String);
    procedure Head;
    procedure Title(sTitle: String);
    procedure IsIndex;
    procedure Base(sUrl: String);
    procedure Meta(sName, sHttpEquiv, sContent: String);
    procedure Comment(sText: String);

    { HTML body procedures }
    procedure BodyParams(sImage: String; clrBackground, clrText,
      clrLinks, clrVisited, clrActive: TColor; bFixed: Boolean);
    procedure Body;
    procedure Sound(sUrl: String; iLoop: Byte);

    { HTML basic formatting procedures }
    procedure HeadingStart(iLevel: THTMLHeadingLevel; ahAlign: THTMLAlignHoriz);
    procedure HeadingEnd(iLevel: THTMLHeadingLevel);
    procedure Heading(iLevel: THTMLHeadingLevel; sHeading: String;
      ahAlign: THTMLAlignHoriz);
    procedure ParagraphStart(ahAlign: THTMLAlignHoriz);
    procedure ParagraphEnd;
    procedure Paragraph(sText: String; ahAlign: THTMLAlignHoriz);

    { HTML image processing procedures }
    procedure ImageParams(sImage, sAlt, sMap: String; aiAlign: THTMLAlignImage;
      iHeight, iWidth: Integer; iHSpace, iVSpace, iBorder: Byte; bIsMap: Boolean);
    procedure Image(sImage, sAlt: String; aiAlign: THTMLAlignImage);
    procedure MapStart(sName: String);
    procedure MapEnd;
    procedure MapArea(shShape: THTMLShapes; iCoords: array of Integer;
      sUrl, sAlt: String);

    { HTML list procedures }
    procedure ListStartParams(ltList: THTMLListType;
      nsNum: THTMLNumberScheme; iStart: Byte);
    procedure ListStart(ltList: THTMLListType);
    procedure ListEnd(ltList: THTMLListType);
    procedure ListItemParams(liItem: THTMLListItem; sText: String;
      nsNum: THTMLNumberScheme; iValue: Byte);
    procedure ListItem(liItem: THTMLListItem; sText: String);

    { HTML line break procedures }
    procedure HorizRuleParams(iSize: Byte; iWidth: Integer;
      ahAlign: THTMLAlignHoriz; bNoShade: Boolean; crClear: THTMLClear);
    procedure HorizRule;
    procedure LineBreak(crClear: THTMLClear);
    procedure WordBreak;

    { HTML character formatting procedures }
    procedure TextEffectStart(efEffect: THTMLTextEffect);
    procedure TextEffectEnd(efEffect: THTMLTextEffect);
    procedure TextEffect(efEffect: THTMLTextEffect; sText: String);
    procedure FontStart(iSize: THTMLFontSize; fcChange: THTMLFontChange;
      sFace: String; clrColour: TColor);
    procedure FontEnd;
    procedure Font(sText: String; iSize: THTMLFontSize;
      fcChange: THTMLFontChange; sFace: String; clrColour: TColor);
    procedure BaseFont(iSize: THTMLFontSize);
    procedure SpecialChar(scSpecial: THTMLSpecialChar);
    procedure SpecialCharValue(iValue: Byte);
    procedure MarqueeStart(avAlign: THTMLAlignVert;
      iHeight, iWidth: Integer; iHSpace, iVSpace, iScrollAmount: Byte;
      iScrollDelay: Integer; mbBehave: THTMLMarqueeBehave;
      mdDir: THTMLMarqueeDirection; iLoop: Byte; clrColour: TColor);
    procedure MarqueeEnd;
    procedure Marquee(sText: String; avAlign: THTMLAlignVert;
      iHeight, iWidth: Integer; iHSpace, iVSpace, iScrollAmount: Byte;
      iScrollDelay: Integer; mbBehave: THTMLMarqueeBehave;
      mdDir: THTMLMarqueeDirection; iLoop: Byte; clrColour: TColor);

    { HTML link procedures }
    procedure LinkStart(sUrl, sName: String);
    procedure LinkEnd;
    procedure Link(sUrl, sName, sText: String);

    { HTML table procedures }
    procedure TableStartParams(iBorder: Byte; iWidth: Integer;
      iCellSpacing, iCellPadding: Byte;
      clrBackground, clrBorder, clrBorderLight, clrBorderDark: TColor;
      sCaption: String; ahCaptionHAlign: THTMLAlignHoriz;
      avCaptionVAlign: THTMLAlignVert);
    procedure TableStart(iBorder: Byte; iWidth: Integer);
    procedure TableEnd;
    procedure TableRowStartParams(ahAlign: THTMLAlignHoriz; avAlign: THTMLAlignVert;
      clrBackground, clrBorder, clrBorderLight, clrBorderDark: TColor);
    procedure TableRowStart;
    procedure TableRowEnd;
    procedure TableHeadingStartParams(iRowSpan, iColSpan: Byte;
      ahAlign: THTMLAlignHoriz; avAlign: THTMLAlignVert;
      clrBackground, clrBorder, clrBorderLight, clrBorderDark: TColor);
    procedure TableHeadingStart;
    procedure TableHeadingEnd;
    procedure TableHeadingParams(sHeading: String; iRowSpan, iColSpan: Byte;
      ahAlign: THTMLAlignHoriz; avAlign: THTMLAlignVert;
      clrBackground, clrBorder, clrBorderLight, clrBorderDark: TColor);
    procedure TableHeading(sHeading: String);
    procedure TableCellStartParams(iRowSpan, iColSpan: Byte; iWidth: Integer;
      ahAlign: THTMLAlignHoriz; avAlign: THTMLAlignVert;
      clrBackground, clrBorder, clrBorderLight, clrBorderDark: TColor);
    procedure TableCellStart;
    procedure TableCellEnd;
    procedure TableCellParams(sText: String; iRowSpan, iColSpan: Byte;
      iWidth: Integer; ahAlign: THTMLAlignHoriz; avAlign: THTMLAlignVert;
      clrBackground, clrBorder, clrBorderLight, clrBorderDark: TColor);
    procedure TableCell(sText: String);

    { HTML form procedures }
    procedure FormStart(sUrl: String; fmMethod: THTMLFormMethod);
    procedure FormEnd;
    procedure TextField(sName, sDefault: String; iSize, iMaxLength: Byte);
    procedure PasswordField(sName, sDefault: String; iSize, iMaxLength: Byte);
    procedure CheckboxField(sName, sValue: String; bChecked: Boolean);
    procedure RadioField(sName, sValue: String; bChecked: Boolean);
    procedure SubmitField(sName, sLabel: String);
    procedure ResetField(sLabel: String);
    procedure ImageField(sName, sImage: String; ahAlign: THTMLAlignHoriz;
      iBorder: Byte);
    procedure HiddenField(sName, sValue: String);
    procedure SelectStart(sName: String; iSize: Byte; bMultiple: Boolean);
    procedure SelectEnd;
    procedure SelectOption(sText, sValue: String; bSelected: Boolean);
    procedure TextAreaStart(sName: String; iRows, iCols: Byte);
    procedure TextAreaEnd;
    procedure TextArea(sName, sDefault: String; iRows, iCols: Byte);

    { HTML bulk insertion procedures }
    procedure Text(sText: String);
    procedure EscapeText(sText: String);
    procedure TextList(slText: TStringList);
    procedure EscapeTextList(slText: TStringList);
    procedure InsertFile(sFileName: String);
    procedure MergeFile(sFileName: String; dicDictionary: THTMLDictionary);

    { And tidy up }
    procedure Finalise;

  published
    { Published declarations }
    property Filename: String read FFilename write SetFilename;
    property Errors: THTMLErrorSet read FErrors write FErrors
      default [erErrors, erWarnings];
    property IncludeMIMEType: Boolean read FIncludeMIMEType write FIncludeMIMEType
      default False;
  end;

  { Base class for all HTML exceptions }
  EHTMLException = class(Exception)
  private
    { Private declarations }
    FTag: THTMLTagCategory;
  public
    { Public declarations }
    property Tag: THTMLTagCategory read FTag write FTag;
    constructor Create(tcTag: THTMLTagCategory; sMessage: String);
  end;

  { Class for all HTML warnings }
  EHTMLWarning = class(EHTMLException)
  private
    { Private declarations }
    FResult: String;
  public
    { Public declarations }
    property Result: String read FResult write FResult;
    constructor Create(tcTag: THTMLTagCategory; sMessage, sResult: String);
  end;

  { Class for all HTML errors }
  EHTMLError = class(EHTMLException)
  end;

function Percent(iValue: Integer): Integer;

procedure Register;

implementation

const
  { Text versions/descriptions of the enumerated types above }
  sAlignHoriz: array [THTMLAlignHoriz] of String[6] =
    ('', 'left', 'center', 'right');
  sAlignVert: array [THTMLAlignVert] of String[6] =
    ('', 'top', 'middle', 'bottom');
  sAlignImage: array [THTMLAlignImage] of String[6] =
    ('', 'top', 'middle', 'bottom', 'left', 'right');
  sShape: array [THTMLShapes] of String[7] =
    ('default', 'rect', 'circle', 'polygon');
  sClear: array [THTMLClear] of String[5] = ('', 'left', 'right', 'all');
  sListType: array [THTMLListType] of String[4] =
    ('ul', 'ol', 'menu', 'dir', 'dl');
  sListDescription: array [THTMLListType] of String[15] =
    ('unordered list', 'ordered list', 'menu list',
    'directory list', 'definition list');
  sListItem: array [THTMLListItem] of String[2] = ('li', 'dt', 'dd');
  sNumberScheme: array [THTMLNumberScheme] of String[1] =
    ('', 'A', 'a', 'I', 'i', '1');
  sTextEffect: array [THTMLTextEffect] of String[10] =
    ('b', 'i', 'u', 'tt', 'em', 'strong', 'code', 'samp', 'kbd', 'cite',
    'dfn', 'var', 'blockquote', 'blink', 'nobr', 'sup', 'sub', 'ins', 'del',
    'small', 'big', 'address', 'pre', 'center');
  sEffectDescription: array [THTMLTextEffect] of String[11] =
    ('bold', 'italic', 'underline', 'fixed size', 'emphasised', 'strong',
    'code', 'sample', 'keyboard', 'citation', 'definition', 'variable',
    'block quote', 'blink', 'no break', 'superscript', 'subscript',
    'insertion', 'deletion', 'small', 'big', 'address', 'preformat',
    'centre');
  sSpecialChar: array [THTMLSpecialChar] of String[6] =
    ('lt', 'gt', 'amp', 'quot', '#160', '#169', '#174', '#162', '#163',
    '#165', '#189', '#188', '#190', 'AElig', 'Aacute', 'Acirc', 'Agrave',
    'Aring', 'Atilde', 'Auml', 'Ccedil', 'ETH', 'Eacute', 'Ecirc', 'Egrave',
    'Euml', 'Iacute', 'Icirc', 'Igrave', 'Iuml', 'Ntilde', 'Oacute', 'Ocirc',
    'Ograve', 'Oslash', 'Otilde', 'Ouml', 'THORN', 'Uacute', 'Ucirc',
    'Ugrave', 'Uuml', 'Yacute', 'aelig', 'aacute', 'acirc', 'agrave', 'aring',
    'atilde', 'auml', 'ccedil', 'eth', 'eacute', 'ecirc', 'egrave', 'euml',
    'iacute', 'icirc', 'igrave', 'iuml', 'ntilde', 'oacute', 'ocirc',
    'ograve', 'oslash', 'otilde', 'ouml', 'thorn', 'szlig', 'uacute', 'ucirc',
    'ugrave', 'uuml', 'yacute', 'yuml');
  sFontChange: array [THTMLFontChange] of String[1] = ('', '-', '+');
  sMarqueeBehaviour: array [THTMLMarqueeBehave] of String[9] =
    ('scroll', 'slide', 'alternate');
  sMarqueeDirection: array [THTMLMarqueeDirection] of String[5] =
    ('left', 'right');
  sFormMethod: array [THTMLFormMethod] of String[4] = ('get', 'post');
  sInputField: array [THTMLInputField] of String[8] =
    ('text', 'password', 'checkbox', 'radio',
    'submit', 'reset', 'image', 'hidden');
  sNewline = #13#10;
  sHTMLWriterVersion = '1.1';

{ Register the THTMLWriter component with Delphi }
procedure Register;
begin
  RegisterComponents('InetLib', [THTMLWriter]);
end;

{ *****************************************************************************
  HTMLWriter dictionary }

{ Create a field value object for the HTML dictionary }
constructor THTMLFieldValue.Create(sValue: String);
begin
  inherited Create;
  FieldValue := sValue;
end;

{ Add a new field/value pair to the dictionary }
procedure THTMLDictionary.AddFieldAndValue(sField, sValue: String);
var
  fvFieldValue: THTMLFieldValue;
begin
  fvFieldValue := THTMLFieldValue.Create(sValue);
  AddObject(sField, fvFieldValue);
  { Field value will be freed by TStringList when it is destroyed }
end;

{ Retrieve value for a given fild from dictionary }
function THTMLDictionary.GetValue(sField: String): String;
var
  i: Integer;
begin
  i := IndexOf(sField);
  if i = -1 then
    Result := EmptyStr
  else
    Result := (Objects[i] as THTMLFieldValue).FieldValue;
end;

{ *****************************************************************************
  HTMLWriter exceptions }

{ Create an HTML exception }
constructor EHTMLException.Create(tcTag: THTMLTagCategory; sMessage: String);
begin
  inherited Create(sMessage);
  Tag := tcTag;
end;

{ Create an HTML warning }
constructor EHTMLWarning.Create(tcTag: THTMLTagCategory; sMessage, sResult: String);
begin
  inherited Create(tcTag, sMessage);
  Result := sResult;
end;

{ *****************************************************************************
  HTMLWriter functions and procedures }

{ Create HTML writer component }
constructor THTMLWriter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  { Set default values for the properties }
  FFilename := DefaultFilename;
  FErrors := [erErrors, erWarnings];
  FIncludeMIMEType := False;
  FVersion := sHTMLWriterVersion;
  sTags := TStringList.Create;
end;

{ Release resources }
destructor THTMLWriter.Destroy;
begin
  sTags.Free;
  if not (csDesigning in ComponentState) then  { Not in design mode }
  begin
    try
      CloseFile(fOutput);
    except on EInOutError do { Ignore errors in closing - assume already done }
    end;
  end;

  inherited Destroy;
end;

{ Initialise internal flags, etc., for a new document }
procedure THTMLWriter.Initialise;
begin
  bWrittenContent := False;      { Has content directive been written ? }
  bWrittenHead := False;         { Has head tag been written ? }
  bInHead := False;              { Are we in the head of the document ? }
  bWrittenTitle := False;        { Has a title been written ? }
  bWrittenIsIndex := False;      { Has an isindex directive been written ? }
  bWrittenBase := False;         { Has a base directive been written ? }
  bInBody := False;              { Are we in the body of the document ? }
  bInMap := False;               { Are we in a map definition ? }
  bWrittenArea := False;         { Have any areas been written for this map ? }
  bInLink := False;              { Are we in a link ? }
  bInForm := False;              { Are we in a form ? }
  bInSelect := False;            { Are we in a select list in a form ? }
  bSelectOption := False;        { Has an option been written for this select ? }
  bInTextArea := False;          { Are we in a text area in a form ? }
  bSubmit := False;              { Has a submit button been written for the form ? }
  bBulkInsert := False;          { Has a file been inserted or merged ? }
  iCurHeadingLevel := 0;
  iLastHeadingLevel := 0;
  iTableLevels := 0;
  iCurList := 0;
  sTags.Clear;

  try
    CloseFile(fOutput);
  except on EInOutError do { Ignore errors in closing - assume already done }
  end;
  AssignFile(fOutput, FFilename);
  Rewrite(fOutput);
end;

{ Set up a new destination for the HTML }
procedure THTMLWriter.SetFilename(sFilename: String);
begin
  if FFilename <> sFilename then
    FFilename := sFilename;
end;

{ Check that the tag is in the correct part of the document }
procedure THTMLWriter.CheckNesting(tcTag: THTMLTagCategory;
    bAllowedInHead, bAllowedInBody, bAllowedInMap, bAllowedInTable,
    bAllowedInForm: Boolean);
begin
  if erErrors in Errors then
  begin
    if not bAllowedInHead and bInHead then
      raise EHTMLError.Create(tcTag, 'Tag not allowed in head block');
    if not bAllowedInBody and bInBody then
      raise EHTMLError.Create(tcTag, 'Tag not allowed in body block');
    if not bAllowedInMap and bInMap then
      raise EHTMLError.Create(tcTag, 'Tag not allowed in map definition');
    if not bAllowedInTable and (iTableLevels > 0) then
      raise EHTMLError.Create(tcTag, 'Tag not allowed in table definition');
    if not bAllowedInForm and bInForm then
      raise EHTMLError.Create(tcTag, 'Tag not allowed in form definition');
  end;
end;

{ Check that the tag can be closed successfully }
procedure THTMLWriter.CheckClosing(tcTag: THTMLTagCategory;
    bNotThere: Boolean; sTag, sText: String);
begin
  if erErrors in Errors then
  begin
    if bNotThere or (sTags.Count = 0) then
      raise EHTMLError.Create(tcTag, 'Closing non-existent ' + sText);
    if sTags[sTags.Count - 1] <> sTag then
      raise EHTMLError.Create(tcTag,
        'Unclosed tags found when closing ' + sText);
  end;
end;

{ *****************************************************************************
  HTML header functions and procedures }

{ Return content type as string - default to text/html }
function THTMLWriter.FormatContent(sContent: String): String;
begin
  if sContent = EmptyStr then
    sContent := 'text/html';
  Result := 'Content-type: ' + sContent + sNewline + sNewline;
end;

{ Write content type }
procedure THTMLWriter.Content(sContent: String);
begin
  if (bInHead or bInBody) and (erErrors in Errors) then
    raise EHTMLError.Create(tcContent,
      'Content type must be first thing in output');
  if bWrittenContent and (erErrors in Errors) then
    raise EHTMLError.Create(tcContent, 'Content type can only appear once');

  bWrittenContent := True;
  Write(fOutput, FormatContent(sContent));
end;

{ Return start of HTML document as string }
function THTMLWriter.FormatHead: String;
begin
  Result := '<html>' + sNewline + '<head>';
end;

{ Write start of HTML document }
procedure THTMLWriter.Head;
begin
  if bWrittenHead and (erErrors in Errors) then
    raise EHTMLError.Create(tcHead, 'Already written the head');

  if not bWrittenContent and IncludeMIMEType then
    Content(EmptyStr);

  bWrittenHead := True;
  bInHead := True;
  Writeln(fOutput, FormatHead);
end;

{ Return page title as string }
function THTMLWriter.FormatTitle(sTitle: String): String;
begin
  if (sTitle = EmptyStr) and (erErrors in Errors) then
    raise EHTMLError.Create(tcTitle, 'No title text supplied');

  Result := '<title>' + sTitle + '</title>';
end;

{ Write page title }
procedure THTMLWriter.Title(sTitle: String);
begin
  if not bWrittenHead then
    Head;

  CheckNesting(tcTitle, True, False, False, False, False);
  if bWrittenTitle and (erErrors in Errors) then
    raise EHTMLError.Create(tcTitle, 'Can only have one title');

  bWrittenTitle := True;
  Writeln(fOutput, FormatTitle(sTitle));
end;

{ Return isindex directive as string }
function THTMLWriter.FormatIsIndex: String;
begin
  Result := '<isindex>';
end;

{ Write isindex directive }
procedure THTMLWriter.IsIndex;
begin
  if not bWrittenHead then
    Head;

  CheckNesting(tcIsIndex, True, False, False, False, False);
  if bWrittenIsIndex and (erErrors in Errors) then
    raise EHTMLError.Create(tcIsIndex, 'Can only have one index reference');

  bWrittenIsIndex := True;
  Writeln(fOutput, FormatIsIndex);
end;

{ Return base directive as string }
function THTMLWriter.FormatBase(sUrl: String): String;
begin
  if (sUrl = EmptyStr) and (erErrors in Errors) then
    raise EHTMLError.Create(tcBase, 'Missing base URL');

  Result := '<base href="' + sUrl + '">';
end;

{ Write base directive }
procedure THTMLWriter.Base(sUrl: String);
begin
  if not bWrittenHead then
    Head;

  CheckNesting(tcBase, True, False, False, False, False);
  if bWrittenBase and (erErrors in Errors) then
    raise EHTMLError.Create(tcBase, 'Can only have one base URL');

  bWrittenBase := True;
  Writeln(fOutput, FormatBase(sUrl));
end;

{ Return meta directive as string }
function THTMLWriter.FormatMeta(sName, sHttpEquiv, sContent: String): String;
begin
  if erErrors in Errors then
  begin
    if (sName = EmptyStr) and (sHttpEquiv = EmptyStr) then
      raise EHTMLError.Create(tcMeta, 'Missing meta name and http-equivalent');
    if sContent = EmptyStr then
      raise EHTMLError.Create(tcMeta, 'Missing meta content');
  end;

  Result := '<meta';
  if sName <> EmptyStr then
    Result := Result + ' name="' + sName + '"';
  if sHttpEquiv <> EmptyStr then
    Result := Result + ' http-equiv="' + sHttpEquiv + '"';
  Result := Result + ' content="' + sContent + '">';
end;

{ Write meta directive }
procedure THTMLWriter.Meta(sName, sHttpEquiv, sContent: String);
begin
  if not bWrittenHead then
    Head;

  CheckNesting(tcMeta, True, False, False, False, False);

  Writeln(fOutput, FormatMeta(sName, sHttpEquiv, sContent));
end;

{ Return HTML comment as string }
function THTMLWriter.FormatComment(sText: String): String;
begin
  Result := '<!-- ' + sText + '-->';
end;

{ Write HTML comment }
procedure THTMLWriter.Comment(sText: String);
begin
  Write(fOutput, FormatComment(sText));
end;

{ *****************************************************************************
  HTML body functions and procedures }

{ Return start of HTML body, with optional parameters, as string }
function THTMLWriter.FormatBodyParams(sImage: String;
    clrBackground, clrText, clrLinks, clrVisited, clrActive: TColor;
    bFixed: Boolean): String;
begin
  Result := '</head>' + sNewline + '<body';
  if sImage <> EmptyStr then
    Result := Result + ' background="' + sImage + '"';
  if clrBackground <> clDefault then
    Result := Result + ' bgcolor=' + ConvertColour(clrBackground);
  if clrText <>clDefault then
    Result := Result + ' text=' + ConvertColour(clrText);
  if clrLinks <> clDefault then
    Result := Result + ' link=' + ConvertColour(clrLinks);
  if clrVisited <> clDefault then
    Result := Result + ' vlink=' + ConvertColour(clrVisited);
  if clrActive <> clDefault then
    Result := Result + ' alink=' + ConvertColour(clrActive);
  if bFixed then
    Result := Result + ' bgproperties=fixed';
  Result := Result + '>';

  if (sImage <> EmptyStr) and (erHTML3 in Errors) then
    raise EHTMLWarning.Create(tcBody, 'BACKGROUND is HTML 3.0 extension', Result);
  if ((clrBackground <> clDefault) or (clrText <> clDefault) or
      (clrLinks <> clDefault) or (clrVisited <> clDefault) or
      (clrActive <> clDefault)) and (erNetscape in Errors) then
    raise EHTMLWarning.Create(tcBody,
      'BGCOLOR, TEXT, LINK, VLINK, ALINK are Netscape extensions', Result);
  if bFixed and (erIExplorer in Errors) then
    raise EHTMLWarning.Create(tcBody,
      'BGPROPERTIES=FIXED is IExplorer extension', Result);
end;

{ Write start of HTML body, with optional parameters }
procedure THTMLWriter.BodyParams(sImage: String;
    clrBackground, clrText, clrLinks, clrVisited, clrActive: TColor;
    bFixed: Boolean);
begin
  if not bWrittenHead then
  begin
    Head;
    Title('Home page');
  end;

  bInHead := False;
  bInBody := True;
  try
    Writeln(fOutput, FormatBodyParams(sImage,
      clrBackground, clrText, clrLinks, clrVisited, clrActive, bFixed));
  except on e: EHTMLWarning do
    begin
      Writeln(fOutput, e.Result);
      raise;
    end
  end;
end;

{ Return start of HTML body as string }
function THTMLWriter.FormatBody: String;
begin
  Result := FormatBodyParams(EmptyStr,
    clDefault, clDefault, clDefault, clDefault, clDefault, False);
end;

{ Write start of HTML body }
procedure THTMLWriter.Body;
begin
  BodyParams(EmptyStr, clDefault, clDefault, clDefault, clDefault, clDefault, False);
end;

{ Return background sound as string }
function THTMLWriter.FormatSound(sUrl: String; iLoop: Byte): String;
begin
  if (sUrl = EmptyStr) and (erErrors in Errors) then
    raise EHTMLError.Create(tcBody, 'Missing URL for background sound');

  Result := '<bgsound src="' + sUrl + '"';
  if iLoop = 0 then
    Result := Result + ' loop=infinite'
  else
    Result := Result + ' loop=' + IntToStr(iLoop);
  Result := Result + '>';

  if erIExplorer in Errors then
    raise EHTMLWarning.Create(tcBody,
      'BGSOUND is IExplorer extension', Result);
end;

{ Write background sound }
procedure THTMLWriter.Sound(sUrl: String; iLoop: Byte);
begin
  CheckNesting(tcBody, False, True, False, True, True);

  try
    Writeln(fOutput, FormatSound(sUrl, iLoop));
  except on e: EHTMLWarning do
    begin
      Writeln(fOutput, e.Result);
      raise;
    end;
  end;
end;

{ *****************************************************************************
  HTML basic formatting functions and procedures }

{ Return start of heading as string }
function THTMLWriter.FormatHeadingStart(iLevel: THTMLHeadingLevel;
    ahAlign: THTMLAlignHoriz): String;
begin
  Result := '<h' + IntToStr(iLevel);
  if ahAlign <> ahDefault then
    Result := Result + ' align=' + sAlignHoriz[ahAlign];
  Result := Result + '>';

  if (ahAlign <> ahDefault) and (erHTML3 in Errors) then
    raise EHTMLWarning.Create(tcHeading, 'ALIGN is HTML 3.0 extension', Result);
end;

{ Write start of heading }
procedure THTMLWriter.HeadingStart(iLevel: THTMLHeadingLevel;
    ahAlign: THTMLAlignHoriz);
var
  iLast: Byte;
begin
  CheckNesting(tcHeading, False, True, False, True, True);
  if (iCurHeadingLevel > 0) and (erErrors in Errors) then
    raise EHTMLError.Create(tcHeading, 'Cannot embed header in another header');

  sTags.Add('h' + IntToStr(iLevel));
  iLast := iLastHeadingLevel;
  iLastHeadingLevel := iLevel;
  iCurHeadingLevel := iLevel;
  try
    Write(fOutput, FormatHeadingStart(iLevel, ahAlign));
  except on e: EHTMLWarning do
    begin
      Write(fOutput, e.Result);
      raise;
    end;
  end;

  if (iLast + 1 < iLevel) and (erWarnings in Errors) then
  begin
    raise EHTMLWarning.Create(tcHeading, 'Heading levels not sequential', EmptyStr);
  end;
end;

{ Return end of heading as string }
function THTMLWriter.FormatHeadingEnd(iLevel: THTMLHeadingLevel): String;
begin
  Result := '</h' + IntToStr(iLevel) + '>';
end;

{ Write end of heading }
procedure THTMLWriter.HeadingEnd(iLevel: THTMLHeadingLevel);
begin
  CheckClosing(tcHeading, (iCurHeadingLevel <> iLevel),
    'h' + IntToStr(iLevel), 'header');

  iCurHeadingLevel := 0;
  sTags.Delete(sTags.Count - 1);
  Writeln(fOutput, FormatHeadingEnd(iLevel));
end;

{ Return heading as string }
function THTMLWriter.FormatHeading(iLevel: THTMLHeadingLevel;
    sHeading: String; ahAlign: THTMLAlignHoriz): String;
begin
  Result := FormatHeadingStart(iLevel, ahAlign) + sHeading +
    FormatHeadingEnd(iLevel);
end;

{ Write heading }
procedure THTMLWriter.Heading(iLevel: THTMLHeadingLevel; sHeading: String;
    ahAlign: THTMLAlignHoriz);
begin
  HeadingStart(iLevel, ahAlign);
  Write(fOutput, sHeading);
  HeadingEnd(iLevel);
end;

{ Return start of paragraph as string }
function THTMLWriter.FormatParagraphStart(ahAlign: THTMLAlignHoriz): String;
begin
  Result := '<p';
  if ahAlign <> ahDefault then
    Result := Result + ' align=' + sAlignHoriz[ahAlign];
  Result := Result + '>';

  if (ahAlign <> ahDefault) and (erHTML3 in Errors) then
    raise EHTMLWarning.Create(tcParagraph, 'ALIGN is HTML 3.0 extension', Result);
end;

{ Write start of paragraph }
procedure THTMLWriter.ParagraphStart(ahAlign: THTMLAlignHoriz);
begin
  CheckNesting(tcParagraph, False, True, False, True, True);

  sTags.Add('p');
  try
    Write(fOutput, FormatParagraphStart(ahAlign));
  except on e: EHTMLWarning do
    begin
      Write(fOutput, e.Result);
      raise;
    end;
  end;
end;

{ Return end of paragraph as string }
function THTMLWriter.FormatParagraphEnd: String;
begin
  Result := '</p>';
end;

{ Write end of paragraph }
procedure THTMLWriter.ParagraphEnd;
begin
  CheckClosing(tcParagraph, (sTags.IndexOf('p') < 0), 'p', 'paragraph');

  sTags.Delete(sTags.Count - 1);
  Writeln(fOutput, FormatParagraphEnd);
end;

{ Return parargraph as string }
function THTMLWriter.FormatParagraph(sText: String;
    ahAlign: THTMLAlignHoriz): String;
begin
  Result := FormatParagraphStart(ahAlign) + sText + FormatParagraphEnd;
end;

{ Write paragraph }
procedure THTMLWriter.Paragraph(sText: String; ahAlign: THTMLAlignHoriz);
begin
  ParagraphStart(ahAlign);
  Write(fOutput, sText);
  ParagraphEnd;
end;

{ *****************************************************************************
  HTML image processing functions and procedures }

{ Return image, with all parameters, as string }
function THTMLWriter.FormatImageParams(sImage, sAlt, sMap: String;
    aiAlign: THTMLAlignImage; iHeight, iWidth: Integer;
    iHSpace, iVSpace, iBorder: Byte; bIsMap: Boolean): String;
begin
  if (sImage = EmptyStr) and (erErrors in Errors) then
    raise EHTMLError.Create(tcImage, 'Missing image source');

  Result := '<img src="' + sImage + '" border=' + IntToStr(iBorder);
  if sAlt <> EmptyStr then
    Result := Result + ' alt="' + sAlt + '"';
  if aiAlign <> aiDefault then
    Result := Result + ' align=' + sAlignImage[aiAlign];
  if iHeight <> 0 then
    Result := Result + CheckIfPercentage('height', iHeight);
  if iWidth <> 0 then
    Result := Result + CheckIfPercentage('width', iWidth);
  if iHSpace <> 0 then
    Result := Result + ' hspace=' + IntToStr(iHSpace);
  if iVSpace <> 0 then
    Result := Result + ' vspace=' + IntToStr(iVSpace);
  if sMap <> EmptyStr then
    Result := Result + ' usemap="' + sMap + '"';
  if bIsMap then
    Result := Result + ' ismap';
  Result := Result + '>';

  if ((iBorder <> 0) or (iHSpace <> 0) or (iVSpace <> 0)) and (erNetscape in Errors) then
    raise EHTMLWarning.Create(tcImage,
      'BORDER, HSPACE, VSPACE are Netscape extensions', Result);
  if ((iWidth <> 0) or (iHeight <> 0) or (aiAlign <> aiDefault) or
      (sMap <> EmptyStr)) and (erHTML3 in Errors) then
    raise EHTMLWarning.Create(tcImage,
      'WIDTH, HEIGHT, ALIGN, USEMAP are HTML 3.0 extensions', Result);
end;

{ Write image, with all parameters }
procedure THTMLWriter.ImageParams(sImage, sAlt, sMap: String;
    aiAlign: THTMLAlignImage; iHeight, iWidth: Integer;
    iHSpace, iVSpace, iBorder: Byte; bIsMap: Boolean);
begin
  CheckNesting(tcImage, False, True, False, True, True);

  try
    Write(fOutput, FormatImageParams(sImage, sAlt, sMap,
      aiAlign, iHeight, iWidth, iHSpace, iVSpace, iBorder, bIsMap));
  except on e: EHTMLWarning do
    begin
      Write(fOutput, e.Result);
      raise;
    end;
  end;
end;

{ Return image as string }
function THTMLWriter.FormatImage(sImage, sAlt: String;
    aiAlign: THTMLAlignImage): String;
begin
  Result :=
    FormatImageParams(sImage, sAlt, EmptyStr, aiAlign, 0, 0, 0, 0, 0, False);
end;

{ Write image }
procedure THTMLWriter.Image(sImage, sAlt: String; aiAlign: THTMLAlignImage);
begin
  ImageParams(sImage, sAlt, EmptyStr, aiAlign, 0, 0, 0, 0, 0, False);
end;

{ Return start of inline map as string }
function THTMLWriter.FormatMapStart(sName: String): String;
begin
  if (sName = EmptyStr) and (erErrors in Errors) then
    raise EHTMLError.Create(tcMap, 'Missing map name');

  Result := '<map name="' + sName + '">';

  if erNetscape in Errors then
    raise EHTMLWarning.Create(tcMap, 'MAP is Netscape extension', Result);
end;

{ Write start of inline map }
procedure THTMLWriter.MapStart(sName: String);
begin
  CheckNesting(tcMap, False, True, False, False, False);

  bInMap := True;
  bWrittenArea := False;
  sTags.Add('map');
  try
    Writeln(fOutput, FormatMapStart(sName));
  except on e: EHTMLWarning do
    begin
      Writeln(fOutput, e.Result);
      raise;
    end;
  end;
end;

{ Return end of inline map as string }
function THTMLWriter.FormatMapEnd: String;
begin
  Result := '</map>';
end;

{ Write end of inline map }
procedure THTMLWriter.MapEnd;
begin
  CheckClosing(tcMap, not bInMap, 'map', 'map');

  bInMap := False;
  sTags.Delete(sTags.Count - 1);
  Writeln(fOutput, FormatMapEnd);

  if not bWrittenArea and (erWarnings in Errors) then
    raise EHTMLWarning.Create(tcMap, 'No areas written for this map', EmptyStr);
end;

{ Return area for inline map as string }
function THTMLWriter.FormatMapArea(shShape: THTMLShapes;
    iCoords: array of Integer; sUrl, sAlt: String): String;
var
  i: Integer;
  sSep: String;
begin
  if erErrors in Errors then
  begin
    i := High(iCoords) + 1; { Number of entries in array }
    if ((shShape = shRect) and (i <> 4)) or ((shShape = shCircle) and (i <> 3)) or
        ((shShape = shPolygon) and ((i < 6) or Odd(i))) then
      raise EHTMLError.Create(tcMap,
        'Invalid number of coordinates for ' + sShape[shShape]);
  end;

  Result := '<area shape=' + sShape[shShape];
  if shShape <> shDefault then
  begin
    sSep := ' coords="';
    for i := 0 to High(iCoords) do
    begin
      Result := Result + sSep + IntToStr(iCoords[i]);
      sSep := ',';
    end;
    Result := Result + '"';
  end;
  if sUrl = EmptyStr then
    Result := Result + ' nohref'
  else
    Result := Result + ' href="' + sUrl + '"';
  if sAlt <> EmptyStr then
    Result := Result + ' alt="' + sAlt + '"';
  Result := Result + '>';
end;

{ Write area for inline map }
procedure THTMLWriter.MapArea(shShape: THTMLShapes;
    iCoords: array of Integer; sUrl, sAlt: String);
begin
  CheckNesting(tcMap, False, True, True, False, False);
  if not bInMap and (erErrors in Errors) then
    raise EHTMLError.Create(tcMap, 'AREA must appear in a MAP block');

  bWrittenArea := True;
  Writeln(fOutput, FormatMapArea(shShape, iCoords, sUrl, sAlt));
end;

{ *****************************************************************************
  HTML list processing functions and procedures }

{ Return start of list with parameters as string }
function THTMLWriter.FormatListStartParams(ltList: THTMLListType;
    nsNum: THTMLNumberScheme; iStart: Byte): String;
begin
  Result := '<' + sListType[ltList];
  if ltList = ltOrdered then
  begin
    if nsNum <> nsDefault then
      Result := Result + ' type=' + sNumberScheme[nsNum];
    if iStart <> 0 then
      Result := Result + ' start=' + IntToStr(iStart);
  end;
  Result := Result + '>';

  if (nsNum <> nsDefault) or (iStart <> 0) then
  begin
    if (ltList <> ltOrdered) and (erWarnings in Errors) then
      raise EHTMLWarning.Create(tcList,
        'TYPE, VALUE only apply to ordered lists', Result);
    if (ltList = ltOrdered) and (erNetscape in Errors) then
     raise EHTMLWarning.Create(tcList, 'TYPE, VALUE are Netscape extensions', Result);
  end;
end;

{ Write start of list with parameters }
procedure THTMLWriter.ListStartParams(ltList: THTMLListType;
    nsNum: THTMLNumberScheme; iStart: Byte);
begin
  CheckNesting(tcList, False, True, False, True, True);
  if (ltList in [ltMenu, ltDirectory]) and (sTags.IndexOf(sListType[ltList]) >= 0) and
      (erErrors in Errors) then
    raise EHTMLError.Create(tcList, 'Menu and directory lists cannot be embedded');

  Inc(iCurList);
  with recListCheck[iCurList] do
  begin
    ListType := ltList;
    Elements := False;
  end;
  sTags.Add(sListType[ltList]);
  try
    Writeln(fOutput, FormatListStartParams(ltList, nsNum, iStart));
  except on e: EHTMLWarning do
    begin
      Writeln(fOutput, e.Result);
      raise;
    end;
  end;
end;

{ Return start of list as string }
function THTMLWriter.FormatListStart(ltList: THTMLListType): String;
begin
  Result := FormatListStartParams(ltList, nsDefault, 0);
end;

{ Write start of list }
procedure THTMLWriter.ListStart(ltList: THTMLListType);
begin
  ListStartParams(ltList, nsDefault, 0);
end;

{ Return end of list as string }
function THTMLWriter.FormatListEnd(ltList: THTMLListType): String;
begin
  Result := '</' + sListType[ltList] + '>';
end;

{ Write end of list }
procedure THTMLWriter.ListEnd(ltList: THTMLListType);
begin
  CheckClosing(tcList, (sTags.IndexOf(sListType[ltList]) < 0),
    sListType[ltList], sListDescription[ltList]);

  Dec(iCurList);
  sTags.Delete(sTags.Count - 1);
  Writeln(fOutput, FormatListEnd(ltList));

  if not recListCheck[iCurList + 1].Elements and (erWarnings in Errors) then
    raise EHTMLWarning.Create(tcList, 'No elements written for this list', EmptyStr);
end;

{ Return list element with parameters as string }
function THTMLWriter.FormatListItemParams(liItem: THTMLListItem;
    sText: String; nsNum: THTMLNumberScheme; iValue: Byte): String;
begin
  Result := '<' + sListItem[liItem];
  if nsNum <> nsDefault then
    Result := Result + ' type=' + sNumberScheme[nsNum];
  if iValue <> 0 then
    Result := Result + ' value=' + IntToStr(iValue);
  Result := Result + '>' + sText;

  if ((nsNum <> nsDefault) or (iValue <> 0)) and (erNetscape in Errors) then
    raise EHTMLWarning.Create(tcList, 'TYPE, VALUE are Netscape extensions', Result);
end;

{ Write list element with parameters }
procedure THTMLWriter.ListItemParams(liItem: THTMLListItem;
    sText: String; nsNum: THTMLNumberScheme; iValue: Byte);
begin
  if erErrors in Errors then
  begin
    if iCurList = 0 then
      raise EHTMLError.Create(tcList,
        'Cannot have a list element without a list');
    if (liItem = liNormal) and
        (recListCheck[iCurList].ListType = ltGlossary) then
      raise EHTMLError.Create(tcList,
        'Normal list elements cannot be used in a glossary list');
    if (liItem <> liNormal) and
        (recListCheck[iCurList].ListType <> ltGlossary) then
      raise EHTMLError.Create(tcList,
        'Definition list elements cannot be used in a non-glossary list');
end;

  recListCheck[iCurList].Elements := True;
  try
    Write(fOutput, FormatListItemParams(liItem, sText, nsNum, iValue));
  except on e: EHTMLWarning do
    begin
      Write(fOutput, e.Result);
      raise;
    end;
  end;

  if (nsNum <> nsDefault) or (iValue <> 0) then
  begin
    if (recListCheck[iCurList].ListType <> ltOrdered) and (erWarnings in Errors) then
      raise EHTMLWarning.Create(tcList,
        'TYPE, VALUE only apply to ordered lists', EmptyStr);
    if (recListCheck[iCurList].ListType = ltOrdered) and (erNetscape in Errors) then
      raise EHTMLWarning.Create(tcList,
        'TYPE, VALUE are Netscape extensions', EmptyStr);
  end;
end;

{ Return list element as string }
function THTMLWriter.FormatListItem(liItem: THTMLListItem;
    sText: String): String;
begin
  Result := FormatListItemParams(liItem, sText, nsDefault, 0);
end;

{ Write list element }
procedure THTMLWriter.ListItem(liItem: THTMLListItem; sText: String);
begin
  ListItemParams(liItem, sText, nsDefault, 0);
end;

{ *****************************************************************************
  HTML line break functions and procedures }

{ Return horizontal rule with parameters as string }
function THTMLWriter.FormatHorizRuleParams(iSize: Byte; iWidth: Integer;
    ahAlign: THTMLAlignHoriz; bNoShade: Boolean; crClear: THTMLClear): String;
begin
  Result := '<hr';
  if iSize <> 0 then
    Result := Result + ' size=' + IntToStr(iSize);
  if iWidth <> 0 then
    Result := Result + CheckIfPercentage('width', iWidth);
  if ahAlign <> ahDefault then
    Result := Result + ' align=' + sAlignHoriz[ahAlign];
  if bNoShade then
    Result := Result + ' noshade';
  if crClear <> crDefault then
    Result := Result + ' clear=' + sClear[crClear];
  Result := Result + '>';

  if ((iSize <> 0) or (iWidth <> 0) or (ahAlign <> ahDefault) or bNoShade) and
      (erNetscape in Errors) then
    raise EHTMLWarning.Create(tcBreak,
      'SIZE, WIDTH, ALIGN, NOSHADE are Netscape extensions', Result);
  if (crClear <> crDefault) and (erHTML3 in Errors) then
    raise EHTMLWarning.Create(tcBreak,
      'CLEAR is HTML 3.0 extension', Result);
end;

{ Write horizontal rule with parameters }
procedure THTMLWriter.HorizRuleParams(iSize: Byte; iWidth: Integer;
    ahAlign: THTMLAlignHoriz; bNoShade: Boolean; crClear: THTMLClear);
begin
  CheckNesting(tcBreak, False, True, False, True, True);

  try
    Writeln(fOutput,
      FormatHorizRuleParams(iSize, iWidth, ahAlign, bNoShade, crClear));
  except on e: EHTMLWarning do
    begin
      Writeln(fOutput, e.Result);
      raise;
    end;
  end;
end;

{ Return default horizontal rule as string }
function THTMLWriter.FormatHorizRule: String;
begin
  Result := FormatHorizRuleParams(0, 0, ahDefault, False, crDefault);
end;

{ Write default horizontal rule }
procedure THTMLWriter.HorizRule;
begin
  HorizRuleParams(0, 0, ahDefault, False, crDefault);
end;

{ Return line break as string }
function THTMLWriter.FormatLineBreak(crClear: THTMLClear): String;
begin
  Result := '<br';
  if crClear <> crDefault then
    Result := Result + ' clear=' + sClear[crClear];
  Result := Result + '>';

  if (crClear <> crDefault) and (erHTML3 in Errors) then
    raise EHTMLWarning.Create(tcBreak, 'CLEAR is HTML 3.0 extension', Result);
end;

{ Write line break }
procedure THTMLWriter.LineBreak(crClear: THTMLClear);
begin
  CheckNesting(tcBreak, False, True, False, True, True);

  try
    Writeln(fOutput, FormatLineBreak(crClear));
  except on e: EHTMLWarning do
    begin
      Writeln(fOutput, e.Result);
      raise;
    end;
  end;
end;

{ Return work break as string }
function THTMLWriter.FormatWordBreak: String;
begin
  Result := '<wbr>';

  if erNetscape in Errors then
    raise EHTMLWarning.Create(tcBreak, 'WBR is Netscape extension', Result);
end;

{ Write word break }
procedure THTMLWriter.WordBreak;
begin
  CheckNesting(tcBreak, False, True, False, True, True);

  try
    Write(fOutput, FormatWordBreak);
  except on e: EHTMLWarning do
    begin
      Write(fOutput, e.Result);
      raise;
    end;
  end;
end;

{ *****************************************************************************
  HTML character formatting functions and procedures }

{ Return start of text formatting as string }
function THTMLWriter.FormatTextEffectStart(efEffect: THTMLTextEffect): String;
begin
  Result := '<' + sTextEffect[efEffect] + '>';

  if (efEffect in [efCentre, efNonBreaking, efBlink]) and (erNetscape in Errors) then
    raise EHTMLWarning.Create(tcEffect,
      sTextEffect[efEffect] + ' is Netscape extension', Result);
  if (efEffect in [efUnderlined, efDefinition, efSuperscript, efSubscript,
      efInserted, efDeleted, efSmall, efBig]) and (erHTML3 in Errors) then
    raise EHTMLWarning.Create(tcEffect,
      sTextEffect[efEffect] + ' is HTML 3.0 extension', Result);
end;

{ Write start of text formatting }
procedure THTMLWriter.TextEffectStart(efEffect: THTMLTextEffect);
begin
  CheckNesting(tcEffect, False, True, False, True, True);
  if sTags.IndexOf(sTextEffect[efEffect]) >= 0 then
    raise EHTMLError.Create(tcEffect,
      'Effect ' + sTextEffect[efEffect] + ' already being applied');

  sTags.Add(sTextEffect[efEffect]);
  try
    Write(fOutput, FormatTextEffectStart(efEffect));
  except on e: EHTMLWarning do
    begin
      Write(fOutput, e.Result);
      raise;
    end;
  end;
end;

{ Return end of text formatting as string }
function THTMLWriter.FormatTextEffectEnd(efEffect: THTMLTextEffect): String;
begin
  Result := '</' + sTextEffect[efEffect] + '>';
end;

{ Write end of text formatting }
procedure THTMLWriter.TextEffectEnd(efEffect: THTMLTextEffect);
begin
  CheckClosing(tcEffect, (sTags.IndexOf(sTextEffect[efEffect]) < 0),
    sTextEffect[efEffect], sEffectDescription[efEffect]);

  sTags.Delete(sTags.Count - 1);
  Write(fOutput, FormatTextEffectEnd(efEffect));
end;

{ Return text formatting as string }
function THTMLWriter.FormatTextEffect(efEffect: THTMLTextEffect;
    sText: String): String;
begin
  Result := FormatTextEffectStart(efEffect) + sText +
    FormatTextEffectEnd(efEffect);
end;

{ Write text formatting }
procedure THTMLWriter.TextEffect(efEffect: THTMLTextEffect; sText: String);
begin
  TextEffectStart(efEffect);
  Write(fOutput, sText);
  TextEffectEnd(efEffect);
end;

{ Return start of font change as string }
function THTMLWriter.FormatFontStart(iSize: THTMLFontSize;
    fcChange: THTMLFontChange; sFace: String; clrColour: TColor): String;
begin
  Result := '<font';
  if iSize > 0 then
    Result := Result + ' size=' + sFontChange[fcChange] + IntToStr(iSize);
  if sFace <> EmptyStr then
    Result := Result + ' face="' + sFace + '"';
  if clrColour <> clDefault then
    Result := Result + ' color=' + ConvertColour(clrColour);
  Result := Result + '>';

  if (iSize > 0) and (erNetscape in Errors) then
    raise EHTMLWarning.Create(tcEffect,
      'FONT : SIZE is Netscape extension', Result);
  if ((sFace <> EmptyStr) or (clrColour <> clDefault)) and (erIExplorer in Errors) then
    raise EHTMLWarning.Create(tcEffect,
      'FONT : FACE, COLOR are IExplorer extensions', Result);
end;

{ Write start of font change }
procedure THTMLWriter.FontStart(iSize: THTMLFontSize;
    fcChange: THTMLFontChange; sFace: String; clrColour: TColor);
begin
  CheckNesting(tcEffect, False, True, False, True, True);

  sTags.Add('font');
  try
    Write(fOutput, FormatFontStart(iSize, fcChange, sFace, clrColour));
  except on e: EHTMLWarning do
    begin
      Write(fOutput, e.Result);
      raise;
    end;
  end;
end;

{ Return end of font change as string }
function THTMLWriter.FormatFontEnd: String;
begin
  Result := '</font>';
end;

{ Write end of font change }
procedure THTMLWriter.FontEnd;
begin
  CheckClosing(tcEffect, (sTags.IndexOf('font') < 0), 'font', 'font');

  sTags.Delete(sTags.Count - 1);
  Write(fOutput, FormatFontEnd);
end;

{ Return font change as string }
function THTMLWriter.FormatFont(sText: String; iSize: THTMLFontSize;
    fcChange: THTMLFontChange; sFace: String; clrColour: TColor): String;
begin
  Result := FormatFontStart(iSize, fcChange, sFace, clrColour) + sText +
    FormatFontEnd;
end;

{ Write font change }
procedure THTMLWriter.Font(sText: String; iSize: THTMLFontSize;
    fcChange: THTMLFontChange; sFace: String; clrColour: TColor);
begin
  FontStart(iSize, fcChange, sFace, clrColour);
  Write(fOutput, sText);
  FontEnd;
end;

{ Return base font as string }
function THTMLWriter.FormatBaseFont(iSize: THTMLFontSize): String;
begin
  Result := '<basefont size=' + IntToStr(iSize) + '>';

  if erNetscape in Errors then
    raise EHTMLWarning.Create(tcEffect, 'BASEFONT is Netscape extension', Result);
end;

{ Write base font }
procedure THTMLWriter.BaseFont(iSize: THTMLFontSize);
begin
  CheckNesting(tcEffect, False, True, False, True, True);

  try
    Writeln(fOutput, FormatBaseFont(iSize));
  except on e: EHTMLWarning do
    begin
      Writeln(fOutput, e.Result);
      raise;
    end;
  end;
end;

{ Return special character as string }
function THTMLWriter.FormatSpecialChar(scSpecial: THTMLSpecialChar): String;
begin
  Result := '&' + sSpecialChar[scSpecial] + ';';
end;

{ Write special character }
procedure THTMLWriter.SpecialChar(scSpecial: THTMLSpecialChar);
begin
  CheckNesting(tcEffect, False, True, False, True, True);

  Write(fOutput, FormatSpecialChar(scSpecial));
end;

{ Return special character by value as string }
function THTMLWriter.FormatSpecialCharValue(iValue: Byte): String;
begin
  Result := '&#' + IntToStr(iValue) + ';';
end;

{ Write special character by value }
procedure THTMLWriter.SpecialCharValue(iValue: Byte);
begin
  CheckNesting(tcEffect, False, True, False, True, True);

  Write(fOutput, FormatSpecialCharValue(iValue));
end;

{ Return start of marquee as string }
function THTMLWriter.FormatMarqueeStart(avAlign: THTMLAlignVert;
    iHeight, iWidth: Integer; iHSpace, iVSpace, iScrollAmount: Byte;
    iScrollDelay: Integer; mbBehave: THTMLMarqueeBehave;
    mdDir: THTMLMarqueeDirection; iLoop: Byte; clrColour: TColor): String;
begin
  Result := '<marquee behaviour=' + sMarqueeBehaviour[mbBehave] +
    ' direction=' + sMarqueeDirection[mdDir];
  if avAlign <> avDefault then
    Result := Result + ' align=' + sAlignVert[avAlign];
  if iHeight <> 0 then
    Result := Result + CheckIfPercentage('height', iHeight);
  if iWidth <> 0 then
    Result := Result + CheckIfPercentage('width', iWidth);
  if iHSpace <> 0 then
    Result := Result + ' hspace=' + IntToStr(iHSpace);
  if iVSpace <> 0 then
    Result := Result + ' vspace=' + IntToStr(iVSpace);
  if iScrollAmount <> 0 then
    Result := Result + ' scrollamount=' + IntToStr(iScrollAmount);
  if iScrollDelay <> 0 then
    Result := Result + ' scrolldelay=' + IntToStr(iScrollDelay);
  if iLoop = 0 then
    Result := Result + ' loop=infinite'
  else
    Result := Result + ' loop=' + IntToStr(iLoop);
  if clrColour <> clDefault then
    Result := Result + ' bgcolor=' + ConvertColour(clrColour);
  Result := Result + '>';

  if erIExplorer in Errors then
    raise EHTMLWarning.Create(tcEffect, 'MARQUEE is IExplorer extension', Result);
end;

{ Write start of marquee }
procedure THTMLWriter.MarqueeStart(avAlign: THTMLAlignVert;
    iHeight, iWidth: Integer; iHSpace, iVSpace, iScrollAmount: Byte;
    iScrollDelay: Integer; mbBehave: THTMLMarqueeBehave;
    mdDir: THTMLMarqueeDirection; iLoop: Byte; clrColour: TColor);
begin
  CheckNesting(tcEffect, False, True, False, True, True);
  if sTags.IndexOf('marquee') >= 0 then
    raise EHTMLError.Create(tcEffect, 'Marquee already being applied');

  sTags.Add('marquee');
  try
    Write(fOutput, 
      FormatMarqueeStart(avAlign, iHeight, iWidth, iHSpace, iVSpace,
      iScrollAmount, iScrollDelay, mbBehave, mdDir, iLoop, clrColour));
  except on e: EHTMLWarning do
    begin
      Write(fOutput, e.Result);
      raise;
    end;
  end;
end;

{ Return end of marquee as string }
function THTMLWriter.FormatMarqueeEnd: String;
begin
  Result := '</marquee>';
end;

{ Write end of marquee }
procedure THTMLWriter.MarqueeEnd;
begin
  CheckClosing(tcEffect, (sTags.IndexOf('marquee') < 0), 'marquee', 'marquee');

  sTags.Delete(sTags.Count - 1);
  Writeln(fOutput, FormatMarqueeEnd);
end;

{ Return marquee as string }
function THTMLWriter.FormatMarquee(sText: String; avAlign: THTMLAlignVert;
    iHeight, iWidth: Integer; iHSpace, iVSpace, iScrollAmount: Byte;
    iScrollDelay: Integer; mbBehave: THTMLMarqueeBehave;
    mdDir: THTMLMarqueeDirection; iLoop: Byte; clrColour: TColor): String;
begin
  Result := FormatMarqueeStart(avAlign, iHeight, iWidth, iHSpace, iVSpace,
    iScrollAmount, iScrollDelay, mbBehave, mdDir, iLoop, clrColour) + sText +
    FormatMarqueeEnd;
end;

{ Write marquee }
procedure THTMLWriter.Marquee(sText: String; avAlign: THTMLAlignVert;
    iHeight, iWidth: Integer; iHSpace, iVSpace, iScrollAmount: Byte;
    iScrollDelay: Integer; mbBehave: THTMLMarqueeBehave;
    mdDir: THTMLMarqueeDirection; iLoop: Byte; clrColour: TColor);
begin
  MarqueeStart(avAlign, iHeight, iWidth, iHSpace, iVSpace,
    iScrollAmount, iScrollDelay, mbBehave, mdDir, iLoop, clrColour);
  Write(fOutput, sText);
  MarqueeEnd;
end;

{ *****************************************************************************
  HTML link functions and procedures }

{ Return start of link as string }
function THTMLWriter.FormatLinkStart(sUrl, sName: String): String;
begin
  if (sUrl = EmptyStr) and (sName = EmptyStr) and (erErrors in Errors) then
    raise EHTMLError.Create(tcLink, 'No URL or name supplied');

  Result := '<a';
  if sUrl <> EmptyStr then
    Result := Result + ' href="' + sUrl + '"';
  if sName <> EmptyStr then
    Result := Result + ' name="' + sName + '"';
  Result := Result + '>';
end;

{ Write start of link }
procedure THTMLWriter.LinkStart(sUrl, sName: String);
begin
  CheckNesting(tcLink, False, True, False, True, True);
  if bInLink and (erErrors in Errors) then
    raise EHTMLError.Create(tcLink, 'Already in a link');

  bInLink := True;
  sTags.Add('a');
  Write(fOutput, FormatLinkStart(sUrl, sName));
end;

{ Return end of link as string }
function THTMLWriter.FormatLinkEnd: String;
begin
  Result := '</a>';
end;

{ Write end of link }
procedure THTMLWriter.LinkEnd;
begin
  CheckClosing(tcLink, not bInLink, 'a', 'link');

  bInLink := False;
  sTags.Delete(sTags.Count - 1);
  Write(fOutput, FormatLinkEnd);
end;

{ Return link as string }
function THTMLWriter.FormatLink(sUrl, sName, sText: String): String;
begin
  Result := FormatLinkStart(sUrl, sName) + sText + FormatLinkEnd;
end;

{ Write link }
procedure THTMLWriter.Link(sUrl, sName, sText: String);
begin
  LinkStart(sUrl, sName);
  Write(fOutput, sText);
  LinkEnd;
end;

{ *****************************************************************************
  HTML table functions and procedures }

{ Return border colours as string }
function THTMLWriter.FormatBorderColours(
    clrBackground, clrBorder, clrBorderLight, clrBorderDark: TColor): String;
begin
  Result := EmptyStr;
  if clrBackground <> clDefault then
    Result := ' bgcolor=' + ConvertColour(clrBackground);
  if clrBorder <> clDefault then
    Result := Result + ' bordercolor=' + ConvertColour(clrBorder);
  if clrBorderLight <> clDefault then
    Result := Result + ' bordercolorlight=' + ConvertColour(clrBorderLight);
  if clrBorderDark <> clDefault then
    Result := Result + ' bordercolordark=' + ConvertColour(clrBorderDark);
end;

{ Check if border colours used - warn if required }
procedure THTMLWriter.CheckBorderColours(
    clrBackground, clrBorder, clrBorderLight, clrBorderDark: TColor;
    sResult: String);
begin
  if ((clrBackground <> clDefault) or (clrBorder <> clDefault) or
      (clrBorderLight <> clDefault) or (clrBorderDark <> clDefault)) and
      (erIExplorer in Errors) then
    raise EHTMLWarning.Create(tcTable,
      'BGCOLOR, BORDERCOLOR, BORDERCOLORLIGHT, BORDERCOLORDARK are IExplorer extensions', sResult);
end;

{ Return start of table as string }
function THTMLWriter.FormatTableStartParams(iBorder: Byte; iWidth: Integer;
    iCellSpacing, iCellPadding: Byte;
    clrBackground, clrBorder, clrBorderLight, clrBorderDark: TColor;
    sCaption: String; ahCaptionHAlign: THTMLAlignHoriz;
    avCaptionVAlign: THTMLAlignVert): String;
begin
  Result := '<table border=' + IntToStr(iBorder);
  if iWidth <> 0 then
    Result := Result + CheckIfPercentage('width', iWidth);
  if iCellSpacing <> 0 then
    Result := Result + ' cellspacing=' + IntToStr(iCellSpacing);
  if iCellPadding <> 0 then
    Result := Result + ' cellpadding=' + IntToStr(iCellPadding);
  Result := Result + 
    FormatBorderColours(clrBackground, clrBorder, clrBorderLight, clrBorderDark) + '>';

  if sCaption <> EmptyStr then
  begin
    Result := Result + sNewline + '<caption';
    if ahCaptionHAlign <> ahDefault then
      Result := Result + ' align=' + sAlignHoriz[ahCaptionHAlign];
    if avCaptionVAlign <> avDefault then
      Result := Result + ' valign=' + sAlignVert[avCaptionVAlign];
    Result := Result + '>' + sCaption + '</caption>';
  end;

  if erHTML3 in Errors then
    raise EHTMLWarning.Create(tcTable, 'TABLE is HTML 3.0 extension', Result);
  if ((iBorder <> 0) or (iCellPadding <> 0) or (iCellSpacing <> 0)) and
      (erNetscape in Errors) then
    raise EHTMLWarning.Create(tcTable,
      'BORDER, CELLPADDING, CELLSPACING are Netscape extensions', Result);
  CheckBorderColours(clrBackground, clrBorder, clrBorderLight, clrBorderDark, Result);
end;

{ Write start of table with all parameters }
procedure THTMLWriter.TableStartParams(iBorder: Byte; iWidth: Integer;
    iCellSpacing, iCellPadding: Byte;
    clrBackground, clrBorder, clrBorderLight, clrBorderDark: TColor;
    sCaption: String; ahCaptionHAlign: THTMLAlignHoriz;
    avCaptionVAlign: THTMLAlignVert);
begin
  CheckNesting(tcTable, False, True, False, True, True);

  iTableLevels := iTableLevels + 1;
  sTags.Add('table');
  try
    Writeln(fOutput, FormatTableStartParams(iBorder, iWidth,
      iCellSpacing, iCellPadding, clrBackground, clrBorder, clrBorderLight,
      clrBorderDark, sCaption, ahCaptionHAlign, avCaptionVAlign));
  except on e: EHTMLWarning do
    begin
      Writeln(fOutput, e.Result);
      raise;
    end;
  end;
end;

{ Return start of table as string }
function THTMLWriter.FormatTableStart(iBorder: Byte; iWidth: Integer): String;
begin
  Result := FormatTableStartParams(iBorder, iWidth, 0, 0, clDefault,
    clDefault, clDefault, clDefault, EmptyStr, ahDefault, avDefault);
end;

{ Write start of table }
procedure THTMLWriter.TableStart(iBorder: Byte; iWidth: Integer);
begin
  TableStartParams(iBorder, iWidth, 0, 0, clDefault, clDefault,
    clDefault, clDefault, EmptyStr, ahDefault, avDefault);
end;

{ Return end of table as string }
function THTMLWriter.FormatTableEnd: String;
begin
  Result := '</table>';
end;

{ Write end of table }
procedure THTMLWriter.TableEnd;
begin
  CheckClosing(tcTable, (iTableLevels = 0), 'table', 'table');

  iTableLevels := iTableLevels - 1;
  sTags.Delete(sTags.Count - 1);
  Writeln(fOutput, FormatTableEnd);
end;

{ Return start of table row with parameters as string }
function THTMLWriter.FormatTableRowStartParams(
    ahAlign: THTMLAlignHoriz; avAlign: THTMLAlignVert;
    clrBackground, clrBorder, clrBorderLight, clrBorderDark: TColor): String;
begin
  Result := '<tr';
  if ahAlign <> ahDefault then
    Result := Result + ' align=' + sAlignHoriz[ahAlign];
  if avAlign <> avDefault then
    Result := Result + ' valign=' + sAlignVert[avAlign];
  Result := Result + 
    FormatBorderColours(clrBackground, clrBorder, clrBorderLight, clrBorderDark) + '>';

  CheckBorderColours(clrBackground, clrBorder, clrBorderLight, clrBorderDark, Result);
end;

{ Write start of table row with parameters }
procedure THTMLWriter.TableRowStartParams(
    ahAlign: THTMLAlignHoriz; avAlign: THTMLAlignVert;
    clrBackground, clrBorder, clrBorderLight, clrBorderDark: TColor);
begin
  if (sTags[sTags.Count - 1] <> 'table') and (erErrors in Errors) then
    raise EHTMLError.Create(tcTable, 'Table rows can only appear within a table');

  sTags.Add('tr');
  try
    Write(fOutput, FormatTableRowStartParams(ahAlign, avAlign,
      clrBackground, clrBorder, clrBorderLight, clrBorderDark));
  except on e: EHTMLWarning do
    begin
      Write(fOutput, e.Result);
      raise e
    end;
  end;
end;

{ Return start of table row as string }
function THTMLWriter.FormatTableRowStart: String;
begin
  Result := FormatTableRowStartParams(ahDefault, avDefault,
    clDefault, clDefault, clDefault, clDefault);
end;

{ Write start of table row }
procedure THTMLWriter.TableRowStart;
begin
  TableRowStartParams(ahDefault, avDefault, clDefault, clDefault, clDefault, clDefault);
end;

{ Return end of table row as string }
function THTMLWriter.FormatTableRowEnd: String;
begin
  Result := '</tr>';
end;

{ Write end of table row }
procedure THTMLWriter.TableRowEnd;
begin
  CheckClosing(tcTable, (iTableLevels = 0), 'tr', 'table row');

  sTags.Delete(sTags.Count - 1);
  Writeln(fOutput, FormatTableRowEnd);
end;

{ Return start of table heading with parameters as string }
function THTMLWriter.FormatTableHeadingStartParams(iRowSpan, iColSpan: Byte;
    ahAlign: THTMLAlignHoriz; avAlign: THTMLAlignVert;
    clrBackground, clrBorder, clrBorderLight, clrBorderDark: TColor): String;
begin
  Result := '<th';
  if iRowSpan <> 0 then
    Result := Result + ' rowspan=' + IntToStr(iRowSpan);
  if iColSpan <> 0 then
    Result := Result + ' colspan=' + IntToStr(iColSpan);
  if ahAlign <> ahDefault then
    Result := Result + ' align=' + sAlignHoriz[ahAlign];
  if avAlign <> avDefault then
    Result := Result + ' valign=' + sAlignVert[avAlign];
  Result := Result + 
    FormatBorderColours(clrBackground, clrBorder, clrBorderLight, clrBorderDark) + '>';

  CheckBorderColours(clrBackground, clrBorder, clrBorderLight, clrBorderDark, Result);
end;

{ Write start of table heading with parameters }
procedure THTMLWriter.TableHeadingStartParams(iRowSpan, iColSpan: Byte;
    ahAlign: THTMLAlignHoriz; avAlign: THTMLAlignVert;
    clrBackground, clrBorder, clrBorderLight, clrBorderDark: TColor);
begin
  if (iTableLevels = 0) and (erErrors in Errors) then
    raise EHTMLError.Create(tcTable,
      'Table headings can only appear within a table');
  if (sTags[sTags.Count - 1] <> 'tr') and (erErrors in Errors) then
    raise EHTMLError.Create(tcTable,
      'Table heading must occur within a table row');

  sTags.Add('th');
  try
    Write(fOutput, 
      FormatTableHeadingStartParams(iRowSpan, iColSpan, ahAlign, avAlign,
      clrBackground, clrBorder, clrBorderLight, clrBorderDark));
  except on e: EHTMLWarning do
    begin
      Write(fOutput, e.Result);
      raise;
    end;
  end;
end;

{ Return start of table heading as string }
function THTMLWriter.FormatTableHeadingStart: String;
begin
  Result := FormatTableHeadingStartParams(0, 0, ahDefault, avDefault,
    clDefault, clDefault, clDefault, clDefault);
end;

{ Write start of table heading }
procedure THTMLWriter.TableHeadingStart;
begin
  TableHeadingStartParams(0, 0, ahDefault, avDefault,
    clDefault, clDefault, clDefault, clDefault);
end;

{ Return end of table heading as string }
function THTMLWriter.FormatTableHeadingEnd: String;
begin
  Result := '</th>';
end;

{ Write end of table heading }
procedure THTMLWriter.TableHeadingEnd;
begin
  CheckClosing(tcTable, (iTableLevels = 0), 'th', 'table heading');

  sTags.Delete(sTags.Count - 1);
  Write(fOutput, FormatTableHeadingEnd);
end;

{ Return table heading with parameters as string }
function THTMLWriter.FormatTableHeadingParams(sHeading: String;
    iRowSpan, iColSpan: Byte; ahAlign: THTMLAlignHoriz; avAlign: THTMLAlignVert;
    clrBackground, clrBorder, clrBorderLight, clrBorderDark: TColor): String;
begin
  Result := FormatTableHeadingStartParams(iRowSpan, iColSpan, ahAlign, avAlign,
    clrBackground, clrBorder, clrBorderLight, clrBorderDark) + sHeading +
    FormatTableHeadingEnd;
end;

{ Write table heading with parameters }
procedure THTMLWriter.TableHeadingParams(sHeading: String;
    iRowSpan, iColSpan: Byte; ahAlign: THTMLAlignHoriz; avAlign: THTMLAlignVert;
    clrBackground, clrBorder, clrBorderLight, clrBorderDark: TColor);
begin
  TableHeadingStartParams(iRowSpan, iColSpan, ahAlign, avAlign,
    clrBackground, clrBorder, clrBorderLight, clrBorderDark);
  Write(fOutput, sHeading);
  TableHeadingEnd;
end;

{ Return table heading as string }
function THTMLWriter.FormatTableHeading(sHeading: String): String;
begin
  Result := FormatTableHeadingParams(sHeading, 0, 0, ahDefault, avDefault,
    clDefault, clDefault, clDefault, clDefault);
end;

{ Write table heading }
procedure THTMLWriter.TableHeading(sHeading: String);
begin
  TableHeadingParams(sHeading, 0, 0, ahDefault, avDefault,
    clDefault, clDefault, clDefault, clDefault);
end;

{ Return start of table cell with parameters as string }
function THTMLWriter.FormatTableCellStartParams(iRowSpan, iColSpan: Byte;
    iWidth: Integer; ahAlign: THTMLAlignHoriz; avAlign: THTMLAlignVert;
    clrBackground, clrBorder, clrBorderLight, clrBorderDark: TColor): String;
begin
  Result := '<td';
  if iRowSpan <> 0 then
    Result := Result + ' rowspan=' + IntToStr(iRowSpan);
  if iColSpan <> 0 then
    Result := Result + ' colspan=' + IntToStr(iColSpan);
  if ahAlign <> ahDefault then
    Result := Result + ' align=' + sAlignHoriz[ahAlign];
  if avAlign <> avDefault then
    Result := Result + ' valign=' + sAlignVert[avAlign];
  if iWidth <> 0 then
    Result := Result + CheckIfPercentage('width', iWidth);
  Result := Result + 
    FormatBorderColours(clrBackground, clrBorder, clrBorderLight, clrBorderDark) + '>';

  if (iWidth <> 0) and (erNetscape in Errors) then
    raise EHTMLWarning.Create(tcTable, 'WIDTH is Netscape extension', Result);
  CheckBorderColours(clrBackground, clrBorder, clrBorderLight, clrBorderDark, Result);
end;

{ Write start of table cell with parameters }
procedure THTMLWriter.TableCellStartParams(iRowSpan, iColSpan: Byte;
    iWidth: Integer; ahAlign: THTMLAlignHoriz; avAlign: THTMLAlignVert;
    clrBackground, clrBorder, clrBorderLight, clrBorderDark: TColor);
begin
  if (iTableLevels = 0) and (erErrors in Errors) then
    raise EHTMLError.Create(tcTable,
      'Table cells can only appear within a table');
  if (sTags[sTags.Count - 1] <> 'tr') and (erErrors in Errors) then
    raise EHTMLError.Create(tcTable,
      'Table cell must occur within a table row');

  sTags.Add('td');
  try
    Write(fOutput, 
      FormatTableCellStartParams(iRowSpan, iColSpan, iWidth, ahAlign, avAlign,
      clrBackground, clrBorder, clrBorderLight, clrBorderDark));
  except on e: EHTMLWarning do
    begin
      Write(fOutput, e.Result);
      raise;
    end;
  end;
end;

{ Return start of table cell as string }
function THTMLWriter.FormatTableCellStart: String;
begin
  Result := FormatTableCellStartParams(0, 0, 0, ahDefault, avDefault,
    clDefault, clDefault, clDefault, clDefault);
end;

{ Write start of table cell }
procedure THTMLWriter.TableCellStart;
begin
  TableCellStartParams(0, 0, 0, ahDefault, avDefault,
    clDefault, clDefault, clDefault, clDefault);
end;

{ Return end of table cell as string }
function THTMLWriter.FormatTableCellEnd: String;
begin
  Result := '</td>';
end;

{ Write end of table cell }
procedure THTMLWriter.TableCellEnd;
begin
  CheckClosing(tcTable, (iTableLevels = 0), 'td', 'table cell');

  sTags.Delete(sTags.Count - 1);
  Write(fOutput, FormatTableCellEnd);
end;

{ Return table cell with parameters as string }
function THTMLWriter.FormatTableCellParams(sText: String;
    iRowSpan, iColSpan: Byte; iWidth: Integer;
    ahAlign: THTMLAlignHoriz; avAlign: THTMLAlignVert;
    clrBackground, clrBorder, clrBorderLight, clrBorderDark: TColor): String;
begin
  Result := FormatTableCellStartParams(iRowSpan, iColSpan, iWidth, ahAlign, avAlign,
    clrBackground, clrBorder, clrBorderLight, clrBorderDark) + sText +
    FormatTableCellEnd;
end;

{ Write table cell with parameters }
procedure THTMLWriter.TableCellParams(sText: String; iRowSpan, iColSpan: Byte;
    iWidth: Integer; ahAlign: THTMLAlignHoriz; avAlign: THTMLAlignVert;
    clrBackground, clrBorder, clrBorderLight, clrBorderDark: TColor);
begin
  TableCellStartParams(iRowSpan, iColSpan, iWidth, ahAlign, avAlign,
    clrBackground, clrBorder, clrBorderLight, clrBorderDark);
  Write(fOutput, sText);
  TableCellEnd;
end;

{ Return table cell as string }
function THTMLWriter.FormatTableCell(sText: String): String;
begin
  Result := FormatTableCellParams(sText, 0, 0, 0, ahDefault, avDefault,
    clDefault, clDefault, clDefault, clDefault);
end;

{ Write table cell }
procedure THTMLWriter.TableCell(sText: String);
begin
  TableCellParams(sText, 0, 0, 0, ahDefault, avDefault,
    clDefault, clDefault, clDefault, clDefault);
end;

{ *****************************************************************************
  HTML form functions and procedures }

{ Return start of form as string }
function THTMLWriter.FormatFormStart(sUrl: String;
    fmMethod: THTMLFormMethod): String;
begin
  if (sUrl = EmptyStr) and (erErrors in Errors) then
    raise EHTMLError.Create(tcForm, 'Missing URL for form');

  Result := '<form method=' + sFormMethod[fmMethod] + ' action="' + sUrl + '">';
end;

{ Write start of form }
procedure THTMLWriter.FormStart(sUrl: String; fmMethod: THTMLFormMethod);
begin
  CheckNesting(tcForm, False, True, False, True, False);

  bInForm := True;
  bSubmit := False;
  sTags.Add('form');
  Writeln(fOutput, FormatFormStart(sUrl, fmMethod));
end;

{ Return end of form as string }
function THTMLWriter.FormatFormEnd: String;
begin
  Result := '</form>';
end;

{ Write end of form }
procedure THTMLWriter.FormEnd;
begin
  CheckClosing(tcForm, not bInForm, 'form', 'form');

  sTags.Delete(sTags.Count - 1);
  bInForm := False;
  Writeln(fOutput, FormatFormEnd);

  if not bSubmit and (erWarnings in Errors) then
    raise EHTMLWarning.Create(tcForm, 'Nothing to submit form with', EmptyStr);
end;

{ Return input field on form as string }
function THTMLWriter.FormatInputField(ifField: THTMLInputField;
    sName, sValue: String; bChecked: Boolean; iSize, iMaxLength: Byte;
    sImage: String; ahAlign: THTMLAlignHoriz; iBorder: Byte): String;
begin
  if erErrors in Errors then
  begin
    if (ifField <> ifReset) and (sName = EmptyStr) then
      raise EHTMLError.Create(tcForm, 'Missing name for input field');
    if (ifField in [ifSubmit, ifReset]) and (sValue = EmptyStr) then
      raise EHTMLError.Create(tcForm, 'Missing label for button');
    if (ifField = ifImage) and (sImage = EmptyStr) then
      raise EHTMLError.Create(tcForm, 'Missing image source');
  end;

  Result := '<input type=' + sInputField[ifField];
  if sName <> EmptyStr then
    Result := Result + ' name="' + sName + '"';
  if sValue <> EmptyStr then
    Result := Result + ' value="' + sValue + '"';
  if bChecked then
    Result := Result + ' checked';
  if iSize <> 0 then
    Result := Result + ' size=' + IntToStr(iSize);
  if iMaxLength <> 0 then
    Result := Result + ' maxlength=' + IntToStr(iMaxLength);
  if ahAlign <> ahDefault then
    Result := Result + ' align=' + sAlignHoriz[ahAlign];
  if sImage <> EmptyStr then
    Result := Result + ' src="' + sImage + '"';
  if iBorder <> 0 then
    Result := Result + ' border=' + IntToStr(iBorder);
  Result := Result + '>';
end;

{ Check positioning of fields }
procedure THTMLWriter.CheckField;
begin
  if erErrors in Errors then
  begin
    if not bInForm then
      raise EHTMLError.Create(tcForm, 'Input field can only appear in a form');
    if bInSelect or bInTextArea then
      raise EHTMLError.Create(tcForm,
        'Input field cannot be embedded in select or text area fields');
  end;
end;

{ Return text field on form as string }
function THTMLWriter.FormatTextField(sName, sDefault: String;
    iSize, iMaxLength: Byte): String;
begin
  Result := FormatInputField(ifText, sName, sDefault, False,
    iSize, iMaxLength, EmptyStr, ahDefault, 0);
end;

{ Write text field on form }
procedure THTMLWriter.TextField(sName, sDefault: String; iSize, iMaxLength: Byte);
begin
  CheckField;

  Write(fOutput, FormatTextField(sName, sDefault, iSize, iMaxLength));
end;

{ Return password field on form as string }
function THTMLWriter.FormatPasswordField(sName, sDefault: String;
    iSize, iMaxLength: Byte): String;
begin
  Result := FormatInputField(ifPassword, sName, sDefault, False,
    iSize, iMaxLength, EmptyStr, ahDefault, 0);
end;

{ Write password field on form }
procedure THTMLWriter.PasswordField(sName, sDefault: String;
    iSize, iMaxLength: Byte);
begin
  CheckField;

  Write(fOutput, FormatPasswordField(sName, sDefault, iSize, iMaxLength));
end;

{ Return check box field on form as string }
function THTMLWriter.FormatCheckboxField(sName, sValue: String;
    bChecked: Boolean): String;
begin
  Result := FormatInputField(ifCheckbox, sName, sValue, bChecked,
    0, 0, EmptyStr, ahDefault, 0);
end;

{ Write check box field on form }
procedure THTMLWriter.CheckboxField(sName, sValue: String; bChecked: Boolean);
begin
  CheckField;

  Write(fOutput, FormatCheckboxField(sName, sValue, bChecked));
end;

{ Return radio button field on form as string }
function THTMLWriter.FormatRadioField(sName, sValue: String;
    bChecked: Boolean): String;
begin
  Result := FormatInputField(ifRadio, sName, sValue, bChecked,
    0, 0, EmptyStr, ahDefault, 0);
end;

{ Write radio button field on form }
procedure THTMLWriter.RadioField(sName, sValue: String; bChecked: Boolean);
begin
  CheckField;

  Write(fOutput, FormatRadioField(sName, sValue, bChecked));
end;

{ Return submit button on form as string }
function THTMLWriter.FormatSubmitField(sName, sLabel: String): String;
begin
  Result := FormatInputField(ifSubmit, sName, sLabel, False,
    0, 0, EmptyStr, ahDefault, 0);
end;

{ Write submit button on form }
procedure THTMLWriter.SubmitField(sName, sLabel: String);
begin
  CheckField;

  bSubmit := True;
  Write(fOutput, FormatSubmitField(sName, sLabel));
end;

{ Return reset button on form as string }
function THTMLWriter.FormatResetField(sLabel: String): String;
begin
  Result := FormatInputField(ifReset, EmptyStr, sLabel, False,
    0, 0, EmptyStr, ahDefault, 0);
end;

{ Write reset button on form }
procedure THTMLWriter.ResetField(sLabel: String);
begin
  CheckField;

  Write(fOutput, FormatResetField(sLabel));
end;

{ Return image field on form as string }
function THTMLWriter.FormatImageField(sName, sImage: String;
    ahAlign: THTMLAlignHoriz; iBorder: Byte): String;
begin
  Result := FormatInputField(ifImage, sName, EmptyStr, False,
    0, 0, sImage, ahAlign, iBorder);
end;

{ Write image field on form }
procedure THTMLWriter.ImageField(sName, sImage: String;
    ahAlign: THTMLAlignHoriz; iBorder: Byte);
begin
  CheckField;

  bSubmit := True;
  Write(fOutput, FormatImageField(sName, sImage, ahAlign, iBorder));
end;

{ Return hidden field on form as string }
function THTMLWriter.FormatHiddenField(sName, sValue: String): String;
begin
  Result := FormatInputField(ifHidden, sName, sValue, False,
    0, 0, EmptyStr, ahDefault, 0);
end;

{ Write hidden field on form }
procedure THTMLWriter.HiddenField(sName, sValue: String);
begin
  CheckField;

  Write(fOutput, FormatHiddenField(sName, sValue));
end;

{ Return start of select field on form as string }
function THTMLWriter.FormatSelectStart(sName: String; iSize: Byte;
    bMultiple: Boolean): String;
begin
  if (sName = EmptyStr) and (erErrors in Errors) then
    raise EHTMLError.Create(tcForm, 'Missing name for select field');

  Result := '<select name="' + sName + '"';
  if iSize <> 0 then
    Result := Result + ' size=' + IntToStr(iSize);
  if bMultiple then
    Result := Result + ' multiple';
  Result := Result + '>';
end;

{ Write start of select field on form }
procedure THTMLWriter.SelectStart(sName: String; iSize: Byte;
    bMultiple: Boolean);
begin
  if erErrors in Errors then
  begin
    if not bInForm then
      raise EHTMLError.Create(tcForm, 'Select field can only appear in a form');
    if bInSelect or bInTextArea then
      raise EHTMLError.Create(tcForm,
        'Select field cannot be embedded in select or text area fields');
  end;

  bInSelect := True;
  bSelectOption := False;
  sTags.Add('select');
  Write(fOutput, FormatSelectStart(sName, iSize, bMultiple));
end;

{ Return end of select field on form as string }
function THTMLWriter.FormatSelectEnd: String;
begin
  Result := '</select>';
end;

{ Write end of select field on form }
procedure THTMLWriter.SelectEnd;
begin
  CheckClosing(tcForm, not bInSelect, 'select', 'select field');

  bInSelect := False;
  sTags.Delete(sTags.Count - 1);
  Write(fOutput, FormatSelectEnd);

  if not bSelectOption and (erWarnings in Errors) then
    raise EHTMLWarning.Create(tcForm, 'No options for select', EmptyStr);
end;

{ Return select field option on form as string }
function THTMLWriter.FormatSelectOption(sText, sValue: String;
    bSelected: Boolean): String;
begin
  Result := '<option';
  if bSelected then
    Result := Result + ' selected';
  if sValue <> EmptyStr then
    Result := Result + ' value="' + sValue + '"';
  Result := Result + '>' + sText;
end;

{ Write select field option on form }
procedure THTMLWriter.SelectOption(sText, sValue: String; bSelected: Boolean);
begin
  if not bInSelect and (erErrors in Errors) then
    raise EHTMLError.Create(tcForm, 'Option field can only appear in a select field');

  bSelectOption := True;
  Write(fOutput, FormatSelectOption(sText, sValue, bSelected));
end;

{ Return start of text area field on form as string }
function THTMLWriter.FormatTextAreaStart(sName: String;
    iRows, iCols: Byte): String;
begin
  if (sName = EmptyStr) and (erErrors in Errors) then
    raise EHTMLError.Create(tcForm, 'Missing name for text area field');

  Result := '<textarea name="' + sName + '"';
  if iRows <> 0 then
    Result := Result + ' rows=' + IntToStr(iRows);
  if iCols <> 0 then
    Result := Result + ' cols=' + IntToStr(iCols);
  Result := Result + '>';
end;

{ Write start of text area field on form }
procedure THTMLWriter.TextAreaStart(sName: String; iRows, iCols: Byte);
begin
  if erErrors in Errors then
  begin
    if not bInForm then
      raise EHTMLError.Create(tcForm, 'Text area field can only appear in a form');
    if bInSelect or bInTextArea then
      raise EHTMLError.Create(tcForm,
        'Text area field cannot be embedded in select or text area fields');
  end;

  bInTextArea := True;
  sTags.Add('textarea');
  Write(fOutput, FormatTextAreaStart(sName, iRows, iCols));
end;

{ Return end of text area field on form as string }
function THTMLWriter.FormatTextAreaEnd: String;
begin
  Result := '</textarea>';
end;

{ Write end of text area field on form }
procedure THTMLWriter.TextAreaEnd;
begin
  CheckClosing(tcForm, not bInTextArea, 'textarea', 'text area field');

  bInTextArea := False;
  sTags.Delete(sTags.Count - 1);
  Write(fOutput, FormatTextAreaEnd);
end;

{ Return text area field on form as string }
function THTMLWriter.FormatTextArea(sName, sDefault: String;
    iRows, iCols: Byte): String;
begin
  Result := FormatTextAreaStart(sName, iRows, iCols) + sDefault +
    FormatTextAreaEnd;
end;

{ Write text area field on form }
procedure THTMLWriter.TextArea(sName, sDefault: String; iRows, iCols: Byte);
begin
  TextAreaStart(sName, iRows, iCols);
  Write(fOutput, sDefault);
  TextAreaEnd;
end;

{ *****************************************************************************
  HTML bulk insertion functions and procedures }

{ Write text straight to output }
procedure THTMLWriter.Text(sText: String);
begin
  Write(fOutput, sText)
end;

{ Return text after converting reserved characters }
function THTMLWriter.FormatEscapeText(sText: String): String;
var
  i: Integer;
begin
  Result := EmptyStr;
  for i := 1 to Length(sText) do
    case sText[i] of
      '<': Result := Result + FormatSpecialChar(scLessThan);
      '>': Result := Result + FormatSpecialChar(scGreaterThan);
      '&': Result := Result + FormatSpecialChar(scAmpersand);
      '"': Result := Result + FormatSpecialChar(scQuote);
    else
      Result := Result + sText[i];
    end;
end;

{ Write text straight to output, converting reserved characters }
procedure THTMLWriter.EscapeText(sText: String);
begin
  Write(fOutput, FormatEscapeText(sText));
end;

{ Write list of text straight to output }
procedure THTMLWriter.TextList(slText: TStringList);
var
  i: Integer;
begin
  for i := 0 to slText.Count - 1 do
    Text(slText[i] + sNewline);
end;

{ Write list of text straight to output, converting reserved characters }
procedure THTMLWriter.EscapeTextList(slText: TStringList);
var
  i: Integer;
begin
  for i := 0 to slText.Count - 1 do
    EscapeText(slText[i] + sNewline);
end;

{ Write the contents of a file straight to output }
procedure THTMLWriter.InsertFile(sFileName: String);
var
  fInput: TextFile;
  sText: String;
begin
  if not FileExists(sFileName) then
  begin
    if erErrors in Errors then
      raise EHTMLError.Create(tcFile, 'File ' + sFileName + ' doesn''t exist');
  end
  else
  begin
    AssignFile(fInput, sFileName);
    Reset(fInput);
    try
      while not Eof(fInput) do
      begin
        Readln(fInput, sText);
        Writeln(fOutput, sText);
      end;
    finally
      CloseFile(fInput);
      Flush(fOutput);
      bBulkInsert := True;
    end;
  end;
end;

{ Write the contents of a file to output, replacing values as directed in dictionary -
  Substitution points are specified between the braces,
  with the contents being '^' followed by a filename, or just a variable name.
  If specified, the file is merged recursively, otherwise the variable's
  value is inserted and processed recursively.
  An opening brace followed immediately by another opening brace
  or a closing brace is replaced by the second character. }
procedure THTMLWriter.MergeFile(sFileName: String; dicDictionary: THTMLDictionary);
var
  fInput: TextFile;
  sText: String;

  procedure CheckForMerge(var sText: String);
  var
    iPos: Byte;
    sReplace: String;
  begin
    iPos := Pos('{', sText);
    while iPos > 0 do
    begin
      Write(fOutput, Copy(sText, 1, iPos - 1));
      sText := Copy(sText, iPos, Length(sText) - iPos + 1);

      if sText[2] in ['{', '}'] then	{ Want this character }
      begin
        Write(fOutput, sText[2]);
        sText := Copy(sText, 3, Length(sText) - 2);
      end
      else                      	{ Substitution required }
      begin
        iPos := Pos('}', sText);
        sReplace := Copy(sText, 2, iPos - 2);
        if sReplace[1] = '^' then	{ Include file }
        begin
          MergeFile(Copy(sReplace, 2, Length(sReplace) - 1), dicDictionary);
          sText := Copy(sText, iPos + 1, Length(sText) - iPos);
        end
        else                        	{ Variable }
          sText := dicDictionary.GetValue(sReplace) +
            Copy(sText, iPos + 1, Length(sText) - iPos);
      end;

      iPos := Pos('{', sText);
    end;
    Writeln(fOutput, sText);
  end;

begin
  if not FileExists(sFileName) then
  begin
    if erErrors in Errors then
      raise EHTMLError.Create(tcFile, 'File ' + sFileName + ' doesn''t exist');
  end
  else
  begin
    AssignFile(fInput, sFileName);
    Reset(fInput);
    try
      while not Eof(fInput) do
      begin
        Readln(fInput, sText);
        CheckForMerge(sText);
      end;
    finally
      CloseFile(fInput);
      Flush(fOutput);
      bBulkInsert := True;
    end;
  end;
end;

{ *****************************************************************************
  HTML miscellaneous functions and procedures }

{ Finish off HTML document }
procedure THTMLWriter.Finalise;
begin
  if erErrors in Errors then
  begin
    if not bWrittenTitle and not bBulkInsert then
      raise EHTMLError.Create(tcTitle, 'No title for this document');
    if bInForm then
      raise EHTMLError.Create(tcForm, 'Form not closed at end of document');
    if (iTableLevels > 0) then
      raise EHTMLError.Create(tcTable, 'Table(s) not closed at end of document');
    if (sTags.Count > 0) then
      raise EHTMLError.Create(tcBody, 'Unclosed tags found at end of document');
  end;

  bInBody := False;
  if not bBulkInsert then
    Writeln(fOutput, '</body>' + sNewline + '</html>');
  CloseFile(fOutput);
end;

{ Convert value to indicate its use as a percentage }
function Percent(iValue: Integer): Integer;
begin
  Result := - Abs(iValue);
end;

{ Return the text and value as a string - allowing for percentages }
function THTMLWriter.CheckIfPercentage(sText: String; iValue: Integer): String;
begin
  Result := ' ' + sText + '=' + IntToStr(Abs(iValue));
  if iValue < 0 then	{ Percentage }
    Result := Result + '%';
end;

{ Convert from Delphi colour value to HTML colour value }
function THTMLWriter.ConvertColour(clrColour: TColor): String;
var
  iR, iG, iB: Byte;

  function AsHex(iValue: Byte): String;
  const
    cHex: array [0..15] of char = '0123456789ABCDEF';
  begin
    Result := cHex[iValue div 16] + cHex[iValue mod 16];
  end;

begin
  iR := GetRValue(clrColour);
  iG := GetGValue(clrColour);
  iB := GetBValue(clrColour);
  Result := '#' + AsHex(iR) + AsHex(iG) + AsHex(iB);
end;

end.
 
