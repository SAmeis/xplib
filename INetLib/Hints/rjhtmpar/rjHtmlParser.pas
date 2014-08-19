unit rjHtmlParser;
{
File:           rjHtmlParser.pas
Version:        0.1 / Initial Public Release
Last Modified:  1. April 2000
Decription:     Contains three Classes for advanced, fast and very flexible
                parsing and/or writing HTML-files.

                There is no help at the moment except the short description
                below. Consult the sources for details and comments.

                TAbstractHtmlParser:
                  Abstract base class for THtmlReporter, THtmlWriter, and
                  THtmlAutoWriter. Do not create instances of
                  TAbstractHtmlParser. The properties and methods of
                  TAbstractHtmlParser provide basic behavior
                  descendent classes can inherit as well as override
                  to customize their behavior.

                  The TAbstractHtmlParser also implements a highly flexible
                  interface for reading the HTML data. String and PChar support
                  is already implemented. Additionally, the OnReadChar event
                  allows to write customized functions to read from streams of
                  files. The last character of the HTML data must always be #0.

                THtmlReporter:
                  The THtmlReporter class allows the parsing of HTML-files for
                  various elements. Calling the Next function will cause the
                  THtmlReporter to parse next element which can then be retrieved.
                  Reporting of elements can be switched on and off as desired.
                  Special filtering for HTML-Tags is also available. Tag support
                  more or less follows the HTML 4.0 specifications.

                  At the moment THtmlReporter handles the following elements:
                    * Tags (with extra filtering options).
                    * Text
                    * Comments
                    * Scripts
                    * Styles
                    * DTD (Document Type Definitions)
                    * SSI (Server Side Includes)
                    * ASP

                  With the exception of Tags, all elements can be read
                  either a strings or as buffers. Tags are available via the
                  THtmlTag object. HTML character entities are
                  automatically converted.

                THtmlWriter:
                  The THtmlWriter is a descendend of the THtmlReporter.
                  It has additional procedures for writing HTML data
                  to a destination buffer.

                THtmlAutoWriter:
                  The THtmlAutoWriter is a descendend of the THtmlWriter.
                  It can automatically write parsed elements to an internal
                  destination buffer. HTML AutoWriting does not affect the
                  HTML reporting. You can have some elements reported and not
                  automatically written, others auto-written but not reported.
                  The AutoWriting also allows to set filters on paricular
                  HTML tags. These filters are again independend of the filters
                  set for the tag-reporting. Use the OnWriteDest event
                  to write output to the destination of your choice
                  (i.e. streams, files).

Author:         Ralf Junker
E-Mail:         ralfjunker@gmx.de
Legal:          This unit is provided "as is" and without warranty of any kind.
                At the moment, it is freeware for private use only.
                I might, however, change this in the future. If you intend
                to use it commercially, please contact me via e-mail.
Bug Report:     If you encounter any bugs or want to suggest improvements,
                just get in touch using my e-mail above. When writing, please
                include the name and version of the unit you are referring to.
Thanks to:      * The mwEdit project <http://www.eccentrica.org/gabr/mw/mwedit.htm>
                  for the technique of hash-arrays of functions to
                  convert tag's and attribute's names to identifiers
                  at high speed.

Copyright (c) 2000 Ralf Junker
}

interface

{$I rj.inc}

{$DEFINE Component}
{ Undefine this directive if you do not need design time components
  but want to create the objects at runtime instead. }

uses{$IFDEF Component}Classes, {$ENDIF}
 rjBase, rjString, rjSearch, rjRegExpr, // common rj-Units
 rjHtmlTag, rjHtmlTags, rjHtmlAttribs, rjHtmlEntities; // rjHtmlParser related

const
 { Defaults: THtmlReporter }
 REPORT_ASPS_DEFAULT          = False;
 REPORT_COMMENTS_DEFUALT      = True;
 REPORT_DTDS_DEFUALT          = True;
 REPORT_SCRIPTS_DEFUALT       = True;
 REPORT_SSIS_DEFUALT          = True;
 REPORT_STYLES_DEFUALT        = True;
 REPORT_TAGS_DEFUALT          = True;
 REPORT_TEXT_DEFUALT          = True;

  { Defaults: THtmlAutoWriter }
 AUTO_WRITE_ASPS_DEFAULT      = False;
 AUTO_WRITE_COMMENTS_DEFUALT  = True;
 AUTO_WRITE_DTDS_DEFUALT      = True;
 AUTO_WRITE_SCRIPTS_DEFUALT   = True;
 AUTO_WRITE_SSIS_DEFUALT      = True;
 AUTO_WRITE_STYLES_DEFUALT    = True;
 AUTO_WRITE_TAGS_DEFUALT      = True;
 AUTO_WRITE_TEXT_DEFUALT      = True;


 { Settings: THtmlReporter: Byte for Saving and Restoring }
 SETTING_REPORT_ASPS          = 1;
 SETTING_REPORT_COMMENTS      = 1 shl 1;
 SETTING_REPORT_DTDS          = 1 shl 2;
 SETTING_REPORT_SCRIPTS       = 1 shl 3;
 SETTING_REPORT_SSIS          = 1 shl 4;
 SETTING_REPORT_STYLES        = 1 shl 5;
 SETTING_REPORT_TAGS          = 1 shl 6;
 SETTING_REPORT_TAGS_FILTERED = 1 shl 7;
 SETTING_REPORT_TEXT          = 1 shl 8;


 { Settings: THtmlReporter: Byte for Saving and Restoring }
 SETTING_AUTO_WRITE_ASPS      = 1 shl 17;
 SETTING_AUTO_WRITE_COMMENTS  = 1 shl 18;
 SETTING_AUTO_WRITE_DTDS      = 1 shl 19;
 SETTING_AUTO_WRITE_SCRIPTS   = 1 shl 20;
 SETTING_AUTO_WRITE_SSIS      = 1 shl 21;
 SETTING_AUTO_WRITE_STYLES    = 1 shl 22;
 SETTING_AUTO_WRITE_TAGS      = 1 shl 23;
 SETTING_AUTO_WRITE_TAGS_FILTERED = 1 shl 24;
 SETTING_AUTO_WRITE_TEXT      = 1 shl 25;

 SpecialTags                  = [tagSCRIPT, tagSTYLE];
type
 TElements = (eUndefined, eASP, eComment, eDTD, eScript, eSSI, eStyle, eTag, eText);
 TElementSet = set of TElements;

 PTagBooleanArray = ^TTagBooleanArray;
 TTagBooleanArray = array[-tagMax..tagMax] of Boolean;
 
 PTagProcArray = ^TTagProcArray;
 TTagProcArray = array[TTags] of TProcEvent;
 
 TOnReadChar = function: Char of object;
 TOnWriteBuffer = procedure (const Buffer; Count: Integer) of object;
 
 { TAbstractHtmlParser }
 
 TAbstractHtmlParser = class{$IFDEF Component} (TComponent){$ENDIF}
 private
  FChar: Char;
  FOnReadChar: TOnReadChar;

  FCDataBuffer: PChar;
  FCDataPosition: Integer;
  FCDataCapacity: Integer;

  FSourcePointer: PChar;
  FSourcePosition: Integer;
  FSourceString: AnsiString;

  { Handling }
  FHandleASP: TProcEvent;
  FHandleComment: TProcEvent;
  FHandleDTD: TProcEvent;
  FHandleSSI: TProcEvent;
  FHandleTag: TProcEvent;
  FHandleText: TProcEvent;
  FHandleTextNormal: TProcEvent;
  FHandleTextPreformatted: TProcEvent;

  FPreFormattedNesting: Integer;

  FTag: THtmlTag;
  FElementType: TElements;

  { CData }
  procedure AddToCDataBuffer (const c: Char);
  function GetCDataString: AnsiString;
  { Source }
  procedure SetSourceString (const Value: AnsiString);
  procedure SetSourceBuffer (const Value: PChar);
  function ReadCharFromSourceBuffer: Char;

  procedure UpdateHandleText;

  procedure NextPrim;
 protected
  { Parsing }
  procedure ParseASP;
  procedure ParseAttribs;
  procedure ParseDoubleDash;
  procedure ParseComment;
  procedure ParseDTD;
  procedure ParseScript;
  procedure ParseSSI;
  procedure ParseStyle;
  procedure ParseTag;
  procedure ParseTextNormal;
  procedure ParseTextPreformatted;
  function ParseEntity: Char; // Called from the ParseText_ Procedures
  { Skipping }
  procedure SkipASP;
  procedure SkipAttribs;
  procedure SkipDoubleDash;
  procedure SkipDTD;
  procedure SkipScript;
  procedure SkipStyle;
  procedure SkipText;
 public
  constructor Create{$IFDEF Component} (AOwner: TComponent); override{$ELSE}; virtual{$ENDIF};
  destructor Destroy; override;
  { ElementType & Tag }
  property ElementType: TElements read FElementType;
  property Tag: THtmlTag read FTag;
  { CData }
  property CDataBuffer: PChar read FCDataBuffer; // You migth want to call TerminateBuf before using this property.
  property CDataLength: Integer read FCDataPosition;
  property CDataString: AnsiString read GetCDataString;
  procedure TerminateCDataBuffer;
  { Source -- Internal }
  property SourcePosition: Integer read FSourcePosition write FSourcePosition;
  property SourceString: AnsiString read FSourceString write SetSourceString;
  procedure ResetSourceBuffer;
  procedure InitOnReadCharFunc;
 published
  { Events }
  property OnReadChar: TOnReadChar read FOnReadChar write FOnReadChar;
 end;
 
 { THtmlReporter }
 
 THtmlReporter = class (TAbstractHtmlParser)
 private
  { Reporting }
  FReportCurrent: Boolean; // Tells us if we need to report the current element.

  FReportASPs: Boolean;
  FReportComments: Boolean;
  FReportDTDs: Boolean;
  FReportScripts: Boolean;
  FReportSSIs: Boolean;
  FReportStyles: Boolean;
  FReportTags: Boolean;
  FReportTagsFiltered: Boolean;
  FReportText: Boolean;

  FReportTagsFilterArray: TTagBooleanArray;

  FHandleTags: PTagProcArray; // Pointer to the current Handle-Tags Array.

  FHandleTagsSkip: TTagProcArray;
  FHandleTagsReportFiltered: TTagProcArray;
  FHandleTagsReport: TTagProcArray;

  function SearchPrim (const Value: AnsiString; const CaseSensitive: Boolean): Pointer;
  function MatchPrim (const Pattern: AnsiString; const Registers: PRegExpRegisters): Boolean;

  procedure SetReportTagsFiltered (const Value: Boolean);
  function GetReportTagFilter (const Tag: TTags): Boolean;
  procedure SetReportTagFilter (const Tag: TTags; const Value: Boolean);

  { Setting Handlers }
  procedure InitHandleProcs; virtual;

  procedure SetAllTagHandlers (const Tag: TTags; const Proc: TProcEvent); virtual;

  procedure SetHandleASPs; virtual;
  procedure SetHandleComments; virtual;
  procedure SetHandleDTDs; virtual;
  procedure SetHandleScripts; virtual;
  procedure SetHandleSSIs; virtual;
  procedure SetHandleStyles; virtual;
  procedure SetHandleTags; virtual;
  procedure SetHandleText; virtual;

  { Setting Report Settings }
  procedure SaveSettings (var i: Integer); virtual;
  procedure RestoreSettings (const i: Integer); virtual;

  procedure SetReportASPs (const Value: Boolean);
  procedure SetReportComments (const Value: Boolean);
  procedure SetReportDTDs (const Value: Boolean);
  procedure SetReportScripts (const Value: Boolean);
  procedure SetReportSSIs (const Value: Boolean);
  procedure SetReportStyles (const Value: Boolean);
  procedure SetReportTags (const Value: Boolean);
  procedure SetReportText (const Value: Boolean);

  procedure UpdateAllHandlers;
  procedure UpdateAllTagProcs;
  procedure UpdateSingleEndTagProc (const Tag: TEndTags); virtual;
  procedure UpdateSingleStartTagProc (const Tag: TStartTags); virtual;
  procedure UpdateSingleTagProc (const Tag: TTags);

 protected
  { Parsing }
  procedure ParseTagAndAttribs;
  { Reporting Events }
  procedure ParseReportASP;
  procedure SkipReportAttribs;
  procedure ParseReportAttribs;
  procedure ParseReportComment;
  procedure ParseReportDTD;
  procedure ParseReportScript;
  procedure ParseReportSSI;
  procedure ParseReportStyle;
  procedure ParseReportTextNormal;
  procedure ParseReportTextPreformatted;

 public
  constructor Create{$IFDEF Component} (AOwner: TComponent); override{$ELSE}; virtual{$ENDIF};
  constructor CreateTagParser{$IFDEF Component} (AOwner: TComponent); {$ENDIF}overload;
  constructor CreateTextParser{$IFDEF Component} (AOwner: TComponent); {$ENDIF}overload;

  function Next: Boolean; virtual;

  function SearchComment (const Value: AnsiString; const CaseSensitive: Boolean = True): Pointer;
  function MatchComment (const Pattern: AnsiString; const Registers: PRegExpRegisters): Boolean;
  function SearchText (const Value: AnsiString; const CaseSensitive: Boolean = True): Pointer;
  function MatchText (const Pattern: AnsiString; const Registers: PRegExpRegisters): Boolean;

  procedure ReportTagsFilter (const Tags: array of TTags; const Value: Boolean);

  property ReportTagFilter[const Tag: TTags]: Boolean read GetReportTagFilter write SetReportTagFilter;
  { Set Parser Modes }
  procedure SetDefaultReporter;
  procedure SetFullReporter;
  procedure SetCommentReporter;
  procedure SetTagReporter;
  procedure SetTextReporter;
 published
  property ReportASPs: Boolean read FReportASPs write SetReportASPs;
  property ReportComments: Boolean read FReportComments write SetReportComments;
  property ReportDTDs: Boolean read FReportDTDs write SetReportDTDs;
  property ReportScripts: Boolean read FReportScripts write SetReportScripts;
  property ReportSSIs: Boolean read FReportSSIs write SetReportSSIs;
  property ReportStyles: Boolean read FReportStyles write SetReportStyles;
  property ReportTags: Boolean read FReportTags write SetReportTags;
  property ReportText: Boolean read FReportText write SetReportText;
  property ReportTagsFiltered: Boolean read FReportTagsFiltered write SetReportTagsFiltered;
 end;
 
 { THtmlWriter }
 
 THtmlWriter = class (THtmlReporter)
 private
  { Destination }
  FDestBuffer: PChar;
  FDestCapacity: Integer;
  FDestPosition: Integer;
  FOnWriteDest: TOnWriteBuffer;

  { Writing Procedures }
  procedure WriteToDestBuffer (const Buffer; Count: Integer);
  function GetDestString: AnsiString;
  procedure WriteCurrentASP;
  procedure WriteCurrentComment;
  procedure WriteCurrentDTD;
  procedure WriteCurrentSSI;
  procedure WriteCurrentTag;
  procedure WriteCurrentStartTag;
  procedure WriteCurrentEndTag;
  procedure WriteCurrentTagAndCData;
  procedure WriteCurrentCData;
 public
  constructor Create{$IFDEF Component} (AOwner: TComponent){$ENDIF}; override;
  destructor Destroy; override;

  { Internal Destination Buffer }
  property DestBuffer: PChar read FDestBuffer;
  property DestPosition: Integer read FDestPosition;
  property DestString: AnsiString read GetDestString;

  { Write Internal Destination Buffer }
  procedure WriteCurrentElement;
  procedure WriteASP (const s: AnsiString);
  procedure WriteComment (const s: AnsiString);
  procedure WriteDTD (const s: AnsiString);
  procedure WriteSSI (const s: AnsiString);
  procedure WriteString (const s: AnsiString);
  procedure WriteTag (const Tag: THtmlTag);
 end;
 
 { THtmlAutoWriter }
 
 THtmlAutoWriter = class (THtmlWriter)
 private
  FAutoWriteTagsFilterArray: array[TTags] of Boolean;

  FHandleTagsWriteFiltered: TTagProcArray;
  FHandleTagsReportFilteredWriteFiltered: TTagProcArray;
  FHandleTagsReportWriteFiltered: TTagProcArray;

  FHandleTagsWrite: TTagProcArray;
  FHandleTagsReportFilteredWrite: TTagProcArray;
  FHandleTagsReportWrite: TTagProcArray;

  { AutoWrite Properties }
  FAutoWriteASPs: Boolean;
  FAutoWriteComments: Boolean;
  FAutoWriteDTDs: Boolean;
  FAutoWriteScripts: Boolean;
  FAutoWriteStyles: Boolean;
  FAutoWriteTags: Boolean;
  FAutoWriteTagsFiltered: Boolean;
  FAutoWriteText: Boolean;
  FAutoWriteSSIs: Boolean;
  FOnWriteDest: TOnWriteBuffer;
  { Setting Handlers }
  procedure InitHandleProcs; override;
  //  procedure UpdateSingleEndTagProc (const Tag: Integer); override;
  //  procedure UpdateSingleStartTagProc (const Tag: Integer); override;

  procedure SetAllTagHandlers (const Tag: TTags; const Proc: TProcEvent); override;
  procedure SetHandleASPs; override;
  procedure SetHandleComments; override;
  procedure SetHandleDTDs; override;
  procedure SetHandleScripts; override;
  procedure SetHandleSSIs; override;
  procedure SetHandleStyles; override;
  procedure SetHandleTags; override;
  procedure SetHandleText; override;

  { Setting AutoWrite Properties }
  procedure RestoreSettings (const i: Integer); override;
  procedure SaveSettings (var i: Integer); override;

  procedure SetAutoWriteASPs (const Value: Boolean);
  procedure SetAutoWriteComments (const Value: Boolean);
  procedure SetAutoWriteDTDs (const Value: Boolean);
  procedure SetAutoWriteScripts (const Value: Boolean);
  procedure SetAutoWriteSSIs (const Value: Boolean);
  procedure SetAutoWriteStyles (const Value: Boolean);
  procedure SetAutoWriteTags (const Value: Boolean);
  procedure SetAutoWriteText (const Value: Boolean);

  function GetAutoWriteTagFilter (const Tag: TTags): Boolean;
  procedure SetAutoWriteTagFilter (const Tag: TTags; const Value: Boolean);
  procedure SetAutoWriteTagsFiltered (const Value: Boolean);

  procedure UpdateSingleEndTagProc (const Tag: TEndTags); override;
  procedure UpdateSingleStartTagProc (const Tag: TStartTags); override;
 protected
  { Parsing & Writing only }
  procedure ParseWriteAttribs;
  procedure SkipWriteAttribs; // Skip
  procedure ParseWriteASP;
  procedure ParseWriteComment;
  procedure ParseWriteDTD;
  procedure ParseWriteScript;
  procedure ParseWriteSSI;
  procedure ParseWriteStyle;
  procedure ParseWriteTextNormal;
  procedure ParseWriteTextPreformatted;
  { Parsing & Reporting & Writing }
  procedure ParseReportWriteASP;
  procedure ParseReportWriteAttribs;
  procedure SkipReportWriteAttribs; // Skip
  procedure ParseReportWriteComment;
  procedure ParseReportWriteDTD;
  procedure ParseReportWriteScript;
  procedure ParseReportWriteSSI;
  procedure ParseReportWriteStyle;
  procedure ParseReportWriteTextNormal;
  procedure ParseReportWriteTextPreformatted;
 public
  constructor Create{$IFDEF Component} (AOwner: TComponent){$ENDIF}; override;
  { AutoWrite Tag Filters }
  property AutoWriteTagFilter[const Tag: TTags]: Boolean read GetAutoWriteTagFilter write SetAutoWriteTagFilter;
  procedure AutoWriteTagsFilter (const Tags: array of TTags; const Value: Boolean);
  { Set some standard writer modes }
  procedure SetDefaultAutoWriter;
  procedure FullTextAutoWriter;
  procedure SetTagAutoWriter;
  procedure SetTextAutoWriter;
 published
  property AutoWriteASPs: Boolean read FAutoWriteASPs write SetAutoWriteASPs;
  property AutoWriteComments: Boolean read FAutoWriteComments write SetAutoWriteComments;
  property AutoWriteDTDs: Boolean read FAutoWriteDTDs write SetAutoWriteDTDs;
  property AutoWriteScripts: Boolean read FAutoWriteScripts write SetAutoWriteScripts;
  property AutoWriteSSIs: Boolean read FAutoWriteSSIs write SetAutoWriteSSIs;
  property AutoWriteStyles: Boolean read FAutoWriteStyles write SetAutoWriteStyles;
  property AutoWriteTags: Boolean read FAutoWriteTags write SetAutoWriteTags;
  property AutoWriteTagsFiltered: Boolean read FAutoWriteTagsFiltered write SetAutoWriteTagsFiltered;
  property AutoWriteText: Boolean read FAutoWriteText write SetAutoWriteText;
  property OnWriteDest: TOnWriteBuffer read FOnWriteDest write FOnWriteDest;
 end;
 
function CompareTA (const p1, p2: PChar; const l: Integer): Boolean; // For Tags and Attribs
function CompareE (const p1, p2: PChar; const l: Integer): Boolean; // For Entities

{$IFDEF Component}
procedure Register;
{$ENDIF}

const
 TagChars                     : TCharSet = ['/', '1'..'6', 'A'..'Z', 'a'..'z'];
 
 AttribChars                  : TCharSet = ['-', 'A'..'Z', 'a'..'z'];
 AttribStartChars             : TCharSet = ['A'..'Z', 'a'..'z'];
 
 EntityChars                  : TCharSet = ['1'..'4', 'A'..'Z', 'a'..'z'];
 
 { Content Models }
 
 cmHead_Misc                  = [tagSCRIPT, tagSTYLE, tagMeta, tagLINK, tagObject];
 
 cmHeading                    = [tagH1, tagH2, tagH3, tagH4, tagH5, tagH6];
 cmList                       = [tagUL, tagOL, tagDir, tagMenu];
 cmPreformatted               = [tagPre];
 cmFontStyle                  = [tagTT, tagI, tagB, tagU, Tags, tagStrike, tagBig, tagSmall];
 cmPhrase                     = [tagEM, tagSTRONG, tagDFN, tagCODE, tagSAMP, tagKBD, tagVar, tagCite, tagABBR, tagAcronym];
 cmSpecial                    = [tagA, tagIMG, tagApplet, tagObject, tagFONT, tagBaseFont, tagBr, tagSCRIPT, tagMAP, tagQ, tagSUB, tagSUP, tagSPAN, tagBDO, tagIFRAME];
 cmFormCtrl                   = [tagINPUT, tagSelect, tagTEXTAREA, tagLabel, tagButton];
 
 cmPRe_Exclusions             = [tagIMG, tagObject, tagApplet, tagBig, tagSmall, tagSUB, tagSUP, tagFONT, tagBaseFont];
 
 cmBlock                      = [tagP] + cmHeading + cmList + cmPreformatted;
 cmInline                     = cmFontStyle + cmPhrase + cmSpecial + cmFormCtrl;
 cmFlow                       = cmBlock + cmInline;
 
 cmHtml_Content               = [taghead, tagBody];
 
type
 TTagInfo = record
  Name: AnsiString;
  SubTags: TTagSet;
 end;
 
const // Allowed SubSubTags according to HTML 4.1 Transitional DTD
 TagInfo                      : array[tagUndefined..tagVar] of TTagInfo = (
  (Name: ''; SubTags: []),
  (Name: 'A'; SubTags: cmInline - [tagA]),
  (Name: 'ABBR'; SubTags: cmInline),
  (Name: 'ACRONYM'; SubTags: cmInline),
  (Name: 'ADDRESS'; SubTags: cmInline + [tagP]),
  (Name: 'APPLET'; SubTags: cmFlow + [tagParam]),
  (Name: 'AREA'; SubTags: []),
  (Name: 'ARTICLE'; SubTags: []), // Special, not covered by HTML
  (Name: 'B'; SubTags: cmInline),
  (Name: 'BASE'; SubTags: []),
  (Name: 'BASEFONT'; SubTags: []),
  (Name: 'BDO'; SubTags: cmInline),
  (Name: 'BIG'; SubTags: cmInline),
  (Name: 'BLOCKQUOTE'; SubTags: cmFlow),
  (Name: 'BODY'; SubTags: cmFlow + [tagIns, tagDel]),
  (Name: 'BR'; SubTags: []), // Empty.
  (Name: 'BUTTON'; SubTags: cmFlow - cmFormCtrl - [tagA, tagFORM, tagISINDEX, tagFIELDSET, tagIFRAME]),
  (Name: 'CAPTION'; SubTags: cmInline),
  (Name: 'CENTER'; SubTags: cmFlow),
  (Name: 'CITE'; SubTags: cmInline),
  (Name: 'CODE'; SubTags: cmInline),
  (Name: 'COL'; SubTags: []), // Empty.
  (Name: 'COLGROUP'; SubTags: [tagCol]),
  (Name: 'DD'; SubTags: cmFlow),
  (Name: 'DEL'; SubTags: []),
  (Name: 'DFN'; SubTags: cmInline),
  (Name: 'DIR'; SubTags: [tagLI]),
  (Name: 'DIV'; SubTags: cmFlow),
  (Name: 'DL'; SubTags: [tagDT, tagDD]),
  (Name: 'DT'; SubTags: cmInline),
  (Name: 'EM'; SubTags: cmInline),
  (Name: 'FIELDSET'; SubTags: cmFlow + [tagLegend]),
  (Name: 'FONT'; SubTags: cmInline),
  (Name: 'FORM'; SubTags: cmFlow),
  (Name: 'FRAME'; SubTags: []),
  (Name: 'FRAMESET'; SubTags: [tagFrameSet, tagFrame, tagNoFrames]),
  (Name: 'H1'; SubTags: cmInline),
  (Name: 'H2'; SubTags: cmInline),
  (Name: 'H3'; SubTags: cmInline),
  (Name: 'H4'; SubTags: cmInline),
  (Name: 'H5'; SubTags: cmInline),
  (Name: 'H6'; SubTags: cmInline),
  (Name: 'HEAD'; SubTags: cmHead_Misc),
  (Name: 'HR'; SubTags: []),
  (Name: 'HTML'; SubTags: cmHtml_Content),
  (Name: 'I'; SubTags: cmInline),
  (Name: 'IFRAME'; SubTags: cmFlow),
  (Name: 'IMG'; SubTags: []),
  (Name: 'INPUT'; SubTags: []), // Empty.
  (Name: 'INS'; SubTags: []), // Empty.
  (Name: 'ISINDEX'; SubTags: []), // Empty.
  (Name: 'KBD'; SubTags: cmInline),
  (Name: 'LABEL'; SubTags: cmInline),
  (Name: 'LEGEND'; SubTags: cmInline),
  (Name: 'LI'; SubTags: cmFlow),
  (Name: 'LINK'; SubTags: []), // Empty.
  (Name: 'MAP'; SubTags: cmBlock + [tagArea]),
  (Name: 'MENU'; SubTags: [tagLI]),
  (Name: 'META'; SubTags: []), // Empty.
  (Name: 'NOFRAMES'; SubTags: cmFlow + [tagBody]),
  (Name: 'NOSCRIPT'; SubTags: cmFlow),
  (Name: 'OBJECT'; SubTags: cmFlow + [tagParam]),
  (Name: 'OL'; SubTags: [tagLI]),
  (Name: 'OPTGROUP'; SubTags: [tagOption]),
  (Name: 'OPTION'; SubTags: []), // Empty.
  (Name: 'P'; SubTags: cmInline),
  (Name: 'PARAM'; SubTags: []), // Empty.
  (Name: 'PRE'; SubTags: cmInline - cmPRe_Exclusions),
  (Name: 'Q'; SubTags: cmInline),
  (Name: 'S'; SubTags: cmInline),
  (Name: 'SAMP'; SubTags: cmInline),
  (Name: 'SCRIPT'; SubTags: []), // Empty.
  (Name: 'SELECT'; SubTags: [tagOptgroup, tagOption]),
  (Name: 'SMALL'; SubTags: cmInline),
  (Name: 'SPAN'; SubTags: cmInline),
  (Name: 'STRIKE'; SubTags: cmInline),
  (Name: 'STRONG'; SubTags: cmInline),
  (Name: 'STYLE'; SubTags: []), // Empty.
  (Name: 'SUB'; SubTags: cmInline),
  (Name: 'SUP'; SubTags: cmInline),
  (Name: 'TABLE'; SubTags: [tagCaption, tagTr, tagCol, tagColGroup, tagTHead, tagTFoot, tagTBODY]), // TR no longer in HTML 4.0
  (Name: 'TBODY'; SubTags: [tagTr]),
  (Name: 'TD'; SubTags: cmFlow),
  (Name: 'TEXTAREA'; SubTags: []), // PCData only.
  (Name: 'TFOOT'; SubTags: [tagTr]),
  (Name: 'TH'; SubTags: cmFlow),
  (Name: 'THEAD'; SubTags: [tagTr]),
  (Name: 'TITLE'; SubTags: []), // PCData only.
  (Name: 'TR'; SubTags: [tagTh, tagTD]),
  (Name: 'TT'; SubTags: cmInline),
  (Name: 'U'; SubTags: cmInline),
  (Name: 'UL'; SubTags: [tagLI]),
  (Name: 'VAR'; SubTags: cmInline)
  );
 
implementation

{ **************************************************************************** }
{ TAbstractHtmlParser }
{ **************************************************************************** }

constructor TAbstractHtmlParser.Create{$IFDEF Component} (AOwner: TComponent){$ENDIF};
begin
{$IFDEF Component}inherited Create (AOwner); {$ENDIF}
 FTag := THtmlTag.Create;
end;

destructor TAbstractHtmlParser.Destroy;
begin
 FTag.Free;
 if Assigned (FCDataBuffer) then FreeMem (FCDataBuffer, FCDataCapacity);
 inherited Destroy;
end;

{ **************************************************************************** }

procedure TAbstractHtmlParser.UpdateHandleText;
begin
 if FPreFormattedNesting > 0 then
  FHandleText := FHandleTextPreformatted
 else
  FHandleText := FHandleTextNormal;
end;

{ **************************************************************************** }

procedure TAbstractHtmlParser.NextPrim;
begin
 Assert (Assigned (FOnReadChar), 'Need to assign FOnReadChar before calling Next.');
 
 if FChar = '<' then
 begin // Tag-opening character found. // Test for tag-opening character.
  FChar := FOnReadChar;
  if FChar = '!' then
  begin
   FChar := FOnReadChar;
   if FChar = '-' then
   begin
    FChar := FOnReadChar;
    if FChar = '-' then
    begin // Found a comment. // Is it a comment or a DTD?
     FChar := FOnReadChar;
     if FChar = '#' then
     begin // Is it a comment or a SSI?
      FChar := FOnReadChar;
      FHandleSSI;
     end
     else
      FHandleComment;
    end;
   end
   else
   begin // Found DTD.
    FHandleDTD
   end;
  end
  else
   if FChar = '%' then
   begin // Found ASP
    FChar := FOnReadChar;
    FHandleASP;
   end
   else
   begin // Found regular tag.
    FHandleTag;
   end;
 end
 else
 begin // Found CData (text).
  FHandleText;
 end;
end;

{ TAbstractHtmlParser: ASP ********** }

procedure TAbstractHtmlParser.ParseASP;
begin
 FElementType := eASP;
 FCDataPosition := 0;
 repeat
  while not (FChar in ['%', #0]) do
  begin
   AddToCDataBuffer (FChar);
   FChar := FOnReadChar;
  end;

  if FChar = '%' then
  begin
   FChar := FOnReadChar;
   if FChar = '>' then
   begin
    FChar := FOnReadChar;
    Break;
   end;
  end;
 until FChar = #0;
end;

{ ********** }

procedure TAbstractHtmlParser.SkipASP;
begin
 FCDataPosition := 0;
 repeat
  while not (FChar in ['%', #0]) do // Eat all chars until the end of the comment or the end of the file.
   FChar := FOnReadChar;

  if FChar = '%' then
  begin
   FChar := FOnReadChar;
   if FChar = '>' then
   begin
    FChar := FOnReadChar;
    Break;
   end;
  end;
 until FChar = #0;
end;

{ TAbstractHtmlParser: Comment ********** }

procedure TAbstractHtmlParser.ParseComment;
begin
 FElementType := eComment;
 ParseDoubleDash;
end;

{ ********** }

procedure TAbstractHtmlParser.ParseDoubleDash;
var
 DoubleDashEnd                : Integer;
begin
 FCDataPosition := 0;
 repeat
  while not (FChar in ['-', #0]) do
  begin // Eat all chars until the end of the comment or the end of the file.
   AddToCDataBuffer (FChar);
   FChar := FOnReadChar;
  end;

  if (FChar = '-') then
  begin
   DoubleDashEnd := FCDataPosition; // Remember end of comment.
   AddToCDataBuffer (FChar); // Keep storing chars in case this is not yet the end.
   FChar := FOnReadChar;
   if FChar = '-' then
   begin
    AddToCDataBuffer (FChar);
    FChar := FOnReadChar;
    while FChar = '-' do
    begin // Skip multipe '--' at the end of the comment marker.
     AddToCDataBuffer (FChar);
     FChar := FOnReadChar;
    end;
    while FChar in csWhiteSpace do
    begin // Skip possible whitespace between '--' and '>'.
     AddToCDataBuffer (FChar);
     FChar := FOnReadChar;
    end;
   end;
  end;
 until FChar in [#0, '>']; // End of searching for comment.
 if FChar = '>' then
 begin
  FChar := FOnReadChar;
  FCDataPosition := DoubleDashEnd; // Set end of comment.
 end;
end;

procedure TAbstractHtmlParser.SkipDoubleDash;
begin
 repeat
  while not (FChar in ['-', #0]) do
   FChar := FOnReadChar;

  if (FChar = '-') then
  begin
   FChar := FOnReadChar;
   if FChar = '-' then
   begin
    FChar := FOnReadChar;
    while FChar = '-' do // Mehrfache '--' am Kommentar-Ende überspringen
     FChar := FOnReadChar;

    while FChar in csWhiteSpace do // Leerzeichen am Kommentar-Ende vor '>' überspringen
     FChar := FOnReadChar;
   end;
  end;
 until FChar in [#0, '>']; // Kommentar-Ende Suchschleife
 if FChar = '>' then FChar := FOnReadChar;
end;

{ TAbstractHtmlParser: DTD ********** }

procedure TAbstractHtmlParser.ParseDTD;
begin
 FElementType := eDTD;
 FCDataPosition := 0;
 while not (FChar in ['>', #0]) do
 begin
  AddToCDataBuffer (FChar);
  FChar := FOnReadChar;
 end;
 if FChar = '>' then FChar := FOnReadChar;
end;

{ ********** }

procedure TAbstractHtmlParser.SkipDTD;
begin
 while not (FChar in ['>', #0]) do
  FChar := FOnReadChar;
 if FChar = '>' then FChar := FOnReadChar;
end;

{ TAbstractHtmlParser: Server Side Includes ********** }

procedure TAbstractHtmlParser.ParseSSI;
begin
 FElementType := eSSI;
 ParseDoubleDash;
end;

{ TAbstractHtmlParser: Style ********** }

procedure TAbstractHtmlParser.ParseStyle;
begin
 FElementType := eStyle;
 ParseAttribs; // Deal with the Attribs first.
 FCDataPosition := 0;
 repeat
  while not (FChar in ['<', #0]) do
  begin
   AddToCDataBuffer (FChar);
   FChar := FOnReadChar;
  end;

  if FChar = '<' then
  begin
   AddToCDataBuffer (FChar);
   FChar := FOnReadChar;
   if FChar = '/' then
   begin
    AddToCDataBuffer (FChar);
    FChar := FOnReadChar;
    if FChar in ['S', 's'] then
    begin
     AddToCDataBuffer (FChar);
     FChar := FOnReadChar;
     if FChar in ['T', 't'] then
     begin
      AddToCDataBuffer (FChar);
      FChar := FOnReadChar;
      if FChar in ['Y', 'y'] then
      begin
       AddToCDataBuffer (FChar);
       FChar := FOnReadChar;
       if FChar in ['L', 'l'] then
       begin
        AddToCDataBuffer (FChar);
        FChar := FOnReadChar;
        if FChar in ['E', 'e'] then
        begin
         Dec (FCDataPosition, 6); // '</Style' vom Buffer abziehen.
         FChar := FOnReadChar;
         while not (FChar in ['>', #0]) do
          FChar := FOnReadChar;
         if FChar = '>' then
         begin
          FChar := FOnReadChar;
          Break;
         end;
        end;
       end;
      end;
     end;
    end;
   end;
  end;
 until FChar = #0;
end;

{ ********** }

procedure TAbstractHtmlParser.SkipStyle;
begin
 SkipAttribs; // Deal with the Attribs first.
 repeat
  while not (FChar in ['<', #0]) do
   FChar := FOnReadChar;

  if FChar = '<' then
  begin
   FChar := FOnReadChar;
   if FChar = '/' then
   begin
    FChar := FOnReadChar;
    if FChar in ['S', 's'] then
    begin
     FChar := FOnReadChar;
     if FChar in ['T', 't'] then
     begin
      FChar := FOnReadChar;
      if FChar in ['Y', 'y'] then
      begin
       FChar := FOnReadChar;
       if FChar in ['L', 'l'] then
       begin
        FChar := FOnReadChar;
        if FChar in ['E', 'e'] then
        begin
         FChar := FOnReadChar;
         while not (FChar in ['>', #0]) do
          FChar := FOnReadChar;
         if FChar = '>' then
         begin
          FChar := FOnReadChar;
          Break;
         end;
        end;
       end;
      end;
     end;
    end;
   end;
  end;
 until FChar = #0;
end;

{ TAbstractHtmlParser: Script ********** }

procedure TAbstractHtmlParser.ParseScript;
begin
 FElementType := eScript;
 ParseAttribs; // Deal with the Attribs first.
 FCDataPosition := 0;
 repeat
  while not (FChar in ['<', #0]) do
  begin
   AddToCDataBuffer (FChar);
   FChar := FOnReadChar;
  end;

  if FChar = '<' then
  begin
   AddToCDataBuffer (FChar);
   FChar := FOnReadChar;
   if FChar = '/' then
   begin
    AddToCDataBuffer (FChar);
    FChar := FOnReadChar;
    if FChar in ['S', 's'] then
    begin
     AddToCDataBuffer (FChar);
     FChar := FOnReadChar;
     if FChar in ['C', 'c'] then
     begin
      AddToCDataBuffer (FChar);
      FChar := FOnReadChar;
      if FChar in ['R', 'r'] then
      begin
       AddToCDataBuffer (FChar);
       FChar := FOnReadChar;
       if FChar in ['I', 'i'] then
       begin
        AddToCDataBuffer (FChar);
        FChar := FOnReadChar;
        if FChar in ['P', 'p'] then
        begin
         AddToCDataBuffer (FChar);
         FChar := FOnReadChar;
         if FChar in ['T', 't'] then
         begin
          Dec (FCDataPosition, 7); // '</Script' vom Buffer abziehen.
          FChar := FOnReadChar;
          while not (FChar in ['>', #0]) do
           FChar := FOnReadChar;
          if FChar = '>' then
          begin
           FChar := FOnReadChar;
           Break;
          end;
         end;
        end;
       end;
      end;
     end;
    end;
   end;
  end;
 until FChar = #0;
end;

{ ********** }

procedure TAbstractHtmlParser.SkipScript;
begin
 SkipAttribs; // Deal with the Attribs first.
 repeat
  while not (FChar in ['<', #0]) do
   FChar := FOnReadChar;

  if FChar = '<' then
  begin
   FChar := FOnReadChar;
   if FChar = '/' then
   begin
    FChar := FOnReadChar;
    if FChar in ['S', 's'] then
    begin
     FChar := FOnReadChar;
     if FChar in ['C', 'c'] then
     begin
      FChar := FOnReadChar;
      if FChar in ['R', 'r'] then
      begin
       FChar := FOnReadChar;
       if FChar in ['I', 'i'] then
       begin
        FChar := FOnReadChar;
        if FChar in ['P', 'p'] then
        begin
         FChar := FOnReadChar;
         if FChar in ['T', 't'] then
         begin
          FChar := FOnReadChar;
          while not (FChar in ['>', #0]) do
           FChar := FOnReadChar;
          if FChar = '>' then
          begin
           FChar := FOnReadChar;
           Break;
          end;
         end;
        end;
       end;
      end;
     end;
    end;
   end;
  end;
 until FChar = #0;
end;

{ TAbstractHtmlParser: Tag ********** }

procedure TAbstractHtmlParser.ParseTag;
var
 Hash                         : Cardinal;
 NewTag, Sign                 : Integer;
begin // FPosition steht auf dem '<'.
 FElementType := eTag;
 
 FTag.ClearAttribs;
 
 while FChar in csWhiteSpace do
  FChar := FOnReadChar; // Anfang des Tags suchen. p steht jetzt auf dem ersten Zeichen des Tags.
 
 if FChar = '/' then
 begin
  Sign := -1;
  FChar := FOnReadChar;
 end
 else
  Sign := 1;
 
 Hash := 0;
 FCDataPosition := 0;
 while FChar in TagChars do
 begin
  Hash := ((Hash shl 5) xor (Hash shr 27)) xor UpperCharArray[FChar];
  AddToCDataBuffer (FChar);
  FChar := FOnReadChar;
 end;
 Hash := Hash mod TAG_HASHSIZE;
 
 NewTag := TagFuncTable[Hash] (FCDataBuffer, FCDataPosition);
 // if FTag.tag = tagUndefined then raise Exception.Create (IntToStr (Hash));
 
 if NewTag = tagPre then
 begin
  Inc (FPreFormattedNesting, Sign);
  if FPreFormattedNesting < 0 then FPreFormattedNesting := 0; // Make sure it's not negative!
  UpdateHandleText;
 end;
 
 FTag.Tag := Sign * NewTag;
end;

{ ********** }

procedure TAbstractHtmlParser.ParseAttribs;
var
 Attrib                       : TAttribs;
 Quote                        : Char;
 Hash                         : Cardinal;
 s                            : AnsiString;
begin
 repeat
  while not (FChar in (AttribStartChars + ['>'])) do
   FChar := FOnReadChar;

  Hash := 0;
  FCDataPosition := 0;
  while FChar in AttribChars do
  begin
   Hash := ((Hash shl 5) xor (Hash shr 27)) xor UpperCharArray[FChar];
   AddToCDataBuffer (FChar);
   FChar := FOnReadChar;
  end;
  Hash := Hash mod ATTRIB_HASHSIZE;

  if FCDataPosition > 0 then
   Attrib := AttribFuncTable[Hash] (FCDataBuffer, FCDataPosition)
  else
   Break;

  if Attrib = attribUndefined then Continue;

  while FChar in csWhiteSpace do // Jump over WhiteSpace after an Attribute.
   FChar := FOnReadChar;

  if FChar <> '=' then
  begin // Weiteren Schlüssel, aber keinen Wert gefunden
   FTag.AddAttrib (Attrib, ''); // Leeren Schlüssel hinzufügen
   Continue;
  end;

  FChar := FOnReadChar;

  while FChar in csWhiteSpace do // Anfang des Werts suchen
   FChar := FOnReadChar;

  FCDataPosition := 0;
  if FChar in ['"', ''''] then
  begin // Wert ist in Gänsefüßchen gefaßt
   Quote := FChar;
   FChar := FOnReadChar;

   while not (FChar in [#0, Quote, '>']) do
   begin
    AddToCDataBuffer (FChar);
    FChar := FOnReadChar; // Ende des Werts, i.e. Gänsefüßchen suchen
   end;

   // if c in [#0, '>'] then Break;

   while (FCDataPosition > 0) and (FCDataBuffer[FCDataPosition - 1] in csWhiteSpace) do
    Dec (FCDataPosition); // Lerrzeichen zurückverfolgen

   SetString (s, FCDataBuffer, FCDataPosition); // Wert auslesen
   FTag.AddAttrib (Attrib, s); // Schlüssel und Wert hinzufügen

   FChar := FOnReadChar;
  end
  else
  begin // Wert steht ohne Gänsefüßchen
   while not (FChar in [#0..#32, '>']) do
   begin
    AddToCDataBuffer (FChar);
    FChar := FOnReadChar; // Anfang des Werts suchen
   end;

   SetString (s, FCDataBuffer, FCDataPosition); // Wert auslesen
   FTag.AddAttrib (Attrib, s); // Schlüssel und Wert hinzufügen
  end;

 until FChar in [#0, '>'];
 
 if FChar = '>' then FChar := FOnReadChar;
end;

{ ********** }

procedure TAbstractHtmlParser.SkipAttribs;
begin
 while not (FChar in ['>', #0]) do
  FChar := FOnReadChar;
 if FChar = '>' then FChar := FOnReadChar;
end;

{ TAbstractHtmlParser: Text ********** }

procedure TAbstractHtmlParser.ParseTextNormal;
var
 LastWasSpace                 : Boolean;
begin
 FElementType := eText;
 FCDataPosition := 0;
 LastWasSpace := False;
 while not (FChar in ['<', #0]) do
  case FChar of
   '&':
    begin
     if ParseEntity in csWhiteSpace then
      if LastWasSpace then
       Dec (FCDataPosition)
      else
       LastWasSpace := True
     else
      LastWasSpace := False;
    end;
   #1..#32:
    begin // csWhiteSpace
     repeat // Eat up all WhiteSpace.
      FChar := FOnReadChar;
     until not (FChar in csWhiteSpace);
     if not LastWasSpace then
     begin
      AddToCDataBuffer (#32);
      LastWasSpace := True;
     end;
    end;
  else
   begin
    repeat // Eat up all regular CData Characters.
     AddToCDataBuffer (FChar);
     FChar := FOnReadChar;
    until FChar in (csWhiteSpace + ['<', #0, '&']);
    LastWasSpace := False;
   end;
  end;
end;

{ ********** }

procedure TAbstractHtmlParser.ParseTextPreformatted;
begin
 FElementType := eText;
 FCDataPosition := 0;
 while not (FChar in ['<', #0]) do
 begin
  if FChar = '&' then
   ParseEntity
  else
   repeat
    AddToCDataBuffer (FChar);
    FChar := FOnReadChar;
   until FChar in ['<', #0, '&'];
 end;
end;

{ ********** }

procedure TAbstractHtmlParser.SkipText;
begin
 while not (FChar in ['<', #0]) do
  FChar := FOnReadChar;
end;

{ ********** }

function TAbstractHtmlParser.ParseEntity: Char;
var
 Hash                         : Cardinal;
 BufPosition                  : Integer;
 j                            : Integer;
begin
 AddToCDataBuffer (FChar); // Store '&' in case entity is invalid.
 BufPosition := FCDataPosition;
 FChar := FOnReadChar;
 
 if FChar = '#' then
 begin // HTML-Entity ist dezimal kodiert
  AddToCDataBuffer (FChar);
  FChar := FOnReadChar;
  j := 0;
  while FChar in ['0'..'9'] do
  begin
   j := j * 10 + Ord (FChar) - 48;
   AddToCDataBuffer (FChar);
   FChar := FOnReadChar;
  end;
  if j in [9, 10, 13, 32..126, 130..140, 145..156, 159..255] then
   Result := Char (j)
  else
   Result := #0;
 end
 else
 begin
  Hash := 0;
  while FChar in EntityChars do
  begin
   Hash := ((Hash shl 5) xor (Hash shr 27)) xor Byte (FChar);
   AddToCDataBuffer (FChar);
   FChar := FOnReadChar;
  end;
  Hash := Hash mod ENTITY_HASHSIZE;

  if (FCDataPosition > BufPosition) then
   Result := EntityFuncTable[Hash] (FCDataBuffer + BufPosition, FCDataPosition - BufPosition)
  else
   Result := #0;
 end;
 
 if Result <> #0 then
 begin
  FCDataPosition := BufPosition - 1; // Substract the '&' added before.
  AddToCDataBuffer (Result);
  if FChar = ';' then FChar := FOnReadChar;
 end;
end;

{ CDataBuffer & CDataString ************************************************** }

procedure TAbstractHtmlParser.AddToCDataBuffer (const c: Char);
begin
 if FCDataPosition >= FCDataCapacity then
 begin
  FCDataCapacity := ((FCDataCapacity + (FCDataCapacity div 4) + 4) div 4) * 4;
  ReAllocMem (FCDataBuffer, FCDataCapacity);
 end;
 FCDataBuffer[FCDataPosition] := c;
 Inc (FCDataPosition);
end;

{ ********** }

function TAbstractHtmlParser.GetCDataString: AnsiString;
begin
 SetString (Result, FCDataBuffer, FCDataPosition);
end;

{ ********** }

procedure TAbstractHtmlParser.TerminateCDataBuffer;
begin
 AddToCDataBuffer (#0);
end;

{ SourceString & SourceBuffer ************************************************ }

procedure TAbstractHtmlParser.SetSourceString (const Value: AnsiString);
begin
 FSourceString := Value;
 SetSourceBuffer (PChar (FSourceString));
end;

{ ********** }

procedure TAbstractHtmlParser.SetSourceBuffer (const Value: PChar);
begin
 FSourcePointer := Value;
 FSourcePosition := 0;
 FOnReadChar := ReadCharFromSourceBuffer;
 FChar := ReadCharFromSourceBuffer;
end;

{ ********** }

function TAbstractHtmlParser.ReadCharFromSourceBuffer: Char;
begin
 Result := FSourcePointer[FSourcePosition];
 Inc (FSourcePosition);
end;

{ ********** }

procedure TAbstractHtmlParser.ResetSourceBuffer;
begin
 FSourcePosition := 0;
end;

{ ********** }

procedure TAbstractHtmlParser.InitOnReadCharFunc;
begin
 FChar := FOnReadChar;
end;

{ **************************************************************************** }
{ THtmlReporter: Various Constructors }
{ **************************************************************************** }

constructor THtmlReporter.Create{$IFDEF Component} (AOwner: TComponent){$ENDIF};
{ Creates full parser: Report everything. }
begin
 inherited Create{$IFDEF Component} (AOwner){$ENDIF};
 InitHandleProcs;
 SetDefaultReporter;
end;

{ ********** }

constructor THtmlReporter.CreateTagParser{$IFDEF Component} (AOwner: TComponent){$ENDIF};
{ Creates tag only parser. }
begin
 Create{$IFDEF Component} (AOwner){$ENDIF};
 SetTagReporter;
end;

{ ********** }

constructor THtmlReporter.CreateTextParser{$IFDEF Component} (AOwner: TComponent){$ENDIF};
{ Creates text only parser. }
begin
 Create{$IFDEF Component} (AOwner){$ENDIF};
 SetTextReporter;
end;

procedure THtmlReporter.InitHandleProcs;
var
 i                            : Integer;
begin
 FHandleTag := ParseTagAndAttribs;
 
 for i := Low (TTags) to High (TTags) do
 begin
  FHandleTagsSkip[i] := SkipAttribs;
  FHandleTagsReportFiltered[i] := SkipAttribs;
  FHandleTagsReport[i] := ParseReportAttribs;
 end;
 
 FHandleTagsSkip[tagUndefined] := SkipAttribs; // Skip tagUndefined.
 FHandleTagsReportFiltered[tagUndefined] := SkipAttribs;
 FHandleTagsReport[tagUndefined] := SkipAttribs;
end;

function THtmlReporter.GetReportTagFilter (const Tag: TTags): Boolean;
begin
 Result := FReportTagsFilterArray[Tag];
end;

procedure THtmlReporter.SetReportTagFilter (const Tag: TTags; const Value: Boolean);
begin
 FReportTagsFilterArray[Tag] := Value;
 UpdateSingleTagProc (Tag);
end;

{ ********** }

procedure THtmlReporter.ReportTagsFilter (const Tags: array of TTags; const Value: Boolean);
var
 i                            : Integer;
 b                            : Boolean;
begin
 b := not Value;
 
 for i := Low (TTags) to High (TTags) do
  FReportTagsFilterArray[i] := b;
 
 i := High (Tags);
 while i >= 0 do
 begin // Clear / Set specified.
  FReportTagsFilterArray[Tags[i]] := Value;
  Dec (i);
 end;
 
 UpdateAllTagProcs;
end;

{ ********** }

procedure THtmlReporter.UpdateAllHandlers;
begin
 SetHandleASPs;
 SetHandleComments;
 SetHandleDTDs;
 SetHandleScripts;
 SetHandleSSIs;
 SetHandleStyles;
 SetHandleTags;
 SetHandleText;
end;

procedure THtmlReporter.UpdateAllTagProcs;
var
 i                            : Integer;
begin
 i := tagMax;
 while i <> tagUndefined do
 begin // StartTags
  if not (i in SpecialTags) then UpdateSingleStartTagProc (i);
  Dec (i);
 end;
 
 i := -tagMax;
 while i <> tagUndefined do
 begin // EndTags
  if not (Abs (i) in SpecialTags) then UpdateSingleEndTagProc (i);
  Inc (i);
 end;
end;

{ ********** }

procedure THtmlReporter.UpdateSingleTagProc (const Tag: TTags);
begin
 Assert (not (Abs (Tag) in SpecialTags), 'Cannot update TagProcs for SpecialTags.');
 
 if Tag > 0 then
  UpdateSingleStartTagProc (Tag)
 else
  if Tag < 0 then
   UpdateSingleEndTagProc (Tag);
end;

{ ********** }

procedure THtmlReporter.UpdateSingleStartTagProc (const Tag: TStartTags);
begin
 if FReportTagsFilterArray[Tag] then
  FHandleTagsReportFiltered[Tag] := ParseReportAttribs
 else
  FHandleTagsReportFiltered[Tag] := SkipAttribs;
end;

{ ********** }

procedure THtmlReporter.UpdateSingleEndTagProc (const Tag: TEndTags);
begin
 if FReportTagsFilterArray[Tag] then
  FHandleTagsReportFiltered[Tag] := SkipReportAttribs
 else
  FHandleTagsReportFiltered[Tag] := SkipAttribs;
end;

{ THtmlReporter: Setting Handlers ********************************************** }

procedure THtmlReporter.SetHandleASPs;
begin
 if FReportASPs then
  FHandleASP := ParseReportASP
 else
  FHandleASP := SkipASP;
end;

{ ********** }

procedure THtmlReporter.SetHandleComments;
begin
 if FReportComments then
  FHandleComment := ParseReportComment
 else
  FHandleComment := SkipDoubleDash;
end;

{ ********** }

procedure THtmlReporter.SetHandleDTDs;
begin
 if FReportDTDs then
  FHandleDTD := ParseReportDTD
 else
  FHandleDTD := SkipDTD;
end;

{ ********** }

procedure THtmlReporter.SetAllTagHandlers (const Tag: TTags; const Proc: TProcEvent);
begin
 FHandleTagsSkip[Tag] := Proc;
 FHandleTagsReportFiltered[Tag] := Proc;
 FHandleTagsReport[Tag] := Proc;
end;

{ ********** }

procedure THtmlReporter.SetHandleScripts;
begin
 if FReportScripts then
  SetAllTagHandlers (tagSCRIPT, ParseReportScript)
 else
  SetAllTagHandlers (tagSCRIPT, SkipScript);
end;

{ ********** }

procedure THtmlReporter.SetHandleSSIs;
begin
 if FReportSSIs then
  FHandleSSI := ParseReportSSI
 else
  FHandleSSI := SkipDoubleDash;
end;

{ ********** }

procedure THtmlReporter.SetHandleStyles;
begin
 if FReportStyles then
  SetAllTagHandlers (tagSTYLE, ParseReportStyle)
 else
  SetAllTagHandlers (tagSTYLE, SkipStyle);
end;

{ ********** }

procedure THtmlReporter.SetHandleTags;
begin
 if FReportTags then
  if FReportTagsFiltered then
   FHandleTags := @FHandleTagsReportFiltered
  else
   FHandleTags := @FHandleTagsReport
 else
  FHandleTags := @FHandleTagsSkip;
end;

{ ********** }

procedure THtmlReporter.SetHandleText;
begin
 if FReportText then
 begin
  FHandleTextNormal := ParseReportTextNormal;
  FHandleTextPreformatted := ParseReportTextPreformatted;
 end
 else
 begin
  FHandleTextNormal := SkipText;
  FHandleTextPreformatted := SkipText;
 end;
 UpdateHandleText;
end;

{ **************************************************************************** }
{ THtmlReporter: Setting Report Properties. }
{ **************************************************************************** }

procedure THtmlReporter.SetReportASPs (const Value: Boolean);
begin
 FReportASPs := Value;
 SetHandleASPs;
end;

{ ********** }

procedure THtmlReporter.SetReportComments (const Value: Boolean);
begin
 FReportComments := Value;
 SetHandleComments;
end;

{ ********** }

procedure THtmlReporter.SetReportDTDs (const Value: Boolean);
begin
 FReportDTDs := Value;
 SetHandleDTDs;
end;

{ ********** }

procedure THtmlReporter.SetReportScripts (const Value: Boolean);
begin
 FReportScripts := Value;
 SetHandleScripts;
end;

{ ********** }

procedure THtmlReporter.SetReportSSIs (const Value: Boolean);
begin
 FReportSSIs := Value;
 SetHandleSSIs;
end;

{ ********** }

procedure THtmlReporter.SetReportStyles (const Value: Boolean);
begin
 FReportStyles := Value;
 SetHandleStyles;
end;

{ ********** }

procedure THtmlReporter.SetReportTags (const Value: Boolean);
begin
 FReportTags := Value;
 SetHandleTags
end;

{ ********** }

procedure THtmlReporter.SetReportTagsFiltered (const Value: Boolean);
begin
 FReportTagsFiltered := Value;
 SetHandleTags;
end;

{ ********** }

procedure THtmlReporter.SetReportText (const Value: Boolean);
begin
 FReportText := Value;
 SetHandleText;
end;

{ **************************************************************************** }

procedure THtmlReporter.SetDefaultReporter;
begin
 FReportASPs := REPORT_ASPS_DEFAULT;
 FReportComments := REPORT_COMMENTS_DEFUALT;
 FReportDTDs := REPORT_DTDS_DEFUALT;
 FReportScripts := REPORT_SCRIPTS_DEFUALT;
 FReportSSIs := REPORT_SSIS_DEFUALT;
 FReportStyles := REPORT_STYLES_DEFUALT;
 FReportTags := REPORT_TAGS_DEFUALT;
 FReportText := REPORT_TEXT_DEFUALT;
 UpdateAllHandlers;
end;

{ ********** }

procedure THtmlReporter.SetFullReporter;
begin
 FReportASPs := True;
 FReportComments := True;
 FReportDTDs := True;
 FReportScripts := True;
 FReportSSIs := True;
 FReportStyles := True;
 FReportTags := True;
 FReportText := True;
 UpdateAllHandlers;
end;

{ ********** }

procedure THtmlReporter.SetTagReporter;
begin
 FReportASPs := False;
 FReportComments := False;
 FReportDTDs := False;
 FReportScripts := False;
 FReportSSIs := False;
 FReportStyles := False;
 FReportTags := True;
 FReportText := False;
 UpdateAllHandlers;
end;

{ ********** }

procedure THtmlReporter.SetTextReporter;
begin
 FReportASPs := False;
 FReportComments := False;
 FReportDTDs := False;
 FReportScripts := False;
 FReportSSIs := False;
 FReportStyles := False;
 FReportTags := False;
 FReportText := True;
 UpdateAllHandlers;
end;

{ ********** }

procedure THtmlReporter.SetCommentReporter;
begin
 FReportASPs := False;
 FReportComments := True;
 FReportDTDs := False;
 FReportScripts := False;
 FReportSSIs := False;
 FReportStyles := False;
 FReportTags := False;
 FReportText := False;
 UpdateAllHandlers;
end;

{ **************************************************************************** }

function THtmlReporter.Next: Boolean;
begin
 FReportCurrent := False;
 while FChar <> #0 do
 begin
  NextPrim;
  if FReportCurrent then
  begin
   Result := True;
   Exit;
  end;
 end;
 Result := False;
end;

{ Search / Match Primary Functions ********** }

function THtmlReporter.SearchPrim (const Value: AnsiString; const CaseSensitive: Boolean): Pointer;
var
 s                            : TSearch;
begin
 Result := nil;
 s := TSearch.Create (Value, CaseSensitive);
 try
  while Next do
  begin
   Result := s.SearchBuffer (FCDataBuffer, FCDataPosition);
   if Result <> nil then Break;
  end;
 finally
  s.Free;
 end;
end;

{ ********** }

function THtmlReporter.MatchPrim (const Pattern: AnsiString; const Registers: PRegExpRegisters): Boolean;
var
 r                            : TRegExpr;
begin
 Result := False;
 r := TRegExpr.Create;
 try
  r.Pattern := Pattern;
  while Next do
  begin
   if r.MatchBuffer (FCDataBuffer, FCDataPosition, Registers) then
   begin
    Result := True;
    Exit;
   end;
  end;
 finally
  r.Free;
 end;
end;

{ Search / Match a Comment ********** }

function THtmlReporter.SearchComment (const Value: AnsiString; const CaseSensitive: Boolean = True): Pointer;
var
 Settings                     : Integer;
begin
 SaveSettings (Settings);
 SetCommentReporter;
 
 Result := SearchPrim (Value, CaseSensitive);
 
 RestoreSettings (Settings);
 UpdateAllHandlers;
end;

{ ********** }

function THtmlReporter.MatchComment (const Pattern: AnsiString; const Registers: PRegExpRegisters): Boolean;
var
 Settings                     : Integer;
begin
 SaveSettings (Settings);
 SetCommentReporter;
 
 Result := MatchPrim (Pattern, Registers);
 
 RestoreSettings (Settings);
 UpdateAllHandlers;
end;

{ Search / Match some Text ********** }

function THtmlReporter.SearchText (const Value: AnsiString; const CaseSensitive: Boolean = True): Pointer;
var
 Settings                     : Integer;
begin
 SaveSettings (Settings);
 SetTextReporter;
 
 Result := SearchPrim (Value, CaseSensitive);
 
 RestoreSettings (Settings);
 UpdateAllHandlers;
end;

{ ********** }

function THtmlReporter.MatchText (const Pattern: AnsiString; const Registers: PRegExpRegisters): Boolean;
var
 Settings                     : Integer;
begin
 SaveSettings (Settings);
 SetTextReporter;
 
 Result := MatchPrim (Pattern, Registers);
 
 RestoreSettings (Settings);
 UpdateAllHandlers;
end;

{ Saving / Restoring Settings ********** }

procedure THtmlReporter.SaveSettings (var i: Integer);
begin
 i := 0;
 if FReportASPs then i := i or SETTING_REPORT_ASPS;
 if FReportComments then i := i or SETTING_REPORT_COMMENTS;
 if FReportDTDs then i := i or SETTING_REPORT_DTDS;
 if FReportScripts then i := i or SETTING_REPORT_SCRIPTS;
 if FReportSSIs then i := i or SETTING_REPORT_SSIS;
 if FReportStyles then i := i or SETTING_REPORT_STYLES;
 if FReportTags then i := i or SETTING_REPORT_TAGS;
 if FReportTagsFiltered then i := i or SETTING_REPORT_TAGS_FILTERED;
 if FReportText then i := i or SETTING_REPORT_TEXT;
end;

procedure THtmlReporter.RestoreSettings (const i: Integer);
begin
 FReportASPs := i and SETTING_REPORT_ASPS <> 0;
 FReportComments := i and SETTING_REPORT_COMMENTS <> 0;
 FReportDTDs := i and SETTING_REPORT_DTDS <> 0;
 FReportScripts := i and SETTING_REPORT_SCRIPTS <> 0;
 FReportSSIs := i and SETTING_REPORT_SSIS <> 0;
 FReportStyles := i and SETTING_REPORT_STYLES <> 0;
 FReportTags := i and SETTING_REPORT_TAGS <> 0;
 FReportTagsFiltered := i and SETTING_REPORT_TAGS_FILTERED <> 0;
 FReportText := i and SETTING_REPORT_TEXT <> 0;
end;

{ **************************************************************************** }
{ THtmlReporter: Parsing Procedures }
{ **************************************************************************** }

{ THtmlReporter: ASP ********** }

procedure THtmlReporter.ParseReportASP;
begin
 ParseASP;
 FReportCurrent := True;
end;

{ THtmlReporter: Attribs ********** }

procedure THtmlReporter.ParseReportAttribs;
begin
 ParseAttribs;
 FReportCurrent := True;
end;

{ ********** }

procedure THtmlReporter.SkipReportAttribs;
begin
 SkipAttribs;
 FReportCurrent := True;
end;

{ THtmlReporter: Comment ********** }

procedure THtmlReporter.ParseReportComment;
begin
 ParseComment;
 FReportCurrent := True;
end;

{ THtmlReporter: DTD ********** }

procedure THtmlReporter.ParseReportDTD;
begin
 ParseDTD;
 FReportCurrent := True;
end;

{ THtmlReporter: Script ********** }

procedure THtmlReporter.ParseReportScript;
begin
 ParseScript;
 FReportCurrent := True;
end;

{ THtmlReporter: SSI ********** }

procedure THtmlReporter.ParseReportSSI;
begin
 ParseSSI;
 FReportCurrent := True;
end;

{ THtmlReporter: Style ********** }

procedure THtmlReporter.ParseReportStyle;
begin
 ParseStyle;
 FReportCurrent := True;
end;

{ THtmlReporter: Text ********** }

procedure THtmlReporter.ParseReportTextNormal;
begin
 ParseTextNormal;
 FReportCurrent := True;
end;

{ ********** }

procedure THtmlReporter.ParseReportTextPreformatted;
begin
 ParseTextPreformatted;
 FReportCurrent := True;
end;

{ THtmlReporter: Tags and Attribs ********** }

procedure THtmlReporter.ParseTagAndAttribs;
begin
 ParseTag;
 FHandleTags[FTag.Tag];
end;

{ **************************************************************************** }
{ THtmlWriter: Constructor & Destructor }
{ **************************************************************************** }

constructor THtmlWriter.Create{$IFDEF Component} (AOwner: TComponent){$ENDIF};
begin
 inherited Create{$IFDEF Component} (AOwner){$ENDIF};
 FOnWriteDest := WriteToDestBuffer;
end;

{ ********** }

destructor THtmlWriter.Destroy;
begin
 if Assigned (FDestBuffer) then FreeMem (FDestBuffer, FDestCapacity);
 inherited Destroy;
end;

{ ********** }

function THtmlWriter.GetDestString: AnsiString;
begin
 SetString (Result, FDestBuffer, FDestPosition);
end;

{ **************************************************************************** }
{ THtmlWriter: General Writing Routines }
{ **************************************************************************** }

procedure THtmlWriter.WriteToDestBuffer (const Buffer; Count: Integer);
var
 NewPosition                  : Integer;
begin
 NewPosition := FDestPosition + Count;
 if NewPosition > FDestCapacity then
 begin
  FDestCapacity := ((NewPosition + (NewPosition div 4) + 4) div 4) * 4; // Set new capacity to a multiple of 4.
  ReAllocMem (FDestBuffer, FDestCapacity);
 end;
 System.Move (Buffer, Pointer (Integer (FDestBuffer) + FDestPosition)^, Count);
 FDestPosition := NewPosition;
end;

{ ********** }

procedure THtmlWriter.WriteASP (const s: AnsiString);
begin
 FOnWriteDest ('<%', 2);
 WriteString (s);
 FOnWriteDest ('%>', 2);
end;

{ ********** }

procedure THtmlWriter.WriteComment (const s: AnsiString);
begin
 FOnWriteDest ('<!--', 4);
 WriteString (s);
 FOnWriteDest ('-->', 3);
end;

{ ********** }

procedure THtmlWriter.WriteDTD (const s: AnsiString);
begin
 FOnWriteDest ('<!', 2);
 WriteString (s);
 WriteString ('>');
end;

{ ********** }

procedure THtmlWriter.WriteSSI (const s: AnsiString);
begin
 FOnWriteDest ('<!--#', 6);
 WriteString (s);
 FOnWriteDest ('-->', 3);
end;

{ ********** }

procedure THtmlWriter.WriteString (const s: AnsiString);
begin
 FOnWriteDest (PChar (s)^, Length (s));
end;

{ ********** }

procedure THtmlWriter.WriteTag (const Tag: THtmlTag);
var
 i                            : Integer;
 s                            : AnsiString;
begin
 if Tag.Tag < tagUndefined then
  FOnWriteDest ('</', 2)
 else
  WriteString ('<');
 
 WriteString (TagNames[Abs (Tag.Tag)]);
 
 i := 0;
 while i < Tag.AttribCount do
 begin
  WriteString (' ');
  WriteString (AttribNames[Tag.Attribs[i]]);

  s := Tag.Values[i];
  if not IsEmptyString (s) then
  begin
   WriteString ('="');
   WriteString (s);
   WriteString ('"');
  end;
  Inc (i);
 end;
 WriteString ('>');
end;

{ **************************************************************************** }
{ THtmlWriter: Current Writing Routines -- Write the current Elements }
{ **************************************************************************** }

procedure THtmlWriter.WriteCurrentElement;
begin
 case FElementType of
  eComment:
   WriteCurrentComment;
  eDTD:
   WriteCurrentDTD;
  eSSI:
   WriteCurrentSSI;
  eScript, eStyle:
   WriteCurrentTagAndCData;
  eTag:
   WriteCurrentTag;
  eText:
   WriteCurrentCData;
 end;
end;

{ ********** }

procedure THtmlWriter.WriteCurrentASP;
begin
 FOnWriteDest ('<%', 2);
 FOnWriteDest (FCDataBuffer^, FCDataPosition);
 FOnWriteDest ('%>', 2);
end;

{ ********** }

procedure THtmlWriter.WriteCurrentCData;
begin
 FOnWriteDest (FCDataBuffer^, FCDataPosition);
end;

{ ********** }

procedure THtmlWriter.WriteCurrentComment;
begin
 FOnWriteDest ('<!--', 4);
 FOnWriteDest (FCDataBuffer^, FCDataPosition);
 FOnWriteDest ('-->', 3);
end;

{ ********** }

procedure THtmlWriter.WriteCurrentDTD;
begin
 FOnWriteDest ('<!', 2);
 FOnWriteDest (FCDataBuffer^, FCDataPosition);
 WriteString ('>');
end;

{ ********** }

procedure THtmlWriter.WriteCurrentSSI;
begin
 FOnWriteDest ('<!--#', 5);
 FOnWriteDest (FCDataBuffer^, FCDataPosition);
 FOnWriteDest ('-->', 3);
end;

{ ********** }

procedure THtmlWriter.WriteCurrentTag;
begin
 WriteTag (FTag);
end;

{ ********** }

procedure THtmlWriter.WriteCurrentTagAndCData;
begin
 WriteCurrentStartTag;
 FOnWriteDest (FCDataBuffer^, FCDataPosition);
 WriteCurrentEndTag;
end;

{ ********** }

procedure THtmlWriter.WriteCurrentStartTag;
var
 i                            : Integer;
 s                            : AnsiString;
begin
 if FTag.Tag < tagUndefined then
  FOnWriteDest ('</', 2)
 else
  WriteString ('<');
 
 WriteString (TagNames[Abs (FTag.Tag)]);
 
 i := 0;
 while i < FTag.AttribCount do
 begin
  WriteString (' ');
  WriteString (AttribNames[FTag.Attribs[i]]);

  s := FTag.Values[i];
  if not IsEmptyString (s) then
  begin
   WriteString ('="');
   WriteString (s);
   WriteString ('"');
  end;
  Inc (i);
 end;
 WriteString ('>');
end;

{ ********** }

procedure THtmlWriter.WriteCurrentEndTag;
begin
 FOnWriteDest ('</', 2);
 WriteString (TagNames[Abs (FTag.Tag)]);
 WriteString ('>');
end;

{ **************************************************************************** }
{ THtmlAutoWriter: Routines setting the AutoWrite Procedures }
{ **************************************************************************** }

constructor THtmlAutoWriter.Create{$IFDEF Component} (AOwner: TComponent){$ENDIF};
begin
 inherited Create{$IFDEF Component} (AOwner){$ENDIF};
 SetDefaultAutoWriter;
end;

{ Saving / Restoring Settings ********** }

procedure THtmlAutoWriter.SaveSettings (var i: Integer);
begin
 i := 0;
 if FAutoWriteASPs then i := i or SETTING_AUTO_WRITE_ASPS;
 if FAutoWriteComments then i := i or SETTING_AUTO_WRITE_COMMENTS;
 if FAutoWriteDTDs then i := i or SETTING_AUTO_WRITE_DTDS;
 if FAutoWriteScripts then i := i or SETTING_AUTO_WRITE_SCRIPTS;
 if FAutoWriteSSIs then i := i or SETTING_AUTO_WRITE_SSIS;
 if FAutoWriteStyles then i := i or SETTING_AUTO_WRITE_STYLES;
 if FAutoWriteTags then i := i or SETTING_AUTO_WRITE_TAGS;
 if FAutoWriteTagsFiltered then i := i or SETTING_AUTO_WRITE_TAGS_FILTERED;
 if FAutoWriteText then i := i or SETTING_AUTO_WRITE_TEXT;
end;

{ ********** }

procedure THtmlAutoWriter.RestoreSettings (const i: Integer);
begin
 FAutoWriteASPs := i and SETTING_AUTO_WRITE_ASPS <> 0;
 FAutoWriteComments := i and SETTING_AUTO_WRITE_COMMENTS <> 0;
 FAutoWriteDTDs := i and SETTING_AUTO_WRITE_DTDS <> 0;
 FAutoWriteScripts := i and SETTING_AUTO_WRITE_SCRIPTS <> 0;
 FAutoWriteSSIs := i and SETTING_AUTO_WRITE_SSIS <> 0;
 FAutoWriteStyles := i and SETTING_AUTO_WRITE_STYLES <> 0;
 FAutoWriteTags := i and SETTING_AUTO_WRITE_TAGS <> 0;
 FAutoWriteTagsFiltered := i and SETTING_AUTO_WRITE_TAGS_FILTERED <> 0;
 FAutoWriteText := i and SETTING_AUTO_WRITE_TEXT <> 0;
end;

{ **************************************************************************** }

function THtmlAutoWriter.GetAutoWriteTagFilter (const Tag: TTags): Boolean;
begin
 Result := FAutoWriteTagsFilterArray[Tag];
end;

{ **************************************************************************** }

procedure THtmlAutoWriter.SetAutoWriteTagFilter (const Tag: TTags; const Value: Boolean);
begin
 FAutoWriteTagsFilterArray[Tag] := Value;
 UpdateSingleTagProc (Tag);
end;

{ ********** }

procedure THtmlAutoWriter.AutoWriteTagsFilter (const Tags: array of TTags; const Value: Boolean);
var
 i                            : Integer;
 b                            : Boolean;
begin
 b := not Value;
 
 for i := Low (TTags) to High (TTags) do
  FAutoWriteTagsFilterArray[i] := b;
 
 i := High (Tags);
 while i >= 0 do
 begin // Clear / Set specified.
  FAutoWriteTagsFilterArray[Tags[i]] := Value;
  Dec (i);
 end;
 
 UpdateAllTagProcs;
end;

{ **************************************************************************** }

procedure THtmlAutoWriter.SetDefaultAutoWriter;
begin
 FAutoWriteASPs := AUTO_WRITE_ASPS_DEFAULT;
 FAutoWriteComments := AUTO_WRITE_COMMENTS_DEFUALT;
 FAutoWriteDTDs := AUTO_WRITE_DTDS_DEFUALT;
 FAutoWriteScripts := AUTO_WRITE_SCRIPTS_DEFUALT;
 FAutoWriteSSIs := AUTO_WRITE_SSIS_DEFUALT;
 FAutoWriteStyles := AUTO_WRITE_STYLES_DEFUALT;
 FAutoWriteTags := AUTO_WRITE_TAGS_DEFUALT;
 FAutoWriteText := AUTO_WRITE_TEXT_DEFUALT;
 UpdateAllHandlers;
end;

{ ********** }

procedure THtmlAutoWriter.FullTextAutoWriter;
begin
 FAutoWriteASPs := True;
 FAutoWriteComments := True;
 FAutoWriteDTDs := True;
 FAutoWriteScripts := True;
 FAutoWriteSSIs := True;
 FAutoWriteStyles := True;
 FAutoWriteTags := True;
 FAutoWriteText := True;
 UpdateAllHandlers;
end;

{ ********** }

procedure THtmlAutoWriter.SetTagAutoWriter;
begin
 FAutoWriteASPs := False;
 FAutoWriteComments := False;
 FAutoWriteDTDs := False;
 FAutoWriteScripts := False;
 FAutoWriteSSIs := False;
 FAutoWriteStyles := False;
 FAutoWriteTags := True;
 FAutoWriteText := False;
 UpdateAllHandlers;
end;

{ ********** }

procedure THtmlAutoWriter.SetTextAutoWriter;
begin
 FAutoWriteASPs := False;
 FAutoWriteComments := False;
 FAutoWriteDTDs := False;
 FAutoWriteScripts := False;
 FAutoWriteSSIs := False;
 FAutoWriteStyles := False;
 FAutoWriteTags := False;
 FAutoWriteText := True;
 UpdateAllHandlers;
end;

{ **************************************************************************** }
{ THtmlAutoWriter: Setting Properties }
{ **************************************************************************** }

procedure THtmlAutoWriter.SetAutoWriteASPs (const Value: Boolean);
begin
 FAutoWriteASPs := Value;
 SetHandleASPs;
end;

{ ********** }

procedure THtmlAutoWriter.SetAutoWriteComments (const Value: Boolean);
begin
 FAutoWriteComments := Value;
 SetHandleComments;
end;

{ ********** }

procedure THtmlAutoWriter.SetAutoWriteDTDs (const Value: Boolean);
begin
 FAutoWriteDTDs := Value;
 SetHandleDTDs;
end;

{ ********** }

procedure THtmlAutoWriter.SetAutoWriteScripts (const Value: Boolean);
begin
 FAutoWriteScripts := Value;
 SetHandleScripts;
end;

{ ********** }

procedure THtmlAutoWriter.SetAutoWriteSSIs (const Value: Boolean);
begin
 FAutoWriteSSIs := Value;
 SetHandleSSIs;
end;

{ ********** }

procedure THtmlAutoWriter.SetAutoWriteStyles (const Value: Boolean);
begin
 FAutoWriteStyles := Value;
 SetHandleStyles;
end;

{ ********** }

procedure THtmlAutoWriter.SetAutoWriteTags (const Value: Boolean);
begin
 FAutoWriteTags := Value;
 SetHandleTags;
end;

{ ********** }

procedure THtmlAutoWriter.SetAutoWriteTagsFiltered (const Value: Boolean);
begin
 FAutoWriteTagsFiltered := Value;
 SetHandleTags;
end;

{ ********** }

procedure THtmlAutoWriter.SetAutoWriteText (const Value: Boolean);
begin
 FAutoWriteText := Value;
 SetHandleText;
end;

{ **************************************************************************** }
{ THtmlAutoWriter: Parsing and Skipping Procedures. }
{ **************************************************************************** }

{ Attribs ********** }

procedure THtmlAutoWriter.ParseReportWriteAttribs;
begin
 ParseReportAttribs;
 WriteCurrentTag;
end;

{ ********** }

procedure THtmlAutoWriter.SkipReportWriteAttribs;
begin
 SkipReportAttribs;
 WriteCurrentTag;
end;

{ ********** }

procedure THtmlAutoWriter.ParseWriteAttribs;
begin
 ParseAttribs;
 WriteCurrentTag;
end;

{ ********** }

procedure THtmlAutoWriter.SkipWriteAttribs;
begin
 SkipAttribs;
 WriteCurrentTag;
end;

{ ASP ********** }

procedure THtmlAutoWriter.ParseReportWriteASP;
begin
 ParseReportASP;
 WriteCurrentASP;
end;

{ ********** }

procedure THtmlAutoWriter.ParseWriteASP;
begin
 ParseASP;
 WriteCurrentASP;
end;

{ Comment ********** }

procedure THtmlAutoWriter.ParseReportWriteComment;
begin
 ParseReportComment;
 WriteCurrentComment;
end;

{ ********** }

procedure THtmlAutoWriter.ParseWriteComment;
begin
 ParseComment;
 WriteCurrentComment;
end;

{ DTD ********** }

procedure THtmlAutoWriter.ParseReportWriteDTD;
begin
 ParseReportDTD;
 WriteCurrentDTD;
end;

{ ********** }

procedure THtmlAutoWriter.ParseWriteDTD;
begin
 ParseDTD;
 WriteCurrentDTD;
end;

{ Script ********** }

procedure THtmlAutoWriter.ParseReportWriteScript;
begin
 ParseReportScript;
 WriteCurrentTagAndCData;
end;

{ ********** }

procedure THtmlAutoWriter.ParseWriteScript;
begin
 ParseScript;
 WriteCurrentTagAndCData;
end;

{ SSI ********** }

procedure THtmlAutoWriter.ParseReportWriteSSI;
begin
 ParseReportSSI;
 WriteCurrentSSI;
end;

{ ********** }

procedure THtmlAutoWriter.ParseWriteSSI;
begin
 ParseSSI;
 WriteCurrentSSI;
end;

{ Style ********** }

procedure THtmlAutoWriter.ParseReportWriteStyle;
begin
 ParseReportStyle;
 WriteCurrentTagAndCData;
end;

{ ********** }

procedure THtmlAutoWriter.ParseWriteStyle;
begin
 ParseStyle;
 WriteCurrentTagAndCData;
end;

{ Text ********** }

procedure THtmlAutoWriter.ParseReportWriteTextNormal;
begin
 ParseReportTextNormal;
 WriteCurrentCData;
end;

{ ********** }

procedure THtmlAutoWriter.ParseReportWriteTextPreformatted;
begin
 ParseReportTextPreformatted;
 WriteCurrentCData;
end;

{ ********** }

procedure THtmlAutoWriter.ParseWriteTextNormal;
begin
 ParseTextNormal;
 WriteCurrentCData;
end;

{ ********** }

procedure THtmlAutoWriter.ParseWriteTextPreformatted;
begin
 ParseTextPreformatted;
 WriteCurrentCData;
end;

{ **************************************************************************** }

procedure THtmlAutoWriter.InitHandleProcs;
var
 i                            : Integer;
begin
 for i := Low (TTags) to High (TTags) do
 begin
  FHandleTagsWriteFiltered[i] := SkipAttribs;
  FHandleTagsReportFilteredWriteFiltered[i] := SkipAttribs;
  FHandleTagsReportWriteFiltered[i] := ParseReportAttribs;

  FHandleTagsWrite[i] := ParseWriteAttribs;
  FHandleTagsReportFilteredWrite[i] := ParseWriteAttribs;
  FHandleTagsReportWrite[i] := ParseReportWriteAttribs;
 end;
 
 FHandleTagsWriteFiltered[tagUndefined] := SkipAttribs;
 FHandleTagsReportFilteredWriteFiltered[tagUndefined] := SkipAttribs;
 FHandleTagsReportWriteFiltered[tagUndefined] := SkipAttribs;
 
 FHandleTagsWrite[tagUndefined] := SkipAttribs;
 FHandleTagsReportFilteredWrite[tagUndefined] := SkipAttribs;
 FHandleTagsReportWrite[tagUndefined] := SkipAttribs;
 
 inherited InitHandleProcs;
end;

{ **************************************************************************** }

procedure THtmlAutoWriter.UpdateSingleStartTagProc (const Tag: TStartTags);
begin
 if FAutoWriteTagsFilterArray[Tag] then
  if FReportTagsFilterArray[Tag] then
  begin
   FHandleTagsWriteFiltered[Tag] := ParseWriteAttribs;
   FHandleTagsReportFilteredWriteFiltered[Tag] := ParseReportWriteAttribs;
   FHandleTagsReportWriteFiltered[Tag] := ParseReportWriteAttribs;
   FHandleTagsReportFilteredWrite[Tag] := ParseReportWriteAttribs;
   FHandleTagsReportFiltered[Tag] := ParseReportAttribs;
  end
  else
  begin
   FHandleTagsWriteFiltered[Tag] := ParseWriteAttribs;
   FHandleTagsReportFilteredWriteFiltered[Tag] := ParseWriteAttribs;
   FHandleTagsReportWriteFiltered[Tag] := ParseWriteAttribs;
   FHandleTagsReportFilteredWrite[Tag] := ParseWriteAttribs;
   FHandleTagsReportFiltered[Tag] := ParseAttribs;
  end
 else
  if FReportTagsFilterArray[Tag] then
  begin
   FHandleTagsWriteFiltered[Tag] := SkipAttribs;
   FHandleTagsReportFilteredWriteFiltered[Tag] := ParseReportAttribs;
   FHandleTagsReportWriteFiltered[Tag] := ParseReportAttribs;
   FHandleTagsReportFilteredWrite[Tag] := ParseReportAttribs;
   FHandleTagsReportFiltered[Tag] := ParseReportAttribs;
  end
  else
  begin
   FHandleTagsWriteFiltered[Tag] := SkipAttribs;
   FHandleTagsReportFilteredWriteFiltered[Tag] := SkipAttribs;
   FHandleTagsReportWriteFiltered[Tag] := ParseReportAttribs;
   FHandleTagsReportFilteredWrite[Tag] := ParseWriteAttribs;
   FHandleTagsReportFiltered[Tag] := SkipAttribs;
  end;
end;

{ ********** }

procedure THtmlAutoWriter.UpdateSingleEndTagProc (const Tag: TEndTags);
begin
 if FAutoWriteTagsFilterArray[Tag] then
  if FReportTagsFilterArray[Tag] then
  begin
   FHandleTagsWriteFiltered[Tag] := SkipWriteAttribs;
   FHandleTagsReportFilteredWriteFiltered[Tag] := SkipReportWriteAttribs;
   FHandleTagsReportWriteFiltered[Tag] := SkipReportWriteAttribs;
   FHandleTagsReportFilteredWrite[Tag] := SkipReportWriteAttribs;
   FHandleTagsReportFiltered[Tag] := SkipReportAttribs;
  end
  else
  begin
   FHandleTagsWriteFiltered[Tag] := SkipWriteAttribs;
   FHandleTagsReportFilteredWriteFiltered[Tag] := SkipWriteAttribs;
   FHandleTagsReportWriteFiltered[Tag] := SkipWriteAttribs;
   FHandleTagsReportFilteredWrite[Tag] := SkipWriteAttribs;
   FHandleTagsReportFiltered[Tag] := SkipAttribs;
  end
 else
  if FReportTagsFilterArray[Tag] then
  begin
   FHandleTagsWriteFiltered[Tag] := SkipAttribs;
   FHandleTagsReportFilteredWriteFiltered[Tag] := SkipReportAttribs;
   FHandleTagsReportWriteFiltered[Tag] := SkipReportAttribs;
   FHandleTagsReportFilteredWrite[Tag] := SkipReportAttribs;
   FHandleTagsReportFiltered[Tag] := SkipReportAttribs;
  end
  else
  begin
   FHandleTagsWriteFiltered[Tag] := SkipAttribs;
   FHandleTagsReportFilteredWriteFiltered[Tag] := SkipAttribs;
   FHandleTagsReportWriteFiltered[Tag] := SkipReportAttribs;
   FHandleTagsReportFilteredWrite[Tag] := SkipWriteAttribs;
   FHandleTagsReportFiltered[Tag] := SkipAttribs;
  end;
end;

{ **************************************************************************** }
{ THtmlAutoWriter: Setting the Handlers. }
{ **************************************************************************** }

procedure THtmlAutoWriter.SetHandleASPs;
begin
 if FAutoWriteASPs then
  if FReportASPs then
   FHandleASP := ParseReportWriteASP
  else
   FHandleASP := ParseWriteASP
 else
  inherited SetHandleASPs;
end;

{ ********** }

procedure THtmlAutoWriter.SetHandleComments;
begin
 if FAutoWriteComments then
  if FReportComments then
   FHandleComment := ParseReportWriteComment
  else
   FHandleComment := ParseWriteComment
 else
  inherited SetHandleComments;
end;

{ ********** }

procedure THtmlAutoWriter.SetHandleDTDs;
begin
 if FAutoWriteDTDs then
  if FReportDTDs then
   FHandleDTD := ParseReportWriteDTD
  else
   FHandleDTD := ParseWriteDTD
 else
  inherited SetHandleDTDs;
end;

{ ********** }

procedure THtmlAutoWriter.SetAllTagHandlers (const Tag: TTags; const Proc: TProcEvent);
begin
 FHandleTagsWriteFiltered[Tag] := Proc;
 FHandleTagsReportFilteredWriteFiltered[Tag] := Proc;
 FHandleTagsReportWriteFiltered[Tag] := Proc;
 
 FHandleTagsWrite[Tag] := Proc;
 FHandleTagsReportFilteredWrite[Tag] := Proc;
 FHandleTagsReportWrite[Tag] := Proc;
 
 inherited SetAllTagHandlers (Tag, Proc);
end;

{ ********** }

procedure THtmlAutoWriter.SetHandleScripts;
begin
 if FAutoWriteScripts then
  if FReportScripts then
   SetAllTagHandlers (tagSCRIPT, ParseReportWriteScript)
  else
   SetAllTagHandlers (tagSCRIPT, ParseWriteScript)
 else
  inherited SetHandleScripts;
end;

{ ********** }

procedure THtmlAutoWriter.SetHandleSSIs;
begin
 if FAutoWriteSSIs then
  if FReportSSIs then
   FHandleSSI := ParseReportWriteSSI
  else
   FHandleSSI := ParseWriteSSI
 else
  inherited SetHandleSSIs;
end;

{ ********** }

procedure THtmlAutoWriter.SetHandleStyles;
begin
 if FAutoWriteStyles then
  if FReportStyles then
   SetAllTagHandlers (tagSTYLE, ParseReportWriteStyle)
  else
   SetAllTagHandlers (tagSTYLE, ParseWriteStyle)
 else
  inherited SetHandleStyles;
end;

{ ********** }

procedure THtmlAutoWriter.SetHandleTags;
begin
 if FAutoWriteTags then
  if FAutoWriteTagsFiltered then
   if FReportTags then
    if FReportTagsFiltered then
     FHandleTags := @FHandleTagsReportFilteredWriteFiltered
    else
     FHandleTags := @FHandleTagsReportWriteFiltered
   else
    FHandleTags := @FHandleTagsWriteFiltered
  else
   if FReportTags then
    if FReportTagsFiltered then
     FHandleTags := @FHandleTagsReportFilteredWrite
    else
     FHandleTags := @FHandleTagsReportWrite
   else
    FHandleTags := @FHandleTagsWrite
  else
   inherited SetHandleTags;
end;

{ ********** }

procedure THtmlAutoWriter.SetHandleText;
begin
 if FAutoWriteText then
 begin
  if FReportText then
  begin
   FHandleTextNormal := ParseReportWriteTextNormal;
   FHandleTextPreformatted := ParseReportWriteTextPreformatted;
  end
  else
  begin
   FHandleTextNormal := ParseWriteTextNormal;
   FHandleTextPreformatted := ParseWriteTextPreformatted;
  end;
  UpdateHandleText;
 end
 else
  inherited SetHandleText;
end;

{ **************************************************************************** }
{ General Functions & Procedures }
{ **************************************************************************** }

function CompareTA (const p1, p2: PChar; const l: Integer): Boolean;
{ Compares a Tag or an Attrib (with p1 case sensitivity). }
var
 Run                          : Integer;
begin
 Assert (Assigned (p1) and Assigned (p2), 'Cannot pass nil pointer to CompareTA function.');
 Run := 0;
 while (Run < l) and ((p1[Run] = p2[Run]) or (AsciiUpperChars[p1[Run]] = p2[Run])) do
  if (Run + 1 < l) and ((p1[Run + 1] = p2[Run + 1]) or (AsciiUpperChars[p1[Run + 1]] = p2[Run + 1])) then
   if (Run + 2 < l) and ((p1[Run + 2] = p2[Run + 2]) or (AsciiUpperChars[p1[Run + 2]] = p2[Run + 2])) then
    if (Run + 3 < l) and ((p1[Run + 3] = p2[Run + 3]) or (AsciiUpperChars[p1[Run + 3]] = p2[Run + 3])) then
     Inc (Run, 4)
    else
    begin
     Inc (Run, 3);
     Break;
    end
   else
   begin
    Inc (Run, 2);
    Break;
   end
  else
  begin
   Inc (Run, 1);
   Break;
  end;
 Result := Run = l;
end;

{ **************************************************************************** }

function CompareE (const p1, p2: PChar; const l: Integer): Boolean;
{ Compares an Entity (not case sensitive). }
var
 Run                          : Integer;
begin
 Assert (Assigned (p1) and Assigned (p2), 'Cannot pass nil pointer to CompareE function.');
 Run := 0;
 while (Run < l) and (p1[Run] = p2[Run]) do
  if (Run + 1 < l) and (p1[Run + 1] = p2[Run + 1]) then
   if (Run + 2 < l) and (p1[Run + 2] = p2[Run + 2]) then
    if (Run + 3 < l) and (p1[Run + 3] = p2[Run + 3]) then
     Inc (Run, 4)
    else
    begin
     Inc (Run, 3);
     Break;
    end
   else
   begin
    Inc (Run, 2);
    Break;
   end
  else
  begin
   Inc (Run, 1);
   Break;
  end;
 Result := Run = l;
end;

{ **************************************************************************** }
{ Registering the Components
{ **************************************************************************** }

{$IFDEF Component}

procedure Register;
begin
 RegisterComponents ('Standard', [THtmlReporter, THtmlWriter, THtmlAutoWriter]);
end;
{$ENDIF}

end.

