{$IFDEF IHTML4}
{$A+,B-,C-,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O-,P+,Q-,R+,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$DEBUGINFO ON}
{$ELSE}
{$A+,B-,C-,D-,E-,F-,G+,H+,I+,J+,K-,L-,M-,N+,O+,P+,Q+,R+,S-,T-,U-,V+,W-,X+,Y-,Z4}
{$ENDIF}

unit IHTML4;

{
   Interface produtora de HTML.
   Define um interface( IHTMLProducer ), que produz texto no formato HTML 4.0
   Define tb varias outras classes que implementam esta interface
   para realizar funcoes basicas de geracao de documentos HTML.
   Sua saida eh compativel com a especificacao 4.0

   Baseado na versao 1.0 de 22 de julho de 1998 escrita por Keith Wood.
}


// Event              Occurs when...                                                  Event Handler
// click              User clicks on form element or link                             onClick
// change             User changes value of text, textarea, or select element         onChange
// focus              User gives form element input focus                             onFocus
// blur               User removes input focus from form element                      onBlur
// mouseover          User moves mouse pointer over a link or anchor                  onMouseOver
// mouseout           User moves mouse pointer off of link or anchor                  onMouseOut
// select             User selects form element's input field                         onSelect
// submit             User submits a form                                             onSubmit
// resize             User resizes the browser window                                 onResize
// load               User loads the page in the Navigator                            onLoad
// unload             User exits the page                                             onUnload


//!!! Isto possibilita uma saida melhor formatada com maior consumo de CPU
{$DEFINE WELL_FORMATED_HTML }

interface

uses
  Windows, Classes, SysUtils, Graphics;

resourcestring
  sDeprecatedTag = 'Deprecated tag %s';

const
  Version = '1.0';
  iRow = 1;
  iCol = 2;

type
   //The HTML producing interface - a single function that returns HTML formatted text
  IHTMLProducer = interface(IUnknown)
    ['{1265C6A2-5791-11D2-A65A-0000C08699E7}']
    function AsHTML: string; stdcall;
  end;

  { Tag types }
  THTMLTag = (tgCustom, tgLink, tgImage, tgTable, tgImageMap, tgObject, tgEmbed);

  { Method signature for handling HTML replacement }
  THTMLTagEvent = procedure (Sender: TObject; Tag: THTMLTag; const TagString: string;
    TagParams: TStrings; var ReplaceText: string) of object;

  { HTML %Pixels definition }
  THTMLPixels = Word;
  { HTML Number definition }
  THTMLNumber = Word;
  { HTML %Length definition }
  THTMLLength = Integer;

  { Direction for text and tables }
  THTMLDirection = (diDefault, diLeftToRight, diRightToLeft);
  { Horizontal alignments in HTML }
  THTMLAlignmentHoriz = (ahDefault, ahLeft, ahCentre, ahRight);
  { Vertical alignments in HTML }
  THTMLAlignmentVert = (avDefault, avTop, avMiddle, avBottom);
  { Image alignments in HTML }
  THTMLAlignmentImage = (aiDefault, aiTop, aiMiddle, aiBottom, aiLeft, aiRight);
  { Caption and legend alignments in HTML }
  THTMLAlignmentCaption = (acDefault, acTop, acBottom, acLeft, acRight);
  { Frame scrolling options }
  THTMLFrameScroll = (fsAuto, fsYes, fsNo);
  { Headings levels in HTML }
  THTMLHeadingLevel = 1..6;
  { Image map shapes in HTML }
  THTMLShape = (shDefault, shRect, shCircle, shPolygon);
  { Object parameter types in HTML }
  THTMLParamTypes = (ptData, ptRef, ptObject);
  { List types in HTML }
  THTMLListType = (ltUnordered, ltOrdered, ltGlossary);
  { Bullet types for lists in HTML }
  THTMLListBullet = (lbDefault, lbDisc, lbCircle, lbSquare);
  { List item types in HTML }
  THTMLListItemType = (liNormal, liTerm, liDefinition);
  { List numbering schemes in HTML }
  THTMLNumberScheme = (nsDefault, nsLargeLetters, nsSmallLetters,
    nsLargeRoman, nsSmallRoman, nsNumbers);
  { Clear margins options in HTML }
  THTMLClearTo = (ctDefault, ctLeft, ctRight, ctAll);
  { HTML text styles }
  THTMLTextStyle = (tsNormal, tsBold, tsItalic, tsFixedPitch, tsBig, tsSmall, tsAddress, tsPreformat, tsEmphasised,
                    tsStrong, tsDefinition, tsCode, tsSample, tsKeyboard, tsVariable, tsCitation, tsAbbreviation,
                    tsAcronym, tsBlockQuote, tsQuote, tsSuperscript, tsSubscript, tsInserted, tsDeleted,
                    { The rest are deprecated in HTML 4 }
                    tsUnderlined, tsStrike, tsBlink, tsNonBreaking, tsCentre);
  { HTML table framing options }
  THTMLTableFrame = (tfVoid, tfAbove, tfBelow, tfHSides, tfVSides, tfLeft, tfRight, tfBox, tfBorder);
  { HTML table rules options }
  THTMLTableRule = (trNone, trGroups, trRows, trCols, trAll);
  { Horizontal alignments in HTML tables }
  THTMLTableAlignHoriz = (thDefault, thLeft, thCentre, thRight, thJustify, thChar);
  { Vertical alignments in HTML tables }
  THTMLTableAlignVert = (tvDefault, tvTop, tvMiddle, tvBottom, tvBaseline);
  { Table row group types in HTML }
  THTMLTableRowGrouping = (rgHead, rgBody, rgFoot);
  { Form protocols in HTML }
  THTMLFormMethod = (fmGet, fmPost);
  { Form field types in HTML }
  THTMLFieldType = (ftText, ftPassword, ftCheckbox, ftRadio, ftSubmit, ftReset, ftButton, ftImage, ftHidden, ftFile);
  { Form button types }
  THTMLButtonType = ftSubmit..ftButton;

  //Classe base para implementacao de IHTMLProducer interface
  THTMLBase = class(TPersistent, IHTMLProducer) //Alteracao para TPersistent deve ser "profilada", perdendo voltar para TObject
  private
    FStyle: string;
    FId: string;
    FTagClass: string;
    FLanguage: string;
    FDirection: THTMLDirection;
    FTitle: string;
    FAccessKey: Char;
    FTabIndex: THTMLNumber;
    FOtherAttributes: string;
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    property Style: string read FStyle write FStyle;
    property Id: string read FId write FId;
    property TagClass: string read FTagClass write FTagClass;
    property Language: string read FLanguage write FLanguage;
    property Direction: THTMLDirection read FDirection write FDirection;
    property Title: string read FTitle write FTitle;
    property AccessKey: Char read FAccessKey write FAccessKey;
    property TabIndex: THTMLNumber read FTabIndex write FTabIndex;
    property OtherAttributes: string read FOtherAttributes write FOtherAttributes;
    function BaseAttributesAsHTML: string;
  public
    { TODO 5 -oRoger -cDSG : Verificar o que se pode se pode fazer com a distincao de TagHeader e Document_content }
    function AsHTML: string; virtual; stdcall;
  end;

  { Base container class for HTML. Any number of HTML producers can be added to this container and each will generate its HTML in turn. }
  THTMLContainer = class(THTMLBase)
  private
    lstHTML: TList;
    FOwnContents: Boolean;
    function GetCount: Word;
    function GetItems(Index: Integer): TObject;
  public
    constructor Create;
    destructor Destroy; override;
    function AsHTML: string; override; stdcall;
    function ContentsAsHTML: string;
    procedure Add(objHTML: TObject);
    procedure Insert( Index : integer; objHTML: TObject);
    procedure Clear; virtual;
    property Count: Word read GetCount;
    property Items[Index: Integer]: TObject read GetItems; default;
    property OwnContents: Boolean read FOwnContents write FOwnContents;
  end;

  { An HTML comment }
  THTMLComment = class(THTMLBase)
  private
    FComment: string;
  public
    constructor Create(sComment: string); virtual;
    function AsHTML: string; override; stdcall;
    property Comment: string read FComment write FComment;
  end;

  THTMLStyleSheet = class;
  THTMLFrameSet = class;

  { An HTML document }
  THTMLDocument = class(THTMLContainer)
  private
    FHeaderLanguage: string;
    FHeaderDirection: THTMLDirection;
    FTitle: string;
    FBase: string;
    FHeaderTags: THTMLContainer;
    FStyleSheet: THTMLStyleSheet;
    FFrameSet: THTMLFrameSet;
    FBackgroundImage: string;
    FBackgroundColour: TColor;
    FTextColour: TColor;
    FLinkColour: TColor;
    FVisitedColour: TColor;
    FActiveColour: TColor;
    function GetOwnContents: Boolean;
    procedure SetOwnContents(bOwnContents: Boolean);
  public
    constructor Create( sTitle: string; sBase: string = ''; sHeaderLanguage: string = ''; diHeaderDirection: THTMLDirection = diDefault); virtual;
    destructor Destroy; override;
    function AsHTML: string; override; stdcall;
    property HeaderLanguage: string read FHeaderLanguage write FHeaderLanguage;
    property HeaderDirection: THTMLDirection read FHeaderDirection write FHeaderDirection;
    property Title: string read FTitle write FTitle;
    property Base: string read FBase write FBase;
    property StyleSheet: THTMLStyleSheet read FStyleSheet write FStyleSheet;
    property FrameSet: THTMLFrameSet read FFrameSet write FFrameSet;
    property BackgroundImage: string read FBackgroundImage write FBackgroundImage;
    property BackgroundColour: TColor read FBackgroundColour write FBackgroundColour;
    property TextColour: TColor read FTextColour write FTextColour;
    property LinkColour: TColor read FLinkColour write FLinkColour;
    property VisitedColour: TColor read FVisitedColour write FVisitedColour;
    property ActiveColour: TColor read FActiveColour write FActiveColour;
    property OwnContents: Boolean read GetOwnContents write SetOwnContents;
    property Style;
    property Id;
    property TagClass;
    property Language;
    property Direction;
    property OtherAttributes;
    procedure AddHeaderTag(objHTML: TObject);
    procedure ClearHeaderTags;
  end;

  { HTML meta data tags }
  THTMLMetadata = class(THTMLBase)
  private
    FName: string;
    FHTTPEquivalent: string;
    FContent: string;
  public
    constructor Create(sName, sContent: string; sHTTPEquivalent: string = ''); virtual;
    function AsHTML: string; override; stdcall;
    property Name: string read FName write FName;
    property HTTPEquivalent: string read FHTTPEquivalent write FHTTPEquivalent;
    property Content: string read FContent write FContent;
    property Language;
    property Direction;
    property OtherAttributes;
  end;

  { HTML relationship links }
  THTMLLink = class(THTMLBase)
  private
    FDestination: string;
    FTarget: string;
    FContentType: string;
    FRelType: string;
    FRevType: string;
  public
    constructor Create(sRelType, sDestination: string; sTarget: string = ''; sContentType: string = ''); virtual;
    function AsHTML: string; override; stdcall;
    property Destination: string read FDestination write FDestination;
    property Target: string read FTarget write FTarget;
    property ContentType: string read FContentType write FContentType;
    property RelType: string read FRelType write FRelType;
    property RevType: string read FRevType write FRevType;
    property Style;
    property Id;
    property TagClass;
    property Language;
    property Direction;
    property Title;
    property OtherAttributes;
  end;

  { Style sheet for HTML document }
  THTMLStyleSheet = class(THTMLBase)
  private
    FStyleType: string;
    FMedia: string;
    FCommentOut: Boolean;
    slsStyles: TStringList;
  public
    constructor Create(sStyleType: string = 'text/css'); virtual;
    destructor Destroy; override;
    function AsHTML: string; override; stdcall;
    property StyleType: string read FStyleType write FStyleType;
    property Media: string read FMedia write FMedia;
    property CommentOut: Boolean read FCommentOut write FCommentOut;
    property Language;
    property Direction;
    property Title;
    property OtherAttributes;
    procedure AddStyle(sElement, sStyle: string);
    procedure ClearStyles;
  end;

  { Frame layout for HTML }
  THTMLFrameSet = class(THTMLContainer)
  private
    FRows: TList;
    FCols: TList;
    FNoFrames: THTMLContainer;
    function GetAsText(RowOrCol: Integer): string;
    function GetCount(RowOrCol: Integer): Integer;
    function GetSize(Index, RowOrCol: Integer): THTMLLength;
    function GetOwnContents: Boolean;
    procedure SetSize(Index, RowOrCol: Integer; iSize: THTMLLength);
    procedure SetSizes(RowOrCol: Integer; iSizes: array of THTMLLength);
    procedure SetOwnContents(bOwnContents: Boolean);
  public
    constructor Create(iRows, iCols: array of THTMLLength; sTagClass: string = ''; sId: string = ''; sStyle: string = ''); virtual;
    destructor Destroy; override;
    function AsHTML: string; override; stdcall;
    property RowsAsText: string index iRow read GetAsText;
    property ColsAsText: string index iCol read GetAsText;
    property RowCount: Integer index iRow read GetCount;
    property ColCount: Integer index iCol read GetCount;
    property Rows[Index: Integer]: THTMLLength index iRow read GetSize write SetSize;
    property Cols[Index: Integer]: THTMLLength index iCol read GetSize write SetSize;
    property OwnContents: Boolean read GetOwnContents write SetOwnContents;
    property Style;
    property Id;
    property TagClass;
    property Title;
    property OtherAttributes;
    procedure SetRows(iRows: array of THTMLLength);
    procedure SetCols(iCols: array of THTMLLength);
    procedure AddNoFrames(objHTML: TObject);
    procedure ClearNoFrames;
  end;

  { Single frame in HTML }
  THTMLFrame = class(THTMLBase)
  private
    FName: string;
    FSource: string;
    FTarget: string;
    FFrameBorder: Boolean;
    FMarginWidth: THTMLPixels;
    FMarginHeight: THTMLPixels;
    FNoResize: Boolean;
    FScrolling: THTMLFrameScroll;
  public
    constructor Create(sSource: string; sName: string = ''; sTarget: string = ''); virtual;
    function AsHTML: string; override; stdcall;
    property Name: string read FName write FName;
    property Source: string read FSource write FSource;
    property Target: string read FTarget write FTarget;
    property FrameBorder: Boolean read FFrameBorder write FFrameBorder;
    property MarginWidth: THTMLPixels read FMarginWidth write FMarginWidth;
    property MarginHeight: THTMLPixels read FMarginHeight write FMarginHeight;
    property NoResize: Boolean read FNoResize write FNoResize;
    property Scrolling: THTMLFrameScroll read FScrolling write FScrolling;
    property Style;
    property Id;
    property TagClass;
    property Title;
    property OtherAttributes;
  end;

  { Inline frame layout for HTML }
  THTMLInlineFrame = class(THTMLBase)
  private
    FName: string;
    FSource: string;
    FTarget: string;
    FFrameBorder: Boolean;
    FMarginWidth: THTMLPixels;
    FMarginHeight: THTMLPixels;
    FScrolling: THTMLFrameScroll;
    FAlignment: THTMLAlignmentCaption;
    FHeight: THTMLLength;
    FWidth: THTMLLength;
    FNoFrames: THTMLContainer;
    function GetOwnContents: Boolean;
    procedure SetOwnContents(bOwnContents: Boolean);
  public
    constructor Create(sSource: string; sName: string = ''; sTarget: string = ''); virtual;
    destructor Destroy; override;
    function AsHTML: string; override; stdcall;
    property Name: string read FName write FName;
    property Source: string read FSource write FSource;
    property Target: string read FTarget write FTarget;
    property FrameBorder: Boolean read FFrameBorder write FFrameBorder;
    property MarginWidth: THTMLPixels read FMarginWidth write FMarginWidth;
    property MarginHeight: THTMLPixels read FMarginHeight write FMarginHeight;
    property Scrolling: THTMLFrameScroll read FScrolling write FScrolling;
    property Alignment: THTMLAlignmentCaption read FAlignment write FAlignment;
    property Height: THTMLLength read FHeight write FHeight;
    property Width: THTMLLength read FWidth write FWidth;
    property OwnContents: Boolean read GetOwnContents write SetOwnContents;
    property Style;
    property Id;
    property TagClass;
    property Title;
    property OtherAttributes;
    procedure AddNoFrames(objHTML: TObject);
    procedure ClearNoFrames;
  end;

  { Executable code within an HTML document }
  THTMLScript = class(THTMLBase)
  private
    FScriptType: string;
    FSource: string;
    FDefer: Boolean;
    FHideInComment: Boolean;
    FCommentSequence: string;
    FCode: string;
    procedure SetScriptType(sScriptType: string);
  public
    constructor Create(sScriptType, sCode: string); virtual;
    function AsHTML: string; override; stdcall;
    property ScriptType: string read FScriptType write SetScriptType;
    property Source: string read FSource write FSource;
    property Defer: Boolean read FDefer write FDefer;
    property HideInComment: Boolean read FHideInComment write FHideInComment;
    property CommentSequence: string read FCommentSequence write FCommentSequence;
    property Code: string read FCode write FCode;
    property OtherAttributes;
  end;

  { Replacement for executable code }
  THTMLNoScript = class(THTMLContainer)
  public
    function AsHTML: string; override; stdcall;
    property Style;
    property Id;
    property TagClass;
    property Language;
    property Direction;
    property Title;
    property OtherAttributes;
  end;

  { An HTML division (or span) }
  THTMLDivision = class(THTMLContainer)
  private
    FBlockLevel: Boolean;
    FAlignment: THTMLAlignmentHoriz;
    FText: string;
  public
    constructor Create(bBlockLevel: Boolean; sStyle, sText: string; sTagClass: string = ''; sId: string = ''); virtual;
    function AsHTML: string; override; stdcall;
    property BlockLevel: Boolean read FBlockLevel write FBlockLevel;
    property Alignment: THTMLAlignmentHoriz read FAlignment write FAlignment;
    property Text: string read FText write FText;
    property Style;
    property Id;
    property TagClass;
    property Language;
    property Direction;
    property Title;
    property OtherAttributes;
  end;

  { An HTML paragraph }
  THTMLParagraph = class(THTMLContainer)
  private
    FText: string;
    FAlignment: THTMLAlignmentHoriz;
  public
    constructor Create(sText: string; sTagClass: string = ''; sId: string = ''; sStyle: string = ''); virtual;
    function AsHTML: string; override; stdcall;
    property Text: string read FText write FText;
    property Alignment: THTMLAlignmentHoriz read FAlignment write FAlignment;
    property Style;
    property Id;
    property TagClass;
    property Language;
    property Direction;
    property Title;
    property OtherAttributes;
  end;

  { An HTML heading }
  THTMLHeading = class(THTMLContainer)
  private
    FLevel: THTMLHeadingLevel;
    FText: string;
    FAlignment: THTMLAlignmentHoriz;
  public
    constructor Create(iLevel: THTMLHeadingLevel; sText: string; sTagClass: string = ''; sId: string = ''; sStyle: string = ''); virtual;
    function AsHTML: string; override; stdcall;
    property Level: THTMLHeadingLevel read FLevel write FLevel;
    property Text: string read FText write FText;
    property Alignment: THTMLAlignmentHoriz read FAlignment write FAlignment;
    property Style;
    property Id;
    property TagClass;
    property Language;
    property Direction;
    property Title;
    property OtherAttributes;
  end;

  { Formatted text - styles }
  THTMLText = class(THTMLContainer)
  private
    FTextStyle: THTMLTextStyle;
    FText: string;
  public
    constructor Create(tsStyle: THTMLTextStyle; sText: string; sTagClass: string = ''; sId: string = ''; sStyle: string = ''); overload; virtual;
    constructor Create(sText: string; sTagClass: string = ''; sId: string = ''; sStyle: string = ''); overload; virtual;
    function AsHTML: string; override; stdcall;
    property TextStyle: THTMLTextStyle read FTextStyle write FTextStyle;
    property Text: string read FText write FText;
    property Style;
    property Id;
    property TagClass;
    property Language;
    property Direction;
    property Title;
    property OtherAttributes;
  end;

  { A link to an image }
  THTMLImage = class(THTMLBase)
  private
    FSource: string;
    FHeight: THTMLLength;
    FWidth: THTMLLength;
    FAlignment: THTMLAlignmentImage;
    FHorizSpace: THTMLPixels;
    FVertSpace: THTMLPixels;
    FBorderWidth: THTMLPixels;
    FAlternateText: string;
    FUseMap: string;
    FIsMap: Boolean;
  public
    constructor Create(sSource, sAlternateText: string; iHeight: THTMLLength = 0; iWidth: THTMLLength = 0; aiAlignment: THTMLAlignmentImage = aiDefault); virtual;
    function AsHTML: string; override; stdcall;
    property Source: string read FSource write FSource;
    property Height: THTMLLength read FHeight write FHeight;
    property Width: THTMLLength read FWidth write FWidth;
    property Alignment: THTMLAlignmentImage read FAlignment write FAlignment;
    property HorizSpace: THTMLPixels read FHorizSpace write FHorizSpace;
    property VertSpace: THTMLPixels read FVertSpace write FVertSpace;
    property BorderWidth: THTMLPixels read FBorderWidth write FBorderWidth;
    property AlternateText: string read FAlternateText write FAlternateText;
    property UseMap: string read FUseMap write FUseMap;
    property IsMap: Boolean read FIsMap write FIsMap;
    property Style;
    property Id;
    property TagClass;
    property Language;
    property Direction;
    property Title;
    property OtherAttributes;
  end;

  { An HTML image map }
  THTMLImageMap = class(THTMLContainer)
  private
    FName: string;
  public
    constructor Create(sName: string); virtual;
    function AsHTML: string; override; stdcall;
    property Name: string read FName write FName;
    property OtherAttributes;
  end;

  { An HTML image map area }
  THTMLImageMapArea = class(THTMLBase)
  private
    FShape: THTMLShape;
    FDestination: string;
    FTarget: string;
    FAlternateText: string;
    lstCoords: TList;
  public
    constructor Create(shShape: THTMLShape; iCoords: array of THTMLPixels; sDestination, sAlternateText: string; sTarget: string = ''); virtual;
    destructor Destroy; override;
    function AsHTML: string; override; stdcall;
    property Shape: THTMLShape read FShape;
    property Destination: string read FDestination write FDestination;
    property Target: string read FTarget write FTarget;
    property AlternateText: string read FAlternateText write FAlternateText;
    property Style;
    property Id;
    property TagClass;
    property Language;
    property Direction;
    property Title;
    property AccessKey;
    property TabIndex;
    property OtherAttributes;
    procedure SetShape(shShape: THTMLShape; iCoords: array of THTMLPixels);
  end;

  { A link to a generic object }
  THTMLObject = class(THTMLContainer)
  private
    FClassId: string;
    FCodeBase: string;
    FCodeType: string;
    FData: string;
    FDataType: string;
    FName: string;
    FHeight: THTMLLength;
    FWidth: THTMLLength;
    FAlignment: THTMLAlignmentHoriz;
    FHorizSpace: THTMLPixels;
    FVertSpace: THTMLPixels;
    FBorderWidth: THTMLPixels;
    FStandBy: string;
    FUseMap: string;
  public
    constructor Create(sClassId: string; sData: string = ''; sName: string = ''; ahAlignment: THTMLAlignmentHoriz = ahDefault; iHeight: THTMLLength = 0; iWidth: THTMLLength = 0); virtual;
    function AsHTML: string; override; stdcall;
    property ClassId: string read FClassId write FClassId;
    property CodeBase: string read FCodeBase write FCodeBase;
    property CodeType: string read FCodeType write FCodeType;
    property Data: string read FData write FData;
    property DataType: string read FDataType write FDataType;
    property Name: string read FName write FName;
    property Height: THTMLLength read FHeight write FHeight;
    property Width: THTMLLength read FWidth write FWidth;
    property Alignment: THTMLAlignmentHoriz read FAlignment write FAlignment;
    property HorizSpace: THTMLPixels read FHorizSpace write FHorizSpace;
    property VertSpace: THTMLPixels read FVertSpace write FVertSpace;
    property BorderWidth: THTMLPixels read FBorderWidth write FBorderWidth;
    property StandBy: string read FStandBy write FStandBy;
    property UseMap: string read FUseMap write FUseMap;
    property Style;
    property Id;
    property TagClass;
    property Language;
    property Direction;
    property Title;
    property TabIndex;
    property OtherAttributes;
  end;

  { Parameters for generic objects }
  THTMLObjectParam = class(THTMLBase)
  private
    FName: string;
    FValue: string;
    FValueType: THTMLParamTypes;
    FContentType: string;
  public
    constructor Create(sName, sValue: string; iValueType: THTMLParamTypes = ptData; sContentType: string = ''); virtual;
    function AsHTML: string; override; stdcall;
    property Name: string read FName write FName;
    property Value: string read FValue write FValue;
    property ValueType: THTMLParamTypes read FValueType write FValueType;
    property ContentType: string read FContentType write FContentType;
    property Id;
    property OtherAttributes;
  end;

  { An HTML list }
  THTMLList = class(THTMLContainer)
  private
    FListType: THTMLListType;
    FBulletType: THTMLListBullet;
    FNumberScheme: THTMLNumberScheme;
    FStart: THTMLNumber;
  public
    constructor Create(ltListType: THTMLListType; sTagClass: string = ''; sId: string = ''; sStyle: string = ''); virtual;
    constructor CreateUnordered(lbBulletType: THTMLListBullet = lbDefault; sTagClass: string = ''; sId: string = ''; sStyle: string = '');
    constructor CreateOrdered( nsNumberScheme: THTMLNumberScheme = nsDefault; iStart: THTMLNumber = 0; sTagClass: string = ''; sId: string = ''; sStyle: string = '');
    function AsHTML: string; override; stdcall;
    property ListType: THTMLListType read FListType write FListType;
    property BulletType: THTMLListBullet read FBulletType write FBulletType;
    property NumberScheme: THTMLNumberScheme read FNumberScheme write FNumberScheme;
    property Start: THTMLNumber read FStart write FStart;
    property Style;
    property Id;
    property TagClass;
    property Language;
    property Direction;
    property Title;
    property OtherAttributes;
  end;

  { An HTML list item }
  THTMLListItem = class(THTMLContainer)
  private
    FListItem: THTMLListItemType;
    FText: string;
    FNumberScheme: THTMLNumberScheme;
    FValue: THTMLNumber;
  public
    constructor Create(liListItem: THTMLListItemType; sText: string; sTagClass: string = ''; sId: string = ''; sStyle: string = ''); virtual;
    function AsHTML: string; override; stdcall;
    property ListItem: THTMLListItemType read FListItem write FListItem;
    property Text: string read FText write FText;
    property NumberScheme: THTMLNumberScheme read FNumberScheme write FNumberScheme;
    property Value: THTMLNumber read FValue write FValue;
    property Style;
    property Id;
    property TagClass;
    property Language;
    property Direction;
    property Title;
    property OtherAttributes;
  end;

  { A hypertext link }
  THTMLAnchor = class(THTMLContainer)
  private
    FDestination: string;
    FName: string;
    FTarget: string;
    FContentType: string;
    FText: string;
  public
    constructor Create(sDestination, sText: string; sTarget: string = ''; sName: string = ''); virtual;
    function AsHTML: string; override; stdcall;
    property Destination: string read FDestination write FDestination;
    property Name: string read FName write FName;
    property Target: string read FTarget write FTarget;
    property ContentType: string read FContentType write FContentType;
    property Text: string read FText write FText;
    property Style;
    property Id;
    property TagClass;
    property Language;
    property Direction;
    property Title;
    property AccessKey;
    property TabIndex;
    property OtherAttributes;
  end;

  { An HTML horizontal rule }
  THTMLHorizRule = class(THTMLBase)
  private
    FSize: THTMLPixels;
    FWidth: THTMLLength;
    FAlignment: THTMLAlignmentHoriz;
    FNoShade: Boolean;
  public
    constructor Create(iWidth: THTMLLength = 0; ahAlignment: THTMLAlignmentHoriz = ahDefault; iSize: THTMLPixels = 0); virtual;
    function AsHTML: string; override; stdcall;
    property Size: THTMLPixels read FSize write FSize;
    property Width: THTMLLength read FWidth write FWidth;
    property Alignment: THTMLAlignmentHoriz read FAlignment write FAlignment;
    property NoShade: Boolean read FNoShade write FNoShade;
    property Style;
    property Id;
    property TagClass;
    property Language;
    property Direction;
    property Title;
    property OtherAttributes;
  end;

  { An HTML line break }
  THTMLLineBreak = class(THTMLBase)
  private
    FClearTo: THTMLClearTo;
  public
    constructor Create(ctClearTo: THTMLClearTo = ctDefault); virtual;
    function AsHTML: string; override; stdcall;
    property ClearTo: THTMLClearTo read FClearTo write FClearTo;
    property Style;
    property Id;
    property TagClass;
    property Title;
    property OtherAttributes;
  end;

  { Base for HTML table tags }
  THTMLTableBase = class(THTMLContainer)
  private
    FAlignHoriz: THTMLTableAlignHoriz;
    FAlignVert: THTMLTableAlignVert;
    FAlignToChar: Char;
    FBackground: TColor;
    FBorderColour: TColor;
    FBorderHighlight: TColor;
    FBorderShadow: TColor;
  protected
    property AlignHoriz: THTMLTableAlignHoriz read FAlignHoriz write FAlignHoriz;
    property AlignVert: THTMLTableAlignVert read FAlignVert write FAlignVert;
    property AlignToChar: Char read FAlignToChar write FAlignToChar;
    property Background: TColor read FBackground write FBackground default clNone;
    property BorderColour: TColor read FBorderColour write FBorderColour default clNone;
    property BorderHighlight: TColor read FBorderHighlight write FBorderHighlight default clNone;
    property BorderShadow: TColor read FBorderShadow write FBorderShadow default clNone;
    function AlignmentsAsHTML: string;
    function ColoursAsHTML: string;
  public
    constructor Create;
    property OtherAttributes;
  end;

  { An HTML table }
  THTMLTable = class(THTMLTableBase)
  private
    FCaption: string;
    FCaptionAlignment: THTMLAlignmentCaption;
    FSummary: string;
    FFrame: THTMLTableFrame;
    FRules: THTMLTableRule;
    FBorderWidth: THTMLPixels;
    FWidth: THTMLLength;
    FCellSpacing: THTMLPixels;
    FCellPadding: THTMLPixels;
  public
    constructor Create(iWidth: THTMLLength = -100; { 100% } iBorderWidth: THTMLPixels = 2; sCaption: string = ''; acCaptionAlignment: THTMLAlignmentCaption = acDefault); virtual;
    function AsHTML: string; override; stdcall;
    property Caption: string read FCaption write FCaption;
    property CaptionAlignment: THTMLAlignmentCaption read FCaptionAlignment write FCaptionAlignment;
    property Summary: string read FSummary write FSummary;
    property Frame: THTMLTableFrame read FFrame write FFrame;
    property Rules: THTMLTableRule read FRules write FRules;
    property BorderWidth: THTMLPixels read FBorderWidth write FBorderWidth;
    property Width: THTMLLength read FWidth write FWidth;
    property CellSpacing: THTMLPixels read FCellSpacing write FCellSpacing;
    property CellPadding: THTMLPixels read FCellPadding write FCellPadding;
    property Background;
    property BorderColour;
    property BorderHighlight;
    property BorderShadow;
    property Style;
    property Id;
    property TagClass;
    property Language;
    property Direction;
    property Title;
    property OtherAttributes;
  end;

  { An HTML table row group }
  THTMLTableRowGroup = class(THTMLTableBase)
  private
    FRowGrouping: THTMLTableRowGrouping;
  public
    constructor Create(rgRowGrouping: THTMLTableRowGrouping); virtual;
    function AsHTML: string; override; stdcall;
    property RowGrouping: THTMLTableRowGrouping read FRowGrouping write FRowGrouping;
    property AlignHoriz;
    property AlignVert;
    property AlignToChar;
    property Style;
    property Id;
    property TagClass;
    property Language;
    property Direction;
    property Title;
    property OtherAttributes;
  end;

  { An HTML table column group }
  THTMLTableColumnGroup = class(THTMLTableBase)
  private
    FSpan: THTMLNumber;
    FWidth: THTMLLength;
  public
    constructor Create(sStyle: string = ''; iSpan: THTMLNumber = 0; iWidth: THTMLLength = 0; sTagClass: string = ''; sId: string = ''); virtual;
    function AsHTML: string; override; stdcall;
    property Span: THTMLNumber read FSpan write FSpan;
    property Width: THTMLLength read FWidth write FWidth;
    property AlignHoriz;
    property AlignVert;
    property AlignToChar;
    property Style;
    property Id;
    property TagClass;
    property Language;
    property Direction;
    property Title;
    property OtherAttributes;
  end;

  { An HTML table column style }
  THTMLTableColumn = class(THTMLBase)
  private
    FSpan: THTMLNumber;
    FWidth: THTMLLength;
    FAlignHoriz: THTMLTableAlignHoriz;
    FAlignVert: THTMLTableAlignVert;
    FAlignToChar: Char;
  public
    constructor Create(sStyle: string = ''; iSpan: THTMLNumber = 0; iWidth: THTMLLength = 0; sTagClass: string = ''; sId: string = ''); virtual;
    function AsHTML: string; override; stdcall;
    property Span: THTMLNumber read FSpan write FSpan;
    property Width: THTMLLength read FWidth write FWidth;
    property AlignHoriz: THTMLTableAlignHoriz read FAlignHoriz write FAlignHoriz;
    property AlignVert: THTMLTableAlignVert read FAlignVert write FAlignVert;
    property AlignToChar: Char read FAlignToChar write FAlignToChar;
    property Style;
    property Id;
    property TagClass;
    property Language;
    property Direction;
    property Title;
    property OtherAttributes;
  end;

  { An HTML table row }
  THTMLTableRow = class(THTMLTableBase)
  public
    function AsHTML: string; override; stdcall;
    property AlignHoriz;
    property AlignVert;
    property AlignToChar;
    property Background;
    property BorderColour;
    property BorderHighlight;
    property BorderShadow;
    property Style;
    property Id;
    property TagClass;
    property Language;
    property Direction;
    property Title;
    property OtherAttributes;
  end;

  { Base for an HTML table cell }
  THTMLTableCellBase = class(THTMLTableBase)
  private
    FColSpan: THTMLNumber;
    FRowSpan: THTMLNumber;
    FWidth: THTMLLength;
    FHeight: THTMLLength;
    FText: string;
  protected
    constructor Create;
    property ColSpan: THTMLNumber read FColSpan write FColSpan;
    property RowSpan: THTMLNumber read FRowSpan write FRowSpan;
    property Width: THTMLLength read FWidth write FWidth;
    property Height: THTMLLength read FHeight write FHeight;
    property Text: string read FText write FText;
    function SpansAsHTML: string;
  end;

  { Base for an HTML table cell }
  THTMLTableHeading = class(THTMLTableCellBase)
  public
    constructor Create(sText: string; iColSpan: THTMLNumber = 1; iRowSpan: THTMLNumber = 1); virtual;
    function AsHTML: string; override; stdcall;
    property ColSpan;
    property RowSpan;
    property Width;
    property Height;
    property Text;
    property AlignHoriz;
    property AlignVert;
    property AlignToChar;
    property Background;
    property BorderColour;
    property BorderHighlight;
    property BorderShadow;
    property Style;
    property Id;
    property TagClass;
    property Language;
    property Direction;
    property Title;
    property OtherAttributes;
  end;

  { Base for an HTML table cell }
  THTMLTableDetail = class(THTMLTableCellBase)
  public
    constructor Create(sText: string; iColSpan: THTMLNumber = 1; iRowSpan: THTMLNumber = 1); virtual;
    function AsHTML: string; override; stdcall;
    property ColSpan;
    property RowSpan;
    property Width;
    property Height;
    property Text;
    property AlignHoriz;
    property AlignVert;
    property AlignToChar;
    property Background;
    property BorderColour;
    property BorderHighlight;
    property BorderShadow;
    property Style;
    property Id;
    property TagClass;
    property Language;
    property Direction;
    property Title;
    property OtherAttributes;
  end;

  { An HTML form }
  THTMLForm = class(THTMLContainer)
  private
    FDestination: string;
    FMethod: THTMLFormMethod;
    FTarget: string;
  public
    constructor Create(fmMethod: THTMLFormMethod; sDestination: string; sTarget: string = ''); virtual;
    function AsHTML: string; override; stdcall;
    property Destination: string read FDestination write FDestination;
    property Method: THTMLFormMethod read FMethod write FMethod;
    property Target: string read FTarget write FTarget;
    property Style;
    property Id;
    property TagClass;
    property Language;
    property Direction;
    property Title;
    property OtherAttributes;
  end;

  { A set of fields on an HTML form }
  THTMLFieldSet = class(THTMLContainer)
  private
    FLegend: string;
    FLegendAlignment: THTMLAlignmentCaption;
  public
    constructor Create(sLegend: string; acLegendAlignment: THTMLAlignmentCaption = acDefault); virtual;
    function AsHTML: string; override; stdcall;
    property Legend: string read FLegend write FLegend;
    property LegendAlignment: THTMLAlignmentCaption read FLegendAlignment write FLegendAlignment;
    property Style;
    property Id;
    property TagClass;
    property Language;
    property Direction;
    property Title;
    property AccessKey;
    property OtherAttributes;
  end;

  { An HTML input field }
  THTMLInputField = class(THTMLBase)
  private
    FFieldType: THTMLFieldType;
    FName: string;
    FChecked: Boolean;
    FDisabled: Boolean;
    FReadOnly: Boolean;
    FSize: THTMLLength;
    FMaxLength: THTMLNumber;
    FImage: string;
    FAlignment: THTMLAlignmentHoriz;
    FBorderWidth: THTMLPixels;
    FOnClick: string;
  protected
    FValue: string;
  public
    constructor Create(ftFieldType: THTMLFieldType; sName: string); virtual;
    function AsHTML: string; override; stdcall;
    property FieldType: THTMLFieldType read FFieldType write FFieldType;
    property Name: string read FName write FName;
    property Value: string read FValue write FValue;
    property Checked: Boolean read FChecked write FChecked;
    property Disabled: Boolean read FDisabled write FDisabled;
    property ReadOnly: Boolean read FReadOnly write FReadOnly;
    property Size: THTMLLength read FSize write FSize;
    property MaxLength: THTMLNumber read FMaxLength write FMaxLength;
    property Image: string read FImage write FImage;
    property Alignment: THTMLAlignmentHoriz read FAlignment write FAlignment;
    property BorderWidth: THTMLPixels read FBorderWidth write FBorderWidth;
    property OnClick : string read FOnClick write FOnClick; //incrementado ROGER
    property Style;
    property Id;
    property TagClass;
    property Language;
    property Direction;
    property Title;
    property AccessKey;
    property TabIndex;
    property OtherAttributes;
  end;

  { An HTML text input field }
  THTMLTextField = class(THTMLInputField)
  public
    constructor Create(sName: string; sValue: string = ''; iSize: THTMLLength = 0; iMaxLength: THTMLNumber = 0); reintroduce;
  end;

  { An HTML password input field }
  THTMLPasswordField = class(THTMLInputField)
  public
    constructor Create(sName: string; sValue: string = ''; iSize: THTMLLength = 0; iMaxLength: THTMLNumber = 0); reintroduce;
  end;

  { An HTML checkbox field }
  THTMLCheckboxField = class(THTMLInputField)
  public
    constructor Create(sName: string; sValue: string = ''; bChecked: Boolean = False); reintroduce;
  end;

  { An HTML radio button field }
  THTMLRadioField = class(THTMLInputField)
  public
    constructor Create(sName, sValue: string; bChecked: Boolean = False); reintroduce;
  end;

  { An HTML submit field }
  THTMLSubmitField = class(THTMLInputField)
  public
    constructor Create(sName: string = ''; sValue: string = 'Submit'); reintroduce;
  end;

  { An HTML reset field }
  THTMLResetField = class(THTMLInputField)
  public
    constructor Create(sValue: string = 'Reset'); reintroduce;
  end;

  { An HTML image field }
  THTMLImageField = class(THTMLInputField)
  public
    constructor Create(sName, sImage: string; ahAlignment: THTMLAlignmentHoriz = ahDefault; iBorderWidth: THTMLPixels = 0); reintroduce;
  end;

  { An HTML hidden field }
  THTMLHiddenField = class(THTMLInputField)
  public
    constructor Create(sName, sValue: string); reintroduce;
  end;

  { An HTML file field }
  THTMLFileField = class(THTMLInputField)
  public
    constructor Create(sName: string); reintroduce;
  end;

  { An HTML button field }
  THTMLButtonField = class(THTMLInputField)
  public
    constructor Create(sName, sValue: string); reintroduce;
  end;

  { An HTML button }
  THTMLButton = class(THTMLContainer)
  private
    FButtonType: THTMLButtonType;
    FName: string;
    FValue: string;
    FDisabled: Boolean;
    FText: string;
  public
    constructor Create(btButtonType: THTMLButtonType; sName, sText: string); virtual;
    function AsHTML: string; override; stdcall;
    property ButtonType: THTMLButtonType read FButtonType write FButtonType;
    property Name: string read FName write FName;
    property Value: string read FValue write FValue;
    property Disabled: Boolean read FDisabled write FDisabled;
    property Text: string read FText write FText;
    property Style;
    property Id;
    property TagClass;
    property Language;
    property Direction;
    property Title;
    property AccessKey;
    property TabIndex;
    property OtherAttributes;
  end;

  { An HTML selection field }
  THTMLSelectField = class(THTMLContainer)
  private
    FName: string;
    FSize: THTMLNumber;
    FMultiple: Boolean;
    FDisabled: Boolean;
  public
    constructor Create(sName: string; iSize: THTMLNumber = 1; bMultiple: Boolean = False); virtual;
    function AsHTML: string; override; stdcall;
    property Name: string read FName write FName;
    property Size: THTMLNumber read FSize write FSize;
    property Multiple: Boolean read FMultiple write FMultiple;
    property Disabled: Boolean read FDisabled write FDisabled;
    property Style;
    property Id;
    property TagClass;
    property Language;
    property Direction;
    property Title;
    property TabIndex;
    property OtherAttributes;
    procedure AddItem(sOption, sValue: string; bSelected: Boolean);
    procedure AddItems(slsItems: TStrings);
  end;

  { An HTML selection field }
  THTMLSelectGroup = class(THTMLContainer)
  private
    FGroupLabel: string;
    FDisabled: Boolean;
  public
    constructor Create(sGroupLabel: string); virtual;
    function AsHTML: string; override; stdcall;
    property GroupLabel: string read FGroupLabel write FGroupLabel;
    property Disabled: Boolean read FDisabled write FDisabled;
    property Style;
    property Id;
    property TagClass;
    property Language;
    property Direction;
    property Title;
    property OtherAttributes;
    procedure AddItem(sOption, sValue: string; bSelected: Boolean);
    procedure AddItems(slsItems: TStrings);
  end;

  { An HTML selection item }
  THTMLSelectOption = class(THTMLBase)
  private
    FOption: string;
    FValue: string;
    FSelected: Boolean;
    FDisabled: Boolean;
    FShortLabel: string;
  public
    constructor Create(sOption: string; sValue: string = ''; bSelected: Boolean = False); virtual;
    function AsHTML: string; override; stdcall;
    property Option: string read FOption write FOption;
    property Value: string read FValue write FValue;
    property Selected: Boolean read FSelected write FSelected;
    property Disabled: Boolean read FDisabled write FDisabled;
    property ShortLabel: string read FShortLabel write FShortLabel;
    property Style;
    property Id;
    property TagClass;
    property Language;
    property Direction;
    property Title;
    property OtherAttributes;
  end;

  { An HTML memo field }
  THTMLTextareaField = class(THTMLBase)
  private
    FName: string;
    FText: string;
    FCols: THTMLNumber;
    FRows: THTMLNumber;
    FDisabled: Boolean;
    FReadOnly: Boolean;
  public
    constructor Create(sName: string; sText: string = ''; iCols: THTMLNumber = 40; iRows: THTMLNumber = 3); virtual;
    function AsHTML: string; override; stdcall;
    property Name: string read FName write FName;
    property Text: string read FText write FText;
    property Cols: THTMLNumber read FCols write FCols;
    property Rows: THTMLNumber read FRows write FRows;
    property Disabled: Boolean read FDisabled write FDisabled;
    property ReadOnly: Boolean read FReadOnly write FReadOnly;
    property Style;
    property Id;
    property TagClass;
    property Language;
    property Direction;
    property Title;
    property AccessKey;
    property TabIndex;
    property OtherAttributes;
  end;

  { An HTML form label field }
  THTMLLabel = class(THTMLContainer)
  private
    FForId: string;
    FText: string;
  public
    constructor Create(sForId, sText: string); virtual;
    function AsHTML: string; override; stdcall;
    property ForId: string read FForId write FForId;
    property Text: string read FText write FText;
    property Style;
    property Id;
    property TagClass;
    property Language;
    property Direction;
    property Title;
    property AccessKey;
    property OtherAttributes;
  end;

  { Bulk insertion from a stream }
  THTMLStream = class(THTMLBase)
  private
    FStream: TStream;
    FOwnStream: Boolean;
    FOnHTMLTag: THTMLTagEvent;
    function HandleTag(const sTag: string; slsParams: TStrings): string;
    procedure DoTagEvent(iTag: THTMLTag; const sTag: string;
      slsParams: TStrings; var sReplace: string);
    function HandleCommonTag(const sTag: string; slsParams: TStrings): string;
  public
    constructor Create(stmStream: TStream); virtual;
    destructor Destroy; override;
    function AsHTML: string; override; stdcall;
    property OwnStream: Boolean read FOwnStream write FOwnStream;
    property OnHTMLTag: THTMLTagEvent read FOnHTMLTag write FOnHTMLTag;
  end;

  { All exceptions from HTML generation }
  EHTMLError = class(Exception);

  { Which errors can be reported? }
  THTMLErrorLevel = (heStrictHTML4, heErrors);

{ Encode special characters for HTML, ie. '<', '>', '&', '"' }
function EscapeText(sText: string): string;
{ Encode special characters for HTML, ie. those > #80 }
function EncodeSpecialChars(sText: string): string;
{ Denote the value as a percentage }
function AsPercentage(iValue: THTMLLength): THTMLLength;
{ Denote the value as a relative size }
function AsRelative(iValue: THTMLLength): THTMLLength;
{ Is the value a percentage? }
function IsPercentage(var iValue: THTMLLength): Boolean;
{ Is the value a relative size? }
function IsRelative(var iValue: THTMLLength): Boolean;
{ Format a colour for HTML }
function HTMLColour(clr: TColor): string;
  { Which errors are to be reported? }

var
  { Which errors are to be reported? }
  HTMLErrorLevel: set of THTMLErrorLevel;

implementation

uses
  CopyPrsr;

resourcestring
  { Error messages }
  sNotHTMLProducer         = 'Impossível adcionar um objeto não-HTML-producer '; //Cannot add a non-HTML-producing object
  sDeprecatedAttribute     = 'Deprecated attribute(s) in %s tag';
  sMissingDocumentTitle    = 'Missing document title';
  sMissingMetadataName     = 'Missing meta data name or HTTP-equivalent';
  sMissingMetadataContent  = 'Missing meta data content';
  sMissingScriptType       = 'Missing script type';
  sMissingScriptCode       = 'Missing script source and code';
  sMissingImageSource      = 'Missing image source';
  sMissingImageAltText     = 'Missing image alternate text';
  sMissingImageMapName     = 'Missing image map name';
  sMissingImageAreaAltText = 'Missing image map area alternate text';
  sInvalidNumberOfCoords   = 'Invalid number of coordinates for %s';
  sMissingClassIdData      = 'Missing class id and data for object';
  sMissingParameterName    = 'Missing object parameter name';
  sMissingAnchorDetails    = 'Faltando texto e/ou destino do link'; //'Missing anchor name and destination';
  sMissingDestination      = 'Missing %s destination';
  sMissingRadioValue       = 'Missing radio button value';
  sNoOptionsForSelect      = 'No options in select field';
  sMissingSelectGroupLabel = 'Missing select group label';
  sMissingTextAreaColsRows = 'Missing text area columns or rows';
  { Other text }
  sHideScriptStart = 'Hide script contents from old browsers';
  sHideScriptEnd   = 'End hiding contents from old browsers';
  sTimeSubstitute  = 'time';    { Subtitution tag for time stamp }
  sTimeFormat      = 'format';  { Attribute for time format }

const
  { Text representations of enumerated values }
  sHTMLDirection: array [THTMLDirection] of string[3] =
    ('', 'ltr', 'rtl');
  sHTMLAlignmentHoriz: array [THTMLAlignmentHoriz] of string[6] =
    ('', 'left', 'center', 'right');
  sHTMLAlignmentVert: array [THTMLAlignmentVert] of string[6] =
    ('', 'top', 'middle', 'bottom');
  sHTMLAlignmentImage: array [THTMLAlignmentImage] of string[6] =
    ('', 'top', 'middle', 'bottom', 'left', 'right');
  sHTMLAlignmentCaption: array [THTMLAlignmentCaption] of string[6] =
    ('', 'top', 'bottom', 'left', 'right');
  sHTMLFrameScroll: array [THTMLFrameScroll] of string[3] =
    ('', 'yes', 'no');
  sHTMLShape: array [THTMLShape] of string[7] =
    ('default', 'rect', 'circle', 'polygon');
  sHTMLParamTypes: array [THTMLParamTypes] of string[6] =
    ('data', 'ref', 'object');
  sHTMLListType: array [THTMLListType] of string[2] =
    ('ul', 'ol', 'dl');
  sHTMLListBullet: array [THTMLListBullet] of string[6] =
    ('', 'disc', 'circle', 'square');
  sHTMLListItem: array [THTMLListItemType] of string[2] =
    ('li', 'dt', 'dd');
  sHTMLNumberScheme: array [THTMLNumberScheme] of string[1] =
    ('', 'A', 'a', 'I', 'i', '1');
  sHTMLClearTo: array [THTMLClearTo] of string[5] =
    ('', 'left', 'right', 'all');
  sHTMLTextStyle: array [THTMLTextStyle] of string[10] =
    ('', 'b', 'i', 'tt', 'big', 'small', 'address', 'pre', 'em', 'strong',
    'dfn', 'code', 'samp', 'kbd', 'var', 'cite', 'abbr', 'acronym',
    'blockquote', 'q', 'sup', 'sub', 'ins', 'del',
    'u', 'strike', 'blink', 'nobr', 'center');
  sHTMLTableFrame: array [THTMLTableFrame] of string[6] =
    ('void', 'above', 'below', 'hsides', 'vsides', 'lhs', 'rhs', 'box', 'border');
  sHTMLTableRule: array [THTMLTableRule] of string[6] =
    ('none', 'groups', 'rows', 'cols', 'all');
  sHTMLTableAlignHoriz: array [THTMLTableAlignHoriz] of string[7] =
    ('', 'left', 'center', 'right', 'justify', 'char');
  sHTMLTableAlignVert: array [THTMLTableAlignVert] of string[8] =
    ('', 'top', 'middle', 'bottom', 'baseline');
  sHTMLTableRowGrouping: array [THTMLTableRowGrouping] of string[5] =
    ('thead', 'tbody', 'tfoot');
  sHTMLFormMethod: array [THTMLFormMethod] of string[4] =
    ('get', 'post');
  sHTMLFieldType: array [THTMLFieldType] of string[8] =
    ('text', 'password', 'checkbox', 'radio', 'submit',
    'reset', 'button', 'image', 'hidden', 'file');

type
  TCharSet = set of Char;

{ Common functions ------------------------------------------------------------}

{ Encode special characters for HTML, ie. '<', '>', '&', '"' }
function EscapeText(sText: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(sText) do
    case sText[i] of
      '<': Result := Result + '&lt;';
      '>': Result := Result + '&gt;';
      '&': Result := Result + '&amp;';
      '"': Result := Result + '&quot;';
      else Result := Result + sText[i];
    end;
end;

{ Encode special characters for HTML, ie. those > #80 }
function EncodeSpecialChars(sText: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(sText) do
    case sText[i] of
      'Œ': Result := Result + '&OElig;';   { 140, #8C }
      'œ': Result := Result + '&oelig;';   { 156, #9C }
      'Ÿ': Result := Result + '&Yuml;';    { 159, #9F }
      ' ': Result := Result + '&nbsp;';    { 160, #A0 }
      '¡': Result := Result + '&iexcl;';   { 161, #A1 }
      '¢': Result := Result + '&cent;';    { 162, #A2 }
      '£': Result := Result + '&pound;';   { 163, #A3 }
      '¤': Result := Result + '&curren;';  { 164, #A4 }
      '¥': Result := Result + '&yen;';     { 165, #A5 }
      '¦': Result := Result + '&brvbar;';  { 166, #A6 }
      '§': Result := Result + '&sect;';    { 167, #A7 }
      '¨': Result := Result + '&uml;';     { 168, #A8 }
      '©': Result := Result + '&copy;';    { 169, #A9 }
      'ª': Result := Result + '&ordf;';    { 170, #AA }
      '«': Result := Result + '&laquo;';   { 171, #AB }
      '¬': Result := Result + '&not;';     { 172, #AC }
      '­': Result := Result + '&shy;';     { 173, #AD }
      '®': Result := Result + '&reg;';     { 174, #AE }
      '¯': Result := Result + '&macr;';    { 175, #AF }
      '°': Result := Result + '&deg;';     { 176, #B0 }
      '±': Result := Result + '&plusmn;';  { 177, #B1 }
      '²': Result := Result + '&sup2;';    { 178, #B2 }
      '³': Result := Result + '&sup3;';    { 179, #B3 }
      '´': Result := Result + '&acute;';   { 180, #B4 }
      'µ': Result := Result + '&micro;';   { 181, #B5 }
      '¶': Result := Result + '&para;';    { 182, #B6 }
      '·': Result := Result + '&middot;';  { 183, #B7 }
      '¸': Result := Result + '&cedil;';   { 184, #B8 }
      '¹': Result := Result + '&sup1;';    { 185, #B9 }
      'º': Result := Result + '&ordm;';    { 186, #BA }
      '»': Result := Result + '&raquo;';   { 187, #BB }
      '¼': Result := Result + '&frac14;';  { 188, #BC }
      '½': Result := Result + '&frac12;';  { 189, #BD }
      '¾': Result := Result + '&frac34;';  { 190, #BE }
      '¿': Result := Result + '&iquest;';  { 191, #BF }
      'À': Result := Result + '&Agrave;';  { 192, #C0 }
      'Á': Result := Result + '&Aacute;';  { 193, #C1 }
      'Â': Result := Result + '&Acirc;';   { 194, #C2 }
      'Ã': Result := Result + '&Atilde;';  { 195, #C3 }
      'Ä': Result := Result + '&Auml;';    { 196, #C4 }
      'Å': Result := Result + '&Aring;';   { 197, #C5 }
      'Æ': Result := Result + '&AElig;';   { 198, #C6 }
      'Ç': Result := Result + '&Ccedil;';  { 199, #C7 }
      'È': Result := Result + '&Egrave;';  { 200, #C8 }
      'É': Result := Result + '&Eacute;';  { 201, #C9 }
      'Ê': Result := Result + '&Ecirc;';   { 202, #CA }
      'Ë': Result := Result + '&Euml;';    { 203, #CB }
      'Ì': Result := Result + '&Igrave;';  { 204, #CC }
      'Í': Result := Result + '&Iacute;';  { 205, #CD }
      'Î': Result := Result + '&Icirc;';   { 206, #CE }
      'Ï': Result := Result + '&Iuml;';    { 207, #CF }
      'Ð': Result := Result + '&ETH;';     { 208, #D0 }
      'Ñ': Result := Result + '&Ntilde;';  { 209, #D1 }
      'Ò': Result := Result + '&Ograve;';  { 210, #D2 }
      'Ó': Result := Result + '&Oacute;';  { 211, #D3 }
      'Ô': Result := Result + '&Ocirc;';   { 212, #D4 }
      'Õ': Result := Result + '&Otilde;';  { 213, #D5 }
      'Ö': Result := Result + '&Ouml;';    { 214, #D6 }
      '×': Result := Result + '&times;';   { 215, #D7 }
      'Ø': Result := Result + '&Oslash;';  { 216, #D8 }
      'Ù': Result := Result + '&Ugrave;';  { 217, #D9 }
      'Ú': Result := Result + '&Uacute;';  { 218, #DA }
      'Û': Result := Result + '&Ucirc;';   { 219, #DB }
      'Ü': Result := Result + '&Uuml;';    { 220, #DC }
      'Ý': Result := Result + '&Yacute;';  { 221, #DD }
      'Þ': Result := Result + '&THORN;';   { 222, #DE }
      'ß': Result := Result + '&szlig;';   { 223, #DF }
      'à': Result := Result + '&agrave;';  { 224, #E0 }
      'á': Result := Result + '&aacute;';  { 225, #E1 }
      'â': Result := Result + '&acirc;';   { 226, #E2 }
      'ã': Result := Result + '&atilde;';  { 227, #E3 }
      'ä': Result := Result + '&auml;';    { 228, #E4 }
      'å': Result := Result + '&aring;';   { 229, #E5 }
      'æ': Result := Result + '&aelig;';   { 230, #E6 }
      'ç': Result := Result + '&ccedil;';  { 231, #E7 }
      'è': Result := Result + '&egrave;';  { 232, #E8 }
      'é': Result := Result + '&eacute;';  { 233, #E9 }
      'ê': Result := Result + '&ecirc;';   { 234, #EA }
      'ë': Result := Result + '&euml;';    { 235, #EB }
      'ì': Result := Result + '&igrave;';  { 236, #EC }
      'í': Result := Result + '&iacute;';  { 237, #ED }
      'î': Result := Result + '&icirc;';   { 238, #EE }
      'ï': Result := Result + '&iuml;';    { 239, #EF }
      'ð': Result := Result + '&eth;';     { 240, #F0 }
      'ñ': Result := Result + '&ntilde;';  { 241, #F1 }
      'ò': Result := Result + '&ograve;';  { 242, #F2 }
      'ó': Result := Result + '&oacute;';  { 243, #F3 }
      'ô': Result := Result + '&ocirc;';   { 244, #F4 }
      'õ': Result := Result + '&otilde;';  { 245, #F5 }
      'ö': Result := Result + '&ouml;';    { 246, #F6 }
      '÷': Result := Result + '&divide;';  { 247, #F7 }
      'ø': Result := Result + '&oslash;';  { 248, #F8 }
      'ù': Result := Result + '&ugrave;';  { 249, #F9 }
      'ú': Result := Result + '&uacute;';  { 250, #FA }
      'û': Result := Result + '&ucirc;';   { 251, #FB }
      'ü': Result := Result + '&uuml;';    { 252, #FC }
      'ý': Result := Result + '&yacute;';  { 253, #FD }
      'þ': Result := Result + '&thorn;';   { 254, #FE }
      'ÿ': Result := Result + '&yuml;';    { 255, #FF }
      else Result := Result + sText[i];
    end;
end;

{ Denote the value as a percentage }
function AsPercentage(iValue: THTMLLength): THTMLLength;
begin
  Result := - Abs(iValue);
end;

{ Denote the value as a relative size }
function AsRelative(iValue: THTMLLength): THTMLLength;
begin
  Result := MaxInt - Abs(iValue);
end;

{ Is the value a percentage? }
function IsPercentage(var iValue: THTMLLength): Boolean;
begin
  Result := (iValue < 0);
  if Result then
    iValue := Abs(iValue);
end;

{ Is the value a relative size? }
function IsRelative(var iValue: THTMLLength): Boolean;
begin
  Result := (iValue > MaxInt div 2);
  if Result then
    iValue := MaxInt - iValue;
end;

{ Convert size to text representation }
function SizeAsText(iValue: THTMLLength): string;
begin
  if iValue < 0 then
    Result := IntToStr(Abs(iValue)) + '%'
  else if iValue > MaxInt div 2 then
    Result := IntToStr(MaxInt - iValue) + '*'    
  else
    Result := IntToStr(iValue);
end;

{ Format a colour for HTML }
function HTMLColour(clr: TColor): string;
begin
  Result := '#' + IntToHex(GetRValue(clr), 2) + IntToHex(GetGValue(clr), 2) +
    IntToHex(GetBValue(clr), 2);
end;

{ Format numeric attribute for inclusion in tag }
function NumericAttribute(sName: string; iValue: THTMLLength): string;
begin
  Result :='';
  if iValue <> 0 then
    Result := ' ' + sName + '=' + SizeAsText(iValue);
end;

{ Format text attribute for inclusion in tag }
function TextAttribute(sName, sValue: string): string;
begin
  Result :='';
  if sValue <> '' then
    Result := ' ' + sName + '="' + sValue + '"';
end;

{ Format boolean attribute for inclusion in tag }
function BooleanAttribute(bTest: Boolean; sValue: string): string;
begin
  Result :='';
  if bTest then
    Result := ' ' + sValue;
end;

{ Format colour attribute for inclusion in tag }
function ColourAttribute(sName: string; clValue: TColor): string;
begin
  Result :='';
  if clValue <> clNone then
    Result := ' ' + sName + '=' + HTMLColour(clValue);
end;

{ Format horizontal alignment attribute for inclusion in tag }
function AlignHorizAttribute(sName: string; ahValue: THTMLAlignmentHoriz): string;
begin
  Result :='';
  if ahValue <> ahDefault then
    Result := ' ' + sName + '=' + sHTMLAlignmentHoriz[ahValue];
end;

{ Format vertical alignment attribute for inclusion in tag }
function AlignVertAttribute(sName: string; avValue: THTMLAlignmentVert): string;
begin
  Result :='';
  if avValue <> avDefault then
    Result := ' ' + sName + '=' + sHTMLAlignmentVert[avValue];
end;

{ Format image alignment attribute for inclusion in tag }
function AlignImageAttribute(sName: string; aiValue: THTMLAlignmentImage): string;
begin
  Result :='';
  if aiValue <> aiDefault then
    Result := ' ' + sName + '=' + sHTMLAlignmentImage[aiValue];
end;

{ Format caption alignment attribute for inclusion in tag }
function AlignCaptionAttribute(sName: string; acValue: THTMLAlignmentCaption): string;
begin
  Result :='';
  if acValue <> acDefault then
    Result := ' ' + sName + '=' + sHTMLAlignmentCaption[acValue];
end;

{ Format clear attribute for inclusion in tag }
function ClearAttribute(sName: string; ctValue: THTMLClearTo): string;
begin
  Result :='';
  if ctValue <> ctDefault then
    Result := ' ' + sName + '=' + sHTMLClearTo[ctValue];
end;

{ THTMLBase -------------------------------------------------------------------}

{ Does this object support this interface? }
function THTMLBase.QueryInterface(const IID: TGUID; out Obj): HResult;
const
  E_NOINTERFACE : DWORD = $80004002;
begin
	if GetInterface(IID, Obj) then begin
		Result := 0
	end else begin
		Result := E_NOINTERFACE;
	end;
end;

{ Disable reference counting }
function THTMLBase._AddRef: Integer;
begin
  Result := -1;
end;

{ Disable reference counting }
function THTMLBase._Release: Integer;
begin
  Result := -1;
end;

{ Return formatted HTML }
function THTMLBase.AsHTML: string;
begin
  Result := '';
end;

{ Format all base attributes for inclusion in tag }
function THTMLBase.BaseAttributesAsHTML: string;
begin
  Result := TextAttribute('style', Style) + TextAttribute('id', Id) +
    TextAttribute('class', TagClass) + TextAttribute('lang', Language) +
    TextAttribute('title', Title) + NumericAttribute('tabindex', TabIndex);
  if AccessKey <> #0 then
    Result := Result + TextAttribute('accesskey', AccessKey);
  if Direction <> diDefault then
    Result := Result + ' dir=' + sHTMLDirection[Direction];
  Result := Result + BooleanAttribute((OtherAttributes <> ''), OtherAttributes);
end;

{ THTMLContainer --------------------------------------------------------------}

{ Initialise }
constructor THTMLContainer.Create;
begin
  inherited Create;
  lstHTML := TList.Create;
  FOwnContents := True;
end;

{ Release resources }
destructor THTMLContainer.Destroy;
begin
  Clear;
  lstHTML.Free;
  inherited Destroy;
end;

{ Return number of entries }
function THTMLContainer.GetCount: Word;
begin
  Result := lstHTML.Count;
end;

{ Return specific item from list }
function THTMLContainer.GetItems(Index: Integer): TObject;
begin
  Result := TObject(lstHTML[Index]);
end;

{ Return formatted HTML }
function THTMLContainer.AsHTML: string;
begin
  Result := ContentsAsHTML;
end;


function THTMLContainer.ContentsAsHTML: string;
//----------------------------------------------------------------------------------------------------------------------
//Concatenate HTML from each contained element in trun
var
  i: Integer;
  intHTML: IHTMLProducer;
{$IFDEF WELL_FORMATED_HTML }
  j : integer;
  SL : TStringList;
{$ENDIF}
begin
{$IFDEF WELL_FORMATED_HTML }
   SL:=TStringList.Create;
   try
       Result :=EmptyStr;
       for i := 0 to Count - 1 do begin
           TObject(Items[i]).GetInterface(IHTMLProducer, intHTML);
           SL.Text:=intHTML.AsHTML;
           for j:=0 to SL.Count-1 do begin
               SL.Strings[ j ] :='  ' + SL.Strings[ j ];
           end;
           Result := Result + SL.Text;
       end;
   finally
       Sl.Free;
   end;
{$ELSE}
   Result :=EmptyStr;
   for i := 0 to Count - 1 do begin
       TObject(Items[i]).GetInterface(IHTMLProducer, intHTML);
       Result := Result + intHTML.AsHTML;
   end;
{$ENDIF}
end;

procedure THTMLContainer.Add(objHTML: TObject);
//----------------------------------------------------------------------------------------------------------------------
//Add a new HTML producer to the list of contents
var
   intHTML: IHTMLProducer;
begin
   if not objHTML.GetInterface(IHTMLProducer, intHTML) then begin
       raise EHTMLError.Create(sNotHTMLProducer);
   end;
   lstHTML.Add(objHTML);
end;

{ Remove all elements from the contents list }
procedure THTMLContainer.Clear;
var
  i: Integer;
begin
  if FOwnContents then
    for i := 0 to lstHTML.Count - 1 do
      TObject(lstHTML[i]).Free;
  lstHTML.Clear;
end;

{ THTMLComment ----------------------------------------------------------------}

{ Initialise }
constructor THTMLComment.Create(sComment: string);
begin
  inherited Create;
  FComment := sComment;
end;

{ Return formatted HTML }
function THTMLComment.AsHTML: string;
begin
  Result := '<!--' + Comment + '-->';
end;

{ THTMLDocument ---------------------------------------------------------------}

{ Initialise }
constructor THTMLDocument.Create(sTitle: string; sBase: string = '';
  sHeaderLanguage: string = ''; diHeaderDirection: THTMLDirection = diDefault);
begin
  inherited Create;
  FHeaderTags := THTMLContainer.Create;
  FStyleSheet := nil;
  FFrameSet := nil;
  FTitle := sTitle;
  //IFDEF Delphi 4
  FBase := sBase;
  FHeaderLanguage := sHeaderLanguage;
  FHeaderDirection := diHeaderDirection;
  //ENDIF
  FBackgroundColour := clNone;
  FTextColour := clNone;
  FLinkColour := clNone;
  FVisitedColour := clNone;
  FActiveColour := clNone;
end;

{ Release resources }
destructor THTMLDocument.Destroy;
begin
  FHeaderTags.Free;
  if OwnContents then
  begin
    FStyleSheet.Free;
    FFrameSet.Free;
  end;
  inherited Destroy;
end;

{ Return formatted HTML }
function THTMLDocument.AsHTML: string;
begin
  { Check for errors }
  if (heErrors in HTMLErrorLevel) and (Title = '') then
    raise EHTMLError.Create(sMissingDocumentTitle);
  if (heStrictHTML4 in HTMLErrorLevel) and
      ((BackgroundImage <> '') or (BackgroundColour <> clNone) or
      (TextColour <> clNone) or (LinkColour <> clNone) or
      (VisitedColour <> clNone) or (ActiveColour <> clNone)) then
    raise EHTMLError.Create(Format(sDeprecatedAttribute, ['body']));

  { Generate HTML }
  Result := '<html>'#13#10'<head' + TextAttribute('lang', HeaderLanguage);
  if HeaderDirection <> diDefault then
    Result := Result + ' dir=' + sHTMLDirection[HeaderDirection];
  Result := Result + '>'#13#10'<title>' + EncodeSpecialChars(Title) +
    '</title>'#13#10;
  if Base <> '' then
    Result := Result + '<base href="' + Base + '">'#13#10;
  if StyleSheet <> nil then
    Result := Result + StyleSheet.AsHTML;
  Result := Result + FHeaderTags.AsHTML + '</head>'#13#10;
  if FrameSet <> nil then  { Body is a frameset }
    Result := Result + FrameSet.AsHTML
  else                     { Normal body text }
    Result := Result +
    '<body' + TextAttribute('background', BackgroundImage) +
      ColourAttribute('bgcolor', BackgroundColour) +
      ColourAttribute('text', TextColour) +
      ColourAttribute('link', LinkColour) +
      ColourAttribute('vlink', VisitedColour) +
      ColourAttribute('alink', ActiveColour) + BaseAttributesAsHTML +
      '>'#13#10 +
      ContentsAsHTML +
      '</body>'#13#10;
  Result := Result + '</html>';
end;

{ Return whether contents are owned }
function THTMLDocument.GetOwnContents: Boolean;
begin
  Result := inherited OwnContents;
end;

{ Set whether contents are owned - in internal list as well }
procedure THTMLDocument.SetOwnContents(bOwnContents: Boolean);
begin
  inherited OwnContents := bOwnContents;
  FHeaderTags.OwnContents := bOwnContents;
end;

{ Add a new tag to the header block }
procedure THTMLDocument.AddHeaderTag(objHTML: TObject);
begin
  FHeaderTags.Add(objHTML);
end;

{ Clear all tags from the header block }
procedure THTMLDocument.ClearHeaderTags;
begin
  FHeaderTags.Clear;
end;

{ THTMLMetadata ---------------------------------------------------------------}

{ Initialise }
constructor THTMLMetadata.Create(sName, sContent: string; sHTTPEquivalent: string = '');
begin
  inherited Create;
  FName := sName;
  FContent := sContent;
  //IFDEF Delphi 4 }
  FHTTPEquivalent := sHTTPEquivalent;
  //$ENDIF
end;

{ Return formatted HTML }
function THTMLMetadata.AsHTML: string;
begin
  { Check for errors }
  if (heErrors in HTMLErrorLevel) and (Name = '') and (HTTPEquivalent = '') then
    raise EHTMLError.Create(sMissingMetadataName);
  if (heErrors in HTMLErrorLevel) and (Content = '') then
    raise EHTMLError.Create(sMissingMetadataContent);

  { Generate HTML }
  Result := '<meta' + TextAttribute('name', Name) +
    TextAttribute('http-equiv', HTTPEquivalent) +
    TextAttribute('content', Content) + BaseAttributesAsHTML + '>'#13#10;
end;

{ THTMLLink -------------------------------------------------------------------}

{ Initialise }
constructor THTMLLink.Create(sRelType, sDestination: string; sTarget: string = ''; sContentType: string = '');
begin
  inherited Create;
  FRelType := sRelType;
  FDestination := sDestination;
  //IFDEF Delphi 4
  FTarget := sTarget;
  FContentType := sContentType;
  //$ENDIF
end;

{ Return formatted HTML }
function THTMLLink.AsHTML: string;
begin
  { Check for errors }
  if (heErrors in HTMLErrorLevel) and (Destination = '') then
    raise EHTMLError.Create(Format(sMissingDestination, ['link']));

  { Generate HTML }
  Result := '<link' + TextAttribute('href', Destination) +
    TextAttribute('rel', RelType) + TextAttribute('rev', RevType) +
    TextAttribute('target', Target) + TextAttribute('type', ContentType) +
    TextAttribute('title', Title) + BaseAttributesAsHTML + '>'#13#10;
end;

{ THTMLStyleSheet -------------------------------------------------------------}

{ Initialise }
constructor THTMLStyleSheet.Create(sStyleType: string = 'text/css');
begin
  inherited Create;
  slsStyles := TStringList.Create;
  //IFDEF Delphi 4
  FStyleType := sStyleType;
  //$ENDIF
  FCommentOut := True;
end;

{ Release resources }
destructor THTMLStyleSheet.Destroy;
begin
  slsStyles.Free;
  inherited Destroy;
end;

{ Return formatted HTML }
function THTMLStyleSheet.AsHTML: string;
var
  i: Integer;
begin
  Result := '<style' + TextAttribute('type', StyleType) +
    TextAttribute('media', Media) + TextAttribute('title', Title) +
    BaseAttributesAsHTML + '>'#13#10;
  if CommentOut then
    Result := Result + '<!--'#13#10;
  for i := 0 to slsStyles.Count - 1 do
    Result := Result + slsStyles.Names[i] + ' { ' +
      slsStyles.Values[slsStyles.Names[i]] + ' }'#13#10;
  if CommentOut then
    Result := Result + '-->'#13#10;
  Result := Result + '</style>'#13#10;
end;

{ Add new style to list }
procedure THTMLStyleSheet.AddStyle(sElement, sStyle: string);
begin
  slsStyles.Values[sElement] := sStyle;
end;

{ Cear all styles from list }
procedure THTMLStyleSheet.ClearStyles;
begin
  slsStyles.Clear;
end;

{ THTMLFrameSet ---------------------------------------------------------------}

{ Initialise }
constructor THTMLFrameSet.Create(iRows, iCols: array of THTMLLength; sTagClass: string = ''; sId: string = ''; sStyle: string = '');
begin
  inherited Create;
  FRows := TList.Create;
  FCols := TList.Create;
  FNoFrames := THTMLContainer.Create;
  //IFDEF Delphi 4
  TagClass := sTagClass;
  Id := sId;
  Style := sStyle;
  //ENDIF
  SetRows(iRows);
  SetCols(iCols);
end;

{ Release resources }
destructor THTMLFrameSet.Destroy;
begin
  FRows.Free;
  FCols.Free;
  FNoFrames.Free;
  inherited Destroy;
end;

{ Return formatted HTML }
function THTMLFrameSet.AsHTML: string;
begin
  Result := '<frameset' + TextAttribute('rows', RowsAsText) +
    TextAttribute('cols', ColsAsText) + BaseAttributesAsHTML + '>'#13#10 +
    ContentsAsHTML;
  if FNoFrames.Count > 0 then
    Result := Result + '<noframes>'#13#10 +
      FNoFrames.ContentsAsHTML + '</noframes>'#13#10;
  Result := Result + '</frameset>'#13#10;
end;

{ Return row/column sizes as text }
function THTMLFrameSet.GetAsText(RowOrCol: Integer): string;
var
  lst: TList;
  i: Integer;
begin
  if RowOrCol = iRow then
    lst := FRows
  else
    lst := FCols;
  Result := '';
  if (lst.Count > 0) and (THTMLLength(lst[0]) <> 0) then
  begin
    for i := 0 to lst.Count - 1 do
      Result := Result + ', ' + SizeAsText(THTMLLength(lst[i]));
    if Result <> '' then
      Result := Copy(Result, 3, Length(Result) - 2);
  end;
end;

{ Return number of rows/columns }
function THTMLFrameSet.GetCount(RowOrCol: Integer): Integer;
begin
  if RowOrCol = iRow then
    Result := FRows.Count
  else
    Result := FCols.Count;
end;

{ Return size of specified row/column }
function THTMLFrameSet.GetSize(Index, RowOrCol: Integer): THTMLLength;
var
  lst: TList;
begin
  if RowOrCol = iRow then
    lst := FRows
  else
    lst := FCols;
  if Index in [0..lst.Count - 1] then
    Result := THTMLLength(lst[Index])
  else
    Result := 0;
end;

{ Return whether contents are owned }
function THTMLFrameSet.GetOwnContents: Boolean;
begin
  Result := inherited OwnContents;
end;

{ Set size of specified row/column }
procedure THTMLFrameSet.SetSize(Index, RowOrCol: Integer; iSize: THTMLLength);
var
  lst: TList;
begin
  if RowOrCol = iRow then
    lst := FRows
  else
    lst := FCols;
  if Index in [0..lst.Count - 1] then
    lst.Delete(Index);
  lst.Insert(Index, Pointer(iSize));
end;

{ Set all row sizes }
procedure THTMLFrameSet.SetRows(iRows: array of THTMLLength);
begin
  SetSizes(iRow, iRows);
end;

{ Set all column sizes }
procedure THTMLFrameSet.SetCols(iCols: array of THTMLLength);
begin
  SetSizes(iCol, iCols);
end;

{ Set all row/column sizes }
procedure THTMLFrameSet.SetSizes(RowOrCol: Integer; iSizes: array of THTMLLength);
var
  lst: TList;
  i: Integer;
begin
  if RowOrCol = iRow then
    lst := FRows
  else
    lst := FCols;
  lst.Clear;
  for i := 0 to High(iSizes) do
    lst.Add(Pointer(iSizes[i]));
end;

{ Set whether contents are owned - pass on to interal list }
procedure THTMLFrameSet.SetOwnContents(bOwnContents: Boolean);
begin
  inherited OwnContents := bOwnContents;
  FNoFrames.OwnContents := bOwnContents;
end;

{ Add a new tag to the content of the no-frames tag }
procedure THTMLFrameSet.AddNoFrames(objHTML: TObject);
begin
  FNoFrames.Add(objHTML);
end;

{ Clear the contents of the no-frames tag }
procedure THTMLFrameSet.ClearNoFrames;
begin
  FNoFrames.Clear;
end;

{ THTMLFrame ------------------------------------------------------------------}

{ Initialise }
constructor THTMLFrame.Create(sSource: string; sName: string = ''; sTarget: string = '');
begin
  inherited Create;
  FSource := sSource;
  FFrameBorder := True;
  //IFDEF Delphi 4
  FTarget := sTarget;
  FName := sName;
  //ENDIF
end;

{ Return formatted HTML }
function THTMLFrame.AsHTML: string;
begin
  { Check for errors }
  if (heErrors in HTMLErrorLevel) and (Source = '') then
    raise EHTMLError.Create(Format(sMissingDestination, ['frame']));

  { Generate HTML }
  Result := '<frame' + TextAttribute('src', Source) +
    TextAttribute('target', Target) + TextAttribute('name', Name) +
    BooleanAttribute(not FrameBorder, 'frameborder=0') +
    NumericAttribute('marginwidth', MarginWidth) +
    NumericAttribute('marginheight', MarginHeight) +
    BooleanAttribute(NoResize, 'noresize');
  if Scrolling <> fsAuto then
    Result := Result + ' scrolling=' + sHTMLFrameScroll[Scrolling];
  Result := Result + BaseAttributesAsHTML + '>'#13#10;
end;

{ THTMLInlineFrame ------------------------------------------------------------}

{ Initialise }
constructor THTMLInlineFrame.Create(sSource: string; sName: string = ''; sTarget: string = '');
begin
  inherited Create;
  FNoFrames := THTMLContainer.Create;
  FSource := sSource;
  FFrameBorder := True;
  //IFDEF Delphi 4
  FTarget := sTarget;
  FName := sName;
  //ENDIF
end;

{ Release resources }
destructor THTMLInlineFrame.Destroy;
begin
  FNoFrames.Free;
  inherited Destroy;
end;

{ Return formatted HTML }
function THTMLInlineFrame.AsHTML: string;
begin
  { Check for errors }
  if (heErrors in HTMLErrorLevel) and (Source = '') then
    raise EHTMLError.Create(Format(sMissingDestination, ['iframe']));

  { Generate HTML }
  Result := '<iframe' + TextAttribute('src', Source) +
    TextAttribute('target', Target) + TextAttribute('name', Name) +
    BooleanAttribute(not FrameBorder, 'frameborder=0') +
    NumericAttribute('marginwidth', MarginWidth) +
    NumericAttribute('marginheight', MarginHeight) +
    NumericAttribute('width', Width) + NumericAttribute('height', Height) +
    AlignCaptionAttribute('align', Alignment);
  if Scrolling <> fsAuto then
    Result := Result + ' scrolling=' + sHTMLFrameScroll[Scrolling];
  Result := Result + BaseAttributesAsHTML + '>'#13#10 +
    FNoFrames.ContentsAsHTML + '</iframe>'#13#10;
end;

{ Return whether no-frames contents are owned }
function THTMLInlineFrame.GetOwnContents: Boolean;
begin
  Result := FNoFrames.OwnContents;
end;

{ Set whether no-frames contents are owned }
procedure THTMLInlineFrame.SetOwnContents(bOwnContents: Boolean);
begin
  FNoFrames.OwnContents := bOwnContents;
end;

{ Add a new tag to the content of the no-frames tag }
procedure THTMLInlineFrame.AddNoFrames(objHTML: TObject);
begin
  FNoFrames.Add(objHTML);
end;

{ Clear the contents of the no-frames tag }
procedure THTMLInlineFrame.ClearNoFrames;
begin
  FNoFrames.Clear;
end;

{ THTMLScript -----------------------------------------------------------------}

{ Initialise }
constructor THTMLScript.Create(sScriptType, sCode: string);
begin
  inherited Create;
  ScriptType := sScriptType;
  FCode := sCode;
  FHideInComment := True;
end;

{ Set type of scripting language, default comment sequence too }
procedure THTMLScript.SetScriptType(sScriptType: string);
begin
  if FScriptType <> sScriptType then
  begin
    FScriptType := sScriptType;
    if Pos('javascript', FScriptType) > 0 then
      FCommentSequence := '//'
    else if Pos('vbscript', FScriptType) > 0 then
      FCommentSequence := ''''
    else if Pos('tcl', FScriptType) > 0 then
      FCommentSequence := '#';
  end;
end;

{ Return formatted HTML }
function THTMLScript.AsHTML: string;
begin
  { Check for errors }
  if (heErrors in HTMLErrorLevel) and (ScriptType = '') then
    raise EHTMLError.Create(sMissingScriptType);
  if (heErrors in HTMLErrorLevel) and ((Source = '') and (Code = '')) then
    raise EHTMLError.Create(sMissingScriptCode);

  { Generate HTML }
  Result := '<script' + TextAttribute('type', ScriptType) +
    TextAttribute('src', Source) + BooleanAttribute(Defer, 'defer') + '>'#13#10;
  if Code <> '' then
  begin
    if HideInComment then
      Result := Result + '<!-- ' + sHideScriptStart + #13#10;
    Result := Result + Code;
    if HideInComment then
      Result := Result + #13#10 + CommentSequence +
        ' ' + sHideScriptEnd + ' -->'#13#10;
  end;
  Result := Result + '</script>'#13#10;
end;

{ THTMLNoScript ---------------------------------------------------------------}

{ Return formatted HTML }
function THTMLNoScript.AsHTML: string;
begin
  Result := '<noscript' + BaseAttributesAsHTML + '>'#13#10 +
    ContentsAsHTML + '</noscript>'#13#10;
end;

{ THTMLDivision ---------------------------------------------------------------}

{ Initialise }
constructor THTMLDivision.Create(bBlockLevel: Boolean; sStyle, sText: string; sTagClass: string = ''; sId: string = '');
begin
  inherited Create;
  FBlockLevel := bBlockLevel;
  FStyle := sStyle;
  FText := sText;
  FAlignment := ahDefault;
  //IFDEF Delphi 4
  TagClass := sTagClass;
  Id := sId;
  //ENDIF
end;

{ Return formatted HTML }
function THTMLDivision.AsHTML: string;
const
  sDivisionType: array [Boolean, 1..2] of string[4] =
    (('span', ''), ('div', #13#10));
begin
  { Check for errors }
  if (heStrictHTML4 in HTMLErrorLevel) and (Alignment <> ahDefault) then
    raise EHTMLError.Create(Format(sDeprecatedAttribute, ['div/span']));

  { Generate HTML }
  Result := '<' + sDivisionType[BlockLevel, 1] + BaseAttributesAsHTML +
    AlignHorizAttribute('align', Alignment) + '>' +
    sDivisionType[BlockLevel, 2] + EncodeSpecialChars(Text) + ContentsAsHTML +
    '</' + sDivisionType[BlockLevel, 1] + '>' + sDivisionType[BlockLevel, 2];
end;

{ THTMLParagraph --------------------------------------------------------------}

{ Initialise }
constructor THTMLParagraph.Create(sText: string; sTagClass: string = ''; sId: string = ''; sStyle: string = '');
begin
  inherited Create;
  FText := sText;
  FAlignment := ahDefault;
  //IFDEF Delphi 4
  TagClass := sTagClass;
  Id := sId;
  Style := sStyle;
  //ENDIF
end;

{ Return formatted HTML }
function THTMLParagraph.AsHTML: string;
begin
  { Check for errors }
  if (heStrictHTML4 in HTMLErrorLevel) and (Alignment <> ahDefault) then
    raise EHTMLError.Create(Format(sDeprecatedAttribute, ['p']));

  { Generate HTML }
  Result := '<p' + AlignHorizAttribute('align', Alignment) +
    BaseAttributesAsHTML + '>' + EncodeSpecialChars(Text) + ContentsAsHTML + '</p>'#13#10;
end;

{ THTMLHeading ----------------------------------------------------------------}

{ Initialise }
constructor THTMLHeading.Create(iLevel: THTMLHeadingLevel; sText: string; sTagClass: string = ''; sId: string = ''; sStyle: string = '');
begin
  inherited Create;
  FLevel := iLevel;
  FText := sText;
  FAlignment := ahDefault;
  //IFDEF Delphi 4
  TagClass := sTagClass;
  Id := sId;
  Style := sStyle;
  //ENDIF
end;

{ Return formatted HTML }
function THTMLHeading.AsHTML: string;
begin
  { Check for errors }
  if (heStrictHTML4 in HTMLErrorLevel) and (Alignment <> ahDefault) then
    raise EHTMLError.Create(Format(sDeprecatedAttribute, ['h*']));

  { Generate HTML }
  Result := '<h' + IntToStr(Level) +
    AlignHorizAttribute('align', Alignment) + BaseAttributesAsHTML + '>' +
    EncodeSpecialChars(Text) + ContentsAsHTML +
    '</h' + IntToStr(Level) + '>'#13#10;
end;

{ THTMLText -------------------------------------------------------------------}

{ Initialise - default to normal text }
constructor THTMLText.Create(sText: string; sTagClass: string = ''; sId: string = ''; sStyle: string = '');
//----------------------------------------------------------------------------------------------------------------------
begin
  Create(tsNormal, sText, sTagClass, sId, sStyle);
end;

{ Initialise }
constructor THTMLText.Create(tsStyle: THTMLTextStyle; sText: string; sTagClass: string = ''; sId: string = ''; sStyle: string = '');
begin
  inherited Create;
  FTextStyle := tsStyle;
  FText := sText;
  //IFDEF Delphi 4
  TagClass := sTagClass;
  Id := sId;
  Style := sStyle;
  //ENDIF
end;

{ Return formatted HTML }
function THTMLText.AsHTML: string;
begin
  { Check for errors }
  if (heStrictHTML4 in HTMLErrorLevel) and (TextStyle >= tsUnderlined) then
    raise EHTMLError.Create(Format(sDeprecatedTag, [sHTMLTextStyle[TextStyle]]));

  { Generate HTML }
  if TextStyle = tsNormal then
    Result := ''
  else
    Result := '<' + sHTMLTextStyle[TextStyle] + BaseAttributesAsHTML + '>';
  Result := Result + EncodeSpecialChars(Text) + ContentsAsHTML;
  if TextStyle <> tsNormal then
    Result := Result + '</' + sHTMLTextStyle[TextStyle] + '>';
end;

{ THTMLImage ------------------------------------------------------------------}

{ Initialise }
constructor THTMLImage.Create(sSource, sAlternateText: string; iHeight: THTMLLength = 0; iWidth: THTMLLength = 0; aiAlignment: THTMLAlignmentImage = aiDefault);
begin
  inherited Create;
  FSource := sSource;
  FAlternateText := sAlternateText;
  //IFDEF Delphi 4
  FHeight := iHeight;
  FWidth := iWidth;
  FAlignment := aiAlignment;
  //ENDIF
end;

{ Return formatted HTML }
function THTMLImage.AsHTML: string;
begin
  { Check for errors }
  if (heErrors in HTMLErrorLevel) and (Source = '') then
    raise EHTMLError.Create(sMissingImageSource);
  if (heErrors in HTMLErrorLevel) and (AlternateText = '') then
    raise EHTMLError.Create(sMissingImageAltText);
  if (heStrictHTML4 in HTMLErrorLevel) and (BorderWidth <> 0) then
    raise EHTMLError.Create(Format(sDeprecatedAttribute, ['img']));

  { Generate HTML }
  Result := '<img src="' + Source + '" border=' + IntToStr(BorderWidth) +
    TextAttribute('alt', AlternateText) +
    AlignImageAttribute('align', Alignment) +
    NumericAttribute('width', Width) + NumericAttribute('height', Height) +
    NumericAttribute('hspace', HorizSpace) +
    NumericAttribute('vspace', VertSpace) +
    TextAttribute('usemap', UseMap) + BooleanAttribute(IsMap, 'ismap') +
    BaseAttributesAsHTML + '>';
end;

{ THTMLImageMap ---------------------------------------------------------------}

{ Initialise }
constructor THTMLImageMap.Create(sName: string);
begin
  inherited Create;
  FName := sName;
end;

{ Return formatted HTML }
function THTMLImageMap.AsHTML: string;
begin
  { Check for errors }
  if (heErrors in HTMLErrorLevel) and (Name = '') then
    raise EHTMLError.Create(sMissingImageMapName);

  { Generate HTML }
  Result := #13#10'<map name="' + Name + '"' + BaseAttributesAsHTML + '>'#13#10 +
    ContentsAsHTML + '</map>'#13#10;
end;

{ THTMLImageMapArea -----------------------------------------------------------}

{ Initialise }
constructor THTMLImageMapArea.Create(shShape: THTMLShape; iCoords: array of THTMLPixels; sDestination, sAlternateText: string; sTarget: string = '');
begin
  inherited Create;
  lstCoords := TList.Create;
  FDestination := sDestination;
  FAlternateText := sAlternateText;
  //IFDEF Delphi 4
  FTarget := sTarget;
  //ENDIF
  SetShape(shShape, iCoords);
end;

{ Release resources }
destructor THTMLImageMapArea.Destroy;
begin
  lstCoords.Free;
  inherited Destroy;
end;

{ Set area shape and coordinates }
procedure THTMLImageMapArea.SetShape(shShape: THTMLShape;
  iCoords: array of THTMLPixels);
var
  i, iCount: Integer;
begin
  { Check for errors }
  iCount := High(iCoords) + 1;
  if (heErrors in HTMLErrorLevel) and (((shShape = shRect) and (iCount <> 4)) or
      ((shShape = shCircle) and (iCount <> 3)) or
      ((shShape = shPolygon) and ((iCount < 6) or Odd(iCount)))) then
    raise EHTMLError.Create(Format(sInvalidNumberOfCoords, [sHTMLShape[shShape]]));

  FShape := shShape;
  with lstCoords do
  begin
    Clear;
    for i := 0 to iCount - 1 do
      Add(Pointer(iCoords[i]));
  end;
end;

{ Return formatted HTML }
function THTMLImageMapArea.AsHTML: string;
var
  i: Integer;
  sSep: string;
begin
  { Check for errors }
  if (heErrors in HTMLErrorLevel) and (AlternateText = '') then
    raise EHTMLError.Create(sMissingImageAreaAltText);

  { Generate HTML }
  Result := '<area shape=' + sHTMLShape[Shape];
  if Shape <> shDefault then
  begin
    sSep := ' coords="';
    for i := 0 to lstCoords.Count - 1 do
    begin
      Result := Result + sSep + IntToStr(LongInt(lstCoords[i]));
      sSep := ',';
    end;
    Result := Result + '"';
  end;
  Result := Result + BooleanAttribute((Destination = ''), 'nohref') +
    TextAttribute('href', Destination) + TextAttribute('target', Target) +
    TextAttribute('alt', AlternateText) + BaseAttributesAsHTML + '>'#13#10;
end;

{ THTMLObject -----------------------------------------------------------------}

{ Initialise }
constructor THTMLObject.Create(sClassId: string; sData: string = ''; sName: string = ''; ahAlignment: THTMLAlignmentHoriz = ahDefault; iHeight: THTMLLength = 0; iWidth: THTMLLength = 0);
begin
  inherited Create;
  FClassId := sClassId;
  FData := sData;
  FName := sName;
  //IFDEF Delphi 4
  FAlignment := ahAlignment;
  FHeight := iHeight;
  FWidth := iWidth;
  //ENDIF
end;

{ Return formatted HTML }
function THTMLObject.AsHTML: string;
begin
  { Check for errors }
  if (heErrors in HTMLErrorLevel) and (ClassId = '') and (Data = '') then
    raise EHTMLError.Create(sMissingClassIdData);

  { Generate HTML }
  Result := '<object' + TextAttribute('classid', ClassId) +
    TextAttribute('codebase', CodeBase) + TextAttribute('codetype', CodeType) +
    TextAttribute('data', Data) + TextAttribute('type', DataType) +
    TextAttribute('name', Name) + TextAttribute('standby', StandBy) +
    NumericAttribute('width', Width) + NumericAttribute('height', Height) +
    AlignHorizAttribute('align', Alignment) +
    NumericAttribute('border', BorderWidth) +
    NumericAttribute('hspace', HorizSpace) +
    NumericAttribute('vspace', VertSpace) + TextAttribute('usemap', UseMap) +
    BaseAttributesAsHTML + '>'#13#10 +
    ContentsAsHTML + '</object>'#13#10;
end;

{ THTMLObjectParam ------------------------------------------------------------}

{ Initialise }
constructor THTMLObjectParam.Create(sName, sValue: string; iValueType: THTMLParamTypes = ptData; sContentType: string = '');
begin
  inherited Create;
  FName := sName;
  FValue := sValue;
  //IFDEF Delphi 4
  FValueType := iValueType;
  //ENDIF
end;

{ Return formatted HTML }
function THTMLObjectParam.AsHTML: string;
begin
  { Check for errors }
  if (heErrors in HTMLErrorLevel) and (Name = '') then
    raise EHTMLError.Create(sMissingParameterName);

  { Generate HTML }
  Result := '<param' + TextAttribute('name', Name) +
    TextAttribute('value', Value) +
    TextAttribute('valuetype', sHTMLParamTypes[ValueType]) +
    TextAttribute('type', ContentType) + BaseAttributesAsHTML + '>'#13#10;
end;

{ THTMLList -------------------------------------------------------------------}

{ Initialise }
constructor THTMLList.Create(ltListType: THTMLListType; sTagClass: string = ''; sId: string = ''; sStyle: string = '');
begin
  inherited Create;
  FListType := ltListType;
  FBulletType := lbDefault;
  FNumberScheme := nsDefault;
  //IFDEF Delphi 4
  TagClass := sTagClass;
  Id := sId;
  Style := sStyle;
  //ENDIF
end;

{ Initialise - unordered list }
constructor THTMLList.CreateUnordered(lbBulletType: THTMLListBullet = lbDefault; sTagClass: string = ''; sId: string = ''; sStyle: string = '');
begin
  //FDEF Delphi 4
  Create(ltUnordered, sTagClass, sId, sStyle);
  //ENDIF
  FBulletType := lbBulletType;
end;

{ Initialise - ordered list }
constructor THTMLList.CreateOrdered( nsNumberScheme: THTMLNumberScheme = nsDefault; iStart: THTMLNumber = 0; sTagClass: string = ''; sId: string = ''; sStyle: string = '');
begin
  //IFDEF Delphi 4
  Create(ltOrdered, sTagClass, sId, sStyle);
  //ENDIF
  FNumberScheme := nsNumberScheme;
  FStart := iStart;
end;

{ Return formatted HTML }
function THTMLList.AsHTML: string;
begin
  { Check for errors }
  if (heStrictHTML4 in HTMLErrorLevel) and ((NumberScheme <> nsDefault) or
      (Start <> 0) or (BulletType <> lbDefault)) then
    raise EHTMLError.Create(Format(sDeprecatedAttribute, [sHTMLListType[ListType]]));

  { Generate HTML }
  Result := '<' + sHTMLListType[ListType];
  if ListType = ltOrdered then
    Result := Result + BooleanAttribute((NumberScheme <> nsDefault),
      'type=' + sHTMLNumberScheme[NumberScheme]) +
      NumericAttribute('start', Start)
  else if ListType = ltUnordered then
  begin
    if BulletType <> lbDefault then
      Result := Result + ' type=' + sHTMLListBullet[BulletType];
  end;
  Result := Result + BaseAttributesAsHTML + '>'#13#10 +
    ContentsAsHTML + '</' + sHTMLListType[ListType] + '>'#13#10;
end;

{ THTMLListItem ---------------------------------------------------------------}

{ Initialise }
constructor THTMLListItem.Create(liListItem: THTMLListItemType; sText: string; sTagClass: string = ''; sId: string = ''; sStyle: string = '');
begin
  inherited Create;
  FListItem := liListItem;
  FText := sText;
  FNumberScheme := nsDefault;
  //IFDEF Delphi 4
  TagClass := sTagClass;
  Id := sId;
  Style := sStyle;
  //ENDIF
end;

{ Return formatted HTML }
function THTMLListItem.AsHTML: string;
begin
  { Check for errors }
  if (heStrictHTML4 in HTMLErrorLevel) and
      ((NumberScheme <> nsDefault) or (Value <> 0)) then
    raise EHTMLError.Create(Format(sDeprecatedAttribute, [sHTMLListItem[ListItem]]));

  { Generate HTML }
  Result := '<' + sHTMLListItem[ListItem] +
    BooleanAttribute((NumberScheme <> nsDefault),
      'type=' + sHTMLNumberScheme[NumberScheme]) +
    NumericAttribute('value', Value) + BaseAttributesAsHTML + '>' +
    EncodeSpecialChars(Text) + ContentsAsHTML +
    '</' + sHTMLListItem[ListItem] + '>'#13#10;
end;

{ THTMLAnchor -----------------------------------------------------------------}

{ Initialise }
constructor THTMLAnchor.Create(sDestination, sText: string; sTarget: string = ''; sName: string = '');
begin
  inherited Create;
  FDestination := sDestination;
  Text := sText;
  //IFDEF Delphi 4
  FTarget := sTarget;
  FName := sName;
  //ENDIF
end;

{ Return formatted HTML }
function THTMLAnchor.AsHTML: string;
begin
  { Check for errors }
  if (heErrors in HTMLErrorLevel) and (Destination = '') and (Name = '') then
    raise EHTMLError.Create(sMissingAnchorDetails);

  { Generate HTML }
  Result := '<a' + TextAttribute('href', Destination) +
    TextAttribute('name', Name) + TextAttribute('target', Target) +
    TextAttribute('type', ContentType) + BaseAttributesAsHTML + '>' +
    EncodeSpecialChars(Text) + ContentsAsHTML + '</a>';
end;

{ THTMLHorizRule --------------------------------------------------------------}

{ Initialise }
constructor THTMLHorizRule.Create(iWidth: THTMLLength = 0; ahAlignment: THTMLAlignmentHoriz = ahDefault; iSize: THTMLPixels = 0);
begin
  inherited Create;
  FWidth := iWidth;
  FAlignment := ahAlignment;
  //IFDEF Delphi 4
  FSize := iSize;
  //ENDIF
end;

{ Return formatted HTML }
function THTMLHorizRule.AsHTML: string;
begin
  { Check for errors }
  if (heStrictHTML4 in HTMLErrorLevel) and
      ((Alignment <> ahDefault) or NoShade or (Width <> 0)  or (Size <> 0)) then
    raise EHTMLError.Create(Format(sDeprecatedAttribute, ['hr']));

  { Generate HTML }
  Result := '<hr' + NumericAttribute('size', Size) +
    NumericAttribute('width', Width) +
    AlignHorizAttribute('align', Alignment) +
    BooleanAttribute(NoShade, 'noshade') + BaseAttributesAsHTML + '>'#13#10;
end;

{ THTMLLineBreak --------------------------------------------------------------}

{ Initialise }
constructor THTMLLineBreak.Create(ctClearTo: THTMLClearTo = ctDefault);
begin
  inherited Create;
  FClearTo := ctClearTo;
end;

{ Return formatted HTML }
function THTMLLineBreak.AsHTML: string;
begin
  Result := '<br' + ClearAttribute('clear', ClearTo) +
    BaseAttributesAsHTML + '>'#13#10;
end;

{ THTMLTableBase --------------------------------------------------------------}

{ Initialise }
constructor THTMLTableBase.Create;
begin
  inherited Create;
  FAlignHoriz := thDefault;
  FAlignVert := tvDefault;
  FAlignToChar := #0;
  FBackground := clNone;
  FBorderColour := clNone;
  FBorderHighlight := clNone;
  FBorderShadow := clNone;
end;

{ Format alignment attributes as HTML for inclusion in tag }
function THTMLTableBase.AlignmentsAsHTML: string;
begin
  Result := '';
  if AlignHoriz <> thDefault then
    Result := Result + ' align=' + sHTMLTableAlignHoriz[AlignHoriz];
  if AlignVert <> tvDefault then
    Result := Result + ' valign=' + sHTMLTableAlignVert[AlignVert];
  if AlignToChar in [#32..#255] then
    Result := Result + ' char="' + AlignToChar + '"';
end;

{ Format colour attributes as HTML for inclusion in tag }
function THTMLTableBase.ColoursAsHTML: string;
begin
  { Check for errors }
  if (heStrictHTML4 in HTMLErrorLevel) and ((Background <> clNone) or (BorderColour <> clNone) or
      (BorderHighlight <> clNone) or (BorderShadow <> clNone)) then
  begin
   raise EHTMLError.Create(Format(sDeprecatedAttribute, ['table']));
  end;

  { Generate HTML }
  Result := ColourAttribute('bgcolor', Background) + ColourAttribute('bordercolor', BorderColour) +
            ColourAttribute('bordercolorlight', BorderHighlight) + ColourAttribute('bordercolordark', BorderShadow);
end;

{ THTMLTable ------------------------------------------------------------------}

{ Initialise }
constructor THTMLTable.Create(iWidth: THTMLLength = -100; { 100% } iBorderWidth: THTMLPixels = 2; sCaption: string = ''; acCaptionAlignment: THTMLAlignmentCaption = acDefault);
begin
  inherited Create;
  FWidth := iWidth;
  FBorderWidth := iBorderWidth;
  //IFDEF Delphi 4
  FCaption := sCaption;
  FCaptionAlignment := acCaptionAlignment;
  //ENDIF
end;

{ Return formatted HTML }
function THTMLTable.AsHTML: string;
begin
  { Check for errors }
  if (heStrictHTML4 in HTMLErrorLevel) and (CaptionAlignment <> acDefault) then
    raise EHTMLError.Create(Format(sDeprecatedAttribute, ['caption']));

  { Generate HTML }

  { Generate HTML }
  Result := '<table border=' + IntToStr(BorderWidth) +
    NumericAttribute('width', Width) + TextAttribute('summary', Summary);
  if Frame <> tfVoid then
    Result := Result + ' frame=' + sHTMLTableFrame[Frame];
  if Rules <> trNone then
    Result := Result + ' rules=' + sHTMLTableRule[Rules];
  Result := Result + NumericAttribute('cellspacing', CellSpacing) +
    NumericAttribute('cellpadding', CellPadding) + ColoursAsHTML +
    BaseAttributesAsHTML + '>'#13#10;
  { Include caption tag if present }
  if Caption <> '' then
    Result := Result + '<caption' +
      AlignCaptionAttribute('align', CaptionAlignment) + '>' +
      EncodeSpecialChars(Caption) + '</caption>'#13#10;
  Result := Result + ContentsAsHTML + '</table>'#13#10;
end;

{ THTMLTableRowGroup ----------------------------------------------------------}

{ Initialise }
constructor THTMLTableRowGroup.Create(rgRowGrouping: THTMLTableRowGrouping);
begin
  inherited Create;
  FRowGrouping := rgRowGrouping;
end;

{ Return formatted HTML }
function THTMLTableRowGroup.AsHTML: string;
begin
  Result :='<' + sHTMLTableRowGrouping[RowGrouping] + AlignmentsAsHTML + BaseAttributesAsHTML +
           '>'#13#10 + ContentsAsHTML + '</' + sHTMLTableRowGrouping[RowGrouping] + '>'#13#10;
end;

{ THTMLTableColumnGroup -------------------------------------------------------}

{ Initialise }
constructor THTMLTableColumnGroup.Create(sStyle: string = ''; iSpan: THTMLNumber = 0; iWidth: THTMLLength = 0; sTagClass: string = ''; sId: string = '');
begin
  inherited Create;
  Style := sStyle;
  FSpan := iSpan;
  FWidth := iWidth;
  //IFDEF Delphi 4 }
  TagClass := sTagClass;
  Id := sId;
   //ENDIF
end;

{ Return formatted HTML }
function THTMLTableColumnGroup.AsHTML: string;
begin
  Result := '<colgroup' + NumericAttribute('span', Span) +
    NumericAttribute('width', Width) + AlignmentsAsHTML +
    BaseAttributesAsHTML + '>'#13#10 + ContentsAsHTML + '</colgroup>'#13#10;
end;

{ THTMLTableColumn ------------------------------------------------------------}

{ Initialise }
constructor THTMLTableColumn.Create(sStyle: string = ''; iSpan: THTMLNumber = 0; iWidth: THTMLLength = 0; sTagClass: string = ''; sId: string = '');
begin
  inherited Create;
  Style := sStyle;
  FSpan := iSpan;
  FWidth := iWidth;
  FAlignHoriz := thDefault;
  FAlignVert := tvDefault;
  FAlignToChar := #0;
  //IFDEF Delphi 4
  TagClass := sTagClass;
  Id := sId;
  //$ENDIF
end;

{ Return formatted HTML }
function THTMLTableColumn.AsHTML: string;
begin
  Result := '<col' + NumericAttribute('span', Span) +
    NumericAttribute('width', Width);
  if AlignHoriz <> thDefault then
    Result := Result + sHTMLTableAlignHoriz[AlignHoriz];
  if Alignvert <> tvDefault then
    Result := Result + sHTMLTableAlignvert[AlignVert];
  if AlignToChar in [#32..#255] then
    Result := Result + ' char="' + AlignToChar + '"';
  Result := Result + BaseAttributesAsHTML + '>';
end;

{ THTMLTableRow ---------------------------------------------------------------}

{ Return formatted HTML }
function THTMLTableRow.AsHTML: string;
begin
  Result := '<tr' + AlignmentsAsHTML + ColoursAsHTML + BaseAttributesAsHTML + '>'#13#10 + ContentsAsHTML + '</tr>'#13#10;
end;

{ THTMLTableCellBase ----------------------------------------------------------}

{ Initialise }
constructor THTMLTableCellBase.Create;
begin
  inherited Create;
  ColSpan := 1;
  RowSpan := 1;
end;

{ Format span attributes as HTML for inclusion in tag }
function THTMLTableCellBase.SpansAsHTML: string;
begin
  { Check for errors }
  if (heStrictHTML4 in HTMLErrorLevel) and ((Width <> 0) or (Height <> 0)) then
    raise EHTMLError.Create(Format(sDeprecatedAttribute, ['th/td']));

  { Generate HTML }
  Result := BooleanAttribute(ColSpan <> 1, 'colspan=' + IntToStr(ColSpan)) +
    BooleanAttribute(RowSpan <> 1, 'rowspan=' + IntToStr(RowSpan)) +
    NumericAttribute('width', Width) + NumericAttribute('height', Height);
end;

{ THTMLTableHeading -----------------------------------------------------------}

{ Initialise }
constructor THTMLTableHeading.Create(sText: string; iColSpan: THTMLNumber = 1; iRowSpan: THTMLNumber = 1);
begin
  inherited Create;
  Text := sText;
  //IFDEF Delphi 4
  ColSpan := iColSpan;
  RowSpan := iRowSpan;
  //ENDIF
end;

{ Return formatted HTML }
function THTMLTableHeading.AsHTML: string;
begin
  Result := '<th' + SpansAsHTML + AlignmentsAsHTML + ColoursAsHTML +
    BaseAttributesAsHTML + '>' +
    EncodeSpecialChars(Text) + ContentsAsHTML + '</th>';
end;

{ THTMLTableDetail ------------------------------------------------------------}

{ Initialise }
constructor THTMLTableDetail.Create(sText: string; iColSpan: THTMLNumber = 1; iRowSpan: THTMLNumber = 1);
begin
  inherited Create;
  Text := sText;
  //IFDEF Delphi 4
  ColSpan := iColSpan;
  RowSpan := iRowSpan;
  //ENDIF
end;

function THTMLTableDetail.AsHTML: string;
//----------------------------------------------------------------------------------------------------------------------
//Return formatted HTML
begin
  Result:='<td' + SpansAsHTML + AlignmentsAsHTML + ColoursAsHTML + BaseAttributesAsHTML + '>' +
                  EncodeSpecialChars(Text) + ContentsAsHTML +
          '</td>';
end;

{ THTMLForm -------------------------------------------------------------------}

{ Initialise }
constructor THTMLForm.Create(fmMethod: THTMLFormMethod; sDestination: string; sTarget: string = '');
begin
  inherited Create;
  FDestination := sDestination;
  FMethod := fmMethod;
  FTarget := sTarget;
end;

{ Return formatted HTML }
function THTMLForm.AsHTML: string;
begin
  { Check for errors }
  if (heErrors in HTMLErrorLevel) and (Destination = '') then
    raise EHTMLError.Create(Format(sMissingDestination, ['form']));

  { Generate HTML }
  Result := '<form method=' + sHTMLFormMethod[Method] +
    ' action="' + Destination + '"' + TextAttribute('target', Target) +
    BaseAttributesAsHTML + '>'#13#10 + ContentsAsHTML + '</form>'#13#10;
end;

{ THTMLFieldSet ---------------------------------------------------------------}

{ Initialise }
constructor THTMLFieldSet.Create(sLegend: string; acLegendAlignment: THTMLAlignmentCaption = acDefault);
begin
  inherited Create;
  FLegend := sLegend;
  FLegendAlignment := acLegendAlignment;
end;

{ Return formatted HTML }
function THTMLFieldSet.AsHTML: string;
begin
  { Check for errors }
  if (heStrictHTML4 in HTMLErrorLevel) and (LegendAlignment <> acDefault) then
    raise EHTMLError.Create(Format(sDeprecatedAttribute, ['legend']));

  { Generate HTML }
  Result := '<fieldset' + BaseAttributesAsHTML + '>'#13#10;
  if Legend <> '' then
    Result := Result + '<legend' +
      AlignCaptionAttribute('align', LegendAlignment) + '>' +
      EncodeSpecialChars(Legend) + '</legend>'#13#10;
  Result := Result + ContentsAsHTML + #13#10'</fieldset>'#13#10;
end;

{ THTMLInputField -------------------------------------------------------------}

{ Initialise }
constructor THTMLInputField.Create(ftFieldType: THTMLFieldType; sName: string);
begin
  inherited Create;
  FFieldType := ftFieldType;
  FName := sName;
  FAlignment := ahDefault;
end;

{ Return formatted HTML }
function THTMLInputField.AsHTML: string;
begin
  { Check for errors }
  if (heErrors in HTMLErrorLevel) and (FieldType = ftImage) and (Image = '') then
    raise EHTMLError.Create(sMissingImageSource);
  if (heErrors in HTMLErrorLevel) and (FieldType = ftRadio) and (Value = '') then
    raise EHTMLError.Create(sMissingRadioValue);
  if (heStrictHTML4 in HTMLErrorLevel) and (Alignment <> ahDefault) then
    raise EHTMLError.Create(Format(sDeprecatedAttribute, ['input']));

  { Generate HTML }
  Result := '<input type=' + sHTMLFieldType[FieldType] +
    TextAttribute('name', Name) + TextAttribute('value', Value) +
    BooleanAttribute(Checked, 'checked') + BooleanAttribute(Disabled, 'disabled') +
    BooleanAttribute(ReadOnly, 'readonly') + NumericAttribute('size', Size) +
    NumericAttribute('maxlength', MaxLength) +
    TextAttribute('src', Image) + AlignHorizAttribute('align', Alignment) +
    NumericAttribute('border', BorderWidth) +
    TextAttribute( 'onclick', OnClick ) + //Onclick event
    BaseAttributesAsHTML + '>';
end;

{ THTMLTextField --------------------------------------------------------------}

{ Initialise }
constructor THTMLTextField.Create(sName: string; sValue: string = ''; iSize: THTMLLength = 0; iMaxLength: THTMLNumber = 0);
begin
  inherited Create(ftText, sName);
  Value := sValue;
  Size := iSize;
  MaxLength := iMaxLength;
end;

{ THTMLPasswordField ----------------------------------------------------------}

{ Initialise }
constructor THTMLPasswordField.Create(sName: string; sValue: string = ''; iSize: THTMLLength = 0; iMaxLength: THTMLNumber = 0);
begin
  inherited Create(ftPassword, sName);
  Value := sValue;
  Size := iSize;
  MaxLength := iMaxLength;
end;

{ THTMLCheckboxField ----------------------------------------------------------}

{ Initialise }
constructor THTMLCheckboxField.Create(sName: string; sValue: string = ''; bChecked: Boolean = False);
begin
  inherited Create(ftCheckbox, sName);
  Value := sValue;
  Checked := bChecked;
end;

{ THTMLRadioField -------------------------------------------------------------}

{ Initialise }
constructor THTMLRadioField.Create(sName, sValue: string; bChecked: Boolean = False);
begin
  inherited Create(ftRadio, sName);
  Value := sValue;
  Checked := bChecked;
end;

{ THTMLSubmitField ------------------------------------------------------------}

{ Initialise }
constructor THTMLSubmitField.Create(sName: string = ''; sValue: string = 'Submit');
begin
  inherited Create(ftSubmit, sName);
  Value := sValue;
end;

{ THTMLResetField -------------------------------------------------------------}

{ Initialise }
constructor THTMLResetField.Create(sValue: string = 'Reset');
begin
  inherited Create(ftReset, '');
  Value := sValue;
end;

{ THTMLImageField -------------------------------------------------------------}

{ Initialise }
constructor THTMLImageField.Create(sName, sImage: string; ahAlignment: THTMLAlignmentHoriz = ahDefault; iBorderWidth: THTMLPixels = 0);
begin
  inherited Create(ftImage, sName);
  Image := sImage;
  Alignment := ahAlignment;
  BorderWidth := iBorderWidth;
end;

{ THTMLHiddenField ------------------------------------------------------------}

{ Initialise }
constructor THTMLHiddenField.Create(sName, sValue: string);
begin
  inherited Create(ftHidden, sName);
  Value := sValue;
end;

{ THTMLFileField ------------------------------------------------------------}

{ Initialise }
constructor THTMLFileField.Create(sName: string);
begin
  inherited Create(ftFile, sName);
end;

{ THTMLButtonField ------------------------------------------------------------}

{ Initialise }
constructor THTMLButtonField.Create(sName, sValue: string);
begin
  inherited Create(ftButton, sName);
  Value := sValue;
end;

{ THTMLButton -----------------------------------------------------------------}

{ Initialise }
constructor THTMLButton.Create(btButtonType: THTMLButtonType; sName, sText: string);
begin
  inherited Create;
  FButtonType := btButtonType;
  FName := sName;
  FText := sText;
end;

{ Return formatted HTML }
function THTMLButton.AsHTML: string;
begin
  Result := '<button type=' + sHTMLFieldType[ButtonType] +
    TextAttribute('name', Name) + TextAttribute('value', Value) +
    BooleanAttribute(Disabled, 'disabled') + BaseAttributesAsHTML + '>' +
    EncodeSpecialChars(Text) + ContentsAsHTML + '</button>';
end;

{ THTMLSelectField ------------------------------------------------------------}

{ Initialise }
constructor THTMLSelectField.Create(sName: string; iSize: THTMLNumber = 1; bMultiple: Boolean = False);
begin
  inherited Create;
  FName := sName;
  FSize := iSize;
  FMultiple := bMultiple;
end;

{ Add an item with specified values to the selection }
procedure THTMLSelectField.AddItem(sOption, sValue: string; bSelected: Boolean);
begin
  Add(THTMLSelectOption.Create(sOption, sValue, bSelected));
end;

{ Add a list of items with default values to the selection }
procedure THTMLSelectField.AddItems(slsItems: TStrings);
var
  i: Integer;
begin
  for i := 0 to slsItems.Count - 1 do
    Add(THTMLSelectOption.Create(slsItems[i], '', False));
end;

{ Return formatted HTML }
function THTMLSelectField.AsHTML: string;
begin
  { Check for errors }
  if (heErrors in HTMLErrorLevel) and (Count = 0) then
    raise EHTMLError.Create(sNoOptionsForSelect);

  { Generate HTML }
  Result := '<select' + TextAttribute('name', Name) +
    NumericAttribute('size', Size) + BooleanAttribute(Multiple, 'multiple') +
    BooleanAttribute(Disabled, 'disabled') + BaseAttributesAsHTML + '>' +
    ContentsAsHTML + '</select>';
end;

{ THTMLSelectGroup ------------------------------------------------------------}

{ Initialise }
constructor THTMLSelectGroup.Create(sGroupLabel: string);
begin
  inherited Create;
  FGroupLabel := sGroupLabel;
end;

{ Add an item with specified values to the selection }
procedure THTMLSelectGroup.AddItem(sOption, sValue: string; bSelected: Boolean);
begin
  Add(THTMLSelectOption.Create(sOption, sValue, bSelected));
end;

{ Add a list of items with default values to the selection }
procedure THTMLSelectGroup.AddItems(slsItems: TStrings);
var
  i: Integer;
begin
  for i := 0 to slsItems.Count - 1 do
    Add(THTMLSelectOption.Create(slsItems[i], '', False));
end;

{ Return formatted HTML }
function THTMLSelectGroup.AsHTML: string;
begin
  { Check for errors }
  if (heErrors in HTMLErrorLevel) and (Count = 0) then
    raise EHTMLError.Create(sNoOptionsForSelect);
  if (heErrors in HTMLErrorLevel) and (GroupLabel = '') then
    raise EHTMLError.Create(sMissingSelectGroupLabel);

  { Generate HTML }
  Result := '<optgroup' + TextAttribute('label', GroupLabel) +
    BooleanAttribute(Disabled, 'disabled') + BaseAttributesAsHTML + '>' +
    ContentsAsHTML + '</optgroup>';
end;

{ THTMLSelectOption -----------------------------------------------------------}

{ Initialise }
constructor THTMLSelectOption.Create(sOption: string; sValue: string = '';
  bSelected: Boolean = False);
begin
  inherited Create;
  FOption := sOption;
  FValue := sValue;
  FSelected := bSelected;
end;

{ Return formatted HTML }
function THTMLSelectOption.AsHTML: string;
begin
  Result := '<option' + TextAttribute('value', Value) +
    BooleanAttribute(Selected, 'selected') + 
    BooleanAttribute(Disabled, 'disabled') +
    TextAttribute('label', ShortLabel) + BaseAttributesAsHTML + '>' +
    EncodeSpecialChars(Option) + '</option>';
end;

{ THTMLTextareaField ----------------------------------------------------------}

{ Initialise }
constructor THTMLTextareaField.Create(sName: string; sText: string = ''; iCols: THTMLNumber = 40; iRows: THTMLNumber = 3);
begin
  inherited Create;
  FName := sName;
  FText := sText;
  FCols := iCols;
  FRows := iRows;
end;

{ Return formatted HTML }
function THTMLTextareaField.AsHTML: string;
begin
  { Check for errors }
  if (heErrors in HTMLErrorLevel) and ((Cols = 0) or (Rows = 0)) then
    raise EHTMLError.Create(sMissingTextAreaColsRows);

  { Generate HTML }
  Result := '<textarea' + TextAttribute('name', Name) +
    NumericAttribute('cols', Cols) + NumericAttribute('rows', Rows) +
    BooleanAttribute(Disabled, 'disabled') +
    BooleanAttribute(ReadOnly, 'readonly') + BaseAttributesAsHTML + '>' +
    EncodeSpecialChars(Text) + '</textarea>';
end;

{ THTMLLabel -----------------------------------------------------------------}

{ Initialise }
constructor THTMLLabel.Create(sForId, sText: string);
begin
  inherited Create;
  FForId := sForId;
  FText := sText;
end;

{ Return formatted HTML }
function THTMLLabel.AsHTML: string;
begin
  Result := '<label' + TextAttribute('for', ForId) +
    BaseAttributesAsHTML + '>' + EncodeSpecialChars(Text) +
    ContentsAsHTML + '</label>';
end;

{ THTMLStream -----------------------------------------------------------------}

{ Initialise }
constructor THTMLStream.Create(stmStream: TStream);
begin
  inherited Create;
  FStream := stmStream;
  FOwnStream := True;
end;

{ Release resources }
destructor THTMLStream.Destroy;
begin
  if OwnStream then
    FStream.Free;
  inherited Destroy;
end;

{ Break up string of attributes, and optional values, into a string list }
procedure ExtractParams(sAttributes: string; slsParams: TStrings);
var
  sAttr1, sAttr2: string;
  cDelim: Char;
  bFoundEqual: Boolean;
  iPos: Integer;

  { Extract and compile characters up to the specified set of characters }
  function ExtractTo(cDelims: TCharSet): string;
  begin
    Result := '';
    while (iPos <= Length(sAttributes)) and not (sAttributes[iPos] in cDelims) do
    begin
      Result := Result + sAttributes[iPos];
      Inc(iPos);
    end;
  end;

begin
  if sAttributes = '' then
    Exit;

  sAttr1 := '';
  sAttr2 := '';
  bFoundEqual := False;
  iPos := 1;
  while True do
  begin
    ExtractTo([#33..#255]);             { Skip blanks }
    if iPos > Length(sAttributes) then  { End of string }
      Break;
    case sAttributes[iPos] of
      '=':  begin                       { Name = value }
              bFoundEqual := True;
              Inc(iPos);
            end;
      '"', '''':                        { Value delimited by quotes }
            begin
              cDelim := sAttributes[iPos];
              Inc(iPos);
              sAttr2 := ExtractTo([cDelim]);
              Inc(iPos);
              if sAttr2 = '' then
                sAttr2 := ' ';
            end;
      else                              { Name or value }
            sAttr2 := sAttr2 + ExtractTo([' ', '=']);
    end;
    if sAttr1 = '' then    { Found first attribute - name }
    begin
      sAttr1 := sAttr2;
      sAttr2 := '';
    end;
    if sAttr2 <> '' then   { Found second attribute - name or value }
    begin
      if bFoundEqual then  { Name = value }
      begin
        slsParams.Values[UpperCase(sAttr1)] := sAttr2;
        sAttr1 := '';
        sAttr2 := '';
        bFoundEqual := False;
      end
      else                 { Name only }
      begin
        slsParams.Values[UpperCase(sAttr1)] := 'Y';
        sAttr1 := sAttr2;
        sAttr2 := '';
      end;
    end;
  end;
  if sAttr1 <> '' then     { One more at the end }
    slsParams.Values[UpperCase(sAttr1)] := 'Y';
end;

{ Return formatted HTML }
function THTMLStream.AsHTML: string;
var
  psrCopy: TCopyParser;
  stmOut: TStringStream;
  sParam, sReplace, sToken: string;
  slsParams: TStringList;
  hst: THTMLStream;
begin
  stmOut := TStringStream.Create('');
  try
    psrCopy := TCopyParser.Create(FStream, stmOut);
    with psrCopy do
    try
      { Parse HTML stream looking for replacement tags: <#xyz> }
      while Token <> toEOF do
      begin
        while not (Token in [toEof, '<']) do
        begin
          CopyTokenToOutput;
          SkipToken(True);
        end;
        if Token = '<' then               { Found tag }
        begin
          if SkipToken(False) = '#' then  { Found substitution tag }
          begin
            SkipToken(False);
            sToken := TokenString;
            sParam := Trim(SkipToToken('>'));
            if sToken = '^' then begin         { Found file inclusion }
              hst := THTMLStream.Create(TFileStream.Create(sParam, fmOpenRead));
              try
                { Parse this new file in the same way }
                hst.OnHTMLTag := OnHTMLTag;
                stmOut.WriteString(hst.AsHTML);
              finally
                hst.Free;
              end
            end else begin                          { Substitute for tag }
              slsParams := TStringList.Create;
              try
                ExtractParams(sParam, slsParams);
                sReplace := HandleTag(sToken, slsParams);
                if sReplace = '' then
                  sReplace := HandleCommonTag(sToken, slsParams);
                stmOut.WriteString(sReplace);
              finally
                slsParams.Free;
              end;
            end;
            SkipToken(True);
          end
          else                            { Not a substitution tag }
          begin
            stmOut.WriteString('<');
            CopyTokenToOutput;
            SkipToken(True);
          end;
        end;
      end;
    finally
      psrCopy.Free;
    end;
    Result := stmOut.DataString;
  finally
    stmOut.Free;
  end;
end;

var
  sTagSymbols: array[THTMLTag] of string =
    ('', 'LINK', 'IMAGE', 'TABLE', 'IMAGEMAP', 'OBJECT', 'EMBED');

{ Find tag type and call user event to process }
function THTMLStream.HandleTag(const sTag: string; slsParams: TStrings): string;
var
  iTag: THTMLTag;
begin
  for iTag := High(THTMLTag) downto Low(THTMLTag) do
    if (iTag = tgCustom) or (CompareText(sTagSymbols[iTag], sTag) = 0) then
      Break;
  Result := '';
  DoTagEvent(iTag, sTag, slsParams, Result);
end;

{ Call user event to process }
procedure THTMLStream.DoTagEvent(iTag: THTMLTag; const sTag: string;
  slsParams: TStrings; var sReplace: string);
begin
  if Assigned(FOnHTMLTag) then
    FOnHTMLTag(Self, iTag, sTag, slsParams, sReplace);
end;

{ Handle common tags here }
function THTMLStream.HandleCommonTag(const sTag: string; slsParams: TStrings): string;
var
  sFormat: string;
begin
  Result := '';
  if LowerCase(sTag) = sTimeSubstitute then
  begin
    sFormat := slsParams.Values[sTimeFormat];
    if sFormat = '' then
      sFormat := 'c';
    Result := FormatDateTime(sFormat, Now);
  end;
end;

procedure THTMLContainer.Insert(Index: integer; objHTML: TObject);
//----------------------------------------------------------------------------------------------------------------------
var
  intHTML: IHTMLProducer;
begin
   if not objHTML.GetInterface(IHTMLProducer, intHTML) then begin
       raise EHTMLError.Create(sNotHTMLProducer);
   end;
   lstHTML.Insert(Index, objHTML);
end;

initialization
  { Set initial error level }
  HTMLErrorLevel := [heErrors];
end.
