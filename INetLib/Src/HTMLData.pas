{$IFDEF HTMLData}
{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I INetLib.inc}
unit HTMLData;

{
  This unit implements a component that allows the contents of
  database tables to be generated into HTML tables.
  Written by Keith Wood - 22 Jun 1996.

  Version 1.1 - 30 Oct 1996
  Added ShowNulls property to display empty frame for nulls
}

interface

uses
	SysUtils, Windows, Messages, Classes, Graphics, Controls, Forms, Dialogs, DB, DBTables, HTMLWrtr;

type
	THTMLRowEvent = procedure(Sender: TObject; var ahAlignHoriz: THTMLAlignHoriz; var avAlignVert: THTMLAlignVert;
		var clrBackground, clrBorder, clrBorderLight, clrBorderDark: TColor) of object;

	THTMLCellEvent = procedure(Sender: TObject; fField: TField; var ahAlignHoriz: THTMLAlignHoriz; var avAlignVert: THTMLAlignVert;
		var clrBackground, clrBorder, clrBorderLight, clrBorderDark: TColor) of object;

	THTMLDataSource = class(TDataSource)
	private
		{ Private declarations }
		FHTMLWriter:                THTMLWriter;
		FBorder:                    Byte;
		FWidth:                     Integer;
		FCellSpacing, FCellPadding: Byte;
		FAlignHoriz:                THTMLAlignHoriz;
		FAlignVert:                 THTMLAlignVert;
		FColourBackground, FColourBorder, FColourBorderLight, FColourBorderDark: TColor;
		FHeaders: Boolean;
		FHeaderBackground, FHeaderBorder, FHeaderBorderLight, FHeaderBorderDark: TColor;
		FCaption:           string;
		FCaptionAlignHoriz: THTMLAlignHoriz;
		FCaptionAlignVert:  THTMLAlignVert;
		FUseFieldAlign:     Boolean;
		FLinkField:         TField;
		FLinkTarget:        TField;
		FShowNulls:         Boolean;
		FOnRowShow:         THTMLRowEvent;
		FOnCellShow:        THTMLCellEvent;
		FVersion:           string;
	protected
		{ Protected declarations }
	public
		{ Public declarations }
		constructor Create(AOwner: TComponent); override;
		property Version: string read FVersion; { Read-only }
		procedure GenerateHTML;
	published
		{ Published declarations }
		property HTMLWriter:        THTMLWriter read FHTMLWriter write FHTMLWriter;
		property Border:            Byte read FBorder write FBorder default 2;
		property Width:             Integer read FWidth write FWidth;
		property CellSpacing:       Byte read FCellSpacing write FCellSpacing default 0;
		property CellPadding:       Byte read FCellPadding write FCellPadding default 0;
		property AlignHoriz:        THTMLAlignHoriz read FAlignHoriz write FAlignHoriz default ahDefault;
		property AlignVert:         THTMLAlignVert read FAlignVert write FAlignVert default avDefault;
		property ColourBackground:  TColor read FColourBackground write FColourBackground default clDefault;
		property ColourBorder:      TColor read FColourBorder write FColourBorder default clDefault;
		property ColourBorderLight: TColor read FColourBorderLight write FColourBorderLight default clDefault;
		property ColourBorderDark:  TColor read FColourBorderDark write FColourBorderDark default clDefault;
		property Headers:           Boolean read FHeaders write FHeaders default True;
		property HeaderBackground:  TColor read FHeaderBackground write FHeaderBackground default clDefault;
		property HeaderBorder:      TColor read FHeaderBorder write FHeaderBorder default clDefault;
		property HeaderBorderLight: TColor read FHeaderBorderLight write FHeaderBorderLight default clDefault;
		property HeaderBorderDark:  TColor read FHeaderBorderDark write FHeaderBorderDark default clDefault;
		property Caption:           string read FCaption write FCaption;
		property CaptionAlignHoriz: THTMLAlignHoriz read FCaptionAlignHoriz write FCaptionAlignHoriz default ahDefault;
		property CaptionAlignVert:  THTMLAlignVert read FCaptionAlignVert write FCaptionAlignVert default avDefault;
		property UseFieldAlign:     Boolean read FUseFieldAlign write FUseFieldAlign default True;
		property LinkField:         TField read FLinkField write FLinkField;
		property LinkTarget:        TField read FLinkTarget write FLinkTarget;
		property ShowNulls:         Boolean read FShowNulls write FShowNulls default True;
		{ Eventos }
		property OnRowShow:  THTMLRowEvent read FOnRowShow write FOnRowShow;
		property OnCellShow: THTMLCellEvent read FOnCellShow write FOnCellShow;
	end;

	EHTMLDataSource = class(EHTMLError);

procedure Register;

implementation

const
	HTMLDataSourceVersion = '1.1';

	{ Register the THTMLDataSource component with Delphi }
procedure Register;
begin
	RegisterComponents('InetLib', [THTMLDataSource]);
end;

{ Create HTML table object }
constructor THTMLDataSource.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);

	{ Initialise default values for internal fields }
	FBorder := 2;
	FWidth := Percent(100);
	FCellSpacing := 0;
	FCellPadding := 0;
	FAlignHoriz := ahDefault;
	FAlignVert := avDefault;
	FColourBackground := clDefault;
	FColourBorder := clDefault;
	FColourBorderLight := clDefault;
	FColourBorderDark := clDefault;
	FHeaders := True;
	FHeaderBackground := clDefault;
	FHeaderBorder := clDefault;
	FHeaderBorderLight := clDefault;
	FHeaderBorderDark := clDefault;
	FCaptionAlignHoriz := ahDefault;
	FCaptionAlignVert := avDefault;
	FUseFieldAlign := True;
	FShowNulls := True;
end;

{ Generate HTML to display the table }
procedure THTMLDataSource.GenerateHTML;
const
	ahAlignments: array [TAlignment] of THTMLAlignHoriz = (ahLeft, ahRight, ahCentre);
var
	i, iCount:    Integer;
	ahAlignHoriz: THTMLAlignHoriz;
	avAlignVert:  THTMLAlignVert;
	clrBackground, clrBorder, clrBorderLight, clrBorderDark: TColor;
	sCell:   string;
	slMemo:  TStringList;
	bmkSave: TBookmark;
begin
	{ Check that HTMLWriter is set }
	if not Assigned(FHTMLWriter) then begin
		raise EHTMLDataSource.Create(tcTable, 'No HTMLWriter assigned');
	end;

	{ Check that DataSet is set }
	if DataSet = nil then begin
		raise EHTMLDataSource.Create(tcTable, 'No DataSet assigned');
	end;

	{ Check linked fields }
	if (LinkField <> nil) or (LinkTarget <> nil) then begin
		if LinkField = nil then
			raise EHTMLDataSource.Create(tcTable, 'Missing linked field name');
		if not LinkField.Visible then
			raise EHTMLDataSource.Create(tcTable, 'Linked field is not visible');
		if LinkTarget = nil then
			raise EHTMLDataSource.Create(tcTable, 'Missing link target field name');
	end;

	with DataSet do begin
		{ Determine whether table has visible fields and records }
		iCount := 0;
		for i := 0 to FieldCount - 1 do
			if Fields[i].Visible then
				Inc(iCount);
		if iCount = 0 then
			raise EHTMLDataSource.Create(tcTable, 'No fields in this dataset are visible');
		if RecordCount = 0 then
			raise EHTMLDataSource.Create(tcTable, 'There are no records in this dataset');

		{ Dump table to HTML }
		with HTMLWriter do begin
			TableStartParams(Border, Width, CellSpacing, CellPadding, ColourBackground, ColourBorder, ColourBorderLight,
				ColourBorderDark, Caption, CaptionAlignHoriz, CaptionAlignVert);

			{ Write headers }
			if Headers then begin
				TableRowStartParams(AlignHoriz, AlignVert, HeaderBackground, HeaderBorder, HeaderBorderLight, HeaderBorderDark);
				for i := 0 to FieldCount - 1 do
					if Fields[i].Visible then begin
						{$IFDEF WIN32}
						sCell := Fields[i].DisplayName;
						{$ELSE}
						sCell := Fields[i].DisplayName^;
						{$ENDIF}
						if UseFieldAlign then
							TableHeadingParams(FormatEscapeText(sCell), 0, 0, ahAlignments[Fields[i].Alignment], avDefault,
								clDefault, clDefault, clDefault, clDefault)
						else
							TableHeading(FormatEscapeText(sCell));
					end;
				TableRowEnd;
			end;

			{ Don't update screen while processing }
			DisableControls;
			{ Save data set position }
			bmkSave := GetBookmark;

			{ Create temporary area for memo fields }
			slMemo := TStringList.Create;

			{ Write contents of rows }
			try
				First;
				while not EOF do { Process all rows }
				begin
					{ Check row alignment and colours }
					ahAlignHoriz := ahDefault;
					avAlignVert := avDefault;
					clrBackground := clDefault;
					clrBorder := clDefault;
					clrBorderLight := clDefault;
					clrBorderDark := clDefault;
					if Assigned(FOnRowShow) then
						OnRowShow(Self, ahAlignHoriz, avAlignVert, clrBackground, clrBorder, clrBorderLight, clrBorderDark);
					{ And start the row }
					TableRowStartParams(ahAlignHoriz, avAlignVert, clrBackground, clrBorder, clrBorderLight, clrBorderDark);

					{ Display each visible field }
					for i := 0 to FieldCount - 1 do
						if Fields[i].Visible then begin
							{ Check column alignment and colours - default to row values }
							if UseFieldAlign then
								ahAlignHoriz := ahAlignments[Fields[i].Alignment]
							else
								ahAlignHoriz := ahDefault;
							avAlignVert := avDefault;
							clrBackground := clDefault;
							clrBorder := clDefault;
							clrBorderLight := clDefault;
							clrBorderDark := clDefault;
							if Assigned(FOnCellShow) then
								OnCellShow(Self, Fields[i], ahAlignHoriz, avAlignVert, clrBackground, clrBorder, clrBorderLight,
									clrBorderDark);

							{ And display the field }
							if Fields[i] is TMemoField then begin { Add all the lines }
								slMemo.Assign(TMemoField(Fields[i]));
								TableCellStartParams(0, 0, 0, ahAlignHoriz, avAlignVert, clrBackground, clrBorder, clrBorderLight,
									clrBorderDark);
								if slMemo.Count = 0 then begin{ Null field }
									if ShowNulls then
										SpecialChar(scNBSpace);
								end
								else
									for iCount := 0 to slMemo.Count - 1 do
										EscapeText(slMemo[iCount] + ' ');
								TableCellEnd;
							end else begin { Add text representation of the field }
								if Fields[i] = LinkField then
									sCell := FormatLink(LinkTarget.AsString, '', FormatEscapeText(Fields[i].DisplayText))
								else
									sCell := FormatEscapeText(Fields[i].DisplayText);
								if ShowNulls and ((sCell = '') or (sCell = ' ')) then { Null field }
									sCell := FormatSpecialChar(scNBSpace);
								TableCellParams(sCell, 0, 0, 0, ahAlignHoriz, avAlignVert, clrBackground, clrBorder, clrBorderLight,
									clrBorderDark);
							end;
						end;

					{ Finish the row }
					TableRowEnd;
					Next;
				end;
			finally
				{ Return to original position }
				GotoBookmark(bmkSave);
				FreeBookmark(bmkSave);
				{ Update screen again }
				EnableControls;
				{ Finish off the HTML table }
				TableEnd;
				{ Release string list resources }
				slMemo.Free;
			end;
		end;
	end;
end;

end.
