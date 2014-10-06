{$IFDEF CheckCombo }
{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I ECLib.inc}
unit CheckCombo;

{ {
  ///    TCheckedComboBox ver 1.3
  ///    tested on D3,D4,D5
  ///    ComboBox with CheckListBox
  ///    When you check/uncheck an item this is added/removed in the visual part of combo.
  ///    It has also a popup with Select all, unSelect all items with other additions
  ///    -----------------------------------------------------------------
  ///    This component was created 20-Deceber 1999 by Tsourinakis Antonis
  ///    Can be freely used and distributed in commercial and
  ///    private environments. You are free to copy and modify the source code.
  ///    If you want I would like this notice to be provided as is.
  ///    -----------------------------------------------------------------
  ///    feel free to contact me:
  ///    tsoyran@otenet.gr
  ///    -----------------------------------------------------------------
  ///    special thanks to Jan Verhoeven
  ///                    email  : jan1.verhoeven@wxs.nl
  ///                    website: http://members.xoom.com/JanVee/jfdelphi.htm
  ///                    for his help
  ///    and to any other component creator from which a get ideas
  ///    -----------------------------------------------------------------
  ///    History
  ///
  ///    Ver 1.3 2000/4/8
  ///
  ///        Special Properties
  ///            CapSelectAll         The caption on popupmenu item for Check All
  ///            -----------------------------------------------------------------
  ///            CapDeSelectAll     The caption on popupmenu item for UnCheck All
  ///            -----------------------------------------------------------------
  ///            NotFocusColor     The color when the component has no focus
  ///            -----------------------------------------------------------------
  ///            Columns                    Like the columns of common check box
  ///            -----------------------------------------------------------------
  ///            DropDownLines     The number of dropDown lines
  ///                                            between    MINDROPLINES and MAXDROPLINES;
  ///                                            They are autoarranged if the Columns are>1
  ///            -----------------------------------------------------------------
  ///            Checked[Index: Integer]
  ///                                            you can traverse this array to have if an
  ///                                            item is checked or not
  ///            -----------------------------------------------------------------
  ///            QuoteStyle            of TCHBQuoteStyle = (qsNone,qsSingle,qsDouble) is the
  ///                                            type with which you can set the format of the selected
  ///                                            options in text format.
  ///            -----------------------------------------------------------------
  ///        Special Methods
  ///            procedure     Clear;
  ///            -----------------------------------------------------------------
  ///            procedure     SetUnCheckedAll( Sender: TObject );
  ///            procedure     SetCheckedAll(  Sender: TObject  );
  ///            procedure     SetChecked(Index: Integer; Checked: Boolean);
  ///            -----------------------------------------------------------------
  ///            function      CheckedCount: integer
  ///                                        Returns the number of checked items
  ///            function      IsChecked ( Index: integer ) : boolean;
  ///                                        Returns True if the Item[index] is checked
  ///            -----------------------------------------------------------------
  ///            function         GetText : string;
  ///                                        As the component has no Caption this is the
  ///                                        text with all the choises separated by comma
  ///                                        in format depended by the value of QuoteStyle
  ///                                        property (see history ver 1.3)
  ///
  ///    -----------------------------------------------------------------
  ///
  ///        Ver 1.1 2000/1/16
  ///
  ///        changes prompted by Kyriakos Tasos
  ///
  ///        - corrections
  ///                            onenter events
  ///                            onexit  events
  ///        - additions
  ///                            onchange event
  ///                            CheckedCount integer
  ///                            The dropdownlistbox closes with the ESC character
  ///        - changes
  ///                            the internal glyph image name
  ///        -----------------------------------------------------------------
  ///        Ver 1.2 2000/1/31
  ///
  ///        changes prompted by Amilcar Dornelles da Costa Leite"
  ///
  ///        - corrections
  ///                            SetChecked(Index: Integer; Checked: Boolean);
  ///                            is now working and by code
  ///        -----------------------------------------------------------------
  ///        Ver 1.3 2000/4/8
  ///
  ///        changes prompted by Jayan Chandrasekhar, Riyadh, Saudi Arabia"
  ///
  ///        -additions
  ///                    property QuoteStyle :qsNone,qsSingle,qsDouble;
  ///
  ///                    so if the Selected values are
  ///                             Germany,UK,USA,Russia
  ///                    the GetText function returns
  ///
  ///                        case qsNone         -> Germany,UK,USA,Russia
  ///                        case qsSingle        -> 'Germany','UK','USA','Russia'
  ///                        case qsDouble        -> "Germany","UK","USA","Russia"
  ///
  ///                    so you can use it, as Jayan noted. in SQL in
  ///                        the SELECT .. IN clause
  ///                        e.g.
  ///                                SELECT NationID, Nation
  ///                                FROM Country
  ///                                WHERE Nation In ( "Germany", "UK", "USA", "Russia" )
  ///        -----------------------------------------
  ///        Ver 1.4 2000/12/26
  ///            corrected bug prompted by "Daniel Azkona Coya"
}

interface

uses
	Windows, Messages, SysUtils, Buttons, Classes, Graphics, Controls, Forms, Dialogs,
	StdCtrls, Menus, CheckLst, extctrls;

type

	TCHBQuoteStyle = (qsNone, qsSingle, qsDouble);
	{ {added 2000/04/08 }

	TCheckedComboBox = class(TCustomPanel)
	private
		FCapSelAll, FCapDeselAll: string;
		FEdit                   : TEdit;
		FButton                 : TSpeedButton;
		FItems                  : TStrings;
		FPrivForm               : TForm;
		FListBox                : TCheckListBox;
		FPopup                  : TPopupMenu;
		FSelectAll              : TMenuItem;
		FDeSelectAll            : TMenuItem;
		FNotFocusColor          : TColor;
		FSorted                 : Boolean;
		FQuoteStyle             : TCHBQuoteStyle; //added 2000/04/08
		FCheckedCount           : integer;
		FColumns                : integer;
		FDropDownLines          : integer;
		FOnChange               : TNotifyEvent;
		FOnEnter                : TNotifyEvent;
		FOnExit                 : TNotifyEvent;
		procedure SetItems(AItems: TStrings);
		procedure ToggleOnOff(Sender: TObject);
		procedure KeyListBox(Sender: TObject; var Key: Word; Shift: TShiftState);
		procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
		procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
		procedure WMSize(var Message: TWMSize); message WM_SIZE;
		procedure ShowCheckList(Sender: TObject);
		procedure CloseCheckList(Sender: TObject);
		procedure ItemsChange(Sender: TObject);
		procedure SetSorted(Value: Boolean);
		procedure AutoSize;
		procedure AdjustHeight;
		procedure EditOnEnter(Sender: TObject);
		procedure EditOnExit(Sender: TObject);
		procedure SetNotFocusColor(Value: TColor);
		procedure SetColumns(Value: integer);
		procedure SetChecked(Index: integer; Checked: Boolean);
		procedure SetDropDownLines(Value: integer);
		function GetChecked(Index: integer): Boolean;
		procedure Change;
	protected
		procedure CreateWnd; override;
	public
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
		procedure Clear;
		procedure SetUnCheckedAll(Sender: TObject);
		procedure SetCheckedAll(Sender: TObject);
		function IsChecked(Index: integer): Boolean;
		function GetText: string;
		property Checked[index: integer]: Boolean read GetChecked write SetChecked;
		property CheckedCount: integer read FCheckedCount;
	published
		property Items         : TStrings read FItems write SetItems;
		property CapSelectAll  : string read FCapSelAll write FCapSelAll;
		property CapDeSelectAll: string read FCapDeselAll write FCapDeselAll;
		property NotFocusColor : TColor read FNotFocusColor write SetNotFocusColor;
		property Sorted        : Boolean read FSorted write SetSorted default FALSE;
		property QuoteStyle    : TCHBQuoteStyle read FQuoteStyle write FQuoteStyle default qsNone; //added 2000/04/08
		property Columns       : integer read FColumns write SetColumns default 0;
		property DropDownLines : integer read FDropDownLines write SetDropDownLines default 6;
		//events
		property OnChange: TNotifyEvent read FOnChange write FOnChange;
		property OnEnter : TNotifyEvent read FOnEnter write FOnEnter;
		property OnExit  : TNotifyEvent read FOnExit write FOnExit;
		//from panel
		property Ctl3D;
		property Cursor;
		property Enabled;
		property Font;
		property ParentColor;
		property ParentCtl3D;
		property ParentFont;
		property ParentShowHint;
		property ShowHint;
		property TabOrder;
		property TabStop;
		property Visible;
		property OnClick;
		property OnMouseDown;
		property OnMouseMove;
		property OnMouseUp;
		property OnResize;
		property OnStartDrag;
	end;

implementation

//Include the images resources - By Roger
{$R *.R32}

const
	Delimit      = ',';
	MAXSELLENGTH = 256;
	MINDROPLINES = 6;
	MAXDROPLINES = 10;

resourcestring
	sFCapSelAll = '&Selecionar todos';
	sFCapDeselAll = '&Deselecionar todos';
	sNoMoreLength = 'Sem espaço para área de seleção';

	{ TCheckedComboBox }
constructor TCheckedComboBox.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
	FDropDownLines := MINDROPLINES;
	FColumns       := 0;
	FQuoteStyle    := qsNone; //added 2000/04/08
	FCheckedCount  := 0;
	FNotFocusColor := clWhite;
	Caption        := '';
	FCapSelAll     := sFCapSelAll;
	FCapDeselAll   := sFCapDeselAll;
	BevelOuter     := bvLowered;
	Height         := 24;
	Width          := 121;

	FItems                       := TStringList.Create;
	TStringList(FItems).OnChange := ItemsChange;

	FEdit             := TEdit.Create(Self);
	FEdit.Parent      := Self;
	FEdit.ParentColor := FALSE;
	FEdit.color       := clWhite;
	FEdit.ReadOnly    := TRUE;

	FButton := TSpeedButton.Create(Self);
	with FButton do begin
		Glyph.LoadFromResourceName(HInstance, 'BTNCHECKPOPUP');
		Parent         := Self;
		FButton.Width  := 16;
		FButton.Height := Self.Height - 2;
		FButton.Top    := Self.Top + 2;
		NumGlyphs      := 1;
		Parent         := Self;
		Layout         := blGlyphRight;
		OnClick        := ShowCheckList;
	end;

	FEdit.Text    := '';
	FEdit.OnEnter := EditOnEnter;
	FEdit.OnExit  := EditOnExit;

	//Create a form with its contents
	FPrivForm       := TForm.Create(Self);
	FPrivForm.color := clWindow;

	//Create CheckListBox
	FListBox              := TCheckListBox.Create(FPrivForm);
	FListBox.Parent       := FPrivForm;
	FListBox.Ctl3D        := FALSE;
	FListBox.Columns      := FColumns;
	FListBox.Align        := alClient;
	FListBox.OnClickCheck := ToggleOnOff;
	FListBox.OnKeyDown    := KeyListBox;
	//Create PopUp
	FPopup               := TPopupMenu.Create(FListBox);
	FSelectAll           := TMenuItem.Create(FPopup);
	FSelectAll.Caption   := FCapSelAll;
	FDeSelectAll         := TMenuItem.Create(FPopup);
	FDeSelectAll.Caption := FCapDeselAll;
	FPopup.Items.Insert(0, FSelectAll);
	FPopup.Items.Insert(1, FDeSelectAll);
	FSelectAll.OnClick   := SetCheckedAll;
	FDeSelectAll.OnClick := SetUnCheckedAll;
	FListBox.PopupMenu   := FPopup;
end;

destructor TCheckedComboBox.Destroy;
begin
	FEdit.free;
	FSelectAll.free;
	FDeSelectAll.free;
	FPopup.free;
	FButton.free;
	FListBox.free;
	FItems.free;
	FPrivForm.free;
	inherited Destroy;
end;

procedure TCheckedComboBox.ShowCheckList(Sender: TObject);
var
	ScreenPoint: TPoint;
begin
	if FButton.tag = 1 then begin //Jan Verhoeven
		FButton.tag := 0;
		exit;
	end;
	Click;
	if FColumns > 1 then begin
		FDropDownLines := FListBox.Items.Count div FColumns + 1;
	end;
	if FDropDownLines < MINDROPLINES then begin
		FDropDownLines := MINDROPLINES;
	end;
	if FDropDownLines > MAXDROPLINES then begin
		FDropDownLines := MAXDROPLINES;
	end;

	//Assign Form coordinate and show
	ScreenPoint          := Parent.ClientToScreen(Point(Self.Left, Self.Top + Self.Height));
	FSelectAll.Caption   := FCapSelAll;
	FDeSelectAll.Caption := FCapDeselAll;
	with FPrivForm do begin
		Font         := Self.Font;
		Left         := ScreenPoint.X;
		Top          := ScreenPoint.Y;
		Width        := Self.Width;
		Height       := (FDropDownLines * FListBox.ItemHeight + 4 { FEdit.Height });
		BorderStyle  := bsNone;
		OnDeactivate := CloseCheckList;
	end;
	if FPrivForm.Height + ScreenPoint.Y > Screen.Height - 20 then begin
		FPrivForm.Top := ScreenPoint.Y - FPrivForm.Height - Self.Height;
	end;
	FPrivForm.Show;
end;

procedure TCheckedComboBox.CloseCheckList(Sender: TObject);
var
	pt: TPoint;
begin
	//code added by Jan Verhoeven
	//check if the mouse is over the combobox button
	//pt:=mouse.CursorPos;
	//this doesn't work on delphi 3
	GetCursorPos(pt);
	pt := FButton.ScreenToClient(pt);
	with FButton do begin
		if (pt.X > 0) and (pt.X < Width) and (pt.Y > 0) and (pt.Y < Height) then begin
			tag := 1;
		end else begin
			tag := 0;
		end;
	end;
	FPrivForm.Close;
end;

function PartExist(const part, source: string): Boolean;
{ {
  exanines if string (part) exist in string (source) where source is in format part1[,part2]
}
var
	m           : integer;
	temp1, temp2: string;
begin
	temp1  := Copy(source, 1, MAXSELLENGTH);
	Result := part = temp1;
	while not Result do begin
		m := Pos(Delimit, temp1);
		if m > 0 then begin
			temp2 := Copy(temp1, 1, m - 1);
		end else begin
			temp2 := temp1;
		end;
		Result := part = temp2;
		if (Result) or (m = 0) then begin
			break;
		end;
		temp1 := Copy(temp1, m + 1, MAXSELLENGTH);
	end;
end;

function RemovePart(const part, source: string): string;
{ {
  removes a string (part) from another string (source)
  when source is in format part1[,part2]
}
var
	lp, p : integer;
	s1, s2: string;
begin
	Result := source;
	s1     := Delimit + part + Delimit;
	s2     := Delimit + source + Delimit;
	p      := Pos(s1, s2);
	if p > 0 then begin
		lp := Length(part);
		if p = 1 then begin
			Result := Copy(source, p + lp + 1, MAXSELLENGTH);
		end else begin
			Result := Copy(s2, 2, p - 1) + Copy(s2, p + lp + 2, MAXSELLENGTH);
			Result := Copy(Result, 1, Length(Result) - 1);
		end;
	end;
end;

function Add(const sub: string; var str: string): Boolean;
begin
	Result := FALSE;
	if Length(str) + Length(sub) + 1 >= MAXSELLENGTH then begin
		ShowMessage(sNoMoreLength);
		exit;
	end;
	if str = '' then begin
		str    := sub;
		Result := TRUE;
	end else if not PartExist(sub, str) then begin
		str    := str + Delimit + sub;
		Result := TRUE;
	end;
end;

function Remove(const sub: string; var str: string): Boolean;
var
	temp: string;
begin
	Result := FALSE;
	if str <> '' then begin
		temp := RemovePart(sub, str);
		if temp <> str then begin
			str    := temp;
			Result := TRUE;
		end;
	end;
end;

procedure TCheckedComboBox.ToggleOnOff(Sender: TObject);
var
	s: string;
begin
	s := FEdit.Text;
	if FListBox.Checked[FListBox.itemindex] then begin
		if Add(FListBox.Items[FListBox.itemindex], s) then begin
			FCheckedCount := FCheckedCount + 1;
		end;
	end else if Remove(FListBox.Items[FListBox.itemindex], s) then begin
		FCheckedCount := FCheckedCount - 1;
	end;
	FEdit.Text := s;
	Change;
end;

procedure TCheckedComboBox.KeyListBox(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
	if Key = VK_ESCAPE then begin
		FPrivForm.Close;
	end else begin
		inherited;
	end;
end;

function GetFormatedText(kind: TCHBQuoteStyle; str: string): string;
{ {
  added 2000/04/08
}
var
	s: string;
begin
	Result := str;
	if Length(str) > 0 then begin
		s := str;
		case kind of
			qsSingle: begin
					Result := '''' + StringReplace(s, ',', ''',''', [rfReplaceAll]) + '''';
				end;
			qsDouble: begin
					Result := '"' + StringReplace(s, ',', '","', [rfReplaceAll]) + '"';
				end;
		end;
	end;
end;

function TCheckedComboBox.GetText: string;
begin
	if FQuoteStyle = qsNone then begin
		Result := FEdit.Text;
	end else begin
		Result := GetFormatedText(FQuoteStyle, FEdit.Text);
	end;
end;

procedure TCheckedComboBox.SetDropDownLines(Value: integer);
begin
	if FDropDownLines <> Value then begin
		if (Value >= MINDROPLINES) and (Value <= MAXDROPLINES) then begin
			FDropDownLines := Value;
		end;
	end;
end;

procedure TCheckedComboBox.SetColumns(Value: integer);
begin
	if FColumns <> Value then begin
		FColumns         := Value;
		FListBox.Columns := FColumns;
	end;
end;

procedure TCheckedComboBox.SetCheckedAll(Sender: TObject);
var
	i: integer;
	s: string;
begin
	s     := '';
	for i := 0 to FListBox.Items.Count - 1 do begin
		if not FListBox.Checked[i] then begin
			FListBox.Checked[i] := TRUE;
		end;
		if i = 0 then begin
			s := FListBox.Items[i];
		end else begin
			s := s + ',' + FListBox.Items[i];
		end;
	end;
	FEdit.Text    := s;
	FCheckedCount := FListBox.Items.Count;
	FEdit.Repaint;
	Change;
end;

procedure TCheckedComboBox.SetUnCheckedAll(Sender: TObject);
var
	i: integer;
begin
	FCheckedCount := 0;
	with FListBox do begin
		for i := 0 to Items.Count - 1 do begin
			if Checked[i] then begin
				Checked[i] := FALSE;
			end;
		end;
	end;
	FEdit.Text := '';
	Change;
end;

function TCheckedComboBox.IsChecked(Index: integer): Boolean;
begin
	Result := FListBox.Checked[index];
end;

procedure TCheckedComboBox.SetChecked(Index: integer; Checked: Boolean);
var
	s : string;
	ok: Boolean;
begin
	if index < FListBox.Items.Count then begin
		s  := FEdit.Text;
		ok := FALSE;
		if not FListBox.Checked[index] and Checked then begin
			if Add(FListBox.Items[index], s) then begin
				FCheckedCount := FCheckedCount + 1;
				ok            := TRUE;
			end;
		end else if FListBox.Checked[index] and not Checked then begin
			if Remove(FListBox.Items[index], s) then begin
				FCheckedCount := FCheckedCount - 1;
				ok            := TRUE;
			end;
		end;
		if ok then begin
			FListBox.Checked[index] := Checked;
			FEdit.Text              := s;
			Change;
		end;
	end;
end;

function TCheckedComboBox.GetChecked(Index: integer): Boolean;
begin
	if index < FListBox.Items.Count then begin
		Result := FListBox.Checked[index];
	end else begin
		Result := FALSE;
	end;
end;

procedure TCheckedComboBox.SetItems(AItems: TStrings);
begin
	FItems.Assign(AItems);
end;

procedure TCheckedComboBox.ItemsChange(Sender: TObject);
begin
	FListBox.Clear;
	FEdit.Text := '';
	FListBox.Items.Assign(FItems);
end;

procedure TCheckedComboBox.CMEnabledChanged(var Message: TMessage);
{ {
  Auto Sizing routines
}
begin
	inherited;
	FButton.Enabled := Enabled;
	FEdit.Enabled   := Enabled;
end;

procedure TCheckedComboBox.AutoSize;
begin
	AdjustHeight;
	FButton.Height := Height - 2;
	FEdit.Height   := Height;
	FEdit.Width    := Width - FButton.Width - 3;
	FButton.Left   := FEdit.Width + 1;
end;

procedure TCheckedComboBox.AdjustHeight;
var
	DC                 : HDC;
	SaveFont           : HFont;
	i                  : integer;
	SysMetrics, Metrics: TTextMetric;
begin
	DC := GetDC(0);
	GetTextMetrics(DC, SysMetrics);
	SaveFont := SelectObject(DC, Font.Handle);
	GetTextMetrics(DC, Metrics);
	SelectObject(DC, SaveFont);
	ReleaseDC(0, DC);
	if NewStyleControls then begin
		if Ctl3D then begin
			i := 8;
		end else begin
			i := 6;
		end;
		i := GetSystemMetrics(SM_CYBORDER) * i;
	end else begin
		i := SysMetrics.tmHeight;
		if i > Metrics.tmHeight then begin
			i := Metrics.tmHeight;
		end;
		i := i div 4 + GetSystemMetrics(SM_CYBORDER) * 4;
	end;
	Height := Metrics.tmHeight + i;
end;

procedure TCheckedComboBox.CreateWnd;
begin
	inherited;
	AutoSize;
end;

procedure TCheckedComboBox.WMSize(var Message: TWMSize);
begin
	inherited;
	AutoSize;
end;

procedure TCheckedComboBox.CMFontChanged(var Message: TMessage);
begin
	inherited;
	AutoSize;
	invalidate;
end;

procedure TCheckedComboBox.EditOnEnter;
begin
	FEdit.color := clWhite;
	if Assigned(FOnEnter) then begin
		FOnEnter(Self);
	end;
end;

procedure TCheckedComboBox.EditOnExit;
begin
	FEdit.color := FNotFocusColor;
	if Assigned(FOnExit) then begin
		FOnExit(Self);
	end;
end;

procedure TCheckedComboBox.SetNotFocusColor(Value: TColor);
begin
	if FNotFocusColor <> Value then begin
		FNotFocusColor := Value;
		FEdit.color    := Value;
	end;
end;

procedure TCheckedComboBox.Change;
begin
	if Assigned(FOnChange) then begin
		FOnChange(Self);
	end;
end;

procedure TCheckedComboBox.Clear;
begin
	FEdit.Text := '';
	FItems.Clear;
	FListBox.Clear;
end;

procedure TCheckedComboBox.SetSorted(Value: Boolean);
begin
	if FSorted <> Value then begin
		FSorted                    := Value;
		TStringList(FItems).Sorted := FSorted;
	end;
end;

end.
