{$IFDEF PathEdit}
    {$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I DlgLib.inc}

unit PathEdit;

interface

uses
	Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls, Buttons, ExtCtrls, lbhorz, ShellShlDlg;
	{ TODO -oRoger -cLIB : Resolver depencia do ShellLib do melhor modo possivel, ou seja removendo ( ShellShlDlg )}


type
	TPathEditorDlg = Class (TForm)
		OKBtn : TButton;
		CancelBtn : TButton;
		Bevel1 : TBevel;
		DescriptionLabel : TLabel;
		UpItemBtn : TSpeedButton;
		DownItemBtn : TSpeedButton;
		BrowseFolderBtn : TSpeedButton;
		PathListBox : TListBoxHrz;
		ItemEditBox : TEdit;
		ReplaceBtn : TButton;
		AddBtn : TButton;
		DelBtn : TButton;
		BrowserFolderDlg : TkbBrowseForFolderDialog;
		procedure FormShow(Sender : TObject);
		procedure FormCreate(Sender : TObject);
		procedure UpItemBtnClick(Sender : TObject);
		procedure DownItemBtnClick(Sender : TObject);
		procedure PathListBoxClick(Sender : TObject);
		procedure ItemEditBoxChange(Sender : TObject);
		procedure ReplaceBtnClick(Sender : TObject);
		procedure AddBtnClick(Sender : TObject);
		procedure BrowseFolderBtnClick(Sender : TObject);
		procedure DelBtnClick(Sender : TObject);
		procedure PathListBoxDragOver(Sender, Source : TObject; X, Y : Integer; State : TDragState; var Accept : Boolean);
		procedure PathListBoxDragDrop(Sender, Source : TObject; X, Y : Integer);
		procedure OKBtnClick(Sender : TObject);
	private
		FTokenChar : Char;
		{ Private declarations }
	public
		{ Public declarations }
		property TokenChar : Char read FTokenChar write FTokenChar;
	end;

type  //Definacao do component
	TMultPathEditor = Class (TComponent)
	private
		FPathString : TStringList;
		FTokenChar : Char;
		FHelpContext : integer;
		FTextLimit : integer;
		FLabelString : string;
		FCaption : string;
		FEntriesLimit : integer;
		function GetPath : string;
		procedure SetPath(const Value : string);
		procedure SetPathString(const Value : TStrings);
		function GetPathString : TStrings;
	protected
		Dialog : TPathEditorDlg;
	public
		constructor Create(AOwner : TComponent); override;
		destructor Destroy; override;
		function Execute : boolean;
	published
		property Caption : string read FCaption write FCaption;
		property EntriesLimit : integer read FEntriesLimit write FEntriesLimit;
		property LabelString : string read FLabelString write FLabelString;
		property HelpContext : integer read FHelpContext write FHelpContext;
		property Path : string read GetPath write SetPath;
		property PathString : TStrings read GetPathString write SetPathString;
		property TextLimit : integer read FTextLimit write FTextLimit;
		property TokenChar : Char read FTokenChar write FTokenChar;
	end;

var
	PathEditorDlg : TPathEditorDlg;

implementation

{$R *.DFM}

uses
	Math, Dialogs;

constructor TMultPathEditor.Create(AOwner : TComponent);
	//-------------------------------------------------------------------------------------------------------------------------------
begin
	inherited;
	Self.FTokenChar := ';';
	FPathString := TStringList.Create;
end;

destructor TMultPathEditor.Destroy;
	//-------------------------------------------------------------------------------------------------------------------------------
begin
	Self.FPathString.Free;
	inherited;
end;

function TMultPathEditor.Execute : boolean;
	//-------------------------------------------------------------------------------------------------------------------------------
var
	OldPath : TStringList;
begin
	Self.Dialog := TPathEditorDlg.Create(Self);
	try
		OldPath := TStringList.Create;
		try
			OldPath.Assign(Self.PathString); //Valor original da lista
			Result := (Self.Dialog.ShowModal = mrOK);
			if Result then begin
				Self.FPathString.Assign(Self.Dialog.PathListBox.Items);
			end else begin
				Self.FPathString.Assign(OldPath);  //Recupera valor original se cancelada a edicao
			end;
		finally
			OldPath.Free;
		end;
	finally
		FreeAndNil(Self.Dialog);
	end;
end;

function TMultPathEditor.GetPath : string;
	//-------------------------------------------------------------------------------------------------------------------------------
begin
	if Assigned(Self.Dialog) then begin
		Result := Self.Dialog.PathListBox.Items.Text;
	end else begin
		Result := Self.FPathString.Text;
	end;
	Result := StringReplace(Result, #13#10, Self.TokenChar, [rfReplaceAll, rfIgnoreCase]);
end;

function TMultPathEditor.GetPathString : TStrings;
	//-------------------------------------------------------------------------------------------------------------------------------
begin
	if Assigned(Self.Dialog) then begin
		Result := Self.Dialog.PathListBox.Items;
	end else begin
		Result := Self.FPathString;
	end;
end;

procedure TMultPathEditor.SetPath(const Value : string);
//-------------------------------------------------------------------------------------------------------------------------------
var
	ItemsText : string;
begin
	ItemsText := StringReplace(Value, Self.TokenChar, #13#10, [rfReplaceAll, rfIgnoreCase]);
	if Self.PathString.Text <> ItemsText then begin
		Self.PathString.Text := ItemsText;
	end;
end;

procedure TMultPathEditor.SetPathString(const Value : TStrings);
//-------------------------------------------------------------------------------------------------------------------------------
begin
	Self.FPathString.Assign(Value);
	if Assigned(Self.Dialog) then begin
		Self.Dialog.PathListBox.Items.Assign(Self.FPathString);
	end;
end;

{ TPathEditorDlg }

procedure TPathEditorDlg.FormShow(Sender : TObject);
//-------------------------------------------------------------------------------------------------------------------------------
begin
	//Self.MakeList();
end;

procedure TPathEditorDlg.FormCreate(Sender : TObject);
//-------------------------------------------------------------------------------------------------------------------------------
begin
	if Self.Owner is TMultPathEditor then begin
		Self.Caption := TMultPathEditor(Owner).Caption;
		Self.DescriptionLabel.Caption := TMultPathEditor(Owner).LabelString;
		Self.TokenChar := TMultPathEditor(Owner).TokenChar;
		Self.PathListBox.Items.Assign(TMultPathEditor(Owner).PathString);
	end;
end;

procedure TPathEditorDlg.UpItemBtnClick(Sender : TObject);
//-------------------------------------------------------------------------------------------------------------------------------
var
	I : integer;
begin
	i := Self.PathListBox.ItemIndex;
	if I > 0 then begin
		Self.PathListBox.Items.Move(i, i - 1);
		Self.PathListBox.ItemIndex := (i - 1);
	end;
end;

procedure TPathEditorDlg.DownItemBtnClick(Sender : TObject);
//-------------------------------------------------------------------------------------------------------------------------------
var
	I : integer;
begin
	i := Self.PathListBox.ItemIndex;
	if (I >= 0) and (i < Self.PathListBox.Items.Count - 1) then begin
		Self.PathListBox.Items.Move(i, i + 1);
		Self.PathListBox.ItemIndex := (i + 1);
	end;
end;

procedure TPathEditorDlg.PathListBoxClick(Sender : TObject);
//-------------------------------------------------------------------------------------------------------------------------------
begin
	Self.DelBtn.Enabled := (Self.PathListBox.ItemIndex >= 0);
	Self.ItemEditBox.Text := Self.PathListBox.Items[Self.PathListBox.ItemIndex];
end;

procedure TPathEditorDlg.ItemEditBoxChange(Sender : TObject);
//-------------------------------------------------------------------------------------------------------------------------------
var
	OldVal : string;
	P1, P2 : integer;
begin
	P1 := Self.PathListBox.ItemIndex;
	if P1 >= 0 then begin //Valor selecionado para possivel edicao
		OldVal := Self.PathListBox.Items[P1]; //Valor do elemento selecionado
		if Self.ItemEditBox.Text <> OldVal then begin //Valor digitado <> do original
			P2 := Self.PathListBox.Items.IndexOf(Self.ItemEditBox.Text);
			if (P2 >= 0) and (P2 <> P1) then begin
				Self.PathListBox.ItemIndex := P2;
				Self.AddBtn.Enabled := FALSE;
				Self.ReplaceBtn.Enabled := FALSE;
			end else begin
				Self.AddBtn.Enabled := TRUE;
				Self.ReplaceBtn.Enabled := TRUE;
			end;
		end else begin
			Self.AddBtn.Enabled := FALSE;
			Self.ReplaceBtn.Enabled := FALSE;
		end;
	end else begin  //Novo item a ser entrado
		Self.AddBtn.Enabled := TRUE;
		Self.ReplaceBtn.Enabled := FALSE;
	end;
end;

procedure TPathEditorDlg.ReplaceBtnClick(Sender : TObject);
//-------------------------------------------------------------------------------------------------------------------------------
begin
	Self.PathListBox.Items[Self.PathListBox.ItemIndex] := Self.ItemEditBox.Text;
end;

procedure TPathEditorDlg.AddBtnClick(Sender : TObject);
//-------------------------------------------------------------------------------------------------------------------------------
var
	I : integer;
begin
	I := Self.PathListBox.ItemIndex;
	Self.PathListBox.Items.Insert(I + 1, Self.ItemEditBox.Text);
	Self.PathListBox.ItemIndex := I + 1;
	Self.AddBtn.Enabled := FALSE;
	Self.ReplaceBtn.Enabled := FALSE;
end;

procedure TPathEditorDlg.BrowseFolderBtnClick(Sender : TObject);
//-------------------------------------------------------------------------------------------------------------------------------
begin
	if Self.BrowserFolderDlg.Execute then begin
		Self.ItemEditBox.Text := Self.BrowserFolderDlg.RootPath;
	end;
end;

procedure TPathEditorDlg.DelBtnClick(Sender : TObject);
//-------------------------------------------------------------------------------------------------------------------------------
begin
	Self.PathListBox.Items.Delete(Self.PathListBox.ItemIndex);
	Self.ItemEditBox.Text := EmptyStr;
	Self.AddBtn.Enabled := FALSE;
end;

procedure TPathEditorDlg.PathListBoxDragOver(Sender, Source : TObject; X, Y : Integer; State : TDragState; var Accept : Boolean);
//-------------------------------------------------------------------------------------------------------------------------------
begin
	Accept := (Sender = Source);
end;

procedure TPathEditorDlg.PathListBoxDragDrop(Sender, Source : TObject; X, Y : Integer);
//-------------------------------------------------------------------------------------------------------------------------------
var
	P1 : integer;
begin
	P1 := Self.PathListBox.ItemAtPos(Point(X, Y), FALSE);
	Self.PathListBox.Items.Move(Self.PathListBox.ItemIndex, Min(P1, Self.PathListBox.Items.Count - 1));
end;

procedure TPathEditorDlg.OKBtnClick(Sender : TObject);
//----------------------------------------------------------------------------------------------------------------------------------
begin
	if Self.AddBtn.Enabled then begin
		Self.AddBtnClick(Sender); //Tenta adcionar o valor pendente
	end;
end;

end.


