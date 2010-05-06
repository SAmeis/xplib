unit NetWatchMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActnList, Menus, ComCtrls, ImgList, ToolWin, ExtCtrls, Svrapi, LmClasses;

type
  TMainForm = class(TForm)
    MainMenu: TMainMenu;
    ActionList: TActionList;
    StatusBar: TStatusBar;
    ImageList: TImageList;
    PageControl1: TPageControl;
    SessionsSheet: TTabSheet;
    ShareSheet: TTabSheet;
    FilesSheet: TTabSheet;
    NetSessionListView: TListView;
    Obnovit1: TMenuItem;
    NetShareListView: TListView;
    SmallListViewImages: TImageList;
    LargeListViewImages: TImageList;
    ConnectionTreeView: TTreeView;
    Splitter1: TSplitter;
    NetFileListView: TListView;
    ToolBar: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
	 ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    Exit1: TAction;
    Exit2: TMenuItem;
    Refresh1: TAction;
    View1: TMenuItem;
    Refresh2: TMenuItem;
    NetSessionView1: TAction;
    NetShareView1: TAction;
    NetFileView1: TAction;
    Viewusers1: TMenuItem;
    Viewsharedfiles1: TMenuItem;
    Viewshareditems1: TMenuItem;
    N1: TMenuItem;
    DeleteSession1: TAction;
    DeleteSession11: TMenuItem;
    N2: TMenuItem;
    ToolButton4: TToolButton;
    ToolButton9: TToolButton;
    SelectServer1: TAction;
    Selectserver2: TMenuItem;
    RefreshTimer: TTimer;
    About1: TAction;
    Help1: TMenuItem;
    About2: TMenuItem;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    LargeIconsView1: TAction;
    SmallIconsView1: TAction;
    ReportView1: TAction;
	 N3: TMenuItem;
    LargeIcons1: TMenuItem;
    SmallIcons1: TMenuItem;
    Report1: TMenuItem;
    AutoRefresh1: TAction;
    ToolButton14: TToolButton;
    AutoRefresh2: TMenuItem;
    N4: TMenuItem;
    NetFileClose1: TAction;
    ToolButton15: TToolButton;
    NetFileClose11: TMenuItem;
    Splitter2: TSplitter;
    ShareTreeView: TTreeView;
    NetShareDel1: TAction;
    ToolButton16: TToolButton;
    ToolButton17: TToolButton;
    NetShareAdd1: TAction;
    ToolButton18: TToolButton;
    Addshare1: TMenuItem;
    Deleteshare1: TMenuItem;
    N5: TMenuItem;
    N6: TMenuItem;
	 procedure NetSessionListViewColumnClick(Sender: TObject; Column: TListColumn);
	 procedure NetSessionListViewCompare(Sender: TObject; Item1, Item2: TListItem; Data: Integer; var Compare: Integer);
	 procedure NetSessionListViewChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure FormCreate(Sender: TObject);
    procedure Exit1Execute(Sender: TObject);
    procedure Refresh1Execute(Sender: TObject);
    procedure NetSessionView1Execute(Sender: TObject);
    procedure DeleteSession1Update(Sender: TObject);
    procedure DeleteSession1Execute(Sender: TObject);
    procedure SelectServer1Execute(Sender: TObject);
	 procedure RefreshTimerTimer(Sender: TObject);
    procedure LargeIconsView1Execute(Sender: TObject);
    procedure AutoRefresh1Execute(Sender: TObject);
    procedure About1Execute(Sender: TObject);
    procedure NetFileClose1Update(Sender: TObject);
    procedure NetFileClose1Execute(Sender: TObject);
    procedure NetShareListViewChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure NetShareDel1Update(Sender: TObject);
    procedure NetShareAdd1Update(Sender: TObject);
    procedure NetShareDel1Execute(Sender: TObject);
    procedure NetShareAdd1Execute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FAutoRefresh: Boolean;
    FComputerName: String;
    FServerName: String;
    procedure BuildConnectionTree(const ComputerName: String);
    procedure BuildShareTree(const ShareName: String);
	 procedure BuildNetFileList;
	 procedure BuildNetSessionList;
	 procedure BuildNetShareList;
	 procedure RefreshView;
	 procedure SetAutoRefresh(const Value: Boolean);
	 procedure SetServerName(const Value: String);
	 class function MessageBoxYesNo(const S: String): Boolean;
  public
	 SessionList: TNetSessionList;
	 ShareList: TNetShareList;
    FileList: TNetFileList;
    property AutoRefresh: Boolean read FAutoRefresh write SetAutoRefresh;
    property ComputerName: String read FComputerName;
    property ServerName: String read FServerName write SetServerName;
  end;

var
  MainForm: TMainForm;

implementation

uses SelectServer, About, LmErr, LmCons, LmUtils, ShareAdd;

resourcestring
  sAppTitle = 'NetWatch95 - %d conexões';
  sComputerName = 'Computador: %s';
  sDeleteShare = 'Apagar compartilhamento "%s" ?';
  sDisconnectUser = 'Disconectar Computador "%s" ?';
  sServerName = 'Servidor: %s';
  sApiNotLoaded = 'Erro Fatal: Impossível carregar módulos do Lan Manager';

{$R *.DFM}

const
  MaxNetArrayItems = 512;

function FmtStrToInt(S: String): Integer;
//----------------------------------------------------------------------------------------------------------------------------------
var
	I : Integer;
begin
  I := 1;
  while I <= Length(S) do
	 if not (S[I] in ['0'..'9', '-']) then
		Delete(S, I, 1)
	 else
		Inc(I);
  Result := StrToInt(S);
end;

function LVCheckFocusChanged(Item: TListItem; Change: TItemChange): Boolean;
//----------------------------------------------------------------------------------------------------------------------------------
begin
  Result := (Item <> nil) and (Change = ctState) and Item.Focused and Item.Selected;
end;

procedure LVColumnClick(ListView: TListView; Column: TListColumn);
//----------------------------------------------------------------------------------------------------------------------------------
var
	ColIndex: Integer;
begin
	ColIndex := Column.Index;
	with ListView do begin
		if ( Tag and $FF ) = ColIndex  then begin
			Tag := Tag xor $100
		end else begin
			Tag := ColIndex;
		end;
		AlphaSort;
		if Selected <> nil then begin
			Selected.MakeVisible(False);
		end;
	end;
end;

procedure LVCompare(ListView: TListView; Item1, Item2: TListItem; Data: Integer; var Compare: Integer);
//----------------------------------------------------------------------------------------------------------------------------------
var
	ColIndex: Integer;
begin
	with ListView do begin
		ColIndex := Tag and ($FF - 1);
		if Columns[ColIndex + 1].Alignment = taLeftJustify then begin
			if ColIndex = -1 then begin
				Compare := AnsiCompareText(Item1.Caption, Item2.Caption)
			end else begin
				Compare := AnsiCompareText(Item1.SubItems[ColIndex], Item2.SubItems[ColIndex]);
			end;
		end else begin
			if ColIndex = -1 then begin
				Compare := FmtStrToInt(Item1.Caption) - FmtStrToInt(Item2.Caption)
			end else begin
				Compare := FmtStrToInt(Item1.SubItems[ColIndex]) - FmtStrToInt(Item2.SubItems[ColIndex]);
			end;
		end;
		if Tag and $100 <> 0 then begin
			Compare := -Compare;
		end;
	end;
end;

function ShareTypeImageIndex(ShareType: Byte): Integer;
//----------------------------------------------------------------------------------------------------------------------------------
begin
	case ShareType of
	  	STYPE_DISKTREE: Result := 1;
		STYPE_PRINTQ: Result := 2;
		STYPE_DEVICE: Result := -1;
		STYPE_IPC: Result := 5;
	else
		Result := -1;
	end;
end;

{ TForm1 }

procedure TMainForm.BuildNetSessionList;
//----------------------------------------------------------------------------------------------------------------------------------
var
  I: Integer;
  LastComputerName: String;
  NewItem, FindItem: TListItem;
begin
	with NetSessionListView do begin
		Items.BeginUpdate;
		try
			if Selected <> nil then begin
				LastComputerName := Selected.SubItems[0]
			end else begin
				LastComputerName := EmptyStr;
			end;
			Items.Clear;  //Limpa elementos anteriores
			FindItem := nil;
			SessionList.Refresh;
			for I := 0 to SessionList.Count - 1 do begin
				NewItem := Items.Add;
				with NewItem, SessionList[I] do begin
					Caption := UserName;
					ImageIndex := 3;
					SubItems.Add(ComputerName);
					SubItems.Add(IntToStr(Connections));
					SubItems.Add(IntToStr(Opens));
					SubItems.Add(TimeStr);
					SubItems.Add(IdleTimeStr);
					SubItems.AddObject(IntToStr(Key), Pointer(Key));
					if SubItems[0] = LastComputerName then begin
						FindItem := NewItem;
					end;
				end;
			end;
			AlphaSort;
			if FindItem <> nil then begin
				FindItem.Selected := True;
				ItemFocused := FindItem;
			end;
			Application.Title := Format(sAppTitle, [SessionList.Count]);
			Self.Caption := Application.Title;
		finally
			Items.EndUpdate;
		end;
	end;
end;

procedure TMainForm.BuildNetShareList;
//----------------------------------------------------------------------------------------------------------------------------------
var
	I : Integer;
	LastCaption : string;
	FindItem: TListItem;
begin
	NetShareListView.Items.BeginUpdate;
	try
		if NetShareListView.Selected <> nil then begin
			LastCaption := NetShareListView.Selected.Caption
		end else begin
			LastCaption := EmptyStr;
		end;
		NetShareListView.Items.Clear;
		ShareList.Refresh;
		for I := 0 to ShareList.Count - 1 do begin
			with NetShareListView.Items.Add do begin
				Caption := ShareList[I].Path;
				Data := ShareList[I];
				ImageIndex := ShareTypeImageIndex(ShareList[I].ShareType);
				SubItems.Add(ShareList[I].ShareName);
				SubItems.Add(Format('%.3x', [ShareList[I].ShareFlags])); // !!!
				SubItems.Add(ShareList[I].PasswordRO);
				SubItems.Add(ShareList[I].PasswordRW);
				SubItems.Add(ShareList[I].Remark);
			end;
		end;
		NetShareListView.AlphaSort;
		FindItem := NetShareListView.FindCaption(0, LastCaption, False, True, False);
		if FindItem <> nil then begin
			FindItem.Selected := True;
			NetShareListView.ItemFocused := FindItem;
		end;
	finally
		NetShareListView.Items.EndUpdate;
	end;
end;

procedure TMainForm.BuildConnectionTree(const ComputerName : string);
//----------------------------------------------------------------------------------------------------------------------------------
var
	TreeConnList : TNetConnetionList;
	TreeFileList : TNetFileList;
	I, FI : Integer;
	ShareNode : TTreeNode;
begin
	TreeConnList := TNetConnetionList.Create;
	TreeFileList := TNetFileList.Create(True);
	try
		ConnectionTreeView.Items.BeginUpdate;
		try
			ConnectionTreeView.Items.Clear;
			TreeConnList.ServerName := ServerName;
			TreeConnList.ComputerName := ComputerName;
			TreeFileList.ServerName := ServerName;
			if TreeFileList.LastError = NERR_Success then begin
			   for I := 0 to TreeConnList.Count - 1 do begin
				   ShareNode := ConnectionTreeView.Items.Add(nil, TreeConnList[I].ShareName);
				   ShareNode.ImageIndex := 1;
				   ShareNode.SelectedIndex := 1;
				   for FI := 0 to TreeFileList.Count - 1 do begin
					   if TreeFileList[FI].UserNameSame(TreeConnList[I].UserName) and
						  TreeFileList[FI].ShareNameSame(TreeConnList[I].ShareName) then
					   begin
						   ConnectionTreeView.Items.AddChildObject(ShareNode, TreeFileList[FI].PathName, Pointer(TreeFileList[FI].ID));
					   end;
				   end;
				   ShareNode.Expand(True);
			   end;
			end;
		finally
			ConnectionTreeView.Items.EndUpdate;
		end;
	finally
		TreeConnList.Free;
		TreeFileList.Free;
	end;
end;

procedure TMainForm.BuildShareTree(const ShareName: String);
//----------------------------------------------------------------------------------------------------------------------------------
//Monta toda a arvore  de compartilhamentos do servidor ativo
var
	TreeConnList : TNetConnetionList;
	TreeFileList : TNetFileList;
	I, FI : Integer;
	UserNode : TTreeNode;
begin
	TreeConnList := TNetConnetionList.Create(True);
	TreeFileList := TNetFileList.Create;
	try
		ShareTreeView.Items.BeginUpdate;
		try
			ShareTreeView.Items.Clear;
			TreeConnList.ServerName := ServerName;
			TreeConnList.ShareName := ShareName;
			if TreeConnList.LastError <> NERR_NetNameNotFound then begin
				NetCheck(TreeConnList.LastError);
				for I := 0 to TreeConnList.Count - 1 do begin
					with ShareTreeView.Items.Add(nil, TreeConnList[I].UserName) do begin
						ImageIndex := 4;
						SelectedIndex := 4;
					end;
				end;
				TreeFileList.ServerName := ServerName;
				for FI := 0 to TreeFileList.Count - 1 do begin
					if TreeFileList[FI].ShareNameSame(ShareName) then begin
						UserNode := nil;
						for I := 0 to ShareTreeView.Items.Count - 1 do begin
							if TreeFileList[FI].UserNameSame(ShareTreeView.Items[I].Text) then begin
								UserNode := ShareTreeView.Items[I];
								Break;
							end;
						end;
						ShareTreeView.Items.AddChild(UserNode, TreeFileList[FI].PathName);
					end;
				end;
				ShareTreeView.FullExpand;
			end;
		finally
			ShareTreeView.Items.EndUpdate;
		end;
	finally
		TreeConnList.Free;
		TreeFileList.Free;
	end;
end;

procedure TMainForm.BuildNetFileList;
//----------------------------------------------------------------------------------------------------------------------------------
var
	I : Integer;
	LastCaption : string;
	FindItem : TListItem;
begin
	NetFileListView.Items.BeginUpdate;
	try
		if NetFileListView.Selected <> nil then begin
			LastCaption := NetFileListView.Selected.Caption
		end else begin
			LastCaption := EmptyStr;
		end;
		NetFileListView.Items.Clear;
		FileList.Refresh;
		for I := 0 to FileList.Count - 1 do begin
			with NetFileListView.Items.Add do begin
				Caption := FileList[I].PathName;
				Data := FileList[I];
				SubItems.Add(FileList[I].ShareName);
				SubItems.Add(FileList[I].UserName);
				SubItems.Add(IntToStr(FileList[I].Permissions));
				SubItems.Add(IntToStr(FileList[I].NumLocks));
				SubItems.Add(IntToStr(FileList[I].ID));
			end;
		end;
		NetFileListView.AlphaSort;
		FindItem := NetFileListView.FindCaption(0, LastCaption, False, True, False);
		if FindItem <> nil then begin
			FindItem.Selected := True;
			NetFileListView.ItemFocused := FindItem;
		end;
	finally
		NetFileListView.Items.EndUpdate;
	end;
end;

procedure TMainForm.RefreshView;
//----------------------------------------------------------------------------------------------------------------------------------
begin
	Screen.Cursor := crHourGlass;
	try
		try
			BuildConnectionTree('');
			BuildShareTree('\');  { TODO -oRoger -cDSG : Alterado troca para \ no lugar de espaco vazio }
			BuildNetSessionList;
			BuildNetShareList; { TODO -oRoger -cDSG : Checar montagem da lista de compartilhamentos }
			BuildNetFileList;
		except
			AutoRefresh := False;
		  	raise;
		end;
	finally
		Screen.Cursor := crDefault;
	end;
end;

procedure TMainForm.SetServerName(const Value: string);
//----------------------------------------------------------------------------------------------------------------------------------
begin
	if Value = EmptyStr then begin
		FServerName := AddDoubleSlash(ComputerName)
	end else begin
		FServerName := AddDoubleSlash(Value);
	end;
	StatusBar.Panels[0].Text := Format(sServerName, [FServerName]);
	SessionList.ServerName := FServerName;
	ShareList.ServerName := FServerName;
	FileList.ServerName := FServerName;
	RefreshView;
end;

procedure TMainForm.FormCreate(Sender: TObject);
//----------------------------------------------------------------------------------------------------------------------------------
var
	CompName: array[0..MAX_COMPUTERNAME_LENGTH] of Char;
	L : DWORD;
begin
	if not LmClassesLoaded then begin
		ShowMessage(sApiNotLoaded);
		Application.Terminate;
		Exit;
	end;

	SessionList := TNetSessionList.Create;
	ShareList := TNetShareList.Create;
	FileList := TNetFileList.Create;

	L := Sizeof(CompName);
	Win32Check(GetComputerName(@CompName, L));
	FComputerName := CompName;
	StatusBar.Panels[1].Text := Format(sComputerName, [FComputerName]);

	{$IFDEF FORCE_NT}
	{$IFDEF DEBUG}
	ServerName := 'HOLDING_AR';
	{$ELSE}
	ServerName := '';
	{$ENDIF}
	{$ELSE}
	{$IFDEF DEBUG}
	ServerName := 'HOLDING_AR';
	{$ELSE}
	ServerName := '';
	{$ENDIF}
	{$ENDIF}
	LargeIconsView1.Tag := Longint(vsIcon);
	SmallIconsView1.Tag := Longint(vsSmallIcon);
	ReportView1.Tag := Longint(vsReport);
	{$IFDEF DEBUG}
	AutoRefresh := False;
	{$ELSE}
	AutoRefresh := True;
	{$ENDIF}
end;

procedure TMainForm.Exit1Execute(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.NetSessionListViewColumnClick(Sender: TObject; Column: TListColumn);
begin
  LVColumnClick(TListView(Sender), Column);
end;

procedure TMainForm.NetSessionListViewCompare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
begin
  LVCompare(TListView(Sender), Item1, Item2, Data, Compare);
end;

procedure TMainForm.NetSessionListViewChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  if LVCheckFocusChanged(Item, Change) then
    BuildConnectionTree(TListView(Sender).Selected.SubItems[0]);
end;

procedure TMainForm.Refresh1Execute(Sender: TObject);
begin
  RefreshView;
end;

procedure TMainForm.NetSessionView1Execute(Sender: TObject);
//----------------------------------------------------------------------------------------------------------------------------------
begin
	try
		NetSessionView1.Checked := False;
		NetShareView1.Checked := False;
		NetFileView1.Checked := False;
		RefreshView;
	 finally
		with PageControl1 do begin
			ActivePage := Pages[TAction(Sender).Tag];
		end;
		TAction(Sender).Checked := True;
	 end;
end;

procedure TMainForm.DeleteSession1Update(Sender: TObject);
begin
  TAction(Sender).Enabled := NetSessionListView.Focused and
	 (NetSessionListView.Selected <> nil) and
	 (FComputerName <> NetSessionListView.Selected.SubItems[0]);
end;

procedure TMainForm.DeleteSession1Execute(Sender: TObject);
begin
  with NetSessionListView.Selected.SubItems do
    if MessageBoxYesNo(Format(sDisconnectUser, [Strings[0]])) then
    begin
      NetCheck(NetSessionDel(PChar(FServerName), PChar(Strings[0]), DWORD(Objects[5])));
      RefreshView;
    end;
end;

procedure TMainForm.SelectServer1Execute(Sender: TObject);
begin
  with TSelectServerDlg.Create(Application) do
  try
    ServerName := CutDoubleSlash(Self.ServerName);
    if ShowModal = mrOk then
      Self.ServerName := AddDoubleSlash(ServerName);
  finally
    Free;
  end;
end;

procedure TMainForm.RefreshTimerTimer(Sender: TObject);
begin
  RefreshView;
end;

procedure TMainForm.LargeIconsView1Execute(Sender: TObject);
begin
  LargeIconsView1.Checked := False;
  SmallIconsView1.Checked := False;
  ReportView1.Checked := False;
  TAction(Sender).Checked := True;
  NetSessionListView.ViewStyle := TViewStyle(TAction(Sender).Tag);
  NetShareListView.ViewStyle := TViewStyle(TAction(Sender).Tag);
  NetFileListView.ViewStyle := TViewStyle(TAction(Sender).Tag);
end;

procedure TMainForm.SetAutoRefresh(const Value: Boolean);
//----------------------------------------------------------------------------------------------------------------------------------
begin
   if FAutoRefresh <> Value then begin
      FAutoRefresh := Value;
      RefreshTimer.Enabled := Value;
      AutoRefresh1.Checked := Value;
   end;
end;

procedure TMainForm.AutoRefresh1Execute(Sender: TObject);
begin
  AutoRefresh := not AutoRefresh;
end;

procedure TMainForm.About1Execute(Sender: TObject);
begin
  with TAboutDlg.Create(Application) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure TMainForm.NetFileClose1Update(Sender: TObject);
begin
  TAction(Sender).Enabled := NetFileListView.Focused and (NetFileListView.Selected <> nil);
end;

procedure TMainForm.NetFileClose1Execute(Sender: TObject);
//----------------------------------------------------------------------------------------------------------------------------------
begin
	if NetFileListView.Selected <> nil then begin
		TNetFileItem(NetFileListView.Selected.Data).CloseFile;
		RefreshView;
	end;
end;

procedure TMainForm.NetShareListViewChange(Sender: TObject;
  Item: TListItem; Change: TItemChange);
begin
  if LVCheckFocusChanged(Item, Change) then
    BuildShareTree(Item.SubItems[0]);
end;

procedure TMainForm.NetShareDel1Update(Sender: TObject);
begin
  TAction(Sender).Enabled := NetShareListView.Focused and (NetShareListView.Selected <> nil);
end;

procedure TMainForm.NetShareAdd1Update(Sender: TObject);
begin
  TAction(Sender).Enabled := NetShareListView.Focused;
end;

class function TMainForm.MessageBoxYesNo(const S: String): Boolean;
begin
  Result := Application.MessageBox(PChar(S), PChar(Application.Title),
    MB_YESNO or MB_ICONWARNING or MB_DEFBUTTON2) = ID_YES;
end;

procedure TMainForm.NetShareDel1Execute(Sender: TObject);
begin
  with TNetShareItem(NetShareListView.Selected.Data) do
    if MessageBoxYesNo(Format(sDeleteShare, [ShareName])) then
    begin
      DeleteShare;
      RefreshView;
    end;
end;

procedure TMainForm.NetShareAdd1Execute(Sender: TObject);
begin
  with TShareAddDlg.Create(Application) do
  try
    with NetShareListView do
      if Selected <> nil then ShareItem := Selected.Data;
    if ShowModal = mrOk then
      RefreshView;
  finally
    Free;
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  SessionList.Free;
  ShareList.Free;
  FileList.Free;
end;

end.
