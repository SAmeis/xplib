unit SelectServer;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls;

type
  TSelectServerDlg = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    Bevel1: TBevel;
    BrowseBtn: TButton;
    Label1: TLabel;
    ServerEdit: TEdit;
    procedure OKBtnClick(Sender: TObject);
    procedure BrowseBtnClick(Sender: TObject);
  private
    function GetServerName: String;
    procedure SetServerName(const Value: String);
    procedure CheckServerConnection;
  public
    property ServerName: String read GetServerName write SetServerName;
  end;

var
  SelectServerDlg: TSelectServerDlg;

implementation

uses ShlObj, ActiveX, NetWatchMain, LmUtils;

{$R *.DFM}

function SelectComputer(const Caption: string; const Root: WideString;
  out CompName: string): Boolean;
var
  BrowseInfo: TBrowseInfo;
  Buffer: PChar;
  RootItemIDList, ItemIDList: PItemIDList;
  ShellMalloc: IMalloc;
begin
  Result := False;
  CompName := '';
  FillChar(BrowseInfo, SizeOf(BrowseInfo), 0);
  if (ShGetMalloc(ShellMalloc) = S_OK) and (ShellMalloc <> nil) then
  begin
    Buffer := ShellMalloc.Alloc(MAX_PATH);
    try
      SHGetSpecialFolderLocation(0, CSIDL_NETWORK, RootItemIDList);
      with BrowseInfo do
      begin
        hwndOwner := Application.Handle;
        pidlRoot := RootItemIDList;
        pszDisplayName := Buffer;
        lpszTitle := PChar(Caption);
        ulFlags := BIF_BROWSEFORCOMPUTER;
      end;
      ItemIDList := SHBrowseForFolder(BrowseInfo);
      Result := ItemIDList <> nil;
      if Result then
      begin
        CompName := Buffer;
        ShellMalloc.Free(ItemIDList);
      end;
    finally
      ShellMalloc.Free(Buffer);
    end;
  end;
end;

{ TSelectServerDlg }

function TSelectServerDlg.GetServerName: String;
begin
  Result := AnsiUpperCase(Trim(ServerEdit.Text));
end;

procedure TSelectServerDlg.SetServerName(const Value: String);
begin
  ServerEdit.Text := Value;
end;

procedure TSelectServerDlg.CheckServerConnection;
var
  NetRes: TNetResource;
  Res: DWORD;
begin
  if ServerName = MainForm.ComputerName then Exit;
  ZeroMemory(@NetRes, Sizeof(NetRes));
  NetRes.dwType := RESOURCETYPE_ANY;
  NetRes.lpRemoteName := PChar('\\' + ServerName + '\ADMIN$');
  Screen.Cursor := crHourGlass;
  OKBtn.Enabled := False;
  Res := WNetAddConnection3(Application.Handle, NetRes, nil, nil, CONNECT_INTERACTIVE);
  OKBtn.Enabled := True;
  Screen.Cursor := crDefault;
  if Res = ERROR_CANCELLED then SysUtils.Abort;
  NetCheck(Res);
end;

procedure TSelectServerDlg.OKBtnClick(Sender: TObject);
begin
  CheckServerConnection;
  ModalResult := mrOk;
end;

procedure TSelectServerDlg.BrowseBtnClick(Sender: TObject);
var
  S: String;
begin
  if SelectComputer('', '', S) then ServerName := S;
end;

end.
