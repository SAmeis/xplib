{$IFDEF WinFileNotification}
{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I WinSysLib.inc}
{

  Acessado por http://forum.codecall.net/topic/76318-monitor-a-folder-for-changes/

  pode ser melhorado por http://qualapps.blogspot.com.br/2010/05/understanding-readdirectorychangesw.html

}

unit WinFileNotification;

interface

uses
	SysUtils, Classes, Windows, XPThreads;

type
	TFolderAction = (faNew, faRemoved, faModified, faRenamedOld, faRenamedNew);

const
	FOLDER_ACTION_NAMES: array[TFolderAction] of string = ('Novo', 'Apagado', 'Modificado', 'Nome antigo', 'Nome novo');

type
	TFolderItemInfo = record
		Name: string;
		Action: TFolderAction;
	end;

	TChangeType = (ctFileName, ctDirName, ctAttr, ctSize, ctLastWriteTime, ctLastAccessTime, ctCreationTime, ctSecurityAttr);

	TChangeTypes          = set of TChangeType;
	TWinFileSystemMonitor = class;
	TFolderChangeEvent    = procedure(Sender: TWinFileSystemMonitor; AFolderItem: TFolderItemInfo) of object;

	TWinFileSystemMonitor = class(TComponent)
	private
		FFolder:            string;
		FWorker:            TXPNamedThread;
		FMonitoredChanges:  TChangeTypes;
		FMonitorSubFolders: boolean;
		FOnFOlderChange:    TFolderChangeEvent;
		FOnDeactivated:     TNotifyEvent;
		FOnActivated:       TNotifyEvent;
		procedure SetFolder(const Value: string);
		function GetIsActive: boolean;
		procedure SetIsActive(const Value: boolean);
		procedure SetMonitoredChanges(const Value: TChangeTypes);
		procedure SetMonitorSubFolders(const Value: boolean);
	public
		destructor Destroy; override;
		procedure Activate;
		procedure Deactivate;
	published
		property Folder:           string read FFolder write SetFolder;
		property IsActive:         boolean read GetIsActive write SetIsActive;
		property MonitoredChanges: TChangeTypes read FMonitoredChanges write SetMonitoredChanges;
		property Recursive:        boolean read FMonitorSubFolders write SetMonitorSubFolders;
		property OnFolderChange:   TFolderChangeEvent read FOnFOlderChange write FOnFOlderChange;
		property OnActivated:      TNotifyEvent read FOnActivated write FOnActivated;
		property OnDeactivated:    TNotifyEvent read FOnDeactivated write FOnDeactivated;
	end;

procedure Register;

implementation

const
	NOTIFY_FILTERS: array[TChangeType] of DWORD = (FILE_NOTIFY_CHANGE_FILE_NAME //ctFileName
		, FILE_NOTIFY_CHANGE_DIR_NAME //ctDirName
		, FILE_NOTIFY_CHANGE_ATTRIBUTES //ctAttr
		, FILE_NOTIFY_CHANGE_SIZE //ctSize
		, FILE_NOTIFY_CHANGE_LAST_WRITE //ctLastWriteTime
		, FILE_NOTIFY_CHANGE_LAST_ACCESS //ctLastAccessTime
		, FILE_NOTIFY_CHANGE_CREATION //ctCreationTime
		, FILE_NOTIFY_CHANGE_SECURITY //ctSecurityAttr
		);

type
	TFolderMonWorker = class(TXPNamedThread)
	private
		Owner:           TWinFileSystemMonitor;
		FFolder:         THandle;
		FMonFilter:      DWORD;
		FFolderItemInfo: TFolderItemInfo;
		procedure SetUp;
		procedure TearDown;
		procedure DoFolderItemChange;
	protected
		procedure Execute; override;
	public
		constructor Create(AOwner: TWinFileSystemMonitor); reintroduce;
	end;

procedure Register;
begin
	RegisterComponents('Win32', [TWinFileSystemMonitor]);
end;

procedure TWinFileSystemMonitor.Activate;
begin
	if IsActive then begin
		Exit;
	end;

	if FMonitoredChanges = [] then begin
		raise Exception.Create('Necessário especificar eventos para monitoração');
	end;
	if not DirectoryExists(FFolder) then begin
		raise Exception.Create('Necessário informar um caminho existente/válido');
	end;

	FWorker := TFolderMonWorker.Create(Self);
	if Assigned(FOnActivated) then begin
		FOnActivated(Self);
	end;
end;

procedure TWinFileSystemMonitor.Deactivate;
begin
	if not IsActive then begin
		Exit;
	end;

	TFolderMonWorker(FWorker).Owner := nil;
	TFolderMonWorker(FWorker).MaxTerminateTime := 100;
	TFolderMonWorker(FWorker).Suspended := False;
	//TFolderMonWorker(FWorker).Start;
	TFolderMonWorker(FWorker).DoTerminate;

	FWorker := nil;
	if Assigned(FOnDeactivated) then begin
		FOnDeactivated(Self);
	end;
end;

destructor TWinFileSystemMonitor.Destroy;
begin
	Deactivate;
	inherited;
end;

function TWinFileSystemMonitor.GetIsActive: boolean;
begin
	Result := FWorker <> nil;
end;

procedure TWinFileSystemMonitor.SetFolder(const Value: string);
begin
	if LowerCase(FFolder) = LowerCase(Value) then begin
		Exit;
	end;

	if IsActive then begin
		raise Exception.Create('Monitoração deve ser desativada antes de alterar caminho observado.');
	end;

	FFolder := Value;
end;

procedure TWinFileSystemMonitor.SetIsActive(const Value: boolean);
begin
	if Value then begin
		Activate;
	end else begin
		Deactivate;
	end;
end;

procedure TWinFileSystemMonitor.SetMonitoredChanges(const Value: TChangeTypes);
begin
	if FMonitoredChanges = Value then begin
		Exit;
	end;

	if IsActive then begin
		raise Exception.Create('The monitor must be deactivated before changing the monitored event(s)');
	end;
	FMonitoredChanges := Value;
end;

procedure TWinFileSystemMonitor.SetMonitorSubFolders(const Value: boolean);
begin
	if FMonitorSubFolders = Value then begin
		Exit;
	end;
	if IsActive then begin
		raise Exception.Create('Please deactivate the monitor first');
	end;
	FMonitorSubFolders := Value;
end;

{ TFolderMonWorker }

constructor TFolderMonWorker.Create(AOwner: TWinFileSystemMonitor);
begin
	//ajustado para compatibilidade com o mecanismo original do componente
	Self.FreeOnTerminate := True;
	//fim ajustes

	Owner := AOwner;
	if Owner = nil then begin
		raise Exception.Create('Reference to TFolderMon instance must be specified');
	end;
	inherited Create(False, Self.ClassName);
	Self.MaxTerminateTime := 100;
	FreeOnTerminate := True;
	SetUp;
end;

const
	FILE_LIST_DIRECTORY = $0001;

type
	_FILE_NOTIFY_INFORMATION = packed record
		NextEntryOffset: DWORD;
		Action: DWORD;
		FileNameLength: DWORD;
		FileName: widechar;
	end;

	FILE_NOTIFY_INFORMATION  = _FILE_NOTIFY_INFORMATION;
	PFILE_NOTIFY_INFORMATION = ^FILE_NOTIFY_INFORMATION;

procedure TFolderMonWorker.DoFolderItemChange;
begin
	if Assigned(Owner) and Assigned(Owner.FOnFOlderChange) then begin
		Owner.FOnFOlderChange(Owner, FFolderItemInfo);
	end;
end;

procedure TFolderMonWorker.Execute;
const
	cBufSize = 32 * 1024; //32k
var
	B:         Pointer;
	vCount:    DWORD;
	vOffset:   DWORD;
	vFileInfo: PFILE_NOTIFY_INFORMATION;
begin
	{$WARN UNSAFE_CODE OFF}
	inherited;
	B := GetMemory(cBufSize);
	try
		while not Terminated do begin
			if Owner = nil then begin
				Exit;
			end;

			if ReadDirectoryChangesW(FFolder, B, cBufSize, Owner.Recursive, FMonFilter, PDWORD(@vCount), nil, nil) and (vCount > 0)
			then begin
				if Owner = nil then begin
					Exit;
				end;

				vFileInfo := B;
				repeat
					vOffset := vFileInfo.NextEntryOffset;

					FFolderItemInfo.Name := WideCharLenToString(@vFileInfo^.FileName, vFileInfo^.FileNameLength);
					SetLength(FFolderItemInfo.Name, vFileInfo^.FileNameLength div 2);
					case vFileInfo^.Action of
						FILE_ACTION_ADDED: begin
								FFolderItemInfo.Action := faNew;
							end;
						FILE_ACTION_REMOVED: begin
								FFolderItemInfo.Action := faRemoved;
							end;
						FILE_ACTION_MODIFIED: begin
								FFolderItemInfo.Action := faModified;
							end;
						FILE_ACTION_RENAMED_OLD_NAME: begin
								FFolderItemInfo.Action := faRenamedOld;
							end;
						FILE_ACTION_RENAMED_NEW_NAME: begin
								FFolderItemInfo.Action := faRenamedNew;
							end;
					end;
					Synchronize(DoFolderItemChange);
					PByte(vFileInfo) := PByte(DWORD(vFileInfo) + vOffset);
				until vOffset = 0;
			end;
		end;
	finally
		TearDown;
		FreeMemory(B);
	end;
	{$WARN UNSAFE_CODE ON}
end;

procedure TFolderMonWorker.SetUp;
var
	i: TChangeType;
begin
	FFolder := CreateFile(PChar(Owner.Folder), FILE_LIST_DIRECTORY or GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE or
		FILE_SHARE_DELETE, nil, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, 0);

	FMonFilter := 0;
	for i := low(TChangeType) to high(TChangeType) do begin
		if i in Owner.MonitoredChanges then begin
			FMonFilter := FMonFilter or NOTIFY_FILTERS[i];
		end;
	end;
end;

procedure TFolderMonWorker.TearDown;
begin
	CloseHandle(FFolder);
end;

end.
