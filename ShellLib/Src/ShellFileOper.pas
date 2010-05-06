unit ShellFileOper;

interface

uses
	Windows, Messages, SysUtils, Classes, Controls;


type
  TFileOperation = (foCopy, foDelete, foMove, foRename);
  TFileOperFlag = (flAllowUndo, flConfirmMouse, flFilesOnly, flMultiDest, flNoConfirmation, flNoConfirmMkDir,
					flRenameOnCollision, flSilent, flSimpleProgress, flNoErrorUI);
  TFileOperFlags = set of TFileOperFlag;

  TShellFileOperator = class(TComponent)
  private
	 FAborted: Boolean;
	 FOperation: TFileOperation;
	 FOptions: TFileOperFlags;
	 FProgressTitle: string;
	 FSource: string;
	 FDestination: string;
	 function TaskModalDialog(DialogFunc: Pointer; var DialogData): Boolean;
  public
	 constructor Create(AOwner: TComponent); override;
	 function Execute: Boolean; virtual;
	 property Aborted: Boolean read FAborted;
  published
	 property Destination: string read FDestination write FDestination;
	 property Operation: TFileOperation read FOperation write FOperation default foCopy;
	 property Options: TFileOperFlags read FOptions write FOptions default [flAllowUndo, flNoConfirmMkDir];
	 property ProgressTitle: string read FProgressTitle write FProgressTitle;
	 property Source: string read FSource write FSource;
  end;

  procedure Register;


implementation

uses
	ShellAPI, Forms;
  (* OleAuto, {$ENDIF} {$ENDIF} DateUtil, ActiveX,{$ELSE} Ole2, ComObj, *)


constructor TShellFileOperator.Create(AOwner: TComponent);
//----------------------------------------------------------------------------------------------------------------------------------
begin
  inherited Create(AOwner);
  FOptions := [flAllowUndo, flNoConfirmMkDir];
end;


function TShellFileOperator.TaskModalDialog(DialogFunc: Pointer; var DialogData): Boolean;
//----------------------------------------------------------------------------------------------------------------------------------
type
  TDialogFunc = function(var DialogData): Integer stdcall;
var
  ActiveWindow: HWnd;
  WindowList: Pointer;
begin
  ActiveWindow := GetActiveWindow;
  WindowList := DisableTaskWindows(0);
  try
	 Result := TDialogFunc(DialogFunc)(DialogData) = 0;
  finally
	 EnableTaskWindows(WindowList);
	 SetActiveWindow(ActiveWindow);
  end;
end;

function TShellFileOperator.Execute: Boolean;
//----------------------------------------------------------------------------------------------------------------------------------
const
  OperTypes: array[TFileOperation] of UINT = (
	 FO_COPY, FO_DELETE, FO_MOVE, FO_RENAME);
  OperOptions: array[TFileOperFlag] of FILEOP_FLAGS = (
	 FOF_ALLOWUNDO, FOF_CONFIRMMOUSE, FOF_FILESONLY, FOF_MULTIDESTFILES,
	 FOF_NOCONFIRMATION, FOF_NOCONFIRMMKDIR, FOF_RENAMEONCOLLISION,
	 FOF_SILENT, FOF_SIMPLEPROGRESS, FOF_NOERRORUI);
var
  OpStruct: TSHFileOpStruct;
  Flag: TFileOperFlag;
//................................................................................................................................
  function AllocFileStr(const S: string): PChar;
  var
	 P: PChar;
  begin
	 Result := nil;
	 if S <> '' then begin
	   Result := StrCopy(StrAlloc(Length(S) + 2), PChar(S));
	   P := Result;
	   while P^ <> #0 do begin
		 if (P^ = ';') or (P^ = '|') then P^ := #0;
		 Inc(P);
	   end;
	   Inc(P);
	   P^ := #0;
	 end;
  end;
//................................................................................................................................
begin
	 FAborted := False;
	 FillChar(OpStruct, SizeOf(OpStruct), 0);
	 with OpStruct do begin
		 try
		   if (Application.MainForm <> nil) and Application.MainForm.HandleAllocated then begin
			  Wnd := Application.MainForm.Handle
		   end else begin
			  Wnd := Application.Handle;
		   end;
		   wFunc := OperTypes[Operation];
		   pFrom := AllocFileStr(FSource);
		   pTo := AllocFileStr(FDestination);
		   fFlags := 0;
		   for Flag := Low(Flag) to High(Flag) do begin
			   if Flag in FOptions then begin
				   fFlags := fFlags or OperOptions[Flag];
			   end;
		   end;
		   lpszProgressTitle := PChar(FProgressTitle);
		   Result := TaskModalDialog(@SHFileOperation, OpStruct);
		   FAborted := fAnyOperationsAborted;
		 finally
		   if pFrom <> nil then begin
			  StrDispose(pFrom);
		   end;
		   if pTo <> nil then begin
			  StrDispose(pTo);
		   end;
		 end;
	 end;
end;

function GetAppOpenFile(FileName: string; var AppOpen: String): Boolean;
//----------------------------------------------------------------------------------------------------------------------------------
var
   app, dir: array[0..255] of char;
begin
//   dir  := 'c:\';
   { TODO -oBello/Roger -cDSG : Resolver qual valor deve ser no parametro 'Default Diretory' }
   Result := (FindExecutable(PChar(FileName), dir, app) >= 32 );
   if Result then begin
      AppOpen := app;
   end;
end;


procedure Register();
//----------------------------------------------------------------------------------------------------------------------------------
begin
	RegisterComponents( 'Win32', [ TShellFileOperator ] );
end;

end.
