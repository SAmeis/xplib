{$A+,B-,C-,D-,E-,F-,G+,H+,I-,J+,K-,L-,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y-,Z1}
unit CopyDlg;

interface

{$IFDEF WIN32}
uses
  Windows, SysUtils, Messages, Classes, Graphics, Controls, Forms, Dialogs, Super, CpDlgFrm, ExtCtrls, FileHnd;
{$ELSE}
uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls, Forms, Dialogs, Super, CpDlgFrm, ExtCtrls,
  FileHnd;

{$ENDIF}


const
	_MIN_BLOCK_SIZE_ = 512;
	_MAX_BLOCK_SIZE_ = 40960;
	_MIN_COPYDIALOG_INTERVAL_INTERVAL = 200;
const
	CDE_NO_ERROR		  	=  0;
	CDE_SOURCE_INACESSIBLE 	= -1;
	CDE_SOURCE_LOCKED		= -2;
	CDE_OUT_OF_SPACE	  	= -3;
	CDE_IO_ERROR		  	= -4;
	CDE_USER_CANCELED		= -5;
	CDE_UNKNOW				= -6;



type
	TCopyDialogMode = (cdmPreserve, cdmDelAfter, cdmDelWhile);
	TCopyStatusUpdateMode = (sumStep, sumTimer, sumNone, sumBlock);

type
	TNotifyCopyError = procedure(Sender : TObject;  var Continue : boolean) of Object;

	TNotifyCopyStatusUpdate = procedure(Sender : TObject; var Continue : boolean) of Object;

type
  TCopyDialog = class(TComponent)
  private
	 FCaption : string;
	 FBlockSize : integer;
	 FIndex : integer;
	 FTimer : TTimer;
	 FCanOverWrite : boolean;
	 FCopyMode : TCopyDialogMode;
	 FOnError : TNotifyCopyError;
	 FUpdateMode : TCopyStatusUpdateMode;
	 FOnStatusUpdate : TNotifyCopyStatusUpdate;
	 FActualSource : string;
	 FActualDest : string;
	 FSourceItems : TStringList;
	 FDestItems : TStringList;
	 FBytesTransfered : longint;
	 FCanCancel : boolean;
	 FProgress : integer;
	 FFileLength : longint;
	 function GaugedCopyFile : boolean;
  protected
	 { Protected declarations }
	 StatusForm : TCopyStatusFrm;
	 FLastError : integer;
	 ProcessNextFile : boolean;
	 procedure Error(var Msg : TMsg); message UM_COPY_ERROR;
	 procedure TimerUpdate(var Msg : TMsg); message WM_TIMER;
	 procedure StatusUpdate(var Msg : TMsg); message UM_PULSE;
	 procedure Start(Sender : TObject);
	 procedure SetBlockSize(Value : integer);
	 function GetInterval : word;
	 procedure SetInterval(value : word);
	 procedure SetCopyMode(Value : TCopyDialogMode);
  public
	 Handle : THandle;
	 constructor Create(AOwner : TComponent); override;
	 destructor Destroy; override;
	 function ExecuteModal : TModalResult;
	 procedure AddToList(const SourceFile, DestFile : string);
	 procedure Resume;
	 procedure Cancel(Sender : TObject);
	 property ActualSource : string read FActualSource;
	 property ActualDest : string read FActualDest;
	 property SourceItems : TStringList read FSourceItems;
	 property DestItems : TStringList read FDestItems;
	 property Index : integer read FIndex default 0;
	 property LastError : integer read FLastError;
	 property Progress : integer read FProgress;
	 property BytesTransfered : longint read FBytesTransfered;
  published
	 {Propriedades}
	 property OnCopyError : TNotifyCopyError read FOnError write FOnError;
	 property OnStatusUpdate : TNotifyCopyStatusUpdate read FOnStatusUpdate write FOnStatusUpdate;
	 property Caption : string read FCaption write FCaption;
	 property CanOverWrite : boolean read FCanOverWrite write FCanOverWrite default false;
	 property CopyMode : TCopyDialogMode read FCopyMode write SetCopyMode;
	 property Interval : word read GetInterval write SetInterval;
	 property BlockSize : integer read FBlockSize write SetBlockSize default 4096;
	 property UpdateMode : TCopyStatusUpdateMode read FUpdateMode write FUpdateMode;
	 property CanCancel : boolean read FCanCancel write FCanCancel default true;
	 {Eventos}
  end;

  procedure Register;

implementation
{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
constructor TCopyDialog.Create(AOwner : TComponent);
begin
	inherited Create(AOwner);
	FSourceItems:=TStringList.Create;
	FDestItems:=TStringList.Create;
	FBlockSize:=4096;
	FIndex:=0;
   FCanCancel:=True;
	FTimer:=nil;
end;

{------------------------------------------------------------------------------}
destructor TCopyDialog.Destroy;
begin
	FSourceItems.Destroy;
	FDestItems.Destroy;
	inherited Destroy;
end;
{------------------------------------------------------------------------------}
procedure TCopyDialog.TimerUpdate(var Msg : TMsg);
begin

end;
{------------------------------------------------------------------------------}
function TCopyDialog.ExecuteModal : TModalResult;
begin
	Self.StatusForm:=TCopyStatusFrm.Create(Self);
	Self.StatusForm.Caption:=Self.Caption;
	Self.Handle:=Self.StatusForm.Handle;
	Self.StatusForm.CancelBtn.OnClick:=Self.Cancel;
	Self.StatusForm.CancelBtn.Visible:=FCanCancel;

	{Self.StatusForm.OnActivate:=Self.Start;}
	Self.StatusForm.OnPaint:=Self.Start; {Resolveu a persistencia de aparecer apos final}

	{Inicia processo ativado pela mensagem de exibicao do form}
	Self.FIndex:=0;
	Result:=Self.StatusForm.ShowModal;
	if Result = mrOK then begin
		Self.FLastError:=CDE_NO_ERROR;
	end;
	PostMessage(Self.StatusForm.Handle, WM_CLOSE, 0,0);
	Application.ProcessMessages;
	Self.StatusForm.Free;
end;


{------------------------------------------------------------------------------}
procedure TCopyDialog.Error(var Msg : TMsg);
var
	Continue : boolean;
begin
	if Assigned(FOnError) then begin
	   Continue := True;
	   FOnError(Self ,Continue );   {Executa rotina do usuario}
	   if Continue then begin
		   Resume;
	   end else begin
		   Cancel(Self);
	   end;
	end else begin
	   Cancel(Self);
	end;
end;
{------------------------------------------------------------------------------}
function TCopyDialog.GaugedCopyFile : boolean;
var
	Src, Dest : File;
	BytesWrited, BytesRead, i : integer;
	Query : TModalResult;
	Buf : pointer;
	Continue, SectionOverWrite : boolean;

{..............................................................................}

procedure LocalAbort(Error : integer);
begin
	 FLastError:=Error;
	 SendMessage(Handle, UM_COPY_ERROR, 0, 0);
	 FreeMem(Buf, FBlockSize + 1);
	 CloseFile(Src);
end;

{..............................................................................}

begin
	Result:=False;
	SectionOverWrite:=FCanOverWrite;
	i:=FIndex-1;
	repeat
		Inc(i);
		ProcessNextFile:=True;
		Self.FIndex:=i + 1; {Atualiza contador}
		FActualSource:=SourceItems.Strings[i];
		if FileExists(ActualSource) then begin
			FActualDest:=DestItems.Strings[i];
			if (FileExists(FActualDest)) and ( not(SectionOverWrite) )then begin
				Query:=MessageDlg('Arquivo '+ FActualDest + 'já existe.'#13 +
								  'Deseja sobre-escrever?', mtConfirmation,
								  [mbYES, mbALL, mbNO], 0);
				if Query = mrAll then begin
					SectionOverWrite:=True;
				end;
				if not((Query = mrYes) or (FCanOverWrite)) then begin
					Break; {Passa p/ proximo arquivo}
				end;
			end;
			{Inicia a copia}
			AssignFile(Src,ActualSource);
			AssignFile(Dest, FActualDest);
			try
				Reset(Src,1);
			Except
				FLastError:=CDE_SOURCE_LOCKED;
				if Assigned(FOnError) then begin
					Continue := True;
					FOnError(Self, Continue);
					if Continue then begin
						Dec(i);
						System.Continue;
					end else begin
						Exit;
					end;
				end;
			end;
			FFileLength:=FileSize(Src); {Atualiza tamanho do arquivo}
			if SpaceFree(FActualDest) < FFileLength then begin
				FLastError:=CDE_OUT_OF_SPACE;
				if Assigned(FOnError) then begin
					Continue := True;
					FOnError(Self, Continue);
					if Continue then begin
						Dec(i);
						System.Continue;
					end else begin
						CloseFile(Src);
						FLastError:=CDE_USER_CANCELED;
						Exit;
					end;
				end;
			end;
			Try
				ReWrite(Dest,1);
				CloseFile(Dest);
			Except
				FLastError:=CDE_UNKNOW;
				if Assigned(FOnError) then begin
					Continue := True;
					FOnError(Self, Continue);
					if Continue then begin
						Dec(i);
						CloseFile(Src);
						System.Continue;
					end else begin
						CloseFile(Src);
						Exit;
					end;
				end;
				Exit;
			end;
			GetMem(Buf, FBlockSize + 1);
			Self.FBytesTransfered:=0;
			While not(Eof(Src)) do begin
				try
					BlockRead(Src, Buf^, FBlockSize, BytesRead);
				except
					LocalAbort(CDE_UNKNOW);
					Exit;
				end;
				try
					Reset(Dest,1);
					Seek(Dest, FileSize(Dest));
					BlockWrite(Dest, Buf^, BytesRead, BytesWrited);
					CloseFile(Dest);
				except
					LocalAbort(CDE_UNKNOW);
					Exit;
				end;
				if BytesWrited <> BytesRead then begin
					LocalAbort(CDE_UNKNOW);
					Exit;
				end;
				FBytesTransfered:=FBytesTransfered + BytesWrited;
				FProgress:=trunc(100*(FBytesTransfered / FFileLength));
				{Forca a atualizacao}
				if Assigned(FOnStatusUpdate) then begin
					FOnStatusUpdate(Self, Continue);
					if Continue then begin
					end else begin
						{Termina copia}
					end;
				end;
				Self.StatusForm.SourceLbl.Caption:=Self.FActualSource;
				Self.StatusForm.DestLbl.Caption:=Self.FActualDest;
				Self.StatusForm.Gauge.Progress:=Self.FProgress;
				Self.StatusForm.DestLbl.Repaint;
				Self.StatusForm.SourceLbl.Repaint;
				Self.StatusForm.Gauge.Repaint;
				Application.ProcessMessages;
				{Final de atualizacao de status}
				if not(ProcessNextFile) then begin
					LocalAbort(CDE_USER_CANCELED);
					Exit;
				end;
			end;
			FreeMem(Buf, FBlockSize + 1);
			CloseFile(Src);
		end else begin
			FLastError:=CDE_SOURCE_INACESSIBLE;
			if Assigned(FOnError) then begin
				Continue := True;
				FOnError(Self, Continue);
				if Continue then begin
					if MessageDlg('Deseja tentar novamente?', mtConfirmation,
								   [mbYes, mbNo], 0) = mrYes then begin
						Dec(i);
					end;
					System.Continue;
				end;
			end;
			Exit;
		end;
	until (i >= (Self.SourceItems.Count -1));
	Result:=True;
	StatusForm.Close;
end;
{------------------------------------------------------------------------------}
procedure TCopyDialog.SetBlockSize(Value : integer);
begin
	if (Value > _MIN_BLOCK_SIZE_) and (Value < _MAX_BLOCK_SIZE_ ) then begin
		FBlockSize:=Value;
	end;
end;

{------------------------------------------------------------------------------}
procedure TCopyDialog.Resume;
begin
	{Retorna de onde parou}
end;
{------------------------------------------------------------------------------}
procedure TCopydialog.SetInterval(Value : word);
begin
	if Assigned(FTimer) and (Value > _MIN_COPYDIALOG_INTERVAL_INTERVAL) then
		begin
		FTimer.Interval:=Value;
	end;
end;
{------------------------------------------------------------------------------}
function TCopydialog.GetInterval : word;
begin
	if Assigned(FTimer) then begin
		Result:=FTimer.Interval;
	end else begin
		Result:=0;
	end
end;

{------------------------------------------------------------------------------}
procedure TCopyDialog.SetCopyMode(Value : TCopyDialogMode);
begin
	{Permite alteracao apenas antes do inicio da operacao}
	if FIndex = 0 then begin
		FCopyMode:=Value;
	end;
end;
{------------------------------------------------------------------------------}
procedure TCopyDialog.Cancel(Sender : TObject);
begin
	Self.ProcessNextFile:=False;
	Self.StatusForm.Close;
end;
{------------------------------------------------------------------------------}
procedure TCopyDialog.Start(Sender : TObject);
begin
	if Self.Index = 0 then begin
		GaugedCopyFile;
		Self.StatusForm.ModalResult:=mrOK;
		{Self.StatusForm.Close;}
	end else begin
		Self.StatusForm.OnPaint:=nil; {jogada esperta para desvincular chamada do start}
		Self.StatusForm.SourceLbl.Repaint;
		Self.StatusForm.DestLbl.Repaint;
		Self.StatusForm.Gauge.Repaint;
		Self.StatusForm.RePaint;
	end;
end;
{------------------------------------------------------------------------------}
procedure TCopyDialog.AddToList(const SourceFile, DestFile : string);
begin
	Self.FSourceItems.Add(SourceFile);
	Self.FDestItems.Add(DestFile);
end;

{------------------------------------------------------------------------------}
procedure TCopyDialog.StatusUpdate(var Msg: TMsg);
var
	Continue : boolean;
begin
	if Assigned(FOnStatusUpdate) then begin
		FOnStatusUpdate(Self, Continue);
		if Continue then begin

		end else begin
			Cancel(Self);
		end;
	end else begin
		Self.StatusForm.SourceLbl.Caption:=Self.FActualSource;
		Self.StatusForm.DestLbl.Caption:=Self.FActualDest;
		Self.StatusForm.Gauge.Progress:=Self.FProgress;
		Self.StatusForm.Refresh;
	end;
end;



procedure Register;

begin
	RegisterComponents('Super', [TCopyDialog]);
end;

{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
end.
