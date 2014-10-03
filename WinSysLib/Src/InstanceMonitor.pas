{$IFDEF InstanceMonitor}
{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I WinSysLib.inc}
unit InstanceMonitor;

interface

uses
	Windows, Messages, SysUtils, Classes, Controls, Super;

type
	//Evento disparado qdo encontrado aplicativo irmao.
	TFindPreviousInstanceEvent = procedure(SiblingAppHandle, PipeWndHandle: THandle; cmdLine: string; var ContinueScan: boolean;
		var TerminateSelf: boolean) of object;
	//Mensagem a ser enviada para o aplicativo irmao no momento de sua deteccao
	TInterrogateInstanceEvent = procedure(var Message: TMessage) of object;

	TInstanceMonitor = class;

	TInstanceMonitorCaptureWinControl = class(TWinControl)
	private
		FMonitor: TInstanceMonitor;
	protected
		{ TODO -oRoger -cDSG : Complementar os outros identificadores de mensagens para UM_INSTANCE_ANNOUNCE na Super.pas }
		procedure AnnounceMessage(var Message: TMessage); message UM_INSTANCE_ANNOUNCE;
	public
		property Monitor: TInstanceMonitor read FMonitor;
		constructor CreateByInstanceMonitor(AOwner: TInstanceMonitor);
	end;

	TInstanceMonitor = class(TComponent)
	private
		MonitorWindow:     TInstanceMonitorCaptureWinControl;
		FCreateOrder:      DWORD;
		FMutexHandle:      THandle;
		FSignature:        string;
		FOnFindSiblingApp: TFindPreviousInstanceEvent;
		FOnFinishScan:     TNotifyEvent;
		FOnInterrogate:    TInterrogateInstanceEvent;
		FAppId:            Integer;
		FInstanceNumber:   Integer;
		procedure DestroyMutex();
		function GetRootWindow: TWinControl;
		{ Private declarations }
	protected
		{ Protected declarations }
		function ProcessExternalMessage(var Message: TMessage): Integer;
	public
		{ Public declarations }
		property CreateOrder:    DWORD read FCreateOrder;
		property InstanceNumber: Integer read FInstanceNumber;
		property RootWindow:     TWinControl read GetRootWindow;
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
		function Scan(): Integer;
	published
		{ Published declarations }
		property AppID:            Integer read FAppId write FAppId;
		property Signature:        string read FSignature write FSignature;
		property OnFindSiblingApp: TFindPreviousInstanceEvent read FOnFindSiblingApp write FOnFindSiblingApp;
		property OnFinishScan:     TNotifyEvent read FOnFinishScan write FOnFinishScan;
		property OnInterrogate:    TInterrogateInstanceEvent read FOnInterrogate write FOnInterrogate;
	end;

procedure Register;

implementation

uses
	Dialogs, APIHnd, WinHnd;

procedure Register;
//----------------------------------------------------------------------------------------------------------------------------------
begin
	RegisterComponents('Super', [TInstanceMonitor]);
end;

function EnumWindowsFunc(Handle: HWND; Param: LParam): boolean; stdcall;
//----------------------------------------------------------------------------------------------------------------------------------  function IsAPrevInstanceRunning: boolean;
var
	ClassName: array[0 .. 32] of char;

	iAppID:    Integer;
	Master:    TInstanceMonitor;
	ChildWnd:  THandle;
begin
	Result := True;
	{$WARN UNSAFE_CAST OFF}
	Master := TInstanceMonitor(Param); //Le classname para as top-level windows especificadas por Handle
	{$WARN UNSAFE_CAST ON}
	//GetClassName(Handle, ClassName, SizeOf(ClassName));
	GetClassName(Handle, ClassName, Length(ClassName)); { TODO -oroger -clib : Confirmar uso correto desta chamada comparar acima }
	if SameText(ClassName, Master.RootWindow.ClassName) then begin
		ChildWnd := WinHnd.FindWindowChildByClass(Handle, TInstanceMonitorCaptureWinControl.ClassName);
		if ChildWnd <> 0 then begin
			{ get the APP_ID for the application where the top-level window specified by Handle belongs }
			iAppID := SendMessage(ChildWnd, UM_INSTANCE_ANNOUNCE, 0, 0); //Originalmente UM_APP_ID_CHECK
			{ if ClassName matches to the this application's Main Form class name... }
			{ and iAppID matches to to the this application's APP_ID }
			//Result :=  (StrIComp(ClassName, MAIN_FORM_CLASS) = 0) and (iAppID = APP_ID);
			Result := (iAppID = Master.AppID);
		end;
	end;
end;

{ TInstanceMonitor }

constructor TInstanceMonitor.Create(AOwner: TComponent);
//----------------------------------------------------------------------------------------------------------------------------------
var
	Ret: Integer;
begin
	inherited;
	Self.FInstanceNumber := -1; //Indica valor indeterminado
	// **** Colocar atributo Active *****
	Self.FMutexHandle := CreateMutex(nil, True, PChar(Self.FSignature));
	Ret := GetLastError();
	if FMutexHandle <> 0 then begin //Consegui criar o Mutex, averiguar sua unicidade agora, usando GetLastError para isso
		if Ret = ERROR_ALREADY_EXISTS then begin //Ha outro alem deste
			Self.DestroyMutex(); //Coloca-se como nao sendo a instancia principal
		end;
	end else begin
		TAPIHnd.CheckAPI(Ret);
		RaiseLastOSError();
	end;
	MonitorWindow := TInstanceMonitorCaptureWinControl.CreateByInstanceMonitor(Self);
end;

destructor TInstanceMonitor.Destroy;
//----------------------------------------------------------------------------------------------------------------------------------
begin
	Self.DestroyMutex();
	Self.MonitorWindow.Free();
	inherited;
end;

procedure TInstanceMonitor.DestroyMutex;
//----------------------------------------------------------------------------------------------------------------------
begin
	if (Self.FMutexHandle <> 0) then begin
		CloseHandle(Self.FMutexHandle);
		Self.FMutexHandle := 0;
	end;
end;

function TInstanceMonitor.GetRootWindow(): TWinControl;
//----------------------------------------------------------------------------------------------------------------------------------
var
	AParent:   TComponent;
	FirstName: string;
begin
	AParent := Self.Owner;
	if AParent = nil then begin
		FirstName := 'nil';
	end else begin
		FirstName := AParent.Name;
	end;
	{ TODO -oRoger -cDSG : Verrificar forma de amarrar para a  instancia de TApplication como sendo o mais alto nivel das janelas do aplicativo }
	while (not(AParent is TWinControl)) and (AParent <> nil) do begin //Varre cadeia de Owner em busca de ajanelado
		AParent := AParent.Owner;
	end;
	if AParent = nil then begin
		raise Exception.CreateFmt('Não existe descendente de componente ajanelado na cadeia de %s', [FirstName]);
	end else begin
		Result := TWinControl(AParent);
	end;
end;

function TInstanceMonitor.ProcessExternalMessage(var Message: TMessage): Integer;
//----------------------------------------------------------------------------------------------------------------------------------
begin
	{ TODO -oRoger -cDSG : Tratar qual das mensagens chegou. }
	Result := 0;
	case message.Msg of
		UM_INSTANCE_ANNOUNCE: begin
				if Assigned(Self.FOnInterrogate) then begin
					{ TODO -oRoger -cDSG : Chamar este evento aqui }
					//Self.FOnInterrogate( Self );
				end;
			end;
	end;
end;

function TInstanceMonitor.Scan(): Integer;
//----------------------------------------------------------------------------------------------------------------------------------
var
	Ret: Integer;
begin
	Result := 0;
	//Self.DestroyMutex(); //Consegui criar o Mutex, averiguar sua unicidade agora, usando GetLastError para isso
	Self.FMutexHandle := CreateMutex(nil, True, PChar(Self.FSignature));
	Ret := GetLastError();
	if FMutexHandle <> 0 then begin //Consegui criar o Mutex, averiguar sua unicidade agora, usando GetLastError para isso
		if Ret = ERROR_ALREADY_EXISTS then begin //Ha outro alem deste
			Self.DestroyMutex(); //Coloca-se como nao sendo a instancia principal
			{ TODO -oRoger -cDSG : Iniciar busca por class da janela e/ou titulo }
			{$WARN UNSAFE_CAST OFF}
			EnumWindows(@EnumWindowsFunc, LPARAM(Self));
			{$WARN UNSAFE_CAST ON}
		end;
	end else begin //Neste caso deixa o mutex aberto para avisar a outras intancias
		TAPIHnd.CheckAPI(Ret);
	end;
end;

{ TInstanceMonitorCaptureWinControl }

procedure TInstanceMonitorCaptureWinControl.AnnounceMessage(var Message: TMessage);
//----------------------------------------------------------------------------------------------------------------------------------
begin
	if csDesigning in Self.ComponentState then begin
		message.Result := 0;
	end else begin
		message.Result := Self.Monitor.ProcessExternalMessage(message);
	end;
end;

constructor TInstanceMonitorCaptureWinControl.CreateByInstanceMonitor(AOwner: TInstanceMonitor);
//----------------------------------------------------------------------------------------------------------------------------------
begin
	Self.Name := 'TESTE_NAME';
	inherited CreateParented(AOwner.RootWindow.Handle);
	Self.FMonitor := AOwner;
	Self.HandleNeeded;
end;

end.
