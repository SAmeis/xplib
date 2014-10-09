{$IFDEF SimpleProgress}
{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I DlgLib.inc}
unit SimpleProgress;

interface

uses
	Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls, Buttons, ExtCtrls, Gauges, Super, Messages;

type
	EProgressException       = class(Exception);
	TSimpleProgress          = class;
	TProgressUpdateEvent     = procedure(Sender: TSimpleProgress) of object;
	TProgressCloseQueryEvent = TCloseQueryEvent;

	TSimpleProgressForm = class(TForm)
		CancelBtn: TBitBtn;
		Gauge: TGauge;
		DescriptionLabel: TLabel;
		ProgressTimer: TTimer;
		procedure ProgressTimerTimer(Sender: TObject);
		procedure FormShow(Sender: TObject);
		procedure CancelBtnClick(Sender: TObject);
	private
		{ Private declarations }
		ErrorCode     : Integer;
		ErrorMessage  : string;
		Running       : boolean;
		OwnerComponent: TSimpleProgress;
		procedure UMProgressStart(var Message: TMessage); message Super.UM_PROGRESS_START;
	public
		{ Public declarations }
		constructor Create(AOwner: TComponent); override;
	end;

	TSimpleProgress = class(TComponent)
	private
		FDescription      : string;
		FCaption          : TCaption;
		FOnUpdateProgress : TProgressUpdateEvent;
		FCanCancel        : boolean;
		FMaxValue         : Integer;
		FMinValue         : Integer;
		FProgress         : Integer;
		FOnExecuteProgress: TProgressUpdateEvent;
		FOnCancel         : TProgressUpdateEvent;
		FOnCancelQuery    : TProgressCloseQueryEvent;
		FOnExecuteError   : TProgressUpdateEvent;
		procedure SetCaption(const Value: TCaption);
		procedure SetDescription(const Value: string);
		procedure SetCanCancel(const Value: boolean);
		function GetMaxValue: Integer;
		function GetMinValue: Integer;
		function GetProgress: Integer;
		procedure SetMaxValue(const Value: Integer);
		procedure SetMinValue(const Value: Integer);
		procedure SetProgress(const Value: Integer);
		function GetRunning: boolean;
		function GetFinished: boolean;
		procedure SetFinished(const Value: boolean);
	protected
		Dlg: TSimpleProgressForm;
	public
		function Execute(): Integer;
		property Running: boolean read GetRunning;
		property Finished: boolean read GetFinished write SetFinished;
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
	published
		property CanCancel        : boolean read FCanCancel write SetCanCancel default True;
		property Caption          : TCaption read FCaption write SetCaption;
		property Description      : string read FDescription write SetDescription;
		property MaxValue         : Integer read GetMaxValue write SetMaxValue stored True;
		property MinValue         : Integer read GetMinValue write SetMinValue stored True;
		property OnUpdateProgress : TProgressUpdateEvent read FOnUpdateProgress write FOnUpdateProgress;
		property OnExecuteProgress: TProgressUpdateEvent read FOnExecuteProgress write FOnExecuteProgress;
		property OnExecuteError   : TProgressUpdateEvent read FOnExecuteError write FOnExecuteError;
		property OnCancelQuery    : TProgressCloseQueryEvent read FOnCancelQuery write FOnCancelQuery;
		property OnCancel         : TProgressUpdateEvent read FOnCancel write FOnCancel;
		property Progress         : Integer read GetProgress write SetProgress;
	end;

var
	SimpleProgressForm: TSimpleProgressForm;

implementation

{$R *.DFM}

constructor TSimpleProgress.Create(AOwner: TComponent);
//----------------------------------------------------------------------------------------------------------------------------------
begin
	inherited;
	Self.Dlg        := nil;
	Self.FCanCancel := True;
end;

destructor TSimpleProgress.Destroy;
//----------------------------------------------------------------------------------------------------------------------------------
begin
	if Assigned(Self.Dlg) then begin
		Self.Dlg.Free;
	end;
	inherited;
end;

function TSimpleProgress.Execute: Integer;
//----------------------------------------------------------------------------------------------------------------------------------
begin
	Result := ERROR_SUCCESS;
	if not Assigned(Self.Dlg) then begin
		Application.CreateForm(TSimpleProgressForm, Self.Dlg);
		Self.Dlg.OwnerComponent := Self;
	end;
	try
		try
			//Inicializa componentes do dialogo
			Dlg.Gauge.MinValue           := Self.FMinValue;
			Dlg.Gauge.MaxValue           := Self.FMaxValue;
			Dlg.Gauge.Progress           := Self.FProgress;
			Dlg.CancelBtn.Enabled        := Self.FCanCancel;
			Dlg.Caption                  := Self.FCaption;
			Dlg.DescriptionLabel.Caption := Self.FDescription;
			//Exibe o dialogo com os valores pre-setados
			Dlg.ErrorCode := ERROR_SUCCESS;
			Dlg.ShowModal;
			Result := Dlg.ErrorCode;
			if Dlg.ErrorMessage <> EmptyStr then begin
				raise EProgressException.Create(Dlg.ErrorMessage);
			end;
		except
			on E: Exception do begin
				if not(E is EProgressException) then begin
					Result := ERROR_CONTINUE; //Nao pode executar operacao
				end;
				raise EProgressException.Create(E.Message);
			end;
		end;
	finally
		FreeAndNil(Self.Dlg);
	end;
end;

function TSimpleProgress.GetFinished: boolean;
//----------------------------------------------------------------------------------------------------------------------------------
begin
	Result := not Self.Running;
end;

function TSimpleProgress.GetMaxValue: Integer;
//----------------------------------------------------------------------------------------------------------------------------------
begin
	if Assigned(Self.Dlg) then begin
		Result := Self.Dlg.Gauge.MaxValue;
	end else begin
		Result := Self.FMaxValue;
	end;
end;

function TSimpleProgress.GetMinValue: Integer;
//----------------------------------------------------------------------------------------------------------------------------------
begin
	if Assigned(Self.Dlg) then begin
		Result := Self.Dlg.Gauge.MinValue;
	end else begin
		Result := Self.FMinValue;
	end;
end;

function TSimpleProgress.GetProgress: Integer;
//----------------------------------------------------------------------------------------------------------------------------------
begin
	if Assigned(Self.Dlg) then begin
		Result := Self.Dlg.Gauge.Progress;
	end else begin
		Result := Self.FProgress;
	end;
end;

function TSimpleProgress.GetRunning: boolean;
//----------------------------------------------------------------------------------------------------------------------------------
begin
	Result := Assigned(Self.Dlg) and (Self.Dlg.Visible);
end;

procedure TSimpleProgress.SetCanCancel(const Value: boolean);
//----------------------------------------------------------------------------------------------------------------------------------
begin
	FCanCancel := Value;
	if Assigned(Self.Dlg) then begin
		Dlg.CancelBtn.Enabled := Value;
	end;
end;

procedure TSimpleProgress.SetCaption(const Value: TCaption);
//----------------------------------------------------------------------------------------------------------------------------------
begin
	FCaption := Value;
	if Assigned(Self.Dlg) then begin
		Dlg.Caption := Value;
	end;
end;

procedure TSimpleProgress.SetDescription(const Value: string);
//----------------------------------------------------------------------------------------------------------------------------------
begin
	FDescription := Value;
	if Assigned(Self.Dlg) then begin
		Dlg.DescriptionLabel.Caption := Value;
	end;
end;

procedure TSimpleProgress.SetFinished(const Value: boolean);
//----------------------------------------------------------------------------------------------------------------------------------
begin
	if Assigned(Self.Dlg) and (Value) then begin //Ignora valores Value = false
		Self.Dlg.Close;
		Self.Dlg.ModalResult := mrIgnore;
		Self.Dlg.ErrorCode   := ERROR_SUCCESS; //Me disse que acabou!!!
	end;
end;

procedure TSimpleProgress.SetMaxValue(const Value: Integer);
//----------------------------------------------------------------------------------------------------------------------------------
begin
	Self.FMaxValue := Value;
	if Assigned(Self.Dlg) then begin
		Dlg.Gauge.MaxValue := Value;
	end;
end;

procedure TSimpleProgress.SetMinValue(const Value: Integer);
//----------------------------------------------------------------------------------------------------------------------------------
begin
	Self.FMinValue := Value;
	if Assigned(Self.Dlg) then begin
		Dlg.Gauge.MinValue := Value;
	end;
end;

procedure TSimpleProgress.SetProgress(const Value: Integer);
//----------------------------------------------------------------------------------------------------------------------------------
begin
	Self.FProgress := Value;
	if Assigned(Self.Dlg) then begin
		Dlg.Gauge.Progress := Value;
	end;
end;

{ TSimpleProgressForm }

constructor TSimpleProgressForm.Create(AOwner: TComponent);
//----------------------------------------------------------------------------------------------------------------------------------
begin
	inherited;
	ErrorMessage := EmptyStr;
	Self.Running := False;
end;

procedure TSimpleProgressForm.ProgressTimerTimer(Sender: TObject);
//----------------------------------------------------------------------------------------------------------------------------------
begin
	if Assigned(Self.OwnerComponent.OnUpdateProgress) then begin
		Running                    := Self.Visible; //Indica que o progresso foi iniciado
		Self.ProgressTimer.Enabled := False;        //Impede sobre-posicao de chamadas
		try
			Self.OwnerComponent.OnUpdateProgress(Self.OwnerComponent);
		except
			on E: Exception do begin
				Self.ProgressTimer.Enabled := False;
				//dispara evento de erro
				if (Assigned(Self.OwnerComponent.OnExecuteError)) then begin
					Self.OwnerComponent.OnExecuteError(Self.OwnerComponent);
				end;
				Self.Close;
				Self.ErrorMessage := E.Message;
				Self.ErrorCode    := ERROR_CONTINUE; //Nao pode executar operacao, Chamador que se vire
				Exit;
			end;
		end;
		if Self.OwnerComponent.Running then begin
			Self.Gauge.MinValue := Self.OwnerComponent.MinValue;
			Self.Gauge.MaxValue := Self.OwnerComponent.MaxValue;
			Self.Gauge.Progress := Self.OwnerComponent.Progress;
		end else begin
			Self.Close;
			if (Self.OwnerComponent.Progress >= Self.OwnerComponent.MaxValue) then begin
			//Assume que operacao foi totalmente concluida
				Self.ModalResult := mrOk;
			end else begin
				if Self.ModalResult <> mrCancel then begin
					Self.ModalResult := mrIgnore;
					Self.ErrorCode   := ERROR_CAN_NOT_COMPLETE;
				end else begin
					Self.ErrorCode := ERROR_CANCELLED; //Cancelada pelo usuario
				end;
			end;
		end;
		Self.ProgressTimer.Enabled := True; //Restaura novos incrementos de progresso
	end else begin
		Self.Close;
	end;
end;

procedure TSimpleProgressForm.FormShow(Sender: TObject);
//----------------------------------------------------------------------------------------------------------------------------------
begin
	Application.ProcessMessages;
	PostMessage(Self.Handle, UM_PROGRESS_START, 0, 0); //Apos estabilizar inicia processo
end;

procedure TSimpleProgressForm.UMProgressStart(var Message: TMessage);
//----------------------------------------------------------------------------------------------------------------------------------
begin
	if Assigned(Self.OwnerComponent.OnExecuteProgress) then begin
		try
			Self.ProgressTimer.Enabled := True;                         //Dispara os eventos para a magica ocorrer
			Self.OwnerComponent.OnExecuteProgress(Self.OwnerComponent); //Callback para inicio proesso
		except
			on E: Exception do begin
				Self.ProgressTimer.Enabled := False;
				Application.ProcessMessages;
				Self.Close;
				Self.ModalResult  := mrIgnore;
				Self.ErrorMessage := E.Message;
				Self.ErrorCode    := ERROR_CONTINUE; //Nao pode executar operacao, Chamador que se vire
				Exit;
			end;
		end;
		Self.Close;
	end else begin
		Self.Close;
		if Self.ModalResult <> mrCancel then begin
			Self.ErrorCode := ERROR_SUCCESS; //Nada pedido -> Nada feito
		end;
	end;
end;

procedure TSimpleProgressForm.CancelBtnClick(Sender: TObject);
//----------------------------------------------------------------------------------------------------------------------------------
var
	ReallyCancel: boolean;
begin
	if (Assigned(Self.OwnerComponent.FOnCancelQuery)) then begin
		ReallyCancel := True;
		Self.OwnerComponent.FOnCancelQuery(Self.OwnerComponent, ReallyCancel);
		if (not ReallyCancel) then begin
			Exit;
		end;
	end;
	Self.ErrorCode := ERROR_CANCELLED;
	Self.Visible   := False;
	Self.Close;
	if (Assigned(Self.OwnerComponent.FOnCancel)) then begin
		Self.OwnerComponent.FOnCancel(Self.OwnerComponent);
	end;
end;

end.
