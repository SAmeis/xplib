{$IFDEF DEBUG_DCU}
	{$DEFINE DateOper}
{$ENDIF}
{$IFDEF DateOper}
{$A+,B-,C-,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O-,P+,Q-,R+,S-,T-,U-,V+,W-,X+,Y+,Z4}
{$DEBUGINFO ON}
{$ELSE}
{$A+,B-,C-,D-,E-,F-,G+,H-,I-,J-,K-,L-,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y-,Z1}
{$F+}
{$ENDIF}

unit BlowPan;

interface

uses
	Windows, Classes, Controls, ExtCtrls;

type
	TBlowStyles   = (bsCenter,bsWipeRight,bsWipeLeft,bsWipeDown,bsWipeUp);
	TBlowStates   = (bstClosed,bstOpen);
	TBlowPanel    = class(TCustomPanel)
	private
	  FTimer      : TTimer;
	  FBlowSpeed  : Integer;
	  FBlowSteps  : Integer;
	  FBlowStyle  : TBlowStyles;
	  FBlowState  : TBlowStates;
	  OpenRect    : TRect;
	  ClosedRect  : TRect;
	  {Left, top, width & height when open}
	  {OL,OT,OW,OH : Integer;}
	  {Left, top, width & height when closed}
	  {CL,CT,CW,CH : Integer;}
	  CurrStep    : Integer;
	  {BlowDir     : Integer;}
	  procedure SeTBlowSpeed(Value: Integer);
	  procedure SeTBlowSteps(Value: Integer);
	  procedure SeTBlowStyle(Value: TBlowStyles);
	protected
	  procedure TimerTick(Sender: TObject);
	  procedure BlowThePanel;
	  procedure RestartTimer;
	  procedure CreateParams(var Params: TCreateParams); override;
	  procedure SetOpenClosedRects(L,T,W,H: Integer);
	  procedure OpenOrClose;
	public
	  constructor Create(AOwner: TComponent); override;
	  destructor Destroy; override;
	  procedure OpenBlowPanel;
	  procedure CloseBlowPanel;
	published
	  property Align;
	  property Alignment;
	  property BevelInner;
	  property BevelOuter;
	  property BorderStyle;
	  property BorderWidth;
	  property Caption;
	  property Color;
	  property Ctl3D;
	  property DragCursor;
	  property DragMode;
	  property Enabled;
	  property Font;
	  property Locked;
	  property ParentColor;
	  property ParentCtl3D;
	  property ParentFont;
	  property ParentShowHint;
	  property PopupMenu;
	  property ShowHint;
	  property TabOrder;
	  property TabStop;
	  property Visible;
	  property BlowSpeed : Integer read FBlowSpeed write SeTBlowSpeed default 1000;
	  property BlowSteps : Integer read FBlowSteps write SeTBlowSteps default 5;
	  property BlowStyle : TBlowStyles read FBlowStyle write SeTBlowStyle default bsCenter;
	  property OnClick;
	  property OnDblClick;
	  property OnDragDrop;
	  property OnDragOver;
	  property OnEndDrag;
	  property OnEnter;
	  property OnExit;
	  property OnMouseDown;
	  property OnMouseMove;
	  property OnMouseUp;
	  property OnResize;
	end;

	procedure Register;

implementation

constructor TBlowPanel.Create(AOwner: TComponent);
//----------------------------------------------------------------------------------------------------------------------------------
begin
	inherited Create(AOwner);
	//BlowSpeed controls the Speed at which the panel steps through its Blowing.
	//The actual millisecond delay used is 1000 divided by BlowSpeed. Therefore setting BlowSpeed to a higher number will result
	//in less of a delay between steps and hence a faster Blow.
	FBlowSpeed := 5;
	//This defines the total number of steps that the panel will take to get to its final size and location
	FBlowSteps := 15;
	CurrStep := -1;
	//BlowStyle defines the effect used when Blowing.
	FBlowStyle := bsCenter;
	//Set the panel as Open
	FBlowState := bstOpen;
	//First, we need to create a timer that will control the pacing of the panel's Blow effect.
	FTimer := TTimer.Create(Self);
	//Set the procedure that will handle the timer. Every time the timer expires it will transfer control to this procedure.
	//The procedure moves the panel a single step and then the timer starts over.
	FTimer.OnTimer := TimerTick;
	//Set the timer as initially off. CreateParams turns it on
	FTimer.Enabled := False;
	//Set the rate at which the panel opens or closes
	FTimer.Interval := 1000 div BlowSpeed;
end;

destructor TBlowPanel.Destroy;
//----------------------------------------------------------------------------------------------------------------------------------
begin
	//When the panel is destroyed, kill the timer
	FTimer.Free;
	inherited Destroy;
end;

procedure TBlowPanel.TimerTick(Sender: TObject);
//----------------------------------------------------------------------------------------------------------------------------------
begin
	//Go and do a single Blow step
	BlowThePanel;
end;

procedure TBlowPanel.SeTBlowSpeed(Value: Integer);
//----------------------------------------------------------------------------------------------------------------------------------
begin
	//BlowSpeed must be at least 1 (slowest) and no greater than 1000 (top speed)
	if FBlowSpeed <> Value then begin
		if Value > 1000 then begin
			FBlowSpeed := 1000;
		end else begin
			if Value < 1 then begin
				FBlowSpeed := 1;
			end else begin
				FBlowSpeed := Value;
			end;
		end;
		//Set the rate at which the panel opens or closes
		If FTimer <> nil then begin
			 FTimer.Interval := 1000 div BlowSpeed;
       end;
   end;
end;

procedure TBlowPanel.SeTBlowSteps(Value: Integer);
//----------------------------------------------------------------------------------------------------------------------------------
begin
	//BlowSteps must be at least 1 and no greater than 100
	if FBlowSteps <> Value then begin
		if Value < 1 then begin
			FBlowSteps := 1;
		end else begin
			if Value > 100 then begin
				FBlowSteps := 100;
			end else begin
				FBlowSteps := Value;
			end;
		end;
	end;
end;

procedure TBlowPanel.SeTBlowStyle(Value: TBlowStyles);
//----------------------------------------------------------------------------------------------------------------------------------
begin
	 if FBlowStyle <> Value then begin
		FBlowStyle := Value;
		//Go set the sizes of the open and closed states of the panel
		SetOpenClosedRects(Left,Top,Width,Height);
		//Do a Blow in design mode so we can see the effect
		if (csDesigning in ComponentState) and (CurrStep=-1) then begin
			OpenOrClose;
		end;
	 end;
end;

procedure TBlowPanel.BlowThePanel;
//----------------------------------------------------------------------------------------------------------------------------------
var
	CH,OW,OH,CW : integer;
begin
	if FBlowState = bstOpen then begin
	   Inc(CurrStep)
	end else begin
	   Dec(CurrStep);
	end;
	OW := OpenRect.Right-OpenRect.Left; //open width
	OH := OpenRect.Bottom-OpenRect.Top; //open height
	CW := Trunc(OW/BlowSteps*CurrStep); //current width
	CH := Trunc(OH/BlowSteps*CurrStep); //current height
	case BlowStyle of
		bsCenter    : begin
			with ClosedRect do begin
				SetBounds(Left-CW div 2,Top-CH div 2,CW,CH);
			end;
		end;
		bsWipeRight : begin
			with ClosedRect do begin
				SetBounds(Left,Top,CW,OH);
			end;
		end;
		bsWipeLeft  : begin
			with ClosedRect do begin
				SetBounds(Right-CW,Top,CW,OH);
			end;
		end;
		bsWipeUp    : begin
			with ClosedRect do begin
				SetBounds(Left,Top-CH,OW,CH);
			end;
		end;
		bsWipeDown  : begin
			with ClosedRect do begin
				SetBounds(Left,Top,OW,CH);
			end;
		end;
	end;
	//Disable the timer if we have done our last Blow step
	if (CurrStep >= BlowSteps) or (CurrStep <= 0) Then begin
		FTimer.Enabled := False;
		CurrStep := -1;
	end;
end;

procedure TBlowPanel.SetOpenClosedRects(L,T,W,H: Integer);
//----------------------------------------------------------------------------------------------------------------------------------
begin
	//Save the size that the panel should be when open. I had to do this in the CreateParams method because the size of the control
	//is not set until after the Create is complete - so that users could override the size.
	OpenRect := Rect( L, T, L+W, T+H );
	//Save the size that the panel should be when closed.
	case BlowStyle of
		//This is the default Blowing behavior. It picks a spot in the exact center of the control as the center of the Blow effect.
		bsCenter    : begin
			ClosedRect := Rect(L+W div 2,T+H div 2,L+W div 2,T+H div 2); //The panel will open from and close into this point.
		end;
		bsWipeRight : begin
			ClosedRect := Rect(L,T,L,T+H); //The panel will open like a "screen door" to the right
		end;
		bsWipeLeft  : begin
			ClosedRect := Rect(L+W,T,L+W,T+H); //The panel will open like a "screen door" to the left
		end;
		bsWipeUp    : begin
			ClosedRect := Rect(L,T+H,L+W,T+H); //The panel will open like a "window shade" going up
		end;
		bsWipeDown  : begin
			ClosedRect := Rect(L,T,L+W,T); //The panel will open like a "window shade" going down
		end;
	end;
end;

procedure TBlowPanel.CreateParams(var Params: TCreateParams);
//----------------------------------------------------------------------------------------------------------------------------------
begin
	 inherited CreateParams(Params);
	 //Go set the sizes of the open and closed states of the panel
	 with Params do begin
		SetOpenClosedRects(Left,Top,Width,Height);
	 end;
	 //Activate the timer to start the Blow open
	 OpenOrClose;
end;

procedure TBlowPanel.OpenOrClose;
//----------------------------------------------------------------------------------------------------------------------------------
begin
	 if (FBlowState = bstOpen) then begin
		OpenBlowPanel
	 end else begin
		CloseBlowPanel;
	 end;
end;

procedure TBlowPanel.OpenBlowPanel;
//----------------------------------------------------------------------------------------------------------------------------------
begin
	FBlowState := bstOpen;
	RestartTimer;
end;

procedure TBlowPanel.CloseBlowPanel;
//----------------------------------------------------------------------------------------------------------------------------------
begin
	FBlowState := bstClosed;
	RestartTimer;
end;

procedure TBlowPanel.RestartTimer;
//----------------------------------------------------------------------------------------------------------------------------------
begin
	 if FBlowState = bstOpen then begin
		CurrStep := 0
	 end else  begin
		CurrStep := BlowSteps;
	 end;
	 //Turn the timer on
	 FTimer.Enabled := True;
end;

procedure Register;
//----------------------------------------------------------------------------------------------------------------------------------
begin
	RegisterComponents('FX Controls', [TBlowPanel]);
end;

{------------------------------------------------------------------------------}

end.

