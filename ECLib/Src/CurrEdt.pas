{$IFDEF CurrEdt }
	{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I ECLib.inc}


unit CurrEdt;

{ TODO -oRoger -cDSG :
precisa ter algumas de suas propriedades ocultas(Nao e possivel), tais  como:
    passworchar
    OEMConvert
    charcase
alem de acrescentar as de maximo e minimo valor }

interface

uses
	SysUtils, Windows, Classes, Controls, StdCtrls;

type
	TCurrencyEdt = Class (TEdit)
	private
		FieldValue : Extended;
		FMaxValue :  Extended;
		FMinValue :  Extended;
		procedure SetFieldValue(A : Extended);
		procedure SetMaxValue(Val : Extended);
		procedure SetMinValue(Val : Extended);
		procedure CMEnter(var Message : TCMEnter); message CM_ENTER;
		procedure CMExit(var Message : TCMExit); message CM_EXIT;
		procedure FormatText;
		procedure UnFormatText;
	protected
		procedure KeyPress(var Key : Char); override;
		procedure CreateParams(var Params : TCreateParams); override;
	public
		constructor Create(AOwner : TComponent); override;
	published
		property AutoSize default TRUE;
		property MinValue : Extended Read FMinValue Write SetMinValue;
		property MaxValue : Extended Read FMaxValue Write SetMaxValue;
		property Value : Extended read FieldValue write SetFieldValue;
		property OnChange;
		property OnEnter;
		property OnExit;
		property OnKeyDown;
		property OnKeyPress;
		property OnKeyUp;
	end;


implementation


constructor TCurrencyEdt.Create(AOwner : TComponent);
	//----------------------------------------------------------------------------------------------------------------------------------
begin
	inherited Create(AOwner);
	MinValue := 0.0;
	MaxValue := 0.0;
	AutoSize := TRUE;
	Width := 121;
	Height := 25;
	FieldValue := 0.0;
	AutoSelect := FALSE;
	FormatText;
end;

procedure TCurrencyEdt.SetMaxValue(Val : Extended);
//----------------------------------------------------------------------------------------------------------------------------------
begin
	if Val < FMinValue then begin
		Abort;
	end else begin
		FMaxValue := Val;
	end;
end;

procedure TCurrencyEdt.SetMinValue(Val : Extended);
//----------------------------------------------------------------------------------------------------------------------------------
begin
	if Val > FMaxValue then begin
		Abort;
	end else begin
		FMinValue := Val;
	end;
end;

procedure TCurrencyEdt.SetFieldValue(A : Extended);
//----------------------------------------------------------------------------------------------------------------------
begin
	if FieldValue <> A then begin
		if (FMinValue <> 0) and (FMaxValue <> 0) then begin
			//Gera validacao de intervalo
			if (A < FMinValue) or (A > FMaxValue) then begin
				raise Exception.Create('Valor fora da faixa válida');
			end;
		end;
		FieldValue := A;
		FormatText;
	end;
end;

procedure TCurrencyEdt.UnFormatText;
//----------------------------------------------------------------------------------------------------------------------------------
var
	TmpText : String;
	Tmp : Byte;
	IsNeg : Boolean;
begin
	IsNeg := (Pos('-', Text) > 0) or (Pos('(', Text) > 0);
	TmpText := '';
	for Tmp := 1 to Length(Text) do begin
		if Text[Tmp] in ['0'..'9', '.'] then begin
			TmpText := TmpText + Text[Tmp];
		end;
	end;
	try
		FieldValue := StrToFloat(TmpText);
		if IsNeg then begin
			FieldValue := -FieldValue;
		end;
		if ((MaxValue <> 0) or (MinValue <> 0)) and ((FieldValue > FMaxValue) or (FieldValue < MinValue)) then begin
			FormatText;
			SelectAll;
			Self.SetFocus;
			Abort;
		end;
	except
		MessageBeep(mb_IconAsterisk);
	end;
end;

procedure TCurrencyEdt.FormatText;
//----------------------------------------------------------------------------------------------------------------------
begin
	Text := Format('%m', [FieldValue]);
end;

procedure TCurrencyEdt.CMEnter(var Message : TCMEnter);
//----------------------------------------------------------------------------------------------------------------------------------
begin
	SelectAll;
	inherited;
end;

procedure TCurrencyEdt.CMExit(var Message : TCMExit);
//----------------------------------------------------------------------------------------------------------------------------------
begin
	UnformatText;
	FormatText;
	inherited;
end;

procedure TCurrencyEdt.KeyPress(var Key : Char);
//----------------------------------------------------------------------------------------------------------------------------------
begin
	if not (Key in ['0'..'9', '.', '-']) then begin
		Key := #0;
	end;
	inherited KeyPress(Key);
end;

procedure TCurrencyEdt.CreateParams(var Params : TCreateParams);
//----------------------------------------------------------------------------------------------------------------------------------
begin
	inherited CreateParams(Params);
end;

end.


