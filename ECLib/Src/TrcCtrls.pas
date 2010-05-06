{$IFDEF TrcCtrls }
	{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I ECLib.inc}

unit TrcCtrls;

interface

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	StdCtrls, Super;


type
	TTruncLabel = Class (TCustomLabel)
	private
		FTruncPosition : THrzPosition;
		procedure SetTruncPosition(const Value : THrzPosition);
	protected
		procedure DoDrawText(var Rect : TRect; Flags : Longint); override;
	public
		constructor Create(AOwner : TComponent); override;
	published
		// Removidas por projeto property ShowAccelChar; property AutoSize;
		property Align;
		property Alignment;
		property Anchors;
		property BiDiMode;
		property Caption;
		property Color;
		property Constraints;
		property Cursor;
		property DragCursor;
		property DragKind;
		property DragMode;
		property Enabled;
		property FocusControl;
		property Font;
		property Hint;
		property ParentBiDiMode;
		property ParentColor;
		property ParentFont;
		property ParentShowHint;
		property PopupMenu;
		property ShowAccelChar;
		property ShowHint;
		property Transparent;
		property TruncPosition : THrzPosition read FTruncPosition write SetTruncPosition;
		property Layout;
		property Visible;
		property OnClick;
		property OnContextPopup;
		property OnDblClick;
		property OnDragDrop;
		property OnDragOver;
		property OnEndDock;
		property OnEndDrag;
		property OnMouseDown;
		property OnMouseMove;
		property OnMouseUp;
		property OnStartDock;
		property OnStartDrag;
	end;

	TTruncFileLabel = Class (TTruncLabel)
	protected
		procedure DoDrawText(var Rect : TRect; Flags : Longint); override;
	public
		constructor Create(AOwner : TComponent); override;
	end;

function TruncFileNameRect(Canvas : TCanvas; const FileName : TFileName; Rect : TRect; TruncPos : THrzPosition) : string;
function TruncTextCanvasWidthLeft(Canvas : TCanvas; const Text : string; Width : integer) : string;
function TruncTextCanvasWidthRigth(Canvas : TCanvas; const Text : string; Width : integer) : string;
function TruncTextRect(Canvas : TCanvas; const Text : string; Rect : TRect; Position : THrzPosition) : string;


implementation

uses
	FileHnd, Str_pas;

const
	_ELLIPSIS_ = '...';


{-------------------------------------------------------------------------------------------------------------}
//                            ROTINAS AUXILIARES EXPORTADAS
{-------------------------------------------------------------------------------------------------------------}
function TruncTextCanvasWidthRigth(Canvas : TCanvas; const Text : string; Width : integer) : string;
{{
Trunca texto para caber em Width cortando a direita

Revision:6/6/2005
}
var
	ActualLen, Len : integer;
begin
	Result := Text;
	if Result <> EmptyStr then begin
		repeat
			ActualLen := Canvas.TextWidth(Result); //comprimento atual do texto
			if ActualLen > Width then begin
				Len := Length(Result) div 2;
				if Len <> 0 then begin
					Result := Copy(Result, 1, Len);
					ActualLen := Canvas.TextWidth(Result);
					if ActualLen > Width then begin
						Result := TruncTextCanvasWidthRigth(Canvas, Result, Width); //Aparar mais ainda
					end else begin
						//Fim da aparas, inicio volta
						Result := Result +
							TruncTextCanvasWidthRigth(Canvas, Copy(Text, Len + 1, Length(Text)), Width - ActualLen);
					end;
				end else begin
					Result := EmptyStr;
				end;
			end;
		until (ActualLen <= Width) or (Result = EmptyStr);
	end;
end;


function TruncFileNameRect(Canvas : TCanvas; const FileName : TFileName; Rect : TRect; TruncPos : THrzPosition) : string;
{{
Trunca nome de arquivo para nao passar em area de exibicao para modo alternativo Ver DrawText e DrawTextEx
}
var
	IncDir, Partial, LSide : string;
	Delta, Acc : integer;
begin
	Result := FileName;
	Delta  := Rect.Right - Rect.Left;
	if Canvas.TextWidth(Result) < Delta then begin //Tudo OK!
		Exit;
	end;
	LSide := ExtractRootResource(Result); //Parte inicial fixa
	Partial := ExtractFilePath(Copy(Result, Length(LSide) + 1, Length(Result)));
	Partial := Copy(Partial, 2, Length(Partial) - 2);
	Result := ExtractFileName(Result); //Nome do arq fixo
	Acc := Canvas.TextWidth(Result) + Canvas.TextWidth(LSide) + Canvas.TextWidth('\' + _ELLIPSIS_ + '\');
	//Acc:=Canvas.TextWidth( Result ) + Canvas.TextWidth( LSide ) + Canvas.TextWidth( '\'+_ELLIPSIS_+'\' );
	if TruncPos <> hpCenter then begin
		if TruncPos = hpRigth then begin
			Partial := TruncTextCanvasWidthRigth(Canvas, Partial, Delta - Acc);
			Result  := LSide + '\' + Partial + _ELLIPSIS_ + '\' + Result;
		end else begin //hpLeft
			IncDir := Copy(Partial, 1, Pos('\', Partial));
			Partial := TruncTextCanvasWidthLeft(Canvas, Partial, Delta - Acc);
			//####if 
			Result := LSide + '\' + _ELLIPSIS_ + Partial + '\' + Result;
		end;
	end else begin

		Acc := Canvas.TextWidth(LSide) + Canvas.TextWidth(_ELLIPSIS_) + Canvas.TextWidth(Result);
		if Acc < Delta then begin //Falta algum preenchimento
			Partial := Copy(Result, Length(LSide) + 1, Length(Result) - Length(LSide) - Length(Result));
			{
            if Optimize then begin
                repeat
                    Inc( Acc, Canvas.TextWidth( '\' + ExtractFilePath( Partial ) ));
                    Result:=ExtractFileName( Partial ) + '\' + Result;
                    if Acc < Delta then begin
                        //TODO  Inc( Acc, Canvas.
                    end;
                until Acc > Delta;
            end else begin

            end;
            }
		end else begin //O minimo ja estourou tamanho
			{
            if Optimize then begin //Retirar parte do servidor
                if Length( LSide ) <= 3 then begin
                    Result:= _ELLIPSIS_ + '\' + Result;
                end else begin
                    if Str_Pas.StrCountRepet( '\', LSide ) >= 4 then begin //UNC
                        LSide:=Copy( LSide, 1, StrNPos( '\', LSide, 3));
                        Result:=LSide + Result;
                    end else begin
                        Result:= _ELLIPSIS_ + '\' + Result;
                    end;
                end;
            end else begin
                Result:= _ELLIPSIS_ + '\' + Result;
            end;
			}
		end;
	end;
end;


function TruncTextCanvasWidthLeft(Canvas : TCanvas; const Text : string; Width : integer) : string;
{{
Trunca texto para caber em Width cortando a esquerda
}
var
	ActualLen, Len : integer;
begin
	Result := Text;
	if Result <> EmptyStr then begin
		repeat
			ActualLen := Canvas.TextWidth(Result); //comprimento atual do texto
			if ActualLen > Width then begin
				Len := Length(Result) div 2;
				if Len <> 0 then begin
					Delete(Result, 1, Len);
					ActualLen := Canvas.TextWidth(Result);
					if ActualLen > Width then begin
						Result := TruncTextCanvasWidthLeft(Canvas, Result, Width); //Aparar mais ainda
					end else begin
						//Fim da aparas, inicio volta
						Result := TruncTextCanvasWidthLeft(Canvas, Copy(Text, 1, Len), Width - ActualLen) + Result;
					end;
				end else begin
					Result := EmptyStr;
				end;
			end;
		until (ActualLen <= Width) or (Result = EmptyStr);
	end;
end;


function TruncTextRect(Canvas : TCanvas; const Text : string; Rect : TRect; Position : THrzPosition) : string;
{{
Trunca o texto de modo ao mesmo nao extrapolar a regiao dada por Rect em comprimento

Revision:6/6/2005
}
var
	Total, Delta, EllipSize : integer;
begin
	Delta := (Rect.Right - Rect.Left);
	Total := Canvas.TextWidth(Text);
	if Total > Delta then begin
		EllipSize := Canvas.TextWidth(_ELLIPSIS_);
		case Position of
			hpLeft	: begin
				Result := _ELLIPSIS_ + TruncTextCanvasWidthLeft(Canvas, Text, Delta - EllipSize);
			end;
			hpCenter	: begin
				Result := TruncTextCanvasWidthRigth(Canvas, Text, (Delta - EllipSize) div 2) + _ELLIPSIS_ +
					TruncTextCanvasWidthLeft(Canvas, Text, (Delta - EllipSize) div 2);
			end;
			hpRigth	: begin
				Result := TruncTextCanvasWidthRigth(Canvas, Text, Delta - EllipSize) + _ELLIPSIS_;
			end;
		end;
	end else begin
		Result := Text;
	end;
end;


constructor TTruncLabel.Create(AOwner : TComponent);
begin
	inherited Create(AOwner);
	Self.FTruncPosition := hpCenter;
	Self.ShowAccelChar := FALSE;
	Self.AutoSize := FALSE;
end;

procedure TTruncLabel.DoDrawText(var Rect : TRect; Flags : Integer);
{-------------------------------------------------------------------------------------------------------------}
var
	DspTxt : TCaption;
begin
	Text := GetLabelText;
	if FTruncPosition = hpRigth then begin
		DspTxt := Text;
		Flags  := Flags or DT_END_ELLIPSIS or DT_RIGHT;
	end else begin
		DspTxt := TruncTextRect(Self.Canvas, Text, Rect, Self.FTruncPosition); //Altera texto para a area
	end;
	if (Flags and DT_CALCRECT <> 0) and
		((DspTxt = EmptyStr) or ShowAccelChar and (DspTxt[1] = '&') and (DspTxt[2] = #0)) then begin
		DspTxt := DspTxt + ' ';
	end;
	if not ShowAccelChar then begin
		Flags := Flags or DT_NOPREFIX;
	end;
	Flags := DrawTextBiDiModeFlags(Flags);

	Canvas.Font := Font;
	if not Enabled then begin
		OffsetRect(Rect, 1, 1);
		Canvas.Font.Color := clBtnHighlight;
		DrawText(Canvas.Handle, PChar(DspTxt), Length(DspTxt), Rect, Flags);
		OffsetRect(Rect, -1, -1);
		Canvas.Font.Color := clBtnShadow;
		DrawText(Canvas.Handle, PChar(DspTxt), Length(DspTxt), Rect, Flags);
	end else begin
		DrawText(Canvas.Handle, PChar(DspTxt), Length(DspTxt), Rect, Flags);
	end;
end;

procedure TTruncLabel.SetTruncPosition(const Value : THrzPosition);
{-------------------------------------------------------------------------------------------------------------}
begin
	if Value <> FTruncPosition then begin
		FTruncPosition := Value;
		Self.Invalidate;
	end;
end;



{ TTruncFileLabel }
constructor TTruncFileLabel.Create(AOwner : TComponent);
	{-------------------------------------------------------------------------------------------------------------}
begin
	inherited;
	Self.FTruncPosition := hpRigth;
end;

procedure TTruncFileLabel.DoDrawText(var Rect : TRect; Flags : Integer);
{-------------------------------------------------------------------------------------------------------------}
var
	DspTxt : TCaption;
begin
	Text := GetLabelText;
	case FTruncPosition of
		hpRigth	: begin
			DspTxt := TruncFileNameRect(Canvas, Text, Rect, FTruncPosition);
		end;
		hpCenter	: begin
			//TODO
			DspTxt := TruncFileNameRect(Self.Canvas, Text, Rect, Self.FTruncPosition); //Altera texto para Rect
		end;
		hpLeft	: begin
			//TODO
			DspTxt := TruncFileNameRect(Self.Canvas, Text, Rect, Self.FTruncPosition); //Altera texto para Rect
		end;
	end;
	if (Flags and DT_CALCRECT <> 0) and ((DspTxt = EmptyStr) or ShowAccelChar and (DspTxt[1] = '&') and
		(DspTxt[2] = #0)) then	begin
		DspTxt := DspTxt + ' ';
	end;
	if not ShowAccelChar then begin
		Flags := Flags or DT_NOPREFIX;
	end;
	Flags := DrawTextBiDiModeFlags(Flags);
	Canvas.Font := Font;
	if not Enabled then begin
		OffsetRect(Rect, 1, 1);
		Canvas.Font.Color := clBtnHighlight;
		DrawText(Canvas.Handle, PChar(DspTxt), Length(DspTxt), Rect, Flags);
		OffsetRect(Rect, -1, -1);
		Canvas.Font.Color := clBtnShadow;
		DrawText(Canvas.Handle, PChar(DspTxt), Length(DspTxt), Rect, Flags);
	end else begin
		DrawText(Canvas.Handle, PChar(DspTxt), Length(DspTxt), Rect, Flags);
	end;

end;

end.


