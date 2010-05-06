{$IFDEF LBHorz}
	{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I XPLib.inc}


unit LBHorz;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls;

type
  TListBoxHrz = class(TListBox)
  private
	 { Private declarations }
	 function GetScrollWidth : Integer;
	 procedure SetScrollWidth(value : Integer);
	 procedure LBAddString(var Msg: TMessage); message LB_ADDSTRING;
	 procedure LBInsertString(var Msg: TMessage); message LB_INSERTSTRING;
	 procedure LBDeleteString(var Msg: TMessage); message LB_DELETESTRING;
	 procedure LBResetContent(var Msg: TMessage); message LB_RESETCONTENT;
	 procedure CMFontChanged(var Msg: TMessage); message CM_FONTCHANGED;
  protected
	 { Protected declarations }
	 procedure AllWidths;
	 procedure NewWidth(P : PChar);
	 function WidthOfString(const S : string): integer; virtual;
	 function WidthOfPChar(P : PChar): integer; virtual;
  public
	 { Public declarations }
	 procedure CreateParams(var Params: TCreateParams); override;
  published
	 { Published declarations }
	 property ScrollWidth: integer read GetScrollWidth write SetScrollWidth;
  end;

procedure Register;

implementation

procedure TListBoxHrz.CreateParams(var Params: TCreateParams);
//----------------------------------------------------------------------------------------------------------------------------------
begin
	inherited CreateParams(Params);
end;

function TListBoxHrz.GetScrollWidth : integer;
//----------------------------------------------------------------------------------------------------------------------------------
begin
	Result := Perform(LB_GETHORIZONTALEXTENT, 0, 0);
end;

procedure TListBoxHrz.SetScrollWidth(value : integer);
//----------------------------------------------------------------------------------------------------------------------------------
begin
	Perform(LB_SETHORIZONTALEXTENT, value, 0);
end;

procedure TListBoxHrz.LBAddString(var Msg: TMessage);
//----------------------------------------------------------------------------------------------------------------------------------
begin
	inherited;
	NewWidth(PChar(Msg.lParam));
end;

procedure TListBoxHrz.LBInsertString(var Msg: TMessage);
//----------------------------------------------------------------------------------------------------------------------------------
begin
	inherited;
	NewWidth(PChar(Msg.lParam));
end;

procedure TListBoxHrz.LBDeleteString(var Msg: TMessage);
//----------------------------------------------------------------------------------------------------------------------------------
begin
	inherited;
	AllWidths;
end;

procedure TListBoxHrz.LBResetContent(var Msg: TMessage);
//----------------------------------------------------------------------------------------------------------------------------------
begin
	inherited;
	ScrollWidth := 0;
end;

procedure TListBoxHrz.CMFontChanged(var Msg: TMessage);
//----------------------------------------------------------------------------------------------------------------------------------
begin
	inherited;
	AllWidths;
end;

function TListBoxHrz.WidthOfString(const S : string): integer;
//----------------------------------------------------------------------------------------------------------------------------------
begin
	Canvas.Font := Font;
	Result := Canvas.TextWidth(S + 'X'); //Margem dada para caracter largo como este
end;

function TListBoxHrz.WidthOfPChar(P : PChar): integer;
//----------------------------------------------------------------------------------------------------------------------------------
begin
	Result := WidthOfString(StrPas(P));
end;

procedure TListBoxHrz.AllWidths;
//----------------------------------------------------------------------------------------------------------------------------------
var
	i, NewWid, Wid : integer;
begin
	NewWid := 0;
	for i := 0 to Items.Count-1 do begin
		Wid := WidthOfString(Items[i]);
		if Wid > NewWid then begin
			NewWid := Wid;
		end;
	end;
	ScrollWidth := NewWid;
end;

procedure TListBoxHrz.NewWidth(P : PChar);
//----------------------------------------------------------------------------------------------------------------------------------
var
	Wid : integer;
begin
	Canvas.Font := Font;
	Wid := WidthOfPChar(P);
	if Wid > ScrollWidth then begin
		ScrollWidth := Wid;
	end;
end;

procedure Register;
//----------------------------------------------------------------------------------------------------------------------------------
begin
  RegisterComponents('Standard', [TListBoxHrz]);
end;

end.
