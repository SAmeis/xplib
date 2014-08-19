{$IFDEF CpltComboBox }
	{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I ECLib.inc}

unit CpltComboBox;

interface

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls;

type
	TCompletingComboBox = Class (TComboBox)
	private
		FTextCompletion : Boolean;
		function GetTextCompletion : Boolean;
		procedure SetTextCompletion(const Value : Boolean);
	protected
		procedure ComboWndProc(var Message : TMessage; ComboWnd : HWnd; ComboProc : Pointer); override;
	public
		{ Public declarations }
	published
		property TextCompletion : Boolean read GetTextCompletion write SetTextCompletion;
	end;

implementation


function TCompletingComboBox.GetTextCompletion : Boolean;
	//----------------------------------------------------------------------------------------------------------------------
begin
	Result := FTextCompletion;
end;

procedure TCompletingComboBox.SetTextCompletion(const Value : Boolean);
//----------------------------------------------------------------------------------------------------------------------
begin
	Self.FTextCompletion := Value;
end;

procedure TCompletingComboBox.ComboWndProc(var Message : TMessage; ComboWnd : HWnd; ComboProc : Pointer);
{{
override the WndProc() so that we can trap KeyUp events.

Revision:6/6/2005
}
var
	rc, len : Integer;
begin
	{ TODO -oRoger -cLIB : Crair no momento em que se pega o foco a selecao do todo o texto }
	inherited;
	case Message.Msg of
		WM_KEYUP	: begin
			//Testa se caracter deve ser processado
			if (Message.WParam <> 8) and
				//(Message.WParam <> VK_DELETE) and (Message.WParam <> VK_SHIFT) and
				not (Message.WParam in [VK_BACK, VK_CLEAR, VK_FINAL, VK_PRIOR, VK_NEXT, VK_END, VK_HOME, VK_LEFT,
				VK_RIGHT, VK_SELECT, VK_DELETE, VK_SHIFT, VK_RETURN, VK_TAB]) and
				(FTextCompletion = TRUE) then begin
				// Use CB_FINDSTRING to locate the string in the Items property
				rc := Perform(CB_FINDSTRING, -1, Integer(PChar(Caption)));
				// if its in there then add the new string to the Text and
				// select the portion that wasn't typed in by the user
				if rc <> CB_ERR then begin
					// store the length of the current string
					len := Length(Text);
					// set the new string
					ItemIndex := rc;
					// highlight the rest of the text that was added.
					SelStart := len;
					SelLength := Length(Text) - len;
					// return 0 to signify that the message has been handled.
					Message.Result := 0;
				end;
			end;
		end;
	end; // case
end;

end.


