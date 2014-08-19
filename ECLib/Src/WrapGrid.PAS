{$IFDEF Wrapgrid }
	{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I ECLib.inc}

unit Wrapgrid;

interface

uses
	SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls, Forms, Grids;

type
	TWrapGrid = Class (TStringGrid)
	private
	protected
		procedure DrawCell(ACol, ARow : Longint; ARect : TRect; AState : TGridDrawState); override;
	public
		constructor Create(AOwner : TComponent); override;
	published
		{ Published declarations }
	end;

implementation

constructor TWrapGrid.Create(AOwner : TComponent);
{{
The Create procedure is overriden to use the DrawCell procedure by default

Revision:6/6/2005
}
begin
	//Create a TStringGrid
	inherited Create(AOwner);
	//Make the drawing use our DrawCell procedure by default
	DefaultDrawing := FALSE;
end;

procedure TWrapGrid.DrawCell(ACol, ARow : Longint; ARect : TRect; AState : TGridDrawState);
{{
This DrawCell procedure wraps text in the grid cell
}
var
	Sentence,				  // What is left in the cell to output
	CurWord : string;		  // The word we are currently outputting
	SpacePos,				  // The position of the first space
	CurX,					  // The x position of the 'cursor'
	CurY : Integer;			// The y position of the 'cursor
	EndOfSentence : Boolean;   // Whether or not we are done outputting the cell
begin
	//Initialize the font to be the control's font
	Canvas.Font := Font;

	with Canvas do begin

		if gdFixed in AState then begin //If this is a fixed cell, then use the fixed color
			Pen.Color := FixedColor;
			Brush.Color := FixedColor;
		end else begin //else, use the normal color
			Pen.Color := Color;
			Brush.Color := Color;
		end;
		Rectangle(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom); //Prepaint cell in cell color
	end;

	//Start the drawing in the upper left corner of the cell
	CurX := ARect.Left;
	CurY := ARect.Top;

	//Here we get the contents of the cell
	Sentence := Cells[ACol, ARow];

	//for each word in the cell
	EndOfSentence := FALSE;
	while (not EndOfSentence) do begin
		SpacePos := Pos(' ', Sentence); //to get the next word, we search for a space
		if SpacePos > 0 then begin
			CurWord := Copy(Sentence, 0, SpacePos); //get the current word plus the space
			Sentence := Copy(Sentence, SpacePos + 1, Length(Sentence) - SpacePos);  //get the rest of the sentence
		end else begin //this is the last word in the sentence
			EndOfSentence := TRUE;
			CurWord := Sentence;
		end;

		with Canvas do begin
			if (TextWidth(CurWord) + CurX) > ARect.Right then begin //if the text goes outside the boundary of the cell
				//wrap to the next line
				CurY := CurY + TextHeight(CurWord);
				CurX := ARect.Left;
			end;
			TextOut(CurX, CurY, CurWord); //write out the word
			CurX := CurX + TextWidth(CurWord); //increment the x position of the cursor
		end;
	end;
end;

end.


