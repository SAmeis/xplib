{$IFDEF WCtrls}
{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I INetLib.inc}
unit WCtrls;

interface

uses
	IHTML4, Grids;

function HTMLCloneGrid(HTbl: THTMLTable; Grid: TStringGrid): integer;

implementation

function HTMLCloneGrid(HTbl: THTMLTable; Grid: TStringGrid): integer;
//----------------------------------------------------------------------------------------------------------------------
var
	r, c:  integer;
	HRow:  THTMLTableRow;
	HCell: THTMLTableDetail;
begin
	Result := 0;
	for r := 0 to Grid.RowCount - 1 do begin //Varre linhas
		HRow := THTMLTableRow.Create;
		for c := 0 to Grid.ColCount - 1 do begin //Varre colunas
			HCell := THTMLTableDetail.Create(Grid.Cells[r, c]);
			if (c < Grid.FixedCols) or (r < Grid.FixedRows) then begin //Checa cor de area fixa
				HCell.Background := Grid.FixedColor;
			end;
			HRow.Add(HCell);
		end;
		HTbl.Add(HRow);
		Inc(Result);
	end;
end;

end.
