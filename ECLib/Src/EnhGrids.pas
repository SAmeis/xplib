{$IFDEF EnhGrids }
{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I ECLib.inc}
unit EnhGrids;

interface

uses
	Windows, SysUtils, Classes, Controls, StdCtrls, Grids;

type
	TEnhStringGrid = class(TStringGrid)
	private
		//InplaceEditor : TWinControl;
		FOnDrawCellGetProperties: TDrawCellEvent;
		procedure DoPreDrawCell(ACol, ARow: Integer; ARect: TRect; AState: TGridDrawState);
	protected
		procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
	public
		constructor Create(AOwner: TComponent); override;
	published
		property OnDrawCellGetProperties: TDrawCellEvent read FOnDrawCellGetProperties write FOnDrawCellGetProperties;
	end;

implementation

{ EnhStringGrid }

constructor TEnhStringGrid.Create(AOwner: TComponent);
//----------------------------------------------------------------------------------------------------------------------
begin
	inherited;
	//Self.FixedCols:=0;
	//Self.FixedRows:=0;
end;

procedure TEnhStringGrid.DoPreDrawCell(ACol, ARow: Integer; ARect: TRect; AState: TGridDrawState);
//----------------------------------------------------------------------------------------------------------------------
begin
	if Assigned(Self.FOnDrawCellGetProperties) then begin
		Self.FOnDrawCellGetProperties(Self, ACol, ARow, ARect, AState);
	end;
end;

procedure TEnhStringGrid.DrawCell(ACol, ARow: Integer; ARect: TRect; AState: TGridDrawState);
//----------------------------------------------------------------------------------------------------------------------
begin
	DoPreDrawCell(ACol, ARow, ARect, AState);
	inherited;
end;

end.
