{$IFDEF DsgECLibRegister }
{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I ECLib.inc}
unit DsgECLibRegister;

interface

uses
	Classes;

procedure Register;

implementation

uses
	CheckCombo, Wrapgrid, CpltComboBox, CURREDT, DosMove, EnhGrids, Operatrs, RsRuler, StpPanel, TrcCtrls, XPSpinControls;

const
	PALLETE_NAME = 'EControls';

procedure Register;
begin
	RegisterComponents(PALLETE_NAME, [TTruncLabel, TTruncFileLabel, TOperatorDictionary, TCheckedComboBox, TWrapGrid,
		TOperatorDictionary]);
	RegisterComponents(PALLETE_NAME, [TXPSpinEdit]);
	RegisterComponents(PALLETE_NAME, [TEnhStringGrid]);
	RegisterComponents(PALLETE_NAME, [TCurrencyEdt]);
	RegisterComponents(PALLETE_NAME, [TCompletingComboBox]);
	RegisterComponents(PALLETE_NAME, [TStepPanel]);
	RegisterComponents(PALLETE_NAME, [TRsRuler, TRsRulerCorner]);
	RegisterComponents(PALLETE_NAME, [TDosMove]);
end;

end.
