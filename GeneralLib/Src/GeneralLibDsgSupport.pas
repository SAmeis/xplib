unit GeneralLibDsgSupport;

interface

uses
    Classes, DesignConst, DesignIntf, DesignEditors, UPlasmaForm, UPlasmaRegion, uJetButtonDesign, UPlasmaEd, ujetanimbutton, ujetbutton, ujetcheckbox;


const
	GENERAL_IMPORTS_COMPONENT_FAMILY = 'General Imports';

procedure Register;

implementation


procedure Register;
begin
	 //Familia JetButton
	 RegisterComponents(GENERAL_IMPORTS_COMPONENT_FAMILY, [TJetButton, TJetCheckbox]);
	 RegisterPropertyEditor(TypeInfo(TAboutJetCheckbox), TJetCheckbox, 'ABOUT', TAboutJetCheckbox);
	 RegisterPropertyEditor(TypeInfo(TAboutJetButton), TJetButton, 'ABOUT', TAboutJetButton);

	 //Familia Plasma
	 RegisterComponents(GENERAL_IMPORTS_COMPONENT_FAMILY, [TPlasmaForm]);
	 RegisterPropertyEditor(TypeInfo(TPlasmaRegion), nil, '', TPlasmaMaskProperty);
end;


end.
