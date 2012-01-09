unit GeneralLibDsgSupport;

interface

uses
    Classes, DesignConst, DesignIntf, DesignEditors, UPlasmaForm, UPlasmaRegion, uJetButtonDesign, UPlasmaEd, ujetanimbutton, ujetbutton, ujetcheckbox;

procedure Register;

implementation


procedure Register;
begin
    //Familia JetButton
    RegisterComponents('Jet', [TJetButton, TJetCheckbox]);
    RegisterPropertyEditor(TypeInfo(TAboutJetCheckbox), TJetCheckbox, 'ABOUT', TAboutJetCheckbox);
    RegisterPropertyEditor(TypeInfo(TAboutJetButton), TJetButton, 'ABOUT', TAboutJetButton);

    //Familia Plasma
    RegisterComponents('Justus', [TPlasmaForm]);
    RegisterPropertyEditor(TypeInfo(TPlasmaRegion), nil, '', TPlasmaMaskProperty);
end;


end.
