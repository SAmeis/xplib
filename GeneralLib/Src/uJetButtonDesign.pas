unit uJetButtonDesign;

interface

uses
    DesignConst, DesignIntf, DesignEditors, Forms;

type
    TAboutJetButton = class(TPropertyEditor)
    public
        procedure Edit; override;
        function GetAttributes : TPropertyAttributes; override;
        function GetValue : string; override;
    end;

    procedure Register;

implementation

uses
    Controls, Dialogs, Windows, SysUtils, Classes, ujetbutton;



procedure Register;
begin
    RegisterComponents('Jet', [TJetButton]);
    RegisterPropertyEditor(TypeInfo(TAboutJetButton), TJetButton, 'ABOUT', TAboutJetButton);
end;

////////////////////////////////////////////////////////////////////////////////////////
procedure TAboutJetButton.Edit;
begin
    Application.MessageBox('TJetButton v1.00 for Delphi32. (C) 1999 Jimmy Theo Weku.' + #10#13 +
        'for more information about how to use this component please read README.TXT that included with this component',
        'About TJetButton Component', MB_OK + MB_ICONINFORMATION);
end;

function TAboutJetButton.GetAttributes : TPropertyAttributes;
begin
    Result := [paMultiSelect, paDialog, paReadOnly];
end;

function TAboutJetButton.GetValue : string;
begin
    Result := '(About)';
end;

end.
