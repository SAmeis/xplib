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


    TAboutJetCheckbox = class(TPropertyEditor)
    public
        procedure Edit; override;
        function GetAttributes : TPropertyAttributes; override;
        function GetValue : string; override;
    end;


implementation

uses
    Controls, Dialogs, Windows, SysUtils, Classes, ujetbutton, ujetcheckbox;


///
//Revision - 20111222 - Roger
// Removida a parte de suporte a design de modo a torna unit compilavel diretamente
///

////////////////////////////////////////////////////////////////////////////////////////
procedure TAboutJetCheckbox.Edit;
begin
    Application.MessageBox('TJetCheckbox v1.00 for Delphi32. (C) 1999 Jimmy Theo Weku.' + #10#13 +
        'for more information about how to use this component please read README.TXT that included with this component',
        'About TJetCheckbox Component', MB_OK + MB_ICONINFORMATION);
end;

function TAboutJetCheckbox.GetAttributes : TPropertyAttributes;
begin
    Result := [paMultiSelect, paDialog, paReadOnly];
end;

function TAboutJetCheckbox.GetValue : string;
begin
    Result := '(About)';
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
