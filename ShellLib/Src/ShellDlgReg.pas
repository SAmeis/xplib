unit ShellDlgReg;

interface

uses DesignEditors, DesignIntf, ShellShlDlg;

{Property editor interfaces}
type
  TPathProperty = class(TStringProperty)
  private
    procedure OnChange(Sender: TObject; var Path: String; var StatusText: String; var OKState: TkbOKState);
  public
    procedure Edit;                               override;
    function  GetAttributes: TPropertyAttributes; override;
    function  GetEditLimit:  Integer;             override;
  end;


{Unit public method interfaces}
procedure Register;



implementation

uses Windows, Classes, ShellPIDL;

{Unit private constants}
const
  {*****************************************************}
  {* Change this constant to set the default component *}
  {* palette page for installation.                    *}
  {*****************************************************}
  PalettePageName = 'Dialogs';



procedure TPathProperty.OnChange(Sender: TObject; var Path: String; var StatusText: String; var OKState: TkbOKState);
begin
  {Whenever the selection changes, update the status text to
   reflect the currently selected path.}
  StatusText := Path;
end;

procedure TPathProperty.Edit;
var
  PathDlg: TkbBrowseForFolderDialog;
begin
  {Create an instance of the browser dialog.}
  PathDlg := TkbBrowseForFolderDialog.Create(nil);
  try {..finally}

    {Initialize the dialog's properties to suit this property editor.}
    PathDlg.Caption          := 'Editing ' + TComponent(GetComponent(0)).Name + '.' + Self.GetName;
    PathDlg.InstructionText  := 'Select a directory path:';
    PathDlg.RootFolder       := kbsdDrives;
    PathDlg.Filter           := kbsdBrowseForDirectories;
    PathDlg.CanExpandDomains := False;
    PathDlg.ShowStatusText   := True;
    PathDlg.OnChange         := Self.OnChange;

    {Execute the dialog. If the OK button is clicked, update
     the value of the property with the selected path.}
    if (PathDlg.Execute) then begin
      Self.SetValue(PathDlg.RootPath);
    end; {if}

  {Ensure the dialog object is freed.}
  finally
    PathDlg.Free;
  end; {try..finally}
end;

function TPathProperty.GetAttributes: TPropertyAttributes;
begin
  {Have the property editor show the dialog button.}
  Result := [paDialog];
end;

function TPathProperty.GetEditLimit: Integer;
begin
  {Limit the string length to the maximum size of a path.}
  Result := MAX_PATH;
end;


{Unit public method implementations}
procedure Register;
begin
  {Register components.}
  {$WARN SYMBOL_PLATFORM OFF }

  RegisterComponents(PalettePageName, [TkbBrowseForFolderDialog, TkbShellAboutDialog, TkbFormatDriveDialog,
                                       TkbPickIconDialog, TkbFindFilesDialog, TkbRunFileDialog,
                                       TkbRestartWindowsDialog, TkbObjectPropertiesDialog]);

  {Register property editors.}
  RegisterPropertyEditor(TypeInfo(String), TkbBrowseForFolderDialog, 'Path', TPathProperty);
end;



end.
