{$IFDEF DlgTxtEditor }
  {$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I DlgLib.inc}

unit DlgTxtEditor;

{{

Janela genérica para edição e visualização de textos.

}

interface

uses
    SysUtils, Windows, Messages, Classes, Graphics, Controls, Forms, Dialogs, Menus, StdCtrls, ComCtrls, Buttons, ExtCtrls;

type
    TXPTxtEditForm = class(TForm)
        Editor :      TRichEdit;
        FontDialog :  TFontDialog;
        MainMenu :    TMainMenu;
        MenuAlignCenter : TMenuItem;
        MenuAlignLeft : TMenuItem;
        MenuAlignRight : TMenuItem;
        MenuChangeFont : TMenuItem;
        MenuCharacter : TMenuItem;
        MenuClose :   TMenuItem;
        MenuCopy :    TMenuItem;
        MenuCut :     TMenuItem;
        MenuDeleteText : TMenuItem;
        MenuEdit :    TMenuItem;
        MenuExit :    TMenuItem;
        MenuFile :    TMenuItem;
        MenuNew :     TMenuItem;
        MenuOpen :    TMenuItem;
        MenuPaste :   TMenuItem;
        MenuPrint :   TMenuItem;
        MenuPrinterSetup : TMenuItem;
        MenuSave :    TMenuItem;
        MenuSaveAs :  TMenuItem;
        MenuSelectall : TMenuItem;
        MenuSeparator2 : TMenuItem;
        MenuSeparator3 : TMenuItem;
        MenuSeparator4 : TMenuItem;
        MenuSeparator5 : TMenuItem;
        MenuWordWrap : TMenuItem;
        PopMenuCopy : TMenuItem;
        PopMenuCut :  TMenuItem;
        PopMenuPaste : TMenuItem;
        PopupMenu :   TPopupMenu;
        PrintDialog : TPrintDialog;
        PrinterSetupDialog : TPrinterSetupDialog;
        SaveFileDialog : TSaveDialog;
        procedure AlignClick(Sender : TObject);
        procedure FormCloseQuery(Sender : TObject; var CanClose : boolean);
        procedure FormCreate(Sender : TObject);
        procedure MenuChangeFontClick(Sender : TObject);
        procedure MenuCloseClick(Sender : TObject);
        procedure MenuCopyClick(Sender : TObject);
        procedure MenuCutClick(Sender : TObject);
        procedure MenuDeleteTextClick(Sender : TObject);
        procedure MenuEditClick(Sender : TObject);
        procedure MenuExitClick(Sender : TObject);
        procedure MenuNewClick(Sender : TObject);
        procedure MenuOpenClick(Sender : TObject);
        procedure MenuPasteClick(Sender : TObject);
        procedure MenuPrintClick(Sender : TObject);
        procedure MenuPrinterSetupClick(Sender : TObject);
        procedure MenuSaveAsClick(Sender : TObject);
        procedure MenuSaveClick(Sender : TObject);
        procedure MenuSelectallClick(Sender : TObject);
        procedure MenuWordWrapClick(Sender : TObject);
    private
        PathName : string;
    public
        procedure Open(const AFileName : string);
        class function ShowTextFile(const FileName : string; ReadOnly : boolean) : Integer;
        class function ShowTextStream(Stream : TStream; const Tile : string; ReadOnly : boolean) : Integer;
        class function ShowTextStrings(const Strings, Tile : string; ReadOnly : boolean) : Integer; overload;
        class function ShowTextStrings(Strings : TStrings; const Tile : string; ReadOnly : boolean) : Integer; overload;
    end;


var
    XPTxtEditForm : TXPTxtEditForm;

const
    DefaultFileName = 'Sem Título';

implementation

uses
    Clipbrd, Printers;

{$R *.DFM}

{-**********************************************************************
************************************************************************
******************
******************  Class:    TXPTxtEditForm
******************  Category: No category
******************
************************************************************************
************************************************************************}
{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPTxtEditForm.AlignClick(Sender : TObject);
begin
    Self.MenuAlignLeft.Checked   := False;
    Self.MenuAlignRight.Checked  := False;
    Self.MenuAlignCenter.Checked := False;
    TMenuItem(Sender).Checked    := True;
    if Self.MenuAlignLeft.Checked then begin
        Editor.Paragraph.Alignment := taLeftJustify;
    end else begin
        if Self.MenuAlignRight.Checked then begin
            Editor.Paragraph.Alignment := taRightJustify;
        end else begin
            if Self.MenuAlignCenter.Checked then begin
                Editor.Paragraph.Alignment := taCenter;
            end;
        end;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPTxtEditForm.FormCloseQuery(Sender : TObject; var CanClose : boolean);
const
    SWarningText = 'Salvar alteraçãoes para %s?';
begin
    if Editor.Modified then begin
        case MessageDlg(Format(SWarningText, [PathName]), mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
            idYes : begin
                Self.MenuSaveClick(Self);
            end;
            idCancel : begin
                CanClose := False;
            end;
        end;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPTxtEditForm.FormCreate(Sender : TObject);

//----------------------------------------------------------------------------------------------------------------------

begin
    PathName := DefaultFileName;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPTxtEditForm.MenuChangeFontClick(Sender : TObject);
begin
    Self.FontDialog.Font := Editor.Font;
    if Self.FontDialog.Execute then begin
        Editor.SelAttributes.Assign(Self.FontDialog.Font);
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPTxtEditForm.MenuCloseClick(Sender : TObject);

//----------------------------------------------------------------------------------------------------------------------

begin
    Close;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPTxtEditForm.MenuCopyClick(Sender : TObject);
begin
    Editor.CopyToClipboard;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPTxtEditForm.MenuCutClick(Sender : TObject);
begin
    Editor.CutToClipboard;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPTxtEditForm.MenuDeleteTextClick(Sender : TObject);
begin
    Editor.ClearSelection;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPTxtEditForm.MenuEditClick(Sender : TObject);
var
    HasSelection : boolean;
begin
    Self.MenuPaste.Enabled := Clipboard.HasFormat(CF_TEXT);
    Self.PopMenuPaste.Enabled := Self.MenuPaste.Enabled;
    HasSelection := Editor.SelLength > 0;
    Self.MenuCut.Enabled := HasSelection;
    Self.PopMenuCut.Enabled := HasSelection;
    Self.MenuCopy.Enabled := HasSelection;
    Self.PopMenuCopy.Enabled := HasSelection;
    Self.MenuDeleteText.Enabled := HasSelection;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPTxtEditForm.MenuExitClick(Sender : TObject);

//----------------------------------------------------------------------------------------------------------------------

begin
    Self.Close();
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPTxtEditForm.MenuNewClick(Sender : TObject);

//----------------------------------------------------------------------------------------------------------------------

begin
    //  FrameForm.New1Click(Sender);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPTxtEditForm.MenuOpenClick(Sender : TObject);

//----------------------------------------------------------------------------------------------------------------------

begin
    //  FrameForm.Open1Click(Sender);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPTxtEditForm.MenuPasteClick(Sender : TObject);
begin
    Editor.PasteFromClipboard;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPTxtEditForm.MenuPrintClick(Sender : TObject);

//----------------------------------------------------------------------------------------------------------------------

begin
    if Self.PrintDialog.Execute then begin
        Editor.Print('Resumo de operação');
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPTxtEditForm.MenuPrinterSetupClick(Sender : TObject);

//----------------------------------------------------------------------------------------------------------------------

begin
    Self.PrinterSetupDialog.Execute;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPTxtEditForm.MenuSaveAsClick(Sender : TObject);
begin
    SaveFileDialog.FileName := PathName;
    if SaveFileDialog.Execute then begin
        PathName := SaveFileDialog.FileName;
        Caption  := ExtractFileName(PathName);
        Self.MenuSaveClick(Sender);
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPTxtEditForm.MenuSaveClick(Sender : TObject);
begin
    if PathName = DefaultFileName then begin
        Self.MenuSaveAsClick(Sender);
    end else begin
        Editor.Lines.SaveToFile(PathName);
        Editor.Modified := False;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPTxtEditForm.MenuSelectallClick(Sender : TObject);
begin
    Editor.SelectAll;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPTxtEditForm.MenuWordWrapClick(Sender : TObject);
begin
    Editor.WordWrap := not Editor.WordWrap; // toggle word wrapping
    if Editor.WordWrap then begin
        Editor.ScrollBars := ssVertical;
    end else begin
        Editor.ScrollBars := ssBoth;
    end;
    Self.MenuWordWrap.Checked := Editor.WordWrap; // set menu item check
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPTxtEditForm.Open(const AFileName : string);
begin
    PathName := AFileName;
    Caption  := ExtractFileName(AFileName);
    Editor.Lines.LoadFromFile(PathName);
    Editor.SelStart := 0;
    Editor.Modified := False;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
class function TXPTxtEditForm.ShowTextFile(const FileName : string; ReadOnly : boolean) : Integer;
var
    Dlg : TXPTxtEditForm;
begin
    Application.CreateForm(TXPTxtEditForm, Dlg);
    try
        Dlg.Open(FileName);
        Dlg.Editor.ReadOnly := ReadOnly;
        Dlg.Caption := FileName;
        Result      := Dlg.ShowModal();
    finally
        Dlg.Free();
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
class function TXPTxtEditForm.ShowTextStream(Stream : TStream; const Tile : string; ReadOnly : boolean) : Integer;
var
    Dlg : TXPTxtEditForm;
begin
    Application.CreateForm(TXPTxtEditForm, Dlg);
    try
        Dlg.Editor.Lines.LoadFromStream(Stream);
        Dlg.Editor.ReadOnly := ReadOnly;
        Dlg.Caption := Tile;
        Result      := Dlg.ShowModal();
    finally
        Dlg.Free();
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
class function TXPTxtEditForm.ShowTextStrings(const Strings, Tile : string; ReadOnly : boolean) : Integer;
var
    Dlg : TXPTxtEditForm;
begin
    Application.CreateForm(TXPTxtEditForm, Dlg);
    try
        Dlg.Editor.Lines.Text := Strings;
        Dlg.Editor.ReadOnly := ReadOnly;
        Dlg.Caption := Tile;
        Result      := Dlg.ShowModal();
    finally
        Dlg.Free();
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
class function TXPTxtEditForm.ShowTextStrings(Strings : TStrings; const Tile : string; ReadOnly : boolean) : Integer;

    //----------------------------------------------------------------------------------------------------------------------

begin
    Result := ShowTextStrings(Strings.Text, Tile, ReadOnly);
end;

end.
