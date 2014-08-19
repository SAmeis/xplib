unit Main;

interface

uses
    Windows, Messages, SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls,
    ComCtrls, WinHotKey;

type
    TMainForm = class(TForm)
        Label1 :          TLabel;
        GroupBox1 :       TGroupBox;
        GroupBox2 :       TGroupBox;
        GroupBox3 :       TGroupBox;
        HotKey1 :         THotKey;
        BtnAdd :          TButton;
        BtnGetHotKey :    TButton;
        BtnTextToHotKey : TButton;
        Edit1 :           TEdit;
        GroupBox4 :       TGroupBox;
        BtnClear :        TButton;
        ListBox1 :        TListBox;
        BtnRemove :       TButton;
        BtnExit :         TButton;
        CheckBox1 :       TCheckBox;
        CheckBox2 :       TCheckBox;
        CheckBox3 :       TCheckBox;
        CheckBox4 :       TCheckBox;
        Label2 :          TLabel;
        BtnTest :         TButton;
        HotKeyManager1 :  THotKeyManager;
        ComboBox1 :       TComboBox;
        procedure FormCreate(Sender : TObject);
        procedure FormDestroy(Sender : TObject);
        procedure BtnAddClick(Sender : TObject);
        procedure BtnGetHotKeyClick(Sender : TObject);
        procedure BtnTextToHotKeyClick(Sender : TObject);
        procedure BtnTestClick(Sender : TObject);
        procedure BtnRemoveClick(Sender : TObject);
        procedure BtnClearClick(Sender : TObject);
        procedure BtnExitClick(Sender : TObject);
        procedure HotKeyManager1HotKeyPressed(HotKey : cardinal; Index : Word);
    private
        procedure AddHotKey(HotKey : cardinal);
        procedure GetPotentialKeys;
    end;

var
    MainForm : TMainForm;
    ToShow :   boolean = False;

implementation

{$R *.DFM}

const
    LOCALIZED_KEYNAMES = True;

type
    THotKeyEntry = class
        HotKey : cardinal;
        constructor Create(iHotKey : cardinal);
    end;

    TPotentialKey = class
        Key : Word;
        constructor Create(iKey : Word);
    end;

constructor THotKeyEntry.Create(iHotKey : cardinal);
begin
    inherited Create;
    HotKey := iHotKey;
end;

constructor TPotentialKey.Create(iKey : Word);
begin
    inherited Create;
    Key := iKey;
end;

{--------------------- TMainForm ----------------------}

procedure TMainForm.AddHotKey(HotKey : cardinal);
begin
    if HotKeyManager1.AddHotKey(HotKey) <> 0 then begin
        ListBox1.Items.AddObject(HotKeyToText(HotKey, LOCALIZED_KEYNAMES), THotKeyEntry.Create(HotKey));
        HotKey1.HotKey := 0;     // Just a nice touch
    end else begin
        MessageDlg(HotKeyToText(HotKey, LOCALIZED_KEYNAMES) + ' couldn''t be assigned to a hotkey.',
            mtWarning, [mbOK], 0);
    end;
end;


procedure TMainForm.FormCreate(Sender : TObject);
begin
    GetPotentialKeys;
end;


procedure TMainForm.FormDestroy(Sender : TObject);
var
    I : Integer;
begin
    for I := ComboBox1.Items.Count - 1 downto 0 do begin
        ComboBox1.Items.Objects[I].Free;
    end;
end;


procedure TMainForm.BtnAddClick(Sender : TObject);
var
    HotKeyVar : cardinal;
begin
    HotKeyVar := HotKey1.HotKey;
    if HotKeyVar = 0 then begin
        MessageDlg('No hotkey specified.', mtWarning, [mbOK], 0);
    end else begin
        AddHotKey(HotKeyVar);
    end;
end;


procedure TMainForm.BtnGetHotKeyClick(Sender : TObject);
var
    HotKeyVar :    cardinal;
    Modifiers :    Word;
    PotentialKey : TPotentialKey;
begin
    Modifiers := 0;
    if CheckBox1.Checked then begin
        Modifiers := Modifiers or MOD_CONTROL;
    end;
    if CheckBox2.Checked then begin
        Modifiers := Modifiers or MOD_SHIFT;
    end;
    if CheckBox3.Checked then begin
        Modifiers := Modifiers or MOD_ALT;
    end;
    if CheckBox4.Checked then begin
        Modifiers := Modifiers or MOD_WIN;
    end;
    if ComboBox1.ItemIndex <> -1 then begin
        PotentialKey := (ComboBox1.Items.Objects[ComboBox1.ItemIndex] as TPotentialKey);
        HotKeyVar    := GetHotKey(Modifiers, PotentialKey.Key);
        AddHotKey(HotKeyVar);
    end else begin
        MessageDlg('No key selected from the list.', mtWarning, [mbOK], 0);
    end;
end;


procedure TMainForm.BtnTextToHotKeyClick(Sender : TObject);
var
    HotKeyVar : cardinal;
begin
    HotKeyVar := TextToHotKey(Edit1.Text, LOCALIZED_KEYNAMES);
    if HotKeyVar <> 0 then begin
        AddHotKey(HotKeyVar);
    end else begin
        MessageDlg(Edit1.Text + ' doesn''t appear to be a hotkey.', mtWarning, [mbOK], 0);
    end;
end;


procedure TMainForm.BtnTestClick(Sender : TObject);
var
    HotKeyVar : cardinal;
    S1 : string;
begin
    HotKeyVar := TextToHotKey(Edit1.Text, LOCALIZED_KEYNAMES);
    if HotKeyVar <> 0 then begin
        S1 := '';
        if not HotKeyAvailable(HotKeyVar) then begin
            S1 := 'NOT ';
        end;
        MessageDlg(HotKeyToText(HotKeyVar, LOCALIZED_KEYNAMES) + ' is ' + S1 +
            'available for registration.', mtInformation, [mbOK], 0);
    end else begin
        MessageDlg(Edit1.Text + ' doesn''t appear to be a hotkey.', mtWarning, [mbOK], 0);
    end;
end;


procedure TMainForm.BtnRemoveClick(Sender : TObject);
var
    HotKeyEntry : THotKeyEntry;
begin
    if ListBox1.ItemIndex > -1 then begin
        HotKeyEntry := (ListBox1.Items.Objects[ListBox1.ItemIndex] as THotKeyEntry);
        if HotKeyManager1.RemoveHotKey(HotKeyEntry.HotKey) then begin
            HotKeyEntry.Free;
            ListBox1.Items.Delete(ListBox1.ItemIndex);
        end else begin
            MessageDlg(HotKeyToText(HotKeyEntry.HotKey, LOCALIZED_KEYNAMES) +
                ' couldn''t be removed.', mtWarning, [mbOK], 0);
        end;
    end;
end;


procedure TMainForm.BtnClearClick(Sender : TObject);
var
    I : Integer;
begin
    HotKeyManager1.ClearHotKeys;
    HotKey1.HotKey := 0;
    for I := 0 to ListBox1.Items.Count - 1 do begin
        (ListBox1.Items.Objects[I] as THotKeyEntry).Free;
    end;
    ListBox1.Items.Clear;
end;


procedure TMainForm.BtnExitClick(Sender : TObject);
begin
    Close;
end;

procedure ShowDesktop(const YesNo : boolean);
var
    h :  THandle;
    rc : boolean;
begin
    h := FindWindow('ProgMan', nil);
    h := GetWindow(h, GW_CHILD);
    if YesNo = True then begin
        rc := ShowWindow(h, SW_SHOW);
    end else begin
        rc := ShowWindow(h, SW_HIDE);
    end;
    if rc then begin
        messagebeep(0);
    end;

end;


procedure TMainForm.HotKeyManager1HotKeyPressed(HotKey : cardinal; Index : Word);
begin
    SetForegroundWindow(Application.Handle);
    MessageDlg('Hotkey ' + HotKeyToText(HotKey, LOCALIZED_KEYNAMES) + ' pressed.', mtInformation, [mbOK], 0);
end;


procedure TMainForm.GetPotentialKeys;

    procedure AddKeys(Min, Max : Word);
    var
        I : Integer;
        KeyName : string;
    begin
        for I := Min to Max do begin
            KeyName := HotKeyToText(I, LOCALIZED_KEYNAMES);
            if KeyName <> '' then begin
                ComboBox1.Items.AddObject(KeyName, TPotentialKey.Create(I));
            end;
        end;
    end;

begin
    // Add standard keys
    AddKeys($08, $09);
    AddKeys($0D, $0D);
    AddKeys($14, $91);
    AddKeys($BA, $FF);
    // Add extended keys
    AddKeys(VK_BROWSER_BACK, VK_LAUNCH_APP2);
    if ComboBox1.Items.Count > 0 then begin
        ComboBox1.ItemIndex := 0;
    end;
end;

end.
