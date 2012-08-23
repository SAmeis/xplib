{$IFDEF WinHotKey}
	{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I WinSysLib.inc}

///
///  <summary>
///    Gerencia as teclas de atalho
///	Criado em 20120821 ver notas em comentárioas abaixo no código
///  </summary>
///  <remarks>
///
///  </remarks>




 {*****************************************************************}
 { HotKeyManager is a component that allows you to add system-wide }
 { hotkeys to your application.                                    }

 { The component is freeware. Feel free to use and improve it.     }
 { I would be pleased to hear what you think.                      }

 { Troels Jakobsen - delphihacker@get2net.dk                       }
 { Copyright (c) 2003                                              }
 {*****************************************************************}

unit WinHotKey;

interface

uses
    Classes, Windows, Messages;

const
    // Self-invented names for the extended keys
    NAME_VK_BROWSER_BACK        = 'Browser Back';
    NAME_VK_BROWSER_FORWARD     = 'Browser Forward';
    NAME_VK_BROWSER_REFRESH     = 'Browser Refresh';
    NAME_VK_BROWSER_STOP        = 'Browser Stop';
    NAME_VK_BROWSER_SEARCH      = 'Browser Search';
    NAME_VK_BROWSER_FAVORITES   = 'Browser Favorites';
    NAME_VK_BROWSER_HOME        = 'Browser Start/Home';
    NAME_VK_VOLUME_MUTE         = 'Volume Mute';
    NAME_VK_VOLUME_DOWN         = 'Volume Down';
    NAME_VK_VOLUME_UP           = 'Volume Up';
    NAME_VK_MEDIA_NEXT_TRACK    = 'Next Track';
    NAME_VK_MEDIA_PREV_TRACK    = 'Previous Track';
    NAME_VK_MEDIA_STOP          = 'Stop Media';
    NAME_VK_MEDIA_PLAY_PAUSE    = 'Play/Pause Media';
    NAME_VK_LAUNCH_MAIL         = 'Start Mail';
    NAME_VK_LAUNCH_MEDIA_SELECT = 'Select Media';
    NAME_VK_LAUNCH_APP1         = 'Start Application 1';
    NAME_VK_LAUNCH_APP2         = 'Start Application 2';

type
    TOnHotKeyPressed = procedure(HotKey : cardinal; Index : Word) of object;

    PHotKeyRegistration = ^THotKeyRegistration;

    THotKeyRegistration = record
        HotKey:   cardinal;
        KeyIndex: Word;
    end;

    THotKeyManager = class(TComponent)
    private
        FHandle :          HWND;
        HotKeyList :       TList;
        FOnHotKeyPressed : TOnHotKeyPressed;
    protected
        function DisposeHotKey(hkr : PHotKeyRegistration) : boolean;
        procedure HookProc(var Msg : TMessage); virtual;        // Hook method
    public
        constructor Create(AOwner : TComponent); override;
        destructor Destroy; override;
        function AddHotKey(HotKey : cardinal) : Word;
        function ChangeHotKey(Index : Word; NewHotKey : cardinal) : Word;
        function RemoveHotKey(HotKey : cardinal) : boolean;
        function RemoveHotKeyByIndex(Index : Word) : boolean;
        procedure ClearHotKeys;
        function HotKeyValid(HotKey : cardinal) : boolean;
    published
        property OnHotKeyPressed : TOnHotKeyPressed read FOnHotKeyPressed write FOnHotKeyPressed;
    end;

function HotKeyAvailable(HotKey : cardinal) : boolean;
function GetHotKey(Modifiers, Key : Word) : cardinal;
procedure SeparateHotKey(HotKey : cardinal; var Modifiers, Key : Word);
function HotKeyToText(HotKey : cardinal; Localized : boolean) : string;
function TextToHotKey(Text : string; Localized : boolean) : cardinal;
function IsExtendedKey(Key : Word) : boolean;

procedure Register;


implementation

uses
    Forms, SysUtils;

const
    HotKeyAtomPrefix = 'HotKeyManagerHotKey';
    // Non-localized (!) modifier names
    ModName_Shift    = 'Shift';
    ModName_Ctrl     = 'Ctrl';
    ModName_Alt      = 'Alt';
    ModName_Win      = 'Win';

var
    EnglishKeyboardLayout : HKL;
    ShouldUnloadEnglishKeyboardLayout : boolean;
    // Localized (!) modifier names; initialized to English names
    LocalModName_Shift :    string = ModName_Shift;
    LocalModName_Ctrl :     string = ModName_Ctrl;
    LocalModName_Alt :      string = ModName_Alt;
    LocalModName_Win :      string = ModName_Win;

{------------------- Static methods -------------------}

function GetHotKey(Modifiers, Key : Word) : cardinal;
    // Get a shortcut from key and modifiers
const
    VK2_SHIFT   = 32;
    VK2_CONTROL = 64;
    VK2_ALT     = 128;
    VK2_WIN     = 256;
var
    hk : cardinal;
begin
    hk := 0;
    if (Modifiers and MOD_ALT) <> 0 then begin
        Inc(hk, VK2_ALT);
    end;
    if (Modifiers and MOD_CONTROL) <> 0 then begin
        Inc(hk, VK2_CONTROL);
    end;
    if (Modifiers and MOD_SHIFT) <> 0 then begin
        Inc(hk, VK2_SHIFT);
    end;
    if (Modifiers and MOD_WIN) <> 0 then begin
        Inc(hk, VK2_WIN);
    end;
    hk := hk shl 8;
    Inc(hk, Key);
    Result := hk;
end;


procedure SeparateHotKey(HotKey : cardinal; var Modifiers, Key : Word);
// Separate key and modifiers, so they can be used with RegisterHotKey
const
    VK2_SHIFT   = 32;
    VK2_CONTROL = 64;
    VK2_ALT     = 128;
    VK2_WIN     = 256;
var
    Virtuals : Integer;
    V : Word;
    //  x: Byte;
    x : Word;
begin
    Key := byte(HotKey);
    x   := HotKey shr 8;
    Virtuals := x;
    V   := 0;
    if (Virtuals and VK2_WIN) <> 0 then begin
        Inc(V, MOD_WIN);
    end;
    if (Virtuals and VK2_ALT) <> 0 then begin
        Inc(V, MOD_ALT);
    end;
    if (Virtuals and VK2_CONTROL) <> 0 then begin
        Inc(V, MOD_CONTROL);
    end;
    if (Virtuals and VK2_SHIFT) <> 0 then begin
        Inc(V, MOD_SHIFT);
    end;
    Modifiers := V;
end;


function HotKeyAvailable(HotKey : cardinal) : boolean;
    // Test if HotKey is available (test if it can be registered - by this or any app.)
var
    M, K : Word;
    Atom : Word;
begin
    Atom := GlobalAddAtom(PChar('HotKeyManagerHotKeyTest'));   //ALERTA: Pode-se pensar em colocar ParamStr(0) de modo a isolar apenas o aplicativo
    SeparateHotKey(HotKey, M, K);
    Result := RegisterHotKey(Application.Handle, Atom, M, K);
    if Result then begin
        UnregisterHotKey(Application.Handle, Atom);
    end;
    GlobalDeleteAtom(Atom);
end;


function IsExtendedKey(Key : Word) : boolean;
begin
    Result := ((Key >= VK_BROWSER_BACK) and (Key <= VK_LAUNCH_APP2));
end;


function HotKeyToText(HotKey : cardinal; Localized : boolean) : string;
    // Return localized(!) or English(!) string value from key combination

    function GetExtendedVKName(Key : Word) : string;
    begin
        case Key of
            VK_BROWSER_BACK : begin
                Result := NAME_VK_BROWSER_BACK;
            end;
            VK_BROWSER_FORWARD : begin
                Result := NAME_VK_BROWSER_FORWARD;
            end;
            VK_BROWSER_REFRESH : begin
                Result := NAME_VK_BROWSER_REFRESH;
            end;
            VK_BROWSER_STOP : begin
                Result := NAME_VK_BROWSER_STOP;
            end;
            VK_BROWSER_SEARCH : begin
                Result := NAME_VK_BROWSER_SEARCH;
            end;
            VK_BROWSER_FAVORITES : begin
                Result := NAME_VK_BROWSER_FAVORITES;
            end;
            VK_BROWSER_HOME : begin
                Result := NAME_VK_BROWSER_HOME;
            end;
            VK_VOLUME_MUTE : begin
                Result := NAME_VK_VOLUME_MUTE;
            end;
            VK_VOLUME_DOWN : begin
                Result := NAME_VK_VOLUME_DOWN;
            end;
            VK_VOLUME_UP : begin
                Result := NAME_VK_VOLUME_UP;
            end;
            VK_MEDIA_NEXT_TRACK : begin
                Result := NAME_VK_MEDIA_NEXT_TRACK;
            end;
            VK_MEDIA_PREV_TRACK : begin
                Result := NAME_VK_MEDIA_PREV_TRACK;
            end;
            VK_MEDIA_STOP : begin
                Result := NAME_VK_MEDIA_STOP;
            end;
            VK_MEDIA_PLAY_PAUSE : begin
                Result := NAME_VK_MEDIA_PLAY_PAUSE;
            end;
            VK_LAUNCH_MAIL : begin
                Result := NAME_VK_LAUNCH_MAIL;
            end;
            VK_LAUNCH_MEDIA_SELECT : begin
                Result := NAME_VK_LAUNCH_MEDIA_SELECT;
            end;
            VK_LAUNCH_APP1 : begin
                Result := NAME_VK_LAUNCH_APP1;
            end;
            VK_LAUNCH_APP2 : begin
                Result := NAME_VK_LAUNCH_APP2;
            end;
            else begin
                Result := EmptyStr;
            end;
        end;
    end;

    function GetModifierNames : string;
    var
        S : string;
    begin
        S := '';
        if Localized then begin
            if (HotKey and $4000) <> 0 then begin     // scCtrl
                S := S + LocalModName_Ctrl + '+';
            end;
            if (HotKey and $2000) <> 0 then begin      // scShift
                S := S + LocalModName_Shift + '+';
            end;
            if (HotKey and $8000) <> 0 then begin      // scAlt
                S := S + LocalModName_Alt + '+';
            end;
            if (HotKey and $10000) <> 0 then begin
                S := S + LocalModName_Win + '+';
            end;
        end else begin
            if (HotKey and $4000) <> 0 then begin      // scCtrl
                S := S + ModName_Ctrl + '+';
            end;
            if (HotKey and $2000) <> 0 then begin      // scShift
                S := S + ModName_Shift + '+';
            end;
            if (HotKey and $8000) <> 0 then begin      // scAlt
                S := S + ModName_Alt + '+';
            end;
            if (HotKey and $10000) <> 0 then begin
                S := S + ModName_Win + '+';
            end;
        end;
        Result := S;
    end;

    function GetVKName(Special : boolean) : string;
    var
        ScanCode : cardinal;
		 KeyName :  array[0..255] of char;
        oldkl :    HKL;
        Modifiers, Key : Word;
    begin
        Result := '';
        if Localized then begin       // Local language key names
            if Special then begin
				 ScanCode := (MapVirtualKey(byte(HotKey), MAPVK_VK_TO_VSC) shl 16) or (1 shl 24);
            end else begin
				 ScanCode := (MapVirtualKey(byte(HotKey), MAPVK_VK_TO_VSC) shl 16);
            end;
			 if ScanCode <> 0 then begin
				 GetKeyNameText(ScanCode, KeyName, Length(KeyName));
                Result := KeyName;
            end;
        end else begin                    // English key names
            if Special then begin
                ScanCode := (MapVirtualKeyEx(byte(HotKey), 0, EnglishKeyboardLayout) shl 16) or (1 shl 24);
            end else begin
                ScanCode := (MapVirtualKeyEx(byte(HotKey), 0, EnglishKeyboardLayout) shl 16);
            end;
            if ScanCode <> 0 then begin
                oldkl := GetKeyboardLayout(0);
                if oldkl <> EnglishKeyboardLayout then begin
                    ActivateKeyboardLayout(EnglishKeyboardLayout, 0);
                end;  // Set English kbd. layout
				 GetKeyNameText(ScanCode, KeyName, Length(KeyName));
                Result := KeyName;
                if oldkl <> EnglishKeyboardLayout then begin
                    if ShouldUnloadEnglishKeyboardLayout then begin
                        UnloadKeyboardLayout(EnglishKeyboardLayout);
                    end;     // Restore prev. kbd. layout
                    ActivateKeyboardLayout(oldkl, 0);
                end;
            end;
        end;

        if Length(Result) <= 1 then begin
            // Try the internally defined names
            SeparateHotKey(HotKey, Modifiers, Key);
            if IsExtendedKey(Key) then begin
                Result := GetExtendedVKName(Key);
            end;
        end;
    end;

var
    KeyName : string;
begin
    case byte(HotKey) of
        // PgUp, PgDn, End, Home, Left, Up, Right, Down, Ins, Del
        $21..$28, $2D, $2E : begin
            KeyName := GetVKName(True);
        end;
        else begin
            KeyName := GetVKName(False);
        end;
    end;
    Result := GetModifierNames + KeyName;
end;


function TextToHotKey(Text : string; Localized : boolean) : cardinal;
    // Return key combination created from (non-localized!) string value
var
    Tokens : TStringList;

    function GetModifiersValue : Word;
    var
        I : Integer;
        M : Word;
        ModName : string;
    begin
        M := 0;
        for I := 0 to Tokens.Count - 2 do begin
            ModName := Trim(Tokens[I]);
            if (AnsiCompareText(ModName, ModName_Shift) = 0) or
                (AnsiCompareText(ModName, LocalModName_Shift) = 0) then begin
                M := M or MOD_SHIFT;
            end else
            if (AnsiCompareText(ModName, ModName_Ctrl) = 0) or
                (AnsiCompareText(ModName, LocalModName_Ctrl) = 0) then begin
                M := M or MOD_CONTROL;
            end else
            if (AnsiCompareText(ModName, ModName_Alt) = 0) or
                (AnsiCompareText(ModName, LocalModName_Alt) = 0) then begin
                M := M or MOD_ALT;
            end else
            if (AnsiCompareText(ModName, ModName_Win) = 0) or
                (AnsiCompareText(ModName, LocalModName_Win) = 0) then begin
                M := M or MOD_WIN;
            end else begin
                // Unrecognized modifier encountered
                Result := 0;
                Exit;
            end;
        end;
        Result := M;
    end;

    function IterateVKNames(KeyName : string) : Word;
    var
        I : Integer;
        K : Word;
    begin
        K := 0;
        for I := $08 to $FF do begin       // The brute force approach
            if AnsiCompareText(KeyName, HotKeyToText(I, Localized)) = 0 then begin
                K := I;
                Break;
            end;
        end;
        Result := K;
    end;

    function GetKeyValue : Word;
    var
        K : Word;
        KeyName : string;
        C : char;
    begin
        K := 0;
        if Tokens.Count > 0 then begin
            KeyName := Trim(Tokens[Tokens.Count - 1]);
            if Length(KeyName) = 1 then begin
                C := UpCase(KeyName[1]);
                case byte(C) of
                    $30..$39, $41..$5A :     // 0..9, A..Z
                    begin
                        K := Ord(C);
                    end;
                    else begin
                        K := IterateVKNames(C);
                    end;
                end;
            end else begin
                if KeyName = 'Num' then begin  // Special handling for 'Num +'
                    KeyName := KeyName + ' +';
                end;
                if (KeyName <> ModName_Ctrl) and (KeyName <> LocalModName_Ctrl) and
                    (KeyName <> ModName_Alt) and (KeyName <> LocalModName_Alt) and
                    (KeyName <> ModName_Shift) and (KeyName <> LocalModName_Shift) and
                    (KeyName <> ModName_Win) and (KeyName <> LocalModName_Win) then begin
                    K := IterateVKNames(KeyName);
                end;
            end;
        end;
        Result := K;
    end;

var
    Modifiers, Key : Word;
begin
    Tokens := TStringList.Create;
    try
        ExtractStrings(['+'], [' '], PChar(Text), Tokens);
        Modifiers := GetModifiersValue;
        if (Modifiers = 0) and (Tokens.Count > 1) then begin // Something went wrong when translating the modifiers
            Result := 0;
        end else begin
            Key := GetKeyValue;
            if Key = 0 then begin // Something went wrong when translating the key
                Result := 0;
            end else begin
                Result := GetHotKey(Modifiers, Key);
            end;
        end;
    finally
        Tokens.Free;
    end;
end;

{------------------- THotKeyManager -------------------}

constructor THotKeyManager.Create(AOwner : TComponent);
begin
    inherited Create(AOwner);
    HotKeyList := TList.Create;
    if not (csDesigning in ComponentState) then begin  // Create a virtual window with a callback method and use it as handle
        FHandle := Classes.AllocateHWnd(HookProc);
    end;
end;


destructor THotKeyManager.Destroy;
begin
    ClearHotKeys;
    HotKeyList.Free;
    if not (csDesigning in ComponentState) then begin // Destroy our virtual window
        Classes.DeallocateHWnd(FHandle);
    end;
    inherited Destroy;
end;


function THotKeyManager.AddHotKey(HotKey : cardinal) : Word;
var
    hkr :  PHotKeyRegistration;
    Modifiers, Key : Word;
    Atom : Word;
begin
    SeparateHotKey(HotKey, Modifiers, Key);
    // Create unique id (global atom)
    Atom := GlobalAddAtom(PChar(HotKeyAtomPrefix + IntToStr(HotKey)));
    // Register
    if RegisterHotKey(FHandle, Atom, Modifiers, Key) then begin
        hkr := New(PHotKeyRegistration);
        hkr.HotKey := HotKey;
        hkr.KeyIndex := Atom;
        HotKeyList.Add(hkr);
        Result := Atom;
    end else begin
        GlobalDeleteAtom(Atom);
        Result := 0;
    end;
end;


function THotKeyManager.ChangeHotKey(Index : Word; NewHotKey : cardinal) : Word;
var
    I :   Integer;
    hkr : PHotKeyRegistration;
begin
    Result := 0;
    for I := 0 to HotKeyList.Count - 1 do begin
        hkr := PHotKeyRegistration(HotKeyList[I]);
        if hkr.KeyIndex = Index then begin
            RemoveHotKeyByIndex(hkr.KeyIndex);
            Result := AddHotKey(NewHotKey);
            Exit;
        end;
    end;
end;


function THotKeyManager.RemoveHotKey(HotKey : cardinal) : boolean;
var
    I :   Integer;
    hkr : PHotKeyRegistration;
begin
    Result := False;
    for I := 0 to HotKeyList.Count - 1 do begin
        hkr := PHotKeyRegistration(HotKeyList[I]);
        if hkr.HotKey = HotKey then begin
            Result := DisposeHotKey(hkr);
            HotKeyList.Remove(hkr);
            Exit;
        end;
    end;
end;


function THotKeyManager.RemoveHotKeyByIndex(Index : Word) : boolean;
var
    I :   Integer;
    hkr : PHotKeyRegistration;
begin
    Result := False;
    for I := 0 to HotKeyList.Count - 1 do begin
        hkr := PHotKeyRegistration(HotKeyList[I]);
        if hkr.KeyIndex = Index then begin
            Result := DisposeHotKey(hkr);
            HotKeyList.Remove(hkr);
            Exit;
        end;
    end;
end;


function THotKeyManager.HotKeyValid(HotKey : cardinal) : boolean;
    // Test if HotKey is valid (test if it can be registered even if this app. already registered it)
var
    M, K : Word;
    WasRegistered : boolean;
    Atom : Word;
begin
    Atom := GlobalAddAtom(PChar(HotKeyAtomPrefix + IntToStr(HotKey)));
    SeparateHotKey(HotKey, M, K);
    WasRegistered := UnregisterHotKey(FHandle, Atom);
    if WasRegistered then begin
        RegisterHotKey(FHandle, Atom, M, K);
        Result := True;
    end else begin
        Result := RegisterHotKey(FHandle, Atom, M, K);
        if Result then begin
            UnregisterHotKey(FHandle, Atom);
        end;
    end;
    GlobalDeleteAtom(Atom);
end;


procedure THotKeyManager.ClearHotKeys;
var
    I :   Integer;
    hkr : PHotKeyRegistration;
begin
    for I := HotKeyList.Count - 1 downto 0 do begin
        hkr := PHotKeyRegistration(HotKeyList[I]);
        DisposeHotKey(hkr);
        HotKeyList.Remove(hkr);
    end;
end;


function THotKeyManager.DisposeHotKey(hkr : PHotKeyRegistration) : boolean;
begin
    // Unregister using previously assigned id (global atom)
    Result := UnregisterHotKey(FHandle, hkr.KeyIndex);
    GlobalDeleteAtom(hkr.KeyIndex);
    if Result then begin
        Dispose(hkr);
    end;
end;


procedure THotKeyManager.HookProc(var Msg : TMessage);

    function HotKeyFound(HotKey : cardinal) : boolean;
    var
        I : Integer;
    begin
        Result := False;
        for I := 0 to HotKeyList.Count - 1 do begin
            if PHotKeyRegistration(HotKeyList[I]).HotKey = HotKey then begin
                Result := True;
                Break;
            end;
        end;
    end;

var
    Modifier : cardinal;
begin
    case Msg.Msg of
        WM_HOTKEY : begin
            if Assigned(FOnHotKeyPressed) then begin
                // Get modifier keys status
                Modifier := 0;
                if (Msg.LParamLo and MOD_SHIFT) <> 0 then begin
                    Inc(Modifier, $2000);
                end;        // scShift
                if (Msg.LParamLo and MOD_CONTROL) <> 0 then begin
                    Inc(Modifier, $4000);
                end;        // scCtrl
                if (Msg.LParamLo and MOD_ALT) <> 0 then begin
                    Inc(Modifier, $8000);
				 end;        // scAlt
                if (Msg.LParamLo and MOD_WIN) <> 0 then begin
                    Inc(Modifier, $10000);
                end;
				 ///Check if the hotkey is in the list (it's possible user has registered hotkeys without using
				 ///this component and handles these hotkeys by himself).
                if HotKeyFound(Msg.LParamHi + Modifier) then begin
                    OnHotKeyPressed(Msg.LParamHi + Modifier, Msg.WParam);
                end;
            end;
        end;
    end;

    // Pass the message on
    Msg.Result := DefWindowProc(FHandle, Msg.Msg, Msg.wParam, Msg.lParam);
end;


function IsUpperCase(S : string) : boolean;
var
    I : Integer;
    C : byte;
begin
    Result := True;
    for I := 1 to Length(S) do begin
        C := Ord(S[I]);
        if (C < $41) or (C > $5A) then begin
            Result := False;
            Exit;
        end;
    end;
end;


procedure Register;
begin
	 RegisterComponents('Win32', [THotKeyManager]);
end;

var
	 Layouts : cardinal;
    //  kllist: array of HKL;
	 kllist : array[0..100] of HKL;
	 I : Integer;

initialization
	 // Get localized names of modfiers
	 LocalModName_Shift := HotKeyToText( VK_SHIFT { $10 }, True);
	 LocalModName_Ctrl  := HotKeyToText( VK_CONTROL { $11 }, True);
	 LocalModName_Alt   := HotKeyToText( VK_MENU { $12 == alt }, True);
    if IsUpperCase(LocalModName_Alt) then begin
        LocalModName_Win := UpperCase(LocalModName_Win);
    end;

  { To get the non-localized (English) names of keys and modifiers we must load
    and activate the US English keyboard layout. However, we shouldn't change the
    user's current list of layouts, so the English layout should be unloaded
    after it is used (in HotKeyToText) in case it wasn't originally part of the
     user's list of layouts. It's a bit of a hack, but it's the only way I can
    think of to get the English names. }

    // Get all keyboard layouts
    //  Layouts := GetKeyboardLayoutList(0, kllist);
    //  SetLength(kllist, Layouts);
	 Layouts := GetKeyboardLayoutList(100, kllist);

    // Load (but don't activate) US English keyboard layout for use in HotKeyToText
	 EnglishKeyboardLayout := LoadKeyboardLayout(PChar('00000409'), KLF_NOTELLSHELL);

    // Examine if US English layout is already in user's list of keyboard layouts
    ShouldUnloadEnglishKeyboardLayout := True;
    for I := 0 to Layouts - 1 do begin
        if kllist[I] = EnglishKeyboardLayout then begin
            ShouldUnloadEnglishKeyboardLayout := False;
            Exit;
        end;
    end;

finalization
    if ShouldUnloadEnglishKeyboardLayout then begin
        UnloadKeyboardLayout(EnglishKeyboardLayout);
    end;   // Restore prev. kbd. layout

end.
