unit HotKeyUnit;

interface

uses
    Classes;

procedure InitDll;


implementation

uses
    SysUtils, WinHotKey, Windows, Dialogs;

type
    TContainer = class    // A dummy class that allows us to use event methods
        hkm : THotKeyManager;
        procedure HotKeyPressed(HotKey : cardinal; Index : Word);
    end;

var
    Container : TContainer;

procedure TContainer.HotKeyPressed(HotKey : cardinal; Index : Word);
begin
    ShowMessage(HotKeyToText(HotKey, True) + ' pressed.');
end;

procedure InitDll;
var
    HotKey :      cardinal;
    HotKeyIndex : Word;
begin
    Container := TContainer.Create;
    Container.hkm := THotKeyManager.Create(nil);
    Container.hkm.OnHotKeyPressed := Container.HotKeyPressed;
    HotKey := GetHotKey(MOD_ALT, VK_HOME);
    if HotKey = 0 then begin
        ShowMessage('Invalid hotkey.');
    end;
    HotKeyIndex := Container.hkm.AddHotKey(HotKey);
    if HotKeyIndex = 0 then begin
        ShowMessage('Could not register hotkey.');
    end;
end;


initialization

finalization
    if Container <> nil then begin
        Container.hkm.Free;
        Container.Free;
    end;
end.
