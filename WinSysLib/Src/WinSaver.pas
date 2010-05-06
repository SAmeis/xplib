{$IFDEF WinSaver}
	{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I WinSysLib.inc}

unit Winsaver;

{{
Removidos os "with" do codigo abaixo.

Revision: 7/8/2006 - Roger
}

interface

uses
    SysUtils, Windows, Forms, Classes, WinReg32;

function FindFormClassByName(const Name : string) : TForm;

type

    TVisualWinProperties = record
        Heigth:  Integer;
        Width:   Integer;
        Visible: boolean;
        Top:     Integer;
        Left:    Integer;
        State:   TWindowState;
    end;

    TWinSaver32 = class(TComponent)
    private
        FSaveSize : boolean;
        FSaveLocation : boolean;
        FSaveState : boolean;
        FReg :     TRegistryNT;
        FRegPath : string;
        FSaveVisibility : boolean;
        procedure LoadClassNames(List : TStringList);
        procedure SetRegPath(const Value : string);
    public
        constructor Create(AOwner : TComponent); override;
        destructor Destroy; override;
        procedure NormalizeScalePos(Resize : boolean); //Repoe proporcionalmente a posicao para a nova resolucao
        procedure Save;
        procedure Restore;
        procedure SaveChildrens(MDIForm : TForm);
        procedure RestoreChildrens(MDIForm : TForm);
        procedure SaveWindow(AFormWin : TForm);
        procedure RestoreWindow(AFormWin : TForm);
        function GetRegWinProperties(const AClassWinName : string) : TVisualWinProperties;
        function GetWindowProperties(AFormWin : TForm) : TVisualWinProperties;
    published
        property RegPath : string read FRegPath write SetRegPath;
        property SaveSize : boolean read FSaveSize write FSaveSize default True;
        property SaveLocation : boolean read FSaveLocation write FSaveLocation default True;
        property SaveState : boolean read FSaveState write FSaveState default True;
        property SaveVisibility : boolean read FSaveVisibility write FSaveVisibility;
    end;


    TWinSaver = class(TComponent)
    private
        FIniFile :      string;
        FIsLocal :      boolean;
        FIniSect :      string;
        FSaveSize :     boolean;
        FSaveLocation : boolean;
        FSaveState :    boolean;
        FRealIniFile :  string;
        procedure OpenIniFile;
    public
        constructor Create(AOwner : TComponent); override;
        procedure Save;
        procedure Restore;
        procedure SaveChildren(TheForm : TForm);
        procedure RestoreChildren(TheForm : TForm);
        procedure SaveWindow(TheForm : TForm);
        procedure RestoreWindow(TheForm : TForm);
    published
        property IniFile : string read FIniFile write FIniFile;
        property LocalDir : boolean read FIsLocal write FIsLocal default True;
        property IniSection : string read FIniSect write FIniSect;
        property SaveSize : boolean read FSaveSize write FSaveSize default True;
        property SaveLocation : boolean read FSaveLocation write FSaveLocation default True;
        property SaveState : boolean read FSaveState write FSaveState default True;
    end;

procedure Register;


implementation

uses
    IniFiles, Str_Pas, FileHnd, Math;

const
    _PRE_HRES_ENTRY_ = 'HRES';
    _PRE_VRES_ENTRY_ = 'VRES';
    _TMARK_ = '_Top';
    _LMARK_ = '_Left';
    _WMARK_ = '_Width';
    _HMARK_ = '_Height';
    _SMARK_ = '_WindowState';
    _VMARK_ = '_Visible';
    _BASE_DEF_KEY_ = 'HKEY_CURRENT_USER\software\Digitalsys\Windows\';

var
    Ini : TIniFile; {fica melhor aqui devido ao construtor/destructor do tipo}




function FindFormClassByName(const Name : string) : TForm;
var
    i : Integer;
begin
    Result := nil;
    for i := 0 to Screen.FormCount - 1 do begin
        if Screen.Forms[i].Name = Name then begin
            Result := Screen.Forms[i];
            Exit;
        end;
    end;
end;


{------------------------------------------------------------------------------}
constructor TWinSaver.Create(AOwner : TComponent);
begin
    inherited Create(AOwner);
    {Inicializa campos}
    FIsLocal      := True;
    FSaveSize     := True;
    FSaveLocation := True;
    FSaveState    := True;
    {Usada na criacao dinamica para o caso de nome omisso}
    if csDesigning in Self.ComponentState then begin
        FIniFile := Copy(AOwner.ClassName, 1, 8) + '.INI';
    end else begin
        FIniFile := ChangeFileExt(ExtractFileName(Application.ExeName), '.INI');
    end;
    FIniSect := Self.Name;{Idem caso acima para secao do arquivo}
end;

{------------------------------------------------------------------------------}
procedure TWinSaver.RestoreWindow(TheForm : TForm);
var
    Created :  boolean;
    TmpState : Integer;
begin
    Created := False;
    try
        if Ini = nil then begin
            Created := True;
            OpenIniFile;
        end else begin
            Created := False;
        end;
        if FSaveSize then begin
            TheForm.Width  := Ini.ReadInteger(FIniSect, TheForm.ClassName + _WMARK_, TheForm.Width);
            TheForm.Height := Ini.ReadInteger(FIniSect, TheForm.ClassName + _HMARK_, TheForm.Height);
        end;
        if FSaveLocation then begin
            TheForm.Top  := Ini.ReadInteger(FIniSect, TheForm.ClassName + _TMARK_, TheForm.Top);
            TheForm.Left := Ini.ReadInteger(FIniSect, TheForm.ClassName + _LMARK_, TheForm.Left);
        end;
        if FSaveState then begin
            TmpState := Integer(wsNormal);
            TheForm.WindowState := TWindowState(Ini.ReadInteger(FIniSect, TheForm.ClassName + _SMARK_, TmpState));
        end;
    finally
        if Created then begin
            Ini.Free;
        end;
    end;
end;

{------------------------------------------------------------------------------}
procedure TWinSaver.RestoreChildren(TheForm : TForm);
var
    i : Integer;
begin
    for i := 0 to TheForm.MDIChildCount - 1 do begin
        RestoreWindow(TheForm.MDIChildren[i]);
    end;
end;

{------------------------------------------------------------------------------}
procedure TWinSaver.Restore;
var
    i : Integer;

begin
    OpenIniFile;
    try
        for i := 0 to Application.ComponentCount - 1 do begin
            if (Application.Components[i] is TForm) then begin
                RestoreWindow(TForm(Application.Components[i]));
                if TForm(Application.Components[i]).FormStyle = fsMDIForm then begin
                    RestoreChildren(TForm(Application.Components[i]));
                end;
            end;
        end;
    finally
        Ini.Free;
    end;
end;

{------------------------------------------------------------------------------}
procedure TWinSaver.Save;
var
    i : Integer;
begin
    OpenIniFile;
    try
        for i := 0 to Application.ComponentCount - 1 do begin
            if (Application.Components[i] is TForm) then begin
                SaveWindow(TForm(Application.Components[i]));
                if TForm(Application.Components[i]).FormStyle = fsMDIForm then begin
                    SaveChildren(TForm(Application.Components[i]));
                end;
            end;
        end;
    finally
        Ini.Free;
    end;
end;

{------------------------------------------------------------------------------}
procedure TWinSaver.SaveWindow(TheForm : TForm);
var
    Created : boolean;
begin
    Created := False;
    try
        if Ini = nil then begin
            Created := True;
            OpenIniFile;
        end else begin
            Created := False;
        end;
        if SaveSize then begin
            Ini.WriteInteger(FIniSect, TheForm.ClassName + _WMARK_, TheForm.Width);
            Ini.WriteInteger(FIniSect, TheForm.ClassName + _HMARK_, TheForm.Height);
        end;
        if FSaveLocation then begin
            Ini.WriteInteger(FIniSect, TheForm.ClassName + _TMARK_, TheForm.Top);
            Ini.WriteInteger(FIniSect, TheForm.ClassName + _LMARK_, TheForm.Left);
        end;
        if FSaveState then begin
            Ini.WriteInteger(FIniSect, TheForm.ClassName + _SMARK_, Integer(TheForm.WindowState));
        end;
    finally
        if Created then begin
            Ini.Free;
        end;
    end;
end;

{------------------------------------------------------------------------------}
procedure TWinSaver.SaveChildren(TheForm : TForm);
var
    i : Integer;
begin
    for i := 0 to TheForm.MDIChildCount - 1 do begin
        SaveWindow(TheForm.MDIChildren[i]);
    end;
end;

{------------------------------------------------------------------------------}
procedure TWinSaver.OpenIniFile;
var
    buf : PChar;
begin
    if FIniFile = '' then begin
        { if IniFile property is empty, set to appname.ini}
        FIniFile := ChangeFileExt(ExtractFileName(Application.ExeName), '.INI');
    end else begin   { change to '.ini' extesion }
        FIniFile := ChangeFileExt(ExtractFileName(FIniFile), '.INI');
        if not FIsLocal then begin
            GetMem(buf, 256);
            try
                GetWindowsDirectory(buf, 256);
                FRealIniFile := StrPas(buf) + '\' + FIniFile;
            finally
                FreeMem(buf, 256);
            end;
        end else begin
            FRealIniFile := ExtractFilePath(Application.ExeName) + FIniFile;
        end;
        if FIniSect = EmptyStr then begin
            {if IniSection property is empty, set to[WinSaver]}
            FIniSect := 'WinSaver';
        end;
        Ini := TIniFile.Create(FRealIniFile);
    end;
end;

constructor TWinSaver32.Create(AOwner : TComponent);
{{
Inicia acesso ao registro da maquina para o caminho especificado.

Revision: 7/8/2006 - Roger  
}
begin
    inherited Create(AOwner);
    {Inicializa campos}
    FSaveSize := True;
    FSaveLocation := True;
    FSaveState := True;
    FSaveVisibility := True;
    FRegPath := _BASE_DEF_KEY_ + Application.Title + '\Windows';
    FReg := TRegistryNT.Create;
    FReg.AccessMode := (KEY_WRITE or KEY_READ);
    if not (csDesigning in Self.ComponentState) then begin
        try
            FReg.OpenFullKey(FRegPath, True);
        except
            Self.Free;
            raise;
        end;
    end;
end;


destructor TWinSaver32.Destroy;
    //----------------------------------------------------------------------------------------------------------------------------------
begin
    FReg.Free;
    inherited Destroy;
end;


function TWinSaver32.GetRegWinProperties(const AClassWinName : string) : TVisualWinProperties;
    {-------------------------------------------------------------------------------------------------------------}
var
    TmpVal :   Integer;
    FormInst : TForm;
begin
    FormInst := FindFormClassByName(AClassWinName); //Referenced From instance
    //Location
    try
        FReg.ReadFullInteger(FRegPath + AClassWinName + _TMARK_, Result.Top);
    except
        if Assigned(FormInst) then begin
            Result.Top := FormInst.Top;
        end else begin
            Result.Top := 0;
        end;
    end;
    try
        FReg.ReadFullInteger(FRegPath + AClassWinName + _LMARK_, Result.Left);
    except
        if Assigned(FormInst) then begin
            Result.Left := FormInst.Left;
        end else begin
            Result.Left := 0;
        end;
    end;
    //Size;
    try
        FReg.ReadFullInteger(FRegPath + AClassWinName + _WMARK_, Result.Top);
    except
        if Assigned(FormInst) then begin
            Result.Width := FormInst.Width;
        end else begin
            Result.Width := Screen.Width;
        end;
    end;
    try
        FReg.ReadFullInteger(FRegPath + AClassWinName + _HMARK_, Result.Left);
    except
        if Assigned(FormInst) then begin
            Result.Heigth := FormInst.Height;
        end else begin
            Result.Heigth := Screen.Height;
        end;
    end;
    //State
    try
        TmpVal:=integer(wsNormal);
        if (FReg.ReadFullInteger(FRegPath + AClassWinName + _SMARK_, TmpVal)) then begin
            Result.State := TWindowState(TmpVal);
        end;
    except
        if Assigned(FormInst) then begin
            Result.State := FormInst.WindowState;
        end else begin
            Result.State := wsNormal;
        end;
    end;
    //Visibility
    try
        FReg.ReadFullBool(FRegPath + AClassWinName + _VMARK_, Result.Visible);
    except
        if Assigned(FormInst) then begin
            Result.Visible := FormInst.Visible;
        end else begin
            Result.Visible := True;
        end;
    end;
end;

function TWinSaver32.GetWindowProperties(AFormWin : TForm) : TVisualWinProperties;
    {-------------------------------------------------------------------------------------------------------------}
var
    TmpVal : Integer;
begin
    //Location
    try
        FReg.ReadFullInteger(FRegPath + AFormWin.ClassName + _TMARK_, Result.Top);
    except
        Result.Top := AFormWin.Top;
    end;
    try
        FReg.ReadFullInteger(FRegPath + AFormWin.ClassName + _LMARK_, Result.Left);
    except
        Result.Left := AFormWin.Left;
    end;
    //Size;
    try
        FReg.ReadFullInteger(FRegPath + AFormWin.ClassName + _WMARK_, Result.Top);
    except
        Result.Width := AFormWin.Width;
    end;
    try
        FReg.ReadFullInteger(FRegPath + AFormWin.ClassName + _HMARK_, Result.Left);
    except
        Result.Heigth := AFormWin.Height;
    end;
    //State
    try
        TmpVal:=integer( wsNormal );
        if (FReg.ReadFullInteger(FRegPath + AFormWin.ClassName + _SMARK_, TmpVal)) then begin
            Result.State := TWindowState(TmpVal);
        end;
    except
        Result.State := AFormWin.WindowState;
    end;
    //Visibility
    try
        FReg.ReadFullBool(FRegPath + AFormWin.ClassName + _VMARK_, Result.Visible);
    except
        Result.Visible := AFormWin.Visible;
    end;
end;

procedure TWinSaver32.LoadClassNames(List : TStringList);
{-------------------------------------------------------------------------------------------------------------}
var
    i :   Integer;
    Hit : string;
    Ret : TStringList;
begin
    List.Clear;
    List.Sorted := True;
    FReg.GetValueNames(List);
    TStringList(List).Find(_PRE_VRES_ENTRY_, i); //Remove entradas de resolucao antiga
    List.Delete(i);
    TStringList(List).Find(_PRE_HRES_ENTRY_, i);
    List.Delete(i);
    Ret := TStringList.Create;
    Ret.Sorted := True;
    Ret.Duplicates := dupIgnore;
    try
        for i := 0 to List.Count - 1 do begin
            if Pos(_HMARK_, List.Strings[i]) <> 0 then begin //Pode ser repeido para outros delimitadores
                Hit := StrRScanStr(_HMARK_, PChar(List.Strings[i]));
                Ret.Add(Copy(List.Strings[i], 1, Length(List.Strings[i]) - Length(Hit)));
            end;
        end;
    finally
        List.Assign(Ret);
        Ret.Free;
    end;
end;

procedure TWinSaver32.NormalizeScalePos(Resize : boolean);
{-------------------------------------------------------------------------------------------------------------}
var
    {LocalH, LocalW, LocalL, LocalT,} OldW, OldH, OldVal : Integer;
    i :    Integer;
    List : TStringList;
begin
    //Read Previous resolution
    try
        OldH := FReg.ReadInteger(_PRE_HRES_ENTRY_); //Le valor anterior
    except
        OldH := Screen.Height;
    end;
    try
        OldW := FReg.ReadInteger(_PRE_VRES_ENTRY_); //Le valor anterior
    except
        OldW := Screen.Width;
    end;
    //Save Current
    FReg.WriteFullInteger(TFileHnd.ConcatPath([Self.FRegPath, _PRE_VRES_ENTRY_]), Screen.Width, True);
    FReg.WriteFullInteger(TFileHnd.ConcatPath([Self.FRegPath, _PRE_HRES_ENTRY_]), Screen.Height, True);

    //Work changes
    if (OldW <> Screen.Width) or (OldH <> Screen.Height) then begin //Rescale position
        List := TStringList.Create;
        try
            Self.LoadClassNames(List); //Search all entries at key
            for i := 0 to List.Count - 1 do begin
                //Top
                try
                    OldVal := FReg.ReadInteger(List.Strings[i] + _TMARK_);
                    FReg.WriteInteger(List.Strings[i] + _TMARK_, Trunc((OldVal * OldH) / Screen.Height));
                except
                    FReg.DeleteValue(List.Strings[i] + _TMARK_);
                end;
                //Left
                try
                    OldVal := FReg.ReadInteger(List.Strings[i] + _LMARK_);
                    FReg.WriteInteger(List.Strings[i] + _LMARK_, Trunc((OldVal * OldW) / Screen.Width));
                except
                    FReg.DeleteValue(List.Strings[i] + _LMARK_);
                end;
                if Resize then begin
                    //Width
                    try
                        OldVal := FReg.ReadInteger(List.Strings[i] + _WMARK_);
                        FReg.WriteInteger(List.Strings[i] + _WMARK_, Trunc((OldVal * OldW) / Screen.Width));
                    except
                        FReg.DeleteValue(List.Strings[i] + _WMARK_);
                    end;
                    //Height
                    try
                        OldVal := FReg.ReadInteger(List.Strings[i] + _HMARK_);
                        FReg.WriteInteger(List.Strings[i] + _HMARK_, Trunc((OldVal * OldH) / Screen.Width));
                    except
                        FReg.DeleteValue(List.Strings[i] + _HMARK_);
                    end;
                end;
            end;
        finally
            List.Free;
        end;
    end;
end;

procedure TWinSaver32.Restore;
{-------------------------------------------------------------------------------------------------------------}
var
    i : Integer;
begin
    for i := 0 to Application.ComponentCount - 1 do begin
        if (Application.Components[i] is TForm) then begin
            RestoreWindow(TForm(Application.Components[i]));
            if TForm(Application.Components[i]).FormStyle = fsMDIForm then begin
                RestoreChildrens(TForm(Application.Components[i]));
            end;
        end;
    end;
end;

{-------------------------------------------------------------------------------------------------------------}
procedure TWinSaver32.RestoreChildrens(MDIForm : TForm);
var
    i : Integer;
begin
    for i := 0 to MDIForm.MDIChildCount - 1 do begin
        RestoreWindow(MDIForm.MDIChildren[i]);
    end;
end;

{-------------------------------------------------------------------------------------------------------------}
procedure TWinSaver32.RestoreWindow(AFormWin : TForm);
{{
Recupera as informações relativas a AFormWin.

Em 7/8/2006 foram vinculados os valores para os seus respectivos ordinais inteiros evitando a conversão indireta.

Revision: 7/8/2006 - Roger
}
var
    TmpVal : Integer;
    TmpBool : boolean;
    Scr :    TRect;
begin
    //Nota: A janela deve possuir poDesigned em position para os atributos serem repassados corretamente no evento OnShow
    Scr := Screen.WorkAreaRect;

    if FSaveSize then begin
        //Width
        try
            if FReg.ReadFullInteger(FRegPath + AFormWin.ClassName + _WMARK_, TmpVal) then begin
                AFormWin.Width := Min(TmpVal, Scr.Right);
            end;
        except
            FReg.WriteFullInteger(FRegPath + AFormWin.ClassName + _WMARK_, AFormWin.Width, True);
        end;
        //Height
        try
            if FReg.ReadFullInteger(FRegPath + AFormWin.ClassName + _HMARK_, TmpVal) then begin
                AFormWin.Height := Min(TmpVal, Scr.Bottom);
            end;
        except
            FReg.WriteFullInteger(FRegPath + AFormWin.ClassName + _HMARK_, AFormWin.Height, True);
        end;
    end;
    if FSaveLocation then begin
        //Top
        try
            if FReg.ReadFullInteger(FRegPath + AFormWin.ClassName + _TMARK_, TmpVal) then begin
                AFormWin.Top := Min(TmpVal, Scr.Bottom);
            end;
        except
            FReg.WriteFullInteger(FRegPath + AFormWin.ClassName + _TMARK_, AFormWin.Top, True);
        end;
        //Left
        try
            if FReg.ReadFullInteger(FRegPath + AFormWin.ClassName + _LMARK_, TmpVal) then begin
                AFormWin.Left := Min(TmpVal, Scr.Right);
            end;
        except
            FReg.WriteFullInteger(FRegPath + AFormWin.ClassName + _LMARK_, AFormWin.Left, True);
        end;
    end;

    if FSaveState then begin
        try
            TmpVal:=integer( wsNormal );
            if not FReg.ReadFullInteger(FRegPath + AFormWin.ClassName + _SMARK_, TmpVal) then begin
                TmpVal := Integer(wsNormal);
                FReg.WriteFullInteger(FRegPath + AFormWin.ClassName + _SMARK_, TmpVal, True);
            end;
        except
            TmpVal := Integer(wsNormal);
            FReg.WriteFullInteger(FRegPath + AFormWin.ClassName + _SMARK_, TmpVal, True);
        end;
        AFormWin.WindowState := TWindowState(TmpVal);
    end;
    if FSaveVisibility then begin
        try
            if ( Self.FReg.ReadFullBool(FRegPath + AFormWin.ClassName + _VMARK_, TmpBool ) ) then begin
                AFormWin.Visible:=TmpBool;
            end else begin
                FReg.WriteFullBool(FRegPath + AFormWin.ClassName + _VMARK_, AFormWin.Visible, True);
            end;
        except
            FReg.WriteFullBool(FRegPath + AFormWin.ClassName + _VMARK_, AFormWin.Visible, True);
        end;
    end;

end;


{-------------------------------------------------------------------------------------------------------------}
procedure TWinSaver32.Save;
var
    i : Integer;
begin
    try
        for i := 0 to Application.ComponentCount - 1 do begin
            if (Application.Components[i] is TForm) then begin
                SaveWindow(TForm(Application.Components[i]));
                if TForm(Application.Components[i]).FormStyle = fsMDIForm then begin
                    SaveChildrens(TForm(Application.Components[i]));
                end;
            end;
        end;
    finally
        FReg.CloseKey;
    end;
end;

{-------------------------------------------------------------------------------------------------------------}
procedure TWinSaver32.SaveChildrens(MDIForm : TForm);
var
    i : Integer;
begin
    for i := 0 to MDIForm.MDIChildCount - 1 do begin
        SaveWindow(MDIForm.MDIChildren[i]);
    end;
end;

{-------------------------------------------------------------------------------------------------------------}
procedure TWinSaver32.SaveWindow(AFormWin : TForm);
begin
    //Size
    if SaveSize then begin
        FReg.WriteFullInteger(FRegPath + AFormWin.ClassName + _WMARK_, AFormWin.Width, True);
        FReg.WriteFullInteger(FRegPath + AFormWin.ClassName + _HMARK_, AFormWin.Height, True);
    end;
    //Location
    if FSaveLocation then begin
        FReg.WriteFullInteger(FRegPath + AFormWin.ClassName + _TMARK_, AFormWin.Top, True);
        FReg.WriteFullInteger(FRegPath + AFormWin.ClassName + _LMARK_, AFormWin.Left, True);
    end;
    //State
    if FSaveState then begin
        FReg.WriteFullInteger(FRegPath + AFormWin.ClassName + _SMARK_, Integer(AFormWin.WindowState), True);
    end;
    //Visibilty
    if FSaveVisibility then begin
        if AFormWin.Visible then begin
            FReg.WriteFullBool(FRegPath + AFormWin.ClassName + _VMARK_, True, True);
        end else begin
            FReg.WriteFullBool(FRegPath + AFormWin.ClassName + _VMARK_, False, True);
        end;
    end;
end;


procedure TWinSaver32.SetRegPath(const Value : string);
{-------------------------------------------------------------------------------------------------------------}
begin
    if Value[Length(Value)] <> '\' then begin
        FRegPath := Value + '\';
    end else begin
        FRegPath := Value;
    end;
end;



procedure Register;
{-------------------------------------------------------------------------------------------------------------}
begin
    RegisterComponents('Super', [TWinSaver, TWinSaver32]);
end;

end.
