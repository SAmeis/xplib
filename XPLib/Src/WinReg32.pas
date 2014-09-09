{$IFDEF WinReg32}
    {$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I XPLib.inc}

unit WinReg32;

{{
Implements classes for Windows registry manipulation.
}

interface

uses
    Windows, Registry, Classes, RegCLX, XPTypes;

type
    TRegistryNT = class(TCLXRegistry)
    {{
    Subclass of TRegistry that supports utilities methofs and access mode control for Windows Registry.
    }
    private
        procedure AssignValuesToKey(DestKey : HKEY);
        procedure AssignValueToKey(DestKey : HKEY; const Name : string);
        function IsRelative(const Value : string) : boolean;
        function TranslateKey(const KeyName : string) : HKey;
        function GetAccessMode : REGSAM;
        procedure SetAccessMode(Value : REGSAM);
    protected
        function GetKey(const Key : string) : HKEY;
        procedure PutDataXP(const Name : string; Buffer : Pointer; BufSize : Integer; RegDataEx : Integer);
    public
        constructor Create;
        function ClearValues : boolean;
        procedure CloneKey(RegDest : TRegistryNT);
        procedure CopyValues(SourceReg : TRegistryNT);
        function CreateKey(const Key : string) : boolean;
        function DeleteFullKey(const FullKey : string) : boolean;
        function DeleteFullSubKeys(const FullKeyName : string) : boolean;
        function DeleteFullValue(const FullValueName : string) : boolean;
        function FullKeyExists(const FullKey : string) : boolean;
        function FullValueExists(const FullName : string) : boolean;
        procedure GetKeyNames(Strings : TStrings);
        procedure MoveKey(const OldName, NewName : string; Delete : boolean);
        function OpenFullKey(const FullKey : string; CanCreate : boolean) : boolean;
        function OpenKey(const Key : string; CanCreate : boolean) : boolean;
        function ReadFullBinaryData(const FullKeyValueName : string; var Buffer; BufSize : Integer) : Integer;
        function ReadFullBool(const FullKeyValueName : string; var Value : boolean) : boolean;
        function ReadFullDateTime(const FullKeyValueName : string; var Value : TDateTime) : boolean;
        function ReadFullFloat(const FullKeyValueName : string; var Value : double) : boolean;
        function ReadFullInteger(const FullKeyValueName : string; var Value : Integer) : boolean;
        function ReadFullMultiSZ(FullKeyValueName : string; var MultiSZValues : TStringList) : boolean;
        function ReadFullString(const FullKeyValueName : string; var Value : string) : boolean;
        function ReadMultiSZ(const ValueName : string; var MultiSZValues : TStringList) : boolean;
        function WriteMultiSZ(const ValueName : string; MultiSZValues : TStrings) : boolean;
        procedure WriteFullMultiSZ(const FullKeyValueName : string; Value : TStringList; const CanCreate : boolean);
        procedure WriteFullBinaryData(const FullKeyValueName : string; var Buffer; BufSize : Integer);
        procedure WriteFullBool(const FullKeyValueName : string; const Value, CanCreate : boolean);
        procedure WriteFullDateTime(const FullKeyValueName : string; const Value : TDateTime; const CanCreate : boolean);
        procedure WriteFullFloat(const FullKeyValueName : string; Value : double; CanCreate : boolean);
        procedure WriteFullInteger(const FullKeyValueName : string; const Value : Integer; const CanCreate : boolean);
        procedure WriteFullString(const FullKeyValueName, Value : string; const CanCreate : boolean);
        property AccessMode : REGSAM read GetAccessMode write SetAccessMode;
    end;

implementation


uses
    Consts, Str_Pas, FileHnd, SysUtils, IniFiles, Super, Math, RTLConsts;


procedure ReadError(const Name : string);
begin
    raise ERegistryException.CreateFmt(SInvalidRegType, [Name]);
end;


{-**********************************************************************
************************************************************************
******************
******************  Class:    TRegistryNT
******************  Category: No category
******************
************************************************************************
************************************************************************}
{--------------------------------------------------------------------------------------------------------------------------------}
procedure TRegistryNT.AssignValuesToKey(DestKey : HKEY);
{{
Copies atributes to DestKey.

Revision:27/5/2005
}
var
    Len : DWORD;
    I :   Integer;
    KeyInfo : TRegKeyInfo;
    S :   string;
begin
    Self.AccessMode := Self.AccessMode or KEY_READ;
    if GetKeyInfo(KeyInfo) then begin
        Self.AssignValueToKey(DestKey, ''); //Valor "padrao" da chave
        SetString(S, nil, KeyInfo.MaxValueLen + 1);
        for I := 0 to KeyInfo.NumValues - 1 do begin
            Len := KeyInfo.MaxValueLen + 1;
            if RegEnumValue(Self.CurrentKey, I, PChar(S), Len, nil, nil, nil, nil) = ERROR_SUCCESS then begin
                Self.AssignValueToKey(DestKey, PChar(S));
            end;
        end;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TRegistryNT.AssignValueToKey(DestKey : HKEY; const Name : string);
{{
Undocumented method!!!!

Revision:27/5/2005
}
var
    Len :     Integer;
    PrevKey : HKEY;
    Buffer :  PChar;
    RegData : TRegDataType;
begin
    Len := GetDataSize(Name);
    if Len > 0 then begin
        Buffer := AllocMem(Len);
        try
            Len     := GetData(Name, Buffer, Len, RegData);
            PrevKey := CurrentKey;
            SetCurrentKey(DestKey);
            try
                PutData(Name, Buffer, Len, RegData);
            finally
                SetCurrentKey(PrevKey);
            end;
        finally
            FreeMem(Buffer);
        end;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TRegistryNT.IsRelative(const Value : string) : boolean;
{{
Private support method inported from TRegistry.

Revision 27/5/2005 
}
begin
    Result := not ((Value <> EmptyStr) and (Value[1] = '\'));
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TRegistryNT.TranslateKey(const KeyName : string) : HKey;
{{
Get the HKey value for KeyName passed.

KeyName is a string that initiates with HKEY_CLASSES_ROOT, HKEY_CURRENT_USER, HKEY_LOCAL_MACHINE, HKEY_USERS


Revision:27/5/2005
}
var
    Root : string;
begin
    Root := GetDelimitedSubStr('\', KeyName, 0);
    if Root = 'HKEY_CLASSES_ROOT' then begin
        Result := HKEY_CLASSES_ROOT;
    end else begin
        if Root = 'HKEY_CURRENT_USER' then begin
            Result := HKEY_CURRENT_USER;
        end else begin
            if Root = 'HKEY_LOCAL_MACHINE' then begin
                Result := HKEY_LOCAL_MACHINE;
            end else begin
                if Root = 'HKEY_USERS' then begin
                    Result := HKEY_USERS;
                end else begin
                    Result := 0; //hive invalido?????
                end;
            end;
        end;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TRegistryNT.GetAccessMode : REGSAM;
{{
Returns the default AccessMode for this instance.

Revision:27/5/2005
}
begin
    Result := REGSAM(Self.Access);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TRegistryNT.SetAccessMode(Value : REGSAM);
{{
Adjust the access mode for this instance.


Revision:27/5/2005
}
var
    OldKey : string;
begin
    if Value <> Access then begin
        OldKey := Self.CurrentPath;
        Self.CloseKey;
        Access := Value;
        if OldKey <> EmptyStr then begin
            Self.OpenKey(OldKey, False);
        end;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TRegistryNT.GetKey(const Key : string) : HKEY;
{{
Adjust the access mode and calls inherited.

Revision:27/5/2005
}
var
    S : string;
    Relative : boolean;
begin
    S := Key;
    Relative := IsRelative(S);
    if not Relative then begin
        Delete(S, 1, 1);
    end;
    Result := 0;
    RegOpenKeyEx(GetBaseKey(Relative), PChar(S), 0, Self.Access, Result);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TRegistryNT.ClearValues : boolean;
{{
Delete all sub keys from the current key

Returns True if success.

Revision:27/5/2005
}
var
    i :    Integer;
    List : TStringList;
begin
    Result := True;
    Self.AccessMode := Self.Access or KEY_ENUMERATE_SUB_KEYS or KEY_READ;
    List   := TStringList.Create;
    try
        Self.GetValueNames(List);
        for i := 0 to List.Count - 1 do begin
            if not Self.DeleteValue(List.Strings[i]) then begin
                Result := False;
                Abort;
            end;
        end;
    finally
        List.Free;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TRegistryNT.CloneKey(RegDest : TRegistryNT);
{{
Copies recursively all keys, sub keys and values from this instance to RegDest.
raises ERegistryException if fail.

Revision:27/5/2005
}
var
    List : TStringList;
    i :    Integer;
    BaseSrc, BaseDest : string;
begin
    RegDest.CopyValues(Self);
    List := TStringList.Create;
    try
        BaseDest := RegDest.CurrentPath;
        BaseSrc  := Self.CurrentPath;
        Self.GetKeyNames(List);
        try
            for i := 0 to List.Count - 1 do begin
                if Self.OpenKey('\' + TFileHnd.ConcatPath([BaseSrc, List.Strings[i]]), False) then begin
                    if RegDest.OpenKey('\' + TFileHnd.ConcatPath([BaseDest, List.Strings[i]]), True) then begin
                        Self.CloneKey(RegDest);
                    end else begin
                        raise ERegistryException.CreateFmt('Falha criando chave:'#13'"%s"',
                            [TFileHnd.ConcatPath([BaseDest, List.Strings[i]])]);
                    end;
                end else begin
                    raise ERegistryException.CreateFmt('Falha acessando valores para:'#13'"%s"', [BaseSrc]);
                end;
            end;
        finally
            Self.OpenKey('\' + BaseSrc, False);
        end;
    finally
        List.Free;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TRegistryNT.CopyValues(SourceReg : TRegistryNT);
{{
Copies atributes from SourceReg to current key.

Revision:27/5/2005
}
begin
    SourceReg.AssignValuesToKey(Self.CurrentKey);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
constructor TRegistryNT.Create;
{{
Calls super class and adjust acces mode to KEY_ALL_ACCESS. This value can be lower, so each method adjust to the minimum necessary
when called.

Revision 27/5/2005
}
begin
    inherited Create;
    AccessMode := KEY_ALL_ACCESS;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TRegistryNT.CreateKey(const Key : string) : boolean;
{{
Creates a key with the key path relative to the current key

Returns true of success.

Revision:27/5/2005
}
begin
    Self.Access := KEY_WRITE;
    Result      := inherited CreateKey(Key);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TRegistryNT.DeleteFullKey(const FullKey : string) : boolean;
{{
Delete a key specified by FullKey path.

Returns true if success.

Revision:27/5/2005
}
var
    List : TStringList;
    i :    Integer;
    KeyName : string;
begin
    Result := True; //Verdadeiro se a chave ja nao existia
    Self.AccessMode := Self.AccessMode or KEY_WRITE or KEY_ENUMERATE_SUB_KEYS or KEY_READ;
    if Self.OpenFullKey(FullKey, False) then begin
        if Self.HasSubKeys then begin
            List := TStringList.Create;
            Self.GetKeyNames(List);
            for i := 0 to List.Count - 1 do begin
                Result := Result and Self.DeleteFullKey(FullKey + '\' + List.Strings[i]);
                if not Result then begin
                    Break;
                end;
            end;
            List.Free;
        end;
        if Self.ClearValues then begin
            //Elimina todas as entradas
            KeyName := TFileHnd.SlashRem(ExtractFilePath(FullKey)); //Chave pai
            Self.OpenFullKey(KeyName, False);  //Abre chave pai
            KeyName := ExtractFileName(FullKey); //Determina nome da chave desejada
            Result  := Self.DeleteKey(KeyName); //Apaga a chave desejada
        end else begin
            Result := False;
        end;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TRegistryNT.DeleteFullSubKeys(const FullKeyName : string) : boolean;
{{
Delete all sub keys inside FullKeyName.

Returns true if success.

Stability weak for this method.


Revision:27/5/2005
}
var
    i :   Integer;
    Lst : TStringList;
begin
    Self.AccessMode := (KEY_WRITE or KEY_READ);
    Result := Self.OpenFullKey(FullKeyName, False);
    if Result then begin
        Lst := TStringList.Create;
        try
            Self.GetKeyNames(Lst);
            for i := 0 to Lst.Count - 1 do begin
                if not Self.DeleteKey(Lst.Strings[i]) then begin
                    Result := False;
                    Exit;
                end;
            end;
        finally
            Lst.Free;
        end;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TRegistryNT.DeleteFullValue(const FullValueName : string) : boolean;
{{
Delete the value specified by FullValueName.

returns true if success.

Revision:27/5/2005
}
begin
    if Self.OpenFullKey(ExtractFilePath(FullValueName), False) then begin
        Self.AccessMode := KEY_WRITE;
        Result := Self.DeleteValue(ExtractFileName(FullValueName));
    end else begin
        Result := False;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TRegistryNT.FullKeyExists(const FullKey : string) : boolean;
{{
Returns true if the key and valuename exists

Revision:27/5/2005

Fixed a call with wrong parameter to Self.KeyExists with the root hive appended, and avoided changes to Rootkey after call.

Revision: 18/11/2005 - Roger
}
var
    Path :   string;
    OldKey : HKEY;
begin
    OldKey := Self.RootKey;
    try
        Self.RootKey := TranslateKey(FullKey);
        Path   := string(StrScan(PChar(FullKey), '\'));
        Result := Self.KeyExists(Path);
    finally
        Self.RootKey := OldKey;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TRegistryNT.FullValueExists(const FullName : string) : boolean;
{{
True if FullName is a valid entry name.

Revision: 4/8/2006 - Roger  
}
var
    PrevPath, key : string;
    PrevKey : HKEY;
begin
    PrevKey  := Self.CurrentKey;
    PrevPath := Self.CurrentPath;
    try
        Result := False;
        key    := TFileHnd.SlashRem(ExtractFilePath(FullName));
        if (Self.OpenFullKey(key, False)) then begin
            Result := Self.ValueExists(ExtractFileName(FullName));
        end;
    finally
        Self.ChangeKey(PrevKey, PrevPath);
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TRegistryNT.GetKeyNames(Strings : TStrings);
{{
Adjust the access mode and calls inherited.

Revision:27/5/2005
}
begin
    Self.AccessMode := Self.AccessMode or KEY_READ; //Renova acesso a chave corrente
    inherited;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TRegistryNT.MoveKey(const OldName, NewName : string; Delete : boolean);
{{
Moves the OldName to NewName.

Undocumented details.


Revision:27/5/2005
}
var
    DestReg : TRegistryNT;
begin
    { TODO -oRoger -cFUTURE : Filtrar esta chamada burocratica apenas para NT4 }
    DestReg := TRegistryNT.Create;
    try
        DestReg.RootKey := Self.RootKey; //Colocar na documentacao que so funciona na mesma raiz
        DestReg.Access  := KEY_WRITE;
        if IsRelative(NewName) then begin
            DestReg.OpenKey(Self.CurrentPath, False);
        end;
        if DestReg.OpenKey(NewName, True) then begin
            try
                if Self.OpenKey(OldName, False) then begin
                    Self.CloneKey(DestReg);
                end;
            except
                on E : Exception do begin
                    raise ERegistryException.CreateFmt('Falha movendo chave de registro:'#13'"%s"'#13'para'#13'"%s"'#13'%s',
                        [OldName, NewName, E.Message]);
                end;
            end;
            if Delete then begin //eliminar a chave origem
                Self.OpenKey('\' + TFileHnd.SlashRem(ExtractFilePath(Self.CurrentPath)), False);
                if not Self.DeleteKey(OldName) then begin
                    raise ERegistryException.CreateFmt('Falha removendo chave: "%s"', [Self.CurrentPath + '\' + OldName]);
                end;
            end;
        end else begin
            raise ERegistryException.CreateFmt('Falha acessando chave "%s".', [NewName]);
        end;
    finally
        DestReg.Free;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TRegistryNT.OpenFullKey(const FullKey : string; CanCreate : boolean) : boolean;
{{
Opens the key specified by FullKey, if the key dont exists and CanCreate is true the key will be created.

Returns true if success.

Revision:27/5/2005
}
var
    Path : string;
begin
	 Result := False;
    Self.RootKey := TranslateKey(FullKey);
    if Self.RootKey <> 0 then begin
        Path := string(StrScan(PChar(FullKey), '\'));
        if Path <> EmptyStr then begin
            Result := (Self.OpenKey(TFileHnd.SlashRem(Path), CanCreate)); //Evita \ no final do caminho
        end;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TRegistryNT.OpenKey(const Key : string; CanCreate : boolean) : boolean;
{{
Opens the key specified by key argument relative to the current key.

if the key dont exists and CanCreate is true the key is created.

Differs from inherited method by the access mode auto adjusted for operation.

Revision:27/5/2005
}
begin
    Result := inherited OpenKey(Key, CanCreate); //Tenta com o modo corrente
    if not Result then begin  //Falhando tenta novamente com modo mais restritivo
        if CanCreate then begin
            Self.Access := KEY_WRITE;
        end else begin
            Self.Access := KEY_READ;
        end;
        Result := inherited OpenKey(Key, CanCreate);
    end;
end;

procedure TRegistryNT.PutDataXP(const Name : string; Buffer : Pointer; BufSize, RegDataEx : Integer);
{{
 Expoe metodo padrão da API do Windows para escrita no regitro.
 Suplante a da VCL por permitir os demais tipos de dados
}
begin
    if not CheckResult(RegSetValueEx(CurrentKey, PChar(Name), 0, RegDataEx, Buffer, BufSize)) then begin
        raise ERegistryException.CreateResFmt(PResStringRec(@SRegSetDataFailed), [Name]);
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TRegistryNT.ReadFullBinaryData(const FullKeyValueName : string; var Buffer; BufSize : Integer) : Integer;
{{
Acts compounding FullOpenKey and ReadBinaryData with AccessMode to access the registry.

Revision:27/5/2005
}
begin
    Self.Access := KEY_QUERY_VALUE;
    if OpenFullKey(ExtractFilePath(FullKeyValueName), False) then begin
        Result := ReadBinaryData(ExtractFileName(FullKeyValueName), Buffer, BufSize);
    end else begin
        Result := 0;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TRegistryNT.ReadFullBool(const FullKeyValueName : string; var Value : boolean) : boolean;
{{
Acts compounding FullOpenKey and ReadBool with AccessMode to access the registry.

Revision:27/5/2005
}
var
    Entry : string;
begin
    Self.Access := KEY_QUERY_VALUE;
    Result      := False;
    if OpenFullKey(ExtractFilePath(FullKeyValueName), False) then begin
        try
            Entry  := ExtractFileName(FullKeyValueName);
            Value  := ReadBool(Entry);
            Result := True;
        except
            //Retornara false
        end;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TRegistryNT.ReadFullDateTime(const FullKeyValueName : string; var Value : TDateTime) : boolean;
{{
Acts compounding FullOpenKey and ReadDateTime with AccessMode to access the registry.

Revision:27/5/2005
}
begin
    Self.Access := KEY_QUERY_VALUE;
    if OpenFullKey(ExtractFilePath(FullKeyValueName), False) then begin
        try
            Value  := Self.ReadDateTime(ExtractFileName(FullKeyValueName));
            Result := True;
        except
            //Retornara false
            Result := False;
        end;
    end else begin
        Result := False;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TRegistryNT.ReadFullFloat(const FullKeyValueName : string; var Value : double) : boolean;
{{
Acts compounding FullOpenKey and ReadFloat with AccessMode to access the registry.

Revision:27/5/2005
}
var
    Name : string;
begin
    Self.Access := KEY_QUERY_VALUE;
    if OpenFullKey(ExtractFilePath(FullKeyValueName), False) then begin
        Name := ExtractFileName(FullKeyValueName);
        try
            Value  := Self.ReadFloat(Name);
            Result := True;
        except //Retornara false
            Result := False;
        end;
    end else begin
        Result := False;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TRegistryNT.ReadFullInteger(const FullKeyValueName : string; var Value : Integer) : boolean;
{{
Acts compounding FullOpenKey and ReadInteger with AccessMode to access the registry.

Revision:27/5/2005

Fixed a exception raised if FullKeyValueName dont exists.

Revision: 4/8/2006 - Roger
}
var
    Name : string;
begin
    Result      := False;
    Self.Access := KEY_QUERY_VALUE;
    if OpenFullKey(ExtractFilePath(FullKeyValueName), False) then begin
        try
            Name := ExtractFileName(FullKeyValueName);
            if (Self.ValueExists(Name)) then begin
                Value  := Self.ReadInteger(Name);
                Result := True;
            end;
        except
            on E : Exception do begin    //Retornara false
                Result := False;
            end;
        end;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TRegistryNT.ReadFullMultiSZ(FullKeyValueName : string; var MultiSZValues : TStringList) : boolean;
begin
    Self.Access := KEY_QUERY_VALUE;
    if OpenFullKey(ExtractFilePath(FullKeyValueName), False) then begin
        Result := Self.ReadMultiSZ(ExtractFileName(FullKeyValueName), MultiSZValues);
    end else begin
        Result := False;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TRegistryNT.ReadFullString(const FullKeyValueName : string; var Value : string) : boolean;
{{
Acts compounding FullOpenKey and ReadString with AccessMode to access the registry.

Fill Value with the string indicated by FullKeyValueName.
if reads fails(value dont exists by example) returns false.


ALERT: Modifications at 27/5/2005 force the return to false if Value name dont exists, befora true was returned and Value filled
with EmptyStr..

Revision:27/5/2005
}
var
    Name : string;
begin
	 {$IF CompilerVersion >= 22.00}
	 atenção abaixo
	 {$IFEND}
	 Self.Access := KEY_QUERY_VALUE or KEY_WOW64_64KEY; {TODO -oroger -clib : avaliar a inclusão de KEY_WOW64_KEY (para aplicativos 32) ao lado}
	 if OpenFullKey(ExtractFilePath(FullKeyValueName), False) then begin
        Name := ExtractFileName(FullKeyValueName);
        if (Self.ValueExists(Name)) then begin
            Value  := Self.ReadString(Name);
            Result := True;
        end else begin
            Result := False;
        end;
    end else begin
        Result := False;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TRegistryNT.ReadMultiSZ(const ValueName : string; var MultiSZValues : TStringList) : boolean;
var
    MultiSZ, MultiSZOriginal : PChar;
    RegDataInfo :     TRegDataInfo;
    TempResult, tmp : string;
begin
    Result := False;
    if Self.GetDataInfo(ValueName, RegDataInfo) then begin
        MultiSZ := StrAlloc(RegDataInfo.DataSize + 1);
        MultiSZOriginal := MultiSZ;
        try
            ReadBinaryData(ValueName, MultiSZ^, RegDataInfo.DataSize + 1);
            TempResult := EmptyStr;
            repeat
                tmp := string(MultiSZ);
                TempResult := TempResult + tmp + #13#10;
                Inc(MultiSZ, Length(tmp) + 1);
            until (MultiSZ^ = #0);
            if TempResult <> #13#10 then begin
                MultiSZValues.Text := TempResult;
            end;
            Result := True;
        finally
            StrDispose(MultiSZOriginal);
        end;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TRegistryNT.WriteFullBinaryData(const FullKeyValueName : string; var Buffer; BufSize : Integer);
{{
Acts compounding FullOpenKey and WriteBinaryData with AccessMode to access the registry.

Revision:27/5/2005
}
begin
    Self.Access := KEY_WRITE;
    if OpenFullKey(ExtractFilePath(FullKeyValueName), True) then begin
        WriteBinaryData(ExtractFileName(FullKeyValueName), Buffer, BufSize);
    end else begin
        raise ERegistryException.CreateFmt(SRegSetDataFailed, [FullKeyValueName]);
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TRegistryNT.WriteFullBool(const FullKeyValueName : string; const Value, CanCreate : boolean);
{{
Acts compounding FullOpenKey and WriteBool with AccessMode to access the registry.

Revision:27/5/2005
}
begin
    Self.Access := KEY_WRITE;
    if OpenFullKey(ExtractFilePath(FullKeyValueName), CanCreate) then begin
        WriteBool(ExtractFileName(FullKeyValueName), Value);
    end else begin
        raise ERegistryException.CreateFmt(SRegSetDataFailed, [FullKeyValueName]);
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TRegistryNT.WriteFullDateTime(const FullKeyValueName : string; const Value : TDateTime; const CanCreate : boolean);
{{
Acts compounding FullOpenKey and WriteDateTime with AccessMode to access the registry.

Revision:27/5/2005
}
begin
    Self.Access := KEY_WRITE;
    if OpenFullKey(ExtractFilePath(FullKeyValueName), CanCreate) then begin
        Self.WriteDate(ExtractFileName(FullKeyValueName), Value);
    end else begin
        raise ERegistryException.CreateFmt(SRegSetDataFailed, [FullKeyValueName]);
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TRegistryNT.WriteFullFloat(const FullKeyValueName : string; Value : double; CanCreate : boolean);
{{
Acts compounding FullOpenKey and WriteFloat with AccessMode to access the registry.

Revision:27/5/2005
}
begin
    Self.Access := KEY_WRITE;
    if OpenFullKey(ExtractFilePath(FullKeyValueName), CanCreate) then begin
        try
            Self.WriteFloat(ExtractFileName(FullKeyValueName), Value);
        except
            raise;
        end;
    end else begin
        raise ERegistryException.CreateFmt(SRegSetDataFailed, [FullKeyValueName]);
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TRegistryNT.WriteFullInteger(const FullKeyValueName : string; const Value : Integer; const CanCreate : boolean);
{{
Acts compounding FullOpenKey and WriteInteger with AccessMode to access the registry.

Revision:27/5/2005
}
begin
    Self.Access := KEY_WRITE;
    if OpenFullKey(ExtractFilePath(FullKeyValueName), CanCreate) then begin
        Self.WriteInteger(ExtractFileName(FullKeyValueName), Value);
    end else begin
        raise ERegistryException.CreateFmt(SRegSetDataFailed, [FullKeyValueName]);
    end;
end;

procedure TRegistryNT.WriteFullMultiSZ(const FullKeyValueName : string; Value : TStringList; const CanCreate : boolean);
{{ Write a MultiByteString at current hive

Revision - 20120124 - Roger
}
begin
    Self.Access := KEY_WRITE;
    if OpenFullKey(ExtractFilePath(FullKeyValueName), CanCreate) then begin
        Self.WriteMultiSZ(ExtractFileName(FullKeyValueName), Value);
    end else begin
        raise ERegistryException.CreateFmt(SRegSetDataFailed, [FullKeyValueName]);
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TRegistryNT.WriteFullString(const FullKeyValueName, Value : string; const CanCreate : boolean);
{{
Acts compounding FullOpenKey and WriteString with AccessMode to access the registry.

Revision:27/5/2005
}
begin
    Self.Access := KEY_WRITE;
    if OpenFullKey(ExtractFilePath(FullKeyValueName), CanCreate) then begin
        Self.WriteString(ExtractFileName(FullKeyValueName), Value);
    end else begin
        raise ERegistryException.CreateFmt(SRegSetDataFailed, [FullKeyValueName]);
    end;
end;

function TRegistryNT.WriteMultiSZ(const valueName : string; MultiSZValues : TStrings) : boolean;
var
    buffer :  array of Char;
    Line :    String;
    i, size : Integer;
begin
    Result := False;
    size   := 0;
    Line:=EmptyStr;
    //Computa tamanho em caracteres
    for i := 0 to MultiSZValues.Count - 1 do begin
        Line:= MultiSZValues[i];
        size := size + length(Line) + 1;
    end;
    Inc(size);
    SetLength(buffer, size);
    //FillChar(buffer, CharLength(Line, 1)*size, 0);

    //Preenche buffer
    size := 0;
    for i := 0 to MultiSZValues.Count - 1 do begin
        Line := MultiSZValues[i];
        MoveChars(Line[1], buffer[size],Length(Line) );
        Inc(size, Length(Line) + 1);
    end;
    Inc(size);   //Final da cadeia duplo zero
    Self.PutDataXP(ValueName, buffer, size*SizeOf(Char), REG_MULTI_SZ);
end;

end.
