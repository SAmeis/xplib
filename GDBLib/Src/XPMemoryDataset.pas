{$IFDEF XPMemoryDataset}
  {$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I GDBLib.inc}

unit XPMemoryDataset;

{{
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvMemDS.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
$Id: JvMemoryDataset.pas,v 1.21 2004/09/03 16:16:08 ahuser Exp $
}


interface

uses
    SysUtils, Classes, DB, Variants;

type
    //----------------- Added by CFZ 2004/03/03 ------------------
    TPVariant      = ^variant;
    TApplyMode     = (amNone, amAppend, amMerge);
    TRecordStatus  = (rsOriginal, rsUpdated, rsInserted, rsDeleted);
    //------------------------------------------------------------
    TMemBlobData   = string;
    TMemBlobArray  = array [0..0] of TMemBlobData;
    PMemBlobArray  = ^TMemBlobArray;
    TXPMemoryRecord = class;
    TLoadMode      = (lmCopy, lmAppend);
    TSaveLoadState = (slsNone, slsLoading, slsSaving);
    TCompareRecords = function(Item1, Item2 : TXPMemoryRecord) : Integer of object;

    TXPMemoryDataset = class(TDataSet)
    private
        FActive :      boolean;
        FAfterApply :  TDatasetNotifyEvent;
        FApplyMode :   TApplyMode;
        FAutoInc :     longint;
        FAutoIncAsInteger : boolean;
        FAutoIncField : TField;
        FBeforeApply : TDatasetNotifyEvent;
        FBlobOfs :     Integer;
        FBookmarkOfs : Integer;
        FCaseInsensitiveSort : boolean;
        FDataSet :     TDataSet;
        FDeletedValues : TList;
        FDescendingSort : boolean;
        FExactApply :  boolean;
        FIndexList :   TList;
        FInOpen :      boolean;
        FInRefresh :   boolean;
        FKeyFieldNames : string;
        FLastID :      Integer;
        FLoadRecords : boolean;
        FLoadStructure : boolean;
        FOffsets :     PWordArray;
        FRecBufSize :  Integer;
        FRecordPos :   Integer;
        FRecords :     TList;
        FRecordSize :  Integer;
        FRowsAffected : Integer;
        FRowsChanged : Integer;
        FRowsOriginals : Integer;
        FSaveLoadState : TSaveLoadState;
        FSrcAutoIncField : TField;
        FStatusName :  string;
        function AddRecord : TXPMemoryRecord;
        procedure AddStatusField;
        function CalcRecordSize : Integer;
        procedure CheckStructure(UseAutoIncAsInteger : boolean = False);
        procedure ClearRecords;
        function CopyFromDataset : Integer;
        procedure CreateIndexList(const FieldNames : string);
        procedure DoAfterApply;
        procedure DoBeforeApply;
        function FindRecordID(ID : Integer) : TXPMemoryRecord;
        procedure FixReadOnlyFields(MakeReadOnly : boolean);
        procedure FreeIndexList;
        function GetKeyValues : variant;
        procedure HideStatusField;
        procedure InitBufferPointers(GetProps : boolean);
        function InsertRecord(Index : Integer) : TXPMemoryRecord;
        procedure QuickSort(L, R : Integer; Compare : TCompareRecords);
        function RecordFilter : boolean;
        procedure Sort;
        procedure SetApplyMode(Value : TApplyMode);
        function GetCapacity : Integer;
        procedure SetCapacity(Value : Integer);
        procedure SetDataSet(ADataSet : TDataSet);
        procedure SetExactApply(Value : boolean);
        procedure SetLoadRecords(Value : boolean);
        procedure SetLoadStructure(Value : boolean);
        function GetMemoryRecord(Index : Integer) : TXPMemoryRecord;
    protected
        function AllocRecordBuffer : PByte; override;
        procedure AssignMemoryRecord(Rec : TXPMemoryRecord; Buffer : PChar);
        procedure ClearCalcFields(Buffer : TRecordBuffer); override;
        procedure CloseBlob(Field : TField); override;
        function CompareFields(Data1, Data2 : Pointer; FieldType : TFieldType; CaseInsensitive : boolean) : Integer; virtual;
        function CompareRecords(Item1, Item2 : TXPMemoryRecord) : Integer; virtual;
        procedure DataConvert(Field : TField; Source, Dest : Pointer; ToNative : boolean); override;
        function FindFieldData(Buffer : Pointer; Field : TField) : Pointer;
        procedure FreeRecordBuffer(var Buffer : TRecordBuffer); override;
        function GetActiveRecBuf(var RecBuf : PChar) : boolean; virtual;
        function GetBlobData(Field : TField; Buffer : PChar) : TMemBlobData;
        procedure GetBookmarkData(Buffer : TRecordBuffer; Data : Pointer); override;
        function GetBookmarkFlag(Buffer : TRecordBuffer) : TBookmarkFlag; override;
        function GetIsIndexField(Field : TField) : boolean; override;
        function GetRecNo : Integer; override;
        function GetRecord(Buffer : TRecordBuffer; GetMode : TGetMode; DoCheck : boolean) : TGetResult; override;
        function GetRecordCount : Integer; override;
        function GetRecordSize : Word; override;
        procedure InitFieldDefsFromFields;
        procedure InitRecord(Buffer : TRecordBuffer); override;
        procedure InternalAddRecord(Buffer : Pointer; Append : boolean); override;
        procedure InternalClose; override;
        procedure InternalDelete; override;
        procedure InternalFirst; override;
        procedure InternalGotoBookmark(Bookmark : Pointer); override;
        procedure InternalHandleException; override;
        procedure InternalInitFieldDefs; override;
        procedure InternalInitRecord(Buffer : TRecordBuffer); override;
        procedure InternalLast; override;
        procedure InternalOpen; override;
        procedure InternalPost; override;
        procedure InternalSetToRecord(Buffer : TRecordBuffer); override;
        function IsCursorOpen : boolean; override;
        procedure OpenCursor(InfoQuery : boolean); override;
        procedure RecordToBuffer(Rec : TXPMemoryRecord; Buffer : PChar);
        procedure SetAutoIncFields(Buffer : PChar); virtual;
        procedure SetBlobData(Field : TField; Buffer : PChar; Value : TMemBlobData);
        procedure SetBookmarkData(Buffer : TRecordBuffer; Data : Pointer); override;
        procedure SetBookmarkFlag(Buffer : TRecordBuffer; Value : TBookmarkFlag); override;
        procedure SetFieldData(Field : TField; Buffer : Pointer); override;
        procedure SetFiltered(Value : boolean); override;
        procedure SetMemoryRecordData(Buffer : PChar; Pos : Integer); virtual;
        procedure SetOnFilterRecord(const Value : TFilterRecordEvent); override;
        procedure SetRecNo(Value : Integer); override;
        property Records[Index : Integer] : TXPMemoryRecord read GetMemoryRecord;
    public
        constructor Create(AOwner : TComponent); override;
        destructor Destroy; override;
        function ApplyChanges : boolean;
        function BookmarkValid(Bookmark : TBookmark) : boolean; override;
        procedure CancelChanges;
        procedure ClearChanges;
        function CompareBookmarks(Bookmark1, Bookmark2 : TBookmark) : Integer; override;
        procedure CopyStructure(Source : TDataSet; UseAutoIncAsInteger : boolean = False);
        function CreateBlobStream(Field : TField; Mode : TBlobStreamMode) : TStream; override;
        procedure EmptyTable;
        function FindDeleted(KeyValues : variant) : Integer;
        function GetCurrentRecord(Buffer : TRecordBuffer) : boolean; override;
        function GetFieldData(Field : TField; Buffer : Pointer) : boolean; override;
        function IsDeleted(out Index : Integer) : boolean;
        function IsInserted : boolean;
        function IsOriginal : boolean;
        function IsSequenced : boolean; override;
        function IsUpdated : boolean;
        function LoadFromDataSet(Source : TDataSet; RecordCount : Integer; Mode : TLoadMode; DisableAllControls : boolean = True) :
            Integer;
        function Locate(const KeyFields : string; const KeyValues : variant; Options : TLocateOptions) : boolean; override;
        procedure Open; reintroduce;
        function SaveToDataSet(Dest : TDataSet; RecordCount : Integer; DisableAllControls : boolean = True) : Integer;
        procedure SortOnFields(const FieldNames : string; CaseInsensitive : boolean = True; Descending : boolean = False);
        property RowsAffected : Integer read FRowsAffected;
        property RowsChanged : Integer read FRowsChanged;
        property RowsOriginals : Integer read FRowsOriginals;
        property SaveLoadState : TSaveLoadState read FSaveLoadState;
    published
        property Active;
        property AfterApply : TDatasetNotifyEvent read FAfterApply write FAfterApply;
        property AfterCancel;
        property AfterClose;
        property AfterDelete;
        property AfterEdit;
        property AfterInsert;
        property AfterOpen;
        property AfterPost;
        property AfterScroll;
        property ApplyMode : TApplyMode read FApplyMode write SetApplyMode default amNone;
        property AutoCalcFields;
        property AutoIncAsInteger : boolean read FAutoIncAsInteger write FAutoIncAsInteger default False;
        property BeforeApply : TDatasetNotifyEvent read FBeforeApply write FBeforeApply;
        property BeforeCancel;
        property BeforeClose;
        property BeforeDelete;
        property BeforeEdit;
        property BeforeInsert;
        property BeforeOpen;
        property BeforePost;
        property BeforeScroll;
        property Capacity : Integer read GetCapacity write SetCapacity default 0;
        property DataSet : TDataSet read FDataSet write SetDataSet;
        property ExactApply : boolean read FExactApply write SetExactApply default False;
        property FieldDefs;
        property Filtered;
        property KeyFieldNames : string read FKeyFieldNames write FKeyFieldNames;
        property LoadRecords : boolean read FLoadRecords write SetLoadRecords default False;
        property LoadStructure : boolean read FLoadStructure write SetLoadStructure default False;
        property ObjectView default False;
        property OnCalcFields;
        property OnDeleteError;
        property OnEditError;
        property OnFilterRecord;
        property OnNewRecord;
        property OnPostError;
    end;

    TXPMemBlobStream = class(TStream)
    private
        FBuffer :   PChar;
        FCached :   boolean;
        FDataSet :  TXPMemoryDataset;
        FField :    TBlobField;
        FMode :     TBlobStreamMode;
        FModified : boolean;
        FOpened :   boolean;
        FPosition : longint;
        function GetBlobFromRecord(Field : TField) : TMemBlobData;
        function GetBlobSize : longint;
    public
        constructor Create(Field : TBlobField; Mode : TBlobStreamMode);
        destructor Destroy; override;
        function Read(var Buffer; Count : longint) : longint; override;
        function Seek(Offset : longint; Origin : Word) : longint; override;
        procedure Truncate;
        function Write(const Buffer; Count : longint) : longint; override;
    end;

    TXPMemoryRecord = class(TPersistent)
    private
        FBlobs : Pointer;
        FData :  Pointer;
        FID :    Integer;
        FMemoryData : TXPMemoryDataset;
        procedure SetMemoryData(Value : TXPMemoryDataset; UpdateParent : boolean);
        function GetIndex : Integer;
    protected
        procedure SetIndex(Value : Integer); virtual;
    public
        constructor Create(MemoryData : TXPMemoryDataset); virtual;
        constructor CreateEx(MemoryData : TXPMemoryDataset; UpdateParent : boolean); virtual;
        destructor Destroy; override;
        property Data : Pointer read FData;
        property ID : Integer read FID write FID;
        property Index : Integer read GetIndex write SetIndex;
        property MemoryData : TXPMemoryDataset read FMemoryData;
    end;

implementation

uses
    Forms, Dialogs, DBConsts, Math, GDBHnd, SqlTimSt;

resourcestring
    RsEMemNoRecords   = 'Dados não encontrados';
    // Added by CFZ 2004/03/03
    // 'Los registros aplicados, difieren de los cambiados.';
    //SNoExactApply = 'The applied records differs from the changed records.';
    // 'Record already exists.Registro ya existente.';
    SRecordDuplicate  = 'Registro já existe';
    // 'Registro no encontrado.';
    SRecordInexistent = 'Registro não encontrado';
    // 'No se pudo agregar el registro.';
    SInsertError      = 'Impossível acrescentar o registro';
    // 'No se pudo modificar el registro.';
    SUpdateError      = 'Impossível modificar o registro';
    // 'No se pudo eliminar el registro.';
    SDeleteError      = 'Impossível apagar o registro';


const
    ftBlobTypes = [ftBlob, ftMemo, ftGraphic, ftFmtMemo, ftParadoxOle, ftDBaseOle, ftTypedBinary, ftOraBlob, ftOraClob];

    ftSupported = [ftString, ftSmallint, ftInteger, ftWord, ftBoolean, ftFloat, ftCurrency, ftDate, ftTime, ftDateTime, ftAutoInc,
        ftBCD, ftBytes, ftVarBytes, ftADT, ftFixedChar, ftWideString, ftLargeint, ftVariant, ftGuid, ftTimeStamp ] + ftBlobTypes;

    fkStoredFields = [fkData];

    GuidSize = 38;

    //-------- Added by CFZ 2004/03/03 ---------
    STATUSNAME = 'C67F70Z90';

 (* Magic *)
 //------------------------------------------

{ Utility routines }

function CalcFieldLen(FieldType : TFieldType; Size : Word) : Word;
{{
Calcula o espaço em bytes ocupados pelo campo passado.
Size = Valor em bytes para os tipos de comprimento variavel.

Revision: 5/12/2006 - Roger

Inserido suporte ao tipo ftTimeStamp

Revision: 5/12/2006 - Roger
}
begin
    if not (FieldType in ftSupported) then begin
        Result := 0;
    end else begin
        if FieldType in ftBlobTypes then begin
            Result := SizeOf(longint);
        end else begin
            Result := Size;
            case FieldType of
                ftString : begin
                    Inc(Result);
                end;
                ftSmallint : begin
                    Result := SizeOf(smallint);
                end;
                ftInteger : begin
                    Result := SizeOf(longint);
                end;
                ftWord : begin
                    Result := SizeOf(Word);
                end;
                ftBoolean : begin
                    Result := SizeOf(wordbool);
                end;
                ftFloat : begin
                    Result := SizeOf(double);
                end;
                ftCurrency : begin
                    Result := SizeOf(double);
                end;
                ftBCD : begin
                    Result := 34;
                end;
                ftDate, ftTime : begin
                    Result := SizeOf(longint);
                end;
                ftDateTime : begin
                    Result := SizeOf(TDateTime);
                end;
                ftBytes : begin
                    Result := Size;
                end;
                ftVarBytes : begin
                    Result := Size + 2;
                end;
                ftAutoInc : begin
                    Result := SizeOf(longint);
                end;
                ftADT : begin
                    Result := 0;
                end;
                ftFixedChar : begin
                    Inc(Result);
                end;
                ftWideString : begin
                    Result := (Result + 1) * 2;
                end;
                ftLargeint : begin
                    Result := SizeOf(int64);
                end;
                ftVariant : begin
                    Result := SizeOf(variant);
                end;
                ftGuid : begin
                    Result := GuidSize + 1;
                end;
                ftTimeStamp : begin
                    Result := SizeOf(TSQLTimeStamp);
                end;
                else begin
                    DatabaseError('Tipo de campo não pode ter seu comprimento calculado.');
                end;
            end;
        end;
    end;
end;

procedure CalcDataSize(FieldDef : TFieldDef; var DataSize : Integer);
var
    I : Integer;
begin
    if FieldDef.DataType in ftSupported - ftBlobTypes then begin
        Inc(DataSize, CalcFieldLen(FieldDef.DataType, FieldDef.Size) + 1);
    end;
    for I := 0 to FieldDef.ChildDefs.Count - 1 do begin
        CalcDataSize(FieldDef.ChildDefs[I], DataSize);
    end;
end;

procedure Error(const Msg : string);
begin
    DatabaseError(Msg);
end;

procedure ErrorFmt(const Msg : string; const Args : array of const);
begin
    DatabaseErrorFmt(Msg, Args);
end;

type
    TBookmarkData      = Integer;
    PXPMemBookmarkInfo = ^TXPMemBookmarkInfo;

    TXPMemBookmarkInfo = record
        BookmarkData: TBookmarkData;
        BookmarkFlag: TBookmarkFlag;
    end;

{-**********************************************************************
************************************************************************
******************
******************  Class:    TXPMemoryRecord
******************  Category: No category
******************
************************************************************************
************************************************************************}
{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPMemoryRecord.SetMemoryData(Value : TXPMemoryDataset; UpdateParent : boolean);
var
    I : Integer;
    DataSize : Integer;
begin
    if FMemoryData <> Value then begin
        if FMemoryData <> nil then begin
            FMemoryData.FRecords.Remove(Self);
            if FMemoryData.BlobFieldCount > 0 then begin
                Finalize(PMemBlobArray(FBlobs)[0], FMemoryData.BlobFieldCount);
            end;
            ReallocMem(FBlobs, 0);
            ReallocMem(FData, 0);
            FMemoryData := nil;
        end;
        if Value <> nil then begin
            if UpdateParent then begin
                Value.FRecords.Add(Self);
                Inc(Value.FLastID);
                FID := Value.FLastID;
            end;
            FMemoryData := Value;
            if Value.BlobFieldCount > 0 then begin
                ReallocMem(FBlobs, Value.BlobFieldCount * SizeOf(Pointer));
                Initialize(PMemBlobArray(FBlobs)[0], Value.BlobFieldCount);
            end;
            DataSize := 0;
            for I := 0 to Value.FieldDefs.Count - 1 do begin
                CalcDataSize(Value.FieldDefs[I], DataSize);
            end;
            ReallocMem(FData, DataSize);
        end;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TXPMemoryRecord.GetIndex : Integer;
begin
    if FMemoryData <> nil then begin
        Result := FMemoryData.FRecords.IndexOf(Self);
    end else begin
        Result := -1;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPMemoryRecord.SetIndex(Value : Integer);
var
    CurIndex : Integer;
begin
    CurIndex := GetIndex;
    if (CurIndex >= 0) and (CurIndex <> Value) then begin
        FMemoryData.FRecords.Move(CurIndex, Value);
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
constructor TXPMemoryRecord.Create(MemoryData : TXPMemoryDataset);
begin
    CreateEx(MemoryData, True);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
constructor TXPMemoryRecord.CreateEx(MemoryData : TXPMemoryDataset; UpdateParent : boolean);
begin
    inherited Create;
    SetMemoryData(MemoryData, UpdateParent);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
destructor TXPMemoryRecord.Destroy;
begin
    SetMemoryData(nil, True);
    inherited Destroy;
end;

{-**********************************************************************
************************************************************************
******************
******************  Class:    TXPMemoryDataset
******************  Category: No category
******************
************************************************************************
************************************************************************}
{--------------------------------------------------------------------------------------------------------------------------------}
function TXPMemoryDataset.AddRecord : TXPMemoryRecord;
begin
    Result := TXPMemoryRecord.Create(Self);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPMemoryDataset.AddStatusField;
begin
    FieldDefs.Add(FStatusName, ftSmallint);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TXPMemoryDataset.CalcRecordSize : Integer;
var
    I : Integer;
begin
    Result := 0;
    for I := 0 to FieldDefs.Count - 1 do begin
        CalcDataSize(FieldDefs[I], Result);
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPMemoryDataset.CheckStructure(UseAutoIncAsInteger : boolean = False);
var
    I : Integer;

    procedure CheckDataTypes(FieldDefs : TFieldDefs);
    var
        J : Integer;
    begin
        for J := FieldDefs.Count - 1 downto 0 do begin
            if (FieldDefs.Items[J].DataType = ftAutoInc) and UseAutoIncAsInteger then begin
                FieldDefs.Items[J].DataType := ftInteger;
            end;
            if not (FieldDefs.Items[J].DataType in ftSupported) then begin
                FieldDefs.Items[J].Free;
            end;
        end;
    end;

begin
    CheckDataTypes(FieldDefs);
    for I := 0 to FieldDefs.Count - 1 do begin
        if (csDesigning in ComponentState) and (Owner <> nil) then begin
            FieldDefs.Items[I].CreateField(Owner);
        end else begin
            FieldDefs.Items[I].CreateField(Self);
        end;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPMemoryDataset.ClearRecords;
begin
    while FRecords.Count > 0 do begin
        TObject(FRecords.Last).Free;
    end;
    FLastID    := Low(Integer);
    FRecordPos := -1;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TXPMemoryDataset.CopyFromDataset : Integer;
var
    bOpen :  boolean;
    I, Len : Integer;
    FOriginal, FClient : TField;
begin
    Result := 0;
    if FDataSet = nil then begin
        Exit;
    end;
    if FApplyMode <> amNone then begin
        Len := FieldDefs.Count - 2;
    end else begin
        Len := FieldDefs.Count - 1;
    end;
    if Len < 1 then begin
        Exit;
    end;
    bOpen := FDataSet.Active;
    try
        if not bOpen then begin
            FDataSet.Open;
        end;
    except
        Exit;
    end;
    if FDataSet.IsEmpty then begin
        if not bOpen then begin
            FDataSet.Close;
        end;
        Exit;
    end;

    FDataSet.DisableControls;
    DisableControls;
    try
        FDataSet.First;
        while not FDataSet.EOF do begin
            Append;
            for I := 0 to Len do begin
                FClient   := Fields[I];
                FOriginal := FDataSet.FindField(FClient.FieldName);
                if (FClient <> nil) and (FOriginal <> nil) then begin
                    if FOriginal.IsNull then begin
                        Fields[I].Clear;
                    end else begin
                        Fields[I].Value := FOriginal.Value;
                    end;
                end;
            end;
            FieldByName(FStatusName).AsInteger := Integer(rsOriginal);
            Post;
            Inc(Result);
            FDataSet.Next;
        end;
    finally
        EnableControls;
        FDataSet.EnableControls;
        if not bOpen then begin
            FDataSet.Close;
        end;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPMemoryDataset.CreateIndexList(const FieldNames : string);
var
    Pos : Integer;
    F :   TField;
begin
    if FIndexList = nil then begin
        FIndexList := TList.Create;
    end else begin
        FIndexList.Clear;
    end;
    Pos := 1;
    while Pos <= Length(FieldNames) do begin
        F := FieldByName(ExtractFieldName(FieldNames, Pos));
        if (F.FieldKind = fkData) and
            (F.DataType in ftSupported - ftBlobTypes) then begin
            FIndexList.Add(F);
        end else begin
            ErrorFmt(SFieldTypeMismatch, [F.DisplayName]);
        end;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPMemoryDataset.DoAfterApply;
begin
    if Assigned(FAfterApply) then begin
        FAfterApply(Self);
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPMemoryDataset.DoBeforeApply;
begin
    if Assigned(FBeforeApply) then begin
        FBeforeApply(Self);
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TXPMemoryDataset.FindRecordID(ID : Integer) : TXPMemoryRecord;
var
    I : Integer;
begin
    for I := 0 to FRecords.Count - 1 do begin
        Result := TXPMemoryRecord(FRecords[I]);
        if Result.ID = ID then begin
            Exit;
        end;
    end;
    Result := nil;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPMemoryDataset.FixReadOnlyFields(MakeReadOnly : boolean);
var
    I : Integer;
begin
    if MakeReadOnly then begin
        for I := 0 to FieldCount - 1 do begin
            Fields[I].ReadOnly := (Fields[I].Tag = 1);
        end;
    end else begin
        for I := 0 to FieldCount - 1 do begin
            Fields[I].Tag      := Ord(Fields[I].ReadOnly);
            Fields[I].ReadOnly := False;
            if Fields[I].DataType = ftAutoInc then begin
                FAutoIncField := Fields[I];
            end;
        end;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPMemoryDataset.FreeIndexList;
begin
    FIndexList.Free;
    FIndexList := nil;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TXPMemoryDataset.GetKeyValues : variant;
var
    I :    Integer;
    List : TStrings;

    function FldNamesToStrList : TStrings;
    var
        InStr, SubStr : string;
        I, Len : Integer;
    begin
        Result := TStringList.Create;
        Len    := Length(FKeyFieldNames);
        InStr  := FKeyFieldNames;
        SubStr := '';
        I      := 1;
        while (I <= Len) do begin
            if (InStr[I] = ';') or (I = Len) then begin
                if (I = Len) and not (InStr[I] = ';') then begin
                    SubStr := SubStr + InStr[I];
                end;
                Result.Add(SubStr);
                SubStr := '';
            end else begin
                SubStr := SubStr + InStr[I];
            end;
            Inc(I);
        end;
    end;

begin
    Result := Null;
    List   := FldNamesToStrList;
    try
        I      := List.Count;
        Result := VarArrayCreate([0, I], varVariant);
        for I := 0 to List.Count - 1 do begin
            Result[I] := FieldValues[List[I]];
        end;
    finally
        FreeAndNil(List);
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPMemoryDataset.HideStatusField;
begin
    FieldDefs[FieldDefs.Count - 1].Attributes := [faHiddenCol];
    Fields[Fields.Count - 1].Visible := False;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPMemoryDataset.InitBufferPointers(GetProps : boolean);
begin
    if GetProps then begin
        FRecordSize := CalcRecordSize;
    end;
    FBookmarkOfs := FRecordSize + CalcFieldsSize;
    FBlobOfs     := FBookmarkOfs + SizeOf(TXPMemBookmarkInfo);
    FRecBufSize  := FBlobOfs + BlobFieldCount * SizeOf(Pointer);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TXPMemoryDataset.InsertRecord(Index : Integer) : TXPMemoryRecord;
begin
    Result := AddRecord;
    Result.Index := Index;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPMemoryDataset.QuickSort(L, R : Integer; Compare : TCompareRecords);
var
    I, J : Integer;
    P :    TXPMemoryRecord;
begin
    repeat
        I := L;
        J := R;
        P := Records[(L + R) shr 1];
        repeat
            while Compare(Records[I], P) < 0 do begin
                Inc(I);
            end;
            while Compare(Records[J], P) > 0 do begin
                Dec(J);
            end;
            if I <= J then begin
                FRecords.Exchange(I, J);
                Inc(I);
                Dec(J);
            end;
        until I > J;
        if L < J then begin
            QuickSort(L, J, Compare);
        end;
        L := I;
    until I >= R;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TXPMemoryDataset.RecordFilter : boolean;
var
    SaveState : TDataSetState;
begin
    Result := True;
    if Assigned(OnFilterRecord) then begin
        if (FRecordPos >= 0) and (FRecordPos < RecordCount) then begin
            SaveState := SetTempState(dsFilter);
            try
                RecordToBuffer(Records[FRecordPos], TempBuffer);
                OnFilterRecord(Self, Result);
            except
                Application.HandleException(Self);
            end;
            RestoreState(SaveState);
        end else begin
            Result := False;
        end;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPMemoryDataset.Sort;
var
    Pos : TBookmarkStr;
begin
    if Active and (FRecords <> nil) and (FRecords.Count > 0) then begin
        Pos := Bookmark;
        try
            QuickSort(0, FRecords.Count - 1, CompareRecords);
            SetBufListSize(0);
            InitBufferPointers(False);
            try
                SetBufListSize(BufferCount + 1);
            except
                SetState(dsInactive);
                CloseCursor;
                raise;
            end;
        finally
            Bookmark := Pos;
        end;
        Resync([]);
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPMemoryDataset.SetApplyMode(Value : TApplyMode);
begin
    if (csDesigning in ComponentState) and (FDataSet = nil) then begin
        FApplyMode := amNone;
    end else begin
        FApplyMode := Value;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TXPMemoryDataset.GetCapacity : Integer;
begin
    if FRecords <> nil then begin
        Result := FRecords.Capacity;
    end else begin
        Result := 0;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPMemoryDataset.SetCapacity(Value : Integer);
begin
    if FRecords <> nil then begin
        FRecords.Capacity := Value;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPMemoryDataset.SetDataSet(ADataSet : TDataSet);
begin
    FDataSet := ADataSet;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPMemoryDataset.SetExactApply(Value : boolean);
begin
    if (csDesigning in ComponentState) and (FDataSet = nil) then begin
        FExactApply := False;
    end else begin
        FExactApply := Value;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPMemoryDataset.SetLoadRecords(Value : boolean);
begin
    if (csDesigning in ComponentState) and (FDataSet = nil) then begin
        FLoadRecords := False;
    end else begin
        FLoadRecords := Value;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPMemoryDataset.SetLoadStructure(Value : boolean);
begin
    if (csDesigning in ComponentState) and (FDataSet = nil) then begin
        FLoadStructure := False;
    end else begin
        FLoadStructure := Value;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TXPMemoryDataset.GetMemoryRecord(Index : Integer) : TXPMemoryRecord;
begin
    Result := TXPMemoryRecord(FRecords[Index]);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TXPMemoryDataset.AllocRecordBuffer : PByte;
begin
    Result := StrAlloc(FRecBufSize);
    if BlobFieldCount > 0 then begin
        Initialize(PMemBlobArray(Result + FBlobOfs)[0], BlobFieldCount);
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPMemoryDataset.AssignMemoryRecord(Rec : TXPMemoryRecord; Buffer : PChar);
var
    I : Integer;
begin
    Move(Buffer^, Rec.Data^, FRecordSize);
    for I := 0 to BlobFieldCount - 1 do begin
        PMemBlobArray(Rec.FBlobs)[I] := PMemBlobArray(Buffer + FBlobOfs)[I];
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPMemoryDataset.ClearCalcFields(Buffer : TRecordBuffer);
begin
		FillChar(Buffer[FRecordSize], CalcFieldsSize, 0);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPMemoryDataset.CloseBlob(Field : TField);
begin
    if (FRecordPos >= 0) and (FRecordPos < FRecords.Count) and (State = dsEdit) then begin
        PMemBlobArray(ActiveBuffer + FBlobOfs)[Field.Offset] :=
            PMemBlobArray(Records[FRecordPos].FBlobs)[Field.Offset];
    end else begin
        PMemBlobArray(ActiveBuffer + FBlobOfs)[Field.Offset] := '';
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TXPMemoryDataset.CompareFields(Data1, Data2 : Pointer; FieldType : TFieldType; CaseInsensitive : boolean) : Integer;
{{
Compara valores de campos para fins de ordenação.

Revision: 5/12/2006 - Roger

Inserido suporte ao tipo ftTimeStamp.

}
begin
    Result := 0;
    case FieldType of
        ftString : begin
            if CaseInsensitive then begin
                Result := AnsiCompareText(PChar(Data1), PChar(Data2));
            end else begin
                Result := AnsiCompareStr(PChar(Data1), PChar(Data2));
            end;
        end;
        ftSmallint : begin
            if smallint(Data1^) > smallint(Data2^) then begin
                Result := 1;
            end else if smallint(Data1^) < smallint(Data2^) then begin
                Result := -1;
            end;
        end;
        ftInteger, ftDate, ftTime, ftAutoInc : begin
            if longint(Data1^) > longint(Data2^) then begin
                Result := 1;
            end else if longint(Data1^) < longint(Data2^) then begin
                Result := -1;
            end;
        end;
        ftWord : begin
            if Word(Data1^) > Word(Data2^) then begin
                Result := 1;
            end else if Word(Data1^) < Word(Data2^) then begin
                Result := -1;
            end;
        end;
        ftBoolean : begin
            if wordbool(Data1^) and not wordbool(Data2^) then begin
                Result := 1;
            end else if not wordbool(Data1^) and wordbool(Data2^) then begin
                Result := -1;
            end;
        end;
        ftFloat, ftCurrency : begin
            if double(Data1^) > double(Data2^) then begin
                Result := 1;
            end else if double(Data1^) < double(Data2^) then begin
                Result := -1;
            end;
        end;
        ftDateTime : begin
            if TDateTime(Data1^) > TDateTime(Data2^) then begin
                Result := 1;
            end else if TDateTime(Data1^) < TDateTime(Data2^) then begin
                Result := -1;
            end;
        end;
        ftFixedChar : begin
            if CaseInsensitive then begin
                Result := AnsiCompareText(PChar(Data1), PChar(Data2));
            end else begin
                Result := AnsiCompareStr(PChar(Data1), PChar(Data2));
            end;
        end;
        ftWideString : begin
            if CaseInsensitive then begin
                Result := AnsiCompareText(WideCharToString(PWideChar(Data1)),
                    WideCharToString(PWideChar(Data2)));
            end else begin
                Result := AnsiCompareStr(WideCharToString(PWideChar(Data1)),
                    WideCharToString(PWideChar(Data2)));
            end;
        end;
        ftLargeint : begin
            if int64(Data1^) > int64(Data2^) then begin
                Result := 1;
            end else if int64(Data1^) < int64(Data2^) then begin
                Result := -1;
            end;
        end;
        ftVariant : begin
            Result := 0;
        end;
        ftGuid : begin
            Result := CompareText(PChar(Data1), PChar(Data2));
        end;
        ftTimeStamp : begin
            if( SQLTimeStampToDateTime(PSQLTimeStamp(Data1)^) = SQLTimeStampToDateTime(PSQLTimeStamp(Data2)^) )then begin
                Result:=0;
            end else begin
                if( SQLTimeStampToDateTime(PSQLTimeStamp(Data1)^) > SQLTimeStampToDateTime(PSQLTimeStamp(Data2)^))then begin
                    Result:=1;
                end else begin
                    Result:=-1;
                end;
            end;
        end;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TXPMemoryDataset.CompareRecords(Item1, Item2 : TXPMemoryRecord) : Integer;
var
    Data1, Data2 : PChar;
    F : TField;
    I : Integer;
begin
    Result := 0;
    if FIndexList <> nil then begin
        for I := 0 to FIndexList.Count - 1 do begin
            F     := TField(FIndexList[I]);
            Data1 := FindFieldData(Item1.Data, F);
            if Data1 <> nil then begin
                Data2 := FindFieldData(Item2.Data, F);
                if Data2 <> nil then begin
                    if (Data1[0] <> #0) and (Data2[0] <> #0) then begin
                        Inc(Data1);
                        Inc(Data2);
                        Result := CompareFields(Data1, Data2, F.DataType,
                            FCaseInsensitiveSort);
                    end else if Data1[0] <> #0 then begin
                        Result := 1;
                    end else if Data2[0] <> #0 then begin
                        Result := -1;
                    end;
                    if FDescendingSort then begin
                        Result := -Result;
                    end;
                end;
            end;
            if Result <> 0 then begin
                Exit;
            end;
        end;
    end;
    if Result = 0 then begin
        if Item1.ID > Item2.ID then begin
            Result := 1;
        end else if Item1.ID < Item2.ID then begin
            Result := -1;
        end;
        if FDescendingSort then begin
            Result := -Result;
        end;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPMemoryDataset.DataConvert(Field : TField; Source, Dest : Pointer; ToNative : boolean);
begin
    if Field.DataType = ftWideString then begin
        if ToNative then begin
            Word(Dest^) := Length(PWideString(Source)^) * 2;
            Move(PWideChar(Source^)^, (PWideChar(Dest) + 1)^, Word(Dest^));
        end else begin
            SetString(WideString(Dest^), PWideChar(PChar(Source) + 2), Word(Source^) div 2);
        end;
    end else begin
        inherited DataConvert(Field, Source, Dest, ToNative);
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TXPMemoryDataset.FindFieldData(Buffer : Pointer; Field : TField) : Pointer;
var
    Index :    Integer;
    DataType : TFieldType;
begin
    Result := nil;
    Index  := FieldDefList.IndexOf(Field.FullName);
    if (Index >= 0) and (Buffer <> nil) then begin
        DataType := FieldDefList[Index].DataType;
        if DataType in ftSupported then begin
            if DataType in ftBlobTypes then begin
                Result := Pointer(GetBlobData(Field, Buffer));
            end else begin
                Result := (PChar(Buffer) + FOffsets[Index]);
            end;
        end;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPMemoryDataset.FreeRecordBuffer(var Buffer : TRecordBuffer);
begin
		if BlobFieldCount > 0 then begin
        Finalize(PMemBlobArray(Buffer + FBlobOfs)[0], BlobFieldCount);
    end;
    StrDispose(Buffer);
    Buffer := nil;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TXPMemoryDataset.GetActiveRecBuf(var RecBuf : PChar) : boolean;
begin
    case State of
        dsBrowse : begin
            if IsEmpty then begin
                RecBuf := nil;
            end else begin
                RecBuf := ActiveBuffer;
            end;
        end;
        dsEdit, dsInsert : begin
            RecBuf := ActiveBuffer;
        end;
        dsCalcFields : begin
            RecBuf := CalcBuffer;
        end;
        dsFilter : begin
            RecBuf := TempBuffer;
        end;
        else begin
            RecBuf := nil;
        end;
    end;
    Result := RecBuf <> nil;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TXPMemoryDataset.GetBlobData(Field : TField; Buffer : PChar) : TMemBlobData;
begin
    Result := PMemBlobArray(Buffer + FBlobOfs)[Field.Offset];
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPMemoryDataset.GetBookmarkData(Buffer : TRecordBuffer; Data : Pointer);
begin
		Move(PXPMemBookmarkInfo(Buffer + FBookmarkOfs)^.BookmarkData, Data^, SizeOf(TBookmarkData));
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TXPMemoryDataset.GetBookmarkFlag(Buffer : TRecordBuffer) : TBookmarkFlag;
begin
		Result := PXPMemBookmarkInfo(Buffer + FBookmarkOfs)^.BookmarkFlag;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TXPMemoryDataset.GetIsIndexField(Field : TField) : boolean;
begin
    if FIndexList <> nil then begin
        Result := FIndexList.IndexOf(Field) >= 0;
    end else begin
        Result := False;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TXPMemoryDataset.GetRecNo : Integer;
begin
    CheckActive;
    UpdateCursorPos;
    if (FRecordPos = -1) and (RecordCount > 0) then begin
        Result := 1;
    end else begin
        Result := FRecordPos + 1;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TXPMemoryDataset.GetRecord(Buffer : TRecordBuffer; GetMode : TGetMode; DoCheck : boolean) : TGetResult;
var
    Accept : boolean;
begin
		Result := grOk;
    Accept := True;
    case GetMode of
        gmPrior : begin
            if FRecordPos <= 0 then begin
                Result     := grBOF;
                FRecordPos := -1;
            end else begin
                repeat
                    Dec(FRecordPos);
                    if Filtered then begin
                        Accept := RecordFilter;
                    end;
                until Accept or (FRecordPos < 0);
                if not Accept then begin
                    Result     := grBOF;
                    FRecordPos := -1;
                end;
            end;
        end;
        gmCurrent : begin
            if (FRecordPos < 0) or (FRecordPos >= RecordCount) then begin
                Result := grError;
            end else if Filtered then begin
                if not RecordFilter then begin
                    Result := grError;
                end;
            end;
        end;
        gmNext : begin
            if FRecordPos >= RecordCount - 1 then begin
                Result := grEOF;
            end else begin
                repeat
                    Inc(FRecordPos);
                    if Filtered then begin
                        Accept := RecordFilter;
                    end;
                until Accept or (FRecordPos > RecordCount - 1);
                if not Accept then begin
                    Result     := grEOF;
                    FRecordPos := RecordCount - 1;
                end;
            end;
        end;
    end;
    if Result = grOk then begin
        RecordToBuffer(Records[FRecordPos], Buffer);
    end else if (Result = grError) and DoCheck then begin
        Error(RsEMemNoRecords);
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TXPMemoryDataset.GetRecordCount : Integer;
begin
    Result := FRecords.Count;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TXPMemoryDataset.GetRecordSize : Word;
begin
    Result := FRecordSize;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPMemoryDataset.InitFieldDefsFromFields;
var
    I :      Integer;
    Offset : Word;
begin
    if FieldDefs.Count = 0 then begin
        for I := 0 to FieldCount - 1 do begin
            if (Fields[I].FieldKind in fkStoredFields) and not (Fields[I].DataType in ftSupported) then begin
                ErrorFmt(SUnknownFieldType, [Fields[I].DisplayName]);
            end;
        end;
        FreeIndexList;
    end;
    Offset := 0;
    inherited InitFieldDefsFromFields;
    { Calculate fields offsets }
    ReallocMem(FOffsets, FieldDefList.Count * SizeOf(Word));
    for I := 0 to FieldDefList.Count - 1 do begin
        FOffsets^[I] := Offset;
        if FieldDefList[I].DataType in ftSupported - ftBlobTypes then begin
            Inc(Offset, CalcFieldLen(FieldDefList[I].DataType, FieldDefList[I].Size) + 1);
        end;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPMemoryDataset.InitRecord(Buffer : TRecordBuffer);
var
    BInfo : PXPMemBookmarkInfo;
begin
		inherited InitRecord(Buffer);
    BInfo := PXPMemBookmarkInfo(Buffer + FBookmarkOfs);
    BInfo^.BookmarkData := Low(Integer);
    BInfo^.BookmarkFlag := bfInserted;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPMemoryDataset.InternalAddRecord(Buffer : Pointer; Append : boolean);
var
    RecPos : Integer;
    Rec :    TXPMemoryRecord;
begin
    if Append then begin
        Rec := AddRecord;
        FRecordPos := FRecords.Count - 1;
    end else begin
        if FRecordPos = -1 then begin
            RecPos := 0;
        end else begin
            RecPos := FRecordPos;
        end;
        Rec := InsertRecord(RecPos);
        FRecordPos := RecPos;
    end;
    SetAutoIncFields(Buffer);
    SetMemoryRecordData(Buffer, Rec.Index);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPMemoryDataset.InternalClose;
begin
    ClearRecords;
    FAutoInc := 1;
    BindFields(False);
    if DefaultFields then begin
        DestroyFields;
    end;
    FreeIndexList;
    FActive := False;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPMemoryDataset.InternalDelete;
var
    Accept :   boolean;
    Status :   TRecordStatus;
    PFValues : TPVariant;

    //---------------------------------

begin
    //---------------------- Added by CFZ 2004/03/03 ----------------------
    Status   := rsOriginal;
    PFValues := nil;
    if FApplyMode <> amNone then begin
        try
            Status := TRecordStatus(FieldByName(FStatusName).AsInteger);
            if Status <> rsInserted then begin
                if FApplyMode = amAppend then begin
                    Cancel;
                    Exit;
                end else begin
                    New(PFValues);
                    PFValues^ := GetKeyValues;
                end;
            end;
        except
            SysUtils.Abort;
            if not (PFValues = nil) then begin
                Dispose(PFValues);
            end;
            Exit;
        end;
    end;
    //----------------------------------------------------------------------

    Records[FRecordPos].Free;
    if FRecordPos >= FRecords.Count then begin
        Dec(FRecordPos);
    end;
    Accept := True;
    repeat
        if Filtered then begin
            Accept := RecordFilter;
        end;
        if not Accept then begin
            Dec(FRecordPos);
        end;
    until Accept or (FRecordPos < 0);
    if FRecords.Count = 0 then begin
        FLastID := Low(Integer);
    end;

    //---------------------- Added by CFZ 2004/03/03 ----------------------
    if FApplyMode <> amNone then begin
        if Status = rsInserted then begin
            Dec(FRowsChanged);
        end else begin
            FDeletedValues.Add(PFValues);
        end;
        if Status = rsOriginal then begin
            Inc(FRowsChanged);
        end;
    end;
    //----------------------------------------------------------------------
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPMemoryDataset.InternalFirst;
begin
    FRecordPos := -1;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPMemoryDataset.InternalGotoBookmark(Bookmark : Pointer);
var
    Rec :     TXPMemoryRecord;
    SavePos : Integer;
    Accept :  boolean;
begin
		Rec := FindRecordID(TBookmarkData(Bookmark^));
    if Rec <> nil then begin
        Accept  := True;
        SavePos := FRecordPos;
        try
            FRecordPos := Rec.Index;
            if Filtered then begin
                Accept := RecordFilter;
            end;
        finally
            if not Accept then begin
                FRecordPos := SavePos;
            end;
        end;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPMemoryDataset.InternalHandleException;
begin
    Application.HandleException(Self);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPMemoryDataset.InternalInitFieldDefs;
begin
    // InitFieldDefsFromFields
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPMemoryDataset.InternalInitRecord(Buffer : TRecordBuffer);
var
    I : Integer;
begin
		FillChar(Buffer^, FBlobOfs, 0);
    for I := 0 to BlobFieldCount - 1 do begin
        PMemBlobArray(Buffer + FBlobOfs)[I] := '';
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPMemoryDataset.InternalLast;
begin
    FRecordPos := FRecords.Count;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPMemoryDataset.InternalOpen;
begin
    BookmarkSize := SizeOf(TBookmarkData);
    if DefaultFields then begin
        CreateFields;
    end;
    BindFields(True);
    InitBufferPointers(True);
    InternalFirst;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPMemoryDataset.InternalPost;
var
    RecPos :    Integer;
    Index :     Integer;
    Status :    TRecordStatus;
    NewChange : boolean;

    //-------------------------------------

begin
    //------------------------ Added by CFZ 2004/03/03 ------------------------
    NewChange := False;
    if (FApplyMode <> amNone) and (not FInOpen) and (not FInRefresh) then begin
        try
            Status := TRecordStatus(FieldByName(FStatusName).AsInteger);
            if State = dsInsert then begin
                if IsDeleted(Index) then begin
                    FDeletedValues.Delete(Index);
                    NewChange := False;
                    if FApplyMode = amAppend then begin
                        FieldByName(FStatusName).AsInteger := Integer(rsInserted);
                    end else begin
                        FieldByName(FStatusName).AsInteger := Integer(rsUpdated);
                    end;
                end else begin
                    NewChange := True;
                    FieldByName(FStatusName).AsInteger := Integer(rsInserted);
                end;
            end;
            if State = dsEdit then begin
                if (Status = rsOriginal) and (FApplyMode = amAppend) then begin
                    Cancel;
                    Exit;
                end;
                (* If (Status = rsInserted) Or (Satus = rsUpdated) Then NewChange := False; *)
            end;
        except
            SysUtils.Abort;
            Exit;
        end;
    end;
    //---------------------------------------------------------------------------
    if State = dsEdit then begin
        SetMemoryRecordData(ActiveBuffer, FRecordPos);
    end else begin
        if State in [dsInsert] then begin
            SetAutoIncFields(ActiveBuffer);
        end;
        if FRecordPos >= FRecords.Count then begin
            SetMemoryRecordData(ActiveBuffer, AddRecord.Index);
            FRecordPos := FRecords.Count - 1;
        end else begin
            if FRecordPos = -1 then begin
                RecPos := 0;
            end else begin
                RecPos := FRecordPos;
            end;
            SetMemoryRecordData(ActiveBuffer, InsertRecord(RecPos).Index);
            FRecordPos := RecPos;
        end;
    end;
    //------------------------ Added by CFZ 2004/03/03 ------------------------
    if NewChange then begin
        Inc(FRowsChanged);
    end;
    //---------------------------------------------------------------------------
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPMemoryDataset.InternalSetToRecord(Buffer : TRecordBuffer);
begin
		InternalGotoBookmark(@PXPMemBookmarkInfo(Buffer + FBookmarkOfs)^.BookmarkData);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TXPMemoryDataset.IsCursorOpen : boolean;
begin
    Result := FActive;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPMemoryDataset.OpenCursor(InfoQuery : boolean);
begin
    if not InfoQuery then begin
        if FieldCount > 0 then begin
            FieldDefs.Clear;
        end;
        InitFieldDefsFromFields;
    end;
    FActive := True;
    inherited OpenCursor(InfoQuery);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPMemoryDataset.RecordToBuffer(Rec : TXPMemoryRecord; Buffer : PChar);
var
    I :     Integer;
    BInfo : PXPMemBookmarkInfo;
begin
    Move(Rec.Data^, Buffer^, FRecordSize);
    BInfo := PXPMemBookmarkInfo(Buffer + FBookmarkOfs);
    BInfo^.BookmarkData := Rec.ID;
    BInfo^.BookmarkFlag := bfCurrent;
    for I := 0 to BlobFieldCount - 1 do begin
        PMemBlobArray(Buffer + FBlobOfs)[I] := PMemBlobArray(Rec.FBlobs)[I];
    end;
    GetCalcFields(Buffer);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPMemoryDataset.SetAutoIncFields(Buffer : PChar);
var
    I, Count : Integer;
    Data :     PChar;
begin
    Count := 0;
    for I := 0 to FieldCount - 1 do begin
        if (Fields[I].FieldKind in fkStoredFields) and
            (Fields[I].DataType = ftAutoInc) then begin
            Data := FindFieldData(Buffer, Fields[I]);
            if Data <> nil then begin
                Data[0] := char(Ord(True));
                Inc(Data);
                Move(FAutoInc, Data^, SizeOf(longint));
                Inc(Count);
            end;
        end;
    end;
    if Count > 0 then begin
        Inc(FAutoInc);
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPMemoryDataset.SetBlobData(Field : TField; Buffer : PChar; Value : TMemBlobData);
begin
    if Buffer = ActiveBuffer then begin
        if State = dsFilter then begin
            Error(SNotEditing);
        end;
        PMemBlobArray(Buffer + FBlobOfs)[Field.Offset] := Value;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPMemoryDataset.SetBookmarkData(Buffer : TRecordBuffer; Data : Pointer);
begin
		Move(Data^, PXPMemBookmarkInfo(Buffer + FBookmarkOfs)^.BookmarkData, SizeOf(TBookmarkData));
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPMemoryDataset.SetBookmarkFlag(Buffer : TRecordBuffer; Value : TBookmarkFlag);
begin
		PXPMemBookmarkInfo(Buffer + FBookmarkOfs)^.BookmarkFlag := Value;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPMemoryDataset.SetFieldData(Field : TField; Buffer : Pointer);
var
    RecBuf, Data : PChar;
    VarData :      variant;
begin
    if not (State in dsWriteModes) then begin
        Error(SNotEditing);
    end;
    GetActiveRecBuf(RecBuf);
    with Field do begin
        if FieldNo > 0 then begin
            if State in [dsCalcFields, dsFilter] then begin
                Error(SNotEditing);
            end;
            if ReadOnly and not (State in [dsSetKey, dsFilter]) then begin
                ErrorFmt(SFieldReadOnly, [DisplayName]);
            end;
            Validate(Buffer);
            if FieldKind <> fkInternalCalc then begin
                Data := FindFieldData(RecBuf, Field);
                if Data <> nil then begin
                    if DataType = ftVariant then begin
                        if Buffer <> nil then begin
                            VarData := PVariant(Buffer)^;
                        end else begin
                            VarData := EmptyParam;
                        end;
                        Data[0] := char(Ord((Buffer <> nil) and not
                            (VarIsNull(VarData) or VarIsEmpty(VarData))));
                        if Data[0] <> #0 then begin
                            Inc(Data);
                            PVariant(Data)^ := VarData;
                        end else begin
                            FillChar(Data^, CalcFieldLen(DataType, Size), 0);
                        end;
                    end else begin
                        Data[0] := char(Ord(Buffer <> nil));
                        Inc(Data);
                        if Buffer <> nil then begin
                            Move(Buffer^, Data^, CalcFieldLen(DataType, Size));
                        end else begin
                            FillChar(Data^, CalcFieldLen(DataType, Size), 0);
                        end;
                    end;
                end;
            end;
        end else {fkCalculated, fkLookup} begin
            Inc(RecBuf, FRecordSize + Offset);
            RecBuf[0] := char(Ord(Buffer <> nil));
            if RecBuf[0] <> #0 then begin
                Move(Buffer^, RecBuf[1], DataSize);
            end;
        end;
        if not (State in [dsCalcFields, dsFilter, dsNewValue]) then begin
            DataEvent(deFieldChange, longint(Field));
        end;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPMemoryDataset.SetFiltered(Value : boolean);
begin
    if Active then begin
        CheckBrowseMode;
        if Filtered <> Value then begin
            inherited SetFiltered(Value);
        end;
        First;
    end else begin
        inherited SetFiltered(Value);
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPMemoryDataset.SetMemoryRecordData(Buffer : PChar; Pos : Integer);
var
    Rec : TXPMemoryRecord;
begin
    if State = dsFilter then begin
        Error(SNotEditing);
    end;
    Rec := Records[Pos];
    AssignMemoryRecord(Rec, Buffer);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPMemoryDataset.SetOnFilterRecord(const Value : TFilterRecordEvent);
begin
    if Active then begin
        CheckBrowseMode;
        inherited SetOnFilterRecord(Value);
        if Filtered then begin
            First;
        end;
    end else begin
        inherited SetOnFilterRecord(Value);
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPMemoryDataset.SetRecNo(Value : Integer);
begin
    if (Value > 0) and (Value <= FRecords.Count) then begin
        FRecordPos := Value - 1;
        Resync([]);
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TXPMemoryDataset.ApplyChanges : boolean;
var
    xKey :     variant;
    PxKey :    TPVariant;
    Len, Row : Integer;
    Status :   TRecordStatus;
    bOpen, bFound : boolean;
    FOriginal, FClient : TField;

    function WriteFields : boolean;
    var
        J : Integer;
    begin
        try
            for J := 0 to Len do begin
                FClient   := Fields[J];
                FOriginal := FDataSet.FindField(FClient.FieldName);
                if (FOriginal <> nil) and (FClient <> nil) then begin
                    if FClient.IsNull then begin
                        FOriginal.Clear;
                    end else begin
                        FDataSet.FieldByName(FOriginal.FieldName).Value := FClient.Value;
                    end;
                end;
            end;
            Result := True;
        except
            Result := False;
        end;
    end;

    function InsertRec : boolean;
    begin
        try
            FDataSet.Append;
            WriteFields;
            FDataSet.Post;
            Result := True;
        except
            Result := False;
        end;
    end;

    function UpdateRec : boolean;
    begin
        try
            FDataSet.Edit;
            WriteFields;
            FDataSet.Post;
            Result := True;
        except
            Result := False;
        end;
    end;

    function DeleteRec : boolean;
    begin
        try
            FDataSet.Delete;
            Result := True;
        except
            Result := False;
        end;
    end;

    function SaveChanges : Integer;
    var
        I : Integer;
    begin
        Result := 0;
        FDataSet.DisableControls;
        DisableControls;
        Row := RecNo;
        try
            First;
            while not EOF do begin
                Status := TRecordStatus(FieldByName(FStatusName).AsInteger);
                xKey   := GetKeyValues;
                bFound := FDataSet.Locate(FKeyFieldNames, xKey, []);
                if Status = rsInserted then begin
                    if bFound then begin
                        if FExactApply then begin
                            Error(SRecordDuplicate);
                            Break;
                        end;
                    end else begin
                        if InsertRec then begin
                            Inc(Result);
                        end else begin
                            if FExactApply then begin
                                Error(SInsertError);
                                Break;
                            end else begin
                                SysUtils.Abort;
                                if FDataSet.State in dsEditModes then begin
                                    FDataSet.Cancel;
                                end;
                            end;
                        end;
                    end;
                end;
                if Status = rsUpdated then begin
                    if not bFound then begin
                        if FExactApply then begin
                            Error(SRecordInexistent);
                            Break;
                        end;
                    end else begin
                        if UpdateRec then begin
                            Inc(Result);
                        end else begin
                            if FExactApply then begin
                                Error(SUpdateError);
                                Break;
                            end else begin
                                SysUtils.Abort;
                                if FDataSet.State in dsEditModes then begin
                                    FDataSet.Cancel;
                                end;
                            end;
                        end;
                    end;
                end;
                Next;
            end;
            if FApplyMode = amMerge then begin
                for I := 0 to FDeletedValues.Count - 1 do begin
                    PxKey  := FDeletedValues[I];
                    xKey   := PxKey^;
                    bFound := FDataSet.Locate(FKeyFieldNames, xKey, []);
                    if bFound then begin
                        if DeleteRec then begin
                            Inc(Result);
                        end else begin
                            if FExactApply then begin
                                Error(SDeleteError);
                                Break;
                            end else begin
                                SysUtils.Abort;
                            end;
                        end;
                    end;
                end;
            end;
        finally
            RecNo := Row;
            EnableControls;
            FDataSet.EnableControls;
        end;
    end;

begin
    CheckBrowseMode;
    Result := False;

    if FRowsChanged < 1 then begin
        Exit;
    end;
    if IsEmpty and (FDeletedValues.Count < 1) then begin
        Exit;
    end;
    if (FApplyMode = amNone) or (FDataSet = nil) then begin
        Exit;
    end;
    if (FApplyMode <> amNone) and (FKeyFieldNames = '') then begin
        Exit;
    end;
    Len := FieldDefs.Count - 2;
    if Len < 1 then begin
        Exit;
    end;

    bOpen := FDataSet.Active;
    try
        if not bOpen then begin
            FDataSet.Open;
        end;
    except
        Exit;
    end;

    DoBeforeApply;
    FRowsAffected := SaveChanges;
    Result := (FRowsAffected = FRowsChanged) or
        ((FRowsAffected > 0) and (FRowsAffected <= FRowsChanged) and not FExactApply);
    if Result then begin
        ClearChanges;
    end;
    DoAfterApply;

    if not bOpen then begin
        FDataSet.Close;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TXPMemoryDataset.BookmarkValid(Bookmark : TBookmark) : boolean;
begin
    Result := (Bookmark <> nil) and FActive and (TBookmarkData(Bookmark^) > Low(Integer)) and
        (TBookmarkData(Bookmark^) <= FLastID);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPMemoryDataset.CancelChanges;
begin
    CheckBrowseMode;
    if FRowsChanged < 1 then begin
        Exit;
    end;
    if (FApplyMode = amNone) or (FDataSet = nil) then begin
        Exit;
    end;
    if (FApplyMode <> amNone) and (FKeyFieldNames = '') then begin
        Exit;
    end;
    ClearChanges;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPMemoryDataset.ClearChanges;
begin
    // FRowsAffected := 0;
    FRowsChanged   := 0;
    FRowsOriginals := 0;
    FDeletedValues.Clear;
    EmptyTable;
    if FLoadRecords then begin
        FInRefresh     := True;
        FRowsOriginals := CopyFromDataset;
        if FKeyFieldNames <> '' then begin
            SortOnFields(FKeyFieldNames);
        end;
        if FApplyMode = amAppend then begin
            Last;
        end else begin
            First;
        end;
        FInRefresh := False;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TXPMemoryDataset.CompareBookmarks(Bookmark1, Bookmark2 : TBookmark) : Integer;
begin
    if (Bookmark1 = nil) and (Bookmark2 = nil) then begin
        Result := 0;
    end else if (Bookmark1 <> nil) and (Bookmark2 = nil) then begin
        Result := 1;
    end else if (Bookmark1 = nil) and (Bookmark2 <> nil) then begin
        Result := -1;
    end else if TBookmarkData(Bookmark1^) > TBookmarkData(Bookmark2^) then begin
        Result := 1;
    end else if TBookmarkData(Bookmark1^) < TBookmarkData(Bookmark2^) then begin
        Result := -1;
    end else begin
        Result := 0;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPMemoryDataset.CopyStructure(Source : TDataSet; UseAutoIncAsInteger : boolean = False);
var
    I : Integer;
begin
    if Source = nil then begin
        Exit;
    end;
    CheckInactive;
    for I := FieldCount - 1 downto 0 do begin
        Fields[I].Free;
    end;

    Source.FieldDefs.Update;
    FieldDefs := Source.FieldDefs;
    if FApplyMode <> amNone then begin
        AddStatusField;
    end;
    CheckStructure(UseAutoIncAsInteger);
    if FApplyMode <> amNone then begin
        HideStatusField;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
constructor TXPMemoryDataset.Create(AOwner : TComponent);
begin
    inherited Create(AOwner);
    FRecordPos   := -1;
    FLastID      := Low(Integer);
    FAutoInc     := 1;
    FRecords     := TList.Create;
    //------- Added by CFZ 2004/03/03 -------
    FStatusName  := STATUSNAME;
    FDeletedValues := TList.Create;
    FRowsOriginals := 0;
    FRowsChanged := 0;
    FRowsAffected := 0;
    FInOpen      := False;
    FInRefresh   := False;
    //---------------------------------------
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TXPMemoryDataset.CreateBlobStream(Field : TField; Mode : TBlobStreamMode) : TStream;
begin
    Result := TXPMemBlobStream.Create(Field as TBlobField, Mode);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
destructor TXPMemoryDataset.Destroy;
begin
    //------- Added by CFZ 2004/03/03 -------
    FreeAndNil(FDeletedValues);
    //---------------------------------------
    FreeIndexList;
    ClearRecords;
    FRecords.Free;
    ReallocMem(FOffsets, 0);
    inherited Destroy;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPMemoryDataset.EmptyTable;
begin
    if Active then begin
        CheckBrowseMode;
        ClearRecords;
        ClearBuffers;
        DataEvent(deDataSetChange, 0);
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TXPMemoryDataset.FindDeleted(KeyValues : variant) : Integer;
var
    I, J, Len, Equals : Integer;
    PxKey : TPVariant;
    xKey, ValRow, ValDel : variant;
begin
    Result := -1;
    if VarIsNull(KeyValues) then begin
        Exit;
    end;
    PxKey := nil;
    Len   := VarArrayHighBound(KeyValues, 1);
    try
        for I := 0 to FDeletedValues.Count - 1 do begin
            PxKey  := FDeletedValues[I];
            xKey   := PxKey^;
            Equals := -1;
            for J := 0 to Len - 1 do begin
                ValRow := KeyValues[J];
                ValDel := xKey[J];
   {$IFDEF COMPILER6_UP}
       if VarCompareValue(ValRow, ValDel) = vrEqual then
       {$ELSE}
                if ValRow = ValDel then {$ENDIF COMPILER6_UP}
                begin
                    Inc(Equals);
                    if Equals = (Len - 1) then begin
                        Break;
                    end;
                end;
            end;
            if Equals = (Len - 1) then begin
                Result := I;
                Break;
            end;
        end;
    finally
        if not (PxKey = nil) then begin
            Dispose(PxKey);
        end;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TXPMemoryDataset.GetCurrentRecord(Buffer : PChar) : boolean;
begin
    Result := False;
    if not IsEmpty and (GetBookmarkFlag(ActiveBuffer) = bfCurrent) then begin
        UpdateCursorPos;
        if (FRecordPos >= 0) and (FRecordPos < RecordCount) then begin
            Move(Records[FRecordPos].Data^, Buffer^, FRecordSize);
            Result := True;
        end;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TXPMemoryDataset.GetFieldData(Field : TField; Buffer : Pointer) : boolean;
var
    RecBuf, Data : PChar;
    VarData :      variant;
begin
    Result := False;
    if not GetActiveRecBuf(RecBuf) then begin
        Exit;
    end;
    if Field.FieldNo > 0 then begin
        Data := FindFieldData(RecBuf, Field);
        if Data <> nil then begin
            if Field is TBlobField then begin
                Result := Data <> nil;
            end else begin
                Result := Data[0] <> #0;
            end;
            Inc(Data);
            if Field.DataType in [ftString, ftFixedChar, ftWideString, ftGuid] then begin
                Result := Result and (StrLen(Data) > 0);
            end;
            if Result and (Buffer <> nil) then begin
                if Field.DataType = ftVariant then begin
                    VarData := PVariant(Data)^;
                    PVariant(Buffer)^ := VarData;
                end else begin
                    Move(Data^, Buffer^, CalcFieldLen(Field.DataType, Field.Size));
                end;
            end;
        end;
    end else if State in [dsBrowse, dsEdit, dsInsert, dsCalcFields] then begin
        Inc(RecBuf, FRecordSize + Field.Offset);
        Result := RecBuf[0] <> #0;
        if Result and (Buffer <> nil) then begin
            Move(RecBuf[1], Buffer^, Field.DataSize);
        end;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TXPMemoryDataset.IsDeleted(out Index : Integer) : boolean;
begin
    Index  := FindDeleted(GetKeyValues);
    Result := Index > -1;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TXPMemoryDataset.IsInserted : boolean;
begin
    Result := TRecordStatus(FieldByName(FStatusName).AsInteger) = rsInserted;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TXPMemoryDataset.IsOriginal : boolean;
begin
    Result := TRecordStatus(FieldByName(FStatusName).AsInteger) = rsOriginal;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TXPMemoryDataset.IsSequenced : boolean;
begin
    Result := not Filtered;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TXPMemoryDataset.IsUpdated : boolean;
begin
    Result := TRecordStatus(FieldByName(FStatusName).AsInteger) = rsUpdated;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TXPMemoryDataset.LoadFromDataSet(Source : TDataSet; RecordCount : Integer; Mode : TLoadMode; DisableAllControls : boolean
    = True) : Integer;
var
    SourceActive : boolean;
    MovedCount, I : Integer;
    SB, DB : TBookmark;
begin
    Result := 0;
    if Source = Self then begin
        Exit;
    end;
    FSaveLoadState := slsLoading;
    SourceActive   := Source.Active;
    if DisableAllControls then begin
        Source.DisableControls;
    end;
    SB := Source.GetBookmark;
    try
        if DisableAllControls then begin
            Self.DisableControls;
        end;
        DB := GetBookmark;
        try
            Filtered := False;
            Source.Open;
            Source.CheckBrowseMode;
            Source.UpdateCursorPos;
            if Mode = lmCopy then begin
                Close;
                CopyStructure(Source, AutoIncAsInteger);
            end;
            FreeIndexList;
            if not Active then begin
                Open;
            end;
            CheckBrowseMode;
            if RecordCount > 0 then begin
                MovedCount := RecordCount;
            end else begin
                Source.First;
                MovedCount := MaxInt;
            end;
            FAutoIncField := nil;
            // FixReadOnlyFields also sets FAutoIncField if there is any
            FixReadOnlyFields(False);
            // find first source autoinc field
            FSrcAutoIncField := nil;
            if Mode = lmCopy then begin
                for I := 0 to Source.FieldCount - 1 do begin
                    if Source.Fields[I].DataType = ftAutoInc then begin
                        FSrcAutoIncField := Source.Fields[I];
                        Break;
                    end;
                end;
            end;
            try
                while not Source.EOF do begin
                    Append;
                    TGDBHnd.AssignRecord(Source, Self, True);
                    // assign AutoInc value manually (make usre to keep largest if source isn't sorted by autoinc field)
                    if (FAutoIncField <> nil) and (FSrcAutoIncField <> nil) then begin
                        FAutoInc := Max(FAutoInc, FSrcAutoIncField.AsInteger);
                    end;
                    Post;
                    Inc(Result);
                    if Result >= MovedCount then begin
                        Break;
                    end;
                    Source.Next;
                end;
            finally
                FixReadOnlyFields(True);
                FAutoIncField    := nil;
                FSrcAutoIncField := nil;
                First;
            end;
            // move back to where we started from
            if (DB <> nil) and BookmarkValid(DB) then begin
                GotoBookmark(DB);
                FreeBookmark(DB);
            end;
        finally
            if DisableAllControls then begin
                EnableControls;
            end;
        end;
    finally
        // move back to where we started from
        if (SB <> nil) and Source.BookmarkValid(SB) then begin
            Source.GotoBookmark(SB);
            Source.FreeBookmark(SB);
        end;
        if not SourceActive then begin
            Source.Close;
        end;
        if DisableAllControls then begin
            Source.EnableControls;
        end;
        FSaveLoadState := slsNone;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TXPMemoryDataset.Locate(const KeyFields : string; const KeyValues : variant; Options : TLocateOptions) : boolean;
begin
    DoBeforeScroll;
    Result := TGDBHnd.DataSetLocateThrough(Self, KeyFields, KeyValues, Options);
    if Result then begin
        DataEvent(deDataSetChange, 0);
        DoAfterScroll;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPMemoryDataset.Open;
begin
    FInOpen := True;
    try
        if FDataSet <> nil then begin
            if FLoadStructure then begin
                CopyStructure(FDataSet, FAutoIncAsInteger);
            end else begin
                AddStatusField;
                CheckStructure(FAutoIncAsInteger);
                HideStatusField;
            end;
        end;
        inherited;
    except
        FInOpen := False;
        SysUtils.Abort;
        Exit;
    end;

    if Assigned(FDataSet) and (FApplyMode <> amNone) then begin
        if not FDataSet.Active then begin
            FDataSet.Open;
        end;
        if FLoadRecords then begin
            FRowsOriginals := CopyFromDataset;
            if FKeyFieldNames <> '' then begin
                SortOnFields(KeyFieldNames);
            end;
            if FApplyMode = amAppend then begin
                Last;
            end else begin
                First;
            end;
        end;
    end;
    FInOpen := False;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TXPMemoryDataset.SaveToDataSet(Dest : TDataSet; RecordCount : Integer; DisableAllControls : boolean = True) : Integer;
var
    MovedCount : Integer;
    SB, DB :     TBookmark;
begin
    Result := 0;
    if Dest = Self then begin
        Exit;
    end;
    CheckBrowseMode;
    UpdateCursorPos;
    if DisableAllControls then begin
        DisableControls;
        Dest.DisableControls;
    end;
    FSaveLoadState := slsSaving;
    try
        SB := GetBookmark;
        DB := Dest.GetBookmark;
        try
            if not Dest.Active then begin
                Dest.Open;
            end else begin
                Dest.CheckBrowseMode;
            end;
            if RecordCount > 0 then begin
                MovedCount := RecordCount;
            end else begin
                First;
                MovedCount := MaxInt;
            end;
            try
                while not EOF do begin
                    Dest.Append;
                    TGDBHnd.AssignRecord(Self, Dest, True);
                    Dest.Post;
                    Inc(Result);
                    if Result >= MovedCount then begin
                        Break;
                    end;
                    Next;
                end;
            finally
                Dest.First;
            end;
        finally
            if (SB <> nil) and BookmarkValid(SB) then begin
                GotoBookmark(SB);
                FreeBookmark(SB);
            end;
            if (DB <> nil) and Dest.BookmarkValid(DB) then begin
                Dest.GotoBookmark(DB);
                Dest.FreeBookmark(DB);
            end;
        end;
    finally
        if DisableAllControls then begin
            EnableControls;
            Dest.EnableControls;
        end;
        FSaveLoadState := slsNone;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPMemoryDataset.SortOnFields(const FieldNames : string; CaseInsensitive : boolean = True; Descending : boolean = False);
begin
    CreateIndexList(FieldNames);
    FCaseInsensitiveSort := CaseInsensitive;
    FDescendingSort      := Descending;
    try
        Sort;
    except
        FreeIndexList;
        raise;
    end;
end;

{-**********************************************************************
************************************************************************
******************
******************  Class:    TXPMemBlobStream
******************  Category: No category
******************
************************************************************************
************************************************************************}
{--------------------------------------------------------------------------------------------------------------------------------}
function TXPMemBlobStream.GetBlobFromRecord(Field : TField) : TMemBlobData;
var
    Rec : TXPMemoryRecord;
    Pos : Integer;
begin
    Result := '';
    Pos    := FDataSet.FRecordPos;
    if (Pos < 0) and (FDataSet.RecordCount > 0) then begin
        Pos := 0;
    end else if Pos >= FDataSet.RecordCount then begin
        Pos := FDataSet.RecordCount - 1;
    end;
    if (Pos >= 0) and (Pos < FDataSet.RecordCount) then begin
        Rec := FDataSet.Records[Pos];
        if Rec <> nil then begin
            Result := PMemBlobArray(Rec.FBlobs)[FField.Offset];
        end;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TXPMemBlobStream.GetBlobSize : longint;
begin
    Result := 0;
    if FOpened then begin
        if FCached then begin
            Result := Length(FDataSet.GetBlobData(FField, FBuffer));
        end else begin
            Result := Length(GetBlobFromRecord(FField));
        end;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
constructor TXPMemBlobStream.Create(Field : TBlobField; Mode : TBlobStreamMode);
begin
    // (rom) added inherited Create;
    inherited Create;
    FMode    := Mode;
    FField   := Field;
    FDataSet := FField.DataSet as TXPMemoryDataset;
    if not FDataSet.GetActiveRecBuf(FBuffer) then begin
        Exit;
    end;
    if not FField.Modified and (Mode <> bmRead) then begin
        if FField.ReadOnly then begin
            ErrorFmt(SFieldReadOnly, [FField.DisplayName]);
        end;
        if not (FDataSet.State in [dsEdit, dsInsert]) then begin
            Error(SNotEditing);
        end;
        FCached := True;
    end else begin
        FCached := (FBuffer = FDataSet.ActiveBuffer);
    end;
    FOpened := True;
    if Mode = bmWrite then begin
        Truncate;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
destructor TXPMemBlobStream.Destroy;
begin
    if FOpened and FModified then begin
        FField.Modified := True;
    end;
    if FModified then begin
        try
            FDataSet.DataEvent(deFieldChange, longint(FField));
        except
            Application.HandleException(Self);
        end;
    end;
    // (rom) added inherited Destroy;
    inherited Destroy;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TXPMemBlobStream.Read(var Buffer; Count : longint) : longint;
begin
    Result := 0;
    if FOpened then begin
        if Count > Size - FPosition then begin
            Result := Size - FPosition;
        end else begin
            Result := Count;
        end;
        if Result > 0 then begin
            if FCached then begin
                Move(PChar(FDataSet.GetBlobData(FField, FBuffer))[FPosition], Buffer,
                    Result);
                Inc(FPosition, Result);
            end else begin
                Move(PChar(GetBlobFromRecord(FField))[FPosition], Buffer, Result);
                Inc(FPosition, Result);
            end;
        end;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TXPMemBlobStream.Seek(Offset : longint; Origin : Word) : longint;
begin
    case Origin of
        soFromBeginning : begin
            FPosition := Offset;
        end;
        soFromCurrent : begin
            Inc(FPosition, Offset);
        end;
        soFromEnd : begin
            FPosition := GetBlobSize + Offset;
        end;
    end;
    Result := FPosition;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPMemBlobStream.Truncate;
begin
    if FOpened and FCached and (FMode <> bmRead) then begin
        FDataSet.SetBlobData(FField, FBuffer, '');
        FModified := True;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TXPMemBlobStream.Write(const Buffer; Count : longint) : longint;
var
    Temp : TMemBlobData;
begin
    Result := 0;
    if FOpened and FCached and (FMode <> bmRead) then begin
        Temp := FDataSet.GetBlobData(FField, FBuffer);
        if Length(Temp) < FPosition + Count then begin
            SetLength(Temp, FPosition + Count);
        end;
        Move(Buffer, PChar(Temp)[FPosition], Count);
        FDataSet.SetBlobData(FField, FBuffer, Temp);
        Inc(FPosition, Count);
        Result    := Count;
        FModified := True;
    end;
end;

end.
