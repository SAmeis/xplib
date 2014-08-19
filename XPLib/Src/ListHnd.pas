{$IFDEF ListHnd}
    {$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I XPLib.inc}

unit ListHnd;

{{
   Autor: Roger
  Criado em : 11/5/2005
  Descrição: Manipula listas e seus descendentes


  Histórico:
  <desconhecido> - Criação da unit
  11/5/2005 - Criação da documentacao parcial da unit
}

interface

uses
    Windows, Classes, SysUtils, IniFiles;

type
    TReferenceType = (rtStruct, rtClass);

    TContainerList = class(TList)
    protected
        FRefType : TReferenceType;
    public
        constructor Create(RefType : TReferenceType);
        destructor Destroy; override;
        procedure Clear; override;
        procedure Delete(Index : integer);
        procedure Move(CurIndex, NewIndex : integer);
        function Remove(Item : Pointer) : integer;
    end;

    PPIdString = ^PIdString;
    PIdString  = ^TIdString;

    TIdString = record
        ID:    integer;
        Value: string;
    end;

    TValueList = class(TObject)
 {{
 Classe para armazenar valores de strings vinculadas a numeros inteiros.
 }
    private
        FBlockSize : word;
        FCapacity :  integer;
        FCount :     integer;
        FPointers :  PPIdString;
        procedure CheckCapacity;
    public
        constructor Create(BlockSize : word = 10);
        destructor Destroy; override;
        procedure Clear;
        function Delete(ID : integer) : boolean;
        function Exists(ID : integer) : boolean;
        function Insert(ID : integer; const Value : string) : boolean;
        function Value(ID : integer) : string;
    published
        property Capacity : integer read FCapacity;
        property Count : integer read FCount;
    end;

    TListHnd = class
    public
        class function HasDupItems(List : TList) : boolean;
        class procedure InvertOrder(List : TList);
    end;

    TStringsHnd = class
    public
        class procedure AddIdentMapEntriesIndex(Strings : TStrings; Items : array of TIdentMapEntry);
        class procedure LoadIdentMapEntriesIndex(Strings : TStrings; Items : array of TIdentMapEntry);
        class procedure AddIdentMapEntries(Strings : TStrings; Items : array of TIdentMapEntry);
        class procedure LoadIdentMapEntries(Strings : TStrings; Items : array of TIdentMapEntry);
    end;

    TNamedObjectList = class
    private
        FList : THashedStringList;
        FOwnedObjects : boolean;
        function GetObjects(const Name : string) : TObject;
        function GetItems(Index : integer) : TObject;
        function GetNames(Index : integer) : string;
        function GetCount : integer;
    public
        constructor Create(AOwnedObjects : boolean = False); virtual;
        destructor Destroy; override;
        procedure Add(const AName : string; Obj : TObject);
        procedure Remove(const AName : string);
        procedure Extract(const AName : string);
        property Items[Index : integer] : TObject read GetItems;
        property Objects[const Name : string] : TObject read GetObjects;
        property Names[Index : integer] : string read GetNames;
        property OwnedObjects : boolean read FOwnedObjects write FOwnedObjects;
        property Count : integer read GetCount;
    end;

implementation

{ TContainerList }

{-**********************************************************************
************************************************************************
******************
******************  Class:    TContainerList
******************  Category: No category
******************
************************************************************************
************************************************************************}
{--------------------------------------------------------------------------------------------------------------------------------}
constructor TContainerList.Create(RefType : TReferenceType);
begin
    FRefType := RefType;
    inherited Create;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
destructor TContainerList.Destroy;
begin
    Clear;
    inherited;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TContainerList.Clear;
var
    i : integer;
begin
    for i := Count - 1 downto 0 do begin
        Delete(i);
    end;
    inherited;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TContainerList.Delete(Index : integer);
begin
    if Items[Index] <> nil then begin
        if FRefType = rtClass then begin
            TObject(Items[Index]).Free;
        end else begin
            if FRefType = rtStruct then begin
                Dispose(Items[Index]);
            end;
        end;
    end;
    inherited;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TContainerList.Move(CurIndex, NewIndex : integer);
begin
    if Items[NewIndex] <> nil then begin
        if FRefType = rtClass then begin
            TObject(Items[NewIndex]).Free;
        end else begin
            if FRefType = rtStruct then begin
                Dispose(Items[NewIndex]);
            end;
        end;
    end;
    inherited;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TContainerList.Remove(Item : Pointer) : integer;

type
    PPointer = ^Pointer;
var
    pArray : Pointer;
    p :      PPointer; // p is a pointer to memory that contains a pointer. Confusing?
    i :      integer;

begin
    GetMem(pArray, Count * SizeOf(Pointer));
    try
        p := pArray;
        for i := 0 to Count - 1 do begin
            Inc(p, i);
            p^ := Items[i];
        end;
        Result := inherited Remove(Item);
        if Result >= 0 then begin
            p := pArray;
            Inc(p, Result);
            if p^ <> nil then begin
                if FRefType = rtClass then begin
                    TObject(p^).Free;
                end else begin
                    if FRefType = rtStruct then begin
                        Dispose(p^);
                    end;
                end;
            end;
        end;
    finally
        FreeMem(pArray);
    end;
end;

{ TValueList }

{-**********************************************************************
************************************************************************
******************
******************  Class:    TValueList
******************  Category: No category
******************
************************************************************************
************************************************************************}
{--------------------------------------------------------------------------------------------------------------------------------}
constructor TValueList.Create(BlockSize : word = 10);
{{
Constructor que recebe o tamanho do bloco a ser alocado, um valor sugerido é 10.
}
begin
    inherited Create;
    FBlockSize := BlockSize;
    if FBlockSize = 0 then begin
        Inc(FBlockSize);
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
destructor TValueList.Destroy;
{{
 Destrutor

 Chama Clear() e libera a memoria da lista
 }
begin
    Clear;
    ReallocMem(FPointers, 0);
    inherited;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TValueList.CheckCapacity;
{{
Verifica se existe espaco para a insercao de novos items, caso necessario realoca este espacao com o incremento de BlockSize entradas

// TODO -oRoger -cLIB : Liberar memoria caso esta seja dispensavel, e não apenas aumentar a lista qdo necessario.

}
begin
    if FCount <= (FCapacity - FBlockSize) then begin
        if (FCount mod FBlockSize) > 0 then begin
            FCapacity := ((FCount div FBlockSize) + 1) * FBlockSize;
        end else begin
            FCapacity := FCount;
        end;
        ReallocMem(FPointers, FCapacity * SizeOf(PPIdString));
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TValueList.Clear;
{{
 Limpa TODAS as entradas da lista e chama CheckCapacity() ao final
 }
var
    p : PPIdString;
begin
    p := FPointers;
    while FCount > 0 do begin
        Dispose(p^);
        Inc(p);
        Dec(FCount);
    end;
    CheckCapacity;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TValueList.Delete(ID : integer) : boolean;
{{
 Realiza a busca da entrada com o identificador passado e o remove da lista
 }
var
    i, j : integer;
    p :    PPIdString;
begin
    p      := FPointers;
    Result := False;
    for i := 0 to FCount - 1 do begin
        if p^^.ID = ID then begin
            Dispose(p^);
            Dec(FCount);
            for j := i to FCount - 1 do begin
                p^ := PPIdString(DWORD(p) + SizeOf(PPIdString))^;
                Inc(p);
            end;
            Result := True;
            Break;
        end;
        Inc(p);
    end;
    CheckCapacity;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TValueList.Exists(ID : integer) : boolean;
{{
 Verifica se existe valor associado ao identificador dado
 }
var
    i : integer;
    p : PPIdString;
begin
    p      := FPointers;
    Result := False;
    for i := 0 to FCount - 1 do begin
        if p^^.ID = ID then begin
            Result := True;
            Break;
        end;
        Inc(p);
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TValueList.Insert(ID : integer; const Value : string) : boolean;
{{
 Insere um valor associado ao identificador passado
 }
var
    p : PPIdString;
begin
    Result := not Exists(ID);
    if Result then begin
        if FCount >= FCapacity then begin
            ReallocMem(FPointers, (FCapacity + FBlockSize) * SizeOf(PPIdString));
            Inc(FCapacity, FBlockSize);
        end;
        p := FPointers;
        Inc(p, FCount);
        new(p^);
        p^^.ID    := ID;
        p^^.Value := Value;
        Inc(FCount);
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TValueList.Value(ID : integer) : string;
{{
 Recupera o valor associado ao identificador dado, Se este nao existir EmptyStr sera retornado
 }
var
    i : integer;
    p : PPIdString;
begin
    p      := FPointers;
    Result := EmptyStr;
    for i := 0 to FCount - 1 do begin
        if p^^.ID = ID then begin
            Result := p^^.Value;
            Break;
        end;
        Inc(p);
    end;
end;

{ TListHnd }

class function TListHnd.HasDupItems(List : TList) : boolean;
{{
 Identifica se existe elementos duplicados na lista.
 }
var
    c, i : integer;
    Item : Pointer;
begin
    Result := False;
    C      := 0;
    while C < List.Count do begin
        Item := List.Items[C];
        for i := (C + 1) to List.Count - 1 do begin
            if (Item = List.Items[i]) then begin
                Result := True;
                Exit;
            end;
        end;
        Inc(C);
    end;
end;

class procedure TListHnd.InvertOrder(List : TList);
{{
Inverte a ordem dos elementos de um TList
}
var
    LIndex, RIndex : integer;
begin
    LIndex := 0;
    RIndex := List.Count - 1;
    while (RIndex > LIndex) do begin
        List.Exchange(LIndex, RIndex);
        Inc(LIndex);
        Dec(RIndex);
    end;
end;

class procedure TStringsHnd.AddIdentMapEntries(Strings : TStrings; Items : array of TIdentMapEntry);
{{
Adciona os valores do atributo nome dos elementos dado por items na TStrings passada.

Para setar todos os valores veja LoadIdentMapEntries.

Revision: 7/4/2006 - Roger
}
var
    i : integer;
begin
    Strings.BeginUpdate;
    try
        for i := Low(Items) to High(Items) do begin
            Strings.Add(Items[i].Name);
        end;
    finally
        Strings.EndUpdate;
    end;
end;

class procedure TStringsHnd.AddIdentMapEntriesIndex(Strings : TStrings; Items : array of TIdentMapEntry);
{{
Adciona os valores do atributo nome dos elementos dado por items colocando o indice do mapeamento em Objects da
TStrings passada.

Para setar todos os valores veja LoadIdentMapEntries.

Revision: 7/4/2006 - Roger
}
var
    i : integer;
begin
    Strings.BeginUpdate;
    try
        for i := Low(Items) to High(Items) do begin
            Strings.AddObject(Items[i].Name, TObject(i));
        end;
    finally
        Strings.EndUpdate;
    end;
end;

class procedure TStringsHnd.LoadIdentMapEntries(Strings : TStrings; Items : array of TIdentMapEntry);
{{
Carrega os valores do atributo nome dos elementos dado por items na TStrings passada.

Para Adcionar os valores veja AddIdentMapEntries.

Revision: 7/4/2006 - Roger
}
var
    i : integer;
begin
    Strings.BeginUpdate;
    try
        for i := Low(Items) to High(Items) do begin
            Strings.Add(Items[i].Name);
        end;
    finally
        Strings.EndUpdate;
    end;
end;

class procedure TStringsHnd.LoadIdentMapEntriesIndex(Strings : TStrings; Items : array of TIdentMapEntry);
{{
Carrega os valores do atributo nome dos elementos dado por items colocando o indice do mapeamento em Objects da
TStrings passada.

Para apenas adcionar os valores veja LoadIdentMapEntries.

Revision: 7/4/2006 - Roger
}
var
    i : integer;
begin
    Strings.BeginUpdate;
    try
        Strings.Clear;
        for i := Low(Items) to High(Items) do begin
            Strings.AddObject(Items[i].Name, TObject(i));
        end;
    finally
        Strings.EndUpdate;
    end;
end;

procedure TNamedObjectList.Add(const AName : string; Obj : TObject);
begin
    Self.FList.AddObject(AName, Obj);
end;

constructor TNamedObjectList.Create(AOwnedObjects : boolean = False);
{{
Inicializa a lista interna de objetos e impede a inserção de items duplicados.

Revision: 17/4/2006 - Roger  
}
begin
    inherited Create;
    Self.FList := THashedStringList.Create;
    Self.FList.Duplicates := dupError;
end;

destructor TNamedObjectList.Destroy;
{{
Destrutor que libera a lista de objetos caso OwnedObjects = true.

Revision: 17/4/2006 - Roger  
}
var
    i : integer;
begin
    if (Self.FOwnedObjects) then begin
        for i := 0 to Self.FList.Count - 1 do begin
            Self.FList.Objects[i].Free;
        end;
    end;
    inherited;
end;

procedure TNamedObjectList.Extract(const AName : string);
{{
Extrai o objeto referenciado pela lista sem destrui-lo.

Revision: 17/4/2006 - Roger
}
var
    p : integer;
begin
    p := Self.FList.IndexOf(AName);
    if (p > 0) then begin
        Self.FList.Delete(p);
    end;
end;

function TNamedObjectList.GetCount : integer;
begin
    Result := Self.FList.Count;
end;

function TNamedObjectList.GetItems(Index : integer) : TObject;
{{
Retorna o objeto indexado por index.

Revision: 17/4/2006 - Roger
}
begin
    Result := Self.FList.Objects[Index];
end;

function TNamedObjectList.GetNames(Index : integer) : string;
{{
Retorna o nome do obejto indexado por Index.

Revision: 17/4/2006 - Roger  
}
begin
    Result := Self.FList.Strings[Index];
end;

function TNamedObjectList.GetObjects(const Name : string) : TObject;
{{
Retorna a referencia do objeto referenciado por seu nome na lista.

Revision: 17/4/2006 - Roger  
}
var
    p : integer;
begin
    p := Self.FList.IndexOf(Name);
    if (p >= 0) then begin
        Result := Self.FList.Objects[p];
    end else begin
        Result := nil;
    end;
end;

procedure TNamedObjectList.Remove(const AName : string);
{{
Elimina o elemento pelo seu nome.
Caso OwnedObjects = true o destroi tambem.

Revision: 17/4/2006 - Roger  
}
var
    item : TObject;
begin
    if Self.FOwnedObjects then begin
        item := Self.Objects[AName];
        if (Assigned(item)) then begin
            item.Free;
        end;
    end;
end;

end.
