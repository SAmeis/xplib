{$IFDEF XPLists}
  {$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I XPLib.inc}

unit XPLists;
{{
Handles with Lists and containers.
}

interface

uses
    Classes, SysUtils, Windows, IniFiles;

type
 TXPThreadStringList = class(TObject)
 {{
 Sub class of TStringList threaded safe.
 }
 private
  FList: TStringList;
  FLock: TRTLCriticalSection;
  function GetCaseSensitive: Boolean;
  procedure SetCaseSensitive(const Value: Boolean);
  function GetCount: Integer;
  function GetDuplicates: TDuplicates;
  procedure SetDuplicates(const Value: TDuplicates);
  function GetSorted: Boolean;
  procedure SetSorted(const Value: Boolean);
 public
  constructor Create;
  destructor Destroy; override;
  function Add(const S : String): Integer;
  function AddObject(const S : String; AObject : TObject): Integer;
  procedure Clear;
  procedure CustomSort(Compare : TStringListSortCompare); virtual;
  procedure Delete(Index : Integer);
  procedure Exchange(Index1, Index2 : Integer);
  function Find(const S : String; var Index : Integer): Boolean;
  function IndexOf(const S : String): Integer;
  procedure Insert(Index : Integer; const S : String);
  procedure InsertObject(Index : Integer; const S : String; AObject : TObject);
  function LockList: TStringList;
  procedure Sort; virtual;
  procedure UnlockList;
  property CaseSensitive: Boolean read GetCaseSensitive write SetCaseSensitive;
  {{
  Idem TStrings
  }
  {1 Idem TStrings }
  property Count: Integer read GetCount;
  {{
  Idem TStrings
  }
  {1 Idem TStrings }
  property Duplicates: TDuplicates read GetDuplicates write SetDuplicates;
  {{
  Idem TStrings
  }
  {1 Idem TStrings }
  property Sorted: Boolean read GetSorted write SetSorted;
  {{
  Idem TStrings
  }
  {1 Idem TStrings }
 end;
 
type
 TMemoryPropertiesList = class(TObject)
 private
  FDefaults: TMemoryPropertiesList;
  FEOL: string;
  FValues: THashedStringList;
 public
  constructor Create;
  destructor Destroy; override;
  procedure Clear;
  function GetValue(const Name : string; const DefaultValue : string = ''): string;
  {1 Realiza a busca na hash table interna. }
  procedure Load(const Filename : string); overload;
  procedure Load(Stream : TStream); overload;
  {1 Carrega os valores por streamer }
  procedure Save(const Filename : string); overload;
  procedure Save(Stream : TStream); overload;
  procedure SetValue(const Name, Value : string);
  property Defaults: TMemoryPropertiesList read FDefaults write FDefaults;
  property EOL: string read FEOL write FEOL;
 end;
 

implementation

uses
    StrHnd, Str_Pas;

{-**********************************************************************
************************************************************************
******************
******************  Class:    TXPThreadStringList
******************  Category: No category
******************
************************************************************************
************************************************************************}
{{
Sub class of TStringList threaded safe.
}
{--------------------------------------------------------------------------------------------------------------------------------}
function TXPThreadStringList.GetCaseSensitive: Boolean;
{{
Returns if StringList is case sensitive
}
begin
 Self.LockList;
 try
     Result := Self.FList.CaseSensitive;
 finally
     Self.UnlockList;
 end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPThreadStringList.SetCaseSensitive(const Value: Boolean);
{{
Sets the StringList case sensitive
}
begin
 Self.LockList;
 try
     Self.FList.CaseSensitive := Value;
 finally
     Self.UnlockList;
 end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TXPThreadStringList.GetCount: Integer;
{{
Returns if StringList count
}
begin
 Result := Self.FList.Count;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TXPThreadStringList.GetDuplicates: TDuplicates;
{{
Returns if StringList duplicates
}
begin
 Self.LockList;
 try
     Result := Self.FList.Duplicates;
 finally
     Self.UnlockList;
 end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPThreadStringList.SetDuplicates(const Value: TDuplicates);
{{
Sets the StringList duplicates
}
begin
 Self.LockList;
 try
     Self.FList.Duplicates := Value;
 finally
     Self.UnlockList;
 end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TXPThreadStringList.GetSorted: Boolean;
{{
Returns if StringList sorted.
}
begin
 Self.LockList;
 try
     Result := Self.FList.Sorted;
 finally
     Self.UnlockList;
 end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPThreadStringList.SetSorted(const Value: Boolean);
{{
Sets the StringList sorted.
}
begin
 Self.LockList;
 try
     Self.FList.Sorted := Value;
 finally
     Self.UnlockList;
 end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TXPThreadStringList.Add(const S : String): Integer;
{{
Adds a string to the internal list
}
begin
 Self.LockList;
 try
     Result := Self.FList.Add(S);
 finally
     Self.UnlockList;
 end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TXPThreadStringList.AddObject(const S : String; AObject : TObject): Integer;
{{
Adds an Object and a string associated.
}
begin
 Self.LockList;
 try
     Result := Self.FList.AddObject(S, AObject);
 finally
     Self.UnlockList;
 end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPThreadStringList.Clear;
{{
Clear the internal StringList.
}
begin
 Self.LockList;
 try
     Self.FList.Clear();
 finally
     Self.UnlockList;
 end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
constructor TXPThreadStringList.Create;
{{
Constructor of TXPThreadStringList, this class has internally a CriticalSection and a TStringList.
}
begin
 inherited Create;
 InitializeCriticalSection(FLock);
 FList := TStringList.Create;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPThreadStringList.CustomSort(Compare : TStringListSortCompare);
{{
Calls List.CustomSort() with the method passed.
}
begin
 Self.LockList;
 try
     Self.FList.CustomSort(Compare);
 finally
     Self.UnlockList;
 end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPThreadStringList.Delete(Index : Integer);
{{
Deletes the string with the index.
}
begin
 Self.LockList;
 try
     Self.FList.Delete(Index);
 finally
     Self.UnlockList;
 end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
destructor TXPThreadStringList.Destroy;
{{
Destructor of TXPThreadStringList, this class has internally a CriticalSection and a TStringList that are freed here.
}
begin
 Self.LockList();    // Certeza q todos os threads fora da lista
 try
     FList.Free;
 finally
     Self.UnlockList();
     DeleteCriticalSection(FLock);
 end;
 inherited Destroy;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPThreadStringList.Exchange(Index1, Index2 : Integer);
{{
Like TSrings Exchange.
}
begin
 Self.LockList;
 try
     Self.FList.Exchange(Index1, Index2);
 finally
     Self.UnlockList;
 end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TXPThreadStringList.Find(const S : String; var Index : Integer): Boolean;
{{
Like TSrings Exchange.Find
}
begin
 Self.LockList;
 try
     Result := Self.FList.Find(S, Index);
 finally
     Self.UnlockList;
 end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TXPThreadStringList.IndexOf(const S : String): Integer;
{{
Like TSrings Exchange.IndexOf
}
begin
 Self.LockList;
 try
     Result := Self.FList.IndexOf(S);
 finally
     Self.UnlockList;
 end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPThreadStringList.Insert(Index : Integer; const S : String);
{{
Like TSrings Exchange.Insert
}
begin
 Self.LockList;
 try
     Self.FList.Insert(Index, S);
 finally
     Self.UnlockList;
 end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPThreadStringList.InsertObject(Index : Integer; const S : String; AObject : TObject);
{{
Like TSrings Exchange.InsertObject.
}
begin
 Self.LockList;
 try
     Self.FList.InsertObject(Index, S, AObject);
 finally
     Self.UnlockList;
 end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TXPThreadStringList.LockList: TStringList;
{{
Lock this instance for protected access.
}
begin
 EnterCriticalSection(FLock);
 Result := FList;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPThreadStringList.Sort;
{{
Like TSrings Sort.
}
begin
 Self.LockList;
 try
     Self.FList.Sort;
 finally
     Self.UnlockList;
 end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXPThreadStringList.UnlockList;
{{
Release this instance for access by another thread.
}
begin
 LeaveCriticalSection(FLock);
end;

{-**********************************************************************
************************************************************************
******************
******************  Class:    TMemoryPropertiesList
******************  Category: No category
******************
************************************************************************
************************************************************************}
{--------------------------------------------------------------------------------------------------------------------------------}
procedure TMemoryPropertiesList.Clear;
{{
Limpa todos pares chaves/valores
}
var
 i: Integer;
 item: PChar;
begin
 Self.FValues.BeginUpdate();
 try
     for i := Self.FValues.Count - 1 downto 0 do begin
         item := PChar(Self.FValues.Objects[i]);
         StrDispose(item);
     end;
     Self.FValues.Clear;
 finally
     Self.FValues.EndUpdate();
 end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
constructor TMemoryPropertiesList.Create;
{{
Construtor que pre seta fim de linha para StrHnd.DOS_EOL;
}
begin
 inherited Create;
 Self.FEOL := StrHnd.DOS_EOL;
 Self.FValues := THashedStringList.Create;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
destructor TMemoryPropertiesList.Destroy;
{{
Limpa lista interna antes da liberação final.
}
begin
 Self.Clear();
 inherited;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TMemoryPropertiesList.GetValue(const Name : string; const DefaultValue : string = ''): string;
{{
Realiza a busca na hash table interna.

Caso nao encontre um valor para a chave recorre a lista de valores default Self.Defaults.

Name : Nome da chave a ser localizada.

DefaultValue : Valor retornado se não for possivel

NOTAS: Ainda não existe proteção para referencia circular de Self.Defaults ( Verificar Setter para esta propriedade ).
}
{1 Realiza a busca na hash table interna. }
var
 i: Integer;
 ret: PChar;
begin
 i := Self.FValues.IndexOf(Name);
 if (i >= 0) then begin
     ret := PChar(Self.FValues.Objects[i]);
     if (ret <> NIL) then begin
         Result := ret;
     end else begin
         Result := EmptyStr;
     end;
 end else begin
     if (Self.FDefaults <> NIL) then begin
         Result := Self.FDefaults.GetValue(Name, DefaultValue);
     end else begin
         Result := DefaultValue;
     end;
 end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TMemoryPropertiesList.Load(const Filename : string);
{{
Carga por nome de arquivo chama metodo sobrecarregado Load( TStream ).

Filename : Caminho para o arquivo  a ser salvo. ( Conteudo sobrescrito ).
}
var
 Stream: TMemoryStream;
begin
 Stream := TMemoryStream.Create;
 try
     Stream.LoadFromFile(Filename);
     Self.Load(Stream);
 finally
     Stream.Free;
 end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TMemoryPropertiesList.Load(Stream : TStream);
{{
Carrega os valores por streamer

Stream : Streamer com o texto do arquivo  a ser lido.
}
{1 Carrega os valores por streamer }
var
 BufferStream: TBufferedStringStream;
 key, Line: string;
 ch: Char;
 p: Integer;
begin
 BufferStream := TBufferedStringStream.Create(Stream);
 try
	  while ( not BufferStream.EoS) do begin
		  Line := string(BufferStream.ReadLine());
		  Line := Str_Pas.AllTrim(Line);
         p := Pos('=', Line);
         ch := Str_Pas.GetIChar(Line, 1);
         if (CharInSet( ch,  [#0, '#', '!']) ) then begin
             //insercao sem valor atribuido
             Self.FValues.AddObject(Line, NIL);
             System.Continue;
         end;
         if (p > 0) then begin
             Key := Copy(Line, 1, p - 1);
             Delete(Line, 1, p);
             Self.FValues.AddObject(Key, TObject(TStrHnd.StrToPChar(Line)));
         end else begin    //insercao da chave sem o valor
             //***NOTA: Poder-se-ia lancar excessao
             Self.FValues.AddObject(Line, TObject(TStrHnd.StrToPChar(Line)));
         end;
     end;
 finally
     BufferStream.Free;
 end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TMemoryPropertiesList.Save(const Filename : string);
{{
Salva o conteudo das propiedades para o stream passado.

Filename : arquivo de destino.
}
var
 fs: TFileStream;
begin
 fs := TFileStream.Create(Filename, fmCreate or fmShareExclusive);
 try
     Self.Save(fs);
 finally
     fs.Free;
 end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TMemoryPropertiesList.Save(Stream : TStream);
{{
Salva o conteudo das propiedades para o stream passado.

Stream : Stream de destino.
}
var
 line: Integer;
 value: PChar;
 KeyValue: string;
begin
 for line := 0 to Self.FValues.Count - 1 do begin
     KeyValue := Self.FValues.Strings[line];
     value := PChar(Self.FValues.Objects[line]);
     if (value <> NIL) then begin    //Linha de comentario
         KeyValue := KeyValue + '=' + Value;
     end;
     Stream.Write(PChar(KeyValue)^, Length(KeyValue));
     Stream.Write(PChar(Self.FEOL)^, Length(Self.FEOL));
 end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TMemoryPropertiesList.SetValue(const Name, Value : string);
{{
Altera/cria valor na lista de propriedades

Name : Nome da chave.

Value : Valor associado.
}
var
 index: Integer;
 item: PChar;
begin
 index := Self.FValues.IndexOf(Name);
 if (index >= 0) then begin
     item := PChar(Self.FValues.Objects[index]);
     StrDispose(item);
     item := TStrHnd.StrToPChar(Value);
     Self.FValues.Objects[index] := Pointer(item);
 end else begin
     item := TStrHnd.StrToPChar(Value);
     Self.FValues.AddObject(Name, Pointer(item));
 end;
end;

end.


