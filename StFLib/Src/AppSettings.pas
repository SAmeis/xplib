{$IFDEF AppSettings}
	 {$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I StFLib.inc}

unit AppSettings;

{{
A interface IBDEStartupSettings foi removida para unit BDEHnd.
}


{ TODO -oroger -clib : ListSubKeys(); para todas as variações, exceto registro está incluindo ou a própria chave ou todas as chaves
que tenham o nome iniciando com o valor dado, corrigir a unit AppSettings com urgencia }


interface

uses
    Classes, IniFiles, SyncObjs, SysUtils, Windows, WinReg32, XMLIntf, XMLDoc;

const
    APP_SETTINGS_FRAMEWORK_VERSION  = '1.0.0.0';
    APP_SETTINGS_DEFAULT_ENTRYNAME  = '@';
    APP_SETTINGS_EXTENSION_FILE_INI = '.ini';

type
    TDefaultSettingValue = class(TPersistent)
    private
        FUsed :  boolean;
        FValue : variant;
        function GetAsBoolean : boolean;
        procedure SetAsBoolean(const AValue : boolean);
        function GetAsDateTime : TDateTime;
        procedure SetAsDateTime(const AValue : TDateTime);
        function GetAsFloat : double;
        procedure SetAsFloat(const AValue : double);
        function GetAsInteger : Integer;
        procedure SetAsInteger(const AValue : Integer);
        function GetAsString : string;
        procedure SetAsString(const AValue : string);
        function GetValue : variant;
        procedure SetValue(const AValue : variant);
    public
        constructor Create; overload;
        constructor Create(AValue : variant); overload;
        property AsBoolean : boolean read GetAsBoolean write SetAsBoolean;
        property AsDateTime : TDateTime read GetAsDateTime write SetAsDateTime;
        property AsFloat : double read GetAsFloat write SetAsFloat;
        property AsInteger : Integer read GetAsInteger write SetAsInteger;
        property AsString : string read GetAsString write SetAsString;
        property Used : boolean read FUsed;
        property Value : variant read GetValue write SetValue;
    end;


     {TODO -oroger -clib : Incorporar os métodos SysUtils.FindCmdLineSwitch implementando a coleta de valores de variaveis
     com os tokens ":" ou "=" para coletar os valores após o identificador }
    TBaseSettings = class(TCriticalSection)
 {{
 Denominador comum para todos os tipos de armazenagem desejáveis:

 IniFiles, Registry e XML.
 
 Esta classe é a ancestral para os tipos de configurações gerais de um aplicativo padrão Digitalsys:
 
 TBaseGlobalSettings
 TBaseLocalSettings
 TBaseStartSettings
 TBaseUserPreferences
 TBaseUserSettings
 }
    private
        FAutoCreate :   boolean;
        FDefaultValue : TDefaultSettingValue;
        FKeyPrefix :    string;
        procedure SetKeyPrefix(const Value : string);
        procedure SetAutoCreate(const Value : boolean); virtual;
    protected
        FRefCount : Integer;
        function QueryInterface(const IID : TGUID; out Obj) : HResult; stdcall;
        procedure RaiseEntryNotFound(const FullEntry : string);
        procedure RaiseKeyNotFound(const FullKey : string);
        class procedure SplitNames(const FullName : string; var KeyPath : string; var EntryName : string);
        function _AddRef : Integer; stdcall;
        function _Release : Integer; stdcall;
    public
        constructor Create(const AKeyPrefix : string = ''); virtual;
        procedure AfterConstruction; override;
        procedure BeforeDestruction; override;
        class function DOMOwnerComponent : TComponent;
        procedure EraseKey(const Name : string); virtual; abstract;
        procedure EraseValue(const Name : string); virtual; abstract;
        function GetVersion : string; virtual;
        function KeyExists(const Name : string) : boolean; virtual; abstract;
        procedure ListSubKeys(const Name : string; SubKeys : TStrings); virtual; abstract;
        procedure ListValuesNames(const Name : string; Values : TStrings); virtual; abstract;
        procedure Import(Source : TBaseSettings; const SourceKeyName, DestKeyName : string; Recursive : boolean);
        class function NewInstance : TObject; override;
        function ReadBinary(const Name : string; Stream : TStream) : Integer; virtual; abstract;
        function ReadBoolean(const Name : string; ADefaultValue : TDefaultSettingValue = nil) : boolean; virtual; abstract;
        function ReadBooleanDefault(const Name : string; ADefaultValue : boolean = False) : boolean; virtual;
        function ReadDateTime(const Name : string; ADefaultValue : TDefaultSettingValue = nil) : TDateTime; virtual; abstract;
        function ReadFloat(const Name : string; ADefaultValue : TDefaultSettingValue = nil) : double; virtual; abstract;
        function ReadFloatDefault(const Name : string; ADefaultValue : double = 0) : double; virtual;
        function ReadInteger(const Name : string; ADefaultValue : TDefaultSettingValue = nil) : Integer; virtual; abstract;
        function ReadIntegerDefault(const Name : string; ADefaultValue : Integer = 0) : Integer; virtual;
        function ReadString(const Name : string; ADefaultValue : TDefaultSettingValue = nil) : string; virtual; abstract;
        function ReadStringDefault(const Name : string; const ADefaultValue : string = '') : string; virtual;
        function ValueExists(const Name : string) : boolean; virtual; abstract;
        function WriteBinary(const Name : string; BinStream : TStream; HowMany : Integer) : Integer; virtual; abstract;
        procedure WriteBoolean(const Name : string; Value : boolean); virtual; abstract;
        procedure WriteDateTime(const Name : string; Value : TDateTime); virtual; abstract;
        procedure WriteFloat(const Name : string; Value : double); virtual; abstract;
        procedure WriteInteger(const Name : string; Value : Integer); virtual; abstract;
        procedure WriteString(const Name : string; Value : string); virtual; abstract;
        property AutoCreate : boolean read FAutoCreate write SetAutoCreate;
        property DefaultValue : TDefaultSettingValue read FDefaultValue;
        property KeyPrefix : string read FKeyPrefix write SetKeyPrefix;
        property RefCount : Integer read FRefCount;
    end;


    TXMLBasedSettings = class(TBaseSettings)
    private
        FRootNode : IXMLNode;
    protected
        function GetAttributeValue(const FullName : string; VarType : Word; ADefaultValue : TDefaultSettingValue = nil) : variant;
        function OpenNode(const FullName : string; CanCreate : boolean) : IXMLNode;
        procedure SetAttributeValue(const FullName : string; Value : variant);
        procedure SetRootNode(const Value : IXMLNode);
        property RootNode : IXMLNode read FRootNode write SetRootNode;
    public
        constructor Create(ARootNode : IXMLNode); reintroduce; overload; virtual;
        destructor Destroy; override;
        class function CreateEmptyXMLFile(const Filename : string) : TXMLDocument;
        class function CreateFromFile(const Filename, RootNodeName : string) : TXMLBasedSettings; virtual;
        procedure EraseKey(const Name : string); override;
        procedure EraseValue(const Name : string); override;
        function KeyExists(const Name : string) : boolean; override;
        procedure ListSubKeys(const Name : string; SubKeys : TStrings); override;
        procedure ListValuesNames(const Name : string; Values : TStrings); override;
        class procedure LoadEmptyXMLDocumentContent(Stream : TStream);
        function ReadBinary(const Name : string; Stream : TStream) : Integer; override;
        function ReadBoolean(const Name : string; ADefaultValue : TDefaultSettingValue = nil) : boolean; override;
        function ReadDateTime(const Name : string; ADefaultValue : TDefaultSettingValue = nil) : TDateTime; override;
        function ReadFloat(const Name : string; ADefaultValue : TDefaultSettingValue = nil) : double; override;
        function ReadInteger(const Name : string; ADefaultValue : TDefaultSettingValue = nil) : Integer; override;
        function ReadString(const Name : string; ADefaultValue : TDefaultSettingValue = nil) : string; override;
        procedure Refresh;
        procedure Update;
        function ValueExists(const Name : string) : boolean; override;
        function WriteBinary(const Name : string; BinStream : TStream; HowMany : Integer) : Integer; override;
        procedure WriteBoolean(const Name : string; Value : boolean); override;
        procedure WriteDateTime(const Name : string; Value : TDateTime); override;
        procedure WriteFloat(const Name : string; Value : double); override;
        procedure WriteInteger(const Name : string; Value : Integer); override;
        procedure WriteString(const Name : string; Value : string); override;
    end;


    TBaseUserSettings = class(TXMLBasedSettings)
 {{
 Classe de acesso as configuracoes forçadas pelo sistema ajustadas pelo administrador/instalador.
 Um exemplo seria os direitos de acesso a determinada operação e/ou carga de módulo funcional.
 A persistencia destas configurações são mantidas em um documento XML, o qual pode ser baixado de um local comum a todos os
 usuários por exemplo da base de dados imediamente após o login do operador.
 }
    public
        function GetVersion : string; override;
    end;


    TBaseGlobalSettings = class(TXMLBasedSettings)
 {{
 Classe de acesso as configuracoes globais impostas pelo sistema.
 Por exemplo o valor padrão para um determinado campo, uma lista de feriados da unidade de negocio, etc.
 Seus dados são acessados mediante um documento XML baixado pelo aplicativo de um local comum a todos os usuários( DB por exemplo ).
 }
    public
        function GetVersion : string; override;
    end;


    TRegistryBasedSettings = class(TBaseSettings)
    protected
        FReg : TRegistryNT;
    public
        constructor Create(const AKeyPrefix : string = ''); override;
        destructor Destroy; override;
        procedure EraseKey(const Name : string); override;
        procedure EraseValue(const Name : string); override;
        function KeyExists(const Name : string) : boolean; override;
        procedure ListSubKeys(const Name : string; SubKeys : TStrings); override;
        procedure ListValuesNames(const Name : string; Values : TStrings); override;
        function ReadBinary(const Name : string; Stream : TStream) : Integer; override;
        function ReadBoolean(const Name : string; ADefaultValue : TDefaultSettingValue = nil) : boolean; override;
        function ReadDateTime(const Name : string; ADefaultValue : TDefaultSettingValue = nil) : TDateTime; override;
        function ReadFloat(const Name : string; ADefaultValue : TDefaultSettingValue = nil) : double; override;
        function ReadInteger(const Name : string; ADefaultValue : TDefaultSettingValue = nil) : Integer; override;
        function ReadString(const Name : string; ADefaultValue : TDefaultSettingValue = nil) : string; override;
        function ValueExists(const Name : string) : boolean; override;
        function WriteBinary(const Name : string; BinStream : TStream; HowMany : Integer) : Integer; override;
        procedure WriteBoolean(const Name : string; Value : boolean); override;
        procedure WriteDateTime(const Name : string; Value : TDateTime); override;
        procedure WriteFloat(const Name : string; Value : double); override;
        procedure WriteInteger(const Name : string; Value : Integer); override;
        procedure WriteString(const Name : string; Value : string); override;
    end;


    TBaseUserPreferences = class(TRegistryBasedSettings)
 {{
 Classe de acesso as configuracoes de preferencias do usuário.
 Um exemplo seria a corzinha daquela área da janela, etc.
 A persistencia destas configurações são mantidas no registro do computador, preferencialmente na chave HKCU\...
 }
    public
        constructor Create(const AKeyPrefix : string = ''); override;
    end;


    TBaseLocalSettings = class(TRegistryBasedSettings)
 {{
 Classe de acesso as configuracoes locais gerenciadas pelo operador e/ou administrador/instalador.
 Um exemplo seria se este aplicativo pode ou não ter multiplas instancias neste computador.
 A persistencia destas configurações são mantidas no registro da máquina local, preferencialmente na chave HKLM\...
 }
    public
        constructor Create(const AKeyPrefix : string = ''); override;
    end;


    TBaseStartSettings = class(TBaseSettings)
 {{
 Classe de acesso as configuracoes de inicialização do aplicativo ajustadas pelo administrador/instalador.
 Um exemplo seria os detalhes de conexão com o servidor de banco de dados.
 A persistencia destas configurações são mantidas em um arquivo do tipo INI, preferencialmente no mesmo caminho do aplicativo.
 }
    protected
        FIni : TCustomIniFile;
    public
        constructor Create; reintroduce; overload;
        constructor Create(const FileName : string; const AKeyPrefix : string = ''); reintroduce; overload; virtual;
        destructor Destroy; override;
        procedure EraseKey(const Name : string); override;
        procedure EraseValue(const Name : string); override;
        function KeyExists(const Name : string) : boolean; override;
        procedure ListSubKeys(const Name : string; SubKeys : TStrings); override;
        procedure ListValuesNames(const Name : string; Values : TStrings); override;
        function ReadBinary(const Name : string; Stream : TStream) : Integer; override;
        function ReadBoolean(const Name : string; ADefaultValue : TDefaultSettingValue = nil) : boolean; override;
        function ReadDateTime(const Name : string; ADefaultValue : TDefaultSettingValue = nil) : TDateTime; override;
        function ReadFloat(const Name : string; ADefaultValue : TDefaultSettingValue = nil) : double; override;
        function ReadInteger(const Name : string; ADefaultValue : TDefaultSettingValue = nil) : Integer; override;
        function ReadString(const Name : string; ADefaultValue : TDefaultSettingValue = nil) : string; override;
        procedure Refresh;
        procedure Update;
        function ValueExists(const Name : string) : boolean; override;
        function WriteBinary(const Name : string; BinStream : TStream; HowMany : Integer) : Integer; override;
        procedure WriteBoolean(const Name : string; Value : boolean); override;
        procedure WriteDateTime(const Name : string; Value : TDateTime); override;
        procedure WriteFloat(const Name : string; Value : double); override;
        procedure WriteInteger(const Name : string; Value : Integer); override;
        procedure WriteString(const Name : string; Value : string); override;
    end;

    EConfigException = class(Exception)
    end;

implementation

uses
    ActiveX, FileHnd, Registry, Str_Pas, StreamHnd, Variants, StrHnd;


const
    EMPTY_CFG = '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'#13 +
        //'<!DOCTYPE ConfigDocument>'#13 +
        '<ConfigDocument>'#13 +
        '</ConfigDocument>'#13;

var
    InternalDOMOwnerComponent : TComponent = nil;

{-**********************************************************************
************************************************************************
******************
******************  Class:    TBaseUserPreferences
******************  Category: No category
******************
************************************************************************
************************************************************************}
{--------------------------------------------------------------------------------------------------------------------------------}
constructor TBaseUserPreferences.Create(const AKeyPrefix : string = '');
{{
Construtor de acesso as configuracoes de preferencias do usuário.
Um exemplo seria a corzinha daquela área da janela, etc.
A persistencia destas configurações são mantidas no registro do computador, preferencialmente na chave HKCU\...

AKeyPrefix : Prefixo a ser usado antes de todos os caminhos passados para as chamadas de acesso aos elementos de configuracao.

IMPORTANTE : Caso este valor seja não vazio sempre finalizar com "\" para que os valores sejam montados corretamente.
}
begin
    inherited;
    if (AKeyPrefix = EmptyStr) then begin
        Self.FKeyPrefix := 'HKEY_CURRENT_USER\';
    end else begin
        Self.KeyPrefix := AKeyPrefix; //Garantir final com \
    end;
end;


{-**********************************************************************
************************************************************************
******************
******************  Class:    TRegistryBasedSettings
******************  Category: No category
******************
************************************************************************
************************************************************************}
{--------------------------------------------------------------------------------------------------------------------------------}
constructor TRegistryBasedSettings.Create(const AKeyPrefix : string = '');
{{
Classe de acesso as configuracoes de preferencias do usuário.
Um exemplo seria a corzinha daquela área da janela, etc.
A persistencia destas configurações são mantidas no registro do computador, preferencialmente na chave HKCU\...

AKeyPrefix : Prefixo a ser usado antes de todos os caminhos passados para as chamadas de acesso aos elementos de configuracao.

IMPORTANTE : Caso este valor seja não vazio sempre finalizar com "\" para que os valores sejam montados corretamente.
}
begin
    inherited;
    Self.FReg := TRegistryNT.Create;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
destructor TRegistryBasedSettings.Destroy;
begin
    Self.FReg.Free;
    inherited;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TRegistryBasedSettings.EraseKey(const Name : string);
{{
Name. Nome da chave a ser apagada. Apaga ela e seus filhos tambem.
}
begin
    Self.FReg.DeleteFullSubKeys(Self.FKeyPrefix + Name);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TRegistryBasedSettings.EraseValue(const Name : string);
{{
Apaga entrada com o nome desejado
Name : Caminho da entrada
}
begin
    Self.FReg.DeleteFullValue(Self.FKeyPrefix + Name);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TRegistryBasedSettings.KeyExists(const Name : string) : boolean;
{{
Name. Nome da chave a ser verificada

Returns:
Verdadeiro se esta chave existe, Falso caso contrario.
Cuidado para o caso do acesso a essa chave ser negado e o resultado retorne valor enganoso.
}
begin
    Result := Self.FReg.FullKeyExists(Self.FKeyPrefix + Name);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TRegistryBasedSettings.ListSubKeys(const Name : string; SubKeys : TStrings);
{{
Name : Nome da chave a ter suas sub-chaves carregadas.
SubKeys : TStrings na qual será a lista de nomes das sub-chaves.

Nota: As sub-chaves sem permissão de acesso podem não ser listadas. Subkeys será "appendada" é missão do chamador passá-la vazia se
desejavel.
}
begin
    if (Self.FReg.OpenFullKey(Self.FKeyPrefix + Name, False)) then begin
        Self.FReg.GetKeyNames(SubKeys);
    end else begin
        SubKeys.Clear;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TRegistryBasedSettings.ListValuesNames(const Name : string; Values : TStrings);
{{
Name : Nome da chave da qual se deseja os nomes dos valores contidos.
Values : TStrings na qual serão "appendados" os nomes dos valores contidos nesta chave.

Nota: Valores sem permissão de acesso podem ser omitidos.
}
begin
    if (Self.FReg.OpenFullKey(Self.FKeyPrefix + Name, False)) then begin
        Self.FReg.GetValueNames(Values);
    end else begin
        Values.Clear();
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TRegistryBasedSettings.ReadBinary(const Name : string; Stream : TStream) : Integer;
{{
Name : Nome composto da chave + nome do valor a ser lido para o Streamer dado.

Stream : Descendente de TStream, no qual será carregado a totalidade do conteúdo dos dados contidos na entrada. A posição do stream
não será resetada nesta operação nem o mesmo será retornado a posicao original.

Returns: Quantidade de bytes repassados para o streamer. -1 se houver falha na leitura.

NOTAS: O ponteiro do streamer ficara posicionado em seu final. Nao existe valor padrao para Streams
}
var
    Buffer : Pointer;
    key :    string;
    entry :  string;
begin
    SplitNames(Self.FKeyPrefix + Name, key, entry);
    if (Self.FReg.OpenFullKey(key, False)) then begin
        Result := Self.FReg.GetDataSize(entry);
        if (Result > 0) then begin
            GetMem(Buffer, Result);
            try
                Self.FReg.ReadBinaryData(entry, Buffer^, Result);
                Result := Stream.Write(Buffer^, Result);
            finally
                FreeMem(Buffer);
            end;
        end;
    end else begin
        Result := -1;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TRegistryBasedSettings.ReadBoolean(const Name : string; ADefaultValue : TDefaultSettingValue = nil) : boolean;
{{
Name : Nome da entrada a ser lida.

ADefaultValue : Referencia para TDefaultSettingValue a ser usado se a entrada nao puder ser lida. Este mesmo valor sera usado se
AutoCreate for habilitado. Se nil o da instancia será usado. Apos a chamada se o valor retornado foi o contido em ADefaultValue o
atributo sera marcado como usado. Ver TDefaultSettingValue para maiores detalhes.

Returns: Valor booleano contido na entrada. Para caso de falha de leitura o valor padrao sera FALSO !

NOTAS: EConfigException sera elevada se erro ocorrer.
Todas as implementacoes de Readxxx de TRegistryBasedSettings sao identicas em estrutura.
}
var
    key :    string;
    entry :  string;
    defVal : TDefaultSettingValue;
    Fail :   boolean;
begin
    try
        Fail := not Self.FReg.ReadFullBool(Self.FKeyPrefix + Name, Result); //Tenta leitura direta
    except
        Fail := True;
    end;
    if Fail then begin

        //Inicia tentativa de uso do valor padrao
        if (ADefaultValue <> nil) then begin
            defVal := ADefaultValue;
        end else begin
            defVal := Self.FDefaultValue;
        end;

        Self.SplitNames(Self.FKeyPrefix + Name, key, entry);
        if (Self.FReg.OpenFullKey(key, Self.FAutoCreate)) then begin  //Tenta acessar a chave
            //Acessa o valor
            if (Self.FReg.ValueExists(entry)) then begin
                Result := Self.FReg.ReadBool(entry);  //Leitura do tipo requerido
            end else begin //Valor NotExists
                if (defVal.FValue <> Null) then begin
                    Result := defVal.AsBoolean;  //Valor padrao repassado
                    if (Self.FAutoCreate) then begin
                        Self.WriteBoolean(Name, Result);  //Escrita do valor padrao
                    end;
                end else begin
                    Self.RaiseEntryNotFound(Self.FKeyPrefix + Name);
                end;
            end;
        end else begin //Chave notExists
            if (defVal.FValue = Null) then begin
                Self.RaiseKeyNotFound(key);
            end else begin
                Result := defVal.AsBoolean;  //Repasse do valor padrao
            end;
        end;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TRegistryBasedSettings.ReadDateTime(const Name : string; ADefaultValue : TDefaultSettingValue = nil) : TDateTime;
{{
Name : Nome da entrada a ser lida.

ADefaultValue : Referencia para TDefaultSettingValue a ser usado se a entrada nao puder ser lida. Este mesmo valor sera usado se
AutoCreate for habilitado. Se nil o da instancia será usado. Apos a chamada se o valor retornado foi o contido em ADefaultValue o
atributo sera marcado como usado. Ver TDefaultSettingValue para maiores detalhes.

Returns: Valor da date do tipo TDateTime.

NOTAS: EConfigException sera elevada se falha na leitura.
Todas as implementacoes de Readxxx de TRegistryBasedSettings sao identicas em estrutura.
}
var
    key :    string;
    entry :  string;
    defVal : TDefaultSettingValue;
    Fail :   boolean;
begin
    try
        Fail := not Self.FReg.ReadFullDateTime(Self.FKeyPrefix + Name, Result); //Tenta leitura direta
    except
        Fail := True;
    end;
    if Fail then begin

        //Inicia tentativa de uso do valor padrao
        if (ADefaultValue <> nil) then begin
            defVal := ADefaultValue;
        end else begin
            defVal := Self.FDefaultValue;
        end;

        Self.SplitNames(Self.FKeyPrefix + Name, key, entry);
        if (Self.FReg.OpenFullKey(key, Self.FAutoCreate)) then begin  //Tenta acessar a chave
            //Acessa o valor
            if (Self.FReg.ValueExists(Entry)) then begin
                Result := Self.FReg.ReadDateTime(Entry);  //Leitura do tipo requerido
            end else begin //Valor NotExists
                if (defVal.FValue <> Null) then begin
                    Result := defVal.AsDateTime;  //Valor padrao repassado
                    if (Self.FAutoCreate) then begin
                        Self.WriteDateTime(Name, Result);  //Escrita do valor padrao
                    end;
                end else begin
                    Self.RaiseEntryNotFound(Self.FKeyPrefix + Name);
                end;
            end;
        end else begin //Chave notExists
            if (defVal.FValue = Null) then begin
                Self.RaiseKeyNotFound(key);
            end else begin
                Result := defVal.AsDateTime;  //Repasse do valor padrao
            end;
        end;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TRegistryBasedSettings.ReadFloat(const Name : string; ADefaultValue : TDefaultSettingValue = nil) : double;
{{
Name : Nome do valor a ser lido.

ADefaultValue : Referencia para TDefaultSettingValue a ser usado se a entrada nao puder ser lida. Este mesmo valor sera usado se
AutoCreate for habilitado. Se nil o da instancia será usado. Apos a chamada se o valor retornado foi o contido em ADefaultValue o
atributo sera marcado como usado. Ver TDefaultSettingValue para maiores detalhes.

Returns: Valor da entrada.

NOTAS: EConfigException sera elevada se falha na leitura.
Todas as implementacoes de Readxxx de TRegistryBasedSettings sao identicas em estrutura.
}
var
    key :    string;
    entry :  string;
    defVal : TDefaultSettingValue;
    Fail :   boolean;
begin
    try
        Fail := not Self.FReg.ReadFullFloat(Self.FKeyPrefix + Name, Result); //Tenta leitura direta
    except
        Fail := True;
    end;
    if Fail then begin

        //Inicia tentativa de uso do valor padrao
        if (ADefaultValue <> nil) then begin
            defVal := ADefaultValue;
        end else begin
            defVal := Self.FDefaultValue;
        end;

        Self.SplitNames(Self.FKeyPrefix + Name, key, entry);
        if (Self.FReg.OpenFullKey(key, Self.FAutoCreate)) then begin  //Tenta acessar a chave
            //Acessa o valor
            if (Self.FReg.ValueExists(Entry)) then begin
                Result := Self.FReg.ReadFloat(Entry);  //Leitura do tipo requerido
            end else begin //Valor NotExists
                if (defVal.FValue <> Null) then begin
                    Result := defVal.AsFloat;  //Valor padrao repassado
                    if (Self.FAutoCreate) then begin
                        Self.FReg.AccessMode := Windows.KEY_WRITE;
                        Self.FReg.WriteFloat(entry, Result);  //Escrita do valor padrao
                    end;
                end else begin
                    Self.RaiseEntryNotFound(Self.FKeyPrefix + Name);
                end;
            end;
        end else begin //Chave notExists
            if (defVal.FValue = Null) then begin
                Self.RaiseKeyNotFound(key);
            end else begin
                Result := defVal.AsFloat;  //Repasse do valor padrao
            end;
        end;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TRegistryBasedSettings.ReadInteger(const Name : string; ADefaultValue : TDefaultSettingValue = nil) : Integer;
{{
Name : Nome do valor a ser lido.

ADefaultValue : Referencia para TDefaultSettingValue a ser usado se a entrada nao puder ser lida. Este mesmo valor sera usado se
AutoCreate for habilitado. Se nil o da instancia será usado. Apos a chamada se o valor retornado foi o contido em ADefaultValue o
atributo sera marcado como usado. Ver TDefaultSettingValue para maiores detalhes.

Returns: Valor da entrada.

NOTAS: EConfigException sera elevada se falha na leitura.
Todas as implementacoes de Readxxx de TRegistryBasedSettings sao identicas em estrutura.
}
var
    key :    string;
    entry :  string;
    defVal : TDefaultSettingValue;
    Fail :   boolean;
begin
    try
        Fail := not Self.FReg.ReadFullInteger(Self.FKeyPrefix + Name, Result); //Tenta leitura direta
    except
        Fail := True;
    end;
    if Fail then begin

        //Inicia tentativa de uso do valor padrao
        if (ADefaultValue <> nil) then begin
            defVal := ADefaultValue;
        end else begin
            defVal := Self.FDefaultValue;
        end;

        Self.SplitNames(Self.FKeyPrefix + Name, key, entry);
        if (Self.FReg.OpenFullKey(key, Self.FAutoCreate)) then begin  //Tenta acessar a chave
            //Acessa o valor
            if (Self.FReg.ValueExists(Entry)) then begin
                Result := Self.FReg.ReadInteger(Entry);  //Leitura do tipo requerido
            end else begin //Valor NotExists
                if (defVal.FValue <> Null) then begin
                    Result := defVal.AsInteger;  //Valor padrao repassado
                    if (Self.FAutoCreate) then begin
                        Self.FReg.AccessMode := Windows.KEY_WRITE;
                        Self.FReg.WriteInteger(entry, Result);  //Escrita do valor padrao
                    end;
                end else begin
                    Self.RaiseEntryNotFound(Self.FKeyPrefix + Name);
                end;
            end;
        end else begin //Chave notExists
            if (defVal.FValue = Null) then begin
                Self.RaiseKeyNotFound(key);
            end else begin
                Result := defVal.AsInteger;  //Repasse do valor padrao
            end;
        end;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TRegistryBasedSettings.ReadString(const Name : string; ADefaultValue : TDefaultSettingValue = nil) : string;
{{
Name : Nome da entrada a ser lida.

ADefaultValue : Referencia para TDefaultSettingValue a ser usado se a entrada nao puder ser lida. Este mesmo valor sera usado se
AutoCreate for habilitado. Se nil o da instancia será usado. Apos a chamada se o valor retornado foi o contido em ADefaultValue o
atributo sera marcado como usado. Ver TDefaultSettingValue para maiores detalhes.

Returns: String contida na entrada.

NOTAS: EConfigException sera elevada se falha na leitura.
Todas as implementacoes de Readxxx de TRegistryBasedSettings sao identicas em estrutura.
}
var
    key :    string;
    entry :  string;
    defVal : TDefaultSettingValue;
    Fail :   boolean;
begin
    try
        Fail := not Self.FReg.ReadFullString(Self.FKeyPrefix + Name, Result); //Tenta leitura direta
    except
        Fail := True;
    end;
    if Fail then begin

        //Inicia tentativa de uso do valor padrao
        if (ADefaultValue <> nil) then begin
            defVal := ADefaultValue;
        end else begin
            defVal := Self.FDefaultValue;
        end;

        Self.SplitNames(Self.FKeyPrefix + Name, key, entry);
        if (Self.FReg.OpenFullKey(key, Self.FAutoCreate)) then begin  //Tenta acessar a chave
            //Acessa o valor
            if (Self.FReg.ValueExists(Entry)) then begin
                Result := Self.FReg.ReadString(Entry);  //Leitura do tipo requerido
            end else begin //Valor NotExists
                if (defVal.FValue <> Null) then begin
                    Result := defVal.AsString;  //Valor padrao repassado
                    if (Self.FAutoCreate) then begin
                        Self.WriteString(Name, Result); //Escrita do valor padrao
                    end;
                end else begin
                    Self.RaiseEntryNotFound(Self.FKeyPrefix + Name);
                end;
            end;
        end else begin //Chave notExists
            if (defVal.FValue = Null) then begin
                Self.RaiseKeyNotFound(key);
            end else begin
                Result := defVal.AsString;  //Repasse do valor padrao
            end;
        end;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TRegistryBasedSettings.ValueExists(const Name : string) : boolean;
{{
Name : Nome da entrada a ser testada.

Returns: Verdadeiro se o valor for encontrado, Falso caso contrário. Se as permissões falhem no acesso a essa entrada.
}
begin
    Result := Self.FReg.FullValueExists(Self.FKeyPrefix + Name);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TRegistryBasedSettings.WriteBinary(const Name : string; BinStream : TStream; HowMany : Integer) : Integer;
{{
Name : Nome da entrada ser escrita.

BinStream : TStreamer do qual serão lidos os bytes a serem gravados

HowMany : quantidade de bytes a serem gravados.( -1 ) Implica em todos da posicao corrente ao final do streamer. Outro valor da 
posicao corrente ate o valor passado.

Returns: Quantidade de bytes efetivamente salvos. (-1) Se houver erro.

NOTAS:
O Streamer ficara posicionado no ponto final de sua leitura.
}
var
    Buffer :  Pointer;
    BufSize : Integer;
begin
    { TODO -oRoger -cLIB : Testar rotina }
    if (HowMany >= 0) then begin
        BufSize := HowMany;
    end else begin
        BufSize := BinStream.Size - BinStream.Position;
    end;
    GetMem(Buffer, BufSize);
    try
        Result := BinStream.Read(Buffer^, BufSize);
        Self.FReg.WriteFullBinaryData(Self.FKeyPrefix + Name, Buffer^, BufSize);
    finally
        FreeMem(Buffer);
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TRegistryBasedSettings.WriteBoolean(const Name : string; Value : boolean);
{{
Name : Nome da entrada a ser escrita.

Value : Valor booleano a ser escrito.
}
begin
    Self.FReg.WriteFullBool(Self.FKeyPrefix + Name, Value, True);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TRegistryBasedSettings.WriteDateTime(const Name : string; Value : TDateTime);
{{
Name : Nome da entrada a ser escrita.

Value : Valor do tipo TDateTime a ser escrito.
}
begin
    Self.FReg.WriteFullDateTime(Self.FKeyPrefix + Name, Value, True);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TRegistryBasedSettings.WriteFloat(const Name : string; Value : double);
{{
Name : Nome da entrada a ser escrita.

Value : Valor do tipo Double a ser escrito.
}
begin
    Self.FReg.WriteFullFloat(Self.FKeyPrefix + Name, Value, True);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TRegistryBasedSettings.WriteInteger(const Name : string; Value : Integer);
{{
Name : Nome da entrada a ser escrita.

Value : Valor inteiro a escrito.
}
begin
    Self.FReg.WriteFullInteger(Self.FKeyPrefix + Name, Value, True);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TRegistryBasedSettings.WriteString(const Name : string; Value : string);
{{
Name : Nome da entrada a ser escrita.

Value : Valor do string a ser escrita.
}
begin
    Self.FReg.WriteFullString(Self.FKeyPrefix + Name, Value, True);
end;


{-**********************************************************************
************************************************************************
******************
******************  Class:    TBaseStartSettings
******************  Category: No category
******************
************************************************************************
************************************************************************}
{--------------------------------------------------------------------------------------------------------------------------------}
constructor TBaseStartSettings.Create;
{{
Esse construtor que nao recebe o nome do arquivo de acesso deve ser gereado em memoria. Isso implica funcionalidade alegorica, pois
nao havera persistencia entre os tempos de vida.
}
begin
    inherited Create;
    Self.FIni := TMemIniFile.Create('nul');
end;

{--------------------------------------------------------------------------------------------------------------------------------}
constructor TBaseStartSettings.Create(const FileName : string; const AKeyPrefix : string = '');
{{
Esse construtor recebe FileName para acesso aos dados persistentes desta classe
}
begin
    inherited Create;
    Self.KeyPrefix := AKeyPrefix;//Garantir final com \
    Self.FIni      := TIniFile.Create(FileName);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
destructor TBaseStartSettings.Destroy;
{{
Destrutor da classe.

NOTA: Se alterações pendentes deve-se chamar Self.Update() antes de destruir esta instancia( Bug Win9x )
}
begin
    Self.FIni.Free;
    inherited;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TBaseStartSettings.EraseKey(const Name : string);
{{
Name : Nome da chave a ser apagada. Apaga ela e seus filhos tambem, Assim os nomes das secoes que iniciam com o Name passado serao
apagadas.
}
var
    i :    Integer;
    List : TStringList;
    SectionPrefix : string;
begin
    SectionPrefix := Self.FKeyPrefix + Name;
    List := TStringList.Create;
    try
        Self.FIni.ReadSections(List);
        for i := List.Count - 1 downto 0 do begin   //Apaga a todas que se iniciam com SectionPrefix
            if (Pos(SectionPrefix, List.Strings[i]) = 1) then begin
                Self.FIni.EraseSection(List.Strings[i]);
            end;
        end;
    finally
        List.Free;
    end;
    Self.FIni.EraseSection(SectionPrefix); //Apaga a si mesma ? isso eh feito acima de toda a forma ?
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TBaseStartSettings.EraseValue(const Name : string);
{{
Apaga entrada com o nome desejado
Name : Caminho da entrada
}
var
    KeyPath :   string;
    EntryName : string;
begin
    SplitNames(Name, KeyPath, EntryName);
    Self.FIni.DeleteKey(KeyPath, EntryName);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TBaseStartSettings.KeyExists(const Name : string) : boolean;
{{
Name : Nome da chave a ser verificada

Returns:
Verdadeiro se esta chave existe, Falso caso contrario.
Cuidado para o caso do acesso a essa chave ser negado e o resultado retorne valor enganoso.
}
begin
    if Name = EmptyStr then begin
        Result := False;
    end else begin
        Result := Self.FIni.SectionExists(Self.FKeyPrefix + Name);
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TBaseStartSettings.ListSubKeys(const Name : string; SubKeys : TStrings);
{{
Name : Nome da chave a ter suas sub-chaves carregadas.
SubKeys : TStrings na qual será a lista de nomes das sub-chaves.

Notas: As sub-chaves sem permissão de acesso podem não ser listadas. Caso Name vazio o raiz sera usado.
}
var
    i, BasePos : Integer;
    CollectorList : TStringList;
    List : TStringList;
    SectionPrefix, BaseKey : string;
begin
    if Name = EmptyStr then begin
        SectionPrefix := Self.FKeyPrefix + PathDelim;
    end else begin
        SectionPrefix := Self.FKeyPrefix + Name;
    end;
    List := TStringList.Create;
    try
        Self.FIni.ReadSections(List);
        SubKeys.BeginUpdate();
        try
            SubKeys.Clear;
            if SectionPrefix = PathDelim then begin
                //Carrega todos removendo duplicados
                CollectorList := TStringList.Create;
                try
                    CollectorList.Sorted     := True;
                    CollectorList.Duplicates := dupIgnore;
                    for i := 0 to List.Count - 1 do begin
                        CollectorList.Add(Str_Pas.GetDelimitedSubStr(PathDelim, List.Strings[i], 0));
                    end;
                    SubKeys.AddStrings(CollectorList); //repassa valores coletados
                finally
                    CollectorList.Free
                end;
            end else begin
                for i := 0 to List.Count - 1 do begin
                    if (Pos(SectionPrefix, List.Strings[i]) = 1) then begin
                        BaseKey := Copy(List.Strings[i], Length(SectionPrefix) + 2, Length(List.Strings[i]));
                        //remove caminho de busca pai
                        BaseKey := Str_Pas.GetDelimitedSubStr(PathDelim, BaseKey, 0); //descarta subcaminhos caso existam
                        if ((SubKeys.IndexOf(BaseKey) < 0) and (BaseKey <> EmptyStr)) then begin
                            //evita duplicidade e ela mesma
                            SubKeys.Add(BaseKey);
                        end;
                    end;
                end;
            end;
        finally
            SubKeys.EndUpdate;
        end;
    finally
        List.Free;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TBaseStartSettings.ListValuesNames(const Name : string; Values : TStrings);
{{
Name : Nome da chave da qual se deseja os nomes dos valores contidos.
Values : TStrings na qual serão adcionados os nomes dos valores contidos nesta chave.

Nota: Valores sem permissão de acesso podem ser omitidos.

Revision: 16/3/2006 - Roger
Removido bug no qual sempre se retornava um conjunto vazio.
}
var
    List : TStringList;
begin
    List := TStringList.Create;
    try
        Self.FIni.ReadSection(Self.FKeyPrefix + Name, Values);
        Values.AddStrings(List);
    finally
        List.Free;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TBaseStartSettings.ReadBinary(const Name : string; Stream : TStream) : Integer;
{{
Name : Nome composto da chave + nome do valor a ser lido para o Streamer dado.

Stream : Descendente de TStream, no qual será carregado a totalidade do conteúdo dos dados contidos na entrada. A posição do stream
não será resetada nesta operação nem o mesmo será retornado a posicao original.

Returns: Quantidade de bytes repassados para o streamer.
}
var
    Key :   string;
    Entry : string;
begin
    SplitNames(Self.FKeyPrefix + Name, key, entry);
    Result := Self.FIni.ReadBinaryStream(key, entry, Stream);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TBaseStartSettings.ReadBoolean(const Name : string; ADefaultValue : TDefaultSettingValue = nil) : boolean;
{{
Name : Nome da entrada a ser lida.

ADefaultValue : TDefaultSettingValue contendo o valor padrao para falha de leitura. Caso ADefaultValue = nil, o Default será o da
instancia. Se o valor retornado foi obtido pelo valor padrao seu atributo Used sera verdadeiro. Ver TDefaultSettingValue para mais
detalhes.

Returns: Valor booleano contido na entrada.

NOTAS: O valor padrao para falha de leitura especificamente na entrada sera FALSE.
As estrutura dos metodos TBaseStartSettings.Readxxx sao identicas
}
var
    Key :    string;
    Entry :  string;
    DefVal : TDefaultSettingValue;
begin
    Result := False;
    SplitNames(Self.FKeyPrefix + Name, key, entry);
    if (Self.FIni.ValueExists(key, entry)) then begin
        Result := Self.FIni.ReadBool(key, entry, False);
    end else begin

        //Definir valor padrao a ser usado
        if (ADefaultValue <> nil) then begin
            DefVal := ADefaultValue;
        end else begin
            DefVal := Self.FDefaultValue;
        end;

        if (DefVal.FValue <> Null) then begin
            Result := DefVal.AsBoolean;
            if (Self.FAutoCreate) then begin
                Self.FIni.WriteBool(Key, Entry, Result);
            end;
        end else begin
            Self.RaiseEntryNotFound(Self.FKeyPrefix + Name);
        end;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TBaseStartSettings.ReadDateTime(const Name : string; ADefaultValue : TDefaultSettingValue = nil) : TDateTime;
{{
Name : Nome da entrada a ser lida.

ADefaultValue : TDefaultSettingValue contendo o valor padrao para falha de leitura. Caso ADefaultValue = nil, o Default será o da
instancia. Se o valor retornado foi obtido pelo valor padrao seu atributo Used sera verdadeiro. Ver TDefaultSettingValue para mais
detalhes.

Returns: Valor da date do tipo TDateTime.

NOTAS: O valor padrao para falha de leitura sera -1.
As estrutura dos metodos TBaseStartSettings.Readxxx sao identicas
}
var
    Key :    string;
    Entry :  string;
    DefVal : TDefaultSettingValue;
begin
    Result := -1;
    SplitNames(Self.FKeyPrefix + Name, key, entry);
    if (Self.FIni.ValueExists(key, entry)) then begin
        Result := Self.FIni.ReadDateTime(key, entry, -1);
    end else begin

        //Definir valor padrao a ser usado
        if (ADefaultValue <> nil) then begin
            DefVal := ADefaultValue;
        end else begin
            DefVal := Self.FDefaultValue;
        end;

        if (DefVal.FValue <> Null) then begin
            Result := DefVal.AsDateTime;
            if (Self.FAutoCreate) then begin
                Self.FIni.WriteDateTime(Key, Entry, Result);
            end;
        end else begin
            Self.RaiseEntryNotFound(Self.FKeyPrefix + Name);
        end;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TBaseStartSettings.ReadFloat(const Name : string; ADefaultValue : TDefaultSettingValue = nil) : double;
{{
Name : Nome do valor a ser lido.

ADefaultValue : TDefaultSettingValue contendo o valor padrao para falha de leitura. Caso ADefaultValue = nil, o Default será o da
instancia. Se o valor retornado foi obtido pelo valor padrao seu atributo Used sera verdadeiro. Ver TDefaultSettingValue para mais
detalhes.

Returns: Valor da entrada.

NOTAS: O valor padrao para falha de leitura sera -1.
As estrutura dos metodos TBaseStartSettings.Readxxx sao identicas
}
var
    Key :    string;
    Entry :  string;
    DefVal : TDefaultSettingValue;
begin
    Result := -1;
    SplitNames(Self.FKeyPrefix + Name, key, entry);
    if (Self.FIni.ValueExists(key, entry)) then begin
        Result := Self.FIni.ReadFloat(key, entry, -1);
    end else begin

        //Definir valor padrao a ser usado
        if (ADefaultValue <> nil) then begin
            DefVal := ADefaultValue;
        end else begin
            DefVal := Self.FDefaultValue;
        end;

        if (DefVal.FValue <> Null) then begin
            Result := DefVal.AsFloat;
            if (Self.FAutoCreate) then begin
                Self.FIni.WriteFloat(Key, Entry, Result);
            end;
        end else begin
            Self.RaiseEntryNotFound(Self.FKeyPrefix + Name);
        end;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TBaseStartSettings.ReadInteger(const Name : string; ADefaultValue : TDefaultSettingValue = nil) : Integer;
{{
Name : Nome do valor a ser lido.

ADefaultValue : TDefaultSettingValue contendo o valor padrao para falha de leitura. Caso ADefaultValue = nil, o Default será o da
instancia. Se o valor retornado foi obtido pelo valor padrao seu atributo Used sera verdadeiro. Ver TDefaultSettingValue para mais
detalhes.

Returns: Valor da entrada.

NOTAS: O valor padrao para falha de leitura sera -1.
As estrutura dos metodos TBaseStartSettings.Readxxx sao identicas
}
var
    Key :    string;
    Entry :  string;
    DefVal : TDefaultSettingValue;
begin
    Result := -1;
    SplitNames(Self.FKeyPrefix + Name, key, entry);
    if (Self.FIni.ValueExists(key, entry)) then begin
        Result := Self.FIni.ReadInteger(key, entry, -1);
    end else begin

        //Definir valor padrao a ser usado
        if (ADefaultValue <> nil) then begin
            DefVal := ADefaultValue;
        end else begin
            DefVal := Self.FDefaultValue;
        end;

        if (DefVal.FValue <> Null) then begin
            Result := DefVal.AsInteger;
            if (Self.FAutoCreate) then begin
                Self.FIni.WriteInteger(Key, Entry, Result);
            end;
        end else begin
            Self.RaiseEntryNotFound(Self.FKeyPrefix + Name);
        end;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TBaseStartSettings.ReadString(const Name : string; ADefaultValue : TDefaultSettingValue = nil) : string;
{{
Name : Nome da entrada a ser lida.

ADefaultValue : TDefaultSettingValue contendo o valor padrao para falha de leitura. Caso ADefaultValue = nil, o Default será o da
instancia. Se o valor retornado foi obtido pelo valor padrao seu atributo Used sera verdadeiro. Ver TDefaultSettingValue para mais
detalhes.

Caso o valor padrão seja utilizado e a instância tenha o atributo AutoCreate setado, o valor padrão será persistido, dispensando o
recalculo, sendo sempre usado no futuro. Caso contrário o valor padrão será sempre recalculado e retornado.

Returns: String contida na entrada.

NOTAS: O valor padrao para falha de leitura sera EmptyStr.
As estrutura dos metodos TBaseStartSettings.Readxxx sao identicas
}
var
    Key :    string;
    Entry :  string;
    DefVal : TDefaultSettingValue;
begin
    Result := EmptyStr;
    SplitNames(Self.FKeyPrefix + Name, key, entry);
    if (Self.FIni.ValueExists(key, entry)) then begin
        Result := Self.FIni.ReadString(key, entry, EmptyStr);
    end else begin

        //Definir valor padrao a ser usado
        if (ADefaultValue <> nil) then begin
            DefVal := ADefaultValue;
        end else begin
            DefVal := Self.FDefaultValue;
        end;

        if (DefVal.FValue <> Null) then begin
            Result := DefVal.AsString;
            if (Self.FAutoCreate and DefVal.Used) then begin //Persiste valor caso autocreate e used apenas
                Self.FIni.WriteString(Key, Entry, Result);
            end;
        end else begin
            Self.RaiseEntryNotFound(Self.FKeyPrefix + Name);
        end;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TBaseStartSettings.Refresh;
{{
Recarrega os dados da origem. Para persistencia em arquivo esta sera re-lido, descartando as alteracoes ainda em cache do S.O.
}
var
    FName : string;
begin
    if (Self.FIni is TIniFile) then begin //Recria o TIniFile descartando alteracoes pendentes
        FName := TIniFile(Self.FIni).FileName;
        Self.FIni.Free;
        Self.FIni := TIniFile.Create(FName);
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TBaseStartSettings.Update;
{{
Descarrega as alteracoes pendentes.
}
begin
    Self.FIni.UpdateFile;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TBaseStartSettings.ValueExists(const Name : string) : boolean;
{{
Name : Nome da entrada a ser testada.

Returns: Verdadeiro se o valor for encontrado, Falso caso contrário. Se as permissões falhem no acesso a essa entrada.
}
var
    Key :   string;
    Entry : string;
begin
    SplitNames(Self.FKeyPrefix + Name, key, entry);
    Result := Self.FIni.ValueExists(key, entry);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TBaseStartSettings.WriteBinary(const Name : string; BinStream : TStream; HowMany : Integer) : Integer;
{{
Name : Nome da entrada ser lisda.

BinStream : TStreamer do qual serão lidos os bytes a serem gravados

HowMany : quantidade de bytes a serem gravados( -1 ) todos até o final do streamer.

Returns: Quantidade de bytes efetivamente salvos.
}
var
    Key :     string;
    Entry :   string;
    TempStream : TMemoryStream;
    BufSize : Integer;
begin
    SplitNames(Self.FKeyPrefix + Name, key, entry);
    if (HowMany >= 0) then begin
        BufSize := HowMany;
    end else begin
        BufSize := BinStream.Size - BinStream.Position;
    end;
    TempStream := TMemoryStream.Create;
    try
        Result := TStreamHnd.ReadStream(BinStream, TempStream, BufSize);
        Self.FIni.WriteBinaryStream(key, entry, TempStream);
    finally
        TempStream.Free;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TBaseStartSettings.WriteBoolean(const Name : string; Value : boolean);
{{
Name : Nome da entrada a ser escrita.

Value : Valor booleano a ser escrito.
}
var
    key :   string;
    entry : string;
begin
    SplitNames(Self.FKeyPrefix + Name, key, entry);
    Self.FIni.WriteBool(key, entry, Value);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TBaseStartSettings.WriteDateTime(const Name : string; Value : TDateTime);
{{
Name : Nome da entrada a ser escrita.

Value : Valor do tipo TDateTime a ser escrito.
}
var
    key :   string;
    entry : string;
begin
    SplitNames(Self.FKeyPrefix + Name, key, entry);
    Self.FIni.WriteDateTime(key, entry, Value);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TBaseStartSettings.WriteFloat(const Name : string; Value : double);
{{
Name : Nome da entrada a ser escrita.

Value : Valor do tipo Double a ser escrito.
}
var
    key :   string;
    entry : string;
begin
    SplitNames(Self.FKeyPrefix + Name, key, entry);
    Self.FIni.WriteFloat(key, entry, Value);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TBaseStartSettings.WriteInteger(const Name : string; Value : Integer);
{{
Name : Nome da entrada a ser escrita.

Value : Valor inteiro a escrito.
}
var
    key :   string;
    entry : string;
begin
    SplitNames(Self.FKeyPrefix + Name, key, entry);
    Self.FIni.WriteInteger(key, entry, Value);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TBaseStartSettings.WriteString(const Name : string; Value : string);
{{
Name : Nome da entrada a ser escrita.

Value : Valor do string a ser escrita.
}
var
    key :   string;
    entry : string;
begin
    SplitNames(Self.FKeyPrefix + Name, key, entry);
    Self.FIni.WriteString(key, entry, Value);
end;



{-**********************************************************************
************************************************************************
******************
******************  Class:    TBaseLocalSettings
******************  Category: No category
******************
************************************************************************
************************************************************************}
{--------------------------------------------------------------------------------------------------------------------------------}
constructor TBaseLocalSettings.Create(const AKeyPrefix : string = '');
{{
Classe de acesso as configuracoes locais gerenciadas pelo operador e/ou administrador/instalador.
Um exemplo seria se este aplicativo pode ou não ter multiplas instancias neste computador.
A persistencia destas configurações são mantidas no registro da máquina local, preferencialmente na chave HKLM\...

AKeyPrefix : Prefixo a ser usado antes de todos os caminhos passados para as chamadas de acesso aos elementos de configuracao.

IMPORTANTE : Caso este valor seja não vazio sempre finalizar com "\" para que os valores sejam montados corretamente.
}
begin
    inherited;
    if (AKeyPrefix = EmptyStr) then begin
        Self.FKeyPrefix := 'HKEY_LOCAL_MACHINE\';
    end else begin
        Self.KeyPrefix := AKeyPrefix; //Garantir final com \
    end;
end;


{-**********************************************************************
************************************************************************
******************
******************  Class:    TBaseSettings
******************  Category: No category
******************
************************************************************************
************************************************************************}
{--------------------------------------------------------------------------------------------------------------------------------}
procedure TBaseSettings.SetKeyPrefix(const Value : string);
begin
    if (Value <> EmptyStr) then begin
        if (not TStrHnd.endsWith(Value, char(PathDelim))) then begin
            FKeyPrefix := Value + PathDelim;
        end else begin
            FKeyPrefix := Value;
        end;
    end else begin
        FKeyPrefix := Value;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TBaseSettings.QueryInterface(const IID : TGUID; out Obj) : HResult;
{{
Garante a implementacao de uma interface completa a esta classe, e incrementa a contagem de referencia.

NOTA: Modificado em 22/7/2005 devido a informacao da documentacao indicando que este metodo DEVE incrementar a referencia.

Revision: 22/7/2005
}
begin
    if GetInterface(IID, Obj) then begin
        Self._AddRef();
        Result := 0;
    end else begin
        Result := E_NOINTERFACE;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TBaseSettings.RaiseEntryNotFound(const FullEntry : string);
{{
Eleva a excessão EConfigException informando que a entrada não foi encontrada.
}
begin
    raise EConfigException.CreateFmt('A entrada "%s" não foi encontrada.', [FullEntry]);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TBaseSettings.RaiseKeyNotFound(const FullKey : string);
{{
Eleva a excessão EConfigException informando que a entrada não foi encontrada.
}
begin
    raise EConfigException.CreateFmt('A chave "%s" não foi encontrada.', [FullKey]);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
class procedure TBaseSettings.SplitNames(const FullName : string; var KeyPath : string; var EntryName : string);
begin
    KeyPath := TFileHnd.SlashRem(ExtractFilePath(FullName));
    if (KeyPath = EmptyStr) then begin
        KeyPath   := FullName;
        EntryName := APP_SETTINGS_DEFAULT_ENTRYNAME;  //Valor padrao para o raiz
    end else begin
        EntryName := ExtractFileName(FullName);
        if (EntryName = EmptyStr) then begin
            EntryName := APP_SETTINGS_DEFAULT_ENTRYNAME;
        end;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TBaseSettings._AddRef : Integer;
begin
    Result := InterlockedIncrement(FRefCount);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TBaseSettings._Release : Integer;
begin
    Result := InterlockedDecrement(FRefCount);
    if Result = 0 then begin
        Destroy;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TBaseSettings.AfterConstruction;
{{
Segundo o modo de ser de TInterfacedObject: "Release the constructor's implicit refcount"
}
begin
    inherited;
    InterlockedDecrement(FRefCount);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TBaseSettings.BeforeDestruction;
begin
    inherited;
    if RefCount <> 1 then begin
        //Passa a assumir que sempre existe a referencia da 1a instancia ao inves de apenas as interfaes existentes
        System.Error(reInvalidPtr);
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
constructor TBaseSettings.Create(const AKeyPrefix : string = '');
{{
Denominador comum para todos os tipos de armazenagem desejáveis:

IniFiles, Registry e XML.

AKeyPrefix : Prefixo a ser usado antes de todos os caminhos passados para as chamadas de acesso aos elementos de configuracao.

IMPORTANTE : Caso este valor seja não vazio sempre finalizar com "\" para que os valores sejam montados corretamente.

Esta classe é a ancestral para os tipos de configurações gerais de um aplicativo padrão Digitalsys:

TBaseGlobalSettings
TBaseLocalSettings
TBaseStartSettings
TBaseUserPreferences
TBaseUserSettings

Revision: 22/7/2005
O construtor chama _AddRef de pelo fato de - se instanciou existe uma referencia

}
begin
    inherited Create;
    Self.FAutoCreate   := True;
    Self.FDefaultValue := TDefaultSettingValue.Create(EmptyStr);  //Valor padrao atribuido como vazio
    if ((AKeyPrefix <> EmptyStr) and (not TStrHnd.endsWith(AKeyPrefix, PathDelim))) then begin
        Self.KeyPrefix := AKeyPrefix + PathDelim; //Garantir final com "\" = PathDelim
    end else begin
        Self.KeyPrefix := AKeyPrefix;
    end;
    Self._AddRef(); //inserido em 2005/07/20 para incrementar referencia na 1a construcao
end;

{--------------------------------------------------------------------------------------------------------------------------------}
class function TBaseSettings.DOMOwnerComponent : TComponent;
{{
Retorna a instancia reservada para ser o container padrao para os DOM´s
}
begin
    Result := InternalDOMOwnerComponent;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
{{
procedure EraseKey (abstract).
Name. Nome da chave a ser apagada. Apaga ela e seus filhos tambem.
}

{--------------------------------------------------------------------------------------------------------------------------------}
function TBaseSettings.GetVersion : string;
{{
Retorna versao do framework de acesso a configuracoes, este metodo foi criado apenas para a classe nao ser abstrata, tentativa 5
}
begin
    Result := APP_SETTINGS_FRAMEWORK_VERSION;
end;

procedure TBaseSettings.Import(Source : TBaseSettings; const SourceKeyName, DestKeyName : string; Recursive : boolean);
var
    ValueNames, SubKeysNames : TStringList;
    x : Integer;
    tmp, tmp2 : string;
begin
    if (Recursive) then begin
        SubKeysNames := TStringList.Create;
        try
            Source.ListSubKeys(SourceKeyName, SubKeysNames);
            for x := 0 to SubKeysNames.Count - 1 do begin
                Self.Import(Source, TFileHnd.ConcatPath([SourceKeyName, SubKeysNames[x]]), TFileHnd.ConcatPath(
                    [DestKeyName, SubKeysNames[x]]), Recursive);
            end;
        finally
            SubKeysNames.Free;
        end;
    end;
    ValueNames := TStringList.Create;
    try
        Source.ListValuesNames(SourceKeyName, ValueNames);
		 for x := 0 to ValueNames.Count - 1 do begin
			 tmp  := TFileHnd.ConcatPath([Self.KeyPrefix, DestKeyName, ValueNames[x]]);
			 tmp2:=TFileHnd.ConcatPath([ Source.KeyPrefix, SourceKeyName, ValueNames[x]]);
			 tmp2 := Source.ReadString( tmp2 );
            Self.WriteString(tmp, tmp2);
        end;
    finally
        ValueNames.Free;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
{{
function KeyExists (abstract).
Name. Nome da chave a ser verificada

Returns:
Verdadeiro se esta chave existe, Falso caso contrario.
Cuidado para o caso do acesso a essa chave ser negado e o resultado retorne valor enganoso.
}

{--------------------------------------------------------------------------------------------------------------------------------}
{{
procedure ListSubKeys (abstract).

Name : Nome da chave a ter suas sub-chaves carregadas.
SubKeys : TStrings na qual será a lista de nomes das sub-chaves.

Nota: As sub-chaves sem permissão de acesso podem não ser listadas. Subkeys será "appendada" é missão do chamador passá-la vazia se
desejavel.
}

{--------------------------------------------------------------------------------------------------------------------------------}
{{
procedure ListValuesNames (abstract).
Name : Nome da chave da qual se deseja os nomes dos valores contidos.
Values : TStrings na qual serão "appendados" os nomes dos valores contidos nesta chave.

Nota: Valores sem permissão de acesso podem ser omitidos.
}

{--------------------------------------------------------------------------------------------------------------------------------}
class function TBaseSettings.NewInstance : TObject;
{{
Adjust RefCount to 1 to ensure a secure interface use this class and subclasses.
Revision:27/5/2005
}
begin
    Result := inherited NewInstance;
    TBaseSettings(Result).FRefCount := 1;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
{{
function ReadBinary (abstract).

Name : Nome composto da chave + nome do valor a ser lido para o Streamer dado.

Stream : Descendente de TStream, no qual será carregado a totalidade do conteúdo dos dados contidos na entrada. A posição do stream
não será resetada nesta operação nem o mesmo será retornado a posicao original.

Returns: Quantidade de bytes repassados para o streamer.
}

{--------------------------------------------------------------------------------------------------------------------------------}
{{
function ReadBoolean (abstract).

Name : Nome da entrada a ser lida.

Returns: Valor booleano contido na entrada.
}

{--------------------------------------------------------------------------------------------------------------------------------}
function TBaseSettings.ReadBooleanDefault(const Name : string; ADefaultValue : boolean = False) : boolean;
{{
Cria um valor default especifico para esta leitura
}
var
    defVal : TDefaultSettingValue;
begin
    defVal := TDefaultSettingValue.Create();
    try
        defVal.AsBoolean := ADefaultValue;
        Result := Self.ReadBoolean(Name, DefVal);
    finally
        defVal.Free;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
{{
function ReadDateTime (abstract).

Name : Nome da entrada a ser lida.

Returns: Valor da date do tipo TDateTime.
}


{--------------------------------------------------------------------------------------------------------------------------------}
function TBaseSettings.ReadFloatDefault(const Name : string; ADefaultValue : double = 0) : double;
{{
function ReadFloatDefault

Name : Nome da entrada a ser lida.

Returns: Valor da date do tipo Double.
}
var
    defVal : TDefaultSettingValue;
begin
    defVal := TDefaultSettingValue.Create();
    try
        defVal.AsFloat := ADefaultValue;
        Result := Self.ReadFloat(Name, DefVal);
    finally
        defVal.Free;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
{{
function ReadInteger (abstract).

Name : Nome do valor a ser lido.

Returns: Valor da entrada.
}

{--------------------------------------------------------------------------------------------------------------------------------}
function TBaseSettings.ReadIntegerDefault(const Name : string; ADefaultValue : Integer = 0) : Integer;
{{
Cria um valor default especifico para esta leitura
}
var
    defVal : TDefaultSettingValue;
begin
    defVal := TDefaultSettingValue.Create();
    try
        defVal.AsInteger := ADefaultValue;
        Result := Self.ReadInteger(Name, DefVal);
    finally
        defVal.Free;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
{{
function ReadString (abstract).

Name : Nome da entrada a ser lida.

Returns: String contida na entrada.
}

{--------------------------------------------------------------------------------------------------------------------------------}
function TBaseSettings.ReadStringDefault(const Name : string; const ADefaultValue : string = '') : string;
{{
Cria um valor default especifico para esta leitura
}
var
    defVal : TDefaultSettingValue;
begin
    defVal := TDefaultSettingValue.Create();
    try
        defVal.AsString := ADefaultValue;
        Result := Self.ReadString(Name, DefVal);
    finally
        defVal.Free;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
{{
function ValueExists (abstract).

Name : Nome da entrada a ser testada.

Returns: Verdadeiro se o valor for encontrado, Falso caso contrário. Se as permissões falhem no acesso a essa entrada.
}

{--------------------------------------------------------------------------------------------------------------------------------}
{{
procedure WriteBinary (abstract).

Name : Nome da entrada ser lisda.

BinStream : TStreamer do qual serão lidos os bytes a serem gravados

HowMany : quantidade de bytes a serem gravados( -1 ) todos até o final do streamer.

Returns: Quantidade de bytes efetivamente salvos.
}

{--------------------------------------------------------------------------------------------------------------------------------}
{{
procedure WriteBoolean (abstract).

Name : Nome da entrada a ser escrita.

Value : Valor booleano a ser escrito.
}

{--------------------------------------------------------------------------------------------------------------------------------}
{{
procedure WriteDateTime (abstract).

Name : Nome da entrada a ser escrita.

Value : Valor do tipo TDateTime a ser escrito.
}

{--------------------------------------------------------------------------------------------------------------------------------}
{{
procedure WriteInteger (abstract).

Name : Nome da entrada a ser escrita.

Value : Valor inteiro a escrito.
}

{--------------------------------------------------------------------------------------------------------------------------------}
{{
procedure WriteString (abstract).

Name : Nome da entrada a ser escrita.

Value : Valor do string a ser escrita.
}

{{
procedure EraseKey (abstract).
Name. Nome da chave a ser apagada. Apaga ela e seus filhos tambem.
}

{{
function KeyExists (abstract).
Name. Nome da chave a ser verificada

Returns:
Verdadeiro se esta chave existe, Falso caso contrario.
Cuidado para o caso do acesso a essa chave ser negado e o resultado retorne valor enganoso.
}

{{
procedure ListSubKeys (abstract).

Name : Nome da chave a ter suas sub-chaves carregadas.
SubKeys : TStrings na qual será a lista de nomes das sub-chaves.

Nota: As sub-chaves sem permissão de acesso podem não ser listadas. Subkeys será "appendada" é missão do chamador passá-la vazia se
desejavel.
}

{{
procedure ListValuesNames (abstract).
Name : Nome da chave da qual se deseja os nomes dos valores contidos.
Values : TStrings na qual serão "appendados" os nomes dos valores contidos nesta chave.

Nota: Valores sem permissão de acesso podem ser omitidos.
}

{{
function ReadBinary (abstract).

Name : Nome composto da chave + nome do valor a ser lido para o Streamer dado.

Stream : Descendente de TStream, no qual será carregado a totalidade do conteúdo dos dados contidos na entrada. A posição do stream
não será resetada nesta operação nem o mesmo será retornado a posicao original.

Returns: Quantidade de bytes repassados para o streamer.
}

{{
function ReadBoolean (abstract).

Name : Nome da entrada a ser lida.

Returns: Valor booleano contido na entrada.
}

{{
function ReadInteger (abstract).

Name : Nome do valor a ser lido.

Returns: Valor da entrada.
}

{{
function ReadString (abstract).

Name : Nome da entrada a ser lida.

Returns: String contida na entrada.
}

{{
function ValueExists (abstract).

Name : Nome da entrada a ser testada.

Returns: Verdadeiro se o valor for encontrado, Falso caso contrário. Se as permissões falhem no acesso a essa entrada.
}

{{
procedure WriteBinary (abstract).

Name : Nome da entrada ser lisda.

BinStream : TStreamer do qual serão lidos os bytes a serem gravados

HowMany : quantidade de bytes a serem gravados( -1 ) todos até o final do streamer.

Returns: Quantidade de bytes efetivamente salvos.
}

{{
procedure WriteBoolean (abstract).

Name : Nome da entrada a ser escrita.

Value : Valor booleano a ser escrito.
}

{{
procedure WriteDateTime (abstract).

Name : Nome da entrada a ser escrita.

Value : Valor do tipo TDateTime a ser escrito.
}

{{
procedure WriteInteger (abstract).

Name : Nome da entrada a ser escrita.

Value : Valor inteiro a escrito.
}

{{
procedure WriteString (abstract).

Name : Nome da entrada a ser escrita.

Value : Valor do string a ser escrita.
}

{-**********************************************************************
************************************************************************
******************
******************  Class:    TDefaultSettingValue
******************  Category: No category
******************
************************************************************************
************************************************************************}
{--------------------------------------------------------------------------------------------------------------------------------}
function TDefaultSettingValue.GetAsBoolean : boolean;
{{
Leitura do valor como boolean. Seta flag de uso para verdadeiro.
}
begin
    if (not Variants.VarIsType(Self.FValue, varBoolean)) then begin
        Self.FValue := False;
    end;
    Result := boolean(Self.FValue);
    FUsed  := True;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TDefaultSettingValue.SetAsBoolean(const AValue : boolean);
{{
Escrita do valor como boolean. Seta flag de uso para False.
}
begin
    Self.FUsed  := False;
    Self.FValue := AValue;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TDefaultSettingValue.GetAsDateTime : TDateTime;
{{
Leitura do valor como DateTime. Seta flag de uso para verdadeiro.
}
begin
    if (not Variants.VarIsType(Self.FValue, varDate)) then begin
        Self.FValue := Now();
    end;
    Result := TDateTime(Self.FValue);
    FUsed  := True;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TDefaultSettingValue.SetAsDateTime(const AValue : TDateTime);
{{
Escrita do valor como DateTime. Seta flag de uso para False.
}
begin
    Self.FUsed  := False;
    Self.FValue := AValue;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TDefaultSettingValue.GetAsFloat : double;
{{
Leitura do valor como Float. Seta flag de uso para verdadeiro.
}
begin
    if (not Variants.VarIsType(Self.FValue, varDouble)) then begin
        Self.FValue := 0;
    end;
    Result := double(Self.FValue);
    FUsed  := True;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TDefaultSettingValue.SetAsFloat(const AValue : double);
{{
Escrita do valor como Float. Seta flag de uso para False.
}
begin
    Self.FUsed  := False;
    Self.FValue := AValue;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TDefaultSettingValue.GetAsInteger : Integer;
{{
Leitura do valor como integer. Seta flag de uso para verdadeiro.
}
begin
    if (not Variants.VarIsType(Self.FValue, varInteger)) then begin
        Self.FValue := 0;
    end;
    Result := Integer(Self.FValue);
    FUsed  := True;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TDefaultSettingValue.SetAsInteger(const AValue : Integer);
{{
Escrita do valor como integer. Seta flag de uso para False.
}
begin
    Self.FUsed  := False;
    Self.FValue := AValue;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TDefaultSettingValue.GetAsString : string;
{{
Leitura do valor como string. Seta flag de uso para verdadeiro.
Revision - 20101130 - roger
Portado para Unicode, ajustando de Self.FValue := 0; para Self.FValue := '';
}
begin
    //#if (not Variants.VarIsType(Self.FValue, varString  )) then begin
     {$IF CompilerVersion >= 21.00}
    if (not Variants.VarIsType(Self.FValue, [varString, varUString])) then begin
        Self.FValue := '';
    end;
     {$ELSE}
	 if (not Variants.VarIsType(Self.FValue, [varString])) then begin
		 Self.FValue := '';
	 end;
	 {$IFEND}
    Result := string(Self.FValue);
    FUsed  := True;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TDefaultSettingValue.SetAsString(const AValue : string);
{{
Escrita do valor como string. Seta flag de uso para False.
}
begin
    Self.FUsed  := False;
    Self.FValue := AValue;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TDefaultSettingValue.GetValue : variant;
{{
Leitura do valor como variant. Seta flag de uso para verdadeiro.
}
begin
    Result := Self.FValue;
    FUsed  := True;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TDefaultSettingValue.SetValue(const AValue : variant);
{{
Escrita do valor como Variant. Seta flag de uso para False.
}
begin
    Self.FUsed  := False;
    Self.FValue := AValue;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
constructor TDefaultSettingValue.Create;
{{
Construtor de TDefaultSettingValue. Iniciar atributos Used e Value.
}
begin
    inherited;
    Self.FValue := Null;
    Self.FUsed  := False;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
constructor TDefaultSettingValue.Create(AValue : variant);
{{
Construtor de TDefaultSettingValue. Iniciar atributos Used = False e Value com o valor passado.
}
begin
    Create();
    Self.FValue := AValue;
end;


{-**********************************************************************
************************************************************************
******************
******************  Class:    TXMLBasedSettings
******************  Category: No category
******************
************************************************************************
************************************************************************}
{--------------------------------------------------------------------------------------------------------------------------------}
function TXMLBasedSettings.GetAttributeValue(const FullName : string; VarType : Word; ADefaultValue : TDefaultSettingValue = nil) :
variant;
{{
Realiza a leitura do valor do atributo

Revision - 20101130 - roger
Suporte para Unicode insere como retorno EmptyStr
}
var
    Node :   IXMLNode;
    key :    string;
    Entry :  string;
    DefVal : TDefaultSettingValue;
begin
    Self.SplitNames(FullName, key, entry);
    Node := OpenNode(key, Self.FAutoCreate);
    if (Node <> nil) then begin
        Result := Node.GetAttribute(entry);
        if (Result = Null) then begin   //Atributo nao existe.
            //Definir valor padrao para retorno
            if (ADefaultValue <> nil) then begin
                DefVal := ADefaultValue;
            end else begin
                DefVal := Self.DefaultValue;
            end;
            if (DefVal.FValue = Null) then begin  //Valor padrao invalido
                Self.RaiseEntryNotFound(FullName);
            end else begin
                if (not VarIsType(DefVal.Value, VarType)) then begin
                    //Caso o tipo informado incompativel com o solicitado -> padrão

                    case VarType of
                        varEmpty, varNull, varSmallint, varInteger, varSingle, varDouble, varCurrency, varDate : begin
                            DefVal.Value := 0;
                        end;
                        varOleStr : begin
                            DefVal.Value := EmptyStr;
                        end;
                        //varDispatch , varError Desconsiderados
                        varBoolean : begin
                            DefVal.Value := False;
                        end;
                        varVariant, varUnknown : begin
                            DefVal.Value := Null;
                        end;
                        //varDecimal  = $000E; { vt_decimal     14 } {UNSUPPORTED as of v6.x code base}
                        //varUndef0F  = $000F; { undefined      15 } {UNSUPPORTED per Microsoft}
                        varShortInt, varByte, varWord, varLongWord, varInt64 : begin
                            DefVal.Value := 0;
                        end;
                        //varWord64   = $0015; { vt_ui8         21 } {UNSUPPORTED as of v6.x code base}
                        {  if adding new items, update Variants' varLast, BaseTypeMap and OpTypeMap }
                         {$IF CompilerVersion >= 21.00}
                        varStrArg, varString, varUString : begin
                         {$ELSE}
						 varStrArg, varString : begin
						 {$IFEND}
                            DefVal.Value := EmptyStr;
                        end;
                        varAny : begin
                            DefVal.Value := Null;
                        end;
                        else begin
                            DefVal.Value := Null;
                        end;
                    end;
                end;
                //Usa o valor padrao, sendo este o original ou o padrao calculado a partir do tipo desejado
                Result := DefVal.Value;
            end;
            if (Self.FAutoCreate) then begin
                Node.SetAttribute(entry, Result);
            end;
        end else begin
            try
                Result := VarAsType(Result, VarType);
            except
                raise EConfigException.CreateFmt('Valor da entrada "%s" incompatível com o tipo.', [FullName]);
            end;
        end;
    end else begin
        if (Self.FAutoCreate) then begin
            Self.RaiseKeyNotFound(key);
        end else begin
            raise EConfigException.CreateFmt('Falha criando chave: "%s".', [key]);
        end;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TXMLBasedSettings.OpenNode(const FullName : string; CanCreate : boolean) : IXMLNode;
{{
NodePath : Caminho do no a ser retornado. Se AutoCreate verdadeiro este noh sera criado de todo o modo.
}
var
    Child : IXMLNode;
    Path : string;
    NextNode : string;
    P : Integer;
    OldOptions : TXMLDocOptions;
begin
    Result := nil;
    Path   := TFileHnd.SlashRem(FullName);
    Child  := Self.FRootNode;
    Result := Child;
    OldOptions := Self.FRootNode.OwnerDocument.Options;
    if (CanCreate) then begin
        Self.FRootNode.OwnerDocument.Options := Self.FRootNode.OwnerDocument.Options + [doNodeAutoCreate];
    end else begin
        Self.FRootNode.OwnerDocument.Options := Self.FRootNode.OwnerDocument.Options - [doNodeAutoCreate];
    end;
    try
        while (Path <> EmptyStr) and (Result <> nil) do begin

            //Busca do nome do no a ser recuperado na iteracao
            P := Pos(PathDelim, Path);
            if P > 0 then begin
                NextNode := Copy(Path, 1, P - Length(PathDelim));
                Path     := Copy(Path, p + Length(PathDelim), Length(Path));
            end else begin
                NextNode := Path;
                Path     := EmptyStr;
            end;

            //Abre o node desejado na iteracao -> Se FAutoCreate -> [ noNodeAutoCreate ]a linha abaixo cria o node sempre
            Result := Child.ChildNodes.Nodes[NextNode];
            Child  := Result;
        end;
    finally
        Self.FRootNode.OwnerDocument.Options := OldOptions;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXMLBasedSettings.SetAttributeValue(const FullName : string; Value : variant);
{{
EntryPath : Caminho da entrada a ser alterada.

Value: Valor a ser escrito nesta entrada.
}
var
    Node :  IXMLNode;
    key :   string;
    entry : string;
begin
    Self.SplitNames(FullName, key, entry);
    Node := OpenNode(Key, True); //Sempre criado
    if (Assigned(Node)) then begin
        Node.SetAttribute(entry, Value);
    end else begin
        Self.RaiseKeyNotFound(key);
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXMLBasedSettings.SetRootNode(const Value : IXMLNode);
begin
    FRootNode := Value;
    if (Self.FAutoCreate) then begin
        Self.FRootNode.OwnerDocument.Options := Self.FRootNode.OwnerDocument.Options + [doNodeAutoCreate];
    end else begin
        Self.FRootNode.OwnerDocument.Options := Self.FRootNode.OwnerDocument.Options - [doNodeAutoCreate];
    end;
    Self.FRootNode.OwnerDocument.Options := Self.FRootNode.OwnerDocument.Options + [doAutoSave];
end;

{--------------------------------------------------------------------------------------------------------------------------------}
constructor TXMLBasedSettings.Create(ARootNode : IXMLNode);
{{
Contrutor de TXMLBasedSettings para persistencia no formato XML.

ARootNode : Node XML base de acesso aos dados.
}
var
    IDoc : IXMLDocument;
begin
    inherited Create;
    if (ARootNode = nil) then begin
        raise EConfigException.Create('Instância para persistência em XML necessita de nó principal');
    end;
    Self.FRootNode := ARootNode;
    IDoc := Self.FRootNode.OwnerDocument; //Tentativa de nao limpar referencia pelas chamadas abaixo
    if (Self.AutoCreate) then begin
        IDoc.Options := IDoc.Options + [doNodeAutoCreate];
    end else begin
        IDoc.Options := IDoc.Options - [doNodeAutoCreate];
    end;
end;



 /// Cria um documento vazio para uso generico
 /// Este documento sera criado baseado em um arquivo temporario não liberado automaticamente na sua destruicao.
 /// NOTA: O Owner da instancia retornada sempre será o InternalDOMOwnerComponent para evitar erros do parser MS-XML
 /// No momento da descarga desta unit todos os documentos serão liberados, ou no momento explicitamente invocado.
 /// Revision: Roger 20120925
 /// Salvar arquivo usando TStrings de modo a converter para a codificação de forma automática
 ///
class function TXMLBasedSettings.CreateEmptyXMLFile(const Filename : string) : TXMLDocument;
var
    w : TStrings;
begin
    w := TStringList.Create;
    try
        w.Text := EMPTY_CFG;
        w.SaveToFile(Filename);
    finally
        w.Free;
    end;
    Result := TXMLDocument.Create(InternalDOMOwnerComponent);
    Result.FileName := Filename;
    Result.ParseOptions := Result.ParseOptions - [poResolveExternals, poValidateOnParse];
    Result.Active := True;
end;

class function TXMLBasedSettings.CreateFromFile(const Filename, RootNodeName : string) : TXMLBasedSettings;
var
    RootDoc :  TXMLDocument;
    IDoc :     IXMLDocument;
    RootNode : IXMLNode;
begin
    if (not FileExists(Filename)) then begin
        raise EConfigException.CreateFmt('Arquivo "%s" não encontrado.', [Filename]);
    end;
    RootDoc := TXMLDocument.Create(InternalDOMOwnerComponent);
    RootDoc.Active := True;
    IDoc    := XMLDoc.LoadXMLDocument(Filename);

    if (not Assigned(IDoc.DocumentElement)) then begin
        raise EConfigException.Create('Documento sem elemento raiz');
    end;

    RootNode := IDoc.DocumentElement;
    Result   := TXMLBasedSettings.Create(RootNode);
    Result.KeyPrefix := RootNodeName;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
destructor TXMLBasedSettings.Destroy;
begin
    Self.FRootNode := nil; //Documento proprietario responde por ele
    inherited;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXMLBasedSettings.EraseKey(const Name : string);
{{
Name : Nome da chave a ser apagada. Apaga ela e seus filhos tambem.
}
var
    ParentNode : IXmlNode;
    Node : IXmlNode;
begin
    Node := Self.OpenNode(Self.FKeyPrefix + Name, False);
    if Node <> nil then begin   //Se nao existe -> Feito
        ParentNode := Node.ParentNode;
        ParentNode.ChildNodes.Delete(Node.NodeName);
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXMLBasedSettings.EraseValue(const Name : string);
{{
Apaga entrada com o nome desejado
Name : Caminho da entrada
}
var
    Node :      IXmlNode;
    KeyPath :   string;
    EntryName : string;
begin
    SplitNames(Name, KeyPath, EntryName);
    Node := Self.OpenNode(Self.FKeyPrefix + KeyPath, False);
    if Node <> nil then begin   //Se nao existe -> Feito
        Node.SetAttribute(EntryName, Variants.Null);
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TXMLBasedSettings.KeyExists(const Name : string) : boolean;
{{
Name. Nome da chave a ser verificada

Returns:
Verdadeiro se esta chave existe, Falso caso contrario.
Cuidado para o caso do acesso a essa chave ser negado e o resultado retorne valor enganoso.
}
begin
    Result := (Self.OpenNode(Self.FKeyPrefix + Name, False) <> nil);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXMLBasedSettings.ListSubKeys(const Name : string; SubKeys : TStrings);
{{
Name : Nome da chave a ter suas sub-chaves carregadas.

SubKeys : TStrings na qual será a lista de nomes das sub-chaves.

Nota: As sub-chaves sem permissão de acesso podem não ser listadas.
}
var
    Node :  IXMLNode;
    i :     Integer;
    Child : IXMLNode;
begin
    SubKeys.Clear;
    Node := Self.OpenNode(Self.FKeyPrefix + Name, Self.AutoCreate);
    if Node <> nil then begin
        if (Node.HasChildNodes) and (not Node.IsTextElement) then begin
            SubKeys.BeginUpdate;
            try
                for i := 0 to Node.ChildNodes.Count - 1 do begin
                    Child := Node.ChildNodes.Get(i);
                    SubKeys.Add(Child.NodeName);
                end;
            finally
                SubKeys.EndUpdate;
            end;
        end;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXMLBasedSettings.ListValuesNames(const Name : string; Values : TStrings);
{{
Name : Nome da chave da qual se deseja os nomes dos valores contidos.
Values : TStrings na qual serão colocados os nomes dos valores contidos nesta chave.

Nota: Valores sem permissão de acesso podem ser omitidos.
}
var
    i :    Integer;
    Node : IXMLNode;
    ListAttrs : IXMLNodeList;
begin
    Values.Clear;
    Node := OpenNode(Self.FKeyPrefix + Name, False);
    if (Node <> nil) then begin
        ListAttrs := Node.AttributeNodes;
        Values.BeginUpdate;
        try
            for i := 0 to ListAttrs.Count - 1 do begin  { TODO -oRoger -cLIB : Checar se a leitura abaixo esta correta }
                Values.Add(ListAttrs[i].NodeName);
            end;
        finally
            Values.EndUpdate;
        end;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
class procedure TXMLBasedSettings.LoadEmptyXMLDocumentContent(Stream : TStream);
{{
Carrega a partir da posicao corrente de Stream, o conteudo de um xml vazio para uso posterior.
}
var
    stm : TStringStream;
begin
    stm := TStringStream.Create(EMPTY_CFG);
    try
        Stream.CopyFrom(stm, stm.Size);
    finally
        stm.Free;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TXMLBasedSettings.ReadBinary(const Name : string; Stream : TStream) : Integer;
{{
Name : Nome composto da chave + nome do valor a ser lido para o Streamer dado.

Stream : Descendente de TStream, no qual será carregado a totalidade do conteúdo dos dados contidos na entrada. A posição do stream
não será resetada nesta operação nem o mesmo será retornado a posicao original, portanto todos os dados serao appendados ao stream.

Returns: Quantidade de bytes repassados para o streamer. (-1) se conteudo nulo ou vazio
}
var
    content :   string;
    BufLen :    Integer;
    Buffer :    Pointer;
    oldDefVal : variant;
begin
    { TODO -oRoger -cLIB : Testar este metodo }
    Self.Acquire();  //Trava para leitura segura do conteudo
    try
        oldDefVal := Self.DefaultValue.FValue;
        Self.DefaultValue.FValue := Null;  //Erro de leitura se valor real nao recuperado.
        content   := GetAttributeValue(Self.FKeyPrefix + Name, varSmallint);
    finally
        Self.DefaultValue.FValue := oldDefVal;
        Self.Release;
    end;
    if content <> EmptyStr then begin

        BufLen := 2 * (Length(content)) + 2;
        GetMem(Buffer, BufLen);
        try
            HexToBin(PChar(content), Buffer, BufLen);
            Result := Stream.Write(Buffer^, BufLen);
        finally
            FreeMem(Buffer);
        end;
    end else begin
        Result := -1;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TXMLBasedSettings.ReadBoolean(const Name : string; ADefaultValue : TDefaultSettingValue = nil) : boolean;
{{
Name : Nome da entrada a ser lida.

Returns: Valor booleano contido na entrada.
}
begin
    Result := GetAttributeValue(Self.FKeyPrefix + Name, varBoolean, ADefaultValue);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TXMLBasedSettings.ReadDateTime(const Name : string; ADefaultValue : TDefaultSettingValue = nil) : TDateTime;
{{
Name : Nome da entrada a ser lida.

Returns: Valor da date do tipo TDateTime.
}
begin
    Result := GetAttributeValue(Self.FKeyPrefix + Name, varDouble, ADefaultValue);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TXMLBasedSettings.ReadFloat(const Name : string; ADefaultValue : TDefaultSettingValue = nil) : double;
{{
Name : Nome do valor a ser lido.

Returns: Valor da entrada.
Revision - 2/6/2009 - roger
Tipo passado era varInteger alterado para varDouble para evitar erro de conversão.
}
begin
    Result := GetAttributeValue(Self.FKeyPrefix + Name, varDouble, ADefaultValue);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TXMLBasedSettings.ReadInteger(const Name : string; ADefaultValue : TDefaultSettingValue = nil) : Integer;
{{
Name : Nome do valor a ser lido.

Returns: Valor da entrada.
}
begin
    Result := GetAttributeValue(Self.FKeyPrefix + Name, varInteger, ADefaultValue);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TXMLBasedSettings.ReadString(const Name : string; ADefaultValue : TDefaultSettingValue = nil) : string;
{{
Name : Nome da entrada a ser lida.

Returns: String contida na entrada.
}
begin
    if (Assigned(ADefaultValue)) then begin
        //NOTA: devido a confusão do UNICODE , a opção foi usar tipo nativo do Variant
        Result := GetAttributeValue(Self.FKeyPrefix + Name, FindVarData(ADefaultValue.Value)^.VType, ADefaultValue);
    end else begin
        Result := GetAttributeValue(Self.FKeyPrefix + Name, varString, ADefaultValue);
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXMLBasedSettings.Refresh;
{{
Faz releitura do streamer xml com os dados, caso qualquer informacao tenha sido alterada serah perdida.
}
begin
    { TODO -oRoger -cLIB : Recarrega documento para ser lido no acesso dos dados }
    Self.FRootNode.OwnerDocument.Resync;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXMLBasedSettings.Update;
{{
Faz releitura do streamer xml com os dados, caso qualquer informacao tenha sido alterada serah perdida.
}
var
    OldState : TXMLDocOptions;
begin
    { TODO -oRoger -cLIB : Salva documento para ser lido no acesso dos dados }
    if (Self.FRootNode.OwnerDocument.Active) then begin
        OldState := Self.FRootNode.OwnerDocument.Options;
        Self.FRootNode.OwnerDocument.Options := Self.FRootNode.OwnerDocument.Options + [doAutoSave];
        Self.FRootNode.OwnerDocument.Active := False;
        Self.FRootNode.OwnerDocument.Active := True;
        Self.FRootNode.OwnerDocument.Options := OldState;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TXMLBasedSettings.ValueExists(const Name : string) : boolean;
{{
Name : Nome da entrada a ser testada.

Returns: Verdadeiro se o valor for encontrado, Falso caso contrário. Se as permissões falhem no acesso a essa entrada.
}
var
    Node :  IXMLNode;
    key :   string;
    entry : string;
begin
    SplitNames(Self.FKeyPrefix + Name, key, entry);
    Node := Self.OpenNode(key, False);
    if (Node <> nil) then begin
        Result := (Node.HasAttribute(entry));
    end else begin
        Result := False;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TXMLBasedSettings.WriteBinary(const Name : string; BinStream : TStream; HowMany : Integer) : Integer;
{{
Name : Nome da entrada ser lisda.

BinStream : TStreamer do qual serão lidos os bytes a serem gravados

HowMany : quantidade de bytes a serem gravados( -1 ) todos até o final do streamer.

Returns: Quantidade de bytes efetivamente salvos.

NOTAS: BinStream será avancado tantos bytes quantos forem necessarios para completar a operação.
}
var
    contentSize : int64;
    Qtd :     int64;
    Buffer :  PAnsiChar;
    content : PAnsiChar;
begin
    if (HowMany >= 0) then begin
        Qtd := HowMany;
    end else begin
        Qtd := BinStream.Size - BinStream.Position;
    end;
    GetMem(Buffer, Qtd);
    contentSize := 2 * (Qtd + 1);
    try
        Result  := BinStream.Read(Buffer^, Qtd);
        content := AnsiStrAlloc(contentSize);
        try
            BinToHex(Buffer, content, contentSize);  //Representacao Hexa do fluxo de bytes
            Self.WriteString(Self.FKeyPrefix + Name, string(content));
        finally
            StrDispose(content);
        end;
    finally
        FreeMem(Buffer);
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXMLBasedSettings.WriteBoolean(const Name : string; Value : boolean);
{{
Name : Nome da entrada a ser escrita.

Value : Valor booleano a ser escrito.
}
begin
    SetAttributeValue(Self.FKeyPrefix + Name, Value);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXMLBasedSettings.WriteDateTime(const Name : string; Value : TDateTime);
{{
Name : Nome da entrada a ser escrita.

Value : Valor do tipo TDateTime a ser escrito.
}
begin
    SetAttributeValue(Self.FKeyPrefix + Name, Value);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXMLBasedSettings.WriteFloat(const Name : string; Value : double);
{{
Name : Nome da entrada a ser escrita.

Value : Valor do tipo Double a ser escrito.
}
begin
    SetAttributeValue(Self.FKeyPrefix + Name, Value);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXMLBasedSettings.WriteInteger(const Name : string; Value : Integer);
{{
Name : Nome da entrada a ser escrita.

Value : Valor inteiro a escrito.
}
begin
    SetAttributeValue(Self.FKeyPrefix + Name, Value);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TXMLBasedSettings.WriteString(const Name : string; Value : string);
{{
Name : Nome da entrada a ser escrita.

Value : Valor do string a ser escrita.
}
begin
    SetAttributeValue(TFileHnd.ConcatPath([Self.FKeyPrefix, Name]), Value);
end;


{-**********************************************************************
************************************************************************
******************
******************  Class:    TBaseGlobalSettings
******************  Category: No category
******************
************************************************************************
************************************************************************}
{--------------------------------------------------------------------------------------------------------------------------------}
function TBaseGlobalSettings.GetVersion : string;
{{
Classe de acesso as configuracoes globais impostas pelo sistema.
Por exemplo o valor padrão para um determinado campo, uma lista de feriados da unidade de negocio, etc.
Seus dados são acessados mediante um documento XML baixado pelo aplicativo de um local comum a todos os usuários( DB por exemplo ).
}
begin
    Result := inherited GetVersion;
end;


{-**********************************************************************
************************************************************************
******************
******************  Class:    TBaseUserSettings
******************  Category: No category
******************
************************************************************************
************************************************************************}
{--------------------------------------------------------------------------------------------------------------------------------}
function TBaseUserSettings.GetVersion : string;
{{
Classe de acesso as configuracoes forçadas pelo sistema ajustadas pelo administrador/instalador.
Um exemplo seria os direitos de acesso a determinada operação e/ou carga de módulo funcional.
A persistencia destas configurações são mantidas em um documento XML, o qual pode ser baixado de um local comum a todos os usuários
por exemplo da base de dados imediamente após o login do operador.
}
begin
    Result := inherited GetVersion();
end;


procedure TBaseSettings.SetAutoCreate(const Value : boolean);
{{
TBaseSettings.SetAutoCreate
Ajusta atributo de auto criação de chaves/valores

Revision: 11/5/2009 - roger 
}
begin
    FAutoCreate := Value;
end;

initialization
    begin
        InternalDOMOwnerComponent := TComponent.Create(nil);
    end;

finalization
    begin
        FreeAndNil(InternalDOMOwnerComponent);
    end;

end.
