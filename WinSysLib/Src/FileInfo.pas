{$IFDEF FileInfo}
	{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I WinSysLib.inc}

unit FileInfo;

interface

uses
    SysUtils, Windows, Classes, Super;

type
    PVerTranslation = ^TVerTranslation;

    TVerTranslation = record
        Language: Word;
        CharSet:  Word;
    end;
    TNumberVersion = record
        case Tag: Integer of
            0: (
                Major: Word;
                Minor: Word;
                Release: Word;
                Build: Word;
            );
            1: (
                AsFloat: QUADWORD;
            );
			 2: (
				 MajorVersionEx: DWORD;
				 MinorVersionEx: DWORD;
			 );
	 end;

	 TVersionInfo = class
	 {{
	 Classe de manipulação de informação de versões abstratas que podem ter sua fonte das mais variadas origens.

	 Revision: 21/5/2007 - roger
	 }
	 private
		 FVersion : TNumberVersion;
	 public
		 constructor Create(const VersionString : string); virtual;
		 function Compares(Another : TVersionInfo) : Integer;
		 class function CompareTo(ver1, ver2 : string) : Integer;
		 class function Convert(const StrVersion : string) : TNumberVersion;
        property Version : TNumberVersion read FVersion;
    end;

    TVersionFileInfo = class(TVersionInfo)
    {{
    Classe de manipulação de versão concretas de arquivos.
    
    Revision: 21/5/2007 - roger
    }
    public
        constructor Create(FileName : PAnsiChar); reintroduce;
    end;


    TCustomFileVersionInfo = class(TComponent)
    protected {private}
        FComments :         string;
        FCompanyName :      string;
        FFileDate :         TDateTime;
        FFileDescription :  string;
        FFileFlags :        longint;
        FFileFlagsMask :    longint;
        FFileMajorVersion : longint;
        FFileMinorVersion : longint;
        FFileName :         string;
        FFileOS :           longint;
        FFileType :         longint;
        FFileSubtype :      longint;
        FFileVersion :      string;
        FFileVersionFloat : double;
        FInternalName :     string;
        FLanguageCount :    longint;
        FLanguageName :     string;
        FLegalCopyright :   string;
        FLegalTrademark :   string;
        FOriginalFilename : string;
        FProductMajorVersion : longint;
        FProductMinorVersion : longint;
        FProductName :      string;
        FProductVersion :   string;
        FProductVersionFloat : double;
        FTranslationValue : longint;
        VInfoLoaded :       boolean;

        function GetComments : string;
        function GetCompanyName : string;
        function GetFileDate : TDateTime;
        function GetFileDescription : string;
        function GetFileFlags : longint;
        function GetFileFlagsMask : longint;
        function GetFileMajorVersion : longint;
        function GetFileMinorVersion : longint;
        function GetFileOS : longint;
        function GetFileSubtype : longint;
        function GetFileType : longint;
        function GetFileVersion : string;
        function GetFileVersionFloat : double;
        function GetInternalName : string;
        function GetLanguageCount : longint;
        function GetLanguageName : string;
        function GetLegalCopyright : string;
        function GetLegalTrademark : string;
        function GetOriginalFilename : string;
        function GetProductMajorVersion : longint;
        function GetProductMinorVersion : longint;
        function GetProductName : string;
        function GetProductVersion : string;
        function GetProductVersionFloat : double;
        function GetTranslationValue : longint;
        procedure Loaded; override;
        function LoadVersionInfo(const Key : string) : string;
        procedure SetFileName(const Value : string);
    protected
        {properties}
        property Comments : string read GetComments;
        property CompanyName : string read GetCompanyName;
        property FileDate : TDateTime read GetFileDate;
        property FileDescription : string read GetFileDescription;
        property FileFlags : longint read GetFileFlags;
        property FileFlagsMask : longint read GetFileFlagsMask;
        property FileMajorVersion : longint read GetFileMajorVersion;
        property FileMinorVersion : longint read GetFileMinorVersion;
        property FileName : string read FFileName write SetFileName;
        property FileOS : longint read GetFileOS;
        property FileType : longint read GetFileType;
        property FileSubtype : longint read GetFileSubtype;
        property FileVersion : string read GetFileVersion;
        property FileVersionFloat : double read GetFileVersionFloat;
        property InternalName : string read GetInternalName;
        property LanguageCount : longint read GetLanguageCount;
        property LanguageName : string read GetLanguageName;
        property LegalCopyright : string read GetLegalCopyright;
        property LegalTrademark : string read GetLegalTrademark;
        property OriginalFilename : string read GetOriginalFilename;
        property ProductName : string read GetProductName;
        property ProductMajorVersion : longint read GetProductMajorVersion;
        property ProductMinorVersion : longint read GetProductMinorVersion;
        property ProductVersion : string read GetProductVersion;
        property ProductVersionFloat : double read GetProductVersionFloat;
        property TranslationValue : longint read GetTranslationValue;
    public
        constructor Create(AOwner : TComponent); override;
        destructor Destroy; override;
        function GetKeyValue(const Key : string) : string;
    published
        { Published declarations }
    end;


    TFileVersionInfo = class(TCustomFileVersionInfo)
    public
        {properties}
        property Comments;
        property CompanyName;
        property FileDescription;
        property FileDate;
        property FileFlags;
        property FileFlagsMask;
        property FileMajorVersion;
        property FileMinorVersion;
        property FileOS;
        property FileType;
        property FileSubtype;
        property FileVersion;
        property FileVersionFloat;
        property InternalName;
        property LanguageCount;
        property LanguageName;
        property LegalCopyright;
        property LegalTrademark;
        property OriginalFilename;
        property ProductMajorVersion;
        property ProductMinorVersion;
        property ProductName;
        property ProductVersion;
        property ProductVersionFloat;
        property TranslationValue;
    published
        {properties}
        property FileName;
    end;

function FileVersionStrToNumberVersion(const StrVer : string) : TNumberVersion;

procedure Register;

implementation


uses
    Str_Pas;

function TCustomFileVersionInfo.GetComments : string;
    {-------------------------------------------------------------------------------------------------------------}
begin
    if not VInfoLoaded then begin
        LoadVersionInfo(EmptyStr);
    end;
    Result := FComments;
end;

function TCustomFileVersionInfo.GetCompanyName : string;
    {-------------------------------------------------------------------------------------------------------------}
begin
    if not VInfoLoaded then begin
        LoadVersionInfo(EmptyStr);
    end;
    Result := FCompanyName;
end;

function TCustomFileVersionInfo.GetFileDate : TDateTime;
    {-------------------------------------------------------------------------------------------------------------}
begin
    if not VInfoLoaded then begin
        LoadVersionInfo(EmptyStr);
    end;
    Result := FFileDate;
end;


function TCustomFileVersionInfo.GetFileDescription : string;
    {-------------------------------------------------------------------------------------------------------------}
begin
    if not VInfoLoaded then begin
        LoadVersionInfo(EmptyStr);
    end;
    Result := FFileDescription;
end;

function TCustomFileVersionInfo.GetFileVersion : string;
    {-------------------------------------------------------------------------------------------------------------}
begin
    if not VInfoLoaded then begin
        LoadVersionInfo(EmptyStr);
    end;
    Result := FFileVersion;
end;

function TCustomFileVersionInfo.GetFileVersionFloat : double;
    {-------------------------------------------------------------------------------------------------------------}
begin
    if not VInfoLoaded then begin
        LoadVersionInfo(EmptyStr);
    end;
    Result := FFileVersionFloat;
end;

function TCustomFileVersionInfo.GetFileFlags : longint;
    {-------------------------------------------------------------------------------------------------------------}
begin
    if not VInfoLoaded then begin
        LoadVersionInfo(EmptyStr);
    end;
    Result := FFileFlags;
end;

function TCustomFileVersionInfo.GetFileFlagsMask : longint;
    {-------------------------------------------------------------------------------------------------------------}
begin
    if not VInfoLoaded then begin
        LoadVersionInfo(EmptyStr);
    end;
    Result := FFileFlagsMask;
end;

function TCustomFileVersionInfo.GetFileMajorVersion : longint;
    {-------------------------------------------------------------------------------------------------------------}
begin
    if not VInfoLoaded then begin
        LoadVersionInfo(EmptyStr);
    end;
    Result := FFileMajorVersion;
end;

function TCustomFileVersionInfo.GetFileMinorVersion : longint;
    {-------------------------------------------------------------------------------------------------------------}
begin
    if not VInfoLoaded then begin
        LoadVersionInfo(EmptyStr);
    end;
    Result := FFileMinorVersion;
end;

function TCustomFileVersionInfo.GetFileOS : longint;
    {-------------------------------------------------------------------------------------------------------------}
begin
    if not VInfoLoaded then begin
        LoadVersionInfo(EmptyStr);
    end;
    Result := FFileOS;
end;

function TCustomFileVersionInfo.GetFileSubtype : longint;
    {-------------------------------------------------------------------------------------------------------------}
begin
    if not VInfoLoaded then begin
        LoadVersionInfo(EmptyStr);
    end;
    Result := FFileSubtype;
end;

function TCustomFileVersionInfo.GetFileType : longint;
    {-------------------------------------------------------------------------------------------------------------}
begin
    if not VInfoLoaded then begin
        LoadVersionInfo(EmptyStr);
    end;
    Result := FFileType;
end;


function TCustomFileVersionInfo.GetInternalName : string;
    {-------------------------------------------------------------------------------------------------------------}
begin
    if not VInfoLoaded then begin
        LoadVersionInfo(EmptyStr);
    end;
    Result := FInternalName;
end;

function TCustomFileVersionInfo.GetLanguageCount : longint;
    {-------------------------------------------------------------------------------------------------------------}
begin
    if not VInfoLoaded then begin
        LoadVersionInfo(EmptyStr);
    end;
    Result := FLanguageCount;
end;

function TCustomFileVersionInfo.GetLanguageName : string;
    {-------------------------------------------------------------------------------------------------------------}
begin
    if not VInfoLoaded then begin
        LoadVersionInfo(EmptyStr);
    end;
    Result := FLanguageName;
end;

function TCustomFileVersionInfo.GetLegalCopyright : string;
    {-------------------------------------------------------------------------------------------------------------}
begin
    if not VInfoLoaded then begin
        LoadVersionInfo(EmptyStr);
    end;
    Result := FLegalCopyright;
end;

function TCustomFileVersionInfo.GetLegalTrademark : string;
    {-------------------------------------------------------------------------------------------------------------}
begin
    if not VInfoLoaded then begin
        LoadVersionInfo(EmptyStr);
    end;
    Result := FLegalTrademark;
end;

function TCustomFileVersionInfo.GetOriginalFilename : string;
    {-------------------------------------------------------------------------------------------------------------}
begin
    if not VInfoLoaded then begin
        LoadVersionInfo(EmptyStr);
    end;
    Result := FOriginalFilename;
end;

function TCustomFileVersionInfo.GetProductMajorVersion : longint;
    {-------------------------------------------------------------------------------------------------------------}
begin
    if not VInfoLoaded then begin
        LoadVersionInfo(EmptyStr);
    end;
    Result := FProductMajorVersion;
end;

function TCustomFileVersionInfo.GetProductMinorVersion : longint;
    {-------------------------------------------------------------------------------------------------------------}
begin
    if not VInfoLoaded then begin
        LoadVersionInfo(EmptyStr);
    end;
    Result := FProductMinorVersion;
end;

function TCustomFileVersionInfo.GetProductName : string;
    {-------------------------------------------------------------------------------------------------------------}
begin
    if not VInfoLoaded then begin
        LoadVersionInfo(EmptyStr);
    end;
    Result := FProductName;
end;

function TCustomFileVersionInfo.GetProductVersion : string;
    {-------------------------------------------------------------------------------------------------------------}
begin
    if not VInfoLoaded then begin
        LoadVersionInfo(EmptyStr);
    end;
    Result := FProductVersion;
end;

function TCustomFileVersionInfo.GetProductVersionFloat : double;
    {-------------------------------------------------------------------------------------------------------------}
begin
    if not VInfoLoaded then begin
        LoadVersionInfo(EmptyStr);
    end;
    Result := FProductVersionFloat;
end;

function TCustomFileVersionInfo.GetTranslationValue : longint;
    {-------------------------------------------------------------------------------------------------------------}
begin
    if not VInfoLoaded then begin
        LoadVersionInfo(EmptyStr);
    end;
    Result := FTranslationValue;
end;


procedure TCustomFileVersionInfo.Loaded;
{-------------------------------------------------------------------------------------------------------------}
begin
    inherited Loaded;
    if FFileName = EmptyStr then begin
        SetFileName(EmptyStr);
    end;
end;


function TCustomFileVersionInfo.LoadVersionInfo(const Key : string) : string;
    {-------------------------------------------------------------------------------------------------------------}
var
  {$IFDEF WIN32}
  Handle : DWORD;
  {$ELSE}
    Handle :   longint;
  {$ENDIF}
    Res :      boolean;
    Size :     Integer;
    Error :    longint;
    Data :     Pointer;
    Buffer :   Pointer;
    ErrCode :  Integer;
  {$IFDEF WIN32}
	 {$IFDEF VERSION4}
  Bytes   : Cardinal;
	 {$ELSE}
  Bytes   : cardinal;
	 {$ENDIF}
  {$ELSE}
    Bytes :    Word;
  {$ENDIF}
    TempStr :  array [0..259] of char;
    LangBuff : array [0..259] of char;
    BaseStr :  string;
    InfoStr :  string;
    Trans :    PVerTranslation;
    TrSize :   Integer;
  {$IFDEF VER80}
  FixedInfo : Tvs_FixedFileInfo;
  {$ELSE}
    FixedInfo : TVSFixedFileInfo;

  {$ENDIF}

    function MakeFloat(S : string) : double;
    var
        Buff :  array [0..5] of char;
        I :     Integer;
        Count : Integer;
    begin
        { TODO -oRoger -cLIB : Remover surgimento de excessao para esta rotina e melhorar robustez para range checking }
  {$R-}
        Count := 0;
        FillChar(Buff, SizeOf(Buff), 0);
        { The file version string might be specified like }
        { 4.72.3105.0. Parse it down to just one decimal  }
        { place and create the floating point version #.  }

        for I := 0 to Pred(Length(S)) do begin
            if (S[I] = DecimalSeparator) then begin
                { Found the first period }
                Inc(Count);
                if Count = 2 then begin
                    Move(S[1], Buff, I - 1);
                    Break;
                end;
            end;
        end;
        Result := StrToFloat(Buff);
  {$R+}
    end;

begin
    TrSize := 0;
    Size   := GetFileVersionInfoSize(StrPCopy(TempStr, FFileName), Handle);
    if Size = 0 then begin
        { In Win32 GetFileVersionInfoSize might fail because the }
        { file is a 16-bit file or because the file does not     }
        { contain version info. }
        Error := GetLastError;
        if Error = ERROR_RESOURCE_TYPE_NOT_FOUND then begin
            raise Exception.Create('Arquivo não possui recursos de informações');
            //RaiseStError(EStVersionInfoError, stscNoVerInfo);
        end;
    end;
    { Allocate some memory and get version info block. }
    GetMem(Data, Size);
    Res   := GetFileVersionInfo(TempStr, Handle, Size, Data);
    Trans := nil;
    try
        if not Res then begin
            { Error. Raise an exception. }
            raise Exception.Create('Arquivo não possui recursos de informações');
            //RaiseStError(EStVersionInfoError, stscVerInfoFail);
        end;
        { Get the translation value. We need it to get the version info. }
        Res := VerQueryValue(Data, '\VarFileInfo\Translation', Buffer, Bytes);
        if not Res then begin
            raise Exception.Create('Arquivo não possui recursos de informações');
            //RaiseStError(EStVersionInfoError, stscVerInfoFail);
        end;
        TrSize := Bytes;
        GetMem(Trans, TrSize);
        Move(Buffer^, Trans^, TrSize);
        FTranslationValue := longint(Trans^);
        FLanguageCount    := Bytes div SizeOf(TVerTranslation);
        VerLanguageName(Trans^.Language, LangBuff, SizeOf(LangBuff));
        FLanguageName := StrPas(LangBuff);
        VInfoLoaded   := True;

        { Build a base string including the translation value. }
        BaseStr := Format('StringFileInfo\%.4x%.4x\', [Trans^.Language, Trans^.CharSet]);

        { User-defined string. Get the string and exit. }
        if Key <> '' then begin
            InfoStr := BaseStr + Key;
            Res     := VerQueryValue(Data, StrPCopy(TempStr, InfoStr), Buffer, Bytes);
            if Res then begin
                Result := StrPas(PChar(Buffer));
                Exit;
            end else begin
                Result := '';
                raise Exception.Create('Arquivo recurso de informações com chave inconsistente');
                //RaiseStError(EStVersionInfoError, stscBadVerInfoKey);
            end;
        end;

        { Get the fixed version info. }
        Bytes := SizeOf(FixedInfo);
        FillChar(FixedInfo, Bytes, 0);
        { '\' is used to get the root block. }
        Res := VerQueryValue(Data, '\', Buffer, Bytes);
        if not Res then begin
            raise Exception.Create('Arquivo não possui recursos de informações');
            //RaiseStError(EStVersionInfoError, stscVerInfoFail);
        end;
        Move(Buffer^, FixedInfo, Bytes);
        with FixedInfo do begin
            FFileMajorVersion := dwFileVersionMS;
            FFileMinorVersion := dwFileVersionLS;
            FProductMajorVersion := dwProductVersionMS;
            FProductMinorVersion := dwProductVersionLS;
            FFileFlagsMask := dwFileFlagsMask;
            FFileFlags   := dwFileFlags;
            { Note: Most files don't set the binary date. }
            FFileDate    := MakeLong(dwFileDateMS, dwFileDateLS);
            FFileOS      := dwFileOS;
            FFileType    := dwFileType;
            FFileSubtype := dwFileSubtype;
        end;

        { Comments }
        InfoStr := BaseStr + 'Comments';
        Res     := VerQueryValue(Data, StrPCopy(TempStr, InfoStr), Buffer, Bytes);
        if Res and (Bytes <> 0) then begin
            FComments := StrPas(PChar(Buffer));
        end else begin
            FComments := EmptyStr;
        end;
        { CompanyName }
        InfoStr := BaseStr + 'CompanyName';
        Res     := VerQueryValue(Data, StrPCopy(TempStr, InfoStr), Buffer, Bytes);
        if Res and (Bytes <> 0) then begin
            FCompanyName := StrPas(PChar(Buffer));
        end else begin
            FCompanyName := EmptyStr;
        end;

        { FileDescription }
        InfoStr := BaseStr + 'FileDescription';
        Res     := VerQueryValue(Data, StrPCopy(TempStr, InfoStr), Buffer, Bytes);
        if Res and (Bytes <> 0) then begin
            FFileDescription := StrPas(PChar(Buffer));
        end else begin
            FFileDescription := EmptyStr;
        end;

        { FileVersion }
        InfoStr := BaseStr + 'FileVersion';
        Res     := VerQueryValue(Data, StrPCopy(TempStr, InfoStr), Buffer, Bytes);
        if Res and (Bytes <> 0) then begin
            FFileVersion := StrPas(PChar(Buffer));
            { First try to convert the version number to a float as-is. }
            Val(FFileVersion, FFileVersionFloat, ErrCode);
            if ErrCode <> 0 then begin
                { Failed. Create the float with the local MakeFloat function. }
                try
                    FFileVersionFloat := MakeFloat(StringReplace(FFileVersion, '.', DecimalSeparator, [rfReplaceAll]));
                except
                    FFileVersionFloat := 0;
                end;
            end;
        end else begin
            FFileVersion      := EmptyStr;
            FFileVersionFloat := 0;
        end;

        { InternalName }
        InfoStr := BaseStr + 'InternalName';
        Res     := VerQueryValue(Data, StrPCopy(TempStr, InfoStr), Buffer, Bytes);
        if Res and (Bytes <> 0) then begin
            FInternalName := StrPas(PChar(Buffer));
        end else begin
            FInternalName := EmptyStr;
        end;

        { LegalCopyright }
        InfoStr := BaseStr + 'LegalCopyright';
        Res     := VerQueryValue(Data, StrPCopy(TempStr, InfoStr), Buffer, Bytes);
        if Res and (Bytes <> 0) then begin
            FLegalCopyright := StrPas(PChar(Buffer));
        end else begin
            FLegalCopyright := EmptyStr;
        end;

        { LegalTrademark }
        InfoStr := BaseStr + 'LegalTrademark';
        Res     := VerQueryValue(Data, StrPCopy(TempStr, InfoStr), Buffer, Bytes);
        if Res and (Bytes <> 0) then begin
            FLegalTrademark := StrPas(PChar(Buffer));
        end else begin
            FLegalTrademark := EmptyStr;
        end;

        { OriginalFilename }
        InfoStr := BaseStr + 'OriginalFilename';
        Res     := VerQueryValue(Data, StrPCopy(TempStr, InfoStr), Buffer, Bytes);
        if Res and (Bytes <> 0) then begin
            FOriginalFilename := StrPas(PChar(Buffer));
        end else begin
            FOriginalFilename := EmptyStr;
        end;

        { ProductName }
        InfoStr := BaseStr + 'ProductName';
        Res     := VerQueryValue(Data, StrPCopy(TempStr, InfoStr), Buffer, Bytes);
        if Res and (Bytes <> 0) then begin
            FProductName := StrPas(PChar(Buffer));
        end else begin
            FProductName := EmptyStr;
        end;

        { ProductVersion }
        InfoStr := BaseStr + 'ProductVersion';
        Res     := VerQueryValue(Data, StrPCopy(TempStr, InfoStr), Buffer, Bytes);
        if Res and (Bytes <> 0) then begin
            FProductVersion := StrPas(PChar(Buffer));
            { First try to convert the product number to a float as-is. }
            Val(FProductVersion, FProductVersionFloat, ErrCode);
            if ErrCode <> 0 then begin
                { Failed. Create the float with the local MakeFloat function. }
                try
                    FProductVersionFloat := MakeFloat(StringReplace(FProductVersion, '.', DecimalSeparator, [rfReplaceAll]));
                except
                    FProductVersionFloat := 0;
                end;
            end;
        end else begin
            FProductVersion      := EmptyStr;
            FProductVersionFloat := 0;
        end;
    finally
        FreeMem(Data, Size);
        FreeMem(Trans, TrSize);
    end;
end;

procedure TCustomFileVersionInfo.SetFileName(const Value : string);
{-------------------------------------------------------------------------------------------------------------}
var
    Buff : array [0..255] of char;
begin
    if (Value <> '') and not (csDesigning in ComponentState) then begin
        if not FileExists(Value) then begin
            raise Exception.Create('Arquivo não não encontrado');
            //RaiseStError(EStVersionInfoError, stscFileOpen);
        end;
    end;
    if FFileName <> Value then begin
        VInfoLoaded := False;
    end;
    FFileName := Value;
    { If FileName is an emtpy string then load the }
    { version info for the current process.        }
    if (FFileName = EmptyStr) and not (csDesigning in ComponentState) then begin
        if GetModuleFileName(0, Buff, SizeOf(Buff)) = 0 then begin
            FFileName := EmptyStr;
        end else begin
            FFileName := StrPas(Buff);
        end;
    end;
end;


{TCustomFileVersionInfo PUBLIC}
constructor TCustomFileVersionInfo.Create(AOwner : TComponent);
    {-------------------------------------------------------------------------------------------------------------}
begin
    inherited Create(AOwner);
    VInfoLoaded := False;
    SetFileName(EmptyStr);
end;

destructor TCustomFileVersionInfo.Destroy;
    {-------------------------------------------------------------------------------------------------------------}
begin
    inherited Destroy;
end;

function TCustomFileVersionInfo.GetKeyValue(const Key : string) : string;
    {-------------------------------------------------------------------------------------------------------------}
begin
    Result := LoadVersionInfo(Key);
end;

function FileVersionStrToNumberVersion(const StrVer : string) : TNumberVersion;
    //----------------------------------------------------------------------------------------------------------------------
var
    i, ECode, Num : Integer;
    S : string;
begin
	 Result.MajorVersionEx := 0;
    Result.MinorVersionEx := 0;
    for i := 0 to 3 do begin
        S := Str_Pas.GetDelimitedSubStr('.', StrVer, i);
        Val(S, Num, ECode);
        if ECode <> 0 then begin
            case i of
                0 : begin
                    Result.Major := 0;
                end;
                1 : begin
                    Result.Minor := 0;
                end;
                2 : begin
                    Result.Release := 0;
                end;
                3 : begin
                    Result.Build := 0;
                end;
            end;
            System.Continue; //pega próximo item
        end;
        //Atribuicao correta do valor
        case i of
            0 : begin
                Result.Major := Num;
            end;
            1 : begin
                Result.Minor := Num;
            end;
            2 : begin
                Result.Release := Num;
            end;
            3 : begin
                Result.Build := Num;
            end;
        end;
    end;
end;

procedure Register;
//----------------------------------------------------------------------------------------------------------------------------------
begin
    RegisterComponents('Super', [TFileVersionInfo]);
end;


function TVersionInfo.Compares(Another : TVersionInfo) : Integer;
{{
Compara a versão passada com a instância e retorna -1 se a instância for menor, 0 se iguais e 1 se acima.
Ex.: (Self = 1.2.8; Another = 1.20.0) = 1
(Self = 1.2.8; Another = 1.2.0) = -1

Revision: 21/5/2007 - roger
}
begin
	 if (another.FVersion.Major = Self.FVersion.Major) then begin
		 if (another.FVersion.Minor = Self.FVersion.Minor) then begin
			 if (another.FVersion.Release = Self.FVersion.Release) then begin
				 if (Another.FVersion.Build = Self.FVersion.Build) then begin
					Result := 0;
				 end else begin
					if (Another.FVersion.Build > Self.FVersion.Build) then begin
						Result:=1;
					end else begin
						Result:=-1;
					end;
				 end;
			 end else begin
				 if another.FVersion.Release > Self.FVersion.Release then begin
                    Result := 1;
                end else begin
                    Result := -1;
                end;
            end;
        end else begin
            if (another.FVersion.Minor > Self.FVersion.Minor) then begin
                Result := 1;
            end else begin
                Result := -1;
            end;
        end;
    end else begin
        if (another.FVersion.Major > Self.FVersion.Major ) then begin
            Result := 1;
        end else begin
            Result := -1;
        end;
    end;
end;

class function TVersionInfo.CompareTo(ver1, ver2 : string) : Integer;
var
    vi1, vi2 : TVersionInfo;
begin
    vi1 := TVersionInfo.Create(ver1);
    try
        vi2 := TVersionInfo.Create(ver2);
        try
            Result := vi1.Compares(vi2);
        finally
            vi2.Free;
        end;
    finally
        vi1.Free;
    end;
end;

class function TVersionInfo.Convert(const StrVersion : string) : TNumberVersion;
var
    i, ECode, Num : Integer;
    S : string;
begin
	 Result.MajorVersionEx := 0;
	 Result.MinorVersionEx := 0;
	 for i := 0 to 3 do begin
		 S := Str_Pas.GetDelimitedSubStr('.', StrVersion, i);
        Val(S, Num, ECode);
        if ECode <> 0 then begin
            case i of
                0 : begin
                    Result.Major := 0;
                end;
                1 : begin
                    Result.Minor := 0;
                end;
                2 : begin
                    Result.Release := 0;
                end;
                3 : begin
                    Result.Build := 0;
                end;
            end;
            //Exit;
        end else begin
            //Atribuicao correta do valor
            case i of
                0 : begin
                    Result.Major := Num;
                end;
                1 : begin
                    Result.Minor := Num;
                end;
                2 : begin
                    Result.Release := Num;
                end;
                3 : begin
                    Result.Build := Num;
                end;
            end;
        end;
    end;
end;

constructor TVersionInfo.Create(const VersionString : string);
{{
Convert a string passada na estrutura abstrata de versão.

Revision: 21/5/2007 - roger
}
begin
    Self.FVersion := Convert(VersionString);
end;

constructor TVersionFileInfo.Create(FileName : PAnsiChar);
{{
Carrega a versão do arquivo passado.

Revision: 21/5/2007 - roger
}
var
    fi : TCustomFileVersionInfo;
begin
    fi := TCustomFileVersionInfo.Create(nil);
    try
        fi.FileName := String(Filename);
        inherited Create(fi.GetFileVersion);
    finally
        fi.Free;
    end;
end;

end.
