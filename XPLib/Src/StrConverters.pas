unit StrConverters;

interface

uses
		SysUtils, Classes, XPTypes;

type
    TStrConv = class
    private
        //FFormatSettings : PFormatSettings;  static;
        class function GetFormatSettings : TFormatSettings; static;
    public
        class property FormatSettings : TFormatSettings read GetFormatSettings;
        class function GlobalLocalSettings : TFormatSettings;
        class function StrToCurrency(const Str : string) : currency;
        class procedure AdjustFormatSettings(fs : TFormatSettings);
        class function StrToFloat2(const Str : string) : Extended;
        class function MonthLongNameToInt(const StrMonth : string) : Integer;
		 class function MonthShortNameToInt(const StrMonth : string) : Integer;
		 class function MonthFromName(const S : string; MaxLen : byte) : byte;
	 end;


implementation


uses
    Windows;

var
    GlobalFormatSettings :  PFormatSettings = nil;
    DefaultFormatSettings : TFormatSettings;

procedure InitDefaultFormatSettings();
begin
    SysUtils.GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT, DefaultFormatSettings);
    GlobalFormatSettings := @DefaultFormatSettings;
end;

{ TStrConv }

class procedure TStrConv.AdjustFormatSettings(fs : TFormatSettings);
begin
    DefaultFormatSettings := fs;
end;

class function TStrConv.GetFormatSettings : TFormatSettings;
begin
    Result := DefaultFormatSettings;
end;

class function TStrConv.GlobalLocalSettings : TFormatSettings;
var
    FS : TFormatSettings;
begin
    if (not Assigned(GlobalFormatSettings)) then begin
        SysUtils.GetLocaleFormatSettings(0, FS);
    end;
end;

class function TStrConv.MonthFromName(const S: string; MaxLen: byte): byte;
{TODO -oroger -clib : dever receber SysUtils.FormatSettings passado com o padrao classico }
begin
	 if Length(S) > 0 then begin
		 for Result := 1 to 12 do begin
			 if (Length(FormatSettings.LongMonthNames[Result]) > 0) and
				 (AnsiCompareText(Copy(S, 1, MaxLen),
				 Copy(FormatSettings.LongMonthNames[Result], 1, MaxLen)) = 0) then begin
				 Exit;
			 end;
		 end;
	 end;
	 Result := 0;
end;

class function TStrConv.MonthLongNameToInt(const StrMonth : string) : Integer;
    //----------------------------------------------------------------------------------------------------------------------
    //Recupera o valor do mes pela sua grafia longa
    {TODO -oroger -clib : dever receber SysUtils.FormatSettings passado com o padrao classico }
var
    i :  Integer;
    fs : TFormatSettings;
begin
    fs := FormatSettings;
    for i := 1 to high(fs.LongMonthNames) do begin
        if fs.LongMonthNames[i] = StrMonth then begin
            Result := i;
            Exit;
        end;
    end;
    Result := -1; //Valor invalido
end;

class function TStrConv.MonthShortNameToInt(const StrMonth : string) : Integer;
    //----------------------------------------------------------------------------------------------------------------------
    //Recupera o valor do mes pela sua grafia curta
    {TODO -oroger -clib : dever receber SysUtils.FormatSettings passado com o padrao classico }
var
    i :  Integer;
    fs : TFormatSettings;
begin
    fs := FormatSettings;
    for i := 1 to high(fs.ShortMonthNames) do begin
        if fs.LongMonthNames[i] = StrMonth then begin
            Result := i;
            Exit;
        end;
    end;
    Result := -1; //Valor invalido
end;

class function TStrConv.StrToCurrency(const Str : string) : currency;
    //----------------------------------------------------------------------------------------------------------------------
    //Converte string para currency
var
    DotSep, SemiCSep : PChar;
    OldDecimalSeparator : char;
    FS : TFormatSettings;
begin
    FS     := FormatSettings;
    DotSep := StrRScan(PChar(Str), '.');
    SemiCSep := StrRScan(PChar(Str), ',');
    if ((DWORD(DotSep) or DWORD(SemiCSep)) <> $0) then begin //Existe separador
        OldDecimalSeparator := FormatSettings.DecimalSeparator; //***Caso outro thread alterar esse valor UM ABRACO !!!!!!!
        if longint(DotSep) > longint(SemiCSep) then begin //Assume que o separador sera ponto
            try
                FS.DecimalSeparator := '.';
                StringReplace(Str, ',', EmptyStr, [rfReplaceAll]);
                Result := StrToCurr(Str);
            finally
                FS.DecimalSeparator := OldDecimalSeparator;
            end;
        end else begin //Separador sera virgula
            try
                FS.DecimalSeparator := ',';
                StringReplace(Str, '.', EmptyStr, [rfReplaceAll]);
                Result := StrToCurr(Str);
            finally
                FS.DecimalSeparator := OldDecimalSeparator;
            end;
        end;
    end else begin //Converte direto
        Result := StrToCurr(Str);
    end;
end;

class function TStrConv.StrToFloat2(const Str : string) : Extended;
    //----------------------------------------------------------------------------------------------------------------------
    //Converte string para float
var
    DotSep, SemiCSep : PChar;
    OldDecimalSeparator : char;
    fs : TFormatSettings;
begin
    fs     := FormatSettings;
    DotSep := StrRScan(PChar(Str), '.');
    SemiCSep := StrRScan(PChar(Str), ',');
    if ((DWORD(DotSep) or DWORD(SemiCSep)) <> $0) then begin //Existe separador
        OldDecimalSeparator := fs.DecimalSeparator; //***Caso outro thread alterar esse valor UM ABRACO !!!!!!!
        if longint(DotSep) > longint(SemiCSep) then begin //Assume que o separador sera ponto
            try
                fs.DecimalSeparator := '.';
                StringReplace(Str, ',', EmptyStr, [rfReplaceAll]);
                Result := StrToFloat(Str);
            finally
                fs.DecimalSeparator := OldDecimalSeparator;
            end;
        end else begin //Separador sera virgula
            try
                fs.DecimalSeparator := ',';
                StringReplace(Str, '.', EmptyStr, [rfReplaceAll]);
                Result := StrToFloat(Str);
            finally
                fs.DecimalSeparator := OldDecimalSeparator;
            end;
        end;
    end else begin //Converte direto
        Result := StrToFloat(Str);
    end;
end;

initialization
    begin
        InitDefaultFormatSettings();
    end;

end.
