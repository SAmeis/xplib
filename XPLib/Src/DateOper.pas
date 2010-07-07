{$IFDEF DateOper}
    {$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I XPLib.inc}
//NOTAS: Para usar esta unit setar Z4
 { TODO -oRoger -cLIB : Validar a informacao acima, se verdadeira descomentar a linha abaixo
 Esta unit possui enumerações longas que não são suportadas por byte(suspeita)
 Z4 = DWORD
 Z1 = byte(padrão compilador) }
{$Z4}


unit DateOper;

interface

uses
    Classes, Windows, StrHnd;

const
     {{
     Revision: 1/6/2009 - roger
     Constantes de formatação data/hora para uso comum
     }
    DATE_FORMAT_SHORT_ORDERED = 'yyyymmdd';
    DATE_FORMAT_LONG_ORDERED  = 'yyyymmddhhnnss';
    DATE_FORMAT_FULL_ORDERED  = 'yyyymmddhhnnsszzz';

const
    DaysOfWeekAbrev: array[0..6] of string[3] = ('DOM', 'SEG', 'TER', 'QUA', 'QUI', 'SEX', 'SAB');
    MonthOfYearNames: array[1..12] of string[10] = ('Janeiro', 'Fevereiro', 'Março', 'Abril', 'Maio', 'Junho', 'Julho', 'Agosto',
        'Setembro', 'Outubro', 'Novembro', 'Dezembro');

type
    TXPDateOrder = (doMDY, doDMY, doYMD); //Sequencia na qual os componentes de uma data aparecem


    TDiffDate = record
        DifYears:  Word;
        DifMonths: Word;
        DifDays:   Word;
        Signal:    Word;
    end;
    TDifDate = TDiffDate; //Renomear corretamente no futuro

    //*** O modo abaixo nao pode ser linkado como pacote !!!!
    //TMonthEnum    =    ( meJan = 1, meFev = 2, meMar = 3, meAbr = 4, meMay = 5, meJun = 6, meJul = 7, meAgo = 8, meSet = 9, meOct = 10, meNov = 11, meDec = 12);
    TMonthEnum = (meNone, meJan, meFev, meMar, meAbr, meMay, meJun, meJul, meAgo, meSet, meOct, meNov, meDec);
    TMonthSet  = set of TMonthEnum;

type
    TDayPeriod  = (dpMorning, dpAfternoon, dpNight, dpDawn);
    TDayPeriods = set of TDayPeriod;

const
    FILETIME_BASE           = -109205.0; {{ 1 Data de refereência para deslocamento relativo ao Windows }
    FILETIME_STEP: Extended = 24.0 * 60.0 * 60.0 * 1000.0 * 1000.0 * 10.0; {{ Quantidade de nanosegundos por dia}

    MonthOrdinalNames: array[meJan..meDec] of string[10] =
        ('Janeiro', 'Fevereiro', 'Março', 'Abril', 'Maio', 'Junho', 'Julho', 'Agosto',
        'Setembro', 'Outubro', 'Novembro', 'Dezembro');


type
    TTimeHnd = class
    {{
    Classe para manipulação de dados relacionados com data e hora
    Boa parte de seu conteudo foi extraido de JvDateUtil ( incluindo todos os metodos privados que devem ser re-alocados e/ou
    substituidos no futuro )


    Revision: 10/8/2005 - Roger
    }
        { TODO -oroger -clib : re-alocar os metodos privados desta classe }
    private
        class function InternalStrToDate(const DateFormat, S : string; var Date : TDateTime) : boolean;
        class function ScanDateStr(const Format, S : string; var D, M, Y : Integer) : boolean;
        class procedure ExtractMask(const Format, S : string; Ch : char; Cnt : Integer; var I : Integer; Blank, Default : Integer);
        class function ScanDate(const S, DateFormat : string; var Pos : Integer; var Y, M, D : Integer) : boolean;
        class function ScanNumber(const S : string; MaxLength : Integer; var Pos : Integer; var Number : longint) : boolean;
        class function ScanChar(const S : string; var Pos : Integer; Ch : char) : boolean;
        class procedure ScanBlanks(const S : string; var Pos : Integer);
        class procedure ScanToNumber(const S : string; var Pos : Integer);
    public
        class function DayPeriod(const Time : TDateTime) : TDayPeriod;
        class function StrToDateFmt(const DateFormat, S : string) : TDateTime;
        class function MonthFromName(const S : string; MaxLen : byte) : byte;
        class function ExpandYear(Year : Integer) : Integer;
        class procedure SetCenturyOffset(Value : Integer);
        class function GetDateOrder(const DateFormat : string) : TXPDateOrder;
    end;

    TTimeConvert = class
    {{
    Class for times conversions

    Para o uso de algo do tipo DosDateTimeToFileTime usar a API do Windows.

    Ver também as contidas na API Windows:

    CompareFileTime
    DosDateTimeToFileTime    FileTimeToDosDateTime    FileTimeToLocalFileTime    FileTimeToSystemTime    GetFileTime    GetLocalTime    GetSystemTime    GetSystemTimeAdjustment    GetSystemTimeAsFileTime    GetTickCount    GetTimeZoneInformation    LocalFileTimeToFileTime    SetFileTime    SetLocalTime    SetSystemTime    SetSystemTimeAdjustment    SetTimeZoneInformation    SystemTimeToFileTime    SystemTimeToTzSpecificLocalTime

    Classe baseada na Unit JclDateTime

    Revision: 30/11/2005 - Roger
    }
    private
        class procedure ResultCheck(Val : longbool);
    public
        class function DateTimeToLocalDateTime(const DateTime : TDateTime) : TDateTime;
        class function DateTimeToFileTime(const DateTime : TDateTime) : TFileTime;
        class function LocalDateTimeToFileTime(const DateTime : TDateTime) : FileTime;
        class function LocalDateTimeToDateTime(const DateTime : TDateTime) : TDateTime;
        class function DosDateTimeToFileTime(const DosTime : Integer) : TFileTime;
        class function DosDateTimeToSystemTime(const DosTime : Integer) : TSystemTime;
        class function FileTimeToDateTime(const FileTime : TFileTime) : TDateTime;
        class function FileTimeToLocalDateTime(const FileTime : TFileTime) : TDateTime;
        class function FileTimeToDosDateTime(const FileTime : TFileTime) : Integer;
        class function SystemTimeToDosDateTime(const SystemTime : TSystemTime) : Integer;
        class function FileTimeToSystemTime(const FileTime : TFileTime) : TSystemTime;
    end;


function DaysInMonth(Month : Integer; Year : Integer; Epoch : Integer = 1900) : Integer;
//Retorna estrutura com a diferenca de anos, meses e dias entre duas datas
function DeltaDate(ADate1, ADate2 : TDateTime) : TDiffDate;
function DifDate(ADate : TDateTime) : TDifDate; deprecated;  //usar DeltaDate
function GetLocalBiasUTC : longint;
function GetMonth(Dt : TDateTime) : Word;
function GetDay(Dt : TDateTime) : Word;
function GetYear(Dt : TDateTime) : Word;
function IncTime(ATime : TDateTime; Hours, Minutes, Seconds, MSecs : Integer) : TDateTime;
function AddDays(Actual : TDateTime; Days : Integer) : TDateTime;
function FirstMonth(Dt : TDateTime) : TDateTime; overload;
function FirstMonth(Month, Year : Word) : TDateTime; overload;
function DaysGap(Dt1, Dt2 : TDateTime) : Integer;
function ActualSeconds : Word;
function ActualMinutes : Word;
function ActualHours : Word;
function ActualYear : Word;
function ActualMonth : Word;
function ActualDay : Word;
function ActualDayofWeek : Word;
function ActualDayofWeekStr : ShortString;
function DayofWeekStr(Dt : TDateTime) : ShortString;
function LastDayOfMonth(Month : Word; Year : Word = 0) : Word;
function LastDateNotWeekendOfMonth(ADate : TDateTime) : TDateTime; overload;
function LastDateNotWeekendOfMonth(Month : Word; Year : Word) : TDateTime; overload;
//incrementa a data passada em n meses
function MonthIncrement(ADate : TDateTime; nM : Word) : TDateTime;
//Retorna o conjunto de meses da mascara dada
function MonthsFromBitMask(Mask : Integer; StartMonth : TMonthEnum = meJan) : TMonthSet;
//Monta mascara de bits dado o conjunto de meses
function MonthsToBitMask(Months : TMonthSet; StartMonth : TMonthEnum = meJan) : Integer;
//Monta lista com o meses incluidos em Months
function MonthEnumListNames(Months : TMonthSet) : string;
//Monta lista com os meses incluidos em Months na forma de numerais
function MonthEnumListNumbers(Months : TMonthSet; TwoDigits : boolean = False) : string;
//Recupera o valor do mes pela sua grafia longa
function MonthLongNameToInt(const StrMonth : string) : Integer;
//Recupera o valor do mes pela sua grafia curta
function MonthShortNameToInt(const StrMonth : string) : Integer;
function SecondsOfDay : double;
//Ajusta a hora da maquina de acordo o padrao MS -> GMT
function SetLocalDateTime(DateTime : TDateTime) : Integer;

implementation

uses
    SysUtils, Str_Pas, Math, DateUtils, SysConst;


resourcestring
    RsMakeUTCTime    = 'Erro convertendo tempo para formato UTC. Aspectos regionais não podem ser determinados.';
    RsDateConversion = 'Data ou hora em formato ilegal.';

var
    GlobalCenturyOffset : Integer = 60;

function ActualYear : Word;
    //----------------------------------------------------------------------------------------------------------------------
    //Retorna o valor do ano atual
var
    No : Word;
begin
    DecodeDate(Now, Result, No, No);
end;

function ActualMonth : Word;
    //----------------------------------------------------------------------------------------------------------------------
    //Retorna o valor do mes atual
var
    No : Word;
begin
    DecodeDate(Now, No, Result, No);
end;

function ActualDay : Word;
    //----------------------------------------------------------------------------------------------------------------------
    //Retorna o valor do dia atual
var
    No : Word;
begin
    DecodeDate(Now(), No, No, Result);
end;

function ActualDayofWeek : Word;
    //----------------------------------------------------------------------------------------------------------------------
    //Retorna o valor do dia atual expresso em byte ordenado de 0 a 6
begin
    ActualDayofWeek := Word(DayOfWeek(Now));
end;

function ActualDayOfWeekStr : ShortString;
    //----------------------------------------------------------------------------------------------------------------------
    //Retorna o valor do dia da semana atual no formato abreviado 'DOM...SAB'
begin
    ActualDayOfWeekStr := DaysOfWeekAbrev[ActualDayOfWeek];
end;

function DayOfWeekStr(Dt : TDateTime) : ShortString;
    //----------------------------------------------------------------------------------------------------------------------
    //Retorna o valor do dia da semana no formato abreviado 'DOM...SAB'
begin
    Result := DaysOfWeekAbrev[DayOfWeek(Dt)];
end;

function ActualSeconds : Word;
    //----------------------------------------------------------------------------------------------------------------------
    //Retorna o valor dos segundos passados do minuto atual
var
    No : Word;
begin
    DecodeTime(Now, No, No, Result, No);
end;

function ActualMinutes : Word;
    //----------------------------------------------------------------------------------------------------------------------
    //Retorna o valor dos minutos passados da hora atual
var
    No : Word;
begin
    DecodeTime(Now, No, Result, No, No);
end;

function ActualHours : Word;
    //----------------------------------------------------------------------------------------------------------------------
    //Retorna o valor das horas passados do dia atual
var
    No : Word;
begin
    DecodeTime(Now, No, Result, No, No);
end;

function ActualTimeStr : string;
    //----------------------------------------------------------------------------------------------------------------------
    //Retorna a representacao em string da hora atual
begin
    Result := TimeToStr(Now);
end;

function LastDayOfMonth(Month : Word; Year : Word = 0) : Word;
    //----------------------------------------------------------------------------------------------------------------------
var
    Epoch : Word;
begin
    if Year = 0 then begin
        Year  := ActualYear;
        Epoch := 2000; //Posso garantir que em 2099 o Delphi nem sera lembrado!
    end else begin
        Epoch := 0;
    end;
    Result := DaysInMonth(Month, Year, Epoch);
end;

function LastDateNotWeekendOfMonth(ADate : TDateTime) : TDateTime; overload;
    //----------------------------------------------------------------------------------------------------------------------
var
    Y, D, M : Word;
begin
    DecodeDate(ADate, Y, M, D);
    Result := LastDateNotWeekendOfMonth(M, Y);
end;

function LastDateNotWeekendOfMonth(Month : Word; Year : Word) : TDateTime;
    //----------------------------------------------------------------------------------------------------------------------
    //Retorna data do ultimo dia "teoricamente" util do mes especificado
begin
    Result := EncodeDate(Year, Month, DaysInAMonth(Year, Month));
    while (DayOfWeek(Result) in [1, 7]) do begin
        Result := Result - 1;
    end;
end;

function MonthIncrement(ADate : TDateTime; nM : Word) : TDateTime;
    //----------------------------------------------------------------------------------------------------------------------------------
    //incrementa a data passada em n meses
var
    D, M, Y : Word;
    MxDay :   Integer;
begin
    DecodeDate(ADate, Y, M, D);
    Inc(M, nM mod 12);
    Inc(Y, M div 12);
    M     := Max(1, (M mod 12));
    MxDay := DaysInMonth(M, Y);
    if D > MxDay then begin
        D := MxDay;
    end;
    Result := EncodeDate(Y, M, D);
end;

function MonthsFromBitMask(Mask : Integer; StartMonth : TMonthEnum = meJan) : TMonthSet;
    //----------------------------------------------------------------------------------------------------------------------------------
    //Retorna o conjunto de meses da mascara dada
var
    i : TMonthEnum;
begin
    Result := [];
    for i := StartMonth to meDec do begin
        if Mask and (1 shl (Ord(i) - Ord(StartMonth))) <> 0 then begin
            Include(Result, TMonthEnum(i));
        end;
    end;
end;

/// <summary>
///  Monta mascara de bits dado o conjunto de meses
/// <bold>Esta rotina precisa de melhor documentação</bold>
/// </summary>
/// <param name="Months">Meses que pertencem ao conjunto de interesse</param>
/// <param name="StartMonth">Mês de offset da operação</param>
/// <returns>Valor inteiro mascarado em bits dos meses do conjunto</returns>
function MonthsToBitMask(Months : TMonthSet; StartMonth : TMonthEnum = meJan) : Integer;
var
    i : TMonthEnum;
begin
    Result := 0;
    for i := meJan to meDec do begin
        if i in Months then begin
            Inc(Result, 1 shl (Ord(i) - Ord(StartMonth)));
        end;
    end;
end;

function MonthEnumListNames(Months : TMonthSet) : string;
    //----------------------------------------------------------------------------------------------------------------------------------
    //Monta lista com o meses incluidos em Months
var
    m :   TMonthEnum;
    Lst : TStringList;
    i :   Integer;
begin
    Lst := TStringList.Create;
    try
        for m := meJan to meDec do begin
            if m in Months then begin
                Lst.Add(MonthOrdinalNames[m]);
            end;
        end;
        for i := 0 to Lst.Count - 1 do begin
            Result := Result + Lst[i] + ', ';
        end;
        Result := StrCopyBeforeLast(', ', Result); //Remove ultima virgula
        i      := StrLastPos(',', Result);
        if i > 0 then begin
            Result := Copy(Result, 1, i) + ' e' + Copy(Result, i + 2, 10);
        end;
    finally
        Lst.Free;
    end;
end;

function MonthEnumListNumbers(Months : TMonthSet; TwoDigits : boolean = False) : string;
    //----------------------------------------------------------------------------------------------------------------------------------
    //Monta lista com o meses incluidos em Months
var
    m :   TMonthEnum;
    Lst : TStringList;
    i :   Integer;
begin
    Lst := TStringList.Create;
    try
        for m := meJan to meDec do begin
            if m in Months then begin
                if TwoDigits then begin
                    Lst.Add(PadL(IntToStr(Ord(m)), 2, '0'));
                end else begin
                    Lst.Add(IntToStr(Ord(m)));
                end;
            end;
        end;
        for i := 0 to Lst.Count - 1 do begin
            Result := Result + Lst[i] + ', ';
        end;
        Result := StrCopyBeforeLast(', ', Result); //Remove ultima virgula
        i      := StrLastPos(',', Result);
        if i > 0 then begin
            Result := Copy(Result, 1, i) + ' e' + Copy(Result, i + 2, 10);
        end;
    finally
        Lst.Free;
    end;
end;

function MonthLongNameToInt(const StrMonth : string) : Integer;
    //----------------------------------------------------------------------------------------------------------------------
    //Recupera o valor do mes pela sua grafia longa
var
    i : Integer;
begin
    for i := 1 to high(SysUtils.LongMonthNames) do begin
        if LongMonthNames[i] = StrMonth then begin
            Result := i;
            Exit;
        end;
    end;
    Result := -1; //Valor invalido
end;

function MonthShortNameToInt(const StrMonth : string) : Integer;
    //----------------------------------------------------------------------------------------------------------------------
    //Recupera o valor do mes pela sua grafia curta
var
    i : Integer;
begin
    for i := 1 to high(SysUtils.ShortMonthNames) do begin
        if LongMonthNames[i] = StrMonth then begin
            Result := i;
            Exit;
        end;
    end;
    Result := -1; //Valor invalido
end;

function SecondsOfDay : double;
    //----------------------------------------------------------------------------------------------------------------------
    //    Calcula o total de segundos decorridos a partir da 00:00hs
var
    h, m, s, ms : Word;
begin
    DecodeTime(Now, h, m, s, ms);
    Result := 3600 * (h) + 60 * (m) + (s) + (ms) / 100;
end;

function DaysGap(Dt1, Dt2 : TDateTime) : Integer;
    //----------------------------------------------------------------------------------------------------------------------
    //Retorna a diferenca em dias de duas datas dadas
begin
    Result := trunc(double(Dt2) - double(Dt1));
end;


function DaysInMonth(Month : Integer; Year : Integer; Epoch : Integer = 1900) : Integer;
    //----------------------------------------------------------------------------------------------------------------------
    //Retorna o No de dias no mes de um ano especifico
var
    EpochYear, EpochCent : Integer;
begin
    if Word(Year) < 100 then begin
        EpochYear := Epoch mod 100;
        EpochCent := (Epoch div 100) * 100;
        if (Year < EpochYear) then begin
            Inc(Year, EpochCent + 100);
        end else begin
            Inc(Year, EpochCent);
        end;
    end;

    case Month of
        1, 3, 5, 7, 8, 10, 12 : begin
            Result := 31;
        end;
        4, 6, 9, 11 : begin
            Result := 30;
        end;
        2 : begin
            Result := 28 + Ord(IsLeapYear(Year));
        end;
        else begin //Mes invalido
            Result := -1;
        end;
    end;
end;

function DeltaDate(ADate1, ADate2 : TDateTime) : TDiffDate;
    //----------------------------------------------------------------------------------------------------------------------
    //Retorna a diferenca em dias, meses, e anos entre duas datas julianas
var
    Day1, Day2, Month1, Month2, Year1, Year2 : Word;
    TmpDate : TDateTime;
begin

    //Date2 deve ser maior que Date1
    if (ADate1 > ADate2) then begin
        TmpDate := ADate2;
        ADate2  := ADate1;
        ADate1  := TmpDate;
        Result.Signal := 1;
    end else begin
        Result.Signal := 0;
    end;

    //converte datas para day,month,year
    DecodeDate(ADate1, Year1, Month1, Day1);
    DecodeDate(ADate2, Year2, Month2, Day2);

    //dias 1o
    if Day2 < Day1 then begin
        Dec(Month2);
        if Month2 = 0 then begin
            Month2 := 12;
            Dec(Year2);
        end;
        Inc(Day2, DaysInMonth(Month2, Year2, 0));
    end;
    Result.DifDays := Day2 - Day1;

    //Agora meses e anos
    if Month2 < Month1 then begin
        Inc(Month2, 12);
        Dec(Year2);
    end;
    Result.DifMonths := Month2 - Month1;
    Result.DifYears  := Year2 - Year1;
end;

function DifDate(ADate : TDateTime) : TDifDate;
    {-------------------------------------------------------------------------------------------------------------}
    //Retorna estrutura com a diferenca de anos, meses e dias entre duas datas
    //BUG sem correcao futura
var
    Today : TDateTime;
begin
    Today := Now - ADate;
    DecodeDate(Today, Result.DifYears, Result.DifMonths, Result.DifDays);
    Dec(Result.DifYears, 1900);
    Dec(Result.DifMonths, 1);
    Dec(Result.DifDays, 1);
end;

function GetYear(Dt : TDateTime) : Word;
    //----------------------------------------------------------------------------------------------------------------------
    //    Decodifica o ano da data
var
    M, D : Word;
begin
    DecodeDate(Dt, Result, M, D);
end;

function GetLocalBiasUTC : longint;
    //----------------------------------------------------------------------------------------------------------------------
    //Retorna valor em minutos do meridiano de grewish?????
var
    tzInfo : TTimeZoneInformation;
const
    TIME_ZONE_ID_STANDARD = 1;
    TIME_ZONE_ID_DAYLIGHT = 2;
begin
    case GetTimeZoneInformation(tzInfo) of
        TIME_ZONE_ID_STANDARD : begin
            Result := tzInfo.Bias + tzInfo.StandardBias;
        end;
        TIME_ZONE_ID_DAYLIGHT : begin
            Result := tzInfo.Bias + tzInfo.DaylightBias;
        end;
        else begin
            Result := tzInfo.Bias;
        end;
    end;
end;

function GetMonth(Dt : TDateTime) : Word;
    //----------------------------------------------------------------------------------------------------------------------
    //Decodifica o mes da data
var
    Y, D : Word;
begin
    DecodeDate(Dt, Y, Result, D);
end;

function FirstMonth(Dt : TDateTime) : TDateTime;
    //----------------------------------------------------------------------------------------------------------------------
    //Calcula a data do primeiro dia do mes da data passada
begin
    Result := Dt - GetDay(Dt) - 1;
end;

function FirstMonth(Month, Year : Word) : TDateTime; overload;
    //----------------------------------------------------------------------------------------------------------------------
    //Calcula a data do primeiro dia do mes segundo mes e ano passado
begin
    Result := EncodeDate(Year, Month, 1);
end;

function GetDay(Dt : TDateTime) : Word;
    //----------------------------------------------------------------------------------------------------------------------
    //Decodifica o dia da data
var
    Y, M : Word;
begin
    DecodeDate(Dt, Y, M, Result);
end;

function IncTime(ATime : TDateTime; Hours, Minutes, Seconds, MSecs : Integer) : TDateTime;
    //----------------------------------------------------------------------------------------------------------------------
begin
    Result := ATime + (Hours div 24) + (((Hours mod 24) * 3600000 + Minutes * 60000 + Seconds * 1000 + MSecs) / MSecsPerDay);
    if Result < 0 then begin
        Result := Result + 1;
    end;
end;

function AddDays(Actual : TDateTime; Days : Integer) : TDateTime;
    //----------------------------------------------------------------------------------------------------------------------
    //Soma a uma data uma quantidade fixa de dias
begin
    Result := Actual + Days;
end;

function SetLocalDateTime(DateTime : TDateTime) : Integer;
    //----------------------------------------------------------------------------------------------------------------------
    //Ajusta a hora da maquina de acordo o padrao MS -> GMT
var
    STime :    TSystemTime;
    TimeZone : TTimeZoneInformation;
begin
    GetTimeZoneInformation(TimeZone);
    DateTimeToSystemTime(DateTime, STime);
    STime.wHour := STime.wHour + (GetLocalBiasUTC() div 60);
    if not SetSystemTime(STime) then begin
        Result := GetLastError;
    end else begin
        Result := 0;
    end;
end;



(*************** LEIA ISTO **********************)

{
TDateTime is actually a floating point number defined like

TYPE    TDateTime=float;

The definition goes something like this;

The real numer of the floating point matches the days passed since 1st Jan
the year of 0001. The fraction of the floationg point matches the time of
the day, where .0 would be midnight and .5 would be noon, 0.99999... would
be just befor midnight,

Using this representation of time& dates you can easliy calculate several
things,

Days beetwen dates:
    days:=real(date1-date2);
Day of the week
    dow:=date1 div 7;
Add 25 seconds to a date/time:
    date:=date+25/(24*60*60);

There are several routines included in the SysUtils unit, giving you the
possibilty of converting & formating dates. Checkout the online manual.
A good startpoint is "TDateTime".

What you will have problem calculating is WeekNumber of date. If you would
like this function, I suspect that their is someone out there willing to
suply you with one, as it's a quite general & usefull code. Myself I use an
old routine I developed on BP7.0 using a quite simular techniqe as TDateTime.
I'll be happy to suply anyone with this library if requested.

If I were into dates below the year of 1700 I would first check out weather
Borland took in account that there were some ten missing dates, and this leap
was not introduced simultanious all across the world.   My advice, check this
out if you're not going to sticking to the latest centuries.
}

{ TTimeHnd }

class function TTimeHnd.DayPeriod(const Time : TDateTime) : TDayPeriod;
{{
retorna em qual periodo do dia aquela hora representa.
}
var
    h, dummy : Word;
begin
    { TODO -oRoger -cLIB : pode ser optmizada se usada a parte fracionaria do valor }
    DecodeTime(Time, h, dummy, dummy, dummy);
    if (h >= 5) then begin
        if (h >= 12) then begin
            if h >= 18 then begin
                Result := dpNight;
            end else begin
                Result := dpAfternoon;
            end;
        end else begin
            Result := dpMorning;
        end;
    end else begin
        Result := dpDawn;
    end;
end;

class function TTimeHnd.ExpandYear(Year : Integer) : Integer;
var
    N : longint;
begin
    Result := Year;
    if Result < 100 then begin
        N := CurrentYear - GlobalCenturyOffset;
        Inc(Result, N div 100 * 100);
        if (GlobalCenturyOffset > 0) and (Result < N) then begin
            Inc(Result, 100);
        end;
    end;
end;

class procedure TTimeHnd.ExtractMask(const Format, S : string; Ch : char; Cnt : Integer; var I : Integer;
    Blank, Default : Integer);
var
    Tmp :  string;
    J, L : Integer;
begin
    I  := Default;
    Ch := UpCase(Ch);
    L  := Length(Format);
    if Length(S) < L then begin
        L := Length(S);
    end else
    if Length(S) > L then begin
        Exit;
    end;
    J := Pos(StringOfChar(Ch, Cnt), AnsiUpperCase(Format));
    if J <= 0 then begin
        Exit;
    end;
    Tmp := '';
    while (UpCase(Format[J]) = Ch) and (J <= L) do begin
        if S[J] <> ' ' then begin
            Tmp := Tmp + S[J];
        end;
        Inc(J);
    end;
    if Tmp = '' then begin
        I := Blank;
    end else
    if Cnt > 1 then begin
        I := MonthFromName(Tmp, Length(Tmp));
        if I = 0 then begin
            I := -1;
        end;
    end else begin
        I := StrToIntDef(Tmp, -1);
    end;
end;

class function TTimeHnd.GetDateOrder(const DateFormat : string) : TXPDateOrder;
var
    I : Integer;
begin
    Result := doDMY; //Valor compativel com o brasil, mas poderia ser accessivel estaticamente pela classe
    I      := 1;
    while I <= Length(DateFormat) do begin
        case Chr(Ord(DateFormat[I]) and $DF) of
            'E' : begin
                Result := doYMD;
            end;
            'Y' : begin
                Result := doYMD;
            end;
            'M' : begin
                Result := doMDY;
            end;
            'D' : begin
                Result := doDMY;
            end;
            else begin
                Inc(I);
            end;
                Continue;
        end;
        Exit;
    end;
    Result := doDMY; //Idem para o cado do inicio da rotina
end;

class function TTimeHnd.InternalStrToDate(const DateFormat, S : string; var Date : TDateTime) : boolean;
var
    D, M, Y : Integer;
begin
    if S = '' then begin
        Date   := 0; //Data invalida
        Result := True;
    end else begin
        Result := ScanDateStr(DateFormat, S, D, M, Y);
        if Result then begin
            try
                Date := EncodeDate(Y, M, D);
            except
                Result := False;
            end;
        end;
    end;
end;

class function TTimeHnd.MonthFromName(const S : string; MaxLen : byte) : byte;
begin
    if Length(S) > 0 then begin
        for Result := 1 to 12 do begin
            if (Length(LongMonthNames[Result]) > 0) and
                (AnsiCompareText(Copy(S, 1, MaxLen),
                Copy(LongMonthNames[Result], 1, MaxLen)) = 0) then begin
                Exit;
            end;
        end;
    end;
    Result := 0;
end;

class procedure TTimeHnd.ScanBlanks(const S : string; var Pos : Integer);
var
    I : Integer;
begin
    I := Pos;
    while (I <= Length(S)) and (S[I] = ' ') do begin
        Inc(I);
    end;
    Pos := I;
end;

class function TTimeHnd.ScanChar(const S : string; var Pos : Integer; Ch : char) : boolean;
begin
    Result := False;
    ScanBlanks(S, Pos);
    if (Pos <= Length(S)) and (S[Pos] = Ch) then begin
        Inc(Pos);
        Result := True;
    end;
end;

class function TTimeHnd.ScanDate(const S, DateFormat : string; var Pos, Y, M, D : Integer) : boolean;
var
    DateOrder :  TXPDateOrder;
    N1, N2, N3 : longint;
begin
    Result := False;
    Y      := 0;
    M      := 0;
    D      := 0;
    DateOrder := GetDateOrder(DateFormat);
    if ShortDateFormat[1] = 'g' then { skip over prefix text } begin
        ScanToNumber(S, Pos);
    end;
    if not (ScanNumber(S, MaxInt, Pos, N1) and ScanChar(S, Pos, DateSeparator) and
        ScanNumber(S, MaxInt, Pos, N2)) then begin
        Exit;
    end;
    if ScanChar(S, Pos, DateSeparator) then begin
        if not ScanNumber(S, MaxInt, Pos, N3) then begin
            Exit;
        end;
        case DateOrder of
            doMDY : begin
                Y := N3;
                M := N1;
                D := N2;
            end;
            doDMY : begin
                Y := N3;
                M := N2;
                D := N1;
            end;
            doYMD : begin
                Y := N1;
                M := N2;
                D := N3;
            end;
        end;
        Y := ExpandYear(Y);
    end else begin
        Y := CurrentYear;
        if DateOrder = doDMY then begin
            D := N1;
            M := N2;
        end else begin
            M := N1;
            D := N2;
        end;
    end;
    ScanChar(S, Pos, DateSeparator);
    ScanBlanks(S, Pos);
    if SysLocale.FarEast and (System.Pos('ddd', ShortDateFormat) <> 0) then begin { ignore trailing text }
        if ShortTimeFormat[1] in ['0'..'9'] then { stop at time digit } begin
            ScanToNumber(S, Pos);
        end else { stop at time prefix } begin
            repeat
                while (Pos <= Length(S)) and (S[Pos] <> ' ') do begin
                    Inc(Pos);
                end;
                ScanBlanks(S, Pos);
            until (Pos > Length(S)) or
                (AnsiCompareText(TimeAMString, Copy(S, Pos, Length(TimeAMString))) = 0) or
                (AnsiCompareText(TimePMString, Copy(S, Pos, Length(TimePMString))) = 0);
        end;
    end;
    Result := IsValidDate(Y, M, D) and (Pos > Length(S));
end;

class function TTimeHnd.ScanDateStr(const Format, S : string; var D, M, Y : Integer) : boolean;
var
    Pos : Integer;
begin
    ExtractMask(Format, S, 'm', 3, M, -1, 0); { short month name? }
    if M = 0 then begin
        ExtractMask(Format, S, 'm', 1, M, -1, 0);
    end;
    ExtractMask(Format, S, 'd', 1, D, -1, 1);
    ExtractMask(Format, S, 'y', 1, Y, -1, CurrentYear);
    Y      := ExpandYear(Y);
    Result := IsValidDate(Y, M, D);
    if not Result then begin
        Pos    := 1;
        Result := ScanDate(S, Format, Pos, Y, M, D);
    end;
end;

class function TTimeHnd.ScanNumber(const S : string; MaxLength : Integer; var Pos, Number : Integer) : boolean;
var
    I : Integer;
    N : Word;
begin
    Result := False;
    ScanBlanks(S, Pos);
    I := Pos;
    N := 0;
    while (I <= Length(S)) and (longint(I - Pos) < MaxLength) and
        (S[I] in ['0'..'9']) and (N < 1000) do begin
        N := N * 10 + (Ord(S[I]) - Ord('0'));
        Inc(I);
    end;
    if I > Pos then begin
        Pos    := I;
        Number := N;
        Result := True;
    end;
end;

class procedure TTimeHnd.ScanToNumber(const S : string; var Pos : Integer);
begin
    while (Pos <= Length(S)) and not CharInSet(S[Pos], ['0'..'9']) do begin
    {$IFNDEF UNICODE}// Utf16: '0'..'9' are in the BMP => no lead byte handling necessary
        if S[Pos] in LeadBytes then begin
            Inc(Pos);
        end;
    {$ENDIF ~UNICODE}
        Inc(Pos);
    end;
end;

class procedure TTimeHnd.SetCenturyOffset(Value : Integer);
begin
    GlobalCenturyOffset := Value;
end;

class function TTimeHnd.StrToDateFmt(const DateFormat, S : string) : TDateTime;
{{
Converte um texto em uma data segundo a formatação passada em S.

Revision: 10/8/2005 - Roger
}
begin
    if not InternalStrToDate(DateFormat, S, Result) then begin
        raise EConvertError.CreateFmt(SInvalidDate, [S]);
    end;
end;

class function TTimeConvert.DateTimeToFileTime(const DateTime : TDateTime) : TFileTime;
var
    E :   Extended;
    F64 : int64;
begin
    E      := (DateTime - FILETIME_BASE) * FILETIME_STEP;
    F64    := Round(E);
    Result := TFileTime(F64);
end;

class function TTimeConvert.DateTimeToLocalDateTime(const DateTime : TDateTime) : TDateTime;
{{
Ajusta a hora absoluta para o valor gmt local.

Revision: 30/11/2005 - Roger  
}
var
    TimeZoneInfo : TTimeZoneInformation;
begin
    FillChar(TimeZoneInfo, SizeOf(TimeZoneInfo), #0);
    case GetTimeZoneInformation(TimeZoneInfo) of
        TIME_ZONE_ID_STANDARD : begin
            Result := DateTime + (TimeZoneInfo.Bias / SysUtils.MinsPerDay);
        end;
        TIME_ZONE_ID_DAYLIGHT : begin
            Result := DateTime - ((TimeZoneInfo.Bias + TimeZoneInfo.DaylightBias) / SysUtils.MinsPerDay);
        end;
        else begin
            raise Exception.Create(RsMakeUTCTime);
        end;
    end;
end;

class function TTimeConvert.DosDateTimeToFileTime(const DosTime : Integer) : TFileTime;
begin
    ResultCheck(Windows.DosDateTimeToFileTime(HiWord(DosTime), LoWord(DosTime), Result));
end;

class function TTimeConvert.DosDateTimeToSystemTime(const DosTime : Integer) : TSystemTime;
var
    FileTime : TFileTime;
begin
    FileTime := DosDateTimeToFileTime(DosTime);
    Result   := FileTimeToSystemTime(FileTime);
end;

class function TTimeConvert.FileTimeToDateTime(const FileTime : TFileTime) : TDateTime;
begin
    Result := int64(FileTime) / FILETIME_STEP;
    Result := Result + FILETIME_BASE;
end;

class function TTimeConvert.FileTimeToDosDateTime(const FileTime : TFileTime) : Integer;
var
    Date, Time : Word;
begin
    ResultCheck(Windows.FileTimeToDosDateTime(FileTime, Date, Time));
    Result := (Date shl 16) or Time;
end;

class function TTimeConvert.FileTimeToLocalDateTime(const FileTime : TFileTime) : TDateTime;
var
    LocalFileTime : TFileTime;
begin
    ResultCheck(FileTimeToLocalFileTime(FileTime, LocalFileTime));
    Result := FileTimeToDateTime(LocalFileTime);
end;

class function TTimeConvert.FileTimeToSystemTime(const FileTime : TFileTime) : TSystemTime;
begin
    ResultCheck(Windows.FileTimeToSystemTime(FileTime, Result));
end;

class function TTimeConvert.LocalDateTimeToDateTime(const DateTime : TDateTime) : TDateTime;
var
    TimeZoneInfo : TTimeZoneInformation;
begin
    FillChar(TimeZoneInfo, SizeOf(TimeZoneInfo), #0);
    case GetTimeZoneInformation(TimeZoneInfo) of
        TIME_ZONE_ID_STANDARD : begin
            Result := DateTime + (TimeZoneInfo.Bias / SysUtils.MinsPerDay);
        end;
        TIME_ZONE_ID_DAYLIGHT : begin
            Result := DateTime + ((TimeZoneInfo.Bias + TimeZoneInfo.DaylightBias) / SysUtils.MinsPerDay);
        end;
        else begin
            raise Exception.Create(RsMakeUTCTime);
        end;
    end;
end;

class function TTimeConvert.LocalDateTimeToFileTime(const DateTime : TDateTime) : FileTime;
var
    LocalFileTime : TFileTime;
begin
    LocalFileTime := DateTimeToFileTime(DateTime);
    ResultCheck(LocalFileTimeToFileTime(LocalFileTime, Result));
end;

class procedure TTimeConvert.ResultCheck(Val : longbool);
begin
    if not Val then begin
        raise Exception.Create(RsDateConversion);
    end;
end;

class function TTimeConvert.SystemTimeToDosDateTime(const SystemTime : TSystemTime) : Integer;
var
    FileTime : TFileTime;
begin
    SystemTimeToFileTime(SystemTime, FileTime);
    Result := FileTimeToDosDateTime(FileTime);
end;

end.
