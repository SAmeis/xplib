{$IFDEF WinHnd}
	{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I WinSysLib.inc}

 //NOTAS : As funcoes da toolhelp sao exclusivas para o windows 95 e nao podem ser acessadas pelo
 // WIN-NT 4.0
 //O Escalonador da multi-tarefa pode influenciar na reposta das funcoes que usam a toolhelp
 //Assim devemos ter cuidado com as repostas obtidas e procurar quando for o caso diminuir ou
 //aumentar o chaveamento do escalonador EX: a GetAllActiveModules chama a GetProcessModules
 //para dar change para um novo snapshot apos uma passagem pelo escalonador
 //Talves uma solucao seja aumentar a prioridade do processo atual

unit WinHnd;

interface

uses
    Windows, SysUtils, Forms, Controls, Str_Pas, Classes, TLHelp32,
    PsAPI, //Especifica para NT ou superior ,
    SysConst;

const
    _MAX_PROCESS_NUMBER_ = 200;

type
    TProcessInfoArray = array[0.._MAX_PROCESS_NUMBER_ - 1] of TProcessEntry32;
    PProcessInfoArray = ^TProcessInfoArray;

    {{
    Rotina tipo callback para as APIs
    EnumDesktopProc, EnumDesktops, EnumDesktopWindows, EnumWindowStationProc, EnumWindowStations, EnumChildProc
    EnumChildWindows, EnumThreadWindows, EnumThreadWndProc, EnumWindows, EnumWindowsProc    Revision: 25/4/2007 - Roger
    }
    TEnumWindowCallBack = function(Wnd : HWND; Message : LPARAM) : BOOL; stdcall;



//As Rotinas abaixo podem ser definidas apenas para o windows domestico
{$IFNDEF WINNT}
//Registra um aplicativo como servico do Windows 95( NAO DISPONIVEL NO WINDOWS NT )
function RegisterServiceProcess(dwProcessID, dwType : Integer) : Integer; stdcall; external 'KERNEL32.DLL';
function GetActiveModulesList(List : TStrings) : boolean;
function GetActiveProcessList(List : TStrings) : boolean;
function GetAllActiveModulesList(List : TStrings) : boolean;
function GetProcessInfoArray(var ProcArray : array of TProcessEntry32) : Integer;
function GetProcessModules(List : TStrings; th32ProcessID : DWORD) : boolean;
function IsModuleLoaded(FileName : string) : boolean;
function MainThreadProcess(th32ProcessID : cardinal) : cardinal;
{$ENDIF}

function CheckWinAPI(LastError : Integer) : boolean; deprecated; //Deve passar a usar APIHnd.CheckAPI
 //Indica se existe depurador carregado no momento pelo Windows
function DebuggerLoaded() : boolean;
function FindDOSWindowApp(ProcessId, AThread : DWORD; ExeName : string) : HWnd;
function FindWindowByTitle(const Title : string) : HWnd;
function FindWindowChildByClass(const ParentWnd : HWnd; const ClassName : string) : HWnd;
function FindWindowChildByTitle(const ParentWnd : HWnd; const Title : string) : HWnd;
function FindWindowClassInThread(ThreadId : Integer; ClassName : PChar) : HWnd;
//#### A ser depurada #### function GetModuleNameByWinHnd( Hnd : HWND ) : string;
procedure HideAppTaskButton(AppHandle : DWORD);
//Ajsuta token de privilégio para outra conta
function ImpersonateAnotherUser( const UserName, Pwd : string ) : Integer;
function IsThreadValid(dwThreadID : longword) : boolean;
function LookForClass(hWnd : HWND; lParam : LPARAM) : BOOL; stdcall;
//Simula uma suspensao da tarefa em 16 bits
procedure Sleep16(const App : TApplication; const WaitTime : longint);
//Seta estado do aplicativo como "Ocioso"
function SetSystemIdle : boolean;
//Seta estado do aplicativo como "Ocupado"
procedure SetSystemBusy;
//Retorna o caminho da pasta do Windows
function WinDir : string;
//Executa modulo dado por Exename e espera seu termino para continuar prog, retorna process termination status
function WinExecAndWait32(ExeName : string; Visibility : Word; ProcMessages : boolean) : DWORD;
//Retorna o caminho da pasta do SYSTEM do Windows
function WinSystemDir : string;

implementation

const
    _APP_TIME_RESPONSE_ = 500;

var
    BusyCountReference : Integer = 0;


{$IFNDEF WINNT}
//As Rotinas cuja implementacao se encontram aqui podem ser usadas apenas no Windows domestico
function GetActiveModulesList(List : TStrings) : boolean;
    //------------------------------------------------------------------------------
var
    Handle :  THandle;
    ModInfo : TModuleEntry32;
begin
    List.Clear;
    Result := False;
    Handle := CreateToolhelp32Snapshot(TH32CS_SNAPALL, 0); //Acesso completo
    if longint(Handle) <> -1 then begin
        ModInfo.dwSize := SizeOf(TModuleEntry32);
        if Module32First(Handle, ModInfo) then begin
            Result := True;
            List.Add(ModInfo.szExePath);
            while Module32Next(Handle, ModInfo) do begin
                ModInfo.dwSize := SizeOf(TModuleEntry32); //Valor pode ser mudado
                List.Add(ModInfo.szExePath);
            end;
        end;
        CloseHandle(Handle);
    end;
end;

function GetActiveProcessList(List : TStrings) : boolean;
    //----------------------------------------------------------------------------------------------------------------------
var
    Handle :   THandle;
    ProcInfo : TProcessEntry32;
begin
    Result := False;
    List.BeginUpdate;
    try
        List.Clear;
        Handle := CreateToolhelp32Snapshot(TH32CS_SNAPALL, 0); //Acesso completo
        if longint(Handle) <> -1 then begin
            ProcInfo.dwSize := SizeOf(TProcessEntry32);
            if Process32First(Handle, ProcInfo) then begin
                Result := True;
                List.Add(ProcInfo.szExeFile);
                while Process32Next(Handle, ProcInfo) do begin
                    ProcInfo.dwSize := SizeOf(TProcessEntry32); //Valor pode ser mudado
                    List.Add(ProcInfo.szExeFile);
                end;
            end;
            CloseHandle(Handle);
        end;
    finally
        List.EndUpdate;
    end;
end;

procedure GetActiveWin32ProcessList(PList : TStringList);
//----------------------------------------------------------------------------------------------------------------------------------
//Carrega lista de processos
var
    I :    Integer;
    pidNeeded : DWORD;
    PIDList : array[0..1000] of Integer;
    PIDName : array [0..MAX_PATH - 1] of char;
    PH :   THandle;
    hMod : HMODULE;
    dwSize2 : DWORD;
begin
    PList.Clear;
    if not EnumProcesses(PDWORD(@PIDList), 1000, pidNeeded) then begin
        raise Exception.Create('PSAPI.DLL Falhou ao ser carregada');
    end;
    for i := 0 to (pidNeeded div SizeOf(Integer) - 1) do begin
        PH := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, PIDList[I]);
        if PH <> 0 then begin
            if GetModuleFileNameExA(PH, 0, PIDName, SizeOf(PIDName)) > 0 then begin
                if EnumProcessModules(PH, @hMod, SizeOf(hMod), dwSize2) then begin
                    GetModuleFileNameExA(PH, hMod, PIDName, SizeOf(PIDName));
                    PList.Add(PIDName);
                end;
                CloseHandle(PH);
            end;
        end;
    end;
end;

function GetAllActiveModulesList(List : TStrings) : boolean;
    //------------------------------------------------------------------------------
var
    Handle : THandle;
    ProcInfo : TProcessEntry32;
    SubMods : TStringList;
    i : Integer;
begin
    Result := False;
    List.BeginUpdate;
    try
        List.Clear;
        Handle := CreateToolhelp32Snapshot(TH32CS_SNAPALL, 0);//Acesso completo para os processos
        if longint(Handle) <> -1 then begin
            ProcInfo.dwSize := SizeOf(TProcessEntry32);
            SubMods := TStringList.Create;
            if Process32First(Handle, ProcInfo) then begin
                Result := True;
                repeat
                    if List.IndexOf(ProcInfo.szExeFile) = -1 then begin
                        List.Add(ProcInfo.szExeFile);
                    end;
                    //Busca modulos do processo da vez
                    GetProcessModules(SubMods, ProcInfo.th32ProcessID);
                    for i := 0 to SubMods.Count - 1 do begin
                        if List.IndexOf(SubMods.Strings[i]) = -1 then begin
                            List.Add(SubMods.Strings[i]);
                        end;
                    end;
                    ProcInfo.dwSize := SizeOf(TProcessEntry32); //Valor pode ser mudado
                until (not Process32Next(Handle, ProcInfo));
            end;
            SubMods.Free;
            CloseHandle(Handle);
        end;
    finally
        List.EndUpdate;
    end;
end;

function GetProcessInfoArray(var ProcArray : array of TProcessEntry32) : Integer;
    //------------------------------------------------------------------------------
var
    i :      Integer;
    Handle : HWND;
    ProcInfo : TProcessEntry32;
begin
    Result := 0;
    //GetHeapStatus; //maluquices do escalonador
    Handle := CreateToolhelp32Snapshot(TH32CS_SNAPALL, 0); //Acesso completo
    if longint(Handle) <> -1 then begin
        ProcInfo.dwSize := SizeOf(TProcessEntry32); //Nao setar este valor foi uma merda!
        if Process32First(Handle, ProcInfo) then begin
            for i := Low(ProcArray) to High(ProcArray) - 1 do begin
                ProcArray[i] := ProcInfo;
                Inc(Result);
                ProcInfo.dwSize := SizeOf(TProcessEntry32); //Valor pode ser alterado
                if not Process32Next(Handle, ProcInfo) then begin
                    Break;
                end;
            end;
        end;
        CloseHandle(Handle);
    end;
end;

function GetProcessModules(List : TStrings; th32ProcessID : DWORD) : boolean;
    //------------------------------------------------------------------------------
var
    FirstHandle : THandle;
    ModInfo :     TModuleEntry32;
begin
    Result := False;
    List.Clear;
    FirstHandle := CreateToolhelp32Snapshot(TH32CS_SNAPALL, th32ProcessID); //Acesso completo
    if longint(FirstHandle) <> -1 then begin
        ModInfo.dwSize := SizeOf(TModuleEntry32);
        if Module32First(FirstHandle, ModInfo) then begin
            Result := True;
            repeat
                List.Add(ModInfo.szExePath);
                ModInfo.dwSize := SizeOf(TModuleEntry32); //Valor pode ser alterado
            until not Module32Next(FirstHandle, ModInfo);
        end;
        CloseHandle(FirstHandle);
    end;
end;

function IsModuleLoaded(FileName : string) : boolean;
    //------------------------------------------------------------------------------
var
    AList : TStringList;
begin
    AList := TStringList.Create;
    if GetAllActiveModulesList(AList) then begin
        Result := (AList.IndexOf(UpperCase(FileName)) <> -1);
    end else begin
        Result := False;
    end;
    AList.Free;
end;

function MainThreadProcess(th32ProcessID : cardinal) : cardinal;
    //------------------------------------------------------------------------------
var
    Snap : HWND;
    ThreadInfo : TTHREADENTRY32;
begin
    Result := 0;
    Snap   := CreateToolhelp32Snapshot(TH32CS_SNAPALL, th32ProcessID); //Acesso completo
    if longint(Snap) <> -1 then begin
        ThreadInfo.dwSize := SizeOf(TTHREADENTRY32);
        if Thread32First(Snap, ThreadInfo) then begin
            repeat
                if ThreadInfo.cntUsage > 0 then begin
                    Result := ThreadInfo.th32ThreadID;
                end;
                ThreadInfo.dwSize := SizeOf(TTHREADENTRY32); //Valor pode ser alterado
            until (Result <> 0) or (not Thread32Next(Snap, ThreadInfo));
        end;
        CloseHandle(Snap);
    end;
end;

{$ENDIF}


function CheckWinAPI(LastError : Integer) : boolean;
    //----------------------------------------------------------------------------------------------------------------------------------
    //Deve passar a usar APIHnd.CheckAPI
var
    Error : EOSError;
begin
    if LastError <> 0 then begin
        Error := SysUtils.EOSError.CreateResFmt(PResStringRec(@SysConst.SOSError),
            [LastError, SysUtils.SysErrorMessage(LastError)]);
        Error.ErrorCode := LastError;
        raise Error;
    end else begin
        Result := True;
    end;
end;

function DebuggerLoaded() : boolean;
    //----------------------------------------------------------------------------------------------------------------------------------
    //Indica se existe depurador carregado no momento pelo Windows
type
    TDebugProc = function : boolean; stdcall;
var
    Kernel32 :  HMODULE;
    DebugProc : TDebugProc;
begin
    Result   := False;
    Kernel32 := GetModuleHandle('kernel32.dll');
    if Kernel32 <> 0 then begin
        @DebugProc := GetProcAddress(Kernel32, 'IsDebuggerPresent');
        if Assigned(DebugProc) then begin
            Result := DebugProc();
        end;
    end;
end;

function FindDOSWindowApp(ProcessId, AThread : DWORD; ExeName : string) : HWnd;
    //----------------------------------------------------------------------------------------------------------------------------------
type
    TLocalParam = record
        DosWin: THandle;
        Proc:   longword;
    end;
    PLocalParam = ^TLocalParam;
var
    LocalParam : TLocalParam;

    {.............................................................................................}
    function CheckParent(HWND : hwnd; LPARAM : lParam) : BOOL; stdcall;
    var
        Local :    PLocalParam;
        WinClass : array[0..64] of char;
    begin
        Local := Pointer(lParam);
        //####Impedia a total execucaoWaitForInputIdle( Local^.Proc, _APP_TIME_RESPONSE_);
        GetClassName(HWND, WinClass, SizeOf(WinClass));
        //A unica janela de um process "DOS" com a classe "TTY"
        if (StrPos(WinClass, 'tty') <> nil) then begin //Para NT tty equivale a 'ConsoleWindowClass' ver versao e mudar valor
            Local^.DosWin := HWND;
        end;
        //####Impedia a total execucaoWaitForInputIdle( Local.Proc, _APP_TIME_RESPONSE_);
        Result := (Local^.DosWin = 0);
    end;

    {.............................................................................................}
    //************* Entrada de FindDOSWindowApp ************
begin
    with LocalParam do begin
        Proc   := ProcessId;
        DosWin := 0;
    end;
    EnumThreadWindows(AThread, @CheckParent, longint(@LocalParam));
    Result := LocalParam.DosWin;
end;

function FindWindowByTitle(const Title : string) : HWnd;
    //------------------------------------------------------------------------------
var
    PrevWin, NextWin : HWnd;
    WinName : array[0..256] of char;
begin
    NextWin := FindWindow(nil, nil);
    GetWindowText(NextWin, WinName, SizeOf(WinName));
    repeat
        PrevWin := NextWin;
        GetWindowText(NextWin, WinName, SizeOf(WinName));
        NextWin := GetNextWindow(NextWin, GW_HWNDNEXT);
    until (PrevWin = NextWin) or (StrPas(WinName) = Title);
    if (StrComp(WinName, PChar(Title)) = 0) then begin
        Result := PrevWin;
    end else begin
        Result := 0;
    end;
end;

function FindWindowChildByClass(const ParentWnd : HWnd; const ClassName : string) : HWnd;
    //------------------------------------------------------------------------------
var
    W1st, Wth : HWnd;
    WinClass :  array[0..256] of char;
begin
    Result := 0;
    W1st   := GetWindow(ParentWnd, GW_CHILD);
    if W1st <> 0 then begin
        Wth := W1st;
        repeat
            GetClassName(Wth, WinClass, SizeOf(WinClass));
            if (SameText(WinClass, ClassName)) then begin
                Result := Wth;
                break;
            end;
            Wth := GetNextWindow(Wth, GW_HWNDNEXT);
        until (Wth = 0) or (Wth = W1st);
    end;
end;

function FindWindowChildByTitle(const ParentWnd : HWnd; const Title : string) : HWnd;
    //------------------------------------------------------------------------------
var
    W1st, Wth : HWnd;
    WinName :   PChar;
begin
    Result := 0;
    W1st   := GetWindow(ParentWnd, GW_CHILD);
    if W1st <> 0 then begin
        WinName := StrAlloc(256);
        Wth     := W1st;
        repeat
            GetWindowText(Wth, WinName, 255);
            if StrPas(WinName) = Title then begin
                Result := Wth;
                break;
            end;
            Wth := GetNextWindow(Wth, GW_HWNDNEXT);
        until (Wth = 0) or (Wth = W1st);
        StrDispose(WinName);
    end;
end;

function FindWindowClassInThread(ThreadId : Integer; ClassName : PChar) : HWnd;
    //------------------------------------------------------------------------------
var
    WinHandle : HWnd;
    //funcao principal
begin
    WinHandle := 0;
    EnumThreadWindows(ThreadID, @LookForClass, longint(ClassName));
    Result := WinHandle;
end;

procedure HideAppTaskButton(AppHandle : DWORD);
{-------------------------------------------------------------------------------------------------------------}
begin
    ShowWindow(AppHandle, SW_HIDE);
    SetWindowLong(AppHandle, GWL_EXSTYLE, GetWindowLong(Application.Handle, GWL_EXSTYLE)
        or WS_EX_TOOLWINDOW and not WS_EX_APPWINDOW);
end;

function ImpersonateAnotherUser(const UserName, Pwd : string ) : Integer;
	 //----------------------------------------------------------------------------------------------------------------------------------
var
	 TKHandle :   THandle;
	 User, Pass : PChar;
begin
	 User   := PChar(UserName);
	 Pass   := PChar(Pwd);
	 Result := ERROR_SUCCESS;
	 SetLastError(Result);
	 if LogonUser(User, nil, Pass, LOGON32_LOGON_INTERACTIVE, LOGON32_PROVIDER_DEFAULT, TKHandle) then begin
		 if not ImpersonateLoggedOnUser(TKHandle) then begin
			 Result := GetLastError();
		 end;
	 end else begin
		 Result := GetLastError();
	 end;
end;


function IsThreadValid(dwThreadID : longword) : boolean;
	 {-------------------------------------------------------------------------------------------------------------}
	 {..................................................................................................}
	 function ThreadWin(hwnd : HWND; lparam : LPARAM) : BOOL; stdcall;
	 begin
		 Result := True;
	 end;

	 {..................................................................................................}
begin
	 Result := EnumThreadWindows(dwThreadID, @ThreadWin, 0);
end;


function LookForClass(hWnd : HWND; lParam : LPARAM) : BOOL; stdcall;
    //------------------------------------------------------------------------------
    { TODO -oRoger -cFW : Descobrir e colocar documentacao a respeito deste metodo }
var
    CName : array[0..100] of char;
begin
    Result := True;
    GetClassName(hWnd, CName, 100);
    if StrIComp(CName, PChar(lParam)) = 0 then begin
        //WinHandle:=HWnd;
    end;{
        Result:=False;
    end else begin
        Result:=True;
    end;
    }
end;

procedure Sleep16(const App : TApplication; const WaitTime : longint);
//----------------------------------------------------------------------------------------------------------------------------------
//Simula uma suspensao da tarefa em 16 bits, pode dispensar Application
//..................................................................................................................................
    procedure LSRDoNothing();
    var
        i : Integer;
    begin
        for i := 0 to 65350 do begin
            //Nada
        end;
    end;
    //..................................................................................................................................
var
    OverTime : boolean;
    MaxDif, Before : double;
begin
    MaxDif := WaitTime / (86400000); // = 24*60*60*1000;
    Before := Now;
    repeat
        if Assigned(App) then begin
            TApplication(App).ProcessMessages;
        end else begin
            LSRDoNothing();
        end;
        OverTime := ((Now - Before) >= MaxDif);
    until OverTime;
end;

function SetSystemIdle : boolean;
    //----------------------------------------------------------------------------------------------------------------------
    //Seta estado do aplicativo como "Ocioso"
begin
(*
How to change the cursor global?

var
fic,foic : HCursor;

begin
  fic:=LoadCursor(hinstance, makeIntResource(1)); // load any cursor
  foic:=GetCursor; // save handle of old cursor
  SetSystemCursor(fic, OCR_NORMAL); // use new cursor
  { Restoring cursor can be done by:
     SetSystemCursor(foic, OCR_NORMAL); }
end;
*)
    Dec(BusyCountReference);
    if BusyCountReference <= 0 then begin
        BusyCountReference := 0; //Normaliza multiplas/excessivas chamadas a este metodo
        Screen.Cursor      := crDefault;
        ReleaseCapture();
        Result := True;
    end else begin
        Result := False;
    end;
end;

procedure SetSystemBusy;
 //----------------------------------------------------------------------------------------------------------------------
 //Seta estado do aplicativo como "Ocupado"
begin
    Inc(BusyCountReference);
    Screen.Cursor := crHourGlass;
    SetCapture(Application.Handle);
end;

function WinDir : string;
    //----------------------------------------------------------------------------------------------------------------------
    //Retorna o caminho da pasta do Windows
var
    L : Integer;
begin
    SetLength(Result, MAX_PATH);
    L := GetWindowsDirectory(PChar(Result), MAX_PATH);
    SetLength(Result, L);
end;

function WinExecAndWait32(ExeName : string; Visibility : Word; ProcMessages : boolean) : DWORD;
    {-------------------------------------------------------------------------------------------------------------}
var
    zAppName :    array[0..512] of char;
    zCurDir :     array[0..255] of char;
    WorkDir :     string;
    StartupInfo : TStartupInfo;
    ProcessInfo : TProcessInformation;
begin
    StrPCopy(zAppName, ExeName);
    GetDir(0, WorkDir);
    StrPCopy(zCurDir, WorkDir);
    FillChar(StartupInfo, Sizeof(StartupInfo), #0);
    StartupInfo.cb      := Sizeof(StartupInfo);
    StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
    StartupInfo.wShowWindow := Visibility;
    if not CreateProcess(nil, zAppName, nil, nil, False, CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS, nil, zCurDir,
        StartupInfo, ProcessInfo) then begin
        Result := 0;
    end else begin
        if ProcMessages then begin
            Result := WAIT_TIMEOUT;
            while Result = WAIT_TIMEOUT do begin
                Result := WaitforSingleObject(ProcessInfo.hProcess, 1000);
                if Result = WAIT_FAILED then begin
                    RaiseLastOSError; //**
                end;
                Application.ProcessMessages;
            end;
        end else begin
            WaitforSingleObject(ProcessInfo.hProcess, INFINITE);
            GetExitCodeProcess(ProcessInfo.hProcess, Result);
        end;
    end;
end;

function WinSystemDir : string;
    //----------------------------------------------------------------------------------------------------------------------
    //Retorna o caminho da pasta do SYSTEM do Windows
var
    L : Integer;
begin
    SetLength(Result, MAX_PATH);
    L := GetSystemDirectory(PChar(Result), MAX_PATH);
    SetLength(Result, L);
end;

{-----------------------------------------------------------------------------}



(*

    ROTINA EM VISUAL BOSTA PARA CAPTURA AS JANELAS DAS APLICAÇÕES QUE APARECEM NA BARRA DE TAREFAS


Private Sub RefreshTopWinList()
     Dim sTitle As String, hWnd As Long
     SetRedraw lstTopWin, False
     lstTopWin.Clear
     ' Get first top-level window
     hWnd = GetWindow(GetDesktopWindow(), GW_CHILD)
     BugAssert hWnd <> hNull
     ' Iterate through remaining windows
     Do While hWnd <> hNull
         sTitle = WindowTextLineFromWnd(hWnd)
         ' Determine whether to display titled, visible, and unowned
         If IsVisibleTopWnd(hWnd, chkBlank, _
                            chkInvisible, chkOwned) Then
             lstTopWin.AddItem sTitle
             lstTopWin.ItemData(lstTopWin.NewIndex) = hWnd
         End If
         ' Get next child
         hWnd = GetWindow(hWnd, GW_HWNDNEXT)
     Loop
     SetRedraw lstTopWin, True
End Sub

Function IsVisibleTopWnd(hWnd As Long, _
                Optional IgnoreEmpty As Boolean = False, _
                Optional IgnoreVisible As Boolean = False, _
                Optional IgnoreOwned As Boolean = False) _
                As Boolean
    If IgnoreEmpty Or WindowTextFromWnd(hWnd) <> sEmpty Then
         If IgnoreVisible Or IsWindowVisible(hWnd) Then
            If IgnoreOwned Or GetWindow(hWnd, GW_OWNER) = hNull Then
                IsVisibleTopWnd = True
            End If
        End If
    End If
End Function

If IsVisibleTopWnd(hWnd, chkBlank, _
                    chkInvisible, chkOwned) Then
     lstTopWin.AddItem sTitle
     lstTopWin.ItemData(lstTopWin.NewIndex) = hWnd
End If



***************** ROTINA EM C++ PARA CAPTURAR OS PROCESSOS ATIVOS

#include <windows.h>
#include <tlhelp32.h>
#include <stdio.h>

BOOL GetProcessList ()
{
     HANDLE         hProcessSnap = NULL;
     BOOL           bRet      = FALSE;
     PROCESSENTRY32 pe32      = {0};

     //  Take a snapshot of all processes in the system.

     hProcessSnap = CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);

     if (hProcessSnap == (HANDLE)-1)
         return (FALSE);

     //  Fill in the size of the structure before using it.

     pe32.dwSize = sizeof(PROCESSENTRY32);

     //  Walk the snapshot of the processes, and for each process,
     //  display information.

     if (Process32First(hProcessSnap, &pe32))
     {
         DWORD         dwPriorityClass;
         BOOL          bGotModule = FALSE;
         MODULEENTRY32 me32       = {0};

         do
         {
             bGotModule = GetProcessModule(pe32.th32ProcessID,
                 pe32.th32ModuleID, &me32, sizeof(MODULEENTRY32));

             if (bGotModule)
             {
                 HANDLE hProcess;

                 // Get the actual priority class.
                 hProcess = OpenProcess (PROCESS_ALL_ACCESS,
                     FALSE, pe32.th32ProcessID);
                 dwPriorityClass = GetPriorityClass (hProcess);
                 CloseHandle (hProcess);

                 // Print the process's information.
                 printf( "\nPriority Class Base\t%d\n",
                     pe32.pcPriClassBase);
                 printf( "PID\t\t\t%d\n", pe32.th32ProcessID);
                 printf( "Thread Count\t\t%d\n", pe32.cntThreads);
                 printf( "Module Name\t\t%s\n", me32.szModule);
                 printf( "Full Path\t\t%s\n\n", me32.szExePath);
             }
         }
         while (Process32Next(hProcessSnap, &pe32));
        bRet = TRUE; 
    } 
    else 
        bRet = FALSE;    // could not walk the list of processes 
 
    // Do not forget to clean up the snapshot object. 

     CloseHandle (hProcessSnap);
     return (bRet);
}


//#### A ser depurada #### function GetModuleNameByWinHnd( Hnd : HWND ) : string;
function GetModuleNameByWinHnd( Hnd : HWND ) : string;
{-------------------------------------------------------------------------------------------------------------}
var
    Instance : HMODULE;
    Buf : PChar;
    nSize : integer;
begin
    Instance:=Windows.GetWindowLong( Hnd, GWL_HINSTANCE);
    nSize:=GetModuleFileName( Instance, Buf, 0);
    nSize:=32400;
    if nSize <> 0 then begin
        Buf:=StrAlloc( nSize + 1 );
        GetModuleFileName(Instance, Buf, nSize);
        Result:=string( Buf );
        StrDispose( Buf );
    end else begin
        Result:=SysErrorMessage( GetLastError );
        //Result:=EmptyStr;
    end;
end;



function WinExecAndWait32(Path: PChar; Visibility: Word): integer;
var Msg: TMsg;
     { Delphi 3:    lpExitCode: integer; }
     { Delphi 4 information courtesy of Joel Milne }
     { Delphi 4: }  lpExitCode: cardinal;
     StartupInfo: TStartupInfo;
     ProcessInfo: TProcessInformation;
begin
  FillChar(StartupInfo, SizeOf(TStartupInfo), 0);
  with StartupInfo do
  begin
     cb := SizeOf(TStartupInfo);
     dwFlags := STARTF_USESHOWWINDOW or STARTF_FORCEONFEEDBACK;
     wShowWindow := visibility; {you could pass sw_show or sw_hide as parameter}
  end;

  if CreateProcess(nil, path, nil, nil, False, NORMAL_PRIORITY_CLASS, nil, nil, StartupInfo,
                    ProcessInfo) then
  begin
     repeat
       while PeekMessage(Msg, 0, 0, 0, pm_Remove) do
       begin
         if Msg.Message = wm_Quit then Halt(Msg.WParam);
         TranslateMessage(Msg);
        DispatchMessage(Msg); 
      end; 
      GetExitCodeProcess(ProcessInfo.hProcess,lpExitCode); 
    until lpExitCode <> Still_Active; 

    with ProcessInfo do {not sure this is necessary but seen in in some code elsewhere} 
    begin 
      CloseHandle(hThread); 
      CloseHandle(hProcess); 
    end; 
    Result := 0; {success} 
  end else Result := GetLastError; 
end; 
  

*)
end.
