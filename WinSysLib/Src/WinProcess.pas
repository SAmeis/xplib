{$IFDEF WinProcess}
	{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I WinSysLib.inc}

unit WinProcess;

interface

type
	TProcessMemoryInfo = record
		PhysicalMemoryUsage: integer;
		VirtualMemoryUsage: integer;
		PeakPhysicalMemoryUsage: integer;
		PeakVirtualMemoryUsage: integer;
		PageFaultCount: integer;
	end;

function GetProcessMemoryInfo(PID : Cardinal) : TProcessMemoryInfo;
function GetProcessCPUTime(PID : Cardinal) : Int64;
function GetProcessCPUUsagePerc(PID : Cardinal; Interval : integer) : integer;
function GetProcessStartTime(PID : Cardinal) : TDateTime;


implementation

uses psapi, windows, SysUtils;


function GetProcessMemoryInfo(PID : Cardinal) : TProcessMemoryInfo;
	//----------------------------------------------------------------------------------------------------------------------------------
	// retorna um record com informações de uso de memória (física e virtual), pico de uso de memória e falhas de página.
var
	myHandle : THandle;
	MemoryInfo : psapi.TProcessMemoryCounters;
begin
	// Abre um handle para o processo...
	myHandle := OpenProcess(PROCESS_ALL_ACCESS, FALSE, PID);
	// Pega as informações de memória...
	try
		psapi.GetProcessMemoryInfo(myHandle, @MemoryInfo, SizeOf(MemoryInfo));
		Result.PhysicalMemoryUsage := MemoryInfo.WorkingSetSize;
		Result.VirtualMemoryUsage := MemoryInfo.PagefileUsage;
		Result.PeakPhysicalMemoryUsage := MemoryInfo.PeakWorkingSetSize;
		Result.PeakVirtualMemoryUsage := MemoryInfo.PeakPagefileUsage;
		Result.PageFaultCount := MemoryInfo.PageFaultCount;
	finally
		// Fecha o handle do processo...
		CloseHandle(myHandle);
	end;
end;


function GetProcessCPUTime2(PID : Cardinal) : Int64;
	//----------------------------------------------------------------------------------------------------------------------------------
	// Retorna o tempo de processador que o processo usou desde a sua inicialização em unidades de nanosegundos.
var
	myHandle : THandle;
	CreationTime, ExitTime, UserTime, KernelTime : TFileTime;
	Kernel64, User64 : Int64;
begin
	// zera as variáveis para evitar que fiquem com lixo...
	KernelTime.dwHighDateTime := 0;
	KernelTime.dwLowDateTime := 0;
	UserTIme.dwLowDateTime := 0;
	UserTime.dwHighDateTime := 0;

	// Abre um handle para o processo...
	myHandle := OpenProcess(PROCESS_QUERY_INFORMATION, FALSE, PID);
	try
		// Pega as informações de tempo...
		GetProcessTimes(myHandle, CreationTime, ExitTime, KernelTime, UserTime);

		// Calcula os valores de tempo do kernel e user compondo os dois bytes 32 bits para um de 64 bits.
		Kernel64 := KernelTime.dwHighDateTime;
		Kernel64 := (Kernel64 shl 32) + KernelTime.dwLowDateTime;
		User64 := UserTime.dwHighDateTime;
		User64 := (User64 shl 32) + UserTime.dwLowDateTime;

		Result := (Kernel64 + User64);
	finally
		// Fecha o handle do processo...
		CloseHandle(myHandle);
	end;
end;


function GetProcessCPUTime(PID : Cardinal) : Int64;
	//----------------------------------------------------------------------------------------------------------------------------------
	// Retorna o tempo de processador que o processo usou desde a sua inicialização em milisegundos.
begin
	Result := GetProcessCPUTime2(PID) div 10000; // Transforma unidades de 100 nanosegundos em milisegundos.
end;


function GetProcessCPUUsagePerc(PID : Cardinal; Interval : integer) : integer;
	//----------------------------------------------------------------------------------------------------------------------------------
	// Retorna o percentual de utilização de CPU do processo identificado por PID nos próximos Interval milesegundos.
var
	InitValue, FinalValue : Int64;
begin
	InitValue := GetProcessCPUTime2(PID);
	Sleep(Interval);
	FinalValue := GetProcessCPUTime2(PID);
	//%    //Acha a dif. no tempo  // Transforma o intervalo passado em unidades de 100 nanosegundos
	Result := 100 * (FinalValue - InitValue) div (Interval * 10000);
end;


function GetProcessStartTime(PID : Cardinal) : TDateTime;
	//------------------------------------------------------------------------------------------------------------------------
	// Retorna a um TdateTime contendo a data e a hora que o processo foi startado. Recebe o PID do Processo
var
	myHandle : THandle;
	CreationTime, ExitTime, UserTime, KernelTime : TFileTime;
	ProcSystemTime : TSystemTime;

begin
	// zera as variáveis para evitar que fiquem com lixo...
	CreationTime.dwLowDateTime := 0;
	CreationTime.dwHighDateTime := 0;

	// Abre um handle para o processo...
	myHandle := OpenProcess(PROCESS_QUERY_INFORMATION, FALSE, PID);
	try
		// Pega as informações de tempo...
		GetProcessTimes(myHandle, CreationTime, ExitTime, KernelTime, UserTime);

		// Converte um TFileTime em TSystemTime...
		FileTimeToSystemTime(CreaTionTime, ProcSystemTime);

		// Retorna um TDateTime...
		Result := SystemTimeToDateTime(ProcSystemTime);
	finally
		// Fecha o handle do processo...
		CloseHandle(myHandle);
	end;
end;


end.


