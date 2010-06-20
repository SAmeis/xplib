{$IFDEF WinDisks}
	{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I WinSysLib.inc}

unit WinDisks;

interface

uses Windows, SysUtils;

type
	//    TDriveType=    (dtUnknown, dtNoDrive, dt3Floppy, dt5Floppy, dtFixed, dtRFixed, dtNetwork, dtCDROM, dtTape); Servicos extendidos requeridos para uso deste tipo foram removidos com WIN32

	PDeviceParams = ^TDeviceParams;

	TDeviceParams = record
		bSpecFunc: byte;		{Special functions}
		bDevType: byte;			{Device type}
		wDevAttr: word;			{Device attributes}
		wCylinders: word;		{Number of cylinders}
		bMediaType: byte;		{Media type}
		{Beginning of BIOS parameter block (BPB)}
		wBytesPerSec: word;		{Bytes per sector}
		bSecPerClust: byte;		{Sectors per cluster}
		wResSectors: word;		{Number of reserved sectors}
		bFATs: byte;			{Number of FATs}
		wRootDirEnts: word;		{Number of root-directory entries}
		wSectors: word;			{Total number of sectors}
		bMedia: byte;			{Media descriptor}
		wFATsecs: word;			{Number of sectors per FAT}
		wSecPerTrack: word;		{Number of sectors per track}
		wHeads: word;			{Number of heads}
		dwHiddenSecs: longInt;	{Number of hidden sectors}
		dwHugeSectors: longInt;	{Number of sectors if wSectors == 0}
		reserved: array[0..10] of char;
		{End of BIOS parameter block (BPB)}
	end;

{Retorna o espaco livre de uma unidade de disco}
function _DiskFree(Drive : byte) : int64;
{Retorna a capacidade total de uma unidade de disco}
function _DiskSize(Drive : Byte) : int64;
{Tests to see if a drive is ready.  (floppy there and door closed)}
function DriveReady(wDrive : word) : boolean;
{Returns current default drive}
function GetDefaultDrive : word;
{Gets drive label}
function GetDriveLabel(wDrive : word) : string; platform;
//Retorna string contendo as letras presentes na maquina segundo os tipos passados
function GetDrivesStringChain(DesiredDriveType : UINT) : string;
{Identify a CD-ROM drive}
function IsCDROMDrive(wDrive : word) : boolean;
//Salva todo o cache do disco. Funciona apenas para Win9.x
procedure SaveWin9xOSDiskCache(Drive : word);

implementation

{------------------------------------------------------------------------------}
{Retorna o espaco livre de uma unidade de disco}
function _DiskFree(Drive : byte) : int64;
var
	RootPath : array[0..4] of Char;
	RootPtr : PChar;
	SectorsPerCluster, BytesPerSector, FreeClusters, TotalClusters : cardinal;
	OS : OSVERSIONINFO;
	TotalFree, TotalExisting : int64;
begin
	RootPtr := NIL;
	if Drive > 0 then	begin
		StrCopy(RootPath, 'A:\');
		RootPath[0] := Char(Drive + $40);
		RootPtr := RootPath;
	end;
	OS.dwOSVersionInfoSize := SizeOf(OSVERSIONINFO);
	GetVersionEx(OS);
	if (OS.dwMajorVersion = 4) and (OS.dwBuildNumber < 1000) and (OS.dwPlatformId = 1) then begin //Usar modo do OSR1
		if GetDiskFreeSpace(RootPtr, SectorsPerCluster, BytesPerSector, FreeClusters, TotalClusters) then begin
			Result := (SectorsPerCluster * BytesPerSector * FreeClusters);
		end else begin
			Result := -1;
		end;
	end else begin //Usar os servicos do NT ou Win95B ou superior
		if not GetDiskFreeSpaceEx(RootPtr, Result, TotalExisting, @TotalFree) then begin
			Result := -1;
		end;
	end;
end;

{------------------------------------------------------------------------------}
function _DiskSize(Drive : Byte) : int64;
var
	RootPath : array[0..4] of Char;
	RootPtr : PChar;
	SectorsPerCluster, BytesPerSector, FreeClusters, TotalClusters : cardinal;
	OS : OSVERSIONINFO;
	TotalFree, FreeAvailble : int64;
begin
	RootPtr := NIL;
	if Drive > 0 then begin
		StrCopy(RootPath, 'A:\');
		RootPath[0] := Char(Drive + $40);
		RootPtr := RootPath;
	end;
	OS.dwOSVersionInfoSize := SizeOf(OSVERSIONINFO);
	GetVersionEx(OS);
	if (OS.dwMajorVersion = 4) and (OS.dwBuildNumber < 1000) and (OS.dwPlatformId = 1) then begin //Usar modo do OSR1
		if GetDiskFreeSpace(RootPtr, SectorsPerCluster, BytesPerSector, FreeClusters, TotalClusters) then begin
			Result := (SectorsPerCluster * BytesPerSector * TotalClusters);
		end else begin
			Result := -1;
		end;
	end else begin //Usar os servicos do NT ou Win95B ou superior
		if not GetDiskFreeSpaceEx(RootPtr, FreeAvailble, Result, @TotalFree) then begin
			Result := -1;
		end;
	end;
end;

{------------------------------------------------------------------------------}
{determins if the drive is ready w/o critical errors enabled}
function DriveReady(wDrive : word) : boolean;
var
	OldErrorMode : Word;
begin
	{turn off errors}
	OldErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
	try
		if DiskSize(wDrive) = -1
		then begin
			Result := FALSE;
		end else begin
			Result := TRUE;
		end;
	finally
		{turn on errors}
		SetErrorMode(OldErrorMode);
	end;
end;

{------------------------------------------------------------------------------}

function GetDefaultDrive : word; assembler;
asm
	mov	ah, 19h			 {convert default to real}
	int 21h					{int    21h}
	xor	ah, ah				{clear hi byte, where is made the return of function}
end;

function GetDriveLabel(wDrive : word) : string; platform;
	{------------------------------------------------------------------------------}
	//Get label from drive.  0=default, 1=A...
	//return string of 11 character or "NO NAME" if not found
var
	sr : TsearchRec;
	OldErrorMode : Word;
	DotPos : Byte;
	pattern: string[6];
begin
	pattern:= 'c:\*.*';
	{get default drive}
	if wDrive = 0
	then begin
		wDrive := GetDefaultDrive;
	end else begin
		dec(wDrive);
	end;

	{switch out drive letter}
	pattern[1] := AnsiChar(65 + wDrive);

	{stop errors and try}
	OldErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
	try
		if FindFirst(Pattern, faVolumeID, sr) = 0 then begin
			Result := sr.Name;
			DotPos := Pos('.', Result);
			if DotPos <> 0 then begin
				Delete(Result, DotPos, 1);
			end;
		end else begin
			Result := 'NO NAME';
		end;
	finally
		{restore errorsa}
		SetErrorMode(OldErrorMode);
		{$IFDEF WIN32}
		FindClose(sr);
		{$ENDIF}
	end;
end;

function GetDrivesStringChain(DesiredDriveType : UINT) : string;
	//----------------------------------------------------------------------------------------------------------------------------------
	//Retorna string contendo as letras presentes na maquina segundo os tipos passados
{
    **** Tipos validos  *****
  DRIVE_UNKNOWN = 0;
  DRIVE_NO_ROOT_DIR = 1;
  DRIVE_REMOVABLE = 2;
  DRIVE_FIXED = 3;
  DRIVE_REMOTE = 4;
  DRIVE_CDROM = 5;
  DRIVE_RAMDISK = 6;
}
var
	DriveNum : Integer;
	DriveChar : Char;
	DriveBits : set of 0..25;
begin
	Result := EmptyStr;
	Integer(DriveBits) := GetLogicalDrives;
	for DriveNum := 0 to 25 do begin
		if not (DriveNum in DriveBits) then begin //Letra nao existe na maquina
			Continue;
		end;
		DriveChar := Char(DriveNum + Ord('a'));
		if GetDriveType(PChar(DriveChar + ':\')) = DesiredDriveType then begin
			Result := Result + Upcase(DriveChar);
		end;
	end;
end;

function IsCDROMDrive(wDrive : word) : boolean; assembler;
	//----------------------------------------------------------------------------------------------------------------------------------
	//Determine id drive is a CDROM, 0=default, 1=A ...
var
	wTempDrive : word;
asm
	mov	AX, wDrive
	or  AX, AX
	jnz	@not_default
	mov	AH, 19h			 {convert default to drive}
	int 21h					{int    21h}
	xor	AH, AH
	mov wTempDrive, AX
	jmp	@test_it
	@not_default:				 {zero base it}
	dec	AX
	mov wTempDrive, AX
	@test_it:
	mov AX, 1500h			 {first test for presence of MSCDEX}
	xor BX, BX
	int 2fh
	mov AX, BX				{MSCDEX is not there if BX is zero}
	or  AX, AX				{so return FALSE}
	jz  @no_mscdex
	mov AX, 150bh			 {MSCDEX driver check API}
	mov CX, wTempDrive		{...cx is drive index}
	int 2Fh
	or	 AX, AX
	@no_mscdex:
end;



procedure SaveWin9xOSDiskCache(Drive : word);
//----------------------------------------------------------------------------------------------------------------------
//Salva todo o cache do disco. Funciona apenas para Win9.x
begin
	//NOTAS : As flags podem ser
	//0000h
	// Resets the drive and flushes the file system buffers for the given drive.
	//0001h
	// Resets the drive, flushes the file system buffers, and flushes and invalidates the cache for the specified drive.
	//0002h
	// Remounts the drivespace volume
	asm
		mov ax, 710Dh;	  // Reset Drive
		mov cx, 0000h;	  // see above para os valores da flag
		mov dx, Drive;	  // see belowint 21h
	end;
end;

end.


