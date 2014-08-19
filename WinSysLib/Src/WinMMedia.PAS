{$IFDEF WinMMedia}
	{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I WinSysLib.inc}

unit WinMMedia;

interface

uses
	SysUtils, Windows, mmSystem;

function PlayWAVResourceFile(ResWAVName, ResWAVType : PChar) : integer;
function PlayWAVFile(WavFileName : string) : boolean;

implementation

function PlayWAVResourceFile(ResWAVName, ResWAVType : PChar) : integer;
	//----------------------------------------------------------------------------------------------------------------------
var
	lpRes : PChar;
	hResInfo, hRes : THandle;
begin
	// Find the WAVE resource.
	hResInfo := FindResource(hInstance, ResWAVName, ResWAVType);
	if hResInfo = 0 then begin
		Result := GetLastError;
		Exit;
	end;
	// Load the WAVE resource.
	hRes := LoadResource(hInstance, hResInfo);
	if hRes = 0 then begin
		Result := GetLastError;
		Exit;
	end;
	try
		// Lock the WAVE resource and play it.
		lpRes := LockResource(hRes);
		if lpRes <> NIL then begin
			if not sndPlaySound(lpRes, SND_MEMORY or SND_SYNC or SND_NODEFAULT) then begin
				Result := ERROR_INVALID_DATA;
			end;
			UnlockResource(hRes);
			Result := 0;
		end else begin
			Result := ERROR_RESOURCE_DATA_NOT_FOUND;
		end;
	finally
		// Free the WAVE resource and return success or failure.
		FreeResource(hRes);
	end;
end;

function PlayWAVFile(WavFileName : string) : boolean;
	{-------------------------------------------------------------------------------------------------------------}
begin
	if WavFileName <> EmptyStr then begin
		Result := sndPlaySound(PChar(WavFileName), SND_ASYNC + SND_NODEFAULT);
	end else begin
		Result := sndPlaySound(NIL, SND_ASYNC + SND_NODEFAULT);
	end;
end;

end.


 