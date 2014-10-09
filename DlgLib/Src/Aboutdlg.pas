{$IFDEF Aboutdlg}
{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I DlgLib.inc}
unit Aboutdlg;

interface

uses
	Windows, SysUtils, Classes, Forms, Dialogs, About;

type
	TAboutBoxDlg = class(TComponent)
	private
		FProductName: string;
		FVersion    : string;
		FCopyright  : string;
		FComments   : string;
		fileHandle  : THandle;
		fileBuffer  : array [0 .. 29] of Char;
		wVersion    : Word;
		dVersion    : Word;
		SysInfo     : SYSTEM_INFO;
		MemStat     : MEMORYSTATUS;
	public
		function Execute: Boolean;
	published
		property ProductName: string read FProductName write FProductName;
		property Version    : string read FVersion write FVersion;
		property Copyright  : string read FCopyright write FCopyright;
		property Comments   : string read FComments write FComments;
	end;

var
	AboutBox: TAboutBox;

implementation

function TAboutBoxDlg.Execute: Boolean;
//----------------------------------------------------------------------------------------------------------------------------------
begin
	{$WARN UNSAFE_CODE OFF}
	//Create dialog in memory
	AboutBox := TAboutBox.Create(Application);
	//Set dialog strings
	AboutBox.ProductName.Caption := ProductName;
	AboutBox.Version.Caption     := Version;
	AboutBox.Copyright.Caption   := Copyright;
	AboutBox.Comments.Caption    := Comments;
	AboutBox.Caption             := 'About ' + ProductName;
	//Get Win/Dos version numbers
	wVersion                    := LoWord(GetVersion);
	dVersion                    := HiWord(GetVersion);
	AboutBox.WinVersion.Caption := IntToStr(LO(wVersion)) + '.' + IntToStr(HI(wVersion));
	AboutBox.DosVersion.Caption := IntToStr(HI(dVersion)) + '.' + IntToStr(LO(dVersion));
	//Get CPU type
	GetSystemInfo(SysInfo);
	AboutBox.CPU.Caption := 'Processor Type : ' + IntToStr(SysInfo.dwProcessorType) + ' Revisão:' +
		IntToStr(SysInfo.wProcessorRevision);
	//Get free memory, resources, disk space
	AboutBox.FreeMemory.Caption := IntToStr(GetFreeSpace(0) div 1000) + ' KB';
	GlobalMemoryStatus(MemStat);
	AboutBox.FreeResources.Caption :=
		Format('%f %%', [100 * ((MemStat.dwAvailPhys + MemStat.dwAvailVirtual) / (MemStat.dwTotalVirtual + MemStat.dwAvailPhys))]);
	//Get user name and company name
	fileHandle := LoadLibrary('USER');
	if fileHandle >= HINSTANCE_ERROR then begin
		{$TYPEDADDRESS OFF}
		if LoadString(fileHandle, 514, @fileBuffer, 30) <> 0 then begin
			AboutBox.UserName.Caption := fileBuffer;
		end;
		if LoadString(fileHandle, 515, @fileBuffer, 30) <> 0 then begin
			AboutBox.CompanyName.Caption := fileBuffer;
		end;
		{$TYPEDADDRESS ON}
		FreeLibrary(fileHandle);
	end;
	with AboutBox do begin
		ProgramIcon.Picture.Graphic := Application.Icon;
		Result                      := (ShowModal = IDOK);
	end;
	AboutBox.Free;
	{$WARN UNSAFE_CODE ON}
end;

end.
