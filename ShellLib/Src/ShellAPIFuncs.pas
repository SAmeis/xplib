{$IFDEF ShellAPIFuncs}
	{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I ShellLib.inc}

unit ShellAPIFuncs;

interface

uses
    Windows, Messages, ShlObj, SysUtils, Classes, Graphics, ShellPIDL;


const
    Shell32 = 'shell32.dll';

      {$IF CompilerVersion < 21.00}
	 SHFormatDrive_Name = 'SHFormatDriveA';

	 PickIconDlg_Name = 'PickIconDlg'; // old index  PickIconDlg_Index = 62;
	  {$IFEND}

	{$IF CompilerVersion < 21.00}
	function SHFormatDrive(Owner : HWND; Drive : UINT; FormatID : UINT; OptionFlags : UINT) : DWORD; stdcall;

	function PickIconDlg(Owner : HWND; FileName : PWideChar; MaxFileNameChars : UINT; var IconIndex : Integer) : Integer; stdcall;
	{$IFEND}


    {$IF CompilerVersion < 21.00}

function SHFormatDrive; external Shell32 Name SHFormatDrive_Name;

function PickIconDlg; external Shell32 Name PickIconDlg_Name; //Minimum = XP
    {$IFEND}


implementation

end.
