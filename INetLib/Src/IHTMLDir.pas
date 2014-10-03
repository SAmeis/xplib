{$IFDEF IHTMLDir}
{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I INetLib.inc}
unit IHTMLDir;

{
  THTMLDirectory - generates an HTML table from a directory listing.

  Written by Keith Wood, 30 July 1998.
}

interface

uses
	Classes, SysUtils, IHTML4;

type
	THTMLDirectory = class(THTMLBase)
	private
		FDirectory:             string;
		FMask:                  string;
		FIncludeSubDirectories: Boolean;
		procedure GenerateHTMLForDir(htb: THTMLTable; sDirectory: string; iLevel: Word);
	public
		constructor Create(sDirectory, sMask: string; bIncludeSubdirectories: Boolean);
		function AsHTML: string; override; stdcall;
		property Directory: string read FDirectory write FDirectory;
		property Mask: string read FMask write FMask;
		property IncludeSubDirectories: Boolean read FIncludeSubDirectories write FIncludeSubDirectories;
	end;

implementation

{ TFileInfo ------------------------------------------------------------------- }

type
	{ Object to keep track of details about a file entry }
	TFileInfo = class(TObject)
	public
		Name: string;
		Size: Integer;
		Time: TDateTime;
		Attr: Integer;
		constructor Create(const sName: string; iSize : Integer; iTime : TDateTime; iAttr: Integer);
	end;

	{ Set values on creation }
constructor TFileInfo.Create(const sName: string; iSize : Integer; iTime : TDateTime; iAttr: Integer);
begin
	inherited Create;
	name := sName;
	Size := iSize;
	Time := iTime;
	Attr := iAttr;
end;

{ Functions ------------------------------------------------------------------- }

{ Use non-breaking spaces to provide level indents }
function Indent(iLevel: Word): string;
begin
	Result := '';
	for iLevel := iLevel downto 1 do
		Result := Result + '&nbsp;&nbsp;';
end;

{ THTMLDirectory -------------------------------------------------------------- }

{ Initialisation }
constructor THTMLDirectory.Create(sDirectory, sMask: string; bIncludeSubdirectories: Boolean);
begin
	inherited Create;
	FDirectory := sDirectory;
	FMask := sMask;
	FIncludeSubDirectories := bIncludeSubdirectories;
end;

{ Generate an HTML document showing the selected directory structure }
function THTMLDirectory.AsHTML: string;
var
	htb: THTMLTable;
	hcg: THTMLTableColumnGroup;
	htr: THTMLTableRow;
begin
	{ Then create the table to hold the directories/files }
	htb := THTMLTable.Create(AsPercentage(100), 0);
	try
		{ Align the file size column to the right }
		hcg := THTMLTableColumnGroup.Create('', 0, 0);
		hcg.Add(THTMLTableColumn.Create('', 0, 0));
		hcg.Add(THTMLTableColumn.Create('text-align: right', 0, 0));
		hcg.Add(THTMLTableColumn.Create('', 0, 0));
		htb.Add(hcg);
		{ Create a new HTML table row }
		htr := THTMLTableRow.Create;
		htr.AlignHoriz := thLeft;
		{ And add headings to it }
		htr.Add(THTMLTableHeading.Create('Name'));
		htr.Add(THTMLTableHeading.Create('Size'));
		htr.Add(THTMLTableHeading.Create(Indent(1) + 'Modified'));
		{ Add the row to the table }
		htb.Add(htr);
		{ Generate the directory tree into the table }
		GenerateHTMLForDir(htb, Directory, 0);
		Result := htb.AsHTML;
	finally
		{ Free all the HTML objects }
		htb.Free;
	end;
end;

{ Generate a directory structure for a specified directory }
procedure THTMLDirectory.GenerateHTMLForDir(htb: THTMLTable; sDirectory: string; iLevel: Word);
const
	{ To sort directories (d) before files (f) }
	sSortType: array [Boolean] of string = ('f', 'd');
var
	slsFiles:  TStringList; { To sort the files }
	src:       TSearchRec; { File details found in search }
	i, iFound: Integer; { Working variables }
	htr:       THTMLTableRow; { The current HTML table row }

	{ Is the current file entry a directory ? }
	function IsDirectory(iAttr: Integer): Boolean;
	begin
		Result := ((faDirectory and iAttr) <> 0);
	end;

begin
	slsFiles := TStringList.Create;
	try
		slsFiles.Sorted := True;
		slsFiles.Duplicates := dupIgnore;
		if IncludeSubDirectories then begin
			{ Find subdirectories in specified directory }
			iFound := FindFirst(sDirectory + '\*.*', faDirectory, src);
			while iFound = 0 do
				with src do begin
					{ If not self or parent directory reference then add to list }
					if IsDirectory(Attr) and not((name = '.') or (name = '..')) then
						slsFiles.AddObject(sSortType[True] + name, TFileInfo.Create(name, Size, Src.TimeStamp, Attr));
					{ Any more ? }
					iFound := FindNext(src);
				end;
			FindClose(src);
		end;
		{ Find matching files in specified directory }
		iFound := FindFirst(sDirectory + '\' + Mask, faAnyFile, src);
		while iFound = 0 do
			with src do begin
				{ If not self or parent directory reference then add to list }
				if not((name = '.') or (name = '..')) then
					slsFiles.AddObject(sSortType[IsDirectory(Attr)] + name, TFileInfo.Create(name, Size, TimeStamp, Attr));
				{ Any more ? }
				iFound := FindNext(src);
			end;
		FindClose(src);
		{ Now process in sorted order }
		for i := 0 to slsFiles.Count - 1 do
			with TFileInfo(slsFiles.Objects[i]) do begin
				{ Create a new HTML table row }
				htr := THTMLTableRow.Create;
				{ And add cells to it }
				htr.Add(THTMLTableDetail.Create(Indent(iLevel) + name));
				if IsDirectory(Attr) then
					htr.Add(THTMLTableDetail.Create('Dir'))
				else
					htr.Add(THTMLTableDetail.Create(IntToStr(Size)));
				htr.Add(THTMLTableDetail.Create(Indent(1) + DateTimeToStr(Time)));
				{ Add the row to the table }
				htb.Add(htr);
				{ Recurse into subdirectories if appropriate }
				if IsDirectory(Attr) and IncludeSubDirectories then
					GenerateHTMLForDir(htb, sDirectory + '\' + name, iLevel + 1);
			end;
	finally
		{ Tidy up }
		for i := 0 to slsFiles.Count - 1 do begin
			slsFiles.Objects[i].Free;
		end;
		slsFiles.Free;
	end;
end;

end.
