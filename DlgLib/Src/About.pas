{$IFDEF About}
{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I DlgLib.inc}
unit About;

interface

uses
	WinTypes, WinProcs, Classes, Graphics, Forms, Controls, StdCtrls, Buttons, ExtCtrls;

type
	TAboutBox = class(TForm)
		Panel1: TPanel;
		ProgramIcon: TImage;
		ProductName: TLabel;
		Version: TLabel;
		Copyright: TLabel;
		Comments: TLabel;
		Button1: TButton;
		Label1: TLabel;
		Label2: TLabel;
		WinVersion: TLabel;
		DosVersion: TLabel;
		Label4: TLabel;
		CPU: TLabel;
		Label5: TLabel;
		FreeMemory: TLabel;
		Label6: TLabel;
		FreeResources: TLabel;
		Label8: TLabel;
		UserName: TLabel;
		Label10: TLabel;
		CompanyName: TLabel;
		procedure Button1Click(Sender: TObject);
	private
		{ Private declarations }
	public
		{ Public declarations }
	end;

var
	AboutBox: TAboutBox;

implementation

{$R *.DFM}

procedure TAboutBox.Button1Click(Sender: TObject);
begin
	Self.Close;
end;

end.
