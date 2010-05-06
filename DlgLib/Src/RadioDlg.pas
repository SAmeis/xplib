{$IFDEF RadioDlg}
    {$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I DlgLib.inc}

unit RadioDlg;

interface

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	StdCtrls, Buttons, ExtCtrls;

type
	TRadioDlgFrm = Class (TForm)
		RadioGroup : TRadioGroup;
		HlpBtn : TBitBtn;
		CancelBtn : TBitBtn;
    PanelBottom: TPanel;
    OkBtn: TBitBtn;
		procedure FormActivate(Sender : TObject);
    procedure FormShow(Sender: TObject);
	private
		{ Private declarations }
	public
		{ Public declarations }
	end;

type
	TRadioDlg = Class (TComponent)
	private
		FHelpContext : THelpContext;
		RadioFrm : TRadioDlgFrm;
		FItems : TStrings;
		FRemoveButtons : TMsgDlgButtons;
		FItemIndex : integer;
		FCaption : string;
		FTitle : string;
		procedure SetItems(const Value : TStrings);
	public
		constructor Create(AOwner : TComponent); override;
		destructor Destroy; override;
		function Execute : boolean;
	published
		property Title : string read FTitle write FTitle;
		property Caption : string read FCaption write FCaption;
		property Items : TStrings read FItems write SetItems;
		property ItemIndex : integer read FItemIndex write FItemIndex stored TRUE;
		property HelpContext : THelpContext read FHelpContext write FHelpContext;
		property RemoveButtons : TMsgDlgButtons read FRemoveButtons write FRemoveButtons;
	end;

var
	RadioDlgFrm : TRadioDlgFrm;

implementation

{$R *.DFM}

constructor TRadioDlg.Create(AOwner : TComponent);
	{------------------------------------------------------------------------------------------------------------}
begin
	inherited Create(AOwner);
	Self.FRemoveButtons := [];
	Self.FItemIndex := -1;
	FItems := TStringList.Create;
	RadioFrm := NIL;
end;

destructor TRadioDlg.Destroy;
	{------------------------------------------------------------------------------------------------------------}
begin
	if Assigned(RadioFrm) then begin
		RadioFrm.Free;
	end;
	FItems.Free;
	inherited Destroy;
end;

function TRadioDlg.Execute : boolean;
	{------------------------------------------------------------------------------------------------------------}
begin
	if not Assigned(RadioFrm) then begin
		Self.RadioFrm := TRadioDlgFrm.Create(Self);
		RadioFrm.RadioGroup.Items.Assign(Self.FItems);
	end;
	RadioFrm.RadioGroup.Caption := Self.Caption;
	RadioFrm.Caption := Self.Title;
	RadioFrm.RadioGroup.ItemIndex := FItemIndex;
	try
		Result := (RadioFrm.ShowModal = mrOK);
		FItemIndex := RadioFrm.RadioGroup.ItemIndex;
	finally
		RadioFrm.Free;
		RadioFrm := NIL;
	end;
end;

procedure TRadioDlg.SetItems(const Value : TStrings);
{------------------------------------------------------------------------------------------------------------}
begin
	FItems.Assign(Value);
	if Assigned(RadioFrm) then begin
		RadioFrm.RadioGroup.Items.Assign(Value);
	end;
end;

procedure TRadioDlgFrm.FormActivate(Sender : TObject);
begin
	Self.ActiveControl := RadioGroup;
end;

procedure TRadioDlgFrm.FormShow(Sender: TObject);
begin
    Self.RadioGroup.Columns:=1 + ( Self.RadioGroup.Items.Count div 8  );
end;

end.


