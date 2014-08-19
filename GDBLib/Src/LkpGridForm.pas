{$IFDEF LkpGridForm}
	{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I GDBLib.inc}

unit LkpGridForm;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls, Buttons, ExtCtrls, Db, Grids, DBGrids, Mask, DBCtrls;


type
	TLookupGridForm = Class (TForm)
		Label1 : TLabel;
		DBGrid : TDBGrid;
		LkpEdit : TEdit;
		Panel1 : TPanel;
		OKBtn :  TButton;
		CancelBtn : TButton;
		AutoSeekCheckBox : TCheckBox;
		SeekButton : TButton;
		GridDataSource : TDataSource;
		IgnoreCaseCheckBox : TCheckBox;
		procedure LkpEditChangeByUser(Sender : TObject);
		procedure GridDataSourceDataChangeUpdate(Sender : TObject; Field : TField);
		procedure SeekButtonClick(Sender : TObject);
		procedure AutoSeekCheckBoxClick(Sender : TObject);
		procedure DBGridDblClick(Sender : TObject);
	private
		FFillingOK : boolean;
		procedure SetFillingOK(const Value : boolean);
	public
		property FillingOK : boolean read FFillingOK write SetFillingOK;
	end;

	TLkpGrid = Class (TComponent)
	private
		FDlg : TLookupGridForm;
		FDataSet : TDataSet;
		FCacheDialog : boolean;
		FLookupFields : TStrings;
		FIncrementalAutoSeek : boolean;
		FEditCharCase : TEditCharCase;
		FIgnoreCase : Boolean;
		FBackOnCancel : boolean;
		procedure AdjustVisibility(Value : TStrings);
		procedure ReleaseDialog();
		procedure SetDataSet(const Value : TDataSet);
		procedure SetLookupFields(const Value : TStrings);
	public
		constructor Create(AOwner : TComponent); override;
		destructor Destroy; override;
		function Execute() : boolean;
	published
		{ TODO -oRoger -cLIB : Acrescentar as propriedades MustExist e CanBeNull }
		property IncrementalAutoSeek : boolean read FIncrementalAutoSeek write FIncrementalAutoSeek;
		property BackOnCancel : boolean read FBackOnCancel write FBackOnCancel default TRUE;
		property CacheDialog : boolean read FCacheDialog write FCacheDialog;
		property DataSet : TDataSet read FDataSet write SetDataSet;
		property EditCharCase : TEditCharCase read FEditCharCase write FEditCharCase;
		property IgnoreCase : Boolean read FIgnoreCase write FIgnoreCase;
		property LookupFields : TStrings read FLookupFields write SetLookupFields;
	end;

var
	LookupGridForm : TLookupGridForm;

implementation

{$R *.DFM}

uses
	GDBHnd;


procedure TLkpGrid.AdjustVisibility(Value : TStrings);
//----------------------------------------------------------------------------------------------------------------------
var
	j, i : integer;
	FName, ColCaption : string;
	Found : boolean;
begin
	{ TODO -oRoger -cDSG : Corrigir bug de posicionamento, usar lista dos lookupfields como base de tudo }
	if (Self.LookupFields.Text <> '*') or (Self.LookupFields.Text = EmptyStr) then begin  //Filtro para campos -> Valores originais sem tratamento
		//limpa campos indesejaveis
		Self.FDlg.DBGrid.Columns.BeginUpdate();
		try
			for j := 0 to Self.LookupFields.Count - 1 do begin
				FName := Self.LookupFields.Names[j];
				Found := FALSE;
				for i := j to Self.FDlg.DBGrid.Columns.Count - 1 do begin
					if CompareText(Self.FDlg.DBGrid.Columns[i].Field.FieldName, FName) = 0 then begin //Nome do campo match
						Found := TRUE;
						ColCaption := Self.LookupFields.Values[Self.FDlg.DBGrid.Columns[i].Field.FieldName];
						if ColCaption <> EmptyStr then begin
							Self.FDlg.DBGrid.Columns[i].Visible := TRUE;
							Self.FDlg.DBGrid.Columns[i].Title.Caption := ColCaption;
						end else begin
							Self.FDlg.DBGrid.Columns[i].Visible := FALSE;
						end;
						Self.FDlg.DBGrid.Columns[i].Title.Alignment := taCenter;
						//Reposiciona coluna no grid
						Self.FDlg.DBGrid.Columns[i].Index := j;
						Break;
					end;
				end;
				if not Found then begin
					raise Exception.CreateFmt('Campo "%s" não encontrado na tabela presente', [FName]);
				end;
			end;
			//Oculta colunas restantes
			for i := Self.LookupFields.Count to Self.FDlg.DBGrid.Columns.Count - 1 do begin
				Self.FDlg.DBGrid.Columns[i].Visible := FALSE;
			end;
		finally
			Self.FDlg.DBGrid.Columns.EndUpdate();
		end;
	end;
end;

constructor TLkpGrid.Create(AOwner : TComponent);
	//----------------------------------------------------------------------------------------------------------------------
begin
	inherited;
	FBackOnCancel := TRUE;
	FIncrementalAutoSeek := TRUE;
	FEditCharCase := ecNormal;
	FIgnoreCase := TRUE;
	Self.FLookupFields := TStringList.Create();
	Self.FDlg := NIL;
end;

destructor TLkpGrid.Destroy;
	//----------------------------------------------------------------------------------------------------------------------
begin
	Self.FLookupFields.Free();
	if Assigned(Self.FDlg) then begin
		Self.ReleaseDialog();
	end;
	inherited;
end;

function TLkpGrid.Execute() : boolean;
	//----------------------------------------------------------------------------------------------------------------------
var
	OldState, Revert : boolean;
	BM : TBookmark;
begin
	OldState := Self.DataSet.Active;
	Self.DataSet.Active := TRUE;
	try
		if not Assigned(Self.FDlg) then begin
			Application.CreateForm(TLookupGridForm, Self.FDlg);
		end;
		try
			Self.FDlg.FillingOK := TRUE;
			Self.FDlg.LkpEdit.CharCase := Self.FEditCharCase;
			Self.FDlg.AutoSeekCheckBox.Checked := Self.IncrementalAutoSeek;
			Self.FDlg.SeekButton.Enabled := not Self.IncrementalAutoSeek;
			Self.FDlg.IgnoreCaseCheckBox.Checked := Self.IgnoreCase;
			Self.FDlg.GridDataSource.DataSet := Self.FDataSet;  //Atribuicao do DataSet para busca
			Self.AdjustVisibility(Self.LookupFields);		   //Atribuicao dos campos a serem exibidos
			if Self.BackOnCancel then begin
				BM := Self.DataSet.GetBookmark();
			end else begin
				BM := NIL;
			end;
			Revert := TRUE;
			try
				Result := (Self.FDlg.ShowModal() = mrOK);
				Revert := (not Result) and (Assigned(BM));
			finally
				if Revert then begin
					if Self.DataSet.BookmarkValid(BM) then begin
						Self.DataSet.GotoBookmark(BM);
					end;
					Self.DataSet.FreeBookmark(BM);
				end;
			end;
		finally
			if not Self.CacheDialog then begin
				Self.ReleaseDialog();
			end;
		end;
	finally
		Self.DataSet.Active := OldState;
	end;
end;

procedure TLkpGrid.ReleaseDialog();
//----------------------------------------------------------------------------------------------------------------------
begin
	Self.FDlg.GridDataSource.DataSet := NIL;
	Self.FDlg.Free();
	Self.FDlg := NIL;
end;

procedure TLkpGrid.SetDataSet(const Value : TDataSet);
{{
Altera propriedade do datasource

Revision: 26/7/2005
}
begin
	if Assigned(Self.FDlg) then begin
		Self.FDlg.GridDataSource.DataSet := Value;
		FDataSet := Value;
		Self.AdjustVisibility(Self.FLookupFields);
	end else begin
		FDataSet := Value;
	end;
end;

procedure TLookupGridForm.LkpEditChangeByUser(Sender : TObject);
{{
Se ativada busca sequencial realizar seek

Revision: 26/7/2005
}
var
	FName, Value : string;
begin
	try
		if Self.AutoSeekCheckBox.Checked or (Sender = NIL) then begin //Sender = nil -> Chamada externa
			Value := Self.LkpEdit.Text;
			FName := GridDataSource.DataSet.Fields[0].FieldName;
			if Self.IgnoreCaseCheckBox.Checked then begin
				DataSetLocate(GridDataSource.DataSet, FName, [Value], [loCaseInsensitive, loPartialKey]);
			end else begin
				DataSetLocate(GridDataSource.DataSet, FName, [Value], [loPartialKey]);
			end;
		end;
	finally
		if (Self.LkpEdit.Text = EmptyStr) or
			(Pos(UpperCase(Self.LkpEdit.Text), UpperCase(Self.DBGrid.Columns[0].Field.AsString)) = 1) then	   begin
			Self.FillingOK := TRUE;
		end else begin
			Self.FillingOK := FALSE;
		end;
	end;
end;

procedure TLkpGrid.SetLookupFields(const Value : TStrings);
//----------------------------------------------------------------------------------------------------------------------
begin
	Self.FLookupFields.Assign(Value);
	if Assigned(Self.FDlg) then begin
		Self.AdjustVisibility(Value);
	end;
end;

procedure TLookupGridForm.GridDataSourceDataChangeUpdate(Sender : TObject; Field : TField);
//----------------------------------------------------------------------------------------------------------------------
begin
	if (Field = NIL) and (not Self.LkpEdit.Focused) then begin
		Self.LkpEdit.Text := Self.GridDataSource.DataSet.Fields[0].AsString;
	end;
end;

procedure TLookupGridForm.SeekButtonClick(Sender : TObject);
//----------------------------------------------------------------------------------------------------------------------
begin
	Self.LkpEditChangeByUser(NIL); //Forca novo seek
end;

procedure TLookupGridForm.AutoSeekCheckBoxClick(Sender : TObject);
//----------------------------------------------------------------------------------------------------------------------
begin
	Self.SeekButton.Enabled := not Self.AutoSeekCheckBox.Checked;
end;

procedure TLookupGridForm.DBGridDblClick(Sender : TObject);
//----------------------------------------------------------------------------------------------------------------------
begin
	Self.Close();
	Self.ModalResult := mrOK;
end;

procedure TLookupGridForm.SetFillingOK(const Value : boolean);
//----------------------------------------------------------------------------------------------------------------------
begin
	FFillingOK := Value;
	if Self.FFillingOK then begin
		Self.LkpEdit.Color := clWindow;
	end else begin
		Self.LkpEdit.Color := clRed;
	end;
end;

end.


