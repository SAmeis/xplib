{$IFDEF MultiUpdateSQL}
    {$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I BDELib.inc}

unit MultiUpdateSQL;

{{
Implements MultiUpdateSQL component and related stuff.

Based in work from Craig Manley.
Email: craig@skybound.nl
Website: http://www.skybound.nl
}


interface

uses
	Windows, Classes, SysUtils, Variants, DB, DBTables, ListHnd;

type
	PStatement = ^TStatement;

	TStatement = record
 {{
 Statement record wrapper
 }
		SQL: string;
	end;

	TExecuteEvent = procedure(Sender : TObject; Index : integer; const Statement : string) of object;
 {{
 Event for each record update at TMultiUpdateSQL.
 
 Sender is a instance of the object responsable for the update
 Index is the index of changed record that need a update
 Statement is the SQL statement to update this record.
 }
	TCheckRowsAffected = (craAlways, craLastStatementOnly, craNever);

	TMultiUpdateSQL = Class (TDataSetUpdateObject)
	private
		FCheckRowsAffected : TCheckRowsAffected;
		FDataSet : TDataSet;
		FOnAfterExecute : TExecuteEvent;
		FOnBeforeExecute : TExecuteEvent;
		FQueries : array[TUpdateKind] of TQuery;
		FSQL : TContainerList;
		FSQLText : array[TUpdateKind] of TStrings;
		FTerminator : string;
		function GetCount(UpdateKind : TUpdateKind) : Cardinal;
		function GetSQLIndex(Index : Integer) : TStrings;
		procedure SetSQLIndex(Index : Integer; Value : TStrings);
		function GetQuery(UpdateKind : TUpdateKind) : TQuery;
		function GetSQL(UpdateKind : TUpdateKind) : TStrings;
		procedure SetSQL(UpdateKind : TUpdateKind; Value : TStrings);
		function GetStatement(UpdateKind : TUpdateKind; Index : Integer) : string;
		procedure SetTerminator(const AValue : string);
	protected
		function GetDataSet : TDataSet; override;
		procedure SetDataSet(ADataSet : TDataSet); override;
		procedure SetParams(UpdateKind : TUpdateKind);
		procedure SQLChanged(Sender : TObject);
	public
		procedure Apply(UpdateKind : TUpdateKind); override;
		property Count[UpdateKind : TUpdateKind] : Cardinal read GetCount;
		constructor Create(AOwner : TComponent); override;
		property DataSet;
		destructor Destroy; override;
		property Query[UpdateKind : TUpdateKind] : TQuery read GetQuery;
		property SQL[UpdateKind : TUpdateKind] : TStrings read GetSQL write SetSQL;
		property Statements[UpdateKind : TUpdateKind; Index : Integer] : string read GetStatement;
	published
		property CheckRowsAffected : TCheckRowsAffected read FCheckRowsAffected write FCheckRowsAffected;
		property DeleteSQL : TStrings index 2 read GetSQLIndex write SetSQLIndex;
		property InsertSQL : TStrings index 1 read GetSQLIndex write SetSQLIndex;
		property ModifySQL : TStrings index 0 read GetSQLIndex write SetSQLIndex;
		property OnAfterExecute : TExecuteEvent read FOnAfterExecute write FOnAfterExecute;
		property OnBeforeExecute : TExecuteEvent read FOnBeforeExecute write FOnBeforeExecute;
		property Terminator : string read FTerminator write SetTerminator;
	end;

procedure Register;

implementation

uses
	BDEConst, Str_Pas;

{ $ R   **** Removido o *.RES correspondente e agregado o recurso com icone para o DCR do pacote }

procedure Register;
begin
	RegisterComponents('BDE', [TMultiUpdateSQL]);
end;

procedure SplitStatements(ListOfStatements : TContainerList; Statements : TStrings; const Terminator : string);
//----------------------------------------------------------------------------------------------------------------------------------
//Segmenta as clausulas SQL em suas componentes
var
	i : integer;
	s : string;
	Statement : string;
	p : PStatement;
begin
	ListOfStatements.Clear;
	Statement := '';
	for i := 0 to Statements.Count - 1 do begin
		s := SysUtils.TrimRight(Statements.Strings[i]);
		if (Length(s) > 0) and (StrLastPos(Terminator, s) = Length(s) - Length(Terminator)) then begin
			Statement := Statement + Copy(s, 1, Length(s) - Length(Terminator));
			new(p);
			p^.SQL := Statement;
			ListOfStatements.Add(p);
			Statement := EmptyStr;
		end else begin
			Statement := Statement + Statements.Strings[i] + #13#10;
			if (i = Statements.Count - 1) then begin
				new(p);
				p^.SQL := Statement;
				ListOfStatements.Add(p);
				Statement := EmptyStr;
			end;
		end;
	end;
end;

{TMultiUpdateSQL}

{-**********************************************************************
************************************************************************
******************
******************  Class:    TMultiUpdateSQL
******************  Category: No category
******************
************************************************************************
************************************************************************}
{--------------------------------------------------------------------------------------------------------------------------------}
function TMultiUpdateSQL.GetCount(UpdateKind : TUpdateKind) : Cardinal;
var
	List : TContainerList;
begin
	List := TContainerList.Create(rtStruct);
	try
		SplitStatements(List, FSQLText[UpdateKind], FTerminator);
		Result := List.Count;
	finally
		List.Free;
	end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TMultiUpdateSQL.GetSQLIndex(Index : Integer) : TStrings;
begin
	Result := FSQLText[TUpdateKind(Index)];
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TMultiUpdateSQL.SetSQLIndex(Index : Integer; Value : TStrings);
begin
	SetSQL(TUpdateKind(Index), Value);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TMultiUpdateSQL.GetQuery(UpdateKind : TUpdateKind) : TQuery;
begin
	if not Assigned(FQueries[UpdateKind]) then begin
		FQueries[UpdateKind] := TQuery.Create(Self);
		//FQueries[UpdateKind].SQL.Assign(FSQLText[UpdateKind]);
		if (FDataSet is TDBDataSet) then begin
			FQueries[UpdateKind].SessionName := TDBDataSet(FDataSet).SessionName;
			FQueries[UpdateKind].DatabaseName := TDBDataSet(FDataSet).DataBaseName;
		end;
	end;
	Result := FQueries[UpdateKind];
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TMultiUpdateSQL.GetSQL(UpdateKind : TUpdateKind) : TStrings;
begin
	Result := FSQLText[UpdateKind];
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TMultiUpdateSQL.SetSQL(UpdateKind : TUpdateKind; Value : TStrings);
begin
	FSQLText[UpdateKind].Assign(Value);
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TMultiUpdateSQL.GetStatement(UpdateKind : TUpdateKind; Index : Integer) : string;
var
	List : TContainerList;
begin
	List := TContainerList.Create(rtStruct);
	try
		SplitStatements(List, FSQLText[UpdateKind], FTerminator);
		if (Index < 0) or (Index > List.Count - 1) then begin
			Result := '';
		end else begin
			Result := TStatement(List.Items[Index]^).SQL;
		end;
	finally
		List.Free;
	end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TMultiUpdateSQL.SetTerminator(const AValue : string);
begin
	if AValue > '' then begin
		FTerminator := AValue;
	end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TMultiUpdateSQL.GetDataSet : TDataSet;
begin
	Result := FDataSet;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TMultiUpdateSQL.SetDataSet(ADataSet : TDataSet);
begin
	FDataSet := ADataSet;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TMultiUpdateSQL.SetParams(UpdateKind : TUpdateKind);
var
	I : Integer;
	Old : Boolean;
	Param : TParam;
	PName : string;
	Field : TField;
	Value : Variant;
	ExecQry : TQuery;
begin
	if not Assigned(FDataSet) then begin
		Exit;
	end;
	ExecQry := Query[UpdateKind];
	for I := 0 to ExecQry.Params.Count - 1 do begin
		Param := ExecQry.Params[I];
		PName := Param.Name;
		Old := CompareText(Copy(PName, 1, 4), 'OLD_') = 0;
		if Old then begin
			System.Delete(PName, 1, 4);
		end;
		Field := FDataSet.FindField(PName);
		if not Assigned(Field) then begin
			Continue;
		end;
		if Old then begin
			Param.AssignFieldValue(Field, Field.OldValue);
		end else begin
			Value := Field.NewValue;
			if VarIsEmpty(Value) then begin
				Value := Field.OldValue;
			end;
			Param.AssignFieldValue(Field, Value);
		end;
	end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TMultiUpdateSQL.SQLChanged(Sender : TObject);
var
	UpdateKind : TUpdateKind;
begin
	for UpdateKind := Low(TUpdateKind) to High(TUpdateKind) do begin
		if Sender = FSQLText[UpdateKind] then begin
			if Assigned(FQueries[UpdateKind]) then begin
				FQueries[UpdateKind].Params.Clear;
				FQueries[UpdateKind].SQL.Assign(FSQLText[UpdateKind]);
			end;
			Break;
		end;
	end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TMultiUpdateSQL.Apply(UpdateKind : TUpdateKind);
var
	i : Integer;
	ExecQry : TQuery;
begin
	ExecQry := Self.Query[UpdateKind];
	SplitStatements(FSQL, FSQLText[UpdateKind], FTerminator);
	for i := 0 to FSQL.Count - 1 do begin
		ExecQry.Params.Clear;
		ExecQry.SQL.Text := TStatement(FSQL.Items[i]^).SQL;
		if Assigned(FOnBeforeExecute) then begin
			FOnBeforeExecute(Self, i, TStatement(FSQL.Items[i]^).SQL);
		end;
		SetParams(UpdateKind);
		ExecQry.Prepare;
		ExecQry.ExecSQL;
		if ((CheckRowsAffected = craAlways) or ((CheckRowsAffected = craLastStatementOnly) and (i = FSQL.Count - 1))) and
			(ExecQry.RowsAffected <> 1) then begin
			DatabaseError(SUpdateFailed);
		end;
		if Assigned(FOnAfterExecute) then begin
			FOnAfterExecute(Self, i, TStatement(FSQL.Items[i]^).SQL);
		end;
	end;
	FSQL.Clear;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
constructor TMultiUpdateSQL.Create(AOwner : TComponent);
var
	UpdateKind : TUpdateKind;

	//----------------------------------------------------------------------------------------------------------------------------------

begin
	inherited Create(AOwner);
	FSQL := TContainerList.Create(rtStruct);
	for UpdateKind := Low(TUpdateKind) to High(TUpdateKind) do begin
		FSQLText[UpdateKind] := TStringList.Create;
		TStringList(FSQLText[UpdateKind]).OnChange := SQLChanged;
	end;
	FCheckRowsAffected := craAlways;
	FTerminator := ';';
end;

{--------------------------------------------------------------------------------------------------------------------------------}
destructor TMultiUpdateSQL.Destroy;
var
	UpdateKind : TUpdateKind;

	//----------------------------------------------------------------------------------------------------------------------------------

begin
	if Assigned(FDataSet) and (FDataSet is TBDEDataset) and (TBDEDataset(FDataSet).UpdateObject = Self) then begin
		TBDEDataset(FDataSet).UpdateObject := NIL;
	end;
	for UpdateKind := Low(TUpdateKind) to High(TUpdateKind) do begin
		FSQLText[UpdateKind].Free;
	end;
	FSQL.Free;
	inherited Destroy;
end;

end.


