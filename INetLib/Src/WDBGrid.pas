{$IFDEF WCtrls}
{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I INetLib.inc}
//Diretivas removidas desta versão legada
{ -$IFDEF WDBGrid }
{ -$A+,B-,C-,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O-,P+,Q-,R+,S-,T-,U-,V+,W-,X+,Y+,Z4 }
{ -$DEBUGINFO ON }
{ -$ELSE }
{ -$A+,B-,C-,D-,E-,F-,G+,H+,I+,J+,K-,L-,M-,N+,O+,P+,Q+,R+,S-,T-,U-,V+,W-,X+,Y-,Z4 }
{ -$F+ }
{ -$ENDIF }

unit WDBGrid;

interface

uses
	Classes, IHTML4, IHTMLDB, Db, Graphics, SysUtils, ClassHnd, Controls, httpapp;

const
	_MAX_ITEMS_WEB_SELECT_ = 100;

	//----------------------------------------------------------------------------------------------------------------------
type
	TWebDataSetUpdate = record
		RecEdited: integer;
		RecDeleted: integer;
		RecInserted: integer;
		RecEditViolation: integer;
		RecInsertViolation: integer;
	end;

	THTMLLookupValues = procedure(Sender: Tobject; Field: TField; ValueList: TStrings; Cell: THTMLTableDetail; UseValues: boolean)
		of object;
	TOnGetIntValue         = procedure(Sender: Tobject; var Value: longint);
	TWebDataSetInfoStorage = class;
	TOnWebGridSeekError    = procedure(WebDataSetInfoStorage: TWebDataSetInfoStorage; Index: integer; Request: TWebRequest;
		Response: TWebResponse; var Proceed: boolean) of object;
	TOnCheckedRecord = function(DataSet: TDataSet; Request: TWebRequest; Response: TWebResponse; Index: integer;
		var Checked: boolean): boolean of object;
	//Referencia posterior
	TDataSetWebGrid      = class;
	TWebUpdateMode       = (wumInsert, wumEdit, wumDelete);
	TWebUpdateModes      = set of TWebUpdateMode;
	TWebUpdateRecordProc = function(Sender: TWebDataSetInfoStorage; RecordIndex: integer; Request: TWebRequest;
		Response: TWebResponse): boolean of object;

	//propriedades individual de cada WebDataSet
	TWebDataSetInfoStorage = class(TComponent)
	private
		FAppendRecordsAvailble: integer;
		FVersion:               string;
		FIndexName:             string;
		FKeyFieldsNames:        TStringList;
		FKeyFieldsValues:       TStringList;
		FInitialKey:            TStringList;
		FFinalKey:              TStringList;
		FRecordsLoaded:         integer;
		FWebGridName:           string;
		FWebDataGrid:           TDataSetWebGrid;
		FRecordOffSet:          longint;
		FOnDeleteError:         TDataSetErrorEvent;
		FOnEditError:           TDataSetErrorEvent;
		FOnPostError:           TDataSetErrorEvent;
		FBeforeInsert:          TDataSetNotifyEvent;
		FOnNewRecord:           TDataSetNotifyEvent;
		FOnWebGridSeekError:    TOnWebGridSeekError;
		FOnCheckedRecord:       TOnCheckedRecord;
		FReadOnly:              boolean;
		FOnBeforeEditRecord:    TWebUpdateRecordProc;
		FOnBeforePostRecord:    TWebUpdateRecordProc;
		FOnBeforeAppendRecord:  TWebUpdateRecordProc;
		FWebUpdateMode:         TWebUpdateModes;
	protected
		function CheckCursorPos(DS: TDataSet; Index: integer): boolean;
		function DoAddNewRecords(DS: TDataSet; Request: TWebRequest; Response: TWebResponse; Log: TWebDataSetUpdate): integer;
		function DoDeleteCheckedRecords(DS: TDataSet; Request: TWebRequest; Response: TWebResponse; Log: TWebDataSetUpdate)
			: integer;
		function DoSeekDataSet(DS: TDataSet; Index: integer): boolean;
		function DoBeforeEditRecord(RecordIndex: integer; Request: TWebRequest; Response: TWebResponse): boolean;
		function DoBeforePostRecord(RecordIndex: integer; Request: TWebRequest; Response: TWebResponse): boolean;
		function DoBeforeAppendRecord(RecordIndex: integer; Request: TWebRequest; Response: TWebResponse): boolean;
		function DoUpdateDataSetValues(DS: TDataSet; Request: TWebRequest; Response: TWebResponse; Log: TWebDataSetUpdate): integer;
		procedure DoUpdateRecord(DS: TDataSet; Index: integer; Request: TWebRequest; Response: TWebResponse);
		procedure UpdateStatus;
		function UpdateTableData(Request: TWebRequest; Response: TWebResponse; DataSetContainer: TComponent): TWebDataSetUpdate;
	public
		//Eventos similares a TDataSet que serao passados ao mesmo
		property BeforeInsert:         TDataSetNotifyEvent read FBeforeInsert write FBeforeInsert;
		property OnBeforeEditRecord:   TWebUpdateRecordProc read FOnBeforeEditRecord write FOnBeforeEditRecord;
		property OnBeforeAppendRecord: TWebUpdateRecordProc read FOnBeforeAppendRecord write FOnBeforeAppendRecord;
		property OnBeforePostRecord:   TWebUpdateRecordProc read FOnBeforePostRecord write FOnBeforePostRecord;
		property OnCheckedRecord:      TOnCheckedRecord read FOnCheckedRecord write FOnCheckedRecord;
		property OnDeleteError:        TDataSetErrorEvent read FOnDeleteError write FOnDeleteError;
		property OnEditError:          TDataSetErrorEvent read FOnEditError write FOnEditError;
		property OnNewRecord:          TDataSetNotifyEvent read FOnNewRecord write FOnNewRecord;
		property OnPostError:          TDataSetErrorEvent read FOnPostError write FOnPostError;
		property OnWebGridSeekError:   TOnWebGridSeekError read FOnWebGridSeekError write FOnWebGridSeekError; //Sempre Onwner
		property WebDataGrid: TDataSetWebGrid read FWebDataGrid write FWebDataGrid; //Deve ser correto antes chamada UpdateStatus
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
		function AsText: string;
		function IsCheckedRecord(WebInputRecordIndex: integer; Request: TWebRequest): boolean;
		{ TODO 5 -oRoger -cdsg : Implementar metodo para montar uma list de Bookmarks ( similar ao dbrid multselected ) para pegar os registros marcados }
	published //Estes campos sao published apenas para efeito de streaming
		property AppendRecordsAvailble: integer read FAppendRecordsAvailble write FAppendRecordsAvailble;
		//espaco para novos registros
		property IndexName:       string read FIndexName write FIndexName; //Indice a ser usado, pode ser um SQL no futuro
		property InitialKey:      TStringList read FInitialKey write FInitialKey; //Valor para os campos chaves do registro inicial
		property FinalKey:        TStringList read FFinalKey write FFinalKey; //Valor dos campos chaves do ultimo registro
		property KeyFieldsValues: TStringList read FKeyFieldsValues write FKeyFieldsValues; //Valores dos campos chaves
		property KeyFieldsNames:  TStringList read FKeyFieldsNames write FKeyFieldsNames; //Nomes dos campos chaves
		property readonly:        boolean read FReadOnly write FReadOnly;
		//Evita alteracoes nos dados diretamente, apenas pelas enumeracoes
		property RecordOffSet: longint read FRecordOffSet write FRecordOffSet;
		//Property usada apenas qdo nao ha KeyFields eh basada em RecNo
		property RecordsLoaded:  integer read FRecordsLoaded write FRecordsLoaded; //Registros expostos para edicao WEB
		property Version:        string read FVersion write FVersion; //Verso do engenho de geracao
		property WebGridName:    string read FWebGridName write FWebGridName; //nome da tabela destes dados
		property WebUpdateModes: TWebUpdateModes read FWebUpdateMode write FWebUpdateMode;
		//Indica operacoes permitidas durante a atualizacao dos dados
	end;

	//Uma instancia externa sera gerada para enumerar as propriedades das tabelas existentes;
	TWebDataSource = class(THTMLHiddenField)
	private
		List:     TList;
		FWebName: string;
		function GetValue: string;
	protected
		//Pode-se fazer um descendente para persistir um component
	public
		property Value: string read GetValue;
		constructor Create(sName, sValue: string);
		destructor Destroy; override;
		procedure AddWebDataSet(Instance: TDataSetWebGrid);
		function AsHTML: string; override; stdcall;
	end;

	TOnNextWebDataSetInfoStorage = procedure(WebDataSetInfoStorage: TWebDataSetInfoStorage; Index: integer);

	TWebDataSourceInput = class(TComponent)
	private
		FOnDeleteError:               TDataSetErrorEvent;
		FOnEditError:                 TDataSetErrorEvent;
		FOnPostError:                 TDataSetErrorEvent;
		FBeforeInsert:                TDataSetNotifyEvent;
		FOnNewRecord:                 TDataSetNotifyEvent;
		FOnWebGridSeekError:          TOnWebGridSeekError;
		FOnCheckedRecord:             TOnCheckedRecord;
		FOnNextWebDataSetInfoStorage: TOnNextWebDataSetInfoStorage;
		FReadOnly:                    boolean;
		FOnBeforeEditRecord:          TWebUpdateRecordProc;
		FOnBeforeAppendRecord:        TWebUpdateRecordProc;
		FOnBeforePostRecord:          TWebUpdateRecordProc;
		FOnBeforeDeleteRecord:        TWebUpdateRecordProc;
		FWebUpdateMode:               TWebUpdateModes;
		function GetAsText: string;
		procedure SetAsText(const Value: string); //Esta sera a classe para ser usada na recuperacao dos dados pela web
	protected
		procedure DoNextWebDataSetInfoStorage(WDataGridInfo: TWebDataSetInfoStorage; Index: integer); virtual;
	public
		property AsText:                      string read GetAsText write SetAsText;
		property BeforeInsert:                TDataSetNotifyEvent read FBeforeInsert write FBeforeInsert;
		property OnBeforeEditRecord:          TWebUpdateRecordProc read FOnBeforeEditRecord write FOnBeforeEditRecord;
		property OnBeforeAppendRecord:        TWebUpdateRecordProc read FOnBeforeAppendRecord write FOnBeforeAppendRecord;
		property OnBeforePostRecord:          TWebUpdateRecordProc read FOnBeforePostRecord write FOnBeforePostRecord;
		property OnBeforeDeleteRecord:        TWebUpdateRecordProc read FOnBeforeDeleteRecord write FOnBeforeDeleteRecord;
		property OnCheckedRecord:             TOnCheckedRecord read FOnCheckedRecord write FOnCheckedRecord;
		property OnDeleteError:               TDataSetErrorEvent read FOnDeleteError write FOnDeleteError;
		property OnEditError:                 TDataSetErrorEvent read FOnEditError write FOnEditError;
		property OnPostError:                 TDataSetErrorEvent read FOnPostError write FOnPostError;
		property OnNewRecord:                 TDataSetNotifyEvent read FOnNewRecord write FOnNewRecord;
		property OnNextWebDataSetInfoStorage: TOnNextWebDataSetInfoStorage read FOnNextWebDataSetInfoStorage
			write FOnNextWebDataSetInfoStorage;
		property OnWebGridSeekError: TOnWebGridSeekError read FOnWebGridSeekError write FOnWebGridSeekError; //Override Childs
		property WebUpdateModes:     TWebUpdateModes read FWebUpdateMode write FWebUpdateMode;
		//Indica operacoes permitidas durante a atualizacao dos dados
		property readonly: boolean read FReadOnly write FReadOnly;
		//Evita a chamada aos metodos que alteram valores, mas chama enumeradores
		procedure SetChildEvents(Child: TWebDataSetInfoStorage);
		procedure UpdateDataSets(Request: TWebRequest; Response: TWebResponse; DataSetContainer: TComponent);
	end;

	//----------------------------------------------------------------------------------------------------------------------
	//Classe base
	//Eventos de geracao
	THTMLCheckRowPosition = (hcrPosNone, hcrPosFirst, hcrPosLast);
	THTMLHeaderEvent      = procedure(Sender: Tobject; Field: TField; HeaderCell: THTMLTableHeading) of object;
	THTMLCheckBoxEvent    = procedure(Sender: Tobject; CheckBox: THTMLCheckBoxField; htdCell: THTMLTableDetail) of object;

	//Classe basica de publicacao dos DataSet's
	TCustomDataSetWebOut = class(THTMLDataTable)
	private
		FMaxRecordsOut:            integer;
		FRecordsLoaded:            integer;
		FOnDetailHeaderProperties: THTMLHeaderEvent;
		FOnFieldCellURLLink:       THTMLCellEvent;
		FOnFieldCellContent:       THTMLCellEvent;
		FRecordOffSet:             longint; { TODO 5 -oRoger : Verificar uso desta propriedade }
		FOnGetRecordOffSet:        TOnGetIntValue;
	protected
		//eventos
		property OnDetailHeaderProperties: THTMLHeaderEvent read FOnDetailHeaderProperties write FOnDetailHeaderProperties;
		property OnFieldCellContent:       THTMLCellEvent read FOnFieldCellContent write FOnFieldCellContent;
		property OnFieldCellURLLink:       THTMLCellEvent read FOnFieldCellURLLink write FOnFieldCellURLLink;
		procedure LoadRecordField(htd: THTMLTableDetail; Field: TField); virtual;
		procedure LoadHeaders(htr: THTMLTableRow); virtual;
		procedure LoadRecord(htr: THTMLTableRow); virtual;
		procedure CheckEmptyTable(DS: TDataSet); virtual;
	public
		procedure Clear; override; //Limpa elementos internos
		function LoadFromDataSet: integer; override; //Carrega os elementos a partir do DataSet e parametros
		property OnGetRecordOffSet: TOnGetIntValue read FOnGetRecordOffSet write FOnGetRecordOffSet;
		property RecordOffSet: longint read FRecordOffSet write FRecordOffSet;
		property RecordsLoaded: integer read FRecordsLoaded; //Quantos registros forma tratados ate o momento
		property MaxRecordsOut: integer read FMaxRecordsOut write FMaxRecordsOut default 100; //limite para a saida
	end;

	//----------------------------------------------------------------------------------------------------------------------
	//Classe exportada
	TDataSetWebOut = class(TCustomDataSetWebOut)
	published
		property DataSet; //DataSet a ser usado como fonte dos dados
		property HeaderBackground; //Usar Header para a tabela HTML
		property HeaderBorderColour; //Cor de borda Header
		property HeaderBorderHighlight; //Cor bordar de foco do Header
		property HeaderBorderShadow; //Cor de borda Header inferior
		//Eventos
		property OnHeaderRowProperties; //Linha de Header sendo gerada
		property OnDetailHeaderProperties; //Celula de Header sendo gerada
		property OnFieldCellContent; //Celula com o dado do DataSet sendo publicado
		property OnFieldCellURLLink; //Conteudo da celula irah ter um link
	end;

	//----------------------------------------------------------------------------------------------------------------------
	TDataSetWebGrid = class(TDataSetWebOut)
	private
		FCheckRowPosition:            THTMLCheckRowPosition;
		FOnGetColumnCheckBox:         THTMLCheckBoxEvent;
		FWebTableName:                string;
		FReadOnly:                    boolean;
		FWebInfo:                     TWebDataSetInfoStorage;
		FWebDataSource:               TWebDataSource;
		FWebAppendedRecordsAvailable: integer;
		FIncludeKeyFieldsList:        boolean;
		FKeyFieldsNames:              TStringList;
		FKeyFieldsValues:             TStringList;
		FFinalKey:                    TStringList;
		FInitialKey:                  TStringList;
		FOnGetLookupValues:           THTMLLookupValues;
		FAppendRecordsBackground:     TColor;
		FState:                       TDataSetState;
	protected
		procedure LoadHeaders(htr: THTMLTableRow); override; //Carga dos Headers
		procedure LoadRecordField(htd: THTMLTableDetail; Field: TField); override; //Carga do campo do registro
		procedure LoadRecord(htr: THTMLTableRow); override; //Carga do registro
		procedure AddAppendedRecords;
		function EditBooleanValue(Field: TField; htd: THTMLTableDetail): boolean;
		function EditLookupValue(Field: TField; htd: THTMLTableDetail): boolean;
		function EditIntegerValue(Field: TField; htd: THTMLTableDetail): boolean;
		function EditStringValue(Field: TField; htd: THTMLTableDetail): boolean;
		procedure CheckEmptyTable(DS: TDataSet); override;
	public
		property AppendRecordsBackground: TColor read FAppendRecordsBackground write FAppendRecordsBackground;
		property CheckRowPosition:        THTMLCheckRowPosition read FCheckRowPosition write FCheckRowPosition default hcrPosNone;
		//Coluna para assinalar pertinencia a um grupo de selecao
		property FinalKey:             TStringList read FFinalKey;
		property IncludeKeyFieldsList: boolean read FIncludeKeyFieldsList write FIncludeKeyFieldsList;
		property InitialKey:           TStringList read FInitialKey;
		property KeyFieldsNames:       TStringList read FKeyFieldsNames;
		property KeyFieldsValues:      TStringList read FKeyFieldsValues;
		property OnGetColumnCheckBox:  THTMLCheckBoxEvent read FOnGetColumnCheckBox write FOnGetColumnCheckBox;
		//Qdo coluna inclusao gerada
		property OnGetLookupValues:           THTMLLookupValues read FOnGetLookupValues write FOnGetLookupValues;
		property readonly:                    boolean read FReadOnly write FReadOnly; //Apenas saida dos dados
		property State:                       TDataSetState read FState;
		property WebAppendedRecordsAvailable: integer read FWebAppendedRecordsAvailable write FWebAppendedRecordsAvailable;
		property WebInfo: TWebDataSetInfoStorage read FWebInfo write FWebInfo; //Informacoes geradas qdo LoadFromDataSet e chamado
		property WebDataSource:  TWebDataSource read FWebDataSource;
		property WebDataSetName: string read FWebTableName write FWebTableName; //Nome da tabela em questao
		constructor Create(dtsDataSet: TDataSet; ParentWebDataSource: TWebDataSource); reintroduce;
		destructor Destroy; override;
		function LoadFromDataSet: integer; override;
		function WebFieldName(Field: TField): string;
	end;

function DBFieldLookupList(Field: TField; List: TStrings; MaxItems: integer): integer;
function LoadCFieldsFromFile(const Text: string; CFields: TStrings): boolean;
function LoadWebDataSourceInput(Str: string): TWebDataSourceInput;

implementation

uses
	dbTables, Str_Pas, IOObj, Variants, XP.StrConverters;

const
	thAlignments: array [TAlignment] of THTMLTableAlignHoriz = (thLeft, thRight, thCentre);
	thcAlignments: array [TAlignment] of THTMLAlignmentHoriz = (ahLeft, ahRight, ahCentre);

function DBFieldLookupList(Field: TField; List: TStrings; MaxItems: integer): integer;
//----------------------------------------------------------------------------------------------------------------------
var
	Mark: TBookmark;
begin
	//check the field first
	if Field.FieldKind <> fkLookup then begin
		Result :=-1;
		List.Clear;
		List.Add('Campo fornecido não lookup: ' + Field.FieldName);
		Exit;
	end;
	if not Assigned(Field.LookupDataSet) or (Field.LookupKeyFields = '') or (Field.LookupResultField = '') then begin
		Result :=-1;
		List.Clear;
		List.Add('Dados do lookup inconsistentes para: ' + Field.FieldName);
		Exit;
	end;
	Result := 0;
	Mark := Field.LookupDataSet.GetBookmark;
	try
		Field.LookupDataSet.DisableControls;
		try
			Field.LookupDataSet.First;
			try
				List.BeginUpdate;
				try
					List.Clear;
					while not Field.LookupDataSet.Eof do begin
						Inc(Result);
						if Result > MaxItems then begin
							Break;
						end;
						List.Add(Field.LookupDataSet.FieldByName(Field.LookupResultField).AsString);
						Field.LookupDataSet.Next;
					end;
				finally
					List.EndUpdate;
				end;
			finally
				Field.LookupDataSet.GotoBookmark(Mark);
			end;
		finally
			Field.LookupDataSet.EnableControls;
		end;
	finally
		Field.LookupDataSet.FreeBookmark(Mark);
	end;
end;

function LoadCFieldsFromFile(const Text: string; CFields: TStrings): boolean;
//----------------------------------------------------------------------------------------------------------------------
//Rotina de careter apenas de testes. Tem como funcao ler os valores para os WebFields de um texto
var
	Lines: TStringList;
	L:     string;
begin
	CFields.Clear;
	Lines := TStringList.Create;
	Lines.Text := Text;
	try
		L := EmptyStr;
		//Ler conteudo do WDataSource
		while (Lines.Strings[0] <> '#MARCADOR#') and (Lines.Count >= 1) do begin
			L := L + Lines.Strings[0] + #13#10;
			if Lines.Strings[0] = '#MARCADOR#' then begin
				System.Continue;
			end;
			Lines.Delete(0);
		end;
		Lines.Delete(0);
		CFields.Add(L);
		//Ler outros campos
		CFields.AddStrings(Lines);
	finally
		Lines.Free;
	end;
	Result := True;
end;

function LoadWebDataSourceInput(Str: string): TWebDataSourceInput;
//----------------------------------------------------------------------------------------------------------------------
begin
	Result := TWebDataSourceInput.Create(nil);
	try
		IOObj.ImportObjText(Result, Str);
	except
		Result.Free;
		Result := nil;
	end;
end;

{ TCustomWebDBGrid }

procedure TCustomDataSetWebOut.CheckEmptyTable(DS: TDataSet);
//----------------------------------------------------------------------------------------------------------------------
//Checa se a saida desta tabela pode ser vazia, de forma diferente para cada tipo de classe
begin
	if DS.RecordCount = 0 then begin
		raise EHTMLError.Create(sNoRecordsFound);
	end;
end;

procedure TCustomDataSetWebOut.Clear;
//----------------------------------------------------------------------------------------------------------------------
begin
	inherited;
	Self.FRecordsLoaded :=-1;
end;

function TCustomDataSetWebOut.LoadFromDataSet: integer;
//----------------------------------------------------------------------------------------------------------------------
var
	i:                            integer;
	bVisible, OldControlsDisable: boolean;
	htr:                          THTMLTableRow;
	RecordLimit:                  longint;
begin
	Self.Clear; //Limpa conteudo danterior da tabela, Indica que o cabecalho ainda esta para ser processado
	if Self.DataSet = nil then begin
		raise EHTMLError.Create(sNoDataSet);
	end;
	with Self.DataSet do begin
		//Verifica campos presentes e visiveis
		bVisible := False;
		for i := 0 to FieldCount - 1 do begin
			if Fields[i].Visible then begin
				bVisible := True;
				Break;
			end;
		end;
		if not bVisible then begin
			raise EHTMLError.Create(sNoVisibleFields);
		end;

		Self.CheckEmptyTable(Self.DataSet);

		//Escreve cabecalhos
		if Self.Headers then begin
			htr := THTMLTableRow.Create;
			Self.LoadHeaders(htr);
			Self.Add(htr);
		end;
		Self.FRecordsLoaded := 0; //Cabecalho foi processado
		OldControlsDisable := ControlsDisabled;
		DisableControls; //Desabilita controles do DataSet
		try
			{ TODO 4 -oRoger : Remover referencia a RecNO desta porcaria, podemos avaliar a possibilidade de fazer RecordOffSet ser
			  lido/setado transparentemente por Self.DataSet.RecNo ( Os valores sao sempre IGUAIS }
			Self.FRecordOffSet := Self.DataSet.RecNo; //Avaliado no inicio da carga dos valores do primeiro registro
			if Assigned(FOnGetRecordOffSet) then begin
				FOnGetRecordOffSet(Self, FRecordOffSet);
			end;
			//Varre registros para escrever as linhas ate fim de arquivo ou limite de registros
			if Self.MaxRecordsOut = 0 then begin
				RecordLimit := high(longint); //Limite de registros inatingivel
			end else begin
				RecordLimit := Self.MaxRecordsOut;
			end;
			while (RecordLimit > Self.FRecordsLoaded) and (not Eof) do begin //Processa registros
				Inc(Self.FRecordsLoaded); //Contador de registros procesados
				htr := THTMLTableRow.Create;
				Self.LoadRecord(htr); //Inserir informacoes sobre a chave de busca em WebObject
				Self.Add(htr);
				Next;
			end;

			Self.FDataLoaded := True; // ***Set non Empty Table flag

		finally
			if not OldControlsDisable then begin
				EnableControls;
			end;
		end;
	end;
	Result := Self.FRecordsLoaded;
end;

procedure TCustomDataSetWebOut.LoadHeaders(htr: THTMLTableRow);
//----------------------------------------------------------------------------------------------------------------------
var
	hth: THTMLTableHeading;
	i:   integer;
begin
	htr.AlignHoriz := Self.AlignHoriz;
	htr.AlignVert := Self.AlignVert;
	htr.Background := Self.HeaderBackground;
	htr.BorderColour := Self.HeaderBorderColour;
	htr.BorderHighlight := Self.HeaderBorderHighlight;
	htr.BorderShadow := Self.HeaderBorderShadow;
	if Assigned(Self.OnHeaderRowProperties) then begin //Dispara evento de formatacao de linha de cabecalhos
		Self.OnHeaderRowProperties(Self, htr);
	end;
	//Coloca o cabecalho para cada campo
	for i := 0 to Self.DataSet.FieldCount - 1 do begin
		if Self.DataSet.Fields[i].Visible then begin
			hth := THTMLTableHeading.Create(EscapeText(Self.DataSet.Fields[i].DisplayName));
			hth.AlignHoriz := thAlignments[Self.DataSet.Fields[i].Alignment];
			if Assigned(Self.FOnDetailHeaderProperties) then begin //Dispara formatacao celulas do cabecalho
				FOnDetailHeaderProperties(Self, Self.DataSet.Fields[i], hth);
			end;
			htr.Add(hth);
		end;
	end;
end;

procedure TCustomDataSetWebOut.LoadRecord(htr: THTMLTableRow);
//----------------------------------------------------------------------------------------------------------------------
var
	i:   integer;
	htd: THTMLTableDetail;
begin
	if Assigned(Self.OnDetailRowProperties) then begin //Dispara evento de formatacao da linha
		Self.OnDetailRowProperties(Self, htr);
	end;
	for i := 0 to Self.DataSet.FieldCount - 1 do begin //Adciona cada campo visivel
		if Self.DataSet.Fields[i].Visible then begin
			htd := THTMLTableDetail.Create(''); //Cria Celula
			Self.LoadRecordField(htd, Self.DataSet.Fields[i]);
			htr.Add(htd);
		end;
	end;
end;

procedure TCustomDataSetWebOut.LoadRecordField(htd: THTMLTableDetail; Field: TField);
//----------------------------------------------------------------------------------------------------------------------
var
	slsMemo: TStringList;
begin
	htd.AlignHoriz := thAlignments[Field.Alignment];
	if Field is TMemoField then begin { Add all the lines }
		slsMemo := TStringList.Create;
		try
			slsMemo.Assign(TMemoField(Field));
			htd.Text := EscapeText(slsMemo.Text);
		finally
			slsMemo.Free;
		end;
	end else begin
		//A parte relativa aos links da table
		if Field = LinkField then begin //Este campo sera adcionado como link
			if LinkTarget <> nil then begin
				htd.Add(THTMLAnchor.Create(LinkTarget.AsString, EscapeText(LinkField.DisplayText)));
			end;
			if Assigned(Self.OnFieldCellURLLink) then begin
				OnFieldCellURLLink(Self, Field, htd);
			end;
		end else begin //Add text of field
			if Assigned(Self.FOnFieldCellContent) then begin //Dispara formatacao final da celula
				if not Self.FOnFieldCellContent(Self, Field, htd) then begin //Conteudo ainda vazio
					htd.Text := EscapeText(Field.DisplayText);
				end;
			end else begin
				if Field.DataType = ftDateTime then begin //Sem explicacao este tipo de campo da problemas
					htd.Text := EscapeText(DateTimeToStr(Field.AsDateTime));
				end else begin
					htd.Text := EscapeText(Field.DisplayText);
				end;
			end;
		end;
	end;
end;

{ TDataSetWebGrid }

procedure TDataSetWebGrid.AddAppendedRecords;
//----------------------------------------------------------------------------------------------------------------------
var
	f, i:     integer;
	htr:      THTMLTableRow;
	htd:      THTMLTableDetail;
	hChkBox:  THTMLCheckBoxField;
	OldState: TDataSetState;
begin
	OldState := Self.FState;
	Self.FState := dsInsert;
	try
		for i := 1 to Self.FWebAppendedRecordsAvailable do begin
			Inc(Self.FRecordsLoaded);
			htr := THTMLTableRow.Create;
			htr.Background := Self.FAppendRecordsBackground;
			if Assigned(Self.OnDetailRowProperties) then begin //Dispara evento de formatacao da linha
				Self.OnDetailRowProperties(Self, htr);
			end;
			for f := 0 to Self.DataSet.FieldCount - 1 do begin //Adciona cada campo visivel
				if Self.DataSet.Fields[f].Visible then begin
					htd := THTMLTableDetail.Create(''); //Cria Celula
					Self.LoadRecordField(htd, Self.DataSet.Fields[f]);
					htr.Add(htd);
				end;
			end;
			if Self.CheckRowPosition <> hcrPosNone then begin
				htd := THTMLTableDetail.Create(EmptyStr, 1, 1);
				hChkBox := THTMLCheckBoxField.Create(Self.WebDataSetName + '.' + IntToStr(Self.FRecordsLoaded), EmptyStr, True);
				htd.Add(hChkBox);
				if Assigned(Self.FOnGetColumnCheckBox) then begin
					FOnGetColumnCheckBox(Self, hChkBox, htd);
				end;
				case Self.CheckRowPosition of
					hcrPosFirst: begin
							htr.Insert(0, htd);
						end;
					hcrPosLast: begin
							htr.Add(htd);
						end;
				end;
			end;
			Self.Add(htr);
		end;
	finally
		Self.FState := OldState;
	end;
end;

constructor TDataSetWebGrid.Create(dtsDataSet: TDataSet; ParentWebDataSource: TWebDataSource);
//----------------------------------------------------------------------------------------------------------------------
begin
	inherited Create(dtsDataSet);
	FState := dsBrowse;
	Self.FAppendRecordsBackground := clNone;
	Self.FWebDataSource := ParentWebDataSource;
	if Assigned(ParentWebDataSource) then begin
		ParentWebDataSource.AddWebDataSet(Self);
	end;
	FKeyFieldsNames := TStringList.Create;
	FKeyFieldsValues := TStringList.Create;
	FInitialKey := TStringList.Create;
	FFinalKey := TStringList.Create;
	Self.FWebTableName := dtsDataSet.Name; // ***Sempre o nome do component para ser espelhado qdo buscar no DataModule
	Self.WebInfo := TWebDataSetInfoStorage.Create(nil);
	Self.WebInfo.WebDataGrid := Self;
end;

destructor TDataSetWebGrid.Destroy;
//----------------------------------------------------------------------------------------------------------------------
begin
	FKeyFieldsNames.Free;
	FKeyFieldsValues.Free;
	FInitialKey.Free;
	FFinalKey.Free;
	WebInfo.Free;
	inherited;
end;

function TDataSetWebGrid.LoadFromDataSet: integer;
//----------------------------------------------------------------------------------------------------------------------
var
	i:   integer;
	Val: string;
begin
	Self.FInitialKey.Clear; //Carrega a chave inicial
	for i := 0 to Self.FKeyFieldsNames.Count - 1 do begin
		Val := DataSet.FieldByName(FKeyFieldsNames.Strings[i]).AsString;
		Self.FInitialKey.Add(Val);
	end;
	Result := inherited LoadFromDataSet; //Limpa conteudo anterior e carrega os dados da tabela
	if not Self.ReadOnly then begin
		//Adcionar as linhas com os registros a serem adcionados
		Self.AddAppendedRecords;
	end;
	Self.FFinalKey.Clear; //Carrega a chave final
	for i := 0 to Self.FKeyFieldsNames.Count - 1 do begin
		Val := DataSet.FieldByName(FKeyFieldsNames.Strings[i]).AsString;
		Self.FFinalKey.Add(Val);
	end;
end;

function TDataSetWebGrid.WebFieldName(Field: TField): string;
//----------------------------------------------------------------------------------------------------------------------
begin
	Result := Self.FWebTableName + '.' + IntToStr(Self.FRecordsLoaded) + '.' + Field.FieldName;
end;

procedure TDataSetWebGrid.LoadHeaders(htr: THTMLTableRow);
//----------------------------------------------------------------------------------------------------------------------
var
	hth: THTMLTableHeading;
begin
	inherited;
	if Self.FCheckRowPosition <> hcrPosNone then begin
		hth := THTMLTableHeading.Create(EmptyStr, 1, 1);
		if Assigned(Self.FOnDetailHeaderProperties) then begin
			Self.FOnDetailHeaderProperties(Self, nil, hth); //Isso aqui pode dar muito pau Field = nil
		end;
		case Self.FCheckRowPosition of
			hcrPosFirst: begin
					htr.Insert(0, hth);
				end;
			hcrPosLast: begin
					htr.Add(hth);
				end;
		end;
	end;
end;

procedure TDataSetWebGrid.LoadRecord(htr: THTMLTableRow);
//----------------------------------------------------------------------------------------------------------------------
var
	Val:     string;
	i:       integer;
	hChkBox: THTMLCheckBoxField;
	htd:     THTMLTableDetail;
begin
	inherited;
	if Self.CheckRowPosition <> hcrPosNone then begin
		htd := THTMLTableDetail.Create(EmptyStr, 1, 1);
		hChkBox := THTMLCheckBoxField.Create(Self.WebDataSetName + '.' + IntToStr(Self.FRecordsLoaded), EmptyStr, True);
		htd.Add(hChkBox);
		if Assigned(Self.FOnGetColumnCheckBox) then begin
			FOnGetColumnCheckBox(Self, hChkBox, htd);
		end;
		case Self.CheckRowPosition of
			hcrPosFirst: begin
					htr.Insert(0, htd);
				end;
			hcrPosLast: begin
					htr.Add(htd);
				end;
		end;
	end;
	//Enumera campos chaves para montar a lista de seus valores
	if Self.FIncludeKeyFieldsList then begin
		for i := 0 to FKeyFieldsNames.Count - 1 do begin
			Val := DataSet.FieldByName(FKeyFieldsNames.Strings[i]).AsString;
			Self.FKeyFieldsValues.Add(Val);
		end;
	end;
end;

procedure TDataSetWebGrid.LoadRecordField(htd: THTMLTableDetail; Field: TField);
//----------------------------------------------------------------------------------------------------------------------
begin
	if Self.ReadOnly or Field.ReadOnly then begin
		if (State <> dsInsert) or ((Field.DataType = ftAutoInc) and (Self.LinkField = Field)) then begin
			inherited; //Conteudo sera sempre mostrado apenas
		end;
	end else begin //Verifica a edicao
		if Field.FieldKind = fkLookup then begin
			Self.EditLookupValue(Field, htd);
			Exit;
		end;
		case Field.DataType of

			ftString, ftWideString, ftFixedChar: begin //Tipos string de caracteres
					if not Self.EditStringValue(Field, htd) then begin //adciona controle na celula se necessario
						inherited;
					end
				end;

			ftSmallint, ftInteger, ftWord, ftLargeint: begin //Tipos cardinais
					if not Self.EditIntegerValue(Field, htd) then begin
						inherited;
					end;
				end;

			ftBoolean: begin //tipo boolean
					if not Self.EditBooleanValue(Field, htd) then begin
						inherited;
					end;
				end;

			ftFloat, ftCurrency: begin //Tipo numerico com precisao
					if not Self.EditIntegerValue(Field, htd) then begin //atualmente usa o mesmo que inteiro
						inherited;
					end;
				end;

			ftDate, ftTime, ftDateTime: begin //Tipo data/hora
					if not Self.EditStringValue(Field, htd) then begin //atualmente usa o mesmo que string
						inherited;
					end
				end;

			ftAutoInc: begin //Apenas exibe o valor
					if State <> dsInsert then begin //Para insercao nada sera mostrado aqui
						inherited;
					end;
				end;

			ftMemo, ftFmtMemo: begin
					if not Self.EditStringValue(Field, htd) then begin //atualmente usa o mesmo que string
						inherited;
					end
				end;

			ftGraphic: begin //LEGAL??? O QUE FAZER ?????
					inherited;
				end;
		end;
	end;
end;

function TDataSetWebGrid.EditStringValue(Field: TField; htd: THTMLTableDetail): boolean;
//----------------------------------------------------------------------------------------------------------------------
var
	HTMLItem: THTMLTextField;
begin
	//Calcula o controle para edicao do valor
	if Self.State = dsInsert then begin //Pega valor default
		HTMLItem := THTMLTextField.Create(Self.WebFieldName(Field), Field.DefaultExpression, 0, Field.DataSize);
		htd.Add(HTMLItem);
	end else begin //Pega valor atual
		HTMLItem := THTMLTextField.Create(Self.WebFieldName(Field), Field.AsString, 0, Field.DataSize);
		htd.Add(HTMLItem);
	end;
	Result := True;
end;

function TDataSetWebGrid.EditBooleanValue(Field: TField; htd: THTMLTableDetail): boolean;
//----------------------------------------------------------------------------------------------------------------------
var
	htSel: THTMLSelectField;
	Val:   boolean;
begin
	if (Self.State = dsInsert) and (Field.DefaultExpression <> EmptyStr) then begin
		Val := StrToBool(Field.DefaultExpression);
		htSel := THTMLSelectField.Create(Self.WebFieldName(Field));
		if Val then begin
			htSel.AddItem('SIM', 'YES', True);
			htSel.AddItem('NÃO', 'NO', False);
		end else begin
			htSel.AddItem('SIM', 'YES', False);
			htSel.AddItem('NÃO', 'NO', True);
		end;
	end;
	htSel := THTMLSelectField.Create(Self.WebFieldName(Field));
	if Field.AsBoolean then begin
		htSel.AddItem('SIM', 'YES', True);
		htSel.AddItem('NÃO', 'NO', False);
	end else begin
		htSel.AddItem('SIM', 'YES', False);
		htSel.AddItem('NÃO', 'NO', True);
	end;
	htd.AlignHoriz := thAlignments[Field.Alignment];
	htd.Add(htSel);
	Result := True;
end;

function TDataSetWebGrid.EditIntegerValue(Field: TField; htd: THTMLTableDetail): boolean;
//----------------------------------------------------------------------------------------------------------------------
var
	HTMLItem: THTMLTextField;
begin
	if Self.State = dsInsert then begin
		HTMLItem := THTMLTextField.Create(Self.WebFieldName(Field), Field.DefaultExpression, 0, Field.DisplayWidth);
	end else begin
		HTMLItem := THTMLTextField.Create(Self.WebFieldName(Field), Field.AsString, 0, Field.DisplayWidth);
	end;
	HTMLItem.Alignment := thcAlignments[Field.Alignment];
	htd.Add(HTMLItem);
	Result := True;
end;

function TDataSetWebGrid.EditLookupValue(Field: TField; htd: THTMLTableDetail): boolean;
//----------------------------------------------------------------------------------------------------------------------
var
	HTMLSelect: THTMLSelectField;
	SelItem:    THTMLSelectOption;
	ItemList:   TStringList;
	i:          integer;
	Filled:     boolean;
begin
	//Calcula o controle para edicao do valor
	Filled := False;
	i := 0;
	ItemList := TStringList.Create;
	try
		if Assigned(Self.FOnGetLookupValues) then begin
			Self.FOnGetLookupValues(Self, Field, ItemList, htd, Filled);
		end;
		if not Filled then begin
			i := DBFieldLookupList(Field, ItemList, _MAX_ITEMS_WEB_SELECT_); //-1 se falha
		end;
		if (ItemList.Count >= 0) or (i < 0) then begin
			if ItemList.Count > 0 then begin //Existem opcoes a serem expostas
				HTMLSelect := THTMLSelectField.Create(Self.WebFieldName(Field));
				htd.Add(HTMLSelect);
				for i := 0 to ItemList.Count - 1 do begin
					if Field.AsString = ItemList.Strings[i] then begin
						SelItem := THTMLSelectOption.Create(ItemList.Strings[i], EmptyStr, True);
					end else begin
						SelItem := THTMLSelectOption.Create(ItemList.Strings[i]);
					end;
					HTMLSelect.Add(SelItem);
				end;
			end else begin //( i = -1 )Nenhum opcao indica edicao livre ?????
				Self.EditStringValue(Field, htd);
			end;
		end;
	finally
		ItemList.Free;
	end;
	Result := True;
end;

procedure TDataSetWebGrid.CheckEmptyTable(DS: TDataSet);
//----------------------------------------------------------------------------------------------------------------------
begin
	if Self.FWebAppendedRecordsAvailable <= 0 then begin
	//Se nao houver geracao de registros para insercao checa registros de outro tipo
		inherited;
	end;
end;

{ TWebDataSource }

procedure TWebDataSource.AddWebDataSet(Instance: TDataSetWebGrid);
//----------------------------------------------------------------------------------------------------------------------
begin
	List.Add(Instance);
end;

function TWebDataSource.AsHTML: string;
//----------------------------------------------------------------------------------------------------------------------
begin
	Self.FValue := Self.GetValue;
	Result := inherited AsHTML;
end;

constructor TWebDataSource.Create(sName, sValue: string);
//----------------------------------------------------------------------------------------------------------------------
begin
	inherited;
	//Self.WebInfoStorage:=TWebDataSetInfoStorage.Create( nil, Self );
	List := TList.Create;
	Self.FWebName := sName;
end;

destructor TWebDataSource.Destroy;
//----------------------------------------------------------------------------------------------------------------------
begin
	List.Free;
	inherited;
end;

{ TWebInfoStorage }

function TWebDataSetInfoStorage.AsText: string;
//----------------------------------------------------------------------------------------------------------------------
begin
	Result := IOObj.ExportObjText(Self);
end;

function TWebDataSetInfoStorage.CheckCursorPos(DS: TDataSet; Index: integer): boolean;
//----------------------------------------------------------------------------------------------------------------------
//Checa se o registro atual possui as chaves com os valores existentes em KeyFieldsValues
var
	ManyKeys, FieldCnt: integer;
begin
	Result := True;
	if FKeyFieldsValues.Count > 0 then begin //Existem valores chaves para serem confrontados
		ManyKeys := Self.FKeyFieldsNames.Count;
		for FieldCnt := 0 to ManyKeys - 1 do begin
			if DS.FieldByName(Self.FKeyFieldsNames[FieldCnt]).AsString <> Self.FKeyFieldsValues.Strings[ManyKeys * index + FieldCnt]
			then begin
				Result := False;
				Exit;
			end;
		end;
	end;
end;

constructor TWebDataSetInfoStorage.Create(AOwner: TComponent);
//----------------------------------------------------------------------------------------------------------------------
begin
	inherited Create(AOwner);
	Self.KeyFieldsValues := TStringList.Create;
	Self.FKeyFieldsNames := TStringList.Create;
	Self.FInitialKey := TStringList.Create;
	Self.FFinalKey := TStringList.Create;
	Self.FKeyFieldsNames := TStringList.Create;
	Self.FWebDataGrid := nil;
	Self.FVersion := '1.0';
	Self.WebUpdateModes :=[wumInsert, wumEdit, wumDelete];
end;

destructor TWebDataSetInfoStorage.Destroy;
//----------------------------------------------------------------------------------------------------------------------
begin
	Self.KeyFieldsValues.Free;
	Self.FKeyFieldsNames.Free;
	Self.FInitialKey.Free;
	Self.FFinalKey.Free;
	inherited;
end;

function TWebDataSource.GetValue: string;
//----------------------------------------------------------------------------------------------------------------------
var
	j, i:           integer;
	ResultList, SL: TStringList;
	Item:           TDataSetWebGrid;
begin
	ResultList := TStringList.Create;
	try
		ResultList.Add('object WebDataSource : TWebDataSourceInput'); //O tipo aqui hard-coded para ser streamead na recuperacao
		SL := TStringList.Create;
		try
			for i := 0 to Self.List.Count - 1 do begin
				SL.Clear;
				Item := Self.List.Items[i];
				Item.FWebInfo.UpdateStatus;
				SL.Text := Item.FWebInfo.AsText;
				for j := 0 to SL.Count - 1 do begin //Endentacao
					SL.Strings[i]:= #9 + SL.Strings[i];
				end;
				ResultList.AddStrings(SL);
			end;
		finally
			SL.Free;
		end;
	finally
		ResultList.Add('end');
		Result := ResultList.Text;
		ResultList.Free;
	end;
end;

function TWebDataSetInfoStorage.DoAddNewRecords(DS: TDataSet; Request: TWebRequest; Response: TWebResponse;
	Log: TWebDataSetUpdate): integer;
//----------------------------------------------------------------------------------------------------------------------
//Checa e adciona os registros novos com os valores passados pelo WebRequest
var
	i: integer;
begin
	Result := 0;
	try
		for i :=(Self.FRecordsLoaded - Self.FAppendRecordsAvailble) to Self.FRecordsLoaded - 1 do begin
			if Self.DoBeforeAppendRecord(i, Request, Response) then begin
				DS.Append;
				try
					Self.DoUpdateRecord(DS, i, Request, Response);
					if Self.DoBeforePostRecord(i, Request, Response) then begin
						DS.Post;
						Inc(Result);
					end else begin
						DS.Cancel;
					end;
				except
					DS.Cancel; //Notificacao falha insercao registro foi dada pelo OnPostError desta classe repassada ao DataSet
				end;
			end;
		end;
	finally
		Inc(Log.RecInserted, Result);
	end;
end;

function TWebDataSetInfoStorage.DoBeforeAppendRecord(RecordIndex: integer; Request: TWebRequest; Response: TWebResponse): boolean;
//----------------------------------------------------------------------------------------------------------------------
begin
	if Assigned(Self.FOnBeforeAppendRecord) then begin
		Result := Self.FOnBeforeAppendRecord(Self, RecordIndex, Request, Response);
	end else begin
		Result := True;
	end;
end;

function TWebDataSetInfoStorage.DoBeforeEditRecord(RecordIndex: integer; Request: TWebRequest; Response: TWebResponse): boolean;
//----------------------------------------------------------------------------------------------------------------------
begin
	if Assigned(FOnBeforeEditRecord) then begin
		Result := Self.FOnBeforeEditRecord(Self, RecordIndex, Request, Response);
	end else begin
		Result := True;
	end;
end;

function TWebDataSetInfoStorage.DoBeforePostRecord(RecordIndex: integer; Request: TWebRequest; Response: TWebResponse): boolean;
//----------------------------------------------------------------------------------------------------------------------
begin
	if Assigned(FOnBeforePostRecord) then begin
		Result := Self.FOnBeforePostRecord(Self, RecordIndex, Request, Response);
	end else begin
		Result := True;
	end;
end;

function TWebDataSetInfoStorage.DoDeleteCheckedRecords(DS: TDataSet; Request: TWebRequest; Response: TWebResponse;
	Log: TWebDataSetUpdate): integer;
//----------------------------------------------------------------------------------------------------------------------
//Apaga os registros marcados no conjunto de delecao
var
	i: integer;
begin
	Result := 0;
	try
		for i := 0 to (Self.RecordsLoaded - 1) do begin
			if Self.IsCheckedRecord(i, Request) then begin
				if Self.DoSeekDataSet(DS, i) then begin
					try
						DS.Delete;
						Inc(Result);
					except
						//Ignora falha, Esta falha pode ser capturada no OnDeleteError do DataSet
					end;
				end;
			end;
		end;
	finally
		Inc(Log.RecDeleted, Result);
	end;
end;

function TWebDataSetInfoStorage.DoSeekDataSet(DS: TDataSet; Index: integer): boolean;
//----------------------------------------------------------------------------------------------------------------------
//Pega indice e posiciona o cursor da tabela nesta posicao segundo as regras passadas
var
	Lixo, KeyFields, OriginalIndex: string;
	ManyKeys, i, MaxIndex:          integer;
	KeyValues:                      Variant;
begin
	Result := False;
	//Tentar usar valores chaves( Meio mais seguro )
	ManyKeys := Self.FKeyFieldsNames.Count; //Qtd Campos da chave
	MaxIndex := ManyKeys *(index + 1); //Valor do indice final da lista de valores chaves ( KeyValuesFields )
	if (Self.FKeyFieldsValues.Count >= MaxIndex) and (Self.FKeyFieldsNames.Count > 0) then begin //Items para pesquisa
		for i := 0 to ManyKeys - 1 do begin
			KeyFields := KeyFields + Self.FKeyFieldsNames.Strings[i] + ';'
		end;
		KeyFields := Copy(KeyFields, 1, Length(KeyFields) - 1); //Remover ";" final
		KeyValues := VarArrayCreate([0, ManyKeys - 1], varVariant);
		for i := ManyKeys * index to MaxIndex - 1 do begin
			KeyValues[i - (ManyKeys * index)]:= Self.FKeyFieldsValues.Strings[i];
			Lixo := string(KeyValues[i - (ManyKeys * index)]);
		end;
		if DS is TTable then begin
			OriginalIndex := TTable(DS).IndexName;
			try
				TTable(DS).IndexName := EmptyStr;
				//!!!! Colocar na documentacao que os valores para a chave SEMPRE devem ser primarios
				Result := TTable(DS).Locate(KeyFields, KeyValues, [loPartialKey]);
				//Chave parcial pode permancer chamando CheckCursorPos
			finally
				TTable(DS).IndexName := OriginalIndex;
			end;
		end else begin //!!!Lembrar de possivel erro de seek para Queries aqui
			Result := DS.Locate(KeyFields, KeyValues, [loPartialKey]); //Chave parcial pode permancer chamando CheckCursorPos
		end;
	end;
	if Result and Self.CheckCursorPos(DS, index) then begin //Localizado OK
		Exit;
	end;
	//Sem os valores e campos chaves resta apenas usar a chave inicial e o Index=Salto/offset
	if Self.FInitialKey.Count > 0 then begin //Chave inicial setada para track do 1 registro
		ManyKeys := Self.FInitialKey.Count;
		for i := 0 to ManyKeys - 1 do begin
			KeyFields := KeyFields + Self.FKeyFieldsNames.Strings[i] + ';'
		end;
		KeyFields := Copy(KeyFields, 1, Length(KeyFields) - 1); //Remover ";" final
		KeyValues := VarArrayCreate([0, ManyKeys - 1], varVariant);
		for i := 0 to ManyKeys - 1 do begin
			KeyValues[i]:= Self.FInitialKey.Strings[i];
		end;
		if DS is TTable then begin
			OriginalIndex := TTable(DS).IndexName;
			try
				TTable(DS).IndexName := EmptyStr;
				//!!!! Colocar na documentacao que os valores para a chave SEMPRE devem ser primarios
				Result := TTable(DS).Locate(KeyFields, KeyValues, [loPartialKey]);
				//Chave parcial pode permancer chamando CheckCursorPos
			finally
				TTable(DS).IndexName := OriginalIndex;
			end;
		end else begin //!!!Lembrar de possivel erro de seek para Queries aqui
			Result := DS.Locate(KeyFields, KeyValues, [loPartialKey]); //Chave parcial pode permancer chamando CheckCursorPos
		end;
		if Result then begin
			Result :=(DS.MoveBy(index) = Abs(index));
			Result :=(Result and Self.CheckCursorPos(DS, index)); //CheckCursorPos talves desnecessario
		end;
	end else begin //Se nem mesno a chave inicial for dada, usar numero do registro
		DS.First;
		Result :=(DS.MoveBy(index) = Abs(index)) and (Self.CheckCursorPos(DS, index));
	end;
end;

function TWebDataSetInfoStorage.DoUpdateDataSetValues(DS: TDataSet; Request: TWebRequest; Response: TWebResponse;
	Log: TWebDataSetUpdate): integer;
//----------------------------------------------------------------------------------------------------------------------
//Varre os registros e atualiza os valores com os valores do WebRequest
//2003.03.07 - Colocado o retorno desta rotina para ser o qtd de registros processados
var
	i:                integer;
	Checked, Proceed: boolean;
begin
	Result := 0;
	Proceed := True;
	//Loop descrescente para possibilitar exclusoes sem alterar Seek pelos valores chaves e/ou offset
	for i := Self.FRecordsLoaded - Self.FAppendRecordsAvailble - 1 downto 0 do begin
		if Self.DoSeekDataSet(DS, i) then begin //Verifica se o cursor atende aos valores dos campos chaves
			if Assigned(Self.FOnCheckedRecord) then begin
				//Ler o valor de checked para o registro atual
				Checked := Str_Pas.StrToBool(Request.ContentFields.Values[Self.WebGridName + '.' + IntToStr(i + 1)]);
				//Falou em WebFields +1 no valor
				Proceed := Self.FOnCheckedRecord(DS, Request, Response, i, Checked);
				if not Proceed then begin
					Result := i; //Elementos processados
					Exit;
				end;
			end;
			if not Self.FReadOnly then begin
				if DoBeforeEditRecord(i, Request, Response) then begin
					DS.Edit;
					DoUpdateRecord(DS, i, Request, Response); //Preenche valores dos campos com dados montados do Request
					if DoBeforePostRecord(i, Request, Response) then begin
						DS.Post;
					end else begin
						DS.Cancel;
					end;
				end;
			end;
		end else begin
			if Assigned(Self.FOnWebGridSeekError) then begin
				FOnWebGridSeekError(Self, i, Request, Response, Proceed);
				if not Proceed then begin
					Result := i; //Elementos processados
					Exit;
				end;
			end;
		end;
	end;
end;

procedure TWebDataSetInfoStorage.DoUpdateRecord(DS: TDataSet; Index: integer; Request: TWebRequest; Response: TWebResponse);
//----------------------------------------------------------------------------------------------------------------------
//Atualiza os valores para o registro atual com os valores passados pelo WebRequest
var
	Prefix, WFName: string;
	i:              integer;
begin
	Prefix := Self.WebGridName + '.' + IntToStr(index + 1) + '.'; //Acrescentar 1 para WebVar
	for i := 0 to DS.FieldCount - 1 do begin
		WFName := Prefix + DS.Fields.Fields[i].FieldName;
		if Request.ContentFields.IndexOfName(WFName) <> -1 then begin
			case DS.Fields.Fields[i].DataType of
				ftFloat: begin
						DS.Fields.Fields[i].AsFloat := TStrConv.StrToFloat2(Request.ContentFields.Values[WFName]);
					end;
				ftCurrency: begin
						DS.Fields.Fields[i].AsCurrency := TStrConv.StrToCurrency(Request.ContentFields.Values[WFName]);
					end;
				ftBoolean: begin
						DS.Fields.Fields[i].AsBoolean := Str_Pas.StrToBool(Request.ContentFields.Values[WFName]);
					end;
			else begin
					DS.Fields.Fields[i].AsString := Request.ContentFields.Values[WFName];
				end;
			end;
		end;
	end;
end;

function TWebDataSetInfoStorage.IsCheckedRecord(WebInputRecordIndex: integer; Request: TWebRequest): boolean;
//----------------------------------------------------------------------------------------------------------------------
var
	WebVar: string;
begin
	WebVar := Self.WebGridName + '.' + IntToStr(WebInputRecordIndex + 1); //Toda WebVar +1
	Result := Request.ContentFields.Values[WebVar] <> EmptyStr;
end;

procedure TWebDataSetInfoStorage.UpdateStatus;
//----------------------------------------------------------------------------------------------------------------------
//Le os parametros a partir de seu WebDataGrid antes de sua exportacao em modo texto
begin
	if Self.FWebDataGrid.ReadOnly then begin
		Self.FAppendRecordsAvailble := 0;
	end else begin
		Self.FAppendRecordsAvailble := Self.FWebDataGrid.FWebAppendedRecordsAvailable;
	end;
	if Self.FWebDataGrid.DataSet is TTable then begin
		Self.FIndexName := TTable(Self.FWebDataGrid.DataSet).IndexName;
	end;
	Self.KeyFieldsValues.Assign(Self.FWebDataGrid.FKeyFieldsValues);
	Self.FKeyFieldsNames.Assign(Self.FWebDataGrid.FKeyFieldsNames);
	Self.FInitialKey.Assign(Self.FWebDataGrid.FInitialKey);
	Self.FFinalKey.Assign(Self.FWebDataGrid.FFinalKey);
	Self.FRecordsLoaded := Self.FWebDataGrid.RecordsLoaded;
	Self.FRecordOffSet := Self.FWebDataGrid.RecordOffSet;
	Self.FWebGridName := Self.FWebDataGrid.FWebTableName;
	Self.Name := Self.FWebGridName; //Coloca o nome igual ao WebName para VCLStreaming usa-lo
end;

{ TWebDataSourceInput }

procedure TWebDataSourceInput.DoNextWebDataSetInfoStorage(WDataGridInfo: TWebDataSetInfoStorage; Index: integer);
//----------------------------------------------------------------------------------------------------------------------
begin
	SetChildEvents(WDataGridInfo); //Ajustas os eventos para os mesmos do pai
	if Assigned(FOnNextWebDataSetInfoStorage) then begin
		FOnNextWebDataSetInfoStorage(WDataGridInfo, index);
	end;
end;

function TWebDataSourceInput.GetAsText: string;
//----------------------------------------------------------------------------------------------------------------------
begin
	Result := ClassHnd.ComponentToString(Self);
end;

procedure TWebDataSourceInput.SetAsText(const Value: string);
//----------------------------------------------------------------------------------------------------------------------
begin
	IOObj.ImportObjText(Self, Value);
end;

procedure TWebDataSourceInput.SetChildEvents(Child: TWebDataSetInfoStorage);
//----------------------------------------------------------------------------------------------------------------------
begin
	Child.FOnDeleteError := Self.FOnDeleteError;
	Child.FOnEditError := Self.FOnDeleteError;
	Child.FOnPostError := Self.FOnPostError;
	Child.FBeforeInsert := Self.FBeforeInsert;
	Child.FOnNewRecord := Self.FOnNewRecord;
	Child.FOnWebGridSeekError := Self.FOnWebGridSeekError; //Sempre Seta evento para filhos
	Child.FOnCheckedRecord := Self.FOnCheckedRecord;
	//Seta Os FOnBefore's?Record para o Child
	Child.FOnBeforeEditRecord := Self.FOnBeforeEditRecord;
	Child.FOnBeforePostRecord := Self.FOnBeforePostRecord;
	Child.FOnBeforeAppendRecord := Self.FOnBeforeAppendRecord;
	Child.WebUpdateModes := Self.WebUpdateModes;
end;

procedure TWebDataSourceInput.UpdateDataSets(Request: TWebRequest; Response: TWebResponse; DataSetContainer: TComponent);
//----------------------------------------------------------------------------------------------------------------------
//Varrer os dados do request para atualizar as tabelas
var
	i:             integer;
	WDataGridInfo: TWebDataSetInfoStorage;
begin
	for i := 0 to Self.ComponentCount - 1 do begin
		WDataGridInfo := TWebDataSetInfoStorage(Self.Components[i]);
		WDataGridInfo.ReadOnly :=(WDataGridInfo.FReadOnly or Self.FReadOnly);
		DoNextWebDataSetInfoStorage(WDataGridInfo, i);
		WDataGridInfo.UpdateTableData(Request, Response, DataSetContainer);
	end;
end;

function TWebDataSetInfoStorage.UpdateTableData(Request: TWebRequest; Response: TWebResponse; DataSetContainer: TComponent)
	: TWebDataSetUpdate;
//----------------------------------------------------------------------------------------------------------------------
//Usando o Request o DataSet localizado no DataSetContainer comecamos a inserir os dados
var
	DS:                         TDataSet;
	OldDeleteError:             TDataSetErrorEvent;
	OldOnEditError:             TDataSetErrorEvent;
	OldOnPostError:             TDataSetErrorEvent;
	OldBeforeInsert:            TDataSetNotifyEvent;
	OldOnNewRecord:             TDataSetNotifyEvent;
	OldCalcFields, OldControls: boolean;
begin
	DS := TDataSet(DataSetContainer.FindComponent(Self.WebGridName));
	if Assigned(DS) then begin
		if not(DS is TDataSet) then begin
			raise Exception.Create('Localizado ' + Self.WebGridName + ' em ' + DataSetContainer.Name + ' como ' + DS.ClassName);
		end;
		OldDeleteError := DS.OnDeleteError;
		OldOnEditError := DS.OnEditError;
		OldOnPostError := DS.OnPostError;
		OldBeforeInsert := DS.BeforeInsert;
		OldOnNewRecord := DS.OnNewRecord;
		OldControls := DS.ControlsDisabled; //Evita recarga nos deslocamentos e recalculo de campos
		DS.DisableControls;
		OldCalcFields := DS.AutoCalcFields;
		DS.AutoCalcFields := False;
		try
			if not(DS is TDBDataSet) then begin
				raise Exception.Create('Transações não suportadas para este DataSet');
			end;
			TDBDataSet(DS).Database.StartTransaction;
			try
				//Chamada ao atualizadores de registros e adicao de novos
				if wumDelete in Self.WebUpdateModes then begin
					DoDeleteCheckedRecords(DS, Request, Response, Result);
				end;
				if wumEdit in Self.WebUpdateModes then begin
					DoUpdateDataSetValues(DS, Request, Response, Result); //Atualiza dados
				end;
				if wumInsert in Self.WebUpdateModes then begin
					DoAddNewRecords(DS, Request, Response, Result); //Insere novos registros
				end;
				TDBDataSet(DS).Database.Commit;
			except
				on E: Exception do begin
					TDBDataSet(DS).Database.RollBack;
					raise;
				end;
			end;
		finally //Recupera os eventos;
			DS.OnDeleteError := OldDeleteError;
			DS.OnEditError := OldOnEditError;
			DS.OnPostError := OldOnPostError;
			DS.BeforeInsert := OldBeforeInsert;
			DS.OnNewRecord := OldOnNewRecord;
			if not OldControls then begin
				DS.EnableControls;
			end;
			DS.AutoCalcFields := OldCalcFields;
		end;
	end else begin
		raise Exception.CreateFmt('DataSet %s não localizado em %s', [Self.WebGridName, DataSetContainer.Name]);
	end;
end;

initialization

//----------------------------------------------------------------------------------------------------------------------
begin
	{ TODO 5 -oRoger -cDSG :
	  A alteracao do tipo matriz de HTMLBase para TPersistent pode gerar perca de desempenho
	  Procurar estudar a perca de performance em ambiente multi-thread }
	RegisterClasses([TWebDataSetInfoStorage, TWebDataSource, TWebDataSourceInput, TStringList]);
end;

finalization

//----------------------------------------------------------------------------------------------------------------------
begin
	UnRegisterClasses([TWebDataSetInfoStorage, TWebDataSource, TWebDataSourceInput, TStringList]);
end;

end.
