{$IFDEF BDEHnd}
    {$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I BDELib.inc}

unit BDEHnd;

{{
Classes and methods to BDE correlated operations.
}

interface

uses
	Classes, BDE, DB, DbTables, SysUtils;

const
	PARADOX_NET_FILE_NAME = 'PDOXUSRS.NET';
	PARADOX_LCK_FILE1_NAME = 'Pdoxusrs.lck';
	PARADOX_LCK_FILE2_NAME = 'Paradox.lck';


type
	TFieldRecInfo = packed record
 {{
 Registro com os atributos basicos de um campo para base de dados relacional
 }
		szName: DBINAME;
		{1 The name of the field. }
		iType:  Word;
		{1 The type of the field }
		iSubType: Word;
		{1 Unknow function at 2005.05.18 }
		iLength: Word;
		{1 Size of the field }
		iPrecision: Byte;
		{1 Precision for float point fields, if nor applicable set to zero }
	end;


type
	IBDEConfigGetter = interface (IInterface)
		['{4642B05A-FB13-451B-B627-41B6FFE8AB96}']
 {{
 Interface in ReadOnly mode to access the BDE configuration properties.
 }
		function GetBDEDatabaseDir : string;
		{1 Get the Database diretory used by a BDE Database with local database files. }
		procedure GetBDEDatabaseParams(Params : TStrings);
		{1 Gets the parametes like a TStrings for a BDE Database. }
		function GetBDEDriverName : string;
		{1 Gets the driver name for a BDE Database. }
		function GetBDENetDir : string;
		{1 Returns the network diretory used by a BDE session. }
		function GetBDEPrivateDir : string;
		{1 Returns the private diretory used by a BDE session. }
        function GetBDEAliasName : string;
        {1 Returns the alias for a BDE connection. "" means a use based in explicit values of Database and etc }
	end;

	IBDEConfig = interface (IBDEConfigGetter)
		['{68466CC5-0D69-4078-98C3-84018ABAC6C2}']
 {{
 BDE Configuration is a interface to all BDE settings of an application in ReadWrite mode.
 }
		procedure SetBDEDatabaseDir(const Value : string);
		{1 Set the BDE Database directory to local files databases. }
		procedure SetBDEDriverName(const Value : string);
		{1 Set the Driver name used by BDE. }
		procedure SetBDENetDir(const Value : string);
		{1 Sets network diretory used by the BDE Session. }
		procedure SetBDEPrivateDir(const Value : string);
		{1 Sets the private directory used by BDE session. }
        procedure SetBDEAliasName( const AAliasName : string );
        {1 Sets the BDE AliasName for a BDE Database }
		property BDEDatabaseDir : string read GetBDEDatabaseDir write SetBDEDatabaseDir;
  {{
  Path to a directory where are the database files.
  }
		property BDEDriverName : string read GetBDEDriverName write SetBDEDriverName;
  {{
  The Driver name used by BDE database.
  }
		property BDENetDir : string read GetBDENetDir write SetBDENetDir;
  {{
  The path to the BDE network directory
  }
		property BDEPrivateDir : string read GetBDEPrivateDir write SetBDEPrivateDir;
       {{
       The path to the BDE private directory.
       }
        property BDEAliasName : string read GetBDEAliasName write SetBDEALiasName;
        {{
        The alias to used by a BDE database
        }
	end;



	TBDEHnd = Class (TObject)
 {{
 Implements routines for BDE related operations
 }
	public
		Class procedure ClearLockAndNetFiles(BDEConfig : IBDEConfigGetter);
		Class function GetBDEAliasPath(const AAliasName : string) : string;
		Class procedure InitializeDatabase(DataBase : TDatabase; BDEConfig : IBDEConfigGetter);
		Class procedure InitializeSession(Session : TSession; BDEConfig : IBDEConfigGetter);
	end;


procedure BDEDbaseExpressionCreateIndex(Table : TTable; IdxTagName, IdxExpression : PAnsiChar; Descending : boolean = FALSE; Unique :
	boolean = FALSE; Maintained : boolean = TRUE);

procedure BDEDbaseFieldsCreateIndex(Table : TTable; const IdxTagName, FieldName : string);

function BDEDBFDetachMDXIndexFile(const DBFFileName : string) : integer;

function BDEErrorString(Code : DBIResult) : string;

procedure BDEFlushDatabase(DB : TDatabase);

function BDEGetAliasPath(const AliasName : string) : string;

function BDEGetFileBaseFileName(Table : TTable) : string;

procedure BDELocalChangeFieldProps(Table : TTable; Field : TField; FieldRecInfo : TFieldRecInfo);

procedure BDEPackTable(Table : TTable);

procedure BDEParadoxFieldsCreateIndex(Table : TTable; const IdxName, FieldNames : string; Descending : boolean = FALSE;
	CaseInsensitive : boolean = FALSE; Unique : boolean = FALSE; Primary : boolean = FALSE; Maintained : boolean = TRUE);

procedure BDESetSessionDirs(ASession : TSession; const ANetDir, APrivDir : string);

procedure BDETblMakeCopy(Table : TTable; DestDir, DestName : PChar);

procedure BDETblRecreateIndex(Table : TTable);

function FindWithIndex(SrcTable : TTable; Values : array of const; idxName : string) : Boolean;

implementation

uses
	Windows, Str_Null, FileHnd, StrHnd, Str_Pas;


procedure BDEDbaseExpressionCreateIndex(Table : TTable; IdxTagName, IdxExpression : PAnsiChar;
	Descending : boolean = FALSE;
	Unique : boolean = FALSE;
	Maintained : boolean = TRUE);
{{
Cria um novo indice para tabelas Xbase do tipo expressao

Revision - Roger - 20110117

Alterada a passagem de parametros para suporte a UNICODE
 }
var
	NewIndex : IDXDesc;
	OldState, OldExc : boolean;
   TblFilename : AnsiString;
begin
	OldState := Table.Active;
	OldExc := Table.Exclusive;
	if not OldExc then begin
		Table.Active := FALSE;
		Table.Exclusive := TRUE;
	end;
	try
		Table.Active := TRUE;
	except
		Table.Exclusive := OldExc;
		raise;
	end;
	try
		StrLCopy(NewIndex.szTagName, IdxTagName, DBIMAXNAMELEN);
		NewIndex.bPrimary := FALSE;
		NewIndex.bUnique  := Unique;
		NewIndex.bDescending := Descending;
		NewIndex.bMaintained := Maintained;
		NewIndex.bSubset  := FALSE;
		NewIndex.bExpIdx  := TRUE;
		NewIndex.iFldsInKey := 1; //Sem razao, mas o exemplo colocava estes parametros
		NewIndex.aiKeyFld[0] := 2;
		StrLCopy(NewIndex.szKeyExp, IdxExpression, DBIMAXKEYEXPLEN);
		NewIndex.szKeyCond := '';
		NewIndex.bCaseInsensitive := FALSE;
		NewIndex.iBlockSize := 0;
       //Alterada a passagem de parametros para suporte a UNICODE
		//Check(DbiAddIndex(Table.dbhandle, Table.handle, PAnsiChar(Table.TableName), szDBASE, NewIndex, NIL));
       TblFilename:=AnsiString(Table.TableName);
       Check(DbiAddIndex(Table.dbhandle, Table.handle, PAnsiChar( TblFilename ), szDBASE, NewIndex, NIL));
	finally
		if not OldExc then begin
			Table.Active := FALSE;
			Table.Exclusive := OldExc;
		end;
		Table.Active := OldState;
	end;
end;

procedure BDEDbaseFieldsCreateIndex(Table : TTable; const IdxTagName, FieldName : string);
{{
Cria indice por unico campo para tabelas XBase

Revision - Roger - 20110117
Ajustes de tipo para compatibilidade com UNICODE
}
var
	Fld : TField;
	NewIndex : IDXDesc;
	OldState, OldExc : boolean;
   TblFilename : AnsiString;
begin
	OldState := Table.Active;
	OldExc := Table.Exclusive;
	if not OldExc then begin
		Table.Active := FALSE;
		Table.Exclusive := TRUE;
	end;
	try
		Table.Active := TRUE;
	except
		Table.Exclusive := OldExc;
		raise;
	end;
	try
		FillChar(NewIndex, SizeOf(IDXDesc), #0);
		Fld := Table.FieldByName(FieldName); //Erro se nao encontrar
		NewIndex.aiKeyFld[0] := Fld.FieldNo;
       StrLCopy(NewIndex.szTagName, PAnsiChar(AnsiString(IdxTagName)), DBIMAXNAMELEN);
       NewIndex.bPrimary := FALSE;
       NewIndex.bUnique  := FALSE;
       NewIndex.bDescending := FALSE;
       NewIndex.bMaintained := TRUE;
       NewIndex.bSubset  := FALSE;
       NewIndex.bExpIdx  := FALSE;
       NewIndex.iFldsInKey := 1; //Para DBase sempre unico campo
       NewIndex.szKeyExp := '';   // Although this is not an Expression index,
       NewIndex.szKeyCond := '';  // szKeyExp and szKeyCond must be set blank
       NewIndex.bCaseInsensitive := FALSE;
       NewIndex.iBlockSize := 0;
       TblFilename:=AnsiString(Table.TableName);
		Check(DbiAddIndex(Table.dbhandle, Table.handle, PAnsiChar(TblFilename), szDBASE, NewIndex, NIL));
	finally
		if not OldExc then begin
			Table.Active := FALSE;
			Table.Exclusive := OldExc;
		end;
		Table.Active := OldState;
	end;
end;

function BDEDBFDetachMDXIndexFile(const DBFFileName : string) : integer;
{{
Reset the byte stating there is an MDX associated with the given DBF
Vale apenas para tabelas Dbase 4 e 5. Checagem da versao nao pode ser feita pela estrutura
Para a reconstrucao dos indices originais deve-   se sabe-los e executar o codigo apropriado
}
const
	DBF_MDX_INDEX_FLAG_OFFSET = 28;
var
	PF, Handle : integer;
	Buffer : char;
begin
	Result := ERROR_SUCCESS;
	SetLastError(Result);
	if not FileExists(DBFFileName) then begin
		Result := ERROR_FILE_NOT_FOUND;
		Exit;
	end;
	Handle := FileOpen(DBFFileName, fmOpenReadWrite); //Abre R/W
	if Handle > 0 then begin  //Manipulador valido
		try
			PF := FileSeek(Handle, DBF_MDX_INDEX_FLAG_OFFSET, 0);
			if PF > 0 then begin
				PF := FileRead(Handle, Buffer, 1);
				if (Buffer = #1) and (PF > 0) then begin //change bytes to 'No MDX'
					FileSeek(Handle, DBF_MDX_INDEX_FLAG_OFFSET, 0);
					Buffer := #0;
					FileWrite(Handle, Buffer, 1);
				end else begin
					if PF <= 0 then begin  //Falha de leitura do arquivo
						Result := GetLastError();
					end else begin
						if (Buffer <> #0) then begin //Indica que arquivo possui flag nao reconhecida para indice
							Result := ERROR_MAPPED_ALIGNMENT; //se ficar ruim colocar ERROR_FILE_CORRUPT
						end;
					end;
				end;
			end else begin
				Result := GetLastError();
			end;
		finally
			FileClose(Handle);
		end;
		if Result = ERROR_SUCCESS then begin //Apagar arquivo MDX vinculado para liberar espaco
			SysUtils.DeleteFile(ChangeFileExt(DBFFileName, '.MDX'));
		end;
	end else begin
		Result := GetLastError();
	end;
end;

function BDEErrorString(Code : DBIResult) : string;
{{
Retorna string para o codigo de erro da BDE
}
var
	Msg : DBIMSG;
begin
	DbiGetErrorString(Code, Msg);
	Result := String(Msg);
	Trim(Result);
end;

procedure BDEFlushDatabase(DB : TDatabase);
{{
Varrer datasets para salvar dados em disco
}
var
	i : integer;
begin
	for i := 0 to DB.DatasetCount - 1 do begin
		if (DB.DataSets[i].Active) and (DB.DataSets[i] is TBDEDataSet) then begin
			dbiSaveChanges(TBDEDataSet(DB.DataSets[i]).Handle);
		end;
	end;
	for i := 0 to DB.DatasetCount - 1 do begin
		if (DB.DataSets[i].Active) then begin
			DB.DataSets[i].Refresh();
		end;
	end;
end;

function BDEGetAliasPath(const AliasName : string) : string;
{{
Retorna o caminho para uma base de dados baseada em arquivo
}
var
	SAlias : DBINAME;
	Desc : DBDesc;
	Params : TStrings;
begin
	Result := EmptyStr;
	StrPLCopy(SAlias, AnsiString(AliasName), SizeOf(SAlias) - 1);
	AnsiToOem(SAlias, SAlias);
	Check(DbiGetDatabaseDesc(SAlias, @Desc));
	if StrIComp(Desc.szDbType, szCFGDBSTANDARD) = 0 then begin
		OemToAnsi(Desc.szPhyName, Desc.szPhyName);
		Result := String(Desc.szPhyName);
	end else begin
		Params := TStringList.Create;
		try
			Session.Active := TRUE;
			Session.GetAliasParams(AliasName, Params);
			Result := Params.Values['SERVER NAME'];
		finally
			Params.Free;
		end;
	end;
end;

function BDEGetFileBaseFileName(Table : TTable) : string;
{{
Retorna o nome do arquivo vinculado a tabela usando file based database
}
begin
	{ TODO -oRoger -cFUTURE : Procurar por meio de acesso direto  a BDE, levar em consideracao possivel cursor invalido }
	Result := BDEGetAliasPath(Table.DatabaseName);
	Result := TFileHnd.ConcatPath([Result, ExtractFileName(Table.TableName)]);
	if not FileExists(Result) then begin
		Result := Table.TableName;
		Result := ExpandFileName(Result);
		if not FileExists(Result) then begin
			Result := EmptyStr;
		end;
	end;
end;

procedure BDELocalChangeFieldProps(Table : TTable; Field : TField; FieldRecInfo : TFieldRecInfo);
{{
Altera propriedades de um campo em tabelas Pdox/XBase
}
var
	Props : CURProps;
	hDb : hDBIDb;
	TableDesc : CRTblDesc;
	p : pCRTblDesc;
	pFields : pFLDDesc;
	pOp : pCROpType;
begin
	if not ((Table.Active) and (Table.Exclusive)) then begin
		raise EDatabaseError.CreateFmt('Tabela %s para restruturação deve estar aberta no modo exclusivo',
			[Table.TableName]);
	end;
	if (StrComp(FieldRecInfo.szName, PAnsiChar(EmptyStr)) = 0) then begin  //Garante referencia ao nome do campo
		StrLCopy(FieldRecInfo.szName, PAnsiChar(Field.FieldName), DBIMAXNAMELEN);
	end;
	Check(DbiGetCursorProps(Table.Handle, Props));
	pFields := AllocMem(Table.FieldCount * sizeof(FLDDesc));
	pOp := AllocMem(Table.FieldCount * sizeof(CROpType));
	Inc(pOp, Field.Index);
	pOp^ := crMODIFY;
	Dec(pOp, Field.Index);
	try
		Check(DbiGetFieldDescs(Table.Handle, pFields));
		Inc(pFields, Field.Index);
		if Length(FieldRecInfo.szName) > 0 then begin
			pFields^.szName := FieldRecInfo.szName;
		end;
		if FieldRecInfo.iType > 0 then begin
			pFields^.iFldType := FieldRecInfo.iType;
		end;
		if FieldRecInfo.iSubType > 0 then begin
			pFields^.iSubType := FieldRecInfo.iSubType;
		end;
		if FieldRecInfo.iLength > 0 then begin
			pFields^.iUnits1 := FieldRecInfo.iLength;
		end;
		if FieldRecInfo.iPrecision > 0 then begin
			pFields^.iUnits2 := FieldRecInfo.iPrecision;
		end;
		Dec(pFields, Field.Index);
		FillChar(TableDesc, sizeof(TableDesc), #0);
		Check(DbiGetObjFromObj(hDBIObj(Table.Handle), objDATABASE, hDBIObj(hDb)));
		StrPCopy(TableDesc.szTblName, Table.TableName);
		StrPCopy(TableDesc.szTblType, Props.szTableType);

		TableDesc.iFldCount := Table.FieldCount;
		TableDesc.pecrFldOp := pOp;
		TableDesc.pFldDesc  := pFields;
		Table.Close;
		P := @TableDesc;
		Check(DbiDoRestructure(hDb, 1, P, NIL, NIL, NIL, FALSE));
	finally
		if pFields <> NIL then begin
			FreeMem(pFields, sizeof(pFields));
		end;
		if pOp <> NIL then begin
			FreeMem(pOp, sizeof(pOp));
		end;
	end;
end;

procedure BDEPackTable(Table : TTable);
{{
Realiza Pack para tabelas que usam os drivers DBase e Paradox
Rotina importada da RX/JEDI e adaptada
}
var
	FCurProp : CurProps;   //FCurProp holds information about the structure of the table
	TblDesc : CRTblDesc; //Specific information about the table structure, indexes, etc.
	hDb : hDbiDB; //Uses as a handle to the database
	TablePath : array[0..dbiMaxPathLen] of AnsiChar; { Path to the currently opened table }
	OldExclusive, OldState : boolean;
begin
	OldState := Table.Active;
	Table.Active := TRUE; //Necessario abrir o cursor para esta tabela
	try
		Check(DbiGetCursorProps(Table.Handle, FCurProp));
		//Pack para tabela PARADOX
		if StrComp(FCurProp.szTableType, szParadox) = 0 then begin //Call DbiDoRestructure procedure if PARADOX table
			hDb := NIL;
			FillChar(TblDesc, SizeOf(CRTblDesc), 0); { Initialize the table descriptor }
			StrPCopy(TblDesc.szTblName, Table.TableName); { Place the table name in descriptor }
			StrCopy(szTblType, FCurProp.szTableType);  { Place the table type in descriptor }
			TblDesc.bPack := TRUE;
			TblDesc.bProtected := FCurProp.bProtected;
			//Get the current table's directory. This is why the table MUST be opened until now
			Check(DbiGetDirectory(Table.DBHandle, FALSE, TablePath));
			Table.Close; //Se OldState sera reaberta ao final
			//NOW: since the DbiDoRestructure call needs a valid DB handle BUT the
			//table cannot be opened, call DbiOpenDatabase to get a valid handle.
			//Setting TTable.Active = False does not give you a valid handle
			Check(DbiOpenDatabase(NIL, szCFGDBSTANDARD, dbiReadWrite, dbiOpenExcl, NIL, 0, NIL, NIL, hDb));
			Check(DbiSetDirectory(hDb, TablePath)); //Set the table's directory to the old directory
			Check(DbiDoRestructure(hDb, 1, @TblDesc, NIL, NIL, NIL, FALSE)); //Pack the PARADOX table
			Check(DbiCloseDatabase(hDb)); //Close the temporary database handle
		end else begin
			if StrComp(FCurProp.szTableType, szDBase) = 0 then begin  //Call DbiPackTable procedure if dBase table
				OldExclusive := Table.Exclusive;
				Table.Close;
				try
					Table.Exclusive := TRUE;
					Table.Open;
					Check(DbiPackTable(Table.DBHandle, Table.Handle, NIL, NIL, TRUE));
				finally
					if Table.Exclusive <> OldExclusive then begin //Necessario fechar para alterar o modo
						Table.Close;
						Table.Exclusive := OldExclusive;
					end;
				end;
			end else begin //Nao se aplica para tipos <> (DBase e Paradox)
				DbiError(DBIERR_WRONGDRVTYPE);
			end;
		end;
	finally
		Table.Active := OldState;
	end;
end;

procedure BDEParadoxFieldsCreateIndex(Table : TTable; const IdxName, FieldNames : string;
	Descending : boolean = FALSE;
	CaseInsensitive : boolean = FALSE;
	Unique : boolean = FALSE;
	Primary : boolean = FALSE;
	Maintained : boolean = TRUE);
{{
Adciona um novo indice para as tabelas Paradox O valor de FieldNames deve conter os campos separados por ";"
}
var
	FName : string;
	FldCount, i : integer;
	Fld : TField;
	NewIndex : IDXDesc;
begin
	FldCount := StrCountCharRepet(';', FieldNames);
	FillChar(NewIndex, SizeOf(IDXDesc), #0);
	if GetIChar(FieldNames, Length(FieldNames)) = ';' then begin
		Dec(FldCount);
	end; //Remove possivel finalizador incorreto
	for i := 0 to FldCount do begin
		FName := Str_Pas.GetDelimitedSubStr(';', FieldNames, i);
		Fld := Table.FieldByName(FName); //Erro se nao encontrar
		NewIndex.aiKeyFld[i] := Fld.FieldNo;
	end;
	StrLCopy(NewIndex.szName, PAnsiChar(IdxName), DBIMAXTBLNAMELEN);
	with NewIndex do begin
		StrLCopy(szTagName, PAnsiChar(IdxName), DBIMAXNAMELEN);
		bPrimary := Primary;
		bUnique  := Unique;
		bDescending := Descending;
		bMaintained := Maintained;
		bSubset  := FALSE;
		bExpIdx  := FALSE;
		iFldsInKey := (FldCount + 1);
		szKeyExp := '';   // Although this is not an Expression index,
		szKeyCond := '';  // szKeyExp and szKeyCond must be set blank
		bCaseInsensitive := CaseInsensitive;
		iBlockSize := 0;
	end;
	Check(DbiAddIndex(Table.dbhandle, Table.handle, PAnsiChar(Table.TableName), szDBASE, NewIndex, NIL));
end;

procedure BDESetSessionDirs(ASession : TSession; const ANetDir, APrivDir : string);
//----------------------------------------------------------------------------------------------------------------------------------
//Ajusta caminhos para uma sessao BDE
var
	OldSessionState : boolean;
	UsedPrivDir, UsedNetDir : string;
begin
	if (ANetDir = EmptyStr) then begin   //Ler valores padrao
		UsedNetDir := GetCurrentDir();
	end else begin
		UsedNetDir := ANetDir;
	end;
	if (APrivDir = EmptyStr) then begin
		UsedNetDir := GetCurrentDir();
	end else begin
		UsedPrivDir := APrivDir;
	end;
	OldSessionState := Session.Active;
	ASession.Active := FALSE;
	//Limpeza dos arquivos de rede e lck das pastas NetDir PrivDir
	FileHnd.DeleteMaskedFiles(TFileHnd.ConcatPath([UsedNetDir, PARADOX_NET_FILE_NAME]));
	FileHnd.DeleteMaskedFiles(TFileHnd.ConcatPath([UsedNetDir, PARADOX_LCK_FILE1_NAME]));
	FileHnd.DeleteMaskedFiles(TFileHnd.ConcatPath([UsedNetDir, PARADOX_LCK_FILE2_NAME]));
	FileHnd.DeleteMaskedFiles(TFileHnd.ConcatPath([UsedPrivDir, PARADOX_NET_FILE_NAME]));
	FileHnd.DeleteMaskedFiles(TFileHnd.ConcatPath([UsedPrivDir, PARADOX_LCK_FILE1_NAME]));
	FileHnd.DeleteMaskedFiles(TFileHnd.ConcatPath([UsedPrivDir, PARADOX_LCK_FILE2_NAME]));
	ASession.Active := TRUE;
	ASession.PrivateDir := UsedPrivDir;  //Preservar esta ordem ( Priv, Net )para usar pre-existentes
	ASession.NetFileDir := UsedNetDir;
	if not OldSessionState then begin
		ASession.Active := FALSE;
	end;
end;

procedure BDETblMakeCopy(Table : TTable; DestDir, DestName : PChar);
{{
Copia uma tabela para uma dada pasta, se o nome nao for dado usa o mesmo
}
var
	WasActive : Boolean;
	WasExclusive : Boolean;
	SrcName, DestFile : PAnsiChar;
begin
	SrcName := StrAllocAnsiString(Table.TableName);
	if DestName = NIL then begin
		DestFile := StrAllocAnsiString(TFileHnd.ConcatPath([DestDir, ExtractFileName(Table.TableName)]));
	end else begin
		DestFile := StrAllocAnsiString(TFileHnd.ConcatPath([DestDir, string(DestName)]));
	end;
	try
		if not DirectoryExists(DestDir) then begin
			RaiseLastOSError;
		end;
		with Table do begin
			WasActive := Active;
			WasExclusive := Exclusive;
			DisableControls;
			try
				if not (WasActive and WasExclusive) then begin
					Close;
				end;
				try
					Exclusive := TRUE;
					Open;
					Check(BDE.DbiCopyTable(DBHandle, TRUE, SrcName, NIL, PAnsiChar(DestFile)));
				finally
					if not (WasActive and WasExclusive) then begin
						Close;
						Exclusive := WasExclusive;
						Active := WasActive;
					end;
				end;
			finally
				EnableControls;
			end;
		end;
	finally
		StrDispose(SrcName);
		StrDispose(DestFile);
	end;
end;

procedure BDETblRecreateIndex(Table : TTable);
//----------------------------------------------------------------------------------------------------------------------------------
{{
Rotina simples de reindexacao
}
var
	WasActive : Boolean;
	WasExclusive : Boolean;
begin
	with Table do begin
		WasActive := Active;
		WasExclusive := Exclusive;
		DisableControls;
		try
			if not (WasActive and WasExclusive) then begin
				Close;
			end;
			try
				Exclusive := TRUE;
				Open;
				Check(dbiRegenIndexes(Handle));
			finally
				if not (WasActive and WasExclusive) then begin
					Close;
					Exclusive := WasExclusive;
					Active := WasActive;
				end;
			end;
		finally
			EnableControls;
		end;
	end;
end;

function FindWithIndex(SrcTable : TTable; Values : array of const; idxName : string) : Boolean;
{{
Funcao generica de pesquisa utilizando o indice passado.
}
var
	oldidx : string;
begin
	oldidx := SrcTable.IndexName;
	try
		if (oldidx <> idxName) then begin
			SrcTable.IndexName := idxName;
		end;
		Result := SrcTable.FindKey(Values);
	finally
		SrcTable.IndexName := oldidx;
	end;
end;



{{
Implements routines for BDE related operations
}
{-**********************************************************************
************************************************************************
******************
******************  Class:    TBDEHnd
******************  Category: No category
******************
************************************************************************
************************************************************************}
{{
Implements routines for BDE related operations
}
{--------------------------------------------------------------------------------------------------------------------------------}
Class procedure TBDEHnd.ClearLockAndNetFiles(BDEConfig : IBDEConfigGetter);
{{
Delete any BDE control files, to avoid invalid locks.

BDEConfig is Interface to IBDEConfigGetter used for get the BDE configurations.
Directories affecteds are : PrivateDir, NetDir and the DatabaseDir for Local Databases only.

See also
IBDEConfigGetter
}

const
	FILE_MASKS: array[0..3] of string = ('Pdoxusrs.lck', 'Pdoxusrs.net', 'paradox.lck', 'INTERLCK.*');
var
	FList : TStringList;

	//......................................................................................................................
	procedure LSRClearDir(const Dir : string);
	var
		i : integer;
	begin
		if (Dir <> EmptyStr) and DirectoryExists(Dir) then begin
			DirFileList(FList, Dir, FILE_MASKS);
			for i := 0 to FList.Count - 1 do begin
				DeleteFile(PChar(FList.Strings[i]));
			end;
		end;
	end;

begin
	//    const PrivDir, NetDir, DatabaseDir : string //A serem pegos pela interface
	FList := TStringList.Create;
	try
		LSRClearDir(BDEConfig.GetBDEPrivateDir());
		LSRClearDir(BDEConfig.GetBDENetDir());
		LSRClearDir(BDEConfig.GetBDEDatabaseDir());
	finally
		FList.Free;
	end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
Class function TBDEHnd.GetBDEAliasPath(const AAliasName : string) : string;
{{
Returns a path for a specific BDE Alias
}
var
	database : TDatabase;
	pzDir : PAnsiChar;
begin
	database := TDatabase.Create(NIL);
	try
		pzDir := AnsiStrAlloc(MAX_PATH);
		try
			with database do begin
				AliasName := AAliasName;
				DatabaseName := 'temp'; //Any name used
				Connected := TRUE;
				dbigetdirectory(Handle, TRUE, pzDir);
				Result := StrPas(pzDir);
				Connected := FALSE;
			end;
		finally
			StrDispose(pzDir);
		end;
	finally
		database.Free;
	end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
Class procedure TBDEHnd.InitializeDatabase(DataBase : TDatabase; BDEConfig : IBDEConfigGetter);
{{
Initialize a Database properties via a BDEConfig interface. Util for a automatic configuration load.
To SQLBased Database the Directory property is not loaded.

}
begin
	Database.Connected := FALSE;
	Database.Params.Clear();
	BDEConfig.GetBDEDatabaseParams(Database.Params);
	Database.Connected := TRUE;
    if ( not Database.IsSQLBased ) then begin
        Database.Directory := BDEConfig.GetBDEDatabaseDir();
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
Class procedure TBDEHnd.InitializeSession(Session : TSession; BDEConfig : IBDEConfigGetter);
{{
Initialize a Session properties via a BDEConfig interface. Util for a automatic configuration load.
}
begin
	Session.Active := FALSE;
	ClearLockAndNetFiles(BDEConfig);
	Session.ConfigMode := [cfmSession];
	Session.Active := TRUE;
	Session.PrivateDir := TFileHnd.SlashRem(BDEConfig.GetBDEPrivateDir());
	Session.NetFileDir := TFileHnd.SlashRem(BDEConfig.GetBDENetDir());
end;

end.


