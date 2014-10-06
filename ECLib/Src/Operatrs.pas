{$IFDEF Operatrs }
{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I ECLib.inc}
unit Operatrs;

interface

uses
	Windows, SysUtils, Classes, Contnrs, Controls, IniFiles, Graphics, TreeHnd;

type
	TOperatorType         = (otUnknown, otResult, otVariable, otConstant, otFunction, otOperator);
	TOperatorReturnType   = (ortUnknown, ortInteger, ortNumber, ortString, ortBoolean, ortDate);
	TOperatorArgumentType = set of TOperatorReturnType;
	POperatorArgumentType = ^TOperatorArgumentType;

type
	TVisualOperator = class(TCollectionItem)
	private
		FDisplayName         : string;
		FOperatorType        : TOperatorType;
		FOperatorText        : string;
		FMaskString          : string;
		FMinOperandNumber    : integer;
		FMaxOperandNumber    : integer;
		FBackGroundColor     : TColor;
		FOperatorArgumentType: TOperatorArgumentType;
		FOperatorReturnType  : TOperatorArgumentType;
		function GetAsText: string;
		procedure SetOperatorType(const Value: TOperatorType);
		procedure SetMaxOperandNumber(const Value: integer);
		procedure SetMinOperandNumber(const Value: integer);
		procedure SetOperatorText(const Value: string);
		function GetImageIndex: integer;
		function GetDisableImageIndex: integer;
	protected
		FFontName : string;
		IconString: string;
		function GetDisplayName: string; override;
		procedure SetDisplayName(const Value: string); override;
		function GetUnknownOperator: TVisualOperator;
	public
		property AsText              : string read GetAsText;
		property BackGroundColor     : TColor read FBackGroundColor write FBackGroundColor;
		property FontName            : string read FFontName write FFontName;
		property ImageIndex          : integer read GetImageIndex;
		property DisableImageIndex   : integer read GetDisableImageIndex;
		property MaskString          : string read FMaskString write FMaskString;
		property MinOperandNumber    : integer read FMinOperandNumber write SetMinOperandNumber;
		property MaxOperandNumber    : integer read FMaxOperandNumber write SetMaxOperandNumber;
		property OperatorText        : string read FOperatorText write SetOperatorText; //Usado na montagem da expressao
		property OperatorType        : TOperatorType read FOperatorType write SetOperatorType;
		property OperatorArgumentType: TOperatorArgumentType read FOperatorArgumentType write FOperatorArgumentType;
		property OperatorReturnType  : TOperatorArgumentType read FOperatorReturnType write FOperatorReturnType;
		function AcceptArgumentReturnTypes(ArgumentType: TOperatorArgumentType): boolean;
		constructor Create(Collection: TCollection); override;
	end;

type
	TOperatorDictionary = class(TImageList)
	private
		OperatorList : TCollection;
		FOnUpdateItem: TNotifyEvent;
		procedure AddIconImage(Oper: TVisualOperator);
		procedure CreateIconImage(const Oper: TVisualOperator; Bmp: TBitmap; AFont: TFont);
		function GetOperatorCount: integer;
		function GetOperators(Index: integer): TVisualOperator;
		function GetResultOperator: TVisualOperator;
		function GetUnknownOperator: TVisualOperator;
		function GetVarOperator: TVisualOperator;
	protected
		procedure UpdateDictionary(IniFile: TIniFile);
		procedure LoadOperator(const OperName: string; IniFile: TIniFile);
	public
		property OperatorCount            : integer read GetOperatorCount;
		property Operators[index: integer]: TVisualOperator read GetOperators;
		property ResultOperator           : TVisualOperator read GetResultOperator;
		property UnknownOperator          : TVisualOperator read GetUnknownOperator;
		property VarOperator              : TVisualOperator read GetVarOperator;
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
		procedure Clear;
		function FindOperator(const OperatorName: string): TVisualOperator;
		procedure InitializeDefaultOperators;
		function LoadOperatorDictionary(const FileName: string): boolean;
		function UpdateIconCache(const FileName: string): boolean;
		procedure UpdateGUI(operator: TVisualOperator); virtual;
	published
		property OnUpdateItem: TNotifyEvent read FOnUpdateItem write FOnUpdateItem;
	end;

type
	TVisualOperand = class(TStrTree)
	private
		FOperatorDictionary: TOperatorDictionary;
		FOperatorName      : string;
		FValue             : string;
		FDisplayName       : string;
		function GetAsText: string;
		procedure SetAsText(const Value: string);
		procedure SetOperator(const Value: TVisualOperator);
		function GetImageIndex: integer;
		function GetMinOperandNumber: integer;
		function GetMaxOperandNumber: integer;
		function GetOperatorDictionary: TOperatorDictionary;
		function GetOperator: TVisualOperator;
		function GetParentOperand: TVisualOperand;
		function GetValue: string;
		function MaskedText(const Mask: string): string;
		function GetReturnType: TOperatorArgumentType; virtual;
		procedure SetReturnType(const Value: TOperatorArgumentType); virtual;
		procedure SetValue(const Value: string);
	protected
		function GetUnknownOperator: TVisualOperator;
		function GetDisplayName: string; virtual;
	public
		property AsText          : string read GetAsText write SetAsText;
		property DisplayName     : string read GetDisplayName write FDisplayName;
		property ImageIndex      : integer read GetImageIndex;
		property MaxOperandNumber: integer read GetMaxOperandNumber;
		property MinOperandNumber: integer read GetMinOperandNumber; property
		operator: TVisualOperator read GetOperator write SetOperator;
		property OperatorName: string read FOperatorName;
		property OperatorDictionary: TOperatorDictionary read GetOperatorDictionary write FOperatorDictionary;
		property ParentOperand: TVisualOperand read GetParentOperand;
		property ReturnType: TOperatorArgumentType read GetReturnType write SetReturnType;
		property Value: string read GetValue write SetValue;
		constructor Create(const AOperatorName: string; AOperatorDictionary: TOperatorDictionary; AParentOperand: TVisualOperand);
		procedure AddSubOperand(SubOperand: TVisualOperand);
		function AcceptOperand(Operand: TVisualOperand): boolean;
		function AcceptArgumentReturnTypes(ArgumentType: TOperatorArgumentType): boolean;
		procedure CheckArguments();
	end;

	TVisualOperandValue = class(TVisualOperand)
	private
		FReturnType: TOperatorArgumentType;
		FLixo      : string;
		function GetName: string;
	protected
		function GetDisplayName: string; override;
		function GetReturnType: TOperatorArgumentType; override;
		procedure SetName(const Value: string); override;
		procedure SetReturnType(const Value: TOperatorArgumentType); override;
	public
		property Lixo: string read FLixo write FLixo;
		property name: string read GetName write SetName;
		constructor Create(const AOperatorName: string; AOperatorDictionary: TOperatorDictionary; AParentOperand: TVisualOperand);
	end;

implementation

uses
	{$IFDEF DEBUG}
	Dialogs,
	{$ENDIF}
	Math, VCLHnd, grf_Hnd, ImgLHnd, Str_Pas;

const
	_STATIC_OPERATORS_COUNT_ = 3;
	//Secao Geral
	_INI_SECTION_GENERAL_ = 'Geral';
	_INI_ENTRY_NAME_      = 'Name';
	_INI_ENTRY_VARICON_   = 'VarIcon';
	_INI_ENTRY_CONSTICON_ = 'ConstIcon';
	//Secao dos operadores
	_INI_ENTRY_ICON_STRING_     = 'IconString';
	_INI_ENTRY_OPERATOR_TYPE_   = 'OperatorType';
	_INI_ENTRY_OPERANDS_TYPES_  = 'ArgumentTypes';
	_INI_ENTRY_RETURN_TYPE_     = 'ReturnType';
	_INI_ENTRY_OPERATORTEXT_    = 'OperatorText';
	_INI_ENTRY_MASKSTRING_      = 'Mask';
	_INI_ENTRY_MINOPERANDS_     = 'MinOperandNumber';
	_INI_ENTRY_MAXOPERANDS_     = 'MaxOperandNumber';
	_INI_ENTRY_FONTNAME_        = 'FontName';
	_INI_ENTRY_BACKGROUNDCOLOR_ = 'BackGroundColor';

	{ TDictionaryOperator }

procedure TOperatorDictionary.AddIconImage(Oper: TVisualOperator);
//----------------------------------------------------------------------------------------------------------------------
var
	Bmp: TBitmap;
begin
	Bmp := TBitmap.Create;
	try
		Bmp.Canvas.Font.Name := Oper.FontName;
		Self.CreateIconImage(Oper, Bmp, Bmp.Canvas.Font);
		Self.AddMasked(Bmp, clWhite);
	finally
		Bmp.Free;
	end;
end;

procedure TOperatorDictionary.Clear;
//----------------------------------------------------------------------------------------------------------------------
begin
	Self.OperatorList.Clear;
	inherited;
end;

constructor TOperatorDictionary.Create(AOwner: TComponent);
//----------------------------------------------------------------------------------------------------------------------
begin
	inherited;
	Self.OperatorList := TCollection.Create(TVisualOperator);
end;

procedure TOperatorDictionary.CreateIconImage(const Oper: TVisualOperator; Bmp: TBitmap; AFont: TFont);
//----------------------------------------------------------------------------------------------------------------------
//Cria Bmp com tamanho dado pelo imagelist em dois estados, na font passada
var
	ImgH, ImgW: integer;
	NewBmp    : TBitmap;
	R         : TRect;
begin
	//Dimensoes desejadas
	ImgH := Self.Height;
	ImgW := Self.Width;

	//Ajusta dimensoes e cor
	Bmp.Height             := ImgH;
	Bmp.Width              := 2 * ImgW;
	Bmp.Canvas.Brush.Color := Oper.BackGroundColor;
	Bmp.Canvas.FillRect(Bmp.Canvas.ClipRect);

	//Pinta texto em outra imagem
	NewBmp := TBitmap.Create;
	try
		//Ajusta nova imagem
		NewBmp.Canvas.Brush.Color := Oper.BackGroundColor;
		NewBmp.Canvas.Font.Assign(AFont);
		NewBmp.Height := NewBmp.Canvas.TextHeight(Oper.IconString);
		NewBmp.Width  := NewBmp.Canvas.TextWidth(Oper.IconString);

		//Imagem 1
		NewBmp.Canvas.TextOut(0, 0, Oper.IconString);
		R       := Bmp.Canvas.ClipRect;
		R.Right := R.Left + ((R.Right - R.Left) div 2);
		R       := grf_Hnd.RectCenterOverRect(R, NewBmp.Canvas.ClipRect); //Centraliza na parte esquerda
		Bmp.Canvas.Draw(R.Left, R.Top, NewBmp);

		//Imagem 2
		NewBmp.Canvas.Font.Color := clRed;
		NewBmp.Canvas.FillRect(NewBmp.Canvas.ClipRect);
		NewBmp.Canvas.TextOut(0, 0, Oper.IconString);
		Bmp.Canvas.Draw(R.Left + ImgW, R.Top, NewBmp); //Adciona offset ImgW
	finally
		NewBmp.Free;
	end;
end;

destructor TOperatorDictionary.Destroy;
//----------------------------------------------------------------------------------------------------------------------
begin
	Self.OperatorList.Free;
	inherited;
end;

function TOperatorDictionary.FindOperator(const OperatorName: string): TVisualOperator;
//----------------------------------------------------------------------------------------------------------------------
var
	i: integer;
begin
	for i := 0 to Self.OperatorList.Count - 1 do begin
		if Self.OperatorList.Items[i].DisplayName = OperatorName then begin
			Result := TVisualOperator(Self.OperatorList.Items[i]);
			Exit;
		end;
	end;
	Result := nil;
end;

function TOperatorDictionary.GetOperatorCount: integer;
//----------------------------------------------------------------------------------------------------------------------
begin
	Result := Self.OperatorList.Count;
end;

function TOperatorDictionary.GetOperators(Index: integer): TVisualOperator;
//----------------------------------------------------------------------------------------------------------------------
begin
	Result := TVisualOperator(Self.OperatorList.Items[index]);
end;

function TOperatorDictionary.GetResultOperator: TVisualOperator;
//----------------------------------------------------------------------------------------------------------------------
//Pega o primeiro operatortype = otResult
var
	i: integer;
begin
	for i := 0 to Self.OperatorList.Count - 1 do begin
		if Self.Operators[i].OperatorType = otResult then begin
			Result := Self.Operators[i];
			Exit;
		end;
	end;
	Result := nil; //Nenhum encontrado
end;

function TOperatorDictionary.GetUnknownOperator: TVisualOperator;
//----------------------------------------------------------------------------------------------------------------------
//Pega o primeiro operatortype = otUnknown
var
	i: integer;
begin
	for i := 0 to Self.OperatorList.Count - 1 do begin
		if Self.Operators[i].OperatorType = otUnknown then begin
			Result := Self.Operators[i];
			Exit;
		end;
	end;
	Result := nil; //Nenhum encontrado
end;

function TOperatorDictionary.GetVarOperator: TVisualOperator;
//----------------------------------------------------------------------------------------------------------------------
//Retornar o operador representativo de variavel
var
	i: integer;
begin
	for i := 0 to Self.OperatorList.Count - 1 do begin
		if Self.Operators[i].OperatorType = otVariable then begin
			Result := Self.Operators[i];
			Exit;
		end;
	end;
	Result := nil; //Nenhum encontrado
end;

procedure TOperatorDictionary.InitializeDefaultOperators;
//----------------------------------------------------------------------------------------------------------------------
//Carrega operandos resultado e nulo para uso imediato
var
	DicOper: TVisualOperator;
begin
	//Limpa elementos anteriores
	Self.Clear;

	//Cria o operador nulo
	DicOper := TVisualOperator.Create(Self.OperatorList);
	//atribuição dos atributos
	DicOper.DisplayName          := 'Operando';
	DicOper.FOperatorType        := otUnknown;
	DicOper.FOperatorText        := EmptyStr;
	DicOper.FMinOperandNumber    := 0;
	DicOper.FMaxOperandNumber    := 32;
	DicOper.BackGroundColor      := clYellow;
	DicOper.IconString           := '?';
	DicOper.OperatorArgumentType := [ortUnknown];
	DicOper.FOperatorReturnType  := [ortUnknown];
	Self.AddIconImage(DicOper);
	Self.UpdateGUI(DicOper);

	//Cria o operador Resultado
	DicOper                       := TVisualOperator.Create(Self.OperatorList);
	DicOper.DisplayName           := 'Resultado';
	DicOper.BackGroundColor       := clNavy;
	DicOper.FOperatorType         := otResult;
	DicOper.FOperatorText         := EmptyStr;
	DicOper.FMinOperandNumber     := 0;
	DicOper.FMaxOperandNumber     := 32;
	DicOper.IconString            := '=';
	DicOper.FOperatorArgumentType := [ortUnknown];
	DicOper.FOperatorReturnType   := [ortUnknown];
	Self.AddIconImage(DicOper);
	Self.UpdateGUI(DicOper);

	//Cria o operador Variavel
	DicOper                       := TVisualOperator.Create(Self.OperatorList);
	DicOper.DisplayName           := 'Valor';
	DicOper.BackGroundColor       := clGray;
	DicOper.FOperatorType         := otVariable;
	DicOper.FOperatorText         := EmptyStr;
	DicOper.FMinOperandNumber     := 0;
	DicOper.FMaxOperandNumber     := 0;
	DicOper.IconString            := 'X';
	DicOper.FOperatorArgumentType := [ortUnknown];
	DicOper.FOperatorReturnType   := [ortUnknown];
	Self.AddIconImage(DicOper);
	Self.UpdateGUI(DicOper);

end;

procedure TOperatorDictionary.LoadOperator(const OperName: string; IniFile: TIniFile);
//----------------------------------------------------------------------------------------------------------------------
//Carrega operador de nome "OperName" do Ini passado
var
	Oper : TVisualOperator;
	Entry: string;
	Ret  : TIntegerSet;
begin
	Self.OperatorList.BeginUpdate();
	try
		Oper             := TVisualOperator(Self.OperatorList.Add);
		Oper.DisplayName := OperName;

		//Tipo do operador
		Entry := IniFile.ReadString(OperName, _INI_ENTRY_OPERATOR_TYPE_, 'otUnknown');
		try
			Oper.FOperatorType := TOperatorType(VCLHnd.StrToOrdinalValue(TypeInfo(TOperatorType), Entry));
		except
			Oper.FOperatorType := otUnknown;
		end;
		//Tipo de retorno
		Entry := IniFile.ReadString(OperName, _INI_ENTRY_RETURN_TYPE_, 'ortUnknown');
		try
			Ret := VCLHnd.StrToSetValue(TypeInfo(TOperatorArgumentType), '[' + Entry + ']');
			{$WARN UNSAFE_CODE OFF}
			Oper.FOperatorReturnType := POperatorArgumentType(@Ret)^;
			{$WARN UNSAFE_CODE ON}
		except
			Oper.FOperatorReturnType := [ortUnknown];
		end;
		//Tipos dos argumentos
		Entry := IniFile.ReadString(OperName, _INI_ENTRY_OPERANDS_TYPES_, 'ortUnknown');
		try
			Ret := VCLHnd.StrToSetValue(TypeInfo(TOperatorArgumentType), '[' + Entry + ']');
			{$WARN UNSAFE_CODE OFF}
			Oper.OperatorArgumentType := POperatorArgumentType(@Ret)^;
			{$WARN UNSAFE_CODE ON}
		except
			Oper.OperatorArgumentType := [ortUnknown];
		end;
		//Mascara de saida
		Oper.FMaskString := IniFile.ReadString(OperName, _INI_ENTRY_MASKSTRING_, EmptyStr);
		//Limites dos operandos
		Oper.FOperatorText := IniFile.ReadString(OperName, _INI_ENTRY_OPERATORTEXT_, '?');
		if Oper.FOperatorType <> otUnknown then begin
			Oper.FMinOperandNumber := IniFile.ReadInteger(OperName, _INI_ENTRY_MINOPERANDS_, 1);
			Oper.FMaxOperandNumber := IniFile.ReadInteger(OperName, _INI_ENTRY_MAXOPERANDS_, 32);
		end else begin
			Oper.FMinOperandNumber := IniFile.ReadInteger(OperName, _INI_ENTRY_MINOPERANDS_, 0);
			Oper.FMaxOperandNumber := IniFile.ReadInteger(OperName, _INI_ENTRY_MAXOPERANDS_, 0);
		end;
		//Propriedades da fonte
		Oper.FontName := IniFile.ReadString(OperName, _INI_ENTRY_FONTNAME_, 'Arial');
		Entry         := IniFile.ReadString(OperName, _INI_ENTRY_BACKGROUNDCOLOR_, 'clNone');
		try
			Oper.BackGroundColor := StringToColor(Entry);
		except
			Oper.BackGroundColor := clNone;
		end;
		//Texto do icone
		Oper.IconString := IniFile.ReadString(OperName, _INI_ENTRY_ICON_STRING_, '( ? )');
		//Atualiza display
		Self.AddIconImage(Oper);
		Self.UpdateGUI(Oper);
	finally
		Self.OperatorList.EndUpdate();
	end;
end;

function TOperatorDictionary.LoadOperatorDictionary(const FileName: string): boolean;
//----------------------------------------------------------------------------------------------------------------------
//Carrega lista de operadores do arquivo
var
	IniFile: TIniFile;
begin
	Result := FALSE;
	if FileExists(FileName) then begin
		IniFile := TIniFile.Create(FileName);
		try
			Self.UpdateDictionary(IniFile);
		finally
			IniFile.Free;
		end;
	end else begin
		raise Exception.Create('Arquivo ' + FileName + ' não encontrado');
	end;
end;

procedure TOperatorDictionary.UpdateDictionary(IniFile: TIniFile);
//----------------------------------------------------------------------------------------------------------------------
//Carrega parametros e operadores do arquivo
var
	Sec: TStringList;
	i  : integer;
begin
	Sec := TStringList.Create;
	try
		//Descarrega operadores dinamicos
		for i := Self.OperatorCount - 1 downto _STATIC_OPERATORS_COUNT_ do begin
			Self.Delete(2 * i); //as duas imagens associadas
			Self.Delete(2 * i);
			Self.OperatorList.Delete(i);
		end;
		IniFile.ReadSections(Sec);
		for i := 0 to Sec.Count - 1 do begin
			if UpperCase(Sec.Strings[i]) <> UpperCase(_INI_SECTION_GENERAL_) then begin
				Self.LoadOperator(Sec.Strings[i], IniFile);
			end;
		end;
	finally
		Sec.Free;
		Self.UpdateGUI(nil);
	end;
end;

procedure TOperatorDictionary.UpdateGUI(operator: TVisualOperator);
//----------------------------------------------------------------------------------------------------------------------
begin
	if Assigned(Self.FOnUpdateItem) then begin
		Self.FOnUpdateItem(operator);
	end;
end;

function TOperatorDictionary.UpdateIconCache(const FileName: string): boolean;
//----------------------------------------------------------------------------------------------------------------------
begin
	Result := FALSE;
end;

{ TOperatorFrame }

function TVisualOperator.AcceptArgumentReturnTypes(ArgumentType: TOperatorArgumentType): boolean;
{ {
  Verifica se tipo passado possui retorno compativel com argumentos do operador

  Revision:6/6/2005
}
var
	rt1, rt2: ^Cardinal;
	Ret     : Cardinal;
begin
	{$TYPEDADDRESS OFF}
	if (ortUnknown in ArgumentType) or (ortUnknown in Self.OperatorArgumentType) then begin
		Result := TRUE;
		Exit;
	end else begin
		{$WARN UNSAFE_CODE OFF}
		rt1 := @Self.OperatorArgumentType;
		//Tipos dos argumentos
		rt2    := @ArgumentType; //Tipos de retorno passado
		Ret    := rt1^ and rt2^; //interseccao nula -> 0
		Result := (Ret <> 0);
		{$WARN UNSAFE_CODE ON}
	end;
	{$TYPEDADDRESS ON}
end;

constructor TVisualOperator.Create(Collection: TCollection);
//----------------------------------------------------------------------------------------------------------------------
begin
	inherited;
	Self.FFontName        := 'Arial';
	Self.FBackGroundColor := clNone;
	Self.FOperatorType    := otUnknown;
end;

function TVisualOperator.GetAsText: string;
//----------------------------------------------------------------------------------------------------------------------
begin
	{ TODO -oRoger -cDSG : Retorna texto do operador }
	Result := Self.FOperatorText;
end;

function TVisualOperator.GetDisableImageIndex: integer;
//----------------------------------------------------------------------------------------------------------------------
//Sempre o ImageIndex + 1
begin
	if Assigned(Self.Collection) then begin
		Result := 2 * (Self.Index) + 1;
	end else begin
		Result := 0;
	end;
end;

function TVisualOperator.GetDisplayName: string;
//----------------------------------------------------------------------------------------------------------------------
begin
	Result := Self.FDisplayName;
end;

function TVisualOperator.GetImageIndex: integer;
//----------------------------------------------------------------------------------------------------------------------
//Usa seu indice no Collection pai para obter este valor
begin
	if Assigned(Self.Collection) then begin
		Result := 2 * (Self.Index);
	end else begin
		Result := 0;
	end;
end;

function TVisualOperator.GetUnknownOperator: TVisualOperator;
//----------------------------------------------------------------------------------------------------------------------
//Retorna Operador desconhecido
var
	i: integer;
begin
	Result := nil;
	for i  := 0 to Self.Collection.Count - 1 do begin
		if TVisualOperator(Self.Collection.Items[i]).FOperatorType = otUnknown then begin
			Result := TVisualOperator(Self.Collection.Items[i]);
			Exit;
		end;
	end;
end;

procedure TVisualOperator.SetDisplayName(const Value: string);
//----------------------------------------------------------------------------------------------------------------------
begin
	Self.FDisplayName := Value;
	inherited;
end;

procedure TVisualOperator.SetMaxOperandNumber(const Value: integer);
//----------------------------------------------------------------------------------------------------------------------
begin
	FMaxOperandNumber := Value;
end;

procedure TVisualOperator.SetMinOperandNumber(const Value: integer);
//----------------------------------------------------------------------------------------------------------------------
begin
	FMinOperandNumber := Value;
end;

procedure TVisualOperator.SetOperatorText(const Value: string);
//----------------------------------------------------------------------------------------------------------------------
begin
	FOperatorText := Value;
end;

procedure TVisualOperator.SetOperatorType(const Value: TOperatorType);
//----------------------------------------------------------------------------------------------------------------------
begin
	FOperatorType := Value;
end;

{ TVisualOperand }

function TVisualOperand.AcceptOperand(Operand: TVisualOperand): boolean;
//----------------------------------------------------------------------------------------------------------------------
//Indica se o operando dado por ser um argumento valido para o operador do qual o atual descende
begin
	Result := Self.AcceptArgumentReturnTypes(Operand.Operator.OperatorReturnType);
end;

function TVisualOperand.AcceptArgumentReturnTypes(ArgumentType: TOperatorArgumentType): boolean;
//----------------------------------------------------------------------------------------------------------------------
//Verifica se tipo operador passado possui retorno compativel com argumentos do operador do qual este operando descende
begin
	Result := Self.Operator.AcceptArgumentReturnTypes(ArgumentType);
end;

procedure TVisualOperand.AddSubOperand(SubOperand: TVisualOperand);
//----------------------------------------------------------------------------------------------------------------------
begin
	SubOperand.OnAddSubNode    := Self.OnAddSubNode;
	SubOperand.OnRemoveSubNode := Self.OnRemoveSubNode;
	SubOperand.OnUpdateNode    := Self.OnUpdateNode;
	Self.Add(SubOperand);
	Self.CheckArguments();
	//Procura remover elementos inconsistentes
end;

procedure TVisualOperand.CheckArguments;
//----------------------------------------------------------------------------------------------------------------------
var
	i      : integer;
	Operand: TVisualOperand;
begin
	Self.BeginUpdate();
	try
		//Remove operandos que apresentam tipo incompativel com o novo tipo setado para este Operando
		for i := Self.Count - 1 downto 0 do begin
			if not Self.AcceptOperand(TVisualOperand(Self.Items[i])) then begin
				Self.Delete(i);
			end;
		end;

		//Insere faltantes
		while Self.Count < Self.MinOperandNumber do begin
			//Adcionar operandos sem tipo
			TVisualOperand.Create(Self.OperatorDictionary.UnknownOperator.DisplayName, Self.OperatorDictionary, Self);
		end;

		//Remove sobrantes
		if Self.Count > Self.MaxOperandNumber then begin
			for i := Self.Count - 1 downto 0 do begin
				if TVisualOperand(Self.Items[i]).Operator.OperatorType = otUnknown then begin
					Operand := TVisualOperand(Self.Items[i]);
					Self.Delete(Operand.ParentOperand.IndexOf(Operand));
				end;
				if Self.Count <= Self.MaxOperandNumber then begin
					Break;
				end;
			end;
		end;
	finally
		Self.EndUpdate();
	end;
end;

constructor TVisualOperand.Create(const AOperatorName: string; AOperatorDictionary: TOperatorDictionary;
	AParentOperand: TVisualOperand);
//----------------------------------------------------------------------------------------------------------------------
begin
	inherited Create(AParentOperand);
	Self.FDisplayName        := AOperatorName;
	Self.FValue              := EmptyStr;
	Self.Name                := AOperatorName;
	Self.FOperatorName       := AOperatorName;
	Self.FOperatorDictionary := AOperatorDictionary;
	if Assigned(AParentOperand) then begin
		AParentOperand.AddSubOperand(Self);
	end;
end;

function TVisualOperand.GetAsText: string;
//----------------------------------------------------------------------------------------------------------------------
var
	i: integer;
begin
	case Self.Operator.FOperatorType of
		otUnknown: begin
				Result     := EmptyStr;
				for i      := 0 to Self.Count - 1 do begin
					Result := Result + TVisualOperand(Self.Items[i]).AsText;
				end;
			end;
		otFunction: begin
				Result     := Self.Operator.AsText + '( ';
				for i      := 0 to Self.Count - 1 do begin
					Result := Result + TVisualOperand(Self.Items[i]).AsText;
				end;
				Result := Result + ' )';
			end;
		otOperator: begin { TODO -oRoger -cDSG : Arrumar forma de montar todos os operadores }
				Result     := EmptyStr;
				for i      := 0 to Self.Count - 1 do begin
					Result := Result + Self.MaskedText(Self.Operator.MaskString);
				end;
			end;
		otConstant, otVariable: begin
				Result := Self.Value;
			end;
		otResult: begin
				Result     := EmptyStr;
				for i      := 0 to Self.Count - 1 do begin
					Result := Result + TVisualOperand(Self.Items[i]).AsText;
				end;
			end;
	end;
end;

function TVisualOperand.GetImageIndex: integer;
//----------------------------------------------------------------------------------------------------------------------
begin
	Result := Self.Operator.ImageIndex;
end;

function TVisualOperand.GetMaxOperandNumber: integer;
//----------------------------------------------------------------------------------------------------------------------
begin
	Result := Self.Operator.MaxOperandNumber;
end;

function TVisualOperand.GetMinOperandNumber: integer;
//----------------------------------------------------------------------------------------------------------------------
begin
	Result := Self.Operator.MinOperandNumber;
end;

function TVisualOperand.GetOperator: TVisualOperator;
//----------------------------------------------------------------------------------------------------------------------
begin
	if Assigned(Self.OperatorDictionary) then begin
		Result := Self.OperatorDictionary.FindOperator(Self.OperatorName);
	end else begin
		Result := nil;
	end;
end;

function TVisualOperand.GetOperatorDictionary: TOperatorDictionary;
//----------------------------------------------------------------------------------------------------------------------
begin
	if Assigned(Self.FOperatorDictionary) then begin
		Result := Self.FOperatorDictionary;
	end else begin
		if Assigned(Self.ParentOperand) then begin
			Result                   := Self.ParentOperand.OperatorDictionary;
			Self.FOperatorDictionary := Result; //Cache it
		end else begin
			Result := nil;
		end;
	end;
end;

function TVisualOperand.GetParentOperand: TVisualOperand;
//----------------------------------------------------------------------------------------------------------------------
begin
	Result := TVisualOperand(Self.ParentNode);
end;

function TVisualOperand.GetReturnType: TOperatorArgumentType;
//----------------------------------------------------------------------------------------------------------------------
begin
	Result := Self.Operator.OperatorReturnType;
end;

function TVisualOperand.GetUnknownOperator: TVisualOperator;
//----------------------------------------------------------------------------------------------------------------------
//Retorna operador nulo
var
	RootOperand: TVisualOperand;
begin
	//!!!!Para esta rotina funcionar o no raiz deve ter um operator valido!!!!!
	RootOperand := TVisualOperand(Self.RootNode);
	Result      := RootOperand.Operator.GetUnknownOperator;
end;

function TVisualOperand.GetValue: string;
//----------------------------------------------------------------------------------------------------------------------
begin
	if Self.Operator.OperatorType in [otVariable, otConstant] then begin
		Result := FValue;
	end else begin
		Result := Self.AsText;
	end;
end;

function TVisualOperand.MaskedText(const Mask: string): string;
//----------------------------------------------------------------------------------------------------------------------
//var
//ThisLevelMask : string;
{$WARN NO_RETVAL ON}
begin
	{TODO -oroger -cfuture : entender a razão da falta de alertas para o retorno desta rotina}
	//ThisLevelMask:=GetDelimitedLevelSubStr( '%[', ']%', Mask, 0 );
	//ShowMessage( ThisLevelMask );
end;

procedure TVisualOperand.SetAsText(const Value: string);
begin

end;

procedure TVisualOperand.SetOperator(const Value: TVisualOperator);
//----------------------------------------------------------------------------------------------------------------------
begin
	if Assigned(Value) then begin
		FOperatorName := Value.DisplayName;
	end else begin
		//Se nil para valor o tipo sera otUnknow
		FOperatorName := Self.GetUnknownOperator().DisplayName;
	end;
end;

procedure TVisualOperand.SetReturnType(const Value: TOperatorArgumentType);
//----------------------------------------------------------------------------------------------------------------------
begin
	//Sempre le o valor de Self.Operator.OperatorReturnType;
end;

procedure TVisualOperand.SetValue(const Value: string);
//----------------------------------------------------------------------------------------------------------------------
begin
	if Self.Operator.OperatorType in [otVariable, otConstant] then begin
		//Altera apenas para os tipos mutaveis
		FValue := Value;
	end;
end;

function TVisualOperand.GetDisplayName: string;
//----------------------------------------------------------------------------------------------------------------------
begin
	Result := FDisplayName;
end;

{ TVisualOperandValue }

constructor TVisualOperandValue.Create(const AOperatorName: string; AOperatorDictionary: TOperatorDictionary;
	AParentOperand: TVisualOperand);
//----------------------------------------------------------------------------------------------------------------------
begin
	inherited;
	Self.ReturnType := Self.Operator.OperatorArgumentType;
	Self.FLixo      := 'LIXO';
end;

function TVisualOperandValue.GetDisplayName: string;
//----------------------------------------------------------------------------------------------------------------------
begin
	Result := Self.Name + '=' + '"' + Self.Value + '"';
end;

function TVisualOperandValue.GetName: string;
//----------------------------------------------------------------------------------------------------------------------
begin
	Result := Self.FDisplayName;
end;

function TVisualOperandValue.GetReturnType: TOperatorArgumentType;
//----------------------------------------------------------------------------------------------------------------------
begin
	Result := Self.FReturnType;
end;

procedure TVisualOperandValue.SetName(const Value: string);
//----------------------------------------------------------------------------------------------------------------------
begin
	Self.DisplayName := Value;
end;

procedure TVisualOperandValue.SetReturnType(const Value: TOperatorArgumentType);
//----------------------------------------------------------------------------------------------------------------------
begin
	Self.FReturnType := Value;
end;

end.
