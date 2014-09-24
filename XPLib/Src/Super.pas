{$IFDEF Super}
{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I XPLib.inc}
unit Super;

{ 1 }
{ {
}
interface

uses Windows, Messages;

// Mensagens definidas pelo usuario para implemtacao de atividades assincronas dentro desta biblioteca
const
  UM_BASE              = WM_USER + $0500; { { Definida como inicio das mensagens }
  UM_ERROR_BASE        = UM_BASE + 1;
  UM_COPY_ERROR        = UM_ERROR_BASE + 1;
  UM_TOOLTRAYICON      = UM_COPY_ERROR + 1;
  UM_TRAYRESETTOOLTIP  = UM_TOOLTRAYICON + 1;
  UM_SETTING_CHANGE    = UM_TRAYRESETTOOLTIP + 1;
  UM_PROGRESS_START    = UM_SETTING_CHANGE + 1;
  UM_INSTANCE_ANNOUNCE = UM_PROGRESS_START + 1;
  UM_REFRESH           = UM_INSTANCE_ANNOUNCE + 1;
  UM_APP_LOGIN         = UM_REFRESH + 1;

  UM_TIMER_BASE = $0550;
  UM_PULSE      = UM_TIMER_BASE + 1;
  UM_TCP_MSG    = UM_BASE + $0100;
  UM_DESTROY    = UM_BASE + $0150;

  // Tipos dados como limites de algum outro ou de estrutura
const
  _REAL_MIN_ABS = +1E-20;
  _REAL_MAX_ABS = +1E+20;
  _REAL_VAL_POS = +1E+20;
  _REAL_VAL_NEG = -1E+20;
  _STRING_SIZE  = 255;
  MaxInt64      = int64(4611686018427387904); // 2^62
  MinInt64      = int64(-4611686018427387904); // 2^62

  // constantes de som produzidas pelo sistema
const
  MB_SPEAKER = $FFFFFFFF;

  // constantes nao localizadas na API do windows e fornecidas aqui
const
  MAX_USERNAME_LENGTH = 2 * MAX_COMPUTERNAME_LENGTH;

  {$IFNDEF WIN32}

type
  DWORD = record
	{ 1 Tipo usado apenas para o Delphi 1 ( 16 bits ) }
	{ {
	  Tipo usado apenas para o Delphi 1 ( 16 bits )
	}
	HI: Word;
	{ 1 Parte alta da palavra dupla }
	{ {
	  Parte alta da palavra dupla
	}
	LO: Word;
	{ 1 Parte baixa da palavra dupla }
	{ {
	  Parte baixa da palavra dupla
	}
  end;

  Int16 = Integer;
  {$ELSE}

type
  Int16 = smallint;
  {$ENDIF}

type
  QUADWORD = record
	case Tag: boolean of
	  True:
		(DWORD1: DWORD;
			DWORD2: DWORD;
		);
	  False:
		(WORD1: Word;
			WORD2: Word;
			WORD3: Word;
			WORD4: Word;
		);
  end;

type
  PLong = ^longint;

type
  _REAL    = double;
  _GENERIC = _REAL;

  // Alinhamentos
  THrzPosition    = (hpLeft, hpCenter, hpRigth);
  TVrtPosition    = (vpUpper, vpCenter, vpLower);
  TControlPositon = (cpLeft, cpCenter, cpRigth);

type
  TBooleanState = (bsNo, bsYes, bsUndefined);

function HeapMemoryFree: longint; platform; deprecated;

function IIf(Cond: boolean; TrueCond, FalseCond: string): string; overload;

function IIf(Cond: boolean; TrueCond, FalseCond: Integer): Integer; overload;

function IIfInt(const Cond: boolean; const T, F: Integer): Integer;

function InRange(const Value, MinValue, MaxValue: Extended): boolean;

{$IFNDEF WIN32}
procedure IncPointer16(var P: Pointer; const Gap: Word);

procedure DecPointer16(var P: Pointer; const Gap: Word);

{$ENDIF}
function MaxVal(const Val1, Val2: double): double;

function MinVal(const Val1, Val2: double): double;

function ReturnAddress: Pointer;

procedure MemMoveESBOffset(const Source; const Ofs1: Integer; var Dest; const Ofs2: Integer; const Size: Integer);

function SwapInstances(pInstance1, pInstance2: Pointer; InstSize: Word): boolean;

implementation

function HeapMemoryFree: longint;
{ 1 Retorna a memoria do heap atualmente livre }
begin
  {$IFDEF MSWINDOWS}
  Result := GetHeapStatus.TotalFree;
  {$ELSE}
  Result := 0;
  {$ENDIF}
end;

function IIf(Cond: boolean; TrueCond, FalseCond: string): string; overload;
{ { 1 Se a condição passada for verdadeira retorna TrueCond, senao FalseCond }
begin
  if Cond then begin
	Result := TrueCond;
  end else begin
	Result := FalseCond;
  end;
end;

function IIf(Cond: boolean; TrueCond, FalseCond: Integer): Integer; overload;
{ { 1 Se a condição passada for verdadeira retorna TrueCond, senao FalseCond }
begin
  if Cond then begin
	Result := TrueCond;
  end else begin
	Result := FalseCond;
  end;
end;

function IIfInt(const Cond: boolean; const T, F: Integer): Integer;
{ {1 Retorna valor T se a condicao for for vedadeira e F se contrario }
begin
  if Cond then begin
	Result := T;
  end else begin
	Result := F;
  end;
end;

function InRange(const Value, MinValue, MaxValue: Extended): boolean;
{ {1 Valida se um valor esta dentro de dois limites }
begin
  Result := (Value >= MinValue) and (Value <= MaxValue);
end;

{$IFNDEF WIN32}

procedure DecPointer16(var P: Pointer; const Gap: Word);
{ {1 Decrementa um ponteiro de 16 bits com um "Gap" de bytes de enderecos }
var
  Flag: byte;
  Sgmt, Ofst: Word;
begin
  Flag := 0;
  Sgmt := DWORD(P).LO;
  Ofst := DWORD(P).HI;
  asm
	MOV     AX, Ofst
	MOV     BX, &Gap
	SUB     AX,BX;
	JNC     @@1  { Estouro de segmento }
	MOV &Flag, 01h
  @@1:

	MOV     &Ofst, AX
  end;
  if Flag <> 0 then begin
	Dec(Sgmt);
  end;
  P := Ptr(Sgmt, Ofst);
end;

procedure IncPointer16(var P: Pointer; const Gap: Word);
{ {1 Incrementa um ponteiro de 16 bits com um "Gap" de bytes de enderecos }
var
  Flag: byte;
  Sgmt, Ofst: Word;
begin
  Flag := 0;
  Sgmt := DWORD(P).LO;
  Ofst := DWORD(P).HI;
  asm
	MOV     AX, Ofst
	MOV     BX, &Gap
	ADD     AX,BX
	JNC     @@1        { Estouro de segmento }
	MOV &Flag, 01h
  @@1:

	MOV     &Ofst, AX
  end;
  if Flag <> 0 then begin
	Inc(Sgmt);
  end;
  P := Ptr(Sgmt, Ofst);
end;

{$ENDIF}

function MaxVal(const Val1, Val2: double): double;
{ {1 Retorna o maior valor dois passados }
begin
  if Val1 > Val2 then begin
	Result := Val1;
  end else begin
	Result := Val2;
  end;
end;

function MinVal(const Val1, Val2: double): double;
{ {1 Retorna o menor valor dois passados }
begin
  if Val1 > Val2 then begin
	Result := Val2;
  end else begin
	Result := Val1;
  end;
end;

function ReturnAddress: Pointer;
{ {1 Retorna o endereco do metodo anterior da pilha }
asm
  // MOV     EAX,[ESP + 4] !!! codegen dependant
  MOV     EAX,[EBP - 4]
end;

function SwapInstances(pInstance1, pInstance2: Pointer; InstSize: Word): boolean;
{ {1 Troca o conteúdo das instancias de classes iguais }
var
  TempBuffer: Pointer;
begin
  GetMem(TempBuffer, InstSize);
  if TempBuffer = nil then begin
	SwapInstances := False;
	Exit;
  end;
  Move(Pointer(pInstance1^), Pointer(TempBuffer^), InstSize);
  Move(Pointer(pInstance2^), Pointer(pInstance1^), InstSize);
  Move(Pointer(TempBuffer^), Pointer(pInstance2^), InstSize);
  FreeMem(TempBuffer, InstSize);
  SwapInstances := True;
end;

procedure MemMoveESBOffset(const Source; const Ofs1: Integer; var Dest; const Ofs2: Integer; const Size: Integer);
// ----------------------------------------------------------------------------------------------------------------------------------
asm
  push		esi
  push		edi

  mov		esi,		Source
  add		esi,		Ofs1
  mov		edi,		Dest
  add		edi,		Ofs2

  mov		eax,		Size
  mov		ecx,		eax

  cmp		edi,		esi
  jg		@@DOWN
  je		@@EXIT

  sar		ecx,2           		// copy count DIV 4 dwords
  js		@@EXIT

  rep		movsd

  mov		ecx,		eax
  and		ecx,		03h
  rep		movsb           		// copy count MOD 4 bytes
  jmp		@@EXIT

@@DOWN:
  lea		esi,		[esi+ecx-4] // point ESI to last dword of source
  lea		edi,		[edi+ecx-4] // point EDI to last dword of dest

  sar		ecx,		2			// copy count DIV 4 dwords
  js		@@EXIT
  std
  rep		movsd

  mov		ecx,		eax
  and		ecx,		03h         // Copy count MOD 4 bytes
  add		esi,		4-1         // point to last byte of rest
  add		edi,		4-1
  rep		movsb
  cld
@@EXIT:
  pop		edi
  pop		esi
end;

(* o programa abaixo desabilita ctrl+alt+del ???????
  program small;

  {written by Richard Leigh, Deakin Univesity 1997}

  uses
  WinProcs;

  {$R *.RES}

  var
  Dummy : integer;

  begin
  Dummy := 0;
  {Disable ALT-TAB}
  SystemParametersInfo( SPI_SETFASTTASKSWITCH, 1, @Dummy, 0);
  {Disable CTRL-ALT-DEL}
  SystemParametersInfo( SPI_SCREENSAVERRUNNING, 1, @Dummy, 0);
  end.

*)
end.
