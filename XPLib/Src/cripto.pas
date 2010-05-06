{$IFDEF cripto}
	{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I XPLib.inc}

unit cripto;

interface
//Decodifica uma string usando apenas ASCII reduzido
function ASCIIDec(const Str : string) : string;
//Codifica uma string usando apenas ASCII reduzido
function ASCIIEnc(const Str : string) : string;
//Criptografa/Decriptografa um arquivo usando matriz de off-sets
function CriptoFileByOffSet( const FileIn, FileOut : string; Cripto : boolean) : boolean;
//Decodifica uma string usando apenas sequencias de numeros espacados por brancos
function CriptoSpaceDecode( Str : string ) : string;
//Codifica uma string usando apenas sequencias de numeros espacados c/ brancos
function CriptoSpaceEncode( const Str : string ) : string;

implementation

uses SysUtils, Str_Pas;

function CriptoSpaceEncode( const Str : string ) : string;
//----------------------------------------------------------------------------------------------------------------------------------
//Codifica uma string usando apenas sequencias de numeros espacados c/ brancos
var
	l, i : integer;
	Key, car : Byte;
begin
	Result:=EmptyStr;
	l:=Length(Str);
	Key:=Random(255);
	Result:=Result + IntToStr(Byte(Key));
	for i:=1 to l do begin
		car:=Byte(Str[i]);
		asm
			mov al,key
			mov ah,car
			xor al,ah
			mov key,al
		end;
		Result:=Result + ' ' + IntToStr(Byte(Key));
	end;
end;

function CriptoSpaceDecode( Str : string ) : string;
//----------------------------------------------------------------------------------------------------------------------------------
//Decodifica uma string usando apenas sequencias de numeros espacados por brancos
const
	_SEP_	= ' '; //espaco em branco
var
	p : integer;
	Key, Prev : Byte;
	Item : string;
begin
	Result:=EmptyStr;
	p:=Pos(_SEP_, Str);
	if P = 0 then begin
		Exit;
	end;
	Item:=GetDelimitedSubStr(_SEP_, Str, 0);
	try
		Key:=StrToInt( Item );
	except
		on Exception do begin
			Item := EmptyStr; //Assinala retorno da funcao
			Key:=0;
		end;
	end;
	if Item = EmptyStr then begin
		Exit;
	end;
	Prev:=Key;
	Delete(Str,1,p);
	repeat
		Item:=GetDelimitedSubStr(_SEP_, Str, 0);
		Delete(Str,1,Length(Item)+1);
		try
			Key:=StrToInt(Item);
		except
			on Exception do begin
				Item:=EmptyStr;
			end;
		end;
		if Item <> EmptyStr then begin
			Result:=Result + Chr(Byte(Byte(Key) xor Byte(Prev)));
		end;
		Prev:=Key;
	until Str = EmptyStr;
end;


{------------------------------------------------------------------------------}
function ASCIIDec(const Str : string) : string;
var
	CPos: integer;
	CMask, C : byte;
begin
	Result:=Str;
	CMask:=Lo(Length(Str));
	for cPos:=1 to Length(Str) do begin
		C:=byte(Str[cPos]);
		C:=(C - 32)  mod  CMask ;
		Result[CPos]:=Char(C);
		CMask:=C;
	end;
end;
{------------------------------------------------------------------------------}
function ASCIIEnc(const Str : string) : string;
var
	CPos: integer;
	CMask, C : byte;
begin
	Result:=Str;
	CMask:=Lo(Length(Str));
	for cPos:=1 to Length(Str) do begin
		C:=byte(Str[cPos]);
		C:=(C mod CMask) + 32 ;
		Result[CPos]:=Char(C);
		CMask:=C;
	end;
end;


{------------------------------------------------------------------------------}
{Criptografa/Decriptografa um arquivo usando matriz de off-sets}
function CriptoFileByOffSet( const FileIn, FileOut : string;  Cripto : boolean) : boolean;
const
	AValChave : array[1..80] of integer = ( 7, 5, 3, 3, 5, 3, 7, 9, 5,23,11,34, 3,16,15,16,13,
										   58,19,30,21,24,33,24,22,21,77,28,79,30,91,32,43,35,
										   65,36,77,38,35,44,41,52,13,15,16,16,37,18,32,12,11,
										   22,13,15,18,56,17,58,16,60,21,62,23,24,35,24,37,22,
										   39,33,41,35,63,64,37,34,47,32,29,30 );
//................................................................................................................................
var
  fOrigem     : File;
  fDestino    : File;
  iBytesLidos : Integer;
  aBuffer     : Array [ 1..80 ] of Byte;
  aCripto     : Array [ 1..80 ] of char;

procedure Criptografa ;
var  i: Integer;
begin
	if Cripto then begin
		for i:= 1 to 80 do begin
			aCripto[ i ] := Chr( Ord( aBuffer[ i ] ) + aValChave[ i ] );
		end;
	end else begin
		for i:= 1 to 80 do begin
			aCripto[ i ] := Chr( Ord( aBuffer[ i ] ) - aValChave[ i ] );
		end;
	end;
end;
//................................................................................................................................
begin

	 Result := False;
	 {$I-}
	 AssignFile( fOrigem , FileIn ) ;
	 AssignFile( fDestino, FileOut ) ;
	 Reset( fOrigem, 1 ) ;
	 If IOresult <> 0 Then Begin
		Exit;
	 End;
	 ReWrite( fDestino, 1 ) ;
	 If IOresult <> 0 Then Begin
		CloseFile( fOrigem );
		Exit;
	 End;
	 {$I+}

	 BlockRead( fOrigem, aBuffer, SizeOf( aBuffer ), iBytesLidos );
	 while iBytesLidos > 0 Do Begin
		   Criptografa;
		   BlockWrite( fDestino,  aCripto, iBytesLidos );
		   BlockRead( fOrigem, aBuffer, SizeOf( aBuffer ), iBytesLidos );
	 end;
	 CloseFile( fOrigem );
	 CloseFile( fDestino );   { Fecha o Arquivo, atualizando fisicamente as alteracoes }
	 Result := True;

end;

end.
