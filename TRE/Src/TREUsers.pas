{$IFDEF TREUsers}
	 {$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I TRELib.inc}

unit TREUsers;

interface

uses
	SysUtils, Windows, Classes, LmAccess, LmCons, WinNetHnd;

type
	///Indentifica o tipo de conta em relação ao escopo de utilização
	/// invalido - indefinido
	/// usZone - titulo ordinário(exclui contas de suporte 10191, por exemplo)
   /// usSupport - contas do sis ou privativas do suporte TRE-PB(vncacesso por exemplo)
	 TUserScope = (usInvalid, usZone, usSupport);

	 TTREZEUser = class
	 private
		 FUserName : string;
		 FPassword : string;
		 FChecked :  boolean;
		 FScope :    TUserScope;
	 public
		 constructor Create(const AName, APassword : string); virtual;
		 /// <summary>
		 ///  traduz a senha para a forma calculada
		 /// </summary>
		 /// <param name="Zone">Identificador da zona</param>
		 /// <returns>senha calculada deste usuário</returns>
		 function TranslatedPwd(Zone : Integer) : string;
		 property UserName : string read FUserName;
		 property Password : string read FPassword write FPassword;
		 property Checked : boolean read FChecked write FChecked;
		 property Scope : TUserScope read FScope write FScope;
		 /// <summary>
		 /// Troca a senha da conta deste usuário de zona no computador local
		 /// </summary>
		 /// <returns>Código de erro da operação</returns>
		 function SetPassword(Zone : Integer) : NET_API_STATUS;
	 end;

implementation

uses
  Str_pas, StrHnd, TREUtils, WinReg32, JclWin32;


constructor TTREZEUser.Create(const AName, APassword : string);
begin
    Self.FUserName := AName;
    Self.FPassword := APassword;
    Self.FScope    := usInvalid;
end;

function TTREZEUser.SetPassword(Zone : Integer) : NET_API_STATUS;
var
    userInfo :   TUserInfo1003;
    PError :     DWORD;
    PDomain, PUsername : PWideChar;
    tokenIndex : Integer;
    ErrorlogString : string;
begin
    tokenIndex := Pos('\', Self.FUserName);
    if tokenIndex <> 0 then begin //conta de dominio
        PDomain   := StrNew(PWideChar(Copy(Self.UserName, 1, tokenIndex - 1)));
        PUsername := StrNew(PWideChar(Copy(Self.FUserName, tokenIndex + 1, Length(Self.UserName))));
    end else begin
		 PDomain   := nil;
		 PUsername := StrNew(PWideChar(Self.FUserName));
	 end;
	 userInfo.usri1003_password := StrNew(PWideChar(Self.TranslatedPwd(Zone)));
	 try
		 Result := NetUserSetInfo(PDomain, PUserName, 1003, PByte(@userInfo), @PError);
	 finally
		 StrDispose(PUsername);
		 StrDispose(PDomain);
		 StrDispose(userInfo.usri1003_password);
	 end;
end;

function TTREZEUser.TranslatedPwd(Zone : Integer) : string;
var
	 chain : string;
	 zv :    array[1..3] of char;
begin
	 chain  := Format('%3.3d', [Zone]);
	 zv[1]  := Chr(Ord('i') + StrToInt(Copy(chain, 1, 1)));
    zv[2]  := Chr(Ord('a') + StrToInt(Copy(chain, 2, 1)));
    zv[3]  := Chr(Ord('o') + StrToInt(Copy(chain, 3, 1)));
    Result := Self.Password;
    Result := Str_Pas.ReplaceSubString(Result, '<1>', zv[1]);
    Result := Str_Pas.ReplaceSubString(Result, '<2>', zv[2]);
    Result := Str_Pas.ReplaceSubString(Result, '<3>', zv[3]);
end;

end.
