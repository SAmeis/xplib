{$IFDEF WNetExHnd}
{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I WinNetLib.inc}
{
  *****************************************************************************************************
  Unit para agrupar rotinas de gerenciamento avançado de rede
  *****************************************************************************************************

  Revision - 20110209 - roger
  A partir daqui se importa a Jedi Windows API devido a melhor compatibilidade futura

}

unit WNetExHnd;

interface

uses
	Windows, SysUtils, LmErr, LmApibuf, LmRemUtl, StrHnd;

function GetDomainFromComputerName(const ComputerName: string): string;
//Retorna o nome do domínio da estação passada, usando-se nil a em execução
function GetWorkstationLocalGroup(const WksName: string): string;
//Pega a data/hora de um computador remoto 0 -> sucesso
function GetRemoteDateTime(const UNCName: string; var RemoteTime: TDateTime): Integer;
//Envia mensagem popup para a rede
function SendNetMessage(const Targ, Msg: string; const SenderIdentification: string = ''; const FromComputer: string = '';
	const ServerSender: string = ''): DWORD;

function SendNetMessageWin32(const ATargetIdentification, AMsg: string; const ASenderIdentification: string = ''): DWORD;
function SendNetMessageWin9x(const TargetIdentification, Msg: string; const SenderIdentification: string = '';
	const TargetUser: string = ''): DWORD;

implementation

uses
	Str_Null, DateOper, Dialogs, LMMsg, WinNetHnd, jwaWindows;

function GetDomainFromComputerName(const ComputerName: string): string;
///
///Retorna o dominio ao qual o computador pertence
///NOTAS: Internamente a API exige que algum controlador de domínio esteja acessivel
///'.' Representa o computador local e tem como retorno 'BUILTIN'
var
	Count1, Count2: DWORD;
	Sd            : PSID; //PSecurityDescriptor; // FPC requires PSID
	Snu           : SID_Name_Use;
	Name          : string;
begin
	if ComputerName = EmptyStr then begin
		name := WinNetHnd.GetComputerName();
	end else begin
		name := ComputerName;
	end;
	if (name <> '.') and (not TStrHnd.endsWith(name, '$')) then begin
		name := name + '$';
	end;
	Count1 := 0;
	Count2 := 0;
	Sd     := nil;
	Snu    := SIDTypeUser;
	Result := '';
	LookUpAccountName(nil, PChar(name), Sd, Count1, PChar(Result), Count2, Snu);
	//set buffer size to Count2 + 2 characters for safety
	SetLength(Result, Count2 + 1);
	Sd := System.AllocMem(Count1);
	try
		if LookUpAccountName(nil, PChar(name), Sd, Count1, PChar(Result), Count2, Snu) then begin
			TStrHnd.ResetLength(Result);
		end else begin
			Result := EmptyStr;
		end;
	finally
		FreeMemory(Sd);
	end;
end;

function GetWorkstationLocalGroup(const WksName: string): string;
///Retorna o nome do domínio ou grupo de trabalho da estação passada, passando vazio usa-se a estaçao em execução
///Esta rotina não diferencia domínio de grupo de trabalho e isso pode influenciar na lógica do programa chamador
var
	Info          : jwaWindows.PWKSTA_INFO_100;
	NetworkAddress: string;
begin
	if WksName = EmptyStr then begin
		NetworkAddress := WinNetHnd.GetComputerName();
	end else begin
		NetworkAddress := WksName;
	end;
	if jwaWindows.NetWkstaGetInfo(PChar(NetworkAddress), 100, PByte(Info)) = NERR_Success then begin
		try
			if (Info.wki100_computername <> nil) and (Info.wki100_langroup <> nil) then begin
				Result := Info.wki100_langroup;
			end;
		finally
			NetApiBufferFree(Info);
		end;
	end;
end;

function GetRemoteDateTime(const UNCName: string; var RemoteTime: TDateTime): Integer;
///----------------------------------------------------------------------------------------------------------------------
///
///Alterações nas diretivas de compilação geraram a remoção dos ponteiros sem tipo 13 linhas abaixo
///Revision - 20110209 - roger
///
///----------------------------------------------------------------------------------------------------------------------
var
	TimeInfo : PTimeOfDayInfo;
	GMTTime  : TSystemTime;
	UCharName: array[0 .. 255] of widechar;
begin
	{$WARN UNSAFE_CODE OFF}
	if not LoadLmRemUtl() then begin
		Result := ERROR_CALL_NOT_IMPLEMENTED;
	end else begin
		//Alocar o buffer e passa-lo para a rotina de hora remota
		StringToWideChar(UNCName, UCharName, SizeOf(UCharName));
		SetLastError(ERROR_SUCCESS);
		Result := LmRemUtl.NetRemoteTOD(UCharName, PByte(@TimeInfo));
		try
			if Result = NERR_Success then begin
				FillChar(GMTTime, SizeOf(GMTTime), 0);
				GMTTime.wYear         := TimeInfo^.tod_year;
				GMTTime.wMonth        := TimeInfo^.tod_month;
				GMTTime.wDayOfWeek    := TimeInfo^.tod_weekday;
				GMTTime.wDay          := TimeInfo^.tod_day;
				GMTTime.wHour         := TimeInfo^.tod_hours;
				GMTTime.wMinute       := TimeInfo^.tod_mins;
				GMTTime.wSecond       := TimeInfo^.tod_secs;
				GMTTime.wMilliseconds := TimeInfo^.tod_hunds;
				RemoteTime            := SystemTimeToDateTime(GMTTime);
				RemoteTime            := RemoteTime - ((GetLocalBiasUTC() div 60) / 24); //Ajuste TimeZone
			end;
		finally
			if TimeInfo <> nil then begin
				NetApiBufferFree(TimeInfo);
			end;
		end;
	end;
	{$WARN UNSAFE_CODE ON}
end;

function SendNetMessage(const Targ, Msg: string; const SenderIdentification: string = ''; const FromComputer: string = '';
	const ServerSender: string = ''): DWORD;
//----------------------------------------------------------------------------------------------------------------------
begin
	if SysUtils.Win32Platform = VER_PLATFORM_WIN32_NT then begin
		Result := SendNetMessageWin32(Targ, Msg, SenderIdentification);
	end else begin
		Result := SendNetMessageWin9x(Targ, Msg, SenderIdentification);
	end;
end;

function SendNetMessageWin32(const ATargetIdentification, AMsg: string; const ASenderIdentification: string = ''): DWORD;
//----------------------------------------------------------------------------------------------------------------------
//ATargetIdentification Nome da maquina/usuario de destino, pelo momento aceita apenas nome da maquina
//AMsg : Mensagem a ser enviada
//ASenderIdentification Nome do computador ou nome do usuario emissor da mensagem: EmptyStr -> nome do computador local
// **** Ver notas abaixo lidas de http://groups.yahoo.com/group/realpopupdisc/message/488
{
  >> the NetMessageBufferSend() is a win32 function which "sends a buffer
  >> of information to a registered message alias". it works on NT-based
  >> platforms, such as win2k and xp. on these two, if you're connected to
  >> a DC, then it works if ACL permits that (admins and account operators
  >> by default, but you may change it if you want).
  >
  >Now with that sample code we're able to progress: I have written a
  >test program that confirms that on XP (and presumably on W2K too)
  >NetMessageBufferSend() fails with error 2273 ("The message alias
  >could not be found on the network.") if the messenger service is not
  >running. This means that as soon as "take over" is set in RP (and
  >thus you have to stop the messenger service) sending with that API,
  >including using NET SEND, will FAIL. IIRC this behavior is different
  >from NT4's where NET SEND was working EVEN with RP's "take over"
  >option set and the messenger service stopped.
  void enum_names( const wchar_t *server )
  ***** TENTAR IMPLEMENTAR O METODO ENCONTRADO NA DOCUMENTACAO DESTE PACOTE
}
var
	PSenderIdentification, PTargetIdentification, PSenderAgentName: LPCWSTR;
	PMessage                                                      : PByte;
	L                                                             : Integer;
begin
	//Resolve o conteudo da mensagem
	L        := (Length(AMsg) * 2 + 2); //\0 com 2 bytes NO FINAL da cadeia
	PMessage := GetMemory(L);
	StringToWideChar(AMsg, PWideChar(PMessage), L);

	//Resolve maquina onde estah o destinatario
	{ TODO -oRoger -cLIB : No futuro testar se a trata-se de uma maquina ou usuario }
	PTargetIdentification := GetMemory((Length(ATargetIdentification) * 2) + 1);
	StringToWideChar(ATargetIdentification, PTargetIdentification, Length(ATargetIdentification) * 2);

	//Resolver nome da maquina que irah repassar a mensagem -> Usar computador local para isso sempre nesta rotina
	PSenderAgentName := nil;

	//Resolve Emissor
	if (ASenderIdentification <> EmptyStr) then begin
		//Ajusta para emitir a assinatura do usuario. ***Lembrar das resticoes de seguranca
		PSenderIdentification := GetMemory((Length(ASenderIdentification) * 2) + 1);
		StringToWideChar(ASenderIdentification, PSenderIdentification, Length(ASenderIdentification) * 2);
	end else begin
		PSenderIdentification := nil;
	end;

	try
		Result := NetMessageBufferSend(PSenderAgentName, PTargetIdentification, PSenderIdentification, PMessage, L);
		if Result <> ERROR_SUCCESS then begin
			//Segundo dica da internet se esta maquina nao for capaz de enviar por si mesma -> usar mailslots win9x
			Result := SendNetMessageWin9x(ATargetIdentification, AMsg, ASenderIdentification);
		end;
	finally
		FreeMemory(PTargetIdentification);
		FreeMemory(PMessage);
		if Assigned(PSenderIdentification) then begin
			FreeMemory(PSenderIdentification);
		end;
	end;
end;

function SendNetMessageWin9x(const TargetIdentification, Msg: string; const SenderIdentification: string = '';
	const TargetUser: string = ''): DWORD;
///----------------------------------------------------------------------------------------------------------------------
///ASenderIdentification Nome do computador ou nome do usuario emissor da mensagem: EmptyStr -> nome do computador local
///ATargetIdentification Nome da maquina/usuario de destino, pelo momento aceita apenas nome da maquina
///AMsg : Mensagem a ser enviada
///TargetUser : Nome do usuario de destino
///
///Revision - 20110209 - roger
///Alterado para suporte ao uso da jedi Windows API
///
var
	OTargetIdentification, OSenderIdentification, OTargetUser, OMsg: PChar;
	SlotName, BufferStr                                            : string;
	SlotHandle                                                     : THandle;
	Data                                                           : Pointer;
	Required, BytesWritten                                         : DWORD;
begin
	{ TODO -oRoger -cLIB : Adpatar esta rotina para suplantar a SendNetMessage da API NetApi nao suportada para Win9x }
	{ TODO -oRoger -cLIB : Testar se a trata-se de uma maquina ou usuario }
	OTargetIdentification := StrConvertToOEM(PChar(TargetIdentification));
	try
		SlotName   := '\\' + OTargetIdentification + '\mailslot\messngr'; //MailSlot string
		SlotHandle := CreateFile(PChar(SlotName), GENERIC_WRITE, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
		if SlotHandle <> INVALID_HANDLE_VALUE then begin
			try
				if (SenderIdentification <> EmptyStr) then begin
					OSenderIdentification := StrConvertToOEM(PChar(SenderIdentification));
				end else begin
					OSenderIdentification := StrConvertToOEM(PChar(WinNetHnd.GetComputerName()));
				end;
				if (TargetUser <> EmptyStr) then begin
					OTargetUser := StrConvertToOEM(PChar(TargetUser));
				end else begin
					OTargetUser := StrConvertToOEM(PChar(TargetIdentification));
				end;
				OMsg := StrConvertToOEM(PChar(Msg));
				try
					BufferStr    := OSenderIdentification + #0 + OTargetUser + #0 + OMsg + #0;
					Required     := Length(BufferStr);
					Data         := PChar(BufferStr);
					BytesWritten := 0;
					{$WARN UNSAFE_CODE OFF}
					if (not WriteFile(SlotHandle, Data, Required, PDWORD(@BytesWritten), nil)) or (BytesWritten <> Required) then
					begin
						Result := GetLastError();
					end else begin
						Result := ERROR_SUCCESS;
					end;
					{$WARN UNSAFE_CODE ON}
				finally
					StrDispose(OSenderIdentification);
					StrDispose(OTargetUser);
					StrDispose(OMsg);
				end;
			finally
				if not CloseHandle(SlotHandle) then begin
					Result := GetLastError();
				end;
			end;
		end else begin
			Result := GetLastError(); //Nao conseguiu abrir o slot
		end;
	finally
		StrDispose(OTargetIdentification);
	end;
end;

initialization

//----------------------------------------------------------------------------------------------------------------------
begin
	LmApibuf.LoadLmApibuf();
	//usado para teste em caso de erro     @LmApiBuf.NetApiBufferFree:=nil;
end;

end.
