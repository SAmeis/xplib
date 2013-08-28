{$IFDEF WinLSAUtils}
	{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I WinAdvLib.inc}

unit WinLSAUtils;

interface

uses
	 Windows, StdCtrls, JwaWindows;


function LogonAsServiceToAccount(AAccountName : string) : DWORD;
function AddPrivilegeToAccount(AAccountName, APrivilege : string) : DWORD;

implementation


function AddPrivilegeToAccount(AAccountName, APrivilege : string) : DWORD;
var
    lStatus : TNTStatus;
    lObjectAttributes : TLsaObjectAttributes;
    lPolicyHandle : TLsaHandle;
    lPrivilege : TLsaUnicodeString;
    lSid :    PSID;
    lSidLen : DWORD;
    lTmpDomain : string;
    lTmpDomainLen : DWORD;
    lTmpSidNameUse : TSidNameUse;
{$IFDEF UNICODE}
    lPrivilegeWStr : string;
{$ELSE}
    lPrivilegeWStr : WideString;
{$ENDIF}
begin
    ZeroMemory(@lObjectAttributes, SizeOf(lObjectAttributes));
    lStatus := LsaOpenPolicy(nil, lObjectAttributes, POLICY_LOOKUP_NAMES, lPolicyHandle);

    if lStatus <> STATUS_SUCCESS then begin
        Result := LsaNtStatusToWinError(lStatus);
        Exit;
    end;

    try
        lTmpDomainLen := JwaWindows.DNLEN; // In 'clear code' this should be get by LookupAccountName
        //lTmpDomainLen := JwaLmCons.DNLEN; // In 'clear code' this should be get by LookupAccountName
        SetLength(lTmpDomain, lTmpDomainLen);

        lSidLen := SECURITY_MAX_SID_SIZE;
        GetMem(lSid, lSidLen);
        try
            if LookupAccountName(nil, PChar(AAccountName), lSid, lSidLen, PChar(lTmpDomain),
                lTmpDomainLen, lTmpSidNameUse) then begin
                lPrivilegeWStr := APrivilege;

                lPrivilege.Buffer := PWideChar(lPrivilegeWStr);
                lPrivilege.Length := Length(lPrivilegeWStr) * SizeOf(char);
                lPrivilege.MaximumLength := lPrivilege.Length;

                lStatus := LsaAddAccountRights(lPolicyHandle, lSid, @lPrivilege, 1);
                Result  := LsaNtStatusToWinError(lStatus);
            end else begin
                Result := GetLastError;
            end;
		 finally
			 FreeMem(lSid);
		 end;
	 finally
		 LsaClose(lPolicyHandle);
	 end;
end;

function LogonAsServiceToAccount(AAccountName : string) : DWORD;
begin
	 Result := AddPrivilegeToAccount(AAccountName {or any account/group name}, SE_SERVICE_LOGON_NAME);
end;

end.
