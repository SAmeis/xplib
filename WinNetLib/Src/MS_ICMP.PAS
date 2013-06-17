{$IFDEF DOT_NET}
{$WARN UNSAFE_TYPE ON}
{$WARN UNSAFE_CODE ON}
{$WARN UNSAFE_CAST ON}
{$ELSE}
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}
{$ENDIF}
{$IFDEF MS_ICMP}
{$A+,B-,C-,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O-,P+,Q-,R+,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$DEBUGINFO ON}
{$ELSE}
{$A+,B-,C-,D-,E-,F-,G+,H+,I+,J+,K-,L-,M-,N+,O+,P+,Q+,R+,S-,T-,U-,V+,W-,X+,Y-,Z4}
{$F+}
{$ENDIF}

{HISTORICO:

Alterado o nome da unit para MS_ICMP e alocada a mesma no pacote WinNetLib
Roger 2003-05-30

Alterado TICMP.ResolveAddr; de modo a invalidar o valor do endereco IP para o host nao resolvido
Roger 2003-05-29


}


{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  This unit encapsulate the ICMP.DLL into an object of type TICMP.
              Using this object, you can easily ping any host on your network.
              Works only in 32 bits mode (no Delphi 1) under NT or 95.
              TICMP is perfect for a console mode program, but if you build a
              GUI program, you could use the TPing object wich is a true VCL
              encapsulating the TICMP object. Then you can use object inspector
              to change properties or event handler. This is much simpler to
              use for a GUI program.
EMail:        http://users.swing.be/francois.piette  francois.piette@swing.be
              http://www.rtfm.be/fpiette             francois.piette@rtfm.be
              francois.piette@pophost.eunet.be
Creation:     January 6, 1997
Version:      1.04
Support:      Use the mailing list twsocket@rtfm.be See website for details.
Legal issues: Copyright (C) 1997-2000 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
              <francois.piette@pophost.eunet.be>

              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                  you must not claim that you wrote the original software.
                  If you use this software in a product, an acknowledgment
                  in the product documentation would be appreciated but is
                  not required.

               2. Altered source versions must be plainly marked as such, and
                  must not be misrepresented as being the original software.

               3. This notice may not be removed or altered from any source
                  distribution.

               4. You must register this software by sending a picture postcard
                  to the author. Use a nice stamp and mention your name, street
                  address, EMail address and any comment you like to say.

Updates:
Dec 13, 1997 V1.01 Added OnEchoRequest and OnEchoReply events and removed the
              corresponding OnDisplay event. This require to modify existing
              programs.
Mar 15, 1998 V1.02 Deplaced address resolution just before use
Sep 24, 1998 V1.93 Changed TIPAddr and others to LongInt to avoid range error
              problems with Delphi 4
Jan 24, 1999 V1.11 Surfaced Flags property to allow fragmentation check
              (Flags = IP_FLAG_DF to enable fragmentation check)


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit MS_ICMP;

interface

{$IFDEF VER80}
// This source file is *NOT* compatible with Delphi 1 because it uses
// Win 32 features.
{$ENDIF}

uses
    Windows, SysUtils, Classes, WinSock;

const
    IcmpVersion = 102;
    IcmpDLL     = 'icmp.dll';

    // IP status codes returned to transports and user IOCTLs.
    IP_SUCCESS         = 0;
    IP_STATUS_BASE     = 11000;
    IP_BUF_TOO_SMALL   = (IP_STATUS_BASE + 1);
    IP_DEST_NET_UNREACHABLE = (IP_STATUS_BASE + 2);
    IP_DEST_HOST_UNREACHABLE = (IP_STATUS_BASE + 3);
    IP_DEST_PROT_UNREACHABLE = (IP_STATUS_BASE + 4);
    IP_DEST_PORT_UNREACHABLE = (IP_STATUS_BASE + 5);
    IP_NO_RESOURCES    = (IP_STATUS_BASE + 6);
    IP_BAD_OPTION      = (IP_STATUS_BASE + 7);
    IP_HW_ERROR        = (IP_STATUS_BASE + 8);
    IP_PACKET_TOO_BIG  = (IP_STATUS_BASE + 9);
    IP_REQ_TIMED_OUT   = (IP_STATUS_BASE + 10);
    IP_BAD_REQ         = (IP_STATUS_BASE + 11);
    IP_BAD_ROUTE       = (IP_STATUS_BASE + 12);
    IP_TTL_EXPIRED_TRANSIT = (IP_STATUS_BASE + 13);
    IP_TTL_EXPIRED_REASSEM = (IP_STATUS_BASE + 14);
    IP_PARAM_PROBLEM   = (IP_STATUS_BASE + 15);
    IP_SOURCE_QUENCH   = (IP_STATUS_BASE + 16);
    IP_OPTION_TOO_BIG  = (IP_STATUS_BASE + 17);
    IP_BAD_DESTINATION = (IP_STATUS_BASE + 18);

    // status codes passed up on status indications.
    IP_ADDR_DELETED    = (IP_STATUS_BASE + 19);
    IP_SPEC_MTU_CHANGE = (IP_STATUS_BASE + 20);
    IP_MTU_CHANGE      = (IP_STATUS_BASE + 21);

    IP_GENERAL_FAILURE = (IP_STATUS_BASE + 50);

    MAX_IP_STATUS = IP_GENERAL_FAILURE;

    IP_PENDING = (IP_STATUS_BASE + 255);

    // IP header flags
    IP_FLAG_DF = $02;         // Don't fragment this packet.

    // IP Option Types
    IP_OPT_EOL      = $00;         // End of list option
    IP_OPT_NOP      = $01;         // No operation
    IP_OPT_SECURITY = $82;         // Security option.
    IP_OPT_LSRR     = $83;         // Loose source route.
    IP_OPT_SSRR     = $89;         // Strict source route.
    IP_OPT_RR       = $07;         // Record route.
    IP_OPT_TS       = $44;         // Timestamp.
    IP_OPT_SID      = $88;         // Stream ID (obsolete)
    MAX_OPT_SIZE    = $40;

type
    // IP types
    TIPAddr   = longint;   // An IP address.
    TIPMask   = longint;   // An IP subnet mask.
    TIPStatus = longint;   // Status code returned from IP APIs.

    PIPOptionInformation = ^TIPOptionInformation;

    TIPOptionInformation = packed record
        TTL:         byte;      // Time To Live (used for traceroute)
        TOS:         byte;      // Type Of Service (usually 0)
        Flags:       byte;      // IP header flags (usually 0)
        OptionsSize: byte;      // Size of options data (usually 0, max 40)
        OptionsData: PChar;     // Options data buffer
    end;

    PIcmpEchoReply = ^TIcmpEchoReply;

    TIcmpEchoReply = packed record
        Address:  TIPAddr;              // Replying address
        Status:   DWord;                // IP status value
        RTT:      DWord;                // Round Trip Time in milliseconds
        DataSize: Word;                 // Reply data size
        Reserved: Word;                 // Reserved
        Data:     Pointer;              // Pointer to reply data buffer
        Options:  TIPOptionInformation; // Reply options
    end;

    // IcmpCreateFile:
    //     Opens a handle on which ICMP Echo Requests can be issued.
    // Arguments:
    //     None.
    // Return Value:
    //     An open file handle or INVALID_HANDLE_VALUE. Extended error information
    //     is available by calling GetLastError().
    TIcmpCreateFile = function : THandle; stdcall;

    // IcmpCloseHandle:
    //     Closes a handle opened by ICMPOpenFile.
    // Arguments:
    //     IcmpHandle  - The handle to close.
    // Return Value:
    //     TRUE if the handle was closed successfully, otherwise FALSE. Extended
    //     error information is available by calling GetLastError().
    TIcmpCloseHandle = function(IcmpHandle : THandle) : boolean; stdcall;

    // IcmpSendEcho:
    //     Sends an ICMP Echo request and returns one or more replies. The
    //     call returns when the timeout has expired or the reply buffer
    //     is filled.
    // Arguments:
    //     IcmpHandle         - An open handle returned by ICMPCreateFile.
    //     DestinationAddress - The destination of the echo request.
    //     RequestData        - A buffer containing the data to send in the
    //                          request.
    //     RequestSize        - The number of bytes in the request data buffer.
    //     RequestOptions     - Pointer to the IP header options for the request.
    //                          May be NULL.
    //     ReplyBuffer        - A buffer to hold any replies to the request.
    //                          On return, the buffer will contain an array of
    //                          ICMP_ECHO_REPLY structures followed by options
    //                          and data. The buffer should be large enough to
    //                          hold at least one ICMP_ECHO_REPLY structure
    //                          and 8 bytes of data - this is the size of
    //                          an ICMP error message.
    //     ReplySize          - The size in bytes of the reply buffer.
    //     Timeout            - The time in milliseconds to wait for replies.
    // Return Value:
    //     Returns the number of replies received and stored in ReplyBuffer. If
    //     the return value is zero, extended error information is available
    //     via GetLastError().
    TIcmpSendEcho = function(IcmpHandle : THandle;
        DestinationAddress : TIPAddr;
        RequestData : Pointer;
        RequestSize : Word;
        RequestOptions : PIPOptionInformation;
        ReplyBuffer : Pointer;
        ReplySize : DWord;
        Timeout : DWord
        ) : DWord; stdcall;

    // Event handler type declaration for TICMP.OnDisplay event.
    TICMPDisplay = procedure(Sender : TObject; Msg : string) of object;
    TICMPReply = procedure(Sender : TObject; Error : Integer) of object;

    // The object wich encapsulate the ICMP.DLL
    TICMP = class(TObject)
    private
        hICMPdll :       HModule;                    // Handle for ICMP.DLL
        IcmpCreateFile : TIcmpCreateFile;
        IcmpCloseHandle : TIcmpCloseHandle;
        IcmpSendEcho :   TIcmpSendEcho;
        hICMP :          THandle;                    // Handle for the ICMP Calls
        FReply :         TIcmpEchoReply;             // ICMP Echo reply buffer
        FAddress :       string;                     // Address given
        FHostName :      string;                     // Dotted IP of host (output)
        FHostIP :        string;                     // Name of host      (Output)
        FIPAddress :     TIPAddr;                    // Address of host to contact
        FSize :          Integer;                    // Packet size (default to 56)
        FTimeOut :       Integer;                    // Timeout (default to 4000mS)
        FTTL :           Integer;                    // Time To Live (for send)
        FFlags :         Integer;                    // Options flags
        FOnDisplay :     TICMPDisplay;               // Event handler to display
        FOnEchoRequest : TNotifyEvent;
        FOnEchoReply :   TICMPReply;
        FLastError :     DWORD;                      // After sending ICMP packet
        FAddrResolved :  boolean;
        procedure ResolveAddr;
    public
        constructor Create; virtual;
        destructor Destroy; override;
        function Ping : Integer;
        procedure SetAddress(Value : string);
        function GetErrorString : string;

        property Address : string read FAddress write SetAddress;
        property Size : Integer read FSize write FSize;
        property Timeout : Integer read FTimeout write FTimeout;
        property Reply : TIcmpEchoReply read FReply;
        property TTL : Integer read FTTL write FTTL;
        property Flags : Integer read FFlags write FFlags;
        property ErrorCode : DWORD read FLastError;
        property ErrorString : string read GetErrorString;
        property HostName : string read FHostName;
        property HostIP : string read FHostIP;
        property OnDisplay : TICMPDisplay read FOnDisplay write FOnDisplay;
        property OnEchoRequest : TNotifyEvent read FOnEchoRequest
            write FOnEchoRequest;
        property OnEchoReply : TICMPReply read FOnEchoReply
            write FOnEchoReply;
    end;

    TICMPException = class(Exception);

implementation

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TICMP.Create;
var
    WSAData : TWSAData;
begin
    hICMP    := INVALID_HANDLE_VALUE;
    FSize    := 56;
    FTTL     := 64;
    FTimeOut := 4000;

    // initialise winsock
    if WSAStartup($101, WSAData) <> 0 then begin
        raise TICMPException.Create('Error initialising Winsock');
    end;

    // register the icmp.dll stuff
    hICMPdll := LoadLibrary(icmpDLL);
    if hICMPdll = 0 then begin
        raise TICMPException.Create('Unable to register ' + icmpDLL);
    end;

    @ICMPCreateFile  := GetProcAddress(hICMPdll, 'IcmpCreateFile');
    @IcmpCloseHandle := GetProcAddress(hICMPdll, 'IcmpCloseHandle');
    @IcmpSendEcho    := GetProcAddress(hICMPdll, 'IcmpSendEcho');

    if (@ICMPCreateFile = nil) or
        (@IcmpCloseHandle = nil) or
        (@IcmpSendEcho = nil) then begin
        raise TICMPException.Create('Error loading dll functions');
    end;

    hICMP := IcmpCreateFile;
    if hICMP = INVALID_HANDLE_VALUE then begin
        raise TICMPException.Create('Unable to get ping handle');
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TICMP.Destroy;
begin
    if hICMP <> INVALID_HANDLE_VALUE then begin
        IcmpCloseHandle(hICMP);
    end;
    if hICMPdll <> 0 then begin
        FreeLibrary(hICMPdll);
    end;
    WSACleanup;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function MinInteger(X, Y : Integer) : Integer;
begin
    if X >= Y then begin
        Result := Y;
    end else begin
        Result := X;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TICMP.ResolveAddr;
var
    Phe : PHostEnt;             // HostEntry buffer for name lookup
begin
    // Convert host address to IP address
    FIPAddress := inet_addr(PAnsiChar(Self.FAddress));
    if FIPAddress <> longint(INADDR_NONE) then // Was a numeric dotted address let it in this format
    begin
        FHostName := FAddress;
    end else begin
        // Not a numeric dotted address, try to resolve by name
        Phe := GetHostByName(PAnsiChar(Self.FAddress));
        if Phe = nil then begin
            FHostIP    := EmptyStr; //****NOTA: Se endereco nao foi resolvido seu valor agora serah nulo
            FLastError := GetLastError;
            if Assigned(FOnDisplay) then begin
                FOnDisplay(Self, 'Unable to resolve ' + FAddress);
            end;
            Exit;
        end;

        FIPAddress := longint(plongint(Phe^.h_addr_list^)^);
        FHostName  := Phe^.h_name;
    end;

    FHostIP := StrPas(inet_ntoa(TInAddr(FIPAddress)));
    FAddrResolved := True;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TICMP.SetAddress(Value : string);
begin
    // Only change if needed (could take a long time)
    if FAddress = Value then begin
        Exit;
    end;
    FAddress      := Value;
    FAddrResolved := False;
    //    ResolveAddr;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TICMP.GetErrorString : string;
begin
    case FLastError of
        IP_SUCCESS : begin
            Result := 'No error';
        end;
        IP_BUF_TOO_SMALL : begin
            Result := 'Buffer too small';
        end;
        IP_DEST_NET_UNREACHABLE : begin
            Result := 'Destination network unreachable';
        end;
        IP_DEST_HOST_UNREACHABLE : begin
            Result := 'Destination host unreachable';
        end;
        IP_DEST_PROT_UNREACHABLE : begin
            Result := 'Destination protocol unreachable';
        end;
        IP_DEST_PORT_UNREACHABLE : begin
            Result := 'Destination port unreachable';
        end;
        IP_NO_RESOURCES : begin
            Result := 'No resources';
        end;
        IP_BAD_OPTION : begin
            Result := 'Bad option';
        end;
        IP_HW_ERROR : begin
            Result := 'Hardware error';
        end;
        IP_PACKET_TOO_BIG : begin
            Result := 'Packet too big';
        end;
        IP_REQ_TIMED_OUT : begin
            Result := 'Request timed out';
        end;
        IP_BAD_REQ : begin
            Result := 'Bad request';
        end;
        IP_BAD_ROUTE : begin
            Result := 'Bad route';
        end;
        IP_TTL_EXPIRED_TRANSIT : begin
            Result := 'TTL expired in transit';
        end;
        IP_TTL_EXPIRED_REASSEM : begin
            Result := 'TTL expired in reassembly';
        end;
        IP_PARAM_PROBLEM : begin
            Result := 'Parameter problem';
        end;
        IP_SOURCE_QUENCH : begin
            Result := 'Source quench';
        end;
        IP_OPTION_TOO_BIG : begin
            Result := 'Option too big';
        end;
        IP_BAD_DESTINATION : begin
            Result := 'Bad Destination';
        end;
        IP_ADDR_DELETED : begin
            Result := 'Address deleted';
        end;
        IP_SPEC_MTU_CHANGE : begin
            Result := 'Spec MTU change';
        end;
        IP_MTU_CHANGE : begin
            Result := 'MTU change';
        end;
        IP_GENERAL_FAILURE : begin
            Result := 'General failure';
        end;
        IP_PENDING : begin
            Result := 'Pending';
        end;
        else begin
            Result := 'ICMP error #' + IntToStr(FLastError);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TICMP.Ping : Integer;
var
    BufferSize : Integer;
    pReqData, pData : Pointer;
    pIPE :  PIcmpEchoReply;       // ICMP Echo reply buffer
    IPOpt : TIPOptionInformation; // IP Options for packet to send
    Msg :   string;
begin
    Result     := 0;
    FLastError := 0;

    if not FAddrResolved then begin
        ResolveAddr;
    end;

    if FIPAddress = longint(INADDR_NONE) then begin
        FLastError := IP_BAD_DESTINATION;
        if Assigned(FOnDisplay) then begin
            FOnDisplay(Self, 'Invalid host address');
        end;
        Exit;
    end;

    // Allocate space for data buffer space
    BufferSize := SizeOf(TICMPEchoReply) + FSize;
    GetMem(pReqData, FSize);
    GetMem(pData, FSize);
    GetMem(pIPE, BufferSize);

    try
        // Fill data buffer with some data bytes
        FillChar(pReqData^, FSize, $20);
        Msg := 'Pinging from Delphi code written by F. Piette';
        Move(Msg[1], pReqData^, MinInteger(FSize, Length(Msg)));

        pIPE^.Data := pData;
        FillChar(pIPE^, SizeOf(pIPE^), 0);

        if Assigned(FOnEchoRequest) then begin
            FOnEchoRequest(Self);
        end;

        FillChar(IPOpt, SizeOf(IPOpt), 0);
        IPOpt.TTL   := FTTL;
        IPOpt.Flags := FFlags;
        Result      := IcmpSendEcho(hICMP, FIPAddress, pReqData, FSize,
            @IPOpt, pIPE, BufferSize, FTimeOut);
        FLastError  := GetLastError;
        FReply      := pIPE^;

        if Assigned(FOnEchoReply) then begin
            FOnEchoReply(Self, Result);
        end;
    finally
        // Free those buffers
        FreeMem(pIPE);
        FreeMem(pData);
        FreeMem(pReqData);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
