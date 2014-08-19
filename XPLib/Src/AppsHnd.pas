{$IFDEF AppsHnd}
	{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I XPLib.inc}

unit AppsHnd;

interface

uses
	Windows, Messages;

//Verifica se o Depurador do IDE esta carregado
function  DebbugerIDELoaded() : boolean;{$IF CompilerVersion >= 21.00}platform;{$IFEND}


implementation

function  DebbugerIDELoaded() : boolean;{$IF CompilerVersion >= 21.00}platform;{$IFEND}

//----------------------------------------------------------------------------------------------------------------------------------
//Verifica se o Depurador do IDE esta carregado
begin
	Result:=( System.DebugHook <> 0 );
end;

end.
