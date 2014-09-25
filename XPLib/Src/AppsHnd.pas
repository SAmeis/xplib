{$IFDEF AppsHnd}
{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I XPLib.inc}
unit AppsHnd;

interface

uses
  Windows, Messages;

// Verifica se o Depurador do IDE esta carregado
function DebbugerIDELoaded(): boolean;

implementation

function DebbugerIDELoaded(): boolean;
// ----------------------------------------------------------------------------------------------------------------------------------
// Verifica se o Depurador do IDE esta carregado
begin
  {$WARN SYMBOL_PLATFORM OFF}
  Result := (System.DebugHook <> 0);
  {$WARN SYMBOL_PLATFORM ON}
end;

end.
