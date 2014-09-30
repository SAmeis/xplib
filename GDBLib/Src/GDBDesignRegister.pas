{$IFDEF GDBDesignRegister }
{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I GDBLib.inc}
unit GDBDesignRegister;

interface

uses
  Classes;

procedure Register;

implementation

uses
  LkpGridForm, XPMemoryDataset;

procedure Register;
begin
  RegisterComponents('Data Access', [TXPMemoryDataset]);
  RegisterComponents('Super', [TLkpGrid]);
end;

end.
