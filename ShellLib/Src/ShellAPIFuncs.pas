{$IFDEF ShellAPIFuncs}
{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I ShellLib.inc}

unit ShellAPIFuncs deprecated 'Nada nesta unit vale para Win Vista ou superior = nunca usar';

interface

uses
  Windows, Messages, ShlObj, SysUtils, Classes, Graphics, ShellPIDL;

const
  Shell32 = 'shell32.dll';

implementation

end.
