{$IFDEF RegCLX}
	{$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I XPLib.inc}


unit RegCLX;

interface

uses
	Windows, SysUtils, Classes, Registry;


type
	TMetaCLXRegistry	=	class( TRegistry );
	TCLXRegistry		=	class( TMetaCLXRegistry );

implementation

end.
