{$IFDEF WebHnd}
{$A+,B-,C-,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O-,P+,Q-,R+,S-,T-,U-,V+,W-,X+,Y+,Z4}
{$DEBUGINFO ON}
{$ELSE}
{$A+,B-,C-,D-,E-,F-,G+,H+,I+,J+,K-,L-,M-,N+,O+,P+,Q+,R+,S-,T-,U-,V+,W-,X+,Y-,Z4}
{$F+}
{$ENDIF}

unit WebHnd;

interface

uses
   Graphics;


function RGB2HTM( Color : TColor ) : string;

implementation

uses
   BinHnd;

function RGB2HTM( Color : TColor ) : string;
//----------------------------------------------------------------------------------------------------------------------
begin
	Result:=BinHnd.Long2Hexa( ColorToRGB( Color ) );
	Result:=Copy( Result, 7, 2 ) + Copy( Result, 5, 2 ) + Copy( Result, 3, 2 );
end;


end.
 