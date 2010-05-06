//Linha adcionada para teste !!!!
//----------------------------------------------------------------------------------------------------------------------------------
{$IFDEF DEBUG_DCU}
  {$ASSERTIONS ON}
  {$DEBUGINFO ON}
  {$LOCALSYMBOLS ON}
  {$OPTIMIZATION OFF}
  {$REFERENCEINFO ON}
  {$A+,B-,C+,D+,E-,F-,G+,H+,I+,J-,K-,L+,M-,N+,O-,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$ELSE}
  {$ASSERTIONS OFF}
  {$DEBUGINFO OFF}
  {$LOCALSYMBOLS OFF}
  {$OPTIMIZATION ON}
  {$REFERENCEINFO OFF}
  {$A+,B-,C-,D-,E-,F-,G+,H+,I+,J-,K-,L-,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y-,Z1}
{$ENDIF}