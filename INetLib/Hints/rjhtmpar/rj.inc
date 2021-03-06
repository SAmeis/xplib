{ Compiler Directives }

{$DEFINE Debug}

{.$DEFINE RangeChecking}
{ This define determines whether Code operations will check for Ranges
  (in Lists, for example) and result in an Error-Message in case of violations. }

{$IFDEF Debug}

{$DEFINE RangeChecking}

{$D+,L+,C+  Enable debug information }
{$Q+} // Enable Overflow check
{$R+} // Enable RangeChecks

{$ELSE}

{$D-,L-,C-  Disable debug information }
{$Q-} // Disable Overflow check
{$R-} // Disable RangeChecks

{$ENDIF}

{$A+}
{ Use DWORD alignment whereever possible }

{$B-}
{ Short-circuit boolean expressions }

{$H+}
{ Huge string support}

{$I+}
{ I/O checking via exceptions }

{$IFDEF RangeChecking}
{$V+} { Enable var string checking }
{$ELSE}
{$V-} { Disable var string checking }
{$ENDIF}

{$T-}
{ The $T directive controls the types of pointer values generated by the @ operator.
  In the $T- state, the result type of the @ operator is always an untyped pointer
  that is compatible with all other pointer types. When @ is applied to a variable
  reference in the $T+ state, the type of the result is ^T, where T is compatible
  only with other pointers to the type of the variable. }

{$W-}
{ No Windows realmode stack frame }

{$X+}
{ Enable extended syntax }

