TQRAngledLabels
QuickReport's text labels components with rotate capabilities
For Delphi 1, Delphi 2 (not tested) and Delphi 3.
Version 1.1a Out-20-1997
(c) 1997 Francisco Maia Goncalves Neto 



Description:

  TQRAngledLabel is a TQRAngledCustom descendant (only the publication), a text label 
  component with rotate text capabilities to use with QuickReport versions 1.x and 2.x

  TQRAngledDBText is a TQRAngledCustom descendant too, but with DB capabilities.



Properties:

  AnchorStyle: TAnchorStyle
    This property replaces the Alignment property of normal labels. It's to supply the 
    text alignment in any angle. If AutoSize is True, the position of component (Left and Top) 
    is modified to maintain the position of text. Otherwise, if AutoSize is False, the 
    position of text is adjusted inside the component.
    +--------------+--------------------------+
    | Value        | Meaning                  |
    +==============+==========================+
    |*asNone       | Normal behavior          | 
    +--------------+--------------------------+
    | asTextLeft   | Achor to left of text    |
    +--------------+--------------------------+
    | asTextCenter | Anchor on center of text |
    +--------------+--------------------------+
    | asTextRight  | Anchor to right of text  |
    +--------------+--------------------------+
    * Default value


  Angle: Integer
    This property indicates the angle of text in degrees. The value for this property can be 
    any integer value, and this value is automaticaly reduced to 0..359 range.


  TrueTypeAlert: TTrueTypeAlert
    This property prevents the use of non TrueType Fonts in the component. The Font 
    property normaly accept any font, but only TrueType Fonts can be cornered.
    +-----------------+------------------------------------------------------------+
    | Value           | Meaning                                                    |
    +=================+============================================================+
    | ttaNone         | Normal behavior. Accept any font                           | 
    +-----------------+------------------------------------------------------------+
    | ttaMessage      | Accept any font, but raise an alert message                |
    +-----------------+------------------------------------------------------------+
    |*ttaAbort        | Don't accept non TrueType fonts. A valid font is assigned  |
    +-----------------+------------------------------------------------------------+
    | ttaAbortMessage | Don't accept non TrueType fonts and raise an alert message |
    +-----------------+------------------------------------------------------------+
    * Default value



Disclaimer:

  This component is donated to the public as public domain.

  This component can be freely used and distributed in commercial and
  private environments.

  The source code may be freely distributed and used. The author
  accepts no responsibility for its use or misuse.

  The author assumes no liability for damages, direct or consequential, which
  may result from the use of this component.                           

  Please forward any comments or suggestions to 

  Francisco Maia Goncalves Neto at   fneto_br@hotmail.com



Contents of QRANGLBL.ZIP:

  README.TXT   : This file 
  QRANGLBL.PAS : Contains TQRAngledLabel and TQRAngledDBText components and their Register 
                 procedure
  QRACONST.PAS : Alert messages unit
  QRACONST.R16 : 16-bits compiled string resource (alert messages in english)
  QRACONST.R32 : 32-bits compiled string resource
  QRANGLBL.D16 : 16-bits component resource (image for the component palette)
  QRANGLBL.D32 : 32-bits component resource
  QRANGLBL.Q1X : QuickReport 1.x include file
  QRANGLBL.Q2X : QuickReport 2.x include file
  ..\RESOURCE    
     ..\ENG    : Fonts of resource files (english)
        QRACONST.R16 : 16-bits compiled resource alert messages
        QRACONST.R32 : 32-bits compiled resource alert messages
        QRACONST.RC  : Font of english resource file 
     ..\POR    : Fonts of resource files (portuguese)
        QRACONST.R16 : 16-bits compiled resource alert messages
        QRACONST.R32 : 32-bits compiled resource alert messages
        QRACONST.RC  : Font of portuguese resource file 



Installation:

  Verify your QuickReport version:

===>  For QuickReport 1.x:                            <===
        RENAME the QRANGLBL.Q1X file to QRANGLBL.INC  

===>  For QuickReport 2.x:                            <===
        RENAME the QRANGLBL.Q2X file to QRANGLBL.INC


  Copy the necessary files to your component directory:

    For Delphi 1.x:
      - QRANGLBL.PAS
      - QRANGLBL.D16
      - QRANGLBL.INC
      - QRACONST.PAS
      - QRACONST.R16

    For Delphi 2.x and Delphi 3.0:
      - QRANGLBL.PAS
      - QRANGLBL.D32
      - QRANGLBL.INC
      - QRACONST.PAS
      - QRACONST.R32


  To add this component to the component palette do:

    In Delphi 1.x and Delphi 2.x:
      - Select Options | Install Components option from delphi menu
      - Add QRANGLBL.PAS

    In Delphi 3.0:
      - Select Component | Install Components option from delphi menu
      - Into existing package page, type unit file name as QRANGLBL.PAS
        (the default package is 'Delphi User's Component')


History:

  October, 20, 1997 - Version 1.1a
    . Bug fix! Anchor Style don't work correctly in QRAngledDBText with QR1
    . Bug fix! .EXE Preview and Print don't work with some versions of QR2
    . Bug fix! The first QRAngledLabel placed directly in the QuickRep surface
               aren't printed rotated in QR2

  September, 25, 1997 - Version 1.1
    . New base class TQRAngledCustom. Totaly rewriten to works better with QR2
      Several print problems solved
    . New component! QRAngledDBText
    . New property! AnchorStyle
    . New property! TrueTypeAlert

  August, 27, 1997 - Version 1.0c
    . Bug fix! Cause a GPF when not placed in a TQuickRep in QR2
    . Bug fix! Preview and Print don't work in QR2 under D1

  August, 17, 1997 - Version 1.0b
    . Minor adjustments

  August, 03, 1997 - Version 1.0a
    . Bug fix! Several print bugs fixed under QR1 and QR2
    . Bug fix! Zoom of ParentReport considered in paint routine of QR2

  July, 26, 1997 - Version 1.0
    . Initial version



Know problems:

  QR2. The DataField property of QRAngledDBText must be assigned manualy



Notes:

  This component was tested with:
    - QuickReport 1.1a under Delphi 1;
    - QuickReport 1.1b under Delphi 3; 
    - QuickReport 2.0i under Delphi 1 and Delphi 3. 

  I think it's works properly with Delphi 2.x, but I really don't know.


Thanks to Keith Wood for his LabelEffect component.
Thanks to Olivier Exbrayat and Michael Snell for your help.



Please report any experience you get with this component to fneto_br@hotmail.com



Thanks in advance,

Francisco Maia Goncalves Neto
Belém - Pará - Brazil