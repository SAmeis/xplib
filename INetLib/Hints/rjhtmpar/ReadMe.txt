rjHtmlParser Version 0.1a
------------------------------------------

Copyright (c) 2000 by Ralf Junker <ralfjunker@gmx.de>.
All rights reserved.


OVERVIEW
--------

rjHtmlParser includes three components: THtmlReporter, THtmlWriter, and
THtmlAutoWriter. They allow fast and flexible parsing and writing of HTML files.
Some of their features are:

* Very flexible interface. Read HTML files from any source and write to any
  destination by as-signing your own OnReadChar function. The parser will only
  read one character at a time, so you can even parse files of unlimited size.

* Advanced filtering options: THtmlReporter and THtmlAutoWriter allow to include
  or exclude all 8 elements (ASP, Comments, DTD, Scripts, SSI, Styles, Tags and
  Text) into the reporting or auto-writing. Filters for tags can be set for
  starttags and endtags separately.

* Proper handling of preformatted text.

* Conversion of HTML character entities.


LICENCE
-------

rjHtmlParser is provided "as is" and without warranty of any kind. At the
moment, it is free-ware for private use only. I might, however, change this in
the future. If you intend to use it commercially, please contact me via e-mail
at <ralfjunker@gmx.de>.


INSTALLATION
------------

IMPORTANT: When unzipping rjHtmlParser, make sure to recreate the directory
structure (in WinZip, check Use Folder Names).

You install rjHtmlParser by registering the components.
Select Component --> Install Component... on the menu bar.

Enter the full path and filename to the rjHtmlParser.pas file into the "Unit
file name" edit box or click "Browse" and locate it. If you want to add
rjHtmlParser to one of your packages, stay on the "Into existing package" tab,
otherwise move on to the "Into new package" tab. In any case enter the full
path and filename for the package where you want to install rjHtmlParser into
to the "Package file name" edit box. Then press "OK". Delphi will install the
components and add three new icons to your components palette.

You might also want to your adjust your library path to include rjHtmlParser.
Select Tools --> Environment Options... on the menu bar. Go to Library tab and
add the full path of your rjHtmlParser source directory to the Library Path.

HISTORY
-------

0.1   * Initial Public Release

0.1a  * Component palette icons now included.