program DemoTRELib;

uses
  Forms,
  MainDemoForm in 'MainDemoForm.pas' {Form1},
  TREZones in '..\..\Src\TREZones.pas',
  OPXMLSerializable in '..\..\..\OPLib\Src\OPXMLSerializable.pas',
  XmlSerial in 'XmlSerial.pas',
  RttiUtils in 'RttiUtils.pas',
  Rtti in 'c:\arquivos de programas\embarcadero\rad studio\7.0\source\Win32\rtl\common\Rtti.pas',
  MainDemoUtils in 'MainDemoUtils.pas',
  JvgXMLSerializer in '..\..\..\Jedi\jvcl\run\JvgXMLSerializer.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
