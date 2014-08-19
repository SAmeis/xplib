unit strutils;

interface

uses sysutils,classes;

function ReplaceString (mainstring,findstring,replacewith:String):String;

implementation

function ReplaceString (mainstring,findstring,replacewith:String):String;
var fstart,fend,flen,mainlen:integer;preseg,postseg,holdresult:String;
begin
holdresult:=mainstring;
if findstring<>'' then
begin
fstart:= pos(findstring,mainstring);
flen:=length(findstring);
fend:= fstart + flen -1;
preseg:='';
postseg:='';
mainlen:=length(mainstring);
if fstart<>0 then
 begin
    if fstart>1 then preseg:=copy(mainstring,1,fstart-1);
    if fend<mainlen then postseg:=copy(mainstring,fend+1,mainlen-fend);
    holdresult:= preseg + replacewith + postseg;
 end;
 end;
replacestring:=holdresult;
end;

end.