unit lowX;
//{$MODE DELPHI}
interface
uses {Strings,}classes,sysutils,strutils{,dos,crt};

function GetTagProperty(searchstring,tagname,propertyname:String):String;
function GetTagData (searchstring,tagname:String):String;
function GetTagString (searchstring,tagname:STRING):String;
function GetTagList (searchstring,tagname:String):TStringlist; {list of tagdata}
function GetTagListProperty (List:TStringlist;tag,propertyname:String):TStringlist;
function GetStringFromListWhereProperty (List:TStringlist;tag,propertyname,value:String):String;

function RemoveTagListFromText (executionSource:String;objhold:TStringlist):String;


function GetTermTagString (searchstring,tagname:String):String;
function GetTermTagList (searchstring,tagname:STRING):TStringlist; {list of tagdata}
function GetTermTagListProperty (List:TStringlist;tag,propertyname:String):TStringlist;
function GetTermTagProperty(searchstring,tagname,propertyname:String):String;

implementation

function GetStringFromListWhereProperty (List:TStringlist;tag,propertyname,value:String):String;
var mlist:TStringlist;hold:String;i:integer;
begin
{Return }
hold:=list.text;
mlist:=GetTagList(hold,tag);
result:='';
for i:= 0 to mlist.count-1 do
  begin
   if GetTagProperty(Mlist[i],tag,propertyname)=value then result:=result+mlist[i];
  end;
end;

function RemoveTagListFromText (executionSource:String;objhold:TStringlist):String;
var i,maxl:integer; hold,pulltext,puttext,wc:String;
begin
 hold:=executionsource;
 maxl:=objhold.count-1;
 for i:= 0 to maxl do
   begin
     	pulltext:=objhold[i];	
	hold:=ReplaceString(hold,pulltext,'');
   end;
result:=hold;
end;

function GetTermTagListProperty (List:TStringlist;tag,propertyname:String):TStringlist;
var i,max:integer;
begin
 result:=tstringlist.create;
 max:=List.count-1;
 for i:= 0 to max do
   begin
     result.add (GetTermTagProperty (List[i],tag,propertyname));
   end;
end;

function GetTagListProperty (List:TStringlist;tag,propertyname:String):TStringlist;
var i,max:integer;
begin
 result:=tstringlist.create;
 max:=List.count-1;
 for i:= 0 to max do
   begin
     result.add (GetTagProperty (List[i],tag,propertyname));
   end;
end;

function GetTermTagString (searchstring,tagname:STRING):String;
var bpos,epos,slen,i,mlen:integer;hold:String;
begin
{Gets the text of a terminated Tag <TAG />}
hold:=searchstring;
mlen:=length(hold);
bpos:=pos('<'+tagname,searchstring);
epos:=-1;
hold:=copy(hold,bpos,mlen-bpos);
epos:=pos('/>',hold);
result:='';
if epos<>0 then result:=copy(hold,1,epos+1);
end;

function GetTagString (searchstring,tagname:STRING):String;
var bpos,epos,slen,i,mlen:integer;hold:String;
begin
{Gets the text of a terminated Tag <TAG />}
hold:=searchstring;
mlen:=length(hold);
bpos:=pos('<'+UPPERCASE(tagname),uppercase(searchstring));
epos:=-1;
hold:=copy(hold,bpos,mlen-bpos);
epos:=pos('</'+UPPERCASE(tagname)+'>',UPPERCASE(hold));
result:='';
if epos<>0 then result:=copy(hold,1,epos+2+length(tagname));
end;


function GetTagList (searchstring,tagname:STRING):TStringlist; {list of tagdata}
var hold,tagtext:String;
begin
result:=tstringlist.create;
hold:=searchstring;
 while pos('<'+tagname,hold)<>0 do
   begin
	tagtext:=GetTagString(hold,tagname);
 	if tagtext<>'' then result.add (tagtext);
	hold:=Replacestring(hold,tagtext,'');	
   end;
end;

function GetTermTagList (searchstring,tagname:STRING):TStringlist; {list of tagdata}
var hold,tagtext:String;
begin
result:=tstringlist.create;
hold:=searchstring;
 while pos('<'+tagname,hold)<>0 do
   begin
	tagtext:=GetTermTagString(hold,tagname);
 	if tagtext<>'' then result.add (tagtext);
	hold:=Replacestring(hold,tagtext,'');	
   end;
end;

function TagEnd (searchstring,tagname:String):integer;
var startpos,endpos,iter:integer;hold:String;inquote:boolean;
begin
startpos:=pos('<'+tagname,searchstring);
endpos:=0;
iter:=startpos;
inquote:=false;
while endpos=0 do
 begin
   hold:=hold+searchstring[iter];
   inc(iter);
   if searchstring[iter]='"' then inquote:=not(inquote) ;
   if (not(inquote) and (searchstring[iter]='>')  ) then endpos:=iter;
 end;
result:=endpos;
end;

function GetTagProperty(searchstring,tagname,propertyname:String):String;
var propdata,tagdata:String;spos,epos,ppos,plen,qpos:integer;
begin
 spos:=pos('<'+tagname,searchstring);
propdata:='';
 epos:=TagEnd(searchstring,tagname);
//writeln ('Start:'+inttostr(spos)+' END:'+inttostr(epos));
 tagdata:= Copy(searchstring,1,epos);
//writeln ('TAGDATA:'+tagdata);
 ppos:=pos( Uppercase(propertyname)+'=',Uppercase(tagdata));
 if ppos<>0 then
 begin
   plen:=ppos+length(propertyname)+1;
   propdata:=copy(tagdata,plen+1,length(tagdata)-plen-1);
  //writeln('PROPDATA:'+propdata);
   qpos:=pos('"',propdata);
   propdata:=copy(propdata,1,qpos-1);
 end;
 result:=propdata;
end;
 

function GetTermTagProperty(searchstring,tagname,propertyname:String):String;
var propdata,tagdata:String;spos,epos,ppos,plen,qpos:integer;
begin
 //writeln(searchstring);
 propdata:='';
 spos:=pos('<'+tagname,searchstring);
 epos:=TagEnd(searchstring,tagname);
//writeln ('Start:'+inttostr(spos)+' END:'+inttostr(epos));
 tagdata:= Copy(searchstring,1,epos);
//writeln ('TAGDATA:'+tagdata);
 
 ppos:=pos( Uppercase(propertyname)+'=',Uppercase(tagdata));
 if ppos<>0 then
 begin
   plen:=ppos+length(propertyname)+1;
   propdata:=copy(tagdata,plen+1,length(tagdata)-plen-1);
  //writeln('PROPDATA:'+propdata);
   qpos:=pos('"',propdata);
   propdata:=copy(propdata,1,qpos-1);
 end;
 result:=propdata;
end;

function GetTagData (searchstring,tagname:String):String;
var  tagdata:String;spos,epos,ppos,plen,qpos:integer;
begin
 spos:=pos('<'+tagname,searchstring);
 result:='';
 if spos<>0 then
 begin
 epos:=TagEnd(searchstring,tagname);
 ppos:=pos('</'+Uppercase(tagname)+'>',searchstring)-1;
 result:=copy(searchstring,epos+1,ppos-epos);
 end ;
end; 

end.