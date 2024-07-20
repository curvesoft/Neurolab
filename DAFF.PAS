{ Borland-Pascal 7.0 }

unit daff;

{$IFDEF MSDOS}
{$A+,B-,E+,F-,G-,I-,N+,O-,P+,T-,V+,X-}
{$ELSE}
{$A+,B-,E+,F-,G+,I-,N+,P+,T-,V+,X-}
{$ENDIF}

interface

uses   crt, dos, objects, bequem;

const  samplebit=25;                      sampleshift=14;
       maxsample=(1 shl samplebit)-1;     minsample=-maxsample;
       sampleoffset=1 shl samplebit;
       richtung:(vow,ruw,mit)=vow;
       maxkan=16;

type   sample=minsample..maxsample;
       sampleword=0..1 shl (samplebit-sampleshift+1) -1;
       kanaldef=record
          nr:word;            name:string[12];
          offs:byte;          faktor1,faktor2:extended;
          einheit:string[12];
          end;
       kopfdaten=record
          produzent:string20;
          datum:string20;     uhrzeit:string20;   freq:extended;
          kennung:string[64]; anzahl:longint;     kopflaenge:longint;
          nkan:byte;          k:array[0..maxkan-1] of kanaldef;
          end;

       tagzeiger=^tag;
       tagzeigerzeiger=^tagzeiger;
       tag=object
          num:word;                     size:word;
          line:string[60];              next:tagzeiger;
          procedure nopar;              procedure lesen;
          procedure zeigen (var hin:text); virtual;
          procedure einordnen (var ko:kopfdaten); virtual;
          procedure kaneinordnen (var kl:kanaldef; var ko:kopfdaten); virtual;
          destructor done; virtual;
          end;

       contextzeiger=^context;
       context=object (tag)
          nested:tagzeiger;
          procedure zeigen (var hin:text); virtual;
          procedure einordnen (var ko:kopfdaten); virtual;
          destructor done; virtual;
          end;

       chancontextzeiger=^chancontext;
       chancontext=object (context)
          procedure einordnen (var ko:kopfdaten); virtual;
          end;

       tbroken=object (tag)
          w:extended;
          procedure lesen;
          end;

       tstring=object (tag)
          st:string;
          procedure lesen;
          end;

       tname=object (tstring)
          constructor lesen;
          procedure kaneinordnen (var kl:kanaldef; var ko:kopfdaten); virtual;
          end;
       pname=^tname;
       tlabel=object (tstring)
          constructor lesen;
          procedure einordnen (var ko:kopfdaten); virtual;
          end;
       plabel=^tlabel;
       tdate=object (tag)
          dt:datetime;
          constructor lesen;
          procedure einordnen (var ko:kopfdaten); virtual;
          end;
       pdate=^tdate;

       tival   =object (tag)
          li:longint;
          constructor lesen;
          end;
       pival=^tival;
       tdval   =object (tag)
          ex:extended;
          constructor lesen;
          end;
       pdval=^tdval;
       ttval   =object (tag)
          st:string[255];
          constructor lesen;
          end;
       ptval=^ttval;

       theader =object (context)
          constructor lesen;
          end;
       pheader=^theader;
       tftyp   =object (tstring)
          constructor lesen;
          procedure einordnen (var ko:kopfdaten); virtual;
          end;
       pftyp=^tftyp;
       tprod   =object (tstring)
          constructor lesen;
          procedure einordnen (var ko:kopfdaten); virtual;
          end;
       pprod=^tprod;

       tdatadef=object (context)
          constructor lesen;
          end;
       pdatadef=^tdatadef;
       tblksize=object (tag)
          li:longint;
          constructor lesen;
          procedure einordnen (var ko:kopfdaten); virtual;
          end;
       pblksize=^tblksize;
(*     tsource =object (tag)
          st:string[80];
          constructor lesen;
          end;
       tsrcoffs=object (tag)
          li:longint;
          constructor lesen;
          end;
       tsrctyp =object (tag)
          ba:(bin,ascii);
          constructor lesen;
          end; *)
       tnestdatadef=object (chancontext)
          constructor lesen;
          procedure einordnen (var ko:kopfdaten); virtual;
          end;
       pnestdatadef=^tnestdatadef;
       tblkoffs=object (tag)
          bo:byte;
          constructor lesen;
          procedure kaneinordnen (var kl:kanaldef; var ko:kopfdaten); virtual;
          end;
       pblkoffs=^tblkoffs;
       tdatat=object (tag)
          dtp:byte;
          constructor lesen;
          procedure kaneinordnen (var kl:kanaldef; var ko:kopfdaten); virtual;
          end;
       pdatat=^tdatat;

       tchanl=object (context)
          constructor lesen;
          end;
       pchanl=^tchanl;
       tnestchanl=object (chancontext)
          constructor lesen;
          end;
       pnestchanl=^tnestchanl;
       tfactor=object (tbroken)
          constructor lesen;
          procedure kaneinordnen (var kl:kanaldef; var ko:kopfdaten); virtual;
          end;
       pfactor=^tfactor;
       tspec=object (tstring)
          constructor lesen;
          procedure kaneinordnen (var kl:kanaldef; var ko:kopfdaten); virtual;
          end;
       pspec=^tspec;
       tsfrq=object (tbroken)
          constructor lesen;
          procedure kaneinordnen (var kl:kanaldef; var ko:kopfdaten); virtual;
          end;
       psfrq=^tsfrq;

       tschluss=object (context)
          kl,dl:longint;
          constructor lesen;
          procedure einordnen (var ko:kopfdaten); virtual;
          end;
       pschluss=^tschluss;

var    tulabfehler:boolean;
       hin:text;

var    daten,seqdaten:file;

procedure kopflesen (name:string80; var anfang:contextzeiger);
procedure kopfzeigen (anfang:tagzeiger; var hin:text);
procedure kopf (name:string80; var ko:kopfdaten);
procedure oeffne (name:string80; var ko:kopfdaten);
function lese (position:longint; kanal:byte):sample;
procedure schliesse;
procedure ausserbetrieb;

procedure seqschreibe (zahl:longint);
procedure seqoeffne;
procedure seqschliesse;

implementation

const  maxpuffer=(65534 div sizeof(sampleword))-1;
       zustand:(offen,zu,aus)=zu;

       leerkd:kanaldef=(nr:0; name:''; offs:0; faktor1:1/(1 shl sampleshift);
                        faktor2:1; einheit:'V');
       leerk:kopfdaten=(produzent:'Unbekannt'; datum:''; uhrzeit:''; freq:0;
                        kennung:''; anzahl:0; kopflaenge:0; nkan:0);

type   pufferfeld=array[0..maxpuffer-1] of sampleword;
       daffheader=record
          swg:array[1..10] of char;        sex:word;
          version:word;                    flags:word;
          size:longint;                    res:array[1..8] of char;
          end;

var    s:tbufstream;
       puffer,seqpuffer:^pufferfeld;
       pufferanfang,pufferende,seqi:longint;
       kopflaenge:longint;
       fileende:longint;
       kan:byte;

{ Prozeduren }

procedure neu (var now:tagzeigerzeiger; tg:tagzeiger);
begin
now^:=tg; now:=@now^^.next;
end;

procedure skiptag;
var   num,dtyp,size,value:word;
      parm1,lsize,i:longint;
      parm2:array[1..4] of byte;
begin
s.read(num,2); s.read(dtyp,2);
s.read(size,2);
s.read(parm1,4); s.read(parm2,4);
case size of
   0..8:exit;
   9..$FFFE:lsize:=size;
   $FFFF:lsize:=parm1;
   end;
for i:=1 to (lsize+1)div 2 do s.read(value,2);
end;

procedure skipcontext;
var   id:word;
      rest:array[1..14] of byte;
begin
s.read(rest,14);
repeat
   s.read(id,2);
   case id of
      $0001..$7FFF:skiptag;
      $8000..$FFFE:skipcontext;
      $FFFF:begin skiptag; exit end;
      end;
until false;
end;

procedure kopflesen (name:string80; var anfang:contextzeiger);
label ende;
var id:word;
    now:tagzeigerzeiger;
    dh:daffheader;
begin
tulabfehler:=false;
s.init(name,stopenread,32000);
s.read(dh,28);
if dh.swg<>'SWGBSMBWS'#0 then begin
   fehler('No Turbolab-format file.');
   tulabfehler:=true;
   s.done; exit end;
now:=@anfang;
repeat
   s.read(id,2);
      case id of
      $0000..$7FFF:tulabfehler:=true;
      $8002:neu(now,new(pheader,lesen));
      $8003:neu(now,new(pdatadef,lesen));
      $8005:neu(now,new(pchanl,lesen));
      $8006..$FFFE:skipcontext;
      $8004{Daten}:goto ende;
      $FFFF:tulabfehler:=true;
      end;
   id:=$0000;
   if tulabfehler then begin
      fehler('Incorrect Turbolab 4.2 - Header.'); goto ende end;
until false;

ende:neu(now,new(pschluss,lesen)); now^:=nil; s.done;
end;

procedure kopfzeigen (anfang:tagzeiger; var hin:text);
var   wandert:tagzeiger;
begin
wandert:=anfang;
while wandert<>nil do begin
   wandert^.zeigen(hin);
   wandert:=wandert^.next end;
end;

procedure kopf (name:string80; var ko:kopfdaten);
var   anfang,wandert,hilf1,hilf2:tagzeiger;
      i:byte;
begin
kopflesen(name,contextzeiger(anfang));
if tulabfehler then exit;
if anfang<>nil then begin
   wandert:=anfang;
   while (wandert^.next<>nil) and (typeof(wandert^.next^)<>typeof(tdatadef)) do
      wandert:=wandert^.next;
   hilf1:=wandert^.next^.next;
   hilf2:=anfang;
   anfang:=wandert^.next;
   anfang^.next:=hilf2^.next;
   hilf2^.next:=hilf1;
   wandert^.next:=hilf2;
end;
ko:=leerk;
for i:=0 to maxkan-1 do ko.k[i]:=leerkd;
wandert:=anfang;
while wandert<>nil do begin wandert^.einordnen(ko); wandert:=contextzeiger(wandert^.next) end;
if (ko.freq=0) or (ko.nkan=0) then tulabfehler:=true;
if anfang<>nil then dispose(anfang,done);
end;

procedure oeffne (name:string80; var ko:kopfdaten);
var   puf:array[0..1] of byte;
begin
kopflaenge:=ko.kopflaenge; kan:=ko.nkan;
if zustand=aus then exit;
assign(daten,name); reset(daten,sizeof(sampleword));
tulabfehler:=ioresult>0;
if not tulabfehler then begin
   fileende:=(filesize(daten)-kopflaenge) div kan;
   zustand:=offen;
   end
                   else begin
   writeln(lfcr); fehler('No access to file "'+name+'".'); warte end;
pufferanfang:=0;  pufferende:=0;
end;

procedure seqoeffne;
begin
reset(seqdaten,sizeof(sampleword));
seek(seqdaten,filesize(seqdaten));
new(seqpuffer);
seqi:=-1;
end;

procedure seqschliesse;
var   lenpuffer:word;
begin
if seqi>=0 then blockwrite(seqdaten,seqpuffer^,seqi,lenpuffer);
close(seqdaten);
dispose(seqpuffer);
end;

procedure schliesse;
begin
if zustand=offen then begin
   close(daten);
   tulabfehler:=ioresult>0;
   if tulabfehler then begin
      writeln(lfcr); fehler('No access to data file.'); warte end;
   pufferanfang:=0; pufferende:=0;
   end;
end;

procedure ausserbetrieb;
begin
dispose(puffer); zustand:=aus;
end;

procedure seqschreibe (zahl:longint);
var    lenpuffer:word;
begin
inc(seqi);
seqpuffer^[seqi]:=(zahl+sampleoffset) shr sampleshift;
if seqi=maxpuffer-1 then begin
   blockwrite(seqdaten,seqpuffer^,maxpuffer,lenpuffer);
   seqi:=-1;
   end;
end;

function lese (position:longint; kanal:byte):sample;
var   anfang:longint;
      lenpuffer:word;
      puff:longint;
begin
if (position<pufferanfang) or (position>=pufferende) then begin
   if (position<0) or (position>=fileende) then begin
      lese:=0; exit end;
   case richtung of
      vow:anfang:=position;
      ruw:anfang:=max(position+1-maxpuffer div kan,0);
      mit:anfang:=max(position+1-maxpuffer div (2*kan),0);
      end;
   seek(daten,anfang*kan+kopflaenge);
   blockread(daten,puffer^,maxpuffer,lenpuffer);
   tulabfehler:=ioresult>0;
   if not tulabfehler then begin
      pufferanfang:=anfang;
      pufferende:=pufferanfang+lenpuffer div kan end
                      else begin
      writeln(lfcr); fehler('No access to data file.'); warte;
      pufferanfang:=0;
      pufferende:=0;
      zustand:=zu; lese:=0; exit end
   end;
puff:=puffer^[(position-pufferanfang)*kan+kanal];
lese:=puff shl sampleshift - sampleoffset;
end;

{ tag }

procedure tag.nopar;
var   parm:array[1..14] of char;
begin
s.read(parm,14);
end;

procedure tag.lesen;
var   dtyp:word;
begin
s.read(num,2); s.read(dtyp,2); s.read(size,2);
end;

procedure tag.zeigen (var hin:text);
begin
writeln(hin,line);
end;

procedure tag.einordnen (var ko:kopfdaten);
begin end;

procedure tag.kaneinordnen (var kl:kanaldef; var ko:kopfdaten);
begin end;

destructor tag.done;
begin
if next<>nil then dispose(next,done);
end;

{ context }

procedure context.einordnen (var ko:kopfdaten);
var  wandert:tagzeiger;
begin
wandert:=nested;
while wandert<>nil do begin
   wandert^.einordnen(ko); wandert:=wandert^.next end;
end;

procedure context.zeigen(var hin:text);
var   wandert:tagzeiger;
begin
writeln(hin,line);
wandert:=nested;
while wandert<>nil do begin
   wandert^.zeigen(hin);
   wandert:=wandert^.next end;
end;

destructor context.done;
begin
tag.done;
if nested<>nil then dispose(nested,done);
end;

{ chancontext }

procedure chancontext.einordnen (var ko:kopfdaten);
var   i:byte;


procedure bearbeiten (var kl:kanaldef);
var  wandert:tagzeiger;
begin
wandert:=nested;
while wandert<>nil do begin
   wandert^.kaneinordnen(ko.k[i],ko);
   wandert:=wandert^.next end;
end;

begin
if num=0 then for i:=0 to ko.nkan-1 do bearbeiten(ko.k[i])
         else begin
   i:=0; while (ko.k[i].nr<>num) do inc(i);
   bearbeiten(ko.k[i]);
   end;
end;

{ tbroken }

procedure tbroken.lesen;
const b32=2147483648.0;
      ln2=0.693147180559945;
var   zahl:record
        mantissa:longint;
        exponent:integer;
        rest:array[1..2] of byte;
        end;
begin
tag.lesen;
s.read(zahl,8);
w:=(zahl.mantissa/b32)*exp(ln2*zahl.exponent);
end;


{ tstring }

procedure tstring.lesen;
var   parm:array[1..8] of char;
begin
tag.lesen;
s.read(parm,8);
case size of
   0..8:st:=parm;
   8..255:s.read(st[1],((size +1) div 2)*2);
   end;
st[0]:=chr(size);
end;

{ tname }

constructor tname.lesen;
begin
tstring.lesen;
line:='    Name: '+st;
end;

procedure tname.kaneinordnen (var kl:kanaldef; var ko:kopfdaten);
begin
kl.name:=st;
end;

{ tlabel }

constructor tlabel.lesen;
begin
tstring.lesen;
line:='    Description: '+st;
end;

procedure tlabel.einordnen (var ko:kopfdaten);
begin
ko.kennung:=st;
end;

{ tdate }

constructor tdate.lesen;
var   time:longint;
begin
tag.lesen;
case num of
   0:begin s.read(time,4); unpacktime(time,dt); s.read(time,4) end;
   1:;
   end;
with dt do
   line:='    Date: '+wort(day)+'.'+wort(month)+'.'+wort(year)+' Time: '
         +wort(hour)+':'+wort(min)+':'+wort(sec);
end;

procedure tdate.einordnen (var ko:kopfdaten);
begin
with dt do begin
   ko.datum:=wort(day)+'.'+wort(month)+'.'+wort(year);
   ko.uhrzeit:=wort(hour)+':'+wort(min)+':'+wort(sec);
   end;
end;

{ tival }

constructor tival.lesen;
begin
tag.lesen;
end;

{ tdval }

constructor tdval.lesen;
begin
tag.lesen;
end;

{ ttval }

constructor ttval.lesen;
begin
tag.lesen;
end;

{ theader }

constructor theader.lesen;
var   id:word; now:tagzeigerzeiger;
      parms:array[1..8] of char;
begin
tag.lesen;
line:='Header:';
s.read(parms,8);
now:=@nested;
repeat
   s.read(id,2);
   case id of
      $0001:neu(now,new(pname,lesen));
      $0002:neu(now,new(plabel,lesen));
      $0003:neu(now,new(pdate,lesen));
      $0080:neu(now,new(pftyp,lesen));
      $0081:neu(now,new(pprod,lesen));
      $0004..$007F,$0082..$7FFF:skiptag;
      $8000..$FFFE:skipcontext;
      $FFFF:begin nopar; now^:=nil; exit end;
      end;
until false;
end;

{ tftyp }

constructor tftyp.lesen;
begin
tstring.lesen;
line:='    Filetyp: '+st;
end;

procedure tftyp.einordnen (var ko:kopfdaten);
begin
if num<>0 then begin
   tulabfehler:=true;
   fehler('No "Sampled Data File" but "'+st+'".');
   end;
end;

{ tprod }

constructor tprod.lesen;
begin
tstring.lesen;
line:='    Producer of file: '+st;
end;

procedure tprod.einordnen (var ko:kopfdaten);
begin
ko.produzent:=st;
end;

{ tdatadef }

constructor tdatadef.lesen;
var   id:word; now:tagzeigerzeiger;
      parms:array[1..8] of char;
begin
tag.lesen;
line:='Data definition:';
s.read(parms,8);
now:=@nested;
repeat
   s.read(id,2);
   case id of
      $0098:neu(now,new(pblksize,lesen));
      $0000..$0097,$0099..$7FFF:skiptag;
      $8003:neu(now,new(pnestdatadef,lesen));
      $8000..$8002,$8004..$FFFE:skipcontext;
      $FFFF:begin nopar; now^:=nil; exit end;
      end;
if tulabfehler then exit;
until false;
end;

{ tblksize }

constructor tblksize.lesen;
var   parm:array [1..4] of byte;
begin
tag.lesen;
s.read(li,4);
s.read(parm,4);
line:='    Block size: '+wort(li);
end;

procedure tblksize.einordnen (var ko:kopfdaten);
begin
ko.nkan:=li div 2;
end;

(* noch nicht benutzt:
{ tsource }

constructor tsource.lesen;
begin
tag.lesen;
end;

{ tsrcoffs }

constructor tsrcoffs.lesen;
begin
tag.lesen;
end;

{ tsrctyp }

constructor tsrctyp.lesen;
begin
tag.lesen;
end; *)

{ tnestdatadef }

constructor tnestdatadef.lesen;
var   id:word; now:tagzeigerzeiger;
      parms:array[1..8] of char;
begin
tag.lesen;
line:='  Channel data definition ['+wort(num)+']:';
s.read(parms,8);
now:=@nested;
repeat
   s.read(id,2);
   case id of
      $00A0:neu(now,new(pblkoffs,lesen));
      $00A2:neu(now,new(pdatat,lesen));
      $0000..$009F,$00A1,$00A3..$7FFF:skiptag;
      $8000..$FFFE:skipcontext;
      $FFFF:begin nopar; now^:=nil; exit end;
      end;
if tulabfehler then exit;
until false;
end;

procedure tnestdatadef.einordnen (var ko:kopfdaten);
var  wandert:tagzeiger;
     kd:kanaldef;
begin
wandert:=nested;
kd:=leerkd;
while wandert<>nil do begin
   wandert^.kaneinordnen(kd,ko);
   wandert:=wandert^.next end;
kd.nr:=num;
ko.k[kd.offs div 2]:=kd;
end;

{ tblkoffs }

constructor tblkoffs.lesen;
var   parm:array [1..4] of byte;
      blkoffs:longint;
begin
tag.lesen;
s.read(blkoffs,4); bo:=blkoffs;
s.read(parm,4);
line:='    Block offset: '+wort(bo);
end;

procedure tblkoffs.kaneinordnen (var kl:kanaldef; var ko:kopfdaten);
begin
kl.offs:=bo;
end;

{ tdatat }

constructor tdatat.lesen;
const dtyp:array [0..23] of string[10]=
        ('unknown','unknown','Char','Char','ñ Byte','Byte',
         'ñ word','word','ñ long','long','ñ real4','real4','ñ real8','real8',
         'ñ broken','broken','','','','','','','','');
var   parm:array [1..4] of byte;
      datat:longint;
begin
tag.lesen;
s.read(datat,4); dtp:=datat mod 256;
s.read(parm,4);
line:='    Data type: '+dtyp[dtp];
end;

procedure tdatat.kaneinordnen (var kl:kanaldef; var ko:kopfdaten);
begin
if dtp<>6 then begin
   tulabfehler:=true;
   fehler('Unknown data format'); writeln;
   end;
end;

{ tchanl }

constructor tchanl.lesen;
var   id:word; now:tagzeigerzeiger;
      parms:array[1..8] of char;
begin
tag.lesen;
line:='Channel protocol:';
s.read(parms,8);
now:=@nested;
repeat
   s.read(id,2);
   case id of
      $0000..$7FFF:skiptag;
      $8005:neu(now,new(pnestchanl,lesen));
      $8000..$8004,$8006..$FFFE:skipcontext;
      $FFFF:begin nopar; now^:=nil; exit end;
      end;
if tulabfehler then exit;
until false;
end;

{ tnestchanl }

constructor tnestchanl.lesen;
var   id:word; now:tagzeigerzeiger;
      parms:array[1..8] of char;
begin
tag.lesen;
line:='  Channel definition ['+wort(num)+']:';
s.read(parms,8);
now:=@nested;
repeat
   s.read(id,2);
   case id of
      $0001:neu(now,new(pname,lesen));
      $0082:neu(now,new(pfactor,lesen));
      $0083:skiptag;
      $0084:neu(now,new(pspec,lesen));
      $008A:neu(now,new(psfrq,lesen));
      $0000,$0002..$0081,$0085..$0089,$008B..$7FFF:skiptag;
      $8000..$FFFE:skipcontext;
      $FFFF:begin nopar; now^:=nil; exit end;
      end;
until false;
end;

{ tfactor }

constructor tfactor.lesen;
begin
tbroken.lesen;
line:='    Axis: '+wort(num)+' mit Faktor: '+extewort(w,3,2);
end;

procedure tfactor.kaneinordnen (var kl:kanaldef; var ko:kopfdaten);
const mal=1 shl sampleshift;
begin
case num of
   1:kl.faktor1:=w/mal;
   2:kl.faktor2:=w;
   end;
end;

{ tspec }

constructor tspec.lesen;
var   parm:array [1..4] of byte;
      blkoffs:longint;
begin
tstring.lesen;
line:='    Axis: '+wort(num)+' mit Einheit: '+st;
end;

procedure tspec.kaneinordnen (var kl:kanaldef; var ko:kopfdaten);
begin
if num=2 then begin
   if st='Volt' then kl.einheit:='V'
                else kl.einheit:=st;
   end;
end;

{ tsfrq }

constructor tsfrq.lesen;
begin
tbroken.lesen;
line:='    Frequency: '+extwort(w,8,1);
end;

procedure tsfrq.kaneinordnen (var kl:kanaldef; var ko:kopfdaten);
begin
ko.freq:=w;
end;

{ tschluss }

constructor tschluss.lesen;
begin
nopar;
kl:=s.getpos div 2;
dl:=s.getsize div 2 -kl;
nested:=nil;
line:='    Total header: '+wort(kl)+' Size: '+wort(dl);
end;

procedure tschluss.einordnen (var ko:kopfdaten);
begin
ko.kopflaenge:=kl; ko.anzahl:=dl div ko.nkan;
end;

{ Initialisierung }

begin
new(puffer);
pufferanfang:=0; pufferende:=0;
end.
