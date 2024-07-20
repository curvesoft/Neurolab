{Turbo-Pascal 6.0}

unit  tlfilter;

{$A+,B-,F-,G+,I-,N+,O-,V-,X-}

interface

uses  objects, bequem, tulab;

const filtermax=16;
      maxkanal=maxkan+filtermax;

      genau=4; weite=2000;

      spikemax=30;

type  einheitstring=string[7];
      einheittyp=object
         faktor:extended; vor:string[1]; anfang:string[7]; sekunde:integer;
         procedure kopie(var ein:einheittyp);
         function einhwort:einheitstring;
         function plot:string80;
         procedure handlich;
         procedure schwierig;
         end;

      grundtyp=object (einheittyp)
         gain:byte;
         multi:extended;
         procedure setz (m:extended; anf:einheitstring);
         end;
      grundlistetyp=array[0..maxkan-1] of grundtyp;

      belegung=object (einheittyp)
         negativ:boolean;
         quadratda:boolean;
         procedure grundsetzen (k:byte);
         end;

      schriftlistentyp=array[0..maxkanal] of string[10];

      filterzeiger=^filter;
      filter=object (tobject)
         name:string80;
         next:filterzeiger;
         destructor alt; virtual;
         procedure einheitveraendern (var  beleg:belegung); virtual;
         procedure vorbereitung (frequenz:extended); virtual;
         function gefiltert (posi:longint):integer; virtual;
         procedure load (var s:tbufstream);
         procedure store (var s:tbufstream);
         end;

      invertzg=^invert;
      invert=object (filter)
         constructor neu;
         function gefiltert (posi:longint):integer; virtual;
         constructor load (var s:tbufstream);
         end;

      offsetzg=^offset;
      offset=object (filter)
         constructor neu (k:byte; hoch:integer);
         function gefiltert (posi:longint):integer; virtual;
         constructor load (var s:tbufstream);
         procedure store (var s:tbufstream);
       private
         ho:integer;
         end;

      malfaktorzg=^malfaktor;
      malfaktor=object (filter)
         constructor neu (malfak:extended);
         function gefiltert (posi:longint):integer; virtual;
         constructor load (var s:tbufstream);
         procedure store (var s:tbufstream);
       private
         float:extended;
         end;

      kappenzg=^kappen;
      kappen=object (filter)
         constructor neu;
         function gefiltert (posi:longint):integer; virtual;
         constructor load (var s:tbufstream);
         end;

      absolutzg=^absolut;
      absolut=object (filter)
         constructor neu;
         procedure einheitveraendern (var beleg:belegung); virtual;
         function gefiltert (posi:longint):integer; virtual;
         constructor load (var s:tbufstream);
         end;

      squarezg=^square;
      square=object (absolut)
         constructor neu;
         procedure einheitveraendern (var beleg:belegung); virtual;
         function gefiltert (posi:longint):integer; virtual;
         constructor load (var s:tbufstream);
         end;

      glattzg=^glatt;
      glatt=object (filter)
         constructor neu (breims:extended);
         procedure vorbereitung (frequenz:extended); virtual;
         function gefiltert (posi:longint):integer; virtual;
         constructor load (var s:tbufstream);
         procedure store (var s:tbufstream);
       private
         brei:extended;
         breianz,breianz2:longint;
         end;

      spaltzeiger=^spaltfeld;
      spaltfeld=array[0..weite] of single;
      passfilter=object (filter)
         destructor alt; virtual;
         function gefiltert (posi:longint):integer; virtual;
         procedure load (var s:tbufstream);
         procedure store (var s:tbufstream);
       private
         gr:longint;
         spaltgr:spaltzeiger;
         we:word;
         procedure neu (grenzfreq:longint);
         end;
      tiefpasszg=^tiefpass;
      tiefpass=object (passfilter)
         constructor neu (grenzfreq:longint);
         procedure vorbereitung (frequenz:extended); virtual;
         constructor load (var s:tbufstream);
         end;
      hochpasszg=^hochpass;
      hochpass=object (passfilter)
         constructor neu (grenzfreq:longint);
         procedure vorbereitung (frequenz:extended); virtual;
         procedure einheitveraendern (var beleg:belegung); virtual;
         constructor load (var s:tbufstream);
         end;

      diffzg=^diff;
      diff=object (filter)
         constructor neu;
         procedure einheitveraendern (var beleg:belegung); virtual;
         procedure vorbereitung (frequenz:extended); virtual;
         function gefiltert (posi:longint):integer; virtual;
         constructor load (var s:tbufstream);
         end;

      verschiebezg=^verschiebe;
      verschiebe=object (filter)
         constructor neu (umms:extended);
         procedure vorbereitung (frequenz:extended); virtual;
         function gefiltert (posi:longint):integer; virtual;
         constructor load (var s:tbufstream);
         procedure store (var s:tbufstream);
       private
         um:extended;
         posid:longint;
         end;

      maxminzg=^maxmin;
      maxmin=object (filter)
         constructor neu (breims:extended);
         procedure einheitveraendern (var beleg:belegung); virtual;
         procedure vorbereitung (frequenz:extended); virtual;
         function gefiltert (posi:longint):integer; virtual;
         constructor load (var s:tbufstream);
         procedure store (var s:tbufstream);
       private
         brei:extended;
         br2:longint;
         end;

      spikefilterzg=^spikefilter;
      spikefilter=object (filter)
         constructor neu (k:byte; millisek,ablinks,abrechts:extended);
         procedure vorbereitung (frequenz:extended); virtual;
         function gefiltert (posi:longint):integer; virtual;
         constructor load (var s:tbufstream);
         procedure store (var s:tbufstream);
       private
         anz:word;
         abstli,abstre:integer;
         ms:extended;
         abli,abre:extended;
         end;

var   grund:grundlistetyp;
      belegungsliste:array[0..maxkanal-1] of belegung;
      schriftliste:schriftlistentyp;

procedure neukan (kanaele:byte);
procedure beschriftungen (var ko:kopfdaten);

procedure kanalsetz (k,vonk:byte);
procedure filtersetz (hilf:filterzeiger; k:byte);
procedure filterloesch (k:byte);
function filterdrin(k:byte):boolean;
function filterzeile (k:byte):string;

procedure einheitensetzen (frequenz:extended);

procedure oeffne (name:string80; frequenz:extended);

function dat (posi:longint; k:byte):integer;

function extspannung (y:extended; kanal:byte):extended;
function spannung (y:extended; kanal:byte):longint;

function norm (sp:extended; kanal:byte) :extended;

procedure streamput (var s:tbufstream);
procedure streamget (var s:tbufstream);

implementation

type  endezg=^ende;
      ende=object (filter)
         ka:byte;
         constructor neu (k:byte);
         procedure einheitveraendern (var  beleg:belegung); virtual;
         function gefiltert (posi:longint):integer; virtual;
         constructor load (var s:tbufstream);
         procedure store (var s:tbufstream);
         end;

const defaulteinheit:grundtyp=
         (faktor:10/2048; vor:''; anfang:'V'; sekunde:0; gain:1; multi:1);
      vz:array[boolean] of string[1]=('','+');

      rende       :tstreamrec=(objtype:300;           vmtlink:ofs(typeof(ende)^);
                               load:@ende.load;       store:@ende.store);
      rinvert     :tstreamrec=(objtype:301;           vmtlink:ofs(typeof(invert)^);
                               load:@invert.load;     store:@filter.store);
      roffset     :tstreamrec=(objtype:302;           vmtlink:ofs(typeof(offset)^);
                               load:@offset.load;     store:@offset.store);
      rmalfaktor  :tstreamrec=(objtype:303;           vmtlink:ofs(typeof(malfaktor)^);
                               load:@malfaktor.load;  store:@malfaktor.store);
      rkappen     :tstreamrec=(objtype:304;           vmtlink:ofs(typeof(kappen)^);
                               load:@kappen.load;     store:@filter.store);
      rabsolut    :tstreamrec=(objtype:305;           vmtlink:ofs(typeof(absolut)^);
                               load:@absolut.load;    store:@filter.store);
      rsquare     :tstreamrec=(objtype:306;           vmtlink:ofs(typeof(square)^);
                               load:@square.load;     store:@filter.store);
      rglatt      :tstreamrec=(objtype:307;           vmtlink:ofs(typeof(glatt)^);
                               load:@glatt.load;      store:@glatt.store);
      rtiefpass   :tstreamrec=(objtype:308;           vmtlink:ofs(typeof(tiefpass)^);
                               load:@tiefpass.load;   store:@passfilter.store);
      rhochpass   :tstreamrec=(objtype:309;           vmtlink:ofs(typeof(hochpass)^);
                               load:@hochpass.load;   store:@passfilter.store);
      rdiff       :tstreamrec=(objtype:310;           vmtlink:ofs(typeof(diff)^);
                               load:@diff.load;       store:@filter.store);
      rverschiebe :tstreamrec=(objtype:311;           vmtlink:ofs(typeof(verschiebe)^);
                               load:@verschiebe.load; store:@verschiebe.store);
      rmaxmin     :tstreamrec=(objtype:312;           vmtlink:ofs(typeof(maxmin)^);
                               load:@maxmin.load;     store:@maxmin.store);
      rspikefilter:tstreamrec=(objtype:313;           vmtlink:ofs(typeof(spikefilter)^);
                               load:@spikefilter.load;store:@spikefilter.store);

var   filteranfang:array[1..maxkanal-1] of filterzeiger;
      kan:byte;
      fre:extended;
      diffaktor:extended;
      i:word;

procedure einheittyp.kopie (var ein:einheittyp);
begin
self:=ein;
end;

function einheittyp.einhwort:einheitstring;
const liste:array [-2..2] of string[3]=('/s'#253,'/s','','s','s'#253);
begin
case sekunde of
   -2..2:einhwort:=vor+anfang+liste[sekunde];
   else if sekunde<-2 then einhwort:=vor+anfang+'/s^'+bequem.wort(-sekunde)
                      else einhwort:=vor+anfang+'s^'+bequem.wort(sekunde);
   end;
end;

procedure einheittyp.handlich;
var   n:integer;
procedure setz (mal:extended; davor:char);
begin
faktor:=faktor*mal; vor:=davor;
end;
begin
n:=trunc(log(faktor*2048*1.00001));
case n of
  -11,-10, -9:setz(1E12,'p');
   -8, -7, -6:setz( 1E9,'n');
   -5, -4, -3:setz( 1E6,'æ');
   -2, -1,  0:setz( 1E3,'m');
    1,  2,  3:;
    4,  5,  6:setz(1E-3,'k');
    7,  8,  9:setz(1E-6,'M');
   10, 11, 12:setz(1E-9,'G');
  else schwierig;
  end;
end;

procedure einheittyp.schwierig;
var   n:longint;
begin
n:=pred(trunc(log(faktor*2048*1.00001))); if n<0 then dec(n,2);
n:=3*(n div 3);
if n>0 then faktor:=faktor/xpot(n) else faktor:=faktor*xpot(-n);
anfang:='1E'+wort(n); sekunde:=0;
end;

function einheittyp.plot:string80;
var   kom:string80;
      p:byte;
begin
kom:='LB'+vor+anfang;
p:=pos(#253,kom);
while p>0 do begin
   delete(kom,p,1); insert(#3'CP0,0.3;LB2'#3'CP0,-0.3;LB',kom,p);
   p:=pos(#253,kom);
   end;
if sekunde=0 then kom:=kom+#3
             else begin
   if sekunde<0 then kom:=kom+'/';
   kom:=kom+'s'#3;
   if abs(sekunde)>1 then kom:=kom+'CP0,0.3;LB'+bequem.wort(abs(sekunde))+#3'CP0,-0.3;';
   end;
plot:=kom;
end;

procedure grundtyp.setz (m:extended; anf:einheitstring);
var   len:byte absolute anf; pos:byte;
begin
kompri(anf); pos:=len;
if len>=1 then begin
   sekunde:=1;
   case anf[len] of
      '0'..'9':if len>=3 then if anf[pred(len)]='^' then begin
         sekunde:=zahl(anf[len]); dec(pos,2) end;
      #253:if len>=2 then begin sekunde:=2; dec(pos) end;
      end;
   if anf[pos]='s' then begin
      len:=pred(pos);
      if len>=1 then case anf[len] of
         '/':begin dec(len); sekunde:=-sekunde end;
         '*':dec(len);
         end;
      end          else sekunde:=0;
   end    else sekunde:=0;
anfang:=anf;
multi:=m;
faktor:=10/gain*multi/2048;
end;

procedure belegung.grundsetzen (k:byte);
begin
kopie(grund[k]); negativ:=true; quadratda:=false;
end;

procedure filter.load;
begin
s.read(name,sizeof(name));
next:=filterzeiger(s.get);
end;

procedure filter.store;
begin
s.write(name,sizeof(name));
s.put(next);
end;

destructor filter.alt;
begin end;

procedure filter.einheitveraendern (var beleg:belegung);
begin end;

procedure filter.vorbereitung;
begin end;

function filter.gefiltert (posi:longint):integer;
begin end;

constructor ende.neu (k:byte);
begin
ka:=k; next:=nil; name:='#'+wort(k);
end;

constructor ende.load (var s:tbufstream);
begin
filter.load(s);
s.read(ka,sizeof(byte));
end;

procedure ende.store (var s:tbufstream);
begin
filter.store(s);
s.write(ka,sizeof(byte));
end;

procedure ende.einheitveraendern (var beleg:belegung);
begin
beleg.grundsetzen(ka);
end;

function ende.gefiltert (posi:longint):integer;
begin
gefiltert:=lese(posi,ka)-2048;
end;

constructor invert.neu;
begin
name:='*(-1)';
end;

constructor invert.load (var s:tbufstream);
begin
filter.load(s);
end;

function invert.gefiltert (posi:longint):integer;
begin
gefiltert:=-next^.gefiltert(posi);
end;

constructor offset.neu (k:byte; hoch:integer);
begin
ho:=hoch;
name:='Offset '+vz[ho>=0]+extwort(ho*belegungsliste[k].faktor,3,2)
      +belegungsliste[k].einhwort;
end;

constructor offset.load (var s:tbufstream);
begin
filter.load(s);
s.read(ho,sizeof(integer));
end;

procedure offset.store (var s:tbufstream);
begin
filter.store(s);
s.write(ho,sizeof(integer));
end;

function offset.gefiltert (posi:longint):integer;
begin
gefiltert:=next^.gefiltert(posi)+ho;
end;

constructor malfaktor.neu (malfak:extended);
begin
float:=malfak;
name:='*'+extwort(malfak,3,2);
end;

constructor malfaktor.load (var s:tbufstream);
begin
filter.load(s);
s.read(float,sizeof(extended));
end;

procedure malfaktor.store (var s:tbufstream);
begin
filter.store(s);
s.write(float,sizeof(extended));
end;

function malfaktor.gefiltert (posi:longint):integer;
begin
gefiltert:=round(next^.gefiltert(posi)*float);
end;

constructor kappen.neu;
begin
name:='Kappen';
end;

constructor kappen.load (var s:tbufstream);
begin
filter.load(s);
end;

function kappen.gefiltert(posi:longint):integer;
var   wert:integer;
begin
wert:=next^.gefiltert(posi);
if wert>2048 then wert:=2048 else if wert<-2048 then wert:=-2048;
gefiltert:=wert;
end;

constructor absolut.neu;
begin
name:='³y³';
end;

constructor absolut.load (var s:tbufstream);
begin
filter.load(s);
end;

procedure absolut.einheitveraendern (var beleg:belegung);
begin
beleg.negativ:=false;
end;

function absolut.gefiltert (posi:longint):integer;
begin
gefiltert:=abs(next^.gefiltert(posi));
end;

constructor square.neu;
begin
name:='yý';
end;

constructor square.load (var s:tbufstream);
begin
filter.load(s);
end;

procedure square.einheitveraendern (var beleg:belegung);
begin
with beleg do begin
   negativ:=false; faktor:=sqr(faktor)*2084; quadratda:=true end;
end;

function square.gefiltert (posi:longint):integer;
var xs:longint;
begin
xs:=next^.gefiltert(posi);
gefiltert:=sqr(xs) div 2048;
end;

constructor glatt.neu (breims:extended);
begin
brei:=breims; name:='Gl„tten '#29+extwort(brei,3,1)+'ms';
end;

constructor glatt.load (var s:tbufstream);
begin
filter.load(s);
s.read(brei,sizeof(extended));
end;

procedure glatt.store (var s:tbufstream);
begin
filter.store(s);
s.write(brei,sizeof(extended));
end;

procedure glatt.vorbereitung (frequenz:extended);
begin
breianz2:=round(brei*frequenz/2000); breianz:=2*breianz2+1;
end;

function glatt.gefiltert (posi:longint):integer;
var   sum,i:longint;
begin
sum:=0; for i:=posi-breianz2 to posi+breianz2 do inc(sum,next^.gefiltert(i));
gefiltert:=round(sum/breianz);
end;

procedure passfilter.neu (grenzfreq:longint);
begin
gr:=grenzfreq; name:=wort(gr)+'Hz'; new(spaltgr);
end;

procedure passfilter.load (var s:tbufstream);
begin
filter.load(s);
s.read(gr,sizeof(gr)); new(spaltgr);
end;

procedure passfilter.store (var s:tbufstream);
begin
filter.store(s);
s.write(gr,sizeof(gr));
end;

function passfilter.gefiltert (posi:longint):integer;
var   i:-weite..weite;  sum:extended;
begin
sum:=0;
for i:=we downto -we do sum:=sum+next^.gefiltert(posi-i)*spaltgr^[abs(i)];
gefiltert:=round(sum);
end;

destructor passfilter.alt;
begin
dispose(spaltgr);
end;

constructor tiefpass.neu (grenzfreq:longint);
begin
passfilter.neu(grenzfreq);
name:='Tp '+name;
end;

constructor tiefpass.load (var s:tbufstream);
begin
passfilter.load(s);
end;

procedure tiefpass.vorbereitung (frequenz:extended);
var   fak:extended;
      j:integer;
begin
fak:=gr/frequenz; we:=min(round(genau*pi/fak),weite);
spaltgr^[0]:=fak/pi; for j:=1 to we do spaltgr^[j]:=sin(j*fak)/j/pi;
end;

constructor hochpass.neu (grenzfreq:longint);
begin
passfilter.neu(grenzfreq);
name:='Hp '+name;
end;

constructor hochpass.load (var s:tbufstream);
begin
passfilter.load(s);
end;

procedure hochpass.vorbereitung (frequenz:extended);
var   fak:extended;
      j:integer;
begin
fak:=gr/frequenz; we:=min(round(genau*pi/fak),weite);
spaltgr^[0]:=1-fak/pi; for j:=1 to we do spaltgr^[j]:=-sin(j*fak)/j/pi;
end;

procedure hochpass.einheitveraendern (var beleg:belegung);
begin
beleg.negativ:=true;
end;

constructor diff.neu;
begin
name:='dy/dt';
end;

constructor diff.load (var s:tbufstream);
begin
filter.load(s);
end;

procedure diff.einheitveraendern (var beleg:belegung);
begin
with beleg do begin faktor:=faktor*fre; dec(sekunde) end;
beleg.negativ:=true;
end;

procedure diff.vorbereitung (frequenz:extended);
begin end;

function diff.gefiltert (posi:longint):integer;
begin
gefiltert:=round((next^.gefiltert(posi+1)-next^.gefiltert(posi-1))*diffaktor);
end;

constructor verschiebe.neu (umms:extended);
begin
um:=umms; name:=vz[um>=0]+extwort(um,2,1)+'ms';
end;

constructor verschiebe.load (var s:tbufstream);
begin
filter.load(s);
s.read(um,sizeof(extended));
end;

procedure verschiebe.store (var s:tbufstream);
begin
filter.store(s);
s.write(um,sizeof(extended));
end;

procedure verschiebe.vorbereitung (frequenz:extended);
begin
posid:=round(um*frequenz/1000);
end;

function verschiebe.gefiltert (posi:longint):integer;
begin
gefiltert:=next^.gefiltert(posi+posid);
end;

constructor maxmin.neu (breims:extended);
begin
brei:=breims;
name:='Max-Min '#29+extwort(brei,2,1)+'ms';
end;

constructor maxmin.load (var s:tbufstream);
begin
filter.load(s);
s.read(brei,sizeof(extended));
end;

procedure maxmin.store (var s:tbufstream);
begin
filter.store(s);
s.write(brei,sizeof(extended));
end;

procedure maxmin.einheitveraendern (var beleg:belegung);
begin
beleg.faktor:=beleg.faktor*2; beleg.negativ:=false;
end;

procedure maxmin.vorbereitung (frequenz:extended);
begin
br2:=round(brei*frequenz/2000);
end;

function maxmin.gefiltert (posi:longint):integer;
var   i:longint;
      wert,wertmin,wertmax:integer;
begin
wertmax:=-2049; wertmin:=2049;
for i:=posi-br2 to posi+br2 do begin
   wert:=next^.gefiltert(i);
   if wert>wertmax then wertmax:=wert;
   if wert<wertmin then wertmin:=wert;
   end;
gefiltert:=(wertmax-wertmin) div 2;
end;

constructor spikefilter.neu (k:byte; millisek,ablinks,abrechts:extended);
var    ableitungein:einheittyp;
begin
ms:=millisek;
ableitungein:=belegungsliste[k];
dec(ableitungein.sekunde);  ableitungein.faktor:=ableitungein.faktor*fre;
abli:=ablinks/ableitungein.faktor; abre:=abrechts/ableitungein.faktor;
name:='Spike '#29+extwort(ms,3,1)+'ms '#24+extwort(ablinks,3,0)
      +ableitungein.einhwort+' '+#25+extwort(abrechts,3,0)
      +ableitungein.einhwort;
end;

constructor spikefilter.load (var s:tbufstream);
begin
filter.load(s);
s.read(ms,sizeof(extended));
s.read(abli,sizeof(extended)); s.read(abre,sizeof(extended));
end;

procedure spikefilter.store (var s:tbufstream);
begin
filter.store(s);
s.write(ms,sizeof(extended));
s.write(abli,sizeof(extended)); s.write(abre,sizeof(extended));
end;

procedure spikefilter.vorbereitung (frequenz:extended);
begin
abstli:=round(abli*fre/frequenz);
abstre:=round(abre*fre/frequenz);
anz:=min(round(ms*frequenz/1000),spikemax);
end;

function spikefilter.gefiltert (posi:longint):integer;
label weiter;
var   bei,bei2,bis:integer;
      mitte:array[-spikemax..spikemax] of integer;
begin
for bei:=-anz to anz do mitte[bei]:=next^.gefiltert(posi+bei);
bei:=0;
while mitte[bei]-mitte[bei-1]<abstli do begin
    dec(bei); if bei-1<-anz then goto weiter end;
bei2:=bei;
repeat
   dec(bei2); if bei2-1<-anz then goto weiter;
until mitte[bei2]-mitte[bei2-1]<abstli;
bis:=bei2+anz;
repeat
   inc(bei); if bei>bis then goto weiter;
until mitte[bei]-mitte[bei-1]<=-abstre;
repeat
   inc(bei); if bei>bis then goto weiter;
until mitte[bei]-mitte[bei-1]>-abstre;
if bei<=0 then goto weiter;
gefiltert:=round(mitte[bei2]+(mitte[bei-1]-mitte[bei2])/(bei2-bei-1.0)*bei2);
exit;

weiter:gefiltert:=mitte[0];
end;

procedure neukan (kanaele:byte);
var   i:byte;
begin
kan:=kanaele;
for i:=1 to maxkanal-1 do filterloesch(i);
end;

procedure beschriftungen (var ko:kopfdaten);
begin
for i:=0 to kan-1 do begin
   if ko.gain[i]<>0 then begin grund[i].gain:=ko.gain[i]; grund[i].faktor:=10/ko.gain[i]/2048 end;
   schriftliste[i]:=ko.names[i];
   grund[i]:=defaulteinheit;
   end;
for i:=kan to kan+filtermax-1 do schriftliste[i]:=schriftliste[i mod kan];
schriftliste[maxkanal]:='- ';
end;

procedure kanalsetz (k,vonk:byte);
begin
filterloesch(k);
endezg(filteranfang[k])^.ka:=vonk;
schriftliste[k]:=schriftliste[vonk];
end;

procedure filtersetz (hilf:filterzeiger; k:byte);
begin
hilf^.next:=filteranfang[k]; filteranfang[k]:=hilf;
end;

procedure filterloesch (k:byte);
var   hilf:filterzeiger;
begin
hilf:=filteranfang[k];
while hilf^.next<>nil do begin
   filteranfang[k]:=filteranfang[k]^.next;
   dispose(hilf,alt);
   hilf:=filteranfang[k] end;
end;

function filterdrin (k:byte):boolean;
begin
filterdrin:=typeof(filteranfang[k]^)<>typeof(ende);
end;

function filterzeile (k:byte):string;
var puffer:string;
    bei:filterzeiger;
begin
bei:=filteranfang[k]; puffer:='';
while bei<>nil do begin insert(bei^.name+' '#26' ',puffer,0); bei:=bei^.next end;
filterzeile:=+puffer+'#'+wort(k);
end;

procedure einheitensetzen (frequenz:extended);
var   i:word;
      hilf:filterzeiger;
   procedure rekurs (hilf:filterzeiger);
   begin
   if hilf^.next<>nil then rekurs(hilf^.next);
   hilf^.einheitveraendern(belegungsliste[i]);
   end;

begin
fre:=frequenz;
for i:=0 to kan-1 do with belegungsliste[i] do begin grundsetzen(i); handlich end;
for i:=kan to kan+filtermax-1 do with belegungsliste[i] do begin
   rekurs(filteranfang[i]);
   if quadratda then schwierig
                else handlich;
   end;
end;

procedure oeffne (name:string80; frequenz:extended);
var   i:longint;
      hilf:filterzeiger;
begin
tulab.oeffne(name);
diffaktor:=frequenz/fre/2;
for i:=kan to kan+filtermax-1 do begin
   hilf:=filteranfang[i];
   while hilf^.next<>nil do begin
      hilf^.vorbereitung(frequenz);
      hilf:=hilf^.next end;
   end;
end;

function dat (posi:longint; k:byte):integer;
begin
if k<kan then dat:=lese(posi,k)
         else dat:=filteranfang[k]^.gefiltert(posi)+2048;
end;

function extspannung (y:extended; kanal:byte):extended;
begin
extspannung:=(y-2048)*belegungsliste[kanal].faktor
end;

function spannung (y:extended; kanal:byte):longint;
begin
spannung:=round(extspannung(y,kanal));
end;

function norm (sp:extended; kanal:byte) :extended;
begin
norm:=sp/belegungsliste[kanal].faktor+2048;
end;

procedure streamput (var s:tbufstream);
var  i:byte;
begin
s.write(kan,sizeof(kan)); s.write(fre,sizeof(fre));
s.write(grund,sizeof(grund)); s.write(schriftliste,sizeof(schriftliste));
for i:=kan to kan+filtermax-1 do s.put(filteranfang[i]);
end;

procedure streamget (var s:tbufstream);
var  i:byte;
begin
s.read(kan,sizeof(kan)); s.read(fre,sizeof(fre));
s.read(grund,sizeof(grund)); s.read(schriftliste,sizeof(schriftliste));
for i:=kan to kan+filtermax-1 do begin
   filterloesch(i); dispose(filteranfang[i],alt);
   filteranfang[i]:=filterzeiger(s.get) end;
einheitensetzen(fre);
end;

begin
registertype(rende);         registertype(rinvert);        registertype(roffset);
registertype(rmalfaktor);    registertype(rkappen);        registertype(rabsolut);
registertype(rsquare);       registertype(rglatt);         registertype(rtiefpass);
registertype(rhochpass);     registertype(rdiff);          registertype(rverschiebe);
registertype(rmaxmin);       registertype(rspikefilter);
for i:=1 to maxkanal-1 do filteranfang[i]:=new(endezg,neu(0));
end.