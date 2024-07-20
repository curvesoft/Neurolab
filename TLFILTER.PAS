{ Borland-Pascal 7.0 }

unit  tlfilter;

{$IFDEF MSDOS}
{$A+,B-,E+,F-,G-,I-,N+,O-,P+,T+,V+,X-}
{$ELSE}
{$A+,B-,E+,F-,G+,I-,N+,P+,T+,V+,X-}
{$ENDIF}

interface

uses  objects, plotter, bequem, daff, tulab42;

const filtermax=16;
      maxkanal=maxkan+filtermax;

      genau=4; weite=2000;

      spikemax=30;

type  { Einheiten der Y-Achsen }
      einheitstring=string[7];
      einheittyp=object
         faktor:extended; vor:string[1]; anfang:string[7]; sekunde:integer;
         procedure kopie(var ein:einheittyp);
         function einhwort:einheitstring;
         function plot:string80;
         procedure handlich;
         procedure schwierig;
         end;

      { Rohform der Einheiten der ungefilterten Daten }
      grundtyp=object (einheittyp)
         gain, multi : extended;
         procedure setz (m:extended; anf:einheitstring);
         end;
      grundlistetyp=array[0..maxkan-1] of grundtyp;

      { Endform der Einheiten der gefilterten Daten }
      belegung=object (einheittyp)
         negativ, schwierigda : boolean;
         procedure grundsetzen (k:byte);
         end;

      { Beschriftungen der Y-Achsen }
      schriftlistentyp=array[0..maxkanal] of string[10];

      { Abstraktes Filterobjekt }
      filterzeiger=^filter;
      filter=object (tobject)
         name:string80;
         next:filterzeiger;
         destructor alt; virtual;
         procedure einheitgenerieren (var  beleg:belegung); virtual;
         procedure vorbereitung (frequenz:extended); virtual;
         function gefiltert (posi:longint):sample; virtual;
         constructor load (var s:tbufstream);
         procedure store (var s:tbufstream);
         end;

      { abstakter Filter mit Breite }
      breitefilter=object (filter)
       public
         procedure neu (breims:extended);
       private
         brei:extended;
         breianz,breianz2:longint;
         procedure vorbereitung (frequenz:extended); virtual;
         constructor load (var s:tbufstream);
         procedure store (var s:tbufstream);
         end;

      { arcsin - Filter }
      arcsinzg=^arcsin;
      arcsin=object (filter)
       public
         constructor neu;
       private
         procedure einheitgenerieren (var  beleg:belegung); virtual;
         function gefiltert (posi:longint):sample; virtual;
         end;

      { arccos - Filter }
      arccoszg=^arccos;
      arccos=object (filter)
       public
         constructor neu;
       private
         procedure einheitgenerieren (var  beleg:belegung); virtual;
         function gefiltert (posi:longint):sample; virtual;
         end;

      { *(-1) - Filter }
      invertzg=^invert;
      invert=object (filter)
       public
         constructor neu;
       private
         procedure einheitgenerieren (var  beleg:belegung); virtual;
         function gefiltert (posi:longint):sample; virtual;
         end;

      { 1/X - Filter}
      einsdurchzg=^einsdurch;
      einsdurch=object (filter)
       public
         constructor neu;
       private
         procedure einheitgenerieren (var  beleg:belegung); virtual;
         function gefiltert (posi:longint):sample; virtual;
         end;

      { +/- Offset - Filter }
      offsetzg=^offset;
      offset=object (filter)
       public
         constructor neu (k:byte; hoch:sample);
       private
         ho:sample;
         function gefiltert (posi:longint):sample; virtual;
         constructor load (var s:tbufstream);
         procedure store (var s:tbufstream);
         end;

      { *m (Verst„rkungs-) - Filter }
      malfaktorzg=^malfaktor;
      malfaktor=object (filter)
       public
         constructor neu (malfak:extended);
       private
         float:extended;
         function gefiltert (posi:longint):sample; virtual;
         constructor load (var s:tbufstream);
         procedure store (var s:tbufstream);
         end;

      { y-Achsenstreckungs - Filter }
      streckungzg=^streckung;
      streckung=object (filter)
       public
         constructor neu (malfak:extended);
       private
         float:extended;
         procedure einheitgenerieren (var beleg:belegung); virtual;
         function gefiltert (posi:longint):sample; virtual;
         constructor load (var s:tbufstream);
         procedure store (var s:tbufstream);
         end;

      { Kappung aller Werte < minsample und > maxsample }
      kappenzg=^kappen;
      kappen=object (filter)
       public
         constructor neu;
       private
         function gefiltert (posi:longint):sample; virtual;
         end;

      { Additionsfilter }
      additionzg=^addition;
      addition=object (filter)
       public
         constructor neu (additionskanal:byte);
       private
         adk:byte;
         procedure einheitgenerieren (var beleg:belegung); virtual;
         function gefiltert (posi:longint):sample; virtual;
         constructor load (var s:tbufstream);
         procedure store (var s:tbufstream);
         end;

      { Absolutbetrags - Filter }
      absolutzg=^absolut;
      absolut=object (filter)
       public
         constructor neu;
       private
         procedure einheitgenerieren (var beleg:belegung); virtual;
         function gefiltert (posi:longint):sample; virtual;
         end;

      { Quadrat - Filter }
      squarezg=^square;
      square=object (absolut)
       public
         constructor neu;
       private
         procedure einheitgenerieren (var beleg:belegung); virtual;
         function gefiltert (posi:longint):sample; virtual;
         end;

      { Gl„ttungs - Filter }
      glattzg=^glatt;
      glatt=object (breitefilter)
       public
         constructor neu (breims:extended);
       private
         function gefiltert (posi:longint):sample; virtual;
         end;

      { abstrakter Passfilter }
      spaltzeiger=^spaltfeld;
      spaltfeld=array[0..weite] of single;
      passfilter=object (filter)
       private
         gr:longint;
         spaltgr:spaltzeiger;
         we:word;
         procedure neu (grenzfreq:longint);
         destructor alt; virtual;
         function gefiltert (posi:longint):sample; virtual;
         constructor load (var s:tbufstream);
         procedure store (var s:tbufstream);
         end;
      tiefpasszg=^tiefpass;
      tiefpass=object (passfilter)
       public
         constructor neu (grenzfreq:longint);
       private
         procedure vorbereitung (frequenz:extended); virtual;
         end;
      hochpasszg=^hochpass;
      hochpass=object (passfilter)
       public
         constructor neu (grenzfreq:longint);
       private
         procedure vorbereitung (frequenz:extended); virtual;
         procedure einheitgenerieren (var beleg:belegung); virtual;
         end;

      { Differenzierungs- Filter }
      diffzg=^diff;
      diff=object (filter)
       public
         constructor neu;
       private
         procedure einheitgenerieren (var beleg:belegung); virtual;
         procedure vorbereitung (frequenz:extended); virtual;
         function gefiltert (posi:longint):sample; virtual;
         end;

      { Integrations- Filter }
      intzg=^int;
      int=object (filter)
       public
         constructor neu;
       private
         gefiltertwert:sample;
         posiwert:longint;
         procedure einheitgenerieren (var beleg:belegung); virtual;
         procedure vorbereitung (frequenz:extended); virtual;
         function gefiltert (posi:longint):sample; virtual;
         end;

      { gleitender Integrations- Filter }
      glintzg=^glint;
      glint=object (breitefilter)
       public
         constructor neu (breims:extended);
       private
         gefiltertwert:sample;
         posiwert:longint;
         procedure einheitgenerieren (var beleg:belegung); virtual;
         procedure vorbereitung (frequenz:extended); virtual;
         function gefiltert (posi:longint):sample; virtual;
         end;

      { gleitender Linienzug- Filter }
      gllinzg=^gllin;
      gllin=object (breitefilter)
       public
         constructor neu (breims:extended);
       private
         gefiltertwert:sample;
         posiwert:longint;
         procedure einheitgenerieren (var beleg:belegung); virtual;
         procedure vorbereitung (frequenz:extended); virtual;
         function gefiltert (posi:longint):sample; virtual;
         end;

     { X-Achsen-Verschiebungs - Filter }
      verschiebezg=^verschiebe;
      verschiebe=object (filter)
       public
         constructor neu (umms:extended);
       private
         um:extended;
         posid:longint;
         procedure vorbereitung (frequenz:extended); virtual;
         function gefiltert (posi:longint):sample; virtual;
         constructor load (var s:tbufstream);
         procedure store (var s:tbufstream);
         end;

      { Maximum-Minimum-Differenz - Filter }
      maxminzg=^maxmin;
      maxmin=object (filter)
       public
         constructor neu (breims:extended);
       private
         brei:extended;
         br2:longint;
         procedure einheitgenerieren (var beleg:belegung); virtual;
         procedure vorbereitung (frequenz:extended); virtual;
         function gefiltert (posi:longint):sample; virtual;
         constructor load (var s:tbufstream);
         procedure store (var s:tbufstream);
         end;

      { Spike - Filter }
      spikefilterzg=^spikefilter;
      spikefilter=object (filter)
       public
         constructor neu (k:byte; millisek,ablinks,abrechts:extended);
       private
         anz:word;
         abstli,abstre:integer;
         ms:extended;
         abli,abre:extended;
         procedure vorbereitung (frequenz:extended); virtual;
         function gefiltert (posi:longint):sample; virtual;
         constructor load (var s:tbufstream);
         procedure store (var s:tbufstream);
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

procedure oeffne (name:string80; var ko:kopfdaten);

function dat (posi:longint; k:byte):sample;

function extspannung (y:extended; kanal:byte):extended;
function spannung (y:extended; kanal:byte):longint;

function norm (sp:extended; kanal:byte) :extended;

procedure streamput (var s:tbufstream);
procedure streamget (var s:tbufstream);

implementation

type  { Ende der Filterkette }
      endezg=^ende;
      ende=object (filter)
         ka:byte;
         constructor neu (k:byte);
         procedure einheitgenerieren (var  beleg:belegung); virtual;
         function gefiltert (posi:longint):sample; virtual;
         constructor load (var s:tbufstream);
         procedure store (var s:tbufstream);
         end;
      weiterzg=^weiter;
      weiter=object (ende)
         procedure einheitgenerieren (var  beleg:belegung); virtual;
         function gefiltert (posi:longint):sample; virtual;
         end;

const defaulteinheit:grundtyp=
         (faktor:10/maxsample; vor:''; anfang:'V'; sekunde:0; gain:1; multi:1);
      vz:array[boolean] of string[1]=('','+');

      rende       :tstreamrec=(objtype:300;           vmtlink:ofs(typeof(ende)^);
                               load:@ende.load;       store:@ende.store);
      rweiter     :tstreamrec=(objtype:399;           vmtlink:ofs(typeof(weiter)^);
                               load:@ende.load;       store:@ende.store);
      rinvert     :tstreamrec=(objtype:301;           vmtlink:ofs(typeof(invert)^);
                               load:@filter.load;     store:@filter.store);
      roffset     :tstreamrec=(objtype:302;           vmtlink:ofs(typeof(offset)^);
                               load:@offset.load;     store:@offset.store);
      rmalfaktor  :tstreamrec=(objtype:303;           vmtlink:ofs(typeof(malfaktor)^);
                               load:@malfaktor.load;  store:@malfaktor.store);
      rkappen     :tstreamrec=(objtype:304;           vmtlink:ofs(typeof(kappen)^);
                               load:@filter.load;     store:@filter.store);
      rabsolut    :tstreamrec=(objtype:305;           vmtlink:ofs(typeof(absolut)^);
                               load:@filter.load;     store:@filter.store);
      rsquare     :tstreamrec=(objtype:306;           vmtlink:ofs(typeof(square)^);
                               load:@filter.load;     store:@filter.store);
      rglatt      :tstreamrec=(objtype:307;           vmtlink:ofs(typeof(glatt)^);
                               load:@breitefilter.load;store:@breitefilter.store);
      rtiefpass   :tstreamrec=(objtype:308;           vmtlink:ofs(typeof(tiefpass)^);
                               load:@passfilter.load; store:@passfilter.store);
      rhochpass   :tstreamrec=(objtype:309;           vmtlink:ofs(typeof(hochpass)^);
                               load:@passfilter.load; store:@passfilter.store);
      rdiff       :tstreamrec=(objtype:310;           vmtlink:ofs(typeof(diff)^);
                               load:@filter.load;     store:@filter.store);
      rverschiebe :tstreamrec=(objtype:311;           vmtlink:ofs(typeof(verschiebe)^);
                               load:@verschiebe.load; store:@verschiebe.store);
      rmaxmin     :tstreamrec=(objtype:312;           vmtlink:ofs(typeof(maxmin)^);
                               load:@maxmin.load;     store:@maxmin.store);
      rspikefilter:tstreamrec=(objtype:313;           vmtlink:ofs(typeof(spikefilter)^);
                               load:@spikefilter.load;store:@spikefilter.store);
      rstreckung  :tstreamrec=(objtype:314;           vmtlink:ofs(typeof(streckung)^);
                               load:@streckung.load;  store:@streckung.store);
      reinsdurch  :tstreamrec=(objtype:315;           vmtlink:ofs(typeof(einsdurch)^);
                               load:@filter.load;     store:@filter.store);
      raddition   :tstreamrec=(objtype:316;           vmtlink:ofs(typeof(addition)^);
                               load:@addition.load;   store:@addition.store);
      rint        :tstreamrec=(objtype:317;           vmtlink:ofs(typeof(int)^);
                               load:@filter.load;     store:@filter.store);
      rglint      :tstreamrec=(objtype:318;           vmtlink:ofs(typeof(glint)^);
                               load:@breitefilter.load;store:@breitefilter.store);
      rgllin      :tstreamrec=(objtype:319;           vmtlink:ofs(typeof(gllin)^);
                               load:@breitefilter.load;store:@breitefilter.store);
      rarcsin     :tstreamrec=(objtype:320;           vmtlink:ofs(typeof(arcsin)^);
                               load:@filter.load;     store:@filter.store);
      rarccos     :tstreamrec=(objtype:321;           vmtlink:ofs(typeof(arccos)^);
                               load:@filter.load;     store:@filter.store);

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
if (anfang='1') and (sekunde=-1) then einhwort:=vor+'Hz'
                                 else
 if (vor='') and (anfang='1') and (sekunde=0) then einhwort:=''
                                              else
   case sekunde of
      -2..2:einhwort:=vor+anfang+liste[sekunde];
      else if sekunde<-2 then einhwort:=vor+anfang+'/s^'+bequem.wort(-sekunde)
                         else einhwort:=vor+anfang+'s^'+bequem.wort(sekunde);
      end;
end;

function einheittyp.plot:string80;
var   kom:string80;
      p:byte;
begin
if (vor='') and (anfang='1') and (sekunde=0) then kom:=''
                                              else begin
   if vor='æ'then kom:=plmu
             else kom:='LB'+vor+#3;
   if (anfang='1') and (sekunde=-1) then kom:=kom+'LBHz'#3
                                    else begin

      kom:=kom+'LB'+anfang;
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
      end;
   end;
plot:=kom;
end;

procedure einheittyp.handlich;
type  string1=string[1];
var   n:integer;
procedure setz (mal:extended; davor:string1);
begin
faktor:=faktor*mal; vor:=davor;
end;
begin
if (sekunde=0) and (anfang='') then schwierig
                               else begin
   n:=round(log(faktor*maxsample*1.00001)-0.5);
   case n-1 of
     -11,-10, -9:setz(1E12,'p');
      -8, -7, -6:setz( 1E9,'n');
      -5, -4, -3:setz( 1E6,'æ');
      -2, -1,  0:setz( 1E3,'m');
       1,  2,  3:setz( 1E0,'');
       4,  5,  6:setz(1E-3,'k');
       7,  8,  9:setz(1E-6,'M');
      10, 11, 12:setz(1E-9,'G');
     else schwierig;
     end;
   if length(einhwort)>5 then schwierig;
   end;
end;

procedure einheittyp.schwierig;
var   n:longint;
begin
n:=pred(trunc(log(faktor*maxsample*1.00001))); if n<0 then dec(n,2);
n:=3*(n div 3);
if n>0 then faktor:=faktor/xpot(n) else faktor:=faktor*xpot(-n);
anfang:='1E'+wort(n); sekunde:=0;
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
anfang:=copy(anf,1,5);
multi:=m;
faktor:=multi*gain;
end;

procedure belegung.grundsetzen (k:byte);
begin
kopie(grund[k]); negativ:=true; schwierigda:=false;
end;

{ filter }

constructor filter.load;
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

procedure filter.einheitgenerieren (var beleg:belegung);
begin
next^.einheitgenerieren(beleg);
end;

procedure filter.vorbereitung;
begin end;

function filter.gefiltert (posi:longint):sample;
begin end;

{ ende }

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

procedure ende.einheitgenerieren (var beleg:belegung);
begin
beleg.grundsetzen(ka)
end;

function ende.gefiltert (posi:longint):sample;
begin
gefiltert:=lese(posi,ka);
end;

{ weiter }

procedure weiter.einheitgenerieren (var beleg:belegung);
begin
filteranfang[ka]^.einheitgenerieren(beleg);
end;

function weiter.gefiltert (posi:longint):sample;
begin
gefiltert:=filteranfang[ka]^.gefiltert(posi);
end;

{ breitefilter }

procedure breitefilter.neu (breims:extended);
begin
brei:=breims;
end;

constructor breitefilter.load (var s:tbufstream);
begin
filter.load(s);
s.read(brei,sizeof(extended));
end;

procedure breitefilter.store (var s:tbufstream);
begin
filter.store(s);
s.write(brei,sizeof(extended));
end;

procedure breitefilter.vorbereitung (frequenz:extended);
begin
breianz2:=round(brei*frequenz/2000); breianz:=2*breianz2+1;
end;

{ arcsin - Filter }
constructor arcsin.neu;
begin
name:='arcsin';
end;

procedure arcsin.einheitgenerieren (var  beleg:belegung);
begin
with beleg do begin
   faktor:=pi/2/maxsample;
   negativ:=true;
   anfang:='rad';
   sekunde:=0;
   end;
end;

function arcsin.gefiltert (posi:longint):sample;
const fak=maxsample/pi*2;
var x:extended;
begin
x:=next^.gefiltert(posi)/maxsample;
gefiltert:=round(arctan(x/sqrt(1-x*x))*fak);
end;

{ arccos - Filter }
constructor arccos.neu;
begin
name:='arccos';
end;

procedure arccos.einheitgenerieren (var  beleg:belegung);
begin
with beleg do begin
   faktor:=pi/maxsample;
   negativ:=false;
   anfang:='rad';
   sekunde:=0;
   end;
end;

function arccos.gefiltert (posi:longint):sample;
const fak=maxsample/pi;
      pi2=pi/2;
var x:extended;
begin
x:=next^.gefiltert(posi)/maxsample;
gefiltert:=round((pi2-arctan(x/sqrt(1-x*x)))*fak);
end;

{ invert }

constructor invert.neu;
begin
name:='*(-1)';
end;

procedure invert.einheitgenerieren (var beleg:belegung);
begin
filter.einheitgenerieren(beleg);
beleg.negativ:=true;
end;

function invert.gefiltert (posi:longint):sample;
begin
gefiltert:=-next^.gefiltert(posi);
end;

{ einsdurch }

constructor einsdurch.neu;
begin
name:='1/X';
end;

procedure einsdurch.einheitgenerieren (var beleg:belegung);
begin
filter.einheitgenerieren(beleg);
with beleg do begin
   faktor:=1/faktor/maxsample;
   case length(anfang) of
    0:;
    1:anfang:='1/'+anfang;
    2..3:anfang:='1/('+anfang+')';
    else schwierigda:=true;
    end;
   sekunde:=-sekunde;
   end;





end;

function einsdurch.gefiltert (posi:longint):sample;
var   wert:sample;
begin
wert:=next^.gefiltert(posi);
if wert=0 then gefiltert:=maxsample
          else gefiltert:=maxsample div wert;
end;

{ offset }

constructor offset.neu (k:byte; hoch:sample);
begin
ho:=hoch;
name:='Offset '+vz[ho>=0]+extwort(ho*belegungsliste[k].faktor,3,2)
      +belegungsliste[k].einhwort;
end;

constructor offset.load (var s:tbufstream);
begin
filter.load(s);
s.read(ho,sizeof(sample));
end;

procedure offset.store (var s:tbufstream);
begin
filter.store(s);
s.write(ho,sizeof(sample));
end;

function offset.gefiltert (posi:longint):sample;
begin
gefiltert:=next^.gefiltert(posi)+ho;
end;

{ malfaktor }

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

function malfaktor.gefiltert (posi:longint):sample;
begin
gefiltert:=round(next^.gefiltert(posi)*float);
end;

{ streckung }

constructor streckung.neu (malfak:extended);
begin
float:=malfak;
name:='ù'+extwort(malfak,3,2);
end;

constructor streckung.load (var s:tbufstream);
begin
filter.load(s);
s.read(float,sizeof(extended));
end;

procedure streckung.store (var s:tbufstream);
begin
filter.store(s);
s.write(float,sizeof(extended));
end;

procedure streckung.einheitgenerieren (var beleg:belegung);
begin
filter.einheitgenerieren(beleg);
beleg.faktor:=beleg.faktor/float;
end;

function streckung.gefiltert (posi:longint):sample;
begin
gefiltert:=round(next^.gefiltert(posi)*float);
end;

{ kappen }

constructor kappen.neu;
begin
name:='Clip';
end;

function kappen.gefiltert(posi:longint):sample;
var   wert:sample;
begin
wert:=next^.gefiltert(posi);
if wert>maxsample then wert:=maxsample else
   if wert<minsample then wert:=minsample;
gefiltert:=wert;
end;

{ addition }

constructor addition.neu (additionskanal:byte);
begin
adk:=additionskanal;
name:='+ Chan. '+wort(adk);
end;

constructor addition.load (var s:tbufstream);
begin
filter.load(s);
s.read(adk,1);
end;

procedure addition.store (var s:tbufstream);
begin
filter.store(s);
s.write(adk,1);
end;

procedure addition.einheitgenerieren (var beleg:belegung);
begin
filter.einheitgenerieren(beleg);
beleg.faktor:=beleg.faktor*2;
end;

function addition.gefiltert (posi:longint):sample;
begin
gefiltert:=(next^.gefiltert(posi)+dat(posi,adk)) div 2;
end;

{ absolut }

constructor absolut.neu;
begin
name:='³y³';
end;

procedure absolut.einheitgenerieren (var beleg:belegung);
begin
filter.einheitgenerieren(beleg);
beleg.negativ:=false;
end;

function absolut.gefiltert (posi:longint):sample;
begin
gefiltert:=abs(next^.gefiltert(posi));
end;

{ square }

constructor square.neu;
begin
name:='yý';
end;

procedure square.einheitgenerieren (var beleg:belegung);
begin
filter.einheitgenerieren(beleg);
with beleg do begin
   negativ:=false; faktor:=sqr(faktor)*maxsample; schwierigda:=true end;
end;

function square.gefiltert (posi:longint):sample;
var xs:extended;
begin
xs:=next^.gefiltert(posi);
gefiltert:=round(xs*xs/maxsample);
end;

{ glatt }

constructor glatt.neu (breims:extended);
begin
breitefilter.neu(breims); name:='Gl.Avg. '#29+extwort(brei,3,1)+'ms';
end;

function glatt.gefiltert (posi:longint):sample;
var   i:longint;
      sum:extended;
begin
sum:=0; for i:=posi-breianz2 to posi+breianz2 do sum:=sum+next^.gefiltert(i);
gefiltert:=round(sum/breianz);
end;

{ passfilter }

procedure passfilter.neu (grenzfreq:longint);
begin
gr:=grenzfreq; name:=wort(gr)+'Hz'; new(spaltgr);
end;

constructor passfilter.load (var s:tbufstream);
begin
filter.load(s);
s.read(gr,sizeof(gr)); new(spaltgr);
end;

procedure passfilter.store (var s:tbufstream);
begin
filter.store(s);
s.write(gr,sizeof(gr));
end;

function passfilter.gefiltert (posi:longint):sample;
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
name:='LP '+name;
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
name:='HP '+name;
end;

procedure hochpass.vorbereitung (frequenz:extended);
var   fak:extended;
      j:integer;
begin
fak:=gr/frequenz; we:=min(round(genau*pi/fak),weite);
spaltgr^[0]:=1-fak/pi; for j:=1 to we do spaltgr^[j]:=-sin(j*fak)/j/pi;
end;

procedure hochpass.einheitgenerieren (var beleg:belegung);
begin
filter.einheitgenerieren(beleg);
beleg.negativ:=true;
end;

{ Diff-Filter }

constructor diff.neu;
begin
name:='d/dt';
end;

procedure diff.einheitgenerieren (var beleg:belegung);
begin
filter.einheitgenerieren(beleg);
with beleg do begin faktor:=faktor*fre; dec(sekunde) end;
beleg.negativ:=true;
end;

procedure diff.vorbereitung (frequenz:extended);
begin end;

function diff.gefiltert (posi:longint):sample;
begin
gefiltert:=round((next^.gefiltert(posi+1)-next^.gefiltert(posi-1))*diffaktor);
end;

{ Integrationsfilter }

constructor int.neu;
begin
name:='* dt';
end;

procedure int.einheitgenerieren (var beleg:belegung);
begin
filter.einheitgenerieren(beleg);
with beleg do begin faktor:=faktor/fre; inc(sekunde) end;
end;

procedure int.vorbereitung (frequenz:extended);
begin
posiwert:=0; gefiltertwert:=0;
end;

function int.gefiltert (posi:longint):sample;
var  i:longint;
begin
if posi<posiwert then
    for i:=posi+1 to posiwert do dec(gefiltertwert,next^.gefiltert(i))
                 else
    for i:=posiwert+1 to posi do inc(gefiltertwert,next^.gefiltert(i));
posiwert:=posi;
gefiltert:=gefiltertwert;
end;

{ gleitender Integrationsfilter }

constructor glint.neu(breims:extended);
begin
breitefilter.neu(breims); name:='* dt '#29+extwort(brei,3,1)+'ms';
end;

procedure glint.einheitgenerieren (var beleg:belegung);
begin
filter.einheitgenerieren(beleg);
with beleg do begin faktor:=faktor/fre; inc(sekunde) end;
end;

procedure glint.vorbereitung (frequenz:extended);
begin
breitefilter.vorbereitung(frequenz);
posiwert:=-breianz2; gefiltertwert:=0;
end;

function glint.gefiltert (posi:longint):sample;
var  i:longint;
begin
if posi<posiwert then
    for i:=posi+1 to posiwert do
        dec(gefiltertwert,next^.gefiltert(i+breianz2)
                         -next^.gefiltert(i-breianz2))
                 else
    for i:=posiwert+1 to posi do
        inc(gefiltertwert,next^.gefiltert(i+breianz2)
                         -next^.gefiltert(i-breianz2));
posiwert:=posi;
gefiltert:=gefiltertwert;
end;

{ gleitender Linienzugfilter }

constructor gllin.neu(breims:extended);
begin
breitefilter.neu(breims); name:='ä³dy³ '#29+extwort(brei,3,1)+'ms';
end;

procedure gllin.einheitgenerieren (var beleg:belegung);
begin
inherited einheitgenerieren(beleg);
with beleg do begin negativ:=false; faktor:=faktor; dec(sekunde) end;
end;

procedure gllin.vorbereitung (frequenz:extended);
begin
breitefilter.vorbereitung(frequenz);
posiwert:=-breianz2; gefiltertwert:=0;
end;

function gllin.gefiltert (posi:longint):sample;
var  i:longint;
begin
if posi<posiwert then
    for i:=posi+1 to posiwert do
        dec(gefiltertwert,abs(next^.gefiltert(i+breianz2)
                             -next^.gefiltert(i+breianz2-1))
                         -abs(next^.gefiltert(i-breianz2)
                             -next^.gefiltert(i-breianz2-1)))
                 else
    for i:=posiwert+1 to posi do
        inc(gefiltertwert,abs(next^.gefiltert(i+breianz2)
                             -next^.gefiltert(i+breianz2-1))
                         -abs(next^.gefiltert(i-breianz2)
                             -next^.gefiltert(i-breianz2-1)));
posiwert:=posi;
gefiltert:=gefiltertwert;
end;

{ Verschiebefilter }

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

function verschiebe.gefiltert (posi:longint):sample;
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

procedure maxmin.einheitgenerieren (var beleg:belegung);
begin
filter.einheitgenerieren(beleg);
beleg.faktor:=beleg.faktor*2; beleg.negativ:=false;
end;

procedure maxmin.vorbereitung (frequenz:extended);
begin
br2:=round(brei*frequenz/2000);
end;

function maxmin.gefiltert (posi:longint):sample;
var   i:longint;
      wert,wertmin,wertmax:minsample-1..maxsample+1;
begin
wertmax:=minsample-1; wertmin:=maxsample+1;
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

function spikefilter.gefiltert (posi:longint):sample;
label weiter;
var   bei,bei2,bis:integer;
      mitte:array[-spikemax..spikemax] of sample;
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
   schriftliste[i]:=copy(ko.k[i].name,1,10);
   grund[i]:=defaulteinheit;
   grund[i].gain:=ko.k[i].faktor1*ko.k[i].faktor2;
   grund[i].setz(1,copy(ko.k[i].einheit,1,7));
   end;
for i:=kan to kan+filtermax-1 do schriftliste[i]:=schriftliste[i mod kan];
schriftliste[maxkanal]:='- ';
end;

procedure kanalsetz (k,vonk:byte);
begin
filterloesch(k);
dispose(filteranfang[k],alt);
if vonk<kan then begin
   filteranfang[k]:=new(endezg,neu(vonk));
   schriftliste[k]:=schriftliste[vonk];
   end      else begin
   filteranfang[k]:=new(weiterzg,neu(vonk));
   schriftliste[k]:='[#'+wort(vonk)+']';
   end;
end;

procedure filtersetz (hilf:filterzeiger; k:byte);
begin
hilf^.next:=filteranfang[k]; filteranfang[k]:=hilf;
end;

procedure filterloesch (k:byte);
var   hilf:filterzeiger;
begin
hilf:=filteranfang[k];
while hilf<>nil do begin
   filteranfang[k]:=filteranfang[k]^.next;
   dispose(hilf,alt);
   hilf:=filteranfang[k] end;
filteranfang[k]:=new(endezg,neu(0));
end;

function filterdrin (k:byte):boolean;
begin
filterdrin:=(typeof(filteranfang[k]^)<>typeof(ende)) and
            (typeof(filteranfang[k]^)<>typeof(weiter));
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
begin
fre:=frequenz;
for i:=0 to kan-1 do with belegungsliste[i] do begin grundsetzen(i); handlich end;
for i:=kan to kan+filtermax-1 do with belegungsliste[i] do begin
   filteranfang[i]^.einheitgenerieren(belegungsliste[i]);
   if schwierigda then schwierig
                  else handlich;
   end;
end;

procedure oeffne (name:string80; var ko:kopfdaten);
var   i:longint;
      hilf:filterzeiger;
begin
daff.oeffne(name,ko);
diffaktor:=ko.freq/fre/2;
for i:=kan to kan+filtermax-1 do begin
   hilf:=filteranfang[i];
   while hilf^.next<>nil do begin
      hilf^.vorbereitung(ko.freq);
      hilf:=hilf^.next end;
   end;
end;

function dat (posi:longint; k:byte):sample;
begin
if k<kan then dat:=lese(posi,k)
         else dat:=filteranfang[k]^.gefiltert(posi);
end;

function extspannung (y:extended; kanal:byte):extended;
begin
extspannung:=y*belegungsliste[kanal].faktor
end;

function spannung (y:extended; kanal:byte):longint;
begin
spannung:=round(extspannung(y,kanal));
end;

function norm (sp:extended; kanal:byte) :extended;
begin
norm:=sp/belegungsliste[kanal].faktor;
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
registertype(rende);         registertype(rweiter);
registertype(rinvert);       registertype(roffset);
registertype(rmalfaktor);    registertype(rstreckung);     registertype(raddition);
registertype(rkappen);       registertype(rabsolut);       registertype(rint);
registertype(rsquare);       registertype(rglatt);         registertype(rtiefpass);
registertype(rhochpass);     registertype(rdiff);          registertype(rverschiebe);
registertype(rmaxmin);       registertype(rspikefilter);   registertype(reinsdurch);
registertype(rglint);        registertype(rgllin);
registertype(rarcsin);       registertype(rarccos);

for i:=1 to maxkanal-1 do filteranfang[i]:=new(endezg,neu(0));
end.