{ Borland-Pascal 7.0 / FPC 2.4 }

unit  tlfilter;

{$IFDEF MSDOS}
{$A+,B-,E+,F-,G-,I-,N+,O-,P+,T+,V+,X-}
{$ELSE}
{$A+,B-,E+,F-,G+,I-,N+,P+,T+,V+,X-}
{$ENDIF}

interface

uses  objects, plotter, bequem, daff, wavpcm, tulab42;

const filtermax=16;
      maxkanal=maxkan+filtermax;

      genau=4; weite=2000;

      spikemax=300;

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
      grundlistetyp=packed array[0..maxkan-1] of grundtyp;

      { Endform der Einheiten der gefilterten Daten }
      belegung=object (einheittyp)
         negativ, gepunktet, schwierigda : boolean;
         gepunktettl : char;
         procedure grundsetzen (k:byte);
         end;

      { Beschriftungen der Y-Achsen }
      schriftlistentyp=packed array[0..maxkanal] of string[10];

      { Abstraktes Filterobjekt }
      filterzeiger=^filter;
      filter=object (tobject)
         name:string80;
         next:filterzeiger;
         destructor alt; virtual;
         procedure einheitgenerieren (var  beleg:belegung); virtual;
         procedure vorbereitung (frequenz:extended); virtual;
         function gefiltert (posi:grossint):sample; virtual;
         constructor load (var s:tbufstream);
         procedure store (var s:tbufstream);
         end;

      { abstakter Filter mit Breite }
      breitefilter=object (filter)
       public
         procedure neu (breims:extended);
       private
         brei:extended;
         breianz,breianz2:grossint;
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
         function gefiltert (posi:grossint):sample; virtual;
         end;

      { arccos - Filter }
      arccoszg=^arccos;
      arccos=object (filter)
       public
         constructor neu;
       private
         procedure einheitgenerieren (var  beleg:belegung); virtual;
         function gefiltert (posi:grossint):sample; virtual;
         end;

      { *(-1) - Filter }
      invertzg=^invert;
      invert=object (filter)
       public
         constructor neu;
       private
         procedure einheitgenerieren (var  beleg:belegung); virtual;
         function gefiltert (posi:grossint):sample; virtual;
         end;

      { 1/X - Filter}
      einsdurchzg=^einsdurch;
      einsdurch=object (filter)
       public
         constructor neu;
       private
         procedure einheitgenerieren (var  beleg:belegung); virtual;
         function gefiltert (posi:grossint):sample; virtual;
         end;

      { +/- Offset - Filter }
      offsetzg=^offset;
      offset=object (filter)
       public
         constructor neu (k:byte; hoch:sample);
       private
         ho:sample;
         function gefiltert (posi:grossint):sample; virtual;
         constructor load (var s:tbufstream);
         procedure store (var s:tbufstream);
         end;

      { *m (Verstaerkungs-) - Filter }
      malfaktorzg=^malfaktor;
      malfaktor=object (filter)
       public
         constructor neu (malfak:extended);
       private
         float:extended;
         function gefiltert (posi:grossint):sample; virtual;
         constructor load (var s:tbufstream);
         procedure store (var s:tbufstream);
         end;

      { y-Achsenstreckungs - Filter }
      streckungzg=^streckung;
      streckung=object (filter)
       public
         constructor neu (k:byte; maxspannung:extended);
       private
         float:extended;
         procedure einheitgenerieren (var beleg:belegung); virtual;
         function gefiltert (posi:grossint):sample; virtual;
         constructor load (var s:tbufstream);
         procedure store (var s:tbufstream);
         end;

      { Kappung aller Werte < minsample und > maxsample }
      kappenzg=^kappen;
      kappen=object (filter)
       public
         constructor neu;
       private
         function gefiltert (posi:grossint):sample; virtual;
         end;

      { Additionsfilter }
      additionzg=^addition;
      addition=object (filter)
       public
         constructor neu (additionskanal:byte);
       private
         adk:byte;
         procedure einheitgenerieren (var beleg:belegung); virtual;
         function gefiltert (posi:grossint):sample; virtual;
         constructor load (var s:tbufstream);
         procedure store (var s:tbufstream);
         end;

      { Betragssfilter }
      betragzg=^betrag;
      betrag=object (filter)
       public
         constructor neu (kanal2:byte);
       private
         k2:byte;
         procedure einheitgenerieren (var beleg:belegung); virtual;
         function gefiltert (posi:grossint):sample; virtual;
         constructor load (var s:tbufstream);
         procedure store (var s:tbufstream);
         end;

      { Winkel }
      winkelzg=^winkel;
      winkel=object (filter)
       public
         constructor neu (xkanal:byte);
       private
         xk:byte;
         procedure einheitgenerieren (var beleg:belegung); virtual;
         function gefiltert (posi:grossint):sample; virtual;
         constructor load (var s:tbufstream);
         procedure store (var s:tbufstream);
         end;

      { Korrelationsfilter }
      korrelationzg=^korrelation;
      korrelation=object (breitefilter)
       public
          constructor neu (korrkanal:byte; breims:extended);
       private
          kk:byte;
          procedure einheitgenerieren (var beleg:belegung); virtual;
          function gefiltert (posi:grossint):sample; virtual;
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
         function gefiltert (posi:grossint):sample; virtual;
         end;

      { Quadrat - Filter }
      squarezg=^square;
      square=object (absolut)
       public
         constructor neu;
       private
         procedure einheitgenerieren (var beleg:belegung); virtual;
         function gefiltert (posi:grossint):sample; virtual;
         end;

      { Glaettungs - Filter }
      glattzg=^glatt;
      glatt=object (breitefilter)
       public
         constructor neu (breims:extended);
       private
         function gefiltert (posi:grossint):sample; virtual;
         end;

      { abstrakter Passfilter }
      spaltzeiger=^spaltfeld;
      spaltfeld=array[0..weite] of single;
      passfilter=object (filter)
       private
         gr:grossint;
         spaltgr:spaltzeiger;
         we:word;
         procedure neu (grenzfreq:grossint);
         destructor alt; virtual;
         function gefiltert (posi:grossint):sample; virtual;
         constructor load (var s:tbufstream);
         procedure store (var s:tbufstream);
         end;
      tiefpasszg=^tiefpass;
      tiefpass=object (passfilter)
       public
         constructor neu (grenzfreq:grossint);
       private
         procedure vorbereitung (frequenz:extended); virtual;
         end;
      hochpasszg=^hochpass;
      hochpass=object (passfilter)
       public
         constructor neu (grenzfreq:grossint);
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
         function gefiltert (posi:grossint):sample; virtual;
         end;

      { Integrations- Filter }
      intzg=^int;
      int=object (filter)
       public
         constructor neu;
       private
         gefiltertwert:sample;
         posiwert:grossint;
         procedure einheitgenerieren (var beleg:belegung); virtual;
         procedure vorbereitung (frequenz:extended); virtual;
         function gefiltert (posi:grossint):sample; virtual;
         end;

      { gleitender Integrations- Filter }
      glintzg=^glint;
      glint=object (breitefilter)
       public
         constructor neu (breims:extended);
       private
         gefiltertwert:sample;
         posiwert:grossint;
         procedure einheitgenerieren (var beleg:belegung); virtual;
         procedure vorbereitung (frequenz:extended); virtual;
         function gefiltert (posi:grossint):sample; virtual;
         end;

      { gleitender Linienzug- Filter }
      gllinzg=^gllin;
      gllin=object (breitefilter)
       public
         constructor neu (breims:extended);
       private
         gefiltertwert:sample;
         posiwert:grossint;
         procedure einheitgenerieren (var beleg:belegung); virtual;
         procedure vorbereitung (frequenz:extended); virtual;
         function gefiltert (posi:grossint):sample; virtual;
         end;

     { X-Achsen-Verschiebungs - Filter }
      verschiebezg=^verschiebe;
      verschiebe=object (filter)
       public
         constructor neu (umms:extended);
       private
         um:extended;
         posid:grossint;
         procedure vorbereitung (frequenz:extended); virtual;
         function gefiltert (posi:grossint):sample; virtual;
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
         br2:grossint;
         procedure einheitgenerieren (var beleg:belegung); virtual;
         procedure vorbereitung (frequenz:extended); virtual;
         function gefiltert (posi:grossint):sample; virtual;
         constructor load (var s:tbufstream);
         procedure store (var s:tbufstream);
         end;

      { Digitalfilter }
      digitalzg=^digital;
      digital=object (filter)
       public
         constructor neu (k:byte; schwelle:sample; neinheit:string; nwert:extended);
       private
         schw:sample;
         gefiltertwert:sample;
         posilinkswert,posirechtswert:grossint;
         neueeinheit:string;
         neuerwert:extended;
         procedure einheitgenerieren (var beleg:belegung); virtual;
         procedure vorbereitung (frequenz:extended); virtual;
         function gefiltert (posi:grossint):sample; virtual;
         constructor load (var s:tbufstream);
         procedure store (var s:tbufstream);
         end;


      { Spike - Filter }
      spikefilterzg=^spikefilter;
      spikefilter=object (filter)
       public
         constructor neu (k:byte; millisek,ablinks,abrechts:extended);
       private
         anz:exword;
         abstli,abstre:sample;
         ms:extended;
         abli,abre:extended;
         procedure vorbereitung (frequenz:extended); virtual;
         function gefiltert (posi:grossint):sample; virtual;
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

function dat (posi:grossint; k:byte):sample;

function extspannung (y:extended; kanal:byte):extended;
function spannung (y:extended; kanal:byte):grossint;

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
         function gefiltert (posi:grossint):sample; virtual;
         constructor load (var s:tbufstream);
         procedure store (var s:tbufstream);
         end;
      weiterzg=^weiter;
      weiter=object (ende)
         procedure einheitgenerieren (var  beleg:belegung); virtual;
         function gefiltert (posi:grossint):sample; virtual;
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
      rkorrelation:tstreamrec=(objtype:322;           vmtlink:ofs(typeof(korrelation)^);
                               load:@korrelation.load;store:@korrelation.store);
      {rdigital    :tstreamrec=(objtype:323;           vmtlink:ofs(typeof(digital)^);
                               load:@digital.load;    store:@digital.store);}
      rwinkel     :tstreamrec=(objtype:324;           vmtlink:ofs(typeof(winkel)^);
                               load:@winkel.load;     store:@winkel.store);
      rbetrag     :tstreamrec=(objtype:325;           vmtlink:ofs(typeof(betrag)^);
                               load:@betrag.load;     store:@betrag.store);
      rdigital    :tstreamrec=(objtype:326;           vmtlink:ofs(typeof(digital)^);
                               load:@digital.load;    store:@digital.store);



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
      -2..0:einhwort:=vor+anfang+liste[sekunde];
      1,2:if anfang='1' then einhwort:=vor+liste[sekunde]
                        else einhwort:=vor+anfang+liste[sekunde];
      else if sekunde<-2 then einhwort:=vor+anfang+'/s^'+bequem.wort(-sekunde)
                         else einhwort:=vor+anfang+'s^'+bequem.wort(sekunde);
      end;
end;

function einheittyp.plot:string80;
var   kom:string80;
      p:grossint;
begin
if (vor='') and (anfang='1') and (sekunde=0) then kom:=''
                                              else begin
   if vor='æ'then kom:=plmu
             else kom:='LB'+vor+#3;
   if (anfang='1') and (sekunde=-1) then kom:=kom+'LBHz'#3
                                    else begin

      if (sekunde<=0) or (anfang<>'1') then kom:=kom+'LB'+anfang;
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
      x:extended;
procedure setz (mal:extended; davor:string1);
begin
faktor:=faktor*mal; vor:=davor;
end;
begin
x:=faktor*maxsample*1.00001; if x<=1E-9 then x:=1E-9;
if (sekunde=0) and (anfang='') then schwierig
                               else begin
   n:=round(log(x)-0.5);
   case n-1 of
     -11,-10, -9:setz(1E12,'p');
      -8, -7, -6:setz( 1E9,'n');
      -5, -4, -3:setz( 1E6,'u');
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
var   n:grossint;
      x:extended;
begin
x:=faktor*maxsample*1.00001; if x<=1E-9 then x:=1E-9;
n:=pred(trunc(log(x))); if n<0 then dec(n,2);
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
kopie(grund[k]); negativ:=true; gepunktet:=false; schwierigda:=false;
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

function filter.gefiltert (posi:grossint):sample;
begin
gefiltert:=next^.gefiltert(posi);
end;

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

function ende.gefiltert (posi:grossint):sample;
begin
gefiltert:=lesef(posi,ka);
end;

{ weiter }

procedure weiter.einheitgenerieren (var beleg:belegung);
begin
filteranfang[ka]^.einheitgenerieren(beleg);
end;

function weiter.gefiltert (posi:grossint):sample;
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
filter.einheitgenerieren(beleg);
with beleg do begin
   faktor:=pi/2/maxsample;
   negativ:=true;
   anfang:='rad';
   sekunde:=0;
   end;
end;

function arcsin.gefiltert (posi:grossint):sample;
const fak=maxsample/pi*2;
var x,x1:extended;
begin
x:=next^.gefiltert(posi)/maxsample;
x1:=1-x*x;
if x1>1e-10 then gefiltert:=round(arctan(x/sqrt(x1))*fak)
            else if x>0 then gefiltert:=maxsample
                        else gefiltert:=-maxsample;
end;

{ arccos - Filter }
constructor arccos.neu;
begin
name:='arccos';
end;

procedure arccos.einheitgenerieren (var  beleg:belegung);
begin
filter.einheitgenerieren(beleg);
with beleg do begin
   faktor:=pi/maxsample;
   negativ:=false;
   anfang:='rad';
   sekunde:=0;
   end;
end;

function arccos.gefiltert (posi:grossint):sample;
const fak=maxsample/pi;
      pi2=pi/2;
var x,x1:extended;
begin
x:=next^.gefiltert(posi)/maxsample;
x1:=1-x*x;
if x1>1e-10 then gefiltert:=round((pi2-arctan(x/sqrt(x1)))*fak)
       else if x>0 then gefiltert:=0
                   else gefiltert:=maxsample;
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

function invert.gefiltert (posi:grossint):sample;
begin
gefiltert:=-next^.gefiltert(posi);
end;

{ einsdurch }

const einsdurchfak=1e6;

constructor einsdurch.neu;
begin
name:='1/X';
end;

procedure einsdurch.einheitgenerieren (var beleg:belegung);
begin
filter.einheitgenerieren(beleg);
with beleg do begin
   faktor:=1/faktor/maxsample/einsdurchfak;
   case length(anfang) of
    0:;
    1:if anfang<>'1' then anfang:='1/'+anfang;
    2..3:anfang:='1/('+anfang+')';
    else schwierigda:=true;
    end;
   sekunde:=-sekunde;
   end;
end;

function einsdurch.gefiltert (posi:grossint):sample;
var   wert:sample;
begin
wert:=next^.gefiltert(posi);
if wert<=einsdurchfak then gefiltert:=maxsample
                      else gefiltert:=round(maxsample/wert*einsdurchfak);
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

function offset.gefiltert (posi:grossint):sample;
begin
gefiltert:=next^.gefiltert(posi)+ho;
end;

{ malfaktor }

constructor malfaktor.neu (malfak:extended);
begin
float:=malfak;
name:='*'+extfwort(malfak,3);
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

function malfaktor.gefiltert (posi:grossint):sample;
begin
gefiltert:=round(next^.gefiltert(posi)*float);
end;

{ streckung }

constructor streckung.neu (k:byte; maxspannung:extended);
begin
float:=belegungsliste[k].faktor*maxsample/abs(maxspannung);
name:='**'+extfwort(float,3);
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

function streckung.gefiltert (posi:grossint):sample;
begin
gefiltert:=round(next^.gefiltert(posi)*float);
end;

{ kappen }

constructor kappen.neu;
begin
name:='Clip';
end;

function kappen.gefiltert(posi:grossint):sample;
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

function addition.gefiltert (posi:grossint):sample;
begin
gefiltert:=(next^.gefiltert(posi)+dat(posi,adk)) div 2;
end;

{ x-y-Betrag }

constructor betrag.neu (kanal2:byte);
begin
k2:=kanal2;
name:='³(x Chan.'+wort(k2)+')³';
end;

constructor betrag.load (var s:tbufstream);
begin
filter.load(s);
s.read(k2,1);
end;

procedure betrag.store (var s:tbufstream);
begin
filter.store(s);
s.write(k2,1);
end;

procedure betrag.einheitgenerieren (var beleg:belegung);
begin
filter.einheitgenerieren(beleg);
beleg.negativ:=false;
end;

function betrag.gefiltert (posi:grossint):sample;
var ys,zs:extended;
begin
ys:=next^.gefiltert(posi);
zs:=dat(posi,k2);
gefiltert:=round(sqrt(ys*ys+zs*zs));
end;

{ Winkel }

constructor winkel.neu (xkanal:byte);
begin
xk:=xkanal;
name:='(y) Angle (x=#'+wort(xk)+')';
end;

constructor winkel.load (var s:tbufstream);
begin
filter.load(s);
s.read(xk,1);
end;

procedure winkel.store (var s:tbufstream);
begin
filter.store(s);
s.write(xk,1);
end;

procedure winkel.einheitgenerieren (var beleg:belegung);
begin
filter.einheitgenerieren(beleg);
with beleg do begin
   faktor:=pi/maxsample;
   negativ:=true;
   anfang:='rad';
   sekunde:=0;
   end;
end;

function winkel.gefiltert (posi:grossint):sample;
const fak=maxsample/pi;
      halb=round(fak*pi);
var x,y:sample;
    winkel:sample;
begin
x:=dat(posi,xk);
y:=next^.gefiltert(posi);
if (x=0) and (y=0) then winkel:=minsample
   else if abs(x)>abs(y) then
            if (y>=0)=(x>=0) then winkel:=round(arctan(y/x)*fak)
                             else winkel:=round((arctan(y/x)+pi)*fak)
            else winkel:=round((pi/2-arctan(x/y))*fak);
if y<0 then gefiltert:=winkel-halb else gefiltert:=winkel;
end;

{ absolut }

constructor absolut.neu;
begin
name:='³x³';
end;

procedure absolut.einheitgenerieren (var beleg:belegung);
begin
filter.einheitgenerieren(beleg);
beleg.negativ:=false;
end;

function absolut.gefiltert (posi:grossint):sample;
begin
gefiltert:=abs(next^.gefiltert(posi));
end;

{ square }

constructor square.neu;
begin
name:='xý';
end;

procedure square.einheitgenerieren (var beleg:belegung);
begin
filter.einheitgenerieren(beleg);
with beleg do begin
   negativ:=false; faktor:=sqr(faktor)*maxsample; schwierigda:=true end;
end;

function square.gefiltert (posi:grossint):sample;
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

function glatt.gefiltert (posi:grossint):sample;
var   i:grossint;
      sum:extended;
begin
sum:=0; i:=posi-breianz2; while i<=posi+breianz2 do begin sum:=sum+next^.gefiltert(i); inc(i) end;
gefiltert:=round(sum/breianz);
end;

{ Korrelation }

constructor korrelation.neu (korrkanal:byte; breims:extended);
begin
inherited neu(breims);
kk:=korrkanal;
name:='Corr. to '+wort(kk)+' ('+extwort(brei,3,1)+'ms)';
end;

constructor korrelation.load (var s:tbufstream);
begin
inherited load(s);
s.read(kk,1);
end;

procedure korrelation.store (var s:tbufstream);
begin
inherited store(s);
s.write(kk,1);
end;

procedure korrelation.einheitgenerieren (var beleg:belegung);
begin
inherited einheitgenerieren(beleg);
with beleg do begin
   faktor:=1/maxsample;
   anfang:='1';
   sekunde:=0;
   end;
end;

function korrelation.gefiltert (posi:grossint):sample;
var   i:grossint;
      x,y,sx,sy,sxq,syq,sxy:extended;
begin
sx:=0; sy:=0; sxq:=0; syq:=0; sxy:=0;
i:=-breianz2;
while i<=+breianz2 do begin
   x:=next^.gefiltert(posi+i);
   y:=dat(posi+i,kk);
   sx:=sx+x; sy:=sy+y;
   sxq:=sxq+x*x; syq:=syq+y*y;
   sxy:=sxy+x*y;
   inc(i);
   end;
if (sxq<1e-40) and (syq<1e-40) then gefiltert:=maxsample
                               else
gefiltert:=round((breianz*sxy-sx*sy)/sqrt((breianz*sxq-sx*sx)*(breianz*syq-sy*sy))*maxsample);
end;

{ passfilter }

procedure passfilter.neu (grenzfreq:grossint);
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

function passfilter.gefiltert (posi:grossint):sample;
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

constructor tiefpass.neu (grenzfreq:grossint);
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

constructor hochpass.neu (grenzfreq:grossint);
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

function diff.gefiltert (posi:grossint):sample;
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

function int.gefiltert (posi:grossint):sample;
var  i:grossint;
begin
if posi<posiwert then begin
    i:=posi+1;
    while i<=posiwert do begin dec(gefiltertwert,next^.gefiltert(i)); inc(i) end
    end
                 else begin
    i:=posiwert+1;
    while i<=posi do begin inc(gefiltertwert,next^.gefiltert(i)); inc(i) end
    end;
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
posiwert:=-breianz2-2; gefiltertwert:=0;
end;

function glint.gefiltert (posi:grossint):sample;
var  i:grossint;
begin
if posi<posiwert then begin
    i:=posi+1;
    while i<=posiwert do begin
        dec(gefiltertwert,next^.gefiltert(i+breianz2)
                         -next^.gefiltert(i-breianz2));
        inc(i)
        end
    end
                 else begin
    i:=posiwert+1;
    while i<=posi do begin
        inc(gefiltertwert,next^.gefiltert(i+breianz2)
                         -next^.gefiltert(i-breianz2));
        inc(i)
        end
    end;
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
posiwert:=-breianz2-2; gefiltertwert:=0;
end;

function gllin.gefiltert (posi:grossint):sample;
var  i:grossint;
begin
if posi<posiwert then begin
    i:=posi+1;
    while i<=posiwert do begin
        dec(gefiltertwert,abs(next^.gefiltert(i+breianz2)
                             -next^.gefiltert(i+breianz2-1))
                         -abs(next^.gefiltert(i-breianz2)
                             -next^.gefiltert(i-breianz2-1)));
        inc(i)
        end
    end
                  else begin
    i:=posiwert+1;
    while i<=posi do begin
        inc(gefiltertwert,abs(next^.gefiltert(i+breianz2)
                             -next^.gefiltert(i+breianz2-1))
                         -abs(next^.gefiltert(i-breianz2)
                             -next^.gefiltert(i-breianz2-1)));
        inc(i)
        end
    end;
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

function verschiebe.gefiltert (posi:grossint):sample;
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

function maxmin.gefiltert (posi:grossint):sample;
var   i:grossint;
      wert,wertmin,wertmax:minsample-1..maxsample+1;
begin
wertmax:=minsample-1; wertmin:=maxsample+1;
i:=posi-br2;
while i<=posi+br2 do begin
   wert:=next^.gefiltert(i);
   if wert>wertmax then wertmax:=wert;
   if wert<wertmin then wertmin:=wert;
   inc(i)
   end;
gefiltert:=(wertmax-wertmin) div 2;
end;

{ Digitalfilter }

constructor digital.neu (k:byte; schwelle:sample; neinheit:string; nwert:extended);
begin
schw:=schwelle;
neueeinheit:=neinheit;
neuerwert:=nwert;
name:='Pulse counter '+extwort(schw*belegungsliste[k].faktor,3,2)
      +belegungsliste[k].einhwort+' ('+extwort(neuerwert,5,3)+' '+neueeinheit+')';
end;

procedure digital.einheitgenerieren(var beleg:belegung);
begin
inherited einheitgenerieren(beleg);
with beleg do begin
   faktor:=neuerwert*fre/maxsample/2;
   anfang:=neueeinheit;
   sekunde:=-1;
   negativ:=true;
   end;
end;

procedure digital.vorbereitung (frequenz:extended);
begin
posilinkswert:=0; posirechtswert:=0; gefiltertwert:=0;
end;

constructor digital.load (var s:tbufstream);
begin
filter.load(s);
s.read(schw,sizeof(sample));
s.read(neueeinheit,sizeof(neueeinheit));
s.read(neuerwert,sizeof(neuerwert));
end;

procedure digital.store (var s:tbufstream);
begin
filter.store(s);
s.write(schw,sizeof(sample));
s.write(neueeinheit,sizeof(neueeinheit));
s.write(neuerwert,sizeof(neuerwert));
end;

function digital.gefiltert (posi:grossint):sample;
var li,re,minli,maxre:grossint;
begin
if (posi>=posilinkswert) and (posi<posirechtswert) then
   gefiltert:=gefiltertwert
                                                   else begin
   li:=posi; re:=posi;
   minli:=posi-100000; maxre:=posi+100000;
   while (abs(next^.gefiltert(li))<schw) and (li>=minli) do dec(li);
   while abs(next^.gefiltert(li))>schw do dec(li); inc(li);
   while abs(next^.gefiltert(re))>schw do inc(re);
   while (abs(next^.gefiltert(re))<schw) and (re<=maxre) do inc(re);
   if (li<=minli) or (re>maxre) then begin
    gefiltert:=0;
    gefiltertwert:=0; posilinkswert:=li; posirechtswert:=re;
    end
    else begin
      if (next^.gefiltert(li)>0) and (next^.gefiltert(re)>0)
         then gefiltertwert:=round(maxsample/(re-li)*2)
           else if (next^.gefiltert(li)<0) and (next^.gefiltert(re)<0)
                  then gefiltertwert:=round(maxsample/(li-re)*2)
                  else gefiltertwert:=0;
      gefiltert:=gefiltertwert;
      posilinkswert:=li; posirechtswert:=re;
      end;
   end;
end;

{ Spikefilter }

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

function spikefilter.gefiltert (posi:grossint):sample;
label weiter;
var   bei,bei2,bis:longint;
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
filterzeile:=puffer+'#'+wort(k);
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

function dat (posi:grossint; k:byte):sample;
begin
if k<kan then dat:=lesef(posi,k)
         else dat:=filteranfang[k]^.gefiltert(posi);
end;

function extspannung (y:extended; kanal:byte):extended;
begin
extspannung:=y*belegungsliste[kanal].faktor
end;

function spannung (y:extended; kanal:byte):grossint;
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
registertype(rglint);        registertype(rgllin);         registertype(rkorrelation);
registertype(rarcsin);       registertype(rarccos);        registertype(rdigital);
registertype(rwinkel);       registertype(rbetrag);

for i:=1 to maxkanal-1 do filteranfang[i]:=new(endezg,neu(0));
end.
