{ Borland-Pascal 7.0 / FPC 2.0 }
{$ifdef fpc} {$mode TP} {$endif}

unit nltrigg;

{$IFDEF MSDOS}
{$A+,B-,E+,F-,G-,I-,N+,O-,P+,T+,V+,X-}
{$ELSE}
{$A+,B-,E+,F-,G+,I-,N+,P+,T+,V+,X-}
{$ENDIF}

interface

uses  crt, dos,           daff,wavpcm,tulab42,
      objects,            tlfilter,
      bequem,             tlfiles,
      nlrahmen;

const {$ifdef fpc} triggermax=1 shl 22 -1; {$else} triggermax=65520 div sizeof(messwert) -12; {$endif}
      listmax='J';

type  { Triggerlisten }

      {$ifdef fpc} grossintcomp=int64; {$else} grossintcomp=comp; {$endif}

      triggerliste=array[0..triggermax+1] of messwert;
      triggerdaten=object
         autom:^triggerliste;
         automn:longint;
         automda:boolean;
         procedure nimm (var aut:triggerliste; gesamt:longint);
         procedure store (var s:tbufstream);
         procedure load (var s:tbufstream);
         procedure frei;
         procedure such (links,rechts:exword; nach:messwert; var li,re:exword);
         end;

      filemenge=set of 1..maxfiles;

      { Triggeralgorithmen }

      triggerungzg=^triggerung;
      triggerung=object (tobject)
         name:string[50];
         fil:array [1..maxfiles] of triggerdaten;
         tr:byte;
         function fileanz:byte;
         function erstfile:byte;
         function triggsum:exword;
         procedure neu;
         constructor load (var s:tbufstream);
         procedure store (var s:tbufstream);
         destructor alt; virtual;
         procedure triggern (dabei:filemenge); virtual;
       private
         procedure blocktriggern (von,bis:messwert); virtual;
         end;

      keinezg=^keine;
      keine=object (triggerung)
         constructor neu;
         procedure triggern (dabei:filemenge); virtual;
         end;

      gelesenzg=^gelesen;
      gelesen=object (triggerung)
         constructor neu;
      private
         procedure triggern (dabei:filemenge); virtual;
         end;

      punktezg=^punkte;
      punkte=object (triggerung)
         constructor neu;
      private
         procedure triggern (dabei:filemenge); virtual;
         end;

      aequidistantzg=^aequidistant;
      aequidistant=object (triggerung)
         anfa,dist:messwert;
         constructor neu;
{         procedure triggern (dabei:filemenge); virtual;}
         constructor load (var s:tbufstream);
         procedure store (var s:tbufstream);
       private
         procedure blocktriggern (von,bis:messwert); virtual;
         end;

      schwelle=object (triggerung)
         schw:wert;
         procedure neu;
         constructor load (var s:tbufstream);
         procedure store (var s:tbufstream);
         end;
      hochzg=^hoch;
      hoch=object (schwelle)
         constructor neu;
       private
         procedure blocktriggern (von,bis:messwert); virtual;
         end;
      runterzg=^runter;
      runter=object (schwelle)
         constructor neu;
       private
         procedure blocktriggern (von,bis:messwert); virtual;
         end;
      extremum=object (triggerung)
       private
         bmittel,nmittel:extended;
         procedure neu;
         constructor load (var s:tbufstream);
         procedure store (var s:tbufstream);
         end;
      minimumzg=^minimum;
      minimum=object (extremum)
         constructor neu;
       private
         procedure blocktriggern (von,bis:messwert); virtual;
         end;
      maximumzg=^maximum;
      maximum=object (extremum)
         constructor neu;
       private
         procedure blocktriggern (von,bis:messwert); virtual;
         end;
      fenster=object (triggerung)
       private
         schwunten,schwoben:wert;
         procedure neu;
         constructor load (var s:tbufstream);
         procedure store (var s:tbufstream);
         end;
      fenstermaximumzg=^fenstermaximum;
      fenstermaximum=object (fenster)
         constructor neu;
       private
         procedure blocktriggern (von,bis:messwert); virtual;
         end;
      fensterminimumzg=^fensterminimum;
      fensterminimum=object (fenster)
         constructor neu;
       private
         procedure blocktriggern (von,bis:messwert); virtual;
         end;
      eintrittzg=^eintritt;
      eintritt=object (fenster)
         constructor neu;
       private
         procedure blocktriggern (von,bis:messwert); virtual;
         end;
      austrittzg=^austritt;
      austritt=object (fenster)
         constructor neu;
       private
         procedure blocktriggern (von,bis:messwert); virtual;
         end;

      triggerungsliste=array ['A'..listmax] of triggerungzg;

      { Weitere Filter als Ergaenzung zur Unit "TLFILTER" }

      { Abstakter Filter mit Triggerliste }
      triggerfilter=object (filter)
         trliste:char;
         trdaten:triggerdaten;
         procedure vorbereitung (frequenz:extended); virtual;
         constructor load (var s:tbufstream);
         procedure store (var s:tbufstream);
         end;

      { Abstakter Filter mit zwei Triggerlisten }
      doppeltriggerfilter=object (filter)
         retrliste,ertrliste:char;
         retrdaten,ertrdaten:triggerdaten;
         procedure vorbereitung (frequenz:extended); virtual;
         constructor load (var s:tbufstream);
         procedure store (var s:tbufstream);
         end;

      { Zaehlt die Triggerpunkte -> Treppenfunktion }
      zaehltfilterzg=^zaehltfilter;
      zaehltfilter=object (triggerfilter)
         constructor neu(trigliste:char);
         procedure einheitgenerieren (var  beleg:belegung); virtual;
         function gefiltert (posi:grossint):sample; virtual;
         end;

      { Umschaltung auf Punkte }
      punktefilterzg=^punktefilter;
      punktefilter=object (triggerfilter)
         constructor neu(trigliste:char);
         procedure einheitgenerieren (var beleg:belegung); virtual;
         end;

      { Momentanfrequenz aus benachbarten Triggerpunkten berechnet }
      freqfilterzg=^freqfilter;
      freqfilter=object (triggerfilter)
         constructor neu(trigliste:char);
         procedure einheitgenerieren(var beleg:belegung); virtual;
         function gefiltert (posi:grossint):sample; virtual;
         end;

      { Zeitdifferenz aus benachbarten Triggerpunkten berechnet }
      intervallfilterzg=^intervallfilter;
      intervallfilter=object (triggerfilter)
         constructor neu(trigliste:char);
         procedure einheitgenerieren(var beleg:belegung); virtual;
         function gefiltert (posi:grossint):sample; virtual;
         end;

      { Werte an den Triggerstellen werden zu einem Linienzug verbunden }
      polygonfilterzg=^polygonfilter;
      polygonfilter=object (triggerfilter)
         constructor neu(trigliste:char);
         procedure vorbereitung (frequenz:extended); virtual;
         function gefiltert (posi:grossint):sample; virtual;
        private
         erneut:boolean;
         lialt,realt:exword;
         liposi,reposi:grossint;
         liwert,rewert:grossintcomp;
         end;

      { Werte an den Triggerstellen werden aus eine ASCII-Liste gelesen }
      ywert=messwert; { ...dann passts mit der 64kB-Grenze }
      alistentyp=array[1..triggermax] of ywert;
      asciifilterzg=^asciifilter;
      asciifilter=object (triggerfilter)
         constructor neu(aname:string; trigliste:char);
         procedure einheitgenerieren(var beleg:belegung); virtual;
         procedure vorbereitung (frequenz:extended); virtual;
         function gefiltert (posi:grossint):sample; virtual;
         constructor load (var s:tbufstream);
         procedure store (var s:tbufstream);
        private
         aliste:alistentyp;
         amax:ywert;
         end;

      { Zeitdifferenzen zwischen Triggerlisten - alt}
      diffilteraltzg=^diffilteralt;
      diffilteralt=object (doppeltriggerfilter)
         smax:extended;
         constructor neu(retrigliste,ertrigliste:char; msmax:grossint);
         procedure einheitgenerieren(var beleg:belegung); virtual;
         function gefiltert (posi:grossint):sample; virtual;
         constructor load (var s:tbufstream);
         procedure store (var s:tbufstream);
         end;

      { Zeitdifferenzen zwischen Triggerlisten - neu}
      diffilterzg=^diffilter;
      diffilter=object (doppeltriggerfilter)
         smax:extended;
         typ:(naechster,vorwaerts,rueckwaerts);
         constructor neu(retrigliste,ertrigliste:char; msmax:grossint; typchar:char);
         procedure einheitgenerieren(var beleg:belegung); virtual;
         function gefiltert (posi:grossint):sample; virtual;
         constructor load (var s:tbufstream);
         procedure store (var s:tbufstream);
         end;

      { Phasenfilter }
      phasenfilterzg=^phasenfilter;
      phasenfilter=object (doppeltriggerfilter)
         peri:grossint;
         constructor neu(retrigliste,ertrigliste:char; perioden:grossint);
         procedure einheitgenerieren(var beleg:belegung); virtual;
         function gefiltert (posi:grossint):sample; virtual;
         constructor load (var s:tbufstream);
         procedure store (var s:tbufstream);
         end;

      { TL-Integrations- Filter }
      tlintfilterzg=^tlintfilter;
      tlintfilter=object (triggerfilter)
       public
         constructor neu(trigliste:char);
       private
         gefiltertwert:sample;
         posiwert:grossint;
         procedure einheitgenerieren (var beleg:belegung); virtual;
         procedure vorbereitung (frequenz:extended); virtual;
         function gefiltert (posi:grossint):sample; virtual;
         end;

      { Hilfsfeld fuer phasenbezogene Auswertung }

      weiser=array[1..triggermax] of exword;
      triggerweiser=object
         weisliste:array[1..maxfiles] of
                    record
                       t:^weiser; l:exword;
                       n:exword  end;
         gesamt:longint;
         mittelabstand:messwert;
         procedure zaehlen (var feld:triggerung; minabst,maxabst:messwert);
         procedure frei;
         end;


const triggeranz:exword=triggermax;
      triggeranf:exword=1;
      triggerabz:exword=1;
      triggerdst:messwert=0;

var   tliste:triggerungsliste;
      komp84:boolean; {Kompatibilitaet zwischen 8.4 und 8.5}

procedure autoblock (aktfile:byte);

procedure manager;

procedure triggeruebersicht;
procedure kontrolle (nr:byte; trind:char);

{ Abspeichern der Triggerlisten in einem stream }

procedure streamput (var s:tbufstream);
procedure streamget (var s:tbufstream);

implementation

type  uebersicht=array['A'..listmax] of filemenge;
      zahlenuebersicht=array['A'..listmax,1..maxfiles] of exword;
      matrix=object
         tl:uebersicht;
         unsinn:boolean;     escape:boolean;
         procedure eingabe;
         end;
      zahlenmatrix=object (matrix)
         tz:zahlenuebersicht;
         tn:char;            fn:byte;
         zn:byte;            wx,wy:byte;
         procedure uebernehmen;
         procedure ausgabe;
         procedure erstsprung;                procedure sprung;
         end;

      trist=object
         gesamt:exword;
         zaehler,abzaehler:exword;
         letztstelle:messwert;
         zn:byte;
         procedure beginn(trfile:byte);       procedure weiter(stelle:messwert);
         function aufhoeren:boolean;
         end;
      gleitschw=object
         schw:wert;
         nkorr,n2korr:grossint;
         procedure beginn (nmittel,bmittel:extended);
         procedure mitteln (stekorr:grossint; tr:byte);
         procedure zaehlt (var stekorr:grossint; tr:byte);
         end;

const bloecke:boolean=false;
      bloeckeb:array[boolean] of char=('f','b');
      bloecketext:array[boolean] of string20=('File','Block');

      akttrkan:byte=0;

      rkeine:tstreamrec=    (objtype:100;             vmtlink:ofs(typeof(keine)^);
                             load:@triggerung.load;   store:@triggerung.store);
      rgelesen:tstreamrec=  (objtype:111;             vmtlink:ofs(typeof(gelesen)^);
                             load:@triggerung.load;   store:@triggerung.store);
      rpunkte:tstreamrec=   (objtype:101;             vmtlink:ofs(typeof(punkte)^);
                             load:@triggerung.load;   store:@triggerung.store);
      rhoch:tstreamrec=     (objtype:102;             vmtlink:ofs(typeof(hoch)^);
                             load:@schwelle.load;     store:@schwelle.store);
      rrunter:tstreamrec=   (objtype:103;             vmtlink:ofs(typeof(runter)^);
                             load:@schwelle.load;     store:@schwelle.store);
      rminimum:tstreamrec=  (objtype:104;             vmtlink:ofs(typeof(minimum)^);
                             load:@extremum.load;     store:@extremum.store);
      rmaximum:tstreamrec=  (objtype:105;             vmtlink:ofs(typeof(maximum)^);
                             load:@extremum.load;     store:@extremum.store);
      rfenstermaximum:tstreamrec=(objtype:106;        vmtlink:ofs(typeof(fenstermaximum)^);
                             load:@fenster.load;      store:@fenster.store);
      rfensterminimum:tstreamrec=(objtype:107;        vmtlink:ofs(typeof(fensterminimum)^);
                             load:@fenster.load;      store:@fenster.store);
      raequidistant:tstreamrec=(objtype:108;          vmtlink:ofs(typeof(aequidistant)^);
                             load:@aequidistant.load; store:@aequidistant.store);
      reintritt:tstreamrec= (objtype:109;             vmtlink:ofs(typeof(eintritt)^);
                             load:@fenster.load;      store:@fenster.store);
      raustritt:tstreamrec= (objtype:110;        vmtlink:ofs(typeof(austritt)^);
                             load:@fenster.load;      store:@fenster.store);

      rfreqfilter:tstreamrec=(objtype:120;            vmtlink:ofs(typeof(freqfilter)^);
                             load:@triggerfilter.load;store:@triggerfilter.store);
      rpolygonfilter:tstreamrec=(objtype:121;         vmtlink:ofs(typeof(polygonfilter)^);
                             load:@triggerfilter.load;store:@triggerfilter.store);
      rdiffilteralt:tstreamrec=(objtype:122;          vmtlink:ofs(typeof(diffilteralt)^);
                             load:@diffilteralt.load; store:@diffilteralt.store);
      rphasenfilter:tstreamrec=(objtype:123;          vmtlink:ofs(typeof(phasenfilter)^);
                             load:@phasenfilter.load; store:@phasenfilter.store);
      rpunktefilter:tstreamrec=(objtype:124;          vmtlink:ofs(typeof(punktefilter)^);
                             load:@triggerfilter.load;store:@triggerfilter.store);
      rzaehltfilter:tstreamrec=(objtype:125;          vmtlink:ofs(typeof(zaehltfilter)^);
                             load:@triggerfilter.load;store:@triggerfilter.store);
      rintervallfilter:tstreamrec=(objtype:126;       vmtlink:ofs(typeof(intervallfilter)^);
                             load:@triggerfilter.load;store:@triggerfilter.store);
      rdiffilter:tstreamrec=(objtype:127;             vmtlink:ofs(typeof(diffilter)^);
                             load:@diffilter.load;    store:@diffilter.store);
      rasciifilter:tstreamrec=(objtype:128;           vmtlink:ofs(typeof(asciifilter)^);
                             load:@asciifilter.load;  store:@asciifilter.store);
      rtlintfilter:tstreamrec=(objtype:129;             vmtlink:ofs(typeof(tlintfilter)^);
                             load:@triggerfilter.load;store:@triggerfilter.store);

var   mat:zahlenmatrix;
      verfol:trist;

      aut:^triggerliste;
      trind:char;
      abbruch:boolean;


procedure autoblock (aktfile:byte);
var   ta,tb:char;
      wandert:listenzeiger;
      i,j:longint;
      dummy,re:exword;
      linksf,rechtsf:byte;
begin
ueberschrift(false,'Block Creation','Info',farbe4);
triggeruebersicht;
gotoxy(1,17); zwischen('Dialogue',farbe4);
window(1,21,80,25);
if aktfile=0 then begin linksf:=1; rechtsf:=filenr end
             else begin linksf:=aktfile; rechtsf:=aktfile end;
ta:=upcase(readchar('Block begin: Trigger list','A'));
if not (ta in['A'..listmax]) then begin
   fehler('Undefined trigger list'); warte; exit end;
tb:=upcase(readchar('Block end: Trigger list','B'));
if not (tb in['A'..listmax]) then begin
   fehler('Undefined trigger list'); warte; exit end;
writeln;
for aktfile:=linksf to rechtsf do begin
   j:=0;
   with liste[aktfile], tliste[ta]^.fil[aktfile] do begin
      wandert:=block^;
      while wandert^.next<>nil do raus(wandert);
      for i:=1 to automn do begin
         tliste[tb]^.fil[aktfile].such(0,tliste[tb]^.fil[aktfile].automn,autom^[i],dummy,re);
            if (tliste[tb]^.fil[aktfile].autom^[re]<autom^[i+1]) and
               (tliste[tb]^.fil[aktfile].autom^[re]>autom^[i]) then begin
            rein(wandert);
            wandert^.von:=autom^[i];
            wandert^.bis:=tliste[tb]^.fil[aktfile].autom^[re];
            wandert:=wandert^.next;
            inc(j);
            end;
         end;
      end;
   writeln(liste[aktfile].name,' :',j,' blocks created.');
   end;
end;

procedure triggerdaten.nimm (var aut:triggerliste; gesamt:longint);
var   i:longint;
begin
getmem(autom,sizeof(messwert)*(gesamt+2));
for i:=1 to gesamt do autom^[i]:=aut[i]; automn:=gesamt; automda:=true;
autom^[0]:=-maxmesswert; autom^[automn+1]:=maxmesswert;
end;

procedure triggerdaten.store (var s:tbufstream);
begin
s.write(automda,sizeof(boolean));
if automda then begin
   s.write(automn,sizeof(automn));
   s.write(autom^,sizeof(messwert)*(automn+2));
   end;
end;

procedure triggerdaten.load (var s:tbufstream);
begin
s.read(automda,sizeof(boolean));
if automda then begin
   s.read(automn,sizeof(automn));
   getmem(autom,sizeof(messwert)*(automn+2));
   s.read(autom^,sizeof(messwert)*(automn+2));
   end     else automn:=0;
end;

procedure triggerdaten.frei;
begin
if automda then begin
   freemem(autom,sizeof(messwert)*(automn+2));
   automda:=false; automn:=0 end;
end;

procedure triggerdaten.such (links,rechts:exword; nach:messwert; var li,re:exword);
{ Binaeres Suchen zwischen "links" und "rechts" in der TL nach "nach": }
var   neu:exword;
begin
li:=links; re:=rechts;
while re-li>1 do begin
   neu:=li+(re-li) div 2;
   if autom^[neu]<=nach then li:=neu;
   if autom^[neu]>=nach then re:=neu;
   end;
if autom^[re]=nach then li:=re
                   else if autom^[li]=nach then re:=li;
end;

procedure matrix.eingabe;
var  bef:string[3];
     ti:char;
begin
for ti:='A' to listmax do tl[ti]:=[1..filenr];
bef:=readstring('Execution at line, column or position eg: 1, B, A2','');
if bef='' then begin unsinn:=false; escape:=true; exit end;
unsinn:=true; escape:=false;
if bef[1] in ['a'..'z','A'..'Z'] then begin
   for ti:='A' to listmax do if ti<>upcase(bef[1]) then tl[ti]:=[];
   delete(bef,1,1);
   unsinn:=false;
   end;
if bef[1] in ['0'..'9'] then begin
   for ti:='A' to listmax do tl[ti]:=tl[ti]*[zahl(bef)];
   unsinn:=false;
   end;
if unsinn then for ti:='A' to listmax do tl[ti]:=[];
end;

procedure zahlenmatrix.ausgabe;
var   fi:byte;
      ti:char;
procedure zeile;
var   ti:char;
begin
write(fi:3,':',liste[fi].namen:8,' ');
for ti:='A' to listmax do if fi in tl[ti] then write(tz[ti,fi]:6)
                                          else write('-':6);
writeln;
end;

begin
zn:=wherey;
write('File':12,' '); for ti:='A' to listmax do write(ti:6); writeln(lfcr);
for fi:=1 to filenr do zeile;
end;

procedure zahlenmatrix.erstsprung;
begin
wx:=14+(ord(tn)-ord('A'))*6;
wy:=zn+1+fn;
gotoxy(wx,wy);
end;

procedure zahlenmatrix.sprung;
begin
gotoxy(wx,wy);
end;

procedure zahlenmatrix.uebernehmen;
var   ti:char; fi:byte;
begin
for ti:='A' to listmax do begin
   tl[ti]:=[];
   for fi:=1 to maxfiles do with tliste[ti]^.fil[fi] do begin
      tz[ti,fi]:=automn;
      if automda then tl[ti]:=tl[ti]+[fi];
      end;
   end;
end;

{ trist }

procedure trist.beginn(trfile:byte);
var   di:dirstr; na:namestr; ext:extstr;
begin
gesamt:=0; zaehler:=1; abzaehler:=triggerabz; letztstelle:=-maxmesswert;
zn:=wherey;
mat.erstsprung; write(0:6);
fsplit(liste[trfile].name,di,na,ext);
gotoxy(1,zn); clreol;
write('Trigger event in ',na+ext:11,' at           ms: Abort: <Esc>');
abbruch:=keypressed and (readkey=#27);
end;

procedure trist.weiter (stelle:messwert);
var ausgabe:boolean;
begin
if stelle-letztstelle>=triggerdst then begin
   letztstelle:=stelle;
   if zaehler<triggeranf then inc(zaehler)
                         else
      if abzaehler<triggerabz then inc(abzaehler)
                              else begin
         inc(gesamt); aut^[gesamt]:=stelle;
         ausgabe:= (gesamt mod 128) = 0;
         if ausgabe then begin mat.sprung; write(gesamt:6) end;
         abzaehler:=1;
         end;
   end;
if ausgabe then begin gotoxy(32,zn); write(zeit(stelle):8) end;
if keypressed and (readkey=#27) then abbruch:=true;
end;

function trist.aufhoeren:boolean;
begin
aufhoeren:=(gesamt=triggeranz) or abbruch;
end;

{ triggerung }

function triggerung.fileanz:byte;
var   i,enn:byte;
begin
enn:=0; for i:=1 to filenr do if fil[i].automda then inc(enn);
fileanz:=enn;
end;

function triggerung.erstfile:byte;
var   fi:byte;
begin
fi:=1; while not fil[fi].automda and (fi<filenr) do inc(fi);
erstfile:=fi;
end;

function triggerung.triggsum:exword;
var   i:byte; summe:exword;
begin
summe:=0; for i:=1 to filenr do inc(summe,fil[i].automn);
triggsum:=summe;
end;

procedure triggerung.neu;
var   i:byte;
begin
for i:=1 to maxfiles do with fil[i] do begin
   automda:=false; automn:=0 end;
tr:=akttrkan;
end;

constructor triggerung.load (var s:tbufstream);
var   i:byte;
begin
s.read(name,sizeof(name)); s.read(tr,1);
for i:=1 to maxfiles do fil[i].load(s);
end;

procedure triggerung.store (var s:tbufstream);
var   i:byte;
begin
s.write(name,sizeof(name)); s.write(tr,1);
for i:=1 to maxfiles do fil[i].store(s);
end;

procedure triggerung.triggern (dabei:filemenge);
label genug;
var   trfile:byte;
      wandert:listenzeiger;

begin
for trfile:=1 to filenr do if trfile in dabei then
 with fil[trfile], liste[trfile] do begin
   mat.fn:=trfile;
   verfol.beginn(trfile); if abbruch then exit;
   oeffnen(trfile);
   if bloecke then begin
      wandert:=block^;
      while wandert^.next<>nil do begin
         blocktriggern(wandert^.von,wandert^.bis);
         if verfol.aufhoeren then goto genug;
         wandert:=wandert^.next end
      end
              else blocktriggern(0,laenge);
 genug:
   nimm(aut^,verfol.gesamt);
   schliesse;
   if abbruch then exit;
   end;
piep;
end;

procedure triggerung.blocktriggern (von,bis:messwert);
begin abstract end;

destructor triggerung.alt;
var   i:byte;
begin
for i:=1 to filenr do fil[i].frei;
end;

constructor keine.neu;
begin
triggerung.neu;
name:='undefined'; tr:=maxkanal;
end;

procedure keine.triggern (dabei:filemenge);
begin end;

constructor punkte.neu;
begin
triggerung.neu; tr:=maxkanal;
name:='user defined points';
end;

{ Alte Prozedur ohne Bloecke...
procedure punkte.triggern (dabei:filemenge);
var   trfile:byte;
      pwandert:punktzeiger;
begin
for trfile:=1 to filenr do if trfile in dabei then
 with fil[trfile], liste[trfile] do begin
   mat.fn:=trfile;
   verfol.beginn(trfile); if abbruch then exit;
(*   oeffnen(trfile); *)
   pwandert:=selbst^;
   while (pwandert^.next<>nil) and not verfol.aufhoeren do begin
      verfol.weiter(pwandert^.bei);
      pwandert:=pwandert^.next end;
   nimm(aut^,verfol.gesamt);
(*   schliesse;  *)
   if abbruch then exit;
   end;
end;
}

procedure punkte.triggern (dabei:filemenge);
label genug;
var   trfile:byte;
      wandert:listenzeiger;
      pwandert:punktzeiger;
procedure punkteblocktriggern(von,bis:messwert);
begin
while (pwandert^.next<>nil) and (pwandert^.bei<von) do pwandert:=pwandert^.next;
while (pwandert^.next<>nil) and (pwandert^.bei<=bis) do begin
      verfol.weiter(pwandert^.bei);
      pwandert:=pwandert^.next end;
end;
begin
for trfile:=1 to filenr do if trfile in dabei then
 with fil[trfile], liste[trfile] do begin
   mat.fn:=trfile;
   verfol.beginn(trfile); if abbruch then exit;
   pwandert:=selbst^;
   if bloecke then begin
      wandert:=block^;
      while wandert^.next<>nil do begin
         punkteblocktriggern(wandert^.von,wandert^.bis);
         if verfol.aufhoeren then goto genug;
         wandert:=wandert^.next end
      end
              else punkteblocktriggern(0,laenge);
 genug:
   nimm(aut^,verfol.gesamt);
   if abbruch then exit;
   end;
piep;
end;

constructor gelesen.neu;
var  eingabe:text;
     dateiname:string80;
     autfil:array[1..maxfiles] of ^triggerliste;
     nfil:array[1..maxfiles] of exword;
     trfile:byte;
     fn:exword; te:extended;
     znr:grossint;
label abbruch;
begin
triggerung.neu; tr:=maxkanal;
dateiname:=readstring('File name and path','trigger.txt');
name:='from file '+dateiname;
for trfile:=1 to filenr do begin new(autfil[trfile]); nfil[trfile]:=0 end;
znr:=0;
assign(eingabe,dateiname);
reset(eingabe);
while not eof(eingabe) do begin
   inc(znr); read(eingabe,fn,te);
   if (ioresult=0) and (fn in [1..filenr]) then begin
      inc(nfil[fn]); autfil[fn]^[nfil[fn]]:=messwext(te) end
      else
      if znr>3 then begin
         fehler('Invalid line '+wort(znr)+' in import file'); warte; goto abbruch
         end;
   readln(eingabe);
   end;
abbruch:close(eingabe);
for trfile:=1 to filenr do begin
   fil[trfile].nimm(autfil[trfile]^,nfil[trfile]);
   dispose(autfil[trfile]);
   end;
end;

procedure gelesen.triggern (dabei:filemenge);
begin end;

constructor aequidistant.neu;
begin
triggerung.neu; tr:=maxkanal;
anfa:=messwext(readext('Start [ms]',0,1,1));
dist:=messwext(readext('Distance [ms]',0,1,1));
name:=extwort(extzeit(anfa),3,1)+' ms + equidistant '
      +extwort(extzeit(dist),3,1)+' ms';
end;

{ Alte Prozedur ohne Bloecke

procedure aequidistant.triggern (dabei:filemenge);
var   trfile:byte;
      hierbei:messwert;
begin
for trfile:=1 to filenr do if trfile in dabei then
 with fil[trfile], liste[trfile] do begin
   mat.fn:=trfile;
   verfol.beginn(trfile); if abbruch then exit;
   oeffnen(trfile);
   hierbei:=anfa;
   while (laenge>=hierbei) and not verfol.aufhoeren do begin
      verfol.weiter(hierbei);
      hierbei:=hierbei+dist;
      end;
   nimm(aut^,verfol.gesamt);
   schliesse;
   if abbruch then exit;
   end;
end;}

procedure aequidistant.blocktriggern (von,bis:messwert);
var hierbei:messwert;
begin
hierbei:=anfa+von;
while (bis>hierbei) and not verfol.aufhoeren do begin
   verfol.weiter(hierbei);
   hierbei:=hierbei+dist;
   end;
end;

constructor aequidistant.load (var s:tbufstream);
begin
triggerung.load(s);
s.read(dist,sizeof(messwert));
s.read(anfa,sizeof(messwert));
end;

procedure aequidistant.store (var s:tbufstream);
begin
triggerung.store(s);
s.write(dist,sizeof(messwert));
s.write(anfa,sizeof(messwert));
end;

procedure schwelle.neu;
var   einstr:string20;
begin
triggerung.neu;
einstr:=belegungsliste[tr].einhwort;
schw:=norm(readext('Threshold ['+einstr+']',0,1,1),tr);
name:=extwort(extspannung(schw,tr),2,1)+' '+einstr+' - ';
end;

constructor schwelle.load (var s:tbufstream);
begin
triggerung.load(s);
s.read(schw,sizeof(wert));
end;

procedure schwelle.store (var s:tbufstream);
begin
triggerung.store(s);
s.write(schw,sizeof(wert));
end;

constructor hoch.neu;
begin
schwelle.neu;
name:=name+'rising threshold';
end;

procedure hoch.blocktriggern (von,bis:messwert);
var   stekorr,vonkorr,biskorr:grossint;
begin
vonkorr:=trunc(von*korr)+1; biskorr:=trunc(bis*korr);
stekorr:=vonkorr;
repeat
   while (dat(stekorr,tr)>=schw) and (stekorr<=biskorr) do inc(stekorr);
   while (dat(stekorr,tr)<schw) and (stekorr<=biskorr) do inc(stekorr);
   if stekorr>biskorr then exit;
   verfol.weiter((stekorr-0.5)/korr);
until verfol.aufhoeren;
end;

constructor runter.neu;
begin
schwelle.neu;
name:=name+'falling threshold';
end;

procedure runter.blocktriggern (von,bis:messwert);
var   stekorr,vonkorr,biskorr:grossint;
begin
vonkorr:=trunc(von*korr)+1; biskorr:=trunc(bis*korr);
stekorr:=vonkorr;
repeat
   while (dat(stekorr,tr)<=schw) and (stekorr<=biskorr) do inc(stekorr);
   while (dat(stekorr,tr)>schw) and (stekorr<=biskorr) do inc(stekorr);
   if stekorr>biskorr then exit;
   verfol.weiter((stekorr-0.5)/korr);
until verfol.aufhoeren;
end;

procedure extremum.neu;
begin
triggerung.neu;
nmittel:=messw(readint('Width of gliding Average [ms]',100));
bmittel:=messw(readint('Lower limit of signal deviation [ms]',10));
name:='';
end;

constructor extremum.load (var s:tbufstream);
begin
triggerung.load(s);
s.read(bmittel,sizeof(bmittel)); s.read(nmittel,sizeof(nmittel));
end;

procedure extremum.store (var s:tbufstream);
begin
triggerung.store(s);
s.write(bmittel,sizeof(bmittel)); s.write(nmittel,sizeof(nmittel));
end;

procedure gleitschw.beginn (nmittel,bmittel:extended);
begin
n2korr:=round(nmittel*korr) div 2; nkorr:=n2korr*2+1;
end;

procedure gleitschw.mitteln (stekorr:grossint; tr:byte);
var i:grossint;
begin
schw:=0;
i:=stekorr-n2korr;
while i<=stekorr+n2korr do begin schw:=schw+dat(i,tr); inc(i) end;
schw:=schw/nkorr;
end;

procedure gleitschw.zaehlt (var stekorr:grossint; tr:byte);
begin
inc(stekorr);
schw:=schw-(dat(stekorr-n2korr-1,tr)-dat(stekorr+n2korr,tr))/nkorr;
end;

constructor minimum.neu;
begin
extremum.neu;
name:='minimum < gliding average';
end;

procedure minimum.blocktriggern (von,bis:messwert);
var   l,r:grossint;
      hilf,extr:grossint;
      stekorr,vonkorr,biskorr,bkorr:grossint;
      glesch:gleitschw;

begin
vonkorr:=trunc(von*korr)+1; biskorr:=trunc(bis*korr);
r:=vonkorr; glesch.beginn(nmittel,bmittel); glesch.mitteln(r,tr);
bkorr:=trunc(bmittel*korr)+1;
repeat
   repeat
      while (dat(r,tr)<=glesch.schw) and (r<=biskorr) do glesch.zaehlt(r,tr);
      while (dat(r,tr)>glesch.schw) and (r<=biskorr) do glesch.zaehlt(r,tr);
      if r>biskorr then exit;
      l:=r; extr:=maxsample; hilf:=dat(l,tr);
      while hilf<glesch.schw do begin
         if extr>hilf then begin extr:=hilf; stekorr:=r end;
         glesch.zaehlt(r,tr); if r>biskorr then exit;
         hilf:=dat(r,tr) end;
      until r-l>=bkorr;
      verfol.weiter(stekorr/korr);
until verfol.aufhoeren;
end;

constructor maximum.neu;
begin
extremum.neu;
name:='maximum > gliding average';
end;

procedure maximum.blocktriggern (von,bis:messwert);
var   l,r:grossint;
      hilf,extr:grossint;
      stekorr,vonkorr,biskorr,bkorr:grossint;
      glesch:gleitschw;

begin
vonkorr:=trunc(von*korr)+1; biskorr:=trunc(bis*korr);
r:=vonkorr; glesch.beginn(nmittel,bmittel); glesch.mitteln(r,tr);
bkorr:=trunc(bmittel*korr)+1;
repeat
   repeat
      while (dat(r,tr)>=glesch.schw) and (r<=biskorr) do glesch.zaehlt(r,tr);
      while (dat(r,tr)<glesch.schw) and (r<=biskorr) do glesch.zaehlt(r,tr);
      if r>biskorr then exit;
      l:=r; extr:=minsample; hilf:=dat(l,tr);
      while hilf>glesch.schw do begin
         if extr<hilf then begin extr:=hilf; stekorr:=r end;
         glesch.zaehlt(r,tr); if r>biskorr then exit;
         hilf:=dat(r,tr) end;
      until r-l>=bkorr;
      verfol.weiter(stekorr/korr);
until verfol.aufhoeren;
end;

procedure fenster.neu;
var   einstr:string20;
begin
triggerung.neu;
einstr:=belegungsliste[tr].einhwort;
schwunten:=norm(readext('Lower threshold ['+einstr+']',0,1,1),tr);
schwoben:=norm(readext('Upper threshold ['+einstr+']',0,1,1),tr);
name:=extwort(extspannung(schwunten,tr),2,1)+' '+einstr+' - '+
      extwort(extspannung(schwoben,tr),2,1)+' '+einstr+' - ';
end;

constructor fenster.load (var s:tbufstream);
begin
triggerung.load(s);
s.read(schwunten,sizeof(wert)); s.read(schwoben,sizeof(wert));
end;

procedure fenster.store (var s:tbufstream);
begin
triggerung.store(s);
s.write(schwunten,sizeof(wert)); s.write(schwoben,sizeof(wert));
end;

constructor fenstermaximum.neu;
begin
fenster.neu;
name:=name+'maximum in window';
end;

procedure fenstermaximum.blocktriggern (von,bis:messwert);
label zuhoch;
var   l,r:grossint;
      hilf,extr:wert;
      stekorr,vonkorr,biskorr,vorlaeufigkorr:grossint;

begin
vonkorr:=trunc(von*korr)+1; biskorr:=trunc(bis*korr);
r:=vonkorr;
repeat
  zuhoch:
    l:=r;
    while (dat(l,tr)>=schwunten) and (l<=biskorr) do inc(l);
    while (dat(l,tr)<schwunten) and (l<=biskorr) do inc(l);
    if l>biskorr then exit;
    r:=l;
    while (dat(r,tr)>=schwunten) and (r<=biskorr) do inc(r);
    if r>biskorr then exit;
    extr:=schwunten; vorlaeufigkorr:=l;
    stekorr:=l;
    while stekorr<=r do begin
       hilf:=dat(stekorr,tr);
       if hilf>extr then begin
          if hilf>schwoben then goto zuhoch;
          extr:=hilf; vorlaeufigkorr:=stekorr;
          end;
       inc(stekorr)
       end;
    verfol.weiter(vorlaeufigkorr/korr);
until verfol.aufhoeren;
end;

constructor fensterminimum.neu;
begin
fenster.neu;
name:=name+'minimum in window';
end;

procedure fensterminimum.blocktriggern (von,bis:messwert);
label zuniedrig;
var   l,r:grossint;
      hilf,extr:wert;
      stekorr,vonkorr,biskorr,vorlaeufigkorr:grossint;

begin
vonkorr:=trunc(von*korr)+1; biskorr:=trunc(bis*korr);
r:=vonkorr;
repeat
  zuniedrig:
    l:=r;
    while (dat(l,tr)<=schwoben) and (l<=biskorr) do inc(l);
    while (dat(l,tr)>schwoben) and (l<=biskorr) do inc(l);
    if l>biskorr then exit;
    r:=l;
    while (dat(r,tr)<=schwoben) and (r<=biskorr) do inc(r);
    if r>biskorr then exit;
    extr:=schwoben; vorlaeufigkorr:=l;
    stekorr:=l;
    while stekorr<=r do begin
       hilf:=dat(stekorr,tr);
       if hilf<extr then begin
          if hilf<schwunten then goto zuniedrig;
          extr:=hilf; vorlaeufigkorr:=stekorr;
          end;
       inc(stekorr)
       end;
    verfol.weiter(vorlaeufigkorr/korr);
until verfol.aufhoeren;
end;

{ Ein- und Austritt }

constructor eintritt.neu;
begin
fenster.neu;
name:=name+'entering window';
end;

procedure eintritt.blocktriggern (von,bis:messwert);
var   stekorr,vonkorr,biskorr:grossint;
      hilf:wert;
begin
vonkorr:=trunc(von*korr)+1; biskorr:=trunc(bis*korr);
stekorr:=vonkorr;
repeat
   hilf:=dat(stekorr,tr);
   while (hilf>=schwunten) and (hilf<=schwoben) and (stekorr<=biskorr) do
      begin inc(stekorr); hilf:=dat(stekorr,tr) end;
   while ((hilf<schwunten) or (hilf>schwoben)) and (stekorr<=biskorr) do
      begin inc(stekorr); hilf:=dat(stekorr,tr) end;
   if stekorr>biskorr then exit;
   verfol.weiter((stekorr-0.5)/korr);
until verfol.aufhoeren;
end;

constructor austritt.neu;
begin
fenster.neu;
name:=name+'leaving window';
end;

procedure austritt.blocktriggern (von,bis:messwert);
var   stekorr,vonkorr,biskorr:grossint;
      hilf:wert;
begin
vonkorr:=trunc(von*korr)+1; biskorr:=trunc(bis*korr);
stekorr:=vonkorr;
repeat
   hilf:=dat(stekorr,tr);
   while ((hilf<schwunten) or (hilf>schwoben)) and (stekorr<=biskorr) do
      begin inc(stekorr); hilf:=dat(stekorr,tr) end;
   while (hilf>=schwunten) and (hilf<=schwoben) and (stekorr<=biskorr) do
      begin inc(stekorr); hilf:=dat(stekorr,tr) end;
   if stekorr>biskorr then exit;
   verfol.weiter((stekorr-0.5)/korr);
until verfol.aufhoeren;
end;

{ triggerfilter }

procedure triggerfilter.vorbereitung (frequenz:extended);
begin
trdaten:=tliste[trliste]^.fil[offennr];
end;

constructor triggerfilter.load (var s:tbufstream);
begin
filter.load(s);
s.read(trliste,1);
end;

procedure triggerfilter.store (var s:tbufstream);
begin
filter.store(s);
s.write(trliste,1);
end;

{ doppeltriggerfilter }

procedure doppeltriggerfilter.vorbereitung (frequenz:extended);
begin
retrdaten:=tliste[retrliste]^.fil[offennr];
ertrdaten:=tliste[ertrliste]^.fil[offennr];
end;

constructor doppeltriggerfilter.load (var s:tbufstream);
begin
filter.load(s);
s.read(retrliste,1); s.read(ertrliste,1);
end;

procedure doppeltriggerfilter.store (var s:tbufstream);
begin
filter.store(s);
s.write(retrliste,1); s.write(ertrliste,1);
end;

{ zaehltfilter }

constructor zaehltfilter.neu(trigliste:char);
begin
trliste:=trigliste;
name:='Counting of TL '+trliste;
end;

procedure zaehltfilter.einheitgenerieren(var beleg:belegung);
begin
inherited einheitgenerieren(beleg);
with beleg do begin
   faktor:=1;
   anfang:='#';
   sekunde:=0;
   negativ:=false;
   end;
end;

function zaehltfilter.gefiltert (posi:grossint):sample;
const maxminsample=maxsample-minsample;
var   li,re:exword;
begin
with trdaten do begin
   such(0,automn+1,posi/korr,li,re);
   if li<=maxsample then gefiltert:=li else gefiltert:=maxsample;
   end;
end;

{ punktefilter }

constructor punktefilter.neu(trigliste:char);
begin
trliste:=trigliste;
name:='Points TL '+trliste;
end;

procedure punktefilter.einheitgenerieren(var beleg:belegung);
begin
inherited einheitgenerieren(beleg);
with beleg do begin
   gepunktet:=true;
   gepunktettl:=trliste;
   end;
end;

{ freqfilter }

constructor freqfilter.neu(trigliste:char);
begin
trliste:=trigliste;
name:='Frequency of TL '+trliste;
end;

procedure freqfilter.einheitgenerieren(var beleg:belegung);
begin
inherited einheitgenerieren(beleg);
with beleg do begin
   faktor:=fre/maxsample;
   anfang:='1';
   sekunde:=-1;
   negativ:=false;
   end;
end;

function freqfilter.gefiltert (posi:grossint):sample;
var   li,re:exword;
begin
with trdaten do begin
   such(0,automn+1,posi/korr,li,re);
   if (li=re) and (li>0) then dec(li);
   if (li=0) or (re=automn+1) then gefiltert:=0
             else gefiltert:=round(maxsample/(autom^[re]-autom^[li]));
   end;
end;

{ intervallfilter }

constructor intervallfilter.neu(trigliste:char);
begin
trliste:=trigliste;
name:='Interval of TL '+trliste;
end;

procedure intervallfilter.einheitgenerieren(var beleg:belegung);
begin
inherited einheitgenerieren(beleg);
with beleg do begin
   faktor:=1/fre;
   anfang:='';
   sekunde:=1;
   negativ:=false;
   end;
end;

function intervallfilter.gefiltert (posi:grossint):sample;
var   li,re:exword;
      t:extended;
begin
with trdaten do begin
   such(0,automn+1,posi/korr,li,re);
   if (li=re) and (li>0) then dec(li);
   if (li=0) or (re=automn+1) then gefiltert:=0
             else begin
      t:=autom^[re]-autom^[li];
      if t<maxsample then gefiltert:=round(t) else gefiltert:=maxsample;
      end;
   end;
end;

{ polygonfilter }

constructor polygonfilter.neu(trigliste:char);
begin
trliste:=trigliste;
name:='Polygon ('+trliste+')';
end;

procedure polygonfilter.vorbereitung (frequenz:extended);
begin
triggerfilter.vorbereitung(frequenz);
erneut:=false;
end;

function polygonfilter.gefiltert (posi:grossint):sample;
var   li,re:exword;
begin
with trdaten do begin
   such(0,automn+1,posi/korr,li,re);
   if li<1 then li:=1; if re>automn then re:=automn;
   if not erneut or (li<>lialt) or (re<>realt) then begin
      lialt:=li; realt:=re; erneut:=true;
      liposi:=zwi(autom^[li]); reposi:=zwi(autom^[re]);
      liwert:=next^.gefiltert(liposi); rewert:=next^.gefiltert(reposi);
      end;
   if li=re then gefiltert:=round(liwert)
            else gefiltert:=trunc(((reposi-posi)*liwert+(posi-liposi)*rewert)
                             /(reposi-liposi));
   end;
end;

{ asciifilter }

constructor asciifilter.neu(aname:string; trigliste:char);
var   afile:text;
      i:grossint; dummy:extended;
      atmax:ywert;
begin
trliste:=trigliste;
fillchar(aliste,(sizeof(aliste)),0);
name:='ASCII - Read Error "'+aname+'"';
amax:=1;
assign(afile,aname);
reset(afile); if lesefehler then exit;
i:=0; atmax:=0;
while not eof(afile) do begin
   inc(i);
   readln(afile,dummy,aliste[i]);
   if abs(aliste[i])>atmax then atmax:=abs(aliste[i]);
   if atmax>0 then amax:=atmax;
   if lesefehler or (i>=triggermax) then exit;
   end;
close(afile);
name:='ASCII ('+trliste+'+'+aname+')';
end;

procedure asciifilter.vorbereitung (frequenz:extended);
begin
triggerfilter.vorbereitung(frequenz);
end;

procedure asciifilter.einheitgenerieren(var beleg:belegung);
begin
inherited einheitgenerieren(beleg);
with beleg do begin
   faktor:=amax/maxsample/0.9;
   anfang:='U';
   sekunde:=0;
   negativ:=true;
   end;
end;

function asciifilter.gefiltert (posi:grossint):sample;
var   li,re:exword;
      liposi,reposi:grossint;
      liwert,rewert:extended;
begin
with trdaten do begin
   such(0,automn+1,posi/korr,li,re);
   if li<1 then li:=1; if re>automn then re:=automn;
   liposi:=zwi(autom^[li]); reposi:=zwi(autom^[re]);
   liwert:=aliste[li]/amax*(maxsample*0.9); rewert:=aliste[re]/amax*(maxsample*0.9);
   if li=re then gefiltert:=round(liwert)
            else gefiltert:=trunc(((reposi-posi)*liwert+(posi-liposi)*rewert)
                             /(reposi-liposi));
   end;
end;

constructor asciifilter.load (var s:tbufstream);
begin
triggerfilter.load(s);
s.read(aliste,sizeof(alistentyp));
s.read(amax,sizeof(ywert));
end;

procedure asciifilter.store (var s:tbufstream);
begin
triggerfilter.store(s);
s.write(aliste,sizeof(alistentyp));
s.write(amax,sizeof(ywert));
end;

{ diffilter - alt}

constructor diffilteralt.neu(retrigliste,ertrigliste:char; msmax:grossint);
begin
retrliste:=retrigliste; ertrliste:=ertrigliste;
smax:=msmax/1000;
name:='Diff. TL '+retrliste+ ' minus '+ertrliste
      +' (ñ'+wort(msmax)+'ms)';
end;

procedure diffilteralt.einheitgenerieren(var beleg:belegung);
begin
inherited einheitgenerieren(beleg);
with beleg do begin
   faktor:=smax/maxsample;
   anfang:='';
   sekunde:=1;
   negativ:=true;
   end;
end;

function diffilteralt.gefiltert (posi:grossint):sample;
var   stelle:messwert;
      li,re:exword;
      rebei,erbei:messwert;
begin
stelle:=posi/korr;
with retrdaten do begin
   such(0,automn+1,stelle,li,re);
   if (stelle-autom^[li])>(autom^[re]-stelle) then rebei:=autom^[re]
                                              else rebei:=autom^[li];
   end;
with ertrdaten do begin
   such(0,automn+1,rebei,li,re);
   if (li=0) or (re=automn+1) then begin
      gefiltert:=0; exit end;
   if (rebei-autom^[li])>(autom^[re]-rebei) then erbei:=autom^[re]
                                            else erbei:=autom^[li];
   end;
gefiltert:=round((erbei-rebei)/fre/smax*maxsample);
end;

constructor diffilteralt.load (var s:tbufstream);
begin
doppeltriggerfilter.load(s);
s.read(smax,sizeof(extended));
end;

procedure diffilteralt.store (var s:tbufstream);
begin
doppeltriggerfilter.store(s);
s.write(smax,sizeof(extended));
end;

{ diffilter - neu }

constructor diffilter.neu(retrigliste,ertrigliste:char; msmax:grossint; typchar:char);
begin
retrliste:=retrigliste; ertrliste:=ertrigliste;
case typchar of 'F':typ:=vorwaerts; 'B':typ:=rueckwaerts; else typ:=naechster; end;
smax:=msmax/1000;
name:='Diff. TL '+retrliste+ ' minus '+ertrliste
      +' (ñ'+wort(msmax)+'ms,'+typchar+')';
end;

procedure diffilter.einheitgenerieren(var beleg:belegung);
begin
inherited einheitgenerieren(beleg);
with beleg do begin
   faktor:=smax/maxsample;
   anfang:='';
   sekunde:=1;
   negativ:=true;
   end;
end;

function diffilter.gefiltert (posi:grossint):sample;
var   stelle:messwert;
      li,re:exword;
      rebei,erbei:messwert;
begin
stelle:=posi/korr;
with retrdaten do begin
   such(0,automn+1,stelle,li,re);
   if (stelle-autom^[li])>(autom^[re]-stelle) then rebei:=autom^[re]
                                              else rebei:=autom^[li];
   end;
with ertrdaten do begin
   such(0,automn+1,rebei,li,re);
   if (li=0) or (re=automn+1) then begin
      gefiltert:=0; exit end;
   case typ of
      naechster: if (rebei-autom^[li])>(autom^[re]-rebei) then erbei:=autom^[re]
                                                         else erbei:=autom^[li];
      vorwaerts: erbei:=autom^[re];
      rueckwaerts: erbei:=autom^[li];
      end;
   end;
gefiltert:=round((erbei-rebei)/fre/smax*maxsample);
end;

constructor diffilter.load (var s:tbufstream);
begin
doppeltriggerfilter.load(s);
s.read(smax,sizeof(extended));
end;

procedure diffilter.store (var s:tbufstream);
begin
doppeltriggerfilter.store(s);
s.write(smax,sizeof(extended));
end;

{ phasenfilter }

constructor phasenfilter.neu(retrigliste,ertrigliste:char; perioden:grossint);
begin
retrliste:=retrigliste; ertrliste:=ertrigliste; peri:=perioden;
if peri=0 then name:='Phase TL '+ertrliste+' in '+retrliste
          else
   if peri>0 then name:='Phase TL '+ertrliste+' in '+retrliste+'+'+wort(peri)
             else name:='Phase TL '+ertrliste+' in '+retrliste+wort(peri);
end;

procedure phasenfilter.einheitgenerieren (var beleg:belegung);
begin
inherited einheitgenerieren(beleg);
with beleg do begin
   faktor:=1/maxsample;
   anfang:='';
   sekunde:=0;
   negativ:=false;
   end;
end;

function phasenfilter.gefiltert (posi:grossint):sample;
var   stelle:messwert;
      li,re{,lialt}:exword;
      lip,rep:grossint;
      repbeili,repbeire,rebeili,erbei:messwert;
begin
stelle:=posi/korr;
with ertrdaten do begin
   such(0,automn+1,stelle,li,re);
{ Begrenzung auf die Referenzphase, faellt wohl mit peri weg
   lialt:=li;
   if autom^[li]<rebeili then li:=re;
   if autom^[re]>rebeire then re:=lialt;
   if re<li then begin gefiltert:=0; exit end;}
   if (stelle-autom^[li])>(autom^[re]-stelle) then erbei:=autom^[re]
                                              else erbei:=autom^[li];
with retrdaten do begin
   such(0,automn+1,erbei,li,re);
   if li=re then inc(re);
   lip:=li+peri; rep:=re+peri;
   if (lip<=0) or (rep>=automn+1) or (li<=0) then begin
      gefiltert:=0; exit end;
   repbeili:=autom^[lip]; repbeire:=autom^[rep]; rebeili:=autom^[li];
   end;
   end;
gefiltert:=round((erbei-rebeili)/(repbeire-repbeili)*maxsample);
end;

constructor phasenfilter.load (var s:tbufstream);
begin
doppeltriggerfilter.load(s);
s.read(peri,sizeof(grossint));
end;

procedure phasenfilter.store (var s:tbufstream);
begin
doppeltriggerfilter.store(s);
s.write(peri,sizeof(grossint));
end;

{ TL-Integrationsfilter }

constructor tlintfilter.neu(trigliste:char);
begin
trliste:=trigliste;
name:='* dt [TL '+trliste+']';
end;

procedure tlintfilter.einheitgenerieren (var beleg:belegung);
begin
filter.einheitgenerieren(beleg);
with beleg do begin faktor:=faktor/fre; inc(sekunde) end;
end;

procedure tlintfilter.vorbereitung (frequenz:extended);
begin
triggerfilter.vorbereitung(frequenz);
posiwert:=0; gefiltertwert:=0;
end;

function tlintfilter.gefiltert (posi:grossint):sample;
var  i:grossint;
     li,re:exword;
begin
with trdaten do begin
   such(0,automn+1,posi/korr,li,re);
   if (re<=automn) then if autom^[re]<=posiwert/korr then
            if li=0 then begin posiwert:=0; gefiltertwert:=0 end
                    else begin posiwert:=zwi(autom^[li]); gefiltertwert:=0 end;
   if (li>0) then if autom^[li]>=posiwert/korr then begin posiwert:=zwi(autom^[li]); gefiltertwert:=0 end;
   end;
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

{ triggerweiser }

procedure triggerweiser.zaehlen (var feld:triggerung; minabst,maxabst:messwert);
var   nr,tp,n:longint;
      abst:messwert;
      sum:extended;
begin
gesamt:=0; sum:=0;
for nr:=1 to filenr do with weisliste[nr], feld, fil[nr] do begin
   l:=max(automn-1,0);
   getmem(t,sizeof(exword)*l);
   if automda then begin
      n:=0;
      for tp:=1 to automn-1 do begin
         abst:=autom^[tp+1]-autom^[tp];
         if (abst<=maxabst) and (abst>=minabst) then begin
            inc(n); t^[n]:=tp; sum:=sum+abst;  end;
         end;
      weisliste[nr].n:=n;
      inc(gesamt,n);
      end;
   end;
if gesamt<>0 then mittelabstand:=sum/gesamt;
end;

procedure triggerweiser.frei;
var   nr:exword;
begin
for nr:=1 to filenr do with weisliste[nr] do freemem(t,sizeof(exword)*l);
end;


procedure kontrolle (nr:byte; trind:char);
var   pzeiger,philf:punktzeiger;
      i:longint;
begin
if trind in ['A'..listmax] then
 with liste[nr], tliste[trind]^.fil[nr] do begin
   pzeiger:=selbst^^.next;
   while pzeiger<>nil do begin
      philf:=pzeiger^.vor; pzeiger^.vor:=philf^.vor;
      dispose(philf,done);
      selbst^:=pzeiger; pzeiger:=pzeiger^.next;
      end;
   pzeiger:=selbst^^.vor;
   philf:=selbst^;
   for i:=1 to automn do begin
      new(pzeiger^.next,neu); pzeiger^.next^.vor:=pzeiger;
      pzeiger:=pzeiger^.next;
      pzeiger^.bei:=autom^[i] end;
   pzeiger^.next:=philf; philf^.vor:=pzeiger;
   end;
end;

procedure streamput (var s:tbufstream);
var   ind:char;
begin
s.write(akttrkan,sizeof(akttrkan));
for ind:='A' to listmax do s.put(tliste[ind]);
end;

procedure streamget (var s:tbufstream);
var   ind:char;
begin
s.read(akttrkan,sizeof(akttrkan));
if komp84 then
   for ind:='A' to 'H' do begin dispose(tliste[ind],alt); tliste[ind]:=triggerungzg(s.get); end
          else
   for ind:='A' to listmax do begin dispose(tliste[ind],alt); tliste[ind]:=triggerungzg(s.get); end;
end;

procedure triggeruebersicht;
var   trind:char;
begin
writeln('List':5,'Channel':8,'Evts.':8,'Files':6,'  Mode and Label');
for trind:='A' to listmax do with tliste[trind]^ do begin
   writeln(trind:2,schriftliste[tr]:11,triggsum:8,fileanz:4,'    ',tliste[trind]^.name);
   end;
end;


procedure manager;
var   trind:char;
      i:longint;

procedure aktuellkanal;
var   liste:filterliste;
      i:byte;
begin
ueberschrift(false,'Trigger Channel','Info',farbe3);
belegungzeigen; writeln;
liste.zeigen(8,kan); writeln;
zwischen('Dialogue',farbe3);
write(lfcr,'Continue list? (Y/N) ');
while not liste.ende and (readkey in ['Y','y','J','j']) do liste.weiterzeigen;
write(#13); clreol;
i:=readint('Trigger channel',0);
if not (i in [0..kan+filtermax-1]) then begin
   fehler('Undefined channel no.'); warte end
                                   else akttrkan:=i;
end;

procedure triggneu;
const ta:char='A';
var   artbu:char;
begin
ueberschrift(false,'Trigger Mode','Info',farbe3);
writeln('Trigger channel: ',akttrkan,' (',schriftliste[akttrkan],')',lfcr);
triggeruebersicht; writeln;
writeln('Modes:   r = Rising Threshold              f = Falling Threshold',lfcr,
        '       x/a = Max. > Gl. Av./in Window    n/i = Min. < Gl. Av./in Window',lfcr,
{        '       a = Maximum in Window          i = Minimum in Window',lfcr,}
        '         > = Entering Window               < = Leaving Window',lfcr,
        '         p = User Defined                  e = Equidistant',lfcr,
        '         t = Read from text file           u = Undefined');
{writeln;}
gotoxy(1,21); zwischen('Dialogue',farbe3);
window(1,25,80,zeilmax);
ta:=upcase(readchar('Trigger list','A'));
if not (ta in['A'..listmax]) then begin
   fehler('Undefined trigger list'); warte; exit end;
artbu:=readchar('Trigger mode','r');
if not (artbu in ['u','r','f','x','n','p','a','i','e','>','<','t']) then begin
   fehler('Undefined trigger mode'); warte; exit end;
dispose(tliste[ta],alt);
case artbu of
   'u':tliste[ta]:=new(keinezg,neu);    'r':tliste[ta]:=new(hochzg,neu);
   'f':tliste[ta]:=new(runterzg,neu);   'x':tliste[ta]:=new(maximumzg,neu);
   'n':tliste[ta]:=new(minimumzg,neu);  'p':tliste[ta]:=new(punktezg,neu);
   'a':tliste[ta]:=new(fenstermaximumzg,neu);
   'i':tliste[ta]:=new(fensterminimumzg,neu);
   '<':tliste[ta]:=new(austrittzg,neu); '>':tliste[ta]:=new(eintrittzg,neu);
   'e':tliste[ta]:=new(aequidistantzg,neu);
   't':tliste[ta]:=new(gelesenzg,neu);
   end;
with tliste[ta]^ do name:=readstring('Label',name);
end;

procedure konditionen;
var   puff:longint;
begin
ueberschrift(false,'Trigger Conditions','Info',farbe3);
writeln('Trigger range (complete Files or Blocks) : ',bloecketext[bloecke],
   lfcr,'Maximum number of trigger points         : ',triggeranz,
   lfcr,'Start at trigger event no.               : ',triggeranf,
   lfcr,'Trigger point selection each             : ',triggerabz,'. event',
   lfcr,'Skip trigger events up to                : ',zeit(triggerdst),' ms');
writeln;
zwischen('Dialogue',farbe3); writeln;
bloecke:=upcase(
     readchar('Trigger range (f=complete Files, b=Blocks)                 ',
   bloeckeb[bloecke]))=upcase(bloeckeb[true]);
puff:=readint('Maximum number of trigger points per file (max.'+wort(triggermax)+')    ',triggeranz);
if (puff<=0) or (puff>triggermax) then begin
   fehler('Number of trigger points out of range.'); warte end
                                  else triggeranz:=puff;
puff:=readint('Start at trigger event no.                                 ',1);
if puff<=0 then begin fehler('Trigger event out of range.'); warte; exit end;
triggeranf:=puff;
puff:=readint('Trigger point selection, each 1., 2., 3., ... trigger event',1);
if puff<=0 then begin fehler('Number out of range.'); warte; exit end;
triggerabz:=puff;
puff:=readint('Skip following trigger events up to [ms]                   ',0);
if puff<0 then begin fehler('Number out of range.'); warte; exit end;
triggerdst:=messw(puff);
end;

procedure ausfuehren;
var   welche:matrix;
      ta:char;
procedure loeschen;
begin
window(1,zeilmax-3,80,zeilmax); clrscr; window(1,3,80,zeilmax);
end;
begin
repeat
   ueberschrift(filenr>10,'Triggering','Info',farbe3);
   writeln(lfcr,'':27,'Content of trigger lists:',lfcr);
   mat.uebernehmen;
   mat.ausgabe;
   gotoxy(1,zeilmax-8); zwischen('Dialogue',farbe3);
   loeschen; gotoxy(1,zeilmax-4);
   gotoxy(1,zeilmax-6);
   clreol; welche.eingabe; if welche.escape then exit;
   loeschen; gotoxy(1,zeilmax-4);
   if welche.unsinn then begin fehler('Incorrect table position.'); warte end
                    else begin
      new(aut);
      abbruch:=false; writeln;
      for ta:='A' to listmax do begin
         mat.tn:=ta;
         tliste[ta]^.triggern(welche.tl[ta]);
         if abbruch then begin dispose(aut); exit end;
         end;
      dispose(aut);
      end;
until false;
end;

procedure triggerfile;
const filename:string80='trigger.txt';
      ta:char='A';
var   ausgabe:text;
      fn,i,j:exword;
      ykanaele:kanalmenge;
      zkn:integer;
begin
ueberschrift(false,'Export Trigger Data','Info',farbe3);
triggeruebersicht;
gotoxy(1,18); zwischen('Dialogue',farbe3);
window(1,22,80,zeilmax);
ta:=upcase(readchar('Trigger list','A'));
if not (ta in['A'..listmax]) then begin
   fehler('Undefined trigger list'); warte; exit end;
filename:=readstring('File name and path',filename);
if fileschonda(filename) then
   if upcase(readchar('Overwrite? (Y/N)','N'))<>'Y' then exit;
window(1,3,80,zeilmax); clrscr;
if tliste[ta]^.tr=maxkanal then ykanaele.kn:=0
                           else begin
                           ykanaele.kn:=1;
                           ykanaele.k[1]:=tliste[ta]^.tr;
                           end;
ykanaele.lesen(8,farbe3);
assign(ausgabe,filename);
rewrite(ausgabe);
with tliste[ta]^ do begin
   writeln(ausgabe,' " Trigger list: '+name+' "');
   write(ausgabe,' " Data file':12,'Time [ms]':20);
   for j:=0 to maxkanal-1 do if (j in ykanaele.dabei) then
      write(ausgabe,schriftliste[j]+' ['+belegungsliste[j].einhwort+']':20);
   writeln(ausgabe,' "');
   for fn:=1 to filenr do with fil[fn] do begin
      oeffnen(fn);
      if automda then for i:=1 to automn do begin
         write(ausgabe,fn:12,extzeit(autom^[i]):20:3);
         for j:=0 to maxkanal-1 do if (j in ykanaele.dabei) then
            write(ausgabe,extspannung(dat(zwi(autom^[i]),j),j):20:6);
         writeln(ausgabe) end;
      schliesse;
      end;
   end;
close(ausgabe);
end;

begin
repeat
   ueberschrift(false,'Trigger Manager','Info',farbe2);
   writeln('Channel   : ',akttrkan,' (',schriftliste[akttrkan],')');
   writeln('Conditions: ',bloecketext[bloecke],',','max. ',triggeranz,
      ',start at ',triggeranf,'.,each ',triggerabz,'. event,skipping ',
      zeit(triggerdst),'ms');
   writeln;
   triggeruebersicht;
   writeln;
   zwischen('Menu',farbe2);
   writeln(lfcr,'  h...Channel     c...Conditions    b...Create Blocks   e...Export Data',
           lfcr,'  o...Mode        t...Triggering                        m...Main Menu');
   writeln;
   zwischen('Dialogue',farbe2);
   writeln;
   trind:=readcharim('Menu Point','m');
   writeln;
   case upcase(trind) of
      'T':ausfuehren;      'O':triggneu;
      'C':konditionen;     'H':aktuellkanal;
      'E':triggerfile;     'B':begin autoblock(0); warte end;
      'M':exit;
      end;
until false;
end;

begin

registertype(rkeine);    registertype(rpunkte);
registertype(rgelesen);
registertype(rhoch);     registertype(rrunter);
registertype(rminimum);  registertype(rmaximum);
registertype(rfenstermaximum);
registertype(rfensterminimum);
registertype(reintritt); registertype(raustritt);

registertype(raequidistant);

for trind:='A' to listmax do tliste[trind]:=new(keinezg,neu);

registertype(rfreqfilter);   registertype(rpolygonfilter);
registertype(rdiffilteralt); registertype(rphasenfilter);
registertype(rpunktefilter); registertype(rzaehltfilter);
registertype(rintervallfilter);registertype(rdiffilter);
registertype(rasciifilter);  registertype(rtlintfilter);
end.
