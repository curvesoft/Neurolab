{Turbo-Pascal 6.0}

unit nltrigg;

{$A+,B-,F-,G+,I-,N+,O-,V-,X-}

interface

uses  crt, dos,           tulab,
      objects,            tlfilter,
      bequem,             tlfiles,
      nlrahmen;

const triggermax=5000;
      listmax='D';

type  triggerliste=array[0..triggermax+1] of messwert;
      triggerdaten=object
         autom:^triggerliste;
         automn:longint;
         automda:boolean;
         procedure nimm (var aut:triggerliste; gesamt:longint);
         procedure store (var s:tbufstream);
         procedure load (var s:tbufstream);
         procedure frei;
         procedure such (links,rechts:word; nach:messwert; var li,re:word);
         end;

      filemenge=set of 1..maxfiles;

      triggerungzg=^triggerung;
      triggerung=object (tobject)
         name:string[50];
         fil:array [1..maxfiles] of triggerdaten;
         tr:byte;
         function fileanz:byte;
         function erstfile:byte;
         function triggsum:word;
         procedure neu;
         procedure load (var s:tbufstream);
         procedure store (var s:tbufstream);
         destructor alt; virtual;
         procedure triggern (dabei:filemenge); virtual;
       private
         procedure blocktriggern (von,bis:messwert); virtual;
         end;

      keinezg=^keine;
      keine=object (triggerung)
         constructor neu;
         constructor load (var s:tbufstream);
         procedure triggern (dabei:filemenge); virtual;
         end;

      punktezg=^punkte;
      punkte=object (triggerung)
         constructor neu;
         constructor load (var s:tbufstream);
         procedure triggern (dabei:filemenge); virtual;
         end;
      schwelle=object (triggerung)
         schw:wert;
         procedure neu;
         procedure load (var s:tbufstream);
         procedure store (var s:tbufstream);
         end;
      hochzg=^hoch;
      hoch=object (schwelle)
         constructor neu;
         constructor load (var s:tbufstream);
       private
         procedure blocktriggern (von,bis:messwert); virtual;
         end;
      runterzg=^runter;
      runter=object (schwelle)
         constructor neu;
         constructor load (var s:tbufstream);
       private
         procedure blocktriggern (von,bis:messwert); virtual;
         end;
      extremum=object (triggerung)
         bmittel,nmittel:extended;
         procedure neu;
         procedure load (var s:tbufstream);
         procedure store (var s:tbufstream);
         end;
      minimumzg=^minimum;
      minimum=object (extremum)
         constructor neu;
         constructor load (var s:tbufstream);
       private
         procedure blocktriggern (von,bis:messwert); virtual;
         end;
      maximumzg=^maximum;
      maximum=object (extremum)
         constructor neu;
         constructor load (var s:tbufstream);
       private
         procedure blocktriggern (von,bis:messwert); virtual;
         end;

      triggerungsliste=array ['A'..listmax] of triggerungzg;

      weiser=array[1..triggermax] of word;
      triggerweiser=object
         weisliste:array[1..maxfiles] of record t:^weiser; n:word end;
         gesamt:longint;
         mittelabstand:messwert;
         procedure zaehlen (var feld:triggerung; minabst,maxabst:messwert);
         procedure frei;
         end;

const triggeranz:word=triggermax;
      triggeranf:word=1;

var   tliste:triggerungsliste;

procedure manager;

procedure triggeruebersicht;
procedure kontrolle (nr:byte; trind:char);

procedure streamput (var s:tbufstream);
procedure streamget (var s:tbufstream);

implementation

type  uebersicht=array['A'..listmax] of filemenge;
      zahlenuebersicht=array['A'..listmax,1..maxfiles] of word;
      matrix=object
         tl:uebersicht;
         unsinn:boolean;     escape:boolean;
         procedure eingabe;
         end;
      zahlenmatrix=object (matrix)
         tz:zahlenuebersicht;
         tn:char;            fn:byte;
         procedure uebernehmen;
         procedure ausgabe;
         procedure erstsprung;                procedure sprung;
       private
         zn:byte;            wx,wy:byte;
         end;

      trist=object
         gesamt:word;
         procedure beginn(trfile:byte);       procedure weiter(stelle:messwert);
         function aufhoeren:boolean;
       private
         zaehler:word;       zn:byte;
         end;
      gleitschw=object
         schw:wert;
         nkorr,n2korr:longint;
         procedure beginn (nmittel,bmittel:extended);
         procedure mitteln (stekorr:longint; tr:byte);
         procedure zaehlt (var stekorr:longint; tr:byte);
         end;

const bloecke:boolean=false;
      bloeckeb:array[boolean] of char=('f','b');
      bloecketext:array[boolean] of string20=('File','Block');

      akttrkan:byte=0;

      rkeine:tstreamrec=    (objtype:100;             vmtlink:ofs(typeof(keine)^);
                             load:@keine.load;        store:@triggerung.store);
      rpunkte:tstreamrec=   (objtype:101;             vmtlink:ofs(typeof(punkte)^);
                             load:@punkte.load;       store:@triggerung.store);
      rhoch:tstreamrec=     (objtype:102;             vmtlink:ofs(typeof(hoch)^);
                             load:@hoch.load;         store:@schwelle.store);
      rrunter:tstreamrec=   (objtype:103;             vmtlink:ofs(typeof(runter)^);
                             load:@runter.load;       store:@schwelle.store);
      rminimum:tstreamrec=  (objtype:104;             vmtlink:ofs(typeof(minimum)^);
                             load:@minimum.load;      store:@extremum.store);
      rmaximum:tstreamrec=  (objtype:105;             vmtlink:ofs(typeof(maximum)^);
                             load:@maximum.load;      store:@extremum.store);

var   mat:zahlenmatrix;
      verfol:trist;

      aut:^triggerliste;
      trind:char;
      abbruch:boolean;

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

procedure triggerdaten.such (links,rechts:word; nach:messwert; var li,re:word);
var   neu:word;
begin
li:=links; re:=rechts;
while (autom^[li+1]<=nach) or (autom^[re-1]>=nach) do begin
   neu:=li+(re-li) div 2;
   if autom^[neu]<=nach then li:=neu;
   if autom^[neu]>=nach then re:=neu;
   end;
end;

procedure matrix.eingabe;
var  bef:string[3];
     ti:char;
begin
for ti:='A' to listmax do tl[ti]:=[1..filenr];
bef:=readstring('AusfÅhrung fÅr Zeile, Spalte oder Position z.B. 1, B, A2','');
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
procedure zeile;
var   ti:char;
begin
write(fi:10,' : ');
for ti:='A' to listmax do if fi in tl[ti] then write(tz[ti,fi]:5)
                                          else write('-':5);
writeln;
end;
procedure kopf;
var   ti:char;
begin
write('File-Nr':11,'  '); for ti:='A' to listmax do write(ti:5); writeln;
end;

begin
zn:=wherey;
kopf; for fi:=1 to min(filenr,10) do zeile;
gotoxy(41,zn); kopf;
for fi:=11 to filenr do begin gotoxy(41,wherey); zeile end;
end;

procedure zahlenmatrix.erstsprung;
begin
wx:=14+(ord(tn)-ord('A'))*5+40*((fn-1) div 10);
wy:=zn+1+(fn-1) mod 10;
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

procedure trist.beginn(trfile:byte);
var   di:dirstr; na:namestr; ext:extstr;
begin
gesamt:=0; zaehler:=1;
zn:=wherey;
mat.erstsprung; write(0:5);
fsplit(liste[trfile].name,di,na,ext);
gotoxy(1,zn); clreol;
write('Triggerstelle in ',na+ext:11,' bei Zeit           ms: Abbruch mit <Esc>');
abbruch:=keypressed and (readkey=#27);
end;

procedure trist.weiter (stelle:messwert);
begin
if zaehler<triggeranf then inc(zaehler)
                      else begin
   inc(gesamt); aut^[gesamt]:=stelle;
   mat.sprung; write(gesamt:5);
   end;
gotoxy(40,zn); write(zeit(stelle):8);
if keypressed and (readkey=#27) then abbruch:=true;
end;

function trist.aufhoeren:boolean;
begin
aufhoeren:=(gesamt=triggeranz) or abbruch;
end;

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

function triggerung.triggsum:word;
var   i:byte; summe:word;
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

procedure triggerung.load (var s:tbufstream);
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
end;

procedure triggerung.blocktriggern (von,bis:messwert);
begin end;

destructor triggerung.alt;
var   i:byte;
begin
for i:=1 to filenr do fil[i].frei;
end;

constructor keine.neu;
begin
triggerung.neu;
name:='undefiniert'; tr:=maxkanal;
end;

constructor keine.load (var s:tbufstream);
begin
triggerung.load(s);
end;

procedure keine.triggern (dabei:filemenge);
begin end;

constructor punkte.neu;
begin
triggerung.neu;
name:='handgesetzte Punkte';
end;

constructor punkte.load (var s:tbufstream);
begin
triggerung.load(s);
end;

procedure punkte.triggern (dabei:filemenge);
var   trfile:byte;
      pwandert:punktzeiger;
begin
for trfile:=1 to filenr do if trfile in dabei then
 with fil[trfile], liste[trfile] do begin
   mat.fn:=trfile;
   verfol.beginn(trfile); if abbruch then exit;
   oeffnen(trfile);
   pwandert:=selbst^;
   while (pwandert^.next<>nil) and not verfol.aufhoeren do begin
      verfol.weiter(pwandert^.bei);
      pwandert:=pwandert^.next end;
   nimm(aut^,verfol.gesamt);
   schliesse;
   if abbruch then exit;
   end;
end;

procedure schwelle.neu;
var   einstr:string20;
begin
triggerung.neu;
einstr:=belegungsliste[tr].einhwort;
schw:=norm(readext('Schwelle ['+einstr+']',0,1,1),tr);
name:=extwort(extspannung(schw,tr),2,1)+' '+einstr+' - ';
end;

procedure schwelle.load (var s:tbufstream);
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
name:=name+'AufwÑrtsschwelle';
end;

constructor hoch.load (var s:tbufstream);
begin
schwelle.load(s);
end;

procedure hoch.blocktriggern (von,bis:messwert);
var   stekorr,vonkorr,biskorr:longint;
begin
vonkorr:=trunc(von*korr)+1; biskorr:=trunc(bis*korr);
stekorr:=vonkorr;
repeat
   while (dat(stekorr,tr)>=schw) and (stekorr<=biskorr) do inc(stekorr);
   while (dat(stekorr,tr)<schw) and (stekorr<=biskorr) do inc(stekorr);
   if stekorr>biskorr then exit;
   verfol.weiter(stekorr/korr);
until verfol.aufhoeren;
end;

constructor runter.neu;
begin
schwelle.neu;
name:=name+'AbwÑrtsschwelle';
end;

constructor runter.load (var s:tbufstream);
begin
schwelle.load(s);
end;

procedure runter.blocktriggern (von,bis:messwert);
var   stekorr,vonkorr,biskorr:longint;
begin
vonkorr:=trunc(von*korr)+1; biskorr:=trunc(bis*korr);
stekorr:=vonkorr;
repeat
   while (dat(stekorr,tr)<=schw) and (stekorr<=biskorr) do inc(stekorr);
   while (dat(stekorr,tr)>schw) and (stekorr<=biskorr) do inc(stekorr);
   if stekorr>biskorr then exit;
   verfol.weiter(stekorr/korr);
until verfol.aufhoeren;
end;

procedure extremum.neu;
begin
triggerung.neu;
nmittel:=messw(readint('Breite des gleitenden Mittels [ms]',100));
bmittel:=messw(readint('Breite, die ein Maximum/Minimum mindestens haben mu· [ms]',10));
name:='';
end;

procedure extremum.load (var s:tbufstream);
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

procedure gleitschw.mitteln (stekorr:longint; tr:byte);
var i:longint;
begin
schw:=0;
for i:=stekorr-n2korr to stekorr+n2korr do schw:=schw+dat(i,tr);
schw:=schw/nkorr;
end;

procedure gleitschw.zaehlt (var stekorr:longint; tr:byte);
begin
inc(stekorr);
schw:=schw-(dat(stekorr-n2korr-1,tr)-dat(stekorr+n2korr,tr))/nkorr;
end;

constructor minimum.neu;
begin
extremum.neu;
name:='Minimum';
end;

constructor minimum.load (var s:tbufstream);
begin
extremum.load(s);
end;

procedure minimum.blocktriggern (von,bis:messwert);
var   l,r:longint;
      hilf,extr:integer;
      stekorr,vonkorr,biskorr,bkorr:longint;
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
      l:=r; extr:=4096; hilf:=dat(l,tr);
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
name:='Maximum';
end;

constructor maximum.load (var s:tbufstream);
begin
extremum.load(s);
end;

procedure maximum.blocktriggern (von,bis:messwert);
var   l,r:longint;
      hilf,extr:integer;
      stekorr,vonkorr,biskorr,bkorr:longint;
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
      l:=r; extr:=-1; hilf:=dat(l,tr);
      while hilf>glesch.schw do begin
         if extr<hilf then begin extr:=hilf; stekorr:=r end;
         glesch.zaehlt(r,tr); if r>biskorr then exit;
         hilf:=dat(r,tr) end;
      until r-l>=bkorr;
      verfol.weiter(stekorr/korr);
until verfol.aufhoeren;
end;

procedure triggerweiser.zaehlen (var feld:triggerung; minabst,maxabst:messwert);
var   nr,tp,n:longint;
      abst:messwert;
      sum:extended;
begin
gesamt:=0; sum:=0;
for nr:=1 to filenr do with weisliste[nr], feld, fil[nr] do begin
   new(weisliste[nr].t);
   if automda then begin
      n:=0;
      for tp:=1 to automn-1 do begin
         abst:=autom^[tp+1]-autom^[tp];
         if (abst<=maxabst) and (abst>=minabst) then begin
            inc(n); weisliste[nr].t^[n]:=tp; sum:=sum+abst;  end;
         end;
      weisliste[nr].n:=n;
      inc(gesamt,n);
      end;
   end;
if gesamt<>0 then mittelabstand:=sum/gesamt;
end;

procedure triggerweiser.frei;
var   nr:word;
begin
for nr:=1 to filenr do dispose(weisliste[nr].t);
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
for ind:='A' to listmax do begin
   dispose(tliste[ind],alt); tliste[ind]:=triggerungzg(s.get); end;
end;

procedure triggeruebersicht;
var   trind:char;
begin
writeln('Liste':5,'Kanal':8,'Anz.':6,'Files':6,'  Modus und Beschreibung');
for trind:='A' to listmax do with tliste[trind]^ do begin
   writeln(trind:2,schriftliste[tr]:11,triggsum:5,fileanz:5,'    ',tliste[trind]^.name);
   end;
end;

procedure manager;
var   trind:char;

procedure aktuellkanal;
var   liste:filterliste;
      i:byte;
begin
ueberschrift('Triggerkanal','Info',farbe3);
belegungzeigen; writeln;
liste.zeigen(8,kan); writeln;
zwischen('Dialog',farbe3);
write(lfcr,'Filterkanalliste weiterzeigen? (J/N) ');
while not liste.ende and (readkey in ['Y','y','J','j']) do liste.weiterzeigen;
write(#13); clreol;
i:=readint('Triggerkanal',0);
if not (i in [0..kan+filtermax-1]) then begin
   fehler('UnzulÑssige Kanal-Nr.'); warte end
                                   else akttrkan:=i;
end;

procedure triggneu;
const ta:char='A';
var   artbu:char;
begin
ueberschrift('Triggermodus','Info',farbe3);
writeln('Triggerkanal: ',akttrkan,' (',schriftliste[akttrkan],')',lfcr);
triggeruebersicht; writeln;
writeln('Triggermodi:',lfcr,'  u = AufwÑrtsschwelle',lfcr,
        '  b = AbwÑrtsschwelle',lfcr,'  x = Maximum',lfcr,'  n = Minimum',lfcr,
        '  p = handgesetzte Punkte');
writeln;
zwischen('Dialog',farbe3);
window(1,20,80,25);
ta:=upcase(readchar('Triggerliste','A'));
if not (ta in['A'..listmax]) then begin
   fehler('UnzulÑssige Triggerliste'); warte; exit end;
artbu:=readchar('Triggermodus','u');
if not (artbu in ['k','u','b','x','n','p']) then begin
   fehler('UnzulÑssiger Triggermodus'); warte; exit end;
dispose(tliste[ta],alt);
case artbu of
   'k':tliste[ta]:=new(keinezg,neu);
   'u':tliste[ta]:=new(hochzg,neu);
   'b':tliste[ta]:=new(runterzg,neu);
   'x':tliste[ta]:=new(maximumzg,neu);
   'n':tliste[ta]:=new(minimumzg,neu);
   'p':tliste[ta]:=new(punktezg,neu);
   end;
with tliste[ta]^ do name:=readstring('Beschreibung',name);
end;

procedure konditionen;
var   puff:longint;
begin
ueberschrift('Triggerkonditionen','Info',farbe3);
writeln('Triggerbereich (ganze Files/Blîcke in Files): ',bloecketext[bloecke],
   lfcr,'Auswertung ab Triggerstelle Nr. (je File)   : ',triggeranf,
   lfcr,'maximale Triggeranzahl je File              : ',triggeranz);
writeln;
zwischen('Dialog',farbe3); writeln;
bloecke:=upcase(readchar('Triggerbereich (f=ganze Files, b=Blîcke) ',
   bloeckeb[bloecke]))=upcase(bloeckeb[true]);
puff:=readint('Auswertung ab Triggerstelle Nr. (je File)',1);
if puff<=0 then begin fehler('UnzulÑssige Triggerstelle'); warte; exit end;
triggeranf:=puff;
puff:=readint('maximale Triggeranzahl je File [max.'+wort(triggermax)+']',triggeranz);
if (puff<=0) or (puff>triggermax) then begin
   fehler('UnzulÑssige Triggeranzahl'); warte end
                                  else triggeranz:=puff;
end;

procedure ausfuehren;
var   welche:matrix;
      ta:char;
procedure loeschen;
begin
window(1,20,80,25); clrscr; window(1,3,80,25);
end;
begin
ueberschrift('Triggerung','Info',farbe3);
writeln('':27,'Stand der Triggerlisten:',lfcr);
mat.uebernehmen; mat.ausgabe;
repeat
   gotoxy(1,15); zwischen('Dialog',farbe3);
   loeschen; gotoxy(1,21);
   writeln('Der Inhalt der Triggerlisten kann unter "Sichten" mit der Tasten-',
           lfcr,'kombination <Ctrl><F9>, <A>-<D> abgerufen werden.');
   gotoxy(1,17);
   clreol; welche.eingabe; if welche.escape then exit;
   loeschen; gotoxy(1,19);
   if welche.unsinn then begin fehler('Fehlerhafte Feldeingabe'); warte end
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

begin
repeat
   ueberschrift('Trigger-Manager','Info',farbe2);
   writeln('Triggerkanal      : ',akttrkan,' (',schriftliste[akttrkan],')');
   writeln('Triggerkonditionen: ',bloecketext[bloecke],', ','max. ',triggeranz,
      ' Triggerstellen je File, ab der ',triggeranf,'. Stelle');
   writeln;
   triggeruebersicht;
   writeln;
   zwischen('MenÅ',farbe2);
   writeln(lfcr,'    k...Triggerkanal                    t...Triggerung ausfÅhren',
           lfcr,'    m...Triggermodus',
           lfcr,'    o...Triggerkonditionen              h...HauptmenÅ');
   writeln;
   zwischen('Dialog',farbe2);
   writeln;
   trind:=readchar('Wahlpunkt','h');
   writeln;
   case upcase(trind) of
      'T':ausfuehren;
      'M':triggneu;
      'O':konditionen;
      'K':aktuellkanal;
      'H':exit;
      end;
until false;
end;

begin
registertype(rkeine);   registertype(rpunkte);
registertype(rhoch);    registertype(rrunter);
registertype(rminimum); registertype(rmaximum);
for trind:='A' to listmax do tliste[trind]:=new(keinezg,neu);
end.