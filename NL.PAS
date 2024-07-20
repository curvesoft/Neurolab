{ Borland-Pascal 7.0 }

program neurolab;
{Authors: Berthold Hedwig & Marko Knepper}

{$IFDEF MSDOS}
{$A+,B-,E+,F-,G-,I-,N+,O-,P+,T+,V+,X-} {$M 65520,130000,655360}
{$ELSE}
{$A+,B-,E+,F-,G+,I-,N+,P+,T+,V+,X-} {$M 65520,0}
{$ENDIF}

uses  crt,                daff,tulab42,       nlrahmen,           grafik,
      dos,                tlfilter,           nltrigg,            nlgrafik,
      bequem,             tlfiles,            nlfiles,
      objects,                                nlsicht,
                                              nlausw;

const version='7.3';  paramver='7.2';
      {$IFDEF DPMI}   plattform='DPMI '; {$ENDIF}
      {$IFDEF MSDOS}  plattform='MSDOS'; {$ENDIF}
      {$IFDEF WINDOW} plattform='Wind.'; {$ENDIF}
      id:string[8]='Neurl'+paramver;
      sichername:namestr='NEUROLAB';
      pufgroesse=32768;

var   exitsave:pointer;

procedure verstaerkungen;
var   i,j:longint;
      mult:extended; textstr:string20;
procedure tabelle (von:byte);
var   i:byte;
begin
if von<=kan-1 then
   writeln('Channel':5,'Label':11,'Factor':9,'Unit':11);
for i:=von to min(von+7,kan-1) do with grund[i] do
   writeln(i:3,schriftliste[i]:13,extewort(multi,2,2):11,einhwort:8);
end;
begin
ueberschrift(false,'Calibration','Info',farbe2);
i:=0;
repeat
   window(1,3,38,12); clrscr; tabelle(0);
   window(43,3,80,12); clrscr; tabelle(8);
   window(1,13,80,19); zwischen('Menu',farbe2);
   writeln(lfcr,'  c...Calibration',lfcr,'  m...Main Menu',lfcr);
   window(1,18,80,25); clrscr;
   zwischen('Dialogue',farbe2); writeln;
   case upcase(readcharim('Menu Point','m')) of
      'C':begin
        clrscr; zwischen('Dialogue',farbe3);
        window(1,20,80,25);
        j:=readint('Channel No.',i);
        if not(j in [0..kan-1]) then begin
           writeln; fehler('Undefined channel number'); warte end
                                else begin
           i:=j;
           textstr:=readstring('Basic SI Unit',grund[i].einhwort);
           writeln('The factor describes how many ',textstr,
             ' are equivalent to 1V at the A-D-Converter.');
           mult:=readexte('Factor ['+textstr+']',grund[i].multi,2,2);
           grund[i].setz(mult,textstr);
           end;
        end;
      'M':begin einheitensetzen(fre); exit end;
      end;
until false;
end;

procedure filterung;
var    indexalt:byte;
       wahl:char;
       wx,wy:byte;
       liste:filterliste;

procedure filteruebersicht;
begin
writeln('Filters:');
writeln(lfcr,'    o : ñ Offset          a : Amplification     c : Clip',
        lfcr,'    i : Invert Sign       v : Absolute Value    q : Square',
        lfcr,'    g : Gliding Average   l : Low-Pass          h : High-Pass',
        lfcr,'    d : Differentiate     s : Spike Filter      m : Max - Min',
        lfcr,'    y : y-Resolution      f : Frequency (TL)    t : ñ Time Shift',
        lfcr,'    r : Reciprocal Value  p : Polygon (TL)      u : Summation',
        lfcr,'    x : Time Diff. (TL)   w : Integration       n : Gl. Integration',
        lfcr,'    z : Gliding Length    b : Phase (TL)        e : arcsin',
        lfcr,'    k : arccos');
end;

procedure filtersetzen;
var    ableitungein:einheittyp;
       ke,k,i:byte;
       fi:string80;
begin
ueberschrift(false,'Filters', 'Info',farbe3);
belegungzeigen;
gotoxy(1,15); zwischen('Dialogue',farbe3); writeln;
ke:=readint('Input Channel No. (0-'+wort(pred(kan))+')',0);
if not (ke in [0..kan+filtermax-1]) then begin
   fehler('Undefined channel no.'); warte; exit end;
clrscr; writeln('Input Channel: ',ke,' (',schriftliste[ke],')',lfcr);
liste.zeigen(11,kan);
gotoxy(1,15); zwischen('Dialogue',farbe3);
write(lfcr,'Continue filter list? (Y/N) ');
while not liste.ende and (readkey in ['Y','y','J','j']) do liste.weiterzeigen;
write(#13); clreol;
k:=kan;
while filterdrin(k) and (k<maxkanal) do inc(k);
k:=readint('Output Channel No. ('+wort(kan)+'-'+wort(kan+filtermax-1)+')',k);
if not (k in [kan..kan+filtermax-1]) then begin
   fehler('Output channel no. expected'); warte; exit end;
clrscr;
writeln('Input Channel: ',ke,' (',schriftliste[ke],')',lfcr,
        'Output Channel: ',k);
write('Previous: '); if filterdrin(k) then write(filterzeile(k));
gotoxy(1,5); filteruebersicht;
gotoxy(1,16); zwischen('Dialogue',farbe3);
window(1,20,80,25);
kanalsetz(k,ke);
fi:=readstring('New Filters','');
for i:=1 to length(fi) do
   case fi[i] of
      'o':begin
         einheitensetzen(fre);
         filtersetz(new(offsetzg,neu(k,
            round(readext('Offset: Value ['+belegungsliste[k].einhwort
                          +']',0,3,1)/belegungsliste[k].faktor))),k);
         end;
      'a':filtersetz(new(malfaktorzg,neu(
             readext('Amplification: Factor',1,4,2))),k);
      'b':filtersetz(new(phasenfilterzg,neu(
             upcase(readchar('Phase filter: Reference List','A')),
             upcase(readchar('              Event List','B')))),k);
      'y':filtersetz(new(streckungzg,neu(
             readext('y-Resolution: Factor',1,4,2))),k);
      'c':filtersetz(new(kappenzg,neu),k);
      'u':filtersetz(new(additionzg,neu(
             readint('Summation: Channel No',0))),k);
      'v':filtersetz(new(absolutzg,neu),k);
      'q':filtersetz(new(squarezg,neu),k);
      'l':filtersetz(new(tiefpasszg,neu(
             readint('Low-Pass: Frequency (min. '
                     +wort(round(genau*pi/weite*fre))+' Hz)',round(fre/2)))),k);
      'h':filtersetz(new(hochpasszg,neu(
             readint('High-Pass: Frequency (min. '
                     +wort(round(genau*pi/weite*fre))+' Hz)',round(fre/2)))),k);
      'd':filtersetz(new(diffzg,neu),k);
      'w':filtersetz(new(intzg,neu),k);
      't':filtersetz(new(verschiebezg,neu(
            readext('Time Shift: Value [ms]',0,3,1))),k);
      'm':filtersetz(new(maxminzg,neu(
            readext('Max - Min: Width'#29' [ms]',1,3,1))),k);
      's':begin
        einheitensetzen(fre);
        ableitungein:=belegungsliste[k]; dec(ableitungein.sekunde);
        filtersetz(new(spikefilterzg,neu(k,
           readext('Spike Filter: Max. Width '#29' [ms]',3,3,1),
           readext('              Rising Gradient  '#24' ['+ableitungein.einhwort+']',0,3,1),
           readext('              Falling Gradient '#25' ['+ableitungein.einhwort+']',0,3,1))),k);
        end;
      'i':filtersetz(new(invertzg,neu),k);
      'g':filtersetz(new(glattzg,neu(
            readext('Gliding Average: Width [ms]',1,3,1))),k);
      'f':filtersetz(new(freqfilterzg,neu(
            upcase(readchar('Frequency: for Trigger List (A-'+listmax+')','A')))),k);
      'r':filtersetz(new(einsdurchzg,neu),k);
      'p':filtersetz(new(polygonfilterzg,neu(
            upcase(readchar('Polygon: for Trigger List (A-'+listmax+')','A')))),k);
      'x':filtersetz(new(diffilterzg,neu(
            upcase(readchar('Time Difference: Reference List','A')),
            upcase(readchar('                 Event List','B')),
            readint('                 Time Window [ms]',100))),k);
      'n':filtersetz(new(glintzg,neu(
            readext('Gliding Integration: Width [ms]',1,3,1))),k);
      'z':filtersetz(new(gllinzg,neu(
            readext('Gliding Length: Width [ms]',1,3,1))),k);
      'e':filtersetz(new(arcsinzg,neu),k);
      'k':filtersetz(new(arccoszg,neu),k);
      else fehler('Filter "'+fi[i]+'" not defined.');
      end;
indexalt:=k;
end;

begin
indexalt:=kan;
repeat
   ueberschrift(false,'Filter Manager','Info',farbe2);
   liste.zeigen(11,indexalt); writeln;
   zwischen('Menu',farbe2); writeln;
   writeln('f...Info Filter                          d...Declare Filter',lfcr,
           'o...Info Output Channels (continue)      m...Main Menu',lfcr,
           'i...Info Input Channels');
   writeln;
   zwischen('Dialogue',farbe2);
   writeln; write('Menu Point: m'#8);
   repeat
      wahl:=readkey;
      wx:=wherex; wy:=wherey;
      window(1,3,80,15); clrscr;
      case wahl of
         'o':begin indexalt:=liste.index; liste.zeigen(11,liste.index) end;
         'i':begin belegungzeigen; liste.index:=indexalt end;
         'f':begin filteruebersicht; liste.index:=indexalt end;
         'd':filtersetzen;
         'm',#13,#27:begin einheitensetzen(fre); exit end;
         end;
      window(1,3,80,25); gotoxy(wx,wy);
   until wahl='d';
until false;
end;

procedure listen;
const aktfile:byte=1;
      k:byte=0;
var   i,j:longint;
begin
ueberschrift(false,'Data List','Info',farbe2);
fileliste;
gotoxy(1,19); zwischen('Dialogue',farbe2); writeln;
i:=readint('File No.',aktfile);
if not (i in [1..filenr]) then begin
   fehler('Undefined File No.'); warte; exit end;
aktfile:=i;
clrscr; belegungzeigen; writeln;
gotoxy(1,19); zwischen('Dialogue',farbe2); writeln;
i:=readint('Channel No.',k);
if  not (i in [0..pred(kan)]) then begin
   fehler('Undefinded Channel No.'); warte; exit end;
k:=max(i,0);
oeffnen(aktfile);
i:=zwi(messw(readint('Start Time [ms]',0)));
clrscr;
for j:=i to liste[aktfile].ko.anzahl-1 do begin
   write(lese(j,k):8);
   if j mod 210 = 209 then begin
      writeln('Continue: <Return>, Abort: <Esc>');
      if readkey=#27 then begin schliesse; exit end;
      end;
   end;
schliesse;
warte;
end;

procedure analogdaten;

procedure averagen;
const von:messwert=0;       bis:messwert=500;
      akttrind:char='A';
var   laenge,platz:longint;
      i,gesamt:longint;
      chpuff:char;
      grafik:grafikaverage;
begin
ueberschrift(false,'Averaging','Info',farbe3);
triggeruebersicht;
gotoxy(1,18); zwischen('Dialogue',farbe3); writeln;
chpuff:=upcase(readchar('Trigger List',akttrind));
if not (chpuff in ['A'..listmax]) then begin
   fehler('Undefined Trigger List'); warte; exit end;
akttrind:=chpuff; gesamt:=tliste[akttrind]^.triggsum;
if gesamt=0 then begin writeln('No trigger point.'); warte; exit end;
clrscr; kanaele.lesen(5,farbe3); if not (kanaele.kn in [1..32]) then exit;
window(1,22,80,25); clrscr;
writeln('Maximum Averaging Time: ',zeit(maxanzahl)-1,' ms.');
von:=messw(readint('Start Time [ms]',zeit(von)));
bis:=messw(readint('End Time [ms]  ',zeit(bis)));
laenge:=round(bis-von); platz:=(laenge+1)*sizeof(wert);
if (laenge>maxanzahl) or (laenge<=0) then begin
   fehler('Undefined time window'); warte; exit end;
for i:=0 to maxkanal do if i in kanaele.dabei then begin
   if maxavail<platz then begin
      fehler('Memory not sufficient');
      writeln(lfcr,'Help: Use less channels or shorter averaging time.');
      for i:=i-1 downto 0 do if i in kanaele.dabei then
         freemem(grafik.mittel[i],platz);
      warte; exit end;
   getmem(grafik.mittel[i],platz);
   fillchar(grafik.mittel[i]^,platz,0) end;
window(1,3,80,25); clrscr;
with tliste[akttrind]^ do begin
   gotoxy(1,3);
   writeln('Trigger List   : ',akttrind,' - ',name,
      lfcr,'Number of Files: ',fileanz,
      lfcr,'Channel No.    : ',kanaele.ausgabe,
      lfcr,'Trigger Points : ',gesamt,
      lfcr,'Averaging      : from ',zeit(von),' ms to ',zeit(bis),' ms');
   end;
gotoxy(1,18); zwischen('Dialogue',farbe3);
gotoxy(1,23); write('Abort: <Esc>'); gotoxy(1,20);
grafik.aufbauen(kanaele,von,laenge,akttrind,gesamt);
for i:=0 to maxkanal do if i in kanaele.dabei
   then freemem(grafik.mittel[i],platz);
end;

procedure phasenaveragen;
const von=0;
      minabst:messwert=0;             maxabst:messwert=10000;
      akttrind:char='A';
var   weis:triggerweiser;
      i,laenge,platz:longint;
      bis,abst:messwert;
      chpuff:char;
      grafik:grafikphasenaverage;
begin
ueberschrift(false,'Phase Dependent Averaging','Info',farbe3);
triggeruebersicht;
gotoxy(1,18); zwischen('Dialogue',farbe3); writeln;
chpuff:=upcase(readchar('Trigger List',akttrind));
if not (chpuff in ['A'..listmax]) then begin
   fehler('Undefined Trigger List'); warte; exit end;
akttrind:=chpuff;
clrscr; kanaele.lesen(5,farbe3); if not (kanaele.kn in [1..32]) then exit;
window(1,22,80,25); clrscr;
writeln('Maximum Averaging Time: ',zeit(maxanzahl)-1,' ms.');
minabst:=messw(readint(
   'Min. Cycle Duration [ms]',zeit(minabst)));
maxabst:=messw(readint(
   'Max. Cycle Duration [ms]',zeit(maxabst)));
weis.zaehlen(tliste[akttrind]^,minabst,maxabst);
if weis.gesamt=0 then begin writeln('No trigger point'); piep; warte; exit end;
bis:=weis.mittelabstand;
laenge:=round(bis-von); platz:=(laenge+1)*sizeof(wert);
if laenge>maxanzahl then begin
   fehler('Averaging time too long.'); warte; exit end;
for i:=0 to maxkanal do if i in kanaele.dabei then begin
   if maxavail<platz then begin
      fehler('Memory not sufficient');
      writeln(lfcr,'Help: Use less channels or shorter averaging time.');
      for i:=i-1 downto 0 do if i in kanaele.dabei then
         freemem(grafik.mittel[i],platz);
      warte; exit end;
   getmem(grafik.mittel[i],platz);
   fillchar(grafik.mittel[i]^,platz,0) end;
window(1,3,80,25); clrscr;
with tliste[akttrind]^ do begin
   gotoxy(1,3);
   writeln('Trigger List   : ',akttrind,' - ',name,
      lfcr,'File Number    : ',fileanz,
      lfcr,'Channel No.    : ',kanaele.ausgabe,
      lfcr,'Trigger Points : ',weis.gesamt,
      lfcr,'Cycle Duration : min. ',zeit(minabst),' ms, max. ',zeit(maxabst),' ms',
      lfcr,'Mean Duration  : ',zeit(weis.mittelabstand),' ms');
   end;
gotoxy(1,18); zwischen('Dialogue',farbe3);
gotoxy(1,23); write('Abort: <Esc>'); gotoxy(1,20);
grafik.aufbauen(kanaele,von,laenge,akttrind,weis.gesamt,weis);
weis.frei;
for i:=0 to maxkanal do if i in kanaele.dabei then freemem(grafik.mittel[i],platz);
end;

procedure xydiagramm;
const   kx:longint=0;
        ky:longint=1;
var     puff:longint;
        liste:filterliste;
        grafik:grafikxy;
begin
ueberschrift(false,'X-Y-Diagram','Info',farbe3);
belegungzeigen; writeln;
liste.zeigen(7,kan); writeln;
zwischen('Dialog',farbe3);
write(lfcr,'Continue list? (Y/N) ');
while not liste.ende and (readkey in ['Y','y','J','j']) do liste.weiterzeigen;
write(#13); clreol;
puff:=readint('X-channel',kx);
if not (puff in [0..kan+filtermax-1]) then begin
   fehler('Undefined channel no.'); warte; exit end
                                   else kx:=puff;
puff:=readint('Y-channel',ky);
if not (puff in ([0..kan+filtermax-1])-[kx]) then begin
   fehler('Undefined channel no.'); warte; exit end
                                   else ky:=puff;
grafik.aufbauen(kx,ky);
end;

begin
repeat
   ueberschrift(false,'Analog Data','Menu',farbe2);
   gotoxy(1,7);
   writeln(     '          a...Averaging',
           lfcr,'          p...Phase-Dependent Averaging',
           lfcr,'          x...X-Y-Diagram',
      lfcr,lfcr,'          m...Main Menu',lfcr);
   gotoxy(1,19);
   zwischen('Dialogue',farbe2);
   writeln;
   case upcase(readcharim('Menu Point','m')) of
      'A':averagen;
      'P':phasenaveragen;
      'X':xydiagramm;
      'M':exit;
      end;
until false;
end;

procedure intervalldaten;

procedure intervallhist;
const ref:char='A';
      von:messwert=0; bis:messwert=500;
      diffn:word=100;
      ybereich:extended=1;
var   histogramm:intervallhistogramm;
      charpuff:char; wordpuff:word;
begin
ueberschrift(false,'Interval Histogram','Info',farbe3);
triggeruebersicht;
gotoxy(1,12); zwischen('Dialogue',farbe3);
window(1,16,80,25);
charpuff:=upcase(readchar('Trigger List    ',ref));
if not (charpuff in ['A'..listmax]) then begin
   fehler('Undefined Trigger List'); warte; exit end;
ref:=charpuff;
von:=messw(readint('Start Time [ms] ',zeit(von)));
bis:=messw(readint('End Time [ms]   ',zeit(bis)));
wordpuff:=readint('Bins (Max. '+wort(maxzeit)+')',diffn);
if (diffn<=0) or (diffn>maxzeit) then begin
   fehler('Undefined number of bins'); warte; exit end;
diffn:=wordpuff;
ybereich:= readext('y-Scale Factor  ',ybereich,4,2);
histogramm.aufbauen(ref,von,bis,diffn,ybereich);
end;

procedure autokorr;
const ref:char='A';
      von:messwert=0; bis:messwert=500;
      diffn:word=100;
      ybereich:extended=1;
var   histogramm:autokorrelogramm;
      charpuff:char; wordpuff:word;
begin
ueberschrift(false,'Auto Correlogram','Info',farbe3);
triggeruebersicht;
gotoxy(1,12); zwischen('Dialogue',farbe3);
window(1,16,80,25);
charpuff:=upcase(
          readchar('Trigger List    ',ref));
if not (charpuff in ['A'..listmax]) then begin
   fehler('Undefined Trigger List'); warte; exit end;
ref:=charpuff;
von:=messw(readint('Start Time [ms] ',zeit(von)));
bis:=messw(readint('End Time [ms]   ',zeit(bis)));
wordpuff:=readint('Bins (Max. '+wort(maxzeit)+')',diffn);
if (diffn<=0) or (diffn>maxzeit) then begin
   fehler('Undefined number of bins'); warte; exit end;
diffn:=wordpuff;
ybereich:= readext('y-Scale Factor  ',ybereich,4,2);
histogramm.aufbauen(ref,von,bis,diffn,ybereich);
end;

procedure kreuzkorr;
const ref:char='A'; obj:char='B';
      von:messwert=0; bis:messwert=500;
      diffn:word=100;
      ybereich:extended=1;
var   histogramm:kreuzkorrelogramm;
      charpuff:char; wordpuff:word;
begin
ueberschrift(false,'Cross Correlogram','Info',farbe3);
triggeruebersicht;
gotoxy(1,12); zwischen('Dialogue',farbe3);
window(1,16,80,25);
charpuff:=upcase(
          readchar('Reference TL    ',ref));
if not (charpuff in ['A'..listmax]) then begin
   fehler('Undefined Trigger List'); warte; exit end;
ref:=charpuff;
charpuff:=upcase(
          readchar('Event TL        ',obj));
if not (charpuff in ['A'..listmax]) then begin
   fehler('Undefined Trigger List'); warte; exit end;
obj:=charpuff;
von:=messw(readint('Start Time [ms] ',zeit(von)));
bis:=messw(readint('End Time [ms]   ',zeit(bis)));
wordpuff:= readint('Bins (Max. '+wort(maxzeit)+')',diffn);
if (diffn<=0) or (diffn>maxzeit) then begin
   fehler('Undefined number of bins'); warte; exit end;
diffn:=wordpuff;
ybereich:= readext('y-Scale Factor  ',ybereich,4,2);
histogramm.aufbauen(ref,obj,von,bis,diffn,ybereich);
end;

procedure psthist;
const ref:char='A'; obj:char='B';
      von:messwert=0; bis:messwert=500;
      art:char='O';
      diffn:word=100;
      ybereich:extended=1;
var   histogramm:psthistogramm;
      charpuff:char; wordpuff:word;
begin
ueberschrift(false,'PST-Histogram','Info',farbe3);
triggeruebersicht;
gotoxy(1,12); zwischen('Dialogue',farbe3);
window(1,16,80,25);
charpuff:=upcase(
          readchar('Reference TL    ',ref));
if not (charpuff in ['A'..listmax]) then begin
   fehler('Undefined Trigger List'); warte; exit end;
ref:=charpuff;
charpuff:=upcase(
          readchar('Event TL        ',obj));
if not (charpuff in ['A'..listmax]) then begin
   fehler('Undefined Trigger List'); warte; exit end;
obj:=charpuff;
von:=messw(readint('Start Time [ms] ',zeit(von)));
bis:=messw(readint('End Time [ms]   ',zeit(bis)));
wordpuff:= readint('Bins (Max. '+wort(maxzeit)+')',diffn);
if (diffn<=0) or (diffn>maxzeit) then begin
   fehler('Undefined number of bins'); warte; exit end;
diffn:=wordpuff;
ybereich:= readext('y-Scale Factor  ',ybereich,4,2);
histogramm.aufbauen(ref,obj,-1,1,von,bis,diffn,ybereich);
end;

procedure latenzhist;
const ref:char='A'; obj:char='B';
      von:messwert=0; bis:messwert=500;
      diffn:word=100;
      ybereich:extended=1;
var   histogramm:latenzhistogramm;
      charpuff:char; wordpuff:word;
begin
ueberschrift(false,'Latency Histogram','Info',farbe3);
triggeruebersicht;
gotoxy(1,12); zwischen('Dialogue',farbe3);
window(1,16,80,25);
charpuff:=upcase(
          readchar('Reference TL    ',ref));
if not (charpuff in ['A'..listmax]) then begin
   fehler('Undefined Trigger List'); warte; exit end;
ref:=charpuff;
charpuff:=upcase(
          readchar('Event TL        ',obj));
if not (charpuff in ['A'..listmax]) then begin
   fehler('Undefined Trigger List'); warte; exit end;
obj:=charpuff;
von:=messw(readint('Start Time [ms] ',zeit(von)));
bis:=messw(readint('End Time [ms]   ',zeit(bis)));
wordpuff:= readint('Bins (Max. '+wort(maxzeit)+')',diffn);
if (diffn<=0) or (diffn>maxzeit) then begin
   fehler('Undefined number of bins'); warte; exit end;
diffn:=wordpuff;
ybereich:= readext('y-Scale Factor  ',ybereich,4,2);
histogramm.aufbauen(ref,obj,von,bis,diffn,ybereich);
end;

procedure phasenhist;
const ref:char='A'; obj:char='B';
      minabst:messwert=0;             maxabst:messwert=10000;
      anfph:extended=0;               schph:extended=1;
      diffn:word=100;
      ybereich:extended=1;
var   histogramm:phasenhistogramm;
      charpuff:char; wordpuff:word;
begin
ueberschrift(false,'Phase Histogram','Info',farbe3);
triggeruebersicht;
gotoxy(1,12); zwischen('Dialogue',farbe3);
window(1,16,80,25);
charpuff:=upcase(
      readchar('Reference TL            ',ref));
if not (charpuff in ['A'..listmax]) then begin
   fehler('Undefined Trigger List'); warte; exit end;
ref:=charpuff;
minabst:=messw(
       readint('Min. Cycle Duration [ms]',zeit(minabst)));
maxabst:=messw(
       readint('Max. Cycle Duration [ms]',zeit(maxabst)));
charpuff:=upcase(
      readchar('Event TL                ',obj));
if not (charpuff in ['A'..listmax]) then begin
   fehler('Undefined Trigger List'); warte; exit end;
obj:=charpuff;
anfph:=readext('Start Phase (0..1)      ',anfph,3,2);
if (anfph<0) or (anfph>1) then begin
   fehler('Value not allowed.'); warte; exit end;
schph:=readext('End Phase (0..1)        ',schph,3,2);
if (schph<0) or (schph>1) then begin
   fehler('Value not allowed.'); warte; exit end;
wordpuff:=
       readint('Bins (Max. '+wort(maxzeit)+')        ',diffn);
if (diffn<=0) or (diffn>maxzeit) then begin
   fehler('Undefined number of bins'); warte; exit end;
diffn:=wordpuff;
ybereich:=
       readext('y-Scale Factor          ',ybereich,4,2);
histogramm.aufbauen(ref,obj,minabst,maxabst,anfph,schph,diffn,ybereich);
end;

begin
repeat
   ueberschrift(false,'Interval Data','Info',farbe2);
   triggeruebersicht;
   gotoxy(1,10);
   zwischen('Menu',farbe2);
   writeln;
   writeln(     '          i...Interval Histogram',
           lfcr,'          a...Auto Correlogram',
           lfcr,'          l...Latency Histogram',
           lfcr,'          c...Cross Correlogram',
           lfcr,'          s...PST-Histogram',
           lfcr,'          p...Phase Histogram',
      lfcr,lfcr,'          m...Main Menu',lfcr);
   gotoxy(1,21);
   zwischen('Dialogue',farbe2);
   writeln;
   case upcase(readcharim('Menu Point','m')) of
      'I':intervallhist;            'A':autokorr;
      'L':latenzhist;               'C':kreuzkorr;
      'S':psthist;                  'P':phasenhist;
      'M':exit;
      end;
until false;
end;

procedure schluss; far; forward;

procedure sichern;
var   speicher:tbufstream;
      vorher:pointer;
begin
if filenr>0 then begin
   vorher:=exitproc;
   speicher.init(sichername+'.nlb',stcreate,pufgroesse);
   speicher.write(id,sizeof(id));
   tlfiles.streamput(speicher);
   nltrigg.streamput(speicher);
   tlfilter.streamput(speicher);
   if speicher.status=stok then writeln('Configuration file saved!')
                           else writeln(speicher.status:10,speicher.errorinfo:10);
   speicher.done;
   exitproc:=vorher;
   end;
end;

procedure holen;
var   speicher:tbufstream;
      such:searchrec;
      idtest:string[8];
begin
findfirst(sichername+'.nlb',anyfile,such);
if doserror=0 then begin
   exitproc:=exitsave;
   speicher.init(sichername+'.nlb',stopenread,pufgroesse);
   speicher.read(idtest,sizeof(id));
   if idtest=id then begin
      tlfiles.streamget(speicher);
      kanaele.voreinstellung;
      nltrigg.streamget(speicher);
      tlfilter.streamget(speicher);
      if speicher.status<>stok then begin
         clrscr; writeln(lfcr);
         fehler('Can''t load configuration file.');
         writeln(lfcr,lfcr,speicher.status:10,speicher.errorinfo:10);
         halt end;
      end       else begin
      clrscr; writeln(lfcr);
      if copy(idtest,1,5)='Neurl' then
        fehler('Configuration file (format '+copy(idtest,6,3)+') is not compatible.')
                                  else
        fehler('Configuration file incorrect.');
      warte;
      end;
   speicher.done;
   exitproc:=@schluss;
   zoeger(4000);
   end        else zoeger(6000);
end;

function heapvoll (groesse:word):integer; far;
begin
if groesse=0 then exit;
exitproc:=exitsave;
closegraph; window(1,1,80,25); clrscr;
pieps; writeln(lfcr);
if groesse>memavail then write('Memory overflow')
                    else
   if groesse>maxavail then write('Memory full or too small fragments')
                    else write('Memory error');
writeln(', please start again with fingers crossed.',lfcr);
daff.ausserbetrieb; sichern; halt;
end;

procedure schluss;
begin
exitproc:=exitsave;
closegraph; textmode(co80);
if erroraddr<>nil then
   writeln(lfcr,lfcr,'Sorry, error causes abortion.',lfcr)
                  else writeln('Bye bye!',lfcr);
daff.ausserbetrieb;
sichern;
end;

begin
exitsave:=exitproc;
exitproc:=@schluss;
heaperror:=@heapvoll;
if paramcount>0 then sichername:=paramstr(1);
laerman;
clrscr;
textcolor(cyan);
gotoxy(1,6);
writeln(lfcr,'':16,'------------------------------------------',
        lfcr,'':16,'--------   NEUROLAB ',version:4,' (',plattform:5,')  --------',
        lfcr,'':16,'------------------------------------------',
        lfcr,'':16,'------  by B. Hedwig and M. Knepper ------',
        lfcr,'':16,'--- I. Zoologisches Institut G”ttingen ---',
        lfcr,'':16,'------------------------------------------');
writeln(lfcr,lfcr);
writeln('':7,'Program for the analysis of neurobiological and behavioural data');
textcolor(lightgray); gotoxy(1,20);
holen;
repeat
   ueberschrift(false,'Main Menu','Info',farbe1);
   writeln('Free Memory       :   ',memavail div 1024,' (',maxavail div 1024,') kByte');
   writeln('Configuration File:   ',sichername,' (Format:',paramver,')');
   writeln('Parameters        :   Channels (Max.): ',kan,', Max. Sampling Rate: ',fre:4:2,' Hz');
   writeln('Open Files        :   ',filenr);
   gotoxy(1,10); zwischen('Menu',farbe1);
   writeln(
     lfcr,'  f...File Manager                      t...Trigger Manager',
     lfcr,'  c...Calibration',
     lfcr,'  m...Filter Manager',
     lfcr,'  l...List Sampled Data                 a...Analog Data',
     lfcr,'  v...View Data                         i...Interval Data',
     lfcr,
     lfcr,'  s...Save Configuration File           e...End',lfcr);
   zwischen('Dialogue',farbe1);
   window(1,24,80,25);
   if filenr=0 then begin
      case upcase(readcharim('Menu Point','f')) of
         'F':nlfiles.manager;
         'E':halt;
         'M','L','T','C','A','V','S','I':begin
            fehler('No open file: Start with file manager!'); warte end;
         end;
      end      else begin
      case upcase(readcharim('Menu Point','e')) of
         'F':nlfiles.manager;
         'C':verstaerkungen;          'M':filterung;
         'L':listen;                  'V':sichten;
         'T':nltrigg.manager;         'S':sichern;
         'A':analogdaten;             'I':intervalldaten;
         'E':begin
              write(lfcr,'Exit? (Y/N) ');
              if readkey in ['j','J','y','Y'] then halt end;
         '?':begin window(1,1,80,25); clrscr; zeigertest end;
         end;
      end;
until false
end.
