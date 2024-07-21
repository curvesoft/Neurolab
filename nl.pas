
{$R *.res}

{ Borland-Pascal 7.0 / FPC 2.4 }
{$ifdef fpc} {$mode TP} {$endif}

program neurolab;
{Authors: Berthold Hedwig & Marko Knepper}

{$IFDEF MSDOS}
{$A+,B-,E+,F-,G-,I-,N+,O-,P+,T+,V+,X-} {$M 65520,130000,655360}
{$ELSE}
{$A+,B-,E+,F-,G+,I-,N+,P+,T+,V+,X-} {$M 65520,0}
{$ENDIF}

uses  crt, dos,           daff,wavpcm,tulab42,nlrahmen,           grafik,
                          tlfilter,           nltrigg,            nlgrafik,
      bequem,             tlfiles,            nlfiles,
      objects,                                nlsicht,
                                              nlausw;

const version='9.20';  paramver='8.5'; {Beim Erhoehen Diffilter aus nltrigg rausnehmen und komp84 abschaffen!}
      {$IFDEF DPMI}   plattform='DPMI  BP7'; {$ENDIF}
      {$IFDEF MSDOS}  plattform='MSDOS BP7'; {$ENDIF}
      {$IFDEF fpc}    plattform={$I %FPCTARGET%}+' '+{$I %FPCVERSION%}; {$ENDIF}
      {$IFDEF FPC} maxavail=9223372036854775807; memavail=maxavail; {$ENDIF}
      {$ifdef fpc} nlbext='.nlx'; {$else} nlbext='.nlb'; {$endif}
      id:string[8]='Neurl'+paramver;
      sichername:string='NEUROLAB';
      {$IFDEF fpc} pufgroesse=65535; {$ELSE} pufgroesse=32768; {$ENDIF}

var   exitsave:pointer;
      named:dirstr; namen:namestr; namee:extstr;

procedure verstaerkungen;
var   i,j:grossint;
      mult:extended; textstr:string20;
procedure tabelle (von:byte);
var   i:byte;
begin
if von<=kan-1 then
   writeln('Channel':7,'Label':11,'Factor':11,'Unit':8);
for i:=von to min(von+7,kan-1) do with grund[i] do
   writeln(i:4,schriftliste[i]:14,extewort(multi,2,2):11,einhwort:8);
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
           if (mult>1e-19) and (mult<1e19) then
              grund[i].setz(mult,textstr)
                                             else begin
              fehler('Value out of range'); warte end;
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
writeln(     ' y : y-Resolution   a : Amplification  - : Invert Sign    s : Spike Filter',
        lfcr,' j : Points (TL)    o : +/- Offset     v : Absolute Value g : Gliding Av.    ',
        lfcr,' c : Clip           h : High-Pass      2 : Square         z : Gliding Length ',
        lfcr,' t : +/- Time Shift l : Low-Pass       r : Reciprocal V.  f : Frequency (TL) ',
        lfcr,' # : Count (TL)     d : Differentiate  + : Summation      i : Interval (TL)',
        lfcr,' p : Polygon (TL)   w : Integration    e : arcsin         x : Time Diff.(TL)',
        lfcr,' m : Max - Min      n : Gl. Integr.    k : arccos         b : Phase (TL)',
        lfcr,' = : Correlation    > : ASCII Data     . : Pulse counter  / : Angle',
        lfcr,' q : Absolute (x y) u : Integration (TL)');
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
if not ((k in [kan..kan+filtermax-1]) and (k<>ke)) then begin
   fehler('Valid output channel no. expected'); warte; exit end;
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
      '2':filtersetz(new(squarezg,neu),k);
      'a':filtersetz(new(malfaktorzg,neu(
             readext('Amplification: Factor',1,4,2))),k);
      'b':filtersetz(new(phasenfilterzg,neu(
             upcase(readchar('Phase filter: Reference List','A')),
             upcase(readchar('              Event List','B')),
                     readint('              Offset Periods',0))),k);
      'c':filtersetz(new(kappenzg,neu),k);
      'd':filtersetz(new(diffzg,neu),k);
      'e':filtersetz(new(arcsinzg,neu),k);
      'f':filtersetz(new(freqfilterzg,neu(
            upcase(readchar('Frequency: for Trigger List (A-'+listmax+')','A')))),k);
      'g':filtersetz(new(glattzg,neu(
            readext('Gliding Average: Width [ms]',1,3,1))),k);
      'h':filtersetz(new(hochpasszg,neu(
             readint('High-Pass: Frequency (min. '
                     +wort(round(genau*pi/weite*fre))+' Hz)',round(fre/2)))),k);
      'i':filtersetz(new(intervallfilterzg,neu(
            upcase(readchar('Interval: for Trigger List (A-'+listmax+')','A')))),k);
      'j':filtersetz(new(punktefilterzg,neu(
            upcase(readchar('Points: Trigger List (A-'+listmax+')','A')))),k);
      'k':filtersetz(new(arccoszg,neu),k);
      'l':filtersetz(new(tiefpasszg,neu(
             readint('Low-Pass: Frequency (min. '
                     +wort(round(genau*pi/weite*fre))+' Hz)',round(fre/2)))),k);
      'm':filtersetz(new(maxminzg,neu(
            readext('Max - Min: Width'#29' [ms]',1,3,1))),k);
      'n':filtersetz(new(glintzg,neu(
            readext('Gliding Integration: Width [ms]',1,3,1))),k);
      'o':begin
         einheitensetzen(fre);
         filtersetz(new(offsetzg,neu(k,
            round(readext('Offset: Value ['+belegungsliste[k].einhwort
                          +']',0,3,1)/belegungsliste[k].faktor))),k);
         end;
      'p':filtersetz(new(polygonfilterzg,neu(
            upcase(readchar('Polygon: for Trigger List (A-'+listmax+')','A')))),k);
      'q':filtersetz(new(betragzg,neu(
             readint('Absolute (x y): y channel No',0))),k);
      'r':filtersetz(new(einsdurchzg,neu),k);
      's':begin
        einheitensetzen(fre);
        ableitungein:=belegungsliste[k]; dec(ableitungein.sekunde);
        filtersetz(new(spikefilterzg,neu(k,
           readext('Spike Filter: Max. Width '#29' [ms]',3,3,1),
           readext('              Rising Gradient  '#24' ['+ableitungein.einhwort+']',0,3,1),
           readext('              Falling Gradient '#25' ['+ableitungein.einhwort+']',0,3,1))),k);
        end;
      't':filtersetz(new(verschiebezg,neu(
            readext('Time Shift: Value [ms]',0,3,1))),k);
      'u':filtersetz(new(tlintfilterzg,neu(
            upcase(readchar('Integration: for Trigger List (A-'+listmax+')','A')))),k);
      'v':filtersetz(new(absolutzg,neu),k);
      'w':filtersetz(new(intzg,neu),k);
      'x':filtersetz(new(diffilterzg,neu(
            upcase(readchar('Time Difference: Reference List','A')),
            upcase(readchar('                 Event List','B')),
            readint('                 Time Window [ms]',100),
            upcase(readchar('                 n=nearest, f=forward, b=backward','n')))),k);
      'y':begin
        einheitensetzen(fre);
        filtersetz(new(streckungzg,neu(k,
             readext('y-Resolution: max value ['+belegungsliste[k].einhwort+']',
                     spannung(maxsample,k),4,2))),k);
        end;
      'z':filtersetz(new(gllinzg,neu(
            readext('Gliding Length: Width [ms]',1,3,1))),k);
      '-':filtersetz(new(invertzg,neu),k);
      '+':filtersetz(new(additionzg,neu(
             readint('Summation: Channel No',0))),k);
      '=':filtersetz(new(korrelationzg,neu(
            readint('Correlation: Channel No',0),
            readext('Correlation: Width [ms]',1,3,1))),k);
      '#':filtersetz(new(zaehltfilterzg,neu(
       upcase(readchar('Count: Trigger List (A-'+listmax+')','A')))),k);
      '>':filtersetz(new(asciifilterzg,neu(
                 readstring('ASCII Data: File Name','list.asc'),
            upcase(readchar('            Assign to Trigger List (A-'+listmax+')','A')))),k);
      '/':filtersetz(new(winkelzg,neu(
             readint('x-y-angle: x channel No',0))),k);
      '.':begin
         einheitensetzen(fre);
         filtersetz(new(digitalzg,neu(k,
            round(readext('Pulse counter: Threshold ['+belegungsliste[k].einhwort
                          +']',500,3,1)/belegungsliste[k].faktor),
            readstring('               Pulse separation SI unit','1'),
            readext('               Pulse separation value',1,5,3))),k);
         end

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
   write(lesef(j,k):10);
   if j mod 168 = 167 then begin
      writeln('Continue: <Return>, Abort: <Esc>');
      if readkey=#27 then begin schliesse; exit end;
      end;
   end;
schliesse;
warte;
end;

procedure analogdaten;
procedure superposition;
const von:messwert=0;       bis:messwert=1000;
      akttrind:char='A';
var   laenge:grossint;
      chpuff:char;
      grafik:grafiksuperposition;
begin
ueberschrift(false,'Superposition','Info',farbe3);
triggeruebersicht;
gotoxy(1,18); zwischen('Dialogue',farbe3); writeln;
chpuff:=upcase(readchar('Trigger List',akttrind));
if not (chpuff in ['A'..listmax]) then begin
   fehler('Undefined Trigger List'); warte; exit end;
akttrind:=chpuff;
if tliste[akttrind]^.triggsum=0 then
    begin writeln('No trigger point.'); warte; exit end;
clrscr; kanaele.lesen(5,farbe3); if not (kanaele.kn in [1..32]) then exit;
window(1,22,80,25); clrscr;
von:=messwext(readext('Start Time [ms]',extzeit(von),1,0));
bis:=messwext(readext('End Time [ms]  ',extzeit(bis),1,0));
laenge:=round(bis-von);
if (laenge>maxanzahl) or (laenge<=0) then begin
   fehler('Undefined time window'); warte; exit end;
grafik.aufbauen(kanaele,von,bis,akttrind);
end;

procedure averagen;
const von:messwert=0;       bis:messwert=500;
      akttrind:char='A';
var   laenge,platz:grossint;
      i,gesamt:grossint;
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
von:=messwext(readext('Start Time [ms]',extzeit(von),1,0));
bis:=messwext(readext('End Time [ms]  ',extzeit(bis),1,0));
laenge:=round(bis-von); platz:=(laenge+1)*sizeof(wert);
if (laenge>maxanzahl) or (laenge<=0) then begin
   fehler('Undefined time window'); warte; exit end;
{$ifndef fpc}
for i:=0 to maxkanal do if i in kanaele.dabei then begin
   if maxavail<platz then begin
      fehler('Memory not sufficient');
      writeln(lfcr,'Help: Use less channels or shorter averaging time.');
      for i:=i-1 downto 0 do if i in kanaele.dabei then
         freemem(grafik.mittel[i],platz);
      warte; exit end;
   getmem(grafik.mittel[i],platz);
   fillchar(grafik.mittel[i]^,platz,0) end;
{$endif}
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
{$ifndef fpc}
for i:=0 to maxkanal do if i in kanaele.dabei
   then freemem(grafik.mittel[i],platz);
{$endif}
end;

procedure phasenaveragen;
const von=0;
      minabst:messwert=0;             maxabst:messwert=10000;
      akttrind:char='A';
var   weis:triggerweiser;
      i,laenge,platz:grossint;
      bis:messwert;
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
minabst:=messwext(readext(
   'Min. Cycle Duration [ms]',extzeit(minabst),1,0));
maxabst:=messwext(readext(
   'Max. Cycle Duration [ms]',extzeit(maxabst),1,0));
weis.zaehlen(tliste[akttrind]^,minabst,maxabst);
if weis.gesamt=0 then begin writeln('No trigger point'); piep; warte; exit end;
bis:=weis.mittelabstand;
laenge:=round(bis-von); platz:=(laenge+1)*sizeof(wert);
if laenge>maxanzahl then begin
   fehler('Averaging time too long.'); warte; exit end;
{$ifndef fpc}
for i:=0 to maxkanal do if i in kanaele.dabei then begin
   if maxavail<platz then begin
      fehler('Memory not sufficient');
      writeln(lfcr,'Help: Use less channels or shorter averaging time.');
      for i:=i-1 downto 0 do if i in kanaele.dabei then
         freemem(grafik.mittel[i],platz);
      warte; exit end;
   getmem(grafik.mittel[i],platz);
   fillchar(grafik.mittel[i]^,platz,0) end;
{$endif}
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
{$ifndef fpc}
for i:=0 to maxkanal do if i in kanaele.dabei then freemem(grafik.mittel[i],platz);
{$endif}
end;

procedure xydiagramm;
const   kx:grossint=0;
        ky:grossint=1;
var     puff:grossint;
        liste:filterliste;
        grafik:grafikxy;
begin
ueberschrift(false,'X-Y-Diagram','Info',farbe3);
belegungzeigen; writeln;
liste.zeigen(7,kan); writeln;
zwischen('Dialogue',farbe3);
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

procedure ampnormalhist;
const ref:char='A';
      diffn:word=100;
      ybereich:extended=1;
      kanal:byte=0;
      minsamp:sample=minsample;
      maxsamp:sample=maxsample;
var   histogramm:ampnormalhistogramm;
      charpuff:char; wordpuff:word;
      extpuff:extended; puff:grossint;
      liste:filterliste;
begin
ueberschrift(false,'Amplitude Histogram','Info',farbe3);
triggeruebersicht;
gotoxy(1,12); zwischen('Dialogue',farbe3);
window(1,16,80,zeilmax);
charpuff:=upcase(readchar('Trigger List    ',ref));
if not (charpuff in ['A'..listmax]) then begin
   fehler('Undefined Trigger List'); warte; exit end;
ref:=charpuff;
window(1,3,80,zeilmax); clrscr; textcolor(farbenormal);
belegungzeigen; writeln;
liste.zeigen(5,kan); writeln;
zwischen('Dialogue',farbe3);
write(lfcr,'Continue list? (Y/N) ');
while not liste.ende and (readkey in ['Y','y','J','j']) do liste.weiterzeigen;
write(#13); clreol;
window(1,22,80,zeilmax);
puff:=readint('Channel   ',kanal);
if not (puff in [0..kan+filtermax-1]) then begin
   fehler('Undefined channel no.'); warte; exit end
                                      else kanal:=puff;
wordpuff:=readint('Bins (Max. '+wort(maxfeld)+')',diffn);
if (wordpuff<=0) or (wordpuff>maxfeld) then begin
   fehler('Undefined number of bins'); warte; exit end;
diffn:=wordpuff;
extpuff:=kon(norm(readext('Min. amplitude ['+belegungsliste[kanal].einhwort+']',
                      extspannung(rekon(minsamp,kanal),kanal),1,1),kanal),kanal);
minsamp:=round(mine(maxe(extpuff,minsample),maxsample));
extpuff:=kon(norm(readext('Max. amplitude ['+belegungsliste[kanal].einhwort+']',
                      extspannung(rekon(maxsamp,kanal),kanal),1,1),kanal),kanal);
maxsamp:=round(maxe(mine(extpuff,maxsample),minsample));
if maxsamp<=minsamp then begin
   fehler('Bad amplitude range'); warte; exit end;
ybereich:= readext('y-Scale Factor   ',ybereich,4,2);
histogramm.aufbauen(kanal,ref,diffn,ybereich,minsamp,maxsamp);
end;

begin
repeat
   ueberschrift(false,'Analog Data','Menu',farbe2);
   gotoxy(1,7);
   writeln(     '          a...Averaging',
           lfcr,'          p...Phase-Dependent Averaging',
           lfcr,'          x...X-Y-Diagram',
           lfcr,'          h...Amplitude Histogram',
           lfcr,'          s...Superposition',
      lfcr,lfcr,'          m...Main Menu',lfcr);
   gotoxy(1,19);
   zwischen('Dialogue',farbe2);
   writeln;
   case upcase(readcharim('Menu Point','m')) of
      'A':averagen;
      'P':phasenaveragen;
      'X':xydiagramm;
      'H':ampnormalhist;
      'S':superposition;
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
wordpuff:=readint('Bins (Max. '+wort(maxfeld)+')',diffn);
if (wordpuff<=0) or (wordpuff>maxfeld) then begin
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
wordpuff:=readint('Bins (Max. '+wort(maxfeld)+')',diffn);
if (wordpuff<=0) or (wordpuff>maxfeld) then begin
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
wordpuff:= readint('Bins (Max. '+wort(maxfeld)+')',diffn);
if (diffn<=0) or (diffn>maxfeld) then begin
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
wordpuff:= readint('Bins (Max. '+wort(maxfeld)+')',diffn);
if (wordpuff<=0) or (wordpuff>maxfeld) then begin
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
wordpuff:= readint('Bins (Max. '+wort(maxfeld)+')',diffn);
if (wordpuff<=0) or (wordpuff>maxfeld) then begin
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
charpuff:=upcase(readchar('Reference TL            ',ref));
if not (charpuff in ['A'..listmax]) then begin
   fehler('Undefined Trigger List'); warte; exit end;
ref:=charpuff;
minabst:=   messw(readint('Min. Cycle Duration [ms]',zeit(minabst)));
maxabst:=   messw(readint('Max. Cycle Duration [ms]',zeit(maxabst)));
charpuff:=upcase(readchar('Event TL                ',obj));
if not (charpuff in ['A'..listmax]) then begin
   fehler('Undefined Trigger List'); warte; exit end;
obj:=charpuff;
anfph:=           readext('Start Phase (0..1)      ',anfph,3,2);
if (anfph<0) or (anfph>1) then begin
   fehler('Value not allowed.'); warte; exit end;
schph:=           readext('End Phase (0..1)        ',schph,3,2);
if (schph<0) or (schph>1) then begin
   fehler('Value not allowed.'); warte; exit end;
wordpuff:=readint('Bins (Max. '+wort(maxfeld)+')        ',diffn);
if (wordpuff<=0) or (wordpuff>maxfeld) then begin
   fehler('Undefined number of bins'); warte; exit end;
diffn:=wordpuff;
ybereich:=readext('y-Scale Factor          ',ybereich,4,2);
histogramm.aufbauen(ref,obj,minabst,maxabst,anfph,schph,diffn,ybereich);
end;

begin
repeat
   ueberschrift(false,'Interval Data','Info',farbe2);
   triggeruebersicht;
   gotoxy(1,13);
   zwischen('Menu',farbe2);
   writeln;
   writeln('          i...Interval Histogram          a...Auto Correlogram',
      lfcr,'          l...Latency Histogram           c...Cross Correlogram',
      lfcr,'          s...PST-Histogram               p...Phase Histogram',
      lfcr,'          m...Main Menu');
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
   speicher.init(sichername+nlbext,stcreate,pufgroesse);
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

procedure confsichern;
var filename:namestr;
    pname:pathstr;
    dname:dirstr;
    ename:extstr;
begin
pname:=readstring('Configuration file name and path',sichername);
fsplit(pname,dname,filename,ename);
if fileschonda(dname+filename+nlbext) then
   if upcase(readchar('Overwrite? (Y/N)','N'))='Y' then begin
      sichername:=dname+filename;
      sichern;
      end else
                                      else begin
   sichername:=dname+filename;
   sichern;
   end;
end;

{$ifndef fpc}
procedure grafikkarte;
begin
ueberschrift(false,'Grafics Adaptor','Info',farbe2);
writeln(lfcr,'   Adaptor     #              Mode:  Low #   Medium #     High #',
        lfcr,
        lfcr,'   Automatic 255                         -          -        255',
        lfcr,
        lfcr,'   EGA         3                         0          -          1',
        lfcr,'   EGA64       4                         0          -          1',
        lfcr,'   Herc Mono   7                         -          -          0',
        lfcr,'   VGA         9                         0          1          2');
{$ifndef msdos}
writeln('   VESA 16   ',vesa16:3,'                         0          1          2');
{$endif}
gotoxy(1,14);
zwischen('Dialogue',farbe2);
writeln;
grtreiber:=readint('Graphics Adaptor #',grtreiber);
grmodus:=readint('Graphics Mode #',grmodus);
end;
{$endif}

procedure holen;
var   speicher:tbufstream;
      such:searchrec;
      idtest:string[8];
begin
findfirst(sichername+nlbext,anyfile,such);
if doserror=0 then begin
   exitproc:=exitsave;
   speicher.init(sichername+nlbext,stopenread,pufgroesse);
   speicher.read(idtest,sizeof(id));
   komp84:=(idtest='Neurl8.4') and (id='Neurl8.5');  if komp84 then idtest:='Neurl8.5';
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
   zoeger(5000);
   end        else zoeger(8000);
end;

{$IFNDEF FPC}

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

{$ENDIF}

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
{$IFNDEF FPC} heaperror:=@heapvoll; {$ENDIF}
if paramcount>0 then sichername:=paramstr(1);
fsplit(sichername,named,namen,namee);
sichername:=named+namen;
laerman;
clrscr;
textcolor(cyan);
gotoxy(1,6);
writeln(lfcr,'':16,'------------------------------------------',
        lfcr,'':16,'----   NEUROLAB ',version:4,' (',plattform:13,')  ----',
        lfcr,'':16,'------------------------------------------',
        lfcr,'':16,'------  by B. Hedwig and M. Knepper ------',
        lfcr,'':16,'---------- Cambridge / Wiesbaden ---------',
        lfcr,'':16,'------------------------------------------');
writeln(lfcr,lfcr);
writeln('':7,'Program for the analysis of neurobiological and behavioural data');
textcolor(lightgray); gotoxy(1,20);
holen;
repeat
   ueberschrift(false,'Main Menu','Info',farbe1);
   {$ifdef fpc}
   writeln('Version           :   ',version:4,' (',plattform:5,')');
   {$else}
   writeln('Free Memory       :   ',memavail div 1024,' (',maxavail div 1024,') kByte');
   {$endif}
   writeln('Configuration File:   ',sichername+nlbext,' (Format:',paramver,')');
   writeln('Parameters        :   Channels (Max.): ',kan,', Max. Sampling Rate: ',fre:4:2,' Hz');
   writeln('Open Files        :   ',filenr);
   gotoxy(1,9); zwischen('Menu',farbe1);
   writeln(
     lfcr,'  f...File Manager                      t...Trigger Manager',
     lfcr,'  c...Calibration',
     lfcr,'  m...Filter Manager',
     lfcr,'  l...List Sampled Data                 a...Analog Data',
     lfcr,'  v...View Data                         i...Interval Data',
     lfcr,
     {$ifdef fpc}
     lfcr,'  -/+.Sound off/on',
     {$else}
     lfcr,'  g...Graphics Adaptor                  -/+.Sound off/on',
     {$endif}
     lfcr,'  s...Save Configuration File           e...End',lfcr);
   zwischen('Dialogue',farbe1);
   window(1,24,80,25);
   if filenr=0 then begin
      case upcase(readcharim('Menu Point','f')) of
         '+':laerman;                 '-':laermaus;
         'F':nlfiles.manager;
         'E':halt;
         'M','L','T','C','A','V','S','I':begin
            fehler('No open file: Start with file manager!'); warte end;
         end;
      end      else begin
      case upcase(readcharim('Menu Point','e')) of
         '+':laerman;                 '-':laermaus;
         'F':nlfiles.manager;
         'C':verstaerkungen;          'M':filterung;
         'L':listen;                  'V':sichten;
         'T':nltrigg.manager;         'S':confsichern;
         {$ifndef fpc} 'G':grafikkarte; {$endif}
         'A':analogdaten;             'I':intervalldaten;
         'E':begin
              write(lfcr,'Exit? (Y/N) ');
              if readkey in ['j','J','y','Y'] then halt end;
         '?':begin window(1,1,80,25); clrscr; zeigertest end;
         end;
      end;
until false
end.
