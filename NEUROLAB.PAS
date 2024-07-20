{Turbo-Pascal 6.0}

program neurolab;

{$A+,B-,E+,F-,G+,I-,N+,O-,V-,X-}
{$M 65520,131040,655360}

uses  crt,                graph,              tulab,              nlrahmen,
      dos,                grafik,             tlfilter,           nltrigg,
      bequem,             nlgrafik,           tlfiles,            nlfiles,
      objects;

const version='5.0';
      id:string[8]='Neurolab';
      sichername:namestr='NEUROLAB';
      pufgroesse=32768;

      anfang:messwert=0;

var   exitsave:pointer;

      mittel:mittelliste;

procedure verstaerkungen;
var   i,j:longint;
      mult:extended; textstr:string20;
procedure tabelle (von:byte);
var   i:byte;
begin
if von<=kan-1 then
   writeln('Kanal':5,'Belegung':11,'Faktor':9,'Einheit':11);
for i:=von to min(von+7,kan-1) do with grund[i] do
   writeln(i:3,schriftliste[i]:13,extewort(multi,2,2):11,einhwort:8);
end;
begin
ueberschrift('Skalierung','Info',farbe2);
i:=0;
repeat
   window(1,3,38,12); clrscr; tabelle(0);
   window(43,3,80,12); clrscr; tabelle(8);
   window(1,13,80,19); zwischen('MenÅ',farbe2);
   writeln(lfcr,'  s...Skalieren',lfcr,'  h...HauptmenÅ',lfcr);
   window(1,18,80,25); clrscr;
   zwischen('Dialog',farbe2); writeln;
   case upcase(readchar('MenÅpunkt','s')) of
      'S':begin
        clrscr; zwischen('Dialog',farbe3);
        window(1,20,80,25);
        j:=readint('Kanal Nr.',i);
        if not(j in [0..kan-1]) then begin
           writeln; fehler('UnzulÑssige Kanalnummer'); warte end
                                else begin
           i:=j;
           textstr:=readstring('Grundeinheit',grund[i].einhwort);
           writeln('Der Faktor gibt an, wieviel ',textstr,
                      ' im Experiment 1V am A-D-Wandler entsprechen.');
           mult:=readexte('Faktor ['+textstr+']',grund[i].multi,2,2);
           grund[i].setz(mult,textstr);
           end;
        end;
      'H':begin einheitensetzen(fre); exit end;
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
writeln('Filter:');
writeln(lfcr,'    o : Ò Offset          v : VerstÑrkung       k : Kappen',
        lfcr,'    i : Invertierung      b : Betrag            q : Quadrat',
        lfcr,'    g : GlÑtten           t : Tiefpa·           h : Hochpa·',
        lfcr,'    d : Differenzierung   s : Spikefilter       m : Max - Min',
        lfcr,'    z : Ò Zeitverschiebung');
end;

procedure filtersetzen;
var    ableitungein:einheittyp;
       ke,k,i:byte;
       fi:string80;
begin
ueberschrift('Filter setzen','Info',farbe3);
belegungzeigen;
gotoxy(1,15); zwischen('Dialog',farbe3); writeln;
ke:=readint('Eingangskanal Nr. (0-'+wort(pred(kan))+')',0);
if not (ke in [0..kan]) then begin
   fehler('UnzulÑssige Kanalnr.'); warte; exit end;
clrscr; writeln('Eingangskanal: ',ke,' (',schriftliste[ke],')',lfcr);
liste.zeigen(11,kan);
gotoxy(1,15); zwischen('Dialog',farbe3);
write(lfcr,'Filterkanalliste weiterzeigen? (J/N) ');
while not liste.ende and (readkey in ['Y','y','J','j']) do liste.weiterzeigen;
write(#13); clreol;
k:=readint('Ausgangskanal Nr. ('+wort(kan)+'-'+wort(kan+filtermax-1)+')',kan);
if not (k in [kan..kan+filtermax-1]) then begin
   fehler('UnzulÑssige Kanalnr.'); warte; exit end;
clrscr;
writeln('Eingangskanal: ',ke,' (',schriftliste[ke],')',lfcr,
        'Ausgangskanal: ',k);
write('Bisher: '); if filterdrin(k) then write(filterzeile(k));
gotoxy(1,5); filteruebersicht;
gotoxy(1,15); zwischen('Dialog',farbe3);
window(1,19,80,25);
kanalsetz(k,ke);
fi:=readstring('Neue Filterfolge','');
for i:=1 to length(fi) do
   case fi[i] of
      'o':begin
         einheitensetzen(fre);
         filtersetz(new(offsetzg,neu(k,round(readext('Offset: Hîhe ['
            +belegungsliste[k].einhwort+']',0,3,1)/belegungsliste[k].faktor))),k);
         end;
      'v':filtersetz(new(malfaktorzg,neu(readext('VerstÑrkung: Faktor',1,4,2))),k);
      'k':filtersetz(new(kappenzg,neu),k);
      'b':filtersetz(new(absolutzg,neu),k);
      'q':filtersetz(new(squarezg,neu),k);
      't':filtersetz(new(tiefpasszg,neu(readint('Tiefpa·: Grenzfrequenz (min. '
            +wort(round(genau*pi/weite*fre))+' Hz)',round(fre/2)))),k);
      'h':filtersetz(new(hochpasszg,neu(readint('Hochpa·: Grenzfrequenz (min. '
            +wort(round(genau*pi/weite*fre))+' Hz)',round(fre/2)))),k);
      'd':filtersetz(new(diffzg,neu),k);
      'z':filtersetz(new(verschiebezg,neu(
            readext('Zeitverschiebung: um [ms]',0,3,1))),k);
      'm':filtersetz(new(maxminzg,neu(readext('Max - Min  : Breite'#29' [ms]',1,3,1))),k);
      's':begin
        einheitensetzen(fre);
        ableitungein:=belegungsliste[k]; dec(ableitungein.sekunde);
        filtersetz(new(spikefilterzg,neu(k,
           readext('Spikefilter: max. Breite '#29' [ms]',3,3,1),
           readext('             Anstiegssteigung '#24' ['+ableitungein.einhwort+']',0,3,1),
           readext('             Abfallssteigung '#25' ['+ableitungein.einhwort+']',0,3,1))),k);
        end;
      'i':filtersetz(new(invertzg,neu),k);
      'g':filtersetz(new(glattzg,neu(readext('GlÑtten: Breite [ms]',1,3,1))),k);
      else fehler('Filter "'+fi[i]+'" gibt es nicht.');
      end;
indexalt:=k;
end;

begin
indexalt:=kan;
repeat
   ueberschrift('Filter-Manager','Info:',farbe2);
   liste.zeigen(11,indexalt); writeln;
   zwischen('MenÅ',farbe2); writeln;
   writeln('f...Info Filter                          s...Filter setzen',lfcr,
           'a...Info AusgangskanÑle (fortsetzen)     h...HauptmenÅ',lfcr,
           'e...Info EingangskanÑle');
   writeln;
   zwischen('Dialog',farbe2);
   writeln; write('Wahlpunkt: h'#8);
   repeat
      wahl:=readkey;
      wx:=wherex; wy:=wherey;
      window(1,3,80,15); clrscr;
      case wahl of
         'a':begin indexalt:=liste.index; liste.zeigen(11,liste.index) end;
         'e':begin belegungzeigen; liste.index:=indexalt end;
         'f':begin filteruebersicht; liste.index:=indexalt end;
         's':filtersetzen;
         'h',#13,#27:begin einheitensetzen(fre); exit end;
         end;
      window(1,3,80,25); gotoxy(wx,wy);
   until wahl='s';
until false;
end;

procedure listen;
const aktfile:byte=1;
      k:byte=0;
var   i,j:longint;
begin
ueberschrift('Daten listen','Info',farbe2);
fileliste;
gotoxy(1,19); zwischen('Dialog',farbe2); writeln;
i:=readint('File Nr',aktfile);
if not (i in [1..filenr]) then begin
   fehler('UnzulÑssige File-Nr.'); warte; exit end;
aktfile:=i;
clrscr; belegungzeigen; writeln;
gotoxy(1,19); zwischen('Dialog',farbe2); writeln;
i:=readint('Kanal Nr.',k);
if  not (i in [0..pred(kan)]) then begin
   fehler('UnzulÑssige Kanal-Nr.'); warte; exit end;
k:=max(i,0);
oeffnen(aktfile);
i:=zwi(messw(readint('Anfangszeitpunkt [ms]',0)));
clrscr;
for j:=i to liste[aktfile].ko.anzahl-1 do begin
   write(lese(j,k):8);
   if j mod 210 = 209 then begin
      writeln('Weiter mit <Return>, Abbruch mit <Esc>');
      if readkey=#27 then begin schliesse; exit end;
      end;
   end;
schliesse;
warte;
end;

function datzw (stelle:messwert; kanal:byte):integer; far;
begin
datzw:=dat(zwi(stelle),kanal);
end;

procedure sichten;
type  statustyp=(bleibt,neu);
const statustext:array[statustyp] of string20=(' -','Block setzen');
      tstellenmuster:set of 0..15=[0,4,8,12];
      strichfarbe=white;
      buchbr=80;
      abl:extended=200;
      lupe=4;
      aktfile:byte=1;
var   tstellenpattern:word absolute tstellenmuster;
      spannstatus:boolean;
      spalte1,spalte2,strich:word;
      bildlaenge,strichstelle:messwert;
      wandert:listenzeiger; pwandert:punktzeiger;
      status:statustyp;                    gr:byte;
      zei1,zei2,zei3,zei1a,zei2a,zei3a:string20;
      tb:char;
      i,kannr:byte;
      extvar:extended;
   procedure schritt (auf:word);
   begin
   setlinestyle(dashedln,0,normwidth);
   setcolor(min(strichfarbe,getmaxcolor));
   line(strich,oben,strich,maxy-11);
   strich:=auf;
   line(strich,oben,strich,maxy-11);
   setcolor(getmaxcolor);
   end;
   procedure grafik;
   var   wandert:listenzeiger; pwandert:punktzeiger;
         di:dirstr; na:namestr; ex:extstr;
   begin
   richtung:=vow;
   grafiknormal(anfang,bildlaenge,kanaele,datzw,anfang);
   settextjustify(lefttext,centertext);
   fsplit(liste[aktfile].name,di,na,ex);
   outtextxy(0,4, 'File    : '+na+ex);
   outtextxy(0,14,'Dauer   : '+wort(zeit(liste[aktfile].laenge))+' ms');
   outtextxy(0,24,'Zeitabl.: '+extwort(abl,4,2)+' mm/s');
   outtextxy(spalte1,4, 'Kanal   :');
   outtextxy(spalte1,14,'Position:');
   outtextxy(spalte1,24,'Amplit. :');
   outtextxy(spalte2,4, 'Modus   : '+statustext[status]);
   settextjustify(centertext,centertext);
   setwritemode(xorput);
   wandert:=liste[aktfile].block^;
   while wandert^.bis<anfang do wandert:=wandert^.next;
   while wandert^.von<anfang+bildlaenge do begin
      linie (max(lrand+1,stellex(wandert^.von)),min(maxx,stellex(wandert^.bis)));
      wandert:=wandert^.next end;
   setlinestyle(userbitln,tstellenpattern,normwidth);
   pwandert:=liste[aktfile].selbst^;
   while pwandert^.bei<anfang do pwandert:=pwandert^.next;
   while pwandert^.bei<anfang+bildlaenge do begin
      line(stellex(pwandert^.bei),oben,stellex(pwandert^.bei),maxy-11);
      pwandert:=pwandert^.next end;
   if (strichstelle>anfang) and (strichstelle<anfang+bildlaenge) then
      strich:=stellex(strichstelle) else strich:=maxx div 2;
   setcolor(min(strichfarbe,getmaxcolor));
   setlinestyle(dashedln,0,normwidth);
   line(strich,oben,strich,maxy-11);
   settextjustify(lefttext,centertext);
   setcolor(getmaxcolor);
   if spannstatus then richtung:=mit;
   zei1a:=''; zei2a:=''; zei3a:='';
   end;
begin
ueberschrift('Daten sichten','Info',farbe2);
fileliste;
gotoxy(1,19); zwischen('Dialog',farbe2); writeln;
i:=readint('File Nr.',aktfile);
if not (i in [1..filenr]) then begin
   fehler('UnzulÑssige File-Nr.'); warte; exit end;
if i<>aktfile then anfang:=0;
aktfile:=i;
with liste[aktfile] do begin
   clrscr; kanaele.lesen(6); kannr:=1;
   if kanaele.kn=0 then exit;
   extvar:=readext('Zeitablenkung [mm/s]',abl,3,1);
   if extvar<=0 then begin
      fehler('UnzulÑssige Zeitablenkung.');
      warte; exit end;
   abl:=extvar;
   anfang:=messw(readint('Anfangszeitpunkt [ms]',zeit(anfang)));
   oeffnen(aktfile);
   bildlaenge:=fre/abl*228; opengraph;
   spalte1:=maxx div 3; spalte2:=(2*maxx) div 3;
   spannstatus:=false; status:=bleibt; strichstelle:=bildlaenge/2;
   grafik;
   repeat
      strichstelle:=xstelle(strich);
      zei1:=wort(kannr)+' ('+schriftliste[kanaele.k[kannr]]+')';
      zei2:=wort(zeit(strichstelle))+' ms';
      if spannstatus then with kanaele do
         zei3:=extwort(extspannung(dat(zwi(strichstelle),k[kannr]),k[kannr]),4,2)
                      +' '+belegungsliste[k[kannr]].einhwort
                     else zei3:='';
      setcolor(0); outtextxy(spalte1+buchbr,4,zei1a);
      setcolor(getmaxcolor); outtextxy(spalte1+buchbr,4,zei1);
      setcolor(0); outtextxy(spalte1+buchbr,14,zei2a);
      setcolor(getmaxcolor); outtextxy(spalte1+buchbr,14,zei2);
      setcolor(0); outtextxy(spalte1+buchbr,24,zei3a);
      setcolor(getmaxcolor); outtextxy(spalte1+buchbr,24,zei3);
      zei1a:=zei1; zei2a:=zei2; zei3a:=zei3;
      case readkey of
             #0:case readkey of
         {Hoch}     #72: if kannr>1 then dec(kannr);
         {Runter}   #80: if kannr<kanaele.kn then inc(kannr);
         {PgUp}     #73: begin
                         anfang:=mine(anfang+bildlaenge,laenge);
                         grafik end;
         {Ctrl PgUp}#132:begin
                         anfang:=mine(anfang+bildlaenge/4,laenge);
                         grafik end;
         {PgDn}     #81: begin anfang:=maxe(0,anfang-bildlaenge); grafik end;
         {Ctrl PgDn}#118:begin anfang:=maxe(0,anfang-bildlaenge/4); grafik end;
         {F1}       #59: begin
                         abl:=abl*lupe; bildlaenge:=bildlaenge/lupe;
                         anfang:=strichstelle-bildlaenge/2; grafik end;
         {F2}       #60: begin
                         abl:=abl/lupe; bildlaenge:=bildlaenge*lupe;
                         anfang:=maxe(0,strichstelle-bildlaenge/2); grafik end;
         {Ctrl Rech}#116:schritt(min(strich+1,maxx));
         {Ctrl Link}#115:schritt(max(lrand+1,strich-1));
         {Rech}     #77: schritt(min(strich+10,maxx));
         {Link}     #75: schritt(max(lrand+1,strich-10));
         {Ctrl Home}#119:begin anfang:=0; grafik end;
         {Ctrl End} #117:begin anfang:=laenge-bildlaenge;
                         grafik end;
         {Home}     #71: schritt(lrand+1);
         {End}      #79: schritt(maxx);
         {Ins}      #82: begin
                         pwandert:=selbst^;
                         while pwandert^.bei<strichstelle do pwandert:=pwandert^.next;
                         if (stellex(pwandert^.bei)<>strich)
                           and (stellex(pwandert^.vor^.bei)<>strich) then begin
                            setlinestyle(userbitln,tstellenpattern,normwidth);
                            line(strich,oben,strich,maxy-11);
                            prein(pwandert); pwandert^.bei:=strichstelle end;
                         end;
         {Del}      #83: begin
                         pwandert:=selbst^;
                         while pwandert^.bei<strichstelle do pwandert:=pwandert^.next;
                         if pwandert^.bei<anfang+bildlaenge then begin
                            setlinestyle(userbitln,tstellenpattern,normwidth);
                            line(stellex(pwandert^.bei),oben,stellex(pwandert^.bei),maxy-11);
                            praus(pwandert) end;
                         end;
         {F5}       #63: if status=bleibt then begin
                            setcolor(0);
                            outtextxy(spalte2+buchbr,4,statustext[status]);
                            setcolor(getmaxcolor);
                            status:=neu; outtextxy(spalte2+buchbr,4,statustext[status]);
                            wandert:=block^;
                            while wandert^.bis<strichstelle do wandert:=wandert^.next;
                            if wandert^.von>strichstelle then begin
                               rein(wandert);
                               wandert^.von:=strichstelle; wandert^.bis:=strichstelle;
                               linie(strich,strich);
                               end;
                            end;
         {F7}       #65: if status=bleibt then begin
                            wandert:=block^;
                            while wandert^.bis<strichstelle do wandert:=wandert^.next;
                            if wandert^.von<=strichstelle then begin
                               linie(max(lrand+1,stellex(wandert^.von)),
                               min(maxx,stellex(wandert^.bis)));
                               raus(wandert) end;
                            end;
         {Ctrl F8} #101: if status=bleibt then begin
                            wandert:=block^;
                            while wandert^.next<>nil do begin
                               linie(max(lrand+1,stellex(wandert^.von)),
                               min(maxx,stellex(wandert^.bis)));
                               raus(wandert) end;
                            end;
         {F6}       #64: if status=neu then begin
                            while strichstelle>=wandert^.next^.von do begin
                               linie(max(lrand+1,stellex(wandert^.bis)+1),
                                 max(lrand+1,stellex(wandert^.next^.von)-1));
                               wandert^.next^.von:=wandert^.von;
                               raus(wandert) end;
                            while strichstelle<=wandert^.vor^.bis do begin
                               linie(min(maxx,stellex(wandert^.vor^.bis)+1),
                                 min(maxx,stellex(wandert^.von)-1));
                               wandert^.von:=wandert^.vor^.von;
                               wandert:=wandert^.vor; raus(wandert) end;
                            if wandert^.von>strichstelle then begin
                               linie(strich,min(stellex(wandert^.von)-1,maxx));
                               wandert^.von:=strichstelle end;
                            if wandert^.bis<strichstelle then begin
                               linie(max(lrand+1,stellex(wandert^.bis)+1),strich);
                               wandert^.bis:=strichstelle end;
                            setcolor(0);
                            outtextxy(spalte2+buchbr,4,statustext[status]);
                            setcolor(getmaxcolor);
                            status:=bleibt; outtextxy(spalte2+buchbr,4,statustext[status]);
                            end;
         {Ctrl F9}  #102:begin kontrolle(aktfile,upcase(readkey)); grafik end;
         {Ctrl PrtS}#114:begin
                         fenster(200,100,440,156);
                         settextjustify(righttext,centertext);
                         outtextxy(199,18,'Plotten (Grî·e 1..5 ?)');
                         gr:=zahl(readkey); outtextxy(225,18,wort(gr));
                         outtextxy(199,38,'Abbruch mit <ESC>');
                         if gr in [1..5] then
                            plotnormal(anfang,bildlaenge,kanaele,datzw,anfang,gr,
                                        name,ko.kennung);
                         setviewport(0,0,maxx,maxy,clipon); grafik;
                         end;
         {Ctrl F10} #103:begin
                         pwandert:=selbst^;
                         setlinestyle(userbitln,tstellenpattern,normwidth);
                         while pwandert^.next<>nil do begin
                            if (pwandert^.bei<anfang+bildlaenge)
                             and (pwandert^.bei>=anfang) then
                               line(stellex(pwandert^.bei),oben,
                                    stellex(pwandert^.bei),maxy-11);
                            praus(pwandert) end;
                         end;
                    end;
       {Ret} #13:begin anfang:=strichstelle-bildlaenge/2; grafik end;
       {Esc} #27:begin closegraph; schliesse; richtung:=vow; exit end;
             ' ':begin
                 spannstatus:=not spannstatus;
                 if spannstatus then richtung:=mit end;
             end;
      while keypressed do tb:=readkey;
   until false;
   end;
end;

procedure averagen;
label ende;
const von:messwert=0;       bis:messwert=500;
      akttrind:char='A';
var   vonr,bisr,laenge,platz:longint;
      nr:byte;              i,j,tp,gesamt:longint;
      chpuff:char;
begin
ueberschrift('Averagen','Info',farbe2);
triggeruebersicht;
gotoxy(1,18); zwischen('Dialog',farbe2); writeln;
chpuff:=upcase(readchar('Triggerliste',akttrind));
if not (chpuff in ['A'..listmax]) then begin
   fehler('UnzulÑssige Triggerliste'); warte; exit end;
akttrind:=chpuff; gesamt:=tliste[akttrind]^.triggsum;
if gesamt=0 then begin writeln('Kein Triggerpunkt.'); warte; exit end;
clrscr; kanaele.lesen(5);
window(1,22,80,25); clreol;
writeln('Gesamtzeit zum Mitteln maximal ',zeit(maxanzahl)-1,' ms.');
von:=messw(readint('Anfangszeitpunkt relativ zum Triggerpunkt [ms]',zeit(von)));
vonr:=round(von);
bis:=messw(readint('Endzeitpunkt relativ zum Triggerpunkt [ms]',zeit(bis)));
bisr:=round(bis);
laenge:=bisr-vonr; platz:=(laenge+1)*sizeof(wert);
if (laenge>maxanzahl) or (laenge<=0) then begin
   fehler('UnzulÑssiges Zeitfenster'); warte; exit end;
for i:=0 to maxkanal do if i in kanaele.dabei then begin
   if maxavail<platz then begin
      fehler('Speicherplatz reicht nicht aus.');
      writeln(lfcr,'Abhilfe: Weniger KanÑle oder kÅrzere Auswertzeit');
      for i:=i-1 downto 0 do if i in kanaele.dabei then freemem(mittel[i],platz);
      warte; exit end;
   getmem(mittel[i],platz);
   fillchar(mittel[i]^,platz,0) end;
window(1,3,80,25); clrscr;
with tliste[akttrind]^ do begin
   gotoxy(1,3);
   writeln('Trigger-Liste  : ',akttrind,' - ',name,
      lfcr,'File-Anzahl    : ',fileanz,
      lfcr,'Kanal-Nr.      : ',kanaele.ausgabe,
      lfcr,'Trigger-Stellen: ',gesamt,
      lfcr,'Auswertung     : von ',zeit(von),' ms bis ',zeit(bis),' ms');
   end;
gotoxy(1,18); zwischen('Dialog',farbe2);
gotoxy(1,23); write('Abbruch mit <Esc>'); gotoxy(1,20);
for nr:=1 to filenr do with tliste[akttrind]^.fil[nr] do
  if automda then begin
   write(#13); clreol;
   write('Mitteln von ',liste[nr].name,', bei Triggerpunkt Nr.:');
   oeffnen(nr);
   for tp:=1 to automn do begin
      for j:=0 to maxkanal do if j in kanaele.dabei then for i:=0 to laenge do
         mittel[j]^[i]:=mittel[j]^[i]+dat(zwi(autom^[tp]+vonr+i),j);
      write(tp:5,#8#8#8#8#8);
      if keypressed and (readkey=#27) then goto ende;
      end;
   schliesse;
   end;
for j:=0 to maxkanal do if j in kanaele.dabei then
   for i:=0 to laenge do mittel[j]^[i]:=mittel[j]^[i]/gesamt;
piep; grafikmittel(mittel,kanaele,vonr,bisr,akttrind,gesamt);
ende: for i:=0 to maxkanal do if i in kanaele.dabei then freemem(mittel[i],platz);
end;

procedure phasenaveragen;
label ende;
const vonr=0;
      minabst:messwert=0;             maxabst:messwert=0;
      akttrind:char='A';
var   weis:triggerweiser;
      bisr,laenge,platz:longint;
      nr:byte;      i,j,tp:longint;
      abst:messwert;
      faktor:extended;
      chpuff:char;
begin
with weis do begin
   ueberschrift('Averagen mit Phasenangleichung','Info',farbe2);
   triggeruebersicht;
   gotoxy(1,18); zwischen('Dialog',farbe2); writeln;
   chpuff:=upcase(readchar('Triggerliste',akttrind));
   if not (chpuff in ['A'..listmax]) then begin
      fehler('UnzulÑssige Triggerliste'); warte; exit end;
   akttrind:=chpuff;
   clrscr; kanaele.lesen(5);
   window(1,22,80,25); clreol;
   writeln('Mittlere Periodendauer maximal ',zeit(maxanzahl)-1,' ms.');
   minabst:=messw(readint(
      'Minimale Periodendauer [ms]',zeit(minabst)));
   maxabst:=messw(readint(
      'Maximale Periodendauer [ms]',zeit(maxabst)));
   zaehlen(tliste[akttrind]^,minabst,maxabst);
   if gesamt=0 then begin writeln('Keine Triggeerstelle'); piep; warte; exit end;
   writeln('Gesamtzeit zum Mitteln maximal ',zeit(maxanzahl)-1,' ms.');
   bisr:=round(mittelabstand);
   if bisr>maxanzahl then begin
      fehler('Zeitspanne zu gross.'); warte; exit end;
   laenge:=bisr-vonr; platz:=(laenge+1)*sizeof(wert);
   for i:=0 to maxkanal do if i in kanaele.dabei then begin
      if maxavail<platz then begin
         fehler('Speicherplatz reicht nicht aus.');
         writeln(lfcr,'Abhilfe: Weniger KanÑle oder kÅrzere Auswertzeit');
         for i:=i-1 downto 0 do if i in kanaele.dabei then freemem(mittel[i],platz);
         warte; exit end;
      getmem(mittel[i],platz);
      fillchar(mittel[i]^,platz,0) end;
   window(1,3,80,25); clrscr;
   with tliste[akttrind]^ do begin
      gotoxy(1,3);
      writeln('Trigger-Liste  : ',akttrind,' - ',name,
         lfcr,'File-Anzahl    : ',fileanz,
         lfcr,'Kanal-Nr.      : ',kanaele.ausgabe,
         lfcr,'Trigger-Stellen: ',gesamt,
         lfcr,'Auswertung     : min. ',zeit(minabst),' ms, max. ',zeit(maxabst),' ms',
         lfcr,'Mittlerer Abst.: ',zeit(mittelabstand));
      end;
   gotoxy(1,18); zwischen('Dialog',farbe2);
   gotoxy(1,23); write('Abbruch mit <Esc>'); gotoxy(1,20);
   for nr:=1 to filenr do with weisliste[nr], tliste[akttrind]^.fil[nr] do
     if automda then begin
      write(#13); clreol;
      write('Mitteln von ',liste[nr].name,', bei Triggerpunkt Nr.:');
      oeffnen(nr);
      for tp:=1 to n do begin
         abst:=autom^[t^[tp]+1]-autom^[t^[tp]];
         faktor:=mittelabstand/abst;
         for j:=0 to maxkanal do if j in kanaele.dabei then for i:=0 to laenge do
           mittel[j]^[i]:=mittel[j]^[i]+dat(zwi(autom^[tp]+(vonr+i)/faktor),j);
         write(tp:5,#8#8#8#8#8);
         if keypressed and (readkey=#27) then goto ende;
         end;
      schliesse;
      end;
   for j:=0 to maxkanal do if j in kanaele.dabei then
      for i:=0 to laenge do mittel[j]^[i]:=mittel[j]^[i]/gesamt;
   frei;
   piep; grafikmittel(mittel,kanaele,vonr,bisr,akttrind,gesamt);
   ende: for i:=0 to maxkanal do if i in kanaele.dabei then freemem(mittel[i],platz);
   end;
end;

procedure psthistogramm;
const ref:char='A';     obj:char='B';
      von:messwert=0;   bis:messwert=500;
      diffn:word=100;
      ybereich:extended=1;
var   nr:byte;
      tpr,tpo:longint; tpol,tpor,dum:word;
      diff:^zeitfeld;
      klasse,um:extended;
      d,gesamt:longint;
      charpuff:char; wordpuff:word;
begin
ueberschrift('Zeit-Histogramme','Info',farbe2);
triggeruebersicht;
gotoxy(1,10); zwischen('Dialog',farbe2);
window(1,14,80,25);
charpuff:=upcase(readchar('Referenz-Triggerliste',ref));
if not (charpuff in ['A'..listmax]) then begin
   fehler('UnzulÑssige Triggerliste'); warte; exit end;
ref:=charpuff;
gesamt:=tliste[ref]^.triggsum;
if gesamt=0 then begin write('Keine Triggerstelle'); pieps; warte; exit end;
charpuff:=upcase(readchar('Ereignis-Triggerliste',obj));
if not (charpuff in ['A'..listmax]) then begin
   fehler('UnzulÑssige Triggerliste'); warte; exit end;
obj:=charpuff;
if ref=obj then von:=0
           else
   von:=messw(readint('FrÅhester Zeitpunkt relativ zur Trigger-Stelle [ms]',zeit(von)));
bis:=messw(readint('SpÑtester Zeitpunkt relativ zur Trigger-Stelle [ms]',zeit(bis)));
wordpuff:=readint('Anzahl der Klassen max.'+wort(maxzeit),diffn);
if (diffn<=0) or (diffn>maxzeit) then begin
   fehler('UnzulÑssige Klassenanzahl'); warte; exit end;
diffn:=wordpuff;
klasse:=(bis-von)/diffn;
ybereich:=readext('Skalierung y-Achse',ybereich,4,2);
new(diff); fillchar(diff^,sizeof(zeitfeld),0);
um:=1/gesamt;
if ref=obj then begin
   for nr:=1 to filenr do
    with tliste[ref]^.fil[nr] do
       for tpr:=1 to automn-1 do begin
           d:=round(autom^[tpr+1]-autom^[tpr]);
           if d<bis then incex(diff^[trunc(d/klasse)+1],um) end;
   end     else begin
   for nr:=1 to filenr do if tliste[ref]^.fil[nr].automda then
     with tliste[obj]^, fil[nr] do begin
      write(#13); clreol;
      write('ZÑhlen bei ',liste[nr].name,', Trigger-Stelle Nr:');
      for tpr:=1 to tliste[ref]^.fil[nr].automn do begin
         fil[nr].such(0,automn+1,tliste[ref]^.fil[nr].autom^[tpr]+von,dum,tpol);
         fil[nr].such(dum,automn+1,tliste[ref]^.fil[nr].autom^[tpr]+bis,tpor,dum);
         for tpo:=tpol to tpor do
            incex(diff^[trunc((autom^[tpo]-tliste[ref]^.fil[nr].autom^[tpr]-von)/klasse)+1],um);
         write(tpr:6,#8#8#8#8#8#8);
         end;
      end;
   end;
piep;
with liste[tliste[ref]^.erstfile] do
   grafikzeit(diff^,diffn,round(von),round(bis),gesamt,ybereich,name,ko.kennung);
dispose(diff);
end;

procedure phasenhistogramm;
const ref:char='A';     obj:char='B';
      von=0;
      minabst:messwert=0;             maxabst:messwert=0;
      diffn:word=100;
      ybereich:extended=1;
var   weis:triggerweiser;
      nr:byte;
      tpr,tpo,tpol,tpor,dum:word;
      diff:^zeitfeld;
      klasse,um,faktor:extended;
      bis,abst:messwert;
      charpuff:char;
begin
with weis do begin
   ueberschrift('Phasen-Histogramm','Info',farbe2);
   triggeruebersicht;
   gotoxy(1,10); zwischen('Dialog',farbe2);
   window(1,14,80,25);
   charpuff:=upcase(readchar('Referenz-Triggerliste',ref));
   if not (charpuff in ['A'..listmax]) then begin
      fehler('UnzulÑssige Triggerliste'); warte; exit end;
   ref:=charpuff;
   minabst:=messw(readint(
      'Minimale Periodendauer [ms]',zeit(minabst)));
   maxabst:=messw(readint(
      'Maximale Periodendauer [ms]',zeit(maxabst)));
   zaehlen(tliste[ref]^,minabst,maxabst);
   if gesamt=0 then begin write('Keine Triggerstelle'); pieps; warte; exit end;
   charpuff:=upcase(readchar('Ereignis-Triggerliste',obj));
   if not (charpuff in ['A'..listmax]) then begin
      fehler('UnzulÑssige Triggerliste'); warte; exit end;
   bis:=mittelabstand;
   diffn:=readint('Anzahl der Klassen max.'+wort(maxzeit),diffn);
   klasse:=(bis-von)/diffn;
   ybereich:=readext('Skalierung y-Achse',ybereich,4,2);
   new(diff); fillchar(diff^,sizeof(zeitfeld),0);
   um:=1/gesamt;
   for nr:=1 to filenr do
     with tliste[ref]^.fil[nr], weisliste[nr] do if automda then begin
      write(#13); clreol;
      write('ZÑhlen bei ',liste[nr].name,', Trigger-Stelle Nr:');
      for tpr:=1 to n do begin
         abst:=autom^[t^[tpr]+1]-autom^[t^[tpr]];
         faktor:=mittelabstand/abst;
         tliste[obj]^.fil[nr].such(0,tliste[obj]^.fil[nr].automn+1,
              autom^[t^[tpr]]+von/faktor,dum,tpol);
         tliste[obj]^.fil[nr].such(dum,tliste[obj]^.fil[nr].automn+1,
              autom^[t^[tpr]]+bis/faktor,tpor,dum);
         for tpo:=tpol to tpor do
          incex(diff^[trunc((faktor*(tliste[obj]^.fil[nr].autom^[tpo]-autom^[t^[tpr]])
                -von)/klasse)+1],um);
         write(t^[tpr]:6,#8#8#8#8#8#8);
         end;
      end;
   weis.frei;
   piep;
   with liste[tliste[ref]^.erstfile] do
      grafikzeit(diff^,diffn,round(von),round(bis),gesamt,ybereich,name,ko.kennung);
   dispose(diff);
   end;
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
   if speicher.status=stok then writeln('Parameter-File gesichert!')
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
         fehler('Parameterfile kann nicht geladen werden.');
         writeln(lfcr,lfcr,speicher.status:10,speicher.errorinfo:10);
         halt end;
      end       else begin
      clrscr; writeln(lfcr);
      fehler('Parameterfile fehlerhaft.'); warte;
      end;
   speicher.done;
   exitproc:=@schluss;
   delay(2000);
   end        else delay(5000);
end;

function heapvoll (groesse:word):integer; far;
begin
if groesse=0 then exit;
exitproc:=exitsave;
closegraph; window(1,1,80,25); clrscr;
pieps; writeln(lfcr);
if groesse>memavail then write('SpeicherÅberlauf')
                    else
   if groesse>maxavail then write('Speicher zu stark fragmentiert oder voll')
                    else write('Speicherfehler');
writeln(', bitte erneut starten.',lfcr);
tulab.ausserbetrieb; sichern; halt;
end;

procedure schluss;
begin
exitproc:=exitsave;
closegraph; window(1,1,80,25); clrscr;
if erroraddr<>nil then
   writeln(lfcr,lfcr,'Sorry, Fehler-Abbruch:',lfcr)
                  else writeln('TschÅ·',lfcr);
sichern;
end;

begin
exitsave:=exitproc;
exitproc:=@schluss;
heaperror:=@heapvoll;
if paramcount>0 then sichername:=paramstr(1);
clrscr;
textcolor(cyan);
gotoxy(1,6);
writeln(lfcr,'':16,'------------------------------------------',
        lfcr,'':16,'------------   NEUROLAB ',version:4,'  ------------',
        lfcr,'':16,'------------------------------------------',
        lfcr,'':16,'------ von B. Hedwig und M. Knepper ------',
        lfcr,'':16,'--- I. Zoologisches Institut Gîttingen ---',
        lfcr,'':16,'------------------------------------------');
writeln(lfcr,lfcr);
writeln('':7,'Analyseprogramm fÅr neurobiologische Daten im Turbolab-Format');
textcolor(lightgray); gotoxy(1,20);
holen;
repeat
   ueberschrift('NEUROLAB '+version,'Info',farbe1);
   writeln('freier Speicher   :   ',memavail div 1024,' (',maxavail div 1024,') kByte');
   writeln('Konfigurationsfile:   ',sichername);
   writeln('Rahmendaten       :   KanÑle max.: ',kan,', Abtastrate max.: ',fre:4:2,' Hz');
   writeln('offene Files      :   ',filenr);
   gotoxy(1,10); zwischen('MenÅ',farbe1);
   writeln(
     lfcr,'  f...File-Manager                      t...Trigger-Manager',
     lfcr,'  k...Skalierung                        a...Averagen',
     lfcr,'  m...Filter-Manager                    p...Averagen mit Phasenangleichung',
     lfcr,'  l...Daten listen                      z...Zeit-Histogramme',
     lfcr,'  s...Daten sichten                     h...Phasen-Histogramm',
     lfcr,
     lfcr,'                                        e...Ende',lfcr);
   zwischen('Dialog',farbe1);
   window(1,24,80,25);
   if filenr=0 then begin
      case upcase(readchar('MenÅpunkt','f')) of
         'F':nlfiles.manager;
         'E':halt;
         'M','L','T','K','P','H','Z','A','S':begin
            fehler('Kein File offen: Beginne mit File-Manager!'); warte end;
         end;
      end      else begin
      case upcase(readchar('MenÅpunkt','e')) of
         'F':nlfiles.manager;
         'K':verstaerkungen;          'M':filterung;
         'L':listen;                  'S':sichten;
         'T':nltrigg.manager;         'A':averagen;
         'P':phasenaveragen;          'Z':psthistogramm;
         'H':phasenhistogramm;
         'E':begin
              write(lfcr,'Wirklich aufhîren? (J/N) ');
              if readkey in ['j','J','y','Y'] then halt end;
         '?':begin window(1,1,80,25); clrscr; zeigertest end;
         end;
      end;
until false
end.