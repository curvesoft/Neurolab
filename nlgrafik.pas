{ Borland-Pascal 7.0 }

unit  nlgrafik;

{$IFDEF MSDOS}
{$A+,B-,E+,F-,G-,I-,N+,O-,P+,T+,V-,X-}
{$ELSE}
{$A+,B-,E+,F-,G+,I-,N+,P+,T+,V-,X-}
{$ENDIF}

interface

uses  crt,                graph,              daff,wavpcm,tulab42, nlrahmen,
      dos,                grafik,             tlfilter,            nltrigg,
      bequem,             plotter,            tlfiles;

const kleinschr='SI0.14,0.22;';  relativschr='SR1,2.6;';
      ganz=maxsample-minsample;
      lrand=50;

      maxanzahl=(1 shl 16) div sizeof(wert) -2;
      maxfeld=6000;

type  mittelfeld  = array[0..maxanzahl] of wert;
      mittelliste = array[0..maxkanal] of ^mittelfeld;
      bildfeld    = array[0..maxfeld+1] of extended;

      grafikdarstellung=object    {Allgemeine Grafiken}
         filename,filekennung,kommentar:string80; fileanzahl:byte;
         line1,line2:word;
         abbruch:boolean;
         procedure berechnen; virtual;
         procedure bild; virtual;
         procedure plot (gr:byte); virtual;
         procedure filewrite (var outfile:text); virtual;
         procedure plotstart;
         procedure aufbauen (l1,l2:word; var fina,fike:string80; fian:byte);
         end;

      grafikxy=object (grafikdarstellung) {x-y-Grafik}
         x,y:byte;
         procedure bild; virtual;
         procedure plot (gr:byte); virtual;
         procedure filewrite (var outfile:text); virtual;
         constructor aufbauen(xk,yk:byte);
         end;

      grafikamplitude=object (grafikdarstellung) {Amplituden-Histogramm}
         diff:^bildfeld;
         diffn:word;
         klasse:extended;
         ybereich:extended;
         xkanal:byte;
         minsamp,maxsamp:sample;
         ref:char;
         procedure bild; virtual;
         procedure plot (gr:byte); virtual;
         procedure filewrite (var outfile:text); virtual;
         procedure aufbauen (xk:byte; rtl:char; dfn:word; ybe:extended; mins,maxs:sample);
         end;

      yliste=array[1..maxkanal+1] of word;
      grafikkanaele=object (grafikdarstellung)
         anfang,dauer:messwert;
         kanaele:kanalmenge;
         stauchung,faktor:extended;
         breite,oben:word;
         y0:yliste;
         procedure bild; virtual;
         procedure plot (gr:byte); virtual;
         end;

      grafikkurve=object (grafikkanaele)
         function daten (stelle:messwert; kanal:byte):sample; virtual;
         procedure bild; virtual;
         procedure plot (gr:byte); virtual;
         end;

      statustyp=(bleibt,neu);
      menue=procedure (aktfile:byte);
      grafikdaten=object (grafikkanaele) {Sichten}
         aktfile:byte;
         ablenkung:single;
         datanf:messwert;
         strichstelle,strichst1:messwert;  strich,strich1:word;
         status:statustyp;
         spalte1,spalte2:word;
         spannstatus,strich1da:boolean;
         zei1a,zei2a,zei3a,zei4a:string20;
         function stellex (stelle:messwert):longint;
         function xstelle (x:integer):messwert;
         procedure bild; virtual;
         procedure linie (x1,x2:word);
         procedure plot (gr:byte); virtual;
         constructor aufbauen (afi:byte; var kane:kanalmenge;
                               anf:messwert; abl:single; men:menue);
         end;

      grafiksuperposition=object (grafikkanaele) {Superpositionsgrafik}
         tl:char;
         beginn,ende:messwert;
         procedure bild; virtual;
         procedure plot (gr:byte); virtual;
         constructor aufbauen(var kan:kanalmenge;
                              von,bis:messwert; trl:char);
         end;

      grafikmittel=object (grafikkurve) {Grafik vom Averagen}
         mittel:mittelliste;
         tpgesamt:longint;
         tl:char;
         rdauer:longint;
         function daten (stelle:messwert; kanal:byte):sample; virtual;
         procedure bild; virtual;
         procedure plot (gr:byte); virtual;
         procedure filewrite (var outfile:text); virtual;
         procedure aufbauen(var kan:kanalmenge; anf,dau:messwert;
                            trl:char; trp:longint);
         end;

      grafikintervallroh=object (grafikdarstellung) {Intervalldaten-Grafik}
         diff:^bildfeld;
         diffn:word;
         ref:char;
         ybereich:extended;
         procedure bild; virtual;
         procedure plot (gr:byte); virtual;
         procedure aufbauen (rtl:char; dfn:word; ybe:extended);
         end;

      grafikintervall=object (grafikintervallroh) {... mit Zeitachse}
         anfang,schluss,dauer:messwert;
         klasse:extended;
         procedure bild; virtual;
         procedure plot (gr:byte); virtual;
         procedure filewrite (var outfile:text); virtual;
         procedure aufbauen (rtl:char; anf,sch:messwert;
                             dfn:word; ybe:extended);
         end;

      grafikphasenintervall=object (grafikintervallroh) {... mit Phasenachse}
         anfphase,dauerphase:extended;
         klasse:extended;
         diffnganz:longint;
         procedure bild; virtual;
         procedure plot (gr:byte); virtual;
         procedure filewrite (var outfile:text); virtual;
         procedure aufbauen (rtl:char; anfph,dauph:extended;
                             dfn:word; ybe:extended);
         end;

const statustext:array[statustyp] of string20=(' -','Block selection');

function kon (hoehe:wert; k:byte):wert;
function rekon (hoehe:wert; k:byte):wert;

implementation

type  skalafeld=array[1..10] of messwert;
      punkte=record p1x,p1y,p2x,p2y:word end;

const tstellenmuster:set of 0..15=[0,4,8,12]; strichfarbe=white;
      bildfarbe=green;
      ende:boolean=false;

      p:array[1..5] of punkte=((p1x:3000; p1y:3000; p2x:6000; p2y:5000),
                               (p1x:2600; p1y:2600; p2x:6600; p2y:5200),
                               (p1x:2300; p1y:2300; p2x:7530; p2y:5300),
                               (p1x:2000; p1y:2000; p2x:8660; p2y:6000),
                               (p1x:1150; p1y:380;  p2x:9750; p2y:6380));

var   tstellenpattern:word absolute tstellenmuster;

function kon (hoehe:wert; k:byte):wert;
begin
if belegungsliste[k].negativ then kon:=hoehe
                             else kon:=hoehe*2-sampleoffset;
end;

function rekon (hoehe:wert; k:byte):wert;
begin
if belegungsliste[k].negativ then rekon:=hoehe
                             else rekon:=(hoehe+sampleoffset)/2;
end;

{ grafikdarstellung }

procedure grafikdarstellung.bild;
begin
setcolor(min(bildfarbe,getmaxcolor)); settextjustify(lefttext,centertext);
if fileanzahl>0 then
   outtextxy(0,line1,'File:'+filename+' (total:'+wort(fileanzahl)+')');
outtextxy(0,line2,filekennung);
settextjustify(centertext,centertext);
end;

procedure grafikdarstellung.plot (gr:byte);
begin end;

procedure grafikdarstellung.filewrite (var outfile:text);
begin
fehler('Sorry, no file output available.');
warte;
end;

procedure grafikdarstellung.berechnen;
begin
end;

procedure grafikdarstellung.plotstart;
const   g:integer=4;
var     bildzeigen:boolean;
begin
closegraph;
ueberschrift(false,'Graphic Output','Info',farbe3);
writeln('Output format: 0 = ASCII-List',lfcr,
        '               1 = HP-GL (Plotter)',lfcr,
        '               2 = PCL 5 (HP-LJ III)');
writeln('Output size  : 1..5');
writeln('Device / file: PRN, LPT1, LPT2, COM1, COM2, NUL, <filename>');
writeln;
zwischen('Dialogue',farbe3);
byte(plotformat):=
  zahl(readchar('Format               ',chr(ord(plotformat)+ord('0'))));
if plotformat<>ascii then g:=readint('Size                 ',g);
repeat
   repeat
      assignplt(readstring('Output device / file ',plotterdevice));
   until devicetest(plotterdevice) or not fileschonda(plotterdevice)
      or (upcase(readchar('Overwrite? (Y/N)     ','N'))='Y');
   rewrite(plt);
until not lesefehler;
writeln;
ende:=upcase(readchar('Return to menue after plot? (Y/N)','Y'))='Y';
kommentar:=readstring('Plot comment','');
if upcase(readchar('Start plot? (Y/N)','Y'))='Y' then begin
   writeln(lfcr,'>> Abort: Escape <<');
   case plotformat of
      hpgl,pcl5 : if g in [1..5] then begin plotterin; plot(g); plotterunin end;
      ascii : begin filewrite(plt);  close(plt) end;
      end;
   end;
opengraph;
if not ende then bild
end;

procedure grafikdarstellung.aufbauen (l1,l2:word; var fina,fike:string80;
                                      fian:byte);
var   taste:char;
      i:longint;
begin
filename:=fina; filekennung:=fike; fileanzahl:=fian;
line1:=l1; line2:=l2;
abbruch:=false;
berechnen;
if abbruch then exit;
opengraph;
cleardevice; graphdefaults;
bild; piep;
repeat
   while keypressed do taste:=readkey;
   case readkey of
      #0:case readkey of
        #114:plotstart;
        end;
      'P':plotstart;
      #27:begin closegraph; exit end;
      end;
until ende;
closegraph; ende:=false;
end;

function buendig (schrift:string20):string20;
const  leer:string20='     ';
var    n:byte absolute leer;
begin
if length(schrift)<5 then n:=5-length(schrift) else n:=0;
buendig:=leer+schrift;
end;

type skalasorte=(zeitskala,phasenskala);

procedure skala (const anfang,dauer:extended; var anz:byte;
                 var werte:skalafeld; sorte:skalasorte);
const anteil=0.85;
var   n,i,step:byte; n10,zeitpa,zeitpe:longint;
      fre:extended;
begin
case sorte of
   zeitskala   : fre:=tlfiles.fre;
   phasenskala : fre:=1;
   end;
n:=max(trunc(log(anteil*dauer/fre*1000))-1,0);
n10:=pot(n);
zeitpa:=round(anfang/fre*1000);
if (zeitpa mod n10 <>0) and (zeitpa>0) then zeitpa:=zeitpa div n10+1
                                       else zeitpa:=zeitpa div n10;
zeitpe:=round((anfang+anteil*dauer)/fre*1000) div n10;
case zeitpe-zeitpa+1 of
   0      :begin anz:=0; exit end;
   1..6   :step:=1;          7..14  :step:=2;
   15..30 :step:=5;          31..60 :step:=10;
   61..100:step:=20;         else    step:=40;
   end;
if (zeitpa mod step <>0) and (zeitpa>0) then zeitpa:=(zeitpa div step+1)*step
                                        else zeitpa:=(zeitpa div step)*step;
if (zeitpe mod step <>0) and (zeitpe<0) then zeitpe:=(zeitpe div step+1)*step
                                        else zeitpe:=(zeitpe div step)*step;
anz:=(zeitpe-zeitpa) div step+1;
for i:=1 to anz do werte[i]:=(zeitpa+(i-1)*step)*n10*fre/1000;
end;

{ grafikxy }

constructor grafikxy.aufbauen(xk,yk:byte);
const leer:string[1]='';
begin
x:=xk; y:=yk;
grafikdarstellung.aufbauen(14,24,leer,liste[filenr].ko.kennung,0);
end;

procedure grafikxy.bild;
const urand=26; orand=32;
var   i,j,m,xz,tzaehler:longint;
      nr,znr,x0,y0,xp,yp:word;
      faktorx,faktory:extended;
      wandert:listenzeiger;
      td:triggerdaten;
      xyl:byte;
begin
inherited bild;
faktory:=(getmaxy-urand-orand)/maxsample/2;
y0:=getmaxy-urand-(getmaxy-urand-orand) div 2;
faktorx:=(getmaxx-lrand)/maxsample/2;
x0:=lrand+(getmaxx-lrand) div 2;
setcolor(min(bildfarbe,getmaxcolor));
setwritemode(copyput); setlinestyle(solidln,0,normwidth);
line(lrand,orand,lrand,getmaxy-urand);
line(lrand,getmaxy-urand,getmaxx,getmaxy-urand);
with belegungsliste[y] do begin
   setlinestyle(solidln,0,normwidth);
   for xz:=0 to 2 do begin
      moveto(lrand,getmaxy-urand-round(xz/2*(getmaxy-urand-orand)));
      linerel(4,0) end;
   setlinestyle(dashedln,0,normwidth);
   moveto(lrand,getmaxy-urand-(getmaxy-urand-orand) div 2);
   linerel(getmaxx-lrand,0);
   settextstyle(defaultfont,vertdir,1);
   settextjustify(centertext,centertext);
   outtextxy(4,getmaxy-urand-(getmaxy-urand-orand) div 2,schriftliste[y]);
   settextstyle(defaultfont,horizdir,1);
   settextjustify(lefttext,toptext);
   outtextxy(14,orand+22,einhwort);
   settextjustify(righttext,toptext);
   outtextxy(lrand-1,orand+1,wort(spannung(rekon(maxsample,y),y)));
   settextjustify(righttext,bottomtext);
   outtextxy(lrand-1,getmaxy-urand,wort(spannung(rekon(minsample,y),y)));
   settextjustify(righttext,centertext);
   outtextxy(lrand-1,y0,wort(spannung(rekon(0,y),y)));
   end;
with belegungsliste[x] do begin
   setlinestyle(solidln,0,normwidth);
   for xz:=0 to 2 do begin
      moveto(lrand+round(xz/2*(getmaxx-lrand)),getmaxy-urand);
      linerel(0,-4) end;
   setlinestyle(dashedln,0,normwidth);
   moveto(lrand+(getmaxx-lrand) div 2,getmaxy-urand);
   linerel(0,-getmaxy+urand+orand);
   settextstyle(defaultfont,horizdir,1);
   settextjustify(centertext,centertext);
   outtextxy(lrand+(getmaxx-lrand) div 2,getmaxy-4,schriftliste[x]);
   settextjustify(centertext,bottomtext);
   outtextxy(lrand+80,getmaxy-12,einhwort);
   settextjustify(righttext,centertext);
   outtextxy(getmaxx-1,getmaxy-urand+6,wort(spannung(rekon(maxsample,x),x)));
   settextjustify(lefttext,centertext);
   outtextxy(lrand+1,getmaxy-urand+6,wort(spannung(rekon(minsample,x),x)));
   settextjustify(centertext,centertext);
   outtextxy(x0,getmaxy-urand+6,wort(spannung(rekon(0,x),x)));
   end;
setlinestyle(solidln,0,normwidth);
setcolor(getmaxcolor);
settextjustify(lefttext,centertext);
outtextxy(0,4,'"X-Y-DIAGRAM"');
znr:=4;
for nr:=1 to filenr do with liste[nr] do
 if not (belegungsliste[x].gepunktet or belegungsliste[y].gepunktet) then begin
   wandert:=block^;
   oeffnen(nr);
   while wandert^.next<>nil do begin
      if znr<orand then begin
         outtextxy(getmaxx div 2,znr,namen+namee+': '+wort(zeit(wandert^.von))
                          +' ms  - '+wort(zeit(wandert^.bis))+' ms');
         inc(znr,10);
         end       else outtextxy(getmaxx-25,((orand-4) div 10)*10+4,'...');
      moveto(x0+round(kon(dat(zwi(trunc(wandert^.von+1)),x),x)*faktorx),
             y0-round(kon(dat(zwi(trunc(wandert^.von+1)),y),y)*faktory));
      for tzaehler:=trunc(wandert^.von+1)+1 to trunc(wandert^.bis) do
         lineto(x0+round(kon(dat(zwi(tzaehler),x),x)*faktorx),
                y0-round(kon(dat(zwi(tzaehler),y),y)*faktory));
      wandert:=wandert^.next;
      end;
   schliesse;
   end                                                               else begin
   if belegungsliste[x].gepunktet and belegungsliste[y].gepunktet then exit;
   if belegungsliste[x].gepunktet then xyl:=x else xyl:=y;
   oeffnen(nr);
   td:=tliste[belegungsliste[xyl].gepunktettl]^.fil[filenr];
   for tzaehler:=1 to td.automn do begin
      xp:=x0+round(kon(dat(zwi(td.autom^[tzaehler]),x),x)*faktorx);
      yp:=y0-round(kon(dat(zwi(td.autom^[tzaehler]),y),y)*faktory);
      for m:=-1 to 1 do begin
         putpixel(xp+m,yp+m,getmaxcolor);
         putpixel(xp+m,yp-m,getmaxcolor);
         end;
      end;
   schliesse;
   end;
end;

procedure grafikxy.plot (gr:byte);
const lrand=minsample-ganz/12;
      bnrmax=3;
var   tzaehler,j,bnr:longint;
      nr:word;
      wandert:listenzeiger;
      xyl:byte;
      td:triggerdaten;
begin
with p[gr] do write(plt,plip(p1x,p1x,p2y,p2y));
write(plt,relativschr,plsc(minsample,maxsample,minsample,maxsample));
for nr:=1 to filenr do with liste[nr] do begin
 if not (belegungsliste[x].gepunktet or belegungsliste[y].gepunktet) then begin
   wandert:=block^;
   oeffnen(nr);
   while wandert^.next<>nil do begin
      write(plt,plpa(kon(dat(zwi(trunc(wandert^.von+1)),x),x),
                     kon(dat(zwi(trunc(wandert^.von+1)),y),y)),plpd);
      for tzaehler:=trunc(wandert^.von+1)+1 to trunc(wandert^.bis) do
         write(plt,plpa(kon(dat(zwi(tzaehler),x),x),kon(dat(zwi(tzaehler),y),y)));
      write(plt,plpu);
      wandert:=wandert^.next;
      end;
   schliesse;
   end                                                               else begin
   if belegungsliste[x].gepunktet and belegungsliste[y].gepunktet then exit;
   if belegungsliste[x].gepunktet then xyl:=x else xyl:=y;
   oeffnen(nr);
   td:=tliste[belegungsliste[xyl].gepunktettl]^.fil[filenr];
   for tzaehler:=1 to td.automn do
      write(plt,plpa(kon(dat(zwi(td.autom^[tzaehler]),x),x),
            kon(dat(zwi(td.autom^[tzaehler]),y),y)),plkr);
   schliesse;
   end;
   end;
write(plt,plpu,plpa(minsample,minsample),plpd,plpa(maxsample,minsample),
          plpa(maxsample,maxsample),plpa(minsample,maxsample),
          plpa(minsample,minsample),plpu);
write(plt,plpa(lrand,maxsample),'CP0,-0.75;',
          pllb(buendig(wort(spannung(rekon(maxsample,y),y)))),
          'CP;CP0.5,-0.25;',belegungsliste[y].plot);
write(plt,plpa(lrand,0),'CP0,-0.25;',
          pllb(buendig(wort(spannung(rekon(0,y),y)))),
          plpa(lrand,minsample),'CP0,0.25;',
          pllb(buendig(wort(spannung(rekon(minsample,y),y)))));
write(plt,'TL0,1.3;',plpa(minsample,minsample),plyt,plxt);
write(plt,'TL0,0.6;');
for j:=1 to 3 do write(plt,plpa(minsample,minsample+j*ganz/4),plyt);
write(plt,'TL0,1.3;',plpa(minsample,maxsample),plyt);
write(plt,kleinschr,'CP0,3;',pllb(filename),'CP;',pllb(filekennung),'CP;',
             pllb(kommentar),relativschr);
write(plt,'TL0,0.6;');
for j:=1 to 3 do write(plt,plpa(minsample+j*ganz/4,minsample),plxt);
write(plt,'TL0,1.3;',plpa(maxsample,minsample),plxt);
write(plt,'DI0,1;',plpa(lrand,minsample),'CP0,0.5;',pllb(schriftliste[y]));
write(plt,'DI1,0;');
write(plt,plpa(maxsample,minsample),'CP-4.33,-1;',
          pllb(buendig(wort(spannung(rekon(maxsample,x),x)))),
          'CP-14,-0.5;',belegungsliste[x].plot);
write(plt,plpa(0,minsample),'CP-4.33,-1;',
          pllb(buendig(wort(spannung(rekon(0,x),x)))),
          plpa(minsample,minsample),'CP-4.33,-1;',
          pllb(buendig(wort(spannung(rekon(minsample,x),x)))));
write(plt,plpa(minsample,minsample),'CP0,-2;',pllb(schriftliste[x]));
write(plt,kleinschr,'DI0,1;',plpa(maxsample,minsample),'CP0,-2;');
bnr:=0;
for nr:=1 to filenr do with liste[nr] do begin
   wandert:=block^;
   while wandert^.next<>nil do begin
      if bnr<bnrmax then begin
         write(plt,pllb(namen+namee+': '+wort(zeit(wandert^.von))
                    +' ms  - '+wort(zeit(wandert^.bis))+' ms'),'CP;');
         inc(bnr);
         end;
      wandert:=wandert^.next;
      end;
   end;
end;

procedure grafikxy.filewrite (var outfile:text);
var   nr:byte;
      tzaehler:longint;
      wandert:listenzeiger;
begin
for nr:=1 to filenr do with liste[nr] do begin
   wandert:=block^;
   oeffnen(nr);
   while wandert^.next<>nil do begin
      for tzaehler:=trunc(wandert^.von+1) to trunc(wandert^.bis) do
         writeln(outfile,extspannung(dat(zwi(tzaehler),x),x):15:3,
                         extspannung(dat(zwi(tzaehler),y),y):15:3);
      wandert:=wandert^.next;
      writeln(outfile);
      end;
   schliesse;
   end;
end;

{ bildbalken }

procedure bildbalken (const lrand:longint; var xbreite:extended;
                      const klasse:extended; var breite,diffn:word);
begin
breite:=(getmaxx-lrand-2) div diffn;
if breite=0 then begin
   breite:=1; diffn:=getmaxx-lrand-2; xbreite:=klasse*diffn end;
end;

{ grafikamplitude }

procedure grafikamplitude.aufbauen (xk:byte; rtl:char; dfn:word;
                                    ybe:extended; mins,maxs:sample);
begin
xkanal:=xk; ref:=rtl;
ybereich:=ybe; diffn:=dfn;
minsamp:=mins; maxsamp:=maxs;
klasse:=(maxs-mins)/diffn;
new(diff);
with tliste[ref]^, liste[erstfile] do
   grafikdarstellung.aufbauen(24,34,name,ko.kennung,fileanz);
dispose(diff);
end;

procedure grafikamplitude.bild;
const lrand=32;
      urand=26; orand=40;
var   enn,breite:word;
      xbreite:extended;
      i,j,xa:longint;
      faktor:extended;
      einheit:einheitstring;
begin
inherited bild;
enn:=diffn; xbreite:=maxsample-minsample;
einheit:=belegungsliste[xkanal].einhwort;
bildbalken(lrand,xbreite,klasse,breite,enn);
setcolor(min(bildfarbe,getmaxcolor)); settextjustify(centertext,centertext);
setwritemode(copyput); setlinestyle(solidln,0,normwidth);
line(lrand+2,orand,lrand+2,getmaxy-urand);
line(lrand-3,getmaxy-urand,lrand+1+enn*breite,getmaxy-urand);
settextjustify(righttext,centertext);
outtextxy(lrand-4,65,'n'); outtextxy(lrand,76,'bin'); line(8,70,lrand,70);
outtextxy(lrand,47,extwort(ybereich,4,2)); line(lrand+1,orand,lrand-3,orand);
outtextxy(lrand,getmaxy-urand-5,'0');
setlinestyle(solidln,0,normwidth);
for xa:=0 to 2 do begin
   moveto(lrand+2+round(xa/2*breite*enn),getmaxy-urand);
   linerel(0,-4) end;
setlinestyle(dashedln,0,normwidth);
if (minsamp<0) and (maxsamp>0) then begin
   if frac(-minsamp/klasse)<1e-32 then
      moveto(lrand+1+trunc(-minsamp/klasse)*breite,getmaxy-urand)
                                  else
      moveto(lrand+1+trunc(-minsamp/klasse)*breite+breite div 2,getmaxy-urand);
   linerel(0,-getmaxy+urand+orand);
   end;
setlinestyle(solidln,0,normwidth);
settextstyle(defaultfont,horizdir,1);
settextjustify(centertext,centertext);
outtextxy(lrand+2+breite*enn div 2,getmaxy-4,schriftliste[xkanal]);
settextjustify(centertext,bottomtext);
outtextxy(lrand+80,getmaxy-12,einheit);
settextjustify(righttext,centertext);
outtextxy(lrand+2+breite*enn,getmaxy-urand+6,
          wort(spannung(rekon(maxsamp,xkanal),xkanal)));
settextjustify(lefttext,centertext);
outtextxy(lrand+3,getmaxy-urand+6,wort(spannung(rekon(minsamp,xkanal),
          xkanal)));
settextjustify(centertext,centertext);
outtextxy(lrand+2+breite*enn div 2,getmaxy-urand+6,
          wort(spannung(rekon((maxsamp+minsamp)/2,xkanal),xkanal)));
setcolor(getmaxcolor);
faktor:=-(getmaxy-urand-orand)/ybereich;
for i:=1 to enn do if diff^[i]>0 then for j:=1 to breite do begin
   xa:=lrand+1+(i-1)*breite+j;
   line(xa,getmaxy-urand-1,xa,getmaxy-urand-1+round(diff^[i]*faktor));
   end;
settextjustify(lefttext,centertext);
outtextxy(0,14,'Bins þ Width: '+wort(enn)+' þ '
          +extwort(extspannung(klasse,xkanal),3,1)+' '+einheit);
end;

procedure grafikamplitude.plot (gr:byte);
var   i:word;
begin
with p[gr] do write(plt,plip(p1x,p1y,p2x,p2y));
write(plt,plsc(0,diffn,0,ybereich));
write(plt,plpa(0,diff^[1]),plpd);
for i:=1 to diffn-1 do write(plt,plpa(i,diff^[i]),plpa(i,diff^[i+1]));
write(plt,plpa(diffn,diff^[diffn]),plpa(diffn,0),plpu);
write(plt,relativschr,plpa(0,ybereich),'CP-5.5,-0.75;',
          pllb(buendig(extwort(ybereich,5,3))));
write(plt,plpa(0,ybereich),kleinschr,'CP0,3;',pllb(filename),'CP;',pllb(filekennung),'CP;',
             pllb(kommentar),relativschr);
write(plt,plpa(0,ybereich),'TL0,1.3',plyt,plpd,plpa(0,0),plyt,
          plpd,plpa(diffn,0),plpu);
write(plt,plpa(0,0),plxt,'CP-4.33,-1',
          pllb(buendig(wort(spannung(rekon(minsamp,xkanal),xkanal)))));
write(plt,plpa(diffn/2,0),plxt,'CP-4.33,-1',
          pllb(buendig(wort(spannung(rekon((maxsamp+minsamp)/2,xkanal),xkanal)))));
write(plt,plpa(diffn,0),plxt,'CP-4.33,-1',
          pllb(buendig(wort(spannung(rekon(maxsamp,xkanal),xkanal)))));
write(plt,plpa(0,0),'CP-1.5,0.25;',pllb('0'));
write(plt,plpa(0,0),'DI0,1CP0,2.5;',pllb('Occurrences [n/bin]'));
write(plt,plpa(0,ybereich/2),'TL0,0.6;',plyt);
write(plt,kleinschr,plpa(diffn,0),'DI0,1;CP0,-3;');
write(plt,pllb(' Bins          :'+wort(diffn)+' ('+extwort(extspannung(klasse,xkanal),1,1)+' '),
               belegungsliste[xkanal].plot,pllb(')'));
end;

procedure grafikamplitude.filewrite (var outfile:text);
var i:word;
    faktor:extended;
begin
faktor:=(maxsamp-minsamp)/diffn;
for i:=1 to diffn do writeln(outfile,
    extspannung(rekon(minsamp+(i-0.5)*faktor,xkanal),xkanal):12:3,diff^[i]:15:9);
end;

{ grafikkanaele }

procedure grafikkanaele.bild;
var   i,x,buch,yn:longint;
      anzahl:byte; werte:skalafeld;
      y:yliste;
      maxx,maxy:integer;
begin
maxx:=getmaxx; maxy:=getmaxy;
setcolor(min(bildfarbe,getmaxcolor));
with kanaele do begin
   setwritemode(copyput); setlinestyle(solidln,0,normwidth);
   stauchung:=dauer/(maxx-lrand);
   breite:=(maxy-40) div (2*kn);
   oben:=maxy-2*kn*breite-10;
   faktor:=-breite/maxsample;
   settextjustify(righttext,centertext);
   outtextxy(maxx,maxy-4,'t [ms]');
   line(lrand,oben,lrand,maxy-10);
   line(lrand,maxy-10,maxx,maxy-10);
   buch:=breite div 4;
   for i:=1 to kn do with belegungsliste[k[i]] do begin
      y0[i]:=round(oben+(2*(i-1)+1)*breite);
      setlinestyle(solidln,0,normwidth);
      for x:=1 to 3 do begin
         moveto(lrand,y0[i]-round((x/2-1)*breite)); linerel(4,0) end;
      setlinestyle(dashedln,0,normwidth);
      moveto(lrand,y0[i]-breite); linerel(maxx-10,0);
      if kn<=10 then begin
         settextstyle(defaultfont,vertdir,1);
         settextjustify(centertext,centertext);
         outtextxy(4,y0[i],copy(schriftliste[k[i]],1,buch));
         settextstyle(defaultfont,horizdir,1);
         settextjustify(lefttext,toptext);
         outtextxy(14,y0[i]-breite+12,einhwort);
         settextjustify(righttext,toptext);
         outtextxy(lrand-1,y0[i]-breite+1,wort(spannung(rekon(maxsample,k[i]),k[i])));
         settextjustify(righttext,bottomtext);
         outtextxy(lrand-1,y0[i]+breite,wort(spannung(rekon(minsample,k[i]),k[i])));
         if kn<=6 then begin
            settextjustify(righttext,centertext);
            outtextxy(lrand-1,y0[i],wort(spannung(rekon(0,k[i]),k[i])));
            end;
         end
               else begin
         settextjustify(righttext,centertext);
         outtextxy(lrand-1,y0[i],copy(schriftliste[k[i]],1,6));
         end;
      end;
   settextstyle(defaultfont,horizdir,1);
   settextjustify(centertext,centertext);
   skala(anfang,dauer,anzahl,werte,zeitskala);
   for i:=1 to anzahl do begin
      x:=round((werte[i]-anfang)/stauchung+lrand+1);
      line(x,maxy-10,x,maxy-14);
      outtextxy(x,maxy-4,wort(zeit(werte[i])));
      end;
   end;
end;

procedure grafikkanaele.plot (gr:byte);
var   i,j:longint;
      anzahl:byte;          werte:skalafeld;
      rdauer:longint;
      lrand:extended;
      fleinheit:einheittyp;
begin
with kanaele do begin
   rdauer:=round(dauer);
   lrand:=-rdauer/12;
   with p[gr] do write(plt,plip(p1x,p1y,p2x,p2y));
   write(plt,relativschr,plsc(0,rdauer,0,ganz*kn));
   for i:=1 to kn do begin
     write(plt,plpa(lrand,ganz*(kn-i+1)),'CP0,-0.75;',
               pllb(buendig(wort(spannung(rekon(maxsample,k[i]),k[i])))),
               'CP;CP0.5,-0.25;',belegungsliste[k[i]].plot);
     write(plt,plpa(lrand,ganz*(kn-i)+ganz/2),'CP0,-0.25;',
               pllb(buendig(wort(spannung(rekon(0,k[i]),k[i])))),
               plpa(lrand,ganz*(kn-i)),'CP0,0.25;',
               pllb(buendig(wort(spannung(rekon(minsample,k[i]),k[i])))));
     end;
   write(plt,'TL0,1.3;',plpa(0,0),plyt);
   for i:=kn downto 1 do begin
      write(plt,'TL0,0.6;');
      for j:=1 to 3 do write(plt,plpa(0,ganz*(kn-i)+j*ganz/4),plyt);
      write(plt,'TL0,1.3;',plpa(0,ganz*(kn-i+1)),plyt);
      end;
   write(plt,kleinschr,'CP0,3;',pllb(filename),'CP;',pllb(filekennung),'CP;',
             pllb(kommentar),relativschr);
   write(plt,plpu,plpa(0,ganz*kn),plpd,plpa(0,0),plpa(rdauer,0),plpu,
             'CP-6,-1;',pllb('t [ms]'),'TL-1,0;');
   skala(anfang,rdauer,anzahl,werte,zeitskala);
   for i:=anzahl downto 1 do
      write(plt,plpa((werte[i]-anfang),0),plxt,'CP-.33,-1;',
                pllb(wort(zeit(werte[i]))));
   write(plt,'DI0,1;');
   for i:=kn downto 1 do write(plt,plpu,plpa(lrand,ganz*(kn-i)),
        'CP0,0.5;',pllb(schriftliste[k[i]]));
   end;
end;

{ grafikkurve }

function grafikkurve.daten (stelle:messwert; kanal:byte):sample;
begin end;

procedure grafikkurve.bild;
var   i,x,yn:longint;
      y:yliste;
begin
grafikkanaele.bild;
with kanaele do begin
   setwritemode(copyput);
   setlinestyle(solidln,0,normwidth); setcolor(getmaxcolor);
   for i:=1 to kn do
     y[i]:=y0[i]+round(kon(daten(0,k[i]),k[i])*faktor);
   for x:=lrand+1 to getmaxx-1 do
    for i:=1 to kn do begin
      yn:=y0[i]+round(kon(daten((x-lrand-1)*stauchung,k[i]),k[i])*faktor);
      line(x,y[i],x+1,yn);
      y[i]:=yn end;
   end;
end;

procedure grafikkurve.plot (gr:byte);
var   i,j:longint;
      rdauer:longint;
begin
grafikkanaele.plot(gr);
rdauer:=round(dauer);
write(plt,kleinschr,'DI1,0;',plpa(rdauer,ganz*kanaele.kn),'CP-6,2;');
write(plt,pllb('Start: '+extwort(extzeit(anfang),1,3)+' ms'),'CP;CP-6,0;');
write(plt,pllb('End  : '+extwort(extzeit(anfang+dauer),1,3)+' ms'));
with kanaele do begin
   for i:=kn downto 1 do begin
     write(plt,plpa(0,ganz*(kn-i+0.5)+kon(daten(0,k[i]),k[i])),
               plpd);
     for j:=0 to rdauer do begin
       write(plt,plpa(j,ganz*(kn-i+0.5)+kon(daten(j,k[i]),k[i])));
        if keypressed and (readkey=#27) then begin abbruch:=true; exit end;
        end;
     write(plt,plpu);
     end;
   end;
end;

{ grafiksuperposition }

procedure grafiksuperposition.bild;
var   maxx,maxy:integer;
      i,j,l,m,x:longint;
      y:yliste;
      gepunktetmenge:set of 0..31;
      tpo,tpol,tpor,dum:word;
      yn:word;
      zw:extended;
begin
maxx:=getmaxx; maxy:=getmaxy;
richtung:=vow;
cleardevice;
grafikdarstellung.bild;
grafikkanaele.bild;
setcolor(getmaxcolor); settextjustify(righttext,centertext);
outtextxy(getmaxx,24,'From '+wort(zeit(beginn))+' ms to '
                     +wort(zeit(ende))+' ms');
with kanaele do begin
   setwritemode(copyput);
   setlinestyle(solidln,0,normwidth); setcolor(getmaxcolor);
   gepunktetmenge:=[];
   for i:=1 to kn do if belegungsliste[k[i]].gepunktet
      then gepunktetmenge:=gepunktetmenge+[k[i]];
   for l:=1 to filenr do with tliste[tl]^.fil[l] do begin
      oeffnen(l);
      for j:=1 to automn do begin
         if keypressed then if readkey=#27 then begin schliesse; exit end;
         zw:=autom^[j]+beginn;
         for i:=1 to kn do if not (k[i] in gepunktetmenge) then
                y[i]:=y0[i]+round(kon(dat(zwi(zw),k[i]),k[i])*faktor);
         for x:=lrand+2 to maxx do
          for i:=1 to kn do if not (k[i] in gepunktetmenge) then begin
            yn:=y0[i]+
                round(kon(dat(zwi((x-lrand-1)*stauchung+zw),k[i]),k[i])*faktor);
            line(x-1,y[i],x,yn);
            y[i]:=yn end;
         for i:=1 to kn do if k[i] in gepunktetmenge then
            with tliste[belegungsliste[k[i]].gepunktettl]^.fil[l] do begin
               such(0,automn+1,zw,dum,tpol);
               such(dum,automn+1,zw+dauer,tpor,dum);
               for tpo:=tpol to tpor do begin
                  yn:=y0[i]+round(kon(dat(zwi(autom^[tpo]),k[i]),k[i])*faktor);
                  x:=round((autom^[tpo]-zw)/stauchung+lrand+1);
                  for m:=-1 to 1 do begin
                     putpixel(x+m,yn+m,getmaxcolor);
                     putpixel(x+m,yn-m,getmaxcolor);
                     end;
                  end;
               end;
         end;
      schliesse;
      end;
   end;
settextjustify(lefttext,centertext);
outtextxy(0,4,'"SUPERPOSITION"');
end;

procedure grafiksuperposition.plot (gr:byte);
var   i,j,m,l:longint;
      rdauer:longint;
      tpo,tpol,tpor,dum:word;
      zw:extended;
begin
grafikkanaele.plot(gr);
rdauer:=round(dauer);
write(plt,kleinschr,'DI1,0;',plpa(rdauer,ganz*kanaele.kn),'CP-6,2;');
write(plt,pllb('Start: '+extwort(extzeit(anfang),1,3)+' ms'),'CP;CP-6,0;');
write(plt,pllb('End  : '+extwort(extzeit(anfang+dauer),1,3)+' ms'));
with kanaele do begin
   for l:=1 to filenr do with tliste[tl]^.fil[l] do begin
      oeffnen(l);
      for m:=1 to automn do begin
         zw:=autom^[m]+beginn;
         for i:=kn downto 1 do if not belegungsliste[k[i]].gepunktet then begin
           write(plt,plpa(0,ganz*(kn-i+0.5)+kon(dat(zwi(zw),k[i]),k[i])),
                     plpd);
           for j:=1 to rdauer do begin
             write(plt,plpa(j,ganz*(kn-i+0.5)+kon(dat(zwi(zw+j),k[i]),k[i])));
              if keypressed and (readkey=#27) then begin abbruch:=true; exit end;
              end;
           write(plt,plpu);
           end                                                       else
           with tliste[belegungsliste[k[i]].gepunktettl]^.fil[1] do begin
              such(0,automn+1,zw,dum,tpol);
              such(dum,automn+1,zw+dauer,tpor,dum);
              for tpo:=tpol to tpor do
                 write(plt,plpa(autom^[tpo]-zw,ganz*(kn-i+0.5)
                   +kon(dat(zwi(autom^[tpo]),k[i]),k[i])),plkr);
              end;
         end;
      end;
   write(plt,kleinschr,'DI1,0;',plpa(rdauer,ganz*kanaele.kn),'CP-6,4;');
   write(plt,pllb('Files: '+wort(fileanzahl)),'CP;CP-6,0;');
   write(plt,pllb('T.-P.: '+wort(tliste[tl]^.triggsum)));
   write(plt,kleinschr,'DI0,1;',plpa(rdauer,0),'CP-2,-2;',
         pllb('SUPERPOSITION'));

   end;
end;

constructor grafiksuperposition.aufbauen(var kan:kanalmenge;
                                  von,bis:messwert; trl:char);
begin
kanaele:=kan; tl:=trl;
beginn:=von; ende:=bis;
anfang:=beginn;
dauer:=bis-von;
with liste[tliste[tl]^.erstfile] do
   grafikdarstellung.aufbauen(14,24,name,ko.kennung,tliste[tl]^.fileanz);
end;

{ grafikdaten }

function grafikdaten.stellex (stelle:messwert):longint;
var   zwischen:extended;
begin
zwischen:=maxe(mine((stelle-datanf)/stauchung+lrand+1,maxint),0);
stellex:=round(zwischen);
end;

function grafikdaten.xstelle (x:integer):messwert;
begin
xstelle:=datanf+(x-lrand-1)*stauchung;
end;

procedure grafikdaten.linie (x1,x2:word);
var i:word;
begin
setlinestyle(solidln,0,normwidth);
for i:=x1 to x2 do line(i,oben,i,getmaxy-11);
end;

procedure grafikdaten.bild;
var   wandert:listenzeiger; pwandert:punktzeiger;
      di:dirstr; na:namestr; ex:extstr;
      maxx,maxy:integer;
      i,j,x,yn:longint;
      y:yliste;
      gepunktetmenge:set of 0..31;
      tpo,tpol,tpor,dum:word;
begin
maxx:=getmaxx; maxy:=getmaxy;
richtung:=vow;
datanf:=anfang;
cleardevice;
grafikkanaele.bild;
with kanaele do begin
   setwritemode(copyput);
   setlinestyle(solidln,0,normwidth); setcolor(getmaxcolor);
   gepunktetmenge:=[];
   for i:=1 to kn do if belegungsliste[k[i]].gepunktet
        then gepunktetmenge:=gepunktetmenge+[k[i]]
        else y[i]:=y0[i]+round(kon(dat(zwi(datanf),k[i]),k[i])*faktor);
   for x:=lrand+1 to maxx-1 do
    for i:=1 to kn do if not (k[i] in gepunktetmenge) then begin
      yn:=y0[i]+round(kon(dat(zwi(xstelle(x)),k[i]),k[i])*faktor);
      line(x,y[i],x+1,yn);
      y[i]:=yn end;
   for i:=1 to kn do if k[i] in gepunktetmenge then
      with tliste[belegungsliste[k[i]].gepunktettl]^.fil[aktfile] do begin
         such(0,automn+1,anfang,dum,tpol);
         such(dum,automn+1,anfang+dauer,tpor,dum);
         for tpo:=tpol to tpor do begin
            yn:=y0[i]+round(kon(dat(zwi(autom^[tpo]),k[i]),k[i])*faktor);
            x:=stellex(autom^[tpo]);
            for j:=-1 to 1 do begin
               putpixel(x+j,yn+j,getmaxcolor);
               putpixel(x+j,yn-j,getmaxcolor);
               end;
            end;
      end;
   end;
setcolor(getmaxcolor); settextjustify(lefttext,centertext);
fsplit(filename,di,na,ex);
outtextxy(0,4, 'File    : '+na+ex);
outtextxy(0,14,'Duration: '+wort(zeit(liste[aktfile].laenge))+' ms');
outtextxy(0,24,'Speed   : '+extwort(ablenkung,4,2)+' mm/s');
outtextxy(spalte1,4, 'Channel :');
outtextxy(spalte1,14,'Position:');
outtextxy(spalte1,24,'Amplit. :');
outtextxy(spalte2,4, 'Mode    : '+statustext[status]);
outtextxy(spalte2,14,'Distance:');
settextjustify(centertext,centertext);
setwritemode(xorput);
wandert:=liste[aktfile].block^;
while wandert^.bis<anfang do wandert:=wandert^.next;
while wandert^.von<anfang+dauer do begin
   linie (max(lrand+1,stellex(wandert^.von)),min(maxx,stellex(wandert^.bis)));
   wandert:=wandert^.next end;
setlinestyle(userbitln,tstellenpattern,normwidth);
pwandert:=liste[aktfile].selbst^;
while pwandert^.bei<anfang do pwandert:=pwandert^.next;
while pwandert^.bei<anfang+dauer do begin
   line(stellex(pwandert^.bei),oben,stellex(pwandert^.bei),maxy-11);
   pwandert:=pwandert^.next end;
if (strichstelle>anfang) and (strichstelle<anfang+dauer) then
   strich:=stellex(strichstelle) else strich:=maxx div 2;
if (strichst1>anfang) and (strichst1<anfang+dauer) then
   strich1:=stellex(strichst1) else strich1:=lrand+1;
setcolor(min(strichfarbe,getmaxcolor));
setlinestyle(dashedln,0,normwidth);
line(strich,oben,strich,maxy-11);
if strich1da then line(strich1,oben,strich1,maxy-11);
settextjustify(lefttext,centertext);
setcolor(getmaxcolor);
if spannstatus then richtung:=mit;
zei1a:=''; zei2a:=''; zei3a:='';  zei4a:='';
end;

procedure grafikdaten.plot (gr:byte);
var   i,j:longint;
      rdauer:longint;
      tpo,tpol,tpor,dum:word;
begin
grafikkanaele.plot(gr);
rdauer:=round(dauer);
write(plt,kleinschr,'DI1,0;',plpa(rdauer,ganz*kanaele.kn),'CP-6,3;');
write(plt,pllb('Start: '+extwort(extzeit(anfang),1,3)+' ms'),'CP;CP-6,0;');
{write(plt,pllb('End  : '+extwort(extzeit(anfang+dauer),1,3)+' ms'));}
write(plt,pllb('Speed on Screen:'),'CP;CP-6,0;',pllb(extwort(ablenkung,4,2)+' mm/s'));
with kanaele do begin
   for i:=kn downto 1 do if not belegungsliste[k[i]].gepunktet then begin
     write(plt,plpa(0,ganz*(kn-i+0.5)+kon(dat(zwi(datanf),k[i]),k[i])),
               plpd);
     for j:=0 to rdauer do begin
       write(plt,plpa(j,ganz*(kn-i+0.5)+kon(dat(zwi(datanf+j),k[i]),k[i])));
        if keypressed and (readkey=#27) then begin abbruch:=true; exit end;
        end;
     write(plt,plpu);
     end                                                       else
     with tliste[belegungsliste[k[i]].gepunktettl]^.fil[aktfile] do begin
        such(0,automn+1,anfang,dum,tpol);
        such(dum,automn+1,anfang+dauer,tpor,dum);
        for tpo:=tpol to tpor do
           write(plt,plpa(autom^[tpo]-anfang,
             ganz*(kn-i+0.5)+kon(dat(zwi(autom^[tpo]),k[i]),k[i])),plkr);
        end;
   end;
end;

constructor grafikdaten.aufbauen (afi:byte; var kane:kanalmenge;
                                  anf:messwert; abl:single; men:menue);
const buchbr=80;
      lupe=4;
var   gr:byte;
      wandert:listenzeiger; pwandert:punktzeiger;
      zei1,zei2,zei3,zei4:string20;
      tb:char;
      i,kannr:byte;
      extvar:extended;
   procedure schritt (var strich:word; auf:word);
   begin
   setlinestyle(dashedln,0,normwidth);
   setcolor(min(strichfarbe,getmaxcolor));
   line(strich,oben,strich,getmaxy-11);
   strich:=auf;
   line(strich,oben,strich,getmaxy-11);
   setcolor(getmaxcolor);
   end;
begin
anfang:=anf; kanaele:=kane; ablenkung:=abl;
aktfile:=afi;
filename:=liste[aktfile].name; filekennung:=liste[aktfile].ko.kennung;
oeffnen(aktfile);
kannr:=1;
dauer:=fre/ablenkung*228; opengraph;
spalte1:=getmaxx div 3; spalte2:=(2*getmaxx) div 3;
spannstatus:=true; status:=bleibt; strich1da:=false;
strichstelle:=dauer/2; strichst1:=0;
bild;
with liste[aktfile] do
   repeat
      strichstelle:=xstelle(strich); strichst1:=xstelle(strich1);
      zei1:=wort(kannr)+' ('+schriftliste[kanaele.k[kannr]]+')';
      zei2:=extwort(extzeit(strichstelle),4,3)+' ms';
      if spannstatus then with kanaele do
         zei3:=extwort(extspannung(dat(zwi(strichstelle),k[kannr]),k[kannr]),4,2)
                      +' '+belegungsliste[k[kannr]].einhwort
                     else zei3:='';
      if strich1da then
         zei4:=extwort(extzeit(abs(strichstelle-strichst1)),4,3)+' ms'
                  else zei4:='';;
      setcolor(0); outtextxy(spalte1+buchbr,4,zei1a);
      setcolor(getmaxcolor); outtextxy(spalte1+buchbr,4,zei1);
      setcolor(0); outtextxy(spalte1+buchbr,14,zei2a);
      setcolor(getmaxcolor); outtextxy(spalte1+buchbr,14,zei2);
      setcolor(0); outtextxy(spalte1+buchbr,24,zei3a);
      setcolor(getmaxcolor); outtextxy(spalte1+buchbr,24,zei3);
      setcolor(0); outtextxy(spalte2+buchbr,14,zei4a);
      setcolor(getmaxcolor); outtextxy(spalte2+buchbr,14,zei4);
      zei1a:=zei1; zei2a:=zei2; zei3a:=zei3; zei4a:=zei4;
      case readkey of
             #0:case readkey of
         {Hoch}     #72: if kannr>1 then dec(kannr);
         {Runter}   #80: if kannr<kanaele.kn then inc(kannr);
         {PgUp}     #73: begin
                         anfang:=mine(anfang+dauer,laenge);
                         bild end;
         {Ctrl PgUp}#132:begin
                         anfang:=mine(anfang+dauer/4,laenge);
                         bild end;
         {PgDn}     #81: begin anfang:=maxe(0,anfang-dauer); bild end;
         {Ctrl PgDn}#118:begin anfang:=maxe(0,anfang-dauer/4); bild end;
         {F1}       #59: begin
                         ablenkung:=ablenkung*lupe; dauer:=dauer/lupe;
                         anfang:=strichstelle-dauer/2; bild end;
         {F2}       #60: begin
                         ablenkung:=ablenkung/lupe; dauer:=dauer*lupe;
                         anfang:=maxe(0,strichstelle-dauer/2); bild end;
         {Ctrl Rech}#116:schritt(strich,min(strich+1,getmaxx));
         {Ctrl Link}#115:schritt(strich,max(lrand+1,strich-1));
         {Rech}     #77: schritt(strich,min(strich+10,getmaxx));
         {Link}     #75: schritt(strich,max(lrand+1,strich-10));
         {Ctrl F3}  #96: begin strich1da:=not strich1da;
                         setlinestyle(dashedln,0,normwidth);
                         setcolor(min(strichfarbe,getmaxcolor));
                         line(strich1,oben,strich1,getmaxy-11);
                         setcolor(getmaxcolor);
                         end;
         {F3}       #61: if strich1da then begin
                         schritt(strich1,strich);
                         schritt(strich,strich+1) end;
         {Ctrl Home}#119:begin anfang:=0; bild end;
         {Ctrl End} #117:begin anfang:=laenge-dauer;
                         bild end;
         {Home}     #71: schritt(strich,lrand+1);
         {End}      #79: schritt(strich,getmaxx);
         {F4}       #62: if status=bleibt then begin
                            closegraph; men(aktfile); opengraph; bild end;
         {Ins}      #82: begin
                         pwandert:=selbst^;
                         while pwandert^.bei<strichstelle do pwandert:=pwandert^.next;
                         if (stellex(pwandert^.bei)<>strich)
                           and (stellex(pwandert^.vor^.bei)<>strich) then begin
                            setlinestyle(userbitln,tstellenpattern,normwidth);
                            line(strich,oben,strich,getmaxy-11);
                            prein(pwandert); pwandert^.bei:=strichstelle end;
                         end;
         {Del}      #83: begin
                         pwandert:=selbst^;
                         while pwandert^.bei<strichstelle do pwandert:=pwandert^.next;
                         if pwandert^.bei<anfang+dauer then begin
                            setlinestyle(userbitln,tstellenpattern,normwidth);
                            line(stellex(pwandert^.bei),oben,stellex(pwandert^.bei),getmaxy-11);
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
                               min(getmaxx,stellex(wandert^.bis)));
                               raus(wandert) end;
                            end;
         {Ctrl F8} #101: if status=bleibt then begin
                            wandert:=block^;
                            while wandert^.next<>nil do begin
                               linie(max(lrand+1,stellex(wandert^.von)),
                               min(getmaxx,stellex(wandert^.bis)));
                               raus(wandert) end;
                            end;
         {F6}       #64: if status=neu then begin
                            while strichstelle>=wandert^.next^.von do begin
                               linie(max(lrand+1,stellex(wandert^.bis)+1),
                                 max(lrand+1,stellex(wandert^.next^.von)-1));
                               wandert^.next^.von:=wandert^.von;
                               raus(wandert) end;
                            while strichstelle<=wandert^.vor^.bis do begin
                               linie(min(getmaxx,stellex(wandert^.vor^.bis)+1),
                                 min(getmaxx,stellex(wandert^.von)-1));
                               wandert^.von:=wandert^.vor^.von;
                               wandert:=wandert^.vor; raus(wandert) end;
                            if wandert^.von>strichstelle then begin
                               linie(strich,min(stellex(wandert^.von)-1,getmaxx));
                               wandert^.von:=strichstelle end;
                            if wandert^.bis<strichstelle then begin
                               linie(max(lrand+1,stellex(wandert^.bis)+1),strich);
                               wandert^.bis:=strichstelle end;
                            setcolor(0);
                            outtextxy(spalte2+buchbr,4,statustext[status]);
                            setcolor(getmaxcolor);
                            status:=bleibt; outtextxy(spalte2+buchbr,4,statustext[status]);
                            end;
         {Ctrl F9}  #102:begin kontrolle(aktfile,upcase(readkey)); bild end;
         {Ctrl PrtS}#114:plotstart;
         {Ctrl F10} #103:begin
                         pwandert:=selbst^;
                         setlinestyle(userbitln,tstellenpattern,normwidth);
                         while pwandert^.next<>nil do begin
                            if (pwandert^.bei<anfang+dauer)
                             and (pwandert^.bei>=anfang) then
                               line(stellex(pwandert^.bei),oben,
                                    stellex(pwandert^.bei),getmaxy-11);
                            praus(pwandert) end;
                         end;
                    end;
             'P':plotstart;
             'F':begin
                 schritt(strich,strich+1);
                 strichstelle:=xstelle(strich);
                 wandert:=block^;
                 while wandert^.von<strichstelle do wandert:=wandert^.next;
                 if wandert^.next<>nil then schritt(strich,stellex(wandert^.von)) else schritt(strich,stellex(laenge));
                 strichstelle:=xstelle(strich);
                 anfang:=strichstelle-dauer/4;
                 bild;
                 end;

       {Ret} #13:begin anfang:=strichstelle-dauer/2; bild end;
       {Esc} #27:begin closegraph; schliesse; richtung:=vow; exit end;
             ' ':begin
                 spannstatus:=not spannstatus;
                 if spannstatus then richtung:=mit end;
             end;
      while keypressed do tb:=readkey;
   until ende;
closegraph;
schliesse;
richtung:=vow;
ende:=false;
end;

{ grafikmittel }

procedure grafikmittel.aufbauen(var kan:kanalmenge; anf,dau:messwert;
                                trl:char; trp:longint);
begin
kanaele:=kan; anfang:=anf; dauer:=dau;
tl:=trl; tpgesamt:=trp;
rdauer:=round(dauer);
with liste[tliste[tl]^.erstfile] do
   grafikdarstellung.aufbauen(14,24,name,ko.kennung,tliste[tl]^.fileanz);
end;


function grafikmittel.daten (stelle:messwert; kanal:byte):sample;
begin
daten:=round(mittel[kanal]^[round(stelle)]);
end;

procedure grafikmittel.bild;
begin
grafikdarstellung.bild;
grafikkurve.bild;
setcolor(getmaxcolor); settextjustify(righttext,centertext);
outtextxy(getmaxx,4,'Trigger points: '+wort(tpgesamt));
end;

procedure grafikmittel.plot (gr:byte);
begin
grafikkurve.plot(gr);
if abbruch then exit;
write(plt,kleinschr,'DI1,0;',plpa(rdauer,ganz*kanaele.kn),'CP-6,4;');
write(plt,pllb('Files: '+wort(fileanzahl)),'CP;CP-6,0;');
write(plt,pllb('T.-P.: '+wort(tpgesamt)));
end;

procedure grafikmittel.filewrite (var outfile:text);
var i,k:word;
begin
for i:=0 to rdauer do begin
   write(outfile,extzeit(i+anfang):12:3);
   for k:=0 to maxkanal do if k in kanaele.dabei then
      write(outfile,extspannung(mittel[k]^[i],k):15:3);
   writeln(outfile);
   end;
end;

{ garfikintervallroh }

procedure grafikintervallroh.aufbauen (rtl:char; dfn:word; ybe:extended);
begin
ref:=rtl;
ybereich:=ybe; diffn:=dfn;
new(diff);
with tliste[ref]^, liste[erstfile] do
   grafikdarstellung.aufbauen(24,34,name,ko.kennung,fileanz);
dispose(diff);
end;

procedure grafikintervallroh.bild;
const lrand=32;
      urand=14;
var   i,j,x:longint;
      faktor:extended;
      breite,enn:word;
begin
inherited bild;
breite:=max((getmaxx-lrand-2) div diffn,1);
enn:=min(diffn,getmaxx-lrand-2);
setcolor(min(bildfarbe,getmaxcolor)); settextjustify(centertext,centertext);
setwritemode(copyput); setlinestyle(solidln,0,normwidth);
line(lrand+2,40,lrand+2,getmaxy-urand);
line(lrand-3,getmaxy-urand,lrand+1+enn*breite,getmaxy-urand);
settextjustify(righttext,centertext);
outtextxy(lrand-4,65,'n'); outtextxy(lrand,76,'bin'); line(8,70,lrand,70);
outtextxy(lrand,47,extwort(ybereich,4,2)); line(lrand+1,40,lrand-3,40);
outtextxy(lrand,getmaxy-urand-5,'0');
setcolor(getmaxcolor);
faktor:=-(getmaxy-urand-40)/ybereich;
for i:=1 to enn do if diff^[i]>0 then for j:=1 to breite do begin
   x:=lrand+1+(i-1)*breite+j;
   line(x,getmaxy-urand-1,x,getmaxy-urand-1+round(diff^[i]*faktor));
   end;
end;

procedure grafikintervallroh.plot (gr:byte);
var   i:word;
begin
with p[gr] do write(plt,plip(p1x,p1y,p2x,p2y));
write(plt,plsc(0,diffn,0,ybereich));
write(plt,plpa(0,diff^[1]),plpd);
for i:=1 to diffn-1 do write(plt,plpa(i,diff^[i]),plpa(i,diff^[i+1]));
write(plt,plpa(diffn,diff^[diffn]),plpa(diffn,0),plpu);
write(plt,relativschr,plpa(0,ybereich),'CP-5.5,-0.75;',
          pllb(buendig(extwort(ybereich,5,3))));
write(plt,plpa(0,ybereich),kleinschr,'CP0,3;',pllb(filename),'CP;',pllb(filekennung),'CP;',
             pllb(kommentar),relativschr);
write(plt,plpa(0,ybereich),'TL0,1.3',plyt,plpd,plpa(0,0),plyt,
          plpd,plpa(diffn,0),plpu);
write(plt,plpa(0,0),'CP-1.5,0.25;',pllb('0'));
write(plt,plpa(0,0),'DI0,1CP0,2.5;',pllb('Occurrences [n/bin]'));
write(plt,plpa(0,ybereich/2),'TL0,0.6;',plyt);
end;

{ grafikintervall }

procedure grafikintervall.aufbauen (rtl:char; anf,sch:messwert;
                                    dfn:word; ybe:extended);
begin
anfang:=anf; schluss:=sch; dauer:=schluss-anfang;
klasse:=dauer/dfn;
inherited aufbauen(rtl,dfn,ybe);
end;

procedure grafikintervall.bild;
const lrand=32;
      urand=14;
var   i,x:longint;
      anzahl:byte; werte:skalafeld;
      xdauer:extended;
      enn,breite:word;
begin
inherited bild;
enn:=diffn; xdauer:=dauer;
bildbalken(lrand,xdauer,klasse,breite,enn);
skala(anfang,xdauer,anzahl,werte,zeitskala);
setcolor(min(bildfarbe,getmaxcolor)); settextjustify(centertext,centertext);
setwritemode(copyput); setlinestyle(solidln,0,normwidth);
for i:=1 to anzahl do begin
   x:=round((werte[i]-anfang)/klasse*breite+lrand+2);
   line(x,getmaxy-urand+1,x,getmaxy-urand+4);
   outtextxy(x,getmaxy-4,wort(zeit(werte[i])));
   end;
settextjustify(righttext,centertext);
outtextxy(getmaxx,getmaxy-4,'t [ms]');
settextjustify(lefttext,centertext);
outtextxy(0,14,'Bins þ Width: '+wort(enn)+' þ '
          +extwort(extzeit(klasse),8,3)+' ms');
end;

procedure grafikintervall.plot (gr:byte);
var   anzahl:byte; werte:skalafeld;
      i:longint;
begin
inherited plot(gr);
write(plt,plpa(diffn,0),'DI1,0;',relativschr,'CP-6,-1;',pllb('t [ms]'),'TL-1,0;');
skala(anfang,dauer,anzahl,werte,zeitskala);
for i:=anzahl downto 1 do
   write(plt,plpa((werte[i]-anfang)/dauer*diffn,0),plxt,'CP-.33,-1;',
             pllb(wort(zeit(werte[i]))));
write(plt,kleinschr,plpa(diffn,0),'DI0,1;CP0,-3;');
Write(plt,pllb(' Bins          :'+wort(diffn)+' ('+extwort(zeit(klasse),1,3)+' ms)'));
end;

procedure grafikintervall.filewrite (var outfile:text);
var i:word;
begin
for i:=1 to diffn do
   writeln(outfile,extzeit(anfang+(i-0.5)*dauer/diffn):12:3,diff^[i]:12:5);
end;

{ grafikphasenintervall }

procedure grafikphasenintervall.bild;
const lrand=32;
      urand=14;
var   i,x:longint;
      anzahl:byte; werte:skalafeld;
      xdauerphase:extended;
      breite,enn:word;
begin
inherited bild;
enn:=diffn; xdauerphase:=dauerphase;
bildbalken(lrand,xdauerphase,klasse,breite,enn);
skala(anfphase,xdauerphase,anzahl,werte,phasenskala);
setcolor(min(bildfarbe,getmaxcolor)); settextjustify(centertext,centertext);
setwritemode(copyput); setlinestyle(solidln,0,normwidth);
for i:=1 to anzahl do begin
   x:=round((werte[i]-anfphase)/klasse*breite+lrand+2);
   line(x,getmaxy-urand+1,x,getmaxy-urand+4);
   outtextxy(x,getmaxy-4,extwort(frac(werte[i]),4,2));
   end;
settextjustify(righttext,centertext);
outtextxy(getmaxx,getmaxy-4,'Phase');
settextjustify(lefttext,centertext);
outtextxy(0,14,'Bins þ Width: '+wort(enn)+' þ '+extwort(klasse,1,3));
end;

procedure grafikphasenintervall.plot (gr:byte);
var   anzahl:byte; werte:skalafeld;
      i:longint;
begin
inherited plot(gr);
write(plt,plpa(diffn,0),'DI1,0;',relativschr,'CP-6,-1;',pllb('Phase'),'TL-1,0;');
skala(anfphase,dauerphase,anzahl,werte,phasenskala);
for i:=anzahl downto 1 do
   write(plt,plpa((werte[i]-anfphase)/dauerphase*diffn,0),plxt,'CP-.33,-1;',
             pllb(extwort(frac(werte[i]),4,2)));
write(plt,kleinschr,plpa(diffn,0),'DI0,1;CP0,-3;');
Write(plt,pllb(' Bins          :'+wort(diffn)+' ('+extwort(klasse,1,3)+')'));
end;

procedure grafikphasenintervall.filewrite (var outfile:text);
var i:word;
begin
for i:=1 to diffn do
   writeln(outfile,frac(anfphase+(i-0.5)*dauerphase/diffn):9:6,diff^[i]:12:5);
end;

procedure grafikphasenintervall.aufbauen (rtl:char; anfph,dauph:extended; dfn:word;
                                          ybe:extended);
begin
anfphase:=anfph;
diffnganz:=round(dfn/dauph);
dauerphase:=dfn/diffnganz;
klasse:=dauerphase/dfn;
inherited aufbauen(rtl,dfn,ybe);
end;

end.