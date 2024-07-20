{Turbo-Pascal 6.0}

unit  nlgrafik;

{$A+,B-,F-,G+,I-,N+,O-,V-,X-}

interface

uses  crt,                graph,              tulab,              nlrahmen,
      dos,                grafik,             tlfilter,           nltrigg,
      bequem,             plotter,            tlfiles;

const lrand=50;

type  datenfunktion=function (stelle:messwert; kanal:byte):integer;
      yliste=array[1..maxkanal+1] of word;

var   y0:yliste;
      breite,oben:word;

procedure grafikzeit (var diff:zeitfeld; diffn:word; von,bis:messwert;
             refpunkte:longint; ybereich:extended;
             var filename,filekennung:string80);

procedure plotzeit (var diff:zeitfeld; diffn:word; von,bis:messwert;
             refpunkte:longint; ybereich:extended;
             var filename,filekennung:string80; gr:byte);

procedure grafikmittel(var mit:mittelliste; kanaele:kanalliste;
                       von,bis:messwert; tl:char; tp:longint);

procedure grafiknormal (anfang,dauer:messwert; kanaele:kanalliste;
                        daten:datenfunktion; datenanfang:messwert);
function stellex (stelle:messwert):longint;
function xstelle (x:integer):messwert;
procedure linie (x1,x2:word);

procedure plotnormal (anfang,dauer:messwert; kanaele:kanalliste;
                      daten:datenfunktion; datenanfang:messwert; gr:byte;
                      var filename,filekennung:string80);

implementation

const kleinschr='SI0.14,0.22;';  relativschr='SR1,2.6;';
      ganz=4096;

type  skalafeld=array[1..10] of messwert;
      punkte=record p1x,p1y,p2x,p2y:word end;

const p:array[1..5] of punkte=((p1x:3000; p1y:3000; p2x:5400; p2y:5000),
                               (p1x:2600; p1y:2600; p2x:6600; p2y:5200),
                               (p1x:2300; p1y:2300; p2x:7600; p2y:5300),
                               (p1x:2000; p1y:2000; p2x:8660; p2y:5465),
                               (p1x:1150; p1y:380;  p2x:9750; p2y:6380));

var   stauchung,faktor:extended;
      datanf:messwert;
      mittel:mittelliste;

function buendig (schrift:string20):string20;
const  leer:string20='     ';
var    n:byte absolute leer;
begin
if length(schrift)<5 then n:=5-length(schrift) else n:=0;
buendig:=leer+schrift;
end;

procedure skala (anfang,dauer:messwert; var anz:byte; var werte:skalafeld);
var   n,i,step:byte; n10,zeitpa,zeitpe:longint;
begin
n:=max(trunc(log(zeit(0.9*dauer)))-1,0);
n10:=pot(n);
zeitpa:=zeit(anfang);
if (zeitpa mod n10 <>0) and (zeitpa>0) then zeitpa:=zeitpa div n10+1
                                       else zeitpa:=zeitpa div n10;
zeitpe:=zeit(anfang+0.9*dauer) div n10;
case zeitpe-zeitpa+1 of
   0:begin anz:=0; exit end;
   1..6:step:=1;        7..14:step:=2;
   15..30:step:=5;      31..60:step:=10;
   61..100:step:=20;    else step:=40;
   end;
if (zeitpa mod step <>0) and (zeitpa>0) then zeitpa:=(zeitpa div step+1)*step
                                        else zeitpa:=(zeitpa div step)*step;
if (zeitpe mod step <>0) and (zeitpe<0) then zeitpe:=(zeitpe div step+1)*step
                                        else zeitpe:=(zeitpe div step)*step;
anz:=(zeitpe-zeitpa) div step+1;
for i:=1 to anz do werte[i]:=(zeitpa+(i-1)*step)*n10*fre/1000;
end;

procedure grafikzeit (var diff:zeitfeld; diffn:word; von,bis:messwert;
                      refpunkte:longint; ybereich:extended;
                      var filename,filekennung:string80);
const lrand=24;
      urand=14;
var   laenge,klasse:extended;
      anzahl:byte; werte:skalafeld;
      enn,breite:word;
      i,j,x:longint;
      faktor:extended;
      taste:char;
begin
opengraph;
laenge:=bis-von;
breite:=(maxx-lrand-2) div diffn;
klasse:=laenge/diffn;
enn:=diffn;
if breite=0 then begin breite:=1; enn:=maxx-lrand-2; laenge:=enn*klasse end;
skala(von,laenge,anzahl,werte);
setcolor(min(green,getmaxcolor)); settextjustify(centertext,centertext);
setwritemode(copyput); setlinestyle(solidln,0,normwidth);
cleardevice;
line(lrand+2,40,lrand+2,maxy-urand);
line(lrand-3,maxy-urand,maxx,maxy-urand);
for i:=1 to anzahl do begin
   x:=round((werte[i]-von)/klasse*breite+lrand+2);
   line(x,maxy-urand+1,x,maxy-urand+4);
   outtextxy(x,maxy-4,wort(zeit(werte[i])));
   end;
settextjustify(righttext,centertext);
outtextxy(maxx,maxy-4,'t [ms]');
outtextxy(lrand-4,65,'n'); outtextxy(lrand,76,'bin'); line(0,70,lrand,70);
outtextxy(lrand,47,extwort(ybereich,3,1)); line(lrand+1,40,lrand-3,40);
outtextxy(lrand,maxy-urand-5,'0');
setcolor(getmaxcolor);
outtextxy(maxx,4,'Triggerstellen:'+wort(refpunkte));
settextjustify(lefttext,centertext);
outtextxy(0,4,'File:'+filename); outtextxy(0,14,filekennung);
faktor:=-(maxy-urand-40)/ybereich;
for i:=1 to enn do if diff[i]>0 then for j:=1 to breite do begin
   x:=lrand+1+(i-1)*breite+j;
   line(x,maxy-urand-1,x,maxy-urand-1+round(diff[i]*faktor));
   end;
settextjustify(centertext,centertext);
outtextxy(maxx div 2,24,'Plotten:<Ctrl><Print>');
repeat until keypressed; taste:=readkey;
settextjustify(righttext,centertext);
if (taste=#0) and (readkey=#114) then begin
   fenster(200,100,440,156);
   outtextxy(199,18,'Plotten (Gr”áe 1..5 ?)');
   taste:=readkey;
   i:=zahl(taste); outtextxy(225,18,wort(i));
   outtextxy(199,38,'Abbruch mit <ESC>');
   if taste in ['1'..'5'] then
     plotzeit(diff,diffn,von,bis,refpunkte,ybereich,filename,filekennung,i);
   end;
closegraph;
end;

procedure plotzeit (var diff:zeitfeld; diffn:word; von,bis:messwert;
           refpunkte:longint; ybereich:extended;
           var filename,filekennung:string80; gr:byte);
var   anzahl:byte; werte:skalafeld;
      i:word;
      laenge:messwert;
begin
laenge:=bis-von;
rewrite(plt);
with p[gr] do write(plt,plin,plip(p1x,p1y,p2x,p2y));
write(plt,plsc(0,diffn,0,ybereich));
write(plt,plpa(0,diff[1]),plpd);
for i:=1 to diffn-1 do write(plt,plpa(i,diff[i]),plpa(i,diff[i+1]));
write(plt,plpa(diffn,diff[diffn]),plpa(diffn,0),plpu);
write(plt,relativschr,plpa(0,ybereich),'CP-5.5,-0.75;',
          pllb(buendig(extwort(ybereich,5,1))));
write(plt,plpa(0,ybereich),'CP0,2;',kleinschr,pllb(filekennung),'CP;CP0,2;',
          pllb(filename),relativschr);
write(plt,plpa(0,ybereich),'TL0,1.3',plpd,plyt,plpa(0,0),plyt,plpa(diffn,0),
          plpu,relativschr,'CP-6,-1;',pllb('t [ms]'),'TL-1,0;');
skala(von,laenge,anzahl,werte);
for i:=anzahl downto 1 do
   write(plt,plpa((werte[i]-von)/laenge*diffn,0),plxt,'CP-.33,-1;',
             pllb(wort(zeit(werte[i]))));
write(plt,plpa(0,0),'CP-1.5,0.25;',pllb('0'));
write(plt,plpa(0,0),'DI0,1CP0,2.5;',pllb('Occurences [n/bin]'));
write(plt,plpa(0,ybereich/2),'TL0,0.6;',plyt);
end;

function kon (hoehe:wert; k:byte):wert;
begin
if belegungsliste[k].negativ then kon:=hoehe
                             else kon:=(hoehe-2048)*2;
end;

function rekon (hoehe:wert; k:byte):wert;
begin
if belegungsliste[k].negativ then rekon:=hoehe
                             else rekon:=hoehe/2+2048;
end;

function stellex (stelle:messwert):longint;
var   zwischen:extended;
begin
zwischen:=mine((stelle-datanf)/stauchung+lrand+1,maxlongint);
stellex:=round(zwischen);
end;

function xstelle (x:integer):messwert;
begin
xstelle:=datanf+(x-lrand-1)*stauchung;
end;

procedure linie (x1,x2:word);
var i:word;
begin
setlinestyle(solidln,0,normwidth);
for i:=x1 to x2 do line(i,oben,i,maxy-11);
end;

procedure grafiknormal (anfang,dauer:messwert; kanaele:kanalliste;
                        daten:datenfunktion; datenanfang:messwert);
var   i,x,buch,yn:longint;
      anzahl:byte; werte:skalafeld;
      y:yliste;
begin
setcolor(min(green,getmaxcolor));
datanf:=datenanfang;
with kanaele do begin
   cleardevice; setwritemode(copyput); setlinestyle(solidln,0,normwidth);
   stauchung:=dauer/(maxx-lrand);
   breite:=(maxy-40) div (2*kn);
   oben:=maxy-2*kn*breite-10;
   faktor:=-breite/2048;
   settextjustify(righttext,centertext);
   outtextxy(maxx,maxy-4,'t [ms]');
   line(lrand,oben,lrand,maxy-10);
   line(lrand,maxy-10,maxx,maxy-10);
   buch:=breite div 4;
   for i:=1 to kn do with belegungsliste[k[i]] do begin
      y0[i]:=round(oben+(2*i)*breite);
      setlinestyle(solidln,0,normwidth);
      for x:=1 to 3 do begin
         moveto(lrand,y0[i]-round(x/2*breite)); linerel(4,0) end;
      setlinestyle(dashedln,0,normwidth);
      moveto(lrand,y0[i]-2*breite); linerel(maxx-10,0);
      if kn<=10 then begin
         settextstyle(defaultfont,vertdir,1);
         settextjustify(centertext,centertext);
         outtextxy(4,y0[i]-breite,copy(schriftliste[k[i]],1,buch));
         settextstyle(defaultfont,horizdir,1);
         settextjustify(lefttext,toptext);
         outtextxy(14,y0[i]-2*breite+12,einhwort);
         settextjustify(righttext,toptext);
         outtextxy(lrand-1,y0[i]-2*breite+1,wort(spannung(4096,k[i])));
         settextjustify(righttext,bottomtext);
         outtextxy(lrand-1,y0[i],wort(spannung(rekon(0,k[i]),k[i])));
         if kn<=6 then begin
            settextjustify(righttext,centertext);
            outtextxy(lrand-1,y0[i]-breite,wort(spannung(rekon(2048,k[i]),k[i])));
            end;
         end
               else begin
         settextjustify(righttext,centertext);
         outtextxy(lrand-1,y0[i]-breite,copy(schriftliste[k[i]],1,6));
         end;
      end;
   settextstyle(defaultfont,horizdir,1);
   settextjustify(centertext,centertext);
   skala(anfang,dauer,anzahl,werte);
   for i:=1 to anzahl do begin
      x:=round((werte[i]-anfang)/stauchung+lrand+1);
      line(x,maxy-10,x,maxy-14);
      outtextxy(x,maxy-4,wort(zeit(werte[i])));
      end;
   setlinestyle(solidln,0,normwidth); setcolor(getmaxcolor);
   for i:=1 to kn do
     y[i]:=y0[i]+round(kon(daten(datenanfang,k[i]),k[i])*faktor);
   for x:=lrand+1 to maxx-1 do for i:=1 to kn do begin
      yn:=y0[i]+round(kon(daten(xstelle(x),k[i]),k[i])*faktor);
      line(x,y[i],x+1,yn);
      y[i]:=yn end;
      end;
end;

procedure plotnormal (anfang,dauer:messwert; kanaele:kanalliste;
                      daten:datenfunktion; datenanfang:messwert; gr:byte;
                      var filename,filekennung:string80);
var   i,j:longint;
      anzahl:byte;          werte:skalafeld;
      rdauer:longint;
      lrand:extended;
      fleinheit:einheittyp;
begin
with kanaele do begin
   rdauer:=round(dauer);
   lrand:=-rdauer/12;
   rewrite(plt);
   with p[gr] do write(plt,plin,plip(p1x,p1y,p2x,p2y));
   write(plt,relativschr,plsc(0,rdauer,0,ganz*kn));
   for i:=kn downto 1 do begin
     write(plt,plpa(0,ganz*(kn-i)+kon(daten(datenanfang,k[i]),k[i])),
               plpd);
     for j:=0 to rdauer do begin
       write(plt,plpa(j,ganz*(kn-i)+kon(daten(datenanfang+j,k[i]),k[i])));
        if keypressed and (readkey=#27) then exit;
        end;
     write(plt,plpu);
     end;
   for i:=1 to kn do begin
     write(plt,plpa(lrand,ganz*(kn-i+1)),'CP0,-0.75;',
               pllb(buendig(wort(spannung(ganz,k[i])))),
               'CP;CP0.5,-0.25;',belegungsliste[k[i]].plot);
     write(plt,plpa(lrand,ganz*(kn-i)+ganz/2),'CP0,-0.25;',
               pllb(buendig(wort(spannung(rekon(ganz div 2,k[i]),k[i])))),
               plpa(lrand,ganz*(kn-i)),'CP0,0.25;',
               pllb(buendig(wort(spannung(rekon(0,k[i]),k[i])))));
     end;
   write(plt,'TL0,1.3;',plpa(0,0),plyt);
   for i:=kn downto 1 do begin
      write(plt,'TL0,0.6;');
      for j:=1 to 3 do write(plt,plpa(0,ganz*(kn-i)+j*ganz/4),plyt);
      write(plt,'TL0,1.3;',plpa(0,ganz*(kn-i+1)),plyt);
      end;
   write(plt,'CP0,2;',kleinschr,pllb(filekennung),'CP;CP0,2;',
             pllb(filename),relativschr);
   write(plt,plpu,plpa(0,ganz*kn),plpd,plpa(0,0),plpa(rdauer,0),plpu,
             'CP-6,-1;',pllb('t [ms]'),'TL-1,0;');
   skala(anfang,rdauer,anzahl,werte);
   for i:=anzahl downto 1 do
      write(plt,plpa((werte[i]-anfang),0),plxt,'CP-.33,-1;',
                pllb(wort(zeit(werte[i]))));
   write(plt,'DI0,1;');
   for i:=kn downto 1 do write(plt,plpu,plpa(lrand,ganz*(kn-i)),
        'CP0,0.5;',pllb(schriftliste[k[i]]));
   end;
end;

procedure rauschen (var feld:mittelfeld; dauer:messwert; var hoehe:integer);
const breite=8;
var   verteilung:array[0..4097] of longint;
      i,j,wol,wor,gesamt,bisher,jetzt:longint;
begin
fillchar(verteilung,sizeof(verteilung),0);
gesamt:=trunc(dauer);
for i:=0 to gesamt do
  if (feld[i]<=4096) and (feld[i]>=0) then inc(verteilung[round(feld[i])]);
bisher:=0;
for i:=0 to 4096 do begin
   jetzt:=0;
   for j:=max(0,i-breite) to min(4096,i+breite) do inc(jetzt,verteilung[j]);
   if jetzt>=bisher then begin
      if jetzt>bisher then begin wol:=i; bisher:=jetzt end;
      wor:=i; end;
   end;
hoehe:=(wol+wor) div 2;
end;

function mittelf (st:messwert; k:byte):integer; far;
begin
mittelf:=round(mittel[k]^[round(st)]);
end;

procedure grafikmittel(var mit:mittelliste; kanaele:kanalliste;
                       von,bis:messwert; tl:char; tp:longint);
var   dauer:messwert;         rdauer:longint;
      x,y,i,j:longint;
      taste:char;             flaech:extended;
      maxi:array[1..maxkanal+1] of wert;
      flaeche:array[1..maxkanal+1] of wert;
      hoehe:array[1..maxkanal+1] of integer;
      fleinheit:einheittyp;

procedure plotmittel (gr:byte);
var   i:longint;
      fleinheit:einheittyp;

   function buendig (zahl:longint):string20;
   const  leer:string20='     ';
   var    n:byte absolute leer;
          zahlstr:string20;
   begin
   zahlstr:=wort(zahl);
   if length(zahlstr)<5 then n:=5-length(zahlstr) else n:=0;
   buendig:=leer+zahlstr;
   end;

begin
with liste[tliste[tl]^.erstfile] do
   plotnormal(von,dauer,kanaele,mittelf,0,gr,name,ko.kennung);
with kanaele do begin
   write(plt,kleinschr,'DI0,1;',plpa(rdauer,0),'CP-2,-2;',pllb('b'),'CP-1,-1;',
             pllb('m'),'CP-1,-1;',pllb('a'));
   for i:=kn downto 1 do begin
      fleinheit:=belegungsliste[k[i]]; inc(fleinheit.sekunde);
      write(plt,plpa(rdauer,ganz*(kn-i)),'CP0,-2;',
                pllb(extwort(extspannung(hoehe[i],k[i]),1,2)+' '),
                belegungsliste[k[i]].plot,'CP;',
                pllb(extwort(maxi[i]*belegungsliste[k[i]].faktor,1,2)+' '),
                belegungsliste[k[i]].plot,'CP;',
                pllb(extwort(flaeche[i]*belegungsliste[k[i]].faktor,1,3)+' '),
                fleinheit.plot);
      end;
   write(plt,'DI1,0;',plpa(rdauer,ganz*kn),'CP1,1;');
   write(plt,'LBF:',tliste[tl]^.fileanz:2,#3,'CP-4,1;');
   write(plt,'LBTP:',tp,#3);
   end;
end;

begin
with kanaele, tliste[tl]^, liste[erstfile] do begin
   dauer:=bis-von; rdauer:=round(dauer);
   mittel:=mit;
   opengraph;
   grafiknormal (von,dauer,kanaele,mittelf,0);
   settextjustify(lefttext,centertext);
   outtextxy(0,4,'File:'+name+' (Anzahl:'+wort(fileanz)+')');
   outtextxy(0,14,ko.kennung);
   settextjustify(centertext,centertext);
   outtextxy(maxx div 2,24,'Plotten:<Ctrl><Print>');
   settextjustify(righttext,centertext);
   outtextxy(maxx,4,'Trigger-Punkte: '+wort(tp));
   setlinestyle(dottedln,0,normwidth);
   settextjustify(righttext,toptext);
   for j:=1 to kn do begin
      rauschen(mittel[k[j]]^,dauer,hoehe[j]);
      outtextxy(maxx,y0[j]-2*breite+1,'base='
         +extwort(extspannung(hoehe[j],k[j]),1,2)+' '
         +belegungsliste[k[j]].einhwort);
      y:=y0[j]+round(kon(hoehe[j],k[j])*faktor);
      line(lrand+1,y,maxx,y);
      x:=0; maxi[j]:=0;
      for i:=0 to trunc(bis-von) do if maxi[j]<mittel[k[j]]^[i] then begin
         maxi[j]:=mittel[k[j]]^[i]; x:=i end;
      y:=y0[j]+round(kon(maxi[j],k[j])*faktor);
      x:=round(x/stauchung)+lrand+1;
      maxi[j]:=maxi[j]-hoehe[j];
      outtextxy(maxx,y0[j]-2*breite+9,'maximum='
         +extwort(maxi[j]*belegungsliste[k[j]].faktor,1,2)
         +' '+belegungsliste[k[j]].einhwort);
      line(max(lrand+1,x-20),y,min(maxx,x+20),y);
      flaech:=0;
      for i:=0 to rdauer do flaech:=flaech+mittel[k[j]]^[i];
      flaeche[j]:=(flaech/(rdauer+1)-hoehe[j])*zeit(rdauer+1)/1000;
      fleinheit:=belegungsliste[k[j]]; inc(fleinheit.sekunde);
      outtextxy(maxx,y0[j]-2*breite+17,'area='
         +extwort(flaeche[j]*belegungsliste[k[j]].faktor,1,3)+' '
         +fleinheit.einhwort);
      end;
   setlinestyle(solidln,0,normwidth);
   repeat until keypressed; taste:=readkey;
   if (taste=#0) and (readkey=#114) then begin
      fenster(200,100,440,156);
      outtextxy(199,18,'Plotten (Gr”áe 1..5 ?)');
      taste:=readkey;
      i:=zahl(taste); outtextxy(225,18,wort(i));
      outtextxy(199,38,'Abbruch mit <ESC>');
      if taste in ['1'..'5'] then plotmittel(i);
      end;
   closegraph;
   end;
end;

end.