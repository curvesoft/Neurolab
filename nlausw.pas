{ Borland-Pascal 7.0 / FPC 2.4 }
{$ifdef fpc} {$mode TP} {$endif}

unit nlausw;

{$IFDEF MSDOS}
{$A+,B-,E+,F-,G-,I-,N+,O-,P+,T+,V+,X-}
{$ELSE}
{$A+,B-,E+,F-,G+,I-,N+,P+,T+,V+,X-}
{$ENDIF}

{$ifdef win32} {$define wingraph} {$endif}

interface

uses  {$ifdef wingraph} crt,wingraph, {$else} crt,graph, {$endif}
      daff,wavpcm,tulab42,  nlrahmen,
      dos,      grafik,   tlfilter,      nltrigg,
      bequem,   plotter,  tlfiles,
                nlgrafik;

type  statistik=object
         n:grossint;
         sx,ssqrx:extended;
         mx,rox:extended;
         constructor init;
         procedure dazu(x:extended);
         procedure rechnen;
         end;

      phasenstatistik=object
         n:grossint;
         sx,sy:extended;
         mph,lvek:extended;
         constructor init;
         procedure dazu (ph:extended);
         procedure rechnen;
         end;

      ampnormalhistogramm=object (grafikamplitude)
         tstat:statistik;
         gesamt:grossint;
         procedure berechnen; virtual;
         procedure bild; virtual;
         procedure plot (gr:byte); virtual;
         constructor aufbauen (xk:byte; rtl:char; dfn:word; ybe:extended; mins,maxs:sample);
         end;

      grafikaverage=object (grafikmittel)
         hoehe:array[1..maxkanal+1] of sample;
         maxi:array[1..maxkanal+1] of wert;
         maxix:array[1..maxkanal+1] of grossint;
         flaeche:array[1..maxkanal+1] of wert;
         procedure berechnen; virtual;
         procedure bild; virtual;
         procedure plot (gr:byte); virtual;
         constructor aufbauen(var kan:kanalmenge; anf,dau:messwert;
                              trl:char; trp:grossint);
         end;

      grafikphasenaverage=object (grafikmittel)
         triggweis:triggerweiser;
         hoehe:array[1..maxkanal+1] of sample;
         maxi:array[1..maxkanal+1] of wert;
         maxix:array[1..maxkanal+1] of grossint;
         flaeche:array[1..maxkanal+1] of wert;
         procedure berechnen; virtual;
         procedure bild; virtual;
         procedure plot (gr:byte); virtual;
         constructor aufbauen(var kan:kanalmenge; anf,dau:messwert;
                              trl:char; trp:grossint; var weis:triggerweiser);
         end;

      intervallhistogramm=object (grafikintervall)
         gesamt:grossint;
         tstat:statistik;
         procedure berechnen; virtual;
         procedure bild; virtual;
         procedure plot (gr:byte); virtual;
         constructor aufbauen(rtl:char; anf,sch:messwert;
                              din:word; ybe:extended);
         end;

      autokorrelogramm=object (grafikintervall)
         gesamt:grossint;
         tstat:statistik;
         procedure berechnen; virtual;
         procedure bild; virtual;
         procedure plot (gr:byte); virtual;
         constructor aufbauen(rtl:char; anf,sch:messwert;
                              din:word; ybe:extended);
         end;

      kreuzkorrelogramm=object (grafikintervall)
         obj:char;
         refn,objn:grossint;
         nstat,tstat:statistik;
         procedure berechnen; virtual;
         procedure bild; virtual;
         procedure plot (gr:byte); virtual;
         constructor aufbauen(rtl,otl:char; anf,sch:messwert;
                              din:word; ybe:extended);
         end;

      psthistogramm=object (grafikintervall)
         obj:char;
         anfph,endph:shortint;
         refn,objn:grossint;
         nstat,tstat:statistik;
         procedure berechnen; virtual;
         procedure bild; virtual;
         procedure plot (gr:byte); virtual;
         constructor aufbauen(rtl,otl:char; aph,eph:shortint;
                              anf,sch:messwert; din:word; ybe:extended);
         end;

      latenzhistogramm=object (grafikintervall)
         obj:char;
         refn:grossint;
         tstat:statistik;
         procedure berechnen; virtual;
         procedure bild; virtual;
         procedure plot (gr:byte); virtual;
         constructor aufbauen(rtl,otl:char; anf,sch:messwert;
                              din:word; ybe:extended);
         end;

      phasenhistogramm=object (grafikphasenintervall)
         obj:char;
         refn,objn:grossint;
         weis:triggerweiser;
         nstat:statistik;
         phstat:phasenstatistik;
         procedure berechnen; virtual;
         procedure bild; virtual;
         procedure plot (gr:byte); virtual;
         constructor aufbauen(rtl,otl:char; minabst,maxabst:messwert;
                              anfph,schph:extended; din:word; ybe:extended);
         end;

function phasentest(tph,anfph,dauph:extended):boolean;

implementation

function phasentest(tph,anfph,dauph:extended):boolean;
begin
phasentest:=((tph>=anfph) and (tph<=anfph+dauph)) or
            ((tph+1>=anfph) and (tph+1<=anfph+dauph));
end;

{ statistik }

constructor statistik.init;
begin
n:=0;
sx:=0; ssqrx:=0;
end;

procedure statistik.dazu (x:extended);
begin
inc(n);
sx:=sx+x; ssqrx:=ssqrx+sqr(x);
end;

procedure statistik.rechnen;
var   arg:extended;
begin
if n>1 then begin
   mx:=sx/n;
   arg:=(ssqrx-sqr(sx)/n)/(n-1);
   if arg<=0 then rox:=0
             else rox:=sqrt(arg);
   end else begin
   if n>0 then mx:=sx/n else mx:=0;
   rox:=0;
   end
end;

{ phasenstatistik }

constructor phasenstatistik.init;
begin
n:=0;
sx:=0; sy:=0;
end;

procedure phasenstatistik.dazu (ph:extended);
begin
inc(n);
sx:=sx+cos(ph); sy:=sy+sin(ph);
end;

procedure phasenstatistik.rechnen;
begin
if n=0 then begin mph:=0; lvek:=0 end
       else begin
   mph:=arctan(sy/sx);
   if sx<0 then mph:=mph+pi;
   if mph<0 then mph:=mph+2*pi;
   lvek:=sqrt(sqr(sx/n)+sqr(sy/n));
   end;
end;


procedure rauschen (var feld:mittelfeld; dauer:messwert; var hoehe:sample);
const breite=16;
      faktor=maxsample/5000;
var   verteilung:array[-5000..5000] of grossint;
      i,j,wol,wor,gesamt,bisher,jetzt:longint;
begin
fillchar(verteilung,sizeof(verteilung),0);
gesamt:=trunc(dauer);
for i:=0 to gesamt do
  if (feld[i]<=maxsample) and (feld[i]>=minsample) then
     inc(verteilung[round(feld[i]/faktor)]);
bisher:=0;
for i:=-5000 to 5000 do begin
   jetzt:=0;
   for j:=max(-5000,i-breite) to min(5000,i+breite) do inc(jetzt,verteilung[j]);
   if jetzt>=bisher then begin
      if jetzt>bisher then begin wol:=i; bisher:=jetzt end;
      wor:=i; end;
   end;
hoehe:=round((wol+wor)/2*faktor);
end;

{ ampnormalhistogramm }

procedure ampnormalhistogramm.berechnen;
var   um:extended;
      tpr:exword;
      ywert:extended;
      nr:byte;
begin
gesamt:=tliste[ref]^.triggsum;
if gesamt=0 then begin
   write('No trigger point'); pieps; warte;
   abbruch:=true; exit end;
fillchar(diff^,sizeof(bildfeld),0);
tstat.init;
um:=1/gesamt;
for nr:=1 to filenr do with tliste[ref]^.fil[nr] do begin
   oeffnen(nr);
   for tpr:=1 to automn do begin
      ywert:=dat(zwi(autom^[tpr]),xkanal);
      if (kon(ywert,xkanal)<maxsamp+klasse) and (kon(ywert,xkanal)>minsamp-klasse) then begin
         tstat.dazu(ywert);
         incex(diff^[trunc((kon(ywert,xkanal)-minsamp)/klasse)+1],um);
         end;
      end;
   schliesse;
   end;
tstat.rechnen;
end;

procedure ampnormalhistogramm.bild;
var   rand:word;
      einheit:einheitstring;
begin
inherited bild;
rand:=getmaxx div 2;
einheit:=belegungsliste[xkanal].einhwort;
setcolor(getmaxcolor); settextjustify(lefttext,centertext);
outtextxy(rand,4, 'Trigger Points: '+wort(gesamt));
outtextxy(rand,14,'Evaluated     : '+wort(tstat.n));
outtextxy(rand,24,'Mean Amplitude: ('
                 +extwort(extspannung(tstat.mx,xkanal),1,1)+'ñ'
                 +extwort(extspannung(tstat.rox,xkanal),1,1)+') '+einheit);
outtextxy(0,4,'"AMPLITUDE HISTOGRAM"');
end;

procedure ampnormalhistogramm.plot (gr:byte);
begin
inherited plot(gr);
write(plt,kleinschr,plpa(diffn,0),'DI0,1;CP0,-2;');
write(plt,pllb('AMPLITUDE HISTOGRAM'),'CP;CP;');
write(plt,pllb(' Trigger Points: '+wort(gesamt)),'CP;');
write(plt,pllb(' Evaluated     : '+wort(tstat.n)),'CP;');
write(plt,pllb(' Mean Amplitude: ('+extwort(extspannung(tstat.mx,xkanal),1,1)
              +' +/- '+extwort(extspannung(tstat.rox,xkanal),1,1)
              +') '),belegungsliste[xkanal].plot,'CP;');
end;

constructor ampnormalhistogramm.aufbauen (xk:byte; rtl:char; dfn:word;
                                          ybe:extended; mins,maxs:sample);
begin
grafikamplitude.aufbauen (xk,rtl,dfn,ybe,mins,maxs);
end;

{grafikaverage}

procedure grafikaverage.berechnen;
var   i,j,tp,tptot:longint;
      flaech:extended;
      nr:byte;
begin
tptot:=0;
for nr:=1 to filenr do with tliste[tl]^.fil[nr] do
  if automda then begin
   write(#13); clreol;
   write('Averaging: ',liste[nr].name,', total trigger point no.:');
   oeffnen(nr);
   for tp:=1 to automn do begin
      for j:=0 to maxkanal do if j in kanaele.dabei then for i:=0 to rdauer do
         mittel[j]^[i]:=mittel[j]^[i]+dat(zwi(autom^[tp]+anfang+i),j);
      inc(tptot); if (tptot mod 128) = 0 then write(tptot:9,#8#8#8#8#8#8#8#8#8);
      if keypressed and (readkey=#27) then begin abbruch:=true; exit end;
      end;
   schliesse;
   end;
for j:=0 to maxkanal do if j in kanaele.dabei then
   for i:=0 to rdauer do mittel[j]^[i]:=mittel[j]^[i]/tpgesamt;
with kanaele do for j:=1 to kn do begin
   rauschen(mittel[k[j]]^,dauer,hoehe[j]);
   maxix[j]:=0; maxi[j]:=0;
   for i:=0 to trunc(dauer) do if maxi[j]<mittel[k[j]]^[i] then begin
      maxi[j]:=mittel[k[j]]^[i]; maxix[j]:=i end;
   flaech:=0;
   for i:=0 to rdauer do flaech:=flaech+mittel[k[j]]^[i];
   flaeche[j]:=(flaech/(rdauer+1)-hoehe[j])*zeit(rdauer+1)/1000;
   end;
end;

procedure grafikaverage.bild;
var   x,y,j:longint;
      fleinheit:einheittyp;

begin
with kanaele do begin
   grafikmittel.bild;
   setlinestyle(dottedln,0,normwidth);
   settextjustify(righttext,toptext);
   for j:=1 to kn do begin
      outtextxy(getmaxx,y0[j]-breite+1,'Base='
         +extwort(extspannung(hoehe[j],k[j]),1,2)+' '
         +belegungsliste[k[j]].einhwort);
      y:=y0[j]+round(kon(hoehe[j],k[j])*faktor);
      line(lrand+1,y,getmaxx,y);
      y:=y0[j]+round(kon(maxi[j],k[j])*faktor);
      x:=round(maxix[j]/stauchung)+lrand+1;
      outtextxy(getmaxx,y0[j]-breite+9,'Maximum='
         +extwort((maxi[j]-hoehe[j])*belegungsliste[k[j]].faktor,1,2)
         +' '+belegungsliste[k[j]].einhwort);
      line(max(lrand+1,x-20),y,min(getmaxx,x+20),y);
      fleinheit:=belegungsliste[k[j]]; inc(fleinheit.sekunde);
      outtextxy(getmaxx,y0[j]-breite+17,'Area='
         +extwort(flaeche[j]*belegungsliste[k[j]].faktor,1,3)+' '
         +fleinheit.einhwort);
      end;
   setlinestyle(solidln,0,normwidth);
   end;
settextjustify(lefttext,centertext);
outtextxy(0,4,'"AVERAGE"');
end;

procedure grafikaverage.plot (gr:byte);
var   i:longint;
      fleinheit:einheittyp;

begin
grafikmittel.plot(gr);
if abbruch then exit;
with kanaele do begin
   write(plt,kleinschr,'DI0,1;',plpa(rdauer,0),'CP-2,-2;',
             pllb('AVERAGE'),'CP-7,-1;',
             pllb('b'),'CP-1,-1;',pllb('m'),'CP-1,-1;',pllb('a'));
   for i:=kn downto 1 do begin
      fleinheit:=belegungsliste[k[i]]; inc(fleinheit.sekunde);
      write(plt,plpa(rdauer,ganz*(kn-i)),'CP0,-3;',
                pllb(extwort(extspannung(hoehe[i],k[i]),1,2)+' '),
                belegungsliste[k[i]].plot,'CP;',
                pllb(extwort((maxi[i]-hoehe[i])*belegungsliste[k[i]].faktor,1,2)+' '),
                belegungsliste[k[i]].plot,'CP;',
                pllb(extwort(flaeche[i]*belegungsliste[k[i]].faktor,1,3)+' '),
                fleinheit.plot);
      end;
   end;
end;

constructor grafikaverage.aufbauen(var kan:kanalmenge;anf,dau:messwert;
                                   trl:char; trp:grossint);
begin
grafikmittel.aufbauen(kan,anf,dau,trl,trp);
end;

procedure grafikphasenaverage.berechnen;
var   nr:byte;
      i,j,tp,tptot:longint;
      abst:messwert;
      dehnfaktor,flaech:extended;
begin
with triggweis, tliste[tl]^ do begin
   tptot:=0;
   for nr:=1 to filenr do with weisliste[nr], fil[nr] do if automda then begin
      write(#13); clreol;
      write('Averaging: ',liste[nr].name,', total trigger point no.:');
      oeffnen(nr);
      for tp:=1 to n do begin
         abst:=autom^[t^[tp]+1]-autom^[t^[tp]];
         dehnfaktor:=mittelabstand/abst;
         for j:=0 to maxkanal do if j in kanaele.dabei then
          for i:=0 to rdauer do
           mittel[j]^[i]:=mittel[j]^[i]+dat(zwi(autom^[t^[tp]]+(anfang+i)/dehnfaktor),j);
         inc(tptot); if (tptot mod 128) = 0 then write(tptot:9,#8#8#8#8#8#8#8#8#8);
         if keypressed and (readkey=#27) then begin abbruch:=true; exit end;
         end;
      schliesse;
      end;
   for j:=0 to maxkanal do if j in kanaele.dabei then
      for i:=0 to rdauer do mittel[j]^[i]:=mittel[j]^[i]/gesamt;
   end;
with kanaele do for j:=1 to kn do begin
   rauschen(mittel[k[j]]^,dauer,hoehe[j]);
   maxix[j]:=0; maxi[j]:=0;
   for i:=0 to trunc(dauer) do if maxi[j]<mittel[k[j]]^[i] then begin
      maxi[j]:=mittel[k[j]]^[i]; maxix[j]:=i end;
   flaech:=0;
   for i:=0 to rdauer do flaech:=flaech+mittel[k[j]]^[i];
   flaeche[j]:=(flaech/(rdauer+1)-hoehe[j])*zeit(rdauer+1)/1000;
   end;
end;

procedure grafikphasenaverage.bild;
var   x,y,j:longint;
      fleinheit:einheittyp;

begin
with kanaele do begin
   grafikmittel.bild;
   setlinestyle(dottedln,0,normwidth);
   settextjustify(righttext,toptext);
   for j:=1 to kn do begin
      outtextxy(getmaxx,y0[j]-breite+1,'Base='
         +extwort(extspannung(hoehe[j],k[j]),1,2)+' '
         +belegungsliste[k[j]].einhwort);
      y:=y0[j]+round(kon(hoehe[j],k[j])*faktor);
      line(lrand+1,y,getmaxx,y);
      y:=y0[j]+round(kon(maxi[j],k[j])*faktor);
      x:=round(maxix[j]/stauchung)+lrand+1;
      outtextxy(getmaxx,y0[j]-breite+9,'Maximum='
         +extwort((maxi[j]-hoehe[j])*belegungsliste[k[j]].faktor,1,2)
         +' '+belegungsliste[k[j]].einhwort);
      line(max(lrand+1,x-20),y,min(getmaxx,x+20),y);
      fleinheit:=belegungsliste[k[j]]; inc(fleinheit.sekunde);
      outtextxy(getmaxx,y0[j]-breite+17,'Area='
         +extwort(flaeche[j]*belegungsliste[k[j]].faktor,1,3)+' '
         +fleinheit.einhwort);
      end;
   setlinestyle(solidln,0,normwidth);
   end;
settextjustify(lefttext,centertext);
outtextxy(0,4,'"PHASE DEPENDENT AVERAGE"');
end;

procedure grafikphasenaverage.plot (gr:byte);
var   i:longint;
      fleinheit:einheittyp;

begin
grafikmittel.plot(gr);
if abbruch then exit;
with kanaele do begin
   write(plt,kleinschr,'DI0,1;',plpa(rdauer,0),'CP-2,-2;',
             pllb('PHASE DEPENDENT AVERAGE'),'CP-23,-1;',
             pllb('b'),'CP-1,-1;',pllb('m'),'CP-1,-1;',pllb('a'));
   for i:=kn downto 1 do begin
      fleinheit:=belegungsliste[k[i]]; inc(fleinheit.sekunde);
      write(plt,plpa(rdauer,ganz*(kn-i)),'CP0,-3;',
                pllb(extwort(extspannung(hoehe[i],k[i]),1,2)+' '),
                belegungsliste[k[i]].plot,'CP;',
                pllb(extwort((maxi[i]-hoehe[i])*belegungsliste[k[i]].faktor,1,2)+' '),
                belegungsliste[k[i]].plot,'CP;',
                pllb(extwort(flaeche[i]*belegungsliste[k[i]].faktor,1,3)+' '),
                fleinheit.plot);
      end;
   end;
end;

constructor grafikphasenaverage.aufbauen(var kan:kanalmenge;anf,dau:messwert;
                                         trl:char; trp:grossint; var weis:triggerweiser);
begin
triggweis:=weis;
grafikmittel.aufbauen(kan,anf,dau,trl,trp);
end;

constructor intervallhistogramm.aufbauen(rtl:char; anf,sch:messwert;
                                         din:word; ybe:extended);
begin
grafikintervall.aufbauen(rtl,anf,sch,din,ybe);
end;

procedure intervallhistogramm.berechnen;
var   um:extended;
      tpr:longint;
      d:messwert;
      nr:byte;
begin
gesamt:=tliste[ref]^.triggsum;
if gesamt=0 then begin
   write('No trigger point'); pieps; warte;
   abbruch:=true; exit end;
fillchar(diff^,sizeof(bildfeld),0);
tstat.init;
um:=1/gesamt;
for nr:=1 to filenr do with tliste[ref]^.fil[nr] do
   for tpr:=1 to automn-1 do begin
      d:=autom^[tpr+1]-autom^[tpr];
      if (d<schluss) and (d>=anfang) then begin
         tstat.dazu(d);
         incex(diff^[trunc((d-anfang)/klasse)+1],um);
         end;
      end;
tstat.rechnen;
end;

procedure intervallhistogramm.bild;
var   rand:word;
begin
grafikintervall.bild;
rand:=getmaxx div 2;
setcolor(getmaxcolor); settextjustify(lefttext,centertext);
outtextxy(rand,4, 'Trigger Points: '+wort(gesamt));
outtextxy(rand,14,'Evaluated     : '+wort(tstat.n));
outtextxy(rand,24,'Mean Interval : '+extwort(extzeit(tstat.mx),3,3)+' ms ñ '
                  +extwort(extzeit(tstat.rox),3,3)+' ms');
outtextxy(0,4,'"INTERVAL HISTOGRAM"');
end;

procedure intervallhistogramm.plot (gr:byte);
begin
grafikintervall.plot(gr);
write(plt,kleinschr,plpa(diffn,0),'DI0,1;CP0,-2;');
write(plt,pllb('INTERVAL HISTOGRAM'),'CP;CP;');
write(plt,pllb(' Trigger Points: '+wort(gesamt)),'CP;');
write(plt,pllb(' Evaluated     : '+wort(tstat.n)),'CP;');
write(plt,pllb(' Mean Interval : '+extwort(extzeit(tstat.mx),3,3)+' ms +/- '
                +extwort(extzeit(tstat.rox),3,3)+' ms'));
end;

constructor autokorrelogramm.aufbauen(rtl:char; anf,sch:messwert;
                                      din:word; ybe:extended);
begin
grafikintervall.aufbauen(rtl,anf,sch,din,ybe);
end;

procedure autokorrelogramm.berechnen;
var   um:extended;
      tpr,tpo:longint;
      tpol,tpor,dum:exword;
      d:messwert;
      nr:byte;
begin
gesamt:=tliste[ref]^.triggsum;
if gesamt=0 then begin
   write('No trigger point'); pieps; warte;
   abbruch:=true; exit end;
fillchar(diff^,sizeof(bildfeld),0);
tstat.init;
um:=1/gesamt;
with tliste[ref]^ do for nr:=1 to filenr do with fil[nr] do
 if automda then begin
   write(#13); clreol;
   write('Processing: ',liste[nr].name,', trigger point no.:');
   for tpr:=1 to automn do begin
      such(0,automn+1,autom^[tpr]+anfang,dum,tpol);
      such(dum,automn+1,autom^[tpr]-1+schluss,tpor,dum);
      for tpo:=tpol to tpor do begin
         d:=autom^[tpo]-autom^[tpr];
         tstat.dazu(d);
         incex(diff^[trunc((d-anfang)/klasse)+1],um);
         end;
      if (tpr mod 128) = 0 then write(tpr:7,#8#8#8#8#8#8#8);
      end;
   end;
tstat.rechnen;
end;

procedure autokorrelogramm.bild;
var   rand:word;
begin
grafikintervall.bild;
rand:=getmaxx div 2;
setcolor(getmaxcolor); settextjustify(lefttext,centertext);
outtextxy(rand,4, 'Trigger Points : '+wort(gesamt));
outtextxy(rand,14,'Evaluated      : '+wort(tstat.n));
outtextxy(rand,24,'Mean Time      : '+extwort(extzeit(tstat.mx),3,3)+' ms ñ '
                  +extwort(extzeit(tstat.rox),3,3)+' ms');
outtextxy(0,4,'"AUTO CORRELOGRAM"');
end;

procedure autokorrelogramm.plot (gr:byte);
begin
grafikintervall.plot(gr);
write(plt,kleinschr,plpa(diffn,0),'DI0,1;CP0,-2;');
write(plt,pllb('AUTO CORRELOGRAM'),'CP;CP;');
write(plt,pllb(' Trigger Points: '+wort(gesamt)),'CP;');
write(plt,pllb(' Evaluated     : '+wort(tstat.n)),'CP;');
write(plt,pllb(' Mean Time     : '+extwort(extzeit(tstat.mx),3,3)+' ms +/- '
                +extwort(extzeit(tstat.rox),3,3)+' ms'));
end;

constructor kreuzkorrelogramm.aufbauen(rtl,otl:char; anf,sch:messwert;
                                       din:word; ybe:extended);
begin
obj:=otl;
grafikintervall.aufbauen(rtl,anf,sch,din,ybe);
end;

procedure kreuzkorrelogramm.berechnen;
var   um:extended;
      tpr,tpo:longint;
      tpol,tpor,dum:exword;
      d:messwert;
      nr:byte;
begin
refn:=tliste[ref]^.triggsum;
objn:=tliste[obj]^.triggsum;
if (refn=0) or (objn=0) then begin
   write('Empty trigger list.'); pieps; warte;
   abbruch:=true; exit end;
fillchar(diff^,sizeof(bildfeld),0);
nstat.init; tstat.init;
um:=1/refn;
for nr:=1 to filenr do if tliste[ref]^.fil[nr].automda then
  with tliste[obj]^, fil[nr] do begin
   write(#13); clreol;
   write('Processing: ',liste[nr].name,', trigger point no.:');
   for tpr:=1 to tliste[ref]^.fil[nr].automn do begin
      such(0,automn+1,tliste[ref]^.fil[nr].autom^[tpr]+anfang,dum,tpol);
      such(dum,automn+1,tliste[ref]^.fil[nr].autom^[tpr]-1+schluss,tpor,dum);
      nstat.dazu(tpor+1-tpol);
      for tpo:=tpol to tpor do begin
         d:=autom^[tpo]-tliste[ref]^.fil[nr].autom^[tpr];
         tstat.dazu(d);
         incex(diff^[trunc((d-anfang)/klasse)+1],um);
         end;
      if (tpr mod 128) = 0 then write(tpr:7,#8#8#8#8#8#8#8);
      end;
   end;
nstat.rechnen; tstat.rechnen;
end;

procedure kreuzkorrelogramm.bild;
var   rand:word;
begin
grafikintervall.bild;
rand:=getmaxx div 2;
setcolor(getmaxcolor); settextjustify(lefttext,centertext);
outtextxy(rand,4, 'References: '+wort(refn));
outtextxy(rand,14,'Events    : '+wort(round(nstat.sx))+' ('+wort(objn)+')');
outtextxy(rand,24,'Evt./Ref. : '+extwort(nstat.mx,3,1)+' ñ '+extwort(nstat.rox,3,1));
outtextxy(rand,34,'Mean Time : '+extwort(extzeit(tstat.mx),3,3)+' ms ñ '
                  +extwort(extzeit(tstat.rox),3,3)+' ms');
outtextxy(0,4,'"CROSS CORRELOGRAM"');
end;

procedure kreuzkorrelogramm.plot (gr:byte);
begin
grafikintervall.plot(gr);
write(plt,kleinschr,plpa(diffn,0),'DI0,1;CP0,-2;');
write(plt,pllb('CROSS CORRELOGRAM'),'CP;CP;');
write(plt,pllb(' References    : '+wort(refn))+'CP;');
write(plt,pllb(' Events        : '+wort(round(nstat.sx))+' ('+wort(objn)+')')+'CP;');
write(plt,pllb(' Evt./Ref.     : '+extwort(nstat.mx,3,1)+' +/- '+extwort(nstat.rox,3,1))+'CP;');
write(plt,pllb(' Mean Time     : '+extwort(extzeit(tstat.mx),3,3)+' ms +/- '
                +extwort(extzeit(tstat.rox),3,3)+' ms'));
end;

constructor psthistogramm.aufbauen(rtl,otl:char; aph,eph:shortint;
                                   anf,sch:messwert; din:word; ybe:extended);
begin
obj:=otl;
anfph:=aph; endph:=eph;
grafikintervall.aufbauen(rtl,anf,sch,din,ybe);
end;

procedure psthistogramm.berechnen;
var   um:extended;
      nr:byte;
procedure durchzaehlen (var refliste,objliste:triggerdaten);
var   tpol1,tpol2,tpor1,tpor2,tpol,tpor,dum:exword;
      tpr,tpo:longint;
      d:messwert;
begin
for tpr:=1 to refliste.automn do begin
   objliste.such(0,objliste.automn+1,refliste.autom^[tpr]+anfang,dum,tpol1);
   objliste.such(dum,objliste.automn+1,refliste.autom^[tpr]-1+schluss,tpor1,dum);
   objliste.such(0,objliste.automn+1,refliste.autom^[tpr+anfph],dum,tpol2);
   objliste.such(dum,objliste.automn+1,refliste.autom^[tpr+endph]-1,tpor2,dum);
   tpor:=min(tpor1,tpor2); tpol:=max(tpol1,tpol2);
   nstat.dazu(max(tpor+1-tpol,0));
   for tpo:=tpol to tpor do begin
      d:=objliste.autom^[tpo]-refliste.autom^[tpr];
      tstat.dazu(d);
      incex(diff^[trunc((d-anfang)/klasse)+1],um);
      end;
   if (tpr mod 128) = 0 then write(tpr:7,#8#8#8#8#8#8#8);
   end;
end;

begin
refn:=tliste[ref]^.triggsum;
objn:=tliste[obj]^.triggsum;
if (refn=0) or (objn=0) then begin
   write('Empty trigger list.'); pieps; warte;
   abbruch:=true; exit end;
fillchar(diff^,sizeof(bildfeld),0);
nstat.init; tstat.init;
um:=1/refn;
for nr:=1 to filenr do if tliste[ref]^.fil[nr].automda then begin
   write(#13); clreol;
   write('Processing: ',liste[nr].name,', trigger point no.:');
   durchzaehlen(tliste[ref]^.fil[nr],tliste[obj]^.fil[nr]);
   end;
nstat.rechnen; tstat.rechnen;
end;

procedure psthistogramm.bild;
var   rand:word;
begin
grafikintervall.bild;
rand:=getmaxx div 2;
setcolor(getmaxcolor); settextjustify(lefttext,centertext);
outtextxy(rand,4, 'References: '+wort(refn));
outtextxy(rand,14,'Events    : '+wort(round(nstat.sx))+' ('+wort(objn)+')');
outtextxy(rand,24,'Evt./Ref. : '+extwort(nstat.mx,3,1)+' ñ '+extwort(nstat.rox,3,1));
outtextxy(rand,34,'Mean Time : '+extwort(extzeit(tstat.mx),3,3)+' ms ñ '
                  +extwort(extzeit(tstat.rox),3,3)+' ms');
outtextxy(0,4,'"PST HISTOGRAM"');
end;

procedure psthistogramm.plot (gr:byte);
begin
grafikintervall.plot(gr);
write(plt,kleinschr,plpa(diffn,0),'DI0,1;CP0,-2;');
write(plt,pllb('PST HISTOGRAM'),'CP;CP;');
write(plt,pllb(' References    : '+wort(refn))+'CP;');
write(plt,pllb(' Events        : '+wort(round(nstat.sx))+' ('+wort(objn)+')')+'CP;');
write(plt,pllb(' Evt./Ref.     : '+extwort(nstat.mx,3,1)+' +/- '+extwort(nstat.rox,3,1))+'CP;');
write(plt,pllb(' Mean Time     : '+extwort(extzeit(tstat.mx),3,3)+' ms +/- '
                +extwort(extzeit(tstat.rox),3,3)+' ms'));
end;

constructor latenzhistogramm.aufbauen(rtl,otl:char; anf,sch:messwert;
                                      din:word; ybe:extended);
begin
obj:=otl;
grafikintervall.aufbauen(rtl,anf,sch,din,ybe);
end;

procedure latenzhistogramm.berechnen;
var   um:extended;
      tpr:longint;
      tpo,dum:exword;
      d:messwert;
      nr:byte;
begin
refn:=tliste[ref]^.triggsum;
if (refn=0) or (tliste[obj]^.triggsum=0) then begin
   write('Empty trigger list.'); pieps; warte;
   abbruch:=true; exit end;
fillchar(diff^,sizeof(bildfeld),0);
tstat.init;
um:=1/refn;
for nr:=1 to filenr do if tliste[ref]^.fil[nr].automda then
  with tliste[obj]^, fil[nr] do begin
   write(#13); clreol;
   write('Processing: ',liste[nr].name,', trigger point no.:');
   for tpr:=1 to tliste[ref]^.fil[nr].automn do begin
      such(0,automn+1,tliste[ref]^.fil[nr].autom^[tpr]+anfang,dum,tpo);
      d:=autom^[tpo]-tliste[ref]^.fil[nr].autom^[tpr];
      if d<=schluss then begin
         tstat.dazu(d);
         incex(diff^[trunc((d-anfang)/klasse)+1],um);
         end;
      if (tpr mod 128) = 0 then write(tpr:7,#8#8#8#8#8#8#8);
      end;
   end;
tstat.rechnen;
end;

procedure latenzhistogramm.bild;
var   rand:word;
begin
grafikintervall.bild;
rand:=getmaxx div 2;
setcolor(getmaxcolor); settextjustify(lefttext,centertext);
outtextxy(rand,4, 'References: '+wort(refn));
outtextxy(rand,14,'Events    : '+wort(tstat.n));
outtextxy(rand,24,'Latency   : '+extwort(extzeit(tstat.mx),3,3)+' ms ñ '
                  +extwort(extzeit(tstat.rox),3,3)+' ms');
outtextxy(0,4,'"LATENCY HISTOGRAM"');
end;

procedure latenzhistogramm.plot (gr:byte);
begin
grafikintervall.plot(gr);
write(plt,kleinschr,plpa(diffn,0),'DI0,1;CP0,-2;');
write(plt,pllb('LATENCY HISTOGRAM'),'CP;CP;');
write(plt,pllb(' References    : '+wort(refn)),'CP;');
write(plt,pllb(' Events        : '+wort(tstat.n)),'CP;');
write(plt,pllb(' Latency       : '+extwort(extzeit(tstat.mx),3,3)+' ms +/- '
                +extwort(extzeit(tstat.rox),3,3)+' ms'));
end;

{ phasenhistogramm }

constructor phasenhistogramm.aufbauen(rtl,otl:char; minabst,maxabst:messwert;
                                      anfph,schph:extended; din:word; ybe:extended);
begin
obj:=otl;
weis.zaehlen(tliste[rtl]^,minabst,maxabst);
inherited aufbauen(rtl,anfph,1-frac(1-frac(schph-anfph+1)),din,ybe);
weis.frei;
end;

procedure phasenhistogramm.berechnen;
var   mklasse,um,faktor:extended;
      tpr,tpo,versch:longint;
      tpol,tpor,dum:exword;
      nr:byte;
      abst,d:messwert;
begin
mklasse:=weis.mittelabstand*klasse;
if weis.gesamt=0 then begin
   write('No trigger point.'); pieps; warte; abbruch:=true; exit end;
refn:=tliste[ref]^.triggsum; objn:=tliste[obj]^.triggsum;
fillchar(diff^,sizeof(bildfeld),0);
versch:=round(anfphase*diffnganz); anfphase:=versch/diffnganz;
nstat.init; phstat.init;
um:=1/weis.gesamt;
for nr:=1 to filenr do
  with tliste[ref]^.fil[nr], weis.weisliste[nr] do if automda then begin
   write(#13); clreol;
   for tpr:=1 to n do begin
      abst:=autom^[t^[tpr]+1]-autom^[t^[tpr]];
      faktor:=weis.mittelabstand/abst;
      tliste[obj]^.fil[nr].such(0,tliste[obj]^.fil[nr].automn+1,
           autom^[t^[tpr]],dum,tpol);
      tliste[obj]^.fil[nr].such(dum,tliste[obj]^.fil[nr].automn+1,
           autom^[t^[tpr]]+abst-1,tpor,dum);
      nstat.dazu(tpor+1-tpol);
      for tpo:=tpol to tpor do begin
       d:=tliste[obj]^.fil[nr].autom^[tpo]-autom^[t^[tpr]];
       if phasentest(d/abst,anfphase,dauerphase) then phstat.dazu(d/abst*2*pi);
       incex(diff^[(trunc(faktor*d/mklasse)-versch+diffnganz) mod diffnganz +1],um);
       end;
      end;
   end;
nstat.rechnen; phstat.rechnen;
end;

procedure phasenhistogramm.bild;
var   rand:word;
begin
inherited bild;
rand:=getmaxx div 2;
setcolor(getmaxcolor); settextjustify(lefttext,centertext);
outtextxy(rand,4, 'Ref.þ Evt.: '+wort(weis.gesamt)+' ('+wort(refn)+') þ '
                  +wort(round(nstat.sx))+' ('+wort(objn)+')');
outtextxy(rand,14,'Evt./Ref. : '+extwort(nstat.mx,3,1)+' ñ '+extwort(nstat.rox,3,1));
outtextxy(rand,24,'Cycle Dur.: '+extwort(extzeit(weis.mittelabstand),3,3)+' ms');
outtextxy(rand,34,'Phase     : '+extwort(phstat.mph/2/pi,3,3)
                  +' (l='+extwort(phstat.lvek,4,2)+')');
outtextxy(0,4,'"PHASE HISTOGRAM"');
end;

procedure phasenhistogramm.plot (gr:byte);
begin
inherited plot(gr);
write(plt,kleinschr,plpa(diffn,0),'DI0,1;CP0,-2;');
write(plt,pllb('PHASE HISTOGRAM'),'CP;CP;');
write(plt,pllb(' Ref., Evt.    : '+wort(weis.gesamt)+' ('+wort(refn)+') , '
                  +wort(round(nstat.sx))+' ('+wort(objn)+')')+'CP;');
write(plt,pllb(' Evt./Ref.     : '+extwort(nstat.mx,3,1)+' +/- '
               +extwort(nstat.rox,3,1))+'CP;');
write(plt,pllb(' Cycle Dur.    : '+extwort(extzeit(weis.mittelabstand),3,3)+' ms')+'CP;');
write(plt,pllb(' Phase         : '+extwort(phstat.mph/2/pi,3,3)
                  +' (l='+extwort(phstat.lvek,4,2)+')'));
end;

end.
