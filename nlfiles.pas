{ Borland-Pascal 7.0 }

unit nlfiles;

{$IFDEF MSDOS}
{$A+,B-,F-,G-,I-,N+,O-,P+,T+,V+,X-}
{$ELSE}
{$A+,B-,F-,G+,I-,N+,P+,T+,V+,X-}
{$ENDIF}

interface

uses   crt,               daff,wavpcm,tulab42, nlrahmen,           plotter,
       dos,               tlfilter,            nltrigg,
       bequem,            tlfiles,
       objects;

procedure manager;

implementation

const leername='   -';

var   filind:byte;

procedure voreinstellung;
begin
ueberschrift(false,'Conditions for file sets','Dialogue',farbe3);
if filenr=0 then begin
   fre:=readext('  maximum sampling rate [Hz]',fre,1,0);
   kan:=readint('maximum number of channels',kan);
   if not (kan in [0..maxkan]) then begin
      writeln; fehler('Maximum of channels:'+wort(maxkan));
      kan:=0; warte; exit end;
   if kan>0 then begin neukan(kan); kanaele.voreinstellung end;
   end      else begin
   fehler('Define conditions before opening a file.'); warte;
   end;
end;

procedure proto (nr:byte);
var   wy:byte;
procedure list (von:byte);
var   i:byte;
begin
with liste[nr].ko do begin
   if von<=kan-1 then writeln('Nl# TL#','Label':14,'Factor':11,'Unit':7);
   for i:=von to min(von+7,kan-1) do
      writeln(i:3,k[i].nr:3,k[i].name:15,
              extewort(10/(k[i].faktor1*k[i].faktor2)/maxsample,2,2):11,
              copy(k[i].einheit,1,6):7);
   end;
end;
begin
with liste[nr], ko do
   writeln('Protocol: ',kennung,lfcr,'Producer :',produzent,
           '  Date: ',datum,'  Duration: ',zeit(laenge),
           ' ms  Rate: ',freq:5:2,' Hz');
wy:=wherey+hi(windmin);
window(1,wy,40,wy+9); list(0);
window(41,wy,80,wy+9); list(8);
window(1,1,80,25); gotoxy(1,wy+10);
end;

procedure neufile;
var   ganz:pathstr;
      i:byte;
      hilf:listenzeiger; philf:punktzeiger;
      j:char;

begin
ueberschrift(false,'Open File','Dialogue',farbe3);
with liste[filenr+1], ko do begin
   window(1,3,80,8);
   name:=readstring('File name','');
   fsplit(name,named,namen,namee); if namee='' then namee:='.DAT';
   name:=named+namen+namee;
   writeln;
   ganz:=fexpand(name);
   for i:=1 to filenr do if ganz=fexpand(liste[i].name) then begin
      writeln('Identical with no.',i,' : ',liste[i].name,'.');
      if not (readcharim('Open anyway? (Y/N) ','N') in ['J','j','Y','y']) then begin
         name:=leername; exit end;
      end;
   kopf(name,ko); if tulabfehler then begin warte; name:=leername; exit end;
   if kan=0 then begin kan:=nkan; neukan(kan); kanaele.voreinstellung end;
   if fre=0 then fre:=freq
            else if freq>1.1*fre then begin
               name:=leername;
               fehler('Sampling rate of file too large.'); warte; exit end;
   if filenr=0 then beschriftungen(ko);
   korr:=freq/fre; laenge:=anzahl/korr;
   window(1,8,80,25);
   zwischen('Info',farbe3); writeln;
   proto(filenr+1);
   neuzeiger;
   end;
inc(filenr);
einheitensetzen(fre);
window(1,23,80,25);
zwischen('Dialogue',farbe3);
warte end;

procedure protokoll;
var   i:longint;
begin
clrscr;
zwischen('Dialogue',farbe3);
window(1,18,80,25);
i:=readint('Protocol of file no.',1);
if not (i in [1..filenr]) then begin
   writeln; fehler('Undefined file no.'); warte; exit end;
ueberschrift(false,'Protocol','Info',farbe3);
gotoxy(1,3); writeln('File: ',fexpand(liste[i].name),lfcr);
proto(i);
gotoxy(1,22); zwischen('Dialogue',farbe3);
warte;
end;

procedure fildir;
var   direc:pathstr;
      such:searchrec;
      di:dirstr; na:namestr; ex:extstr;
procedure ausgabe(var such:searchrec);
var   d:dirstr; n:namestr; e:extstr;
      nl:byte absolute n; el:byte absolute e;
begin
with such do begin
   fsplit(name,d,n,e); delete(e,1,1);
   write(n,'':9-nl,e,'':7-el);
   end;
end;
begin
clrscr;
zwischen('Dialogue',farbe3);
window(1,18,80,25);
direc:=fexpand(readstring('Directory path',fexpand('*.dat')));
fsplit(direc,di,na,ex);
if na='' then na:='*'; if ex='' then ex:='.dat'; direc:=di+na+ex;
findfirst(direc,anyfile,such);
writeln;
case doserror of
   0:begin
      ueberschrift(false,'Directory','Info',farbe3);
      writeln('Directory of ',direc,lfcr);
      repeat
         ausgabe(such);
         findnext(such);
      until doserror=18;
      window(1,3,80,20);
      window(1,23,80,25);
      zwischen('Dialogue',farbe3);
     end;
   18:fehler('No entry under '+direc+'.');
   else fehler('Directory not found.');
   end;
warte;
end;

procedure schliessen;
var   nr:longint;
      ti:char;
      zwi:auflistenzeiger; pzwi:aufpunktzeiger;
begin
clrscr;
zwischen('Dialogue',farbe3);
window(1,18,80,25);
nr:=readint('Close file no.',0);
if not (nr in [1..filenr]) then begin
   writeln; fehler('Undefined file no.'); warte; exit end;
dec(filenr);
if filenr=0 then begin fre:=0; kan:=0 end;
zwi:=liste[nr].block; pzwi:=liste[nr].selbst;
for ti:='A' to listmax do tliste[ti]^.fil[nr].frei;
for nr:=nr to filenr do begin
   liste[nr]:=liste[nr+1];
   for ti:='A' to listmax do with tliste[ti]^ do fil[nr]:=fil[nr+1];
   end;
liste[filenr+1].block:=zwi; liste[filenr+1].selbst:=pzwi;
liste[filenr+1].name:=leername;
liste[filenr+1].loeschzeiger;
for ti:='A' to listmax do with tliste[ti]^.fil[filenr+1] do begin
   automn:=0; automda:=false end;
end;

procedure kompression;
var   outname:string80;
      ko:kopfdaten;
      tzaehler,j,i:longint;
      nr,result:word;
      wandert:listenzeiger;

begin
clrscr;
zwischen('Dialogue',farbe3);
window(1,18,80,25);
outname:=readstring('Output file name','select.dat');
if fileschonda(outname) then
    if upcase(readchar('Overwrite? (Y/N)','N'))<>'Y' then exit;
ko.kennung:=readstring('Protocol','Selected Data File');
ueberschrift(false,'Selected Data File','Info',farbe3);
clrscr; kanaele.lesen(6,farbe3);
if kanaele.kn=0 then exit;
if not (upcase(readchar('Start? (Y/N)','Y')) in ['Y','J']) then begin
   fehler('No file created.'); warte; exit end;
with ko do begin
   produzent:='NEUROLAB';
   freq:=fre;
   nkan:=kanaele.kn;
   i:=0;
   for j:=0 to maxkanal do if j in kanaele.dabei then begin
      ko.k[i].name:=schriftliste[j];
      ko.k[i].faktor1:=belegungsliste[j].faktor;
      ko.k[i].faktor2:=1;
      i:=i+1;
      end;
   end;
kopfplatzschreiben(outname);
assign(seqdaten,outname);
seqoeffne;
for nr:=1 to filenr do with liste[nr] do begin
   wandert:=block^;
   oeffnen(nr);
   while wandert^.next<>nil do begin
      for tzaehler:=trunc(wandert^.von+1) to trunc(wandert^.bis) do
         for j:=0 to maxkanal do if j in kanaele.dabei then begin
            seqschreibe(dat(zwi(tzaehler),j));
            end;
      wandert:=wandert^.next;
      end;
   schliesse;
   end;
seqschliesse;
kopfschreiben(outname,ko);
end;

procedure manager;
var   i:byte;
begin
repeat
  ueberschrift(false,'File Manager','Info',farbe2);
  writeln('Open files:');
  window(1,5,40,15);
  for i:=1 to 10 do writeln(i:3,' : ',liste[i].name);
  window(41,5,80,15);
  for i:=11 to 20 do writeln(i:3,' : ',liste[i].name);
  window(1,16,80,25);
  zwischen('Menu',farbe2);
  writeln;
  writeln('  d...Directory of Data Files           s...Conditions for File Sets',lfcr,
          '  o...Open File                         c...Close File',lfcr,
          '  p...Protocol of File                  f...Create Selected Data File',lfcr,
          '  m...Main Menu',lfcr);
  zwischen('Dialogue',farbe2);
  writeln;
  if filenr=0 then begin
     case upcase(readcharim('Menu Point','o')) of
        'O':neufile;
        'S':voreinstellung;
        'D':fildir;
        'P','C','F':begin
           window(1,16,80,25); clrscr; zwischen('Dialogue',farbe2);
           fehler('No file open: Start with <o> or <s>.'); writeln;
           pieps; warte end;
        'M':exit;
        end;
     end      else begin
     case upcase(readcharim('Menu Point','m')) of
       'D':fildir;
       'O':neufile;
       'P':protokoll;
       'C':schliessen;
       'S':voreinstellung;
       'F':kompression;
       'M':exit;
       end;
    end;
until false;
end;

begin
for filind:=1 to 20 do liste[filind].name:=leername;
end.