{Turbo-Pascal 6.0}

unit nlfiles;

{$A+,B-,F-,G+,I+,N+,O-,V-,X-}

interface

uses   crt,               tulab,              nlrahmen,
       dos,               tlfilter,           nltrigg,
       bequem,            tlfiles;

procedure manager;

implementation

const leername='   -';

var   filind:byte;

procedure voreinstellung;
begin
ueberschrift('Voreinstellung der Rahmendaten','Dialog',farbe3);
if filenr=0 then begin
   fre:=readext('  maximale Abtastrate [Hz]',fre,1,0);
   kan:=readint('maximale Anzahl der KanÑle',kan);
   if not (kan in [0..maxkan]) then begin
      writeln; fehler('Maximal '+wort(maxkan)+' KanÑle zugelassen.');
      kan:=0; warte; exit end;
   if kan>0 then begin neukan(kan); kanaele.voreinstellung end;
   end      else begin
   fehler('Nur vor dem ôffnen des ersten Files erlaubt.'); warte;
   end;
end;

procedure proto (nr:byte);
var   wy:byte;
procedure list (von:byte);
var   i:byte;
begin
with liste[nr], ko do begin
   if von<=kan-1 then writeln('Kanal':8,'Belegung':14,'Gain':9);
   for i:=von to min(von+7,kan-1) do
      writeln(i:6,names[i]:16,gain[i]:7);
   end;
end;
begin
with liste[nr], ko do
   writeln('Protokoll: ',kennung,lfcr,
           'Datum: ',datum,'   Dauer: ',zeit(laenge),' ms   Rate: ',freq:5:2,' Hz');
wy:=wherey+hi(windmin);
window(1,wy,40,wy+9); list(0);
window(41,wy,80,wy+9); list(8);
window(1,1,80,25); gotoxy(1,wy+10);
end;

procedure neufile;
var   ganz:pathstr;
      i:byte;
      dstr:dirstr; nstr:namestr; estr:extstr;
      hilf:listenzeiger; philf:punktzeiger;
      j:char;

begin
ueberschrift('Neues File','Dialog',farbe3);
with liste[filenr+1], ko do begin
   window(1,3,80,8);
   name:=readstring('Filename','');
   fsplit(name,dstr,nstr,estr); if estr='' then estr:='.DAT'; name:=dstr+nstr+estr;
   writeln;
   ganz:=fexpand(name);
   for i:=1 to filenr do if ganz=fexpand(liste[i].name) then begin
      writeln('Identisch mit Nr.',i,' : ',liste[i].name,'.');
      if not (readchar('Trotzdem îffnen? ','N') in ['J','j','Y','y']) then begin
      name:=leername; exit end;
      end;
   kopf(name,ko); if tulabfehler then begin warte; name:=leername; exit end;
   if kan=0 then begin kan:=nkan; neukan(kan); kanaele.voreinstellung end;
   if fre=0 then fre:=freq
            else if freq>1.1*fre then begin
               name:=leername;
               fehler('Abtastrate des Files zu gro·.'); warte; exit end;
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
zwischen('Dialog',farbe3);
warte end;

procedure protokoll;
var   i:longint;
begin
clrscr;
zwischen('Dialog',farbe3);
window(1,18,80,25);
i:=readint('Protokoll von File Nr.',1);
if not (i in [1..filenr]) then begin
   writeln; fehler('UnzulÑssige File-Nr.'); warte; exit end;
ueberschrift('Protokoll','Info',farbe3);
gotoxy(1,3); writeln('File: ',fexpand(liste[i].name),lfcr);
proto(i);
gotoxy(1,22); zwischen('Dialog',farbe3);
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
zwischen('Dialog',farbe3);
window(1,18,80,25);
direc:=fexpand(readstring('Directory - Pfad',fexpand('*.dat')));
fsplit(direc,di,na,ex);
if na='' then na:='*'; if ex='' then ex:='.dat'; direc:=di+na+ex;
findfirst(direc,anyfile,such);
writeln;
case doserror of
   0:begin
      ueberschrift('Directory','Info',farbe3);
      writeln('Directory von ',direc,lfcr);
      repeat
         ausgabe(such);
         findnext(such);
      until doserror=18;
      window(1,3,80,20);
      window(1,23,80,25);
      zwischen('Dialog',farbe3);
     end;
   18:fehler('Kein Eintrag unter '+direc);
   else fehler('Directory falsch');
   end;
warte;
end;

procedure schliessen;
var   nr:longint;
      ti:char;
      zwi:auflistenzeiger; pzwi:aufpunktzeiger;
begin
clrscr;
zwischen('Dialog',farbe3);
window(1,18,80,25);
nr:=readint('Schlie·en von File Nr.',1);
if not (nr in [1..filenr]) then begin
   writeln; fehler('UnzulÑssige File-Nr.'); warte; exit end;
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

procedure manager;
var   i:byte;
begin
repeat
  ueberschrift('File-Manager','Info',farbe2);
  writeln('offene Files:');
  window(1,5,40,15);
  for i:=1 to 10 do writeln(i:3,' : ',liste[i].name);
  window(41,5,80,15);
  for i:=11 to 20 do writeln(i:3,' : ',liste[i].name);
  window(1,16,80,25);
  zwischen('MenÅ',farbe2);
  writeln;
  writeln('  d...Directory der Datenfiles          v...Voreinstellung bei Fileserien',lfcr,
          '  n...Neuerîffnung eines Files          s...File schlie·en',lfcr,
          '  p...Protokoll eines Files             h...HauptmenÅ',lfcr);
  zwischen('Dialog',farbe2);
  writeln;
  if filenr=0 then begin
     case upcase(readchar('MenÅpunkt','n')) of
        'N':neufile;
        'V':voreinstellung;
        'D':fildir;
        'P','S':begin
           window(1,16,80,25); clrscr; zwischen('Dialog',farbe2);
           fehler('Kein File offen: Beginne mit "n" oder "v".'); writeln;
           pieps; warte end;
        'H':exit;
        end;
     end      else begin
     case upcase(readchar('MenÅpunkt','h')) of
       'D':fildir;
       'N':neufile;
       'P':protokoll;
       'S':schliessen;
       'V':voreinstellung;
       'H':exit;
       end;
    end;
until false;
end;

begin
for filind:=1 to 20 do liste[filind].name:=leername;
end.