{Turbo-Pascal 6.0}

unit tlfiles;

{$A+,B-,F-,G+,I-,N+,O-,V-,X-}

interface

uses  crt,bequem,objects,tulab,tlfilter;

const maxfiles=20;
      maxmesswert=2147483648.0;

type  messwert=double;                   wert=single;

      listenzeiger=^blockliste;
      blockliste=object (tobject)
         next,vor:listenzeiger; von,bis:messwert;
         constructor neu;
         procedure store (var s:tbufstream);
         constructor load (var s:tbufstream);
         end;

      punktzeiger=^punktliste;
      punktliste=object (tobject)
         next,vor:punktzeiger; bei:messwert;
         constructor neu;
         procedure store (var s:tbufstream);
         constructor load (var s:tbufstream);
        end;

      auflistenzeiger=^listenzeiger;
      aufpunktzeiger=^punktzeiger;
      listenfeld=object
         name:string[80];     ko:kopfdaten;
         laenge:messwert;
         block:auflistenzeiger; selbst:aufpunktzeiger;
         procedure neuzeiger;
         procedure loeschzeiger;
         procedure store (var s:tbufstream);
         procedure load (var s:tbufstream);
         end;
      listentyp=array[1..maxfiles] of listenfeld;

const kan:byte=0;
      fre:extended=0;
      filenr:byte=0;

var   liste:listentyp;
      korr:extended;

procedure oeffnen (nr:byte);

function zwi (stelle:messwert):longint;

function zeit (stelle:messwert):longint;
function messw (zeitang:longint):messwert;

procedure rein (var zeiger:listenzeiger);
procedure raus (var zeiger:listenzeiger);
procedure prein (var zeiger:punktzeiger);
procedure praus (var zeiger:punktzeiger);

procedure fileliste;

procedure zeigertest;

procedure streamput (var s:tbufstream);
procedure streamget (var s:tbufstream);

implementation

const anfblock:blockliste=(next:nil; vor:nil; von:-maxmesswert; bis:-maxmesswert);
      endblock:blockliste=(next:nil; vor:nil; von:maxmesswert;  bis:maxmesswert);

      anfpunkt:punktliste=(next:nil; vor:nil; bei:-maxmesswert);
      endpunkt:punktliste=(next:nil; vor:nil; bei:maxmesswert);

      rblockliste:tstreamrec=(objtype:200;
                              vmtlink:ofs(typeof(blockliste)^);
                              load:@blockliste.load;
                              store:@blockliste.store);

      rpunktliste:tstreamrec=(objtype:201;
                              vmtlink:ofs(typeof(punktliste)^);
                              load:@punktliste.load;
                              store:@punktliste.store);

var   i:byte;

procedure oeffnen (nr:byte);
begin
with liste[nr] do begin
   oeffne(name,ko.freq);
   korr:=ko.freq/fre;
   end;
end;

function zwi (stelle:messwert):longint;
begin
zwi:=round(stelle*korr);
end;

function zeit (stelle:messwert):longint;
begin
zeit:=round(stelle/fre*1000);
end;

function messw (zeitang:longint):messwert;
begin
messw:=zeitang*fre/1000;
end;

procedure listenfeld.neuzeiger;
var   hilf:listenzeiger;
      philf:punktzeiger;
begin
new(hilf,neu); hilf^:=anfblock;
new(hilf^.next,neu); hilf^.next^:=endblock; hilf^.next^.vor:=hilf;
block:=addr(hilf^.next);
new(philf,neu); philf^:=anfpunkt;
new(philf^.next,neu); philf^.next^:=endpunkt; philf^.next^.vor:=philf;
selbst:=addr(philf^.next);
end;

procedure rein (var zeiger:listenzeiger);
var hilf:listenzeiger;
begin
new(hilf,neu);
hilf^.next:=zeiger; hilf^.vor:=zeiger^.vor;
zeiger^.vor:=hilf; hilf^.vor^.next:=hilf;
zeiger:=hilf;
end;

procedure raus (var zeiger:listenzeiger);
var hilf:listenzeiger;
begin
hilf:=zeiger; zeiger:=hilf^.next;
hilf^.vor^.next:=hilf^.next; hilf^.next^.vor:=hilf^.vor;
dispose(hilf,done);
end;

procedure prein (var zeiger:punktzeiger);
var hilf:punktzeiger;
begin
new(hilf,neu);
hilf^.next:=zeiger; hilf^.vor:=zeiger^.vor;
zeiger^.vor:=hilf; hilf^.vor^.next:=hilf;
zeiger:=hilf;
end;

procedure praus (var zeiger:punktzeiger);
var hilf:punktzeiger;
begin
hilf:=zeiger; zeiger:=hilf^.next;
hilf^.vor^.next:=hilf^.next; hilf^.next^.vor:=hilf^.vor;
dispose(hilf,done);
end;

procedure listenfeld.loeschzeiger;
var   hilf:listenzeiger;
      philf:punktzeiger;
begin
hilf:=block^;
while hilf^.next<>nil do raus(hilf);
dispose(hilf^.vor); dispose(hilf);
philf:=selbst^;
while philf^.next<>nil do praus(philf);
dispose(philf^.vor); dispose(philf);
end;

constructor blockliste.neu;
begin end;

procedure blockliste.store (var s:tbufstream);
begin
s.write(von,sizeof(messwert)); s.write(bis,sizeof(messwert));
s.put(next);
end;

constructor blockliste.load (var s:tbufstream);
begin
s.read(von,sizeof(messwert)); s.read(bis,sizeof(messwert));
next:=listenzeiger(s.get);
if next<>nil then next^.vor:=@self;
end;

constructor punktliste.neu;
begin end;

procedure punktliste.store (var s:tbufstream);
begin
s.write(bei,sizeof(messwert));
s.put(next);
end;

constructor punktliste.load (var s:tbufstream);
begin
s.read(bei,sizeof(messwert));
next:=punktzeiger(s.get);
if next<>nil then next^.vor:=@self;
end;

procedure listenfeld.store (var s:tbufstream);
begin
s.write(name,sizeof(name)); s.write(ko,sizeof(kopfdaten));
s.write(laenge,sizeof(messwert));
s.put(block^);
s.put(selbst^);
end;

procedure listenfeld.load (var s:tbufstream);
var   hilf:listenzeiger; philf:punktzeiger;
begin
s.read(name,sizeof(name)); s.read(ko,sizeof(kopfdaten));
s.read(laenge,sizeof(messwert));
new(hilf,neu); hilf^:=anfblock;
block:=addr(hilf^.next);
hilf^.next:=listenzeiger(s.get);
hilf^.next^.vor:=hilf;
new(philf,neu); philf^:=anfpunkt;
selbst:=addr(philf^.next);
philf^.next:=punktzeiger(s.get);
philf^.next^.vor:=philf;
end;

procedure fileliste;
var   i:byte;
begin
writeln('  File-Liste');
for i:=1 to min(filenr,10) do begin
   write(i:3,' :  ',liste[i].name);
   gotoxy(40,wherey); if i+10<=filenr then write(i+10:3,' : ',liste[i+10].name);
   writeln end;
end;

procedure streamput (var s:tbufstream);
var   ind:byte;
begin
s.write(kan,1);              s.write(fre,sizeof(extended));
s.write(filenr,1);
for ind:=1 to filenr do liste[ind].store(s);
end;

procedure streamget (var s:tbufstream);
var   ind:byte;
begin
s.read(kan,1);               s.read(fre,sizeof(extended));
s.read(filenr,1);
for ind:=1 to filenr do liste[ind].load(s);
end;

procedure zeigertest;
var   i:byte;
      hilf:listenzeiger;
      philf:punktzeiger;
begin
writeln(lfcr,'Zeigerselbsttest',lfcr);
for i:=1 to filenr do begin
   write('> Filenr: ',i,'  ');
   write('>> Bl”cke  ');
   hilf:=liste[i].block^;
   if hilf^.vor^.next<>hilf then begin
      writeln('### Fehler am Anfang'); warte; exit end;
   while hilf^.next<>nil do begin
    write(hilf^.von:1:0,' ',hilf^.bis:1:0,' ');
    if hilf^.next^.vor<>hilf then begin
       writeln('### Fehler'); warte; exit end;
    hilf:=hilf^.next;
    end;
   write('>> Punkte ');
   philf:=liste[i].selbst^;
   if philf^.vor^.next<>philf then begin
      writeln('### Fehler am Anfang'); warte; exit end;
   while philf^.next<>nil do begin
    write(philf^.bei:1:0,' ');
    if philf^.next^.vor<>philf then begin
       writeln('### Fehler'); warte; exit end;
    philf:=philf^.next;
    end;
   writeln;
   end;
writeln(lfcr,'Alle Zeiger in Ordnung.'); warte;
end;

begin
registertype(rblockliste);
registertype(rpunktliste);
end.