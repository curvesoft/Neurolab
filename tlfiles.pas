{ Borland-Pascal 7.0 / FPC 2.0}

unit tlfiles;

{$IFDEF MSDOS}
{$A+,B-,E+,F-,G-,I-,N+,O-,P+,T+,V+,X-}
{$ELSE}
{$A+,B-,E+,F-,G+,I-,N+,P+,T+,V+,X-}
{$ENDIF}

interface

uses  crt, dos,
      objects, bequem,daff,wavpcm,tulab42,tlfilter;

const maxfiles=20;
      {$ifdef fpc} maxmesswert=9223372036854775807.0; {$else} maxmesswert=2147483648.0; {$endif}

type  {$ifdef fpc} messwert=extended; wert=double; {$else} messwert=double; wert=single; {$endif}

      { Bei den beiden Listen "blockliste" und "punktliste" wird dieselbe Zeigerstruktur }
      { verwendet. Am Anfang und am Ende der Liste wird jeweils ein zusÑtzliches Element }
      { angehÑngt, das bei jeder Suche in der Liste zu klein bzw. zu gro· ist. Die Werte }
      { hierfÅr stammen aus den Konstanten anf.. bzw. end... . Erreicht wird die Liste   }
      { durch einen Zeiger (auf...zeiger), der auf den next-Zeiger (nicht auf das Listen-}
      { element) des Anfangselementes zeigt.                                             }

      auflistenzeiger=^listenzeiger;
      listenzeiger=^blockliste;
      blockliste=object (tobject)
         next,vor:listenzeiger; von,bis:messwert;
         constructor neu;
         constructor anfblock;
         constructor endblock;
         procedure store (var s:tbufstream);
         constructor load (var s:tbufstream);
         end;

      aufpunktzeiger=^punktzeiger;
      punktzeiger=^punktliste;
      punktliste=object (tobject)
         next,vor:punktzeiger; bei:messwert;
         constructor neu;
         constructor anfpunkt;
         constructor endpunkt;
         procedure store (var s:tbufstream);
         constructor load (var s:tbufstream);
        end;

      listenfeld=object
         name:string[80];
         named:dirstr; namen:namestr; namee:extstr;
         ko:kopfdaten;
         laenge:messwert;
         block:auflistenzeiger; selbst:aufpunktzeiger;
         procedure neuzeiger;
         procedure loeschzeiger;
         procedure store (var s:tbufstream);
         procedure load (var s:tbufstream);
         end;
      listentyp=packed array[1..maxfiles] of listenfeld;

const kan:byte=0;
      fre:extended=0;
      filenr:byte=0;

var   liste:listentyp;
      offennr:byte;
      korr:extended;

procedure oeffnen (nr:byte);

function zwi (stelle:messwert):grossint;

function extzeit (stelle:messwert):extended;
function zeit (stelle:messwert):grossint;
function messwext (zeitang:extended):messwert;
function messw (zeitang:grossint):messwert;

procedure rein (var zeiger:listenzeiger);
procedure raus (var zeiger:listenzeiger);
procedure prein (var zeiger:punktzeiger);
procedure praus (var zeiger:punktzeiger);

procedure fileliste;

procedure zeigertest;

procedure streamput (var s:tbufstream);
procedure streamget (var s:tbufstream);

implementation

{const anfblock:blockliste=(next:nil; vor:nil; von:-maxmesswert; bis:-maxmesswert);
      endblock:blockliste=(next:nil; vor:nil; von:maxmesswert;  bis:maxmesswert);

      anfpunkt:punktliste=(next:nil; vor:nil; bei:-maxmesswert);
      endpunkt:punktliste=(next:nil; vor:nil; bei:maxmesswert);}

const rblockliste:tstreamrec=(objtype:200;
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
offennr:=nr;
with liste[nr] do begin
   oeffne(name,ko);
   korr:=ko.freq/fre;
   end;
end;

function zwi (stelle:messwert):grossint;
begin
zwi:=round(stelle*korr);
end;

function extzeit (stelle:messwert):extended;
begin
extzeit:=stelle/fre*1000;
end;

function zeit (stelle:messwert):grossint;
begin
zeit:=round(extzeit(stelle));
end;

function messwext (zeitang:extended):messwert;
begin
messwext:=zeitang*fre/1000;
end;

function messw (zeitang:grossint):messwert;
begin
messw:=messwext(zeitang);
end;

procedure listenfeld.neuzeiger;
var   hilf:listenzeiger;
      philf:punktzeiger;
begin
new(hilf,anfblock);
new(hilf^.next,endblock); hilf^.next^.vor:=hilf;
block:=addr(hilf^.next);
new(philf,anfpunkt);
new(philf^.next,endpunkt); philf^.next^.vor:=philf;
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
dispose(hilf^.vor,done); dispose(hilf,done);
philf:=selbst^;
while philf^.next<>nil do praus(philf);
dispose(philf^.vor,done); dispose(philf,done);
end;

constructor blockliste.neu;
begin end;

constructor blockliste.anfblock;
begin
next:=nil; vor:=nil; von:=-maxmesswert; bis:=-maxmesswert
end;

constructor blockliste.endblock;
begin
next:=nil; vor:=nil; von:=maxmesswert; bis:=maxmesswert
end;

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

constructor punktliste.anfpunkt;
begin
next:=nil; vor:=nil; bei:=-maxmesswert
end;

constructor punktliste.endpunkt;
begin
next:=nil; vor:=nil; bei:=maxmesswert
end;


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
end;

procedure listenfeld.load (var s:tbufstream);
var   hilf:listenzeiger;
      philf:punktzeiger;
begin
s.read(name,sizeof(name)); fsplit(name,named,namen,namee);
s.read(ko,sizeof(kopfdaten));
s.read(laenge,sizeof(messwert));
{ Die Zeigerstruktur der Blîcke wird neu aufgebaut und die Blîcke aus dem Stream gelesen }
new(hilf,anfblock);
block:=addr(hilf^.next);
hilf^.next:=listenzeiger(s.get);
hilf^.next^.vor:=hilf;
{ Die Zeigerstruktur der Punkte wird neu aufgebaut, die Liste bleibt leer }
new(philf,anfpunkt);
new(philf^.next,endpunkt); philf^.next^.vor:=philf;
selbst:=addr(philf^.next);
end;

procedure fileliste;
var   i:byte;
begin
writeln('  File list');
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
writeln(lfcr,'Pointer test',lfcr);
for i:=1 to filenr do begin
   write('> File no: ',i,'  ');
   write('>> Blocks  ');
   hilf:=liste[i].block^;
   if hilf^.vor^.next<>hilf then begin
      writeln('### Error at begin'); warte; exit end;
   while hilf^.next<>nil do begin
    write(hilf^.von:1:0,' ',hilf^.bis:1:0,' ');
    if hilf^.next^.vor<>hilf then begin
       writeln('### Error'); warte; exit end;
    hilf:=hilf^.next;
    end;
   write('>> Points ');
   philf:=liste[i].selbst^;
   if philf^.vor^.next<>philf then begin
      writeln('### Error at begin'); warte; exit end;
   while philf^.next<>nil do begin
    write(philf^.bei:1:0,' ');
    if philf^.next^.vor<>philf then begin
       writeln('### Error'); warte; exit end;
    philf:=philf^.next;
    end;
   writeln;
   end;
writeln(lfcr,'All pointers OK.'); warte;
end;

begin
registertype(rblockliste);
registertype(rpunktliste);
end.
