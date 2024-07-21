{ Borland-Pascal 7.0 / FPC 3.2.2 }
{$ifdef fpc} {$mode TP} {$endif}

unit nlsicht;

{$IFDEF MSDOS}
{$A+,B-,E+,F-,G-,I-,N+,O-,P+,T+,V+,X-}
{$ELSE}
{$A+,B-,E+,F-,G+,I-,N+,P+,T+,V+,X-}
{$ENDIF}

interface

uses  crt,                daff,wavpcm,tulab42, nlrahmen,           grafik,
      dos,                tlfilter,            nltrigg,            nlgrafik,
      bequem,             tlfiles,
      objects;

procedure sichten;

implementation

procedure sichtmenue (aktfile:byte); far; forward;

procedure sichten;
const abl:single=200;
      aktfile:byte=1;
      anfang:messwert=0;
var   sichtgrafik:grafikdaten;
      sinvar:single;
      i:grossint;
begin
ueberschrift(false,'View Data','Info',farbe2);
fileliste;
gotoxy(1,19); zwischen('Dialogue',farbe2); writeln;
i:=readint('File no.',aktfile);
if not (i in [1..filenr]) then begin
   fehler('Undefined file no.'); warte; exit end;
if i<>aktfile then anfang:=0;
aktfile:=i;
clrscr; kanaele.lesen(6,farbe2);
if kanaele.kn=0 then exit;
sinvar:=readext('Speed [mm/s]',abl,3,1);
if sinvar<=0 then begin
   fehler('Speed out of range.');
   warte; exit end;
abl:=sinvar;
anfang:=messw(readint('Start time [ms]',zeit(anfang)));
sichtgrafik.aufbauen(aktfile,kanaele,anfang,abl,sichtmenue);
end;


procedure sichtmenue (aktfile:byte);

{
procedure autoblock;
var   ta,tb:char;
      wandert:listenzeiger;
      i:longint;
      dummy,re:exword;
begin
ueberschrift(false,'Block Creation','Info',farbe4);
triggeruebersicht;
gotoxy(1,17); zwischen('Dialogue',farbe4);
window(1,21,80,25);
ta:=upcase(readchar('Block begin: Trigger list','A'));
if not (ta in['A'..listmax]) then begin
   fehler('Undefined trigger list'); warte; exit end;
tb:=upcase(readchar('Block end: Trigger list','B'));
if not (tb in['A'..listmax]) then begin
   fehler('Undefined trigger list'); warte; exit end;
with liste[aktfile], tliste[ta]^.fil[aktfile] do begin
   wandert:=block^;
   while wandert^.next<>nil do raus(wandert);
   for i:=1 to automn do begin
      tliste[tb]^.fil[aktfile].such(0,tliste[tb]^.fil[aktfile].automn,autom^[i],dummy,re);
         if (tliste[tb]^.fil[aktfile].autom^[re]<autom^[i+1]) and
            (tliste[tb]^.fil[aktfile].autom^[re]>autom^[i]) then begin
         rein(wandert);
         wandert^.von:=autom^[i];
         wandert^.bis:=tliste[tb]^.fil[aktfile].autom^[re];
         wandert:=wandert^.next;
         end;
      end;
   end;
end;
}

begin
ueberschrift(false,'View Data','Menu',farbe3);
gotoxy(1,6);
writeln(lfcr,'  b...Create Blocks from Trigger Lists',
        lfcr,'  x...Exit');
gotoxy(1,19); zwischen('Dialogue',farbe3); writeln;
case upcase(readcharim('Menu point','q')) of
   'B':nltrigg.autoblock(aktfile);
   'X':exit;
   end;
end;

end.
