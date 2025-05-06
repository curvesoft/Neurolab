{ Borland-Pascal 7.0 / FPC 3.2.2 }
{$ifdef fpc} {$mode TP} {$endif}

UNIT nlsicht;

{$IFDEF MSDOS}
{$A+,B-,E+,F-,G-,I-,N+,O-,P+,T+,V+,X-}
{$ELSE}
{$A+,B-,E+,F-,G+,I-,N+,P+,T+,V+,X-}
{$ENDIF}

INTERFACE

USES  crt, daff, wavpcm, tulab42, nlrahmen, grafik,
  dos, tlfilter, nltrigg, nlgrafik,
  bequem, tlfiles,
  objects;

PROCEDURE sichten;

IMPLEMENTATION

PROCEDURE sichtmenue(aktfile : BYTE); FAR; FORWARD;

PROCEDURE sichten;
CONST
  abl : SINGLE          = 200;
  aktfile : BYTE        = 1;
  anfang : typeextended = 0;
VAR
  graphic : TChannelDataGraphic;
  sinvar :  SINGLE;
  i :       bigint64;
BEGIN
  showtitle(False, 'View Data', 'Info', farbe2);
  fileliste;
  gotoxy(1, 19);
  zwischen('Dialogue', farbe2);
  writeln;
  i := readint('File no.', aktfile);
  IF NOT (i IN [1..filenr]) THEN
  BEGIN
    fehler('Undefined file no.');
    warte;
    exit;
  END;
  IF i <> aktfile THEN anfang := 0;
  aktfile := i;
  clrscr;
  kanaele.read(6, farbe2);
  IF kanaele.channelnumber = 0 THEN exit;
  sinvar := readext('Speed [mm/s]', abl, 3, 1);
  IF sinvar <= 0 THEN
  BEGIN
    fehler('Speed out of range.');
    warte;
    exit;
  END;
  abl    := sinvar;
  anfang := messw(readint('Start time [ms]', zeit(anfang)));
  graphic.construct(aktfile, kanaele, anfang, abl, sichtmenue);
END;


PROCEDURE sichtmenue(aktfile : BYTE);

{
procedure autoblock;
var   ta,tb:char;
      wandert:PDataBlock;
      i:longint;
      dummy,re:midint32;
begin
showtitle(false,'Block Creation','Info',farbe4);
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
BEGIN
  showtitle(False, 'View Data', 'Menu', farbe3);
  gotoxy(1, 6);
  writeln(lfcr, '  b...Create Blocks from Trigger Lists',
    lfcr, '  x...Exit');
  gotoxy(1, 19);
  zwischen('Dialogue', farbe3);
  writeln;
  CASE upcase(readcharim('Menu point', 'q')) OF
    'B' : nltrigg.autoblock(aktfile);
    'X' : exit;
  END;
END;

END.
