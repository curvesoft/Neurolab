{ Borland-Pascal 7.0 / FPC 3.2.2 }
{$ifdef fpc} {$mode TP} {$endif}

UNIT nltrigg;

{$IFDEF MSDOS}
{$A+,B-,E+,F-,G-,I-,N+,O-,P+,T+,V+,X-}
{$ELSE}
{$A+,B-,E+,F-,G+,I-,N+,P+,T+,V+,X-}
{$ENDIF}

INTERFACE

USES  crt, dos, daff, wavpcm, tulab42,
  objects, tlfilter,
  bequem, tlfiles,
  nlrahmen;

CONST
  {$ifdef fpc} triggermax=1 shl 22 -1; {$else} triggermax = 65520 DIV sizeof(messwert) - 12; {$endif}
  listmax = 'J';

TYPE  { Triggerlisten }

  {$ifdef fpc} grossintcomp=int64; {$else} grossintcomp = COMP; {$endif}

  triggerliste = ARRAY[0..triggermax + 1] OF typeextended;

  triggerdaten = OBJECT
    autom :   ^triggerliste;
    automn :  LONGINT;
    automda : BOOLEAN;
    PROCEDURE nimm(VAR aut : triggerliste; gesamt : LONGINT);
    PROCEDURE store(VAR s : tbufstream);
    PROCEDURE load(VAR s : tbufstream);
    PROCEDURE frei;
    PROCEDURE such(links, rechts : midint32; nach : typeextended; VAR li, re : midint32);
  END;

  filemenge = SET OF 1..maxfiles;

  { Triggeralgorithmen }

  triggerungzg = ^triggerung;

  triggerung = OBJECT(TObject)
    Name : STRING[50];
    fil :  ARRAY [1..maxfiles] OF triggerdaten;
    tr :   BYTE;
    FUNCTION fileanz : BYTE;
    FUNCTION erstfile : BYTE;
    FUNCTION triggsum : midint32;
    PROCEDURE neu;
    CONSTRUCTOR load(VAR s : tbufstream);
    PROCEDURE store(VAR s : tbufstream);
    DESTRUCTOR alt; VIRTUAL;
    PROCEDURE triggern(dabei : filemenge); VIRTUAL;
  PRIVATE
    PROCEDURE blocktriggern(von, bis : typeextended); VIRTUAL;
  END;

  keinezg = ^keine;

  keine = OBJECT(triggerung)
    CONSTRUCTOR neu;
    PROCEDURE triggern(dabei : filemenge); VIRTUAL;
  END;

  gelesenzg = ^gelesen;

  gelesen = OBJECT(triggerung)
    CONSTRUCTOR neu;
  PRIVATE
    PROCEDURE triggern(dabei : filemenge); VIRTUAL;
  END;

  punktezg = ^punkte;

  punkte = OBJECT(triggerung)
    CONSTRUCTOR neu;
  PRIVATE
    PROCEDURE triggern(dabei : filemenge); VIRTUAL;
  END;

  aequidistantzg = ^aequidistant;

  aequidistant = OBJECT(triggerung)
    anfa, dist : typeextended;
    CONSTRUCTOR neu;
    {         procedure triggern (dabei:filemenge); virtual;}
    CONSTRUCTOR load(VAR s : tbufstream);
    PROCEDURE store(VAR s : tbufstream);
  PRIVATE
    PROCEDURE blocktriggern(von, bis : typeextended); VIRTUAL;
  END;

  schwelle = OBJECT(triggerung)
    schw : typedouble;
    PROCEDURE neu;
    CONSTRUCTOR load(VAR s : tbufstream);
    PROCEDURE store(VAR s : tbufstream);
  END;
  hochzg = ^hoch;

  hoch = OBJECT(schwelle)
    CONSTRUCTOR neu;
  PRIVATE
    PROCEDURE blocktriggern(von, bis : typeextended); VIRTUAL;
  END;
  runterzg = ^runter;

  runter = OBJECT(schwelle)
    CONSTRUCTOR neu;
  PRIVATE
    PROCEDURE blocktriggern(von, bis : typeextended); VIRTUAL;
  END;

  extremum = OBJECT(triggerung)
  PRIVATE
    bmittel, nmittel : EXTENDED;
    PROCEDURE neu;
    CONSTRUCTOR load(VAR s : tbufstream);
    PROCEDURE store(VAR s : tbufstream);
  END;
  minimumzg = ^minimum;

  minimum = OBJECT(extremum)
    CONSTRUCTOR neu;
  PRIVATE
    PROCEDURE blocktriggern(von, bis : typeextended); VIRTUAL;
  END;
  maximumzg = ^maximum;

  maximum = OBJECT(extremum)
    CONSTRUCTOR neu;
  PRIVATE
    PROCEDURE blocktriggern(von, bis : typeextended); VIRTUAL;
  END;

  fenster = OBJECT(triggerung)
  PRIVATE
    schwunten, schwoben : typedouble;
    PROCEDURE neu;
    CONSTRUCTOR load(VAR s : tbufstream);
    PROCEDURE store(VAR s : tbufstream);
  END;
  fenstermaximumzg = ^fenstermaximum;

  fenstermaximum = OBJECT(fenster)
    CONSTRUCTOR neu;
  PRIVATE
    PROCEDURE blocktriggern(von, bis : typeextended); VIRTUAL;
  END;
  fensterminimumzg = ^fensterminimum;

  fensterminimum = OBJECT(fenster)
    CONSTRUCTOR neu;
  PRIVATE
    PROCEDURE blocktriggern(von, bis : typeextended); VIRTUAL;
  END;
  eintrittzg = ^eintritt;

  eintritt = OBJECT(fenster)
    CONSTRUCTOR neu;
  PRIVATE
    PROCEDURE blocktriggern(von, bis : typeextended); VIRTUAL;
  END;
  austrittzg = ^austritt;

  austritt = OBJECT(fenster)
    CONSTRUCTOR neu;
  PRIVATE
    PROCEDURE blocktriggern(von, bis : typeextended); VIRTUAL;
  END;

  triggerungsliste = ARRAY ['A'..listmax] OF triggerungzg;

  { Weitere Filter als Ergaenzung zur Unit "TLFILTER" }

  { Abstakter Filter mit Triggerliste }
  triggerfilter = OBJECT(filter)
    trliste : CHAR;
    trdaten : triggerdaten;
    PROCEDURE vorbereitung(frequenz : EXTENDED); VIRTUAL;
    CONSTRUCTOR load(VAR s : tbufstream);
    PROCEDURE store(VAR s : tbufstream);
  END;

  { Abstakter Filter mit zwei Triggerlisten }
  doppeltriggerfilter = OBJECT(filter)
    retrliste, ertrliste : CHAR;
    retrdaten, ertrdaten : triggerdaten;
    PROCEDURE vorbereitung(frequenz : EXTENDED); VIRTUAL;
    CONSTRUCTOR load(VAR s : tbufstream);
    PROCEDURE store(VAR s : tbufstream);
  END;

  { Zaehlt die Triggerpunkte -> Treppenfunktion }
  zaehltfilterzg = ^zaehltfilter;

  zaehltfilter = OBJECT(triggerfilter)
    CONSTRUCTOR neu(trigliste : CHAR);
    PROCEDURE einheitgenerieren(VAR beleg : belegung); VIRTUAL;
    FUNCTION gefiltert(posi : bigint64) : sample; VIRTUAL;
  END;

  { Umschaltung auf Punkte }
  punktefilterzg = ^punktefilter;

  punktefilter = OBJECT(triggerfilter)
    CONSTRUCTOR neu(trigliste : CHAR);
    PROCEDURE einheitgenerieren(VAR beleg : belegung); VIRTUAL;
  END;

  { Momentanfrequenz aus benachbarten Triggerpunkten berechnet }
  freqfilterzg = ^freqfilter;

  freqfilter = OBJECT(triggerfilter)
    CONSTRUCTOR neu(trigliste : CHAR);
    PROCEDURE einheitgenerieren(VAR beleg : belegung); VIRTUAL;
    FUNCTION gefiltert(posi : bigint64) : sample; VIRTUAL;
  END;

  { Zeitdifferenz aus benachbarten Triggerpunkten berechnet }
  intervallfilterzg = ^intervallfilter;

  intervallfilter = OBJECT(triggerfilter)
    CONSTRUCTOR neu(trigliste : CHAR);
    PROCEDURE einheitgenerieren(VAR beleg : belegung); VIRTUAL;
    FUNCTION gefiltert(posi : bigint64) : sample; VIRTUAL;
  END;

  { Werte an den Triggerstellen werden zu einem Linienzug verbunden }
  polygonfilterzg = ^polygonfilter;

  polygonfilter = OBJECT(triggerfilter)
    CONSTRUCTOR neu(trigliste : CHAR);
    PROCEDURE vorbereitung(frequenz : EXTENDED); VIRTUAL;
    FUNCTION gefiltert(posi : bigint64) : sample; VIRTUAL;
  PRIVATE
    erneut :         BOOLEAN;
    lialt, realt :   midint32;
    liposi, reposi : bigint64;
    liwert, rewert : grossintcomp;
  END;

  { Werte an den Triggerstellen werden aus eine ASCII-Liste gelesen }
  ywert         = typeextended; { ...dann passts mit der 64kB-Grenze }
  alistentyp    = ARRAY[1..triggermax] OF ywert;
  asciifilterzg = ^asciifilter;

  asciifilter = OBJECT(triggerfilter)
    CONSTRUCTOR neu(aname : STRING; trigliste : CHAR);
    PROCEDURE einheitgenerieren(VAR beleg : belegung); VIRTUAL;
    PROCEDURE vorbereitung(frequenz : EXTENDED); VIRTUAL;
    FUNCTION gefiltert(posi : bigint64) : sample; VIRTUAL;
    CONSTRUCTOR load(VAR s : tbufstream);
    PROCEDURE store(VAR s : tbufstream);
  PRIVATE
    aliste : alistentyp;
    amax :   ywert;
  END;

  { Zeitdifferenzen zwischen Triggerlisten - alt}
  diffilteraltzg = ^diffilteralt;

  diffilteralt = OBJECT(doppeltriggerfilter)
    smax : EXTENDED;
    CONSTRUCTOR neu(retrigliste, ertrigliste : CHAR; msmax : bigint64);
    PROCEDURE einheitgenerieren(VAR beleg : belegung); VIRTUAL;
    FUNCTION gefiltert(posi : bigint64) : sample; VIRTUAL;
    CONSTRUCTOR load(VAR s : tbufstream);
    PROCEDURE store(VAR s : tbufstream);
  END;

  { Zeitdifferenzen zwischen Triggerlisten - neu}
  diffilterzg = ^diffilter;

  diffilter = OBJECT(doppeltriggerfilter)
    smax : EXTENDED;
    typ :  (naechster, vorwaerts, rueckwaerts);
    CONSTRUCTOR neu(retrigliste, ertrigliste : CHAR; msmax : bigint64; typchar : CHAR);
    PROCEDURE einheitgenerieren(VAR beleg : belegung); VIRTUAL;
    FUNCTION gefiltert(posi : bigint64) : sample; VIRTUAL;
    CONSTRUCTOR load(VAR s : tbufstream);
    PROCEDURE store(VAR s : tbufstream);
  END;

  { Phasenfilter }
  phasenfilterzg = ^phasenfilter;

  phasenfilter = OBJECT(doppeltriggerfilter)
    peri : bigint64;
    CONSTRUCTOR neu(retrigliste, ertrigliste : CHAR; perioden : bigint64);
    PROCEDURE einheitgenerieren(VAR beleg : belegung); VIRTUAL;
    FUNCTION gefiltert(posi : bigint64) : sample; VIRTUAL;
    CONSTRUCTOR load(VAR s : tbufstream);
    PROCEDURE store(VAR s : tbufstream);
  END;

  { TL-Integrations- Filter }
  tlintfilterzg = ^tlintfilter;

  tlintfilter = OBJECT(triggerfilter)
  PUBLIC
    CONSTRUCTOR neu(trigliste : CHAR);
  PRIVATE
    gefiltertwert : sample;
    posiwert :      bigint64;
    PROCEDURE einheitgenerieren(VAR beleg : belegung); VIRTUAL;
    PROCEDURE vorbereitung(frequenz : EXTENDED); VIRTUAL;
    FUNCTION gefiltert(posi : bigint64) : sample; VIRTUAL;
  END;

  { Hilfsfeld fuer phasenbezogene Auswertung }

  weiser = ARRAY[1..triggermax] OF midint32;

  triggerweiser = OBJECT
    weisliste :     ARRAY[1..maxfiles] OF RECORD
      t : ^weiser;
      l :           midint32;
      n :           midint32
      END;
    gesamt :        LONGINT;
    mittelabstand : typeextended;
    PROCEDURE zaehlen(VAR feld : triggerung; minabst, maxabst : typeextended);
    PROCEDURE frei;
  END;


CONST
  triggeranz : midint32     = triggermax;
  triggeranf : midint32     = 1;
  triggerabz : midint32     = 1;
  triggerdst : typeextended = 0;

VAR
  tliste : triggerungsliste;
  komp84 : BOOLEAN; {Kompatibilitaet zwischen 8.4 und 8.5}

PROCEDURE autoblock(aktfile : BYTE);

PROCEDURE manager;

PROCEDURE triggeruebersicht;
PROCEDURE kontrolle(nr : BYTE; trind : CHAR);

{ Abspeichern der Triggerlisten in einem stream }

PROCEDURE streamput(VAR s : tbufstream);
PROCEDURE streamget(VAR s : tbufstream);

IMPLEMENTATION

TYPE
  uebersicht       = ARRAY['A'..listmax] OF filemenge;
  zahlenuebersicht = ARRAY['A'..listmax, 1..maxfiles] OF midint32;

  matrix = OBJECT
    tl :     uebersicht;
    unsinn : BOOLEAN;
    escape : BOOLEAN;
    PROCEDURE eingabe;
  END;

  zahlenmatrix = OBJECT(matrix)
    tz :     zahlenuebersicht;
    tn :     CHAR;
    fn :     BYTE;
    zn :     BYTE;
    wx, wy : BYTE;
    PROCEDURE uebernehmen;
    PROCEDURE ausgabe;
    PROCEDURE erstsprung;
    PROCEDURE sprung;
  END;

  trist = OBJECT
    gesamt : midint32;
    zaehler, abzaehler : midint32;
    letztstelle : typeextended;
    zn : BYTE;
    PROCEDURE beginn(trfile : BYTE);
    PROCEDURE weiter(stelle : typeextended);
    FUNCTION aufhoeren : BOOLEAN;
  END;

  gleitschw = OBJECT
    schw :          typedouble;
    nkorr, n2korr : bigint64;
    PROCEDURE beginn(nmittel, bmittel : EXTENDED);
    PROCEDURE mitteln(stekorr : bigint64; tr : BYTE);
    PROCEDURE zaehlt(VAR stekorr : bigint64; tr : BYTE);
  END;

CONST
  bloecke : BOOLEAN = False;
  bloeckeb : ARRAY[BOOLEAN] OF CHAR = ('f', 'b');
  bloecketext : ARRAY[BOOLEAN] OF string20 = ('File', 'Block');

  akttrkan : BYTE = 0;

  rkeine : tstreamrec = (objtype : 100; vmtlink : ofs(typeof(keine)^);
    load : @triggerung.load; store : @triggerung.store);
  rgelesen : tstreamrec = (objtype : 111; vmtlink : ofs(typeof(gelesen)^);
    load : @triggerung.load; store : @triggerung.store);
  rpunkte : tstreamrec = (objtype : 101; vmtlink : ofs(typeof(punkte)^);
    load : @triggerung.load; store : @triggerung.store);
  rhoch : tstreamrec = (objtype : 102; vmtlink : ofs(typeof(hoch)^);
    load : @schwelle.load; store : @schwelle.store);
  rrunter : tstreamrec = (objtype : 103; vmtlink : ofs(typeof(runter)^);
    load : @schwelle.load; store : @schwelle.store);
  rminimum : tstreamrec = (objtype : 104; vmtlink : ofs(typeof(minimum)^);
    load : @extremum.load; store : @extremum.store);
  rmaximum : tstreamrec = (objtype : 105; vmtlink : ofs(typeof(maximum)^);
    load : @extremum.load; store : @extremum.store);
  rfenstermaximum : tstreamrec = (objtype : 106; vmtlink : ofs(typeof(fenstermaximum)^);
    load : @fenster.load; store : @fenster.store);
  rfensterminimum : tstreamrec = (objtype : 107; vmtlink : ofs(typeof(fensterminimum)^);
    load : @fenster.load; store : @fenster.store);
  raequidistant : tstreamrec = (objtype : 108; vmtlink : ofs(typeof(aequidistant)^);
    load : @aequidistant.load; store : @aequidistant.store);
  reintritt : tstreamrec = (objtype : 109; vmtlink : ofs(typeof(eintritt)^);
    load : @fenster.load; store : @fenster.store);
  raustritt : tstreamrec = (objtype : 110; vmtlink : ofs(typeof(austritt)^);
    load : @fenster.load; store : @fenster.store);

  rfreqfilter : tstreamrec = (objtype : 120; vmtlink : ofs(typeof(freqfilter)^);
    load : @triggerfilter.load; store : @triggerfilter.store);
  rpolygonfilter : tstreamrec = (objtype : 121; vmtlink : ofs(typeof(polygonfilter)^);
    load : @triggerfilter.load; store : @triggerfilter.store);
  rdiffilteralt : tstreamrec = (objtype : 122; vmtlink : ofs(typeof(diffilteralt)^);
    load : @diffilteralt.load; store : @diffilteralt.store);
  rphasenfilter : tstreamrec = (objtype : 123; vmtlink : ofs(typeof(phasenfilter)^);
    load : @phasenfilter.load; store : @phasenfilter.store);
  rpunktefilter : tstreamrec = (objtype : 124; vmtlink : ofs(typeof(punktefilter)^);
    load : @triggerfilter.load; store : @triggerfilter.store);
  rzaehltfilter : tstreamrec = (objtype : 125; vmtlink : ofs(typeof(zaehltfilter)^);
    load : @triggerfilter.load; store : @triggerfilter.store);
  rintervallfilter : tstreamrec = (objtype : 126; vmtlink : ofs(typeof(intervallfilter)^);
    load : @triggerfilter.load; store : @triggerfilter.store);
  rdiffilter : tstreamrec = (objtype : 127; vmtlink : ofs(typeof(diffilter)^);
    load : @diffilter.load; store : @diffilter.store);
  rasciifilter : tstreamrec = (objtype : 128; vmtlink : ofs(typeof(asciifilter)^);
    load : @asciifilter.load; store : @asciifilter.store);
  rtlintfilter : tstreamrec = (objtype : 129; vmtlink : ofs(typeof(tlintfilter)^);
    load : @triggerfilter.load; store : @triggerfilter.store);

VAR
  mat :    zahlenmatrix;
  verfol : trist;

  aut :     ^triggerliste;
  trind :   CHAR;
  abbruch : BOOLEAN;


PROCEDURE autoblock(aktfile : BYTE);
VAR
  ta, tb :          CHAR;
  wandert :         PDataBlock;
  i, j :            LONGINT;
  dummy, re :       midint32;
  linksf, rechtsf : BYTE;
BEGIN
  showtitle(False, 'Block Creation', 'Info', farbe4);
  triggeruebersicht;
  gotoxy(1, 17);
  zwischen('Dialogue', farbe4);
  window(1, 21, 80, 25);
  IF aktfile = 0 THEN
  BEGIN
    linksf  := 1;
    rechtsf := filenr;
  END
  ELSE
  BEGIN
    linksf  := aktfile;
    rechtsf := aktfile;
  END;
  ta := upcase(readchar('Block begin: Trigger list', 'A'));
  IF NOT (ta IN ['A'..listmax]) THEN
  BEGIN
    fehler('Undefined trigger list');
    warte;
    exit;
  END;
  tb := upcase(readchar('Block end: Trigger list', 'B'));
  IF NOT (tb IN ['A'..listmax]) THEN
  BEGIN
    fehler('Undefined trigger list');
    warte;
    exit;
  END;
  writeln;
  FOR aktfile := linksf TO rechtsf DO
  BEGIN
    j := 0;
    WITH liste[aktfile], tliste[ta]^.fil[aktfile] DO
    BEGIN
      wandert := block^;
      WHILE wandert^.Next <> nil DO raus(wandert);
      FOR i := 1 TO automn DO
      BEGIN
        tliste[tb]^.fil[aktfile].such(0, tliste[tb]^.fil[aktfile].automn, autom^[i], dummy, re);
        IF (tliste[tb]^.fil[aktfile].autom^[re] < autom^[i + 1]) AND
          (tliste[tb]^.fil[aktfile].autom^[re] > autom^[i]) THEN
        BEGIN
          rein(wandert);
          wandert^.frompos := autom^[i];
          wandert^.endpos  := tliste[tb]^.fil[aktfile].autom^[re];
          wandert          := wandert^.Next;
          Inc(j);
        END;
      END;
    END;
    writeln(liste[aktfile].Name, ' :', j, ' blocks created.');
  END;
END;

PROCEDURE triggerdaten.nimm(VAR aut : triggerliste; gesamt : LONGINT);
VAR
  i : LONGINT;
BEGIN
  getmem(autom, sizeof(typeextended) * (gesamt + 2));
  FOR i := 1 TO gesamt DO autom^[i] := aut[i];
  automn             := gesamt;
  automda            := True;
  autom^[0]          := -maxmesswert;
  autom^[automn + 1] := maxmesswert;
END;

PROCEDURE triggerdaten.store(VAR s : tbufstream);
BEGIN
  s.Write(automda, sizeof(BOOLEAN));
  IF automda THEN
  BEGIN
    s.Write(automn, sizeof(automn));
    s.Write(autom^, sizeof(typeextended) * (automn + 2));
  END;
END;

PROCEDURE triggerdaten.load(VAR s : tbufstream);
BEGIN
  s.Read(automda, sizeof(BOOLEAN));
  IF automda THEN
  BEGIN
    s.Read(automn, sizeof(automn));
    getmem(autom, sizeof(typeextended) * (automn + 2));
    s.Read(autom^, sizeof(typeextended) * (automn + 2));
  END
  ELSE
    automn := 0;
END;

PROCEDURE triggerdaten.frei;
BEGIN
  IF automda THEN
  BEGIN
    freemem(autom, sizeof(typeextended) * (automn + 2));
    automda := False;
    automn  := 0;
  END;
END;

PROCEDURE triggerdaten.such(links, rechts : midint32; nach : typeextended; VAR li, re : midint32);
{ Binaeres Suchen zwischen "links" und "rechts" in der TL nach "nach": }
VAR
  neu : midint32;
BEGIN
  li := links;
  re := rechts;
  WHILE re - li > 1 DO
  BEGIN
    neu := li + (re - li) DIV 2;
    IF autom^[neu] <= nach THEN li := neu;
    IF autom^[neu] >= nach THEN re := neu;
  END;
  IF autom^[re] = nach THEN li := re
  ELSE IF autom^[li] = nach THEN re := li;
END;

PROCEDURE matrix.eingabe;
VAR
  bef : STRING[3];
  ti :  CHAR;
BEGIN
  FOR ti := 'A' TO listmax DO tl[ti] := [1..filenr];
  bef := readstring('Execution at line, column or position eg: 1, B, A2', '');
  IF bef = '' THEN
  BEGIN
    unsinn := False;
    escape := True;
    exit;
  END;
  unsinn := True;
  escape := False;
  IF bef[1] IN ['a'..'z', 'A'..'Z'] THEN
  BEGIN
    FOR ti := 'A' TO listmax DO IF ti <> upcase(bef[1]) THEN tl[ti] := [];
    Delete(bef, 1, 1);
    unsinn := False;
  END;
  IF bef[1] IN ['0'..'9'] THEN
  BEGIN
    FOR ti := 'A' TO listmax DO tl[ti] := tl[ti] * [zahl(bef)];
    unsinn := False;
  END;
  IF unsinn THEN FOR ti := 'A' TO listmax DO tl[ti] := [];
END;

PROCEDURE zahlenmatrix.ausgabe;
VAR
  fi : BYTE;
  ti : CHAR;

  PROCEDURE zeile;
  VAR
    ti : CHAR;
  BEGIN
    Write(fi : 3, ':', liste[fi].namename : 8, ' ');
    FOR ti := 'A' TO listmax DO IF fi IN tl[ti] THEN Write(tz[ti, fi] : 6)
      ELSE
        Write('-' : 6);
    writeln;
  END;

BEGIN
  zn := wherey;
  Write('File' : 12, ' ');
  FOR ti := 'A' TO listmax DO Write(ti : 6);
  writeln(lfcr);
  FOR fi := 1 TO filenr DO zeile;
END;

PROCEDURE zahlenmatrix.erstsprung;
BEGIN
  wx := 14 + (Ord(tn) - Ord('A')) * 6;
  wy := zn + 1 + fn;
  gotoxy(wx, wy);
END;

PROCEDURE zahlenmatrix.sprung;
BEGIN
  gotoxy(wx, wy);
END;

PROCEDURE zahlenmatrix.uebernehmen;
VAR
  ti : CHAR;
  fi : BYTE;
BEGIN
  FOR ti := 'A' TO listmax DO
  BEGIN
    tl[ti] := [];
    FOR fi := 1 TO maxfiles DO WITH tliste[ti]^.fil[fi] DO
      BEGIN
        tz[ti, fi] := automn;
        IF automda THEN tl[ti] := tl[ti] + [fi];
      END;
  END;
END;

{ trist }

PROCEDURE trist.beginn(trfile : BYTE);
VAR
  di :  dirstr;
  na :  namestr;
  ext : extstr;
BEGIN
  gesamt      := 0;
  zaehler     := 1;
  abzaehler   := triggerabz;
  letztstelle := -maxmesswert;
  zn          := wherey;
  mat.erstsprung;
  Write(0 : 6);
  fsplit(liste[trfile].Name, di, na, ext);
  gotoxy(1, zn);
  clreol;
  Write('Trigger event in ', na + ext : 11, ' at           ms: Abort: <Esc>');
  abbruch := keypressed AND (readkey = #27);
END;

PROCEDURE trist.weiter(stelle : typeextended);
VAR
  ausgabe : BOOLEAN;
BEGIN
  IF stelle - letztstelle >= triggerdst THEN
  BEGIN
    letztstelle := stelle;
    IF zaehler < triggeranf THEN Inc(zaehler)
    ELSE
    IF abzaehler < triggerabz THEN Inc(abzaehler)
    ELSE
    BEGIN
      Inc(gesamt);
      aut^[gesamt] := stelle;
      ausgabe      := (gesamt MOD 128) = 0;
      IF ausgabe THEN
      BEGIN
        mat.sprung;
        Write(gesamt : 6);
      END;
      abzaehler := 1;
    END;
  END;
  IF ausgabe THEN
  BEGIN
    gotoxy(32, zn);
    Write(zeit(stelle) : 8);
  END;
  IF keypressed AND (readkey = #27) THEN abbruch := True;
END;

FUNCTION trist.aufhoeren : BOOLEAN;
BEGIN
  aufhoeren := (gesamt = triggeranz) OR abbruch;
END;

{ triggerung }

FUNCTION triggerung.fileanz : BYTE;
VAR
  i, enn : BYTE;
BEGIN
  enn := 0;
  FOR i := 1 TO filenr DO IF fil[i].automda THEN Inc(enn);
  fileanz := enn;
END;

FUNCTION triggerung.erstfile : BYTE;
VAR
  fi : BYTE;
BEGIN
  fi := 1;
  WHILE NOT fil[fi].automda AND (fi < filenr) DO Inc(fi);
  erstfile := fi;
END;

FUNCTION triggerung.triggsum : midint32;
VAR
  i :     BYTE;
  summe : midint32;
BEGIN
  summe := 0;
  FOR i := 1 TO filenr DO Inc(summe, fil[i].automn);
  triggsum := summe;
END;

PROCEDURE triggerung.neu;
VAR
  i : BYTE;
BEGIN
  FOR i := 1 TO maxfiles DO WITH fil[i] DO
    BEGIN
      automda := False;
      automn  := 0;
    END;
  tr := akttrkan;
END;

CONSTRUCTOR triggerung.load(VAR s : tbufstream);
VAR
  i : BYTE;
BEGIN
  s.Read(Name, sizeof(Name));
  s.Read(tr, 1);
  FOR i := 1 TO maxfiles DO fil[i].load(s);
END;

PROCEDURE triggerung.store(VAR s : tbufstream);
VAR
  i : BYTE;
BEGIN
  s.Write(Name, sizeof(Name));
  s.Write(tr, 1);
  FOR i := 1 TO maxfiles DO fil[i].store(s);
END;

PROCEDURE triggerung.triggern(dabei : filemenge);
LABEL
  genug;
VAR
  trfile :  BYTE;
  wandert : PDataBlock;
BEGIN
  FOR trfile := 1 TO filenr DO IF trfile IN dabei THEN
      WITH fil[trfile], liste[trfile] DO
      BEGIN
        mat.fn := trfile;
        verfol.beginn(trfile);
        IF abbruch THEN exit;
        openfile(trfile);
        IF bloecke THEN
        BEGIN
          wandert := block^;
          WHILE wandert^.Next <> nil DO
          BEGIN
            blocktriggern(wandert^.frompos, wandert^.endpos);
            IF verfol.aufhoeren THEN GOTO genug;
            wandert := wandert^.Next;
          END;
        END
        ELSE
          blocktriggern(0, length);
        genug :
          nimm(aut^, verfol.gesamt);
        schliesse;
        IF abbruch THEN exit;
      END;
  piep;
END;

PROCEDURE triggerung.blocktriggern(von, bis : typeextended);
BEGIN
  abstract;
END;

DESTRUCTOR triggerung.alt;
VAR
  i : BYTE;
BEGIN
  FOR i := 1 TO filenr DO fil[i].frei;
END;

CONSTRUCTOR keine.neu;
BEGIN
  triggerung.neu;
  Name := 'undefined';
  tr   := maxchannelsandfilters;
END;

PROCEDURE keine.triggern(dabei : filemenge);
BEGIN
END;

CONSTRUCTOR punkte.neu;
BEGIN
  triggerung.neu;
  tr   := maxchannelsandfilters;
  Name := 'user defined points';
END;

{ Alte Prozedur ohne Bloecke...
procedure punkte.triggern (dabei:filemenge);
var   trfile:byte;
      pwandert:PBulletList;
begin
for trfile:=1 to filenr do if trfile in dabei then
 with fil[trfile], liste[trfile] do begin
   mat.fn:=trfile;
   verfol.beginn(trfile); if abbruch then exit;
(*   openfile(trfile); *)
   pwandert:=selbst^;
   while (pwandert^.next<>nil) and not verfol.aufhoeren do begin
      verfol.weiter(pwandert^.bei);
      pwandert:=pwandert^.next end;
   nimm(aut^,verfol.gesamt);
(*   schliesse;  *)
   if abbruch then exit;
   end;
end;
}

PROCEDURE punkte.triggern(dabei : filemenge);
LABEL
  genug;
VAR
  trfile :   BYTE;
  wandert :  PDataBlock;
  pwandert : PBulletList;

  PROCEDURE punkteblocktriggern(von, bis : typeextended);
  BEGIN
    WHILE (pwandert^.Next <> nil) AND (pwandert^.bei < von) DO pwandert := pwandert^.Next;
    WHILE (pwandert^.Next <> nil) AND (pwandert^.bei <= bis) DO
    BEGIN
      verfol.weiter(pwandert^.bei);
      pwandert := pwandert^.Next;
    END;
  END;

BEGIN
  FOR trfile := 1 TO filenr DO IF trfile IN dabei THEN
      WITH fil[trfile], liste[trfile] DO
      BEGIN
        mat.fn := trfile;
        verfol.beginn(trfile);
        IF abbruch THEN exit;
        pwandert := selectedbullet^;
        IF bloecke THEN
        BEGIN
          wandert := block^;
          WHILE wandert^.Next <> nil DO
          BEGIN
            punkteblocktriggern(wandert^.frompos, wandert^.endpos);
            IF verfol.aufhoeren THEN GOTO genug;
            wandert := wandert^.Next;
          END;
        END
        ELSE
          punkteblocktriggern(0, length);
        genug :
          nimm(aut^, verfol.gesamt);
        IF abbruch THEN exit;
      END;
  piep;
END;

CONSTRUCTOR gelesen.neu;
VAR
  eingabe :   Text;
  dateiname : string80;
  autfil :    ARRAY[1..maxfiles] OF ^triggerliste;
  nfil :      ARRAY[1..maxfiles] OF midint32;
  trfile :    BYTE;
  fn :        midint32;
  te :        EXTENDED;
  znr :       bigint64;
LABEL
  abbruch;
BEGIN
  triggerung.neu;
  tr        := maxchannelsandfilters;
  dateiname := readstring('File name and path', 'trigger.txt');
  Name      := 'from file ' + dateiname;
  FOR trfile := 1 TO filenr DO
  BEGIN
    new(autfil[trfile]);
    nfil[trfile] := 0;
  END;
  znr := 0;
  Assign(eingabe, dateiname);
  reset(eingabe);
  WHILE NOT EOF(eingabe) DO
  BEGIN
    Inc(znr);
    Read(eingabe, fn, te);
    IF (ioresult = 0) AND (fn IN [1..filenr]) THEN
    BEGIN
      Inc(nfil[fn]);
      autfil[fn]^[nfil[fn]] := messwext(te);
    END
    ELSE
    IF znr > 3 THEN
    BEGIN
      fehler('Invalid line ' + wort(znr) + ' in import file');
      warte;
      GOTO abbruch;
    END;
    readln(eingabe);
  END;
  abbruch :
    Close(eingabe);
  FOR trfile := 1 TO filenr DO
  BEGIN
    fil[trfile].nimm(autfil[trfile]^, nfil[trfile]);
    dispose(autfil[trfile]);
  END;
END;

PROCEDURE gelesen.triggern(dabei : filemenge);
BEGIN
END;

CONSTRUCTOR aequidistant.neu;
BEGIN
  triggerung.neu;
  tr   := maxchannelsandfilters;
  anfa := messwext(readext('Start [ms]', 0, 1, 1));
  dist := messwext(readext('Distance [ms]', 0, 1, 1));
  Name := extwort(extzeit(anfa), 3, 1) + ' ms + equidistant ' + extwort(extzeit(dist), 3, 1) + ' ms';
END;

{ Alte Prozedur ohne Bloecke

procedure aequidistant.triggern (dabei:filemenge);
var   trfile:byte;
      hierbei:typeextended;
begin
for trfile:=1 to filenr do if trfile in dabei then
 with fil[trfile], liste[trfile] do begin
   mat.fn:=trfile;
   verfol.beginn(trfile); if abbruch then exit;
   openfile(trfile);
   hierbei:=anfa;
   while (laenge>=hierbei) and not verfol.aufhoeren do begin
      verfol.weiter(hierbei);
      hierbei:=hierbei+dist;
      end;
   nimm(aut^,verfol.gesamt);
   schliesse;
   if abbruch then exit;
   end;
end;}

PROCEDURE aequidistant.blocktriggern(von, bis : typeextended);
VAR
  hierbei : typeextended;
BEGIN
  hierbei := anfa + von;
  WHILE (bis > hierbei) AND NOT verfol.aufhoeren DO
  BEGIN
    verfol.weiter(hierbei);
    hierbei := hierbei + dist;
  END;
END;

CONSTRUCTOR aequidistant.load(VAR s : tbufstream);
BEGIN
  triggerung.load(s);
  s.Read(dist, sizeof(typeextended));
  s.Read(anfa, sizeof(typeextended));
END;

PROCEDURE aequidistant.store(VAR s : tbufstream);
BEGIN
  triggerung.store(s);
  s.Write(dist, sizeof(typeextended));
  s.Write(anfa, sizeof(typeextended));
END;

PROCEDURE schwelle.neu;
VAR
  einstr : string20;
BEGIN
  triggerung.neu;
  einstr := belegungsliste[tr].getunit;
  schw   := norm(readext('Threshold [' + einstr + ']', 0, 1, 1), tr);
  Name   := extwort(extspannung(schw, tr), 2, 1) + ' ' + einstr + ' - ';
END;

CONSTRUCTOR schwelle.load(VAR s : tbufstream);
BEGIN
  triggerung.load(s);
  s.Read(schw, sizeof(typedouble));
END;

PROCEDURE schwelle.store(VAR s : tbufstream);
BEGIN
  triggerung.store(s);
  s.Write(schw, sizeof(typedouble));
END;

CONSTRUCTOR hoch.neu;
BEGIN
  schwelle.neu;
  Name := Name + 'rising threshold';
END;

PROCEDURE hoch.blocktriggern(von, bis : typeextended);
VAR
  stekorr, vonkorr, biskorr : bigint64;
BEGIN
  vonkorr := trunc(von * korr) + 1;
  biskorr := trunc(bis * korr);
  stekorr := vonkorr;
  REPEAT
    WHILE (dat(stekorr, tr) >= schw) AND (stekorr <= biskorr) DO Inc(stekorr);
    WHILE (dat(stekorr, tr) < schw) AND (stekorr <= biskorr) DO Inc(stekorr);
    IF stekorr > biskorr THEN exit;
    verfol.weiter((stekorr - 0.5) / korr);
  UNTIL verfol.aufhoeren;
END;

CONSTRUCTOR runter.neu;
BEGIN
  schwelle.neu;
  Name := Name + 'falling threshold';
END;

PROCEDURE runter.blocktriggern(von, bis : typeextended);
VAR
  stekorr, vonkorr, biskorr : bigint64;
BEGIN
  vonkorr := trunc(von * korr) + 1;
  biskorr := trunc(bis * korr);
  stekorr := vonkorr;
  REPEAT
    WHILE (dat(stekorr, tr) <= schw) AND (stekorr <= biskorr) DO Inc(stekorr);
    WHILE (dat(stekorr, tr) > schw) AND (stekorr <= biskorr) DO Inc(stekorr);
    IF stekorr > biskorr THEN exit;
    verfol.weiter((stekorr - 0.5) / korr);
  UNTIL verfol.aufhoeren;
END;

PROCEDURE extremum.neu;
BEGIN
  triggerung.neu;
  nmittel := messw(readint('Width of gliding Average [ms]', 100));
  bmittel := messw(readint('Lower limit of signal deviation [ms]', 10));
  Name    := '';
END;

CONSTRUCTOR extremum.load(VAR s : tbufstream);
BEGIN
  triggerung.load(s);
  s.Read(bmittel, sizeof(bmittel));
  s.Read(nmittel, sizeof(nmittel));
END;

PROCEDURE extremum.store(VAR s : tbufstream);
BEGIN
  triggerung.store(s);
  s.Write(bmittel, sizeof(bmittel));
  s.Write(nmittel, sizeof(nmittel));
END;

PROCEDURE gleitschw.beginn(nmittel, bmittel : EXTENDED);
BEGIN
  n2korr := round(nmittel * korr) DIV 2;
  nkorr  := n2korr * 2 + 1;
END;

PROCEDURE gleitschw.mitteln(stekorr : bigint64; tr : BYTE);
VAR
  i : bigint64;
BEGIN
  schw := 0;
  i    := stekorr - n2korr;
  WHILE i <= stekorr + n2korr DO
  BEGIN
    schw := schw + dat(i, tr);
    Inc(i);
  END;
  schw := schw / nkorr;
END;

PROCEDURE gleitschw.zaehlt(VAR stekorr : bigint64; tr : BYTE);
BEGIN
  Inc(stekorr);
  schw := schw - (dat(stekorr - n2korr - 1, tr) - dat(stekorr + n2korr, tr)) / nkorr;
END;

CONSTRUCTOR minimum.neu;
BEGIN
  extremum.neu;
  Name := 'minimum < gliding average';
END;

PROCEDURE minimum.blocktriggern(von, bis : typeextended);
VAR
  l, r :       bigint64;
  hilf, extr : bigint64;
  stekorr, vonkorr, biskorr, bkorr : bigint64;
  glesch :     gleitschw;
BEGIN
  vonkorr := trunc(von * korr) + 1;
  biskorr := trunc(bis * korr);
  r       := vonkorr;
  glesch.beginn(nmittel, bmittel);
  glesch.mitteln(r, tr);
  bkorr := trunc(bmittel * korr) + 1;
  REPEAT
    REPEAT
      WHILE (dat(r, tr) <= glesch.schw) AND (r <= biskorr) DO glesch.zaehlt(r, tr);
      WHILE (dat(r, tr) > glesch.schw) AND (r <= biskorr) DO glesch.zaehlt(r, tr);
      IF r > biskorr THEN exit;
      l    := r;
      extr := maxsample;
      hilf := dat(l, tr);
      WHILE hilf < glesch.schw DO
      BEGIN
        IF extr > hilf THEN
        BEGIN
          extr    := hilf;
          stekorr := r;
        END;
        glesch.zaehlt(r, tr);
        IF r > biskorr THEN exit;
        hilf := dat(r, tr);
      END;
    UNTIL r - l >= bkorr;
    verfol.weiter(stekorr / korr);
  UNTIL verfol.aufhoeren;
END;

CONSTRUCTOR maximum.neu;
BEGIN
  extremum.neu;
  Name := 'maximum > gliding average';
END;

PROCEDURE maximum.blocktriggern(von, bis : typeextended);
VAR
  l, r :       bigint64;
  hilf, extr : bigint64;
  stekorr, vonkorr, biskorr, bkorr : bigint64;
  glesch :     gleitschw;
BEGIN
  vonkorr := trunc(von * korr) + 1;
  biskorr := trunc(bis * korr);
  r       := vonkorr;
  glesch.beginn(nmittel, bmittel);
  glesch.mitteln(r, tr);
  bkorr := trunc(bmittel * korr) + 1;
  REPEAT
    REPEAT
      WHILE (dat(r, tr) >= glesch.schw) AND (r <= biskorr) DO glesch.zaehlt(r, tr);
      WHILE (dat(r, tr) < glesch.schw) AND (r <= biskorr) DO glesch.zaehlt(r, tr);
      IF r > biskorr THEN exit;
      l    := r;
      extr := minsample;
      hilf := dat(l, tr);
      WHILE hilf > glesch.schw DO
      BEGIN
        IF extr < hilf THEN
        BEGIN
          extr    := hilf;
          stekorr := r;
        END;
        glesch.zaehlt(r, tr);
        IF r > biskorr THEN exit;
        hilf := dat(r, tr);
      END;
    UNTIL r - l >= bkorr;
    verfol.weiter(stekorr / korr);
  UNTIL verfol.aufhoeren;
END;

PROCEDURE fenster.neu;
VAR
  einstr : string20;
BEGIN
  triggerung.neu;
  einstr    := belegungsliste[tr].getunit;
  schwunten := norm(readext('Lower threshold [' + einstr + ']', 0, 1, 1), tr);
  schwoben  := norm(readext('Upper threshold [' + einstr + ']', 0, 1, 1), tr);
  Name      := extwort(extspannung(schwunten, tr), 2, 1) + ' ' + einstr + ' - ' +
    extwort(extspannung(schwoben, tr), 2, 1) + ' ' + einstr + ' - ';
END;

CONSTRUCTOR fenster.load(VAR s : tbufstream);
BEGIN
  triggerung.load(s);
  s.Read(schwunten, sizeof(typedouble));
  s.Read(schwoben, sizeof(typedouble));
END;

PROCEDURE fenster.store(VAR s : tbufstream);
BEGIN
  triggerung.store(s);
  s.Write(schwunten, sizeof(typedouble));
  s.Write(schwoben, sizeof(typedouble));
END;

CONSTRUCTOR fenstermaximum.neu;
BEGIN
  fenster.neu;
  Name := Name + 'maximum in window';
END;

PROCEDURE fenstermaximum.blocktriggern(von, bis : typeextended);
LABEL
  zuhoch;
VAR
  l, r :       bigint64;
  hilf, extr : typedouble;
  stekorr, vonkorr, biskorr, vorlaeufigkorr : bigint64;
BEGIN
  vonkorr := trunc(von * korr) + 1;
  biskorr := trunc(bis * korr);
  r       := vonkorr;
  REPEAT
    zuhoch :
      l := r;
    WHILE (dat(l, tr) >= schwunten) AND (l <= biskorr) DO Inc(l);
    WHILE (dat(l, tr) < schwunten) AND (l <= biskorr) DO Inc(l);
    IF l > biskorr THEN exit;
    r := l;
    WHILE (dat(r, tr) >= schwunten) AND (r <= biskorr) DO Inc(r);
    IF r > biskorr THEN exit;
    extr           := schwunten;
    vorlaeufigkorr := l;
    stekorr        := l;
    WHILE stekorr <= r DO
    BEGIN
      hilf := dat(stekorr, tr);
      IF hilf > extr THEN
      BEGIN
        IF hilf > schwoben THEN GOTO zuhoch;
        extr           := hilf;
        vorlaeufigkorr := stekorr;
      END;
      Inc(stekorr);
    END;
    verfol.weiter(vorlaeufigkorr / korr);
  UNTIL verfol.aufhoeren;
END;

CONSTRUCTOR fensterminimum.neu;
BEGIN
  fenster.neu;
  Name := Name + 'minimum in window';
END;

PROCEDURE fensterminimum.blocktriggern(von, bis : typeextended);
LABEL
  zuniedrig;
VAR
  l, r :       bigint64;
  hilf, extr : typedouble;
  stekorr, vonkorr, biskorr, vorlaeufigkorr : bigint64;
BEGIN
  vonkorr := trunc(von * korr) + 1;
  biskorr := trunc(bis * korr);
  r       := vonkorr;
  REPEAT
    zuniedrig :
      l := r;
    WHILE (dat(l, tr) <= schwoben) AND (l <= biskorr) DO Inc(l);
    WHILE (dat(l, tr) > schwoben) AND (l <= biskorr) DO Inc(l);
    IF l > biskorr THEN exit;
    r := l;
    WHILE (dat(r, tr) <= schwoben) AND (r <= biskorr) DO Inc(r);
    IF r > biskorr THEN exit;
    extr           := schwoben;
    vorlaeufigkorr := l;
    stekorr        := l;
    WHILE stekorr <= r DO
    BEGIN
      hilf := dat(stekorr, tr);
      IF hilf < extr THEN
      BEGIN
        IF hilf < schwunten THEN GOTO zuniedrig;
        extr           := hilf;
        vorlaeufigkorr := stekorr;
      END;
      Inc(stekorr);
    END;
    verfol.weiter(vorlaeufigkorr / korr);
  UNTIL verfol.aufhoeren;
END;

{ Ein- und Austritt }

CONSTRUCTOR eintritt.neu;
BEGIN
  fenster.neu;
  Name := Name + 'entering window';
END;

PROCEDURE eintritt.blocktriggern(von, bis : typeextended);
VAR
  stekorr, vonkorr, biskorr : bigint64;
  hilf : typedouble;
BEGIN
  vonkorr := trunc(von * korr) + 1;
  biskorr := trunc(bis * korr);
  stekorr := vonkorr;
  REPEAT
    hilf := dat(stekorr, tr);
    WHILE (hilf >= schwunten) AND (hilf <= schwoben) AND (stekorr <= biskorr) DO
    BEGIN
      Inc(stekorr);
      hilf := dat(stekorr, tr);
    END;
    WHILE ((hilf < schwunten) OR (hilf > schwoben)) AND (stekorr <= biskorr) DO
    BEGIN
      Inc(stekorr);
      hilf := dat(stekorr, tr);
    END;
    IF stekorr > biskorr THEN exit;
    verfol.weiter((stekorr - 0.5) / korr);
  UNTIL verfol.aufhoeren;
END;

CONSTRUCTOR austritt.neu;
BEGIN
  fenster.neu;
  Name := Name + 'leaving window';
END;

PROCEDURE austritt.blocktriggern(von, bis : typeextended);
VAR
  stekorr, vonkorr, biskorr : bigint64;
  hilf : typedouble;
BEGIN
  vonkorr := trunc(von * korr) + 1;
  biskorr := trunc(bis * korr);
  stekorr := vonkorr;
  REPEAT
    hilf := dat(stekorr, tr);
    WHILE ((hilf < schwunten) OR (hilf > schwoben)) AND (stekorr <= biskorr) DO
    BEGIN
      Inc(stekorr);
      hilf := dat(stekorr, tr);
    END;
    WHILE (hilf >= schwunten) AND (hilf <= schwoben) AND (stekorr <= biskorr) DO
    BEGIN
      Inc(stekorr);
      hilf := dat(stekorr, tr);
    END;
    IF stekorr > biskorr THEN exit;
    verfol.weiter((stekorr - 0.5) / korr);
  UNTIL verfol.aufhoeren;
END;

{ triggerfilter }

PROCEDURE triggerfilter.vorbereitung(frequenz : EXTENDED);
BEGIN
  trdaten := tliste[trliste]^.fil[offennr];
END;

CONSTRUCTOR triggerfilter.load(VAR s : tbufstream);
BEGIN
  filter.load(s);
  s.Read(trliste, 1);
END;

PROCEDURE triggerfilter.store(VAR s : tbufstream);
BEGIN
  filter.store(s);
  s.Write(trliste, 1);
END;

{ doppeltriggerfilter }

PROCEDURE doppeltriggerfilter.vorbereitung(frequenz : EXTENDED);
BEGIN
  retrdaten := tliste[retrliste]^.fil[offennr];
  ertrdaten := tliste[ertrliste]^.fil[offennr];
END;

CONSTRUCTOR doppeltriggerfilter.load(VAR s : tbufstream);
BEGIN
  filter.load(s);
  s.Read(retrliste, 1);
  s.Read(ertrliste, 1);
END;

PROCEDURE doppeltriggerfilter.store(VAR s : tbufstream);
BEGIN
  filter.store(s);
  s.Write(retrliste, 1);
  s.Write(ertrliste, 1);
END;

{ zaehltfilter }

CONSTRUCTOR zaehltfilter.neu(trigliste : CHAR);
BEGIN
  trliste := trigliste;
  Name    := 'Counting of TL ' + trliste;
END;

PROCEDURE zaehltfilter.einheitgenerieren(VAR beleg : belegung);
BEGIN
  INHERITED einheitgenerieren(beleg);
  WITH beleg DO
  BEGIN
    factor  := 1;
    start  := '#';
    second := 0;
    negativ := False;
  END;
END;

FUNCTION zaehltfilter.gefiltert(posi : bigint64) : sample;
CONST
  maxminsample = maxsample - minsample;
VAR
  li, re : midint32;
BEGIN
  WITH trdaten DO
  BEGIN
    such(0, automn + 1, posi / korr, li, re);
    IF li <= maxsample THEN gefiltert := li
    ELSE
      gefiltert := maxsample;
  END;
END;

{ punktefilter }

CONSTRUCTOR punktefilter.neu(trigliste : CHAR);
BEGIN
  trliste := trigliste;
  Name    := 'Points TL ' + trliste;
END;

PROCEDURE punktefilter.einheitgenerieren(VAR beleg : belegung);
BEGIN
  INHERITED einheitgenerieren(beleg);
  WITH beleg DO
  BEGIN
    gepunktet   := True;
    gepunktettl := trliste;
  END;
END;

{ freqfilter }

CONSTRUCTOR freqfilter.neu(trigliste : CHAR);
BEGIN
  trliste := trigliste;
  Name    := 'Frequency of TL ' + trliste;
END;

PROCEDURE freqfilter.einheitgenerieren(VAR beleg : belegung);
BEGIN
  INHERITED einheitgenerieren(beleg);
  WITH beleg DO
  BEGIN
    factor  := fre / maxsample;
    start  := '1';
    second := -1;
    negativ := False;
  END;
END;

FUNCTION freqfilter.gefiltert(posi : bigint64) : sample;
VAR
  li, re : midint32;
BEGIN
  WITH trdaten DO
  BEGIN
    such(0, automn + 1, posi / korr, li, re);
    IF (li = re) AND (li > 0) THEN Dec(li);
    IF (li = 0) OR (re = automn + 1) THEN gefiltert := 0
    ELSE
      gefiltert := round(maxsample / (autom^[re] - autom^[li]));
  END;
END;

{ intervallfilter }

CONSTRUCTOR intervallfilter.neu(trigliste : CHAR);
BEGIN
  trliste := trigliste;
  Name    := 'Interval of TL ' + trliste;
END;

PROCEDURE intervallfilter.einheitgenerieren(VAR beleg : belegung);
BEGIN
  INHERITED einheitgenerieren(beleg);
  WITH beleg DO
  BEGIN
    factor  := 1 / fre;
    start  := '';
    second := 1;
    negativ := False;
  END;
END;

FUNCTION intervallfilter.gefiltert(posi : bigint64) : sample;
VAR
  li, re : midint32;
  t :      EXTENDED;
BEGIN
  WITH trdaten DO
  BEGIN
    such(0, automn + 1, posi / korr, li, re);
    IF (li = re) AND (li > 0) THEN Dec(li);
    IF (li = 0) OR (re = automn + 1) THEN gefiltert := 0
    ELSE
    BEGIN
      t := autom^[re] - autom^[li];
      IF t < maxsample THEN gefiltert := round(t)
      ELSE
        gefiltert := maxsample;
    END;
  END;
END;

{ polygonfilter }

CONSTRUCTOR polygonfilter.neu(trigliste : CHAR);
BEGIN
  trliste := trigliste;
  Name    := 'Polygon (' + trliste + ')';
END;

PROCEDURE polygonfilter.vorbereitung(frequenz : EXTENDED);
BEGIN
  triggerfilter.vorbereitung(frequenz);
  erneut := False;
END;

FUNCTION polygonfilter.gefiltert(posi : bigint64) : sample;
VAR
  li, re : midint32;
BEGIN
  WITH trdaten DO
  BEGIN
    such(0, automn + 1, posi / korr, li, re);
    IF li < 1 THEN li := 1;
    IF re > automn THEN re := automn;
    IF NOT erneut OR (li <> lialt) OR (re <> realt) THEN
    BEGIN
      lialt  := li;
      realt  := re;
      erneut := True;
      liposi := zwi(autom^[li]);
      reposi := zwi(autom^[re]);
      liwert := Next^.gefiltert(liposi);
      rewert := Next^.gefiltert(reposi);
    END;
    IF li = re THEN gefiltert := round(liwert)
    ELSE
      gefiltert               := trunc(((reposi - posi) * liwert + (posi - liposi) * rewert) / (reposi - liposi));
  END;
END;

{ asciifilter }

CONSTRUCTOR asciifilter.neu(aname : STRING; trigliste : CHAR);
VAR
  afile : Text;
  i :     bigint64;
  dummy : EXTENDED;
  atmax : ywert;
BEGIN
  trliste := trigliste;
  fillchar(aliste, (sizeof(aliste)), 0);
  Name := 'ASCII - Read Error "' + aname + '"';
  amax := 1;
  Assign(afile, aname);
  reset(afile);
  IF lesefehler THEN exit;
  i     := 0;
  atmax := 0;
  WHILE NOT EOF(afile) DO
  BEGIN
    Inc(i);
    readln(afile, dummy, aliste[i]);
    IF abs(aliste[i]) > atmax THEN atmax := abs(aliste[i]);
    IF atmax > 0 THEN amax := atmax;
    IF lesefehler OR (i >= triggermax) THEN exit;
  END;
  Close(afile);
  Name := 'ASCII (' + trliste + '+' + aname + ')';
END;

PROCEDURE asciifilter.vorbereitung(frequenz : EXTENDED);
BEGIN
  triggerfilter.vorbereitung(frequenz);
END;

PROCEDURE asciifilter.einheitgenerieren(VAR beleg : belegung);
BEGIN
  INHERITED einheitgenerieren(beleg);
  WITH beleg DO
  BEGIN
    factor  := amax / maxsample / 0.9;
    start  := 'U';
    second := 0;
    negativ := True;
  END;
END;

FUNCTION asciifilter.gefiltert(posi : bigint64) : sample;
VAR
  li, re :         midint32;
  liposi, reposi : bigint64;
  liwert, rewert : EXTENDED;
BEGIN
  WITH trdaten DO
  BEGIN
    such(0, automn + 1, posi / korr, li, re);
    IF li < 1 THEN li := 1;
    IF re > automn THEN re := automn;
    liposi := zwi(autom^[li]);
    reposi := zwi(autom^[re]);
    liwert := aliste[li] / amax * (maxsample * 0.9);
    rewert := aliste[re] / amax * (maxsample * 0.9);
    IF li = re THEN gefiltert := round(liwert)
    ELSE
      gefiltert               := trunc(((reposi - posi) * liwert + (posi - liposi) * rewert) / (reposi - liposi));
  END;
END;

CONSTRUCTOR asciifilter.load(VAR s : tbufstream);
BEGIN
  triggerfilter.load(s);
  s.Read(aliste, sizeof(alistentyp));
  s.Read(amax, sizeof(ywert));
END;

PROCEDURE asciifilter.store(VAR s : tbufstream);
BEGIN
  triggerfilter.store(s);
  s.Write(aliste, sizeof(alistentyp));
  s.Write(amax, sizeof(ywert));
END;

{ diffilter - alt}

CONSTRUCTOR diffilteralt.neu(retrigliste, ertrigliste : CHAR; msmax : bigint64);
BEGIN
  retrliste := retrigliste;
  ertrliste := ertrigliste;
  smax      := msmax / 1000;
  Name      := 'Diff. TL ' + retrliste + ' minus ' + ertrliste + ' (ñ' + wort(msmax) + 'ms)';
END;

PROCEDURE diffilteralt.einheitgenerieren(VAR beleg : belegung);
BEGIN
  INHERITED einheitgenerieren(beleg);
  WITH beleg DO
  BEGIN
    factor  := smax / maxsample;
    start  := '';
    second := 1;
    negativ := True;
  END;
END;

FUNCTION diffilteralt.gefiltert(posi : bigint64) : sample;
VAR
  stelle :       typeextended;
  li, re :       midint32;
  rebei, erbei : typeextended;
BEGIN
  stelle := posi / korr;
  WITH retrdaten DO
  BEGIN
    such(0, automn + 1, stelle, li, re);
    IF (stelle - autom^[li]) > (autom^[re] - stelle) THEN rebei := autom^[re]
    ELSE
      rebei := autom^[li];
  END;
  WITH ertrdaten DO
  BEGIN
    such(0, automn + 1, rebei, li, re);
    IF (li = 0) OR (re = automn + 1) THEN
    BEGIN
      gefiltert := 0;
      exit;
    END;
    IF (rebei - autom^[li]) > (autom^[re] - rebei) THEN erbei := autom^[re]
    ELSE
      erbei := autom^[li];
  END;
  gefiltert := round((erbei - rebei) / fre / smax * maxsample);
END;

CONSTRUCTOR diffilteralt.load(VAR s : tbufstream);
BEGIN
  doppeltriggerfilter.load(s);
  s.Read(smax, sizeof(EXTENDED));
END;

PROCEDURE diffilteralt.store(VAR s : tbufstream);
BEGIN
  doppeltriggerfilter.store(s);
  s.Write(smax, sizeof(EXTENDED));
END;

{ diffilter - neu }

CONSTRUCTOR diffilter.neu(retrigliste, ertrigliste : CHAR; msmax : bigint64; typchar : CHAR);
BEGIN
  retrliste := retrigliste;
  ertrliste := ertrigliste;
  CASE typchar OF
    'F' : typ := vorwaerts;
    'B' : typ := rueckwaerts;
    ELSE
      typ := naechster;
  END;
  smax := msmax / 1000;
  Name := 'Diff. TL ' + retrliste + ' minus ' + ertrliste + ' (ñ' + wort(msmax) + 'ms,' + typchar + ')';
END;

PROCEDURE diffilter.einheitgenerieren(VAR beleg : belegung);
BEGIN
  INHERITED einheitgenerieren(beleg);
  WITH beleg DO
  BEGIN
    factor  := smax / maxsample;
    start  := '';
    second := 1;
    negativ := True;
  END;
END;

FUNCTION diffilter.gefiltert(posi : bigint64) : sample;
VAR
  stelle :       typeextended;
  li, re :       midint32;
  rebei, erbei : typeextended;
BEGIN
  stelle := posi / korr;
  WITH retrdaten DO
  BEGIN
    such(0, automn + 1, stelle, li, re);
    IF (stelle - autom^[li]) > (autom^[re] - stelle) THEN rebei := autom^[re]
    ELSE
      rebei := autom^[li];
  END;
  WITH ertrdaten DO
  BEGIN
    such(0, automn + 1, rebei, li, re);
    IF (li = 0) OR (re = automn + 1) THEN
    BEGIN
      gefiltert := 0;
      exit;
    END;
    CASE typ OF
      naechster : IF (rebei - autom^[li]) > (autom^[re] - rebei) THEN erbei := autom^[re]
        ELSE
          erbei := autom^[li];
      vorwaerts : erbei := autom^[re];
      rueckwaerts : erbei := autom^[li];
    END;
  END;
  gefiltert := round((erbei - rebei) / fre / smax * maxsample);
END;

CONSTRUCTOR diffilter.load(VAR s : tbufstream);
BEGIN
  doppeltriggerfilter.load(s);
  s.Read(smax, sizeof(EXTENDED));
END;

PROCEDURE diffilter.store(VAR s : tbufstream);
BEGIN
  doppeltriggerfilter.store(s);
  s.Write(smax, sizeof(EXTENDED));
END;

{ phasenfilter }

CONSTRUCTOR phasenfilter.neu(retrigliste, ertrigliste : CHAR; perioden : bigint64);
BEGIN
  retrliste := retrigliste;
  ertrliste := ertrigliste;
  peri      := perioden;
  IF peri = 0 THEN Name := 'Phase TL ' + ertrliste + ' in ' + retrliste
  ELSE
  IF peri > 0 THEN Name := 'Phase TL ' + ertrliste + ' in ' + retrliste + '+' + wort(peri)
  ELSE
    Name                := 'Phase TL ' + ertrliste + ' in ' + retrliste + wort(peri);
END;

PROCEDURE phasenfilter.einheitgenerieren(VAR beleg : belegung);
BEGIN
  INHERITED einheitgenerieren(beleg);
  WITH beleg DO
  BEGIN
    factor  := 1 / maxsample;
    start  := '';
    second := 0;
    negativ := False;
  END;
END;

FUNCTION phasenfilter.gefiltert(posi : bigint64) : sample;
VAR
  stelle :         typeextended;
  li, re{,lialt} : midint32;
  lip, rep :       bigint64;
  repbeili, repbeire, rebeili, erbei : typeextended;
BEGIN
  stelle := posi / korr;
  WITH ertrdaten DO
  BEGIN
    such(0, automn + 1, stelle, li, re);
{ Begrenzung auf die Referenzphase, faellt wohl mit peri weg
   lialt:=li;
   if autom^[li]<rebeili then li:=re;
   if autom^[re]>rebeire then re:=lialt;
   if re<li then begin gefiltert:=0; exit end;}
    IF (stelle - autom^[li]) > (autom^[re] - stelle) THEN erbei := autom^[re]
    ELSE
      erbei := autom^[li];
    WITH retrdaten DO
    BEGIN
      such(0, automn + 1, erbei, li, re);
      IF li = re THEN Inc(re);
      lip := li + peri;
      rep := re + peri;
      IF (lip <= 0) OR (rep >= automn + 1) OR (li <= 0) THEN
      BEGIN
        gefiltert := 0;
        exit;
      END;
      repbeili := autom^[lip];
      repbeire := autom^[rep];
      rebeili  := autom^[li];
    END;
  END;
  gefiltert := round((erbei - rebeili) / (repbeire - repbeili) * maxsample);
END;

CONSTRUCTOR phasenfilter.load(VAR s : tbufstream);
BEGIN
  doppeltriggerfilter.load(s);
  s.Read(peri, sizeof(bigint64));
END;

PROCEDURE phasenfilter.store(VAR s : tbufstream);
BEGIN
  doppeltriggerfilter.store(s);
  s.Write(peri, sizeof(bigint64));
END;

{ TL-Integrationsfilter }

CONSTRUCTOR tlintfilter.neu(trigliste : CHAR);
BEGIN
  trliste := trigliste;
  Name    := '* dt [TL ' + trliste + ']';
END;

PROCEDURE tlintfilter.einheitgenerieren(VAR beleg : belegung);
BEGIN
  filter.einheitgenerieren(beleg);
  WITH beleg DO
  BEGIN
    factor := factor / fre;
    Inc(second);
  END;
END;

PROCEDURE tlintfilter.vorbereitung(frequenz : EXTENDED);
BEGIN
  triggerfilter.vorbereitung(frequenz);
  posiwert      := 0;
  gefiltertwert := 0;
END;

FUNCTION tlintfilter.gefiltert(posi : bigint64) : sample;
VAR
  i :      bigint64;
  li, re : midint32;
BEGIN
  WITH trdaten DO
  BEGIN
    such(0, automn + 1, posi / korr, li, re);
    IF (re <= automn) THEN IF autom^[re] <= posiwert / korr THEN
        IF li = 0 THEN
        BEGIN
          posiwert      := 0;
          gefiltertwert := 0;
        END
        ELSE
        BEGIN
          posiwert      := zwi(autom^[li]);
          gefiltertwert := 0;
        END;
    IF (li > 0) THEN IF autom^[li] >= posiwert / korr THEN
      BEGIN
        posiwert      := zwi(autom^[li]);
        gefiltertwert := 0;
      END;
  END;
  IF posi < posiwert THEN
  BEGIN
    i := posi + 1;
    WHILE i <= posiwert DO
    BEGIN
      Dec(gefiltertwert, Next^.gefiltert(i));
      Inc(i);
    END;
  END
  ELSE
  BEGIN
    i := posiwert + 1;
    WHILE i <= posi DO
    BEGIN
      Inc(gefiltertwert, Next^.gefiltert(i));
      Inc(i);
    END;
  END;
  posiwert  := posi;
  gefiltert := gefiltertwert;
END;

{ triggerweiser }

PROCEDURE triggerweiser.zaehlen(VAR feld : triggerung; minabst, maxabst : typeextended);
VAR
  nr, tp, n : LONGINT;
  abst :      typeextended;
  sum :       EXTENDED;
BEGIN
  gesamt := 0;
  sum    := 0;
  FOR nr := 1 TO filenr DO WITH weisliste[nr], feld, fil[nr] DO
    BEGIN
      l := max(automn - 1, 0);
      getmem(t, sizeof(midint32) * l);
      IF automda THEN
      BEGIN
        n := 0;
        FOR tp := 1 TO automn - 1 DO
        BEGIN
          abst := autom^[tp + 1] - autom^[tp];
          IF (abst <= maxabst) AND (abst >= minabst) THEN
          BEGIN
            Inc(n);
            t^[n] := tp;
            sum   := sum + abst;
          END;
        END;
        weisliste[nr].n := n;
        Inc(gesamt, n);
      END;
    END;
  IF gesamt <> 0 THEN mittelabstand := sum / gesamt;
END;

PROCEDURE triggerweiser.frei;
VAR
  nr : midint32;
BEGIN
  FOR nr := 1 TO filenr DO WITH weisliste[nr] DO freemem(t, sizeof(midint32) * l);
END;


PROCEDURE kontrolle(nr : BYTE; trind : CHAR);
VAR
  pzeiger, philf : PBulletList;
  i :              LONGINT;
BEGIN
  IF trind IN ['A'..listmax] THEN
    WITH liste[nr], tliste[trind]^.fil[nr] DO
    BEGIN
      pzeiger := selectedbullet^^.Next;
      WHILE pzeiger <> nil DO
      BEGIN
        philf         := pzeiger^.Prev;
        pzeiger^.Prev := philf^.Prev;
        dispose(philf, done);
        selectedbullet^ := pzeiger;
        pzeiger         := pzeiger^.Next;
      END;
      pzeiger := selectedbullet^^.Prev;
      philf   := selectedbullet^;
      FOR i := 1 TO automn DO
      BEGIN
        new(pzeiger^.Next, new);
        pzeiger^.Next^.Prev := pzeiger;
        pzeiger             := pzeiger^.Next;
        pzeiger^.bei        := autom^[i];
      END;
      pzeiger^.Next := philf;
      philf^.Prev   := pzeiger;
    END;
END;

PROCEDURE streamput(VAR s : tbufstream);
VAR
  ind : CHAR;
BEGIN
  s.Write(akttrkan, sizeof(akttrkan));
  FOR ind := 'A' TO listmax DO s.put(tliste[ind]);
END;

PROCEDURE streamget(VAR s : tbufstream);
VAR
  ind : CHAR;
BEGIN
  s.Read(akttrkan, sizeof(akttrkan));
  IF komp84 THEN
    FOR ind := 'A' TO 'H' DO
    BEGIN
      dispose(tliste[ind], alt);
      tliste[ind] := triggerungzg(s.get);
    END
  ELSE
    FOR ind := 'A' TO listmax DO
    BEGIN
      dispose(tliste[ind], alt);
      tliste[ind] := triggerungzg(s.get);
    END;
END;

PROCEDURE triggeruebersicht;
VAR
  trind : CHAR;
BEGIN
  writeln('List' : 5, 'Channel' : 8, 'Evts.' : 8, 'Files' : 6, '  Mode and Label');
  FOR trind := 'A' TO listmax DO WITH tliste[trind]^ DO
    BEGIN
      writeln(trind : 2, schriftliste[tr] : 11, triggsum : 8, fileanz : 4, '    ', tliste[trind]^.Name);
    END;
END;


PROCEDURE manager;
VAR
  trind : CHAR;
  i :     LONGINT;

  PROCEDURE aktuellkanal;
  VAR
    liste : filterliste;
    i :     BYTE;
  BEGIN
    showtitle(False, 'Trigger Channel', 'Info', farbe3);
    belegungzeigen;
    writeln;
    liste.zeigen(8, kan);
    writeln;
    zwischen('Dialogue', farbe3);
    Write(lfcr, 'Continue list? (Y/N) ');
    WHILE NOT liste.ende AND (readkey IN ['Y', 'y', 'J', 'j']) DO liste.weiterzeigen;
    Write(#13);
    clreol;
    i := readint('Trigger channel', 0);
    IF NOT (i IN [0..kan + maxfilters - 1]) THEN
    BEGIN
      fehler('Undefined channel no.');
      warte;
    END
    ELSE
      akttrkan := i;
  END;

  PROCEDURE triggneu;
  CONST
    ta : CHAR = 'A';
  VAR
    artbu : CHAR;
  BEGIN
    showtitle(False, 'Trigger Mode', 'Info', farbe3);
    writeln('Trigger channel: ', akttrkan, ' (', schriftliste[akttrkan], ')', lfcr);
    triggeruebersicht;
    writeln;
    writeln('Modes:   r = Rising Threshold              f = Falling Threshold', lfcr,
      '       x/a = Max. > Gl. Av./in Window    n/i = Min. < Gl. Av./in Window', lfcr,
      {        '       a = Maximum in Window          i = Minimum in Window',lfcr,}
      '         > = Entering Window               < = Leaving Window', lfcr,
      '         p = User Defined                  e = Equidistant', lfcr,
      '         t = Read from text file           u = Undefined');
    {writeln;}
    gotoxy(1, 21);
    zwischen('Dialogue', farbe3);
    window(1, 25, 80, zeilmax);
    ta := upcase(readchar('Trigger list', 'A'));
    IF NOT (ta IN ['A'..listmax]) THEN
    BEGIN
      fehler('Undefined trigger list');
      warte;
      exit;
    END;
    artbu := readchar('Trigger mode', 'r');
    IF NOT (artbu IN ['u', 'r', 'f', 'x', 'n', 'p', 'a', 'i', 'e', '>', '<', 't']) THEN
    BEGIN
      fehler('Undefined trigger mode');
      warte;
      exit;
    END;
    dispose(tliste[ta], alt);
    CASE artbu OF
      'u' : tliste[ta] := new(keinezg, neu);
      'r' : tliste[ta] := new(hochzg, neu);
      'f' : tliste[ta] := new(runterzg, neu);
      'x' : tliste[ta] := new(maximumzg, neu);
      'n' : tliste[ta] := new(minimumzg, neu);
      'p' : tliste[ta] := new(punktezg, neu);
      'a' : tliste[ta] := new(fenstermaximumzg, neu);
      'i' : tliste[ta] := new(fensterminimumzg, neu);
      '<' : tliste[ta] := new(austrittzg, neu);
      '>' : tliste[ta] := new(eintrittzg, neu);
      'e' : tliste[ta] := new(aequidistantzg, neu);
      't' : tliste[ta] := new(gelesenzg, neu);
    END;
    WITH tliste[ta]^ DO Name := readstring('Label', Name);
  END;

  PROCEDURE konditionen;
  VAR
    puff : LONGINT;
  BEGIN
    showtitle(False, 'Trigger Conditions', 'Info', farbe3);
    writeln('Trigger range (complete Files or Blocks) : ', bloecketext[bloecke],
      lfcr, 'Maximum number of trigger points         : ', triggeranz,
      lfcr, 'Start at trigger event no.               : ', triggeranf,
      lfcr, 'Trigger point selection each             : ', triggerabz, '. event',
      lfcr, 'Skip trigger events up to                : ', zeit(triggerdst), ' ms');
    writeln;
    zwischen('Dialogue', farbe3);
    writeln;
    bloecke := upcase(readchar('Trigger range (f=complete Files, b=Blocks)                 ',
      bloeckeb[bloecke])) = upcase(bloeckeb[True]);
    puff    := readint('Maximum number of trigger points per file (max.' + wort(triggermax) + ')    ', triggeranz);
    IF (puff <= 0) OR (puff > triggermax) THEN
    BEGIN
      fehler('Number of trigger points out of range.');
      warte;
    END
    ELSE
      triggeranz := puff;
    puff         := readint('Start at trigger event no.                                 ', 1);
    IF puff <= 0 THEN
    BEGIN
      fehler('Trigger event out of range.');
      warte;
      exit;
    END;
    triggeranf := puff;
    puff       := readint('Trigger point selection, each 1., 2., 3., ... trigger event', 1);
    IF puff <= 0 THEN
    BEGIN
      fehler('Number out of range.');
      warte;
      exit;
    END;
    triggerabz := puff;
    puff       := readint('Skip following trigger events up to [ms]                   ', 0);
    IF puff < 0 THEN
    BEGIN
      fehler('Number out of range.');
      warte;
      exit;
    END;
    triggerdst := messw(puff);
  END;

  PROCEDURE ausfuehren;
  VAR
    welche : matrix;
    ta :     CHAR;

    PROCEDURE loeschen;
    BEGIN
      window(1, zeilmax - 3, 80, zeilmax);
      clrscr;
      window(1, 3, 80, zeilmax);
    END;

  BEGIN
    REPEAT
      showtitle(filenr > 10, 'Triggering', 'Info', farbe3);
      writeln(lfcr, '' : 27, 'Content of trigger lists:', lfcr);
      mat.uebernehmen;
      mat.ausgabe;
      gotoxy(1, zeilmax - 8);
      zwischen('Dialogue', farbe3);
      loeschen;
      gotoxy(1, zeilmax - 4);
      gotoxy(1, zeilmax - 6);
      clreol;
      welche.eingabe;
      IF welche.escape THEN exit;
      loeschen;
      gotoxy(1, zeilmax - 4);
      IF welche.unsinn THEN
      BEGIN
        fehler('Incorrect table position.');
        warte;
      END
      ELSE
      BEGIN
        new(aut);
        abbruch := False;
        writeln;
        FOR ta := 'A' TO listmax DO
        BEGIN
          mat.tn := ta;
          tliste[ta]^.triggern(welche.tl[ta]);
          IF abbruch THEN
          BEGIN
            dispose(aut);
            exit;
          END;
        END;
        dispose(aut);
      END;
    UNTIL False;
  END;

  PROCEDURE triggerfile;
  CONST
    filename : string80 = 'trigger.txt';
    ta : CHAR           = 'A';
  VAR
    ausgabe :  Text;
    fn, i, j : midint32;
    ykanaele : TChannelVolume;
    zkn :      INTEGER;
  BEGIN
    showtitle(False, 'Export Trigger Data', 'Info', farbe3);
    triggeruebersicht;
    gotoxy(1, 18);
    zwischen('Dialogue', farbe3);
    window(1, 22, 80, zeilmax);
    ta := upcase(readchar('Trigger list', 'A'));
    IF NOT (ta IN ['A'..listmax]) THEN
    BEGIN
      fehler('Undefined trigger list');
      warte;
      exit;
    END;
    filename := readstring('File name and path', filename);
    IF fileschonda(filename) THEN
      IF upcase(readchar('Overwrite? (Y/N)', 'N')) <> 'Y' THEN exit;
    window(1, 3, 80, zeilmax);
    clrscr;
    IF tliste[ta]^.tr = maxchannelsandfilters THEN ykanaele.channelnumber := 0
    ELSE
    BEGIN
      ykanaele.channelnumber   := 1;
      ykanaele.k[1] := tliste[ta]^.tr;
    END;
    ykanaele.read(8, farbe3);
    Assign(ausgabe, filename);
    rewrite(ausgabe);
    WITH tliste[ta]^ DO
    BEGIN
      writeln(ausgabe, ' " Trigger list: ' + Name + ' "');
      Write(ausgabe, ' " Data file' : 12, 'Time [ms]' : 20);
      FOR j := 0 TO maxchannelsandfilters - 1 DO IF (j IN ykanaele.dabei) THEN
          Write(ausgabe, schriftliste[j] + ' [' + belegungsliste[j].getunit + ']' : 20);
      writeln(ausgabe, ' "');
      FOR fn := 1 TO filenr DO WITH fil[fn] DO
        BEGIN
          openfile(fn);
          IF automda THEN FOR i := 1 TO automn DO
            BEGIN
              Write(ausgabe, fn : 12, extzeit(autom^[i]) : 20 : 3);
              FOR j := 0 TO maxchannelsandfilters - 1 DO IF (j IN ykanaele.dabei) THEN
                  Write(ausgabe, extspannung(dat(zwi(autom^[i]), j), j) : 20 : 6);
              writeln(ausgabe);
            END;
          schliesse;
        END;
    END;
    Close(ausgabe);
  END;

BEGIN
  REPEAT
    showtitle(False, 'Trigger Manager', 'Info', farbe2);
    writeln('Channel   : ', akttrkan, ' (', schriftliste[akttrkan], ')');
    writeln('Conditions: ', bloecketext[bloecke], ',', 'max. ', triggeranz,
      ',start at ', triggeranf, '.,each ', triggerabz, '. event,skipping ',
      zeit(triggerdst), 'ms');
    writeln;
    triggeruebersicht;
    writeln;
    zwischen('Menu', farbe2);
    writeln(lfcr, '  h...Channel     c...Conditions    b...Create Blocks   e...Export Data',
      lfcr, '  o...Mode        t...Triggering                        m...Main Menu');
    writeln;
    zwischen('Dialogue', farbe2);
    writeln;
    trind := readcharim('Menu Point', 'm');
    writeln;
    CASE upcase(trind) OF
      'T' : ausfuehren;
      'O' : triggneu;
      'C' : konditionen;
      'H' : aktuellkanal;
      'E' : triggerfile;
      'B' : BEGIN
        autoblock(0);
        warte;
      END;
      'M' : exit;
    END;
  UNTIL False;
END;

BEGIN

  registertype(rkeine);
  registertype(rpunkte);
  registertype(rgelesen);
  registertype(rhoch);
  registertype(rrunter);
  registertype(rminimum);
  registertype(rmaximum);
  registertype(rfenstermaximum);
  registertype(rfensterminimum);
  registertype(reintritt);
  registertype(raustritt);

  registertype(raequidistant);

  FOR trind := 'A' TO listmax DO tliste[trind] := new(keinezg, neu);

  registertype(rfreqfilter);
  registertype(rpolygonfilter);
  registertype(rdiffilteralt);
  registertype(rphasenfilter);
  registertype(rpunktefilter);
  registertype(rzaehltfilter);
  registertype(rintervallfilter);
  registertype(rdiffilter);
  registertype(rasciifilter);
  registertype(rtlintfilter);
END.
