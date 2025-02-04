{ Borland-Pascal 7.0 / FPC 3.2.2 }
{$ifdef fpc} {$mode TP} {$endif}

UNIT  tlfilter;

{$IFDEF MSDOS}
{$A+,B-,E+,F-,G-,I-,N+,O-,P+,T+,V+,X-}
{$ELSE}
{$A+,B-,E+,F-,G+,I-,N+,P+,T+,V+,X-}
{$ENDIF}

INTERFACE

USES  objects, plotter, bequem, daff, wavpcm, tulab42;

CONST
  maxfilters = 16;
  maxchannelsandfilters  = maxchannels + maxfilters;

  genau = 4;
  weite = 2000;

  spikemax = 300;

TYPE  { Einheiten der Y-Achsen }
  einheitstring = STRING[7];

  einheittyp = OBJECT
    faktor :  EXTENDED;
    vor :     STRING[1];
    anfang :  STRING[7];
    sekunde : INTEGER;
    PROCEDURE kopie(VAR ein : einheittyp);
    FUNCTION einhwort : einheitstring;
    FUNCTION plot : string80;
    PROCEDURE handlich;
    PROCEDURE schwierig;
  END;

  { Rohform der Einheiten der ungefilterten Daten }
  grundtyp = OBJECT(einheittyp)
    gain, multi : EXTENDED;
    PROCEDURE setz(m : EXTENDED; anf : einheitstring);
  END;
  grundlistetyp = PACKED ARRAY[0..maxchannels - 1] OF grundtyp;

  { Endform der Einheiten der gefilterten Daten }
  belegung = OBJECT(einheittyp)
    negativ, gepunktet, schwierigda : BOOLEAN;
    gepunktettl : CHAR;
    PROCEDURE grundsetzen(k : BYTE);
  END;

  { Beschriftungen der Y-Achsen }
  schriftlistentyp = PACKED ARRAY[0..maxchannelsandfilters] OF STRING[10];

  { Abstraktes Filterobjekt }
  filterzeiger = ^filter;

  filter = OBJECT(TObject)
    Name : string80;
    Next : filterzeiger;
    DESTRUCTOR alt; VIRTUAL;
    PROCEDURE einheitgenerieren(VAR beleg : belegung); VIRTUAL;
    PROCEDURE vorbereitung(frequenz : EXTENDED); VIRTUAL;
    FUNCTION gefiltert(posi : bigint64) : sample; VIRTUAL;
    CONSTRUCTOR load(VAR s : tbufstream);
    PROCEDURE store(VAR s : tbufstream);
  END;

  { abstakter Filter mit Breite }
  breitefilter = OBJECT(filter)
  PUBLIC
    PROCEDURE neu(breims : EXTENDED);
  PRIVATE
    brei :              EXTENDED;
    breianz, breianz2 : bigint64;
    PROCEDURE vorbereitung(frequenz : EXTENDED); VIRTUAL;
    CONSTRUCTOR load(VAR s : tbufstream);
    PROCEDURE store(VAR s : tbufstream);
  END;

  { arcsin - Filter }
  arcsinzg = ^arcsin;

  arcsin = OBJECT(filter)
  PUBLIC
    CONSTRUCTOR neu;
  PRIVATE
    PROCEDURE einheitgenerieren(VAR beleg : belegung); VIRTUAL;
    FUNCTION gefiltert(posi : bigint64) : sample; VIRTUAL;
  END;

  { arccos - Filter }
  arccoszg = ^arccos;

  arccos = OBJECT(filter)
  PUBLIC
    CONSTRUCTOR neu;
  PRIVATE
    PROCEDURE einheitgenerieren(VAR beleg : belegung); VIRTUAL;
    FUNCTION gefiltert(posi : bigint64) : sample; VIRTUAL;
  END;

  { *(-1) - Filter }
  invertzg = ^invert;

  invert = OBJECT(filter)
  PUBLIC
    CONSTRUCTOR neu;
  PRIVATE
    PROCEDURE einheitgenerieren(VAR beleg : belegung); VIRTUAL;
    FUNCTION gefiltert(posi : bigint64) : sample; VIRTUAL;
  END;

  { 1/X - Filter}
  einsdurchzg = ^einsdurch;

  einsdurch = OBJECT(filter)
  PUBLIC
    CONSTRUCTOR neu;
  PRIVATE
    PROCEDURE einheitgenerieren(VAR beleg : belegung); VIRTUAL;
    FUNCTION gefiltert(posi : bigint64) : sample; VIRTUAL;
  END;

  { +/- Offset - Filter }
  offsetzg = ^offset;

  offset = OBJECT(filter)
  PUBLIC
    CONSTRUCTOR neu(k : BYTE; hoch : sample);
  PRIVATE
    ho : sample;
    FUNCTION gefiltert(posi : bigint64) : sample; VIRTUAL;
    CONSTRUCTOR load(VAR s : tbufstream);
    PROCEDURE store(VAR s : tbufstream);
  END;

  { *m (Verstaerkungs-) - Filter }
  malfaktorzg = ^malfaktor;

  malfaktor = OBJECT(filter)
  PUBLIC
    CONSTRUCTOR neu(malfak : EXTENDED);
  PRIVATE
    float : EXTENDED;
    FUNCTION gefiltert(posi : bigint64) : sample; VIRTUAL;
    CONSTRUCTOR load(VAR s : tbufstream);
    PROCEDURE store(VAR s : tbufstream);
  END;

  { y-Achsenstreckungs - Filter }
  streckungzg = ^streckung;

  streckung = OBJECT(filter)
  PUBLIC
    CONSTRUCTOR neu(k : BYTE; maxspannung : EXTENDED);
  PRIVATE
    float : EXTENDED;
    PROCEDURE einheitgenerieren(VAR beleg : belegung); VIRTUAL;
    FUNCTION gefiltert(posi : bigint64) : sample; VIRTUAL;
    CONSTRUCTOR load(VAR s : tbufstream);
    PROCEDURE store(VAR s : tbufstream);
  END;

  { Kappung aller Werte < minsample und > maxsample }
  kappenzg = ^kappen;

  kappen = OBJECT(filter)
  PUBLIC
    CONSTRUCTOR neu;
  PRIVATE
    FUNCTION gefiltert(posi : bigint64) : sample; VIRTUAL;
  END;

  { Additionsfilter }
  additionzg = ^addition;

  addition = OBJECT(filter)
  PUBLIC
    CONSTRUCTOR neu(additionskanal : BYTE);
  PRIVATE
    adk : BYTE;
    PROCEDURE einheitgenerieren(VAR beleg : belegung); VIRTUAL;
    FUNCTION gefiltert(posi : bigint64) : sample; VIRTUAL;
    CONSTRUCTOR load(VAR s : tbufstream);
    PROCEDURE store(VAR s : tbufstream);
  END;

  { Betragssfilter }
  betragzg = ^betrag;

  betrag = OBJECT(filter)
  PUBLIC
    CONSTRUCTOR neu(kanal2 : BYTE);
  PRIVATE
    k2 : BYTE;
    PROCEDURE einheitgenerieren(VAR beleg : belegung); VIRTUAL;
    FUNCTION gefiltert(posi : bigint64) : sample; VIRTUAL;
    CONSTRUCTOR load(VAR s : tbufstream);
    PROCEDURE store(VAR s : tbufstream);
  END;

  { Winkel }
  winkelzg = ^winkel;

  winkel = OBJECT(filter)
  PUBLIC
    CONSTRUCTOR neu(xkanal : BYTE);
  PRIVATE
    xk : BYTE;
    PROCEDURE einheitgenerieren(VAR beleg : belegung); VIRTUAL;
    FUNCTION gefiltert(posi : bigint64) : sample; VIRTUAL;
    CONSTRUCTOR load(VAR s : tbufstream);
    PROCEDURE store(VAR s : tbufstream);
  END;

  { Korrelationsfilter }
  korrelationzg = ^korrelation;

  korrelation = OBJECT(breitefilter)
  PUBLIC
    CONSTRUCTOR neu(korrkanal : BYTE; breims : EXTENDED);
  PRIVATE
    kk : BYTE;
    PROCEDURE einheitgenerieren(VAR beleg : belegung); VIRTUAL;
    FUNCTION gefiltert(posi : bigint64) : sample; VIRTUAL;
    CONSTRUCTOR load(VAR s : tbufstream);
    PROCEDURE store(VAR s : tbufstream);
  END;

  { Absolutbetrags - Filter }
  absolutzg = ^absolut;

  absolut = OBJECT(filter)
  PUBLIC
    CONSTRUCTOR neu;
  PRIVATE
    PROCEDURE einheitgenerieren(VAR beleg : belegung); VIRTUAL;
    FUNCTION gefiltert(posi : bigint64) : sample; VIRTUAL;
  END;

  { Quadrat - Filter }
  squarezg = ^square;

  square = OBJECT(absolut)
  PUBLIC
    CONSTRUCTOR neu;
  PRIVATE
    PROCEDURE einheitgenerieren(VAR beleg : belegung); VIRTUAL;
    FUNCTION gefiltert(posi : bigint64) : sample; VIRTUAL;
  END;

  { Glaettungs - Filter }
  glattzg = ^glatt;

  glatt = OBJECT(breitefilter)
  PUBLIC
    CONSTRUCTOR neu(breims : EXTENDED);
  PRIVATE
    FUNCTION gefiltert(posi : bigint64) : sample; VIRTUAL;
  END;

  { abstrakter Passfilter }
  spaltzeiger = ^spaltfeld;
  spaltfeld   = ARRAY[0..weite] OF SINGLE;

  passfilter = OBJECT(filter)
  PRIVATE
    gr :      bigint64;
    spaltgr : spaltzeiger;
    we :      WORD;
    PROCEDURE neu(grenzfreq : bigint64);
    DESTRUCTOR alt; VIRTUAL;
    FUNCTION gefiltert(posi : bigint64) : sample; VIRTUAL;
    CONSTRUCTOR load(VAR s : tbufstream);
    PROCEDURE store(VAR s : tbufstream);
  END;
  tiefpasszg = ^tiefpass;

  tiefpass = OBJECT(passfilter)
  PUBLIC
    CONSTRUCTOR neu(grenzfreq : bigint64);
  PRIVATE
    PROCEDURE vorbereitung(frequenz : EXTENDED); VIRTUAL;
  END;
  hochpasszg = ^hochpass;

  hochpass = OBJECT(passfilter)
  PUBLIC
    CONSTRUCTOR neu(grenzfreq : bigint64);
  PRIVATE
    PROCEDURE vorbereitung(frequenz : EXTENDED); VIRTUAL;
    PROCEDURE einheitgenerieren(VAR beleg : belegung); VIRTUAL;
  END;

  { Differenzierungs- Filter }
  diffzg = ^diff;

  diff = OBJECT(filter)
  PUBLIC
    CONSTRUCTOR neu;
  PRIVATE
    PROCEDURE einheitgenerieren(VAR beleg : belegung); VIRTUAL;
    PROCEDURE vorbereitung(frequenz : EXTENDED); VIRTUAL;
    FUNCTION gefiltert(posi : bigint64) : sample; VIRTUAL;
  END;

  { Integrations- Filter }
  intzg = ^int;

  int = OBJECT(filter)
  PUBLIC
    CONSTRUCTOR neu;
  PRIVATE
    gefiltertwert : sample;
    posiwert :      bigint64;
    PROCEDURE einheitgenerieren(VAR beleg : belegung); VIRTUAL;
    PROCEDURE vorbereitung(frequenz : EXTENDED); VIRTUAL;
    FUNCTION gefiltert(posi : bigint64) : sample; VIRTUAL;
  END;

  { gleitender Integrations- Filter }
  glintzg = ^glint;

  glint = OBJECT(breitefilter)
  PUBLIC
    CONSTRUCTOR neu(breims : EXTENDED);
  PRIVATE
    gefiltertwert : sample;
    posiwert :      bigint64;
    PROCEDURE einheitgenerieren(VAR beleg : belegung); VIRTUAL;
    PROCEDURE vorbereitung(frequenz : EXTENDED); VIRTUAL;
    FUNCTION gefiltert(posi : bigint64) : sample; VIRTUAL;
  END;

  { gleitender Linienzug- Filter }
  gllinzg = ^gllin;

  gllin = OBJECT(breitefilter)
  PUBLIC
    CONSTRUCTOR neu(breims : EXTENDED);
  PRIVATE
    gefiltertwert : sample;
    posiwert :      bigint64;
    PROCEDURE einheitgenerieren(VAR beleg : belegung); VIRTUAL;
    PROCEDURE vorbereitung(frequenz : EXTENDED); VIRTUAL;
    FUNCTION gefiltert(posi : bigint64) : sample; VIRTUAL;
  END;

  { X-Achsen-Verschiebungs - Filter }
  verschiebezg = ^verschiebe;

  verschiebe = OBJECT(filter)
  PUBLIC
    CONSTRUCTOR neu(umms : EXTENDED);
  PRIVATE
    um :    EXTENDED;
    posid : bigint64;
    PROCEDURE vorbereitung(frequenz : EXTENDED); VIRTUAL;
    FUNCTION gefiltert(posi : bigint64) : sample; VIRTUAL;
    CONSTRUCTOR load(VAR s : tbufstream);
    PROCEDURE store(VAR s : tbufstream);
  END;

  { Maximum-Minimum-Differenz - Filter }
  maxminzg = ^maxmin;

  maxmin = OBJECT(filter)
  PUBLIC
    CONSTRUCTOR neu(breims : EXTENDED);
  PRIVATE
    brei : EXTENDED;
    br2 :  bigint64;
    PROCEDURE einheitgenerieren(VAR beleg : belegung); VIRTUAL;
    PROCEDURE vorbereitung(frequenz : EXTENDED); VIRTUAL;
    FUNCTION gefiltert(posi : bigint64) : sample; VIRTUAL;
    CONSTRUCTOR load(VAR s : tbufstream);
    PROCEDURE store(VAR s : tbufstream);
  END;

  { Digitalfilter }
  digitalzg = ^digital;

  digital = OBJECT(filter)
  PUBLIC
    CONSTRUCTOR neu(k : BYTE; schwelle : sample; neinheit : STRING; nwert : EXTENDED);
  PRIVATE
    schw :          sample;
    gefiltertwert : sample;
    posilinkswert, posirechtswert : bigint64;
    neueeinheit :   STRING;
    neuerwert :     EXTENDED;
    PROCEDURE einheitgenerieren(VAR beleg : belegung); VIRTUAL;
    PROCEDURE vorbereitung(frequenz : EXTENDED); VIRTUAL;
    FUNCTION gefiltert(posi : bigint64) : sample; VIRTUAL;
    CONSTRUCTOR load(VAR s : tbufstream);
    PROCEDURE store(VAR s : tbufstream);
  END;


  { Spike - Filter }
  spikefilterzg = ^spikefilter;

  spikefilter = OBJECT(filter)
  PUBLIC
    CONSTRUCTOR neu(k : BYTE; millisek, ablinks, abrechts : EXTENDED);
  PRIVATE
    anz :            midint32;
    abstli, abstre : sample;
    ms :             EXTENDED;
    abli, abre :     EXTENDED;
    PROCEDURE vorbereitung(frequenz : EXTENDED); VIRTUAL;
    FUNCTION gefiltert(posi : bigint64) : sample; VIRTUAL;
    CONSTRUCTOR load(VAR s : tbufstream);
    PROCEDURE store(VAR s : tbufstream);
  END;

VAR
  grund :          grundlistetyp;
  belegungsliste : ARRAY[0..maxchannelsandfilters - 1] OF belegung;
  schriftliste :   schriftlistentyp;

PROCEDURE neukan(kanaele : BYTE);
PROCEDURE beschriftungen(VAR ko : headerdata);

PROCEDURE kanalsetz(k, vonk : BYTE);
PROCEDURE filtersetz(hilf : filterzeiger; k : BYTE);
PROCEDURE filterloesch(k : BYTE);
FUNCTION filterdrin(k : BYTE) : BOOLEAN;
FUNCTION filterzeile(k : BYTE) : STRING;

PROCEDURE einheitensetzen(frequenz : EXTENDED);

PROCEDURE openfileheader(Name : string80; VAR ko : headerdata);

FUNCTION dat(posi : bigint64; k : BYTE) : sample;

FUNCTION extspannung(y : EXTENDED; kanal : BYTE) : EXTENDED;
FUNCTION spannung(y : EXTENDED; kanal : BYTE) : bigint64;

FUNCTION norm(sp : EXTENDED; kanal : BYTE) : EXTENDED;

PROCEDURE streamput(VAR s : tbufstream);
PROCEDURE streamget(VAR s : tbufstream);

IMPLEMENTATION

TYPE  { Ende der Filterkette }
  endezg = ^ende;

  ende = OBJECT(filter)
    ka : BYTE;
    CONSTRUCTOR neu(k : BYTE);
    PROCEDURE einheitgenerieren(VAR beleg : belegung); VIRTUAL;
    FUNCTION gefiltert(posi : bigint64) : sample; VIRTUAL;
    CONSTRUCTOR load(VAR s : tbufstream);
    PROCEDURE store(VAR s : tbufstream);
  END;
  weiterzg = ^weiter;

  weiter = OBJECT(ende)
    PROCEDURE einheitgenerieren(VAR beleg : belegung); VIRTUAL;
    FUNCTION gefiltert(posi : bigint64) : sample; VIRTUAL;
  END;

CONST
  defaulteinheit : grundtyp =
    (faktor : 10 / maxsample; vor : ''; anfang : 'V'; sekunde : 0; gain : 1; multi : 1);
  vz : ARRAY[BOOLEAN] OF STRING[1] = ('', '+');

  rende : tstreamrec = (objtype : 300; vmtlink : ofs(typeof(ende)^);
    load : @ende.load; store : @ende.store);
  rweiter : tstreamrec = (objtype : 399; vmtlink : ofs(typeof(weiter)^);
    load : @ende.load; store : @ende.store);
  rinvert : tstreamrec = (objtype : 301; vmtlink : ofs(typeof(invert)^);
    load : @filter.load; store : @filter.store);
  roffset : tstreamrec = (objtype : 302; vmtlink : ofs(typeof(offset)^);
    load : @offset.load; store : @offset.store);
  rmalfaktor : tstreamrec = (objtype : 303; vmtlink : ofs(typeof(malfaktor)^);
    load : @malfaktor.load; store : @malfaktor.store);
  rkappen : tstreamrec = (objtype : 304; vmtlink : ofs(typeof(kappen)^);
    load : @filter.load; store : @filter.store);
  rabsolut : tstreamrec = (objtype : 305; vmtlink : ofs(typeof(absolut)^);
    load : @filter.load; store : @filter.store);
  rsquare : tstreamrec = (objtype : 306; vmtlink : ofs(typeof(square)^);
    load : @filter.load; store : @filter.store);
  rglatt : tstreamrec = (objtype : 307; vmtlink : ofs(typeof(glatt)^);
    load : @breitefilter.load; store : @breitefilter.store);
  rtiefpass : tstreamrec = (objtype : 308; vmtlink : ofs(typeof(tiefpass)^);
    load : @passfilter.load; store : @passfilter.store);
  rhochpass : tstreamrec = (objtype : 309; vmtlink : ofs(typeof(hochpass)^);
    load : @passfilter.load; store : @passfilter.store);
  rdiff : tstreamrec = (objtype : 310; vmtlink : ofs(typeof(diff)^);
    load : @filter.load; store : @filter.store);
  rverschiebe : tstreamrec = (objtype : 311; vmtlink : ofs(typeof(verschiebe)^);
    load : @verschiebe.load; store : @verschiebe.store);
  rmaxmin : tstreamrec = (objtype : 312; vmtlink : ofs(typeof(maxmin)^);
    load : @maxmin.load; store : @maxmin.store);
  rspikefilter : tstreamrec = (objtype : 313; vmtlink : ofs(typeof(spikefilter)^);
    load : @spikefilter.load; store : @spikefilter.store);
  rstreckung : tstreamrec = (objtype : 314; vmtlink : ofs(typeof(streckung)^);
    load : @streckung.load; store : @streckung.store);
  reinsdurch : tstreamrec = (objtype : 315; vmtlink : ofs(typeof(einsdurch)^);
    load : @filter.load; store : @filter.store);
  raddition : tstreamrec = (objtype : 316; vmtlink : ofs(typeof(addition)^);
    load : @addition.load; store : @addition.store);
  rint : tstreamrec = (objtype : 317; vmtlink : ofs(typeof(int)^);
    load : @filter.load; store : @filter.store);
  rglint : tstreamrec = (objtype : 318; vmtlink : ofs(typeof(glint)^);
    load : @breitefilter.load; store : @breitefilter.store);
  rgllin : tstreamrec = (objtype : 319; vmtlink : ofs(typeof(gllin)^);
    load : @breitefilter.load; store : @breitefilter.store);
  rarcsin : tstreamrec = (objtype : 320; vmtlink : ofs(typeof(arcsin)^);
    load : @filter.load; store : @filter.store);
  rarccos : tstreamrec = (objtype : 321; vmtlink : ofs(typeof(arccos)^);
    load : @filter.load; store : @filter.store);
  rkorrelation : tstreamrec = (objtype : 322; vmtlink : ofs(typeof(korrelation)^);
    load : @korrelation.load; store : @korrelation.store);
      {rdigital    :tstreamrec=(objtype:323;           vmtlink:ofs(typeof(digital)^);
                               load:@digital.load;    store:@digital.store);}
  rwinkel : tstreamrec = (objtype : 324; vmtlink : ofs(typeof(winkel)^);
    load : @winkel.load; store : @winkel.store);
  rbetrag : tstreamrec = (objtype : 325; vmtlink : ofs(typeof(betrag)^);
    load : @betrag.load; store : @betrag.store);
  rdigital : tstreamrec = (objtype : 326; vmtlink : ofs(typeof(digital)^);
    load : @digital.load; store : @digital.store);



VAR
  filteranfang : ARRAY[1..maxchannelsandfilters - 1] OF filterzeiger;
  kan :          BYTE;
  fre :          EXTENDED;
  diffaktor :    EXTENDED;
  i :            WORD;

PROCEDURE einheittyp.kopie(VAR ein : einheittyp);
BEGIN
  self := ein;
END;

FUNCTION einheittyp.einhwort : einheitstring;
CONST
  liste : ARRAY [-2..2] OF STRING[3] = ('/s'#253, '/s', '', 's', 's'#253);
BEGIN
  IF (anfang = '1') AND (sekunde = -1) THEN einhwort := vor + 'Hz'
  ELSE
  IF (vor = '') AND (anfang = '1') AND (sekunde = 0) THEN einhwort := ''
  ELSE
    CASE sekunde OF
      -2..0 : einhwort := vor + anfang + liste[sekunde];
      1, 2 : IF anfang = '1' THEN einhwort := vor + liste[sekunde]
        ELSE
          einhwort := vor + anfang + liste[sekunde];
      ELSE
        IF sekunde < -2 THEN einhwort := vor + anfang + '/s^' + bequem.wort(-sekunde)
        ELSE
          einhwort := vor + anfang + 's^' + bequem.wort(sekunde);
    END;
END;

FUNCTION einheittyp.plot : string80;
VAR
  kom : string80;
  p :   bigint64;
BEGIN
  IF (vor = '') AND (anfang = '1') AND (sekunde = 0) THEN kom := ''
  ELSE
  BEGIN
    IF vor = 'æ' THEN kom := plmu
    ELSE
      kom := 'LB' + vor + #3;
    IF (anfang = '1') AND (sekunde = -1) THEN kom := kom + 'LBHz'#3
    ELSE
    BEGIN

      IF (sekunde <= 0) OR (anfang <> '1') THEN kom := kom + 'LB' + anfang;
      p := pos(#253, kom);
      WHILE p > 0 DO
      BEGIN
        Delete(kom, p, 1);
        insert(#3'CP0,0.3;LB2'#3'CP0,-0.3;LB', kom, p);
        p := pos(#253, kom);
      END;
      IF sekunde = 0 THEN kom := kom + #3
      ELSE
      BEGIN
        IF sekunde < 0 THEN kom := kom + '/';
        kom := kom + 's'#3;
        IF abs(sekunde) > 1 THEN kom := kom + 'CP0,0.3;LB' + bequem.wort(abs(sekunde)) + #3'CP0,-0.3;';
      END;
    END;
  END;
  plot := kom;
END;

PROCEDURE einheittyp.handlich;
TYPE
  string1 = STRING[1];
VAR
  n : INTEGER;
  x : EXTENDED;

  PROCEDURE setz(mal : EXTENDED; davor : string1);
  BEGIN
    faktor := faktor * mal;
    vor    := davor;
  END;

BEGIN
  x := faktor * maxsample * 1.00001;
  IF x <= 1E-9 THEN x := 1E-9;
  IF (sekunde = 0) AND (anfang = '') THEN schwierig
  ELSE
  BEGIN
    n := round(log(x) - 0.5);
    CASE n - 1 OF
      -11, -10, -9 : setz(1E12, 'p');
      -8, -7, -6 : setz(1E9, 'n');
      -5, -4, -3 : setz(1E6, 'u');
      -2, -1, 0 : setz(1E3, 'm');
      1, 2, 3 : setz(1E0, '');
      4, 5, 6 : setz(1E-3, 'k');
      7, 8, 9 : setz(1E-6, 'M');
      10, 11, 12 : setz(1E-9, 'G');
      ELSE
        schwierig;
    END;
    IF length(einhwort) > 5 THEN schwierig;
  END;
END;

PROCEDURE einheittyp.schwierig;
VAR
  n : bigint64;
  x : EXTENDED;
BEGIN
  x := faktor * maxsample * 1.00001;
  IF x <= 1E-9 THEN x := 1E-9;
  n := pred(trunc(log(x)));
  IF n < 0 THEN Dec(n, 2);
  n := 3 * (n DIV 3);
  IF n > 0 THEN faktor := faktor / xpot(n)
  ELSE
    faktor             := faktor * xpot(-n);
  anfang := '1E' + wort(n);
  sekunde := 0;
END;

PROCEDURE grundtyp.setz(m : EXTENDED; anf : einheitstring);
VAR
  len : BYTE absolute anf;
  pos : BYTE;
BEGIN
  kompri(anf);
  pos := len;
  IF len >= 1 THEN
  BEGIN
    sekunde := 1;
    CASE anf[len] OF
      '0'..'9' : IF len >= 3 THEN IF anf[pred(len)] = '^' THEN
          BEGIN
            sekunde := zahl(anf[len]);
            Dec(pos, 2);
          END;
      #253 : IF len >= 2 THEN
        BEGIN
          sekunde := 2;
          Dec(pos);
        END;
    END;
    IF anf[pos] = 's' THEN
    BEGIN
      len := pred(pos);
      IF len >= 1 THEN CASE anf[len] OF
          '/' : BEGIN
            Dec(len);
            sekunde := -sekunde;
          END;
          '*' : Dec(len);
        END;
    END
    ELSE
      sekunde := 0;
  END
  ELSE
    sekunde := 0;
  anfang    := copy(anf, 1, 5);
  multi     := m;
  faktor    := multi * gain;
END;

PROCEDURE belegung.grundsetzen(k : BYTE);
BEGIN
  kopie(grund[k]);
  negativ     := True;
  gepunktet   := False;
  schwierigda := False;
END;

{ filter }

CONSTRUCTOR filter.load;
BEGIN
  s.Read(Name, sizeof(Name));
  Next := filterzeiger(s.get);
END;

PROCEDURE filter.store;
BEGIN
  s.Write(Name, sizeof(Name));
  s.put(Next);
END;

DESTRUCTOR filter.alt;
BEGIN
END;

PROCEDURE filter.einheitgenerieren(VAR beleg : belegung);
BEGIN
  Next^.einheitgenerieren(beleg);
END;

PROCEDURE filter.vorbereitung;
BEGIN
END;

FUNCTION filter.gefiltert(posi : bigint64) : sample;
BEGIN
  gefiltert := Next^.gefiltert(posi);
END;

{ ende }

CONSTRUCTOR ende.neu(k : BYTE);
BEGIN
  ka   := k;
  Next := nil;
  Name := '#' + wort(k);
END;

CONSTRUCTOR ende.load(VAR s : tbufstream);
BEGIN
  filter.load(s);
  s.Read(ka, sizeof(BYTE));
END;

PROCEDURE ende.store(VAR s : tbufstream);
BEGIN
  filter.store(s);
  s.Write(ka, sizeof(BYTE));
END;

PROCEDURE ende.einheitgenerieren(VAR beleg : belegung);
BEGIN
  beleg.grundsetzen(ka);
END;

FUNCTION ende.gefiltert(posi : bigint64) : sample;
BEGIN
  gefiltert := lesef(posi, ka);
END;

{ weiter }

PROCEDURE weiter.einheitgenerieren(VAR beleg : belegung);
BEGIN
  filteranfang[ka]^.einheitgenerieren(beleg);
END;

FUNCTION weiter.gefiltert(posi : bigint64) : sample;
BEGIN
  gefiltert := filteranfang[ka]^.gefiltert(posi);
END;

{ breitefilter }

PROCEDURE breitefilter.neu(breims : EXTENDED);
BEGIN
  brei := breims;
END;

CONSTRUCTOR breitefilter.load(VAR s : tbufstream);
BEGIN
  filter.load(s);
  s.Read(brei, sizeof(EXTENDED));
END;

PROCEDURE breitefilter.store(VAR s : tbufstream);
BEGIN
  filter.store(s);
  s.Write(brei, sizeof(EXTENDED));
END;

PROCEDURE breitefilter.vorbereitung(frequenz : EXTENDED);
BEGIN
  breianz2 := round(brei * frequenz / 2000);
  breianz  := 2 * breianz2 + 1;
END;

{ arcsin - Filter }
CONSTRUCTOR arcsin.neu;
BEGIN
  Name := 'arcsin';
END;

PROCEDURE arcsin.einheitgenerieren(VAR beleg : belegung);
BEGIN
  filter.einheitgenerieren(beleg);
  WITH beleg DO
  BEGIN
    faktor  := pi / 2 / maxsample;
    negativ := True;
    anfang  := 'rad';
    sekunde := 0;
  END;
END;

FUNCTION arcsin.gefiltert(posi : bigint64) : sample;
CONST
  fak = maxsample / pi * 2;
VAR
  x, x1 : EXTENDED;
BEGIN
  x  := Next^.gefiltert(posi) / maxsample;
  x1 := 1 - x * x;
  IF x1 > 1e-10 THEN gefiltert := round(arctan(x / sqrt(x1)) * fak)
  ELSE IF x > 0 THEN gefiltert := maxsample
  ELSE
    gefiltert := -maxsample;
END;

{ arccos - Filter }
CONSTRUCTOR arccos.neu;
BEGIN
  Name := 'arccos';
END;

PROCEDURE arccos.einheitgenerieren(VAR beleg : belegung);
BEGIN
  filter.einheitgenerieren(beleg);
  WITH beleg DO
  BEGIN
    faktor  := pi / maxsample;
    negativ := False;
    anfang  := 'rad';
    sekunde := 0;
  END;
END;

FUNCTION arccos.gefiltert(posi : bigint64) : sample;
CONST
  fak = maxsample / pi;
  pi2 = pi / 2;
VAR
  x, x1 : EXTENDED;
BEGIN
  x  := Next^.gefiltert(posi) / maxsample;
  x1 := 1 - x * x;
  IF x1 > 1e-10 THEN gefiltert := round((pi2 - arctan(x / sqrt(x1))) * fak)
  ELSE IF x > 0 THEN gefiltert := 0
  ELSE
    gefiltert := maxsample;
END;

{ invert }

CONSTRUCTOR invert.neu;
BEGIN
  Name := '*(-1)';
END;

PROCEDURE invert.einheitgenerieren(VAR beleg : belegung);
BEGIN
  filter.einheitgenerieren(beleg);
  beleg.negativ := True;
END;

FUNCTION invert.gefiltert(posi : bigint64) : sample;
BEGIN
  gefiltert := -Next^.gefiltert(posi);
END;

{ einsdurch }

CONST
  einsdurchfak = 1e6;

CONSTRUCTOR einsdurch.neu;
BEGIN
  Name := '1/X';
END;

PROCEDURE einsdurch.einheitgenerieren(VAR beleg : belegung);
BEGIN
  filter.einheitgenerieren(beleg);
  WITH beleg DO
  BEGIN
    faktor := 1 / faktor / maxsample / einsdurchfak;
    CASE length(anfang) OF
      0 : ;
      1 : IF anfang <> '1' THEN anfang := '1/' + anfang;
      2..3 : anfang := '1/(' + anfang + ')';
      ELSE
        schwierigda := True;
    END;
    sekunde := -sekunde;
  END;
END;

FUNCTION einsdurch.gefiltert(posi : bigint64) : sample;
VAR
  wert : sample;
BEGIN
  wert := Next^.gefiltert(posi);
  IF wert <= einsdurchfak THEN gefiltert := maxsample
  ELSE
    gefiltert := round(maxsample / wert * einsdurchfak);
END;

{ offset }

CONSTRUCTOR offset.neu(k : BYTE; hoch : sample);
BEGIN
  ho   := hoch;
  Name := 'Offset ' + vz[ho >= 0] + extwort(ho * belegungsliste[k].faktor, 3, 2) + belegungsliste[k].einhwort;
END;

CONSTRUCTOR offset.load(VAR s : tbufstream);
BEGIN
  filter.load(s);
  s.Read(ho, sizeof(sample));
END;

PROCEDURE offset.store(VAR s : tbufstream);
BEGIN
  filter.store(s);
  s.Write(ho, sizeof(sample));
END;

FUNCTION offset.gefiltert(posi : bigint64) : sample;
BEGIN
  gefiltert := Next^.gefiltert(posi) + ho;
END;

{ malfaktor }

CONSTRUCTOR malfaktor.neu(malfak : EXTENDED);
BEGIN
  float := malfak;
  Name  := '*' + extfwort(malfak, 3);
END;

CONSTRUCTOR malfaktor.load(VAR s : tbufstream);
BEGIN
  filter.load(s);
  s.Read(float, sizeof(EXTENDED));
END;

PROCEDURE malfaktor.store(VAR s : tbufstream);
BEGIN
  filter.store(s);
  s.Write(float, sizeof(EXTENDED));
END;

FUNCTION malfaktor.gefiltert(posi : bigint64) : sample;
BEGIN
  gefiltert := round(Next^.gefiltert(posi) * float);
END;

{ streckung }

CONSTRUCTOR streckung.neu(k : BYTE; maxspannung : EXTENDED);
BEGIN
  float := belegungsliste[k].faktor * maxsample / abs(maxspannung);
  Name  := '**' + extfwort(float, 3);
END;

CONSTRUCTOR streckung.load(VAR s : tbufstream);
BEGIN
  filter.load(s);
  s.Read(float, sizeof(EXTENDED));
END;

PROCEDURE streckung.store(VAR s : tbufstream);
BEGIN
  filter.store(s);
  s.Write(float, sizeof(EXTENDED));
END;

PROCEDURE streckung.einheitgenerieren(VAR beleg : belegung);
BEGIN
  filter.einheitgenerieren(beleg);
  beleg.faktor := beleg.faktor / float;
END;

FUNCTION streckung.gefiltert(posi : bigint64) : sample;
BEGIN
  gefiltert := round(Next^.gefiltert(posi) * float);
END;

{ kappen }

CONSTRUCTOR kappen.neu;
BEGIN
  Name := 'Clip';
END;

FUNCTION kappen.gefiltert(posi : bigint64) : sample;
VAR
  wert : sample;
BEGIN
  wert := Next^.gefiltert(posi);
  IF wert > maxsample THEN wert := maxsample
  ELSE
  IF wert < minsample THEN wert := minsample;
  gefiltert := wert;
END;

{ addition }

CONSTRUCTOR addition.neu(additionskanal : BYTE);
BEGIN
  adk  := additionskanal;
  Name := '+ Chan. ' + wort(adk);
END;

CONSTRUCTOR addition.load(VAR s : tbufstream);
BEGIN
  filter.load(s);
  s.Read(adk, 1);
END;

PROCEDURE addition.store(VAR s : tbufstream);
BEGIN
  filter.store(s);
  s.Write(adk, 1);
END;

PROCEDURE addition.einheitgenerieren(VAR beleg : belegung);
BEGIN
  filter.einheitgenerieren(beleg);
  beleg.faktor := beleg.faktor * 2;
END;

FUNCTION addition.gefiltert(posi : bigint64) : sample;
BEGIN
  gefiltert := (Next^.gefiltert(posi) + dat(posi, adk)) DIV 2;
END;

{ x-y-Betrag }

CONSTRUCTOR betrag.neu(kanal2 : BYTE);
BEGIN
  k2   := kanal2;
  Name := '³(x Chan.' + wort(k2) + ')³';
END;

CONSTRUCTOR betrag.load(VAR s : tbufstream);
BEGIN
  filter.load(s);
  s.Read(k2, 1);
END;

PROCEDURE betrag.store(VAR s : tbufstream);
BEGIN
  filter.store(s);
  s.Write(k2, 1);
END;

PROCEDURE betrag.einheitgenerieren(VAR beleg : belegung);
BEGIN
  filter.einheitgenerieren(beleg);
  beleg.negativ := False;
END;

FUNCTION betrag.gefiltert(posi : bigint64) : sample;
VAR
  ys, zs : EXTENDED;
BEGIN
  ys        := Next^.gefiltert(posi);
  zs        := dat(posi, k2);
  gefiltert := round(sqrt(ys * ys + zs * zs));
END;

{ Winkel }

CONSTRUCTOR winkel.neu(xkanal : BYTE);
BEGIN
  xk   := xkanal;
  Name := '(y) Angle (x=#' + wort(xk) + ')';
END;

CONSTRUCTOR winkel.load(VAR s : tbufstream);
BEGIN
  filter.load(s);
  s.Read(xk, 1);
END;

PROCEDURE winkel.store(VAR s : tbufstream);
BEGIN
  filter.store(s);
  s.Write(xk, 1);
END;

PROCEDURE winkel.einheitgenerieren(VAR beleg : belegung);
BEGIN
  filter.einheitgenerieren(beleg);
  WITH beleg DO
  BEGIN
    faktor  := pi / maxsample;
    negativ := True;
    anfang  := 'rad';
    sekunde := 0;
  END;
END;

FUNCTION winkel.gefiltert(posi : bigint64) : sample;
CONST
  fak  = maxsample / pi;
  halb = round(fak * pi);
VAR
  x, y :   sample;
  winkel : sample;
BEGIN
  x := dat(posi, xk);
  y := Next^.gefiltert(posi);
  IF (x = 0) AND (y = 0) THEN winkel := minsample
  ELSE IF abs(x) > abs(y) THEN
    IF (y >= 0) = (x >= 0) THEN winkel := round(arctan(y / x) * fak)
    ELSE
      winkel := round((arctan(y / x) + pi) * fak)
  ELSE
    winkel := round((pi / 2 - arctan(x / y)) * fak);
  IF y < 0 THEN gefiltert := winkel - halb
  ELSE
    gefiltert             := winkel;
END;

{ absolut }

CONSTRUCTOR absolut.neu;
BEGIN
  Name := '³x³';
END;

PROCEDURE absolut.einheitgenerieren(VAR beleg : belegung);
BEGIN
  filter.einheitgenerieren(beleg);
  beleg.negativ := False;
END;

FUNCTION absolut.gefiltert(posi : bigint64) : sample;
BEGIN
  gefiltert := abs(Next^.gefiltert(posi));
END;

{ square }

CONSTRUCTOR square.neu;
BEGIN
  Name := 'xý';
END;

PROCEDURE square.einheitgenerieren(VAR beleg : belegung);
BEGIN
  filter.einheitgenerieren(beleg);
  WITH beleg DO
  BEGIN
    negativ     := False;
    faktor      := sqr(faktor) * maxsample;
    schwierigda := True;
  END;
END;

FUNCTION square.gefiltert(posi : bigint64) : sample;
VAR
  xs : EXTENDED;
BEGIN
  xs        := Next^.gefiltert(posi);
  gefiltert := round(xs * xs / maxsample);
END;

{ glatt }

CONSTRUCTOR glatt.neu(breims : EXTENDED);
BEGIN
  breitefilter.neu(breims);
  Name := 'Gl.Avg. '#29 + extwort(brei, 3, 1) + 'ms';
END;

FUNCTION glatt.gefiltert(posi : bigint64) : sample;
VAR
  i :   bigint64;
  sum : EXTENDED;
BEGIN
  sum := 0;
  i   := posi - breianz2;
  WHILE i <= posi + breianz2 DO
  BEGIN
    sum := sum + Next^.gefiltert(i);
    Inc(i);
  END;
  gefiltert := round(sum / breianz);
END;

{ Korrelation }

CONSTRUCTOR korrelation.neu(korrkanal : BYTE; breims : EXTENDED);
BEGIN
  INHERITED neu(breims);
  kk   := korrkanal;
  Name := 'Corr. to ' + wort(kk) + ' (' + extwort(brei, 3, 1) + 'ms)';
END;

CONSTRUCTOR korrelation.load(VAR s : tbufstream);
BEGIN
  INHERITED load(s);
  s.Read(kk, 1);
END;

PROCEDURE korrelation.store(VAR s : tbufstream);
BEGIN
  INHERITED store(s);
  s.Write(kk, 1);
END;

PROCEDURE korrelation.einheitgenerieren(VAR beleg : belegung);
BEGIN
  INHERITED einheitgenerieren(beleg);
  WITH beleg DO
  BEGIN
    faktor  := 1 / maxsample;
    anfang  := '1';
    sekunde := 0;
  END;
END;

FUNCTION korrelation.gefiltert(posi : bigint64) : sample;
VAR
  i : bigint64;
  x, y, sx, sy, sxq, syq, sxy : EXTENDED;
BEGIN
  sx  := 0;
  sy  := 0;
  sxq := 0;
  syq := 0;
  sxy := 0;
  i   := -breianz2;
  WHILE i <= +breianz2 DO
  BEGIN
    x   := Next^.gefiltert(posi + i);
    y   := dat(posi + i, kk);
    sx  := sx + x;
    sy  := sy + y;
    sxq := sxq + x * x;
    syq := syq + y * y;
    sxy := sxy + x * y;
    Inc(i);
  END;
  IF (sxq < 1e-40) AND (syq < 1e-40) THEN gefiltert := maxsample
  ELSE
    gefiltert := round((breianz * sxy - sx * sy) / sqrt((breianz * sxq - sx * sx) * (breianz * syq - sy * sy)) * maxsample);
END;

{ passfilter }

PROCEDURE passfilter.neu(grenzfreq : bigint64);
BEGIN
  gr   := grenzfreq;
  Name := wort(gr) + 'Hz';
  new(spaltgr);
END;

CONSTRUCTOR passfilter.load(VAR s : tbufstream);
BEGIN
  filter.load(s);
  s.Read(gr, sizeof(gr));
  new(spaltgr);
END;

PROCEDURE passfilter.store(VAR s : tbufstream);
BEGIN
  filter.store(s);
  s.Write(gr, sizeof(gr));
END;

FUNCTION passfilter.gefiltert(posi : bigint64) : sample;
VAR
  i :   -weite..weite;
  sum : EXTENDED;
BEGIN
  sum := 0;
  FOR i := we DOWNTO -we DO sum := sum + Next^.gefiltert(posi - i) * spaltgr^[abs(i)];
  gefiltert := round(sum);
END;

DESTRUCTOR passfilter.alt;
BEGIN
  dispose(spaltgr);
END;

CONSTRUCTOR tiefpass.neu(grenzfreq : bigint64);
BEGIN
  passfilter.neu(grenzfreq);
  Name := 'LP ' + Name;
END;

PROCEDURE tiefpass.vorbereitung(frequenz : EXTENDED);
VAR
  fak : EXTENDED;
  j :   INTEGER;
BEGIN
  fak         := gr / frequenz;
  we          := min(round(genau * pi / fak), weite);
  spaltgr^[0] := fak / pi;
  FOR j := 1 TO we DO spaltgr^[j] := sin(j * fak) / j / pi;
END;

CONSTRUCTOR hochpass.neu(grenzfreq : bigint64);
BEGIN
  passfilter.neu(grenzfreq);
  Name := 'HP ' + Name;
END;

PROCEDURE hochpass.vorbereitung(frequenz : EXTENDED);
VAR
  fak : EXTENDED;
  j :   INTEGER;
BEGIN
  fak         := gr / frequenz;
  we          := min(round(genau * pi / fak), weite);
  spaltgr^[0] := 1 - fak / pi;
  FOR j := 1 TO we DO spaltgr^[j] := -sin(j * fak) / j / pi;
END;

PROCEDURE hochpass.einheitgenerieren(VAR beleg : belegung);
BEGIN
  filter.einheitgenerieren(beleg);
  beleg.negativ := True;
END;

{ Diff-Filter }

CONSTRUCTOR diff.neu;
BEGIN
  Name := 'd/dt';
END;

PROCEDURE diff.einheitgenerieren(VAR beleg : belegung);
BEGIN
  filter.einheitgenerieren(beleg);
  WITH beleg DO
  BEGIN
    faktor := faktor * fre;
    Dec(sekunde);
  END;
  beleg.negativ := True;
END;

PROCEDURE diff.vorbereitung(frequenz : EXTENDED);
BEGIN
END;

FUNCTION diff.gefiltert(posi : bigint64) : sample;
BEGIN
  gefiltert := round((Next^.gefiltert(posi + 1) - Next^.gefiltert(posi - 1)) * diffaktor);
END;

{ Integrationsfilter }

CONSTRUCTOR int.neu;
BEGIN
  Name := '* dt';
END;

PROCEDURE int.einheitgenerieren(VAR beleg : belegung);
BEGIN
  filter.einheitgenerieren(beleg);
  WITH beleg DO
  BEGIN
    faktor := faktor / fre;
    Inc(sekunde);
  END;
END;

PROCEDURE int.vorbereitung(frequenz : EXTENDED);
BEGIN
  posiwert      := 0;
  gefiltertwert := 0;
END;

FUNCTION int.gefiltert(posi : bigint64) : sample;
VAR
  i : bigint64;
BEGIN
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

{ gleitender Integrationsfilter }

CONSTRUCTOR glint.neu(breims : EXTENDED);
BEGIN
  breitefilter.neu(breims);
  Name := '* dt '#29 + extwort(brei, 3, 1) + 'ms';
END;

PROCEDURE glint.einheitgenerieren(VAR beleg : belegung);
BEGIN
  filter.einheitgenerieren(beleg);
  WITH beleg DO
  BEGIN
    faktor := faktor / fre;
    Inc(sekunde);
  END;
END;

PROCEDURE glint.vorbereitung(frequenz : EXTENDED);
BEGIN
  breitefilter.vorbereitung(frequenz);
  posiwert      := -breianz2 - 2;
  gefiltertwert := 0;
END;

FUNCTION glint.gefiltert(posi : bigint64) : sample;
VAR
  i : bigint64;
BEGIN
  IF posi < posiwert THEN
  BEGIN
    i := posi + 1;
    WHILE i <= posiwert DO
    BEGIN
      Dec(gefiltertwert, Next^.gefiltert(i + breianz2) - Next^.gefiltert(i - breianz2));
      Inc(i);
    END;
  END
  ELSE
  BEGIN
    i := posiwert + 1;
    WHILE i <= posi DO
    BEGIN
      Inc(gefiltertwert, Next^.gefiltert(i + breianz2) - Next^.gefiltert(i - breianz2));
      Inc(i);
    END;
  END;
  posiwert  := posi;
  gefiltert := gefiltertwert;
END;

{ gleitender Linienzugfilter }

CONSTRUCTOR gllin.neu(breims : EXTENDED);
BEGIN
  breitefilter.neu(breims);
  Name := 'ä³dy³ '#29 + extwort(brei, 3, 1) + 'ms';
END;

PROCEDURE gllin.einheitgenerieren(VAR beleg : belegung);
BEGIN
  INHERITED einheitgenerieren(beleg);
  WITH beleg DO
  BEGIN
    negativ := False;
    faktor  := faktor;
    Dec(sekunde);
  END;
END;

PROCEDURE gllin.vorbereitung(frequenz : EXTENDED);
BEGIN
  breitefilter.vorbereitung(frequenz);
  posiwert      := -breianz2 - 2;
  gefiltertwert := 0;
END;

FUNCTION gllin.gefiltert(posi : bigint64) : sample;
VAR
  i : bigint64;
BEGIN
  IF posi < posiwert THEN
  BEGIN
    i := posi + 1;
    WHILE i <= posiwert DO
    BEGIN
      Dec(gefiltertwert, abs(Next^.gefiltert(i + breianz2) -
        Next^.gefiltert(i + breianz2 - 1)) - abs(
        Next^.gefiltert(i - breianz2) - Next^.gefiltert(i - breianz2 - 1)));
      Inc(i);
    END;
  END
  ELSE
  BEGIN
    i := posiwert + 1;
    WHILE i <= posi DO
    BEGIN
      Inc(gefiltertwert, abs(Next^.gefiltert(i + breianz2) -
        Next^.gefiltert(i + breianz2 - 1)) - abs(
        Next^.gefiltert(i - breianz2) - Next^.gefiltert(i - breianz2 - 1)));
      Inc(i);
    END;
  END;
  posiwert  := posi;
  gefiltert := gefiltertwert;
END;

{ Verschiebefilter }

CONSTRUCTOR verschiebe.neu(umms : EXTENDED);
BEGIN
  um   := umms;
  Name := vz[um >= 0] + extwort(um, 2, 1) + 'ms';
END;

CONSTRUCTOR verschiebe.load(VAR s : tbufstream);
BEGIN
  filter.load(s);
  s.Read(um, sizeof(EXTENDED));
END;

PROCEDURE verschiebe.store(VAR s : tbufstream);
BEGIN
  filter.store(s);
  s.Write(um, sizeof(EXTENDED));
END;

PROCEDURE verschiebe.vorbereitung(frequenz : EXTENDED);
BEGIN
  posid := round(um * frequenz / 1000);
END;

FUNCTION verschiebe.gefiltert(posi : bigint64) : sample;
BEGIN
  gefiltert := Next^.gefiltert(posi + posid);
END;

CONSTRUCTOR maxmin.neu(breims : EXTENDED);
BEGIN
  brei := breims;
  Name := 'Max-Min '#29 + extwort(brei, 2, 1) + 'ms';
END;

CONSTRUCTOR maxmin.load(VAR s : tbufstream);
BEGIN
  filter.load(s);
  s.Read(brei, sizeof(EXTENDED));
END;

PROCEDURE maxmin.store(VAR s : tbufstream);
BEGIN
  filter.store(s);
  s.Write(brei, sizeof(EXTENDED));
END;

PROCEDURE maxmin.einheitgenerieren(VAR beleg : belegung);
BEGIN
  filter.einheitgenerieren(beleg);
  beleg.faktor  := beleg.faktor * 2;
  beleg.negativ := False;
END;

PROCEDURE maxmin.vorbereitung(frequenz : EXTENDED);
BEGIN
  br2 := round(brei * frequenz / 2000);
END;

FUNCTION maxmin.gefiltert(posi : bigint64) : sample;
VAR
  i : bigint64;
  wert, wertmin, wertmax : INTEGER;//minsample-1..maxsample+1;
BEGIN
  wertmax := minsample - 1;
  wertmin := maxsample + 1;
  i       := posi - br2;
  WHILE i <= posi + br2 DO
  BEGIN
    wert := Next^.gefiltert(i);
    IF wert > wertmax THEN wertmax := wert;
    IF wert < wertmin THEN wertmin := wert;
    Inc(i);
  END;
  gefiltert := (wertmax - wertmin) DIV 2;
END;

{ Digitalfilter }

CONSTRUCTOR digital.neu(k : BYTE; schwelle : sample; neinheit : STRING; nwert : EXTENDED);
BEGIN
  schw        := schwelle;
  neueeinheit := neinheit;
  neuerwert   := nwert;
  Name        := 'Pulse counter ' + extwort(schw * belegungsliste[k].faktor, 3, 2) + belegungsliste[k].einhwort +
    ' (' + extwort(neuerwert, 5, 3) + ' ' + neueeinheit + ')';
END;

PROCEDURE digital.einheitgenerieren(VAR beleg : belegung);
BEGIN
  INHERITED einheitgenerieren(beleg);
  WITH beleg DO
  BEGIN
    faktor  := neuerwert * fre / maxsample / 2;
    anfang  := neueeinheit;
    sekunde := -1;
    negativ := True;
  END;
END;

PROCEDURE digital.vorbereitung(frequenz : EXTENDED);
BEGIN
  posilinkswert  := 0;
  posirechtswert := 0;
  gefiltertwert  := 0;
END;

CONSTRUCTOR digital.load(VAR s : tbufstream);
BEGIN
  filter.load(s);
  s.Read(schw, sizeof(sample));
  s.Read(neueeinheit, sizeof(neueeinheit));
  s.Read(neuerwert, sizeof(neuerwert));
END;

PROCEDURE digital.store(VAR s : tbufstream);
BEGIN
  filter.store(s);
  s.Write(schw, sizeof(sample));
  s.Write(neueeinheit, sizeof(neueeinheit));
  s.Write(neuerwert, sizeof(neuerwert));
END;

FUNCTION digital.gefiltert(posi : bigint64) : sample;
VAR
  li, re, minli, maxre : bigint64;
BEGIN
  IF (posi >= posilinkswert) AND (posi < posirechtswert) THEN
    gefiltert := gefiltertwert
  ELSE
  BEGIN
    li    := posi;
    re    := posi;
    minli := posi - 100000;
    maxre := posi + 100000;
    WHILE (abs(Next^.gefiltert(li)) < schw) AND (li >= minli) DO Dec(li);
    WHILE abs(Next^.gefiltert(li)) > schw DO Dec(li);
    Inc(li);
    WHILE abs(Next^.gefiltert(re)) > schw DO Inc(re);
    WHILE (abs(Next^.gefiltert(re)) < schw) AND (re <= maxre) DO Inc(re);
    IF (li <= minli) OR (re > maxre) THEN
    BEGIN
      gefiltert      := 0;
      gefiltertwert  := 0;
      posilinkswert  := li;
      posirechtswert := re;
    END
    ELSE
    BEGIN
      IF (Next^.gefiltert(li) > 0) AND (Next^.gefiltert(re) > 0) THEN gefiltertwert := round(maxsample / (re - li) * 2)
      ELSE IF (Next^.gefiltert(li) < 0) AND (Next^.gefiltert(re) < 0) THEN
        gefiltertwert := round(maxsample / (li - re) * 2)
      ELSE
        gefiltertwert := 0;
      gefiltert := gefiltertwert;
      posilinkswert  := li;
      posirechtswert := re;
    END;
  END;
END;

{ Spikefilter }

CONSTRUCTOR spikefilter.neu(k : BYTE; millisek, ablinks, abrechts : EXTENDED);
VAR
  ableitungein : einheittyp;
BEGIN
  ms           := millisek;
  ableitungein := belegungsliste[k];
  Dec(ableitungein.sekunde);
  ableitungein.faktor := ableitungein.faktor * fre;
  abli                := ablinks / ableitungein.faktor;
  abre                := abrechts / ableitungein.faktor;
  Name                := 'Spike '#29 + extwort(ms, 3, 1) + 'ms '#24 + extwort(ablinks, 3, 0) + ableitungein.einhwort +
    ' ' + #25 + extwort(abrechts, 3, 0) + ableitungein.einhwort;
END;

CONSTRUCTOR spikefilter.load(VAR s : tbufstream);
BEGIN
  filter.load(s);
  s.Read(ms, sizeof(EXTENDED));
  s.Read(abli, sizeof(EXTENDED));
  s.Read(abre, sizeof(EXTENDED));
END;

PROCEDURE spikefilter.store(VAR s : tbufstream);
BEGIN
  filter.store(s);
  s.Write(ms, sizeof(EXTENDED));
  s.Write(abli, sizeof(EXTENDED));
  s.Write(abre, sizeof(EXTENDED));
END;

PROCEDURE spikefilter.vorbereitung(frequenz : EXTENDED);
BEGIN
  abstli := round(abli * fre / frequenz);
  abstre := round(abre * fre / frequenz);
  anz    := min(round(ms * frequenz / 1000), spikemax);
END;

FUNCTION spikefilter.gefiltert(posi : bigint64) : sample;
LABEL
  weiter;
VAR
  bei, bei2, bis : LONGINT;
  mitte :          ARRAY[-spikemax..spikemax] OF sample;
BEGIN
  FOR bei := -anz TO anz DO mitte[bei] := Next^.gefiltert(posi + bei);
  bei := 0;
  WHILE mitte[bei] - mitte[bei - 1] < abstli DO
  BEGIN
    Dec(bei);
    IF bei - 1 < -anz THEN GOTO weiter;
  END;
  bei2 := bei;
  REPEAT
    Dec(bei2);
    IF bei2 - 1 < -anz THEN GOTO weiter;
  UNTIL mitte[bei2] - mitte[bei2 - 1] < abstli;
  bis := bei2 + anz;
  REPEAT
    Inc(bei);
    IF bei > bis THEN GOTO weiter;
  UNTIL mitte[bei] - mitte[bei - 1] <= -abstre;
  REPEAT
    Inc(bei);
    IF bei > bis THEN GOTO weiter;
  UNTIL mitte[bei] - mitte[bei - 1] > -abstre;
  IF bei <= 0 THEN GOTO weiter;
  gefiltert := round(mitte[bei2] + (mitte[bei - 1] - mitte[bei2]) / (bei2 - bei - 1.0) * bei2);
  exit;

  weiter :
    gefiltert := mitte[0];
END;

PROCEDURE neukan(kanaele : BYTE);
VAR
  i : BYTE;
BEGIN
  kan := kanaele;
  FOR i := 1 TO maxchannelsandfilters - 1 DO filterloesch(i);
END;

PROCEDURE beschriftungen(VAR ko : headerdata);
BEGIN
  FOR i := 0 TO kan - 1 DO
  BEGIN
    schriftliste[i] := copy(ko.channels[i].Name, 1, 10);
    grund[i]        := defaulteinheit;
    grund[i].gain   := ko.channels[i].factor1 * ko.channels[i].factor2;
    grund[i].setz(1, copy(ko.channels[i].channelunit, 1, 7));
  END;
  FOR i := kan TO kan + maxfilters - 1 DO schriftliste[i] := schriftliste[i MOD kan];
  schriftliste[maxchannelsandfilters] := '- ';
END;

PROCEDURE kanalsetz(k, vonk : BYTE);
BEGIN
  filterloesch(k);
  dispose(filteranfang[k], alt);
  IF vonk < kan THEN
  BEGIN
    filteranfang[k] := new(endezg, neu(vonk));
    schriftliste[k] := schriftliste[vonk];
  END
  ELSE
  BEGIN
    filteranfang[k] := new(weiterzg, neu(vonk));
    schriftliste[k] := '[#' + wort(vonk) + ']';
  END;
END;

PROCEDURE filtersetz(hilf : filterzeiger; k : BYTE);
BEGIN
  hilf^.Next      := filteranfang[k];
  filteranfang[k] := hilf;
END;

PROCEDURE filterloesch(k : BYTE);
VAR
  hilf : filterzeiger;
BEGIN
  hilf := filteranfang[k];
  WHILE hilf <> nil DO
  BEGIN
    filteranfang[k] := filteranfang[k]^.Next;
    dispose(hilf, alt);
    hilf := filteranfang[k];
  END;
  filteranfang[k] := new(endezg, neu(0));
END;

FUNCTION filterdrin(k : BYTE) : BOOLEAN;
BEGIN
  filterdrin := (typeof(filteranfang[k]^) <> typeof(ende)) AND (typeof(filteranfang[k]^) <> typeof(weiter));
END;

FUNCTION filterzeile(k : BYTE) : STRING;
VAR
  puffer : STRING;
  bei :    filterzeiger;
BEGIN
  bei    := filteranfang[k];
  puffer := '';
  WHILE bei <> nil DO
  BEGIN
    insert(bei^.Name + ' '#26' ', puffer, 0);
    bei := bei^.Next;
  END;
  filterzeile := puffer + '#' + wort(k);
END;

PROCEDURE einheitensetzen(frequenz : EXTENDED);
VAR
  i : WORD;
BEGIN
  fre := frequenz;
  FOR i := 0 TO kan - 1 DO WITH belegungsliste[i] DO
    BEGIN
      grundsetzen(i);
      handlich;
    END;
  FOR i := kan TO kan + maxfilters - 1 DO WITH belegungsliste[i] DO
    BEGIN
      filteranfang[i]^.einheitgenerieren(belegungsliste[i]);
      IF schwierigda THEN schwierig
      ELSE
        handlich;
    END;
END;

PROCEDURE openfileheader(Name : string80; VAR ko : headerdata);
VAR
  i :    LONGINT;
  hilf : filterzeiger;
BEGIN
  daff.openfileheader(Name, ko);
  diffaktor := ko.frequency / fre / 2;
  FOR i := kan TO kan + maxfilters - 1 DO
  BEGIN
    hilf := filteranfang[i];
    WHILE hilf^.Next <> nil DO
    BEGIN
      hilf^.vorbereitung(ko.frequency);
      hilf := hilf^.Next;
    END;
  END;
END;

FUNCTION dat(posi : bigint64; k : BYTE) : sample;
BEGIN
  IF k < kan THEN dat := lesef(posi, k)
  ELSE
    dat               := filteranfang[k]^.gefiltert(posi);
END;

FUNCTION extspannung(y : EXTENDED; kanal : BYTE) : EXTENDED;
BEGIN
  extspannung := y * belegungsliste[kanal].faktor;
END;

FUNCTION spannung(y : EXTENDED; kanal : BYTE) : bigint64;
BEGIN
  spannung := round(extspannung(y, kanal));
END;

FUNCTION norm(sp : EXTENDED; kanal : BYTE) : EXTENDED;
BEGIN
  norm := sp / belegungsliste[kanal].faktor;
END;

PROCEDURE streamput(VAR s : tbufstream);
VAR
  i : BYTE;
BEGIN
  s.Write(kan, sizeof(kan));
  s.Write(fre, sizeof(fre));
  s.Write(grund, sizeof(grund));
  s.Write(schriftliste, sizeof(schriftliste));
  FOR i := kan TO kan + maxfilters - 1 DO s.put(filteranfang[i]);
END;

PROCEDURE streamget(VAR s : tbufstream);
VAR
  i : BYTE;
BEGIN
  s.Read(kan, sizeof(kan));
  s.Read(fre, sizeof(fre));
  s.Read(grund, sizeof(grund));
  s.Read(schriftliste, sizeof(schriftliste));
  FOR i := kan TO kan + maxfilters - 1 DO
  BEGIN
    filterloesch(i);
    dispose(filteranfang[i], alt);
    filteranfang[i] := filterzeiger(s.get);
  END;
  einheitensetzen(fre);
END;

BEGIN
  registertype(rende);
  registertype(rweiter);
  registertype(rinvert);
  registertype(roffset);
  registertype(rmalfaktor);
  registertype(rstreckung);
  registertype(raddition);
  registertype(rkappen);
  registertype(rabsolut);
  registertype(rint);
  registertype(rsquare);
  registertype(rglatt);
  registertype(rtiefpass);
  registertype(rhochpass);
  registertype(rdiff);
  registertype(rverschiebe);
  registertype(rmaxmin);
  registertype(rspikefilter);
  registertype(reinsdurch);
  registertype(rglint);
  registertype(rgllin);
  registertype(rkorrelation);
  registertype(rarcsin);
  registertype(rarccos);
  registertype(rdigital);
  registertype(rwinkel);
  registertype(rbetrag);

  FOR i := 1 TO maxchannelsandfilters - 1 DO filteranfang[i] := new(endezg, neu(0));
END.
