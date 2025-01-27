{ Borland-Pascal 7.0 / FPC 3.2.2 }
{$ifdef fpc} {$mode TP} {$endif}

PROGRAM neurolab;
{Authors: Berthold Hedwig & Marko Knepper}

{$IFDEF MSDOS}
{$A+,B-,E+,F-,G-,I-,N+,O-,P+,T+,V+,X-} //{$M 65520,130000,655360}
{$ELSE}
{$A+,B-,E+,F-,G+,I-,N+,P+,T+,V+,X-}//{$M 65520,0}
{$ENDIF}

USES
  crt,
  dos,
  daff,
  wavpcm,
  tulab42,
  nlrahmen,
  grafik,
  tlfilter,
  nltrigg,
  nlgrafik,
  bequem,
  tlfiles,
  nlfiles,
  objects,
  nlsicht,
  nlausw;

CONST
  version             = '10.01';
  paramver            = '8.5'; {Beim Erhoehen Diffilter aus nltrigg rausnehmen und komp84 abschaffen!}
  {$IFDEF DPMI}
   plattform='DPMI  BP7';
  {$ENDIF}
  {$IFDEF MSDOS}
  plattform='MSDOS BP7';
  {$ENDIF}
  {$IFDEF fpc}
    plattform={$I %FPCTARGET%}+' '+{$I %FPCVERSION%};
  {$ENDIF}
  {$IFDEF FPC}
 maxavail=9223372036854775807; memavail=maxavail;
  {$ENDIF}
  {$ifdef fpc}
 nlbext='.nlx';
  {$else}
  nlbext              = '.nlb';
  {$endif}
  id : STRING[8]      = 'Neurl' + paramver;
  sichername : STRING = 'NEUROLAB';
  {$IFDEF fpc}
 pufgroesse=65535;
  {$ELSE}
  pufgroesse          = 32768;
  {$ENDIF}

VAR
  exitsave : pointer;
  named :    dirstr;
  namen :    namestr;
  namee :    extstr;

  PROCEDURE verstaerkungen;
  VAR
    i, j :    grossint;
    mult :    EXTENDED;
    textstr : string20;

    PROCEDURE tabelle(von : BYTE);
    VAR
      i : BYTE;
    BEGIN
      IF von <= kan - 1 THEN
        writeln('Channel' : 7, 'Label' : 11, 'Factor' : 11, 'Unit' : 8);
      FOR i := von TO min(von + 7, kan - 1) DO WITH grund[i] DO
          writeln(i : 4, schriftliste[i] : 14, extewort(multi, 2, 2) : 11, einhwort : 8);
    END;

  BEGIN
    showtitle(False, 'Calibration', 'Info', farbe2);
    i := 0;
    REPEAT
      window(1, 3, 38, 12);
      clrscr;
      tabelle(0);
      window(43, 3, 80, 12);
      clrscr;
      tabelle(8);
      window(1, 13, 80, 19);
      zwischen('Menu', farbe2);
      writeln(lfcr, '  c...Calibration', lfcr, '  m...Main Menu', lfcr);
      window(1, 18, 80, 25);
      clrscr;
      zwischen('Dialogue', farbe2);
      writeln;
      CASE upcase(readcharim('Menu Point', 'm')) OF
        'C' : BEGIN
          clrscr;
          zwischen('Dialogue', farbe3);
          window(1, 20, 80, 25);
          j := readint('Channel No.', i);
          IF NOT (j IN [0..kan - 1]) THEN
          BEGIN
            writeln;
            fehler('Undefined channel number');
            warte;
          END
          ELSE
          BEGIN
            i       := j;
            textstr := readstring('Basic SI Unit', grund[i].einhwort);
            writeln('The factor describes how many ', textstr,
              ' are equivalent to 1V at the A-D-Converter.');
            mult := readexte('Factor [' + textstr + ']', grund[i].multi, 2, 2);
            IF (mult > 1e-19) AND (mult < 1e19) THEN
              grund[i].setz(mult, textstr)
            ELSE
            BEGIN
              fehler('Value out of range');
              warte;
            END;
          END;
        END;
        'M' : BEGIN
          einheitensetzen(fre);
          exit;
        END;
      END;
    UNTIL False;
  END;

  PROCEDURE filterung;
  VAR
    indexalt : BYTE;
    wahl :     CHAR;
    wx, wy :   BYTE;
    liste :    filterliste;

    PROCEDURE filteruebersicht;
    BEGIN
      writeln('Filters:');
      writeln(' y : y-Resolution   a : Amplification  - : Invert Sign    s : Spike Filter',
        lfcr, ' j : Points (TL)    o : +/- Offset     v : Absolute Value g : Gliding Av.    ',
        lfcr, ' c : Clip           h : High-Pass      2 : Square         z : Gliding Length ',
        lfcr, ' t : +/- Time Shift l : Low-Pass       r : Reciprocal V.  f : Frequency (TL) ',
        lfcr, ' # : Count (TL)     d : Differentiate  + : Summation      i : Interval (TL)',
        lfcr, ' p : Polygon (TL)   w : Integration    e : arcsin         x : Time Diff.(TL)',
        lfcr, ' m : Max - Min      n : Gl. Integr.    k : arccos         b : Phase (TL)',
        lfcr, ' = : Correlation    > : ASCII Data     . : Pulse counter  / : Angle',
        lfcr, ' q : Absolute (x y) u : Integration (TL)');
    END;

    PROCEDURE filtersetzen;
    VAR
      ableitungein : einheittyp;
      ke, k, i :     BYTE;
      fi :           string80;
    BEGIN
      showtitle(False, 'Filters', 'Info', farbe3);
      belegungzeigen;
      gotoxy(1, 15);
      zwischen('Dialogue', farbe3);
      writeln;
      ke := readint('Input Channel No. (0-' + wort(pred(kan)) + ')', 0);
      IF NOT (ke IN [0..kan + maxfilters - 1]) THEN
      BEGIN
        fehler('Undefined channel no.');
        warte;
        exit;
      END;
      clrscr;
      writeln('Input Channel: ', ke, ' (', schriftliste[ke], ')', lfcr);
      liste.zeigen(11, kan);
      gotoxy(1, 15);
      zwischen('Dialogue', farbe3);
      Write(lfcr, 'Continue filter list? (Y/N) ');
      WHILE NOT liste.ende AND (readkey IN ['Y', 'y', 'J', 'j']) DO liste.weiterzeigen;
      Write(#13);
      clreol;
      k := kan;
      WHILE filterdrin(k) AND (k < maxkanal) DO Inc(k);
      k := readint('Output Channel No. (' + wort(kan) + '-' + wort(kan + maxfilters - 1) + ')', k);
      IF NOT ((k IN [kan..kan + maxfilters - 1]) AND (k <> ke)) THEN
      BEGIN
        fehler('Valid output channel no. expected');
        warte;
        exit;
      END;
      clrscr;
      writeln('Input Channel: ', ke, ' (', schriftliste[ke], ')', lfcr,
        'Output Channel: ', k);
      Write('Previous: ');
      IF filterdrin(k) THEN Write(filterzeile(k));
      gotoxy(1, 5);
      filteruebersicht;
      gotoxy(1, 16);
      zwischen('Dialogue', farbe3);
      window(1, 20, 80, 25);
      kanalsetz(k, ke);
      fi := readstring('New Filters', '');
      FOR i := 1 TO length(fi) DO
        CASE fi[i] OF
          '2' : filtersetz(new(squarezg, neu), k);
          'a' : filtersetz(new(malfaktorzg, neu(readext('Amplification: Factor', 1, 4, 2))), k);
          'b' : filtersetz(new(phasenfilterzg, neu(upcase(readchar('Phase filter: Reference List', 'A')),
              upcase(readchar('              Event List', 'B')), readint(
              '              Offset Periods', 0))), k);
          'c' : filtersetz(new(kappenzg, neu), k);
          'd' : filtersetz(new(diffzg, neu), k);
          'e' : filtersetz(new(arcsinzg, neu), k);
          'f' : filtersetz(new(freqfilterzg, neu(
              upcase(readchar('Frequency: for Trigger List (A-' + listmax + ')', 'A')))), k);
          'g' : filtersetz(new(glattzg, neu(readext('Gliding Average: Width [ms]', 1, 3, 1))), k);
          'h' : filtersetz(new(hochpasszg, neu(readint('High-Pass: Frequency (min. ' +
              wort(round(genau * pi / weite * fre)) + ' Hz)', round(fre / 2)))), k);
          'i' : filtersetz(new(intervallfilterzg, neu(
              upcase(readchar('Interval: for Trigger List (A-' + listmax + ')', 'A')))), k);
          'j' : filtersetz(new(punktefilterzg, neu(
              upcase(readchar('Points: Trigger List (A-' + listmax + ')', 'A')))), k);
          'k' : filtersetz(new(arccoszg, neu), k);
          'l' : filtersetz(new(tiefpasszg, neu(readint('Low-Pass: Frequency (min. ' +
              wort(round(genau * pi / weite * fre)) + ' Hz)', round(fre / 2)))), k);
          'm' : filtersetz(new(maxminzg, neu(readext('Max - Min: Width'#29' [ms]', 1, 3, 1))), k);
          'n' : filtersetz(new(glintzg, neu(readext('Gliding Integration: Width [ms]', 1, 3, 1))), k);
          'o' : BEGIN
            einheitensetzen(fre);
            filtersetz(new(offsetzg, neu(k, round(readext('Offset: Value [' +
              belegungsliste[k].einhwort + ']', 0, 3, 1) / belegungsliste[k].faktor))), k);
          END;
          'p' : filtersetz(new(polygonfilterzg, neu(
              upcase(readchar('Polygon: for Trigger List (A-' + listmax + ')', 'A')))), k);
          'q' : filtersetz(new(betragzg, neu(readint('Absolute (x y): y channel No', 0))), k);
          'r' : filtersetz(new(einsdurchzg, neu), k);
          's' : BEGIN
            einheitensetzen(fre);
            ableitungein := belegungsliste[k];
            Dec(ableitungein.sekunde);
            filtersetz(new(spikefilterzg, neu(k, readext('Spike Filter: Max. Width '#29' [ms]', 3, 3, 1),
              readext('              Rising Gradient  '#24' [' + ableitungein.einhwort + ']', 0, 3, 1),
              readext('              Falling Gradient '#25' [' + ableitungein.einhwort + ']', 0, 3, 1))), k);
          END;
          't' : filtersetz(new(verschiebezg, neu(readext('Time Shift: Value [ms]', 0, 3, 1))), k);
          'u' : filtersetz(new(tlintfilterzg, neu(
              upcase(readchar('Integration: for Trigger List (A-' + listmax + ')', 'A')))), k);
          'v' : filtersetz(new(absolutzg, neu), k);
          'w' : filtersetz(new(intzg, neu), k);
          'x' : filtersetz(new(diffilterzg, neu(upcase(readchar('Time Difference: Reference List', 'A')),
              upcase(readchar('                 Event List', 'B')), readint(
              '                 Time Window [ms]', 100), upcase(
              readchar('                 n=nearest, f=forward, b=backward', 'n')))), k);
          'y' : BEGIN
            einheitensetzen(fre);
            filtersetz(new(streckungzg, neu(k, readext('y-Resolution: max value [' +
              belegungsliste[k].einhwort + ']', spannung(maxsample, k), 4, 2))), k);
          END;
          'z' : filtersetz(new(gllinzg, neu(readext('Gliding Length: Width [ms]', 1, 3, 1))), k);
          '-' : filtersetz(new(invertzg, neu), k);
          '+' : filtersetz(new(additionzg, neu(readint('Summation: Channel No', 0))), k);
          '=' : filtersetz(new(korrelationzg, neu(readint('Correlation: Channel No', 0),
              readext('Correlation: Width [ms]', 1, 3, 1))), k);
          '#' : filtersetz(new(zaehltfilterzg,
              neu(upcase(readchar('Count: Trigger List (A-' + listmax + ')', 'A')))), k);
          '>' : filtersetz(new(asciifilterzg, neu(readstring('ASCII Data: File Name', 'list.asc'),
              upcase(readchar('            Assign to Trigger List (A-' + listmax + ')', 'A')))), k);
          '/' : filtersetz(new(winkelzg, neu(readint('x-y-angle: x channel No', 0))), k);
          '.' : BEGIN
            einheitensetzen(fre);
            filtersetz(new(digitalzg, neu(k, round(readext('Pulse counter: Threshold [' +
              belegungsliste[k].einhwort + ']', 500, 3, 1) / belegungsliste[k].faktor),
              readstring('               Pulse separation SI unit', '1'),
              readext('               Pulse separation value', 1, 5, 3))), k);
          END

          ELSE
            fehler('Filter "' + fi[i] + '" not defined.');
        END;
      indexalt := k;
    END;

  BEGIN
    indexalt := kan;
    REPEAT
      showtitle(False, 'Filter Manager', 'Info', farbe2);
      liste.zeigen(11, indexalt);
      writeln;
      zwischen('Menu', farbe2);
      writeln;
      writeln('f...Info Filter                          d...Declare Filter', lfcr,
        'o...Info Output Channels (continue)      m...Main Menu', lfcr,
        'i...Info Input Channels');
      writeln;
      zwischen('Dialogue', farbe2);
      writeln;
      Write('Menu Point: m'#8);
      REPEAT
        wahl := readkey;
        wx   := wherex;
        wy   := wherey;
        window(1, 3, 80, 15);
        clrscr;
        CASE wahl OF
          'o' : BEGIN
            indexalt := liste.index;
            liste.zeigen(11, liste.index);
          END;
          'i' : BEGIN
            belegungzeigen;
            liste.index := indexalt;
          END;
          'f' : BEGIN
            filteruebersicht;
            liste.index := indexalt;
          END;
          'd' : filtersetzen;
          'm', #13, #27 : BEGIN
            einheitensetzen(fre);
            exit;
          END;
        END;
        window(1, 3, 80, 25);
        gotoxy(wx, wy);
      UNTIL wahl = 'd';
    UNTIL False;
  END;

  PROCEDURE listen;
  CONST
    aktfile : BYTE = 1;
    k : BYTE       = 0;
  VAR
    i, j : LONGINT;
  BEGIN
    showtitle(False, 'Data List', 'Info', farbe2);
    fileliste;
    gotoxy(1, 19);
    zwischen('Dialogue', farbe2);
    writeln;
    i := readint('File No.', aktfile);
    IF NOT (i IN [1..filenr]) THEN
    BEGIN
      fehler('Undefined File No.');
      warte;
      exit;
    END;
    aktfile := i;
    clrscr;
    belegungzeigen;
    writeln;
    gotoxy(1, 19);
    zwischen('Dialogue', farbe2);
    writeln;
    i := readint('Channel No.', k);
    IF NOT (i IN [0..pred(kan)]) THEN
    BEGIN
      fehler('Undefinded Channel No.');
      warte;
      exit;
    END;
    k := max(i, 0);
    oeffnen(aktfile);
    i := zwi(messw(readint('Start Time [ms]', 0)));
    clrscr;
    FOR j := i TO liste[aktfile].ko.anzahl - 1 DO
    BEGIN
      Write(lesef(j, k) : 10);
      IF j MOD 168 = 167 THEN
      BEGIN
        writeln('Continue: <Return>, Abort: <Esc>');
        IF readkey = #27 THEN
        BEGIN
          schliesse;
          exit;
        END;
      END;
    END;
    schliesse;
    warte;
  END;

  PROCEDURE analogdaten;

    PROCEDURE superposition;
    CONST
      von : messwert  = 0;
      bis : messwert  = 1000;
      akttrind : CHAR = 'A';
    VAR
      laenge : grossint;
      chpuff : CHAR;
      grafik : grafiksuperposition;
    BEGIN
      showtitle(False, 'Superposition', 'Info', farbe3);
      triggeruebersicht;
      gotoxy(1, 18);
      zwischen('Dialogue', farbe3);
      writeln;
      chpuff := upcase(readchar('Trigger List', akttrind));
      IF NOT (chpuff IN ['A'..listmax]) THEN
      BEGIN
        fehler('Undefined Trigger List');
        warte;
        exit;
      END;
      akttrind := chpuff;
      IF tliste[akttrind]^.triggsum = 0 THEN
      BEGIN
        writeln('No trigger point.');
        warte;
        exit;
      END;
      clrscr;
      kanaele.lesen(5, farbe3);
      IF NOT (kanaele.kn IN [1..32]) THEN exit;
      window(1, 22, 80, 25);
      clrscr;
      von    := messwext(readext('Start Time [ms]', extzeit(von), 1, 0));
      bis    := messwext(readext('End Time [ms]  ', extzeit(bis), 1, 0));
      laenge := round(bis - von);
      IF (laenge > maxanzahl) OR (laenge <= 0) THEN
      BEGIN
        fehler('Undefined time window');
        warte;
        exit;
      END;
      grafik.aufbauen(kanaele, von, bis, akttrind);
    END;

    PROCEDURE averagen;
    CONST
      von : messwert  = 0;
      bis : messwert  = 500;
      akttrind : CHAR = 'A';
    VAR
      laenge, platz : grossint;
      i, gesamt :     grossint;
      chpuff :        CHAR;
      grafik :        grafikaverage;
    BEGIN
      showtitle(False, 'Averaging', 'Info', farbe3);
      triggeruebersicht;
      gotoxy(1, 18);
      zwischen('Dialogue', farbe3);
      writeln;
      chpuff := upcase(readchar('Trigger List', akttrind));
      IF NOT (chpuff IN ['A'..listmax]) THEN
      BEGIN
        fehler('Undefined Trigger List');
        warte;
        exit;
      END;
      akttrind := chpuff;
      gesamt   := tliste[akttrind]^.triggsum;
      IF gesamt = 0 THEN
      BEGIN
        writeln('No trigger point.');
        warte;
        exit;
      END;
      clrscr;
      kanaele.lesen(5, farbe3);
      IF NOT (kanaele.kn IN [1..32]) THEN exit;
      window(1, 22, 80, 25);
      clrscr;
      writeln('Maximum Averaging Time: ', zeit(maxanzahl) - 1, ' ms.');
      von    := messwext(readext('Start Time [ms]', extzeit(von), 1, 0));
      bis    := messwext(readext('End Time [ms]  ', extzeit(bis), 1, 0));
      laenge := round(bis - von);
      platz  := (laenge + 1) * sizeof(wert);
      IF (laenge > maxanzahl) OR (laenge <= 0) THEN
      BEGIN
        fehler('Undefined time window');
        warte;
        exit;
      END;
      {$ifndef fpc}
      FOR i := 0 TO maxkanal DO IF i IN kanaele.dabei THEN
        BEGIN
          IF maxavail < platz THEN
          BEGIN
            fehler('Memory not sufficient');
            writeln(lfcr, 'Help: Use less channels or shorter averaging time.');
            FOR i := i - 1 DOWNTO 0 DO IF i IN kanaele.dabei THEN
                freemem(grafik.mittel[i], platz);
            warte;
            exit;
          END;
          getmem(grafik.mittel[i], platz);
          fillchar(grafik.mittel[i]^, platz, 0);
        END;
    {$endif}
      window(1, 3, 80, 25);
      clrscr;
      WITH tliste[akttrind]^ DO
      BEGIN
        gotoxy(1, 3);
        writeln('Trigger List   : ', akttrind, ' - ', Name,
          lfcr, 'Number of Files: ', fileanz,
          lfcr, 'Channel No.    : ', kanaele.ausgabe,
          lfcr, 'Trigger Points : ', gesamt,
          lfcr, 'Averaging      : from ', zeit(von), ' ms to ', zeit(bis), ' ms');
      END;
      gotoxy(1, 18);
      zwischen('Dialogue', farbe3);
      gotoxy(1, 23);
      Write('Abort: <Esc>');
      gotoxy(1, 20);
      grafik.aufbauen(kanaele, von, laenge, akttrind, gesamt);
      {$ifndef fpc}
      FOR i := 0 TO maxkanal DO IF i IN kanaele.dabei THEN freemem(grafik.mittel[i], platz);
    {$endif}
    END;

    PROCEDURE phasenaveragen;
    CONST
      von                = 0;
      minabst : messwert = 0;
      maxabst : messwert = 10000;
      akttrind : CHAR    = 'A';
    VAR
      weis :             triggerweiser;
      i, laenge, platz : grossint;
      bis :              messwert;
      chpuff :           CHAR;
      grafik :           grafikphasenaverage;
    BEGIN
      showtitle(False, 'Phase Dependent Averaging', 'Info', farbe3);
      triggeruebersicht;
      gotoxy(1, 18);
      zwischen('Dialogue', farbe3);
      writeln;
      chpuff := upcase(readchar('Trigger List', akttrind));
      IF NOT (chpuff IN ['A'..listmax]) THEN
      BEGIN
        fehler('Undefined Trigger List');
        warte;
        exit;
      END;
      akttrind := chpuff;
      clrscr;
      kanaele.lesen(5, farbe3);
      IF NOT (kanaele.kn IN [1..32]) THEN exit;
      window(1, 22, 80, 25);
      clrscr;
      writeln('Maximum Averaging Time: ', zeit(maxanzahl) - 1, ' ms.');
      minabst := messwext(readext('Min. Cycle Duration [ms]', extzeit(minabst), 1, 0));
      maxabst := messwext(readext('Max. Cycle Duration [ms]', extzeit(maxabst), 1, 0));
      weis.zaehlen(tliste[akttrind]^, minabst, maxabst);
      IF weis.gesamt = 0 THEN
      BEGIN
        writeln('No trigger point');
        piep;
        warte;
        exit;
      END;
      bis    := weis.mittelabstand;
      laenge := round(bis - von);
      platz  := (laenge + 1) * sizeof(wert);
      IF laenge > maxanzahl THEN
      BEGIN
        fehler('Averaging time too long.');
        warte;
        exit;
      END;
      {$ifndef fpc}
      FOR i := 0 TO maxkanal DO IF i IN kanaele.dabei THEN
        BEGIN
          IF maxavail < platz THEN
          BEGIN
            fehler('Memory not sufficient');
            writeln(lfcr, 'Help: Use less channels or shorter averaging time.');
            FOR i := i - 1 DOWNTO 0 DO IF i IN kanaele.dabei THEN
                freemem(grafik.mittel[i], platz);
            warte;
            exit;
          END;
          getmem(grafik.mittel[i], platz);
          fillchar(grafik.mittel[i]^, platz, 0);
        END;
    {$endif}
      window(1, 3, 80, 25);
      clrscr;
      WITH tliste[akttrind]^ DO
      BEGIN
        gotoxy(1, 3);
        writeln('Trigger List   : ', akttrind, ' - ', Name,
          lfcr, 'File Number    : ', fileanz,
          lfcr, 'Channel No.    : ', kanaele.ausgabe,
          lfcr, 'Trigger Points : ', weis.gesamt,
          lfcr, 'Cycle Duration : min. ', zeit(minabst), ' ms, max. ', zeit(maxabst), ' ms',
          lfcr, 'Mean Duration  : ', zeit(weis.mittelabstand), ' ms');
      END;
      gotoxy(1, 18);
      zwischen('Dialogue', farbe3);
      gotoxy(1, 23);
      Write('Abort: <Esc>');
      gotoxy(1, 20);
      grafik.aufbauen(kanaele, von, laenge, akttrind, weis.gesamt, weis);
      weis.frei;
      {$ifndef fpc}
      FOR i := 0 TO maxkanal DO IF i IN kanaele.dabei THEN freemem(grafik.mittel[i], platz);
    {$endif}
    END;

    PROCEDURE xydiagramm;
    CONST
      kx : grossint = 0;
      ky : grossint = 1;
    VAR
      puff :   grossint;
      liste :  filterliste;
      grafik : grafikxy;
    BEGIN
      showtitle(False, 'X-Y-Diagram', 'Info', farbe3);
      belegungzeigen;
      writeln;
      liste.zeigen(7, kan);
      writeln;
      zwischen('Dialogue', farbe3);
      Write(lfcr, 'Continue list? (Y/N) ');
      WHILE NOT liste.ende AND (readkey IN ['Y', 'y', 'J', 'j']) DO liste.weiterzeigen;
      Write(#13);
      clreol;
      puff := readint('X-channel', kx);
      IF NOT (puff IN [0..kan + maxfilters - 1]) THEN
      BEGIN
        fehler('Undefined channel no.');
        warte;
        exit;
      END
      ELSE
        kx := puff;
      puff := readint('Y-channel', ky);
      IF NOT (puff IN ([0..kan + maxfilters - 1]) - [kx]) THEN
      BEGIN
        fehler('Undefined channel no.');
        warte;
        exit;
      END
      ELSE
        ky := puff;
      grafik.aufbauen(kx, ky);
    END;

    PROCEDURE ampnormalhist;
    CONST
      ref : CHAR          = 'A';
      diffn : WORD        = 100;
      ybereich : EXTENDED = 1;
      kanal : BYTE        = 0;
      minsamp : sample    = minsample;
      maxsamp : sample    = maxsample;
    VAR
      histogramm : ampnormalhistogramm;
      charpuff :   CHAR;
      wordpuff :   WORD;
      extpuff :    EXTENDED;
      puff :       grossint;
      liste :      filterliste;
    BEGIN
      showtitle(False, 'Amplitude Histogram', 'Info', farbe3);
      triggeruebersicht;
      gotoxy(1, 12);
      zwischen('Dialogue', farbe3);
      window(1, 16, 80, rowmax);
      charpuff := upcase(readchar('Trigger List    ', ref));
      IF NOT (charpuff IN ['A'..listmax]) THEN
      BEGIN
        fehler('Undefined Trigger List');
        warte;
        exit;
      END;
      ref := charpuff;
      window(1, 3, 80, rowmax);
      clrscr;
      textcolor(farbenormal);
      belegungzeigen;
      writeln;
      liste.zeigen(5, kan);
      writeln;
      zwischen('Dialogue', farbe3);
      Write(lfcr, 'Continue list? (Y/N) ');
      WHILE NOT liste.ende AND (readkey IN ['Y', 'y', 'J', 'j']) DO liste.weiterzeigen;
      Write(#13);
      clreol;
      window(1, 22, 80, rowmax);
      puff := readint('Channel   ', kanal);
      IF NOT (puff IN [0..kan + maxfilters - 1]) THEN
      BEGIN
        fehler('Undefined channel no.');
        warte;
        exit;
      END
      ELSE
        kanal  := puff;
      wordpuff := readint('Bins (Max. ' + wort(maxfeld) + ')', diffn);
      IF (wordpuff <= 0) OR (wordpuff > maxfeld) THEN
      BEGIN
        fehler('Undefined number of bins');
        warte;
        exit;
      END;
      diffn   := wordpuff;
      extpuff := kon(norm(readext('Min. amplitude [' + belegungsliste[kanal].einhwort + ']',
        extspannung(rekon(minsamp, kanal), kanal), 1, 1), kanal), kanal);
      minsamp := round(mine(maxe(extpuff, minsample), maxsample));
      extpuff := kon(norm(readext('Max. amplitude [' + belegungsliste[kanal].einhwort + ']',
        extspannung(rekon(maxsamp, kanal), kanal), 1, 1), kanal), kanal);
      maxsamp := round(maxe(mine(extpuff, maxsample), minsample));
      IF maxsamp <= minsamp THEN
      BEGIN
        fehler('Bad amplitude range');
        warte;
        exit;
      END;
      ybereich := readext('y-Scale Factor   ', ybereich, 4, 2);
      histogramm.aufbauen(kanal, ref, diffn, ybereich, minsamp, maxsamp);
    END;

  BEGIN
    REPEAT
      showtitle(False, 'Analog Data', 'Menu', farbe2);
      gotoxy(1, 7);
      writeln('          a...Averaging',
        lfcr, '          p...Phase-Dependent Averaging',
        lfcr, '          x...X-Y-Diagram',
        lfcr, '          h...Amplitude Histogram',
        lfcr, '          s...Superposition',
        lfcr, lfcr, '          m...Main Menu', lfcr);
      gotoxy(1, 19);
      zwischen('Dialogue', farbe2);
      writeln;
      CASE upcase(readcharim('Menu Point', 'm')) OF
        'A' : averagen;
        'P' : phasenaveragen;
        'X' : xydiagramm;
        'H' : ampnormalhist;
        'S' : superposition;
        'M' : exit;
      END;
    UNTIL False;
  END;

  PROCEDURE intervalldaten;

    PROCEDURE intervallhist;
    CONST
      ref : CHAR          = 'A';
      von : messwert      = 0;
      bis : messwert      = 500;
      diffn : WORD        = 100;
      ybereich : EXTENDED = 1;
    VAR
      histogramm : intervallhistogramm;
      charpuff :   CHAR;
      wordpuff :   WORD;
    BEGIN
      showtitle(False, 'Interval Histogram', 'Info', farbe3);
      triggeruebersicht;
      gotoxy(1, 12);
      zwischen('Dialogue', farbe3);
      window(1, 16, 80, 25);
      charpuff := upcase(readchar('Trigger List    ', ref));
      IF NOT (charpuff IN ['A'..listmax]) THEN
      BEGIN
        fehler('Undefined Trigger List');
        warte;
        exit;
      END;
      ref      := charpuff;
      von      := messw(readint('Start Time [ms] ', zeit(von)));
      bis      := messw(readint('End Time [ms]   ', zeit(bis)));
      wordpuff := readint('Bins (Max. ' + wort(maxfeld) + ')', diffn);
      IF (wordpuff <= 0) OR (wordpuff > maxfeld) THEN
      BEGIN
        fehler('Undefined number of bins');
        warte;
        exit;
      END;
      diffn    := wordpuff;
      ybereich := readext('y-Scale Factor  ', ybereich, 4, 2);
      histogramm.aufbauen(ref, von, bis, diffn, ybereich);
    END;

    PROCEDURE autokorr;
    CONST
      ref : CHAR          = 'A';
      von : messwert      = 0;
      bis : messwert      = 500;
      diffn : WORD        = 100;
      ybereich : EXTENDED = 1;
    VAR
      histogramm : autokorrelogramm;
      charpuff :   CHAR;
      wordpuff :   WORD;
    BEGIN
      showtitle(False, 'Auto Correlogram', 'Info', farbe3);
      triggeruebersicht;
      gotoxy(1, 12);
      zwischen('Dialogue', farbe3);
      window(1, 16, 80, 25);
      charpuff := upcase(readchar('Trigger List    ', ref));
      IF NOT (charpuff IN ['A'..listmax]) THEN
      BEGIN
        fehler('Undefined Trigger List');
        warte;
        exit;
      END;
      ref      := charpuff;
      von      := messw(readint('Start Time [ms] ', zeit(von)));
      bis      := messw(readint('End Time [ms]   ', zeit(bis)));
      wordpuff := readint('Bins (Max. ' + wort(maxfeld) + ')', diffn);
      IF (wordpuff <= 0) OR (wordpuff > maxfeld) THEN
      BEGIN
        fehler('Undefined number of bins');
        warte;
        exit;
      END;
      diffn    := wordpuff;
      ybereich := readext('y-Scale Factor  ', ybereich, 4, 2);
      histogramm.aufbauen(ref, von, bis, diffn, ybereich);
    END;

    PROCEDURE kreuzkorr;
    CONST
      ref : CHAR          = 'A';
      obj : CHAR          = 'B';
      von : messwert      = 0;
      bis : messwert      = 500;
      diffn : WORD        = 100;
      ybereich : EXTENDED = 1;
    VAR
      histogramm : kreuzkorrelogramm;
      charpuff :   CHAR;
      wordpuff :   WORD;
    BEGIN
      showtitle(False, 'Cross Correlogram', 'Info', farbe3);
      triggeruebersicht;
      gotoxy(1, 12);
      zwischen('Dialogue', farbe3);
      window(1, 16, 80, 25);
      charpuff := upcase(readchar('Reference TL    ', ref));
      IF NOT (charpuff IN ['A'..listmax]) THEN
      BEGIN
        fehler('Undefined Trigger List');
        warte;
        exit;
      END;
      ref      := charpuff;
      charpuff := upcase(readchar('Event TL        ', obj));
      IF NOT (charpuff IN ['A'..listmax]) THEN
      BEGIN
        fehler('Undefined Trigger List');
        warte;
        exit;
      END;
      obj      := charpuff;
      von      := messw(readint('Start Time [ms] ', zeit(von)));
      bis      := messw(readint('End Time [ms]   ', zeit(bis)));
      wordpuff := readint('Bins (Max. ' + wort(maxfeld) + ')', diffn);
      IF (diffn <= 0) OR (diffn > maxfeld) THEN
      BEGIN
        fehler('Undefined number of bins');
        warte;
        exit;
      END;
      diffn    := wordpuff;
      ybereich := readext('y-Scale Factor  ', ybereich, 4, 2);
      histogramm.aufbauen(ref, obj, von, bis, diffn, ybereich);
    END;

    PROCEDURE psthist;
    CONST
      ref : CHAR          = 'A';
      obj : CHAR          = 'B';
      von : messwert      = 0;
      bis : messwert      = 500;
      art : CHAR          = 'O';
      diffn : WORD        = 100;
      ybereich : EXTENDED = 1;
    VAR
      histogramm : psthistogramm;
      charpuff :   CHAR;
      wordpuff :   WORD;
    BEGIN
      showtitle(False, 'PST-Histogram', 'Info', farbe3);
      triggeruebersicht;
      gotoxy(1, 12);
      zwischen('Dialogue', farbe3);
      window(1, 16, 80, 25);
      charpuff := upcase(readchar('Reference TL    ', ref));
      IF NOT (charpuff IN ['A'..listmax]) THEN
      BEGIN
        fehler('Undefined Trigger List');
        warte;
        exit;
      END;
      ref      := charpuff;
      charpuff := upcase(readchar('Event TL        ', obj));
      IF NOT (charpuff IN ['A'..listmax]) THEN
      BEGIN
        fehler('Undefined Trigger List');
        warte;
        exit;
      END;
      obj      := charpuff;
      von      := messw(readint('Start Time [ms] ', zeit(von)));
      bis      := messw(readint('End Time [ms]   ', zeit(bis)));
      wordpuff := readint('Bins (Max. ' + wort(maxfeld) + ')', diffn);
      IF (wordpuff <= 0) OR (wordpuff > maxfeld) THEN
      BEGIN
        fehler('Undefined number of bins');
        warte;
        exit;
      END;
      diffn    := wordpuff;
      ybereich := readext('y-Scale Factor  ', ybereich, 4, 2);
      histogramm.aufbauen(ref, obj, -1, 1, von, bis, diffn, ybereich);
    END;

    PROCEDURE latenzhist;
    CONST
      ref : CHAR          = 'A';
      obj : CHAR          = 'B';
      von : messwert      = 0;
      bis : messwert      = 500;
      diffn : WORD        = 100;
      ybereich : EXTENDED = 1;
    VAR
      histogramm : latenzhistogramm;
      charpuff :   CHAR;
      wordpuff :   WORD;
    BEGIN
      showtitle(False, 'Latency Histogram', 'Info', farbe3);
      triggeruebersicht;
      gotoxy(1, 12);
      zwischen('Dialogue', farbe3);
      window(1, 16, 80, 25);
      charpuff := upcase(readchar('Reference TL    ', ref));
      IF NOT (charpuff IN ['A'..listmax]) THEN
      BEGIN
        fehler('Undefined Trigger List');
        warte;
        exit;
      END;
      ref      := charpuff;
      charpuff := upcase(readchar('Event TL        ', obj));
      IF NOT (charpuff IN ['A'..listmax]) THEN
      BEGIN
        fehler('Undefined Trigger List');
        warte;
        exit;
      END;
      obj      := charpuff;
      von      := messw(readint('Start Time [ms] ', zeit(von)));
      bis      := messw(readint('End Time [ms]   ', zeit(bis)));
      wordpuff := readint('Bins (Max. ' + wort(maxfeld) + ')', diffn);
      IF (wordpuff <= 0) OR (wordpuff > maxfeld) THEN
      BEGIN
        fehler('Undefined number of bins');
        warte;
        exit;
      END;
      diffn    := wordpuff;
      ybereich := readext('y-Scale Factor  ', ybereich, 4, 2);
      histogramm.aufbauen(ref, obj, von, bis, diffn, ybereich);
    END;

    PROCEDURE phasenhist;
    CONST
      ref : CHAR          = 'A';
      obj : CHAR          = 'B';
      minabst : messwert  = 0;
      maxabst : messwert  = 10000;
      anfph : EXTENDED    = 0;
      schph : EXTENDED    = 1;
      diffn : WORD        = 100;
      ybereich : EXTENDED = 1;
    VAR
      histogramm : phasenhistogramm;
      charpuff :   CHAR;
      wordpuff :   WORD;
    BEGIN
      showtitle(False, 'Phase Histogram', 'Info', farbe3);
      triggeruebersicht;
      gotoxy(1, 12);
      zwischen('Dialogue', farbe3);
      window(1, 16, 80, 25);
      charpuff := upcase(readchar('Reference TL            ', ref));
      IF NOT (charpuff IN ['A'..listmax]) THEN
      BEGIN
        fehler('Undefined Trigger List');
        warte;
        exit;
      END;
      ref      := charpuff;
      minabst  := messw(readint('Min. Cycle Duration [ms]', zeit(minabst)));
      maxabst  := messw(readint('Max. Cycle Duration [ms]', zeit(maxabst)));
      charpuff := upcase(readchar('Event TL                ', obj));
      IF NOT (charpuff IN ['A'..listmax]) THEN
      BEGIN
        fehler('Undefined Trigger List');
        warte;
        exit;
      END;
      obj   := charpuff;
      anfph := readext('Start Phase (0..1)      ', anfph, 3, 2);
      IF (anfph < 0) OR (anfph > 1) THEN
      BEGIN
        fehler('Value not allowed.');
        warte;
        exit;
      END;
      schph := readext('End Phase (0..1)        ', schph, 3, 2);
      IF (schph < 0) OR (schph > 1) THEN
      BEGIN
        fehler('Value not allowed.');
        warte;
        exit;
      END;
      wordpuff := readint('Bins (Max. ' + wort(maxfeld) + ')        ', diffn);
      IF (wordpuff <= 0) OR (wordpuff > maxfeld) THEN
      BEGIN
        fehler('Undefined number of bins');
        warte;
        exit;
      END;
      diffn    := wordpuff;
      ybereich := readext('y-Scale Factor          ', ybereich, 4, 2);
      histogramm.aufbauen(ref, obj, minabst, maxabst, anfph, schph, diffn, ybereich);
    END;

  BEGIN
    REPEAT
      showtitle(False, 'Interval Data', 'Info', farbe2);
      triggeruebersicht;
      gotoxy(1, 13);
      zwischen('Menu', farbe2);
      writeln;
      writeln('          i...Interval Histogram          a...Auto Correlogram',
        lfcr, '          l...Latency Histogram           c...Cross Correlogram',
        lfcr, '          s...PST-Histogram               p...Phase Histogram',
        lfcr, '          m...Main Menu');
      gotoxy(1, 21);
      zwischen('Dialogue', farbe2);
      writeln;
      CASE upcase(readcharim('Menu Point', 'm')) OF
        'I' : intervallhist;
        'A' : autokorr;
        'L' : latenzhist;
        'C' : kreuzkorr;
        'S' : psthist;
        'P' : phasenhist;
        'M' : exit;
      END;
    UNTIL False;
  END;

  PROCEDURE schluss; FAR; FORWARD;

  PROCEDURE sichern;
  VAR
    speicher : tbufstream;
    vorher :   pointer;
  BEGIN
    IF filenr > 0 THEN
    BEGIN
      vorher := exitproc;
      speicher.init(sichername + nlbext, stcreate, pufgroesse);
      speicher.Write(id, sizeof(id));
      tlfiles.streamput(speicher);
      nltrigg.streamput(speicher);
      tlfilter.streamput(speicher);
      IF speicher.status = stok THEN writeln('Configuration file saved!')
      ELSE
        writeln(speicher.status : 10, speicher.errorinfo : 10);
      speicher.done;
      exitproc := vorher;
    END;
  END;

  PROCEDURE confsichern;
  VAR
    filename : namestr;
    pname :    pathstr;
    dname :    dirstr;
    ename :    extstr;
  BEGIN
    pname := readstring('Configuration file name and path', sichername);
    fsplit(pname, dname, filename, ename);
    IF fileschonda(dname + filename + nlbext) THEN
      IF upcase(readchar('Overwrite? (Y/N)', 'N')) = 'Y' THEN
      BEGIN
        sichername := dname + filename;
        sichern;
      END
      ELSE
    ELSE
    BEGIN
      sichername := dname + filename;
      sichern;
    END;
  END;

  {$ifndef fpc}
  PROCEDURE grafikkarte;
  BEGIN
    ueberschrift(False, 'Grafics Adaptor', 'Info', farbe2);
    writeln(lfcr, '   Adaptor     #              Mode:  Low #   Medium #     High #',
      lfcr,
      lfcr, '   Automatic 255                         -          -        255',
      lfcr,
      lfcr, '   EGA         3                         0          -          1',
      lfcr, '   EGA64       4                         0          -          1',
      lfcr, '   Herc Mono   7                         -          -          0',
      lfcr, '   VGA         9                         0          1          2');
    {$ifndef msdos}
    writeln('   VESA 16   ', vesa16 : 3, '                         0          1          2');
    {$endif}
    gotoxy(1, 14);
    zwischen('Dialogue', farbe2);
    writeln;
    grtreiber := readint('Graphics Adaptor #', grtreiber);
    grmodus   := readint('Graphics Mode #', grmodus);
  END;
  {$endif}

  PROCEDURE holen;
  VAR
    speicher : tbufstream;
    such :     searchrec;
    idtest :   STRING[8];
  BEGIN
    findfirst(sichername + nlbext, anyfile, such);
    IF doserror = 0 THEN
    BEGIN
      exitproc := exitsave;
      speicher.init(sichername + nlbext, stopenread, pufgroesse);
      speicher.Read(idtest, sizeof(id));
      komp84 := (idtest = 'Neurl8.4') AND (id = 'Neurl8.5');
      IF komp84 THEN idtest := 'Neurl8.5';
      IF idtest = id THEN
      BEGIN
        tlfiles.streamget(speicher);
        kanaele.voreinstellung;
        nltrigg.streamget(speicher);
        tlfilter.streamget(speicher);
        IF speicher.status <> stok THEN
        BEGIN
          clrscr;
          writeln(lfcr);
          fehler('Can''t load configuration file.');
          writeln(lfcr, lfcr, speicher.status : 10, speicher.errorinfo : 10);
          halt;
        END;
      END
      ELSE
      BEGIN
        clrscr;
        writeln(lfcr);
        IF copy(idtest, 1, 5) = 'Neurl' THEN
          fehler('Configuration file (format ' + copy(idtest, 6, 3) + ') is not compatible.')
        ELSE
          fehler('Configuration file incorrect.');
        warte;
      END;
      speicher.done;
      exitproc := @schluss;
      zoeger(5000);
    END
    ELSE
      zoeger(8000);
  END;

  {$IFNDEF FPC}

  FUNCTION heapvoll(groesse : WORD) : INTEGER; FAR;
  BEGIN
    IF groesse = 0 THEN exit;
    exitproc := exitsave;
    closegraph;
    window(1, 1, 80, 25);
    clrscr;
    pieps;
    writeln(lfcr);
    IF groesse > memavail THEN Write('Memory overflow')
    ELSE
    IF groesse > maxavail THEN Write('Memory full or too small fragments')
    ELSE
      Write('Memory error');
    writeln(', please start again with fingers crossed.', lfcr);
    daff.ausserbetrieb;
    sichern;
    halt;
  END;

  {$ENDIF}

  PROCEDURE schluss;
  BEGIN
    exitproc := exitsave;
    closegraph;
    textmode(co80);
    IF erroraddr <> nil THEN
      writeln(lfcr, lfcr, 'Sorry, error causes abortion.', lfcr)
    ELSE
      writeln('Bye bye!', lfcr);
    daff.ausserbetrieb;
    sichern;
  END;

BEGIN
  exitsave  := exitproc;
  exitproc  := @schluss;
  {$IFNDEF FPC}
  heaperror := @heapvoll;
  {$ENDIF}
  IF paramcount > 0 THEN sichername := ParamStr(1);
  fsplit(sichername, named, namen, namee);
  sichername := named + namen;
  laerman;
  clrscr;
  textcolor(cyan);
  gotoxy(1, 6);
  writeln(lfcr, '' : 16, '------------------------------------------',
    lfcr, '' : 16, '----   NEUROLAB ', version : 4, ' (', plattform : 13, ')  ----',
    lfcr, '' : 16, '------------------------------------------',
    lfcr, '' : 16, '------  by B. Hedwig and M. Knepper ------',
    lfcr, '' : 16, '---------- Cambridge / Wiesbaden ---------',
    lfcr, '' : 16, '------------------------------------------');
  writeln(lfcr, lfcr);
  writeln('' : 7, 'Program for the analysis of neurobiological and behavioural data');
  textcolor(lightgray);
  gotoxy(1, 20);
  holen;
  REPEAT
    showtitle(False, 'Main Menu', 'Info', farbe1);
    {$ifdef fpc}
   writeln('Version           :   ',version:4,' (',plattform:5,')');
    {$else}
    writeln('Free Memory       :   ', memavail DIV 1024, ' (', maxavail DIV 1024, ') kByte');
    {$endif}
    writeln('Configuration File:   ', sichername + nlbext, ' (Format:', paramver, ')');
    writeln('Parameters        :   Channels (Max.): ', kan, ', Max. Sampling Rate: ', fre : 4 : 2, ' Hz');
    writeln('Open Files        :   ', filenr);
    gotoxy(1, 9);
    zwischen('Menu', farbe1);
    writeln(
      lfcr, '  f...File Manager                      t...Trigger Manager',
      lfcr, '  c...Calibration',
      lfcr, '  m...Filter Manager',
      lfcr, '  l...List Sampled Data                 a...Analog Data',
      lfcr, '  v...View Data                         i...Interval Data',
      lfcr,
      {$ifdef fpc}
     lfcr,'  -/+.Sound off/on',
      {$else}
      lfcr, '  g...Graphics Adaptor                  -/+.Sound off/on',
      {$endif}
      lfcr, '  s...Save Configuration File           e...End', lfcr);
    zwischen('Dialogue', farbe1);
    window(1, 24, 80, 25);
    IF filenr = 0 THEN
    BEGIN
      CASE upcase(readcharim('Menu Point', 'f')) OF
        '+' : laerman;
        '-' : laermaus;
        'F' : nlfiles.manager;
        'E' : halt;
        'M', 'L', 'T', 'C', 'A', 'V', 'S', 'I' : BEGIN
          fehler('No open file: Start with file manager!');
          warte;
        END;
      END;
    END
    ELSE
    BEGIN
      CASE upcase(readcharim('Menu Point', 'e')) OF
        '+' : laerman;
        '-' : laermaus;
        'F' : nlfiles.manager;
        'C' : verstaerkungen;
        'M' : filterung;
        'L' : listen;
        'V' : viewdata;
        'T' : nltrigg.manager;
        'S' : confsichern;
        {$ifndef fpc}
        'G' : grafikkarte;
        {$endif}
        'A' : analogdaten;
        'I' : intervalldaten;
        'E' : BEGIN
          Write(lfcr, 'Exit? (Y/N) ');
          IF readkey IN ['j', 'J', 'y', 'Y'] THEN halt;
        END;
        '?' : BEGIN
          window(1, 1, 80, 25);
          clrscr;
          zeigertest;
        END;
      END;
    END;
  UNTIL False;
END.
