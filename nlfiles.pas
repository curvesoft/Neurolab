{ Borland-Pascal 7.0 / FPC 3.2.2 }
{$ifdef fpc} {$mode TP} {$endif}

UNIT nlfiles;

{$IFDEF MSDOS}
{$A+,B-,F-,G-,I-,N+,O-,P+,T+,V+,X-}
{$ELSE}
{$A+,B-,F-,G+,I-,N+,P+,T+,V+,X-}
{$ENDIF}

INTERFACE

USES  crt, daff, wavpcm, tulab42, nlrahmen, plotter,
  dos, tlfilter, nltrigg,
  bequem, tlfiles,
  objects;

PROCEDURE manager;

IMPLEMENTATION

CONST
  leername = '   -';

VAR
  filind : BYTE;

PROCEDURE voreinstellung;
BEGIN
  showtitle(False, 'Conditions for file sets', 'Dialogue', farbe3);
  IF filenr = 0 THEN
  BEGIN
    fre := readext('  maximum sampling rate [Hz]', fre, 1, 0);
    kan := readint('maximum number of channels', kan);
    IF NOT (kan IN [0..maxchannels]) THEN
    BEGIN
      writeln;
      fehler('Maximum of channels:' + wort(maxchannels));
      kan := 0;
      warte;
      exit;
    END;
    IF kan > 0 THEN
    BEGIN
      neukan(kan);
      kanaele.voreinstellung;
    END;
  END
  ELSE
  BEGIN
    fehler('Define conditions before opening a file.');
    warte;
  END;
END;

PROCEDURE proto(nr : BYTE);
VAR
  wy : BYTE;

  PROCEDURE list(von : BYTE);
  VAR
    i : BYTE;
  BEGIN
    WITH liste[nr].head DO
    BEGIN
      IF von <= kan - 1 THEN writeln('Nl# TL#', 'Label' : 14, 'Factor' : 11, 'Unit' : 7);
      FOR i := von TO min(von + 7, kan - 1) DO
        writeln(i : 3, channels[i].nr : 3, channels[i].Name : 15,
          extewort(10 / (channels[i].factor1 * channels[i].factor2) / maxsample, 2, 2) : 11,
          copy(channels[i].channelunit, 1, 6) : 7);
    END;
  END;

BEGIN
  WITH liste[nr], head DO
    writeln('Protocol: ', protocol, lfcr, 'Producer :', producer,
      '  Date: ', productdate, '  Duration: ', zeit(length),
      ' ms  Rate: ', frequency : 5 : 2, ' Hz');
  wy := wherey + hi(windmin);
  window(1, wy, 40, wy + 9);
  list(0);
  window(41, wy, 80, wy + 9);
  list(8);
  window(1, 1, 80, 25);
  gotoxy(1, wy + 10);
END;

PROCEDURE neufile;
VAR
  ganz : pathstr;
  i :    BYTE;
BEGIN
  showtitle(False, 'Open File', 'Dialogue', farbe3);
  WITH liste[filenr + 1], head DO
  BEGIN
    window(1, 3, 80, 8);
    IF kan <> 0 THEN
      writeln('All files must have identical channels - Only sampling frequencies may vary.', lfcr);
    Name := readstring('File name', '');
    fsplit(Name, namedir, namename, nameext);
    IF nameext = '' THEN nameext := '.DAT';
    Name := namedir + namename + nameext;
    writeln;
    ganz := fexpand(Name);
    FOR i := 1 TO filenr DO IF ganz = fexpand(liste[i].Name) THEN
      BEGIN
        writeln('Identical with no.', i, ' : ', liste[i].Name, '.');
        IF NOT (readcharim('Open anyway? (Y/N) ', 'N') IN ['J', 'j', 'Y', 'y']) THEN
        BEGIN
          Name := leername;
          exit;
        END;
      END;
    kopf(Name, head);
    IF tulabfehler THEN
    BEGIN
      warte;
      Name := leername;
      exit;
    END;
    IF kan = 0 THEN
    BEGIN
      kan := channelnumber;
      neukan(kan);
      kanaele.voreinstellung;
    END;
    IF fre = 0 THEN fre := frequency
    ELSE IF frequency > 1.1 * fre THEN
    BEGIN
      Name := leername;
      fehler('Sampling rate of file too large.');
      warte;
      exit;
    END;
    IF filenr = 0 THEN beschriftungen(head);
    korr   := frequency / fre;
    length := datalength / korr;
    window(1, 8, 80, 25);
    zwischen('Info', farbe3);
    writeln;
    proto(filenr + 1);
    newpointer;
  END;
  Inc(filenr);
  einheitensetzen(fre);
  window(1, 23, 80, 25);
  zwischen('Dialogue', farbe3);
  warte;
END;

PROCEDURE protokoll;
VAR
  i : bigint64;
BEGIN
  clrscr;
  zwischen('Dialogue', farbe3);
  window(1, 18, 80, 25);
  i := readint('Protocol of file no.', 1);
  IF NOT (i IN [1..filenr]) THEN
  BEGIN
    writeln;
    fehler('Undefined file no.');
    warte;
    exit;
  END;
  showtitle(False, 'Protocol', 'Info', farbe3);
  gotoxy(1, 3);
  writeln('File: ', fexpand(liste[i].Name), lfcr);
  proto(i);
  gotoxy(1, 22);
  zwischen('Dialogue', farbe3);
  warte;
END;

PROCEDURE fildir;
VAR
  direc : pathstr;
  such :  searchrec;
  di :    dirstr;
  na :    namestr;
  ex :    extstr;

  PROCEDURE ausgabe(VAR such : searchrec);
  VAR
    d :  dirstr;
    n :  namestr;
    e :  extstr;
    nl : BYTE absolute n;
    el : BYTE absolute e;
  BEGIN
    WITH such DO
    BEGIN
      fsplit(Name, d, n, e);
      Delete(e, 1, 1);
      Write(n, '' : 9 - nl, e, '' : 7 - el);
    END;
  END;

BEGIN
  clrscr;
  zwischen('Dialogue', farbe3);
  window(1, 18, 80, 25);
  direc := fexpand(readstring('Directory path', fexpand('*.dat')));
  fsplit(direc, di, na, ex);
  IF na = '' THEN na := '*';
  IF ex = '' THEN ex := '.dat';
  direc              := di + na + ex;
  findfirst(direc, anyfile, such);
  writeln;
  CASE doserror OF
    0 : BEGIN
      showtitle(False, 'Directory', 'Info', farbe3);
      writeln('Directory of ', direc, lfcr);
      REPEAT
        ausgabe(such);
        findnext(such);
      UNTIL doserror = 18;
      window(1, 3, 80, 20);
      window(1, 23, 80, 25);
      zwischen('Dialogue', farbe3);
    END;
    18 : fehler('No entry under ' + direc + '.');
    ELSE
      fehler('Directory not found.');
  END;
  warte;
END;

PROCEDURE schliessen;
VAR
  nr :   LONGINT;
  ti :   CHAR;
  zwi :  PPDataBlock;
  pzwi : PPBulletList;
BEGIN
  clrscr;
  zwischen('Dialogue', farbe3);
  window(1, 18, 80, 25);
  nr := readint('Close file no.', 0);
  IF NOT (nr IN [1..filenr]) THEN
  BEGIN
    writeln;
    fehler('Undefined file no.');
    warte;
    exit;
  END;
  Dec(filenr);
  IF filenr = 0 THEN
  BEGIN
    fre := 0;
    kan := 0;
  END;
  zwi  := liste[nr].block;
  pzwi := liste[nr].selectedbullet;
  FOR ti := 'A' TO listmax DO tliste[ti]^.fil[nr].frei;
  FOR nr := nr TO filenr DO
  BEGIN
    liste[nr] := liste[nr + 1];
    FOR ti := 'A' TO listmax DO WITH tliste[ti]^ DO fil[nr] := fil[nr + 1];
  END;
  liste[filenr + 1].block  := zwi;
  liste[filenr + 1].selectedbullet := pzwi;
  liste[filenr + 1].Name   := leername;
  liste[filenr + 1].deletepointer;
  FOR ti := 'A' TO listmax DO WITH tliste[ti]^.fil[filenr + 1] DO
    BEGIN
      automn  := 0;
      automda := False;
    END;
END;

PROCEDURE kompression;
VAR
  outname :        string80;
  ganz :           pathstr;
  ko :             headerdata;
  tzaehler, j, i : LONGINT;
  textstr :        STRING;
  nr :             WORD;
  wandert :        PDataBlock;
LABEL
  abbruch;

  PROCEDURE tabelle(von : BYTE);
  VAR
    i : BYTE;
  BEGIN
    IF von <= ko.channelnumber - 1 THEN
      writeln('Channel' : 7, 'Label [Unit]' : 14, 'Scale' : 11);
    FOR i := von TO min(von + 7, ko.channelnumber - 1) DO WITH ko.channels[i] DO
        writeln(i : 5, Name : 16, extewort(factor1 * maxsample, 2, 2) : 12);
  END;

BEGIN
  ko.protocol := readstring('Protocol', liste[1].head.protocol + ' (sel.)');
  showtitle(False, 'Selected Data File', 'Info', farbe3);
  clrscr;
  kanaele.lesen(6, farbe3);
  IF kanaele.kn = 0 THEN exit;
  WITH ko DO
  BEGIN
    producer := 'NEUROLAB';
    frequency      := fre;
    channelnumber      := kanaele.kn;
    i         := 0;
    FOR j := 0 TO maxchannelsandfilters DO IF j IN kanaele.dabei THEN
      BEGIN
        ko.channels[i].Name := schriftliste[j] + ' [' + belegungsliste[j].einhwort + ']';
        IF length(ko.channels[i].Name) > 10 THEN ko.channels[i].Name := schriftliste[j] + '[' + belegungsliste[j].einhwort + ']';
        IF length(ko.channels[i].Name) > 10 THEN ko.channels[i].Name := schriftliste[j];
        ko.channels[i].factor1 := belegungsliste[j].faktor;
        ko.channels[i].factor2 := 1;
        i               := i + 1;
      END;
  END;
  showtitle(False, 'Selected Data File', 'Info', farbe3);
  i := 0;
  REPEAT
    window(1, 3, 38, 12);
    clrscr;
    tabelle(0);
    window(43, 3, 80, 12);
    clrscr;
    tabelle(8);
    window(1, 13, 80, 19);
    zwischen('Menu', farbe3);
    writeln(lfcr, '  w...Create WAV-File', lfcr, '  d...Enter Channel Data', lfcr,
      '  c...Create Turbolab-File', lfcr, '  x...Exit');
    window(1, 20, 80, 25);
    clrscr;
    zwischen('Dialogue', farbe3);
    writeln;
    CASE upcase(readcharim('Menu Point', ' ')) OF
      'D' : BEGIN
        clrscr;
        zwischen('Dialogue', farbe3);
        window(1, 21, 80, 25);
        j := readint('Channel No.', i);
        IF NOT (j IN [0..ko.channelnumber - 1]) THEN
        BEGIN
          writeln;
          fehler('Undefined channel number');
          warte;
        END
        ELSE
          WITH ko.channels[j] DO
          BEGIN
            i       := j;
            Name    := copy(readstring('Label [Basic SI Unit]', Name), 1, 10);
            factor1 := readexte('Maximum on Scale', factor1 * maxsample, 2, 2) / maxsample;
            IF (factor1 < 1e-19) OR (factor1 > 1e19) THEN
            BEGIN
              fehler('Value out of range');
              factor1 := 1;
              warte;
            END;
          END;
      END;
      'W' : BEGIN
        outname := readstring('Output file name', 'select.wav');
        IF fileschonda(outname) THEN
          IF upcase(readchar('Overwrite? (Y/N)', 'N')) <> 'Y' THEN GOTO abbruch;
        ganz := kleinbuchstaben(fexpand(outname));
        FOR i := 1 TO filenr DO IF ganz = kleinbuchstaben(fexpand(liste[i].Name)) THEN
          BEGIN
            fehler('File open (No. ' + wort(i) + ')');
            warte;
            GOTO abbruch;
          END;
        wavpcm.kopfplatzschreiben(outname);
        Assign(seqdaten, outname);
        seqoeffne;
        FOR nr := 1 TO filenr DO WITH liste[nr] DO
          BEGIN
            wandert := block^;
            oeffnen(nr);
            WHILE wandert^.Next <> nil DO
            BEGIN
              FOR tzaehler := trunc(wandert^.frompos + 1) TO trunc(wandert^.endpos) DO
                FOR j := 1 TO kanaele.kn DO seqschreibeint16(dat(zwi(tzaehler), kanaele.k[j]));
              wandert := wandert^.Next;
            END;
            schliesse;
          END;
        seqschliesse;
        wavpcm.kopfschreiben(outname, ko);
        fehler('File ' + outname + ' as WAV-File created');
        warte;
      END;
      'C' : BEGIN
        outname := readstring('Output file name', 'select.dat');
        IF fileschonda(outname) THEN
          IF upcase(readchar('Overwrite? (Y/N)', 'N')) <> 'Y' THEN GOTO abbruch;
        ganz := kleinbuchstaben(fexpand(outname));
        FOR i := 1 TO filenr DO IF ganz = kleinbuchstaben(fexpand(liste[i].Name)) THEN
          BEGIN
            fehler('File open (No. ' + wort(i) + ')');
            warte;
            GOTO abbruch;
          END;
        tulab42.kopfplatzschreiben(outname);
        Assign(seqdaten, outname);
        seqoeffne;
        FOR nr := 1 TO filenr DO WITH liste[nr] DO
          BEGIN
            wandert := block^;
            oeffnen(nr);
            WHILE wandert^.Next <> nil DO
            BEGIN
              FOR tzaehler := trunc(wandert^.frompos + 1) TO trunc(wandert^.endpos) DO
                FOR j := 1 TO kanaele.kn DO seqschreibe(dat(zwi(tzaehler), kanaele.k[j]));
              wandert := wandert^.Next;
            END;
            schliesse;
          END;
        seqschliesse;
        tulab42.kopfschreiben(outname, ko);
        fehler('File ' + outname + ' as Turbolab-File created');
        warte;
      END;
      'X' : BEGIN
        exit;
      END;
    END;
    abbruch :
  UNTIL False;



  IF NOT (upcase(readchar('Start? (Y/N)', 'Y')) IN ['Y', 'J']) THEN
  BEGIN
    fehler('No file created.');
    warte;
    exit;
  END;

END;

PROCEDURE manager;
VAR
  i : BYTE;
BEGIN
  REPEAT
    showtitle(False, 'File Manager', 'Info', farbe2);
    writeln('Open files:');
    window(1, 5, 40, 15);
    FOR i := 1 TO 10 DO writeln(i : 3, ' : ', liste[i].Name);
    window(41, 5, 80, 15);
    FOR i := 11 TO 20 DO writeln(i : 3, ' : ', liste[i].Name);
    window(1, 16, 80, 25);
    zwischen('Menu', farbe2);
    writeln;
    writeln('  d...Directory of Data Files           s...Conditions for File Sets', lfcr,
      '  o...Open File                         c...Close File', lfcr,
      '  p...Protocol of File                  f...Export Selected Data File', lfcr,
      '  m...Main Menu', lfcr);
    zwischen('Dialogue', farbe2);
    writeln;
    IF filenr = 0 THEN
    BEGIN
      CASE upcase(readcharim('Menu Point', 'o')) OF
        'O' : neufile;
        'S' : voreinstellung;
        'D' : fildir;
        'P', 'C', 'F' : BEGIN
          window(1, 16, 80, 25);
          clrscr;
          zwischen('Dialogue', farbe2);
          fehler('No file open: Start with <o> or <s>.');
          writeln;
          pieps;
          warte;
        END;
        'M' : exit;
      END;
    END
    ELSE
    BEGIN
      CASE upcase(readcharim('Menu Point', 'm')) OF
        'D' : fildir;
        'O' : neufile;
        'P' : protokoll;
        'C' : schliessen;
        'S' : voreinstellung;
        'F' : kompression;
        'M' : exit;
      END;
    END;
  UNTIL False;
END;

BEGIN
  FOR filind := 1 TO 20 DO liste[filind].Name := leername;
END.
