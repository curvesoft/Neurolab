{ Borland-Pascal 7.0 / FPC 3.2.2 }
{$ifdef fpc} {$mode TP} {$endif}

UNIT  nlgrafik;

{$IFDEF MSDOS}
{$A+,B-,E+,F-,G-,I-,N+,O-,P+,T+,V-,X-}
{$ELSE}
{$A+,B-,E+,F-,G+,I-,N+,P+,T+,V-,X-}
{$ENDIF}

INTERFACE

USES  crt, graph, daff, wavpcm, tulab42, nlrahmen,
  dos, grafik, tlfilter, nltrigg,
  bequem, plotter, tlfiles;

CONST
  kleinschr       = 'SI0.14,0.22;';
  relativschr     = 'SR1,2.6;';
  fullsamplerange = maxsample - minsample;
  lrand           = 50;

  {$ifdef fpc} maxnumber=(1 shl 27) div sizeof(typedouble) -2; {$else} maxanzahl = (1 SHL 16) DIV sizeof(wert) - 2; {$endif}
  maxbuffer = 6000;

TYPE
  databuffer     = PACKED ARRAY[0..maxnumber] OF typedouble;
  databufferlist = ARRAY[0..maxchannelsandfilters] OF ^databuffer;
  imagesbuffer    = ARRAY[0..maxbuffer + 1] OF EXTENDED;

  TBasicGraphic = OBJECT    {Allgemeine Grafiken}
    filename, fileext, filecomment : string80;
    filenumbers :   BYTE;
    line1, line2 : WORD;
    abortion :      BOOLEAN;
    PROCEDURE calculate; VIRTUAL;
    PROCEDURE image; VIRTUAL;
    PROCEDURE plot(gr : BYTE); VIRTUAL;
    PROCEDURE filewrite(VAR outfile : Text); VIRTUAL;
    PROCEDURE plotstart;
    PROCEDURE construct(l1, l2 : WORD; VAR fina, fike : string80; fian : BYTE);
  END;

  grafikxy = OBJECT(TBasicGraphic) {x-y-Grafik}
    x, y : BYTE;
    PROCEDURE image; VIRTUAL;
    PROCEDURE plot(gr : BYTE); VIRTUAL;
    PROCEDURE filewrite(VAR outfile : Text); VIRTUAL;
    CONSTRUCTOR construct(xk, yk : BYTE);
  END;

  grafikamplitude = OBJECT(TBasicGraphic) {Amplituden-Histogramm}
    diff :             ^imagesbuffer;
    diffn :            WORD;
    klasse :           EXTENDED;
    ybereich :         EXTENDED;
    xkanal :           BYTE;
    minsamp, maxsamp : sample;
    ref :              CHAR;
    PROCEDURE image; VIRTUAL;
    PROCEDURE plot(gr : BYTE); VIRTUAL;
    PROCEDURE filewrite(VAR outfile : Text); VIRTUAL;
    PROCEDURE construct(xk : BYTE; rtl : CHAR; dfn : WORD; ybe : EXTENDED; mins, maxs : sample);
  END;

  yliste = ARRAY[1..maxchannelsandfilters + 1] OF bigint64;

  TChannelGraphic = OBJECT(TBasicGraphic)
    anfang, dauer :     typeextended;
    kanaele :           TChannelVolume;
    stauchung, faktor : EXTENDED;
    breite, oben :      WORD;
    y0 :                yliste;
    PROCEDURE image; VIRTUAL;
    PROCEDURE plot(gr : BYTE); VIRTUAL;
  END;

  grafikkurve = OBJECT(TChannelGraphic)
    FUNCTION daten(stelle : typeextended; kanal : BYTE) : sample; VIRTUAL;
    PROCEDURE image; VIRTUAL;
    PROCEDURE plot(gr : BYTE); VIRTUAL;
  END;

  statustyp = (bleibt, neu);
  menue     = PROCEDURE(aktfile : BYTE);

  TChannelDataGraphic = OBJECT(TChannelGraphic) {Sichten}
    aktfile :          BYTE;
    ablenkung :        SINGLE;
    datanf :           typeextended;
    strichstelle, strichst1 : typeextended;
    strich, strich1 :  bigint64;
    status :           statustyp;
    spalte1, spalte2 : WORD;
    spannstatus, strich1da : BOOLEAN;
    zei1a, zei2a, zei3a, zei4a : string20;
    FUNCTION stellex(stelle : typeextended) : bigint64;
    FUNCTION xstelle(x : bigint64) : typeextended;
    PROCEDURE image; VIRTUAL;
    PROCEDURE linie(x1, x2 : bigint64);
    PROCEDURE plot(gr : BYTE); VIRTUAL;
    CONSTRUCTOR construct(afi : BYTE; VAR kane : TChannelVolume; anf : typeextended; abl : SINGLE; men : menue);
  END;

  grafiksuperposition = OBJECT(TChannelGraphic) {Superpositionsgrafik}
    tl :           CHAR;
    beginn, ende : typeextended;
    PROCEDURE image; VIRTUAL;
    PROCEDURE plot(gr : BYTE); VIRTUAL;
    CONSTRUCTOR construct(VAR kan : TChannelVolume; von, bis : typeextended; trl : CHAR);
  END;

  grafikmittel = OBJECT(grafikkurve) {Grafik vom Averagen}
    mittel :   databufferlist;
    tpgesamt : bigint64;
    tl :       CHAR;
    rdauer :   bigint64;
    FUNCTION daten(stelle : typeextended; kanal : BYTE) : sample; VIRTUAL;
    PROCEDURE image; VIRTUAL;
    PROCEDURE plot(gr : BYTE); VIRTUAL;
    PROCEDURE filewrite(VAR outfile : Text); VIRTUAL;
    PROCEDURE construct(VAR kan : TChannelVolume; anf, dau : typeextended; trl : CHAR; trp : bigint64);
  END;

  grafikintervallroh = OBJECT(TBasicGraphic) {Intervalldaten-Grafik}
    diff :     ^imagesbuffer;
    diffn :    WORD;
    ref :      CHAR;
    ybereich : EXTENDED;
    PROCEDURE image; VIRTUAL;
    PROCEDURE plot(gr : BYTE); VIRTUAL;
    PROCEDURE construct(rtl : CHAR; dfn : WORD; ybe : EXTENDED);
  END;

  grafikintervall = OBJECT(grafikintervallroh) {... mit Zeitachse}
    anfang, schluss, dauer : typeextended;
    klasse : EXTENDED;
    PROCEDURE image; VIRTUAL;
    PROCEDURE plot(gr : BYTE); VIRTUAL;
    PROCEDURE filewrite(VAR outfile : Text); VIRTUAL;
    PROCEDURE construct(rtl : CHAR; anf, sch : typeextended; dfn : WORD; ybe : EXTENDED);
  END;

  grafikphasenintervall = OBJECT(grafikintervallroh) {... mit Phasenachse}
    anfphase, dauerphase : EXTENDED;
    klasse :               EXTENDED;
    diffnganz :            bigint64;
    PROCEDURE image; VIRTUAL;
    PROCEDURE plot(gr : BYTE); VIRTUAL;
    PROCEDURE filewrite(VAR outfile : Text); VIRTUAL;
    PROCEDURE construct(rtl : CHAR; anfph, dauph : EXTENDED; dfn : WORD; ybe : EXTENDED);
  END;

CONST
  statustext : ARRAY[statustyp] OF string20 = (' -', 'Block selection');

FUNCTION kon(hoehe : typedouble; k : BYTE) : typedouble;
FUNCTION rekon(hoehe : typedouble; k : BYTE) : typedouble;

IMPLEMENTATION

TYPE
  skalafeld = ARRAY[1..10] OF typeextended;

  punkte = RECORD
    p1x, p1y, p2x, p2y : WORD
  END;

CONST
  tstellenmuster : SET OF 0..15 = [0, 4, 8, 12];
  ende : BOOLEAN                = False;

  p : ARRAY[1..5] OF punkte = ((p1x : 3000; p1y : 3000; p2x : 6000; p2y : 5000),
    (p1x : 2600; p1y : 2600; p2x : 6600; p2y : 5200),
    (p1x : 2300; p1y : 2300; p2x : 7530; p2y : 5300),
    (p1x : 2000; p1y : 2000; p2x : 8660; p2y : 6000),
    (p1x : 1150; p1y : 380; p2x : 9750; p2y : 6380));

VAR
  tstellenpattern :        WORD absolute tstellenmuster;
  strichfarbe, bildfarbe : LONGINT;

FUNCTION kon(hoehe : typedouble; k : BYTE) : typedouble;
BEGIN
  IF belegungsliste[k].negativ THEN kon := hoehe
  ELSE
    kon := hoehe * 2 - sampleoffset;
END;

FUNCTION rekon(hoehe : typedouble; k : BYTE) : typedouble;
BEGIN
  IF belegungsliste[k].negativ THEN rekon := hoehe
  ELSE
    rekon := (hoehe + sampleoffset) / 2;
END;

{ TBasicGraphic }

PROCEDURE TBasicGraphic.image;
BEGIN
  setcolor(min(bildfarbe, getmaxcolor));
  settextjustify(lefttext, centertext);
  IF filenumbers > 0 THEN
    outtextxy(0, line1, 'File:' + filename + ' (total:' + wort(filenumbers) + ')');
  outtextxy(0, line2, fileext);
  settextjustify(centertext, centertext);
END;

PROCEDURE TBasicGraphic.plot(gr : BYTE);
BEGIN
END;

PROCEDURE TBasicGraphic.filewrite(VAR outfile : Text);
BEGIN
  fehler('Sorry, no file output available.');
  warte;
END;

PROCEDURE TBasicGraphic.calculate;
BEGIN
END;

PROCEDURE TBasicGraphic.plotstart;
CONST
  g : INTEGER = 4;
VAR
  bildzeigen : BOOLEAN;
BEGIN
  closegraph;
  showtitle(False, 'Graphic Output', 'Info', farbe3);
  writeln('Output format: 0 = ASCII-List', lfcr,
    '               1 = HP-GL (Plotter)', lfcr,
    '               2 = PCL 5 (HP-LJ III)');
  writeln('Output size  : 1..5');
  writeln('Device / file: PRN, LPT1, LPT2, COM1, COM2, NUL, <filename>');
  writeln;
  zwischen('Dialogue', farbe3);
  BYTE(plotformat) :=
    zahl(readchar('Format               ', chr(Ord(plotformat) + Ord('0'))));
  IF plotformat <> ascii THEN g := readint('Size                 ', g);
  REPEAT
    REPEAT
      assignplt(readstring('Output device / file ', plotterdevice));
    UNTIL devicetest(plotterdevice) OR NOT fileschonda(plotterdevice) OR
      (upcase(readchar('Overwrite? (Y/N)     ', 'N')) = 'Y');
    rewrite(plt);
  UNTIL NOT lesefehler;
  writeln;
  ende        := upcase(readchar('Return to menue after plot? (Y/N)', 'Y')) = 'Y';
  filecomment := readstring('Plot comment', '');
  IF upcase(readchar('Start plot? (Y/N)', 'Y')) = 'Y' THEN
  BEGIN
    writeln(lfcr, '>> Abort: Escape <<');
    CASE plotformat OF
      hpgl, pcl5 : IF g IN [1..5] THEN
        BEGIN
          plotterin;
          plot(g);
          plotterunin;
        END;
      ascii : BEGIN
        filewrite(plt);
        Close(plt);
      END;
    END;
  END;
  opengraph;
  IF NOT ende THEN image;
END;

PROCEDURE TBasicGraphic.construct(l1, l2 : WORD; VAR fina, fike : string80; fian : BYTE);
VAR
  taste : CHAR;
BEGIN
  filename   := fina;
  fileext    := fike;
  filenumbers := fian;
  line1      := l1;
  line2      := l2;
  abortion    := False;
  calculate;
  IF abortion THEN exit;
  opengraph;
  cleardevice;
  graphdefaults;
  image;
  piep;
  REPEAT
    WHILE keypressed DO taste := readkey;
    CASE readkey OF
      #0 : CASE readkey OF
          #114 : plotstart;
        END;
      'P' : plotstart;
      #27 : BEGIN
        closegraph;
        exit;
      END;
    END;
  UNTIL ende;
  closegraph;
  ende := False;
END;

FUNCTION buendig(schrift : string20) : string20;
CONST
  leer : string20 = '     ';
VAR
  n : BYTE absolute leer;
BEGIN
  IF length(schrift) < 5 THEN n := 5 - length(schrift)
  ELSE
    n := 0;
  buendig := leer + schrift;
END;

TYPE
  skalasorte = (zeitskala, phasenskala);

PROCEDURE skala(CONST anfang, dauer : EXTENDED; VAR anz : BYTE; VAR werte : skalafeld; sorte : skalasorte);
CONST
  anteil = 0.85;
VAR
  n, i, step : BYTE;
  n10, zeitpa, zeitpe : bigint64;
  fre : EXTENDED;
BEGIN
  CASE sorte OF
    zeitskala : fre   := tlfiles.fre;
    phasenskala : fre := 1;
  END;
  n      := max(trunc(log(anteil * dauer / fre * 1000)) - 1, 0);
  n10    := pot(n);
  zeitpa := round(anfang / fre * 1000);
  IF (zeitpa MOD n10 <> 0) AND (zeitpa > 0) THEN zeitpa := zeitpa DIV n10 + 1
  ELSE
    zeitpa := zeitpa DIV n10;
  zeitpe := round((anfang + anteil * dauer) / fre * 1000) DIV n10;
  CASE zeitpe - zeitpa + 1 OF
    0 : BEGIN
      anz := 0;
      exit;
    END;
    1..6 : step    := 1;
    7..14 : step   := 2;
    15..30 : step  := 5;
    31..60 : step  := 10;
    61..100 : step := 20;
    ELSE
      step := 40;
  END;
  IF (zeitpa MOD step <> 0) AND (zeitpa > 0) THEN zeitpa := (zeitpa DIV step + 1) * step
  ELSE
    zeitpa := (zeitpa DIV step) * step;
  IF (zeitpe MOD step <> 0) AND (zeitpe < 0) THEN zeitpe := (zeitpe DIV step + 1) * step
  ELSE
    zeitpe := (zeitpe DIV step) * step;
  anz := (zeitpe - zeitpa) DIV step + 1;
  FOR i := 1 TO anz DO werte[i] := (zeitpa + (i - 1) * step) * n10 * fre / 1000;
END;

{ grafikxy }

CONSTRUCTOR grafikxy.construct(xk, yk : BYTE);
CONST
  leer : STRING[1] = '';
BEGIN
  x := xk;
  y := yk;
  TBasicGraphic.construct(14, 24, leer, liste[filenr].head.protocol, 0);
END;

PROCEDURE grafikxy.image;
CONST
  urand = 26;
  orand = 32;
VAR
  m, xz, tzaehler : LONGINT;
  nr, znr, x0, y0, xp, yp : WORD;
  faktorx, faktory : EXTENDED;
  wandert : PDataBlock;
  td :      triggerdaten;
  xyl :     BYTE;
BEGIN
  INHERITED image;
  faktory := (getmaxy - urand - orand) / maxsample / 2;
  y0      := getmaxy - urand - (getmaxy - urand - orand) DIV 2;
  faktorx := (getmaxx - lrand) / maxsample / 2;
  x0      := lrand + (getmaxx - lrand) DIV 2;
  setcolor(min(bildfarbe, getmaxcolor));
  setwritemode(copyput);
  setlinestyle(solidln, 0, normwidth);
  line(lrand, orand, lrand, getmaxy - urand);
  line(lrand, getmaxy - urand, getmaxx, getmaxy - urand);
  WITH belegungsliste[y] DO
  BEGIN
    setlinestyle(solidln, 0, normwidth);
    FOR xz := 0 TO 2 DO
    BEGIN
      moveto(lrand, getmaxy - urand - round(xz / 2 * (getmaxy - urand - orand)));
      linerel(4, 0);
    END;
    setlinestyle(dashedln, 0, normwidth);
    moveto(lrand, getmaxy - urand - (getmaxy - urand - orand) DIV 2);
    linerel(getmaxx - lrand, 0);
    settextstyle(defaultfont, vertdir, 1);
    settextjustify(centertext, centertext);
    outtextxy(4, getmaxy - urand - (getmaxy - urand - orand) DIV 2, schriftliste[y]);
    settextstyle(defaultfont, horizdir, 1);
    settextjustify(lefttext, toptext);
    outtextxy(14, orand + 22, einhwort);
    settextjustify(righttext, toptext);
    outtextxy(lrand - 1, orand + 1, wort(spannung(rekon(maxsample, y), y)));
    settextjustify(righttext, bottomtext);
    outtextxy(lrand - 1, getmaxy - urand, wort(spannung(rekon(minsample, y), y)));
    settextjustify(righttext, centertext);
    outtextxy(lrand - 1, y0, wort(spannung(rekon(0, y), y)));
  END;
  WITH belegungsliste[x] DO
  BEGIN
    setlinestyle(solidln, 0, normwidth);
    FOR xz := 0 TO 2 DO
    BEGIN
      moveto(lrand + round(xz / 2 * (getmaxx - lrand)), getmaxy - urand);
      linerel(0, -4);
    END;
    setlinestyle(dashedln, 0, normwidth);
    moveto(lrand + (getmaxx - lrand) DIV 2, getmaxy - urand);
    linerel(0, -getmaxy + urand + orand);
    settextstyle(defaultfont, horizdir, 1);
    settextjustify(centertext, centertext);
    outtextxy(lrand + (getmaxx - lrand) DIV 2, getmaxy - 4, schriftliste[x]);
    settextjustify(centertext, bottomtext);
    outtextxy(lrand + 80, getmaxy - 12, einhwort);
    settextjustify(righttext, centertext);
    outtextxy(getmaxx - 1, getmaxy - urand + 6, wort(spannung(rekon(maxsample, x), x)));
    settextjustify(lefttext, centertext);
    outtextxy(lrand + 1, getmaxy - urand + 6, wort(spannung(rekon(minsample, x), x)));
    settextjustify(centertext, centertext);
    outtextxy(x0, getmaxy - urand + 6, wort(spannung(rekon(0, x), x)));
  END;
  setlinestyle(solidln, 0, normwidth);
  setcolor(getmaxcolor);
  settextjustify(lefttext, centertext);
  outtextxy(0, 4, '"X-Y-DIAGRAM"');
  znr := 4;
  FOR nr := 1 TO filenr DO WITH liste[nr] DO
      IF NOT (belegungsliste[x].gepunktet OR belegungsliste[y].gepunktet) THEN
      BEGIN
        wandert := block^;
        openfile(nr);
        WHILE wandert^.Next <> nil DO
        BEGIN
          IF znr < orand THEN
          BEGIN
            outtextxy(getmaxx DIV 2, znr, namename + nameext + ': ' + wort(zeit(wandert^.frompos)) +
              ' ms  - ' + wort(zeit(wandert^.endpos)) + ' ms');
            Inc(znr, 10);
          END
          ELSE
            outtextxy(getmaxx - 25, ((orand - 4) DIV 10) * 10 + 4, '...');
          moveto(x0 + round(kon(dat(zwi(trunc(wandert^.frompos + 1)), x), x) * faktorx),
            y0 - round(kon(dat(zwi(trunc(wandert^.frompos + 1)), y), y) * faktory));
          FOR tzaehler := trunc(wandert^.frompos + 1) + 1 TO trunc(wandert^.endpos) DO
            lineto(x0 + round(kon(dat(zwi(tzaehler), x), x) * faktorx),
              y0 - round(kon(dat(zwi(tzaehler), y), y) * faktory));
          wandert := wandert^.Next;
        END;
        schliesse;
      END
      ELSE
      BEGIN
        IF belegungsliste[x].gepunktet AND belegungsliste[y].gepunktet THEN exit;
        IF belegungsliste[x].gepunktet THEN xyl := x
        ELSE
          xyl := y;
        openfile(nr);
        td := tliste[belegungsliste[xyl].gepunktettl]^.fil[nr];
        FOR tzaehler := 1 TO td.automn DO
        BEGIN
          xp := x0 + round(kon(dat(zwi(td.autom^[tzaehler]), x), x) * faktorx);
          yp := y0 - round(kon(dat(zwi(td.autom^[tzaehler]), y), y) * faktory);
          FOR m := -1 TO 1 DO
          BEGIN
            putpixel(xp + m, yp + m, getmaxcolor);
            putpixel(xp + m, yp - m, getmaxcolor);
          END;
        END;
        schliesse;
      END;
END;

PROCEDURE grafikxy.plot(gr : BYTE);
CONST
  lrand  = minsample - fullsamplerange / 12;
  bnrmax = 3;
VAR
  tzaehler, j, bnr : LONGINT;
  nr :               WORD;
  wandert :          PDataBlock;
  xyl :              BYTE;
  td :               triggerdaten;
BEGIN
  WITH p[gr] DO Write(plt, plip(p1x, p1x, p2y, p2y));
  Write(plt, relativschr, plsc(minsample, maxsample, minsample, maxsample));
  FOR nr := 1 TO filenr DO WITH liste[nr] DO
    BEGIN
      IF NOT (belegungsliste[x].gepunktet OR belegungsliste[y].gepunktet) THEN
      BEGIN
        wandert := block^;
        openfile(nr);
        WHILE wandert^.Next <> nil DO
        BEGIN
          Write(plt, plpa(kon(dat(zwi(trunc(wandert^.frompos + 1)), x), x), kon(
            dat(zwi(trunc(wandert^.frompos + 1)), y), y)), plpd);
          FOR tzaehler := trunc(wandert^.frompos + 1) + 1 TO trunc(wandert^.endpos) DO
            Write(plt, plpa(kon(dat(zwi(tzaehler), x), x), kon(dat(zwi(tzaehler), y), y)));
          Write(plt, plpu);
          wandert := wandert^.Next;
        END;
        schliesse;
      END
      ELSE
      BEGIN
        IF belegungsliste[x].gepunktet AND belegungsliste[y].gepunktet THEN exit;
        IF belegungsliste[x].gepunktet THEN xyl := x
        ELSE
          xyl := y;
        openfile(nr);
        td := tliste[belegungsliste[xyl].gepunktettl]^.fil[nr];
        FOR tzaehler := 1 TO td.automn DO
          Write(plt, plpa(kon(dat(zwi(td.autom^[tzaehler]), x), x), kon(dat(zwi(td.autom^[tzaehler]), y), y)), plkr);
        schliesse;
      END;
    END;
  Write(plt, plpu, plpa(minsample, minsample), plpd, plpa(maxsample, minsample),
    plpa(maxsample, maxsample), plpa(minsample, maxsample),
    plpa(minsample, minsample), plpu);
  Write(plt, plpa(lrand, maxsample), 'CP0,-0.75;',
    pllb(buendig(wort(spannung(rekon(maxsample, y), y)))),
    'CP;CP0.5,-0.25;', belegungsliste[y].plot);
  Write(plt, plpa(lrand, 0), 'CP0,-0.25;',
    pllb(buendig(wort(spannung(rekon(0, y), y)))),
    plpa(lrand, minsample), 'CP0,0.25;',
    pllb(buendig(wort(spannung(rekon(minsample, y), y)))));
  Write(plt, 'TL0,1.3;', plpa(minsample, minsample), plyt, plxt);
  Write(plt, 'TL0,0.6;');
  FOR j := 1 TO 3 DO Write(plt, plpa(minsample, minsample + j * fullsamplerange / 4), plyt);
  Write(plt, 'TL0,1.3;', plpa(minsample, maxsample), plyt);
  Write(plt, kleinschr, 'CP0,3;', pllb(filename), 'CP;', pllb(fileext), 'CP;',
    pllb(filecomment), relativschr);
  Write(plt, 'TL0,0.6;');
  FOR j := 1 TO 3 DO Write(plt, plpa(minsample + j * fullsamplerange / 4, minsample), plxt);
  Write(plt, 'TL0,1.3;', plpa(maxsample, minsample), plxt);
  Write(plt, 'DI0,1;', plpa(lrand, minsample), 'CP0,0.5;', pllb(schriftliste[y]));
  Write(plt, 'DI1,0;');
  Write(plt, plpa(maxsample, minsample), 'CP-4.33,-1;',
    pllb(buendig(wort(spannung(rekon(maxsample, x), x)))),
    'CP-14,-0.5;', belegungsliste[x].plot);
  Write(plt, plpa(0, minsample), 'CP-4.33,-1;',
    pllb(buendig(wort(spannung(rekon(0, x), x)))),
    plpa(minsample, minsample), 'CP-4.33,-1;',
    pllb(buendig(wort(spannung(rekon(minsample, x), x)))));
  Write(plt, plpa(minsample, minsample), 'CP0,-2;', pllb(schriftliste[x]));
  Write(plt, kleinschr, 'DI0,1;', plpa(maxsample, minsample), 'CP0,-2;');
  bnr := 0;
  FOR nr := 1 TO filenr DO WITH liste[nr] DO
    BEGIN
      wandert := block^;
      WHILE wandert^.Next <> nil DO
      BEGIN
        IF bnr < bnrmax THEN
        BEGIN
          Write(plt, pllb(namename + nameext + ': ' + wort(zeit(wandert^.frompos)) + ' ms  - ' +
            wort(zeit(wandert^.endpos)) + ' ms'), 'CP;');
          Inc(bnr);
        END;
        wandert := wandert^.Next;
      END;
    END;
END;

PROCEDURE grafikxy.filewrite(VAR outfile : Text);
VAR
  nr :       BYTE;
  tzaehler : LONGINT;
  wandert :  PDataBlock;
BEGIN
  FOR nr := 1 TO filenr DO WITH liste[nr] DO
    BEGIN
      wandert := block^;
      openfile(nr);
      WHILE wandert^.Next <> nil DO
      BEGIN
        FOR tzaehler := trunc(wandert^.frompos + 1) TO trunc(wandert^.endpos) DO
          writeln(outfile, extspannung(dat(zwi(tzaehler), x), x) : 15 : 3,
            extspannung(dat(zwi(tzaehler), y), y) : 15 : 3);
        wandert := wandert^.Next;
        writeln(outfile);
      END;
      schliesse;
    END;
END;

{ bildbalken }

PROCEDURE bildbalken(CONST lrand : bigint64; VAR xbreite : EXTENDED; CONST klasse : EXTENDED;
  VAR breite, diffn : WORD);
BEGIN
  breite := (getmaxx - lrand - 2) DIV diffn;
  IF breite = 0 THEN
  BEGIN
    breite  := 1;
    diffn   := getmaxx - lrand - 2;
    xbreite := klasse * diffn;
  END;
END;

{ grafikamplitude }

PROCEDURE grafikamplitude.construct(xk : BYTE; rtl : CHAR; dfn : WORD; ybe : EXTENDED; mins, maxs : sample);
BEGIN
  xkanal   := xk;
  ref      := rtl;
  ybereich := ybe;
  diffn    := dfn;
  minsamp  := mins;
  maxsamp  := maxs;
  klasse   := (maxs - mins) / diffn;
  new(diff);
  WITH tliste[ref]^, liste[erstfile] DO
    TBasicGraphic.construct(24, 34, Name, head.protocol, fileanz);
  dispose(diff);
END;

PROCEDURE grafikamplitude.image;
CONST
  lrand = 32;
  urand = 26;
  orand = 40;
VAR
  enn, breite : WORD;
  xbreite :     EXTENDED;
  i, j, xa :    LONGINT;
  faktor :      EXTENDED;
  einheit :     einheitstring;
BEGIN
  INHERITED image;
  enn     := diffn;
  xbreite := maxsample - minsample;
  einheit := belegungsliste[xkanal].einhwort;
  bildbalken(lrand, xbreite, klasse, breite, enn);
  setcolor(min(bildfarbe, getmaxcolor));
  settextjustify(centertext, centertext);
  setwritemode(copyput);
  setlinestyle(solidln, 0, normwidth);
  line(lrand + 2, orand, lrand + 2, getmaxy - urand);
  line(lrand - 3, getmaxy - urand, lrand + 1 + enn * breite, getmaxy - urand);
  settextjustify(righttext, centertext);
  outtextxy(lrand - 4, 65, 'n');
  outtextxy(lrand, 76, 'bin');
  line(8, 70, lrand, 70);
  outtextxy(lrand, 47, extwort(ybereich, 4, 2));
  line(lrand + 1, orand, lrand - 3, orand);
  outtextxy(lrand, getmaxy - urand - 5, '0');
  setlinestyle(solidln, 0, normwidth);
  FOR xa := 0 TO 2 DO
  BEGIN
    moveto(lrand + 2 + round(xa / 2 * breite * enn), getmaxy - urand);
    linerel(0, -4);
  END;
  setlinestyle(dashedln, 0, normwidth);
  IF (minsamp < 0) AND (maxsamp > 0) THEN
  BEGIN
    IF frac(-minsamp / klasse) < 1e-32 THEN
      moveto(lrand + 1 + trunc(-minsamp / klasse) * breite, getmaxy - urand)
    ELSE
      moveto(lrand + 1 + trunc(-minsamp / klasse) * breite + breite DIV 2, getmaxy - urand);
    linerel(0, -getmaxy + urand + orand);
  END;
  setlinestyle(solidln, 0, normwidth);
  settextstyle(defaultfont, horizdir, 1);
  settextjustify(centertext, centertext);
  outtextxy(lrand + 2 + breite * enn DIV 2, getmaxy - 4, schriftliste[xkanal]);
  settextjustify(centertext, bottomtext);
  outtextxy(lrand + 80, getmaxy - 12, einheit);
  settextjustify(righttext, centertext);
  outtextxy(lrand + 2 + breite * enn, getmaxy - urand + 6,
    wort(spannung(rekon(maxsamp, xkanal), xkanal)));
  settextjustify(lefttext, centertext);
  outtextxy(lrand + 3, getmaxy - urand + 6, wort(spannung(rekon(minsamp, xkanal), xkanal)));
  settextjustify(centertext, centertext);
  outtextxy(lrand + 2 + breite * enn DIV 2, getmaxy - urand + 6,
    wort(spannung(rekon((maxsamp + minsamp) / 2, xkanal), xkanal)));
  setcolor(getmaxcolor);
  faktor := -(getmaxy - urand - orand) / ybereich;
  FOR i := 1 TO enn DO IF diff^[i] > 0 THEN FOR j := 1 TO breite DO
      BEGIN
        xa := lrand + 1 + (i - 1) * breite + j;
        line(xa, getmaxy - urand - 1, xa, getmaxy - urand - 1 + round(diff^[i] * faktor));
      END;
  settextjustify(lefttext, centertext);
  outtextxy(0, 14, 'Bins þ Width: ' + wort(enn) + ' þ ' + extwort(extspannung(klasse, xkanal), 3, 1) +
    ' ' + einheit);
END;

PROCEDURE grafikamplitude.plot(gr : BYTE);
VAR
  i : WORD;
BEGIN
  WITH p[gr] DO Write(plt, plip(p1x, p1y, p2x, p2y));
  Write(plt, plsc(0, diffn, 0, ybereich));
  Write(plt, plpa(0, diff^[1]), plpd);
  FOR i := 1 TO diffn - 1 DO Write(plt, plpa(i, diff^[i]), plpa(i, diff^[i + 1]));
  Write(plt, plpa(diffn, diff^[diffn]), plpa(diffn, 0), plpu);
  Write(plt, relativschr, plpa(0, ybereich), 'CP-5.5,-0.75;',
    pllb(buendig(extwort(ybereich, 5, 3))));
  Write(plt, plpa(0, ybereich), kleinschr, 'CP0,3;', pllb(filename), 'CP;', pllb(fileext), 'CP;',
    pllb(filecomment), relativschr);
  Write(plt, plpa(0, ybereich), 'TL0,1.3', plyt, plpd, plpa(0, 0), plyt,
    plpd, plpa(diffn, 0), plpu);
  Write(plt, plpa(0, 0), plxt, 'CP-4.33,-1',
    pllb(buendig(wort(spannung(rekon(minsamp, xkanal), xkanal)))));
  Write(plt, plpa(diffn / 2, 0), plxt, 'CP-4.33,-1',
    pllb(buendig(wort(spannung(rekon((maxsamp + minsamp) / 2, xkanal), xkanal)))));
  Write(plt, plpa(diffn, 0), plxt, 'CP-4.33,-1',
    pllb(buendig(wort(spannung(rekon(maxsamp, xkanal), xkanal)))));
  Write(plt, plpa(0, 0), 'CP-1.5,0.25;', pllb('0'));
  Write(plt, plpa(0, 0), 'DI0,1CP0,2.5;', pllb('Occurrences [n/bin]'));
  Write(plt, plpa(0, ybereich / 2), 'TL0,0.6;', plyt);
  Write(plt, kleinschr, plpa(diffn, 0), 'DI0,1;CP0,-3;');
  Write(plt, pllb(' Bins          :' + wort(diffn) + ' (' + extwort(extspannung(klasse, xkanal), 1, 1) + ' '),
    belegungsliste[xkanal].plot, pllb(')'));
END;

PROCEDURE grafikamplitude.filewrite(VAR outfile : Text);
VAR
  i :      LONGINT;
  faktor : EXTENDED;
BEGIN
  faktor := (maxsamp - minsamp) / diffn;
  FOR i := 1 TO diffn DO writeln(outfile,
      extspannung(rekon(minsamp + (i - 0.5) * faktor, xkanal), xkanal) : 12 : 3, diff^[i] : 15 : 9);
END;

{ TChannelGraphic }

PROCEDURE TChannelGraphic.image;
VAR
  i, x, buch, yn : LONGINT;
  anzahl :         BYTE;
  werte :          skalafeld;
  y :              yliste;
  maxx, maxy :     bigint64;
BEGIN
  maxx := getmaxx;
  maxy := getmaxy;
  setcolor(min(bildfarbe, getmaxcolor));
  WITH kanaele DO
  BEGIN
    setwritemode(copyput);
    setlinestyle(solidln, 0, normwidth);
    stauchung := dauer / (maxx - lrand);
    breite    := (maxy - 40) DIV (2 * channelnumber);
    oben      := maxy - 2 * channelnumber * breite - 10;
    faktor    := -breite / maxsample;
    settextjustify(righttext, centertext);
    outtextxy(maxx, maxy - 4, 't [ms]');
    line(lrand, oben, lrand, maxy - 10);
    line(lrand, maxy - 10, maxx, maxy - 10);
    buch := breite DIV 4;
    FOR i := 1 TO channelnumber DO WITH belegungsliste[k[i]] DO
      BEGIN
        y0[i] := round(oben + (2 * (i - 1) + 1) * breite);
        setlinestyle(solidln, 0, normwidth);
        FOR x := 1 TO 3 DO
        BEGIN
          moveto(lrand, y0[i] - round((x / 2 - 1) * breite));
          linerel(4, 0);
        END;
        setlinestyle(dashedln, 0, normwidth);
        moveto(lrand, y0[i] - breite);
        linerel(maxx - 10, 0);
        IF channelnumber <= 10 THEN
        BEGIN
          settextstyle(defaultfont, vertdir, 1);
          settextjustify(centertext, centertext);
          outtextxy(4, y0[i], copy(schriftliste[k[i]], 1, buch));
          settextstyle(defaultfont, horizdir, 1);
          settextjustify(lefttext, toptext);
          outtextxy(14, y0[i] - breite + 12, einhwort);
          settextjustify(righttext, toptext);
          outtextxy(lrand - 1, y0[i] - breite + 1, wort(spannung(rekon(maxsample, k[i]), k[i])));
          settextjustify(righttext, bottomtext);
          outtextxy(lrand - 1, y0[i] + breite, wort(spannung(rekon(minsample, k[i]), k[i])));
          IF channelnumber <= 6 THEN
          BEGIN
            settextjustify(righttext, centertext);
            outtextxy(lrand - 1, y0[i], wort(spannung(rekon(0, k[i]), k[i])));
          END;
        END
        ELSE
        BEGIN
          settextjustify(righttext, centertext);
          outtextxy(lrand - 1, y0[i], copy(schriftliste[k[i]], 1, 6));
        END;
      END;
    settextstyle(defaultfont, horizdir, 1);
    settextjustify(centertext, centertext);
    skala(anfang, dauer, anzahl, werte, zeitskala);
    FOR i := 1 TO anzahl DO
    BEGIN
      x := round((werte[i] - anfang) / stauchung + lrand + 1);
      line(x, maxy - 10, x, maxy - 14);
      outtextxy(x, maxy - 4, wort(zeit(werte[i])));
    END;
  END;
END;

PROCEDURE TChannelGraphic.plot(gr : BYTE);
VAR
  i, j :      LONGINT;
  anzahl :    BYTE;
  werte :     skalafeld;
  rdauer :    bigint64;
  lrand :     EXTENDED;
  fleinheit : einheittyp;
BEGIN
  WITH kanaele DO
  BEGIN
    rdauer := round(dauer);
    lrand  := -rdauer / 12;
    WITH p[gr] DO Write(plt, plip(p1x, p1y, p2x, p2y));
    Write(plt, relativschr, plsc(0, rdauer, 0, fullsamplerange * channelnumber));
    FOR i := 1 TO channelnumber DO
    BEGIN
      Write(plt, plpa(lrand, fullsamplerange * (channelnumber - i + 1)), 'CP0,-0.75;',
        pllb(buendig(wort(spannung(rekon(maxsample, k[i]), k[i])))),
        'CP;CP0.5,-0.25;', belegungsliste[k[i]].plot);
      Write(plt, plpa(lrand, fullsamplerange * (channelnumber - i) + fullsamplerange / 2), 'CP0,-0.25;',
        pllb(buendig(wort(spannung(rekon(0, k[i]), k[i])))),
        plpa(lrand, fullsamplerange * (channelnumber - i)), 'CP0,0.25;',
        pllb(buendig(wort(spannung(rekon(minsample, k[i]), k[i])))));
    END;
    Write(plt, 'TL0,1.3;', plpa(0, 0), plyt);
    FOR i := channelnumber DOWNTO 1 DO
    BEGIN
      Write(plt, 'TL0,0.6;');
      FOR j := 1 TO 3 DO Write(plt, plpa(0, fullsamplerange * (channelnumber - i) + j * fullsamplerange / 4), plyt);
      Write(plt, 'TL0,1.3;', plpa(0, fullsamplerange * (channelnumber - i + 1)), plyt);
    END;
    Write(plt, kleinschr, 'CP0,3;', pllb(filename), 'CP;', pllb(fileext), 'CP;',
      pllb(filecomment), relativschr);
    Write(plt, plpu, plpa(0, fullsamplerange * channelnumber), plpd, plpa(0, 0), plpa(rdauer, 0), plpu,
      'CP-6,-1;', pllb('t [ms]'), 'TL-1,0;');
    skala(anfang, rdauer, anzahl, werte, zeitskala);
    FOR i := anzahl DOWNTO 1 DO
      Write(plt, plpa((werte[i] - anfang), 0), plxt, 'CP-.33,-1;',
        pllb(wort(zeit(werte[i]))));
    Write(plt, 'DI0,1;');
    FOR i := channelnumber DOWNTO 1 DO Write(plt, plpu, plpa(lrand, fullsamplerange * (channelnumber - i)),
        'CP0,0.5;', pllb(schriftliste[k[i]]));
  END;
END;

{ grafikkurve }

FUNCTION grafikkurve.daten(stelle : typeextended; kanal : BYTE) : sample;
BEGIN
END;

PROCEDURE grafikkurve.image;
VAR
  i, x, yn : LONGINT;
  y :        yliste;
BEGIN
  TChannelGraphic.image;
  WITH kanaele DO
  BEGIN
    setwritemode(copyput);
    setlinestyle(solidln, 0, normwidth);
    setcolor(getmaxcolor);
    FOR i := 1 TO channelnumber DO
      y[i] := y0[i] + round(kon(daten(0, k[i]), k[i]) * faktor);
    FOR x := lrand + 1 TO getmaxx - 1 DO
      FOR i := 1 TO channelnumber DO
      BEGIN
        yn := y0[i] + round(kon(daten((x - lrand - 1) * stauchung, k[i]), k[i]) * faktor);
        line(x, y[i], x + 1, yn);
        y[i] := yn;
      END;
  END;
END;

PROCEDURE grafikkurve.plot(gr : BYTE);
VAR
  i, j :   LONGINT;
  rdauer : bigint64;
BEGIN
  TChannelGraphic.plot(gr);
  rdauer := round(dauer);
  Write(plt, kleinschr, 'DI1,0;', plpa(rdauer, fullsamplerange * kanaele.channelnumber), 'CP-6,2;');
  Write(plt, pllb('Start: ' + extwort(extzeit(anfang), 1, 3) + ' ms'), 'CP;CP-6,0;');
  Write(plt, pllb('End  : ' + extwort(extzeit(anfang + dauer), 1, 3) + ' ms'));
  WITH kanaele DO
  BEGIN
    FOR i := channelnumber DOWNTO 1 DO
    BEGIN
      Write(plt, plpa(0, fullsamplerange * (channelnumber - i + 0.5) + kon(daten(0, k[i]), k[i])),
        plpd);
      FOR j := 0 TO rdauer DO
      BEGIN
        Write(plt, plpa(j, fullsamplerange * (channelnumber - i + 0.5) + kon(daten(j, k[i]), k[i])));
        IF keypressed AND (readkey = #27) THEN
        BEGIN
          abortion := True;
          exit;
        END;
      END;
      Write(plt, plpu);
    END;
  END;
END;

{ grafiksuperposition }

PROCEDURE grafiksuperposition.image;
VAR
  maxx, maxy :     LONGINT;
  i, j, l, m, x :  LONGINT;
  y :              yliste;
  gepunktetmenge : SET OF 0..31;
  tpo, tpol, tpor, dum : midint32;
  yn :             WORD;
  zw :             EXTENDED;
BEGIN
  maxx := getmaxx;
  maxy := getmaxy;
  {richtung:=vow;}
  cleardevice;
  TBasicGraphic.image;
  TChannelGraphic.image;
  setcolor(getmaxcolor);
  settextjustify(righttext, centertext);
  outtextxy(getmaxx, 24, 'From ' + wort(zeit(beginn)) + ' ms to ' + wort(zeit(ende)) + ' ms');
  WITH kanaele DO
  BEGIN
    setwritemode(copyput);
    setlinestyle(solidln, 0, normwidth);
    setcolor(getmaxcolor);
    gepunktetmenge := [];
    FOR i := 1 TO channelnumber DO IF belegungsliste[k[i]].gepunktet THEN gepunktetmenge := gepunktetmenge + [k[i]];
    FOR l := 1 TO filenr DO WITH tliste[tl]^.fil[l] DO
      BEGIN
        openfile(l);
        FOR j := 1 TO automn DO
        BEGIN
          IF keypressed THEN IF readkey = #27 THEN
            BEGIN
              schliesse;
              exit;
            END;
          zw := autom^[j] + beginn;
          FOR i := 1 TO channelnumber DO IF NOT (k[i] IN gepunktetmenge) THEN
              y[i] := y0[i] + round(kon(dat(zwi(zw), k[i]), k[i]) * faktor);
          FOR x := lrand + 2 TO maxx DO
            FOR i := 1 TO channelnumber DO IF NOT (k[i] IN gepunktetmenge) THEN
              BEGIN
                yn := y0[i] + round(kon(dat(zwi((x - lrand - 1) * stauchung + zw), k[i]), k[i]) * faktor);
                line(x - 1, y[i], x, yn);
                y[i] := yn;
              END;
          FOR i := 1 TO channelnumber DO IF k[i] IN gepunktetmenge THEN
              WITH tliste[belegungsliste[k[i]].gepunktettl]^.fil[l] DO
              BEGIN
                such(0, automn + 1, zw, dum, tpol);
                such(dum, automn + 1, zw + dauer, tpor, dum);
                FOR tpo := tpol TO tpor DO
                BEGIN
                  yn := y0[i] + round(kon(dat(zwi(autom^[tpo]), k[i]), k[i]) * faktor);
                  x  := round((autom^[tpo] - zw) / stauchung + lrand + 1);
                  FOR m := -1 TO 1 DO
                  BEGIN
                    putpixel(x + m, yn + m, getmaxcolor);
                    putpixel(x + m, yn - m, getmaxcolor);
                  END;
                END;
              END;
        END;
        schliesse;
      END;
  END;
  settextjustify(lefttext, centertext);
  outtextxy(0, 4, '"SUPERPOSITION"');
END;

PROCEDURE grafiksuperposition.plot(gr : BYTE);
VAR
  i, j, m, l : LONGINT;
  rdauer : bigint64;
  tpo, tpol, tpor, dum : midint32;
  zw : EXTENDED;
BEGIN
  TChannelGraphic.plot(gr);
  rdauer := round(dauer);
  Write(plt, kleinschr, 'DI1,0;', plpa(rdauer, fullsamplerange * kanaele.channelnumber), 'CP-6,2;');
  Write(plt, pllb('Start: ' + extwort(extzeit(anfang), 1, 3) + ' ms'), 'CP;CP-6,0;');
  Write(plt, pllb('End  : ' + extwort(extzeit(anfang + dauer), 1, 3) + ' ms'));
  WITH kanaele DO
  BEGIN
    FOR l := 1 TO filenr DO WITH tliste[tl]^.fil[l] DO
      BEGIN
        openfile(l);
        FOR m := 1 TO automn DO
        BEGIN
          zw := autom^[m] + beginn;
          FOR i := channelnumber DOWNTO 1 DO IF NOT belegungsliste[k[i]].gepunktet THEN
            BEGIN
              Write(plt, plpa(0, fullsamplerange * (channelnumber - i + 0.5) + kon(dat(zwi(zw), k[i]), k[i])),
                plpd);
              FOR j := 1 TO rdauer DO
              BEGIN
                Write(plt, plpa(j, fullsamplerange * (channelnumber - i + 0.5) + kon(dat(zwi(zw + j), k[i]), k[i])));
                IF keypressed AND (readkey = #27) THEN
                BEGIN
                  abortion := True;
                  exit;
                END;
              END;
              Write(plt, plpu);
            END
            ELSE
              WITH tliste[belegungsliste[k[i]].gepunktettl]^.fil[1] DO
              BEGIN
                such(0, automn + 1, zw, dum, tpol);
                such(dum, automn + 1, zw + dauer, tpor, dum);
                FOR tpo := tpol TO tpor DO
                  Write(plt, plpa(autom^[tpo] - zw, fullsamplerange * (channelnumber - i + 0.5) + kon(
                    dat(zwi(autom^[tpo]), k[i]), k[i])), plkr);
              END;
        END;
      END;
    Write(plt, kleinschr, 'DI1,0;', plpa(rdauer, fullsamplerange * kanaele.channelnumber), 'CP-6,4;');
    Write(plt, pllb('Files: ' + wort(filenumbers)), 'CP;CP-6,0;');
    Write(plt, pllb('T.-P.: ' + wort(tliste[tl]^.triggsum)));
    Write(plt, kleinschr, 'DI0,1;', plpa(rdauer, 0), 'CP-2,-2;',
      pllb('SUPERPOSITION'));

  END;
END;

CONSTRUCTOR grafiksuperposition.construct(VAR kan : TChannelVolume; von, bis : typeextended; trl : CHAR);
BEGIN
  kanaele := kan;
  tl      := trl;
  beginn  := von;
  ende    := bis;
  anfang  := beginn;
  dauer   := bis - von;
  WITH liste[tliste[tl]^.erstfile] DO
    TBasicGraphic.construct(14, 24, Name, head.protocol, tliste[tl]^.fileanz);
END;

{ TChannelDataGraphic }

FUNCTION TChannelDataGraphic.stellex(stelle : typeextended) : bigint64;
VAR
  zwischen : EXTENDED;
BEGIN
  zwischen := maxe(mine((stelle - datanf) / stauchung + lrand + 1, maxlongint), -maxlongint);
  stellex  := round(zwischen);
END;

FUNCTION TChannelDataGraphic.xstelle(x : bigint64) : typeextended;
BEGIN
  xstelle := datanf + (x - lrand - 1) * stauchung;
END;

PROCEDURE TChannelDataGraphic.linie(x1, x2 : bigint64);
VAR
  i : WORD;
BEGIN
  setlinestyle(solidln, 0, normwidth);
  FOR i := x1 TO x2 DO line(i, oben, i, getmaxy - 11);
END;

PROCEDURE TChannelDataGraphic.image;
VAR
  wandert :        PDataBlock;
  pwandert :       PBulletList;
  di :             dirstr;
  na :             namestr;
  ex :             extstr;
  maxx, maxy :     bigint64;
  i, j, x, yn :    LONGINT;
  y :              yliste;
  gepunktetmenge : SET OF 0..31;
  tpo, tpol, tpor, dum : midint32;
BEGIN
  maxx   := getmaxx;
  maxy   := getmaxy;
  {richtung:=vow;}
  datanf := anfang;
  cleardevice;
  TChannelGraphic.image;
  WITH kanaele DO
  BEGIN
    setwritemode(copyput);
    setlinestyle(solidln, 0, normwidth);
    setcolor(getmaxcolor);
    gepunktetmenge := [];
    FOR i := 1 TO channelnumber DO IF belegungsliste[k[i]].gepunktet THEN gepunktetmenge := gepunktetmenge + [k[i]]
      ELSE
        y[i] := y0[i] + round(kon(dat(zwi(datanf), k[i]), k[i]) * faktor);
    FOR x := lrand + 1 TO maxx - 1 DO
      FOR i := 1 TO channelnumber DO IF NOT (k[i] IN gepunktetmenge) THEN
        BEGIN
          yn := y0[i] + round(kon(dat(zwi(xstelle(x)), k[i]), k[i]) * faktor);
          line(x, y[i], x + 1, yn);
          y[i] := yn;
        END;
    FOR i := 1 TO channelnumber DO IF k[i] IN gepunktetmenge THEN
        WITH tliste[belegungsliste[k[i]].gepunktettl]^.fil[aktfile] DO
        BEGIN
          such(0, automn + 1, anfang, dum, tpol);
          such(dum, automn + 1, anfang + dauer, tpor, dum);
          FOR tpo := tpol TO tpor DO
          BEGIN
            yn := y0[i] + round(kon(dat(zwi(autom^[tpo]), k[i]), k[i]) * faktor);
            x  := stellex(autom^[tpo]);
            FOR j := -1 TO 1 DO
            BEGIN
              putpixel(x + j, yn + j, getmaxcolor);
              putpixel(x + j, yn - j, getmaxcolor);
            END;
          END;
        END;
  END;
  setcolor(getmaxcolor);
  settextjustify(lefttext, centertext);
  fsplit(filename, di, na, ex);
  outtextxy(0, 4, 'File    : ' + na + ex);
  outtextxy(0, 14, 'Duration: ' + wort(zeit(liste[aktfile].length)) + ' ms');
  outtextxy(0, 24, 'Speed   : ' + extwort(ablenkung, 4, 2) + ' mm/s');
  outtextxy(spalte1, 4, 'Channel :');
  outtextxy(spalte1, 14, 'Position:');
  outtextxy(spalte1, 24, 'Amplit. :');
  outtextxy(spalte2, 4, 'Mode    : ' + statustext[status]);
  outtextxy(spalte2, 14, 'Distance:');
  settextjustify(centertext, centertext);
  setwritemode(xorput);
  wandert := liste[aktfile].block^;
  WHILE wandert^.endpos < anfang DO wandert := wandert^.Next;
  WHILE wandert^.frompos < anfang + dauer DO
  BEGIN
    linie(max(lrand + 1, stellex(wandert^.frompos)), min(maxx, stellex(wandert^.endpos)));
    wandert := wandert^.Next;
  END;
  setlinestyle(userbitln, tstellenpattern, normwidth);
  pwandert := liste[aktfile].selectedbullet^;
  WHILE pwandert^.bei < anfang DO pwandert := pwandert^.Next;
  WHILE pwandert^.bei < anfang + dauer DO
  BEGIN
    line(stellex(pwandert^.bei), oben, stellex(pwandert^.bei), maxy - 11);
    pwandert := pwandert^.Next;
  END;
  IF (strichstelle > anfang) AND (strichstelle < anfang + dauer) THEN
    strich := stellex(strichstelle)
  ELSE
    strich := maxx DIV 2;
  IF (strichst1 > anfang) AND (strichst1 < anfang + dauer) THEN
    strich1 := stellex(strichst1)
  ELSE
    strich1 := lrand + 1;
  setcolor(min(strichfarbe, getmaxcolor));
  setlinestyle(dashedln, 0, normwidth);
  line(strich, oben, strich, maxy - 11);
  IF strich1da THEN line(strich1, oben, strich1, maxy - 11);
  settextjustify(lefttext, centertext);
  setcolor(getmaxcolor);
  {if spannstatus then richtung:=mit;}
  zei1a := '';
  zei2a := '';
  zei3a := '';
  zei4a := '';
END;

PROCEDURE TChannelDataGraphic.plot(gr : BYTE);
VAR
  i, j :   LONGINT;
  rdauer : bigint64;
  tpo, tpol, tpor, dum : midint32;
BEGIN
  TChannelGraphic.plot(gr);
  rdauer := round(dauer);
  Write(plt, kleinschr, 'DI1,0;', plpa(rdauer, fullsamplerange * kanaele.channelnumber), 'CP-6,3;');
  Write(plt, pllb('Start: ' + extwort(extzeit(anfang), 1, 3) + ' ms'), 'CP;CP-6,0;');
  {write(plt,pllb('End  : '+extwort(extzeit(anfang+dauer),1,3)+' ms'));}
  Write(plt, pllb('Speed on Screen:'), 'CP;CP-6,0;', pllb(extwort(ablenkung, 4, 2) + ' mm/s'));
  WITH kanaele DO
  BEGIN
    FOR i := channelnumber DOWNTO 1 DO IF NOT belegungsliste[k[i]].gepunktet THEN
      BEGIN
        Write(plt, plpa(0, fullsamplerange * (channelnumber - i + 0.5) + kon(dat(zwi(datanf), k[i]), k[i])),
          plpd);
        FOR j := 0 TO rdauer DO
        BEGIN
          Write(plt, plpa(j, fullsamplerange * (channelnumber - i + 0.5) + kon(dat(zwi(datanf + j), k[i]), k[i])));
          IF keypressed AND (readkey = #27) THEN
          BEGIN
            abortion := True;
            exit;
          END;
        END;
        Write(plt, plpu);
      END
      ELSE
        WITH tliste[belegungsliste[k[i]].gepunktettl]^.fil[aktfile] DO
        BEGIN
          such(0, automn + 1, anfang, dum, tpol);
          such(dum, automn + 1, anfang + dauer, tpor, dum);
          FOR tpo := tpol TO tpor DO
            Write(plt, plpa(autom^[tpo] - anfang, fullsamplerange * (channelnumber - i + 0.5) +
              kon(dat(zwi(autom^[tpo]), k[i]), k[i])), plkr);
        END;
  END;
END;

CONSTRUCTOR TChannelDataGraphic.construct(afi : BYTE; VAR kane : TChannelVolume; anf : typeextended; abl : SINGLE; men : menue);
CONST
  buchbr = 80;
  lupe   = 4;
VAR
  gr :       BYTE;
  wandert :  PDataBlock;
  pwandert : PBulletList;
  zei1, zei2, zei3, zei4 : string20;
  tb :       CHAR;
  i, kannr : BYTE;
  extvar :   EXTENDED;

  PROCEDURE drawvline(VAR x : bigint64; onto : bigint64);
  BEGIN
    setlinestyle(dashedln, 0, normwidth);
    setcolor(min(strichfarbe, getmaxcolor));
    line(x, oben, x, getmaxy - 11);
    x := onto;
    IF (x >= 0) AND (getmaxx >= x) THEN line(x, oben, x, getmaxy - 11);
    setcolor(getmaxcolor);
  END;

BEGIN
  anfang    := anf;
  kanaele   := kane;
  ablenkung := abl;
  aktfile   := afi;
  filename  := liste[aktfile].Name;
  fileext   := liste[aktfile].head.protocol;
  openfile(aktfile);
  kannr := 1;
  dauer := fre / ablenkung * 228;
  opengraph;
  spalte1      := getmaxx DIV 3;
  spalte2      := (2 * getmaxx) DIV 3;
  spannstatus  := True;
  status       := bleibt;
  strich1da    := False;
  strichstelle := dauer / 2;
  strichst1    := 0;
  image;
  WITH liste[aktfile] DO
    REPEAT
      strichstelle := xstelle(strich);
      strichst1    := xstelle(strich1);
      zei1         := wort(kannr) + ' (' + schriftliste[kanaele.k[kannr]] + ')';
      zei2         := extwort(extzeit(strichstelle), 4, 3) + ' ms';
      IF spannstatus THEN WITH kanaele DO
          zei3 := extwort(extspannung(dat(zwi(strichstelle), k[kannr]), k[kannr]), 4, 2) +
            ' ' + belegungsliste[k[kannr]].einhwort
      ELSE
        zei3   := '';
      IF strich1da THEN
        zei4 := extwort(extzeit(abs(strichstelle - strichst1)), 4, 3) + ' ms'
      ELSE
        zei4 := '';
      ;
      setcolor(0);
      outtextxy(spalte1 + buchbr, 4, zei1a);
      setcolor(getmaxcolor);
      outtextxy(spalte1 + buchbr, 4, zei1);
      setcolor(0);
      outtextxy(spalte1 + buchbr, 14, zei2a);
      setcolor(getmaxcolor);
      outtextxy(spalte1 + buchbr, 14, zei2);
      setcolor(0);
      outtextxy(spalte1 + buchbr, 24, zei3a);
      setcolor(getmaxcolor);
      outtextxy(spalte1 + buchbr, 24, zei3);
      setcolor(0);
      outtextxy(spalte2 + buchbr, 14, zei4a);
      setcolor(getmaxcolor);
      outtextxy(spalte2 + buchbr, 14, zei4);
      zei1a := zei1;
      zei2a := zei2;
      zei3a := zei3;
      zei4a := zei4;
      CASE readkey OF
        #0 : CASE readkey OF
            {Hoch}     #72 : IF kannr > 1 THEN Dec(kannr);
            {Runter}   #80 : IF kannr < kanaele.channelnumber THEN Inc(kannr);
            {PgUp}     #73 : BEGIN
              anfang := mine(anfang + dauer, length);
              image;
            END;
            {Ctrl PgUp}#132 : BEGIN
              anfang := mine(anfang + dauer / 4, length);
              image;
            END;
            {PgDn}     #81 : BEGIN
              anfang := maxe(0, anfang - dauer);
              image;
            END;
            {Ctrl PgDn}#118 : BEGIN
              anfang := maxe(0, anfang - dauer / 4);
              image;
            END;
            {F1}       #59 : BEGIN
              ablenkung := ablenkung * lupe;
              dauer     := dauer / lupe;
              anfang    := strichstelle - dauer / 2;
              image;
            END;
            {F2}       #60 : BEGIN
              ablenkung := ablenkung / lupe;
              dauer     := dauer * lupe;
              anfang    := maxe(0, strichstelle - dauer / 2);
              image;
            END;
            {Ctrl Rech}#116 : drawvline(strich, min(strich + 1, getmaxx));
            {Ctrl Link}#115 : drawvline(strich, max(lrand + 1, strich - 1));
            {Rech}     #77 : drawvline(strich, min(strich + 10, getmaxx));
            {Link}     #75 : drawvline(strich, max(lrand + 1, strich - 10));
            {Ctrl F3}  #96 : BEGIN
              strich1da := NOT strich1da;
              setlinestyle(dashedln, 0, normwidth);
              setcolor(min(strichfarbe, getmaxcolor));
              line(strich1, oben, strich1, getmaxy - 11);
              setcolor(getmaxcolor);
            END;
            {F3}       #61 : IF strich1da THEN
              BEGIN
                drawvline(strich1, strich);
                drawvline(strich, strich + 1);
              END;
            {Ctrl Home}#119 : BEGIN
              anfang := 0;
              image;
            END;
            {Ctrl End} #117 : BEGIN
              anfang := length - dauer;
              image;
            END;
            {Home}     #71 : drawvline(strich, lrand + 1);
            {End}      #79 : drawvline(strich, getmaxx);
            {F4}       #62 : IF status = bleibt THEN
              BEGIN
                closegraph;
                men(aktfile);
                opengraph;
                image;
              END;
            {Ins}      #82 : BEGIN
              pwandert := selectedbullet^;
              WHILE pwandert^.bei < strichstelle DO pwandert := pwandert^.Next;
              IF (stellex(pwandert^.bei) <> strich) AND (stellex(pwandert^.Prev^.bei) <> strich) THEN
              BEGIN
                setlinestyle(userbitln, tstellenpattern, normwidth);
                line(strich, oben, strich, getmaxy - 11);
                prein(pwandert);
                pwandert^.bei := strichstelle;
              END;
            END;
            {Del}      #83 : BEGIN
              pwandert := selectedbullet^;
              WHILE pwandert^.bei < strichstelle DO pwandert := pwandert^.Next;
              IF pwandert^.bei < anfang + dauer THEN
              BEGIN
                setlinestyle(userbitln, tstellenpattern, normwidth);
                line(stellex(pwandert^.bei), oben, stellex(pwandert^.bei), getmaxy - 11);
                praus(pwandert);
              END;
            END;
            {F5}       #63 : IF status = bleibt THEN
              BEGIN
                setcolor(0);
                outtextxy(spalte2 + buchbr, 4, statustext[status]);
                setcolor(getmaxcolor);
                status := neu;
                outtextxy(spalte2 + buchbr, 4, statustext[status]);
                wandert := block^;
                WHILE wandert^.endpos < strichstelle DO wandert := wandert^.Next;
                IF wandert^.frompos > strichstelle THEN
                BEGIN
                  rein(wandert);
                  wandert^.frompos := strichstelle;
                  wandert^.endpos := strichstelle;
                  linie(strich, strich);
                END;
              END;
            {F7}       #65 : IF status = bleibt THEN
              BEGIN
                wandert := block^;
                WHILE wandert^.endpos < strichstelle DO wandert := wandert^.Next;
                IF wandert^.frompos <= strichstelle THEN
                BEGIN
                  linie(max(lrand + 1, stellex(wandert^.frompos)),
                    min(getmaxx, stellex(wandert^.endpos)));
                  raus(wandert);
                END;
              END;
            {Ctrl F8} #101 : IF status = bleibt THEN
              BEGIN
                wandert := block^;
                WHILE wandert^.Next <> nil DO
                BEGIN
                  linie(max(lrand + 1, stellex(wandert^.frompos)),
                    min(getmaxx, stellex(wandert^.endpos)));
                  raus(wandert);
                END;
              END;
            {F6}       #64 : IF status = neu THEN
              BEGIN
                WHILE strichstelle >= wandert^.Next^.frompos DO
                BEGIN
                  linie(max(lrand + 1, stellex(wandert^.endpos) + 1),
                    max(lrand + 1, stellex(wandert^.Next^.frompos) - 1));
                  wandert^.Next^.frompos := wandert^.frompos;
                  raus(wandert);
                END;
                WHILE strichstelle <= wandert^.Prev^.endpos DO
                BEGIN
                  linie(min(getmaxx, stellex(wandert^.Prev^.endpos) + 1),
                    min(getmaxx, stellex(wandert^.frompos) - 1));
                  wandert^.frompos := wandert^.Prev^.frompos;
                  wandert      := wandert^.Prev;
                  raus(wandert);
                END;
                IF wandert^.frompos > strichstelle THEN
                BEGIN
                  linie(strich, min(stellex(wandert^.frompos) - 1, getmaxx));
                  wandert^.frompos := strichstelle;
                END;
                IF wandert^.endpos < strichstelle THEN
                BEGIN
                  linie(max(lrand + 1, stellex(wandert^.endpos) + 1), strich);
                  wandert^.endpos := strichstelle;
                END;
                setcolor(0);
                outtextxy(spalte2 + buchbr, 4, statustext[status]);
                setcolor(getmaxcolor);
                status := bleibt;
                outtextxy(spalte2 + buchbr, 4, statustext[status]);
              END;
            {Ctrl F9}  #102 : BEGIN
              kontrolle(aktfile, upcase(readkey));
              image;
            END;
            {Ctrl PrtS}#114 : plotstart;
            {Ctrl F10} #103 : BEGIN
              pwandert := selectedbullet^;
              setlinestyle(userbitln, tstellenpattern, normwidth);
              WHILE pwandert^.Next <> nil DO
              BEGIN
                IF (pwandert^.bei < anfang + dauer) AND (pwandert^.bei >= anfang) THEN
                  line(stellex(pwandert^.bei), oben,
                    stellex(pwandert^.bei), getmaxy - 11);
                praus(pwandert);
              END;
            END;
          END;
        'P' : plotstart;
        'B' : BEGIN
          drawvline(strich, strich - 1);
          strichstelle := xstelle(strich);
          wandert      := block^;
          IF wandert^.frompos > strichstelle THEN drawvline(strich, stellex(0))
          ELSE
          BEGIN
            WHILE wandert^.Next^.frompos < strichstelle DO wandert := wandert^.Next;
            drawvline(strich, stellex(wandert^.frompos));
          END;
          strichstelle := xstelle(strich);
          IF anfang >= strichstelle THEN
          BEGIN
            anfang := strichstelle - dauer / 4;
            image;
          END;
        END;
        'F' : BEGIN
          drawvline(strich, strich + 1);
          strichstelle := xstelle(strich);
          wandert      := block^;
          WHILE wandert^.frompos < strichstelle DO wandert := wandert^.Next;
          IF wandert^.Next <> nil THEN drawvline(strich, stellex(wandert^.frompos))
          ELSE
            drawvline(strich, stellex(length) - 1);
          strichstelle := xstelle(strich);
          IF strichstelle >= anfang + dauer THEN
          BEGIN
            IF wandert^.Next = nil THEN anfang := length - dauer
            ELSE
              anfang := strichstelle - dauer / 4;
            image;
          END;
        END;
        {Ret} #13 : BEGIN
          anfang := strichstelle - dauer / 2;
          image;
        END;
        {Esc} #27 : BEGIN
          closegraph;
          schliesse;
          {richtung:=vow;} exit;
        END;
        ' ' : BEGIN
          spannstatus := NOT spannstatus;
          {if spannstatus then richtung:=mit} END;
      END;
      WHILE keypressed DO tb := readkey;
    UNTIL ende;
  closegraph;
  schliesse;
  {richtung:=vow;}
  ende := False;
END;

{ grafikmittel }

PROCEDURE grafikmittel.construct(VAR kan : TChannelVolume; anf, dau : typeextended; trl : CHAR; trp : bigint64);
VAR
  i : LONGINT;
BEGIN
  kanaele  := kan;
  anfang   := anf;
  dauer    := dau;
  tl       := trl;
  tpgesamt := trp;
  rdauer   := round(dauer);
  {$ifdef fpc}
for i:=0 to maxchannelsandfilters do if i in kan.dabei then begin new(mittel[i]); fillchar(mittel[i]^,sizeof(mittel[i]^),0) end;
  {$endif}
  WITH liste[tliste[tl]^.erstfile] DO
    TBasicGraphic.construct(14, 24, Name, head.protocol, tliste[tl]^.fileanz);
  {$ifdef fpc}
 for i:=0 to maxchannelsandfilters do if i in kan.dabei then dispose(mittel[i]);
  {$endif}
END;


FUNCTION grafikmittel.daten(stelle : typeextended; kanal : BYTE) : sample;
BEGIN
  daten := round(mittel[kanal]^[round(stelle)]);
END;

PROCEDURE grafikmittel.image;
BEGIN
  TBasicGraphic.image;
  grafikkurve.image;
  setcolor(getmaxcolor);
  settextjustify(righttext, centertext);
  outtextxy(getmaxx, 4, 'Trigger points: ' + wort(tpgesamt));
END;

PROCEDURE grafikmittel.plot(gr : BYTE);
BEGIN
  grafikkurve.plot(gr);
  IF abortion THEN exit;
  Write(plt, kleinschr, 'DI1,0;', plpa(rdauer, fullsamplerange * kanaele.channelnumber), 'CP-6,4;');
  Write(plt, pllb('Files: ' + wort(filenumbers)), 'CP;CP-6,0;');
  Write(plt, pllb('T.-P.: ' + wort(tpgesamt)));
END;

PROCEDURE grafikmittel.filewrite(VAR outfile : Text);
VAR
  i, k : LONGINT;
BEGIN
  FOR i := 0 TO rdauer DO
  BEGIN
    Write(outfile, extzeit(i + anfang) : 12 : 3);
    FOR k := 0 TO maxchannelsandfilters DO IF k IN kanaele.dabei THEN
        Write(outfile, extspannung(mittel[k]^[i], k) : 15 : 3);
    writeln(outfile);
  END;
END;

{ garfikintervallroh }

PROCEDURE grafikintervallroh.construct(rtl : CHAR; dfn : WORD; ybe : EXTENDED);
BEGIN
  ref      := rtl;
  ybereich := ybe;
  diffn    := dfn;
  new(diff);
  WITH tliste[ref]^, liste[erstfile] DO
    TBasicGraphic.construct(24, 34, Name, head.protocol, fileanz);
  dispose(diff);
END;

PROCEDURE grafikintervallroh.image;
CONST
  lrand = 32;
  urand = 14;
VAR
  i, j, x :     LONGINT;
  faktor :      EXTENDED;
  breite, enn : WORD;
BEGIN
  INHERITED image;
  breite := max((getmaxx - lrand - 2) DIV diffn, 1);
  enn    := min(diffn, getmaxx - lrand - 2);
  setcolor(min(bildfarbe, getmaxcolor));
  settextjustify(centertext, centertext);
  setwritemode(copyput);
  setlinestyle(solidln, 0, normwidth);
  line(lrand + 2, 40, lrand + 2, getmaxy - urand);
  line(lrand - 3, getmaxy - urand, lrand + 1 + enn * breite, getmaxy - urand);
  settextjustify(righttext, centertext);
  outtextxy(lrand - 4, 65, 'n');
  outtextxy(lrand, 76, 'bin');
  line(8, 70, lrand, 70);
  outtextxy(lrand, 47, extwort(ybereich, 4, 2));
  line(lrand + 1, 40, lrand - 3, 40);
  outtextxy(lrand, getmaxy - urand - 5, '0');
  setcolor(getmaxcolor);
  faktor := -(getmaxy - urand - 40) / ybereich;
  FOR i := 1 TO enn DO IF diff^[i] > 0 THEN FOR j := 1 TO breite DO
      BEGIN
        x := lrand + 1 + (i - 1) * breite + j;
        line(x, getmaxy - urand - 1, x, getmaxy - urand - 1 + round(diff^[i] * faktor));
      END;
END;

PROCEDURE grafikintervallroh.plot(gr : BYTE);
VAR
  i : WORD;
BEGIN
  WITH p[gr] DO Write(plt, plip(p1x, p1y, p2x, p2y));
  Write(plt, plsc(0, diffn, 0, ybereich));
  Write(plt, plpa(0, diff^[1]), plpd);
  FOR i := 1 TO diffn - 1 DO Write(plt, plpa(i, diff^[i]), plpa(i, diff^[i + 1]));
  Write(plt, plpa(diffn, diff^[diffn]), plpa(diffn, 0), plpu);
  Write(plt, relativschr, plpa(0, ybereich), 'CP-5.5,-0.75;',
    pllb(buendig(extwort(ybereich, 5, 3))));
  Write(plt, plpa(0, ybereich), kleinschr, 'CP0,3;', pllb(filename), 'CP;', pllb(fileext), 'CP;',
    pllb(filecomment), relativschr);
  Write(plt, plpa(0, ybereich), 'TL0,1.3', plyt, plpd, plpa(0, 0), plyt,
    plpd, plpa(diffn, 0), plpu);
  Write(plt, plpa(0, 0), 'CP-1.5,0.25;', pllb('0'));
  Write(plt, plpa(0, 0), 'DI0,1CP0,2.5;', pllb('Occurrences [n/bin]'));
  Write(plt, plpa(0, ybereich / 2), 'TL0,0.6;', plyt);
END;

{ grafikintervall }

PROCEDURE grafikintervall.construct(rtl : CHAR; anf, sch : typeextended; dfn : WORD; ybe : EXTENDED);
BEGIN
  anfang  := anf;
  schluss := sch;
  dauer   := schluss - anfang;
  klasse  := dauer / dfn;
  INHERITED construct(rtl, dfn, ybe);
END;

PROCEDURE grafikintervall.image;
CONST
  lrand = 32;
  urand = 14;
VAR
  i, x :        LONGINT;
  anzahl :      BYTE;
  werte :       skalafeld;
  xdauer :      EXTENDED;
  enn, breite : WORD;
BEGIN
  INHERITED image;
  enn    := diffn;
  xdauer := dauer;
  bildbalken(lrand, xdauer, klasse, breite, enn);
  skala(anfang, xdauer, anzahl, werte, zeitskala);
  setcolor(min(bildfarbe, getmaxcolor));
  settextjustify(centertext, centertext);
  setwritemode(copyput);
  setlinestyle(solidln, 0, normwidth);
  FOR i := 1 TO anzahl DO
  BEGIN
    x := round((werte[i] - anfang) / klasse * breite + lrand + 2);
    line(x, getmaxy - urand + 1, x, getmaxy - urand + 4);
    outtextxy(x, getmaxy - 4, wort(zeit(werte[i])));
  END;
  settextjustify(righttext, centertext);
  outtextxy(getmaxx, getmaxy - 4, 't [ms]');
  settextjustify(lefttext, centertext);
  outtextxy(0, 14, 'Bins þ Width: ' + wort(enn) + ' þ ' + extwort(extzeit(klasse), 8, 3) + ' ms');
END;

PROCEDURE grafikintervall.plot(gr : BYTE);
VAR
  anzahl : BYTE;
  werte :  skalafeld;
  i :      LONGINT;
BEGIN
  INHERITED plot(gr);
  Write(plt, plpa(diffn, 0), 'DI1,0;', relativschr, 'CP-6,-1;', pllb('t [ms]'), 'TL-1,0;');
  skala(anfang, dauer, anzahl, werte, zeitskala);
  FOR i := anzahl DOWNTO 1 DO
    Write(plt, plpa((werte[i] - anfang) / dauer * diffn, 0), plxt, 'CP-.33,-1;',
      pllb(wort(zeit(werte[i]))));
  Write(plt, kleinschr, plpa(diffn, 0), 'DI0,1;CP0,-3;');
  Write(plt, pllb(' Bins          :' + wort(diffn) + ' (' + extwort(zeit(klasse), 1, 3) + ' ms)'));
END;

PROCEDURE grafikintervall.filewrite(VAR outfile : Text);
VAR
  i : WORD;
BEGIN
  FOR i := 1 TO diffn DO
    writeln(outfile, extzeit(anfang + (i - 0.5) * dauer / diffn) : 12 : 3, diff^[i] : 12 : 5);
END;

{ grafikphasenintervall }

PROCEDURE grafikphasenintervall.image;
CONST
  lrand = 32;
  urand = 14;
VAR
  i, x :        LONGINT;
  anzahl :      BYTE;
  werte :       skalafeld;
  xdauerphase : EXTENDED;
  breite, enn : WORD;
BEGIN
  INHERITED image;
  enn         := diffn;
  xdauerphase := dauerphase;
  bildbalken(lrand, xdauerphase, klasse, breite, enn);
  skala(anfphase, xdauerphase, anzahl, werte, phasenskala);
  setcolor(min(bildfarbe, getmaxcolor));
  settextjustify(centertext, centertext);
  setwritemode(copyput);
  setlinestyle(solidln, 0, normwidth);
  FOR i := 1 TO anzahl DO
  BEGIN
    x := round((werte[i] - anfphase) / klasse * breite + lrand + 2);
    line(x, getmaxy - urand + 1, x, getmaxy - urand + 4);
    outtextxy(x, getmaxy - 4, extwort(frac(werte[i]), 4, 2));
  END;
  settextjustify(righttext, centertext);
  outtextxy(getmaxx, getmaxy - 4, 'Phase');
  settextjustify(lefttext, centertext);
  outtextxy(0, 14, 'Bins þ Width: ' + wort(enn) + ' þ ' + extwort(klasse, 1, 3));
END;

PROCEDURE grafikphasenintervall.plot(gr : BYTE);
VAR
  anzahl : BYTE;
  werte :  skalafeld;
  i :      LONGINT;
BEGIN
  INHERITED plot(gr);
  Write(plt, plpa(diffn, 0), 'DI1,0;', relativschr, 'CP-6,-1;', pllb('Phase'), 'TL-1,0;');
  skala(anfphase, dauerphase, anzahl, werte, phasenskala);
  FOR i := anzahl DOWNTO 1 DO
    Write(plt, plpa((werte[i] - anfphase) / dauerphase * diffn, 0), plxt, 'CP-.33,-1;',
      pllb(extwort(frac(werte[i]), 4, 2)));
  Write(plt, kleinschr, plpa(diffn, 0), 'DI0,1;CP0,-3;');
  Write(plt, pllb(' Bins          :' + wort(diffn) + ' (' + extwort(klasse, 1, 3) + ')'));
END;

PROCEDURE grafikphasenintervall.filewrite(VAR outfile : Text);
VAR
  i : WORD;
BEGIN
  FOR i := 1 TO diffn DO
    writeln(outfile, frac(anfphase + (i - 0.5) * dauerphase / diffn) : 9 : 6, diff^[i] : 12 : 5);
END;

PROCEDURE grafikphasenintervall.construct(rtl : CHAR; anfph, dauph : EXTENDED; dfn : WORD; ybe : EXTENDED);
BEGIN
  anfphase   := anfph;
  diffnganz  := round(dfn / dauph);
  dauerphase := dfn / diffnganz;
  klasse     := dauerphase / dfn;
  INHERITED construct(rtl, dfn, ybe);
END;

BEGIN
  strichfarbe := white;
  bildfarbe   := green;
END.
