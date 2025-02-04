{ Borland-Pascal 7.0 / FPC 3.2.2 }
{$ifdef fpc} {$mode TP} {$endif}

UNIT nlausw;

{$IFDEF MSDOS}
{$A+,B-,E+,F-,G-,I-,N+,O-,P+,T+,V+,X-}
{$ELSE}
{$A+,B-,E+,F-,G+,I-,N+,P+,T+,V+,X-}
{$ENDIF}

INTERFACE

USES  crt, graph, daff, wavpcm, tulab42, nlrahmen,
  dos, grafik, tlfilter, nltrigg,
  bequem, plotter, tlfiles,
  nlgrafik;

TYPE
  statistik = OBJECT
    n :         bigint64;
    sx, ssqrx : EXTENDED;
    mx, rox :   EXTENDED;
    CONSTRUCTOR init;
    PROCEDURE dazu(x : EXTENDED);
    PROCEDURE rechnen;
  END;

  phasenstatistik = OBJECT
    n :         bigint64;
    sx, sy :    EXTENDED;
    mph, lvek : EXTENDED;
    CONSTRUCTOR init;
    PROCEDURE dazu(ph : EXTENDED);
    PROCEDURE rechnen;
  END;

  ampnormalhistogramm = OBJECT(grafikamplitude)
    tstat :  statistik;
    gesamt : bigint64;
    PROCEDURE berechnen; VIRTUAL;
    PROCEDURE image; VIRTUAL;
    PROCEDURE plot(gr : BYTE); VIRTUAL;
    CONSTRUCTOR construct(xk : BYTE; rtl : CHAR; dfn : WORD; ybe : EXTENDED; mins, maxs : sample);
  END;

  grafikaverage = OBJECT(grafikmittel)
    hoehe :   ARRAY[1..maxchannelsandfilters + 1] OF sample;
    maxi :    ARRAY[1..maxchannelsandfilters + 1] OF typedouble;
    maxix :   ARRAY[1..maxchannelsandfilters + 1] OF bigint64;
    flaeche : ARRAY[1..maxchannelsandfilters + 1] OF typedouble;
    PROCEDURE berechnen; VIRTUAL;
    PROCEDURE image; VIRTUAL;
    PROCEDURE plot(gr : BYTE); VIRTUAL;
    CONSTRUCTOR construct(VAR kan : TChannelVolume; anf, dau : typeextended; trl : CHAR;
      trp : bigint64);
  END;

  grafikphasenaverage = OBJECT(grafikmittel)
    triggweis : triggerweiser;
    hoehe :     ARRAY[1..maxchannelsandfilters + 1] OF sample;
    maxi :      ARRAY[1..maxchannelsandfilters + 1] OF typedouble;
    maxix :     ARRAY[1..maxchannelsandfilters + 1] OF bigint64;
    flaeche :   ARRAY[1..maxchannelsandfilters + 1] OF typedouble;
    PROCEDURE berechnen; VIRTUAL;
    PROCEDURE image; VIRTUAL;
    PROCEDURE plot(gr : BYTE); VIRTUAL;
    CONSTRUCTOR construct(VAR kan : TChannelVolume; anf, dau : typeextended; trl : CHAR;
      trp : bigint64; VAR weis : triggerweiser);
  END;

  intervallhistogramm = OBJECT(grafikintervall)
    gesamt : bigint64;
    tstat :  statistik;
    PROCEDURE berechnen; VIRTUAL;
    PROCEDURE image; VIRTUAL;
    PROCEDURE plot(gr : BYTE); VIRTUAL;
    CONSTRUCTOR construct(rtl : CHAR; anf, sch : typeextended; din : WORD; ybe : EXTENDED);
  END;

  autokorrelogramm = OBJECT(grafikintervall)
    gesamt : bigint64;
    tstat :  statistik;
    PROCEDURE berechnen; VIRTUAL;
    PROCEDURE image; VIRTUAL;
    PROCEDURE plot(gr : BYTE); VIRTUAL;
    CONSTRUCTOR construct(rtl : CHAR; anf, sch : typeextended; din : WORD; ybe : EXTENDED);
  END;

  kreuzkorrelogramm = OBJECT(grafikintervall)
    obj :          CHAR;
    refn, objn :   bigint64;
    nstat, tstat : statistik;
    PROCEDURE berechnen; VIRTUAL;
    PROCEDURE image; VIRTUAL;
    PROCEDURE plot(gr : BYTE); VIRTUAL;
    CONSTRUCTOR construct(rtl, otl : CHAR; anf, sch : typeextended; din : WORD; ybe : EXTENDED);
  END;

  psthistogramm = OBJECT(grafikintervall)
    obj :          CHAR;
    anfph, endph : SHORTINT;
    refn, objn :   bigint64;
    nstat, tstat : statistik;
    PROCEDURE berechnen; VIRTUAL;
    PROCEDURE image; VIRTUAL;
    PROCEDURE plot(gr : BYTE); VIRTUAL;
    CONSTRUCTOR construct(rtl, otl : CHAR; aph, eph : SHORTINT; anf, sch : typeextended;
      din : WORD; ybe : EXTENDED);
  END;

  latenzhistogramm = OBJECT(grafikintervall)
    obj :   CHAR;
    refn :  bigint64;
    tstat : statistik;
    PROCEDURE berechnen; VIRTUAL;
    PROCEDURE image; VIRTUAL;
    PROCEDURE plot(gr : BYTE); VIRTUAL;
    CONSTRUCTOR construct(rtl, otl : CHAR; anf, sch : typeextended; din : WORD; ybe : EXTENDED);
  END;

  phasenhistogramm = OBJECT(grafikphasenintervall)
    obj :        CHAR;
    refn, objn : bigint64;
    weis :       triggerweiser;
    nstat :      statistik;
    phstat :     phasenstatistik;
    PROCEDURE berechnen; VIRTUAL;
    PROCEDURE image; VIRTUAL;
    PROCEDURE plot(gr : BYTE); VIRTUAL;
    CONSTRUCTOR construct(rtl, otl : CHAR; minabst, maxabst : typeextended;
      anfph, schph : EXTENDED; din : WORD; ybe : EXTENDED);
  END;

FUNCTION phasentest(tph, anfph, dauph : EXTENDED) : BOOLEAN;

IMPLEMENTATION

FUNCTION phasentest(tph, anfph, dauph : EXTENDED) : BOOLEAN;
BEGIN
  phasentest := ((tph >= anfph) AND (tph <= anfph + dauph)) OR ((tph + 1 >= anfph) AND (tph + 1 <= anfph + dauph));
END;

{ statistik }

CONSTRUCTOR statistik.init;
BEGIN
  n     := 0;
  sx    := 0;
  ssqrx := 0;
END;

PROCEDURE statistik.dazu(x : EXTENDED);
BEGIN
  Inc(n);
  sx    := sx + x;
  ssqrx := ssqrx + sqr(x);
END;

PROCEDURE statistik.rechnen;
VAR
  arg : EXTENDED;
BEGIN
  IF n > 1 THEN
  BEGIN
    mx  := sx / n;
    arg := (ssqrx - sqr(sx) / n) / (n - 1);
    IF arg <= 0 THEN rox := 0
    ELSE
      rox                := sqrt(arg);
  END
  ELSE
  BEGIN
    IF n > 0 THEN mx := sx / n
    ELSE
      mx             := 0;
    rox := 0;
  END;
END;

{ phasenstatistik }

CONSTRUCTOR phasenstatistik.init;
BEGIN
  n  := 0;
  sx := 0;
  sy := 0;
END;

PROCEDURE phasenstatistik.dazu(ph : EXTENDED);
BEGIN
  Inc(n);
  sx := sx + cos(ph);
  sy := sy + sin(ph);
END;

PROCEDURE phasenstatistik.rechnen;
BEGIN
  IF n = 0 THEN
  BEGIN
    mph  := 0;
    lvek := 0;
  END
  ELSE
  BEGIN
    mph := arctan(sy / sx);
    IF sx < 0 THEN mph := mph + pi;
    IF mph < 0 THEN mph := mph + 2 * pi;
    lvek                := sqrt(sqr(sx / n) + sqr(sy / n));
  END;
END;


PROCEDURE rauschen(VAR feld : databuffer; dauer : typeextended; VAR hoehe : sample);
CONST
  breite = 16;
  faktor = maxsample / 5000;
VAR
  verteilung : ARRAY[-5000..5000] OF bigint64;
  i, j, wol, wor, gesamt, bisher, jetzt : LONGINT;
BEGIN
  fillchar(verteilung, sizeof(verteilung), 0);
  gesamt := trunc(dauer);
  FOR i := 0 TO gesamt DO
    IF (feld[i] <= maxsample) AND (feld[i] >= minsample) THEN
      Inc(verteilung[round(feld[i] / faktor)]);
  bisher := 0;
  FOR i := -5000 TO 5000 DO
  BEGIN
    jetzt := 0;
    FOR j := max(-5000, i - breite) TO min(5000, i + breite) DO Inc(jetzt, verteilung[j]);
    IF jetzt >= bisher THEN
    BEGIN
      IF jetzt > bisher THEN
      BEGIN
        wol    := i;
        bisher := jetzt;
      END;
      wor := i;
    END;
  END;
  hoehe := round((wol + wor) / 2 * faktor);
END;

{ ampnormalhistogramm }

PROCEDURE ampnormalhistogramm.berechnen;
VAR
  um :    EXTENDED;
  tpr :   midint32;
  ywert : EXTENDED;
  nr :    BYTE;
BEGIN
  gesamt := tliste[ref]^.triggsum;
  IF gesamt = 0 THEN
  BEGIN
    Write('No trigger point');
    pieps;
    warte;
    abortion := True;
    exit;
  END;
  fillchar(diff^, sizeof(imagesbuffer), 0);
  tstat.init;
  um := 1 / gesamt;
  FOR nr := 1 TO filenr DO WITH tliste[ref]^.fil[nr] DO
    BEGIN
      openfile(nr);
      FOR tpr := 1 TO automn DO
      BEGIN
        ywert := dat(zwi(autom^[tpr]), xkanal);
        IF (kon(ywert, xkanal) < maxsamp + klasse) AND (kon(ywert, xkanal) > minsamp - klasse) THEN
        BEGIN
          tstat.dazu(ywert);
          incex(diff^[trunc((kon(ywert, xkanal) - minsamp) / klasse) + 1], um);
        END;
      END;
      schliesse;
    END;
  tstat.rechnen;
END;

PROCEDURE ampnormalhistogramm.image;
VAR
  rand :    WORD;
  einheit : einheitstring;
BEGIN
  INHERITED image;
  rand    := getmaxx DIV 2;
  einheit := belegungsliste[xkanal].einhwort;
  setcolor(getmaxcolor);
  settextjustify(lefttext, centertext);
  outtextxy(rand, 4, 'Trigger Points: ' + wort(gesamt));
  outtextxy(rand, 14, 'Evaluated     : ' + wort(tstat.n));
  outtextxy(rand, 24, 'Mean Amplitude: (' + extwort(extspannung(tstat.mx, xkanal), 1, 1) +
    'ñ' + extwort(extspannung(tstat.rox, xkanal), 1, 1) + ') ' + einheit);
  outtextxy(0, 4, '"AMPLITUDE HISTOGRAM"');
END;

PROCEDURE ampnormalhistogramm.plot(gr : BYTE);
BEGIN
  INHERITED plot(gr);
  Write(plt, kleinschr, plpa(diffn, 0), 'DI0,1;CP0,-2;');
  Write(plt, pllb('AMPLITUDE HISTOGRAM'), 'CP;CP;');
  Write(plt, pllb(' Trigger Points: ' + wort(gesamt)), 'CP;');
  Write(plt, pllb(' Evaluated     : ' + wort(tstat.n)), 'CP;');
  Write(plt, pllb(' Mean Amplitude: (' + extwort(extspannung(tstat.mx, xkanal), 1, 1) +
    ' +/- ' + extwort(extspannung(tstat.rox, xkanal), 1, 1) + ') '), belegungsliste[xkanal].plot, 'CP;');
END;

CONSTRUCTOR ampnormalhistogramm.construct(xk : BYTE; rtl : CHAR; dfn : WORD;
  ybe : EXTENDED; mins, maxs : sample);
BEGIN
  grafikamplitude.construct(xk, rtl, dfn, ybe, mins, maxs);
END;

{grafikaverage}

PROCEDURE grafikaverage.berechnen;
VAR
  i, j, tp, tptot : LONGINT;
  flaech :          EXTENDED;
  nr :              BYTE;
BEGIN
  tptot := 0;
  FOR nr := 1 TO filenr DO WITH tliste[tl]^.fil[nr] DO
      IF automda THEN
      BEGIN
        Write(#13);
        clreol;
        Write('Averaging: ', liste[nr].Name, ', total trigger point no.:');
        openfile(nr);
        FOR tp := 1 TO automn DO
        BEGIN
          FOR j := 0 TO maxchannelsandfilters DO IF j IN kanaele.dabei THEN FOR i := 0 TO rdauer DO
                mittel[j]^[i] := mittel[j]^[i] + dat(zwi(autom^[tp] + anfang + i), j);
          Inc(tptot);
          IF (tptot MOD 128) = 0 THEN Write(tptot : 9, #8#8#8#8#8#8#8#8#8);
          IF keypressed AND (readkey = #27) THEN
          BEGIN
            abortion := True;
            exit;
          END;
        END;
        schliesse;
      END;
  FOR j := 0 TO maxchannelsandfilters DO IF j IN kanaele.dabei THEN
      FOR i := 0 TO rdauer DO mittel[j]^[i] := mittel[j]^[i] / tpgesamt;
  WITH kanaele DO FOR j := 1 TO channelnumber DO
    BEGIN
      rauschen(mittel[k[j]]^, dauer, hoehe[j]);
      maxix[j] := 0;
      maxi[j]  := 0;
      FOR i := 0 TO trunc(dauer) DO IF maxi[j] < mittel[k[j]]^[i] THEN
        BEGIN
          maxi[j]  := mittel[k[j]]^[i];
          maxix[j] := i;
        END;
      flaech := 0;
      FOR i := 0 TO rdauer DO flaech := flaech + mittel[k[j]]^[i];
      flaeche[j] := (flaech / (rdauer + 1) - hoehe[j]) * zeit(rdauer + 1) / 1000;
    END;
END;

PROCEDURE grafikaverage.image;
VAR
  x, y, j :   LONGINT;
  fleinheit : einheittyp;
BEGIN
  WITH kanaele DO
  BEGIN
    grafikmittel.image;
    setlinestyle(dottedln, 0, normwidth);
    settextjustify(righttext, toptext);
    FOR j := 1 TO channelnumber DO
    BEGIN
      outtextxy(getmaxx, y0[j] - breite + 1, 'Base=' + extwort(extspannung(hoehe[j], k[j]), 1, 2) +
        ' ' + belegungsliste[k[j]].einhwort);
      y := y0[j] + round(kon(hoehe[j], k[j]) * faktor);
      line(lrand + 1, y, getmaxx, y);
      y := y0[j] + round(kon(maxi[j], k[j]) * faktor);
      x := round(maxix[j] / stauchung) + lrand + 1;
      outtextxy(getmaxx, y0[j] - breite + 9, 'Maximum=' + extwort(
        (maxi[j] - hoehe[j]) * belegungsliste[k[j]].faktor, 1, 2) + ' ' + belegungsliste[k[j]].einhwort);
      line(max(lrand + 1, x - 20), y, min(getmaxx, x + 20), y);
      fleinheit := belegungsliste[k[j]];
      Inc(fleinheit.sekunde);
      outtextxy(getmaxx, y0[j] - breite + 17, 'Area=' + extwort(flaeche[j] * belegungsliste[k[j]].faktor, 1, 3) +
        ' ' + fleinheit.einhwort);
    END;
    setlinestyle(solidln, 0, normwidth);
  END;
  settextjustify(lefttext, centertext);
  outtextxy(0, 4, '"AVERAGE"');
END;

PROCEDURE grafikaverage.plot(gr : BYTE);
VAR
  i :         LONGINT;
  fleinheit : einheittyp;
BEGIN
  grafikmittel.plot(gr);
  IF abortion THEN exit;
  WITH kanaele DO
  BEGIN
    Write(plt, kleinschr, 'DI0,1;', plpa(rdauer, 0), 'CP-2,-2;',
      pllb('AVERAGE'), 'CP-7,-1;',
      pllb('b'), 'CP-1,-1;', pllb('m'), 'CP-1,-1;', pllb('a'));
    FOR i := channelnumber DOWNTO 1 DO
    BEGIN
      fleinheit := belegungsliste[k[i]];
      Inc(fleinheit.sekunde);
      Write(plt, plpa(rdauer, fullsamplerange * (channelnumber - i)), 'CP0,-3;',
        pllb(extwort(extspannung(hoehe[i], k[i]), 1, 2) + ' '),
        belegungsliste[k[i]].plot, 'CP;',
        pllb(extwort((maxi[i] - hoehe[i]) * belegungsliste[k[i]].faktor, 1, 2) + ' '),
        belegungsliste[k[i]].plot, 'CP;',
        pllb(extwort(flaeche[i] * belegungsliste[k[i]].faktor, 1, 3) + ' '),
        fleinheit.plot);
    END;
  END;
END;

CONSTRUCTOR grafikaverage.construct(VAR kan : TChannelVolume; anf, dau : typeextended; trl : CHAR;
  trp : bigint64);
BEGIN
  grafikmittel.construct(kan, anf, dau, trl, trp);
END;

PROCEDURE grafikphasenaverage.berechnen;
VAR
  nr :              BYTE;
  i, j, tp, tptot : LONGINT;
  abst :            typeextended;
  dehnfaktor, flaech : EXTENDED;
BEGIN
  WITH triggweis, tliste[tl]^ DO
  BEGIN
    tptot := 0;
    FOR nr := 1 TO filenr DO WITH weisliste[nr], fil[nr] DO IF automda THEN
        BEGIN
          Write(#13);
          clreol;
          Write('Averaging: ', liste[nr].Name, ', total trigger point no.:');
          openfile(nr);
          FOR tp := 1 TO n DO
          BEGIN
            abst       := autom^[t^[tp] + 1] - autom^[t^[tp]];
            dehnfaktor := mittelabstand / abst;
            FOR j := 0 TO maxchannelsandfilters DO IF j IN kanaele.dabei THEN
                FOR i := 0 TO rdauer DO
                  mittel[j]^[i] := mittel[j]^[i] + dat(zwi(autom^[t^[tp]] + (anfang + i) / dehnfaktor), j);
            Inc(tptot);
            IF (tptot MOD 128) = 0 THEN Write(tptot : 9, #8#8#8#8#8#8#8#8#8);
            IF keypressed AND (readkey = #27) THEN
            BEGIN
              abortion := True;
              exit;
            END;
          END;
          schliesse;
        END;
    FOR j := 0 TO maxchannelsandfilters DO IF j IN kanaele.dabei THEN
        FOR i := 0 TO rdauer DO mittel[j]^[i] := mittel[j]^[i] / gesamt;
  END;
  WITH kanaele DO FOR j := 1 TO channelnumber DO
    BEGIN
      rauschen(mittel[k[j]]^, dauer, hoehe[j]);
      maxix[j] := 0;
      maxi[j]  := 0;
      FOR i := 0 TO trunc(dauer) DO IF maxi[j] < mittel[k[j]]^[i] THEN
        BEGIN
          maxi[j]  := mittel[k[j]]^[i];
          maxix[j] := i;
        END;
      flaech := 0;
      FOR i := 0 TO rdauer DO flaech := flaech + mittel[k[j]]^[i];
      flaeche[j] := (flaech / (rdauer + 1) - hoehe[j]) * zeit(rdauer + 1) / 1000;
    END;
END;

PROCEDURE grafikphasenaverage.image;
VAR
  x, y, j :   LONGINT;
  fleinheit : einheittyp;
BEGIN
  WITH kanaele DO
  BEGIN
    grafikmittel.image;
    setlinestyle(dottedln, 0, normwidth);
    settextjustify(righttext, toptext);
    FOR j := 1 TO channelnumber DO
    BEGIN
      outtextxy(getmaxx, y0[j] - breite + 1, 'Base=' + extwort(extspannung(hoehe[j], k[j]), 1, 2) +
        ' ' + belegungsliste[k[j]].einhwort);
      y := y0[j] + round(kon(hoehe[j], k[j]) * faktor);
      line(lrand + 1, y, getmaxx, y);
      y := y0[j] + round(kon(maxi[j], k[j]) * faktor);
      x := round(maxix[j] / stauchung) + lrand + 1;
      outtextxy(getmaxx, y0[j] - breite + 9, 'Maximum=' + extwort(
        (maxi[j] - hoehe[j]) * belegungsliste[k[j]].faktor, 1, 2) + ' ' + belegungsliste[k[j]].einhwort);
      line(max(lrand + 1, x - 20), y, min(getmaxx, x + 20), y);
      fleinheit := belegungsliste[k[j]];
      Inc(fleinheit.sekunde);
      outtextxy(getmaxx, y0[j] - breite + 17, 'Area=' + extwort(flaeche[j] * belegungsliste[k[j]].faktor, 1, 3) +
        ' ' + fleinheit.einhwort);
    END;
    setlinestyle(solidln, 0, normwidth);
  END;
  settextjustify(lefttext, centertext);
  outtextxy(0, 4, '"PHASE DEPENDENT AVERAGE"');
END;

PROCEDURE grafikphasenaverage.plot(gr : BYTE);
VAR
  i :         LONGINT;
  fleinheit : einheittyp;
BEGIN
  grafikmittel.plot(gr);
  IF abortion THEN exit;
  WITH kanaele DO
  BEGIN
    Write(plt, kleinschr, 'DI0,1;', plpa(rdauer, 0), 'CP-2,-2;',
      pllb('PHASE DEPENDENT AVERAGE'), 'CP-23,-1;',
      pllb('b'), 'CP-1,-1;', pllb('m'), 'CP-1,-1;', pllb('a'));
    FOR i := channelnumber DOWNTO 1 DO
    BEGIN
      fleinheit := belegungsliste[k[i]];
      Inc(fleinheit.sekunde);
      Write(plt, plpa(rdauer, fullsamplerange * (channelnumber - i)), 'CP0,-3;',
        pllb(extwort(extspannung(hoehe[i], k[i]), 1, 2) + ' '),
        belegungsliste[k[i]].plot, 'CP;',
        pllb(extwort((maxi[i] - hoehe[i]) * belegungsliste[k[i]].faktor, 1, 2) + ' '),
        belegungsliste[k[i]].plot, 'CP;',
        pllb(extwort(flaeche[i] * belegungsliste[k[i]].faktor, 1, 3) + ' '),
        fleinheit.plot);
    END;
  END;
END;

CONSTRUCTOR grafikphasenaverage.construct(VAR kan : TChannelVolume; anf, dau : typeextended;
  trl : CHAR; trp : bigint64; VAR weis : triggerweiser);
BEGIN
  triggweis := weis;
  grafikmittel.construct(kan, anf, dau, trl, trp);
END;

CONSTRUCTOR intervallhistogramm.construct(rtl : CHAR; anf, sch : typeextended; din : WORD;
  ybe : EXTENDED);
BEGIN
  grafikintervall.construct(rtl, anf, sch, din, ybe);
END;

PROCEDURE intervallhistogramm.berechnen;
VAR
  um :  EXTENDED;
  tpr : LONGINT;
  d :   typeextended;
  nr :  BYTE;
BEGIN
  gesamt := tliste[ref]^.triggsum;
  IF gesamt = 0 THEN
  BEGIN
    Write('No trigger point');
    pieps;
    warte;
    abortion := True;
    exit;
  END;
  fillchar(diff^, sizeof(imagesbuffer), 0);
  tstat.init;
  um := 1 / gesamt;
  FOR nr := 1 TO filenr DO WITH tliste[ref]^.fil[nr] DO
      FOR tpr := 1 TO automn - 1 DO
      BEGIN
        d := autom^[tpr + 1] - autom^[tpr];
        IF (d < schluss) AND (d >= anfang) THEN
        BEGIN
          tstat.dazu(d);
          incex(diff^[trunc((d - anfang) / klasse) + 1], um);
        END;
      END;
  tstat.rechnen;
END;

PROCEDURE intervallhistogramm.image;
VAR
  rand : WORD;
BEGIN
  grafikintervall.image;
  rand := getmaxx DIV 2;
  setcolor(getmaxcolor);
  settextjustify(lefttext, centertext);
  outtextxy(rand, 4, 'Trigger Points: ' + wort(gesamt));
  outtextxy(rand, 14, 'Evaluated     : ' + wort(tstat.n));
  outtextxy(rand, 24, 'Mean Interval : ' + extwort(extzeit(tstat.mx), 3, 3) + ' ms ñ ' +
    extwort(extzeit(tstat.rox), 3, 3) + ' ms');
  outtextxy(0, 4, '"INTERVAL HISTOGRAM"');
END;

PROCEDURE intervallhistogramm.plot(gr : BYTE);
BEGIN
  grafikintervall.plot(gr);
  Write(plt, kleinschr, plpa(diffn, 0), 'DI0,1;CP0,-2;');
  Write(plt, pllb('INTERVAL HISTOGRAM'), 'CP;CP;');
  Write(plt, pllb(' Trigger Points: ' + wort(gesamt)), 'CP;');
  Write(plt, pllb(' Evaluated     : ' + wort(tstat.n)), 'CP;');
  Write(plt, pllb(' Mean Interval : ' + extwort(extzeit(tstat.mx), 3, 3) + ' ms +/- ' +
    extwort(extzeit(tstat.rox), 3, 3) + ' ms'));
END;

CONSTRUCTOR autokorrelogramm.construct(rtl : CHAR; anf, sch : typeextended; din : WORD;
  ybe : EXTENDED);
BEGIN
  grafikintervall.construct(rtl, anf, sch, din, ybe);
END;

PROCEDURE autokorrelogramm.berechnen;
VAR
  um :              EXTENDED;
  tpr, tpo :        LONGINT;
  tpol, tpor, dum : midint32;
  d :               typeextended;
  nr :              BYTE;
BEGIN
  gesamt := tliste[ref]^.triggsum;
  IF gesamt = 0 THEN
  BEGIN
    Write('No trigger point');
    pieps;
    warte;
    abortion := True;
    exit;
  END;
  fillchar(diff^, sizeof(imagesbuffer), 0);
  tstat.init;
  um := 1 / gesamt;
  WITH tliste[ref]^ DO FOR nr := 1 TO filenr DO WITH fil[nr] DO
        IF automda THEN
        BEGIN
          Write(#13);
          clreol;
          Write('Processing: ', liste[nr].Name, ', trigger point no.:');
          FOR tpr := 1 TO automn DO
          BEGIN
            such(0, automn + 1, autom^[tpr] + anfang, dum, tpol);
            such(dum, automn + 1, autom^[tpr] - 1 + schluss, tpor, dum);
            FOR tpo := tpol TO tpor DO
            BEGIN
              d := autom^[tpo] - autom^[tpr];
              tstat.dazu(d);
              incex(diff^[trunc((d - anfang) / klasse) + 1], um);
            END;
            IF (tpr MOD 128) = 0 THEN Write(tpr : 7, #8#8#8#8#8#8#8);
          END;
        END;
  tstat.rechnen;
END;

PROCEDURE autokorrelogramm.image;
VAR
  rand : WORD;
BEGIN
  grafikintervall.image;
  rand := getmaxx DIV 2;
  setcolor(getmaxcolor);
  settextjustify(lefttext, centertext);
  outtextxy(rand, 4, 'Trigger Points : ' + wort(gesamt));
  outtextxy(rand, 14, 'Evaluated      : ' + wort(tstat.n));
  outtextxy(rand, 24, 'Mean Time      : ' + extwort(extzeit(tstat.mx), 3, 3) + ' ms ñ ' +
    extwort(extzeit(tstat.rox), 3, 3) + ' ms');
  outtextxy(0, 4, '"AUTO CORRELOGRAM"');
END;

PROCEDURE autokorrelogramm.plot(gr : BYTE);
BEGIN
  grafikintervall.plot(gr);
  Write(plt, kleinschr, plpa(diffn, 0), 'DI0,1;CP0,-2;');
  Write(plt, pllb('AUTO CORRELOGRAM'), 'CP;CP;');
  Write(plt, pllb(' Trigger Points: ' + wort(gesamt)), 'CP;');
  Write(plt, pllb(' Evaluated     : ' + wort(tstat.n)), 'CP;');
  Write(plt, pllb(' Mean Time     : ' + extwort(extzeit(tstat.mx), 3, 3) + ' ms +/- ' +
    extwort(extzeit(tstat.rox), 3, 3) + ' ms'));
END;

CONSTRUCTOR kreuzkorrelogramm.construct(rtl, otl : CHAR; anf, sch : typeextended; din : WORD;
  ybe : EXTENDED);
BEGIN
  obj := otl;
  grafikintervall.construct(rtl, anf, sch, din, ybe);
END;

PROCEDURE kreuzkorrelogramm.berechnen;
VAR
  um :              EXTENDED;
  tpr, tpo :        LONGINT;
  tpol, tpor, dum : midint32;
  d :               typeextended;
  nr :              BYTE;
BEGIN
  refn := tliste[ref]^.triggsum;
  objn := tliste[obj]^.triggsum;
  IF (refn = 0) OR (objn = 0) THEN
  BEGIN
    Write('Empty trigger list.');
    pieps;
    warte;
    abortion := True;
    exit;
  END;
  fillchar(diff^, sizeof(imagesbuffer), 0);
  nstat.init;
  tstat.init;
  um := 1 / refn;
  FOR nr := 1 TO filenr DO IF tliste[ref]^.fil[nr].automda THEN
      WITH tliste[obj]^, fil[nr] DO
      BEGIN
        Write(#13);
        clreol;
        Write('Processing: ', liste[nr].Name, ', trigger point no.:');
        FOR tpr := 1 TO tliste[ref]^.fil[nr].automn DO
        BEGIN
          such(0, automn + 1, tliste[ref]^.fil[nr].autom^[tpr] + anfang, dum, tpol);
          such(dum, automn + 1, tliste[ref]^.fil[nr].autom^[tpr] - 1 + schluss, tpor, dum);
          nstat.dazu(tpor + 1 - tpol);
          FOR tpo := tpol TO tpor DO
          BEGIN
            d := autom^[tpo] - tliste[ref]^.fil[nr].autom^[tpr];
            tstat.dazu(d);
            incex(diff^[trunc((d - anfang) / klasse) + 1], um);
          END;
          IF (tpr MOD 128) = 0 THEN Write(tpr : 7, #8#8#8#8#8#8#8);
        END;
      END;
  nstat.rechnen;
  tstat.rechnen;
END;

PROCEDURE kreuzkorrelogramm.image;
VAR
  rand : WORD;
BEGIN
  grafikintervall.image;
  rand := getmaxx DIV 2;
  setcolor(getmaxcolor);
  settextjustify(lefttext, centertext);
  outtextxy(rand, 4, 'References: ' + wort(refn));
  outtextxy(rand, 14, 'Events    : ' + wort(round(nstat.sx)) + ' (' + wort(objn) + ')');
  outtextxy(rand, 24, 'Evt./Ref. : ' + extwort(nstat.mx, 3, 1) + ' ñ ' + extwort(nstat.rox, 3, 1));
  outtextxy(rand, 34, 'Mean Time : ' + extwort(extzeit(tstat.mx), 3, 3) + ' ms ñ ' +
    extwort(extzeit(tstat.rox), 3, 3) + ' ms');
  outtextxy(0, 4, '"CROSS CORRELOGRAM"');
END;

PROCEDURE kreuzkorrelogramm.plot(gr : BYTE);
BEGIN
  grafikintervall.plot(gr);
  Write(plt, kleinschr, plpa(diffn, 0), 'DI0,1;CP0,-2;');
  Write(plt, pllb('CROSS CORRELOGRAM'), 'CP;CP;');
  Write(plt, pllb(' References    : ' + wort(refn)) + 'CP;');
  Write(plt, pllb(' Events        : ' + wort(round(nstat.sx)) + ' (' + wort(objn) + ')') + 'CP;');
  Write(plt, pllb(' Evt./Ref.     : ' + extwort(nstat.mx, 3, 1) + ' +/- ' + extwort(nstat.rox, 3, 1)) + 'CP;');
  Write(plt, pllb(' Mean Time     : ' + extwort(extzeit(tstat.mx), 3, 3) + ' ms +/- ' +
    extwort(extzeit(tstat.rox), 3, 3) + ' ms'));
END;

CONSTRUCTOR psthistogramm.construct(rtl, otl : CHAR; aph, eph : SHORTINT; anf, sch : typeextended;
  din : WORD; ybe : EXTENDED);
BEGIN
  obj   := otl;
  anfph := aph;
  endph := eph;
  grafikintervall.construct(rtl, anf, sch, din, ybe);
END;

PROCEDURE psthistogramm.berechnen;
VAR
  um : EXTENDED;
  nr : BYTE;

  PROCEDURE durchzaehlen(VAR refliste, objliste : triggerdaten);
  VAR
    tpol1, tpol2, tpor1, tpor2, tpol, tpor, dum : midint32;
    tpr, tpo : LONGINT;
    d :        typeextended;
  BEGIN
    FOR tpr := 1 TO refliste.automn DO
    BEGIN
      objliste.such(0, objliste.automn + 1, refliste.autom^[tpr] + anfang, dum, tpol1);
      objliste.such(dum, objliste.automn + 1, refliste.autom^[tpr] - 1 + schluss, tpor1, dum);
      objliste.such(0, objliste.automn + 1, refliste.autom^[tpr + anfph], dum, tpol2);
      objliste.such(dum, objliste.automn + 1, refliste.autom^[tpr + endph] - 1, tpor2, dum);
      tpor := min(tpor1, tpor2);
      tpol := max(tpol1, tpol2);
      nstat.dazu(max(tpor + 1 - tpol, 0));
      FOR tpo := tpol TO tpor DO
      BEGIN
        d := objliste.autom^[tpo] - refliste.autom^[tpr];
        tstat.dazu(d);
        incex(diff^[trunc((d - anfang) / klasse) + 1], um);
      END;
      IF (tpr MOD 128) = 0 THEN Write(tpr : 7, #8#8#8#8#8#8#8);
    END;
  END;

BEGIN
  refn := tliste[ref]^.triggsum;
  objn := tliste[obj]^.triggsum;
  IF (refn = 0) OR (objn = 0) THEN
  BEGIN
    Write('Empty trigger list.');
    pieps;
    warte;
    abortion := True;
    exit;
  END;
  fillchar(diff^, sizeof(imagesbuffer), 0);
  nstat.init;
  tstat.init;
  um := 1 / refn;
  FOR nr := 1 TO filenr DO IF tliste[ref]^.fil[nr].automda THEN
    BEGIN
      Write(#13);
      clreol;
      Write('Processing: ', liste[nr].Name, ', trigger point no.:');
      durchzaehlen(tliste[ref]^.fil[nr], tliste[obj]^.fil[nr]);
    END;
  nstat.rechnen;
  tstat.rechnen;
END;

PROCEDURE psthistogramm.image;
VAR
  rand : WORD;
BEGIN
  grafikintervall.image;
  rand := getmaxx DIV 2;
  setcolor(getmaxcolor);
  settextjustify(lefttext, centertext);
  outtextxy(rand, 4, 'References: ' + wort(refn));
  outtextxy(rand, 14, 'Events    : ' + wort(round(nstat.sx)) + ' (' + wort(objn) + ')');
  outtextxy(rand, 24, 'Evt./Ref. : ' + extwort(nstat.mx, 3, 1) + ' ñ ' + extwort(nstat.rox, 3, 1));
  outtextxy(rand, 34, 'Mean Time : ' + extwort(extzeit(tstat.mx), 3, 3) + ' ms ñ ' +
    extwort(extzeit(tstat.rox), 3, 3) + ' ms');
  outtextxy(0, 4, '"PST HISTOGRAM"');
END;

PROCEDURE psthistogramm.plot(gr : BYTE);
BEGIN
  grafikintervall.plot(gr);
  Write(plt, kleinschr, plpa(diffn, 0), 'DI0,1;CP0,-2;');
  Write(plt, pllb('PST HISTOGRAM'), 'CP;CP;');
  Write(plt, pllb(' References    : ' + wort(refn)) + 'CP;');
  Write(plt, pllb(' Events        : ' + wort(round(nstat.sx)) + ' (' + wort(objn) + ')') + 'CP;');
  Write(plt, pllb(' Evt./Ref.     : ' + extwort(nstat.mx, 3, 1) + ' +/- ' + extwort(nstat.rox, 3, 1)) + 'CP;');
  Write(plt, pllb(' Mean Time     : ' + extwort(extzeit(tstat.mx), 3, 3) + ' ms +/- ' +
    extwort(extzeit(tstat.rox), 3, 3) + ' ms'));
END;

CONSTRUCTOR latenzhistogramm.construct(rtl, otl : CHAR; anf, sch : typeextended; din : WORD;
  ybe : EXTENDED);
BEGIN
  obj := otl;
  grafikintervall.construct(rtl, anf, sch, din, ybe);
END;

PROCEDURE latenzhistogramm.berechnen;
VAR
  um :       EXTENDED;
  tpr :      LONGINT;
  tpo, dum : midint32;
  d :        typeextended;
  nr :       BYTE;
BEGIN
  refn := tliste[ref]^.triggsum;
  IF (refn = 0) OR (tliste[obj]^.triggsum = 0) THEN
  BEGIN
    Write('Empty trigger list.');
    pieps;
    warte;
    abortion := True;
    exit;
  END;
  fillchar(diff^, sizeof(imagesbuffer), 0);
  tstat.init;
  um := 1 / refn;
  FOR nr := 1 TO filenr DO IF tliste[ref]^.fil[nr].automda THEN
      WITH tliste[obj]^, fil[nr] DO
      BEGIN
        Write(#13);
        clreol;
        Write('Processing: ', liste[nr].Name, ', trigger point no.:');
        FOR tpr := 1 TO tliste[ref]^.fil[nr].automn DO
        BEGIN
          such(0, automn + 1, tliste[ref]^.fil[nr].autom^[tpr] + anfang, dum, tpo);
          d := autom^[tpo] - tliste[ref]^.fil[nr].autom^[tpr];
          IF d <= schluss THEN
          BEGIN
            tstat.dazu(d);
            incex(diff^[trunc((d - anfang) / klasse) + 1], um);
          END;
          IF (tpr MOD 128) = 0 THEN Write(tpr : 7, #8#8#8#8#8#8#8);
        END;
      END;
  tstat.rechnen;
END;

PROCEDURE latenzhistogramm.image;
VAR
  rand : WORD;
BEGIN
  grafikintervall.image;
  rand := getmaxx DIV 2;
  setcolor(getmaxcolor);
  settextjustify(lefttext, centertext);
  outtextxy(rand, 4, 'References: ' + wort(refn));
  outtextxy(rand, 14, 'Events    : ' + wort(tstat.n));
  outtextxy(rand, 24, 'Latency   : ' + extwort(extzeit(tstat.mx), 3, 3) + ' ms ñ ' +
    extwort(extzeit(tstat.rox), 3, 3) + ' ms');
  outtextxy(0, 4, '"LATENCY HISTOGRAM"');
END;

PROCEDURE latenzhistogramm.plot(gr : BYTE);
BEGIN
  grafikintervall.plot(gr);
  Write(plt, kleinschr, plpa(diffn, 0), 'DI0,1;CP0,-2;');
  Write(plt, pllb('LATENCY HISTOGRAM'), 'CP;CP;');
  Write(plt, pllb(' References    : ' + wort(refn)), 'CP;');
  Write(plt, pllb(' Events        : ' + wort(tstat.n)), 'CP;');
  Write(plt, pllb(' Latency       : ' + extwort(extzeit(tstat.mx), 3, 3) + ' ms +/- ' +
    extwort(extzeit(tstat.rox), 3, 3) + ' ms'));
END;

{ phasenhistogramm }

CONSTRUCTOR phasenhistogramm.construct(rtl, otl : CHAR; minabst, maxabst : typeextended;
  anfph, schph : EXTENDED; din : WORD; ybe : EXTENDED);
BEGIN
  obj := otl;
  weis.zaehlen(tliste[rtl]^, minabst, maxabst);
  INHERITED construct(rtl, anfph, 1 - frac(1 - frac(schph - anfph + 1)), din, ybe);
  weis.frei;
END;

PROCEDURE phasenhistogramm.berechnen;
VAR
  mklasse, um, faktor : EXTENDED;
  tpr, tpo, versch : LONGINT;
  tpol, tpor, dum : midint32;
  nr :      BYTE;
  abst, d : typeextended;
BEGIN
  mklasse := weis.mittelabstand * klasse;
  IF weis.gesamt = 0 THEN
  BEGIN
    Write('No trigger point.');
    pieps;
    warte;
    abortion := True;
    exit;
  END;
  refn := tliste[ref]^.triggsum;
  objn := tliste[obj]^.triggsum;
  fillchar(diff^, sizeof(imagesbuffer), 0);
  versch   := round(anfphase * diffnganz);
  anfphase := versch / diffnganz;
  nstat.init;
  phstat.init;
  um := 1 / weis.gesamt;
  FOR nr := 1 TO filenr DO
    WITH tliste[ref]^.fil[nr], weis.weisliste[nr] DO IF automda THEN
      BEGIN
        Write(#13);
        clreol;
        FOR tpr := 1 TO n DO
        BEGIN
          abst   := autom^[t^[tpr] + 1] - autom^[t^[tpr]];
          faktor := weis.mittelabstand / abst;
          tliste[obj]^.fil[nr].such(0, tliste[obj]^.fil[nr].automn + 1,
            autom^[t^[tpr]], dum, tpol);
          tliste[obj]^.fil[nr].such(dum, tliste[obj]^.fil[nr].automn + 1,
            autom^[t^[tpr]] + abst - 1, tpor, dum);
          nstat.dazu(tpor + 1 - tpol);
          FOR tpo := tpol TO tpor DO
          BEGIN
            d := tliste[obj]^.fil[nr].autom^[tpo] - autom^[t^[tpr]];
            IF phasentest(d / abst, anfphase, dauerphase) THEN phstat.dazu(d / abst * 2 * pi);
            incex(diff^[(trunc(faktor * d / mklasse) - versch + diffnganz) MOD diffnganz + 1], um);
          END;
        END;
      END;
  nstat.rechnen;
  phstat.rechnen;
END;

PROCEDURE phasenhistogramm.image;
VAR
  rand : WORD;
BEGIN
  INHERITED image;
  rand := getmaxx DIV 2;
  setcolor(getmaxcolor);
  settextjustify(lefttext, centertext);
  outtextxy(rand, 4, 'Ref.þ Evt.: ' + wort(weis.gesamt) + ' (' + wort(refn) + ') þ ' +
    wort(round(nstat.sx)) + ' (' + wort(objn) + ')');
  outtextxy(rand, 14, 'Evt./Ref. : ' + extwort(nstat.mx, 3, 1) + ' ñ ' + extwort(nstat.rox, 3, 1));
  outtextxy(rand, 24, 'Cycle Dur.: ' + extwort(extzeit(weis.mittelabstand), 3, 3) + ' ms');
  outtextxy(rand, 34, 'Phase     : ' + extwort(phstat.mph / 2 / pi, 3, 3) + ' (l=' + extwort(phstat.lvek, 4, 2) + ')');
  outtextxy(0, 4, '"PHASE HISTOGRAM"');
END;

PROCEDURE phasenhistogramm.plot(gr : BYTE);
BEGIN
  INHERITED plot(gr);
  Write(plt, kleinschr, plpa(diffn, 0), 'DI0,1;CP0,-2;');
  Write(plt, pllb('PHASE HISTOGRAM'), 'CP;CP;');
  Write(plt, pllb(' Ref., Evt.    : ' + wort(weis.gesamt) + ' (' + wort(refn) + ') , ' +
    wort(round(nstat.sx)) + ' (' + wort(objn) + ')') + 'CP;');
  Write(plt, pllb(' Evt./Ref.     : ' + extwort(nstat.mx, 3, 1) + ' +/- ' + extwort(nstat.rox, 3, 1)) + 'CP;');
  Write(plt, pllb(' Cycle Dur.    : ' + extwort(extzeit(weis.mittelabstand), 3, 3) + ' ms') + 'CP;');
  Write(plt, pllb(' Phase         : ' + extwort(phstat.mph / 2 / pi, 3, 3) + ' (l=' +
    extwort(phstat.lvek, 4, 2) + ')'));
END;

END.
