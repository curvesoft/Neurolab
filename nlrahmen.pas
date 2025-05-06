{ Borland-Pascal 7.0 / FPC 3.2.2 }
{$ifdef fpc} {$mode TP} {$endif}

UNIT  nlrahmen;

{$ifdef fpc} {$R *.res} {$endif}

{$IFDEF MSDOS}
{$A+,B-,E+,F-,G-,I-,N+,O-,P+,T+,V+,X-}
{$ELSE}
{$A+,B-,E+,F-,G+,I-,N+,P+,T+,V+,X-}
{$ENDIF}

INTERFACE

USES  crt,
  bequem, tlfilter, tlfiles;

CONST
  farbenormal = lightgray;
  farbe1      = cyan;
  farbe2      = green;
  farbe3      = magenta;
  farbe4      = magenta;

TYPE
  TChannelVolume = OBJECT
    channelnumber :    BYTE;
    k :     ARRAY [1..maxchannelsandfilters] OF BYTE;
    dabei : SET OF 0..maxchannelsandfilters;
    PROCEDURE presetup;
    FUNCTION ausgabe : string80;
    PROCEDURE read(enn : BYTE; farbe : BYTE);
  END;

  filterliste = OBJECT
    ende :  BOOLEAN;
    index : BYTE;
    PROCEDURE zeigen(a, i : BYTE);
    PROCEDURE weiterzeigen;
  PRIVATE
    anz : BYTE;
    zn :  BYTE;
  END;

VAR
  zeilmax : BYTE;
  kanaele : TChannelVolume;

PROCEDURE zwischen(titel : string20; farbe : BYTE);
PROCEDURE showtitle(gross : BOOLEAN; schrift : string80; titel : string20; farbe : BYTE);

PROCEDURE belegungzeigen;

IMPLEMENTATION

CONST
  strich =
    '-------------------------------------------------------------------------------';

PROCEDURE zwischen(titel : string20; farbe : BYTE);
VAR
  wy : BYTE;
BEGIN
  textcolor(farbe);
  titel := ' ' + titel + ' ';
  wy    := wherey;
  Write(strich);
  gotoxy(4, wy);
  writeln(titel);
  textcolor(farbenormal);
END;

PROCEDURE showtitle(gross : BOOLEAN; schrift : string80; titel : string20; farbe : BYTE);
BEGIN
  IF gross <> (zeilmax > 25) THEN
  BEGIN
    IF gross THEN textmode(c80 + font8x8)
    ELSE
      textmode(c80);
    zeilmax := hi(windmax) + 1;
  END;
  window(1, 1, 80, zeilmax);
  clrscr;
  textcolor(farbe);
  Write(strich);
  schrift := ' ' + schrift + ' ';
  titel   := ' ' + titel + ' ';
  gotoxy(40 - length(schrift) DIV 2, 1);
  Write(schrift);
  gotoxy(4, 1);
  Write(titel);
  window(1, 3, 80, zeilmax);
  textcolor(farbenormal);
END;

PROCEDURE belegungzeigen;
VAR
  i : BYTE;
BEGIN
  writeln('Channel' : 8, 'Label' : 7);
  FOR i := 0 TO 7 DO
  BEGIN
    IF i < kan THEN Write(i : 2, ' : ', schriftliste[i] : 10, '' : 25);
    IF i + 8 < kan THEN Write(i + 8 : 2, ' : ', schriftliste[i + 8]);
    writeln;
  END;
END;

PROCEDURE filterliste.zeigen(a, i : BYTE);
BEGIN
  anz := a;
  writeln('Channel' : 5, 'Filters' : 12);
  zn    := wherey + hi(windmin);
  index := i;
  gotoxy(1, wherey + anz);
  weiterzeigen;
END;

PROCEDURE filterliste.weiterzeigen;
LABEL
  voll;
VAR
  windminalt, windmaxalt : WORD;
  wy, wx : BYTE;
BEGIN
  wy         := wherey;
  wx         := wherex;
  windminalt := windmin;
  windmaxalt := windmax;
  ende       := False;
  window(1, zn, 80, zn + anz);
  clrscr;
  WHILE NOT ende DO
  BEGIN
    IF index >= kan + maxfilters - 1 THEN ende := True;
    IF filterdrin(index) THEN
    BEGIN
      IF wherey >= anz THEN GOTO voll;
      writeln(index : 2, ' : ', filterzeile(index));
    END;
    Inc(index);
  END;
  voll :
    window(lo(windminalt) + 1, hi(windminalt) + 1, lo(windmaxalt) + 1, hi(windmaxalt) + 1);
  gotoxy(wx, wy);
  IF ende THEN index := kan;
END;

PROCEDURE TChannelVolume.presetup;
VAR
  i : BYTE;
BEGIN
  FOR i := 1 TO kan DO k[i] := pred(i);
  channelnumber := kan;
END;

FUNCTION TChannelVolume.ausgabe : string80;
VAR
  puffer : string80;
  i :      BYTE;
BEGIN
  puffer := '';
  FOR i := 1 TO channelnumber DO puffer := puffer + wort(k[i]) + ' ';
  ausgabe := puffer;
END;

PROCEDURE TChannelVolume.read(enn : BYTE; farbe : BYTE);
VAR
  i :          BYTE;
  puffer, ts : string80;
  tk :         LONGINT;
  liste :      filterliste;

  PROCEDURE fehler(ausgabetext : STRING);
  BEGIN
    bequem.fehler(ausgabetext);
    zoeger(3000);
    writeln;
  END;

BEGIN
  belegungzeigen;
  writeln;
  liste.zeigen(enn, kan);
  writeln;
  zwischen('Dialogue', farbe);
  Write(lfcr, 'Continue list? (Y/N) ');
  WHILE NOT liste.ende AND (readkey IN ['Y', 'y', 'J', 'j']) DO liste.weiterzeigen;
  Write(#13);
  clreol;
  puffer := readstring('Channels', ausgabe);
  channelnumber     := 0;
  i      := 1;
  WHILE (i <= length(puffer)) AND (channelnumber <= maxchannelsandfilters) DO
  BEGIN
    ts := '';
    WHILE NOT (puffer[i] IN ['0'..'9']) DO
      IF i > length(puffer) THEN exit
      ELSE
        Inc(i);
    WHILE (puffer[i] IN ['0'..'9']) AND (i <= length(puffer)) DO
    BEGIN
      ts := ts + puffer[i];
      Inc(i);
    END;
    IF length(ts) <= 20 THEN tk := zahl(ts)
    ELSE
      tk := -1;
    IF tk IN [0..kan + maxfilters - 1] THEN
      IF (tk < kan) OR (filterdrin(tk)) THEN
      BEGIN
        Inc(channelnumber);
        k[channelnumber] := tk;
      END
      ELSE
        fehler('Channel ' + wort(tk) + ' not defined.')
    ELSE
      fehler('Channel ' + wort(tk) + ' not existing.');
  END;
  dabei := [];
  FOR i := 1 TO channelnumber DO dabei := dabei + [k[i]];
END;

BEGIN
  zeilmax := hi(windmax) + 1;
END.
