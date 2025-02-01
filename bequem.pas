{ Borland-Pascal 7.0 / FPC 3.2.2 }
{$ifdef fpc} {$mode TP} {$endif}

UNIT  bequem;

{$IFDEF MSDOS}
{$A+,B-,E+,F-,G-,I-,N+,O-,P+,T+,V+,X+}
{$ELSE}
{$A+,B-,E+,F-,G+,I-,N+,P+,T+,V+,X+}
{$ENDIF}

INTERFACE

USES  crt;

CONST
  lfcr = #10#13;

  ln10 = 2.302585093;

TYPE
  string8  = STRING[8];
  string20 = STRING[20];
  string80 = STRING[80];
  {$ifdef fpc} bigint64=int64; midint32=longint; {$else} grossint = LONGINT;
  exword   = WORD; {$endif}

FUNCTION min(a, b : bigint64) : bigint64;
FUNCTION max(a, b : bigint64) : bigint64;
FUNCTION mini(a, b : INTEGER) : INTEGER;
FUNCTION maxi(a, b : INTEGER) : INTEGER;
FUNCTION mine(a, b : EXTENDED) : EXTENDED;
FUNCTION maxe(a, b : EXTENDED) : EXTENDED;

FUNCTION log(x : EXTENDED) : EXTENDED;
FUNCTION pot(x : SHORTINT) : bigint64;
FUNCTION xpot(x : BYTE) : EXTENDED;

PROCEDURE incex(VAR a : EXTENDED; b : EXTENDED);

FUNCTION zahl(zahlstr : string20) : bigint64;
FUNCTION wort(zahl : bigint64) : string20;
FUNCTION extwort(zahl : EXTENDED; l, n : BYTE) : string20;
FUNCTION extewort(zahl : EXTENDED; a, b : BYTE) : string20;
FUNCTION extfwort(zahl : EXTENDED; a : BYTE) : string20;
PROCEDURE schieben(VAR puffer : STRING);
PROCEDURE kompri(VAR puffer : STRING);
FUNCTION kleinbuchstaben(puffer : STRING) : STRING;

PROCEDURE laerman;
PROCEDURE laermaus;
PROCEDURE piep;
PROCEDURE pieps;

PROCEDURE fehler(Text : string80);
FUNCTION lesefehler : BOOLEAN;

PROCEDURE warte;
PROCEDURE zoeger(ms : WORD);
FUNCTION fileschonda(filename : string80) : BOOLEAN;

FUNCTION readint(Text : string80; sonst : bigint64) : bigint64;
FUNCTION readext(Text : string80; sonst : EXTENDED; l, n : BYTE) : EXTENDED;
FUNCTION readexte(Text : string80; sonst : EXTENDED; a, b : BYTE) : EXTENDED;
FUNCTION readchar(Text : string80; sonst : CHAR) : CHAR;
FUNCTION readcharim(Text : string80; sonst : CHAR) : CHAR;
FUNCTION readstring(Text : string80; sonst : string80) : string80;

IMPLEMENTATION

CONST
  laerm : BOOLEAN = False;

FUNCTION min(a, b : bigint64) : bigint64;
BEGIN
  IF a < b THEN min := a
  ELSE
    min             := b;
END;

FUNCTION max(a, b : bigint64) : bigint64;
BEGIN
  IF a < b THEN max := b
  ELSE
    max             := a;
END;

FUNCTION mini(a, b : INTEGER) : INTEGER;
BEGIN
  IF a < b THEN mini := a
  ELSE
    mini             := b;
END;

FUNCTION maxi(a, b : INTEGER) : INTEGER;
BEGIN
  IF a < b THEN maxi := b
  ELSE
    maxi             := a;
END;

FUNCTION mine(a, b : EXTENDED) : EXTENDED;
BEGIN
  IF a < b THEN mine := a
  ELSE
    mine             := b;
END;

FUNCTION maxe(a, b : EXTENDED) : EXTENDED;
BEGIN
  IF a < b THEN maxe := b
  ELSE
    maxe             := a;
END;

FUNCTION log(x : EXTENDED) : EXTENDED;
BEGIN
  log := ln(x) / ln10;
END;

FUNCTION pot(x : SHORTINT) : bigint64;
BEGIN
  pot := round(exp(x * ln10));
END;

FUNCTION xpot(x : BYTE) : EXTENDED;
BEGIN
  xpot := int(exp(x * ln10) + 0.5);
END;

PROCEDURE incex(VAR a : EXTENDED; b : EXTENDED);
BEGIN
  a := a + b;
END;

FUNCTION zahl(zahlstr : string20) : bigint64;
VAR
  zahlint : EXTENDED;
  kont :    INTEGER;
BEGIN
  val(zahlstr, zahlint, kont);
  zahl := round(zahlint);
END;

FUNCTION wort(zahl : bigint64) : string20;
VAR
  zahlstr : string20;
BEGIN
  str(zahl, zahlstr);
  wort := zahlstr;
END;

FUNCTION extwort(zahl : EXTENDED; l, n : BYTE) : string20;
VAR
  zahlstr : string20;
BEGIN
  str(zahl : l : n, zahlstr);
  extwort := zahlstr;
END;

FUNCTION extewort(zahl : EXTENDED; a, b : BYTE) : string20;
VAR
  zahlstr : string20;
  i :       BYTE;
BEGIN
  str(zahl : 9 + a, zahlstr);
  insert(' ', zahlstr, 4 + a);
  Delete(zahlstr, 9, 4 - b);
  FOR i := 1 TO b - 1 DO IF zahlstr[7 + a] = '0' THEN
    BEGIN
      Delete(zahlstr, 7 + a, 1);
      zahlstr := zahlstr + ' ';
    END;
  extewort := zahlstr;
END;

FUNCTION extfwort(zahl : EXTENDED; a : BYTE) : string20;
VAR
  zahlstr : string20;
  i :       BYTE;
BEGIN
  str(zahl : 9 + a, zahlstr);
  insert(' ', zahlstr, 4 + a);
  FOR i := 1 TO 3 DO IF zahlstr[7 + a] = '0' THEN
      Delete(zahlstr, 7 + a, 1);
  extfwort := zahlstr;
END;

PROCEDURE laerman;
BEGIN
  laerm := True;
END;

PROCEDURE laermaus;
BEGIN
  laerm := False;
END;

PROCEDURE piep;
BEGIN
  IF laerm THEN
  BEGIN
    sound(2000);
    delay(300);
    nosound;
  END;
END;

PROCEDURE pieps;
BEGIN
  IF laerm THEN
  BEGIN
    sound(3000);
    delay(50);
    nosound;
  END;
END;

PROCEDURE fehler(Text : string80);
VAR
  textattralt : BYTE;
BEGIN
  textattralt := textattr;
  textcolor(lightred + blink);
  Write('--> ');
  textattr := textattralt;
  Write(Text);
  pieps;
END;

FUNCTION lesefehler : BOOLEAN;
VAR
  fehl : WORD;
BEGIN
  fehl := ioresult;
  CASE fehl OF
    0 : ;
    2 : fehler('File not found.');
    3 : fehler('Path not found.');
    106 : fehler('Invalid numeric format.');
    152 : fehler('Drive not ready.');
    ELSE
      fehler('I/O error no:' + wort(fehl) + '.');
  END;
  lesefehler := fehl <> 0;
END;

FUNCTION fileschonda(filename : string80) : BOOLEAN;
VAR
  test : FILE;
BEGIN
  Assign(test, filename);
  reset(test, 1);
  IF ioresult = 0 THEN
  BEGIN
    Close(test);
    fileschonda := True;
  END
  ELSE
    fileschonda := False;
END;

PROCEDURE warte;
BEGIN
  Write(lfcr, 'Continue: <Return> ');
  REPEAT
  UNTIL readkey IN [#13, #27];
END;

PROCEDURE zoeger(ms : WORD);
VAR
  i : WORD;
BEGIN
  WHILE keypressed DO readkey;
  i := ms DIV 100;
  WHILE (i >= 1) AND NOT keypressed DO
  BEGIN
    delay(100);
    Dec(i);
  END;
  IF keypressed THEN readkey;
END;

FUNCTION leerz(wieviel : SHORTINT) : string80;
VAR
  puffer : string80;
BEGIN
  puffer := '';
  FOR wieviel := wieviel DOWNTO 1 DO puffer := puffer + ' ';
  leerz := puffer;
END;

PROCEDURE readstr(VAR Text : string80);
VAR
  buchst : CHAR;
  puffer : string80;
  len :    BYTE absolute puffer;
  wx, wy : BYTE;
BEGIN
  puffer := Text;
  wy     := wherey;
  wx     := wherex;
  Write(puffer);
  gotoxy(wx, wy);
  buchst := readkey;
  IF buchst <> #13 THEN
  BEGIN
    IF (buchst = #0) AND (readkey = #79) THEN
    BEGIN
      Write(puffer);
      buchst := readkey;
    END
    ELSE
    BEGIN
      Write(leerz(len));
      puffer := '';
      gotoxy(wx, wy);
    END;
    REPEAT
      CASE buchst OF
        ' '..#255 : BEGIN
          Write(buchst);
          Inc(len);
          puffer[len] := buchst;
        END;
        #8 : IF len > 0 THEN
          BEGIN
            Dec(len);
            Write(#8' '#8);
          END;
        #13 : BEGIN
          writeln;
          Text := puffer;
          exit;
        END;
        #27 : BEGIN
          gotoxy(wx, wy);
          writeln(Text, leerz(len - length(Text)));
          exit;
        END;
      END;
      buchst := readkey;
    UNTIL False;
  END
  ELSE
    writeln;
END;

PROCEDURE schieben(VAR puffer : STRING);
BEGIN
  WHILE (puffer <> '') AND (puffer[1] = ' ') DO Delete(puffer, 1, 1);
  WHILE (puffer <> '') AND (puffer[length(puffer)] = ' ') DO Delete(puffer, length(puffer), 1);
END;

PROCEDURE kompri(VAR puffer : STRING);
VAR
  i : bigint64;
BEGIN
  i := pos(' ', puffer);
  WHILE i > 0 DO
  BEGIN
    Delete(puffer, i, 1);
    i := pos(' ', puffer);
  END;
END;

FUNCTION kleinbuchstaben(puffer : STRING) : STRING;
CONST
  diff = Ord('a') - Ord('A');
VAR
  i : WORD;
BEGIN
  FOR i := 1 TO length(puffer) DO
    IF puffer[i] IN ['A'..'Z'] THEN puffer[i] := chr(Ord(puffer[i]) + diff);
  kleinbuchstaben := puffer;
END;


FUNCTION readint(Text : string80; sonst : bigint64) : bigint64;
VAR
  puffer : string80;
  code :   INTEGER;
  zahl :   bigint64;
BEGIN
  puffer := wort(sonst);
  Write(Text, ': ');
  readstr(puffer);
  kompri(puffer);
  val(puffer, zahl, code);
  WHILE (code <> 0) DO
  BEGIN
    puffer := wort(sonst);
    fehler('Integer format expected: ');
    readstr(puffer);
    kompri(puffer);
    val(puffer, zahl, code);
  END;
  readint := zahl;
END;

FUNCTION readext(Text : string80; sonst : EXTENDED; l, n : BYTE) : EXTENDED;
VAR
  puffer : string80;
  code :   INTEGER;
  zahl :   EXTENDED;
BEGIN
  puffer := extwort(sonst, l, n);
  Write(Text, ': ');
  readstr(puffer);
  kompri(puffer);
  val(puffer, zahl, code);
  WHILE (code <> 0) DO
  BEGIN
    puffer := extwort(sonst, l, n);
    fehler('Real format expected: ');
    readstr(puffer);
    kompri(puffer);
    val(puffer, zahl, code);
  END;
  readext := zahl;
END;

FUNCTION readexte(Text : string80; sonst : EXTENDED; a, b : BYTE) : EXTENDED;
VAR
  puffer : string80;
  code :   INTEGER;
  zahl :   EXTENDED;
BEGIN
  puffer := extewort(sonst, a, b);
  Write(Text, ': ');
  readstr(puffer);
  kompri(puffer);
  val(puffer, zahl, code);
  WHILE (code <> 0) DO
  BEGIN
    puffer := extewort(sonst, a, b);
    fehler('Real format expected: ');
    readstr(puffer);
    kompri(puffer);
    val(puffer, zahl, code);
  END;
  readexte := zahl;
END;

FUNCTION readchar(Text : string80; sonst : CHAR) : CHAR;
VAR
  puffer, buchstabe : CHAR;
BEGIN
  Write(Text, ': ', sonst, #8);
  buchstabe := sonst;
  REPEAT
    puffer := readkey;
    CASE puffer OF
      #13 : BEGIN
        readchar := buchstabe;
        writeln;
        exit;
      END;
      #27 : BEGIN
        readchar := sonst;
        writeln(sonst);
        exit;
      END;
      ' '..#255 : BEGIN
        buchstabe := puffer;
        Write(buchstabe, #8);
      END;
    END;
  UNTIL False;
END;

FUNCTION readcharim(Text : string80; sonst : CHAR) : CHAR;
VAR
  buchstabe : CHAR;
BEGIN
  Write(Text, ': ', sonst, #8);
  buchstabe := readkey;
  IF buchstabe IN [#13, #27] THEN
  BEGIN
    readcharim := sonst;
    writeln;
  END
  ELSE
  BEGIN
    readcharim := buchstabe;
    IF buchstabe IN [' '..#255] THEN writeln(buchstabe);
  END;
END;

FUNCTION readstring(Text : string80; sonst : string80) : string80;
VAR
  puffer : string80;
BEGIN
  Write(Text, ': ');
  puffer := sonst;
  readstr(puffer);
  schieben(puffer);
  readstring := puffer;
END;

END.
