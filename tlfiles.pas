{ Borland-Pascal 7.0 / FPC 3.2.2}
{$ifdef fpc} {$mode TP} {$endif}

UNIT tlfiles;

{$IFDEF MSDOS}
{$A+,B-,E+,F-,G-,I-,N+,O-,P+,T+,V+,X-}
{$ELSE}
{$A+,B-,E+,F-,G+,I-,N+,P+,T+,V+,X-}
{$ENDIF}

INTERFACE

USES  crt, dos,
  objects, bequem, daff, wavpcm, tulab42, tlfilter;

CONST
  maxfiles = 20;
  {$ifdef fpc} maxmesswert=9223372036854775807.0; {$else} maxmesswert = 2147483648.0; {$endif}

TYPE
  {$ifdef fpc} typeextended=extended; typedouble=double; {$else} messwert = DOUBLE;
  wert = SINGLE; {$endif}

  { Bei den beiden Listen "blockliste" und "punktliste" wird dieselbe Zeigerstruktur }
  { verwendet. Am Anfang und am Ende der Liste wird jeweils ein zus‰tzliches Element }
  { angeh‰ngt, das bei jeder Suche in der Liste zu klein bzw. zu groﬂ ist. Die Werte }
  { hierf¸År stammen aus den Konstanten anf.. bzw. end... . Erreicht wird die Liste   }
  { durch einen Zeiger (auf...zeiger), der auf den next-Zeiger (nicht auf das Listen-}
  { element) des Anfangselementes zeigt.                                             }

  PPDataBlock = ^PDataBlock;
  PDataBlock  = ^TDataBlock;

  TDataBlock = OBJECT(TObject)
    Next, Prev :      PDataBlock;
    frompos, endpos : typeextended;
    CONSTRUCTOR new;
    CONSTRUCTOR beginblock;
    CONSTRUCTOR endblock;
    PROCEDURE store(VAR s : tbufstream);
    CONSTRUCTOR load(VAR s : tbufstream);
  END;

  PPBulletList = ^PBulletList;
  PBulletList  = ^TBulletList;

  TBulletList = OBJECT(TObject)
    Next, Prev : PBulletList;
    bei :        typeextended;
    CONSTRUCTOR new;
    CONSTRUCTOR beginbullet;
    CONSTRUCTOR endbullet;
    PROCEDURE store(VAR s : tbufstream);
    CONSTRUCTOR load(VAR s : tbufstream);
  END;

  listenfeld = OBJECT
    Name :           STRING[80];
    namedir :        dirstr;
    namename :       namestr;
    nameext :        extstr;
    head :           headerdata;
    length :         typeextended;
    block :          PPDataBlock;
    selectedbullet : PPBulletList;
    PROCEDURE newpointer;
    PROCEDURE deletepointer;
    PROCEDURE store(VAR s : tbufstream);
    PROCEDURE load(VAR s : tbufstream);
  END;
  listentyp = PACKED ARRAY[1..maxfiles] OF listenfeld;

CONST
  kan : BYTE     = 0;
  fre : EXTENDED = 0;
  filenr : BYTE  = 0;

VAR
  liste :   listentyp;
  offennr : BYTE;
  korr :    EXTENDED;

PROCEDURE oeffnen(nr : BYTE);

FUNCTION zwi(stelle : typeextended) : bigint64;

FUNCTION extzeit(stelle : typeextended) : EXTENDED;
FUNCTION zeit(stelle : typeextended) : bigint64;
FUNCTION messwext(zeitang : EXTENDED) : typeextended;
FUNCTION messw(zeitang : bigint64) : typeextended;

PROCEDURE rein(VAR zeiger : PDataBlock);
PROCEDURE raus(VAR zeiger : PDataBlock);
PROCEDURE prein(VAR zeiger : PBulletList);
PROCEDURE praus(VAR zeiger : PBulletList);

PROCEDURE fileliste;

PROCEDURE zeigertest;

PROCEDURE streamput(VAR s : tbufstream);
PROCEDURE streamget(VAR s : tbufstream);

IMPLEMENTATION

{
CONST
  anfblock : TDataBlock = (Next : nil; Prev : nil; frompos : -maxmesswert; endpos : -maxmesswert);
  endblock : TDataBlock = (Next : nil; Prev : nil; frompos : maxmesswert; endpos : maxmesswert);

  anfpunkt : TBulletList = (Next : nil; Prev : nil; bei : -maxmesswert);
  endpunkt : TBulletList = (Next : nil; Prev : nil; bei : maxmesswert);
}
CONST
  rblockliste : tstreamrec = (objtype : 200;
    vmtlink : ofs(typeof(TDataBlock)^);
    load : @TDataBlock.load;
    store : @TDataBlock.store);

  rpunktliste : tstreamrec = (objtype : 201;
    vmtlink : ofs(typeof(TBulletList)^);
    load : @TBulletList.load;
    store : @TBulletList.store);

VAR
  i : BYTE;

PROCEDURE oeffnen(nr : BYTE);
BEGIN
  offennr := nr;
  WITH liste[nr] DO
  BEGIN
    oeffne(Name, head);
    korr := head.frequency / fre;
  END;
END;

FUNCTION zwi(stelle : typeextended) : bigint64;
BEGIN
  zwi := round(stelle * korr);
END;

FUNCTION extzeit(stelle : typeextended) : EXTENDED;
BEGIN
  extzeit := stelle / fre * 1000;
END;

FUNCTION zeit(stelle : typeextended) : bigint64;
BEGIN
  zeit := round(extzeit(stelle));
END;

FUNCTION messwext(zeitang : EXTENDED) : typeextended;
BEGIN
  messwext := zeitang * fre / 1000;
END;

FUNCTION messw(zeitang : bigint64) : typeextended;
BEGIN
  messw := messwext(zeitang);
END;

PROCEDURE listenfeld.newpointer;
VAR
  hilf :  PDataBlock;
  philf : PBulletList;
BEGIN
  new(hilf, beginblock);
  new(hilf^.Next, endblock);
  hilf^.Next^.Prev := hilf;
  block            := addr(hilf^.Next);
  new(philf, beginbullet);
  new(philf^.Next, endbullet);
  philf^.Next^.Prev := philf;
  selectedbullet    := addr(philf^.Next);
END;

PROCEDURE rein(VAR zeiger : PDataBlock);
VAR
  hilf : PDataBlock;
BEGIN
  new(hilf, new);
  hilf^.Next       := zeiger;
  hilf^.Prev       := zeiger^.Prev;
  zeiger^.Prev     := hilf;
  hilf^.Prev^.Next := hilf;
  zeiger           := hilf;
END;

PROCEDURE raus(VAR zeiger : PDataBlock);
VAR
  hilf : PDataBlock;
BEGIN
  hilf             := zeiger;
  zeiger           := hilf^.Next;
  hilf^.Prev^.Next := hilf^.Next;
  hilf^.Next^.Prev := hilf^.Prev;
  dispose(hilf, done);
END;

PROCEDURE prein(VAR zeiger : PBulletList);
VAR
  hilf : PBulletList;
BEGIN
  new(hilf, new);
  hilf^.Next       := zeiger;
  hilf^.Prev       := zeiger^.Prev;
  zeiger^.Prev     := hilf;
  hilf^.Prev^.Next := hilf;
  zeiger           := hilf;
END;

PROCEDURE praus(VAR zeiger : PBulletList);
VAR
  hilf : PBulletList;
BEGIN
  hilf             := zeiger;
  zeiger           := hilf^.Next;
  hilf^.Prev^.Next := hilf^.Next;
  hilf^.Next^.Prev := hilf^.Prev;
  dispose(hilf, done);
END;

PROCEDURE listenfeld.deletepointer;
VAR
  hilf :  PDataBlock;
  philf : PBulletList;
BEGIN
  hilf := block^;
  WHILE hilf^.Next <> nil DO raus(hilf);
  dispose(hilf^.Prev, done);
  dispose(hilf, done);
  philf := selectedbullet^;
  WHILE philf^.Next <> nil DO praus(philf);
  dispose(philf^.Prev, done);
  dispose(philf, done);
END;

CONSTRUCTOR TDataBlock.new;
BEGIN
END;

CONSTRUCTOR TDataBlock.beginblock;
BEGIN
  Next    := nil;
  Prev    := nil;
  frompos := -maxmesswert;
  endpos  := -maxmesswert;
END;

CONSTRUCTOR TDataBlock.endblock;
BEGIN
  Next    := nil;
  Prev    := nil;
  frompos := maxmesswert;
  endpos  := maxmesswert;
END;

PROCEDURE TDataBlock.store(VAR s : tbufstream);
BEGIN
  s.Write(frompos, sizeof(typeextended));
  s.Write(endpos, sizeof(typeextended));
  s.put(Next);
END;

CONSTRUCTOR TDataBlock.load(VAR s : tbufstream);
BEGIN
  s.Read(frompos, sizeof(typeextended));
  s.Read(endpos, sizeof(typeextended));
  Next := PDataBlock(s.get);
  IF Next <> nil THEN Next^.Prev := @self;
END;

CONSTRUCTOR TBulletList.new;
BEGIN
END;

CONSTRUCTOR TBulletList.beginbullet;
BEGIN
  Next := nil;
  Prev := nil;
  bei  := -maxmesswert;
END;

CONSTRUCTOR TBulletList.endbullet;
BEGIN
  Next := nil;
  Prev := nil;
  bei  := maxmesswert;
END;


PROCEDURE TBulletList.store(VAR s : tbufstream);
BEGIN
  s.Write(bei, sizeof(typeextended));
  s.put(Next);
END;

CONSTRUCTOR TBulletList.load(VAR s : tbufstream);
BEGIN
  s.Read(bei, sizeof(typeextended));
  Next := PBulletList(s.get);
  IF Next <> nil THEN Next^.Prev := @self;
END;

PROCEDURE listenfeld.store(VAR s : tbufstream);
BEGIN
  s.Write(Name, sizeof(Name));
  s.Write(head, sizeof(headerdata));
  s.Write(length, sizeof(typeextended));
  s.put(block^);
END;

PROCEDURE listenfeld.load(VAR s : tbufstream);
VAR
  hilf :  PDataBlock;
  philf : PBulletList;
BEGIN
  s.Read(Name, sizeof(Name));
  fsplit(Name, namedir, namename, nameext);
  s.Read(head, sizeof(headerdata));
  s.Read(length, sizeof(typeextended));
  { Die Zeigerstruktur der Blˆcke wird neu aufgebaut und die Blˆcke aus dem Stream gelesen }
  new(hilf, beginblock);
  block            := addr(hilf^.Next);
  hilf^.Next       := PDataBlock(s.get);
  hilf^.Next^.Prev := hilf;
  { Die Zeigerstruktur der Punkte wird neu aufgebaut, die Liste bleibt leer }
  new(philf, beginbullet);
  new(philf^.Next, endbullet);
  philf^.Next^.Prev := philf;
  selectedbullet    := addr(philf^.Next);
END;

PROCEDURE fileliste;
VAR
  i : BYTE;
BEGIN
  writeln('  File list');
  FOR i := 1 TO min(filenr, 10) DO
  BEGIN
    Write(i : 3, ' :  ', liste[i].Name);
    gotoxy(40, wherey);
    IF i + 10 <= filenr THEN Write(i + 10 : 3, ' : ', liste[i + 10].Name);
    writeln;
  END;
END;

PROCEDURE streamput(VAR s : tbufstream);
VAR
  ind : BYTE;
BEGIN
  s.Write(kan, 1);
  s.Write(fre, sizeof(EXTENDED));
  s.Write(filenr, 1);
  FOR ind := 1 TO filenr DO liste[ind].store(s);
END;

PROCEDURE streamget(VAR s : tbufstream);
VAR
  ind : BYTE;
BEGIN
  s.Read(kan, 1);
  s.Read(fre, sizeof(EXTENDED));
  s.Read(filenr, 1);
  FOR ind := 1 TO filenr DO liste[ind].load(s);
END;

PROCEDURE zeigertest;
VAR
  i :     BYTE;
  hilf :  PDataBlock;
  philf : PBulletList;
BEGIN
  writeln(lfcr, 'Pointer test', lfcr);
  FOR i := 1 TO filenr DO
  BEGIN
    Write('> File no: ', i, '  ');
    Write('>> Blocks  ');
    hilf := liste[i].block^;
    IF hilf^.Prev^.Next <> hilf THEN
    BEGIN
      writeln('### Error at begin');
      warte;
      exit;
    END;
    WHILE hilf^.Next <> nil DO
    BEGIN
      Write(hilf^.frompos : 1 : 0, ' ', hilf^.endpos : 1 : 0, ' ');
      IF hilf^.Next^.Prev <> hilf THEN
      BEGIN
        writeln('### Error');
        warte;
        exit;
      END;
      hilf := hilf^.Next;
    END;
    Write('>> Points ');
    philf := liste[i].selectedbullet^;
    IF philf^.Prev^.Next <> philf THEN
    BEGIN
      writeln('### Error at begin');
      warte;
      exit;
    END;
    WHILE philf^.Next <> nil DO
    BEGIN
      Write(philf^.bei : 1 : 0, ' ');
      IF philf^.Next^.Prev <> philf THEN
      BEGIN
        writeln('### Error');
        warte;
        exit;
      END;
      philf := philf^.Next;
    END;
    writeln;
  END;
  writeln(lfcr, 'All pointers OK.');
  warte;
END;

BEGIN
  registertype(rblockliste);
  registertype(rpunktliste);
END.
