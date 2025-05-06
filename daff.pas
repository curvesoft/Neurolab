{ Borland-Pascal 7.0  / FPC 3.2.2 }
{$ifdef fpc} {$mode TP} {$endif}

UNIT daff;

{$IFDEF MSDOS}
{$A+,B-,E+,F-,G-,I-,N+,O-,P+,T-,V+,X-}
{$ELSE}
{$A+,B-,E+,F-,G+,I-,N+,P+,T-,V+,X-}
{$ENDIF}

INTERFACE

USES  crt, dos, objects,
  bequem;

CONST
  samplebit    = 25;
  maxsample    = (1 SHL samplebit) - 1;
  minsample    = -maxsample;
  sampleoffset = 1 SHL samplebit;
  richtung : (vow, ruw, mit) = mit;
  maxchannels  = 16;
  dafftypen : SET OF 0..23 = [4, 6, 7, 19];

TYPE
  sample = minsample..maxsample;

  channeldef = PACKED RECORD
    nr :               WORD;
    Name :             STRING[12];
    offs :             BYTE;
    factor1, factor2 : EXTENDED;
    channelunit :      STRING[12];
    dattyp, bits :     BYTE;
  END;

  headerdata = PACKED RECORD
    producer :      string20;
    productdate :   string20;
    producttime :   string20;
    frequency :     EXTENDED;
    protocol :      STRING[64];
    datalength :    bigint64;
    headerbytes :   bigint64;
    datatype :      BYTE;
    bytes :         BYTE;
    channelnumber : BYTE;
    channels :      ARRAY[0..maxchannels - 1] OF channeldef;
  END;

  PTag  = ^Tag;
  PPTag = ^PTag;

  Tag = OBJECT
    num :  WORD;
    size : WORD;
    line : STRING[60];
    Next : PTag;
    PROCEDURE nopar;
    PROCEDURE Read;
    PROCEDURE Show(VAR hin : Text); VIRTUAL;
    PROCEDURE einordnen(VAR ko : headerdata); VIRTUAL;
    PROCEDURE kaneinordnen(VAR kl : channeldef; VAR ko : headerdata); VIRTUAL;
    DESTRUCTOR done; VIRTUAL;
  END;

  contextzeiger = ^context;

  context = OBJECT(Tag)
    nested : PTag;
    PROCEDURE zeigen(VAR hin : Text); VIRTUAL;
    PROCEDURE einordnen(VAR ko : headerdata); VIRTUAL;
    DESTRUCTOR done; VIRTUAL;
  END;

  chancontextzeiger = ^chancontext;

  chancontext = OBJECT(context)
    PROCEDURE einordnen(VAR ko : headerdata); VIRTUAL;
  END;

  tbroken = OBJECT(Tag)
    w : EXTENDED;
    PROCEDURE lesen;
  END;

  tstring = OBJECT(Tag)
    st : STRING;
    PROCEDURE lesen;
  END;

  tname = OBJECT(tstring)
    CONSTRUCTOR lesen;
    PROCEDURE kaneinordnen(VAR kl : channeldef; VAR ko : headerdata); VIRTUAL;
  END;
  pname = ^tname;

  tlabel = OBJECT(tstring)
    CONSTRUCTOR lesen;
    PROCEDURE einordnen(VAR ko : headerdata); VIRTUAL;
  END;
  plabel = ^tlabel;

  tdate = OBJECT(Tag)
    dt : datetime;
    CONSTRUCTOR lesen;
    PROCEDURE einordnen(VAR ko : headerdata); VIRTUAL;
  END;
  pdate = ^tdate;

  tival = OBJECT(Tag)
    li : bigint64;
    CONSTRUCTOR lesen;
  END;
  pival = ^tival;

  tdval = OBJECT(Tag)
    ex : EXTENDED;
    CONSTRUCTOR lesen;
  END;
  pdval = ^tdval;

  ttval = OBJECT(Tag)
    st : STRING[255];
    CONSTRUCTOR lesen;
  END;
  ptval = ^ttval;

  theader = OBJECT(context)
    CONSTRUCTOR lesen;
  END;
  pheader = ^theader;

  tftyp = OBJECT(tstring)
    CONSTRUCTOR lesen;
    PROCEDURE einordnen(VAR ko : headerdata); VIRTUAL;
  END;
  pftyp = ^tftyp;

  tprod = OBJECT(tstring)
    CONSTRUCTOR lesen;
    PROCEDURE einordnen(VAR ko : headerdata); VIRTUAL;
  END;
  pprod = ^tprod;

  tdatadef = OBJECT(context)
    CONSTRUCTOR lesen;
  END;
  pdatadef = ^tdatadef;

  tblksize = OBJECT(Tag)
    li : bigint64;
    CONSTRUCTOR lesen;
    PROCEDURE einordnen(VAR ko : headerdata); VIRTUAL;
  END;
  pblksize     = ^tblksize;
(*     tsource =object (tag)
          st:string[80];
          constructor lesen;
          end;
       tsrcoffs=object (tag)
          li:bigint64;
          constructor lesen;
          end;
       tsrctyp =object (tag)
          ba:(bin,ascii);
          constructor lesen;
          end; *)
  tnestdatadef = OBJECT(chancontext)
    CONSTRUCTOR lesen;
    PROCEDURE einordnen(VAR ko : headerdata); VIRTUAL;
  END;
  pnestdatadef = ^tnestdatadef;

  tblkoffs = OBJECT(Tag)
    bo : BYTE;
    CONSTRUCTOR lesen;
    PROCEDURE kaneinordnen(VAR kl : channeldef; VAR ko : headerdata); VIRTUAL;
  END;
  pblkoffs = ^tblkoffs;

  tdatat = OBJECT(Tag)
    dtp : BYTE;
    CONSTRUCTOR lesen;
    PROCEDURE kaneinordnen(VAR kl : channeldef; VAR ko : headerdata); VIRTUAL;
  END;
  pdatat = ^tdatat;

  tchanl = OBJECT(context)
    kanz : WORD;
    CONSTRUCTOR lesen;
  END;
  pchanl = ^tchanl;

  tnestchanl = OBJECT(chancontext)
    CONSTRUCTOR lesen;
  END;
  pnestchanl = ^tnestchanl;

  tfactor = OBJECT(tbroken)
    CONSTRUCTOR lesen;
    PROCEDURE kaneinordnen(VAR kl : channeldef; VAR ko : headerdata); VIRTUAL;
  END;
  pfactor = ^tfactor;

  tspec = OBJECT(tstring)
    CONSTRUCTOR lesen;
    PROCEDURE kaneinordnen(VAR kl : channeldef; VAR ko : headerdata); VIRTUAL;
  END;
  pspec = ^tspec;

  tsfrq = OBJECT(tbroken)
    CONSTRUCTOR lesen;
    PROCEDURE kaneinordnen(VAR kl : channeldef; VAR ko : headerdata); VIRTUAL;
  END;
  psfrq = ^tsfrq;

  tschluss = OBJECT(context)
    kb, dl : bigint64;
    CONSTRUCTOR lesen;
    PROCEDURE einordnen(VAR ko : headerdata); VIRTUAL;
  END;
  pschluss = ^tschluss;

VAR
  tulabfehler : BOOLEAN;
  hin :         Text;

VAR
  daten, seqdaten : FILE;
  lesef :           FUNCTION(position : bigint64; kanal : BYTE) : sample;

PROCEDURE kopflesen(Name : string80; VAR anfang : contextzeiger);
PROCEDURE kopfzeigen(anfang : PTag; VAR hin : Text);
PROCEDURE kopf(CONST Name : string80; VAR ko : headerdata);
PROCEDURE openfileheader(Name : string80; VAR ko : headerdata);
PROCEDURE schliesse;
PROCEDURE ausserbetrieb;

PROCEDURE seqschreibe(zahl : sample);
PROCEDURE seqschreibeint16(zahl : sample);
PROCEDURE seqoeffne;
PROCEDURE seqschliesse;

IMPLEMENTATION

CONST
  {$ifdef fpc} puffermax=1 shl 26; {$else} puffermax = 65534; {$endif}
  zustand : (offen, zu, aus) = zu;
  leerkd : channeldef        = (nr : 0; Name : ''; offs : 0; factor1 : 1 / maxsample;
    factor2 : 1; channelunit : 'V'; dattyp : 0; bits : 0);
  leerk : headerdata = (producer : 'Unbekannt'; productdate : ''; producttime : ''; frequency : 0;
    protocol : ''; datalength : 0; headerbytes : 0; datatype : 0; bytes : 0; channelnumber : 0);

TYPE
  pufferbyte = PACKED ARRAY[0..puffermax - 1] OF BYTE;

  daffheader = PACKED RECORD
    swg :     PACKED ARRAY[1..10] OF CHAR;
    sex :     WORD;
    version : WORD;
    flags :   WORD;
    size :    bigint64;
    res :     ARRAY[1..8] OF CHAR;
  END;

VAR
  s :            tbufstream;
  pufferb, seqpufferb : ^pufferbyte;
  pufferbyteanfang, pufferbyteende, seqbytei : bigint64;
  datenweg :     BOOLEAN;
  kopfbytelaenge, filebyteende : bigint64;
  kan, blbytes : BYTE;
  koffs :        ARRAY[0..maxchannels - 1] OF BYTE;
  kshift :       ARRAY[0..maxchannels - 1] OF bigint64;

FUNCTION lesenil(position : bigint64; kanal : BYTE) : sample; FAR; FORWARD;
FUNCTION lese4(position : bigint64; kanal : BYTE) : sample; FAR; FORWARD;
FUNCTION lese6(position : bigint64; kanal : BYTE) : sample; FAR; FORWARD;
FUNCTION lese7(position : bigint64; kanal : BYTE) : sample; FAR; FORWARD;
FUNCTION lese19(position : bigint64; kanal : BYTE) : sample; FAR; FORWARD;

CONST
  lesefliste : ARRAY[0..23] OF FUNCTION(position : bigint64; kanal : BYTE) : sample =
    (lesenil, lesenil, lesenil, lesenil, lese4, lesenil, lese6, lese7, lesenil, lesenil,
    lesenil, lesenil, lesenil, lesenil, lesenil, lesenil, lesenil, lesenil, lesenil, lese19,
    lesenil, lesenil, lesenil, lesenil);


  { Prozeduren }

PROCEDURE neu(VAR nowvarvar : PPTag; tg : PTag);
BEGIN
  nowvarvar^ := tg;
  nowvarvar  := @nowvarvar^^.Next;
END;

PROCEDURE skiptag;
VAR
  num, dtyp, size, Value : WORD;
  parm1, lsize, i : LONGINT;
  parm2 : ARRAY[1..4] OF BYTE;
BEGIN
  s.Read(num, 2);
  s.Read(dtyp, 2);
  s.Read(size, 2);
  s.Read(parm1, 4);
  s.Read(parm2, 4);
  CASE size OF
    0..8 : exit;
    9..$FFFE : lsize := size;
    $FFFF : lsize    := parm1;
  END;
  FOR i := 1 TO (lsize + 1) DIV 2 DO s.Read(Value, 2);
END;

PROCEDURE skipcontext;
VAR
  id :   WORD;
  rest : ARRAY[1..14] OF BYTE;
BEGIN
  s.Read(rest, 14);
  REPEAT
    s.Read(id, 2);
    CASE id OF
      $0001..$7FFF : skiptag;
      $8000..$FFFE : skipcontext;
      $FFFF : BEGIN
        skiptag;
        exit;
      END;
    END;
  UNTIL False;
END;

PROCEDURE kopflesen(Name : string80; VAR anfang : contextzeiger);
LABEL
  ende;
VAR
  swgstring : PACKED ARRAY[1..10] OF CHAR;
VAR
  id :        WORD;
  nowvar :    PPTag;
  dh :        daffheader;
BEGIN
  swgstring   := 'SWGBSMBWS'#0;
  tulabfehler := False;
  s.init(Name, stopenread, 32000);
  s.Read(dh, 28);
  IF dh.swg <> swgstring THEN
  BEGIN
    fehler('No Turbolab-format file.');
    tulabfehler := True;
    s.done;
    exit;
  END;
  nowvar := @anfang;
  REPEAT
    s.Read(id, 2);
    CASE id OF
      $0000..$7FFF : tulabfehler := True;
      $8002 : neu(nowvar, new(pheader, lesen));
      $8003 : neu(nowvar, new(pdatadef, lesen));
      $8005 : neu(nowvar, new(pchanl, lesen));
      $8006..$FFFE : skipcontext;
      $8004{Daten} : GOTO ende;
      $FFFF : tulabfehler := True;
    END;
    id := $0000;
    IF tulabfehler THEN
    BEGIN
      fehler('Incorrect Turbolab 4.2 - Header.');
      GOTO ende;
    END;
  UNTIL False;

  ende :
    neu(nowvar, new(pschluss, lesen));
  nowvar^ := nil;
  s.done;
END;

PROCEDURE kopfzeigen(anfang : PTag; VAR hin : Text);
VAR
  wandert : PTag;
BEGIN
  wandert := anfang;
  WHILE wandert <> nil DO
  BEGIN
    wandert^.Show(hin);
    wandert := wandert^.Next;
  END;
END;

PROCEDURE kopf(CONST Name : string80; VAR ko : headerdata);
VAR
  anfang, wandert, hilf1, hilf2 : PTag;
  i : BYTE;
BEGIN
  kopflesen(Name, contextzeiger(anfang));
  IF tulabfehler THEN exit;
  IF anfang <> nil THEN
  BEGIN
    wandert := anfang;
    WHILE (wandert^.Next <> nil) AND (typeof(wandert^.Next^) <> typeof(tdatadef)) DO
      wandert     := wandert^.Next;
    hilf1         := wandert^.Next^.Next;
    hilf2         := anfang;
    anfang        := wandert^.Next;
    anfang^.Next  := hilf2^.Next;
    hilf2^.Next   := hilf1;
    wandert^.Next := hilf2;
  END;
  ko := leerk;
  FOR i := 0 TO maxchannels - 1 DO ko.channels[i] := leerkd;
  wandert := anfang;
  WHILE wandert <> nil DO
  BEGIN
    wandert^.einordnen(ko);
    wandert := contextzeiger(wandert^.Next);
  END;
  IF (ko.frequency = 0) OR (ko.channelnumber = 0) THEN tulabfehler := True;
  IF anfang <> nil THEN dispose(anfang, done);
END;

PROCEDURE openfileheader(Name : string80; VAR ko : headerdata);
VAR
  i : INTEGER;
BEGIN
  {if zustand=offen then begin fehler('Offen!'); warte; schliesse end;}
  kopfbytelaenge := ko.headerbytes;
  kan            := ko.channelnumber;
  blbytes        := ko.bytes;
  FOR i := 0 TO kan DO
  BEGIN
    koffs[i]  := ko.channels[i].offs;
    kshift[i] := samplebit - ko.channels[i].bits + 1;  // 25-12+1=14
  END;
  lesef := lesefliste[ko.datatype];
  IF zustand = aus THEN exit;
  filebyteende := ko.datalength * ko.bytes;
  Assign(daten, Name);
  {$ifdef fpc}
if zustand=zu then zustand:=offen;
datenweg:=false;
  {$else}
  reset(daten, 1);
  tulabfehler := ioresult > 0;
  IF NOT tulabfehler THEN
  BEGIN
    zustand  := offen;
    datenweg := False;
  END
  ELSE
  BEGIN
    writeln(lfcr);
    fehler('No access to file "' + Name + '".');
    warte;
    datenweg := True;
  END;
  {$endif}
  pufferbyteanfang := 0;
  pufferbyteende   := 0;
END;

PROCEDURE seqoeffne;
BEGIN
  {$ifndef fpc}
  reset(seqdaten, 1);
  seek(seqdaten, filesize(seqdaten));
  {$endif}
  new(seqpufferb);
  seqbytei := -2;
END;

PROCEDURE seqschliesse;
VAR
  lenpuffer : midint32;
BEGIN
  {$ifdef fpc}
if seqbytei>=0 then begin
   reset(seqdaten,1);
   seek(seqdaten,filesize(seqdaten));
   blockwrite(seqdaten,seqpufferb^,seqbytei+2,lenpuffer);
   close(seqdaten);
   end;
  {$else}
  IF seqbytei >= 0 THEN blockwrite(seqdaten, seqpufferb^, seqbytei + 2, lenpuffer);
  Close(seqdaten);
  {$endif}
  dispose(seqpufferb);
END;

PROCEDURE schliesse;
BEGIN
  {$ifdef fpc}
if zustand=offen then zustand:=zu;
  {$else}
  IF zustand = offen THEN
  BEGIN
    Close(daten);
    tulabfehler := ioresult > 0;
    IF tulabfehler THEN
    BEGIN
      zustand := zu;
      writeln(lfcr);
      fehler('No access to data file.');
      warte;
    END;
  END;
  {$endif}
  pufferbyteanfang := 0;
  pufferbyteende   := 0;
END;

PROCEDURE ausserbetrieb;
BEGIN
  dispose(pufferb);
  zustand := aus;
END;

PROCEDURE seqschreibe(zahl : sample);
CONST
  sampleshift = samplebit - 12 + 1;
VAR
  lenpuffer : midint32;
  intzeiger : ^WORD;
BEGIN
  Inc(seqbytei, 2);
  intzeiger  := addr(seqpufferb^[seqbytei]);
  zahl       := (zahl + sampleoffset) SHR sampleshift;
  intzeiger^ := zahl;
  IF seqbytei + 2 >= puffermax - 1 THEN
  BEGIN
    {$ifdef fpc}
   reset(seqdaten,1);
   seek(seqdaten,filesize(seqdaten));
    {$endif}
    blockwrite(seqdaten, seqpufferb^, seqbytei + 2, lenpuffer);
    {$ifdef fpc}
   close(seqdaten);
    {$endif}
    seqbytei := -2;
  END;
END;

PROCEDURE seqschreibeint16(zahl : sample);
CONST
  sampleshift = samplebit - 16 + 1;
VAR
  lenpuffer : midint32;
  intzeiger : ^INTEGER;
BEGIN
  Inc(seqbytei, 2);
  intzeiger  := addr(seqpufferb^[seqbytei]);
  zahl       := (zahl) SHR sampleshift;
  intzeiger^ := zahl;
  IF seqbytei + 2 >= puffermax - 1 THEN
  BEGIN
    {$ifdef fpc}
   reset(seqdaten,1);
   seek(seqdaten,filesize(seqdaten));
    {$endif}
    blockwrite(seqdaten, seqpufferb^, seqbytei + 2, lenpuffer);
    {$ifdef fpc}
   close(seqdaten);
    {$endif}
    seqbytei := -2;
  END;
END;

PROCEDURE pufferlesen(byteposition : bigint64);
VAR
  anfang :    bigint64;
  lenpuffer : midint32;
BEGIN
  CASE richtung OF
    vow : anfang := byteposition;
    ruw : anfang := max(byteposition + 1 - puffermax, 0);
    mit : anfang := max(byteposition + (1 - puffermax) DIV 2, 0);
  END;
  {$ifdef fpc}
reset(daten,1);
tulabfehler:=ioresult>0;
if not tulabfehler then zustand:=offen
                   else if not datenweg then begin
                       writeln(lfcr); fehler('No access to data file.'); warte;
                       datenweg:=true;
                       exit end;
  {$endif}
  seek(daten, anfang + kopfbytelaenge);
  blockread(daten, pufferb^, puffermax, lenpuffer);
  tulabfehler := ioresult > 0;
  IF NOT tulabfehler THEN
  BEGIN
    pufferbyteanfang := anfang;
    pufferbyteende   := pufferbyteanfang + lenpuffer;
  END
  ELSE
  BEGIN
    IF NOT datenweg THEN
    BEGIN
      writeln(lfcr);
      fehler('No access to data file.');
      warte;
      datenweg := True;
    END;
    pufferbyteanfang := byteposition;
    pufferbyteende   := byteposition + puffermax;
    zustand          := zu;
    fillchar(pufferb^, sizeof(pufferb^), #0);
  END;
  {$ifdef fpc}
close(daten);
tulabfehler:=ioresult>0;
if tulabfehler then begin
   zustand:=zu;
   writeln(lfcr); fehler('No access to data file.'); warte end;
  {$endif}
END;

FUNCTION lesenil(position : bigint64; kanal : BYTE) : sample;
BEGIN
  fehler('Internal data type error.');
  lesenil := 0;
END;

FUNCTION lese4(position : bigint64; kanal : BYTE) : sample;
VAR
  puff :         ^BYTE;
  byteposition : bigint64;
BEGIN
  byteposition := position * blbytes + koffs[kanal];
  IF (byteposition < pufferbyteanfang) OR (byteposition >= pufferbyteende) THEN
  BEGIN
    IF (byteposition < 0) OR (byteposition >= filebyteende) THEN
    BEGIN
      lese4 := 0;
      exit;
    END;
    pufferlesen(byteposition);
  END;
  puff  := addr(pufferb^[byteposition - pufferbyteanfang]);
  lese4 := puff^ SHL kshift[kanal] - sampleoffset;
END;

FUNCTION lese6(position : bigint64; kanal : BYTE) : sample;
VAR
  puff :         ^WORD;
  byteposition : bigint64;
BEGIN
  byteposition := position * blbytes + koffs[kanal];
  IF (byteposition < pufferbyteanfang) OR (byteposition + 1 >= pufferbyteende) THEN
  BEGIN
    IF (byteposition < 0) OR (byteposition + 1 >= filebyteende) THEN
    BEGIN
      lese6 := 0;
      exit;
    END;
    pufferlesen(byteposition);
  END;
  puff  := addr(pufferb^[byteposition - pufferbyteanfang]);
  lese6 := puff^ SHL kshift[kanal] - sampleoffset;
END;

FUNCTION lese7(position : bigint64; kanal : BYTE) : sample;
VAR
  puff :         ^INTEGER;
  puffl :        bigint64;
  byteposition : bigint64;
BEGIN
  byteposition := position * blbytes + koffs[kanal];
  IF (byteposition < pufferbyteanfang) OR (byteposition + 1 >= pufferbyteende) THEN
  BEGIN
    IF (byteposition < 0) OR (byteposition + 1 >= filebyteende) THEN
    BEGIN
      lese7 := 0;
      exit;
    END;
    pufferlesen(byteposition);
  END;
  puff  := addr(pufferb^[byteposition - pufferbyteanfang]);
  lese7 := puff^ SHL kshift[kanal];
END;

FUNCTION lese19(position : bigint64; kanal : BYTE) : sample;
CONST
  sshift : bigint64 = 10;
  soffs             = sampleoffset SHR 4;
VAR
  puff :         ^INTEGER;
  byteposition : bigint64;
BEGIN
  byteposition := position * blbytes + koffs[kanal];
  IF (byteposition < pufferbyteanfang) OR (byteposition + 1 >= pufferbyteende) THEN
  BEGIN
    IF (byteposition < 0) OR (byteposition + 1 >= filebyteende) THEN
    BEGIN
      lese19 := 0;
      exit;
    END;
    pufferlesen(byteposition);
  END;
  puff   := addr(pufferb^[byteposition - pufferbyteanfang]);
  lese19 := puff^ SHL sshift - soffs;
END;

{ Tag }

PROCEDURE Tag.nopar;
VAR
  parm : ARRAY[1..14] OF CHAR;
BEGIN
  s.Read(parm, 14);
END;

PROCEDURE Tag.Read;
VAR
  dtyp : WORD;
BEGIN
  s.Read(num, 2);
  s.Read(dtyp, 2);
  s.Read(size, 2);
END;

PROCEDURE Tag.Show(VAR hin : Text);
BEGIN
  writeln(hin, line);
END;

PROCEDURE Tag.einordnen(VAR ko : headerdata);
BEGIN
END;

PROCEDURE Tag.kaneinordnen(VAR kl : channeldef; VAR ko : headerdata);
BEGIN
END;

DESTRUCTOR Tag.done;
BEGIN
  IF Next <> nil THEN dispose(Next, done);
END;

{ context }

PROCEDURE context.einordnen(VAR ko : headerdata);
VAR
  wandert : PTag;
BEGIN
  wandert := nested;
  WHILE wandert <> nil DO
  BEGIN
    wandert^.einordnen(ko);
    wandert := wandert^.Next;
  END;
END;

PROCEDURE context.zeigen(VAR hin : Text);
VAR
  wandert : PTag;
BEGIN
  writeln(hin, line);
  wandert := nested;
  WHILE wandert <> nil DO
  BEGIN
    wandert^.Show(hin);
    wandert := wandert^.Next;
  END;
END;

DESTRUCTOR context.done;
BEGIN
  Tag.done;
  IF nested <> nil THEN dispose(nested, done);
END;

{ chancontext }

PROCEDURE chancontext.einordnen(VAR ko : headerdata);
VAR
  i : BYTE;

  PROCEDURE bearbeiten(VAR kl : channeldef);
  VAR
    wandert : PTag;
  BEGIN
    wandert := nested;
    WHILE wandert <> nil DO
    BEGIN
      wandert^.kaneinordnen(ko.channels[i], ko);
      wandert := wandert^.Next;
    END;
  END;

BEGIN
  IF num = 0 THEN FOR i := 0 TO maxchannels - 1 DO bearbeiten(ko.channels[i])
  ELSE
  BEGIN
    i := 0;
    WHILE (ko.channels[i].nr <> num) DO Inc(i);
    bearbeiten(ko.channels[i]);
  END;
END;

{ tbroken }

PROCEDURE tbroken.lesen;
CONST
  b32 = 2147483648.0;
  ln2 = 0.693147180559945;
VAR
  zahl : RECORD
    mantissa : LONGINT;
    exponent : INTEGER;
    rest :     ARRAY[1..2] OF BYTE;
    END;
BEGIN
  Tag.Read;
  s.Read(zahl, 8);
  w := (zahl.mantissa / b32) * exp(ln2 * zahl.exponent);
END;


{ tstring }

PROCEDURE tstring.lesen;
VAR
  parm : ARRAY[1..8] OF CHAR;
BEGIN
  Tag.Read;
  s.Read(parm, 8);
  CASE size OF
    0..8 : st := parm;
    9..255 : s.Read(st[1], ((size + 1) DIV 2) * 2);
  END;
  st[0] := chr(size);
END;

{ tname }

CONSTRUCTOR tname.lesen;
BEGIN
  tstring.lesen;
  line := '    Name: ' + st;
END;

PROCEDURE tname.kaneinordnen(VAR kl : channeldef; VAR ko : headerdata);
BEGIN
  kl.Name := st;
END;

{ tlabel }

CONSTRUCTOR tlabel.lesen;
BEGIN
  tstring.lesen;
  line := '    Description: ' + st;
END;

PROCEDURE tlabel.einordnen(VAR ko : headerdata);
BEGIN
  ko.protocol := st;
END;

{ tdate }

CONSTRUCTOR tdate.lesen;
VAR
  zeitinfo : RECORD
    j :         WORD;
    t :         BYTE;
    mo :        BYTE;
    m :         BYTE;
    s :         BYTE;
    unwichtig : WORD;
    END;
BEGIN
  Tag.Read;
  CASE num OF
    0 : BEGIN
      s.Read(zeitinfo, 8);
      WITH zeitinfo, dt DO
      BEGIN
        day   := t;
        month := mo;
        year  := j;
        hour  := s;
        min   := m;
        sec   := 0;
      END;
    END;
    1 :{4-Byte-UNIX-Format, sek seit 1.1.1970 0:00:00};
  END;
  WITH dt DO
    line := '    Date: ' + wort(day) + '.' + wort(month) + '.' + wort(year) + ' Time: ' +
      wort(hour) + ':' + wort(min) + ':' + wort(sec);
END;

PROCEDURE tdate.einordnen(VAR ko : headerdata);
BEGIN
  WITH dt DO
  BEGIN
    ko.productdate := wort(day) + '.' + wort(month) + '.' + wort(year);
    ko.producttime := wort(hour) + ':' + wort(min) + ':' + wort(sec);
  END;
END;

{ tival }

CONSTRUCTOR tival.lesen;
BEGIN
  Tag.Read;
END;

{ tdval }

CONSTRUCTOR tdval.lesen;
BEGIN
  Tag.Read;
END;

{ ttval }

CONSTRUCTOR ttval.lesen;
BEGIN
  Tag.Read;
END;

{ theader }

CONSTRUCTOR theader.lesen;
VAR
  id :     WORD;
  nowvar : PPTag;
  parms :  ARRAY[1..8] OF CHAR;
BEGIN
  Tag.Read;
  line := 'Header:';
  s.Read(parms, 8);
  nowvar := @nested;
  REPEAT
    s.Read(id, 2);
    CASE id OF
      $0001 : neu(nowvar, new(pname, lesen));
      $0002 : neu(nowvar, new(plabel, lesen));
      $0003 : neu(nowvar, new(pdate, lesen));
      $0080 : neu(nowvar, new(pftyp, lesen));
      $0081 : neu(nowvar, new(pprod, lesen));
      $0004..$007F, $0082..$7FFF : skiptag;
      $8000..$FFFE : skipcontext;
      $FFFF : BEGIN
        nopar;
        nowvar^ := nil;
        exit;
      END;
    END;
  UNTIL False;
END;

{ tftyp }

CONSTRUCTOR tftyp.lesen;
BEGIN
  tstring.lesen;
  line := '    Filetyp: ' + st;
END;

PROCEDURE tftyp.einordnen(VAR ko : headerdata);
BEGIN
  IF num <> 0 THEN
  BEGIN
    tulabfehler := True;
    fehler('No "Sampled Data File" but "' + st + '".');
  END;
END;

{ tprod }

CONSTRUCTOR tprod.lesen;
BEGIN
  tstring.lesen;
  line := '    Producer of file: ' + st;
END;

PROCEDURE tprod.einordnen(VAR ko : headerdata);
BEGIN
  ko.producer := st;
END;

{ tdatadef }

CONSTRUCTOR tdatadef.lesen;
VAR
  id :     WORD;
  nowvar : PPTag;
  parms :  ARRAY[1..8] OF CHAR;
BEGIN
  Tag.Read;
  line := 'Data definition:';
  s.Read(parms, 8);
  nowvar := @nested;
  REPEAT
    s.Read(id, 2);
    CASE id OF
      $0098 : neu(nowvar, new(pblksize, lesen));
      $0000..$0097, $0099..$7FFF : skiptag;
      $8003 : neu(nowvar, new(pnestdatadef, lesen));
      $8000..$8002, $8004..$FFFE : skipcontext;
      $FFFF : BEGIN
        nopar;
        nowvar^ := nil;
        exit;
      END;
    END;
    IF tulabfehler THEN exit;
  UNTIL False;
END;

{ tblksize }

CONSTRUCTOR tblksize.lesen;
VAR
  parm : ARRAY [1..4] OF BYTE;
BEGIN
  Tag.Read;
  s.Read(li, 4);
  s.Read(parm, 4);
  line := '    Block size: ' + wort(li);
END;

PROCEDURE tblksize.einordnen(VAR ko : headerdata);
BEGIN
  ko.bytes := li;
END;

(* noch nicht benutzt:
{ tsource }

constructor tsource.lesen;
begin
Tag.read;
end;

{ tsrcoffs }

constructor tsrcoffs.lesen;
begin
Tag.read;
end;

{ tsrctyp }

constructor tsrctyp.lesen;
begin
Tag.read;
end; *)

{ tnestdatadef }

CONSTRUCTOR tnestdatadef.lesen;
VAR
  id :     WORD;
  nowvar : PPTag;
  parms :  ARRAY[1..8] OF CHAR;
BEGIN
  Tag.Read;
  line := '  Channel data definition [' + wort(num) + ']:';
  s.Read(parms, 8);
  nowvar := @nested;
  REPEAT
    s.Read(id, 2);
    CASE id OF
      $00A0 : neu(nowvar, new(pblkoffs, lesen));
      $00A2 : neu(nowvar, new(pdatat, lesen));
      $0000..$009F, $00A1, $00A3..$7FFF : skiptag;
      $8000..$FFFE : skipcontext;
      $FFFF : BEGIN
        nopar;
        nowvar^ := nil;
        exit;
      END;
    END;
    IF tulabfehler THEN exit;
  UNTIL False;
END;

PROCEDURE tnestdatadef.einordnen(VAR ko : headerdata);
VAR
  wandert : PTag;
  kd :      channeldef;
BEGIN
  wandert := nested;
  kd      := leerkd;
  WHILE wandert <> nil DO
  BEGIN
    wandert^.kaneinordnen(kd, ko);
    wandert := wandert^.Next;
  END;
  kd.nr := num;
  ko.channels[kd.offs DIV 2] := kd;
END;

{ tblkoffs }

CONSTRUCTOR tblkoffs.lesen;
VAR
  parm :    ARRAY [1..4] OF BYTE;
  blkoffs : LONGINT;
BEGIN
  Tag.Read;
  s.Read(blkoffs, 4);
  bo := blkoffs;
  s.Read(parm, 4);
  line := '    Block offset: ' + wort(bo);
END;

PROCEDURE tblkoffs.kaneinordnen(VAR kl : channeldef; VAR ko : headerdata);
BEGIN
  kl.offs := bo;
END;

{ tdatat }

CONSTRUCTOR tdatat.lesen;
CONST
  dtyp : ARRAY [0..23] OF STRING[10] =
    ('unknown', 'unknown', 'Char', 'Char', 'ñ Byte', 'Byte', 'ñ word', 'word', 'ñ long',
    'long', 'ñ real4', 'real4', 'ñ real8', 'real8', 'ñ broken', 'broken', 'ñ bit8', 'bit8',
    'ñ bit16', 'bit16', '', '', '', '');
VAR
  parm :  ARRAY [1..4] OF BYTE;
  datat : LONGINT;
BEGIN
  Tag.Read;
  s.Read(datat, 4);
  dtp := datat MOD 256;
  s.Read(parm, 4);
  line := '    Data type: ' + dtyp[dtp];
END;

PROCEDURE tdatat.kaneinordnen(VAR kl : channeldef; VAR ko : headerdata);
BEGIN
  kl.dattyp := dtp;
  kl.bits   := 12;
  IF ko.datatype = 0 THEN ko.datatype := dtp;
  IF NOT (dtp IN dafftypen) THEN
  BEGIN
    tulabfehler := True;
    fehler('Unknown data format (' + wort(dtp) + ')');
    writeln;
  END;
  IF dtp <> ko.datatype THEN
  BEGIN
    tulabfehler := True;
    fehler('Mixed data types');
  END;
END;

{ tchanl }

CONSTRUCTOR tchanl.lesen;
VAR
  id :     WORD;
  nowvar : PPTag;
  parms :  ARRAY[1..6] OF CHAR;
BEGIN
  Tag.Read;
  line := 'Channel protocol:';
  s.Read(kanz, 2); { Anzahl der Kanaele: Leider meist =0}
  s.Read(parms, 6);
  nowvar := @nested;
  REPEAT
    s.Read(id, 2);
    CASE id OF
      $0000..$7FFF : skiptag;
      $8005 : neu(nowvar, new(pnestchanl, lesen));
      $8000..$8004, $8006..$FFFE : skipcontext;
      $FFFF : BEGIN
        nopar;
        nowvar^ := nil;
        exit;
      END;
    END;
    IF tulabfehler THEN exit;
  UNTIL False;
END;

{ tnestchanl }

CONSTRUCTOR tnestchanl.lesen;
VAR
  id :     WORD;
  nowvar : PPTag;
  parms :  ARRAY[1..8] OF CHAR;
BEGIN
  Tag.Read;
  line := '  Channel definition [' + wort(num) + ']:';
  s.Read(parms, 8);
  nowvar := @nested;
  REPEAT
    s.Read(id, 2);
    CASE id OF
      $0001 : neu(nowvar, new(pname, lesen));
      $0082 : neu(nowvar, new(pfactor, lesen));
      $0083 : skiptag;
      $0084 : neu(nowvar, new(pspec, lesen));
      $008A : neu(nowvar, new(psfrq, lesen));
      $0000, $0002..$0081, $0085..$0089, $008B..$7FFF : skiptag;
      $8000..$FFFE : skipcontext;
      $FFFF : BEGIN
        nopar;
        nowvar^ := nil;
        exit;
      END;
    END;
  UNTIL False;
END;

{ tfactor }

CONSTRUCTOR tfactor.lesen;
BEGIN
  tbroken.lesen;
  line := '    Axis: ' + wort(num) + ' mit Faktor: ' + extewort(w, 3, 2);
END;

PROCEDURE tfactor.kaneinordnen(VAR kl : channeldef; VAR ko : headerdata);
BEGIN
  CASE num OF
    0 : ko.frequency := 1 / w;
    1 : kl.factor1   := w / maxsample * (1 SHL 11);
    2 : kl.factor2   := w;
  END;
END;

{ tspec }

CONSTRUCTOR tspec.lesen;
VAR
  parm :    ARRAY [1..4] OF BYTE;
  blkoffs : LONGINT;
BEGIN
  tstring.lesen;
  line := '    Axis: ' + wort(num) + ' mit Einheit: ' + st;
END;

PROCEDURE tspec.kaneinordnen(VAR kl : channeldef; VAR ko : headerdata);
BEGIN
  IF num = 2 THEN
  BEGIN
    IF st = 'Volt' THEN kl.channelunit := 'V'
    ELSE
      kl.channelunit := st;
  END;
END;

{ tsfrq }

CONSTRUCTOR tsfrq.lesen;
BEGIN
  tbroken.lesen;
  line := '    Frequency: ' + extwort(w, 8, 1);
END;

PROCEDURE tsfrq.kaneinordnen(VAR kl : channeldef; VAR ko : headerdata);
BEGIN
  ko.frequency := w;
END;

{ tschluss }

CONSTRUCTOR tschluss.lesen;
BEGIN
  nopar;
  kb     := s.getpos;
  dl     := (s.getsize - kb) DIV 2;
  nested := nil;
  line   := '    Total header: ' + wort(kb) + ' Size: ' + wort(dl);
END;

PROCEDURE tschluss.einordnen(VAR ko : headerdata);
BEGIN
  ko.headerbytes := kb;
  CASE ko.datatype OF
    4, 5, 16, 17 : ko.channelnumber := ko.bytes;
    6, 7, 18, 19 : ko.channelnumber := ko.bytes DIV 2;
    ELSE
    BEGIN
      tulabfehler := True;
      fehler('Datenformat unbekannt');
    END;
  END;
  ko.datalength := dl DIV ko.channelnumber;
END;

{ Initialisierung }

BEGIN
  new(pufferb);
  pufferbyteanfang := 0;
  pufferbyteende   := 0;
END.
