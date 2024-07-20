{Turbo-Pascal 6.0}

unit  bequem;

{$A+,B-,F-,G+,I-,N+,O-,V-,X-}

interface

uses  crt;

const maxlongint=2147483647;
      lfcr=#10#13;

      ln10=2.302585093;

type  string8=string[8];
      string20=string[20];               string80=string[80];

function min (a,b:longint) :longint;
function max (a,b:longint) :longint;
function mine (a,b:extended):extended;
function maxe (a,b:extended):extended;

function log (x:extended):extended;
function pot (x:shortint):longint;
function xpot (x:byte):extended;

procedure incex (var a:extended; b:extended);

function zahl (zahlstr:string20):longint;
function wort (zahl:longint):string20;
function extwort (zahl:extended; l,n:byte):string20;
function extewort (zahl:extended; a,b:byte):string20;
procedure kompri (var puffer:string);

procedure piep;
procedure pieps;

procedure fehler (text:string80);
function lesefehler:boolean;

procedure warte;

function readint (text:string80; sonst:longint):longint;
function readext (text:string80; sonst:extended; l,n:byte):extended;
function readexte (text:string80; sonst:extended; a,b:byte):extended;
function readchar (text:string80; sonst:char):char;
function readstring (text:string80; sonst:string80):string80;

implementation

function min (a,b:longint) :longint;
begin
if a<b then min:=a else min:=b;
end;

function max (a,b:longint) :longint;
begin
if a<b then max:=b else max:=a;
end;

function mine (a,b:extended):extended;
begin
if a<b then mine:=a else mine:=b;
end;

function maxe (a,b:extended):extended;
begin
if a<b then maxe:=b else maxe:=a;
end;

function log (x:extended):extended;
begin
log:=ln(x)/ln10;
end;

function pot (x:shortint):longint;
begin
pot:=round(exp(x*ln10));
end;

function xpot (x:byte):extended;
begin
xpot:=int(exp(x*ln10)+0.5);
end;

procedure incex (var a:extended; b:extended);
begin
a:=a+b;
end;

function zahl (zahlstr:string20):longint;
var   zahlint:extended;
      kont:integer;
begin
val(zahlstr,zahlint,kont); zahl:=round(zahlint);
end;

function wort (zahl:longint):string20;
var   zahlstr:string20;
begin
str(zahl,zahlstr); wort:=zahlstr;
end;

function extwort (zahl:extended; l,n:byte):string20;
var   zahlstr:string20;
begin
str(zahl:l:n,zahlstr); extwort:=zahlstr;
end;

function extewort (zahl:extended; a,b:byte):string20;
var   zahlstr:string20;
      i:byte;
begin
str(zahl:9+a,zahlstr);
insert(' ',zahlstr,4+a);
delete(zahlstr,9,4-b);
for i:=1 to b-1 do if zahlstr[7+a]='0' then begin
   delete(zahlstr,7+a,1); zahlstr:=zahlstr+' ' end;
extewort:=zahlstr;
end;

procedure piep;
begin
sound(2000); delay(300);
nosound;
end;

procedure pieps;
begin
sound(3000); delay(50);
nosound;
end;

procedure fehler (text:string80);
var  textattralt:byte;
begin
textattralt:=textattr;
textcolor(lightred+blink); write('--> '); textattr:=textattralt; write(text);
pieps;
end;

function lesefehler:boolean;
var   fehl:word;
begin
fehl:=ioresult;
case fehl of
   0:;
   2:fehler('Datei nicht gefunden.');
   3:fehler('Suchweg nicht gefunden.');
   106:fehler('UngÅltiges numerisches Format.');
   152:fehler('Laufwerk nicht betriebsbereit.');
   else fehler('Lesefehler Nr:'+wort(fehl)+'.');
   end;
lesefehler:=fehl<>0;
end;

procedure warte;
begin
write(lfcr,'Weiter mit "Return" ');
repeat until readkey in [#13,#27];
end;

function leerz (wieviel:shortint):string80;
var   puffer:string80;
begin
puffer:='';
for wieviel:=wieviel downto 1 do puffer:=puffer+' ';
leerz:=puffer;
end;

procedure readstr (var text:string80);
var   buchst:char;
      puffer:string80; len:byte absolute puffer;
      wx,wy:byte;
begin
puffer:=text;
wy:=wherey; wx:=wherex;
write(puffer); gotoxy(wx,wy);
buchst:=readkey;
if buchst<>#13 then begin
   if (buchst=#0) and (readkey=#79) then begin
      write(puffer);
      buchst:=readkey;
      end      else begin
      write(leerz(len)); puffer:=''; gotoxy(wx,wy) end;
   repeat
      case buchst of
        ' '..#255:begin write(buchst); inc(len); puffer[len]:=buchst end;
        #8:if len>0 then begin dec(len); write(#8' '#8) end;
        #13:begin writeln; text:=puffer; exit end;
        #27:begin gotoxy(wx,wy); writeln(text,leerz(len-length(text))); exit end;
        end;
      buchst:=readkey;
    until false;
    end         else writeln;
end;

procedure schieben (var puffer:string);
begin
while (puffer<>'') and (puffer[1]=' ') do delete(puffer,1,1);
while (puffer<>'') and (puffer[length(puffer)]=' ') do delete(puffer,length(puffer),1);
end;

procedure kompri (var puffer:string);
var   i:byte;
begin
i:=pos(' ',puffer);
while i>0 do begin delete(puffer,i,1); i:=pos(' ',puffer) end;
end;

function readint (text:string80; sonst:longint):longint;
var   puffer:string80;  code:integer;
      zahl:longint;
begin
puffer:=wort(sonst);
write(text,': ');
readstr(puffer); kompri(puffer); val(puffer,zahl,code);
while (code<>0) do begin
      puffer:=wort(sonst);
      fehler('Nur ganze Zahl eingeben: ');
      readstr(puffer); kompri(puffer); val(puffer,zahl,code);
      end;
readint:=zahl;
end;

function readext (text:string80; sonst:extended; l,n:byte):extended;
var   puffer:string80;  code:integer;
      zahl:extended;
begin
puffer:=extwort(sonst,l,n);
write(text,': ');
readstr(puffer); kompri(puffer); val(puffer,zahl,code);
while (code<>0) do begin
      puffer:=extwort(sonst,l,n);
      fehler('Nur Gleitkommazahl eingeben: ');
      readstr(puffer); kompri(puffer); val(puffer,zahl,code);
      end;
readext:=zahl;
end;

function readexte (text:string80; sonst:extended; a,b:byte):extended;
var   puffer:string80;  code:integer;
      zahl:extended;
begin
puffer:=extewort(sonst,a,b);
write(text,': ');
readstr(puffer); kompri(puffer); val(puffer,zahl,code);
while (code<>0) do begin
      puffer:=extewort(sonst,a,b);
      fehler('Nur Gleitkommazahl eingeben: ');
      readstr(puffer); kompri(puffer); val(puffer,zahl,code);
      end;
readexte:=zahl;
end;

function readchar (text:string80; sonst:char):char;
var   buchstabe:char;
begin
write(text,': ',sonst,#8); buchstabe:=readkey;
if buchstabe in [#13,#27] then begin readchar:=sonst; writeln end
                          else begin
   readchar:=buchstabe;
   if buchstabe in [' '..#255] then writeln(buchstabe);
   end;
end;

function readstring (text:string80; sonst:string80):string80;
var   puffer:string80;
begin
write(text,': ');
puffer:=sonst; readstr(puffer); schieben(puffer);
readstring:=puffer;
end;

end.