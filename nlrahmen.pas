{ Borland-Pascal 7.0 / FPC 2.0 }

unit  nlrahmen;

{$ifdef fpc} {$R *.res} {$endif}

{$IFDEF MSDOS}
{$A+,B-,E+,F-,G-,I-,N+,O-,P+,T+,V+,X-}
{$ELSE}
{$A+,B-,E+,F-,G+,I-,N+,P+,T+,V+,X-}
{$ENDIF}

interface

uses  crt,
      bequem,tlfilter,tlfiles;

const farbenormal=lightgray;
      farbe1=cyan; farbe2=green; farbe3=magenta; farbe4=magenta;

type  kanalmenge=object
         kn:byte;
         k:array [1..maxkanal] of byte;
         dabei:set of 0..maxkanal;
         procedure voreinstellung;
         function ausgabe:string80;
         procedure lesen(enn:byte; farbe:byte);
         end;

      filterliste=object
         ende:boolean;
         index:byte;
         procedure zeigen (a,i:byte);
         procedure weiterzeigen;
       private
         anz:byte;
         zn:byte;
         end;

var   zeilmax:byte;
      kanaele:kanalmenge;

procedure zwischen (titel:string20; farbe:byte);
procedure ueberschrift (gross:boolean; schrift:string80; titel:string20;
                        farbe:byte);

procedure belegungzeigen;

implementation

const strich=
  '-------------------------------------------------------------------------------';

procedure zwischen (titel:string20; farbe:byte);
var   wy:byte;
begin
textcolor(farbe);
titel:=' '+titel+' ';
wy:=wherey; write(strich); gotoxy(4,wy); writeln(titel);
textcolor(farbenormal);
end;

procedure ueberschrift (gross:boolean; schrift:string80; titel:string20;
                        farbe:byte);
begin
if gross<>(zeilmax>25) then begin
   if gross then textmode(c80+font8x8)
            else textmode(c80);
   zeilmax:=hi(windmax)+1;
   end;
window(1,1,80,zeilmax); clrscr; textcolor(farbe);
write(strich);
schrift:=' '+schrift+' '; titel:=' '+titel+' ';
gotoxy(40-length(schrift) div 2,1); write(schrift);
gotoxy(4,1); write(titel);
window(1,3,80,zeilmax); textcolor(farbenormal);
end;

procedure belegungzeigen;
var   i:byte;
begin
writeln('Channel':8,'Label':7);
for i:=0 to 7 do begin
    if i<kan then write(i:2,' : ',schriftliste[i]:10,'':25);
    if i+8<kan then write(i+8:2,' : ',schriftliste[i+8]);
    writeln end;
end;

procedure filterliste.zeigen (a,i:byte);
begin
anz:=a;
writeln('Channel':5,'Filters':12);
zn:=wherey+hi(windmin); index:=i;
gotoxy(1,wherey+anz);
weiterzeigen;
end;

procedure filterliste.weiterzeigen;
label  voll;
var    windminalt,windmaxalt:word;
       wy,wx:byte;
begin
wy:=wherey; wx:=wherex;
windminalt:=windmin; windmaxalt:=windmax;
ende:=false;
window(1,zn,80,zn+anz); clrscr;
while not ende do begin
   if index>=kan+filtermax-1 then ende:=true;
   if filterdrin(index) then begin
      if wherey>=anz then goto voll;
      writeln(index:2,' : ',filterzeile(index));
      end;
   inc(index);
   end;
voll:window(lo(windminalt)+1,hi(windminalt)+1,lo(windmaxalt)+1,hi(windmaxalt)+1);
gotoxy(wx,wy);
if ende then index:=kan;
end;

procedure kanalmenge.voreinstellung;
var   i:byte;
begin
for i:=1 to kan do k[i]:=pred(i); kn:=kan;
end;

function kanalmenge.ausgabe:string80;
var   puffer:string80;
      i:byte;
begin
puffer:='';
for i:=1 to kn do puffer:=puffer+wort(k[i])+' ';
ausgabe:=puffer;
end;

procedure kanalmenge.lesen (enn:byte; farbe:byte);
var   i:byte;
      puffer,ts:string80;
      tk:longint;
      liste:filterliste;
   procedure fehler (ausgabetext:string);
   begin
   bequem.fehler(ausgabetext); zoeger(3000); writeln;
   end;
begin
belegungzeigen; writeln;
liste.zeigen(enn,kan); writeln;
zwischen('Dialogue',farbe);
write(lfcr,'Continue list? (Y/N) ');
while not liste.ende and (readkey in ['Y','y','J','j']) do liste.weiterzeigen;
write(#13); clreol;
puffer:=readstring('Channels',ausgabe);
kn:=0; i:=1;
while (i<=length(puffer)) and (kn<=maxkanal) do begin
   ts:='';
   while not(puffer[i] in ['0'..'9']) do
      if i>length(puffer) then exit else inc(i);
   while (puffer[i] in ['0'..'9']) and (i<=length(puffer)) do begin
      ts:=ts+puffer[i]; inc(i) end;
   if length(ts)<=20 then tk:=zahl(ts) else tk:=-1;
   if tk in [0..kan+filtermax-1] then
     if (tk<kan) or (filterdrin(tk)) then begin inc(kn); k[kn]:=tk end
                           else fehler('Channel '+wort(tk)+' not defined.')
                         else fehler('Channel '+wort(tk)+' not existing.');
   end;
dabei:=[]; for i:=1 to kn do dabei:=dabei+[k[i]];
end;

begin
zeilmax:=hi(windmax)+1;
end.
