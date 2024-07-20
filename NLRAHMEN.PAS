{Turbo-Pascal 6.0}

unit  nlrahmen;

{$A+,B-,F-,G+,I-,N+,O-,V-,X-}

interface

uses  crt,bequem,tulab,tlfilter,tlfiles;

const maxanzahl=16382;
      maxzeit=6000;

      farbenormal=lightgray;
      farbe1=cyan; farbe2=green; farbe3=magenta;

type  kanalliste=object
         kn:byte;
         k:array [1..maxkanal] of byte;
         procedure voreinstellung;
         function ausgabe:string80;
         procedure lesen(enn:byte);
         end;
      kanalmenge=object (kanalliste)
         dabei:set of 0..maxkanal;
         procedure lesen (enn:byte);
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

      mittelfeld=array[0..maxanzahl] of wert;
      mittelliste=array[0..maxkanal] of ^mittelfeld;
      zeitfeld=array[1..maxzeit] of extended;

var   kanaele:kanalmenge;


procedure zwischen (titel:string20; farbe:byte);
procedure ueberschrift (schrift:string80; titel:string20; farbe:byte);

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

procedure ueberschrift (schrift:string80; titel:string20; farbe:byte);
begin
window(1,1,80,25); clrscr; textcolor(farbe);
write(strich);
schrift:=' '+schrift+' '; titel:=' '+titel+' ';
gotoxy(40-length(schrift) div 2,1); write(schrift);
gotoxy(4,1); write(titel);
window(1,3,80,25); textcolor(farbenormal);
end;

procedure belegungzeigen;
var   i:byte;
begin
writeln('Kanal':5,'Belegung':12);
for i:=0 to 7 do begin
    if i<kan then write(i:2,' : ',schriftliste[i]:10,'':25);
    if i+8<kan then write(i+8:2,' : ',schriftliste[i+8]);
    writeln end;
end;

procedure filterliste.zeigen (a,i:byte);
begin
anz:=a;
writeln('Kanal':5,'Filterung':12);
zn:=wherey+hi(windmin); index:=i;
weiterzeigen;
gotoxy(1,wherey+anz);
end;

procedure filterliste.weiterzeigen;
label  voll;
var    i:byte;
       windminalt,windmaxalt:word;
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
voll:windmin:=windminalt; windmax:=windmaxalt;
gotoxy(wx,wy);
if ende then index:=kan;
end;

procedure kanalliste.voreinstellung;
var   i:byte;
begin
for i:=1 to kan do k[i]:=pred(i); kn:=kan;
end;

function kanalliste.ausgabe:string80;
var   puffer:string80;
      i:byte;
begin
puffer:='';
for i:=1 to kn do puffer:=puffer+wort(k[i])+' ';
ausgabe:=puffer;
end;

procedure kanalliste.lesen (enn:byte);
var   i:byte;
      puffer,ts:string80;
      tk:longint;
      liste:filterliste;
begin
belegungzeigen; writeln;
liste.zeigen(enn,kan); writeln;
zwischen('Dialog',farbe2);
write(lfcr,'Filterkanalliste weiterzeigen? (J/N) ');
while not liste.ende and (readkey in ['Y','y','J','j']) do liste.weiterzeigen;
write(#13); clreol;
puffer:=readstring('Kan„le',ausgabe);
kn:=0; i:=1;
while (i<=length(puffer)) and (kn<=maxkanal) do begin
   ts:='';
   while not(puffer[i] in ['0'..'9']) do
      if i>length(puffer) then exit else inc(i);
   while (puffer[i] in ['0'..'9']) and (i<=length(puffer)) do begin
      ts:=ts+puffer[i]; inc(i) end;
   if length(ts)<=20 then tk:=zahl(ts) else tk:=-1;
   if (tk in [0..kan+filtermax-1]) and ((tk<kan) or (filterdrin(tk))) then begin
      inc(kn); k[kn]:=tk end;
   end;
end;

procedure kanalmenge.lesen(enn:byte);
var  i:byte;
begin
kanalliste.lesen(enn);
dabei:=[]; for i:=1 to kn do dabei:=dabei+[k[i]];
end;

end.