{Turbo-Pascal 6.0}

unit  tulab;

{$A+,B-,F-,G+,I-,N+,O-,V-,X-}

interface

uses  crt, bequem;

const maxkan=16;

type  gainfeld=array[0..15] of byte;
      namenfeld=array[0..15] of string[10];
      kopfdaten=record
        datum:string20;     uhrzeit:string20;   anzahl:longint;
        freq:extended;      kennung:string[64]; names:namenfeld;
        gain:gainfeld;      nkan:byte;
        end;

const richtung:(vow,ruw,mit)=vow;

var   tulabfehler:boolean;

procedure kopf (name:string80; var kodaten:kopfdaten);
procedure oeffne (name:string80);
function lese (position:longint; kanal:byte):integer;
procedure schliesse;

procedure ausserbetrieb;

implementation

const maxpuffer=32766;
      zustand:(offen,zu,aus)=zu;

type  pufferfeld=array[0..maxpuffer] of integer;
      header=record
        id:string[8];                      nchan:byte;
        gain:gainfeld;                     names:namenfeld;
        frqkts:string[10];                 frqsts:string[10];
        bufferlen:string[9];               jahr:integer;
        monat:byte;                        tag:byte;
        stunde:byte;                       minute:byte;
        kennung:string[64];                board:byte;
        base:integer;                      dmakan1,dmakan2:byte;
        intlev:byte;                       inprange:byte;
        coding:byte;                       inpmode:byte;
        ffu:array[312..512] of byte
        end;

var   daten:file;
      puffer:^pufferfeld;
      pufferanfang,pufferende:longint;
      fileende:longint;
      kan:byte;

procedure kopf (name:string80; var kodaten:kopfdaten);
var   ko:header;
      i:byte;
begin
tulabfehler:=true;
assign(daten,name);
reset(daten,2); if lesefehler then exit;
blockread(daten,ko,256); if lesefehler then exit;
if ko.id<>'TurboLab' then begin fehler('Kein TurboLab-File.');
   exit end;
with kodaten do begin
   datum:=wort(ko.tag)+'.'+wort(ko.monat)+'.'+wort(ko.jahr);
   uhrzeit:=wort(ko.stunde)+':'+copy('0'+wort(ko.minute),1,2);
   anzahl:=(filesize(daten)-256) div ko.nchan;
   freq:=zahl(ko.frqkts);
   kennung:=ko.kennung;
   names:=ko.names;
   for i:=0 to ko.nchan-1 do if names[i]='' then names[i]:=wort(i)+'.Kanal';
   for i:=ko.nchan to 15 do names[i]:=' - ';
   gain:=ko.gain;
   nkan:=ko.nchan;
   end;
close(daten); if lesefehler then exit;
tulabfehler:=false;
end;

procedure oeffne (name:string80);
var   puf:array[0..1] of byte;
begin
if zustand=aus then exit;
assign(daten,name); reset(daten,2);
seek(daten,4); blockread(daten,puf,1);
tulabfehler:=ioresult>0;
if not tulabfehler then begin
   kan:=puf[1]; fileende:=(filesize(daten)-256) div kan;
   zustand:=offen;
   end
                   else begin
   writeln(lfcr); fehler('Zugriff auf Datei "'+name+'" nicht m”glich.'); warte end;
pufferanfang:=0;  pufferende:=0;
end;

function lese (position:longint; kanal:byte):integer;
var   anfang:longint;
      lenpuffer:word;
begin
if (position<pufferanfang) or (position>=pufferende) then begin
   if zustand<>offen then begin lese:=0; exit end;
   if (position<0) or (position>=fileende) then begin
      lese:=2048; exit end;
   case richtung of
      vow:anfang:=position;
      ruw:anfang:=max(position+1-maxpuffer div kan,0);
      mit:anfang:=max(position+1-maxpuffer div (2*kan),0);
      end;
   seek(daten,anfang*kan+256);
   blockread(daten,puffer^,maxpuffer,lenpuffer);
   tulabfehler:=ioresult>0;
   if not tulabfehler then begin
      pufferanfang:=anfang;
      pufferende:=pufferanfang+lenpuffer div kan end
                      else begin
      writeln(lfcr); fehler('Datei verschwunden.'); warte;
      pufferanfang:=0;
      pufferende:=0;
      zustand:=zu; lese:=0; exit end
   end;
lese:=puffer^[(position-pufferanfang)*kan+kanal];
end;

procedure schliesse;
begin
if zustand=offen then begin
   close(daten);
   tulabfehler:=ioresult>0;
   if tulabfehler then begin
      writeln(lfcr); fehler('Datei verschwunden.'); warte end;
   pufferanfang:=0; pufferende:=0;
   end;
end;

procedure ausserbetrieb;
begin
dispose(puffer); zustand:=aus;
end;

begin
new(puffer);
pufferanfang:=0; pufferende:=0;
end.
