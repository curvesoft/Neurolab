{ Borland-Pascal 7.0 / FPC 2.4 }

unit  wavpcm;

{$IFDEF MSDOS}
{$A+,B-,E+,F-,G-,I-,N+,O-,P+,T+,V+,X-}
{$ELSE}
{$A+,B-,E+,F-,G+,I-,N+,P+,T+,V+,X-}
{$ENDIF}

interface

uses  {$ifdef fpc} crt, dos, objects, {$else}
         {$ifdef windows} wincrt, windos, dostowin,{$else} crt, dos, objects, {$endif}
      {$endif}
      bequem, daff;

procedure kopf (name:fnamestr; var kodaten:kopfdaten);
procedure kopfplatzschreiben (name:string80);
procedure kopfschreiben (name:string80; kodaten:kopfdaten);


implementation

type  header=packed record
        riff:array[1..4] of char;
        len:longint;
        wave:array[1..4] of char;
        ckid:array[1..4] of char;
        nchunksize:longint;
        wformattag:word;
        nchannels:word;
        nsamplespersec:longint;
        navgbytespersec:longint;
        nblockalign:word;
        nbitspersample:word;
        end;
      chunk=packed record
        ckid:array[1..4] of char;
        nchunksize:longint;
        end;

procedure kopf (name:fnamestr; var kodaten:kopfdaten);
var   ko:header; sampbytes:word;
      stamp:longint; dt:datetime;
      ch:chunk;
      i:byte;
begin
tulabfehler:=true;
assign(daten,name);
reset(daten,1); if lesefehler then exit;
blockread(daten,ko,sizeof(header)); if lesefehler then exit;
if ko.riff<>'RIFF' then begin
   close(daten);
   daff.kopf(name,kodaten);
   end               else begin
   blockread(daten,ch,sizeof(chunk)); if lesefehler then exit;
   while ch.ckid<>'data' do begin
      seek(daten,filepos(daten)+ch.nchunksize); if lesefehler then exit;
      blockread(daten,ch,sizeof(chunk)); if lesefehler then exit;
      end;
   with kodaten do begin
      produzent:='WAV-Datei';
      getftime(daten,stamp); unpacktime(stamp,dt);
      datum:=wort(dt.day)+'.'+wort(dt.month)+'.'+wort(dt.year);
      uhrzeit:=wort(dt.hour)+':'+copy('0'+wort(dt.min),1,2);
      sampbytes:=(ko.nbitspersample+7) div 8;
      kopfbytes:=filepos(daten);
      anzahl:=ch.nchunksize div (ko.nchannels * sampbytes);
      freq:=ko.nsamplespersec;
      case sampbytes of 1:gesdattyp:=4; 2:gesdattyp:=7; else exit end;
      kennung:=name;
      nkan:=ko.nchannels;
      bytes:=nkan*sampbytes;
      for i:=0 to nkan-1 do with k[i] do begin
         nr:=i;
         name:=wort(i)+'.Chan.';
         faktor1:=100/maxsample;
         faktor2:=1;
         einheit:='%';
         offs:=i*sampbytes;
         dattyp:=gesdattyp;
         bits:=ko.nbitspersample;
         end;
      for i:=nkan to maxkan-1 do with k[i] do begin
         nr:=i;
         name:=' - ';
         faktor1:=1;
         faktor2:=1;
         einheit:='E';
         offs:=0;
         dattyp:=0;
         end;
      end;
   close(daten); if lesefehler then exit;
   tulabfehler:=false;
   end;
end;

procedure kopfplatzschreiben (name:string80);
var   s:file;
      ko:header;
      ch:chunk;
      l:word;
begin
fillchar(ko,sizeof(ko),#0); fillchar(ch,sizeof(ch),#0);
assign(s,name);
rewrite(s,1);
blockwrite(s,ko,sizeof(ko));
blockwrite(s,ch,sizeof(ch));
close(s);
end;

procedure kopfschreiben (name:string80; kodaten:kopfdaten);
var   s:file;
      ko:header;
      ch:chunk;
      i:longint;
      a,b,c,d:word;
begin
fillchar(ko,sizeof(ko),#0);
assign(s,name);
reset(s,1);
with ko do begin
   riff:='RIFF';
   len:=filesize(s)-8;
   wave:='WAVE';
   ckid:='fmt ';
   nchunksize:=16;
   wformattag:=1;
   nchannels:=kodaten.nkan;
   nsamplespersec:=round(kodaten.freq);
   navgbytespersec:=round(kodaten.freq*kodaten.nkan*2);
   nblockalign:=kodaten.nkan*2;
   nbitspersample:=16;
   end;
with ch do begin
   ckid:='data';
   nchunksize:=filesize(s)-sizeof(ko)-sizeof(ch);
   end;
blockwrite(s,ko,sizeof(ko));
blockwrite(s,ch,sizeof(ch));
close(s);
end;


end.
