{ Borland-Pascal 7.0 / FPC 3.2.2 }
{$ifdef fpc} {$mode TP} {$endif}

unit  wavpcm;

{$IFDEF MSDOS}
{$A+,B-,E+,F-,G-,I-,N+,O-,P+,T+,V+,X-}
{$ELSE}
{$A+,B-,E+,F-,G+,I-,N+,P+,T+,V+,X-}
{$ENDIF}

interface

uses  crt, dos, objects,
      bequem, daff;

procedure kopf (name:fnamestr; var kodaten:headerdata);
procedure kopfplatzschreiben (name:string80);
procedure kopfschreiben (name:string80; kodaten:headerdata);


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

procedure kopf (name:fnamestr; var kodaten:headerdata);
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
      producer:='WAV-Datei';
      getftime(daten,stamp); unpacktime(stamp,dt);
      productdate:=wort(dt.day)+'.'+wort(dt.month)+'.'+wort(dt.year);
      producttime:=wort(dt.hour)+':'+copy('0'+wort(dt.min),1,2);
      sampbytes:=(ko.nbitspersample+7) div 8;
      headerbytes:=filepos(daten);
      datalength:=ch.nchunksize div (ko.nchannels * sampbytes);
      frequency:=ko.nsamplespersec;
      case sampbytes of 1:datatype:=4; 2:datatype:=7; else exit end;
      protocol:=name;
      channelnumber:=ko.nchannels;
      bytes:=channelnumber*sampbytes;
      for i:=0 to channelnumber-1 do with channels[i] do begin
         nr:=i;
         name:=wort(i)+'.Chan.';
         factor1:=100/maxsample;
         factor2:=1;
         channelunit:='%';
         offs:=i*sampbytes;
         dattyp:=datatype;
         bits:=ko.nbitspersample;
         end;
      for i:=channelnumber to maxchannels-1 do with channels[i] do begin
         nr:=i;
         name:=' - ';
         factor1:=1;
         factor2:=1;
         channelunit:='E';
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

procedure kopfschreiben (name:string80; kodaten:headerdata);
var   s:file;
      ko:header;
      ch:chunk;
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
   nchannels:=kodaten.channelnumber;
   nsamplespersec:=round(kodaten.frequency);
   navgbytespersec:=round(kodaten.frequency*kodaten.channelnumber*2);
   nblockalign:=kodaten.channelnumber*2;
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
