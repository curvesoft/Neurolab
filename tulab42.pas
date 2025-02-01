{ Borland-Pascal 7.0 / FPC 3.2.2 }
{$ifdef fpc} {$mode TP} {$endif}

unit  tulab42;

{$IFDEF MSDOS}
{$A+,B-,E+,F-,G-,I-,N+,O-,P+,T+,V+,X-}
{$ELSE}
{$A+,B-,E+,F-,G+,I-,N+,P+,T+,V+,X-}
{$ENDIF}

interface

uses  crt, dos, objects,
      bequem, daff, wavpcm;

procedure kopf (const name:fnamestr; var kodaten:headerdata);
procedure kopfplatzschreiben (name:string80);
procedure kopfschreiben (name:string80; kodaten:headerdata);

implementation

type  gainfeld=packed array[0..15] of byte;
      nlgainfeld=packed array[0..15] of extended;
      namenfeld=packed array[0..15] of string[10];
      header=packed record
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
        nl:string[8];                      nlgain:nlgainfeld;
        ffu:array[481..512] of byte
        end;

function klammereinheit (aus:string) :string;
var i:byte;
begin
klammereinheit:='';
i:=pos('[',aus); if i=0 then exit;
delete(aus,1,i);
i:=pos(']',aus); if i=0 then exit;
delete(aus,i,length(aus)-i+1);
schieben(aus);
klammereinheit:=aus;
end;

function ohneklammereinheit (aus:string) :string;
var i,j:byte;
begin
ohneklammereinheit:=aus;
i:=pos('[',aus); if i=0 then exit;
j:=pos(']',aus); if j=0 then exit;
delete(aus,i,j-i+1);
schieben(aus);
ohneklammereinheit:=aus;
end;

procedure kopf (const name:fnamestr; var kodaten:headerdata);
var   ko:header;
      i:byte;
begin
tulabfehler:=true;
assign(daten,name);
reset(daten,2); if lesefehler then exit;
blockread(daten,ko,256); if lesefehler then exit;
if ko.id<>'TurboLab' then begin
   close(daten);
   wavpcm.kopf(name,kodaten);
   end               else begin
   with kodaten do begin
      if ko.nl='NeuroLab' then producer:='NEUROLAB'
                          else producer:='TL 3.0';
      productdate:=wort(ko.tag)+'.'+wort(ko.monat)+'.'+wort(ko.jahr);
      producttime:=wort(ko.stunde)+':'+copy('0'+wort(ko.minute),1,2);
      headerbytes:=512;
      datalength:=(filesize(daten)-256) div ko.nchan;
      frequency:=zahl(ko.frqkts);
      datatype:=6;
      protocol:=ko.kennung;
      channelnumber:=ko.nchan;
      bytes:=channelnumber*2;
      for i:=0 to channelnumber-1 do with channels[i] do begin
         nr:=i;
         name:=ohneklammereinheit(ko.names[i]); channelunit:=klammereinheit(ko.names[i]);
         if name='' then name:=wort(i)+'.Chan.';
         if ko.nl='NeuroLab' then factor1:=ko.nlgain[i]
                             else factor1:=10/ko.gain[i]/maxsample;
         factor2:=1;
         if channelunit='' then if ko.nl='NeuroLab' then channelunit:='U'
                                                else channelunit:='V';
         offs:=i*2;
         dattyp:=6;
         bits:=12;
         end;
      for i:=channelnumber to maxchannels-1 do with channels[i] do begin
         nr:=i;
         name:=' - ';
         factor1:=1;
         factor2:=1;
         channelunit:='U';
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
begin
fillchar(ko,sizeof(ko),#0);
assign(s,name);
rewrite(s,1);
blockwrite(s,ko,512);
close(s);
end;

procedure kopfschreiben (name:string80; kodaten:headerdata);
var   s:file;
      ko:header;
      i:longint;
      a,b,c,d:word;
begin
fillchar(ko,sizeof(ko),#0);
assign(s,name);
reset(s,1);
with ko do begin
   id:='TurboLab'; nl:='NeuroLab';
   str(kodaten.frequency:10:3,frqkts);
   str(kodaten.frequency*kodaten.channelnumber:10:3,frqsts);
   kennung:=kodaten.protocol;
   nchan:=kodaten.channelnumber;
   getdate(a,b,c,d);
   jahr:=a; monat:=b; tag:=c;
   gettime(a,b,c,d);
   stunde:=a; minute:=b;
   for i:=0 to nchan-1 do begin
      names[i]:=kodaten.channels[i].name;
      gain[i]:=1; nlgain[i]:=kodaten.channels[i].factor1*kodaten.channels[i].factor2;
      end;
   for i:=nchan to 15 do begin
      names[i]:='';
      gain[i]:=0;
      end;
   str((filesize(s)-512) div 2:9,bufferlen);
   board:=3;
   base:=0;
   dmakan1:=1; dmakan2:=2;
   intlev:=15;
   inprange:=2;
   coding:=1;
   inpmode:=2;
   end;
blockwrite(s,ko,512);
close(s);
end;

end.
