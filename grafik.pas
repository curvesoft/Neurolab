{Borland-Pascal 7.0 / FPC 2.4 }

unit  grafik;

{$ifdef msdos} {$G-,O-} {$endif}
{$ifdef DPMI}  {$C moveable preload permanent} {$endif}
{$ifdef win32} { $ define dauergrafik} {$define wingraph} {$endif}

{$A+,B-,E+,F-,I-,N+,P+,T+,V-,X-}

interface

var   gdriver, gmode : integer;
      vesa16:integer;

const
      {$ifdef fpc}
      grtreiber:integer=9;
      grmodus:integer=2;
      {$else}
      grtreiber:integer=255;
      grmodus:integer=255;
      {$endif}
      vesa1=0;
      vesa2=1;
      vesa3=2;

procedure opengraph;
procedure closegraph;

implementation

uses  {$ifdef wingraph} crt,dos,wingraph; {$else} crt,dos,graph; {$endif}


{$ifdef msdos}

procedure egavga; external;
{$L EGAVGA.OBJ}

procedure herc; external;
{$L HERC.OBJ}

{$endif}

procedure opengraph;
begin
{$ifndef dauergrafik}
{$ifdef wingraph}
gdriver:=nopalette; gmode:=m800x600;
initgraph(gdriver,gmode,'Neurolab-Grafik');
{$else}
if grtreiber=255 then detectgraph(gdriver,gmode) else gdriver:=grtreiber;
if grmodus<>255 then gmode:=grmodus;
initgraph(gdriver,gmode,getenv('BGI'));
{$endif}
if gdriver<0 then begin
   writeln('No grafic card or driver missing.');
   halt end;
{$endif}
end;

procedure closegraph;
begin
{$ifndef dauergrafik}
{$ifdef wingraph} wingraph.closegraph; {$else} graph.closegraph; {$endif}
{$endif}
end;

begin
{$ifdef msdos}
if registerbgidriver(@egavga)<0 then begin
   writeln('EGAVGA-driver lost'); halt end;
if registerbgidriver(@herc)<0 then begin
   writeln('Hercules-driver lost'); halt end;
{$else}
{$ifndef wingraph} vesa16:=installuserdriver('vesa16',nil); {$endif}
{$endif}

{$ifdef dauergrafik}
if grtreiber=255 then detectgraph(gdriver,gmode) else gdriver:=grtreiber;
if grmodus<>255 then gmode:=grmodus;
initgraph(gdriver,gmode,getenv('BGI'));
{$endif}
end.
