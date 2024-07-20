{Borland-Pascal 7.0}

unit  grafik;

{$ifdef msdos} {$G-,O-} {$endif}
{$ifdef DPMI}  {$C moveable preload permanent} {$endif}

{$A+,B-,E+,F-,I-,N+,P+,T+,V-,X-}

interface

var   gdriver, gmode : integer;
      vesa16:integer;

const grtreiber:integer=255;
      grmodus:integer=255;
      vesa1=0;
      vesa2=1;
      vesa3=2;

procedure opengraph;
procedure closegraph;

implementation

uses  crt,dos,graph;


{$ifdef msdos}

procedure egavga; external;
{$L EGAVGA.OBJ}

procedure herc; external;
{$L HERC.OBJ}

{$endif}

procedure opengraph;
begin
if grtreiber=255 then detectgraph(gdriver,gmode) else gdriver:=grtreiber;
if grmodus<255 then gmode:=grmodus;
initgraph(gdriver,gmode,getenv('BGI'));
if gdriver<0 then begin
   writeln('No grafic card or driver missing.');
   halt end;
end;

procedure closegraph;
begin
graph.closegraph;
end;

begin
{$ifdef msdos}
if registerbgidriver(@egavga)<0 then begin
   writeln('EGAVGA-driver lost'); halt end;
if registerbgidriver(@herc)<0 then begin
   writeln('Hercules-driver lost'); halt end;
{$else}
vesa16:=installuserdriver('vesa16',nil);
{$endif}
end.