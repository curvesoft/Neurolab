{Turbo-Pascal 6.0}

unit  grafik;

{$A+,B-,F-,G+,I-,N+,O-,V-,X-}

interface

uses  crt,graph;

var   gdriver,gmode:integer;
      maxx,maxy:word;

procedure opengraph;
procedure closegraph;

procedure fenster (ax,ay,bx,by:word);

implementation

procedure egavga; external;
{$L EGAVGA.OBJ}

procedure herc; external;
{$L HERC.OBJ}

procedure opengraph;
begin
assign(output,''); rewrite(output);
detectgraph(gdriver,gmode);
case gdriver of
   EGA:gmode:=EGAhi;
   EGA64:gmode:=EGA64hi;
   VGA:gmode:=VGAhi;
   end;
initgraph(gdriver,gmode,'');
if gdriver<0 then begin
   writeln('Keine Grafikkarte oder Treiber fehlt.');
   halt end;
maxx:=getmaxx; maxy:=getmaxy;
end;

procedure closegraph;
begin
graph.closegraph;
assigncrt(output); rewrite(output);
end;

procedure fenster (ax,ay,bx,by:word);
begin
setviewport(ax,ay,bx,by,clipon); clearviewport;
setlinestyle(solidln,0,normwidth);
moveto(2,2); lineto(2,by-ay-4); lineto(bx-ax-4,by-ay-4);
lineto(bx-ax-4,2); lineto(2,2);
end;

begin
if registerbgidriver(@egavga)<0 then begin
   writeln('EGAVGA-Treiber verloren'); halt end;
if registerbgidriver(@herc)<0 then begin
   writeln('Hercules-Treiber verloren'); halt end;
end.