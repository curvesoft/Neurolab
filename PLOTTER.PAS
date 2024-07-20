{Turbo-Pascal 6.0}

unit plotter;

{$A+,B-,F-,G+,I-,N+,O-,V-,X-}

interface

uses  bequem;

const plin='IN;';         plpd='PD;';
      plpu='PU;';         plyt='YT;';
      plxt='XT;';

type  plotstring=string[50];

var   plt:text;

function plip (p1x,p1y,p2x,p2y:word):plotstring;
function plsc (xmin,xmax,ymin,ymax:extended):plotstring;
function plpa (x,y:extended):plotstring;
function pllb (schreib:string):string;

implementation

var   p1x,p1y,p2x,p2y:word;
      x0,y0:word;
      fakx,faky:extended;

function plip (p1x,p1y,p2x,p2y:word):plotstring;
begin
plotter.p1x:=p1x; plotter.p1y:=p1y;
plotter.p2x:=p2x; plotter.p2y:=p2y;
plip:='IP'+wort(p1x)+','+wort(p1y)+','+wort(p2x)+','+wort(p2y)+';';
end;

function plsc (xmin,xmax,ymin,ymax:extended):plotstring;
begin
fakx:=(p2x-p1x)/(xmax-xmin);
faky:=(p2y-p1y)/(ymax-ymin);
x0:=round(p1x-xmin*fakx);
y0:=round(p1y-ymin*faky);
plsc:='';
end;

function plpa (x,y:extended):plotstring;
begin
plpa:='PA'+wort(round(x0+x*fakx))+','+wort(round(y0+y*faky))+';';
end;

function pllb (schreib:string):string;
begin
pllb:='LB'+schreib+#3;
end;

begin
assign(plt,'COM2');
end.