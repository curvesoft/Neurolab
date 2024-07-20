{ Borland-Pascal 7.0 }

unit plotter;

{$IFDEF MSDOS}
{$A+,B-,E+,F-,G-,I-,N+,O-,P+,T+,V+,X-}
{$ELSE}
{$A+,B-,E+,F-,G+,I-,N+,P+,T+,V+,X-}
{$ENDIF}

interface

uses  bequem;

const plpd='PD;';
      plpu='PU;';

const plotterdevice:string80='COM2';
      plotformat:(ascii,hpgl,pcl5)=hpgl;

type  plotstring=string[50];

var   plt:text;

procedure assignplt(pltdev:string80);

procedure plotterin;
procedure plotterunin;

function plip (p1x,p1y,p2x,p2y:word):plotstring;
function plsc (xmin,xmax,ymin,ymax:extended):plotstring;
function plpa (x,y:extended):plotstring;
function pllb (schreib:string):string;
function pltl (l1,l2:extended):plotstring;
function plxt:plotstring;
function plyt:plotstring;
function plmu:plotstring;

function devicetest(testdevice:string80):boolean;

implementation

var   p1x,p1y,p2x,p2y:word;
      x0,y0:word;
      fakx,faky:extended;
      tlxp,tlxn,tlyp,tlyn:extended;

procedure assignplt(pltdev:string80);
begin
plotterdevice:=pltdev;
assign(plt,plotterdevice);
end;

procedure plotterin;
begin
rewrite(plt);
case plotformat of
   hpgl:;
   pcl5:write(plt,#27#37#49#66);
   end;
write(plt,'IN;');
case plotformat of
   hpgl:;
   pcl5:write(plt,'RO90;SP1;PW0;SD1,21,4,12,5,0,6,0,7,4148;SS;');
   end;
end;

procedure plotterunin;
begin
write(plt,plpu,'PA0,0;');
case plotformat of
   hpgl:;
   pcl5:write(plt,#27#37#48#65#27#69);
   end;
close(plt);
end;

function plip (p1x,p1y,p2x,p2y:word):plotstring;
begin
plotter.p1x:=p1x; plotter.p1y:=p1y;
plotter.p2x:=p2x; plotter.p2y:=p2y;
plip:='IP'+wort(p1x)+','+wort(p1y)+','+wort(p2x)+','+wort(p2y)+';';
tlxp:=0;   tlxn:=0.01*(p2y-p1y);
tlyp:=0;   tlyn:=0.006*(p2x-p1x);
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

function pltl (l1,l2:extended):plotstring;
begin
end;

function plxt:plotstring;
begin
plxt:='PD;PR0,'+wort(-round(tlxn))+',0,'+wort(round(tlxn))+';PU;';
end;

function plyt:plotstring;
begin
plyt:='PD;PR'+wort(-round(tlyn))+',0,'+wort(round(tlyn))+',0;PU';
end;

function plmu:plotstring;
const hpglmu='UC-99,-1,-2,99,1,2,0,6,0,-4,1,-2,1,0,1,2,0,4,-99';
      pcl5mu='LBu'#3;
begin
case plotformat of
   hpgl:plmu:=hpglmu;
   pcl5:plmu:=pcl5mu;
   end;
end;

function devicetest(testdevice:string80):boolean;
const tmax=7;
      tarray:array[1..tmax] of string20=
         ('PRN','LPT1','LPT2','COM1','COM2','NUL','CON');
var   i:byte;
begin
for i:=1 to length(testdevice) do testdevice[i]:=upcase(testdevice[i]);
for i:=1 to tmax do
   if testdevice=tarray[i] then begin
      devicetest:=true;
      exit end;
devicetest:=false;
end;

begin
assign(plt,plotterdevice);
end.