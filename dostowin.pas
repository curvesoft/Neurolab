{ Borland-Pascal 7.0 }

{$ifdef windows}
unit dostowin;

{$A+,B-,E+,F-,G+,I-,N+,P+,T+,V+,X+}

interface

uses windos, objects, strings;

const stopenread=objects.stopenread;

type   datetime=tdatetime;
       dirstr=string[67];
       namestr=string[8];
       extstr=string[4];

       FNameStr = string[79];
       tbufstream=object (objects.tbufstream)
                     constructor Init(FileName: FNameStr; Mode, Size: Word);
                     end;

procedure delay (ms:longint);

implementation

constructor TBufStream.Init(FileName: FNameStr; Mode, Size: Word);
var fname:objects.fnamestr;
begin
strpcopy(fname,filename);
inherited init(fname,mode,size);
end;

procedure delay (ms:longint);
begin
end;

{$endif}
end.
