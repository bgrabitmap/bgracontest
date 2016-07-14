unit GameUnit;

{$mode objfpc}{$H+}

interface

uses Classes, Dialogs, SysUtils, LCLType, math, nxMath, nxMath3D, nxGL,
  nxGame, nxGLextra, nxTypes;

type

  { TOrbiter }

  TOrbiter = class
    startDist, radius, mass: double;
    position, vel: TVector3f;
    color: TfRGB;
    parent: TOrbiter;
    child: array of TOrbiter;
  public
    constructor Create(const numChild: integer); overload;
    destructor Destroy; override;
    function AddChild: TOrbiter;
    function SetValues(const _mass, _radius: double; const _col: TfRGB; dist: double
      ): TOrbiter;
    procedure GWith(o: TOrbiter);
  end;

  { TGame }

  TGame = class(TGameHandler)
  public
    cam: TCamera;
    camPos, toPos: TVector;
    zoom, toZoom: double;
    root: TOrbiter;
    g: array[-15..15, -15..15] of double;
    gscale: single;
    moving: boolean;
    constructor Create;
    procedure GameLoop; override;
    function GAt(p: TVector): double;
  end;

implementation

{ TOrbiter }

constructor TOrbiter.Create(const numChild: integer);
var i: integer;
begin
  for i:=0 to numChild-1 do AddChild;
end;

destructor TOrbiter.Destroy;
var i: integer;
begin
  for i:=0 to high(child) do child[i].Free;
  inherited Destroy;
end;

function TOrbiter.AddChild: TOrbiter;
var index: integer;
begin
  index:=length(child);
  Setlength(child, index+1);
  child[index]:=TOrbiter.Create;
  result:=child[index];
  result.parent:=self;
end;

function TOrbiter.SetValues(const _mass, _radius: double; const _col: TfRGB; dist: double): TOrbiter;
var a, d: single;
begin
  mass:=_mass/2000000;
  radius:=_radius/1000;
  color:=_col;
  a:=random*PI*2;
  dist:=dist*0.01;
  position.x:=cos(a)*dist;
  position.z:=sin(a)*dist;
  if parent<>nil then begin
    d:=1000*mass/(dist*dist);
    a:=a-PI/2;
    vel.x:=cos(a)*d;
    vel.z:=sin(a)*d;
  end;
  result:=self;
end;

function dist(a, b: PVector): single;
begin
  result:=hypot(a^.x-b^.x, a^.z-b^.z);
end;

procedure TOrbiter.GWith(o: TOrbiter);
var i: integer; d: single;
begin
  d:=vectorsqr(position-o.position);
  if d > 0.0000001 then d:=((mass*o.mass)/d)*0.5
  else d:=0;
  vel.x:=vel.x+(o.position.x-position.x)*d;
  vel.z:=vel.z+(o.position.z-position.z)*d;
  for i:=0 to high(child) do GWith(child[i]);
end;

{ TCustomGame }

constructor TGame.Create;
begin
  inherited Create;
  mouseXSpeed:=1; mouseYSpeed:=mouseXSpeed;
  gscale:=1;

  // Using values 1000 times smaller than normal
  //                       Mass     Radius   Color  Distance (million km)
  root:=TOrbiter.Create(8);
  root.SetValues(         1988550, 696.342, frgb(1.0, 0.8, 0.5), 0); // Sun
  root.child[0].SetValues(0.3302,  2.4397,  frgb(0.7, 0.7, 0.7), 57); // Mercury
  root.child[1].SetValues(4.8685,  6.051,   frgb(0.8, 0.8, 0.7), 108); // Venus
  root.child[2].SetValues(5.9736,  6.371,   frgb(0.3, 0.4, 1.0), 150); // Earth
  root.child[3].SetValues(0.00015, 3.3895,  frgb(0.7, 0.6, 0.3), 228); // Mars
  root.child[4].SetValues(1898.6,  69.911,  frgb(0.7, 0.6, 0.3), 779); // Jupiter
  root.child[5].SetValues(568.46,  58.232,  frgb(0.7, 0.6, 0.3), 1430); // Saturn
  root.child[6].SetValues(86.832,  25.362,  frgb(0.7, 0.9, 1.0), 2880); // Uranus
  root.child[7].SetValues(102.43,  24.622,  frgb(0.1, 0.2, 1.0), 4500); // Neptune
end;

procedure TGame.GameLoop;
var mult: single; i, j: integer;
begin
  if mb[1] then begin
    mult:=0.002*zoom;
    toPos.x:=minmax(toPos.x-mDelta.x*mult, -50, 50);
    toPos.z:=minmax(toPos.z-mDelta.y*mult, -50, 50);
  end;
  zoom:=nxmath.Interpolate(zoom, tozoom, 0.2);
  campos:=interpolate(campos, toPos, 0.2);

  if moving then begin
    for i:=0 to high(root.child) do
      with root.child[i] do begin
        GWith(root);
        position:=position+vel;
      end;
  end;

  for j:=-15 to 15 do
    for i:=-15 to 15 do begin
      g[i, j]:=min(GAt(vector(i, 0, j)*gscale+campos)*2, 20);
    end;
end;

function TGame.GAt(p: TVector): double;
var i: integer; d: double;
begin
  result:=0;
  d:=vectorsqr(p);
  if d < 0.00000001 then d:=0.00000001;
  result:=result+(root.mass/d)*0.5;
  for i:=0 to high(root.child) do
    with root.child[i] do begin
      d:=vectorsqr(position-p);
      if d < 0.00000001 then d:=0.00000001;
      result:=result+(mass/d)*0.5;
    end;
end;

end.

