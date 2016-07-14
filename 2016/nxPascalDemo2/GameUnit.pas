unit GameUnit;

{$mode objfpc}{$H+}

interface

uses Classes, Dialogs, SysUtils, LCLType, math, nxMath, nxMath3D, nxGL,
  nxGame, nxGLextra, nxTypes;

const NUM_SPHERES = 100;

type

  { TMover }

  TMover = class
    position, velocity: TVector;
    radius, a: single;
    color: TRGB;
    trail: pTrail;
  public
    constructor Create;
  end;

  { TGame }

  TGame = class(TGameHandler)
  public
    playerPos: TVector2f;
    pt: TParticleEngine;
    frameIndex: cardinal;
    rotation: TVector;
    movers: array of TMover;
    constructor Create;
    destructor Destroy; override;
    procedure GameLoop; override;
  end;

implementation

var game: TGame;

{ TMover }

constructor TMover.Create;
begin
  position:=vector(random*2-1, random*2-1, random*2-1);
  velocity:=vector(random*2-1, random*2-1, random*2-1)*0.02;
  radius:=0.1+random*0.2;
  a:=random*2*pi;
  color:=rgb(30+random(226), 30+random(226), 30+random(226));
  trail:=game.pt.AddTrail(0, 100, radius, position.x, position.y, position.z);
  with trail^ do begin
    r:=color.r/255;
    g:=color.g/255;
    b:=color.b/255;
  end;
end;

{ TCustomGame }

constructor TGame.Create;
var i: integer;
begin
  inherited Create;
  game:=self;
  Randomize;
  pt:=TParticleEngine.Create;
  setlength(movers, NUM_SPHERES);
  for i:=0 to high(movers) do movers[i]:=TMover.Create;
  // Setlength() causes trail pointers to scramble,
  // so here we set them after they are all made
  for i:=0 to high(movers) do movers[i].trail:=@pt.trail[i];
end;

destructor TGame.Destroy;
var i: integer;
begin
  pt.Free;
  for i:=0 to high(movers) do movers[i].Free;
  inherited Destroy;
end;

procedure TGame.GameLoop;
var i: integer; v: TVector;
    a, d: single;
begin
  inc(frameIndex);
  //if frameIndex mod 2 = 0 then
  pt.Move3D;

  if mb[1] then begin
    rotation.x:=rotation.x+mDelta.x;
    rotation.y:=minmax(rotation.y+mDelta.y, -80,80);
  end;

  for i:=0 to high(movers) do
    with movers[i] do begin
      position:=position+velocity;
      {if position.x < -2.5 then velocity.x:=abs(velocity.x)
      else if position.x > 2.5 then velocity.x:=-abs(velocity.x);
      if position.y < -2 then velocity.y:=abs(velocity.y)
      else if position.y > 2 then velocity.y:=-abs(velocity.y);
      if position.z < -2 then velocity.z:=abs(velocity.z)
      else if position.z > 2 then velocity.z:=-abs(velocity.z);}
      d:=position.x*position.x+position.y*position.y+position.z*position.z;
      if d > 6 then begin
        position:=position-velocity;
        velocity:=reflect(velocity, norm(position));
      end;

      a:=a+0.05;
      v.x:=position.x + cos(a)*radius*0.5;
      v.y:=position.y + sin(a)*radius*0.5;
      v.z:=position.z;
      pt.SetTrailPos(trail, v);
      //pt.SetTrailPos(@pt.trail[i], position);
    end;
end;

end.

