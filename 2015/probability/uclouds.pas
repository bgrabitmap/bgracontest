unit UClouds;

{$mode objfpc}

interface

uses
  Classes, SysUtils, BGRAOpenGL, BGRABitmap, BGRABitmapTypes, BGRAGraphics;

type
  { TCloudSprite }

  TCloudSprite = class(TBGLSprite)
    horizPhase,vertPhase: single;
    horizPhaseStep,vertPhaseStep: single;
    horizMoveRadius,vertMoveRadius: single;
    rotationSpeed: single;
    speed: TPointF;
    constructor Create(ATexture: IBGLTexture);
    procedure OnTimer; override;
  end;

  { TClouds }

  TClouds = class
    clouds: array of TCloudSprite;
    background: IBGLTexture;
    backgroundX: single;
    constructor Create(ANbClouds: integer);
    procedure DrawBackground;
    destructor Destroy; override;
  end;


implementation

uses BGRAGradients;

{ TClouds }

constructor TClouds.Create(ANbClouds: integer);
var
  i,j: Integer;
  p: PBGRAPixel;
  radial,noise: TBGRABitmap;
  ballImage: TBGLBitmap;
  ballTex: IBGLTexture;
  backgroundBmp: TBGLBitmap;
begin
  backgroundX := 0;
  backgroundBmp := TBGLBitmap.Create(512,512);
  noise := CreateCyclicPerlinNoiseMap(512,512,4,4,1,rfSpline);
  backgroundBmp.Fill(noise);
  noise.Free;
  backgroundBmp.FillRect(0,0,backgroundBmp.Width,backgroundBmp.Height,BGRA(0,0,0,128),dmDrawWithTransparency);
  background := backgroundBmp.MakeTextureAndFree;

  setlength(clouds, ANbClouds);
  radial := TBGRABitmap.Create(128,128);
  radial.GradientFill(0,0,radial.Width,radial.Height, BGRAWhite, BGRAPixelTransparent,
    gtRadial, PointF((radial.Width-1)/2,(radial.Height-1)/2), PointF(0.5,(radial.Height-1)/2), dmSet);
  p := radial.Data;
  for j := 0 to radial.NbPixels-1 do //it is possible to avoid calling ScanLine because image size is a power of 2
  begin
    p^.alpha := round(p^.alpha*sqrt(p^.alpha+1)/16);
    inc(p);
  end;

  for i := 0 to high(clouds) do
  begin
    if i mod 10 = 0 then
    begin
      ballImage := TBGLBitmap.Create(radial);
      noise := CreatePerlinNoiseMap(128,128);
      ballImage.ApplyMask(noise, rect(0,0,128,128), Point(0,0));
      noise.Free;
      ballTex := ballImage.MakeTextureAndFree;
    end;

    clouds[i] := TCloudSprite.Create(ballTex);
    with clouds[i] do
    begin
      X := random(BGLCanvas.Width+round(2*W))-W;
      Y := random(BGLCanvas.Height+round(2*H))-H;
      Color := BGRA(0,random(100)+80,160+random(220-160),255);
      if W < 255 then Alpha := Round(W);
    end;
  end;

  radial.Free;
end;

procedure TClouds.DrawBackground;
var x: single;
    yb: integer;
begin
  x := backgroundX;
  while x < BGLCanvas.Width do
  begin
    for yb := 0 to ((BGLCanvas.Height+background.Height-1) div background.Height) -1 do
      background.Draw(x,yb*background.Height, BGRA(0,100,140));
    x += background.Width;
  end;
  backgroundX -= 0.3;
  if backgroundX < -background.Width then backgroundX += background.Width;
end;

destructor TClouds.Destroy;
begin
  inherited Destroy;
end;

{ TCloudSprite }

constructor TCloudSprite.Create(ATexture: IBGLTexture);
begin
  inherited Create(ATexture,0);
  HorizontalAlign := taCenter;
  VerticalAlign := tlCenter;
  horizPhase := random(360)*Pi/180;
  vertPhase := random(360)*Pi/180;
  horizPhaseStep := (random(10)+1)/3*Pi/180;
  vertPhaseStep := (random(10)+1)/3*Pi/180;
  horizMoveRadius := (random(50)+5)*2*horizPhaseStep/Pi;
  vertMoveRadius := (random(50)+5)*2*vertPhaseStep/Pi;
  speed := PointF((random(101)-50)/20,(random(101)-50)/20);
  rotationSpeed := (random(21)-10)/10;
  W := random(BGLCanvas.Width div 2)+150;
  H := W;
end;

procedure TCloudSprite.OnTimer;
begin
  inherited OnTimer;
  horizPhase += horizPhaseStep;
  vertPhase += vertPhaseStep;
  X := X + sin(horizPhase)*horizMoveRadius;
  Y := Y + sin(vertPhase)*vertMoveRadius;
  Location := Location + Speed;
  Angle := Angle + rotationSpeed;
  if (Speed.X > 0) and (X > BGLCanvas.Width+W) then X := X - (BGLCanvas.Width+2*W);
  if (Speed.X < 0) and (X < -H) then X := X + (BGLCanvas.Width+2*W);
  if (Speed.Y > 0) and (Y > BGLCanvas.Height+H) then Y := Y - (BGLCanvas.Height+2*H);
  if (Speed.Y < 0) and (Y < -H) then Y := Y + (BGLCanvas.Height+2*H);
end;

end.

