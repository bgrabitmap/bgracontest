unit UStars;

{$mode objfpc}

interface

uses
  Classes, SysUtils, BGRABitmapTypes,
  BGRAOpenGL, BGRABitmap;

type

  { TStars }

  TStars = class
    stars: array of record
        pt: TPointF;
        size: single;
        isEarth: boolean;
      end;
    centerPt: TPointF;
    starBmp: TBGLBitmap;
    earthTex: IBGLTexture;
    time: single;
    earthExpected,earthCreated: boolean;
    constructor Create(AWidth,AHeight: integer; AEarth: TBGRABitmap);
    procedure Elapse(ACreateEarth: boolean);
    procedure Render;
    destructor Destroy; override;
  end;

implementation

{ TStars }

constructor TStars.Create(AWidth,AHeight: integer; AEarth: TBGRABitmap);
var nbStars,i: integer;
begin
  nbStars := round(AWidth/800*AHeight);
  centerPt := PointF(AWidth*0.5, AHeight*0.5);
  setlength(stars, nbStars);
  for i := 0 to high(stars) do
  with stars[i] do
  begin
    pt := PointF(random(AWidth)-round(centerPt.x)-0.5, random(AHeight)-round(centerPt.y)-0.5);
    size := (random(AWidth)+1)/800;
    isEarth := false;
  end;
  starBmp := TBGLBitmap.Create(3,3);
  starBmp.SetPixel(1,1,BGRAWhite);
  earthTex := BGLTexture(AEarth);
  earthCreated:= false;
  earthExpected:= false;
  time := 0;
end;

procedure TStars.Elapse(ACreateEarth: boolean);
var
  i: Integer;
begin
  for i := 0 to high(stars) do
  with stars[i] do
  begin
    pt *= 1.01;
    size *= 1.01;
  end;
  time := time+1;
  centerPt := PointF(BGLCanvas.Width*0.5, BGLCanvas.Height*0.5)+
    PointF((sin(time*0.002) + sin(time*0.00035))*BGLCanvas.Width*0.2,
    (sin(time*0.001) + sin(time*0.00025))*BGLCanvas.Height*0.2);
  if ACreateEarth then earthExpected:= true;
end;

procedure TStars.Render;
var
  i: Integer;
  scrPt: TPointF;
  r: single;
  c: TBGRAPixel;
begin
  for i := 0 to high(stars) do
  with stars[i] do
  begin
    scrPt := pt + centerPt;
    r := 0.5*size;
    c := BGRAWhite;
    if r < 0.5 then
    begin
      c.alpha := round(255*r);
      r := 0.5;
    end;
    r := r*3;
    if isEarth then
    begin
      BGLCanvas.StretchPutImage(scrPt.x-r, scrPt.y-r, 2*r, 2*r, earthTex, c);
      if r*2 < earthTex.Width then
      begin
        BGLCanvas.StretchPutImage(scrPt.x-r+0.33, scrPt.y-r+0.33, 2*r, 2*r, earthTex, BGRA(c.red,c.green,c.blue,c.alpha div 2));
        BGLCanvas.StretchPutImage(scrPt.x-r+0.66, scrPt.y-r+0.66, 2*r, 2*r, earthTex, BGRA(c.red,c.green,c.blue,c.alpha div 4));
      end;
    end
    else
      BGLCanvas.StretchPutImage(scrPt.x-r, scrPt.y-r, 2*r, 2*r, starBmp.Texture, c);
    if (scrPt.x < -r) or (scrPt.y < -r) or (scrPt.x > BGLCanvas.Width+r) or (scrPt.y > BGLCanvas.Height+r) then
    begin
      isEarth:= false;
      pt := PointF(random(BGLCanvas.Width)-round(centerPt.x)-0.5, random(BGLCanvas.Height)-round(centerPt.y)-0.5);
      size := 0.3;
      if earthExpected and not earthCreated then
      begin
        pt := PointF(-0.2,-0.2);
        isEarth := true;
        earthCreated:= true;
      end;
    end;
  end;
end;

destructor TStars.Destroy;
begin
  starBmp.Free;
  inherited Destroy;
end;

end.

