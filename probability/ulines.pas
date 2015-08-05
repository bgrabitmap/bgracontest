unit ULines;

{$mode objfpc}

interface

uses
  Classes, SysUtils, BGRABitmap, BGRAOpenGL, BGRABitmapTypes;

type

  { TLines }

  TLines = class
    points: array of record
      Location: TPointF;
      Speed: TPointF;
      color: TBGRAPixel;
    end;
    bmp: TBGLBitmap;
    temp: TBGRABitmap;
    vOfs: single;
    constructor Create(ANbLines: integer);
    procedure Elapse(AOrdering: boolean);
    procedure Render;
    destructor Destroy; override;
  end;

implementation

{ TLines }

constructor TLines.Create(ANbLines: integer);
var
  i: Integer;
begin
  bmp := TBGLBitmap.Create(140, round(140/BGLCanvas.Width*BGLCanvas.Height));
  temp := TBGRABitmap.Create(bmp.Width,bmp.Height);
  setlength(points, ANbLines*2);
  vOfs := (bmp.Height-100)/2;
  for i := 0 to high(points) do
  with points[i] do
  begin
    Location := PointF(Random(100),random(100)+vOfs);
    Speed := PointF((Random(11)-5)/5,(Random(11)-5)/5);
    color := BGRA(160+random(255-160),0,0);
    color.green := random(color.red);
    color.blue := random(color.green);
  end;
end;

procedure TLines.Elapse(AOrdering: boolean);
var
  i: Integer;
begin
  for i := 0 to high(points) do
  with points[i] do
  begin
    if Location.X > 99-2.5 then Speed.X -= 0.1;
    if Location.Y > vOfs+99-2.5 then Speed.Y -= 0.1;
    if Location.X < 2.5 then Speed.X += 0.1;
    if Location.Y < vOfs+2.5 then Speed.Y += 0.1;
    if AOrdering then
      begin
        if (Location.X > 30) and (Location.X < 70) then
          begin
            if Location.X < 50 then
              begin
                If Speed.X >= 0 then Speed.X -= 0.1;
              end else
                If Speed.X <= 0 then Speed.X += 0.1;
          end else
        if (Location.Y > 40+vOfs) and (Location.Y < 60+vOfs) then
          begin
            if Location.Y < 50+vOfs then
              begin
                if Speed.Y >= 0 then Speed.Y -= 0.1;
              end else
                if Speed.Y <= 0 then Speed.Y += 0.1;
          end;
      end;
    Location += Speed;
  end;
end;

procedure TLines.Render;
var
  i: Integer;
  v: TPointF;
  len: single;
begin
  bmp.Fill(BGRABlack);
  for i := 0 to length(points) div 2 -1 do
  begin
    v := points[i shl 1+1].Location-points[i shl 1].Location;
    len := VectLen(v);
    if len <> 0 then
      v:= v*(1/len);
    v := PointF(-v.y,v.x);
    case i and 1 of
    0: temp.GradientFill(0,0,bmp.Width,bmp.Height,points[i shl 1].color,BGRABlack,gtReflected,
      points[i shl 1].Location,points[i shl 1].Location+v*20,dmSet,False);
    1: temp.GradientFill(0,0,bmp.Width,bmp.Height,points[i shl 1].color,BGRABlack,gtDiamond,
      points[i shl 1].Location,points[i shl 1].Location+v*20,dmSet,False);
    end;
    case i and 3 of
    2: bmp.BlendImage(0,0,temp,boSubtract);
    else bmp.BlendImage(0,0,temp,boLighten);
    end;
  end;
  BGLCanvas.StretchPutImage(0,0, BGLCanvas.Width,BGLCanvas.Height, bmp.Texture);
end;

destructor TLines.Destroy;
begin
  inherited Destroy;
  bmp.Free;
  temp.Free;
end;

end.

