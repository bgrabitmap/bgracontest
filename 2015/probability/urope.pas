unit URope;

{$mode objfpc}

interface

uses
  Classes, SysUtils, BGRAOpenGL, BGRAPath, BGRABitmap, BGRABitmapTypes;

type
  { TRope }

  TRope = class
    ropeNodes: array of record
      Location, Speed, Acceleration: TPointF;
    end;
    circleTex: IBGLTexture;
    panelTex: IBGLTexture;
    fixed: boolean;
    constructor Create(ANbNodes: integer; StartPoint: TPointF; Spacing: TPointF);
    procedure Elapse;
    procedure Render;
  end;

implementation

uses UTexture;

{ TRope }

constructor TRope.Create(ANbNodes: integer; StartPoint: TPointF; Spacing: TPointF);
var
  i: Integer;
  circleBmp: TBGLBitmap;
  panelBmp: TBGLBitmap;
  wood: TBGRABitmap;
begin
  fixed := true;
  setlength(ropeNodes, ANbNodes);
  for i := 0 to high(ropeNodes) do
  with ropeNodes[i] do
  begin
    Location := StartPoint;
    if i <> 0 then
      Location := Location + PointF((Random(21)-10)*0.6,(Random(21)-10)*0.6) + Spacing*i;
    Speed := PointF(0,0);
    Acceleration := PointF(0,0);
  end;
  circleBmp := TBGLBitmap.Create(20,10);
  circleBmp.RoundRectAntialias(8,1,18,8, 4,4, BGRA(128,128,128), 3);
  circleBmp.RoundRectAntialias(8,1,18,8, 4,4, BGRA(180,180,180), 1);
  circleBmp.DrawLineAntialias(1,5,11,5, BGRA(128,128,128), 3);
  circleBmp.DrawLineAntialias(1,5,11,5, BGRA(180,180,180), 1);
  circleTex := circleBmp.MakeTextureAndFree;

  wood := CreateWoodTexture(128,128);
  wood.GradientFill(0,0,wood.width,wood.height, BGRA(0,0,0,60), BGRA(0,0,0,140), gtRadial, PointF(10,10), PointF(60,40), dmDrawWithTransparency);
  panelBmp := TBGLBitmap.Create(120,60);
  panelBmp.FillRoundRectAntialias(0,0,119,59,20,20,wood);
  panelBmp.FontVerticalAnchor := fvaCapCenter;
  panelBmp.TextOut(panelBmp.Width/2+1,panelBmp.Height/2+1,'Probability', BGRA(0,0,0,64), taCenter);
  panelBmp.TextOut(panelBmp.Width/2,panelBmp.Height/2,'Probability', BGRA(60,30,0), taCenter);
  panelTex := panelBmp.MakeTextureAndFree;
end;

procedure TRope.Elapse;
const MaxSegLen = 40;
var seg: TPointF;
  len: single;
  a: TPointF;
  i: Integer;
  freeFloating: array of boolean;
begin
  setlength(freeFloating, length(ropeNodes));
  for i := 0 to high(ropeNodes) do
  begin
    ropeNodes[i].Speed.Y *= 0.99;
    ropeNodes[i].Acceleration := PointF(0,0.03); //gravity
    freeFloating[i] := true;
  end;
  for i := 0 to high(ropeNodes)-1 do
  begin
    seg := ropeNodes[i+1].Location - ropeNodes[i].Location;
    len := VectLen(seg);
    if len > MaxSegLen then
    begin
      a := seg*(1/len)*(len-MaxSegLen)*0.1;
      if freeFloating[i] then
      begin
        //ropeNodes[i].Acceleration := PointF(0,0);
        freeFloating[i] := false;
      end;
      ropeNodes[i].Acceleration += a;
      if freeFloating[i+1] then
      begin
        //ropeNodes[i+1].Acceleration := PointF(0,0);
        freeFloating[i+1] := false;
      end;
      ropeNodes[i+1].Acceleration -= a;
    end;
  end;
  if fixed then
    ropeNodes[0].Acceleration := PointF(0,0); //first point fixed
  for i := 0 to high(ropeNodes) do
    with ropeNodes[i] do
    begin
      Speed += Acceleration;
      Location += Speed;
    end;
end;

procedure TRope.Render;
const nbCircle = 30;
var path: TBGLPath;
  cursor: TBGRAPathCursor;
  pts: array of TPointF;
  i: Integer;
  step: single;

  orig,tang,perp: TPointF;
  prevOrig: TPointF;
  len: single;
begin
  path := TBGLPath.Create;
  setlength(pts, length(ropeNodes));
  for i := 0 to high(pts) do
    pts[i] := ropeNodes[i].Location;

  path.openedSpline(pts,ssInsideWithEnds);
  cursor := TBGRAPathCursor.Create(path);
  step := cursor.PathLength/nbCircle;
  prevOrig := cursor.CurrentCoordinate;
  for i := 0 to nbCircle-1 do
  begin
    cursor.MoveForward(step,False);
    orig := cursor.CurrentCoordinate;
    if (i > 0) and ((orig.x <> prevOrig.x) or (orig.y <> prevOrig.y)) then
    begin
      tang := orig-prevOrig;
      len := VectLen(tang);
      if len <> 0 then
      begin
        tang := tang * (1/len);
        perp := PointF(tang.y,-tang.x);

        tang *= step*1.1;
        prevOrig -= perp*step*0.25;
        perp *= step*0.5;
        circleTex.DrawAffine(prevOrig,prevOrig+tang,prevOrig+perp);
      end;
    end;
    prevOrig := orig;
  end;

  orig := cursor.CurrentCoordinate;
  tang := cursor.CurrentTangent;
  perp := PointF(tang.y,-tang.x);
  tang *= panelTex.Height;
  perp *= panelTex.Width;
  orig -= perp*0.5;
  panelTex.DrawAffine(orig, orig+perp, orig+tang);

  cursor.Free;
  path.Free;
end;

end.

