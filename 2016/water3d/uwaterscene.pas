unit UWaterScene;
{ Water3D demo by circular }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmapTypes, BGRAOpenGL3D, BGRAScene3D, BGRAOpenGL;

const
  precisionFactor = 512; //use power of two to improve compiler optimisation
  gravity = precisionFactor * 2 div 64;
  friction = precisionFactor * 2 div 512;
  hexaside = 20;
  waterhighx = hexaside*2+2-1;
  waterhighy = hexaside*2+2-1;
  equitriheight = sqrt(0.75);
  pondZoom = 4;
  pondScaleZ = 1/(precisionFactor*100);
  groundLevel = 0.75;

type
  TValue = NativeInt;
  PWaterPixel = ^TWaterPixel;
  TWaterPixel = packed record
    h,         //height (with average equal to zero)
    v: TValue; //speed (multiplied by precisionFactor)
    p: IBGRAVertex3D;
  end;

  { TWaterScene3D }

  TWaterScene3D = class(TBGLScene3D)
  protected
    waterColor: TBGRAPixel;
    waterMaterial: IBGRAMaterial3D;
    waterSurface : IBGRAObject3D;
    drops: array of record
      obj: IBGRAObject3D;
      x,y: TValue;
      z,dz: single;
    end;
    pondUnit: single;
    marbleTex,
    groundTex, groundTexLow: TBGLBitmap;
    w : packed array of array of TWaterPixel;
    viewDist,viewDistSpeed: single;
    viewAngle: single;
    elapseAcc: single;
    procedure InitWaterArray;
    procedure CalcWaves;
    procedure WaterDropTouchWater(x, y, r, dropHeight: TValue);
    function PointInsideShape(x,y: TValue): boolean;
    function ShapeLineStart(y: TValue): TValue;
    function ShapeLineEnd(y: TValue): TValue;
    procedure AddWaterDrop;
    procedure AddPondBorder;
    procedure AddGround;
    procedure UpdateViewPoint;


  public
    constructor Create;
    procedure Elapse(timeMs: single);
    destructor Destroy; override;
  end;

implementation

{ TWaterScene3D }

procedure TWaterScene3D.InitWaterArray;
var
  x, y : TValue;
  pw : PWaterPixel;
  sy,coordy: single;
begin
  pondUnit := pondZoom/(waterhighx-1);
  sy := pondZoom/(waterhighy-1)*equitriheight;
  setlength(w,waterhighy+1);

  for y := 0 to waterhighy do
  begin
    setlength(w[y],waterhighx+1);
    coordy := (y-1)*sy - pondZoom*equitriheight*0.5;
    pw := @w[y,0];
    for x := 0 to waterhighx do
    begin
      pw^.h := 0;     //fill "pond" to uniform height
      pw^.v := 0;     //no waves
      if PointInsideShape(x,y) then
      begin
        if odd(y) then
          pw^.p := waterSurface.MainPart.Add((x-1+0.25)*pondUnit - pondzoom*0.5,coordy,0)
        else
          pw^.p := waterSurface.MainPart.Add((x-1-0.25)*pondUnit - pondzoom*0.5,coordy,0);
      end;
      inc(pw);
    end;
  end;

  for y := 0 to waterhighy-1 do
    for x := 0 to waterhighx-1 do
    begin
      if odd(y) then
      begin
        if assigned(w[y,x].p) and assigned(w[y+1,x+1].p) and assigned(w[y+1,x].p) then
          waterSurface.AddFace([w[y,x].p,w[y+1,x+1].p,w[y+1,x].p]);
        if assigned(w[y,x].p) and assigned(w[y,x+1].p) and assigned(w[y+1,x+1].p) then
          waterSurface.AddFace([w[y,x].p,w[y,x+1].p,w[y+1,x+1].p]);
      end else
      begin
        if assigned(w[y,x].p) and assigned(w[y,x+1].p) and assigned(w[y+1,x].p) then
          waterSurface.AddFace([w[y,x].p,w[y,x+1].p,w[y+1,x].p]);
        if assigned(w[y,x+1].p) and assigned(w[y+1,x+1].p) and assigned(w[y+1,x].p) then
          waterSurface.AddFace([w[y,x+1].p,w[y+1,x+1].p,w[y+1,x].p]);
      end;
    end;
end;

procedure TWaterScene3D.CalcWaves;
var
  x, y, start, stop, hgtSurr, hgtDiff, a : TValue;
  denom: TValue;
  pw,pwPrev,pwSuiv: PWaterPixel;
  currentAverage,n: NativeInt;
begin
  //compute speeds
  denom := 6*precisionFactor;
  currentAverage := 0;
  n := 0;
  for y := 1 to waterhighy - 1 do
  begin
    start := ShapeLineStart(y);
    stop := ShapeLineEnd(y);
    pw := @w[y,start];
    pwPrev := @w[y-1,start];
    pwSuiv := @w[y+1,start];
    if odd(y) then
      pwSuiv += 1
    else
      pwPrev -= 1;
    for x := start to stop do
    begin
      currentAverage += pw^.h;
      inc(n);

      hgtSurr := (precisionFactor*((pw-1)^.h + (pw+1)^.h
              + pwPrev^.h + (pwPrev+1)^.h
              + pwSuiv^.h + (pwSuiv-1)^.h)
              + denom div 2) //rounding of divison
              div denom; //average

      hgtDiff := hgtSurr - pw^.h;     //difference x,y and surrounding fields
      a := hgtDiff * gravity; //multiply by gravity (this is premultiplied by precisionFactor)
      pw^.v += a - pw^.v div (precisionFactor div friction);
      inc(pwPrev);
      inc(pwSuiv);
      inc(pw);
    end;
  end;
  currentAverage:= (currentAverage div n) div 8;
  //move water (needs to be separate to avoid interference and recursive effect on successive values)
  for y := 1 to waterhighy - 1 do
  begin
    start := ShapeLineStart(y);
    stop := ShapeLineEnd(y);
    pw := @w[y,start];
    for x := start to stop do
    begin
      pw^.h += pw^.v div precisionFactor - currentAverage;
      if assigned(pw^.p) then
        with pw^.p.SceneCoord do
          pw^.p.SceneCoord := Point3D(x,y,pw^.h*pondScaleZ);
      inc(pw);
    end;
  end;
end;

procedure TWaterScene3D.WaterDropTouchWater(x, y, r, dropHeight: TValue);
var xb,yb: TValue;
    d,f,e: single;
    v: TValue;
begin
  f := Pi/r;
  for xb := x-r to x+r do
    for yb := y-r to y+r do
      if PointInsideShape(xb,yb) then
      begin
        //distance to the center of the water drop
        e := ((yb and 1) - (y and 1))*0.5;
        d := sqrt(sqr(xb-x + e)+sqr((yb-y)*equitriheight))/r;
        if d < 1 then
        begin
          v := round(cos((1-d)*f)                //wave form
                   *dropHeight*precisionFactor   //height
                   *(1-d));                      //attenuation
          w[yb,xb].h += v;
        end;
      end;
end;

function TWaterScene3D.PointInsideShape(x, y: TValue): boolean;
begin
  result := (y>0) and (y<waterhighy) and (x >= ShapeLineStart(y)) and (x <= ShapeLineEnd(y));
end;

function TWaterScene3D.ShapeLineStart(y: TValue): TValue;
begin
  if (y = 0) or (y = waterhighy) then result := waterhighx div 2
  else if y <= hexaside then result := (hexaside-y) div 2 + 1
  else result := (y-hexaside) div 2 + 1;
end;

function TWaterScene3D.ShapeLineEnd(y: TValue): TValue;
begin
  if (y = 0) or (y = waterhighy) then result := waterhighx div 2
  else
  begin
    if y <= hexaside then result := waterhighx - 1 - (hexaside-y) div 2
    else result := waterhighx - 1 - (y-hexaside) div 2;
    if odd(y) then dec(result);
  end;
end;

procedure TWaterScene3D.AddWaterDrop;
var nx,ny: TValue;
    coord: TPoint3D;
begin
  repeat
    nx := random(waterhighx-1)+1;
    ny := random(waterhighx-1)+1;
  until PointInsideShape(nx,ny);
  setlength(drops, length(drops)+1);
  with drops[high(drops)] do
  begin
    x := nx;
    y := ny;
    z := -5;
    dz := 0;
    coord := w[y,x].p.SceneCoord;
    obj := CreateSphere(pondUnit, waterColor);
    obj.Material := waterMaterial;
    obj.LightingNormal := lnVertex;
    obj.MainPart.Translate(coord.x,coord.y,z,False);
  end;
end;

procedure TWaterScene3D.AddPondBorder;
type TVertexType = (lowIn, highIn, highOut, lowOut);
const sides = 6;
var
  pts: array[0..sides-1] of TPoint3D;
  v: array[0..sides-1, TVertexType] of IBGRAVertex3D;
  vCenter: IBGRAVertex3D;
  i,j: Integer;
  pondBorder: IBGRAObject3D;
begin
  marbleTex := TBGLBitmap.Create('marble.jpg');
  pondBorder := CreateObject(marbleTex);
  pts[0] := w[1,ShapeLineStart(1)].p.SceneCoord;
  pts[1] := w[1,ShapeLineEnd(1)].p.SceneCoord;
  pts[2] := w[hexaside,ShapeLineEnd(hexaside)].p.SceneCoord;
  pts[3] := w[waterhighy-1,ShapeLineEnd(waterhighy-1)].p.SceneCoord;
  pts[4] := w[waterhighy-1,ShapeLineStart(waterhighy-1)].p.SceneCoord;
  pts[5] := w[hexaside,ShapeLineStart(hexaside)].p.SceneCoord;
  for i := 0 to sides-1 do
  begin
    v[i,lowIn] := pondBorder.MainPart.Add(pts[i]+Point3D(0,0,groundLevel-0.05));
    v[i,highIn] := pondBorder.MainPart.Add(pts[i]+Point3D(0,0,-0.25));
    v[i,lowOut] := pondBorder.MainPart.Add(pts[i]*1.1+Point3D(0,0,groundLevel));
    v[i,highOut] := pondBorder.MainPart.Add(pts[i]*1.1+Point3D(0,0,-0.25));
  end;
  vCenter := pondBorder.MainPart.Add(0,0,groundLevel);

  for i := 0 to sides-1 do
  begin
    with pondBorder.AddFace([v[(i+1) mod sides,lowIn],v[i,lowIn],vCenter],true) do
    begin
      for j := 0 to VertexCount-1 do
        TexCoord[j] := PointF(Vertex[j].SceneCoord.x*marbleTex.Width/pondZoom/2, Vertex[j].SceneCoord.y*marbleTex.Height/pondZoom/2);
    end;
    with pondBorder.AddFace([v[i,lowIn],v[i,highIn],v[(i+1) mod sides,highIn],v[(i+1) mod sides,lowIn]]) do
    begin
      TexCoord[0] := PointF(i/sides*marbleTex.Width,marbleTex.Height*0.1);
      TexCoord[1] := PointF(i/sides*marbleTex.Width,0);
      TexCoord[2] := PointF((i+1)/sides*marbleTex.Width,0);
      TexCoord[3] := PointF((i+1)/sides*marbleTex.Width,marbleTex.Height*0.1);
    end;
    with pondBorder.AddFace([v[i,highIn],v[i,highOut],v[(i+1) mod sides,highOut],v[(i+1) mod sides,highIn]]) do
    begin
      TexCoord[0] := PointF(i/sides*marbleTex.Width,0);
      TexCoord[1] := PointF(i/sides*marbleTex.Width,-marbleTex.Height*0.05);
      TexCoord[2] := PointF((i+1)/sides*marbleTex.Width,0);
      TexCoord[3] := PointF((i+1)/sides*marbleTex.Width,-marbleTex.Height*0.05);
    end;
    with pondBorder.AddFace([v[i,highOut],v[i,lowOut],v[(i+1) mod sides,lowOut],v[(i+1) mod sides,highOut]]) do
    begin
      TexCoord[0] := PointF(i/sides*marbleTex.Width,-marbleTex.Height*0.05);
      TexCoord[1] := PointF(i/sides*marbleTex.Width,-marbleTex.Height*0.15);
      TexCoord[2] := PointF((i+1)/sides*marbleTex.Width,-marbleTex.Height*0.15);
      TexCoord[3] := PointF((i+1)/sides*marbleTex.Width,-marbleTex.Height*0.05);
    end;
  end;
end;

procedure TWaterScene3D.AddGround;
const sq = 5;
var v: array of IBGRAVertex3D;
  x,y: integer;
  tx1,tx2,ty1,ty2: integer;
  tex: TBGLBitmap;
begin
  groundTex := TBGLBitmap.Create('ground.jpg');
  groundTexLow := groundTex.Resample(groundTex.Width div 2,groundTex.Height div 2) as TBGLBitmap;
  with CreateObject do
  begin
    for x := -6 to 6 do
      for y := -6 to 6 do
      begin
        v := MainPart.Add([sq*(x*2-1),sq*(y*2-1),groundLevel, sq*(x*2+1),sq*(y*2-1),groundLevel, sq*(x*2+1),sq*(y*2+1),groundLevel, sq*(x*2-1),sq*(y*2+1),groundLevel]);
        with AddFace(v) do
        begin
          if (abs(x) > 1) or (abs(y) > 1) then
            tex := groundTexLow else tex := groundTex;
          Texture := tex;
          if odd(x) then
          begin
            tx1 := tex.Width-1;
            tx2 := 0;
          end else
          begin
            tx2 := tex.Width-1;
            tx1 := 0;
          end;
          if odd(y) then
          begin
            ty1 := tex.Height-1;
            ty2 := 0;
          end else
          begin
            ty2 := tex.Height-1;
            ty1 := 0;
          end;
          TexCoord[0] := PointF(tx1,ty1);
          TexCoord[1] := PointF(tx2,ty1);
          TexCoord[2] := PointF(tx2,ty2);
          TexCoord[3] := PointF(tx1,ty2);
        end;
      end;
  end;
end;

procedure TWaterScene3D.UpdateViewPoint;
begin
  Camera.ViewPoint := Point3D(viewDist*cos(viewAngle),viewDist*sin(viewAngle),-7);
end;

constructor TWaterScene3D.Create;
begin
  inherited Create;

  RenderingOptions.LightingInterpolation := liAlwaysHighQuality;
  RenderingOptions.PerspectiveMode := pmZBuffer;
  waterColor := CSSDodgerBlue;
  waterColor.alpha := 160;
  waterMaterial := CreateMaterial(100);
  waterSurface := CreateObject(waterColor);
  waterSurface.Material := waterMaterial;
  waterSurface.LightingNormal := lnVertex;

  randomize;
  InitWaterArray;

  AddPondBorder;
  AddGround;

  viewDist := 10;
  viewDistSpeed := 0;
  viewAngle := 0;
  UpdateViewPoint;
  Camera.LookAt(Point3D(0,0,0),Point3D(0,0,-1));
  AmbiantLightness := 0.5;

  AddDirectionalLight(Point3D(1,1,1),0.5);
end;

procedure TWaterScene3D.Elapse(timeMs: single);
var count,i,j: integer;
begin
  elapseAcc += timeMs;
  count := 0;
  while (elapseAcc > 15) and (count < 10) do
  begin
    if random(100)=0 then AddWaterDrop;
    for i := high(drops) downto 0 do
    with drops[i] do
    begin
      dz += 0.01;
      z += dz;
      obj.MainPart.Translate(0,0,dz,False);
      if z > w[y,x].h*pondScaleZ then
      begin
        WaterDropTouchWater(x,y,3,100);
        RemoveObject(obj);
        for j := i to high(drops)-1 do
          drops[j] := drops[j+1];
        setlength(drops,length(drops)-1);
      end;
    end;

    viewDist += viewDistSpeed;
    if viewDist > 8 then viewDistSpeed -= 0.0001;
    if viewDist < 5 then viewDistSpeed += 0.0001;
    viewAngle += 0.001;

    CalcWaves;
    elapseAcc -= 15;
    count += 1;
  end;
  if elapseAcc > 15 then elapseAcc := 15;
  UpdateViewPoint;
end;

destructor TWaterScene3D.Destroy;
begin
  drops := nil;
  w := nil;
  FreeAndNil(groundTex);
  FreeAndNil(groundTexLow);
  FreeAndNil(marbleTex);
  inherited Destroy;
end;

end.

