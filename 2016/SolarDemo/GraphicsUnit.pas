unit GraphicsUnit;

{$mode objfpc}{$H+}

interface

uses SysUtils, Dialogs, dglOpenGL, nxGL, math, nxMath, nxMath3D,
  nxTypes, GameUnit;

type

  { TGraphicalGame }

  TGraphicalGame = class(TGame)
    sphere: TGLModel;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Draw; override;
    procedure DrawOrbiter(const o: TOrbiter);
  end;

implementation

{ TGame }

constructor TGraphicalGame.Create;
begin
  inherited Create;
  nx.rs.DepthTest:=false;
  glDepthMask(false);
  nxNEARZ:=0.00001;
  //nxFARZ:=999999999999;
  nx.DefaultLights;
  //nx.SetLight(1, GL_POSITION, 0, 0, 0);

  nx.SetClearColor(0, 0, 0);
  nx.Clear(true, true);
  nx.Flip;

  nx.CreateFont('Courier', 8, 256);
  sphere:=TGLModel.CreateSphere(20, 20);
  cam:=TCamera.Create;
  zoom:=5; toZoom:=zoom;

  // Move camera to earth
  cam.Translate(invert(root.child[2].position), false);

  // Last operations
  if nx.LastError<>'' then ShowMessage(nx.LastError);
  ResetTick; Initialized:=true;
end;

destructor TGraphicalGame.Destroy;
begin
  sphere.Free;
  root.Free;
  cam.Free;
  inherited Destroy;
end;

procedure TGraphicalGame.Draw;
var i, j: integer; a, d, c: single;
    r1, r2: TMouseRay; f1, f2: TVector;
begin
  nx.Clear(true, true);
  cam.Reset(false);
  cam.Translate(0, 0, -zoom, false);
  cam.Rotate(70, 1, 0, 0, false);
  cam.Translate(invert(campos), true);

  nx.GetMouseRay(nx.Width/2, nx.Height/2, @r1);
  nx.GetMouseRay(0, nx.Height/2, @r2);
  nxMath3D.RayPlaneIntersect(r1.start, r1.dir, vector(0, 0, 0), vector(0, 1, 0), @f1);
  nxMath3D.RayPlaneIntersect(r2.start, r2.dir, vector(0, 0, 0), vector(0, 1, 0), @f2);
  gscale:=abs(f1.x-f2.x)/15;

  tex.Disable;
  glPointSize(2);
  glBegin(GL_POINTS);
  for i:=0 to high(root.child) do
    with root.child[i] do begin
      with color do nx.SetColor(r, g, b, 0.7);
      d:=hypot(position.x, position.z);
      a:=trunc(angle(0, 0, position.x, position.z)*toDeg/3)*3*toRad;
      for j:=0 to 29 do begin
        a:=a+3*toRad;
        with color do nx.SetColor(r, g, b, 0.7*(1-j/30));
        glVertex3f(cos(a)*d, 0, sin(a)*d);
      end;
    end;
  glEnd;

  cam.Push;
  cam.Translate(campos.x, 0, campos.z, false);
  cam.Scale(gscale);
  glBegin(GL_LINES);
  nx.SetColor(1, 1, 1, 0.2);
  for j:=-15 to 15 do
    for i:=-15 to 14 do begin
      c:=g[i, j]*0.5; nx.SetColor(0.5+c, 0.5, 1-c, 0.2);
      glVertex3f(i, -c, j);
      c:=g[i+1, j]*0.5; nx.SetColor(0.5+c, 0.5, 1-c, 0.2);
      glVertex3f(i+1, -c, j);

      c:=g[j, i]*0.5; nx.SetColor(0.5+c, 0.5, 1-c, 0.2);
      glVertex3f(j, -c, i);
      c:=g[j, i+1]*0.5; nx.SetColor(0.5+c, 0.5, 1-c, 0.2);
      glVertex3f(j, -c, i+1);
    end;
  glEnd;
  cam.Pop;

  nx.SetLight(0, GL_POSITION, 0, 0, 0);

  nx.rs.Lighting:=false;
  DrawOrbiter(root);
  nx.rs.Lighting:=false;
  cam.Pop(false);

  // Draw text
  nx.Enable2D;
  nx.SetFont(0); nx.SetColor(1, 1, 1, 0.6);
  nx.Font[0].DrawC(nx.Width/2, 15, '- Press ESC to quit -');
  nx.Font[0].DrawC(nx.Width/2, 30, 'Press Space to toggle moving');
  nx.Font[0].Draw(1, 1, format('FPS: %d', [nx.FPS]));
  nx.Font[0].Draw(1, 10, format('Zoom: %.4f', [zoom]));
  nx.Font[0].Draw(1, 20, format('R1: %.2f, %.2f, %.2f', [f1.x, f1.y, f1.z]));
  nx.Font[0].Draw(1, 30, format('R2: %.2f, %.2f, %.2f', [f2.x, f2.y, f2.z]));
  nx.Font[0].Draw(1, 40, format('G-middle: %.3f', [GAt(f1)]));
  nx.Disable2D;

  nx.Flip;
end;

procedure TGraphicalGame.DrawOrbiter(const o: TOrbiter);
var i: integer;
begin
  cam.Push;
  cam.Translate(o.position);
  cam.Scale(o.radius);
  tex.Disable;
  with o.color do nx.SetColor(r, g, b);
  sphere.Render;
  tex.Enable;
  nx.rs.Lighting:=true;
  cam.Pop(false);
  for i:=0 to high(o.child) do DrawOrbiter(o.child[i]);
end;

end.

