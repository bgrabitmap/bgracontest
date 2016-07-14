unit GraphicsUnit;

{$mode objfpc}{$H+}

interface

uses SysUtils, Dialogs, dglOpenGL, nxGL, math, nxMath, nxMath3D,
  nxTypes, GameUnit;

type

  { TGraphicalGame }

  TGraphicalGame = class(TGame)
  public
    model: TGLModel;
    constructor Create;
    destructor Destroy; override;
    procedure Draw; override;
  end;

implementation

{ TGame }

constructor TGraphicalGame.Create;
var i, texIndex: integer;
begin
  inherited Create;
  nx.SetClearColor(0, 0, 0);
  nx.Clear(true, true);
  nx.Flip;

  nx.DefaultLights();
  nx.rs.Lighting:=false;
  nx.rs.DepthTest:=true;

  nx.CreateFont('Courier', 8, 256);
  texIndex:=tex.AddTexture('glow', GetPath('textures\glow.png'));
  for i:=0 to high(movers) do
    movers[i].trail^.texture:=texIndex;

  model:=TGLModel.CreateSphere(20, 20);
  model.NewMaterial(-1);
  model.mat[0].shininess:=20;
  model.SetMaterial(0);

  // Last operations
  if nx.LastError<>'' then ShowMessage(nx.LastError);
  ResetTick; Initialized:=true;
end;

destructor TGraphicalGame.Destroy;
begin
  model.Free;
  inherited Destroy;
end;

procedure TGraphicalGame.Draw;
var i: integer;
begin
  // Draw game tick
  nx.Clear(true, true);

  // Set camera
  glLoadIdentity;
  glTranslatef(0, 0, -6.5);
  glRotatef(rotation.y, 1, 0, 0);
  glRotatef(rotation.x, 0, 1, 0);

  tex.Disable;

  // Background sphere
  nx.rs.Push;
  nx.rs.Lighting:=true;
  nx.SetColor(0, 1, 0, 0.2);
  nx.rs.WireFrame:=true;
  glPushMatrix;
  glScalef(5, 5, 5);
  nx.rs.CullFront:=true;
  nx.rs.CullBack:=false;
  model.Render();
  glPopMatrix;
  nx.rs.Pop;

  // Spheres
  nx.rs.Push;
  nx.rs.Lighting:=true;
  glDepthMask(true);
  for i:=0 to high(movers) do
    with movers[i] do begin
      glPushMatrix;
      glTranslatef(position.x, position.y, position.z);
      nx.SetColor(color);
      glScalef(radius, radius, radius);
      model.Render();
      glPopMatrix;
    end;
  glDepthMask(false);
  tex.Enable;
  nx.rs.Lighting:=false;
  nx.rs.AddBlend:=true;

  // Trails
  tex.SetByName('glow');
  pt.Draw3D;
  nx.rs.Pop;

  // Texts
  nx.Enable2D;
  nx.SetFont(0);
  nx.SetColor(1, 1, 1, 0.5);
  nx.Font[0].Draw(1, 1, format('FPS: %d', [nx.FPS]));
  nx.Font[0].DrawC(nx.Width/2, 15, '- Press ESC to quit -');
  nx.Font[0].DrawC(nx.Width/2, 30, '- Use mouse to rotate -');

  nx.Disable2D;
  nx.Flip;
end;

end.

