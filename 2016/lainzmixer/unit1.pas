unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  LCLType,
  BGRABitmap, BGRABitmapTypes, BGRAScene3D,
  BGLVirtualScreen, BGRAOpenGL, BGRAOpenGL3D;

type

  { T3DGraphic }

  T3DGraphic = class(TBGLScene3D)
  public
    constructor Create;
    procedure RenderGL(ACanvas: TBGLCustomCanvas; AMaxZ: single = 1000); override;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    BGLVirtualScreen1: TBGLVirtualScreen;
    Timer1: TTimer;
    procedure BGLVirtualScreen1LoadTextures(Sender: TObject; BGLContext: TBGLContext);
    procedure BGLVirtualScreen1Redraw(Sender: TObject; BGLContext: TBGLContext);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    scene: T3DGraphic;
    c: byte;
    r: boolean;
    d: single;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ T3DGraphic }

constructor T3DGraphic.Create;
begin
  inherited Create;
  FetchDirectory := '.';
  with FetchObject('dragonwindspropicos.obj', True) do
  begin
    // Perfectly aligned with Blender 'Front view'
    MainPart.RotateZDeg(180, False);
    MainPart.RotateYDeg(180, False);
  end;
end;

procedure T3DGraphic.RenderGL(ACanvas: TBGLCustomCanvas; AMaxZ: single);
begin
  inherited RenderGL(ACanvas, AMaxZ);
end;

{ TForm1 }

procedure TForm1.BGLVirtualScreen1LoadTextures(Sender: TObject;
  BGLContext: TBGLContext);
begin
  if not Assigned(scene) then
  begin
    scene := T3DGraphic.Create;
    scene.Camera.ViewPoint := Point3D(0, 0, -10);
    scene.AddDirectionalLight(Point3D(0, 0, 1), 0.25, 0);
  end;
end;

procedure TForm1.BGLVirtualScreen1Redraw(Sender: TObject; BGLContext: TBGLContext);
begin
  scene.RenderingOptions.AntialiasingMode := am3dResample;
  scene.RenderingOptions.AntialiasingResampleLevel := 2;
  scene.RenderingOptions.MinZ := 1;
  scene.RenderingOptions.LightingInterpolation := liAlwaysHighQuality;
  scene.AmbiantLightness := 0.75;
  BGLContext.Canvas.Fill(BGRA(c, c, c, 255));
  scene.RenderGL(BGLContext.Canvas);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  d := 1.5;
  c := 255;
  r := false;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Timer1.Enabled := False;
  scene.Free;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if key = VK_ESCAPE then
  begin
    Close;
  end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  if Assigned(scene) then
  begin
    if scene.Object3DCount > 0 then
    begin
      scene.Object3D[0].MainPart.RotateYDeg(d, False);
      if r then
        scene.Object3D[0].MainPart.RotateXDeg(d, False)
      else
        scene.Object3D[0].MainPart.RotateZDeg(d, False);
    end;

    if r then
    begin
      Inc(c);
      d := d + 0.1;
    end
    else
    begin
      Dec(c);
      d := d - 0.1;
    end;
    if c = 255 then
    begin
      r := False;
    end
    else if c = 0 then
    begin
      r := True;
    end;

    BGLVirtualScreen1.Invalidate;
  end;
end;

end.
