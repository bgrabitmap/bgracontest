unit SmileyTunnelMain;

{$mode objfpc}{$H+}

interface

uses BGRABitmapTypes, BGRASceneTypes, BGRAOpenGL3D,
  BGRAOpenGL, BGLVirtualScreen, BGRABitmap, Forms, ExtCtrls,
  LCLType, uDefinitions, fgl, math, Classes, sysutils;

type

  TQuadPoint3D = array[0..3] of TPoint3D;
  TPictCircleCoord = array of TQuadPoint3D;

  { TCircleZ }

  TCircleZ = class
  private
    FCircle: IBGRAObject3D;
    Fz: Double;
  public
    property Circle: IBGRAObject3D read FCircle write FCircle;
    property z: Double read Fz write Fz;
  end;

  TCircleZList = specialize TFPGList<TCircleZ>;

  { TPictureTunnel }

  TPictureTunnel = class(TBGLScene3D)
  private
    FAdaptStep: Integer;                                    // step of FAdaptStepCount
    FAdaptStepCount: LongWord;                              // if circle changed (PictPerCircle or PictSpace), how many steps, it will take to finish the setting
    FBGLBitmap: TBGLBitmap;                                 // picture for the tunnel
    FCircles: TCircleZList;                                 // list with all cicles
    FCircleStart: Integer;                                  // offset, where normal BGRAScene.Object3Ds (like light) ends and TPictureTunnel-Object3Ds starts
    FOldPictPerCircle: LongWord;                            // old count of pictures per circle
    FOldPictSpace: LongWord;                                // old count of picture placeholders
    FPictPerCircle: LongWord;                               // how many pictures per circle
    FPictSpace: LongWord;                                   // a placeholder (as there were a picture)
    FStep: LongWord;                                        // the life-cycle of a TCircle
    procedure AddCirclePicture(nr: Integer;                 // add one picture of a circle
      aCircleZ: TCircleZ);
    procedure CalculatePictCircleCoord;                     // calculate the picture coordinates of one circle
    procedure SetPictPerCircle(AValue: LongWord);           // set a new count of pictures per circle
    procedure SetPictSpace(AValue: LongWord);               // set a new count of picture placeholders
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddCircle(z: Double);                         // add a new picture circle
    procedure DelCircle(Nr: Integer);                       // del a picture circle
    procedure Move;                                         // move the picture circle
    procedure RandomCircle;                                 // define new pictures per circle and picture placeholders
    property CircleStart: Integer read FCircleStart         // offset, where normal BGRAScene.Object3Ds (like light) ends and TPictureTunnel-Object3Ds starts
      write FCircleStart;
    property PictPerCircle: LongWord read FPictPerCircle    // count of pictures per circle
      write SetPictPerCircle;
    property PictSpace: LongWord read FPictSpace            // count of picture placeholders
      write SetPictSpace;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    BGLVirtualScreen1: TBGLVirtualScreen;
    Timer1: TTimer;
    Timer2: TTimer;
    procedure BGLVirtualScreen1LoadTextures(Sender: TObject; BGLContext: TBGLContext);
    procedure BGLVirtualScreen1Redraw(Sender: TObject; BGLContext: TBGLContext);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Timer1Timer(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
  private
    PictureTunnel: TPictureTunnel;
  end;

var
  Form1: TForm1;
  PictCircleCoord: TPictCircleCoord;

implementation

{$R *.lfm}

{ TPictureTunnel }

procedure TPictureTunnel.AddCirclePicture(nr: Integer;
  aCircleZ: TCircleZ);
var
  aRectangle: array of IBGRAVertex3D;
  aFace: IBGRAFace3D;
  aObject: IBGRAObject3D;
begin
  aObject := aCircleZ.Circle;
  aRectangle := aObject.MainPart.Add([
    PictCircleCoord[nr, 0].x, PictCircleCoord[nr, 0].y, PictCircleCoord[nr, 0].z,
    PictCircleCoord[nr, 1].x, PictCircleCoord[nr, 1].y, PictCircleCoord[nr, 1].z,
    PictCircleCoord[nr, 2].x, PictCircleCoord[nr, 2].y, PictCircleCoord[nr, 2].z,
    PictCircleCoord[nr, 3].x, PictCircleCoord[nr, 3].y, PictCircleCoord[nr, 3].z]);
  aFace := aObject.AddFace(aRectangle);
  aFace.TexCoord[0] := PointF(0, 0);
  aFace.TexCoord[1] := PointF(FBGLBitmap.Width - 1, 0);
  aFace.TexCoord[2] := PointF(FBGLBitmap.Width - 1, FBGLBitmap.Height - 1);
  aFace.TexCoord[3] := PointF(0, FBGLBitmap.Height - 1);
end;

procedure TPictureTunnel.CalculatePictCircleCoord;
const
  PictRadius = 2;                                           // radius of the picturecircle
var
  PictPerimeter: Double;                                    // perimeter of PictureCircle
  PictSize: Double;                                         // it's used as Width / Height of a picture
  BasisPoints: TQuadPoint3D;                                // the original points of one picture
  Angle: Double;
  i, i2: Integer;
  aSin, aCos: Double;
  PPC: Double;
  PictCount: Integer;
begin
  if FAdaptStep < 0 then Exit;
  PPC := ((FOldPictPerCircle + FOldPictSpace) * FAdaptStep +
          (PictPerCircle     + PictSpace)     * (FAdaptStepCount - FAdaptStep)) / FAdaptStepCount;

  PictPerimeter := Pi * PictRadius * 2;
  PictSize := PictPerimeter / PPC;
  BasisPoints[0].x :=            - PictSize / 2;
  BasisPoints[0].y := PictRadius - PictSize / 2;
  BasisPoints[0].z :=              0;
  BasisPoints[1].x :=              PictSize / 2;
  BasisPoints[1].y := PictRadius - PictSize / 2;
  BasisPoints[1].z :=              0;
  BasisPoints[2].x :=              PictSize / 2;
  BasisPoints[2].y := PictRadius + PictSize / 2;
  BasisPoints[2].z :=              0;
  BasisPoints[3].x :=            - PictSize / 2;
  BasisPoints[3].y := PictRadius + PictSize / 2;
  BasisPoints[3].z :=              0;

  PictCount := min(FOldPictPerCircle, PictPerCircle);
  SetLength(PictCircleCoord, PictCount);
  for i := 0 to High(PictCircleCoord) do
  begin
    Angle := i * Pi * 2 / PictCount;
    aSin := Sin(Angle);
    aCos := Cos(Angle);
    for i2 := 0 to 3 do
    begin
      PictCircleCoord[i, i2].x := aCos * BasisPoints[i2].x - aSin * BasisPoints[i2].y;
      PictCircleCoord[i, i2].y := aCos * BasisPoints[i2].y + aSin * BasisPoints[i2].x;
      PictCircleCoord[i, i2].z :=        BasisPoints[i2].z;
    end;
  end;

  Dec(FAdaptStep);
end;

procedure TPictureTunnel.SetPictPerCircle(AValue: LongWord);
begin
  if AValue < 1 then AValue := 1;
  if AValue > 500 then AValue := 500;
  if FPictPerCircle = AValue then Exit;
  if (AValue + PictSpace < 5) then PictSpace := 5 - AValue;
  FOldPictPerCircle := FPictPerCircle;
  FPictPerCircle := AValue;
end;

procedure TPictureTunnel.SetPictSpace(AValue: LongWord);
begin
  if AValue > 100 then AValue := 100;
  if PictPerCircle < 5 then AValue := Max(AValue, 5 - PictPerCircle);
  if FPictSpace = AValue then Exit;
  FOldPictSpace := FPictSpace;
  FPictSpace := AValue;
end;

constructor TPictureTunnel.Create;
var
  Dummy: TBGRABitmap;
begin
  inherited Create;

  FCircles := TCircleZList.Create;
  FStep := 0;
  FOldPictPerCircle := 7;
  FOldPictSpace := 0;
  FPictPerCircle := 7;
  FPictSpace := 0;
  FAdaptStep := 0;
  FAdaptStepCount := 1;

  Dummy := TBGRABitmap.Create;
  try
    Dummy.LoadFromFile(GetCurrentDir + PathDelim + 'smiley.png');
    FBGLBitmap := TBGLBitmap.Create(Normal2(Dummy.Width), Normal2(Dummy.Height));
    FBGLBitmap.CanvasBGRA.StretchDraw(Rect(0, 0, FBGLBitmap.Width, FBGLBitmap.Height), Dummy);
  finally
    Dummy.Free;
  end;
end;

destructor TPictureTunnel.Destroy;
var
  i: Integer;
begin
  if Assigned(FBGLBitmap) then FBGLBitmap.Free;
  for i := FCircles.Count - 1 downto 0 do FCircles.Items[i].Free;
  FCircles.Free;
  inherited Destroy;
end;

procedure TPictureTunnel.AddCircle(z: Double);
var
  i: Integer;
  aCircleZ: TCircleZ;
begin
  CalculatePictCircleCoord;

  aCircleZ := TCircleZ.Create;
  aCircleZ.z := z;
  aCircleZ.Circle := CreateObject(FBGLBitmap);
  for i := 0 to High(PictCircleCoord) do
    AddCirclePicture(i, aCircleZ);
  FCircles.Add(aCircleZ);
end;

procedure TPictureTunnel.DelCircle(Nr: Integer);
begin
  FCircles.Items[Nr].Circle.Clear;
  RemoveObject(FCircles.Items[Nr].Circle);
  FCircles.Items[Nr].Free;
  FCircles.Delete(Nr);
end;

procedure TPictureTunnel.Move;
var
  i, CircleOfs: Integer;
  z: Double;
begin
  for i := Object3DCount - 1 downto CircleStart do begin    // there is one object which is a light, not a circle
    CircleOfs := i - CircleStart;
    z := FCircles.Items[CircleOfs].z - 0.1;
    if z < -2 then begin
      DelCircle(CircleOfs);
      Continue;
    end;
    FCircles.Items[CircleOfs].z := z;
    Object3D[i].MainPart.ResetTransform;
    Object3D[i].MainPart.Translate(
      SinusArray[(FStep * 3 + Round(z * 20)) mod MaxLife] * z / 8,
      SinusArray[(FStep * 2 + Round(z * 15)) mod MaxLife] * z / 8,
      z);
    Object3D[i].MainPart.RotateZRad(SinusArray[(FStep * 2 + Round(z * 30)) mod MaxLife] * 5);
  end;
  Inc(FStep);
end;

procedure TPictureTunnel.RandomCircle;
begin
  PictPerCircle := Random(1 shl Random(8));
  PictSpace := Random(10);
  FAdaptStepCount := 50;
  FAdaptStep := 49;
end;

{ TForm1 }

procedure TForm1.BGLVirtualScreen1LoadTextures(Sender: TObject;
  BGLContext: TBGLContext);
var
  aLight: IBGRAVertex3D;
  aObject: IBGRAObject3D;
begin
  if not Assigned(PictureTunnel) then
  begin
    PictureTunnel := TPictureTunnel.Create;
    aObject := PictureTunnel.CreateObject;
    aLight := aObject.MainPart.Add(0, 0, 0);
    PictureTunnel.AddPointLight(aLight, 15, 1);
    PictureTunnel.CircleStart := 1;
    PictureTunnel.Camera.ViewPoint := Point3D(0, 0, -1);
    PictureTunnel.AddDirectionalLight(Point3D(0, 0, 1), 0.7, 0);
    PictureTunnel.RenderingOptions.AntialiasingMode := am3dResample;
    PictureTunnel.RenderingOptions.AntialiasingResampleLevel := 2;
    PictureTunnel.RenderingOptions.MinZ := 2;
    PictureTunnel.RenderingOptions.LightingInterpolation := liLowQuality;
    PictureTunnel.AmbiantLightness := -1;
  end;
end;

procedure TForm1.BGLVirtualScreen1Redraw(Sender: TObject; BGLContext: TBGLContext);
begin
  BGLContext.Canvas.Fill(BGRA(0, 0, 0, 255));
  PictureTunnel.RenderGL(BGLContext.Canvas, 100);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Timer1.Enabled := False;
  Timer2.Enabled := False;
  PictureTunnel.Free;
end;

procedure TForm1.FormKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE:
      Close;
    VK_SPACE:
      Timer1.Enabled := not Timer1.Enabled;
  end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  if not Assigned(PictureTunnel) then Exit;
  PictureTunnel.Move;
  BGLVirtualScreen1.Invalidate;
end;

procedure TForm1.Timer2Timer(Sender: TObject);
const
  Cnt: DWord = 1;
begin
  if not Assigned(PictureTunnel) then Exit;
  if PictureTunnel.Object3DCount < 60 then
  begin
    if Cnt > 100 then
    begin
      PictureTunnel.RandomCircle;
      Cnt := 0;
    end;
    PictureTunnel.AddCircle(30);
  end;
  Inc(Cnt);
end;

end.
