unit utarget;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRAGraphics, BGRABitmap, BGRABitmapTypes, LCLType, Math,
  BGRATextFX;

type

  { TTarget }

  TTarget = class
  private
    FBitmap: TBGRABitmap;
    FTargetLight: TBGRABitmap;
    FTargetGlow: TBGRABitmap;
    FBG: TBGRABitmap;
    FNextFrame: integer;
    FMaxFrameID: integer;
    FWidth: integer;
    FHeight: integer;
    FMousePos: TPoint;
    FFadingStep: integer;
  private
    procedure SetNextFrame(AValue: integer);
  private
    { Useful stuff}
    function BGRALoadFromResourceName(ResName: string): TBGRABitmap;
    function IsTimeInSeconds(FrameID, Seconds: integer): boolean;
    function IsTimeBetween(FrameID, SecondsLow, SecondsHigh: integer): boolean;
  public
    constructor Create(Width, Height: integer);
    destructor Destroy; override;
  public
    function SecondsToFrames(Seconds: integer): integer;
    { Compute NextFrame and increase NextFrame value }
    procedure ComputeNextFrame;
    { Compute frame specifing the ID }
    procedure ComputeFrame(FrameID: integer);
    { Draw computed frame }
    procedure Draw(ACanvas: TCanvas; ARect: TRect);
    { Save frame }
    procedure SaveFrame(Folder: String);
  public
    { Use NextFrame property to get or set the frame that will be computed }
    property NextFrame: integer read FNextFrame write SetNextFrame;
    property MaxFrameID: integer read FMaxFrameID;
    property MousePos: TPoint read FMousePos write FMousePos;
  end;

implementation

{ TTarget }

function TTarget.BGRALoadFromResourceName(ResName: string): TBGRABitmap;
var
  res: TResourceStream;
begin
  { Create a TResourceStream with the given ResName and use RT_RCDATA as type }
  res := TResourceStream.Create(HInstance, ResName, RT_RCDATA);
  { Create BGRA from Stream }
  Result := TBGRABitmap.Create(res);
  { Free temporary resource }
  res.Free;
end;

function TTarget.SecondsToFrames(Seconds: integer): integer;
begin
  { Using a fixed value of 33 ms (1000 ms / 30 frames by sec) }
  Result := Seconds * 33;
end;

function TTarget.IsTimeInSeconds(FrameID, Seconds: integer): boolean;
begin
  Result := (FrameID = SecondsToFrames(Seconds));
end;

function TTarget.IsTimeBetween(FrameID, SecondsLow, SecondsHigh: integer): boolean;
begin
  Result := ((FrameID >= SecondsToFrames(SecondsLow)) and
    (FrameID < SecondsToFrames(SecondsHigh)));
end;

procedure TTarget.SetNextFrame(AValue: integer);
begin
  if FNextFrame = AValue then
    Exit;
  FNextFrame := AValue;
  if FNextFrame > FMaxFrameID then
    FNextFrame := FMaxFrameID;
end;

constructor TTarget.Create(Width, Height: integer);
begin
  { BGRA }
  FBitmap := TBGRABitmap.Create(Width, Height, BGRABlack);
  { Copy these values }
  FWidth := Width;
  FHeight := Height;
  { Load Picture from resource }
  FTargetLight := BGRALoadFromResourceName('target_light');
  FTargetGlow := BGRALoadFromResourceName('target_glow');
  FBG := BGRALoadFromResourceName('bg');
  BGRAReplace(FBG, TBGRABitmap(FBG.Resample(Width, Height)));
  { Frame counter }
  FNextFrame := 0;
  { Frame data length, the same as the duration of the movie }
  FMaxFrameID := SecondsToFrames(50);
  { Decrease by one because is zero based }
  Dec(FMaxFrameID);
  { General purpouse int }
  FFadingStep := 0;
end;

destructor TTarget.Destroy;
begin
  { The end is coming, I feel it... }
  FBitmap.Free;
  FTargetLight.Free;
  FTargetGlow.Free;
  FBG.Free;
  inherited Destroy;
end;

procedure TTarget.ComputeNextFrame;
begin
  { Compute Frame based on FNextFrame value }
  ComputeFrame(FNextFrame);
  { Prepare FNextFrame for future }
  if FNextFrame < FMaxFrameID then
    Inc(FNextFrame);
end;

procedure TTarget.ComputeFrame(FrameID: integer);
var
  tmp: TBGRABitmap;
begin
  if FrameID > FMaxFrameID then
    FrameID := FMaxFrameID;

  { * The fun is here * }

  { background }
  FBitmap.PutImage(0, 0, FBG, dmDrawWithTransparency, 20);

  if IsTimeBetween(FrameID, 40, 44) then
  begin
    FBitmap.BlendImageOver(RandomRange(0, FWidth), RandomRange(0, FHeight),
      FTargetGlow, boXor);
  end;

  { blur }
  BGRAReplace(FBitmap, FBitmap.FilterBlurRadial(3, rbBox));

  if IsTimeInSeconds(FrameID, 44) then
    FFadingStep := 0;

  if not IsTimeBetween(FrameID, 44, 50) then
  begin
    FBitmap.BlendImageOver(FMousePos.x - (FTargetGlow.Width shr 1),
      FMousePos.y - (FTargetGlow.Height shr 1), FTargetGlow, boNiceGlow);

    FBitmap.BlendImageOver(FMousePos.x - (FTargetLight.Width shr 1),
      FMousePos.y - (FTargetLight.Height shr 1), FTargetLight, boLinearAdd);
  end
  else
  begin
    FBitmap.BlendImageOver(FMousePos.x - (FTargetGlow.Width shr 1),
      FMousePos.y - (FTargetGlow.Height shr 1), FTargetGlow, boLinearAdd);

    FBitmap.BlendImageOver(FMousePos.x - (FTargetLight.Width shr 1),
      FMousePos.y - (FTargetLight.Height shr 1), FTargetLight, boLinearAdd);

    BGRAReplace(FBitmap, FBitmap.FilterPixelate(FFadingStep, True));

    Inc(FFadingStep, 1);
  end;

  if IsTimeInSeconds(FrameID, 7) then
    FBitmap.Negative;

  if IsTimeInSeconds(FrameID, 10) then
    FBitmap.InplaceGrayscale;

  if IsTimeInSeconds(FrameID, 13) then
    BGRAReplace(FBitmap, FBitmap.FilterPixelate(100, True));

  if IsTimeInSeconds(FrameID, 16) then
    BGRAReplace(FBitmap, FBitmap.FilterSphere);

  if IsTimeInSeconds(FrameID, 19) then
    BGRAReplace(FBitmap, FBitmap.FilterContour);

  if IsTimeInSeconds(FrameID, 22) then
    BGRAReplace(FBitmap, FBitmap.FilterCylinder);

  if IsTimeInSeconds(FrameID, 25) then
    FBitmap.Rectangle(50, 50, FWidth - 50, FHeight - 50, BGRAWhite, BGRAPixelTransparent,
      dmDrawWithTransparency);

  if IsTimeBetween(FrameID, 26, 30) then
  begin
    if Odd(FrameID) then
      exit;
    FBitmap.Rectangle(10 + FFadingStep, 10 + FFadingStep, FWidth -
      FFadingStep, FHeight - FFadingStep, BGRAWhite, BGRAPixelTransparent,
      dmDrawWithTransparency);
    Inc(FFadingStep, 20);
  end;

  if IsTimeInSeconds(FrameID, 30) then
    FFadingStep := 10;

  if IsTimeBetween(FrameID, 30, 33) then
  begin
    FBitmap.EllipseAntialias(FWidth shr 1, FHeight shr 1, FFadingStep,
      FFadingStep, BGRA(100, 30, RandomRange(100, 255), 255), RandomRange(1, 4));
    Inc(FFadingStep, 10);
  end;

  if IsTimeBetween(FrameID, 33, 40) then
  begin
    tmp := TBGRABitmap(TextShadow(100, 100, '007', RandomRange(5, 8),
      BGRAWhite, BGRABlack, 10, 10, 2));
    FBitmap.PutImage(RandomRange(0, FWidth), RandomRange(0, FHeight),
      tmp, dmDrawWithTransparency, RandomRange(100, 256));
    tmp.Free;
  end;

  if FrameID = MaxFrameID - 1 then
  begin
    FBitmap.InplaceGrayscale;
    tmp := TBGRABitmap(TextShadow(FWidth, FHeight, 'by 007', FHeight shr 1,
      BGRAWhite, BGRABlack, 10, 10, 2));
    FBitmap.PutImage(0, 0,
      tmp, dmDrawWithTransparency, 220);
    tmp.Free;
  end;
end;

procedure TTarget.Draw(ACanvas: TCanvas; ARect: TRect);
begin
  { Stretch draw frame in the given canvas with specified rect }
  FBitmap.Draw(ACanvas, ARect);
end;

procedure TTarget.SaveFrame(Folder: String);
begin
  FBitmap.SaveToFile(Folder + IntToStr(FNextFrame) + '.png');
end;

end.
