unit ukoala;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRAGraphics, BGRABitmap, BGRABitmapTypes, LCLType, Math,
  BGRATextFX;

type

  { Stores the data that is used by each frame }
  TFrameData = record
    TopImageRect: TRect;
    BottomImageRect: TRect;
    BandRect: TRect;
    BlurAlpha: byte;
    VignetteColor: TBGRAPixel;
    NoiseFading: byte;
    CreditsAlpha: byte;
  end;

  { Stores all the data used by the animation }
  TFrameDataArray = array of TFrameData;

  { TKoala }

  TKoala = class
  private
    FBitmap: TBGRABitmap;
    FKoala: TBGRABitmap;
    FNextFrame: integer;
    FMaxFrameID: integer;
    FFrameData: TFrameDataArray;
    FWidth: integer;
    FHeight: integer;
    FVignettePoint1: TPointF;
    FVignettePoint2: TPointF;
  private
    procedure SetNextFrame(AValue: integer);
  private
    { Useful stuff}
    function BGRALoadFromResourceName(ResName: string): TBGRABitmap;
    procedure TVScanLines(Bitmap: TBGRABitmap);
    procedure Blur(Bitmap: TBGRABitmap; Alpha: byte);
    function RandomBW: byte;
    procedure NoiseBWA(Bitmap: TBGRABitmap);
    procedure PhotoNoise(Bitmap: TBGRABitmap; Ammount: byte);
    function RandText: string;
    procedure DrawCredits(Bitmap: TBGRABitmap; ID: integer);
    function SecondsToFrames(Seconds: integer): integer;
    { Computing all at start will speed the future }
    procedure ComputeFrameData;
  public
    constructor Create(Width, Height: integer);
    destructor Destroy; override;
  public
    { Compute NextFrame and increase NextFrame value }
    procedure ComputeNextFrame;
    { Compute frame specifing the ID }
    procedure ComputeFrame(FrameID: integer);
    { Draw computed frame }
    procedure Draw(ACanvas: TCanvas; ARect: TRect);
  public
    { Use NextFrame property to get or set the frame that will be computed }
    property NextFrame: integer read FNextFrame write SetNextFrame;
    property MaxFrameID: integer read FMaxFrameID;
  end;

implementation

{ TKoala }

function TKoala.BGRALoadFromResourceName(ResName: string): TBGRABitmap;
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

function TKoala.SecondsToFrames(Seconds: integer): integer;
begin
  { Using a fixed value of 33 ms (1000 ms / 30 frames by sec) }
  Result := Seconds * 33;
end;

procedure TKoala.TVScanLines(Bitmap: TBGRABitmap);
var
  x, y: integer;
  p: PBGRAPixel;
begin
  for y := 0 to Bitmap.Height - 1 do
  begin
    p := Bitmap.Scanline[y];
    for x := 0 to Bitmap.Width - 1 do
    begin
      if Odd(y) then
      begin
        p^.red := p^.red - (p^.red shr 2);
        p^.green := p^.green - (p^.green shr 2);
        p^.blue := p^.blue - (p^.blue shr 2);
        //p^.alpha := 255;
      end;
      Inc(p);
    end;
  end;
  Bitmap.InvalidateBitmap;
end;

procedure TKoala.Blur(Bitmap: TBGRABitmap; Alpha: byte);
var
  tmp: TBGRABitmap;
begin
  tmp := TBGRABitmap(Bitmap.FilterBlurRadial(4, rbBox));
  Bitmap.PutImage(0, 0, tmp, dmLinearBlend, Alpha);
  tmp.Free;
end;

function TKoala.RandomBW: byte;
begin
  if RandomRange(0, 2) = 0 then
    Result := 0
  else
    Result := 255;
end;

procedure TKoala.NoiseBWA(Bitmap: TBGRABitmap);
var
  i: integer;
  p: PBGRAPixel;
  c: byte;
begin
  p := Bitmap.Data;

  for i := Bitmap.NBPixels - 1 downto 0 do
  begin
    c := RandomBW;
    p^.red := c;
    p^.green := c;
    p^.blue := c;
    p^.alpha := Random(256);
    Inc(p);
  end;
end;

procedure TKoala.PhotoNoise(Bitmap: TBGRABitmap; Ammount: byte);
var
  tmp: TBGRABitmap;
begin
  tmp := TBGRABitmap.Create(Bitmap.Width, Bitmap.Height);
  NoiseBWA(tmp);
  BGRAReplace(tmp, tmp.FilterBlurRadial(1, rbBox));
  Bitmap.PutImage(0, 0, tmp, dmLinearBlend, Ammount);
  tmp.Free;
end;

function TKoala.RandText: string;
var
  i: integer;
begin
  i := RandomRange(0, 4);

  case i of
    0: Result := 'by.007';
    1: Result := 'b y  0 0 7';
    2: Result := 'b_y:0_0_7';
    3: Result := 'b-y-0-0-7';
  end;
end;

procedure TKoala.DrawCredits(Bitmap: TBGRABitmap; ID: integer);
var
  tmp: TBGRABitmap;
begin
  tmp := BGRATextFX.TextShadow(FWidth, FHeight, RandText, FHeight shr
    2, BGRAWhite, BGRABlack, 30, 30, 10) as TBGRABitmap;
  Bitmap.PutImage(0, 0, tmp, dmLinearBlend, FFrameData[ID].CreditsAlpha);
  tmp.Free;
end;

procedure TKoala.ComputeFrameData;
var
  ID: integer;
  band_height: integer;
  band_y: integer;
  band_min_speed: integer;
  band_max_speed: integer;
  noise_fading: byte;
begin
  band_height := trunc(FHeight * 0.10);
  band_y := 0;
  band_min_speed := 1;
  band_max_speed := 5;
  noise_fading := 0;

  for ID := 0 to High(FFrameData) do
  begin
    { Blur Alpha }
    FFrameData[ID].BlurAlpha := RandomRange(127, 256);

    { Vignette }
    FFrameData[ID].VignetteColor :=
      BGRA(RandomRange(0, 10), 0, RandomRange(0, 50), RandomRange(250, 256));

    { Top Image }
    FFrameData[ID].TopImageRect :=
      Rect(RandomRange(0, 5), RandomRange(0, 5) -
      RandomRange(FHeight, FHeight - 5) + band_y - band_height,
      RandomRange(FWidth, FWidth - 5), band_y - band_height);

    { Bottom Image }
    FFrameData[ID].BottomImageRect :=
      Rect(RandomRange(0, 5), RandomRange(0, 5) + band_y,
      RandomRange(FWidth, FWidth - 5), band_y + RandomRange(FHeight, FHeight - 5));

    { Photo Noise }
    if (ID >= SecondsToFrames(25)) and (ID <= SecondsToFrames(25) + 254) then
    begin
      FFrameData[ID].NoiseFading := noise_fading;
      noise_fading := noise_fading + 1;
    end;

    { Credits }
    if (ID >= SecondsToFrames(35)) and (ID <= SecondsToFrames(39)) then
      FFrameData[ID].CreditsAlpha := RandomRange(127, 245);

    { Banding }

    if ID < SecondsToFrames(21) then
    begin
      Inc(band_y, RandomRange(band_min_speed, band_max_speed));

      { Band }
      FFrameData[ID].BandRect := Rect(0, band_y - band_height, FWidth, band_y);

      if ID = SecondsToFrames(10) then
      begin
        band_min_speed := 10;
        band_max_speed := 15;
      end;

      if ID = SecondsToFrames(15) then
      begin
        band_min_speed := 25;
        band_max_speed := 30;
      end;

      if ID = SecondsToFrames(18) then
      begin
        band_min_speed := 1;
        band_max_speed := 2;
      end;

      if ID = SecondsToFrames(20) then
      begin
        band_min_speed := 0;
        band_max_speed := 2;
      end;

      if (band_y - band_height >= FHeight) then
        band_y := 0;
    end
    else
      band_y := 0;
  end;
end;

procedure TKoala.SetNextFrame(AValue: integer);
begin
  if FNextFrame = AValue then
    Exit;
  FNextFrame := AValue;
  if FNextFrame > FMaxFrameID then
    FNextFrame := FMaxFrameID;
end;

constructor TKoala.Create(Width, Height: integer);
begin
  { BGRA }
  FBitmap := TBGRABitmap.Create(Width, Height);
  { Copy these values }
  FWidth := Width;
  FHeight := Height;
  { Load Picture from resource }
  FKoala := BGRALoadFromResourceName('KOALA');
  { Resample Picture }
  BGRAReplace(FKoala, TBGRABitmap(FKoala.Resample(Width, Height)));
  { Frame counter }
  FNextFrame := 0;
  { Frame data length, the same as the duration of the movie }
  FMaxFrameID := SecondsToFrames(40);
  SetLength(FFrameData, FMaxFrameID);
  { Decrease by one because is zero based }
  Dec(FMaxFrameID);
  { Each frame uses all this data, precompute all of them }
  ComputeFrameData;
  FVignettePoint1 := PointF(FWidth shr 1, FHeight shr 1);
  FVignettePoint2 := PointF(0, 0);
end;

destructor TKoala.Destroy;
begin
  { The end is coming, I feel it... }
  FBitmap.Free;
  FKoala.Free;
  inherited Destroy;
end;

procedure TKoala.ComputeNextFrame;
begin
  { Compute Frame based on FNextFrame value }
  ComputeFrame(FNextFrame);
  { Prepare FNextFrame for future }
  if FNextFrame < FMaxFrameID then
    Inc(FNextFrame);
end;

procedure TKoala.ComputeFrame(FrameID: integer);
begin
  if FrameID > FMaxFrameID then
    FrameID := FMaxFrameID;

  { Clear background }
  FBitmap.FillTransparent;

  { * The fun is here * }

  { TopImage }
  FBitmap.StretchPutImage(FFrameData[FrameID].TopImageRect, FKoala,
    dmLinearBlend);
  { BottomImage }
  FBitmap.StretchPutImage(FFrameData[FrameID].BottomImageRect, FKoala,
    dmLinearBlend);
  { TV Filter }
  TVScanLines(FBitmap);
  { Blur }
  Blur(FBitmap, FFrameData[FrameID].BlurAlpha);
  { Band }
  FBitmap.Rectangle(FFrameData[FrameID].BandRect.Left,
    FFrameData[FrameID].BandRect.Top, FFrameData[FrameID].BandRect.Right,
    FFrameData[FrameID].BandRect.Bottom, BGRAPixelTransparent, BGRABlack, dmSet);
  { Disable GrayScale at 29 sec }
  if not (FrameID >= SecondsToFrames(33)) then
    FBitmap.InplaceGrayscale;
  { Vignette }
  FBitmap.GradientFill(0, 0, FWidth, FHeight, BGRAPixelTransparent,
    FFrameData[FrameID].VignetteColor, gtRadial, FVignettePoint1,
    FVignettePoint2, dmLinearBlend);
  { Flip Bitmap at 22 - 23 sec }
  if (FrameID >= SecondsToFrames(22)) and (FrameID <= SecondsToFrames(23)) then
    FBitmap.HorizontalFlip;
  { Photo Noise }
  if (FrameID >= SecondsToFrames(25)) and (FrameID <= SecondsToFrames(25) + 254) then
    PhotoNoise(FBitmap, FFrameData[FrameID].NoiseFading);
  { Credits }
  if (FrameID >= SecondsToFrames(35)) and (FrameID <= SecondsToFrames(39)) then
  begin
    FBitmap.InplaceGrayscale;
    TVScanLines(FBitmap);
    DrawCredits(FBitmap, FrameID);
  end;
  { The Empty }
  if (FrameID > SecondsToFrames(39)) then
    FBitmap.Fill(BGRABlack);
end;

procedure TKoala.Draw(ACanvas: TCanvas; ARect: TRect);
begin
  { Stretch draw frame in the given canvas with specified rect }
  FBitmap.Draw(ACanvas, ARect);
end;

end.
