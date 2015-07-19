unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Graphics, ExtCtrls,
  BGRAVirtualScreen, BGRABitmap, BGRABitmapTypes, BCTypes, Math, BCFilters,
  BGRATextFX;

const
  SPEEDMS = 15;
  FPS = (1000 div SPEEDMS);
  EXPORTIMAGES = false; // save image sequence to 'out' directory

type

  { TForm1 }

  TForm1 = class(TForm)
    BGRAVirtualScreen1: TBGRAVirtualScreen;
    Timer1: TTimer;
    procedure BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure BGRAVirtualScreen1Resize(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    image: TBGRABitmap;
    y_band: integer;
    band_min_speed: integer;
    band_max_speed: integer;
    band_height: integer;
    fading: byte;
    gray: boolean;
    moving: boolean;
    framecount: integer;
    outpath: string;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure NewTVScanLinesH(Bitmap: TBGRABitmap);
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
        p^.red := p^.red - trunc(p^.red * 0.5);
        p^.green := p^.green - trunc(p^.green * 0.5);
        p^.blue := p^.blue - trunc(p^.blue * 0.5);
        //p^.alpha := 255;
      end;
      Inc(p);
    end;
  end;
  Bitmap.InvalidateBitmap;
end;

procedure NewPhotoNoise(Bitmap: TBGRABitmap; Ammount: byte);
var
  tmp: TBGRABitmap;
begin
  tmp := TBGRABitmap.Create(Bitmap.Width, Bitmap.Height);
  NoiseBWA(tmp);
  BGRAReplace(tmp, tmp.FilterBlurRadial(1, rbFast));
  Bitmap.BlendImageOver(0, 0, tmp, boLinearBlend, Ammount);
  tmp.Free;
end;

procedure TForm1.BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);

function RandBool: boolean;
begin
  result := (RandomRange(0,2) = 1);
end;

function RandText: string;
var
  i: integer;
begin
  i := RandomRange(0,4);

  case i of
    0: Result := 'by.007';
    1: Result := 'b y  0 0 7';
    2: Result := 'b_y:0_0_7';
    3: Result := 'b-y-0-0-7';
  end;
end;

var
  tmp: TBGRABitmap;
begin
  { Top image }
  Bitmap.StretchPutImage(Rect(RandomRange(0, 5), RandomRange(0, 5) - RandomRange(Height, Height - 5) + y_band - band_height, RandomRange(Width, Width - 5), y_band - band_height), image, dmSet);
  { Bottom image }
  Bitmap.StretchPutImage(Rect(RandomRange(0, 5), RandomRange(0, 5) + y_band, RandomRange(Width, Width - 5), y_band + RandomRange(Height, Height - 5)), image, dmSet);
  { TV filter }
  NewTVScanLinesH(Bitmap);
  { Blur }
  tmp := Bitmap.FilterBlurRadial(4, rbBox) as TBGRABitmap;
  Bitmap.StretchPutImage(Rect(0, 0, Width, Height), tmp, dmDrawWithTransparency,
    RandomRange(128, 255));
  tmp.Free;
  { Grayscale}
  if gray then
    Bitmap.InplaceGrayscale;
  { Vignette }
  Bitmap.GradientFill(0, 0, Width, Height, BGRAPixelTransparent, BGRA(
    RandomRange(0, 10), 0, RandomRange(0, 50), RandomRange(250, 256)), gtRadial, PointF(Width div 2, Height div 2),
    PointF(0, 0), dmDrawWithTransparency);
  { Band }
  Bitmap.Rectangle(0, y_band - band_height, Width, y_band, BGRAPixelTransparent, BGRA(0, 0, 0, RandomRange(240, 255)), dmDrawWithTransparency);

  { Band calculations }

  // stop banding at 21 seconds
  if framecount < (FPS * 21) then
    Inc(y_band, RandomRange(band_min_speed, band_max_speed))
  else
    y_band := 0;

  if (y_band - band_height > Height) then
    y_band := 0;

  // 10 seconds
  if framecount = (FPS * 10) then
  begin
    band_min_speed := 10;
    band_max_speed := 15;
  end;
  // 15 seconds
  if framecount = (FPS * 15) then
  begin
    band_min_speed := 25;
    band_max_speed := 30;
  end;
  // 18
  if framecount = (FPS * 18) then
  begin
    band_min_speed := 1;
    band_max_speed := 2;
  end;
  // 20
  if framecount = (FPS * 20) then
  begin
    band_min_speed := 0;
    band_max_speed := 2;
  end;
  // 22 to 23
  if (framecount >= (FPS * 22)) and (framecount <= (FPS * 23)) then
    Bitmap.HorizontalFlip;
  // 25 to 29
  if (framecount >= (FPS * 25)) and (framecount <= (FPS * 29)) then
  begin
    Inc(fading);
    NewPhotoNoise(Bitmap, fading);
  end;
  // 29
  if (framecount = (FPS * 29)) then
  begin
    gray := false;
  end;
  // 30
  if (framecount > (FPS * 30)) then
  begin
    tmp := bitmap.FilterCylinder as TBGRABitmap;
    bitmap.PutImage(RandomRange(0,5),RandomRange(0,5),tmp,dmDrawWithTransparency,RandomRange(50,200));
    tmp.Free;

  end;
  if (framecount = (FPS * 35)) then
  begin
    Fading := 5;
    moving := true;
  end;
  if (framecount >= (FPS * 35)) then
  begin
    if moving then
      Inc(Fading,5);
    if fading = 255 then
      moving := false;

    tmp := bitmap.FilterBlurMotion(fading,RandomRange(0,360), true) as TBGRABitmap;
    if moving then
     bitmap.PutImage(RandomRange(0,5),RandomRange(0,5),tmp,dmDrawWithTransparency,RandomRange(50,200))
    else
      bitmap.PutImage(0,0,tmp,dmDrawWithTransparency,255);
    tmp.Free;
  end;

  if framecount >= FPS * 37 then
  begin
    bitmap.InplaceGrayscale;
    NewTVScanLinesH(Bitmap);
  end;

  if (framecount >= FPS * 37) and (framecount <= FPS * 39) then
  begin
   tmp := BGRATextFX.TextShadow(Width,Height,RandText, height div 3, BGRAWhite, BGRABlack, 30, 30, 10) as TBGRABitmap;
   Bitmap.PutImage(RandomRange(0,5),RandomRange(0,5),tmp,dmDrawWithTransparency,RandomRange(100,250));
   tmp.Free;
  end;

  if (framecount = (FPS * 38)) then
  begin
    fading := 0;
  end;

  if (framecount >= (FPS * 38)) then
  begin
    if fading < 254 then
      Inc(fading);
    bitmap.Fill(BGRA(0,0,0,fading));
  end;

  // end
  if (framecount = (FPS * 40)) then
  begin
    Timer1.Enabled := False;
  end;

  { Counter }
  Inc(framecount);

  { Export }
  if (EXPORTIMAGES and Timer1.Enabled) then
    bitmap.SaveToFile(outpath + IntToStr(framecount) + '.png');
end;

procedure TForm1.BGRAVirtualScreen1Resize(Sender: TObject);
begin
  band_height := trunc(Height * 0.10);
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Timer1.Enabled := False;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Timer1.Interval := SPEEDMS;
  image := TBGRABitmap.Create('Koala.jpg');
  y_band := 0;
  framecount := 0;
  band_min_speed := 1;
  band_max_speed := 5;
  fading := 0;
  gray := true;
  outpath := ExtractFilePath(ParamStr(0)) + 'out' + PathDelim;
  CreateDir(outpath);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  image.Free;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  Timer1.Enabled := True;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  BGRAVirtualScreen1.DiscardBitmap;
end;

end.
