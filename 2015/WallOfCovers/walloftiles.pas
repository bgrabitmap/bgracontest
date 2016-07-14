unit WallOfTiles;
//A source by Ara aradeonas@operamail.com
//If you nee any help or want to use it in real app please let me know.
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmap, BGRABitmapTypes, BGRAVirtualScreen,
  Math, contnrs, ExtCtrls, Forms, AnimateEasing, LCLIntf, BCTypes;

type
  TTileChangeEffect = (tceEffect1, tceEffect2, tceEffect3);

  TTile = class
    Bitmap: TBGRABitmap;
    BitmapIndex: integer;
    Top, Left, Width, Height: integer;
    Visible: boolean;
    Size: integer;
  end;

  { TTiles }

  TTiles = class
  private
    FList: TFPObjectList;
    function GetCount: integer;
    function GetItem(Index: integer): TTile;

    procedure SetItem(Index: integer; AValue: TTile);
  public
    constructor Create;
    destructor Destroy; override;
    function Add(Bitmap: TBGRABitmap): integer;
    function Add(Tile: TTile): integer;
    property Items[Index: integer]: TTile read GetItem write SetItem;
    property Count: integer read GetCount;
  end;

  { TWallOfTiles }

  TWallOfTiles = class(TBGRAVirtualScreen)
  private
    FOnDrawWall: TBGRARedrawEvent;
    //ttt: int64;
    Wall, EffectedWall: TBGRABitmap;
    FActive: boolean;
    FInterval: integer;
    Timer: TTimer;
    BitmapX, BitmapY: integer;
    ChangeSource, ChangeDestination: integer;
    ChangeDestinationBitmap: TBGRABitmap;
    ChangeEffect: TTileChangeEffect;
    DrawTiles: TTiles;
    ResizedTiles: TTiles;
    AnimateEasing: TAnimateEasing;
    procedure AnimateTickEvent(Sender: TObject; Value: extended);
    procedure AnimateFinishEvent(Sender: TObject);
    procedure SetActive(AValue: boolean);
    procedure TimerOnTimer(Sender: TObject);
    procedure CalcTiles;
    procedure DrawWall;
    procedure ChangeADrawTile;
  protected
    procedure RedrawBitmapContent; override;
    procedure Resize; override;
    function Effect1(Bmp1, Bmp2: TBGRABitmap; Percent: single; BackgroundColor: TBGRAPixel): TBGRABitmap;
    function Effect2(Bmp1, Bmp2: TBGRABitmap; Percent: single; BackgroundColor: TBGRAPixel): TBGRABitmap;
    function Effect3(Bmp1, Bmp2: TBGRABitmap; Percent: single; BackgroundColor: TBGRAPixel): TBGRABitmap;
    function WallEffect1(w, h: integer; Percent: single): TBGRABitmap;
    function GetTileBitmap(Index: integer; W, H: integer; S: integer): TBGRABitmap;
  public
    Tiles: TTiles;
    TileSize: integer;
    Margin: integer;
    MakeFullScreen: boolean;
    AnimateTime: integer;
    BlackAndWhite: boolean;
    MovingCamera: boolean;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property Active: boolean read FActive write SetActive;
    property Interval: integer read FInterval write FInterval;
    property OnDrawWall: TBGRARedrawEvent read FOnDrawWall write FOnDrawWall;
  end;

implementation

{ TTiles }

function TTiles.GetCount: integer;
begin
  Result := FList.Count;
end;

function TTiles.GetItem(Index: integer): TTile;
begin
  Result := TTile(FList.Items[Index]);
end;

function TTiles.Add(Tile: TTile): integer;
begin
  Result := FList.Add(Tile);
end;

procedure TTiles.SetItem(Index: integer; AValue: TTile);
begin
  FList.Items[Index] := AValue;
end;

constructor TTiles.Create;
begin
  FList := TFPObjectList.Create;
end;

destructor TTiles.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TTiles.Add(Bitmap: TBGRABitmap): integer;
var
  Tile: TTile;
begin
  Tile := TTile.Create;
  Tile.Bitmap := Bitmap;
  Result := Add(Tile);
end;

{ TWallOfTiles }

procedure TWallOfTiles.SetActive(AValue: boolean);
begin
  if FActive = AValue then
    Exit;
  FActive := AValue;
  Timer.Enabled := AValue;
  AnimateEasing.StopAnimating;
  CalcTiles;
  DrawWall;
end;

procedure TWallOfTiles.AnimateTickEvent(Sender: TObject; Value: extended);
var
  bmp: TBGRABitmap;
begin
  Timer.Enabled := False;
  bmp := TBGRABitmap.Create;
  case ChangeEffect of
    tceEffect1: bmp := Effect1(DrawTiles.Items[ChangeSource].Bitmap, ChangeDestinationBitmap, Value, BGRABlack);
    tceEffect2: bmp := Effect2(DrawTiles.Items[ChangeSource].Bitmap, ChangeDestinationBitmap, Value, BGRABlack);
    tceEffect3: bmp := Effect3(DrawTiles.Items[ChangeSource].Bitmap, ChangeDestinationBitmap, Value, BGRABlack);
  end;
  BGRAReplace(DrawTiles.Items[ChangeSource].Bitmap, bmp);
  DrawWall;
end;

procedure TWallOfTiles.AnimateFinishEvent(Sender: TObject);
var
  f, t: integer;
begin
  ChangeSource := -1;
  ChangeDestination := -1;
  f := round(0.5 * Interval);
  t := round(1.5 * Interval);
  Timer.Interval := RandomRange(f, t);
  Timer.Enabled := True;
end;

procedure TWallOfTiles.TimerOnTimer(Sender: TObject);
begin
  ChangeADrawTile;
  DrawWall;
end;

procedure TWallOfTiles.CalcTiles;
var
  nh, nw, nc, h, w: integer;
  inl: array of integer;
  i, j, c, cd: integer;
  rx, ry: integer;
  ts: integer;
  T2: TTile;
  x3c, x4c, xs: integer;

  function SpaceAvailable(Index, Size: integer): boolean;
  var
    i, j: integer;
  begin
    Result := True;
    for i := 1 to (Size * Size) do
    begin
      j := Index + ((i - 1) mod Size) + (((i - 1) div Size) * nh);
      if (inl[j] = -1) or (j >= nc) then
      begin
        Result := False;
        Break;
      end;
    end;
  end;

  procedure AddTile(Index, Size: integer);
  var
    i, j, k, xx, yy: integer;
  begin
    k := -1;
    while k = -1 do
    begin
      k := inl[Random(Length(inl))];
    end;
    rx := Index div nh * ts;
    ry := Index mod nh * ts;
    T2 := TTile.Create;
    T2.Left := rx;
    T2.Top := ry;
    T2.Width := TileSize * Size + ((Size - 1) * Margin * 2);
    T2.Height := TileSize * Size + ((Size - 1) * Margin * 2);
    T2.Visible := True;
    T2.Size := Size;
    BGRAReplace(T2.Bitmap, GetTileBitmap(k, T2.Width, T2.Height, Size));
    DrawTiles.Add(T2);
    for i := 0 to (Size * Size) - 1 do
    begin
      xx := (Index div nh) + (i div Size) + 1;
      yy := (Index mod nh) + (i mod Size) + 1;
      if (xx <= nw) and (yy <= nh) then
      begin
        j := Index + (i mod Size) + ((i div Size) * nh);
        inl[j] := -1;
        Inc(c);
      end;
    end;
    Inc(cd);
  end;

begin
  if not Active then
  begin
    Exit;
  end;
  ts := TileSize + Margin * 2;
  if MakeFullScreen then
  begin
    h := Screen.Height;
    w := Screen.Width;
  end
  else
  begin
    h := Height;
    w := Width;
  end;
  nh := ceil(h / ts);
  nw := ceil(w / ts);
  if MovingCamera then
  begin
    nh := ceil(1.2 * nh);
    nw := ceil(1.2 * nw);
  end;
  nc := nh * nw;

  SetLength(inl, nc);
  for i := 0 to Length(inl) - 1 do
  begin
    inl[i] := i mod Tiles.Count;
  end;
  if Assigned(Wall) then
    FreeAndNil(Wall);
  Wall := TBGRABitmap.Create(nw * ts, nh * ts, BGRABlack);

  if Assigned(EffectedWall) then
    FreeAndNil(EffectedWall);

  //ttt := GetTickCount64;
  EffectedWall := WallEffect1(Wall.Width, Wall.Height, 100);
  //WriteLn(GetTickCount64 - ttt);

  if Assigned(DrawTiles) then
    FreeAndNil(DrawTiles);

  DrawTiles := TTiles.Create;

  x4c := ceil(nc / 20);
  x3c := x4c * 3;
  c := 0;
  cd := 0;
  while Length(inl) > c do
  begin
    if cd <= x4c then
      xs := 4
    else if cd <= x4c + x3c then
      xs := 3
    else
      xs := 1;

    j := Random(nc);
    if SpaceAvailable(j, xs) then
    begin
      AddTile(j, xs);
    end
    else if xs > 1 then
    begin
      xs := 1;
      if SpaceAvailable(j, xs) then
      begin
        AddTile(j, xs);
      end;
    end;
  end;

  BitmapX := round((w - (nw * ts)) / 2);
  BitmapY := round((h - (nh * ts)) / 2);
end;

procedure TWallOfTiles.DrawWall;
var
  i: integer;
  bm: TBGRABitmap;
begin
  if not Active then
  begin
    DiscardBitmap;
    Exit;
  end;
  if ChangeSource <> -1 then
  begin
    with DrawTiles.Items[ChangeSource] do
    begin
      if Visible then
      begin
        Wall.PutImage(Left, Top, Bitmap, dmSet);
        bm := TBGRABitmap.Create;
        BGRAReplace(bm, EffectedWall.GetPart(Rect(Left, Top, Left + Width, Top + Height)));
        Wall.BlendImage(Left, Top, bm, boMultiply);
        bm.Free;
      end;
    end;
  end
  else
  begin
    for i := 0 to DrawTiles.Count - 1 do
    begin
      with DrawTiles.Items[i] do
      begin
        if Visible then
        begin
          Wall.PutImage(Left, Top, Bitmap, dmSet);
        end;
      end;
    end;
    Wall.BlendImage(0, 0, EffectedWall, boMultiply);
  end;
  DiscardBitmap;
end;

procedure TWallOfTiles.ChangeADrawTile;
var
  i, c: integer;
  t: TTile;
begin
  ChangeSource := -1;
  ChangeDestination := -1;
  c := 0;
  while (ChangeSource = ChangeDestination) and (c < Tiles.Count) do
  begin
    i := Random(DrawTiles.Count - 1);
    t := DrawTiles.Items[i];
    if t.Visible then
    begin
      ChangeSource := i;
      ChangeDestination := Random(Tiles.Count - 1);
      ChangeEffect := TTileChangeEffect(Random(3));
    end;
    Inc(c);
  end;
  if (ChangeSource <> ChangeDestination) then
  begin
    ChangeDestinationBitmap := GetTileBitmap(ChangeDestination, t.Width, t.Height, t.Size);
    AnimateEasing.Animating(0, 100, AnimateTime, etSineEaseIn);
  end;
end;

procedure TWallOfTiles.RedrawBitmapContent;
begin
  inherited RedrawBitmapContent;
  if Active then
  begin
    Bitmap.PutImage(BitmapX, BitmapY, Wall, dmSet);
  end
  else
    Bitmap.Fill(BGRAWhite);

  if Assigned(FOnDrawWall) then
    FOnDrawWall(Self, Bitmap);
end;

procedure TWallOfTiles.Resize;
begin
  DiscardBitmap;
  inherited Resize;
end;

function TWallOfTiles.Effect1(Bmp1, Bmp2: TBGRABitmap; Percent: single; BackgroundColor: TBGRAPixel): TBGRABitmap;
var
  Alpha: byte;
begin
  if Percent > 100 then
    Percent := 100;
  Result := TBGRABitmap.Create(bmp1.Width, bmp1.Height, BackgroundColor);
  if Percent < 50 then
  begin
    Alpha := 255 - round(Percent * 2 / 100 * 255);
    Result.PutImage(0, 0, bmp1, dmDrawWithTransparency, Alpha);
  end
  else
  begin
    Alpha := round((Percent - 50) * 2 / 100 * 255);
    Result.PutImage(0, 0, bmp2, dmDrawWithTransparency, Alpha);
  end;
end;

function TWallOfTiles.Effect2(Bmp1, Bmp2: TBGRABitmap; Percent: single; BackgroundColor: TBGRAPixel): TBGRABitmap;
var
  Alpha: byte;
  OldSize, NewSize, x: integer;
  bmp: TBGRABitmap;
begin
  if Percent > 100 then
    Percent := 100;
  Result := TBGRABitmap.Create(bmp1.Width, bmp1.Height, BackgroundColor);
  OldSize := Bmp1.Width;
  if Percent < 50 then
  begin
    Alpha := 255 - round(Percent * 2 / 100 * 255);
    x := round(Percent / 100 * OldSize);
    NewSize := OldSize - x - x;
    bmp := TBGRABitmap.Create;
    BGRAReplace(bmp, Bmp1.Resample(NewSize, OldSize, rmSimpleStretch));
    Result.PutImage(x, 0, bmp, dmDrawWithTransparency, Alpha);
    bmp.Free;
  end
  else
  begin
    Alpha := round((Percent - 50) * 2 / 100 * 255);
    x := round((OldSize / 2) - ((Percent - 50) / 100 * OldSize));
    NewSize := OldSize - x - x;
    bmp := TBGRABitmap.Create;
    BGRAReplace(bmp, Bmp2.Resample(NewSize, OldSize, rmSimpleStretch));
    Result.PutImage(x, 0, bmp, dmDrawWithTransparency, Alpha);
    bmp.Free;
  end;
end;

function TWallOfTiles.Effect3(Bmp1, Bmp2: TBGRABitmap; Percent: single; BackgroundColor: TBGRAPixel): TBGRABitmap;
var
  Alpha: byte;
  OldSize, NewSize, x: integer;
  bmp: TBGRABitmap;
begin
  if Percent > 100 then
    Percent := 100;
  Result := TBGRABitmap.Create(bmp1.Width, bmp1.Height, BackgroundColor);
  OldSize := Bmp1.Width;
  if Percent < 50 then
  begin
    Alpha := 255 - round(Percent * 2 / 100 * 255);
    x := round(Percent / 100 * OldSize);
    NewSize := OldSize - x - x;
    bmp := TBGRABitmap.Create;
    BGRAReplace(bmp, Bmp1.Resample(NewSize, OldSize, rmSimpleStretch));
    Result.PutImage(x, 0, bmp, dmDrawWithTransparency, Alpha);
    bmp.Free;
  end
  else
  begin
    Alpha := round((Percent - 50) * 2 / 100 * 255);
    Result.PutImage(0, 0, bmp2, dmDrawWithTransparency, Alpha);
  end;
end;

function TWallOfTiles.WallEffect1(w, h: integer; Percent: single): TBGRABitmap;
var
  layer: TBGRABitmap;
  i, j, ww, hh: integer;
  p1, p2: TPointF;
  c1, c2: TBGRAPixel;
  co: integer;
begin
  Result := TBGRABitmap.Create(w, h);
  ww := Result.Width;
  hh := Result.Height;
  for i := 1 to 5 do
  begin
    case i of
      1:
      begin
        c1 := BGRA(179, 130, 210, 255);
        c2 := BGRA(255, 255, 255, 255);
        p1 := PointF(-ww / 5, -hh / 5);
        p2 := PointF(ww, hh);
        co := 2;
      end;
      2:
      begin
        c1 := BGRA(49, 206, 148, 255);
        c2 := BGRA(255, 255, 255, 255);
        p1 := PointF(ww / 3, hh * 1.1);
        p2 := PointF(0, 0);
        co := 2;
      end;
      3:
      begin
        c1 := BGRA(131, 198, 57, 255);
        c2 := BGRA(255, 255, 255, 255);
        p1 := PointF(ww * 1.1, hh / 2);
        p2 := PointF(ww * 0.2, hh / 2);
        co := 2;
      end;
      4:
      begin
        c1 := BGRABlack;
        c2 := BGRA(255, 255, 255, 255);
        p1 := PointF(ww, -hh / 2);
        p2 := PointF(ww / 2, hh / 2);
        co := 1;
      end;
      5:
      begin
        c1 := BGRA(0, 0, 0, 0);
        c2 := BGRAWhite;
        p1 := PointF(-ww / 5, hh * 1.2);
        p2 := PointF(ww / 4, hh * 4 / 5);
        co := 3;
      end;
    end;
    for j := 1 to co do
    begin
      layer := TBGRABitmap.Create(Result.Width, Result.Height);
      layer.GradientFill(0, 0, layer.Width, layer.Height, c1, c2, gtRadial, p1, p2, dmSet);
      Result.BlendImage(0, 0, layer, boMultiply);
      layer.Free;
    end;
  end;
end;

function TWallOfTiles.GetTileBitmap(Index: integer; W, H: integer; S: integer): TBGRABitmap;
var
  i: integer;
  T: TTile;
  Found: boolean;
begin
  Found := False;
  for i := 0 to ResizedTiles.Count - 1 do
  begin
    with ResizedTiles.Items[i] do
    begin
      if (BitmapIndex = Index) and (Width = w) and (Height = h) and (Size = s) then
      begin
        Result := TBGRABitmap.Create(Bitmap);
        Found := True;
        Break;
      end;
    end;
  end;
  if not Found then
  begin
    Result := TBGRABitmap.Create;
    BGRAReplace(Result, Tiles.Items[Index].Bitmap.Resample(W, H));
    if BlackAndWhite then
      Result.InplaceGrayscale;
    T := TTile.Create;
    T.Left := -1;
    T.Top := -1;
    T.Width := w;
    T.Height := h;
    T.Visible := False;
    T.Size := s;
    T.Bitmap := Result;
    T.BitmapIndex := Index;
    ResizedTiles.Add(T);
  end;
end;

constructor TWallOfTiles.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Active := False;
  Tiles := TTiles.Create;
  DrawTiles := TTiles.Create;
  Timer := TTimer.Create(Self);
  Timer.Enabled := False;
  Timer.OnTimer := @TimerOnTimer;
  AnimateEasing := TAnimateEasing.Create;
  AnimateEasing.OnTick := @AnimateTickEvent;
  AnimateEasing.OnFinish := @AnimateFinishEvent;
  BlackAndWhite := True;
  ResizedTiles := TTiles.Create;
  MovingCamera := True;
end;

destructor TWallOfTiles.Destroy;
begin
  ResizedTiles.Free;
  AnimateEasing.Free;
  Timer.Free;
  DrawTiles.Free;
  Tiles.Free;
  inherited Destroy;
end;

end.
