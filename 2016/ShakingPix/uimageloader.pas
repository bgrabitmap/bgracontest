{
 *****************************************************************************
  This file is part of ShakingPix
  Copyright (c) 2016 by Michael W Vogel

  See the file COPYING.LGPL.txt, included in this distribution,
  for details about the license.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *****************************************************************************
}

unit uImageLoader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, math, FileUtil, Graphics, Forms,
  BGRABitmap, BGRABitmapTypes, BGRAOpenGL, BGRASpriteGL,
  BGLVirtualScreen, syncobjs, FPimage, uDefinitions;

type

  TStreamEvent = procedure(Stream: TStream) of object;
  TImages = class;

  { TBGLVirtualScreen }

  TBGLVirtualScreen = class(BGLVirtualScreen.TBGLVirtualScreen)
  public
    function PrepareBGLContext: TBGLContext;
    procedure ReleaseBGLContext(ctx: TBGLContext);
    procedure Paint; override;
  end;

  TSpriteObjectState = (soNew, soLoading, soLoaded);

  { TSpriteObject }

  TSpriteObject = class
  private
    FHeight: Integer;
    FImageIndex: Integer;
    FPaintRect: TRect;
    FSprite: TBGLCustomSprite;
    FState: TSpriteObjectState;
    FWidth: Integer;
    procedure ImageLoadFinished(Stream: TStream);
  public
    constructor Create(aImageIndex: Integer);
    destructor Destroy; override;
    procedure Load;                                         // TODO Load real Images
    property Height: Integer read FHeight;
    property ImageIndex: Integer read FImageIndex;
    property PaintRect: TRect read FPaintRect write FPaintRect;
    property Sprite: TBGLCustomSprite read FSprite;
    property State: TSpriteObjectState read FState;
    property Width: Integer read FWidth;
  end;

  TSpriteObjectList = specialize TFPGList<TSpriteObject>;

  { TImagesLoader }

  TImagesLoader = class(TThread)
  private
    FImages: TImages;
    procedure DirectoryEvent(FileIterator: TFileIterator);
    procedure FileFound(FileIterator: TFileIterator);
  protected
    procedure Execute; override;
  public
    constructor Create(Owner: TImages); overload;
  end;

  { TImages }

  TImages = class
  private
    FDirectory: String;
    FGLScreen: TBGLVirtualScreen;
    FImageList: TStringList;
    FImagesLoader: TImagesLoader;
    FMBounds: TMBounds;
    FSpriteObjects: TSpriteObjectList;
    FZoomPoint: TPoint;
    procedure DestroyImagesLoader;
    function GetCount: Integer;
    function GetItems(Index: Integer): String;
    procedure SetZoomPoint(AValue: TPoint);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddSpriteObject(aSpriteObject: TSpriteObject);
    procedure LoadFromDir(Directory: String);
    procedure LoadNeededImage;
    procedure RemoveSpriteObject(aSpriteObject: TSpriteObject);
    property MBounds: TMBounds read FMBounds write FMBounds;
    property Count: Integer read GetCount;
    property GLScreen: TBGLVirtualScreen read FGLScreen write FGLScreen;
    property Directory: String read FDirectory;
    property Items[Index: Integer]: String read GetItems; default;
    property SpriteObjects: TSpriteObjectList read FSpriteObjects;
    property ZoomPoint: TPoint read FZoomPoint write SetZoomPoint;
  end;

  { TImageLoadThread }

  TImageLoadThread = class(TThread)
  private
    FFileName: String;
    FOnFinished: TStreamEvent;
    FStream: TStream;
    function Shrinked(Bitmap: TBGRABitmap): TBGRABitmap;
    procedure AddTransparent(Bitmap: TBGRABitmap);
    procedure Finished;
  protected
    procedure Execute; override;
  public
    constructor Create(Filename: String); overload;
    destructor Destroy; override;
    property OnFinished: TStreamEvent read FOnFinished write FOnFinished;
  end;

var
  CSLoader: TCriticalSection;
  CSThreads: TCriticalSection;
  Images: TImages;
  ThreadCnt: Integer;
  AppState: TAppState;

  function MBounds(Left, Top, Width, Height: Integer): TMBounds;
  function ClientToModel(aPoint: TPoint; aWidth, aHeight: Integer): TPoint;
  function ModelToClient(aPoint: TPoint; aWidth, aHeight: Integer): TPoint;

implementation

var AlphaSinusArray: array[0..999] of Integer;

procedure InitAlphaSinusArray;
var
  i: Integer;
begin
  for i := 0 to High(AlphaSinusArray) do
    AlphaSinusArray[i] := Round(sin(i * Pi / High(AlphaSinusArray)) * 255);
end;

function MBounds(Left, Top, Width, Height: Integer): TMBounds;
begin
  Result.Left   := Left;
  Result.Top    := Top;
  Result.Width  := Width;
  Result.Height := Height;
  Result.WindowState := wsNormal;
end;

function ClientToModel(aPoint: TPoint; aWidth, aHeight: Integer): TPoint;
begin
  Result.x := aPoint.x * 2001 div aWidth  - 1000;
  Result.y := aPoint.y * 2001 div aHeight - 1000;
end;

function ModelToClient(aPoint: TPoint; aWidth, aHeight: Integer): TPoint;
begin
  Result.x := aPoint.x * aWidth  div 2001 + aWidth  shr 1;
  Result.y := aPoint.y * aHeight div 2001 + aHeight shr 1;
end;

{ TBGLVirtualScreen }

function TBGLVirtualScreen.PrepareBGLContext: TBGLContext;
begin
  Result := inherited;
end;

procedure TBGLVirtualScreen.ReleaseBGLContext(ctx: TBGLContext);
begin
  inherited;
end;

procedure TBGLVirtualScreen.Paint;
begin
  inherited Paint;
end;

{ TImageLoadThread }

function TImageLoadThread.Shrinked(Bitmap: TBGRABitmap): TBGRABitmap;
var
  Factor: Double;
  newBitmap: TBGRABitmap;
  aWidth, aHeight: Integer;
begin
  if (Bitmap.Width <= MaxBitmapSize) and (Bitmap.Height <= MaxBitmapSize) then Exit(Bitmap);
  Factor := MaxBitmapSize / max(Bitmap.Width, Bitmap.Height);
  aWidth  := max(Round(Bitmap.Width  * Factor), 1);
  aHeight := max(Round(Bitmap.Height * Factor), 1);
  newBitmap := TBGRABitmap.Create(aWidth, aHeight);
  newBitmap.CanvasBGRA.StretchDraw(Rect(0, 0, aWidth, aHeight), Bitmap);
  Result := newBitmap;
  Bitmap.Free;
end;

procedure TImageLoadThread.AddTransparent(
  Bitmap: TBGRABitmap);
var
  MaxSin, x, y: Int32or64;
  p: PBGRAPixel;
begin
  if not Assigned(Bitmap) then Exit;
  if Bitmap.Height < 1 then Exit;
  if Bitmap.Width  < 1 then Exit;
  MaxSin := Length(AlphaSinusArray);
  for y := 0 to Bitmap.Height - 1 do begin
    p := Bitmap.Scanline[y];
    for x := 0 to Bitmap.Width - 1 do
    begin
      p^.alpha := min(AlphaSinusArray[x * MaxSin div Bitmap.Width] * AlphaSinusArray[y * MaxSin div Bitmap.Height] shr 8, p^.alpha);
      Inc(p);
    end;
  end;
  Bitmap.InvalidateBitmap;
end;

procedure TImageLoadThread.Finished;
begin
  if Assigned(FOnFinished) then FOnFinished(FStream);
end;

procedure TImageLoadThread.Execute;
var
  Bitmap: TBGRABitmap;
begin
  if AppState <> asRunning then Exit;
  Bitmap := TBGRABitmap.Create;
  try
    Bitmap.LoadFromFile(FFileName);
    Bitmap := Shrinked(Bitmap);
    AddTransparent(Bitmap);
    FStream := TMemoryStream.Create;
    try
      Bitmap.SaveToStreamAs(FStream, ifBmp);
      FStream.Position := 0;
      if AppState = asRunning then Synchronize(@Finished);
    finally
      FStream.Free;
    end;
  finally
    Bitmap.Free;
  end;
end;

constructor TImageLoadThread.Create(Filename: String);
begin
  Create(False);
  FreeOnTerminate := True;
  FFileName := Filename;
  CSThreads.Acquire;
  Inc(ThreadCnt);
  CSThreads.Release;
end;

destructor TImageLoadThread.Destroy;
begin
  if not (AppState = asDestroying) then begin
    CSThreads.Acquire;
    Dec(ThreadCnt);
    CSThreads.Release;
  end;
  inherited Destroy;
end;

{ TSpriteObject }

procedure TSpriteObject.ImageLoadFinished(Stream: TStream);
var
  aBmp: TBGLBitmap;
  aTexture: IBGLTexture;
  Ctx: TBGLContext;
  Reader: TFPCustomImageReader;
begin
  if AppState <> asRunning then Exit;
  aBmp := TBGLBitmap.Create;
  Reader := CreateBGRAImageReader(ifBmp);
  aBmp.LoadFromStream(Stream, Reader);
  Reader.Free;
  aTexture := aBmp.MakeTextureAndFree;

  Ctx := Images.GLScreen.PrepareBGLContext;

  TBGLSprite.Create(aTexture, 0).Location := PointF(Random(Ctx.Width) - Ctx.Width shr 1, Random(Ctx.Height) - Ctx.Height shr 1);
  FSprite := Ctx.Sprites.Sprite[Ctx.Sprites.Count - 1];
  FSprite.HorizontalAlign := taCenter;
  FSprite.VerticalAlign := tlCenter;
  FSprite.W := 0;
  FSprite.H := 0;

  Images.GLScreen.ReleaseBGLContext(ctx);

  FState := soLoaded;
end;

constructor TSpriteObject.Create(aImageIndex: Integer);
begin
  FImageIndex := aImageIndex;
  FPaintRect := Rect(0, 0, 0, 0);
  FWidth := 0;
  FHeight := 0;
  FState := soNew;
end;

destructor TSpriteObject.Destroy;
begin
  if (Assigned(FSprite)) and not (AppState = asDestroying) then FSprite.Layer := -1;
  inherited Destroy;
end;

procedure TSpriteObject.Load;
var
  ImageLoad: TImageLoadThread;
begin
  if State <> soNew then
    raise Exception.Create('SpriteObject already used');
  if ImageIndex >= Images.Count then Exit;
  FState := soLoading;
  ImageLoad := TImageLoadThread.Create(Images[ImageIndex]);
  ImageLoad.OnFinished := @ImageLoadFinished;
end;

{ TImagesLoader }

procedure TImagesLoader.DirectoryEvent(FileIterator: TFileIterator);
begin
  if Terminated or (AppState = asDestroying) then FileIterator.Stop;
end;

procedure TImagesLoader.FileFound(FileIterator: TFileIterator);
begin
  CSLoader.Acquire;
  FImages.FImageList.Add(FileIterator.FileName);
  CSLoader.Release;
  if Terminated or (AppState = asDestroying) then FileIterator.Stop;
end;

procedure TImagesLoader.Execute;
var
  FS: TFileSearcher;
begin
  FS := TFileSearcher.Create;
  try
    FS.OnFileFound := @FileFound;
    FS.OnDirectoryEnter := @DirectoryEvent;
    FS.OnDirectoryFound := @DirectoryEvent;
    FS.Search(FImages.Directory, '*.bmp;*.png;*.jpg;*.jpeg', True, False);
  finally
    FS.Free;
  end;
end;

constructor TImagesLoader.Create(Owner: TImages);
begin
  FImages := Owner;
  FreeOnTerminate := True;
  Create(False);
end;

{ TImages }

procedure TImages.DestroyImagesLoader;
begin
  if Assigned(FImagesLoader) and not FImagesLoader.Finished then
  begin
    FImagesLoader.Terminate;
    while not FImagesLoader.Finished do Sleep(10);
  end;
  CSLoader.Acquire;
  FImageList.Clear;
  CSLoader.Release;
end;

function TImages.GetCount: Integer;
begin
  CSLoader.Acquire;
  Result := FImageList.Count;
  CSLoader.Release;
end;

function TImages.GetItems(Index: Integer): String;
begin
  CSLoader.Acquire;
  Result := FImageList[Index];
  CSLoader.Release;
end;

procedure TImages.SetZoomPoint(AValue: TPoint);
begin
  FZoomPoint := AValue;
end;

constructor TImages.Create;
begin
  FImageList := TStringList.Create;
  FSpriteObjects := TSpriteObjectList.Create;
end;

destructor TImages.Destroy;
begin
  DestroyImagesLoader;
  FSpriteObjects.Free;
  FImageList.Free;
  inherited Destroy;
end;

procedure TImages.AddSpriteObject(aSpriteObject: TSpriteObject);
begin
  FSpriteObjects.Add(aSpriteObject);
end;

procedure TImages.LoadFromDir(Directory: String);
begin
  FDirectory := Directory;
  DestroyImagesLoader;
  FImagesLoader := TImagesLoader.Create(Self);
end;

procedure TImages.LoadNeededImage;
var
  i: Integer;
begin
  for i := 0 to FSpriteObjects.Count - 1 do
    if FSpriteObjects[i].State = soNew then
    begin
      FSpriteObjects[i].Load;
      Break;
    end;
end;

procedure TImages.RemoveSpriteObject(
  aSpriteObject: TSpriteObject);
var
  aIndex: Integer;
begin
  aIndex := FSpriteObjects.IndexOf(aSpriteObject);
  if aIndex >= 0 then
    FSpriteObjects.Delete(aIndex);
end;

initialization

  CSLoader := TCriticalSection.Create;
  CSThreads := TCriticalSection.Create;
  AppState := asCreating;
  Images := TImages.Create;
  ThreadCnt := 0;
  InitAlphaSinusArray;

finalization

  Images.Free;
  CSThreads.Free;
  CSLoader.Free;

end.

