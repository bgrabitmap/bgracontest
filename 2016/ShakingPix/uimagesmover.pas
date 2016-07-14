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

unit uImagesMover;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, fgl, uImageLoader, BGRASpriteGL, uDefinitions;

type
  TMovedImages = class;
  TLife = class;

  { TCustomTransform }

  TCustomTransform = class
  private
    FLife: TLife;
    FSprite: TBGLCustomSprite;
  public
    constructor Create(Owner: TLife);
    procedure MakeMove; virtual;
  end;

  TTransformList = specialize TFPGList<TCustomTransform>;

  { TTransformOriginalSize }

  TTransformOriginalSize = class(TCustomTransform)          // set the original size
  public
    procedure MakeMove; override;
  end;

  { TTransformRandomBasisPoint }

  TTransformRandomBasisPoint = class(TCustomTransform)      // where is the sprite
  private
    FX, FY: Integer;
  public
    constructor Create(Owner: TLife);
    procedure MakeMove; override;
  end;

  { TTransformExWindowBasisPoint }

  TTransformExWindowBasisPoint = class(TCustomTransform)      // where is the sprite
  private
    FX, FY: Single;
  public
    constructor Create(Owner: TLife);
    procedure MakeMove; override;
  end;


  { TTransformAgeHalfSinusSize }

  TTransformAgeHalfSinusSize = class(TCustomTransform)      // the sprite is growing and shrinking cause of the Live.Age
    FFactor: Single;
  public
    constructor Create(Owner: TLife; Factor: Single); overload;
    procedure MakeMove; override;
  end;

  { TTransformAgeFastGrowAndShrinkSize }

  TTransformAgeFastGrowAndShrinkSize = class(TCustomTransform)  // the sprite is growing and shrinking cause of the Live.Age
    FFactor: Single;
  public
    constructor Create(Owner: TLife; SetRandom: Boolean); overload;
    procedure MakeMove; override;
  end;

  { TTransformSinusSize }

  TTransformSinusSize = class(TCustomTransform)             // the sprite is growing and shrinking all the time
    FFactor: Single;
    FSpeed: Integer;
    FCnt: Integer;
  public
    constructor Create(Owner: TLife; Speed: Integer; Factor: Single); overload;
    procedure MakeMove; override;
  end;

  { TTransformSinusAngle }

  TTransformSinusAngle = class(TCustomTransform)            // the sprite is turning all the time
    FFactor: Single;
    FSpeed: Integer;
    FCnt: Integer;
  public
    constructor Create(Owner: TLife; SetRandom: Boolean); overload;
    procedure MakeMove; override;
  end;

  { TTransformConstantAngle }

  TTransformConstantAngle = class(TCustomTransform)         // the sprite is turning all the time
    FSpeed: Integer;
    FCnt: Integer;
  public
    constructor Create(Owner: TLife; SetRandom: Boolean); overload;
    procedure MakeMove; override;
  end;

  { TTransformMoveFromMouse }

  TTransformMoveFromMouse = class(TCustomTransform)         // moving away from mouse
    FX: Single;
    FY: Single;
  public
    constructor Create(Owner: TLife; SetRandom: Boolean); overload;
    procedure MakeMove; override;
  end;

  { TLife }

  TLife = class
  private
    FAlive: Boolean;
    FAge: Integer;                                          // Alive 0..MaxLife
    FAgeStep: Integer;                                      // increase or decrease FAge
    FTransforms: TTransformList;
    FOnFinish: TNotifyEvent;
    FSpriteObject: TSpriteObject;
    procedure ClearTransforms;
    procedure MoveTransforms;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Die;
    procedure MakeMove;
    property Age: Integer read FAge;
    property Alive: Boolean read FAlive;
    property OnFinish: TNotifyEvent read FOnFinish write FOnFinish;
    property SpriteObject: TSpriteObject read FSpriteObject;
  end;

  TLifes = specialize TFPGList<TLife>;

  { TMovedImages }

  TMovedImages = class
  private
    FCount: Integer;
    FLifes: TLifes;
    procedure SetCount(AValue: Integer);
    procedure DestroyLife(aIndex: Integer);
    procedure AddLife;
    procedure ItemFinished(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Invalidate;
    property Count: Integer read FCount write SetCount;
  end;

implementation

{ TTransformMoveFromMouse }

constructor TTransformMoveFromMouse.Create(Owner: TLife;
  SetRandom: Boolean);
begin
  Create(Owner);
  FX := 0;
  FY := 0;
end;

procedure TTransformMoveFromMouse.MakeMove;
begin
  inherited MakeMove;
  if not Assigned(FSprite) then Exit;
  FX := FX + (FX + FSprite.X - Images.ZoomPoint.x) / 200;
  FY := FY + (FY + FSprite.Y - Images.ZoomPoint.y) / 200;
  FSprite.X := FX + FSprite.X;
  FSprite.Y := FY + FSprite.Y;
end;

{ TTransformExWindowBasisPoint }

constructor TTransformExWindowBasisPoint.Create(Owner: TLife);
var
  i: Integer;
begin
  inherited Create(Owner);
  i := Random(MaxLife);
  FX := SinusArray[i] * (Images.GLScreen.Width shr 3) * (Random(50) / 50 + 1) + Images.ZoomPoint.X;
  i := i + MaxLife shr 2;
  FY := SinusArray[i mod MaxLife] * (Images.GLScreen.Height shr 3) * (Random(50) / 50 + 1) + Images.ZoomPoint.Y;
end;

procedure TTransformExWindowBasisPoint.MakeMove;
begin
  inherited MakeMove;
  if not Assigned(FSprite) then Exit;
  FSprite.X := FX;
  FSprite.Y := FY;
end;

{ TTransformConstantAngle }

constructor TTransformConstantAngle.Create(Owner: TLife; SetRandom: Boolean);
begin
  Create(Owner);
  if SetRandom then
    FSpeed := Random(101) - 50
  else
    FSpeed := 50;
  FCnt := 0;
end;

procedure TTransformConstantAngle.MakeMove;
begin
  inherited MakeMove;
  if not Assigned(FSprite) then Exit;
  FSprite.Angle := FSprite.Angle + FCnt / 100;
  Inc(FCnt, FSpeed);
end;

{ TTransformSinusAngle }

constructor TTransformSinusAngle.Create(Owner: TLife;
  SetRandom: Boolean);
begin
  Create(Owner);
  if SetRandom then begin
    FSpeed := Random(MaxLife div 1000) + 1;
    FFactor := Random(100) - 50;
    FCnt := Random(MaxLife);
  end else begin
    FSpeed := MaxLife div 500;
    FFactor := 50;
    FCnt := 0;
  end;
end;

procedure TTransformSinusAngle.MakeMove;
begin
  inherited MakeMove;
  if not Assigned(FSprite) then Exit;
  FSprite.Angle := FSprite.Angle + FFactor * SinusArray[FCnt mod MaxLife];
  Inc(FCnt, FSpeed);
end;

{ TTransformAgeFastGrowAndShrinkSize }

constructor TTransformAgeFastGrowAndShrinkSize.Create(
  Owner: TLife; SetRandom: Boolean);
begin
  Create(Owner);
  if SetRandom then
    FFactor := (200 + Random(400)) / 200
  else
    FFactor := 2;
end;

procedure TTransformAgeFastGrowAndShrinkSize.MakeMove;
begin
  inherited MakeMove;
  if not Assigned(FSprite) then Exit;
  FSprite.H := Round(FSprite.H * FFactor * FastGrowAndShrinkArray[FLife.Age]);
  FSprite.W := Round(FSprite.W * FFactor * FastGrowAndShrinkArray[FLife.Age]);
end;

{ TTransformSinusSize }

constructor TTransformSinusSize.Create(Owner: TLife;
  Speed: Integer; Factor: Single);
begin
  Create(Owner);
  FFactor := Factor;
  FSpeed := Speed;
  FCnt := 0;
end;

procedure TTransformSinusSize.MakeMove;
begin
  inherited MakeMove;
  if not Assigned(FSprite) then Exit;
  FSprite.H := FSprite.H * (1 + FFactor * SinusArray[FCnt mod MaxLife] / 255);
  FSprite.W := FSprite.W * (1 + FFactor * SinusArray[FCnt mod MaxLife] / 255);
  Inc(FCnt, FSpeed);
end;

{ TTransformOriginalSize }

procedure TTransformOriginalSize.MakeMove;
begin
  inherited MakeMove;
  if not Assigned(FSprite) then Exit;
  FSprite.H := FSprite.Texture.Height;
  FSprite.W := FSprite.Texture.Width;
end;

{ TTransformAgeHalfSinusSize }

constructor TTransformAgeHalfSinusSize.Create(Owner: TLife; Factor: Single);
begin
  Create(Owner);
  FFactor := Factor;
end;

procedure TTransformAgeHalfSinusSize.MakeMove;
begin
  inherited MakeMove;
  if not Assigned(FSprite) then Exit;
  FSprite.H := Round(FSprite.H * FFactor * HalfSinusArray[FLife.Age] / 255);
  FSprite.W := Round(FSprite.W * FFactor * HalfSinusArray[FLife.Age] / 255);
end;

{ TTransformRandomBasisPoint }

constructor TTransformRandomBasisPoint.Create(Owner: TLife);
begin
  inherited Create(Owner);
  FX := Random(Images.GLScreen.Width);
  FY := Random(Images.GLScreen.Height);
end;

procedure TTransformRandomBasisPoint.MakeMove;
begin
  inherited MakeMove;
  if not Assigned(FSprite) then Exit;
  FSprite.X := FX;
  FSprite.Y := FY;
end;

{ TCustomTransform }

procedure TCustomTransform.MakeMove;
begin
  if Assigned(FSprite) then Exit;
  if not Assigned(FLife) then
    raise Exception.Create('No Painter assigned');
  if not Assigned(FLife.SpriteObject) then
    raise Exception.Create('No SpriteObject assigned');
  if not Assigned(FLife.SpriteObject.Sprite) then Exit;
  FSprite := FLife.SpriteObject.Sprite;
end;

constructor TCustomTransform.Create(Owner: TLife);
begin
  FLife := Owner;
end;

{ TLife }

procedure TLife.ClearTransforms;
var
  i: Integer;
begin
  for i := FTransforms.Count - 1 downto 0 do
    FTransforms[i].Free;
  FTransforms.Clear;
end;

procedure TLife.MoveTransforms;
var
  i: Integer;
begin
  for i := 0 to FTransforms.Count - 1 do
    FTransforms[i].MakeMove;
end;

constructor TLife.Create;
begin
  FSpriteObject := Nil;
  FAlive := True;
  FAge := 1;
  FAgeStep := 10 + Random(20);
  FAgeStep := 1;

  FSpriteObject := TSpriteObject.Create(Random(Images.Count));   // TODO take a image near other image
  Images.AddSpriteObject(FSpriteObject);
  FTransforms := TTransformList.Create;
  FTransforms.Add(TTransformOriginalSize.Create(Self));
//  FTransforms.Add(TTransformRandomBasisPoint.Create(Self));
  FTransforms.Add(TTransformExWindowBasisPoint.Create(Self));
  FTransforms.Add(TTransformAgeFastGrowAndShrinkSize.Create(Self, True));
  FTransforms.Add(TTransformSinusAngle.Create(Self, True));
//  FTransforms.Add(TTransformConstantAngle.Create(Self, True));
  FTransforms.Add(TTransformMoveFromMouse.Create(Self, True));
end;

destructor TLife.Destroy;
begin
  FAlive := False;
  ClearTransforms;
  FTransforms.Free;
  Images.RemoveSpriteObject(FSpriteObject);
  FSpriteObject.Free;
  inherited Destroy;
end;

procedure TLife.Die;
begin
  FAlive := False;
end;

procedure TLife.MakeMove;
var
  aSprite: TBGLCustomSprite;
begin
  if not Assigned(SpriteObject) then
    raise Exception.Create('No SpriteObject assigned');
  if not Assigned(SpriteObject.Sprite) then Exit;
  Inc(FAge, FAgeStep);
  if FAge < 0 then FAge := 0;
  if (FAge >= MaxLife) or not Alive then
  begin
    FAlive := False;
    if Assigned(FOnFinish) then FOnFinish(Self);
    Exit;
  end;
  aSprite := SpriteObject.Sprite;
//  aSprite.Alpha := 0;
  aSprite.Angle := 0;
  aSprite.H     := 0;
  aSprite.W     := 0;
  aSprite.Layer := 0;
  aSprite.X     := 0;
  aSprite.Y     := 0;
  MoveTransforms;
end;

{ TMovedImages }

procedure TMovedImages.SetCount(AValue: Integer);
begin
  if FCount = AValue then Exit;
  if AValue < 1 then AValue := 1;
  if AValue > MaxImages then AValue := MaxImages;
  FCount := AValue;
end;

procedure TMovedImages.DestroyLife(aIndex: Integer);
begin
  FLifes[aIndex].Free;
  FLifes.Delete(aIndex);
end;

procedure TMovedImages.AddLife;
var
  aLife: TLife;
begin
  aLife := TLife.Create;
  FLifes.Add(aLife);
  aLife.OnFinish := @ItemFinished;
end;

procedure TMovedImages.ItemFinished(Sender: TObject);
begin
  if not (Sender is TLife) then Exit;
  DestroyLife(FLifes.IndexOf(TLife(Sender)));
end;

constructor TMovedImages.Create;
begin
  FLifes := TLifes.Create;
  FCount := 0;
end;

destructor TMovedImages.Destroy;
begin
  while FLifes.Count > 0 do DestroyLife(FLifes.Count - 1);
  FLifes.Free;
  inherited Destroy;
end;

procedure TMovedImages.Clear;
var
  i: LongInt;
begin
  for i := FLifes.Count - 1 downto 0 do FLifes[i].Die;
  MaxImages := 10;
end;

procedure TMovedImages.Invalidate;
var
  i: Integer;
begin
//  while FCount < FLifes.Count do DestroyLife(FLifes.Count - 1);  // is done by OnFinish
  if Images.Count > 0 then
    while FCount > FLifes.Count do AddLife;
  for i := FLifes.Count - 1 downto 0 do FLifes[i].MakeMove;
end;

end.

