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

unit uDefinitions;

{$mode objfpc}{$H+}

interface

uses Forms, BGRABitmapTypes, math;

const
  MaxLife = 1000;                                           // maximum age of a image
  HiMaxLife = MaxLife - 1;                                  // intern
  MaxImages: Integer = 5;                                   // intern
  MaxBitmapSize: Integer = 500;                             // maximum width / height of a loaded image (to copy a big image from thread heap to GPU memory sprite takes some time)
  MaxImageLoadThreads = 4;                                  // maximum threads loading images for sprites
  {$IFDEF Windows}
  BasePath = 'C:\';
  {$ELSE}
  BasePath = '/';
  {$ENDIF}
  PictSpace = 0.1;                                          // space from one image to the next
  ZNext: Double = 0;                                        // intern
  Lying = 3;                                              // intern

type
  TAppState = (asCreating, asRunning, asDestroying);

  TMBounds = record
    Left   : Integer;
    Top    : Integer;
    Width  : Integer;
    Height : Integer;
    WindowState: TWindowState;
  end;

var
  SinusArray:             array[0..HiMaxLife] of Double;
  HalfSinusArray:         array[0..HiMaxLife] of Double;
  FastGrowArray:          array[0..HiMaxLife] of Double;
  FastGrowAndShrinkArray: array[0..HiMaxLife] of Double;

function Normal2(w: QWord): QWord;

implementation

function Normal2(w: QWord): QWord;
var
  b: Byte;
begin
  for b := 63 downto 1 do
    if w shr b > 0 then Exit((w shr b) shl b);
  Result := w;
end;

procedure InitArrays;
var
  i: Integer;
begin
  for i := 0 to HiMaxLife do begin
    SinusArray[i]             := sin(i * Pi * 2 / HiMaxLife);
    HalfSinusArray[i]         := sin(i * Pi / HiMaxLife);
    FastGrowArray[i]          := min(i / 300, 1);
    FastGrowAndShrinkArray[i] := min(arctan(i / HiMaxLife * 8), arctan((HiMaxLife - i) / HiMaxLife * 8));
  end;
end;

initialization

  InitArrays;

end.

