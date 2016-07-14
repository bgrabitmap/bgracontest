unit UGame;

{$mode objfpc}{$H+}

interface

uses
  Classes, sysutils, BGRABitmap, BGRABitmapTypes, BGRAOpenGL,
  Controls, URope, UClouds, ULines, UText, UStars;

const
  FrameDurationMs = 15;

type
  TSequence = (seqInitialBlack,seqFadeInText,seqFadeOutText,seqLoadingClouds,
               seqFadeInClouds,seqClouds,seqCloudsAndRope,seqRopeFalling,seqFadeOutClouds,
               seqLoadingLines,seqFadeInText2,seqFadeOutText2,
               seqFadeInLines,seqLines,seqLinesOrdering,seqFadeOutLines,
               seqFadeInText3,seqFadeOutText3,
               seqFadeInStars,seqStars,seqStarsWithEarth,seqFadeOutStars,
               seqFadeInText4,seqFadeOutText4);

const
  SequenceLen : array[TSequence] of single =
                (200,3000,4000,200,
                1000,5000,12000,5000,4000,
                15,3000,4000,
                3000,4000,12000,3000,
                3000,4000,
                4000,16000,11000,2000,
                3000,4000);

type
  { TGameContext }

  TGameContext = class
    DataLoaded: boolean;
    LeftKey,RightKey,UpKey: boolean;
    elapsedMs, sequenceMs: single;
    sequence: TSequence;
    rope: TRope;
    lines: TLines;
    clouds: TClouds;
    ended: boolean;
    stars: TStars;
    text: TText;
    earthBmp: TBGRABitmap;
    constructor Create;
    destructor Destroy; override;
    procedure Render(AWidth,AHeight: integer);
    procedure LoadData(AWidth,AHeight: Integer);
    procedure Elapse(ms: single);
    procedure MouseDown({%H-}Button: TMouseButton; {%H-}X,{%H-}Y: integer);
    procedure MouseMove({%H-}X,{%H-}Y: integer);
    procedure MouseUp({%H-}Button: TMouseButton);
  end;

implementation

{ TGameContext }

constructor TGameContext.Create;
begin
  Randomize;
  LeftKey := false;
  RightKey := false;
  UpKey := false;
  DataLoaded:= false;
  rope := nil;
  clouds := nil;
  lines := nil;
  stars := nil;
  sequence := seqInitialBlack;
  sequenceMs := 0;
  ended:= false;
  earthBmp := TBGRABitmap.Create('..'+PathDelim+'earth.png');
end;

destructor TGameContext.Destroy;
begin
  earthBmp.Free;
  text.Free;
  clouds.Free;
  rope.Free;
  lines.Free;
  stars.Free;
  BGLSpriteEngine.Clear;
  inherited Destroy;
end;

procedure TGameContext.Render(AWidth, AHeight: integer);
begin
  LoadData(AWidth,AHeight);

  case sequence of
  seqInitialBlack,seqLoadingClouds:
    BGLViewPort(AWidth, AHeight, BGRABlack);
  seqFadeInText, seqFadeInText2, seqFadeInText3, seqFadeInText4:
    begin
      BGLViewPort(AWidth, AHeight);
      BGLCanvas.FillQuadLinearColor(PointF(0,0),PointF(AWidth,0),PointF(AWidth,AHeight),PointF(0,AHeight),
              BGRA(100,120,130),BGRA(100,120,130),BGRABlack,BGRABlack);
      text.Render(0+(1-sqrt(sequenceMs/SequenceLen[sequence]))*AWidth/20);
    end;
  seqFadeOutText, seqFadeOutText2, seqFadeOutText3, seqFadeOutText4:
    begin
      BGLViewPort(AWidth, AHeight);
      BGLCanvas.FillQuadLinearColor(PointF(0,0),PointF(AWidth,0),PointF(AWidth,AHeight),PointF(0,AHeight),
              BGRA(100,120,130),BGRA(100,120,130),BGRABlack,BGRABlack);
      text.Render(0-sequenceMs/SequenceLen[sequence]*AWidth/20);
    end;
  seqFadeInClouds, seqClouds, seqCloudsAndRope, seqRopeFalling, seqFadeOutClouds:
    begin
      BGLViewPort(AWidth, AHeight);
      clouds.DrawBackground;
      BGLSpriteEngine.OnDraw;
      If Assigned(rope) then rope.Render;
    end;
  seqFadeInLines,seqLines,seqLinesOrdering,seqFadeOutLines:
    begin
      BGLViewPort(AWidth, AHeight, BGRABlack);
      If assigned(lines) then lines.Render;
    end;
  seqFadeInStars,seqStars,seqStarsWithEarth,seqFadeOutStars:
    begin
      BGLViewPort(AWidth, AHeight, BGRABlack);
      if Assigned(stars) then stars.Render;
    end;
  end;

  if sequence in[seqFadeInText,seqFadeInClouds,seqFadeInText2,seqFadeInText3,seqFadeInLines,seqFadeInStars,seqFadeInText4] then
    BGLCanvas.FillRect(0,0, AWidth,AHeight, BGRA(0,0,0,255-round(sequenceMs/SequenceLen[sequence]*255)))
  else
  if sequence in[seqFadeOutText,seqFadeOutClouds,seqFadeOutText2,seqFadeOutText3,seqFadeOutLines,seqFadeOutStars,seqFadeOutText4] then
    BGLCanvas.FillRect(0,0, AWidth,AHeight, BGRA(0,0,0,round(sequenceMs/SequenceLen[sequence]*255)));
end;

procedure TGameContext.LoadData(AWidth,AHeight: Integer);
begin
  if DataLoaded then exit;
  text := TText.Create(AWidth,AHeight);
  DataLoaded:= true;
end;

procedure TGameContext.Elapse(ms: single);
begin
  elapsedMs += ms;
  sequenceMs += ms;
  while elapsedMs > FrameDurationMs do
  begin
    BGLSpriteEngine.OnTimer;
    if Assigned(rope) then rope.Elapse;
    if Assigned(lines) then lines.Elapse(sequence in [seqLinesOrdering,seqFadeOutLines]);
    if Assigned(stars) then stars.Elapse(sequence = seqStarsWithEarth);
    elapsedMs -= FrameDurationMs;
  end;
  if sequenceMs > SequenceLen[sequence] then
  begin
    if sequence = high(TSequence) then
      ended := true
    else
    begin
      case sequence of
      seqRopeFalling: FreeAndNil(rope);
      seqFadeOutClouds: FreeAndNil(clouds);
      seqFadeOutStars: FreeAndNil(stars);
      end;

{      if sequence = seqInitialBlack then sequence := seqFadeInStars
       else}
      sequence := succ(sequence);
      sequenceMs := 0;

      case sequence of
      seqFadeInText: text.Write('First there was','randomness...');
      seqLoadingClouds: clouds := TClouds.Create(50);
      seqCloudsAndRope: rope := TRope.Create(BGLCanvas.Height div 50,PointF(BGLCanvas.Width/2,-30),PointF(30,-5));
      seqRopeFalling: rope.fixed := false;
      seqLoadingLines: lines := TLines.Create(45);
      seqFadeInText2: text.Write('Then from chaos','emerged stability');
      seqFadeInText3: text.Write('And the stars','came to be!');
      seqFadeInStars: stars := TStars.Create(BGLCanvas.Width,BGLCanvas.Height, earthBmp);
      seqFadeInText4: text.Write('On rare planets','life appeared...');
      end;
    end;
  end;
end;

procedure TGameContext.MouseDown(Button: TMouseButton; X, Y: integer);
begin

end;

procedure TGameContext.MouseMove(X, Y: integer);
begin

end;

procedure TGameContext.MouseUp(Button: TMouseButton);
begin

end;

end.

