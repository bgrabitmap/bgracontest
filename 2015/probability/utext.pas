unit UText;

{$mode objfpc}

interface

uses
  Classes, SysUtils, BGRAOpenGL, BGRAVectorize, BGRABitmapTypes;

type

  { TText }

  TText = class
    virtualScreen: TBGLBitmap;
    textRenderer: TBGRAVectorizedFontRenderer;
    constructor Create(AWidth,AHeight: integer);
    procedure Write(AFirstLine,ASecondLine: string);
    procedure Render(x: single);
    destructor Destroy; override;
  end;

implementation

{ TText }

constructor TText.Create(AWidth,AHeight: integer);
begin
  virtualScreen := TBGLBitmap.Create(AWidth,AHeight,BGRABlack);
  textRenderer := TBGRAVectorizedFontRenderer.Create;
  virtualScreen.FontRenderer := textRenderer;
end;

procedure TText.Write(AFirstLine, ASecondLine: string);
begin
  virtualScreen.FillTransparent;
  virtualScreen.FontQuality := fqFineAntialiasing;
  virtualScreen.FontHeight := virtualScreen.Height div 12;
  virtualScreen.FontName := 'Verdana';
  textRenderer.OutlineVisible:= true;
  textRenderer.OutlineColor := BGRA(120,120,180);
  textRenderer.OutlineWidth := virtualScreen.FontHeight/8;
  textRenderer.OuterOutlineOnly := true;
  virtualScreen.TextOut(virtualScreen.Width/2,virtualScreen.Height/2-virtualScreen.FontFullHeight,AFirstLine,BGRAWhite,taCenter);
  virtualScreen.TextOut(virtualScreen.Width/2,virtualScreen.Height/2,ASecondLine,BGRAWhite,taCenter);
end;

procedure TText.Render(x: single);
var
  ofs: single;
begin
  ofs := virtualScreen.Height / 40;
  virtualScreen.Texture.Mask.Draw(x*2+ofs,ofs, BGRA(0,0,0,80));
  virtualScreen.Texture.Draw(x,0);
end;

destructor TText.Destroy;
begin
  virtualScreen.Free;
  inherited Destroy;
end;

end.

