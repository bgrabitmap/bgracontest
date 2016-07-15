UNIT voxrend;
(*<Simple voxel renderer.  Much like "Comanche". *)

{$mode objfpc}{$H+}

INTERFACE

  USES
    Allegro5;

  TYPE
  (* Camera information.  All angles in radians. *)
    TvxrCamera = CLASS (TObject)
    PRIVATE
      fX, fY, fZ, fHeadTo, fFOV: DOUBLE;
      fWidth, fHeight, fScreenAt, fHorizon: INTEGER;
    PUBLIC
    (* Constructor. *)
      CONSTRUCTOR Create; VIRTUAL;
    (* Sets viewport and Field Of View (FOV). *)
      PROCEDURE SetViewport
        (CONST aWidth, aHeight: INTEGER; CONST aFOV: DOUBLE);

    (* Camera position. *)
      PROPERTY X: DOUBLE READ fX WRITE fX;
    (* Camera position. *)
      PROPERTY Y: DOUBLE READ fY WRITE fY;
    (* Camera position. *)
      PROPERTY Z: DOUBLE READ fZ WRITE fZ;
    (* Camera head (Y) angle. *)
      PROPERTY AngY: DOUBLE READ fHeadTo WRITE fHeadTo;
    (* Where the horizon is.  Use it to simulate X angle. *)
      PROPERTY HorizonLine: INTEGER READ fHorizon WRITE fHorizon;
    (* Field of view (FOV). *)
      PROPERTY FOV: DOUBLE READ fFOV;
    END;



  (* Stores a heightmap. *)
    TvxrHeightmap = CLASS (TObject)
    PRIVATE
      fHeightMap: ARRAY OF BYTE;
      fColorMap: ARRAY OF ALLEGRO_COLOR;
      fSkyColor: ALLEGRO_COLOR;
      fWidth, fHeight: INTEGER;

      FUNCTION GetHeight (CONST X, Z: INTEGER): BYTE; INLINE;
      PROCEDURE SetHeight (CONST X, Z: INTEGER; CONST V: BYTE); INLINE;
    PUBLIC
    (* Creates a random psychodelic landscape. *)
      PROCEDURE CreateRandom (CONST aWidth, aHeight: INTEGER);
    (* Load from files. *)
      PROCEDURE Load (CONST HeightFile, ColorFile: STRING);
    (* Sets the sky color. *)
      PROCEDURE SetSkyColor (CONST aColor: ALLEGRO_COLOR);
    (* Returns a bitmap with the colormap.  You must destroy it. *)
      FUNCTION ColorMap: ALLEGRO_BITMAPptr;
    (* Render. *)
      PROCEDURE Render (aCamera: TvxrCamera);

    (* Map width. *)
      PROPERTY Width: INTEGER READ fWidth;
    (* Map long. *)
      PROPERTY Long: INTEGER READ fHeight;
    (* Voxel height. *)
      PROPERTY Height[X, Z: INTEGER]: BYTE READ GetHeight WRITE SetHeight;
    END;

IMPLEMENTATION

  USES
    al5color, al5primitives,
    math, sysutils;

(*
 * TvxrCamera
 ***************************************************************************)

(* Constructor. *)
  CONSTRUCTOR TvxrCamera.Create;
  BEGIN
    INHERITED Create;
    SELF.SetViewport (320, 200, ALLEGRO_PI / 2)
  END;



(* Set viewport. *)
  PROCEDURE TvxrCamera.SetViewport
    (CONST aWidth, aHeight: INTEGER; CONST aFOV: DOUBLE);
  BEGIN
    fWidth := aWidth; fHeight := aHeight;
    fFOV := aFOV;
  { Distance from viewpoint to screen. }
    fScreenAt := TRUNC ((aWidth DIV 2) * tan (fFOV / 2));
  { Horizon line. }
    fHorizon := aHeight DIV 2
  END;



(*
 * TvxrHeightmap
 ***************************************************************************)

  FUNCTION TvxrHeightmap.GetHeight (CONST X, Z: INTEGER): BYTE;
  BEGIN
    RESULT := fHeightMap[(Z * fWidth) + X]
  END;



  PROCEDURE TvxrHeightmap.SetHeight (CONST X, Z: INTEGER; CONST V: BYTE);
  BEGIN
    fHeightMap[(Z * fWidth) + X] := V
  END;



(* Create random heightmap. *)
  PROCEDURE TvxrHeightmap.CreateRandom (CONST aWidth, aHeight: INTEGER);
  VAR
    Cnt: INTEGER;
    Palette: ARRAY [0..255] OF ALLEGRO_COLOR;

    PROCEDURE BlendCircle (cX, cY, R: INTEGER);
    VAR
      X, Z, rX, rZ, Tmp: INTEGER;
      D: DOUBLE;
    BEGIN
      FOR X := - R TO R DO
      BEGIN
	rX := cX + X;
	FOR Z := - R TO R DO
	BEGIN
	  rZ := cY + Z;
	  IF  (0 <= rX) AND (rX < fWidth) AND (0 <= rZ) AND (rZ < fHeight) THEN
	  BEGIN
	    D := sqrt (sqr (X) + sqr (Z)) / R;
	    IF D <= 1 THEN
	    BEGIN
	      Tmp := TRUNC (R * (1 - D));
	      IF GetHeight (rX, rZ) = 0 THEN
		SetHeight (rX, rZ, Tmp)
	      ELSE IF GetHeight (rX, rZ) + Tmp < 255 THEN
		SetHeight (rX, rZ, BYTE (GetHeight (rX, rZ) + Tmp))
	      ELSE
		SetHeight (rX, rZ, 255)
	    END
	  END
	END
      END;
      FOR X := LOW (fColorMap) TO HIGH (fColorMap) DO
	fColorMap[X] := Palette[fHeightMap[X]]
    END;

  BEGIN
  { Create a rainbow palette. }
    FOR Cnt := LOW (Palette) TO HIGH (Palette) DO
      Palette[Cnt] := al_color_hsv ((Cnt / 255) * 360, 1, 1);
  { Create heightmap. }
    SetLength (fHeightMap, aWidth * aHeight);
    SetLength (fColorMap, aWidth * aHeight);
    fWidth := aWidth; fHeight := aHeight;
  { Create domes. }
    FOR Cnt := 1 TO 50 DO
      BlendCircle (Random (1024), Random (1024), Random (128) + 32)
  END;



(* Load from files. *)
  PROCEDURE TvxrHeightmap.Load (CONST HeightFile, ColorFile: STRING);
  VAR
    Ground, Texture: ALLEGRO_BITMAPptr;
    x, z, v: INTEGER;
    r, g, b: BYTE;
  BEGIN
    Ground := al_load_bitmap_flags (HeightFile, ALLEGRO_NO_PREMULTIPLIED_ALPHA);
    Texture := al_load_bitmap_flags (ColorFile, ALLEGRO_NO_PREMULTIPLIED_ALPHA);
    TRY
      IF (Ground = NIL) OR (Texture = NIL) THEN
	RAISE Exception.Create ('Couldn''t load files.');
      IF (al_get_bitmap_width (Ground) <> al_get_bitmap_width (Texture))
      OR (al_get_bitmap_width (Ground) <> al_get_bitmap_width (Texture)) THEN
	RAISE Exception.Create ('Heightmap and texture should be same size!');
    { Get data. }
      fWidth := al_get_bitmap_width (Ground);
      fHeight := al_get_bitmap_height (Ground);
      SetLength (fHeightMap, fWidth * fHeight);
      SetLength (fColorMap, fWidth * fHeight);
      X := 0; Z := 0;
      FOR V := LOW (fHeightMap) TO HIGH (fHeightMap) DO
      BEGIN
	al_unmap_rgb (al_get_pixel (Ground, x, z), r, g, b);
	fHeightMap[v] := (r + g + b) DIV 3;
	fColorMap[v] := al_get_pixel (texture, x, z);
	INC (X);
	IF X >= fWidth THEN
	BEGIN
	  INC (Z);
	  X := 0
	END
      END
    FINALLY
      IF Ground <> NIL THEN al_destroy_bitmap (Ground);
      IF Texture <> NIL THEN al_destroy_bitmap (Texture)
    END
  END;



(* Sets the sky color. *)
  PROCEDURE TvxrHeightmap.SetSkyColor (CONST aColor: ALLEGRO_COLOR);
  BEGIN
    fSkyColor := aColor
  END;



(* Get bitmap from heightmap. *)
  FUNCTION TvxrHeightmap.ColorMap: ALLEGRO_BITMAPptr;
  VAR
    OldTarget: ALLEGRO_BITMAPptr;
    x, z, v: INTEGER;
  BEGIN
    RESULT := al_create_bitmap (fWidth, fHeight);
    IF RESULT <> NIL THEN
    BEGIN
      OldTarget := al_get_target_bitmap;
      X := 0; Z := 0;
      TRY
	al_set_target_bitmap (RESULT);
	FOR v := LOW (fColorMap) TO HIGH (fColorMap) DO
	BEGIN
	  al_put_pixel (x, z, fColorMap[(Z * fWidth) + X]);
	  INC (X);
	  IF X >= fWidth THEN
	  BEGIN
	    INC (Z);
	    X := 0
	  END
	END
      FINALLY
        al_set_target_bitmap (OldTarget)
      END
    END
  END;



(* Render. *)
  PROCEDURE TvxrHeightmap.Render (aCamera: TvxrCamera);
  const ResolutionX = 250;
        GrainSizeY = 3;
        Precision = 32768;
  VAR
    Ray, MaxHeight, VoxHeight, CamY: INTEGER;
    MaxHeightWaiting: INTEGER;
    ColorWaiting: ALLEGRO_COLOR;
    AngleRay, iRay,
    iX,iZ: DOUBLE;
    vX0Main,vX0Frac,
    vZ0Main,vZ0Frac,
    vXMain,vXFrac,
    vZMain,vZFrac,
    iXMain,iXFrac,
    iZMain,iZFrac,
    vDist: NativeInt;
    MapPos: NativeInt;
    x0,x1,xStep: Single;
    Color: ALLEGRO_COLOR;
  BEGIN
    al_clear_to_color (fSkyColor);
    AngleRay := aCamera.AngY - aCamera.FOV / 2;
    CamY := TRUNC (aCamera.Y);
    iRay := aCamera.FOV / ResolutionX;
    vX0Main := floor(aCamera.X);
    vX0Frac := trunc((aCamera.X-vX0Main)*Precision);
    vZ0Main := floor(aCamera.Z);
    vZ0Frac := trunc((aCamera.Z-vZ0Main)*Precision);
    xStep := aCamera.fWidth/ ResolutionX;
    x0 := 0;
    x1 := xStep;
    FOR Ray := 1 TO ResolutionX DO
    BEGIN
      ColorWaiting := fSkyColor;
      MaxHeightWaiting := aCamera.fHeight;
      MaxHeight := aCamera.fHeight;
      vXMain := vX0Main;
      vXFrac := vX0Frac;
      vZMain := vZ0Main;
      vZFrac := vZ0Frac;
      { Ray equation. }
      iX := cos (AngleRay); iZ := sin (AngleRay);
      iXMain := floor(iX);
      iXFrac := trunc((iX-iXMain)*Precision);
      iZMain := floor(iZ);
      iZFrac := trunc((iZ-iZMain)*Precision);
      vDist := 0;
      WHILE (MaxHeight > 0)
      DO BEGIN
        { Next voxel. }
        vXMain := vXMain + iXMain; vZMain := vZMain + iZMain;
        vXFrac := vXFrac + iXFrac; vZFrac := vZFrac + iZFrac;
        if vXFrac >= Precision then
        begin
          dec(vXFrac, Precision);
          inc(vXMain);
        end;
        if vZFrac >= Precision then
        begin
          dec(vZFrac, Precision);
          inc(vZMain);
        end;
        inc(vDist);
        if NOT ((0 <= vXMain) AND (vXMain < fWidth) AND
	   (0 <= vZMain) AND (vZMain < fHeight)) then break;
        MapPos := (vZMain * fWidth) + vXMain;
        Color := fColorMap[MapPos];
      { skip voxels that are of the same color }
        repeat
          vXMain := vXMain + iXMain; vZMain := vZMain + iZMain;
          vXFrac := vXFrac + iXFrac; vZFrac := vZFrac + iZFrac;
          if vXFrac >= Precision then
          begin
            dec(vXFrac, Precision);
            inc(vXMain);
          end;
          if vZFrac >= Precision then
          begin
            dec(vZFrac, Precision);
            inc(vZMain);
          end;
          inc(vDist);
        until NOT ((0 <= vXMain) AND (vXMain < fWidth) AND
           (0 <= vZMain) AND (vZMain < fHeight)) OR
           NOT CompareMem(@fColorMap[(vZMain * fWidth) + vXMain],@Color,sizeof(ALLEGRO_COLOR));
        vXMain := vXMain - iXMain; vZMain := vZMain - iZMain;
        vXFrac := vXFrac - iXFrac; vZFrac := vZFrac - iZFrac;
        if vXFrac < 0 then
        begin
          inc(vXFrac, Precision);
          dec(vXMain);
        end;
        if vZFrac < 0 then
        begin
          inc(vZFrac, Precision);
          dec(vZMain);
        end;
        dec(vDist);
        MapPos := (vZMain * fWidth) + vXMain;

      { Get current voxel height.  Then project it to screen. }
	VoxHeight := -fHeightMap[MapPos] + CamY;
	VoxHeight := (VoxHeight * aCamera.fScreenAt) div vDist + aCamera.HorizonLine;
      { If it's over the previous voxel, render waiting one. }
	IF (VoxHeight < MaxHeight-GrainSizeY) and
          (MaxHeightWaiting < MaxHeight) THEN
	BEGIN
          al_draw_filled_rectangle (
  	    x0, MaxHeightWaiting, x1, MaxHeight,
  	    ColorWaiting );
          MaxHeight := MaxHeightWaiting;
        END;
      { Note that there is a voxel to be drawn }
        IF VoxHeight < MaxHeightWaiting then
        begin
          MaxHeightWaiting := VoxHeight;
          ColorWaiting := Color;
        end;
      END;
      if MaxHeightWaiting < MaxHeight then
      begin
        al_draw_filled_rectangle (
          x0, MaxHeightWaiting, x1, MaxHeight,
          ColorWaiting );
      end;
    { Next ray. }
      AngleRay := AngleRay + iRay;
      x0 += xStep;
      x1 += xStep;
    END
  END;

END.

