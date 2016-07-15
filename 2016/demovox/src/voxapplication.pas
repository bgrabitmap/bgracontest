UNIT VoxApplication;
(*<Defines and manages application stuff. *)
(* Copyright (c) 2016 Guillermo Martínez J.

This software is provided 'as-is', without any express or implied
warranty. In no event will the authors be held liable for any damages
arising from the use of this software.

Permission is granted to anyone to use this software for any purpose,
including commercial applications, and to alter it and redistribute it
freely, subject to the following restrictions:

    1. The origin of this software must not be misrepresented; you must not
    claim that you wrote the original software. If you use this software
    in a product, an acknowledgment in the product documentation would be
    appreciated but is not required.

    2. Altered source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

    3. This notice may not be removed or altered from any source
    distribution.
*)

INTERFACE

  USES
    voxrend,
    Allegro5, al5font;

  CONST
  (* Window title. *)
    TITLE = 'Voxel demo';
  (* Screen configuration. *)
    WIDTH = 640; HEIGHT = 480; BPP = 32;
  (* Game speed in frames per second. *)
    FPS = 50;

  TYPE
  (* Extends camera ot make it controlable. *)
    TMyCamera = CLASS (TvxrCamera)
    PRIVATE
      fSpeed, fAccel, fRotSpeed, fStrafe: DOUBLE;
      fH, fiH: DOUBLE;
    PUBLIC
    (* Constructor. *)
      CONSTRUCTOR Create; OVERRIDE;
    (* Handles the event, if any. *)
      PROCEDURE HandleEvent (CONST aEvent: ALLEGRO_EVENT);
    (* Updates camera. *)
      PROCEDURE Update;
    (* Draws camera at minimap. *)
      PROCEDURE Draw (CONST Scale: DOUBLE);
    END;



  (* The demo class. *)
    TVoxDemo = CLASS (TObject)
    PRIVATE
      fDisplay: ALLEGRO_DISPLAYptr;
      fTimer: ALLEGRO_TIMERptr;
      fEventQueue: ALLEGRO_EVENT_QUEUEptr;
      fFont: ALLEGRO_FONTptr;
      fTerminated: BOOLEAN;

      fHeightmap: TvxrHeightmap;
      fMap: ALLEGRO_BITMAPptr;
      fCamera: TMyCamera;

      FUNCTION CreateDisplay: BOOLEAN;
    PUBLIC
    (* Destructor. *)
      DESTRUCTOR Destroy; OVERRIDE;
    (* Initializes the application. *)
      PROCEDURE Initialize;
    (* Game execution. *)
      PROCEDURE Run;
    (* Terminates the application. *)
      PROCEDURE Terminate;
    (* Show error message. *)
      PROCEDURE ShowErrorMessage (CONST Message: STRING);

    (* Is game terminated? *)
      PROPERTY Terminated: BOOLEAN READ fTerminated;
    (* Text font. *)
      PROPERTY Font: ALLEGRO_FONTptr READ fFont;
    (* The heightmap. *)
      PROPERTY Heightmap: TvxrHeightmap READ fHeightmap;
    END;

  VAR
  (* Global reference to demo. *)
    TheDemo: TVoxDemo;
  (* Some colors. *)
    Black, White: ALLEGRO_COLOR;


IMPLEMENTATION

  USES
    al5image, al5nativedlg, al5primitives,
    sysutils;

(*****************************************************************************
 * TMyCamera
 *)

(* Constructor. *)
  CONSTRUCTOR TMyCamera.Create;
  BEGIN
    INHERITED Create;
    fSpeed := 0; fAccel := 0; fRotSpeed := 0;
    fH := 16; fiH := 0
  END;



(* Handles the event, if any. *)
  PROCEDURE TMyCamera.HandleEvent (CONST aEvent: ALLEGRO_EVENT);
  CONST
    vFactor = 1;
    hFactor = 0.01;
  BEGIN
    CASE aEvent._type OF
    ALLEGRO_EVENT_KEY_DOWN:
      CASE aEvent.keyboard.keycode OF
	ALLEGRO_KEY_UP, ALLEGRO_KEY_W: fSpeed := 2;
	ALLEGRO_KEY_DOWN, ALLEGRO_KEY_S: fSpeed := -2;
	ALLEGRO_KEY_RIGHT: fRotSpeed := 0.025;
	ALLEGRO_KEY_LEFT: fRotSpeed := -0.025;
	ALLEGRO_KEY_A: fStrafe := -1;
	ALLEGRO_KEY_D: fStrafe := 1;
	ALLEGRO_KEY_LSHIFT, ALLEGRO_KEY_RSHIFT: fiH := -0.5;
	ALLEGRO_KEY_SPACE:
	  BEGIN
	    fAccel := 0; fSpeed := 0; fRotSpeed := 0; fStrafe := 0;
	    fH := 16
	  END;
      END;
    ALLEGRO_EVENT_KEY_UP:
      CASE aEvent.keyboard.keycode OF
	ALLEGRO_KEY_UP, ALLEGRO_KEY_W: fSpeed := 0;
	ALLEGRO_KEY_DOWN, ALLEGRO_KEY_S: fSpeed := 0;
	ALLEGRO_KEY_RIGHT: fRotSpeed := 0;
	ALLEGRO_KEY_LEFT: fRotSpeed := 0;
	ALLEGRO_KEY_A: fStrafe := 0;
	ALLEGRO_KEY_D: fStrafe := 0;
	ALLEGRO_KEY_LSHIFT, ALLEGRO_KEY_RSHIFT: fiH := 0.2;
      END;
    ALLEGRO_EVENT_MOUSE_AXES:
      BEGIN
	SELF.HorizonLine := SELF.HorizonLine - (aEvent.mouse.dy - vFactor);
	SELF.AngY := SELF.AngY + (aEvent.mouse.dx * hFactor)
      END;
    END
  END;



(* Updates camera. *)
  PROCEDURE TMyCamera.Update;
  BEGIN
    fSpeed := fSpeed;
    SELF.AngY := SELF.AngY + fRotSpeed;
    fH := fH + fiH;

    IF SELF.AngY > ALLEGRO_PI * 2 THEN SELF.AngY := SELF.AngY - (ALLEGRO_PI * 2);
    IF SELF.AngY < 0 THEN SELF.AngY := SELF.AngY + (ALLEGRO_PI * 2);
    IF SELF.HorizonLine < -HEIGHT THEN SELF.HorizonLine := -HEIGHT;
    IF SELF.HorizonLine > HEIGHT THEN SELF.HorizonLine := HEIGHT;
    IF fH < 1 THEN fH := 1;
    IF fH > 16 THEN fH := 16;

    SELF.X := SELF.X + (cos (SELF.AngY) * fSpeed) + (cos (SELF.AngY + (ALLEGRO_PI / 2)) * fStrafe);
    SELF.Z := SELF.Z + (sin (SELF.AngY) * fSpeed) + (sin (SELF.AngY + (ALLEGRO_PI / 2)) * fStrafe);

    IF SELF.X < 0 THEN SELF.X := 0;
    IF SELF.X >= TheDemo.Heightmap.Width THEN SELF.X := TheDemo.Heightmap.Width - 1;
    IF SELF.Z < 0 THEN SELF.Z := 0;
    IF SELF.Z >= TheDemo.Heightmap.Long THEN SELF.Z := TheDemo.Heightmap.Long - 1;

    SELF.Y := TheDemo.Heightmap.Height[TRUNC (SELF.X), TRUNC (SELF.Z)] + fH;
  END;



(* Draws at minimap. *)
  PROCEDURE TMyCamera.Draw (CONST Scale: DOUBLE);
  VAR
    FOV2: DOUBLE;
  BEGIN
    FOV2 := SELF.FOV / 2;
    al_draw_circle (
      SELF.X * Scale, SELF.Z * Scale, 2, WHITE, 1
    );
    al_draw_line (
      SELF.X * Scale, SELF.Z * Scale,
      (SELF.X * Scale) + (cos (SELF.AngY - FOV2) * 50), (SELF.Z * Scale) + (Sin (SELF.AngY - FOV2) * 50),
      WHITE, 1
    );
    al_draw_line (
      SELF.X * Scale, SELF.Z * Scale,
      (SELF.X * Scale) + (cos (SELF.AngY + FOV2) * 50), (SELF.Z * Scale) + (Sin (SELF.AngY + FOV2) * 50),
      WHITE, 1
    );
  END;



(*****************************************************************************
 * TVoxDemo
 *)

  FUNCTION Tvoxdemo.Createdisplay: Boolean;
  VAR
    DisplayFlags: INTEGER;
  BEGIN
    DisplayFlags := ALLEGRO_WINDOWED;
    al_set_new_display_flags (DisplayFlags);
    al_set_new_display_option (ALLEGRO_DEPTH_SIZE, BPP, ALLEGRO_SUGGEST);
    al_set_new_display_option (ALLEGRO_VSYNC, 2, ALLEGRO_SUGGEST);
    fDisplay := al_create_display (WIDTH, HEIGHT);
    IF fDisplay = NIL THEN
    BEGIN
      SELF.ShowErrorMessage ('Can''t create display');
      EXIT (FALSE);
    END;
    al_hide_mouse_cursor (fDisplay);
    Black := al_map_rgb (0, 0, 0);
    White := al_map_rgb (255, 255, 255);
    al_set_window_title (fDisplay, TITLE);
    al_register_event_source (fEventQueue, al_get_display_event_source (fDisplay));
    RESULT := TRUE
  END;



(* Destructor. *)
    DESTRUCTOR Tvoxdemo.Destroy;
  BEGIN
    IF fEventQueue <> NIL THEN
    BEGIN
      IF fDisplay <> NIL THEN
	al_unregister_event_source (fEventQueue, al_get_display_event_source (fDisplay));
      al_unregister_event_source (fEventQueue, al_get_timer_event_source (fTimer));
      al_destroy_event_queue (fEventQueue);
    END;
    IF fFont <> NIL THEN al_destroy_font (fFont);
    IF fDisplay <> NIL THEN al_destroy_display (fDisplay);
    FreeAndNil (fCamera);
    FreeAndNil (fHeightmap);
    INHERITED Destroy
  END;



(* Initialize. *)
  PROCEDURE Tvoxdemo.Initialize;
  BEGIN
  { Set up Allegro and add-ons. }
    IF al_init THEN
    BEGIN
    { At the moment, as bitmap management isn't very sophisticated. }
      al_set_new_bitmap_flags (ALLEGRO_MEMORY_BITMAP);

      al_install_keyboard;
      al_install_mouse;
      al_init_primitives_addon;
      al_init_image_addon;
      fTimer := al_create_timer (ALLEGRO_BPS_TO_SECS (FPS));
    { The event queue. }
      fEventQueue := al_create_event_queue;
      al_register_event_source (fEventQueue, al_get_keyboard_event_source);
      al_register_event_source (fEventQueue, al_get_mouse_event_source);
      al_register_event_source (fEventQueue, al_get_timer_event_source (fTimer));
      IF CreateDisplay THEN
      BEGIN
	Black := al_map_rgb_f (0, 0, 0);
	White := al_map_rgb_f (1, 1, 1);
      { Fonts. }
        fFont := al_create_builtin_font;
	IF fFont <> NIL THEN
	BEGIN
	{ And the heightmap. }
	  fHeightmap := TvxrHeightmap.Create;
	  IF fHeightmap <> NIL THEN
	  BEGIN
	    fCamera := TMyCamera.Create;
	    IF (ParamCount >= 2) THEN
	      fHeightmap.Load (ParamStr (1), ParamStr (2))
	    ELSE
	      fHeightmap.CreateRandom (1024, 1024);
	    fHeightmap.SetSkyColor (al_map_rgb (120, 204, 255));
	    fMap := fHeightmap.ColorMap;
	    fTerminated := FALSE;
	    EXIT
	  END
	  ELSE
	    SELF.ShowErrorMessage ('Can''t create heightmap.')
	END
	ELSE
	  SELF.ShowErrorMessage ('Can''t init font.')
      END
    END
    ELSE
      SELF.ShowErrorMessage ('Can''t initialize Allegro5!');
    fTerminated := TRUE
  END;



(* Run game. *)
  PROCEDURE Tvoxdemo.Run;

    PROCEDURE DrawText (CONST X, Y: REAL; aText: STRING);
    BEGIN
      al_draw_text (fFont, Black, X + 1, Y + 1, 0, aText);
      al_draw_text (fFont, White, X, Y, 0, aText)
    END;

  VAR
    Event: ALLEGRO_EVENT;
    //BmpTmp: ALLEGRO_BITMAPptr;
    MapFactor, MapScale: DOUBLE;
  BEGIN
    IF NOT fTerminated THEN
    TRY
    { To do zoom. }
      //BmpTmp := al_create_bitmap (WIDTH, HEIGHT);
      MapFactor := fHeightmap.Long / fHeightmap.Width;
      MapScale := 128 / fHeightmap.Width;
    { Camera. }
      fCamera.X := fHeightmap.Width / 2;
      fCamera.Z := fHeightmap.Long / 2;
      fCamera.SetViewport (WIDTH, HEIGHT, ALLEGRO_PI / 3);
    { Run }
      al_start_timer (fTimer);
      REPEAT
	al_wait_for_event (fEventQueue, Event);
	CASE Event._type OF
	ALLEGRO_EVENT_DISPLAY_CLOSE:
	  SELF.Terminate;
	ALLEGRO_EVENT_KEY_DOWN:
          CASE Event.keyboard.keycode OF
            ALLEGRO_KEY_ESCAPE: SELF.Terminate;
	    ALLEGRO_KEY_F12:
	      al_save_bitmap ('screenshot.pcx', al_get_backbuffer (fDisplay))
	    ELSE
	      fCamera.HandleEvent (Event);
          END;
	ALLEGRO_EVENT_MOUSE_AXES:
	  BEGIN
	    fCamera.HandleEvent (Event);
	    al_set_mouse_xy (TheDemo.fDisplay, WIDTH DIV 2, HEIGHT DIV 2)
	  END;
	ALLEGRO_EVENT_TIMER:
	  BEGIN
	    fCamera.Update;
	    IF al_is_event_queue_empty (fEventQueue) THEN
	    BEGIN

	      {al_set_target_bitmap (BmpTmp);
	      fHeightmap.Render (fCamera);
	      al_set_target_bitmap (al_get_backbuffer (fDisplay));
	      al_draw_bitmap (BmpTmp, 0, 0, 0);}
              al_set_target_bitmap (al_get_backbuffer (fDisplay));
              fHeightmap.Render (fCamera);

	      al_draw_scaled_bitmap (
	        fMap, 0, 0, fHeightmap.Width, fHeightmap.Long,
	        0, 0, 128, 120 * MapFactor, 0);
	      fCamera.Draw (MapScale);
	      DrawText (200, 8, Format (
		'Camera position <%f, %f, %f>', [fCamera.X, fCamera.Y, fCamera.Z]
	      ));
	      DrawText (200, 16, Format (
		'Camera angles <%d, %f, 0.0>', [fCamera.HorizonLine, fCamera.AngY]
	      ));
	      al_flip_display
	    END
	  END;
	ELSE
	  fCamera.HandleEvent (Event);
	END
      UNTIL fTerminated
    EXCEPT
      ON Error: Exception DO
      BEGIN
        SELF.ShowErrorMessage (Error.Message);
	SELF.Terminate
      END
    END;

    //al_destroy_bitmap (BmpTmp);
  END;



(* Terminate. *)
  PROCEDURE Tvoxdemo.Terminate;
  BEGIN
    fTerminated := TRUE
  END;



(* Show error message. *)
  PROCEDURE Tvoxdemo.Showerrormessage(CONST Message: String);
  BEGIN
    al_show_native_message_box (
      SELF.fDisplay, 'Error', 'An error raised:', Message,
      '', ALLEGRO_MESSAGEBOX_ERROR
    )
  END;

INITIALIZATION
  ;
FINALIZATION
  FreeAndNil (TheDemo)
END.
