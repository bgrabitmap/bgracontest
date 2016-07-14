PROGRAM voxel;
(*<Write here a brief description of the program. *)
(* Write here a briefing of the LICENSE. *)

  USES
    VoxApplication;

{$R *.res}

BEGIN
  TheDemo := TVoxDemo.Create;
  TheDemo.Initialize;
  TheDemo.Run
END.
