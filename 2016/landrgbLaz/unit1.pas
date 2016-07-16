unit Unit1;

{$mode objfpc}{$H+} {$ASMMODE intel}

interface

uses

  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  LCLType, LResources, GraphType, LCLProc, LCLIntf,
  BGRABitmap, BGRABitmapTypes,
  windows;

type
  { TForm1 }
  TForm1 = class(TForm)
  Timer1: TTimer;
  procedure FormCreate(Sender: TObject);
  procedure FormDestroy(Sender: TObject);
  procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  procedure FormPaint(Sender: TObject);
  procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
    ScreenImage, ColMapImage: TBGRABitmap;
    PixMapImage, SkyMapImage: TBGRABitmap;
    procedure MakeScreen;
    procedure MakeSkyMap;
    procedure LoadTextures;
    procedure MakePixMap;
    procedure MakeSinTable;
    procedure PaintScreenImage;
    procedure WorldIt;
  public
    { public declarations }
  end;

var
  Form1: TForm1;
  ScreenStart: PBGRAPixel;
  ScreenWidth,ScreenHeight,ScreenNumPix: integer;
  ScrnHalfWdth,ScrnHalfHght: integer;
  //: integer;
  ColMapStart,PixMapStart,SkyMapStart,SinTabStart: PBGRAPixel; //PByteArray;
  CfadeStart,RfadeStart,GfadeStart,BfadeStart: PBGRAPixel; //PByteArray;
  mousex,mousey: integer;
  Colour,Colcyc                   : integer;
  yaw,pitch                       : integer;
  yawinc,pitchinc                 : integer;
  tyaw,tpitch                     : integer;
  yoffstep,yoffmax,yoffmin        : integer;
  xoff,yoff,zoff,yoffini          : integer;
  xoffinc,yoffinc,zoffinc         : integer;
  txoff,tyoff,tzoff               : integer;
  xsprt,ysprt,zsprt               : integer;
  xmouse,ymouse,zmouse            : integer;
  txsinc,tysinc,tzsinc            : integer;
  zdist,zdinc                     : integer;
  xstrt,ystrt,zstrt               : integer;
  xincr,yincr,zincr               : integer;
  xinc,yinc,zinc                  : integer;
  xsinc,ysinc,zsinc               : integer;
  ydiv,xrot,zrot                  : integer;
  pixcnt,lincnt                   : integer;
  reach,steps                     : integer;
  PitchStrt,SkyYaw                : integer;
  gray,red,green,blue             : Integer;
  PitchMode,Drawing               : Boolean;
implementation
{$R *.lfm}
{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  yoffini:=65536*180; //choose initial altitude
  yoff:=yoffini; //set initial altitude
  yoffmax:=65536*256*100; //set maximum altitude
  yoffmin:=65536*64; //set minimum altitude
  yoffstep:=65536*8; //set altitude change when key pressed
  PitchStrt:=-65536*100; //depends on screen height - don't change
  MakeScreen;  //set the screen buffer
  MakeSkyMap;  //load the panoramic sky picture
  LoadTextures; //load col ground into ColMapArray & htmap = HRGB
  MakePixMap;   //make set of smaller pixilated col/htmaps for distant views
  MakeSinTable; //Make array and generate sindtable
  KeyPreview := True; //respond to KeyUp/Down events when this app has focus
  Form1.OnKeyDown := nil; //set the KeyDown event handler to nil
  Form1.OnKeyUp := nil;   //set the KeyUp event handler to nil
  application.ProcessMessages;
  timer1.Enabled := true;
end;

procedure TForm1.MakeScreen;
begin

  ScreenImage:=TBGRABitmap.Create(320,240,BGRABlack);
  ScreenStart:=ScreenImage.Data;  //bottom line - ok writes upwards
  ScreenWidth:=ScreenImage.Width;{320} ScreenHeight:=ScreenImage.Height; //240
  ScreenNumPix:=ScreenWidth*ScreenHeight; //320*240 pixels
  ScrnHalfWdth:=ScreenWidth div 2;{160} ScrnHalfHght:=ScreenHeight div 2; //120
end;

procedure TForm1.MakeSkyMap;
begin
  SkyMapImage := TBGRABitmap.Create('ma2s2rgb.jpg'); //2048x256 sky new top
  SkyMapStart := SkyMapImage.Data;  //bottom line - ok writes upwards
end;
procedure TForm1.LoadTextures;
Var
  TempBGRAImg: TBGRABitmap;
  TempBGRAImgStart: PBGRAPixel;
begin
  try
  ColMapImage := TBGRABitmap.Create('maptile1.jpg'); //1024sqr tiled with road
  ColMapStart := ColMapImage.Data;  //bottom line - ok writes upwards
  TempBGRAImg := TBGRABitmap.Create('m_a_ht_g.jpg'); //1024sqr indexed (gray)
  TempBGRAImgStart := TempBGRAImg.Data;  //bottom line - ok writes upwards

  asm    //move 1024x1024 byte ht's into 4th byte ground Map array
  pushad  //save registers - push all double
  mov esi,TempBGRAImgStart  //load grayscale height source pointer.
  mov edi,ColMapStart   //load destination pointer.
  mov ecx,1024*1024  //load number of pixels
@gray_Loop:
  mov al,[esi]       //read 1st height gray as byte AGrGrGr - Tested ok
  add esi,4    //inc esi  //set source to next gray Note: now dd Greys
  mov [edi]+3,al        //write ht to ColMapImage ARGB to HRGB - Tested ok
  add edi,4          //set dest for next write
  loop @gray_Loop
  popad  //restore registers - pop all double.
  end;

  finally
    TempBGRAImg.Free;
  end;
end;

procedure TForm1.MakePixMap;
begin
  PixMapImage := TBGRABitmap.Create(512,1024,BGRABlack);
  PixMapStart := PixMapImage.Data;  //bottom line = PixMapImage.height-1
  BfadeStart := PixMapImage.Scanline[255];   //340 lines spare 339...0
  GfadeStart := PixMapImage.Scanline[223];   //255-32 = 223
  RfadeStart := PixMapImage.Scanline[191];   //223-32 = 191
  SinTabStart := PixMapImage.Scanline[159];  //191-32 = 159
  //CfadeStart := ScreenImage.Scanline[120]; //159- 4 = 155...0 spare
  //1 scanline is 512dd = 2048 bytes so 32 lines = 256x256map of bytes
  //pixdata = 6 dwordmaps = 24 bytemaps = 384 scanlines ?
  //128 scanlines spare = 128/16 = 8 bytemaps spare. i.e. scanlines 127..0 ?

  //generate averaged height/colour maps
  asm
  pushad    //save registers - push all double

  mov ebp,ColMapStart   //source strt of Colmap, pixels now HRGB
  mov edi,PixMapStart   //destination
  mov edx,512*4         //set lines =1024/2 *4
  mov ecx,7  //set number of output scaled maps from 1024 htmap
@nextmap:
  push ecx     //save map count
  mov ecx,edx  //512*4
  shr ecx,2    //div 4 for 512 double lines
@pixline:
  push ecx     //save pic height/2=linesout 512,256,128,64,32,16,8
  mov ecx,edx  //get 512 fisrt line (4hts/cols aver)
  shr ecx,2    //div 4 for 512
  mov esi,ebp  //get source
@morelines:
  movzx eax,byte ptr [esi]       //get blue1
  movzx ebx,byte ptr [esi]+4     //get blue2
  add eax,ebx		       //add blue1+2
  mov blue,eax                   //save blue1+2
  movzx eax,byte ptr [esi]+1     //get green1
  movzx ebx,byte ptr [esi]+5     //get green2
  add eax,ebx                    //add green1+2
  mov green,eax                  //save green1+2
  movzx eax,byte ptr [esi]+2     //get red1
  movzx ebx,byte ptr [esi]+6     //get red2
  add eax,ebx		       //add red1+2
  mov red,eax                    //save red1+2
  movzx eax,byte ptr [esi]+3     //get gray1
  movzx ebx,byte ptr [esi]+7     //get gray2
  add eax,ebx		       //add gray1+2
  mov gray,eax                   //save gray1+2
  add esi,edx                    //add 512*4 source...
  add esi,edx		       //...line width=1024*4
  movzx eax,byte ptr [esi]       //get blue3
  movzx ebx,byte ptr [esi]+4     //get blue4
  add eax,ebx		       //add blue3+4
  add blue,eax                   //save blue1+2+3+4
  movzx eax,byte ptr [esi]+1     //get green3
  movzx ebx,byte ptr [esi]+5     //get green4
  add eax,ebx                    //add green3+4
  add green,eax                  //save green1+2+3+4
  movzx eax,byte ptr [esi]+2     //get red3
  movzx ebx,byte ptr [esi]+6     //get red4
  add eax,ebx		       //add red3+4
  add red,eax                    //save red1+2+3+4
  movzx eax,byte ptr [esi]+3     //get gray3
  movzx ebx,byte ptr [esi]+7     //get gray4
  add eax,ebx		       //add gray3+4
  add eax,gray                   //add gray1+2+3+4
  shr eax,2		       //div gray by 4=Average 4x4 Height  eax=000H
  shl eax,8                   //shift gray to rebuild ColHt pixel eax=00H0
  mov ebx,red                 //get red
  shr ebx,2		    //div red by 4=Average 4x4 Red  ebx=000R
  mov al,bl                   //move ebx low byte to eax low byte eax=00HR
  shl eax,8                   //shift to rebuild ColHt pixel eax=0HR0
  mov ebx,green               //get green
  shr ebx,2		    //div green by 4=Average 4x4 Green  ebx=000G
  mov al,bl                   //move ebx low byte to eax low byte eax=0HRG
  shl eax,8                   //shift to rebuild ColHt pixel eax=HRG0
  mov ebx,blue                //get blue
  shr ebx,2		    //div blue by 4=Average 4x4 Blue  ebx=000B
  mov al,bl                   //move ebx low byte to eax low byte eax=HRGB
  stosd			    //store averaged HRGB's consecutively
  sub esi,edx		    //sub 512*4 source...
  sub esi,edx		    //...line width=1024*4
  add esi,8		    //jmp 2*HRGB for next
  dec ecx
  jne @morelines 	//first line done?
  add ebp,edx    //for next 2 source lines
  add ebp,edx    //512*4
  add ebp,edx
  add ebp,edx    //..=linewidth*2=2048*4
  pop ecx
  dec ecx
  jne @pixline
  cmp edx,512*4
  jne @moremaps
  mov ebp,PixMapStart    //source strt of pixheight map
@moremaps:
  shr edx,1      //next map size 256..128..etc.
  pop ecx
  dec ecx
  jne @nextmap
  popad  //restore registers - pop all double.
  end;
end;

procedure TForm1.MakeSinTable;
var
Sinvalue: Variant;  //variant allows J:=Sinvalue trucating decimal part
Increment: Double;      //SinTabImage 2048x1 now 4 lines in PixMap
I,J: Integer;     //SinTabStart is PixMapImage.Scanline[159]
Rfade,Gfade,Bfade: Integer;
begin   // now generate sindtable and fade tables
  Increment:= Pi/1024; //2048 incs for full circle
    for I := 0 to 2047 do
      begin
        Sinvalue := SIN(I * Increment) * 65536;
        J := Sinvalue;
        asm    //SinTabStart is bytepointer so have to use asm.
        pushad //save registers
        mov edi,SinTabStart //dest start address
        mov ecx,I  //count
        mov eax,J  //sin value
        shl ecx,2  //times 4 for 32bit store
        add edi,ecx //add offset into array
        mov [edi],eax  //store sin value
        popad  //restore registers
        end;   //SindTab = 0..65536..0..-65536..0
      end;     //  ...as 2048 32bit integers
   {Next I (for clarity)}

      //generate fade maps 256x256 i.e.pixmap (4byte) 512x512 + 256x256 + 256x256
      //pixmap total = (4x) 8 x 256x256  - 7&8 are free for 32 fade maps.
      //fade x axis is distance (0..255) - y axis is input colour
      //fade value returned is (max fade colour - input colour)/256*distance.
      //so fade value*256 = (max fade colour - input colour)*distance.

      for I := 0 to 255 do  //distance
        for J := 0 to 255 do  //input colour
           begin
              Increment:= 252;   //185; set max blue fade colour
              Sinvalue := (Increment-J)/256; //252-0..255 = 252..0..-3 /256
              Sinvalue := Sinvalue*I;  //times distance 0..255
              Sinvalue := Sinvalue+J;  //plus input colour = final colour
              Bfade := Sinvalue;
              Increment:=182;   //170; set max green fade colour
              Sinvalue := (Increment-J)/256; //182-0..255 = 182..0..-73 /256
              Sinvalue := Sinvalue*I;  //times distance 0..255
              Sinvalue := Sinvalue+J;  //plus input colour = final colour
              Gfade := Sinvalue;
              Increment:=168;   //set max red fade colour
              Sinvalue := (Increment-J)/256; //168-0..255 = 168..0..-87 /256
              Sinvalue := Sinvalue*I;  //times distance 0..255
              Sinvalue := Sinvalue+J;  //plus input colour = final colour
              Rfade := Sinvalue;

              asm
              pushad //save registers
              mov ebx,I  //bl=distance pointer 0..255
              mov ecx,J  //input colour pointer 0..255
              mov bh,cl  //bh=input colour pointer 0..255
              mov edi,BfadeStart //start of blue fade map
              mov eax,Bfade      //get fade value only use al
              mov [edi][ebx],al  //store fade value
              mov edi,GfadeStart //start of green fade map
              mov eax,Gfade      //get fade value only use al
              mov [edi][ebx],al  //store fade value
              mov edi,RfadeStart //start of red fade map
              mov eax,Rfade      //get fade value only use al
              mov [edi][ebx],al  //store fade value
              popad  //restore registers
              end;
           end;
      {Next J
      Next I (for clarity)}
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin   //interval = 30ms
  WorldIt;
end;

procedure TForm1.WorldIt;
begin
      if drawing = false then
         begin
         drawing := true;  //World_It

           asm
           pushad  //save registers - push all double
           mov eax,pitch //limited 100...-200  change for other screen sizes
           shl eax,16	 //pitch times 2^16
           add eax,-65536*100 //PitchOff Constant -65536*100 Note: var later..
           mov PitchStrt,eax     //set pitch start   ...for other screen sizes

      	   mov eax,yaw //get direction (azimuth or bearing)
      	   mov SkyYaw,eax //AND'd in sky routine for panoramic wraparound
      	   add SkyYaw,-768 //SkyPic width start pix, adjust to match highlight..
      	   and eax,$07ff //AND yaw for sintab wraparound.      ...on ground map
      	   mov esi,SinTabStart //source reg esi=sintab start eax=offset
           mov ebx,[esi][eax*4] //ebx=sin(yaw)(*2^16)
      	   add eax,$0200 //and sintab offset 512(90degs) for Cosine
      	   and eax,$07ff //AND again for sintab wraparound
      	   mov eax,[esi][eax*4] //eax=cos(yaw)(*2^16)
      	   //ebx=x sin(0),eax=z (cos65536) if yaw=0
           //save map x/z starts and increments
      	   mov xincr,ebx //sin(yaw)(*2^16)
      	   mov zincr,eax //cos(yaw)(*2^16)
      	   mov edi,ebx	 //ebx=x edi=x
      	   mov esi,eax	 //eax=z esi=z
      	   shl ebx,8	 //x=*256      note: these depend on screen size!
      	   shl eax,8	 //z=*256
      	   shl edi,5	 //x*32
      	   shl esi,5	 //z*32
      	   add ebx,esi	 //ebx=x+z*32
      	   sub eax,edi	 //eax=z-x*32
      	   shl edi,2	 //x*128
      	   shl esi,2	 //z*128
      	   add ebx,esi	 //ebx=x+z*32+z*128=x+z*320=x start
      	   sub eax,edi	 //eax=z-x*32+x*128=z-X*320=z start
      	   mov xrot,ebx	 //save x start
      	   mov zrot,eax	 //save z start
           mov edx,ScreenWidth //320
      	   mov pixcnt,edx //number of pixels (columns) 320
      @Pixel_Loop:
           //ebx=xrot eax=zrot
      	   mov ebp,ebx	    //save signed x
      	   mov ecx,eax	    //save signed z
           mov edi,65536*8  //for xsinc note: screen size!
      	   mov esi,edi	    //for zsinc
      	   cmp ebx,0	    //xpos?
      	   jge @xpos
      	   neg ebx 	//make xpos
      	   neg edi 	//make xsinc pos if used
      @xpos:
           cmp eax,0	//zpos?
           jge @zpos
      	   neg eax 	//make zpos
      	   neg esi 	//make zsinc pos if used
      @zpos:
           cmp ebx,eax	//x<>z?
      	   jae @xlong	//jump to xlong zfraction

      	   mov zsinc,esi //zinc+/-65536*8
      	   add esi,zoff  //add map z position
      	   mov zstrt,esi //temp save
      	   mov ecx,eax	   //zpos both lead&div
      	   mov ebx,ebp	   //xcarry=xdiv
      	   mov edx,65536*256  //zy
      	   mov edi,edx	   //zydif
      	   mov esi,PitchStrt  //y pseudo pitch ini -65536*100? 64?
      	   mov ystrt,esi //y
      @nxtshft:
           shr ecx,1	   //zleaddif/2
      	   je  @endclip
      	   sar ebx,1	   //xdif/2
      	   shr edi,1	   //zydif/2
      	   sar esi,1	   //ydiff/2
      	   cmp eax,65536*8 //2    //zzsprt //4 //target
      	   je  @endclip
      	   jb  @moreclip
      	   sub eax,ecx	//zleadfar-dif/2
      	   sub ebp,ebx	//xcarryfar-dif/2
      	   sub edx,edi	//zyfar-zydif/2
      	   sub ystrt,esi	//pit-pdiff/2
      	   jmp @nxtshft
      @moreclip:
           add eax,ecx	//zleadfar+dif/2
      	   add ebp,ebx	//xcarryfar+dif/2
      	   add edx,edi	//zyfar+zydif/2
      	   add ystrt,esi	//pit+pdiff/2
      	   jmp @nxtshft

      @endclip:
           mov esi,zstrt
      	   mov xsinc,ebp
      	   add ebp,xoff
      	   jmp @gotstrt
      @xlong:
           mov xsinc,edi	//xinc+-1
      	   add edi,xoff
      	   mov xstrt,edi	//temp save
      	   mov ebp,ebx	//x both lead&div

      	   mov esi,ecx	   //zcarry=zdiv
      	   mov edx,65536*256  //zy
      	   mov edi,edx	   //zydif
      	   mov eax,PitchStrt      //pseudo pitch ini -65536*100 //64
      	   mov ystrt,eax
      @nxtshft2:
           shr ebp,1	   //xldif/2
      	   je  @endclip2
      	   sar ecx,1	   //zcarry dif/2
      	   shr edi,1	   //zydif/2
      	   sar eax,1	   //ypdiff/2
      	   cmp ebx,65536*8 //2 //zzsprt //4	 //target
      	   je  @endclip2
      	   jb  @moreclip2
      	   sub ebx,ebp	//xleadfar-dif/2
      	   sub esi,ecx	//zcarryfar-dif/2
      	   sub edx,edi	//zyfar-zydif/2
      	   sub ystrt,eax	//ypit-pdiff/2
      	   jmp @nxtshft2
      @moreclip2:
           add ebx,ebp	//leadfar+dif/2
      	   add esi,ecx	//zcarryfar+dif/2
      	   add edx,edi	//zyfar+zydif/2
      	   add ystrt,eax	//ypit+pdiff/2
      	   jmp @nxtshft2
      @endclip2:
           mov ebp,xstrt
      	   mov zsinc,esi
      	   add esi,zoff
      @gotstrt:
           mov zdist,edx	//save zy       about65536*4
      	   mov zdinc,edx
      	   mov edx,ystrt	//about 65536*1
      	   mov ysinc,edx
      	   add edx,yoff	//yoff+ht
      	   sar xsinc,5 // cl // 2 //
      	   sar zsinc,5 // cl // 2 //
      	   sar ysinc,5 // cl // 2 //
      	   sar zdinc,5 // cl // 2 //
      	   mov edi,pixcnt //Pixel offset 320 to 1
           dec edi        //319 to 0
           shl edi,2            //time four for doublewords
           add edi,ScreenStart  //Destination

           mov ecx,ScreenHeight
      	   mov lincnt,ecx //no.of screen lines currently 240
      	   mov reach,96  //256 //no.of ray casts at this scale
      	   mov ydiv,0    //zdist cmp
      @Line_Loop: //start ray casting from botton of line upwards
      	   mov eax,esi	   //zstrt
      	   sar eax,7       //div zstart, eax=z on colmap (tiled ground image)
      	   mov ebx,ebp	   //xstrt
      	   sar ebx,17      //div xstrt, ebx=x on colmap (tiled ground image)
      	   and eax,$0ffc00   //upper 1023, AND eax for colmap z wraparound
      	   and ebx,$03ff     //lower 1023, AND ebx for colmap x wraparound
      	   add eax,ebx       //source on map times one
           shl eax,2         //source on map times four for doublewords
      	   add eax,ColMapStart         //Colour map start 1024x1024
           mov ebx,[eax]               //get ht/colour HRGB
           mov eax,ebx                 //copy for colour
           shr ebx,24                  //shift ht to bl
           and eax,$00ffffff           //AND out top byte for 0RGB
      	   shl ebx,14        //alpha ht (bl) becomes ht * 16384 (64*256)
      @htcom:
           cmp edx,ebx  //edx=yoff+htupline  ebx=alpha ht*64*256
      	   jg @nxdis	//ystrt>ht...
      @nxht:
           stosd        //...no, so store colour & rise up line edi=edi+4...
      	   add edi,319*4  //... so next line up on screen edi=319*4
      	   dec lincnt     //is it the top line?...
      	   je @endline    //...yes go see if more pixcnt (screen x axis)

      	   mov ecx,zdist
      	   shr ecx,8
      	   add edx,ecx
      	   mov ecx,zdinc
      	   shr ecx,8
      	   add ysinc,ecx
      	   cmp edx,ebx   //edx>ht
      	   jle @nxht
      @nxdis:
           dec reach
      	   je @nextpage0

      	   inc ydiv
      	   cmp ydiv,32    //63
      	   jb @nxdinc
      	   mov ydiv,0
      	   shl xsinc,1
      	   shl zsinc,1
      	   shl ysinc,1
      	   shl zdinc,1
      @nxdinc:
           add ebp,xsinc
      	   add esi,zsinc
      	   add edx,ysinc
      	   mov eax,zdinc
      	   add zdist,eax
      	   jmp @Line_Loop
      @nextpage0:
           shl xsinc,1
      	   shl zsinc,1
      	   shl ysinc,1
      	   shl zdinc,1
      	   mov reach,224
      @Line_Loop0: //continue ray casting up line with world incs doubled
           //1023=3ffh    x  00000000 00000000 : 00000011 11111111 b
      	   //^10=0ffc00h  z  00000000 00001111 : 11111100 00000000 b
      	   mov eax,esi //zstrt
      	   sar eax,7   //div z
      	   mov ebx,ebp //xstrt
      	   sar ebx,17   //div x
      	   and eax,$0ffc00 //upper 1023
      	   and ebx,$03ff   //lower 1023
           add eax,ebx
           shl eax,2       //source on map times four for doublewords
      	   add eax,ColMapStart         //Colour map start
           mov eax,[eax]               //get ht/colour HRGB

           mov ecx,zdist //get distance for fade value
           shr ecx,21    //scale to fade table
           and ecx,$000000ff      //cl= x axis = distance
           mov ebx,BfadeStart  //start of blue fade map
           mov ch,al    //y axis is input colour blue eax=HRGB
           mov al,[ebx][ecx] //get faded blu colour =HRGBf
           mov ebx,GfadeStart  //start of green fade map
           mov ch,ah    //y axis is input colour green.
           mov ah,[ebx][ecx] //get faded green colour HRGfBf
           bswap eax      //reverse HRGfBf byte order eax=BfGfRH
           mov ebx,RfadeStart  //start of red fade map
           mov ch,ah    //y axis is input colour red eax=BfGfRH
           mov ah,[ebx][ecx] //get faded red colour eax=BfGfRfH
           movzx ebx,al      //extract ht before reverse byte
           bswap eax         //reverse byte order back to HRfGfBf
           and eax,$00ffffff //AND out top byte for 0RGB
      	   shl ebx,14        // ebx = ht*16384 (256*64)
      @htcom0:
           cmp edx,ebx
      	   jg @nxdis0	//ystrt>ht
      @nxht0:
           stosd          //edi=edi+4 after store doubleword...
      	   add edi,319*4  //..so 319*4=1276+4=1280=320*4=next line,,,
      	   dec lincnt     //...replace with variable later for other scrn sizes
      	   je @endline

      	   mov ecx,zdist
      	   shr ecx,8
      	   add edx,ecx
      	   mov ecx,zdinc
      	   shr ecx,8
      	   add ysinc,ecx

      	   cmp edx,ebx   //edx<>ht?
      	   jle @nxht0	 //edx>ht
      @nxdis0:
           dec reach     //no.of raycasts at this scale done?
      	   je @nextpage1

      	   add ebp,xsinc  //add map incs
      	   add esi,zsinc
      	   add edx,ysinc
      	   mov eax,zdinc
      	   add zdist,eax
      	   jmp @Line_Loop0 //reach further at this scale
      @nextpage1:
           shl xsinc,1   //double world incs
      	   shl zsinc,1
      	   shl ysinc,1
      	   shl zdinc,1

      	   mov reach,128   //no.of raycasts at this scale
      @Line_Loop1:   //continue ray casting up line with world incs doubled..
           mov eax,esi	   //zstrt          ..but reading pixillated map512x512
      	   sar eax,9       //div z PixMap only 512 wide
      	   mov ebx,ebp	   //xstrt
      	   sar ebx,18      //div x
      	   and eax,$03fe00 //upper 511
      	   and ebx,$01ff   //lower 511
      	   add eax,ebx
           shl eax,2        //source on map, times four for doublewords
      	   add eax,PixMapStart   //PixCol/ht map start
           mov eax,[eax]         //get ht/colour HRGB

           mov ecx,zdist //get distance for fade value
           shr ecx,21    //scale to fade table
           and ecx,$0ff      //cl= x axis = distance
           mov ebx,BfadeStart  //start of blue fade map
           mov ch,al    //y axis is colour.
           mov al,[ebx][ecx] //get faded colour
           mov ebx,GfadeStart  //start of green fade map
           mov ch,ah    //y axis is colour.
           mov ah,[ebx][ecx] //get faded colour
           bswap eax      //reverse HRGB byte order to BGRH
           mov ebx,RfadeStart  //start of red fade map
           mov ch,ah    //y axis is colour.
           mov ah,[ebx][ecx] //get faded colour
           movzx ebx,al      //extract ht before reverse byte
           bswap eax         //reverse byte order back to HRGB
           and eax,$00ffffff     //AND out top byte for 0RGB
      	   shl ebx,14        // ebx = ht*16384 (256*64)
      @htcom1:
      		cmp edx,ebx
      		jg @nxdis1	 //ystrt>ht
      @nxht1:
      		stosd
      		add edi,319*4
      		dec lincnt
      		je @endline

      		mov ecx,zdist
      		shr ecx,8
      		add edx,ecx
      		mov ecx,zdinc
      		shr ecx,8
      		add ysinc,ecx

      		cmp edx,ebx
      		jle @nxht1	      //edx>ht
      @nxdis1:
      		dec reach
      		je @nextpage2

      		add ebp,xsinc
      		add esi,zsinc
      		add edx,ysinc
      		mov eax,zdinc
      		add zdist,eax
      		jmp @Line_Loop1

      @nextpage2:
      		shl xsinc,1
      		shl zsinc,1
      		shl ysinc,1
      		shl zdinc,1

      		mov reach,128 //96 //128 //eax
      @Line_Loop2: //continue ray casting up line with world incs doubled...
      		mov eax,esi //zstrt       ..but reading pixillated map512x512
      		sar eax,11  //div z
      		mov ebx,ebp //xstrt
      		sar ebx,19  //div x
      		and eax,$0ff00	  //upper 255(*256)
      		and ebx,$000ff	  //lower 255
      		add eax,ebx
                add eax,512*512  //jump over map512x512 to nextpixmap 265x256
                shl eax,2         //source on map times four for doublewords
      		add eax,PixMapStart         //PixCol/ht map start
                mov eax,[eax]               //get ht/colour HRGB

                mov ecx,zdist
                shr ecx,21
                and ecx,$0ff      //cl= x axis = distance
                mov ebx,BfadeStart  //start of blue fade map
                mov ch,al    //y axis is colour.
                mov al,[ebx][ecx] //get faded colour
                mov ebx,GfadeStart  //start of green fade map
                mov ch,ah    //y axis is colour.
                mov ah,[ebx][ecx] //get faded colour
                bswap eax      //reverse HRGB byte order to BGRH
                mov ebx,RfadeStart  //start of red fade map
                mov ch,ah    //y axis is colour.
                mov ah,[ebx][ecx] //get faded colour
                movzx ebx,al      //extract ht before reverse byte
                bswap eax         //reverse byte order back to HRGB
                and eax,$00ffffff     //AND out top byte for 0RGB
      		shl ebx,14        // ebx = ht*16384 (256*64)
      @htcom2:
      		cmp edx,ebx
      		jg @nxdis2	 //ystrt>ht
      @nxht2:
      		stosd
      		add edi,319*4
      		dec lincnt
      		je @endline

      		mov ecx,zdist
      		shr ecx,8
      		add edx,ecx
      		mov ecx,zdinc
      		shr ecx,8
      		add ysinc,ecx

      		cmp edx,ebx
      		jle @nxht2	      //edx>ht
      @nxdis2:
      		dec reach
      		je @nextpage3

      		add ebp,xsinc
      		add esi,zsinc
      		add edx,ysinc
      		mov eax,zdinc
      		add zdist,eax
      		jmp @Line_Loop2
      @nextpage3:
      		shl xsinc,1
      		shl zsinc,1
      		shl ysinc,1
      		shl zdinc,1
      		mov reach,128 //64 //128 //eax
      @Line_Loop3:
      		mov eax,esi //zstrt
      		sar eax,13  //div z
      		mov ebx,ebp //xstrt
      		sar ebx,20  //div x
      		and eax,$03f80	 // //upper 127(*128)
      		and ebx,$0007f	  //lower 127
                add eax,ebx
                add eax,512*512+256*256  //nextpixmap
                shl eax,2         //source on map times four for doublewords
      		add eax,PixMapStart         //PixCol/ht map start
                mov eax,[eax]               //get ht/colour HRGB

                mov ecx,zdist
                shr ecx,21
                and ecx,$0ff      //cl= x axis = distance
                mov ebx,BfadeStart  //start of blue fade map
                mov ch,al    //y axis is colour.
                mov al,[ebx][ecx] //get faded colour
                mov ebx,GfadeStart  //start of green fade map
                mov ch,ah    //y axis is colour.
                mov ah,[ebx][ecx] //get faded colour
                bswap eax      //reverse HRGB byte order to BGRH
                mov ebx,RfadeStart  //start of red fade map
                mov ch,ah    //y axis is colour.
                mov ah,[ebx][ecx] //get faded colour
                movzx ebx,al      //extract ht before reverse byte
                bswap eax         //reverse byte order back to HRGB
                and eax,$00ffffff     //AND out top byte for 0RGB
      		shl ebx,14        // ebx = ht*16384 (256*64)
      @htcom3:
      		cmp edx,ebx
      		jg @nxdis3	 //ystrt>ht
      @nxht3:
      		stosd
      		add edi,319*4
      		dec lincnt
      		je @endline

      		mov ecx,zdist
      		shr ecx,8
      		add edx,ecx
      		mov ecx,zdinc
      		shr ecx,8
      		add ysinc,ecx

      		cmp edx,ebx
      		jle @nxht3	      //edx>ht
      @nxdis3:
      		dec reach
      		je  @nextpage4

      		add ebp,xsinc
      		add esi,zsinc
      		add edx,ysinc
      		mov eax,zdinc
      		add zdist,eax
      		jmp @Line_Loop3

      @nextpage4:
      		shl xsinc,1
      		shl zsinc,1
      		shl ysinc,1
      		shl zdinc,1
      		mov reach,128 //64 //32 //64 //128 //eax
      @Line_Loop4:
      		mov eax,esi	  //zstrt
      		sar eax,15        //13 //cl    //div z
      		mov ebx,ebp	  //xstrt
      		sar ebx,21        //19 //cl    //div x
      		and eax,$00fc0	  //upper 63 ?*64
      		and ebx,$0003f	  //lower 63
                add eax,ebx
                add eax,512*512+256*256+128*128  //nextpixmap
                shl eax,2         //source on map times four for doublewords
      		add eax,PixMapStart         //PixCol/ht map start
                mov eax,[eax]               //get ht/colour HRGB

                mov ecx,zdist
                shr ecx,21
                cmp ecx,256
                jae @skyit //raycast limit so read sky for rest of line
                and ecx,$0ff      //cl= x axis = distance
                mov ebx,BfadeStart  //start of blue fade map
                mov ch,al    //y axis is colour.
                mov al,[ebx][ecx] //get faded colour
                mov ebx,GfadeStart  //start of green fade map
                mov ch,ah    //y axis is colour.
                mov ah,[ebx][ecx] //get faded colour
                bswap eax      //reverse HRGB byte order to BGRH
                mov ebx,RfadeStart  //start of red fade map
                mov ch,ah    //y axis is colour.
                mov ah,[ebx][ecx] //get faded colour
                movzx ebx,al      //extract ht before reverse byte
                bswap eax         //reverse byte order back to HRGB
                and eax,$00ffffff     //AND out top byte for 0RGB
      		shl ebx,14        // ebx = ht*16384 (256*64)
      @htcom4:
      		cmp edx,ebx
      		jg @nxdis4	 //ystrt>ht
      @nxht4:
      		stosd
      		add edi,319*4
      		dec lincnt
      		je @endline

      		mov ecx,zdist
      		shr ecx,8
      		add edx,ecx
      		mov ecx,zdinc
      		shr ecx,8
      		add ysinc,ecx

      		cmp edx,ebx
      		jle @nxht4	      //edx>ht
      @nxdis4:
      		dec reach
      		je @nextpage5

      		add ebp,xsinc
      		add esi,zsinc
      		add edx,ysinc
      		mov eax,zdinc
      		add zdist,eax
      		jmp @Line_Loop4
      @nextpage5:
      		shl xsinc,1
      		shl zsinc,1
      		shl ysinc,1
      		shl zdinc,1
      		mov reach,128 //64 //32 //64 //128 //eax
      @Line_Loop5:
      		mov eax,esi	  //zstrt
      		sar eax,17 //13 //cl    //div z
      		mov ebx,ebp	  //xstrt
      		sar ebx,22 //19 //cl    //div x
      		and eax,$03e0	 //upper 31(*32)
      		and ebx,$01f	 //lower 31
                add eax,ebx
                add eax,512*512+256*256+128*128+64*64  //nextpixmap
                shl eax,2         //source on map times four for doublewords
      		add eax,PixMapStart         //PixCol/ht map start
                mov eax,[eax]               //get ht/colour HRGB

                mov ecx,zdist
                shr ecx,21
                cmp ecx,256
                jae @skyit //raycast limit so read sky for rest of line
                and ecx,$0ff      //cl= x axis = distance
                mov ebx,BfadeStart  //start of blue fade map
                mov ch,al    //y axis is colour.
                mov al,[ebx][ecx] //get faded colour
                mov ebx,GfadeStart  //start of green fade map
                mov ch,ah    //y axis is colour.
                mov ah,[ebx][ecx] //get faded colour
                bswap eax      //reverse HRGB byte order to BGRH
                mov ebx,RfadeStart  //start of red fade map
                mov ch,ah    //y axis is colour.
                mov ah,[ebx][ecx] //get faded colour
                movzx ebx,al      //extract ht before reverse byte
                bswap eax         //reverse byte order back to HRGB
                and eax,$00ffffff     //AND out top byte for 0RGB
      		shl ebx,14       // ebx = ht*16384 (256*64)
      @htcom5:
      		cmp edx,ebx
      		jg @nxdis5	 //ystrt>ht
      @nxht5:
      		stosd
      		add edi,319*4
      		dec lincnt
      		je @endline

      		mov ecx,zdist
      		shr ecx,8
      		add edx,ecx
      		mov ecx,zdinc
      		shr ecx,8
      		add ysinc,ecx

      		cmp edx,ebx
      		jle @nxht5	      //edx>ht
      @nxdis5:
      		dec reach
      		je @skyit //raycast at this scale done, read sky for rest of line

      		add ebp,xsinc
      		add esi,zsinc
      		add edx,ysinc
      		mov eax,zdinc
      		add zdist,eax
      		jmp @Line_Loop5
      @skyit:
                //newer pansky 2048x256
      		mov esi,140	  //Line 240...1 note: var scrnht-? later
                sub esi,lincnt	  //Line 0...239 note: var scrnht-1 later
      		add esi,pitch	  //pitch +-1,2,3..
                jge @skyok
                //if below skymap then use fisrt bottom colour of sky
      		mov esi,pixcnt	  //Pixel offset 320...1
                dec esi           //Pixel offset 319...0
      		add esi,SkyYaw	  //direction offset yaw-768, aligns skymap...
      		and esi,$07ff	  //AND to pic width 2047   ..sun to shadows
                shl esi,2         //times four for doublewords
      		add esi,SkyMapStart   //add source start
                mov eax,[esi]        //get fisrt colour
      @lowsky:
      		stosd   //store fisrt bottom colour of sky until skymap start
      		add edi,319*4
      		dec lincnt
      		je @endline //or no skymap seen on this line

      		mov esi,140	  //Line 240...1 note: var later
                sub esi,lincnt	  //Line 0...239
      		add esi,pitch	  //pitch +-1,2,3..
                jl @lowsky
      @skyok:
      		shl esi,11	  //times sky width 2048
      		mov eax,pixcnt	  //Pixel offset 320...1
                dec eax           //Pixel offset 319...0
      		add eax,SkyYaw	  //direction offset yaw-768
      		and eax,$07ff	  //AND to pic width 2047
      		add esi,eax
                shl esi,2         //times four for doublewords
      		add esi,SkyMapStart   //add source start
      @moresky:
      		movsd
      		add esi,2047*4
      		add edi,319*4
      		dec lincnt
      		jne @moresky //skymap for rest of line
      @endline:
      		mov ebx,xrot	//add increments for..
      		sub ebx,zincr	//..next Pixel_Loop
      		mov xrot,ebx
      		mov eax,zrot
      		add eax,xincr
      		mov zrot,eax

      		dec pixcnt //zero=all 320 screenwidth pixels done?
      		jnz @Pixel_Loop //not zero, start at next pixel bottom line

         popad  //restore registers - pop all double.
         end;
         //ScreenImage drawing done-----------------------------------------
         ScreenImage.InvalidateBitmap; //Changed by direct access?
         ScreenImage.Draw(Canvas,0,0,True); //update ScreenImage

         //Read the keyboard - altitude=NUMPAD8 up, NUMPAD2 down
         if (GetAsyncKeyState($68))<>0 then yoff:=yoff+yoffstep; //NPD8 +65536*8
         if (GetAsyncKeyState($62))<>0 then yoff:=yoff-yoffstep; //NPD2 -65536*8
         If yoff>yoffmax then yoff:=yoffmax; //set altitude max 65536*256*100
         If yoff<yoffmin then  //set altitude minimum 65536*64
           begin           // and if at altitude minimum...
           yoff:=yoffmin;  //...and in pitch mode then...
           If PitchMode then Pitch:=0; //..zero pitch to face horizon
           end;
         //move forward or backwards on map in direction of yaw
         if (GetAsyncKeyState($26)) <> 0 then //up arrow forward thrust
           begin
           zoff := zoff+zoffinc shl 4; //zoffinc=cos(yaw)=+/-65536..0 (*16)
           xoff := xoff+xoffinc shl 4; //xoffinc=sin(yaw)=0..+/-65536 (*16)
           end;
         if (GetAsyncKeyState($28)) <> 0 then //down arrow backwards thrust
           begin
           zoff := zoff-zoffinc shl 4; //zoffinc=cos(yaw)=+/-65536..0 (*16)
           xoff := xoff-xoffinc shl 4; //xoffinc=sin(yaw)=0..+/-65536 (*16)
           end;
         if (GetAsyncKeyState($27)) <> 0 then  //right arrow = right thrust
           begin   //move mouse to left to circle a map location
           zoff := zoff-xmouse; //adds +-16(*65536) if yaw=512(90degs)
           xoff := xoff+zmouse; //adds +-16(*65536) if yaw=0
           end;
         if (GetAsyncKeyState($25)) <> 0 then   //left arrow = left thrust
           begin   //move mouse to right to circle a map location
           zoff := zoff+xmouse;   //adds +-16(*65536) if yaw=512 (90degs)
           xoff := xoff-zmouse;   //adds +-16(*65536) if yaw=0
           end;
         //Pitch NUMPAD1 Pitch up, NUMPAD7 Pitch down - was Hkey $48/Jkey $4A
         if (GetAsyncKeyState($61)) <> 0 then pitch := pitch+4; //VK_NUMPAD1
         if (GetAsyncKeyState($67)) <> 0 then pitch := pitch-4; //VK_NUMPAD7
         If Pitch>100 then Pitch:=100; //limit pitch
         If Pitch<-200 then Pitch:=-200;
         //yaw direction (bearing/azimuth) NUMPAD6 bear right, NUMPAD4 bear left
         if (GetAsyncKeyState($66)) <> 0 then yaw := yaw+8; //VK_NUMPAD6
         if (GetAsyncKeyState($64)) <> 0 then yaw := yaw-8; //VK_NUMPAD4
         //spacebar reset initial starting positions
         if (GetAsyncKeyState($20)) <> 0 then //spacebar pressed
           begin
           yoff:=yoffini; //reset to initial height 65536*180
           xoff := 0;   //reset x start position on map
           zoff := 0;   //reset z start position on map
           yaw := 0;    //reset initial direction (bearing/azimuth)
           Pitch := 0;  //reset pitch zero to face horizon
           end;
         //Zkey hold position & hide cursor until next mouse move
         if (GetAsyncKeyState($5A)) <> 0 then //Zkey pressed
           begin
           mousex:=ScrnHalfWdth; //160 overwrite saved OnMouseMoveX
           mousey:=ScrnHalfHght; //120 overwrite saved OnMouseMoveY ...scrn wid/ht later
           end;
         //Pkey pause, resume in FormKeyUp
         if (GetAsyncKeyState($50)) <> 0 then //Pkey pressed
           begin
           Form1.Cursor:=crNone;  //hide cursor until resume
           Form1.OnKeyDown := @FormKeyDown; //start handling KeyDown events
           timer1.Enabled := false; //pause WorldIt
           end;
         //update map position, altitude & pitch from mouse input, and save...
         asm   //...x&z compontents of x&z move to update x&zoff by keyboard.
         pushad  //save registers
         mov ecx,mousex //Mouse_X=0..320
         sub ecx,ScrnHalfWdth //Mouse_X=+-160
         sar ecx,3   //Mouse_X/8=+-20
         add yaw,ecx //add Yaw+-20
         mov eax,mousey	//Mouse_Y=240...0
         sub eax,ScrnHalfHght //Mouse_Y=+-120 forward=Neg
         mov ecx,eax //save Mouse_Y=+-120  for pitch
         shl eax,6   //Mouse_Y*64=+-7680 thrust
         cmp PitchMode,0 //if not pitch mode then send Mouse_Y*64 thrust..
         je @savethrust  //...for saving and using
         mov eax,yoffinc //or restore from saved
         sar ecx,4       //Mouse_Y=+-120/16=+-7.5
         sub pitch,ecx   //update pitch
         cmp pitch,100   //check upper pitch limit Note: make var later
         jle @pitchlo    //upper pitch limit ok
         mov pitch,100   //set upper pitch limit if too high Note: make var later
         jmp @yoffpitch  //pitch ok so set yoff from stored thrust eax
      @pitchlo:
         cmp pitch,-200  //check lower pitch limit Note: make var later
         jge @yoffpitch  //lower pitch limit ok so set yoff from stored thrust eax
         mov pitch,-200  //set lower pitch limit if too low Note: make var later
         jmp @yoffpitch  //lower pitch limit set so set yoff from stored thrust eax
      @savethrust:
         mov yoffinc,eax //save Mouse_Y*64=+-7680 thrust
         mov ebx,eax    //save a copy Mouse_Y*64=+-7680 thrust in ebx
         jmp @zxoffyaw
      @yoffpitch: //update new altitude from pitch if in pitch mode
         mov eax,yoffinc; //restore saved Mouse_Y*64=+-7680 thrust
         mov ebx,eax      //save a copy Mouse_Y*64=+-7680 thrust for zxoff
         mov edi,pitch //pitch ini zero (+/-4 NUMPAD1/7) 100...-200 limit
         shl edi,16    //pitch*2^16
         imul edi      //multiply edi Mouse_Y thrust*64 by eax pitch*2^16
         shrd eax,edx,15 //scale 64bit result for new altitude
         sub yoff,eax    //update new altitude
      @zxoffyaw:
         mov eax,ebx     //restore Mouse_Y*64=+-7680
         mov esi,SinTabStart //point source reg at sintable start
         mov ebp,yaw   //load yaw into ebp=sintable source offset
         and ebp,$07ff //AND offset 2047 for sintable wraparound
         mov edi,[esi][ebp*4] //get sin(yaw) sin is 0..65536..0..-65536..0
         mov xoffinc,edi //save xcomponent of forward/reverse keyboard thrust
         mov xmouse,edi  //save xcomponent of forward/reverse mouse thrust
         shl xmouse,4  //;1 ;2 ;3 //scale xcomponent thrust
         imul edi   //multiply edi sin(yaw)(*2^16) by eax Mouse_Y thrust*64
         shrd eax,edx,7 //;8;7;15 //scale 64bit result for mouse xoff update
         xchg ebx,eax //swap Mouse_Y thrust*64 into eax & mouse xoff into ebx
         add ebp,$0200 //add 512 (90degs) to sintable source offset for Cosine
         and ebp,$07ff //AND offset 2047 for sintable wraparound
         mov edi,[esi][ebp*4] //get cos(yaw) cos is 65536..0..-65536..0..65536
         mov zoffinc,edi //save zcomponent of forward/reverse for keyboard thrust
         mov zmouse,edi  //save zcomponent of forward/reverse for mouse thrust
         shl zmouse,4 //;1 ;2 ;3 //scale zcomponent thrust
         imul edi //multiply edi cos(yaw)(*2^16) by eax Mouse_Y thrust*64
         shrd eax,edx,7 //;8;7;15 //scale 64bit result for mouse zoff update
         sub xoff,ebx  //update map x offset
         sub zoff,eax  //update map z offset
         popad   //restore registers
         end;
       drawing := false;
       end;  //------------------------------------------

   application.ProcessMessages;
end;

procedure TForm1.FormPaint(Sender: TObject);
begin
  PaintScreenImage;
end;

procedure TForm1.PaintScreenImage;
begin
 ScreenImage.Draw(Canvas,0,0,True);
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
 if (Key = VK_P) then //Pkey for pause was pressed in WorldIt so...
    begin          //timers is off & cursor is hidden
    Key := 0; //so clear KeyDown's VK_P too
    Form1.OnKeyDown := nil; //stop handling KeyDown events
    Form1.OnKeyUp := @FormKeyUp; //and start handling KeyUp events
    end;
end;

procedure TForm1.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_P) then
    begin     //KeyUp's VK_P was pressed so...
    Key := 0; //...clear it but continue KeyUp event handling
    end;
  if (GetAsyncKeyState($50)) <> 0 then //wait for next Pkey $50 press
    begin
    Form1.OnKeyUp := nil; //stop KeyUp's event handling
    Form1.Cursor:=crCross; //show cursor cross
    timer1.Enabled := true; //Resume WorldIt
    end;
end;

procedure TForm1.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin                                    //if left mouse button clicked then...
  if Button = mbLeft then PitchMode := NOT PitchMode; //toggle climb & dive mode
  //if Button = mbRight then
end;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
begin
  mousex:= X;     //Read mouse x,y position
  mousey:= Y;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  pitch := pitch + WheelDelta div 30;  //change psudo pitch with mouse wheel
  If Pitch>100 then Pitch:=100;
  If Pitch<-200 then Pitch:=-200;      //limit psudo pitch
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  timer1.Enabled := false;
  ScreenImage.Free;
  SkyMapImage.Free;
  ColMapImage.Free;
  PixMapImage.Free;
end;

end.

