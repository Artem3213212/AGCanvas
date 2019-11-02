unit unit1;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Interfaces,
  AG.Types,
  AG.Graphic,
  AG.Graphic.OpenGl,
  AG.Canvas;

type

  { TForm1 }

  TForm1 = class(TForm)
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CanvasInit(Sender: TObject);
  private
    FTestCanvas:TAGCanvas;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

var
  pic:TAGBitMap;
  s:string='Hello...';

procedure TestDrawer(Core:TAGGraphicCore);
const
  pos0:TAGScreenCoord=(X:300;Y:0;W:100;H:4000);
  pos1:TAGScreenCoord=(X:1000;Y:30;W:500;H:30);
  pos2:TAGScreenCoord=(X:100;Y:100;W:800;H:800);
  vec0:TAGScreenVector=(X:500;Y:500);
  vec1:TAGScreenVector=(X:100;Y:500);
  vec2:TAGScreenVector=(X:100;Y:100);
  vec3:TAGScreenVector=(X:500;Y:100);
  vecc0:TAGScreenVector=(X:50;Y:200);  
  vecc:TAGScreenVector=(X:700;Y:700);
begin
  Core.DrawPoint(vec0,20,WiteColor);
  Core.DrawPoint(vec1,20,WiteColor);
  Core.DrawPoint(vec2,20,WiteColor);
  Core.DrawPoint(vec3,20,WiteColor);
  Core.DrawText(s,pos0,20,0,WiteColor);
  Core.DrawRectangle(TAGscreenCoord.Create(1000,100,400,400),20,WiteColor);
  Core.DrawLine(vec1,vec2,20,WiteColor);
  Core.DrawElips(vecc,vecc0,20,GreenColor);
  Core.DrawRectangle(pos2,2,WiteColor);
  Core.FillRectangle(pos1,WiteColor);
  Core.FillElips(vecc+vec2,vecc0,GreenColor);
  Core.DrawBitmap(pos2,pic,255,True);
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  FTestCanvas:= TAGCanvas.Create(Self);
  FTestCanvas.Parent:=Self;
  FTestCanvas.Align:=alClient;
  FTestCanvas:=CanvasInit;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  //
end;

procedure TForm1.CanvasInit(Sender: TObject);
var
  fn: string;
begin
  FTestCanvas.Core.BackColor:= BlueColor;
  fn:= ExtractFilePath(Application.ExeName)+'test.bmp';
  pic:=FTestCanvas.Core.CreateBitMapFromFile(fn);
  FTestCanvas.Core.drawer:= TestDrawer;
end;

end.

