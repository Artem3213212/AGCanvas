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
    procedure TestDrawer(Core:TAGGraphicCore);
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

procedure TForm1.TestDrawer(Core:TAGGraphicCore);
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
  vectrA1:TAGScreenVector=(X:5;Y:55);
  vectrA2:TAGScreenVector=(X:55;Y:55);
  vectrA3:TAGScreenVector=(X:55;Y:5);
  vectrB1:TAGScreenVector=(X:5;Y:55);
  vectrB2:TAGScreenVector=(X:55;Y:55);
  vectrB3:TAGScreenVector=(X:55;Y:5);
begin
  Core.DrawPoint(vec0,20,WiteColor);
  Core.DrawPoint(vec1,20,WiteColor);
  Core.DrawPoint(vec2,20,WiteColor);
  Core.DrawPoint(vec3,20,WiteColor);
  Core.DrawText(s,pos0,16,0,WiteColor);
  Core.DrawRectangle(TAGscreenCoord.Create(1000,100,400,400),20,WiteColor);
  Core.DrawLine(vec1,vec2,20,WiteColor);
  Core.DrawElips(vecc,vecc0,20,GreenColor);
  Core.DrawRectangle(pos2,2,WiteColor);
  Core.FillRectangle(pos1,WiteColor);
  Core.FillElips(vecc+vec2,vecc0,GreenColor);
  Core.DrawBitmap(pos2,pic,255,True);
  Core.FillTriangle(vectrB1,vectrB2,vectrB3,RedColor);  
  Core.DrawTriangle(vectrA1,vectrA2,vectrA3,3,GreenColor);
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  FTestCanvas:= TAGCanvas.Create(Self);
  FTestCanvas.Parent:=Self;
  FTestCanvas.Align:=alClient;
  FTestCanvas.OnInited:=CanvasInit;
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
  FTestCanvas.Core.LoadFont('Lucida Console','EN-en',24,AGFont_SystemFont,[TAGFontStyle.tsStyleItalic]);
end;

end.

