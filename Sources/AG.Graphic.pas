unit AG.Graphic;

interface

{$IFDEF FPC}
  {$mode DELPHI}
{$ENDIF}

uses
  AG.Types,Classes,SysUtils;

type
  TAGGraphicCore=class;
  TAGOnpantProcedure=procedure(Core:TAGGraphicCore);

  TAGGraphicCore=class abstract
    protected
      procedure SetBackColor(color:TAGColor);virtual;abstract;
      function GetBackColor:TAGColor;virtual;abstract;
    public
      hwnd:TAGWindowHandle;
      drawer:TAGOnpantProcedure;
      property BackColor:TAGColor read GetBackColor write SetBackColor;

      procedure Init(W,H:cardinal;hWindow:TAGWindowHandle);virtual;abstract;
      procedure OnPaint();virtual;abstract;
      procedure Resize(W,H:Word);virtual;abstract;
      //2D
      function CreateBitMap(p:TStream):TAGBitMap;virtual;abstract;
      function CreateBitMapFromFile(Name:String):TAGBitMap;virtual;
      procedure LoadFont(Name,Local:string;size:single;font:TAGFont);virtual;abstract;
      procedure ReleaseBitMap(b:TAGBitMap);virtual;abstract;
      procedure DrawPoint(point:TAGScreenVector;size:word;brush:TAGColor);overload;virtual;
      procedure DrawPoint(point:TAG2DVector;size:word;brush:TAGColor);overload;virtual;abstract;
      procedure DrawRectangle(rect:TAGScreenCoord;size:word;brush:TAGColor);overload;virtual;
      procedure DrawRectangle(rect:TAGCoord;size:word;brush:TAGColor);overload;virtual;abstract;
      procedure DrawElips(point,radaii:TAGscreenVector;size:word;brush:TAGColor);overload;virtual;
      procedure DrawElips(point,radaii:TAG2DVector;size:word;brush:TAGColor);overload;virtual;abstract;
      procedure DrawLine(point0,point1:TAGScreenVector;size:word;brush:TAGColor);overload;virtual;
      procedure DrawLine(point0,point1:TAG2DVector;size:word;brush:TAGColor);overload;virtual;abstract;
      procedure DrawText(text:string;position:TAGScreenCoord;size:word;font:TAGFont;brush:TAGColor);overload;virtual;
      procedure DrawText(text:string;position:TAGCoord;size:word;font:TAGFont;brush:TAGColor);overload;virtual;abstract;
      procedure DrawBitmap(coord:TAGScreenCoord;bitmap:TAGBitMap;Opacity:byte=255;Smooth:boolean=False);overload;virtual;
      procedure DrawBitmap(coord:TAGCoord;bitmap:TAGBitMap;Opacity:byte=255;Smooth:boolean=False);overload;virtual;abstract;
      procedure FillRectangle(rect:TAGScreenCoord;brush:TAGColor);overload;virtual;
      procedure FillRectangle(rect:TAGCoord;brush:TAGColor);overload;virtual;abstract;
      procedure FillElips(point,radaii:TAGscreenVector;brush:TAGColor);overload;virtual;
      procedure FillElips(point,radaii:TAG2DVector;brush:TAGColor);overload;virtual;abstract;
  end;

implementation

function TAGGraphicCore.CreateBitMapFromFile(Name:String):TAGBitMap;
var
  Stream:TStream;
begin
  Stream:=TFileStream.Create(Name,fmOpenRead);
  Result:=CreateBitMap(Stream);
  FreeAndNil(Stream);
end;

procedure TAGGraphicCore.DrawPoint(point:TAGScreenVector;size:word;brush:TAGColor);
begin
  DrawPoint(TAG2DVector(point),size,brush);
end;

procedure TAGGraphicCore.DrawRectangle(rect:TAGScreenCoord;size:word;brush:TAGColor);
begin
  DrawRectangle(TAGCoord(rect),size,brush);
end;

procedure TAGGraphicCore.DrawElips(point,radaii:TAGScreenVector;size:word;brush:TAGColor);
begin
  DrawElips(TAG2DVector(point),radaii,size,brush);
end;

procedure TAGGraphicCore.DrawLine(point0,point1:TAGScreenVector;size:word;brush:TAGColor);
begin
  DrawLine(TAG2DVector(point0),TAG2DVector(point1),size,brush);
end;

procedure TAGGraphicCore.DrawText(text:string;position:TAGScreenCoord;size:word;font:TAGFont;brush:TAGColor);
begin
  DrawText(text,TAGCoord(position),size,font,brush);
end;

procedure TAGGraphicCore.DrawBitmap(coord:TAGScreenCoord;bitmap:TAGBitMap;Opacity:byte=255;Smooth:boolean=False);
begin
  DrawBitmap(TAGCoord(coord),bitmap,opacity,Smooth);
end;

procedure TAGGraphicCore.FillRectangle(rect:TAGScreenCoord;brush:TAGColor);
begin
  FillRectangle(TAGCoord(rect),brush);
end;

procedure TAGGraphicCore.FillElips(point,radaii:TAGScreenVector;brush:TAGColor);
begin
  FillElips(TAG2DVector(point),TAG2DVector(radaii),brush);
end;

end.
