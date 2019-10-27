unit AG.Canvas;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, LMessages,
  AG.Types,
  AG.Graphic,
  AG.Graphic.OpenGl;

type

  { TAGCanvas }

  TAGCanvas = class(TCustomControl)
  private const MaxRate = 1000 div 60;
  private
    FLastPaint: Int64;
    FLimitFPS: boolean;
  protected
    procedure Paint; override;
    procedure DoOnResize; override;
    procedure EraseBkg(var Msg: TLMPaint); message LM_ERASEBKGND;
    procedure CreateHandle; override;
  public
    Core: TAGGraphicCore;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property LimitFPS: boolean read FLimitFPS write FLimitFPS;
  end;

var
  Form1: TAGCanvas;

implementation

{ TAGCanvas }

constructor TAGCanvas.Create(AOwner: TComponent);
begin
  inherited;
  Core:= TAGOpenGlGraphicCore.Create;
  FLimitFPS:= false;
end;

destructor TAGCanvas.Destroy;
begin
  FreeAndNil(Core);
  inherited;
end;

procedure TAGCanvas.Paint;
begin
  if FLimitFPS then
  begin
    if GetTickCount64-FLastPaint>MaxRate then
      Core.OnPaint()
    else
      Sleep(1);
    FLastPaint:= GetTickCount64;
  end
  else
    Core.OnPaint();
end;

procedure TAGCanvas.DoOnResize;
begin
  Core.Resize(Width, Height);
  Core.OnPaint();
end;

procedure TAGCanvas.EraseBkg(var Msg: TLMPaint);
begin
  Msg.Result:=0;
end;

procedure TAGCanvas.CreateHandle;
begin
  inherited;
  Core.Init(400, 300, Handle);
end;

end.

