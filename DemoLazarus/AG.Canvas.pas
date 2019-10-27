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
  private
    FLastPaint: Int64;
  protected
    procedure Paint; override;
    procedure DoOnResize; override;
    procedure EraseBkg(var Msg: TLMPaint); message LM_ERASEBKGND;
    procedure CreateHandle; override;
  public
    Core: TAGGraphicCore;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Form1: TAGCanvas;

implementation

{ TAGCanvas }

constructor TAGCanvas.Create(AOwner: TComponent);
begin
  inherited;
  Core:= TAGOpenGlGraphicCore.Create;
end;

destructor TAGCanvas.Destroy;
begin
  FreeAndNil(Core);
  inherited;
end;

procedure TAGCanvas.Paint;
begin
  Core.OnPaint;
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

