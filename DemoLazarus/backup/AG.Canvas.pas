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
    FCore: TAGGraphicCore;
  protected
    function GetCore:TAGGraphicCore;
    procedure Paint; override;
    procedure DoOnResize; override;
    procedure EraseBkg(var Msg: TLMPaint); message LM_ERASEBKGND;
    procedure CreateHandle; override;
  public
    property Core:TAGGraphicCore read GetCore;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Form1: TAGCanvas;

implementation

{ TAGCanvas }
function TAGCanvas.GetCore:TAGGraphicCore;
begin
  if Core.hwnd=0 then
    raise Exception.Create('Core not loaded')
  else
    Result:=FCore;
end;

constructor TAGCanvas.Create(AOwner: TComponent);
begin
  inherited;
  FCore:=TAGOpenGlGraphicCore.Create;
end;

destructor TAGCanvas.Destroy;
begin
  FreeAndNil(FCore);
  inherited;
end;

procedure TAGCanvas.Paint;
begin
  FCore.OnPaint;
end;

procedure TAGCanvas.DoOnResize;
begin
  FCore.Resize(Width, Height);
  FCore.OnPaint();
end;

procedure TAGCanvas.EraseBkg(var Msg: TLMPaint);
begin
  Msg.Result:=0;
end;

procedure TAGCanvas.CreateHandle;
begin
  inherited;
  FCore.Init(400, 300, Handle);
end;

end.

