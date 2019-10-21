unit AG.Types;

interface

{$IFDEF FPC}
  {$mode DELPHI}
{$ENDIF}

uses
  SysUtils,Classes,GL,Math;

type
  TAGWindowHandle={$IFDEF MSWINDOWS}NativeInt{$ENDIF};

  TAGColor=packed record
    R,G,B,A:byte;
    constructor Create(R,G,B,A:Byte);
  end;

  TAGFloatColor=packed record
    R,G,B,A:Single;
    constructor Create(R,G,B,A:Single);
    class operator Implicit(c:TAGColor):TAGFloatColor;
    class operator Implicit(c:TAGFloatColor):TAGColor;
    class operator Explicit(c:TAGColor):TAGFloatColor;
    class operator Explicit(c:TAGFloatColor):TAGColor;
  end;

  TAGBitMap=packed record
    case byte of
    0:(pn:PNativeInt);
    1:(OpenGL:GlUint);
  end;
  TAGFont=word;

  TAG2DVector=packed record    
     x,y:Single;
     class function Create(x,y:Single):TAG2DVector;static;
     function Add(const apt: TAG2DVector): TAG2DVector;
     function Distance(const apt : TAG2DVector) : Single;
     function DotProduct(const apt : TAG2DVector) : Single;
     function IsZero : Boolean;
     function Subtract(const apt : TAG2DVector): TAG2DVector;

     function Scale(afactor:Single):TAG2DVector;
     function Length:Single;
     function Normalize:TAG2DVector;
     class operator =(const apt1,apt2:TAG2DVector):Boolean;
     class operator <>(const apt1,apt2:TAG2DVector):Boolean;
     class operator +(const apt1,apt2:TAG2DVector):TAG2DVector;
     class operator -(const apt1,apt2:TAG2DVector):TAG2DVector;
     class operator -(const apt1:TAG2DVector):TAG2DVector;
     class operator *(const apt1,apt2:TAG2DVector):Single;//scalar product
     class operator *(const apt1:TAG2DVector;afactor: single):TAG2DVector;
     class operator *(afactor:single;const apt1:TAG2DVector):TAG2DVector;
     class operator /(const apt1:TAG2DVector;afactor:single):TAG2DVector;
  end;
  TAGScreenVector=record
    X,Y:Integer;
    class operator Multiply(A:TAGScreenVector;B:Integer):TAGScreenVector;inline;
    class operator Multiply(A:TAGScreenVector;B:Real):TAG2DVector;inline;
    class operator Multiply(A:TAGScreenVector;B:Single):TAG2DVector;inline;
    class operator Multiply(A:Integer;B:TAGScreenVector):TAGScreenVector;inline;
    class operator Multiply(A:Real;B:TAGScreenVector):TAG2DVector;inline;
    class operator Multiply(A:Single;B:TAGScreenVector):TAG2DVector;inline;
    class operator Multiply(A:TAGScreenVector;B:TAG2DVector):TAG2DVector;inline;
    class operator Multiply(A:TAG2DVector;B:TAGScreenVector):TAG2DVector;inline;

    class operator Divide(A:TAGScreenVector;B:Integer):TAG2DVector;inline;
    class operator Divide(A:TAGScreenVector;B:Real):TAG2DVector;inline;
    class operator Divide(A:TAGScreenVector;B:Single):TAG2DVector;inline;
    class operator Divide(A:Integer;B:TAGScreenVector):TAG2DVector;inline;
    class operator Divide(A:Real;B:TAGScreenVector):TAG2DVector;inline;
    class operator Divide(A:Single;B:TAGScreenVector):TAG2DVector;inline;
    class operator Divide(A:TAGScreenVector;B:TAG2DVector):TAG2DVector;inline;
    class operator Divide(A:TAG2DVector;B:TAGScreenVector):TAG2DVector;inline;

    class operator Add(A,B:TAGScreenVector):TAGScreenVector;inline;
    class operator Subtract(A,B:TAGScreenVector):TAGScreenVector;inline;
    class operator Implicit(a:TAGScreenVector):TAG2DVector;inline;
    class operator Explicit(a:TAG2DVector):TAGScreenVector;inline;
    class operator Explicit(a:TAGScreenVector):TAG2DVector;inline;
    constructor Create(X,Y:Integer);
  end;

  TAGCoord=packed record
    constructor Create(X,Y,W,H:Single);
    function SizePosRev:TAGCoord;inline;
    class operator Add(A,B:TAGCoord):TAGCoord;inline;
    class operator Divide(A:TAGCoord;B:Single):TAGCoord;inline;
    case byte of
      0:(X,Y,W,H:Single);
      1:(arr:array[0..3]of Single);
  end;
  TAGScreenCoord=packed record
    constructor Create(X,Y,W,H:integer);
    constructor FromTRect(Rect:TRect);
    function SizePosRev:TAGScreenCoord;inline;
    function ToTRect:TRect;inline;
    class operator Add(A,B:TAGScreenCoord):TAGScreenCoord;inline;
    class operator IntDivide(A:TAGScreenCoord;B:integer):TAGScreenCoord;inline;
    class operator Implicit(a:TAGScreenCoord):TAGCoord;inline;
    class operator Explicit(a:TAGCoord):TAGScreenCoord;inline;
    class operator Explicit(a:TAGScreenCoord):TAGCoord;inline;
    case byte of
      0:(X,Y,W,H:integer);
      1:(arr:array[0..3]of integer);
  end;

const
  AGFont_SystemFont:TAGFont=0;
  AGFont_UserFonts=1;

  BlackColor:TAGColor=(R:0;G:0;B:0;A:255);
  RedColor:TAGColor=(R:255;G:0;B:0;A:255);
  GreenColor:TAGColor=(R:0;G:255;B:0;A:255);
  BlueColor:TAGColor=(R:0;G:0;B:255;A:255);
  WiteColor:TAGColor=(R:255;G:255;B:255;A:255);
  NoColor:TAGColor=(R:0;G:0;B:0;A:0);

implementation

{TAGColor}

constructor TAGColor.Create(R,G,B,A:Byte);
begin
Self.R:=R;
Self.G:=G;
Self.B:=B;
Self.A:=A;
end;

{TAGFloatColor}

constructor TAGFloatColor.Create(R,G,B,A:Single);
begin
  Self.R:=R;
  Self.G:=G;
  Self.B:=B;
  Self.A:=A;
end;

class operator TAGFloatColor.Implicit(c:TAGColor):TAGFloatColor;
begin
  Result.R:=c.R/255;
  Result.G:=c.G/255;
  Result.B:=c.B/255;
  Result.A:=c.A/255;
end;

class operator TAGFloatColor.Implicit(c:TAGFloatColor):TAGColor;
begin
  Result.R:=Round(c.R*255);
  Result.G:=Round(c.G*255);
  Result.B:=Round(c.B*255);
  Result.A:=Round(c.A*255);
end;

class operator TAGFloatColor.Explicit(c:TAGColor):TAGFloatColor;
begin
  Result.R:=c.R/255;
  Result.G:=c.G/255;
  Result.B:=c.B/255;
  Result.A:=c.A/255;
end;

class operator TAGFloatColor.Explicit(c:TAGFloatColor):TAGColor;
begin
  Result.R:=Round(c.R*255);
  Result.G:=Round(c.G*255);
  Result.B:=Round(c.B*255);
  Result.A:=Round(c.A*255);
end;

{TAGCoord}

constructor TAGCoord.Create(X,Y,W,H:Single);
begin
  Self.X:=X;
  Self.Y:=Y;
  Self.W:=W;
  Self.H:=H;
end;

function TAGCoord.SizePosRev:TAGCoord;
begin
  Result.W:=Self.X;
  Result.H:=Self.Y;
  Result.X:=Self.W;
  Result.Y:=Self.H;
end;

class operator TAGCoord.Add(A,B:TAGCoord):TAGCoord;
var
  i:integer;
begin
for i:=0 to 3 do
  Result.arr[i]:=A.arr[i]+B.arr[i];
end;

class operator TAGCoord.Divide(A:TAGCoord;B:Single):TAGCoord;
var
  i:integer;
begin
  for i:=0 to 3 do
    Result.arr[i]:=A.arr[i]/B;
end;

{TAGScreenCoord}

constructor TAGScreenCoord.Create(X,Y,W,H:integer);
begin
  Self.X:=X;
  Self.Y:=Y;
  Self.W:=W;
  Self.H:=H;
end;

function TAGScreenCoord.SizePosRev:TAGScreenCoord;
begin
  Result.W:=Self.X;
  Result.H:=Self.Y;
  Result.X:=Self.W;
  Result.Y:=Self.H;
end;

class operator TAGScreenCoord.Add(A,B:TAGScreenCoord):TAGScreenCoord;
var
  i:integer;
begin
  for i:=0 to 3 do
    Result.arr[i]:=A.arr[i]+B.arr[i];
end;

function TAGScreenCoord.ToTRect:TRect;
begin
  Result.left:=X;
  Result.top:=Y;
  Result.right:=W+X;
  Result.bottom:=H+Y;
end;

constructor TAGScreenCoord.FromTRect(Rect:TRect);
begin
  X:=Rect.Left;
  Y:=Rect.Top;
  W:=Rect.Right-Rect.Left;
  H:=Rect.Bottom-Rect.Top;
end;

class operator TAGScreenCoord.IntDivide(A:TAGScreenCoord;B:integer):TAGScreenCoord;
var
  i:integer;
begin
  for i:=0 to 3 do
    Result.arr[i]:=A.arr[i] div B;
end;

class operator TAGScreenCoord.Implicit(a:TAGScreenCoord):TAGCoord;
begin
  Result.X:=a.X;
  Result.Y:=a.Y;
  Result.W:=a.W;
  Result.H:=a.H;
end;

class operator TAGScreenCoord.Explicit(a:TAGCoord):TAGScreenCoord;
begin
  Result.X:=Round(a.X);
  Result.Y:=Round(a.Y);
  Result.W:=Round(a.W);
  Result.H:=Round(a.H);
end;

class operator TAGScreenCoord.Explicit(a:TAGScreenCoord):TAGCoord;
begin
  Result.X:=a.X;
  Result.Y:=a.Y;
  Result.W:=a.W;
  Result.H:=a.H;
end;

{TAG2DVector}

class function TAG2DVector.Create(x,y:Single):TAG2DVector;
begin
  Result.x:=x;
  Result.y:=y;
end;

function TAG2DVector.Add(const apt: TAG2DVector): TAG2DVector; 
begin
  Result.x:=x+apt.x;
  Result.y:=y+apt.y;
end;

function TAG2DVector.Distance(const apt : TAG2DVector) : Single; 
begin
  Result:=sqrt(sqr(x-apt.x)+sqr(y-apt.y));
end;

function TAG2DVector.DotProduct(const apt : TAG2DVector) : Single; 
begin
  Result:=x*apt.x+y*apt.y;
end;

function TAG2DVector.IsZero:Boolean;
begin       
  result:=SameValue(x,0.0) and SameValue(y,0.0);
end;

function TAG2DVector.Subtract(const apt : TAG2DVector): TAG2DVector; 
begin
  Result.x:=x-apt.x;
  Result.y:=y-apt.y;
end;

function  TAG2DVector.Scale (afactor:Single)  : TAG2DVector;   
begin
  Result.x:=x*afactor;
  Result.y:=y*afactor;
end;

function  TAG2DVector.Length  : Single;
begin
  Result:=sqrt(sqr(x)+sqr(y));
end;

function  TAG2DVector.Normalize: TAG2DVector; 
begin
  Result:=Self/Length;
end;

class operator TAG2DVector.= (const apt1, apt2 : TAG2DVector) : Boolean;  
begin
  result:=SameValue(apt1.x,apt2.x) and SameValue(apt1.y,apt2.y);
end;

class operator TAG2DVector.<> (const apt1, apt2 : TAG2DVector): Boolean;
begin
  result:=not(SameValue(apt1.x,apt2.x) and SameValue(apt1.y,apt2.y));
end;

class operator TAG2DVector.+ (const apt1, apt2 : TAG2DVector): TAG2DVector; 
begin
  Result.x:=apt1.x+apt2.x;
  Result.y:=apt1.y+apt2.y;
end;

class operator TAG2DVector.- (const apt1, apt2 : TAG2DVector): TAG2DVector;   
begin
  Result.x:=apt1.x-apt2.x;
  Result.y:=apt1.y-apt2.y;
end;

class operator TAG2DVector.- (const apt1 : TAG2DVector): TAG2DVector; 
begin
  Result.x:=-apt1.x;
  Result.y:=-apt1.y;
end;

class operator TAG2DVector.* (const apt1, apt2: TAG2DVector): Single; // scalar product   
begin
  Result:=apt1.x*apt2.x+apt1.y*apt2.y;
end;

class operator TAG2DVector.* (const apt1: TAG2DVector; afactor: single): TAG2DVector;  
begin
  Result:=apt1.Scale(afactor);
end;

class operator TAG2DVector.* (afactor: single; const apt1: TAG2DVector): TAG2DVector;  
begin
  Result:=apt1.Scale(afactor);
end;

class operator TAG2DVector./ (const apt1: TAG2DVector;afactor: single): TAG2DVector; 
begin
  Result.x:=apt1.x/afactor;
  Result.y:=apt1.y/afactor;
end;

{TAGScreenVector}

class operator TAGScreenVector.Multiply(A:TAGScreenVector;B:Integer):TAGScreenVector;
begin
Result.X:=A.X*B;
Result.Y:=A.Y*B;
end;

class operator TAGScreenVector.Multiply(A:TAGScreenVector;B:Real):TAG2DVector;
begin
Result.X:=A.X*B;
Result.Y:=A.Y*B;
end;

class operator TAGScreenVector.Multiply(A:TAGScreenVector;B:Single):TAG2DVector;
begin
Result.X:=A.X*B;
Result.Y:=A.Y*B;
end;

class operator TAGScreenVector.Multiply(A:Integer;B:TAGScreenVector):TAGScreenVector;
begin
Result.X:=A*B.X;
Result.Y:=A*B.Y;
end;

class operator TAGScreenVector.Multiply(A:Real;B:TAGScreenVector):TAG2DVector;
begin
Result.X:=A*B.X;
Result.Y:=A*B.Y;
end;

class operator TAGScreenVector.Multiply(A:Single;B:TAGScreenVector):TAG2DVector;
begin
Result.X:=A*B.X;
Result.Y:=A*B.Y;
end;

class operator TAGScreenVector.Multiply(A:TAGScreenVector;B:TAG2DVector):TAG2DVector;
begin
Result.X:=A.X*B.X;
Result.Y:=A.Y*B.Y;
end;

class operator TAGScreenVector.Multiply(A:TAG2DVector;B:TAGScreenVector):TAG2DVector;
begin
Result.X:=A.X*B.X;
Result.Y:=A.Y*B.Y;
end;

class operator TAGScreenVector.Divide(A:TAGScreenVector;B:Integer):TAG2DVector;
begin
  Result.X:=A.X/B;
  Result.Y:=A.Y/B;
end;

class operator TAGScreenVector.Divide(A:TAGScreenVector;B:Real):TAG2DVector;
begin
  Result.X:=A.X/B;
  Result.Y:=A.Y/B;
end;

class operator TAGScreenVector.Divide(A:TAGScreenVector;B:Single):TAG2DVector;
begin
  Result.X:=A.X/B;
  Result.Y:=A.Y/B;
end;

class operator TAGScreenVector.Divide(A:Integer;B:TAGScreenVector):TAG2DVector;
begin
  Result.X:=A/B.X;
  Result.Y:=A/B.Y;
end;

class operator TAGScreenVector.Divide(A:Real;B:TAGScreenVector):TAG2DVector;
begin
  Result.X:=A/B.X;
  Result.Y:=A/B.Y;
end;

class operator TAGScreenVector.Divide(A:Single;B:TAGScreenVector):TAG2DVector;
begin
  Result.X:=A/B.X;
  Result.Y:=A/B.Y;
end;

class operator TAGScreenVector.Divide(A:TAGScreenVector;B:TAG2DVector):TAG2DVector;
begin
  Result.X:=A.X/B.X;
  Result.Y:=A.Y/B.Y;
end;

class operator TAGScreenVector.Divide(A:TAG2DVector;B:TAGScreenVector):TAG2DVector;
begin
  Result.X:=A.X/B.X;
  Result.Y:=A.Y/B.Y;
end;

class operator TAGScreenVector.Add(A,B:TAGScreenVector):TAGScreenVector;
begin
  Result.X:=A.X+B.X;
  Result.Y:=A.Y+B.Y;
end;

class operator TAGScreenVector.Subtract(A,B:TAGScreenVector):TAGScreenVector;
begin
  Result.X:=A.X-B.X;
  Result.Y:=A.Y-B.Y;
end;

class operator TAGScreenVector.Implicit(a:TAGScreenVector):TAG2DVector;
begin
  Result.X:=A.X;
  Result.Y:=A.Y;
end;

class operator TAGScreenVector.Explicit(a:TAG2DVector):TAGScreenVector;
begin
  Result.X:=Round(A.X);
  Result.Y:=Round(A.Y);
end;

class operator TAGScreenVector.Explicit(a:TAGScreenVector):TAG2DVector;
begin
  Result.X:=A.X;
  Result.Y:=A.Y;
end;

constructor TAGScreenVector.Create(X,Y:Integer);
begin
  Self.X:=X;
  Self.Y:=Y;
end;

end.
