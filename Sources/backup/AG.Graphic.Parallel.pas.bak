unit AG.Graphic.Parallel;

interface

{$IFDEF FPC}
  {$mode DELPHI}
{$ENDIF}

uses
  AG.Graphic,SysUtils,Classes,SyncObjs,lazcollections;

type
  TAGParallelGraphicCore=class abstract(TAGGraphicCore)
    public type
      TAGWorkProc=procedure(Data:Pointer);
      TAGWork=record
        Data:Pointer;
        Work:TAGWorkProc;
        class function Create(Data:Pointer;Work:TAGWorkProc):TAGWork;static;
      end;
    strict protected type
      TAGGraphicThread=class(TThread)
        protected
          TotalDuedWork:UInt64;
          procedure Execute;override;
        public
          WorkQueue:TLazThreadedQueue<TAGWork>;
          constructor Create();
        end;
      var
        GraphicThread:TAGGraphicThread;
    strict protected
      TotalStartedWork:UInt64;
      procedure DoGraphic(Work:TAGWork);overload;
      procedure DoGraphic(Data:Pointer;Work:TAGWorkProc);overload;
      procedure DoGraphicSync(Work:TAGWork);overload;
      procedure DoGraphicSync(Data:Pointer;Work:TAGWorkProc);overload;
    public
      constructor Create();
      destructor Destroy();override;
      procedure Flush();
  end;

implementation

{TAGParallelGraphicCore}
        
{TAGParallelGraphicCore.TAGWork}

class function TAGParallelGraphicCore.TAGWork.Create(Data:Pointer;Work:TAGWorkProc):TAGWork;
begin
  Result.Data:=Data;      
  Result.Work:=Work;
end;

{TAGParallelGraphicCore.TAGMathThread.TAGGraphicThread}
constructor TAGParallelGraphicCore.TAGGraphicThread.Create();
begin
  inherited Create(false);
  TotalDuedWork:=0;
  WorkQueue:=TLazThreadedQueue<TAGWork>.Create();
end;

procedure TAGParallelGraphicCore.TAGGraphicThread.Execute();
var
  WorkData:TAGWork;
begin
  while(WorkQueue.PopItem(WorkData)=wrSignaled)and not Terminated do
  begin
    WorkData.Work(WorkData.Data);
    inc(TotalDuedWork);
  end;
end;
                                         
{TAGParallelGraphicCore}

procedure TAGParallelGraphicCore.DoGraphic(Work:TAGWork);
begin
  inc(TotalStartedWork);
  GraphicThread.WorkQueue.PushItem(Work);
end;

procedure TAGParallelGraphicCore.DoGraphic(Data:Pointer;Work:TAGWorkProc);
begin
  DoGraphic(TAGWork.Create(Data,Work));
end;

procedure TAGParallelGraphicCore.DoGraphicSync(Work:TAGWork);
var
  Num:Cardinal;
begin
  DoGraphic(Work);
  Num:=TotalStartedWork;
  while Num>GraphicThread.TotalDuedWork do
    TThread.Sleep(1);
end;   

procedure TAGParallelGraphicCore.DoGraphicSync(Data:Pointer;Work:TAGWorkProc);
begin
  DoGraphicSync(TAGWork.Create(Data,Work));
end;

constructor TAGParallelGraphicCore.Create();
begin
  inherited Create();
  GraphicThread:=TAGGraphicThread.Create;
  TotalStartedWork:=0;
end;

destructor TAGParallelGraphicCore.Destroy();
begin
  Flush();
  GraphicThread.Terminate;
  while not GraphicThread.Finished do
    TThread.Sleep(1);
  TThread.Sleep(1000);
  FreeAndNil(GraphicThread.WorkQueue);
  FreeAndNil(GraphicThread);
  inherited;
end;

procedure TAGParallelGraphicCore.Flush();
begin
  while GraphicThread.TotalDuedWork<>TotalStartedWork do
    TThread.Sleep(0);
end;

end.
