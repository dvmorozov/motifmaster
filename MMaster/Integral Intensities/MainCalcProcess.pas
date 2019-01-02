{------------------------------------------------------------------------------
    This file is part of the MotifMASTER project. This software is
    distributed under GPL (see gpl.txt for details).

    This software is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Copyright (C) 1999-2007 D.Morozov (dvmorozov@mail.ru)
------------------------------------------------------------------------------}

unit MainCalcProcess;

interface

uses
  Classes, SysUtils;

type
  ETaskIsNotDefined = class(Exception);
  TCurrentTask = procedure of object;
  TShowResultsProc = procedure of object;
  TDoneProc = procedure of object;

  TMainCalcProcess = class(TThread)
  private
    CurrentTask : TCurrentTask;
    ShowResultsProc : TShowResultsProc;
    DoneProc : TDoneProc;
  public
    procedure  Execute; override;
    procedure  Synchronize(Method: TThreadMethod);
    procedure  SetCurrentTask(ACurrentTask : TCurrentTask);
    procedure  SetShowResultsProc(AShowResultsProc : TShowResultsProc);
    procedure  SetDoneProc(ADoneProc : TDoneProc);
    procedure  ShowResults;
  end;

implementation

procedure TMainCalcProcess.SetCurrentTask(ACurrentTask : TCurrentTask);
begin
 CurrentTask := ACurrentTask;
end;

procedure TMainCalcProcess.SetShowResultsProc(AShowResultsProc : TShowResultsProc);
begin
 ShowResultsProc := AShowResultsProc;
end;

procedure TMainCalcProcess.SetDoneProc(ADoneProc : TDoneProc);
begin
 DoneProc := ADoneProc;
end;

procedure TMainCalcProcess.Execute;
begin
 if Assigned(CurrentTask) then CurrentTask
  else raise ETaskIsNotDefined.Create('Current task is not defined...');
 if Assigned(DoneProc) then Synchronize(DoneProc); 
end;

procedure TMainCalcProcess.ShowResults;
begin
 if Assigned(ShowResultsProc) then Synchronize(ShowResultsProc);
end;

procedure  TMainCalcProcess.Synchronize(Method: TThreadMethod);
begin
 inherited Synchronize(Method);
end;

end.
