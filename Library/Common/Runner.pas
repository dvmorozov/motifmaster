{------------------------------------------------------------------------------
    This file is part of the MotifMASTER project. This software is
    distributed under GPL (see gpl.txt for details).

    This software is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Copyright (C) 1999-2007 D.Morozov (dvmorozov@mail.ru)
------------------------------------------------------------------------------}

unit Runner;

{$IFDEF Lazarus}
{$MODE Delphi}
{$ENDIF}

interface

uses Classes,
{$IFNDEF Lazarus}
DsgnIntf,
{$ENDIF}
Tools;

type
    TRunningProcedure = procedure of object;
    TEndRunningProcedure = procedure of object;

    TRunningThread = class(TThread)
    public
        RunningProcedure: TRunningProcedure;
        EndRunningProcedure: TEndRunningProcedure;
        procedure Execute; override;
    end;

    TRunner = class(TComponent)
    protected
        FOnRunningProcedure: TRunningProcedure;
        FOnEndRunningProcedure: TEndRunningProcedure;
        RunningThread: TRunningThread;

    public
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        procedure Run;
        procedure Suspend;
        procedure Resume;
        procedure Synchronize(AProcedure: TThreadMethod);

    published
        property OnRunningProcedure: TRunningProcedure
                read FOnRunningProcedure        write FOnRunningProcedure;
        property OnEndRunningProcedure: TEndRunningProcedure
                read FOnEndRunningProcedure     write FOnEndRunningProcedure;
    end;

procedure Register;

implementation

procedure Register;
begin
    RegisterComponents('Common',[TRunner]);
{$IFNDEF Lazarus}
    RegisterPropertyEditor(TypeInfo(TRunningProcedure),TRunner,'OnRunningProcedure',TMethodProperty);
    RegisterPropertyEditor(TypeInfo(TEndRunningProcedure),TRunner,'OnEndRunningProcedure',TMethodProperty);
{$ENDIF}
end;

procedure TRunningThread.Execute;
begin
    if Assigned(RunningProcedure) then RunningProcedure;
    if (not Terminated) and Assigned(EndRunningProcedure) then EndRunningProcedure;
end;

constructor TRunner.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    RunningThread := TRunningThread.Create(True);
end;

destructor TRunner.Destroy;
begin
    UtilizeObject(RunningThread);
    inherited Destroy;
end;

procedure TRunner.Run;
begin
    RunningThread.RunningProcedure := OnRunningProcedure;
    RunningThread.EndRunningProcedure := OnEndRunningProcedure;
    RunningThread.Resume;
end;

procedure TRunner.Suspend;
begin
    RunningThread.Suspend;
end;

procedure TRunner.Resume;
begin
    RunningThread.Resume;
end;

procedure TRunner.Synchronize;
begin
    RunningThread.Synchronize(AProcedure);
end;

end.
