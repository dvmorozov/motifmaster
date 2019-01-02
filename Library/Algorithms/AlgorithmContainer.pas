{------------------------------------------------------------------------------
    This file is part of the MotifMASTER project. This software is
    distributed under GPL (see gpl.txt for details).

    This software is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Copyright (C) 1999-2007 D.Morozov (dvmorozov@mail.ru)
------------------------------------------------------------------------------}

unit AlgorithmContainer;

interface

uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
    Runner, Algorithm;

type
    TAlgorithmContainer = class(TComponent)
    protected
        Runner: TRunner;
        Algorithm: TAlgorithm;

        procedure Running; virtual; abstract;           
        procedure RunningFinished; virtual; abstract;
        procedure CreateAlgorithm; virtual; abstract;
        procedure StopAlgorithm; virtual; abstract;
        procedure DestroyAlgorithm; virtual; abstract;

    public
        constructor Create(AOwner: TComponent); override;
        procedure Run; virtual;
        procedure Suspend; virtual;
        procedure Resume; virtual;
    end;

procedure Register;

implementation

procedure Register;
begin
end;

constructor TAlgorithmContainer.Create(AOwner: TComponent);
begin
    inherited Create(nil);
    Runner := TRunner.Create(nil);
    Runner.OnRunningProcedure := Running;
    Runner.OnEndRunningProcedure := RunningFinished;
end;

procedure TAlgorithmContainer.Run;
begin
    Runner.Run;
end;

procedure TAlgorithmContainer.Suspend;
begin
    Runner.Suspend;
end;

procedure TAlgorithmContainer.Resume;
begin
    Runner.Resume;
end;

initialization
    RegisterClass(TAlgorithmContainer);
end.
