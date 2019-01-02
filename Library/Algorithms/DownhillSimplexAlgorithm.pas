{------------------------------------------------------------------------------
    This file is part of the MotifMASTER project. This software is
    distributed under GPL (see gpl.txt for details).

    This software is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Copyright (C) 1999-2007 D.Morozov (dvmorozov@mail.ru)
------------------------------------------------------------------------------}

unit DownhillSimplexAlgorithm;

interface

uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
    ComponentList, Decisions, Algorithm, Tools;

type
    TDownhillSimplexDecision = class(TFloatDecision)
    public
       function GetCopy: TDownhillSimplexDecision;
    end;

    TDownhillSimplexSADecision = class(TDownhillSimplexDecision)
    protected
        FFluctEvaluation: Double;

    public
        function GetCopy: TDownhillSimplexSADecision;

    published
        property FluctEvaluation: Double
        read FFluctEvaluation write FFluctEvaluation;
    end;

    IDownhillSimplexServer = interface
        ['{2E685960-1C7C-11D4-893E-FA8655FAEA48}']
        function GetInitParamLength(Sender: TComponent;
            ParameterNumber, ParametersCount: LongInt
            ): Double;
        procedure FillStartDecision(Sender: TComponent;
            StartDecision: TFloatDecision);
        procedure EvaluateDecision(Sender: TComponent;
            Decision: TFloatDecision);
        procedure UpdateResults(Sender: TComponent;
            Decision: TFloatDecision);
        function EndOfCalculation(Sender: TComponent): Boolean;
    end;

    EDownhillSimplexAlgorithm = class(Exception);
    TDownhillSimplexAlgorithm = class(TAlgorithm)
    protected
        FDownhillSimplexServer: IDownhillSimplexServer;
        FFinalTolerance: Double;
        FRestartDisabled: Boolean;
        FinalTolDefined: Boolean;
        FExitDerivative: Double;
        Simplex: TComponentList;
        ParametersSum: array of Double;
        BestDecision: TDownhillSimplexDecision;

        function TryNewDecision(const Highest: LongInt; Factor: Double): Double; virtual;
        function MoveWorstDecision(const Highest: LongInt; Factor: Double): TDownhillSimplexDecision;
        function CreateAppropriateDecision: TDownhillSimplexDecision; virtual;
        function GetActuallyLowestDecision: TDownhillSimplexDecision;
        procedure CreateSimplexVertices(StartDecision: TDownhillSimplexDecision);
        procedure ReplaceDecision(OldDecision, NewDecision: TDownhillSimplexDecision);
        procedure GetIndicativeDecisions(var Highest, NextHighest, Lowest: LongInt); virtual;
        procedure GetParametersSum;
        procedure Start;
        procedure Restart;
        procedure BasicCalcCycle(const Highest, NextHighest, Lowest: LongInt);
        procedure SetFinalTolerance(AFinalTolerance: Double);
        procedure SetParametersNumber(AParametersNumber: LongInt);
        function GetParametersNumber: LongInt;

    public
        procedure AlgorithmRealization; override;
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;

        property DownhillSimplexServer: IDownhillSimplexServer
            read FDownhillSimplexServer     write FDownhillSimplexServer;
        property FinalTolerance: Double
            read FFinalTolerance            write SetFinalTolerance;
        property RestartDisabled: Boolean
            read FRestartDisabled           write FRestartDisabled;
        property ParametersNumber: LongInt
            read GetParametersNumber        write SetParametersNumber;
        property ExitDerivative: Double
            read FExitDerivative            write FExitDerivative;
    end;

    TDownhillSimplexSAAlgorithm = class(TDownhillSimplexAlgorithm)
    protected
        FTemperature: Double;

        procedure GetIndicativeDecisions(var Highest, NextHighest, Lowest: LongInt); override;
        function TryNewDecision(const Highest: LongInt; Factor: Double): Double; override;
        function CreateAppropriateDecision: TDownhillSimplexDecision; override;
        function GetRandomFluct: Double;

    public
        procedure AlgorithmRealization; override;

    published
        property Temperature: Double read FTemperature write FTemperature;
    end;

const TINY = 1e-10;

procedure Register;

implementation

procedure Register;
begin
    RegisterComponents('Algorithms', [TDownhillSimplexAlgorithm]);
    RegisterComponents('Algorithms', [TDownhillSimplexSAAlgorithm]);
end;

procedure TDownhillSimplexAlgorithm.Restart;
var TempDecision: TDownhillSimplexDecision;
begin
    TempDecision := GetActuallyLowestDecision.GetCopy;
    with DownhillSimplexServer do EvaluateDecision(Self, TempDecision);
    CreateSimplexVertices(TempDecision);
    UtilizeObject(BestDecision);
    BestDecision := GetActuallyLowestDecision.GetCopy;
    DownhillSimplexServer.UpdateResults(Self, BestDecision);
end;

procedure TDownhillSimplexAlgorithm.Start;
var TempDecision: TDownhillSimplexDecision;
begin
    TempDecision := CreateAppropriateDecision;
    with DownhillSimplexServer do
    begin
        FillStartDecision(Self, TempDecision);
        EvaluateDecision(Self, TempDecision);
    end;        
    CreateSimplexVertices(TempDecision);
    UtilizeObject(BestDecision);
    BestDecision := GetActuallyLowestDecision.GetCopy;
    DownhillSimplexServer.UpdateResults(Self, BestDecision);
end;

function TDownhillSimplexAlgorithm.GetActuallyLowestDecision:
    TDownhillSimplexDecision;
var MinDecision, TempDecision: TDownhillSimplexDecision;
    i: LongInt;
begin
    MinDecision := TDownhillSimplexDecision(Simplex.Items[0]);
    for i := 1 to Simplex.Count - 1 do
    begin
        TempDecision := TDownhillSimplexDecision(Simplex.Items[i]);
        if TempDecision.Evaluation < MinDecision.Evaluation then
            MinDecision := TempDecision;
    end;
    Result := MinDecision;
end;

procedure TDownhillSimplexAlgorithm.CreateSimplexVertices(
    StartDecision: TDownhillSimplexDecision);
var i, j: LongInt;
    TempDecision: TDownhillSimplexDecision;
begin
    with DownhillSimplexServer do
    begin
        ParametersNumber := StartDecision.ParametersNumber;
        Simplex.Clear;
        Simplex.Add(StartDecision);         
        for i := 0 to ParametersNumber - 1 do
        begin
            TempDecision := CreateAppropriateDecision;
            TempDecision.ParametersNumber := ParametersNumber;
            for j := 0 to ParametersNumber - 1 do
                TempDecision.Parameters[j] := StartDecision.Parameters[j];
            TempDecision.Parameters[i] := TempDecision.Parameters[i] +
                GetInitParamLength(Self, i, TempDecision.ParametersNumber);
            EvaluateDecision(Self, TempDecision);
            Simplex.Add(TempDecision);
        end;        
    end;        
    GetParametersSum;
end;

procedure TDownhillSimplexAlgorithm.GetIndicativeDecisions(
    var Highest, NextHighest, Lowest: LongInt);
var i: LongInt;
begin
    if TDownhillSimplexDecision(Simplex.Items[0]).Evaluation >
       TDownhillSimplexDecision(Simplex.Items[1]).Evaluation then
    begin Highest := 0; NextHighest := 1; Lowest := 1 end
    else
    begin Highest := 1; NextHighest := 0; Lowest := 0 end;

    for i := 2 to Simplex.Count - 1 do
    begin
        if TDownhillSimplexDecision(Simplex.Items[i]).Evaluation <
            TDownhillSimplexDecision(Simplex.Items[Lowest]).Evaluation
            then Lowest := i;

        if TDownhillSimplexDecision(Simplex.Items[i]).Evaluation >
            TDownhillSimplexDecision(Simplex.Items[Highest]).Evaluation then
        begin NextHighest := Highest; Highest := i; end
        else
        begin
            if TDownhillSimplexDecision(Simplex.Items[i]).Evaluation >
                TDownhillSimplexDecision(Simplex.Items[NextHighest]).Evaluation then
            NextHighest := i;
        end;
    end;    
end;

procedure TDownhillSimplexSAAlgorithm.GetIndicativeDecisions(
    var Highest, NextHighest, Lowest: LongInt);
var i: LongInt;
begin
    with Simplex.Items[0] as TDownhillSimplexSADecision do
        FluctEvaluation := Evaluation + GetRandomFluct;

    with Simplex.Items[1] as TDownhillSimplexSADecision do
        FluctEvaluation := Evaluation + GetRandomFluct;

    if TDownhillSimplexSADecision(Simplex.Items[0]).FluctEvaluation >
       TDownhillSimplexSADecision(Simplex.Items[1]).FluctEvaluation then
    begin Highest := 0; NextHighest := 1; Lowest := 1 end
    else
    begin Highest := 1; NextHighest := 0; Lowest := 0 end;

    for i := 2 to Simplex.Count - 1 do
    begin
        with Simplex.Items[i] as TDownhillSimplexSADecision do
            FluctEvaluation := Evaluation + GetRandomFluct;

        if TDownhillSimplexSADecision(Simplex.Items[i]).FluctEvaluation <
            TDownhillSimplexSADecision(Simplex.Items[Lowest]).FluctEvaluation
            then Lowest := i;

        if TDownhillSimplexSADecision(Simplex.Items[i]).FluctEvaluation >
            TDownhillSimplexSADecision(Simplex.Items[Highest]).FluctEvaluation
            then begin NextHighest := Highest; Highest := i; end
        else
        begin
            if TDownhillSimplexSADecision(Simplex.Items[i]).FluctEvaluation >
                TDownhillSimplexSADecision(Simplex.Items[NextHighest]
                ).FluctEvaluation then NextHighest := i;
        end;
    end;    
end;

function TDownhillSimplexAlgorithm.CreateAppropriateDecision:
    TDownhillSimplexDecision;
begin
    Result := TDownhillSimplexDecision.Create(nil);
end;

function TDownhillSimplexSAAlgorithm.CreateAppropriateDecision:
    TDownhillSimplexDecision;
begin
    Result := TDownhillSimplexSADecision.Create(nil);
end;

function TDownhillSimplexAlgorithm.MoveWorstDecision(
    const Highest: LongInt;
    Factor: Double): TDownhillSimplexDecision;
var HighestDecision, TempDecision: TDownhillSimplexDecision;
    Factor1, Factor2: Double;
    j: LongInt;
begin
    HighestDecision := TDownhillSimplexDecision(Simplex.Items[Highest]);
    TempDecision := CreateAppropriateDecision;
    TempDecision.ParametersNumber := ParametersNumber;

    Factor1 := (1 - Factor) / ParametersNumber;
    Factor2 := Factor1 - Factor;
    for j := 0 to ParametersNumber - 1 do
        TempDecision.Parameters[j] := ParametersSum[j] * Factor1 -
            HighestDecision.Parameters[j] * Factor2;

    DownhillSimplexServer.EvaluateDecision(Self, TempDecision);
    Result := TempDecision;

    if TempDecision.Evaluation < BestDecision.Evaluation then
    begin
        UtilizeObject(BestDecision);
        BestDecision := TempDecision.GetCopy;
        DownhillSimplexServer.UpdateResults(Self, BestDecision);
    end;
end;

procedure TDownhillSimplexAlgorithm.ReplaceDecision(
    OldDecision, NewDecision: TDownhillSimplexDecision);
var Index: LongInt;
begin
    Index := Simplex.IndexOf(OldDecision);
    UtilizeObject(OldDecision);
    Simplex.Items[Index] := NewDecision;
    GetParametersSum;
end;

function TDownhillSimplexAlgorithm.TryNewDecision(
    const Highest: LongInt; Factor: Double): Double;
var HighestDecision, TempDecision: TDownhillSimplexDecision;
begin
    TempDecision := MoveWorstDecision(Highest, Factor);
    HighestDecision := TDownhillSimplexDecision(Simplex.Items[Highest]);

    Result := TempDecision.Evaluation;

    if TempDecision.Evaluation < HighestDecision.Evaluation then
        ReplaceDecision(HighestDecision, TempDecision)
    else UtilizeObject(TempDecision);
end;

function TDownhillSimplexSAAlgorithm.TryNewDecision(
    const Highest: LongInt; Factor: Double): Double;
var HighestDecision, TempDecision: TDownhillSimplexSADecision;
begin
    TempDecision := TDownhillSimplexSADecision(MoveWorstDecision(Highest, Factor));
    HighestDecision := TDownhillSimplexSADecision(Simplex.Items[Highest]);

    TempDecision.FluctEvaluation := TempDecision.Evaluation - GetRandomFluct;

    Result := TempDecision.FluctEvaluation;

    if TempDecision.FluctEvaluation < HighestDecision.FluctEvaluation then
        ReplaceDecision(HighestDecision, TempDecision)
    else UtilizeObject(TempDecision);
end;

procedure TDownhillSimplexAlgorithm.GetParametersSum;
var i, j: LongInt;
    Sum: Double;
begin
    for j := 0 to ParametersNumber - 1 do
    begin
        Sum := 0;
        for i := 0 to Simplex.Count - 1 do
            Sum := Sum + TDownhillSimplexDecision(Simplex.Items[i]).Parameters[j];
        ParametersSum[j] := Sum;
    end;
end;

procedure TDownhillSimplexAlgorithm.BasicCalcCycle(
    const Highest, NextHighest, Lowest: LongInt);
var TryResult, SavedResult: Double;
    i, j: LongInt;
begin
    with DownhillSimplexServer do
    begin
        TryResult := TryNewDecision(Highest, -1);
                if TryResult < TDownhillSimplexDecision(
            Simplex.Items[Lowest]).Evaluation then TryNewDecision(Highest, 2)
        else
        begin
            if TryResult >= TDownhillSimplexDecision(
                Simplex.Items[NextHighest]).Evaluation then
            begin
                SavedResult := TDownhillSimplexDecision(
                    Simplex.Items[Highest]).Evaluation;
                TryResult := TryNewDecision(Highest, 0.5);
                if TryResult >= SavedResult then
                begin
                    for i := 0 to Simplex.Count - 1 do
                        if i <> Lowest then
                        begin
                            for j := 0 to ParametersNumber - 1 do
                                TDownhillSimplexDecision(Simplex.Items[i]
                                ).Parameters[j] := 0.5 *
                                (

                                TDownhillSimplexDecision(Simplex.Items[i]
                                ).Parameters[j] +

                                TDownhillSimplexDecision(Simplex.Items[Lowest]
                                ).Parameters[j]

                                );
                            EvaluateDecision(Self,
                                TDownhillSimplexDecision(Simplex.Items[i]));
                        end;                        
                    GetParametersSum;
                end;                
            end;                                
        end;        
    end;    
end;

procedure TDownhillSimplexAlgorithm.AlgorithmRealization;
var Highest, NextHighest, Lowest: LongInt;
    Tolerance: Double;
    EvalHi, EvalLo: Double;
    SavedLoEval: Double;
begin
    if not Assigned(DownhillSimplexServer) then
        raise EDownhillSimplexAlgorithm.Create('Server is not assigned...');

    Start;
    SavedLoEval := GetActuallyLowestDecision.Evaluation;

    with DownhillSimplexServer do
        while not EndOfCalculation(Self) do
        begin
            GetIndicativeDecisions(Highest, NextHighest, Lowest);

            EvalHi := TDownhillSimplexDecision(
                Simplex.Items[Highest]).Evaluation;
            EvalLo := TDownhillSimplexDecision(
                Simplex.Items[Lowest]).Evaluation;

            Tolerance := 2 * Abs(EvalHi - EvalLo) /
                (Abs(EvalHi) + Abs(EvalLo) + TINY);

            if FinalTolDefined then if Tolerance < FinalTolerance then
            begin
                if (Abs(
                    GetActuallyLowestDecision.Evaluation - SavedLoEval
                    ) > ExitDerivative) and (not RestartDisabled) then
                begin

                    SavedLoEval := GetActuallyLowestDecision.Evaluation;
                    Restart;
                    Continue;

                end else Exit;
            end;

            BasicCalcCycle(Highest, NextHighest, Lowest);
        end;
end;

procedure TDownhillSimplexSAAlgorithm.AlgorithmRealization;
var Highest, NextHighest, Lowest: LongInt;
    Tolerance: Double;
    EvalHi, EvalLo: Double;
    SavedLoEval: Double;
    CycleCounter: LongInt;
begin
    if not Assigned(DownhillSimplexServer) then
        raise EDownhillSimplexAlgorithm.Create('Server is not assigned...');

    Randomize;
    Start;
    SavedLoEval := GetActuallyLowestDecision.Evaluation;
    CycleCounter := 0;

    with DownhillSimplexServer do
        while not EndOfCalculation(Self) do
        begin

            GetIndicativeDecisions(Highest, NextHighest, Lowest);

            EvalHi := TDownhillSimplexSADecision(
                Simplex.Items[Highest]).FluctEvaluation;
            EvalLo := TDownhillSimplexSADecision(
                Simplex.Items[Lowest]).FluctEvaluation;

            Tolerance := 2 * Abs(EvalHi - EvalLo) /
                (Abs(EvalHi) + Abs(EvalLo) + TINY);

            if FinalTolDefined and (Tolerance < FinalTolerance) then
            begin
                if GetActuallyLowestDecision.Evaluation < SavedLoEval then
                begin
                    SavedLoEval := GetActuallyLowestDecision.Evaluation;
                    Restart;
                    Continue;
                end else Exit;
            end;                
            BasicCalcCycle(Highest, NextHighest, Lowest);

            Inc(CycleCounter);
            if CycleCounter = 1000 then
            begin
                CycleCounter := 0;
                Temperature := Temperature * 0.95;
            end;           
        end;
end;

constructor TDownhillSimplexAlgorithm.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    Simplex := TComponentList.Create(nil);
end;

procedure TDownhillSimplexAlgorithm.SetFinalTolerance(AFinalTolerance: Double);
begin
    FFinalTolerance := AFinalTolerance;
    FinalTolDefined := True;
end;

procedure TDownhillSimplexAlgorithm.SetParametersNumber(AParametersNumber: LongInt);
begin
    SetLength(ParametersSum, AParametersNumber);
end;

function TDownhillSimplexAlgorithm.GetParametersNumber: LongInt;
begin
    if Assigned(ParametersSum) then Result := Length(ParametersSum)
    else Result := 0;
end;

destructor TDownhillSimplexAlgorithm.Destroy;
begin
    UtilizeObject(Simplex);
    UtilizeObject(BestDecision);
    inherited Destroy;
end;

function TDownhillSimplexSAAlgorithm.GetRandomFluct: Double;
begin
    Result := (-1) * Temperature * Ln(Random + TINY);
end;

function TDownhillSimplexDecision.GetCopy: TDownhillSimplexDecision;
begin
    Result := TDownhillSimplexDecision(inherited GetCopy);
end;

function TDownhillSimplexSADecision.GetCopy: TDownhillSimplexSADecision;
var i: LongInt;
    TempDecision: TDownhillSimplexSADecision;
begin
    TempDecision := TDownhillSimplexSADecision.Create(nil);
    TempDecision.ParametersNumber := ParametersNumber;
    for i := 0 to ParametersNumber - 1 do
        TempDecision.Parameters[i] := Parameters[i];
    TempDecision.Evaluation := Evaluation;
    TempDecision.FluctEvaluation := FluctEvaluation;
    Result := TempDecision;
end;

initialization
    RegisterClass(TDownhillSimplexAlgorithm);
    RegisterClass(TDownhillSimplexSAAlgorithm);
    RegisterClass(TDownhillSimplexDecision);
    RegisterClass(TDownhillSimplexSADecision);
end.

