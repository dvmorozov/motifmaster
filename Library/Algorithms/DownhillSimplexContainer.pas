{------------------------------------------------------------------------------
    This file is part of the MotifMASTER project. This software is
    distributed under GPL (see gpl.txt for details).

    This software is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Copyright (C) 1999-2007 D.Morozov (dvmorozov@mail.ru)
------------------------------------------------------------------------------}

unit DownhillSimplexContainer;

interface

uses
    Classes, DownhillSimplexAlgorithm, AlgorithmContainer, Decisions, SysUtils,
    SimpMath, CombEnumerator, CBRCComponent, Tools, SelfSaved, ClassInheritIDs;

const
    PH_CREATING    = 0;         
    PH_WORKING     = 1;     

type
    TVariableParameter = record
        Value: Double;
        Limited: Boolean;
        MaxLimit, MinLimit: Double;
    end;

    IDownhillSimplexParameters = interface(IDiscretValue)
        function GetParametersNumber: LongInt;
        function GetParameter(index: LongInt): TVariableParameter;
        procedure SetParameter(index: LongInt; AParameter: TVariableParameter);

        property ParametersNumber: LongInt
            read GetParametersNumber;
        property Parameter[index: LongInt]: TVariableParameter
            read GetParameter           write SetParameter;
    end;

    IDownhillRealParameters = interface(IDownhillSimplexParameters)
        procedure CreateParameters;
        procedure ParametersUpdated;
    end;

    IOptimizedFunction = interface
        function GetOptimizedFunction: Double;
    end;

    IUpdatingResults = interface
        procedure ShowCurJobProgress(Sender: TComponent;
            MinValue, MaxValue, CurValue: LongInt);
        procedure ResetCurJobProgress(Sender: TComponent);
        procedure ShowMessage(Sender: TComponent; Msg: string);
        procedure UpdatingResults(Sender: TComponent);
    end;

    EDownhillRealParameters = class(Exception);
    TDownhillRealParameters = class(TCBRCComponent, IDownhillRealParameters)
    protected
        FParameters: Pointer;                   
        FParametersNumber: LongInt;             
        PhaseParameters: Byte;                  
        
        procedure CreateParameters; virtual;
        procedure FreeParameters;
        procedure FillParameters; virtual; abstract;
        procedure ParametersUpdated; virtual; abstract;
        function GetActualParametersNumber: LongInt; virtual; abstract;
        function GetParametersNumber: LongInt;
        function GetParameter(index: LongInt): TVariableParameter;
        procedure SetParameter(index: LongInt; AParameter: TVariableParameter);

        function GetNumberOfValues: LongInt; virtual; abstract;
        function GetValueIndex: LongInt; virtual; abstract;
        procedure SetValueIndex(const AValueIndex: LongInt); virtual; abstract;

        class function GetPropHeaderRec: TPropHeaderRec; override;
        class procedure ReadProperties(
            const Reader: TReader;
            const PropHeaderRec: TPropHeaderRec;
            const AnObject: TSelfSavedComponent
            ); override;

        class procedure WriteProperties(
            const Writer: TWriter;
            const AnObject: TSelfSavedComponent
            ); override;

    public
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;

        property ParametersNumber: LongInt  read GetParametersNumber;
        property Parameter[index: LongInt]: TVariableParameter
            read GetParameter           write SetParameter;

        property NumberOfValues: LongInt    read GetNumberOfValues;
        property ValueIndex: LongInt
            read GetValueIndex          write SetValueIndex;
    end;

    EDownhillSimplexContainer = class(Exception);
    TDownhillSimplexContainer = class(TAlgorithmContainer, IDownhillSimplexServer)
    protected
        FParametersInterfaces : array of IDownhillRealParameters;
        FIOptimizedFunction: IOptimizedFunction;
        FIUpdatingResults: IUpdatingResults;

        CombSelector: TCombSelector;

        FFinalTolerance: Double;
        FRestartDisabled: Boolean;
        FExitDerivative: Double;

        EOC: Boolean;
        TotalMinimum: Double;                                                                                                   
        message: string;

        procedure Running; override;            
        procedure RunningFinished; override;
        procedure ShowMessage;
        procedure UpdateMainForm;
        procedure FillParameters(Decision: TFloatDecision); virtual;
        procedure CreateAlgorithm; override;
        procedure CreateParameters;
        procedure DestroyAlgorithm; override;
        procedure StopAlgorithm; override;
        function GetInitParamLength(Sender: TComponent;
            ParameterNumber, ParametersCount: LongInt): Double; virtual;
        procedure FillStartDecision(Sender: TComponent;
            StartDecision: TFloatDecision); virtual;
        procedure EvaluateDecision(Sender: TComponent;
            Decision: TFloatDecision); virtual;
        procedure UpdateResults(Sender: TComponent;
            Decision: TFloatDecision); virtual;
        function EndOfCalculation(Sender: TComponent): Boolean;
        function GetIUpdatingResults: IUpdatingResults;
        function GetIOptimizedFunction: IOptimizedFunction;

        function GetIDSPsNumber: LongInt;
        function GetIDSP(index: LongInt): IDownhillRealParameters;
        property IDSPsNumber: LongInt read GetIDSPsNumber;
        property IDSP[index: LongInt]: IDownhillRealParameters read GetIDSP;

        function GetParametersNumber: LongInt;
        function GetParameter(index: LongInt): TVariableParameter;
        procedure SetParameter(index: LongInt; AParameter: TVariableParameter);

        property ParametersNumber: LongInt  read GetParametersNumber;
        property Parameter[index: LongInt]: TVariableParameter
            read GetParameter           write SetParameter;

    public
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;

        procedure ClearListOfIDSPs;
        procedure AddIDSPToList(const IDSP: IDownhillRealParameters);

        property OptimizedFunction: IOptimizedFunction
            read GetIOptimizedFunction  write FIOptimizedFunction;
        property UpdatingResults: IUpdatingResults
            read GetIUpdatingResults    write FIUpdatingResults;

        property FinalTolerance: Double
            read FFinalTolerance        write FFinalTolerance;
        property RestartDisabled: Boolean
            read FRestartDisabled       write FRestartDisabled;
        property ExitDerivative: Double
            read FExitDerivative        write FExitDerivative;
    end;

implementation

type
    TParametersArray = array[0..MaxInt div SizeOf(TVariableParameter) - 1] of TVariableParameter;

function TDownhillSimplexContainer.GetInitParamLength(
    Sender: TComponent; ParameterNumber, ParametersCount: LongInt): Double;
begin
    Result := 0.1;
end;

procedure TDownhillSimplexContainer.FillStartDecision(Sender: TComponent; StartDecision: TFloatDecision);
var i: LongInt;
    TempParamsNumber: LongInt;          
    TempParameter: TVariableParameter;
begin
    TempParamsNumber := ParametersNumber;
    StartDecision.ParametersNumber := TempParamsNumber;
    for i := 0 to TempParamsNumber - 1 do begin
        TempParameter := Parameter[i];
        with TempParameter do
            if Limited and not IsValueIntoInterval(MinLimit, MaxLimit, Value) then
                raise EDownhillSimplexContainer.Create(
                    'Parameter value is not into the interval...')
            else StartDecision.Parameters[i] := Value;
    end;{for i := 0 to TempParametersNumber - 1 do...}
end;

procedure TDownhillSimplexContainer.FillParameters(Decision: TFloatDecision);
var i: LongInt;
    TempParamsNumber: LongInt;          
    TempParameter: TVariableParameter;
    TempIDSP: IDownhillRealParameters;
begin
    TempParamsNumber := ParametersNumber;
    for i := 0 to TempParamsNumber - 1 do
    begin
        TempParameter := Parameter[i];
        TempParameter.Value := Decision.Parameters[i];
        with TempParameter do
            if Limited then PutValueIntoInterval(MinLimit, MaxLimit, Value);
        Parameter[i] := TempParameter;
    end;

    for i := 0 to IDSPsNumber - 1 do
    begin
        TempIDSP := IDSP[i];
        TempIDSP.ParametersUpdated;
    end;
end;

procedure TDownhillSimplexContainer.EvaluateDecision(Sender: TComponent;
    Decision: TFloatDecision);
begin
    FillParameters(Decision);
    Decision.Evaluation := OptimizedFunction.GetOptimizedFunction;
end;

procedure TDownhillSimplexContainer.UpdateResults(Sender: TComponent;
    Decision: TFloatDecision);
begin
    EvaluateDecision(Sender, Decision);
    if Decision.Evaluation < TotalMinimum then
    begin
        TotalMinimum := Decision.Evaluation;
        Runner.Synchronize(UpdateMainForm);
    end;
end;

function TDownhillSimplexContainer.EndOfCalculation(Sender: TComponent): Boolean;
begin
    Result := EOC;
end;

constructor TDownhillSimplexContainer.Create;
begin
    inherited Create(AOwner);
    CombSelector := TCombSelector.Create;
end;

destructor TDownhillSimplexContainer.Destroy;
begin
    StopAlgorithm;
    UtilizeObject(Runner);
    DestroyAlgorithm;

    ClearListOfIDSPs;
    UtilizeObject(CombSelector);
    inherited Destroy;
end;

procedure TDownhillSimplexContainer.ClearListOfIDSPs;
var i: LongInt;
begin
    for i := 0 to Length(FParametersInterfaces) - 1 do
        FParametersInterfaces[i] := nil;                                                                                                
    Finalize(FParametersInterfaces);
    CombSelector.ClearDiscretValuesList;
end;

procedure TDownhillSimplexContainer.AddIDSPToList(
    const IDSP: IDownhillRealParameters);
begin
    SetLength(FParametersInterfaces, Length(FParametersInterfaces) + 1);
    FParametersInterfaces[Length(FParametersInterfaces) - 1] := IDSP;
    CombSelector.AddDiscretValue(IDSP);
end;

function TDownhillSimplexContainer.GetIDSPsNumber: LongInt;
begin
    Result := Length(FParametersInterfaces);
end;

function TDownhillSimplexContainer.GetIDSP(
    index: LongInt): IDownhillRealParameters;
begin
    Result := FParametersInterfaces[index];
end;

function TDownhillSimplexContainer.GetIUpdatingResults: IUpdatingResults;
begin
    if Assigned(FIUpdatingResults) then Result := FIUpdatingResults
    else raise EDownhillSimplexContainer.Create(
        'Updating results interface must be assigned...');
end;

function TDownhillSimplexContainer.GetIOptimizedFunction: IOptimizedFunction;
begin
    if Assigned(FIOptimizedFunction) then Result := FIOptimizedFunction
    else raise EDownhillSimplexContainer.Create(
        'Optimized function interface must be assigned...');
end;

procedure TDownhillSimplexContainer.StopAlgorithm;
begin
    EOC := True;
end;

procedure TDownhillSimplexContainer.DestroyAlgorithm;
begin
    UtilizeObject(Algorithm);
end;

procedure TDownhillSimplexContainer.UpdateMainForm;
begin
    UpdatingResults.UpdatingResults(Self);
end;

procedure TDownhillSimplexContainer.ShowMessage;
begin
    UpdatingResults.ShowMessage(Self, message);
end;

procedure TDownhillSimplexContainer.RunningFinished;
begin
    message := 'Calculation done...';
    Runner.Synchronize(ShowMessage);
end;

procedure TDownhillSimplexContainer.CreateAlgorithm;
begin
    UtilizeObject(Algorithm);
    Algorithm := TDownhillSimplexAlgorithm.Create(nil);
        with Algorithm as TDownhillSimplexAlgorithm do
        begin
            DownhillSimplexServer := Self;
            FinalTolerance := Self.FinalTolerance;
            RestartDisabled := Self.RestartDisabled;
            ExitDerivative := Self.ExitDerivative;
        end;
end;

procedure TDownhillSimplexContainer.Running;
var i: LongInt;
begin
    TotalMinimum := 1e6;        
    UpdatingResults.ResetCurJobProgress(Self);
    UpdatingResults.ShowCurJobProgress(Self, 0, CombSelector.CombNumber, 0);
    for i := 0 to CombSelector.CombNumber - 1 do
    begin
        if EOC then Exit;
        CombSelector.CurrentComb := i;
        CreateParameters;
        if ParametersNumber <> 0 then
        begin
            CreateAlgorithm;
            Algorithm.AlgorithmRealization;
        end else
        begin
            message := 'List of parameters is empty for combination ' + IntToStr(i);
            Runner.Synchronize(ShowMessage);
        end;
        UpdatingResults.ShowCurJobProgress(Self, 0, CombSelector.CombNumber, i + 1);
    end;
end;

function TDownhillSimplexContainer.GetParametersNumber: LongInt;
var i: LongInt;
begin
    Result := 0;
    for i := 0 to IDSPsNumber - 1 do
        Result := Result + IDSP[i].ParametersNumber;
end;

procedure TDownhillSimplexContainer.CreateParameters;
var i: LongInt;
begin
    for i := 0 to IDSPsNumber - 1 do IDSP[i].CreateParameters;
end;

function TDownhillSimplexContainer.GetParameter(
    index: LongInt): TVariableParameter;
var i: LongInt;
    TempIDSP: IDownhillRealParameters;
    TempParamNumber: LongInt;
    ParamSum: LongInt;
begin
    if (index < 0) then
        raise EDownhillSimplexContainer.Create('Invalid parameter index...');
    ParamSum := 0;
    for i := 0 to IDSPsNumber - 1 do begin
        TempIDSP := IDSP[i];
        TempParamNumber := TempIDSP.ParametersNumber;
                if (index >= ParamSum) and (index < ParamSum + TempParamNumber) then
        begin
            Result := TempIDSP.Parameter[index - ParamSum];
            Exit;
        end else ParamSum := ParamSum + TempParamNumber;
    end;
    raise EDownhillSimplexContainer.Create('Invalid parameter index...');
end;

procedure TDownhillSimplexContainer.SetParameter(
    index: LongInt; AParameter: TVariableParameter);
var i: LongInt;
    TempIDSP: IDownhillRealParameters;
    TempParamNumber: LongInt;       
    ParamSum: LongInt;
begin
    if (index < 0) then
        raise EDownhillSimplexContainer.Create('Invalid parameter index...');
    ParamSum := 0;
    for i := 0 to IDSPsNumber - 1 do
    begin
        TempIDSP := IDSP[i];
        TempParamNumber := TempIDSP.ParametersNumber;
                if (index >= ParamSum) and (index < ParamSum + TempParamNumber) then
        begin
            TempIDSP.Parameter[index - ParamSum] := AParameter;
            Exit;
        end else ParamSum := ParamSum + TempParamNumber;
    end;
    raise EDownhillSimplexContainer.Create('Invalid parameter index...');
end;

procedure TDownhillRealParameters.CreateParameters;
var ActParNum: LongInt;
begin
    FreeParameters;
    ActParNum := GetActualParametersNumber;
    if ActParNum <> 0 then
    begin
        GetMem(FParameters, ActParNum * SizeOf(TVariableParameter));
        FParametersNumber := ActParNum;
        PhaseParameters := PH_WORKING;
                FillParameters;
    end else
    begin
        FParameters := nil;
        FParametersNumber := 0;
        PhaseParameters := PH_WORKING;
    end;
end;

procedure TDownhillRealParameters.FreeParameters;
begin
    if PhaseParameters = PH_WORKING then
        if Assigned(FParameters) then
            FreeMem(FParameters);
end;

function TDownhillRealParameters.GetParameter(
    index: LongInt): TVariableParameter;
begin
    if (Index < 0) or (Index >= ParametersNumber) then
        raise EDownhillRealParameters.Create('Invalid parameter index...')
    else Result := TParametersArray(FParameters^)[index];
end;

procedure TDownhillRealParameters.SetParameter(
    index: LongInt; AParameter: TVariableParameter);
begin
    if (Index < 0) or (Index >= ParametersNumber) then
        raise EDownhillRealParameters.Create('Invalid parameter index...')
    else TParametersArray(FParameters^)[index] := AParameter;
end;

function TDownhillRealParameters.GetParametersNumber: LongInt;
begin
    case PhaseParameters of
        PH_CREATING : raise EDownhillRealParameters.Create(
            'Parameters must be created...');
        PH_WORKING : Result := FParametersNumber;
        else raise EDownhillRealParameters.Create(
            'Invalid phase number...');
    end;
end;

constructor TDownhillRealParameters.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    PhaseParameters := PH_CREATING;
end;

destructor TDownhillRealParameters.Destroy;
begin
    FreeParameters;
    inherited Destroy;
end;

class function TDownhillRealParameters.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := DRPClassInheritID;
    Result.PropVersionNum := DRPCurVerNum;
end;

class procedure TDownhillRealParameters.ReadProperties(
    const Reader: TReader; const PropHeaderRec: TPropHeaderRec;
    const AnObject: TSelfSavedComponent);
begin

end;

class procedure TDownhillRealParameters.WriteProperties(
    const Writer: TWriter; const AnObject: TSelfSavedComponent);
begin

end;

initialization
    RegisterClass(TDownhillSimplexContainer);
    RegisterClass(TDownhillRealParameters);
end.

