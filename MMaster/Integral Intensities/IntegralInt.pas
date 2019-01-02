{------------------------------------------------------------------------------
    This file is part of the MotifMASTER project. This software is
    distributed under GPL (see gpl.txt for details).

    This software is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Copyright (C) 1999-2007 D.Morozov (dvmorozov@mail.ru)
------------------------------------------------------------------------------}

unit IntegralInt;

interface

uses Classes, DataLoader, ComponentList, SysUtils, Minimizer_S,
     MSCRDataClasses, Dialogs;

type

 EPointsSetIsEmpty = class(Exception);
 EListIsEmpty = class(Exception);
 EResultedIntListIsNotAssigned = class(Exception);
 EPointsSetIsNotGaussian = class(Exception);

 TPlotGaussians = procedure(Sender : TObject; GaussPointsSetList : TComponentList) of object;
 TPlotSelectedPoints = procedure(Sender : TObject; SelectedPoints : TNeutronPointsSet) of object;
 TPlotNeutronPoints = procedure(Sender : TObject; NeutronPoints : TNeutronPointsSet) of object;
 TPlotSelectedArea = procedure(Sender : TObject; SelectedArea : TNeutronPointsSet) of object;
 TPlotBackground = procedure(Sender : TObject; BackgroundPoints : TNeutronPointsSet) of object;
 TPlotGaussProfile = procedure(Sender : TObject; GaussProfile : TNeutronPointsSet) of object;
 TRefresh = procedure(Sender : TObject) of object;
 TRefreshPointsSet = procedure(Sender : TObject; PointsSet : TNeutronPointsSet) of object;
 TClear = procedure(Sender : TObject) of object;
 THide = procedure(Sender : TObject; PointsSet : TNeutronPointsSet) of object;
 TViewResultedIntSet = procedure(Sender : TObject; ResultedIntSet : TMSCRNeutronCompList) of object;
 TGaussiansFindingDone = procedure(Sender : TObject) of object;

 TIntegralIntMaker = class(TComponent)
 private
  MainCalcProcess : TThread;
  SavedPointsSetList : TComponentList;
  NeutronPointsSet : TNeutronPointsSet;
  BackgroundPointsSet : TNeutronPointsSet;
  SelectedArea : TNeutronPointsSet;
  GaussPointsSetList : TComponentList;
  GaussProfile : TNeutronPointsSet;
  SelectedPoints : TNeutronPointsSet;
  ActivePointsSet : TNeutronPointsSet;
  ResultedIntSet : TMSCRNeutronCompList;

  NeutronPointsSetOuter : Boolean;
  ResultedIntSetOuter : Boolean;

  Minimizer : TSimpleMinimizer2;
  AStep,SigmaStep,x0Step : Double;
  CurGPNum : LongInt;
  CurParamNum : LongInt;
  EOC : Boolean;
  WaveLength : Double;
  SigmaVaryingFlag : Boolean;
  FSigma : Double;

  procedure   SetSigma(Value : Double);

  function    Func : Double;
  procedure   CalcFunc;
  function    GetStep : Double;
  procedure   SetStep(NewStepValue : Double);
  procedure   SetNextParam;
  procedure   SetFirstParam;
  function    GetParam : Double;
  procedure   SetParam(NewParamValue : Double);
  function    EndOfCycle : Boolean;
  procedure   ShowCurMin;
  procedure   DivideStepsBy2;
  function    EndOfCalculation : Boolean;
  procedure   DoneProc;

 protected
  FOnPlotGaussians : TPlotGaussians;
  FOnPlotSelectedPoints : TPlotSelectedPoints;
  FOnPlotNeutronPoints : TPlotNeutronPoints;
  FOnPlotSelectedArea : TPlotSelectedArea;
  FOnPlotBackground : TPlotBackground;
  FOnPlotGaussProfile : TPlotGaussProfile;
  FOnRefresh : TRefresh;
  FOnRefreshPointsSet : TRefreshPointsSet;
  FOnClear : TClear;
  FOnHide : THide;
  FOnViewResultedIntSet : TViewResultedIntSet;
  FGaussiansFindingDone : TGaussiansFindingDone;
 public
  IsGaussesFinding : Boolean;
  procedure   SetMainCalcProcess(AMainCalcProcess : TThread);

  procedure   SetNeutronPointsSet(ANeutronPointsSet : TNeutronPointsSet);
  procedure   SetNeutronPointsSetCopy(ANeutronPointsSet : TNeutronPointsSet);
  procedure   SetActivePointSet(AActivePointsSet : TNeutronPointsSet);

  function    GetResultedIntSet : TMSCRNeutronCompList;
  function    GetResultedIntSetCopy : TMSCRNeutronCompList;
  procedure   SetResultedIntSet(AResultedIntSet : TMSCRNeutronCompList);
  procedure   CreateResultedIntSet;

  function    GetBackgroundPointsSet : TNeutronPointsSet;
  function    GetNeutronPointsSet : TNeutronPointsSet;
  function    GetSelectedArea : TNeutronPointsSet;
  function    GetGaussProfile : TNeutronPointsSet;
  function    GetGaussPointsSetList : TComponentList;
  function    GetSelectedPoints : TNeutronPointsSet;
  function    GetActivePointsSet : TNeutronPointsSet;

  procedure   CreateBackgroundPointsSet;
  procedure   CreateGaussPointsList;
  procedure   CreateSelectedPoints;

  procedure   FindGausses;
  procedure   StopFindGausses;
  procedure   CreateMinimizer;
  procedure   Smoothing(ANeutronPointsSet : TNeutronPointsSet);
  procedure   BackgroundSubtraction;
  procedure   CalcGaussProfile;
  function    GetRFactor : Double;
  function    Integrate(ANeutronPoints : TNeutronPointsSet;
                        StartPointIndex,StopPointIndex : LongInt) : Double;
  function    IntegrateAll(ANeutronPoints : TNeutronPointsSet) : Double;
  procedure   AddToResultedIntSet(ANeutronPoints : TNeutronPointsSet;
                        StartPointIndex,PeakPointIndex,StopPointIndex : LongInt);
  procedure   AddGaussToResultedIntSet(ANeutronPoints : TNeutronPointsSet;
                        StartPointIndex,StopPointIndex : LongInt);
  procedure   SelectArea(ANeutronPoints : TNeutronPointsSet;
                        StartPointIndex,StopPointIndex : LongInt);
  procedure   ReturnToTotalProfile;
  procedure   SetWaveLength(AWaveLength : Double);
  function    GetWaveLength : Double;
  procedure   AddNewPointToPointsSet(ANeutronPointsSet : TNeutronPointsSet;XValue,YValue : Double);
  constructor Create(AOwner : TComponent);
  destructor  Destroy; override;
  property    Sigma : Double read FSigma write SetSigma;
  property    OnPlotGaussians : TPlotGaussians read FOnPlotGaussians write FOnPlotGaussians;
  property    OnPlotSelectedPoints : TPlotSelectedPoints read FOnPlotSelectedPoints write FOnPlotSelectedPoints;
  property    OnPlotNeutronPoints : TPlotNeutronPoints read FOnPlotNeutronPoints write FOnPlotNeutronPoints;
  property    OnPlotSelectedArea : TPlotSelectedArea read FOnPlotSelectedArea write FOnPlotSelectedArea;
  property    OnPlotBackground : TPlotBackground read FOnPlotBackground write FOnPlotBackground;
  property    OnPlotGaussProfile : TPlotGaussProfile read FOnPlotGaussProfile write FOnPlotGaussProfile;
  property    OnRefresh : TRefresh read FOnRefresh write FOnRefresh;
  property    OnRefreshPointsSet : TRefreshPointsSet read FOnRefreshPointsSet write FOnRefreshPointsSet;
  property    OnClear : TClear read FOnClear write FOnClear;
  property    OnHide : THide read FOnHide write FOnHide;
  property    OnViewResultedIntSet : TViewResultedIntSet read FOnViewResultedIntSet write FOnViewResultedIntSet;
  property    OnGaussiansFindingDone : TGaussiansFindingDone read FGaussiansFindingDone write FGaussiansFindingDone;
 end;

implementation

uses  MainCalcProcess;

{============================= TIntegralIntMaker ==============================}
procedure TIntegralIntMaker.SetNeutronPointsSet(ANeutronPointsSet : TNeutronPointsSet);
begin
 if Assigned(OnClear) then OnClear(Self);
 if not NeutronPointsSetOuter then NeutronPointsSet.Free;
 NeutronPointsSet := ANeutronPointsSet;
 NeutronPointsSetOuter := True;
 NeutronPointsSet.Lambda := WaveLength;
 if Assigned(OnPlotNeutronPoints) then OnPlotNeutronPoints(Self,NeutronPointsSet);
end;

procedure TIntegralIntMaker.SetNeutronPointsSetCopy(ANeutronPointsSet : TNeutronPointsSet);
begin
 if Assigned(OnClear) then OnClear(Self);
 if not NeutronPointsSetOuter then NeutronPointsSet.Free;
 NeutronPointsSet := ANeutronPointsSet.GetCopy;
 NeutronPointsSetOuter := False;
 NeutronPointsSet.Lambda := WaveLength;
 if Assigned(OnPlotNeutronPoints) then OnPlotNeutronPoints(Self,NeutronPointsSet);
end;

procedure TIntegralIntMaker.SetActivePointSet(AActivePointsSet : TNeutronPointsSet);
begin
 ActivePointsSet := AActivePointsSet;
end;

destructor TIntegralIntMaker.Destroy;
begin
 if not NeutronPointsSetOuter then NeutronPointsSet.Free;
 if not ResultedintSetOuter then ResultedIntSet.Free;
 BackgroundPointsSet.Free;
 SavedPointsSetList.Free;
 SelectedArea.Free;
 GaussPointsSetList.Free;
 GaussProfile.Free;
 Minimizer.Free;
 SelectedPoints.Free;
 inherited Destroy;
end;

constructor TIntegralIntMaker.Create(AOwner : TComponent);
begin
 inherited Create(AOwner);
 SavedPointsSetList := TComponentList.Create(nil);
 WaveLength := 0;
end;

procedure TIntegralIntMaker.Smoothing(ANeutronPointsSet : TNeutronPointsSet);
var i,j : LongInt;
    MeanValue : Double;
begin
 if not Assigned(ANeutronPointsSet) then
  begin
   raise EPointsSetIsNotAssigned.Create('Points set is not assigned...');
   Exit;
  end;
 with ANeutronPointsSet do
   begin
    for i := 1 to PointsCount - 2 do
     begin
      MeanValue := 0;
      MeanValue := PointIntensity[i + 1] - PointIntensity[i - 1];
      MeanValue := MeanValue / 2;
      MeanValue := PointIntensity[i - 1] + MeanValue;
      PointIntensity[i] := MeanValue;
     end;{for i := 0 to PointsCount - SmoothLength do...}
   end;{with NeutronPointsSet as TNeutronPointsSet do...}
 if Assigned(OnRefreshPointsSet) then OnRefreshPointsSet(Self,ANeutronPointsSet);
end;

procedure TIntegralIntMaker.CreateBackgroundPointsSet;
begin
 if Assigned(OnHide) then OnHide(Self,BackgroundPointsSet);
 BackgroundPointsSet.Free;
 BackgroundPointsSet := TNeutronPointsSet.Create(nil);
 BackgroundPointsSet.Lambda := WaveLength;
 if Assigned(OnPlotBackground) then OnPlotBackground(Self,BackgroundPointsSet);
end;

function TIntegralIntMaker.GetBackgroundPointsSet : TNeutronPointsSet;
begin
 if Assigned(BackgroundPointsSet) then Result := BackgroundPointsSet
  else raise EPointsSetIsNotAssigned.Create('Background points set is not assigned...');
end;

function TIntegralIntMaker.GetNeutronPointsSet : TNeutronPointsSet;
begin
 if Assigned(NeutronPointsSet) then Result := NeutronPointsSet
  else raise EPointsSetIsNotAssigned.Create('Neutron points set is not assigned...');
end;

function  TIntegralIntMaker.GetSelectedArea : TNeutronPointsSet;
begin
 if Assigned(SelectedArea) then Result := SelectedArea
  else raise EPointsSetIsNotAssigned.Create('Selected area points set is not assigned...');
end;

function  TIntegralIntMaker.GetGaussProfile : TNeutronPointsSet;
begin
 if Assigned(SelectedArea) then Result := GaussProfile
  else raise EPointsSetIsNotAssigned.Create('Gauss points set is not assigned...');
end;

function  TIntegralIntMaker.GetSelectedPoints : TNeutronPointsSet;
begin
 if Assigned(SelectedPoints) then Result := SelectedPoints
  else raise EPointsSetIsNotAssigned.Create('Selected points set is not assigned...');
end;

function  TIntegralIntMaker.GetActivePointsSet : TNeutronPointsSet;
begin
 if Assigned(ActivePointsSet) then Result := ActivePointsSet
  else raise EPointsSetIsNotAssigned.Create('Active points set is not assigned...');
end;

function  TIntegralIntMaker.GetGaussPointsSetList : TComponentList;
begin
 if Assigned(GaussPointsSetList) then Result := GaussPointsSetList
  else raise EPointsSetIsNotAssigned.Create('Gausses list is not assigned...');
end;

procedure TIntegralIntMaker.BackgroundSubtraction;
var SA : TNeutronPointsSet;
    i : LongInt;
    Delta : Double;
    TempDouble : Double;
begin
 try
  SA := GetSelectedArea;
 except raise; Exit end;
 if SA.PointsCount = 0 then
  begin
   raise EPointsSetIsEmpty.Create('Selected points set is empty...');
   Exit;
  end;
 with SA do
   begin
    Delta := (PointIntensity[PointsCount - 1] - PointIntensity[0]) /
             (PointXCoord[PointsCount - 1] - PointXCoord[0]);
    TempDouble := PointIntensity[0];
    for i := 0 to PointsCount - 1 do
     begin
      PointIntensity[i] := PointIntensity[i] -
       ((PointXCoord[i] - PointXCoord[0]) * Delta  +  TempDouble);
{      if PointIntensity[i] < 0 then PointIntensity[i] := 0;}
     end;
   end;
 if Assigned(OnRefresh) then OnRefresh(Self);
end;

procedure TIntegralIntMaker.CalcGaussProfile;
var i,j : LongInt;
    GP : TNeutronPointsSet;
    PS : TNeutronPointsSet;
begin
 try
  GP := GetGaussProfile;
 except raise; Exit end;
 if not Assigned(GaussPointsSetList) then
  begin
   raise EPointsSetIsNotAssigned.Create('List is not assigned...');
   Exit;
  end;
 for i := 0 to GaussPointsSetList.Count - 1 do
  begin
   PS := TNeutronPointsSet(GaussPointsSetList.Items[i]);
   if PS is TGaussPointsSet then
    with PS as TGaussPointsSet do ReCalc;
  end;
 for j := 0 to GP.PointsCount - 1 do GP.PointYCoord[j] := 0;
 for i := 0 to GaussPointsSetList.Count - 1 do
  begin
   PS := TNeutronPointsSet(GaussPointsSetList.Items[i]);
   for j := 0 to GP.PointsCount - 1 do
    GP.PointYCoord[j] := GP.PointYCoord[j] + PS.PointYCoord[j];
  end;
end;

procedure TIntegralIntMaker.SelectArea(ANeutronPoints : TNeutronPointsSet;
                                       StartPointIndex,StopPointIndex : LongInt);
var i : LongInt;
begin
 if not Assigned(ANeutronPoints) then
  begin
   raise EPointsSetIsNotAssigned.Create('Points set is not assigned...');
   Exit;
  end;
 if ANeutronPoints.PointsCount = 0 then
  begin
   raise EPointsSetIsEmpty.Create('Neutron points set is empty...');
   Exit;
  end;
 if (StartPointIndex < 0) or (StopPointIndex > ANeutronPoints.PointsCount - 1) then
  begin
   raise EPointIndexOutOfBounds.Create('Point index out of bounds...');
   Exit;
  end;
 if Assigned(OnClear) then OnClear(Self);
 SelectedArea.Free;
 SelectedArea := TNeutronPointsSet.Create(nil);
 SelectedArea.Lambda := WaveLength;
 for i := StartPointIndex to StopPointIndex do
  SelectedArea.AddNewPoint(ANeutronPoints.PointXCoord[i],ANeutronPoints.PointYCoord[i]);
 if Assigned(OnPlotSelectedArea) then OnPlotSelectedArea(Self,SelectedArea);
end;

procedure TIntegralIntMaker.ReturnToTotalProfile;
begin
 if not Assigned(NeutronPointsSet) then
  begin
   raise EPointsSetIsNotAssigned.Create('Points set is not assigned...');
   Exit;
  end;
 if Assigned(OnClear) then OnClear(Self);
 if Assigned(OnPlotNeutronPoints) then OnPlotNeutronPoints(Self,NeutronPointsSet);
end;

function TIntegralIntMaker.Integrate(ANeutronPoints : TNeutronPointsSet;
                      StartPointIndex,StopPointIndex : LongInt) : Double;
var i : LongInt;
    TempDouble : Double;
begin
 if not Assigned(ANeutronPoints) then
  begin
   raise EPointsSetIsNotAssigned.Create('Points set is not assigned...');
   Exit;
  end;
 if (StartPointIndex < 0) or (StopPointIndex > ANeutronPoints.PointsCount - 1) then
  begin
   raise EPointIndexOutOfBounds.Create('Point index out of bounds...');
   Exit;
  end;
 TempDouble := 0;
 with ANeutronPoints do
  for i := StartPointIndex to StopPointIndex do
   TempDouble := TempDouble + PointIntensity[i];
 Result := TempDouble;
end;

procedure TIntegralIntMaker.AddToResultedIntSet(ANeutronPoints : TNeutronPointsSet;
                                                StartPointIndex,PeakPointIndex,StopPointIndex : LongInt);
var NC : TMSCRNeutronClass;
    TempDouble : Double;
    RL : TMSCRNeutronCompList;
begin
 try
  RL := GetResultedIntSet;
 except raise; Exit end;
 try
  TempDouble := Integrate(ANeutronPoints,StartPointIndex,StopPointIndex);
 except raise; Exit end;
 NC := TMSCRNeutronClass.Create(nil);
 NC.StartPos := ANeutronPoints.PointXCoord[StartPointIndex];
 NC.PeakPos := ANeutronPoints.PointXCoord[PeakPointIndex];
 NC.FinishPos := ANeutronPoints.PointXCoord[StopPointIndex];
 NC.Intensity := TempDouble;
 RL.Add(NC);
 if Assigned(OnViewResultedIntSet) then OnViewResultedIntSet(Self,ResultedIntSet);
end;

procedure TIntegralIntMaker.AddGaussToResultedIntSet(ANeutronPoints : TNeutronPointsSet;
                                                     StartPointIndex,StopPointIndex : LongInt);
var NC : TMSCRNeutronClass;
    TempDouble : Double;
    RL : TMSCRNeutronCompList;
begin
 try
  RL := GetResultedIntSet;
 except raise; Exit end;
 try
  TempDouble := Integrate(ANeutronPoints,StartPointIndex,StopPointIndex);
 except raise; Exit end;
 if ANeutronPoints is TGaussPointsSet then
  with ANeutronPoints as TGaussPointsSet do
   begin
    NC := TMSCRNeutronClass.Create(nil);
    NC.StartPos := ANeutronPoints.PointXCoord[StartPointIndex];
    NC.PeakPos := x0;
    NC.FinishPos := ANeutronPoints.PointXCoord[StopPointIndex];
    NC.Intensity := TempDouble;
    RL.Add(NC);
    if Assigned(OnViewResultedIntSet) then OnViewResultedIntSet(Self,ResultedIntSet);
   end
 else raise EPointsSetIsNotGaussian.Create('Points set is not gaussian...');
end;

function TIntegralIntMaker.IntegrateAll(ANeutronPoints : TNeutronPointsSet) : Double;
var i : LongInt;
    TempDouble : Double;
begin
 if not Assigned(ANeutronPoints) then
  begin
   raise EPointsSetIsNotAssigned.Create('Points set is not assigned...');
   Exit;
  end;
 TempDouble := 0;
 with ANeutronPoints do
  for i := 0 to PointsCount - 1 do
   TempDouble := TempDouble + PointIntensity[i];
 Result := TempDouble;
end;

function TIntegralIntMaker.GetRFactor : Double;
var GP : TNeutronPointsSet;
    SA : TNeutronPointsSet;
    i : LongInt;
    RFactor : Double;
    SumExpInt : Double;
begin
 try
  GP := GetGaussProfile;
 except raise; Exit end;
 try
  SA := GetSelectedArea;
 except raise; Exit end;
 RFactor := 0;
 SumExpInt := 0;
 for i := 0 to GP.PointsCount - 1 do
  begin
   RFactor := RFactor + Sqr(GP.PointIntensity[i] - SA.PointIntensity[i]);
   SumExpInt := SumExpInt + Sqr(SA.PointIntensity[i]);
  end;
 if SumExpInt <> 0 then RFactor := RFactor / SumExpInt;
 Result := RFactor;
end;

procedure TIntegralIntMaker.CreateGaussPointsList;
var i,j : LongInt;
    SA,SP : TNeutronPointsSet;
    GP : TGaussPointsSet;
begin
 try
  SA := GetSelectedArea;
 except raise; Exit end;
 if Assigned(OnHide) then
  if Assigned(GaussPointsSetList) then
   for i := 0 to GaussPointsSetList.Count - 1 do
    OnHide(Self,TNeutronPointsSet(GaussPointsSetList.Items[i]));
 GaussPointsSetList.Free;
 GaussPointsSetList := TComponentList.Create(nil);
 if Assigned(OnHide) then OnHide(Self,GaussProfile);
 GaussProfile.Free;
 GaussProfile := TNeutronPointsSet.Create(nil);
 GaussProfile.Lambda := WaveLength;
 for i := 0 to SA.PointsCount - 1 do GaussProfile.AddNewPoint(SA.PointXCoord[i],0);
 try
  SP := GetSelectedPoints
 except raise; Exit end;
 if Assigned(OnHide) then OnHide(Self,SP);
 for i := 0 to SP.PointsCount - 1 do
  begin
   GP := TGaussPointsSet.Create(nil);
   for j := 0 to SA.PointsCount - 1 do GP.AddNewPoint(SA.PointXCoord[j],0);
   GP.A := 0.625 * SP.PointIntensity[i];
   GP.x0 := SP.PointXCoord[i];
   GP.Lambda := WaveLength;
   GaussPointsSetList.Add(GP);
  end;
 Sigma := 0.25;
 if Assigned(OnPlotGaussians) then OnPlotGaussians(Self,GaussPointsSetList);
 if Assigned(OnPlotGaussProfile) then OnPlotGaussProfile(Self,GaussProfile);
end;

procedure TIntegralIntMaker.CreateSelectedPoints;
begin
 if Assigned(OnHide) then OnHide(Self,SelectedPoints);
 SelectedPoints.Free;
 SelectedPoints := TNeutronPointsSet.Create(nil);
 SelectedPoints.Lambda := WaveLength;
 if Assigned(OnPlotSelectedPoints) then OnPlotSelectedPoints(Self,SelectedPoints);
end;

procedure TIntegralIntMaker.CreateMinimizer;
begin
 Minimizer.Free;
 Minimizer := TSimpleMinimizer2.Create(nil);
 Minimizer.OnFunc := Func;
 Minimizer.OnCalcFunc := CalcFunc;
 Minimizer.OnGetStep := GetStep;
 Minimizer.OnSetStep := SetStep;
 Minimizer.OnSetNextParam := SetNextParam;
 Minimizer.OnSetFirstParam := SetFirstParam;
 Minimizer.OnGetParam := GetParam;
 Minimizer.OnSetParam := SetParam;
 Minimizer.OnEndOfCycle := EndOfCycle;
 if Assigned(MainCalcProcess) then
  if MainCalcProcess is TMainCalcProcess then
   with MainCalcProcess as TMainCalcProcess do
    begin
     Minimizer.OnShowCurMin := ShowResults;
     SetShowResultsProc(ShowCurMin);
     SetDoneProc(DoneProc);
    end;
 Minimizer.DivideStepsBy2 := DivideStepsBy2;
 Minimizer.EndOfCalculation := EndOfCalculation;
 EOC := False;
 AStep := 100{1};
 SigmaStep := 0.1{0.01};
 x0Step := 0.01;
 CurParamNum := 0;
 CurGPNum := 0;
end;

function TIntegralIntMaker.Func : Double;
begin
 Result := GetRFactor;
end;

procedure TIntegralIntMaker.CalcFunc;
begin
 CalcGaussProfile;
end;

function TIntegralIntMaker.GetStep : Double;
begin
 if SigmaVaryingFlag then Result := SigmaStep
  else
   case CurParamNum of
    0 : Result := AStep;
    1 : Result := x0Step;
    2 : Result := SigmaStep;
   end;
end;

procedure TIntegralIntMaker.SetStep(NewStepValue : Double);
begin
end;

procedure TIntegralIntMaker.SetNextParam;
var GL : TComponentList;
    GP : TGaussPointsSet;
begin
 try
  GL := GetGaussPointsSetList;
 except raise; Exit end;
 if GL.Count = 0 then
  begin
   raise EListIsEmpty.Create('Gausses list is empty...');
   Exit;
  end;
 EOC := True;
 GP := TGaussPointsSet(GL.Items[CurGPNum]);
 if CurParamNum < GP.ParamCount - 1 then
  begin
   Inc(CurParamNum);
   EOC := False;
   Exit;
  end;
 if CurGPNum < GL.Count - 1 then
  begin
   Inc(CurGPNum);
   EOC := False;
   CurParamNum := 0;
   Exit;
  end;
 if not SigmaVaryingFlag then
  begin
   SigmaVaryingFlag := True;
   EOC := False;
   Exit;
  end;
end;

procedure TIntegralIntMaker.SetFirstParam;
var GL : TComponentList;
begin
 try
  GL := GetGaussPointsSetList;
 except raise; Exit end;
 if GL.Count = 0 then
  begin
   raise EListIsEmpty.Create('Gausses list is empty...');
   Exit;
  end;
 CurGPNum := 0;
 CurParamNum := 0;
 EOC := False;
 SigmaVaryingFlag := False;
end;

function TIntegralIntMaker.GetParam : Double;
var GP : TGaussPointsSet;
    GL : TComponentList;
begin
 try
  GL := GetGaussPointsSetList;
 except raise; Exit end;
 if GL.Count = 0 then
  begin
   raise EListIsEmpty.Create('Gausses list is empty...');
   Exit;
  end;
 if SigmaVaryingFlag then Result := Sigma
  else
   begin
    GP := TGaussPointsSet(GL.Items[CurGPNum]);
    Result := GP.Param[CurParamNum];
   end;
end;

procedure TIntegralIntMaker.SetParam(NewParamValue : Double);
var GP : TGaussPointsSet;
    GL : TComponentList;
begin
 try
  GL := GetGaussPointsSetList;
 except raise; Exit end;
 if GL.Count = 0 then
  begin
   raise EListIsEmpty.Create('Gausses list is empty...');
   Exit;
  end;
 if SigmaVaryingFlag then Sigma := NewParamValue
  else
   begin
    GP := TGaussPointsSet(GL.Items[CurGPNum]);
    GP.Param[CurParamNum] := NewParamValue;
   end;
end;

function TIntegralIntMaker.EndOfCycle : Boolean;
begin
 Result := EOC
end;

procedure TIntegralIntMaker.ShowCurMin;
begin
 if Assigned(OnRefresh) then OnRefresh(Self);
end;

procedure TIntegralIntMaker.DivideStepsBy2;
begin
 AStep := AStep / 2;
 SigmaStep := SigmaStep / 2;
 x0Step := x0Step / 2;
end;

function  TIntegralIntMaker.EndOfCalculation : Boolean;
begin
 if AStep < 1 then Result := True else Result := False;
end;

procedure TIntegralIntMaker.FindGausses;
var ErrorCode : LongInt;
begin
 CreateMinimizer;
 IsGaussesFinding := True;
 Minimizer.Minimize(ErrorCode);
 Minimizer.Free;
 Minimizer := nil;
 IsGaussesFinding := False;
end;

procedure TIntegralIntMaker.StopFindGausses;
begin
 if Assigned(Minimizer) then Minimizer.Terminated := True;
end;

procedure TIntegralIntMaker.SetMainCalcProcess(AMainCalcProcess : TThread);
begin
 MainCalcProcess := AMainCalcProcess;
end;

function  TIntegralIntMaker.GetResultedIntSet : TMSCRNeutronCompList;
begin
 if not Assigned(ResultedIntSet) then
  begin
   raise EResultedIntListIsNotAssigned.Create('Resulted intensities list is not assigned...');
   Result := nil;
   Exit;
  end;
 Result := ResultedIntSet;
end;

function TIntegralIntMaker.GetResultedIntSetCopy : TMSCRNeutronCompList;
begin
 if not Assigned(ResultedIntSet) then
  begin
   raise EResultedIntListIsNotAssigned.Create('Resulted intensities list is not assigned...');
   Result := nil;
   Exit;
  end;
 Result := TMSCRNeutronCompList(ResultedIntSet.GetCopy);
end;

procedure TIntegralIntMaker.CreateResultedIntSet;
begin
 if not ResultedIntSetOuter then ResultedIntSet.Free;
 ResultedIntSet := TMSCRNeutronCompList.Create(nil);
 ResultedIntSetOuter := False;
 ResultedIntSet.Lambda := WaveLength;
 if Assigned(OnViewResultedIntSet) then OnViewResultedIntSet(Self,ResultedIntSet);
end;

procedure TIntegralIntMaker.SetResultedIntSet(AResultedIntSet : TMSCRNeutronCompList);
begin
 if not ResultedIntSetOuter then ResultedIntSet.Free;
 ResultedIntSet := AResultedIntSet;
 ResultedIntSetOuter := True;
 ResultedIntSet.Lambda := WaveLength; 
 if Assigned(OnViewResultedIntSet) then OnViewResultedIntSet(Self,ResultedIntSet);
end;

procedure TIntegralIntMaker.SetSigma(Value : Double);
var i : LongInt;
    GP : TNeutronPointsSet;
begin
 FSigma := Value;
 if Assigned(GaussPointsSetList) then
  with GaussPointsSetList do
   for i := 0 to Count - 1 do
    begin
     GP := TNeutronPointsSet(Items[i]);
     if GP is TGaussPointsSet then
      TGaussPointsSet(GP).Sigma := Sigma;
    end
 else raise EPointsSetIsNotAssigned.Create('Gausses list is not assigned...');
end;

procedure TIntegralIntMaker.SetWaveLength(AWaveLength : Double);
var i : LongInt;
    NS : TNeutronPointsSet;
begin
 WaveLength := AWaveLength;
 if Assigned(NeutronPointsSet) then NeutronPointsSet.Lambda := AWaveLength;
 if Assigned(BackgroundPointsSet) then BackgroundPointsSet.Lambda := AWaveLength;
 if Assigned(SelectedArea) then SelectedArea.Lambda := AWaveLength;
 if Assigned(SelectedPoints) then SelectedPoints.Lambda := AWaveLength;
 if Assigned(GaussProfile) then GaussProfile.Lambda := AWaveLength;
 if Assigned(GaussPointsSetList) then
  with GaussPointsSetList do
   for i := 0 to GaussPointsSetList.Count - 1 do
    begin
     NS := TNeutronPointsSet(GaussPointsSetList.Items[i]);
     NS.Lambda := AWaveLength;
    end;
 if Assigned(ResultedIntSet) then ResultedIntSet.Lambda := WaveLength;
end;

function  TIntegralIntMaker.GetWaveLength : Double;
begin
 Result := WaveLength;
end;

procedure TIntegralIntMaker.AddNewPointToPointsSet(ANeutronPointsSet : TNeutronPointsSet;XValue,YValue : Double);
begin
 if not Assigned(ANeutronPointsSet) then
  begin
   raise EPointsSetIsNotAssigned.Create('Points set is not assigned...');
   Exit;
  end;
 ANeutronPointsSet.AddNewPoint(XValue,YValue);
 if Assigned(OnRefreshPointsSet) then OnRefreshPointsSet(Self,ANeutronPointsSet);
end;

procedure TIntegralIntMaker.DoneProc;
begin
 if Assigned(OnGaussiansFindingDone) then OnGaussiansFindingDone(Self);
end; 

end.
