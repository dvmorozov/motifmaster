{------------------------------------------------------------------------------
    This file is part of the MotifMASTER project. This software is
    distributed under GPL (see gpl.txt for details).

    This software is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Copyright (C) 1999-2007 D.Morozov (dvmorozov@mail.ru)
------------------------------------------------------------------------------}

unit CalcModule;

interface

uses
    Classes, Dialogs, SysUtils, StdCtrls, FFDataFile, SimpMath, DataClasses,
    ComponentList, DownhillSimplexContainer, AlgorithmContainer, Tools,
    SelfSaved, Math;

const
    PVM_DEBYE_WALLER        = 1;
    PVM_INT_NORMALIZATION   = 2;

type
    TWaveVectorArray = array of TDoubleVector3;

    ECalcModule = class(Exception);

    TCalcModule = class(TComponent, IOptimizedFunction, IDownhillRealParameters)
        protected
        FMulIntConst: Double;
        FLinearSumExpInt, FSquareLawSumExpInt: Double;
        FLinearRFactor, FSquareLawRFactor: Double;
        FZeroRFactor, FNonZeroRFactor,
        FLinNonZeroRFactor, FLinZeroRFactor: Double;
        FSumZeroInt, FSumNonZeroInt: Double;
        FMaxCalcExpRelation: Double;
        FMaxExpValue: Double; FMaxExpIndex: LongInt;
        FMaxCalcLinValue: Double; FMaxCalcLinIndex: LongInt;
        FMaxZeroLinValue: Double; FMaxZeroLinIndex: LongInt;
        FMaxNonZeroLinValue: Double; FMaxNonZeroLinIndex: LongInt;
        FZeroHKLNumber, FNonZeroHKLNumber: LongInt;
            {  !!! takes into account only magnetic intensity !!! }

        FVariationMode: ShortInt;
        FIFType: ShortInt;

        FSiteList: TSiteList;
        FPatternParams: TComponent;
        HKLList: THKLList;
        PatternAuxiliary: TPatternAuxiliary;
        SinTList: TSinTCompList;
            {  the list contains data about experimental and calculated intensities }

        ViewSinTListReady: Boolean;
        ViewStructListReady: Boolean;

        FContainer: TAlgorithmContainer;

        NuclIntCalculated: Boolean;
            procedure SetMulIntConst(const AMulIntConst: Double);
            function GetParametersNumber: LongInt;
        function GetParameter(index: LongInt): TVariableParameter;
        procedure SetParameter(index: LongInt; AParameter: TVariableParameter);

        function GetNumberOfValues: LongInt;
        function GetValueIndex: LongInt;
        procedure SetValueIndex(const AValueIndex: LongInt);

        function GetSiteList: TSiteList;
        function GetPatternParams: TComponent;
        procedure SetContainer(const AContainer: TAlgorithmContainer);

        procedure CreateSinTList(const PatternAuxiliary: TPatternAuxiliary);
        procedure LinkSinTListWithStars;        
        procedure AddToSinTList;

        function GetNewSinTClass(TH: THKLClass): TSinTClass;
        function GetSinTItem(HR: THKLClass): TSinTClass;

        procedure StandardCalc; virtual;

    public
        ViewSiteList: TSiteList;
            {  the list contains resulting structure }
        ViewSinTList: TSinTCompList;
            {  the list contains resulting intensities }
        TotalRFactor: Double;
        LoopCounter: LongInt;
        RFactorMode: LongInt;           constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;

        procedure CreateParameters;     procedure ParametersUpdated;    function GetMaxPossibleSinTL: Double;
        procedure GetMaxIndexes(out MaxExpIndex, MaxCalcIndex: LongInt);
        procedure CalcRFactors;
        procedure CalcZeroNonZeroHKLNumbers;
        procedure PrepareDataToCalculation; virtual;
        procedure RemoveAllEmptySinTItems;
        procedure CalcExpSums;
            procedure MoveViewSinTListData;
        procedure MoveViewStructListData;
        function GetOptimizedFunction: Double;  procedure RunAlgorithm;

        procedure SetSiteList(const ASiteList: TSiteList);
        procedure SetPatternParams(const APatternParams: TComponent);

        property LinearSumExpInt: Double        read FLinearSumExpInt;
        property SquareLawSumExpInt: Double     read FSquareLawSumExpInt;
        property LinearRFactor: Double          read FLinearRFactor;
        property SquareLawRFactor: Double       read FSquareLawRFactor;
        property ZeroRFactor: Double            read FZeroRFactor;
        property LinZeroRFactor: Double         read FLinZeroRFactor;
        property LinNonZeroRFactor: Double      read FLinNonZeroRFactor;
        property SumZeroInt: Double             read FSumZeroInt;
        property SumNonZeroInt: Double          read FSumNonZeroInt;
        property NonZeroRFactor: Double         read FNonZeroRFactor;
        property MaxExpValue: Double            read FMaxExpValue;
        property MaxExpIndex: LongInt           read FMaxExpIndex;
        property MaxCalcLinValue: Double        read FMaxCalcLinValue;
        property MaxCalcLinIndex: LongInt       read FMaxCalcLinIndex;
        property MaxZeroLinValue: Double        read FMaxZeroLinValue;
        property MaxZeroLinIndex: LongInt       read FMaxZeroLinIndex;
        property MaxNonZeroLinValue: Double     read FMaxNonZeroLinValue;
        property MaxNonZeroLinIndex: LongInt    read FMaxNonZeroLinIndex;
        property MaxCalcExpRelation: Double     read FMaxCalcExpRelation;
        property MulIntConst: Double
            read FMulIntConst                   write SetMulIntConst;
        property ZeroHKLNumber: LongInt         read FZeroHKLNumber;
        property NonZeroHKLNumber: LongInt      read FNonZeroHKLNumber;

        property ParametersNumber: LongInt      read GetParametersNumber;
                {  the number of parameters to optimize }
        property Parameter[index: LongInt]: TVariableParameter
                read GetParameter               write SetParameter;

        property NumberOfValues: LongInt        read GetNumberOfValues;
                {  the number of discrete values of parameter }
        property ValueIndex: LongInt
                {  the index of discrete value currently selected }
                read GetValueIndex              write SetValueIndex;

        property VariationMode: ShortInt
                read FVariationMode             write FVariationMode;
        property IFType: ShortInt
                read FIFType                    write FIFType;

        property Container: TAlgorithmContainer
                read FContainer                 write SetContainer;
    end;

{$DEFINE FITTING}
implementation

function TCalcModule.GetNewSinTClass(TH: THKLClass): TSinTClass;
begin
    Result := CreateNewSinTClass;
    Result.HKLList.SetState(cfPassive);
    Result.SinT := TH.SinT;
    Result.SinTL := TH.SinTL;
    Result.Sin2T := TH.Sin2T;
    SinTList.Add(Result);
end;

function TCalcModule.GetSinTItem(HR: THKLClass): TSinTClass;
    function IsItMyPeak(
        const TR: TSinTClass;
        const HR: THKLClass
        ): Boolean;
    const TINY = 1e-6;
    begin
        if Abs(TR.SinTL - HR.SinTL) < TINY then Result := True
        else
            if TR.ExpIntensity = 0 then Result := False
            else
                if (HR.SinTL >= TR.StartPos) and (HR.SinTL <= TR.FinishPos) then
                    Result := True
                else Result := False;
    end;

var i: LongInt;
    LeftNearest, RightNearest, TR: TSinTClass;
    FallInLeft, FallInRight: Boolean;
    SaveDeltaSinLeft, SaveDeltaSinRight: Double;
begin
    if SinTList.Count <> 0 then
    begin
        SaveDeltaSinLeft := 1; SaveDeltaSinRight := 1;
        LeftNearest := nil; RightNearest := nil;
        for i := 0 to SinTList.Count - 1 do
        begin
            TR := TSinTClass(SinTList.Items[i]);
            if (Abs(TR.SinT - HR.SinT) < SaveDeltaSinLeft) and
               (HR.SinT >= TR.SinT) then
            begin
                SaveDeltaSinLeft := Abs(TR.SinT - HR.SinT);
                LeftNearest := TR;
            end;
            if (Abs(TR.SinT - HR.SinT) < SaveDeltaSinRight) and
               (HR.SinT < TR.SinT) then
            begin
                SaveDeltaSinRight := Abs(TR.SinT - HR.SinT);
                RightNearest := TR;
            end;
        end;

        if Assigned(LeftNearest) then FallInLeft := IsItMyPeak(LeftNearest, HR)
        else FallInLeft := False;

        if Assigned(RightNearest) then FallInRight := IsItMyPeak(RightNearest, HR)
        else FallInRight := False;

        if (not FallInLeft) and (not FallInRight) then Result := GetNewSinTClass(HR);
        if FallInLeft and (not FallInRight) then Result := LeftNearest;
        if (not FallInLeft) and FallInRight then Result := RightNearest;
        if FallInLeft and FallInRight then
            if Abs(LeftNearest.SinTL - HR.SinTL) <= Abs(RightNearest.SinTL - HR.SinTL) then
                Result := LeftNearest else Result := RightNearest;
    end else Result := GetNewSinTClass(HR);
end;

procedure TCalcModule.AddToSinTList;
var i: LongInt;
    HR: THKLClass;
    TR: TSinTClass;
begin
    for i := 0 to HKLList.Count - 1 do
    begin
        HR := THKLClass(HKLList.Items[i]);
        TR := GetSinTItem(HR);
        TR.HKLList.Add(HR);
    end;

    SinTList.Sort(ViewNeutronListSort);

    for i := 0 to SinTList.Count - 1 do
    begin
        TR := TSinTClass(SinTList.Items[i]);
        TR.HKLList.Sort(HKLListSortFunc);
    end;
end;

procedure TCalcModule.CreateSinTList(
    const PatternAuxiliary: TPatternAuxiliary);
    var i: LongInt;
    NR: TNeutronClass;
    NRMono: TNeutronClassMono;
    ST: TSinTClass;
    PatternParams: TComponent;
    TempSinTL: Double;
begin
    SinTList.Clear;
    PatternParams := GetPatternParams;
    if (PatternParams is TNeutronCompList) and not
       (PatternParams is TNeutronCompListMono) then
        with PatternParams as TNeutronCompList do
        begin
            SinTList.Capacity := Count;
                for i := 0 to Count - 1 do
            begin
                NR := TNeutronClass(Items[i]);
                if NR.FinishPos <= GetMaxPossibleSinTL then
                    begin
                    ST := CreateNewSinTClass;
                    ST.HKLList.SetState(cfPassive);
                    ST.ExpIntensity := NR.Intensity;
                    ST.SinT := NR.PeakPos * Lambda;
                    ST.SinTL := NR.PeakPos;
                    ST.Sin2T := Sin(2 * Arctan(Sqrt(Sqr(ST.SinT)/(1 - Sqr(ST.SinT)))));
                    ST.StartPos := NR.StartPos;
                    ST.FinishPos := NR.FinishPos;
                    ST.PeakPos := NR.PeakPos;
                    ST.IntCorrFactor := NR.IntCorrFactor;
                        SinTList.Add(ST);
                end;    end;    Exit;
        end;

    if PatternParams is TNeutronCompListMono then
        with PatternParams as TNeutronCompListMono do
        begin
            SinTList.Capacity := Count;
                for i := 0 to Count - 1 do
            begin
                NRMono := TNeutronClassMono(Items[i]);
                TempSinTL := PatternAuxiliary.GetSinTL(
                    NRMono.H, NRMono.K, NRMono.L);
                if  TempSinTL <= GetMaxPossibleSinTL then
                    begin
                    ST := CreateNewSinTClass;
                    ST.HKLList.SetState(cfPassive);
                    ST.ExpIntensity := NRMono.Intensity;
                    ST.SinT := TempSinTL * Lambda;
                    ST.SinTL := TempSinTL;
                    ST.Sin2T := Sin(2 * Arctan(Sqrt(Sqr(ST.SinT)/(1 - Sqr(ST.SinT)))));
                    ST.StartPos := TempSinTL;
                    ST.FinishPos := TempSinTL;
                    ST.PeakPos := TempSinTL;
                    ST.IntCorrFactor := NRMono.IntCorrFactor;
                        SinTList.Add(ST);
                end;    end;    end;
end;

procedure TCalcModule.CalcRFactors;
var i: LongInt;
    TR: TSinTClass;
    TempDelta: Double;
begin
    FLinearRFactor := 0; FSquareLawRFactor := 0;
    FZeroRFactor := 0; FNonZeroRFactor := 0;
    FLinZeroRFactor := 0; FLinNonZeroRFactor := 0;
    FMaxCalcLinValue := 0; FMaxCalcLinIndex := -1;
    FMaxZeroLinValue := 0; FMaxZeroLinIndex := -1;
    FMaxNonZeroLinValue := 0; FMaxNonZeroLinIndex := -1;
    FMaxCalcExpRelation := 0;
    FSumZeroInt := 0; FSumNonZeroInt := 0;
    with SinTList do
        for i := 0 to Count - 1 do
        begin
            TR := TSinTClass(Items[i]);

            case RFactorMode of
                0 : begin
                    if TR.ExpIntensity = 0 then
                        FSumZeroInt := FSumZeroInt + TR.Intensity + TR.NuclearIntensity
                    else
                        FSumNonZeroInt := FSumNonZeroInt + TR.Intensity +
                            TR.NuclearIntensity;

                    TempDelta := (TR.Intensity + TR.NuclearIntensity -
                        TR.ExpIntensity);

                    if TR.Intensity > FMaxCalcLinValue then
                    begin
                        FMaxCalcLinValue := TR.Intensity;
                        FMaxCalcLinIndex := i;
                    end;

                    if TR.ExpIntensity = 0 then
                        if TR.Intensity > FMaxZeroLinValue then
                        begin
                            FMaxZeroLinValue := TR.Intensity;
                            FMaxZeroLinIndex := i;
                        end;

                    if TR.ExpIntensity <> 0 then
                        if TR.Intensity > FMaxNonZeroLinValue then
                        begin
                            FMaxNonZeroLinValue := TR.Intensity;
                            FMaxNonZeroLinIndex := i;
                        end;
                end;
                1 : begin
                    if TR.ExpIntensity = 0 then
                        FSumZeroInt := FSumZeroInt + (TR.Intensity + TR.NuclearIntensity)
                            / GetIntegralFactor(TR.SinT, TR.Sin2T)
                    else
                        FSumNonZeroInt := FSumNonZeroInt + (TR.Intensity +
                            TR.NuclearIntensity) / GetIntegralFactor(TR.SinT, TR.Sin2T);

                    TempDelta := (TR.Intensity + TR.NuclearIntensity -
                        TR.ExpIntensity) / GetIntegralFactor(TR.SinT, TR.Sin2T);

                    if TR.Intensity / GetIntegralFactor(TR.SinT, TR.Sin2T)
                        > FMaxCalcLinValue then
                    begin
                        FMaxCalcLinValue := TR.Intensity /
                        GetIntegralFactor(TR.SinT, TR.Sin2T);
                        FMaxCalcLinIndex := i;
                    end;

                    if TR.ExpIntensity = 0 then
                        if TR.Intensity / GetIntegralFactor(TR.SinT, TR.Sin2T) >
                            FMaxZeroLinValue then
                        begin
                            FMaxZeroLinValue := TR.Intensity /
                            GetIntegralFactor(TR.SinT, TR.Sin2T);
                            FMaxZeroLinIndex := i;
                        end;

                    if TR.ExpIntensity <> 0 then
                        if TR.Intensity / GetIntegralFactor(TR.SinT, TR.Sin2T) >
                            FMaxNonZeroLinValue then
                        begin
                            FMaxNonZeroLinValue := TR.Intensity /
                            GetIntegralFactor(TR.SinT, TR.Sin2T);
                            FMaxNonZeroLinIndex := i;
                        end;
                end;
            end;
            if TR.ExpIntensity <> 0 then
                if TR.Intensity / TR.ExpIntensity > FMaxCalcExpRelation then
                    FMaxCalcExpRelation := TR.Intensity / TR.ExpIntensity;

            if SquareLawSumExpInt <> 0 then
                TR.RFactor := Sqr(TempDelta) * 100{%} / SquareLawSumExpInt;
            FSquareLawRFactor := FSquareLawRFactor + Sqr(TempDelta);
            FLinearRFactor := FLinearRFactor + Abs(TempDelta);
            if TR.ExpIntensity = 0 then FLinZeroRFactor := FLinZeroRFactor + TempDelta
                else FLinNonZeroRFactor := FLinNonZeroRFactor + Abs(TempDelta);
            if TR.ExpIntensity = 0 then FZeroRFactor := FZeroRFactor + TR.RFactor
                else FNonZeroRFactor := FNonZeroRFactor + TR.RFactor;
        end;
    if LinearSumExpInt <> 0 then begin
        FLinearRFactor := FLinearRFactor / LinearSumExpInt * 100{%};
        FLinZeroRFactor := FLinZeroRFactor / LinearSumExpInt * 100{%};
        FLinNonZeroRFactor := FLinNonZeroRFactor / LinearSumExpInt * 100{%};
        FSquareLawRFactor := FSquareLawRFactor / SquareLawSumExpInt * 100{%};
    end;
end;

procedure TCalcModule.StandardCalc;

    function GetNewMulIntFactor: Double;
    var i: LongInt;
        TS: TSinTClass;
        SumExp, SumCalc: Double;
    begin
        SumExp := 0; SumCalc := 0;
        for i := 0 to SinTList.Count - 1 do
        begin
            TS := TSinTClass(SinTList.Items[i]);
            if TS.ExpIntensity <> 0 then
            begin
                SumExp := SumExp + TS.ExpIntensity;
                SumCalc := SumCalc + TS.PureMagnInt + TS.PureNuclInt;
            end;
        end;
        if SumCalc <> 0 then Result := SumExp / SumCalc else Result := 1;
    end;

    function GetRelativeRFact: Double;
    var i: LongInt;
        TS: TSinTClass;
        MaxExp, MaxCalc: Double;
    begin
        MaxExp := 0; MaxCalc := 0;
        for i := 0 to SinTList.Count - 1 do
        begin
            TS := TSinTClass(SinTList.Items[i]);
            if TS.ExpIntensity > MaxExp then MaxExp := TS.ExpIntensity;
            if (TS.PureMagnInt + TS.PureNuclInt) > MaxCalc then
                MaxCalc := TS.PureMagnInt + TS.PureNuclInt;
        end;

        Result := 0;
        for i := 0 to SinTList.Count - 1 do
        begin
            TS := TSinTClass(SinTList.Items[i]);
            Result := Result + Abs((TS.PureMagnInt + TS.PureNuclInt) / MaxCalc -
            TS.ExpIntensity / MaxExp) * 100;
        end;
    end;

    function GetLorentzRFact: Double;
    var i: LongInt;
        TS: TSinTClass;
        z: Double;
    begin
        Result := 0;
        for i := 0 to SinTList.Count - 1 do
        begin
            TS := TSinTClass(SinTList.Items[i]);
            with TS do
            begin
                z := Intensity + NuclearIntensity - ExpIntensity;
                Result := Result + Log10(1 + 1/2 * Sqr(z));
            end;
        end;
    end;

begin
    if not NuclIntCalculated then
    begin
        HKLList.CalcNuclearIntensities;
        NuclIntCalculated := True;
    end;

    HKLList.CalcMagneticIntensities;

    SinTList.CalcSummaryIntensities;

    {$IFDEF FITTING}
    if (VariationMode and PVM_INT_NORMALIZATION <> 0) then
        SinTList.MulIntParam := GetNewMulIntFactor;
    {$ENDIF}

    SinTList.CalcResultingIntensities;

    CalcRFactors;

    TotalRFactor := LinearRFactor;

    if SinTList.GetCalcResults <> nil then
        with SinTList.GetCalcResults as TCalcResults do
        begin
            RFactor1 := SquareLawRFactor;
            RFactor2 := TotalRFactor;
        end;

    if GetSiteList.GetCalcResults <> nil then
        with GetSiteList.GetCalcResults as TCalcResults do
        begin
            RFactor1 := SquareLawRFactor;
            RFactor2 := TotalRFactor;
        end;
end;

function TCalcModule.GetOptimizedFunction: Double;
begin
    StandardCalc;
    Result := TotalRFactor;
end;

procedure TCalcModule.RemoveAllEmptySinTItems;
var i: LongInt;
    TR: TSinTClass;
begin
    i := 0;
    while i <= SinTList.Count - 1 do
    begin
        TR := TSinTClass(SinTList.Items[i]);
        if TR.HKLList.Count = 0 then SinTList.Remove(TR) else Inc(i);
    end;
end;

constructor TCalcModule.Create;
begin
    inherited;
    SinTList := CreateNewSinTCompList;
    HKLList := THKLList.Create(nil);
    PatternAuxiliary := TPatternAuxiliary.Create;
end;

destructor TCalcModule.Destroy;
begin
    UtilizeObject(FContainer);
    UtilizeObject(FSiteList);
    UtilizeObject(FPatternParams);
    UtilizeObject(HKLList);
    UtilizeObject(PatternAuxiliary);
    UtilizeObject(SinTList);
end;

procedure TCalcModule.SetContainer(const AContainer: TAlgorithmContainer);
begin
    if AContainer is TDownhillSimplexContainer then
    begin
        if Assigned(FContainer) then UtilizeObject(FContainer);
        FContainer := AContainer;
        with AContainer as TDownhillSimplexContainer do OptimizedFunction := Self;
    end else raise ECalcModule.Create('Unknown container type...');
end;

procedure TCalcModule.RunAlgorithm;
var i: LongInt;
    TS: TSite;
    ByOneUsed: Boolean;
begin
    with Container as TDownhillSimplexContainer do
    begin
        ByOneUsed := False;
        for i := 0 to GetSiteList.Count - 1 do
        begin
            TS := TSite(GetSiteList.Items[i]);
            if TS.ReprMode = RM_BY_ONE then ByOneUsed := True;

            if not TS.Disabled then AddIDSPToList(TS);
        end;
        AddIDSPToList(Self);
        if ByOneUsed then
        begin
            FinalTolerance := 0.0000001;
            RestartDisabled := False;
            ExitDerivative := 0.00001;
        end else
        begin
            FinalTolerance := 0.0000001;
            RestartDisabled := False;
            ExitDerivative := 0.00001;
        end;
        Run;
    end;
end;

procedure TCalcModule.PrepareDataToCalculation;
var PatternParams: TComponent;
    IPPC: IPatternParametersContainer;
    MaxSinTL: Double;
begin
    if not GetFFArrays(GetSiteList) then
        raise ECalcModule.Create('Error in form - factors...');
    LinkSinTListWithStars;
    PatternParams := GetPatternParams;

    if PatternParams.GetInterface(PatternParamsContainerGUID, IPPC) then
        with IPPC do
        begin
            HKLList.Clear;

            with GetSiteList do
            begin
                PatternAuxiliary.A := A;
                PatternAuxiliary.B := B;
                PatternAuxiliary.C := C;
                PatternAuxiliary.Alpha := Alpha;
                PatternAuxiliary.Beta := Beta;
                PatternAuxiliary.Gamma := Gamma;

                SinTList.A := A;
                SinTList.B := B;
                SinTList.C := C;
                SinTList.Alpha := Alpha;
                SinTList.Beta := Beta;
                SinTList.Gamma := Gamma;
            end;

            SinTList.Lambda := Lambda;
            PatternAuxiliary.Lambda := Lambda;

            PatternAuxiliary.PrepareToCalc;
            if PatternParams is TNeutronCompListMono then
            begin
                with TNeutronCompListMono(PatternParams) do
                    if UseStartEnd then
                    begin
                        MaxSinTL := Min(EndPos, GetMaxPossibleSinTL);
                        CreateHKLList(PatternAuxiliary, HKLList, StartPos, MaxSinTL);
                    end else
                    begin
                        MaxSinTL := Min(GetMaxNeutronSinTL(PatternAuxiliary),
                            GetMaxPossibleSinTL);
                        CreateHKLList(PatternAuxiliary, HKLList, 0, MaxSinTL);
                    end;
                GetSiteList.LinkWithHKLList(HKLList);
            end else
            begin
                with GetSiteList do
                    if UseStartEnd then
                    begin
                        MaxSinTL := Min(EndPos, GetMaxPossibleSinTL);
                        CreateHKLLists(PatternAuxiliary, HKLList, StartPos, MaxSinTL);
                    end else
                        if PatternParams is TNeutronCompList then
                            with PatternParams as TNeutronCompList do
                            begin
                                MaxSinTL := Min(
                                    GetMaxNeutronSinTL(PatternAuxiliary),
                                    GetMaxPossibleSinTL);
                                CreateHKLLists(PatternAuxiliary, HKLList, 0, MaxSinTL);
                            end
                        else CreateHKLLists(
                            PatternAuxiliary, HKLList, 0, GetMaxPossibleSinTL);
            end;
        end     else raise ECalcModule.Create('Pattern must be assigned...');

    if PatternParams is TNeutronCompList then CreateSinTList(PatternAuxiliary);
    AddToSinTList;

    if PatternParams is TNeutronCompList then
    begin
        RemoveAllEmptySinTItems;
        CalcZeroNonZeroHKLNumbers;
        CalcExpSums;
    end;

    SinTList.MulIntParam := 1;
    SinTList.IFType := IFType;
    NuclIntCalculated := False;

    StandardCalc;
end;

procedure TCalcModule.LinkSinTListWithStars;
var i: LongInt;
    TW: TWaveVector;
    TS: TSite;
begin
    for i := 0 to GetSiteList.Count - 1 do
    begin
        TS := TSite(GetSiteList.Items[i]);
        if Assigned(TS.WaveVectorList.MagnStar) then
            TS.WaveVectorList.MagnStar.IntensityFactors := SinTList;
        if Assigned(TS.WaveVectorList.StructStar) then
            TS.WaveVectorList.StructStar.IntensityFactors := SinTList;
    end;
end;

procedure TCalcModule.GetMaxIndexes(out MaxExpIndex, MaxCalcIndex: LongInt);
var i: LongInt;
    TS: TSinTClass;
    MaxExp, MaxCalc: Double;
begin
    MaxExpIndex := -1; MaxCalcIndex := -1;
    MaxExp := 0; MaxCalc := 0;
    for i := 0 to SinTList.Count - 1 do
    begin
        TS := TSinTClass(SinTList.Items[i]);
        if TS.ExpIntensity > MaxExp then
        begin MaxExp := TS.ExpIntensity; MaxExpIndex := i end;
        if TS.Intensity + TS.NuclearIntensity > MaxCalc then
        begin
            MaxCalc := TS.Intensity + TS.NuclearIntensity;
            MaxCalcIndex := i;
        end;
    end;
end;

procedure TCalcModule.CalcExpSums;
var i: LongInt;
    TS: TSinTClass;
    TempIntensity: Double;
begin
    FLinearSumExpInt := 0; FSquareLawSumExpInt := 0;
    FMaxExpValue := 0; FMaxExpIndex := -1;
    with SinTList do
        for i := 0 to Count - 1 do
        begin
            TS := TSinTClass(Items[i]);
            case RFactorMode of
                0 : TempIntensity := TS.ExpIntensity;
                1 : TempIntensity := TS.ExpIntensity /
                    GetIntegralFactor(TS.SinT, TS.Sin2T);
            end;
            FLinearSumExpInt := FLinearSumExpInt + TempIntensity;
            FSquareLawSumExpInt := FSquareLawSumExpInt + Sqr(TempIntensity);
            if TempIntensity > FMaxExpValue then
            begin FMaxExpValue := TempIntensity; FMaxExpIndex := i end;
        end;
end;

procedure TCalcModule.MoveViewSinTListData;
begin
    SinTList.CopyParameters(ViewSinTList);
end;

procedure TCalcModule.MoveViewStructListData;
begin
    GetSiteList.CopyParameters(ViewSiteList);
end;

procedure TCalcModule.SetMulIntConst(const AMulIntConst: Double);
var i: LongInt;
    HC: THKLClass;
    ST: TSinTClass;
begin
    for i := 0 to HKLList.Count - 1 do
    begin
        HC := THKLClass(HKLList.Items[i]);
        HC.Intensity := HC.Intensity * AMulIntConst;
        HC.ReF := HC.ReF * AMulIntConst;
        HC.ImF := HC.ImF * AMulIntConst;
    end;
    for i := 0 to SinTList.Count - 1 do
    begin
        ST := TSinTClass(SinTList.Items[i]);
        ST.Intensity := ST.Intensity * AMulIntConst;
    end;
end;

procedure TCalcModule.CalcZeroNonZeroHKLNumbers;
var i: LongInt;
    TS: TSinTClass;
begin
    FZeroHKLNumber := 0;
    FNonZeroHKLNumber := 0;
    for i := 0 to SinTList.Count - 1 do
    begin
        TS := TSinTClass(SinTList.Items[i]);
        if TS.ExpIntensity = 0 then
            FZeroHKLNumber := FZeroHKLNumber + TS.HKLList.Count
        else FNonZeroHKLNumber := FNonZeroHKLNumber + TS.HKLList.Count;
    end;
end;

function TCalcModule.GetParametersNumber: LongInt;
begin
    {$IFDEF FITTING}
    if (VariationMode and PVM_DEBYE_WALLER <> 0) then Result := 1
    else Result := 0;
    {$ELSE}
    Result := 0;
    if (VariationMode and PVM_DEBYE_WALLER <> 0) then Inc(Result);
    if (VariationMode and PVM_INT_NORMALIZATION <> 0) then Inc(Result);
    {$ENDIF}
end;

function TCalcModule.GetParameter(index: LongInt): TVariableParameter;
begin
    if (index < 0) or (index >= ParametersNumber) then
        raise ECalcModule.Create('Invalid parameter index...')
    else
    begin
        {$IFDEF FITTING}
        Result.Value := SinTList.DWParam;
        Result.Limited := False;
        {$ELSE}
        case index of
            0 : begin
                Result.Value := SinTList.DWParam;
                Result.Limited := False;
            end;
            1 : begin
                Result.Value := SinTList.MulIntParam;
                Result.Limited := False;
            end;
        end;
        {$ENDIF}
    end;
end;

procedure TCalcModule.SetParameter(index: LongInt;
    AParameter: TVariableParameter);
begin
    if (index < 0) or (index >= ParametersNumber) then
        raise ECalcModule.Create('Invalid parameter index...')
    else
        {$IFDEF FITTING}
        SinTList.DWParam := AParameter.Value;
        {$ELSE}
        case index of
            0 : SinTList.DWParam := AParameter.Value;
            1 : SinTList.MulIntParam := AParameter.Value;
        end;
        {$ENDIF}
end;

function TCalcModule.GetNumberOfValues: LongInt;
begin
    Result := 1;
end;

function TCalcModule.GetValueIndex: LongInt;
begin
    Result := 0;
end;

procedure TCalcModule.SetValueIndex(const AValueIndex: LongInt);
begin
end;

function TCalcModule.GetSiteList: TSiteList;
begin
    if Assigned(FSiteList) then Result := FSiteList
    else raise ECalcModule.Create('Sites list isn''t assigned...');
end;

procedure TCalcModule.SetSiteList(const ASiteList: TSiteList);
begin
    UtilizeObject(FSiteList);
    FSiteList := ASiteList;
end;

function TCalcModule.GetPatternParams: TComponent;
begin
    if Assigned(FPatternParams) then Result := FPatternParams
    else raise ECalcModule.Create('Neutron data isn''t assigned...');
end;

procedure TCalcModule.SetPatternParams(const APatternParams: TComponent);
begin
    UtilizeObject(FPatternParams);
    FPatternParams := APatternParams;
end;

procedure TCalcModule.CreateParameters;
begin
end;

procedure TCalcModule.ParametersUpdated;
begin
end;

function TCalcModule.GetMaxPossibleSinTL: Double;
var IPPC: IPatternParametersContainer;
    PatternParams: TComponent;
begin
    PatternParams := GetPatternParams;
    if PatternParams.GetInterface(PatternParamsContainerGUID, IPPC) then
        with IPPC do Result := 1 / Lambda
    else raise ECalcModule.Create('Pattern parameters interface not found...');
end;

initialization
    RegisterClass(TCalcModule);
end.


