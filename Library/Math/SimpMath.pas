{------------------------------------------------------------------------------
    This file is part of the MotifMASTER project. This software is
    distributed under GPL (see gpl.txt for details).

    This software is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Copyright (C) 1999-2007 D.Morozov (dvmorozov@mail.ru)
------------------------------------------------------------------------------}

unit SimpMath;

interface

uses
    Math, SysUtils, Classes, CBRCComponent, SelfSaved, ClassInheritIDs;

type
    EPointsArrayIsNotAssigned = class(Exception);
    TwoDimArray = array of array[1..2] of Double;

type
    TDoubleVector3 = array[1..3] of Double;

    IVector = interface;

    ISpace = interface
        function GetScalarMul(const Vect1, Vect2: IVector): Double;
            end;

    IVector = interface
        function GetSpace: ISpace;
        procedure SetSpace(const ASpace: ISpace);
        function GetNorma: Double;
        procedure SetNorma(const ANorma: Double);
        function GetCompsNumber: LongInt;
        function GetComp(index: LongInt): Double;
        procedure SetComp(index: LongInt; AComp: Double);
        function GetNormComp(index: LongInt): Double;

        property Space: ISpace
                read GetSpace           write SetSpace;
        property Norma: Double
                read GetNorma           write SetNorma;
        property CompsNumber: LongInt
                read GetCompsNumber;
        property Comps[index: LongInt]: Double
                read GetComp            write SetComp;
        property NormComps[index: LongInt]: Double
                read GetNormComp;
    end;

    IComplexVector = interface(IVector)
        function GetImComp(index: LongInt): Double;
        procedure SetImComp(index: LongInt; AImComp: Double);
        function GetNormImComp(index: LongInt): Double;

        property ImComps[index: LongInt]: Double
            read GetImComp          write SetImComp;
        property NormImComps[index: LongInt]: Double
            read GetNormImComp;
    end;

    E3DVector = class(Exception);
    T3DVector = class(TCBRCComponent, IVector)
        protected
        FSpace: ISpace;
        FVector: TDoubleVector3;
        FNormalizedVector: TDoubleVector3;
        FNorma: Double;

        function GetSpace: ISpace;
        procedure SetSpace(const ASpace: ISpace);
        function GetNorma: Double;
        procedure SetNorma(const ANorma: Double);
        function GetCompsNumber: LongInt;
        function GetComp(index: LongInt): Double;
        procedure SetComp(index: LongInt; AComp: Double);
        function GetNormComp(index: LongInt): Double;

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
        property Space: ISpace
                read GetSpace           write SetSpace;
        property Norma: Double
                read GetNorma           write SetNorma;
        property CompsNumber: LongInt
                read GetCompsNumber;
        property Comps[index: LongInt]: Double
                read GetComp            write SetComp;
        property NormComps[index: LongInt]: Double
                read GetNormComp;
    end;

    T3DComplexVector = class(T3DVector, IComplexVector)
    protected
        FImVector: TDoubleVector3;

        function GetImComp(index: LongInt): Double;
        procedure SetImComp(index: LongInt; AImComp: Double);
        function GetNormImComp(index: LongInt): Double;

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
        property ImComps[index: LongInt]: Double
                read GetImComp          write SetImComp;
        property NormImComps[index: LongInt]: Double
                read GetNormImComp;
    end;

procedure ConvertSphericalToDekart(Theta, Phi, R: Double; var x, y, z: Double);
    procedure ConvertDekartToSpherical(x, y, z: Double; var Theta, Phi, R: Double);
procedure ConvertDekartToAphine(
    const A, B, C, Alpha, Beta, Gamma: Double;
    var Vector: TDoubleVector3);
procedure ConvertAphineToDekart(
    const A, B, C, Alpha, Beta, Gamma: Double;
    var Vector: TDoubleVector3);

procedure DecPhi(Dec: Double; var Phi: Double);
procedure DecTheta(Dec: Double; var Theta: Double);
procedure IncPhi(Inc: Double; var Phi: Double);
procedure IncTheta(Inc: Double; var Theta: Double);

procedure PutValueIntoInterval(
    const MinLimit, MaxLimit: Double;
    var Value: Double);
function IsValueIntoInterval(const MinLimit, MaxLimit, Value: Double): Boolean;
    function GetScalarMul(const Vect1, Vect2: TDoubleVector3): Double;
    function GetScalarMulA(
    const Vect1, Vect2: TDoubleVector3;
    A, B, C, Alpha, Beta, Gamma: Double): Double;
function GetScalarMulAN(
    const Vect1, Vect2: TDoubleVector3;
    A, B, C, Alpha, Beta, Gamma: Double): Double;
function GetAngle(
    const Vect1, Vect2: TDoubleVector3;
    A, B, C, Alpha, Beta, Gamma: Double): Double;
function GetVectorMulA(
    const Vect1, Vect2: TDoubleVector3;
    A, B, C, Alpha, Beta, Gamma: Double): TDoubleVector3;
function GetVectModule(
    const Vect: TDoubleVector3): Double;
function GetVectModuleA(
    const Vect: TDoubleVector3;
    const A, B, C, Alpha, Beta, Gamma: Double): Double;
procedure GetUnitVect(
    const Vect: TDoubleVector3;
    var UnitVect: TDoubleVector3);
procedure GetUnitVectA(
    const Vect: TDoubleVector3;
    A, B, C, Alpha, Beta, Gamma: Double; var UnitVect: TDoubleVector3);
procedure GetMutualVectors(
    const A, B, C, Alpha, Beta, Gamma: Double;
    var Vect1, Vect2, Vect3: TDoubleVector3);
procedure GetMutualVectorsInNewBasis(
    const A, B, C, Alpha, Beta, Gamma: Double;
        NewBasisVect1, NewBasisVect2, NewBasisVect3: TDoubleVector3;
        var Vect1, Vect2, Vect3: TDoubleVector3
        );
function GetVolume(const A, B, C, Alpha, Beta, Gamma: Double): Double;
    function GetVectInNewBasis(
    const A, B, C, Alpha, Beta, Gamma: Double;
        NewBasisVect1, NewBasisVect2, NewBasisVect3: TDoubleVector3;
        InitialVect: TDoubleVector3
        ): TDoubleVector3;
function MulVectByValue(const Vect: TDoubleVector3;
    Value: Double): TDoubleVector3;
procedure SetVectModule(var Vect: TDoubleVector3;
    const A, B, C, Alpha, Beta, Gamma, Module: Double);
function GetSubVect(Vect1, Vect2: TDoubleVector3): TDoubleVector3;
function ArcSin(x: Double): Double;
function ArcCos(x: Double): Double;
function GetNumberDegree(Number: Double): LongInt;
    function GetPowerOf10(Power: LongInt): Double;
function Sign(Number: Double): LongInt;
function Lagrange(
    PointsArray: TwoDimArray;   const X: Double): Double;
procedure Gauss(PointsArray: TwoDimArray; const A, Sigma, x0: Double);

implementation

function GetD(const Alpha, Beta, Gamma: Double): Double; forward;
function GetPAlpha(const Alpha, Beta, Gamma: Double): Double; forward;
function GetPBeta(const Alpha, Beta, Gamma: Double): Double; forward;
function GetPGamma(const Alpha, Beta, Gamma: Double): Double; forward;

function ArcSin(x: Double): Double;
var TempDouble: Double;
const TINY = 1e-6;
begin
    TempDouble := 1 - Sqr(x);
    if Abs(TempDouble) < TINY then TempDouble := 0;
        TempDouble := Sqrt(TempDouble);
    if TempDouble <> 0 then Result := ArcTan2(x, TempDouble)
    else Result := pi / 2;
end;

function ArcCos(x: Double): Double;
var TempDouble: Double;
const TINY = 1e-6;
begin
    TempDouble := 1 - Sqr(x);
    if Abs(TempDouble) < TINY then TempDouble := 0;
    if x <> 0 then Result := ArcTan2(Sqrt(TempDouble), x)
    else Result := pi / 2;
end;

function GetScalarMul(const Vect1, Vect2: TDoubleVector3): Double;
begin
    Result := Vect1[1] * Vect2[1] + Vect1[2] * Vect2[2] + Vect1[3] * Vect2[3];
end;

function GetScalarMulA(const Vect1, Vect2: TDoubleVector3;
    A, B, C, Alpha, Beta, Gamma: Double): Double;
begin
    Result := Vect1[1] * Vect2[1] * Sqr(A) +
    Vect1[2] * Vect2[2] * Sqr(B) + Vect1[3] * Vect2[3] * Sqr(C) +
    (Vect1[2] * Vect2[1] + Vect1[1] * Vect2[2]) * A * B * Cos(Gamma) +
    (Vect1[1] * Vect2[3] + Vect1[3] * Vect2[1]) * C * A * Cos(Beta) +
    (Vect1[3] * Vect2[2] + Vect1[2] * Vect2[3]) * B * C * Cos(Alpha);
end;

function GetScalarMulAN(const Vect1, Vect2: TDoubleVector3;
    A, B, C, Alpha, Beta, Gamma: Double): Double;
var V1, V2: TDoubleVector3;
begin
    GetUnitVectA(Vect1, A, B, C, Alpha, Beta, Gamma, V1);
    GetUnitVectA(Vect2, A, B, C, Alpha, Beta, Gamma, V2);

    Result := GetScalarMulA(V1, V2, A, B, C, Alpha, Beta, Gamma);
end;

function GetAngle(const Vect1, Vect2: TDoubleVector3;
    A, B, C, Alpha, Beta, Gamma: Double): Double;
begin
    Result := ArcCos(GetScalarMulAN(Vect1, Vect2, A, B, C, Alpha, Beta, Gamma));
end;

function GetVectModule(const Vect: TDoubleVector3): Double;
begin
    Result := Sqrt(GetScalarMul(Vect, Vect));
end;

function GetVectModuleA(
    const Vect: TDoubleVector3;
    const A, B, C, Alpha, Beta, Gamma: Double): Double;
begin
    Result := Sqrt(GetScalarMulA(Vect, Vect, A, B, C, Alpha, Beta, Gamma));
end;

function MulVectByValue(const Vect: TDoubleVector3;
    Value: Double): TDoubleVector3;
begin
    Result[1] := Vect[1] * Value;
    Result[2] := Vect[2] * Value;
    Result[3] := Vect[3] * Value;
end;

procedure SetVectModule(var Vect: TDoubleVector3;
    const A, B, C, Alpha, Beta, Gamma, Module: Double);
var TempModule: Double;
begin
    TempModule := GetVectModuleA(Vect, A, B, C, Alpha, Beta, Gamma);
    if TempModule <> 0 then Vect := MulVectByValue(Vect, Module / TempModule)
end;

function GetSubVect(Vect1, Vect2: TDoubleVector3): TDoubleVector3;
begin
    Result[1] := Vect1[1] - Vect2[1];
    Result[2] := Vect1[2] - Vect2[2];
    Result[3] := Vect1[3] - Vect2[3];
end;

procedure ConvertSphericalToDekart(Theta, Phi, R: Double; var x, y, z: Double);
    begin
    x := R * Sin(Theta) * Cos(Phi);
    y := R * Sin(Theta) * Sin(Phi);
    z := R * Cos(Theta);
end;

procedure ConvertDekartToSpherical(x, y, z: Double; var Theta, Phi, R: Double);
begin
    R := Sqrt(Sqr(x) + Sqr(y) + Sqr(z));
    if z <> 0 then Theta := ArcTan2(Sqrt(Sqr(x) + Sqr(y)), z)
    else Theta := pi / 2;
    if x <> 0 then Phi := ArcTan2(y, x)
    else if y >= 0 then Phi := pi / 2 else Phi := -pi / 2;
end;

procedure DecPhi(Dec: Double; var Phi: Double);
begin
    Phi := Phi - Dec;
    if Phi > pi then Phi := -pi + (Phi - pi)
    else if Phi <= -pi then Phi := pi - (-pi - Phi);
end;

procedure DecTheta(Dec: Double; var Theta: Double);
begin
    Theta := Theta - Dec;
    if Theta < 0 then Theta := 0;
end;

procedure IncPhi(Inc: Double; var Phi: Double);
begin
    Phi := Phi + Inc;
    if Phi > pi then Phi := -pi + (Phi - pi)
    else if Phi <= -pi then Phi := pi - (-pi - Phi);
end;

procedure IncTheta(Inc: Double; var Theta: Double);
begin
   Theta := Theta + Inc;
   if Theta > pi then Theta := pi;
end;

function GetNumberDegree(Number: Double): LongInt;
var i: LongInt;
    TempDouble: Double;
begin
    TempDouble := Number;
    if Number = 0 then
    begin Result := 1;  Exit end;
    if Number >= 1 then
    begin
        i := -1;
        while Int(TempDouble) <> 0 do
        begin
            Inc(i);
            TempDouble := TempDouble / 10;
        end
    end
    else
    begin
        i := 0;
        while Int(TempDouble) = 0 do
        begin
            Dec(i);
            TempDouble := TempDouble * 10;
        end;
    end;
    Result := i;
end;

function GetPowerOf10(Power: LongInt): Double;
var i: LongInt;
    TempDouble: Double;
begin
    TempDouble := 1;
    if Power >= 0 then
        for i := 1 to Power do TempDouble := TempDouble * 10
    else
        for i := -1 downto Power do TempDouble := TempDouble * 0.1;
    Result := TempDouble;
end;

function Sign(Number: Double): LongInt;
begin
    if Number >= 0 then Result := 1
    else Result := -1;
end;

function Lagrange(PointsArray: TwoDimArray;
    const X: Double): Double;
var Lagr: Double;
    p1, p2: Double;
    i, j1: LongInt;
begin
    if not Assigned(PointsArray) then
    begin
        raise EPointsArrayIsNotAssigned.Create('Points array is not assigned...');
        Exit;
    end;
    Lagr := 0;
    for i := 0 to Length(PointsArray) - 1 do
    begin
        p1 := 1; p2 := 1;
        for j1 := 0 to Length(PointsArray) - 1 do
        begin
            if i <> j1 then
            begin
                p1 := p1 * (PointsArray[i][1] - PointsArray[j1][1]);
                p2 := p2 * (X - PointsArray[j1][1]);
            end;
        end;
        if p1 <> 0 then Lagr := Lagr + PointsArray[i][2] * p2 / p1;
    end;
    Result := Lagr;
end;

procedure Gauss(PointsArray: TwoDimArray;  const A, Sigma, x0: Double);
var i: LongInt;
begin
    if not Assigned(PointsArray) then
    begin
        raise EPointsArrayIsNotAssigned.Create('Points array is not assigned...');
        Exit;
    end;
    for i := 0 to Length(PointsArray) - 1 do
        PointsArray[i][2] := (A / (Sigma * Sqrt(2 * pi)))  *
        exp(-1 * Sqr(x0 - PointsArray[i][1]) / (2 * Sqr(Sigma)));
end;

procedure PutValueIntoInterval(const MinLimit, MaxLimit: Double;
var Value: Double);
begin
    if Value > MaxLimit then
        Value := MinLimit + Frac((Value - MaxLimit) /
        (MaxLimit - MinLimit)) * (MaxLimit - MinLimit);
    if Value < MinLimit then
        Value := MaxLimit - Frac((MinLimit - Value) /
        (MaxLimit - MinLimit)) * (MaxLimit - MinLimit);
end;

function IsValueIntoInterval(const MinLimit, MaxLimit, Value: Double): Boolean;
begin
    if (Value >= MinLimit) and (Value <= MaxLimit) then Result := True
    else Result := False;
end;

procedure ConvertDekartToAphine(
    const A, B, C, Alpha, Beta, Gamma: Double;
    var Vector: TDoubleVector3);
var V1, V2, V3, Result: TDoubleVector3;
begin
    V1[1] := 1; V1[2] := 0; V1[3] := 0;
    V2[1] := 0; V2[2] := 1; V2[3] := 0;
    V3 := GetVectorMulA(V1, V2, A, B, C, Alpha, Beta, Gamma);
    GetUnitVectA(V1, A, B, C, Alpha, Beta, Gamma, V1);
    GetUnitVectA(V3, A, B, C, Alpha, Beta, Gamma, V3);
    V2 := GetVectorMulA(V3, V1, A, B, C, Alpha, Beta, Gamma);
    GetUnitVectA(V2, A, B, C, Alpha, Beta, Gamma, V2);
    Result[1] := Vector[1] * V1[1] + Vector[2] * V2[1] + Vector[3] * V3[1];
    Result[2] := Vector[1] * V1[2] + Vector[2] * V2[2] + Vector[3] * V3[2];
    Result[3] := Vector[1] * V1[3] + Vector[2] * V2[3] + Vector[3] * V3[3];
    Vector := Result;
end;

procedure ConvertAphineToDekart(
    const A, B, C, Alpha, Beta, Gamma: Double;
    var Vector: TDoubleVector3);
var V1, V2, V3, Result: TDoubleVector3;
begin
    V1[1] := 1; V1[2] := 0; V1[3] := 0;
    V2[1] := 0; V2[2] := 1; V2[3] := 0;
    V3 := GetVectorMulA(V1, V2, A, B, C, Alpha, Beta, Gamma);
    GetUnitVectA(V1, A, B, C, Alpha, Beta, Gamma, V1);
    GetUnitVectA(V3, A, B, C, Alpha, Beta, Gamma, V3);
    V2 := GetVectorMulA(V3, V1, A, B, C, Alpha, Beta, Gamma);
    GetUnitVectA(V2, A, B, C, Alpha, Beta, Gamma, V2);
    Result[1] := GetScalarMulA(Vector, V1, A, B, C, Alpha, Beta, Gamma);
    Result[2] := GetScalarMulA(Vector, V2, A, B, C, Alpha, Beta, Gamma);
    Result[3] := GetScalarMulA(Vector, V3, A, B, C, Alpha, Beta, Gamma);
    Vector := Result;
end;

procedure GetUnitVect(
    const Vect: TDoubleVector3;
    var UnitVect: TDoubleVector3);
var Module: Double;
begin
    Module := GetVectModule(Vect);
    if Module <> 0 then begin
        UnitVect[1] := Vect[1] / Module;
        UnitVect[2] := Vect[2] / Module;
        UnitVect[3] := Vect[3] / Module;
    end else begin
        UnitVect[1] := 0;
        UnitVect[2] := 0;
        UnitVect[3] := 0;
    end;
end;

procedure GetUnitVectA(
    const Vect: TDoubleVector3;
    A, B, C, Alpha, Beta, Gamma: Double; var UnitVect: TDoubleVector3);
var Module: Double;
begin
    Module := GetVectModuleA(Vect, A, B, C, Alpha, Beta, Gamma);
    if Module <> 0 then begin
        UnitVect[1] := Vect[1] / Module;
        UnitVect[2] := Vect[2] / Module;
        UnitVect[3] := Vect[3] / Module;
    end else begin
        UnitVect[1] := 0;
        UnitVect[2] := 0;
        UnitVect[3] := 0;
    end;
end;

function GetD(const Alpha, Beta, Gamma: Double): Double;
begin
    Result := 1 - Sqr(Cos(Alpha)) - Sqr(Cos(Beta)) - Sqr(Cos(Gamma))
        + 2 * Cos(Alpha) * Cos(Beta) * Cos(Gamma);
end;

function GetPAlpha(const Alpha, Beta, Gamma: Double): Double;
begin
    Result := Cos(Beta) * Cos(Gamma) - Cos(Alpha);
end;

function GetPBeta(const Alpha, Beta, Gamma: Double): Double;
begin
    Result := Cos(Alpha) * Cos(Gamma) - Cos(Beta);
end;

function GetPGamma(const Alpha, Beta, Gamma: Double): Double;
begin
    Result := Cos(Beta) * Cos(Alpha) - Cos(Gamma);
end;

function GetVolume(const A, B, C, Alpha, Beta, Gamma: Double): Double;
begin
    Result := A * B * C * Sqrt(GetD(Alpha, Beta, Gamma));
end;

procedure GetMutualVectors(
    const A, B, C, Alpha, Beta, Gamma: Double;
    var Vect1, Vect2, Vect3: TDoubleVector3);
var SqrtD: Double;
    PAlpha, PBeta, PGamma: Double;
    V: Double;
    Angle: Double;
    TempVect: TDoubleVector3;
begin
    SqrtD := Sqrt(GetD(Alpha, Beta, Gamma));
    PAlpha := GetPAlpha(Alpha, Beta, Gamma);
    PBeta := GetPBeta(Alpha, Beta, Gamma);
    PGamma := GetPGamma(Alpha, Beta, Gamma);
    V := GetVolume(A, B, C, Alpha, Beta, Gamma);

    Vect1[1] := C * B * Sqr(Sin(Alpha)) / (A * SqrtD);
    Vect1[2] := C * PGamma / SqrtD;
    Vect1[3] := B * PBeta / SqrtD;
    Vect1 := MulVectByValue(Vect1, 1 / V);
    TempVect[1] := 1; TempVect[2] := 0; TempVect[3] := 0;
    Angle := GetAngle(Vect1, TempVect, A, B, C, Alpha, Beta, Gamma);
    if Angle > pi / 2 then Vect1 := MulVectByValue(Vect1, -1);

    Vect2[1] := C * PGamma / SqrtD;
    Vect2[2] := A * C * Sqr(Sin(Beta)) / (B * SqrtD);
    Vect2[3] := A * PAlpha / SqrtD;
    Vect2 := MulVectByValue(Vect2, 1 / V);
    TempVect[1] := 0; TempVect[2] := 1; TempVect[3] := 0;
    Angle := GetAngle(Vect2, TempVect, A, B, C, Alpha, Beta, Gamma);
    if Angle > pi / 2 then Vect2 := MulVectByValue(Vect2, -1);

    Vect3[1] := B * PBeta / SqrtD;
    Vect3[2] := A * PAlpha / SqrtD;
    Vect3[3] := A * B * Sqr(Sin(Gamma)) / (C * SqrtD);
    Vect3 := MulVectByValue(Vect3, 1 / V);
    TempVect[1] := 0; TempVect[2] := 0; TempVect[3] := 1;
    Angle := GetAngle(Vect3, TempVect, A, B, C, Alpha, Beta, Gamma);
    if Angle > pi / 2 then Vect3 := MulVectByValue(Vect3, -1);
end;

procedure GetMutualVectorsInNewBasis(
    const A, B, C, Alpha, Beta, Gamma: Double;
        NewBasisVect1, NewBasisVect2, NewBasisVect3: TDoubleVector3;
        var Vect1, Vect2, Vect3: TDoubleVector3
        );
var NewA, NewB, NewC, NewAlpha, NewBeta, NewGamma: Double;
        NewV: Double;
        begin
    NewA := GetVectModuleA(NewBasisVect1, A, B, C, Alpha, Beta, Gamma);
    NewB := GetVectModuleA(NewBasisVect2, A, B, C, Alpha, Beta, Gamma);
    NewC := GetVectModuleA(NewBasisVect3, A, B, C, Alpha, Beta, Gamma);
    NewAlpha := GetAngle(NewBasisVect2, NewBasisVect3, A, B, C, Alpha, Beta, Gamma);
    NewBeta := GetAngle(NewBasisVect1, NewBasisVect3, A, B, C, Alpha, Beta, Gamma);
    NewGamma := GetAngle(NewBasisVect1, NewBasisVect2, A, B, C, Alpha, Beta, Gamma);
    NewV := GetVolume(NewA, NewB, NewC, NewAlpha, NewBeta, NewGamma);

    Vect1 := GetVectorMulA(NewBasisVect2, NewBasisVect3, A, B, C, Alpha, Beta, Gamma);
    Vect1 := MulVectByValue(Vect1, 1 / NewV);
    Vect2 := GetVectorMulA(NewBasisVect3, NewBasisVect1, A, B, C, Alpha, Beta, Gamma);
    Vect2 := MulVectByValue(Vect2, 1 / NewV);
    Vect3 := GetVectorMulA(NewBasisVect1, NewBasisVect2, A, B, C, Alpha, Beta, Gamma);
    Vect3 := MulVectByValue(Vect3, 1 / NewV);
end;

function GetVectInNewBasis(
    const A, B, C, Alpha, Beta, Gamma: Double;
        NewBasisVect1, NewBasisVect2, NewBasisVect3: TDoubleVector3;
        InitialVect: TDoubleVector3
        ): TDoubleVector3;
var MutVect1, MutVect2, MutVect3: TDoubleVector3;
begin
    GetMutualVectorsInNewBasis(A, B, C, Alpha, Beta, Gamma,
    NewBasisVect1, NewBasisVect2, NewBasisVect3,
    MutVect1, MutVect2, MutVect3);
    Result[1] := GetScalarMulA(InitialVect, MutVect1, A, B, C, Alpha, Beta, Gamma);
    Result[2] := GetScalarMulA(InitialVect, MutVect2, A, B, C, Alpha, Beta, Gamma);
    Result[3] := GetScalarMulA(InitialVect, MutVect3, A, B, C, Alpha, Beta, Gamma);
end;

function GetVectorMulA(
    const Vect1, Vect2: TDoubleVector3;
    A, B, C, Alpha, Beta, Gamma: Double): TDoubleVector3;
var V1, V2, V3: TDoubleVector3;
begin
    GetMutualVectors(A, B, C, Alpha, Beta, Gamma, V1, V2, V3);
    V1 := MulVectByValue(V1, Vect1[2] * Vect2[3] - Vect1[3] * Vect2[2]);
    V2 := MulVectByValue(V2, Vect1[3] * Vect2[1] - Vect1[1] * Vect2[3]);
    V3 := MulVectByValue(V3, Vect1[1] * Vect2[2] - Vect1[2] * Vect2[1]);
    Result[1] := V1[1] + V2[1] + V3[1];
    Result[2] := V1[2] + V2[2] + V3[2];
    Result[3] := V1[3] + V2[3] + V3[3];
    Result := MulVectByValue(Result, GetVolume(A, B, C, Alpha, Beta, Gamma));
end;

function T3DVector.GetSpace: ISpace;
begin
    if Assigned(FSpace) then Result := FSpace
    else raise E3DVector.Create('Space is not assigned...');
end;

procedure T3DVector.SetSpace(const ASpace: ISpace);
begin
    FSpace := ASpace;
end;

function T3DVector.GetNorma: Double;
begin
    Result := FNorma;
end;

procedure T3DVector.SetNorma(const ANorma: Double);
begin
  end;

function T3DVector.GetCompsNumber: LongInt;
begin
    Result := 3;
end;

function T3DVector.GetComp(index: LongInt): Double;
begin
    if (Index < 0) or (index > CompsNumber) then
        raise E3DVector.Create('Invalid index...')
    else Result := FVector[index + 1];
end;

procedure T3DVector.SetComp(index: LongInt; AComp: Double);
begin
    if (Index < 0) or (index > CompsNumber) then
        raise E3DVector.Create('Invalid index...')
    else FVector[index + 1] := AComp;
    end;

function T3DVector.GetNormComp(index: LongInt): Double;
begin
    if (Index < 0) or (index > CompsNumber) then
        raise E3DVector.Create('Invalid index...')
    else Result := FNormalizedVector[index + 1];
end;

function T3DComplexVector.GetImComp(index: LongInt): Double;
begin
end;

procedure T3DComplexVector.SetImComp(index: LongInt; AImComp: Double);
begin
end;

function T3DComplexVector.GetNormImComp(index: LongInt): Double;
begin
end;

class function T3DVector.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := V3DCClassInheritID;
    Result.PropVersionNum := V3DCurVerNum;
end;

class procedure T3DVector.ReadProperties(const Reader: TReader;
    const PropHeaderRec: TPropHeaderRec;
    const AnObject: TSelfSavedComponent);
var i: LongInt;
begin
    with AnObject as Self do
        for i := 0 to 2 do Comps[i] := Reader.ReadFloat;
end;

class procedure T3DVector.WriteProperties(const Writer: TWriter;
    const AnObject: TSelfSavedComponent);
var i: LongInt;
begin
    with AnObject as Self do
        for i := 0 to 2 do Writer.WriteFloat(Comps[i]);
end;

class function T3DComplexVector.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := CV3DCClassInheritID;
    Result.PropVersionNum := CV3DCurVerNum;
end;

class procedure T3DComplexVector.ReadProperties(const Reader: TReader;
    const PropHeaderRec: TPropHeaderRec;
    const AnObject: TSelfSavedComponent);
var i: LongInt;
begin
    with AnObject as Self do
        for i := 0 to 2 do ImComps[i] := Reader.ReadFloat;
end;

class procedure T3DComplexVector.WriteProperties(const Writer: TWriter;
    const AnObject: TSelfSavedComponent);
var i: LongInt;
begin
    with AnObject as Self do
        for i := 0 to 2 do Writer.WriteFloat(ImComps[i]);
end;

initialization
    RegisterClass(T3DVector);
    RegisterClass(T3DComplexVector);
end.
