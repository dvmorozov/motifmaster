{------------------------------------------------------------------------------
    This file is part of the MotifMASTER project. This software is
    distributed under GPL (see gpl.txt for details).

    This software is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Copyright (C) 1999-2007 D.Morozov (dvmorozov@mail.ru)
------------------------------------------------------------------------------}

unit DataLoader;

interface

uses Classes, SysUtils, SimpMath;

type
  EPointindexOutOfBounds = class(Exception);
  EPointsSetIsNotAssigned = class(Exception);
  EFileNotFound = class(Exception);
  EInvalidFileFormat = class(Exception);
  EWavelengthIsNotSpecified = class(Exception);

  TPointsSet = class(TComponent)
  protected
    FPoints: TwoDimArray;
    function GetPointsCount: LongInt;
    function GetPointXCoord(index: LongInt): Double; virtual;
    procedure SetPointXCoord(index: LongInt; Value: Double); virtual;
    function GetPointYCoord(index: LongInt): Double; virtual;
    procedure SetPointYCoord(index: LongInt; Value: Double); virtual;
    function GetMaxXCoord: Double;
    function GetMaxYCoord: Double;

  public
    function GetCopy: TPointsSet;
    destructor Destroy; override;
    procedure AddNewPoint(XValue,YValue: Double);
    procedure Clear;
    procedure Sort; virtual;
    function indexOfValueX(XValue: Double): LongInt;

    property PointsCount: LongInt read GetPointsCount;
    property Points: TwoDimArray read FPoints;
    property PointXCoord[index: LongInt]: Double read GetPointXCoord
    write SetPointXCoord;
    property PointYCoord[index: LongInt]: Double read GetPointYCoord
    write SetPointYCoord;
    property MaxXCoord: Double read GetMaxXCoord;
    property MaxYCoord: Double read GetMaxYCoord;
  end;

  TNeutronPointsSet = class(TPointsSet)
  protected
    FLambda: Double;
    function GetPointIntensity(index: LongInt): Double;
    procedure SetPointIntensity(index: LongInt; Value: Double);
    function GetPointT(index: LongInt): Double;
    function GetPoint2T(index: LongInt): Double;
    function GetPointSinTL(index: LongInt): Double;

  public
    function GetCopy: TNeutronPointsSet;
    constructor Create(AOwner: TComponent);
    property PointIntensity[index: LongInt]: Double read GetPointIntensity
    write SetPointIntensity;
    property Lambda: Double read FLambda write FLambda;
    property PointT[index: LongInt]: Double read GetPointT;
    property Point2T[index: LongInt]: Double read GetPoint2T;
    property PointSinTL[index: LongInt]: Double read GetPointSinTL;
  end;

  TGaussPointsSet = class(TNeutronPointsSet)
  protected
    Fx0: Double;
    FA: Double;
    Fx0IsSet: Boolean;
    Fx0Low,Fx0High: Double;
    procedure Setx0(Value: Double);
    procedure SetA(Value: Double);
    function GetParamCount: LongInt;
    function GetParam(index: LongInt): Double;
    procedure SetParam(index: LongInt; Value: Double);

  public
    Sigma: Double;
    procedure ReCalc;

    property Param[index: LongInt]: Double read GetParam write SetParam;
    property ParamCount: LongInt read GetParamCount;
    property x0: Double read Fx0 write Setx0;
    property A: Double read FA write SetA;
  end;

 TDataLoader = class(TComponent)
 private
  PointsSet: TNeutronPointsSet;
 public
  procedure LoadDataSet(AFileName: string); virtual; abstract;
  function GetPointsSet: TPointsSet; virtual;
  function GetPointsSetCopy: TPointsSet; virtual;
  destructor Destroy; override;
 end;

 TDATFileLoader = class(TDataLoader)
 public
  procedure LoadDataSet(AFileName: string); override;
 end;

const MIN_VALUE: Double = -1e100;
      MAX_VALUE: Double = 1e100;

implementation

{============================== TDataLoader ===================================}
function TDataLoader.GetPointsSet: TPointsSet;
begin
 if Assigned(PointsSet) then Result := PointsSet
  else
   begin
    Result := nil;
    raise EPointsSetIsNotAssigned.Create('Points set is not assigned...');
   end;
end;

function TDataLoader.GetPointsSetCopy: TPointsSet;
begin
 if Assigned(PointsSet) then Result := PointsSet.GetCopy
  else
   begin
    Result := nil;
    raise EPointsSetIsNotAssigned.Create('Points set is not assigned...');
   end;
end;

destructor TDataLoader.Destroy;
begin
 PointsSet.Free;
 inherited Destroy;
end;

{============================== TDATFileLoader ================================}
procedure TDATFileLoader.LoadDataSet(AFileName: string);
var F: TextFile;
    Val1,Val2: Double;
    Flag,Flag2: Boolean;
begin
 if not FileExists(AFileName) then
  begin
   raise EFileNotFound.Create('File ' + ExtractFileName(AFileName) + ' not found...');
   Exit;
  end;
 AssignFile(F,AFileName);
 Reset(F);
 PointsSet := TNeutronPointsSet.Create(nil);
 try
  try
   Flag := True;
   Flag2 := True;
   while not Eof(F) do
    begin
     try
      ReadLn(F,Val1,Val2);
      PointsSet.AddNewPoint(Val1,Val2);
     except
      Break;
      raise EInvalidFileFormat.Create('File ' + ExtractFileName(AFileName) + ' is not valid DAT - file...')
     end;
    end;
  finally
   CloseFile(F);
  end;
 except
  PointsSet.Free;
  PointsSet := nil;
  Exit;
  raise;
 end;
end;

{============================== TPointsSet =================================}
function TPointsSet.GetPointsCount: LongInt;
begin
 Result := Length(FPoints);
end;

function TPointsSet.GetPointXCoord(index: LongInt): Double;
begin
 if index < PointsCount then Result := FPoints[index][1]
  else raise EPointindexOutOfBounds.Create('Point index out of bounds...');
end;

function TPointsSet.GetPointYCoord(index: LongInt): Double;
begin
 if index < PointsCount then Result := FPoints[index][2]
  else raise EPointindexOutOfBounds.Create('Point index out of bounds...');
end;

procedure TPointsSet.SetPointXCoord(index: LongInt; Value: Double);
begin
 if index < PointsCount then FPoints[index][1] := Value
  else raise EPointindexOutOfBounds.Create('Point index out of bounds...');
end;

procedure TPointsSet.SetPointYCoord(index: LongInt; Value: Double);
begin
 if index < PointsCount then FPoints[index][2] := Value
  else raise EPointindexOutOfBounds.Create('Point index out of bounds...');
end;

function TPointsSet.GetMaxXCoord: Double;
var i: LongInt;
    MaxX: Double;
begin
 MaxX := MIN_VALUE;
 for i := 0 to PointsCount - 1 do
  if PointXCoord[i] > MaxX then MaxX := PointXCoord[i];
 Result := MaxX;
end;

function TPointsSet.GetMaxYCoord: Double;
var i: LongInt;
    MaxY: Double;
begin
 MaxY := MIN_VALUE;
 for i := 0 to PointsCount - 1 do
  if PointYCoord[i] > MaxY then MaxY := PointYCoord[i];
 Result := MaxY;
end;

function TPointsSet.GetCopy: TPointsSet;
var PS: TPointsSet;
    i: LongInt;
begin
 PS := TPointsSet.Create(nil);
 for i := 0 to PointsCount - 1 do PS.AddNewPoint(PointXCoord[i],PointYCoord[i]);
 Result := PS;
end;

procedure TPointsSet.AddNewPoint(XValue,YValue: Double);
begin
 SetLength(FPoints,Length(FPoints) + 1);
 FPoints[PointsCount - 1][1] := XValue;
 FPoints[PointsCount - 1][2] := YValue;
end;

procedure TPointsSet.Clear;
begin
 FPoints := nil;
end;

procedure TPointsSet.Sort;
var NewPoints: TwoDimArray;
    i,j: LongInt;
    MinValueX: Double;
    CurMinValueX: Double;
    index: LongInt;
begin
 SetLength(NewPoints,PointsCount);
 MinValueX := MIN_VALUE;
 for i := 0 to PointsCount - 1 do
  begin
   CurMinValueX := MAX_VALUE;
   index := -1;
   for j := 0 to PointsCount - 1 do
    begin
     if (PointXCoord[j] < CurMinValueX) and (PointXCoord[j] > MinValueX) then
      begin
       CurMinValueX := PointXCoord[j];
       index := j;
      end;
    end;
   NewPoints[i][1] := FPoints[index][1];
   NewPoints[i][2] := FPoints[index][2];
   MinValueX := CurMinValueX;
  end;
 FPoints := nil;
 FPoints := NewPoints;
end;

function   TPointsSet.indexOfValueX(XValue: Double): LongInt;
var i: LongInt;
begin
 Result := -1;
 for i := 0 to PointsCount - 1 do
  begin
   if PointXCoord[i] = XValue then
    begin Result := i; Exit end;
  end;
end;

destructor TPointsSet.Destroy;
begin
 FPoints := nil;
 inherited Destroy;
end;

{=========================== TNeutronPointsSet ================================}
function TNeutronPointsSet.GetCopy: TNeutronPointsSet;
var PS: TNeutronPointsSet;
    i: LongInt;
begin
 PS := TNeutronPointsSet.Create(nil);
 for i := 0 to PointsCount - 1 do PS.AddNewPoint(PointXCoord[i],PointYCoord[i]);
 PS.Lambda := Lambda;
 Result := PS;
end;

constructor TNeutronPointsSet.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
end;

function TNeutronPointsSet.GetPointT(index: LongInt): Double;
begin
  Result := FPoints[index][1] / 2;
end;

function TNeutronPointsSet.GetPoint2T(index: LongInt): Double;
begin
  Result := FPoints[index][1];
end;

function TNeutronPointsSet.GetPointSinTL(index: LongInt): Double;
begin
  if Lambda <> 0 then Result := Sin((FPoints[index][1] * pi) / (2 * 180)) / Lambda
  else raise EWavelengthIsNotSpecified.Create('Wavelength undefined...')
end;

function TNeutronPointsSet.GetPointIntensity(index: LongInt): Double;
begin
  Result := PointYCoord[index]
end;

procedure TNeutronPointsSet.SetPointIntensity(index: LongInt; Value: Double);
begin
  FPoints[index][2] := Value;
end;

{=========================== TGaussPointsSet ==================================}
function TGaussPointsSet.GetParamCount: LongInt;
begin
 Result := {3}2;
end;

function TGaussPointsSet.GetParam(index: LongInt): Double;
begin
 case index of
  0: Result := A;
  1: Result := x0;
  2: Result := Sigma;
 end;
end;

procedure TGaussPointsSet.SetParam(index: LongInt; Value: Double);
begin
 case index of
  0: A := Value;
  1: x0 := Value;
  2: Sigma := Value;
 end;
end;

procedure TGaussPointsSet.Setx0(Value: Double);
var i: LongInt;
    TempDouble: Double;
    Highindex: LongInt;
    Lowindex: LongInt;
begin
 if not Fx0IsSet then
  begin
   {первая установка параметра}
   Fx0IsSet := True;
   Fx0 := Value;
   Fx0Low := MIN_VALUE;
   Fx0High := MAX_VALUE;
   Highindex := -1;
   Lowindex := -1;
   for i := 0 to PointsCount - 1 do
    begin
     TempDouble := PointXCoord[i];
     if TempDouble < Fx0 then
      begin
       if Abs(TempDouble - Fx0) < Abs(Fx0Low - Fx0) then Fx0Low := TempDouble;
       Lowindex := i;
      end;
     if TempDouble > Fx0 then
      begin
       if Abs(TempDouble - Fx0) < Abs(Fx0High - Fx0) then Fx0High := TempDouble;
       Highindex := i;
      end;
    end;
   if Lowindex = -1 then Fx0Low := Fx0;
   if Highindex = -1 then Fx0High := Fx0;
  end
 else
  begin
   if Value < Fx0Low then
    begin Fx0 := Fx0Low; Exit end;
   if Value > Fx0High then
    begin Fx0 := Fx0High; Exit end;
   Fx0 := Value;
  end;
end;

procedure TGaussPointsSet.SetA(Value: Double);
begin
 if Value < 0 then FA := 0 else FA := Value
end;

procedure TGaussPointsSet.ReCalc;
begin
  Gauss(Points,A,Sigma,x0)
end;

end.
