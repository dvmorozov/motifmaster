{------------------------------------------------------------------------------
    This file is part of the MotifMASTER project. This software is
    distributed under GPL (see gpl.txt for details).

    This software is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Copyright (C) 1999-2007 D.Morozov (dvmorozov@mail.ru)
------------------------------------------------------------------------------}

unit MSCRDataClasses;

interface

uses Classes,  ComponentList,  Grids,  SysUtils,  NumericGrid,  SimpMath, 
     Tools,  TableComp,  DataClasses; 

const
    XCM_2T    = 0;
    XCM_T     = 1; 
    XCM_SINTL = 2; 

type
    TMSCRSite = class; 

    TMSCRStructClass = class(TAtom)
    protected
      function GetParameterValue(Param: string): Double;

    public
      CoordExpression: string;
      Site: TMSCRSite;
      FixedX, FixedY, FixedZ: Boolean;
      property M: Double read FM write SetM;
      property UnitMagnVect: TDoubleVector3 read FUnitMagnVect;
      procedure CopyParameters(const Dest: TObject); override;
      procedure CalcCoord;

    published
      property CE: string read CoordExpression write CoordExpression;
    end;

    TMSCRNeutronClass = class(TNeutronClass)
    public
      NearestSinTL: Double; 
      procedure CopyParameters(const Dest: TObject); override;
    end; 

    EMSCRNeutronCompList = class(Exception); 

    TMSCRNeutronCompList = class(TNeutronCompList)
    public
      Lambda: Double;
      ViewMode: LongInt;

      function GetDataFromGrid(Grid: TStringGrid): Boolean;
      function GetRowContents(Grid: TstringGrid; RowNum: LongInt): Boolean; override;
      procedure SetRowContents(Grid: TstringGrid; RowNum: LongInt); override;
      function GetInfoCols: LongInt; override;      

      function GetCopy: TObject; override;
      procedure CopyParameters(const Dest: TObject); override;
    end; 

    TMSCRSite = class(TSite)
    protected
      FParameters: array of Double; 
      FParametersID: array of string;
       
      function GetParametersCount: LongInt; 
      function GetParameters(index: LongInt): Double; 
      procedure SetParameters(index: LongInt; Value: Double); 
      function GetParameterValue(Param: string): Double;
      procedure SetElement(AElement: string);
      procedure SetNuclearScatAmpl(ANuclearScatAmpl: Double);

    public
      procedure AddNewParameter(Param: string; Value: Double);
      procedure ClearParameters;
      procedure RandomParameters;
      procedure GetContentsInText(StrList: Tstrings);
      procedure CalcCoord;
      destructor Destroy; override;
      function GetCopy: TObject;
      procedure CopyParameters(const Dest: TObject); override;
      function IncParameter: Boolean;

      property ParametersCount: LongInt read GetParametersCount;
      property Parameters[index: LongInt]: Double read GetParameters
      write SetParameters;
      property Element: string write SetElement;
      property NuclearScatAmpl: Double write SetNuclearScatAmpl;
     end;

    TMSCRSiteList = class(TSiteList)
    private
      function GetParametersCount: LongInt;

    public
      GroupNumber: LongInt;
      RFactor1: Double;
      RFactor2: Double;
      procedure GetContentsInText(StrList: Tstrings);
      procedure CalcCoord;
      function GetCopy: TObject; override;
      procedure CopyParameters(const Dest: TObject); override;
      procedure ClearParameters;
      procedure RandomParameters;
      function IncParameter: Boolean;

      property ParametersCount: LongInt read GetParametersCount;
    end;


implementation

procedure TMSCRNeutronCompList.SetRowContents(Grid: TstringGrid; RowNum: LongInt);
var NC: TMSCRNeutronClass;
begin
  if (RowNum - Grid.FixedRows >= 0) and (RowNum - Grid.FixedRows < Count) then
  begin
    NC := TMSCRNeutronClass(Items[RowNum - Grid.FixedRows]);
    with Grid, NC do
    begin
      Cells[1, RowNum] := FloatToStrF(Intensity, ffGeneral, 8, 4);
      case ViewMode of
        XCM_2T: begin
          Cells[2, RowNum] := FloatToStrF(StartPos, ffGeneral, 6, 4);
          Cells[3, RowNum] := FloatToStrF(PeakPos, ffGeneral, 6, 4);
          Cells[4, RowNum] := FloatToStrF(FinishPos, ffGeneral, 6, 4);
        end;
        XCM_T : begin
          Cells[2, RowNum] := FloatToStrF(StartPos / 2, ffGeneral, 6, 4);
          Cells[3, RowNum] := FloatToStrF(PeakPos / 2, ffGeneral, 6, 4);
          Cells[4, RowNum] := FloatToStrF(FinishPos / 2, ffGeneral, 6, 4);
        end;
        XCM_SINTL : if Lambda <> 0 then
        begin
          Cells[2, RowNum] := FloatToStrF(
          Sin((StartPos * pi) / (2 * 180)) / Lambda, ffGeneral, 6, 4);
          Cells[3, RowNum] := FloatToStrF(
          Sin((PeakPos * pi) / (2 * 180)) / Lambda, ffGeneral, 6, 4);
          Cells[4, RowNum] := FloatToStrF(
          Sin((FinishPos * pi) / (2 * 180)) / Lambda, ffGeneral, 6, 4);
        end else raise EMSCRNeutronCompList.Create('Wavelength undefined...')
      end;{case ViewMode of...}
    end;
  end
  else
  begin
    with Grid do begin
      Cells[1, RowNum] := '';
      Cells[2, RowNum] := '';
      Cells[3, RowNum] := '';
      Cells[4, RowNum] := '';
    end;
  end;
end;

function TMSCRNeutronCompList.GetDataFromGrid(Grid: TStringGrid): Boolean;
var i: LongInt;
begin
    Clear;
    Result := True;
    with Grid do
        for i := FixedRows to RowCount - 1 do
            if not GetRowContents(Grid, i) then Result := False;
end;

function TMSCRNeutronCompList.GetInfoCols: LongInt;
begin
    Result := 4;
end;

function TMSCRNeutronCompList.GetCopy: TObject;
begin
  Result := TMSCRNeutronCompList.Create(nil);
  CopyParameters(Result);
end;

procedure TMSCRNeutronCompList.CopyParameters(const Dest: TObject);
begin
  inherited;
  TMSCRNeutronCompList(Dest).Lambda := Lambda;
  TMSCRNeutronCompList(Dest).ViewMode := ViewMode;
end;

function TMSCRNeutronCompList.GetRowContents;
var NC: TMSCRNeutronClass;
    St: string;
begin
  Result := True;
  NC := TMSCRNeutronClass.Create(NIL);
  with NC, Grid do
  begin
    try St := Cells[1, RowNum]; Intensity := StrToFloat(St);
    except Result := False; end;
    try
      St := Cells[2, RowNum];
      case ViewMode of
        XCM_T : StartPos := StrToFloat(St) * 2;
        XCM_2T : StartPos := StrToFloat(St);
        XCM_SINTL : if Lambda <> 0 then
          StartPos := 2 * (180 / pi) * ArcSin(StrToFloat(St) * Lambda)
          else raise EMSCRNeutronCompList.Create('Wavelength undefined...');
      end; {case ViewMode of...}
    except Result := False; end;
    try
      St := Cells[3, RowNum];
      case ViewMode of
        XCM_T : PeakPos := StrToFloat(St) * 2;
        XCM_2T : PeakPos := StrToFloat(St);
        XCM_SINTL : if Lambda <> 0 then
          PeakPos := 2 * (180 / pi) * ArcSin(StrToFloat(St) * Lambda)
          else raise EMSCRNeutronCompList.Create('Wavelength undefined...');
      end; {case ViewMode of...}
    except Result := False; end;
    try
      St := Cells[4, RowNum];
      case ViewMode of
        XCM_T : FinishPos := StrToFloat(St) * 2;
        XCM_2T : FinishPos := StrToFloat(St);
        XCM_SINTL : if Lambda <> 0
          then FinishPos := 2 * (180 / pi) * ArcSin(StrToFloat(St) * Lambda)
          else raise EMSCRNeutronCompList.Create('Wavelength undefined...');
      end; {case ViewMode of...}
    except Result := False; end;
  end;
  if Result then Add(NC) else NC.Free;
end;

procedure TMSCRStructClass.CopyParameters(const Dest: TObject);
begin
  inherited;
  TMSCRStructClass(Dest).CoordExpression := CoordExpression;
  TMSCRStructClass(Dest).FixedX := FixedX;
  TMSCRStructClass(Dest).FixedY := FixedY;
  TMSCRStructClass(Dest).FixedZ := FixedZ;
end;

procedure TMSCRStructClass.CalcCoord;
var index, index2: LongInt;
    St: string;
    ErrorCode: LongInt;
    TempDouble: Double;
begin
  index := GetCharPosition(CoordExpression, ',', 1, 1);
  St := Copy(CoordExpression, 1, index - 1);
  TempDouble := CalculateSimpExpr(St, ErrorCode, GetParameterValue);
  if ErrorCode = CALC_NO_ERRORS then x := TempDouble;
  index2 := GetCharPosition(CoordExpression, ',', 1, index + 1);
  St := Copy(CoordExpression, index + 1, index2 - index - 1);
  TempDouble := CalculateSimpExpr(St, ErrorCode, GetParameterValue);
  if ErrorCode = CALC_NO_ERRORS then y := TempDouble;
  St := Copy(CoordExpression, index2 + 1, Length(CoordExpression) - index2);
  TempDouble := CalculateSimpExpr(St, ErrorCode, GetParameterValue);
  if ErrorCode = CALC_NO_ERRORS then z := TempDouble;
end;

function  TMSCRStructClass.GetParameterValue(Param: string): Double;
begin
  if Assigned(Site) then
    with Site do Result := GetParameterValue(Param)
  else Result := 0;
end;

procedure TMSCRNeutronClass.CopyParameters(const Dest: TObject);
begin
  inherited;
  TMSCRNeutronClass(Dest).NearestSinTL := NearestSinTL;
end;

procedure TMSCRSiteList.GetContentsInText;
var i: LongInt;
    TS: TMSCRSite;
begin
  if Assigned(StrList) then
  begin
    for i := 0 to Count - 1 do
    begin
      TS := TMSCRSite(Items[i]);
      TS.GetContentsInText(StrList);
    end;
  end;
end;

procedure TMSCRSiteList.CalcCoord;
var i: LongInt;
    TS: TMSCRSite;
begin
  for i := 0 to Count - 1 do
  begin
    TS := TMSCRSite(Items[i]);
    if not TS.Disabled then TS.CalcCoord;
  end;
end;

function TMSCRSiteList.GetCopy: TObject;
begin
  Result := TMSCRSiteList.Create(nil);
  CopyParameters(Result);
end;

procedure TMSCRSiteList.CopyParameters(const Dest: TObject);
begin
  inherited;
  TMSCRSiteList(Dest).GroupNumber := GroupNumber;
  TMSCRSiteList(Dest).RFactor1 := RFactor1;
  TMSCRSiteList(Dest).RFactor2 := RFactor2;
end;

procedure TMSCRSiteList.ClearParameters;
var i: LongInt;
begin
  for i := 0 to Count - 1 do TMSCRSite(Items[i]).ClearParameters;
end;

procedure TMSCRSiteList.RandomParameters;
var i: LongInt;
begin
  for i := 0 to Count - 1 do TMSCRSite(Items[i]).RandomParameters;
end;

function TMSCRSiteList.GetParametersCount: LongInt;
var i: LongInt;
    ST: TMSCRSite;
begin
  Result := 0;
  for i := 0 to Count - 1 do
  begin
    ST := TMSCRSite(Items[i]);
    if not ST.Disabled then Result := Result + ST.ParametersCount;
  end;
end;

function TMSCRSiteList.IncParameter: Boolean;

  procedure IncCurSiteParameter(var SiteNum: LongInt);
  var ST: TMSCRSite;
  begin
    if SiteNum < 0 then Exit;
    ST := TMSCRSite(Items[SiteNum]);
    if ST.IncParameter then
    begin
      SiteNum := SiteNum - 1;
      IncCurSiteParameter(SiteNum);
    end;
  end;

var SiteNum: LongInt;
begin
  SiteNum := Count - 1;
  IncCurSiteParameter(SiteNum);
  Result := SiteNum < 0;
end;

procedure TMSCRSite.GetContentsInText;
var i: LongInt;
    TC: TMSCRStructClass;
    St: string;
begin
  if Assigned(StrList) then
  begin
    St := SiteName + '>' + IntToStr(ParametersCount);
    StrList.Add(St);
    if Assigned(AtomList) and (AtomList is TComponentList) then
      with AtomList as TComponentList do
        for i := 0 to Count - 1 do
        begin
          TC := TMSCRStructClass(Items[i]);
          St := TC.CoordExpression + '>' + FloatToStr(TC.x) + '|'
                + FloatToStr(TC.y) + '|' + FloatToStr(TC.z);
          StrList.Add(St);
        end;
  end;
end;

procedure TMSCRSite.CalcCoord;
var i: LongInt;
    TC: TMSCRStructClass;
begin
  if Assigned(AtomList) then
    with AtomList do
    begin
      for i := 0 to Count - 1 do
      begin
        TC := TMSCRStructClass(Items[i]);
        TC.CalcCoord;
      end;
    end;
end;

function  TMSCRSite.GetParametersCount: LongInt;
begin
  Result := Length(FParameters)
end;

function  TMSCRSite.GetParameters(index: LongInt): Double;
begin
  if index < ParametersCount then Result := FParameters[index]
  else Result := 0;
end;

procedure TMSCRSite.SetParameters(index: LongInt; Value: Double);
begin
  if index < ParametersCount then FParameters[index] := Value;
end;

function TMSCRSite.GetParameterValue(Param: string): Double;
var i: LongInt;
begin
  for i := 0 to ParametersCount - 1 do
  begin
    if UpperCase(Param) = UpperCase(FParametersID[i]) then
    begin Result := Parameters[i]; Exit end;
  end;
  AddNewParameter(Param, 0);
  Result := 0;
end;

procedure TMSCRSite.AddNewParameter(Param: string; Value: Double);
begin
  SetLength(FParameters, Length(FParameters) + 1);
  SetLength(FParametersID, Length(FParametersID) + 1);
  FParametersID[Length(FParametersID) - 1] :=  Param;
  FParameters[Length(FParametersID) - 1] :=  Value;
end;

procedure TMSCRSite.ClearParameters;
var i: LongInt;
begin
  for i := 0 to ParametersCount - 1 do Parameters[i] := 0;
end;

procedure TMSCRSite.RandomParameters;
var i: LongInt;
begin
  Randomize;
  for i := 0 to ParametersCount - 1 do Parameters[i] := Random;
end;

destructor TMSCRSite.Destroy;
begin
  FParameters := nil;
  FParametersID := nil;
  inherited Destroy;
end; 

function TMSCRSite.GetCopy: TObject;
begin
  Result := TMSCRSite.Create(nil);
  CopyParameters(Result);
end;

procedure TMSCRSite.CopyParameters(const Dest: TObject);
var i: LongInt; 
begin
  inherited; 
  TMSCRSite(Dest).CalcCoord;
  for i := 0 to ParametersCount - 1 do
    TMSCRSite(Dest).Parameters[i] := Parameters[i]; 
  TMSCRSite(Dest).CalcCoord; 
end; 

function TMSCRSite.IncParameter: Boolean; 

  procedure IncCurParameter(var ParamNum: LongInt); 
  begin
    if ParamNum < 0 then Exit; 
    Parameters[ParamNum] := Parameters[ParamNum] + 0.01; 
    if Parameters[ParamNum] > 1 then
    begin
      Parameters[ParamNum] := 0;
      ParamNum := ParamNum - 1; 
      IncCurParameter(ParamNum); 
    end; 
  end; 

var ParamNum: LongInt; 
begin
  ParamNum := ParametersCount - 1; 
  IncCurParameter(ParamNum); 
  Result := ParamNum < 0; 
end; 

procedure TMSCRSite.SetElement(AElement: string); 
var i: LongInt; 
    SC: TAtom; 
begin
  if Assigned(AtomList) then
    for i := 0 to AtomList.Count - 1 do
    begin
      SC := TAtom(AtomList.Items[i]); 
      SC.Element := AElement;
    end; 
end; 

procedure TMSCRSite.SetNuclearScatAmpl(ANuclearScatAmpl: Double); 
var i: LongInt;
    SC: TAtom; 
begin
  if Assigned(AtomList) then
    for i := 0 to AtomList.Count - 1 do
    begin
      SC := TAtom(AtomList.Items[i]); 
      SC.NuclearScatAmpl := ANuclearScatAmpl; 
    end; 
end; 

initialization
  RegisterClass(TMSCRStructClass); 
  RegisterClass(TMSCRNeutronClass); 
  RegisterClass(TMSCRNeutronCompList); 
  DecimalSeparator := '.';
end.
