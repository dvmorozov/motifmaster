{------------------------------------------------------------------------------
    This file is part of the MotifMASTER project. This software is
    distributed under GPL (see gpl.txt for details).

    This software is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Copyright (C) 1999-2007 D.Morozov (dvmorozov@mail.ru)
------------------------------------------------------------------------------}

unit PointSetViewer;

interface

uses DataLoader, Classes, Chart, SysUtils, Series, Graphics, ComponentList,
     CheckLst, IntegralInt, TeEngine;

type

 EChartIsNotAssigned = class(Exception);
 EComponentIsNotAssigned = class(Exception);
 EOnlyOnePointsSetMustBeSelected = class(Exception);
 EPointsSetMustBePresent = class(Exception);
 EPointsSetNotFound = class(Exception);
 EUnknownError = class(Exception);

const
 XCM_2T    = 0;
 XCM_T     = 1;
 XCM_SINTL = 2;

type

 TIIViewer = class(TComponent)
 private
  IIMaker : TIntegralIntMaker;
  Chart : TChart;
  CheckListBox : TCheckListBox;
  PointsSetList : TComponentList;
  FXCoordMode : LongInt;
  MaxX,MinX,MaxY,MinY : Double;
  ViewMarkers : Boolean;
  procedure   SetXCoordMode(AMode : LongInt);
 public
  procedure   PlotGaussians(Sender : TObject; GaussPointsSetList : TComponentList);
  procedure   PlotSelectedPoints(Sender : TObject; SelectedPoints : TNeutronPointsSet);
  procedure   PlotNeutronPoints(Sender : TObject; NeutronPoints : TNeutronPointsSet);
  procedure   PlotSelectedArea(Sender : TObject; SelectedArea : TNeutronPointsSet);
  procedure   PlotBackground(Sender : TObject; BackgroundPoints : TNeutronPointsSet);
  procedure   PlotGaussProfile(Sender : TObject; GaussProfile : TNeutronPointsSet);
  procedure   Refresh(Sender : TObject);
  procedure   RefreshPointsSet(Sender : TObject; PointsSet : TNeutronPointsSet);
  procedure   Clear(Sender : TObject);
  procedure   Hide(Sender : TObject; PointsSet : TNeutronPointsSet);

  procedure   SetChart(AChart : TChart);
  procedure   SetCheckListBox(ACheckListBox : TCheckListBox);
  procedure   SetIIMaker(AIIMaker : TIntegralIntMaker);
  procedure   SetViewMarkers(AViewMarkers : Boolean);
  procedure   ViewAllMarkers(AViewMarkers : Boolean);
  procedure   Plot;
  procedure   RemoveSelectedPoints;
  function    GetActiveCurve : LongInt;
  function    GetActivePointsSet : TNeutronPointsSet;
  function    GetPointsSet(ActiveNumber : LongInt) : TNeutronPointsSet;
  function    GetMaxX : Double;
  function    GetMinX : Double;
  function    GetMaxY : Double;
  function    GetMinY : Double;
  procedure   GetMinMax(var MinX,MaxX,MinY,MaxY : Double);
  constructor Create(AOwner : TComponent);
  destructor  Destroy; override;
  property    XCoordMode : LongInt read FXCoordMode write SetXCoordMode;
 end;

implementation

{========================== TIIViewer ==================================}
procedure TIIViewer.SetChart(AChart : TChart);
begin
 Chart := AChart;
end;

procedure TIIViewer.SetCheckListBox(ACheckListBox : TCheckListBox);
begin
 CheckListBox := ACheckListBox;
end;

procedure TIIViewer.SetIIMaker(AIIMaker : TIntegralIntMaker);
begin
 IIMaker := AIIMaker;
 IIMaker.OnPlotGaussians := PlotGaussians;
 IIMaker.OnPlotSelectedPoints := PlotSelectedPoints;
 IIMaker.OnPlotNeutronPoints := PlotNeutronPoints;
 IIMaker.OnPlotSelectedArea := PlotSelectedArea;
 IIMaker.OnPlotBackground := PlotBackground;
 IIMaker.OnPlotGaussProfile := PlotGaussProfile;
 IIMaker.OnRefresh := Refresh;
 IIMaker.OnRefreshPointsSet := RefreshPointsSet;
 IIMaker.OnClear := Clear;
 IIMaker.OnHide := Hide;
end;

const

ColorPalette : array[1..16] of TColor = (clRed, clGreen, clYellow, clBlue,
                                         {clWhite}clBlack, clGray, clFuchsia, clTeal,
                                         clNavy, clMaroon, clLime, clOlive,
                                         clPurple, clSilver, clAqua, clBlack);

procedure TIIViewer.Plot;
var SA : TNeutronPointsSet;
    LS : TChartSeries;
    i,j : LongInt;
begin
 if not Assigned(Chart) then
  begin
   raise EChartIsNotAssigned.Create('Chart is not assigned...');
   Exit;
  end;
 if not Assigned(IIMaker) then
  begin
   raise EComponentIsNotAssigned.Create('Integral intensities maker is not assigned...');
   Exit;
  end;
 for j := 0 to PointsSetList.Count - 1 do
  begin
   SA := TNeutronPointsSet(PointsSetList.Items[j]);
   LS := Chart.Series[j];
   LS.HorizAxis := aBottomAxis;
   LS.Clear;
   with SA do
    for i := 0 to PointsCount - 1 do
     case XCoordMode of
      XCM_2T : LS.AddXY(Point2T[i],PointIntensity[i],'',LS.SeriesColor);
       XCM_T : LS.AddXY(PointT[i],PointIntensity[i],'',LS.SeriesColor);
   XCM_SINTL : LS.AddXY(PointSinTL[i],PointIntensity[i],'',LS.SeriesColor);
     end;{case XCoordMode of...}
  end;{for j := 0 to PointsSetList.Count - 1 do...}
 ViewAllMarkers(ViewMarkers);
end;

procedure TIIViewer.PlotSelectedArea(Sender : TObject; SelectedArea : TNeutronPointsSet);
var LS : TLineSeries;
    i : LongInt;
begin
 if not Assigned(Chart) then
  begin
   raise EChartIsNotAssigned.Create('Chart is not assigned...');
   Exit;
  end;
 if not Assigned(SelectedArea) then
  begin
   raise EPointsSetIsNotAssigned.Create('Selected area is not assigned...');
   Exit
  end;
 LS := TLineSeries.Create(nil);
 PointsSetList.Add(SelectedArea);
 Chart.AddSeries(LS);
 if Assigned(CheckListBox) then
  begin
   CheckListBox.Items.AddObject('Selected area',LS);
   LS.Title := 'Selected area';
   LS.Pointer.VertSize := 2;
   LS.Pointer.HorizSize := 2;
   LS.SeriesColor := clRed;
   CheckListBox.Checked[CheckListBox.Items.IndexOfObject(LS)] := True;
  end;
 Plot;
end;

procedure TIIViewer.PlotGaussians(Sender : TObject; GaussPointsSetList : TComponentList);
var LS : TLineSeries;
    SA : TNeutronPointsSet;
    i,j : LongInt;
begin
 if not Assigned(Chart) then
  begin
   raise EChartIsNotAssigned.Create('Chart is not assigned...');
   Exit;
  end;
 if not Assigned(GaussPointsSetList) then
  begin
   raise EComponentIsNotAssigned.Create('Gaussians list is not assigned...');
   Exit
  end;
 for j := 0 to GaussPointsSetList.Count - 1 do
  begin
   SA := TNeutronPointsSet(GaussPointsSetList.Items[j]);
   if SA is TGaussPointsSet then
    with SA as TGaussPointsSet do
     begin
       LS := TLineSeries.Create(nil);
       Chart.AddSeries(LS);
       PointsSetList.Add(SA);
       if Assigned(CheckListBox) then
        begin
         CheckListBox.Items.AddObject('Gaussian '+IntToStr(j+1),LS);
         LS.Title := 'Gaussian '+IntToStr(j+1);
         LS.Pointer.VertSize := 2;
         LS.Pointer.HorizSize := 2;
         CheckListBox.Checked[CheckListBox.Items.IndexOfObject(LS)] := True;
         if j + 1 <= 16 then LS.SeriesColor := ColorPalette[j + 1]
          else LS.SeriesColor := ColorPalette[(j + 1) mod 16];
        end;
     end;{with SA as TGaussPointsSet do...}
  end;{for j := 0 to GL.Count - 1 do...}
 Plot;
end;

procedure TIIViewer.Clear(Sender : TObject);
begin
 if Assigned(Chart) then
  while Chart.SeriesCount <> 0 do Chart.Series[0].Free
 else raise EChartIsNotAssigned.Create('Chart is not assigned...');
 if Assigned(CheckListBox) then
  CheckListBox.Items.Clear;
 PointsSetList.Clear;
end;

procedure TIIViewer.Hide(Sender : TObject; PointsSet : TNeutronPointsSet);
var Index : LongInt;
begin
 Index := PointsSetList.IndexOf(PointsSet);
 if Index <> -1 then
  begin
   if Assigned(CheckListBox) then CheckListBox.Items.Delete(Index);
   if Assigned(Chart) then
    if Index < Chart.SeriesCount then Chart.SeriesList[Index].Free;
   PointsSetList.Remove(PointsSet);
  end;
end;

procedure TIIViewer.Refresh(Sender : TObject);
var i,j : LongInt;
    LS : TChartSeries;
    NS : TNeutronPointsSet;
begin
 if not Assigned(Chart) then
  begin
   raise EChartIsNotAssigned.Create('Chart is not assigned...');
   Exit;
  end;
 if not Assigned(PointsSetList) then
  begin
   raise EComponentIsNotAssigned.Create('Points list is not assigned...');
   Exit;
  end;
 with Chart do
  for i := 0 to SeriesCount - 1 do
   begin
    LS := Series[i];
    LS.HorizAxis := aBottomAxis;
    NS := TNeutronPointsSet(PointsSetList.Items[i]);
    with NS do
     for j := 0 to PointsCount - 1 do
      begin
       LS.YValue[j] := PointIntensity[j];
       case XCoordMode of
        XCM_T : LS.XValue[j] := PointT[j];
       XCM_2T : LS.XValue[j] := Point2T[j];
    XCM_SinTL : LS.XValue[j] := PointSinTL[j];
       end;{case XCoordMode of...}
      end;
   end;{for i := 0 to SeriesCount - 1 do...}
 ViewAllMarkers(ViewMarkers);
end;

procedure TIIViewer.RefreshPointsSet(Sender : TObject; PointsSet : TNeutronPointsSet);
var Index,j : LongInt;
    LS : TChartSeries;
    NS : TNeutronPointsSet;
begin
 if not Assigned(Chart) then
  begin
   raise EChartIsNotAssigned.Create('Chart is not assigned...');
   Exit;
  end;
 if not Assigned(PointsSet) then
  begin
   raise EPointsSetIsNotAssigned.Create('Points set is not assigned...');
   Exit;
  end;
 Index := PointsSetList.IndexOf(PointsSet);
 if Index <> -1 then
  begin
   with Chart do
    LS := Series[Index];
    LS.HorizAxis := aBottomAxis;
    LS.Clear;
    NS := TNeutronPointsSet(PointsSetList.Items[Index]);
    with NS do
     for j := 0 to PointsCount - 1 do
      begin
       case XCoordMode of
        XCM_T : LS.AddXY(PointT[j],PointIntensity[j],'',LS.SeriesColor);
       XCM_2T : LS.AddXY(Point2T[j],PointIntensity[j],'',LS.SeriesColor);
    XCM_SinTL : LS.AddXY(PointSinTL[j],PointIntensity[j],'',LS.SeriesColor);
       end;{case XCoordMode of...}
      end;
  end{with Chart do...}
 else raise EPointsSetNotFound.Create('Points set not found...');
end;

procedure TIIViewer.PlotSelectedPoints(Sender : TObject; SelectedPoints : TNeutronPointsSet);
var LS : TPointSeries;
    i : LongInt;
begin
 if not Assigned(Chart) then
  begin
   raise EChartIsNotAssigned.Create('Chart is not assigned...');
   Exit;
  end;
 if not Assigned(SelectedPoints) then
  begin
   raise EPointsSetIsNotAssigned.Create('Selected points set is not assigned...');
   Exit
  end;
 if PointsSetList.IndexOf(SelectedPoints) = -1 then
  begin
   LS := TPointSeries.Create(nil);
   Chart.AddSeries(LS);
   LS.Pointer.VertSize := 3;
   LS.Pointer.HorizSize := 3;
   LS.SeriesColor := clGreen;
   PointsSetList.Add(SelectedPoints);
   if Assigned(CheckListBox) then
    begin
     CheckListBox.Items.AddObject('Selected points',LS);
     LS.Title := 'Selected points';
     CheckListBox.Checked[CheckListBox.Items.IndexOfObject(LS)] := True;
    end;
  end
 else
  begin
   LS := TPointSeries(Chart.Series[PointsSetList.IndexOf(SelectedPoints)]);
   LS.Clear;
  end;
 Plot;
end;

procedure TIIViewer.RemoveSelectedPoints;
var GP : TNeutronPointsSet;
    LS : TPointSeries;
    i : LongInt;
begin
 if not Assigned(Chart) then
  begin
   raise EChartIsNotAssigned.Create('Chart is not assigned...');
   Exit;
  end;
 if not Assigned(IIMaker) then
  begin
   raise EComponentIsNotAssigned.Create('Integral intensities maker is not assigned...');
   Exit;
  end;
 try
  GP := IIMaker.GetSelectedPoints;
 except raise; Exit end;
 if PointsSetList.IndexOf(GP) <> -1 then
  begin
   Chart.Series[PointsSetList.IndexOf(GP)].Free;
   if Assigned(CheckListBox) then
     CheckListBox.Items.Delete(PointsSetList.IndexOf(GP));
   PointsSetList.Remove(GP);
  end;
 Plot;
end;

procedure TIIViewer.PlotGaussProfile(Sender : TObject; GaussProfile : TNeutronPointsSet);
var LS : TLineSeries;
    i : LongInt;
begin
 if not Assigned(Chart) then
  begin
   raise EChartIsNotAssigned.Create('Chart is not assigned...');
   Exit;
  end;
 if not Assigned(GaussProfile) then
  begin
   raise EPointsSetIsNotAssigned.Create('Gauss profile is not assigned...');
   Exit;
  end;
 LS := TLineSeries.Create(nil);
 LS.Pointer.VertSize := 2;
 LS.Pointer.HorizSize := 2;
 LS.SeriesColor := clBlack;
 Chart.AddSeries(LS);
 PointsSetList.Add(GaussProfile);
 if Assigned(CheckListBox) then
  begin
   CheckListBox.Items.AddObject('Summarized gaussians profile',LS);
   LS.Title := 'Summarized gaussians profile';
   CheckListBox.Checked[CheckListBox.Items.IndexOfObject(LS)] := True;
  end;
 Plot;
end;

procedure TIIViewer.PlotBackground(Sender : TObject; BackgroundPoints : TNeutronPointsSet);
begin
end;

procedure TIIViewer.PlotNeutronPoints(Sender : TObject; NeutronPoints : TNeutronPointsSet);
var LS : TLineSeries;
    i : LongInt;
begin
 if not Assigned(Chart) then
  begin
   raise EChartIsNotAssigned.Create('Chart is not assigned...');
   Exit;
  end;
 if not Assigned(NeutronPoints) then
  begin
   raise EpointsSetIsNotAssigned.Create('Neutron points set is not assigned...');
   Exit;
  end;
 LS := TLineSeries.Create(nil);
 LS.Pointer.VertSize := 2;
 LS.Pointer.HorizSize := 2;
 LS.SeriesColor := clRed;
 Chart.AddSeries(LS);
 PointsSetList.Add(NeutronPoints);
 if Assigned(CheckListBox) then
  begin
   CheckListBox.Items.AddObject('Diffraction pattern',LS);
   LS.Title := 'Diffraction pattern';
   CheckListBox.Checked[CheckListBox.Items.IndexOfObject(LS)] := True;
  end;
 Plot;
end;

constructor TIIViewer.Create(AOwner : TComponent);
begin
 inherited Create(AOwner);
 PointsSetList := TComponentList.Create(nil);
 PointsSetList.SetState(cfPassive);
 FXCoordMode := 0;
end;

destructor  TIIViewer.Destroy;
begin
 PointsSetList.Free;
 inherited Destroy;
end;

function   TIIViewer.GetActiveCurve : LongInt;
var i : LongInt;
    TS : TChartSeries;
    ActiveNumber : LongInt;
    IsActive : Boolean;
begin
 Result := -1;
 if not Assigned(Chart) then
  begin
   raise EChartIsNotAssigned.Create('Chart is not assigned...');
   Exit;
  end;
 if not Assigned(IIMaker) then
  begin
   raise EComponentIsNotAssigned.Create('Integral intensities maker is not assigned...');
   Exit;
  end;
 with Chart do
  begin
   IsActive := False;
   ActiveNumber := -1;
   if SeriesCount = 0 then
    begin
     EPointsSetMustBePresent.Create('Points set must be present...');
     Exit;
    end;
   for i := 0 to SeriesCount - 1 do
    begin
     TS := Series[i];
     if TS.Active then
      begin
       if IsActive then
        begin
         raise EOnlyOnePointsSetMustBeSelected.Create('Only one points set must be selected...');
         Exit;
        end
       else
        begin
         IsActive := True;
         ActiveNumber := i;
        end;
      end;{if TS.Active then...}
    end;{for i := 0 to SeriesCount - 1 do...}
  end;{with Chart do...}
 if ActiveNumber = -1 then
  raise EUnknownError.Create('Unknown error...');
 Result := ActiveNumber;
end;

function   TIIViewer.GetActivePointsSet : TNeutronPointsSet;
var ActiveNumber : LongInt;
begin
 try
  ActiveNumber := GetActiveCurve;
 except raise; Exit end;
 Result := TNeutronPointsSet(PointsSetList.Items[ActiveNumber]);
end;

function   TIIViewer.GetPointsSet(ActiveNumber : LongInt) : TNeutronPointsSet;
begin
 Result := TNeutronPointsSet(PointsSetList.Items[ActiveNumber]);
end;

procedure  TIIViewer.SetXCoordMode(AMode : LongInt);
var MinX,MaxX,MinY,MaxY : Double;
begin
 FXCoordMode := AMode;
 Plot;
 if not Assigned(Chart) then
  begin
   raise EChartIsNotAssigned.Create('Chart is not assigned...');
   Exit;
  end;
 GetMinMax(MinX,MaxX,MinY,MaxY);
 Chart.BottomAxis.SetMinMax(MinX,MaxX);
 Chart.LeftAxis.SetMinMax(MinY,MaxY);
end;

function   TIIViewer.GetMaxX : Double;
begin
 GetMinMax(MinX,MaxX,MinY,MaxY);
 Result := MaxX;
end;

function   TIIViewer.GetMinX : Double;
begin
 GetMinMax(MinX,MaxX,MinY,MaxY);
 Result := MinX;
end;

function   TIIViewer.GetMaxY : Double;
begin
 GetMinMax(MinX,MaxX,MinY,MaxY);
 Result := MaxY;
end;

function   TIIViewer.GetMinY : Double;
begin
 GetMinMax(MinX,MaxX,MinY,MaxY);
 Result := MinY;
end;

procedure  TIIViewer.GetMinMax(var MinX,MaxX,MinY,MaxY : Double);
var i,j : LongInt;
    PS : TNeutronPointsSet;
begin
 MinX := MAX_VALUE; MaxX := MIN_VALUE;
 MinY := MAX_VALUE; MaxY := MIN_VALUE;
 for i := 0 to PointsSetList.Count - 1 do
  begin
   if PointsSetList.Items[i] is TPointsSet then
    begin
     PS := TNeutronPointsSet(PointsSetList.Items[i]);
     for j := 0 to PS.PointsCount - 1 do
      begin
       if PS.PointXCoord[j] > MaxX then
        case XCoordMode of
         XCM_T     : MaxX := PS.PointT[j];
        XCM_2T     : MaxX := PS.Point2T[j];
     XCM_SinTL     : MaxX := PS.PointSinTL[j];
        end;
       if PS.PointXCoord[j] < MinX then
        case XCoordMode of
         XCM_T     : MinX := PS.PointT[j];
        XCM_2T     : MinX := PS.Point2T[j];
     XCM_SinTL     : MinX := PS.PointSinTL[j];
        end;
       if PS.PointYCoord[j] > MaxY then MaxY := PS.PointYCoord[j];
       if PS.PointYCoord[j] < MinY then MinY := PS.PointYCoord[j];
      end;
    end;{if PointsSetList.Items[i] is TPointsSet then...}
  end;
end;

procedure TIIViewer.SetViewMarkers(AViewMarkers : Boolean);
begin
 ViewMarkers := AViewMarkers;
 ViewAllMarkers(ViewMarkers);
end;

procedure TIIViewer.ViewAllMarkers(AViewMarkers : Boolean);
var i : LongInt;
    TS : TChartSeries;
begin
 if not Assigned(Chart) then
  begin
   raise EChartIsNotAssigned.Create('Chart is not assigned...');
   Exit;
  end;
 for i := 0 to Chart.SeriesCount - 1 do
  begin
   TS := Chart.Series[i];
   if TS is TLineSeries then
    with TS as TLineSeries do Pointer.Visible := AViewMarkers;
  end;
end;

end.
