{------------------------------------------------------------------------------
    This file is part of the MotifMASTER project. This software is
    distributed under GPL (see gpl.txt for details).

    This software is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Copyright (C) 1999-2007 D.Morozov (dvmorozov@mail.ru)
------------------------------------------------------------------------------}

unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, TeeProcs, TeEngine, Chart, TeeFunci, Series, StdCtrls, Menus,
  DataLoader, PointSetViewer, ToolWin, ComCtrls, IntegralInt, ImgList,
  MainCalcProcess, ComponentList, Grids, NumericGrid, CheckLst, MSCRDataClasses;

type
  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    OpenDialog1: TOpenDialog;
    View1: TMenuItem;
    ZoomIn1: TMenuItem;
    ZoomOut1: TMenuItem;
    Operation1: TMenuItem;
    Smoothing1: TMenuItem;
    ImageList1: TImageList;
    Find_Gaussians1: TMenuItem;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    NumericGrid1: TNumericGrid;
    N2: TMenuItem;
    MovePeaksToResults1: TMenuItem;
    N3: TMenuItem;
    Exit1: TMenuItem;
    Splitter1: TSplitter;
    StopGaussesCalculation1: TMenuItem;
    N5: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    Mode1: TMenuItem;
    N1: TMenuItem;
    Theta1: TMenuItem;
    N2Theta1: TMenuItem;
    SinThetaLambda1: TMenuItem;
    StatusBar1: TStatusBar;
    PanelLeft: TPanel;
    Chart1: TChart;
    Series1: TLineSeries;
    CoolBar2: TCoolBar;
    ToolBar2: TToolBar;
    PanelRight: TPanel;
    CheckListBox1: TCheckListBox;
    CoolBar3: TCoolBar;
    ToolBar3: TToolBar;
    Open_But: TToolButton;
    ToolButton1: TToolButton;
    Zoom_In_But: TToolButton;
    Zoom_Out_But: TToolButton;
    ToolButton4: TToolButton;
    CoolBar1: TCoolBar;
    ToolBar1: TToolBar;
    Save_But: TToolButton;
    View_Mode_But: TToolButton;
    ViewModeMenu: TPopupMenu;
    Theta2: TMenuItem;
    N2Theta2: TMenuItem;
    SinThetaLambda2: TMenuItem;
    ImageList2: TImageList;
    ToolButton9: TToolButton;
    Copy_But: TToolButton;
    Delete_But: TToolButton;
    Select_Area_Limits_But: TToolButton;
    Select_Positions_of_Gaussians_But: TToolButton;
    Selection1: TMenuItem;
    Select_Area_Limits1: TMenuItem;
    Select_Characteristic_Points_of_Peak1: TMenuItem;
    Select_Gaussian_Limits1: TMenuItem;
    Select_Positions_of_Gaussians1: TMenuItem;
    N7: TMenuItem;
    Select_Area1: TMenuItem;
    Return_to_Total_Profile1: TMenuItem;
    SaveResultsAsText1: TMenuItem;
    N4: TMenuItem;
    View_Markers1: TMenuItem;
    SaveDialog1: TSaveDialog;
    Edit1: TMenuItem;
    Copy1: TMenuItem;
    Delete1: TMenuItem;
    N6: TMenuItem;
    Select_All1: TMenuItem;
    Table_View_Mode: TToolButton;
    ToolButton3: TToolButton;
    ToolButton5: TToolButton;
    SetWavelength1: TMenuItem;
    procedure Open1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure ZoomIn1Click(Sender: TObject);
    procedure ZoomOut1Click(Sender: TObject);
    procedure Smoothing1Click(Sender: TObject);
    procedure Chart1ClickSeries(Sender: TCustomChart; Series: TChartSeries;
      ValueIndex: Integer; Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
    procedure Find_Gaussians1Click(Sender: TObject);
    procedure Chart1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure CheckListBox1KeyPress(Sender: TObject; var Key: Char);
    procedure CheckListBox1Click(Sender: TObject);
    procedure StopGaussesCalculation1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure Select_Area1Click(Sender: TObject);
    procedure MovePeaksToResults1Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure ViewofTotalProfile1Click(Sender: TObject);
    procedure SinThetaLambda1Click(Sender: TObject);
    procedure Theta1Click(Sender: TObject);
    procedure N2Theta1Click(Sender: TObject);
    procedure Select_Area_Limits1Click(Sender: TObject);
    procedure Select_Characteristic_Points_of_Peak1Click(Sender: TObject);
    procedure Select_Gaussian_Limits1Click(Sender: TObject);
    procedure Select_Positions_of_Gaussians1Click(Sender: TObject);
    procedure View_Markers1Click(Sender: TObject);
    procedure SaveResultsAsText1Click(Sender: TObject);
    procedure Copy_ButClick(Sender: TObject);
    procedure Delete1Click(Sender: TObject);
    procedure Select_All1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure SetWavelength1Click(Sender: TObject);
  private
    FSelectedAreaMode : Boolean;
    procedure SetSelectedAreaMode(ASelectedAreaMode : Boolean);
    procedure GaussiansFindingDone(Sender : TObject);
  public
   PointCounter : LongInt;
   SelectionMode : LongInt;
   DataLoader : TDataLoader;
   IIViewer : TIIViewer;
   IIMaker : TIntegralIntMaker;
   MainCalcProcess : TMainCalcProcess;
   ActiveNumber : LongInt;
   ResultedIntSet :TMSCRNeutronCompList;
   Modified : Boolean;
   procedure CheckListBoxChanged;
   procedure ViewResultedIntSet(Sender : TObject; ResultedIntSet : TMSCRNeutronCompList);
   procedure ShowHint(Sender : TObject);
   procedure SaveResults;
   property  SelectedAreaMode : Boolean read FSelectedAreaMode write SetSelectedAreaMode;
  end;

var
  Form1: TForm1;

const

  STR_CHART_NORMAL_HINT  : string = 'the left mouse button - Zoom, the right mouse button - Move...';

implementation

uses Unit3, Unit12;

{$R *.DFM}

procedure TForm1.Open1Click(Sender: TObject);
begin
 with OpenDialog1 do
  begin
   if Execute then
    begin
     if UpperCase(ExtractFileExt(FileName)) = '.DAT' then
      begin
       DataLoader.Free;
       DataLoader := TDATFileLoader.Create(nil);
       DataLoader.LoadDataSet(FileName);
       SelectedAreaMode := False;
       IIMaker.SetNeutronPointsSet(TNeutronPointsSet(DataLoader.GetPointsSet));
       IIViewer.XCoordMode := XCM_2T;
      end;
    end;
  end;{with OpenDialog1 do...}
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 if Assigned(MainCalcProcess) then MainCalcProcess.Suspend;
 MainCalcProcess.Free;
 DataLoader.Free;
 IIViewer.Free;
 IIMaker.Free;
 ResultedIntSet.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
 IIMaker := TIntegralIntMaker.Create(nil);
 IIViewer := TIIViewer.Create(nil);
 IIViewer.SetChart(Chart1);
 IIViewer.SetCheckListBox(CheckListBox1);
 IIViewer.SetIIMaker(IIMaker);
 IIViewer.SetViewMarkers(View_Markers1.Checked);
 ActiveNumber := -1;
 SelectedAreaMode := False;
 ResultedIntSet := TMSCRNeutronCompList.Create(nil);
 ResultedIntSet.GridAssign(NumericGrid1);
 IIMaker.SetResultedIntSet(ResultedIntSet);
 IIMaker.OnViewResultedIntSet := ViewResultedIntSet;
 IIMaker.OnGaussiansFindingDone := GaussiansFindingDone;
 Application.OnHint := ShowHint;
 SelectionMode := 0;
 SelectedAreaMode := False;
 PointCounter := 0;
 Chart1.Hint := STR_CHART_NORMAL_HINT;
 while Chart1.SeriesCount <> 0 do Chart1.Series[0].Free;
 Modified := False; 
end;

procedure TForm1.ZoomIn1Click(Sender: TObject);
begin
 Chart1.ZoomPercent(120);
end;

procedure TForm1.ZoomOut1Click(Sender: TObject);
begin
 Chart1.ZoomPercent(80);
end;

procedure TForm1.Smoothing1Click(Sender: TObject);
var ActivePointsSet : TNeutronPointsSet;
begin
 try
  ActivePointsSet := IIViewer.GetActivePointsSet;
 except raise; Exit end;
 IIMaker.Smoothing(ActivePointsSet);
end;

procedure TForm1.Chart1ClickSeries(Sender: TCustomChart;
  Series: TChartSeries; ValueIndex: Integer; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var XValue,YValue : Double;
    NS : TPointsSet;
begin
 if ActiveNumber <> -1 then
  if Series = Chart1.Series[ActiveNumber] then
   begin
    with Chart1 do
     begin
      case SelectionMode of
       0 : Exit;
       1 : begin
            case PointCounter of
             0 : Chart1.Hint := 'Now you can pick a second point - "FINISH"...';
             1 : Chart1.Hint := 'Now you can pick the menu item "Select Area"...';
             2 : Exit;
            end;
            Inc(PointCounter);
           end;
       2 : begin
            case PointCounter of
             0 : Chart1.Hint := 'Now you can pick a second point - "PEAK"...';
             1 : Chart1.Hint := 'Now you can pick a third point - "FINISH"...';
             2 : Chart1.Hint := 'Now you can pick the menu item "Move Peak to Results"...';
             3 : Exit;
            end;
            Inc(PointCounter);
           end;
       3 : begin
            case PointCounter of
             0 : Chart1.Hint := 'Now you can pick a second point - "FINISH"...';
             1 : Chart1.Hint := 'Now you can pick the menu item "Move Peak to Results"...';
             2 : Exit;
            end;
            Inc(PointCounter);
           end;
       4 : begin
            Chart1.Hint := 'Now you can pick a next point or the menu item "Find gaussians"...';
            Inc(PointCounter);
           end;
      end;
      try
       NS := IIViewer.GetPointsSet(ActiveNumber);
      except raise; Exit end;
      XValue := NS.PointXCoord[ValueIndex];
      YValue := NS.PointYCoord[ValueIndex];
      IIMaker.AddNewPointToPointsSet(IIMaker.GetSelectedPoints,XValue,YValue);
     end;{with Chart1 do...}
   end;{if Series = Chart1.Series[ActiveNumber] then...}
end;

procedure TForm1.Find_Gaussians1Click(Sender: TObject);
begin
 if not SelectedAreaMode then
  begin
   MessageDlg('Area must be selected...',mtWarning,[mbOk],0);
   Exit;
  end;
 MainCalcProcess := TMainCalcProcess.Create(True);
 IIMaker.CreateGaussPointsList;
 IIMaker.SetMainCalcProcess(MainCalcProcess);
 MainCalcProcess.SetCurrentTask(IIMaker.FindGausses);
 MainCalcProcess.Resume;
 Chart1.Hint := STR_CHART_NORMAL_HINT;
 Select_positions_of_Gaussians1.Checked := False;
end;

procedure TForm1.Chart1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var i : LongInt;
begin
 with Chart1 do
  for i := 0 to SeriesCount - 1 do
   if Series[i].GetCursorValueIndex <> -1 then
    begin Screen.Cursor := crCross; Exit end;
 Screen.Cursor := crArrow;
end;

procedure TForm1.CheckListBox1KeyPress(Sender: TObject; var Key: Char);
begin
 CheckListBoxChanged
end;

procedure TForm1.CheckListBoxChanged;
var i : LongInt;
    TS : TChartSeries;
begin
 with CheckListBox1 do
  for i := 0 to Items.Count - 1 do
   begin
    if Assigned(Items.Objects[i]) then
     if Items.Objects[i] is TChartSeries then
      begin
       TS := TChartSeries(Items.Objects[i]);
       TS.Active := Checked[i];
      end;
   end;
end;

procedure TForm1.CheckListBox1Click(Sender: TObject);
begin
 CheckListBoxChanged
end;

procedure TForm1.StopGaussesCalculation1Click(Sender: TObject);
begin
 if IIMaker.IsGaussesFinding then IIMaker.StopFindGausses;
end;

procedure TForm1.Exit1Click(Sender: TObject);
begin
 Close;
end;

procedure TForm1.Select_Area1Click(Sender: TObject);
var SP : TNeutronPointsSet;
    NP : TNeutronPointsSet;
    MinX,MaxX,MinY,MaxY : Double;
begin
 if SelectedAreaMode then
  begin
   MessageDlg('Area is already selected...',mtWarning,[mbOk],0);
   Exit;
  end;
 try
  SP := IIMaker.GetSelectedPoints;
 except raise; Exit end;
 if SP.PointsCount <> 2 then
  begin
   MessageDlg('Two limiting points must be selected...',mtWarning,[mbOK],0);
   Exit;
  end;
 try
  NP := IIViewer.GetPointsSet(ActiveNumber);
 except raise; Exit end;
 SP.Sort;
 SelectedAreaMode := True;
 Chart1.Hint := STR_CHART_NORMAL_HINT;
 Select_Area_Limits1.Checked := False;
 IIMaker.SelectArea(NP,NP.IndexOfValueX(SP.PointXCoord[0]),
                       NP.IndexOfValueX(SP.PointXCoord[1]));
 IIMaker.BackgroundSubtraction;
 IIViewer.GetMinMax(MinX,MaxX,MinY,MaxY);
 Chart1.BottomAxis.SetMinMax(MinX,MaxX);
 Chart1.LeftAxis.SetMinMax(MinY,MaxY);
end;

procedure TForm1.MovePeaksToResults1Click(Sender: TObject);
var NS : TNeutronPointsSet;
    SP : TNeutronPointsSet;
begin
 if not SelectedAreaMode then
  begin
   MessageDlg('Area must be selected...',mtWarning,[mbOk],0);
   Exit;
  end;
 try
  NS := IIViewer.GetPointsSet(ActiveNumber);
 except raise; Exit end;
 try
  SP := IIMaker.GetSelectedPoints;
 except raise; Exit end;
 SP.Sort;
 if NS is TGaussPointsSet then
  begin
   if SP.PointsCount = 2 then
    begin
     IIViewer.RemoveSelectedPoints;
     IIMaker.AddGaussToResultedIntSet(NS,NS.IndexOfValueX(SP.PointXCoord[0]),
                                         NS.IndexOfValueX(SP.PointXCoord[1]));
     Select_Gaussian_Limits1.Checked := False;
     Chart1.Hint := STR_CHART_NORMAL_HINT;
     Modified := True;
    end
   else MessageDlg('Two points must be selected on a gaussian: "START","FINISH".',mtInformation,[mbOk],0);
   Exit;
  end;
 if NS is TNeutronPointsSet then
  begin
   if SP.PointsCount = 3 then
    begin
     IIViewer.RemoveSelectedPoints;
     IIMaker.AddToResultedIntSet(NS,NS.IndexOfValueX(SP.PointXCoord[0]),
                                    NS.IndexOfValueX(SP.PointXCoord[1]),
                                    NS.IndexOfValueX(SP.PointXCoord[2]));
     Select_Characteristic_Points_of_Peak1.Checked := False;
     Chart1.Hint := STR_CHART_NORMAL_HINT;
     Modified := True;
    end
   else MessageDlg('Three points must be selected on the profile: "START","PEAK","FINISH".',mtInformation,[mbOk],0);
  end;
end;

procedure TForm1.About1Click(Sender: TObject);
begin
    AboutBox.ShowModal;
end;

procedure TForm1.ViewofTotalProfile1Click(Sender: TObject);
var MinX,MaxX,MinY,MaxY : Double;
begin
 SelectedAreaMode := False;
 IIMaker.ReturnToTotalProfile;
 Chart1.Hint := STR_CHART_NORMAL_HINT;
 IIViewer.GetMinMax(MinX,MaxX,MinY,MaxY);
 Chart1.BottomAxis.SetMinMax(MinX,MaxX);
 Chart1.LeftAxis.SetMinMax(MinY,MaxY);
end;

procedure TForm1.SinThetaLambda1Click(Sender: TObject);
var SaveDecimalSeparator : Char;
begin
 if IIMaker.GetWaveLength = 0 then
  begin
   if InputWavelengthDlg.ShowModal = mrOk then
    begin
     SaveDecimalSeparator := DecimalSeparator;
     DecimalSeparator := '.';
     Screen.Cursor := crHourGlass;
     IIMaker.SetWaveLength(StrToFloat(InputWavelengthDlg.WavelengthValueEdit.Text));
     Screen.Cursor := crDefault;
     DecimalSeparator := SaveDecimalSeparator;
     IIViewer.XCoordMode := XCM_SINTL;
     ResultedIntSet.ViewMode := XCM_SINTL;
     ResultedIntSet.GridAssign(NumericGrid1);
     SinThetaLambda1.Checked := True;
     SinThetaLambda2.Checked := True;
    end;
  end{if IIMaker.GetWaveLength = 0 then...}
 else
  begin
   IIViewer.XCoordMode := XCM_SINTL;
   ResultedIntSet.ViewMode := XCM_SINTL;
   ResultedIntSet.GridAssign(NumericGrid1);
   SinThetaLambda1.Checked := True;
   SinThetaLambda2.Checked := True;
  end;
end;

procedure TForm1.Theta1Click(Sender: TObject);
begin
 IIViewer.XCoordMode := XCM_T;
 ResultedIntSet.ViewMode := XCM_T;
 ResultedIntSet.GridAssign(NumericGrid1);
 Theta1.Checked := True;
 Theta2.Checked := True;
end;

procedure TForm1.N2Theta1Click(Sender: TObject);
begin
 IIViewer.XCoordMode := XCM_2T;
 ResultedIntSet.ViewMode := XCM_2T;
 ResultedIntSet.GridAssign(NumericGrid1);
 N2Theta1.Checked := True;
 N2Theta2.Checked := True;
end;

procedure TForm1.ViewResultedIntSet(Sender : TObject; ResultedIntSet : TMSCRNeutronCompList);
begin
 ResultedIntSet.GridAssign(NumericGrid1);
end;

procedure TForm1.ShowHint(Sender : TObject);
begin
 StatusBar1.SimpleText := Application.Hint;
end;

procedure TForm1.Select_Area_Limits1Click(Sender: TObject);
begin
 if not Select_Area_Limits1.Checked then
  begin
   try
    ActiveNumber := IIViewer.GetActiveCurve;
   except raise; Exit end;
   IIMaker.CreateSelectedPoints;
   Select_Area_Limits1.Checked := True;
   SelectionMode := 1;
   PointCounter := 0;
   Chart1.Hint := 'Now you can pick a first point - "START"...';
  end
 else
  begin
   IIViewer.RemoveSelectedPoints;
   Select_Area_Limits1.Checked := False;
   SelectionMode := 0;
   Chart1.Hint := STR_CHART_NORMAL_HINT;   
  end;
end;

procedure TForm1.Select_Characteristic_Points_of_Peak1Click(Sender: TObject);
begin
 if not Select_Characteristic_Points_of_Peak1.Checked then
  begin
   try
    ActiveNumber := IIViewer.GetActiveCurve;
   except raise; Exit end;
   IIMaker.CreateSelectedPoints;
   Select_Characteristic_Points_of_Peak1.Checked := True;
   SelectionMode := 2;
   PointCounter := 0;
   Chart1.Hint := 'Now you can pick a first point - "START"...';
  end
 else
  begin
   IIViewer.RemoveSelectedPoints;
   Select_Characteristic_Points_of_Peak1.Checked := False;
   SelectionMode := 0;
   Chart1.Hint := STR_CHART_NORMAL_HINT;   
  end;
end;

procedure TForm1.Select_Gaussian_Limits1Click(Sender: TObject);
var PS : TNeutronPointsSet;
begin
 if not Select_Gaussian_Limits1.Checked then
  begin
   try
    ActiveNumber := IIViewer.GetActiveCurve;
    PS := IIViewer.GetActivePointsSet;
   except raise; Exit end;
   if not (PS is TGaussPointsSet) then
    begin
     MessageDlg('This operation is allowed only with the gaussian...',mtWarning,[mbOk],0);
     Exit;
    end;
   IIMaker.CreateSelectedPoints;
   Select_Gaussian_Limits1.Checked := True;
   SelectionMode := 3;
   PointCounter := 0;
   Chart1.Hint := 'Now you can pick a first point - "START"...';
  end
 else
  begin
   IIViewer.RemoveSelectedPoints;
   Select_Gaussian_Limits1.Checked := False;
   SelectionMode := 0;
   Chart1.Hint := STR_CHART_NORMAL_HINT;   
  end;
end;

procedure TForm1.Select_Positions_of_Gaussians1Click(Sender: TObject);
begin
 if not Select_Positions_of_Gaussians1.Checked then
  begin
   try
    ActiveNumber := IIViewer.GetActiveCurve;
   except raise; Exit end;
   IIMaker.CreateSelectedPoints;
   Select_Positions_of_Gaussians1.Checked := True;
   SelectionMode := 4;
   PointCounter := 0;
   Chart1.Hint := 'Now you can pick a first point...';
  end
 else
  begin
   IIViewer.RemoveSelectedPoints;
   Select_Positions_of_Gaussians1.Checked := False;
   SelectionMode := 0;
   Chart1.Hint := STR_CHART_NORMAL_HINT;   
  end;
end;

procedure TForm1.SetSelectedAreaMode(ASelectedAreaMode : Boolean);
begin
 FSelectedAreaMode := ASelectedAreaMode;
 if FSelectedAreaMode then
  begin
   Select_Area_Limits1.Enabled := False;
   Select_Characteristic_Points_of_Peak1.Enabled := True;
   Select_Gaussian_Limits1.Enabled := True;
   Select_Positions_of_Gaussians1.Enabled := True;
   Select_Area1.Enabled := False;
   Return_to_Total_Profile1.Enabled := True;
  end
 else
  begin
   Select_Area_Limits1.Enabled := True;
   Select_Characteristic_Points_of_Peak1.Enabled := False;
   Select_Gaussian_Limits1.Enabled := False;
   Select_Positions_of_Gaussians1.Enabled := False;
   Select_Area1.Enabled := True;
   Return_to_Total_Profile1.Enabled := False;
   if IIMaker.IsGaussesFinding then IIMaker.StopFindGausses;
  end;
end;

procedure TForm1.View_Markers1Click(Sender: TObject);
begin
 View_Markers1.Checked := not View_Markers1.Checked;
 IIViewer.SetViewMarkers(View_Markers1.Checked);
end;

procedure TForm1.SaveResultsAsText1Click(Sender: TObject);
begin
 SaveResults;
end;

procedure TForm1.SaveResults;
var i,j : LongInt;
    F : TextFile;
    St : string;
    FileName : string;
begin
 with SaveDialog1 do
  begin
   if Execute then
    begin
     FileName := SaveDialog1.FileName;
     if FileName <> '' then
      begin
       SaveDialog1.InitialDir := ExtractFilePath(FileName);
       if ExtractFileExt(FileName) = '' then FileName := FileName + '.txt';
       AssignFile(F,FileName);
       Rewrite(F);
       try
        WriteLn(F,'Intensity',#9,'Start Pos.',#9,'Peak Pos.',#9,'Finish Pos.');
        with NumericGrid1 do
         for i := FixedRows to RowCount - 1 do
          begin
           St := '';
           for j := FixedCols to ColCount - 1 do
            if j <> ColCount - 1 then St := St + Cells[j,i] + #9
             else St := St + Cells[j,i];
           WriteLn(F,St);
          end;{for i := FixedRows to RowCount - 1 do...}
       finally
        CloseFile(F);
        Modified := False;
       end;
      end
     else MessageDlg('File name must not be empty...',mtError,[mbOk],0);
    end;
  end;
end;

procedure TForm1.Copy_ButClick(Sender: TObject);
begin
 if ActiveControl is TNumericGrid then
  with ActiveControl as TNumericGrid do CopyToClipBoard;
end;

procedure TForm1.Delete1Click(Sender: TObject);
var i : LongInt;
begin
 if ActiveControl is TNumericGrid then
  with ActiveControl as TNumericGrid do
   begin
    DeleteSelection;
    if RowCount = 1 then
     begin
      RowCount := 2;
      FixedRows := 1;
      Row := 1;
      for i := 1 to ColCount - 1 do Cells[i,Row] := '';
     end;
    EnumerateRows;
    ResultedIntSet.GetDataFromGrid(NumericGrid1);
    Modified := True;    
   end;
end;

procedure TForm1.Select_All1Click(Sender: TObject);
begin
 if ActiveControl is TNumericGrid then
  With ActiveControl as TNumericGrid do SelectAll;
end;

procedure TForm1.GaussiansFindingDone(Sender : TObject);
begin
 MessageDlg('Gaussians calculation done...',mtInformation,[mbOk],0);
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
 if Modified then
  begin
   if MessageDlg('Save results as text file ?',mtConfirmation,[mbYes,mbNo],0) = mrYes then
    begin
     SaveResults;
     CanClose := not Modified;
    end
   else CanClose := True;
  end{if Modified then...}
 else CanClose := True;
end;

procedure TForm1.SetWavelength1Click(Sender: TObject);
var SaveDecimalSeparator : Char;
begin
 if InputWavelengthDlg.ShowModal = mrOk then
  begin
   SaveDecimalSeparator := DecimalSeparator;
   DecimalSeparator := '.';
   Screen.Cursor := crHourGlass;
   IIMaker.SetWaveLength(StrToFloat(InputWavelengthDlg.WavelengthValueEdit.Text));
   Screen.Cursor := crDefault;
   DecimalSeparator := SaveDecimalSeparator;
   IIViewer.XCoordMode := XCM_SINTL;
   ResultedIntSet.ViewMode := XCM_SINTL;
   ResultedIntSet.GridAssign(NumericGrid1);
   SinThetaLambda1.Checked := True;
   SinThetaLambda2.Checked := True;
  end;
end;

end.
