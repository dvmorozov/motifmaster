{------------------------------------------------------------------------------
    This file is part of the MotifMASTER project. This software is
    distributed under GPL (see gpl.txt for details).

    This software is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Copyright (C) 1999-2007 D.Morozov (dvmorozov@mail.ru)
------------------------------------------------------------------------------}

unit Unit11;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, DataClasses, ExtCtrls, Plotter2, ColorGrd, DsgnIntf,
  ComponentList, Tools;

type
  TUpdateEvent = procedure(Sender: TObject) of object;

  EStructViewPropDlg = class(Exception);

  TStructViewPropDlg = class(TForm)
    TabControlSites: TTabControl;
    BtnOk: TButton;
    BtnCancel: TButton;
    BtnApply: TButton;
    BtnHelp: TButton;
    RadioMomentsMode: TRadioGroup;
    GroupBox1: TGroupBox;
    ColorGrid: TColorGrid;

    procedure TabControlSitesChange(Sender: TObject);
    procedure TabControlSitesChanging(Sender: TObject;
              var AllowChange: Boolean);
    procedure BtnApplyClick(Sender: TObject);
    procedure BtnOkClick(Sender: TObject);
    procedure BtnHelpClick(Sender: TObject);

  protected
    FEditedSiteList: TSiteList;
    PlotParamsList: TComponentList;
    FOnUpdate: TUpdateEvent;

    procedure SetEditedSiteList(const AEditedSiteList: TSiteList);
    procedure SetSitesToTabs;
    procedure SetMomentsMode;
    procedure GetMomentsMode;
    procedure GetChanges;
    procedure UpdateChanges;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property EditedSiteList: TSiteList   write SetEditedSiteList;
    property OnUpdate: TUpdateEvent
             read FOnUpdate              write FOnUpdate;
  end;

var
  StructViewPropDlg: TStructViewPropDlg;

implementation

{$R *.DFM}

procedure TStructViewPropDlg.SetEditedSiteList(const AEditedSiteList: TSiteList);
var i: LongInt;
    TSP: TSitePlotParams;
begin
     if Assigned(AEditedSiteList) then
     begin
          FEditedSiteList := AEditedSiteList;
          PlotParamsList.Clear;
          for i := 0 to FEditedSiteList.Count - 1 do begin
              TSP := TSitePlotParams(TSite(FEditedSiteList.Items[i]).GetPlotParams);
              PlotParamsList.Add(TComponent(TSP.GetCopy));
          end;
          SetSitesToTabs;
          SetMomentsMode;
     end else raise EStructViewPropDlg.Create('Edited sites list must be assigned...');
end;

procedure TStructViewPropDlg.SetSitesToTabs;
var i: LongInt;
begin
     with TabControlSites do
     begin
          Tabs.Clear;
          for i := 0 to FEditedSiteList.Count - 1 do
              Tabs.Add(FEditedSiteList.GetObjectIdentifier(i));
          if FEditedSiteList.Count <> 0 then TabControlSites.TabIndex := 0
          else TabControlSites.TabIndex := -1;
     end;
end;

procedure TStructViewPropDlg.SetMomentsMode;
var TSP: TSitePlotParams;
begin
     if PlotParamsList.Count <> 0 then
     begin
          TSP := TSitePlotParams(PlotParamsList.Items[TabControlSites.TabIndex]);
          case TSP.MomentsPlotMode of
               PM_ARROWS : RadioMomentsMode.ItemIndex := 0;
               PM_MODULES : RadioMomentsMode.ItemIndex := 1;
               PM_ELEMENT : RadioMomentsMode.ItemIndex := 2;
               PM_SERIAL_NUMBER : RadioMomentsMode.ItemIndex := 3;
          end;

          case TSP.Color of
               clBlack : ColorGrid.ForegroundIndex := 0;
               clMaroon : ColorGrid.ForegroundIndex := 1;
               clGreen : ColorGrid.ForegroundIndex := 2;
               clOlive : ColorGrid.ForegroundIndex := 3;
               clNavy : ColorGrid.ForegroundIndex := 4;
               clPurple : ColorGrid.ForegroundIndex := 5;
               clTeal : ColorGrid.ForegroundIndex := 6;
               clGray : ColorGrid.ForegroundIndex := 7;
               clSilver : ColorGrid.ForegroundIndex := 8;
               clRed : ColorGrid.ForegroundIndex := 9;
               clLime : ColorGrid.ForegroundIndex := 10;
               clYellow : ColorGrid.ForegroundIndex := 11;
               clBlue : ColorGrid.ForegroundIndex := 12;
               clFuchsia : ColorGrid.ForegroundIndex := 13;
               clAqua : ColorGrid.ForegroundIndex := 14;
               clWhite : ColorGrid.ForegroundIndex := 15;
               else ColorGrid.ForegroundIndex := -1;
          end;
     end;
end;

procedure TStructViewPropDlg.GetMomentsMode;
var TSP: TSitePlotParams;
begin
     if PlotParamsList.Count <> 0 then
     begin
          TSP := TSitePlotParams(PlotParamsList.Items[TabControlSites.TabIndex]);
          case RadioMomentsMode.ItemIndex of
               0 : TSP.MomentsPlotMode := PM_ARROWS;
               1 : TSP.MomentsPlotMode := PM_MODULES;
               2 : TSP.MomentsPlotMode := PM_ELEMENT;
               3 : TSP.MomentsPlotMode := PM_SERIAL_NUMBER;
          end;
          TSP.Color := ColorGrid.ForegroundColor;
     end;
end;

procedure TStructViewPropDlg.TabControlSitesChange(Sender: TObject);
begin
     SetMomentsMode;
end;

constructor TStructViewPropDlg.Create(AOwner: TComponent);
begin
     inherited;
     PlotParamsList := TComponentList.Create(nil);
end;

destructor TStructViewPropDlg.Destroy;
begin
     UtilizeObject(PlotParamsList);
     inherited;
end;

procedure TStructViewPropDlg.TabControlSitesChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
     GetChanges;
end;

procedure TStructViewPropDlg.BtnApplyClick(Sender: TObject);
begin
     GetChanges;
     UpdateChanges;
end;

procedure TStructViewPropDlg.UpdateChanges;
var TSP: TSitePlotParams;
    i: LongInt;
begin
     for i := 0 to FEditedSiteList.Count - 1 do begin
         TSP := TSitePlotParams(PlotParamsList.Items[i]);
         TSP.CopyParameters(TSite(FEditedSiteList.Items[i]).GetPlotParams);
     end;
     if Assigned(OnUpdate) then OnUpdate(Self);
end;

procedure TStructViewPropDlg.BtnOkClick(Sender: TObject);
begin
     GetChanges;
     UpdateChanges;
end;

procedure TStructViewPropDlg.GetChanges;
begin
     GetMomentsMode;
end;

procedure TStructViewPropDlg.BtnHelpClick(Sender: TObject);
begin
    Application.HelpJump('hlp_StructViewPropDlg');
end;

end.
 