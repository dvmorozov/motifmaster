


{------------------------------------------------------------------------------}
{       Copyright (C) 1999-2007 D.Morozov (dvmorozov@mail.ru)                  }
{------------------------------------------------------------------------------}
unit Unit11;

{$MODE Delphi}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, DataClasses, ExtCtrls, Plotter2, ComponentList, Tools,
  LResources, Buttons, MyExceptions, ColorBox, HelpIntfs;

type
  TUpdateEvent = procedure(Sender: TObject) of object;

  EStructViewPropDlg = class(EMyException);

  { TStructViewPropDlg }

  TStructViewPropDlg = class(TForm)
    ColorBox1: TColorBox;
    GroupBox1: TGroupBox;
    RadioMomentsMode: TRadioGroup;
    TabControlSites: TTabControl;
    BtnOk: TButton;
    BtnCancel: TButton;
    BtnApply: TButton;
    BtnHelp: TButton;

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
               clBlack : ColorBox1.ItemIndex := 0;
               clMaroon : ColorBox1.ItemIndex := 1;
               clGreen : ColorBox1.ItemIndex := 2;
               clOlive : ColorBox1.ItemIndex := 3;
               clNavy : ColorBox1.ItemIndex := 4;
               clPurple : ColorBox1.ItemIndex := 5;
               clTeal : ColorBox1.ItemIndex := 6;
               clGray : ColorBox1.ItemIndex := 7;
               clSilver : ColorBox1.ItemIndex := 8;
               clRed : ColorBox1.ItemIndex := 9;
               clLime : ColorBox1.ItemIndex := 10;
               clYellow : ColorBox1.ItemIndex := 11;
               clBlue : ColorBox1.ItemIndex := 12;
               clFuchsia : ColorBox1.ItemIndex := 13;
               clAqua : ColorBox1.ItemIndex := 14;
               clWhite : ColorBox1.ItemIndex := 15;
               else ColorBox1.ItemIndex := -1;
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
          case ColorBox1.ItemIndex of
               0 : TSP.Color := clBlack;
               1 : TSP.Color := clMaroon;
               2 : TSP.Color := clGreen;
               3 : TSP.Color := clOlive;
               4 : TSP.Color := clNavy;
               5 : TSP.Color := clPurple;
               6 : TSP.Color := clTeal;
               7 : TSP.Color := clGray;
               8 : TSP.Color := clSilver;
               9 : TSP.Color := clRed;
               10 : TSP.Color := clLime;
               11 : TSP.Color := clYellow;
               12 : TSP.Color := clBlue;
               13 : TSP.Color := clFuchsia;
               14 : TSP.Color := clAqua;
               15 : TSP.Color := clWhite;
               else TSP.Color := clBlack;
          end;
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
    ShowHelpOrErrorForKeyword('','HTML/struct_view_prop_dlg.html');
end;

initialization
{$I Unit11.lrs}
end.
  
