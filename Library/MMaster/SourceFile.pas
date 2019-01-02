{------------------------------------------------------------------------------
    This file is part of the MotifMASTER project. This software is
    distributed under GPL (see gpl.txt for details).

    This software is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Copyright (C) 1999-2007 D.Morozov (dvmorozov@mail.ru)
------------------------------------------------------------------------------}

unit SourceFile;

interface

uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
    Dialogs, ComponentList, CalcModule, DataClasses, ComCtrls, TableComp,
    Plotter, Tools, SelfSaved, ClassInheritIDs;

const
    FILE_ICON_INDEX         = 0;
    TABLE_ICON_INDEX        = 1;
    PLOT_IN_3D_ICON_INDEX   = 2;
    COMMENTS_ICON_INDEX     = 3;
    BAR_GRAPH_ICON_INDEX    = 4;
    UNIT_OPEN_ICON_INDEX    = 5;
    UNIT_CLOSE_ICON_INDEX   = 6;
    GENERAL_ICON_INDEX      = 7;

    PROJECT_DEF_EXT = 'mmp';    NDT_POLYCRYSTAL         = 0;
    NDT_MONOCRYSTAL         = 1;

type
    EReverseUnit = class(Exception);
    EReverseSourceData = class(Exception);
    EReverseCalcData = class(Exception);

    TSinTDataPointer = class(TComponent)
    public
        SinTPointer: TSinTCompList;
        PlotMode: Integer;
    end;

    TReverseSourceData = class(TSelfSavedComponent)
    protected
        FStructureData: TSiteList;
        FNeutronData: TNeutronCompList;
        FGeneralData: TGeneralClass;
        FCaption: string;

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
        procedure CreateStructureData(ACaption: string);
        procedure CreateNeutronData(
            const ACaption: string; const NeutronDataType: LongInt);
        procedure CreateGeneralData(ACaption: string);

        procedure GetStructureDataToCalc(CalcModule: TCalcModule);
        procedure GetNeutronDataToCalc(CalcModule: TCalcModule);

        procedure ViewContents(TreeNodes: TTreeNodes;
            TreeNode: TTreeNode; TreeView: TTreeView);

        constructor Create(AOwner: TComponent; const NeutronDataType: LongInt);
        destructor Destroy; override;
        procedure IsReady;

        property StructureData: TSiteList       read FStructureData;
        property NeutronData: TNeutronCompList  read FNeutronData;
        property GeneralData: TGeneralClass     read FGeneralData;
        property Caption: string                read FCaption
            write FCaption;
    end;

    TReverseCalcData = class(TSelfSavedComponent)
    protected
        FSinTData: TSinTCompList;
        FCalcStructureData: TSiteList;
        FSinTDataPointer: TSintDataPointer;
        FCaption: string;

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
        procedure CreateSinTData(ACaption: string);
        procedure CreateCalcStructureData(ACaption: string);
        procedure GetSinTDataToCalc(CalcModule: TCalcModule);
        procedure GetCalcStructureDataToCalc(CalcModule: TCalcModule);
        procedure ViewContents(TreeNodes: TTreeNodes;
            TreeNode: TTreeNode; TreeView: TTreeView);
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        procedure IsReady;

        property SinTData: TSinTCompList        read FSinTData;
        property CalcStructureData: TSiteList   read FCalcStructureData;
        property Caption: string                read FCaption
            write FCaption;
    end;

    TReverseUnit = class(TSelfSavedComponent)
    protected
        FComments: TComponent;
        FSourceData: TReverseSourceData;
        FCalcData: TReverseCalcData;
        FCaption: string;

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
        procedure CreateComments(const ACaption: string);
        procedure CreateSourceData(
            const ACaption: string; const NeutronDataType: LongInt);
        procedure CreateCalcData(const ACaption: string);

        procedure ViewContents(TreeNodes: TTreeNodes; TreeNode: TTreeNode;
            TreeView: TTreeView);

        constructor Create(AOwner: TComponent; const NeutronDataType: LongInt);
        destructor Destroy; override;

        procedure IsReady;

        property Comments: TComponent           read FComments;
        property SourceData: TReverseSourceData read FSourceData;
        property CalcData: TReverseCalcData     read FCalcData;
        property Caption: string                read FCaption
            write FCaption;
    end;

    TDirectSourceData = class(TSelfSavedComponent)
    protected
        FModelData: TSiteList;
        FGeneralData: TGeneralClass;
        FPatternParams: TPatternParametersContainer;
        FCaption: string;

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
        procedure GetDataToCalc(CalcModule: TCalcModule);
        procedure GetPatternParamsToCalc(CalcModule: TCalcModule);    
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        procedure ViewContents(TreeNodes: TTreeNodes; TreeNode: TTreeNode; TreeView: TTreeView);
        function IsReady: LongInt;

        property ModelData: TSiteList           read FModelData;
        property GeneralData: TGeneralClass     read FGeneralData;
        property Caption: string                read FCaption
            write FCaption;
    end;

    TDirectCalcData = class(TSelfSavedComponent)
    protected
        FModelSinTData: TTableCompList;
        FSinTDataPointer: TSinTDataPointer;
        FCaption: string;

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
        procedure CreateModelSinTData(ACaption: string);
        procedure GetModelSinTDataToCalc(CalcModule: TCalcModule);
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        procedure ViewContents(TreeNodes: TTreeNodes;
            TreeNode: TTreeNode; TreeView: TTreeView);

        property ModelSinTData: TTableCompList  read FModelSinTData;
        property Caption: string                read FCaption
            write FCaption;
    end;

    TDirectUnit = class(TSelfSavedComponent)
    protected
        FComments: TComponent;
        FSourceData: TDirectSourceData;
        FCalcData: TComponent;
        FCaption: string;

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
        procedure CreateComments(ACaption: string);
        procedure CreateSourceData(ACaption: string);
        procedure CreateCalcData(ACaption: string);
        procedure ViewContents(TreeNodes: TTreeNodes; TreeNode: TTreeNode;
            TreeView: TTreeView);
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        function IsReady: LongInt;

        property Comments: TComponent           read FComments;
        property SourceData: TDirectSourceData  read FSourceData;
        property CalcData: TComponent           read FCalcData;
        property Caption: string                read FCaption
            write FCaption;
    end;

    TSourceFile = class(TComponentList)
    protected
        TreeView: TTreeView;
        FCaption: string;

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
        FileName: string;
        Modified: Boolean;
        Mode: LongInt;

        constructor Create(const AFileName: TFileName);
                
        function CreateReverseUnit(ACaption: string;
            const NeutronDataType: LongInt): TReverseUnit;
        function CreateDirectUnit(ACaption: string): TDirectUnit;

        procedure SetTreeView(ATreeView: TTreeView);
        procedure ViewSourceFileContents;

        property Caption: string read FCaption write FCaption;
    end;

const
    sfCreate = 1;
    sfOpened = 2;
    sfError = 3;

function CreateNewDirectCalcData: TDirectCalcData;
function CreateNewDirectSourceData: TDirectSourceData;
function CreateNewDirectUnit: TDirectUnit;
function CreateNewReverseCalcData: TReverseCalcData;
function CreateNewReverseSourceData(
    const NeutronDataType: LongInt): TReverseSourceData;
function CreateNewReverseUnit(
    const NeutronDataType: LongInt): TReverseUnit;
function CreateNewSinTDataPointer: TSinTDataPointer;

function OpenSourceFile(const FileName: TFileName): TSourceFile;
    function CreateNewSourceFile(const FileName: string): TSourceFile;
procedure SaveSourceFile(const SourceFile: TSourceFile);

implementation

type
    TDCD = class(TDirectCalcData)
    protected
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
    end;

    TDSD = class(TDirectSourceData)
    protected
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
    end;

    TDU = class(TDirectUnit)
    protected
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
    end;

    TRCD = class(TReverseCalcData)
    protected
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
    end;

    TRSD = class(TReverseSourceData)
    protected
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
    end;

    TRU = class(TReverseUnit)
    protected
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
    end;

    TSTDP = class(TSinTDataPointer);

    TSF = class(TSourceFile)
    protected
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
    end;

function CreateNewDirectCalcData: TDirectCalcData;
begin
    Result := TDCD.Create(nil);
end;

function CreateNewDirectSourceData: TDirectSourceData;
begin
    Result := TDSD.Create(nil);
end;

function CreateNewDirectUnit: TDirectUnit;
begin
    Result := TDU.Create(nil);
end;

function CreateNewReverseCalcData: TReverseCalcData;
begin
    Result := TRCD.Create(nil);
end;

function CreateNewReverseSourceData(
    const NeutronDataType: LongInt): TReverseSourceData;
begin
    Result := TRSD.Create(nil, NeutronDataType);
end;

function CreateNewReverseUnit(
    const NeutronDataType: LongInt): TReverseUnit;
begin
    Result := TRU.Create(nil, NeutronDataType);
end;

function CreateNewSinTDataPointer: TSinTDataPointer;
begin
    Result := TSTDP.Create(nil);
end;

function CreateNewSourceFile(const FileName: string): TSourceFile;
begin
    Result := TSF.Create(FileName);
    Result.Caption := ExtractFileName(FileName);
end;

function OpenSourceFile(const FileName: TFileName): TSourceFile;
var DataFile: TFileStream;
begin
    if FileExists(FileName) then
    begin
        if UpperCase(ExtractFileExt(FileName)) <> UpperCase('.' + PROJECT_DEF_EXT) then
        begin
            MessageDlg('Invalid file type...', mtError, [mbOk], 0);
            Exit;
        end;
    end
    else
    begin
        MessageDlg('Can''t open file ' + ExtractFileName(FileName) + '...',
            mtError, [mbOk], 0);
        Exit;
    end;

    DataFile := TFileStream.Create(FileName, fmOpenRead);
    try
        try
            Result := TSourceFile(DataFile.ReadComponent(nil));
            Result.Mode := sfOpened;
        finally
            UtilizeObject(DataFile);
        end;
    except
        MessageDlg('File ' + ExtractFileName(FileName) +
            ' has not valid type...', mtError, [mbOk], 0);
        UtilizeObject(Result);
        Result := nil;
    end;
end;

procedure SaveSourceFile(const SourceFile: TSourceFile);
var DataFile: TFileStream;
begin
    DataFile := TFileStream.Create(SourceFile.FileName, fmCreate);
    DataFile.WriteComponent(SourceFile);
    UtilizeObject(DataFile);
    SourceFile.Modified := False;
    SourceFile.Mode := sfOpened;
end;

procedure TReverseCalcData.CreateCalcStructureData;
begin
    UtilizeObject(FCalcStructureData);
    FCalcStructureData := CreateNewSiteList;
    FCalcStructureData.Caption := ACaption;
end;

procedure TReverseUnit.CreateComments;
begin
    UtilizeObject(FComments);
    FComments := CreateNewCommentsClass;
    TCommentsClass(FComments).Caption := ACaption;
end;

procedure TReverseUnit.CreateSourceData;
begin
    UtilizeObject(FSourceData);
    FSourceData := CreateNewReverseSourceData(NeutronDataType);
    FSourceData.Caption := ACaption;
end;

procedure TDirectUnit.CreateSourceData;
begin
    UtilizeObject(FSourceData);
    FSourceData := CreateNewDirectSourceData;
    FSourceData.Caption := ACaption;
end;

procedure TReverseUnit.CreateCalcData;
begin
    UtilizeObject(FCalcData);
    FCalcData := CreateNewReverseCalcData;
    TReverseCalcData(FCalcData).Caption := ACaption;
end;

procedure TDirectUnit.CreateCalcData;
begin
    UtilizeObject(FCalcData);
    FCalcData := CreateNewDirectCalcData;
    TDirectCalcData(FCalcData).Caption := ACaption;
end;

procedure TDirectUnit.CreateComments;
begin
    UtilizeObject(FComments);
    FComments := CreateNewCommentsClass;
    TCommentsClass(FComments).Caption := ACaption;
end;

procedure TReverseCalcData.CreateSinTData;
begin
    UtilizeObject(FSinTData);
    FSinTData := CreateNewSinTCompList;
    FSinTData.Caption := ACaption;

    UtilizeObject(FSinTDataPointer);
    FSinTDataPointer := CreateNewSinTDataPointer;
    FSinTDataPointer.SinTPointer := TSinTCompList(SinTData);
    FSinTDataPointer.PlotMode := PM_INTENSITY or PM_NUCLEAR_INTENSITY
        or PM_EXP_INTENSITY;
end;

procedure TDirectCalcData.CreateModelSinTData;
begin
    UtilizeObject(FModelSinTData);
    FModelSinTData := CreateNewSinTCompList;
    FModelSinTData.Caption := ACaption;
    UtilizeObject(FSinTDataPointer);
    FSinTDataPointer := CreateNewSinTDataPointer;
    FSinTDataPointer.SinTPointer := TSinTCompList(ModelSinTData);
    FSinTDataPointer.PlotMode := PM_INTENSITY or PM_NUCLEAR_INTENSITY
        or PM_EXP_INTENSITY;
end;

procedure TDirectSourceData.GetDataToCalc(CalcModule: TCalcModule);
begin
    with CalcModule do
        SetSiteList(TSiteList(ModelData.GetCopy));
end;

procedure TDirectSourceData.GetPatternParamsToCalc(CalcModule: TCalcModule);
begin
    with CalcModule do
        SetPatternParams(TComponent(FPatternParams.GetCopy));
end;

procedure TReverseCalcData.GetCalcStructureDataToCalc;
begin
    if Assigned(CalcModule) then
    begin
        CreateCalcStructureData('Calculated_Structure');
        CalcModule.ViewSiteList := CalcStructureData;
    end;
end;

procedure TReverseCalcData.GetSinTDataToCalc;
begin
    if Assigned(CalcModule) then
    begin
        CreateSinTData('Calculated_Intensities');
        CalcModule.ViewSinTList := SinTData;
    end;
end;

procedure TDirectCalcData.GetModelSinTDataToCalc;
begin
    if Assigned(CalcModule) then
    begin
        CreateModelSinTData('Calculated_Intensities');
        CalcModule.ViewSinTList := TSinTCompList(ModelSinTData);
    end;
end;

constructor TSourceFile.Create;
begin
    inherited Create(nil);
    FileName := AFileName;
    Mode := sfCreate;
end;

procedure TSourceFile.ViewSourceFileContents;
var TreeNode: TTreeNode;
    i: LongInt;
    TC: TComponent;
    SubItem: TTreeNode;
    SaveExpandState: Boolean;
begin
    if Assigned(TreeView) then
    begin
        SaveExpandState := TreeView.AutoExpand;
        if TreeView.AutoExpand then TreeView.AutoExpand := False;
        with TreeView.Items do
        begin
            Clear;
            AddObject(nil, Caption, Self);
            TreeNode := TreeView.Items[0];
            TreeNode.ImageIndex := FILE_ICON_INDEX;
            for i := 0 to Self.Count - 1 do
            begin
                TC := Self.Items[i];
                if TC is TDirectUnit then
                    with TC as TDirectUnit do
                    begin
                        SubItem := AddChildObject(TreeNode, Caption, TC);
                        SubItem.ImageIndex := UNIT_CLOSE_ICON_INDEX;
                        SubItem.SelectedIndex := UNIT_OPEN_ICON_INDEX;
                        ViewContents(TreeView.Items, SubItem, TreeView);
                        Continue;
                    end;
                if TC is TReverseUnit then
                    with TC as TReverseUnit do
                    begin
                        SubItem := AddChildObject(TreeNode, Caption, TC);
                        SubItem.ImageIndex := UNIT_CLOSE_ICON_INDEX;
                        SubItem.SelectedIndex := UNIT_OPEN_ICON_INDEX;
                        ViewContents(TreeView.Items, SubItem, TreeView);
                        Continue;
                    end;
                AddChildObject(TreeNode, TC.Name, TC);
            end;
        end;{with TreeView.Items do...}
        TreeView.AutoExpand := SaveExpandState;
        TreeNode.Expand(False);
        TreeView.Repaint;
    end;
end;

procedure TReverseUnit.ViewContents(TreeNodes: TTreeNodes; TreeNode: TTreeNode; TreeView: TTreeView);
var TempTreeNode: TTreeNode;
begin
    if SourceData is TReverseSourceData then
        with SourceData as TReverseSourceData do
        begin
            TempTreeNode := TreeNodes.AddChildObject(TreeNode, Caption, SourceData);
            TempTreeNode.ImageIndex := UNIT_CLOSE_ICON_INDEX;
            TempTreeNode.SelectedIndex := UNIT_OPEN_ICON_INDEX;
            ViewContents(TreeNodes, TempTreeNode, TreeView);
        end;
    if CalcData is TReverseCalcData then
        with CalcData as TReverseCalcData do
        begin
            TempTreeNode := TreeNodes.AddChildObject(TreeNode, Caption, CalcData);
            TempTreeNode.ImageIndex := UNIT_CLOSE_ICON_INDEX;
            TempTreeNode.SelectedIndex := UNIT_OPEN_ICON_INDEX;
            ViewContents(TreeNodes, TempTreeNode, TreeView);
        end;
    with Comments as TCommentsClass do
        TempTreeNode := TreeNodes.AddChildObject(TreeNode, Caption, Comments);
    TempTreeNode.ImageIndex := COMMENTS_ICON_INDEX;
    TempTreeNode.SelectedIndex := TempTreeNode.ImageIndex;
end;

procedure TReverseUnit.IsReady;
begin
    if not Assigned(SourceData) then
        raise EReverseUnit.Create('Source data object is not assigned...');
    if not Assigned(CalcData) then
        raise EReverseUnit.Create('Calculation results object is not assigned...');
    SourceData.IsReady;
    CalcData.IsReady;
end;

procedure TDirectUnit.ViewContents(TreeNodes: TTreeNodes; TreeNode: TTreeNode;
    TreeView: TTreeView);
var TempTreeNode: TTreeNode;
begin
    if SourceData is TDirectSourceData then
        with SourceData as TDirectSourceData do
        begin
            TempTreeNode := TreeNodes.AddChildObject(TreeNode, Caption, SourceData);
            TempTreeNode.ImageIndex := UNIT_CLOSE_ICON_INDEX;
            TempTreeNode.SelectedIndex := UNIT_OPEN_ICON_INDEX;
            ViewContents(TreeNodes, TempTreeNode, TreeView);
        end;
    if CalcData is TDirectCalcData then
        with CalcData as TDirectCalcData do
        begin
            TempTreeNode := TreeNodes.AddChildObject(TreeNode, Caption, CalcData);
            TempTreeNode.ImageIndex := UNIT_CLOSE_ICON_INDEX;
            TempTreeNode.SelectedIndex := UNIT_OPEN_ICON_INDEX;
            ViewContents(TreeNodes, TempTreeNode, TreeView);
        end;
    with Comments as TCommentsClass do
        TempTreeNode := TreeNodes.AddChildObject(TreeNode, Caption, Comments);
    TempTreeNode.ImageIndex := COMMENTS_ICON_INDEX;
    TempTreeNode.SelectedIndex := TempTreeNode.ImageIndex;
end;

function TDirectUnit.IsReady: LongInt;
begin
    Result := 0;
    if not Assigned(SourceData) then
    begin Result := -1; Exit end;
    if SourceData is TDirectSourceData then
        with SourceData as TDirectSourceData do Result := IsReady
    else Result := -1;
end;

procedure TReverseSourceData.ViewContents(TreeNodes: TTreeNodes;
    TreeNode: TTreeNode; TreeView: TTreeView);
var TempTreeNode: TTreeNode;
begin
    TempTreeNode := TreeNodes.AddChildObject(
    TreeNode, StructureData.Caption, StructureData);
    TempTreeNode.ImageIndex := TABLE_ICON_INDEX;
    TempTreeNode.SelectedIndex := TempTreeNode.ImageIndex;

    TempTreeNode := TreeNodes.AddChildObject(
    TreeNode, NeutronData.Caption, NeutronData);
    TempTreeNode.ImageIndex := TABLE_ICON_INDEX;
    TempTreeNode.SelectedIndex := TempTreeNode.ImageIndex;

    TempTreeNode := TreeNodes.AddChildObject(
    TreeNode, GeneralData.Caption, GeneralData);
    TempTreeNode.ImageIndex := GENERAL_ICON_INDEX;
    TempTreeNode.SelectedIndex := TempTreeNode.ImageIndex;

    TempTreeNode := TreeNodes.AddChildObject(
    TreeNode, 'Plot_in_3D', StructureData.GetPlotParams);
    TempTreeNode.ImageIndex := PLOT_IN_3D_ICON_INDEX;
    TempTreeNode.SelectedIndex := TempTreeNode.ImageIndex;
end;

procedure TReverseSourceData.IsReady;
begin
    if not Assigned(StructureData) then
        raise EReverseSourceData.Create('Structure data object is not assigned...');
    if not Assigned(NeutronData) then
        raise EReverseSourceData.Create('Neutron diffraction data object is not assigned...');
    if not Assigned(GeneralData) then
        raise EReverseSourceData.Create('General data object is not assigned...');

    StructureData.IsReady;
    NeutronData.IsReady;
end;

procedure TDirectSourceData.ViewContents(TreeNodes: TTreeNodes;
    TreeNode: TTreeNode; TreeView: TTreeView);
var TempTreeNode: TTreeNode;
begin
    TempTreeNode := TreeNodes.AddChildObject(
    TreeNode, ModelData.Caption, ModelData);
    TempTreeNode.ImageIndex := TABLE_ICON_INDEX;
    TempTreeNode.SelectedIndex := TempTreeNode.ImageIndex;

    TempTreeNode := TreeNodes.AddChildObject(
    TreeNode, GeneralData.Caption, GeneralData);
    TempTreeNode.ImageIndex := GENERAL_ICON_INDEX;
    TempTreeNode.SelectedIndex := TempTreeNode.ImageIndex;

    TempTreeNode := TreeNodes.AddChildObject(
    TreeNode, 'Plot_in_3D', ModelData.GetPlotParams);
    TempTreeNode.ImageIndex := PLOT_IN_3D_ICON_INDEX;
    TempTreeNode.SelectedIndex := TempTreeNode.ImageIndex;
end;

function TDirectSourceData.IsReady: LongInt;
begin
    Result := 0;
    
end;

procedure TReverseCalcData.ViewContents(TreeNodes: TTreeNodes;
    TreeNode: TTreeNode; TreeView: TTreeView);
var TempTreeNode: TTreeNode;
begin
    TempTreeNode := TreeNodes.AddChildObject(
    TreeNode, CalcStructureData.Caption, CalcStructureData);
    TempTreeNode.ImageIndex := TABLE_ICON_INDEX;
    TempTreeNode.SelectedIndex := TempTreeNode.ImageIndex;

    TempTreeNode := TreeNodes.AddChildObject(
    TreeNode, SinTData.Caption, SinTData);
    TempTreeNode.ImageIndex := TABLE_ICON_INDEX;
    TempTreeNode.SelectedIndex := TempTreeNode.ImageIndex;

    TempTreeNode := TreeNodes.AddChildObject(
    TreeNode, 'Plot_in_3D', CalcStructureData.GetPlotParams);
    TempTreeNode.ImageIndex := PLOT_IN_3D_ICON_INDEX;
    TempTreeNode.SelectedIndex := TempTreeNode.ImageIndex;

    TempTreeNode := TreeNodes.AddChildObject(
    TreeNode, 'Bar_Graph', FSinTDataPointer);
    TempTreeNode.ImageIndex := BAR_GRAPH_ICON_INDEX;
    TempTreeNode.SelectedIndex := TempTreeNode.ImageIndex;
end;

procedure TDirectCalcData.ViewContents(TreeNodes: TTreeNodes;
    TreeNode: TTreeNode; TreeView: TTreeView);
var TempTreeNode: TTreeNode;
begin
    TempTreeNode := TreeNodes.AddChildObject(TreeNode,
    ModelSinTData.Caption, ModelSinTData);
    TempTreeNode.ImageIndex := TABLE_ICON_INDEX;
    TempTreeNode.SelectedIndex := TempTreeNode.ImageIndex;
    TempTreeNode := TreeNodes.AddChildObject(TreeNode,
        'Bar_Graph', FSinTDataPointer);
    TempTreeNode.ImageIndex := BAR_GRAPH_ICON_INDEX;
    TempTreeNode.SelectedIndex := TempTreeNode.ImageIndex;
end;

procedure TSourceFile.SetTreeView(ATreeView: TTreeView);
begin
    TreeView := ATreeView;
end;

constructor TReverseSourceData.Create;
begin
    inherited Create(AOwner);
    CreateStructureData('Structure_Data');
    CreateNeutronData('Neutron_Data', NeutronDataType);
    CreateGeneralData('General_Data');
end;

destructor TReverseSourceData.Destroy;
begin
    UtilizeObject(FGeneralData);
    UtilizeObject(FStructureData);
    UtilizeObject(FNeutronData);
    inherited;
end;

constructor TReverseCalcData.Create;
begin
    inherited Create(AOwner);
    CreateSinTData('Calculated_Intensities');
    CreateCalcStructureData('Calculated_Structure');
end;

destructor TReverseCalcData.Destroy;
begin
    UtilizeObject(FSinTData);
    UtilizeObject(FCalcStructureData);
    UtilizeObject(FSinTDataPointer);
    inherited Destroy;
end;

procedure TReverseCalcData.IsReady;
begin
    if not Assigned(CalcStructureData) then
        raise EReverseCalcData.Create('Calculated structure object is not assigned...');
    if not Assigned(SinTData) then
        raise EReverseCalcData.Create('Pattern object is not assigned...');
end;

procedure TReverseSourceData.CreateStructureData;
begin
    UtilizeObject(FStructureData);
    FStructureData := CreateNewSiteList;
    TSiteList(FStructureData).Caption := ACaption;
end;

procedure TReverseSourceData.CreateNeutronData;
begin
    UtilizeObject(FNeutronData);
    case NeutronDataType of
        NDT_POLYCRYSTAL : FNeutronData := CreateNewNeutronCompList;
        NDT_MONOCRYSTAL : FNeutronData := CreateNewNeutronCompListMono;
    end;
    FNeutronData.Caption := ACaption;
end;

procedure TReverseSourceData.CreateGeneralData;
var IPPC: IPatternParametersContainer;
begin
    UtilizeObject(FGeneralData);
    FGeneralData := CreateNewGeneralClass;
    FGeneralData.Caption := ACaption;

    FGeneralData.StructurePointer := StructureData;
    NeutronData.GetInterface(PatternParamsContainerGUID, IPPC);
    FGeneralData.PatternParamsPointer := IPPC;
end;

procedure TReverseSourceData.GetStructureDataToCalc;
begin
    with CalcModule do SetSiteList(TSiteList(StructureData.GetCopy));
end;

procedure TReverseSourceData.GetNeutronDataToCalc;
begin
    with CalcModule do SetPatternParams(TComponent(NeutronData.GetCopy));
end;

function TSourceFile.CreateReverseUnit;
var TempComp: TReverseUnit;
begin
    TempComp := CreateNewReverseUnit(NeutronDataType);
    TempComp.Caption := ACaption;
    Add(TempComp);
    Result := TempComp;
end;

function  TSourceFile.CreateDirectUnit;
var TempComp: TDirectUnit;
begin
    TempComp := CreateNewDirectUnit;
    TempComp.Caption := ACaption;
    Add(TempComp);
    Result := TempComp;
end;

constructor TReverseUnit.Create;
begin
    inherited Create(AOwner);
    CreateComments('Comments');
    CreateSourceData('Source_Data', NeutronDataType);
    CreateCalcData('Calculation_Results');
end;

destructor TReverseUnit.Destroy;
begin
    UtilizeObject(Comments);
    UtilizeObject(SourceData);
    UtilizeObject(CalcData);
    inherited Destroy;
end;

constructor TDirectUnit.Create;
begin
    inherited Create(AOwner);
    CreateComments('Comments');
    CreateSourceData('Source_Data');
    CreateCalcData('Calculated_Data');
end;

destructor TDirectUnit.Destroy;
begin
    UtilizeObject(Comments);
    UtilizeObject(SourceData);
    UtilizeObject(CalcData);
    inherited Destroy;
end;

constructor TDirectSourceData.Create;
var IPPC: IPatternParametersContainer;
begin
    inherited Create(AOwner);

    FModelData := CreateNewSiteList;
    FModelData.Caption := 'Structure_Data';

    FPatternParams := CreateNewPatternParams;

    FGeneralData := CreateNewGeneralClass;
    FGeneralData.Caption := 'General_Data';

    FGeneralData.StructurePointer := FModelData;
    FPatternParams.GetInterface(PatternParamsContainerGUID, IPPC);
    FGeneralData.PatternParamsPointer := IPPC;
end;

destructor TDirectSourceData.Destroy;
begin
    UtilizeObject(FModelData);
    UtilizeObject(FGeneralData);
    UtilizeObject(FPatternParams);
    inherited Destroy;
end;

constructor TDirectCalcData.Create;
begin
    inherited Create(AOwner);
    CreateModelSinTData('Calculated_Intensities');
end;

destructor TDirectCalcData.Destroy;
begin
    UtilizeObject(FModelSinTData);
    UtilizeObject(FSinTDataPointer);
    inherited Destroy;
end;

class function TReverseSourceData.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := RSDClassInheritID;
    Result.PropVersionNum := RSDCurVerNum;
end;

class procedure TReverseSourceData.ReadProperties(const Reader: TReader;
    const PropHeaderRec: TPropHeaderRec;
    const AnObject: TSelfSavedComponent);
var IPPC: IPatternParametersContainer;
begin
    with AnObject as Self do
    begin
        UtilizeObject(FGeneralData);
        UtilizeObject(FStructureData);
        UtilizeObject(FNeutronData);

        FStructureData := TSiteList(ReadComponentByReader(Reader));
        FNeutronData := TNeutronCompList(ReadComponentByReader(Reader));
        FGeneralData := TGeneralClass(ReadComponentByReader(Reader));
        FCaption := Reader.ReadString;

        FGeneralData.StructurePointer := StructureData;
        NeutronData.GetInterface(PatternParamsContainerGUID, IPPC);
        FGeneralData.PatternParamsPointer := IPPC;
    end;
end;

class procedure TReverseSourceData.WriteProperties(const Writer: TWriter;
    const AnObject: TSelfSavedComponent);
begin
    with AnObject as Self, Writer do
    begin
        WriteComponent(FStructureData);
        WriteComponent(FNeutronData);
        WriteComponent(FGeneralData);
        WriteString(FCaption);
    end;
end;

class function TReverseCalcData.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := RCDClassInheritID;
    Result.PropVersionNum := RCDCurVerNum;
end;

class procedure TReverseCalcData.ReadProperties(const Reader: TReader;
    const PropHeaderRec: TPropHeaderRec;
    const AnObject: TSelfSavedComponent);
begin
    with AnObject as Self do
    begin
        UtilizeObject(FCalcStructureData);
        UtilizeObject(FSinTData);

        FCalcStructureData := TSiteList(ReadComponentByReader(Reader));
        FSinTData := TSinTCompList(ReadComponentByReader(Reader));
        FCaption := Reader.ReadString;

        FSinTDataPointer := CreateNewSinTDataPointer;
        FSinTDataPointer.SinTPointer := SinTData;
        FSinTDataPointer.PlotMode := PM_INTENSITY or PM_NUCLEAR_INTENSITY
        or PM_EXP_INTENSITY;
    end;
end;

class procedure TReverseCalcData.WriteProperties(const Writer: TWriter;
    const AnObject: TSelfSavedComponent);
begin
    with AnObject as Self, Writer do
    begin
        WriteComponent(FCalcStructureData);
        WriteComponent(FSinTData);
        WriteString(FCaption);
    end;
end;

class function TReverseUnit.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := RUClassInheritID;
    Result.PropVersionNum := RUCurVerNum;
end;

class procedure TReverseUnit.ReadProperties(const Reader: TReader;
    const PropHeaderRec: TPropHeaderRec;
    const AnObject: TSelfSavedComponent);
begin
    with AnObject as Self do
    begin
        UtilizeObject(FSourceData);
        UtilizeObject(FCalcData);
        UtilizeObject(FComments);

        FSourceData := TReverseSourceData(ReadComponentByReader(Reader));
        FCalcData := TReverseCalcData(ReadComponentByReader(Reader));
        FComments := ReadComponentByReader(Reader);
        FCaption := Reader.ReadString;
    end;
end;

class procedure TReverseUnit.WriteProperties(const Writer: TWriter;
    const AnObject: TSelfSavedComponent);
begin
    with AnObject as Self, Writer do
    begin
        WriteComponent(FSourceData);
        WriteComponent(FCalcData);
        WriteComponent(FComments);
        WriteString(FCaption);
    end;
end;

class function TDirectSourceData.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := DSDClassInheritID;
    Result.PropVersionNum := DSDCurVerNum;
end;

class procedure TDirectSourceData.ReadProperties(const Reader: TReader;
    const PropHeaderRec: TPropHeaderRec;
    const AnObject: TSelfSavedComponent);
var IPPC: IPatternParametersContainer;
begin
    with AnObject as Self do
    begin
        UtilizeObject(FModelData);
        UtilizeObject(FGeneralData);
        UtilizeObject(FPatternParams);

        FModelData := TSiteList(ReadComponentByReader(Reader));
        FGeneralData := TGeneralClass(ReadComponentByReader(Reader));
        FPatternParams := TPatternParametersContainer(ReadComponentByReader(Reader));
        FCaption := Reader.ReadString;

        FGeneralData.StructurePointer := FModelData;
        FPatternParams.GetInterface(PatternParamsContainerGUID, IPPC);
        FGeneralData.PatternParamsPointer := IPPC;
    end;
end;

class procedure TDirectSourceData.WriteProperties(const Writer: TWriter;
    const AnObject: TSelfSavedComponent);
begin
    with AnObject as Self, Writer do
    begin
        WriteComponent(FModelData);
        WriteComponent(FGeneralData);
        WriteComponent(FPatternParams);
        WriteString(FCaption);
    end;
end;

class function TDirectCalcData.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := DCDClassInheritID;
    Result.PropVersionNum := DCDCurVerNum;
end;

class procedure TDirectCalcData.ReadProperties(const Reader: TReader;
    const PropHeaderRec: TPropHeaderRec;
    const AnObject: TSelfSavedComponent);
begin
    with AnObject as Self do
    begin
        UtilizeObject(FModelSinTData);

        FModelSinTData := TTableCompList(ReadComponentByReader(Reader));
        FCaption := Reader.ReadString;

        UtilizeObject(FSinTDataPointer);
        FSinTDataPointer := CreateNewSinTDataPointer;
        FSinTDataPointer.SinTPointer := TSinTCompList(ModelSinTData);
        FSinTDataPointer.PlotMode := PM_INTENSITY or PM_NUCLEAR_INTENSITY
            or PM_EXP_INTENSITY;
    end;
end;

class procedure TDirectCalcData.WriteProperties(const Writer: TWriter;
    const AnObject: TSelfSavedComponent);
begin
    with AnObject as Self, Writer do
    begin
        WriteComponent(FModelSinTData);
        WriteString(FCaption);
    end;
end;

class function TDirectUnit.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := DUClassInheritID;
    Result.PropVersionNum := DUCurVerNum;
end;

class procedure TDirectUnit.ReadProperties(const Reader: TReader;
    const PropHeaderRec: TPropHeaderRec;
    const AnObject: TSelfSavedComponent);
begin
    with AnObject as Self do
    begin
        UtilizeObject(FSourceData);
        UtilizeObject(FCalcData);
        UtilizeObject(FComments);

        FSourceData := TDirectSourceData(ReadComponentByReader(Reader));
        FCalcData := ReadComponentByReader(Reader);
        FComments := ReadComponentByReader(Reader);
        FCaption := Reader.ReadString;
    end;
end;

class procedure TDirectUnit.WriteProperties(const Writer: TWriter;
    const AnObject: TSelfSavedComponent);
begin
    with AnObject as Self, Writer do
    begin
        WriteComponent(FSourceData);
        WriteComponent(FCalcData);
        WriteComponent(FComments);
        WriteString(FCaption);
    end;
end;

{ TDCD }

class function TDCD.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := DCDAlClassInheritID;
    Result.PropVersionNum := DCDAlCurVerNum;
end;

class procedure TDCD.ReadProperties(const Reader: TReader;
    const PropHeaderRec: TPropHeaderRec;
    const AnObject: TSelfSavedComponent);
begin

end;

class procedure TDCD.WriteProperties(const Writer: TWriter;
    const AnObject: TSelfSavedComponent);
begin

end;

{ TDSD }

class function TDSD.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := DSDAlClassInheritID;
    Result.PropVersionNum := DSDAlCurVerNum;
end;

class procedure TDSD.ReadProperties(const Reader: TReader;
  const PropHeaderRec: TPropHeaderRec;
  const AnObject: TSelfSavedComponent);
begin

end;

class procedure TDSD.WriteProperties(const Writer: TWriter;
  const AnObject: TSelfSavedComponent);
begin

end;

{ TDU }

class function TDU.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := DUAlClassInheritID;
    Result.PropVersionNum := DUAlCurVerNum;
end;

class procedure TDU.ReadProperties(const Reader: TReader;
    const PropHeaderRec: TPropHeaderRec;
    const AnObject: TSelfSavedComponent);
begin

end;

class procedure TDU.WriteProperties(const Writer: TWriter;
    const AnObject: TSelfSavedComponent);
begin

end;

{ TRCD }

class function TRCD.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := RCDAlClassInheritID;
    Result.PropVersionNum := RCDAlCurVerNum;
end;

class procedure TRCD.ReadProperties(const Reader: TReader;
    const PropHeaderRec: TPropHeaderRec;
    const AnObject: TSelfSavedComponent);
begin

end;

class procedure TRCD.WriteProperties(const Writer: TWriter;
    const AnObject: TSelfSavedComponent);
begin

end;

{ TRSD }

class function TRSD.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := RSDAlClassInheritID;
    Result.PropVersionNum := RSDAlCurVerNum;
end;

class procedure TRSD.ReadProperties(const Reader: TReader;
    const PropHeaderRec: TPropHeaderRec;
    const AnObject: TSelfSavedComponent);
begin

end;

class procedure TRSD.WriteProperties(const Writer: TWriter;
    const AnObject: TSelfSavedComponent);
begin

end;

{ TRU }

class function TRU.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := RUAlClassInheritID;
    Result.PropVersionNum := RUAlCurVerNum;
end;

class procedure TRU.ReadProperties(const Reader: TReader;
    const PropHeaderRec: TPropHeaderRec;
    const AnObject: TSelfSavedComponent);
begin

end;

class procedure TRU.WriteProperties(const Writer: TWriter;
    const AnObject: TSelfSavedComponent);
begin

end;

{ TSF }

class function TSF.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := SFAlClassInheritID;
    Result.PropVersionNum := SFAlCurVerNum;
end;

class procedure TSF.ReadProperties(const Reader: TReader;
    const PropHeaderRec: TPropHeaderRec;
    const AnObject: TSelfSavedComponent);
begin

end;

class procedure TSF.WriteProperties(const Writer: TWriter;
    const AnObject: TSelfSavedComponent);
begin

end;

class function TSourceFile.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := SFClassInheritID;
    Result.PropVersionNum := SFCurVerNum;
end;

class procedure TSourceFile.ReadProperties(const Reader: TReader;
    const PropHeaderRec: TPropHeaderRec;
    const AnObject: TSelfSavedComponent);
begin
    with AnObject as Self do
        FCaption := Reader.ReadString;
end;

class procedure TSourceFile.WriteProperties(const Writer: TWriter;
    const AnObject: TSelfSavedComponent);
begin
    with AnObject as Self do
        Writer.WriteString(FCaption);
end;

initialization
    RegisterClass(TSourceFile);
    RegisterClass(TReverseSourceData);
    RegisterClass(TReverseCalcData);
    RegisterClass(TReverseUnit);
    RegisterClass(TDirectSourceData);
    RegisterClass(TDirectCalcData);
    RegisterClass(TDirectUnit);

    RegisterClass(TDCD);
    RegisterClass(TDSD);
    RegisterClass(TDU);
    RegisterClass(TRCD);
    RegisterClass(TRSD);
    RegisterClass(TRU);
    RegisterClass(TSTDP);
    RegisterClass(TSF);
end.
