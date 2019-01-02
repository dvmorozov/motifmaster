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
    SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Forms,
    Dialogs, DB, DBTables, Grids, DBGrids, Menus, Tabs, StdCtrls, ExtCtrls,
    Plotter, Plotter2, ClipBrd, SimpMath, Buttons, ComCtrls, NumericGrid,
    ShellAPI, FileCtrl, ToolWin, ImgList, Controls, Math3d, SourceFile,
    ComponentList, DataClasses, CalcModule, Tools, FFDataFile, SyntMessages,
    TableComp, AlgorithmContainer, DownhillSimplexContainer,
    FocusPanel;

    {$REALCOMPATIBILITY ON}
const
    LBM_VIEW            = 0;
    LBM_CALC            = 1;

type
    TForm1 = class(TForm, IUpdatingResults)
        MainMenu: TMainMenu;
        MenuFileEdit: TMenuItem;
        MenuFileNew: TMenuItem;
        MenuFileOpen: TMenuItem;
        MenuFileExit: TMenuItem;
        MenuFileSave: TMenuItem;
        MenuCalculation: TMenuItem;
        SaveDialog1: TSaveDialog;
        MenuFileSaveAs: TMenuItem;
        MenuStartReverse: TMenuItem;
        MenuStopReverse: TMenuItem;
        StartRecording: TMenuItem;
        StopRecording: TMenuItem;
        RecordItem: TMenuItem;
        StatusBar1: TStatusBar;
        MenuDirect: TMenuItem;
        OpenDialog1: TOpenDialog;
        RecordViewer: TMenuItem;
        N5: TMenuItem;
        N3: TMenuItem; 
        Help1: TMenuItem;
        Contents1: TMenuItem; 
        N6: TMenuItem;
        About1: TMenuItem;
        MenuFileNewUnit: TMenuItem;
        PanelTable: TPanel;
        PanelGeneralData: TPanel;
        GroupBox1: TGroupBox; 
        InputA: TEdit; 
        InputB: TEdit;
        InputC: TEdit;
        InputAlpha: TEdit;
        InputBeta: TEdit;
        InputGamma: TEdit;
        Label1: TLabel; 
        Label2: TLabel; 
        Label3: TLabel; 
        Label4: TLabel; 
        Label5: TLabel; 
        Label6: TLabel;
        MenuFileNewDirect: TMenuItem;
        PanelComments: TPanel;
        Comments: TMemo;
        PanelBack: TPanel;
        StaticMsg: TStaticText; 
        GroupBox2: TGroupBox;
        InputLambda: TEdit; 
        Label7: TLabel;
        Splitter: TSplitter; 
        N12: TMenuItem;
        PanelPlotter: TFocusPanel;
    MenuFileNewPoly: TMenuItem;
        PanelSites: TPanel;
        TopPanel: TPanel;
        Panel1: TPanel;
        Panel6: TPanel;
        PanelSitesRepr: TPanel;
        BottomPanel: TPanel; 
        Splitter1: TSplitter;
        Panel4: TPanel;
        PanelSitesBF: TPanel;
        SourceFileContentsPanel: TPanel; 
        SourceFileContents: TTreeView; 
        Plotter: TPaintBox;
        CoolBar2: TCoolBar; 
        ToolBar2: TToolBar;
        ToolPlotterCopy: TToolButton;
        ToolPlotterSeparat1: TToolButton;
        ToolPlotterZoomIn: TToolButton;
        ToolPlotterZoomOut: TToolButton;
        ToolPlotterRotX: TToolButton;
        ToolPlotterRotY: TToolButton;
        ToolPlotterRotZ: TToolButton;
        ToolPlotterReset: TToolButton;
        CoolBar3: TCoolBar;
        ToolBar3: TToolBar;
        ToolMemoCopy: TToolButton;
        ToolMemoPaste: TToolButton;
        CoolBar4: TCoolBar;
        ToolBar4: TToolBar;
        CoolBar5: TCoolBar; 
        ToolBar5: TToolBar;
        ToolTableCopy: TToolButton;
        ToolTablePaste: TToolButton;
        ToolTableDelete: TToolButton;
        CoolBar6: TCoolBar;
        ToolBar6: TToolBar; 
        CoolBar1: TCoolBar; 
        ToolBar1: TToolBar;
        NewBut: TToolButton;
        OpenBut: TToolButton; 
        SaveBut: TToolButton; 
        ToolButton9: TToolButton;
        ToolButton1: TToolButton;
        ToolPlotterSeparat2: TToolButton;
        Panel3: TPanel;
        PanelCalcOptions: TPanel;
        ImportSARAhsoutputfile: TMenuItem;
        N1: TMenuItem; 
        ImageList3: TImageList;
        Splitter2: TSplitter;
        Splitter3: TSplitter;
        Splitter4: TSplitter;
        CoolBar10: TCoolBar;
        Label9: TLabel;
        ToolBarSites: TToolBar;
        ToolAddSite: TToolButton;
        ToolDelSite: TToolButton;
        ToolEditSite: TToolButton;
        CoolBar7: TCoolBar;
        ToolBarAtoms: TToolBar;
        ToolAtomsCopy: TToolButton;
        ToolAtomsPaste: TToolButton;
        ToolAtomsDelete: TToolButton;
        GroupBox5: TGroupBox;
        Sites: TListBox;
        GroupBox6: TGroupBox;
        CoolBar11: TCoolBar;
        ToolBarBF: TToolBar;
        ToolBFCopy: TToolButton;
        ToolBFPaste: TToolButton;
        ToolBFDelete: TToolButton;
        GroupBox7: TGroupBox;
        CoolBar12: TCoolBar;
        ToolBarStars: TToolBar;
        ToolAddPropVect: TToolButton;
        ToolDelPropVect: TToolButton;
        ToolEditPropVect: TToolButton;
        GroupBox8: TGroupBox;
        WaveVectors: TListBox;
        CoolBar13: TCoolBar;
        ToolBarRepr: TToolBar;
        ToolAddRepr: TToolButton;
        ToolDelRepr: TToolButton;
        GroupBox9: TGroupBox;
        Representations: TListBox;
        Panel9: TPanel;
        Splitter5: TSplitter;
        Splitter6: TSplitter;
        PanelSitesCalcOpt: TPanel;
        Label10: TLabel;
        CoolBar14: TCoolBar;
        ToolBar14: TToolBar;
        ToolButSitesCalcOpt: TToolButton;
        GroupBox10: TGroupBox;
        ListBoxSitesCalcOpt: TListBox;
        PanelStarsCalcOpt: TPanel;
        CoolBar15: TCoolBar;
        ToolBar15: TToolBar;
        ToolButton14: TToolButton;
        GroupBox11: TGroupBox;
        ListBoxPropVectCalcOpt: TListBox;
        PanelReprCalcOpt: TPanel;
        CoolBar16: TCoolBar;
        GroupBox12: TGroupBox;
        ListBoxReprCalcOpt: TListBox;
        Panel5: TPanel;
        GroupCommonSettings: TGroupBox;
        CheckDebyeWaller: TCheckBox;
        SelectorContainerPanel: TPanel;
        RFactorSourceSelector: TRadioGroup;
        CurJobProgress: TProgressBar;
        ToolBar9: TToolBar;
        RadioLorentz: TRadioGroup;
        ToolAtomsHelp: TToolButton;
        ToolBFHelp: TToolButton;
        ToolPlotterSeparat3: TToolButton;
        ToolPlotterProperties: TToolButton;
        ToolButton16: TToolButton;
        ToolButton17: TToolButton;
        GroupDirectSettings: TGroupBox;
        Label25: TLabel;
        NeutronDataSelector: TComboBox;
        GroupPatternBounds: TGroupBox;
        CheckUseStartEnd: TCheckBox;
        Label8: TLabel;
        InputStartPos: TEdit;
        Label21: TLabel;
        InputEndPos: TEdit;
        MenuCheckDataValidity: TMenuItem;
        Atoms: TColoredGrid;
        BasisFunctions: TColoredGrid;
        Table: TColoredGrid;
        GroupBox3: TGroupBox;
        CalcComments: TMemo;
        CheckIntensNorm: TCheckBox;
        ToolButton2: TToolButton;
        ToolButton8: TToolButton;
        ToolButton10: TToolButton;
        ToolButton11: TToolButton;
        ToolButton13: TToolButton;
        ToolButton15: TToolButton;
        PanelResultedStruct: TPanel;
        MenuProjectEdit: TMenuItem;
        MenuProjectRenameUnit: TMenuItem;
        MenuProjectDeleteUnit: TMenuItem;
        MenuAtomsEdit: TMenuItem;
        MenuAtomsCopy: TMenuItem;
        MenuAtomsPaste: TMenuItem;
        MenuAtomsDelete: TMenuItem;
        N7: TMenuItem;
        MenuAtomsSelectAll: TMenuItem;
        MenuAtomsInsert: TMenuItem;
        N13: TMenuItem;
        MenuAtomsAdd: TMenuItem;
        MenuBFEdit: TMenuItem;
        MenuBFCopy: TMenuItem;
        MenuBFPaste: TMenuItem;
        MenuBFDelete: TMenuItem;
        N14: TMenuItem;
        MenuBFSelectAll: TMenuItem;
        N15: TMenuItem;
        MenuBFInsert: TMenuItem;
        MenuBFAdd: TMenuItem;
        MenuSitesEdit: TMenuItem;
        MenuSitesAdd: TMenuItem;
        MenuSitesDelete: TMenuItem;
        N16: TMenuItem;
        MenuSitesProperties: TMenuItem;
        MenuStarsEdit: TMenuItem;
        MenuStarsAdd: TMenuItem;
        MenuStarsDelete: TMenuItem;
        N17: TMenuItem;
        MenuStarsProperties: TMenuItem;
        MenuReprEdit: TMenuItem;
        MenuReprAdd: TMenuItem;
        MenuReprDelete: TMenuItem;
        MenuResultsEdit: TMenuItem;
        MenuResultsCopy: TMenuItem;
        MenuNeutronEdit: TMenuItem;
        MenuNeutronCopy: TMenuItem;
        MenuNeutronPaste: TMenuItem;
        MenuNeutronDelete: TMenuItem;
        N18: TMenuItem;
        MenuNeutronSelectAll: TMenuItem;
        N19: TMenuItem;
        MenuNeutronInsert: TMenuItem;
        MenuNeutronAdd: TMenuItem;
        MenuPlotPatternEdit: TMenuItem;
        MenuPlotPatternCopy: TMenuItem;
        MenuPlotPatternSave: TMenuItem;
        MenuPlotStructEdit: TMenuItem;
        MenuPlotStructCopy: TMenuItem;
        MenuPlotStructSave: TMenuItem;
        MenuProjectView: TMenuItem;
        MenuProjectAutoExpand: TMenuItem;
        MenuProjectFullExpand: TMenuItem;
        PopupProject: TPopupMenu;
        PopupProjectRenameUnit: TMenuItem;
        PopupProjectDeleteUnit: TMenuItem;
        N2: TMenuItem;
        PopupProjectAutoExpand: TMenuItem;
        PopupProjectFullExpand: TMenuItem;
        PopupAtoms: TPopupMenu;
        PopupAtomsCopy: TMenuItem;
        PopupAtomsPaste: TMenuItem;
        PopupAtomsDelete: TMenuItem;
        N20: TMenuItem;
        PopupAtomsSelectAll: TMenuItem;
        N21: TMenuItem;
        PopupAtomsInsert: TMenuItem;
        PopupAtomsAdd: TMenuItem;
        PopupBF: TPopupMenu;
        PopupBFCopy: TMenuItem;
        PopupBFPaste: TMenuItem;
        PopupBFDelete: TMenuItem;
        N22: TMenuItem;
        PopupBFSelectAll: TMenuItem;
        N23: TMenuItem;
        PopupBFInsert: TMenuItem;
        PopupBFAdd: TMenuItem;
        PopupSites: TPopupMenu;
        PopupSitesAdd: TMenuItem;
        PopupSitesDelete: TMenuItem;
        N11: TMenuItem;
        PopupSitesProperties: TMenuItem;
        PopupStars: TPopupMenu;
        PopupStarsAdd: TMenuItem;
        PopupStarsDelete: TMenuItem;
        N24: TMenuItem;
        PopupStarsProperties: TMenuItem;
        PopupRepr: TPopupMenu;
        PopupReprAdd: TMenuItem;
        PopupReprDelete: TMenuItem;
        PopupResults: TPopupMenu;
        PopupResultsCopy: TMenuItem;
        PopupNeutron: TPopupMenu;
        PopupNeutronCopy: TMenuItem;
        PopupNeutronPaste: TMenuItem;
        PopupNeutronDelete: TMenuItem;
        N9: TMenuItem;
        PopupNeutronSelectAll: TMenuItem;
        N25: TMenuItem;
        PopupNeutronInsert: TMenuItem;
        PopupNeutronAdd: TMenuItem;
        PopupPlotStruct: TPopupMenu;
        PopupPlotStructCopy: TMenuItem;
        PopupPlotStructSave: TMenuItem;
        MenuMemoEdit: TMenuItem;
        MenuMemoCopy: TMenuItem;
        MenuMemoPaste: TMenuItem;
        N10: TMenuItem;
        MenuFFEdit: TMenuItem;
        MenuSettings: TMenuItem;
        PopupMemo: TPopupMenu;
        PopupMemoCopy: TMenuItem;
        PopupMemoPaste: TMenuItem;
        PopupPlotPattern: TPopupMenu;
        PopupPlotPatternNeutron: TMenuItem;
        PopupPlotPatternNucl: TMenuItem;
        PopupPlotPatternMagn: TMenuItem;
        PopupPlotPatternStructFact: TMenuItem;
        MenuPlotPatternView: TMenuItem;
        MenuPlotPatternNeutron: TMenuItem;
        MenuPlotPatternNucl: TMenuItem;
        MenuPlotPatternMagn: TMenuItem;
        MenuPlotPatternStructFact: TMenuItem;
        N4: TMenuItem;
        PopupPlotPatternCopy: TMenuItem;
        PopupPlotPattenSave: TMenuItem;
    MenuFileNewMono: TMenuItem;
        procedure FormCreate(Sender: TObject);
        procedure MenuFileExitClick(Sender: TObject);
        procedure FormClose(Sender: TObject; var Action: TCloseAction); 
        procedure MenuFileSaveClick(Sender: TObject);
        procedure MenuFileOpenClick(Sender: TObject);
        procedure MenuStartReverseClick(Sender: TObject);
        procedure MenuFileSaveAsClick(Sender: TObject);
        procedure MenuFileNewClick(Sender: TObject);
        procedure MenuStopReverseClick(Sender: TObject);
        procedure ToolPlotterRotZClick(Sender: TObject);
        procedure ToolPlotterRotYClick(Sender: TObject);
        procedure ToolPlotterRotXClick(Sender: TObject);
        procedure ToolPlotterResetClick(Sender: TObject);
        procedure ToolPlotterZoomInClick(Sender: TObject);
        procedure ToolPlotterZoomOutClick(Sender: TObject);
        procedure MenuDirectClick(Sender: TObject); 
        procedure Contents1Click(Sender: TObject); 
        procedure About1Click(Sender: TObject); 
        procedure MenuFileNewDirectClick(Sender: TObject);
        procedure PlotterPaint(Sender: TObject);
        procedure SourceFileContentsChanging(Sender: TObject; Node: TTreeNode; 
        var AllowChange: Boolean);
        procedure SourceFileContentsChange(Sender: TObject; Node: TTreeNode); 
        procedure TableEnter(Sender: TObject); 
        procedure TableExit(Sender: TObject);
        procedure SourceFileContentsKeyPress(Sender: TObject; var Key: Char);
        procedure SourceFileContentsEditing(Sender: TObject; Node: TTreeNode;
        var AllowEdit: Boolean);
        procedure SourceFileContentsEdited(Sender: TObject; Node: TTreeNode;
        var S: string);
        procedure SourceFileContentsEnter(Sender: TObject); 
        procedure SourceFileContentsExit(Sender: TObject);
        procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); 
        procedure MenuPlotPatternNeutronClick(Sender: TObject);
        procedure MenuPlotPatternNuclClick(Sender: TObject);
        procedure MenuPlotPatternMagnClick(Sender: TObject);
        procedure MenuPlotPatternStructFactClick(Sender: TObject);
        procedure MenuFileNewPolyClick(Sender: TObject);
        procedure SitesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure SitesClick(Sender: TObject);
        procedure WaveVectorsKeyDown(Sender: TObject; var Key: Word;
        Shift: TShiftState);
        procedure RepresentationsKeyDown(Sender: TObject; var Key: Word;
        Shift: TShiftState);
        procedure WaveVectorsClick(Sender: TObject);
        procedure RepresentationsClick(Sender: TObject);
        procedure AtomsExit(Sender: TObject);
        procedure BasisFunctionsExit(Sender: TObject);
        procedure InputAKeyPress(Sender: TObject; var Key: Char);
        procedure EditWVButClick(Sender: TObject);
        procedure NeutronDataSelectorChange(Sender: TObject);
        procedure CommentsChange(Sender: TObject);
        procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
        procedure CheckUseStartEndClick(Sender: TObject);
        procedure TableMouseDown(Sender: TObject; Button: TMouseButton;
          Shift: TShiftState; X, Y: Integer);
        procedure ToolButSitesCalcOptClick(Sender: TObject);
        procedure ListBoxSitesCalcOptClick(Sender: TObject);
        procedure ListBoxSitesCalcOptKeyDown(Sender: TObject; var Key: Word;
          Shift: TShiftState);
        procedure ToolPlotterPropertiesClick(Sender: TObject);
        procedure FormResize(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
        procedure MenuCheckDataValidityClick(Sender: TObject);
        procedure ToolAtomsHelpClick(Sender: TObject);
        procedure ToolButton2Click(Sender: TObject);
        procedure ToolBFHelpClick(Sender: TObject);
        procedure ToolButton11Click(Sender: TObject);
        procedure ToolButton15Click(Sender: TObject);
        procedure MenuProjectDeleteUnitClick(Sender: TObject);
        procedure MenuProjectRenameUnitClick(Sender: TObject);
        procedure SitesEnter(Sender: TObject);
        procedure SitesExit(Sender: TObject);
        procedure WaveVectorsEnter(Sender: TObject);
        procedure WaveVectorsExit(Sender: TObject);
        procedure RepresentationsEnter(Sender: TObject);
        procedure RepresentationsExit(Sender: TObject);
        procedure AtomsEnter(Sender: TObject);
        procedure BasisFunctionsEnter(Sender: TObject);
        procedure MenuProjectAutoExpandClick(Sender: TObject);
        procedure MenuProjectFullExpandClick(Sender: TObject);
        procedure MenuAtomsCopyClick(Sender: TObject);
        procedure MenuAtomsPasteClick(Sender: TObject);
        procedure MenuAtomsDeleteClick(Sender: TObject);
        procedure MenuAtomsSelectAllClick(Sender: TObject);
        procedure MenuAtomsInsertClick(Sender: TObject);
        procedure MenuAtomsAddClick(Sender: TObject);
        procedure MenuBFCopyClick(Sender: TObject);
        procedure MenuBFPasteClick(Sender: TObject);
        procedure MenuBFDeleteClick(Sender: TObject);
        procedure MenuBFSelectAllClick(Sender: TObject);
        procedure MenuBFAddClick(Sender: TObject);
        procedure MenuBFInsertClick(Sender: TObject);
        procedure MenuSitesAddClick(Sender: TObject);
        procedure MenuSitesDeleteClick(Sender: TObject);
        procedure MenuSitesPropertiesClick(Sender: TObject);
        procedure MenuStarsAddClick(Sender: TObject);
        procedure MenuStarsDeleteClick(Sender: TObject);
        procedure MenuStarsPropertiesClick(Sender: TObject);
        procedure MenuReprAddClick(Sender: TObject);
        procedure MenuReprDeleteClick(Sender: TObject);
        procedure MenuResultsCopyClick(Sender: TObject);
        procedure MenuNeutronCopyClick(Sender: TObject);
        procedure MenuNeutronPasteClick(Sender: TObject);
        procedure MenuNeutronDeleteClick(Sender: TObject);
        procedure MenuNeutronSelectAllClick(Sender: TObject);
        procedure MenuNeutronInsertClick(Sender: TObject);
        procedure MenuNeutronAddClick(Sender: TObject);
        procedure MenuPlotPatternCopyClick(Sender: TObject);
        procedure MenuPlotStructCopyClick(Sender: TObject);
        procedure MenuPlotPatternSaveClick(Sender: TObject);
        procedure MenuPlotStructSaveClick(Sender: TObject);
        procedure MenuMemoCopyClick(Sender: TObject);
        procedure MenuMemoPasteClick(Sender: TObject);
        procedure MenuFFEditClick(Sender: TObject);
        procedure CommentsEnter(Sender: TObject);
        procedure CommentsExit(Sender: TObject);
        procedure MenuAtomsEditClick(Sender: TObject);
        procedure MenuBFEditClick(Sender: TObject);
        procedure MenuNeutronEditClick(Sender: TObject);
        procedure MenuMemoEditClick(Sender: TObject);
        procedure AtomsGridModified(Sender: TObject);
    procedure MenuSitesEditClick(Sender: TObject);
    procedure MenuStarsEditClick(Sender: TObject);
    procedure MenuReprEditClick(Sender: TObject);
    procedure MenuFileNewMonoClick(Sender: TObject);

    protected
        FSaved: Boolean;
        FSourceFile: TSourceFile;
        FFileName: TFileName;
        FFileOpened: Boolean;
        FPlotMode: Integer;
        FPlotDiagrMode: Integer;
        FCurModified: Boolean;

        procedure SetSavedState(SaveState: Boolean);
        procedure SetSourceFile(ASourceFile: TSourceFile);
        procedure SetFileName(AFileName: TFileName);
        procedure SetFileOpenedState(AFileOpened: Boolean);
        procedure SetPlotDiagrMode(APlotDiagrMode: Integer);
        procedure SetModifiedState(AModifiedState: Boolean);
        procedure Paint(var Msg: TMessage); message WM_PAINT;

    public
        Container: TAlgorithmContainer;
        CalcModule: TCalcModule;
        CalcModuleDirect: TCalcModule;
        Flag_Rec: Boolean;
        RecFile: TFileStream;
        Ext: string;
        TempBitMap: TBitMap;
        PlayerPath: string;
        FFPath: string;
        CurrentPath: string;
        OpenFilePath: string;

        Plotter3D: TExtBufPlotter3D;

        TreeUpdatingAllowed: Boolean;
        FileLoading: Boolean;

        procedure Minimize;

        procedure Save(SaveAs: Boolean);

        procedure StartRecord;
        procedure MakeRecord;
        procedure StopRecord;

        function SetUserParameters(ACalcModule: TCalcModule): Boolean;

        procedure StandardViewAction;

        procedure LoadProgramSettings;
        procedure SaveProgramSettings;
        procedure SetDefaultSettings;
        procedure LoadImages;

        procedure NewSourceFile;
        procedure OpenFile(AFileName: string);
        procedure SaveFile(AFileName: string);

        function GetReverseData(ACalcModule: TCalcModule;
        ASourceFile: TSourceFile): Boolean;
        function GetDirectData(ACalcModule: TCalcModule;
        ASourceFile: TSourceFile): Boolean;

        function GetSelectedData: TComponent;
        function GetLastEditedData(TC: TComponent): Boolean;
        function GetNeutronDataCollection(ASourceFile: TSourceFile): TStrings;
        procedure FillSites;
        procedure FillRepresentations;
        procedure FillPropVectors;

        procedure FillAtoms;
        procedure FillBasisFunctions;
        procedure AssignAtoms;
        procedure AssignBasisFunctions;
        procedure ReleaseAtoms;
        procedure ReleaseBasisFunctions;

        procedure SetAtoms;
        procedure SetBasisFunctions;

        function GetCurSite: TSite;
        function GetCurRepr: TRepresentation;

        procedure SitesChanged;
        procedure PropVectorsChanged;
        procedure RepresentationsChanged;

        procedure LinkPlotterParamsWithSiteList;

        procedure FillSitesCalcOpt;
        procedure FillPropVectorsCalcOpt;
        procedure SitesChangedCalcOpt;
        procedure PropVectorsChangedCalcOpt;

        procedure FillListBoxByPropVectorsList(
            const PropVectorsList: TWaveVectorList; ListBox: TListBox; Mode: LongInt);
        procedure FillListBoxBySiteList(
            const SiteList: TSiteList; ListBox: TListBox; Mode: LongInt);

        procedure ShowHint(Sender: TObject);
        procedure MessagesHandler(var Msg: TMsg; var Handled: Boolean);
        procedure StopCalculationProcess;
        procedure UpdateFFContents;

        procedure ShowCurJobProgress(Sender: TComponent;
            MinValue, MaxValue, CurValue: LongInt);
        procedure ResetCurJobProgress(Sender: TComponent);
        procedure ShowMessage(Sender: TComponent; Msg: string);
        procedure UpdatingResults(Sender: TComponent);

        procedure UpdateStructViewProperties(Sender: TObject);

        procedure ShowFileInTree;

        procedure SaveBitmapIntoFile(const BitMap: TBitMap);

        property Saved: Boolean read FSaved write SetSavedState;
        property SourceFile: TSourceFile read FSourceFile write SetSourceFile;
        property FileName: TFileName read FFileName write SetFileName;
        property FileOpened: Boolean read FFileOpened write SetFileOpenedState;
        property PlotDiagrMode: Integer write SetPlotDiagrMode;

        property CurModified: Boolean
            read FCurModified write SetModifiedState;
    end;

var Form1: TForm1;

const
    PROGRAM_CAPTION: string = 'Motif MASTER v. 3.1';
    PROJECT_FILE_FILTER: string = 'Motif MASTER Project (*.MMP)|*.MMP';
    REC_FILE_FILTER: string = 'File of records (*.REC)|*.REC';
    MAX_CAPTION_LEN = 300;

    SETTINGS_FILE_NAME = 'MotifMASTER.set';
    FORM_FACTORS_FILE_NAME = 'MotifMASTER.ff';
    HELP_FILE_NAME = 'MotifMASTER.hlp';

    UpDownCodes = [VK_UP, VK_DOWN];

type
    TListSortCompare = function(Item1, Item2: Pointer): Integer;

implementation

uses
    Unit2, Unit4, Unit6, Unit7, Main, Unit9, Unit5, Unit8, Unit10,
    Unit11, Unit12;

{$R SyntIcon.res}
{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
var St: string;
begin
    FileOpened := False;
    Saved := True;
    Flag_Rec := False;

    TempBitMap := TBitMap.Create;
    TempBitMap.Width := Plotter.Width;
    TempBitMap.Height := Plotter.Height;

    Plotter3D := TExtBufPlotter3D.Create;
    Plotter3D.SetBufCanvas(TempBitMap.Canvas);
    Plotter3D.SetOutCanvas(Plotter.Canvas);
    Plotter3D.SetHeight(TempBitMap.Height);
    Plotter3D.SetWidth(TempBitMap.Width);

    CurrentPath := ExtractFilePath(ParamStr(0));
    OpenFilePath := ExtractFilePath(ParamStr(0));

    if FileExists(CurrentPath + SETTINGS_FILE_NAME) then
        LoadProgramSettings
    else SetDefaultSettings;

    Application.HelpFile := ExtractFilePath(ParamStr(0)) + HELP_FILE_NAME;
    Application.OnHint := ShowHint;
    Application.OnMessage := MessagesHandler;
    Application.Title := PROGRAM_CAPTION;
    SourceFile := nil;
    FileName := '';
    Table.DoubleBuffered := True;
    Form1.KeyPreview := True;
    LoadImages;
    OpenFFDataFile(FFPath + FORM_FACTORS_FILE_NAME);
    St := ParamStr(1);
    if FileExists(St) then OpenFile(St);
end;

procedure TForm1.MenuFileExitClick(Sender: TObject);
begin
    Close;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    Halt(1);
end;

function TForm1.GetReverseData(ACalcModule: TCalcModule;
    ASourceFile: TSourceFile): Boolean;
var TC: TComponent;
begin
    TC := GetSelectedData;
    if Assigned(TC) then
    begin
        if TC is TReverseUnit then
            with TC as TReverseUnit do
            begin
                CreateCalcData('Calculation_Results');
                Saved := False;
                if SourceData is TReverseSourceData then
                    with SourceData as TReverseSourceData do
                    begin
                        GetStructureDataToCalc(ACalcModule);
                        GetNeutronDataToCalc(ACalcModule);
                    end;
                if CalcData is TReverseCalcData then
                    with CalcData as TReverseCalcData do
                    begin
                        GetCalcStructureDataToCalc(ACalcModule);
                        GetSinTDataToCalc(ACalcModule);
                    end;
            end;    //  with TC as TReverseUnit do...
        ShowFileInTree;
        LinkPlotterParamsWithSiteList;
        Result := True;
    end
    else Result := False;
end;

function TForm1.GetDirectData(ACalcModule: TCalcModule; ASourceFile: TSourceFile): Boolean;
var TC: TComponent;
    IU: TReverseUnit;
begin
    TC := GetSelectedData;
    if Assigned(TC) then
    begin
        if TC is TDirectUnit then
            with TC as TDirectUnit do
            begin
                CreateCalcData('Calculation_Results');
                Saved := False;
                if SourceData is TDirectSourceData then
                with SourceData as TDirectSourceData do
                begin
                    GetDataToCalc(ACalcModule);
                    with NeutronDataSelector do
                    begin
                        if Assigned(Items.Objects[ItemIndex]) and
                          (Items.Objects[ItemIndex] is TReverseUnit) then
                        begin
                            IU := TReverseUnit(Items.Objects[ItemIndex]);
                            IU.SourceData.GetNeutronDataToCalc(CalcModuleDirect);
                        end else GetPatternParamsToCalc(ACalcModule);
                    end;
                end;
                if CalcData is TDirectCalcData then
                    with CalcData as TDirectCalcData do
                        GetModelSinTDataToCalc(ACalcModule);
            end;
        ShowFileInTree;
        LinkPlotterParamsWithSiteList;
        Result := True;
    end else Result := False;
end;

procedure TForm1.Save(SaveAs: Boolean);
var Result: Boolean;
    TempFileName: TFileName;
begin
    if Assigned(SourceFile) then
    begin
        if SaveAs then
        begin
            SaveDialog1.FileName := '';
            if SetCurrentDir(OpenFilePath) then SaveDialog1.InitialDir := OpenFilePath;
            SaveDialog1.Filter := PROJECT_FILE_FILTER;
            Result := SaveDialog1.Execute;
            TempFileName := SaveDialog1.FileName;
            if ExtractFileExt(TempFileName) = '' then
                TempFileName := TempFileName + '.' + PROJECT_DEF_EXT;
            OpenFilePath := ExtractFilePath(TempFileName);
            if Result then
            begin
                if FileExists(TempFileName) then
                    if MessageDlg('File ' + ExtractFileName(TempFileName) +
                        ' already exists...Overwrite ?',
                        mtConfirmation, [mbYes, mbNo, mbCancel], 0) <> mrYes then Exit;
            end else Exit;
        end //  if SaveAs then...
        else TempFileName := FileName;
        Ext := ExtractFileExt(TempFileName);
        if Ext = '' then TempFileName := TempFileName + '.' + PROJECT_DEF_EXT;
        SaveFile(TempFileName);
        Saved := True;
    end;    //  if Assigned(SourceFile) then...
end;

procedure TForm1.MenuFileSaveClick(Sender: TObject);
var SaveCursor: TCursor;
begin
    SaveCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    if Assigned(SourceFile) then
        case SourceFile.Mode of
            sfCreate : Save(True);
            sfOpened : Save(False);
        end;
    Screen.Cursor := SaveCursor;
end;

procedure TForm1.MenuFileOpenClick(Sender: TObject);
var Result: Boolean;
    TempFileName: string;
    St: string;
    DlgResult: Word;
begin
    with OpenDialog1 do
    begin
        FileName := '';
        if SetCurrentDir(OpenFilePath) then InitialDir := OpenFilePath;
        Filter := PROJECT_FILE_FILTER;
        DefaultExt := PROJECT_DEF_EXT;
        Title := 'Open Motif MASTER Project...';
        Result := Execute;
        TempFileName := FileName;
    end;

    if Result then
    begin
        if Assigned(SourceFile) and (not Saved) then
        begin
            St := ExtractFileName(SourceFile.FileName);
            DlgResult := MessageDlg('File ' + St + ' has been modified...Save ?',
                mtConfirmation, [mbYes, mbNo, mbCancel], 0);
            case DlgResult of
                mrYes : case SourceFile.Mode of
                    sfCreate : Save(True);
                    sfOpened : Save(False);
                end;
                mrCancel : Exit;
            end;
        end;
        Ext := ExtractFileExt(TempFileName);
        if Ext = '' then TempFileName := TempFileName + '.' + PROJECT_DEF_EXT;
        if not FileExists(TempFileName) then
        begin
            MessageDlg('File '+ ExtractFileName(TempFileName)+ ' does not exist...',
                mtError, [mbOk], 0);
            Exit;
        end;
        OpenFile(TempFileName);
    end;    //  if Result then...
end;

procedure TForm1.MenuStartReverseClick(Sender: TObject);

    procedure TerminateCurrentCalculation;
    begin
        StopRecord;
        UtilizeObject(CalcModule);
        Container := nil;
        CalcModule := nil;
    end;

var TC: TComponent;
    SaveCursor: TCursor;
begin
    TC := GetSelectedData;
    if (not Assigned(TC)) or (not (TC is TReverseUnit)) then
    begin
        MessageDlg(
        'To start the calculation, you must choose any Reverse data unit...',
        mtInformation, [mbOk], 0);
        Exit;
    end;

    with TC as TReverseUnit do IsReady;

    CalcModule := TCalcModule.Create(nil);
    with CalcModule do
    if FileOpened then
    begin
        SaveCursor := Screen.Cursor;
        Screen.Cursor := crHourGlass;
        if not SetUserParameters(CalcModule) then
        begin TerminateCurrentCalculation; Exit; end;
        if not GetReverseData(CalcModule, SourceFile) then
        begin TerminateCurrentCalculation; Exit; end;
        PrepareDataToCalculation;
        StandardViewAction;
        CalcComments.Lines.Clear;
        Minimize;

        MenuStopReverse.Enabled := True;
        MenuStartReverse.Enabled := False;

        StartRecording.Enabled := True;

        OpenBut.Enabled := False;
        NewBut.Enabled := False;
        Screen.Cursor := SaveCursor;
    end;    //  if FileOpened then...
end;

procedure TForm1.MenuFileSaveAsClick(Sender: TObject);
begin
    Save(True);
end;

procedure TForm1.MenuFileNewClick(Sender: TObject);
begin
    NewSourceFile;
end;

procedure TForm1.MenuStopReverseClick(Sender: TObject);
begin
    StopCalculationProcess;
end;

procedure TForm1.StopCalculationProcess;
begin
    if Assigned(Container) then
    begin
        StopRecord;
        UtilizeObject(CalcModule);
        Container := nil;
        CalcModule := nil;
        MenuStartReverse.Enabled := True;
        MenuStopReverse.Enabled := False;
        StartRecording.Enabled := False;
        StopRecording.Enabled := False;
        OpenBut.Enabled := True;
        NewBut.Enabled := True;
    end;
end;

procedure TForm1.MakeRecord;
begin
end;

procedure TForm1.StartRecord;

begin
end;

procedure TForm1.StopRecord;
begin
end;

procedure TForm1.ToolPlotterRotZClick(Sender: TObject);
begin
    RotationZ(Rad(5));
    Plotter3D.Plot;
end;

procedure TForm1.ToolPlotterRotYClick(Sender: TObject);
begin
    RotationY(Rad(5));
    Plotter3D.Plot;
end;

procedure TForm1.ToolPlotterRotXClick(Sender: TObject);
begin
    RotationX(Rad(5));
    Plotter3D.Plot;
end;

procedure TForm1.ToolPlotterResetClick(Sender: TObject);
begin
    GetUnitMatrix(TempMatr2);
    GetUnitMatrix(TempRotMatr);
    Plotter3D.Plot;
end;

var Dilat1, Dilat2, Dilat3: Real;

procedure TForm1.ToolPlotterZoomInClick(Sender: TObject);
begin
    Dilat3 := Dilat3 / 0.95;
    Dilat2 := Dilat2 / 0.95;
    Dilat1 := Dilat1 / 0.95;
    Dilatation(Dilat1, Dilat2, Dilat3);
    Plotter3D.Plot;
end;

procedure TForm1.ToolPlotterZoomOutClick(Sender: TObject);
begin
    Dilat3 := Dilat3 * 0.95;
    Dilat2 := Dilat2 * 0.95;
    Dilat1 := Dilat1 * 0.95;
    Dilatation(Dilat1, Dilat2, Dilat3);
    Plotter3D.Plot;
end;

procedure TForm1.MenuDirectClick(Sender: TObject);

    procedure TerminateCurrentCalculation;
    begin
        UtilizeObject(CalcModuleDirect);
        CalcModuleDirect := nil;
    end;

var TC: TComponent;
    SaveCursor: TCursor;
begin
    TC := GetSelectedData;
    if (not Assigned(TC)) or (not (TC is TDirectUnit)) then
    begin
        MessageDlg('To start the calculation, you must choose any direct data unit...',
        mtInformation, [mbOk], 0);
        Exit;
    end;

    with TC as TDirectUnit do IsReady;

    TerminateCurrentCalculation;
    CalcModuleDirect := TCalcModule.Create(nil);
    with CalcModuleDirect do
        if FileOpened then
        begin
            SaveCursor := Screen.Cursor;
            Screen.Cursor := crHourGlass;
            if not SetUserParameters(CalcModuleDirect) then
            begin TerminateCurrentCalculation; Exit; end;
            if not GetDirectData(CalcModuleDirect, SourceFile) then
            begin TerminateCurrentCalculation; Exit; end;
            PrepareDataToCalculation;
            MoveViewSinTListData;
            Screen.Cursor := SaveCursor;
        end;
end;

function TForm1.SetUserParameters(ACalcModule: TCalcModule): Boolean;
begin
    if Assigned(ACalcModule) then
        with ACalcModule do
        begin
            VariationMode := 0;
            if CheckDebyeWaller.Checked then
                VariationMode := VariationMode or PVM_DEBYE_WALLER;
            if CheckIntensNorm.Checked then
                VariationMode := VariationMode or PVM_INT_NORMALIZATION;
            RFactorMode := RFactorSourceSelector.ItemIndex;
            IFType := RadioLorentz.ItemIndex;
        end;
    Result := True;
end;

procedure TForm1.StandardViewAction;
var St: string;
    TC: TObject;
    TC2: TComponent;
begin
    if Assigned(CalcModule) then
    with CalcModule do
    begin
        MoveViewSinTListData;
        MoveViewStructListData;
        Inc(LoopCounter);
        St := ' Cycles: ' + IntToStr(LoopCounter);
        StatusBar1.Panels[0].Text := St;
        if Plotter.Visible then
        begin
            TC := GetSelectedData;
            if Assigned(TC) then
            begin
                if TC is TSiteListPlotParams then
                    with TC as TSiteListPlotParams do
                        if GetSiteList = ViewSiteList then Plotter3D.Plot;

                if TC is TSinTDataPointer then
                    with TC as TSinTDataPointer do
                        if SinTPointer = ViewSinTList then with SinTPointer do
                            PlotDiagr(PlotMode, TempBitMap, Plotter, SinTPointer);
            end;    //  if Assigned(TC) do...
        end;    //  if Plotter.Visible then...

        if Table.Visible then
        begin
            TC2 := GetSelectedData;
            if Assigned(TC2) then
                if (TC2 is TSinTCompList) and (TC2 = ViewSinTList) then
                    with TC2 as TSinTCompList do SetDataToGrid(Table);
        end;    //  if Table.Visible then...

        if PanelSites.Visible then
        begin
            TC2 := GetSelectedData;
            if Assigned(TC2) then
                if (TC2 is TSiteList) and (TC2 = ViewSiteList) then
                begin
                    SetAtoms;
                    SetBasisFunctions;
                end;
        end;

        //    if Flag_Rec then MakeRecord;
        Saved := False;
    end;
end;

procedure TForm1.SetDefaultSettings;
begin
    PlayerPath := ExtractFilePath(ParamStr(0));
    FFPath := ExtractFilePath(ParamStr(0));
end;

procedure TForm1.LoadProgramSettings;
var SettingsStream: TFileStream;
    TempStr: string[255];
begin
    try
        try
            SettingsStream := TFileStream.Create(
            CurrentPath + SETTINGS_FILE_NAME, fmOpenRead);
            SettingsStream.Read(TempStr, SizeOf(TempStr));
            PlayerPath := TempStr;
            SettingsStream.Read(TempStr, SizeOf(TempStr));
            FFPath := TempStr;
        except
            MessageDlg('Can''t open settings file...', mtError, [mbOk], 0);
            PlayerPath := CurrentPath;
        end;
    finally
        UtilizeObject(SettingsStream);
    end;
end;

procedure TForm1.SaveProgramSettings;
var SettingsStream: TFileStream;
    TempStr: string[255];
begin
    try
        try
            SettingsStream := TFileStream.Create(CurrentPath + SETTINGS_FILE_NAME, fmCreate);
            TempStr := PlayerPath;
            SettingsStream.Write(TempStr, SizeOf(TempStr));
            TempStr := FFPath;
            SettingsStream.Write(TempStr, SizeOf(TempStr));
        except
            MessageDlg('Can''t create settings file...', mtError, [mbOk], 0);
        end;
    finally
        UtilizeObject(SettingsStream);
    end;
end;

procedure TForm1.Contents1Click(Sender: TObject);
begin
    if FileExists(Application.HelpFile) then
        Application.HelpJump('hlp_Index')
    else MessageDlg('Help file does not exist...', mtError, [mbOk], 0);
end;

procedure TForm1.About1Click(Sender: TObject);
begin
    AboutBox.ShowModal;
end;

procedure TForm1.SetSavedState(SaveState: Boolean);
begin
    SaveBut.Enabled := not SaveState;
    MenuFileSave.Enabled := not SaveState;
    FSaved := SaveState;
    if Assigned(SourceFile) then SourceFile.Modified := not SaveState;
end;

procedure TForm1.NewSourceFile;
var St: string;
    DlgResult: Word;
begin
    if Assigned(SourceFile) and (not Saved) then
    begin
        St := ExtractFileName(SourceFile.FileName);
        DlgResult := MessageDlg('File ' + St + ' has been modified...Save ?',
        mtConfirmation, [mbYes, mbNo, mbCancel], 0);
        case DlgResult of
            mrYes :
                case SourceFile.Mode of
                    sfCreate : Save(True);
                    sfOpened : Save(False);
                end;
            mrCancel: Exit;
        end;
        //  if Flag_Rec then StopRecord;
        UtilizeObject(CalcModule);
        UtilizeObject(CalcModuleDirect);
        UtilizeObject(SourceFile);
    end;

    if SetCurrentDir(OpenFilePath) then
        FileName := OpenFilePath + 'untitled.' + PROJECT_DEF_EXT
    else FileName := CurrentPath + 'untitled.' + PROJECT_DEF_EXT;
    SourceFile := CreateNewSourceFile(FileName);
    FileOpened := True;
    Saved := True;
end;

procedure TForm1.OpenFile(AFileName: string);
var SaveCursor: TCursor;
begin
    try
        FileLoading := True;

        if Assigned(SourceFile) then
        begin
            SourceFileContents.Selected := nil;
            UtilizeObject(SourceFile);
        end;
        SaveCursor := Screen.Cursor;
        Screen.Cursor := crHourGlass;
        FileName := AFileName;

        SourceFile := OpenSourceFile(AFileName);

        Screen.Cursor := SaveCursor;
        if SourceFile <> nil then
        begin
            FileOpened := True;
            Saved := True;
            OpenFilePath := ExtractFilePath(AFileName);
        end else
        begin
            FileOpened := False;
            Saved := True;
            OpenFilePath := '';
        end;
    finally
        FileLoading := False;
    end;
end;

procedure TForm1.SaveFile(AFileName: string);
begin
    if CurModified then GetLastEditedData(GetSelectedData);
    FileName := AFileName;
    SourceFile.FileName := AFileName;
    SaveSourceFile(SourceFile);
end;

procedure TForm1.SetFileName;
var St: string;
begin
    FFileName := AFileName;
    if AFileName <> '' then
    begin
        St := PROGRAM_CAPTION;
        St := St + ' [ ' +
        MinimizeName(AFileName, Form1.Canvas, MAX_CAPTION_LEN) + ' ]';
        Caption := St;
    end else Caption := PROGRAM_CAPTION;
end;

procedure TForm1.SetSourceFile;
begin
    FSourceFile := ASourceFile;
    PanelBack.BringToFront;
    if ASourceFile = nil then
    begin
        MenuFileNewUnit.Enabled := False;
        MenuFileSave.Enabled := False;
        MenuFileSaveAs.Enabled := False;
        StaticMsg.Visible := False;
    end
    else
    begin
        MenuFileNewUnit.Enabled := True;
        MenuFileSaveAs.Enabled := True;
        StaticMsg.Visible := True;
        with ASourceFile do SetTreeView(SourceFileContents);
        ShowFileInTree;
        LinkPlotterParamsWithSiteList;
    end;
end;

procedure TForm1.Minimize;
begin
    Container := TDownhillSimplexContainer.Create(nil);
    with Container as TDownhillSimplexContainer do UpdatingResults := Self;

    CalcModule.Container := Container;
    CalcModule.RunAlgorithm;
end;

procedure TForm1.SetFileOpenedState;
begin
    FFileOpened := AFileOpened;
    if AFileOpened then
    begin
        MenuStartReverse.Enabled := True;
        MenuDirect.Enabled := True;
        MenuCheckDataValidity.Enabled := True;
    end;
end;

procedure TForm1.SetPlotDiagrMode(APlotDiagrMode: Integer);
begin
    FPlotDiagrMode := APlotDiagrMode;

    MenuPlotPatternNeutron.Checked := APlotDiagrMode and PM_EXP_INTENSITY <> 0;
    MenuPlotPatternNucl.Checked := APlotDiagrMode and PM_NUCLEAR_INTENSITY <> 0;
    MenuPlotPatternMagn.Checked := APlotDiagrMode and PM_INTENSITY <> 0;
    MenuPlotPatternStructFact.Checked := APlotDiagrMode and PM_STRUCT_FACT <> 0;

    PopupPlotPatternNeutron.Checked := APlotDiagrMode and PM_EXP_INTENSITY <> 0;
    PopupPlotPatternNucl.Checked := APlotDiagrMode and PM_NUCLEAR_INTENSITY <> 0;
    PopupPlotPatternMagn.Checked := APlotDiagrMode and PM_INTENSITY <> 0;
    PopupPlotPatternStructFact.Checked := APlotDiagrMode and PM_STRUCT_FACT <> 0;
end;

procedure TForm1.MenuFileNewDirectClick(Sender: TObject);
begin
    if Assigned(SourceFile) then
    begin
        SourceFile.CreateDirectUnit('Direct_Data_Unit');
        ShowFileInTree;
        LinkPlotterParamsWithSiteList;
        Saved := False;
    end;
end;

procedure TForm1.PlotterPaint(Sender: TObject);
var TC: TComponent;
begin
    TC := GetSelectedData;
    if Assigned(TC) then
    begin
        if TC is TSiteListPlotParams then Plotter3D.Plot;
        if TC is TSinTDataPointer then
            with TC as TSinTDataPointer, SinTPointer do
                PlotDiagr(PlotMode, TempBitMap, Plotter, SinTPointer);
    end;    //  if Assigned(TC) do...
end;

function TForm1.GetLastEditedData(TC: TComponent): Boolean;

    function GetComments(TC: TComponent): Boolean;
    var i: LongInt;
    begin
        with TC as TCommentsClass do
        begin
            List.Clear;
            for i := 0 to Comments.Lines.Count - 1 do List.Add(Comments.Lines[i]);
            CurModified := False;
        end;
        Result := True;
    end;

    function GetGeneral(TC: TComponent): Boolean;

        procedure MakeMessage(const Control: TWinControl);
        begin
            MessageDlg('Invalid input...', mtError, [mbOk], 0);
            ActiveControl := Control;
        end;

    begin
        Result := True;
        with TC as TGeneralClass do
        begin
            try A := StrToFloat(InputA.Text);
            except MakeMessage(InputA); Result := False; Exit; end;

            try B := StrToFloat(InputB.Text);
            except MakeMessage(InputB); Result := False; Exit; end;

            try C := StrToFloat(InputC.Text);
            except MakeMessage(InputC); Result := False; Exit; end;

            try Alpha := StrToFloat(InputAlpha.Text);
            except MakeMessage(InputAlpha); Result := False; Exit; end;

            try Beta := StrToFloat(InputBeta.Text);
            except MakeMessage(InputBeta); Result := False; Exit; end;

            try Gamma := StrToFloat(InputGamma.Text);
            except MakeMessage(InputGamma); Result := False; Exit; end;

            try Lambda := StrToFloat(InputLambda.Text);
            except MakeMessage(InputLambda); Result := False; Exit; end;
        end;
    end;

    function GetReverseUnit(TC: TComponent): Boolean;
    begin
        with TC as TReverseUnit do
            with SourceData.GeneralData do
            begin
                UseStartEnd := CheckUseStartEnd.Checked;
                if UseStartEnd then try
                    StartPos := StrToFloat(InputStartPos.Text);
                    EndPos := StrToFloat(InputEndPos.Text);
                except
                    Result := False; Exit
                end;
            end;
        Result := True;
    end;

    function GetDirectUnit(TC: TComponent): Boolean;
    begin
        with TC as TDirectUnit do
            with SourceData.GeneralData do
            begin
                UseStartEnd := CheckUseStartEnd.Checked;
                if UseStartEnd then
                begin
                    try
                        StartPos := StrToFloat(InputStartPos.Text);
                    except
                        MessageDlg('Invalid input...', mtError, [mbOk], 0);
                        ActiveControl := InputStartPos;
                        Result := False; Exit
                    end;
                    try
                        EndPos := StrToFloat(InputEndPos.Text);
                    except
                        MessageDlg('Invalid input...', mtError, [mbOk], 0);
                        ActiveControl := InputEndPos;
                        Result := False; Exit
                    end;
                end;
            end;
        Result := True;
    end;

begin
    if Assigned(TC) then
    begin
        Result := False;
        if CurModified then
        begin
            if TC is TGeneralClass then Result := GetGeneral(TC);
            if TC is TCommentsClass then Result := GetComments(TC);
            if TC is TDirectUnit then Result := GetDirectUnit(TC);
            if TC is TReverseUnit then Result := GetReverseUnit(TC);
        end else Result := True;
    end else Result := True;
end;

procedure TForm1.SourceFileContentsChanging(Sender: TObject;
    Node: TTreeNode; var AllowChange: Boolean);
var TC, TempComp: TComponent;
    TempNode: TTreeNode;
begin
    if FileLoading then Exit;

    TC := GetSelectedData;
    AllowChange := GetLastEditedData(TC);
    if AllowChange and Assigned(TC) then
    begin
        if TC is TSinTCompList then
            with TC as TSinTCompList do
            begin GridRelease(Table); Exit; end;

        if TC is TSiteList then
        begin
            TempNode := Node.Parent;
            if TempNode <> nil then
            begin
                TempComp := TComponent(TempNode.Data);
                if TempComp <> nil then
                begin
                    if TempComp is TReverseSourceData then
                    begin
                        Exit;
                    end;

                    if TempComp is TDirectSourceData then
                    begin
                        Exit;
                    end;

                    if TempComp is TReverseCalcData then
                    begin
                        ReleaseAtoms;
                        ReleaseBasisFunctions;
                        Exit;
                    end;
                end;
            end;
            Exit;
        end;

        if TC is TNeutronCompList then
        begin
            Table.SetGridDataSource(nil);
            Exit;
        end;

        if TC is TSinTDataPointer then
        begin
            PanelPlotter.PopupMenu := nil;
            ToolPlotterCopy.OnClick := nil;
            MenuPlotPatternView.Visible := False;
            MenuPlotPatternEdit.Visible := False;
            Exit;
        end;

        if TC is TSiteListPlotParams then
        begin
            PanelPlotter.PopupMenu := nil;
            ToolPlotterCopy.OnClick := nil;
            MenuPlotStructEdit.Visible := False;
            Exit;
        end;
    end;
end;

procedure TForm1.SourceFileContentsChange(Sender: TObject;
    Node: TTreeNode);

    procedure ViewSitesList(TC: TObject);

        procedure SetButtonState(const AState: Boolean);
        var i: LongInt;
        begin
            with ToolBarSites do
                for i := 0 to ButtonCount - 1 do Buttons[i].Enabled := AState;

            with ToolBarStars do
                for i := 0 to ButtonCount - 1 do Buttons[i].Enabled := AState;

            with ToolBarRepr do
                for i := 0 to ButtonCount - 1 do Buttons[i].Enabled := AState;

            with ToolBarAtoms do
                for i := 0 to ButtonCount - 1 do
                    if (Buttons[i] <> ToolAtomsHelp) and
                       (Buttons[i] <> ToolAtomsCopy) then
                            Buttons[i].Enabled := AState;

            with ToolBarBF do
                for i := 0 to ButtonCount - 1 do
                    if (Buttons[i] <> ToolBFHelp) and
                       (Buttons[i] <> ToolBFCopy) then
                            Buttons[i].Enabled := AState;
        end;

    var TempNode: TTreeNode;
        Comp: TComponent;
    begin
        TempNode := Node.Parent;
        if TempNode <> nil then
        begin
            Comp := TComponent(TempNode.Data);
            if Comp <> nil then
            begin
                if Comp is TReverseSourceData then
                begin
                    PanelSites.BringToFront;
                    FillSites;
                    FillAtoms;
                    FillPropVectors;
                    FillRepresentations;
                    FillBasisFunctions;

                    PanelSitesRepr.Enabled := True;
                    PanelSitesBF.Enabled := True;
                    Representations.Color := clWindow;

                    SetButtonState(True);
                    Exit;
                end;

                if Comp is TDirectSourceData then
                begin
                    PanelSites.BringToFront;
                    FillSites;
                    FillAtoms;
                    FillPropVectors;
                    FillRepresentations;
                    FillBasisFunctions;

                    PanelSitesRepr.Enabled := False;
                    PanelSitesBF.Enabled := False;
                    Representations.Color := clBtnFace;

                    SetButtonState(True);
                    Exit;
                end;

                if Comp is TReverseCalcData then
                begin
                    PanelSites.BringToFront;
                    FillSites;
                    FillPropVectors;
                    FillRepresentations;

                    AssignAtoms;
                    AssignBasisFunctions;

                    PanelSitesRepr.Enabled := True;
                    PanelSitesBF.Enabled := True;
                    Representations.Color := clWindow;

                    SetButtonState(False);
                    Exit;
                end;
            end;
        end;
    end;

    procedure ViewTable(TC: TComponent);
    begin
        if TC is TNeutronCompList then
        begin
            PanelTable.BringToFront;
            Table.SetGridDataSource(TNeutronCompList(TC));
            ToolTableDelete.Enabled := True;
            ToolTablePaste.Enabled := True;
        end else
        begin
            PanelTable.BringToFront;
            TTableCompList(TC).GridAssign(Table);
            ToolTableDelete.Enabled := False;
            ToolTablePaste.Enabled := False;
        end;
    end;

    procedure ViewBack(St: string);
    begin
        PanelBack.BringToFront;
        StaticMsg.Caption := St;
    end;

    procedure ViewReverseCalcSettings(TC: TComponent);
    begin
        NeutronDataSelector.Items.Clear;
        NeutronDataSelector.Text := '';
        NeutronDataSelector.Color := clBtnFace;
        GroupDirectSettings.Enabled := False;
        CheckDebyeWaller.Enabled := True;
        CheckIntensNorm.Enabled := True;

        if not RFactorSourceSelector.Enabled then
        begin
            RFactorSourceSelector.Enabled := True;
            RFactorSourceSelector.ItemIndex := 0;
        end;

        PanelCalcOptions.BringToFront;
        FillSitesCalcOpt;
        FillPropVectorsCalcOpt;

        with TC as TReverseUnit do
            with SourceData.GeneralData do
            begin
                InputStartPos.Text := FloatToStrF(StartPos, ffGeneral, 6, 4);
                InputEndPos.Text := FloatToStrF(EndPos, ffGeneral, 6, 4);
                CheckUseStartEnd.Checked := UseStartEnd;
                if CheckUseStartEnd.Checked then
                begin
                    InputStartPos.Enabled := True;
                    InputEndPos.Enabled := True
                end else
                begin
                    InputStartPos.Enabled := False;
                    InputEndPos.Enabled := False
                end;
            end;
    end;

    procedure ViewDirectCalcSettings(TC: TComponent);
    begin
        NeutronDataSelector.Items.Clear;
        NeutronDataSelector.Items.AddStrings(GetNeutronDataCollection(SourceFile));
        NeutronDataSelector.Items.InsertObject(0, 'No selected neutron data', nil);
        NeutronDataSelector.ItemIndex := 0;
        RFactorSourceSelector.ItemIndex := -1;
        RFactorSourceSelector.Enabled := False;

        GroupDirectSettings.Enabled := True;
        NeutronDataSelector.Color := clWindow;
        CheckDebyeWaller.Enabled := False;
        CheckDebyeWaller.Checked := False;
        CheckIntensNorm.Enabled := False;
        CheckIntensNorm.Checked := False;

        PanelCalcOptions.BringToFront;
        FillSitesCalcOpt;
        FillPropVectorsCalcOpt;

        with TC as TDirectUnit do
            with SourceData.GeneralData do
            begin
                InputStartPos.Text := FloatToStrF(StartPos, ffGeneral, 6, 4);
                InputEndPos.Text := FloatToStrF(EndPos, ffGeneral, 6, 4);
                CheckUseStartEnd.Checked := UseStartEnd;
                if CheckUseStartEnd.Checked then
                begin
                    InputStartPos.Enabled := True;
                    InputEndPos.Enabled := True
                end else
                begin
                    InputStartPos.Enabled := False;
                    InputEndPos.Enabled := False
                end;
            end;
    end;

    procedure ViewComments(TC: TComponent);
    var i: LongInt;
    begin
        with TC as TCommentsClass, Comments do
        begin
            Tag := 1;
            
            Lines.Clear;
            for i := 0 to List.Count - 1 do Lines.Add(List.strings[i]);
            Modified := False;
            PanelComments.BringToFront;
            Tag := 0;
        end;
    end;

    procedure ViewGeneral(TC: TComponent);
    begin
        with TC as TGeneralClass do
        begin
            InputA.Text := FloatToStrF(A, ffGeneral, 6, 4);
            InputB.Text := FloatToStrF(B, ffGeneral, 6, 4);
            InputC.Text := FloatToStrF(C, ffGeneral, 6, 4);
            InputAlpha.Text := FloatToStrF(Alpha, ffGeneral, 6, 4);
            InputBeta.Text := FloatToStrF(Beta, ffGeneral, 6, 4);
            InputGamma.Text := FloatToStrF(Gamma, ffGeneral, 6, 4);
            InputLambda.Text := FloatToStrF(Lambda, ffGeneral, 6, 4);
        end;
        PanelGeneralData.BringToFront;
    end;

    procedure ViewStructDataAs3D(TC: TObject);
    begin
        with TC as TSiteListPlotParams do
        begin
            Plotter3D.SetSiteList(GetSiteList);
            Plotter3D.Plot;
        end;
        PanelPlotter.BringToFront;

        ToolPlotterRotX.Enabled := True;
        ToolPlotterRotY.Enabled := True;
        ToolPlotterRotZ.Enabled := True;
        ToolPlotterReset.Enabled := True;
        ToolPlotterZoomIn.Enabled := True;
        ToolPlotterZoomOut.Enabled := True;
        ToolPlotterProperties.Enabled := True;

        MenuPlotStructEdit.Visible := True;
        ToolPlotterCopy.OnClick := MenuPlotStructCopyClick;
        PanelPlotter.PopupMenu := PopupPlotStruct;
    end;

    procedure ViewSinTDataAsBarGraph(TC: TObject);
    begin
        with TC as TSinTDataPointer do
            with SinTPointer do
                PlotDiagr(PlotMode, TempBitMap, Plotter, SinTPointer);
        PanelPlotter.BringToFront;
        PlotDiagrMode := TSinTDataPointer(TC).PlotMode;

        ToolPlotterRotX.Enabled := False;
        ToolPlotterRotY.Enabled := False;
        ToolPlotterRotZ.Enabled := False;
        ToolPlotterReset.Enabled := False;
        ToolPlotterZoomIn.Enabled := False;
        ToolPlotterZoomOut.Enabled := False;
        ToolPlotterProperties.Enabled := False;

        MenuPlotPatternView.Visible := True;
        MenuPlotPatternEdit.Visible := True;
        PanelPlotter.PopupMenu := PopupPlotPattern;
        ToolPlotterCopy.OnClick := MenuPlotPatternCopyClick;
    end;

var TC: TComponent;
begin
    if TreeUpdatingAllowed then
    begin
        CurModified := False;

        TC := GetSelectedData;
        if Assigned(TC) then
        begin
            if TC is TTableCompList then begin ViewTable(TC); Exit end;
            if TC is TGeneralClass then begin ViewGeneral(TC); Exit end;
            if TC is TCommentsClass then begin ViewComments(TC); Exit end;
            if TC is TSiteList then begin ViewSitesList(TC); Exit end;
            if TC is TReverseUnit then begin ViewReverseCalcSettings(TC); Exit end;
            if TC is TDirectUnit then begin ViewDirectCalcSettings(TC); Exit end;
            if TC is TSiteListPlotParams then begin ViewStructDataAs3D(TC); Exit; end;
            if TC is TSinTDataPointer then begin ViewSinTDataAsBarGraph(TC); Exit; end;
            ViewBack('Select necessary data unit in the left box');
        end //  if Assigned(TC) then...
        else ViewBack('Select necessary data unit in the left box');
    end;
end;

procedure TForm1.TableEnter(Sender: TObject);
var TC: TComponent;
begin
    TC := GetSelectedData;

    if TC <> nil then
        with Sender as TIDA_Grid do
        begin
            if TC is TSinTCompList then
            begin
                MenuResultsEdit.Visible := True;
                PopupMenu := PopupResults;

                ToolTableCopy.OnClick := MenuResultsCopyClick;
                ToolTablePaste.OnClick := nil;
                ToolTableDelete.OnClick := nil;
            end;
            if TC is TNeutronCompList then
            begin
                MenuNeutronEdit.Visible := True;
                PopupMenu := PopupNeutron;

                ToolTableCopy.OnClick := MenuNeutronCopyClick;
                ToolTablePaste.OnClick := MenuNeutronPasteClick;
                ToolTableDelete.OnClick := MenuNeutronDeleteClick;
            end;
        end;
end;

procedure TForm1.TableExit(Sender: TObject);
begin
    MenuResultsEdit.Visible := False;
    MenuNeutronEdit.Visible := False;
end;

function TForm1.GetSelectedData: TComponent;
begin
    Result := nil;
    if not Assigned(SourceFileContents.Selected) then Exit
    else Result := SourceFileContents.Selected.Data;
end;

procedure TForm1.SourceFileContentsKeyPress(Sender: TObject;
    var Key: Char);
begin
    if Key = ' ' then Key := '_';
end;

procedure TForm1.SourceFileContentsEditing(Sender: TObject;
    Node: TTreeNode; var AllowEdit: Boolean);
var TC: TComponent;
begin
    AllowEdit := False;
    TC := GetSelectedData;
    if Assigned(TC) then
    begin
        if TC is TReverseUnit then AllowEdit := True;
        if TC is TDirectUnit then AllowEdit := True;
        if TC is TSourceFile then AllowEdit := True;
    end;
end;

procedure TForm1.SourceFileContentsEdited(Sender: TObject; Node: TTreeNode;
    var S: string);
var TC: TComponent;
begin
    TC := GetSelectedData;
    if Assigned(TC) then
    begin
        if TC is TReverseUnit then
            with TC as TReverseUnit do
            begin Caption := S; Saved := False end;
        if TC is TDirectUnit then
            with TC as TDirectUnit do
            begin Caption := S; Saved := False end;
        if TC is TSourceFile then
            with TC as TSourceFile do
            begin Caption := S; Saved := False end;
    end;
end;

procedure TForm1.SourceFileContentsEnter(Sender: TObject);
begin
    MenuProjectEdit.Visible := True;
    MenuProjectView.Visible := True;
end;

procedure TForm1.SourceFileContentsExit(Sender: TObject);
begin
    MenuProjectEdit.Visible := False;
    MenuProjectView.Visible := False;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word;
    Shift: TShiftState);
var TC: TComponent;
const
    ArrowsCodes = [VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN];
begin
    TC := GetSelectedData;
    if Assigned(TC) then
        if TC is TSiteListPlotParams then
        begin
          if (ssCtrl in Shift) and (Key in ArrowsCodes) then
          begin
              case Key of
                  VK_UP : RotationX2(Rad(-5));
                  VK_DOWN : RotationX2(Rad(5));
                  VK_LEFT : RotationY2(Rad(-5));
                  VK_RIGHT : RotationY2(Rad(5));
              end;
              Plotter3D.Plot;
              Key := 0;
            end;    //  if (ssShift in Shift) and (Key in UpDownCodes) then...
        end;
end;

procedure TForm1.SetModifiedState;
begin
    FCurModified := AModifiedState;
    if AModifiedState then Saved := False;
end;

function TForm1.GetNeutronDataCollection(ASourceFile: TSourceFile): Tstrings;
var TempStrings: TStringList;
    i: LongInt;
    TC: TComponent;
begin
    TempStrings := TStringList.Create;
    with ASourceFile do
        for i := 0 to Count - 1 do
        begin
            TC := Items[i];
            if TC is TReverseUnit then with TC as TReverseUnit do
                TempStrings.AddObject(Caption, TC);
        end;
    Result := TempStrings;
end;

procedure TForm1.MenuPlotPatternNeutronClick(Sender: TObject);
var TC: TComponent;
begin
    TC := GetSelectedData;
    if Assigned(TC) then
        if TC is TSinTDataPointer then
            with TC as TSinTDataPointer do
            begin
                PlotMode := PlotMode xor PM_EXP_INTENSITY;
                PlotDiagrMode := PlotMode;
                with SinTPointer do
                    PlotDiagr(PlotMode, TempBitMap,
                        Plotter, SinTPointer);
            end;
end;

procedure TForm1.MenuPlotPatternNuclClick(Sender: TObject);
var TC: TComponent;
begin
    TC := GetSelectedData;
    if Assigned(TC) then
        if TC is TSinTDataPointer then
            with TC as TSinTDataPointer do
            begin
                PlotMode := PlotMode xor PM_NUCLEAR_INTENSITY;
                PlotDiagrMode := PlotMode;
                with SinTPointer do
                    PlotDiagr(PlotMode, TempBitMap,
                        Plotter, SinTPointer);
            end;
end;

procedure TForm1.MenuPlotPatternMagnClick(Sender: TObject);
var TC: TComponent;
begin
    TC := GetSelectedData;
    if Assigned(TC) then
        if TC is TSinTDataPointer then
            with TC as TSinTDataPointer do
            begin
                PlotMode := PlotMode xor PM_INTENSITY;
                PlotDiagrMode := PlotMode;
                with SinTPointer do
                    PlotDiagr(PlotMode, TempBitMap,
                        Plotter, SinTPointer);
            end;
end;

procedure TForm1.MenuPlotPatternStructFactClick(Sender: TObject);
var TC: TComponent;
begin
    TC := GetSelectedData;
    if Assigned(TC) then
        if TC is TSinTDataPointer then
            with TC as TSinTDataPointer do
            begin
                PlotMode := PlotMode xor PM_STRUCT_FACT;
                PlotDiagrMode := PlotMode;
                with SinTPointer do
                    PlotDiagr(PlotMode, TempBitMap, Plotter, SinTPointer);
            end;
end;

procedure TForm1.LoadImages;
var NewIcon: TIcon;
begin
    SourceFileContents.Images := TImageList.Create(Self);
    NewIcon := TIcon.Create;
    NewIcon.Handle := LoadIcon(HInstance, 'FileIcon');
    SourceFileContents.Images.AddIcon(NewIcon);

    NewIcon.Handle := LoadIcon(HInstance, 'TableIcon');
    SourceFileContents.Images.AddIcon(NewIcon);

    NewIcon.Handle := LoadIcon(HInstance, 'PlotIn3DIcon');
    SourceFileContents.Images.AddIcon(NewIcon);

    NewIcon.Handle := LoadIcon(HInstance, 'CommentsIcon');
    SourceFileContents.Images.AddIcon(NewIcon);

    NewIcon.Handle := LoadIcon(HInstance, 'BarGraphIcon');
    SourceFileContents.Images.AddIcon(NewIcon);

    NewIcon.Handle := LoadIcon(HInstance, 'UnitOpenIcon');
    SourceFileContents.Images.AddIcon(NewIcon);

    NewIcon.Handle := LoadIcon(HInstance, 'UnitCloseIcon');
    SourceFileContents.Images.AddIcon(NewIcon);

    NewIcon.Handle := LoadIcon(HInstance, 'GeneralIcon');
    SourceFileContents.Images.AddIcon(NewIcon);
    UtilizeObject(NewIcon);
end;

procedure TForm1.Paint(var Msg: TMessage);
begin
    if Msg.Msg = WM_PAINT then
        if WindowState = wsMinimized then Hide
        else inherited;
end;

procedure TForm1.MenuFileNewPolyClick(Sender: TObject);
var DateTime: TDateTime;
begin
    if Assigned(SourceFile) then
    begin
        DateTime := Date;
        SourceFile.CreateReverseUnit(
            'POLYCRYSTAL (' + DateToStr(DateTime) + ')', NDT_POLYCRYSTAL);
        ShowFileInTree;
        LinkPlotterParamsWithSiteList;
        Saved := False;
    end;
end;

procedure TForm1.MenuFileNewMonoClick(Sender: TObject);
var DateTime: TDateTime;
begin
    if Assigned(SourceFile) then
    begin
        DateTime := Date;
        SourceFile.CreateReverseUnit(
            'MONOCRYSTAL (' + DateToStr(DateTime) + ')', NDT_MONOCRYSTAL);
        ShowFileInTree;
        LinkPlotterParamsWithSiteList;
        Saved := False;
    end;
end;

procedure TForm1.FillSites;
var TC: TComponent;
begin
    TC := GetSelectedData;
    if Assigned(TC) then
        if TC is TSiteList then
            FillListBoxBySiteList(TSiteList(TC), Sites, LBM_VIEW);
end;

procedure TForm1.FillSitesCalcOpt;
var TC: TComponent;
begin
    TC := GetSelectedData;
    if Assigned(TC) then begin
        if TC is TReverseUnit then
            with TC as TReverseUnit do
                FillListBoxBySiteList(SourceData.StructureData,
                    ListBoxSitesCalcOpt, LBM_CALC);
        if TC is TDirectUnit then
            with TC as TDirectUnit do
                FillListBoxBySiteList(SourceData.ModelData,
                    ListBoxSitesCalcOpt, LBM_CALC);
    end;
end;

function TForm1.GetCurSite: TSite;
begin
    if Sites.Items.Count <> 0 then
        Result := TSite(Sites.Items.Objects[Sites.ItemIndex])
    else Result := nil;
end;

function TForm1.GetCurRepr: TRepresentation;
begin
    with Representations.Items do
        if Count <> 0 then
            Result := TRepresentation(Objects[Representations.ItemIndex])
        else Result := nil;
end;

procedure TForm1.FillAtoms;
var TS: TSite;
begin
    TS := GetCurSite;
    if TS <> nil then
        Atoms.SetGridDataSource(TS.AtomList)
    else Atoms.SetGridDataSource(nil);
end;

procedure TForm1.FillBasisFunctions;
var TR: TRepresentation;
begin
    TR := GetCurRepr;
    if TR <> nil then
        BasisFunctions.SetGridDataSource(TR)
    else BasisFunctions.SetGridDataSource(nil);
end;

procedure TForm1.AssignAtoms;
var TS: TSite;
begin
    TS := GetCurSite;
    if TS <> nil then
        TS.AtomList.GridAssign(Atoms)
    else Atoms.SetGridDataSource(nil);
end;

procedure TForm1.AssignBasisFunctions;
var TR: TRepresentation;
begin
    TR := GetCurRepr;
    if TR <> nil then
        TR.GridAssign(BasisFunctions)
    else BasisFunctions.SetGridDataSource(nil);
end;

procedure TForm1.ReleaseAtoms;
var TS: TSite;
begin
    TS := GetCurSite;
    if TS <> nil then
        TS.AtomList.GridRelease(Atoms);
end;

procedure TForm1.ReleaseBasisFunctions;
var TR: TRepresentation;
begin
    TR := GetCurRepr;
    if TR <> nil then
        TR.GridRelease(BasisFunctions);
end;

procedure TForm1.SetAtoms;
var TS: TSite;
begin
    TS := GetCurSite;
    if TS <> nil then
        TS.AtomList.SetDataToGrid(Atoms);
end;

procedure TForm1.SetBasisFunctions;
var TR: TRepresentation;
begin
    TR := GetCurRepr;
    if TR <> nil then
        TR.SetDataToGrid(BasisFunctions);
end;

procedure TForm1.FillPropVectors;
var TS: TSite;
begin
    if Sites.Items.Count <> 0 then
        with Sites do
        begin
            TS := TSite(Sites.Items.Objects[ItemIndex]);
            if Assigned(TS) then
                FillListBoxByPropVectorsList(
                    TS.WaveVectorList, WaveVectors, LBM_VIEW);
        end
    else WaveVectors.Items.Clear;
end;

procedure TForm1.FillPropVectorsCalcOpt;
var TS: TSite;
begin
    if ListBoxSitesCalcOpt.Items.Count <> 0 then
        with ListBoxSitesCalcOpt do
        begin
            TS := TSite(ListBoxSitesCalcOpt.Items.Objects[ItemIndex]);
            if Assigned(TS) then
                FillListBoxByPropVectorsList(
                    TS.WaveVectorList, ListBoxPropVectCalcOpt, LBM_CALC);
        end
    else ListBoxPropVectCalcOpt.Items.Clear;
end;

procedure TForm1.FillListBoxByPropVectorsList(
    const PropVectorsList: TWaveVectorList; ListBox: TListBox; Mode: LongInt);
var i: LongInt;
    St: string;
    TW: TWaveVector;
begin
    ListBox.Items.Clear;
    for i := 0 to PropVectorsList.Count - 1 do
    begin
        St := PropVectorsList.GetObjectIdentifier(i);
        TW := TWaveVector(PropVectorsList.Items[i]);
        case Mode of
            LBM_CALC : begin
                if TW.IsPropVectMagnetic then
                    St := St + '  M' else St := St + ' NM';
                if TW.IsPropVectStructural then
                    St := St + '  S' else St := St + ' NS';
                if TW.AllowedTransTypesNumber <> 0 then
                begin
                    case TW.AllowedTransTypes[TW.TransTypeIndex] of
                        TT_SS : St := St + '   SS/ FS';
                        TT_LSW : St := St + '  LSW/TSW';
                        TT_CS : St := St + '   CS';
                        TT_ES : St := St + '   ES';
                    end;
                end else St := St + '   DISABLED';
                case TW.RotAxisType of
                    RA_NONE : St := St + '  NONE';
                    RA_X : St := St + '  X';
                    RA_Y : St := St + '  Y';
                    RA_Z : St := St + '  Z';
                    RA_OTHER : St := St + '  OTHER';
                end;
            end;

            LBM_VIEW : begin
            end;
        end;
        ListBox.Items.AddObject(St, TW);
    end;
end;

procedure TForm1.FillListBoxBySiteList(
    const SiteList: TSiteList; ListBox: TListBox; Mode: LongInt);
var i: LongInt;
    St: string;
    TS: TSite;
begin
    ListBox.Items.Clear;
    for i := 0 to SiteList.Count - 1 do
    begin
        St := SiteList.GetObjectIdentifier(i);
        TS := TSite(SiteList.Items[i]);
        case Mode of
            LBM_CALC : with TS do begin
                if Disabled then St := St + '  D' else St := St + '  E';
                if NotMagnetic then St := St + '  NM' else St := St + '   M';
                case VariationMode of
                    VM_ALL_COMPONENTS : St := St + '  VAMC';
                    VM_EQUAL_MODULES : St := St + '  SEMS';
                    VM_ONLY_MODULES : St := St + '  OMV';
                end;
            end;

            LBM_VIEW : begin
            end;
        end;
        ListBox.Items.AddObject(St, TS);
    end;
end;

procedure TForm1.FillRepresentations;
var WV: TWaveVector;
    i: LongInt;
begin
    Representations.Items.Clear;
    if WaveVectors.Items.Count <> 0 then
    begin
        WV := TWaveVector(WaveVectors.Items.Objects[WaveVectors.ItemIndex]);
        if Assigned(WV) then
            with TRepresentationList(WV.Representations) do
                for i := 0 to Count - 1 do
                    Representations.Items.AddObject(GetObjectIdentifier(i), Items[i]);

    end;
end;

procedure TForm1.SitesKeyDown(Sender: TObject; var Key: Word;
    Shift: TShiftState);
begin
    if Key in UpDownCodes then SitesChanged;
end;

procedure TForm1.SitesChanged;
begin
    FillAtoms;
    FillPropVectors;
    FillRepresentations;
    FillBasisFunctions;
end;

procedure TForm1.SitesClick(Sender: TObject);
begin
    SitesChanged;
end;

procedure TForm1.WaveVectorsKeyDown(Sender: TObject; var Key: Word;
    Shift: TShiftState);
begin
    if Key in UpDownCodes then PropVectorsChanged;
end;

procedure TForm1.RepresentationsKeyDown(Sender: TObject; var Key: Word;
    Shift: TShiftState);
begin
    if Key in UpDownCodes then RepresentationsChanged;
end;

procedure TForm1.PropVectorsChanged;
begin
    FillRepresentations;
    FillBasisFunctions;
end;

procedure TForm1.SitesChangedCalcOpt;
begin
    FillPropVectorsCalcOpt;
end;

procedure TForm1.PropVectorsChangedCalcOpt;
begin
end;

procedure TForm1.WaveVectorsClick(Sender: TObject);
begin
    PropVectorsChanged;
end;

procedure TForm1.RepresentationsChanged;
begin
    FillBasisFunctions;
end;

procedure TForm1.RepresentationsClick(Sender: TObject);
begin
    RepresentationsChanged;
end;

procedure TForm1.AtomsExit(Sender: TObject);
begin
    MenuAtomsEdit.Visible := False;
end;

procedure TForm1.BasisFunctionsExit(Sender: TObject);
begin
    MenuBFEdit.Visible := False;
end;

procedure TForm1.InputAKeyPress(Sender: TObject; var Key: Char);
begin
    if (Key in POS_REAL_SET) and (Sender is TEdit) then
        CurModified := True else Key := #0;
end;

procedure TForm1.EditWVButClick(Sender: TObject);
var TW: TWaveVector;
    TS: TSite;
    SaveIndex: LongInt;
begin
    if ListBoxPropVectCalcOpt.Items.Count <> 0 then
    begin
        SaveIndex := ListBoxPropVectCalcOpt.ItemIndex;

        with ListBoxPropVectCalcOpt do
            TW := TWaveVector(Items.Objects[ItemIndex]);

        with ListBoxSitesCalcOpt do
            TS := TSite(Items.Objects[ItemIndex]);

        PropVectCalcOptDlg.EditedPropVector := TW;
        PropVectCalcOptDlg.Site := TS;
        if PropVectCalcOptDlg.ShowModal = mrOk then
        begin
            ListBoxPropVectCalcOpt.ItemIndex := SaveIndex;
            FillPropVectorsCalcOpt;
            Saved := False;
        end;
    end;
end;

procedure TForm1.NeutronDataSelectorChange(Sender: TObject);
begin
    with NeutronDataSelector do
    begin
        if Assigned(Items.Objects[ItemIndex]) then
        begin
            RFactorSourceSelector.ItemIndex := 0;
            RFactorSourceSelector.Enabled := True;
        end
        else
        begin
            RFactorSourceSelector.ItemIndex := -1;
            RFactorSourceSelector.Enabled := False;
        end;
    end;
end;

procedure TForm1.CommentsChange(Sender: TObject);
begin
    with Comments do
        if Modified and (Tag = 0) then CurModified := True;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var DlgResult: Word;
    St: string;
begin
    if Assigned(SourceFile) and (not Saved) then
    begin
        St := ExtractFileName(SourceFile.FileName);
        DlgResult := MessageDlg('File ' + St + ' has been modified...Save ?',
        mtConfirmation, [mbYes, mbNo, mbCancel], 0);
        case DlgResult Of
            mrYes : begin
                case SourceFile.Mode of
                    sfCreate : Save(True);
                    sfOpened : Save(False);
                end;
                CanClose := True;
            end;
            mrNo : CanClose := True;
            mrCancel : CanClose := False;
        end;
    end;
end;

procedure TForm1.ShowHint(Sender: TObject);
begin
    StatusBar1.Panels[2].Text := Application.Hint;
end;

procedure TForm1.UpdateFFContents;
begin
    OpenFFDataFile(FFPath + FORM_FACTORS_FILE_NAME);
    SourceFileContentsChange(nil, nil);
end;

procedure TForm1.MessagesHandler(var Msg: TMsg; var Handled: Boolean);
begin
    if Msg.Message = WM_FFEditor then
    begin
        UpdateFFContents;
        Handled := True;
    end;
end;

procedure TForm1.CheckUseStartEndClick(Sender: TObject);
begin
    if CheckUseStartEnd.Checked then
    begin InputStartPos.Enabled := True; InputEndPos.Enabled := True end
    else begin InputStartPos.Enabled := False; InputEndPos.Enabled := False end;
    CurModified := True;
end;

const PROGRESS_BAR_HINT = 'Current Job Progress';

procedure TForm1.ShowCurJobProgress(Sender: TComponent;
    MinValue, MaxValue, CurValue: LongInt);
begin
    if not (csDestroying in ComponentState) then
    begin
        CurJobProgress.Max := MaxValue;
        CurJobProgress.Min := MinValue;
        CurJobProgress.Position := CurValue;
        CurJobProgress.Hint := PROGRESS_BAR_HINT + ' (' +
            IntToStr(CurValue) + ' of ' + IntToStr(MaxValue) + ')';
    end;
end;

procedure TForm1.ResetCurJobProgress(Sender: TComponent);
begin
    if not (csDestroying in ComponentState) then
    begin
        CurJobProgress.Max := 0;
        CurJobProgress.Min := 0;
        CurJobProgress.Position := 0;
        CurJobProgress.Hint := PROGRESS_BAR_HINT + '(0 of 0)'
    end;
end;

procedure TForm1.ShowMessage(Sender: TComponent; Msg: string);
begin
    if not (csDestroying in ComponentState) then  CalcComments.Lines.Add(Msg);
end;

procedure TForm1.UpdatingResults(Sender: TComponent);
begin
    if not (csDestroying in ComponentState) then StandardViewAction;
end;

procedure TForm1.TableMouseDown(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
var SR: TSinTClass;
    Number, Index: LongInt;
    St: string;
    TC: TComponent;
    Coord: TGridCoord;
begin
    TC := GetSelectedData;
    if Assigned(TC) then
    begin
        if TC is TSinTCompList then
            with TC as TSinTCompList do
            begin
                with Table do
                begin
                    if Shift = [ssLeft, ssDouble] then
                    begin
                        Coord := MouseCoord(X, Y);
                        if Coord.X <= FixedCols - 1 then
                        begin
                            St := Cells[Coord.X, Coord.Y];

                            Index := GetCharPosition(St, ' ', 1, 1);
                            if Index <> -1 then
                            begin
                                St := Copy(St, 1, Index - 1);
                                Number := StrToInt(St);

                                GridRelease(Table); //  !!!

                                SR := TSinTClass(Items[Number - 1]);
                                ToggleExpanded(IndexOf(SR));

                                GridAssign(Table);
                            end;
                        end;    //  if Coord.X <= FixedCols - 1 then...
                    end;    //  if Shift = [ssLeft, ssDouble] then...
                end;    //  with Table do...
            end;    //  with TC as TSinTCompList do...
    end;    //  if Assigned(Selected) do...
end;

procedure TForm1.ToolButSitesCalcOptClick(Sender: TObject);
var TS: TSite;
    SaveIndex: LongInt;
begin
    with ListBoxSitesCalcOpt do
        if Items.Count <> 0 then
        begin
            SaveIndex := ItemIndex;
            TS := TSite(Items.Objects[ItemIndex]);
            SiteCalcOptDlg.EditedSite := TS;
            if SiteCalcOptDlg.ShowModal = mrOk then
            begin
                FillSitesCalcOpt;
                ItemIndex := SaveIndex;
                Saved := False;
            end;
        end;
end;

procedure TForm1.ListBoxSitesCalcOptClick(Sender: TObject);
begin
    SitesChangedCalcOpt;
end;

procedure TForm1.ListBoxSitesCalcOptKeyDown(Sender: TObject; var Key: Word;
    Shift: TShiftState);
begin
    if Key in UpDownCodes then SitesChangedCalcOpt;
end;

procedure TForm1.ToolPlotterPropertiesClick(Sender: TObject);
var TC: TComponent;
begin
    TC := GetSelectedData;
    if TC is TSiteListPlotParams then
    begin
        with TC as TSiteListPlotParams do
            StructViewPropDlg.EditedSiteList := GetSiteList;

        StructViewPropDlg.OnUpdate := UpdateStructViewProperties;
        StructViewPropDlg.ShowModal;
    end;
end;

procedure TForm1.UpdateStructViewProperties(Sender: TObject);
begin
    PlotterPaint(Sender);
    Saved := False;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
    if not (csDestroying in ComponentState) then
    begin
        Plotter3D.SetHeight(Plotter.Height);
        Plotter3D.SetWidth(Plotter.Width);
    end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
    UtilizeObject(CalcModule);
    UtilizeObject(CalcModuleDirect);

    UtilizeObject(Plotter3D);

    UtilizeObject(TempBitMap);
    UtilizeObject(RecFile);
    UtilizeObject(SourceFile);
    CloseFFDataFile;
    Application.HelpCommand(Help_Quit, 0);
end;

procedure TForm1.LinkPlotterParamsWithSiteList;
var i: LongInt;
    TC, TC2: TComponent;
begin
    for i := 0 to SourceFile.Count - 1 do
    begin
        TC := SourceFile.Items[i];

        if TC is TReverseUnit then
        begin
            TC2 := TReverseUnit(TC).SourceData.StructureData;
            if TC2 is TSiteList then
                with TC2 as TSiteList do
                    TSiteListPlotParams(GetPlotParams).SetSiteList(TSiteList(TC2));

            TC2 := TReverseUnit(TC).CalcData.CalcStructureData;
            if TC2 is TSiteList then
                with TC2 as TSiteList do
                    TSiteListPlotParams(GetPlotParams).SetSiteList(TSiteList(TC2));
            Continue;
        end;

        if TC is TDirectUnit then
        begin
            TC2 := TDirectUnit(TC).SourceData.ModelData;
            if TC2 is TSiteList then
                with TC2 as TSiteList do
                    TSiteListPlotParams(GetPlotParams).SetSiteList(TSiteList(TC2));
            Continue;
        end;
    end;
end;

procedure TForm1.MenuCheckDataValidityClick(Sender: TObject);
var TC: TComponent;
begin
    TC := GetSelectedData;
    if Assigned(TC) then
    begin
        if TC is TReverseUnit then
            with TC as TReverseUnit do begin
                IsReady;
                MessageDlg('All data are valid...', mtInformation, [mbOk], 0);
            end;

        if TC is TDirectUnit then
            with TC as TDirectUnit do begin
                IsReady;
                MessageDlg('All data is valid', mtInformation, [mbOk], 0);
            end;
    end;
end;

procedure TForm1.ShowFileInTree;
begin
    TreeUpdatingAllowed := False;
    SourceFile.ViewSourceFileContents;
    TreeUpdatingAllowed := True;
end;

procedure TForm1.ToolAtomsHelpClick(Sender: TObject);
begin
    Application.HelpJump('hlp_AtomsParInput');
end;

procedure TForm1.ToolButton2Click(Sender: TObject);
var TC: TComponent;
begin
    TC := GetSelectedData;
    if Assigned(TC) then
    begin
        if TC is TNeutronCompList then
            Application.HelpJump('hlp_NeutronDataInput');
        if TC is TSinTCompList then
            Application.HelpJump('hlp_CalcResOutput');
    end;
end;

procedure TForm1.ToolBFHelpClick(Sender: TObject);
begin
    Application.HelpJump('hlp_BasalFunctionsInput');
end;

procedure TForm1.ToolButton11Click(Sender: TObject);
begin
    Application.HelpJump('hlp_PanelCalcOptions');
end;

procedure TForm1.ToolButton15Click(Sender: TObject);
begin
    Application.HelpJump('hlp_SourceFile');
end;

procedure TForm1.MenuProjectDeleteUnitClick(Sender: TObject);
var TC: TComponent;
begin
    TC := GetSelectedData;
    if Assigned(TC) then
    begin
        if (TC is TReverseUnit) or
           (TC is TDirectUnit) then
        begin
            if MessageDlg('Do you really want to delete this unit ?', mtWarning,
            [mbYes, mbNo, mbCancel], 0) <> mrYes then Exit;
            SourceFile.Remove(TC);
            ShowFileInTree;
            LinkPlotterParamsWithSiteList;
            Saved := False;
        end
        else MessageDlg('This item can''t be deleted...', mtWarning, [mbOk], 0);
    end;    //  if Assigned(TC) then...
end;

procedure TForm1.MenuProjectRenameUnitClick(Sender: TObject);
begin
    with SourceFileContents do
        if Assigned(Selected) then Selected.EditText
        else MessageDlg('Select item to edit...', mtWarning, [mbOk], 0);
end;

procedure TForm1.SitesEnter(Sender: TObject);
var TempNode: TTreeNode;
    Comp: TComponent;
    i: LongInt;
begin
    MenuSitesEdit.Visible := True;

    TempNode := SourceFileContents.Selected;
    TempNode := TempNode.Parent;
    if TempNode <> nil then
    begin
        Comp := TComponent(TempNode.Data);
        if Comp <> nil then
        begin
            if (Comp is TReverseSourceData) or
               (Comp is TDirectSourceData) then
            begin
                for i := 0 to PopupSites.Items.Count - 1 do
                    PopupSites.Items[i].Enabled := True;
                Exit;
            end;

            if Comp is TReverseCalcData then
            begin
                for i := 0 to PopupSites.Items.Count - 1 do
                    PopupSites.Items[i].Enabled := False;
            end;
        end;
    end;
end;

procedure TForm1.SitesExit(Sender: TObject);
begin
    MenuSitesEdit.Visible := False;
end;

procedure TForm1.WaveVectorsEnter(Sender: TObject);
var TempNode: TTreeNode;
    Comp: TComponent;
    i: LongInt;
begin
    MenuStarsEdit.Visible := True;

    TempNode := SourceFileContents.Selected;
    TempNode := TempNode.Parent;
    if TempNode <> nil then
    begin
        Comp := TComponent(TempNode.Data);
        if Comp <> nil then
        begin
            if (Comp is TReverseSourceData) or
               (Comp is TDirectSourceData) then
            begin
                for i := 0 to PopupStars.Items.Count - 1 do
                    PopupStars.Items[i].Enabled := True;
                Exit;
            end;

            if Comp is TReverseCalcData then
            begin
                for i := 0 to PopupStars.Items.Count - 1 do
                    PopupStars.Items[i].Enabled := False;
            end;
        end;
    end;
end;

procedure TForm1.WaveVectorsExit(Sender: TObject);
begin
    MenuStarsEdit.Visible := False;
end;

procedure TForm1.RepresentationsEnter(Sender: TObject);
var TempNode: TTreeNode;
    Comp: TComponent;
    i: LongInt;
begin
    MenuReprEdit.Visible := True;

    TempNode := SourceFileContents.Selected;
    TempNode := TempNode.Parent;
    if TempNode <> nil then
    begin
        Comp := TComponent(TempNode.Data);
        if Comp <> nil then
        begin
            if Comp is TReverseSourceData then
            begin
                for i := 0 to PopupRepr.Items.Count - 1 do
                    PopupRepr.Items[i].Enabled := True;
                Exit;
            end;

            if (Comp is TReverseCalcData) or
               (Comp is TDirectSourceData) then
            begin
                for i := 0 to PopupRepr.Items.Count - 1 do
                    PopupRepr.Items[i].Enabled := False;
            end;
        end;
    end;
end;

procedure TForm1.RepresentationsExit(Sender: TObject);
begin
    MenuReprEdit.Visible := False;
end;

procedure TForm1.AtomsEnter(Sender: TObject);
var TempNode: TTreeNode;
    Comp: TComponent;
    i: LongInt;
begin
    MenuAtomsEdit.Visible := True;

    TempNode := SourceFileContents.Selected;
    TempNode := TempNode.Parent;
    if TempNode <> nil then
    begin
        Comp := TComponent(TempNode.Data);
        if Comp <> nil then
        begin
            if (Comp is TReverseSourceData) or
               (Comp is TDirectSourceData) then
            begin
                if ClipBoard.HasFormat(CF_TEXT) then
                    PopupAtomsPaste.Enabled := True
                else PopupAtomsPaste.Enabled := False;

                for i := 0 to PopupAtoms.Items.Count - 1 do
                    if PopupAtoms.Items[i] <> PopupAtomsPaste then
                        PopupAtoms.Items[i].Enabled := True;
                Exit;
            end;

            if Comp is TReverseCalcData then
            begin
                PopupAtomsCopy.Enabled := True;

                for i := 0 to PopupAtoms.Items.Count - 1 do
                    if PopupAtoms.Items[i] <> PopupAtomsCopy then
                        PopupAtoms.Items[i].Enabled := False;
            end;
        end;
    end;
end;

procedure TForm1.BasisFunctionsEnter(Sender: TObject);
var TempNode: TTreeNode;
    Comp: TComponent;
    i: LongInt;
begin
    MenuBFEdit.Visible := True;

    TempNode := SourceFileContents.Selected;
    TempNode := TempNode.Parent;
    if TempNode <> nil then
    begin
        Comp := TComponent(TempNode.Data);
        if Comp <> nil then
        begin
            if (Comp is TReverseSourceData) or
               (Comp is TDirectSourceData) then
            begin
                if ClipBoard.HasFormat(CF_TEXT) then
                    PopupBFPaste.Enabled := True
                else PopupBFPaste.Enabled := False;

                for i := 0 to PopupBF.Items.Count - 1 do
                    if PopupBF.Items[i] <> PopupBFPaste then
                        PopupBF.Items[i].Enabled := True;
                Exit;
            end;

            if Comp is TReverseCalcData then
            begin
                PopupBFCopy.Enabled := True;

                for i := 0 to PopupBF.Items.Count - 1 do
                    if PopupBF.Items[i] <> PopupBFCopy then
                        PopupBF.Items[i].Enabled := False;
            end;
        end;
    end;
end;

procedure TForm1.MenuProjectAutoExpandClick(Sender: TObject);
begin
    SourceFileContents.AutoExpand := not SourceFileContents.AutoExpand;
    MenuProjectAutoExpand.Checked := SourceFileContents.AutoExpand;
    PopupProjectAutoExpand.Checked := SourceFileContents.AutoExpand;
end;

procedure TForm1.MenuProjectFullExpandClick(Sender: TObject);
begin
    SourceFileContents.FullExpand;
end;

procedure TForm1.MenuAtomsCopyClick(Sender: TObject);
begin
    Atoms.CopyToClipBoard;
end;

procedure TForm1.MenuAtomsPasteClick(Sender: TObject);
begin
    if Clipboard.HasFormat(CF_TEXT) then
    begin
        Atoms.PasteFromClipboard;
        FillBasisFunctions;
    end;
end;

procedure TForm1.MenuAtomsDeleteClick(Sender: TObject);
begin
    Atoms.DeleteSelection;
    FillBasisFunctions;
end;

procedure TForm1.MenuAtomsSelectAllClick(Sender: TObject);
begin
    Atoms.SelectAll;
end;

procedure TForm1.MenuAtomsInsertClick(Sender: TObject);
begin
    with Atoms do InsertRows(
        Row, 1, False);
    FillBasisFunctions;
end;

procedure TForm1.MenuAtomsAddClick(Sender: TObject);
begin
    Atoms.AddRow;
    FillBasisFunctions;
end;

procedure TForm1.MenuBFCopyClick(Sender: TObject);
begin
    BasisFunctions.CopyToClipBoard;
end;

procedure TForm1.MenuBFPasteClick(Sender: TObject);
begin
    if Clipboard.HasFormat(CF_TEXT) then BasisFunctions.PasteFromClipboard;
end;

procedure TForm1.MenuBFDeleteClick(Sender: TObject);
begin
    BasisFunctions.DeleteSelection;
end;

procedure TForm1.MenuBFSelectAllClick(Sender: TObject);
begin
    BasisFunctions.SelectAll;
end;

procedure TForm1.MenuBFAddClick(Sender: TObject);
begin
    BasisFunctions.AddColumn;
end;

procedure TForm1.MenuBFInsertClick(Sender: TObject);
begin
    with BasisFunctions do InsertColumns(
        Col, 1, False);
end;

procedure TForm1.MenuSitesAddClick(Sender: TObject);
var TC: TComponent;
    TS: TSite;
begin
    TC := GetSelectedData;
    if Assigned(TC) then
        if TC is TSiteList then
            with TC as TSiteList do
            begin
                TS := CreateNewSite;
                EditSiteDlg.EditedSite := TS;
                if EditSiteDlg.ShowModal = mrOk then
                begin
                    Add(TS);
                    FillSites;
                    FillAtoms;
                    FillPropVectors;
                    FillRepresentations;
                    FillBasisFunctions;
                    Saved := False;
                end else UtilizeObject(TS);
            end;    //  if TC is TSiteList then...
end;

procedure TForm1.MenuSitesDeleteClick(Sender: TObject);
var TC: TComponent;
begin
    if Sites.Items.Count <> 0 then
    begin
        TC := GetSelectedData;
        if Assigned(TC) then
            if TC is TSiteList then
                with TC as TSiteList do
                begin
                    Delete(Sites.ItemIndex);
                    FillSites;
                    FillAtoms;
                    FillPropVectors;
                    FillRepresentations;
                    FillBasisFunctions;
                    Saved := False;
                end;
    end;
end;

procedure TForm1.MenuSitesPropertiesClick(Sender: TObject);
var TS: TSite;
    SaveIndex: LongInt;
begin
    if Sites.Items.Count <> 0 then
    begin
        SaveIndex := Sites.ItemIndex;
        TS := TSite(Sites.Items.Objects[Sites.ItemIndex]);
        EditSiteDlg.EditedSite := TS;
        if EditSiteDlg.ShowModal = mrOk then
        begin
            FillSites;
            FillAtoms;
            Sites.ItemIndex := SaveIndex;
            Saved := False;
        end;
    end;
end;

procedure TForm1.MenuStarsAddClick(Sender: TObject);
var TS: TSite;
    TW: TWaveVector;
begin
    if Sites.Items.Count <> 0 then
    begin
        TS := TSite(Sites.Items.Objects[Sites.ItemIndex]);
        TW := CreateNewWaveVector;
        EditPropVectorDlg.EditedPropVector := TW;
        if EditPropVectorDlg.ShowModal = mrOk then
        begin
            TComponentList(TS.WaveVectorList).Add(TW);
            FillPropVectors;
            FillRepresentations;
            FillBasisFunctions;
            Saved := False;
        end else UtilizeObject(TW);
    end;
end;

procedure TForm1.MenuStarsDeleteClick(Sender: TObject);
var TS: TSite;
    WL: TWaveVectorList;
begin
    if WaveVectors.Items.Count <> 0 then
    begin
        TS := TSite(Sites.Items.Objects[Sites.ItemIndex]);
        WL := TWaveVectorList(TS.WaveVectorList);
        WL.Delete(WaveVectors.ItemIndex);
        FillPropVectors;
        FillRepresentations;
        FillBasisFunctions;
        Saved := False;
    end;
end;

procedure TForm1.MenuStarsPropertiesClick(Sender: TObject);
var TW: TWaveVector;
    SaveIndex: LongInt;
begin
    if WaveVectors.Items.Count <> 0 then
    begin
        SaveIndex := WaveVectors.ItemIndex;

        TW := TWaveVector(WaveVectors.Items.Objects[WaveVectors.ItemIndex]);

        EditPropVectorDlg.EditedPropVector := TW;
        if EditPropVectorDlg.ShowModal = mrOk then
        begin
            WaveVectors.ItemIndex := SaveIndex;
            FillPropVectors;
            Saved := False;
        end;
    end;
end;

procedure TForm1.MenuReprAddClick(Sender: TObject);
var TW: TWaveVector;
    TR: TRepresentation;
begin
    if WaveVectors.Items.Count <> 0 then
    begin
        TW := TWaveVector(WaveVectors.Items.Objects[WaveVectors.ItemIndex]);
        TR := CreateNewRepr;
        TComponentList(TW.Representations).Add(TR);
        FillRepresentations;
        FillBasisFunctions;
        Saved := False;
    end;
end;

procedure TForm1.MenuReprDeleteClick(Sender: TObject);
var TW: TWaveVector;
    TR: TRepresentationList;
begin
    if Representations.Items.Count <> 0 then
    begin
        TW := TWaveVector(WaveVectors.Items.Objects[WaveVectors.ItemIndex]);
        TR := TRepresentationList(TW.Representations);
        TR.Delete(Representations.ItemIndex);
        FillRepresentations;
        FillBasisFunctions;
        Saved := False;
    end;
end;

procedure TForm1.MenuResultsCopyClick(Sender: TObject);
begin
    if ActiveControl is TIDA_Grid then
        with ActiveControl as TIDA_Grid do CopyToClipBoard;
end;

procedure TForm1.MenuNeutronCopyClick(Sender: TObject);
begin
    Table.CopyToClipBoard;
end;

procedure TForm1.MenuNeutronPasteClick(Sender: TObject);
begin
    if Clipboard.HasFormat(CF_TEXT) then Table.PasteFromClipboard;
end;

procedure TForm1.MenuNeutronDeleteClick(Sender: TObject);
begin
    Table.DeleteSelection;
end;

procedure TForm1.MenuNeutronSelectAllClick(Sender: TObject);
begin
    Table.SelectAll;
end;

procedure TForm1.MenuNeutronInsertClick(Sender: TObject);
begin
    with Table do InsertRows(
        Row, 1, False);
end;

procedure TForm1.MenuNeutronAddClick(Sender: TObject);
begin
    Table.AddRow;
end;

procedure TForm1.MenuPlotPatternCopyClick(Sender: TObject);
var TC: TComponent;
begin
    TC := GetSelectedData;
    if Assigned(TC) then
        if TC is TSinTDataPointer then ClipBoard.Assign(TempBitMap);
end;

procedure TForm1.MenuPlotStructCopyClick(Sender: TObject);
var TC: TComponent;
begin
    TC := GetSelectedData;
    if Assigned(TC) then
        if TC is TSiteListPlotParams then ClipBoard.Assign(TempBitMap);
end;

procedure TForm1.MenuPlotPatternSaveClick(Sender: TObject);
begin
    SaveBitmapIntoFile(TempBitMap);
end;

procedure TForm1.SaveBitmapIntoFile(const BitMap: TBitMap);
var Result: Boolean;
    BMPFileName: TFileName;
    BMPStream: TFileStream;
begin
    SaveDialog1.Filter := 'Bitmaps (*.BMP)|*.BMP';
    SaveDialog1.FileName := '';
    Result := SaveDialog1.Execute;
    if Result then
    begin
        BMPFileName := SaveDialog1.FileName;
        if ExtractFileExt(BMPFileName) = '' then BMPFileName := BMPFileName + '.bmp';
        if FileExists(BMPFileName) then
            if MessageDlg('File ' + Uppercase(ExtractFileName(BMPFileName)) +
                ' already exists...Overwrite?', mtWarning, [mbYes, mbNo, mbCancel],
                0) <> mrYes then Exit;
        BMPStream := TFileStream.Create(BMPFileName, fmCreate);
        BitMap.SaveToStream(BMPStream);
        UtilizeObject(BMPStream);
    end;
end;

procedure TForm1.MenuPlotStructSaveClick(Sender: TObject);
begin
    SaveBitmapIntoFile(TempBitMap);
end;

procedure TForm1.MenuMemoCopyClick(Sender: TObject);
begin
    if ActiveControl is TMemo then
        with ActiveControl as TMemo do CopyToClipBoard;
end;

procedure TForm1.MenuMemoPasteClick(Sender: TObject);
begin
    if Clipboard.HasFormat(CF_TEXT) then
        with Comments do PasteFromClipboard;
end;

procedure TForm1.MenuFFEditClick(Sender: TObject);
var St: string;
begin
    St := '"' + FFPath + FORM_FACTORS_FILE_NAME + '"';
    if ShellExecute(0, nil, PChar(FFPath + 'FFEditor.exe'),
    PChar(St), nil, sw_Show) <= 32 then
        MessageDlg('Can''t execute "Form - Factor Editor"...', mtError, [mbOk], 0);
end;

procedure TForm1.CommentsEnter(Sender: TObject);
begin
    MenuMemoEdit.Visible := True;
end;

procedure TForm1.CommentsExit(Sender: TObject);
begin
    MenuMemoEdit.Visible := False;
end;

procedure TForm1.MenuAtomsEditClick(Sender: TObject);
var TempNode: TTreeNode;
    Comp: TComponent;
    i: LongInt;
begin
    TempNode := SourceFileContents.Selected;
    TempNode := TempNode.Parent;
    if TempNode <> nil then
    begin
        Comp := TComponent(TempNode.Data);
        if Comp <> nil then
        begin
            if (Comp is TReverseSourceData) or
               (Comp is TDirectSourceData) then
            begin
                if ClipBoard.HasFormat(CF_TEXT) then
                    MenuAtomsPaste.Enabled := True
                else MenuAtomsPaste.Enabled := False;

                for i := 0 to MenuAtomsEdit.Count - 1 do
                    if MenuAtomsEdit[i] <> MenuAtomsPaste then
                        MenuAtomsEdit[i].Enabled := True;
                Exit;
            end;

            if Comp is TReverseCalcData then
            begin
                MenuAtomsCopy.Enabled := True;

                for i := 0 to MenuAtomsEdit.Count - 1 do
                    if MenuAtomsEdit[i] <> MenuAtomsCopy then
                        MenuAtomsEdit[i].Enabled := False;
            end;
        end;
    end;
end;

procedure TForm1.MenuBFEditClick(Sender: TObject);
var TempNode: TTreeNode;
    Comp: TComponent;
    i: LongInt;
begin
    TempNode := SourceFileContents.Selected;
    TempNode := TempNode.Parent;
    if TempNode <> nil then
    begin
        Comp := TComponent(TempNode.Data);
        if Comp <> nil then
        begin
            if (Comp is TReverseSourceData) or
               (Comp is TDirectSourceData) then
            begin
                if ClipBoard.HasFormat(CF_TEXT) then
                    MenuBFPaste.Enabled := True
                else MenuBFPaste.Enabled := False;

                for i := 0 to MenuBFEdit.Count - 1 do
                    if MenuBFEdit[i] <> MenuBFPaste then
                        MenuBFEdit[i].Enabled := True;
                Exit;
            end;

            if Comp is TReverseCalcData then
            begin
                MenuBFCopy.Enabled := True;

                for i := 0 to MenuBFEdit.Count - 1 do
                    if MenuBFEdit[i] <> MenuBFCopy then
                        MenuBFEdit[i].Enabled := False;
            end;
        end;
    end;
end;

procedure TForm1.MenuNeutronEditClick(Sender: TObject);
begin
    if ClipBoard.HasFormat(CF_TEXT) then
        MenuNeutronPaste.Enabled := True
    else MenuNeutronPaste.Enabled := False;
end;

procedure TForm1.MenuMemoEditClick(Sender: TObject);
begin
    if ClipBoard.HasFormat(CF_TEXT) then
        MenuMemoPaste.Enabled := True
    else MenuMemoPaste.Enabled := False;
end;

procedure TForm1.AtomsGridModified(Sender: TObject);
begin
    Saved := False;
end;

procedure TForm1.MenuSitesEditClick(Sender: TObject);
var TempNode: TTreeNode;
    Comp: TComponent;
    i: LongInt;
begin
    TempNode := SourceFileContents.Selected;
    TempNode := TempNode.Parent;
    if TempNode <> nil then
    begin
        Comp := TComponent(TempNode.Data);
        if Comp <> nil then
        begin
            if (Comp is TReverseSourceData) or
               (Comp is TDirectSourceData) then
            begin
                for i := 0 to MenuSitesEdit.Count - 1 do
                    MenuSitesEdit[i].Enabled := True;
                Exit;
            end;

            if Comp is TReverseCalcData then
            begin
                for i := 0 to MenuSitesEdit.Count - 1 do
                    MenuSitesEdit[i].Enabled := False;
            end;
        end;
    end;
end;

procedure TForm1.MenuStarsEditClick(Sender: TObject);
var TempNode: TTreeNode;
    Comp: TComponent;
    i: LongInt;
begin
    TempNode := SourceFileContents.Selected;
    TempNode := TempNode.Parent;
    if TempNode <> nil then
    begin
        Comp := TComponent(TempNode.Data);
        if Comp <> nil then
        begin
            if (Comp is TReverseSourceData) or
               (Comp is TDirectSourceData) then
            begin
                for i := 0 to MenuStarsEdit.Count - 1 do
                    MenuStarsEdit[i].Enabled := True;
                Exit;
            end;

            if Comp is TReverseCalcData then
            begin
                for i := 0 to MenuStarsEdit.Count - 1 do
                    MenuStarsEdit[i].Enabled := False;
            end;
        end;
    end;
end;

procedure TForm1.MenuReprEditClick(Sender: TObject);
var TempNode: TTreeNode;
    Comp: TComponent;
    i: LongInt;
begin
    TempNode := SourceFileContents.Selected;
    TempNode := TempNode.Parent;
    if TempNode <> nil then
    begin
        Comp := TComponent(TempNode.Data);
        if Comp <> nil then
        begin
            if Comp is TReverseSourceData then
            begin
                for i := 0 to MenuReprEdit.Count - 1 do
                    MenuReprEdit[i].Enabled := True;
                Exit;
            end;

            if (Comp is TReverseCalcData) or
               (Comp is TDirectSourceData) then
            begin
                for i := 0 to MenuReprEdit.Count - 1 do
                    MenuReprEdit[i].Enabled := False;
            end;
        end;
    end;
end;

initialization
    Dilat1 := 1; Dilat2 := 1; Dilat3 := 1;
end.
