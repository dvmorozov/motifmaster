{------------------------------------------------------------------------------
    This file is part of the MotifMASTER project. This software is
    distributed under GPL (see gpl.txt for details).

    This software is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Copyright (C) 1999-2007 D.Morozov (dvmorozov@mail.ru)
------------------------------------------------------------------------------}

unit DataClasses;

interface

uses
    Classes, ComponentList, Grids, SysUtils, NumericGrid, SimpMath,
    Math3D, Tools, TableComp, DownhillSimplexContainer, SelfCopied,
    ObjSavingStringList, Graphics, SelfSaved, ClassInheritIDs;

const
    MAX_HKL_INDEX: ShortInt = 40;
    PROP_FACTOR: Double = 0.2695;
    IF_SINT_SIN2T       = 0;
    IF_SQR_SIN2T        = 1;

    PVT_MAGNETIC        = 1;
    PVT_STRUCTURAL      = 2;

    TT_SS               = 0;
    TT_LSW              = 1;
    TT_ES               = 2;
    TT_CS               = 3;
    TT_NUMBER           = 3;
    RA_NONE             = 0;
    RA_X                = 1;
    RA_Y                = 2;
    RA_Z                = 3;
    RA_OTHER            = 4;

    ALLOWED_AXIS_X      = 1;
    ALLOWED_AXIS_Y      = 2;
    ALLOWED_AXIS_Z      = 4;
    ALLOWED_AXIS_ANY    = 8;
    ST_ONE_ARM          = 1;
    ST_TWO_ARMS         = 2;
    VM_NONE             = 0;
    VM_ALL_COMPONENTS   = 1;
    VM_EQUAL_MODULES    = 2;
    VM_ONLY_MODULES     = 3;
    EVMT_ALL_COMPONENTS = 1;
    EVMT_EQUAL_MODULES  = 2;
    EVMT_ONLY_MODULES   = 4;

    RM_NONE             = 0;
    RM_BY_ONE           = 1;
    RM_BY_ALL           = 2;
    RM_DONT_USE         = 3;
    ERMT_BY_ONE         = 1;
    ERMT_BY_ALL         = 2;
    ERMT_DONT_USE       = 4;

    CH_DIS_ANG_X        = 1;
    CH_DIS_ANG_Y        = 2;
    CH_DIS_ANG_Z        = 4;

    INVALID_X           = 1;
    INVALID_Y           = 2;
    INVALID_Z           = 4;

    SSC_NO_CHECKING     = 0;
    SSC_REPR            = 1;
    SSC_ONLY_SELF       = 2;

    SSCOPY_DEFAULT      = 0;
    SSCOPY_WITH_REPR    = 1;
    SSCOPY_WITHOUT_REPR = 2;

const
    PatternParamsContainerGUID: TGUID = '{44825A04-DD2D-4AB7-A989-421B62B26588}';

var
    HKLSetSort: TListSortCompare;

type
    TCellProc = procedure(Grid: TStringGrid; ColNum, RowNum: LongInt);
        procedure StructCompListCellProcedure(Grid: TStringGrid;
    ColNum, RowNum: LongInt);
{$IFNDEF Lazarus}
         forward;
{$ENDIF}

const
    StructCompListCellProc: Pointer = @StructCompListCellProcedure;

type
    EAtom = class(Exception);
    EAtomList = class(Exception);
    EBasisFunctions = class(Exception);
    EGeneralClass = class(Exception);
    EGeneratorClass = class(Exception);
    EHKLList = class(Exception);
    ENeutronClass = class(Exception);
    ENeutronClassMono = class(Exception);
    ENeutronCompList = class(Exception);
    ENeutronCompListMono = class(Exception);
    EPatternAuxiliary = class(Exception);
    EPossibleModel = class(Exception);
    ERepresentation = class(Exception);
    ERepresentationList = class(Exception);
    ESinTCompList = class(Exception);    
    ESite = class(Exception);
    ESiteList = class(Exception);
    EStarLink = class(Exception);
    EStructure = class(Exception);
    EWaveVector = class(Exception);
    EWaveVectorList = class(Exception);

    TAtom = class;
    TAtomList = class;
    TBasisFunctions = class;
    TCommentsClass = class;
    THKLList = class;
    TPatternAuxiliary = class;
    TRepresentationList = class;
    TSite = class;
    TSiteList = class;
    TStructure = class;
    TWaveVector = class;
    TWaveVectorList = class;
    TPatternParametersContainer = class;

    IWaveVectorServer = interface
        function GetA: Double;
        function GetB: Double;
        function GetC: Double;
        function GetAlpha: Double;
        function GetBeta: Double;
        function GetGamma: Double;
        function GetAtomList: TAtomList;

        property A: Double read GetA;
        property B: Double read GetB;
        property C: Double read GetC;
        property Alpha: Double read GetAlpha;
        property Beta: Double read GetBeta;
        property Gamma: Double read GetGamma;
        property AtomList: TAtomList read GetAtomList;
    end;

    IIntensityFactors = interface
        function GetMulIntParam: Double;
        procedure SetMulIntParam(const AMulIntParam: Double);
        function GetDWParam: Double;
        procedure SetDWParam(const ADWParam: Double);
        function GetIFType: ShortInt;
        procedure SetIFType(const AIFType: ShortInt);
        function GetLambda: Double;

        procedure SetLambda(const ALambda: Double);

        function GetIntegralFactor(const SinT, Sin2T: Double): Double;
        function GetDebyeWallerFactor(const SinTL: Double): Double;

        property MulIntParam: Double
            read GetMulIntParam         write SetMulIntParam;
        property DWParam: Double
            read GetDWParam             write SetDWParam;
        property IFType: ShortInt
            read GetIFType              write SetIFType;
        property Lambda: Double
            read GetLambda              write SetLambda;
    end;

    TPositionRec = class
    public
        Position: string[3];
        AtomList: TSelfCleanList;
        constructor Create; virtual;
        destructor Destroy; override;
    end;

    TFFArray = array[0..43] of Double;

    TFFArrRec = class
        NameOfElement: string[10];
        NumberOfPoints: LongInt;
        StepOfPoints: Double;
        FFArray: TFFArray;
    end;

    TAtom = class(TSelfCopiedComponent, ISelfChecked)
    protected
        Fx, Fy, Fz: Double;
        FDispX, FDispY, FDispZ: Double;
        FMx, FMy, FMz: Double;
        FM: Double;
        FNuclearScatAmpl: Double;
        FElement: string;
        FPosition: string;
        FUnitMagnVect: TDoubleVector3;
        FNumber: LongInt;
        DirectionChanged: Boolean;
        FEllipseParam: Double;

        FSiteList: TSiteList;
        FSite: TSite;
        function GetMx: Double;
        function GetMy: Double;
        function GetMz: Double;
        procedure SetMx(AMx: Double);
        procedure SetMy(AMy: Double);
        procedure SetMz(AMz: Double);

        function GetM: Double;
        procedure SetM(AM: Double);

        function GetUnitMagnVect: TDoubleVector3;
        function GetMoment: TDoubleVector3;

        procedure RecalcUnitVect;

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

        function GetSelfCheckingMode: LongInt; virtual; abstract;
        procedure SetSelfCheckingMode(const AMode: LongInt); virtual; abstract;

    public
        constructor Create(AOwner: TComponent); override;
        procedure IsReady;
        function MyNameIs: string;
        procedure CopyParameters(const Dest: TObject); override;

        function GetSite: TSite;
        procedure SetSite(const Site: TSite);
        function GetSiteList: TSiteList;
        procedure SetSiteList(const SiteList: TSiteList);

        property NuclearScatAmpl: Double
            read FNuclearScatAmpl           write FNuclearScatAmpl;
        property Element: string
            read FElement                   write FElement;
        property Position: string
            read FPosition                  write FPosition;
        property Number: LongInt
            read FNumber                    write FNumber;

        property DispX: Double
            read FDispX                     write FDispX;
        property DispY: Double
            read FDispY                     write FDispY;
        property DispZ: Double
            read FDispZ                     write FDispZ;
        property Mx: Double
            read GetMx                      write SetMx;
        property My: Double
            read GetMy                      write SetMy;
        property Mz: Double
            read GetMz                      write SetMz;
        property Moment: TDoubleVector3
            read GetMoment;

        property UnitMagnVect: TDoubleVector3
                read GetUnitMagnVect;

        property EllipseParam: Double
            read FEllipseParam              write FEllipseParam;

        property x: Double      read Fx         write Fx;
        property y: Double      read Fy         write Fy;
        property z: Double      read Fz         write Fz;

        property M: Double      read GetM       write SetM;
            end;

    TNeutronClass = class(TSelfCopiedComponent, ISelfChecked)
    protected
        FIntensity: Double;
        FStartPos, FFinishPos, FPeakPos: Double;

        FIntCorrFactor: Double;
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

        function GetSelfCheckingMode: LongInt; virtual; abstract;
        procedure SetSelfCheckingMode(const AMode: LongInt); virtual; abstract;

    public
        constructor Create(AOwner: TComponent); override;
        function MyNameIs: string;
        procedure IsReady;
        procedure CopyParameters(const Dest: TObject); override;

        property Intensity: Double
            read FIntensity             write FIntensity;
        property StartPos: Double
            read FStartPos              write FStartPos;
        property FinishPos: Double
            read FFinishPos             write FFinishPos;
        property PeakPos: Double
            read FPeakPos               write FPeakPos;
        property IntCorrFactor: Double
            read FIntCorrFactor         write FIntCorrFactor;
    end;

    TNeutronClassMono = class(TSelfCopiedComponent, ISelfChecked)
    protected
        FIntensity: Double;
        FH, FK, FL: Double;

        FIntCorrFactor: Double;
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

        function GetSelfCheckingMode: LongInt; virtual; abstract;
        procedure SetSelfCheckingMode(const AMode: LongInt); virtual; abstract;

    public
        constructor Create(AOwner: TComponent); override;
        function MyNameIs: string;
        procedure IsReady;
        procedure CopyParameters(const Dest: TObject); override;

        property Intensity: Double
            read FIntensity             write FIntensity;
        property H: Double
            read FH                     write FH;
        property K: Double
            read FK                     write FK;
        property L: Double
            read FL                     write FL;
        property IntCorrFactor: Double
            read FIntCorrFactor         write FIntCorrFactor;
    end;

    TCommentsClass = class(TSelfSavedComponent)
    protected
        FList: TStringList;
        FCaption: string;

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

    public
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;

        property List: TStringList  read FList;
        property Caption: string    read FCaption   write FCaption;
    end;

    TAtomList = class(TRowCompList, IGridDataSource)
    protected
        FSite: TSite;

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

        procedure LinkItemWithList(const Item: TComponent); override;

        function CreateNewObject: TComponent; override;

    public
        procedure SetRandomMoments(Site: string; Range: Double; EntirelyAtSite: Boolean);
        procedure SetModuleAtSite(Site: string; Module: Double);
        procedure SetMeanModuleAtSite(Site: string);
        function GetMeanModuleAtSite(Site: string): Double;
        procedure MulModuleAtSite(Site: string; MulConst: Double);
        procedure EnumerateAtoms;
        procedure SortPosition(PL: TList);
        function GetCopyAsSiteList: TSiteList;

        procedure SetSiteList(const ASiteList: TSiteList);
        procedure SetSite(const ASite: TSite);

        procedure SetDataToGrid(Grid: TStringGrid); override;
        procedure SetCaption(Grid: TStringGrid); override;
        procedure SetColOptions(Grid: TStringGrid); override;
        procedure SetColFunc(Grid: TStringGrid); override;
        procedure SetRowContents(
            Grid: TStringGrid; RowNum: LongInt); override;
        function GetRowContents(
            Grid: TStringGrid; RowNum: LongInt): Boolean; override;

        function ValueToString(const ACol, ARow: LongInt
            ): string; override;
        procedure StringToValue(const ACol, ARow: LongInt;
            const AString: string
            ); override;
        procedure SetValueByDefault(const ACol, ARow: LongInt); override;
        function IsDataValid(const ACol, ARow: LongInt;
            const AString: string): Boolean; override;
        function GetCellEnabledCharSet(
            const ACol, ARow: LongInt): TCharSet; override;

        function GetInfoCols: LongInt; override;

        procedure Delete(Index: Integer); override;
        procedure Insert(Index: Integer; Item: TComponent); override;
        function Add(Item: TComponent): Integer; override;
    end;

    IPatternParametersContainer = interface
    ['{44825A04-DD2D-4AB7-A989-421B62B26588}']
        function GetStartPos: Double;
        procedure SetStartPos(const AStartPos: Double);
        function GetEndPos: Double;
        procedure SetEndPos(const AEndPos: Double);
        function GetLambda: Double;
        procedure SetLambda(const ALambda: Double);
        function GetUseStartEnd: Boolean;
        procedure SetUseStartEnd(const AUseStartEnd: Boolean);

        property StartPos: Double read GetStartPos write SetStartPos;
        property EndPos: Double read GetEndPos write SetEndPos;
        property UseStartEnd: Boolean read GetUseStartEnd write SetUseStartEnd;
        property Lambda: Double read GetLambda write SetLambda;
    end;

    TPatternParametersContainer = class(TSelfCopiedComponent,
        IPatternParametersContainer, ISelfChecked)
    protected
        FLambda: Double;
        FStartPos, FEndPos: Double;
        FUseStartEnd: Boolean;

        function GetStartPos: Double;
        procedure SetStartPos(const AStartPos: Double);
        function GetEndPos: Double;
        procedure SetEndPos(const AEndPos: Double);
        function GetLambda: Double;
        procedure SetLambda(const ALambda: Double);
        function GetUseStartEnd: Boolean;
        procedure SetUseStartEnd(const AUseStartEnd: Boolean);

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

        function GetSelfCheckingMode: LongInt; virtual; abstract;
        procedure SetSelfCheckingMode(const AMode: LongInt); virtual; abstract;

    public
        procedure CopyParameters(const Dest: TObject); override;
        procedure IsReady;
        function MyNameIs: string;

        property StartPos: Double
                read GetStartPos            write SetStartPos;
        property EndPos: Double
                read GetEndPos              write SetEndPos;
        property UseStartEnd: Boolean
                read GetUseStartEnd         write SetUseStartEnd;
        property Lambda: Double
                read GetLambda              write SetLambda;
    end;

    TNeutronCompList = class(TRowCompList, IPatternParametersContainer)
    protected
        FPatternParameters: TPatternParametersContainer;

        function GetStartPos: Double;
        procedure SetStartPos(const AStartPos: Double);
        function GetEndPos: Double;
        procedure SetEndPos(const AEndPos: Double);
        function GetLambda: Double;
        procedure SetLambda(const ALambda: Double);
        function GetUseStartEnd: Boolean;
        procedure SetUseStartEnd(const AUseStartEnd: Boolean);

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

        function CreateNewObject: TComponent; override;

    public
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;

        function GetMaxNeutronSinTL(
            const PatternAuxiliary: TPatternAuxiliary): Double; virtual;

        procedure SetCaption(Grid: TStringGrid); override;
        procedure SetColOptions(Grid: TStringGrid); override;
        function GetRowContents(Grid: TStringGrid; RowNum: LongInt): Boolean; override;
        procedure SetRowContents(Grid: TStringGrid; RowNum: LongInt); override;

        procedure IsReady; override;
        procedure SendErrorMessage(
            const ExceptionMsg, ObjectName: string;
            const ItemNumber: LongInt); override;
        procedure CopyParameters(const Dest: TObject); override;

        function ValueToString(const ACol, ARow: LongInt
            ): string; override;
        procedure StringToValue(const ACol, ARow: LongInt;
            const AString: string
            ); override;
        procedure SetValueByDefault(const ACol, ARow: LongInt); override;
            function IsDataValid(const ACol, ARow: LongInt;
            const AString: string): Boolean; override;
        function GetCellEnabledCharSet(
            const ACol, ARow: LongInt): TCharSet; override;

        function GetInfoCols: LongInt; override;

        property PatternParameters: TPatternParametersContainer
            read FPatternParameters     write FPatternParameters;

        property StartPos: Double
            read GetStartPos            write SetStartPos;
        property EndPos: Double
            read GetEndPos              write SetEndPos;
        property UseStartEnd: Boolean
            read GetUseStartEnd         write SetUseStartEnd;
        property Lambda: Double
            read GetLambda              write SetLambda;
    end;

    TNeutronCompListMono = class(TNeutronCompList, IPatternParametersContainer)
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

        function CreateNewObject: TComponent; override;

    public
        procedure CreateHKLList(
            const PatternAuxiliary: TPatternAuxiliary;
            const ExtHKLList: THKLList;
            const AMinSinTL, AMaxSinTL: Double);

        function GetMaxNeutronSinTL(
            const PatternAuxiliary: TPatternAuxiliary): Double; override;

        procedure SetCaption(Grid: TStringGrid); override;
        procedure SetColOptions(Grid: TStringGrid); override;
        function GetRowContents(Grid: TStringGrid; RowNum: LongInt): Boolean; override;
        procedure SetRowContents(Grid: TStringGrid; RowNum: LongInt); override;

        function ValueToString(const ACol, ARow: LongInt
            ): string; override;
        procedure StringToValue(const ACol, ARow: LongInt;
            const AString: string
            ); override;
        procedure SetValueByDefault(const ACol, ARow: LongInt); override;
            function IsDataValid(const ACol, ARow: LongInt;
            const AString: string): Boolean; override;
        function GetCellEnabledCharSet(
            const ACol, ARow: Integer): TCharSet; override;

        function GetInfoCols: LongInt; override;
    end;

    TSinTClass = class(TSelfCopiedComponent)
        protected
        FSinT: Double;
        FSinTL: Double;
        FSin2T: Double;

        FPureMagnInt: Double;
        FPureNuclInt: Double;
        FExpIntensity: Double;
        FIntensity: Double;
        FNuclearIntensity: Double;
        FRFactor: Double;
        FStartPos, FFinishPos, FPeakPos: Double;

        FFullView: Boolean;

        FHKLList: TSelfCopiedCompList;
        FIntCorrFactor: Double;

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
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        procedure CopyParameters(const Dest: TObject); override;

        property HKLList: TSelfCopiedCompList read FHKLList;
        property ExpIntensity: Double
            read FExpIntensity          write FExpIntensity;
        property Intensity: Double
            read FIntensity             write FIntensity;
        property NuclearIntensity: Double
            read FNuclearIntensity      write FNuclearIntensity;
        property PureMagnInt: Double
            read FPureMagnInt           write FPureMagnInt;
        property PureNuclInt: Double
            read FPureNuclInt           write FPureNuclInt;
        property SinT: Double
            read FSinT                  write FSinT;
        property SinTL: Double
            read FSinTL                 write FSinTL;
        property Sin2T: Double
            read FSin2T                 write FSin2T;
        property RFactor: Double
            read FRFactor               write FRFactor;
        property StartPos: Double
            read FStartPos              write FStartPos;
        property FinishPos: Double
            read FFinishPos             write FFinishPos;
        property PeakPos: Double
            read FPeakPos               write FPeakPos;
        property IntCorrFactor: Double
            read FIntCorrFactor         write FIntCorrFactor;
        property FullView: Boolean
            read FFullView              write FFullView;
    end;

    TSinTCompList = class(TTableCompList, IIntensityFactors)
    protected
        FAl, FBt, FGm: Double;
        FA, FB, FC: Double;
        FDWParam: Double;
        FMulIntParam: Double;
        FIFType: ShortInt;
        FLambda: Double;
        FCalcResults: TSelfCopiedComponent;

        function GetMulIntParam: Double;
        procedure SetMulIntParam(const AMulIntParam: Double);
        function GetDWParam: Double;
        procedure SetDWParam(const ADWParam: Double);
        function GetIFType: ShortInt;
        procedure SetIFType(const AIFType: ShortInt);
        function GetLambda: Double;
        procedure SetLambda(const ALambda: Double);

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
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        function Add(Item: TComponent): Integer; override;
        procedure Delete(Index: Integer); override;
        procedure Insert(Index: Integer; Item: TComponent); override;

        procedure ExpandItem(const Index: LongInt);
        procedure CollapseItem(const Index: LongInt);
        procedure ToggleExpanded(const Index: LongInt);
        function GetExpandedRowsNum(const Item: TSinTClass): LongInt;
        procedure CheckItemIndex(const Index: LongInt);
        function GetInfoCols: LongInt; override;
        function GetInfoRows: LongInt; override;

        procedure CopyParameters(const Dest: TObject); override;

        procedure SetDataToGrid(Grid: TStringGrid); override;
        procedure SetCaption(Grid: TStringGrid); override;
        procedure SetColOptions(Grid: TStringGrid); override;

        function GetCalcResults: TSelfCopiedComponent;
        procedure SetCalcResults(const ACalcResults: TSelfCopiedComponent);

        procedure CalcSummaryIntensities;
        procedure CalcResultingIntensities;
        function GetIntegralFactor(const SinT, Sin2T: Double): Double;
        function GetDebyeWallerFactor(const SinTL: Double): Double;

        property MulIntParam: Double
            read GetMulIntParam             write SetMulIntParam;
        property DWParam: Double
            read GetDWParam                 write SetDWParam;
        property IFType: ShortInt
            read GetIFType                  write SetIFType;
        property Lambda: Double
            read FLambda                    write FLambda;
        property A: Double
            read FA                         write FA;
        property B: Double
            read FB                         write FB;
        property C: Double
            read FC                         write FC;
        property Alpha: Double
            read FAl                        write FAl;
        property Beta: Double
            read FBt                        write FBt;
        property Gamma: Double
            read FGm                        write FGm;
    end;

    TStarLink = class
    public
        Star: TWaveVector;
        H, K, L: LongInt;
    end;

    THKLClass = class(TSelfCopiedComponent)
    protected
        FReF, FImF: Double;
        FIntensity: Double;
        FNuclearIntensity: Double;
        FMulConst: Double;
        FSinT: Double;
        FSin2T: Double;
        FSinTL: Double;

        FWVH, FWVK, FWVL: Double;
        FUnitScatVect: TDoubleVector3;
        StarLinks: TSelfCleanList;

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
        VectorFRe: TDoubleVector3;
        VectorFIm: TDoubleVector3;
        NuclearReF, NuclearImF: Double;

        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;

        procedure CopyParameters(const Dest: TObject); override;

        procedure AddStarLink(const StarLink: TStarLink);
        procedure ClearStarLinks;
        function GetStarLinksNumber: LongInt;

        procedure CalcNuclearIntensity;
        procedure CalcMagneticIntensity;

        property Intensity: Double
                read FIntensity         write FIntensity;
        property NuclearIntensity: Double
                read FNuclearIntensity  write FNuclearIntensity;
        property MulConst: Double
                read FMulConst          write FMulConst;

        property SinT: Double
                read FSinT              write FSinT;
        property Sin2T: Double
                read FSin2T             write FSin2T;
        property SinTL: Double
                read FSinTL             write FSinTL;
        property ReF: Double
                read FReF               write FReF;
        property ImF: Double
                read FImF               write FImF;
        property WVH: Double
                read FWVH               write FWVH;
        property WVK: Double
                read FWVK               write FWVK;
        property WVL: Double
                read FWVL               write FWVL;
        property UnitScatVect: TDoubleVector3
                read FUnitScatVect      write FUnitScatVect;
    end;

    THKLList = class(TComponentList)
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

    public
        function CreateNewHKL(
            const PatternAuxiliary: TPatternAuxiliary;
            const Vector: TDoubleVector3;
            var ReallyNew: Boolean
            ): THKLClass;

        procedure CalcMagneticIntensities;
        procedure CalcNuclearIntensities;
    end;

    TScattSphere = class(TComponentList)
    protected
        FRadius: Double;
        FSinT: Double;
        FSinTL: Double;
        FSin2T: Double;

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
        property Radius: Double     read FRadius    write FRadius;
        property SinT: Double       read FSinT      write FSinT;
        property SinTL: Double      read FSinTL     write FSinTL;
        property Sin2T: Double      read FSin2T     write FSin2T;
    end;

    TScattSphereList = class(TComponentList)
    protected
    public
        procedure FindSpheres(
            const PatternAuxiliary: TPatternAuxiliary;
            const HKLList: THKLList);
    end;

    TGeneralClass = class(TSelfCopiedComponent)
    protected
        FCaption: string;
        FStructurePointer: TSiteList;
        FPatternParamsPointer: IPatternParametersContainer;

        procedure SetA(const AA: Double);
        function GetA: Double;
        procedure SetB(const AB: Double);
        function GetB: Double;
        procedure SetC(const AC: Double);
        function GetC: Double;
        procedure SetAlpha(const AAlpha: Double);
        function GetAlpha: Double;
        procedure SetBeta(const ABeta: Double);
        function GetBeta: Double;
        procedure SetGamma(const AGamma: Double);
        function GetGamma: Double;
        procedure SetLambda(const ALambda: Double);
        function GetLambda: Double;
        procedure SetStartPos(const AStartPos: Double);
        function GetStartPos: Double;
        procedure SetEndPos(const AEndPos: Double);
        function GetEndPos: Double;
        procedure SetUseStartEnd(const AUseStartEnd: Boolean);
        function GetUseStartEnd: Boolean;

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
        property StructurePointer: TSiteList
                read FStructurePointer      write FStructurePointer;
        property PatternParamsPointer: IPatternParametersContainer
                read FPatternParamsPointer  write FPatternParamsPointer;
        property A: Double
                read GetA                   write SetA;
        property B: Double
                read GetB                   write SetB;
        property C: Double
                read GetC                   write SetC;
        property Alpha: Double
                read GetAlpha               write SetAlpha;
        property Beta: Double
                read GetBeta                write SetBeta;
        property Gamma: Double
            read GetGamma                   write SetGamma;
        property Lambda: Double
                read GetLambda              write SetLambda;
        property StartPos: Double
                read GetStartPos            write SetStartPos;
        property EndPos: Double
                read GetEndPos              write SetEndPos;
        property UseStartEnd: Boolean
                read GetUseStartEnd         write SetUseStartEnd;
        property Caption: string
                read FCaption               write FCaption;
    end;

    TGeneratorClass = class(TComponent)
    protected
        FBFList: TComponentList;
        FStructuresList: TComponentList;
        FUpperLimIndex: LongInt;

        function GetBFList: TComponentList;
        function GetStructuresList: TComponentList;

    public
        CurStructIndex: LongInt;
        destructor Destroy; override;

        property BFList: TComponentList
            read GetBFList                  write FBFList;
        property StructuresList: TComponentList
            read GetStructuresList          write FStructuresList;
        property UpperLimIndex: LongInt
            read FUpperLimIndex             write FUpperLimIndex;
    end;

    TDynamicLongArray = array of LongInt;

    TStructure = class(TComponent)
    protected
        FVectors: array of TDoubleVector3;
        FBFNumbers: array of ShortInt;
        FDiffrPattern: TSinTCompList;
        FGoodPeaksNumber: LongInt;
        function GetVectors(index: LongInt): TDoubleVector3;
        procedure SetVectors(index: LongInt; AVector: TDoubleVector3);
        function GetCount: LongInt;
        procedure SetCount(ACount: LongInt);
        function GetBFNumbers(index: ShortInt): ShortInt;
        function GetBFNumbersCount: ShortInt;
        function GetGoodPeaksNumber: LongInt;

    public
        LinZeroRFactor: Double;
        LinNonZeroRFactor: Double;
        LinearRFactor: Double;
        ZeroNonZeroRelation: Double;
        MaxCalcLinValue: Double;
        MaxCalcLinIndex: LongInt;
        MaxNonZeroLinValue: Double;
        MaxCalcExpRelation: Double;

        destructor Destroy; override;
        function SaveModules: Boolean;
        function HasVacuity: Boolean;
        function GetVacuities: TDynamicLongArray;
        function GetFillings: TDynamicLongArray;
        function FillingsDoNotCoincide(const BF: TBasisFunctions): Boolean;
        function VacuitiesCoincide(const BF: TBasisFunctions): Boolean;
        function IsVectorsOrthogonal(const TS: TStructure): Boolean;
        function HasOppositeVectors(const TS: TStructure): Boolean;
        function GetCopy: TStructure;
        procedure ModulesFlatten;
        procedure SetMomentModule(const MomentModule: Double);
        procedure AddBFNumber(const BFNumber: ShortInt);
        property Vectors[index: LongInt]: TDoubleVector3 read GetVectors
            write SetVectors; default;
        property Count: LongInt read GetCount write SetCount;
        property BFNumbers[index: ShortInt]: ShortInt read GetBFNumbers;
        property BFNumbersCount: ShortInt read GetBFNumbersCount;
        property GoodPeaksNumber: LongInt read GetGoodPeaksNumber;
    end;

    TSite = class(TDownhillRealParameters, ISelfCopied, ISelfChecked)
    protected
        FDisabled: Boolean; FNotMagnetic: Boolean;
        FSiteModule: Double;
        FSiteName: string;
        FNuclearScatAmpl: Double;
        FElement: string;
        FVariationMode: Byte;
        FReprMode: Byte;

        TempAtom: TAtom;
        FSiteList: TSiteList;
        FAtomList: TAtomList;
        FWaveVectorList: TWaveVectorList;
        FGeneratorClass: TGeneratorClass;
        FPlotParams: TSelfCopiedComponent;

        function GetAtomList: TAtomList;
        function GetAtomCount: LongInt;
        function GetGeneratorClass: TGeneratorClass;
        procedure SetBFVectorCount(const AVectorCount: LongInt);
        function GetMySiteList: TSiteList;
        function GetA: Double;
        function GetB: Double;
        function GetC: Double;
        function GetAlpha: Double;
        function GetBeta: Double;
        function GetGamma: Double;
        function GetMagnA: Double;
        function GetMagnB: Double;
        function GetMagnC: Double;
        function GetChemicalXNumber: LongInt;
        function GetChemicalYNumber: LongInt;
        function GetChemicalZNumber: LongInt;
        function GetTransNumber: LongInt;
        function GetMinPropVectComponent(const CompNumber: LongInt): Double;
        function GetMagnAtomsNumber: LongInt;
        function GetMagnAtom(index: LongInt): TAtom;
        function GetMagnAtomInChemical(index: LongInt): TAtom;
        function GetTranslation(index: LongInt): TDoubleVector3;
        procedure SetSiteModule(const Module: Double);
        procedure SetSiteName(const SiteName: string);
        procedure SetElement(const Element: string);
        procedure SetNuclearScatAmpl(const NuclearScatAmpl: Double);
        procedure CreateParameters; override;
        procedure FillParameters; override;
        procedure ParametersUpdated; override;
        function GetActualParametersNumber: LongInt; override;
        function GetNumberOfValues: LongInt; override;
        function GetValueIndex: LongInt; override;
        procedure SetValueIndex(const AValueIndex: LongInt); override;
        function GetAllowedVariationModes: Byte;
        function GetAllowedReprModes: Byte;
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
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;

        procedure LinkAtomWithSiteList(const Atom: TAtom);
        procedure LinkAtomsWithSite;
        function GetCopy: TObject;
        procedure CopyParameters(const Dest: TObject); virtual;
        function GetSelfCheckingMode: LongInt; virtual; abstract;
        procedure SetSelfCheckingMode(const AMode: LongInt); virtual; abstract;
        function GetSelfCopyingMode: LongInt; virtual; abstract;
        procedure SetSelfCopyingMode(const AMode: LongInt); virtual; abstract;
        procedure SetSiteList(const ASiteList: TSiteList);
        procedure DeleteVector(const Index: LongInt);
        procedure InsertNewVector(const Index: LongInt);
        procedure AddNewVector;
        procedure IsReady;
        function MyNameIs: string;
        function IsModulesEqual: Boolean;
        function IsModulesReal: Boolean;
        function GetPlotParams: TSelfCopiedComponent;
        procedure SetPlotParams(const APlotParams: TSelfCopiedComponent);
        procedure SetAtomMomentsCur;
        procedure SetAtomMomentsAll;
        procedure ResetMixingConst;
        procedure IncludeAllBasalFunctions;
        procedure ExcludeAllBasalFunctions;
        procedure MulModules(const MulConst: Double);
        procedure SetVectorsFromStruct(Struct: TStructure);
        procedure GetWaveVectorSubChain(var First, Last: TWaveVector);
        procedure CreateAtomList;
        procedure ReplaceAtomList(const AtomList: TAtomList);
        procedure CreatePropVectorsList;
        procedure ReplacePropVectorsList(const PropVectorsList: TWaveVectorList);
        procedure AddPropVectorToList(const PropVector: TWaveVector);
        procedure CreateHKLLists(
            const PatternAuxiliary: TPatternAuxiliary;
            const HKLList: THKLList;
            const AMinSinTL, AMaxSinTL: Double);
        procedure LinkWithHKLList(const HKLList: THKLList);

        property AtomCount: LongInt read GetAtomCount;

        property AtomList: TAtomList read GetAtomList;

        property WaveVectorList: TWaveVectorList read FWaveVectorList;

        property GeneratorClass: TGeneratorClass
            read GetGeneratorClass          write FGeneratorClass;

        property SiteModule: Double
            read FSiteModule                write SetSiteModule;

        property SiteName: string
            read FSiteName                  write SetSiteName;
        property NuclearScatAmpl: Double
            read FNuclearScatAmpl           write SetNuclearScatAmpl;
        property Element: string
            read FElement                   write SetElement;

        property ChemicalXNumber: LongInt
            read GetChemicalXNumber;
        property ChemicalYNumber: LongInt
            read GetChemicalYNumber;
        property ChemicalZNumber: LongInt
            read GetChemicalZNumber;

        property TransNumber: LongInt
            read GetTransNumber;

        property MagnAtomsNumber: LongInt
            read GetMagnAtomsNumber;

        property MagnAtom[index: LongInt]: TAtom
            read GetMagnAtom;

        property MagnAtomInChemical[index: LongInt]: TAtom
            read GetMagnAtomInChemical;

        property VariationMode: Byte
            read FVariationMode             write FVariationMode;
        property AllowedVariationModes: Byte
                read GetAllowedVariationModes;

        property ReprMode: Byte
            read FReprMode                  write FReprMode;
        property AllowedReprModes: Byte
                read GetAllowedReprModes;

        property A: Double read GetA;
        property B: Double read GetB;
        property C: Double read GetC;
        property Alpha: Double read GetAlpha;
        property Beta: Double read GetBeta;
        property Gamma: Double read GetGamma;
        property MagnA: Double read GetMagnA;
        property MagnB: Double read GetMagnB;
        property MagnC: Double read GetMagnC;
        property Disabled: Boolean read FDisabled write FDisabled;
        property NotMagnetic: Boolean read FNotMagnetic write FNotMagnetic;
    end;

    TCalcResults = class(TSelfCopiedComponent)
    protected
        FRFactor1: Double;
        FRFactor2: Double;

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
         procedure CopyParameters(const Dest: TObject); override;

         property RFactor1: Double read FRFactor1 write FRFactor1;
         property RFactor2: Double read FRFactor2 write FRFactor2;
     end;

    TSiteList = class(TObjSavingStringList)
    protected
        FCaption: string;
        FAl, FBt, FGm: Double;
        FA, FB, FC: Double;

        FPlotParams: TSelfCopiedComponent;
        FCalcResults: TSelfCopiedComponent;

        procedure LinkItemWithList(const Item: TComponent); override;

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
        destructor Destroy; override;
        function GetObjectIdentifier(const ObjNum: LongInt): string; override;
        function GetCopy: TObject; override;
        procedure CopyParameters(const Dest: TObject); override;
        procedure IsReady; override;

        function GetFullAtomsNumber: LongInt;
        function GetAtomInChemical(index: LongInt): TAtom;
        function GetWaveVectorsChainBegin: TWaveVector;
        function GetPlotParams: TSelfCopiedComponent;
        procedure SetPlotParams(const APlotParams: TSelfCopiedComponent);
        function GetCalcResults: TSelfCopiedComponent;
        procedure SetCalcResults(const ACalcResults: TSelfCopiedComponent);

        procedure SetAtomMomentsCur;
        procedure SetAtomMomentsAll;
        procedure ResetMixingConst;
        procedure MulModules(const MulConst: Double);
        procedure ResetSitesModules;
        procedure IncludeAllBasalFunctions;
        procedure ExcludeAllBasalFunctions;
        procedure CreateHKLLists(
            const PatternAuxiliary: TPatternAuxiliary;
            const HKLList: THKLList;
            const AMinSinTL, AMaxSinTL: Double);
        procedure LinkWithHKLList(const HKLList: THKLList);

        property A: Double read FA write FA;
        property B: Double read FB write FB;
        property C: Double read FC write FC;
        property Alpha: Double read FAl write FAl;
        property Beta: Double read FBt write FBt;
        property Gamma: Double read FGm write FGm;

        property Caption: string read FCaption write FCaption;
    end;

    TWaveVector = class(TDownhillRealParameters, ISelfCopied, ISelfChecked)
    protected
        FWaveVector: TDoubleVector3;
        FRotAxis: TDoubleVector3;
        FPropVectorType: Byte;
        FStarType: Byte;
        FTransTypeIndex: ShortInt;
        FPropType: ShortInt;
        FCurReprNum: LongInt;

        FNext: TWaveVector;
        FWaveVectorList: TWaveVectorList;
        FRepresentations: TRepresentationList;

        FSelfCheckingMode: LongInt;
        FSelfCopyingMode: LongInt;

        FWaveVectorServer: TSite;
        FIntensityFactors: IIntensityFactors;

        function GetAtomCount: LongInt;
        procedure SetBFVectorCount(const AVectorCount: LongInt);
        function GetWaveVector(index: LongInt): Double;
        function GetRotAxisType: ShortInt;
        function GetPropTypesNumber: LongInt;
        function GetWaveVectorServer: TSite;
        function GetIntensityFactors: IIntensityFactors;

        function GetAllowedRotAxis: Byte;
        procedure SetPropType(const APropType: ShortInt);
        procedure SetWaveVector(index: LongInt; const Value: Double);
        function GetRotAxis: TDoubleVector3;
        procedure SetRotAxis(const ARotAxis: TDoubleVector3);
        procedure SetAxisFromConst(const AxisType: Byte);
        function GetFirstAllowedAxis: Byte;
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

        procedure ParametersUpdated; override;
        procedure FillParameters; override;
        procedure CreateParameters; override;
        function GetActualParametersNumber: LongInt; override;
        function GetNumberOfValues: LongInt; override;
        function GetValueIndex: LongInt; override;
        procedure SetValueIndex(const AValueIndex: LongInt); override;
        function GetAllowedPropVectorTypes: Byte;
        procedure SetPropVectorType(const APropVectorType: Byte);
        procedure CheckPropVectorType;
        function GetAllowedStarTypes: Byte;
        procedure SetStarType(const AStarType: Byte);
        procedure CheckStarType;
        function GetAllowedTransTypes(index: LongInt): LongInt;
        function GetAllowedTransTypesNumber: LongInt;
        procedure SetTransTypeIndex(const ATransTypeIndex: ShortInt);
        procedure CheckTransTypeIndex;
        procedure CheckAxisType;
        procedure MultiplyVectorByDirectMatrix(
            const InVect: TDoubleVector3; var OutVect: TDoubleVector3);
        procedure MultiplyVectorByReverseMatrix(
        const InVect: TDoubleVector3; var OutVect: TDoubleVector3);

    public
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        procedure CreateRepresentations;
        procedure ReplaceRepresentations(
            const ARepresentations: TRepresentationList);

        procedure DeleteVector(const Index: LongInt);
        procedure InsertNewVector(const Index: LongInt);
        procedure AddNewVector;

        function CheckVector(const Vect: TDoubleVector3): Byte;
        procedure SetVectorProperly(const InVect: TDoubleVector3;
            var OutVect: TDoubleVector3);
        procedure DirectTransform(
            const InVect: TDoubleVector3;
            var OutVect: TDoubleVector3; var Unchangeables: Byte);
        procedure ReverseTransform(
            const InVect: TDoubleVector3;
            var OutVect: TDoubleVector3);

        procedure ResetCurrentRepNum;
        function IncCurrentRepNum: Boolean;
        procedure ResetPropType;
        function NextPropType: Boolean;
        procedure CreateHKLList(
            const PatternAuxiliary: TPatternAuxiliary;
            const ExtHKLList: THKLList;
            const AMinSinTL, AMaxSinTL: Double);
        procedure LinkWithHKLList(const ExtHKLList: THKLList);
        procedure CalcMagneticIntensity(
            const HKLClass: THKLClass; const StarLink: TStarLink);
        procedure CalcNuclearIntensity(const HKLClass: THKLClass);

        function GetCopy: TObject;
        procedure CopyParameters(const Dest: TObject); virtual;
        procedure IsReady;
        function MyNameIs: string;
        function GetSelfCheckingMode: LongInt;
        procedure SetSelfCheckingMode(const AMode: LongInt);

        function GetSelfCopyingMode: LongInt;
        procedure SetSelfCopyingMode(const AMode: LongInt);

        function IsPropVectMagnetic: Boolean;
        function IsPropVectStructural: Boolean;
        function AsString: string;
            function GetRotMatrix(
            const Translation: TDoubleVector3
            ): TMatrix;

        function GetModuleTransFactor(
            const Translation: TDoubleVector3;
            const Moment: TDoubleVector3;
            const EllipseParam: Double
            ): Double;

        procedure RotateMoment(
            const Translation: TDoubleVector3;
            var Moment: TDoubleVector3
            );

        function HasOnlyGivenComponents(const Component: Double): Boolean;
            function HasOnlyZeroComponents: Boolean;
            property WaveVectorList: TWaveVectorList
            read FWaveVectorList            write FWaveVectorList;

        property Representations: TRepresentationList
            read FRepresentations;

        property CurReprNum: LongInt
            read FCurReprNum                write FCurReprNum;

        property Next: TWaveVector
            read FNext                      write FNext;

        property WaveVector[index: LongInt]: Double
            read GetWaveVector              write SetWaveVector; default;

        property AtomCount: LongInt
            read GetAtomCount;

        property AllowedPropVectorTypes: Byte
            read GetAllowedPropVectorTypes;
        property PropVectorType: Byte
            read FPropVectorType            write SetPropVectorType;

        property AllowedTransTypes[index: LongInt]: LongInt
            read GetAllowedTransTypes;

        property AllowedTransTypesNumber: LongInt
            read GetAllowedTransTypesNumber;
        property TransTypeIndex: ShortInt
            read FTransTypeIndex            write SetTransTypeIndex;

        property PropTypesNumber: LongInt
            read GetPropTypesNumber;
        property PropType: ShortInt
            read FPropType                  write SetPropType;

        property AllowedRotAxis: Byte
            read GetAllowedRotAxis;
        property RotAxis: TDoubleVector3
            read GetRotAxis                 write SetRotAxis;
        property RotAxisType : ShortInt
            read GetRotAxisType;

        property AllowedStarTypes: Byte
            read GetAllowedStarTypes;

        property StarType: Byte
            read FStarType                  write SetStarType;

        property WaveVectorServer: TSite
            read GetWaveVectorServer        write FWaveVectorServer;
        property IntensityFactors: IIntensityFactors
            read GetIntensityFactors        write FIntensityFactors;
    end;

    TWaveVectorList = class(TObjSavingStringList)
    protected
        FSite: TSite;
        function GetAtomCount: LongInt;

        function GetMagnStar: TWaveVector;
        function GetStructStar: TWaveVector;

        procedure LinkItemWithList(const Item: TComponent); override;
        procedure SetSite(const ASite: TSite);

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
        function GetObjectIdentifier(const ObjNum: LongInt): string; override;
        procedure StarStateChanged(const Star: TWaveVector);
        procedure DeleteVector(const Index: LongInt);
        procedure InsertNewVector(const Index: LongInt);
        procedure AddNewVector;

        property Site: TSite
            read FSite                      write SetSite;

        property AtomCount: LongInt
            read GetAtomCount;
        property MagnStar: TWaveVector
            read GetMagnStar;
        property StructStar: TWaveVector
            read GetStructStar;
    end;

    TBasisFunctions = class(TSelfCopiedComponent)
    protected
        FBasisFunctions: array of TDoubleVector3;
        FBasisFunctionsIm: array of TDoubleVector3;
        FMixingConst: Double;
        FExcluded: Boolean;
        FDisabled: Boolean;

        procedure SetBasisFunction(Index: Integer; BasisFunction: TDoubleVector3);
        function GetBasisFunction(Index: Integer): TDoubleVector3;
        procedure SetBasisFunctionIm(Index: Integer; BasisFunction: TDoubleVector3);
        function GetBasisFunctionIm(Index: Integer): TDoubleVector3;
        procedure SetVectorsCount(ACount: LongInt);
        function GetVectorsCount: LongInt;

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
        destructor Destroy; override;
        procedure CopyParameters(const Dest: TObject); override;

        procedure InsertVector(const Index: LongInt;
            const ReVector, ImVector: TDoubleVector3);
        procedure DeleteVector(const Index: LongInt);
        procedure AddVector(const ReVector, ImVector: TDoubleVector3);
        procedure InsertNewVector(const Index: LongInt);
        procedure AddNewVector;

        function SaveModules: Boolean;
        function HasVacuity: Boolean;
        function IsInteger: Boolean;
        function GetVacuities: TDynamicLongArray;
        function GetFillings: TDynamicLongArray;
        function FillingsAndVacuitiesCoincide(const BF: TBasisFunctions): Boolean;
        property BasisFunction[index: Integer]: TDoubleVector3
            read GetBasisFunction           write SetBasisFunction; default;
        property BasisFunctionIm[index: Integer]: TDoubleVector3
            read GetBasisFunctionIm         write SetBasisFunctionIm;
        property VectorsCount: LongInt
            read GetVectorsCount            write SetVectorsCount;
        property MixingConst: Double
            read FMixingConst               write FMixingConst;

        property Excluded: Boolean
            read FExcluded                  write FExcluded;
        property Disabled: Boolean
            read FDisabled                      write FDisabled;
    end;

    TRepresentation = class(TColCompList, IGridDataSource)
    protected
        FRepresentations: TRepresentationList;
        FVectorCount: LongInt;

        procedure LinkItemWithList(const Item: TComponent); override;

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

        function CreateNewObject: TComponent; override;
        procedure SetVectorCount(const AVectorCount: LongInt);

        procedure _DeleteVector(const Index: LongInt);
        procedure _InsertNewVector(const Index: LongInt);
        procedure _AddNewVector;

    public
        procedure CopyParameters(const Dest: TObject); override;

        procedure SetColContents(Grid: TStringGrid; ColNum: LongInt); override;
        function GetColContents(Grid: TStringGrid; ColNum: LongInt): Boolean; override;
        procedure SetCaption(Grid: TStringGrid); override;
            procedure SetColOptions(Grid: TStringGrid); override;

        function ValueToString(const ACol, ARow: LongInt
            ): string; override;
        procedure StringToValue(const ACol, ARow: LongInt;
            const AString: string
            ); override;
        procedure SetValueByDefault(const ACol, ARow: LongInt); override;
            function IsDataValid(const ACol, ARow: LongInt;
            const AString: string): Boolean; override;
        function GetCellEnabledCharSet(
            const ACol, ARow: LongInt): TCharSet; override;

        function GetInfoRows: LongInt; override;

        function BFsInUseNumber: LongInt;
        procedure DeleteVector(const Index: LongInt);
        procedure InsertNewVector(const Index: LongInt);
        procedure AddNewVector;
        procedure RandMixFactors;
        procedure IsReady; override;
        function MyNameIs: string; override;

        property Representations: TRepresentationList
            read FRepresentations           write FRepresentations;
        property VectorCount: LongInt
            read FVectorCount               write SetVectorCount;
    end;

    TRepresentationList = class(TObjSavingStringList)
    protected
        FWaveVector: TComponent;
        FLinkItemDisabled: Boolean;

        function GetAtomCount: LongInt;

        procedure LinkItemWithList(const Item: TComponent); override;

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
        procedure CopyParameters(const Dest: TObject); override;    
        function GetObjectIdentifier(const ObjNum: LongInt): string; override;

        procedure DeleteVector(const Index: LongInt);
        procedure InsertNewVector(const Index: LongInt);
        procedure AddNewVector;

        procedure IsReady; override;
        procedure SendErrorMessage(
            const ExceptionMsg, ObjectName: string;
            const ItemNumber: LongInt); override;

        property  AtomCount: LongInt
            read GetAtomCount;
        property WaveVector: TComponent
            read  FWaveVector               write FWaveVector;
        property LinkItemDisabled: Boolean
            read FLinkItemDisabled          write FLinkItemDisabled;
    end;

    TPossibleModel = class(TComponent)
    protected
        SitesIndexes: array of LongInt;
        StructIndexes: array of LongInt;
        function GetSitesCount: LongInt;
        function GetSiteIndex(index: LongInt): LongInt;
        function GetStructIndex(index: LongInt): LongInt;

    public
        RFactor: Double;
        destructor Destroy; override;
        procedure AddStructToModel(const SiteIndex, StructIndex: LongInt);
        function HasThisSiteIndex(const SiteIndex: LongInt): Boolean;
        function GetCopy: TPossibleModel;
        function GetStructNum(const SiteNum: LongInt): LongInt;

        property SitesCount: LongInt
            read GetSitesCount;
        property SiteIndex[index: LongInt]: LongInt
            read GetSiteIndex;
        property StructIndex[index: LongInt]: LongInt
            read GetStructIndex;
    end;

    TPatternAuxiliary = class
    protected
        FA, FB, FC, FAlpha, FBeta, FGamma, FLambda: Double;
        Prepared: Boolean;

        CosArr: array[1..3] of Double;
        SinArr: array[1..3] of Double;
        P: array[1..3] of Double;
        D: Double;

        procedure SetA(const AA: Double);
        procedure SetB(const AB: Double);
        procedure SetC(const AC: Double);
        procedure SetAlpha(const AAlpha: Double);
        procedure SetBeta(const ABeta: Double);
        procedure SetGamma(const AGamma: Double);
        procedure SetLambda(const ALambda: Double);

        procedure CheckPrepared;
    public
        procedure PrepareToCalc;

        procedure GetUnitScatVect(
            const H, K, L: Double;
            var UnitVect: TDoubleVector3);
        function GetScatVectModule(
            const H, K, L : Double  ): Double;
        function GetSinTL(
            const H, K, L: Double
            ): Double;

        property A: Double      read FA         write SetA;
        property B: Double      read FB         write SetB;
        property C: Double      read FC         write SetC;
        property Alpha: Double  read FAlpha     write SetAlpha;
        property Beta: Double   read FBeta      write SetBeta;
        property Gamma: Double  read FGamma     write SetGamma;
        property Lambda: Double read FLambda    write SetLambda;
    end;

const TINY = 1e-6;

function ViewNeutronListSortFunc(Item1, Item2: Pointer): Integer;
function ViewPositionListSortFunc(Item1, Item2: Pointer): Integer;
function StructListSortFunc(Item1, Item2: Pointer): Integer;
function HKLListSortFunc(Item1, Item2: Pointer): Integer;

const
    ViewNeutronListSort: TListSortCompare = ViewNeutronListSortFunc;
    ViewPositionListSort: TListSortCompare = ViewPositionListSortFunc;
    StructListSort: TListSortCompare = StructListSortFunc;
    HKLListSort: TListSortCompare = HKLListSortFunc;

function CreateNewSite: TSite;
function CreateNewSiteList: TSiteList;
function CreateNewSinTCompList: TSinTCompList;
function CreateNewHKLClass: THKLClass;
function CreateNewAtom: TAtom;
function CreateNewAtomList: TAtomList;
function CreateNewBasisFunction: TBasisFunctions;
function CreateNewCommentsClass: TCommentsClass;
function CreateNewGeneralClass: TGeneralClass;
function CreateNewNeutronClass: TNeutronClass;
function CreateNewNeutronClassMono: TNeutronClassMono;
function CreateNewNeutronCompList: TNeutronCompList;
function CreateNewNeutronCompListMono: TNeutronCompListMono;
function CreateNewRepr: TRepresentation;
function CreateNewReprList: TRepresentationList;
function CreateNewWaveVector: TWaveVector;
function CreateNewWaveVectorList: TWaveVectorList;
function CreateNewSinTClass: TSinTClass;
function CreateNewPatternParams: TPatternParametersContainer;

implementation

uses FFDataFile, Plotter2;

type
    TA = class(TAtom)
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

    TAL = class(TAtomList)
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

    TBF = class(TBasisFunctions)
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

    TCC = class(TCommentsClass)
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

    TCR = class(TCalcResults)
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

    TGC = class(TGeneralClass)
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

    THKLC = class(THKLClass)
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

    TNC = class(TNeutronClass)
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

    TNCM = class(TNeutronClassMono)
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

    TNCL = class(TNeutronCompList)
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

    TNCLM = class(TNeutronCompListMono)
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

    TPPC = class(TPatternParametersContainer)
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

    TR = class(TRepresentation)
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

    TRL = class(TRepresentationList)
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

    TS = class(TSite)
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

    TSL = class(TSiteList)
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

    TSTC = class(TSinTClass)
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

    TSTCL = class(TSinTCompList)
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

    TWV = class(TWaveVector)
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

    TWVL = class(TWaveVectorList)
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

function ViewNeutronListSortFunc(Item1, Item2: Pointer): Integer;
var NR1: TSinTClass absolute Item1;
    NR2: TSinTClass absolute Item2;
begin
  if NR1.SinTL < NR2.SinTL then Result := -1 else
    if NR1.SinTL > NR2.SinTL then Result := 1 else Result := 0;
end;

function ViewPositionListSortFunc(Item1, Item2: Pointer): Integer;
var NR1: TPositionRec absolute Item1;
    NR2: TPositionRec absolute Item2;
begin
  if UpperCase(NR1.Position) < UpperCase(NR2.Position) then Result := -1 else
    if UpperCase(NR1.Position) > UpperCase(NR2.Position) then Result := 1
    else Result := 0;
end;

function StructListSortFunc(Item1, Item2: Pointer): Integer;
var NR1: TAtom absolute Item1;
    NR2: TAtom absolute Item2;
begin
    if NR1.Number < NR2.Number then Result := -1 else
        if NR1.Number > NR2.Number then Result := 1 else Result := 0;
end;

function HKLListSortFunc(Item1, Item2: Pointer): Integer;
var NR1: THKLClass absolute Item1;
    NR2: THKLClass absolute Item2;
begin
    if NR1.SinTL < NR2.SinTL then Result := -1 else
        if NR1.SinTL > NR2.SinTL then Result := 1 else Result := 0;
end;

procedure TAtom.RecalcUnitVect;
var TempVect: TDoubleVector3;
begin
  with GetSiteList do begin
    TempVect[1] := FMx; TempVect[2] := FMy; TempVect[3] := FMz;
    GetUnitVectA(TempVect, A, B, C, Alpha, Beta, Gamma, FUnitMagnVect);
  end;
end;

procedure TAtom.SetMx(AMx: Double);
begin
  FMx := AMx;
  DirectionChanged := True;
end;

procedure TAtom.SetMy(AMy: Double);
begin
  FMy := AMy;
  DirectionChanged := True;
end;

procedure TAtom.SetMz(AMz: Double);
begin
  FMz := AMz;
  DirectionChanged := True;  
end;

function TAtom.GetMx: Double;
begin
  Result := FMx;
end;

function TAtom.GetMy: Double;
begin
  Result := FMy;
end;

function TAtom.GetMz: Double;
begin
  Result := FMz;
end;

procedure TAtom.SetM(AM: Double);
begin
  FM := AM;
end;

function TAtom.GetUnitMagnVect: TDoubleVector3;
begin
  if DirectionChanged then begin
    RecalcUnitVect;
    DirectionChanged := False;
  end;
  Result := FUnitMagnVect;
end;

function TAtom.GetMoment: TDoubleVector3;
begin
  Result[1] := Mx; Result[2] := My; Result[3] := Mz;
end;

function TAtom.GetM: Double;
begin
  Result := FM;
end;

function TAtom.GetSiteList: TSiteList;
begin
  if Assigned(FSiteList) then Result := FSiteList
  else raise EAtom.Create('Sites list is not assigned...');
end;

function TAtom.GetSite: TSite;
begin
  if Assigned(FSite) then Result := FSite
  else raise EAtom.Create('Site is not assigned...');
end;

procedure TAtom.SetSite(const Site: TSite);
begin
  FSite := Site;
end;

procedure TAtom.SetSiteList(const SiteList: TSiteList);
begin
  FSiteList := SiteList;
end;

procedure TAtomList.SetCaption;
const
    CaptionsArr: array[0..11] of string = ('Num.', 'X', 'Y', 'Z',
    'Mx', 'My', 'Mz', 'Module', 'NSA', 'Element', 'Site', 'Ell. Param.');
var i: LongInt;
begin
    with Grid do
        if FixedRows <> 0 then
            for i := 0 to 11 do Cells[i, 0] := CaptionsArr[i];
end;

procedure TAtomList.LinkItemWithList(const Item: TComponent);
begin
    if not (csLoading in ComponentState) then
    begin
        TAtom(Item).SetSite(FSite);
        if Assigned(FSite) then
            with FSite do
            begin
                LinkAtomWithSiteList(TAtom(Item));

                TAtom(Item).Position := SiteName;
            end;
    end;
end;

procedure TAtomList.SetSite(const ASite: TSite);
begin
    FSite := ASite;
    LinkAllItemsWithList;
end;

procedure TAtomList.SetSiteList(const ASiteList: TSiteList);
var i: LongInt;
begin
    for i := 0 to Count - 1 do TAtom(Items[i]).SetSiteList(ASiteList);
end;

procedure TAtomList.SetColOptions(Grid: TStringGrid);
begin
    if Grid is TNumericGrid then
        with TNumericGrid(Grid) do
        begin
            ColOptions[1] := coReal; ColOptions[2] := coReal;
            ColOptions[3] := coReal; ColOptions[4] := coReal;
            ColOptions[5] := coReal; ColOptions[6] := coReal;
            ColOptions[7] := coReal; ColOptions[8] := coReal;
            ColOptions[9] := coText; ColOptions[10] := coText;
            ColOptions[11] := coDisabled;
        end;
end;

procedure StructCompListCellProcedure;
begin
end;

procedure TAtomList.SetColFunc(Grid: TStringGrid);
begin
    inherited SetColFunc(Grid);
    if Grid.FixedRows <> 0 then
        with Grid do
        begin
            Objects[4, 0] := StructCompListCellProc;
            Objects[5, 0] := StructCompListCellProc;
            Objects[6, 0] := StructCompListCellProc;
        end;
end;

procedure TAtomList.SetRowContents(Grid: TStringGrid; RowNum: LongInt);
var TA: TAtom;
    i: LongInt;
begin
    with Grid do
        if (RowNum - FixedRows >= 0) and (RowNum - FixedRows < Count) then
        begin
            TA := TAtom(Items[RowNum - FixedRows]);
            with TA do
            begin
                Cells[0, RowNum] := IntToStr(RowNum - FixedRows + 1);
                Cells[FixedCols, RowNum] := FloatToStrF(X, ffFixed, 5, 3);
                Cells[FixedCols + 1, RowNum] := FloatToStrF(Y, ffFixed, 5, 3);
                Cells[FixedCols + 2, RowNum] := FloatToStrF(Z, ffFixed, 5, 3);
                Cells[FixedCols + 3, RowNum] :=
                    FloatToStrF(UnitMagnVect[1], ffFixed, 5, 3);
                Cells[FixedCols + 4, RowNum] :=
                    FloatToStrF(UnitMagnVect[2], ffFixed, 5, 3);
                Cells[FixedCols + 5, RowNum] :=
                    FloatToStrF(UnitMagnVect[3], ffFixed, 5, 3);
                Cells[FixedCols + 6, RowNum] := FloatToStrF(M, ffFixed, 5, 3);
                Cells[FixedCols + 7, RowNum] :=
                    FloatToStrF(NuclearScatAmpl, ffGeneral, 6, 4);
                Cells[FixedCols + 8, RowNum] := Element;
                Cells[FixedCols + 9, RowNum] := Position;
                Cells[FixedCols + 10, RowNum] :=
                    FloatToStrF(EllipseParam, ffGeneral, 6, 4);
            end;
        end
        else
        begin
            Cells[0, RowNum] := IntToStr(RowNum - FixedRows + 1);
            for i := FixedCols to ColCount - 1 do Cells[i, RowNum] := '';
        end;
end;

procedure TAtomList.SetDataToGrid;
begin
    Sort(StructListSort);
    inherited;
end;

function TAtomList.GetRowContents(Grid: TStringGrid;
    RowNum: LongInt): Boolean;
var NC: TAtom;
    St: string;
begin
    with Grid do
        if (RowNum - FixedRows >= 0) and (RowNum - FixedRows < Count) then
        begin
            NC := TAtom(Items[RowNum - FixedRows]);
            with NC, Grid do
            begin
                St := Cells[FixedCols, RowNum]; X := StrToFloatDef(St, 0);
                St := Cells[FixedCols + 1, RowNum]; Y := StrToFloatDef(St, 0);
                St := Cells[FixedCols + 2, RowNum]; Z := StrToFloatDef(St, 0);
                St := Cells[FixedCols + 3, RowNum]; Mx := StrToFloatDef(St, 0);
                St := Cells[FixedCols + 4, RowNum]; My := StrToFloatDef(St, 0);
                St := Cells[FixedCols + 5, RowNum]; Mz := StrToFloatDef(St, 0);
                St := Cells[FixedCols + 6, RowNum]; M := StrToFloatDef(St, 0);
                St := Cells[FixedCols + 7, RowNum];
                NuclearScatAmpl := StrToFloatDef(St, 0);

                Element := Cells[FixedCols + 8, RowNum];
                Position := Cells[FixedCols + 9, RowNum];

                St := Cells[FixedCols + 10, RowNum];
                EllipseParam := StrToFloatDef(St, 0);

                Number := RowNum - FixedRows + 1;
            end;
        end;
    Result := True;
end;

procedure TAtomList.Delete(Index: Integer);
var Flag: Boolean;
begin
    Flag := Index <> Count - 1;
    inherited;
    if Flag then EnumerateAtoms;

    if (not (csLoading in ComponentState)) and Assigned(FSite) and
       (not IntControlled) then FSite.DeleteVector(Index);
end;

procedure TAtomList.Insert(Index: Integer; Item: TComponent);
begin
    if (not (csLoading in ComponentState)) and Assigned(FSite) then
        FSite.InsertNewVector(Index);
    inherited;
    EnumerateAtoms;
end;

function TAtomList.Add(Item: TComponent): Integer;
begin
    if (not (csLoading in ComponentState)) and Assigned(FSite) then
        FSite.AddNewVector;
    Result := inherited Add(Item);
    TAtom(Item).Number := Count;
end;

function TAtomList.CreateNewObject: TComponent;
begin
    Result := CreateNewAtom;
end;

procedure TNeutronCompList.SetCaption;
const CaptionsArr: array[0..5] of string =
    ('Num.', 'Intensity', 'Start Pos.', 'Peak Pos.',
    'Finish Pos.',  'Int. Corr.');
var i: LongInt;
begin
    with Grid do
        if FixedRows <> 0 then
            for i := 0 to 5 do Cells[i, 0] := CaptionsArr[i];
end;

procedure TNeutronCompList.SetColOptions(Grid: TStringGrid);
begin
    if Grid is TNumericGrid then
        with TNumericGrid(Grid) do
        begin
            ColOptions[1] := coReal; ColOptions[2] := coReal;
            ColOptions[3] := coReal; ColOptions[4] := coReal;
        end;
end;

procedure TNeutronCompList.SetRowContents(Grid: TStringGrid; RowNum: LongInt);
var NC: TNeutronClass;
    i: LongInt;
begin
    with Grid do
        if (RowNum - FixedRows >= 0) and (RowNum - FixedRows < Count) then
        begin
            NC := TNeutronClass(Items[RowNum - FixedRows]);
            with NC do
            begin
                Cells[0, RowNum] := IntToStr(RowNum);
                Cells[FixedCols, RowNum] :=
                    FloatToStrF(Intensity, ffGeneral, 8, 4);
                Cells[FixedCols + 1, RowNum] :=
                    FloatToStrF(StartPos, ffGeneral, 6, 4);
                Cells[FixedCols + 2, RowNum] :=
                    FloatToStrF(PeakPos, ffGeneral, 6, 4);
                Cells[FixedCols + 3, RowNum] :=
                    FloatToStrF(FinishPos, ffGeneral, 6, 4);
                Cells[FixedCols + 4, RowNum] :=
                    FloatToStrF(IntCorrFactor, ffGeneral, 6, 4);
            end;
        end
        else
        begin
            Cells[0, RowNum] := IntToStr(RowNum);
            for i := FixedCols to ColCount - 1 do Cells[i, RowNum] := '';
        end;
end;

function TNeutronCompList.GetRowContents;
var NC: TNeutronClass;
    St: string;
begin
    with Grid do
        if (RowNum - FixedRows >= 0) and (RowNum - FixedRows < Count) then
        begin
            NC := TNeutronClass(Items[RowNum - FixedRows]);
            with NC do
            begin
                St := Cells[FixedCols, RowNum];
                    Intensity := StrToFloatDef(St, 0);

                St := Cells[FixedCols + 1, RowNum];
                    StartPos := StrToFloatDef(St, 0);

                St := Cells[FixedCols + 2, RowNum];
                    PeakPos := StrToFloatDef(St, 0);

                St := Cells[FixedCols + 3, RowNum];
                    FinishPos := StrToFloatDef(St, 0);

                St := Cells[FixedCols + 4, RowNum];
                    IntCorrFactor := StrToFloatDef(St, 0);
            end;
        end;
    Result := True;
end;

constructor TNeutronCompList.Create(AOwner: TComponent);
begin
    inherited;
    FPatternParameters := CreateNewPatternParams;
end;

destructor TNeutronCompList.Destroy;
begin
    UtilizeObject(FPatternParameters);
    inherited;
end;

function TNeutronCompList.GetMaxNeutronSinTL(
    const PatternAuxiliary: TPatternAuxiliary): Double;
var i: LongInt;
    NC: TNeutronClass;
begin
    Result := 0;
    for i := 0 to Count - 1 do
    begin
        NC := TNeutronClass(Items[i]);
        if NC.FinishPos > Result then Result := NC.FinishPos;
    end;
end;

procedure TNeutronCompList.IsReady;
begin
    inherited;  if Count = 0 then
        raise ENeutronCompList.Create('The list of peaks is empty');
    PatternParameters.IsReady;
end;

procedure TNeutronCompList.SendErrorMessage(
    const ExceptionMsg, ObjectName: string;
    const ItemNumber: LongInt);
var Str: string;
begin
    Str := ExceptionMsg + ' in ' + ObjectName + ' ' + IntToStr(ItemNumber + 1);
    if MyNameIs <> '' then Str := Str + ' in ' + MyNameIs;
    raise ENeutronCompList.Create(Str);
end;

procedure TPatternParametersContainer.IsReady;
begin
    if Lambda = 0 then
        raise ENeutronCompList.Create('Neutron wavelength can''t be zero...');
    if UseStartEnd then
        if EndPos <= StartPos then
            raise ENeutronCompList.Create(
            '"End pos." can''t be less than "Start pos."...');
end;

function TPatternParametersContainer.MyNameIs: string;
begin
    Result := '';
end;

procedure TPatternParametersContainer.CopyParameters(const Dest: TObject);
begin
    inherited;
    TPatternParametersContainer(Dest).Lambda := Lambda;
    TPatternParametersContainer(Dest).StartPos := StartPos;
    TPatternParametersContainer(Dest).EndPos := EndPos;
    TPatternParametersContainer(Dest).UseStartEnd := UseStartEnd;
end;

function TPatternParametersContainer.GetStartPos: Double;
begin
    Result := FStartPos;
end;

procedure TPatternParametersContainer.SetStartPos(const AStartPos: Double);
begin
    FStartPos := AStartPos;
end;

function TPatternParametersContainer.GetEndPos: Double;
begin
    Result := FEndPos;
end;

procedure TPatternParametersContainer.SetEndPos(const AEndPos: Double);
begin
    FEndPos := AEndPos;
end;

function TPatternParametersContainer.GetLambda: Double;
begin
    Result := FLambda;
end;

procedure TPatternParametersContainer.SetLambda(const ALambda: Double);
begin
    FLambda := ALambda;
end;

function TPatternParametersContainer.GetUseStartEnd: Boolean;
begin
    Result := FUseStartEnd;
end;

procedure TPatternParametersContainer.SetUseStartEnd(const AUseStartEnd: Boolean);
begin
    FUseStartEnd := AUseStartEnd;
end;

procedure TNeutronCompList.CopyParameters(const Dest: TObject);
begin
    inherited;
    PatternParameters.CopyParameters(TNeutronCompList(Dest).PatternParameters);
end;

function TNeutronCompList.GetStartPos: Double;
begin
    Result := PatternParameters.StartPos;
end;

procedure TNeutronCompList.SetStartPos(const AStartPos: Double);
begin
    PatternParameters.StartPos := AStartPos;
end;

function TNeutronCompList.GetEndPos: Double;
begin
    Result := PatternParameters.EndPos;
end;

procedure TNeutronCompList.SetEndPos(const AEndPos: Double);
begin
    PatternParameters.EndPos := AEndPos;
end;

function TNeutronCompList.GetLambda: Double;
begin
    Result := PatternParameters.Lambda;
end;

procedure TNeutronCompList.SetLambda(const ALambda: Double);
begin
    PatternParameters.Lambda := ALambda;
end;

function TNeutronCompList.GetUseStartEnd: Boolean;
begin
    Result := PatternParameters.UseStartEnd;
end;

procedure TNeutronCompList.SetUseStartEnd(const AUseStartEnd: Boolean);
begin
    PatternParameters.UseStartEnd := AUseStartEnd;
end;

constructor TCommentsClass.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FList := TStringList.Create;
end;

destructor TCommentsClass.Destroy;
begin
    UtilizeObject(FList);
    inherited Destroy;
end;

constructor TSinTClass.Create(AOwner: TComponent);
begin
    inherited;
    FHKLList := CreateNewSelfCopiedCompList;
    IntCorrFactor := 1;
end;

destructor TSinTClass.Destroy;
begin
    UtilizeObject(FHKLList);
    inherited Destroy;
end;

procedure TSinTClass.CopyParameters(const Dest: TObject);
var i: LongInt;
    HC, HC2: THKLClass;
begin
    inherited;
    TSinTClass(Dest).SinT := SinT;
    TSinTClass(Dest).SinTL := SinTL;
    TSinTClass(Dest).Sin2T := Sin2T;
    TSinTClass(Dest).ExpIntensity := ExpIntensity;
    TSinTClass(Dest).Intensity := Intensity;
    TSinTClass(Dest).PureMagnInt := PureMagnInt;
    TSinTClass(Dest).PureNuclInt := PureNuclInt;
    TSinTClass(Dest).NuclearIntensity := NuclearIntensity;
    TSinTClass(Dest).RFactor := RFactor;
    TSinTClass(Dest).StartPos := StartPos;
    TSinTClass(Dest).FinishPos := FinishPos;
    TSinTClass(Dest).PeakPos := PeakPos;
    TSinTClass(Dest).IntCorrFactor := IntCorrFactor;

    HKLList.CopyParameters(TSinTClass(Dest).HKLList);
end;

procedure TSinTCompList.SetColOptions(Grid: TStringGrid);
var i: LongInt;
begin
    if Grid is TNumericGrid then
        with Grid as TNumericGrid do
            for i := FixedCols to ColCount - 1 do ColOptions[i] := coDisabled;
end;

procedure TSinTCompList.SetDataToGrid;
var i, j: LongInt;
    SC: TSinTClass;
    HC: THKLClass;
    CurrentRow: LongInt;
const
    CaptionsArr: array[0..4] of string = (
        'Sin(T)/L', 'HKL', 'Calc.Int.', 'Nuc.Int.', 'Q');
begin
    with Grid do
    begin
        CurrentRow := FixedRows;
        for i := 0 to Count - 1 do
        begin
            SC := TSinTClass(Items[i]);
            if SC.FullView then
                Cells[0, CurrentRow] := IntToStr(i + 1) + ' <<'
            else Cells[0, CurrentRow] := IntToStr(i + 1) + ' >>';
            Cells[FixedCols, CurrentRow] :=
                FloatToStrF(SC.SinTL, ffFixed, 6, 4);
            Cells[FixedCols + 1, CurrentRow] :=
                FloatToStrF(SC.ExpIntensity, ffFixed, 6, 4);
            Cells[FixedCols + 2, CurrentRow] :=
                FloatToStrF(SC.Intensity, ffFixed, 6, 2);
            Cells[FixedCols + 3, CurrentRow] :=
                FloatToStrF(SC.NuclearIntensity, ffFixed, 6, 2);
            Cells[FixedCols + 4, CurrentRow] :=
                FloatToStrF(SC.RFactor, ffFixed, 6, 2);

            if SC.FullView then
            begin
                Cells[0, CurrentRow + 1] := '';
                for j := 0 to 4 do
                    Cells[FixedCols + j, CurrentRow + 1] := CaptionsArr[j];
                for j := 0 to SC.HKLList.Count - 1 do
                begin
                    HC := THKLClass(SC.HKLList.Items[j]);
                    Cells[0, CurrentRow + 2 + j] := '';

                    Cells[FixedCols, CurrentRow + 2 + j] :=
                        FloatToStrF(HC.SinTL, ffFixed, 6, 4);

                    Cells[FixedCols + 1, CurrentRow + 2 + j] := '[' +
                        FloatToStrF(HC.WVH, ffGeneral, 6, 4) + '|' +
                        FloatToStrF(HC.WVK, ffGeneral, 6, 4) + '|' +
                        FloatToStrF(HC.WVL, ffGeneral, 6, 4) + ']';

                    Cells[FixedCols + 2, CurrentRow + 2 + j] :=
                        FloatToStrF(HC.Intensity, ffFixed, 6, 2);
                    Cells[FixedCols + 3, CurrentRow + 2 + j] :=
                        FloatToStrF(HC.NuclearIntensity, ffFixed, 6, 2);

                    Cells[FixedCols + 4, CurrentRow + 2 + j] :=
                        FloatToStrF(HC.MulConst, ffFixed, 2, 0);
                end;
                CurrentRow := CurrentRow + SC.HKLList.Count + 2;
            end
            else CurrentRow := CurrentRow + 1;
        end;
    end;
end;

procedure TSinTCompList.SetCaption;
const CaptionsArr: array[0..5] of string = (
    'Num.', 'Sin(T)/L', 'Exp.Int.', 'Calc.Int.',
    'Nuc.Int.', 'R - Factor');
var i: LongInt;
begin
    with Grid do
        if FixedRows <> 0 then
        begin
            Cells[0, 0] := CaptionsArr[0];
            for i := 1 to 5 do Cells[FixedCols + i - 1, 0] := CaptionsArr[i];
        end;
end;

procedure TSinTCompList.CopyParameters(const Dest: TObject);
begin
    inherited;
    TSinTCompList(Dest).Lambda := Lambda;
    TSinTCompList(Dest).MulIntParam := MulIntParam;
    TSinTCompList(Dest).DWParam := DWParam;
    TSinTCompList(Dest).IFType := IFType;
    TSinTCompList(Dest).A := A;
    TSinTCompList(Dest).B := B;
    TSinTCompList(Dest).C := C;
    TSinTCompList(Dest).Alpha := Alpha;
    TSinTCompList(Dest).Beta := Beta;
    TSinTCompList(Dest).Gamma := Gamma;

    if (GetCalcResults <> nil) and (TSinTCompList(Dest).GetCalcResults <> nil) then
        GetCalcResults.CopyParameters(TSinTCompList(Dest).GetCalcResults);
end;

function TSinTCompList.GetCalcResults: TSelfCopiedComponent;
begin
    Result := FCalcResults;
end;

procedure TSinTCompList.SetCalcResults(const ACalcResults: TSelfCopiedComponent);
begin
    UtilizeObject(FCalcResults);
    FCalcResults := ACalcResults;
end;

constructor TSinTCompList.Create;
var i: LongInt;
begin
    inherited Create(AOwner);
    MulIntParam := 1;
    IFType := IF_SINT_SIN2T;
end;

destructor TSinTCompList.Destroy;
begin
    UtilizeObject(FCalcResults);
    inherited;
end;

function TSinTCompList.GetMulIntParam: Double;
begin
    Result := FMulIntParam;
end;

procedure TSinTCompList.SetMulIntParam(const AMulIntParam: Double);
begin
    FMulIntParam := Abs(AMulIntParam);
end;

function TSinTCompList.GetDWParam: Double;
begin
    Result := FDWParam;
end;

procedure TSinTCompList.SetDWParam(const ADWParam: Double);
begin
    FDWParam := Abs(ADWParam);
end;

function TSinTCompList.GetIFType: ShortInt;
begin
    Result := FIFType;
end;

procedure TSinTCompList.SetIFType(const AIFType: ShortInt);
begin
    FIFType := AIFType;
end;

function TSinTCompList.GetIntegralFactor(const SinT, Sin2T: Double): Double;
begin
    case IFType of
        IF_SINT_SIN2T : Result := 1 / (SinT * Sin2T);
        IF_SQR_SIN2T : Result := 1 / Sqr(Sin2T);
    end;
end;

function TSinTCompList.GetDebyeWallerFactor(const SinTL: Double): Double;
begin
    Result := exp(-DWParam * Sqr(SinTL));
end;

procedure TSinTCompList.CalcResultingIntensities;
var i: LongInt;
    TS: TSinTClass;
begin
    for i := 0 to Count - 1 do
    begin
        TS := TSinTClass(Items[i]);
        with TS do
        begin
            Intensity := PureMagnInt * MulIntParam * IntCorrFactor;
                NuclearIntensity := PureNuclInt * MulIntParam;
        end;
    end;
end;

procedure TSinTCompList.CalcSummaryIntensities;
var i, j, k: LongInt;
    SR: TSinTClass;
    HR: THKLClass;
    TSS: TScattSphere;

    TempReFMagn, TempImFMagn : Double;
    VectorFRe: TDoubleVector3;
    VectorFIm: TDoubleVector3;

    TempReFNucl, TempImFNucl : Double;
begin
    for i := 0 to Count - 1 do
    begin
        SR := TSinTClass(Items[i]);
        SR.PureMagnInt := 0; SR.PureNuclInt := 0;

        for j := 0 to SR.HKLList.Count - 1 do
        begin
            HR := THKLClass(SR.HKLList.Items[j]);
            SR.PureMagnInt := SR.PureMagnInt + HR.Intensity;
            SR.PureNuclInt := SR.PureNuclInt + HR.NuclearIntensity;
        end;
    end; {for i := 0 to ASinTList.Count - 1 do...}
end;

function TSinTCompList.GetLambda: Double;
begin
    Result := FLambda;
end;

procedure TSinTCompList.SetLambda(const ALambda: Double);
begin
    FLambda := ALambda;
end;

procedure TGeneralClass.SetA(const AA: Double);
begin
    if Assigned(FStructurePointer) then FStructurePointer.A := AA;
end;

function TGeneralClass.GetA;
begin
    if Assigned(FStructurePointer) then Result := FStructurePointer.A;
end;

procedure TGeneralClass.SetB(const AB: Double);
begin
    if Assigned(FStructurePointer) then FStructurePointer.B := AB;
end;

function TGeneralClass.GetB;
begin
    if Assigned(FStructurePointer) then Result := FStructurePointer.B;
end;

procedure TGeneralClass.SetC(const AC: Double);
begin
    if Assigned(FStructurePointer) then FStructurePointer.C := AC;
end;

function TGeneralClass.GetC;
begin
    if Assigned(FStructurePointer) then Result := FStructurePointer.C;
end;

procedure TGeneralClass.SetAlpha(const AAlpha: Double);
begin
    if Assigned(FStructurePointer) then
        FStructurePointer.Alpha := (AAlpha / 180) * pi;
end;

function TGeneralClass.GetAlpha;
begin
    if Assigned(FStructurePointer) then Result := (FStructurePointer.Alpha / pi) * 180;
end;

procedure TGeneralClass.SetBeta(const ABeta: Double);
begin
    if Assigned(FStructurePointer) then
        FStructurePointer.Beta := (ABeta / 180) * pi;
end;

function TGeneralClass.GetBeta;
begin
    if Assigned(FStructurePointer) then Result := (FStructurePointer.Beta / pi) * 180;
end;

procedure TGeneralClass.SetGamma(const AGamma: Double);
begin
    if Assigned(FStructurePointer) then
        FStructurePointer.Gamma := (AGamma / 180) * pi;
end;

function TGeneralClass.GetGamma;
begin
    if Assigned(FStructurePointer) then Result := (FStructurePointer.Gamma / pi) * 180;
end;

procedure TGeneralClass.SetLambda(const ALambda: Double);
begin
    if Assigned(FPatternParamsPointer) then
        FPatternParamsPointer.Lambda := ALambda;
end;

function TGeneralClass.GetLambda;
begin
    if Assigned(FPatternParamsPointer) then
        Result := FPatternParamsPointer.Lambda;
end;

procedure TGeneralClass.SetStartPos(const AStartPos: Double);
begin
    if Assigned(FPatternParamsPointer) then
        FPatternParamsPointer.StartPos := AStartPos;
end;

function TGeneralClass.GetStartPos;
begin
    if Assigned(FPatternParamsPointer) then
        Result := FPatternParamsPointer.StartPos;
end;

procedure TGeneralClass.SetUseStartEnd(const AUseStartEnd: Boolean);
begin
    if Assigned(FPatternParamsPointer) then
        FPatternParamsPointer.UseStartEnd := AUseStartEnd;
end;

function TGeneralClass.GetUseStartEnd;
begin
    if Assigned(FPatternParamsPointer) then
        Result := FPatternParamsPointer.UseStartEnd;
end;

procedure TGeneralClass.SetEndPos(const AEndPos: Double);
begin
    if Assigned(FPatternParamsPointer) then
        FPatternParamsPointer.EndPos := AEndPos;
end;

function TGeneralClass.GetEndPos;
begin
    if Assigned(FPatternParamsPointer) then
        Result := FPatternParamsPointer.EndPos;
end;

const Decimals: Integer = 4;
procedure TAtomList.SetRandomMoments(Site: string; Range: Double;
    EntirelyAtSite: Boolean);
    
  function GetRandomSign: Integer;
  var TempDouble: Double;
    begin
    TempDouble := Random;
    if TempDouble > 0.5 then Result := 1
    else Result := -1;
  end;

  function GetRandomValue: Double;
  var TempDouble: Double;
  begin
    TempDouble := Range * Random * GetRandomSign;
    Result := WithGivenAccuracy(TempDouble, Decimals);
  end;

var i, j: LongInt;
    SC: TAtom;
    PR: TPositionRec;
    TempPosList: TSelfCleanList;
    TempMx, TempMy, TempMz: Double;
begin
  Randomize;
  if EntirelyAtSite then begin
    TempPosList := TSelfCleanList.Create;
    SortPosition(TempPosList);
    for i := 0 to TempPosList.Count - 1 do begin
      PR := TPositionRec(TempPosList.Items[i]);
      if (UpperCase(PR.Position) = UpperCase(Site)) or
      ((UpperCase(Site) = 'ALL') and (PR.Position <> '')) then
      begin
        TempMx := GetRandomValue;
        TempMy := GetRandomValue;
        TempMz := GetRandomValue;
        for j := 0 to PR.AtomList.Count - 1 do begin
         SC := TAtom(PR.AtomList.Items[j]);
         SC.Mx := TempMx;
         SC.My := TempMy;
         SC.Mz := TempMz;
        end;
      end;
    end; {for i := 0 to TempPosList.Count - 1 do...}
    TempPosList.ClearAll;
    UtilizeObject(TempPosList);
  end
  else
  begin
    for i := 0 to Count - 1 do begin
      SC := TAtom(Items[i]);
      with SC do begin
        if (UpperCase(SC.Position) = UpperCase(Site)) or
        ((UpperCase(Site) = 'ALL') and (SC.Position <> '')) then
        begin
          Mx := GetRandomValue;
          My := GetRandomValue;
          Mz := GetRandomValue;
        end
      end;
    end;
  end;
end;

procedure TAtomList.SetModuleAtSite(Site: string; Module: Double);
var i: LongInt;
    SC: TAtom;
begin
  for i := 0 to Count - 1 do
  begin
    SC := TAtom(Items[i]);
    if (UpperCase(SC.Position) = UpperCase(Site)) or
    ((UpperCase(Site) = 'ALL') and (SC.Position <> '')) then SC.M := Module;
  end;
end;

procedure TAtomList.MulModuleAtSite(Site: string; MulConst: Double);
var i: LongInt;
    SC: TAtom;
begin
  for i := 0 to Count - 1 do
  begin
    SC := TAtom(Items[i]);
    if (UpperCase(SC.Position) = UpperCase(Site)) or
    ((UpperCase(Site) = 'ALL') and (SC.Position <> '')) then
      SC.M := SC.M * MulConst;
  end;
end;

procedure TAtomList.SetMeanModuleAtSite(Site: string);
var i: LongInt;
    SC: TAtom;
    MeanModule: Double;
begin
  MeanModule := GetMeanModuleAtSite(Site);
  if MeanModule <> 0 then
  begin
    for i := 0 to Count - 1 do
    begin
      SC := TAtom(Items[i]);
      if (UpperCase(SC.Position) = UpperCase(Site)) or
      ((UpperCase(Site) = 'ALL') and (SC.Position <> '')) then
      SC.M := MeanModule;
    end;
  end;
end;

function TAtomList.GetMeanModuleAtSite(Site: string): Double;
var i: LongInt;
    SC: TAtom;
    MeanModule: Double;
    AtomCount: LongInt;
begin
  MeanModule := 0;
  AtomCount := 0;
  for i := 0 to Count - 1 do
  begin
    SC := TAtom(Items[i]);
    if (UpperCase(SC.Position) = UpperCase(Site)) or
    ((UpperCase(Site) = 'ALL') and (SC.Position <> '')) then
    begin
      Inc(AtomCount);
      MeanModule := MeanModule + SC.M;
    end;
  end;
  if AtomCount <> 0 then MeanModule := MeanModule / AtomCount
  else MeanModule := 0;
  Result := MeanModule;
end;

constructor THKLClass.Create(AOwner: TComponent);
begin
    inherited;
    StarLinks := TSelfCleanList.Create;
end;

destructor THKLClass.Destroy;
begin
    StarLinks.ClearAll;
    UtilizeObject(StarLinks);
    inherited;
end;

procedure THKLClass.AddStarLink(const StarLink: TStarLink);
begin
    StarLinks.Add(StarLink);
end;

procedure THKLClass.ClearStarLinks;
begin
    StarLinks.Clear;
end;

function THKLClass.GetStarLinksNumber: LongInt;
begin
    Result := StarLinks.Count;
end;

procedure THKLClass.CalcNuclearIntensity;
var i: LongInt;
    SL: TStarLink;
begin
    NuclearIntensity := 0;
    NuclearReF := 0; NuclearImF := 0;
    for i := 0 to StarLinks.Count - 1 do
    begin
        SL := TStarLink(StarLinks.Items[i]);
        SL.Star.CalcNuclearIntensity(Self);
    end;
end;

procedure THKLClass.CalcMagneticIntensity;
var i: LongInt;
    SL: TStarLink;
begin
    Intensity := 0;
    VectorFRe[1] := 0; VectorFRe[2] := 0; VectorFRe[3] := 0;
    VectorFIm[1] := 0; VectorFIm[2] := 0; VectorFIm[3] := 0;
    ImF := 0; ReF := 0;
     
    for i := 0 to StarLinks.Count - 1 do
    begin
        SL := TStarLink(StarLinks.Items[i]);
        SL.Star.CalcMagneticIntensity(Self, SL);
    end;
end;

procedure THKLClass.CopyParameters(const Dest: TObject);
begin
    inherited;
    THKLClass(Dest).FIntensity := FIntensity;
    THKLClass(Dest).FNuclearIntensity := FNuclearIntensity;
    THKLClass(Dest).FMulConst := FMulConst;
    THKLClass(Dest).FSinT := FSinT;
    THKLClass(Dest).FSin2T := FSin2T;
    THKLClass(Dest).FSinTL := FSinTL;
    THKLClass(Dest).FReF := FReF;
    THKLClass(Dest).FImF := FImF;
    THKLClass(Dest).UnitScatVect := UnitScatVect;
    THKLClass(Dest).WVH := WVH;
    THKLClass(Dest).WVK := WVK;
    THKLClass(Dest).WVL := WVL;
end;

procedure TAtomList.SortPosition;
var i, j: LongInt;
    SR: TAtom;
    PR: TPositionRec;
    Index: LongInt;
begin
  if PL is TSelfCleanList then
    with PL as TSelfCleanList do ClearAll
  else PL.Clear;
  for i := 0 to Count - 1 do begin
    SR := TAtom(Items[i]);
    Index := -1;
    for j := 0 to PL.Count - 1 do begin
      PR := PL.Items[j];
      if UpperCase(PR.Position) = UpperCase(SR.Position) then
      begin Index := j; Break end;
    end;
    if Index <> -1 then begin
      PR := PL.Items[Index];
      PR.AtomList.Add(SR)
    end else begin
      PR := TPositionRec.Create;
      PR.AtomList := TSelfCleanList.Create;
      PR.AtomList.Add(SR);
      PL.Add(PR);
      PR.Position := SR.Position;
    end;
  end;
end;

function TAtomList.GetCopyAsSiteList: TSiteList;
var i, j: LongInt;
    SR: TAtom;
    TS: TSite;
    Index: LongInt;
begin
  Result := CreateNewSiteList;
  for i := 0 to Count - 1 do
  begin
    SR := TAtom(Items[i]);
    Index := -1;
    for j := 0 to Result.Count - 1 do
    begin
      TS := TSite(Result.Items[j]);
      if UpperCase(TS.SiteName) = UpperCase(SR.Position) then
      begin Index := j; Break end;
    end;{for j := 0 to Result.Count - 1 do...}
    if Index <> -1 then
    begin
      TS := TSite(Result.Items[Index]);
      TS.AtomList.Add(TComponent(SR.GetCopy))
    end
    else
    begin
      TS := CreateNewSite;
      TS.AtomList.Add(TComponent(SR.GetCopy));
      Result.Add(TS);
      TS.SiteName := SR.Position;
      if TS.SiteName = '' then TS.NotMagnetic := True;  end;
  end;{for i := 0 to Count - 1 do...}
end;

constructor TPositionRec.Create;
begin
  AtomList := nil;
end;

destructor TPositionRec.Destroy;
begin
    UtilizeObject(AtomList);
end;

constructor TAtom.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  EllipseParam := 1;
end;

procedure TAtom.CopyParameters(const Dest: TObject);
begin
  inherited;
  TAtom(Dest).x := x;
  TAtom(Dest).y := y;
  TAtom(Dest).z := z;
  TAtom(Dest).Mx := Mx;
  TAtom(Dest).My := My;
  TAtom(Dest).Mz := Mz;
  TAtom(Dest).M := M;
  TAtom(Dest).NuclearScatAmpl := NuclearScatAmpl;
  TAtom(Dest).Element := Element;
  TAtom(Dest).Position := Position;
  TAtom(Dest).Number := Number;
  TAtom(Dest).DispX := DispX;
  TAtom(Dest).DispY := DispY;
  TAtom(Dest).DispZ := DispZ;
  TAtom(Dest).EllipseParam := EllipseParam;
end;

procedure TAtom.IsReady;
begin
    GetSiteList;
    if not GetSite.NotMagnetic then
        if SearchElement(Element) = nil then
            raise EAtom.Create('Unknown element "' + Element + '"...');
end;

function TAtom.MyNameIs: string;
begin
    Result := 'the atom ' + IntToStr(Number);
end;

procedure TNeutronClass.IsReady;
begin
    if (PeakPos = 0) or (StartPos = 0) or (FinishPos = 0) then
        raise ENeutronClass.Create('The peak characteristic point has zero value');
    if StartPos > PeakPos then
        raise ENeutronClass.Create('"Start pos." mustn''t be more than "Peak pos."');
    if FinishPos < PeakPos then
        raise ENeutronClass.Create('"Finish pos." mustn''t be less than "Peak pos."');
end;

function TNeutronClass.MyNameIs: string;
begin
    Result := 'the peak';
end;

constructor TNeutronClass.Create;
begin
    inherited;
    IntCorrFactor := 1;
end;

procedure TNeutronClass.CopyParameters(const Dest: TObject);
begin
    inherited;
    TNeutronClass(Dest).Intensity := Intensity;
    TNeutronClass(Dest).StartPos := StartPos;
    TNeutronClass(Dest).FinishPos := FinishPos;
    TNeutronClass(Dest).PeakPos := PeakPos;
    TNeutronClass(Dest).IntCorrFactor := IntCorrFactor;
end;

function TWaveVector.GetActualParametersNumber: LongInt;
begin
    if AllowedTransTypesNumber <> 0 then
    begin
        if TransTypeIndex <> -1 then
        begin
            case AllowedTransTypes[TransTypeIndex] of
                TT_SS, TT_LSW : Result := 0;    TT_ES : Result := WaveVectorServer.AtomList.Count;
                    TT_CS : raise EWaveVector.Create('Feature doesn''t realized...');
                else raise EWaveVector.Create('Unknown type of transformation...')
            end;
        end
        else
            raise EWaveVector.Create('Transformation type must be defined...');
    end else Result := 0;
end;

procedure TWaveVector.FillParameters;
var i: LongInt;
    TA: TAtom;
    TempParameter: TVariableParameter;
begin
    if AllowedTransTypesNumber <> 0 then
    begin
        if TransTypeIndex <> -1 then
        begin
            if AllowedTransTypes[TransTypeIndex] = TT_ES then
                with WaveVectorServer.AtomList do
                    for i := 0 to Count - 1 do
                    begin
                        TA := TAtom(Items[i]);

                        TempParameter.Value := TA.EllipseParam;
                        TempParameter.Limited := True;
                        TempParameter.MaxLimit := 1;
                        TempParameter.MinLimit := 0;
                        Parameter[i] := TempParameter;
                    end
        end
        else
            raise EWaveVector.Create('Transformation type must be defined...');
    end
end;

procedure TWaveVector.ParametersUpdated;
var i: LongInt;
    TA: TAtom;
begin
    if AllowedTransTypesNumber <> 0 then
    begin
        if TransTypeIndex <> -1 then
        begin
            if AllowedTransTypes[TransTypeIndex] = TT_ES then
                with WaveVectorServer.AtomList do
                    for i := 0 to Count - 1 do
                    begin
                        TA := TAtom(Items[i]);

                        TA.EllipseParam := Abs(Parameter[i].Value);
                    end;
        end
        else
            raise EWaveVector.Create('Transformation type must be defined...');
    end
end;

function TWaveVector.GetNumberOfValues: LongInt;
begin
    if Representations.Count <> 0 then
        Result := Representations.Count
    else Result := 1;
end;

function TWaveVector.GetValueIndex: LongInt;
begin
    Result := CurReprNum;
end;

procedure TWaveVector.SetValueIndex(const AValueIndex: LongInt);
var TR: TRepresentation;
begin
    CurReprNum := AValueIndex;
    TR := TRepresentation(Representations.Items[CurReprNum]);
    TR.RandMixFactors; 
end;

function TWaveVector.HasOnlyGivenComponents(const Component: Double): Boolean;
var i: LongInt;
    OnlyZero: Boolean;
const TINY = 1e-6;
begin
  Result := True; OnlyZero := True;
  for i := 1 to 3 do begin
    if (Abs(WaveVector[i]) > TINY) and
       (Abs(Abs(WaveVector[i]) - Component) > TINY) then
    begin Result := False; Exit end;
    if (Abs(WaveVector[i]) > TINY) then OnlyZero := False;
  end;
  if OnlyZero then Result := False;
end;

function TWaveVector.HasOnlyZeroComponents: Boolean;
var i: LongInt;
begin
  Result := True;
  for i := 1 to 3 do
    if Abs(WaveVector[i]) > TINY then
    begin Result := False; Exit end;
end;

function TWaveVector.GetPropTypesNumber: LongInt;
begin
  raise EWaveVector.Create('Method does not realized...');
end;

procedure TWaveVector.SetPropType(const APropType: ShortInt);
begin
  FPropType := APropType;
  raise EWaveVector.Create('Method does not realized...');
end;

function TWaveVector.GetCopy: TObject;
begin
  Result := CreateNewWaveVector;
  CopyParameters(Result);
end;

procedure TWaveVector.CopyParameters(const Dest: TObject);
begin
    TWaveVector(Dest)[1] := WaveVector[1];
    TWaveVector(Dest)[2] := WaveVector[2];
    TWaveVector(Dest)[3] := WaveVector[3];
    TWaveVector(Dest).PropVectorType := PropVectorType;
    TWaveVector(Dest).StarType := StarType;
    TWaveVector(Dest).TransTypeIndex := TransTypeIndex;
    TWaveVector(Dest).RotAxis := RotAxis;

    case FSelfCopyingMode of
        SSCOPY_DEFAULT, SSCOPY_WITH_REPR :
            Representations.CopyParameters(TWaveVector(Dest).Representations);
        SSCOPY_WITHOUT_REPR : begin end;
    end;
end;

procedure TWaveVector.ReplaceRepresentations(
const ARepresentations: TRepresentationList);
begin
    UtilizeObject(FRepresentations);
    FRepresentations := ARepresentations;
    FRepresentations.WaveVector := Self;
end;

procedure TWaveVector.ResetCurrentRepNum;
begin
  {$B-}
  if IsPropVectMagnetic and Assigned(WaveVectorList) and
  Assigned(WaveVectorList.Site) and not WaveVectorList.Site.NotMagnetic and
  (Representations.Count > 0) then CurReprNum := 0 else CurReprNum := -1;

  if Assigned(Next) then Next.ResetCurrentRepNum;
  {$B+}
end;

procedure TWaveVector.ResetPropType;
begin
  {$B-}
  if IsPropVectMagnetic and Assigned(WaveVectorList) and
  Assigned(WaveVectorList.Site) and not WaveVectorList.Site.NotMagnetic and
  (PropTypesNumber > 0) then PropType := 0 else PropType := -1;

  if Assigned(Next) then Next.ResetPropType;
  {$B+}
end;

procedure TWaveVector.CalcNuclearIntensity(const HKLClass: THKLClass);
var j: LongInt;
    SR: TAtom;
begin
    if IsPropVectStructural then
    begin
        with WaveVectorServer do begin
            for j := 0 to AtomList.Count - 1 do
            begin
                SR := TAtom(AtomList.Items[j]);
                with SR, HKLClass do begin
                    NuclearReF := NuclearReF + NuclearScatAmpl * Cos(2 * pi *
                    (WVH * X + WVK * Y + WVL * Z));

                    NuclearImF := NuclearImF + NuclearScatAmpl * Sin(2 * pi *
                    (WVH * X + WVK * Y + WVL * Z));
                end;{with SR, HKLClass do...}
            end;{for j := 0 to SL.Count - 1 do...}
        end;{with WaveVectorServer do...}

        with HKLClass, IntensityFactors do
            NuclearIntensity := (Sqr(NuclearReF) + Sqr(NuclearImF)) * MulConst
            * GetDebyeWallerFactor(SinTL) * GetIntegralFactor(SinT, Sin2T);
    end;{if IsPropVectStructural then...}
end;

procedure TWaveVector.CalcMagneticIntensity(
    const HKLClass: THKLClass;
    const StarLink: TStarLink
    );
var SR: TAtom;
    j, ck: LongInt;
    InterVect, InterVect2: TDoubleVector3;
    R: Double;
    ATot, BTot: Double;
    FormFactor: Double;
    Flag: Boolean;
    m, m1, m2: TDoubleVector3;
    TempVect: TDoubleVector3;
    Angle: Double;
    TempCos, TempSin: Double;

const TINY = 1e-6;
begin
  if IsPropVectMagnetic then begin
    with WaveVectorServer do begin
      Flag := False;
      for j := 0 to AtomList.Count - 1 do
        begin
        SR := TAtom(AtomList.Items[j]);
        with SR, HKLClass do begin
            TempCos := Cos(2 * Pi * (WVH * X + WVK * Y + WVL * Z));
            TempSin := Sin(2 * Pi * (WVH * X + WVK * Y + WVL * Z));
        end;

        if not Flag then begin
            FormFactor := GetFFValue(HKLClass.SinTL, SR);
          Flag := True;
        end;

        if (AllowedTransTypesNumber <> 0) then
            if TransTypeIndex <> -1 then
          begin
            case AllowedTransTypes[TransTypeIndex] of
              TT_SS : begin
                GetUnitVectA(RotAxis, A, B, C, Alpha, Beta, Gamma, m);
                if RotAxisType = RA_NONE then
                  raise EWaveVector.Create('Transfomation axis must be defined...');
                Angle := GetAngle(m, SR.Moment, A, B, C, Alpha, Beta, Gamma);

                with HKLClass do
                  if (Abs(Frac(WVH)) < TINY) and (Abs(Frac(WVK)) < TINY) and (Abs(Frac(WVL)) < TINY) then
                  begin
                    InterVect := GetSubVect(m, MulVectByValue(UnitScatVect,
                    GetScalarMulA(UnitScatVect, m, A, B, C, Alpha, Beta, Gamma)));
                    with SR do
                     begin
                       R := PROP_FACTOR * M * Cos(Angle) * FormFactor;

                       for ck := 1 to 3 do begin
                         VectorFRe[ck] := VectorFRe[ck] + R * InterVect[ck] * TempCos;
                         VectorFIm[ck] := VectorFIm[ck] + R * InterVect[ck] * TempSin;
                       end;
                    end;
                  end
                  else
                  begin
                    TempVect := GetSubVect(SR.Moment, MulVectByValue(m,
                    GetScalarMulA(SR.Moment, m, A, B, C, Alpha, Beta, Gamma)));

                    GetUnitVectA(TempVect, A, B, C, Alpha, Beta, Gamma, m1);

                    m2 := GetVectorMulA(m, m1, A, B, C, Alpha, Beta, Gamma);

                    with SR, StarLink do
                    begin
                      R := PROP_FACTOR * 0.5 * M * Sin(Angle) * FormFactor;

                      InterVect := GetSubVect(m1, MulVectByValue(UnitScatVect,
                      GetScalarMulA(UnitScatVect, m1, A, B, C, Alpha, Beta, Gamma)));

                      InterVect2 := GetSubVect(m2, MulVectByValue(UnitScatVect,
                      GetScalarMulA(UnitScatVect, m2, A, B, C, Alpha, Beta, Gamma)));

                      if (Abs(WVH - WaveVector[1] - StarLink.H) < TINY) and
                         (Abs(WVK - WaveVector[2] - StarLink.K) < TINY) and
                         (Abs(WVL - WaveVector[3] - StarLink.L) < TINY) then
                        for ck := 1 to 3 do begin
                          VectorFRe[ck] := VectorFRe[ck] + R * (
                          InterVect[ck] * TempCos - InterVect2[ck] * TempSin
                          );

                          VectorFIm[ck] := VectorFIm[ck] + R * (
                          InterVect[ck] * TempSin + InterVect2[ck] * TempCos
                          );
                        end;

                      if (Abs(WVH + WaveVector[1] - StarLink.H) < TINY) and
                         (Abs(WVK + WaveVector[2] - StarLink.K) < TINY) and
                         (Abs(WVL + WaveVector[3] - StarLink.L) < TINY) then
                        for ck := 1 to 3 do begin
                          VectorFRe[ck] := VectorFRe[ck] + R * (
                          InterVect[ck] * TempCos + InterVect2[ck] * TempSin
                          );

                          VectorFIm[ck] := VectorFIm[ck] + R * (
                          InterVect2[ck] * TempCos - InterVect[ck] * TempSin
                          );
                        end;
                    end;
                  end;
              end;{TT_SS...}

              TT_LSW : begin
                with SR, HKLClass do
                begin
                  InterVect := GetSubVect(UnitMagnVect, MulVectByValue(
                  UnitScatVect, GetScalarMulA(UnitScatVect,
                  UnitMagnVect, A, B, C, Alpha, Beta, Gamma)));
                  R := PROP_FACTOR * 0.5 * M * FormFactor;

                  for ck := 1 to 3 do begin
                    VectorFRe[ck] := VectorFRe[ck] + R * InterVect[ck] * TempCos;
                    VectorFIm[ck] := VectorFIm[ck] + R * InterVect[ck] * TempSin;
                  end;
                end;
              end;{TT_LSW...}

              TT_ES : begin
                GetUnitVectA(RotAxis, A, B, C, Alpha, Beta, Gamma, m);
                
                m1 := SR.UnitMagnVect;
                
                m2 := GetVectorMulA(m, m1, A, B, C, Alpha, Beta, Gamma);

                with SR, HKLClass do
                begin
                  R := PROP_FACTOR * 0.5 * M * FormFactor;

                  InterVect := GetSubVect(m1, MulVectByValue(UnitScatVect,
                  GetScalarMulA(UnitScatVect, m1, A, B, C, Alpha, Beta, Gamma)));

                  InterVect2 := GetSubVect(m2, MulVectByValue(UnitScatVect,
                  GetScalarMulA(UnitScatVect, m2, A, B, C, Alpha, Beta, Gamma)));

                  if (Abs(WVH - WaveVector[1] - StarLink.H) < TINY) and
                     (Abs(WVK - WaveVector[2] - StarLink.K) < TINY) and
                     (Abs(WVL - WaveVector[3] - StarLink.L) < TINY) then
                    for ck := 1 to 3 do begin
                      VectorFRe[ck] := VectorFRe[ck] + R * (
                      InterVect[ck] * TempCos -
                      InterVect2[ck] * EllipseParam * TempSin
                      );

                      VectorFIm[ck] := VectorFIm[ck] + R * (
                      InterVect[ck] * TempSin +
                      InterVect2[ck] * EllipseParam * TempCos
                      );
                    end;

                  if (Abs(WVH + WaveVector[1] - StarLink.H) < TINY) and
                     (Abs(WVK + WaveVector[2] - StarLink.K) < TINY) and
                     (Abs(WVL + WaveVector[3] - StarLink.L) < TINY) then

                    for ck := 1 to 3 do begin
                      VectorFRe[ck] := VectorFRe[ck] + R * (
                      InterVect[ck] * TempCos +
                      InterVect2[ck] * EllipseParam * TempSin
                      );

                      VectorFIm[ck] := VectorFIm[ck] + R * (
                      InterVect2[ck] * EllipseParam * TempCos -
                      InterVect[ck] * TempSin
                      );
                    end;
                end;
              end;{TT_ES...}
            end
          end{if TransTypeIndex <> -1 then...} else
            raise EWaveVector.Create('Transformation type must be defined...')
        else
        begin
          with SR, HKLClass do
          begin
            InterVect := GetSubVect(UnitMagnVect, MulVectByValue(UnitScatVect,
            GetScalarMulA(UnitScatVect, UnitMagnVect, A, B, C, Alpha, Beta, Gamma)));
            R := PROP_FACTOR * M * FormFactor;

            for ck := 1 to 3 do begin
              VectorFRe[ck] := VectorFRe[ck] + R * InterVect[ck] * TempCos;
              VectorFIm[ck] := VectorFIm[ck] + R * InterVect[ck] * TempSin;
            end;
          end;
        end;{else...}
      end;{for j := 0 to AtomList.Count - 1 do...}

    with HKLClass, IntensityFactors do begin
        ATot := GetScalarMulA(VectorFRe, VectorFRe, A, B, C, Alpha, Beta, Gamma);
        BTot := GetScalarMulA(VectorFIm, VectorFIm, A, B, C, Alpha, Beta, Gamma);

        ReF := ATot * GetIntegralFactor(SinT, Sin2T) *
        GetDebyeWallerFactor(SinTL) * MulConst;
        ImF := BTot * GetIntegralFactor(SinT, Sin2T) *
        GetDebyeWallerFactor(SinTL) * MulConst;
        Intensity := ReF + ImF;
      end;
    end;{with WaveVectorServer do...}
  end;{if IsPropVectMagnetic then...}
end;

function TWaveVector.IncCurrentRepNum: Boolean;

  function __IncCurrentRepNumber: Boolean;
  begin
    {$B-}
    if IsPropVectMagnetic and Assigned(WaveVectorList) and
    Assigned(WaveVectorList.Site) and not WaveVectorList.Site.NotMagnetic and
    (Representations.Count > 0) then
    begin
      CurReprNum := CurReprNum + 1;
      if CurReprNum = Representations.Count then
      begin CurReprNum := 0; Result := True end else Result := False;
    end else Result := True
    {$B+}
  end;

begin
  if Assigned(Next) then
    if Next.IncCurrentRepNum then
      Result := __IncCurrentRepNumber else Result := False
  else Result := __IncCurrentRepNumber;
end;

function TWaveVector.NextPropType: Boolean;

  function __NextPropType: Boolean;
  begin
    {$B-}
    if IsPropVectMagnetic and Assigned(WaveVectorList) and
    Assigned(WaveVectorList.Site) and not WaveVectorList.Site.NotMagnetic and
    (PropTypesNumber > 0) then
    begin
      PropType := PropType + 1;
      if PropType = PropTypesNumber then
      begin PropType := 0; Result := True end else Result := False;
    end else Result := True
    {$B+}
  end;

begin
  if Assigned(Next) then
    if Next.NextPropType then
      Result := __NextPropType else Result := False
  else Result := __NextPropType;
end;

procedure TWaveVector.DeleteVector(const Index: LongInt);
begin
    Representations.DeleteVector(Index);
end;

procedure TWaveVector.InsertNewVector(const Index: LongInt);
begin
    Representations.InsertNewVector(Index);
end;

procedure TWaveVector.AddNewVector;
begin
    Representations.AddNewVector;
end;

procedure TWaveVector.IsReady;
var i: LongInt;
    TA: TAtom;
    InVect, OutVect: TDoubleVector3;
begin
    case FSelfCheckingMode of
        SSC_REPR : Representations.IsReady;
        SSC_ONLY_SELF : begin end;
    end;

    if IsPropVectMagnetic then
        with WaveVectorServer do
            for i := 0 to AtomList.Count - 1 do
            begin
                TA := TAtom(AtomList.Items[i]);
                InVect := TA.Moment;
                ConvertAphineToDekart(A, B, C, Alpha, Beta, Gamma, InVect);
                SetVectorProperly(InVect, OutVect);
                ConvertDekartToAphine(A, B, C, Alpha, Beta, Gamma, OutVect);
                TA.Mx := OutVect[1];
                TA.My := OutVect[2];
                TA.Mz := OutVect[3];
            end;
end;

function TWaveVector.GetSelfCheckingMode: LongInt;
begin
    Result := FSelfCheckingMode;
end;

procedure TWaveVector.SetSelfCheckingMode(const AMode: LongInt);
begin
    FSelfCheckingMode := AMode;
end;

function TWaveVector.GetSelfCopyingMode: LongInt;
begin
    Result := FSelfCopyingMode;
end;

procedure TWaveVector.SetSelfCopyingMode(const AMode: LongInt);
begin
    FSelfCopyingMode := AMode;
end;

function TWaveVector.MyNameIs: string;
begin
    Result := AsString;
end;

function TWaveVector.GetModuleTransFactor(
    const Translation: TDoubleVector3;
    const Moment: TDoubleVector3;
    const EllipseParam: Double
    ): Double;
var TempVect: TDoubleVector3;
    Angle, CosAngle, SinAngle: Double;
begin
    if AllowedTransTypesNumber <> 0 then
        if TransTypeIndex <> -1 then
            case AllowedTransTypes[TransTypeIndex] of
                TT_LSW : begin
                    TempVect[1] := WaveVector[1];
                    TempVect[2] := WaveVector[2];
                    TempVect[3] := WaveVector[3];
                    Angle := pi * GetScalarMul(TempVect, Translation);

                    Result := Abs(Cos(Angle));
                end;
                TT_ES : begin
                    if EllipseParam <> 0 then
                    begin
                        TempVect[1] := Moment[1];
                        TempVect[2] := Moment[2];
                        TempVect[3] := Moment[3];

                        RotateMoment(Translation, TempVect);

                        with WaveVectorServer do
                            Angle := GetAngle(TempVect, Moment, A, B, C,
                                Alpha, Beta, Gamma);

                        CosAngle := Cos(Angle); SinAngle := Sin(Angle);
                        Result := Sqrt(1 / (Sqr(CosAngle) +
                            (1 / Sqr(EllipseParam)) * Sqr(SinAngle)));
                    end else Result := 0;
                end else Result := 1;
            end else raise EWaveVector.Create('Transformation type must be defined...')
    else Result := 1;
end;

function TWaveVector.GetRotMatrix(
    const Translation: TDoubleVector3): TMatrix;
var TempVect: TDoubleVector3;
    Angle: Double;
    TempMatrix: TMatrix;
    TempDouble: Double;
const TINY = 1e-6;
begin
    TempVect[1] := WaveVector[1];
    TempVect[2] := WaveVector[2];
    TempVect[3] := WaveVector[3];

    Angle := 2 * pi * GetScalarMul(TempVect, Translation);

    case StarType of
        ST_ONE_ARM : begin
            TempDouble := Abs(Angle) - pi;
            if (TempDouble < TINY) and HasOnlyGivenComponents(0.5)
                then GetReverseMatrix(Result) else GetUnitMatrix(Result);
        end;

        ST_TWO_ARMS :
            if AllowedTransTypesNumber <> 0 then
                if TransTypeIndex <> -1 then
                    case AllowedTransTypes[TransTypeIndex] of
                        TT_SS : case RotAxisType of RA_X : GetMatrixRotX(Angle, Result);
                                RA_Y : GetMatrixRotY(Angle, Result);
                                RA_Z : GetMatrixRotZ(Angle, Result);
                                RA_OTHER :  begin end;
                            end;

                        TT_LSW :if Odd(Round(Angle / pi)) then GetReverseMatrix(Result)
                            else GetUnitMatrix(Result);

                        TT_ES : case RotAxisType of
                                RA_X : GetMatrixRotX(Angle, Result);
                                RA_Y : GetMatrixRotY(Angle, Result);
                                RA_Z : GetMatrixRotZ(Angle, Result);
                                RA_OTHER :  begin end;
                            end;

                        TT_CS : raise EWaveVector.Create('Feature doesn''t realized...');
                    end else raise EWaveVector.Create('Transformation type must be defined...')
            else raise EWaveVector.Create('Number of transformations must be non-zero...')
    end;
end;

function TWaveVector.CheckVector(const Vect: TDoubleVector3): Byte;
var OutVect: TDoubleVector3;
    i: LongInt;
const TINY = 1e-6;
begin
    Result := 0;
    SetVectorProperly(Vect, OutVect);
    
    if Abs(Vect[1] - OutVect[1]) > TINY then Result := Result or INVALID_X;
    if Abs(Vect[2] - OutVect[2]) > TINY then Result := Result or INVALID_Y;
    if Abs(Vect[3] - OutVect[3]) > TINY then Result := Result or INVALID_Z;
end;

procedure TWaveVector.SetVectorProperly(const InVect: TDoubleVector3;
    var OutVect: TDoubleVector3);
var TempVect: TDoubleVector3;
    Module, Module2: Double;
begin
    if AllowedTransTypesNumber <> 0 then
    begin
        if TransTypeIndex <> -1 then
        begin
            if AllowedTransTypes[TransTypeIndex] <> TT_LSW then
                MultiplyVectorByDirectMatrix(InVect, TempVect);
            case AllowedTransTypes[TransTypeIndex] of
                TT_SS, TT_LSW : begin end;
                TT_ES :
                begin
                    Module := GetVectModule(TempVect);
                    TempVect[3] := 0;
                    Module2 := GetVectModule(TempVect);
                    if Module2 <> 0 then
                    begin
                        TempVect[1] := TempVect[1] * (Module / Module2);
                        TempVect[2] := TempVect[2] * (Module / Module2);
                    end else
                    begin
                        TempVect[1] := Module / Sqrt(2);
                        TempVect[2] := Module / Sqrt(2);
                    end;
                end;
                TT_CS : raise EWaveVector.Create('Feature doesn''t realized...');
                else raise EWaveVector.Create('Unknown type of transformation...')
            end;    if AllowedTransTypes[TransTypeIndex] <> TT_LSW then
                MultiplyVectorByReverseMatrix(TempVect, OutVect)
            else OutVect := InVect;
        end else
        raise EWaveVector.Create('Transformation type must be defined...');
    end else OutVect := InVect;
end;

procedure TWaveVector.MultiplyVectorByDirectMatrix(
const InVect: TDoubleVector3; var OutVect: TDoubleVector3);

  function GetDirectMatrix(const AxisDir: TDoubleVector3): TMatrix;

  var D: Double;
      Module: Double;
  begin
    D := Sqrt(Sqr(AxisDir[1]) + Sqr(AxisDir[3]));
    Module := Sqrt(Sqr(AxisDir[1]) + Sqr(AxisDir[2]) + Sqr(AxisDir[3]));
    if (D <> 0) and (Module <> 0) then begin
      Result[1, 1] := AxisDir[3] / D;
      Result[1, 2] := 0;
      Result[1, 3] := -AxisDir[1] / D;
      Result[1, 4] := 0;

      Result[2, 1] := -AxisDir[2] * AxisDir[1] / (D * Module);
      Result[2, 2] := D / Module;
      Result[2, 3] := -AxisDir[2] * AxisDir[3] / (D * Module);
      Result[2, 4] := 0;

      Result[3, 1] := AxisDir[1] / Module;
      Result[3, 2] := AxisDir[2] / Module;
      Result[3, 3] := AxisDir[3] / Module;
      Result[3, 4] := 0;

      Result[4, 1] := 0;
      Result[4, 2] := 0;
      Result[4, 3] := 0;
      Result[4, 4] := 1;
    end else GetZerosMatrix(Result);
  end;

var TempVect, TempAxis: TDoubleVector3;
begin
  if RotAxisType <> RA_NONE then begin
    TempVect := InVect;

    TempAxis := RotAxis;
    with WaveVectorServer do
      ConvertAphineToDekart(A, B, C, Alpha, Beta, Gamma, TempAxis);
    MulVectMatr(GetDirectMatrix(TempAxis), TempVect);
    OutVect := TempVect;
  end else raise EWaveVector.Create('Transformation axis must be defined...');
end;

procedure TWaveVector.DirectTransform(const InVect: TDoubleVector3;
    var OutVect: TDoubleVector3; var Unchangeables: Byte);
begin
     if AllowedTransTypesNumber <> 0 then begin
        if TransTypeIndex <> -1 then begin

          if CheckVector(InVect) <> 0 then
            raise EWaveVector.Create('Incoming vector has forbidden components...');
           if AllowedTransTypes[TransTypeIndex] <> TT_LSW then
              MultiplyVectorByDirectMatrix(InVect, OutVect)
           else OutVect := InVect;
           case AllowedTransTypes[TransTypeIndex] of
                TT_SS, TT_LSW :                 Unchangeables := 0;
                TT_ES :
                begin
                     Unchangeables := CH_DIS_ANG_Z;
                end;
                TT_CS : raise EWaveVector.Create('Feature doesn''t realized...');
                else raise EWaveVector.Create('Unknown type of transformation...')
           end; {case AllowedTransTypes[TransTypeIndex] of...}
        end {if TransTypeIndex <> -1 then...} else
            raise EWaveVector.Create('Transformation type must be defined...');
     end {if AllowedTransTypesNumber <> 0 then...} else begin
         OutVect := InVect;
         Unchangeables := 0;
     end;
end;

procedure TWaveVector.MultiplyVectorByReverseMatrix(
const InVect: TDoubleVector3; var OutVect: TDoubleVector3);

  function GetReverseMatrix(const AxisDir: TDoubleVector3): TMatrix;

  var D: Double;
      Module: Double;
  begin
    D := Sqrt(Sqr(AxisDir[1]) + Sqr(AxisDir[3]));
    Module := Sqrt(Sqr(AxisDir[1]) + Sqr(AxisDir[2]) + Sqr(AxisDir[3]));
    if (D <> 0) and (Module <> 0) then begin
      Result[1, 1] := AxisDir[3] / D;
      Result[1, 2] := -AxisDir[1] * AxisDir[2] / (D * Module);
      Result[1, 3] := AxisDir[1] / Module;
      Result[1, 4] := 0;

      Result[2, 1] := 0;
      Result[2, 2] := D / Module;
      Result[2, 3] := AxisDir[2] / Module;
      Result[2, 4] := 0;

      Result[3, 1] := -AxisDir[1] / D;
      Result[3, 2] := -AxisDir[2] * AxisDir[3] / (D * Module);
      Result[3, 3] := AxisDir[3] / Module;
      Result[3, 4] := 0;

      Result[4, 1] := 0;
      Result[4, 2] := 0;
      Result[4, 3] := 0;
      Result[4, 4] := 1;
    end else GetZerosMatrix(Result);
  end;

var TempVect, TempAxis: TDoubleVector3;
begin

  if RotAxisType <> RA_NONE then begin
    TempVect := InVect;

    TempAxis := RotAxis;
    with WaveVectorServer do
      ConvertAphineToDekart(A, B, C, Alpha, Beta, Gamma, TempAxis);
    MulVectMatr(GetReverseMatrix(TempAxis), TempVect);
    OutVect := TempVect;
  end else raise EWaveVector.Create('Transformation axis must be defined...');
end;

procedure TWaveVector.ReverseTransform(const InVect: TDoubleVector3;
var OutVect: TDoubleVector3);
begin
     if AllowedTransTypesNumber <> 0 then
     begin
          if TransTypeIndex <> -1 then
          begin
               if AllowedTransTypes[TransTypeIndex] <> TT_LSW then
                  MultiplyVectorByReverseMatrix(InVect, OutVect)
               else OutVect := InVect;
          end else
              raise EWaveVector.Create('Transformation type must be defined...');
     end else OutVect := InVect;
end;

function TWaveVector.GetAtomCount: LongInt;
begin
  Result := WaveVectorServer.AtomList.Count;
end;

function TWaveVector.GetAllowedRotAxis: Byte;
begin
  if (AllowedTransTypesNumber <> 0) and (TransTypeIndex <> -1) then begin
    case AllowedTransTypes[TransTypeIndex] of
      TT_SS : Result := ALLOWED_AXIS_X or ALLOWED_AXIS_Y or
              ALLOWED_AXIS_Z;TT_LSW : Result := 0;
      TT_CS : raise EWaveVector.Create('Feature does not realized...');
      TT_ES : Result := ALLOWED_AXIS_X or ALLOWED_AXIS_Y or
              ALLOWED_AXIS_Z;end;
  end else Result := 0;
end;

function TWaveVector.GetAllowedStarTypes: Byte;
begin
  Result := 0;
  if IsPropVectStructural then begin Result := ST_ONE_ARM; Exit end;
  if IsPropVectMagnetic then begin
    if HasOnlyGivenComponents(0.5) then Result := ST_ONE_ARM or ST_TWO_ARMS
    else
      if HasOnlyZeroComponents then Result := ST_ONE_ARM
      else Result := ST_TWO_ARMS;
  end;
end;

function TWaveVector.GetAllowedTransTypes(index: LongInt): LongInt;
begin
    if index >= AllowedTransTypesNumber then
      raise EWaveVector.Create('Invalid trans. type index...');
    case index of
      0 : Result := TT_SS;
      1 : Result := TT_LSW;
      2 : Result := TT_ES;
    end;
end;

function TWaveVector.GetAllowedTransTypesNumber: LongInt;
begin
  Result := 0;
  if StarType = ST_TWO_ARMS then Result := TT_NUMBER;
end;

function TWaveVector.GetAllowedPropVectorTypes: Byte;
begin
  if HasOnlyZeroComponents then Result := PVT_MAGNETIC or PVT_STRUCTURAL
  else Result := PVT_MAGNETIC;
end;

function TWaveVector.GetWaveVectorServer: TSite;
begin
    if Assigned(FWaveVectorServer) then Result := FWaveVectorServer
    else raise EWaveVector.Create('Server isn''t assigned...');
end;

function TWaveVector.GetIntensityFactors: IIntensityFactors;
begin
  if Assigned(FIntensityFactors) then Result := FIntensityFactors
  else raise EWaveVector.Create('Intensity factors aren''t assigned...');
end;

constructor TWaveVector.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateRepresentations;
  FTransTypeIndex := -1;
  FSelfCheckingMode := SSC_NO_CHECKING;
  FSelfCopyingMode := SSCOPY_DEFAULT;
end;

procedure TWaveVector.CreateRepresentations;
begin
  ReplaceRepresentations(CreateNewReprList);
end;

destructor TWaveVector.Destroy;
begin
    UtilizeObject(Representations);
    inherited Destroy;
end;

procedure TWaveVector.CreateHKLList(
    const PatternAuxiliary: TPatternAuxiliary;
    const ExtHKLList: THKLList;
    const AMinSinTL, AMaxSinTL: Double);

var h, k, l: LongInt;
    WVH, WVK, WVL: Double;
    TempSinTL: Double;
    i: LongInt;
    CycleNum: Byte;
    Vector: TDoubleVector3;
    ReallyNew: Boolean;
    SL: TStarLink;
const TINY = 1e-6;
begin
    with ExtHKLList, PatternAuxiliary do
    begin
        for h := -MAX_HKL_INDEX to MAX_HKL_INDEX do
            for k := -MAX_HKL_INDEX to MAX_HKL_INDEX do
                for l := -MAX_HKL_INDEX to MAX_HKL_INDEX do
                begin
                    if (AllowedTransTypesNumber <> 0) then
                        if TransTypeIndex <> -1 then
                        begin
                            case AllowedTransTypes[TransTypeIndex] of
                                 TT_SS : CycleNum := 3
                                 else CycleNum := 2
                            end
                        end else
                            raise EWaveVector.Create('Transformation type must be defined...')
                    else    CycleNum := 1;

                    for i := 1 to CycleNum do
                    begin
                        case i of
                            1 : begin WVH := h + WaveVector[1];
                                WVK := k + WaveVector[2];
                                WVL := l + WaveVector[3];
                            end;
                            2 : begin WVH := h - WaveVector[1];
                                WVK := k - WaveVector[2];
                                WVL := l - WaveVector[3];
                            end;
                            3 : begin
                                WVH := h;
                                WVK := k;
                                WVL := l;
                            end;
                        end;

                        if (Abs(WVH) < TINY) and (Abs(WVK) < TINY) and
                           (Abs(WVL) < TINY) then Continue;

                        TempSinTL := GetSinTL(WVH, WVK, WVL);

                        if (TempSinTL >= AMinSinTL) and (TempSinTL <= AMaxSinTL) then
                        begin
                            Vector[1] := WVH; Vector[2] := WVK; Vector[3] := WVL;
                            ExtHKLList.CreateNewHKL(
                                PatternAuxiliary, Vector, ReallyNew);
                        end;
                    end;
                end;
    end;
    LinkWithHKLList(ExtHKLList);
end;

procedure TWaveVector.LinkWithHKLList(const ExtHKLList: THKLList);

    function IsItLatticeVector(
        const H, K, L: Double;
        var LatticeH, LatticeK, LatticeL: LongInt): Boolean;
    const TINY = 1e-6;
    begin
        if (Abs(Frac(H)) < TINY) and
           (Abs(Frac(K)) < TINY) and
           (Abs(Frac(L)) < TINY) then
        begin
            Result := True;
            LatticeH := Round(H);
            LatticeK := Round(K);
            LatticeL := Round(L);
        end else
        begin
            Result := False;
            LatticeH := 0;
            LatticeK := 0;
            LatticeL := 0;
        end;
    end;

    function CreateStarLink(const H, K, L: LongInt): TStarLink;
    begin
        Result := TStarLink.Create;
        Result.Star := Self;
        Result.H := H;
        Result.K := K;
        Result.L := L;
    end;

    procedure AddNewStarLink(const HC: THKLClass;
        const H, K, L: Double);
    var LatticeH, LatticeK, LatticeL: LongInt;
    begin
        if IsItLatticeVector(
            H, K, L, LatticeH, LatticeK, LatticeL) then
            HC.AddStarLink(CreateStarLink(LatticeH, LatticeK, LatticeL));
    end;

var i: LongInt;
    HC: THKLClass;
    TempH, TempK, TempL: Double;
begin
    with ExtHKLList do
    begin
        for i := 0 to Count - 1 do
        begin
            HC := THKLClass(Items[i]);

            if (AllowedTransTypesNumber <> 0) then
                if TransTypeIndex <> -1 then
                begin
                    case AllowedTransTypes[TransTypeIndex] of
                        TT_SS : begin
                            TempH := HC.WVH - WaveVector[1];
                            TempK := HC.WVK - WaveVector[2];
                            TempL := HC.WVL - WaveVector[3];
                            AddNewStarLink(HC, TempH, TempK, TempL);

                            TempH := HC.WVH + WaveVector[1];
                            TempK := HC.WVK + WaveVector[2];
                            TempL := HC.WVL + WaveVector[3];
                            AddNewStarLink(HC, TempH, TempK, TempL);

                            TempH := HC.WVH;
                            TempK := HC.WVK;
                            TempL := HC.WVL;
                            AddNewStarLink(HC, TempH, TempK, TempL);
                        end
                        else begin
                            TempH := HC.WVH - WaveVector[1];
                            TempK := HC.WVK - WaveVector[2];
                            TempL := HC.WVL - WaveVector[3];
                            AddNewStarLink(HC, TempH, TempK, TempL);

                            TempH := HC.WVH + WaveVector[1];
                            TempK := HC.WVK + WaveVector[2];
                            TempL := HC.WVL + WaveVector[3];
                            AddNewStarLink(HC, TempH, TempK, TempL);
                        end;
                    end
                end else
                    raise EWaveVector.Create('Transformation type must be defined...')
            else    begin
                    TempH := HC.WVH - WaveVector[1];
                    TempK := HC.WVK - WaveVector[2];
                    TempL := HC.WVL - WaveVector[3];
                    AddNewStarLink(HC, TempH, TempK, TempL);
                end;
        end;
    end;
end;

function TWaveVector.GetWaveVector(index: LongInt): Double;
begin
    Result := FWaveVector[index];
end;

function TWaveVector.GetRotAxis: TDoubleVector3;
begin
    Result := FRotAxis;
end;

procedure TWaveVector.SetRotAxis(const ARotAxis: TDoubleVector3);
var TempAllowed: Byte;
const TINY = 1e-6;
begin
  
  TempAllowed := AllowedRotAxis;

  if TempAllowed <> 0 then begin
    if (Abs(ARotAxis[1]) > TINY) and (Abs(ARotAxis[2]) < TINY)
    and (Abs(ARotAxis[3]) < TINY) then begin
      if TempAllowed and ALLOWED_AXIS_X = 0 then
        raise EWaveVector.Create('Invalid axis components...');
      FRotAxis := ARotAxis;
      Exit
    end;

    if (Abs(ARotAxis[1]) < TINY) and (Abs(ARotAxis[2]) > TINY)
    and (Abs(ARotAxis[3]) < TINY) then begin
      if TempAllowed and ALLOWED_AXIS_Y = 0 then
        raise EWaveVector.Create('Invalid axis components...');
      FRotAxis := ARotAxis;
      Exit
    end;

    if (Abs(ARotAxis[1]) < TINY) and (Abs(ARotAxis[2]) < TINY)
    and (Abs(ARotAxis[3]) > TINY) then begin
      if TempAllowed and ALLOWED_AXIS_Z = 0 then
        raise EWaveVector.Create('Invalid axis components...');
      FRotAxis := ARotAxis;
      Exit
    end;

    
    if (Abs(ARotAxis[1]) < TINY) and (Abs(ARotAxis[2]) < TINY) and
       (Abs(ARotAxis[3]) < TINY) then
      raise EWaveVector.Create('Rotation axis mustn''t be zero...');

    if TempAllowed and ALLOWED_AXIS_ANY = 0 then
    raise EWaveVector.Create('Invalid axis components...');
    FRotAxis := ARotAxis;
  end else begin
    if (Abs(ARotAxis[1]) > TINY) or (Abs(ARotAxis[2]) > TINY) or
       (Abs(ARotAxis[3]) > TINY) then
      raise EWaveVector.Create('Rotation axis must be zero...');
    FRotAxis := ARotAxis;
  end;
end;

procedure TWaveVector.SetStarType(const AStarType: Byte);
begin
  if AStarType and AllowedStarTypes <> AStarType then
    raise EWaveVector.Create('Invalid star type...');
  FStarType := AStarType;
  CheckTransTypeIndex;
end;

procedure TWaveVector.SetTransTypeIndex(const ATransTypeIndex: ShortInt);
begin
  if ATransTypeIndex >= AllowedTransTypesNumber then
    raise EWaveVector.Create('Invalid trans. type index...');
  FTransTypeIndex := ATransTypeIndex;

  CheckAxisType;
end;

function TWaveVector.GetFirstAllowedAxis: Byte;
var TempByte, AxisMask: Byte;
    i: LongInt;
begin
  Result := RA_NONE;
  TempByte := AllowedRotAxis;
  AxisMask := 1;
  for i := 1 to 8 do begin
    if TempByte and AxisMask <> 0 then begin
      case AxisMask of
        ALLOWED_AXIS_X : Result := RA_X;
        ALLOWED_AXIS_Y : Result := RA_Y;
        ALLOWED_AXIS_Z : Result := RA_Z;
        ALLOWED_AXIS_ANY :  Result := RA_OTHER;
      end;
      Break;
    end else AxisMask := AxisMask shl 1;
  end;
end;

procedure TWaveVector.SetAxisFromConst(const AxisType: Byte);
begin
  case AxisType of
    RA_X : begin FRotAxis[1] := 1; FRotAxis[2] := 0; FRotAxis[3] := 0 end;
    RA_Y : begin FRotAxis[1] := 0; FRotAxis[2] := 1; FRotAxis[3] := 0 end;
    RA_Z : begin FRotAxis[1] := 0; FRotAxis[2] := 0; FRotAxis[3] := 1 end;
    else raise EWaveVector.Create('Invalid axis const...')
  end;
end;

function TWaveVector.GetRotAxisType: ShortInt;
const TINY = 1e-6;
begin
  if (Abs(FRotAxis[1]) > TINY) and (Abs(FRotAxis[2]) < TINY)
  and (Abs(FRotAxis[3]) < TINY) then begin
    Result := RA_X; Exit
  end;

  if (Abs(FRotAxis[1]) < TINY) and (Abs(FRotAxis[2]) > TINY)
  and (Abs(FRotAxis[3]) < TINY) then begin
    Result := RA_Y; Exit
  end;

  if (Abs(FRotAxis[1]) < TINY) and (Abs(FRotAxis[2]) < TINY)
  and (Abs(FRotAxis[3]) > TINY) then begin
    Result := RA_Z; Exit
  end;

  if (Abs(FRotAxis[1]) < TINY) and (Abs(FRotAxis[2]) < TINY)
  and (Abs(FRotAxis[3]) < TINY) then begin
    Result := RA_NONE; Exit
  end;

  Result := RA_OTHER
end;

procedure TWaveVector.SetWaveVector(index: Integer; const Value: Double);
const TINY = 1e-6;
begin
  FWaveVector[index] := Value;
  if (Abs(Frac(FWaveVector[1])) < TINY) and
     (Abs(Frac(FWaveVector[2])) < TINY) and
     (Abs(Frac(FWaveVector[3])) < TINY) then
  begin FWaveVector[1] := 0; FWaveVector[2] := 0; FWaveVector[3] := 0; end;
  CheckPropVectorType;
end;

procedure TWaveVector.CheckStarType;
var TempByte, StarMask: Byte;
    i: LongInt;
begin
  TempByte := AllowedStarTypes;
  if TempByte <> 0 then begin
    if (FStarType and TempByte <> FStarType) or (FStarType = 0) then begin

      StarMask := 1;
      for i := 1 to 8 do begin
        if TempByte and StarMask <> 0 then begin

          FStarType := StarMask;
          Break
        end
        else StarMask := StarMask shl 1;
      end;
    end;
  end else FStarType := 0;

  CheckTransTypeIndex;
end;

procedure TWaveVector.CheckPropVectorType;
begin
  FPropVectorType := FPropVectorType and AllowedPropVectorTypes;

  CheckStarType;
end;

procedure TWaveVector.CheckTransTypeIndex;

var NumberOfAllowed: Byte;
begin
  NumberOfAllowed := AllowedTransTypesNumber;
  if (FTransTypeIndex >= NumberOfAllowed) or (FTransTypeIndex = -1) then
    if NumberOfAllowed > 0 then FTransTypeIndex := 0
    else FTransTypeIndex := -1;

  CheckAxisType;
end;

procedure TWaveVector.CheckAxisType;

var TempAllowed, TempFirst: Byte;
const TINY = 1e-6;
begin
  TempAllowed := AllowedRotAxis;

  if TempAllowed <> 0 then begin
    if (Abs(FRotAxis[1]) > TINY) and (Abs(FRotAxis[2]) < TINY)
    and (Abs(FRotAxis[3]) < TINY) then begin
      if TempAllowed and ALLOWED_AXIS_X = 0 then begin
        TempFirst := GetFirstAllowedAxis; SetAxisFromConst(TempFirst)
      end;
      Exit
    end;

    if (Abs(FRotAxis[1]) < TINY) and (Abs(FRotAxis[2]) > TINY)
    and (Abs(FRotAxis[3]) < TINY) then begin
      if TempAllowed and ALLOWED_AXIS_Y = 0 then begin
        TempFirst := GetFirstAllowedAxis; SetAxisFromConst(TempFirst)
      end;
      Exit
    end;

    if (Abs(FRotAxis[1]) < TINY) and (Abs(FRotAxis[2]) < TINY)
    and (Abs(FRotAxis[3]) > TINY) then begin
      if TempAllowed and ALLOWED_AXIS_Z = 0 then begin
        TempFirst := GetFirstAllowedAxis; SetAxisFromConst(TempFirst)
      end;
      Exit
    end;

    if (Abs(FRotAxis[1]) < TINY) and (Abs(FRotAxis[2]) < TINY)
    and (Abs(FRotAxis[3]) < TINY) then begin
      TempFirst := GetFirstAllowedAxis; SetAxisFromConst(TempFirst);
      Exit;
    end;

    if TempAllowed and ALLOWED_AXIS_ANY = 0 then begin
      TempFirst := GetFirstAllowedAxis; SetAxisFromConst(TempFirst)
    end;
  end else begin
    FRotAxis[1] := 0; FRotAxis[2] := 0; FRotAxis[3] := 0
  end;
end;

procedure TWaveVector.SetPropVectorType(const APropVectorType: Byte);
begin
  if APropVectorType and AllowedPropVectorTypes <> APropVectorType then
    raise EWaveVector.Create('Invalid star type combination...');
  FPropVectorType := APropVectorType;
  if Assigned(WaveVectorList) then WaveVectorList.StarStateChanged(Self);

  CheckStarType;
end;

function TWaveVector.IsPropVectMagnetic: Boolean;
begin
  Result := (PropVectorType and PVT_MAGNETIC) <> 0;
end;

function TWaveVector.IsPropVectStructural: Boolean;
begin
  Result := (PropVectorType and PVT_STRUCTURAL) <> 0;
end;

procedure TSite.SetAtomMomentsCur;
var AtomNum, BasisFuncNum: LongInt;
    MagnStar: TWaveVector;
    TR: TRepresentation;
    TB: TBasisFunctions;
    SR: TAtom;
    TempVect: TDoubleVector3;
begin
    MagnStar := WaveVectorList.GetMagnStar;
    if MagnStar <> nil then
    begin
        if MagnStar.Representations.Count <> 0 then with MagnStar do
        begin
            TR := TRepresentation(Representations.Items[CurReprNum]);
            for AtomNum := 0 to AtomList.Count - 1 do
            begin
                SR := TAtom(AtomList.Items[AtomNum]);
                SR.Mx := 0; SR.My := 0; SR.Mz := 0;
                for BasisFuncNum := 0 to TR.Count - 1 do
                begin
                    TB := TBasisFunctions(TR.Items[BasisFuncNum]);
                    if not TB.Excluded then
                    begin
                        TempVect := TB[AtomNum];
                        SR.Mx := SR.Mx + TB.MixingConst * TempVect[1];
                        SR.My := SR.My + TB.MixingConst * TempVect[2];
                        SR.Mz := SR.Mz + TB.MixingConst * TempVect[3];
                    end;
                end; {for BasisFuncNum := 0 to TR.Count - 1 do...}
            end; {for AtomNum := 0 to AtomList.Count - 1 do...}
        end else
            raise ESite.Create(
                'Representation list is empty in the site (' + SiteName + ')...'
            );
    end {if MagnStar <> nil then...}
    else
        raise ESite.Create(
            'Magnetic star undefined in the site (' + SiteName + ')...'
        );
end;

procedure TSite.SetAtomMomentsAll;
var AtomNum, BasisFuncNum, RepNum: LongInt;
    MagnStar: TWaveVector;
    TR: TRepresentation;
    TB: TBasisFunctions;
    SR: TAtom;
    TempVect: TDoubleVector3;
begin
    MagnStar := WaveVectorList.GetMagnStar;
    if MagnStar <> nil then
    begin
        if MagnStar.Representations.Count <> 0 then with MagnStar do
        begin
            for AtomNum := 0 to AtomList.Count - 1 do
            begin
                SR := TAtom(AtomList.Items[AtomNum]);
                SR.Mx := 0; SR.My := 0; SR.Mz := 0;

                for RepNum := 0 to Representations.Count - 1 do
                begin
                    TR := TRepresentation(Representations.Items[RepNum]);
                    for BasisFuncNum := 0 to TR.Count - 1 do
                    begin
                        TB := TBasisFunctions(TR.Items[BasisFuncNum]);
                        if not TB.Excluded then
                        begin
                            TempVect := TB[AtomNum];
                            SR.Mx := SR.Mx + TB.MixingConst * TempVect[1];
                            SR.My := SR.My + TB.MixingConst * TempVect[2];
                            SR.Mz := SR.Mz + TB.MixingConst * TempVect[3];
                        end;
                    end;{for BasisFuncNum := 0 to TR.Count - 1 do...}
                end;{for RepNum := 0 to Representations.Count - 1 do...}
            end;{for AtomNum := 0 to AtomList.Count - 1 do...}
        end else
            raise ESite.Create(
                'Representation list is empty in the site (' + SiteName + ')...'
            );
    end
    else
        raise ESite.Create(
            'Magnetic star undefined in the site (' + SiteName + ')...'
        );
end;

procedure TSite.MulModules(const MulConst: Double);
var i: LongInt;
    TS: TAtom;
begin
  for i := 0 to AtomList.Count - 1 do
  begin
    TS := TAtom(AtomList.Items[i]);
    TS.M := TS.M * MulConst;
  end;
end;

procedure TSite.SetSiteModule(const Module: Double);
var i: LongInt;
    TS: TAtom;
begin
  FSiteModule := Module;
  for i := 0 to AtomList.Count - 1 do
  begin
    TS := TAtom(AtomList.Items[i]);
    if FSiteModule = 0 then TS.M := TINY else TS.M := FSiteModule;
  end;
end;

procedure TSite.SetSiteName(const SiteName: string);
var i: LongInt;
    TS: TAtom;
begin
    FSiteName := SiteName;
    if Assigned(AtomList) then
    for i := 0 to AtomList.Count - 1 do
    begin
        TS := TAtom(AtomList.Items[i]);
        TS.Position := FSiteName;
    end;
end;

procedure TSite.CreateParameters;
var ActParNum: LongInt;
    MagnStar: TWaveVector;
begin
    MagnStar := WaveVectorList.MagnStar;
    if Assigned(MagnStar) and not NotMagnetic
        and not Disabled then MagnStar.CreateParameters;

    inherited;
end;

procedure TSite.ParametersUpdated;
var MagnStar: TWaveVector;
    TR: TRepresentation;
    TB: TBasisFunctions;
    i, j, CurParamNum: LongInt;
    TA: TAtom;
    Unchangeables: Byte;
    InVect, OutVect: TDoubleVector3;
    DisAxisNum: Byte;
    Theta, Phi, R: Double;
begin
  MagnStar := WaveVectorList.MagnStar;
  if Assigned(MagnStar) and not NotMagnetic and not Disabled then
  begin
    CurParamNum := 0;
    case ReprMode of
      RM_BY_ONE : begin
        with MagnStar do
          TR := TRepresentation(Representations.Items[CurReprNum]);

        for i := 0 to TR.Count - 1 do begin
        
          TB := TBasisFunctions(TR.Items[i]);

          TB.MixingConst := Parameter[CurParamNum].Value;
          Inc(CurParamNum);
        end;
        SetAtomMomentsCur;

        case VariationMode of
            VM_ALL_COMPONENTS : begin
                for i := 0 to AtomList.Count - 1 do
                begin
                    TA := TAtom(AtomList.Items[i]);
                    TA.M := Abs(Parameter[CurParamNum].Value);
                    Inc(CurParamNum);
                end;
            end;
            VM_EQUAL_MODULES :  begin
                AtomList.SetModuleAtSite('ALL', Abs(Parameter[CurParamNum].Value));
                Inc(CurParamNum);
            end;
        end;
      end;

      RM_BY_ALL : begin
        for i := 0 to MagnStar.Representations.Count - 1 do begin
          TR := TRepresentation(MagnStar.Representations.Items[i]);
          for j := 0 to TR.Count - 1 do begin
          
            TB := TBasisFunctions(TR.Items[j]);

            TB.MixingConst := Parameter[CurParamNum].Value;
            Inc(CurParamNum);
          end;
        end;
        SetAtomMomentsAll;

        case VariationMode of
            VM_ALL_COMPONENTS : begin
                for i := 0 to AtomList.Count - 1 do
                begin
                    TA := TAtom(AtomList.Items[i]);
                    TA.M := Abs(Parameter[CurParamNum].Value);
                    Inc(CurParamNum);
                end;
            end;
            VM_EQUAL_MODULES :  begin
                AtomList.SetModuleAtSite('ALL', Abs(Parameter[CurParamNum].Value));
                Inc(CurParamNum);
            end;
        end;
      end;

      RM_DONT_USE : begin
        case VariationMode of
          VM_ALL_COMPONENTS, VM_EQUAL_MODULES : begin
            for i := 0 to AtomList.Count - 1 do
            begin
              TA := TAtom(AtomList.Items[i]);
              InVect := TA.Moment;
              
              ConvertAphineToDekart(A, B, C, Alpha, Beta, Gamma, InVect);
              MagnStar.DirectTransform(InVect, OutVect, Unchangeables);
              ConvertDekartToSpherical(
              OutVect[1], OutVect[2], OutVect[3], Theta, Phi, R);
              

              DisAxisNum := 0;
              if Unchangeables and CH_DIS_ANG_X <> 0 then Inc(DisAxisNum);
              if Unchangeables and CH_DIS_ANG_Y <> 0 then Inc(DisAxisNum);
              if Unchangeables and CH_DIS_ANG_Z <> 0 then Inc(DisAxisNum);

              case DisAxisNum of
                0 : begin
                  Theta := Parameter[CurParamNum].Value;
                  Inc(CurParamNum);

                  Phi := Parameter[CurParamNum].Value;
                  Inc(CurParamNum);

                  if VariationMode = VM_ALL_COMPONENTS then begin
                    TA.M := Abs(Parameter[CurParamNum].Value);
                    
                    Inc(CurParamNum);
                  end;
                end;

                1 : begin
                  
                  if Unchangeables and CH_DIS_ANG_X <> 0 then
                    raise ESite.Create('Feature doesn''t supported...');
                  if Unchangeables and CH_DIS_ANG_Y <> 0 then
                    raise ESite.Create('Feature doesn''t supported...');

                  if Unchangeables and CH_DIS_ANG_Z <> 0 then begin
                  
                    Phi := Parameter[CurParamNum].Value;
                    Inc(CurParamNum);
                  end;

                  if VariationMode = VM_ALL_COMPONENTS then begin
                    TA.M := Abs(Parameter[CurParamNum].Value);
                    
                    Inc(CurParamNum);
                  end;
                end;

                else begin
                  
                  if VariationMode = VM_ALL_COMPONENTS then begin
                    TA.M := Abs(Parameter[CurParamNum].Value);
                    
                    Inc(CurParamNum);
                  end;
                end;
              end;{case DisAxisNum of...}

              ConvertSphericalToDekart(Theta, Phi, R, InVect[1], InVect[2], InVect[3]);
              
              MagnStar.ReverseTransform(InVect, OutVect);
              
              ConvertDekartToAphine(A, B, C, Alpha, Beta, Gamma, OutVect);
              TA.Mx := OutVect[1]; TA.My := OutVect[2]; TA.Mz := OutVect[3];
            end;{for i := 0 to AtomList.Count - 1 do...}

            if VariationMode = VM_EQUAL_MODULES then begin
            
              AtomList.SetModuleAtSite('ALL', Abs(Parameter[CurParamNum].Value));
              Inc(CurParamNum);
            end;
          end;

          VM_ONLY_MODULES : begin
            AtomList.SetModuleAtSite('ALL', Abs(Parameter[CurParamNum].Value));
            Inc(CurParamNum);
          end;
        end;{case VariationMode of...}
      end;
    end;{case ReprMode of...}
    for i := 0 to MagnStar.ParametersNumber - 1 do begin
    
      MagnStar.Parameter[i] := Parameter[CurParamNum];
      Inc(CurParamNum);
    end;
    MagnStar.ParametersUpdated;
  end;{if Assigned(MagnStar) and not NotMagnetic and not Disabled then...}
end;

function TSite.GetActualParametersNumber: LongInt;
var MagnStar: TWaveVector;
    TR: TRepresentation;
    i: LongInt;
    TA: TAtom;
    Unchangeables: Byte;
    InVect, OutVect: TDoubleVector3;
    DisAxisNum: Byte;
begin
  MagnStar := WaveVectorList.MagnStar;
  if Assigned(MagnStar) and not NotMagnetic and not Disabled then
  begin
    Result := 0;
    case ReprMode of
      RM_BY_ONE : begin
        with MagnStar do
          TR := TRepresentation(Representations.Items[CurReprNum]);
        Result := TR.BFsInUseNumber;
        
        case VariationMode of
            VM_ALL_COMPONENTS : begin
                Result := Result + AtomList.Count;
                
            end;
            VM_EQUAL_MODULES :  begin
                Result := Result + 1;
                
            end;
        end;
      end;

      RM_BY_ALL : begin
        for i := 0 to MagnStar.Representations.Count - 1 do begin
          TR := TRepresentation(MagnStar.Representations.Items[i]);
          Result := Result + TR.BFsInUseNumber;
          
        end;
        case VariationMode of
            VM_ALL_COMPONENTS : begin
                Result := Result + AtomList.Count;
                
            end;
            VM_EQUAL_MODULES :  begin
                Result := Result + 1;
                
            end;
        end;
      end;

      RM_DONT_USE : begin
        case VariationMode of
          VM_ALL_COMPONENTS, VM_EQUAL_MODULES : begin
            for i := 0 to AtomList.Count - 1 do
            begin
              TA := TAtom(AtomList.Items[i]);
              InVect := TA.Moment;
              
              ConvertAphineToDekart(A, B, C, Alpha, Beta, Gamma, InVect);
              MagnStar.DirectTransform(InVect, OutVect, Unchangeables);
              

              DisAxisNum := 0;
              if Unchangeables and CH_DIS_ANG_X <> 0 then Inc(DisAxisNum);
              if Unchangeables and CH_DIS_ANG_Y <> 0 then Inc(DisAxisNum);
              if Unchangeables and CH_DIS_ANG_Z <> 0 then Inc(DisAxisNum);

              case DisAxisNum of
                0 : 
                  case VariationMode of
                    VM_ALL_COMPONENTS : Result := Result + 3;
                    
                    VM_EQUAL_MODULES : Result := Result + 2;
                    
                  end;

                1 : begin
                  
                  case VariationMode of
                    VM_ALL_COMPONENTS : Result := Result + 2;
                    
                    VM_EQUAL_MODULES : Result := Result + 1;
                    
                  end;
                end;

                else begin
                  
                  if VariationMode = VM_ALL_COMPONENTS then
                    Result := Result + 1;
                end;
              end;
            end;{for i := 0 to AtomList.Count - 1 do...}
            if VariationMode = VM_EQUAL_MODULES then Result := Result + 1;
            
          end;

          VM_ONLY_MODULES : begin
            
            Result := 1;
          end;
        end;{case VariationMode of...}
      end;
    end;{case ReprMode of...}
    Result := Result + MagnStar.ParametersNumber;
    
  end else Result := 0;
end;

procedure TSite.FillParameters;
var MagnStar: TWaveVector;
    TR: TRepresentation;
    TB: TBasisFunctions;
    i, j, CurParamNum: LongInt;
    TA: TAtom;
    Unchangeables: Byte;
    InVect, OutVect: TDoubleVector3;
    DisAxisNum: Byte;
    Theta, Phi, R: Double;
    TempParameter: TVariableParameter;
begin
  MagnStar := WaveVectorList.MagnStar;
  if Assigned(MagnStar) and not NotMagnetic and not Disabled then
  begin
    CurParamNum := 0;
    case ReprMode of
      RM_BY_ONE : begin
        with MagnStar do
          TR := TRepresentation(Representations.Items[CurReprNum]);

        for i := 0 to TR.Count - 1 do begin
        
          TB := TBasisFunctions(TR.Items[i]);

          TempParameter.Value := TB.MixingConst;
          TempParameter.Limited := False;
          Parameter[CurParamNum] := TempParameter;
          Inc(CurParamNum);
        end;

        case VariationMode of
            VM_ALL_COMPONENTS : begin
                for i := 0 to AtomList.Count - 1 do
                begin
                    TA := TAtom(AtomList.Items[i]);
                    TempParameter.Value := TA.M;
                    
                    TempParameter.Limited := False;
                    Parameter[CurParamNum] := TempParameter;
                    Inc(CurParamNum);
                end;
            end;

            VM_EQUAL_MODULES :  begin
                AtomList.SetMeanModuleAtSite('ALL');
                TempParameter.Value := AtomList.GetMeanModuleAtSite('ALL');
                TempParameter.Limited := False;
                Parameter[CurParamNum] := TempParameter;
                Inc(CurParamNum);
            end;
        end;
      end;

      RM_BY_ALL : begin
        for i := 0 to MagnStar.Representations.Count - 1 do begin
          TR := TRepresentation(MagnStar.Representations.Items[i]);
          for j := 0 to TR.Count - 1 do begin
          
            TB := TBasisFunctions(TR.Items[j]);

            TempParameter.Value := TB.MixingConst;
            TempParameter.Limited := False;
            Parameter[CurParamNum] := TempParameter;
            Inc(CurParamNum);
          end;
        end;
        case VariationMode of
            VM_ALL_COMPONENTS : begin
                for i := 0 to AtomList.Count - 1 do
                begin
                    TA := TAtom(AtomList.Items[i]);
                    TempParameter.Value := TA.M;
                    
                    TempParameter.Limited := False;
                    Parameter[CurParamNum] := TempParameter;
                    Inc(CurParamNum);
                end;
            end;
            VM_EQUAL_MODULES :  begin
                AtomList.SetMeanModuleAtSite('ALL');
                TempParameter.Value := AtomList.GetMeanModuleAtSite('ALL');
                TempParameter.Limited := False;
                Parameter[CurParamNum] := TempParameter;
                Inc(CurParamNum);
            end;
        end;
      end;

      RM_DONT_USE : begin
        case VariationMode of
          VM_ALL_COMPONENTS, VM_EQUAL_MODULES : begin
            for i := 0 to AtomList.Count - 1 do
            begin
              TA := TAtom(AtomList.Items[i]);
              InVect := TA.Moment;
              
              ConvertAphineToDekart(A, B, C, Alpha, Beta, Gamma, InVect);
              MagnStar.DirectTransform(InVect, OutVect, Unchangeables);
              ConvertDekartToSpherical(
              OutVect[1], OutVect[2], OutVect[3], Theta, Phi, R);
              

              DisAxisNum := 0;
              if Unchangeables and CH_DIS_ANG_X <> 0 then Inc(DisAxisNum);
              if Unchangeables and CH_DIS_ANG_Y <> 0 then Inc(DisAxisNum);
              if Unchangeables and CH_DIS_ANG_Z <> 0 then Inc(DisAxisNum);

              case DisAxisNum of
                0 : begin

                  TempParameter.Value := Theta;
                  TempParameter.Limited := True;
                  TempParameter.MinLimit := 0;
                  TempParameter.MaxLimit := pi;
                  Parameter[CurParamNum] := TempParameter;
                  Inc(CurParamNum);

                  TempParameter.Value := Phi;
                  TempParameter.Limited := True;
                  TempParameter.MinLimit := -pi;
                  TempParameter.MaxLimit := pi;
                  Parameter[CurParamNum] := TempParameter;
                  Inc(CurParamNum);

                  if VariationMode = VM_ALL_COMPONENTS then begin

                    TempParameter.Value := TA.M;
                    
                    TempParameter.Limited := False;
                    Parameter[CurParamNum] := TempParameter;
                    Inc(CurParamNum);
                  end;
                end;

                1 : begin
                  
                  if Unchangeables and CH_DIS_ANG_X <> 0 then
                    raise ESite.Create('Feature doesn''t supported...');
                  if Unchangeables and CH_DIS_ANG_Y <> 0 then
                    raise ESite.Create('Feature doesn''t supported...');

                  if Unchangeables and CH_DIS_ANG_Z <> 0 then begin
                  
                    TempParameter.Value := Phi;
                    TempParameter.Limited := True;
                    TempParameter.MinLimit := -pi;
                    TempParameter.MaxLimit := pi;
                    Parameter[CurParamNum] := TempParameter;
                    Inc(CurParamNum);
                  end;

                  if VariationMode = VM_ALL_COMPONENTS then begin
                    TempParameter.Value := TA.M;
                    
                    TempParameter.Limited := False;
                    Parameter[CurParamNum] := TempParameter;
                    Inc(CurParamNum);
                  end;
                end;

                else begin
                  
                  if VariationMode = VM_ALL_COMPONENTS then begin
                    TempParameter.Value := TA.M;
                    
                    TempParameter.Limited := False;
                    Parameter[CurParamNum] := TempParameter;
                    Inc(CurParamNum);
                  end;
                end;
              end;
            end;{for i := 0 to AtomList.Count - 1 do...}

            if VariationMode = VM_EQUAL_MODULES then begin
            
              AtomList.SetMeanModuleAtSite('ALL');
              TempParameter.Value := AtomList.GetMeanModuleAtSite('ALL');
              TempParameter.Limited := False;
              Parameter[CurParamNum] := TempParameter;
              Inc(CurParamNum);
            end;
          end;

          VM_ONLY_MODULES : begin
            
            AtomList.SetMeanModuleAtSite('ALL');
            TempParameter.Value := AtomList.GetMeanModuleAtSite('ALL');
            TempParameter.Limited := False;
            Parameter[CurParamNum] := TempParameter;
            Inc(CurParamNum);
          end;
        end;{case VariationMode of...}
      end;
    end;{case ReprMode of...}
    for i := 0 to MagnStar.ParametersNumber - 1 do begin
    
      Parameter[CurParamNum] := MagnStar.Parameter[i];
      Inc(CurParamNum);
    end;
  end;{if Assigned(MagnStar) and not NotMagnetic and not Disabled then...}
end;

function TSite.GetNumberOfValues: LongInt;
var MagnStar: TWaveVector;
begin
    case ReprMode of
        RM_BY_ONE :
        begin
            MagnStar := WaveVectorList.GetMagnStar;
            if Assigned(MagnStar) and (not NotMagnetic) and
                (not Disabled) then Result := MagnStar.NumberOfValues
            else Result := 1;
        end;
        RM_BY_ALL, RM_DONT_USE : Result := 1;
    end;
end;

function TSite.GetValueIndex: LongInt;
var MagnStar: TWaveVector;
begin
    case ReprMode of
        RM_BY_ONE :
        begin
            MagnStar := WaveVectorList.GetMagnStar;
            if Assigned(MagnStar) and (not NotMagnetic) and
                (not Disabled) then Result := MagnStar.ValueIndex
            else Result := 0;
        end;
        RM_BY_ALL, RM_DONT_USE : Result := 0;
    end;
end;

procedure TSite.SetValueIndex(const AValueIndex: LongInt);
var MagnStar: TWaveVector;
begin
  if ReprMode = RM_BY_ONE then begin
    MagnStar := WaveVectorList.GetMagnStar;
    if Assigned(MagnStar) and (not NotMagnetic) and (not Disabled) then
      MagnStar.ValueIndex := AValueIndex;
  end;
end;

function TSite.GetAllowedVariationModes: Byte;
begin
  if (WaveVectorList.MagnStar = nil) or (ReprMode <> RM_DONT_USE) then
  Result := 0 else
    Result := EVMT_ALL_COMPONENTS or EVMT_EQUAL_MODULES or EVMT_ONLY_MODULES;
end;

function TSite.GetAllowedReprModes: Byte;
var MagnStar: TWaveVector;
begin
    MagnStar := WaveVectorList.MagnStar;

    if MagnStar = nil then Result := ERMT_DONT_USE
    else begin
        try
            MagnStar.Representations.IsReady;
        except
            Result := ERMT_DONT_USE;
            Exit
        end;
        Result := ERMT_BY_ONE or ERMT_BY_ALL or ERMT_DONT_USE;
    end;
end;

procedure TSite.SetVectorsFromStruct(Struct: TStructure);
var i: LongInt;
    SC: TAtom;
begin
  if AtomList.Count <> Struct.Count then
    raise ESite.Create('Invalid structure...');
  for i := 0 to AtomList.Count - 1 do
  begin
    SC := TAtom(AtomList.Items[i]);
    SC.Mx := Struct[i][1];
    SC.My := Struct[i][2];
    SC.Mz := Struct[i][3];
  end;
end;

procedure TSite.DeleteVector(const Index: LongInt);
begin
    WaveVectorList.DeleteVector(Index);
end;

procedure TSite.InsertNewVector(const Index: LongInt);
begin
    WaveVectorList.InsertNewVector(Index);
end;

procedure TSite.AddNewVector;
begin
    WaveVectorList.AddNewVector;
end;

procedure TSite.IsReady;
var MagnStar, StructStar: TWaveVector;
begin
    if not Disabled then
    begin
        StructStar := WaveVectorList.StructStar;
        MagnStar := WaveVectorList.MagnStar;

        if StructStar <> nil then StructStar.IsReady;        
        if (not NotMagnetic) and (MagnStar <> nil) then
        begin
            if (ReprMode = RM_BY_ONE) or (ReprMode = RM_BY_ALL) then
                MagnStar.SetSelfCheckingMode(SSC_REPR)
            else MagnStar.SetSelfCheckingMode(SSC_ONLY_SELF);
            MagnStar.IsReady;
            AtomList.IsReady;            
        end;
        if (StructStar = nil) and (MagnStar = nil) then
            raise ESite.Create('No star defined');
    end;
end;

function TSite.MyNameIs: string;
begin
    Result := 'the site (' + SiteName + ')';
end;

function TSite.IsModulesEqual: Boolean;
var i: LongInt;
    TS: TAtom;
    SavedModule: Double;
begin
  Result := True;
  with AtomList do
  begin
    TS := TAtom(Items[0]);
    SavedModule := TS.M;
    for i := 1 to Count - 1 do
    begin
      TS := TAtom(Items[i]);
      if Abs(TS.M - SavedModule) > TINY then
      begin Result := False; Exit end;
    end; {for i := 1 to Count - 1 do...}
  end; {with AtomList do...}
end;

function TSite.IsModulesReal: Boolean;
var i: LongInt;
    TS: TAtom;
begin
  Result := True;
  with AtomList do
  begin
    for i := 0 to Count - 1 do
    begin
      TS := TAtom(Items[i]);
      if TS.M > TINY then
      begin Result := False; Exit end;
    end; {for i := 1 to Count - 1 do...}
  end; {with AtomList do...}
end;

procedure TSite.GetWaveVectorSubChain(var First, Last: TWaveVector);
var i: LongInt;
    TW, TW2: TWaveVector;
begin
  if Assigned(WaveVectorList) then
  begin
    if WaveVectorList.Count <> 0 then
    begin
      with WaveVectorList do
      begin
        for i := 0 to Count - 2 do
        begin
          TW := TWaveVector(Items[i]);
          TW2 := TWaveVector(Items[i + 1]);
          TW.Next := TW2;
        end;
        First := TWaveVector(Items[0]);
        Last := TWaveVector(Items[Count - 1]);
      end;{with WaveVectorList do...}
    end{if WaveVectorList.Count <> 0 then...}
    else
    begin
      First := nil;
      Last := nil;
    end;
  end;{if Assigned(WaveVectorList) then...}
end;

function TSite.GetAtomCount: LongInt;
begin
    Result := AtomList.Count;
end;

function TSite.GetAtomList: TAtomList;
begin
    Result := FAtomList;
end;

function TSite.GetMinPropVectComponent(const CompNumber: LongInt): Double;
var i: LongInt;
    TW: TWaveVector;
    TempComp: Double;
begin
  Result := 1;
  for i := 0 to WaveVectorList.Count - 1 do
  begin
    TW := TWaveVector(WaveVectorList.Items[i]);
    if TW.IsPropVectMagnetic then
    begin
      TempComp := TW[CompNumber];
      PutValueIntoInterval(-0.5, 0.5, TempComp);
      if (Abs(TempComp) < Result) and (Abs(TempComp) <> 0) then
        Result := Abs(TempComp);
    end;
  end;
end;

function TSite.GetMagnAtomsNumber: LongInt;
begin
    Result := AtomCount * TransNumber;
end;

function TSite.GetMagnAtom(index: LongInt): TAtom;
var Translation, Moment: TDoubleVector3;
    TW: TWaveVector;
begin
    if index mod TransNumber = 0 then   Result := TAtom(AtomList.Items[index div TransNumber])
    else
    begin
        Result := TempAtom;
        TAtom(AtomList.Items[index div TransNumber]).CopyParameters(Result);
        Translation := GetTranslation(index mod TransNumber);
        Result.x := (Result.x + Translation[1]) / ChemicalXNumber;
        Result.y := (Result.y + Translation[2]) / ChemicalYNumber;
        Result.z := (Result.z + Translation[3]) / ChemicalZNumber;
        TW := WaveVectorList.MagnStar;
        if Assigned(TW) then
        begin
            Moment[1] := Result.Mx;
            Moment[2] := Result.My;
            Moment[3] := Result.Mz;

            Result.M := Result.M * TW.GetModuleTransFactor(
                Translation, Moment, Result.EllipseParam);

            TW.RotateMoment(Translation, Moment);
                Result.Mx := Moment[1];
            Result.My := Moment[2];
            Result.Mz := Moment[3];
        end;
    end;
end;

function TSite.GetMagnAtomInChemical(index: LongInt): TAtom;
var Translation, Moment: TDoubleVector3;
    TW: TWaveVector;
begin
    if index mod TransNumber = 0 then   Result := TAtom(AtomList.Items[index div TransNumber])
    else
    begin
        Result := TempAtom;
        TAtom(AtomList.Items[index div TransNumber]).CopyParameters(Result);
        Translation := GetTranslation(index mod TransNumber);
        Result.x := Result.x + Translation[1];
        Result.y := Result.y + Translation[2];
        Result.z := Result.z + Translation[3];
        TW := WaveVectorList.MagnStar;
        if Assigned(TW) then
        begin
            Moment[1] := Result.Mx;
            Moment[2] := Result.My;
            Moment[3] := Result.Mz;

            Result.M := Result.M * TW.GetModuleTransFactor(
                Translation, Moment, Result.EllipseParam);

            TW.RotateMoment(Translation, Moment);
            Result.Mx := Moment[1];
            Result.My := Moment[2];
            Result.Mz := Moment[3];
        end;
    end;
end;

function TSite.GetChemicalXNumber: LongInt;
var TempComp: Double;
begin
  TempComp := GetMinPropVectComponent(1);
  Result := Round(1 / TempComp);
end;

function TSite.GetChemicalYNumber: LongInt;
var TempComp: Double;
begin
  TempComp := GetMinPropVectComponent(2);
  Result := Round(1 / TempComp);
end;

function TSite.GetChemicalZNumber: LongInt;
var TempComp: Double;
begin
  TempComp := GetMinPropVectComponent(3);
  Result := Round(1 / TempComp);
end;

function TSite.GetTransNumber: LongInt;
begin
  Result := ChemicalXNumber * ChemicalYNumber * ChemicalZNumber;
end;

function TSite.GetTranslation(index: LongInt): TDoubleVector3;
begin
  
  Result[3] := index div (ChemicalXNumber * ChemicalYNumber);
  Result[2] := (index - Round(Result[3] * ChemicalXNumber *
  ChemicalYNumber)) div ChemicalXNumber;
  Result[1] := index - Result[3] * ChemicalXNumber *
  ChemicalYNumber - Result[2] * ChemicalXNumber;
end;

procedure TWaveVector.RotateMoment(
    const Translation: TDoubleVector3;
    var Moment: TDoubleVector3);
var TempMatrix: TMatrix;
    TempVect: TDoubleVector3;
begin
    GetUnitMatrix(TempMatrix);
    TempMatrix := GetRotMatrix(Translation);
    TempVect := Moment;

    with WaveVectorServer do
    begin
        ConvertAphineToDekart(A, B, C, Alpha, Beta, Gamma, TempVect);
        MulVectMatr(TempMatrix, TempVect);
        ConvertDekartToAphine(A, B, C, Alpha, Beta, Gamma, TempVect);
    end;

    Moment := TempVect;
end;

function TSite.GetGeneratorClass: TGeneratorClass;
begin
    if Assigned(FGeneratorClass) then Result := FGeneratorClass
    else raise ESite.Create('the generator class is not assigned...');
end;

function TSite.GetMySiteList: TSiteList;
begin
  if Assigned(FSiteList) then Result := FSiteList
  else raise ESite.Create('the sites list is not assigned...');
end;

procedure TSite.SetSiteList(const ASiteList: TSiteList);
begin
  FSiteList := ASiteList;
  AtomList.SetSiteList(ASiteList);
  TempAtom.SetSiteList(ASiteList);
end;

function TSite.GetA: Double;
begin
  Result := GetMySiteList.A;
end;

function TSite.GetB: Double;
begin
  Result := GetMySiteList.B;
end;

function TSite.GetC: Double;
begin
  Result := GetMySiteList.C;
end;

function TSite.GetMagnA: Double;
begin
  Result := A * ChemicalXNumber;
end;

function TSite.GetMagnB: Double;
begin
  Result := B * ChemicalYNumber;
end;

function TSite.GetMagnC: Double;
begin
  Result := C * ChemicalZNumber;
end;

function TSite.GetAlpha: Double;
begin
  Result := GetMySiteList.Alpha;
end;

function TSite.GetBeta: Double;
begin
  Result := GetMySiteList.Beta;
end;

function TSite.GetGamma: Double;
begin
  Result := GetMySiteList.Gamma;
end;

constructor TSite.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateAtomList;
  CreatePropVectorsList;
  Name := '';
  TempAtom := CreateNewAtom;
  TempAtom.SetSite(Self);
  VariationMode := VM_EQUAL_MODULES;
  ReprMode := RM_DONT_USE;
end;

procedure TSite.CreateAtomList;
begin
  ReplaceAtomList(CreateNewAtomList);
end;

procedure TSite.CreateHKLLists(
    const PatternAuxiliary: TPatternAuxiliary;
    const HKLList: THKLList;
    const AMinSinTL, AMaxSinTL: Double);
var StructStar, MagnStar: TWaveVector;
begin
    if not Disabled then
    begin
        StructStar := WaveVectorList.GetStructStar;
        MagnStar := WaveVectorList.GetMagnStar;
        if Assigned(StructStar) then
            StructStar.CreateHKLList(
                PatternAuxiliary, HKLList, AMinSinTL, AMaxSinTL);

        if not NotMagnetic then
            if Assigned(MagnStar) and (MagnStar <> StructStar) then
                MagnStar.CreateHKLList(
                    PatternAuxiliary, HKLList, AMinSinTL, AMaxSinTL);
    end;
end;

procedure TSite.LinkWithHKLList(const HKLList: THKLList);
var StructStar, MagnStar: TWaveVector;
begin
    if not Disabled then
    begin
        StructStar := WaveVectorList.GetStructStar;
        MagnStar := WaveVectorList.GetMagnStar;
        if Assigned(StructStar) then
            StructStar.LinkWithHKLList(HKLList);

        if not NotMagnetic then
            if Assigned(MagnStar) and (MagnStar <> StructStar) then
                MagnStar.LinkWithHKLList(HKLList);
    end;
end;

procedure TSite.ReplaceAtomList(const AtomList: TAtomList);
begin
    UtilizeObject(FAtomList);
    FAtomList := AtomList;
    FAtomList.SetSite(Self);
end;

procedure TSite.CreatePropVectorsList;
begin
  ReplacePropVectorsList(CreateNewWaveVectorList);
end;

procedure TSite.ReplacePropVectorsList(const PropVectorsList: TWaveVectorList);
begin
    UtilizeObject(FWaveVectorList);
    FWaveVectorList := PropVectorsList;
    FWaveVectorList.Site := Self;
end;

procedure TSite.AddPropVectorToList(const PropVector: TWaveVector);
begin
  FWaveVectorList.Add(PropVector);
end;

function TSite.GetCopy: TObject;
begin
  Result := CreateNewSite;
  CopyParameters(Result);
end;

function TSite.GetPlotParams: TSelfCopiedComponent;
begin
  Result := FPlotParams;
end;

procedure TSite.SetPlotParams(const APlotParams: TSelfCopiedComponent);
begin
  UtilizeObject(FPlotParams);
  FPlotParams := APlotParams;
end;

function TSiteList.GetCalcResults: TSelfCopiedComponent;
begin
  Result := FCalcResults;
end;

procedure TSiteList.SetCalcResults(const ACalcResults: TSelfCopiedComponent);
begin
  UtilizeObject(FCalcResults);
  FCalcResults := ACalcResults;
end;

function TSiteList.GetFullAtomsNumber: LongInt;

var i: LongInt;
begin
    Result := 0;
    for i := 0 to Count - 1 do
        Result := Result + TSite(Items[i]).MagnAtomsNumber;
end;

function TSiteList.GetAtomInChemical(index: LongInt): TAtom;

var TempArray: array of LongInt;
    i: LongInt;
    SiteIndex, AtomIndex: LongInt;
begin
     if (index < 0) or (index >= GetFullAtomsNumber) then
        raise ESiteList.Create('Invalid atom index...');
     SetLength(TempArray, Count);
     for i := 0 to Count - 1 do
         TempArray[i] := TSite(Items[i]).MagnAtomsNumber;
     GetPosInArrays(TempArray, index, SiteIndex, AtomIndex);
     Finalize(TempArray);
     Result := TSite(Items[SiteIndex]).MagnAtomInChemical[AtomIndex];
end;

procedure TSite.CopyParameters(const Dest: TObject);
begin
    TSite(Dest).SiteName := SiteName;
    TSite(Dest).Disabled := Disabled;
    TSite(Dest).NotMagnetic := NotMagnetic;
    TSite(Dest).ReprMode := ReprMode;
    TSite(Dest).VariationMode := VariationMode;

    AtomList.CopyParameters(TSite(Dest).AtomList);
    WaveVectorList.CopyParameters(TSite(Dest).WaveVectorList);

    end;

procedure TSite.LinkAtomWithSiteList(const Atom: TAtom);
begin
    Atom.SetSiteList(FSiteList);
end;

procedure TSite.LinkAtomsWithSite;
var i: LongInt;
    TA: TAtom;
begin
    with AtomList do
        for i := 0 to Count - 1 do
        begin
            TA := TAtom(Items[i]);
            TA.SetSite(Self);
            LinkAtomWithSiteList(TA);
        end;
end;

destructor TSite.Destroy;
begin
    UtilizeObject(FAtomList);
    UtilizeObject(FWaveVectorList);
    UtilizeObject(FGeneratorClass);
    UtilizeObject(TempAtom);
    UtilizeObject(FPlotParams);
    inherited Destroy;
end;

function TSiteList.GetObjectIdentifier(const ObjNum: LongInt): string;
begin
    Result := '';
    if Assigned(Items[ObjNum]) then
        if Items[ObjNum] is TSite then Result := TSite(Items[ObjNum]).SiteName;
end;

function TSiteList.GetWaveVectorsChainBegin: TWaveVector;
var i: LongInt;
    TS: TSite;
    TW: TWaveVector;
    First: TWaveVector;
    Last, SaveLast: TWaveVector;
    Flag: Boolean;
begin
  TW := nil; First := nil; Last := nil; SaveLast := nil;
  Flag := True;
  for i := 0 to Count - 1 do
  begin
    TS := TSite(Items[i]);
    if not TS.Disabled then
    begin
      TS.GetWaveVectorSubChain(First, Last);
      if Flag then begin TW := First; Flag := False end;
      if Assigned(SaveLast) then SaveLast.Next := First;
      SaveLast := Last;
    end;
  end;
  if Assigned(SaveLast) then SaveLast.Next := nil;
  Result := TW;
end;

procedure TSiteList.SetAtomMomentsCur;
var i: LongInt;
    TS: TSite;
begin
  for i := 0 to Count - 1 do
  begin
    TS := TSite(Items[i]);
    if (not TS.Disabled) and (not TS.NotMagnetic) then TS.SetAtomMomentsCur;
  end;
end;

procedure TSiteList.SetAtomMomentsAll;
var i: LongInt;
    TS: TSite;
begin
  for i := 0 to Count - 1 do
  begin
    TS := TSite(Items[i]);
    if (not TS.Disabled) and (not TS.NotMagnetic) then TS.SetAtomMomentsAll;
  end;
end;

procedure TSiteList.MulModules(const MulConst: Double);
var i: LongInt;
    TS: TSite;
begin
  for i := 0 to Count - 1 do
  begin
    TS := TSite(Items[i]);
    if (not TS.Disabled) and (not TS.NotMagnetic) then
      TS.MulModules(MulConst);
  end;
end;

procedure TSiteList.ResetSitesModules;
var i: LongInt;
    Site: TSite;
begin
  for i := 0 to Count - 1 do
  begin
    Site := TSite(Items[i]);
    Site.SiteModule := 0;
  end;
end;

procedure TSiteList.IncludeAllBasalFunctions;
var i: LongInt;
    TS: TSite;
begin
  for i := 0 to Count - 1 do
  begin
    TS := TSite(Items[i]);
    if (not TS.Disabled) and (not TS.NotMagnetic) then
      TS.IncludeAllBasalFunctions;
  end;
end;

procedure TSiteList.IsReady;
var i: LongInt;
    TS: TSite;
begin
    inherited;
    if (A = 0) or (B = 0) or (C = 0) or (Alpha = 0)
    or (Beta = 0) or (Gamma = 0) then
        raise ESiteList.Create('Invalid cell parameters');
    if Count <> 0 then begin
        for i := 0 to Count - 1 do
        begin
            TS := TSite(Items[i]);
            if not TS.Disabled then Exit;
        end;
        raise ESiteList.Create('At least one site must be enabled')
    end else
        raise ESiteList.Create('At least one site must be created')
end;

procedure TSiteList.ExcludeAllBasalFunctions;
var i: LongInt;
    TS: TSite;
begin
  for i := 0 to Count - 1 do
  begin
    TS := TSite(Items[i]);
    if (not TS.Disabled) and (not TS.NotMagnetic) then
      TS.ExcludeAllBasalFunctions;
  end;
end;

procedure TSiteList.CreateHKLLists(
    const PatternAuxiliary: TPatternAuxiliary;
    const HKLList: THKLList;
    const AMinSinTL, AMaxSinTL: Double);
var i: LongInt;
    TS: TSite;
begin
    for i := 0 to Count - 1 do
    begin
        TS := TSite(Items[i]);
        TS.CreateHKLLists(PatternAuxiliary, HKLList, AMinSinTL, AMaxSinTL);
    end;
end;

procedure TSiteList.LinkWithHKLList(const HKLList: THKLList);
var i: LongInt;
    TS: TSite;
begin
    for i := 0 to Count - 1 do
    begin
        TS := TSite(Items[i]);
        TS.LinkWithHKLList(HKLList);
    end;
end;

procedure TSiteList.ResetMixingConst;
var i: LongInt;
    TS: TSite;
begin
  for i := 0 to Count - 1 do
  begin
    TS := TSite(Items[i]);
    if (not TS.Disabled) and (not TS.NotMagnetic) then TS.ResetMixingConst;
  end;
end;

procedure TSite.ResetMixingConst;
var i, j, k: LongInt;
    TR: TRepresentation;
    TB: TBasisFunctions;
    TW: TWaveVector;
begin
  for j := 0 to WaveVectorList.Count - 1 do
  begin
    TW := TWaveVector(WaveVectorList.Items[j]);
    for k := 0 to TW.Representations.Count - 1 do
    begin
      TR := TRepresentation(TW.Representations.Items[k]);
      for i := 0 to TR.Count - 1 do
      begin
        TB := TBasisFunctions(TR.Items[i]);
        TB.MixingConst := 0;
      end; {for i := 0 to TR.Count - 1 do...}
    end; {for k := 0 to TW.Representations.Count - 1 do...}
  end; {for j := 0 to WaveVectorList.Count - 1 do...}
end;

procedure TSite.IncludeAllBasalFunctions;
var i, j, k: LongInt;
    TR: TRepresentation;
    TB: TBasisFunctions;
    TW: TWaveVector;
begin
  for j := 0 to WaveVectorList.Count - 1 do
  begin
    TW := TWaveVector(WaveVectorList.Items[j]);
    for k := 0 to TW.Representations.Count - 1 do
    begin
      TR := TRepresentation(TW.Representations.Items[k]);
      for i := 0 to TR.Count - 1 do
      begin
        TB := TBasisFunctions(TR.Items[i]);
        TB.Excluded := False;
      end; {for i := 0 to TR.Count - 1 do...}
    end; {for k := 0 to TW.Representations.Count - 1 do...}
  end; {for j := 0 to WaveVectorList.Count - 1 do...}
end;

procedure TSite.ExcludeAllBasalFunctions;
var i, j, k: LongInt;
    TR: TRepresentation;
    TB: TBasisFunctions;
    TW: TWaveVector;
begin
  for j := 0 to WaveVectorList.Count - 1 do
  begin
    TW := TWaveVector(WaveVectorList.Items[j]);
    for k := 0 to TW.Representations.Count - 1 do
    begin
      TR := TRepresentation(TW.Representations.Items[k]);
      for i := 0 to TR.Count - 1 do
      begin
        TB := TBasisFunctions(TR.Items[i]);
        TB.Excluded := True;
      end; {for i := 0 to TR.Count - 1 do...}
    end; {for k := 0 to TW.Representations.Count - 1 do...}
  end; {for j := 0 to WaveVectorList.Count - 1 do...}
end;

function TSiteList.GetCopy: TObject;
begin
    Result := CreateNewSiteList;
    CopyParameters(Result);
end;

destructor TSiteList.Destroy;
begin
    UtilizeObject(FPlotParams);
    UtilizeObject(FCalcResults);
    inherited;
end;

function TSiteList.GetPlotParams: TSelfCopiedComponent;
begin
    Result := FPlotParams;
end;

procedure TSiteList.SetPlotParams(const APlotParams: TSelfCopiedComponent);
begin
    UtilizeObject(FPlotParams);
    FPlotParams := APlotParams;
end;

procedure TSiteList.CopyParameters(const Dest: TObject);
begin
    inherited;
    TSiteList(Dest).A := A;
    TSiteList(Dest).B := B;
    TSiteList(Dest).C := C;
    TSiteList(Dest).Alpha := Alpha;
    TSiteList(Dest).Beta := Beta;
    TSiteList(Dest).Gamma := Gamma;

    if (GetCalcResults <> nil) and (TSiteList(Dest).GetCalcResults <> nil) then
        GetCalcResults.CopyParameters(TSiteList(Dest).GetCalcResults);
    end;

procedure TSiteList.LinkItemWithList(const Item: TComponent);
begin
    with Item as TSite do SetSiteList(Self);
end;

procedure TWaveVectorList.DeleteVector(const Index: LongInt);
var i: LongInt;
    TS: TWaveVector;
begin
    for i := 0 to Count - 1 do
    begin
        TS := TWaveVector(Items[i]);
        TS.DeleteVector(Index);
    end;
end;

procedure TWaveVectorList.InsertNewVector(const Index: LongInt);
var i: LongInt;
    TS: TWaveVector;
begin
    for i := 0 to Count - 1 do
    begin
        TS := TWaveVector(Items[i]);
        TS.InsertNewVector(Index);
    end;
end;

procedure TWaveVectorList.AddNewVector;
var i: LongInt;
    TS: TWaveVector;
begin
    for i := 0 to Count - 1 do
    begin
        TS := TWaveVector(Items[i]);
        TS.AddNewVector;
    end;
end;

function TWaveVectorList.GetObjectIdentifier(const ObjNum: LongInt): string;
begin
  Result := '';
  if Assigned(Items[ObjNum]) then
    if Items[ObjNum] is TWaveVector then
      with Items[ObjNum] as TWaveVector do
        Result := AsString;
end;

function TWaveVector.AsString: string;
begin
    Result := DoubleVector3AsString(FWaveVector, True, 3, 2);
end;

procedure TWaveVectorList.LinkItemWithList(const Item: TComponent);
begin
    with Item as TWaveVector do
    begin
        WaveVectorList := Self;
        WaveVectorServer := Site;
    end;
end;

procedure TWaveVectorList.SetSite(const ASite: TSite);
begin
  FSite := ASite;
  LinkAllItemsWithList;
  
end;

function TWaveVectorList.GetAtomCount: LongInt;
begin
  Result := 0;
  if Assigned(Site) then
    if Site is TSite then
      Result := TSite(Site).AtomCount;
end;

function TWaveVectorList.GetMagnStar: TWaveVector;
var i: LongInt;
    TW: TWaveVector;
begin
  Result := nil;
  for i := 0 to Count - 1 do begin
    TW := TWaveVector(Items[i]);
    if TW.IsPropVectMagnetic then begin Result := TW; Exit end;
  end;
end;

function TWaveVectorList.GetStructStar: TWaveVector;
var i: LongInt;
    TW: TWaveVector;
begin
  Result := nil;
  for i := 0 to Count - 1 do begin
    TW := TWaveVector(Items[i]);
    if TW.IsPropVectStructural then begin Result := TW; Exit end;
  end;
end;

procedure TWaveVectorList.StarStateChanged(const Star: TWaveVector);
var i: LongInt;
    TempStar: TWaveVector;
begin
  if IndexOf(Star) <> -1 then
  begin
    for i := 0 to Count - 1 do
    begin
      TempStar := TWaveVector(Items[i]);
      if TempStar <> Star then
      begin
        if TempStar.IsPropVectMagnetic and Star.IsPropVectMagnetic then
          with TempStar do
            PropVectorType := PropVectorType and not PVT_MAGNETIC;

        if TempStar.IsPropVectStructural and Star.IsPropVectStructural then
          with TempStar do
            PropVectorType := PropVectorType and not PVT_STRUCTURAL;
      end;
    end;
  end;
end;

function TRepresentationList.GetObjectIdentifier(const ObjNum: LongInt): String;
begin
  Result := '';
  if Assigned(Items[ObjNum]) then
    if Items[ObjNum] is TRepresentation then Result := 'Tau ' +
    IntToStr(ObjNum + 1);
end;

procedure TRepresentationList.DeleteVector(const Index: LongInt);
var i: LongInt;
    TR: TRepresentation;
begin
    for i := 0 to Count - 1 do
    begin
        TR := TRepresentation(Items[i]);
        TR.DeleteVector(Index);
    end;
end;

procedure TRepresentationList.InsertNewVector(const Index: LongInt);
var i: LongInt;
    TR: TRepresentation;
begin
    for i := 0 to Count - 1 do
    begin
        TR := TRepresentation(Items[i]);
        TR.InsertNewVector(Index);
    end;
end;

procedure TRepresentationList.AddNewVector;
var i: LongInt;
    TR: TRepresentation;
begin
    for i := 0 to Count - 1 do
    begin
        TR := TRepresentation(Items[i]);
        TR.AddNewVector;
    end;
end;

procedure TRepresentationList.IsReady;
begin
    inherited;  if Count = 0 then
        raise ERepresentationList.Create('The list of representations is empty');
end;

procedure TRepresentationList.SendErrorMessage(
    const ExceptionMsg, ObjectName: string;
    const ItemNumber: LongInt);
var Str: string;
begin
    Str := ExceptionMsg + ' in ' + ObjectName + ' ' +
        GetObjectIdentifier(ItemNumber);
    if MyNameIs <> '' then Str := Str + ' in ' + MyNameIs;
    raise ERepresentationList.Create(Str);
end;

procedure TRepresentationList.LinkItemWithList;
begin
    with Item as TRepresentation do
    begin
        Representations := Self;
        if not (csLoading in ComponentState) and
            not LinkItemDisabled then VectorCount := AtomCount;
        end;
end;

function TRepresentationList.GetAtomCount: LongInt;
begin
    Result := 0;
    if Assigned(WaveVector) then
        if WaveVector is TWaveVector then
            Result := TWaveVector(WaveVector).AtomCount;
end;

procedure TBasisFunctions.CopyParameters(const Dest: TObject);
var i: LongInt;
    TempVect: TDoubleVector3;
begin
    inherited;
    TBasisFunctions(Dest).VectorsCount := VectorsCount;
    for i := 0 to VectorsCount - 1 do begin
        TempVect := BasisFunction[i];
        TBasisFunctions(Dest)[i] := TempVect;TBasisFunctions(Dest).FBasisFunctionsIm[i] := BasisFunctionIm[i];
    end;
    TBasisFunctions(Dest).MixingConst := MixingConst;
    TBasisFunctions(Dest).Excluded := Excluded;
    TBasisFunctions(Dest).Disabled := Disabled;
end;

procedure TBasisFunctions.SetBasisFunction(Index: Integer;
BasisFunction: TDoubleVector3);
begin
    if Index >= VectorsCount then VectorsCount := Index + 1;
    if Index < VectorsCount then FBasisFunctions[Index] := BasisFunction;
end;

procedure TBasisFunctions.SetBasisFunctionIm(Index: Integer;
BasisFunction: TDoubleVector3);
begin
    if Index >= VectorsCount then VectorsCount := Index + 1;
    if Index < VectorsCount then FBasisFunctionsIm[Index] := BasisFunction;
end;

function  TBasisFunctions.GetBasisFunction(Index: Integer): TDoubleVector3;
var TempVect: TDoubleVector3;
begin
    TempVect[1] := 0; TempVect[2] := 0; TempVect[3] := 0;
    if Assigned(FBasisFunctions) then
        if Index < VectorsCount then TempVect := FBasisFunctions[Index];
    Result := TempVect;
end;

function  TBasisFunctions.GetBasisFunctionIm(Index: Integer): TDoubleVector3;
var TempVect: TDoubleVector3;
begin
    TempVect[1] := 0; TempVect[2] := 0; TempVect[3] := 0;
    if Assigned(FBasisFunctionsIm) then
        if Index < VectorsCount then TempVect := FBasisFunctionsIm[Index];
    Result := TempVect;
end;

procedure TBasisFunctions.InsertVector(const Index: LongInt;
    const ReVector, ImVector: TDoubleVector3);
begin
    InsertVectorIntoArray(TVector3Array(FBasisFunctions), Index, ReVector);
    InsertVectorIntoArray(TVector3Array(FBasisFunctionsIm), Index, ImVector);
end;

procedure TBasisFunctions.DeleteVector(const Index: LongInt);
begin
    DeleteVectorFromArray(TVector3Array(FBasisFunctions), Index);
    DeleteVectorFromArray(TVector3Array(FBasisFunctionsIm), Index);
end;

procedure TBasisFunctions.AddVector(
    const ReVector, ImVector: TDoubleVector3);
begin
    AddVectorToArray(TVector3Array(FBasisFunctions), ReVector);
    AddVectorToArray(TVector3Array(FBasisFunctionsIm), ImVector);
end;

procedure TBasisFunctions.InsertNewVector(const Index: LongInt);
var ReVector, ImVector: TDoubleVector3;
begin
    ReVector[1] := 0; ReVector[2] := 0; ReVector[3] := 0;
    ImVector[1] := 0; ImVector[2] := 0; ImVector[3] := 0;
    InsertVector(Index, ReVector, ImVector);
end;

procedure TBasisFunctions.AddNewVector;
var ReVector, ImVector: TDoubleVector3;
begin
    ReVector[1] := 0; ReVector[2] := 0; ReVector[3] := 0;
    ImVector[1] := 0; ImVector[2] := 0; ImVector[3] := 0;
    AddVector(ReVector, ImVector);
end;

destructor TBasisFunctions.Destroy;
begin
    Finalize(FBasisFunctions);
    Finalize(FBasisFunctionsIm);
    inherited Destroy;
end;

procedure TBasisFunctions.SetVectorsCount(ACount: LongInt);
var SavedLength: LongInt;
    i, j: LongInt;
begin
    SavedLength := Length(FBasisFunctions);
    SetLength(FBasisFunctions, ACount);
    SetLength(FBasisFunctionsIm, ACount);
    if ACount > SavedLength then
        for i := SavedLength to ACount - 1 do
            for j := 1 to 3 do
            begin
                FBasisFunctions[i, j] := 0;
                FBasisFunctionsIm[i, j] := 0;
            end;
end;

function TBasisFunctions.GetVectorsCount: LongInt;
begin
    Result := Length(FBasisFunctions);
end;

function TBasisFunctions.SaveModules: Boolean;
var i: LongInt;
    TempVect: TDoubleVector3;
    M, M2: Double;
    Flag: Boolean;
begin
    Result := True; Flag := False;
    for i := 0 to VectorsCount - 1 do
    begin
        TempVect := FBasisFunctions[i];
        M2 := Sqrt(Sqr(TempVect[1]) + Sqr(TempVect[2]) + Sqr(TempVect[3]));
        if M2 > TINY then
        
        begin
            if not Flag then
            begin
                M := M2; Flag := True;
            end else
                if Abs(M2 - M) > TINY then begin Result := False; Exit end;
        end;
    end;
end;

function TBasisFunctions.HasVacuity: Boolean;
var i: LongInt;
    TempVect: TDoubleVector3;
    M: Double;
begin
    Result := False;
    for i := 0 to VectorsCount - 1 do
    begin
        TempVect := FBasisFunctions[i];
        M := Sqrt(Sqr(TempVect[1]) + Sqr(TempVect[2]) + Sqr(TempVect[3]));
        if M <= TINY then begin Result := True; Exit end;
    end;{for i := 0 to Count - 1 do...}
end;

function TBasisFunctions.IsInteger: Boolean;
var i, j: LongInt;
begin
    Result := True;
    for i := 0 to VectorsCount - 1 do
        for j := 1 to 3 do
            if Abs(Frac(FBasisFunctions[i][j])) > TINY then
            begin Result := False; Exit end;
end;

function TBasisFunctions.GetVacuities: TDynamicLongArray;
var i: LongInt;
    TempVect: TDoubleVector3;
    M: Double;
begin
    Result := nil;
    for i := 0 to VectorsCount - 1 do
    begin
        TempVect := FBasisFunctions[i];
        M := Sqrt(Sqr(TempVect[1]) + Sqr(TempVect[2]) + Sqr(TempVect[3]));
        if M <= TINY then
        begin
            SetLength(Result, Length(Result) + 1);
            Result[Length(Result) - 1] := i;
        end;
    end;{for i := 0 to Count - 1 do...}
end;

function TBasisFunctions.FillingsAndVacuitiesCoincide(
const BF: TBasisFunctions): Boolean;
var i: LongInt;
    TempVect: TDoubleVector3;
    M, M2: Double;
begin
    if BF.VectorsCount <> VectorsCount then
        raise EBasisFunctions.Create('Invalid basis function...');
    Result := True;
    for i := 0 to VectorsCount - 1 do
    begin
        TempVect := BasisFunction[i];
        M := Sqrt(Sqr(TempVect[1]) + Sqr(TempVect[2]) + Sqr(TempVect[3]));
        TempVect := BF[i];
        M2 := Sqrt(Sqr(TempVect[1]) + Sqr(TempVect[2]) + Sqr(TempVect[3]));
        if ((M > TINY) and (M2 < TINY)) or
           ((M < TINY) and (M2 > TINY)) then begin Result := False; Exit end;
    end;{for i := 0 to Count - 1 do...}
end;

function TBasisFunctions.GetFillings: TDynamicLongArray;
var i: LongInt;
    TempVect: TDoubleVector3;
    M: Double;
begin
    Result := nil;
    for i := 0 to VectorsCount - 1 do
    begin
        TempVect := FBasisFunctions[i];
        M := Sqrt(Sqr(TempVect[1]) + Sqr(TempVect[2]) + Sqr(TempVect[3]));
        if M > TINY then
        begin
            SetLength(Result, Length(Result) + 1);
            Result[Length(Result) - 1] := i;
        end;
    end;{for i := 0 to Count - 1 do...}
end;

procedure TRepresentation.SetColContents(Grid: TStringGrid; ColNum: LongInt);
var BF: TBasisFunctions;
    i: LongInt;
    TempVect: TDoubleVector3;
    St: string;
begin
    with Grid do
        if (ColNum - FixedCols >= 0) and (ColNum - FixedCols < ColCount) then
        begin
            BF := TBasisFunctions(Items[ColNum - FixedCols]);
            for i := 0 to BF.VectorsCount - 1 do
            begin
                TempVect := BF[i];
                St := Vector3ToStr(TempVect);
                Cells[ColNum, FixedRows + i] := St;
            end;    end
        else for i := FixedRows to RowCount - 1 do Cells[ColNum, i] := '';
end;

function TRepresentation.GetColContents(Grid: TStringGrid;
    ColNum: LongInt): Boolean;
var BF: TBasisFunctions;
    i: LongInt;
    TempVect: TDoubleVector3;
    St: string;
begin
    with Grid do
        if (ColNum - FixedCols >= 0) and (ColNum - FixedCols < Count) then
        begin
            BF := TBasisFunctions(Items[ColNum - FixedCols]);
            for i := FixedRows to RowCount - 1 do
            begin
                St := Cells[ColNum, i];
                try
                    TempVect := StrToVector3(St);
                    BF[i - FixedRows] := TempVect;
                except
                end;
            end;    end;
    Result := True;
end;

function TRepresentation.BFsInUseNumber: LongInt;
var i: LongInt;
    BF: TBasisFunctions;
begin
    Result := 0;
    for i := 0 to Count - 1 do
    begin
        BF := TBasisFunctions(Items[i]);
        if not BF.Disabled and not BF.Excluded then Result := Result + 1;
    end;
end;

procedure TRepresentation.DeleteVector(const Index: LongInt);
begin
    _DeleteVector(Index);
    Dec(FVectorCount);  if AreRowHeightsReady then
    begin
        DeleteRowHeightItem(Index + GetFixedRows);
        if VectorCount = 0 then AddRowHeightItem;
    end;
end;

procedure TRepresentation.InsertNewVector(const Index: LongInt);
var Flag: Boolean;
begin
    Flag := VectorCount = 0;
    _InsertNewVector(Index);
    Inc(FVectorCount);  if AreRowHeightsReady and (not Flag) then
        InsertRowHeightItem(Index + GetFixedRows);
end;

procedure TRepresentation.AddNewVector;
var Flag: Boolean;
begin
    Flag := VectorCount = 0;
    _AddNewVector;
    Inc(FVectorCount);  if AreRowHeightsReady and (not Flag) then AddRowHeightItem;
end;

procedure TRepresentation.RandMixFactors;
var i: LongInt;
    BF: TBasisFunctions;
begin
    Randomize;
    for i := 0 to Count - 1 do
    begin
        BF := TBasisFunctions(Items[i]);
        BF.MixingConst := Random;
    end;
end;

procedure TRepresentation.IsReady;
begin
    inherited;
    if Count = 0 then
        raise ERepresentation.Create('The list of basal functions is empty');
end;

function TRepresentation.MyNameIs: string;
begin
    Result := 'the representation'
end;

procedure TRepresentation.SetCaption(Grid: TStringGrid);
var i: LongInt;
begin
    with Grid do
    begin
        if FixedRows <> 0 then
            begin
                Cells[0, 0] := 'Num.\B.F.';
                for i := FixedCols to ColCount - 1 do
                    Cells[i, 0] := IntToStr(i - FixedCols + 1);
            end;

        if FixedCols <> 0 then
            for i := FixedRows to RowCount - 1 do
                Cells[0, i] := IntToStr(i - FixedRows + 1);
    end;
end;

procedure TRepresentation.SetColOptions(Grid: TStringGrid);
var i: LongInt;
begin
    if Grid is TNumericGrid then
        with Grid as TNumericGrid do
            for i := FixedCols to ColCount - 1 do ColOptions[i] := coText;
end;

procedure TRepresentation.LinkItemWithList(const Item: TComponent);
begin
    if not (csLoading in ComponentState) then
        with Item as TBasisFunctions do VectorsCount := VectorCount;
end;

function TRepresentation.CreateNewObject: TComponent;
begin
    Result := CreateNewBasisFunction;
end;

destructor TGeneratorClass.Destroy;
begin
  UtilizeObject(FBFList);
  UtilizeObject(FStructuresList);
  inherited Destroy;
end;

function TGeneratorClass.GetBFList: TComponentList;
begin
  if Assigned(FBFList) then Result := FBFList
  else raise EGeneratorClass.Create('List of basis functions is not assigned...');
end;

function TGeneratorClass.GetStructuresList: TComponentList;
begin
  if Assigned(FStructuresList) then Result := FStructuresList
  else raise EGeneratorClass.Create('List of structures is not assigned...');
end;

function TStructure.GetVectors(index: LongInt): TDoubleVector3;
begin
  if index < Count then Result := FVectors[index]
  else raise EStructure.Create('Vector index out of range...');
end;

procedure TStructure.SetVectors(index: LongInt; AVector: TDoubleVector3);
begin
  if index < Count then FVectors[index] := AVector
  else raise EStructure.Create('Vector index out of range...');
end;

function TStructure.GetCount: LongInt;
begin
  Result := Length(FVectors);
end;

procedure TStructure.SetCount(ACount: LongInt);
var SavedLength: LongInt;
    i, j: LongInt;
begin
  SavedLength := Length(FVectors);
  SetLength(FVectors, ACount);
  if ACount > SavedLength then
    for i := SavedLength to ACount - 1 do
      for j := 1 to 3 do FVectors[i, j] := 0;
end;

destructor TStructure.Destroy;
begin
  Finalize(FVectors);
  Finalize(FBFNumbers);
  UtilizeObject(FDiffrPattern);
  inherited Destroy;
end;

function TStructure.SaveModules: Boolean;
var i: LongInt;
    TempVect: TDoubleVector3;
    M, M2: Double;
    Flag: Boolean;
begin
  Result := True; Flag := False;
  for i := 0 to Count - 1 do
  begin
    TempVect := FVectors[i];
    M2 := Sqrt(Sqr(TempVect[1]) + Sqr(TempVect[2]) + Sqr(TempVect[3]));
    if M2 > TINY then
    
    begin
      if not Flag then
      begin
        M := M2; Flag := True;
      end else if Abs(M2 - M) > TINY then begin Result := False; Exit end;
    end;
  end;
end;

function TStructure.HasVacuity: Boolean;
var i: LongInt;
    TempVect: TDoubleVector3;
    M: Double;
begin
  Result := False;
  for i := 0 to Count - 1 do
  begin
    TempVect := FVectors[i];
    M := Sqrt(Sqr(TempVect[1]) + Sqr(TempVect[2]) + Sqr(TempVect[3]));
    if M <= TINY then begin Result := True; Exit end;
  end;{for i := 0 to Count - 1 do...}
end;

function TStructure.GetVacuities: TDynamicLongArray;
var i: LongInt;
    TempVect: TDoubleVector3;
    M: Double;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    TempVect := FVectors[i];
    M := Sqrt(Sqr(TempVect[1]) + Sqr(TempVect[2]) + Sqr(TempVect[3]));
    if M <= TINY then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[Length(Result) - 1] := i;
    end;
  end;{for i := 0 to Count - 1 do...}
end;

function TStructure.GetFillings: TDynamicLongArray;
var i: LongInt;
    TempVect: TDoubleVector3;
    M: Double;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    TempVect := FVectors[i];
    M := Sqrt(Sqr(TempVect[1]) + Sqr(TempVect[2]) + Sqr(TempVect[3]));
    if M > TINY then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[Length(Result) - 1] := i;
    end;
  end;{for i := 0 to Count - 1 do...}
end;

function TStructure.FillingsDoNotCoincide(const BF: TBasisFunctions): Boolean;
var i: LongInt;
    TempVect: TDoubleVector3;
    StructM, BFM: Double;
begin
    if BF.VectorsCount <> Count then
        raise EStructure.Create('Invalid basis function...');
    Result := True;
    for i := 0 to Count - 1 do
    begin
        TempVect := FVectors[i];
        StructM := Sqrt(Sqr(TempVect[1]) + Sqr(TempVect[2]) + Sqr(TempVect[3]));
        TempVect := BF[i];
        BFM := Sqrt(Sqr(TempVect[1]) + Sqr(TempVect[2]) + Sqr(TempVect[3]));
        
        if (BFM > TINY) and (StructM > TINY) then begin Result := False; Exit end;
    end;{for i := 0 to Count - 1 do...}
end;

function TStructure.VacuitiesCoincide(const BF: TBasisFunctions): Boolean;
var i: LongInt;
    TempVect: TDoubleVector3;
    M, M2: Double;
begin
    if BF.VectorsCount <> Count then
        raise EStructure.Create('Invalid basis function...');
    Result := False;
    for i := 0 to Count - 1 do
    begin
        TempVect := FVectors[i];
        M := Sqrt(Sqr(TempVect[1]) + Sqr(TempVect[2]) + Sqr(TempVect[3]));
        TempVect := BF[i];
        M2 := Sqrt(Sqr(TempVect[1]) + Sqr(TempVect[2]) + Sqr(TempVect[3]));
        if (M <= TINY) and (M2 <= TINY) then begin Result := True; Exit end;
    end;{for i := 0 to Count - 1 do...}
end;

function TStructure.IsVectorsOrthogonal(const TS: TStructure): Boolean;
var i: LongInt;
    TempVect, TempVect2: TDoubleVector3;
begin
  if TS.Count <> Count then
    raise EStructure.Create('Invalid structure...');
  Result := True;
  for i := 0 to Count - 1 do
  begin
    TempVect := FVectors[i];
    TempVect2 := TS[i];
    if Abs(TempVect[1] * TempVect2[1] + TempVect[2] * TempVect2[2] +
           TempVect[3] * TempVect2[3]) > TINY then
    begin Result := False; Exit end;
  end;
end;

function TStructure.HasOppositeVectors(const TS: TStructure): Boolean;
var i: LongInt;
    TempVect, TempVect2: TDoubleVector3;
begin
  if TS.Count <> Count then
    raise EStructure.Create('Invalid structure...');
  Result := False;
  for i := 0 to Count - 1 do
  begin
    TempVect := FVectors[i];
    TempVect2 := TS[i];
    if (Abs(TempVect[1] + TempVect2[1]) < TINY) and
       (Abs(TempVect[2] + TempVect2[2]) < TINY) and
       (Abs(TempVect[3] + TempVect2[3]) < TINY) then
    begin Result := True; Exit end;
  end;
end;

procedure TStructure.ModulesFlatten;
var i: LongInt;
    TempVect: TDoubleVector3;
    M, M2: Double;
begin
  TempVect := Vectors[0];
  M := Sqr(TempVect[1]) + Sqr(TempVect[2]) + Sqr(TempVect[3]);
  M := Sqrt(M);
  for i := 1 to Count - 1 do
  begin
    TempVect := Vectors[i];
    M2 := Sqrt(Sqr(TempVect[1]) + Sqr(TempVect[2]) + Sqr(TempVect[3]));
    
    if Abs(M - M2) > TINY then
    begin
      TempVect[1] := TempVect[1] * M / M2;
      TempVect[2] := TempVect[2] * M / M2;
      TempVect[3] := TempVect[3] * M / M2;
      Vectors[i] := TempVect;
    end;
  end;{for i := 1 to Count - 1 do...}
end;

procedure TStructure.SetMomentModule(const MomentModule: Double);
var i: LongInt;
    TempVect: TDoubleVector3;
    M2: Double;
begin
  for i := 0 to Count - 1 do
  begin
    TempVect := Vectors[i];
    M2 := Sqrt(Sqr(TempVect[1]) + Sqr(TempVect[2]) + Sqr(TempVect[3]));
    
    TempVect[1] := TempVect[1] * MomentModule / M2;
    TempVect[2] := TempVect[2] * MomentModule / M2;
    TempVect[3] := TempVect[3] * MomentModule / M2;
    Vectors[i] := TempVect;
  end;{for i := 0 to Count - 1 do...}
end;

function TStructure.GetCopy: TStructure;
var i: LongInt;
    TempVect: TDoubleVector3;
begin
  Result := TStructure.Create(nil);
  Result.Count := Count;
  for i := 0 to Count - 1 do Result.Vectors[i] := Vectors[i];
  for i := 0 to BFNumbersCount - 1 do Result.AddBFNumber(BFNumbers[i]);
end;

function TStructure.GetBFNumbers(index: ShortInt): ShortInt;
begin
  if index < BFNumbersCount then Result := FBFNumbers[index]
  else raise EStructure.Create('BFNumbers index out of range...');
end;

function TStructure.GetBFNumbersCount: ShortInt;
begin
  Result := Length(FBFNumbers);
end;

procedure TStructure.AddBFNumber(const BFNumber: ShortInt);
begin
  SetLength(FBFNumbers, Length(FBFNumbers) + 1);
  FBFNumbers[Length(FBFNumbers) - 1] := BFNumber;
end;

function TStructure.GetGoodPeaksNumber: LongInt;
begin
  if not Assigned(FDiffrPattern) then
    raise EStructure.Create('Diffraction pattern is not assigned...')
  else Result := FGoodPeaksNumber;
end;

destructor TPossibleModel.Destroy;
begin
  Finalize(StructIndexes);
  Finalize(SitesIndexes);
  inherited Destroy;
end;

function TPossibleModel.GetStructNum(const SiteNum: LongInt): LongInt;
var i: LongInt;
begin
  for i := 0 to SitesCount - 1 do
  begin
    if SiteIndex[i] = SiteNum then
    begin Result := StructIndex[i]; Exit end;
  end;
  raise EPossibleModel.Create('Site number ' +
  IntToStr(SiteNum) + ' not found...');
end;

procedure TPossibleModel.AddStructToModel(const SiteIndex, StructIndex: LongInt);
begin
  if HasThisSiteIndex(SiteIndex) then
    raise EPossibleModel.Create('Site index already exists...')
  else
  begin
    SetLength(SitesIndexes, Length(SitesIndexes) + 1);
    SitesIndexes[Length(SitesIndexes) - 1] := SiteIndex;
    SetLength(StructIndexes, Length(StructIndexes) + 1);
    StructIndexes[Length(StructIndexes) - 1] := StructIndex;
  end;
end;

function TPossibleModel.GetSitesCount: LongInt;
begin
  Result := Length(SitesIndexes);
end;

function TPossibleModel.GetSiteIndex(index: LongInt): LongInt;
begin
  if (index < 0) or (index >= Length(SitesIndexes)) then
    raise EPossibleModel.Create('Site index out of range...')
  else Result := SitesIndexes[index];
end;

function TPossibleModel.GetStructIndex(index: LongInt): LongInt;
begin
  if (index < 0) or (index >= Length(StructIndexes)) then
    raise EPossibleModel.Create('Struct. index out of range...')
  else Result := StructIndexes[index];
end;

function TPossibleModel.HasThisSiteIndex(const SiteIndex: LongInt): Boolean;
var i: LongInt;
begin
  Result := False;
  for i := 0 to Length(SitesIndexes) - 1 do
    if SiteIndex = SitesIndexes[i] then begin Result := True; Exit end;
end;

function TPossibleModel.GetCopy: TPossibleModel;
var i: LongInt;
begin
  Result := TPossibleModel.Create(nil);
  for i := 0 to SitesCount - 1 do
    Result.AddStructToModel(SiteIndex[i], StructIndex[i]);
end;

function HKLSetSortFunc(Item1, Item2: Pointer): Integer;
var NR1: THKLClass absolute Item1;
    NR2: THKLClass absolute Item2;
begin
  if NR1.SinTL < NR2.SinTL then begin Result := -1; Exit end;
  if NR1.SinTL > NR2.SinTL then begin Result :=  1; Exit end;
  if NR1.SinTL = NR2.SinTL then begin Result :=  0; Exit end;
end;

procedure TCalcResults.CopyParameters(const Dest: TObject);
begin
  inherited;
  TCalcResults(Dest).RFactor1 := RFactor1;
  TCalcResults(Dest).RFactor2 := RFactor2;
end;

class function TCalcResults.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := CRClassInheritID;
    Result.PropVersionNum := CRCurVerNum;
end;

class procedure TCalcResults.ReadProperties(const Reader: TReader;
    const PropHeaderRec: TPropHeaderRec;
    const AnObject: TSelfSavedComponent);
begin
    with AnObject as Self, Reader do
    begin
        FRFactor1 := ReadFloat;
        FRFactor2 := ReadFloat;
    end;
end;

function THKLList.CreateNewHKL(
    const PatternAuxiliary: TPatternAuxiliary;
    const Vector: TDoubleVector3;
    var ReallyNew: Boolean): THKLClass;
var i: LongInt;
    HC: THKLClass;
    TempVect: TDoubleVector3;
const TINY = 1e-6;
begin
    for i := 0 to Count - 1 do
    begin
        HC := THKLClass(Items[i]);
        if (Abs(HC.WVH - Vector[1]) < TINY) and (Abs(HC.WVK - Vector[2]) < TINY)
        and (Abs(HC.WVL - Vector[3]) < TINY) then
        begin Result := HC; ReallyNew := False; Exit end;
    end;
    Result := CreateNewHKLClass;
    ReallyNew := True;

    with Result, PatternAuxiliary do
    begin
        WVH := Vector[1];
        WVK := Vector[2];
        WVL := Vector[3];

        SinTL := GetSinTL(WVH, WVK, WVL);
        SinT := SinTL * Lambda;
        Sin2T := Sin(2 * Arctan(Sqrt(Sqr(SinT) / (1 - Sqr(SinT)))));
        MulConst := 1;
        GetUnitScatVect(WVH, WVK, WVL, TempVect);
        UnitScatVect := TempVect;
    end;

    Add(Result);
end;

procedure THKLList.CalcMagneticIntensities;
var i: LongInt;
    HC: THKLClass;
begin
    for i := 0 to Count - 1 do
    begin
        HC := THKLClass(Items[i]);
        HC.CalcMagneticIntensity;
    end;
end;

procedure THKLList.CalcNuclearIntensities;
var i: LongInt;
    HC: THKLClass;
begin
    for i := 0 to Count - 1 do
    begin
        HC := THKLClass(Items[i]);
        HC.CalcNuclearIntensity;
    end;
end;

function CreateNewSite: TSite;
var SPP: TSPP;
begin
     Result := TS.Create(nil);
     SPP := TSPP.Create(nil);
     SPP.SetSite(Result);
     Result.SetPlotParams(SPP);
end;

function CreateNewSiteList: TSiteList;
var SLPP: TSLPP;
begin
    Result := TSL.Create(nil);
    SLPP := TSLPP.Create(nil);
    SLPP.SetSiteList(Result);
    Result.SetPlotParams(SLPP);
    Result.SetCalcResults(TCR.Create(nil));
end;

function CreateNewSinTCompList: TSinTCompList;
begin
    Result := TSTCL.Create(nil);
    Result.SetCalcResults(TCR.Create(nil));
end;

function CreateNewHKLClass: THKLClass;
begin
     Result := THKLC.Create(nil);
end;

function CreateNewAtom: TAtom;
begin
     Result := TA.Create(nil);
end;

function CreateNewAtomList: TAtomList;
begin
     Result := TAL.Create(nil);
end;

function CreateNewBasisFunction: TBasisFunctions;
begin
     Result := TBF.Create(nil);
end;

function CreateNewGeneralClass: TGeneralClass;
begin
    Result := TGC.Create(nil);
end;

function CreateNewNeutronClass: TNeutronClass;
begin
    Result := TNC.Create(nil);
end;

function CreateNewNeutronClassMono: TNeutronClassMono;
begin
    Result := TNCM.Create(nil);
end;

function CreateNewNeutronCompList: TNeutronCompList;
begin
    Result := TNCL.Create(nil);
end;

function CreateNewNeutronCompListMono: TNeutronCompListMono;
begin
    Result := TNCLM.Create(nil);
end;

function CreateNewRepr: TRepresentation;
begin
    Result := TR.Create(nil);
end;

function CreateNewReprList: TRepresentationList;
begin
     Result := TRL.Create(nil);
end;

function CreateNewWaveVector: TWaveVector;
begin
     Result := TWV.Create(nil);
end;

function CreateNewWaveVectorList: TWaveVectorList;
begin
     Result := TWVL.Create(nil);
end;

function CreateNewSinTClass: TSinTClass;
begin
    Result := TSTC.Create(nil);
end;

function CreateNewCommentsClass: TCommentsClass;
begin
    Result := TCC.Create(nil);
end;

function CreateNewPatternParams: TPatternParametersContainer;
begin
    Result := TPPC.Create(nil);
end;

procedure TScattSphereList.FindSpheres(
    const PatternAuxiliary: TPatternAuxiliary;
    const HKLList: THKLList);
var i, j: LongInt;
    TH: THKLClass;
    TSS: TScattSphere;
    Module: Double;
    SphereFound: Boolean;
const TINY = 1e-6;
begin
    Clear;

    with PatternAuxiliary do
        for i := 0 to HKLList.Count - 1 do
        begin
            TH := THKLClass(HKLList.Items[i]);

            with HKLList do
                Module := GetScatVectModule(TH.WVH, TH.WVK, TH.WVL);

            SphereFound := False;
            for j := 0 to Count - 1 do
            begin
                TSS := TScattSphere(Items[j]);
                if Abs(TSS.Radius - Module) < TINY then
                begin TSS.Add(TH); SphereFound := True; Break end;
            end;

            if not SphereFound then
            
            begin
                TSS := TScattSphere.Create(nil);
                TSS.SetState(cfPassive);    TSS.Radius := Module;
                TSS.FSinT := TH.SinT;
                TSS.FSinTL := TH.SinTL;
                TSS.FSin2T := TH.Sin2T;
                TSS.Add(TH);
                Add(TSS);
            end;
        end;
end;

class function TCommentsClass.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := CCClassInheritID;
    Result.PropVersionNum := CCClassCurVerNum;
end;

class procedure TCommentsClass.ReadProperties(const Reader: TReader;
  const PropHeaderRec: TPropHeaderRec;
  const AnObject: TSelfSavedComponent);
begin
    with Reader, TCommentsClass(AnObject) do
    begin
        List.Clear;

        ReadListBegin;
        while not EndOfList do List.Add(ReadString);
        ReadListEnd;

        Caption := ReadString;
    end;
end;

class procedure TCommentsClass.WriteProperties(const Writer: TWriter;
    const AnObject: TSelfSavedComponent);
var i: LongInt;
begin
    with Writer, TCommentsClass(AnObject) do
    begin
        WriteListBegin;
        for i := 0 to List.Count - 1 do
            WriteString(List.Strings[i]);
        WriteListEnd;

        WriteString(Caption);
    end;
end;

class function TSite.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := SClassInheritID;
    Result.PropVersionNum := SCurVerNum;
end;

class procedure TSite.ReadProperties(const Reader: TReader;
    const PropHeaderRec: TPropHeaderRec;
    const AnObject: TSelfSavedComponent);
begin
    with AnObject as Self, Reader do
    begin
        ReplaceAtomList(TAtomList(ReadComponentByReader(Reader)));
        ReplacePropVectorsList(TWaveVectorList(ReadComponentByReader(Reader)));
        FPlotParams := TSelfCopiedComponent(Reader.ReadComponent(nil));
        SiteName := Reader.ReadString;
        FDisabled := ReadBoolean;
        FNotMagnetic := ReadBoolean;
        FVariationMode := ReadInteger;
        FReprMode := ReadInteger;

        if PropHeaderRec.PropVersionNum = 1 then
        begin
            Element := ReadString;
            NuclearScatAmpl := ReadFloat;
        end;

        LinkAtomsWithSite;
        SetBFVectorCount(AtomList.Count);
    end;
end;

class procedure TSite.WriteProperties(const Writer: TWriter;
    const AnObject: TSelfSavedComponent);
begin
    with AnObject as Self, Writer do
    begin
        WriteComponent(FAtomList);
        WriteComponent(FWaveVectorList);
        WriteComponent(FPlotParams);
        WriteString(SiteName);
        WriteBoolean(FDisabled);
        WriteBoolean(FNotMagnetic);
        WriteInteger(FVariationMode);
        WriteInteger(FReprMode);

        WriteString(Element);
        WriteFloat(NuclearScatAmpl);
    end;
end;

{ TS }

class function TS.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := SAlClassInheritID;
    Result.PropVersionNum := SAlCurVerNum;
end;

class procedure TS.ReadProperties(const Reader: TReader;
    const PropHeaderRec: TPropHeaderRec;
    const AnObject: TSelfSavedComponent);
begin

end;

class procedure TS.WriteProperties(const Writer: TWriter;
    const AnObject: TSelfSavedComponent);
begin

end;

class function TWaveVector.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := WVClassInheritID;
    Result.PropVersionNum := WVCurVerNum;
end;

class procedure TWaveVector.ReadProperties(const Reader: TReader;
    const PropHeaderRec: TPropHeaderRec;
    const AnObject: TSelfSavedComponent);
var i: LongInt;
begin
    with AnObject as Self, Reader do
    begin
        UtilizeObject(FRepresentations);
        FRepresentations := TRepresentationList(ReadComponentByReader(Reader));
        FRepresentations.WaveVector := AnObject;
        for i := 1 to 3 do WaveVector[i] := ReadFloat;
        PropVectorType := ReadInteger;
        StarType := Reader.ReadInteger;
        TransTypeIndex := Reader.ReadInteger;
        for i := 1 to 3 do FRotAxis[i] := Reader.ReadFloat;
        end;
end;

class procedure TWaveVector.WriteProperties(const Writer: TWriter;
    const AnObject: TSelfSavedComponent);
var i: LongInt;
begin
    with AnObject as Self, Writer do
    begin
        WriteComponent(Representations);
        for i := 1 to 3 do WriteFloat(FWaveVector[i]);
        WriteInteger(PropVectorType);
        WriteInteger(StarType);
        WriteInteger(TransTypeIndex);
        for i := 1 to 3 do WriteFloat(FRotAxis[i]);
    end;
end;

{ TWV }

class function TWV.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := WVAlClassInheritID;
    Result.PropVersionNum := WVAlCurVerNum;
end;

class procedure TWV.ReadProperties(const Reader: TReader;
    const PropHeaderRec: TPropHeaderRec;
    const AnObject: TSelfSavedComponent);
begin

end;

class procedure TWV.WriteProperties(const Writer: TWriter;
    const AnObject: TSelfSavedComponent);
begin

end;

class function TAtom.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := AClassInheritID;
    Result.PropVersionNum := ACurVerNum;
end;

class procedure TAtom.ReadProperties(const Reader: TReader;
    const PropHeaderRec: TPropHeaderRec;
    const AnObject: TSelfSavedComponent);
begin
    with AnObject as Self, Reader do
    begin
        Fx := ReadFloat;
        Fy := ReadFloat;
        Fz := ReadFloat;
        FMx := ReadFloat;
        FMy := ReadFloat;
        FMz := ReadFloat;
        FDispX := ReadFloat;
        FDispY := ReadFloat;
        FDispZ := ReadFloat;
        FNuclearScatAmpl := ReadFloat;
        FEllipseParam := ReadFloat;
        FM := ReadFloat;

        FNumber := ReadInteger;
        FElement := ReadString;
        FPosition := ReadString;
        DirectionChanged := True;
    end;
end;

class procedure TAtom.WriteProperties(const Writer: TWriter;
    const AnObject: TSelfSavedComponent);
begin
    with AnObject as Self, Writer do
    begin
        WriteFloat(Fx);
        WriteFloat(Fy);
        WriteFloat(Fz);
        WriteFloat(FMx);
        WriteFloat(FMy);
        WriteFloat(FMz);
        WriteFloat(FDispX);
        WriteFloat(FDispY);
        WriteFloat(FDispZ);
        WriteFloat(FNuclearScatAmpl);
        WriteFloat(FEllipseParam);
        WriteFloat(FM);

        WriteInteger(FNumber);
        WriteString(FElement);
        WriteString(FPosition);
    end;
end;

{ TA }

class function TA.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := AAlClassInheritID;
    Result.PropVersionNum := AAlCurVerNum;
end;

class procedure TA.ReadProperties(const Reader: TReader;
    const PropHeaderRec: TPropHeaderRec;
    const AnObject: TSelfSavedComponent);
begin

end;

class procedure TA.WriteProperties(const Writer: TWriter;
    const AnObject: TSelfSavedComponent);
begin

end;

class function TNeutronClass.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := NCClassInheritID;
    Result.PropVersionNum := NCCurVerNum;
end;

class procedure TNeutronClass.ReadProperties(const Reader: TReader;
    const PropHeaderRec: TPropHeaderRec;
    const AnObject: TSelfSavedComponent);
begin
    with AnObject as Self, Reader do
    begin
        if PropHeaderRec.PropVersionNum = 0 then ReadFloat;

        Intensity := ReadFloat;
        StartPos := ReadFloat;
        FinishPos := ReadFloat;
        PeakPos := ReadFloat;
        IntCorrFactor := ReadFloat;
    end;
end;

class procedure TNeutronClass.WriteProperties(const Writer: TWriter;
    const AnObject: TSelfSavedComponent);
begin
    with AnObject as Self, Writer do
    begin
        WriteFloat(Intensity);
        WriteFloat(StartPos);
        WriteFloat(FinishPos);
        WriteFloat(PeakPos);
        WriteFloat(IntCorrFactor);
    end;
end;

{ TNC }

class function TNC.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := NCAlClassInheritID;
    Result.PropVersionNum := NCAlCurVerNum;
end;

class procedure TNC.ReadProperties(const Reader: TReader;
    const PropHeaderRec: TPropHeaderRec;
    const AnObject: TSelfSavedComponent);
begin

end;

class procedure TNC.WriteProperties(const Writer: TWriter;
    const AnObject: TSelfSavedComponent);
begin

end;

class function TPatternParametersContainer.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := PPCClassInheritID;
    Result.PropVersionNum := PPCCurVerNum;
end;

class procedure TPatternParametersContainer.ReadProperties(
    const Reader: TReader; const PropHeaderRec: TPropHeaderRec;
    const AnObject: TSelfSavedComponent);
begin
    with AnObject as Self, Reader do
    begin
        FLambda := ReadFloat;
        FStartPos := ReadFloat;
        FEndPos := ReadFloat;
        FUseStartEnd := ReadBoolean;
    end;
end;

class procedure TPatternParametersContainer.WriteProperties(
    const Writer: TWriter; const AnObject: TSelfSavedComponent);
begin
    with AnObject as Self, Writer do
    begin
        WriteFloat(FLambda);
        WriteFloat(FStartPos);
        WriteFloat(FEndPos);
        WriteBoolean(FUseStartEnd);
    end;
end;

{ TPPC }

class function TPPC.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := PPCAlClassInheritID;
    Result.PropVersionNum := PPCAlCurVerNum;
end;

class procedure TPPC.ReadProperties(const Reader: TReader;
    const PropHeaderRec: TPropHeaderRec;
    const AnObject: TSelfSavedComponent);
begin

end;

class procedure TPPC.WriteProperties(const Writer: TWriter;
    const AnObject: TSelfSavedComponent);
begin

end;

class function THKLClass.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := HKLClassInheritID;
    Result.PropVersionNum := HKLCurVerNum;
end;

class procedure THKLClass.ReadProperties(const Reader: TReader;
    const PropHeaderRec: TPropHeaderRec;
    const AnObject: TSelfSavedComponent);
begin
    with AnObject as Self, Reader do
    begin
        FWVH := ReadFloat;
        FWVK := ReadFloat;
        FWVL := ReadFloat;
        FIntensity := ReadFloat;
        FNuclearIntensity := ReadFloat;
        FMulConst := ReadFloat;
        FSinT := ReadFloat;
        FSin2T := ReadFloat;
        FSinTL := ReadFloat;
        FReF := ReadFloat;
        FImF := ReadFloat;
    end;
end;

class procedure THKLClass.WriteProperties(const Writer: TWriter;
    const AnObject: TSelfSavedComponent);
begin
    with AnObject as Self, Writer do
    begin
        WriteFloat(FWVH);
        WriteFloat(FWVK);
        WriteFloat(FWVL);
        WriteFloat(FIntensity);
        WriteFloat(FNuclearIntensity);
        WriteFloat(FMulConst);
        WriteFloat(FSinT);
        WriteFloat(FSin2T);
        WriteFloat(FSinTL);
        WriteFloat(FReF);
        WriteFloat(FImF);
    end;
end;

{ THKLC }

class function THKLC.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := HKLAlClassInheritID;
    Result.PropVersionNum := HKLAlCurVerNum;
end;

class procedure THKLC.ReadProperties(const Reader: TReader;
    const PropHeaderRec: TPropHeaderRec;
    const AnObject: TSelfSavedComponent);
begin

end;

class procedure THKLC.WriteProperties(const Writer: TWriter;
    const AnObject: TSelfSavedComponent);
begin

end;

{ TSTC }

class function TSTC.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := STAlClassInheritID;
    Result.PropVersionNum := STAlCurVerNum
end;

class procedure TSTC.ReadProperties(const Reader: TReader;
    const PropHeaderRec: TPropHeaderRec;
    const AnObject: TSelfSavedComponent);
begin

end;

class procedure TSTC.WriteProperties(const Writer: TWriter;
    const AnObject: TSelfSavedComponent);
begin

end;

class function TSinTClass.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := STClassInheritID;
    Result.PropVersionNum := STCurVerNum;
end;

class procedure TSinTClass.ReadProperties(const Reader: TReader;
    const PropHeaderRec: TPropHeaderRec;
    const AnObject: TSelfSavedComponent);
begin
    with AnObject as Self, Reader do
    begin
        FHKLList := TSelfCopiedCompList(ReadComponentByReader(Reader));
        FExpIntensity := ReadFloat;
        FIntensity := ReadFloat;
        FNuclearIntensity := ReadFloat;

        FSinT := ReadFloat;
        FSinTL := ReadFloat;
        FSin2T := ReadFloat;

        FRFactor := ReadFloat;
        FStartPos := ReadFloat;
        FFinishPos := ReadFloat;
        FPeakPos := ReadFloat;
    end;
end;

class procedure TSinTClass.WriteProperties(const Writer: TWriter;
    const AnObject: TSelfSavedComponent);
begin
    with AnObject as Self, Writer do
    begin
        Writer.WriteComponent(HKLList);
        WriteFloat(FExpIntensity);
        WriteFloat(FIntensity);
        WriteFloat(FNuclearIntensity);

        WriteFloat(FSinT);
        WriteFloat(FSinTL);
        WriteFloat(FSin2T);

        WriteFloat(FRFactor);
        WriteFloat(FStartPos);
        WriteFloat(FFinishPos);
        WriteFloat(FPeakPos);
    end;
end;

{ TGC }

class function TGC.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := GAlClassInheritID;
    Result.PropVersionNum := GAlCurVerNum;
end;

class procedure TGC.ReadProperties(const Reader: TReader;
    const PropHeaderRec: TPropHeaderRec;
    const AnObject: TSelfSavedComponent);
begin

end;

class procedure TGC.WriteProperties(const Writer: TWriter;
    const AnObject: TSelfSavedComponent);
begin

end;

{ TCR }

class function TCR.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := CRAlClassInheritID;
    Result.PropVersionNum := CRAlCurVerNum;
end;

class procedure TCR.ReadProperties(const Reader: TReader;
    const PropHeaderRec: TPropHeaderRec;
    const AnObject: TSelfSavedComponent);
begin

end;

class procedure TCR.WriteProperties(const Writer: TWriter;
    const AnObject: TSelfSavedComponent);
begin

end;

{ TBF }

class function TBF.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := BFAlClassInheritID;
    Result.PropVersionNum := BFAlCurVerNum;
end;

class procedure TBF.ReadProperties(const Reader: TReader;
    const PropHeaderRec: TPropHeaderRec;
    const AnObject: TSelfSavedComponent);
begin

end;

class procedure TBF.WriteProperties(const Writer: TWriter;
    const AnObject: TSelfSavedComponent);
begin

end;

class function TGeneralClass.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := GClassInheritID;
    Result.PropVersionNum := GCurVerNum;
end;

class procedure TGeneralClass.ReadProperties(const Reader: TReader;
    const PropHeaderRec: TPropHeaderRec;
    const AnObject: TSelfSavedComponent);
begin
    with AnObject as Self, Reader do Caption := ReadString;
end;

class procedure TGeneralClass.WriteProperties(const Writer: TWriter;
    const AnObject: TSelfSavedComponent);
begin
    with AnObject as Self, Writer do WriteString(Caption);
end;

class function TBasisFunctions.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := BFClassInheritID;
    Result.PropVersionNum := BFCurVerNum;
end;

class procedure TBasisFunctions.ReadProperties(const Reader: TReader;
    const PropHeaderRec: TPropHeaderRec;
    const AnObject: TSelfSavedComponent);
var j: LongInt;
    TempVect: TDoubleVector3;
    TempCount: LongInt;
begin
    with AnObject as Self, Reader do
    begin
        ReadListBegin;
        TempCount := 0;
        while not EndOfList do
        begin
            for j := 1 to 3 do TempVect[j] := ReadFloat;
            BasisFunction[TempCount] := TempVect;
            TempCount := TempCount + 1;
        end;
        ReadListEnd;

        ReadListBegin;
        TempCount := 0;
        while not EndOfList do
        begin
            for j := 1 to 3 do TempVect[j] := ReadFloat;
            BasisFunctionIm[TempCount] := TempVect;
            TempCount := TempCount + 1;
        end;
        ReadListEnd;

        MixingConst := ReadFloat;
        Excluded := ReadBoolean;
        Disabled := ReadBoolean;
    end;
end;

class procedure TBasisFunctions.WriteProperties(const Writer: TWriter;
    const AnObject: TSelfSavedComponent);
var i, j: LongInt;
    TempVect: TDoubleVector3;
begin
    with AnObject as Self, Writer do
    begin
        WriteListBegin;
        for i := 0 to VectorsCount - 1 do
        begin
            TempVect := BasisFunction[i];
            for j := 1 to 3 do WriteFloat(TempVect[j]);
        end;
        WriteListEnd;

        WriteListBegin;
        for i := 0 to VectorsCount - 1 do
        begin
            TempVect := BasisFunctionIm[i];
            for j := 1 to 3 do WriteFloat(TempVect[j]);
        end;
        WriteListEnd;

        WriteFloat(MixingConst);
        WriteBoolean(Excluded);
        WriteBoolean(Disabled);
    end;
end;

class procedure TCalcResults.WriteProperties(const Writer: TWriter;
    const AnObject: TSelfSavedComponent);
begin
    with AnObject as Self, Writer do
    begin
        WriteFloat(FRFactor1);
        WriteFloat(FRFactor2);
    end;
end;

class function THKLList.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := HKLLClassInheritID;
    Result.PropVersionNum := HKLLCurVerNum;
end;

class procedure THKLList.ReadProperties(const Reader: TReader;
    const PropHeaderRec: TPropHeaderRec;
    const AnObject: TSelfSavedComponent);
begin

end;

class procedure THKLList.WriteProperties(const Writer: TWriter;
    const AnObject: TSelfSavedComponent);
begin

end;

{ TScattSphere }

class function TScattSphere.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := SSClassInheritID;
    Result.PropVersionNum := SSCurVerNum;
end;

class procedure TScattSphere.ReadProperties(const Reader: TReader;
    const PropHeaderRec: TPropHeaderRec;
    const AnObject: TSelfSavedComponent);
begin

end;

class procedure TScattSphere.WriteProperties(const Writer: TWriter;
    const AnObject: TSelfSavedComponent);
begin

end;

{ TAL }

class function TAL.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := ALAlClassInheritID;
    Result.PropVersionNum := ALAlCurVerNum;
end;

class procedure TAL.ReadProperties(const Reader: TReader;
    const PropHeaderRec: TPropHeaderRec;
    const AnObject: TSelfSavedComponent);
begin

end;

class procedure TAL.WriteProperties(const Writer: TWriter;
    const AnObject: TSelfSavedComponent);
begin

end;

{ TNCL }

class function TNCL.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := NCLAlClassInheritID;
    Result.PropVersionNum := NCLAlCurVerNum;
end;

class procedure TNCL.ReadProperties(const Reader: TReader;
    const PropHeaderRec: TPropHeaderRec;
    const AnObject: TSelfSavedComponent);
begin

end;

class procedure TNCL.WriteProperties(const Writer: TWriter;
    const AnObject: TSelfSavedComponent);
begin

end;

{ TSTCL }

class function TSTCL.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := STCLAlClassInheritID;
    Result.PropVersionNum := STCLAlCurVerNum
end;

class procedure TSTCL.ReadProperties(const Reader: TReader;
    const PropHeaderRec: TPropHeaderRec;
    const AnObject: TSelfSavedComponent);
begin

end;

class procedure TSTCL.WriteProperties(const Writer: TWriter;
    const AnObject: TSelfSavedComponent);
begin

end;

{ TR }

class function TR.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := RAlClassInheritID;
    Result.PropVersionNum := RAlCurVerNum;
end;

class procedure TR.ReadProperties(const Reader: TReader;
    const PropHeaderRec: TPropHeaderRec;
    const AnObject: TSelfSavedComponent);
begin

end;

class procedure TR.WriteProperties(const Writer: TWriter;
    const AnObject: TSelfSavedComponent);
begin

end;

class function TAtomList.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := ALClassInheritID;
    Result.PropVersionNum := ALCurVerNum;
end;

class procedure TAtomList.ReadProperties(const Reader: TReader;
    const PropHeaderRec: TPropHeaderRec;
    const AnObject: TSelfSavedComponent);
begin

end;

class procedure TAtomList.WriteProperties(const Writer: TWriter;
    const AnObject: TSelfSavedComponent);
begin

end;

class function TNeutronCompList.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := NCLClassInheritID;
    Result.PropVersionNum := NCLCurVerNum;
end;

class procedure TNeutronCompList.ReadProperties(const Reader: TReader;
    const PropHeaderRec: TPropHeaderRec;
    const AnObject: TSelfSavedComponent);
begin
    with AnObject as Self, Reader do
    begin
        UtilizeObject(FPatternParameters);
        FPatternParameters := TPatternParametersContainer(
            ReadComponentByReader(Reader));
    end;
end;

class procedure TNeutronCompList.WriteProperties(const Writer: TWriter;
    const AnObject: TSelfSavedComponent);
begin
    with AnObject as Self, Writer do
        WriteComponent(FPatternParameters);
end;

class function TSinTCompList.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := STCLClassInheritID;
    Result.PropVersionNum := STCLCurVerNum;
end;

class procedure TSinTCompList.ReadProperties(const Reader: TReader;
    const PropHeaderRec: TPropHeaderRec;
    const AnObject: TSelfSavedComponent);
begin
    with AnObject as Self, Reader do
    begin
        FCalcResults := TSelfCopiedComponent(ReadComponentByReader(Reader));
        MulIntParam := ReadFloat;
        DWParam := ReadFloat;
        Lambda := ReadFloat;
        IFType := ReadInteger;
    end;
end;

class procedure TSinTCompList.WriteProperties(const Writer: TWriter;
    const AnObject: TSelfSavedComponent);
begin
    with AnObject as Self, Writer do
    begin
        WriteComponent(FCalcResults);
        WriteFloat(MulIntParam);
        WriteFloat(DWParam);
        WriteFloat(Lambda);
        WriteInteger(IFType);
    end;
end;

class function TRepresentation.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := RClassInheritID;
    Result.PropVersionNum := RCurVerNum;
end;

class procedure TRepresentation.ReadProperties(const Reader: TReader;
    const PropHeaderRec: TPropHeaderRec;
    const AnObject: TSelfSavedComponent);
begin
    with Reader, AnObject as TRepresentation do
        case PropHeaderRec.PropVersionNum of
            1 : ReadFloat;
            2 : VectorCount := ReadInteger;
        end;
end;

class procedure TRepresentation.WriteProperties(const Writer: TWriter;
    const AnObject: TSelfSavedComponent);
begin
    with Writer, AnObject as TRepresentation do
        WriteInteger(VectorCount);
end;

{ TSL }

class function TSL.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := SLAlClassInheritID;
    Result.PropVersionNum := SLAlCurVerNum;
end;

class procedure TSL.ReadProperties(const Reader: TReader;
    const PropHeaderRec: TPropHeaderRec;
    const AnObject: TSelfSavedComponent);
begin

end;

class procedure TSL.WriteProperties(const Writer: TWriter;
    const AnObject: TSelfSavedComponent);
begin

end;

{ TRL }

class function TRL.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := RLAlClassInheritID;
    Result.PropVersionNum := RLAlCurVerNum;
end;

class procedure TRL.ReadProperties(const Reader: TReader;
    const PropHeaderRec: TPropHeaderRec;
    const AnObject: TSelfSavedComponent);
begin

end;

class procedure TRL.WriteProperties(const Writer: TWriter;
    const AnObject: TSelfSavedComponent);
begin

end;

{ TWVL }

class function TWVL.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := WVLAlClassInheritID;
    Result.PropVersionNum := WVLAlCurVerNum;
end;

class procedure TWVL.ReadProperties(const Reader: TReader;
    const PropHeaderRec: TPropHeaderRec;
    const AnObject: TSelfSavedComponent);
begin

end;

class procedure TWVL.WriteProperties(const Writer: TWriter;
    const AnObject: TSelfSavedComponent);
begin

end;

class function TSiteList.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := SLClassInheritID;
    Result.PropVersionNum := SLCurVerNum;
end;

class procedure TSiteList.ReadProperties(const Reader: TReader;
    const PropHeaderRec: TPropHeaderRec;
    const AnObject: TSelfSavedComponent);
begin
    with AnObject as Self, Reader do
    begin
        FCalcResults := TSelfCopiedComponent(ReadComponentByReader(Reader));
        FPlotParams := TSelfCopiedComponent(ReadComponentByReader(Reader));
        FCaption := ReadString;
        FA := ReadFloat;
        FB := ReadFloat;
        FC := ReadFloat;
        FAl := ReadFloat;
        FBt := ReadFloat;
        FGm := ReadFloat;
    end;
end;

class procedure TSiteList.WriteProperties(const Writer: TWriter;
    const AnObject: TSelfSavedComponent);
begin
    with AnObject as Self, Writer do
    begin
        WriteComponent(FCalcResults);
        WriteComponent(FPlotParams);
        WriteString(FCaption);
        WriteFloat(FA);
        WriteFloat(FB);
        WriteFloat(FC);
        WriteFloat(FAl);
        WriteFloat(FBt);
        WriteFloat(FGm);
    end
end;

class function TWaveVectorList.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := WVLClassInheritID;
    Result.PropVersionNum := WVLCurVerNum;
end;

class procedure TWaveVectorList.ReadProperties(const Reader: TReader;
    const PropHeaderRec: TPropHeaderRec;
    const AnObject: TSelfSavedComponent);
begin

end;

class procedure TWaveVectorList.WriteProperties(const Writer: TWriter;
    const AnObject: TSelfSavedComponent);
begin

end;

class function TRepresentationList.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := RLClassInheritID;
    Result.PropVersionNum := RLCurVerNum;
end;

class procedure TRepresentationList.ReadProperties(const Reader: TReader;
    const PropHeaderRec: TPropHeaderRec;
    const AnObject: TSelfSavedComponent);
begin

end;

class procedure TRepresentationList.WriteProperties(const Writer: TWriter;
    const AnObject: TSelfSavedComponent);
begin

end;

{ TCC }

class function TCC.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := CCAlClassInheritID;
    Result.PropVersionNum := CCAlCurVerNum;
end;

class procedure TCC.ReadProperties(const Reader: TReader;
    const PropHeaderRec: TPropHeaderRec;
    const AnObject: TSelfSavedComponent);
begin

end;

class procedure TCC.WriteProperties(const Writer: TWriter;
    const AnObject: TSelfSavedComponent);
begin

end;

function TNeutronCompList.CreateNewObject: TComponent;
begin
    Result := CreateNewNeutronClass;
end;

function TAtomList.GetInfoCols: LongInt;
begin
    Result := 11;
end;

function TNeutronCompList.GetInfoCols: LongInt;
begin
    Result := 5;
end;

function TSinTCompList.GetInfoCols: LongInt;
begin
    Result := 5;
end;

function TRepresentation.GetInfoRows: LongInt;
begin
    if VectorCount <> 0 then Result := VectorCount
    else Result := 1;
    end;

function TSinTCompList.GetInfoRows: LongInt;
var i: LongInt;
    SC: TSinTClass;
    HC: THKLClass;
begin
    if Count <> 0 then
    begin
        Result := 0;
        for i := 0 to Count - 1 do
        begin
            SC := TSinTClass(Items[i]);
            Result := Result + 1;   if SC.FullView then Result := Result + GetExpandedRowsNum(SC);
        end;
    end else Result := 1;
end;

function TAtomList.ValueToString(const ACol, ARow: Integer): string;
const
    CaptionsArr: array[0..11] of string = ('Num.', 'X', 'Y', 'Z',
    'Mx', 'My', 'Mz', 'Module', 'NSA', 'Element', 'Site', 'Ell. Param.');
var TA: TAtom;
begin
    CheckColIndex(ACol);
    CheckRowIndex(ARow);

    if ARow <= GetFixedRows - 1 then
    begin
        if ARow > 0 then Result := ''
        else Result := CaptionsArr[ACol];
        Exit;
    end;

    if (ACol <= GetFixedCols - 1) and (ARow > GetFixedRows - 1) then
    begin
        if ACol > 0 then Result := ''
        else
        begin
            if Count <> 0 then
            begin
                TA := TAtom(Items[ARow - GetFixedRows]);
                Result := IntToStr(TA.Number);
            end else Result := IntToStr(ARow - (GetFixedRows - 1));
        end;
        Exit;
    end;

    if (ACol > GetFixedCols - 1) and (ARow > GetFixedRows - 1) then
    begin
        if Count <> 0 then
            begin
            TA := TAtom(Items[ARow - GetFixedRows]);
            with TA do case ACol - GetFixedCols of
                0 : Result := FloatToStrF(X, ffFixed, 5, 3);
                1 : Result := FloatToStrF(Y, ffFixed, 5, 3);
                2 : Result := FloatToStrF(Z, ffFixed, 5, 3);
                3 : Result := FloatToStrF(UnitMagnVect[1], ffFixed, 5, 3);
                4 : Result := FloatToStrF(UnitMagnVect[2], ffFixed, 5, 3);
                5 : Result := FloatToStrF(UnitMagnVect[3], ffFixed, 5, 3);
                6 : Result := FloatToStrF(M, ffFixed, 5, 3);
                7 : Result := FloatToStrF(NuclearScatAmpl, ffGeneral, 6, 4);
                8 : Result := Element;
                9 : Result := Position;
               10 : Result := FloatToStrF(EllipseParam, ffGeneral, 6, 4);
            end;
        end else    case ACol - GetFixedCols of
                0, 1, 2, 3, 4, 5, 6 : Result := '0';
                7 : if Assigned(FSite) then
                    Result := FloatToStr(FSite.NuclearScatAmpl) else Result := '';
                8 : if Assigned(FSite) then Result := FSite.Element else Result := '';
                9 : if Assigned(FSite) then Result := FSite.SiteName else Result := '';
               10 : Result := '1';
            end;
    end;
end;

function TNeutronCompList.ValueToString(const ACol, ARow: Integer): string;
const
    CaptionsArr: array[0..5] of string =
        ('Num.', 'Intensity', 'Start Pos.', 'Peak Pos.',
        'Finish Pos.',  'Int. Corr.');
var TN: TNeutronClass;
begin
    CheckColIndex(ACol);
    CheckRowIndex(ARow);

    if ARow <= GetFixedRows - 1 then
    begin
        if ARow > 0 then Result := ''
        else Result := CaptionsArr[ACol];
        Exit;
    end;

    if (ACol <= GetFixedCols - 1) and (ARow > GetFixedRows - 1) then
    begin
        if ACol > 0 then Result := ''
        else Result := IntToStr(ARow - (GetFixedRows - 1));
        Exit;
    end;

    if (ACol > GetFixedCols - 1) and (ARow > GetFixedRows - 1) then
    begin
        if Count <> 0 then
            begin
            TN := TNeutronClass(Items[ARow - GetFixedRows]);
            with TN do case ACol - GetFixedCols of
                0 : Result := FloatToStrF(Intensity, ffGeneral, 8, 4);
                1 : Result := FloatToStrF(StartPos, ffGeneral, 6, 4);
                2 : Result := FloatToStrF(PeakPos, ffGeneral, 6, 4);
                3 : Result := FloatToStrF(FinishPos, ffGeneral, 6, 4);
                4 : Result := FloatToStrF(IntCorrFactor, ffGeneral, 6, 4);
            end;
        end else    case ACol - GetFixedCols of
                0, 1, 2, 3 : Result := '0';
                4 : Result := '1';
            end;
    end;
end;

function TRepresentation.ValueToString(const ACol, ARow: Integer): string;
var TB: TBasisFunctions;
    TempVect: TDoubleVector3;
begin
    CheckColIndex(ACol);
    CheckRowIndex(ARow);

    if ARow <= GetFixedRows - 1 then
    begin
        if ARow > 0 then Result := ''
        else
            if ACol = 0 then Result := 'Num.\B.F.' else
                if ACol >= GetFixedCols then
                    Result := IntToStr(ACol - (GetFixedCols - 1)) else
                        Result := '';
        Exit;
    end;

    if (ACol <= GetFixedCols - 1) and (ARow > GetFixedRows - 1) then
    begin
        if ACol > 0 then Result := ''
        else Result := IntToStr(ARow - (GetFixedRows - 1));
        Exit;
    end;

    if (ACol > GetFixedCols - 1) and (ARow > GetFixedRows - 1) then
    begin
        if Count <> 0 then
        begin
            TB := TBasisFunctions(Items[ACol - GetFixedCols]);
            TempVect := TB[ARow - GetFixedRows];
            Result := '(' + FloatToStrF(TempVect[1], ffGeneral, 6, 4) + ',' +
                            FloatToStrF(TempVect[2], ffGeneral, 6, 4) + ',' +
                            FloatToStrF(TempVect[3], ffGeneral, 6, 4) + ')';
        end else    Result := '(0,0,0)';
    end;
end;

function TAtomList.IsDataValid(
    const ACol, ARow: Integer; const AString: string): Boolean;
begin
    CheckColIndex(ACol);
    CheckRowIndex(ARow);
    if (ACol > GetFixedCols - 1) and (ARow > GetFixedRows - 1) then
    begin
        Result := True;
        case ACol - GetFixedCols of
            0, 1, 2, 3, 4, 5, 6, 7, 10 :
                try StrToFloat(AString) except Result := False; Exit end;
        end;
    end else Result := True;
end;

procedure TAtomList.SetValueByDefault(const ACol, ARow: Integer);
var TA: TAtom;
begin
    if (ACol > GetFixedCols - 1) and (ARow > GetFixedRows - 1) then
        if Count <> 0 then
            begin
            TA := TAtom(Items[ARow - GetFixedRows]);
            with TA do case ACol - GetFixedCols of
                0 : X := 0;
                1 : Y := 0;
                2 : Z := 0;
                3 : Mx := 0;
                4 : My := 0;
                5 : Mz := 0;
                6 : M := 0;
                7 : NuclearScatAmpl := 0;
                8 : Element := '';
                9 : Position := '';
               10 : EllipseParam := 1;
            end;
        end;
end;

procedure TAtomList.StringToValue(const ACol, ARow: Integer;
    const AString: string);
var TA: TAtom;    
begin
    BeforeStringToValue(ACol, ARow, AString);
    if (ACol > GetFixedCols - 1) and (ARow > GetFixedRows - 1) then
    begin
        if IsDataValid(ACol, ARow, AString) then
        begin
            TA := TAtom(Items[ARow - GetFixedRows]);
            with TA do case ACol - GetFixedCols of
                0 : X := StrToFloat(AString);
                1 : Y := StrToFloat(AString);
                2 : Z := StrToFloat(AString);
                3 : Mx := StrToFloat(AString);
                4 : My := StrToFloat(AString);
                5 : Mz := StrToFloat(AString);
                6 : M := StrToFloat(AString);
                7 : NuclearScatAmpl := StrToFloat(AString);
                8 : Element := AString;
                9 : Position := AString;
               10 : EllipseParam := StrToFloat(AString);
            end;
        end else raise EAtomList.Create('Invalid input...');
    end;
end;

function TNeutronCompList.IsDataValid(const ACol, ARow: Integer;
    const AString: string): Boolean;
begin
    CheckColIndex(ACol);
    CheckRowIndex(ARow);
    if (ACol > GetFixedCols - 1) and (ARow > GetFixedRows - 1) then
    begin
        Result := True;
        try StrToFloat(AString) except Result := False; Exit end;
        end else Result := True;
end;

procedure TNeutronCompList.SetValueByDefault(const ACol, ARow: Integer);
var TN: TNeutronClass;
begin
    if (ACol > GetFixedCols - 1) and (ARow > GetFixedRows - 1) then
        if Count <> 0 then
            begin
            TN := TNeutronClass(Items[ARow - GetFixedRows]);
            with TN do case ACol - GetFixedCols of
                0 : Intensity := 0;
                1 : StartPos := 0;
                2 : PeakPos := 0;
                3 : FinishPos := 0;
                4 : IntCorrFactor := 1;
            end;
        end;
end;

procedure TNeutronCompList.StringToValue(const ACol, ARow: Integer;
    const AString: string);
var TN: TNeutronClass;
begin
    BeforeStringToValue(ACol, ARow, AString);
    if (ACol > GetFixedCols - 1) and (ARow > GetFixedRows - 1) then
    begin
        if IsDataValid(ACol, ARow, AString) then
        begin
            TN := TNeutronClass(Items[ARow - GetFixedRows]);
            with TN do case ACol - GetFixedCols of
                0 : Intensity := StrToFloat(AString);
                1 : StartPos := StrToFloat(AString);
                2 : PeakPos := StrToFloat(AString);
                3 : FinishPos := StrToFloat(AString);
                4 : IntCorrFactor := StrToFloat(AString);
            end;
        end else raise ENeutronCompList.Create('Invalid input...');
    end;
end;

function TRepresentation.IsDataValid(const ACol, ARow: Integer;
    const AString: string): Boolean;
begin
    CheckColIndex(ACol);
    CheckRowIndex(ARow);
    if (ACol > GetFixedCols - 1) and (ARow > GetFixedRows - 1) then
    begin
        Result := True;
        try StrToVector3(AString) except Result := False end;
    end else Result := True;
end;

procedure TRepresentation.SetValueByDefault(const ACol, ARow: Integer);
var TB: TBasisFunctions;
    TempVect: TDoubleVector3;
begin
    if (ACol > GetFixedCols - 1) and (ARow > GetFixedRows - 1) then
        if Count <> 0 then
            begin
            TB := TBasisFunctions(Items[ACol - GetFixedCols]);
            TempVect[1] := 0; TempVect[2] := 0; TempVect[3] := 0;
            TB[ARow - GetFixedRows] := TempVect;
        end;
end;

procedure TRepresentation.StringToValue(const ACol, ARow: Integer;
    const AString: string);
var TB: TBasisFunctions;
    TempVect: TDoubleVector3;
begin
    BeforeStringToValue(ACol, ARow, AString);
    if (ACol > GetFixedCols - 1) and (ARow > GetFixedRows - 1) then
    begin
        if IsDataValid(ACol, ARow, AString) then
        begin
            TB := TBasisFunctions(Items[ACol - GetFixedCols]);
            TempVect :=  StrToVector3(AString);
            TB[ARow - GetFixedRows] := TempVect;
        end else raise ERepresentation.Create('Invalid input...');
    end;
end;

function TSinTCompList.Add(Item: TComponent): Integer;
var i: LongInt;
begin
    Result := inherited Add(Item);
    if AreRowHeightsReady then
    begin
        if not IsDataSourceEmpty then AddRowHeightItem;
        if TSinTClass(Item).FullView then
            for i := 1 to GetExpandedRowsNum(TSinTClass(Item)) do
                AddRowHeightItem;
    end;
end;

procedure TSinTCompList.Delete(Index: Integer);
var i: LongInt;
begin
    if AreRowHeightsReady and (not Destroying) then
    begin
        DeleteRowHeightItem(Index);
        if TSinTClass(Items[Index]).FullView then
            for i := 1 to GetExpandedRowsNum(TSinTClass(Items[Index])) do
                DeleteRowHeightItem(Index);
    end;
    inherited;
end;

procedure TSinTCompList.Insert(Index: Integer; Item: TComponent);
var i: LongInt;
begin
    inherited;
    if AreRowHeightsReady then
    begin
        InsertRowHeightItem(Index);
        if TSinTClass(Item).FullView then
            for i := 1 to GetExpandedRowsNum(TSinTClass(Item)) do
                InsertRowHeightItem(Index);
    end;
end;

procedure TSinTCompList.CollapseItem(const Index: Integer);
var TS: TSinTClass;
    i: LongInt;
begin
    TS := TSinTClass(Items[Index]);
    if TS.FullView then
    begin
        TS.FullView := False;
        for i := 1 to GetExpandedRowsNum(TS) do
            DeleteRowHeightItem(Index + 1);
    end;
end;

procedure TSinTCompList.ExpandItem(const Index: Integer);
var TS: TSinTClass;
    i: LongInt;
begin
    TS := TSinTClass(Items[Index]);
    if not TS.FullView then
    begin
        TS.FullView := True;
        if IndexOf(TS) <> Count - 1 then
            for i := 1 to GetExpandedRowsNum(TS) do
                InsertRowHeightItem(Index + 1)
        else
            for i := 1 to GetExpandedRowsNum(TS) do
                AddRowHeightItem;
    end;
end;

function TSinTCompList.GetExpandedRowsNum(const Item: TSinTClass): LongInt;
begin
    Result := Item.HKLList.Count + 1;
end;

procedure TSinTCompList.ToggleExpanded(const Index: Integer);
var TS: TSinTClass;
begin
    CheckItemIndex(Index);
    TS := TSinTClass(Items[Index]);
    if not TS.FullView then ExpandItem(Index)
    else CollapseItem(Index);
end;

procedure TSinTCompList.CheckItemIndex(const Index: LongInt);
begin
    if (Index < 0) or (Index > Count - 1) then
        raise ESinTCompList.Create('Invalid item index (' +
            IntToStr(Index) + ')...');
end;

function TAtomList.GetCellEnabledCharSet(const ACol,
    ARow: Integer): TCharSet;
begin
    case ACol - GetFixedCols of
        0, 1, 2, 3, 4, 5, 6, 7, 10 : Result := REAL_SET;
        8, 9 : Result := CHAR_SET;
    end;
end;

function TNeutronCompList.GetCellEnabledCharSet(const ACol,
    ARow: Integer): TCharSet;
begin
    Result := POS_REAL_SET;
end;

function TRepresentation.GetCellEnabledCharSet(const ACol,
    ARow: Integer): TCharSet;
begin
    Result := ['0'..'9','.','(',')',',','-'];
end;

procedure TSite.SetElement(const Element: string);
var i: LongInt;
    TA: TAtom;
begin
    FElement := Element;

    if not (csLoading in ComponentState) then with AtomList do
        for i := 0 to Count - 1 do
        begin
            TA := TAtom(Items[i]);
            TA.Element := Element;
        end;
end;

procedure TSite.SetNuclearScatAmpl(const NuclearScatAmpl: Double);
var i: LongInt;
    TA: TAtom;
begin
    FNuclearScatAmpl := NuclearScatAmpl;

    if not (csLoading in ComponentState) then with AtomList do
        for i := 0 to Count - 1 do
        begin
            TA := TAtom(Items[i]);
            TA.NuclearScatAmpl := NuclearScatAmpl;
        end;
end;

procedure TRepresentation.SetVectorCount(const AVectorCount: Integer);
var i: LongInt;
    BF: TBasisFunctions;
begin
    FVectorCount := AVectorCount;
    for i := 0 to Count - 1 do
    begin
        BF := TBasisFunctions(Items[i]);
        BF.VectorsCount := AVectorCount;
    end;
end;

procedure TRepresentation.CopyParameters(const Dest: TObject);
begin
    TRepresentation(Dest).VectorCount := VectorCount;
        inherited;  end;

procedure TSite.SetBFVectorCount(const AVectorCount: Integer);
var i: LongInt;
    TS: TWaveVector;
begin
    with WaveVectorList do
        for i := 0 to Count - 1 do
        begin
            TS := TWaveVector(Items[i]);
            TS.SetBFVectorCount(AVectorCount);
        end;
end;

procedure TWaveVector.SetBFVectorCount(const AVectorCount: Integer);
var i: LongInt;
    TR: TRepresentation;
begin
    with Representations do
        for i := 0 to Count - 1 do
        begin
            TR := TRepresentation(Items[i]);
            TR.VectorCount := AVectorCount;
        end;
end;

procedure TRepresentation._AddNewVector;
var i: LongInt;
    BF: TBasisFunctions;
begin
    for i := 0 to Count - 1 do
    begin
        BF := TBasisFunctions(Items[i]);
        BF.AddNewVector;
    end;
end;

procedure TRepresentation._DeleteVector(const Index: Integer);
var i: LongInt;
    BF: TBasisFunctions;
begin
    for i := 0 to Count - 1 do
    begin
        BF := TBasisFunctions(Items[i]);
        BF.DeleteVector(Index);
    end;
end;

procedure TRepresentation._InsertNewVector(const Index: Integer);
var i: LongInt;
    BF: TBasisFunctions;
begin
    for i := 0 to Count - 1 do
    begin
        BF := TBasisFunctions(Items[i]);
        BF.InsertNewVector(Index);
    end;
end;

procedure TRepresentationList.CopyParameters(const Dest: TObject);
begin
    TRepresentationList(Dest).LinkItemDisabled := True;
    inherited;  TRepresentationList(Dest).LinkItemDisabled := False;
end;

procedure TAtomList.EnumerateAtoms;
var i: LongInt;
    TA: TAtom;
begin
    for i := 0 to Count - 1 do
    begin
        TA := TAtom(Items[i]);
        TA.Number := i + 1;
    end;
end;

procedure TWaveVector.CreateParameters;
var i: LongInt;
    TR: TRepresentation;
begin
    with Representations do
        for i := 0 to Count - 1 do
        begin
            TR := TRepresentation(Items[i]);
            TR.RandMixFactors;
        end;

    inherited;
end;

{ TNeutronClassMono }

procedure TNeutronClassMono.CopyParameters(const Dest: TObject);
begin
    inherited;
    TNeutronClassMono(Dest).Intensity := Intensity;
    TNeutronClassMono(Dest).H := H;
    TNeutronClassMono(Dest).K := K;
    TNeutronClassMono(Dest).L := L;
    TNeutronClassMono(Dest).IntCorrFactor := IntCorrFactor;
end;

constructor TNeutronClassMono.Create(AOwner: TComponent);
begin
    inherited;
    IntCorrFactor := 1;
end;

class function TNeutronClassMono.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := NCMClassInheritID;
    Result.PropVersionNum := NCMCurVerNum;
end;

procedure TNeutronClassMono.IsReady;
begin

end;

function TNeutronClassMono.MyNameIs: string;
begin
    Result := 'the peak';
end;

class procedure TNeutronClassMono.ReadProperties(const Reader: TReader;
    const PropHeaderRec: TPropHeaderRec;
    const AnObject: TSelfSavedComponent);
begin
    with AnObject as Self, Reader do
    begin
        Intensity := ReadFloat;
        H := ReadFloat;
        K := ReadFloat;
        L := ReadFloat;
        IntCorrFactor := ReadFloat;
    end;
end;

class procedure TNeutronClassMono.WriteProperties(const Writer: TWriter;
    const AnObject: TSelfSavedComponent);
begin
    with AnObject as Self, Writer do
    begin
        WriteFloat(Intensity);
        WriteFloat(H);
        WriteFloat(K);
        WriteFloat(L);
        WriteFloat(IntCorrFactor);
    end;
end;

{ TNCM }

class function TNCM.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := NCMAlClassInheritID;
    Result.PropVersionNum := NCMAlCurVerNum;
end;

class procedure TNCM.ReadProperties(const Reader: TReader;
    const PropHeaderRec: TPropHeaderRec;
    const AnObject: TSelfSavedComponent);
begin

end;

class procedure TNCM.WriteProperties(const Writer: TWriter;
    const AnObject: TSelfSavedComponent);
begin

end;

{ TNeutronCompListMono }

procedure TNeutronCompListMono.CreateHKLList(
    const PatternAuxiliary: TPatternAuxiliary;
    const ExtHKLList: THKLList;
    const AMinSinTL, AMaxSinTL: Double);
var i: LongInt;
    NC: TNeutronClassMono;
    TempSinTL: Double;
    Vector: TDoubleVector3;
    ReallyNew: Boolean;
begin
    with PatternAuxiliary do
        for i := 0 to Count - 1 do
        begin
            NC := TNeutronClassMono(Items[i]);
            TempSinTL := GetSinTL(NC.H, NC.K, NC.L);

            if (TempSinTL >= AMinSinTL) and (TempSinTL <= AMaxSinTL) then
            begin
                Vector[1] := NC.H; Vector[2] := NC.K; Vector[3] := NC.L;
                ExtHKLList.CreateNewHKL(PatternAuxiliary, Vector, ReallyNew);
            end;
        end;
end;

function TNeutronCompListMono.CreateNewObject: TComponent;
begin
    Result := CreateNewNeutronClassMono;
end;

function TNeutronCompListMono.GetCellEnabledCharSet(const ACol,
    ARow: Integer): TCharSet;
begin
    case ACol of
        1,5 : Result := POS_REAL_SET;
        2,3,4 : Result := REAL_SET;
        else Result := REAL_SET;
    end;
end;

function TNeutronCompListMono.GetInfoCols: LongInt;
begin
    Result := 5;
end;

function TNeutronCompListMono.GetMaxNeutronSinTL(
    const PatternAuxiliary: TPatternAuxiliary): Double;
var i: LongInt;
    NC: TNeutronClassMono;
    TempSinTL: Double;
begin
    Result := 0;
    for i := 0 to Count - 1 do
    begin
        NC := TNeutronClassMono(Items[i]);
        TempSinTL := PatternAuxiliary.GetSinTL(NC.H, NC.K, NC.L);
        if TempSinTL > Result then Result := TempSinTL;
    end;
end;

class function TNeutronCompListMono.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := NCLMClassInheritID;
    Result.PropVersionNum := NCLMCurVerNum;
end;

function TNeutronCompListMono.GetRowContents(Grid: TStringGrid;
    RowNum: Integer): Boolean;
var NC: TNeutronClassMono;
    St: string;
begin
    with Grid do
        if (RowNum - FixedRows >= 0) and (RowNum - FixedRows < Count) then
        begin
            NC := TNeutronClassMono(Items[RowNum - FixedRows]);
            with NC do
            begin
                St := Cells[FixedCols, RowNum];
                    Intensity := StrToFloatDef(St, 0);

                St := Cells[FixedCols + 1, RowNum];
                    H := StrToFloatDef(St, 0);

                St := Cells[FixedCols + 2, RowNum];
                    K := StrToFloatDef(St, 0);

                St := Cells[FixedCols + 3, RowNum];
                    L := StrToFloatDef(St, 0);

                St := Cells[FixedCols + 4, RowNum];
                    IntCorrFactor := StrToFloatDef(St, 0);
            end;
        end;
    Result := True;
end;

function TNeutronCompListMono.IsDataValid(const ACol, ARow: Integer;
    const AString: string): Boolean;
begin
    CheckColIndex(ACol);
    CheckRowIndex(ARow);
    if (ACol > GetFixedCols - 1) and (ARow > GetFixedRows - 1) then
    begin
        Result := True;
        try StrToFloat(AString) except Result := False; Exit end;
        end else Result := True;
end;

class procedure TNeutronCompListMono.ReadProperties(const Reader: TReader;
    const PropHeaderRec: TPropHeaderRec;
    const AnObject: TSelfSavedComponent);
begin

end;

procedure TNeutronCompListMono.SetCaption(Grid: TStringGrid);
const CaptionsArr: array[0..5] of string =
    ('Num.', 'Intensity', 'H', 'K', 'L',  'Int. Corr.');
var i: LongInt;
begin
    with Grid do
        if FixedRows <> 0 then
            for i := 0 to 5 do Cells[i, 0] := CaptionsArr[i];
end;

procedure TNeutronCompListMono.SetColOptions(Grid: TStringGrid);
begin
    if Grid is TNumericGrid then
        with TNumericGrid(Grid) do
        begin
            ColOptions[1] := coReal; ColOptions[2] := coReal;
            ColOptions[3] := coReal; ColOptions[4] := coReal;
        end;
end;

procedure TNeutronCompListMono.SetRowContents(Grid: TStringGrid;
    RowNum: Integer);
var NC: TNeutronClassMono;
    i: LongInt;
begin
    with Grid do
        if (RowNum - FixedRows >= 0) and (RowNum - FixedRows < Count) then
        begin
            NC := TNeutronClassMono(Items[RowNum - FixedRows]);
            with NC do
            begin
                Cells[0, RowNum] := IntToStr(RowNum);
                Cells[FixedCols, RowNum] :=
                    FloatToStrF(Intensity, ffGeneral, 8, 4);
                Cells[FixedCols + 1, RowNum] :=
                    FloatToStrF(H, ffGeneral, 6, 4);
                Cells[FixedCols + 2, RowNum] :=
                    FloatToStrF(K, ffGeneral, 6, 4);
                Cells[FixedCols + 3, RowNum] :=
                    FloatToStrF(L, ffGeneral, 6, 4);
                Cells[FixedCols + 4, RowNum] :=
                    FloatToStrF(IntCorrFactor, ffGeneral, 6, 4);
            end;
        end
        else
        begin
            Cells[0, RowNum] := IntToStr(RowNum);
            for i := FixedCols to ColCount - 1 do Cells[i, RowNum] := '';
        end;
end;

procedure TNeutronCompListMono.SetValueByDefault(const ACol,
    ARow: Integer);
var TN: TNeutronClassMono;
begin
    if (ACol > GetFixedCols - 1) and (ARow > GetFixedRows - 1) then
        if Count <> 0 then
            begin
            TN := TNeutronClassMono(Items[ARow - GetFixedRows]);
            with TN do case ACol - GetFixedCols of
                0 : Intensity := 0;
                1 : H := 0;
                2 : K := 0;
                3 : L := 0;
                4 : IntCorrFactor := 1;
            end;
        end;
end;

procedure TNeutronCompListMono.StringToValue(const ACol, ARow: Integer;
    const AString: string);
var TN: TNeutronClassMono;
begin
    BeforeStringToValue(ACol, ARow, AString);
    if (ACol > GetFixedCols - 1) and (ARow > GetFixedRows - 1) then
    begin
        if IsDataValid(ACol, ARow, AString) then
        begin
            TN := TNeutronClassMono(Items[ARow - GetFixedRows]);
            with TN do case ACol - GetFixedCols of
                0 : Intensity := StrToFloat(AString);
                1 : H := StrToFloat(AString);
                2 : K := StrToFloat(AString);
                3 : L := StrToFloat(AString);
                4 : IntCorrFactor := StrToFloat(AString);
            end;
        end else raise ENeutronCompListMono.Create('Invalid input...');
    end;
end;

function TNeutronCompListMono.ValueToString(const ACol,
    ARow: Integer): string;
const
    CaptionsArr: array[0..5] of string =
        ('Num.', 'Intensity', 'H', 'K', 'L',  'Int. Corr.');
var TN: TNeutronClassMono;
begin
    CheckColIndex(ACol);
    CheckRowIndex(ARow);

    if ARow <= GetFixedRows - 1 then
    begin
        if ARow > 0 then Result := ''
        else Result := CaptionsArr[ACol];
        Exit;
    end;

    if (ACol <= GetFixedCols - 1) and (ARow > GetFixedRows - 1) then
    begin
        if ACol > 0 then Result := ''
        else Result := IntToStr(ARow - (GetFixedRows - 1));
        Exit;
    end;

    if (ACol > GetFixedCols - 1) and (ARow > GetFixedRows - 1) then
    begin
        if Count <> 0 then
        begin
            TN := TNeutronClassMono(Items[ARow - GetFixedRows]);
            with TN do case ACol - GetFixedCols of
                0 : Result := FloatToStrF(Intensity, ffGeneral, 8, 4);
                1 : Result := FloatToStrF(H, ffGeneral, 6, 4);
                2 : Result := FloatToStrF(K, ffGeneral, 6, 4);
                3 : Result := FloatToStrF(L, ffGeneral, 6, 4);
                4 : Result := FloatToStrF(IntCorrFactor, ffGeneral, 6, 4);
            end;
        end else
            case ACol - GetFixedCols of
                0, 1, 2, 3 : Result := '0';
                4 : Result := '1';
            end;
    end;
end;

class procedure TNeutronCompListMono.WriteProperties(const Writer: TWriter;
    const AnObject: TSelfSavedComponent);
begin

end;

{ TNCLM }

class function TNCLM.GetPropHeaderRec: TPropHeaderRec;
begin

end;

class procedure TNCLM.ReadProperties(const Reader: TReader;
    const PropHeaderRec: TPropHeaderRec;
    const AnObject: TSelfSavedComponent);
begin

end;

class procedure TNCLM.WriteProperties(const Writer: TWriter;
    const AnObject: TSelfSavedComponent);
begin

end;

{ TPatternAuxiliary }

function TPatternAuxiliary.GetScatVectModule(
    const H, K, L : Double  ): Double;
var B1, B2, B3, ScatVect: TDoubleVector3;
begin
    CheckPrepared;
    GetMutualVectors(A, B, C, Alpha, Beta, Gamma, B1, B2, B3);

    B1 := MulVectByValue(B1, 2 * pi * H);
    B2 := MulVectByValue(B2, 2 * pi * K);
    B3 := MulVectByValue(B3, 2 * pi * L);
    ScatVect[1] := B1[1] + B2[1] + B3[1];
    ScatVect[2] := B1[2] + B2[2] + B3[2];
    ScatVect[3] := B1[3] + B2[3] + B3[3];

    Result := GetVectModuleA(ScatVect, A, B, C, Alpha, Beta, Gamma);
end;

procedure TPatternAuxiliary.GetUnitScatVect(
    const H, K, L: Double;
    var UnitVect: TDoubleVector3);
var B1, B2, B3, ScatVect: TDoubleVector3;
begin
    CheckPrepared;
    GetMutualVectors(A, B, C, Alpha, Beta, Gamma, B1, B2, B3);

    B1 := MulVectByValue(B1, 2 * pi * H);
    B2 := MulVectByValue(B2, 2 * pi * K);
    B3 := MulVectByValue(B3, 2 * pi * L);
    ScatVect[1] := B1[1] + B2[1] + B3[1];
    ScatVect[2] := B1[2] + B2[2] + B3[2];
    ScatVect[3] := B1[3] + B2[3] + B3[3];

    GetUnitVectA(ScatVect, A, B, C, Alpha, Beta, Gamma, UnitVect);
end;

function TPatternAuxiliary.GetSinTL(
    const H, K, L: Double
    ): Double;
var G: Double;
begin
    CheckPrepared;
    G := Sqr(A) * Sqr(B) * Sqr(L) * Sqr(SinArr[3])
        + Sqr(A) * Sqr(C) * Sqr(K) * Sqr(SinArr[2])
        + Sqr(B) * Sqr(C) * Sqr(H) * Sqr(SinArr[1])
        + 2 * (A * B * Sqr(C) * H * K * P[3]
        + A * Sqr(B) * C * H * L * P[2]
        + Sqr(A) * B * C * K * L * P[1]);

    Result := (1 / (2 * A * B * C)) * Sqrt(G / D);
end;

procedure TPatternAuxiliary.PrepareToCalc;
begin
    CosArr[1] := Cos(Alpha); CosArr[2] := Cos(Beta); CosArr[3] := Cos(Gamma);
    SinArr[1] := Sin(Alpha); SinArr[2] := Sin(Beta); SinArr[3] := Sin(Gamma);
    P[1] := Cos(Beta) * Cos(Gamma) - Cos(Alpha);
    P[2] := Cos(Alpha) * Cos(Gamma) - Cos(Beta);
    P[3] := Cos(Alpha) * Cos(Beta) - Cos(Gamma);
    D := 1 - Sqr(Cos(Alpha)) - Sqr(Cos(Beta)) - Sqr(Cos(Gamma))
        + 2 * Cos(Alpha) * Cos(Beta) * Cos(Gamma);
    Prepared := True;
end;

procedure TPatternAuxiliary.SetA(const AA: Double);
begin
    FA := AA;
    Prepared := False;
end;

procedure TPatternAuxiliary.SetAlpha(const AAlpha: Double);
begin
    FAlpha := AAlpha;
    Prepared := False;
end;

procedure TPatternAuxiliary.SetB(const AB: Double);
begin
    FB := AB;
    Prepared := False;
end;

procedure TPatternAuxiliary.SetBeta(const ABeta: Double);
begin
    FBeta := ABeta;
    Prepared := False;
end;

procedure TPatternAuxiliary.SetC(const AC: Double);
begin
    FC := AC;
    Prepared := False;
end;

procedure TPatternAuxiliary.SetGamma(const AGamma: Double);
begin
    FGamma := AGamma;
    Prepared := False;
end;

procedure TPatternAuxiliary.SetLambda(const ALambda: Double);
begin
    FLambda := ALambda;
    Prepared := False;
end;

procedure TPatternAuxiliary.CheckPrepared;
begin
    if not Prepared then
        raise EPatternAuxiliary.Create('Data must be prepared...');
end;

initialization
    RegisterClass(TAtom);
    RegisterClass(TAtomList);
    RegisterClass(TNeutronClass);
    RegisterClass(TNeutronClassMono);
    RegisterClass(TGeneralClass);
    RegisterClass(TSinTClass);
    RegisterClass(THKLClass);
    RegisterClass(TNeutronCompList);
    RegisterClass(TNeutronCompListMono);
    RegisterClass(TCommentsClass);
    RegisterClass(TSinTCompList);
    RegisterClass(TSite);
    RegisterClass(TSiteList);
    RegisterClass(TWaveVector);
    RegisterClass(TWaveVectorList);
    RegisterClass(TRepresentation);
    RegisterClass(TRepresentationList);
    RegisterClass(TBasisFunctions);
    RegisterClass(TGeneratorClass);
    RegisterClass(TStructure);
    RegisterClass(TPossibleModel);
    RegisterClass(TSelfCopiedComponent);
    RegisterClass(TCalcResults);
    RegisterClass(TPatternParametersContainer);

    RegisterClass(TA);
    RegisterClass(TAL);
    RegisterClass(TBF);
    RegisterClass(TCC);
    RegisterClass(TGC);
    RegisterClass(THKLC);
    RegisterClass(TNC);
    RegisterClass(TNCM);
    RegisterClass(TNCL);
    RegisterClass(TNCLM);
    RegisterClass(TR);
    RegisterClass(TRL);
    RegisterClass(TSTC);
    RegisterClass(TSTCL);
    RegisterClass(TS);
    RegisterClass(TSL);
    RegisterClass(TCR);
    RegisterClass(TWV);
    RegisterClass(TWVL);
    RegisterClass(TPPC);
    DecimalSeparator := '.';
    HKLSetSort := HKLSetSortFunc;
end.

