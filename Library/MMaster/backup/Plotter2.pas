{------------------------------------------------------------------------------
    This file is part of the MotifMASTER project. This software is
    distributed under GPL (see gpl.txt for details).

    This software is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Copyright (C) 1999-2007 D.Morozov (dvmorozov@mail.ru)
------------------------------------------------------------------------------}

unit Plotter2;

interface

uses
    ExtCtrls,
    {$IFNDEF Lazarus}
    WinTypes,
    {$ENDIF}
    Graphics, Math3d, Classes, SysUtils,
    DataClasses, ComponentList, SimpMath, SelfCopied, Tools,
    SelfSaved, ClassInheritIDs;

const
    PM_ARROWS = 5;
    PM_MODULES = 6;
    PM_SERIAL_NUMBER = 7;
    PM_ELEMENT = 8;

type
    TSitePlotParams = class(TSelfCopiedComponent)
    protected
        FMomentsPlotMode: LongInt;  FColor: TColor;
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

    public
        constructor Create(AOwner: TComponent); override;
        procedure CopyParameters(const Dest: TObject); override;

        procedure SetSite(const ASite: TSite);
        function GetSite: TSite;

        property MomentsPlotMode: LongInt
                read FMomentsPlotMode           write FMomentsPlotMode;
        property Color: TColor
                read FColor                     write FColor;
    end;

    TSPP = class(TSitePlotParams)
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

    TSiteListPlotParams = class(TSelfCopiedComponent)
    protected
        FSiteList: TSiteList;

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
        procedure CopyParameters(const Dest: TObject); override;

        procedure SetSiteList(const ASiteList: TSiteList);
        function GetSiteList: TSiteList;
    end;

    TSLPP = class(TSiteListPlotParams)
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

    EPlotter3D = class(Exception);
    TPlotter3D = class
    protected
        FSiteList: TSiteList;   FCommonAtomList: TComponentList;
            function GetMySiteList: TSiteList;
        procedure AddAtomsToCommonList;

    public
        constructor Create; virtual;
        destructor Destroy; override;
        procedure SetSiteList(const ASiteList: TSiteList);
        function GetSiteList: TSiteList;
        procedure Plot; virtual; abstract;
    end;

    EBufferedPlotter3D = class(Exception);
    EIntBufPlotter3D = class(Exception);    
    EExtBufPlotter3D = class(Exception);

    TBufferedPlotter3D = class(TPlotter3D)
    protected
        FOutCanvas: TCanvas;

        procedure ActualPlot;
        function GetMyOutCanvas: TCanvas;
        function GetMyBufCanvas: TCanvas; virtual; abstract;
        function GetMyHeight: LongInt; virtual; abstract;
        function GetMyWidth: LongInt; virtual; abstract;

    public
        procedure SetOutCanvas(const AOutCanvas: TCanvas);
        procedure Plot; override;
    end;

    TIntBufPlotter3D = class(TBufferedPlotter3D)
    protected
        FBufBitMap: TBitMap;

        function GetMyBufCanvas: TCanvas; override;
        function GetMyHeight: LongInt; override;
        function GetMyWidth: LongInt; override;

    public
        constructor Create; virtual;
        destructor Destroy; override;

        procedure SetHeight(const AHeight: LongInt);
        procedure SetWidth(const AWidth: LongInt);
    end;

    TExtBufPlotter3D = class(TBufferedPlotter3D)
    protected
        FBufCanvas: TCanvas;
        FHeight, FWidth: LongInt;

        function GetMyBufCanvas: TCanvas; override;
        function GetMyHeight: LongInt; override;
        function GetMyWidth: LongInt; override;

    public
        procedure SetBufCanvas(const ABufCanvas: TCanvas);
        procedure SetHeight(const AHeight: LongInt);
        procedure SetWidth(const AWidth: LongInt);
    end;

    TAdvancedPlotter3D = class(TPlotter3D)
    public
        procedure Plot; override;
    end;

const
    CL_EXP_INTENSITY: TColor = clRed;
    CL_NUCLEAR_INTENSITY: TColor = clBlue;
    CL_INTENSITY: TColor = clBlack;

    ARROW_SCALE: Double = 0.3;
    function ViewStructListSortFunc(Item1, Item2: Pointer): Integer;

const
    ViewStructListSort: TListSortCompare = ViewStructListSortFunc;
    implementation

const
    PosColNum = 4;
    PositionColors: array[0..PosColNum] of TColor =
        (clRed, clBlue, clGreen, clBlack, clMaroon);

procedure TBufferedPlotter3D.Plot;
var Coord, Coord2, Coord3: T3Vector;
    x, y, x3, y3: Integer;
    DeltaX, DeltaY: Double;
    R: TRect;
    MaxValue: Double;
    MinSize: Integer;
    MaxModule: Double;
    Arrow: array[1..2] of T3Vector;
    SR: TAtom;
    i, j: LongInt;
    PR: TPositionRec;
    TempColor, TempColor2, PaintColor: TColor;

const TINY = 1e-6;

    procedure PlotArrow(St: String);
        var DrawPoints: array[1..5] of TPoint;
            l: Double;
        a: Double;
        Alpha: Double;
        SavedWidth: LongInt;
    begin
        with GetMyBufCanvas do
        begin
            SavedWidth := Pen.Width;
            Pen.Width := 2;
            StandardVectTrans(Arrow[1]);
            x := GetMyWidth div 2 + Round((Arrow[1, 1]) * MinSize);
            y := GetMyHeight div 2 - Round((Arrow[1, 2]) * MinSize);
            DrawPoints[1].x := x;
            DrawPoints[1].y := y;
            StandardVectTrans(Arrow[2]);
            x := GetMyWidth div 2 + Round((Arrow[2, 1]) * MinSize);
            y := GetMyHeight div 2 - Round((Arrow[2, 2]) * MinSize);
            DrawPoints[2].x := x;   DrawPoints[2].y := y;
            DrawPoints[4].x := x;
            DrawPoints[4].y := y;
            DeltaX := (DrawPoints[2].x - DrawPoints[1].x);
            DeltaY := (DrawPoints[2].y - DrawPoints[1].y);
            l := 0.25 * Sqrt(Sqr(DeltaX) + Sqr(DeltaY));
            if l > 10 then l := 10;
            a := 0.5 * l;
            if DeltaX <> 0 then Alpha := ArcTan(DeltaY / DeltaX)
            else Alpha := pi / 2;
            if Sqrt(Sqr(DeltaX) + Sqr(DeltaY)) <> 0 then begin
                x := DrawPoints[2].x - Round(l * DeltaX / Sqrt(Sqr(DeltaX) + Sqr(DeltaY)));
                y := DrawPoints[2].y - Round(l * DeltaY / Sqrt(Sqr(DeltaX) + Sqr(DeltaY)));
                DrawPoints[3].x := x + Round(a * Cos(pi / 2 + Alpha));
                DrawPoints[3].y := y + Round(a * Sin(pi / 2 + Alpha));
                DrawPoints[5].x := x - Round(a * Cos(pi / 2 + Alpha));
                DrawPoints[5].y := y - Round(a * Sin(pi / 2 + Alpha));
            end else begin
                x := DrawPoints[2].x;
                y := DrawPoints[2].y;
                DrawPoints[3].x := x;
                DrawPoints[3].y := y;
                DrawPoints[5].x := x;
                DrawPoints[5].y := y;
            end;

            x := DrawPoints[2].x - (DrawPoints[2].x - DrawPoints[1].x) div 2;
            y := DrawPoints[2].y - (DrawPoints[2].y - DrawPoints[1].y) div 2;
            PolyLine(DrawPoints);

            Pen.Width := SavedWidth;

            TextOut(DrawPoints[2].x + 3, DrawPoints[2].y - 3, St);
        end;
    end;

    procedure PlotLine;
        var DrawPoints: array[1..2] of TPoint;
    begin
        with GetMyBufCanvas do
        begin
            StandardVectTrans(Arrow[1]);
            x := GetMyWidth div 2 + Round((Arrow[1, 1]) * MinSize);
            y := GetMyHeight div 2 - Round((Arrow[1, 2]) * MinSize);
            DrawPoints[1].x := x;
            DrawPoints[1].y := y;
            StandardVectTrans(Arrow[2]);
            x := GetMyWidth div 2 + Round((Arrow[2, 1]) * MinSize);
            y := GetMyHeight div 2 - Round((Arrow[2, 2]) * MinSize);
            DrawPoints[2].x := x;   DrawPoints[2].y := y;
            MoveTo(DrawPoints[1].x, DrawPoints[1].y);
            LineTo(DrawPoints[2].x, DrawPoints[2].y);
        end;
    end;

    procedure PlotAxes;
        var SavedWidth, j: Integer;
        St: string;
    begin
        with GetMyBufCanvas, GetMySiteList do
        begin
            SavedWidth := Pen.Width;
            Pen.Width := 2;
                Coord[1] := -1 / 2 * (A / MaxValue);
            Coord[2] := -1 / 2 * (B / MaxValue);
            Coord[3] := -1 / 2 * (C / MaxValue);

            Coord2[1] :=  1 / 2 * (A / MaxValue);
            Coord2[2] := -1 / 2 * (B / MaxValue);
            Coord2[3] := -1 / 2 * (C / MaxValue);   for j := 1 to 3 do Arrow[1, j] := Coord[j];
            for j := 1 to 3 do Arrow[2, j] := Coord2[j];
            PlotArrow('(1, 0, 0)');
            Coord2[1] := -1 / 2 * (A / MaxValue);
            Coord2[2] :=  1 / 2 * (B / MaxValue);
            Coord2[3] := -1 / 2 * (C / MaxValue);   for j := 1 to 3 do Arrow[1, j] := Coord[j];
            for j := 1 to 3 do Arrow[2, j] := Coord2[j];
            PlotArrow('(0, 1, 0)');
            Coord2[1] := -1 / 2 * (A / MaxValue);
            Coord2[2] := -1 / 2 * (B / MaxValue);
            Coord2[3] :=  1 / 2 * (C / MaxValue);   for j := 1 to 3 do Arrow[1, j] := Coord[j];
            for j := 1 to 3 do Arrow[2, j] := Coord2[j];
            PlotArrow('(0, 0, 1)');

            Pen.Width := 1;
            Pen.Style := psDot;

            Coord[1] :=  1 / 2 * (A / MaxValue);    Coord[2] :=  1 / 2 * (B / MaxValue);
            Coord[3] := -1 / 2 * (C / MaxValue);
            Coord2[1] :=  1 / 2 * (A / MaxValue);   Coord2[2] := -1 / 2 * (B / MaxValue);
            Coord2[3] := -1 / 2 * (C / MaxValue);   for j := 1 to 3 do Arrow[1, j] := Coord[j];
            for j := 1 to 3 do Arrow[2, j] := Coord2[j];
            PlotLine;
            Coord2[1] := -1 / 2 * (A / MaxValue);   Coord2[2] :=  1 / 2 * (B / MaxValue);
            Coord2[3] := -1 / 2 * (C / MaxValue);   for j := 1 to 3 do Arrow[1, j] := Coord[j];
            for j := 1 to 3 do Arrow[2, j] := Coord2[j];
            PlotLine;
            Coord2[1] :=  1 / 2 * (A / MaxValue);   Coord2[2] :=  1 / 2 * (B / MaxValue);
            Coord2[3] :=  1 / 2 * (C / MaxValue);   for j := 1 to 3 do Arrow[1, j] := Coord[j];
            for j := 1 to 3 do Arrow[2, j] := Coord2[j];
            PlotLine;

            Coord[1] :=  1 / 2 * (A / MaxValue);    Coord[2] := -1 / 2 * (B / MaxValue);
            Coord[3] :=  1 / 2 * (C / MaxValue);
            Coord2[1] := -1 / 2 * (A / MaxValue);   Coord2[2] := -1 / 2 * (B / MaxValue);
            Coord2[3] :=  1 / 2 * (C / MaxValue);   for j := 1 to 3 do Arrow[1, j] := Coord[j];
            for j := 1 to 3 do Arrow[2, j] := Coord2[j];
            PlotLine;
            Coord2[1] :=  1 / 2 * (A / MaxValue);   Coord2[2] :=  1 / 2 * (B / MaxValue);
            Coord2[3] :=  1 / 2 * (C / MaxValue);   for j := 1 to 3 do Arrow[1, j] := Coord[j];
            for j := 1 to 3 do Arrow[2, j] := Coord2[j];
            PlotLine;
            Coord2[1] :=  1 / 2 * (A / MaxValue);   Coord2[2] := -1 / 2 * (B / MaxValue);
            Coord2[3] := -1 / 2 * (C / MaxValue);   for j := 1 to 3 do Arrow[1, j] := Coord[j];
            for j := 1 to 3 do Arrow[2, j] := Coord2[j];
            PlotLine;

            Coord[1] := -1 / 2 * (A / MaxValue);    Coord[2] :=  1 / 2 * (B / MaxValue);
            Coord[3] :=  1 / 2 * (C / MaxValue);
            Coord2[1] :=  1 / 2 * (A / MaxValue);   Coord2[2] :=  1 / 2 * (B / MaxValue);
            Coord2[3] :=  1 / 2 * (C / MaxValue);   for j := 1 to 3 do Arrow[1, j] := Coord[j];
            for j := 1 to 3 do Arrow[2, j] := Coord2[j];
            PlotLine;
            Coord2[1] := -1 / 2 * (A / MaxValue);   Coord2[2] := -1 / 2 * (B / MaxValue);
            Coord2[3] :=  1 / 2 * (C / MaxValue);   for j := 1 to 3 do Arrow[1, j] := Coord[j];
            for j := 1 to 3 do Arrow[2, j] := Coord2[j];
            PlotLine;
            Coord2[1] := -1 / 2 * (A / MaxValue);   Coord2[2] :=  1 / 2 * (B / MaxValue);
            Coord2[3] := -1 / 2 * (C / MaxValue);   for j := 1 to 3 do Arrow[1, j] := Coord[j];
            for j := 1 to 3 do Arrow[2, j] := Coord2[j];
            PlotLine;
            Pen.Width := SavedWidth;
            if GetMySiteList.GetCalcResults <> nil then
                with GetMySiteList.GetCalcResults as TCalcResults do
                begin
                    St := FloatToStrF(RFactor1, ffGeneral, 6, 4);
                    St := 'R(Quadr.) = ' + St + ' %';
                    TextOut(GetMyWidth - 150, 20, St);
                    St := FloatToStrF(RFactor2, ffGeneral, 6, 4);
                    St := 'R(Linear)  = ' + St + ' %';
                    TextOut(GetMyWidth - 150, 40, St);
                end;
        end;
    end;

    procedure PlotSphere(Radius: Double; St: string);
    var RadInt: Integer;
        SavedColor: TColor;
    begin
        with GetMyBufCanvas do
        begin
            StandardVectTrans(Coord3);
            x3 := GetMyWidth div 2 + Round((Coord3[1]) * MinSize);
            y3 := GetMyHeight div 2 - Round((Coord3[2]) * MinSize);
            RadInt := Round(Radius);
            Ellipse(x3 - RadInt, y3 - RadInt, x3 + RadInt, y3 + RadInt);
            SavedColor := GetMyBufCanvas.Brush.Color;
            GetMyBufCanvas.Brush.Color := PaintColor;
            if RadInt >= 2 then FloodFill(x3, y3, PaintColor,
            {$IFNDEF Lazarus}
            fsBorder
            {$ELSE}
            tFillStyle.fsBorder
            {$ENDIF}
            );
            TextOut(x3 - TextWidth(St) div 2, y3 - TextHeight(St) div 2, St);
            GetMyBufCanvas.Brush.Color := SavedColor;
        end;
    end;

    procedure PlotSphereNum(Num: LongInt);
    var St: String;
        SavedColor: TColor;    
    begin
        with GetMyBufCanvas do begin
            StandardVectTrans(Coord3);
            x3 := GetMyWidth div 2 + Round((Coord3[1]) * MinSize);
            y3 := GetMyHeight div 2 - Round((Coord3[2]) * MinSize);
            Ellipse(x3 - 15, y3 - 15, x3 + 15, y3 + 15);
            Str(Num, St);

            SavedColor := GetMyBufCanvas.Brush.Color;
            GetMyBufCanvas.Brush.Color := PaintColor;
            FloodFill(x3, y3, PaintColor, fsBorder);
            TextOut(x3 - TextWidth(St) div 2, y3 - TextHeight(St) div 2, St);
            GetMyBufCanvas.Brush.Color := SavedColor;
        end;
    end;

    function GetMaxModule: Double;
    var i, j: LongInt;
        TA: TAtom;
        TS: TSite;
    begin
        Result := 0;
        for i := 0 to GetSiteList.Count - 1 do
        begin
            TS := TSite(FSiteList.Items[i]);
            for j := 0 to TS.AtomList.Count - 1 do begin
                TA := TAtom(TS.AtomList.Items[j]);
                if TA.M > Result then Result := TA.M;
            end;
        end;
    end;

begin
    with GetMyBufCanvas do
    begin
        Brush.Color := clWhite;
        R := Rect(0, 0, GetMyWidth, GetMyHeight);
        FillRect(R);
        if GetMySiteList = nil then begin ActualPlot; Exit; end;
        with GetMySiteList do
        begin
            if (Abs(A) < TINY) or (Abs(B) < TINY) or (Abs(C) < TINY)
            or (Abs(Alpha) < TINY) or (Abs(Beta) < TINY) or (Abs(Gamma) < TINY) then
            begin ActualPlot; Exit; end;
            MaxModule := GetMaxModule;
            Transition(Alpha, Beta, Gamma);
            AddAtomsToCommonList;
            FCommonAtomList.Sort(ViewStructListSort);
            MaxValue := Sqrt(Sqr(A) + Sqr(B) + Sqr(C));
            MinSize := GetMyHeight;
            if GetMyWidth < MinSize then MinSize := GetMyWidth;
            with GetMyBufCanvas do
            begin
                Pen.Style := psSolid;
                for i := 0 to FCommonAtomList.Count - 1 do
                begin
                    SR := TAtom(FCommonAtomList.Items[i]);

                    PaintColor := TSitePlotParams(SR.GetSite.GetPlotParams).Color;
                    with SR do
                    begin
                        Coord[1] := (X - 1 / 2) *
                            (A / MaxValue) + UnitMagnVect[1] * ARROW_SCALE;
                        Coord[2] := (Y - 1 / 2) *
                            (B / MaxValue) + UnitMagnVect[2] * ARROW_SCALE;
                        Coord[3] := (Z - 1 / 2) *
                            (C / MaxValue) + UnitMagnVect[3] * ARROW_SCALE;
                        Coord2[1] := (X - 1 / 2) *
                            (A / MaxValue) - UnitMagnVect[1] * ARROW_SCALE;
                        Coord2[2] := (Y - 1 / 2) *
                            (B / MaxValue) - UnitMagnVect[2] * ARROW_SCALE;
                        Coord2[3] := (Z - 1 / 2) *
                            (C / MaxValue) - UnitMagnVect[3] * ARROW_SCALE;
                        Coord3[1] := (X - 1 / 2) * (A / MaxValue);
                        Coord3[2] := (Y - 1 / 2) * (B / MaxValue);
                        Coord3[3] := (Z - 1 / 2) * (C / MaxValue);
                        Arrow[1] := Coord2;
                        Arrow[2] := Coord;
                        TempColor := GetMyBufCanvas.Pen.Color;
                        TempColor2 := GetMyBufCanvas.Font.Color;
                        GetMyBufCanvas.Pen.Color := PaintColor;
                        GetMyBufCanvas.Font.Color := (not PaintColor) and $FFFFFF;
                        case TSitePlotParams(SR.GetSite.GetPlotParams).MomentsPlotMode of
                            PM_ARROWS : begin
                                PlotArrow('');
                                PlotSphere(4, '');
                            end;
                            PM_MODULES : if MaxModule <> 0 then
                                PlotSphere(M / MaxModule * 20, SR.Position);
                            PM_SERIAL_NUMBER : PlotSphereNum(SR.Number);
                            PM_ELEMENT : PlotSphere(20, SR.Element);
                        end;
                        GetMyBufCanvas.Pen.Color := TempColor;
                        GetMyBufCanvas.Font.Color := TempColor2;
                    end;
                end;    TempColor := GetMyBufCanvas.Pen.Color;
                TempColor2 := GetMyBufCanvas.Font.Color;
                GetMyBufCanvas.Pen.Color := clBlack;
                GetMyBufCanvas.Font.Color := clBlack;
                PlotAxes;
                GetMyBufCanvas.Pen.Color := TempColor;
                GetMyBufCanvas.Font.Color := TempColor2;
            end;
        end;    end;    ActualPlot;     end;

procedure TBufferedPlotter3D.ActualPlot;
begin
    GetMyOutCanvas.CopyRect(Rect(0, 0, GetMyWidth, GetMyHeight),
        GetMyBufCanvas, Rect(0, 0, GetMyWidth, GetMyHeight));
end;

constructor TSitePlotParams.Create(AOwner: TComponent);
begin
    inherited;
    FMomentsPlotMode := PM_ARROWS;
end;

procedure TSitePlotParams.SetSite(const ASite: TSite);
begin
    FSite := ASite;
end;

function TSitePlotParams.GetSite: TSite;
begin
    Result := FSite;
end;

procedure TSitePlotParams.CopyParameters(const Dest: TObject);
begin
    inherited;
    TSitePlotParams(Dest).MomentsPlotMode := MomentsPlotMode;
    TSitePlotParams(Dest).Color := Color;
end;

constructor TSiteListPlotParams.Create(AOwner: TComponent);
begin
    inherited;
end;

procedure TSiteListPlotParams.SetSiteList(const ASiteList: TSiteList);
begin
    FSiteList := ASiteList;
end;

function TSiteListPlotParams.GetSiteList: TSiteList;
begin
    Result := FSiteList;
end;

procedure TSiteListPlotParams.CopyParameters(const Dest: TObject);
begin
    inherited;
end;

procedure TPlotter3D.SetSiteList(const ASiteList: TSiteList);
begin
    FSiteList := ASiteList;
end;

function TPlotter3D.GetSiteList: TSiteList;
begin
    Result := FSiteList;
end;

constructor TPlotter3D.Create;
begin
    inherited;
    FCommonAtomList := TComponentList.Create(nil);
end;

destructor TPlotter3D.Destroy;
begin
    UtilizeObject(FCommonAtomList);
    inherited;
end;

procedure TPlotter3D.AddAtomsToCommonList;

    function GetFullAtomsNumberInEnabled: LongInt;
        var i: LongInt;
        TS: TSite;
    begin
        Result := 0;
        with GetMySiteList do
            for i := 0 to Count - 1 do
            begin
                TS := TSite(Items[i]);
                if not TS.Disabled then Result := Result + TS.MagnAtomsNumber;
            end;
    end;

var i, j, CommonIndex: LongInt;
    TS: TSite;
    Flag: Boolean;
    TA: TAtom;
begin
    if GetFullAtomsNumberInEnabled <> FCommonAtomList.Count then
    begin FCommonAtomList.Clear; Flag := True end
    else Flag := False;
    CommonIndex := 0;
    for i := 0 to GetMySiteList.Count - 1 do
    begin
        TS := TSite(FSiteList.Items[i]);
        if not TS.Disabled then
            for j := 0 to TS.MagnAtomsNumber - 1 do
            begin
                TA := TS.MagnAtomInChemical[j];
                if Flag then FCommonAtomList.Add(TComponent(TA.GetCopy))
                else TA.CopyParameters(FCommonAtomList.Items[CommonIndex]);
                TAtom(FCommonAtomList.Items[CommonIndex]).SetSite(TA.GetSite);
                TAtom(FCommonAtomList.Items[CommonIndex]).SetSiteList(TA.GetSiteList);
                Inc(CommonIndex);
            end;
    end;
end;

function TPlotter3D.GetMySiteList: TSiteList;
begin
    if Assigned(FSiteList) then Result := FSiteList
    else raise EPlotter3D.Create('Sites list must be assigned...');
end;

procedure TBufferedPlotter3D.SetOutCanvas(const AOutCanvas: TCanvas);
begin
    if not Assigned(AOutCanvas) then
        raise EBufferedPlotter3D.Create('An attempt to set up unassigned output canvas...');
    FOutCanvas := AOutCanvas;
end;

function TBufferedPlotter3D.GetMyOutCanvas: TCanvas;
begin
    if Assigned(FOutCanvas) then Result := FOutCanvas
    else raise EBufferedPlotter3D.Create('The output canvas must be assigned...');
end;

function TIntBufPlotter3D.GetMyBufCanvas: TCanvas;
begin
    Result := FBufBitMap.Canvas;
end;

function TIntBufPlotter3D.GetMyHeight: LongInt;
begin
    Result := FBufBitMap.Height;
end;

function TIntBufPlotter3D.GetMyWidth: LongInt;
begin
    Result := FBufBitMap.Width;
end;

procedure TIntBufPlotter3D.SetHeight(const AHeight: LongInt);
begin
    FBufBitMap.Height := AHeight;
end;

procedure TIntBufPlotter3D.SetWidth(const AWidth: LongInt);
begin
    FBufBitMap.Width := AWidth;
end;

constructor TIntBufPlotter3D.Create;
begin
    inherited;
    FBufBitMap := TBitMap.Create;
end;

destructor TIntBufPlotter3D.Destroy;
begin
    UtilizeObject(FBufBitMap);
    inherited;
end;

procedure TAdvancedPlotter3D.Plot;
begin
end;

function ViewStructListSortFunc(Item1, Item2: Pointer): Integer;
var SR1: TAtom absolute Item1;
    SR2: TAtom absolute Item2;
    V1, V2: T3Vector;
begin
    V1[1] := SR1.x; V1[2] := SR1.y; V1[3] := SR1.z;
    V2[1] := SR2.x; V2[2] := SR2.y; V2[3] := SR2.z;
    StandardVectTrans(V1);
    StandardVectTrans(V2);
    if V1[3] < V2[3] then begin Result := -1; Exit end;
    if V1[3] > V2[3] then begin Result := 1; Exit end;
    if V1[3] = V2[3] then begin Result := 0; Exit end;
end;

class function TSitePlotParams.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := SPPClassInheritID;
    Result.PropVersionNum := SPPCurVerNum;
end;

class procedure TSitePlotParams.ReadProperties(const Reader: TReader;
    const PropHeaderRec: TPropHeaderRec;
    const AnObject: TSelfSavedComponent);
begin
    with AnObject as Self, Reader do
    begin
        FMomentsPlotMode := ReadInteger;
        FColor := ReadInteger;
    end;
end;

class procedure TSitePlotParams.WriteProperties(const Writer: TWriter;
    const AnObject: TSelfSavedComponent);
begin
    with AnObject as Self, Writer do
    begin
        WriteInteger(FMomentsPlotMode);
        WriteInteger(FColor);
    end;
end;

{ TSLPP }

class function TSLPP.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := SLPPAlClassInheritID;
    Result.PropVersionNum := SLPPAlCurVerNum;
end;

class procedure TSLPP.ReadProperties(const Reader: TReader;
    const PropHeaderRec: TPropHeaderRec;
    const AnObject: TSelfSavedComponent);
begin

end;

class procedure TSLPP.WriteProperties(const Writer: TWriter;
    const AnObject: TSelfSavedComponent);
begin

end;

{ TSPP }

class function TSPP.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := SPPAlClassInheritID;
    Result.PropVersionNum := SPPAlCurVerNum;
end;

class procedure TSPP.ReadProperties(const Reader: TReader;
    const PropHeaderRec: TPropHeaderRec;
    const AnObject: TSelfSavedComponent);
begin

end;

class procedure TSPP.WriteProperties(const Writer: TWriter;
    const AnObject: TSelfSavedComponent);
begin

end;

class function TSiteListPlotParams.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := SLPPClassInheritID;
    Result.PropVersionNum := SLPPCurVerNum;
end;

class procedure TSiteListPlotParams.ReadProperties(const Reader: TReader;
    const PropHeaderRec: TPropHeaderRec;
    const AnObject: TSelfSavedComponent);
begin

end;

class procedure TSiteListPlotParams.WriteProperties(const Writer: TWriter;
    const AnObject: TSelfSavedComponent);
begin

end;

{ TExtBufPlotter3D }

function TExtBufPlotter3D.GetMyBufCanvas: TCanvas;
begin
    Result := FBufCanvas;
end;

function TExtBufPlotter3D.GetMyHeight: LongInt;
begin
    Result := FHeight;
end;

function TExtBufPlotter3D.GetMyWidth: LongInt;
begin
    Result := FWidth;
end;

procedure TExtBufPlotter3D.SetBufCanvas(const ABufCanvas: TCanvas);
begin
    if not Assigned(ABufCanvas) then
        raise EExtBufPlotter3D.Create('An attempt to set up unassigned buffer canvas...');
    FBufCanvas := ABufCanvas;
end;

procedure TExtBufPlotter3D.SetHeight(const AHeight: Integer);
begin
    FHeight := AHeight;
end;

procedure TExtBufPlotter3D.SetWidth(const AWidth: Integer);
begin
    FWidth := AWidth;
end;

initialization
    RegisterClass(TSitePlotParams);
    RegisterClass(TSiteListPlotParams);

    RegisterClass(TSPP);
    RegisterClass(TSLPP);
end.
