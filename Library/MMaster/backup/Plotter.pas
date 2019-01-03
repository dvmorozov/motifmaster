{------------------------------------------------------------------------------
    This file is part of the MotifMASTER project. This software is
    distributed under GPL (see gpl.txt for details).

    This software is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Copyright (C) 1999-2007 D.Morozov (dvmorozov@mail.ru)
------------------------------------------------------------------------------}

unit Plotter;

interface

uses ExtCtrls, WinTypes, Graphics, Math3d, Classes, SysUtils, CalcModule,
     DataClasses, ComponentList, SimpMath, SelfCopied, Tools;

const
  
  PM_EXP_INTENSITY = 1;
  PM_NUCLEAR_INTENSITY = 2;
  PM_INTENSITY = 4;
  PM_STRUCT_FACT = 8;
  
  PM_ARROWS = 5;
  PM_MODULES = 6;
  PM_SERIAL_NUMBER = 7;
  PM_ELEMENT = 8;

procedure Plot3D(A, B, C, Alpha, Beta, Gamma: Double; PlotMode: Integer;
TempBitMap: TBitMap; OutPaintBox: TPaintBox; ViewStructList: TAtomList);
procedure PlotDiagr(PlotMode: Integer; TempBitMap: TBitMap;
OutPaintBox: TPaintBox; ViewNeutronList: TSinTCompList);
procedure ActualPlot(TempBitMap: TBitMap; OutPaintBox: TPaintBox);

const
  CL_EXP_INTENSITY: TColor = clRed;
  CL_NUCLEAR_INTENSITY: TColor = clBlue;
  CL_INTENSITY: TColor = clBlack;

  ARROW_SCALE: Double = 0.3;    

function ViewStructListSortFunc(Item1, Item2: Pointer): Integer;

const
  ViewStructListSort: TListSortCompare = ViewStructListSortFunc;
  

implementation

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

var ViewPositionList: TSelfCleanList;


const
  PosColNum = 4;
  PositionColors: array[0..PosColNum] of TColor =
  (clRed, clBlue, clGreen, clBlack, clMaroon);

procedure Plot3D(A, B, C, Alpha, Beta, Gamma: Double; PlotMode: Integer;
TempBitMap: TBitMap; OutPaintBox: TPaintBox; ViewStructList: TAtomList);
var Coord, Coord2, Coord3: T3Vector;
    x, y, x3, y3: Integer;
    DeltaX, DeltaY: Double;
    R: TRect;
    MaxValue: Double;
    MinSize: Integer;
    MaxModule: Double;
    Arrow: array[1..2] of T3Vector;
    

  procedure PlotArrow(St: String); 
  var DrawPoints: array[1..5] of TPoint; 
      l: Double;
      a: Double;
      Alpha: Double;
  begin
    with TempBitMap.Canvas do begin
      StandardVectTrans(Arrow[1]);
      x := TempBitMap.Width div 2 + Round((Arrow[1, 1]) * MinSize);
      y := TempBitMap.Height div 2 - Round((Arrow[1, 2]) * MinSize);
      DrawPoints[1].x := x;
      DrawPoints[1].y := y;
      StandardVectTrans(Arrow[2]);
      x := TempBitMap.Width div 2 + Round((Arrow[2, 1]) * MinSize);
      y := TempBitMap.Height div 2 - Round((Arrow[2, 2]) * MinSize);
      DrawPoints[2].x := x; 
      DrawPoints[2].y := y;
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
      TextOut(DrawPoints[2].x + 3, DrawPoints[2].y - 3, St);
    end;
  end;

  procedure PlotLine; 
  var DrawPoints: array[1..2] of TPoint;
  begin
    with TempBitMap.Canvas do begin
      StandardVectTrans(Arrow[1]); 
      x := TempBitMap.Width div 2 + Round((Arrow[1, 1]) * MinSize); 
      y := TempBitMap.Height div 2 - Round((Arrow[1, 2]) * MinSize); 
      DrawPoints[1].x := x; 
      DrawPoints[1].y := y; 
      StandardVectTrans(Arrow[2]); 
      x := TempBitMap.Width div 2 + Round((Arrow[2, 1]) * MinSize);
      y := TempBitMap.Height div 2 - Round((Arrow[2, 2]) * MinSize); 
      DrawPoints[2].x := x; 
      DrawPoints[2].y := y; 
      MoveTo(DrawPoints[1].x, DrawPoints[1].y); 
      LineTo(DrawPoints[2].x, DrawPoints[2].y); 
    end; 
  end; 

  procedure PlotAxes; 
  var SaveWidth, j: Integer; 
      St: string; 
  begin
    with TempBitMap.Canvas do begin
      SaveWidth := Pen.Width; 
      Pen.Width := 2; 
      Coord[1] := -1 / 2 * (A / MaxValue); 
      Coord[2] := -1 / 2 * (B / MaxValue); 
      Coord[3] := -1 / 2 * (C / MaxValue); 
      Coord2[1] :=  1 / 2 * (A / MaxValue); 
      Coord2[2] := -1 / 2 * (B / MaxValue); 
      Coord2[3] := -1 / 2 * (C / MaxValue); 
      for j := 1 to 3 do Arrow[1, j] := Coord[j]; 
      for j := 1 to 3 do Arrow[2, j] := Coord2[j]; 
      PlotArrow('(1, 0, 0)'); 
      Coord2[1] := -1 / 2 * (A / MaxValue); 
      Coord2[2] :=  1 / 2 * (B / MaxValue); 
      Coord2[3] := -1 / 2 * (C / MaxValue); 
      for j := 1 to 3 do Arrow[1, j] := Coord[j]; 
      for j := 1 to 3 do Arrow[2, j] := Coord2[j]; 
      PlotArrow('(0, 1, 0)'); 
      Coord2[1] := -1 / 2 * (A / MaxValue); 
      Coord2[2] := -1 / 2 * (B / MaxValue); 
      Coord2[3] :=  1 / 2 * (C / MaxValue); 
      for j := 1 to 3 do Arrow[1, j] := Coord[j]; 
      for j := 1 to 3 do Arrow[2, j] := Coord2[j];
      PlotArrow('(0, 0, 1)'); 

      Pen.Width := 1; 
      Pen.Style := psDot; 

      Coord[1] :=  1 / 2 * (A / MaxValue);  {1, 1, 0}
      Coord[2] :=  1 / 2 * (B / MaxValue); 
      Coord[3] := -1 / 2 * (C / MaxValue); 
      Coord2[1] :=  1 / 2 * (A / MaxValue); {1, 0, 0}
      Coord2[2] := -1 / 2 * (B / MaxValue); 
      Coord2[3] := -1 / 2 * (C / MaxValue); 
      for j := 1 to 3 do Arrow[1, j] := Coord[j]; 
      for j := 1 to 3 do Arrow[2, j] := Coord2[j]; 
      PlotLine; 
      Coord2[1] := -1 / 2 * (A / MaxValue); {0, 1, 0}
      Coord2[2] :=  1 / 2 * (B / MaxValue); 
      Coord2[3] := -1 / 2 * (C / MaxValue); 
      for j := 1 to 3 do Arrow[1, j] := Coord[j];
      for j := 1 to 3 do Arrow[2, j] := Coord2[j]; 
      PlotLine; 
      Coord2[1] :=  1 / 2 * (A / MaxValue); {1, 1, 1}
      Coord2[2] :=  1 / 2 * (B / MaxValue); 
      Coord2[3] :=  1 / 2 * (C / MaxValue); 
      for j := 1 to 3 do Arrow[1, j] := Coord[j]; 
      for j := 1 to 3 do Arrow[2, j] := Coord2[j]; 
      PlotLine; 

      Coord[1] :=  1 / 2 * (A / MaxValue);  {1, 0, 1}
      Coord[2] := -1 / 2 * (B / MaxValue); 
      Coord[3] :=  1 / 2 * (C / MaxValue); 
      Coord2[1] := -1 / 2 * (A / MaxValue); {0, 0, 1}
      Coord2[2] := -1 / 2 * (B / MaxValue); 
      Coord2[3] :=  1 / 2 * (C / MaxValue); 
      for j := 1 to 3 do Arrow[1, j] := Coord[j]; 
      for j := 1 to 3 do Arrow[2, j] := Coord2[j]; 
      PlotLine;
      Coord2[1] :=  1 / 2 * (A / MaxValue); {1, 1, 1}
      Coord2[2] :=  1 / 2 * (B / MaxValue); 
      Coord2[3] :=  1 / 2 * (C / MaxValue); 
      for j := 1 to 3 do Arrow[1, j] := Coord[j]; 
      for j := 1 to 3 do Arrow[2, j] := Coord2[j]; 
      PlotLine; 
      Coord2[1] :=  1 / 2 * (A / MaxValue); {1, 0, 0}
      Coord2[2] := -1 / 2 * (B / MaxValue); 
      Coord2[3] := -1 / 2 * (C / MaxValue); 
      for j := 1 to 3 do Arrow[1, j] := Coord[j];
      for j := 1 to 3 do Arrow[2, j] := Coord2[j]; 
      PlotLine; 

      Coord[1] := -1 / 2 * (A / MaxValue);  {0, 1, 1}
      Coord[2] :=  1 / 2 * (B / MaxValue); 
      Coord[3] :=  1 / 2 * (C / MaxValue); 
      Coord2[1] :=  1 / 2 * (A / MaxValue); {1, 1, 1}
      Coord2[2] :=  1 / 2 * (B / MaxValue);
      Coord2[3] :=  1 / 2 * (C / MaxValue); 
      for j := 1 to 3 do Arrow[1, j] := Coord[j]; 
      for j := 1 to 3 do Arrow[2, j] := Coord2[j]; 
      PlotLine; 
      Coord2[1] := -1 / 2 * (A / MaxValue); {0, 0, 1}
      Coord2[2] := -1 / 2 * (B / MaxValue); 
      Coord2[3] :=  1 / 2 * (C / MaxValue); 
      for j := 1 to 3 do Arrow[1, j] := Coord[j]; 
      for j := 1 to 3 do Arrow[2, j] := Coord2[j]; 
      PlotLine; 
      Coord2[1] := -1 / 2 * (A / MaxValue); {0, 1, 0}
      Coord2[2] :=  1 / 2 * (B / MaxValue); 
      Coord2[3] := -1 / 2 * (C / MaxValue); 
      for j := 1 to 3 do Arrow[1, j] := Coord[j]; 
      for j := 1 to 3 do Arrow[2, j] := Coord2[j]; 
      PlotLine; 
      Pen.Width := SaveWidth; 
 
    end; 
  end; 

  procedure PlotSphere(Radius: Double; St: string); 
  var RadInt: Integer; 
  begin
    with TempBitMap.Canvas do begin
      
      StandardVectTrans(Coord3); 
      x3 := TempBitMap.Width div 2 + Round((Coord3[1]) * MinSize); 
      y3 := TempBitMap.Height div 2 - Round((Coord3[2]) * MinSize); 
      RadInt := Round(Radius); 
      Ellipse(x3 - RadInt, y3 - RadInt, x3 + RadInt, y3 + RadInt);
      TextOut(x3 - TextWidth(St) div 2, y3 - TextHeight(St) div 2, St); 
    end; 
  end; 

  procedure PlotSphereNum(Num: LongInt); 
  var St: String; 
  begin
    with TempBitMap.Canvas do begin
      
      StandardVectTrans(Coord3); 
      x3 := TempBitMap.Width div 2 + Round((Coord3[1]) * MinSize); 
      y3 := TempBitMap.Height div 2 - Round((Coord3[2]) * MinSize); 
      Ellipse(x3 - 15, y3 - 15, x3 + 15, y3 + 15); 
      Str(Num, St); 
      TextOut(x3 - TextWidth(St) div 2, y3 - TextHeight(St) div 2, St); 
    end; 
  end; 

var SR: TAtom; 
    i, j, Count: LongInt; 
    PR: TPositionRec; 
    TempColor, TempColor2: TColor; 
begin
  
  
  with TempBitMap.Canvas do begin
    if (TempBitMap.Width <> OutPaintBox.Width) or
       (TempBitMap.Height <> OutPaintBox.Height) then begin
      TempBitMap.Width := OutPaintBox.Width; 
      TempBitMap.Height := OutPaintBox.Height; 
    end;
    Brush.Color := clWhite; 
    R := Rect(0, 0, TempBitMap.Width, TempBitMap.Height); 
    FillRect(R); 
    if not Assigned(ViewStructList) then
    begin ActualPlot(TempBitMap, OutPaintBox); Exit; end; 
    try
      if ViewStructList.Count = 0 then
      begin ActualPlot(TempBitMap, OutPaintBox); Exit; end; 
    except
      ActualPlot(TempBitMap, OutPaintBox); 
      Exit; 
    end; 
    if (A = 0) or (B = 0) or (C = 0) then
    begin ActualPlot(TempBitMap, OutPaintBox); Exit; end; 
    if (Alpha = 0) or (Beta = 0) or (Gamma = 0) then
    begin ActualPlot(TempBitMap, OutPaintBox); Exit; end; 
  end; 
  MaxModule := 0;
  for i := 0 to ViewStructList.Count - 1 do
  begin
    SR := TAtom(ViewStructList.Items[i]); 
    if SR.M > MaxModule then MaxModule := SR.M; 
  end; 
  Transition(Alpha, Beta, Gamma); 
  ViewStructList.Sort(ViewStructListSort); 
  ViewStructList.SortPosition(ViewPositionList); 
  ViewPositionList.Sort(ViewPositionListSort); 
  MaxValue := Sqrt(Sqr(A) + Sqr(B) + Sqr(C)); 
  MinSize := TempBitMap.Height; 
  if TempBitMap.Width < MinSize then MinSize := TempBitMap.Width; 
  with TempBitMap.Canvas do begin
    Pen.Style := psSolid; 
    Count := ViewStructList.Count; 
    for i := 0 to Count - 1 do begin
      SR := TAtom(ViewStructList.Items[i]);
      for j := 0 to ViewPositionList.Count - 1 do begin
        
        PR := ViewPositionList.Items[j]; 
        if UpperCase(PR.Position) = UpperCase(SR.Position) then Break; 
      end; 
      j := j mod (PosColNum + 1); 
      with SR do begin
        Coord[1] := (X - 1 / 2) * (A / MaxValue) + UnitMagnVect[1] * ARROW_SCALE;
        Coord[2] := (Y - 1 / 2) * (B / MaxValue) + UnitMagnVect[2] * ARROW_SCALE;
        Coord[3] := (Z - 1 / 2) * (C / MaxValue) + UnitMagnVect[3] * ARROW_SCALE;
        Coord2[1] := (X - 1 / 2) * (A / MaxValue) - UnitMagnVect[1] * ARROW_SCALE;
        Coord2[2] := (Y - 1 / 2) * (B / MaxValue) - UnitMagnVect[2] * ARROW_SCALE;
        Coord2[3] := (Z - 1 / 2) * (C / MaxValue) - UnitMagnVect[3] * ARROW_SCALE;
        Coord3[1] := (X - 1 / 2) * (A / MaxValue); 
        Coord3[2] := (Y - 1 / 2) * (B / MaxValue); 
        Coord3[3] := (Z - 1 / 2) * (C / MaxValue); 
        Arrow[1] := Coord2; 
        Arrow[2] := Coord; 
        TempColor := TempBitMap.Canvas.Pen.Color;
        TempColor2 := TempBitMap.Canvas.Font.Color; 
        TempBitMap.Canvas.Pen.Color := PositionColors[j]; 
        TempBitMap.Canvas.Font.Color := PositionColors[j]; 
        case PlotMode of
          PM_ARROWS: begin
                      PlotArrow(''); 
                      PlotSphere(3, ''); 
                    end; 
          PM_MODULES: if MaxModule <> 0 then
                      PlotSphere(M / MaxModule * 20, SR.Position); 
          PM_SERIAL_NUMBER: PlotSphereNum(SR.Number);
          PM_ELEMENT: PlotSphere(20, SR.Element);
        end; 
        TempBitMap.Canvas.Pen.Color := TempColor; 
        TempBitMap.Canvas.Font.Color := TempColor2; 
      end; 
    end; {for i := 0 to Count - 1 do...}
    TempColor := TempBitMap.Canvas.Pen.Color;
    TempColor2 := TempBitMap.Canvas.Font.Color; 
    TempBitMap.Canvas.Pen.Color := clBlack;
    TempBitMap.Canvas.Font.Color := clBlack;
    PlotAxes;
    TempBitMap.Canvas.Pen.Color := TempColor;
    TempBitMap.Canvas.Font.Color := TempColor2;
  end; {with TempBitMap do...}
  ActualPlot(TempBitMap, OutPaintBox); 
end;

procedure Plot3D_2(const A, B, C, Alpha, Beta, Gamma: Double;
TempBitMap: TBitMap; OutPaintBox: TPaintBox;
SiteList: TSiteList);
begin
end;

procedure ActualPlot(TempBitMap: TBitMap; OutPaintBox: TPaintBox);
begin
  with OutPaintBox do
    Canvas.CopyRect(Rect(0, 0, Width, Height),
    TempBitMap.Canvas, Rect(0, 0, Width, Height));
end;

const BottomAxesY: Integer = 40;
      LeftAxesX: Integer = 80;
      TopAxesY: Integer = 20;
      RightAxesX: Integer = 20;

procedure PlotDiagr(PlotMode: Integer; TempBitMap: TBitMap;
OutPaintBox: TPaintBox; ViewNeutronList: TSinTCompList);


  procedure GetScaleValues2(PlotMode: Integer;
  var StartX, StopX, StartY, StopY: Double; ViewNeutronList: TSinTCompList);
  
  var i: LongInt;
      SR: TSinTClass;
  begin
    StartX := 0;
    StopX := 0;
    StartY := 0;
    StopY := 0;
    with ViewNeutronList do
      for i := 0 to Count - 1 do
      begin
        SR := TSinTClass(Items[i]);
        if (PlotMode and PM_STRUCT_FACT = 0) then
        begin
          if (PlotMode and PM_INTENSITY <> 0) then
            if SR.Intensity > StopY then StopY := SR.Intensity;
          if (PlotMode and PM_EXP_INTENSITY <> 0) then
            if SR.ExpIntensity > StopY then StopY := SR.ExpIntensity;
          if (PlotMode and PM_NUCLEAR_INTENSITY <> 0) then
            if SR.NuclearIntensity > StopY then StopY := SR.NuclearIntensity;
        end
        else
        begin
          if (PlotMode and PM_INTENSITY <> 0) then
            if SR.Intensity / GetIntegralFactor(SR.SinT, SR.Sin2T) > StopY then
              StopY := SR.Intensity / GetIntegralFactor(SR.SinT, SR.Sin2T);
          if (PlotMode and PM_EXP_INTENSITY <> 0) then
            if SR.ExpIntensity / GetIntegralFactor(SR.SinT, SR.Sin2T) > StopY then
              StopY := SR.ExpIntensity / GetIntegralFactor(SR.SinT, SR.Sin2T);
          if (PlotMode and PM_NUCLEAR_INTENSITY <> 0) then
            if SR.NuclearIntensity / GetIntegralFactor(SR.SinT, SR.Sin2T) > StopY then
              StopY := SR.NuclearIntensity / GetIntegralFactor(SR.SinT, SR.Sin2T);
        end;
        if SR.SinTL > StopX then StopX := SR.SinTL;
      end;{for i := 0 to Count - 1 do...}
  end;

var StartX, StopX, StartY, StopY: Double;
    HR: TSinTClass; 
    i, Count: LongInt; 
    x, y: Integer; 
    R: TRect;
    St: string; 
    DeltaY: Double; 
    OutSin, DeltaX: Double; 

  procedure PlotIntensity(HR: TSinTClass;  ViewNeutronList: TSinTCompList);
  var x, y: Integer;
  begin
    with TempBitMap.Canvas, ViewNeutronList do
    begin
      try
        x := LeftAxesX + Round(((HR.SinTL - StartX) / (StopX - StartX))
        * (TempBitMap.Width - LeftAxesX - RightAxesX));
      except x := 0 end;
      try
        if (PlotMode and PM_STRUCT_FACT = 0) then
          y := TempBitMap.Height - BottomAxesY -
          Round(((HR.Intensity - StartY) / (StopY - StartY))
          * (TempBitMap.Height - BottomAxesY - TopAxesY))
        else
          y := TempBitMap.Height - BottomAxesY -
          Round(((HR.Intensity / GetIntegralFactor(HR.SinT, HR.Sin2T) - StartY)
          / (StopY - StartY)) * (TempBitMap.Height - BottomAxesY - TopAxesY))
      except y := 0 end;
      Pen.Color := CL_INTENSITY;
      MoveTo(x, y);
      LineTo(x, TempBitMap.Height - BottomAxesY);
      MoveTo(x - 2, y);
      LineTo(x + 3, y);
      

    end;
  end;

  procedure PlotExpIntensity(HR: TSinTClass; ViewNeutronList: TSinTCompList);
  var x, y: Integer;
  begin
    with TempBitMap.Canvas, ViewNeutronList do
    begin
      try
        x := LeftAxesX + Round(((HR.SinTL - StartX) / (StopX - StartX))
        * (TempBitMap.Width - LeftAxesX - RightAxesX));
      except x := 0 end;
      try
        if (PlotMode and PM_STRUCT_FACT = 0) then
          y := TempBitMap.Height - BottomAxesY -
          Round(((HR.ExpIntensity - StartY) / (StopY - StartY))
          * (TempBitMap.Height - BottomAxesY - TopAxesY))
        else
          y := TempBitMap.Height - BottomAxesY -
          Round(((HR.ExpIntensity / GetIntegralFactor(HR.SinT, HR.Sin2T) - StartY)
          / (StopY - StartY)) * (TempBitMap.Height - BottomAxesY - TopAxesY))
      except y := 0 end;
      Pen.Color := CL_EXP_INTENSITY;
      MoveTo(x, y);
      LineTo(x, TempBitMap.Height - BottomAxesY);
      MoveTo(x - 2, y);
      LineTo(x + 3, y);
    end;
  end;

  procedure PlotNuclearIntensity(HR: TSinTClass; ViewNeutronList: TSinTCompList);
  var x, y: Integer;
  begin
    with TempBitMap.Canvas, ViewNeutronList do
    begin
      try
        x := LeftAxesX + Round(((HR.SinTL - StartX) / (StopX - StartX))
        * (TempBitMap.Width - LeftAxesX - RightAxesX));
      except x := 0 end;
      try
        if (PlotMode and PM_STRUCT_FACT = 0) then
          y := TempBitMap.Height - BottomAxesY -
          Round(((HR.NuclearIntensity - StartY) / (StopY - StartY))
          * (TempBitMap.Height - BottomAxesY - TopAxesY))
        else
          y := TempBitMap.Height - BottomAxesY -
          Round(((HR.NuclearIntensity /
          GetIntegralFactor(HR.SinT, HR.Sin2T) - StartY) /
          (StopY - StartY)) * (TempBitMap.Height - BottomAxesY - TopAxesY))
      except y := 0 end;
      Pen.Color := CL_NUCLEAR_INTENSITY;
      MoveTo(x, y);
      LineTo(x, TempBitMap.Height - BottomAxesY);
      MoveTo(x - 2, y);
      LineTo(x + 3, y);
    end;
  end;

var TempLong: LongInt;
begin
  with TempBitMap.Canvas do begin
    if (TempBitMap.Width <> OutPaintBox.Width) or
       (TempBitMap.Height <> OutPaintBox.Height) then begin
      TempBitMap.Width := OutPaintBox.Width;
      TempBitMap.Height := OutPaintBox.Height;
    end;
    Brush.Color := clWhite;
    Pen.Style := psSolid;
    R := Rect(0, 0, TempBitMap.Width, TempBitMap.Height);
    FillRect(R);
    if not Assigned(ViewNeutronList) then
    begin ActualPlot(TempBitMap, OutPaintBox); Exit; end;
    try
      if ViewNeutronList.Count = 0 then
      begin ActualPlot(TempBitMap, OutPaintBox); Exit; end;
    except
      ActualPlot(TempBitMap, OutPaintBox);
      Exit;
    end;
    GetScaleValues2(PlotMode, StartX, StopX, StartY, StopY, ViewNeutronList);
    TempLong := GetNumberDegree(StopY - StartY);
    DeltaY := GetPowerOf10(TempLong);
    TempLong := Trunc((StopY - StartY) / DeltaY) + 1;
    if TempLong < 10 then begin
      if TempLong <= 2 then DeltaY := DeltaY / 10;
      if (TempLong > 2) and (TempLong < 3) then DeltaY := DeltaY * 2 / 10;
      if (TempLong >= 3) and (TempLong < 8) then DeltaY := DeltaY / 2;
      TempLong := Trunc((StopY - StartY) / DeltaY) + 1;
    end;
    StopY := StartY + TempLong * DeltaY;

    TempLong := GetNumberDegree(StopX - StartX);
    DeltaX := GetPowerOf10(TempLong);
    TempLong := Trunc((StopX - StartX) / DeltaX) + 1;
    if TempLong < 10 then begin
      if TempLong <= 2 then DeltaX := DeltaX / 10;
      if (TempLong > 2) and (TempLong < 3) then DeltaX := DeltaX * 2 / 10;
      if (TempLong >= 3) and (TempLong < 8) then DeltaX := DeltaX / 2;
      TempLong := Trunc((StopX - StartX) / DeltaX) + 1;
    end;
    StopX := StartX + TempLong * DeltaX;

    Count := ViewNeutronList.Count;
    for i := 0 to Count - 1 do
    begin
      HR := TSinTClass(ViewNeutronList.Items[i]);
      if (PlotMode and PM_INTENSITY <> 0) then
        PlotIntensity(HR, ViewNeutronList);
      if (PlotMode and PM_EXP_INTENSITY <> 0) then
        PlotExpIntensity(HR, ViewNeutronList);
      if (PlotMode and PM_NUCLEAR_INTENSITY <> 0) then
        PlotNuclearIntensity(HR, ViewNeutronList);
    end;
    Pen.Color := clBlack;
    Pen.Width := 2;
    MoveTo(LeftAxesX, TempBitMap.Height - BottomAxesY);
    LineTo(TempBitMap.Width - RightAxesX, TempBitMap.Height - BottomAxesY);
    MoveTo(LeftAxesX, TempBitMap.Height - BottomAxesY);
    LineTo(LeftAxesX, TopAxesY);
    Pen.Width := 1;

    TempLong := Trunc((StopY - StartY) / DeltaY);
    for i := 0 to TempLong do
    begin
      Str((StartY + i * DeltaY):5:2, St);
      y := TempBitMap.Height - BottomAxesY - TextHeight(St) div 2 -
      Round(((StartY + i * DeltaY) / (StopY - StartY)) *
      (TempBitMap.Height - BottomAxesY - TopAxesY));
      MoveTo(LeftAxesX, y + TextHeight(St) div 2);
      LineTo(LeftAxesX - 7, y + TextHeight(St) div 2);
      TextOut(LeftAxesX - TextWidth(St)- 9, y, St);
    end;

    TempLong := Trunc((StopX - StartX) / DeltaX);
    for i := 0 to TempLong do begin
      OutSin := StartX + DeltaX * i;
      Str(OutSin:4:2, St);
      x := LeftAxesX + Round(((StartX + i * DeltaX) / (StopX - StartX)) *
      (TempBitMap.Width - LeftAxesX - RightAxesX));
      MoveTo(x, TempBitMap.Height - BottomAxesY);
      LineTo(x, TempBitMap.Height - BottomAxesY + 7);
      TextOut(x - TextWidth(St) div 2, TempBitMap.Height - BottomAxesY + 9, St);
    end;
    if ViewNeutronList.GetCalcResults <> nil then
        with ViewNeutronList.GetCalcResults as TCalcResults do begin
            St := FloatToStrF(RFactor1, ffGeneral, 6, 4);
            St := 'R(Quadr.) = ' + St + ' %';
            TextOut(TempBitMap.Width - 150, 20, St);
            St := FloatToStrF(RFactor2, ffGeneral, 6, 4);
            St := 'R(Linear)  = ' + St + ' %';
            TextOut(TempBitMap.Width - 150, 40, St);
        end;
  end; {with TempBitMap.Canvas do...}
  ActualPlot(TempBitMap, OutPaintBox);
end;

initialization
  ViewPositionList := TSelfCleanList.Create;

finalization
  ViewPositionList.ClearAll;
  UtilizeObject(ViewPositionList);
end.
