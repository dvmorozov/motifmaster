{------------------------------------------------------------------------------
    This file is part of the MotifMASTER project. This software is
    distributed under GPL (see gpl.txt for details).

    This software is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Copyright (C) 1999-2007 D.Morozov (dvmorozov@mail.ru)
------------------------------------------------------------------------------}

unit Minimizer_S;

interface

uses Minimizer, MSCRDataClasses, ComponentList, Classes;

type

 TSimpleMinimizer = class(TMinimizer)
 public
  procedure Minimize(var ErrorCode : LongInt); override;
 end;

 TSimpleMinimizer2 = class(TMinimizer)
 public
  DivideStepsBy2 : procedure of object;
  EndOfCalculation : function : Boolean of object;
  procedure Minimize(var ErrorCode : LongInt); override;
 end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Minimax',[TSimpleMinimizer]);
  RegisterComponents('Minimax',[TSimpleMinimizer2]);
end;

{============================== TSimpleMinimizer ==============================}
procedure TSimpleMinimizer.Minimize(var ErrorCode : LongInt);
var Step : Double;
    SaveParam : Double;
    SaveMinimizeValue : Double;
    MinimizeValue,MinimizeValue2 : Double;
    MinIndex : LongInt;
    TotalMinimum : Double;
begin
 ErrorCode := IsReady;
 Terminated := False;
 if ErrorCode <> MIN_NO_ERRORS then Exit;
 Step := OnGetStep;
 while (Step >= 0.001) and (not Terminated) do
  begin
   OnSetFirstParam;
   Step := OnGetStep;
   OnCalcFunc;
   TotalMinimum := OnFunc;
   while (not OnEndOfCycle) and (not Terminated) do
    begin
      OnCalcFunc;
      SaveParam := OnGetParam;
      SaveMinimizeValue := OnFunc;

      OnSetParam(SaveParam + Step);
      OnCalcFunc;
      MinimizeValue := OnFunc;

      OnSetParam(SaveParam - Step);
      OnCalcFunc;
      MinimizeValue2 := OnFunc;

      OnSetParam(SaveParam);
      MinIndex := 0;

      if (MinimizeValue >= SaveMinimizeValue) and (MinimizeValue2 >= SaveMinimizeValue) then MinIndex := 0;
      if (MinimizeValue >= SaveMinimizeValue) and (MinimizeValue2 < SaveMinimizeValue) then MinIndex := 2;
      if (MinimizeValue < SaveMinimizeValue) and (MinimizeValue2 >= SaveMinimizeValue) then MinIndex := 1;
      if (MinimizeValue < SaveMinimizeValue) and (MinimizeValue2 < SaveMinimizeValue) then
       begin
        if MinimizeValue <= MinimizeValue2 then MinIndex := 1 else MinIndex := 2;
       end;

      case MinIndex of
       1 : OnSetParam(SaveParam + Step);
       2 : OnSetParam(SaveParam - Step);
      end;

      OnCalcFunc;

      if Assigned(OnShowCurMin) then
       if OnFunc < SaveMinimizeValue then OnShowCurMin;
      OnSetNextParam;
    end;
    if OnFunc >= TotalMinimum then OnSetStep(Step / 2);
  end;{while (Step > 1e-5) and (not Terminated) do...}
end;

{============================== TSimpleMinimizer2 =============================}
procedure TSimpleMinimizer2.Minimize(var ErrorCode : LongInt);
var Step : Double;
    SaveParam : Double;
    SaveMinimizeValue : Double;
    MinimizeValue,MinimizeValue2 : Double;
    MinIndex : LongInt;
    TotalMinimum : Double;
begin
 ErrorCode := IsReady;
 Terminated := False;
 if ErrorCode <> MIN_NO_ERRORS then Exit;
 while (not EndOfCalculation) and (not Terminated) do
  begin
   OnSetFirstParam;
   TotalMinimum := OnFunc;
   while (not OnEndOfCycle) and (not Terminated) do
    begin
      Step := OnGetStep;
      SaveParam := OnGetParam;
      SaveMinimizeValue := OnFunc;

      OnSetParam(SaveParam + Step);
      OnCalcFunc;
      MinimizeValue := OnFunc;

      OnSetParam(SaveParam - Step);
      OnCalcFunc;
      MinimizeValue2 := OnFunc;

      OnSetParam(SaveParam);
      MinIndex := 0;
      if (MinimizeValue >= SaveMinimizeValue) and (MinimizeValue2 >= SaveMinimizeValue) then MinIndex := 0;
      if (MinimizeValue >= SaveMinimizeValue) and (MinimizeValue2 < SaveMinimizeValue) then MinIndex := 2;
      if (MinimizeValue < SaveMinimizeValue) and (MinimizeValue2 >= SaveMinimizeValue) then MinIndex := 1;
      if (MinimizeValue < SaveMinimizeValue) and (MinimizeValue2 < SaveMinimizeValue) then
       begin
        if MinimizeValue <= MinimizeValue2 then MinIndex := 1 else MinIndex := 2;
       end;
      case MinIndex of
       1 : OnSetParam(SaveParam + Step);
       2 : OnSetParam(SaveParam - Step);
      end;
      OnCalcFunc;
      if Assigned(OnShowCurMin) then OnShowCurMin;
      OnSetNextParam;
    end;
   if Abs(OnFunc - TotalMinimum)/TotalMinimum < 1e-5 then
    if Assigned(DivideStepsBy2) then DivideStepsBy2;
  end;{while (not EndOfCalculation) and (not Terminated) do...}
end;

end.
