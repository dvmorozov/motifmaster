{------------------------------------------------------------------------------
    This file is part of the MotifMASTER project. This software is
    distributed under GPL (see gpl.txt for details).

    This software is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Copyright (C) 1999-2007 D.Morozov (dvmorozov@mail.ru)
------------------------------------------------------------------------------}

unit CombEnumerator;

interface

uses SysUtils;

type
    ECombEnumerator = class(Exception);

    TCombEnumerator = class
    protected
        NumbersOfValues: array of LongInt;
        ValuesIndexes: array of LongInt;
        FCurrentComb: LongInt;
        FIsCombDefined: Boolean;

        function GetCombNumber: LongInt;
        function GetCombNumberStartStop(const StartIndex, StopIndex: LongInt): LongInt;
        procedure SetCurrentComb(const ACurrentComb: LongInt); virtual;
        function GetValueIndex(index: LongInt): LongInt;
        function GetValuesNumber: LongInt;

    public
        destructor Destroy; override;
        procedure AddNumberOfValues(const ANumberOfValues: LongInt); virtual;
        procedure ClearListOfNumbersOfValues; virtual;

        property CombNumber: LongInt read GetCombNumber;
        property CurrentComb: LongInt read FCurrentComb write SetCurrentComb;
        property ValueIndex[index: LongInt]: LongInt read GetValueIndex;
        property ValuesNumber: LongInt read GetValuesNumber;
    end;

    IDiscretValue = interface
        function GetNumberOfValues: LongInt;
        function GetValueIndex: LongInt;
        procedure SetValueIndex(const AValueIndex: LongInt);
        property NumberOfValues: LongInt read GetNumberOfValues;
        property ValueIndex: LongInt read GetValueIndex write SetValueIndex;
    end;

    TCombSelector = class(TCombEnumerator)
    protected
        FValuesList: array of IDiscretValue;
        procedure SetCurrentComb(const ACurrentComb: LongInt); override;

    public
        procedure AddNumberOfValues(const ANumberOfValues: LongInt); override;
        procedure ClearListOfNumbersOfValues; override;
        procedure AddDiscretValue(const AValue: IDiscretValue);
        procedure ClearDiscretValuesList;
    end;

implementation

destructor TCombEnumerator.Destroy;
begin
    ClearListOfNumbersOfValues;
    inherited Destroy;
end;

function TCombEnumerator.GetCombNumber: LongInt;
begin
    Result := GetCombNumberStartStop(0, ValuesNumber - 1);
end;

function TCombEnumerator.GetCombNumberStartStop(
    const StartIndex, StopIndex: LongInt): LongInt;
var i: LongInt;
begin
    Result := 0;
    for i := StartIndex to StopIndex do
    begin
        if (Result <> 0) and (NumbersOfValues[i] <> 0) then
            Result := Result * NumbersOfValues[i]
        else Result := Result + NumbersOfValues[i];
    end;
end;

procedure TCombEnumerator.AddNumberOfValues(const ANumberOfValues: LongInt);
begin
    SetLength(NumbersOfValues, Length(NumbersOfValues) + 1);
    SetLength(ValuesIndexes, Length(ValuesIndexes) + 1);
    NumbersOfValues[Length(NumbersOfValues) - 1] := ANumberOfValues;
    FIsCombDefined := False;
end;

procedure TCombEnumerator.ClearListOfNumbersOfValues;
begin
    FIsCombDefined := False;
    Finalize(NumbersOfValues);
    Finalize(ValuesIndexes);
end;

procedure TCombEnumerator.SetCurrentComb(const ACurrentComb: LongInt);
var TempCurrentComb, TempCombNumber: LongInt;
    i: LongInt;
begin
    if (ACurrentComb < 0) or (ACurrentComb >= CombNumber) then
        raise ECombEnumerator.Create('Invalid combination index...');
    TempCurrentComb := ACurrentComb;
    FCurrentComb := ACurrentComb;
    for i := 0 to ValuesNumber - 2 do
    begin
        TempCombNumber := GetCombNumberStartStop(i + 1, ValuesNumber - 1);
        ValuesIndexes[i] := TempCurrentComb div TempCombNumber;
        TempCurrentComb := TempCurrentComb mod TempCombNumber;
    end;
    ValuesIndexes[ValuesNumber - 1] := TempCurrentComb;
    FIsCombDefined := True;
end;

function TCombEnumerator.GetValueIndex(index: LongInt): LongInt;
begin
    if not FIsCombDefined then
        raise ECombEnumerator.Create('Combination must be defined...');
    if (index < 0) or (index >= ValuesNumber) then
        raise ECombEnumerator.Create('Invalid value index...')
    else Result := ValuesIndexes[index];
end;

function TCombEnumerator.GetValuesNumber: LongInt;
begin
    Result := Length(NumbersOfValues);
end;

procedure TCombSelector.AddDiscretValue(const AValue: IDiscretValue);
begin
    AddNumberOfValues(AValue.NumberOfValues);
    FValuesList[Length(FValuesList) - 1] := AValue;
end;

procedure TCombSelector.ClearDiscretValuesList;
begin
    ClearListOfNumbersOfValues;
end;

procedure TCombSelector.AddNumberOfValues(const ANumberOfValues: LongInt);
begin
    inherited AddNumberOfValues(ANumberOFValues);
    SetLength(FValuesList, Length(FValuesList) + 1);
    FValuesList[Length(FValuesList) - 1] := nil;
end;

procedure TCombSelector.ClearListOfNumbersOfValues;
var i: LongInt;
begin
    inherited ClearListOfNumbersOfValues;
    for i := 0 to Length(FValuesList) - 1 do
        FValuesList[i] := nil;                                                                      
    Finalize(FValuesList);
end;

procedure TCombSelector.SetCurrentComb(const ACurrentComb: LongInt);
var i: LongInt;
begin
    inherited SetCurrentComb(ACurrentComb);
    for i := 0 to ValuesNumber - 1 do
        if Assigned(FValuesList[i]) then
            FValuesList[i].ValueIndex := ValueIndex[i];
end;

end.
