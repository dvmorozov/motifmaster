{------------------------------------------------------------------------------
    This file is part of the MotifMASTER project. This software is
    distributed under GPL (see gpl.txt for details).

    This software is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Copyright (C) 1999-2007 D.Morozov (dvmorozov@mail.ru)
------------------------------------------------------------------------------}

unit ComponentList;

interface

uses
    Windows,  Messages,  SysUtils,  Classes,  Graphics,  Controls,  Forms,
    Dialogs,  StdCtrls, CBRCComponent, Tools, SelfSaved, ClassInheritIDs;

type
    ISelfChecked = interface
        ['{E7E7008A-EE1C-4828-B1D6-A53806820A66}']
        procedure IsReady;
        function MyNameIs: string;

        procedure SetSelfCheckingMode(const AMode: LongInt);
        function GetSelfCheckingMode: LongInt;
    end;

const SelfCheckedGUID: TGUID = '{E7E7008A-EE1C-4828-B1D6-A53806820A66}';

type
    EComponentList = class(Exception);

    TComponentList = class(TCBRCComponent, ISelfChecked)
    protected
        List: TList;
        State: LongInt;

        function GetCount: Integer;
        function GetItem(index: Integer): TComponent;
        function GetCapacity: Integer;

        procedure SetItem(index: Integer; Item: TComponent);
        procedure SetCapacity(ACapacity: Integer);
        procedure LinkItemWithList(const Item: TComponent); virtual;

        function GetSelfCheckingMode: LongInt; virtual; abstract;
        procedure SetSelfCheckingMode(const AMode: LongInt); virtual; abstract;

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

        procedure IsReady; virtual;
        function MyNameIs: string; virtual;

        procedure SetCheckingModeInItems(const AMode: LongInt);
        procedure SendErrorMessage(
            const ExceptionMsg, ObjectName: string;
            const ItemNumber: LongInt); virtual;

        procedure Sort(Compare: TListSortCompare);
        procedure Pack;
        function GetState: LongInt;
        procedure SetState(AState: LongInt);
        procedure ActionAfterReading; virtual;
        procedure LinkAllItemsWithList;

        procedure Clear;
        procedure ClearAll;
        function Add(Item: TComponent): Integer; virtual;
        procedure Delete(Index: Integer); virtual;
        procedure Insert(Index: Integer; Item: TComponent); virtual;
        function Remove(Item: Pointer): Integer;
        function IndexOf(Item: Pointer): Integer;

        property Items[index: Integer]: TComponent read GetItem write SetItem;
        property Capacity: Integer read GetCapacity write SetCapacity;
        property Count: Integer read GetCount;
    end;

const
    cfActive: LongInt = 1;
    cfPassive: LongInt = 2;

type
    TSelfCleanList = class(TList)
    public
        procedure ClearAll; virtual;
    end;

procedure Register;

implementation

procedure Register;
begin
    RegisterComponents('Common',  [TComponentList]);
end;

constructor TComponentList.Create;
begin
    inherited Create(AOwner);
    List := TList.Create;
    SetState(cfActive);
end;

destructor TComponentList.Destroy;
begin
    Clear;
    UtilizeObject(List);
    inherited Destroy;
end;

procedure TComponentList.Clear;
begin
    if State = cfActive then ClearAll;
    List.Clear;
end;

function TComponentList.GetCount;
begin
    GetCount := List.Count;
end;

function TComponentList.GetItem;
begin
    Result := List.Items[Index];
end;

procedure TComponentList.SetItem;
begin
    List.Items[Index] := Item;
end;

function TComponentList.GetCapacity: Integer;
begin
    GetCapacity := List.Capacity
end;

procedure TComponentList.SetCapacity(ACapacity: Integer);
begin
    List.Capacity := ACapacity;
end;

procedure TComponentList.ActionAfterReading;
begin
    LinkAllItemsWithList;
end;

procedure TComponentList.LinkAllItemsWithList;
var i: LongInt;
    TC: TComponent;
begin
    for i := 0 to Count - 1 do
    begin
        TC := Items[i];
        LinkItemWithList(TC);
    end;
end;

function TComponentList.Add;
begin
    Add := List.Add(Item);
    LinkItemWithList(Item);
end;

procedure TComponentList.Sort(Compare: TListSortCompare);
begin
    List.Sort(Compare);
end;

procedure TComponentList.Delete(Index: Integer);
var TC: TComponent;
begin
    if State = cfActive then
    begin
        TC := Items[Index];
        UtilizeObject(TC);
    end;
    List.Delete(Index);
end;

function TComponentList.Remove(Item: Pointer): Integer;
begin
    Result := IndexOf(Item);
    Delete(Result);
end;

procedure TComponentList.ClearAll;
begin
    while Count <> 0 do Delete(0);
end;

function TComponentList.IndexOf(Item: Pointer): Integer;
begin
    Result := List.IndexOf(Item);
end;

procedure TComponentList.SetState;
begin
    State := AState;
end;

function TComponentList.GetState: LongInt;
begin
    Result := State;
end;

procedure TComponentList.Insert(Index: Integer; Item: TComponent);
begin
    List.Insert(Index, Item);
    LinkItemWithList(Item);
end;

procedure TComponentList.Pack;
begin
    List.Pack;
end;

procedure TSelfCleanList.ClearAll;
var i: LongInt;
    Item: Pointer;
begin
    for i := 0 to Count - 1 do
    begin
        Item := Items[i];
        if Assigned(Item) then
            with TObject(Item) do try
                UtilizeObject(TObject(Item));
                Items[i] := nil;
            except Items[i] := nil end;
    end;
    Clear;
end;

procedure TComponentList.LinkItemWithList(const Item: TComponent);
begin
end;

procedure TComponentList.IsReady;
var i: LongInt;
    ISC: ISelfChecked;
begin
    try
        for i := 0 to Count - 1 do
            if Items[i].GetInterface(SelfCheckedGUID, ISC) then ISC.IsReady;
    except
        on E: Exception do SendErrorMessage(E.Message, ISC.MyNameIs, i);
    end;
end;

procedure TComponentList.SetCheckingModeInItems(const AMode: LongInt);
var i: LongInt;
    ISC: ISelfChecked;
begin
    for i := 0 to Count - 1 do
        if Items[i].GetInterface(SelfCheckedGUID, ISC) then
            ISC.SetSelfCheckingMode(AMode);
end;

procedure TComponentList.SendErrorMessage(
    const ExceptionMsg, ObjectName: string;
    const ItemNumber: LongInt);
var Str: string;
begin
    Str := ExceptionMsg + ' in ' + ObjectName;
    if MyNameIs <> '' then Str := Str + ' in ' + MyNameIs;
    raise EComponentList.Create(Str);
end;

function TComponentList.MyNameIs: string;
begin
    Result := '';
end;

class function TComponentList.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := CLClassInheritID;
    Result.PropVersionNum := CLCurVerNum;
end;

class procedure TComponentList.ReadProperties(const Reader: TReader;
    const PropHeaderRec: TPropHeaderRec;
    const AnObject: TSelfSavedComponent);
var Comp: TComponent;
    SaveReaderOwner: TComponent;
begin
    with AnObject as TComponentList do
    begin
        Reader.ReadListBegin;
        Clear;
        SaveReaderOwner := Reader.Owner;
        Reader.Owner := nil;
        while not Reader.EndOfList do
        begin
            Comp := Reader.ReadComponent(nil);
            Add(Comp);
        end;
        Reader.ReadListEnd;
        Reader.Owner := SaveReaderOwner;
        ActionAfterReading;
    end;
end;

class procedure TComponentList.WriteProperties(const Writer: TWriter;
    const AnObject: TSelfSavedComponent);
var i: Integer;
    TempComp: TComponent;
begin
    with AnObject as TComponentList do
    begin
        Writer.WriteListBegin;
        for i := 0 to Count - 1 do
        begin
            TempComp := Items[i];
            Writer.WriteComponent(TempComp);
        end;
        Writer.WriteListEnd;
    end;
end;

initialization
    RegisterClass(TComponentList);
end.
