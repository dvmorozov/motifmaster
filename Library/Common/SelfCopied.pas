{------------------------------------------------------------------------------
    This file is part of the MotifMASTER project. This software is
    distributed under GPL (see gpl.txt for details).

    This software is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Copyright (C) 1999-2007 D.Morozov (dvmorozov@mail.ru)
------------------------------------------------------------------------------}

unit SelfCopied;

interface

uses
    Classes, ComponentList, SysUtils, CBRCComponent, SelfSaved, ClassInheritIDs;

type
    ISelfCopied = interface
        ['{DF1ABB41-F255-11D4-968F-C7AD39AA7469}']
        function GetCopy: TObject;
        procedure CopyParameters(const Dest: TObject);

        procedure SetSelfCopyingMode(const AMode: LongInt);
        function GetSelfCopyingMode: LongInt;
    end;

const SelfCopiedGUID: TGUID = '{DF1ABB41-F255-11D4-968F-C7AD39AA7469}';

type
    TSelfCopiedComponent = class(TCBRCComponent, ISelfCopied)
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
        function GetCopy: TObject; virtual;
        procedure CopyParameters(const Dest: TObject); virtual;

        procedure SetSelfCopyingMode(const AMode: LongInt); virtual; abstract;
        function GetSelfCopyingMode: LongInt; virtual; abstract;
    end;

    ESelfCopiedCompList = class(Exception);

    TSelfCopiedCompList = class(TComponentList, ISelfCopied)
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
        function GetCopy: TObject; virtual;
        function GetSharedCopy: TObject; virtual;
        procedure CopyParameters(const Dest: TObject); virtual;

        procedure SetSelfCopyingMode(const AMode: LongInt); virtual; abstract;
        function GetSelfCopyingMode: LongInt; virtual; abstract;

        procedure Insert(Index: Integer; Item: TComponent); override;
        function Add(Item: TComponent): Integer; override;
    end;

function CreateNewSelfCopiedCompList: TSelfCopiedCompList;

implementation

type
    TSCCL = class(TSelfCopiedCompList)
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

function CreateNewSelfCopiedCompList: TSelfCopiedCompList;
begin
    Result := TSCCL.Create(nil);
end;

function TSelfCopiedCompList.GetCopy: TObject;
begin
    Result := NewInstance;
    TSelfCopiedCompList(Result).Create(nil);
    CopyParameters(Result);
end;

function TSelfCopiedCompList.GetSharedCopy: TObject;
var i: LongInt;
begin
    Result := NewInstance;
    TSelfCopiedCompList(Result).Create(nil);
    for i := 0 to Count - 1 do
        TSelfCopiedCompList(Result).Add(TComponent(Items[i]));
end;

procedure TSelfCopiedCompList.CopyParameters(const Dest: TObject);
var i: LongInt;
    ISC: ISelfCopied;
begin
    if Dest.ClassType <> Self.ClassType then
        raise ESelfCopiedCompList.Create('Invalid destination type...');

    if Count <> 0 then
        if Count <> TSelfCopiedCompList(Dest).Count then
        begin
            TSelfCopiedCompList(Dest).Clear;
            for i := 0 to Count - 1 do
            begin
                if Items[i].GetInterface(SelfCopiedGUID, ISC) then
                    TSelfCopiedCompList(Dest).Add(TComponent(ISC.GetCopy))
                else raise ESelfCopiedCompList.Create('Invalid item type...');
            end;
        end else
        begin
            for i := 0 to Count - 1 do
            begin
                if Items[i].GetInterface(SelfCopiedGUID, ISC) then
                    ISC.CopyParameters(TSelfCopiedCompList(Dest).Items[i])
                else raise ESelfCopiedCompList.Create('Invalid item type...');
            end;
        end;
end;

procedure TSelfCopiedCompList.Insert(Index: Integer; Item: TComponent);
var ISC: ISelfCopied;
begin
    if Item.GetInterface(SelfCopiedGUID, ISC) then inherited
    else raise ESelfCopiedCompList.Create('Invalid item type...');
end;

function TSelfCopiedCompList.Add(Item: TComponent): Integer;
var ISC: ISelfCopied;
begin
    if Item.GetInterface(SelfCopiedGUID, ISC) then Result := inherited Add(Item)
    else raise ESelfCopiedCompList.Create('Invalid item type...');
end;

function TSelfCopiedComponent.GetCopy: TObject;
begin
    Result := NewInstance;
    TSelfCopiedComponent(Result).Create(nil);
    CopyParameters(Result);
end;

procedure TSelfCopiedComponent.CopyParameters(const Dest: TObject);
begin
    if Dest.ClassType <> Self.ClassType then
        raise ESelfCopiedCompList.Create('Invalid destination type...');
end;

class function TSelfCopiedComponent.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := SCCClassInheritID;
    Result.PropVersionNum := SCCCurVerNum;
end;

class procedure TSelfCopiedComponent.ReadProperties(const Reader: TReader;
    const PropHeaderRec: TPropHeaderRec;
    const AnObject: TSelfSavedComponent);
begin

end;

class procedure TSelfCopiedComponent.WriteProperties(const Writer: TWriter;
    const AnObject: TSelfSavedComponent);
begin

end;

class function TSelfCopiedCompList.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := SCCLClassInheritID;
    Result.PropVersionNum := SCCLCurVerNum;
end;

class procedure TSelfCopiedCompList.ReadProperties(const Reader: TReader;
    const PropHeaderRec: TPropHeaderRec;
    const AnObject: TSelfSavedComponent);
begin

end;

class procedure TSelfCopiedCompList.WriteProperties(const Writer: TWriter;
    const AnObject: TSelfSavedComponent);
begin

end;

{ TSCCL }

class function TSCCL.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := SCCLAlClassInheritID;
    Result.PropVersionNum := SCCLAlCurVerNum;
end;

class procedure TSCCL.ReadProperties(const Reader: TReader;
    const PropHeaderRec: TPropHeaderRec;
    const AnObject: TSelfSavedComponent);
begin

end;

class procedure TSCCL.WriteProperties(const Writer: TWriter;
    const AnObject: TSelfSavedComponent);
begin

end;

initialization
    RegisterClass(TSelfCopiedComponent);
    RegisterClass(TSelfCopiedCompList);

    RegisterClass(TSCCL);
end.
