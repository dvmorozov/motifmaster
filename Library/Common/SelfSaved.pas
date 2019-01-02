{------------------------------------------------------------------------------
    This file is part of the MotifMASTER project. This software is
    distributed under GPL (see gpl.txt for details).

    This software is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Copyright (C) 1999-2007 D.Morozov (dvmorozov@mail.ru)
------------------------------------------------------------------------------}

unit SelfSaved;

interface

uses Classes, ClassInheritIDs;

type
    TPropHeaderRec = record
        ClassInheritID: Byte;
        PropVersionNum: Byte
    end;

    TSelfSavedComponent = class(TComponent)
    private
        procedure ReadData(Reader: TReader);
        class procedure ReadInheritChain(
            const Reader: TReader;
            const AnObject: TSelfSavedComponent
            );
        class procedure ReadPropHeader(const Reader: TReader;
            out PropHeaderRec: TPropHeaderRec
            );

        procedure WriteData(Writer: TWriter);
        class procedure WriteInheritChain(
            const Writer: TWriter;
            const AnObject: TSelfSavedComponent
            ); virtual;
        class procedure WritePropHeader(const Writer: TWriter); virtual;

    protected
        class function IsClassInheritIDValid(
            const ClassInheritID: Byte
            ): Boolean; virtual;
        class function GetPropHeaderRec: TPropHeaderRec; virtual;
        class procedure ReadProperties(
            const Reader: TReader;
            const PropHeaderRec: TPropHeaderRec;
            const AnObject: TSelfSavedComponent
            ); virtual;

        class procedure WriteProperties(
            const Writer: TWriter;
            const AnObject: TSelfSavedComponent
            ); virtual;

    public
        procedure DefineProperties(Filer: TFiler); override;
    end;

    TSelfSavedComponents = class of TSelfSavedComponent;

implementation

{ TSelfSavedComponent }

class function TSelfSavedComponent.IsClassInheritIDValid(
    const ClassInheritID: Byte): Boolean;
begin
    if ClassInheritID = GetPropHeaderRec.ClassInheritID then
        Result := True else Result := False;
end;

procedure TSelfSavedComponent.DefineProperties(Filer: TFiler);
begin
    Filer.DefineProperty(' ', ReadData, WriteData, True)
end;

class function TSelfSavedComponent.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := SelfSavedInheritID;
    Result.PropVersionNum := SelfSavedCurVerNum;
end;

procedure TSelfSavedComponent.ReadData(Reader: TReader);
begin
    with Reader do
    begin
        ReadListBegin;
        ReadInheritChain(Reader, Self);
        ReadListEnd;
    end;
end;

class procedure TSelfSavedComponent.ReadProperties(
    const Reader: TReader;
    const PropHeaderRec: TPropHeaderRec;
    const AnObject: TSelfSavedComponent
    );
begin
end;

class procedure TSelfSavedComponent.ReadPropHeader(const Reader: TReader;
    out PropHeaderRec: TPropHeaderRec);
begin
    with Reader, PropHeaderRec do
    begin
        ClassInheritID := ReadInteger;
        PropVersionNum := ReadInteger;
    end;
end;

procedure TSelfSavedComponent.WriteData(Writer: TWriter);
begin
    with Writer do
    begin
        WriteListBegin;
        WriteInheritChain(Writer, Self);
        WriteListEnd;
    end;
end;

class procedure TSelfSavedComponent.WriteInheritChain(
    const Writer: TWriter;
    const AnObject: TSelfSavedComponent
    );
var CurClassType: TSelfSavedComponents;
begin
    CurClassType := TSelfSavedComponents(AnObject.ClassType);
    repeat
        with CurClassType do
        begin
            WritePropHeader(Writer);
            WriteProperties(Writer, AnObject);
        end;
        CurClassType := TSelfSavedComponents(CurClassType.ClassParent);
    until CurClassType = TSelfSavedComponent.ClassParent;
end;

class procedure TSelfSavedComponent.WriteProperties(
    const Writer: TWriter;
    const AnObject: TSelfSavedComponent
    );
begin
end;

class procedure TSelfSavedComponent.WritePropHeader(const Writer: TWriter);
var PropHeaderRec: TPropHeaderRec;
begin
    PropHeaderRec := GetPropHeaderRec;
    with Writer, PropHeaderRec do
    begin
        WriteInteger(ClassInheritID);
        WriteInteger(PropVersionNum);
    end;
end;

class procedure TSelfSavedComponent.ReadInheritChain(
    const Reader: TReader;
    const AnObject: TSelfSavedComponent
    );
var PropHeaderRec: TPropHeaderRec;
    CurClassType: TSelfSavedComponents;
begin
    while not Reader.EndOfList do
    begin
        ReadPropHeader(Reader, PropHeaderRec);

        CurClassType := TSelfSavedComponents(AnObject.ClassType);
        repeat
            with CurClassType do
                if IsClassInheritIDValid(PropHeaderRec.ClassInheritID) then
                begin
                    ReadProperties(Reader, PropHeaderRec, AnObject);
                    Break;
                end;
            CurClassType := TSelfSavedComponents(CurClassType.ClassParent);
        until CurClassType = TSelfSavedComponent.ClassParent;
    end;
end;

end.
