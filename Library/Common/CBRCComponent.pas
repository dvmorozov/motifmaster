{------------------------------------------------------------------------------
    This file is part of the MotifMASTER project. This software is
    distributed under GPL (see gpl.txt for details).

    This software is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Copyright (C) 1999-2007 D.Morozov (dvmorozov@mail.ru)
------------------------------------------------------------------------------}

unit CBRCComponent;

interface

uses Classes, SelfSaved, ClassInheritIDs;

type
    TCBRCComponent = class(TSelfSavedComponent)
        {  the component Controlled By References Counter (CBRC) }
    protected
        FRefCount: LongInt;
        IntControlled: Boolean;

        function _AddRef: Integer; virtual; stdcall;
        function _Release: Integer; virtual; stdcall;

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
        procedure Free;
        property RefCount: LongInt read FRefCount;
    end;

implementation

procedure TCBRCComponent.Free;
begin
    if Self <> nil then
    begin
        if RefCount = 0 then inherited
        else IntControlled := True;
    end;
end;

class function TCBRCComponent.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := CBRCClassInheritID;
    Result.PropVersionNum := CBRCCurVerNum;
end;

class procedure TCBRCComponent.ReadProperties(const Reader: TReader;
  const PropHeaderRec: TPropHeaderRec;
  const AnObject: TSelfSavedComponent);
begin

end;

class procedure TCBRCComponent.WriteProperties(const Writer: TWriter;
  const AnObject: TSelfSavedComponent);
begin

end;

function TCBRCComponent._AddRef: Integer;
begin
    Inc(FRefCount);
    Result := RefCount;
end;

function TCBRCComponent._Release: Integer;
begin
    Dec(FRefCount);
    Result := RefCount;
    if IntControlled and (Result = 0) then Free;
end;

initialization
    RegisterClass(TCBRCComponent);
end.


