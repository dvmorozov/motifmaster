{------------------------------------------------------------------------------
    This file is part of the MotifMASTER project. This software is
    distributed under GPL (see gpl.txt for details).

    This software is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Copyright (C) 1999-2007 D.Morozov (dvmorozov@mail.ru)
------------------------------------------------------------------------------}

unit ObjSavingStringList;

interface

uses
    ComponentList, SelfCopied, SysUtils, Classes, SelfSaved, ClassInheritIDs;

type
    TObjSavingStringList = class(TSelfCopiedCompList)
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
        function GetObjectIdentifier(const ObjNum: LongInt): string; virtual;
    end;

implementation

function TObjSavingStringList.GetObjectIdentifier(const ObjNum: LongInt): string;
begin
    Result := 'Object ' + IntToStr(ObjNum);
end;

class function TObjSavingStringList.GetPropHeaderRec: TPropHeaderRec;
begin
    Result.ClassInheritID := OSSLClassInheritID;
    Result.PropVersionNum := OSSLCurVerNum;
end;

class procedure TObjSavingStringList.ReadProperties(const Reader: TReader;
    const PropHeaderRec: TPropHeaderRec;
    const AnObject: TSelfSavedComponent);
begin

end;

class procedure TObjSavingStringList.WriteProperties(const Writer: TWriter;
    const AnObject: TSelfSavedComponent);
begin

end;

initialization
    RegisterClass(TObjSavingStringList);
end.
