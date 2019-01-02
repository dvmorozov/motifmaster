{------------------------------------------------------------------------------
    This file is part of the MotifMASTER project. This software is
    distributed under GPL (see gpl.txt for details).

    This software is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Copyright (C) 1999-2007 D.Morozov (dvmorozov@mail.ru)
------------------------------------------------------------------------------}

unit FFDataFile;

interface

uses Classes,  Dialogs,  SysUtils, ComponentList, DataClasses, Tools;


type
  TCatalogElement = class
    NameOfElement: string[10];
    FullName: string[15];
    NumberOfPoints: SmallInt;
    StepOfPoints: Double;
    StartPos: LongInt;
    EndPos: LongInt;
    Reserved1: LongInt;
    Reserved2: Double;
    Comments: string[50];
    ViewItem: TObject;
  end;

const 
  IM_NONE = 0;
  IM_LINEAR = 1;

var
  Catalog: TList;
  FFDF: TFileStream;
  CatalogPos: LongInt;
  FileIsOpened: Boolean;
  FN: string;
  FFArrayList: TSelfCleanList;
  InterpolationMode: LongInt = IM_NONE;

procedure CreateFFDataFile(FileName: string);
procedure OpenFFDataFile(FileName: string);
procedure CloseFFDataFile;
procedure ReadCatalog;
procedure SaveCatalog;
procedure DeleteRecord(var CE: TCatalogElement);
procedure ReadArray(const CE: TCatalogElement; FFAR: TFFArrRec);
function GetFFArrays(ASiteList: TSiteList): Boolean;
function GetFFValue(SinTL: Double; SR: TAtom): Double;
function SearchElement(St: string): TCatalogElement;

implementation

procedure CreateFFDataFile(FileName: string);
var TempLong: LongInt;
begin
  CloseFFDataFile;
  Catalog := TList.Create;
  try
    FFDF := TFileStream.Create(FileName, fmCreate or fmShareDenyNone);
  except
    MessageDlg('Can''t create form - factors file...', mtError, [mbOk], 0);
    FileIsOpened := False;
    Exit;
  end;
  TempLong := SizeOf(TempLong);
  CatalogPos := TempLong;
  FFDF.Seek(0, soFromBeginning);
  FFDF.Write(TempLong, SizeOf(TempLong));
  FileIsOpened := True;
  FN := FileName;
end;

procedure OpenFFDataFile(FileName: string);
begin
  CloseFFDataFile;
  Catalog := TList.Create;
  try
    FFDF := TFileStream.Create(FileName, fmOpenReadWrite or fmShareDenyNone);
  except
    MessageDlg('Can''t open form - factors file...', mtError, [mbOk], 0);
    FileIsOpened := False;
    Exit;
  end;
  FFDF.Seek(0, soFromBeginning);
  FFDF.Read(CatalogPos, SizeOf(CatalogPos));
  ReadCatalog;
  FileIsOpened := True;
  FN := FileName;
end;

procedure ReadCatalog;
var CE: TCatalogElement;
begin
  FFDF.Seek(CatalogPos, soFromBeginning);
  while FFDF.Position <> FFDF.Size do begin
    CE := TCatalogElement.Create;
    with CE do begin
      FFDF.Read(NameOfElement, SizeOf(NameOfElement));
      FFDF.Read(FullName, SizeOf(FullName));
      FFDF.Read(NumberOfPoints, SizeOf(NumberOfPoints));
      FFDF.Read(StepOfPoints, SizeOf(StepOfPoints));
      FFDF.Read(StartPos, SizeOf(StartPos));
      FFDF.Read(EndPos, SizeOf(EndPos));
      FFDF.Read(Reserved1, SizeOf(Reserved1));
      FFDF.Read(Reserved2, SizeOf(Reserved2));
      FFDF.Read(Comments, SizeOf(Comments));
    end;
    Catalog.Add(CE);
  end;
end;

procedure CloseFFDataFile;
begin
  if FileIsOpened then begin
    UtilizeObject(FFDF);
    FFDF := nil;
    UtilizeObject(Catalog);
    Catalog := nil;
    FileIsOpened := False;
  end;
  FN := '';
end;

procedure SaveCatalog;
var i: LongInt;
    CE: TCatalogElement;
begin
  CatalogPos := FFDF.Position;
  for i := 0 to Catalog.Count - 1 do begin
    CE := Catalog.Items[i];
    with CE do begin
      FFDF.Write(NameOfElement, SizeOf(NameOfElement));
      FFDF.Write(FullName, SizeOf(FullName));
      FFDF.Write(NumberOfPoints, SizeOf(NumberOfPoints));
      FFDF.Write(StepOfPoints, SizeOf(StepOfPoints));
      FFDF.Write(StartPos, SizeOf(StartPos));
      FFDF.Write(EndPos, SizeOf(EndPos));
      FFDF.Write(Reserved1, SizeOf(Reserved1));
      FFDF.Write(Reserved2, SizeOf(Reserved2));
      FFDF.Write(Comments, SizeOf(Comments));
    end;
  end;
  FFDF.Seek(0, soFromBeginning);
  FFDF.Write(CatalogPos, SizeOf(CatalogPos));
end;

procedure DeleteRecord(var CE: TCatalogElement);
var TempFile: TFileStream;
    RequiredSpace: LongInt;
    CatalogPos: LongInt;
    ElementSize: LongInt;
    i: LongInt;
    CE2: TCatalogElement;
begin
  FFDF.Seek(0, soFromBeginning);
  FFDF.Read(CatalogPos, SizeOf(CatalogPos));
  ElementSize := CE.NumberOfPoints * SizeOf(Double);
  RequiredSpace := FFDF.Size - ElementSize - (FFDF.Size - CatalogPos);
  if DiskFree(0) < RequiredSpace then begin
    MessageDlg('Not enought disk space...', mtError, [mbOk], 0);
    Exit;
  end;
  TempFile := TFileStream.Create(ExtractFilePath(FN)+'FFEditor.tmp', fmCreate);
  TempFile.Seek(0, soFromBeginning);
  FFDF.Seek(0, soFromBeginning);
  TempFile.CopyFrom(FFDF, CE.StartPos);
  FFDF.Seek(CE.StartPos + ElementSize, soFromBeginning);
  if CatalogPos - FFDF.Position > 0 then
    TempFile.CopyFrom(FFDF, CatalogPos - FFDF.Position);
  for i := 0 to Catalog.Count - 1 do begin
    CE2 := Catalog.Items[i];
    if CE2 <> CE then
      if CE2.StartPos >= CE.StartPos + ElementSize then
        CE2.StartPos := CE2.StartPos - ElementSize;
  end;
  Catalog.Remove(CE);
  UtilizeObject(CE);
  UtilizeObject(FFDF);
  UtilizeObject(TempFile);
  DeleteFile(FN);
  RenameFile(ExtractFilePath(FN)+'FFEditor.tmp', FN);
  FFDF := TFileStream.Create(FN, fmOpenReadWrite or fmShareDenyNone);
  FFDF.Seek(FFDF.Size, soFromBeginning);
  SaveCatalog;
end;

function SearchElement(St: string): TCatalogElement;
var i: LongInt;
   CE: TCatalogElement;
begin
  for i := 0 to Catalog.Count - 1 do begin
    CE := Catalog.Items[i];
    if UpperCase(CE.NameOfElement) = UpperCase(St) then begin
      SearchElement := CE;
      Exit;
    end;
  end;
  SearchElement := nil;
end;

function GetFFArrays(ASiteList: TSiteList): Boolean;

  function HaveThisElement(const SR: TAtom): Boolean;
  var i: LongInt;
      FFAR: TFFArrRec;
  begin
    for i := 0 to FFArrayList.Count - 1 do
    begin
      FFAR := FFArrayList.Items[i];
      if FFAR.NameOfElement = SR.Element then
      begin
        HaveThisElement := True;
        Exit;
      end;
    end;
    HaveThisElement := False;
  end;

var i, j: LongInt;
    TS: TSite;
    SR: TAtom;
    CE: TCatalogElement;
    FFAR: TFFArrRec;
    MagnStar: TWaveVector;
begin
  FFArrayList.ClearAll;
  if not FileIsOpened then begin GetFFArrays := False; Exit end;
  for i := 0 to ASiteList.Count - 1 do
  begin
    TS := TSite(ASiteList.Items[i]);
    MagnStar := TS.WaveVectorList.MagnStar;
    if (not TS.Disabled) and (not TS.NotMagnetic) and (MagnStar <> nil) then
      for j := 0 to TS.AtomList.Count - 1 do
      begin
        SR := TAtom(TS.AtomList.Items[j]);
        if not HaveThisElement(SR) then
        begin
          CE := SearchElement(SR.Element);
          if CE = nil then
          begin
            if SR.Position <> '' then
            begin
              MessageDlg('Can''t find element '+ SR.Element +
                         ' in form - factors database...', mtError, [mbOk], 0);
              FFArrayList.ClearAll;
              GetFFArrays := False;
              Exit;
            end else Continue;
          end;
          FFAR := TFFArrRec.Create;
          FFAR.NameOfElement := SR.Element;
          ReadArray(CE, FFAR);
          FFArrayList.Add(FFAR);
        end;{if not HaveThisElement(SR) then...}
      end;{for j := 0 to TS.AtomList.Count - 1 do...}
  end;{for i := 0 to ASiteList.Count - 1 do...}
  GetFFArrays := True;
end;

function GetFFValue(SinTL: Double;SR: TAtom): Double;

var TempDelta: Double;
    i, j: LongInt;
    FFAR: TFFArrRec;
begin
  for j := 0 to FFArrayList.Count - 1 do
  begin
    FFAR := FFArrayList.Items[j];
    if UpperCase(SR.Element) = UpperCase(FFAR.NameOfElement) then
    with FFAR do
    begin
      case InterpolationMode of
        IM_NONE : 
            begin
              i := Round((SinTL / (NumberOfPoints * StepOfPoints)) *
              (NumberOfPoints - 1));
              Result := FFArray[i * 2];
              Exit
            end;
        IM_LINEAR : 
            begin
              i := Trunc((SinTL / 0.42) * 21);
              if FFArray[i * 2 + 1] = SinTL then
              begin
                GetFFValue := FFArray[i * 2];
                Exit;
              end;
              TempDelta := FFArray[(i + 1) * 2] - FFArray[i * 2];
              TempDelta := (TempDelta / 0.02) * (SinTL - FFArray[i * 2 + 1]);
              GetFFValue := FFArray[i * 2] + TempDelta;
              Exit;
            end;
      end;{case InterpolationMode of...}
    end;{With FFAR do...}
  end;{for j := 0 to FFArrayList.Count - 1 do...}
  Result := 0;
end;

procedure ReadArray(const CE: TCatalogElement; FFAR: TFFArrRec);
var i: LongInt;
    TempDouble: Double;
    TempDouble2: Double;
begin
  FFDF.Seek(CE.StartPos, soFromBeginning);
  FFAR.NumberOfPoints := CE.NumberOfPoints;
  FFAR.StepOfPoints := CE.StepOfPoints;
  for i := 0 to CE.NumberOfPoints - 1 do
  begin
    TempDouble := CE.StepOfPoints * i;
    FFDF.Read(TempDouble2, SizeOf(Double));
    FFAR.FFArray[2 * i] := TempDouble2;
    FFAR.FFArray[2 * i + 1] := TempDouble;
  end;
end;

initialization
  FileIsOpened := False;
  FFArrayList := TSelfCleanList.Create;
finalization
  FFArrayList.ClearAll;
  UtilizeObject(FFArrayList);
end.
