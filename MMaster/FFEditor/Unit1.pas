{------------------------------------------------------------------------------
    This file is part of the MotifMASTER project. This software is
    distributed under GPL (see gpl.txt for details).

    This software is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Copyright (C) 1999-2007 D.Morozov (dvmorozov@mail.ru)
------------------------------------------------------------------------------}

unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Menus, Grids, NumericGrid, ExtCtrls, ComCtrls,
  FFDataFile, Tools, SyntMessages;

type
  TForm1 = class(TForm)
    ContentsViewer: TListView;
    Splitter1: TSplitter;
    FFArrayEditor: TNumericGrid;
    Splitter2: TSplitter;
    AdditionalParameters: TPanel;
    AddBut: TButton;
    DeleteBut: TButton;
    ExitBut: TButton;
    SaveBut: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ExitButClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure AddButClick(Sender: TObject);
    procedure SaveButClick(Sender: TObject);
    procedure ContentsViewerClick(Sender: TObject);
    procedure FFArrayEditorKeyPress(Sender: TObject; var Key: Char);
    procedure ContentsViewerEdited(Sender: TObject; Item: TListItem;
      var S: String);
    procedure DeleteButClick(Sender: TObject);
    procedure ContentsViewerKeyPress(Sender: TObject; var Key: Char);
  public
   FFArrRows: LongInt;
   FFStep: Double;
   ArraySaved: Boolean;
   SizeChanged: Boolean;
   EditingElement: TCatalogElement;
   Procedure LoadFFData;
   Procedure SaveFFData;
   Procedure EnumerateFFArrayEditor(FFRows: LongInt;Step: Double);
   Procedure ClearAllInFF;
   Procedure ReadArray(var CE: TCatalogElement);
   Procedure WriteArray(var CE: TCatalogElement);
   Function  GetEditingElement(var LI: TListItem): TCatalogElement;
   Function  GetSelectedElement: TListItem;
   Procedure MakeFirstSelected;
   Procedure DeleteElement(var CE: TCatalogElement);
  end;

var
  Form1: TForm1;

implementation

uses Unit2;

{$R *.DFM}

Const crExit = 500;
      crSave = 501;
      crDelete = 503;

procedure TForm1.FormCreate(Sender: TObject);
var Cur: HCursor;
begin
 LoadFFData;
 Form1.ActiveControl := ContentsViewer;
end;

procedure TForm1.LoadFFData;
Var i: LongInt;
    LI: TListItem;
    CE: TCatalogElement;
    St: String;
begin
 ArraySaved := True;
 St := GetCmdLineParameters;
 if St = '' then
  begin Close;Exit end
 else
  begin
   if Not FileExists(St) then CreateFFDataFile(St) else OpenFFDataFile(St);
   if Not FileIsOpened then Close;
   With ContentsViewer do
    begin
     Items.Clear;
     for i := 0 to Catalog.Count - 1 do
      begin
       CE := Catalog.Items[i];
       LI := Items.Add;
       LI.Caption := CE.NameOfElement;
       CE.ViewItem := LI;
      end;{for i := 0 to Catalog.Count - 1 do...}
    end;{With ContentsViwer do...}
  end;
 MakeFirstSelected;
end;

Procedure TForm1.MakeFirstSelected;
Var LI: TListItem;
begin
 if ContentsViewer.Items.Count <> 0 then
  begin
   LI := ContentsViewer.Items.Item[0];
   LI.Selected := True;
   EditingElement := GetEditingElement(LI);
   ReadArray(EditingElement);
  end;
end;

procedure TForm1.SaveFFData;
begin
 WriteArray(EditingElement);
end;

procedure TForm1.EnumerateFFArrayEditor(FFRows: LongInt;Step: Double);
Var i: LongInt;
    St : String;
begin
 With Form1.FFArrayEditor do
  begin
   RowCount := FFRows + 1;
   ClearAllInFF;
   Cells[0,0] := 'SinT/L';
   Cells[1,0] := 'FF Value';
   for i := 0 to FFRows - 1 do
    begin
     Str(i * Step:5:3,St);
     Cells[0,i + 1] := St;
    end;
  end;{With FFArrayEditor do...}
end;

Procedure TForm1.ClearAllInFF;
Var i,j: LongInt;
begin
 With Form1.FFArrayEditor do
  begin
   for i := 0 to ColCount - 1 do
    for j := 0 to RowCount - 1 do
     Cells[i,j] := '';
  end;{With FFArrayEditor do...}
end;

procedure TForm1.ExitButClick(Sender: TObject);
begin
 Close;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 if Not ArraySaved then
  if MessageDlg('Save changes in ' + EditingElement.NameOfElement + ' ?',mtConfirmation,[mbYes,mbNo,mbCancel],0) = mrYes
   then SaveFFData;
 CloseFFDataFile;
 PostMessage(HWND_BROADCAST,WM_FFEditor,0,0);
end;

procedure TForm1.AddButClick(Sender: TObject);
Var Result: Integer;
    CE: TCatalogElement;
    Code: Integer;
    NOP: Integer;
    S: Double;
    LI: TListItem;
    Valid: Boolean;
begin
 if Not ArraySaved then
  if MessageDlg('Save changes in '+EditingElement.NameOfElement+' ?',mtWarning,[mbYes,mbNo,mbCancel],0) = mrYes
   then SaveFFData;
 With EnterNewData do
  begin
   ElSymbol.Text := '';
   ElFullName.Text := '';
   ActiveControl := ElSymbol;
  end;
 Repeat
  Result := EnterNewData.ShowModal;
  if Result <> mrOk then Exit;
  Valid := True;
  With EnterNewData do
   begin
    if ElSymbol.Text = '' then
     begin
      MessageDlg('This field must not be empty...',mtError,[mbOk],0);
      ActiveControl := ElSymbol;
      Valid := False;
      Continue;
     end;
    if ElFullName.Text = '' then
     begin
      MessageDlg('This field must not be empty...',mtError,[mbOk],0);
      ActiveControl := ElFullName;
      Valid := False;
      Continue;
     end;
    Val(ElNumOfPoints.Text,NOP,Code);
    if Code <> 0 then
     begin
      MessageDlg('Invalid numeric value has been entered...',mtError,[mbOk],0);
      ActiveControl := ElNumOfPoints;
      Valid := False;
      Continue;
     end;
    Val(ElStep.Text,S,Code);
    if Code <> 0 then
     begin
      MessageDlg('Invalid numeric value has been entered...',mtError,[mbOk],0);
      ActiveControl := ElStep;
      Valid := False;
      Continue;
     end;
   end;{With EnterNewData do...}
 Until Valid;
 CE := TCatalogElement.Create;
 With CE,EnterNewData do
  begin
   NameOfElement := ElSymbol.Text;
   FullName := ElFullName.Text;
   NumberOfPoints := NOP;
   StepOfPoints := S;
   Comments := '';
   Reserved1 := 0;
   Reserved2 := 0;
   EndPos := 0;
   StartPos := CatalogPos;
  end;
 Catalog.Add(CE);
 FFArrRows := NOP;
 FFStep := S;
 EnumerateFFArrayEditor(FFArrRows,FFStep);
 LI := ContentsViewer.Items.Add;
 LI.Caption := CE.NameOfElement;
 ArraySaved := False;
 EditingElement := CE;
 CE.ViewItem := LI;
 LI.Selected := True;
 WriteArray(CE);
 SaveCatalog;
 Form1.ActiveControl := ContentsViewer;
end;

procedure TForm1.SaveButClick(Sender: TObject);
begin
 if Not ArraySaved then SaveFFData;
 Form1.ActiveControl := ContentsViewer;
end;

procedure TForm1.ReadArray(var CE: TCatalogElement);
Var i: Integer;
    TempDouble: Double;
    St: String;
begin
 FFArrayEditor.RowCount := CE.NumberOfPoints + 1;
 ClearAllInFF;
 FFArrRows := CE.NumberOfPoints;
 FFStep := CE.StepOfPoints;
 EnumerateFFArrayEditor(FFArrRows,FFStep);
 try
  FFDF.Seek(CE.StartPos,soFromBeginning);
  for i := 1 to CE.NumberOfPoints do
   begin
    FFDF.Read(TempDouble,SizeOf(TempDouble));
    Str(TempDouble:5:3,St);
    FFArrayEditor.Cells[1,i] := St;
   end;{for i := 1 to CE.NumberOfPoints do...}
  except
   MessageDlg('Error reading form - factor file...',mtError,[mbOk],0);
  end;
 EditingElement := CE;
 ArraySaved := True;
end;

procedure TForm1.WriteArray(var CE: TCatalogElement);
Var i: Integer;
    TempDouble: Double;
    St: String;
    Code: Integer;
begin
 try
  FFDF.Seek(CE.StartPos,soFromBeginning);
  for i := 1 to CE.NumberOfPoints do
   begin
    St := FFArrayEditor.Cells[1,i];
    Val(St,TempDouble,Code);
    FFDF.Write(TempDouble,SizeOf(TempDouble));
   end;{for i := 1 to CE.NumberOfPoints do...}
  except
   MessageDlg('Error writing form - factor file...',mtError,[mbOk],0);
  end;
 ArraySaved := True;
end;

procedure TForm1.ContentsViewerClick(Sender: TObject);
Var LI: TListItem;
begin
 if Not ArraySaved then
  if MessageDlg('Save changes in '+EditingElement.NameOfElement+' ?',mtWarning,[mbYes,mbNo,mbCancel],0) = mrYes
   then SaveFFData;
 LI := GetSelectedElement;
 if LI <> NIL then
  begin
   EditingElement := GetEditingElement(LI);
   ReadArray(EditingElement);
  end;
end;

procedure TForm1.FFArrayEditorKeyPress(Sender: TObject; var Key: Char);
begin
 if Key <> Chr(9) then ArraySaved := False;
end;

procedure TForm1.ContentsViewerEdited(Sender: TObject; Item: TListItem;
  var S: String);
Var i: LongInt;
    CE: TCatalogElement;
begin
 CE := GetEditingElement(Item);
 CE.NameOfElement := S;
 FFDF.Seek(CatalogPos,soFromBeginning);
end;

Function TForm1.GetEditingElement(var LI: TListItem): TCatalogElement;
Var j: LongInt;
    CE: TCatalogElement;
begin
 for j := 0 to Catalog.Count - 1 do
  begin
   CE := Catalog.Items[j];
   if CE.ViewItem = LI then
    begin
     GetEditingElement := CE;
     Exit;
    end;
  end;
 GetEditingElement := NIL;
end;

Function TForm1.GetSelectedElement: TListItem;
Var i: LongInt;
    LI: TListItem;
begin
 for i := 0 to ContentsViewer.Items.Count - 1 do
  begin
   LI := ContentsViewer.Items.Item[i];
   if LI.Selected then
    begin
     GetSelectedElement := LI;
     Exit;
    end;
  end;
 GetSelectedElement := NIL;
end;

procedure TForm1.DeleteButClick(Sender: TObject);
begin
 if MessageDlg('Do you want delete element '+EditingElement.NameOfElement+' ?',mtWarning,[mbYes,mbNo,mbCancel],0) = mrYes then
  begin
   DeleteElement(EditingElement);
   MakeFirstSelected;
  end;{if MessageDlg() = mrYes then...}
 Form1.ActiveControl := ContentsViewer;  
end;

procedure TForm1.DeleteElement(var CE: TCatalogElement);
var LI: TListItem;
begin
 LI := TListItem(CE.ViewItem);
 ContentsViewer.Items.Delete(ContentsViewer.Items.IndexOf(LI));
 DeleteRecord(CE);
end;

procedure TForm1.ContentsViewerKeyPress(Sender: TObject; var Key: Char);
Var LI: TListItem;
begin
 if Key = #13 then
  begin
   if Not FileIsOpened then Exit;
   if Not ArraySaved then
    if MessageDlg('Save changes in '+EditingElement.NameOfElement+' ?',mtWarning,[mbYes,mbNo,mbCancel],0) = mrYes
     then SaveFFData;
   LI := GetSelectedElement;
   if LI <> NIL then
    begin
     EditingElement := GetEditingElement(LI);
     ReadArray(EditingElement);
    end; 
  end; 
end;

end.
