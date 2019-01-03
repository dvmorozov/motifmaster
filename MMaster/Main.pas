


{------------------------------------------------------------------------------}
{       Copyright (C) 1999-2007 D.Morozov (dvmorozov@mail.ru)                  }
{------------------------------------------------------------------------------}
unit Main;

{$MODE Delphi}

interface

uses
    LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
    ShellAPI, LResources;

type
    THiddenForm = class(TForm)
        procedure FormCreate(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
        procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);

    public
        NotifyIconData : TNotifyIconDataA;
        procedure SetIconToTaskBar;
        procedure DeleteIconFromTaskBar;
        procedure ProcessIconMessage(var Msg : TMessage); message WM_USER;
            
    end;

var
    HiddenForm: THiddenForm;

implementation

uses Unit1;

{$IFDEF USE_DELPHI}
{$R *.DFM}
{$ENDIF}

procedure THiddenForm.SetIconToTaskBar;
var i: LongInt;
begin
    (*???
    with NotifyIconData do begin
        cbSize := SizeOf(NotifyIconData);
        Wnd := Handle;
        uID := 0;
        uFlags := NIF_ICON or NIF_TIP or NIF_MESSAGE;
        uCallBackMessage := WM_USER;
        hIcon := Icon.Handle;
        for i := 1 to Length(PROGRAM_CAPTION) do
            szTip[i - 1] := PROGRAM_CAPTION[i];
    end;
    Shell_NotifyIcon(NIM_ADD, @NotifyIconData);
    *)
end;

procedure THiddenForm.DeleteIconFromTaskBar;
begin
    (*???
    with NotifyIconData do begin
        cbSize := SizeOf(NotifyIconData);
        Wnd := Handle;
        uID := 0;
        uFlags := NIF_ICON or NIF_TIP or NIF_Message;
        uCallBackMessage := WM_USER;
        hIcon := Icon.Handle;
        szTip := '';
    end;
    Shell_NotifyIcon(NIM_DELETE, @NotifyIconData);
    *)
end;

procedure THiddenForm.ProcessIconMessage(var Msg : TMessage);
begin
    (*???
    if Msg.lParam = wm_LButtonDblClk then begin
        if Form1.WindowState = wsMinimized then Form1.WindowState := wsNormal;
        Form1.Show;
    end;
    inherited;
    *)
end;

procedure THiddenForm.FormCreate(Sender: TObject);
begin
    SetIconToTaskBar;
end;

procedure THiddenForm.FormDestroy(Sender: TObject);
begin
    DeleteIconFromTaskBar;
end;

procedure THiddenForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
    Form1.FormCloseQuery(Sender, CanClose);
end;

initialization
{$IFNDEF USE_DELPHI}
{$I Main.lrs}
{$ENDIF}
end.
 
