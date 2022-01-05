unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus,
  PythonMemo, FileHandle;

type

  { TForm1 }

  TForm1 = class(TForm)
    MainMenu: TMainMenu;
    MenuFile: TMenuItem;
    MenuRun: TMenuItem;
    MenuOpen: TMenuItem;
    MenuRevert: TMenuItem;
    MenuSave: TMenuItem;
    MenuSaveAs: TMenuItem;
    OpenDialog: TOpenDialog;
    Memo: TRichMemo;
    SaveDialog: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure MenuFileClick(Sender: TObject);
    procedure MenuOpenClick(Sender: TObject);
    procedure MenuRevertClick(Sender: TObject);
    procedure MenuRunClick(Sender: TObject);
    procedure MenuSaveAsClick(Sender: TObject);
    procedure MenuSaveClick(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

var HomeDirectory : string;


procedure TForm1.MenuOpenClick(Sender: TObject);
begin
  if OpenDialog.Execute then Memo.OpenFrom(OpenDialog.FileName);
end;

procedure TForm1.MenuSaveClick(Sender: TObject);
begin
  Memo.Save();
end;

procedure TForm1.MenuSaveAsClick(Sender: TObject);
begin
  if SaveDialog.Execute then Memo.SaveTo(SaveDialog.FileName);
end;

procedure TForm1.MenuRevertClick(Sender: TObject);
begin
  Memo.Revert();
end;

procedure TForm1.MenuRunClick(Sender: TObject);
begin
  if Memo.HasFile() then
     begin
       Memo.Save();
       StartProcess(WhereeverYouKeepPython + '\python.exe', Memo.CurrentFile)
     end
  else
     begin
       MenuSaveAs.Click();
       if Memo.HasFile() then
          StartProcess(WhereeverYouKeepPython + '\python.exe', Memo.CurrentFile)
     end;
end;

procedure TForm1.MenuFileClick(Sender: TObject);
begin
  MenuSave.Enabled := Memo.HasFile();
  MenuRevert.Enabled := Memo.HasFile();
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
   HomeDirectory  := ExtractFileDir(ParamStr(0));
   OpenDialog.InitialDir := YourDefaultDirectoryForPythonScripts;
   SaveDialog.InitialDir := YourDefaultDirectoryForPythonScripts;
end;

end.

