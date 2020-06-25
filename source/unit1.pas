unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls,
  EditBtn, ExtCtrls, ActnList, Grids, ValEdit, FileUtil, LazFileUtils, IniFiles, Unit2;

type

  { State }

  State = class
    public
      StateName: string;            // StateName,               i.e. 1bob, ..
      LocalPath: string;            // StateName.zip            i.e. 1bob.zip, ..
      AbsolutePath: string;         // C:\..\StateName.zip      i.e. C:\Users\piano\Desktop\SM64Practice\16star\1bob.zip, ..

      constructor Create; overload;
      constructor Create(absPath: string); overload;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    DestinationComboBox: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    SaveINIButton: TButton;
    PJ64DirectoryEdit: TDirectoryEdit;
    SourceDirectoryComboBox: TComboBox;
    StateComboBox: TComboBox;
    UpdateStateListButton: TButton;
    procedure LoadINI();
    procedure SaveINI();
    procedure UpdateDestinationsComboBox();
    procedure DestinationComboBoxChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
    procedure MenuItem8Click(Sender: TObject);
    procedure SaveINIButtonClick(Sender: TObject);
    procedure SourceDirectoryComboBoxChange(Sender: TObject);
    procedure SourceDirectoryComboBoxEditingDone(Sender: TObject);
    procedure StateComboBoxChange(Sender: TObject);
    procedure UpdateStateListButtonClick(Sender: TObject);
  private
    SourcePath: string;
    DestPath: string;
    DefaultSaveStatePath: string;

    LoadedStates: array of State;        // State contains the displayname as State.StateName and the path for CopyFile as State.AbsolutePath
    PotentialDestinations: array of string;

    SourceDirectories: array[0..4] of string;
  public

  end;

var
  Form1: TForm1;
  Form2: TForm2;

implementation


// s.c.6

function GetDestFileFromPath(const path:string): string;
begin
   GetDestFileFromPath := RightStr(path, Length(path) - LastDelimiter('\',path));
end;

{ State }

constructor State.Create;
begin
  StateName := 'UNDEFINED';
  LocalPath := 'UNDEFINED';
  AbsolutePath := 'UNDEFINED';
end;

constructor State.Create(absPath: string);
var
  indexA: integer;
begin
  AbsolutePath := absPath;

  indexA := LastDelimiter('\', absPath);

  LocalPath := RightStr(absPath, Length(absPath) - indexA);
  StateName := LeftStr(LocalPath, Length(LocalPath) - 4);

end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  INI: TINIFile;
  index: integer;
begin
  Application.CreateForm(TForm2, Form2);
  // s.c.1
  // s.c.4
  // load the previously saved directories from the file
  LoadINI;
end;

procedure TForm1.MenuItem3Click(Sender: TObject);
begin
  LoadINI;
end;

procedure TForm1.MenuItem5Click(Sender: TObject);
begin
     UpdateStateListButtonClick(Sender);
end;

procedure TForm1.MenuItem7Click(Sender: TObject);
begin
    Application.Terminate;
end;

procedure TForm1.MenuItem8Click(Sender: TObject);
var
  index:integer;
begin
  // open a new form with 5 directories that can be changed
    Form2.ShowModal;

    SourceDirectoryComboBox.Items.Clear;

    for index:=0 to 4 do
    begin
        SourceDirectories[index] := Form2.GetDirectory(index);
        SourceDirectoryComboBox.Items.Add(SourceDirectories[index]);
    end;
end;

procedure TForm1.LoadINI();
var
  INI:TINIFile;
  index:integer;
begin
  INI := TINIFile.Create('stateSwapData.ini');
  SourceDirectoryComboBox.Items.Clear;

  // reclaim all 5 directories from INI
  for index:=0 to 4 do
  begin
      SourceDirectories[index] := INI.ReadString('Directories', Format('SourceDir%d',[index+1]), '');
      SourceDirectoryComboBox.Items.Add(SourceDirectories[index]);
  end;

  // update UI elements in Form2
  Form2.DirectoryEdit1.Directory := SourceDirectories[0];
  Form2.DirectoryEdit2.Directory := SourceDirectories[1];
  Form2.DirectoryEdit3.Directory := SourceDirectories[2];
  Form2.DirectoryEdit4.Directory := SourceDirectories[3];
  Form2.DirectoryEdit5.Directory := SourceDirectories[4];

  // select the first path in the list
  SourcePath := SourceDirectories[0];
  // update the combo box
  SourceDirectoryComboBox.ItemIndex:=0;

  // restore PJ64 save folder from INI
  DestPath := INI.ReadString('Directories', 'PJ64SaveDir', '');
  // update UI
  PJ64DirectoryEdit.Directory   := DestPath;

  // Memory leaks aren't real don't lie to me.
  INI.Free;
end;

procedure TForm1.SaveINI();
var
  INI:TINIFile;
  index:integer;
begin
     INI := TINIFile.Create('stateSwapData.ini');

     for index:=0 to 4 do
     begin
          INI.WriteString('Directories', Format('SourceDir%d',[index+1]), SourceDirectories[index]);
     end;
     INI.WriteString('Directories', 'PJ64SaveDir', DestPath);
     //INI.WriteInteger('Other', 'SelectedDirectory', SourceDirectoryComboBox.ItemIndex);           // perhaps

     INI.Free;
end;

procedure TForm1.UpdateDestinationsComboBox();
var
  potentialStates : TStringList;
  index: integer;
begin
     // reset the combo box
  DestinationComboBox.Items.Clear;

  // search for Destination State combo box values
  potentialStates := TStringList.Create();
  FindAllFiles(potentialStates, DestPath, '*.zip', false);
  SetLength(PotentialDestinations, potentialStates.Count);

  for index := 0 to potentialStates.Count do
  begin
      if index = potentialStates.Count then break;
      PotentialDestinations[index] := potentialStates[index];
      DestinationComboBox.Items.Add(GetDestFileFromPath(PotentialDestinations[index]));
  end;

  if potentialStates.Count > 0 then
  begin
    DestinationComboBox.ItemIndex:=0;
    DefaultSaveStatePath := PotentialDestinations[DestinationComboBox.ItemIndex];
  end;
end;

procedure TForm1.DestinationComboBoxChange(Sender: TObject);
begin
  // select the correct DefaultSaveStatePath based on the index chosen
     DefaultSaveStatePath := PotentialDestinations[DestinationComboBox.ItemIndex];
end;

// s.c.5

// when the user chooses to update the INI file
procedure TForm1.SaveINIButtonClick(Sender: TObject);
begin
  SaveINI;
end;

procedure TForm1.SourceDirectoryComboBoxChange(Sender: TObject);
begin
  SourcePath := SourceDirectories[SourceDirectoryComboBox.ItemIndex];
end;

procedure TForm1.SourceDirectoryComboBoxEditingDone(Sender: TObject);
begin
     UpdateDestinationsComboBox;
end;

// when the user selects a state
procedure TForm1.StateComboBoxChange(Sender: TObject);
begin
     CopyFile(LoadedStates[StateComboBox.ItemIndex].AbsolutePath, DefaultSaveStatePath);
end;

// when the user locks in the current config to update the state list
procedure TForm1.UpdateStateListButtonClick(Sender: TObject);
  var
    fileNames: TStringList;
    index: integer;
begin
     StateComboBox.Items.Clear();

     DestPath := PJ64DirectoryEdit.Directory;

     fileNames := TStringList.Create();
     FindAllFiles(fileNames, SourcePath, '*.zip', false);
     SetLength(LoadedStates, fileNames.Count);

     for index := 0 to fileNames.Count do
     begin
          if index = fileNames.Count then break;
          LoadedStates[index] := State.Create(fileNames[index]);
          StateComboBox.Items.Add(LoadedStates[index].StateName);
     end;

     UpdateDestinationsComboBox;
end;

{$R *.lfm}

{ TForm1 }


end.

