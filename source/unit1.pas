unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls,
  EditBtn, ExtCtrls, FileUtil, LazFileUtils, IniFiles;

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
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    RomNameEdit: TEdit;
    SaveINIButton: TButton;
    SourceDirectoryEdit: TDirectoryEdit;
    PJ64DirectoryEdit: TDirectoryEdit;
    StateComboBox: TComboBox;
    UpdateStateListButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure SaveINIButtonClick(Sender: TObject);
    procedure StateComboBoxChange(Sender: TObject);
    procedure UpdateStateListButtonClick(Sender: TObject);
  private
    SourcePath: string;
    DestPath: string;
    RomName: string;
    DefaultSaveStatePath: string;

    LoadedStates: array of State;        // State contains the displayname as State.StateName and the path for CopyFile as State.AbsolutePath
  public

  end;

var
  Form1: TForm1;


implementation

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
begin
     // s.c.1
     // load the previously saved directories from the file
     INI := TINIFile.Create('stateSwapData.ini');

     try      // this part uses voodoo from IniFiles
       try
         // s.c.4
         SourcePath := INI.ReadString('Directories', 'SourceDir', '');
         DestPath := INI.ReadString('Directories', 'PJ64SaveDir', '');
         RomName := INI.ReadString('RomSettings', 'RomName', 'Super Mario 64 (U)');

         // update the directories in the form
         SourceDirectoryEdit.Directory := SourcePath;
         PJ64DirectoryEdit.Directory   := DestPath;
         RomNameEdit.Text              := RomName;

       except               // set some default paths
         ShowMessage('stateSwapData.ini does not exist or does not contain valid data. Please manually select the directories and Rom Name.');
       end;
     finally              // update the dest for CopyFile
       INI.Free;
       DefaultSaveStatePath := Concat(DestPath, '\', RomName, '.pj.zip');
     end;
end;

// s.c.5

// when the user chooses to update the INI file
procedure TForm1.SaveINIButtonClick(Sender: TObject);
var
   INI : TINIFile;
begin
   INI := TINIFile.Create('stateSwapData.ini');

   try      // this part uses voodoo from IniFiles
     try
       // s.c.4
       INI.WriteString('Directories', 'SourceDir', SourcePath);
       INI.WriteString('Directories', 'PJ64SaveDir', DestPath);
       INI.WriteString('RomSettings', 'RomName', RomName);
     except               // set some default paths
       ShowMessage('stateSwapData.ini could not be written to. Your configuration will not be saved at this time.');
     end;
   finally              // update the dest for CopyFile
     INI.Free;
   end;
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

     SourcePath           := SourceDirectoryEdit.Directory;
     DestPath             := PJ64DirectoryEdit.Directory;
     RomName              := RomNameEdit.Text;

     DefaultSaveStatePath := DestPath + '\' + RomName + '.pj.zip';      // consider having the user select the default save state file and use that path

     fileNames := TStringList.Create();
     FindAllFiles(fileNames, SourceDirectoryEdit.Directory, '*.zip', false);
     SetLength(LoadedStates, fileNames.Count);
     ShowMessage('Directory searched: '+SourceDirectoryEdit.Directory+LineEnding+Format('Files found: %d',[fileNames.Count]));

     for index := 0 to fileNames.Count do
     begin
          if index = fileNames.Count then break;
          LoadedStates[index] := State.Create(fileNames[index]);
          StateComboBox.Items.Add(LoadedStates[index].StateName);
     end;
end;


{$R *.lfm}

{ TForm1 }


end.

