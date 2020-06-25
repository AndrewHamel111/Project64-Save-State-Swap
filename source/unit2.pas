unit Unit2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, EditBtn, StdCtrls;

type

  { TForm2 }

  TForm2 = class(TForm)
    Button1: TButton;
    DirectoryEdit1: TDirectoryEdit;
    DirectoryEdit2: TDirectoryEdit;
    DirectoryEdit3: TDirectoryEdit;
    DirectoryEdit4: TDirectoryEdit;
    DirectoryEdit5: TDirectoryEdit;
    procedure FormCreate(Sender: TObject);
    function GetDirectory(index:integer):string;
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    SourceDirectories: array[0..4] of string;
  public

  end;

var
  Form2: TForm2;

implementation

{$R *.lfm}

{ TForm2 }

procedure TForm2.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Form2.SourceDirectories[0] := DirectoryEdit1.Directory;
  Form2.SourceDirectories[1] := DirectoryEdit2.Directory;
  Form2.SourceDirectories[2] := DirectoryEdit3.Directory;
  Form2.SourceDirectories[3] := DirectoryEdit4.Directory;
  Form2.SourceDirectories[4] := DirectoryEdit5.Directory;
end;

function TForm2.GetDirectory(index: integer): string;
begin
  if index < 0 then GetDirectory:=''
  else if index > 4 then GetDirectory:='';

  GetDirectory:=SourceDirectories[index];
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
    Form2 := Self;
end;

procedure TForm2.Button1Click(Sender: TObject);
begin
  Form2.Close;                        // this will also trigger FormClose
end;

end.

