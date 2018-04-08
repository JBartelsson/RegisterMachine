unit umanualform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Buttons, ExtCtrls;

type

  { TmanualForm }

  TmanualForm = class(TForm)
    CloseBtn: TBitBtn;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    Image5: TImage;
    Image6: TImage;
    manualPages: TPageControl;
    theoryScrollBox: TScrollBox;
    manualScrollBox: TScrollBox;
    TabTheory: TTabSheet;
    TabManual: TTabSheet;
    procedure CloseBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  manualForm: TmanualForm;

implementation

{$R *.lfm}

{ TmanualForm }

procedure TmanualForm.CloseBtnClick(Sender: TObject);
begin
  Hide;
end;

procedure TmanualForm.FormCreate(Sender: TObject);
var i : Integer;

begin
  //Image1.Picture.LoadFromFile('images\1.jpg');
  //for i := 0 to 5 do
  //begin
  //  temp := TBitmap.Create;
  //  ImageList1.add;
  //end;

end;

procedure TmanualForm.FormResize(Sender: TObject);
var i : Integer;
  actImage: TImage;
begin
  for i := 1 to 6 do
  begin
  actImage := (manualForm.FindComponent('Image' + IntToStr(i)) as TImage);
  actImage.Width := theoryScrollBox.Width - 40;
  actImage.Height := Round(actImage.Picture.Height/actImage.Picture.Width*actImage.Width);
  end;
end;

end.

