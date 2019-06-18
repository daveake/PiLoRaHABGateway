unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  LoRa;

type

  { TForm1 }

  TForm1 = class(TForm)
      ListBox1: TListBox;
      tmrPoll: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure tmrPollTimer(Sender: TObject);
  private
    LoRaDevices: Array[0..0] of TLoRa;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  // Create LoRa objects
  for i := Low(LoRaDevices) to High(LoRaDevices) do begin
    LoRaDevices[i] := TLoRa.Create(i, 1, 434.447);
  end;

  // Set mode
  // LoRaDevices[0].SetStandardLoRaParameters(1);

  // Set frequency
  // LoRaDevices[0].SetFrequency(434.450);

  // Listen
  LoRaDevices[0].Listen;
end;

procedure TForm1.tmrPollTimer(Sender: TObject);
var
    Sentence: String;
begin
    if LoRaDevices[0] <> nil then begin
        if LoRaDevices[0].GotPacket then begin
            ListBox1.ItemIndex := ListBox1.Items.Add(LoRaDevices[0].GetPacket);
        end;
    end;
end;

end.

