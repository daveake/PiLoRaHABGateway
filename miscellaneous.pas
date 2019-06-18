unit Miscellaneous;
{$mode objfpc}{$H+}

interface

uses sysutils;

function CRC16(Line: AnsiString): String;

type TGPSPosition = record
	TimeStamp:		TDateTime;
	Latitude:		Double;
	Longitude:		Double;
	Altitude:		Double;
	Satellites:		Integer;
	Speed:			Double;
	Direction:		Double;
end;

implementation

function CRC16(Line: AnsiString): String;
var
    i, j: Integer;
    CRC, xPolynomial: WORD;
    Temp: AnsiString;
begin
    CRC := $ffff;           // Seed
    xPolynomial := $1021;

    for i := 1 to Length(Line) do begin
        CRC := CRC xor (WORD(Line[i]) shl 8);
        for j := 0 to 7 do begin
            if (CRC and $8000) <> 0 then begin
                CRC := CRC and $7FFF;
                CRC := (CRC shl 1) xor xPolynomial;
            end else begin
                CRC := CRC shl 1;
            end;
        end;
    end;

    Temp := IntToHex(CRC, 4);

    Result := Temp;
end;

end.

