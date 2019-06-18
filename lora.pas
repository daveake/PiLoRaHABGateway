unit LoRa;
{$mode objfpc}{$H+}

interface

// uses  rpi_hal,cthreads,BaseUnix,Unix,objects,sysutils,strings;
// uses  PXL.Boards.RPi, objects,sysutils,strings;
  uses SPI, objects, sysutils, strings;
  
type
  TLoRa = class
  private
    { Private declarations }
	CurrentMode: Byte;
	PayloadLength: Integer;
	Sending: Boolean;
        //FastSystemCore: TFastSystemCore;
        //FastGPIO: TFastGPIO;
        //FastSPI: TFastSPI;
    SPIDevice: Tspi;
    procedure SetMode(Mode: Integer);
	procedure WriteRegister(Reg, Value: Byte);
	function ReadRegister(Reg: Byte): Byte;
  public
    { Public declarations }
    constructor Create(Channel, Mode: Integer; Frequency: Double);
	function IsSending: Boolean;
	procedure SetLoRaFrequency(Frequency: Double);
	procedure SetLoRaParameters(ImplicitOrExplicit, ErrorCoding, Bandwidth, SpreadingFactor, LowDataRateOptimize: Byte);
	procedure SetStandardLoRaParameters(Mode: Integer);
	procedure SendText(Text: String);
    procedure Listen;
    function GotPacket: Boolean;
    function GetPacket: String;
end;

const
	REG_FIFO                         = $00;
	REG_FIFO_ADDR_PTR	         = $0D;
	REG_FIFO_TX_BASE_AD              = $0E;
	REG_FIFO_RX_BASE_AD              = $0F;
	REG_RX_NB_BYTES                  = $13;
	REG_OPMODE                       = $01;
	REG_FIFO_RX_CURRENT_ADDR         = $10;
	REG_IRQ_FLAGS                    = $12;
	REG_PACKET_SNR				= $19;
	REG_PACKET_RSSI				= $1A;
	REG_CURRENT_RSSI			= $1B;
	REG_DIO_MAPPING_1          = $40;
	REG_DIO_MAPPING_2          = $41;
	REG_MODEM_CONFIG           = $1D;
	REG_MODEM_CONFIG2          = $1E;
	REG_MODEM_CONFIG3          = $26;
	REG_PAYLOAD_LENGTH         = $22;
	REG_IRQ_FLAGS_MASK         = $11;
	REG_HOP_PERIOD             = $24;
	REG_FREQ_ERROR				= $28;
	REG_DETECT_OPT				= $31;
	REG_DETECTION_THRESHOLD		= $37;

	// MODES
	RF98_MODE_RX_CONTINUOUS    = $85;
	RF98_MODE_TX               = $83;
	RF98_MODE_SLEEP            = $80;
	RF98_MODE_STANDBY          = $81;

	// Modem Config 1
	EXPLICIT_MODE              = $00;
	IMPLICIT_MODE              = $01;

	ERROR_CODING_4_5           = $02;
	ERROR_CODING_4_6           = $04;
	ERROR_CODING_4_7           = $06;
	ERROR_CODING_4_8           = $08;

	BANDWIDTH_7K8              = $00;
	BANDWIDTH_10K4             = $10;
	BANDWIDTH_15K6             = $20;
	BANDWIDTH_20K8             = $30;
	BANDWIDTH_31K25            = $40;
	BANDWIDTH_41K7             = $50;
	BANDWIDTH_62K5             = $60;
	BANDWIDTH_125K             = $70;
	BANDWIDTH_250K             = $80;
	BANDWIDTH_500K             = $90;

	// Modem Config 2

	SPREADING_6                = $60;
	SPREADING_7                = $70;
	SPREADING_8                = $80;
	SPREADING_9                = $90;
	SPREADING_10               = $A0;
	SPREADING_11               = $B0;
	SPREADING_12               = $C0;

	CRC_OFF                    = $00;
	CRC_ON                     = $04;

	// POWER AMPLIFIER CONFIG
	REG_PA_CONFIG              = $09;
	PA_MAX_BOOST               = $8F;
	PA_LOW_BOOST               = $81;
	PA_MED_BOOST               = $8A;
	PA_MAX_UK                  = $88;
	PA_OFF_BOOST               = $00;
	RFO_MIN                    = $00;

	// LOW NOISE AMPLIFIER
	REG_LNA                    = $0C;
	LNA_MAX_GAIN               = $23;
	LNA_OFF_GAIN               = $00;
	LNA_LOW_GAIN               = $C0;

implementation

procedure TLoRa.WriteRegister(Reg, Value: Byte);
var
    Data: Array[0..1] of Byte;
begin
	WriteLn('Write(' + IntToHex(Reg, 2) + ') = ' + IntToHex(Value, 2));
    Data[0] := Reg or $80;
    Data[1] := Value;

    // FastSPI.Write(@Data, 2);

    SPIDevice.DataOut(Data, 2);
end;

function TLoRa.ReadRegister(Reg: Byte): Byte;
var
    DataOut, DataIn: Array[0..1] of Byte;
begin
    DataOut[0] := Reg and $7F;
    DataOut[1] := 0;

    // FastSPI.Transfer(@Data, @Data, 2);
    SPIDevice.TransferSync(DataOut, DataIn, 2);

    Result := DataIn[1];
	WriteLn('Read(' + IntToHex(Reg, 2) + ') = ' + IntToHex(Result, 2));
end;

procedure TLoRa.SetMode(Mode: Integer);
begin
	if Mode <> CurrentMode then begin
		if Mode = RF98_MODE_TX then begin
			// TURN LNA OFF FOR TRANSMIT
			WriteRegister(REG_LNA, LNA_OFF_GAIN);
			
			// Set 10mW
			WriteRegister(REG_PA_CONFIG, PA_MAX_UK);
		end else if Mode = RF98_MODE_RX_CONTINUOUS then begin
			// PA Off
			WriteRegister(REG_PA_CONFIG, PA_OFF_BOOST);
			
			// Max LNA Gain
			WriteRegister(REG_LNA, LNA_MAX_GAIN);
		end;
	
		WriteRegister(REG_OPMODE, Mode);
		
		CurrentMode := Mode;
		
		Sleep(10);
	end;
end;

procedure TLoRa.SetLoRaFrequency(Frequency: Double);
var
	FrequencyValue: LongInt;
begin
	SetMode(RF98_MODE_STANDBY);
	SetMode(RF98_MODE_SLEEP);
	WriteRegister(REG_OPMODE, $80);
	SetMode(RF98_MODE_STANDBY);
		
	FrequencyValue := Trunc((Frequency * 7110656) / 434);
	
	WriteLn('Setting frequency to ' + FormatFloat('0.00', Frequency));
	WriteLn('Frequency value is ' + IntToHex(FrequencyValue, 6));
	
	WriteRegister($06, (FrequencyValue shr 16) and $FF);
	WriteRegister($07, (FrequencyValue shr 8) and $FF);
	WriteRegister($08, FrequencyValue and $FF);
end;

procedure TLoRa.SetLoRaParameters(ImplicitOrExplicit, ErrorCoding, Bandwidth, SpreadingFactor, LowDataRateOptimize: Byte);
begin
	WriteRegister(REG_MODEM_CONFIG, ImplicitOrExplicit or ErrorCoding or Bandwidth);
	WriteRegister(REG_MODEM_CONFIG2, SpreadingFactor or CRC_ON);
	WriteRegister(REG_MODEM_CONFIG3, $04 or LowDataRateOptimize);
	if SpreadingFactor = SPREADING_6 then begin
		WriteRegister(REG_DETECTION_THRESHOLD, $0C);
		WriteRegister(REG_DETECT_OPT, (ReadRegister(REG_DETECT_OPT) and $F8) or $05);
	end else begin
		WriteRegister(REG_DETECTION_THRESHOLD, $0A);
		WriteRegister(REG_DETECT_OPT, (ReadRegister(REG_DETECT_OPT) and $F8) or $03);
	end;

	if ImplicitOrExplicit = IMPLICIT_MODE then begin
		PayloadLength := 255;
	end else begin
		PayloadLength := 0;
	end;
	
	WriteRegister(REG_PAYLOAD_LENGTH, PayloadLength);
	WriteRegister(REG_RX_NB_BYTES, PayloadLength);
end;

procedure TLoRa.SetStandardLoRaParameters(Mode: Integer);
begin
	WriteLn('Set LoRa Mode ' + IntToStr(Mode));
	if Mode = 0 then begin
		SetLoRaParameters(EXPLICIT_MODE, ERROR_CODING_4_8, BANDWIDTH_20K8, SPREADING_11, $08);
	end else if Mode = 1 then begin
		SetLoRaParameters(IMPLICIT_MODE, ERROR_CODING_4_5, BANDWIDTH_20K8, SPREADING_6, $00);
	end else if Mode = 2 then begin
		SetLoRaParameters(EXPLICIT_MODE, ERROR_CODING_4_8, BANDWIDTH_62K5, SPREADING_8, $00);
	end;
end;

constructor TLoRa.Create(Channel,  Mode: Integer; Frequency: Double);
begin
	WriteLn('LoRa Channel ' + IntToStr(Channel));

    //FastSystemCore := TFastSystemCore.Create;
    //FastGPIO := TFastGPIO.Create(FastSystemCore);
    //FastSPI := TFastSPI.Create(FastGPIO);
    //FastSPI.ChipSelectIndex := Channel;
    SPIDevice := Tspi.Create;
    SPIDevice.Init;
	
	CurrentMode := $81;
	
	// DIO Mapping
	WriteRegister(REG_DIO_MAPPING_2, $00);
		
	SetLoRaFrequency(Frequency);
	
	SetStandardLoRaParameters(Mode);
end;

function TLoRa.IsSending: Boolean;
begin
	if Sending then begin
		// Test TxDone flag
		Sending := (ReadRegister(REG_IRQ_FLAGS) and $08) = 0;
		
		if not Sending then begin
			// Reset TxDone
			WriteRegister(REG_IRQ_FLAGS, $08);
		end;
	end;
	
	Result := Sending;
end;

procedure TLoRa.SendText(Text: String);
var	
	Data: Array[0..255] of Byte;
	i, Len: Integer;
begin
	Sending := True;
	
	Len := Length(Text);
	WriteLn('Sending ' + IntToStr(Len) + ' bytes');
	
	SetMode(RF98_MODE_STANDBY);
	
	WriteRegister(REG_FIFO_TX_BASE_AD, $00);
	WriteRegister(REG_FIFO_ADDR_PTR, $00);

	Data[0] := $80;
	for i := 1 to Len do begin
		Data[i] := Byte(Text[i]);
	end;
	
	// SPI_Transfer(LoRaBus, LoRaChannel, #80 + 'Hello');
	// SPI_Write_Buffer(LoRaBus, LoRaChannel, Data, Len+1);
		
	if PayloadLength > 0 then begin
		WriteRegister(REG_PAYLOAD_LENGTH, PayloadLength);
	end else begin
		WriteRegister(REG_PAYLOAD_LENGTH, Length(Text));
	end;

	// Send it
	SetMode(RF98_MODE_TX);
end;

procedure TLoRa.Listen;
begin
    // WriteRegister(REG_DIO_MAPPING_1, 0x00)  	# 00 00 00 00 maps DIO0 to RxDone

    WriteRegister(REG_PAYLOAD_LENGTH, 255);
    WriteRegister(REG_RX_NB_BYTES, 255);

	WriteRegister(REG_FIFO_RX_BASE_AD, 0);
	WriteRegister(REG_FIFO_ADDR_PTR, 0);

	// Setup Receive Continous Mode
	SetMode(RF98_MODE_RX_CONTINUOUS);
end;

function TLoRa.GotPacket: Boolean;
begin
	Result := (ReadRegister(REG_IRQ_FLAGS) and $40) <> 0;
end;

function TLoRa.GetPacket: String;
var
    Status, CurrentAddress, ByteCount: Byte;
    i: Integer;
    Buffer: Array[0..255] of Byte;
begin
    Result := '';

    // Grab status register (CRC etc)
	Status := ReadRegister(REG_IRQ_FLAGS);

    // Clear RxDone
	WriteRegister(REG_IRQ_FLAGS, $40);


	// check for payload crc issues
	if (Status and $20 ) <> 0 then begin
	    // print("CRC Failure, RSSI " + str(self.__PacketRSSI()))

		// reset the crc flags
		WriteRegister(REG_IRQ_FLAGS, $20);
        Result := 'CRC Error';
		// BadCRCCount++
	end else begin
	    CurrentAddress := ReadRegister(REG_FIFO_RX_CURRENT_ADDR);
		ByteCount := ReadRegister(REG_RX_NB_BYTES);

		// FreqError = FrequencyError() / 1000;

		WriteRegister(REG_FIFO_ADDR_PTR, CurrentAddress);

        Buffer[0] := REG_FIFO;
        Buffer[1] := 0;
        SPIDevice.TransferSync(Buffer, Buffer, ByteCount);
		// Request = [REG_FIFO] + [0] * Bytes
		// Packet = self.spi.xfer(Request)[1:]

			//# if Config.LoRaDevices[Channel].AFC && (fabs( FreqError ) > 0.5)
			//# {
			//	# if (Config.LoRaDevices[Channel].MaxAFCStep > 0)
			//	# {
			//		# // Limit step to MaxAFCStep
			//		# if (FreqError > Config.LoRaDevices[Channel].MaxAFCStep)
			//		# {
			//			# FreqError = Config.LoRaDevices[Channel].MaxAFCStep;
			//		# }
			//		# else if (FreqError < -Config.LoRaDevices[Channel].MaxAFCStep)
			//		# {
			//			# FreqError = -Config.LoRaDevices[Channel].MaxAFCStep;
			//		# }
			//	# }
			//	# ReTune( Channel, FreqError / 1000 );
			//# }

		// Clear all flags
		WriteRegister(REG_IRQ_FLAGS, $FF);

        for i := 1 to ByteCount-1 do begin
            Result := Result + Char(Buffer[i]);
        end;

		// return {'packet': Packet, 'freq_error': self.FreqError}
    end;
end;

end.
