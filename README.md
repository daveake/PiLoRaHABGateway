LazarusC Pi HAB LoRa Gateway
============================

(Very) simple Pascal LoRa receiver for the Pi.

Runs on a Raspberry Pi with LoRa module attached to the SPI port.

Connections
===========

Connect the LoRa module like so:

	LORA     PI
	----     --
	3.3V	3.3V Power
	GND		Ground
	MOSI	MOSI (pin 19)
	MISO	MISO (pin 21)
	NSS		CE0 (pin 24) (CE1 (pin 26) for 2nd module)
	SCK		SLCK

DIO pins are not used in this implementation.

Installation
============

Enable SPI in raspi-config.

Extract this repository.

Open PiLoRaHABGateway in Lazarus IDE, compile and run with F9.

Program does not need root access.

NOTE
====

This is not a true gateway yet; it only receives and does not upload to Habitat.  It's basically useless.  I suggest you wait till it's somewhat finished !!

	