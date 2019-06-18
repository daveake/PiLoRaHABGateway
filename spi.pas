unit  SPI;

{$ mode objfpc} {$ H +}

interface

uses
  Classes ,  SysUtils ,  BaseUnix ;

type
  Tspi  =  class

  procedure  Init () ;
  procedure  Close () ;

  function  DataIn ( var  din ;  cnt  :  byte )  :  integer ;
  function  DataOut ( const  dout ;  cnt  :  byte )  :  integer ;

  procedure  TransferSync ( const  dout ;  var  din ;  len  :  byte ) ;
  procedure  TransmitSync ( const  dout ;  len  :  byte ) ;
  // function  FastShift ( dout  :  byte )  :  byte ;

  private
    {private declarations}
  public
    {public declarations}
  end ;

const
  // SPI Mode Flags

  cSPI_CPHA  =  $01 ;
  cSPI_CPOL  =  $02 ;

  cSPI_MODE_0  =  ( 0  or  0 ) ;
  cSPI_MODE_1  =  ( 0  or  cSPI_CPHA ) ;
  cSPI_MODE_2  =  ( cSPI_CPOL  or  0 ) ;
  cSPI_MODE_3  =  ( cSPI_CPOL  or  cSPI_CPHA ) ;

  cSPI_CS_HIGH    =  $04 ;
  cSPI_LSB_FIRST  =  $08 ;
  cSPI_3WIRE      =  $10 ;
  cSPI_LOOP       =  $20 ;
  cSPI_NO_CS      =  $40 ;
  cSPI_READY      =  $80 ;

  cSPI_CTRL  =  $6B ;   // is the "magic byte"

  // Control Register
  // Read / Write + Size + MagicByte + Register

  cSPI_RD_MODE           :  uint32  =  $80016B01 ;
  cSPI_WR_MODE           :  uint32  =  $40016B01 ;
  cSPI_RD_LSB_FIRST      :  uint32  =  $80016B02 ;
  cSPI_WR_LSB_FIRST      :  uint32  =  $40016B02 ;
  cSPI_RD_BITS_PER_WORD  :  uint32  =  $80016B03 ;
  cSPI_WR_BITS_PER_WORD  :  uint32  =  $40016B03 ;
  cSPI_RD_MAX_SPEED_HZ   :  uint32  =  $80046B04 ;
  cSPI_WR_MAX_SPEED_HZ   :  uint32 =  $40046B04 ;

  cSPI_DEVICE  =  '/dev/spidev0.0' ;   // Device 0, ChipSelect 0
// cSPI_DEVICE = '/dev/spidev0.1'; // Device 0, ChipSelect 1
  cSPI_SPEED   =  1000000 ;   // data rate in Hz
  cSPI_BITS    =  8 ;   // data bits
  cSPI_LSBF    =  0 ;   // LSB first = -1
  cSPI_MODE    =  cSPI_MODE_0 ;

type
  spi_ioc_transfer  =  record   // Control register for FileIO requires
    tx_buf  :  uint64 ;   //pointer;
    rx_buf  :  uint64 ;   //pointer; // always 64-bit
    len  :  uint32 ;     // number of characters
    speed  :  uint32 ;   // data rate in Hz
    delay  :  uint16 ;   // delay CS in usec
    bpw  :  uint8 ;   // bits per word
    csc  :  uint8 ;   // CS change
    txn  :  uint8 ;
    rxn  :  uint8 ;
    pad  :  uint16 ;
  end ;   // 32 bytes in total

var
  //spi  :  Tspi ;
  spihnd  :  longint ;   // the filehandle for the interface

implementation

// initialize SPI

procedure  Tspi . Init () ;
var
 val8  :  byte ;
 val32  :  longword ;
begin
 try
   spihnd  :=  FpOpen ( cSPI_DEVICE ,  O_RdWr ) ;   // open interface for read / write

   if  spihnd  <>  - 1  then  begin
     val8  :=  cSPI_MODE ;
     FpIOCtl ( spihnd ,  cSPI_WR_MODE ,  @ val8 ) ;   // set mode
     val8  :=  cSPI_BITS ;
     FpIOCtl ( spihnd ,  cSPI_WR_BITS_PER_WORD ,  @ val8 ) ;   // set data bits per byte
     val8  :=  cSPI_LSBF ;   // - 1
     FpIOCtl ( spihnd ,  cSPI_WR_LSB_FIRST , @ val8 ) ;   // MSB or LSB first set
     val32  :=  cSPI_SPEED ;
     FpIOCtl ( spihnd ,  cSPI_WR_MAX_SPEED_HZ ,  @ val32 ) ;   // set speed
   end ;
 finally

 end ;
end ;

// finish SPI

procedure  Tspi . Close () ;
begin
  if  spihnd  <>  - 1  then  begin
    FpClose ( spihnd ) ;
  end ;
end ;

// SPI Buffer Read

function  Tspi . DataIn ( var  din ;  cnt  :  byte )  :  integer ;
begin
  if  spihnd  <>  - 1  then  begin
    DataIn  :=  FpRead ( spihnd ,  din ,  cnt ) ;
  end ;
end ;

// SPI Buffer Write

function  Tspi . DataOut ( const  dout ;  cnt  :  byte )  :  integer ;
begin
  if  spihnd  <>  - 1  then  begin
    DataOut  :=  FpWrite ( spihnd ,  dout ,  cnt ) ;
  end ;
end ;

// send SPI data, receive

procedure  Tspi . TransferSync ( const  dout ;  var  din ;  len  :  byte ) ;
var
  outbuf  :  array  of  byte ;
  inbuf  :  array  of  byte ;
  transfer  :  spi_ioc_transfer ;
begin
  if  len  >  0  then  begin
    SetLength ( outbuf ,  len ) ;
    FillByte ( outbuf [ 0] ,  len ,  0 ) ;
    Move ( dout ,  outbuf [ 0 ] ,  len ) ;

    SetLength ( inbuf ,  len ) ;
    FillByte ( inbuf [ 0 ] ,  len ,  0 ) ;

    FillByte ( transfer ,  SizeOf ( transfer ) ,  0 ) ;
    transfer . tx_buf  :=  uint64 ( @ outbuf [ 0 ]) ;
    transfer . rx_buf  :=  uint64 ( @ inbuf [ 0 ]) ;
    transfer . len  :=  len ;
    transfer . delay  :=  0 ;
    transfer . speed  :=  cSPI_SPEED;
    transfer . bpw  :=  cSPI_BITS ;
    transfer . csc  :=  0 ;

    try
      FpIOCtl ( spihnd ,  $40206B00 ,  @ transfer ) ;
    finally
      Move ( inbuf [ 0 ] ,  din ,  len ) ;
    end ;
  end ;
end ;

// SPI send data without receiving

procedure  Tspi . TransmitSync ( const  dout ;  len  :  byte ) ;
var
  outbuf  :  array  of  byte ;
  transfer  :  spi_ioc_transfer ;
begin
  if  len  >  0  then  begin
    SetLength ( outbuf ,  len ) ;
    FillByte ( outbuf [ 0 ] ,  len ,  0 ) ;
    Move (dout ,  outbuf [ 0 ] ,  len ) ;

    FillByte ( transfer ,  SizeOf ( transfer ) ,  0 ) ;
    transfer . tx_buf  :=  uint64 ( @ outbuf [ 0 ]) ;
    transfer . rx_buf  :=  uint64 ( @ outbuf [ 0 ]) ;   // same buffer
    transfer . len  :=  len ;
    transfer . delay  :=  0 ;
    transfer . speed :=  cSPI_SPEED ;
    transfer . bpw  :=  cSPI_BITS ;
    transfer . csc  :=  0 ;

    try
      FpIOCtl ( spihnd ,  $40206B00 ,  @ transfer ) ;
    finally

    end ;
  end ;
end ;

end.

