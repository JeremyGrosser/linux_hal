--
--  Copyright (C) 2023 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
private with Interfaces.C;
with HAL.SPI;
with HAL;

package Linux.SPI is

   type Port
      (Buffer_Length : Natural)
   is new HAL.SPI.SPI_Port with private;

   procedure Open
      (This     : in out Port;
       Filename : String);

   procedure Set_Max_Speed
      (This : in out Port;
       Hz   : Natural);

   function Max_Speed
      (This : Port)
      return Natural;

   function Is_Open
      (This : Port)
      return Boolean;

   procedure Close
      (This : in out Port)
   with Pre => This.Is_Open;

   function Available
      (This : Port)
      return Natural;
   --  Number of bytes in receive buffer

   procedure Transfer
      (This    : in out Port;
       Data    : in out HAL.SPI.SPI_Data_8b;
       Status  : out HAL.SPI.SPI_Status)
   with Pre => This.Is_Open;
   --  Transfer synchronously transmits Data while receiving bytes into the
   --  same array. Transfer does not modify the This.Port.Buffer.

   overriding
   function Data_Size
      (This : Port)
      return HAL.SPI.SPI_Data_Size;

   overriding
   procedure Transmit
      (This    : in out Port;
       Data    : HAL.SPI.SPI_Data_8b;
       Status  : out HAL.SPI.SPI_Status;
       Timeout : Natural := 1_000)
   with Pre => This.Is_Open and then Data'Length <= This.Buffer_Length;
   --  Transmit clocks out Data on MOSI while writing bytes from MISO to
   --  This.Buffer. Any existing data in This.Buffer is overwritten.

   overriding
   procedure Receive
      (This    : in out Port;
       Data    : out HAL.SPI.SPI_Data_8b;
       Status  : out HAL.SPI.SPI_Status;
       Timeout : Natural := 1_000)
   with Pre => This.Is_Open and then This.Available <= Data'Length;

   overriding
   procedure Transmit
      (This    : in out Port;
       Data    : HAL.SPI.SPI_Data_16b;
       Status  : out HAL.SPI.SPI_Status;
       Timeout : Natural := 1_000)
   is null;
   --  Not implemented

   overriding
   procedure Receive
      (This    : in out Port;
       Data    : out HAL.SPI.SPI_Data_16b;
       Status  : out HAL.SPI.SPI_Status;
       Timeout : Natural := 1_000)
   is null;
   --  Not implemented

private

   use type Interfaces.C.int;

   type Port
      (Buffer_Length : Natural)
   is new HAL.SPI.SPI_Port with record
      Buffer : HAL.SPI.SPI_Data_8b (1 .. Buffer_Length);
      First, Last : Natural := 0;
      FD : Interfaces.C.int := -1;
   end record;

end Linux.SPI;
