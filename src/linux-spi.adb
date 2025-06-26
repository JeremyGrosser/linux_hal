--
--  Copyright (C) 2023 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
with Interfaces; use Interfaces;
with System.Storage_Elements;
with System;
with GNAT.OS_Lib;

package body Linux.SPI
   with Preelaborate
is
   use Interfaces.C;

   --  /usr/include/linux/spi/spi.h
   type spi_ioc_transfer is record
      tx_buf            : Unsigned_64 := 0; --  Pointer to userspace transmit buffer
      rx_buf            : Unsigned_64 := 0; --  Pointer to userspace receive buffer

      len               : Unsigned_32 := 0; --  Length of tx and rx buffers in bytes
      speed_hz          : Unsigned_32 := 0; --  Bitrate

      delay_usecs       : Unsigned_16 := 0; --  If nonzero, delay after the last bit transfer before toggling CS
      bits_per_word     : Unsigned_8 := 0;  --  Word size
      cs_change         : Unsigned_8 := 0;  --  Override 1 if CS should be deasserted before the next transfer
      tx_nbits          : Unsigned_8 := 0;  --  Override transmit bits per clock
      rx_nbits          : Unsigned_8 := 0;  --  Overrride receive bits per clock
      word_delay_usecs  : Unsigned_8 := 0;  --  If nonzero, delay between words within a transfer
      pad               : Unsigned_8 := 0;
   end record
      with Convention => C_Pass_By_Copy;

   type spi_ioc_transfer_array is array (Unsigned_8 range <>) of spi_ioc_transfer
      with Convention => C;

   --  linux_ioctl_wrap.c
   function linux_spi_ioc_message
      (fd  : int;
       len : Unsigned_8;
       msg : spi_ioc_transfer_array)
       return int
   with Import, Convention => C, External_Name => "linux_spi_ioc_message";

   function linux_spi_set_max_speed
      (fd : int;
       hz : Unsigned_32)
       return int
   with Import, Convention => C, External_Name => "linux_spi_set_max_speed";

   function linux_spi_get_max_speed
      (fd : int;
       hz : not null access Unsigned_32)
       return int
   with Import, Convention => C, External_Name => "linux_spi_get_max_speed";

   function linux_spi_set_mode
      (fd   : int;
       mode : Unsigned_8)
       return int
   with Import, Convention => C, External_Name => "linux_spi_set_mode";

   procedure Open
      (This     : in out Port;
       Filename : String)
   is
   begin
      This.FD := int (GNAT.OS_Lib.Open_Read_Write (Filename, GNAT.OS_Lib.Binary));
      if This.FD < 0 then
         raise Program_Error with "Error opening " & Filename;
      end if;
   end Open;

   function Is_Open
      (This : Port)
      return Boolean
   is
   begin
      return This.FD >= 0;
   end Is_Open;

   procedure Close
      (This : in out Port)
   is
   begin
      GNAT.OS_Lib.Close (GNAT.OS_Lib.File_Descriptor (This.FD));
   end Close;

   function Available
      (This : Port)
      return Natural
   is (This.Last - This.First);

   procedure Set_Max_Speed
      (This : in out Port;
       Hz   : Natural)
   is
   begin
      if linux_spi_set_max_speed (This.FD, Unsigned_32 (Hz)) /= 0 then
         raise Program_Error with "Error setting SPI max speed";
      end if;
   end Set_Max_Speed;

   function Max_Speed
      (This : Port)
      return Natural
   is
      Speed : aliased Unsigned_32;
   begin
      if linux_spi_get_max_speed (This.FD, Speed'Access) /= 0 then
         raise Program_Error with "Error reading SPI max speed";
      else
         return Natural (Speed);
      end if;
   end Max_Speed;

   procedure Set_Mode
      (This : in out Port;
       Mode : SPI_Mode)
   is
      Result : int;
   begin
      Result := linux_spi_set_mode (This.FD, SPI_Mode'Pos (Mode));
      if Result /= 0 then
         raise Program_Error with "Error setting SPI mode";
      end if;
   end Set_Mode;

   overriding
   function Data_Size
      (This : Port)
      return HAL.SPI.SPI_Data_Size
   is (HAL.SPI.Data_Size_8b);

   procedure Transfer
      (This    : in out Port;
       Data    : in out HAL.SPI.SPI_Data_8b;
       Status  : out HAL.SPI.SPI_Status)
   is
      package SSE renames System.Storage_Elements;
      Transfer : constant spi_ioc_transfer_array (1 .. 1) := (1 =>
         (tx_buf => Unsigned_64 (SSE.To_Integer (Data'Address)),
          rx_buf => Unsigned_64 (SSE.To_Integer (Data'Address)),
          len    => Unsigned_32 (Data'Length),
          others => <>));
      Result : int;
   begin
      Result := linux_spi_ioc_message
         (fd  => This.FD,
          len => Unsigned_8 (Transfer'Length),
          msg => Transfer);
      if Result < 0 then
         Status := HAL.SPI.Err_Error;
      else
         Status := HAL.SPI.Ok;
      end if;
   end Transfer;

   overriding
   procedure Transmit
      (This    : in out Port;
       Data    : HAL.SPI.SPI_Data_8b;
       Status  : out HAL.SPI.SPI_Status;
       Timeout : Natural := 1_000)
   is
      package SSE renames System.Storage_Elements;
      Transfer : constant spi_ioc_transfer_array (1 .. 1) := (1 =>
         (tx_buf => Unsigned_64 (SSE.To_Integer (Data'Address)),
          rx_buf => Unsigned_64 (SSE.To_Integer (This.Buffer'Address)),
          len    => Unsigned_32 (Data'Length),
          others => <>));
      Result : int;
   begin
      Result := linux_spi_ioc_message
         (fd  => This.FD,
          len => Unsigned_8 (Transfer'Length),
          msg => Transfer);
      if Result < 0 then
         This.First := 0;
         This.Last := 0;
         Status := HAL.SPI.Err_Error;
      else
         This.First := This.Buffer'First;
         This.Last := Natural (Result);
         Status := HAL.SPI.Ok;
      end if;
   end Transmit;

   overriding
   procedure Receive
      (This    : in out Port;
       Data    : out HAL.SPI.SPI_Data_8b;
       Status  : out HAL.SPI.SPI_Status;
       Timeout : Natural := 1_000)
   is
      First : constant Natural := This.First;
      Last  : constant Natural := First + Data'Length - 1;
   begin
      if This.Last = 0 or else First > This.Last or else Last > This.Last then
         Status := HAL.SPI.Err_Error;
      else
         Data (Data'First .. Data'Last) := This.Buffer (First .. Last);
         This.First := Last + 1;
         Status := HAL.SPI.Ok;
      end if;
   end Receive;

end Linux.SPI;
