with Ada.Assertions; use Ada.Assertions;
with Ada.Calendar.Formatting;
with Ada.Calendar;
with Ada.Text_IO;
with Ada.Command_Line;
with HAL.GPIO;
with HAL.SPI;
with HAL.I2C;
with HAL;
with Linux.GPIO;
with Linux.SPI;
with Linux.I2C;

procedure Test is
   DS3231_Addr : constant HAL.I2C.I2C_Address := 2#1101000#;

   function From_BCD
      (N : HAL.UInt8)
      return Natural
   is
      use HAL;
      X : Natural;
   begin
      X := Natural (N and 16#0F#);
      X := X + Natural (Shift_Right (N, 4)) * 10;
      return X;
   end From_BCD;

   function To_BCD
      (N : Natural)
      return HAL.UInt8
   is
      use HAL;
   begin
      return Shift_Left (UInt8 (N / 10), 4) or UInt8 (N mod 10);
   end To_BCD;

   procedure Set_RTC
      (Port : in out Linux.I2C.Port;
       T    : Ada.Calendar.Time)
   is
      package ACF renames Ada.Calendar.Formatting;
      use HAL;
      use HAL.I2C;

      Data   : I2C_Data (0 .. 6);
      Status : I2C_Status;
   begin
      Data (0) := To_BCD (ACF.Second (T));
      Data (1) := To_BCD (ACF.Minute (T));
      Data (2) := To_BCD (ACF.Hour (T));
      Data (3) := UInt8 (ACF.Day_Name'Pos (ACF.Day_Of_Week (T)));
      Data (4) := To_BCD (ACF.Day (T));
      Data (5) := To_BCD (ACF.Month (T));
      Data (6) := To_BCD (ACF.Year (T) mod 100);
      Port.Mem_Write
         (Addr          => DS3231_Addr,
          Mem_Addr      => 0,
          Mem_Addr_Size => HAL.I2C.Memory_Size_8b,
          Data          => Data,
          Status        => Status);
   end Set_RTC;

   function Get_RTC
      (Port : in out Linux.I2C.Port)
      return Ada.Calendar.Time
   is
      use Ada.Calendar.Formatting;
      use Ada.Calendar;
      use HAL.I2C;
      Data     : I2C_Data (0 .. 6);
      Status   : I2C_Status;
   begin
      Port.Mem_Read
         (Addr          => DS3231_Addr,
          Mem_Addr      => 0,
          Mem_Addr_Size => Memory_Size_8b,
          Data          => Data,
          Status        => Status);
      return Time_Of
         (Year    => Year_Number (From_BCD (Data (6)) + 2000),
          Month   => Month_Number (From_BCD (Data (5))),
          Day     => Day_Number (From_BCD (Data (4))),
          Hour    => Hour_Number (From_BCD (Data (2))),
          Minute  => Minute_Number (From_BCD (Data (1))),
          Second  => Second_Number (From_BCD (Data (0))));
   end Get_RTC;

   package Cmd renames Ada.Command_Line;
   package Log renames Ada.Text_IO;
   Test_GPIO, Test_SPI, Test_I2C : Boolean := False;
begin
   if Cmd.Argument_Count = 0 then
      Log.Put ("Usage: ");
      Log.Put (Cmd.Command_Name);
      Log.Put (" (gpio|spi|i2c)");
      Log.New_Line;
      Cmd.Set_Exit_Status (1);
      return;
   end if;

   for I in 1 .. Cmd.Argument_Count loop
      declare
         Arg : constant String := Cmd.Argument (I);
      begin
         if Arg = "gpio" then
            Test_GPIO := True;
         elsif Arg = "spi" then
            Test_SPI := True;
         elsif Arg = "i2c" then
            Test_I2C := True;
         else
            Log.Put ("Unknown test: """);
            Log.Put (Cmd.Argument (I));
            Log.Put ("""");
            Log.New_Line;
            Cmd.Set_Exit_Status (2);
            return;
         end if;
      end;
   end loop;

   if Test_GPIO then
      declare
         use HAL.GPIO;
         CHIP_0 : constant Linux.GPIO.Chip := Linux.GPIO.Open ("/dev/gpiochip0");
         GP25   : Linux.GPIO.GPIO_Point := Linux.GPIO.Find (CHIP_0, "GPIO25");
      begin
         GP25.Set_Mode (Output);
         GP25.Set_Pull_Resistor (Pull_Up);

         GP25.Set;
         delay 1.0;
         GP25.Clear;
         delay 1.0;
      end;
   end if;

   if Test_SPI then
      declare
         use HAL.SPI;
         Port   : aliased Linux.SPI.Port (Buffer_Length => 4);
         Data   : SPI_Data_8b (1 .. 4) := (others => 16#AA#);
         Status : SPI_Status;
      begin
         Port.Open ("/dev/spidev0.0");
         Port.Set_Max_Speed (10_000_000);

         Port.Transmit (Data, Status);
         Assert (Status = Ok);
         Port.Receive (Data, Status);
         Assert (Status = Ok);
         Port.Close;
      end;
   end if;

   if Test_I2C then
      declare
         use HAL.I2C;
         Port   : aliased Linux.I2C.Port;
         Addr   : constant I2C_Address := 2#1101000#; --  DS3231
         Data   : I2C_Data (1 .. 1);
         Status : I2C_Status;

         T : Ada.Calendar.Time;
      begin
         Port.Open ("/dev/i2c-1");
         if not Port.Is_Open then
            Log.Put_Line ("Opening i2c bus device failed");
            Cmd.Set_Exit_Status (1);
            return;
         end if;

         Port.Mem_Read
            (Addr          => Addr,
             Mem_Addr      => 16#00#,
             Mem_Addr_Size => Memory_Size_8b,
             Data          => Data,
             Status        => Status);

         if Status /= Ok then
            Log.Put_Line ("I2C Mem_Read failed");
            Cmd.Set_Exit_Status (1);
            return;
         end if;

         T := Get_RTC (Port);
         Log.Put ("Before Set: ");
         Log.Put (Ada.Calendar.Formatting.Image (T));
         Log.New_Line;

         T := Ada.Calendar.Clock;
         Set_RTC (Port, T);
         Log.Put ("Set:        ");
         Log.Put (Ada.Calendar.Formatting.Image (T));
         Log.New_Line;

         T := Get_RTC (Port);
         Log.Put ("Get:        ");
         Log.Put (Ada.Calendar.Formatting.Image (T));
         Log.New_Line;

         Port.Close;
      end;
   end if;
end Test;
