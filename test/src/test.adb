with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO;
with Ada.Command_Line;
with HAL.GPIO;
with HAL.SPI;
with HAL.I2C;
with Linux.GPIO;
with Linux.SPI;
with Linux.I2C;

procedure Test is
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
         Addr   : constant I2C_Address := 16#3C#;
         Data   : I2C_Data (1 .. 1);
         Status : I2C_Status;
      begin
         Port.Open ("/dev/i2c-1");
         Assert (Port.Is_Open);

         Port.Master_Receive (Addr, Data, Status);
         Assert (Status = Ok);
         Log.Put ("I2C read OK: ");
         Log.Put (Data (1)'Image);
         Log.New_Line;
      end;
   end if;
end Test;
