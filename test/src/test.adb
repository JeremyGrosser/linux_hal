with Ada.Assertions; use Ada.Assertions;
with HAL.GPIO; use HAL.GPIO;
with HAL.SPI; use HAL.SPI;
with Linux.GPIO;
with Linux.SPI;

procedure Test is
begin
   declare
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

   declare
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
end Test;
