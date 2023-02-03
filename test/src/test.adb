with Ada.Assertions; use Ada.Assertions;
with HAL.SPI; use HAL.SPI;
with Linux.SPI;

procedure Test is
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
end Test;
