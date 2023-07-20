private with GNAT.OS_Lib;
with HAL.I2C; use HAL.I2C;
with HAL; use HAL;

package Linux.I2C is
   type Port is new HAL.I2C.I2C_Port with private;

   type I2C_Address_Size is (Address_Size_7b, Address_Size_10b);

   procedure Open
      (This         : in out Port;
       Filename     : String := "/dev/i2c-0";
       Address_Size : I2C_Address_Size := Address_Size_7b);

   function Is_Open
      (This : Port)
      return Boolean;

   procedure Close
      (This : in out Port);

   overriding
   procedure Master_Transmit
      (This    : in out Port;
       Addr    : I2C_Address;
       Data    : I2C_Data;
       Status  : out I2C_Status;
       Timeout : Natural := 1000)
   with Pre => This.Is_Open and then Data'Length in 1 .. 255;

   overriding
   procedure Master_Receive
      (This    : in out Port;
       Addr    : I2C_Address;
       Data    : out I2C_Data;
       Status  : out I2C_Status;
       Timeout : Natural := 1000)
   with Pre => This.Is_Open and then Data'Length in 1 .. 255;

   overriding
   procedure Mem_Write
      (This          : in out Port;
       Addr          : I2C_Address;
       Mem_Addr      : UInt16;
       Mem_Addr_Size : I2C_Memory_Address_Size;
       Data          : I2C_Data;
       Status        : out I2C_Status;
       Timeout       : Natural := 1000)
   with Pre => This.Is_Open and then Mem_Addr_Size = Memory_Size_8b;

   overriding
   procedure Mem_Read
      (This          : in out Port;
       Addr          : I2C_Address;
       Mem_Addr      : UInt16;
       Mem_Addr_Size : I2C_Memory_Address_Size;
       Data          : out I2C_Data;
       Status        : out I2C_Status;
       Timeout       : Natural := 1000)
   with Pre => This.Is_Open and then Mem_Addr_Size = Memory_Size_8b;

private

   use type GNAT.OS_Lib.File_Descriptor;

   type Port is new HAL.I2C.I2C_Port with record
      FD           : GNAT.OS_Lib.File_Descriptor := -1;
      Address_Size : I2C_Address_Size := Address_Size_7b;
      Slave_Addr   : I2C_Address := 0;
   end record;

end Linux.I2C;
