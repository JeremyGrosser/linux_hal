with Interfaces.C;

package body Linux.I2C is
   subtype int is Interfaces.C.int;
   use type Interfaces.C.int;

   subtype s32 is Interfaces.Integer_32;
   use type Interfaces.Integer_32;

   --  linux_i2c functions wrap ioctls
   function linux_i2c_set_slave_address
      (file : int; addr : int)
      return int
   with Import, Convention => C, External_Name => "linux_i2c_set_slave_address";

   procedure linux_i2c_set_timeout
      (fd : int; ms : int)
   with Import, Convention => C, External_Name => "linux_i2c_set_timeout";

   function linux_i2c_set_tenbit_addressing
      (file : int; enabled : int)
      return int
   with Import, Convention => C, External_Name => "linux_i2c_set_tenbit_addressing";

   --  i2c_smbus functions are implemented by libi2c from i2c-tools. They also wrap ioctls.
   function i2c_smbus_write_i2c_block_data
      (file    : int;
       command : UInt8;
       length  : UInt8;
       values  : I2C_Data)
       return s32
   with Import, Convention => C, External_Name => "i2c_smbus_write_i2c_block_data";

   function i2c_smbus_read_i2c_block_data
      (file    : int;
       command : UInt8;
       length  : UInt8;
       values  : out I2C_Data)
       return s32
   with Import, Convention => C, External_Name => "i2c_smbus_read_i2c_block_data";

   procedure Check
      (Errno  : int;
       Status : out I2C_Status)
   is
   begin
      if Errno < 0 then
         Status := Err_Error;
      else
         Status := Ok;
      end if;
   end Check;

   procedure Set_Slave_Address
      (This    : in out Port;
       Addr    : I2C_Address;
       Status  : out I2C_Status)
   is
      Errno : int;
   begin
      Errno := linux_i2c_set_tenbit_addressing
         (int (This.FD), (if This.Address_Size = Address_Size_10b then 1 else 0));
      Check (Errno, Status);
      if Status /= Ok then
         return;
      end if;

      if This.Slave_Addr /= Addr then
         Errno := linux_i2c_set_slave_address (int (This.FD), int (Addr));
         Check (Errno, Status);
         if Status = Ok then
            This.Slave_Addr := Addr;
         end if;
      end if;
   end Set_Slave_Address;

   procedure Set_Timeout
      (This    : Port;
       Timeout : Natural)
   is
   begin
      linux_i2c_set_timeout (int (This.FD), int (Timeout));
   end Set_Timeout;

   procedure Open
      (This          : in out Port;
       Filename      : String := "/dev/i2c-0";
       Address_Size  : I2C_Address_Size := Address_Size_7b)
   is
   begin
      This.FD := GNAT.OS_Lib.Open_Read_Write (Filename, GNAT.OS_Lib.Binary);
      This.Address_Size := Address_Size;
   end Open;

   function Is_Open
      (This : Port)
      return Boolean
   is (This.FD >= 0);

   procedure Close
      (This : in out Port)
   is
   begin
      GNAT.OS_Lib.Close (This.FD);
      This.FD := -1;
   end Close;

   overriding
   procedure Master_Transmit
      (This    : in out Port;
       Addr    : I2C_Address;
       Data    : I2C_Data;
       Status  : out I2C_Status;
       Timeout : Natural := 1000)
   is
      Bytes_Written : Integer;
   begin
      Set_Timeout (This, Timeout);
      Set_Slave_Address (This, Addr, Status);
      if Status /= Ok then
         return;
      end if;

      Bytes_Written := GNAT.OS_Lib.Write (This.FD, Data'Address, Data'Length);
      if Bytes_Written /= Data'Length then
         Status := Err_Error;
      end if;
   end Master_Transmit;

   overriding
   procedure Master_Receive
      (This    : in out Port;
       Addr    : I2C_Address;
       Data    : out I2C_Data;
       Status  : out I2C_Status;
       Timeout : Natural := 1000)
   is
      Bytes_Read : Integer;
   begin
      Set_Timeout (This, Timeout);
      Set_Slave_Address (This, Addr, Status);
      if Status /= Ok then
         return;
      end if;

      Bytes_Read := GNAT.OS_Lib.Read (This.FD, Data'Address, Data'Length);
      if Bytes_Read /= Data'Length then
         Status := Err_Error;
      end if;
   end Master_Receive;

   overriding
   procedure Mem_Write
      (This          : in out Port;
       Addr          : I2C_Address;
       Mem_Addr      : UInt16;
       Mem_Addr_Size : I2C_Memory_Address_Size;
       Data          : I2C_Data;
       Status        : out I2C_Status;
       Timeout       : Natural := 1000)
   is
      Err : s32;
   begin
      Set_Timeout (This, Timeout);
      Set_Slave_Address (This, Addr, Status);
      if Status /= Ok then
         return;
      end if;

      Err := i2c_smbus_write_i2c_block_data
         (file    => int (This.FD),
          command => UInt8 (Mem_Addr),
          length  => UInt8 (Data'Length),
          values  => Data);

      Status := (if Err < 0 then Err_Error else Ok);
   end Mem_Write;

   overriding
   procedure Mem_Read
      (This          : in out Port;
       Addr          : I2C_Address;
       Mem_Addr      : UInt16;
       Mem_Addr_Size : I2C_Memory_Address_Size;
       Data          : out I2C_Data;
       Status        : out I2C_Status;
       Timeout       : Natural := 1000)
   is
      Err : s32;
   begin
      Set_Timeout (This, Timeout);
      Set_Slave_Address (This, Addr, Status);
      if Status /= Ok then
         return;
      end if;

      Err := i2c_smbus_read_i2c_block_data
         (file    => int (This.FD),
          command => UInt8 (Mem_Addr),
          length  => UInt8 (Data'Length),
          values  => Data);

      Status := (if Err < 0 then Err_Error else Ok);
   end Mem_Read;

end Linux.I2C;
