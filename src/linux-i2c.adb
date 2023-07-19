with GNAT.OS_Lib;
with Interfaces;

package body Linux.I2C is
   subtype s32 is Interfaces.Integer_32;
   use type Interfaces.Integer_32;

   subtype int is Interfaces.C.int;

   function i2c_smbus_write_byte
      (file  : int;
       value : UInt8)
       return s32
   with Import, Convention => C, External_Name => "i2c_smbus_write_byte";

   function i2c_smbus_read_byte
      (file : int)
       return s32
   with Import, Convention => C, External_Name => "i2c_smbus_read_byte";

   procedure Check
      (Errno  : s32;
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
      function linux_i2c_set_slave_address
         (file : int; addr : int)
         return int
      with Import, Convention => C, External_Name => "linux_i2c_set_slave_address";

      A     : int;
      Errno : int;
   begin
      if This.Address_Size = Address_Size_7b then
         A := int (Addr and 16#7F#);
      else
         A := int (Addr and 16#3FF#);
      end if;

      if This.Slave_Addr /= Addr then
         Errno := linux_i2c_set_slave_address (This.FD, A);
         Check (s32 (Errno), Status);
         if Status = Ok then
            This.Slave_Addr := Addr;
         end if;
      end if;
   end Set_Slave_Address;

   procedure Open
      (This          : in out Port;
       Filename      : String := "/dev/i2c-0";
       Address_Size  : I2C_Address_Size := Address_Size_7b)
   is
   begin
      This.FD := int (GNAT.OS_Lib.Open_Read_Write (Filename, GNAT.OS_Lib.Binary));
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
      GNAT.OS_Lib.Close (GNAT.OS_Lib.File_Descriptor (This.FD));
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
      Errno : s32;
   begin
      Set_Slave_Address (This, Addr, Status);
      if Status /= Ok then
         return;
      end if;

      for D of Data loop
         Errno := i2c_smbus_write_byte (This.FD, D);
         exit when Errno < 0;
      end loop;

      Check (Errno, Status);
   end Master_Transmit;

   overriding
   procedure Master_Receive
      (This    : in out Port;
       Addr    : I2C_Address;
       Data    : out I2C_Data;
       Status  : out I2C_Status;
       Timeout : Natural := 1000)
   is
      Value : s32;
   begin
      Set_Slave_Address (This, Addr, Status);
      if Status /= Ok then
         return;
      end if;

      for I in Data'Range loop
         Value := i2c_smbus_read_byte (This.FD);
         exit when Value < 0;
         Data (I) := UInt8 (Value);
      end loop;
      Check (Value, Status);
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
   is null;

   overriding
   procedure Mem_Read
      (This          : in out Port;
       Addr          : I2C_Address;
       Mem_Addr      : UInt16;
       Mem_Addr_Size : I2C_Memory_Address_Size;
       Data          : out I2C_Data;
       Status        : out I2C_Status;
       Timeout       : Natural := 1000)
   is null;
end Linux.I2C;
