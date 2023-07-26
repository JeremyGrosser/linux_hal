with Interfaces;
with GNAT.OS_Lib;
with System;

package body Linux.SMBus is
   subtype s32 is Interfaces.Integer_32;
   subtype int is Interfaces.C.int;

   procedure Open
      (This     : in out Port;
       Filename : String := "/dev/i2c-1")
   is
   begin
      This.FD := int (GNAT.OS_Lib.Open_Read_Write (Filename, GNAT.OS_Lib.Binary));
      if This.FD < 0 then
         raise SMBus_Error with "Open failed: " & Filename;
      end if;
   end Open;

   procedure Close
      (This : in out Port)
   is
   begin
      GNAT.OS_Lib.Close (GNAT.OS_Lib.File_Descriptor (This.FD));
      This.FD := -1;
   end Close;

   procedure Check
      (Err : s32)
   is
      use type Interfaces.Integer_32;
   begin
      if Err < 0 then
         raise SMBus_Error with GNAT.OS_Lib.Errno_Message (Integer (Err));
      end if;
   end Check;

   procedure Set_Address
      (This    : in out Port;
       Address : Target_Address)
   is
      function linux_i2c_set_slave_address (fd : int; addr : int) return int
         with Import, Convention => C, External_Name => "linux_i2c_set_slave_address";
   begin
      if This.Address /= int (Address) then
         Check (s32 (linux_i2c_set_slave_address (This.FD, int (Address))));
         This.Address := int (Address);
      end if;
   end Set_Address;

   procedure Set_PEC
      (This    : in out Port;
       Enabled : Boolean)
   is
      function linux_i2c_set_pec (fd : int; enabled : int) return int
         with Import, Convention => C, External_Name => "linux_i2c_set_pec";
   begin
      if Enabled /= This.PEC then
         Check (s32 (linux_i2c_set_pec (This.FD, (if Enabled then 1 else 0))));
         This.PEC := Enabled;
      end if;
   end Set_PEC;

   procedure Quick_Command
      (This : in out Port;
       RW   : Boolean)
   is
      function i2c_smbus_write_quick (file : int; value : UInt8) return s32
         with Import, Convention => C, External_Name => "i2c_smbus_write_quick";
      Data : UInt8;
   begin
      Data := Shift_Left (UInt8 (This.Address), 1);
      if RW then
         Data := Data or 1;
      end if;
      Check (i2c_smbus_write_quick (This.FD, Data));
   end Quick_Command;

   procedure Send_Byte
      (This : in out Port;
       Data : UInt8)
   is
      function i2c_smbus_write_byte (file : int; value : UInt8) return s32
         with Import, Convention => C, External_Name => "i2c_smbus_write_byte";
   begin
      Check (i2c_smbus_write_byte (This.FD, Data));
   end Send_Byte;

   procedure Receive_Byte
      (This : in out Port;
       Data : out UInt8)
   is
      function i2c_smbus_read_byte (file : int) return s32
         with Import, Convention => C, External_Name => "i2c_smbus_read_byte";
      Result : s32;
   begin
      Result := i2c_smbus_read_byte (This.FD);
      Check (Result);
      Data := UInt8 (Result);
   end Receive_Byte;

   procedure Write_Byte
      (This    : in out Port;
       Command : UInt8;
       Data    : UInt8)
   is
      function i2c_smbus_write_byte_data (file : int; command : UInt8; value : UInt8) return s32
         with Import, Convention => C, External_Name => "i2c_smbus_write_byte_data";
   begin
      Check (i2c_smbus_write_byte_data (This.FD, Command, Data));
   end Write_Byte;

   procedure Write_Word
      (This    : in out Port;
       Command : UInt8;
       Data    : UInt16)
   is
      function i2c_smbus_write_word_data (file : int; command : UInt8; value : UInt16) return s32
         with Import, Convention => C, External_Name => "i2c_smbus_write_word_data";
   begin
      Check (i2c_smbus_write_word_data (This.FD, Command, Data));
   end Write_Word;

   procedure Read_Byte
      (This    : in out Port;
       Command : UInt8;
       Data    : out UInt8)
   is
      function i2c_smbus_read_byte_data (file : int; command : UInt8) return s32
         with Import, Convention => C, External_Name => "i2c_smbus_read_byte_data";
      Result : s32;
   begin
      Result := i2c_smbus_read_byte_data (This.FD, Command);
      Check (Result);
      Data := UInt8 (Result);
   end Read_Byte;

   procedure Read_Word
      (This    : in out Port;
       Command : UInt8;
       Data    : out UInt16)
   is
      function i2c_smbus_read_word_data (file : int; command : UInt8) return s32
         with Import, Convention => C, External_Name => "i2c_smbus_read_word_data";
      Result : s32;
   begin
      Result := i2c_smbus_read_word_data (This.FD, Command);
      Check (Result);
      Data := UInt16 (Result);
   end Read_Word;

   procedure Process_Call
      (This    : in out Port;
       Command : UInt8;
       Data    : UInt16;
       Result  : out UInt16)
   is
      function i2c_smbus_process_call (file : int; command : UInt8; value : UInt16) return s32
         with Import, Convention => C, External_Name => "i2c_smbus_process_call";
      R : s32;
   begin
      R := i2c_smbus_process_call (This.FD, Command, Data);
      Check (R);
      Result := UInt16 (R);
   end Process_Call;

   procedure Block_Write
      (This    : in out Port;
       Command : UInt8;
       Data    : UInt8_Array)
   is
      function i2c_smbus_write_block_data
         (file : int; command : UInt8; length : UInt8; values : UInt8_Array) return s32
         with Import, Convention => C, External_Name => "i2c_smbus_write_block_data";
   begin
      Check (i2c_smbus_write_block_data (This.FD, Command, UInt8 (Data'Length), Data));
   end Block_Write;

   procedure Block_Read
      (This    : in out Port;
       Command : UInt8;
       Data    : out UInt8_Array;
       Last    : out Natural)
   is
      function i2c_smbus_read_block_data
         (file : int; command : UInt8; values : System.Address) return s32
         with Import, Convention => C, External_Name => "i2c_smbus_read_block_data";
      Buffer : UInt8_Array (1 .. 32);
      Length : s32;
   begin
      Length := i2c_smbus_read_block_data (This.FD, Command, Buffer'Address);
      Check (Length);
      Last := Data'First + Natural (Length) - 1;
      Data (Data'First .. Last) := Buffer (1 .. Natural (Length));
   end Block_Read;

   procedure Block_RW_Process_Call
      (This    : in out Port;
       Command : UInt8;
       Write   : UInt8_Array;
       Read    : out UInt8_Array)
   is
      function i2c_smbus_block_process_call
         (file : int; command : UInt8; length : UInt8; values : in out UInt8_Array) return s32
         with Import, Convention => C, External_Name => "i2c_smbus_block_process_call";
      Data : UInt8_Array (1 .. 32);
      Result : s32;
   begin
      Data (1 .. Write'Length) := Write;
      Result := i2c_smbus_block_process_call (This.FD, Command, Write'Length, Data);
      Check (Result);
      if Natural (Result) /= Read'Length then
         raise SMBus_Error with "Device returned " & Result'Image & " bytes, expected " & Read'Length'Image;
      end if;
      Read := Data (1 .. Read'Length);
   end Block_RW_Process_Call;

end Linux.SMBus;
