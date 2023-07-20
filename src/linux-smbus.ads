--  System Management Bus (SMBus) Specification, version 3.2
--  http://www.smbus.org/specs/SMBus_3_2_20220112.pdf
private with Interfaces.C;
with HAL; use HAL;

package Linux.SMBus is

   SMBus_Error : exception;

   type Target_Address is new UInt7;

   type Port is tagged private;

   procedure Open
      (This     : in out Port;
       Filename : String := "/dev/i2c-1");

   procedure Close
      (This : in out Port);

   procedure Set_Address
      (This    : in out Port;
       Address : Target_Address);

   procedure Set_PEC
      (This    : in out Port;
       Enabled : Boolean);

   --  6.5 Bus Protocols
   procedure Quick_Command
      (This    : in out Port;
       RW      : Boolean);

   procedure Send_Byte
      (This    : in out Port;
       Data    : UInt8);

   procedure Receive_Byte
      (This    : in out Port;
       Data    : out UInt8);

   procedure Write_Byte
      (This    : in out Port;
       Command : UInt8;
       Data    : UInt8);

   procedure Write_Word
      (This    : in out Port;
       Command : UInt8;
       Data    : UInt16);

   procedure Read_Byte
      (This    : in out Port;
       Command : UInt8;
       Data    : out UInt8);

   procedure Read_Word
      (This    : in out Port;
       Command : UInt8;
       Data    : out UInt16);

   procedure Process_Call
      (This    : in out Port;
       Command : UInt8;
       Data    : UInt16;
       Result  : out UInt16);

   --  The SMBus specification allows block sizes up to 255 bytes, but libi2c
   --  only supports 32 bytes for backwards compatibility reasons.
   procedure Block_Write
      (This    : in out Port;
       Command : UInt8;
       Data    : UInt8_Array)
   with Pre => Data'Length <= 32;

   procedure Block_Read
      (This    : in out Port;
       Command : UInt8;
       Data    : out UInt8_Array)
   with Pre => Data'Length <= 32;

   procedure Block_RW_Process_Call
      (This    : in out Port;
       Command : UInt8;
       Write   : UInt8_Array;
       Read    : out UInt8_Array)
   with Pre => Write'Length + Read'Length <= 32;

   --  6.5.9 .. 6.5.13
   --  Host Notify protocol is not supported by libi2c
   --  32 and 64-bit writes are not supported by libi2c

private

   use type Interfaces.C.int;

   type Port is tagged record
      FD       : Interfaces.C.int := -1;
      PEC      : Boolean := False;
      Address  : Interfaces.C.int := -1;
   end record;

end Linux.SMBus;
