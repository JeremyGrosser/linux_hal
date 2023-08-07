with HAL.Audio;
with HAL;

package Linux.Audio is

   type Audio_Stream is new HAL.Audio.Audio_Stream with private;

   Audio_Error : exception;

   function Initialized
      (This : Audio_Stream)
      return Boolean;
   --  Streams are initialized on the first call to Transmit or Receive.
   --  Initialized can be reset by calling Finalize.

   procedure Set_Channels
      (This     : in out Audio_Stream;
       Channels : HAL.UInt8)
   with Pre => not This.Initialized;

   overriding
   procedure Set_Frequency
      (This      : in out Audio_Stream;
       Frequency : HAL.Audio.Audio_Frequency)
   with Pre => not This.Initialized;

   procedure Set_Frequency
      (This       : in out Audio_Stream;
       Frequency  : HAL.UInt32)
   with Pre => not This.Initialized;

   procedure Finalize
      (This : in out Audio_Stream)
   with Post => not This.Initialized;

   overriding
   procedure Transmit
      (This : in out Audio_Stream;
       Data : HAL.Audio.Audio_Buffer)
   with Post => This.Initialized;

   procedure Drain
      (This : in out Audio_Stream)
   with Pre => This.Initialized;
   --  Wait for transmit buffer to empty.

   overriding
   procedure Receive
      (This : in out Audio_Stream;
       Data : out HAL.Audio.Audio_Buffer)
   with Post => This.Initialized;

private

   type PA_Stream_Opaque is null record;
   type PA_Stream is access all PA_Stream_Opaque;

   type Audio_Stream is new HAL.Audio.Audio_Stream with record
      TX, RX   : PA_Stream := null;
      Channels : HAL.UInt8 := 1;
      Rate     : HAL.UInt32 := 48_000;
   end record;

end Linux.Audio;
