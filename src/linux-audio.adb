pragma Warnings (Off, "* is not referenced");
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Interfaces.C; use Interfaces.C;
with System;

package body Linux.Audio is
   use HAL;

   App_Name    : constant chars_ptr := New_String ("linux_hal"); --  leak
   Format_Name : constant chars_ptr := New_String ("s16le"); --  leak

   package PulseAudio is
      type Stream_Direction is (No_Direction, Stream_Playback, Stream_Record, Stream_Upload);

      type Sample_Spec is record
         Format   : int;
         Rate     : UInt32;
         Channels : UInt8;
      end record;

      --  type pa_channel_map_opaque is null record;
      --  type Channel_Map is access all pa_channel_map_opaque;

      --  type pa_buffer_attr_opaque is null record;
      --  type Buffer_Attr is access all pa_buffer_attr_opaque;

      function Create
         (server        : chars_ptr;
          name          : chars_ptr;
          dir           : Stream_Direction;
          dev           : chars_ptr;
          stream_name   : chars_ptr;
          ss            : access Sample_Spec;
          map           : System.Address;
          attr          : System.Address;
          error         : access int)
          return PA_Stream
      with Import, Convention => C, External_Name => "pa_simple_new";

      procedure Free
         (S : PA_Stream)
      with Import, Convention => C, External_Name => "pa_simple_free";

      function Write
         (S     : PA_Stream;
          Data  : HAL.Audio.Audio_Buffer;
          Bytes : size_t;
          Error : access int)
          return int
      with Import, Convention => C, External_Name => "pa_simple_write";

      function Read
         (S       : PA_Stream;
          Data    : out HAL.Audio.Audio_Buffer;
          Bytes   : size_t;
          Error   : access int)
          return int
      with Import, Convention => C, External_Name => "pa_simple_read";

      function Drain
         (S     : PA_Stream;
          Error : access int)
          return int
      with Import, Convention => C, External_Name => "pa_simple_drain";

      function Strerror
         (Error : int)
         return chars_ptr
      with Import, Convention => C, External_Name => "pa_strerror";

      function Parse_Sample_Format
         (Format : chars_ptr)
         return int
      with Import, Convention => C, External_Name => "pa_parse_sample_format";
   end PulseAudio;

   function Initialized
      (This : Audio_Stream)
      return Boolean
   is (This.TX /= null or else This.RX /= null);

   procedure Initialize
      (This : in out Audio_Stream;
       Dir  : PulseAudio.Stream_Direction)
   is
      use type PulseAudio.Stream_Direction;
      Spec        : aliased PulseAudio.Sample_Spec;
      Stream_Name : chars_ptr := New_String (Dir'Image);
      Status      : aliased int;
      P           : PA_Stream;
   begin
      Spec.Format := PulseAudio.Parse_Sample_Format (Format_Name);
      Spec.Rate := This.Rate;
      Spec.Channels := This.Channels;

      P := PulseAudio.Create
         (server        => Null_Ptr,
          name          => App_Name,
          dir           => Dir,
          dev           => Null_Ptr,
          stream_name   => Stream_Name,
          ss            => Spec'Access,
          map           => System.Null_Address,
          attr          => System.Null_Address,
          error         => Status'Access);

      if Status /= 0 then
         declare
            Message : constant chars_ptr := PulseAudio.Strerror (Status);
         begin
            if Message /= Null_Ptr then
               raise Audio_Error with Value (PulseAudio.Strerror (Status));
            else
               raise Audio_Error;
            end if;
         end;
      end if;

      if Dir = PulseAudio.Stream_Playback then
         This.TX := P;
      else
         This.RX := P;
      end if;

      Free (Stream_Name);
   end Initialize;

   procedure Finalize
      (This : in out Audio_Stream)
   is
   begin
      if This.TX /= null then
         PulseAudio.Free (This.TX);
         This.TX := null;
      end if;

      if This.RX /= null then
         PulseAudio.Free (This.RX);
         This.RX := null;
      end if;
   end Finalize;

   procedure Set_Channels
      (This : in out Audio_Stream;
       Channels : HAL.UInt8)
   is
   begin
      This.Channels := Channels;
   end Set_Channels;

   overriding
   procedure Set_Frequency
      (This      : in out Audio_Stream;
       Frequency : HAL.Audio.Audio_Frequency)
   is
   begin
      Set_Frequency (This, UInt32 (HAL.Audio.Audio_Frequency'Enum_Rep (Frequency)));
   end Set_Frequency;

   procedure Set_Frequency
      (This       : in out Audio_Stream;
       Frequency  : HAL.UInt32)
   is
   begin
      This.Rate := Frequency;
   end Set_Frequency;

   overriding
   procedure Transmit
      (This : in out Audio_Stream;
       Data : HAL.Audio.Audio_Buffer)
   is
      Status : int;
   begin
      if This.TX = null then
         Initialize (This, PulseAudio.Stream_Playback);
      end if;

      Status := PulseAudio.Write (This.TX, Data, size_t (Data'Size / 8), null);
      if Status /= 0 then
         raise Audio_Error;
      end if;
   end Transmit;

   procedure Drain
      (This : in out Audio_Stream)
   is
      Status : int;
   begin
      if This.TX = null then
         return;
      end if;

      Status := PulseAudio.Drain (This.TX, null);
      if Status /= 0 then
         raise Audio_Error;
      end if;
   end Drain;

   overriding
   procedure Receive
      (This : in out Audio_Stream;
       Data : out HAL.Audio.Audio_Buffer)
   is
      Status : int;
   begin
      if This.RX = null then
         Initialize (This, PulseAudio.Stream_Record);
      end if;

      Status := PulseAudio.Read (This.RX, Data, size_t (Data'Size / 8), null);
      if Status /= 0 then
         raise Audio_Error;
      end if;
   end Receive;

end Linux.Audio;
