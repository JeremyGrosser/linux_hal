--
--  Copyright (C) 2023 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
with Interfaces.C;
with GNAT.OS_Lib;

package body Linux.GPIO is

   Consumer_Name : aliased constant String := "linux_hal";

   procedure Reserve
      (This : in out GPIO_Point)
   is
      use Interfaces.C;

      function gpiod_line_request_input
         (l : Line;
          consumer : String)
          return int
      with Import, Convention => C, External_Name => "gpiod_line_request_input";
   begin
      if gpiod_line_request_input (This.L, Consumer_Name) /= 0 then
         raise Program_Error with "Error reserving GPIO line: " & GNAT.OS_Lib.Errno_Message;
      end if;
   end Reserve;

   procedure Release
      (This : in out GPIO_Point)
   is
      procedure gpiod_line_release (l : Line)
         with Import, Convention => C, External_Name => "gpiod_line_release";
   begin
      gpiod_line_release (This.L);
   end Release;

   function Find
      (This : Chip;
       Name : String)
       return GPIO_Point
   is
      function gpiod_chip_find_line
         (c : Chip;
          n : String)
          return Line
      with Import, Convention => C, External_Name => "gpiod_chip_find_line";

      Point : GPIO_Point := (This, gpiod_chip_find_line (This, Name));
   begin
      if Point.L = null then
         raise Program_Error with "No GPIO pin named " & Name & ", run `gpioinfo` for a list of available pins";
      else
         Reserve (Point);
         return Point;
      end if;
   end Find;

   function Get_Point
      (This : Chip;
       Num  : Natural)
       return GPIO_Point
   is
      function gpiod_chip_get_line
         (c : Chip;
          n : Interfaces.C.unsigned)
          return Line
      with Import, Convention => C, External_Name => "gpiod_chip_get_line";

      Point : GPIO_Point := (This, gpiod_chip_get_line (This, Interfaces.C.unsigned (Num)));
   begin
      if Point.L = null then
         raise Program_Error with "No GPIO pin number " & Num'Image & ", run `gpioinfo` for a list of available pins";
      else
         Reserve (Point);
         return Point;
      end if;
   end Get_Point;

   overriding
   function Support
      (This : GPIO_Point;
       Capa : HAL.GPIO.Capability)
       return Boolean
   is (True);

   overriding
   function Mode
      (This : GPIO_Point)
      return HAL.GPIO.GPIO_Mode
   is
      use type Interfaces.C.int;
      function gpiod_line_direction (l : Line) return Interfaces.C.int
         with Import, Convention => C, External_Name => "gpiod_line_direction";
   begin
      return (if gpiod_line_direction (This.L) = 1 then HAL.GPIO.Input else HAL.GPIO.Output);
   end Mode;

   overriding
   procedure Set_Mode
      (This : in out GPIO_Point;
       Mode : HAL.GPIO.GPIO_Config_Mode)
   is
      use type HAL.GPIO.GPIO_Config_Mode;
      use type Interfaces.C.int;
      function gpiod_line_set_direction_input
         (l : Line)
          return Interfaces.C.int
      with Import, Convention => C, External_Name => "gpiod_line_set_direction_input";

      function gpiod_line_set_direction_output
         (l : Line)
          return Interfaces.C.int
      with Import, Convention => C, External_Name => "gpiod_line_set_direction_output";
   begin
      if Mode = HAL.GPIO.Input then
         if gpiod_line_set_direction_input (This.L) /= 0 then
            raise Program_Error with "Error setting GPIO Input direction: " & GNAT.OS_Lib.Errno_Message;
         end if;
      elsif Mode = HAL.GPIO.Output then
         if gpiod_line_set_direction_output (This.L) /= 0 then
            raise Program_Error with "Error setting GPIO Output direction: " & GNAT.OS_Lib.Errno_Message;
         end if;
      end if;
   end Set_Mode;

   overriding
   function Pull_Resistor
      (This : GPIO_Point)
      return HAL.GPIO.GPIO_Pull_Resistor
   is
      function gpiod_line_bias
         (l : Line)
         return Interfaces.C.int
      with Import, Convention => C, External_Name => "gpiod_line_bias";
   begin
      case gpiod_line_bias (This.L) is
         when 1 => return HAL.GPIO.Floating; --  Unknown bias, assume floating
         when 2 => return HAL.GPIO.Floating;
         when 3 => return HAL.GPIO.Pull_Up;
         when 4 => return HAL.GPIO.Pull_Down;
         when others => raise Program_Error with "Invalid line bias from gpiod";
      end case;
   end Pull_Resistor;

   overriding
   procedure Set_Pull_Resistor
      (This : in out GPIO_Point;
       Pull : HAL.GPIO.GPIO_Pull_Resistor)
   is
      use Interfaces.C;
      use HAL.GPIO;

      function gpiod_line_set_flags
         (l : Line;
          flags : int)
          return int
      with Import, Convention => C, External_Name => "gpiod_line_set_flags";

      Flags : int;
   begin
      case Pull is
         --  Open_Drain  => Flags := 2#0000_0001#;
         --  Open_Source => Flags := 2#0000_0010#;
         --  Active_Low  => Flags := 2#0000_0100#;
         when Floating  => Flags := 2#0000_1000#;
         when Pull_Down => Flags := 2#0001_0000#;
         when Pull_Up   => Flags := 2#0010_0000#;
      end case;

      if gpiod_line_set_flags (This.L, Flags) /= 0 then
         raise Program_Error with "Error setting GPIO flags: " & GNAT.OS_Lib.Errno_Message;
      end if;
   end Set_Pull_Resistor;

   overriding
   function Set
      (This : GPIO_Point)
      return Boolean
   is
      function gpiod_line_get_value
         (l : Line)
         return Interfaces.C.int
      with Import, Convention => C, External_Name => "gpiod_line_get_value";
   begin
      case gpiod_line_get_value (This.L) is
         when 0 => return False;
         when 1 => return True;
         when others =>
            raise Program_Error with "Error getting GPIO value: " & GNAT.OS_Lib.Errno_Message;
      end case;
   end Set;

   function gpiod_line_set_value
      (l : Line;
       value : Interfaces.C.int)
       return Interfaces.C.int
   with Import, Convention => C, External_Name => "gpiod_line_set_value";

   overriding
   procedure Set
      (This : in out GPIO_Point)
   is
      use type Interfaces.C.int;
   begin
      if gpiod_line_set_value (This.L, 1) /= 0 then
         raise Program_Error with "Error setting GPIO: " & GNAT.OS_Lib.Errno_Message;
      end if;
   end Set;

   overriding
   procedure Clear
      (This : in out GPIO_Point)
   is
      use type Interfaces.C.int;
   begin
      if gpiod_line_set_value (This.L, 0) /= 0 then
         raise Program_Error with "Error clearing GPIO: " & GNAT.OS_Lib.Errno_Message;
      end if;
   end Clear;

   overriding
   procedure Toggle
      (This : in out GPIO_Point)
   is
   begin
      if This.Set then
         This.Clear;
      else
         This.Set;
      end if;
   end Toggle;

end Linux.GPIO;
