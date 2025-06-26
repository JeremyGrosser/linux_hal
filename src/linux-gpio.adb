--
--  Copyright (C) 2023 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
with Interfaces.C;
with GNAT.OS_Lib;

package body Linux.GPIO is

   Consumer_Name : aliased constant String := "linux_hal";

   --  libgpiod v2 types
   type gpiod_line_settings is null record;
   type Line_Settings is access all gpiod_line_settings;

   type gpiod_line_config is null record;
   type Line_Config is access all gpiod_line_config;

   type gpiod_request_config is null record;
   type Request_Config is access all gpiod_request_config;

   --  libgpiod v2 enums
   type gpiod_line_direction is new Interfaces.C.int;
   GPIOD_LINE_DIRECTION_INPUT : constant gpiod_line_direction := 2;
   GPIOD_LINE_DIRECTION_OUTPUT : constant gpiod_line_direction := 3;

   type gpiod_line_bias is new Interfaces.C.int;
   GPIOD_LINE_BIAS_AS_IS : constant gpiod_line_bias := 1;
   GPIOD_LINE_BIAS_DISABLED : constant gpiod_line_bias := 3;
   GPIOD_LINE_BIAS_PULL_UP : constant gpiod_line_bias := 4;
   GPIOD_LINE_BIAS_PULL_DOWN : constant gpiod_line_bias := 5;

   type gpiod_line_value is new Interfaces.C.int;
   GPIOD_LINE_VALUE_INACTIVE : constant gpiod_line_value := 0;
   GPIOD_LINE_VALUE_ACTIVE : constant gpiod_line_value := 1;

   --  libgpiod v2 configuration functions
   function gpiod_line_settings_new return Line_Settings
      with Import, Convention => C, External_Name => "gpiod_line_settings_new";

   procedure gpiod_line_settings_free (settings : Line_Settings)
      with Import, Convention => C, External_Name => "gpiod_line_settings_free";

   function gpiod_line_settings_set_direction
      (settings : Line_Settings;
       direction : gpiod_line_direction)
       return Interfaces.C.int
      with Import, Convention => C, External_Name => "gpiod_line_settings_set_direction";

   function gpiod_line_settings_set_bias
      (settings : Line_Settings;
       bias : gpiod_line_bias)
       return Interfaces.C.int
      with Import, Convention => C, External_Name => "gpiod_line_settings_set_bias";

   function gpiod_line_config_new return Line_Config
      with Import, Convention => C, External_Name => "gpiod_line_config_new";

   procedure gpiod_line_config_free (config : Line_Config)
      with Import, Convention => C, External_Name => "gpiod_line_config_free";

   function gpiod_line_config_add_line_settings
      (config : Line_Config;
       offsets : access Interfaces.C.unsigned;
       num_offsets : Interfaces.C.size_t;
       settings : Line_Settings)
       return Interfaces.C.int
      with Import, Convention => C, External_Name => "gpiod_line_config_add_line_settings";

   function gpiod_request_config_new return Request_Config
      with Import, Convention => C, External_Name => "gpiod_request_config_new";

   procedure gpiod_request_config_free (config : Request_Config)
      with Import, Convention => C, External_Name => "gpiod_request_config_free";

   procedure gpiod_request_config_set_consumer
      (config : Request_Config;
       consumer : String)
      with Import, Convention => C, External_Name => "gpiod_request_config_set_consumer";

   --  Line request functions
   function gpiod_chip_request_lines
      (c : Chip;
       req_cfg : Request_Config;
       line_cfg : Line_Config)
       return Line_Request
      with Import, Convention => C, External_Name => "gpiod_chip_request_lines";

   procedure gpiod_line_request_release (request : Line_Request)
      with Import, Convention => C, External_Name => "gpiod_line_request_release";

   function gpiod_line_request_get_value
      (request : Line_Request;
       offset : Interfaces.C.unsigned)
       return gpiod_line_value
      with Import, Convention => C, External_Name => "gpiod_line_request_get_value";

   function gpiod_line_request_set_value
      (request : Line_Request;
       offset : Interfaces.C.unsigned;
       value : gpiod_line_value)
       return Interfaces.C.int
      with Import, Convention => C, External_Name => "gpiod_line_request_set_value";

   --  Line finding functions
   function gpiod_chip_get_line_offset_from_name
      (c : Chip;
       name : String)
       return Interfaces.C.int
      with Import, Convention => C, External_Name => "gpiod_chip_get_line_offset_from_name";

   function Create_Line_Request
      (C : Chip;
       Offset : Natural;
       Direction : gpiod_line_direction;
       Bias : gpiod_line_bias := GPIOD_LINE_BIAS_AS_IS)
       return Line_Request
   is
      use type Interfaces.C.int;

      Settings : constant Line_Settings := gpiod_line_settings_new;
      Config : constant Line_Config := gpiod_line_config_new;
      Req_Config : constant Request_Config := gpiod_request_config_new;
      Offset_C : aliased Interfaces.C.unsigned := Interfaces.C.unsigned (Offset);
      Request : Line_Request;
   begin
      if Settings = null or else Config = null or else Req_Config = null then
         raise Program_Error with "Failed to create gpiod configuration objects";
      end if;

      --  Configure settings
      if gpiod_line_settings_set_direction (Settings, Direction) /= 0 then
         gpiod_line_settings_free (Settings);
         gpiod_line_config_free (Config);
         gpiod_request_config_free (Req_Config);
         raise Program_Error with "Failed to set line direction: " & GNAT.OS_Lib.Errno_Message;
      end if;

      if gpiod_line_settings_set_bias (Settings, Bias) /= 0 then
         gpiod_line_settings_free (Settings);
         gpiod_line_config_free (Config);
         gpiod_request_config_free (Req_Config);
         raise Program_Error with "Failed to set line bias: " & GNAT.OS_Lib.Errno_Message;
      end if;

      --  Add line to config
      if gpiod_line_config_add_line_settings (Config, Offset_C'Access, 1, Settings) /= 0 then
         gpiod_line_settings_free (Settings);
         gpiod_line_config_free (Config);
         gpiod_request_config_free (Req_Config);
         raise Program_Error with "Failed to add line settings: " & GNAT.OS_Lib.Errno_Message;
      end if;

      --  Set consumer name
      gpiod_request_config_set_consumer (Req_Config, Consumer_Name);

      --  Create request
      Request := gpiod_chip_request_lines (C, Req_Config, Config);

      --  Clean up config objects (they're copied into the request)
      gpiod_line_settings_free (Settings);
      gpiod_line_config_free (Config);
      gpiod_request_config_free (Req_Config);

      if Request = null then
         raise Program_Error with "Failed to request GPIO line: " & GNAT.OS_Lib.Errno_Message;
      end if;

      return Request;
   end Create_Line_Request;

   procedure Reserve
      (This : in out GPIO_Point)
   is
   begin
      if This.Request = null then
         This.Request := Create_Line_Request (This.C, This.Offset, GPIOD_LINE_DIRECTION_INPUT);
      end if;
   end Reserve;

   procedure Release
      (This : in out GPIO_Point)
   is
   begin
      if This.Request /= null then
         gpiod_line_request_release (This.Request);
         This.Request := null;
      end if;
   end Release;

   function Find
      (This : Chip;
       Name : String)
       return GPIO_Point
   is
      use type Interfaces.C.int;
      Offset : Interfaces.C.int;
   begin
      Offset := gpiod_chip_get_line_offset_from_name (This, Name);
      if Offset < 0 then
         raise Program_Error with "No GPIO pin named " & Name & ", run `gpioinfo` for a list of available pins";
      else
         return Point : GPIO_Point := (This, null, Natural (Offset)) do
            Reserve (Point);
         end return;
      end if;
   end Find;

   function Get_Point
      (This : Chip;
       Num  : Natural)
       return GPIO_Point
   is
   begin
      return Point : GPIO_Point := (This, null, Num) do
         Reserve (Point);
      end return;
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
   begin
      --  In v2 API, we can't query line direction from request, so we track it internally
      --  For now, return Input as default since we can't determine actual mode
      return HAL.GPIO.Input;
   end Mode;

   overriding
   procedure Set_Mode
      (This : in out GPIO_Point;
       Mode : HAL.GPIO.GPIO_Config_Mode)
   is
      use type HAL.GPIO.GPIO_Config_Mode;
      Direction : gpiod_line_direction;
   begin
      if Mode = HAL.GPIO.Input then
         Direction := GPIOD_LINE_DIRECTION_INPUT;
      elsif Mode = HAL.GPIO.Output then
         Direction := GPIOD_LINE_DIRECTION_OUTPUT;
      else
         raise Program_Error with "Unsupported GPIO mode";
      end if;

      --  Release current request and create new one with new direction
      Release (This);
      This.Request := Create_Line_Request (This.C, This.Offset, Direction);
   end Set_Mode;

   overriding
   function Pull_Resistor
      (This : GPIO_Point)
       return HAL.GPIO.GPIO_Pull_Resistor
   is
   begin
      --  In v2 API, we can't query line bias from request, so return default
      return HAL.GPIO.Floating;
   end Pull_Resistor;

   overriding
   procedure Set_Pull_Resistor
      (This : in out GPIO_Point;
       Pull : HAL.GPIO.GPIO_Pull_Resistor)
   is
      use HAL.GPIO;
      Bias : gpiod_line_bias;
      Current_Direction : constant gpiod_line_direction := GPIOD_LINE_DIRECTION_INPUT;
   begin
      case Pull is
         when Floating  => Bias := GPIOD_LINE_BIAS_DISABLED;
         when Pull_Down => Bias := GPIOD_LINE_BIAS_PULL_DOWN;
         when Pull_Up   => Bias := GPIOD_LINE_BIAS_PULL_UP;
      end case;

      --  We need to recreate the request with new bias settings
      --  For simplicity, assume it's an input line
      Release (This);
      This.Request := Create_Line_Request (This.C, This.Offset, Current_Direction, Bias);
   end Set_Pull_Resistor;

   overriding
   function Set
      (This : GPIO_Point)
      return Boolean
   is
      Value : gpiod_line_value;
   begin
      Value := gpiod_line_request_get_value (This.Request, Interfaces.C.unsigned (This.Offset));
      case Value is
         when GPIOD_LINE_VALUE_INACTIVE => return False;
         when GPIOD_LINE_VALUE_ACTIVE => return True;
         when others =>
            raise Program_Error with "Error getting GPIO value: " & GNAT.OS_Lib.Errno_Message;
      end case;
   end Set;

   overriding
   procedure Set
      (This : in out GPIO_Point)
   is
      use type Interfaces.C.int;
   begin
      if gpiod_line_request_set_value
         (This.Request, Interfaces.C.unsigned (This.Offset), GPIOD_LINE_VALUE_ACTIVE) /= 0
      then
         raise Program_Error with "Error setting GPIO: " & GNAT.OS_Lib.Errno_Message;
      end if;
   end Set;

   overriding
   procedure Clear
      (This : in out GPIO_Point)
   is
      use type Interfaces.C.int;
   begin
      if gpiod_line_request_set_value
         (This.Request, Interfaces.C.unsigned (This.Offset), GPIOD_LINE_VALUE_INACTIVE) /= 0
      then
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