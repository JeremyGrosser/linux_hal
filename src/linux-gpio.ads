--
--  Copyright (C) 2023 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
with HAL.GPIO;

package Linux.GPIO
   with Preelaborate
is

   type GPIO_Point is new HAL.GPIO.GPIO_Point with private;

   type Chip is private;

   function Open
      (Path : String)
      return Chip
   with Import, Convention => C, External_Name => "gpiod_chip_open";

   procedure Close
      (This : Chip)
   with Import, Convention => C, External_Name => "gpiod_chip_close";

   function Find
      (This : Chip;
       Name : String)
       return GPIO_Point;

   function Get_Point
      (This : Chip;
       Num  : Natural)
       return GPIO_Point;

   overriding
   function Support
      (This : GPIO_Point;
       Capa : HAL.GPIO.Capability)
       return Boolean;

   overriding
   function Mode
      (This : GPIO_Point)
      return HAL.GPIO.GPIO_Mode;

   overriding
   procedure Set_Mode
      (This : in out GPIO_Point;
       Mode : HAL.GPIO.GPIO_Config_Mode);

   overriding
   function Pull_Resistor
      (This : GPIO_Point)
      return HAL.GPIO.GPIO_Pull_Resistor;

   overriding
   procedure Set_Pull_Resistor
      (This : in out GPIO_Point;
       Pull : HAL.GPIO.GPIO_Pull_Resistor);

   overriding
   function Set
      (This : GPIO_Point)
      return Boolean;

   overriding
   procedure Set
      (This : in out GPIO_Point);

   overriding
   procedure Clear
      (This : in out GPIO_Point);

   overriding
   procedure Toggle
      (This : in out GPIO_Point);

private

   type gpiod_chip is null record;
   type Chip is access all gpiod_chip;

   type gpiod_line is null record;
   type Line is access all gpiod_line;

   type GPIO_Point is new HAL.GPIO.GPIO_Point with record
      C : Chip;
      L : Line;
   end record;

   procedure Reserve
      (This : in out GPIO_Point);

   procedure Release
      (This : in out GPIO_Point);

end Linux.GPIO;
