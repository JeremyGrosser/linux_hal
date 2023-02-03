--
--  Copyright (C) 2023 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
with HAL.Time;

package Linux.Time
   with Preelaborate
is

   type Delays is new HAL.Time.Delays with null record;

   overriding
   procedure Delay_Microseconds
      (This : in out Delays;
       Us   : Integer);

   overriding
   procedure Delay_Milliseconds
      (This : in out Delays;
       Ms   : Integer);

   overriding
   procedure Delay_Seconds
      (This : in out Delays;
       S    : Integer);

end Linux.Time;
