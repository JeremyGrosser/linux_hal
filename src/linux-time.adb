--
--  Copyright (C) 2023 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
package body Linux.Time is

   overriding
   procedure Delay_Microseconds
      (This : in out Delays;
       Us   : Integer)
   is
   begin
      delay Duration (Us) / 1_000_000.0;
   end Delay_Microseconds;

   overriding
   procedure Delay_Milliseconds
      (This : in out Delays;
       Ms   : Integer)
   is
   begin
      delay Duration (Ms) / 1_000.0;
   end Delay_Milliseconds;

   overriding
   procedure Delay_Seconds
      (This : in out Delays;
       S    : Integer)
   is
   begin
      delay Duration (S);
   end Delay_Seconds;

end Linux.Time;
