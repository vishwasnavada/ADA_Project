------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2015-2016, AdaCore                     --
--                                                                          --
--  Redistribution and use in source and binary forms, with or without      --
--  modification, are permitted provided that the following conditions are  --
--  met:                                                                    --
--     1. Redistributions of source code must retain the above copyright    --
--        notice, this list of conditions and the following disclaimer.     --
--     2. Redistributions in binary form must reproduce the above copyright --
--        notice, this list of conditions and the following disclaimer in   --
--        the documentation and/or other materials provided with the        --
--        distribution.                                                     --
--     3. Neither the name of the copyright holder nor the names of its     --
--        contributors may be used to endorse or promote products derived   --
--        from this software without specific prior written permission.     --
--                                                                          --
--   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS    --
--   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT      --
--   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR  --
--   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT   --
--   HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, --
--   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT       --
--   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,  --
--   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY  --
--   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT    --
--   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE  --
--   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.   --
--                                                                          --
------------------------------------------------------------------------------

--  This is a demo of the features available on the STM32F4-DISCOVERY board.
--
--  Tilt the board and the LED closer to the ground will light up.

with Ada.Real_Time;      use Ada.Real_Time;
with HAL;                use HAL;
with STM32.Board;        use STM32.Board;
with LIS3DSH;            use LIS3DSH;
with System;          use System;
with STM32_SVD.USART; use STM32_SVD, STM32_SVD.USART;
with STM32.Device;    use STM32.Device;

procedure Main is

   Values : LIS3DSH.Axes_Accelerations;

   Threshold_High : constant LIS3DSH.Axis_Acceleration :=  25;
   Threshold_Low  : constant LIS3DSH.Axis_Acceleration := -28;
   Breath_rate : UInt9;

   procedure My_Delay (Milli : Natural);

   procedure My_Delay (Milli : Natural) is
   begin
      delay until Clock + Milliseconds (Milli);
   end My_Delay;

begin
   Initialize_LEDs;

   Initialize_Accelerometer;

   Accelerometer.Configure
     (Output_DataRate => Data_Rate_100Hz,
      Axes_Enable     => XYZ_Enabled,
      SPI_Wire        => Serial_Interface_4Wire,
      Self_Test       => Self_Test_Normal,
      Full_Scale      => Fullscale_2g,
      Filter_BW       => Filter_800Hz);

     -------------------
   -- Set_Baud_Rate --
   -------------------

   procedure Set_Baud_Rate (This : in out USART; To : Baud_Rates)
   is
      Clock        : constant UInt32 := APB_Clock (This);
      Over_By_8    : constant Boolean := This.Periph.CR1.OVER8;
      Int_Scale    : constant UInt32 := (if Over_By_8 then 2 else 4);
      Int_Divider  : constant UInt32 := (25 * Clock) / (Int_Scale * To);
      Frac_Divider : constant UInt32 := Int_Divider rem 100;
   begin
      --  the integer part of the divi
      if Over_By_8 then
         This.Periph.BRR.DIV_Fraction :=
           BRR_DIV_Fraction_Field (((Frac_Divider * 8) + 50) / 100 mod 8);
      else
         This.Periph.BRR.DIV_Fraction :=
           BRR_DIV_Fraction_Field (((Frac_Divider * 16) + 50) / 100 mod 16);
      end if;

      This.Periph.BRR.DIV_Mantissa :=
        BRR_DIV_Mantissa_Field (Int_Divider / 100);
   end Set_Baud_Rate;


   if Accelerometer.Device_Id /= I_Am_LIS3DSH then
      All_LEDs_On;
      My_Delay (100);
      All_LEDs_Off;
      My_Delay (100);
   else
      loop
         Accelerometer.Get_Accelerations (Values);
         if abs Values.Z > abs Values.Y then
            if Values.Z > Threshold_High then
               STM32.Board.Red_LED.Set;
               Breath_rate ++;
            elsif Values.Z < Threshold_Low then
               STM32.Board.Green_LED.Set;
               Breath_rate ++;
            end if
            Breath_rate = Breath_rate/2;
         end if;
       procedure Transmit (This : in out USART;  Data : UInt9) is
   begin
      This.Periph.DR.DR := Breath_rate;
   end Transmit;
      end loop;
   end if;
end Main;
