with Ada.Real_Time; use Ada.Real_Time;
with STM32.Board;   use STM32.Board;
with GUI;           use GUI;

package body LED_Blinker is

   task body Blinker is
      Blink_Period : constant Time_Span := Milliseconds (100);
      Next_Time    : Time               := Clock;
   begin
      Initialize_LEDs;

      loop
         Next_Time := Next_Time + Blink_Period;
         Toggle_LEDs (All_LEDs);
         GUI.Check_Buttons;
         delay until Next_Time;
      end loop;
   end Blinker;

end LED_Blinker;
