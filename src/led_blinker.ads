with Ada.Real_Time; use Ada.Real_Time;
with STM32.Board;   use STM32.Board;

package LED_Blinker is
   task Blinker;
end LED_Blinker;
