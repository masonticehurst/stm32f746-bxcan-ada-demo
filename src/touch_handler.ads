with Ada.Real_Time; use Ada.Real_Time;
with STM32.Board;   use STM32.Board;

package Touch_Handler is
   task Handler;
end Touch_Handler;
