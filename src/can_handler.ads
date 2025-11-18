with STM32.CAN; use STM32.CAN;

package CAN_Handler is
   Recv_Counter : Natural := 0;
   procedure On_Test (F : CAN_Frame);
end CAN_Handler;
