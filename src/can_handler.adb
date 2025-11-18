with GUI; use GUI;

package body CAN_Handler is
   procedure On_Test (F : CAN_Frame) is
   begin
      Recv_Counter := Recv_Counter + 1;
   end On_Test;
end CAN_Handler;
