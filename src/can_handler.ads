with STM32.CAN; use STM32.CAN;

package CAN_Handler is
   Steering_Angle : Long_Float := 0.0;
   procedure On_SteeringAngle (F : CAN_Frame);

   -- Despite being named "Speed" in the DBC, this is really a velocity and can be negative, this is the absolute value
   Vehicle_Speed : Natural := 0;
   procedure On_Speed (F : CAN_Frame);
end CAN_Handler;
