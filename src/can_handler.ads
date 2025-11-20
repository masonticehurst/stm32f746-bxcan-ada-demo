with STM32.CAN; use STM32.CAN;

package CAN_Handler is

   -- Despite being named "Speed" in the DBC,
   -- this is really a velocity and can be negative.
   -- ATM this is the absolute value of velocity
   procedure On_Speed (F : CAN_Frame);
   procedure On_DriverSystemStatus (F : CAN_Frame);
   procedure On_SteeringAngle (F : CAN_Frame);

   type Gear is (Invalid, Drive, Neutral, Park, Rev);
   function To_String (Value : Gear) return String;

   Steering_Angle_Degrees             : Long_Float := 0.0;
   Vehicle_Speed_MPH                  : Natural    := 0;
   Vehicle_Gear                       : Gear       := Invalid;
   Accelerator_Pedal_Position_Percent : Natural    := 0;

end CAN_Handler;
