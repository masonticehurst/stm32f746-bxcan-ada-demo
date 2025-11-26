with STM32.CAN; use STM32.CAN;

package CAN_Handler is

   -- Despite being named "Speed" in the DBC,
   -- this is really a velocity and can be negative.
   -- ATM this is the absolute value of velocity
   procedure On_Speed (F : CAN_Frame);
   procedure On_DriverSystemStatus (F : CAN_Frame);
   procedure On_SteeringAngle (F : CAN_Frame);
   procedure On_Lighting (F : CAN_Frame);
   procedure On_VIN (F : CAN_Frame);
   procedure On_RangeSOC (F : CAN_Frame);
   procedure On_HVBattAmpVolt (F : CAN_Frame);
   procedure On_THSStatus (F : CAN_Frame);
   procedure On_RearInverterPower (F : CAN_Frame);

   type Gear is (Invalid, Drive, Neutral, Park, Rev);
   function To_String (Value : Gear) return String;

   Steering_Angle_Degrees             : Long_Float       := 0.0;
   Vehicle_Speed_MPH                  : Natural          := 0;
   Vehicle_Gear                       : Gear             := Invalid;
   Accelerator_Pedal_Position_Percent : Natural          := 0;
   Left_Turn_Signal                   : Boolean          := False;
   Right_Turn_Signal                  : Boolean          := False;
   VIN_Number                         : String (1 .. 17) := (others => ' ');
   Range_Miles                        : Natural          := 0;
   HV_Battery_Voltage                 : Long_Float       := 0.0;
   Humidity                           : Natural          := 0;
   Temperature                        : Long_Float       := 0.0;
   Rear_Power_kW                      : Long_Float       := 0.0;
end CAN_Handler;
