with STM32.CAN;     use STM32.CAN;
with Ada.Real_Time; use Ada.Real_Time;

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
   procedure On_VehicleControl1 (F : CAN_Frame);
   procedure On_VehicleControl2 (F : CAN_Frame);
   procedure On_HVACRequest (F : CAN_Frame);

   procedure Capture_Callback;
   procedure Frunk_Callback;
   procedure Trunk_Callback;
   procedure Glovebox_Callback;
   procedure Heat_Callback;

   type Gear is (Invalid, Drive, Neutral, Park, Rev);
   function To_String (Value : Gear) return String;

   Capture_Enabled                    : Boolean          := False;
   Steering_Angle_Degrees             : Integer          := 0;
   Vehicle_Speed_MPH                  : Natural          := 0;
   Vehicle_Gear                       : Gear             := Invalid;
   Accelerator_Pedal_Position_Percent : Natural          := 0;
   State_Of_Charge_Percent            : Natural          := 0;
   Left_Turn_Signal_Request           : Boolean          := False;
   Right_Turn_Signal_Request          : Boolean          := False;
   VIN_Number                         : String (1 .. 17) := (others => ' ');
   Range_Miles                        : Natural          := 0;
   HV_Battery_Voltage                 : Long_Float       := 0.0;
   Humidity                           : Natural          := 0;
   Temperature                        : Long_Float       := 0.0;
   Rear_Power_kW                      : Integer          := 0;

   Last_Vehicle_Control1 : CAN_Frame (ID_Type => STM32.CAN.Standard) :=
     (others => <>);
   Last_Vehicle_Control1_Time         : Time             := Time_First;

   Last_Vehicle_Control2 : CAN_Frame (ID_Type => STM32.CAN.Standard) :=
     (others => <>);
   Last_Vehicle_Control2_Time         : Time             := Time_First;

   Last_HVAC_Request : CAN_Frame (ID_Type => STM32.CAN.Standard) :=
     (others => <>);
   Last_HVAC_Request_Time             : Time             := Time_First;
end CAN_Handler;
