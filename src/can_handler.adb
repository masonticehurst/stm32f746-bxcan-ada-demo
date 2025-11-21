with GUI;        use GUI;
with GUI.Images;
with Interfaces; use Interfaces;

package body CAN_Handler is
   function Extract_LE_Signal
     (Data : CAN_Data_8b; StartBit : Natural; Length : Natural)
      return Unsigned_32
   is
      -- Pack the 8 bytes into a 64-bit little-endian value:
      --   byte 0 (Data(1)) -> bits 0..7
      --   byte 1 (Data(2)) -> bits 8..15
      --   ...
      Buf : Unsigned_64 := 0;
   begin
      for I in 0 .. 7 loop
         Buf := Buf or Shift_Left (Unsigned_64 (Data (I + 1)), I * 8);
      end loop;

      -- Shift down so bit StartBit is now bit 0:
      Buf := Shift_Right (Buf, StartBit);

      -- Mask off the desired number of bits:
      declare
         Mask : constant Unsigned_64 :=
           (if Length = 32 then Unsigned_64'Last
            else Shift_Left (1, Length) - 1);
      begin
         return Unsigned_32 (Buf and Mask);
      end;
   end Extract_LE_Signal;

   function To_String (Value : Gear) return String is
   begin
      case Value is
         when Park =>
            return "P";
         when Rev =>
            return "R";
         when Neutral =>
            return "N";
         when Drive =>
            return "D";
         when others =>
            return " ";
      end case;
   end To_String;

   --  BO_ 297 ID129SteeringAngle: 8 VehicleBus
   --   SG_ SteeringSensorC129 : 56|8@1+ (1,0) [0|255] ""  Receiver
   --   SG_ SteeringSensorB129 : 48|8@1+ (1,0) [0|255] ""  Receiver
   --   SG_ SteeringSensorA129 : 46|2@1+ (1,0) [0|3] ""  Receiver
   --   SG_ SteeringSpeed129 : 32|14@1+ (0.5,-4096) [-4096|4095.5] "D/S"  Receiver
   --   SG_ SteeringAngle129 : 16|14@1+ (0.1,-819.2) [-819.2|819.1] "Deg"  Receiver
   procedure On_SteeringAngle (F : CAN_Frame) is
      Raw_SteeringAngle : constant Unsigned_32 :=
        Extract_LE_Signal (Data => F.Data, StartBit => 16, Length => 14);

      -- DBC: (factor, offset) = (0.1, -819.2)
      Scaled_SteeringAngle : constant Long_Float :=
        Long_Float (Raw_SteeringAngle) * 0.1 - 819.2;
   begin
      Steering_Angle_Degrees := Scaled_SteeringAngle;
   end On_SteeringAngle;

   --  BO_ 599 ID257DIspeed: 8 VehicleBus
   --   SG_ DI_speedChecksum : 0|8@1+ (1,0) [0|255] ""  Receiver
   --   SG_ DI_speedCounter : 8|4@1+ (1,0) [0|15] ""  Receiver
   --   SG_ DI_uiSpeed : 24|9@1+ (1,0) [0|510] ""  Receiver
   --   SG_ DI_uiSpeedHighSpeed : 34|9@1+ (1,0) [0|510] ""  Receiver
   --   SG_ DI_uiSpeedUnits : 33|1@1+ (1,0) [0|1] ""  Receiver
   --   SG_ DI_vehicleSpeed : 12|12@1+ (0.08,-40) [-40|285] "kph"  Receiver
   procedure On_Speed (F : CAN_Frame) is
      Raw_VehicleSpeed : constant Unsigned_32 :=
        Extract_LE_Signal (Data => F.Data, StartBit => 12, Length => 12);

      -- DBC scaling: (factor, offset) = (0.08, -40)
      Scaled_VehicleSpeed : constant Long_Float :=
        Long_Float (Raw_VehicleSpeed) * 0.08 - 40.0;

      -- Convert KPH to MPH
      KPH_TO_MPH : constant Long_Float := 0.621_371;
      Scaled_MPH : constant Long_Float := Scaled_VehicleSpeed * KPH_TO_MPH;
   begin
      Vehicle_Speed_MPH := Natural (abs Scaled_MPH);
   end On_Speed;

   --  BO_ 280 ID118DriveSystemStatus: 8 VehicleBus
   --   SG_ DI_accelPedalPos : 32|8@1+ (0.4,0) [0|100] "%"  Receiver
   --   SG_ DI_brakePedalState : 19|2@1+ (1,0) [0|2] ""  Receiver
   --   SG_ DI_driveBlocked : 12|2@1+ (1,0) [0|2] ""  Receiver
   --   SG_ DI_epbRequest : 44|2@1+ (1,0) [0|2] ""  Receiver
   --   SG_ DI_gear : 21|3@1+ (1,0) [0|7] ""  Receiver
   --   SG_ DI_immobilizerState : 27|3@1+ (1,0) [0|6] ""  Receiver
   --   SG_ DI_keepDrivePowerStateRequest : 47|1@1+ (1,0) [0|1] ""  Receiver
   --   SG_ DI_proximity : 46|1@1+ (1,0) [0|1] ""  Receiver
   --   SG_ DI_regenLight : 26|1@1+ (1,0) [0|1] ""  Receiver
   --   SG_ DI_systemState : 16|3@1+ (1,0) [0|5] ""  Receiver
   --   SG_ DI_systemStatusChecksum : 0|8@1+ (1,0) [0|255] ""  Receiver
   --   SG_ DI_systemStatusCounter : 8|4@1+ (1,0) [0|15] ""  Receiver
   --   SG_ DI_trackModeState : 48|2@1+ (1,0) [0|2] ""  Receiver
   --   SG_ DI_tractionControlMode : 40|3@1+ (1,0) [0|6] ""  Receiver
   procedure On_DriverSystemStatus (F : CAN_Frame) is
      Raw_AccelPedalPos : constant Unsigned_32 :=
        Extract_LE_Signal (Data => F.Data, StartBit => 32, Length => 8);

      Raw_Gear : constant Unsigned_32 :=
        Extract_LE_Signal (Data => F.Data, StartBit => 21, Length => 3);

      -- DBC scaling: (factor, offset) = (0.4, 0)
      Scaled_AccelPedalPos : constant Long_Float :=
        Long_Float (Raw_AccelPedalPos) * 0.4;
   begin
      Accelerator_Pedal_Position_Percent := Natural (Scaled_AccelPedalPos);

      case Raw_Gear is
         when 1 =>
            Vehicle_Gear := Park;
         when 2 =>
            Vehicle_Gear := Rev;
         when 3 =>
            Vehicle_Gear := Neutral;
         when 4 =>
            Vehicle_Gear := Drive;
         when others =>
            Vehicle_Gear := Invalid;
      end case;
   end On_DriverSystemStatus;

   --  BO_ 1013 ID3F5VCFRONT_lighting: 8 VehicleBus
   --   SG_ VCFRONT_DRLLeftStatus : 36|2@1+ (1,0) [0|3] ""  Receiver
   --   SG_ VCFRONT_DRLRightStatus : 38|2@1+ (1,0) [0|3] ""  Receiver
   --   SG_ VCFRONT_ambientLightingBrightnes : 8|8@1+ (0.5,0) [0|127] "%"  Receiver
   --   SG_ VCFRONT_approachLightingRequest : 25|1@1+ (1,0) [0|1] ""  Receiver
   --   SG_ VCFRONT_courtesyLightingRequest : 24|1@1+ (1,0) [0|1] ""  Receiver
   --   SG_ VCFRONT_fogLeftStatus : 40|2@1+ (1,0) [0|3] ""  Receiver
   --   SG_ VCFRONT_fogRightStatus : 42|2@1+ (1,0) [0|3] ""  Receiver
   --   SG_ VCFRONT_hazardLightRequest : 4|4@1+ (1,0) [0|8] ""  Receiver
   --   SG_ VCFRONT_hazardSwitchBacklight : 27|1@1+ (1,0) [0|1] ""  Receiver
   --   SG_ VCFRONT_highBeamLeftStatus : 32|2@1+ (1,0) [0|3] ""  Receiver
   --   SG_ VCFRONT_highBeamRightStatus : 34|2@1+ (1,0) [0|3] ""  Receiver
   --   SG_ VCFRONT_highBeamSwitchActive : 58|1@1+ (1,0) [0|1] ""  Receiver
   --   SG_ VCFRONT_indicatorLeftRequest : 0|2@1+ (1,0) [0|2] ""  Receiver
   --   SG_ VCFRONT_indicatorRightRequest : 2|2@1+ (1,0) [0|2] ""  Receiver
   --   SG_ VCFRONT_lowBeamLeftStatus : 28|2@1+ (1,0) [0|3] ""  Receiver
   --   SG_ VCFRONT_lowBeamRightStatus : 30|2@1+ (1,0) [0|3] ""  Receiver
   --   SG_ VCFRONT_lowBeamsCalibrated : 62|1@1+ (1,0) [0|1] ""  Receiver
   --   SG_ VCFRONT_lowBeamsOnForDRL : 61|1@1+ (1,0) [0|1] ""  Receiver
   --   SG_ VCFRONT_parkLeftStatus : 54|2@1+ (1,0) [0|3] ""  Receiver
   --   SG_ VCFRONT_parkRightStatus : 56|2@1+ (1,0) [0|3] ""  Receiver
   --   SG_ VCFRONT_seeYouHomeLightingReq : 26|1@1+ (1,0) [0|1] ""  Receiver
   --   SG_ VCFRONT_sideMarkersStatus : 44|2@1+ (1,0) [0|3] ""  Receiver
   --   SG_ VCFRONT_sideRepeaterLeftStatus : 46|2@1+ (1,0) [0|3] ""  Receiver
   --   SG_ VCFRONT_sideRepeaterRightStatus : 48|2@1+ (1,0) [0|3] ""  Receiver
   --   SG_ VCFRONT_simLatchingStalk : 59|2@1+ (1,0) [0|3] ""  Receiver
   --   SG_ VCFRONT_switchLightingBrightness : 16|8@1+ (0.5,0) [0|127] "%"  Receiver
   --   SG_ VCFRONT_turnSignalLeftStatus : 50|2@1+ (1,0) [0|3] ""  Receiver
   --   SG_ VCFRONT_turnSignalRightStatus : 52|2@1+ (1,0) [0|3] ""  Receiver
   procedure On_Lighting (F : CAN_Frame) is
      Raw_TurnSignalLeftStatus : constant Unsigned_32 :=
        Extract_LE_Signal (Data => F.Data, StartBit => 50, Length => 2);

      Raw_TurnSignalRightStatus : constant Unsigned_32 :=
        Extract_LE_Signal (Data => F.Data, StartBit => 52, Length => 2);
   begin
      if Raw_TurnSignalLeftStatus = 1 then
         Left_Turn_Signal := True;
      else
         Left_Turn_Signal := False;
      end if;

      if Raw_TurnSignalRightStatus = 1 then
         Right_Turn_Signal := True;
      else
         Right_Turn_Signal := False;
      end if;
   end On_Lighting;

   --  BO_ 1029 ID405VIN: 8 VehicleBus
   --   SG_ VINB405 m17 : 8|56@1+ (1,0) [0|7.20576E+016] ""  Receiver
   --   SG_ VINC405 m18 : 8|56@1+ (1,0) [0|7.20576E+016] ""  Receiver
   --   SG_ VINA405 m16 : 8|56@1+ (1,0) [0|7.20576E+016] ""  Receiver
   --   SG_ mux405 M : 0|8@1+ (1,0) [0|255] ""  Receiver
   procedure On_VIN (F : CAN_Frame) is
      Mux : constant Unsigned_32 :=
        Extract_LE_Signal (Data => F.Data, StartBit => 0, Length => 8);
   begin
      case Mux is
         -- VINA405: first 3 characters
         when 16 =>
            for I in 4 .. 6 loop
               VIN_Number (I - 3) := Character'Val (Natural (F.Data (2 + I)));
            end loop;

            -- VINB405: middle 7 characters
         when 17 =>
            for I in 1 .. 7 loop
               VIN_Number (3 + I) := Character'Val (Natural (F.Data (1 + I)));
            end loop;

            -- VINC405: last 7 characters
         when 18 =>
            for I in 1 .. 7 loop
               VIN_Number (10 + I) := Character'Val (Natural (F.Data (1 + I)));
            end loop;

         when others =>
            null;

      end case;
   end On_VIN;

end CAN_Handler;
