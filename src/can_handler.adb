with CAN_Handler;
with Interfaces; use Interfaces;
with HAL;        use HAL;
with GUI;        use GUI;

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
      Steering_Angle_Degrees := Integer (Scaled_SteeringAngle);
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
      --  Raw_TurnSignalLeftStatus  : constant Unsigned_32 :=
      --    Extract_LE_Signal (Data => F.Data, StartBit => 50, Length => 2);
      --  Raw_TurnSignalRightStatus : constant Unsigned_32 :=
      --    Extract_LE_Signal (Data => F.Data, StartBit => 52, Length => 2);
      Raw_IndicatorLeftRequest  : constant Unsigned_32 :=
        Extract_LE_Signal (Data => F.Data, StartBit => 0, Length => 2);
      Raw_IndicatorRightRequest : constant Unsigned_32 :=
        Extract_LE_Signal (Data => F.Data, StartBit => 2, Length => 2);

   begin
      if Raw_IndicatorLeftRequest = 2 or Raw_IndicatorLeftRequest = 1 then
         Left_Turn_Signal_Request := True;
      else
         Left_Turn_Signal_Request := False;
      end if;

      if Raw_IndicatorRightRequest = 2 or Raw_IndicatorRightRequest = 1 then
         Right_Turn_Signal_Request := True;
      else
         Right_Turn_Signal_Request := False;
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

   --  BO_ 826 ID33AUI_rangeSOC: 8 VehicleBus
   --   SG_ UI_idealRange : 16|10@1+ (1,0) [0|1023] "mi"  Receiver
   --   SG_ UI_Range : 0|10@1+ (1,0) [0|1023] "mi"  Receiver
   --   SG_ UI_SOC : 48|7@1+ (1,0) [0|127] "%"  Receiver
   --   SG_ UI_uSOE : 56|7@1+ (1,0) [0|127] "%"  Receiver
   --   SG_ UI_ratedWHpM : 32|10@1+ (1,0) [0|1023] "WHpM"  Receiver
   procedure On_RangeSOC (F : CAN_Frame) is
      Raw_Range : constant Unsigned_32 :=
        Extract_LE_Signal (Data => F.Data, StartBit => 0, Length => 10);

      Raw_SOC : constant Unsigned_32 :=
        Extract_LE_Signal (Data => F.Data, StartBit => 48, Length => 7);
   begin
      Range_Miles             := Natural (Raw_Range);
      State_Of_Charge_Percent :=
        100 - Natural (Long_Float (Raw_SOC) / 127.0 * 100.0);
   end On_RangeSOC;

   --  BO_ 306 ID132HVBattAmpVolt: 8 VehicleBus
   --   SG_ ChargeHoursRemaining132 : 48|12@1+ (1,0) [0|4095] "Min"  Receiver
   --   SG_ BattVoltage132 : 0|16@1+ (0.01,0) [0|655.35] "V"  Receiver
   --   SG_ RawBattCurrent132 : 32|16@1- (-0.05,822) [-1138.35|2138.4] "A"  Receiver
   --   SG_ SmoothBattCurrent132 : 16|16@1- (-0.1,0) [-3276.7|3276.7] "A"  Receiver
   procedure On_HVBattAmpVolt (F : CAN_Frame) is
      Raw_Batt_Voltage    : constant Unsigned_32 :=
        Extract_LE_Signal (Data => F.Data, StartBit => 0, Length => 16);
      Scaled_Batt_Voltage : constant Long_Float  :=
        Long_Float (Raw_Batt_Voltage) * 0.01;
   begin
      HV_Battery_Voltage := Scaled_Batt_Voltage;
   end On_HVBattAmpVolt;

   --  BO_ 899 ID383VCRIGHT_thsStatus: 8 VehicleBus
   --   SG_ VCRIGHT_estimatedThsSolarLoad : 53|10@1+ (1,0) [0|1022] "W/m2"  Receiver
   --   SG_ VCRIGHT_estimatedVehicleSituatio : 31|2@1+ (1,0) [0|2] ""  Receiver
   --   SG_ VCRIGHT_thsActive : 0|1@1+ (1,0) [0|1] ""  Receiver
   --   SG_ VCRIGHT_thsHumidity : 17|8@1+ (1,0) [0|100] "%"  Receiver
   --   SG_ VCRIGHT_thsSolarLoadInfrared : 33|10@1+ (1,0) [0|1022] "W/m2"  Receiver
   --   SG_ VCRIGHT_thsSolarLoadVisible : 43|10@1+ (1,0) [0|1022] "W/m2"  Receiver
   --   SG_ VCRIGHT_thsTemperature : 1|8@1- (1,-40) [-40|150] "C"  Receiver
   procedure On_THSStatus (F : CAN_Frame) is
      Raw_Humidity    : constant Unsigned_32 :=
        Extract_LE_Signal (Data => F.Data, StartBit => 17, Length => 8);
      Raw_Temperature : constant Unsigned_32 :=
        Extract_LE_Signal (Data => F.Data, StartBit => 1, Length => 8);

      Scaled_Temperature : constant Long_Float :=
        Long_Float (Raw_Temperature) * 1.0 - 40.0;
   begin
      Humidity    := Natural (Raw_Humidity);
      Temperature := Scaled_Temperature;
   end On_THSStatus;

   --  BO_ 614 ID266RearInverterPower: 8 VehicleBus
   --   SG_ RearHeatPowerMax266 : 24|8@1+ (0.08,0) [0|20] "kW"  Receiver
   --   SG_ RearPowerLimit266 : 48|9@1+ (1,0) [0|400] "kW"  Receiver
   --   SG_ RearHeatPower266 : 32|8@1+ (0.08,0) [0|20] "kW"  Receiver
   --   SG_ RearHeatPowerOptimal266 : 16|8@1+ (0.08,0) [0|20] "kW"  Receiver
   --   SG_ RearExcessHeatCmd : 40|8@1+ (0.08,0) [0|20] "kW"  Receiver
   --   SG_ RearPower266 : 0|11@1- (0.5,0) [-500|500] "kW"  Receiver
   procedure On_RearInverterPower (F : CAN_Frame) is
      Raw_Rear_Power : constant Unsigned_32 :=
        Extract_LE_Signal (Data => F.Data, StartBit => 0, Length => 11);

      Scaled_Rear_Power : constant Long_Float :=
        Long_Float (Raw_Rear_Power) * 0.5;
   begin
      Rear_Power_kW := Integer (Scaled_Rear_Power);
   end On_RearInverterPower;

   procedure On_VehicleControl1 (F : CAN_Frame) is
   begin
      if (F.Standard_ID = 627) then
         Last_Vehicle_Control1      := F;
         Last_Vehicle_Control1_Time := Clock;
      end if;
   end On_VehicleControl1;

   procedure On_VehicleControl2 (F : CAN_Frame) is
   begin
      if (F.Standard_ID = 947) then
         Last_Vehicle_Control2      := F;
         Last_Vehicle_Control2_Time := Clock;
      end if;
   end On_VehicleControl2;

   procedure On_HVACRequest (F : CAN_Frame) is
   begin
      if (F.Standard_ID = 755) then
         Last_HVAC_Request      := F;
         Last_HVAC_Request_Time := Clock;
      end if;
   end On_HVACRequest;

   procedure Capture_Callback is
   begin
      Capture_Enabled := not Capture_Enabled;
      Control_Page_Init;
   end Capture_Callback;

   procedure Frunk_Callback is
      New_Frame : STM32.CAN.CAN_Frame (STM32.CAN.Standard) := (others => <>);
      RC        : STM32.CAN.CAN_Status := Transmission_Error;
   begin
      if (Clock - CAN_Handler.Last_Vehicle_Control1_Time) <
        Milliseconds (1_000)
      then
         New_Frame.DLC         := CAN_Handler.Last_Vehicle_Control1.DLC;
         New_Frame.RTR         := CAN_Handler.Last_Vehicle_Control1.RTR;
         New_Frame.Standard_ID :=
           CAN_Handler.Last_Vehicle_Control1.Standard_ID;
         New_Frame.Data        := CAN_Handler.Last_Vehicle_Control1.Data;

         declare
            -- bit 5 lives in first byte (1-indexed)
            Byte_Index : constant Natural := 1;
            Bit_Mask   : constant UInt8   := 2#0010_0000#;
         begin
            New_Frame.Data (Byte_Index) :=
              New_Frame.Data (Byte_Index) or Bit_Mask;
         end;

         RC := STM32.CAN.Transmit (CAN_1, New_Frame);
         GUI.Put_Line (RC'Image);
      end if;
   end Frunk_Callback;

   procedure Trunk_Callback is
      New_Frame : STM32.CAN.CAN_Frame (STM32.CAN.Standard) := (others => <>);
      RC        : STM32.CAN.CAN_Status := Transmission_Error;
   begin
      if (Clock - CAN_Handler.Last_Vehicle_Control2_Time) <
        Milliseconds (1_000)
      then
         New_Frame.DLC         := CAN_Handler.Last_Vehicle_Control2.DLC;
         New_Frame.RTR         := CAN_Handler.Last_Vehicle_Control2.RTR;
         New_Frame.Standard_ID :=
           CAN_Handler.Last_Vehicle_Control2.Standard_ID;
         New_Frame.Data        := CAN_Handler.Last_Vehicle_Control2.Data;

         declare
            -- bit 1 lives in first byte (1-indexed)
            Byte_Index : constant Natural := 1;
            Bit_Mask   : constant UInt8   := 2#0000_0010#;
         begin
            New_Frame.Data (Byte_Index) :=
              New_Frame.Data (Byte_Index) or Bit_Mask;
         end;

         RC := STM32.CAN.Transmit (CAN_1, New_Frame);
         GUI.Put_Line (RC'Image);
      end if;
   end Trunk_Callback;

   procedure Glovebox_Callback is
      New_Frame : STM32.CAN.CAN_Frame (STM32.CAN.Standard) := (others => <>);
      RC        : STM32.CAN.CAN_Status := Transmission_Error;
   begin
      if (Clock - CAN_Handler.Last_Vehicle_Control2_Time) <
        Milliseconds (1_000)
      then
         New_Frame.DLC         := CAN_Handler.Last_Vehicle_Control2.DLC;
         New_Frame.RTR         := CAN_Handler.Last_Vehicle_Control2.RTR;
         New_Frame.Standard_ID :=
           CAN_Handler.Last_Vehicle_Control2.Standard_ID;
         New_Frame.Data        := CAN_Handler.Last_Vehicle_Control2.Data;

         declare
            -- bit 0 lives in first byte (1-indexed)
            Byte_Index : constant Natural := 1;
            Bit_Mask   : constant UInt8   := 2#0000_0001#;
         begin
            New_Frame.Data (Byte_Index) :=
              New_Frame.Data (Byte_Index) or Bit_Mask;
         end;

         RC := STM32.CAN.Transmit (CAN_1, New_Frame);
         GUI.Put_Line (RC'Image);
      end if;
   end Glovebox_Callback;

   procedure Heat_Callback is
      New_Frame : STM32.CAN.CAN_Frame (STM32.CAN.Standard) := (others => <>);
      RC        : STM32.CAN.CAN_Status := Transmission_Error;
   begin
      if (Clock - CAN_Handler.Last_HVAC_Request_Time) < Milliseconds (1_000)
      then
         New_Frame.DLC         := CAN_Handler.Last_HVAC_Request.DLC;
         New_Frame.RTR         := CAN_Handler.Last_HVAC_Request.RTR;
         New_Frame.Standard_ID := CAN_Handler.Last_HVAC_Request.Standard_ID;
         New_Frame.Data        := CAN_Handler.Last_HVAC_Request.Data;

         declare
            Byte_Index : constant Natural := 1;                  -- first byte
            Temp_Mask  : constant UInt8   := 2#0001_1111#;       -- 5 LSBs

            Old_Byte : UInt8   := New_Frame.Data (Byte_Index);
            Old_Raw  : Integer := Integer (Old_Byte and Temp_Mask);
            New_Raw : Integer := Old_Raw + 2;
         begin
            declare
               -- clear out the lower 5 bits, keep the upper 3 bits as-is
               Cleared_Lows : constant UInt8 := Old_Byte and not Temp_Mask;

               -- new 5-bit temperature
               New_Lows : constant UInt8 := UInt8 (New_Raw) and Temp_Mask;
            begin
               New_Frame.Data (Byte_Index) := Cleared_Lows or New_Lows;
            end;
         end;

         RC := STM32.CAN.Transmit (CAN_1, New_Frame);
         GUI.Put_Line (RC'Image);
      end if;
   end Heat_Callback;
end CAN_Handler;
