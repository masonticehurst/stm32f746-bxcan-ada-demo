with GUI;        use GUI;
with GUI.Images;
with Interfaces; use Interfaces;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

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

   --  BO_ 297 ID129SteeringAngle: 8 VehicleBus
   --   SG_ SteeringSensorC129 : 56|8@1+ (1,0) [0|255] ""  Receiver
   --   SG_ SteeringSensorB129 : 48|8@1+ (1,0) [0|255] ""  Receiver
   --   SG_ SteeringSensorA129 : 46|2@1+ (1,0) [0|3] ""  Receiver
   --   SG_ SteeringSpeed129 : 32|14@1+ (0.5,-4096) [-4096|4095.5] "D/S"  Receiver
   --   SG_ SteeringAngle129 : 16|14@1+ (0.1,-819.2) [-819.2|819.1] "Deg"  Receiver
   procedure On_SteeringAngle (F : CAN_Frame) is
      Raw : constant Unsigned_32 :=
        Extract_LE_Signal (Data => F.Data, StartBit => 16, Length => 14);

      -- DBC: (factor, offset) = (0.1, -819.2)
      Scaled : constant Long_Float := Long_Float (Raw) * 0.1 - 819.2;
   begin
      Steering_Angle := Scaled;

      GUI.Images.Draw_Image
        (X0            => 190, Y0 => 50, Image => GUI.Images.Steering_Wheel,
         Angle_Degrees => Steering_Angle);

      GUI.Redraw;
   end On_SteeringAngle;

   --  BO_ 599 ID257DIspeed: 8 VehicleBus
   --   SG_ DI_speedChecksum : 0|8@1+ (1,0) [0|255] ""  Receiver
   --   SG_ DI_speedCounter : 8|4@1+ (1,0) [0|15] ""  Receiver
   --   SG_ DI_uiSpeed : 24|9@1+ (1,0) [0|510] ""  Receiver
   --   SG_ DI_uiSpeedHighSpeed : 34|9@1+ (1,0) [0|510] ""  Receiver
   --   SG_ DI_uiSpeedUnits : 33|1@1+ (1,0) [0|1] ""  Receiver
   --   SG_ DI_vehicleSpeed : 12|12@1+ (0.08,-40) [-40|285] "kph"  Receiver
   procedure On_Speed (F : CAN_Frame) is
      Raw : constant Unsigned_32 :=
        Extract_LE_Signal (Data => F.Data, StartBit => 12, Length => 12);

      -- DBC scaling: (factor, offset) = (0.08, -40)
      Scaled : constant Long_Float := Long_Float (Raw) * 0.08 - 40.0;
   begin
      Vehicle_Speed := Natural (abs Scaled);

      Draw_Info
            (Point => (X => 9, Y => 205), Text => "Speed (MPH)",
               Val   =>
               Trim
                  (CAN_Handler.Vehicle_Speed'Image, Ada.Strings.Both));
   end On_Speed;

end CAN_Handler;
