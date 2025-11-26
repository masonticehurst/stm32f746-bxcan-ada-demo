with LED_Blinker;
with Cortex_M.Cache;
with Exception_Handler;
with SDCard;

with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Real_Time;     use Ada.Real_Time;
with STM32.Board;       use STM32.Board;
with STM32.Device;      use STM32.Device;
with HAL.Bitmap;
with HAL.SDMMC;         use HAL.SDMMC;
with STM32.SDMMC;
with File_IO;           use File_IO;
with STM32.CAN;         use STM32.CAN;
with HAL;               use HAL;
with BMP_Fonts;         use BMP_Fonts;
with GUI;               use GUI;
with GUI.Bitmap;        use GUI.Bitmap;
with GUI.Images;        use GUI.Images;
with CAN_Handler;

procedure bxcan is
   SD_Card_Info        : HAL.SDMMC.Card_Information;
   SDMMC_Status        : HAL.SDMMC.SD_Error;
   FS_Status           : File_IO.Status_Code := Disk_Error;
   FD                  : File_IO.File_Descriptor;
   File_Size           : File_IO.File_Size   := -1;
   Loop_Iter           : Natural             := 0;
   Frame               : STM32.CAN.CAN_Frame (STM32.CAN.Standard);
   CAN_Status          : STM32.CAN.CAN_Status;
   Period              : constant Time_Span  := Microseconds (100_000);
   Next_Release        : Time                := Clock;
   Last_Steering_Angle : Long_Float := CAN_Handler.Steering_Angle_Degrees;
begin
   Cortex_M.Cache.Disable_D_Cache;

   -- Misc initialization
   Initialize_SDRAM;

   -- Stuff for LCD initialization
   Clear_Screen;
   Set_Font (Font8x8);

   -- CAN Bus Initialization
   STM32.CAN.Initialize (STM32.CAN.CAN_1, STM32.CAN.CAN_500K);

   declare
      Filter0 : constant CAN_Filter_Config :=
        (Bank => 0, FIFO => 0, Mode => List, ID_Type => STM32.CAN.Standard,
         Id1        => 16#118#, Id2 => 16#129#, Mask1 => 0, Mask2 => 0,
         Use_32_Bit => True);

      Filter1 : constant CAN_Filter_Config :=
        (Bank => 1, FIFO => 0, Mode => List, ID_Type => STM32.CAN.Standard,
         Id1        => 16#257#, Id2 => 16#3F5#, Mask1 => 0, Mask2 => 0,
         Use_32_Bit => True);

      Filter2 : constant CAN_Filter_Config :=
        (Bank => 2, FIFO => 0, Mode => List, ID_Type => STM32.CAN.Standard,
         Id1        => 16#405#, Id2 => 16#33A#, Mask1 => 0, Mask2 => 0,
         Use_32_Bit => True);

      Filter3 : constant CAN_Filter_Config :=
        (Bank => 3, FIFO => 0, Mode => List, ID_Type => STM32.CAN.Standard,
         Id1        => 16#266#, Id2 => 16#383#, Mask1 => 0, Mask2 => 0,
         Use_32_Bit => True);

      --  Filter4 : constant CAN_Filter_Config :=
      --    (Bank => 4, FIFO => 0, Mode => List, ID_Type => STM32.CAN.Standard,
      --     Id1        => 16#132#, Id2 => 16#000#, Mask1 => 0, Mask2 => 0,
      --     Use_32_Bit => True);
   begin
      Configure_Filter (CAN_1, Filter0);
      Configure_Filter (CAN_1, Filter1);
      Configure_Filter (CAN_1, Filter2);
      Configure_Filter (CAN_1, Filter3);
      --  Configure_Filter (CAN_1, Filter4);
   end;

   -- SD Card initialization
   STM32.Board.SDCard_Device.Initialize;

   delay 0.5;

   SDMMC_Status := STM32.SDMMC.Initialize (STM32.Device.SDMMC_1);
   FS_Status    := Mount_Drive ("sdcard", STM32.Board.SDCard_Device'Access);
   FS_Status    := File_IO.Open (FD, "/sdcard/m3.bmp", File_IO.Read_Write);

   if (FS_Status = OK) then
      GUI.Current_Background_Color :=
        (Alpha => 255, Red => 26, Green => 36, Blue => 46);

      Fill_Rounded_Rectangle
        (Rect   => (Position => (7, 6), Width => 161, Height => 82),
         Color  => (Alpha => 255, Red => 26, Green => 36, Blue => 46),
         Radius => 8);

      GUI.Bitmap.Draw_Image_From_File (FD);
   end if;

   -- Top Row
   Draw_Info (Point => (X => 9, Y => 122), Text => "Range");
   Draw_Info (Point => (X => 128, Y => 122), Text => "HV Battery");
   Draw_Info (Point => (X => 248, Y => 122), Text => "Temperature");
   Draw_Info (Point => (X => 367, Y => 122), Text => "Humidity");

   -- Bottom Row
   Draw_Info (Point => (X => 9, Y => 198), Text => "Speed (MPH)");
   Draw_Info (Point => (X => 128, Y => 198), Text => "Charge (%)");
   Draw_Info (Point => (X => 248, Y => 198), Text => "Gear");
   Draw_Info (Point => (X => 367, Y => 198), Text => "Power (kW)");

   GUI.Current_Background_Color := GUI.Default_Background_Color;

   Fill_Rounded_Rectangle
     (Rect   => (Position => (7, 92), Width => 161, Height => 20),
      Color  => (Alpha => 255, Red => 26, Green => 36, Blue => 46),
      Radius => 4);

   GUI.Current_Background_Color :=
     (Alpha => 255, Red => 26, Green => 36, Blue => 46);

   GUI.Current_Text_Color :=
     (Alpha => 255, Red => 76, Green => 174, Blue => 80);
   Put
     (X =>
        7 + 161 - (161 / 2) -
        (MeasureText ("CAN Connected", Font8x8).Width - 12) / 2,
      Y => 98, Msg => "CAN Connected");

   GUI.Images.Draw_Image (X0 => 24, Y0 => 94, Image => GUI.Images.Link_On);

   Display.Update_Layer (1, True);

   GUI.Current_Background_Color := GUI.Default_Background_Color;
   GUI.Current_Text_Color       := GUI.Default_Text_Color;

   Receiver.Register_Handler
     (ID => CAN_Standard_ID (16#129#),
      CB => CAN_Handler.On_SteeringAngle'Access);

   Receiver.Register_Handler
     (ID => CAN_Standard_ID (16#257#), CB => CAN_Handler.On_Speed'Access);

   Receiver.Register_Handler
     (ID => CAN_Standard_ID (16#118#),
      CB => CAN_Handler.On_DriverSystemStatus'Access);

   Receiver.Register_Handler
     (ID => CAN_Standard_ID (16#3F5#), CB => CAN_Handler.On_Lighting'Access);

   Receiver.Register_Handler
     (ID => CAN_Standard_ID (16#405#), CB => CAN_Handler.On_VIN'Access);

   Receiver.Register_Handler
     (ID => CAN_Standard_ID (16#33A#), CB => CAN_Handler.On_RangeSOC'Access);

   --  Receiver.Register_Handler
   --    (ID => CAN_Standard_ID (16#132#),
   --     CB => CAN_Handler.On_HVBattAmpVolt'Access);

   Receiver.Register_Handler
     (ID => CAN_Standard_ID (16#383#), CB => CAN_Handler.On_THSStatus'Access);

   Receiver.Register_Handler
     (ID => CAN_Standard_ID (16#266#),
      CB => CAN_Handler.On_RearInverterPower'Access);

   loop
      -- Top Row
      Draw_Info
        (Point => (X => 9, Y => 135), Text => "Range",
         Val   => CAN_Handler.Range_Miles'Image & "mi ");

      Draw_Info
        (Point => (X => 128, Y => 135), Text => "HV Battery",
         Val   => Integer (CAN_Handler.HV_Battery_Voltage)'Image & "V ");

      Draw_Info
        (Point => (X => 248, Y => 135), Text => "Temperature",
         Val   => Integer (CAN_Handler.Temperature)'Image & "C ");

      Draw_Info
        (Point => (X => 367, Y => 135), Text => "Humidity",
         Val   => Integer (CAN_Handler.Humidity)'Image & "% ");

      -- Bottom Row
      Draw_Info
        (Point => (X => 9, Y => 206), Text => "Speed (MPH)",
         Val   => CAN_Handler.Vehicle_Speed_MPH'Image & " ");

      Draw_Info
        (Point => (X => 248, Y => 206), Text => "Gear",
         Val   => CAN_Handler.To_String (CAN_Handler.Vehicle_Gear));

      Draw_Info
        (Point => (X => 367, Y => 198), Text => "Power (kW)",
         Val   => CAN_Handler.Rear_Power_kW'Image & " ");

      GUI.Images.Draw_Image
        (X0            => 230, Y0 => 45, Image => GUI.Images.Steering_Wheel,
         Angle_Degrees => CAN_Handler.Steering_Angle_Degrees);

      if CAN_Handler.Left_Turn_Signal then
         GUI.Images.Draw_Image
           (X0            => 190, Y0 => 65, Image => GUI.Images.Arrow,
            Angle_Degrees => 0.0);
      else
         Draw_Rectangle
           (Rect  => (Position => (190, 65), Width => 32, Height => 32),
            Color => GUI.Default_Background_Color);
      end if;

      if CAN_Handler.Right_Turn_Signal then
         GUI.Images.Draw_Image
           (X0            => 310, Y0 => 65, Image => GUI.Images.Arrow,
            Angle_Degrees => 180.0);
      else
         Draw_Rectangle
           (Rect  => (Position => (310, 65), Width => 32, Height => 32),
            Color => GUI.Default_Background_Color);
      end if;

      Put (X => 173, Y => 9, Msg => "Make:");
      Put (X => 228, Y => 9, Msg => "Tesla");
      Put (X => 173, Y => 19, Msg => "Model:");
      Put (X => 228, Y => 19, Msg => "Model 3");
      Put (X => 173, Y => 29, Msg => "VIN:");
      Put (X => 228, Y => 29, Msg => CAN_Handler.VIN_Number);

      Display.Update_Layer (1, True);

      Next_Release := Next_Release + Period;
      delay until Next_Release;
   end loop;
   -- Close (FD);
end bxcan;
