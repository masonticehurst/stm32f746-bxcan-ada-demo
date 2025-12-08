with Touch_Handler;
with Cortex_M.Cache;
with Exception_Handler;
with SDCard;

with Ada.Real_Time;       use Ada.Real_Time;
with STM32.Board;         use STM32.Board;
with STM32.Device;        use STM32.Device;
with HAL.SDMMC;           use HAL.SDMMC;
with HAL.I2C;             use HAL.I2C;
with STM32.I2C;           use STM32.I2C;
with STM32.SDMMC;
with File_IO;             use File_IO;
with STM32.CAN;           use STM32.CAN;
with HAL;                 use HAL;
with BMP_Fonts;           use BMP_Fonts;
with GUI;                 use GUI;
with GUI.Bitmap;          use GUI.Bitmap;
with GUI.Images;          use GUI.Images;
with CAN_Handler;
with STM32.DS3231;        use STM32.DS3231;
with HAL.Real_Time_Clock; use HAL.Real_Time_Clock;
with HAL.Touch_Panel;     use HAL.Touch_Panel;
procedure can_ada is
   SDMMC_Status : HAL.SDMMC.SD_Error;
   FS_Status    : File_IO.Status_Code := Disk_Error;
   FD           : File_IO.File_Descriptor;
   Period       : constant Time_Span  := Microseconds (33_333);
   Next_Release : Time                := Clock;
   --  RTC :
   --    aliased DS3231_Device
   --      (I2C_Port => I2C_3'Access, I2C_Address => 16#68# * 2);
begin
   Cortex_M.Cache.Disable_D_Cache;

   -- Misc initialization
   Initialize_SDRAM;
   STM32.Board.Touch_Panel.Initialize;

   -- Stuff for LCD initialization
   Clear_Screen;
   Set_Font (Font8x8);

   -- CAN Bus Initialization
   STM32.CAN.Initialize
     (This => STM32.CAN.CAN_1, Speed => STM32.CAN.CAN_500K, Loopback => False);

   declare
      -- 0x118 = 280 = ID118DriveSystemStatus
      -- 0x129 = 297 = ID129SteeringAngle
      Filter0 : constant CAN_Filter_Config :=
        (Bank => 0, FIFO => 0, Mode => List, ID_Type => STM32.CAN.Standard,
         Id1        => 16#118#, Id2 => 16#129#, Mask1 => 0, Mask2 => 0,
         Use_32_Bit => True);

      -- 0x257 = 599 = ID257DIspeed
      -- 0x3F5 = 1013 = ID3F5VCFRONT_lighting
      Filter1 : constant CAN_Filter_Config :=
        (Bank => 1, FIFO => 0, Mode => List, ID_Type => STM32.CAN.Standard,
         Id1        => 16#257#, Id2 => 16#3F5#, Mask1 => 0, Mask2 => 0,
         Use_32_Bit => True);

      -- 0x405 = 1029 = ID405VIN
      -- 0x266 = 614 = ID266RearInverterPower
      Filter2 : constant CAN_Filter_Config :=
        (Bank => 2, FIFO => 0, Mode => List, ID_Type => STM32.CAN.Standard,
         Id1        => 16#405#, Id2 => 16#266#, Mask1 => 0, Mask2 => 0,
         Use_32_Bit => True);

      -- 0x383 = 899 = ID383VCRIGHT_thsStatus
      -- 0x33A = 826 = ID33AUI_rangeSOC
      Filter3 : constant CAN_Filter_Config :=
        (Bank => 3, FIFO => 0, Mode => List, ID_Type => STM32.CAN.Standard,
         Id1        => 16#383#, Id2 => 16#33A#, Mask1 => 0, Mask2 => 0,
         Use_32_Bit => True);

      -- 0x273 = 627 = ID273UI_vehicleControl
      -- 0x3B3 = 947 = ID3B3UI_vehicleControl2
      Filter4 : constant CAN_Filter_Config :=
        (Bank => 4, FIFO => 0, Mode => List, ID_Type => STM32.CAN.Standard,
         Id1        => 16#273#, Id2 => 16#3B3#, Mask1 => 0, Mask2 => 0,
         Use_32_Bit => True);

      -- 0x2F3 = 755 = ID2F3UI_hvacRequest
      Filter5 : constant CAN_Filter_Config :=
        (Bank => 4, FIFO => 0, Mode => List, ID_Type => STM32.CAN.Standard,
         Id1        => 16#2F3#, Id2 => 16#0FF#, Mask1 => 0, Mask2 => 0,
         Use_32_Bit => True);
   begin
      Configure_Filter (CAN_1, Filter0);
      Configure_Filter (CAN_1, Filter1);
      Configure_Filter (CAN_1, Filter2);
      Configure_Filter (CAN_1, Filter3);
      Configure_Filter (CAN_1, Filter4);
      Configure_Filter (CAN_1, Filter5);
   end;

   -- SD Card initialization
   SDCard_Device.Initialize;

   delay 0.5;

   SDMMC_Status := STM32.SDMMC.Initialize (SDMMC_1);
   FS_Status    := Mount_Drive ("sdcard", SDCard_Device'Access);

   if FS_Status /= Ok then
      raise Program_Error with "Unable to mount SD Card";
   end if;

   FS_Status := Open (FD, "/sdcard/m3.bmp", Read_Write);

   if (FS_Status = OK) then
      GUI.Current_Background_Color := Button_color;

      Fill_Rounded_Rectangle
        (Rect  => (Position => (7, 6), Width => 161, Height => 82),
         Color => Button_Color, Radius => 8);

      GUI.Bitmap.Draw_Image_From_File (FD);
   end if;

   GUI.Status_Page_Init;

   GUI.Current_Background_Color := Default_Background_Color;
   GUI.Current_Text_Color       := Default_Text_Color;

   STM32.CAN.Receiver.Register_Handler
     (ID => CAN_Standard_ID (16#118#),
      CB => CAN_Handler.On_DriverSystemStatus'Access);

   STM32.CAN.Receiver.Register_Handler
     (ID => CAN_Standard_ID (16#129#),
      CB => CAN_Handler.On_SteeringAngle'Access);

   STM32.CAN.Receiver.Register_Handler
     (ID => CAN_Standard_ID (16#257#), CB => CAN_Handler.On_Speed'Access);

   STM32.CAN.Receiver.Register_Handler
     (ID => CAN_Standard_ID (16#3F5#), CB => CAN_Handler.On_Lighting'Access);

   STM32.CAN.Receiver.Register_Handler
     (ID => CAN_Standard_ID (16#405#), CB => CAN_Handler.On_VIN'Access);

   STM32.CAN.Receiver.Register_Handler
     (ID => CAN_Standard_ID (16#33A#), CB => CAN_Handler.On_RangeSOC'Access);

   --  STM32.CAN.Receiver.Register_Handler
   --    (ID => CAN_Standard_ID (16#132#),
   --     CB => CAN_Handler.On_HVBattAmpVolt'Access);

   STM32.CAN.Receiver.Register_Handler
     (ID => CAN_Standard_ID (16#383#), CB => CAN_Handler.On_THSStatus'Access);

   STM32.CAN.Receiver.Register_Handler
     (ID => CAN_Standard_ID (16#266#),
      CB => CAN_Handler.On_RearInverterPower'Access);

   STM32.CAN.Receiver.Register_Handler
     (ID => CAN_Standard_ID (16#273#),
      CB => CAN_Handler.On_VehicleControl1'Access);

   STM32.CAN.Receiver.Register_Handler
     (ID => CAN_Standard_ID (16#3B3#),
      CB => CAN_Handler.On_VehicleControl2'Access);

   STM32.CAN.Receiver.Register_Handler
     (ID => CAN_Standard_ID (16#2F3#),
      CB => CAN_Handler.On_HVACRequest'Access);

   loop
      case GUI.Current_Page is
         when Status_Page =>
            GUI.Status_Page_Tick;
         when Control_Page =>
            GUI.Control_Page_Tick;
         when Capture_Page =>
            null;
      end case;

      Next_Release := Next_Release + Period;
      delay until Next_Release;
   end loop;
   -- Close (FD);
end can_ada;
