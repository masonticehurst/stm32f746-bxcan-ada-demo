with LED_Blinker;
with Exception_Handler;
with SDCard;
with Bitmap;

with Ada.Real_Time; use Ada.Real_Time;
with STM32.Board;   use STM32.Board;
with STM32.Device;  use STM32.Device;
with HAL.Bitmap;
with HAL.SDMMC;     use HAL.SDMMC;
with STM32.SDMMC;
with File_IO;       use File_IO;
with STM32.CAN;     use STM32.CAN;
with HAL;           use HAL;
with BMP_Fonts;     use BMP_Fonts;
with LCD_Std_Out;   use LCD_Std_Out;
with Bitmap;        use Bitmap;

procedure bxcan is
   SD_Card_Info : HAL.SDMMC.Card_Information;
   SDMMC_Status : HAL.SDMMC.SD_Error;
   FS_Status    : File_IO.Status_Code := Disk_Error;
   FD           : File_IO.File_Descriptor;
   File_Size    : File_IO.File_Size   := -1;
   Loop_Iter    : Natural             := 0;
   Frame        : STM32.CAN.CAN_Frame (STM32.CAN.Standard);
   CAN_Status   : STM32.CAN.CAN_Status;
begin
   -- Stuff for LCD initialization
   Clear_Screen;
   Set_Font (Font12x12);

   -- Misc initialization
   Initialize_SDRAM;

   -- CAN Bus Initialization
   STM32.CAN.Initialize (STM32.CAN.CAN_1, STM32.CAN.CAN_1M);

   -- SD Card initialization
   STM32.Board.SDCard_Device.Initialize;

   delay 0.5;

   SDMMC_Status := STM32.SDMMC.Initialize (STM32.Device.SDMMC_1);

   if (SDMMC_Status = OK) then
      Put_Line ("SD Card Initialization Successful");
   else
      Put_Line
        ("SD Card Initialization Failed: " & SD_Error'Image (SDMMC_Status));
   end if;

   FS_Status := Mount_Drive ("sdcard", STM32.Board.SDCard_Device'Access);

   if (FS_Status = OK) then
      Put_Line ("Mounted File System on /sdcard/");
   else
      Put_Line ("Failed to Mount File System on /sdcard/: " & FS_Status'Image);
   end if;

   FS_Status := File_IO.Open (FD, "/sdcard/m3.bmp", File_IO.Read_Write);

   if (FS_Status = OK) then

      Put_Line ("Opened /sdcard/m3.bmp successfully.");

      -- Read the BMP file header
      declare
         BMP_Header      : Bitmap_Header;
         BMP_Header_Size : File_IO.File_Size := Bitmap_Header'Size / 8;
         BMP_Info_Header : Bitmap_Info_Header;
         Width           : Natural           := 0;
         Height          : Natural           := 0;
         Bits            : Uint16            := 0;
         Num_Colors      : Natural           := 0;
      begin
         File_Size := Read (FD, BMP_Header'Address, Bitmap_Header'Size / 8);
         File_Size :=
           Read (FD, BMP_Info_Header'Address, Bitmap_Info_Header'Size / 8);

         Width  := Natural (BMP_Info_Header.Width);
         Height := Natural (BMP_Info_Header.Height);
         Bits   := BMP_Info_Header.BitCount;

         Num_Colors :=
           (if BMP_Info_Header.Colors_Used /= 0 then
              Natural (BMP_Info_Header.Colors_Used)
            elsif Bits in 1 | 4 | 8 then 2**Integer (Bits) else 0);

         declare
            Color_Table :
              Color_Table_Array
                (0 .. (if Num_Colors = 0 then 0 else Num_Colors - 1));

            Data_Offset : File_IO.File_Size :=
              File_IO.File_Size (BMP_Header.Data_Offset) -
              Bitmap_Info_Header'Size / 8 - Bitmap_Header'Size / 8;
            Dummy_Bytes : Byte_Array (0 .. Natural (Data_Offset) - 1);
         begin
            if Num_Colors > 0 then
               -- Read palette
               File_Size :=
                 Read
                   (FD, Color_Table'Address,
                    File_IO.File_Size (Color_Table'Size / 8));
            end if;

            -- Skip to pixel data offset
            File_Size := Read (FD, Dummy_Bytes'Address, Data_Offset);

            if Bits = 32 then
               ----------------------------------------------------------------
               -- 32 bpp: BGR (little-endian: B,G,R,A in memory)
               -- We read from the file in 512-byte chunks and consume 4 bytes
               -- at a time to fill Bitmap.Pixel (B,G,R,A).
               ----------------------------------------------------------------
               declare
                  -- Pixel we draw with
                  P : Bitmap.Pixel;

                  -- Simple byte buffer for 512-byte chunks
                  Buffer  : aliased Byte_Array (0 .. 511);
                  Buf_Len : File_IO.File_Size :=
                    0;  -- how many bytes currently in Buffer
                  Buf_Pos : File_IO.File_Size :=
                    0;  -- index of next unread byte in Buffer

                  ----------------------------------------------------------------
                  -- Refill the buffer so there are (ideally) up to 512 bytes
                  -- starting at Buffer (0). Preserves any unread bytes by
                  -- sliding them to the front.
                  ----------------------------------------------------------------
                  procedure Fill_Buffer is
                     Remaining : constant File_IO.File_Size :=
                       Buf_Len - Buf_Pos;
                  begin
                     -- Move any remaining unread bytes to the front
                     if Remaining > 0 then
                        for I in 0 .. Integer (Remaining) - 1 loop
                           Buffer (I) := Buffer (Integer (Buf_Pos) + I);
                        end loop;
                     end if;

                     Buf_Len := Remaining;
                     Buf_Pos := 0;

                     -- Read as many new bytes as possible (up to 512 total)
                     declare
                        Max_To_Read : constant File_IO.File_Size :=
                          File_IO.File_Size (Buffer'Length) -
                          Buf_Len;  -- 512 - Buf_Len
                        To_Read     : File_IO.File_Size := Max_To_Read;
                        Read_Len    : File_IO.File_Size;
                     begin
                        if To_Read > 0 then
                           Read_Len :=
                             Read
                               (FD, Buffer (Integer (Buf_Len))'Address,
                                To_Read);
                           Buf_Len  := Buf_Len + Read_Len;
                        end if;
                     end;
                  end Fill_Buffer;

                  ----------------------------------------------------------------
                  -- Get the next 4 bytes from the buffer into P.B, P.G, P.R, P.A
                  -- Returns False on EOF/short read.
                  ----------------------------------------------------------------
                  function Next_Pixel return Boolean is
                  begin
                     -- Ensure at least 4 bytes available; refill if needed
                     if Buf_Pos + 4 > Buf_Len then
                        Fill_Buffer;
                     end if;

                     -- Still not enough? We hit EOF or a short read.
                     if Buf_Pos + 4 > Buf_Len then
                        return False;
                     end if;

                     declare
                        Start : constant Natural := Natural (Buf_Pos);
                     begin
                        P.B := Uint8 (Buffer (Start));
                        P.G := Uint8 (Buffer (Start + 1));
                        P.R := Uint8 (Buffer (Start + 2));
                        P.A := Uint8 (Buffer (Start + 3));
                     end;

                     Buf_Pos := Buf_Pos + 4;
                     return True;
                  end Next_Pixel;

               begin
                  -- For each scanline
                  for Row_Idx in 0 .. Height - 1 loop

                     -- BMP is bottom-up when Height > 0
                     declare
                        Y_Screen : constant Natural := Height - 1 - Row_Idx;
                     begin
                        for X in 0 .. Width - 1 loop

                           if not Next_Pixel then
                              Put_Line
                                ("Short/failed read at (" & X'Image & "," &
                                 Y_Screen'Image & ")");
                              exit;  -- or raise an exception

                           end if;

                           Display.Hidden_Buffer (1).Set_Source (ARGB => Bitmap.Blend ((Alpha => P.A, Red => P.R, Green => P.G, Blue => P.B), (Alpha => Current_Background_Color.Alpha, Red => Current_Background_Color.Red, Green => Current_Background_Color.Green, Blue => Current_Background_Color.Blue)));
                           Display.Hidden_Buffer (1).Set_Pixel (Pt => (X => X, Y => Y_Screen));
                        end loop;

                        -- After all pixels in this row are written to the hidden buffer:
                        Display.Update_Layer (1, True);
                     end;
                  end loop;
               end;
            else
               Put_Line ("Unsupported BitCount: " & Integer (Bits)'Img);
            end if;
         end;  -- color table + raster
      end;
   else
      Put_Line ("Failed to Open /sdcard/m3crop.bmp: " & FS_Status'Image);
   end if;

   --  Frame.DLC         := 8;
   --  Frame.Standard_ID := 16#AA#;
   --  Frame.Data (1)    := 16#01#;
   --  Frame.Data (2)    := 16#02#;
   --  Frame.Data (3)    := 16#03#;
   --  Frame.Data (4)    := 16#04#;
   --  Frame.Data (5)    := 16#05#;
   --  Frame.Data (6)    := 16#06#;
   --  Frame.Data (7)    := 16#07#;
   --  Frame.Data (8)    := 16#08#;
   --  Frame.RTR         := False;

   Put_Line ("Done!");

   Fill_Rounded_Rectangle
     (Rect   => (Position => (100, 100), Width => 75, Height => 75),
      Color  => (Alpha => 255, Red => 26, Green => 36, Blue => 46),
      Radius => 8);

   loop
      Display.Update_Layer (1, True);
      delay 1.0;
   end loop;

   --  loop
   --     --  declare
   --     --     Received_Frame : STM32.CAN.CAN_Frame;
   --     --  begin
   --     --     CAN_Status := STM32.CAN.Receive (STM32.CAN.CAN_1, Received_Frame);
   --     --     if CAN_Status = STM32.CAN.OK then
   --     --        if Received_Frame.ID_Type = STM32.CAN.Standard then
   --     --           Put_Line
   --     --             ("Received Standard CAN Frame with ID: " &
   --     --              Received_Frame.Standard_ID'Image);
   --     --        else
   --     --           Put_Line
   --     --             ("Received Extended CAN Frame with ID: " &
   --     --              Received_Frame.Extended_ID'Image);
   --     --        end if;
   --     --     end if;
   --     --  end;

   --     --  CAN_Status := STM32.CAN.Transmit (STM32.CAN.CAN_1, Frame);
   --     --  Put_Line (CAN_Status'Image);

   --     --  if FS_Status = OK then
   --     --     declare
   --     --        FSize : File_Size := 0;
   --     --        Hello_World : aliased constant String := Loop_Iter'Image & ASCII.LF;
   --     --     begin
   --     --        if FS_Status = OK then
   --     --              FSize := Write (FD, Hello_World'Address, Hello_World'Length);
   --     --              FS_Status := Flush (FD);
   --     --        end if;
   --     --     end;
   --     --  else
   --     --     All_LEDs_Off;
   --     --  end if;
   --  end loop;

   -- Close (FD);
end bxcan;
