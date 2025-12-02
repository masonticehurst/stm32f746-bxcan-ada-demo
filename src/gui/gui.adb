with STM32.Board;             use STM32.Board;
with STM32.CAN;
with Bitmapped_Drawing;
with Bitmap_Color_Conversion; use Bitmap_Color_Conversion;
with GUI.Images;
with Ada.Real_Time;           use Ada.Real_Time;

package body GUI is

   --  We don't make the current font visible to clients because changing it
   --  requires recomputation of the screen layout (eg the char height) and
   --  issuance of commands to the LCD component driver (eg to refill).

   Current_Font : BMP_Font := Default_Font;

   Width_Margin  : constant Natural := 0;
   Height_Margin : constant Natural := 2;

   Char_Width  : Natural := BMP_Fonts.Char_Width (Current_Font) + Width_Margin;
   Char_Height : Natural :=
     BMP_Fonts.Char_Height (Current_Font) + Height_Margin;

   --  The last place on the current "line" on the LCD where a char of the
   --  current font size can be printed
   Max_Width : Natural := LCD_Natural_Width - Char_Width;

   --  The last "line" on the LCD where a char of this current font size can be
   --  printed
   Max_Height : Natural := LCD_Natural_Height - Char_Height;

   --  The current "line" that the text will appear upon. Note this wraps
   --  around to the top of the screen.
   Current_Y : Natural := 0;

   --  The number of characters currently printed on the current line
   Char_Count : Natural := 0;

   Initialized : Boolean := False;

   --  Convenience routine for call Drawing.Draw_Char
   procedure Draw_Char (X, Y : Natural; Msg : Character);

   --  Determins the max height and width for the specified font, given the
   --  current LCD orientation
   procedure Recompute_Screen_Dimensions (Font : BMP_Font);

   --  Ensures that the LCD display is initialized and DMA2D
   --  is up and running
   procedure Check_Initialized with
     Inline;

   --  Puts a new String in the frame buffer
   procedure Internal_Put (Msg : String);

   --  Puts a new character in the frame buffer.
   procedure Internal_Put (Msg : Character);

   -----------------------
   -- Check_Initialized --
   -----------------------

   procedure Check_Initialized is
   begin
      if Initialized then
         return;
      end if;

      Initialized := True;

      if Display.Initialized then
         --  Ensure we use polling here: LCD_Std_Out may be called from the
         --  Last chance handler, and we don't want unexpected tasks or
         --  protected objects calling an entry not meant for that
         Display.Set_Mode (HAL.Framebuffer.Polling);
      else
         Display.Initialize (Mode => HAL.Framebuffer.Polling);
         Display.Initialize_Layer (1, HAL.Bitmap.RGB_565);
         Clear_Screen;
      end if;
   end Check_Initialized;

   ---------------------------------
   -- Recompute_Screen_Dimensions --
   ---------------------------------

   procedure Recompute_Screen_Dimensions (Font : BMP_Font) is
   begin
      Check_Initialized;
      Char_Width  := BMP_Fonts.Char_Width (Font);
      Char_Height := BMP_Fonts.Char_Height (Font);
      Max_Width   := Display.Width - Char_Width - 1;
      Max_Height  := Display.Height - Char_Height - 1;
   end Recompute_Screen_Dimensions;

   --------------
   -- Set_Font --
   --------------

   procedure Set_Font (To : BMP_Font) is
   begin
      Current_Font := To;
      Recompute_Screen_Dimensions (Current_Font);
   end Set_Font;

   ---------------------
   -- Set_Orientation --
   ---------------------

   procedure Set_Orientation (To : HAL.Framebuffer.Display_Orientation) is
   begin
      Display.Set_Orientation (To);
      Recompute_Screen_Dimensions (Current_Font);
      Clear_Screen;
   end Set_Orientation;

   ------------------
   -- Clear_Screen --
   ------------------

   procedure Clear_Screen is
   begin
      Check_Initialized;
      Display.Hidden_Buffer (1).Set_Source (Current_Background_Color);
      Display.Hidden_Buffer (1).Fill;
      Current_Y  := 0;
      Char_Count := 0;
      Display.Update_Layer (1, True);
   end Clear_Screen;

   ------------------
   -- Internal_Put --
   ------------------

   procedure Internal_Put (Msg : String) is
   begin
      for C of Msg loop
         if C = ASCII.LF then
            New_Line;
         else
            Internal_Put (C);
         end if;
      end loop;
   end Internal_Put;

   ---------
   -- Put --
   ---------

   procedure Put (Msg : String) is
   begin
      Internal_Put (Msg);
      Display.Update_Layer (1, True);
   end Put;

   ---------------
   -- Draw_Char --
   ---------------

   procedure Draw_Char (X, Y : Natural; Msg : Character) is
   begin
      Check_Initialized;
      Bitmapped_Drawing.Draw_Char
        (Display.Hidden_Buffer (1).all, Start => (X, Y), Char => Msg,
         Font                                 => Current_Font,
         Foreground                           =>
           Bitmap_Color_To_Word (Display.Color_Mode (1), Current_Text_Color),
         Background                           =>
           Bitmap_Color_To_Word
             (Display.Color_Mode (1), Current_Background_Color));
   end Draw_Char;

   ---------
   -- Put --
   ---------

   procedure Internal_Put (Msg : Character) is
      X : Natural;
   begin
      if Char_Count * Char_Width > Max_Width then
         --  go to the next line down
         Current_Y := Current_Y + Char_Height;
         if Current_Y > Max_Height then
            Current_Y := 0;
         end if;
         --  and start at beginning of the line
         X          := 0;
         Char_Count := 0;
      else
         X := Char_Count * Char_Width;
      end if;

      Draw_Char (X, Current_Y, Msg);
      Char_Count := Char_Count + 1;
   end Internal_Put;

   ---------
   -- Put --
   ---------

   procedure Put (Msg : Character) is
   begin
      Internal_Put (Msg);
      Display.Update_Layer (1, True);
   end Put;

   --------------
   -- New_Line --
   --------------

   procedure New_Line is
   begin
      Char_Count := 0; -- next char printed will be at the start of a new line
      if Current_Y + Char_Height > Max_Height then
         Current_Y := 0;
      else
         Current_Y := Current_Y + Char_Height + Height_Margin;
      end if;
   end New_Line;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line
     (Msg : String; Color : HAL.Bitmap.Bitmap_Color := Default_Text_Color)
   is
   begin
      Current_Text_Color := Color;
      Put (Msg);
      New_Line;
      Current_Text_Color := Default_Text_Color;
   end Put_Line;

   ---------
   -- Put --
   ---------

   procedure Put (X, Y : Natural; Msg : Character) is
   begin
      Draw_Char (X, Y, Msg);
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put (X, Y : Natural; Msg : String) is
      Count  : Natural := 0;
      Next_X : Natural;
   begin
      for C of Msg loop
         Next_X := X + Count * Char_Width;
         Draw_Char (Next_X, Y, C);
         Count := Count + 1;
      end loop;
   end Put;

   procedure Draw_Filled_Circle
     (Point : HAL.Bitmap.Point; Radius : Natural;
      Color : HAL.Bitmap.Bitmap_Color)
   is
   begin
      HAL.Bitmap.Set_Source
        (Buffer => Display.Hidden_Buffer (1).all, ARGB => Color);

      HAL.Bitmap.Fill_Circle
        (Buffer => Display.Hidden_Buffer (1).all, Center => Point,
         Radius => Radius);
   end Draw_Filled_Circle;

   procedure Draw_Rectangle
     (Rect : HAL.Bitmap.Rect; Color : HAL.Bitmap.Bitmap_Color)
   is
   begin
      HAL.Bitmap.Set_Source
        (Buffer => Display.Hidden_Buffer (1).all, ARGB => Color);
      HAL.Bitmap.Fill_Rect
        (Buffer => Display.Hidden_Buffer (1).all, Area => Rect);
   end Draw_Rectangle;

   procedure Draw_Rounded_Rectangle
     (Rect   : HAL.Bitmap.Rect; Color : HAL.Bitmap.Bitmap_Color;
      Radius : Natural; Thickness : Natural)
   is
   begin
      HAL.Bitmap.Set_Source
        (Buffer => Display.Hidden_Buffer (1).all, ARGB => Color);
      HAL.Bitmap.Draw_Rounded_Rect
        (Buffer => Display.Hidden_Buffer (1).all, Area => Rect,
         Radius => Radius, Thickness => Thickness);
   end Draw_Rounded_Rectangle;

   procedure Fill_Rounded_Rectangle
     (Rect   : HAL.Bitmap.Rect; Color : HAL.Bitmap.Bitmap_Color;
      Radius : Natural)
   is
   begin
      HAL.Bitmap.Set_Source
        (Buffer => Display.Hidden_Buffer (1).all, ARGB => Color);
      HAL.Bitmap.Fill_Rounded_Rect
        (Buffer => Display.Hidden_Buffer (1).all, Area => Rect,
         Radius => Radius);
   end Fill_Rounded_Rectangle;

   procedure Add_Button
     (Rect : HAL.Bitmap.Rect; Color : HAL.Bitmap.Bitmap_Color;
      T    : Access_String)
   is
      Btn : constant Button := (Rect, Color, T);
   begin
      Buttons.Append (Btn);
   end Add_Button;

   procedure Draw_Info
     (Point : HAL.Bitmap.Point; Text : String; Val : String := "")
   is
      Text_Size : Size             := (0, 0);
      Text_Pos  : HAL.Bitmap.Point := (0, 0);
   begin
      if Val /= "" then
         GUI.Current_Background_Color :=
           (Alpha => 255, Red => 26, Green => 36, Blue => 46);

         -- Measuring font so we can center it properly using midpoint formula
         Set_Font (Font16x24);
         Text_Size := MeasureText (Val, Font16x24);
         Text_Pos  :=
           (X => Point.X + 103 / 2 - Text_Size.Width / 2, Y => Point.Y + 20);

         -- Clear any potential larger text that was written prior
         Draw_Rectangle
           (Rect  =>
              (Position => (Point.X, Point.Y + 20), Width => 103,
               Height   => 24),
            Color => (Alpha => 255, Red => 26, Green => 36, Blue => 46));

         -- Put value on screen
         Put (X => Text_Pos.X, Y => Text_Pos.Y, Msg => Val);

         -- Resetting font back to 8x8
         Set_Font (Font8x8);

         GUI.Current_Background_Color := GUI.Default_Background_Color;
      else
         -- Draw info box
         Fill_Rounded_Rectangle
           (Rect   =>
              (Position => (Point.X, Point.Y), Width => 103, Height => 66),
            Color  => (Alpha => 255, Red => 26, Green => 36, Blue => 46),
            Radius => 8);

         -- Measure text size so we can center it properly using midpoint formula
         Text_Size := MeasureText (Text, Font8x8);

         -- Put header value on screen
         Put
           (X   => Point.X + 103 / 2 - Text_Size.Width / 2, Y => Point.Y + 6,
            Msg => Text);
      end if;
   end Draw_Info;

   function MeasureText (Text : String; Font : BMP_Fonts.BMP_Font) return Size
   is
   begin
      return
        (Text'Length * BMP_Fonts.Char_Width (Font),
         BMP_Fonts.Char_Height (Font));
   end MeasureText;

   procedure Draw_Static_UI is
   begin
      Put (X => 173, Y => 9, Msg => "Make:");
      Put (X => 228, Y => 9, Msg => "Tesla");

      Put (X => 173, Y => 19, Msg => "Model:");
      Put (X => 228, Y => 19, Msg => "Model 3");

      Put (X => 173, Y => 29, Msg => "VIN:");

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

      Current_Background_Color := Default_Background_Color;

      Current_Background_Color :=
        (Alpha => 255, Red => 26, Green => 36, Blue => 46);

      Display.Update_Layer (1, True);
   end Draw_Static_UI;

   function VIN_Complete (S : String) return Boolean is
   begin
      --  Consider VIN "complete" when no character is NUL or space
      for C of S loop
         if C = Character'Val (0) or else C = ' ' then
            return False;
         end if;
      end loop;
      return True;
   end VIN_Complete;

   procedure Update_Range_If_Changed is
      Curr     : constant Natural := CAN_Handler.Range_Miles;
      Str      : constant String  := Curr'Image;
      Str_Trim : constant String  := Str (Str'First + 1 .. Str'Last);
   begin
      if Curr /= Last_Range_Miles then
         GUI.Draw_Info
           (Point => (X => 9, Y => 135), Text => "Range",
            Val   => Str_Trim & "mi");

         Last_Range_Miles := Curr;
      end if;
   end Update_Range_If_Changed;

   procedure Update_Speed_If_Changed is
      Curr : constant Natural := CAN_Handler.Vehicle_Speed_MPH;
   begin
      if Curr /= Last_Speed_MPH then
         GUI.Draw_Info
           (Point => (X => 9, Y => 206), Text => "Speed (MPH)",
            Val   => Curr'Image & " ");

         Last_Speed_MPH := Curr;
      end if;
   end Update_Speed_If_Changed;

   procedure Update_Gear_If_Changed is
      Curr : constant CAN_Handler.Gear := CAN_Handler.Vehicle_Gear;
      use type CAN_Handler.Gear;
   begin
      if Curr /= Last_Gear then
         GUI.Draw_Info
           (Point => (X => 248, Y => 206), Text => "Gear",
            Val   => CAN_Handler.To_String (Curr));

         Last_Gear := Curr;
      end if;
   end Update_Gear_If_Changed;

   procedure Update_Power_If_Changed is
      Curr : constant Integer := CAN_Handler.Rear_Power_kW;
   begin
      if abs (Curr - Last_Rear_Power_kW) > 1 then
         GUI.Draw_Info
           (Point => (X => 367, Y => 206), Text => "Power (kW)",
            Val   => Curr'Image & " ");

         Last_Rear_Power_kW := Curr;
      end if;
   end Update_Power_If_Changed;

   procedure Update_Turn_Signals_If_Changed is
   begin
      --  Left turn signal
      if CAN_Handler.Left_Turn_Signal /= Last_Left_Turn then
         if CAN_Handler.Left_Turn_Signal then
            GUI.Images.Draw_Image
              (X0            => 190, Y0 => 65, Image => GUI.Images.Arrow,
               Angle_Degrees => 0);
         else
            GUI.Draw_Rectangle
              (Rect  => (Position => (190, 65), Width => 32, Height => 32),
               Color => GUI.Default_Background_Color);
         end if;

         Last_Left_Turn := CAN_Handler.Left_Turn_Signal;
      end if;

      --  Right turn signal
      if CAN_Handler.Right_Turn_Signal /= Last_Right_Turn then
         if CAN_Handler.Right_Turn_Signal then
            GUI.Images.Draw_Image
              (X0            => 310, Y0 => 65, Image => GUI.Images.Arrow,
               Angle_Degrees => 180);
         else
            GUI.Draw_Rectangle
              (Rect  => (Position => (310, 65), Width => 32, Height => 32),
               Color => GUI.Default_Background_Color);
         end if;

         Last_Right_Turn := CAN_Handler.Right_Turn_Signal;
      end if;
   end Update_Turn_Signals_If_Changed;

   procedure Update_Steering_Wheel_If_Changed is
   begin
      if abs (CAN_Handler.Steering_Angle_Degrees - Last_Steering_Angle) >=
        Steering_Threshold
      then
         Draw_Filled_Circle
           (Point => (X => 258, Y => 80), Radius => 27,
            Color =>
              (Alpha => 255, Red => 0, Green => 16#6F#, Blue => 16#FD#));

         GUI.Images.Draw_Image
           (X0            => 226, Y0 => 49, Image => GUI.Images.Steering_Wheel,
            Angle_Degrees => CAN_Handler.Steering_Angle_Degrees);

         Last_Steering_Angle := CAN_Handler.Steering_Angle_Degrees;
      end if;
   end Update_Steering_Wheel_If_Changed;

   procedure Update_VIN_If_Completed is
      Curr_VIN : constant String := CAN_Handler.VIN_Number;
   begin
      --  Only draw once all characters look valid AND the VIN has changed
      if VIN_Complete (Curr_VIN) and then Curr_VIN /= Last_VIN then
         --  We assume labels "VIN:" and static Make/Model were already drawn
         GUI.Put (X => 228, Y => 29, Msg => Curr_VIN);

         Last_VIN := Curr_VIN;
      end if;
   end Update_VIN_If_Completed;

   procedure Update_Link_Status is
      CAN_Connected    : constant String := "CAN Connected";
      CAN_Disconnected : constant String := "CAN Disconnected";
   begin
      Current_Background_Color :=
        (Alpha => 255, Red => 26, Green => 36, Blue => 46);

      Fill_Rounded_Rectangle
        (Rect   => (Position => (7, 92), Width => 161, Height => 20),
         Color  => (Alpha => 255, Red => 26, Green => 36, Blue => 46),
         Radius => 4);

      if Clock - STM32.CAN.Last_RX_Time > Seconds (5) then
         Current_Text_Color :=
           (Alpha => 255, Red => 228, Green => 52, Blue => 52);

         Put
           (X =>
              8 + 161 - (161 / 2) -
              (MeasureText (CAN_Disconnected, Font8x8).Width - 12) / 2,
            Y => 98, Msg => CAN_Disconnected);

         GUI.Images.Draw_Image
           (X0 => 14, Y0 => 94, Image => GUI.Images.Link_Off);
      else
         Current_Text_Color :=
           (Alpha => 255, Red => 76, Green => 174, Blue => 80);

         Put
           (X =>
              7 + 161 - (161 / 2) -
              (MeasureText (CAN_Connected, Font8x8).Width - 12) / 2,
            Y => 98, Msg => CAN_Connected);

         GUI.Images.Draw_Image
           (X0 => 24, Y0 => 94, Image => GUI.Images.Link_On);
      end if;

      Current_Text_Color       := Default_Text_Color;
      Current_Background_Color := Default_Background_Color;
   end Update_Link_Status;
end GUI;
