------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2015-2016, AdaCore                     --
--                                                                          --
--  Redistribution and use in source and binary forms, with or without      --
--  modification, are permitted provided that the following conditions are  --
--  met:                                                                    --
--     1. Redistributions of source code must retain the above copyright    --
--        notice, this list of conditions and the following disclaimer.     --
--     2. Redistributions in binary form must reproduce the above copyright --
--        notice, this list of conditions and the following disclaimer in   --
--        the documentation and/or other materials provided with the        --
--        distribution.                                                     --
--     3. Neither the name of the copyright holder nor the names of its     --
--        contributors may be used to endorse or promote products derived   --
--        from this software without specific prior written permission.     --
--                                                                          --
--   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS    --
--   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT      --
--   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR  --
--   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT   --
--   HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, --
--   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT       --
--   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,  --
--   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY  --
--   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT    --
--   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE  --
--   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.   --
--                                                                          --
------------------------------------------------------------------------------

with STM32.Board;             use STM32.Board;
with Bitmapped_Drawing;
with Bitmap_Color_Conversion; use Bitmap_Color_Conversion;

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

   Max_Width : Natural := LCD_Natural_Width - Char_Width;
   --  The last place on the current "line" on the LCD where a char of the
   --  current font size can be printed

   Max_Height : Natural := LCD_Natural_Height - Char_Height;
   --  The last "line" on the LCD where a char of this current font size can be
   --  printed

   Current_Y : Natural := 0;
   --  The current "line" that the text will appear upon. Note this wraps
   --  around to the top of the screen.

   Char_Count : Natural := 0;
   --  The number of characters currently printed on the current line

   Initialized : Boolean := False;

   procedure Draw_Char (X, Y : Natural; Msg : Character);
   --  Convenience routine for call Drawing.Draw_Char

   procedure Recompute_Screen_Dimensions (Font : BMP_Font);
   --  Determins the max height and width for the specified font, given the
   --  current LCD orientation

   procedure Check_Initialized with
     Inline;
   --  Ensures that the LCD display is initialized and DMA2D
   --  is up and running

   procedure Internal_Put (Msg : String);
   --  Puts a new String in the frame buffer

   procedure Internal_Put (Msg : Character);
   --  Puts a new character in the frame buffer.

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

   procedure Set_Font (To : in BMP_Font) is
   begin
      Current_Font := To;
      Recompute_Screen_Dimensions (Current_Font);
   end Set_Font;

   ---------------------
   -- Set_Orientation --
   ---------------------

   procedure Set_Orientation (To : in HAL.Framebuffer.Display_Orientation) is
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
      Display.Update_Layer (1, True);
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

      Display.Update_Layer (1, True);
   end Put;

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
     (Point : HAl.Bitmap.Point; Text : String; Val : String := "")
   is
      Text_Size : Size := (0, 0);
   begin
      if Val /= "" then
         Set_Font (Font16x24);
         Text_Size := MeasureText (Val, Font16x24);
         Put
           (X   => Point.X + 103 / 2 - Text_Size.Width / 2, Y => Point.Y + 20,
            Msg => Val);
         Set_Font (Font8x8);
      else
         Fill_Rounded_Rectangle
           (Rect   =>
              (Position => (Point.X, Point.Y), Width => 103, Height => 66),
            Color  => (Alpha => 255, Red => 26, Green => 36, Blue => 46),
            Radius => 8);

         Text_Size := MeasureText (Text, Font8x8);

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
end GUI;
