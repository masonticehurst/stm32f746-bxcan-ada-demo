with GUI;         use GUI;
with STM32.Board; use STM32.Board;
with File_IO;     use File_IO;

package body GUI.Bitmap is

   function Blend
     (Foreground : Bitmap_Color; Background : Bitmap_Color) return Bitmap_Color
   is
      -- Convert alpha from 0..255 -> 0.0 .. 1.0
      A : constant Float := Float (Foreground.Alpha) / 255.0;

      -- Helper to blend a single channel
      function Blend_Channel (F, B : UInt8) return UInt8 is
         V : constant Float := A * Float (F) + (1.0 - A) * Float (B);
      begin
         return UInt8 (Integer (Float'Rounding (V)));
      end Blend_Channel;

      Result : Bitmap_Color;
   begin
      Result.Alpha := 255;  -- Result is fully opaque after compositing
      Result.Red   := Blend_Channel (Foreground.Red, Background.Red);
      Result.Green := Blend_Channel (Foreground.Green, Background.Green);
      Result.Blue  := Blend_Channel (Foreground.Blue, Background.Blue);

      return Result;
   end Blend;

   procedure Draw_Image_From_File (FD : File_IO.File_Descriptor) is
      BMP_Header      : Bitmap_Header;
      BMP_Info_Header : Bitmap_Info_Header;
      Width           : Natural   := 0;
      Height          : Natural   := 0;
      Bits            : Uint16    := 0;
      Num_Colors      : Natural   := 0;
      Bytes_Read      : File_Size := 0;
   begin
      Bytes_Read := Read (FD, BMP_Header'Address, Bitmap_Header'Size / 8);
      Bytes_Read :=
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

         Data_Offset : constant File_IO.File_Size :=
           File_IO.File_Size (BMP_Header.Data_Offset) -
           Bitmap_Info_Header'Size / 8 - Bitmap_Header'Size / 8;
         Dummy_Bytes : Byte_Array (0 .. Natural (Data_Offset) - 1);
      begin
         if Num_Colors > 0 then
            -- Read palette
            Bytes_Read :=
              Read
                (FD, Color_Table'Address,
                 File_IO.File_Size (Color_Table'Size / 8));
         end if;

         -- Skip to pixel data offset
         Bytes_Read := Read (FD, Dummy_Bytes'Address, Data_Offset);

         if Bits = 32 then
            ----------------------------------------------------------------
            -- 32 bpp: BGR (little-endian: B,G,R,A in memory)
            -- We read from the file in 512-byte chunks and consume 4 bytes
            -- at a time to fill Bitmap.Pixel (B,G,R,A).
            ----------------------------------------------------------------
            declare
               -- Pixel we draw with
               P : GUI.Bitmap.Pixel;

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
                  Remaining : constant File_IO.File_Size := Buf_Len - Buf_Pos;
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
                     To_Read     : File_IO.File_Size          := Max_To_Read;
                     Read_Len    : File_IO.File_Size;
                  begin
                     if To_Read > 0 then
                        Read_Len :=
                          File_IO.Read
                            (FD, Buffer (Integer (Buf_Len))'Address, To_Read);
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
                           -- Short/failed read
                           exit;
                        end if;

                        Display.Hidden_Buffer (1).Set_Source
                          (ARGB =>
                             GUI.Bitmap.Blend
                               ((Alpha => P.A, Red => P.R, Green => P.G,
                                 Blue  => P.B),
                                (Alpha => Current_Background_Color.Alpha,
                                 Red   => Current_Background_Color.Red,
                                 Green => Current_Background_Color.Green,
                                 Blue  => Current_Background_Color.Blue)));
                        Display.Hidden_Buffer (1).Set_Pixel
                          (Pt => (X => X + 20, Y => Y_Screen + 20));
                     end loop;

                     -- After all pixels in this row are written to the hidden buffer:
                     Display.Update_Layer (1, True);
                  end;
               end loop;
            end;
         end if;
      end;  -- color table + raster
   end Draw_Image_From_File;

end GUI.Bitmap;
