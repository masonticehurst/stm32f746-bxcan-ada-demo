with Ada.Numerics.Long_Elementary_Functions;

package body GUI.Images is
   procedure Draw_Image
     (X0, Y0        : Natural; Image : Bitmap_Color_Image;
      Angle_Degrees : Long_Float := 0.0)
   is
      use Ada.Numerics;
      use Ada.Numerics.Long_Elementary_Functions;

      Screen_Width  : constant Integer :=
        Integer (Display.Hidden_Buffer (1).Width);
      Screen_Height : constant Integer :=
        Integer (Display.Hidden_Buffer (1).Height);

      Img_Width  : constant Integer :=
        Integer (Image'Length (2));  -- columns (X)
      Img_Height : constant Integer := Integer (Image'Length (1));  -- rows (Y)

      -- Pivot at image center (in source coordinates)
      Cx : constant Long_Float := Long_Float (Img_Width - 1) / 2.0;
      Cy : constant Long_Float := Long_Float (Img_Height - 1) / 2.0;

      Angle_Rad : constant Long_Float := Angle_Degrees * Pi / 180.0;

      Cos_A : constant Long_Float := Cos (Angle_Rad);
      Sin_A : constant Long_Float := Sin (Angle_Rad);
   begin
      -- Fast path: no rotation → use your original loops
      if Angle_Degrees = 0.0 then
         for Row in Image'Range (1) loop
            for Col in Image'Range (2) loop
               declare
                  Dest_X : constant Natural :=
                    X0 + Natural (Col - Image'First (2));
                  Dest_Y : constant Natural :=
                    Y0 + Natural (Row - Image'First (1));
               begin
                  Display.Hidden_Buffer (1).Set_Source
                    (ARGB =>
                       Blend (Image (Row, Col), Current_Background_Color));
                  Display.Hidden_Buffer (1).Set_Pixel
                    (Pt => (X => Dest_X, Y => Dest_Y));
               end;
            end loop;
         end loop;

         return;
      end if;

      -- Rotated path
      for Row in Image'Range (1) loop
         for Col in Image'Range (2) loop
            declare
               -- Source coordinates (X_s, Y_s) relative to image origin
               X_Src : constant Integer := Integer (Col - Image'First (2));
               Y_Src : constant Integer := Integer (Row - Image'First (1));

               -- Convert to float and shift so pivot is at (0, 0)
               X0_F : constant Long_Float := Long_Float (X_Src) - Cx;
               Y0_F : constant Long_Float := Long_Float (Y_Src) - Cy;

               -- Rotate around the pivot:
               -- standard 2D rotation:
               --   x' = x cosθ - y sinθ
               --   y' = x sinθ + y cosθ
               X_Rot : constant Long_Float := X0_F * Cos_A - Y0_F * Sin_A;
               Y_Rot : constant Long_Float := X0_F * Sin_A + Y0_F * Cos_A;

               -- Shift back by pivot and add screen offset
               Dest_X_Int : constant Integer :=
                 Integer (Long_Float (Integer (X0)) + X_Rot + Cx);
               Dest_Y_Int : constant Integer :=
                 Integer (Long_Float (Integer (Y0)) + Y_Rot + Cy);
            begin
               -- Clip to screen to avoid Constraint_Error on Natural conversion
               if Dest_X_Int >= 0 and then Dest_Y_Int >= 0
                 and then Dest_X_Int < Screen_Width
                 and then Dest_Y_Int < Screen_Height
               then
                  Display.Hidden_Buffer (1).Set_Source
                    (ARGB =>
                       Blend (Image (Row, Col), Current_Background_Color));

                  Display.Hidden_Buffer (1).Set_Pixel
                    (Pt =>
                       (X => Natural (Dest_X_Int), Y => Natural (Dest_Y_Int)));
               end if;
            end;
         end loop;
      end loop;
   end Draw_Image;

end GUI.Images;
