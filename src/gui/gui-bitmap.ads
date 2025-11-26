with HAL;        use HAL;
with HAL.Bitmap; use HAL.Bitmap;

package GUI.Bitmap is
   type Bitmap_Format is (Bmp_RGB_24, Bmp_ARGB_32, Bmp_Unknown);

   type Bitmap_Header is record
      Signature   : Uint16;
      File_Size   : Uint32;
      Data_Offset : Uint32;
   end record;

   for Bitmap_Header use record
      Signature   at  0 range 0 .. 15;
      File_Size   at  2 range 0 .. 31;
      Data_Offset at 10 range 0 .. 31;
   end record;

   type Bitmap_Info_Header is record
      Size             : Uint32;
      Width            : Uint32;
      Height           : Uint32;
      Planes           : Uint16;
      BitCount         : Uint16;
      Compression      : Uint32;
      Image_Size       : Uint32;
      X_Pixels_Per_M   : Uint32;
      Y_Pixels_Per_M   : Uint32;
      Colors_Used      : Uint32;
      Colors_Important : Uint32;
   end record;

   for Bitmap_Info_Header use record
      Size             at  0 range 0 .. 31;
      Width            at  4 range 0 .. 31;
      Height           at  8 range 0 .. 31;
      Planes           at 12 range 0 .. 15;
      BitCount         at 14 range 0 .. 15;
      Compression      at 16 range 0 .. 31;
      Image_Size       at 20 range 0 .. 31;
      X_Pixels_Per_M   at 24 range 0 .. 31;
      Y_Pixels_Per_M   at 28 range 0 .. 31;
      Colors_Used      at 32 range 0 .. 31;
      Colors_Important at 36 range 0 .. 31;
   end record;

   type Color_Table_Entry is record
      B : Uint8;  -- Blue
      G : Uint8;  -- Green
      R : Uint8;  -- Red
      A : Uint8;  -- Reserved / Alpha
   end record;

   for Color_Table_Entry use record
      B at 0 range 0 .. 7;
      G at 1 range 0 .. 7;
      R at 2 range 0 .. 7;
      A at 3 range 0 .. 7;
   end record;

   for Color_Table_Entry'Size use 32;  -- 4 bytes * 8 bits

   type Byte is new Uint8;
   type Byte_Array is array (Natural range <>) of aliased Byte;

   type Color_Table_Array is
     array (Natural range <>) of aliased Color_Table_Entry;

   type Pixel is record
      B, G, R, A : Uint8;
   end record;
   for Pixel'Size use 32;

   for Pixel use record
      B at 0 range 0 .. 7;
      G at 1 range 0 .. 7;
      R at 2 range 0 .. 7;
      A at 3 range 0 .. 7;
   end record;

   type Pixel_Matrix is array (Natural range <>, Natural range <>) of Pixel;

   type Bitmap_Raster (Width : Natural; Height : Natural) is record
      Pixels : Pixel_Matrix (0 .. Height, 0 .. Width);
   end record;

   function Blend
     (Foreground : HAL.Bitmap.Bitmap_Color;
      Background : HAL.Bitmap.Bitmap_Color) return HAL.Bitmap.Bitmap_Color;

   procedure Draw_Image_From_File (FD : File_IO.File_Descriptor);

end GUI.Bitmap;
