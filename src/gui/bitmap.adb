package body Bitmap is


function Blend
  (Foreground : Bitmap_Color;
   Background : Bitmap_Color)
   return Bitmap_Color
is
   -- Convert alpha from 0..255 -> 0.0 .. 1.0
   A : constant Float := Float(Foreground.Alpha) / 255.0;

   -- Helper to blend a single channel
   function Blend_Channel (F, B : UInt8) return UInt8 is
      V : constant Float :=
        A * Float(F) + (1.0 - A) * Float(B);
   begin
      return UInt8 (Integer (Float'Rounding (V)));
   end Blend_Channel;

   Result : Bitmap_Color;
begin
   Result.Alpha := 255;  -- Result is fully opaque after compositing
   Result.Red   := Blend_Channel (Foreground.Red,   Background.Red);
   Result.Green := Blend_Channel (Foreground.Green, Background.Green);
   Result.Blue  := Blend_Channel (Foreground.Blue,  Background.Blue);

   return Result;
end Blend;

end Bitmap;