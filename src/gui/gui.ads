--  This package provides a set of convenience routines for putting characters
--  and strings out to the LCD.

with BMP_Fonts; use BMP_Fonts;

with CAN_Handler;
with HAL.Bitmap;
with HAL.Framebuffer;
with HAL.Touch_Panel;
with Ada.Containers.Vectors;

package GUI is

   type Size is record
      Width  : Natural;
      Height : Natural;
   end record;

   type Button_Callback is access procedure;

   type Button_Entry is record
      Rect     : HAL.Bitmap.Rect;
      Callback : Button_Callback;
      Pressed  : Boolean := False;
   end record;

   package Button_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Button_Entry);

   Buttons : Button_Vectors.Vector;

   Black       : HAL.Bitmap.Bitmap_Color renames HAL.Bitmap.Black;
   Blue        : HAL.Bitmap.Bitmap_Color renames HAL.Bitmap.Blue;
   Light_Blue  : HAL.Bitmap.Bitmap_Color renames HAL.Bitmap.Light_Blue;
   Green       : HAL.Bitmap.Bitmap_Color renames HAL.Bitmap.Green;
   Cyan        : HAL.Bitmap.Bitmap_Color renames HAL.Bitmap.Cyan;
   Gray        : HAL.Bitmap.Bitmap_Color renames HAL.Bitmap.Gray;
   Magenta     : HAL.Bitmap.Bitmap_Color renames HAL.Bitmap.Magenta;
   Light_Green : HAL.Bitmap.Bitmap_Color renames HAL.Bitmap.Light_Green;
   Brown       : HAL.Bitmap.Bitmap_Color renames HAL.Bitmap.Brown;
   Red         : HAL.Bitmap.Bitmap_Color renames HAL.Bitmap.Red;
   Orange      : HAL.Bitmap.Bitmap_Color renames HAL.Bitmap.Orange;
   Yellow      : HAL.Bitmap.Bitmap_Color renames HAL.Bitmap.Yellow;
   White       : HAL.Bitmap.Bitmap_Color renames HAL.Bitmap.White;

   Default_Text_Color       : constant HAL.Bitmap.Bitmap_Color := White;
   Default_Background_Color : constant HAL.Bitmap.Bitmap_Color :=
     (Alpha => 255, Red => 16, Green => 25, Blue => 34);
   Default_Font             : constant BMP_Font                := Font16x24;
   --     Default_Orientation      : constant LCD.Orientations := LCD.Portrait_2;

   --  Changes to these current values will appear on subsequent calls to the
   --  output routines.
   Current_Text_Color       : HAL.Bitmap.Bitmap_Color := Default_Text_Color;
   Current_Background_Color : HAL.Bitmap.Bitmap_Color :=
     Default_Background_Color;

   Last_Range_Miles        : Natural                               := 9_999;
   Last_HV_Batt_V          : Natural                               := 9_999;
   Last_Humidity_Pct       : Natural                               := 9_999;
   Last_Speed_MPH          : Natural                               := 9_999;
   Last_Rear_Power_kW      : Integer                               := -1;
   Last_Temperature_C      : Integer                               := -1;
   Last_Gear               : CAN_Handler.Gear := CAN_Handler.Invalid;
   Last_Left_Turn          : Boolean                               := False;
   Last_Right_Turn         : Boolean                               := False;
   Last_Left_Turn_Request  : Boolean                               := False;
   Last_Right_Turn_Request : Boolean                               := False;
   Steering_Angle_Filtered : Long_Float                            := 0.0;
   Last_Steering_Angle     : Integer                               := -1;
   Steering_Alpha          : constant Long_Float := 0.25;  -- smoothing
   Steering_Threshold : constant Natural := 2;   -- redraw threshold in deg
   Last_VIN : String (CAN_Handler.VIN_Number'Range) := (others => ' ');

   --  Changes the current font setting so that subsequent output is in the
   --  specified font.
   procedure Set_Font (To : BMP_Font);
   procedure Set_Orientation (To : HAL.Framebuffer.Display_Orientation);

   procedure Clear_Screen;

   --  These routines maintain a logical line and column, such that text will
   --  wrap around to the next "line" when necessary, as determined by the
   --  current orientation of the screen.
   procedure Put_Line
     (Msg : String; Color : HAL.Bitmap.Bitmap_Color := Default_Text_Color);

   procedure Put (Msg : String);

   procedure Put (Msg : Character);

   --  A subsequent call to Put or Put_Line will start printing characters at
   --  the beginning of the next line, wrapping around to the top of the LCD
   --  screen if necessary.
   procedure New_Line;

   --  Prints the character at the specified location. Has no other effect
   --  whatsoever, especially none on the state of the current logical line
   --  or logical column.
   procedure Put (X, Y : Natural; Msg : Character);

   --  Prints the string, starting at the specified location. Has no other
   --  effect whatsoever, especially none on the state of the current logical
   --  line or logical column. Does not wrap around.
   procedure Put (X, Y : Natural; Msg : String);

   procedure Draw_Filled_Circle
     (Point : HAL.Bitmap.Point; Radius : Natural;
      Color : HAL.Bitmap.Bitmap_Color);

   procedure Draw_Rectangle
     (Rect : HAL.Bitmap.Rect; Color : HAL.Bitmap.Bitmap_Color);

   procedure Draw_Rounded_Rectangle
     (Rect   : HAL.Bitmap.Rect; Color : HAL.Bitmap.Bitmap_Color;
      Radius : Natural; Thickness : Natural) with
     Pre => Rect.Position.X - Thickness >= 0;
   pragma Assertion_Policy (Pre => Check);

   procedure Draw_Info
     (Point : HAL.Bitmap.Point; Text : String; Val : String := "");

   procedure Fill_Rounded_Rectangle
     (Rect   : HAL.Bitmap.Rect; Color : HAL.Bitmap.Bitmap_Color;
      Radius : Natural);

   procedure Draw_Button
     (Rect : HAL.Bitmap.Rect; Text : String; On_Press : Button_Callback);

   procedure Check_Buttons;

   function MeasureText (Text : String; Font : BMP_Fonts.BMP_Font) return Size;

   procedure Draw_Static_UI;

   procedure Update_Range_If_Changed;
   procedure Update_Speed_If_Changed;
   procedure Update_Gear_If_Changed;
   procedure Update_Power_If_Changed;
   procedure Update_Turn_Signals_If_Changed;
   procedure Update_Steering_Wheel_If_Changed;
   procedure Update_VIN_If_Completed;
   procedure Update_Link_Status;

   function Has_Touch_Within_Area
     (P : HAL.Bitmap.Point; S : Size) return Boolean;

end GUI;
