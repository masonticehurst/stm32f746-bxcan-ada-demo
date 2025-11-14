with STM32_SVD.CAN;

package CAN_SVD_Periph is
   type CAN_Driver is limited interface;
   subtype Peripheral is STM32_SVD.CAN.CAN_Peripheral;
end CAN_SVD_Periph;
