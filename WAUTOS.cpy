      ******************************************************************
      *COPY DE REGISTRO-AUTOS                                          *
      *LONGITUD DE REGISTRO X(39)                                      *
      *LA CLAVE VEHCLAVE 1 MIDE 5 POSICIONES DE LA 1 A LA 5            *
      *LA CLAVE VEHCLAVE 2 MIDE 9 POSICIONES DE LA 6 A LA 14           *
      *LA CLAVE VEHCLAVE 3 MIDE 8 POSICIONES DE LA 15 A LA 22          *
      ******************************************************************

       01  VEH-REGISTRO.
           03 VEH-CLAVE.
              05 VEH-ID            PIC 9(05).
           03 VEH-CLAVE-2.
              05 VEH-PATENTE       PIC X(09).
           03 VEH-CLAVE-3.
              05 VEH-MARCA         PIC X(08).
           03 VEH-COLOR            PIC X(08).
           03 VEH-MODELO           PIC X(09).
