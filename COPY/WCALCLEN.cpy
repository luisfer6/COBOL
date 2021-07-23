      *----------------------------------------------------------------*
      * COPY DE COMUNICACION CON LA RUTINA CALCLEN                     *
      * SE COMPLETA CON EL CAMPO A EVALUAR Y DEVUELVE LA LONGITUD      *
      *----------------------------------------------------------------*
       01  WCALCLEN.
           05 WCALCLEN-ENTRADA.
              07 WCALCLEN-MSG              PIC  X(80).
              07 WCALCLEN-CAMPO.
                 10 WCALCLEN-CAMPO-BYTE    PIC  X OCCURS 4096 TIMES.
           05 WCALCLEN-SALIDA.
              07 WCALCLEN-LEN              PIC  S9(4) COMP.
              07 WCALCLEN-RC               PIC   X(2).
              07 WCALCLEN-MSG              PIC   X(80).
