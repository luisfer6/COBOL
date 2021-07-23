      *----------------------------------------------------------------*
      *COPY RESUMEN RECAUDACION
      *LONGITUD DE RESUMEN RECAUDACION (37)
      *----------------------------------------------------------------*

       01  RESUMEN-RECAUDACION-REG.
           03 COD-CAJERO             PIC 9(03).
           03 FECHA-RECAUDACION.
              05 FECHA-AAAA          PIC X(04).
              05 FILLER              PIC X(01).
              05 FECHA-MM            PIC X(02).
              05 FILLER              PIC X(01).
              05 FECHA-DD            PIC X(02).
           03 IMPORTE                PIC 9(13)V99.
           03 CANTIDAD-CASOS         PIC 9(09).
