      *----------------------------------------------------------------*
      *COPY RECAUDACION
      *LONGITUD DE REGISTRO RECAUDACION (28)
      *----------------------------------------------------------------*

       01  RECAUDACION-REG.
           03 CLAVE-RECAUDACION.
              05 COD-CAJERO             PIC 9(03).
              05 FECHA-RECAUDACION.
                 07 FECHA-AAAA          PIC X(04).
                 07 FILLER              PIC X(01).
                 07 FECHA-MM            PIC X(02).
                 07 FILLER              PIC X(01).
                 07 FECHA-DD            PIC X(02).
           03 CONCEPTO-RECAUDACION      PIC X(04).
           03 IMPORTE                   PIC 9(9)V99.
