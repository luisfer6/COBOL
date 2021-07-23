      ******************************************************************
      *COPY DE REGISTRO MAESTRO TARJETAS                               *
      *LONGITUD DE REGISTRO (53)                                       *
      ******************************************************************

       01  REG-MAESTRO-TARJETAS.
           03 TAR-NRO-TARJETA      PIC X(19).
           03 FILLER               PIC X(02).
           03 TAR-TIPO-TARJETA.
              05 TIPO-VISA         PIC X(01).
              05 TIPO-MASTERCARD   PIC X(01).
           03 FILLER               PIC X(02).
           03 TAR-LIMITE           PIC 9(05).
           03 FILLER               PIC X(02).
           03 TAR-NRO-CLI          PIC 9(03).
           03 FILLER               PIC X(02).
           03 TAR-FEC-ALTA.
              05 FEC-MM            PIC 9(02).
              05 FILLER            PIC X(01).
              05 FEC-AAAA          PIC 9(04).
           03 TAR-FEC-VTO.
              05 FILLER            PIC X(02).
              05 FEC-MM            PIC 9(02).
              05 FILLER            PIC X(01).
              05 FEC-AAAA          PIC 9(04).
