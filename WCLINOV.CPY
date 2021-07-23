      *----------------------------------------------------------------*
      * COPY DEL ARCHIVO DE NOVEDADES AL MAESTRO DE CLIENTES           *
      * LREC = 80                                                      *
      * CLINOV-COD-NOVEDAD = 'A' ES ALTA                               *
      *                    = 'B' ES BAJA LOGICA                        *
      *                    = 'D' ES BAJA FISICA                        *
      *                    = 'M' ES MODIFICACION                       *
      *----------------------------------------------------------------*
       01  REG-CLINOV.
           05 CLINOV-CLAVE.
              07 CLINOV-ID                 PIC  9(10).
           05 CLINOV-COD-NOVEDAD           PIC  X(01).
              88 88-COD-ES-ALTA                      VALUE 'A'.
              88 88-COD-ES-BAJA-LOGICA               VALUE 'B'.
              88 88-COD-ES-BAJA-FISICA               VALUE 'D'.
              88 88-COD-ES-MODIFICACION              VALUE 'M'.
           05 CLINOV-COD-CAMPO             PIC  X(02).
              88 88-MOD-LAST-NAME                    VALUE 01.
              88 88-MOD-FIRST-NAME                   VALUE 02.
              88 88-MOD-SEX                          VALUE 03.
              88 88-MOD-BIRTH                        VALUE 04.
              88 88-MOD-DEATH                        VALUE 05.
              88 88-MOD-MARITAL-STATUS               VALUE 06.
              88 88-MOD-HOME-ADDY                    VALUE 07.
              88 88-MOD-HOME-CITY                    VALUE 08.
              88 88-MOD-HOME-ZIP-CODE                VALUE 09.
              88 88-MOD-HOME-PHONE                   VALUE 10.
              88 88-MOD-HOME-EMAIL                   VALUE 11.
              88 88-MOD-HOME-PROVINCE-NAME           VALUE 12.
           05 CLINOV-DATO                  PIC  X(50).
           05 CLINOV-USO-FUTURO            PIC  X(17).
