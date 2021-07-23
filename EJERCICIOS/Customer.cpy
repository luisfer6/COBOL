      *----------------------------------------------------------------*
      * COPY DEL ARCHIVO MAESTRO DE CLIENTES                           *
      *----------------------------------------------------------------*
       01  REG-CLIENTE.
           05 CLI-CLAVE.
              07 CLI-ID                    PIC  9(10).
           05 CLI-LAST-NAME                PIC  X(50).
           05 CLI-FIRST-NAME               PIC  X(50).
           05 CLI-SEX                      PIC  X(01).
           05 CLI-BIRTH                    PIC  X(19).
           05 CLI-DEATH                    PIC  X(19).
           05 CLI-MARITAL-STATUS           PIC  9.
           05 CLI-HOME-ADDY                PIC  X(50).
           05 CLI-HOME-CITY                PIC  X(50).
           05 CLI-HOME-ZIP-CODE            PIC  X(50).
           05 CLI-HOME-PHONE               PIC  X(50).
           05 CLI-HOME-EMAIL               PIC  X(50).
           05 CLI-HOME-PROVINCE-NAME       PIC  X(50).
           05 CLI-USO-FUTURE               PIC  X(200).
