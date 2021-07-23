      *----------------------------------------------------------------*
      * COPY DEL ARCHIVO MAESTRO DE CLIENTES                           *
      *----------------------------------------------------------------*
       01  REG-CLIENTES.
           05 CLI-CLAVE.
              07 CLI-ID                    PIC  9(10).
           05 CLI-LAST-NAME                PIC  X(50).
           05 CLI-FIRST-NAME               PIC  X(50).
           05 CLI-SEX                      PIC  X(01).
           05 CLI-BIRTH.
              07 CLI-BIRTH-DATE.
                 10 CLI-BIRTH-DATE-AAAA    PIC  9(04).
                 10 CLI-BIRTH-DATE-S1      PIC  X(01).
                 10 CLI-BIRTH-DATE-MM      PIC  9(02).
                 10 CLI-BIRTH-DATE-S2      PIC  X(01).
                 10 CLI-BIRTH-DATE-DD      PIC  9(02).
              07 CLI-BIRTH-SEP             PIC  X(01).
              07 CLI-BIRTH-TIME.
                 10 CLI-BIRTH-TIME-HH      PIC  9(02).
                 10 CLI-BIRTH-TIME-S1      PIC  X(01).
                 10 CLI-BIRTH-TIME-MM      PIC  9(02).
                 10 CLI-BIRTH-TIME-S2      PIC  X(01).
                 10 CLI-BIRTH-TIME-SS      PIC  9(02).
           05 CLI-DEATH.
              07 CLI-DEATH-DATE.
                 10 CLI-DEATH-DATE-AAAA    PIC  9(04).
                 10 CLI-DEATH-DATE-S1      PIC  X(01).
                 10 CLI-DEATH-DATE-MM      PIC  9(02).
                 10 CLI-DEATH-DATE-S2      PIC  X(01).
                 10 CLI-DEATH-DATE-DD      PIC  9(02).
              07 CLI-DEATH-SEP             PIC  X(01).
              07 CLI-DEATH-TIME.
                 10 CLI-DEATH-TIME-HH      PIC  9(02).
                 10 CLI-DEATH-TIME-S1      PIC  X(01).
                 10 CLI-DEATH-TIME-MM      PIC  9(02).
                 10 CLI-DEATH-TIME-S2      PIC  X(01).
                 10 CLI-DEATH-TIME-SS      PIC  9(02).
           05 CLI-MARITAL-STATUS           PIC  9.
           05 CLI-HOME-ADDY                PIC  X(50).
           05 CLI-HOME-CITY                PIC  X(50).
           05 CLI-HOME-ZIP-CODE            PIC  X(50).
           05 CLI-HOME-PHONE               PIC  X(50).
           05 CLI-HOME-EMAIL               PIC  X(50).
           05 CLI-HOME-PROVINCE-NAME       PIC  X(50).
           05 CLI-STATUS                   PIC  X(01).
              88 CLI-STATUS-ACTIVO              VALUE ' '.
              88 CLI-STATUS-INACTIVO            VALUE 'B'.
           05 CLI-USO-FUTURE               PIC  X(199).
