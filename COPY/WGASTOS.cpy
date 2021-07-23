      *----------------------------------------------------------------*
      *COPY DE REGISTRO GASTOS TARJETAS                                *
      *LONGITUD DE REGISTRO (41)                                       *
      *----------------------------------------------------------------*
       01  REG-GASTOS.
           03 GTO-CLAVE.
              05 GTO-ID                    PIC  9(03).
		   03 FILLER                       PIC  X(02).
           03 GTO-NUM-TARJETA              PIC  9(19).
		   03 FILLER                       PIC  X(02).
		   03 GTO-MONTO                    PIC  9(04)V99.
		   03 FILLER                       PIC  X(02).
		   03 GTO-FECHA.
		      05 FEC-DD                  PIC 9(02).
			  05 FILLER                  PIC X(01).
		      05 FEC-MM                  PIC 9(02).
			  05 FILLER                  PIC X(01).
			  05 FEC-AAAA                PIC 9(04).
		      
