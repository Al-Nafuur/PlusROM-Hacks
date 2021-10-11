; Chetiry Bankswitching - v1.0
; (C) Copyright Chris Walton 2011

  IFNCONST CHETIRY_BASE_ADDRESS
CHETIRY_BASE_ADDRESS = $1000
  ENDIF

  IFNCONST CHETIRY_BASE_WRITE_ADDRESS
CHETIRY_BASE_WRITE_ADDRESS = CHETIRY_BASE_ADDRESS
  ENDIF

  IFNCONST CHETIRY_BASE_READ_ADDRESS
CHETIRY_BASE_READ_ADDRESS = CHETIRY_BASE_ADDRESS+$40
  ENDIF

  SEG.U CHETIRY_REGISTERS_WRITE
  ORG CHETIRY_BASE_WRITE_ADDRESS

OPERATION_W     DS.B 1    ; $00
SEED_W          DS.B 1    ; $01
TUNERESET_W     DS.B 1    ; $02
TUNESTEP_W      DS.B 1    ; $03
RAM_W           DS.B 60   ; $04

  SEG.U CHETIRY_REGISTERS_READ
  ORG CHETIRY_BASE_READ_ADDRESS

ERRORCODE_R     DS.B 1    ; $40
RANDOM_R        DS.B 1    ; $41
TUNELO_R        DS.B 1    ; $42
TUNEHI_R        DS.B 1    ; $43
RAM_R           DS.B 60   ; $44

; Operation: Bits 0-3 = Operation, 4-7 = Index

; Valid Operations (Stall Atari after setting operation type):
; 0 = NOP
; 1 = Load Tune (index = tune)
; 2 = Load Score Table (index = table)
; 3 = Save Score Table (index = table)
; 4 = Wipe Score Tables

; Error Codes:
; 0 = No Error
; 1 = Error

; Random Numbers: Set seed > 0 before reading (updated on every read)
