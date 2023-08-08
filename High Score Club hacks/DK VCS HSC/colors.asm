
; ********************************************************************
;  colors
;
;    NTSC PAL color translation table
;
;    $Date: Thu, 05 Jan 2017 20:55:15 +0100 $
;    $Author: dietrich $
;    $Revision: 477 $
;
;  Copyright (C) 2017 Andreas Dietrich
; ********************************************************************

;
; External settings
;

#ifconst TV_EXTERN
#if TV_EXTERN == 1
TV_SYSTEM SET "NTSC"
#endif
#if TV_EXTERN == 2
TV_SYSTEM SET "PAL60"
#endif
#endif

;
; Switch colors
;

#if TV_SYSTEM == "NTSC"

; NTSC colors

COL_00 = $00
COL_02 = $02
COL_04 = $04
COL_06 = $06
COL_08 = $08
COL_0A = $0A
COL_0C = $0C
COL_0E = $0E

COL_10 = $10
COL_12 = $12
COL_14 = $14
COL_16 = $16
COL_18 = $18
COL_1A = $1A
COL_1C = $1C
COL_1E = $1E

COL_20 = $20
COL_22 = $22
COL_24 = $24
COL_26 = $26
COL_28 = $28
COL_2A = $2A
COL_2C = $2C
COL_2E = $2E

COL_30 = $30
COL_32 = $32
COL_34 = $34
COL_36 = $36
COL_38 = $38
COL_3A = $3A
COL_3C = $3C
COL_3E = $3E

COL_40 = $40
COL_42 = $42
COL_44 = $44
COL_46 = $46
COL_48 = $48
COL_4A = $4A
COL_4C = $4C
COL_4E = $4E

COL_50 = $50
COL_52 = $52
COL_54 = $54
COL_56 = $56
COL_58 = $58
COL_5A = $5A
COL_5C = $5C
COL_5E = $5E

COL_60 = $60
COL_62 = $62
COL_64 = $64
COL_66 = $66
COL_68 = $68
COL_6A = $6A
COL_6C = $6C
COL_6E = $6E

COL_70 = $70
COL_72 = $72
COL_74 = $74
COL_76 = $76
COL_78 = $78
COL_7A = $7A
COL_7C = $7C
COL_7E = $7E

COL_80 = $80
COL_82 = $82
COL_84 = $84
COL_86 = $86
COL_88 = $88
COL_8A = $8A
COL_8C = $8C
COL_8E = $8E

COL_90 = $90
COL_92 = $92
COL_94 = $94
COL_96 = $96
COL_98 = $98
COL_9A = $9A
COL_9C = $9C
COL_9E = $9E

COL_A0 = $A0
COL_A2 = $A2
COL_A4 = $A4
COL_A6 = $A6
COL_A8 = $A8
COL_AA = $AA
COL_AC = $AC
COL_AE = $AE

COL_B0 = $B0
COL_B2 = $B2
COL_B4 = $B4
COL_B6 = $B6
COL_B8 = $B8
COL_BA = $BA
COL_BC = $BC
COL_BE = $BE

COL_C0 = $C0
COL_C2 = $C2
COL_C4 = $C4
COL_C6 = $C6
COL_C8 = $C8
COL_CA = $CA
COL_CC = $CC
COL_CE = $CE

COL_D0 = $D0
COL_D2 = $D2
COL_D4 = $D4
COL_D6 = $D6
COL_D8 = $D8
COL_DA = $DA
COL_DC = $DC
COL_DE = $DE

COL_E0 = $E0
COL_E2 = $E2
COL_E4 = $E4
COL_E6 = $E6
COL_E8 = $E8
COL_EA = $EA
COL_EC = $EC
COL_EE = $EE

COL_F0 = $F0
COL_F2 = $F2
COL_F4 = $F4
COL_F6 = $F6
COL_F8 = $F8
COL_FA = $FA
COL_FC = $FC
COL_FE = $FE

#else

; PAL color translation

COL_00 = $00
COL_02 = $02
COL_04 = $04
COL_06 = $06
COL_08 = $08
COL_0A = $0A
COL_0C = $0C
COL_0E = $0E

COL_10 = $20
COL_12 = $22
COL_14 = $24
COL_16 = $26
COL_18 = $28
COL_1A = $2A
COL_1C = $2C
COL_1E = $2E

COL_20 = $20
COL_22 = $22
COL_24 = $24
COL_26 = $26
COL_28 = $28
COL_2A = $2A
COL_2C = $2C
COL_2E = $2E

COL_30 = $40
COL_32 = $42
COL_34 = $44
COL_36 = $46
COL_38 = $48
COL_3A = $4A
COL_3C = $4C
COL_3E = $4E

COL_40 = $60
COL_42 = $62
COL_44 = $64
COL_46 = $66
COL_48 = $68
COL_4A = $6A
COL_4C = $6C
COL_4E = $6E

COL_50 = $80
COL_52 = $82
COL_54 = $84
COL_56 = $86
COL_58 = $88
COL_5A = $8A
COL_5C = $8C
COL_5E = $8E

COL_60 = $A0
COL_62 = $A2
COL_64 = $A4
COL_66 = $A6
COL_68 = $A8
COL_6A = $AA
COL_6C = $AC
COL_6E = $AE

COL_70 = $C0
COL_72 = $C2
COL_74 = $C4
COL_76 = $C6
COL_78 = $C8
COL_7A = $CA
COL_7C = $CC
COL_7E = $CE

COL_80 = $D0
COL_82 = $D2
COL_84 = $D4
COL_86 = $D6
COL_88 = $D8
COL_8A = $DA
COL_8C = $DC
COL_8E = $DE

COL_90 = $B0
COL_92 = $B2
COL_94 = $B4
COL_96 = $B6
COL_98 = $B8
COL_9A = $BA
COL_9C = $BC
COL_9E = $BE

COL_A0 = $90
COL_A2 = $92
COL_A4 = $94
COL_A6 = $96
COL_A8 = $98
COL_AA = $9A
COL_AC = $9C
COL_AE = $9E

COL_B0 = $70
COL_B2 = $72
COL_B4 = $74
COL_B6 = $76
COL_B8 = $78
COL_BA = $7A
COL_BC = $7C
COL_BE = $7E

COL_C0 = $50
COL_C2 = $52
COL_C4 = $54
COL_C6 = $56
COL_C8 = $58
COL_CA = $5A
COL_CC = $5C
COL_CE = $5E

COL_D0 = $30
COL_D2 = $32
COL_D4 = $34
COL_D6 = $36
COL_D8 = $38
COL_DA = $3A
COL_DC = $3C
COL_DE = $3E

COL_E0 = $30
COL_E2 = $32
COL_E4 = $34
COL_E6 = $36
COL_E8 = $38
COL_EA = $3A
COL_EC = $3C
COL_EE = $3E

COL_F0 = $20
COL_F2 = $22
COL_F4 = $24
COL_F6 = $26
COL_F8 = $28
COL_FA = $2A
COL_FC = $2C
COL_FE = $2E

#endif
