;--------------------------------------------------
;--- Constants

KERNEL_VBLANK_TIME   = $28  ;--33.6 * 76 / 64
KERNEL_DISPLAY_TIME  = $E4  ;--192 * 76 / 64
KERNEL_OVERSCAN_TIME = $25  ;--31.2 * 76 / 64

        ALIGN	256
CARD_ACE_CROSS
        .byte $A7,$A2,$E2,$A7,$42
CARD_ACE_PIK
        .byte $A7,$A2,$E7,$A7,$42
CARD_ACE_HEART
        .byte $A0,$A2,$E7,$A7,$45
CARD_ACE_CARO
        .byte $A0,$A2,$E7,$A7,$42

CARD_KING_CROSS
        .byte $A7,$A2,$C2,$A7,$A2
CARD_KING_PIK
        .byte $A7,$A2,$C7,$A7,$A2
CARD_KING_HEART
        .byte $A0,$A2,$C7,$A7,$A5
CARD_KING_CARO
        .byte $A0,$A2,$C7,$A7,$A2

CARD_QUEEN_CROSS
        .byte $67,$A2,$A2,$A7,$42
CARD_QUEEN_PIK
        .byte $67,$A2,$A7,$A7,$42
CARD_QUEEN_HEART
        .byte $60,$A2,$A7,$A7,$45
CARD_QUEEN_CARO
        .byte $60,$A2,$A7,$A7,$42

CARD_JACK_CROSS
        .byte $47,$A2,$22,$27,$22
CARD_JACK_PIK
        .byte $47,$A2,$27,$27,$22
CARD_JACK_HEART
        .byte $40,$A2,$27,$27,$25
CARD_JACK_CARO
        .byte $40,$A2,$27,$27,$22

CARD_10_CROSS
        .byte $B7,$B2,$B2,$B7,$B2
CARD_10_PIK
        .byte $B7,$B2,$B7,$B7,$B2
CARD_10_HEART
        .byte $B8,$AA,$AF,$AF,$BD
CARD_10_CARO
        .byte $B0,$B2,$B7,$B7,$B2

CARD_9_CROSS
        .byte $E7,$22,$E2,$A7,$E2
CARD_9_PIK
        .byte $E7,$22,$E7,$A7,$E2
CARD_9_HEART
        .byte $E0,$22,$E7,$A7,$E5
CARD_9_CARO
        .byte $E0,$22,$E7,$A7,$E2

CARD_8_CROSS
        .byte $E7,$A2,$E2,$A7,$E2
CARD_8_PIK
        .byte $E7,$A2,$E7,$A7,$E2
CARD_8_HEART
        .byte $E0,$A2,$E7,$A7,$E5
CARD_8_CARO
        .byte $E0,$A2,$E7,$A7,$E2

CARD_7_CROSS
        .byte $27,$22,$22,$27,$E2
CARD_7_PIK
        .byte $27,$22,$27,$27,$E2
CARD_7_HEART
        .byte $20,$22,$27,$27,$E5
CARD_7_CARO
        .byte $20,$22,$27,$27,$E2

CARD_6_CROSS
        .byte $E7,$A2,$E2,$87,$E2
CARD_6_PIK
        .byte $E7,$A2,$E7,$87,$E2
CARD_6_HEART
        .byte $E0,$A2,$E7,$87,$E5
CARD_6_CARO
        .byte $E0,$A2,$E7,$87,$E2

CARD_5_CROSS
        .byte $E7,$22,$E2,$87,$E2
CARD_5_PIK
        .byte $E7,$22,$E7,$87,$E2
CARD_5_HEART
        .byte $E0,$22,$E7,$87,$E5
CARD_5_CARO
        .byte $E0,$22,$E7,$87,$E2

CARD_4_CROSS
        .byte $27,$22,$E2,$A7,$A2
CARD_4_PIK
        .byte $27,$22,$E7,$A7,$A2
CARD_4_HEART
        .byte $20,$22,$E7,$A7,$A5
CARD_4_CARO
        .byte $20,$22,$E7,$A7,$A2

CARD_3_CROSS
        .byte $E7,$22,$62,$27,$E2
CARD_3_PIK
        .byte $E7,$22,$67,$27,$E2
CARD_3_HEART
        .byte $E0,$22,$67,$27,$E5
CARD_3_CARO
        .byte $E0,$22,$67,$27,$E2

CARD_2_CROSS
        .byte $E7,$82,$E2,$27,$E2
CARD_2_PIK
        .byte $E7,$82,$E7,$27,$E2
CARD_2_HEART
        .byte $E0,$82,$E7,$27,$E5

;padding to prevent page crossing in next card
        ALIGN	256
CARD_2_CARO
        .byte $E0,$82,$E7,$27,$E2

CARD_FACE_DOWN
;        .byte $FF,$AB,$D5,$AB,$FF
        .byte $55,$AA,$55,$AA,$55

CARD_BLANK
        .byte $00,$00,$00,$00,$00

DIGIT_0
        .byte $0E,$0A,$0A,$0A,$0E
DIGIT_1
        .byte $02,$02,$02,$06,$02
DIGIT_2
        .byte $0E,$08,$0E,$02,$0E
DIGIT_3
        .byte $0E,$02,$06,$02,$0E
DIGIT_4
        .byte $02,$02,$0E,$0A,$0A
DIGIT_5
        .byte $0E,$02,$0E,$08,$0E
DIGIT_6
        .byte $0E,$0A,$0E,$08,$0E
DIGIT_7
        .byte $02,$02,$02,$02,$0E
DIGIT_8
        .byte $0E,$0A,$0E,$0A,$0E
DIGIT_9
        .byte $0E,$02,$0E,$0A,$0E
DIGIT_10
        .byte $2E,$2A,$2A,$6A,$2E
DIGIT_11
        .byte $22,$22,$22,$62,$22
DIGIT_12
        .byte $2E,$28,$2E,$62,$2E
DIGIT_13
        .byte $2E,$22,$26,$62,$2E
DIGIT_14
        .byte $22,$22,$2E,$6A,$2A
DIGIT_15
        .byte $2E,$22,$2E,$68,$2E
DIGIT_16
        .byte $2E,$2A,$2E,$68,$2E
DIGIT_17
        .byte $22,$22,$22,$62,$2E
DIGIT_18
        .byte $2E,$2A,$2E,$6A,$2E
DIGIT_19
        .byte $2E,$22,$2E,$6A,$2E
DIGIT_20
        .byte $EE,$8A,$EA,$2A,$EE
DIGIT_21
        .byte $E2,$82,$E2,$26,$E2
DIGIT_22
        .byte $EE,$88,$EE,$22,$EE
DIGIT_23
        .byte $EE,$82,$E6,$22,$EE
DIGIT_24
        .byte $E2,$82,$EE,$2A,$EA
DIGIT_25
        .byte $EE,$82,$EE,$28,$EE
DIGIT_26
        .byte $EE,$8A,$EE,$28,$EE
DIGIT_27
        .byte $E2,$82,$E2,$22,$EE
DIGIT_28
        .byte $EE,$8A,$EE,$2A,$EE
DIGIT_29
        .byte $EE,$82,$EE,$2A,$EE
DIGIT_30
        .byte $EE,$2A,$6A,$2A,$EE
DIGIT_31
        .byte $E2,$22,$62,$26,$E2
DIGIT_32
        .byte $EE,$28,$6E,$22,$EE
DIGIT_33
        .byte $EE,$22,$66,$22,$EE
DIGIT_34
        .byte $E2,$22,$6E,$2A,$EA
DIGIT_35
        .byte $EE,$22,$6E,$28,$EE
DIGIT_36
        .byte $EE,$2A,$6E,$28,$EE
DIGIT_37
        .byte $E2,$22,$62,$22,$EE
DIGIT_38
        .byte $EE,$2A,$6E,$2A,$EE
DIGIT_39
        .byte $EE,$22,$6E,$2A,$EE
DIGIT_40
        .byte $2E,$2A,$EA,$AA,$AE
DIGIT_41
        .byte $22,$22,$E2,$A6,$A2
DIGIT_42
        .byte $2E,$28,$EE,$A2,$AE
DIGIT_43
        .byte $2E,$22,$E6,$A2,$AE
DIGIT_44
        .byte $22,$22,$EE,$AA,$AA
DIGIT_45
        .byte $2E,$22,$EE,$A8,$AE
DIGIT_46
        .byte $2E,$2A,$EE,$A8,$AE
DIGIT_47
        .byte $22,$22,$E2,$A2,$AE

;padding to prevent page crossing in next card
        ALIGN	256
DIGIT_48
        .byte $2E,$2A,$EE,$AA,$AE
DIGIT_49
        .byte $2E,$22,$EE,$AA,$AE
DIGIT_50
        .byte $EE,$2A,$EA,$8A,$EE
DIGIT_51
        .byte $E2,$22,$E2,$86,$E2
DIGIT_52
        .byte $EE,$28,$EE,$82,$EE
DIGIT_53
        .byte $EE,$22,$E6,$82,$EE
DIGIT_54
        .byte $E2,$22,$EE,$8A,$EA
DIGIT_55
        .byte $EE,$22,$EE,$88,$EE
DIGIT_56
        .byte $EE,$2A,$EE,$88,$EE
DIGIT_57
        .byte $E2,$22,$E2,$82,$EE
DIGIT_58
        .byte $EE,$2A,$EE,$8A,$EE
DIGIT_59
        .byte $EE,$22,$EE,$8A,$EE
DIGIT_60
        .byte $EE,$AA,$EA,$8A,$EE
DIGIT_61
        .byte $E2,$A2,$E2,$86,$E2
DIGIT_62
        .byte $EE,$A8,$EE,$82,$EE
DIGIT_63
        .byte $EE,$A2,$E6,$82,$EE
DIGIT_64
        .byte $E2,$A2,$EE,$8A,$EA
DIGIT_65
        .byte $EE,$A2,$EE,$88,$EE
DIGIT_66
        .byte $EE,$AA,$EE,$88,$EE
DIGIT_67
        .byte $E2,$A2,$E2,$82,$EE
DIGIT_68
        .byte $EE,$AA,$EE,$8A,$EE
DIGIT_69
        .byte $EE,$A2,$EE,$8A,$EE
DIGIT_70
        .byte $2E,$2A,$2A,$2A,$EE
DIGIT_71
        .byte $22,$22,$22,$26,$E2
DIGIT_72
        .byte $2E,$28,$2E,$22,$EE
DIGIT_73
        .byte $2E,$22,$26,$22,$EE
DIGIT_74
        .byte $22,$22,$2E,$2A,$EA
DIGIT_75
        .byte $2E,$22,$2E,$28,$EE
DIGIT_76
        .byte $2E,$2A,$2E,$28,$EE
DIGIT_77
        .byte $22,$22,$22,$22,$EE
DIGIT_78
        .byte $2E,$2A,$2E,$2A,$EE
DIGIT_79
        .byte $2E,$22,$2E,$2A,$EE
DIGIT_80
        .byte $EE,$AA,$EA,$AA,$EE
DIGIT_81
        .byte $E2,$A2,$E2,$A6,$E2
DIGIT_82
        .byte $EE,$A8,$EE,$A2,$EE
DIGIT_83
        .byte $EE,$A2,$E6,$A2,$EE
DIGIT_84
        .byte $E2,$A2,$EE,$AA,$EA
DIGIT_85
        .byte $EE,$A2,$EE,$A8,$EE
DIGIT_86
        .byte $EE,$AA,$EE,$A8,$EE
DIGIT_87
        .byte $E2,$A2,$E2,$A2,$EE
DIGIT_88
        .byte $EE,$AA,$EE,$AA,$EE
DIGIT_89
        .byte $EE,$A2,$EE,$AA,$EE
DIGIT_90
        .byte $EE,$2A,$EA,$AA,$EE
DIGIT_91
        .byte $E2,$22,$E2,$A6,$E2
DIGIT_92
        .byte $EE,$28,$EE,$A2,$EE
DIGIT_93
        .byte $EE,$22,$E6,$A2,$EE
DIGIT_94
        .byte $E2,$22,$EE,$AA,$EA
DIGIT_95
        .byte $EE,$22,$EE,$A8,$EE
DIGIT_96
        .byte $EE,$2A,$EE,$A8,$EE
DIGIT_97
        .byte $E2,$22,$E2,$A2,$EE
DIGIT_98
        .byte $EE,$2A,$EE,$AA,$EE
;padding to prevent page crossing in next card
        ALIGN	256
DIGIT_99
        .byte $EE,$22,$EE,$AA,$EE
DIGIT_00
        .byte $EE,$AA,$AA,$AA,$EE
DIGIT_01
        .byte $E2,$A2,$A2,$A6,$E2
DIGIT_02
        .byte $EE,$A8,$AE,$A2,$EE
DIGIT_03
        .byte $EE,$A2,$A6,$A2,$EE
DIGIT_04
        .byte $E2,$A2,$AE,$AA,$EA
DIGIT_05
        .byte $EE,$A2,$AE,$A8,$EE
DIGIT_06
        .byte $EE,$AA,$AE,$A8,$EE
DIGIT_07
        .byte $E2,$A2,$A2,$A2,$EE
DIGIT_08
        .byte $EE,$AA,$AE,$AA,$EE
DIGIT_09
        .byte $EE,$A2,$AE,$AA,$EE

MOVE_FO ; FOLD
	.byte $8E ; |X   XXX |
	.byte $8A ; |X   X X |
	.byte $CA ; |XX  X X |
	.byte $8A ; |X   X X |
	.byte $EE ; |XXX XXX |
MOVE_CH ; CHECK
	.byte $EA ; |XXX X X |
	.byte $8A ; |X   X X |
	.byte $8E ; |X   XXX |
	.byte $8A ; |X   X X |
	.byte $EA ; |XXX X X |
MOVE_BB ; POST
	.byte $8E ; |X   XXX |
	.byte $8A ; |X   X X |
	.byte $EA ; |XXX X X |
	.byte $AA ; |X X X X |
	.byte $EE ; |XXX XXX |
MOVE_BL ; BET LOW (e.g. 5 of 5/10, or 2 of 2/5 first round)
	.byte $EE ; |XXX XXX |
	.byte $A8 ; |X X X   |
	.byte $C8 ; |XX  X   |
	.byte $A8 ; |X X X   |
	.byte $E8 ; |XXX X   |
MOVE_BH ; BET HIGH (e.g. 10)
	.byte $EA ; |XXX X X |
	.byte $AA ; |X X X X |
	.byte $CE ; |XX  XXX |
	.byte $AA ; |X X X X |
	.byte $EA ; |XXX X X |
MOVE_CA ; CALL
	.byte $EA ; |XXX X X |
	.byte $8A ; |X   X X |
	.byte $8E ; |X   XXX |
	.byte $8A ; |X   X X |
	.byte $E4 ; |XXX  X  |
MOVE_RA ; RAISE
	.byte $AA ; |X X X X |
	.byte $AA ; |X X X X |
	.byte $CE ; |XX  XXX |
	.byte $AA ; |X X X X |
	.byte $E4 ; |XXX  X  |

card_pointers_hi
CARD_ACE_CROSS_ID = * - card_pointers_hi
        .byte >CARD_ACE_CROSS
CARD_ACE_PIK_ID = * - card_pointers_hi
        .byte >CARD_ACE_PIK
        .byte >CARD_KING_CROSS
        .byte >CARD_KING_PIK
        .byte >CARD_QUEEN_CROSS
        .byte >CARD_QUEEN_PIK
        .byte >CARD_JACK_CROSS
        .byte >CARD_JACK_PIK
        .byte >CARD_10_CROSS
        .byte >CARD_10_PIK
        .byte >CARD_9_CROSS
        .byte >CARD_9_PIK
        .byte >CARD_8_CROSS
        .byte >CARD_8_PIK
        .byte >CARD_7_CROSS
        .byte >CARD_7_PIK
        .byte >CARD_6_CROSS
        .byte >CARD_6_PIK
        .byte >CARD_5_CROSS
        .byte >CARD_5_PIK
        .byte >CARD_4_CROSS
        .byte >CARD_4_PIK
        .byte >CARD_3_CROSS
        .byte >CARD_3_PIK
        .byte >CARD_2_CROSS
        .byte >CARD_2_PIK

CARD_FACE_DOWN_ID = * - card_pointers_hi
        .byte >CARD_FACE_DOWN

        .byte >CARD_ACE_HEART
        .byte >CARD_ACE_CARO
        .byte >CARD_KING_HEART
        .byte >CARD_KING_CARO
        .byte >CARD_QUEEN_HEART
        .byte >CARD_QUEEN_CARO
        .byte >CARD_JACK_HEART
        .byte >CARD_JACK_CARO
        .byte >CARD_10_HEART
        .byte >CARD_10_CARO
        .byte >CARD_9_HEART
        .byte >CARD_9_CARO
        .byte >CARD_8_HEART
        .byte >CARD_8_CARO
        .byte >CARD_7_HEART
        .byte >CARD_7_CARO
        .byte >CARD_6_HEART
        .byte >CARD_6_CARO
        .byte >CARD_5_HEART
        .byte >CARD_5_CARO
        .byte >CARD_4_HEART
        .byte >CARD_4_CARO
        .byte >CARD_3_HEART
        .byte >CARD_3_CARO
        .byte >CARD_2_HEART
        .byte >CARD_2_CARO

CARD_BLANK_ID = * - card_pointers_hi
        .byte >CARD_BLANK
DIGIT_0_ID = * - card_pointers_hi
        .byte >DIGIT_0
DIGIT_1_ID = * - card_pointers_hi
        .byte >DIGIT_1
        .byte >DIGIT_2
        .byte >DIGIT_3
        .byte >DIGIT_4
        .byte >DIGIT_5
        .byte >DIGIT_6
        .byte >DIGIT_7
        .byte >DIGIT_8
        .byte >DIGIT_9
        .byte >DIGIT_10
        .byte >DIGIT_11
        .byte >DIGIT_12
        .byte >DIGIT_13
        .byte >DIGIT_14
        .byte >DIGIT_15
        .byte >DIGIT_16
        .byte >DIGIT_17
        .byte >DIGIT_18
        .byte >DIGIT_19
        .byte >DIGIT_20
        .byte >DIGIT_21
        .byte >DIGIT_22
        .byte >DIGIT_23
        .byte >DIGIT_24
        .byte >DIGIT_25
        .byte >DIGIT_26
        .byte >DIGIT_27
        .byte >DIGIT_28
        .byte >DIGIT_29
        .byte >DIGIT_30
        .byte >DIGIT_31
        .byte >DIGIT_32
        .byte >DIGIT_33
        .byte >DIGIT_34
        .byte >DIGIT_35
        .byte >DIGIT_36
        .byte >DIGIT_37
        .byte >DIGIT_38
        .byte >DIGIT_39
        .byte >DIGIT_40
        .byte >DIGIT_41
        .byte >DIGIT_42
        .byte >DIGIT_43
        .byte >DIGIT_44
        .byte >DIGIT_45
        .byte >DIGIT_46
        .byte >DIGIT_47
        .byte >DIGIT_48
        .byte >DIGIT_49
        .byte >DIGIT_50
        .byte >DIGIT_51
        .byte >DIGIT_52
        .byte >DIGIT_53
        .byte >DIGIT_54
        .byte >DIGIT_55
        .byte >DIGIT_56
        .byte >DIGIT_57
        .byte >DIGIT_58
        .byte >DIGIT_59
        .byte >DIGIT_60
        .byte >DIGIT_61
        .byte >DIGIT_62
        .byte >DIGIT_63
        .byte >DIGIT_64
        .byte >DIGIT_65
        .byte >DIGIT_66
        .byte >DIGIT_67
        .byte >DIGIT_68
        .byte >DIGIT_69
        .byte >DIGIT_70
        .byte >DIGIT_71
        .byte >DIGIT_72
        .byte >DIGIT_73
        .byte >DIGIT_74
        .byte >DIGIT_75
        .byte >DIGIT_76
        .byte >DIGIT_77
        .byte >DIGIT_78
        .byte >DIGIT_79
        .byte >DIGIT_80
        .byte >DIGIT_81
        .byte >DIGIT_82
        .byte >DIGIT_83
        .byte >DIGIT_84
        .byte >DIGIT_85
        .byte >DIGIT_86
        .byte >DIGIT_87
        .byte >DIGIT_88
        .byte >DIGIT_89
        .byte >DIGIT_90
        .byte >DIGIT_91
        .byte >DIGIT_92
        .byte >DIGIT_93
        .byte >DIGIT_94
        .byte >DIGIT_95
        .byte >DIGIT_96
        .byte >DIGIT_97
        .byte >DIGIT_98
        .byte >DIGIT_99
        .byte >DIGIT_00
        .byte >DIGIT_01
        .byte >DIGIT_02
        .byte >DIGIT_03
        .byte >DIGIT_04
        .byte >DIGIT_05
        .byte >DIGIT_06
        .byte >DIGIT_07
        .byte >DIGIT_08
        .byte >DIGIT_09

MOVE_FO_ID = * - card_pointers_hi
        .byte >MOVE_FO
MOVE_CH_ID = * - card_pointers_hi
        .byte >MOVE_CH
MOVE_BB_ID = * - card_pointers_hi
        .byte >MOVE_BB
MOVE_BL_ID = * - card_pointers_hi
        .byte >MOVE_BL
MOVE_BH_ID = * - card_pointers_hi
        .byte >MOVE_BH
MOVE_CA_ID = * - card_pointers_hi
        .byte >MOVE_CA
MOVE_RA_ID = * - card_pointers_hi
        .byte >MOVE_RA

    align 256
card_pointers_lo
        .byte <CARD_ACE_CROSS
        .byte <CARD_ACE_PIK
        .byte <CARD_KING_CROSS
        .byte <CARD_KING_PIK
        .byte <CARD_QUEEN_CROSS
        .byte <CARD_QUEEN_PIK
        .byte <CARD_JACK_CROSS
        .byte <CARD_JACK_PIK
        .byte <CARD_10_CROSS
        .byte <CARD_10_PIK
        .byte <CARD_9_CROSS
        .byte <CARD_9_PIK
        .byte <CARD_8_CROSS
        .byte <CARD_8_PIK
        .byte <CARD_7_CROSS
        .byte <CARD_7_PIK
        .byte <CARD_6_CROSS
        .byte <CARD_6_PIK
        .byte <CARD_5_CROSS
        .byte <CARD_5_PIK
        .byte <CARD_4_CROSS
        .byte <CARD_4_PIK
        .byte <CARD_3_CROSS
        .byte <CARD_3_PIK
        .byte <CARD_2_CROSS
        .byte <CARD_2_PIK

        .byte <CARD_FACE_DOWN

        .byte <CARD_ACE_HEART
        .byte <CARD_ACE_CARO
        .byte <CARD_KING_HEART
        .byte <CARD_KING_CARO
        .byte <CARD_QUEEN_HEART
        .byte <CARD_QUEEN_CARO
        .byte <CARD_JACK_HEART
        .byte <CARD_JACK_CARO
        .byte <CARD_10_HEART
        .byte <CARD_10_CARO
        .byte <CARD_9_HEART
        .byte <CARD_9_CARO
        .byte <CARD_8_HEART
        .byte <CARD_8_CARO
        .byte <CARD_7_HEART
        .byte <CARD_7_CARO
        .byte <CARD_6_HEART
        .byte <CARD_6_CARO
        .byte <CARD_5_HEART
        .byte <CARD_5_CARO
        .byte <CARD_4_HEART
        .byte <CARD_4_CARO
        .byte <CARD_3_HEART
        .byte <CARD_3_CARO
        .byte <CARD_2_HEART
        .byte <CARD_2_CARO

        .byte <CARD_BLANK
        .byte <DIGIT_0
        .byte <DIGIT_1
        .byte <DIGIT_2
        .byte <DIGIT_3
        .byte <DIGIT_4
        .byte <DIGIT_5
        .byte <DIGIT_6
        .byte <DIGIT_7
        .byte <DIGIT_8
        .byte <DIGIT_9
        .byte <DIGIT_10
        .byte <DIGIT_11
        .byte <DIGIT_12
        .byte <DIGIT_13
        .byte <DIGIT_14
        .byte <DIGIT_15
        .byte <DIGIT_16
        .byte <DIGIT_17
        .byte <DIGIT_18
        .byte <DIGIT_19
        .byte <DIGIT_20
        .byte <DIGIT_21
        .byte <DIGIT_22
        .byte <DIGIT_23
        .byte <DIGIT_24
        .byte <DIGIT_25
        .byte <DIGIT_26
        .byte <DIGIT_27
        .byte <DIGIT_28
        .byte <DIGIT_29
        .byte <DIGIT_30
        .byte <DIGIT_31
        .byte <DIGIT_32
        .byte <DIGIT_33
        .byte <DIGIT_34
        .byte <DIGIT_35
        .byte <DIGIT_36
        .byte <DIGIT_37
        .byte <DIGIT_38
        .byte <DIGIT_39
        .byte <DIGIT_40
        .byte <DIGIT_41
        .byte <DIGIT_42
        .byte <DIGIT_43
        .byte <DIGIT_44
        .byte <DIGIT_45
        .byte <DIGIT_46
        .byte <DIGIT_47
        .byte <DIGIT_48
        .byte <DIGIT_49
        .byte <DIGIT_50
        .byte <DIGIT_51
        .byte <DIGIT_52
        .byte <DIGIT_53
        .byte <DIGIT_54
        .byte <DIGIT_55
        .byte <DIGIT_56
        .byte <DIGIT_57
        .byte <DIGIT_58
        .byte <DIGIT_59
        .byte <DIGIT_60
        .byte <DIGIT_61
        .byte <DIGIT_62
        .byte <DIGIT_63
        .byte <DIGIT_64
        .byte <DIGIT_65
        .byte <DIGIT_66
        .byte <DIGIT_67
        .byte <DIGIT_68
        .byte <DIGIT_69
        .byte <DIGIT_70
        .byte <DIGIT_71
        .byte <DIGIT_72
        .byte <DIGIT_73
        .byte <DIGIT_74
        .byte <DIGIT_75
        .byte <DIGIT_76
        .byte <DIGIT_77
        .byte <DIGIT_78
        .byte <DIGIT_79
        .byte <DIGIT_80
        .byte <DIGIT_81
        .byte <DIGIT_82
        .byte <DIGIT_83
        .byte <DIGIT_84
        .byte <DIGIT_85
        .byte <DIGIT_86
        .byte <DIGIT_87
        .byte <DIGIT_88
        .byte <DIGIT_89
        .byte <DIGIT_90
        .byte <DIGIT_91
        .byte <DIGIT_92
        .byte <DIGIT_93
        .byte <DIGIT_94
        .byte <DIGIT_95
        .byte <DIGIT_96
        .byte <DIGIT_97
        .byte <DIGIT_98
        .byte <DIGIT_99
        .byte <DIGIT_00
        .byte <DIGIT_01
        .byte <DIGIT_02
        .byte <DIGIT_03
        .byte <DIGIT_04
        .byte <DIGIT_05
        .byte <DIGIT_06
        .byte <DIGIT_07
        .byte <DIGIT_08
        .byte <DIGIT_09

        .byte <MOVE_FO
        .byte <MOVE_CH
        .byte <MOVE_BB
        .byte <MOVE_BL
        .byte <MOVE_BH
        .byte <MOVE_CA
        .byte <MOVE_RA

; Kernel Code start

multisprite_setup
    ;--- make sure display kernel starts relatively quick
    lda		#KERNEL_OVERSCAN_TIME
    sta		TIM64T
    rts

drawscreen

nextframe
;        VERTICAL_SYNC
    inc framecounter
WaitForOverscanEnd
    lda TIMINT
    bpl WaitForOverscanEnd

	;------ Vertical Sync
    lda #2
    sta WSYNC
    sta VSYNC
    sta WSYNC
    sta WSYNC
    lsr
    sta WSYNC
    sta VSYNC        ;turn off VSYNC

    lda #KERNEL_OVERSCAN_TIME
    sta TIM64T
      
; 3x lines of VBLANK
        ldx #0
        lda #LEFT_PLAYERS_XPOS
        jsr SetHorizPos        ; set player 0 horiz. pos (1 scanlines)
        inx
        lda #RIGHT_PLAYERS_XPOS
        jsr SetHorizPos        ; set player 0 horiz. pos (1 scanlines)
        ldx #4
        lda game_data+2
        and #ACTIVE_P_COL_MASK
        lsr
        lsr
        lsr
        tay
        lda active_player_sign_position,y
        jsr SetHorizPos        ; set player 0 horiz. pos (1 scanlines)

        lda game_data+2
        and #ROUND_MASK
        tay
        lda roundPlayfieldOffset,y
        sta pf_offset

        lda #EARLY_HMOVE_M00
        sta HMBL

; run possible vblank bB code
  ifconst vblank_bB_code
    jsr vblank_bB_code
  endif

WaitForScreenStart:                    	;-- Line #194 :	    while (RIOT.timint >= 0) {}
	LDA     TIMINT
	BPL     WaitForScreenStart

        ldy #0
        sta WSYNC
        sty VBLANK

	LDA     #<KERNEL_DISPLAY_TIME		;--  start display timer
	STA     TIM64T

        sty temp3

        lda #5
        sta temp2

; Draw 192 scanlines
drawRow
;        sta WSYNC              ; wait for next scanline
        ldx multiply_by_4_table,y
        ldy #7

unpackRowData
        dex
        stx temp1
;        sta WSYNC        ; wait for next scanline
        lda player_data,x
        tax
        lda card_pointers_hi,x
        sta current_row,y
        dey
        lda card_pointers_lo,x
        sta current_row,y

        cpx #26
        beq bluecard
        bpl redcard
        lda #_00
        jmp setCardColor
bluecard
        lda #_80
        jmp setCardColor
redcard
        lda #_40
setCardColor
        sta current_color,y     ; only every second used (0,2,4,6)

        ldx temp1
        dey
        bpl unpackRowData


;        lda pf_offset
;        adc row_counter
;        tax


        ldx row_counter
        lda DataPF0,x
        sta PF0
        lda DataPF1,x
        sta PF1
        lda DataPF2,x
        sta PF2
        ldy #4                  ; y is sprite data index

        cpx #7                    ; rows 3-7 are wideRow
        bpl narrowRow
        cpx #3
        bmi narrowRow

wideRow
        sta WSYNC               ; wait for next scanline
        lda current_color_1
        sta COLUP0
        lda (current_row),y
        sta GRP0                ; set sprite0 pixels

        lda (current_row+4),y
        sta.w GRP1
;        nop

        ldx current_color_2
        lda (current_row+2),y
        sta GRP0
        stx COLUP0

        lda current_color_3
        sta COLUP1

        lda (current_row+6),y
        ldx current_color_4
        SLEEP 7
        stx COLUP1
        sta GRP1
        dey
        bpl wideRow                ; repeat next scanline until finished
        jmp endOfRow

narrowRow
        sta WSYNC              ; wait for next scanline
        lda current_color_1
        sta COLUP0
        lda (current_row),y
        sta GRP0               ; set sprite 0 pixels

        lda (current_row+4),y
        sta GRP1
        nop

        ldx current_color_2
        lda (current_row+2),y
        sta GRP0
        stx COLUP0

        lda current_color_3
        sta COLUP1

        lda (current_row+6),y
        ldx current_color_4
        SLEEP 5
        stx COLUP1
        sta GRP1
        dey
        bpl narrowRow                ; repeat next scanline until finished

endOfRow
        iny                            ; +2    2
        sta WSYNC

        sty GRP0                    ; +3    5
        sty GRP1                    ; +3    8

        inc row_counter                ; +5    13
        ldy row_counter                ; +3    16
        cpy temp2                     ; +2    18    a = row_counter
        bpl drawBottomOfLastCard    ; +2    20

        lda card_hmove_table_1-1,y    ; +4    24
        sta HMP0                    ; +3    27

        sta RESP0                    ; +3    30
        lda card_hmove_table_2-1,y    ; +4    34
        sta HMP1                    ; +3    37

        SLEEP 19                    ;+17    54    other TIA updates can be done here
        sta RESP1                    ; +3    57
        SLEEP 13                    ;+13    70

;        lda #$c0                ; +2    22
;        sta HMP0                ; +3     25
;        sta HMP1                ; +3     28
;        SLEEP 42                ; +42    70
        sta HMOVE                ; +3     73
        sta WSYNC

        jmp drawRow

drawBottomOfLastCard
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        lda DataPF0_END,y
        sta PF0
        lda DataPF1_END,y
        sta PF1
        lda DataPF2_END,y
        sta PF2
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        iny
        lda DataPF0_END,y
        sta PF0
        lda DataPF1_END,y
        sta PF1
        lda DataPF2_END,y
        sta PF2
        sta WSYNC
        sta WSYNC
        sta WSYNC
        lda #%1000
        sta REFP0
        sta REFP1
        sta WSYNC

        ldy #0                 ; y is sprite data index
lastCardIdent
        sta WSYNC              ; wait for next scanline
        lda current_color_1
        sta COLUP0
        lda (current_row),y
        sta GRP0               ; set sprite 0 pixels

        lda (current_row+4),y
        sta.w GRP1


        ldx current_color_2
        lda (current_row+2),y
        stx COLUP0
        sta GRP0

        lda current_color_3
        sta COLUP1

        lda (current_row+6),y
        ldx current_color_4
        SLEEP 6
        stx COLUP1
        sta GRP1
        iny
        cpy #5
        bne lastCardIdent            ; repeat next scanline until finished

        ldy #0
        sty GRP0
        sty GRP1


        sta WSYNC

        sty REFP0
        sty REFP1
        sty PF0
        sty PF1
        sty PF2

        lda row_counter
        cmp #10
        bpl endOfAllPlayerSections

        sta WSYNC

        lda game_data+2
        and #NO_ACTIVE_P_MASK
        bne skipActivePlayerMarkTop
        lda game_data+2
        and #ACTIVE_P_ROW_MASK
        bne skipActivePlayerMarkTop
        lda #2
        sta ENABL
skipActivePlayerMarkTop

        jsr purseKernelA
        sty ENABL                 ; y = 0: disable ball

        ldx #3
        jsr spacer

        jmp potKernel
returnToBottomPlayers

        lda #8
        sta temp3

        sta WSYNC

        lda game_data+2
        and #NO_ACTIVE_P_MASK
        bne skipActivePlayerMarkBottom
        lda game_data+2
        and #ACTIVE_P_ROW_MASK
        beq skipActivePlayerMarkBottom
        lda #2
        sta ENABL
skipActivePlayerMarkBottom
        jsr purseKernelA       ; reposition for bottom player section
        sty ENABL                 ; y = 0: disable ball


        jsr Reposition            ; reposition for bottom player section


        lda #10
        sta temp2
        ldy row_counter
        sta WSYNC
        jmp drawRow

endOfAllPlayerSections
        sty row_counter     ; y = 0 !


WaitForDisplayEnd:
    LDA     TIMINT
    BPL     WaitForDisplayEnd

    ;--- Start VBLANK timer

    lda  #KERNEL_VBLANK_TIME
    sta  TIM64T

        RETURN

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; extra kernels:
;
potKernel
    sta WSYNC
;reposition for Pot / Round display
    lda #0                ; 2  2
    sta GRP0              ; 3  5
    sta GRP1              ; 3  8
;    sta NUSIZ1            ; 3 11
    lda #EARLY_HMOVE_M03  ; 2 13
    sta HMP0              ; 3 16
    lda #EARLY_HMOVE_M00  ; 2 17
    sta HMP1              ; 3 20
    lda #_0E              ; 2 22
    sta COLUP0            ; 3 25
    sta COLUP1            ; 3 28
    SLEEP 9               ;23 51 (other TIA updates can be done here)
    sta RESP0             ; 3 54
    SLEEP 4               ; 2 56 (other TIA updates can be done here)
    sta RESP1             ; 3 59
    SLEEP 26              ;11 70 (other TIA updates can be done here)
    sta HMOVE             ; 3 73
    sta WSYNC             ; 3

; unpack Pot / Round data int currentRow
        lda game_data+2
        and #ROUND_MASK
        adc #DIGIT_0_ID
        tax
        lda card_pointers_hi,x
        sta current_row+1
        lda card_pointers_lo,x
        sta current_row

        ldx game_data
        lda card_pointers_hi,x
        sta current_row+3
        lda card_pointers_lo,x
        sta current_row+2

        ldx game_data+1
        lda card_pointers_hi,x
        sta current_row+5
        lda card_pointers_lo,x
        sta current_row+4

        ldx current_move
        lda card_pointers_hi,x
        sta current_row+7
        lda card_pointers_lo,x
        sta current_row+6

        ldy #4
RoundPotLoop
    sta WSYNC    ; 3
        lda (current_row),y
        sta GRP0                ; set sprite0 pixels
        lda (current_row+2),y
        sta GRP1                ; set sprite1 pixels

        SLEEP 20

        lda (current_row+4),y
        sta GRP0                ; set sprite0 pixels
        lda (current_row+6),y
        sta GRP1                ; set sprite1 pixels

    dey
    bpl RoundPotLoop
        iny
        sty GRP0
        sty GRP1

        jmp returnToBottomPlayers


purseKernelA
    sta WSYNC
    lda framecounter        ; 3  3
    and #1                  ; 2  5
    tay                     ; 2  7
    lda pursesP0Offset,y    ; 4 12
    sta HMP0                ; 3 15
    lda pursesP1Offset,y    ; 4 19
    sta HMP1                ; 3 22

    SLEEP 4                 ; 4 25
    sta RESP0               ; 3 28
    lda #_Color_Purse       ; 2 30
    sta COLUP0              ; 3 33
    sta COLUP1              ; 3 36
    tya                     ; 2 38
    adc temp3               ; 3 41
    tay                     ; 2 43


    SLEEP 13                ;13 56    other TIA updates can be done here
    sta RESP1               ; 3 59
    SLEEP 11                ;11 70
    sta HMOVE               ; 3 73
    sta WSYNC               ; 3

        ldx player_purse,y
        lda card_pointers_hi,x
        sta current_row+1
        lda card_pointers_lo,x
        sta current_row

        ldx player_purse+2,y
        lda card_pointers_hi,x
        sta current_row+3
        lda card_pointers_lo,x
        sta current_row+2

        ldx player_purse+4,y
        lda card_pointers_hi,x
        sta current_row+5
        lda card_pointers_lo,x
        sta current_row+4

        ldx player_purse+6,y
        lda card_pointers_hi,x
        sta current_row+7
        lda card_pointers_lo,x
        sta current_row+6

        ldy #4
PurseLoop
        sta WSYNC               ; 
        lda (current_row),y     ; 5
        sta GRP0                ; 3  8
        lda (current_row+4),y   ; 5 13
        sta GRP1                ; 3 16

        SLEEP 10                ;10 26 

        lda (current_row+2),y   ; 5 31
        sta GRP0                ; 3 34
        SLEEP 20                ;20 54
        lda (current_row+6),y   ; 5
        sta GRP1                ; 3
        dey
        bpl PurseLoop
        iny
        sty GRP0
        sty GRP1
        rts

; Subroutines !!

Reposition                  ;    v2
    sta WSYNC
    lda #0                  ; 2  2
    sta GRP0                ; 3  5
    sta GRP1                ; 3  8
    lda #EARLY_HMOVE_M09    ; 2 10
    sta HMP0                ; 3 13
    lda #EARLY_HMOVE_M06    ; 2 15
    sta HMP1                ; 3 18
    SLEEP 7                 ; 7 25 other TIA updates can be done here
    sta RESP0               ; 3 28
    SLEEP 28                ;28 56    other TIA updates can be done here
    sta RESP1               ; 3 59
    SLEEP 11                ;11 70
    sta HMOVE               ; 3 73
    sta WSYNC               ; 3
        rts


spacer
        sta WSYNC        ; wait for next scanline
        dex
        bne spacer
        rts

SetHorizPos
;call this function with A == horizontal position (0-159)
;and X == the object to be positioned (0=P0, 1=P1, 2=M0, etc.)
;if you do not wish to write to P1 during this function, make
;sure Y==0 before you call it.  This function will change Y, and A
;will be the value put into HMxx when returned.
;Call this function with at least 11 cycles left in the scanline 
;(jsr + sec + sta WSYNC = 11); it will return 9 cycles
;into the second scanline
        sec
        sta WSYNC                       ;begin line 1
        sta.w HMCLR                     ;+4         4
DivideBy15Loop
        sbc #15
        bcs DivideBy15Loop              ;+4/5        8/13.../58

        tay                             ;+2        10/15/...60
        lda FineAdjustTableEnd,Y        ;+5        15/20/...65

        sta HMP0,X                      ;+4        19/24/...69
        sta RESP0,X                     ;+4        23/28/33/38/43/48/53/58/63/68/73
        sta WSYNC                       ;+3         0        begin line 2
        sta HMOVE                       ;+3
        rts

multiply_by_4_table
        .byte 4,8,12,16,20
        .byte 24,28,32,36,40
;multiply_by_7_table
;        .byte 7,14,21,28,35,42,49,56
;        .byte 6,12,18,24,30,36,42,48
;        .byte 5,11,17,23,29,35,41,47

active_player_sign_position
;        .byte 29,61,93,125
        .byte 13,45,109,141


card_hmove_table_1
        .byte EARLY_HMOVE_M00
        .byte EARLY_HMOVE_M04
        .byte EARLY_HMOVE_M08
        .byte EARLY_HMOVE_M12

        .byte EARLY_HMOVE_M12 ; skipped!
        .byte EARLY_HMOVE_M08
        .byte EARLY_HMOVE_M04
        .byte EARLY_HMOVE_M00
        .byte EARLY_HMOVE_M00 ; should be +4

card_hmove_table_2
        .byte EARLY_HMOVE_M15
        .byte EARLY_HMOVE_M11
        .byte EARLY_HMOVE_M07
        .byte EARLY_HMOVE_M03

        .byte EARLY_HMOVE_M02 ; skipped
        .byte EARLY_HMOVE_M06
        .byte EARLY_HMOVE_M10
        .byte EARLY_HMOVE_M14
        .byte EARLY_HMOVE_M15 ; should be -18


DataPF0_X:  ; two cards round 1
    .byte $00,$00,$00,$00,$00
    .byte $C0,$C0,$C0,$80,$00
DataPF0: ; three cards round 2
    .byte $00,$00,$00,$80,$C0
    .byte $C0,$C0,$C0,$80,$00
DataPF0_3: ; four cards round 3
    .byte $00,$00,$00,$80,$C0
    .byte $C0,$C0,$C0,$80,$00
DataPF0_4: ; five cards round 4 and 5
    .byte $00,$00,$00,$80,$C0
    .byte $C0,$C0,$C0,$80,$00

DataPF1_X: ; two cards round 1
    .byte $3C,$7C,$7C,$78,$00
    .byte $C3,$E3,$E3,$E1,$00
DataPF1: ; three cards round 2
    .byte $3C,$7C,$FC,$F9,$F3
    .byte $C3,$E3,$F3,$F9,$FC
DataPF1_3: ; four cards round 3
    .byte $3C,$7C,$FC,$F9,$F3
    .byte $C3,$E3,$F3,$F9,$FC
DataPF1_4: ; five cards round 4 and 5
    .byte $3C,$7C,$FC,$F9,$F3
    .byte $C3,$E3,$F3,$F9,$FC

DataPF2_X: ; two cards round 1
    .byte $3C,$3E,$3E,$1E,$00
    .byte $03,$07,$07,$07,$00
DataPF2: ; three cards round 2
    .byte $3C,$3E,$3F,$1F,$0F
    .byte $03,$07,$0F,$1F,$3F
DataPF2_3: ; four cards round 3
    .byte $3C,$3E,$3F,$1F,$0F
    .byte $03,$07,$0F,$1F,$3F
DataPF2_4: ; five cards round 4 and 5
    .byte $3C,$3E,$3F,$1F,$0F
    .byte $03,$07,$0F,$1F,$3F


DataPF0_END:
    .byte $00,$00,$00,$80,$C0,$C0,$C0
    .byte $00,$80,$C0,$00,$00
DataPF1_END:
    .byte $3C,$7C,$FC,$F9,$F3,$E3,$C3
    .byte $FC,$F9,$F3,$7C,$3C
DataPF2_END:
    .byte $3C,$3E,$3F,$1F,$0F,$07,$03
    .byte $3F,$1F,$0F,$3E,$3C

;playfield_table
;    .byte $00,$78,$78 ;|     XXXX      XXXX | ( 4)
;    .byte $00,$F8,$7C ;|    XXXXX     XXXXX | ( 5)
;    .byte $80,$F8,$7E ;|   XXXXXX    XXXXXX | ( 6)
;    .byte $C0,$F0,$3F ;|  XXXXXX    XXXXXX  | ( 7)
;    .byte $E0,$E1,$1F ;| XXXXXX    XXXXXX   | ( 8)
;    .byte $E0,$C1,$0F ;| XXXXX     XXXXX    | ( 9)
;    .byte $E0,$81,$07 ;| XXXX      XXXX     | (10)

;    .byte $00,$3C,$3C ;|      XXXX    XXXX  | ( 4)
;    .byte $00,$7C,$3E ;|     XXXXX   XXXXX  | ( 5)
;    .byte $00,$FC,$3F ;|    XXXXXX  XXXXXX  | ( 6)
;    .byte $80,$F9,$1F ;|   XXXXXX  XXXXXX   | ( 7)
;    .byte $C0,$F3,$0F ;|  XXXXXX  XXXXXX    | ( 8)
;    .byte $C0,$E3,$07 ;|  XXXXX   XXXXX     | ( 9)
;    .byte $C0,$C3,$03 ;|  XXXX    XXXX      | (10)

pursesP0Offset
        .byte EARLY_HMOVE_M13, EARLY_HMOVE_M05

pursesP1Offset
        .byte EARLY_HMOVE_M10, EARLY_HMOVE_M02

roundPlayfieldOffset
        .byte 0, 0, 10, 20, 30, 30, 30

        align 256
FineAdjustTableBegin
        .byte %01100000                ;left 6
        .byte %01010000
        .byte %01000000
        .byte %00110000
        .byte %00100000
        .byte %00010000
        .byte %00000000                ;left 0
        .byte %11110000
        .byte %11100000
        .byte %11010000
        .byte %11000000
        .byte %10110000
        .byte %10100000
        .byte %10010000
        .byte %10000000                ;right 8
FineAdjustTableEnd        =        (FineAdjustTableBegin - 241)
