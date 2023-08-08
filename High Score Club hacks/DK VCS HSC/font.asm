
; ********************************************************************
;  Donkey Kong VCS
;
;    Font graphics
;
;    $Date: Thu, 05 Jan 2017 20:55:15 +0100 $
;    $Author: dietrich $
;    $Revision: 477 $
;
;  Copyright (C) 2017 Andreas Dietrich
; ********************************************************************

        MAC DATA_font

FONT_HEIGHT = 8

DigitFont = *
;
Zero            = DigitFont + 0 * FONT_HEIGHT
One             = DigitFont + 1 * FONT_HEIGHT
Two             = DigitFont + 2 * FONT_HEIGHT
Three           = DigitFont + 3 * FONT_HEIGHT
Four            = DigitFont + 4 * FONT_HEIGHT
Five            = DigitFont + 5 * FONT_HEIGHT
Six             = DigitFont + 6 * FONT_HEIGHT
Seven           = DigitFont + 7 * FONT_HEIGHT
Eight           = DigitFont + 8 * FONT_HEIGHT
Nine            = DigitFont + 9 * FONT_HEIGHT

LetterFont = DigitFont + 10 * FONT_HEIGHT
;
Space           = LetterFont +  0 * FONT_HEIGHT
A               = LetterFont +  1 * FONT_HEIGHT
B               = LetterFont +  2 * FONT_HEIGHT
C               = LetterFont +  3 * FONT_HEIGHT
D               = LetterFont +  4 * FONT_HEIGHT
E               = LetterFont +  5 * FONT_HEIGHT
F               = LetterFont +  6 * FONT_HEIGHT
G               = LetterFont +  7 * FONT_HEIGHT
H               = LetterFont +  8 * FONT_HEIGHT
I               = LetterFont +  9 * FONT_HEIGHT
J               = LetterFont + 10 * FONT_HEIGHT
K               = LetterFont + 11 * FONT_HEIGHT
L               = LetterFont + 12 * FONT_HEIGHT
M               = LetterFont + 13 * FONT_HEIGHT
N               = LetterFont + 14 * FONT_HEIGHT
O               = LetterFont + 15 * FONT_HEIGHT
P               = LetterFont + 16 * FONT_HEIGHT
Q               = LetterFont + 17 * FONT_HEIGHT
R               = LetterFont + 18 * FONT_HEIGHT
S               = LetterFont + 19 * FONT_HEIGHT
T               = LetterFont + 20 * FONT_HEIGHT
U               = LetterFont + 21 * FONT_HEIGHT
V               = LetterFont + 22 * FONT_HEIGHT
W               = LetterFont + 23 * FONT_HEIGHT
X               = LetterFont + 24 * FONT_HEIGHT
Y               = LetterFont + 25 * FONT_HEIGHT
Z               = LetterFont + 26 * FONT_HEIGHT

SpecialCharacterFont = LetterFont + 27 * FONT_HEIGHT
;
zero            = SpecialCharacterFont +  0 * FONT_HEIGHT
one             = SpecialCharacterFont +  1 * FONT_HEIGHT
two             = SpecialCharacterFont +  2 * FONT_HEIGHT
three           = SpecialCharacterFont +  3 * FONT_HEIGHT
four            = SpecialCharacterFont +  4 * FONT_HEIGHT
five            = SpecialCharacterFont +  5 * FONT_HEIGHT
six             = SpecialCharacterFont +  6 * FONT_HEIGHT
seven           = SpecialCharacterFont +  7 * FONT_HEIGHT
eight           = SpecialCharacterFont +  8 * FONT_HEIGHT
nine            = SpecialCharacterFont +  9 * FONT_HEIGHT
m               = SpecialCharacterFont + 10 * FONT_HEIGHT
Question        = SpecialCharacterFont + 11 * FONT_HEIGHT
Dot             = SpecialCharacterFont + 12 * FONT_HEIGHT
Copyleft        = SpecialCharacterFont + 13 * FONT_HEIGHT
Copyright       = SpecialCharacterFont + 14 * FONT_HEIGHT

                SUBROUTINE

; Digit font

.Zero           BYTE    %00000000 ; |        |
                BYTE    %00011100 ; |   XXX  |
                BYTE    %00110010 ; |  XX  X |
                BYTE    %01100011 ; | XX   XX|
                BYTE    %01100011 ; | XX   XX|
                BYTE    %01100011 ; | XX   XX|
                BYTE    %00100110 ; |  X  XX |
                BYTE    %00011100 ; |   XXX  |

.One            BYTE    %00000000 ; |        |
                BYTE    %00111111 ; |  XXXXXX|
                BYTE    %00001100 ; |    XX  |
                BYTE    %00001100 ; |    XX  |
                BYTE    %00001100 ; |    XX  |
                BYTE    %00001100 ; |    XX  |
                BYTE    %00011100 ; |   XXX  |
                BYTE    %00001100 ; |    XX  |

.Two            BYTE    %00000000 ; |        |
                BYTE    %01111111 ; | XXXXXXX|
                BYTE    %01110000 ; | XXX    |
                BYTE    %00111100 ; |  XXXX  |
                BYTE    %00011110 ; |   XXXX |
                BYTE    %00000111 ; |     XXX|
                BYTE    %01100011 ; | XX   XX|
                BYTE    %00111110 ; |  XXXXX |

.Three          BYTE    %00000000 ; |        |
                BYTE    %00111110 ; |  XXXXX |
                BYTE    %01100011 ; | XX   XX|
                BYTE    %00000011 ; |      XX|
                BYTE    %00011110 ; |   XXXX |
                BYTE    %00001100 ; |    XX  |
                BYTE    %00000110 ; |     XX |
                BYTE    %00111111 ; |  XXXXXX|

.Four           BYTE    %00000000 ; |        |
                BYTE    %00000110 ; |     XX |
                BYTE    %00000110 ; |     XX |
                BYTE    %01111111 ; | XXXXXXX|
                BYTE    %01100110 ; | XX  XX |
                BYTE    %00110110 ; |  XX XX |
                BYTE    %00011110 ; |   XXXX |
                BYTE    %00001110 ; |    XXX |

.Five           BYTE    %00000000 ; |        |
                BYTE    %00111110 ; |  XXXXX |
                BYTE    %01100011 ; | XX   XX|
                BYTE    %00000011 ; |      XX|
                BYTE    %00000011 ; |      XX|
                BYTE    %01111110 ; | XXXXXX |
                BYTE    %01100000 ; | XX     |
                BYTE    %01111110 ; | XXXXXX |

.Six            BYTE    %00000000 ; |        |
                BYTE    %00111110 ; |  XXXXX |
                BYTE    %01100011 ; | XX   XX|
                BYTE    %01100011 ; | XX   XX|
                BYTE    %01111110 ; | XXXXXX |
                BYTE    %01100000 ; | XX     |
                BYTE    %00110000 ; |  XX    |
                BYTE    %00011110 ; |   XXXX |

.Seven          BYTE    %00000000 ; |        |
                BYTE    %00011000 ; |   XX   |
                BYTE    %00011000 ; |   XX   |
                BYTE    %00011000 ; |   XX   |
                BYTE    %00001100 ; |    XX  |
                BYTE    %00000110 ; |     XX |
                BYTE    %01100011 ; | XX   XX|
                BYTE    %01111111 ; | XXXXXXX|

.Eight          BYTE    %00000000 ; |        |
                BYTE    %00111110 ; |  XXXXX |
                BYTE    %01000011 ; | X    XX|
                BYTE    %01001111 ; | X  XXXX|
                BYTE    %00111100 ; |  XXXX  |
                BYTE    %01110010 ; | XXX  X |
                BYTE    %01100010 ; | XX   X |
                BYTE    %00111100 ; |  XXXX  |

.Nine           BYTE    %00000000 ; |        |
                BYTE    %00111100 ; |  XXXX  |
                BYTE    %00000110 ; |     XX |
                BYTE    %00000011 ; |      XX|
                BYTE    %00111111 ; |  XXXXXX|
                BYTE    %01100011 ; | XX   XX|
                BYTE    %01100011 ; | XX   XX|
                BYTE    %00111110 ; |  XXXXX |

; Letter font

.Space          BYTE    %00000000 ; |        |
                BYTE    %00000000 ; |        |
                BYTE    %00000000 ; |        |
                BYTE    %00000000 ; |        |
                BYTE    %00000000 ; |        |
                BYTE    %00000000 ; |        |
                BYTE    %00000000 ; |        |
                BYTE    %00000000 ; |        |

.A              BYTE    %00000000 ; |        |
                BYTE    %01100011 ; | XX   XX|
                BYTE    %01100011 ; | XX   XX|
                BYTE    %01111111 ; | XXXXXXX|
                BYTE    %01100011 ; | XX   XX|
                BYTE    %01100011 ; | XX   XX|
                BYTE    %00110110 ; |  XX XX |
                BYTE    %00011100 ; |   XXX  |

.B              BYTE    %00000000 ; |        |
                BYTE    %01111110 ; | XXXXXX |
                BYTE    %01100011 ; | XX   XX|
                BYTE    %01100011 ; | XX   XX|
                BYTE    %01111110 ; | XXXXXX |
                BYTE    %01100011 ; | XX   XX|
                BYTE    %01100011 ; | XX   XX|
                BYTE    %01111110 ; | XXXXXX |

.C              BYTE    %00000000 ; |        |
                BYTE    %00011110 ; |   XXXX |
                BYTE    %00110011 ; |  XX  XX|
                BYTE    %01100000 ; | XX     |
                BYTE    %01100000 ; | XX     |
                BYTE    %01100000 ; | XX     |
                BYTE    %00110011 ; |  XX  XX|
                BYTE    %00011110 ; |   XXXX |

.D              BYTE    %00000000 ; |        |
                BYTE    %01111100 ; | XXXXX  |
                BYTE    %01100110 ; | XX  XX |
                BYTE    %01100011 ; | XX   XX|
                BYTE    %01100011 ; | XX   XX|
                BYTE    %01100011 ; | XX   XX|
                BYTE    %01100110 ; | XX  XX |
                BYTE    %01111100 ; | XXXXX  |

.E              BYTE    %00000000 ; |        | ; Original : BYTE    %00000000 ; |        |
                BYTE    %01111111 ; | XXXXXXX| ;            BYTE    %00111111 ; |  XXXXXX|
                BYTE    %01100000 ; | XX     | ;            BYTE    %00110000 ; |  XX    |
                BYTE    %01100000 ; | XX     | ;            BYTE    %00110000 ; |  XX    |
                BYTE    %01111110 ; | XXXXXX | ;            BYTE    %00111110 ; |  XXXXX |
                BYTE    %01100000 ; | XX     | ;            BYTE    %00110000 ; |  XX    |
                BYTE    %01100000 ; | XX     | ;            BYTE    %00110000 ; |  XX    |
                BYTE    %01111111 ; | XXXXXXX| ;            BYTE    %00111111 ; |  XXXXXX|

.F              BYTE    %00000000 ; |        |
                BYTE    %01100000 ; | XX     |
                BYTE    %01100000 ; | XX     |
                BYTE    %01100000 ; | XX     |
                BYTE    %01111110 ; | XXXXXX |
                BYTE    %01100000 ; | XX     |
                BYTE    %01100000 ; | XX     |
                BYTE    %01111111 ; | XXXXXXX|

.G              BYTE    %00000000 ; |        |
                BYTE    %00011111 ; |   XXXXX|
                BYTE    %00110011 ; |  XX  XX|
                BYTE    %01100011 ; | XX   XX|
                BYTE    %01100111 ; | XX  XXX|
                BYTE    %01100000 ; | XX     |
                BYTE    %00110000 ; |  XX    |
                BYTE    %00011111 ; |   XXXXX|

.H              BYTE    %00000000 ; |        |
                BYTE    %01100011 ; | XX   XX|
                BYTE    %01100011 ; | XX   XX|
                BYTE    %01100011 ; | XX   XX|
                BYTE    %01111111 ; | XXXXXXX|
                BYTE    %01100011 ; | XX   XX|
                BYTE    %01100011 ; | XX   XX|
                BYTE    %01100011 ; | XX   XX|

.I              BYTE    %00000000 ; |        |
                BYTE    %00111111 ; |  XXXXXX|
                BYTE    %00001100 ; |    XX  |
                BYTE    %00001100 ; |    XX  |
                BYTE    %00001100 ; |    XX  |
                BYTE    %00001100 ; |    XX  |
                BYTE    %00001100 ; |    XX  |
                BYTE    %00111111 ; |  XXXXXX|

.J              BYTE    %00000000 ; |        |
                BYTE    %00111110 ; |  XXXXX |
                BYTE    %01100011 ; | XX   XX|
                BYTE    %00000011 ; |      XX|
                BYTE    %00000011 ; |      XX|
                BYTE    %00000011 ; |      XX|
                BYTE    %00000011 ; |      XX|
                BYTE    %00000011 ; |      XX|

.K              BYTE    %00000000 ; |        |
                BYTE    %01100111 ; | XX  XXX|
                BYTE    %01101110 ; | XX XXX |
                BYTE    %01111100 ; | XXXXX  |
                BYTE    %01111000 ; | XXXX   |
                BYTE    %01101100 ; | XX XX  |
                BYTE    %01100110 ; | XX  XX |
                BYTE    %01100011 ; | XX   XX|

.L              BYTE    %00000000 ; |        |
                BYTE    %00111111 ; |  XXXXXX|
                BYTE    %00110000 ; |  XX    |
                BYTE    %00110000 ; |  XX    |
                BYTE    %00110000 ; |  XX    |
                BYTE    %00110000 ; |  XX    |
                BYTE    %00110000 ; |  XX    |
                BYTE    %00110000 ; |  XX    |

.M              BYTE    %00000000 ; |        |
                BYTE    %01100011 ; | XX   XX|
                BYTE    %01100011 ; | XX   XX|
                BYTE    %01101011 ; | XX X XX|
                BYTE    %01111111 ; | XXXXXXX|
                BYTE    %01111111 ; | XXXXXXX|
                BYTE    %01110111 ; | XXX XXX|
                BYTE    %01100011 ; | XX   XX|

.N              BYTE    %00000000 ; |        |
                BYTE    %01100011 ; | XX   XX|
                BYTE    %01100111 ; | XX  XXX|
                BYTE    %01101111 ; | XX XXXX|
                BYTE    %01111111 ; | XXXXXXX|
                BYTE    %01111011 ; | XXXX XX|
                BYTE    %01110011 ; | XXX  XX|
                BYTE    %01100011 ; | XX   XX|

.O              BYTE    %00000000 ; |        |
                BYTE    %00111110 ; |  XXXXX |
                BYTE    %01100011 ; | XX   XX|
                BYTE    %01100011 ; | XX   XX|
                BYTE    %01100011 ; | XX   XX|
                BYTE    %01100011 ; | XX   XX|
                BYTE    %01100011 ; | XX   XX|
                BYTE    %00111110 ; |  XXXXX |

.P              BYTE    %00000000 ; |        |
                BYTE    %01100000 ; | XX     |
                BYTE    %01100000 ; | XX     |
                BYTE    %01111110 ; | XXXXXX |
                BYTE    %01100011 ; | XX   XX|
                BYTE    %01100011 ; | XX   XX|
                BYTE    %01100011 ; | XX   XX|
                BYTE    %01111110 ; | XXXXXX |

.Q              BYTE    %00000000 ; |        |
                BYTE    %00111101 ; |  XXXX X|
                BYTE    %01100110 ; | XX  XX |
                BYTE    %01101111 ; | XX XXXX|
                BYTE    %01100011 ; | XX   XX|
                BYTE    %01100011 ; | XX   XX|
                BYTE    %01100011 ; | XX   XX|
                BYTE    %00111110 ; |  XXXXX |

.R              BYTE    %00000000 ; |        |
                BYTE    %01100111 ; | XX  XXX|
                BYTE    %01101110 ; | XX XXX |
                BYTE    %01111100 ; | XXXXX  |
                BYTE    %01100111 ; | XX  XXX|
                BYTE    %01100011 ; | XX   XX|
                BYTE    %01100011 ; | XX   XX|
                BYTE    %01111110 ; | XXXXXX |

.S              BYTE    %00000000 ; |        |
                BYTE    %00111110 ; |  XXXXX |
                BYTE    %01100011 ; | XX   XX|
                BYTE    %00000011 ; |      XX|
                BYTE    %00111110 ; |  XXXXX |
                BYTE    %01100000 ; | XX     |
                BYTE    %01100110 ; | XX  XX |
                BYTE    %00111100 ; |  XXXX  |

.T              BYTE    %00000000 ; |        |
                BYTE    %00001100 ; |    XX  |
                BYTE    %00001100 ; |    XX  |
                BYTE    %00001100 ; |    XX  |
                BYTE    %00001100 ; |    XX  |
                BYTE    %00001100 ; |    XX  |
                BYTE    %00001100 ; |    XX  |
                BYTE    %00111111 ; |  XXXXXX|

.U              BYTE    %00000000 ; |        |
                BYTE    %00111110 ; |  XXXXX |
                BYTE    %01100011 ; | XX   XX|
                BYTE    %01100011 ; | XX   XX|
                BYTE    %01100011 ; | XX   XX|
                BYTE    %01100011 ; | XX   XX|
                BYTE    %01100011 ; | XX   XX|
                BYTE    %01100011 ; | XX   XX|

.V              BYTE    %00000000 ; |        |
                BYTE    %00001000 ; |    X   |
                BYTE    %00011100 ; |   XXX  |
                BYTE    %00111110 ; |  XXXXX |
                BYTE    %01110111 ; | XXX XXX|
                BYTE    %01100011 ; | XX   XX|
                BYTE    %01100011 ; | XX   XX|
                BYTE    %01100011 ; | XX   XX|

.W              BYTE    %00000000 ; |        |
                BYTE    %00100010 ; |  X   X |
                BYTE    %00110110 ; |  XX XX |
                BYTE    %01111111 ; | XXXXXXX|
                BYTE    %01111111 ; | XXXXXXX|
                BYTE    %01101011 ; | XX X XX|
                BYTE    %01100011 ; | XX   XX|
                BYTE    %01100011 ; | XX   XX|

.X              BYTE    %00000000 ; |        |
                BYTE    %01100011 ; | XX   XX|
                BYTE    %01110111 ; | XXX XXX|
                BYTE    %00111110 ; |  XXXXX |
                BYTE    %00011100 ; |   XXX  |
                BYTE    %00111110 ; |  XXXXX |
                BYTE    %01110111 ; | XXX XXX|
                BYTE    %01100011 ; | XX   XX|

.Y              BYTE    %00000000 ; |        |
                BYTE    %00001100 ; |    XX  |
                BYTE    %00001100 ; |    XX  |
                BYTE    %00001100 ; |    XX  |
                BYTE    %00011110 ; |   XXXX |
                BYTE    %00010010 ; |   X  X |
                BYTE    %00110011 ; |  XX  XX|
                BYTE    %00110011 ; |  XX  XX|

.Z              BYTE    %00000000 ; |        |
                BYTE    %01111111 ; | XXXXXXX|
                BYTE    %01110000 ; | XXX    |
                BYTE    %00111000 ; |  XXX   |
                BYTE    %00011100 ; |   XXX  |
                BYTE    %00001110 ; |    XXX |
                BYTE    %00000111 ; |     XXX|
                BYTE    %01111111 ; | XXXXXXX|

; Special character font

.zero           BYTE    %00000000 ; |        |
                BYTE    %00111100 ; |  XXXX  |
                BYTE    %01100110 ; | XX  XX |
                BYTE    %01110110 ; | XXX XX |
                BYTE    %01101110 ; | XX XXX |
                BYTE    %01100110 ; | XX  XX |
                BYTE    %00111100 ; |  XXXX  |
                BYTE    %00000000 ; |        |

.one            BYTE    %00000000 ; |        |
                BYTE    %01111110 ; | XXXXXX |
                BYTE    %00011000 ; |   XX   |
                BYTE    %00011000 ; |   XX   |
                BYTE    %00011000 ; |   XX   |
                BYTE    %00111000 ; |  XXX   |
                BYTE    %00011000 ; |   XX   |
                BYTE    %00000000 ; |        |

.two            BYTE    %00000000 ; |        |
                BYTE    %01111110 ; | XXXXXX |
                BYTE    %00110000 ; |  XX    |
                BYTE    %00011000 ; |   XX   |
                BYTE    %00001100 ; |    XX  |
                BYTE    %01100110 ; | XX  XX |
                BYTE    %00111100 ; |  XXXX  |
                BYTE    %00000000 ; |        |

.three          BYTE    %00000000 ; |        |
                BYTE    %00111100 ; |  XXXX  |
                BYTE    %01100110 ; | XX  XX |
                BYTE    %00001100 ; |    XX  |
                BYTE    %00011000 ; |   XX   |
                BYTE    %00001100 ; |    XX  |
                BYTE    %01111110 ; | XXXXXX |
                BYTE    %00000000 ; |        |

.four           BYTE    %00000000 ; |        |
                BYTE    %00001100 ; |    XX  |
                BYTE    %01111110 ; | XXXXXX |
                BYTE    %01101100 ; | XX XX  |
                BYTE    %00111100 ; |  XXXX  |
                BYTE    %00011100 ; |   XXX  |
                BYTE    %00001100 ; |    XX  |
                BYTE    %00000000 ; |        |

.five           BYTE    %00000000 ; |        |
                BYTE    %00111100 ; |  XXXX  |
                BYTE    %01100110 ; | XX  XX |
                BYTE    %00000110 ; |     XX |
                BYTE    %01111100 ; | XXXXX  |
                BYTE    %01100000 ; | XX     |
                BYTE    %01111110 ; | XXXXXX |
                BYTE    %00000000 ; |        |

.six            BYTE    %00000000 ; |        |
                BYTE    %00111100 ; |  XXXX  |
                BYTE    %01100110 ; | XX  XX |
                BYTE    %01100110 ; | XX  XX |
                BYTE    %01111100 ; | XXXXX  |
                BYTE    %01100000 ; | XX     |
                BYTE    %00111100 ; |  XXXX  |
                BYTE    %00000000 ; |        |

.seven          BYTE    %00000000 ; |        |
                BYTE    %00110000 ; |  XX    |
                BYTE    %00110000 ; |  XX    |
                BYTE    %00011000 ; |   XX   |
                BYTE    %00001100 ; |    XX  |
                BYTE    %00000110 ; |     XX |
                BYTE    %01111110 ; | XXXXXX |
                BYTE    %00000000 ; |        |

.eight          BYTE    %00000000 ; |        |
                BYTE    %00111100 ; |  XXXX  |
                BYTE    %01100110 ; | XX  XX |
                BYTE    %01100110 ; | XX  XX |
                BYTE    %00111100 ; |  XXXX  |
                BYTE    %01100110 ; | XX  XX |
                BYTE    %00111100 ; |  XXXX  |
                BYTE    %00000000 ; |        |

.nine           BYTE    %00000000 ; |        |
                BYTE    %00111000 ; |  XXX   |
                BYTE    %00001100 ; |    XX  |
                BYTE    %00000110 ; |     XX |
                BYTE    %00111110 ; |  XXXXX |
                BYTE    %01100110 ; | XX  XX |
                BYTE    %00111100 ; |  XXXX  |
                BYTE    %00000000 ; |        |

.m              BYTE    %00000000 ; |        |
                BYTE    %01001001 ; | X  X  X|
                BYTE    %01001001 ; | X  X  X|
                BYTE    %01001001 ; | X  X  X|
                BYTE    %01001001 ; | X  X  X|
                BYTE    %01110110 ; | XXX XX |
                BYTE    %00000000 ; |        |
                BYTE    %00000000 ; |        |

.Question       BYTE    %00000000 ; |        |
                BYTE    %00010000 ; |   X    |
                BYTE    %00000000 ; |        |
                BYTE    %00010000 ; |   X    |
                BYTE    %00001100 ; |    XX  |
                BYTE    %00000010 ; |      X |
                BYTE    %01000010 ; | X    X |
                BYTE    %00111100 ; |  XXXX  |

.Dot            BYTE    %00000000 ; |        |
                BYTE    %00110000 ; |  XX    |
                BYTE    %00110000 ; |  XX    |
                BYTE    %00000000 ; |        |
                BYTE    %00000000 ; |        |
                BYTE    %00000000 ; |        |
                BYTE    %00000000 ; |        |
                BYTE    %00000000 ; |        |

.Copyleft       BYTE    %00000111 ; |     XXX|
                BYTE    %00001000 ; |    X   |
                BYTE    %00010011 ; |   X  XX|
                BYTE    %00010010 ; |   X  X |
                BYTE    %00010010 ; |   X  X |
                BYTE    %00010011 ; |   X  XX|
                BYTE    %00001000 ; |    X   |
                BYTE    %00000111 ; |     XXX|

.Copyright      BYTE    %11000000 ; |XX      |
                BYTE    %00100000 ; |  X     |
                BYTE    %10010000 ; |X  X    |
                BYTE    %00010000 ; |   X    |
                BYTE    %00010000 ; |   X    |
                BYTE    %10010000 ; |X  X    |
                BYTE    %00100000 ; |  X     |
                BYTE    %11000000 ; |XX      |

        ENDM
