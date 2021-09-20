# Online Combat PlusROM-Hack

PoC for an realtime online game with the PlusROM functions.

The clients send their (real) joystick data to the server and move both tanks by the (virtual) joystick data they receive from the server. For a real online game the clients should also receive and partly send the position and direction data of the tanks. In case of combat this will be a bit of a challenge, because the horizontal movement of the tanks is done by HMOVE so the clients don't know (and therefor can't tell the server) the horizontal position of the tanks.

The PlusROM backend only distributes the Joystick data to the clients and leaves the rest to client-side prediction (without any server-side correction). That's why the clients are out of sync pretty soon.

The original disassembly by Harry Dodgson, can be found here:
http://www.bjars.com/source/Combat.asm

