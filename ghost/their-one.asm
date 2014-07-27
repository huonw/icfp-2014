const TELL_DIR 0
const LAMBDA_POS_1 1
const LAMBDA_POS_2 2
const MY_INDEX 3
const GHOST_START_POS 4
const GHOST_CURR_POS 5
const GHOST_INFO 6
const SQUARE_STATUS 7
const DEBUG 8

    mov a,255
	mov b,0
	mov c,255
label_a:
	inc c
	jgt label_b,[c],a

	mov a,[c]
	mov b,c
label_b:
	jlt label_a,c,3

	mov a,b
	int $TELL_DIR

	int $MY_INDEX
	int $GHOST_INFO
	inc [b]
	hlt
