const TELL_DIR 0
const LAMBDA_POS_1 1
const LAMBDA_POS_2 2
const MY_INDEX 3
const GHOST_START_POS 4
const GHOST_CURR_POS 5
const GHOST_INFO 6
const SQUARE_STATUS 7
const DEBUG 8

const ONE 1

decl A
decl B

        int $DEBUG
        inc c
        @A = $ONE
        @B = @A
        @B += $ONE
        h = @B
        int $DEBUG
        a |= @B
        int $DEBUG
        int $TELL_DIR
        jlt label, @A, @A
label:
        hlt
