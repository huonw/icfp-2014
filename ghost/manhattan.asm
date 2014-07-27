include "consts.asm"

const WALL_PENALTY 100
const DIR_PENALTY 100
const PLAYER_DRIFT_BONUS 5
const LAST_DIR_PENALTY 1

decl up
decl right
decl down
decl left

decl tick
decl player_x
decl player_y
decl my_x
decl my_y
decl my_direction
decl my_index
decl last_dir

main:

        @up = 128
        @right = 128
        @down = 128
        @left = 128

        int $LAMBDA_POS_1
        @player_x = a
        @player_y = b


        int $MY_INDEX
        @my_index = a

        int $GHOST_CURR_POS
        @my_x = a
        @my_y = b

        a = @my_index
        int $GHOST_INFO
        @my_direction = b
        debug

;;; opposite direction penalty
        ;; flip the direction, the direction variables are at fixed
        ;; locations (exactly the corresponding direction)
        b ^= 2
        a = $DIR_PENALTY
        c = b
        call apply-penalty
;;; last direction penalty
        a = $LAST_DIR_PENALTY
        c = @last_dir
        call apply-penalty

;;; wall penalties
        ;; RIGHT
        a = @my_x
        a += 1
        b = @my_y
        c = &@right
        call rate-wall

        ;; LEFT
        a = @my_x
        a -= 1
        b = @my_y
        c = &@left
        call rate-wall

        ;; DOWN
        a = @my_x
        b = @my_y
        b += 1
        c = &@down
        call rate-wall

        ;; UP
        a = @my_x
        b = @my_y
        b -= 1
        c = &@up
        call rate-wall

        a = $PLAYER_DRIFT_BONUS
;;; Check horizonal directions
        jgt player-to-left, @my_x, @player_x
        jeq end-player-hori, @my_x, @player_x
        c = &@right
        call-ret-to apply-bonus end-player-hori
player-to-left:
        c = &@left
        call apply-bonus
end-player-hori:

        a = $PLAYER_DRIFT_BONUS
;;; Check vertical
        jgt player-above, @my_y, @player_y
        jeq end-player-vert, @my_y, @player_y
        c = &@down
        call-ret-to apply-bonus end-player-vert
player-above:
        c = &@up
        call apply-bonus
end-player-vert:

        ;; Find the highest rated direction.
        a = $UP
        b = @up
        jgt a-big-1, b, @down
        a = $DOWN
        b = @down
a-big-1:
        jgt a-big-2, b, @left
        a = $LEFT
        b = @left
a-big-2:
        jgt a-big-3, b, @right
        a = $RIGHT
        b = @right
a-big-3:
        c = @up
        d = @right
        e = @down
        f = @left
        g = @my_direction
        @last_dir = a
        debug

        int $TELL_DIR
        inc @tick
        hlt

;;; a = x position, b = y position, c = address of variable to adjust
rate-wall:
        int $SQUARE_STATUS
        jgt rate-wall-is-not-wall, a, $WALL
        a = $WALL_PENALTY
        call apply-penalty
rate-wall-is-not-wall:
        return


;;; a = points to add, c = address to adjust
apply-bonus:
        push [c]
        [c] += a
        pop a
        jlt apply-bonus-overflow,[c],a
        return
apply-bonus-overflow:
        [c] = 255
        return

;;; a = points to remove, c = address to adjust
apply-penalty:
        push [c]
        [c] -= a
        pop a
        jgt apply-bonus-overflow,[c],a
        return
apply-penalty-overflow:
        [c] = 0
        return
