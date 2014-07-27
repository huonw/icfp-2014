include "consts.asm"

const WALL_PENALTY 100
const DIR_PENALTY 100
const LAST_DIR_PENALTY 1
const LAST_PLACE_PENALTY 1

const PLAYER_DRIFT_BONUS 5
const MY_INDEX_BONUS 5

decl up
decl right
decl down
decl left

decl tick
decl player_x
decl player_y
decl my_x
decl my_y
decl my_vitality
decl my_direction
decl my_index
decl last_dir

;;; last_places is stored as [x, y, x, y, ...], these numbers are the
;;; number of cells, not the number of logical pairs.
const NUM_LAST_PLACES 4
const LAST_PLACE_MASK 3
decl last_place_idx
decl last_places

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
        [a] += $MY_INDEX_BONUS

        int $GHOST_CURR_POS
        @my_x = a
        @my_y = b

        a = @my_index
        int $GHOST_INFO
        @my_vitality = a
        @my_direction = b

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
        d = a
        e = b
        call rate-wall
        call rate-last-visited

        ;; LEFT
        a = @my_x
        a -= 1
        b = @my_y
        c = &@left
        d = a
        e = b
        call rate-wall
        call rate-last-visited

        ;; DOWN
        a = @my_x
        b = @my_y
        b += 1
        c = &@down
        d = a
        e = b
        call rate-wall
        call rate-last-visited

        ;; UP
        a = @my_x
        b = @my_y
        b -= 1
        c = &@up
        d = a
        e = b
        call rate-wall
        call rate-last-visited

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
        c = &@last_places
        c += @last_place_idx
        [c] = @my_x
        c += 1
        [c] = @my_y
        @last_place_idx += 2
        @last_place_idx &= $LAST_PLACE_MASK

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

;;; c = direction, d = x position, e = y position
rate-last-visited:
        g = &@last_places
        f = g
        f += $NUM_LAST_PLACES
visitations-start:
        b = [g]
        g += 1
        ;; last-x == current x
        jeq visitations-check-y, b, d
        jump visitations-next
visitations-check-y:
        b = [g]
        ;; last-y == current y
        jeq visitations-penalty, b, e
        jump visitations-next
visitations-penalty:
        a = $LAST_PLACE_PENALTY
        call apply-penalty
visitations-next:
        g += 1
        jlt visitations-start, g, f
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
        jgt apply-penalty-overflow,[c],a
        return
apply-penalty-overflow:
        [c] = 0
        return
